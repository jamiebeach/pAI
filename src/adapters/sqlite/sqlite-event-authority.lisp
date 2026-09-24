;;;; sqlite-event-authority.lisp -- explicit single-authority event port.

(in-package :agent)

(export '(sqlite-event-authority-prepare
          sqlite-event-authority-checkpoint
          sqlite-event-authority-active-p))

(defvar *sqlite-event-authority-backend* nil)
(defvar *sqlite-event-authority-checkpoint-backend* nil)
(defvar *sqlite-event-authority-agent-id* nil)
(defvar *sqlite-event-authority-database* nil)
(defvar *sqlite-event-authority-derived-database* nil)
(defvar *sqlite-event-authority-projection-enabled-p* nil)

(defun %sqlite-authority-jsonl-sources (source-jsonl)
  "Resolve the complete ordered legacy source without mutating that storage."
  (cond
    ((null source-jsonl) nil)
    ((or (listp source-jsonl) (vectorp source-jsonl))
     (coerce source-jsonl 'list))
    ((and (boundp '*event-log-file*)
          (fboundp '%event-storage-paths)
          (equal (namestring (pathname source-jsonl))
                 (namestring (pathname *event-log-file*))))
     (remove-if-not #'probe-file (%event-storage-paths)))
    ((probe-file source-jsonl) (list source-jsonl))
    (t nil)))

(defun sqlite-event-authority-active-p ()
  (and *sqlite-event-authority-backend* *event-authority-port* t))

(defun %sqlite-authority-replay
    (backend agent-id &key from to limit types exclude-types)
  (multiple-value-bind (events ignored-boundary)
      (storage-query-events
       backend :agent-id agent-id :from from :to to :limit limit
       :event-types types :exclude-event-types exclude-types)
    (declare (ignore ignored-boundary))
    events))

(defun %sqlite-authority-map
    (backend agent-id visitor
     &key after-position through-position after-id through-id from to types
          exclude-types)
  ;; An unbounded generation read is the memory-sensitive recursive replay
  ;; path. Stream its SQL-filtered rows in physical authority order instead of
  ;; materializing a second full decoded ledger before the caller can narrow
  ;; or project it. ID/time/exclusion windows retain the snapshot query path.
  (if (and (null after-id) (null through-id) (null from) (null to)
           (null exclude-types))
      (let ((maximum-id nil))
        (multiple-value-bind (complete-p ignored-position count ignored-bytes)
            (storage-map-events
             backend
             (lambda (event position)
               (declare (ignore position))
               (let ((id (gethash "id" event)))
                 (when (and (integerp id)
                            (or (null maximum-id) (> id maximum-id)))
                   (setf maximum-id id)))
               (funcall visitor event))
             :agent-id agent-id :after-position (or after-position 0)
             :through-position through-position
             :event-types types)
          (declare (ignore ignored-position ignored-bytes))
          (values complete-p maximum-id count)))
      (multiple-value-bind (events last-id)
          (storage-query-events
           backend :agent-id agent-id :after-id after-id
           :through-id through-id :from from :to to :event-types types
           :exclude-event-types exclude-types)
        (dolist (event events) (funcall visitor event))
        (values t last-id (length events)))))

(defun %sqlite-authority-root-recent
    (backend agent-id root-id event-types limit before-event-id)
  "Use the prepared causal index to read only this root's recent children.
Every returned event is verified by the authority lookup. Imported duplicate
logical IDs fail closed when resolving the physical page cursor."
  (let ((before (storage-event-position backend agent-id before-event-id))
        (root (storage-event-position backend agent-id root-id))
        (events nil))
    (unless (and before root (< root before))
      (error 'storage-conflict-error :operation :root-recent
             :detail "root or before-event is absent, ambiguous or misordered"))
    (let ((frontier (1- before)))
      (loop repeat limit
            while (plusp frontier)
            do (multiple-value-bind (present event)
                   (storage-root-has-event-type-p
                    backend agent-id root-id event-types
                    :through-position frontier :newest-p t)
                 (unless present (return))
                 (let ((position
                         (storage-event-position
                          backend agent-id (gethash "id" event))))
                   (unless (and position (<= position frontier)
                                (> position root))
                     (error 'storage-conflict-error :operation :root-recent
                            :detail "causal witness is outside root boundary"))
                   (push event events)
                   (setf frontier (1- position))))))
    events))

(defun %sqlite-authority-episodic-events
    (backend agent-id &optional through-event-id)
  "Read only exact episodic inputs at one source-bound physical frontier.
The bounded list is still a transitional recall input, not a stored pair
index. Refuse overflow rather than omitting older unsealed dialogue."
  (let* ((boundary (storage-authority-boundary
                    backend :agent-id agent-id))
         (through-position
           (if through-event-id
               (let ((position (storage-event-position
                                backend agent-id through-event-id)))
                 (unless (and position
                              (<= position
                                  (gethash "through_storage_position" boundary)))
                   (error 'storage-conflict-error
                          :operation :episodic-context-events
                          :detail "episodic boundary event is absent"))
                 position)
               (gethash "through_storage_position" boundary)))
         (events nil) (charged 0) (selected 0) (maximum-rows 16384)
         (maximum-bytes 33554432))
    (multiple-value-bind (complete-p ignored count source-bytes)
        (storage-map-events
         backend
         (lambda (event position)
           (declare (ignore position))
           (when (>= selected maximum-rows)
             (error 'storage-unavailable-error
                    :operation :episodic-context-events
                    :detail "episodic input row bound exceeded"))
           (incf charged
                 (length (babel:string-to-octets
                          (%storage-json event) :encoding :utf-8)))
           (when (> charged maximum-bytes)
             (error 'storage-unavailable-error
                    :operation :episodic-context-events
                    :detail "episodic input byte bound exceeded"))
           (incf selected)
           (push event events))
         :agent-id agent-id :after-position 0
         :through-position through-position
         :event-types *event-episodic-context-types*
         :limit (1+ maximum-rows))
      (declare (ignore ignored))
      (unless (and complete-p (<= count maximum-rows)
                   (<= source-bytes maximum-bytes))
        (error 'storage-unavailable-error
               :operation :episodic-context-events
               :detail "typed episodic read is incomplete or over bound")))
    (unless (equal (gethash "source_binding" boundary)
                   (gethash "source_binding"
                            (storage-authority-boundary
                             backend :agent-id agent-id)))
      (error 'storage-conflict-error :operation :episodic-context-events
             :detail "authority advanced during episodic read"))
    (nreverse events)))

(defun %sqlite-authority-install
    (backend database checkpoint-backend derived-database agent-id
     &key (projection-enabled-p t))
  (event-authority-install
   :sqlite
   :append
   (lambda (type payload caused-by tick-id affect-snapshot)
     (let ((event
             (storage-append-event
              backend type payload :agent-id agent-id
              :occurred-at (%event-now-iso8601)
              :caused-by (or caused-by :null) :tick-id (or tick-id :null)
              :affect-snapshot affect-snapshot)))
       (values (gethash "id" event) t event)))
   :append-if-head
   (lambda (head type payload caused-by tick-id affect-snapshot)
     (let ((event (storage-append-event-if-head
                   backend head type payload :agent-id agent-id
                   :occurred-at (%event-now-iso8601)
                   :caused-by (or caused-by :null) :tick-id (or tick-id :null)
                   :affect-snapshot affect-snapshot)))
       (values (gethash "id" event) t event)))
   :owns-storage (lambda (candidate) (eq candidate backend))
   :replay (lambda (&rest arguments)
             (apply #'%sqlite-authority-replay backend agent-id arguments))
   :map (lambda (visitor &rest arguments)
          (apply #'%sqlite-authority-map backend agent-id visitor arguments))
   :restore (lambda () (storage-max-event-id backend))
   :read-event
   (lambda (event-id event-type)
     (storage-read-event backend event-id :agent-id agent-id
                        :event-type event-type))
   :projection-events
   (lambda ()
     (unless projection-enabled-p
       (error 'storage-unavailable-error :operation :projection-events
              :detail "conscious projection is not installed in ledger-only mode"))
     (conscious-storage-restore-event-sequence
      backend :checkpoint-backend checkpoint-backend :agent-id agent-id))
   :recent-conversation
   (lambda (before-event-id limit)
     (storage-recent-events
      backend '("user-message" "agent-message" "model-response") limit
      :agent-id agent-id :before-event-id before-event-id))
   :root-recent
   (lambda (root-id event-types limit before-event-id)
     (%sqlite-authority-root-recent
      backend agent-id root-id event-types limit before-event-id))
   :episodic-events
   (lambda (through-event-id)
     (%sqlite-authority-episodic-events
      backend agent-id through-event-id))
   :activity-read
   (lambda (reference-id through-id)
     (storage-read-activity-context backend reference-id :agent-id agent-id
                                    :through-event-id through-id))
   :experience-page
   (lambda (from to before-position limit)
     (sqlite-experience-page backend agent-id from to before-position limit))
   :checkpoint-load
   (lambda (projection-name)
     (storage-load-checkpoint checkpoint-backend projection-name
                              :agent-id agent-id))
   :checkpoint-publish
   (lambda (projection-name state through-event-id through-position
            projector-revision policy-revision)
     (storage-publish-checkpoint
      checkpoint-backend projection-name state :agent-id agent-id
      :through-event-id through-event-id :through-position through-position
      :projector-revision projector-revision
      :policy-revision policy-revision))
   :checkpoint-source-binding
   (lambda (through-event-id through-position)
     (storage-checkpoint-source-binding
      backend :agent-id agent-id :through-event-id through-event-id
      :through-position through-position))
   :report
   (lambda ()
     (obj "schema_version" 1 "authority" "sqlite"
          "database" (namestring database)
          "agent_id" agent-id
          "head_position" (storage-head-position backend :agent-id agent-id)
          "max_event_id" (storage-max-event-id backend :agent-id agent-id)))
   :close (lambda () (storage-close backend)))
  ;; EVENT-AUTHORITY-CLEAR owns both handles once installation succeeds.
  (when (not (eq backend checkpoint-backend))
    (let ((old-close (getf *event-authority-port* :close)))
      (setf (getf *event-authority-port* :close)
            (lambda ()
              (unwind-protect (funcall old-close)
                (storage-close checkpoint-backend))))))
  (setf *sqlite-event-authority-backend* backend
        *sqlite-event-authority-checkpoint-backend* checkpoint-backend
        *sqlite-event-authority-agent-id* agent-id
        *sqlite-event-authority-database* database
        *sqlite-event-authority-derived-database* derived-database
        *sqlite-event-authority-projection-enabled-p* projection-enabled-p)
  backend)

(defun %sqlite-authority-prepare-report
    (status backend agent-id sources import-report audit-report checkpoint-report)
  (let ((source-report (or import-report audit-report)))
    (obj "schema_version" 1 "status" status "authority" "sqlite"
       "agent_id" agent-id
       "source_file_count" (length sources)
       "event_count"
       (if source-report
           (gethash "event_count" source-report)
           (storage-head-position backend :agent-id agent-id))
       "first_event_id"
       (if source-report (gethash "first_event_id" source-report) :null)
       "last_event_id"
       (if source-report (gethash "last_event_id" source-report) :null)
       "maximum_event_id" (storage-max-event-id backend :agent-id agent-id)
       "duplicate_id_count"
       (if source-report (gethash "duplicate_id_count" source-report) 0)
       "rewind_count" (if source-report (gethash "rewind_count" source-report) 0)
       "audit_status" (if audit-report (gethash "status" audit-report) "existing")
       "checkpoint_storage_position"
       (if checkpoint-report
           (gethash "through_storage_position" checkpoint-report)
           (storage-head-position backend :agent-id agent-id)))))

(defun %sqlite-authority-copy-legacy-checkpoint
    (event-backend checkpoint-backend agent-id rebuild-stale-checkpoint-p)
  (when (not (eq event-backend checkpoint-backend))
    (let ((target
            (storage-load-checkpoint
             checkpoint-backend *conscious-storage-checkpoint-name*
             :agent-id agent-id))
          (legacy
            (storage-load-checkpoint
             event-backend *conscious-storage-checkpoint-name*
             :agent-id agent-id)))
      ;; A literal copy would preserve the old capsule without binding it to
      ;; this event database. Rebuild once from authoritative events instead.
      ;; The build proves bounded/full projection equality before publishing.
      (when (or (and (null target) legacy)
                (and target
                     (not (string=
                           *conscious-storage-projector-revision*
                           (gethash "projector_revision" target "")))))
        (unless rebuild-stale-checkpoint-p
          (error 'storage-conflict-error :operation :authority-prepare
                 :detail "checkpoint relocation requires explicit rebuild authority"))
        (conscious-storage-build-checkpoint
         event-backend :checkpoint-backend checkpoint-backend
         :agent-id agent-id)))))

(defun sqlite-event-authority-prepare
    (database source-jsonl
     &key derived-database (agent-id "default") migrate-p initialize-p
       rebuild-stale-checkpoint-p (restore-projection-p t)
       (missing-derived-replay-max-head 0))
  "Open the authoritative SQLite ledger. MIGRATE-P requires non-empty legacy
history; INITIALIZE-P explicitly creates empty history. Neither is implicit.
MISSING-DERIVED-REPLAY-MAX-HEAD permits only bounded recovery of an absent
derived database; it never grants stale-checkpoint or migration repair."
  (when *event-authority-port*
    (error 'storage-conflict-error :operation :authority-prepare
           :detail "an event authority is already installed"))
  (when (and migrate-p initialize-p)
    (error 'storage-conflict-error :operation :authority-prepare
           :detail "migration and empty initialization are mutually exclusive"))
  (unless (member restore-projection-p '(t nil))
    (error 'storage-error :operation :authority-prepare
           :detail "restore-projection-p must be boolean"))
  (unless (typep missing-derived-replay-max-head '(integer 0 *))
    (error 'storage-error :operation :authority-prepare
           :detail "missing-derived-replay-max-head must be a nonnegative integer"))
  (let* ((database (pathname database))
         (new-p (not (probe-file database)))
         (backend (make-sqlite-storage database))
         (derived-database (and derived-database (pathname derived-database)))
         ;; Capture absence before MAKE-SQLITE-DERIVED-STORAGE creates the
         ;; schema.  A missing derived database is a rebuild request, while
         ;; an existing checkpoint-free database can indicate an interrupted
         ;; migration and must retain the stricter resume contract.
         (derived-new-p (and derived-database
                             (not (probe-file derived-database))))
         (bounded-rebuild-p
           (and derived-new-p (plusp missing-derived-replay-max-head)
                (<= (storage-head-position backend :agent-id agent-id)
                    missing-derived-replay-max-head)))
         (checkpoint-backend
           (progn
             ;; Refuse before creating an empty derived database. Otherwise a
             ;; later explicit repair mistakes this failed normal start for
             ;; an interrupted migration and cannot use the rebuild path.
             (when (and derived-new-p (not rebuild-stale-checkpoint-p)
                        (not bounded-rebuild-p)
                        (not migrate-p) (not initialize-p)
                        (plusp (storage-head-position backend :agent-id agent-id)))
               (storage-close backend)
               (error 'storage-conflict-error :operation :authority-prepare
                      :detail "derived database is absent; explicit offline rebuild is required"))
             (if derived-database
                 (make-sqlite-derived-storage derived-database)
                 backend)))
         (sources (%sqlite-authority-jsonl-sources source-jsonl)))
    (handler-case
        (progn
          (unless restore-projection-p
            (let ((head (storage-head-position backend :agent-id agent-id))
                  (status nil) (import-report nil) (audit-report nil))
              (cond
                ((zerop head)
                 (cond
                   (migrate-p
                    (unless sources
                      (error 'storage-conflict-error
                             :operation :authority-prepare
                             :detail "event migration resolved no legacy source"))
                    (setf import-report
                          (sqlite-import-jsonl backend sources
                                               :legacy-agent-id agent-id))
                    (when (zerop (gethash "event_count" import-report))
                      (error 'storage-conflict-error
                             :operation :authority-prepare
                             :detail "migration source contained zero events"))
                    (setf audit-report
                          (sqlite-audit-jsonl-import backend sources
                                                     :legacy-agent-id agent-id)
                          status "migrated-ledger-only"))
                   (initialize-p
                    (when sources
                      (error 'storage-conflict-error
                             :operation :authority-prepare
                             :detail "legacy history exists; refuse empty initialization"))
                    (setf status "initialized-empty-ledger-only"))
                   (t
                    (error 'storage-conflict-error
                           :operation :authority-prepare
                           :detail "empty authority requires explicit migration or initialization"))))
                (initialize-p
                 (error 'storage-conflict-error :operation :authority-prepare
                        :detail "event authority is already initialized"))
                (migrate-p
                 (unless sources
                   (error 'storage-conflict-error
                          :operation :authority-prepare
                          :detail "migration resume resolved no legacy source"))
                 (setf audit-report
                       (sqlite-audit-jsonl-import backend sources
                                                  :legacy-agent-id agent-id)
                       status "migration-resumed-ledger-only"))
                (t (setf status "opened-ledger-only")))
              (%sqlite-authority-install
               backend database checkpoint-backend derived-database agent-id
               :projection-enabled-p nil)
              (return-from sqlite-event-authority-prepare
                (values backend
                        (%sqlite-authority-prepare-report
                         status backend agent-id sources import-report
                         audit-report nil)))))
          (%sqlite-authority-copy-legacy-checkpoint
           backend checkpoint-backend agent-id rebuild-stale-checkpoint-p)
          (let* ((checkpoint
                 (storage-load-checkpoint
                  checkpoint-backend *conscious-storage-checkpoint-name*
                  :agent-id agent-id))
               (head (storage-head-position backend :agent-id agent-id))
               (status nil) (import-report nil) (audit-report nil)
               (checkpoint-report nil))
          (cond
            (checkpoint
             (when (or migrate-p initialize-p)
               (error 'storage-conflict-error :operation :authority-prepare
                      :detail "SQLite event authority is already initialized; omit migration/initialization flags"))
             (setf status "opened" checkpoint-report checkpoint))
            ((zerop head)
             (cond
               (migrate-p
                (unless sources
                  (error 'storage-conflict-error :operation :authority-prepare
                         :detail "event migration resolved no legacy source; no empty migration was performed"))
                (setf import-report
                      (sqlite-import-jsonl backend sources
                                           :legacy-agent-id agent-id))
                (when (zerop (gethash "event_count" import-report))
                  (error 'storage-conflict-error :operation :authority-prepare
                         :detail "event migration source contained zero events; use explicit empty initialization for a new state"))
                (setf audit-report
                      (sqlite-audit-jsonl-import backend sources
                                                 :legacy-agent-id agent-id)
                      status "migrated"))
               (initialize-p
                (when sources
                  (error 'storage-conflict-error :operation :authority-prepare
                         :detail "legacy event history exists; empty initialization would discard it"))
                (setf status "initialized-empty"))
               (t
                (error 'storage-conflict-error :operation :authority-prepare
                       :detail "SQLite event authority is uninitialized; choose explicit migration or empty initialization")))
             (setf checkpoint-report
                   (conscious-storage-build-checkpoint
                    backend :checkpoint-backend checkpoint-backend
                    :agent-id agent-id)))
            (t
             (if (and derived-new-p
                      (or rebuild-stale-checkpoint-p bounded-rebuild-p)
                      (not migrate-p) (not initialize-p))
                 ;; The event ledger is authoritative and a deliberately
                 ;; absent derived database carries no state to reconcile.
                 ;; Build under explicit full or bounded missing-state rebuild
                 ;; authority. Existing checkpoint-free databases still fail
                 ;; closed below because they may be interrupted migrations.
                 (setf status "projection-rebuilt-from-ledger"
                       checkpoint-report
                       (conscious-storage-build-checkpoint
                        backend :checkpoint-backend checkpoint-backend
                        :agent-id agent-id))
                 (progn
                   ;; Import committed but checkpoint publication did not.
                   ;; Resume only after independently re-auditing the source.
                   (unless migrate-p
                     (error 'storage-conflict-error :operation :authority-prepare
                            :detail "SQLite authority has uncheckpointed history; explicit migration resume is required"))
                   (unless sources
                     (error 'storage-conflict-error :operation :authority-prepare
                            :detail "migration resume resolved no legacy source"))
                   (setf audit-report
                         (sqlite-audit-jsonl-import
                          backend sources :legacy-agent-id agent-id)
                         status "migration-resumed"
                         checkpoint-report
                         (conscious-storage-build-checkpoint
                          backend :checkpoint-backend checkpoint-backend
                          :agent-id agent-id))))))
          (handler-case
              (conscious-storage-restore-event-sequence
               backend :checkpoint-backend checkpoint-backend :agent-id agent-id)
            (storage-conflict-error (condition)
              ;; Composition/projector revisions identify rebuildable code,
              ;; not event authority. An operator entry point may explicitly
              ;; replace only this derived checkpoint after the old one has
              ;; proved readable and source-bound. Integrity/binding failures
              ;; never enter this path.
              (unless (and rebuild-stale-checkpoint-p
                           (eq :projection-restore
                               (storage-error-operation condition))
                           (string=
                            "conscious projection composition changed"
                            (storage-error-detail condition)))
                (error condition))
              (setf checkpoint-report
                    (conscious-storage-build-checkpoint
                     backend :checkpoint-backend checkpoint-backend
                     :agent-id agent-id)
                    status "checkpoint-rebuilt")
              (conscious-storage-restore-event-sequence
               backend :checkpoint-backend checkpoint-backend
               :agent-id agent-id)))
          (%sqlite-authority-install
           backend database checkpoint-backend derived-database agent-id)
          (values
           backend
           (%sqlite-authority-prepare-report
            status backend agent-id sources import-report audit-report
            checkpoint-report))))
      (error (condition)
        (ignore-errors (storage-close backend))
        (unless (eq checkpoint-backend backend)
          (ignore-errors (storage-close checkpoint-backend)))
        (when (and new-p (probe-file database)
                   (zerop (or (ignore-errors (with-open-file (s database)
                                               (file-length s))) 0)))
          (ignore-errors (delete-file database)))
        (error condition)))))

(defun sqlite-event-authority-checkpoint ()
  "Refresh from the verified bounded prefix plus tail without full replay."
  (unless (sqlite-event-authority-active-p)
    (error "SQLite event authority is not active"))
  (unless *sqlite-event-authority-projection-enabled-p*
    (error 'storage-unavailable-error :operation :projection-checkpoint
           :detail "conscious projection is not installed in ledger-only mode"))
  (conscious-storage-refresh-checkpoint
   *sqlite-event-authority-backend*
   :checkpoint-backend *sqlite-event-authority-checkpoint-backend*
   :agent-id *sqlite-event-authority-agent-id*))

(register-layer recursive-root-failure-receipt sqlite-indexed-root
  :order 100
  :function
  (lambda (next root-event-id)
    (if (and (sqlite-event-authority-active-p)
             (storage-activity-index-ready-p
              *sqlite-event-authority-backend*))
        (let ((boundary
                (storage-authority-boundary
                 *sqlite-event-authority-backend*
                 :agent-id *sqlite-event-authority-agent-id*)))
          ;; An indexed failure is never treated as an absent receipt. The
          ;; legacy reader is only for an authority without the explicit
          ;; maintenance index (or the non-SQLite harness).
          (nth-value
           1
           (storage-root-has-event-type-p
            *sqlite-event-authority-backend*
            *sqlite-event-authority-agent-id* root-event-id
            "recursive-root-failed" :source-boundary boundary :newest-p t)))
        (funcall next root-event-id))))
