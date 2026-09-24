(in-package :agent)

(ql:quickload '(:cffi :shasht :ironclad :bordeaux-threads) :silent t)

;; This module-isolation fixture loads the SQLite adapter without the recursive
;; mind. Declare its registered seam with a neutral base, preserving the real
;; module's ownership rather than hiding an undeclared layer error.
(load (test-source "seams.lisp"))
(define-seam recursive-root-failure-receipt (root-event-id)
  (declare (ignore root-event-id))
  nil)

(defvar *sea-pass* 0)
(defvar *sea-fail* 0)

(defun sea-check (name condition)
  (if condition
      (progn (incf *sea-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *sea-fail*) (format t "  FAIL ~a~%" name))))

(defun sea-signals-p (type thunk)
  (handler-case (progn (funcall thunk) nil)
    (condition (actual) (typep actual type))))

(defun sea-delete-db (path)
  (dolist (candidate (list path
                           (pathname (concatenate 'string (namestring path) "-wal"))
                           (pathname (concatenate 'string (namestring path) "-shm"))))
    (when (probe-file candidate) (delete-file candidate))))

(dolist (file '("event-log.lisp" "policy.lisp" "stimulus.lisp" "census.lisp"
                "concern.lisp" "codelets.lisp" "context.lisp" "inbox.lisp"
                "attention.lisp" "mind/conscious/lifecycle.lisp" "lifecycle-semantics.lisp"
                "state.lisp" "storage-substrate.lisp" "activity-storage.lisp"
                "sqlite-storage.lisp" "sqlite-activity-storage.lisp"
                "memory-storage.lisp" "sqlite-derived-storage.lisp"
                "sqlite-import.lisp" "storage-projection.lisp"
                "sqlite-event-authority.lisp" "cognitive-work.lisp"
                "runtime-composition.lisp"))
  (load (test-source file)))

(format t "~%== SQLite single event authority ==~%")

(let* ((root (test-state-dir))
       (source (merge-pathnames "authority-source.jsonl" root))
       (segment (merge-pathnames
                 "events-000000000001-000001000000.jsonl" root))
       (database (merge-pathnames "authority.sqlite3" root))
       (derived-database (merge-pathnames "authority-derived.sqlite3" root))
       (empty-database (merge-pathnames "authority-empty.sqlite3" root))
       (zero-source-database
         (merge-pathnames "authority-zero-source.sqlite3" root))
       (new-state-database
         (merge-pathnames "authority-new-state.sqlite3" root))
       (agent-id "authority-dev")
       (events
         (list
          (obj "schema_version" 1 "id" 1 "agent_id" agent-id
               "timestamp" "2026-08-19T20:00:00Z" "type" "user-message"
               "payload" (obj "text" "handled" "channel" "cli"
                              "origin_runtime_revision" "conscious-q5-v2")
               "caused_by" :null "tick_id" :null "affect_snapshot" :null)
          (obj "schema_version" 1 "id" 2 "agent_id" agent-id
               "timestamp" "2026-08-19T20:00:01Z" "type" "agent-message"
               "payload"
               (obj "authorization_kind" "solicited-publication-candidate"
                    "metadata" (obj "source" "q4.5-conversation"
                                    "publication_validation" "accepted")
                    "content" "reply")
               "caused_by" 1 "tick_id" :null "affect_snapshot" :null)
          (obj "schema_version" 1 "id" 3 "agent_id" agent-id
               "timestamp" "2026-08-19T20:00:02Z" "type" "user-message"
               "payload" (obj "text" "outstanding" "channel" "cli"
                              "origin_runtime_revision" "conscious-q5-v2")
               "caused_by" :null "tick_id" :null "affect_snapshot" :null)))
       (source-before nil))
  (sea-delete-db database)
  (sea-delete-db derived-database)
  (sea-delete-db empty-database)
  (sea-delete-db zero-source-database)
  (sea-delete-db new-state-database)
  (when (probe-file source) (delete-file source))
  (when (probe-file segment) (delete-file segment))
  (unwind-protect
      (progn
        (with-open-file (stream source :direction :output :if-exists :supersede
                                      :if-does-not-exist :create
                                      :external-format :utf-8)
          (dolist (event events) (write-line (%storage-json event) stream)))
        (setf source-before (uiop:read-file-string source))
        (with-open-file (stream segment :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create)
          (write-line "{}" stream))
        (let ((*event-log-file* source)
              (*event-log-segment-directory* root))
          (let ((resolved (%sqlite-authority-jsonl-sources source)))
            (sea-check "migration enumerates root and ordered legacy segments"
                       (and (equal (namestring (truename source))
                                   (namestring (truename (first resolved))))
                            (find (namestring (truename segment)) resolved
                                  :key (lambda (path)
                                         (namestring (truename path)))
                                  :test #'string=)))))
        (delete-file segment)
        (multiple-value-bind (ignored receipt)
            (sqlite-event-authority-prepare
             database source :derived-database derived-database
             :agent-id agent-id :migrate-p t)
          (declare (ignore ignored))
          (sea-check "migration returns operator-visible parity receipt"
                     (and (string= "migrated" (gethash "status" receipt))
                          (= 3 (gethash "event_count" receipt))
                          (string= "verified" (gethash "audit_status" receipt)))))
        (sea-check "explicit migration installs SQLite as sole authority"
                   (string= "sqlite"
                            (gethash "authority" (event-authority-report))))
        (sea-check "canonical checkpoint authority is the separate derived database"
                   (and
                    (null (storage-load-checkpoint
                           *sqlite-event-authority-backend*
                           *conscious-storage-checkpoint-name*
                           :agent-id agent-id))
                    (storage-load-checkpoint
                     *sqlite-event-authority-checkpoint-backend*
                     *conscious-storage-checkpoint-name*
                     :agent-id agent-id)))
        (%event-restore-next-id)
        (multiple-value-bind (id durable receipt)
            (log-event "heap-health" (obj "status" "native"))
          (sea-check "authority append returns exact durable receipt"
                     (and (= 4 id) durable (= id (gethash "id" receipt)))))
        (sea-check "authoritative append never changes legacy JSONL"
                   (string= source-before (uiop:read-file-string source)))
        (sea-check "full audit replay reads SQLite history"
                   (= 4 (length (replay-events))))
        (sea-check "typed episodic authority read matches full replay selection"
                   (equal
                    (mapcar (lambda (event) (gethash "id" event))
                            (event-episodic-context-events))
                    (mapcar (lambda (event) (gethash "id" event))
                            (remove-if-not
                             (lambda (event)
                               (member (gethash "type" event)
                                       *event-episodic-context-types*
                                       :test #'string=))
                             (replay-events)))))
        (sea-check "episodic authority read excludes events after turn boundary"
                   (equal '(1 2)
                          (mapcar (lambda (event) (gethash "id" event))
                                  (event-episodic-context-events 2))))
        (sea-check "episodic authority refuses missing turn boundary"
                   (sea-signals-p
                    'storage-conflict-error
                    (lambda () (event-episodic-context-events 999))))
        (let ((exact (event-read-event 4)))
          (sea-check "exact event reads use the installed SQLite authority"
                     (and (= 4 (gethash "id" exact))
                          (string= "heap-health" (gethash "type" exact))
                          (string= "native"
                                   (gethash "status"
                                            (gethash "payload" exact))))))
        (sea-check "root-recent port refuses missing causal index"
                   (and
                    (not (storage-activity-index-ready-p
                          *sqlite-event-authority-backend*))
                    (sea-signals-p
                     'error
                     (lambda ()
                       (event-root-recent-events 1 '("agent-message") 1 3)))))
        (storage-prepare-activity-index *sqlite-event-authority-backend*)
        (sea-check "root-recent port returns bounded causal evidence"
                   (equal '(2)
                          (mapcar (lambda (event) (gethash "id" event))
                                  (event-root-recent-events
                                   1 '("agent-message") 1 3))))
        (sea-check "authority replay preserves the legacy single-value contract"
                   (= 1 (length (multiple-value-list (replay-events)))))
        (sea-check "audit replay preserves inclusive time windows"
                   (= 2 (length
                         (replay-events
                          :from (%event-parse-ts-string
                                 "2026-08-19T20:00:01Z")
                          :to (%event-parse-ts-string
                               "2026-08-19T20:00:02Z")))))
        (let ((latest-user
                (replay-events :types '("user-message") :limit 1)))
          (sea-check "limited typed replay uses newest-match semantics"
                     (and (= 1 (length latest-user))
                          (= 3 (gethash "id" (first latest-user))))))
        (let ((visited 0))
          (multiple-value-bind (complete last-id count)
              (map-events
               (lambda (event) (declare (ignore event)) (incf visited))
               :from (%event-parse-ts-string "2026-08-19T20:00:01Z")
               :to (%event-parse-ts-string "2026-08-19T20:00:02Z"))
            (sea-check "authority map preserves time filtering and watermark"
                       (and complete (= 4 last-id) (= 2 count) (= 2 visited)))))
        (let ((visited nil)
              (types
                (append '("user-message" "agent-message" "heap-health")
                        (loop for index below 40
                              collect (format nil "future-protocol-~d"
                                              index)))))
          (multiple-value-bind (complete last-id count)
              (map-events
               (lambda (event) (push (gethash "id" event) visited))
               :types types)
            (sea-check "authority streams more than 32 selected types in physical order"
                       (and complete (= 4 last-id) (= 4 count)
                            (equal '(1 2 3 4) (nreverse visited))))))
        (let ((visited nil)
              (types
                (append '("user-message" "agent-message" "heap-health")
                        (loop for index below 40
                              collect (format nil "future-protocol-~d"
                                              index)))))
          (multiple-value-bind (complete last-id count)
              (map-events
               (lambda (event) (push (gethash "id" event) visited))
               :after-position 2 :types types)
            (sea-check
             "physical-boundary map streams more than 32 types in authority order"
             (and complete (= 4 last-id) (= 2 count)
                  (equal '(3 4) (nreverse visited))))))
        (let ((visited nil))
          (multiple-value-bind (complete last-id count)
              (map-events
               (lambda (event) (push (gethash "id" event) visited))
               :after-position 1 :through-position 3
               :types '("user-message" "agent-message" "heap-health"))
            (sea-check
             "physical-boundary map stops at its captured authority head"
             (and complete (= 3 last-id) (= 2 count)
                  (equal '(2 3) (nreverse visited))))))
        (sea-check "projection replay uses bounded checkpoint plus tail"
                   (< (length (event-projection-events)) 4))
        (sea-check "recent dialogue query remains available from SQLite"
                   (= 3 (length (event-recent-conversation-events 4 16))))
        (let ((refreshed (sqlite-event-authority-checkpoint)))
          (multiple-value-bind (ignored restore-report)
              (conscious-storage-restore-event-sequence
               *sqlite-event-authority-backend*
               :checkpoint-backend
               *sqlite-event-authority-checkpoint-backend*
               :agent-id agent-id)
            (declare (ignore ignored))
            (sea-check "checkpoint refresh advances without leaving a tail"
                       (and (= 4 (gethash "through_event_id" refreshed))
                            (= 0 (gethash "tail_event_count"
                                          restore-report))))))
        (event-authority-clear)
        (sqlite-event-authority-prepare
         database source :derived-database derived-database
         :agent-id agent-id :migrate-p nil)
        (%event-restore-next-id)
        (sea-check "restart continues above the durable SQLite maximum"
                   (= 5 (log-event "heap-health" (obj "status" "restart"))))
        (event-authority-clear)
        (sea-delete-db derived-database)
        (sea-check "missing derived database fails closed on ordinary startup"
                   (sea-signals-p
                    'storage-conflict-error
                    (lambda ()
                      (sqlite-event-authority-prepare
                       database source :derived-database derived-database
                       :agent-id agent-id))))
        (sea-check "refused ordinary startup does not create derived state"
                   (null (probe-file derived-database)))
        (sea-check "bounded recovery refuses a ledger beyond its limit"
                   (sea-signals-p
                    'storage-conflict-error
                    (lambda ()
                      (sqlite-event-authority-prepare
                       database source :derived-database derived-database
                       :agent-id agent-id :missing-derived-replay-max-head 4))))
        (sea-check "bounded refusal does not create derived state"
                   (null (probe-file derived-database)))
        (multiple-value-bind (ignored rebuilt)
            (sqlite-event-authority-prepare
             database source :derived-database derived-database
             :agent-id agent-id :missing-derived-replay-max-head 5)
          (declare (ignore ignored))
          (sea-check "bounded recovery includes its exact ledger boundary"
                     (and (string= "projection-rebuilt-from-ledger"
                                   (gethash "status" rebuilt))
                          (= 5 (gethash "event_count" rebuilt)))))
        (event-authority-clear)
        (sea-delete-db derived-database)
        (multiple-value-bind (ignored rebuilt)
            (sqlite-event-authority-prepare
             database source :derived-database derived-database
             :agent-id agent-id :rebuild-stale-checkpoint-p t)
          (declare (ignore ignored))
          (sea-check "missing derived database rebuilds from the event ledger"
                     (and (string= "projection-rebuilt-from-ledger"
                                   (gethash "status" rebuilt))
                          (= 5 (gethash "event_count" rebuilt))
                          (storage-load-checkpoint
                           *sqlite-event-authority-checkpoint-backend*
                           *conscious-storage-checkpoint-name*
                           :agent-id agent-id))))
        (event-authority-clear)
        (progv (list '*conscious-cognition-runtime-revision*)
               (list "composition-change-fixture")
          (sea-check "composition drift still fails without rebuild authority"
                     (sea-signals-p
                      'storage-conflict-error
                      (lambda ()
                        (sqlite-event-authority-prepare
                         database source :derived-database derived-database
                         :agent-id agent-id))))
          (sea-check "bounded missing-state recovery cannot repair composition drift"
                     (sea-signals-p
                      'storage-conflict-error
                      (lambda ()
                        (sqlite-event-authority-prepare
                         database source :derived-database derived-database
                         :agent-id agent-id :missing-derived-replay-max-head 5))))
          (multiple-value-bind (ignored rebuilt)
              (sqlite-event-authority-prepare
               database source :derived-database derived-database
               :agent-id agent-id :rebuild-stale-checkpoint-p t)
            (declare (ignore ignored))
            (sea-check "explicit stale checkpoint rebuild preserves authority"
                       (string= "checkpoint-rebuilt"
                                (gethash "status" rebuilt))))
          (event-authority-clear))
        (sea-check "uninitialized SQLite refuses implicit migration"
                   (sea-signals-p
                    'storage-conflict-error
                    (lambda ()
                      (sqlite-event-authority-prepare
                       empty-database source :agent-id agent-id
                       :migrate-p nil)))))
        (sea-check "migration refuses a missing or empty legacy source"
                   (sea-signals-p
                    'storage-conflict-error
                    (lambda ()
                      (sqlite-event-authority-prepare
                       zero-source-database nil :agent-id agent-id
                       :migrate-p t))))
        (multiple-value-bind (ignored receipt)
            (sqlite-event-authority-prepare
             new-state-database nil :agent-id agent-id :initialize-p t)
          (declare (ignore ignored))
          (sea-check "new empty state requires explicit initialization"
                     (and (string= "initialized-empty"
                                   (gethash "status" receipt))
                          (= 0 (gethash "event_count" receipt))))
          (let* ((context (gethash "solicited-conversation-dev"
                           (gethash "profiles" (shasht:read-json
                             (uiop:read-file-string (merge-pathnames
                               "config/conscious-context-profiles.json" *pai-root*))))))
                 (work (gethash "interactive-dev"
                        (gethash "profiles" (shasht:read-json
                          (uiop:read-file-string (merge-pathnames
                            "config/conscious-work-profiles.json" *pai-root*))))))
                 (tools (make-hash-table :test #'equal))
                 (proposals (make-hash-table :test #'equal)))
            (loop for tool across (gethash "permitted_tools" work)
                  do (setf (gethash tool tools)
                           (obj "consumer" "conscious-tool-operation-runtime"
                                "authority_class" "bounded-read-only"
                                "max_result_characters" (gethash "max_tool_result_characters" work))))
            (loop for kind across (gethash "permitted_proposal_kinds" work)
                  do (setf (gethash kind proposals) #( "fixture-consumer")))
            (let* ((plan (conscious-runtime-plan-compile
                          context work (obj "tool_consumers" tools "proposal_consumers" proposals)
                          (obj "profile_id" "contained-provider" "revision" 1
                               "max_requests" (gethash "max_model_calls" work)
                               "max_input_characters" (gethash "total_character_budget" context))
                          (obj "profile_id" "contained-publication" "revision" 1 "channels" #( "terminal"))
                          (obj "profile_id" "contained-transport" "revision" 1 "channel" "terminal")))
                   (gate (sb-thread:make-semaphore :count 0))
                   (workers (loop repeat 2 collect
                              (bt:make-thread
                                (lambda ()
                                  (sb-thread:wait-on-semaphore gate)
                                  (handler-case (conscious-runtime-plan-retain plan)
                                    (error () :failed)))
                                :name "disposable plan registrar"))))
              (sb-thread:signal-semaphore gate 2)
              (let ((results (mapcar (lambda (thread)
                                      (sb-thread:join-thread thread :timeout 10 :default :timed-out))
                                    workers)))
                (sea-check "competing real SQLite plan registrations append once and both join"
                           (and (every #'hash-table-p results)
                                (equal '("registered" "retained")
                                       (sort (mapcar (lambda (row) (gethash "status" row)) results) #'string<))
                                (= 1 (length (replay-events :types (list *conscious-runtime-plan-event-type*)))))))
              (event-authority-clear)
              (sqlite-event-authority-prepare new-state-database nil :agent-id agent-id)
              (sea-check "retained plan resolves after closing and reopening SQLite"
                         (string= (conscious-runtime-plan-hash plan)
                                  (conscious-runtime-plan-hash
                                    (conscious-runtime-plan-resolve (conscious-runtime-plan-hash plan)))))))
          (event-authority-clear)))
    (when *event-authority-port* (ignore-errors (event-authority-clear)))
    (when (probe-file source) (delete-file source))
    (when (probe-file segment) (delete-file segment))
    (sea-delete-db database)
    (sea-delete-db derived-database)
    (sea-delete-db empty-database)
    (sea-delete-db zero-source-database)
    (sea-delete-db new-state-database))

;; Existing CLI databases can have their checkpoint in events.sqlite3.
;; Relocation requires an explicitly authorized offline rebuild; an ordinary
;; startup must not silently replay the full ledger.
(let* ((root (test-state-dir))
       (database (merge-pathnames "authority-relocation.sqlite3" root))
       (mismatch-database
         (merge-pathnames "authority-relocation-mismatch.sqlite3" root))
       (derived (merge-pathnames "authority-relocation-derived.sqlite3" root))
       (agent-id "relocation-dev")
       (legacy-backend nil))
  (sea-delete-db database)
  (sea-delete-db mismatch-database)
  (sea-delete-db derived)
  (unwind-protect
      (progn
        (setf legacy-backend (make-sqlite-storage database))
        (storage-append-event
         legacy-backend "user-message"
         (obj "text" "fixture" "channel" "cli"
              "origin_runtime_revision" "conscious-q5-v2")
         :agent-id agent-id :occurred-at "2026-08-19T21:00:00Z")
        (multiple-value-bind (report ignored)
            (conscious-storage-build-checkpoint
             legacy-backend :agent-id agent-id :now 1000)
          (declare (ignore report ignored)))
        (storage-close legacy-backend)
        (setf legacy-backend nil)
        (sea-check "ordinary startup refuses implicit legacy checkpoint rebuild"
                   (sea-signals-p
                    'storage-conflict-error
                    (lambda ()
                      (sqlite-event-authority-prepare
                       database nil :derived-database derived
                       :agent-id agent-id))))
        (let ((derived-backend (make-sqlite-derived-storage derived)))
          (unwind-protect
              (sea-check "refused startup published no derived checkpoint"
                         (null (storage-load-checkpoint
                                derived-backend
                                *conscious-storage-checkpoint-name*
                                :agent-id agent-id)))
            (storage-close derived-backend)))
        (sqlite-event-authority-prepare
         database nil :derived-database derived :agent-id agent-id
         :rebuild-stale-checkpoint-p t)
        (let ((relocated
                (storage-load-checkpoint
                 *sqlite-event-authority-checkpoint-backend*
                 *conscious-storage-checkpoint-name* :agent-id agent-id)))
          (sea-check "legacy checkpoint rebuild relocates with event binding"
                     (and relocated
                          (string= *conscious-storage-projector-revision*
                                   (gethash "projector_revision" relocated))
                          (plusp (length
                                  (gethash "event_source_binding"
                                  (gethash "state" relocated)))))))
        (event-authority-clear)
        (setf legacy-backend (make-sqlite-storage mismatch-database))
        (storage-append-event
         legacy-backend "user-message"
         (obj "text" "different fixture" "channel" "cli"
              "origin_runtime_revision" "conscious-q5-v2")
         :agent-id agent-id :occurred-at "2026-08-19T21:00:00Z")
        (storage-close legacy-backend)
        (setf legacy-backend nil)
        (sea-check
         "derived checkpoint cannot be paired with another event database"
         (sea-signals-p
          'storage-integrity-error
          (lambda ()
            (sqlite-event-authority-prepare
             mismatch-database nil :derived-database derived
             :agent-id agent-id)))))
    (when *event-authority-port* (ignore-errors (event-authority-clear)))
    (when legacy-backend (ignore-errors (storage-close legacy-backend)))
    (sea-delete-db database)
    (sea-delete-db mismatch-database)
    (sea-delete-db derived)))

(let* ((root (test-state-dir))
       (database (merge-pathnames "ledger-only-events.sqlite3" root))
       (derived (merge-pathnames "ledger-only-derived.sqlite3" root))
       (agent-id "ledger-only-fixture"))
  (sea-delete-db database)
  (sea-delete-db derived)
  (unwind-protect
       (progn
         (let ((source (make-sqlite-storage database))
               (rows (make-sqlite-derived-storage derived)))
           (unwind-protect
                (storage-append-event
                 source "user-message" (obj "text" "synthetic")
                 :agent-id agent-id)
             (storage-close rows)
             (storage-close source)))
         (dotimes (reopen 2)
           (multiple-value-bind (backend receipt)
               (sqlite-event-authority-prepare
                database nil :derived-database derived :agent-id agent-id
                :restore-projection-p nil)
             (declare (ignore backend))
             (sea-check "ledger-only reopen skips conscious checkpoint"
                        (and (string= "opened-ledger-only"
                                      (gethash "status" receipt))
                             (null (storage-load-checkpoint
                                    *sqlite-event-authority-checkpoint-backend*
                                    *conscious-storage-checkpoint-name*
                                    :agent-id agent-id))
                             (= 1 (length (replay-events)))))
             (sea-check "ledger-only projection requests fail closed"
                        (sea-signals-p
                         'storage-unavailable-error
                         (lambda () (event-projection-events))))
             (sea-check "ledger-only checkpoint refresh is unavailable"
                        (sea-signals-p
                         'storage-unavailable-error
                         (lambda () (sqlite-event-authority-checkpoint))))
             (event-authority-clear))))
    (when *event-authority-port* (ignore-errors (event-authority-clear)))
    (sea-delete-db database)
    (sea-delete-db derived)))

(format t "~%~d passed, ~d failed~%" *sea-pass* *sea-fail*)
(when (plusp *sea-fail*) (error "SQLite event authority tests failed"))
