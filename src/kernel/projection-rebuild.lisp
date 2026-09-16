;;;; projection-rebuild.lisp -- deterministic projection folds.
;;;; Source-only until separately sealed. The pure fold never writes durable
;;;; state; materialization requires an explicit destination root.

(in-package :agent)

(export '(projection-rebuild-file-specs
          make-projection-rebuild-baseline
          projection-rebuild-fold-files
          projection-rebuild-load-file-checkpoints
          projection-rebuild-write-files
          projection-rebuild-file-parity
          projection-rebuild-memory-specs
          make-projection-rebuild-row-baseline
          projection-rebuild-memory-checkpoint-content
          projection-rebuild-load-memory-checkpoints
          projection-rebuild-fold-memory
          make-projection-rebuild-event-source
          projection-rebuild-memory-table-map
          projection-rebuild-postgres-table-specs
          make-projection-rebuild-table-baseline
          projection-rebuild-table-checkpoint-content
          projection-rebuild-load-table-checkpoints
          projection-rebuild-fold-postgres-tables
          projection-rebuild-postgres-table-map
          projection-rebuild-write-postgres-inserts
          projection-rebuild-write-row-checkpoint-inserts
          projection-rebuild-write-exact-postgres-tail))

(defparameter *projection-rebuild-file-specs*
  '((:name "modulators" :type "projection-state" :file "modulators.json")
    (:name "drives" :type "projection-state" :file "drives.json")
    (:name "contact-log" :type "projection-state" :file "contact-log.json")
    (:name "ambient-recall-history" :type "projection-state"
     :file "ambient-recall-history.json")
    (:name "schedules" :type "projection-state" :file "schedules.json")
    (:name "scheduled-context" :type "projection-state"
     :file "scheduled-context.json")
    (:name "public-outbound-audit" :type "projection-state"
     :file "public-outbound-audit.json")
    (:name "conversation-history" :type "conversation-history-transform"
     :file "conversation.json"))
  "Declarative inventory for the first projection family.")

(defparameter *projection-rebuild-memory-specs*
  '((:name "memory-nodes" :type "memory-node-state")
    (:name "memory-edges" :type "memory-edge-state"))
  "Declarative inventory for the R0e2 relational memory family.")

(defparameter *projection-rebuild-postgres-table-specs*
  '((:table "memory_atom_rollouts" :keys ("rollout_id"))
    (:table "memory_atom_jobs" :keys ("id"))
    (:table "memory_atom_candidates" :keys ("candidate_id"))
    (:table "memory_atom_candidate_roots"
     :keys ("candidate_id" "evidence_id"))
    (:table "grounded_project_proposals" :keys ("id"))
    (:table "agent_processes" :keys ("id"))
    (:table "creative_projects" :keys ("id"))
    (:table "agent_process_operations" :keys ("id"))
    (:table "agent_artifacts" :keys ("id"))
    (:table "agent_artifact_versions" :keys ("artifact_id" "version"))
    (:table "agent_attestations" :keys ("id"))
    (:table "publication_candidates" :keys ("id")))
  "Allowlisted R0c5 table identities for the R0e3 generic fold.")

(defun projection-rebuild-file-specs ()
  (copy-tree *projection-rebuild-file-specs*))

(defun projection-rebuild-memory-specs ()
  (copy-tree *projection-rebuild-memory-specs*))

(defun projection-rebuild-postgres-table-specs ()
  (copy-tree *projection-rebuild-postgres-table-specs*))

(defun projection-rebuild-memory-table-map ()
  '(("memory-nodes" . "memory_nodes")
    ("memory-edges" . "memory_edges")))

(defun projection-rebuild-postgres-table-map
    (&optional (specs *projection-rebuild-postgres-table-specs*))
  (mapcar (lambda (spec)
            (cons (getf spec :table) (getf spec :table)))
          specs))

(defun make-projection-rebuild-event-source
    (&key after-id through-id types exclude-types)
  "Return a repeatable streaming source backed by MAP-EVENTS.
The returned function accepts one visitor and returns MAP-EVENTS' three
values: complete-p, last persisted event ID in range, and visited count."
  (unless (fboundp 'map-events)
    (error "MAP-EVENTS is not loaded"))
  (lambda (visitor)
    (map-events visitor
                :after-id after-id :through-id through-id
                :types types :exclude-types exclude-types)))

(defun %projection-rebuild-consume-events (events visitor)
  "Visit either a sequence or a streaming event-source function."
  (if (functionp events)
      (funcall events visitor)
      (progn
        (map nil visitor events)
        (values t nil (length events)))))

(defun %projection-rebuild-object (&rest pairs)
  (loop with result = (make-hash-table :test #'equal)
        for (key value) on pairs by #'cddr
        do (setf (gethash key result) value)
        finally (return result)))

(defun make-projection-rebuild-baseline (event-id file content)
  (unless (and (integerp event-id) (not (minusp event-id)))
    (error "Projection baseline event ID must be a non-negative integer"))
  (unless (and (stringp file) (string= file (file-namestring file)))
    (error "Projection baseline file must be a basename: ~s" file))
  (unless (stringp content)
    (error "Projection baseline content must be a string"))
  (%projection-rebuild-object
   "event_id" event-id "file" file "content" content "source" "checkpoint"))

(defun %projection-rebuild-spec (name specs)
  (find name specs :key (lambda (spec) (getf spec :name)) :test #'string=))

(defun %projection-rebuild-spec-by-type (type specs)
  (find type specs :key (lambda (spec) (getf spec :type)) :test #'string=))

(defun %projection-rebuild-copy-hash-table (table)
  (let ((copy (make-hash-table :test (hash-table-test table)
                               :size (max 1 (hash-table-count table)))))
    (maphash (lambda (key value) (setf (gethash key copy) value)) table)
    copy))

(defun %projection-rebuild-shasht-function (name)
  (unless (find-package :shasht)
    (ql:quickload :shasht :silent t))
  (let* ((package (find-package :shasht))
         (symbol (and package (find-symbol name package))))
    (unless (and symbol (fboundp symbol))
      (error "SHASHT function ~a is unavailable" name))
    (symbol-function symbol)))

(defun %projection-rebuild-gap (projection reason &optional event-id)
  (%projection-rebuild-object
   "projection" projection "reason" reason
   "event_id" (if event-id event-id :null)))

(defun %projection-rebuild-valid-state-p (state spec)
  (and (hash-table-p state)
       (integerp (gethash "event_id" state))
       (not (minusp (gethash "event_id" state)))
       (stringp (gethash "content" state))
       (stringp (gethash "file" state))
       (string= (gethash "file" state) (getf spec :file))))

(defun %projection-rebuild-valid-event-p (event payload spec)
  (and (hash-table-p event) (hash-table-p payload)
       (integerp (gethash "id" event)) (plusp (gethash "id" event))
       (stringp (gethash "type" event))
       (string= (gethash "type" event) (getf spec :type))
       (stringp (gethash "projection" payload))
       (string= (gethash "projection" payload) (getf spec :name))
       (stringp (gethash "operation" payload))
       (string= (gethash "operation" payload) "replace")
       (stringp (gethash "encoding" payload))
       (string= (gethash "encoding" payload) "utf-8")
       (stringp (gethash "file" payload))
       (string= (gethash "file" payload) (getf spec :file))
       (stringp (gethash "content" payload))))

(defun projection-rebuild-fold-files
    (events &key (baselines (make-hash-table :test #'equal))
                 (specs *projection-rebuild-file-specs*) expected-tail-event-id)
  "Purely fold file replacement EVENTS over verified BASELINES.
Returns one result object containing STATES, GAPS, COMPLETE, and LAST_EVENT_ID."
  (let ((states (make-hash-table :test #'equal))
        (gaps nil)
        (last-event-id nil)
        (source-complete-p t)
        (source-last-event-id nil)
        (checkpoint-event-ids nil))
    (dolist (spec specs)
      (let* ((name (getf spec :name)) (baseline (gethash name baselines)))
        (when baseline
          (if (%projection-rebuild-valid-state-p baseline spec)
              (progn
                (setf (gethash name states) baseline)
                (pushnew (gethash "event_id" baseline) checkpoint-event-ids))
              (push (%projection-rebuild-gap name "invalid-checkpoint") gaps)))))
    ;; One stopped-ledger boundary is essential to the bootstrap contract.
    ;; Differing latest checkpoints could omit intervening state for one
    ;; projection and must not be normalized silently.
    (when (> (length checkpoint-event-ids) 1)
      (push (%projection-rebuild-gap
             "checkpoint-boundary" "inconsistent-event-ids")
            gaps))
    (when checkpoint-event-ids
      (setf last-event-id (reduce #'max checkpoint-event-ids)))
    (multiple-value-bind (complete-p source-last-id visited-count)
        (%projection-rebuild-consume-events
         events
         (lambda (event)
           (let ((id (and (hash-table-p event) (gethash "id" event)))
            (type (and (hash-table-p event) (gethash "type" event)))
            (payload (and (hash-table-p event) (gethash "payload" event))))
        (when (integerp id)
          (when (and last-event-id (<= id last-event-id))
            (push (%projection-rebuild-gap "ledger-order" "non-increasing-event-id" id)
                  gaps))
          (setf last-event-id (if last-event-id (max last-event-id id) id)))
        (when (and (stringp type)
                   (member type '("projection-state"
                                  "conversation-history-transform")
                           :test #'string=))
          (let* ((name (and (hash-table-p payload)
                            (gethash "projection" payload)))
                 (spec (and (stringp name)
                            (%projection-rebuild-spec name specs))))
            (when spec
              (let ((current (gethash name states)))
                (if (%projection-rebuild-valid-event-p event payload spec)
                    (when (or (null current)
                              (> id (gethash "event_id" current)))
                      (setf (gethash name states)
                            (%projection-rebuild-object
                             "event_id" id "file" (gethash "file" payload)
                             "content" (gethash "content" payload)
                             "source" "event")))
                    (push (%projection-rebuild-gap
                           name "invalid-replacement-event" id)
                          gaps)))))))))
      (declare (ignore visited-count))
      (setf source-complete-p complete-p
            source-last-event-id source-last-id))
    (unless source-complete-p
      (push (%projection-rebuild-gap "event-source" "incomplete-scan") gaps))
    (when (integerp source-last-event-id)
      (setf last-event-id
            (if last-event-id
                (max last-event-id source-last-event-id)
                source-last-event-id)))
    (dolist (spec specs)
      (let ((name (getf spec :name)))
        (unless (gethash name states)
          (push (%projection-rebuild-gap name "missing-checkpoint-or-event") gaps))))
    (when (and expected-tail-event-id
               (not (eql expected-tail-event-id last-event-id)))
      (push (%projection-rebuild-gap "ledger-tail" "unexpected-last-event-id"
                                    last-event-id)
            gaps))
    (setf gaps (nreverse gaps))
    (%projection-rebuild-object
     "states" states "gaps" gaps "complete" (null gaps)
     "last_event_id" (or last-event-id :null))))

(defun projection-rebuild-load-file-checkpoints
    (&key (specs *projection-rebuild-file-specs*))
  "Load hash-verified checkpoints into baseline objects."
  (unless (fboundp 'read-verified-event-checkpoint)
    (error "READ-VERIFIED-EVENT-CHECKPOINT is not loaded"))
  (let ((baselines (make-hash-table :test #'equal)))
    (dolist (spec specs baselines)
      (multiple-value-bind (content manifest)
          (read-verified-event-checkpoint (getf spec :name))
        (when (and content manifest)
          (setf (gethash (getf spec :name) baselines)
                (make-projection-rebuild-baseline
                 (gethash "event_id" manifest) (getf spec :file) content)))))))

(defun projection-rebuild-write-files (result destination-root)
  "Materialize a COMPLETE result beneath explicit DESTINATION-ROOT."
  (unless (and (hash-table-p result) (gethash "complete" result))
    (error "Refusing to materialize an incomplete projection rebuild"))
  (let ((states (gethash "states" result)))
    (maphash
     (lambda (name state)
       (declare (ignore name))
       (let ((path (merge-pathnames (gethash "file" state) destination-root)))
         (ensure-directories-exist path)
         (with-open-file (out path :direction :output :if-exists :supersede
                                   :if-does-not-exist :create
                                   :external-format :utf-8)
           (write-string (gethash "content" state) out))))
     states))
  destination-root)

(defun %projection-rebuild-sha256 (content)
  (let ((octets (sb-ext:string-to-octets content :external-format :utf-8)))
    (if (fboundp '%event-sha256-octets)
        (%event-sha256-octets octets)
        (progn
          (unless (find-package :ironclad)
            (ql:quickload :ironclad :silent t))
          (let ((package (find-package :ironclad)))
            (string-downcase
             (funcall (intern "BYTE-ARRAY-TO-HEX-STRING" package)
                      (funcall (intern "DIGEST-SEQUENCE" package)
                               :sha256 octets))))))))

(defun projection-rebuild-file-parity (result live-root)
  "Return named per-file SHA-256 parity rows without writing state."
  (unless (and (hash-table-p result) (gethash "complete" result))
    (error "Refusing parity for an incomplete projection rebuild"))
  (let ((rows nil) (states (gethash "states" result)))
    (maphash
     (lambda (name state)
       (let* ((path (merge-pathnames (gethash "file" state) live-root))
              (rebuilt (gethash "content" state))
              (live (and (probe-file path) (uiop:read-file-string path)))
              (rebuilt-sha (%projection-rebuild-sha256 rebuilt))
              (live-sha (and live (%projection-rebuild-sha256 live))))
         (push (%projection-rebuild-object
                "projection" name "file" (gethash "file" state)
                "rebuilt_sha256" rebuilt-sha
                "live_sha256" (or live-sha :null)
                "equal" (and live-sha (string= rebuilt-sha live-sha)))
               rows)))
     states)
    (sort rows #'string< :key (lambda (row) (gethash "projection" row)))))

;;; --- R0e2 memory_nodes / memory_edges folds ----------------------------

(defun %projection-rebuild-memory-row-key (projection row)
  (when (hash-table-p row)
    (cond
      ((string= projection "memory-nodes")
       (let ((id (gethash "id" row)))
         (and (stringp id) (plusp (length id)) id)))
      ((string= projection "memory-edges")
       (let ((id (gethash "id" row)))
         (and (integerp id) (plusp id) id))))))

(defun make-projection-rebuild-row-baseline
    (event-id projection rows &key (specs *projection-rebuild-memory-specs*))
  "Create one validated relational baseline from complete row objects."
  (unless (and (integerp event-id) (not (minusp event-id)))
    (error "Projection baseline event ID must be a non-negative integer"))
  (unless (%projection-rebuild-spec projection specs)
    (error "Unknown relational projection: ~s" projection))
  (let ((table (make-hash-table :test #'equal)))
    (map nil
         (lambda (row)
           (let ((key (%projection-rebuild-memory-row-key projection row)))
             (unless key
               (error "Invalid ~a checkpoint row identity" projection))
             (when (nth-value 1 (gethash key table))
               (error "Duplicate ~a checkpoint row identity: ~s"
                      projection key))
             (setf (gethash key table) row)))
         rows)
    (%projection-rebuild-object
     "event_id" event-id "rows" table "source" "checkpoint")))

(defun projection-rebuild-memory-checkpoint-content (projection rows)
  "Encode complete relational checkpoint rows in the frozen R0e2 schema."
  (unless (%projection-rebuild-spec projection *projection-rebuild-memory-specs*)
    (error "Unknown relational projection: ~s" projection))
  (let ((*print-pretty* nil))
    (funcall (%projection-rebuild-shasht-function "WRITE-JSON")
             (%projection-rebuild-object
              "schema_version" 1 "projection" projection
              "rows" (if (vectorp rows) rows (coerce rows 'vector)))
             nil)))

(defun projection-rebuild-load-memory-checkpoints
    (&key (specs *projection-rebuild-memory-specs*))
  "Load and schema-validate hash-verified relational checkpoints."
  (unless (fboundp 'read-verified-event-checkpoint)
    (error "READ-VERIFIED-EVENT-CHECKPOINT is not loaded"))
  (let ((baselines (make-hash-table :test #'equal)))
    (dolist (spec specs baselines)
      (let ((name (getf spec :name)))
        (multiple-value-bind (content manifest)
            (read-verified-event-checkpoint name)
          (when (and content manifest)
            (let* ((document
                     (funcall (%projection-rebuild-shasht-function "READ-JSON")
                              content))
                   (rows (and (hash-table-p document)
                              (gethash "rows" document))))
              (unless (and (eql 1 (gethash "schema_version" document))
                           (stringp (gethash "projection" document))
                           (string= name (gethash "projection" document))
                           (or (listp rows) (vectorp rows)))
                (error "Invalid R0e2 checkpoint schema for ~a" name))
              (setf (gethash name baselines)
                    (make-projection-rebuild-row-baseline
                     (gethash "event_id" manifest) name rows :specs specs)))))))))

(defun %projection-rebuild-json-equal-p (left right)
  (cond
    ((and (hash-table-p left) (hash-table-p right))
     (and (= (hash-table-count left) (hash-table-count right))
          (block equal
            (maphash
             (lambda (key value)
               (multiple-value-bind (other present-p) (gethash key right)
                 (unless (and present-p
                              (%projection-rebuild-json-equal-p value other))
                   (return-from equal nil))))
             left)
            t)))
    ((and (vectorp left) (vectorp right))
     (and (= (length left) (length right))
          (loop for index below (length left)
                always (%projection-rebuild-json-equal-p
                        (aref left index) (aref right index)))))
    ((and (listp left) (listp right))
     (and (= (length left) (length right))
          (loop for l in left for r in right
                always (%projection-rebuild-json-equal-p l r))))
    ((and (numberp left) (numberp right)) (= left right))
    (t (equal left right))))

(defun %projection-rebuild-valid-memory-event-p (event payload spec)
  (let ((row (and (hash-table-p payload) (gethash "row" payload)))
        (operation (and (hash-table-p payload) (gethash "operation" payload)))
        (name (getf spec :name)))
    (and (hash-table-p event) (hash-table-p payload) (hash-table-p row)
         (integerp (gethash "id" event)) (plusp (gethash "id" event))
         (stringp (gethash "type" event))
         (string= (gethash "type" event) (getf spec :type))
         (stringp operation)
         (if (string= name "memory-nodes")
             (member operation '("upsert" "update") :test #'string=)
             (member operation '("insert" "delete") :test #'string=))
         (stringp (gethash "mutation_kind" payload))
         (%projection-rebuild-memory-row-key name row))))

(defun projection-rebuild-fold-memory
    (events &key (baselines (make-hash-table :test #'equal))
                 (specs *projection-rebuild-memory-specs*)
                 expected-tail-event-id)
  "Purely fold R0c2 full-row events over complete relational baselines.
EVENTS must be an ordered tail after the common checkpoint boundary."
  (let ((states (make-hash-table :test #'equal))
        (seen (make-hash-table :test #'equal))
        (checkpoint-event-ids nil)
        (gaps nil)
        (source-complete-p t)
        (source-last-event-id nil)
        (last-event-id nil))
    (dolist (spec specs)
      (let* ((name (getf spec :name)) (baseline (gethash name baselines)))
        (when baseline
          (let ((event-id (gethash "event_id" baseline))
                (rows (gethash "rows" baseline)))
            (if (and (integerp event-id) (not (minusp event-id))
                     (hash-table-p rows))
                (progn
                  (setf (gethash name states)
                        (%projection-rebuild-object
                         "event_id" event-id
                         "rows" (%projection-rebuild-copy-hash-table rows)
                         "source" "checkpoint")
                        (gethash name seen) t)
                  (pushnew event-id checkpoint-event-ids))
                (push (%projection-rebuild-gap name "invalid-checkpoint") gaps))))))
    (when (> (length checkpoint-event-ids) 1)
      (push (%projection-rebuild-gap
             "checkpoint-boundary" "inconsistent-event-ids") gaps))
    (when checkpoint-event-ids
      (setf last-event-id (reduce #'max checkpoint-event-ids)))
    (multiple-value-bind (complete-p source-last-id visited-count)
        (%projection-rebuild-consume-events
         events
         (lambda (event)
           (let ((id (and (hash-table-p event) (gethash "id" event)))
            (type (and (hash-table-p event) (gethash "type" event)))
            (payload (and (hash-table-p event) (gethash "payload" event))))
        (when (integerp id)
          (when (and last-event-id (<= id last-event-id))
            (push (%projection-rebuild-gap
                   "ledger-order" "non-increasing-event-id" id) gaps))
          (setf last-event-id (if last-event-id (max last-event-id id) id)))
        (when (stringp type)
          (when (string= type "memory-operation-state")
            ;; The legacy R0e2 fold understands one PostgreSQL row per event.
            ;; Never silently skip the newer atomic command envelope.
            (push (%projection-rebuild-gap
                   "memory-operation"
                   "atomic-envelope-requires-event-first-projector" id)
                  gaps))
          (unless (string= type "memory-operation-state")
           (let ((spec (%projection-rebuild-spec-by-type type specs)))
            (when spec
              (let ((name (getf spec :name)))
                (if (%projection-rebuild-valid-memory-event-p event payload spec)
                    (let* ((state (or (gethash name states)
                                      (%projection-rebuild-object
                                       "event_id" 0
                                       "rows" (make-hash-table :test #'equal)
                                       "source" "event")))
                           (rows (gethash "rows" state))
                           (row (gethash "row" payload))
                           (key (%projection-rebuild-memory-row-key name row))
                           (operation (gethash "operation" payload))
                           (current (gethash key rows))
                           (applied-p nil))
                      (cond
                        ((string= name "memory-nodes")
                         (if (and (string= operation "update") (null current))
                             (push (%projection-rebuild-gap
                                    name "update-without-prior-row" id) gaps)
                             (setf (gethash key rows) row
                                   applied-p t)))
                        ((string= operation "insert")
                         (if current
                             (push (%projection-rebuild-gap
                                    name "duplicate-edge-insert" id) gaps)
                             (setf (gethash key rows) row
                                   applied-p t)))
                        (t
                         (cond
                           ((null current)
                            (push (%projection-rebuild-gap
                                   name "delete-without-prior-row" id) gaps))
                           ((not (%projection-rebuild-json-equal-p current row))
                            (push (%projection-rebuild-gap
                                   name "delete-row-mismatch" id) gaps))
                           (t (remhash key rows)
                              (setf applied-p t)))))
                      (when applied-p
                        (setf (gethash "event_id" state) id
                              (gethash "source" state) "event"
                              (gethash name states) state
                              (gethash name seen) t)))
                    (push (%projection-rebuild-gap
                           name "invalid-row-event" id) gaps))))))))))
      (declare (ignore visited-count))
      (setf source-complete-p complete-p
            source-last-event-id source-last-id))
    (unless source-complete-p
      (push (%projection-rebuild-gap "event-source" "incomplete-scan") gaps))
    (when (integerp source-last-event-id)
      (setf last-event-id
            (if last-event-id
                (max last-event-id source-last-event-id)
                source-last-event-id)))
    (dolist (spec specs)
      (let ((name (getf spec :name)))
        (unless (gethash name seen)
          (push (%projection-rebuild-gap
                 name "missing-checkpoint-or-event") gaps))))
    (when (and expected-tail-event-id
               (not (eql expected-tail-event-id last-event-id)))
      (push (%projection-rebuild-gap
             "ledger-tail" "unexpected-last-event-id" last-event-id) gaps))
    (setf gaps (nreverse gaps))
    (%projection-rebuild-object
     "states" states "gaps" gaps "complete" (null gaps)
     "last_event_id" (or last-event-id :null))))

;;; --- R0e3 generic R0c5 PostgreSQL table folds --------------------------

(defun %projection-rebuild-table-spec (table specs)
  (find table specs :key (lambda (spec) (getf spec :table)) :test #'string=))

(defun %projection-rebuild-present-json-value-p (object key)
  (multiple-value-bind (value present-p) (gethash key object)
    (and present-p (not (or (null value) (eq value :null))))))

(defun %projection-rebuild-table-key-from-object (spec object)
  (when (and (hash-table-p object)
             (every (lambda (field)
                      (%projection-rebuild-present-json-value-p object field))
                    (getf spec :keys)))
    (mapcar (lambda (field) (gethash field object)) (getf spec :keys))))

(defun make-projection-rebuild-table-baseline
    (event-id table rows &key (specs *projection-rebuild-postgres-table-specs*))
  "Create one validated complete-table baseline for the generic R0e3 fold."
  (unless (and (integerp event-id) (not (minusp event-id)))
    (error "Projection baseline event ID must be a non-negative integer"))
  (let ((spec (%projection-rebuild-table-spec table specs))
        (row-table (make-hash-table :test #'equal)))
    (unless spec (error "Unknown R0e3 table: ~s" table))
    (map nil
         (lambda (row)
           (let ((key (%projection-rebuild-table-key-from-object spec row)))
             (unless key (error "Invalid ~a checkpoint primary key" table))
             (when (nth-value 1 (gethash key row-table))
               (error "Duplicate ~a checkpoint primary key: ~s" table key))
             (setf (gethash key row-table) row)))
         rows)
    (%projection-rebuild-object
     "event_id" event-id "rows" row-table "source" "checkpoint")))

(defun projection-rebuild-table-checkpoint-content (table rows)
  "Encode one complete R0e3 table checkpoint in its frozen schema."
  (unless (%projection-rebuild-table-spec
           table *projection-rebuild-postgres-table-specs*)
    (error "Unknown R0e3 table: ~s" table))
  (let ((*print-pretty* nil))
    (funcall (%projection-rebuild-shasht-function "WRITE-JSON")
             (%projection-rebuild-object
              "schema_version" 1 "projection" "postgres-table"
              "table" table
              "rows" (if (vectorp rows) rows (coerce rows 'vector)))
             nil)))

(defun projection-rebuild-load-table-checkpoints
    (&key (specs *projection-rebuild-postgres-table-specs*))
  "Load and schema-validate all available R0e3 table checkpoints."
  (unless (fboundp 'read-verified-event-checkpoint)
    (error "READ-VERIFIED-EVENT-CHECKPOINT is not loaded"))
  (let ((baselines (make-hash-table :test #'equal)))
    (dolist (spec specs baselines)
      (let ((table (getf spec :table)))
        (multiple-value-bind (content manifest)
            (read-verified-event-checkpoint table)
          (when (and content manifest)
            (let* ((document
                     (funcall (%projection-rebuild-shasht-function "READ-JSON")
                              content))
                   (rows (and (hash-table-p document)
                              (gethash "rows" document))))
              (unless (and (hash-table-p document)
                           (eql 1 (gethash "schema_version" document))
                           (string= "postgres-table"
                                    (gethash "projection" document))
                           (stringp (gethash "table" document))
                           (string= table (gethash "table" document))
                           (or (listp rows) (vectorp rows)))
                (error "Invalid R0e3 checkpoint schema for ~a" table))
              (setf (gethash table baselines)
                    (make-projection-rebuild-table-baseline
                     (gethash "event_id" manifest) table rows
                     :specs specs)))))))))

(defun %projection-rebuild-valid-table-event-p (event payload spec)
  (let* ((primary-key (and (hash-table-p payload)
                           (gethash "primary_key" payload)))
         (row (and (hash-table-p payload) (gethash "row" payload)))
         (key (%projection-rebuild-table-key-from-object spec primary-key))
         (row-key (%projection-rebuild-table-key-from-object spec row)))
    (and (hash-table-p event) (hash-table-p payload)
         (integerp (gethash "id" event)) (plusp (gethash "id" event))
         (stringp (gethash "projection" payload))
         (string= "postgres" (gethash "projection" payload))
         (stringp (gethash "table" payload))
         (string= (getf spec :table) (gethash "table" payload))
         (stringp (gethash "operation" payload))
         (string= "upsert" (gethash "operation" payload))
         (hash-table-p primary-key) (hash-table-p row)
         key row-key (%projection-rebuild-json-equal-p key row-key))))

(defun projection-rebuild-fold-postgres-tables
    (events &key (baselines (make-hash-table :test #'equal))
                 (specs *projection-rebuild-postgres-table-specs*)
                 expected-tail-event-id)
  "Purely fold allowlisted R0c5 complete-row upserts over table checkpoints."
  (let ((states (make-hash-table :test #'equal))
        (seen (make-hash-table :test #'equal))
        (checkpoint-event-ids nil)
        (gaps nil)
        (source-complete-p t)
        (source-last-event-id nil)
        (last-event-id nil))
    (dolist (spec specs)
      (let* ((table (getf spec :table)) (baseline (gethash table baselines)))
        (when baseline
          (let ((event-id (gethash "event_id" baseline))
                (rows (gethash "rows" baseline)))
            (if (and (integerp event-id) (not (minusp event-id))
                     (hash-table-p rows))
                (progn
                  (setf (gethash table states)
                        (%projection-rebuild-object
                         "event_id" event-id
                         "rows" (%projection-rebuild-copy-hash-table rows)
                         "source" "checkpoint")
                        (gethash table seen) t)
                  (pushnew event-id checkpoint-event-ids))
                (push (%projection-rebuild-gap table "invalid-checkpoint")
                      gaps))))))
    (when (> (length checkpoint-event-ids) 1)
      (push (%projection-rebuild-gap
             "checkpoint-boundary" "inconsistent-event-ids") gaps))
    (when checkpoint-event-ids
      (setf last-event-id (reduce #'max checkpoint-event-ids)))
    (multiple-value-bind (complete-p source-last-id visited-count)
        (%projection-rebuild-consume-events
         events
         (lambda (event)
           (let ((id (and (hash-table-p event) (gethash "id" event)))
            (type (and (hash-table-p event) (gethash "type" event)))
            (payload (and (hash-table-p event) (gethash "payload" event))))
        (when (integerp id)
          (when (and last-event-id (<= id last-event-id))
            (push (%projection-rebuild-gap
                   "ledger-order" "non-increasing-event-id" id) gaps))
          (setf last-event-id (if last-event-id (max last-event-id id) id)))
        (when (and (stringp type) (string= type "postgres-row-state"))
          (let* ((table (and (hash-table-p payload)
                             (gethash "table" payload)))
                 (spec (and (stringp table)
                            (%projection-rebuild-table-spec table specs))))
            (cond
              ((null spec)
               (push (%projection-rebuild-gap
                      "postgres-table-registry" "unregistered-table" id)
                     gaps))
              ((%projection-rebuild-valid-table-event-p event payload spec)
               (let* ((state (or (gethash table states)
                                 (%projection-rebuild-object
                                  "event_id" 0
                                  "rows" (make-hash-table :test #'equal)
                                  "source" "event")))
                      (rows (gethash "rows" state))
                      (row (gethash "row" payload))
                      (key (%projection-rebuild-table-key-from-object
                            spec (gethash "primary_key" payload))))
                 (setf (gethash key rows) row
                       (gethash "event_id" state) id
                       (gethash "source" state) "event"
                       (gethash table states) state
                       (gethash table seen) t)))
              (t
               (push (%projection-rebuild-gap
                      table "invalid-table-row-event" id) gaps))))))))
      (declare (ignore visited-count))
      (setf source-complete-p complete-p
            source-last-event-id source-last-id))
    (unless source-complete-p
      (push (%projection-rebuild-gap "event-source" "incomplete-scan") gaps))
    (when (integerp source-last-event-id)
      (setf last-event-id
            (if last-event-id
                (max last-event-id source-last-event-id)
                source-last-event-id)))
    (dolist (spec specs)
      (let ((table (getf spec :table)))
        (unless (gethash table seen)
          (push (%projection-rebuild-gap
                 table "missing-checkpoint-or-event") gaps))))
    (when (and expected-tail-event-id
               (not (eql expected-tail-event-id last-event-id)))
      (push (%projection-rebuild-gap
             "ledger-tail" "unexpected-last-event-id" last-event-id) gaps))
    (setf gaps (nreverse gaps))
    (%projection-rebuild-object
     "states" states "gaps" gaps "complete" (null gaps)
     "last_event_id" (or last-event-id :null))))

;;; --- R0e4 disposable PostgreSQL materialization -----------------------

(defun %projection-rebuild-safe-sql-identifier-p (value)
  (and (stringp value) (plusp (length value))
       (or (alpha-char-p (char value 0)) (char= #\_ (char value 0)))
       (every (lambda (character)
                (or (alphanumericp character) (char= character #\_)))
              value)))

(defun %projection-rebuild-sql-identifier (value)
  (unless (%projection-rebuild-safe-sql-identifier-p value)
    (error "Unsafe SQL identifier: ~s" value))
  (format nil "\"~a\"" value))

(defun %projection-rebuild-sql-literal (value)
  (with-output-to-string (out)
    (write-char #\' out)
    (loop for character across value
          do (write-char character out)
             (when (char= character #\') (write-char #\' out)))
    (write-char #\' out)))

(defun %projection-rebuild-json-string (value)
  (let ((*print-pretty* nil))
    (funcall (%projection-rebuild-shasht-function "WRITE-JSON") value nil)))

(defun %projection-rebuild-sorted-state-rows (state)
  (let (rows)
    (maphash (lambda (key row)
               (push (cons (prin1-to-string key) row) rows))
             (gethash "rows" state))
    (mapcar #'cdr (sort rows #'string< :key #'car))))

(defun projection-rebuild-write-postgres-inserts
    (result pathname table-map
     &key (destination-schema "r0e4_rebuild") (source-schema "public"))
  "Write deterministic INSERT statements for a COMPLETE result.
TABLE-MAP associates result state names with allowlisted PostgreSQL tables.
The caller owns schema creation and the transaction; PUBLIC is rejected as a
destination so this adapter cannot target the live projection schema."
  (unless (and (hash-table-p result) (gethash "complete" result))
    (error "Refusing to materialize an incomplete PostgreSQL rebuild"))
  (when (string= destination-schema "public")
    (error "Refusing to materialize into the public schema"))
  (let ((destination (%projection-rebuild-sql-identifier destination-schema))
        (source (%projection-rebuild-sql-identifier source-schema))
        (states (gethash "states" result)))
    (ensure-directories-exist pathname)
    (with-open-file (out pathname :direction :output :if-exists :supersede
                                  :if-does-not-exist :create
                                  :external-format :utf-8)
      (dolist (mapping table-map)
        (let* ((state-name (car mapping))
               (table-name (cdr mapping))
               (state (gethash state-name states))
               (table (%projection-rebuild-sql-identifier table-name)))
          (unless (and state (hash-table-p (gethash "rows" state)))
            (error "Missing materializable state: ~a" state-name))
          (dolist (row (%projection-rebuild-sorted-state-rows state))
            (format out
                    "INSERT INTO ~a.~a SELECT * FROM json_populate_record(NULL::~a.~a, ~a::json);~%"
                    destination table source table
                    (%projection-rebuild-sql-literal
                     (%projection-rebuild-json-string row))))))))
  pathname)

(defun projection-rebuild-write-row-checkpoint-inserts
    (projection pathname table-name
     &key (destination-schema "r0e4_rebuild") (source-schema "public"))
  "Stream one verified JSONL checkpoint into a deterministic SQL spool.
The spool is atomically published only after checkpoint verification and full
delivery. The caller owns schema creation, transaction, parity, and rollback."
  (unless (fboundp 'map-verified-event-row-checkpoint-lines)
    (error "MAP-VERIFIED-EVENT-ROW-CHECKPOINT-LINES is not loaded"))
  (when (string= destination-schema "public")
    (error "Refusing to materialize into the public schema"))
  (let* ((destination (%projection-rebuild-sql-identifier destination-schema))
         (source (%projection-rebuild-sql-identifier source-schema))
         (table (%projection-rebuild-sql-identifier table-name))
         (temporary
           (make-pathname
            :name (format nil "~a-tmp-~d-~d" (pathname-name pathname)
                          (get-universal-time) (random 1000000))
            :type (pathname-type pathname) :defaults pathname)))
    (ensure-directories-exist pathname)
    (unwind-protect
        (let ((complete nil) (manifest nil) (visited 0))
          (with-open-file (out temporary :direction :output
                                        :if-exists :supersede
                                        :if-does-not-exist :create
                                        :external-format :utf-8)
            (multiple-value-setq (complete manifest visited)
              (map-verified-event-row-checkpoint-lines
               projection
               (lambda (line)
                 (format out
                         "INSERT INTO ~a.~a SELECT * FROM json_populate_record(NULL::~a.~a, ~a::json);~%"
                         destination table source table
                         (%projection-rebuild-sql-literal
                          line)))))
            (finish-output out))
          (unless complete
            (error "Verified row checkpoint delivery failed for ~a" projection))
          (uiop:rename-file-overwriting-target temporary pathname)
          (values pathname manifest visited))
      (when (probe-file temporary)
        (ignore-errors (delete-file temporary))))))

;;; --- R0e5 opaque exact PostgreSQL tail materialization ----------------

(defun %projection-rebuild-exact-tail-spec (type payload)
  "Return the allowlisted relational mutation shape for TYPE and PAYLOAD."
  (cond
    ((and (stringp type) (string= type "memory-node-state"))
     '(:table "memory_nodes" :keys ("id") :operations ("upsert" "update")))
    ((and (stringp type) (string= type "memory-edge-state"))
     '(:table "memory_edges" :keys ("id") :operations ("insert" "delete")))
    ((and (stringp type) (string= type "postgres-row-state"))
     (let* ((table (and (hash-table-p payload) (gethash "table" payload)))
            (spec (and (stringp table)
                       (%projection-rebuild-table-spec
                        table *projection-rebuild-postgres-table-specs*))))
       (and spec
            (list :table table :keys (copy-list (getf spec :keys))
                  :operations '("upsert")))))
    (t nil)))

(defun %projection-rebuild-exact-tail-relevant-type-p (type)
  (and (stringp type)
       (member type '("memory-node-state" "memory-edge-state"
                      "memory-operation-state" "postgres-row-state")
               :test #'string=)))

(defun %projection-rebuild-exact-tail-row-key (spec row)
  (when (hash-table-p row)
    (let ((keys (getf spec :keys)))
      (when (every (lambda (field)
                     (%projection-rebuild-present-json-value-p row field))
                   keys)
        (mapcar (lambda (field) (gethash field row)) keys)))))

(defun %projection-rebuild-exact-tail-gap (reason event-id)
  (format nil "~a@~a" reason (or event-id "unknown")))

(defun %projection-rebuild-parse-exact-row-json (row-json)
  (handler-case
      (let ((row
              (funcall (%projection-rebuild-shasht-function "READ-JSON")
                       row-json)))
        (and (hash-table-p row) row))
    (error () nil)))

(defun %projection-rebuild-exact-row-compatible-p (left right)
  "Compare parsed compatibility data without making it the emitted source.
JSON envelope round-trips can narrow a floating value even though ROW_JSON
retains the exact PostgreSQL spelling. Permit only that bounded numeric drift;
all object shape, strings, booleans, arrays, and integer values remain exact."
  (cond
    ((and (hash-table-p left) (hash-table-p right))
     (and (= (hash-table-count left) (hash-table-count right))
          (block compatible
            (maphash
             (lambda (key value)
               (multiple-value-bind (other present-p) (gethash key right)
                 (unless (and present-p
                              (%projection-rebuild-exact-row-compatible-p
                               value other))
                   (return-from compatible nil))))
             left)
            t)))
    ((and (vectorp left) (vectorp right))
     (and (= (length left) (length right))
          (loop for index below (length left)
                always (%projection-rebuild-exact-row-compatible-p
                        (aref left index) (aref right index)))))
    ((and (listp left) (listp right))
     (and (= (length left) (length right))
          (loop for l in left for r in right
                always (%projection-rebuild-exact-row-compatible-p l r))))
    ((and (integerp left) (integerp right)) (= left right))
    ((and (numberp left) (numberp right))
     (let* ((l (coerce left 'double-float))
            (r (coerce right 'double-float))
            (difference (abs (- l r)))
            (scale (max (abs l) (abs r))))
       (<= difference (+ 1d-15 (* 2d-7 scale)))))
    (t (equal left right))))

(defun %projection-rebuild-write-exact-row-sql
    (out destination source spec operation row-json)
  "Write one validated mutation using ROW-JSON without reserializing it."
  (let* ((table (%projection-rebuild-sql-identifier (getf spec :table)))
         (keys (getf spec :keys))
         (literal (%projection-rebuild-sql-literal row-json)))
    (format out "WITH r AS (SELECT * FROM json_populate_record(NULL::~a.~a, ~a::json)) DELETE FROM ~a.~a AS d USING r WHERE "
            source table literal destination table)
    (loop for field in keys for first = t then nil
          do (unless first (write-string " AND " out))
             (let ((identifier (%projection-rebuild-sql-identifier field)))
               (format out "d.~a IS NOT DISTINCT FROM r.~a"
                       identifier identifier)))
    (format out ";~%")
    (unless (string= operation "delete")
      (format out
              "INSERT INTO ~a.~a SELECT * FROM json_populate_record(NULL::~a.~a, ~a::json);~%"
              destination table source table literal))))

(defun projection-rebuild-write-exact-postgres-tail
    (events pathname expected-tail-event-id
     &key (destination-schema "r0e5_rebuild") (source-schema "public"))
  "Atomically stream spelling-exact relational tail mutations to a SQL spool.
EVENTS is a sequence or event-source function. Relevant legacy events lacking
ROW_JSON fail closed; the parsed ROW remains validation data only."
  (unless (and (integerp expected-tail-event-id)
               (not (minusp expected-tail-event-id)))
    (error "Exact PostgreSQL tail requires a non-negative terminal event ID"))
  (when (string= destination-schema "public")
    (error "Refusing to materialize into the public schema"))
  (let* ((destination (%projection-rebuild-sql-identifier destination-schema))
         (source (%projection-rebuild-sql-identifier source-schema))
         (temporary
           (make-pathname
            :name (format nil "~a-tmp-~d-~d" (pathname-name pathname)
                          (get-universal-time) (random 1000000))
            :type (pathname-type pathname) :defaults pathname))
         (gaps nil)
         (last-visited-id nil)
         (source-last-id nil)
         (source-complete-p nil)
         (written 0))
    (ensure-directories-exist pathname)
    (unwind-protect
        (progn
          (with-open-file (out temporary :direction :output
                                        :if-exists :supersede
                                        :if-does-not-exist :create
                                        :external-format :utf-8)
            (multiple-value-bind (complete-p terminal-id visited-count)
                (%projection-rebuild-consume-events
                 events
                 (lambda (event)
                   (let* ((id (and (hash-table-p event) (gethash "id" event)))
                          (type (and (hash-table-p event)
                                     (gethash "type" event)))
                          (payload (and (hash-table-p event)
                                        (gethash "payload" event))))
                     (cond
                       ((not (and (integerp id) (plusp id)))
                        (push (%projection-rebuild-exact-tail-gap
                               "invalid-event-id" id) gaps))
                       ((and last-visited-id (<= id last-visited-id))
                        (push (%projection-rebuild-exact-tail-gap
                               "non-increasing-event-id" id) gaps))
                       (t (setf last-visited-id id)))
                     (when (%projection-rebuild-exact-tail-relevant-type-p type)
                       (let* ((spec (%projection-rebuild-exact-tail-spec
                                     type payload))
                              (operation (and (hash-table-p payload)
                                              (gethash "operation" payload)))
                              (row (and (hash-table-p payload)
                                        (gethash "row" payload)))
                              (row-json (and (hash-table-p payload)
                                             (gethash "row_json" payload)))
                              (raw-row (and (stringp row-json)
                                            (%projection-rebuild-parse-exact-row-json
                                             row-json))))
                         (cond
                           ((null spec)
                            (push (%projection-rebuild-exact-tail-gap
                                   "unregistered-table" id) gaps))
                           ((not (and (stringp operation)
                                      (member operation (getf spec :operations)
                                              :test #'string=)))
                            (push (%projection-rebuild-exact-tail-gap
                                   "invalid-operation" id) gaps))
                           ((not (stringp row-json))
                            (push (%projection-rebuild-exact-tail-gap
                                   "missing-exact-row-json" id) gaps))
                           ((null raw-row)
                            (push (%projection-rebuild-exact-tail-gap
                                   "malformed-exact-row-json" id) gaps))
                           ((not (and (hash-table-p row)
                                      (%projection-rebuild-exact-row-compatible-p
                                       raw-row row)))
                            (push (%projection-rebuild-exact-tail-gap
                                   "exact-row-mismatch" id) gaps))
                           ((not (%projection-rebuild-exact-tail-row-key
                                  spec raw-row))
                            (push (%projection-rebuild-exact-tail-gap
                                   "missing-row-primary-key" id) gaps))
                           ((and (string= type "postgres-row-state")
                                 (let ((primary-key
                                         (gethash "primary_key" payload)))
                                   (not (and
                                         (hash-table-p primary-key)
                                         (%projection-rebuild-json-equal-p
                                          (%projection-rebuild-exact-tail-row-key
                                           spec primary-key)
                                          (%projection-rebuild-exact-tail-row-key
                                           spec raw-row))))))
                            (push (%projection-rebuild-exact-tail-gap
                                   "primary-key-mismatch" id) gaps))
                           (t
                            (%projection-rebuild-write-exact-row-sql
                             out destination source spec operation row-json)
                            (incf written))))))))
              (declare (ignore visited-count))
              (setf source-complete-p complete-p
                    source-last-id (or terminal-id last-visited-id)))
            (finish-output out))
          (unless source-complete-p
            (push "incomplete-scan" gaps))
          (unless (eql expected-tail-event-id source-last-id)
            (push (format nil "unexpected-last-event-id@~a"
                          (or source-last-id "unknown")) gaps))
          (when gaps
            (error "Exact PostgreSQL tail refused: ~{~a~^, ~}"
                   (nreverse gaps)))
          (uiop:rename-file-overwriting-target temporary pathname)
          (values pathname written source-last-id))
      (when (probe-file temporary)
        (ignore-errors (delete-file temporary))))))
