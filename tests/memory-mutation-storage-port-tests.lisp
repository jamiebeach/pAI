(in-package :agent)

(ql:quickload '(:cffi :shasht :ironclad :bordeaux-threads) :silent t)

(defvar *memory-mutation-port-pass* 0)
(defvar *memory-mutation-port-fail* 0)

(defun memory-mutation-port-check (name condition)
  (if condition
      (progn (incf *memory-mutation-port-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *memory-mutation-port-fail*) (format t "  FAIL ~a~%" name))))

(dolist (file '("storage-substrate.lisp" "memory-storage.lisp"
                "sqlite-storage.lisp" "sqlite-derived-storage.lisp"))
  (load (test-source file)))

(defclass memory-mutation-source (memory-storage-backend) ())

(defclass memory-mutation-failing-projection (memory-storage-backend)
  ((inner :initarg :inner :reader memory-mutation-inner)
   (fail-next-p :initform nil :accessor memory-mutation-fail-next-p)
   (apply-count :initform 0 :accessor memory-mutation-apply-count)))

(defmethod memory-storage-projection-report
    ((backend memory-mutation-failing-projection))
  (memory-storage-projection-report (memory-mutation-inner backend)))

(defmethod memory-storage-apply-mutation
    ((backend memory-mutation-failing-projection) mutation)
  (incf (memory-mutation-apply-count backend))
  (when (memory-mutation-fail-next-p backend)
    (setf (memory-mutation-fail-next-p backend) nil)
    (error 'memory-storage-error :operation :fixture-projection-failure
           :detail "injected after durable append"))
  (memory-storage-apply-mutation (memory-mutation-inner backend) mutation))

(defmethod memory-storage-capabilities ((source memory-mutation-source))
  (declare (ignore source))
  (%memory-storage-object
   "schema_version" 1 "backend" "mutation-fixture"
   "read_snapshot" t "node_snapshot" t "edge_snapshot" t
   "exact_vector_export" t))

(defun memory-mutation-vector (x y)
  (format nil "00020000~8,'0x~8,'0x"
          (if (zerop x) 0 #x3f800000) (if (zerop y) 0 #x3f800000)))

(defun memory-mutation-node (id content x y)
  (list (format nil
                "{\"id\":\"~a\",\"kind\":\"observation\",\"content\":\"~a\",\"is_cold\":false,\"quarantined\":false,\"origin_class\":\"lived-user\",\"epistemic_status\":\"asserted\",\"grounding_status\":\"grounded\",\"epistemic_metadata\":{}}"
                id content)
        (memory-mutation-vector x y)))

(defmethod memory-storage-map-snapshot
    ((source memory-mutation-source) node-visitor edge-visitor)
  (declare (ignore source edge-visitor))
  (let ((nodes (list (memory-mutation-node "a" "alpha" 1 0)
                     (memory-mutation-node "b" "beta" 0 1)))
        (node-digest (ironclad:make-digest :sha256))
        (vector-digest (ironclad:make-digest :sha256))
        (edge-digest (ironclad:make-digest :sha256)))
    (dolist (node nodes)
      (%derived-digest-update node-digest (first node))
      (%derived-digest-update vector-digest (string-downcase (second node)))
      (%derived-digest-update vector-digest (string-downcase (second node)))
      (funcall node-visitor
               (%memory-storage-object
                "scalar_json" (first node)
                "embedding_binary_hex" (second node)
                "retrieval_embedding_binary_hex" (second node))))
    (%memory-storage-object
     "schema_version" 1 "node_count" 2 "edge_count" 0
     "node_sha256" (%derived-digest-hex node-digest)
     "vector_sha256" (%derived-digest-hex vector-digest)
     "edge_sha256" (%derived-digest-hex edge-digest)
     "vector_binary_encoding" "pgvector-send-v1")))

(defun memory-mutation-provenance ()
  (make-memory-embedding-provenance
   :embedding-model "fixture" :embedding-revision "fixture-v1"
   :retrieval-embedding-model "fixture"
   :retrieval-embedding-revision "fixture-v1"
   :vector-dimension 2 :revision-evidence "deterministic-fixture"
   :approval-scope "qualification-only"))

(defun memory-mutation-delete-db (path)
  (dolist (candidate (list path
                           (pathname (concatenate 'string (namestring path) "-wal"))
                           (pathname (concatenate 'string (namestring path) "-shm"))))
    (when (probe-file candidate) (delete-file candidate))))

(defun memory-mutation-payload-node (id content operation)
  (let ((node (memory-mutation-node id content 1 0)))
    (%memory-storage-object
     "operation" operation "mutation_kind" "fixture-node"
     "scalar_json" (first node) "embedding_binary_hex" (second node)
     "retrieval_embedding_binary_hex" (second node))))

(defun memory-mutation-event
    (id position type payload &optional (storage "ledger-1"))
  (let ((event (%memory-storage-object
                "schema_version" 1 "id" id "agent_id" "default"
                "timestamp" "fixture-time" "type" type "payload" payload
                "caused_by" :null "tick_id" :null "affect_snapshot" :null)))
    (make-memory-storage-mutation
     :event-json (%storage-json event) :storage-position position
     :storage-id storage)))

(defun memory-mutation-scalar (backend id)
  (%with-sqlite-statement
      (statement (%sqlite-derived-handle backend :fixture-read)
                 "SELECT scalar_json FROM pai_memory_nodes WHERE id=?1"
                 :fixture-read)
    (%sqlite-bind-text (%sqlite-derived-handle backend :fixture-read)
                       statement 1 id :fixture-read)
    (and (= (%sqlite-step-raw statement) +sqlite-row+)
         (%sqlite-column-text statement 0))))

(defun memory-mutation-stored-node (backend id)
  (%with-sqlite-statement
      (statement (%sqlite-derived-handle backend :fixture-read)
                 "SELECT scalar_json,embedding,retrieval_embedding,integrity_hash FROM pai_memory_nodes WHERE id=?1"
                 :fixture-read)
    (%sqlite-bind-text (%sqlite-derived-handle backend :fixture-read)
                       statement 1 id :fixture-read)
    (when (= (%sqlite-step-raw statement) +sqlite-row+)
      (list (%sqlite-column-text statement 0)
            (%sqlite-column-blob statement 1)
            (%sqlite-column-blob statement 2)
            (%sqlite-column-text statement 3)))))

(defun memory-mutation-edge-count (backend)
  (let ((handle (%sqlite-derived-handle backend :fixture-read)))
    (%with-sqlite-statement
      (statement handle
                 "SELECT count(*) FROM pai_memory_edges" :fixture-read)
      (%sqlite-step handle statement :fixture-read +sqlite-row+)
      (%sqlite-column-int64 statement 0))))

(format t "~%== event-applied memory mutation storage port ==~%")

(let ((unsupported (make-instance 'memory-storage-backend)))
  (memory-mutation-port-check
   "unsupported mutation operations fail closed"
   (and (handler-case
            (progn (memory-storage-projection-report unsupported) nil)
          (memory-storage-unsupported-error () t))
        (handler-case
            (progn (memory-storage-apply-mutation unsupported (make-hash-table)) nil)
          (memory-storage-unsupported-error () t))
        (handler-case
            (progn
              (make-memory-storage-mutation
               :event-json
               (%storage-json
                (%memory-storage-object
                 "schema_version" 1 "id" 1 "agent_id" "default"
                 "type" "memory-edge-state" "payload"
                 (%memory-storage-object
                  "operation" "insert" "mutation_kind" "fixture-edge"
                  "row_json" "{}" "undeclared" "not-accepted")))
               :storage-position 1 :storage-id "ledger-1")
              nil)
          (memory-storage-error () t)))))

(let* ((database (merge-pathnames "memory-mutation.sqlite3" (test-state-dir)))
       (backend nil))
  (memory-mutation-delete-db database)
  (unwind-protect
      (progn
        (setf backend (make-sqlite-derived-storage database))
        (let* ((receipt
                 (memory-storage-import-snapshot
                  backend (make-instance 'memory-mutation-source)
                  (memory-mutation-provenance)))
               (seal (gethash "provenance_seal_hash" receipt)))
          (memory-mutation-port-check
           "wrong baseline seal cannot bind authority"
           (handler-case
               (progn
                 (memory-storage-bind-projection
                  backend :baseline-seal "wrong" :storage-id "ledger-1"
                  :agent-id "default" :through-event-id 10
                  :through-position 10 :boundary-hash "hash-10")
                 nil)
             (storage-integrity-error () t)))
          (let ((binding
                  (memory-storage-bind-projection
                   backend :baseline-seal seal :storage-id "ledger-1"
                   :agent-id "default" :through-event-id 10
                   :through-position 10 :boundary-hash "hash-10")))
            (memory-mutation-port-check
             "sealed import binds to one event authority"
             (and (string= "ledger-1" (gethash "storage_id" binding))
                  (= 10 (gethash "through_event_id" binding)))))
          (let* ((event (memory-mutation-event
                         11 11 "memory-node-state"
                         (memory-mutation-payload-node "c" "gamma" "upsert")))
                 (first (memory-storage-apply-mutation backend event))
                 (again nil))
            (storage-close backend)
            (setf backend (make-sqlite-derived-storage database)
                  again (memory-storage-apply-mutation backend event))
            (memory-mutation-port-check
             "node event preserves vectors and retry stays idempotent after reopen"
             (let ((stored (memory-mutation-stored-node backend "c")))
               (and (string= "applied" (gethash "status" first))
                    (string= "already-applied" (gethash "status" again))
                    (= 11
                       (gethash
                        "through_event_id"
                        (memory-storage-bind-projection
                         backend :baseline-seal seal :storage-id "ledger-1"
                         :agent-id "default" :through-event-id 10
                         :through-position 10 :boundary-hash "hash-10")))
                    (search "gamma" (first stored))
                    (string= (string-downcase (memory-mutation-vector 1 0))
                             (%derived-octets-hex (second stored)))
                    (string= (fourth stored)
                             (%derived-row-integrity
                              (first stored) (second stored) (third stored)))))))
          (memory-mutation-port-check
           "bound mutable generation remains readable with row verification"
           (gethash
            "integrity_verified"
            (memory-storage-exact-search
             backend
             (make-memory-exact-query
              :vector-binary-hex (memory-mutation-vector 1 0)
              :profile "all-vectors-v1" :limit 1))))
          (let ((replacement
                  (memory-mutation-event
                   12 12 "memory-node-state"
                   (memory-mutation-payload-node "c" "gamma revised" "update"))))
            (memory-storage-apply-mutation backend replacement)
            (memory-mutation-port-check
             "full replacement advances state and conflicting retry is refused"
             (let ((asserted-retry
                     (memory-mutation-event
                      12 12 "memory-node-state"
                      (memory-mutation-payload-node "c" "asserted" "update"))))
               ;; Recreate the reviewed defect shape: changed exact event JSON
               ;; beside the already-applied event's asserted hash.
               (setf (gethash "event_hash" asserted-retry)
                     (gethash "event_hash" replacement))
             (and (search "gamma revised" (memory-mutation-scalar backend "c"))
                  (handler-case
                      (progn
                        (memory-storage-apply-mutation backend asserted-retry)
                        nil)
                    (memory-storage-error () t))
                  (handler-case
                      (progn
                        (memory-storage-apply-mutation
                         backend
                         (memory-mutation-event
                          12 12 "memory-node-state"
                          (memory-mutation-payload-node "c" "conflict" "update")))
                        nil)
                     (storage-conflict-error () t))))))
          (let* ((report
                   (memory-storage-exact-search
                    backend
                    (make-memory-exact-query
                     :vector-binary-hex (memory-mutation-vector 1 0)
                     :profile "all-vectors-v1" :limit 10 :hydrate-p t)))
                 (candidate
                   (find "c" (gethash "results" report) :test #'string=
                         :key (lambda (item) (gethash "id" item)))))
            (memory-mutation-port-check
             "validated node update advances the warm exact cache in place"
             (and candidate
                  (search "gamma revised"
                          (gethash "content" (gethash "row" candidate)))
                  (= 1 (%sqlite-derived-exact-cache-builds backend))
                  (= 1
                     (%sqlite-derived-exact-cache-incremental-advances
                      backend)))))
          (let ((before (memory-storage-projection-report backend)))
            (let ((tampered
                    (memory-mutation-event
                     13 13 "memory-node-state"
                     (memory-mutation-payload-node "d" "delta" "upsert"))))
              (setf (gethash "event_json" tampered)
                    (gethash "event_json"
                             (memory-mutation-event
                              13 13 "memory-node-state"
                              (memory-mutation-payload-node
                               "d" "substituted" "upsert"))))
              (memory-mutation-port-check
               "projection payload is derived from hash-bound event JSON"
               (and (handler-case
                        (progn (memory-storage-apply-mutation backend tampered) nil)
                      (memory-storage-error () t))
                    (handler-case
                        (progn
                          (make-memory-storage-mutation-from-receipt
                           (%memory-storage-object
                            "schema_version" 1 "storage_id" "ledger-1"
                            "storage_position" 13 "event_id" 13
                            "agent_id" "default"
                            "event_type" "memory-node-state"
                            "event_json" "{}" "integrity_hash" :null))
                          nil)
                      (memory-storage-error () t))
                    (null (memory-mutation-scalar backend "d")))))
            (memory-mutation-port-check
             "wrong-ledger event rolls back without advancing watermark"
             (and (handler-case
                      (progn
                        (memory-storage-apply-mutation
                         backend
                         (memory-mutation-event
                          13 13 "memory-node-state"
                          (memory-mutation-payload-node "d" "delta" "upsert")
                          "other-ledger"))
                        nil)
                    (storage-conflict-error () t))
                  (= (gethash "through_event_id" before)
                     (gethash "through_event_id"
                              (memory-storage-projection-report backend)))
                   (null (memory-mutation-scalar backend "d")))))
          (memory-storage-apply-mutation
           backend
           (memory-mutation-event
            13 13 "memory-node-state"
            (memory-mutation-payload-node "d" "delta" "upsert")))
          (let* ((report
                   (memory-storage-exact-search
                    backend
                    (make-memory-exact-query
                     :vector-binary-hex (memory-mutation-vector 1 0)
                     :profile "all-vectors-v1" :limit 10 :hydrate-p t)))
                 (candidate
                   (find "d" (gethash "results" report) :test #'string=
                         :key (lambda (item) (gethash "id" item)))))
            (memory-mutation-port-check
             "validated node upsert appends to the warm exact cache"
             (and candidate
                  (search "delta"
                          (gethash "content" (gethash "row" candidate)))
                  (= 1 (%sqlite-derived-exact-cache-builds backend))
                  (= 2
                     (%sqlite-derived-exact-cache-incremental-advances
                      backend)))))
          (let* ((row "{\"id\":1,\"from_id\":\"a\",\"to_id\":\"b\",\"edge_type\":\"follows\"}")
                 (insert (%memory-storage-object
                          "operation" "insert" "mutation_kind" "fixture-edge"
                          "row_json" row))
                 (delete (%memory-storage-object
                          "operation" "delete" "mutation_kind" "fixture-edge"
                          "row_json"
                          "{\"edge_type\":\"follows\",\"to_id\":\"b\",\"from_id\":\"a\",\"id\":1}")))
            (memory-storage-apply-mutation
             backend (memory-mutation-event 14 14 "memory-edge-state" insert))
            (memory-storage-apply-mutation
             backend (memory-mutation-event 15 15 "memory-edge-state" delete))
            (memory-mutation-port-check
             "edge changes advance cache identity without rebuilding nodes"
             (and (= 15 (gethash "through_event_id"
                                 (memory-storage-projection-report backend)))
                  (= 1 (%sqlite-derived-exact-cache-builds backend))
                  (= 4
                     (%sqlite-derived-exact-cache-incremental-advances
                      backend)))))
          (let ((invalid (%memory-storage-object
                          "operation" "insert" "mutation_kind" "fixture-edge"
                          "row_json"
                          "{\"id\":2,\"from_id\":\"a\",\"to_id\":\"missing\",\"edge_type\":\"follows\"}")))
            (memory-mutation-port-check
             "foreign-key failure rolls back row and watermark atomically"
             (and (handler-case
                      (progn
                        (memory-storage-apply-mutation
                         backend
                         (memory-mutation-event
                          16 16 "memory-edge-state" invalid))
                        nil)
                    (storage-error () t))
                  (= 15 (gethash "through_event_id"
                                 (memory-storage-projection-report backend))))))
          (let ((other (make-sqlite-derived-storage database)))
            (unwind-protect
                 (%sqlite-exec
                  (%sqlite-derived-handle other :fixture-corruption)
                  "UPDATE pai_memory_nodes SET scalar_json=scalar_json || ' ' WHERE id='c'"
                  :fixture-corruption)
              (storage-close other)))
          (memory-mutation-port-check
           "unexplained external writes invalidate the incremental generation"
           (handler-case
               (progn
                 (memory-storage-exact-search
                  backend
                  (make-memory-exact-query
                   :vector-binary-hex (memory-mutation-vector 1 0)
                   :profile "all-vectors-v1" :limit 1))
                 nil)
             (storage-integrity-error () t)))))
    (when backend (storage-close backend))
    (memory-mutation-delete-db database)))

(let* ((database (merge-pathnames "memory-incremental-fallback.sqlite3"
                                  (test-state-dir)))
       (backend nil))
  (memory-mutation-delete-db database)
  (unwind-protect
       (progn
         (setf backend (make-sqlite-derived-storage database))
         (let* ((receipt
                  (memory-storage-import-snapshot
                   backend (make-instance 'memory-mutation-source)
                   (memory-mutation-provenance)))
                (seal (gethash "provenance_seal_hash" receipt)))
           (memory-storage-bind-projection
            backend :baseline-seal seal :storage-id "ledger-fallback"
            :agent-id "default" :through-event-id 10 :through-position 10
            :boundary-hash "hash-10"))
         (memory-storage-exact-search
          backend
          (make-memory-exact-query
           :vector-binary-hex (memory-mutation-vector 1 0)
           :profile "all-vectors-v1" :limit 2))
         ;; An unexplained generation mismatch must never be advanced from a
         ;; mutation payload that did not start at that exact cache identity.
         (setf (%sqlite-exact-memory-cache-projection-position
                (%sqlite-derived-exact-memory-cache backend))
               9)
         (memory-storage-apply-mutation
          backend
          (memory-mutation-event
           11 11 "memory-node-state"
           (memory-mutation-payload-node "c" "gamma" "upsert")
           "ledger-fallback"))
         (memory-mutation-port-check
          "stale exact cache falls back to a full verified rebuild"
          (and (null (%sqlite-derived-exact-memory-cache backend))
               (= 1 (%sqlite-derived-exact-cache-incremental-fallbacks backend))))
         (memory-storage-exact-search
          backend
          (make-memory-exact-query
           :vector-binary-hex (memory-mutation-vector 1 0)
           :profile "all-vectors-v1" :limit 3))
         (memory-mutation-port-check
          "fallback rebuild restores the authoritative mutation"
          (= 2 (%sqlite-derived-exact-cache-builds backend))))
    (when backend (storage-close backend))
    (memory-mutation-delete-db database)))

(let* ((database (merge-pathnames "memory-coordinator.sqlite3" (test-state-dir)))
       (backend nil) (projection nil) (ledger (make-array 0 :adjustable t :fill-pointer 0))
       (next-id 11) (append-fails-p nil) (coordinator nil))
  (memory-mutation-delete-db database)
  (unwind-protect
      (progn
        (setf backend (make-sqlite-derived-storage database))
        (let* ((receipt
                 (memory-storage-import-snapshot
                  backend (make-instance 'memory-mutation-source)
                  (memory-mutation-provenance)))
               (seal (gethash "provenance_seal_hash" receipt)))
          (memory-storage-bind-projection
           backend :baseline-seal seal :storage-id "ledger-1"
           :agent-id "default" :through-event-id 10 :through-position 10
           :boundary-hash "hash-10")
          (setf projection
                (make-instance 'memory-mutation-failing-projection :inner backend)
                coordinator
                (make-memory-mutation-coordinator
                 :projection projection
                 :append-fn
                 (lambda (type payload)
                   (when append-fails-p
                     (error 'memory-storage-error :operation :fixture-append
                            :detail "injected required append failure"))
                   (let ((mutation
                           (memory-mutation-event next-id next-id type payload)))
                     (incf next-id)
                     (vector-push-extend mutation ledger)
                     mutation))
                 :map-tail-fn
                 (lambda (after visitor)
                   (loop for mutation across ledger
                         when (> (gethash "storage_position" mutation) after)
                           do (funcall visitor mutation))
                   (values t (if (plusp (length ledger))
                                 (gethash "storage_position"
                                          (aref ledger (1- (length ledger))))
                                 after)))))
          (setf append-fails-p t)
          (memory-mutation-port-check
           "required append failure changes no projection"
           (and (handler-case
                    (progn
                      (memory-mutation-coordinator-commit
                       coordinator "memory-node-state"
                       (memory-mutation-payload-node "c" "not durable" "upsert"))
                      nil)
                  (memory-storage-error () t))
                (= 10 (gethash "through_event_id"
                               (memory-storage-projection-report backend)))
                (zerop (length ledger))))
          (setf append-fails-p nil
                (memory-mutation-fail-next-p projection) t)
          (memory-mutation-port-check
           "append success and projection failure leave an authoritative tail"
           (and (handler-case
                    (progn
                      (memory-mutation-coordinator-commit
                       coordinator "memory-node-state"
                       (memory-mutation-payload-node "c" "recover me" "upsert"))
                      nil)
                  (memory-storage-error () t))
                (= 1 (length ledger))
                (= 10 (gethash "through_event_id"
                               (memory-storage-projection-report backend)))
                (null (memory-mutation-scalar backend "c"))))
          (memory-mutation-port-check
           "coordinated read reconciles the durable tail before observing state"
           (and (search
                 "recover me"
                 (memory-mutation-coordinator-read
                  coordinator (lambda () (memory-mutation-scalar backend "c"))))
                (= 11 (gethash "through_event_id"
                               (memory-storage-projection-report backend)))))
          (let ((before (memory-mutation-apply-count projection)))
            (memory-mutation-coordinator-reconcile coordinator)
            (memory-mutation-port-check
             "reconciliation applies an authoritative event exactly once"
             (= before (memory-mutation-apply-count projection))))
          (memory-mutation-port-check
           "incomplete tail enumeration cannot authorize a read"
           (let ((incomplete
                   (make-memory-mutation-coordinator
                    :projection projection :append-fn (lambda (&rest ignored)
                                                        (declare (ignore ignored)) nil)
                    :map-tail-fn (lambda (after visitor)
                                   (declare (ignore visitor))
                                   (values nil after)))))
             (handler-case
                 (progn
                   (memory-mutation-coordinator-read incomplete (lambda () t))
                   nil)
               (memory-storage-error () t))))))
    (when backend (storage-close backend))
    (memory-mutation-delete-db database)))

(let* ((event-database
         (merge-pathnames "memory-receipt-events.sqlite3" (test-state-dir)))
       (derived-database
         (merge-pathnames "memory-receipt-derived.sqlite3" (test-state-dir)))
       (event-backend nil) (derived-backend nil))
  (memory-mutation-delete-db event-database)
  (memory-mutation-delete-db derived-database)
  (unwind-protect
      (progn
        (setf event-backend (make-sqlite-storage event-database)
              derived-backend (make-sqlite-derived-storage derived-database))
        (let* ((import
                 (memory-storage-import-snapshot
                  derived-backend (make-instance 'memory-mutation-source)
                  (memory-mutation-provenance)))
               (bootstrap-receipt
                 (nth-value
                  1 (storage-append-event
                     event-backend "fixture-bootstrap"
                     (%memory-storage-object "status" "content-free")
                     :agent-id "default" :occurred-at "fixture-time")))
               (storage-id (gethash "storage_id" bootstrap-receipt)))
          (memory-storage-bind-projection
           derived-backend
           :baseline-seal (gethash "provenance_seal_hash" import)
           :storage-id storage-id :agent-id "default"
           :through-event-id 0 :through-position 0 :boundary-hash "empty")
          (let ((coordinator
                  (make-storage-memory-mutation-coordinator
                   :event-storage event-backend :projection derived-backend
                   :agent-id "default")))
            (memory-mutation-coordinator-commit
             coordinator "memory-node-state"
             (memory-mutation-payload-node "c" "durable adapter" "upsert"))
            (memory-mutation-port-check
             "SQLite append receipt drives the projection from exact stored bytes"
             (let ((report (memory-storage-projection-report derived-backend)))
               (and (search "durable adapter"
                            (memory-mutation-scalar derived-backend "c"))
                    (= 2 (gethash "through_storage_position" report))
                    (string= storage-id (gethash "storage_id" report)))))
            (storage-append-event
             event-backend "memory-node-state"
             (memory-mutation-payload-node "c" "tail recovered" "update")
             :agent-id "default" :occurred-at "fixture-time")
            (memory-mutation-port-check
             "SQLite relevant-tail receipts recover a directly appended event"
             (search
              "tail recovered"
              (memory-mutation-coordinator-read
               coordinator
               (lambda () (memory-mutation-scalar derived-backend "c")))))
            (storage-append-event
             event-backend "memory-node-state"
             (memory-mutation-payload-node "c" "must not project" "update")
             :agent-id "default" :occurred-at "fixture-time")
            (%sqlite-exec
             (%sqlite-storage-handle event-backend)
             "UPDATE pai_events SET integrity_hash='invalid' WHERE storage_sequence=4"
             :fixture-corrupt-event)
            (memory-mutation-port-check
             "corrupt SQLite tail receipt cannot authorize a coordinated read"
             (and (handler-case
                      (progn
                        (memory-mutation-coordinator-read coordinator (lambda () t))
                        nil)
                    (storage-integrity-error () t))
                  (search "tail recovered"
                          (memory-mutation-scalar derived-backend "c")))))))
    (when event-backend (storage-close event-backend))
    (when derived-backend (storage-close derived-backend))
    (memory-mutation-delete-db event-database)
    (memory-mutation-delete-db derived-database)))

(let* ((database (merge-pathnames "memory-atomic-operation.sqlite3"
                                  (test-state-dir)))
       (backend nil))
  (memory-mutation-delete-db database)
  (unwind-protect
      (progn
        (setf backend (make-sqlite-derived-storage database))
        (let* ((receipt
                 (memory-storage-import-snapshot
                  backend (make-instance 'memory-mutation-source)
                  (memory-mutation-provenance)))
               (seal (gethash "provenance_seal_hash" receipt)))
          (memory-storage-bind-projection
           backend :baseline-seal seal :storage-id "ledger-atomic"
           :agent-id "default" :through-event-id 10
           :through-position 10 :boundary-hash "hash-10")
          (memory-mutation-port-check
           "atomic operation envelope is closed to declared cognitive families"
           (and (fboundp 'make-memory-operation-command)
                (fboundp 'make-memory-operation-payload)
                (handler-case
                    (progn
                      (make-memory-operation-payload "unknown" (vector))
                      nil)
                  (memory-storage-error () t))
                (let ((edge
                        (%memory-storage-object
                         "operation" "insert" "mutation_kind" "admission"
                         "row_json"
                         "{\"id\":9,\"from_id\":\"a\",\"to_id\":\"b\",\"edge_type\":\"derived-from\"}")))
                  (handler-case
                      (progn
                        (make-memory-operation-payload
                         "admission"
                         (vector (make-memory-operation-command
                                  "memory-edge-state" edge)))
                        nil)
                    (memory-storage-error () t)))))
          (memory-mutation-port-check
           "closed cognitive return contracts preserve established shapes"
           (let* ((report
                    (%memory-storage-object
                     "consumer" "fixture" "generation_id" :null
                     "user_visible" t "requested_count" 2
                     "updated_count" 1))
                  (copied (%memory-operation-return-value
                           "use-report" report)))
             (and (eq t (%memory-operation-return-value "true" t))
                  (= 3 (%memory-operation-return-value "count" 3))
                  (equal '("a" "b")
                         (%memory-operation-return-value
                          "node-id-list" '("a" "b")))
                  (not (eq copied report))
                  (= 1 (gethash "updated_count" copied))
                  (handler-case
                      (progn (%memory-operation-return-value "count" -1) nil)
                    (memory-storage-error () t)))))
          (memory-mutation-port-check
           "one invalid command rolls back every row and the operation watermark"
           (and (fboundp 'make-memory-operation-command)
                (fboundp 'make-memory-operation-payload)
                (let* ((node (memory-mutation-payload-node
                              "b" "must roll back" "update"))
                       (edge (%memory-storage-object
                              "operation" "insert"
                              "mutation_kind" "supersession"
                              "row_json"
                              "{\"id\":1,\"from_id\":\"b\",\"to_id\":\"missing\",\"edge_type\":\"supersedes\"}")))
                  (setf (gethash "mutation_kind" node) "supersession")
                  (handler-case
                      (progn
                        (memory-storage-apply-mutation
                         backend
                         (memory-mutation-event
                          11 11 "memory-operation-state"
                          (make-memory-operation-payload
                           "supersession"
                           (vector
                            (make-memory-operation-command
                             "memory-node-state" node)
                            (make-memory-operation-command
                             "memory-edge-state" edge)))
                          "ledger-atomic"))
                        nil)
                    (storage-error ()
                      (and (search "beta" (memory-mutation-scalar backend "b"))
                           (zerop (memory-mutation-edge-count backend))
                           (= 10 (gethash
                                  "through_event_id"
                                  (memory-storage-projection-report backend)))))))))
          (memory-mutation-port-check
           "valid node and edge commands apply under one operation receipt"
           (and (fboundp 'make-memory-operation-command)
                (fboundp 'make-memory-operation-payload)
                (let* ((node (memory-mutation-payload-node
                              "b" "superseding beta" "update"))
                       (edge (%memory-storage-object
                              "operation" "insert"
                              "mutation_kind" "supersession"
                              "row_json"
                              "{\"id\":1,\"from_id\":\"b\",\"to_id\":\"a\",\"edge_type\":\"supersedes\"}")))
                  (setf (gethash "mutation_kind" node) "supersession")
                  (memory-storage-apply-mutation
                   backend
                   (memory-mutation-event
                    11 11 "memory-operation-state"
                    (make-memory-operation-payload
                     "supersession"
                     (vector
                      (make-memory-operation-command "memory-node-state" node)
                      (make-memory-operation-command "memory-edge-state" edge)))
                    "ledger-atomic"))
                  (and (search "superseding beta"
                               (memory-mutation-scalar backend "b"))
                       (= 1 (memory-mutation-edge-count backend))
                       (= 11 (gethash
                              "through_event_id"
                              (memory-storage-projection-report backend)))))))
          (memory-mutation-port-check
           "coordinator returns the established node-id only after atomic commit"
           (and (fboundp 'memory-mutation-coordinator-commit-operation)
                (fboundp 'make-memory-operation-command)
                (let ((next-id 12))
                  (let ((coordinator
                          (make-memory-mutation-coordinator
                           :projection backend
                           :append-fn
                           (lambda (type payload)
                             (memory-mutation-event
                              next-id next-id type payload "ledger-atomic"))
                           :map-tail-fn
                           (lambda (after visitor)
                             (declare (ignore visitor))
                             (values t after)))))
                    (let* ((node (memory-mutation-payload-node
                                  "c" "admitted gamma" "upsert"))
                           (edge (%memory-storage-object
                                  "operation" "insert"
                                  "mutation_kind" "admission-lineage"
                                  "row_json"
                                  "{\"id\":2,\"from_id\":\"c\",\"to_id\":\"a\",\"edge_type\":\"derived-from\"}")))
                      (setf (gethash "mutation_kind" node) "admission")
                      (and (string=
                            "c"
                            (memory-mutation-coordinator-commit-operation
                             coordinator "admission"
                             (vector
                              (make-memory-operation-command
                               "memory-node-state" node)
                              (make-memory-operation-command
                               "memory-edge-state" edge))
                             "node-id" "c"))
                           (search "admitted gamma"
                                   (memory-mutation-scalar backend "c"))
                           (= 2 (memory-mutation-edge-count backend))))))))))
    (when backend (storage-close backend))
    (memory-mutation-delete-db database)))

(format t "~%~d passed, ~d failed~%"
        *memory-mutation-port-pass* *memory-mutation-port-fail*)
(when (plusp *memory-mutation-port-fail*)
  (error "Memory mutation storage port tests failed"))
