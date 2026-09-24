(in-package :agent)

(ql:quickload '(:cffi :shasht :ironclad :bordeaux-threads) :silent t)

(defvar *sdm-pass* 0)
(defvar *sdm-fail* 0)

(defun sdm-check (name condition)
  (if condition
      (progn (incf *sdm-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *sdm-fail*) (format t "  FAIL ~a~%" name))))

(defun sdm-signals-p (type thunk)
  (handler-case (progn (funcall thunk) nil)
    (condition (actual) (typep actual type))))

(defun sdm-delete-db (path)
  (dolist (candidate (list path
                           (pathname (concatenate 'string (namestring path)
                                                  "-wal"))
                           (pathname (concatenate 'string (namestring path)
                                                  "-shm"))))
    (when (probe-file candidate) (delete-file candidate))))

(dolist (file '("storage-substrate.lisp" "memory-storage.lisp"
                "sqlite-storage.lisp" "sqlite-derived-storage.lisp"))
  (load (test-source file)))

(defclass sdm-source (memory-storage-backend)
  ((bad-edge-p :initarg :bad-edge-p :initform nil :reader sdm-bad-edge-p)))

(defmethod memory-storage-capabilities ((source sdm-source))
  (declare (ignore source))
  (%memory-storage-object
   "schema_version" 1 "backend" "fixture" "authority_role" "source"
   "read_snapshot" t "exact_vector_export" t
   "runtime_reads" nil "runtime_writes" nil))

(defun sdm-digest-update (digest text)
  (let* ((octets (sb-ext:string-to-octets text :external-format :utf-8))
         (prefix (sb-ext:string-to-octets
                  (format nil "~d:" (length octets))
                  :external-format :utf-8)))
    (ironclad:update-digest digest prefix)
    (ironclad:update-digest digest octets)))

(defun sdm-digest-hex (digest)
  (string-downcase
   (ironclad:byte-array-to-hex-string (ironclad:produce-digest digest))))

(defmethod memory-storage-map-snapshot
    ((source sdm-source) node-visitor edge-visitor)
  (let* ((nodes
           (list
            (list
             "{\"id\":\"n1\",\"kind\":\"observation\",\"content\":\"private fixture\"}"
             "000200003f80000040000000" "000200004040000040800000")
            (list
             "{\"id\":\"n2\",\"kind\":\"reflection\",\"content\":\"private derived fixture\"}"
             "0002000040a0000040c00000" "0002000040e0000041000000")))
         (edge
           (format nil
                   "{\"id\":1,\"from_id\":\"n1\",\"to_id\":\"~a\",\"edge_type\":\"supports\",\"created_at\":\"2026-08-19T00:00:00Z\"}"
                   (if (sdm-bad-edge-p source) "missing" "n2")))
         (node-digest (ironclad:make-digest :sha256))
         (vector-digest (ironclad:make-digest :sha256))
         (edge-digest (ironclad:make-digest :sha256)))
    (dolist (node nodes)
      (sdm-digest-update node-digest (first node))
      (sdm-digest-update vector-digest (second node))
      (sdm-digest-update vector-digest (third node))
      (funcall node-visitor
               (%memory-storage-object
                "scalar_json" (first node)
                "embedding_binary_hex" (second node)
                "retrieval_embedding_binary_hex" (third node))))
    (sdm-digest-update edge-digest edge)
    (funcall edge-visitor edge)
    (%memory-storage-object
     "schema_version" 1 "backend" "fixture"
     "node_count" 2 "edge_count" 1
     "node_sha256" (sdm-digest-hex node-digest)
     "vector_sha256" (sdm-digest-hex vector-digest)
     "vector_binary_encoding" "pgvector-send-v1"
     "edge_sha256" (sdm-digest-hex edge-digest))))

(defun sdm-provenance ()
  (make-memory-embedding-provenance
   :embedding-model "fixture-embedder"
   :embedding-revision "fixture-revision"
   :retrieval-embedding-model "fixture-embedder"
   :retrieval-embedding-revision "fixture-revision"
   :vector-dimension 2 :revision-evidence "deterministic-fixture"
   :approval-scope "qualification-only"))

(format t "~%== separate derived SQLite checkpoint and memory store ==~%")

(let* ((root (test-state-dir))
       (database (merge-pathnames "derived-memory.sqlite3" root))
       (failure-database (merge-pathnames "derived-memory-failure.sqlite3" root))
       (backend nil) (failure-backend nil))
  (sdm-delete-db database)
  (sdm-delete-db failure-database)
  (unwind-protect
      (progn
        (setf backend (make-sqlite-derived-storage database))
        (let ((handle (%sqlite-derived-handle backend :fixture-schema-version))
              (version nil)
              (graph-table-count 0))
          (%with-sqlite-statement
              (statement handle
                         "SELECT meta_value FROM pai_derived_meta WHERE meta_key='format_version'"
                         :fixture-schema-version)
            (%sqlite-step handle statement :fixture-schema-version +sqlite-row+)
            (setf version (%sqlite-column-text statement 0)))
          (%with-sqlite-statement
              (statement handle
                         "SELECT count(*) FROM sqlite_master WHERE type='table' AND name IN ('pai_knowledge_graph_nodes','pai_knowledge_graph_edges','pai_knowledge_graph_evidence')"
                         :fixture-schema-version)
            (%sqlite-step handle statement :fixture-schema-version +sqlite-row+)
            (setf graph-table-count (%sqlite-column-int64 statement 0)))
          (sdm-check "fresh derived store declares reviewed-graph format 4"
                     (and (string= "4" version)
                          (= 3 graph-table-count))))
        (let ((capabilities (memory-storage-capabilities backend)))
          (sdm-check
           "capabilities declare migration destination without runtime authority"
           (and (eq t (gethash "atomic_import" capabilities))
                (null (gethash "runtime_reads" capabilities))
                (null (gethash "runtime_writes" capabilities)))))
        (let ((handle (%sqlite-derived-handle backend :fixture-schema))
              (event-table-count -1))
          (%with-sqlite-statement
              (statement handle
                         "SELECT count(*) FROM sqlite_master WHERE type='table' AND name='pai_events'"
                         :fixture-schema)
            (%sqlite-step handle statement :fixture-schema +sqlite-row+)
            (setf event-table-count (%sqlite-column-int64 statement 0)))
          (sdm-check "derived database contains no event table"
                     (zerop event-table-count)))
        (let* ((state (%storage-object "schema_version" 1 "events" (vector)))
               (published
                 (storage-publish-checkpoint
                  backend "fixture-projection" state :agent-id "fixture"
                  :through-event-id 7 :through-position 9
                  :projector-revision "fixture-projector"
                  :policy-revision "fixture-policy"))
               (loaded
                 (storage-load-checkpoint
                  backend "fixture-projection" :agent-id "fixture")))
          (sdm-check "checkpoint is independently integrity-bound in derived storage"
                     (and (= 7 (gethash "through_event_id" loaded))
                          (= 9 (gethash "through_storage_position" loaded))
                          (string= (gethash "integrity_hash" published)
                                   (gethash "integrity_hash" loaded)))))
        ;; Reopen a logically version-1 copy and prove that the additive
        ;; migration preserves incumbent checkpoint authority while restoring
        ;; the three graph tables.  A separate database keeps this fixture from
        ;; weakening the import/audit assertions below.
        (let* ((migration-database
                 (merge-pathnames "derived-memory-v1-migration.sqlite3" root))
               (migration-backend nil))
          (sdm-delete-db migration-database)
          (unwind-protect
              (progn
                (setf migration-backend
                      (make-sqlite-derived-storage migration-database))
                (storage-publish-checkpoint
                 migration-backend "incumbent" (%storage-object "kept" t)
                 :agent-id "fixture" :through-event-id 4 :through-position 5)
                (let ((handle (%sqlite-derived-handle
                               migration-backend :fixture-v1-shape)))
                  (%sqlite-exec handle "DROP TABLE pai_knowledge_graph_evidence"
                                :fixture-v1-shape)
                  (%sqlite-exec handle "DROP TABLE pai_knowledge_graph_edges"
                                :fixture-v1-shape)
                  (%sqlite-exec handle "DROP TABLE pai_knowledge_graph_nodes"
                                :fixture-v1-shape)
                  (%sqlite-exec
                   handle
                   "UPDATE pai_derived_meta SET meta_value='1' WHERE meta_key='format_version'"
                   :fixture-v1-shape))
                (storage-close migration-backend)
                (setf migration-backend
                      (make-sqlite-derived-storage migration-database))
                (let ((kept (storage-load-checkpoint
                             migration-backend "incumbent"
                             :agent-id "fixture"))
                      (handle (%sqlite-derived-handle
                               migration-backend :fixture-v1-result))
                      (version nil)
                      (table-count 0))
                  (%with-sqlite-statement
                      (statement handle
                                 "SELECT meta_value FROM pai_derived_meta WHERE meta_key='format_version'"
                                 :fixture-v1-result)
                    (%sqlite-step handle statement :fixture-v1-result +sqlite-row+)
                    (setf version (%sqlite-column-text statement 0)))
                  (%with-sqlite-statement
                      (statement handle
                                 "SELECT count(*) FROM sqlite_master WHERE type='table' AND name LIKE 'pai_knowledge_graph_%'"
                                 :fixture-v1-result)
                    (%sqlite-step handle statement :fixture-v1-result +sqlite-row+)
                    (setf table-count (%sqlite-column-int64 statement 0)))
                  (sdm-check "version-1 migration is additive and preserves checkpoint state"
                             (and (string= "4" version)
                                  (= 3 table-count)
                                  (= 4 (gethash "through_event_id" kept))
                                  (eq t (gethash "kept"
                                                (gethash "state" kept)))))))
            (when migration-backend
              (ignore-errors (storage-close migration-backend)))
            (sdm-delete-db migration-database)))
        (sdm-check
         "import refuses absent provenance before accepting private rows"
         (sdm-signals-p
          'memory-storage-error
          (lambda ()
            (memory-storage-import-snapshot
             backend (make-instance 'sdm-source) nil))))
        (let* ((report
                 (memory-storage-import-snapshot
                  backend (make-instance 'sdm-source) (sdm-provenance)))
               (audit (memory-storage-audit-snapshot backend)))
          (sdm-check "atomic import returns only content-free parity evidence"
                     (and (string= "imported" (gethash "status" report))
                          (= 2 (gethash "node_count" report))
                          (= 1 (gethash "edge_count" report))
                          (= 64 (length
                                 (gethash "provenance_seal_hash" report)))
                          (not (search "private" (%storage-json report)))))
          (sdm-check "independent destination audit matches every source digest"
                     (every (lambda (key)
                              (equal (gethash key report)
                                     (gethash key audit)))
                            '("node_count" "edge_count" "node_sha256"
                              "vector_sha256" "edge_sha256"
                              "vector_binary_encoding"))))
        (let ((characterization (memory-storage-characterize backend)))
          (sdm-check "one sealed import makes migration state explicit"
                     (and (= 1 (gethash "sealed_import_count"
                                        characterization))
                          (eq t (gethash "migration_ready"
                                        characterization)))))
        (sdm-check
         "non-empty destination rejects an accidental second import"
         (sdm-signals-p
          'storage-conflict-error
          (lambda ()
            (memory-storage-import-snapshot
             backend (make-instance 'sdm-source) (sdm-provenance)))))
        (%sqlite-exec
         (%sqlite-derived-handle backend :fixture-corruption)
         "UPDATE pai_memory_imports SET seal_hash='corrupt' WHERE import_name='canonical'"
         :fixture-corruption)
        (sdm-check "provenance-seal corruption fails independent audit closed"
                   (sdm-signals-p
                    'storage-integrity-error
                    (lambda () (memory-storage-audit-snapshot backend))))
        (setf failure-backend
              (make-sqlite-derived-storage failure-database))
        (sdm-check
         "orphan edge aborts the entire destination transaction"
         (and
          (sdm-signals-p
           'storage-error
           (lambda ()
             (memory-storage-import-snapshot
              failure-backend
              (make-instance 'sdm-source :bad-edge-p t)
              (sdm-provenance))))
          (let ((audit (memory-storage-audit-snapshot failure-backend)))
            (and (zerop (gethash "node_count" audit))
                 (zerop (gethash "edge_count" audit)))))))
    (when backend (ignore-errors (storage-close backend)))
    (when failure-backend (ignore-errors (storage-close failure-backend)))
    (sdm-delete-db database)
    (sdm-delete-db failure-database)))

(format t "~%~d passed, ~d failed~%" *sdm-pass* *sdm-fail*)
(when (plusp *sdm-fail*)
  (error "SQLite derived memory tests failed"))
