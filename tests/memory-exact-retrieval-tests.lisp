(in-package :agent)

(ql:quickload '(:cffi :shasht :ironclad :bordeaux-threads) :silent t)

(defvar *mer-pass* 0)
(defvar *mer-fail* 0)

(defun mer-check (name condition)
  (if condition
      (progn (incf *mer-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *mer-fail*) (format t "  FAIL ~a~%" name))))

(defun mer-delete-db (path)
  (dolist (candidate (list path
                           (pathname (concatenate 'string (namestring path)
                                                  "-wal"))
                           (pathname (concatenate 'string (namestring path)
                                                  "-shm"))))
    (when (probe-file candidate) (delete-file candidate))))

(dolist (file '("storage-substrate.lisp" "memory-storage.lisp"
                "sqlite-storage.lisp" "sqlite-derived-storage.lisp"))
  (load (test-source file)))

(defclass mer-source (memory-storage-backend) ())

(defmethod memory-storage-capabilities ((source mer-source))
  (declare (ignore source))
  (%memory-storage-object
   "schema_version" 1 "backend" "exact-retrieval-fixture"
   "read_snapshot" t "exact_vector_export" t))

(defun mer-vector (x y)
  (format nil "00020000~8,'0x~8,'0x"
          (ecase x ((-1) #xbf800000) ((0) 0) ((1) #x3f800000))
          (ecase y ((-1) #xbf800000) ((0) 0) ((1) #x3f800000))))

(defun mer-node (id x y &key cold turn (content "fixture"))
  (list
   (format nil
           "{\"id\":\"~a\",\"kind\":\"observation\",\"content\":\"~a\",\"is_cold\":~a,\"quarantined\":false,\"origin_class\":\"lived-user\",\"epistemic_status\":\"asserted\",\"grounding_status\":\"grounded\",\"source_event_id\":\"event-~a\",\"epistemic_metadata\":{\"turn_id\":\"~a\"}}"
           id content (if cold "true" "false") id turn)
   (mer-vector x y)))

(defmethod memory-storage-map-snapshot
    ((source mer-source) node-visitor edge-visitor)
  (declare (ignore source edge-visitor))
  (let ((nodes (list (mer-node "a" 1 0 :turn "t1"
                               :content "Alpha bedtime routine")
                     (mer-node "b" 0 1 :turn "t2"
                               :content "The BEDTIME   routine works")
                     (mer-node "c" 1 0 :cold t :turn "t1"
                               :content "bedtime routine")
                     (mer-node "d" -1 0 :turn "t3"
                               :content "bedtimer")))
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
    ;; This fixture exercises retrieval only. Import supports an empty edge set.
    (%memory-storage-object
     "schema_version" 1 "backend" "exact-retrieval-fixture"
     "node_count" 4 "edge_count" 0
     "node_sha256" (%derived-digest-hex node-digest)
     "vector_sha256" (%derived-digest-hex vector-digest)
     "vector_binary_encoding" "pgvector-send-v1"
     "edge_sha256" (%derived-digest-hex edge-digest))))

(defun mer-provenance ()
  (make-memory-embedding-provenance
   :embedding-model "fixture" :embedding-revision "fixture-v1"
   :retrieval-embedding-model "fixture"
   :retrieval-embedding-revision "fixture-v1"
   :vector-dimension 2 :revision-evidence "deterministic-fixture"
   :approval-scope "qualification-only"))

(format t "~%== backend-neutral exact memory retrieval ==~%")

(let* ((root (test-state-dir))
       (database (merge-pathnames "exact-retrieval.sqlite3" root))
       (backend nil))
  (mer-delete-db database)
  (unwind-protect
      (progn
        (setf backend (make-sqlite-derived-storage database))
        (mer-check "unsealed derived data is not searchable"
                   (handler-case
                       (progn
                         (memory-storage-exact-search
                          backend
                          (make-memory-exact-query
                           :vector-binary-hex (mer-vector 1 0)
                           :profile "all-vectors-v1" :limit 1))
                         nil)
                     (storage-integrity-error () t)))
        (memory-storage-import-snapshot
         backend (make-instance 'mer-source) (mer-provenance))
        (mer-check "SQLite advertises qualification-only exact retrieval"
                   (let ((caps (memory-storage-capabilities backend)))
                     (and (eq t (gethash "exact_retrieval" caps))
                          (null (gethash "runtime_reads" caps)))))
        (let* ((query (make-memory-exact-query
                       :vector-binary-hex (mer-vector 1 0)
                       :profile "all-vectors-v1" :limit 4))
               (report (memory-storage-exact-search backend query))
               (results (gethash "results" report)))
          (mer-check "first exact read builds one generation-bound cache"
                     (and (= 1 (%sqlite-derived-exact-cache-builds backend))
                          (= 0 (%sqlite-derived-exact-cache-hits backend))))
          (mer-check "pgvector binary floats decode without decimal drift"
                     (equalp #(1.0 0.0)
                             (memory-exact-query-vector query)))
          (mer-check "cosine ordering is exact and deterministic on ties"
                     (equal '("a" "c" "b" "d")
                            (loop for row across results
                                  collect (gethash "id" row))))
          (mer-check "cosine distances match the portable reference"
                     (equal '(0.0d0 0.0d0 1.0d0 2.0d0)
                            (loop for row across results
                                  collect (gethash "distance" row)))))
        (let* ((safe (memory-storage-exact-search
                      backend
                      (make-memory-exact-query
                       :vector-binary-hex (mer-vector 1 0)
                       :profile "safe-semantic-v1" :limit 10)))
               (turn (memory-storage-exact-search
                      backend
                      (make-memory-exact-query
                       :vector-binary-hex (mer-vector 1 0)
                       :profile "turn-neighborhood-v1" :turn-ids '("t2")
                       :limit 10))))
          (mer-check "safe semantic eligibility excludes cold candidates"
                     (equal '("a" "b" "d")
                            (loop for row across (gethash "results" safe)
                                  collect (gethash "id" row))))
          (mer-check "turn-neighborhood eligibility is represented explicitly"
                     (equal '("b")
                            (loop for row across (gethash "results" turn)
                                  collect (gethash "id" row))))
          (mer-check "unchanged exact reads reuse the verified generation"
                     (and (= 1 (%sqlite-derived-exact-cache-builds backend))
                          (= 2 (%sqlite-derived-exact-cache-hits backend)))))
        (let* ((with-vector
                 (memory-storage-exact-search
                  backend
                  (make-memory-exact-query
                   :vector-binary-hex (mer-vector 1 0)
                   :profile "all-vectors-v1" :limit 1
                   :include-vector-p t :hydrate-p t)))
               (returned (gethash "vector"
                                  (aref (gethash "results" with-vector) 0)))
               (returned-row (gethash "row"
                                      (aref (gethash "results" with-vector) 0))))
          (setf (aref returned 0) -1.0)
          (setf (gethash "content" returned-row) "caller mutation")
          (let* ((again
                   (memory-storage-exact-search
                    backend
                    (make-memory-exact-query
                     :vector-binary-hex (mer-vector 1 0)
                     :profile "all-vectors-v1" :limit 1
                     :include-vector-p t :hydrate-p t)))
                 (fresh (gethash "vector"
                                 (aref (gethash "results" again) 0)))
                 (fresh-row (gethash "row"
                                     (aref (gethash "results" again) 0))))
            (mer-check "returned rows and vectors cannot mutate cached state"
                       (and (= 1.0 (aref fresh 0))
                            (not (string= "caller mutation"
                                          (gethash "content" fresh-row)))))))
        (let* ((phrase (%memory-storage-object
                        "text" "bedtime routine" "kind" "phrase"))
               (token (%memory-storage-object
                       "text" "alpha" "kind" "token"))
               (lexical
                 (memory-storage-lexical-search
                  backend
                  (make-memory-exact-query
                   :vector-binary-hex (mer-vector 1 0)
                   :profile "safe-semantic-v1" :limit 10 :hydrate-p t
                   :lexemes (list phrase token))))
               (results (gethash "results" lexical)))
          (mer-check "literal phrase/token matching preserves tiers and bounds"
                     (and (equal '("a" "b")
                                 (loop for row across results
                                       collect (gethash "id" row)))
                          (= 2 (gethash "lexical_tier" (aref results 0)))
                          (= 2 (gethash "lexical_match_count"
                                        (aref results 0)))
                          (= 1 (gethash "lexical_match_count"
                                        (aref results 1))))))
        (mer-check "unknown profiles and zero query vectors fail closed"
                   (and
                    (handler-case
                        (progn
                          (make-memory-exact-query
                           :vector-binary-hex (mer-vector 1 0)
                           :profile "invented" :limit 1)
                          nil)
                      (memory-storage-error () t))
                    (handler-case
                        (progn
                          (make-memory-exact-query
                           :vector-binary-hex (mer-vector 0 0)
                           :profile "all-vectors-v1" :limit 1)
                          nil)
                      (memory-storage-error () t))))
        ;; Prove that the generation-bound audit receipt is not a permanent
        ;; trust bit. A commit through another SQLite connection changes
        ;; PRAGMA data_version; the next read must re-audit the bytes and
        ;; reject this deliberately corrupted disposable fixture.
        (let ((other (make-sqlite-derived-storage database)))
          (unwind-protect
              (%sqlite-exec
               (%sqlite-derived-handle other :external-corruption)
               "UPDATE pai_memory_nodes SET scalar_json=scalar_json || ' ' WHERE id='a'"
               :external-corruption)
            (storage-close other)))
        (mer-check "external commits invalidate the verified-generation receipt"
                   (handler-case
                       (progn
                         (memory-storage-exact-search
                          backend
                          (make-memory-exact-query
                           :vector-binary-hex (mer-vector 1 0)
                           :profile "all-vectors-v1" :limit 1))
                         nil)
                     (storage-integrity-error () t))))
    (when backend (ignore-errors (storage-close backend)))
    (mer-delete-db database)))

(format t "~%~d passed, ~d failed~%" *mer-pass* *mer-fail*)
(when (plusp *mer-fail*)
  (error "Exact memory retrieval tests failed"))
