;;;; Bounded exact/lexical selection parity and a streamed synthetic store.
(in-package :agent)
(load (merge-pathnames "memory-exact-retrieval-tests.lisp" *load-truename*))

;; Compare independent full sorting with heap selection in adversarial orders.
(dolist (better (list #'%sqlite-memory-exact-better-p
                     #'%sqlite-memory-lexical-better-p))
  (dolist (count '(0 1 7 251 1024))
    (dolist (limit '(1 7 20 250))
      (dolist (order '(:forward :reverse :permuted))
        (let* ((items
                 (loop for i below count
                       for j = (ecase order
                                 (:forward i) (:reverse (- count i 1))
                                 (:permuted (mod (* i 17) (max count 1))))
                       collect
                       (%memory-storage-object
                        "id" (format nil "~8,'0d" j)
                        "distance" (float (mod j 11) 1.0d0)
                        "lexical_tier" (1+ (mod j 2))
                        "lexical_match_count" (1+ (mod j 5)))))
               (expected (sort (copy-list items) better))
               (heap (make-array limit :fill-pointer 0)))
          (dolist (item items)
            (%sqlite-memory-top-k-offer heap item better)
            (assert (<= (length heap) limit))
            (assert (= (array-total-size heap) limit)))
          (mer-check
           (format nil "bounded ranking count=~d k=~d order=~a" count limit order)
           (equalp (coerce (subseq expected 0 (min count limit)) 'vector)
                   (sort heap better))))))))

(defclass mer-scale-source (mer-source) ())
(defparameter *mer-scale-count* 12000)
(defmethod memory-storage-map-snapshot
    ((source mer-scale-source) node-visitor edge-visitor)
  (declare (ignore source edge-visitor))
  (let ((node-digest (ironclad:make-digest :sha256))
        (vector-digest (ironclad:make-digest :sha256))
        (edge-digest (ironclad:make-digest :sha256)))
    ;; Generate one row at a time; the fixture itself must not mask the bound.
    (dotimes (i *mer-scale-count*)
      (let* ((node (mer-node (format nil "~8,'0d" i) 1 0 :turn "scale"
                             :content (concatenate 'string "needle "
                                                   (make-string 4096 :initial-element #\x))))
             (scalar (first node))
             (vector (string-downcase (second node))))
        (%derived-digest-update node-digest scalar)
        (%derived-digest-update vector-digest vector)
        (%derived-digest-update vector-digest vector)
        (funcall node-visitor
                 (%memory-storage-object
                  "scalar_json" scalar "embedding_binary_hex" vector
                  "retrieval_embedding_binary_hex" vector))))
    (%memory-storage-object
     "schema_version" 1 "backend" "synthetic-streamed-scale"
     "node_count" *mer-scale-count* "edge_count" 0
     "node_sha256" (%derived-digest-hex node-digest)
     "vector_sha256" (%derived-digest-hex vector-digest)
     "vector_binary_encoding" "pgvector-send-v1"
     "edge_sha256" (%derived-digest-hex edge-digest))))

(let* ((database (merge-pathnames "bounded-scale.sqlite3" (test-state-dir)))
       (backend nil))
  (mer-delete-db database)
  (unwind-protect
       (progn
         (setf backend (make-sqlite-derived-storage database))
         (memory-storage-import-snapshot backend (make-instance 'mer-scale-source)
                                         (mer-provenance))
         (sb-ext:gc :full t)
         (let ((before (sb-kernel:dynamic-usage)))
           (dotimes (iteration 3)
             (dolist (lexical-p '(nil t))
               (let* ((query
                        (make-memory-exact-query
                         :vector-binary-hex (mer-vector 1 0)
                         :profile "safe-semantic-v1" :limit 7 :hydrate-p t
                         :include-vector-p t
                         :lexemes (when lexical-p
                                    (list (%memory-storage-object
                                           "text" "needle" "kind" "token")))))
                      (report (if lexical-p
                                  (memory-storage-lexical-search backend query)
                                  (memory-storage-exact-search backend query)))
                      (results (gethash "results" report)))
                 (mer-check "scale scan keeps exact ties, winner content and vectors"
                            (and (= 7 (length results))
                                 (equal (loop for i below 7 collect (format nil "~8,'0d" i))
                                        (loop for row across results collect (gethash "id" row)))
                                 (every (lambda (item)
                                          (and (= 4103 (length (gethash "content" (gethash "row" item))))
                                               (equalp #(1.0 0.0) (gethash "vector" item))))
                                        results))))))
           (sb-ext:gc :full t)
           (mer-check "repeated scale searches retain less than 8 MiB"
                      (< (- (sb-kernel:dynamic-usage) before) (* 8 1024 1024))))
         (mer-check "backend has no resident collection slot"
                    (not (slot-exists-p backend 'exact-memory-cache)))
         (let* ((query (make-memory-exact-query
                        :vector-binary-hex (mer-vector 1 0)
                        :profile "all-vectors-v1" :limit 1
                        :excluded-ids '("00000000") :include-vector-p t))
                (item (aref (gethash "results" (memory-storage-exact-search backend query)) 0)))
           (mer-check "limit one honors exclusions and vector-only hydration"
                      (and (string= "00000001" (gethash "id" item))
                           (null (gethash "row" item))
                           (equalp #(1.0 0.0) (gethash "vector" item)))))
         (let ((query (make-memory-exact-query
                       :vector-binary-hex (mer-vector 1 0)
                       :profile "turn-neighborhood-v1" :turn-ids '("missing")
                       :limit 7)))
           (mer-check "empty eligible set yields an empty vector"
                      (zerop (length (gethash "results"
                                             (memory-storage-exact-search backend query))))))
         ;; Bind a mutable projection so verification occurs in the streaming
         ;; scan, not solely through the immutable-import audit. Corrupt the
         ;; final (non-winning) row and require both scans to reject it.
         (memory-storage-bind-projection
          backend :baseline-seal (%sqlite-derived-current-memory-seal
                                   (%sqlite-derived-handle backend :test) :test)
          :storage-id "synthetic-stream-ledger" :agent-id "default"
          :through-event-id 1 :through-position 1 :boundary-hash "fixture")
         (let ((other (make-sqlite-derived-storage database)))
           (unwind-protect
                (%sqlite-exec (%sqlite-derived-handle other :test)
                              "UPDATE pai_memory_nodes SET scalar_json=scalar_json || ' ' WHERE source_ordinal=(SELECT max(source_ordinal) FROM pai_memory_nodes)"
                              :test)
             (storage-close other)))
         (dolist (lexical-p '(nil t))
           (mer-check "corrupt non-winning row is not hidden by top-k limit"
                      (handler-case
                          (let ((query (make-memory-exact-query
                                        :vector-binary-hex (mer-vector 1 0)
                                        :profile "all-vectors-v1" :limit 1
                                        :lexemes (list (%memory-storage-object
                                                        "kind" "token" "text" "needle")))))
                            (if lexical-p
                                (memory-storage-lexical-search backend query)
                                (memory-storage-exact-search backend query))
                            nil)
                        (storage-integrity-error () t)))))
    (when backend (storage-close backend))
    (mer-delete-db database)))
(format t "~%~d passed, ~d failed~%" *mer-pass* *mer-fail*)
(when (plusp *mer-fail*) (error "Bounded retrieval tests failed"))


