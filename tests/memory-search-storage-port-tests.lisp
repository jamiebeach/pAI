(in-package :agent)

(ql:quickload '(:postmodern :bordeaux-threads :shasht) :silent t)

(defvar *mssp-pass* 0)
(defvar *mssp-fail* 0)
(defvar *mssp-query* nil)

(defun mssp-check (name condition)
  (if condition
      (progn (incf *mssp-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *mssp-fail*) (format t "  FAIL ~a~%" name))))

(dolist (file '("memory-storage.lisp" "stabilization-config.lisp"
                "memory-nodes.lisp" "typed-retrieval.lisp"))
  (load (test-source file)))

(defclass mssp-backend (memory-storage-backend) ())

(defun mssp-row (id distance importance activation created kind)
  (%memory-storage-object
   "id" id "distance" distance
   "row"
   (%memory-storage-object
    "id" id "kind" kind "content" (format nil "fixture ~a" id)
    "created_at" created "observed_at" :null "valid_from" :null
    "valid_to" :null "last_accessed" created "access_count" 0
    "importance" importance "valence" 0.0d0
    "arousal_at_encoding" 0.0d0 "activation" activation
    "source_event_id" (format nil "event-~a" id)
    "is_cold" nil "origin_class" "lived-user"
    "epistemic_status" "user-report" "producer" "fixture"
    "model_purpose" :null "confidence" 1.0d0
    "grounding_status" "grounded" "root_observation_ids" (vector)
    "generation_id" :null "supersedes_node_id" :null
    "quarantined" nil
    "epistemic_metadata" (%memory-storage-object
                           "turn_id" (format nil "turn-~a" id)
                           "role" "user" "sequence" 0))))

(defmethod memory-storage-exact-search
    ((backend mssp-backend) (query memory-exact-query))
  (declare (ignore backend))
  (setf *mssp-query* query)
  (%memory-storage-object
   "schema_version" 1 "backend" "fixture" "exact_scan_forced" t
   "results"
   (if (string= "turn-neighborhood-v1"
                (memory-exact-query-profile query))
       (let ((candidate
               (mssp-row "b" 0.2d0 1.0d0 0.9d0
                         "2026-08-18 12:00:00+00" "reflection")))
         (setf (gethash "vector" candidate) #(0.0 1.0))
         (vector candidate))
       (vector
        ;; Similarity alone prefers A; the shared reranker must prefer B.
        (mssp-row "a" 0.0d0 0.1d0 0.1d0 "2026-08-18 12:00:00+00"
                  "observation")
        (mssp-row "b" 0.2d0 1.0d0 0.9d0 "2026-08-18 12:00:00+00"
                  "reflection")))))

(defmethod memory-storage-lexical-search
    ((backend mssp-backend) (query memory-exact-query))
  (declare (ignore backend))
  (setf *mssp-query* query)
  (let ((candidate
          (mssp-row "lex" 0.4d0 0.7d0 0.6d0
                    "2026-08-18 12:00:00+00" "observation")))
    (setf (gethash "lexical_tier" candidate) 1
          (gethash "lexical_match_count" candidate) 1
          (gethash "lexical_coverage" candidate) 0.5d0
          (gethash "lexical_terms" candidate) #( "fixture" ))
    (%memory-storage-object
     "schema_version" 1 "backend" "fixture" "results" (vector candidate))))

(format t "~%== actual memory-search through storage port ==~%")

(let ((*memory-search-storage-backend* (make-instance 'mssp-backend))
      (*memory-search-query-vector-fn*
        (lambda (query typed-p)
          (declare (ignore query typed-p))
          #(1.0 0.0)))
      (*memory-recall-clock-fn*
        (lambda () (encode-universal-time 0 0 12 19 8 2026 0)))
      (*retrieval-embedding-mode* :enforced))
  (let ((rows
          (memory-search "fixture query" :k 2 :mode :conversation
                         :kinds '("observation" "reflection")
                         :origins '("lived-user")
                         :statuses '("user-report")
                         :exclude-ids '("excluded")
                         :exclude-turn-ids '("turn-current")
                         :exclude-source-event-ids '("event-current")
                         :as-of "2026-08-19T12:00:00Z")))
    (mssp-check "actual consumer selects the declared backend port"
                (typep *mssp-query* 'memory-exact-query))
    (mssp-check "all pre-limit filters cross the query boundary"
                (and (equal '("observation" "reflection")
                            (memory-exact-query-kinds *mssp-query*))
                     (equal '("lived-user")
                            (memory-exact-query-origins *mssp-query*))
                     (equal '("user-report")
                            (memory-exact-query-statuses *mssp-query*))
                     (equal '("excluded")
                            (memory-exact-query-excluded-ids *mssp-query*))
                     (equal '("turn-current")
                            (memory-exact-query-excluded-turn-ids
                             *mssp-query*))
                     (equal '("event-current")
                            (memory-exact-query-excluded-source-event-ids
                             *mssp-query*))
                     (string= "2026-08-19T12:00:00Z"
                              (memory-exact-query-as-of *mssp-query*))))
    (mssp-check "hydrated candidates use the incumbent composite reranker"
                (equal '("b" "a")
                       (mapcar (lambda (row) (gethash "id" row)) rows)))
    (mssp-check "hydrated output retains private row and score components"
                (every (lambda (row)
                         (and (stringp (gethash "content" row))
                              (equal (gethash "created_at" row)
                                     (gethash "observed_at" row))
                              (numberp (gethash "similarity" row))
                              (numberp (gethash "recency_score" row))
                              (numberp (gethash "retrieval_score" row))))
                       rows))))

(let* ((*memory-search-storage-backend* (make-instance 'mssp-backend))
       (*memory-search-query-vector-fn*
         (lambda (query typed-p)
           (declare (ignore query typed-p))
           #(1.0 0.0)))
       (*memory-recall-clock-fn*
         (lambda () (encode-universal-time 0 0 12 19 8 2026 0)))
       (*retrieval-embedding-mode* :enforced)
       (anchor (gethash "row" (mssp-row
                                "b" 0.2d0 1.0d0 0.9d0
                                "2026-08-18 12:00:00+00" "reflection")))
       (rows nil))
  (setf (gethash "candidate_sources" anchor) #( "semantic" )
        rows (memory-search-turn-neighborhood "fixture query" (list anchor)))
  (mssp-check "turn-neighborhood crosses the same storage boundary"
              (and (string= "turn-neighborhood-v1"
                            (memory-exact-query-profile *mssp-query*))
                   (equal '("turn-b")
                          (memory-exact-query-turn-ids *mssp-query*))))
  (mssp-check "turn-neighborhood retains the document vector for bundling"
              (equalp '(0.0 1.0)
                      (gethash "retrieval_embedding" (first rows))))
  (mssp-check "turn-neighborhood preserves anchor candidate provenance"
              (equalp #( "semantic" )
                      (gethash "candidate_sources" (first rows)))))

(let ((*memory-search-storage-backend* (make-instance 'mssp-backend))
      (*memory-search-query-vector-fn*
        (lambda (query typed-p)
          (declare (ignore query typed-p))
          #(1.0 0.0)))
      (*memory-recall-clock-fn*
        (lambda () (encode-universal-time 0 0 12 19 8 2026 0)))
      (*retrieval-embedding-mode* :enforced))
  (multiple-value-bind (rows report)
      (handler-case
          (memory-search "remember fixture" :k 3 :mode :conversation
                         :candidate-strategy :hybrid-explicit)
        (error () (values nil nil)))
    (mssp-check "actual hybrid consumer unions lexical storage candidates"
                (and rows report
                     (string= "hybrid-explicit" (gethash "strategy" report))
                     (plusp (gethash "lexical_candidate_count" report 0))
                     (memory-exact-query-lexemes *mssp-query*)))))

(format t "~%~d passed, ~d failed~%" *mssp-pass* *mssp-fail*)
(when (plusp *mssp-fail*)
  (error "Memory-search storage port tests failed"))
