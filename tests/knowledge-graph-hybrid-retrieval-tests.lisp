;;;; knowledge-graph-hybrid-retrieval-tests.lisp -- pure KG4 bridge fixtures.

;;;; harness: full-system

(in-package :agent)

(defvar *kgh-pass* 0)
(defvar *kgh-fail* 0)

(defun kgh-check (name condition)
  (if condition
      (progn (incf *kgh-pass*) (format t "PASS ~a~%" name))
      (progn (incf *kgh-fail*) (format t "FAIL ~a~%" name))))

(format t "~%== KG4 pure hybrid retrieval ==~%")

(let* ((captured-source nil)
       (captured-events nil)
       (semantic
         (vector
          (obj "id" "memory-a" "source_event_id" 101
               "evidence_node_ids" #( "memory-b" "memory-a"))))
       (episodes
         (vector
          (obj "episode_id" "episode:one" "event_id" 120
               "source_event_ids" #(101 102))))
       (result
         (knowledge-graph-hybrid-retrieve
          (knowledge-graph-search-request :query "something about color")
          semantic episodes
          (lambda (request source-ids event-ids)
            (declare (ignore request))
            (setf captured-source source-ids captured-events event-ids)
            (obj "schema_version" 1 "status" "available" "paths" #()
                 "path_count" 0 "non_exhaustive" nil
                 "database_write_count" 0)))))
  (kgh-check "semantic memory and episode identifiers become exact link seeds"
             (equal (coerce captured-source 'list)
                    '("memory:memory-a" "memory:memory-b" "episode:one")))
  (kgh-check "candidate evidence event IDs are stable and deduplicated"
             (equal (coerce captured-events 'list) '(101 102 120)))
  (kgh-check "hybrid result names typed candidate coverage"
             (and (string= "knowledge-graph-hybrid-retrieval-v1"
                           (gethash "hybrid_revision" result))
                  (= 1 (gethash "semantic_candidate_count" result))
                  (= 1 (gethash "episodic_candidate_count" result))
                  (not (gethash "hybrid_non_exhaustive" result)))))

(let* ((semantic
         (coerce (loop for index below 12
                       collect (obj "id" (format nil "m~d" index)))
                 'vector))
       (result
         (knowledge-graph-hybrid-retrieve
          (knowledge-graph-search-request :query "bounded") semantic #()
          (lambda (request source-ids event-ids)
            (declare (ignore request source-ids event-ids))
            (obj "schema_version" 1 "status" "empty" "paths" #()
                 "path_count" 0 "non_exhaustive" nil
                 "database_write_count" 0))
          :episodic-status "unavailable")))
  (kgh-check "candidate clipping and unavailable families stay truthful"
             (and (= 8 (gethash "semantic_candidate_count" result))
                  (gethash "hybrid_non_exhaustive" result)
                  (gethash "non_exhaustive" result))))

(format t "~%KG4 hybrid retrieval: ~d passed, ~d failed.~%"
        *kgh-pass* *kgh-fail*)
(when (plusp *kgh-fail*) (uiop:quit 1))
