;;;; knowledge-graph-search-tests.lisp -- pure KG3 contract/traversal fixtures.
;;;; harness: full-system

(in-package :agent)

(unless (fboundp 'knowledge-graph-search-traverse)
  (load (test-source "knowledge-graph-ontology.lisp"))
  (load (test-source "knowledge-graph-formation.lisp"))
  (load (test-source "knowledge-graph-search.lisp")))

(defvar *kgs-pass* 0)
(defvar *kgs-fail* 0)

(defun kgs-check (name condition)
  (if condition
      (progn (incf *kgs-pass*) (format t "PASS ~a~%" name))
      (progn (incf *kgs-fail*) (format t "FAIL ~a~%" name))))

(defun kgs-node (id &optional (projection "generic-knowledge-graph"))
  (obj "projection_name" projection "node_id" id "node_kind" "concept"
       "label" id "aliases" #() "status" "current"
       "disclosure_class" "private"))

(defun kgs-edge (id from predicate to)
  (obj "projection_name" "generic-knowledge-graph" "edge_id" id
       "from_node_id" from "predicate" predicate "to_node_id" to
       "traversal_direction" "outgoing" "status" "current"))

(defun kgs-neighbor (node edge)
  (obj "node" node "edge" edge))

(format t "~%== KG3 pure graph search ==~%")

(let ((request (knowledge-graph-search-request :query "color accessibility")))
  (kgs-check "constructor emits the exact closed request shape"
             (knowledge-graph-search-request-valid-p request))
  (setf (gethash "extra" request) t)
  (kgs-check "unknown request keys fail closed"
             (not (knowledge-graph-search-request-valid-p request))))

(kgs-check "one of exact start or lexical query is required"
           (handler-case
               (progn (knowledge-graph-search-request) nil)
             (error () t)))

(kgs-check "an exact-query audit satisfies the closed request"
           (knowledge-graph-search-request-valid-p
            (knowledge-graph-search-request
             :exact-queries #( "Existing Pet" "Second Pet" ))))

(kgs-check "constructor rejects predicates outside the advertised ontology"
           (handler-case
               (progn
                 (knowledge-graph-search-request
                  :query "family" :predicates #( "is-wife-of"))
                 nil)
             (error () t)))

(kgs-check "typed family and attribute predicates are deliberately searchable"
           (knowledge-graph-search-request-valid-p
            (knowledge-graph-search-request
             :query "family ages"
             :predicates #( "parent_of" "has_age" ))))

(let* ((request (knowledge-graph-search-request
                 :starting-node-id "a" :query :null :maximum-depth 0
                 :maximum-paths 10))
       (calls 0)
       (result (knowledge-graph-search-traverse
                request (vector (kgs-node "a"))
                (lambda (node closed)
                  (declare (ignore node closed))
                  (incf calls)
                  (values #() nil)))))
  (kgs-check "depth zero returns the verified seed without traversal"
             (and (= 1 (gethash "path_count" result))
                  (= 0 calls)
                  (= 0 (gethash "depth" (aref (gethash "paths" result) 0))))))

(let* ((request (knowledge-graph-search-request
                 :starting-node-id "a" :query :null :maximum-depth 3
                 :maximum-paths 10))
       (result
         (knowledge-graph-search-traverse
          request (vector (kgs-node "a"))
          (lambda (node closed)
            (declare (ignore closed))
            (cond
              ((string= "a" (gethash "node_id" node))
               (values
                (vector
                 (kgs-neighbor (kgs-node "b")
                               (kgs-edge "ab" "a" "requires" "b")))
                nil))
              ((string= "b" (gethash "node_id" node))
               ;; The back edge proves path-local cycle suppression.
               (values
                (vector
                 (kgs-neighbor (kgs-node "a")
                               (kgs-edge "ba" "b" "relates-to" "a"))
                 (kgs-neighbor (kgs-node "c")
                               (kgs-edge "bc" "b" "supports" "c")))
                nil))
              (t (values #() nil)))))))
  (kgs-check "bounded BFS expands verified paths and suppresses cycles"
             (and (= 3 (gethash "path_count" result))
                  (equal '(0 1 2)
                         (map 'list (lambda (path) (gethash "depth" path))
                              (gethash "paths" result))))))

(let* ((request (knowledge-graph-search-request
                 :starting-node-id "a" :query :null :maximum-depth 2
                 :maximum-paths 1))
       (result (knowledge-graph-search-traverse
                request (vector (kgs-node "a"))
                (lambda (node closed)
                  (declare (ignore node closed))
                  (values (vector
                           (kgs-neighbor (kgs-node "b")
                                         (kgs-edge "ab" "a" "p" "b")))
                          t)))))
  (kgs-check "path and adapter clipping remain explicit"
             (and (= 1 (gethash "path_count" result))
                  (gethash "non_exhaustive" result)
                  (= 0 (gethash "database_write_count" result -1)))))

(let* ((request (knowledge-graph-search-request
                 :query "relationship-heavy seeds" :maximum-depth 1
                 :maximum-paths 2))
       (seeds (vector (kgs-node "a") (kgs-node "b") (kgs-node "c")))
       (result
         (knowledge-graph-search-traverse
          request seeds
          (lambda (node closed)
            (declare (ignore closed))
            (let* ((from (gethash "node_id" node))
                   (to (format nil "~a-detail" from)))
              (values
               (vector
                (kgs-neighbor (kgs-node to)
                              (kgs-edge (format nil "~a-edge" from)
                                        from "relates-to" to)))
               nil))))))
  (kgs-check "relationship paths are not crowded out by depth-zero seeds"
             (and (= 2 (gethash "path_count" result))
                  (every (lambda (path) (plusp (gethash "depth" path)))
                         (gethash "paths" result))
                  (gethash "non_exhaustive" result))))

(let* ((request (knowledge-graph-search-request
                 :query "cross projection" :maximum-depth 1
                 :maximum-paths 4))
       (seeds (vector (kgs-node "kg1-a" "conversation-episode-graph")
                      (kgs-node "kg2-a" "generic-knowledge-graph")))
       (result
         (knowledge-graph-search-traverse
          request seeds
          (lambda (node closed)
            (declare (ignore closed))
            (let ((from (gethash "node_id" node)))
              (values
               (coerce
                (loop for index from 1 to
                        (if (string= from "kg1-a") 12 1)
                      for to = (format nil "~a-detail-~d" from index)
                      collect
                      (kgs-neighbor
                       (kgs-node to (gethash "projection_name" node))
                       (kgs-edge (format nil "~a-edge-~d" from index)
                                 from "relates-to" to)))
                'vector)
               nil))))))
  (kgs-check "returned path ceiling represents every related projection"
             (and (= 4 (gethash "path_count" result))
                  (find "conversation-episode-graph"
                        (coerce (gethash "paths" result) 'list)
                        :test #'string=
                        :key (lambda (path)
                               (gethash "projection_name" path "")))
                  (find "generic-knowledge-graph"
                        (coerce (gethash "paths" result) 'list)
                        :test #'string=
                        :key (lambda (path)
                               (gethash "projection_name" path ""))))))

(let* ((target (kgs-node "zz-target"))
       (noise (kgs-node "aa-noise"))
       (request (knowledge-graph-search-request :query "health history"
                                               :maximum-paths 1)))
  (setf (gethash "label" target) "Health history"
        (gethash "label" noise) "Runtime history")
  (let ((result
          (knowledge-graph-search-traverse request (vector noise target)
            (lambda (node closed)
              (declare (ignore closed))
              (let ((id (gethash "node_id" node)))
                (values (vector (kgs-neighbor (kgs-node (concatenate 'string id "-detail"))
                    (kgs-edge (concatenate 'string id "-edge") id "related_to" "detail"))) nil))))))
    (kgs-check "relevant path beats lexicographically earlier noise before limit"
      (string= "zz-target" (gethash "node_id"
                            (aref (gethash "nodes" (aref (gethash "paths" result) 0)) 0))))))

(kgs-check "query words do not include conversational stop words"
  (equal '("health" "issues" "earlier" "year")
         (%kgs-query-terms "What were my health issues earlier this year?")))

(format t "~%KG3 pure search: ~d passed, ~d failed.~%"
        *kgs-pass* *kgs-fail*)
(when (plusp *kgs-fail*) (uiop:quit 1))
