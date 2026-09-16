;;;; knowledge-graph-search-tool-tests.lisp -- KG3 native boundary fixtures.

;;;; harness: full-system

(in-package :agent)

(defvar *kgst-pass* 0)
(defvar *kgst-fail* 0)

(defun kgst-check (name condition)
  (if condition
      (progn (incf *kgst-pass*) (format t "PASS ~a~%" name))
      (progn (incf *kgst-fail*) (format t "FAIL ~a~%" name))))

(format t "~%== KG3 graph-search tool ==~%")

(let* ((schema (knowledge-graph-search-tool-schema))
       (function (gethash "function" schema))
       (parameters (gethash "parameters" function))
       (exact-query-schema
         (gethash "exact_queries" (gethash "properties" parameters)))
       (predicate-schema
         (gethash "predicates" (gethash "properties" parameters)))
       (predicate-enum (gethash "enum" (gethash "items" predicate-schema))))
  (kgst-check "native tool is search-graph with a closed object schema"
              (and (string= "search-graph" (gethash "name" function))
                   (eq nil (gethash "additionalProperties" parameters :absent))
                   (equalp #() (gethash "required" parameters))
                   (search "exact opaque node_id"
                           (gethash "description"
                                    (gethash "starting_node_id"
                                             (gethash "properties" parameters))))))
  (kgst-check "tool advertises exact ontology and legacy predicates"
              (and (find "owns" predicate-enum :test #'string=)
                   (find "parent_of" predicate-enum :test #'string=)
                   (find "has_age" predicate-enum :test #'string=)
                   (find "has-concept" predicate-enum :test #'string=)
                   (not (find "is-wife-of" predicate-enum :test #'string=))))
  (kgst-check "tool exposes one bounded multi-name exact audit"
              (and (= 16 (gethash "maxItems" exact-query-schema))
                   (eq t (gethash "uniqueItems" exact-query-schema))
                   (= 1 (gethash "minLength"
                                 (gethash "items" exact-query-schema)))
                   (= 240 (gethash "maxLength"
                                   (gethash "items" exact-query-schema))))))

(let ((request (knowledge-graph-search-request
                :query "operator requirements" :maximum-depth 1)))
  (kgst-check "normalizer accepts only the shared closed request"
              (knowledge-graph-search-request-valid-p
               (knowledge-graph-search-tool-normalize request)))
  (setf (gethash "unknown" request) 1)
  (kgst-check "normalizer rejects model-added metadata"
              (handler-case
                  (progn (knowledge-graph-search-tool-normalize request) nil)
                (error () t))))

(let* ((sparse (obj "query" "operator color vision"))
       (normalized (knowledge-graph-search-tool-normalize sparse)))
  (kgst-check "normalizer deterministically supplies optional traversal defaults"
              (and (or (eq :null (gethash "starting_node_id" normalized))
                       (null (gethash "starting_node_id" normalized)))
                   (vectorp (gethash "predicates" normalized))
                   (zerop (length (gethash "predicates" normalized)))
                   (vectorp (gethash "exact_queries" normalized))
                   (zerop (length (gethash "exact_queries" normalized)))
                   (string= "both" (gethash "direction" normalized))
                   (string= "verified" (gethash "evidence_policy" normalized))
                   (= 2 (gethash "maximum_depth" normalized))
                   (= 6 (gethash "maximum_paths" normalized)))))

(let ((normalized
        (knowledge-graph-search-tool-normalize
         (obj "exact_queries" #( "Existing Pet" "Second Pet" "Project Alpha" )))))
  (kgst-check "normalizer accepts an audit-only multi-name request"
              (and (knowledge-graph-search-request-valid-p normalized)
                   (equalp #( "Existing Pet" "Second Pet" "Project Alpha" )
                           (gethash "exact_queries" normalized))
                   (eq :null (gethash "query" normalized)))))

(dolist (queries (list (make-array 17 :initial-element "name")
                       #( "duplicate" "duplicate" )
                       #( "" )))
  (kgst-check "normalizer rejects an invalid exact-query batch"
              (handler-case
                  (progn
                    (knowledge-graph-search-tool-normalize
                     (obj "exact_queries" queries))
                    nil)
                (error () t))))

(let ((request (knowledge-graph-search-request :query "family")))
  (setf (gethash "predicates" request) #( "is-wife-of"))
  (kgst-check "normalizer rejects invented predicate filters"
              (handler-case
                  (progn (knowledge-graph-search-tool-normalize request) nil)
                (error () t))))

(kgst-check "normalizer accepts the reviewed inference policy"
            (knowledge-graph-search-request-valid-p
             (knowledge-graph-search-tool-normalize
              (obj "query" "family" "evidence_policy" "inferred"))))

(let* ((result (obj "schema_version" 1 "status" "empty" "paths" #()
                    "seed_count" 0 "path_count" 0
                    "database_write_count" 0))
       (decoded (shasht:read-json
                 (knowledge-graph-search-tool-render result))))
  (kgst-check "renderer preserves a structural result without prose parsing"
              (and (string= "empty" (gethash "status" decoded))
                   (string= "no-match" (gethash "answer_status" decoded))
                   (vectorp (gethash "nodes" decoded))
                   (vectorp (gethash "edges" decoded))
                   (zerop (gethash "returned_node_count" decoded))
                   (zerop (gethash "returned_edge_count" decoded))
                   (search "not graph totals"
                           (gethash "scope_note" decoded)))))

(let* ((edge (obj "projection_name" "grounded" "edge_id" "edge:inferred"
                  "from_node_id" "node:one" "predicate" "related_to"
                  "to_node_id" "node:two" "traversal_direction" "outgoing"
                  "status" "current" "evidence_status" "inference"))
       (result (obj "schema_version" 1 "status" "available"
                    "paths" (vector (obj "nodes" #() "edges" (vector edge)))
                    "seed_count" 1 "path_count" 1
                    "database_write_count" 0))
       (decoded (shasht:read-json
                 (knowledge-graph-search-tool-render result)))
       (rendered-edge (aref (gethash "edges" decoded) 0)))
  (kgst-check "renderer marks inference as confirmation-worthy without a fake score"
              (and (string= "inference"
                            (gethash "evidence_status" rendered-edge))
                   (eq t (gethash "confirmation_recommended" rendered-edge))
                   (not (nth-value 1 (gethash "confidence" rendered-edge))))))

(let* ((result
         (obj "schema_version" 1 "status" "available" "paths" #()
              "graph_entity_count" 63 "graph_fact_count" 145
              "exact_query_results"
              (vector
               (obj "query" "Existing Pet" "match_count" 1
                    "returned_match_count" 1
                    "matches"
                    (vector (obj "projection_name" "grounded"
                                 "node_id" "node:existing-pet" "node_kind" "pet"
                                 "label" "Existing Pet"))
                    "scan_complete" t "non_exhaustive" nil
                    "absence_confirmed" nil
                    "absence_note" "One exact match was found.")
               (obj "query" "Absent Relative" "match_count" 0
                    "returned_match_count" 0 "matches" #()
                    "scan_complete" t "non_exhaustive" nil
                    "absence_confirmed" t
                    "absence_note" "No exact match in the complete index.")
               (obj "query" "Unknown" "match_count" 0
                    "returned_match_count" 0 "matches" #()
                    "scan_complete" nil "non_exhaustive" t
                    "absence_confirmed" nil
                    "absence_note" "Incomplete scan."))
              "database_write_count" 0))
       (decoded (shasht:read-json
                 (knowledge-graph-search-tool-render result)))
       (audits (gethash "exact_query_results" decoded)))
  (kgst-check "renderer separates global totals from the returned subset"
              (and (= 63 (gethash "graph_entity_count" decoded))
                   (= 145 (gethash "graph_fact_count" decoded))
                   (zerop (gethash "node_count" decoded))
                   (zerop (gethash "returned_node_count" decoded))))
  (kgst-check "renderer preserves exact-audit absence authority"
              (and (= 3 (gethash "exact_query_count" decoded))
                   (= 1 (gethash "match_count" (aref audits 0)))
                   (eq t (gethash "absence_confirmed" (aref audits 1)))
                   (null (gethash "absence_confirmed" (aref audits 2)))
                   (eq t (gethash "non_exhaustive" (aref audits 2)))
                   (= 1 (length (gethash "matches" (aref audits 0)))))))

(let* ((result (obj "schema_version" 1 "status" "available" "paths" #()
                    "query_match_kind" "related-suggestions"
                    "query_match_count" 1 "suggestion_count" 1
                    "query_match_nodes"
                    (vector (obj "projection_name" "grounded"
                                 "node_id" "node:suggestion"
                                 "node_kind" "person" "label" "Possible match"))
                    "database_write_count" 0))
       (decoded (shasht:read-json
                 (knowledge-graph-search-tool-render result))))
  (kgst-check "suggestions are visibly distinct from answer facts"
              (and (string= "suggestions-only"
                            (gethash "answer_status" decoded))
                   (zerop (gethash "matched_fact_count" decoded))
                   (= 1 (gethash "suggestion_count" decoded))
                   (search "do not establish"
                           (gethash "answer_note" decoded)))))

(let* ((node-a (obj "projection_name" "grounded" "node_id" "node:a"
                    "node_kind" "person" "label" "Operator"
                    "evidence_event_ids" #(1 2 3)))
       (node-b (obj "projection_name" "grounded" "node_id" "node:b"
                    "node_kind" "organism"
                    "label" (make-string 400 :initial-element #\x)
                    "aliases" #( "pet" "animal" "companion" "fourth" )
                    "classifications"
                    #( "cat" "pet" "animal" "companion" "rascal" "sixth" )
                    "evidence_event_ids" #(4 5)))
       (query-match
         (obj "projection_name" "grounded" "node_id" "node:match"
              "node_kind" "state" "label" "Anemia" "aliases" #()
              "classifications" #( "health" "condition" )
              "status" "current" "disclosure_class" "private"
              "evidence_status" "direct"))
       (edge (obj "projection_name" "grounded" "edge_id" "edge:1"
                  "from_node_id" "node:a" "predicate" "owns"
                  "to_node_id" "node:b" "evidence_event_ids" #(6 7)))
       (path (obj "projection_name" "grounded" "depth" 1
                  "nodes" (vector node-a node-b) "edges" (vector edge)))
       (result (obj "schema_version" 1 "status" "available"
                     "seed_count" 1 "path_count" 2
                     "query_match_count" 1
                     "query_match_nodes" (vector query-match)
                     "paths" (vector path path) "database_write_count" 0))
       (rendered (knowledge-graph-search-tool-render result))
       (decoded (shasht:read-json rendered)))
  (kgst-check "renderer deduplicates nodes and edges while retaining opaque IDs"
              (and (= 3 (length (gethash "nodes" decoded)))
                   (= 1 (gethash "query_match_count" decoded))
                   (= 1 (length (gethash "edges" decoded)))
                   (string= "node:b"
                             (gethash "node_id" (aref (gethash "nodes" decoded) 2)))
                   (string= "edge:1"
                            (gethash "edge_id" (aref (gethash "edges" decoded) 0)))))
  (kgst-check "renderer excludes repeated evidence payloads"
              (and (< (length rendered) 3000)
                   (null (search "evidence_event_ids" rendered))))
  (kgst-check "renderer bounds labels and aliases independently of storage"
              (let ((compact-node (aref (gethash "nodes" decoded) 2)))
                (and (= 240 (length (gethash "label" compact-node)))
                     (= 3 (length (gethash "aliases" compact-node)))
                     (= 5 (length
                           (gethash "classifications" compact-node)))
                     (find "cat" (gethash "classifications" compact-node)
                           :test #'string=)))))

(format t "~%KG3 tool: ~d passed, ~d failed.~%"
        *kgst-pass* *kgst-fail*)
(when (plusp *kgst-fail*) (uiop:quit 1))
