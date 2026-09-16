;;;; knowledge-graph-search.lisp -- pure KG3 request and bounded traversal.

(in-package :agent)

(export '(knowledge-graph-search-request
          knowledge-graph-search-supported-predicates
          knowledge-graph-search-request-valid-p
          knowledge-graph-search-traverse
          knowledge-graph-search-compact-result))

(defparameter *knowledge-graph-search-revision*
  "knowledge-graph-search-v4")
(defparameter *knowledge-graph-search-maximum-seeds* 16)
(defparameter *knowledge-graph-search-maximum-neighbors* 64)
(defparameter *knowledge-graph-search-maximum-evidence-per-owner* 64)

(defun knowledge-graph-search-supported-predicates ()
  "Return the exact predicate vocabulary accepted by deliberate search.

The fixed ontology owns KG2 predicates. HAS-CONCEPT is the sole structural
predicate exposed by the separately verified KG1 episode/concept projection.
An empty predicate vector remains the natural-language, unfiltered path."
  (coerce
   (append (mapcar #'first *knowledge-graph-ontology-signatures*)
           (mapcar #'first *knowledge-graph-family-ontology-signatures*)
           '("has-concept"))
   'vector))

(defun %kgs-items (value)
  (cond ((null value) nil)
        ((vectorp value) (coerce value 'list))
        ((listp value) value)
        (t nil)))

(defun %kgs-present-string-p (value &optional (maximum 512))
  (and (stringp value)
       (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                   value)))
       (<= (length value) maximum)))

(defun %kgs-null-p (value)
  (or (null value) (eq value :null)))

(defun knowledge-graph-search-request-valid-p (request)
  (and (hash-table-p request)
       (equal '("direction" "evidence_policy" "exact_queries"
                "maximum_depth" "maximum_paths" "predicates" "query"
                "starting_node_id")
              (sort (loop for key being the hash-keys of request collect key)
                    #'string<))
       (let ((start (gethash "starting_node_id" request))
             (query (gethash "query" request))
             (exact-queries (gethash "exact_queries" request)))
         (and (or (%kgs-null-p start) (%kgs-present-string-p start 240))
              (or (%kgs-null-p query) (%kgs-present-string-p query 1000))
              (or (%kgs-present-string-p start 240)
                  (%kgs-present-string-p query 1000)
                  (and (vectorp exact-queries)
                       (plusp (length exact-queries))))))
       (let ((exact-queries (gethash "exact_queries" request)))
         (and (vectorp exact-queries)
              (<= (length exact-queries) 16)
              (every (lambda (value) (%kgs-present-string-p value 240))
                     exact-queries)
              (= (length exact-queries)
                 (length (remove-duplicates (coerce exact-queries 'list)
                                            :test #'string=)))))
       (let ((predicates (gethash "predicates" request)))
         (and (vectorp predicates) (<= (length predicates) 16)
              (every (lambda (value) (%kgf-token-p value)) predicates)
              (every (lambda (value)
                       (find value
                             (knowledge-graph-search-supported-predicates)
                             :test #'string=))
                     predicates)
              (= (length predicates)
                 (length (remove-duplicates (coerce predicates 'list)
                                            :test #'string=)))))
       (member (gethash "direction" request)
               '("outgoing" "incoming" "both") :test #'string=)
       (member (gethash "evidence_policy" request)
               '("verified" "inferred" "all") :test #'string=)
       (let ((depth (gethash "maximum_depth" request)))
         (and (integerp depth) (<= 0 depth 3)))
       (let ((paths (gethash "maximum_paths" request)))
         (and (integerp paths) (<= 1 paths 20)))))

(defun knowledge-graph-search-request
    (&key (starting-node-id :null) (query :null) (exact-queries #())
          (predicates #())
          (direction "both") (maximum-depth 2) (maximum-paths 10)
          (evidence-policy "verified"))
  (let ((request
          (obj "starting_node_id" (or starting-node-id :null)
               "query" (or query :null)
               "exact_queries"
               (cond ((vectorp exact-queries) (copy-seq exact-queries))
                     ((listp exact-queries) (coerce exact-queries 'vector))
                     (t exact-queries))
               "predicates"
               (cond ((vectorp predicates) (copy-seq predicates))
                     ((listp predicates) (coerce predicates 'vector))
                     (t predicates))
               "direction" direction "evidence_policy" evidence-policy
               "maximum_depth" maximum-depth
               "maximum_paths" maximum-paths)))
    (unless (knowledge-graph-search-request-valid-p request)
      (error "Graph search request violates the closed KG3 contract"))
    request))

(defun %kgs-copy-object (value)
  (let ((copy (make-hash-table :test #'equal)))
    (maphash (lambda (key child)
               (setf (gethash key copy)
                     (if (vectorp child) (copy-seq child) child)))
             value)
    copy))

(defun %kgs-copy-selected-fields (source fields)
  (let ((copy (make-hash-table :test #'equal)))
    (dolist (field fields copy)
      (multiple-value-bind (value present-p) (gethash field source)
        (when present-p
          (setf (gethash field copy)
                (if (vectorp value) (copy-seq value) value)))))))

(defun %kgs-compact-string (value maximum)
  (if (and (stringp value) (> (length value) maximum))
      (concatenate 'string (subseq value 0 (1- maximum)) "…")
      value))

(defun %kgs-origin-agent-id (source-id)
  "Return the explicit source agent encoded by a migrated evidence ID."
  (when (and (stringp source-id)
             (or (uiop:string-prefix-p "migrated-event:" source-id)
                 (uiop:string-prefix-p "migrated-memory:" source-id)))
    (let* ((first (position #\: source-id))
           (second (and first (position #\: source-id :start (1+ first)))))
      (when (and first second (> second (1+ first)))
        (subseq source-id (1+ first) second)))))

(defun %kgs-origin-agent-ids (source-ids)
  (coerce
   (sort (remove-duplicates
          (loop for source-id in (%kgs-items source-ids)
                for origin = (%kgs-origin-agent-id source-id)
                when origin collect origin)
          :test #'string=)
         #'string<)
   'vector))

(defun %kgs-compact-node (node)
  (let ((copy (%kgs-copy-selected-fields
               node
               '("projection_name" "node_id" "node_kind" "label" "aliases"
                 "classifications" "status" "disclosure_class"
                 "evidence_status" "origin_agent_ids"))))
    (multiple-value-bind (label present-p) (gethash "label" copy)
      (when present-p
        (setf (gethash "label" copy) (%kgs-compact-string label 240))))
    (let ((aliases (gethash "aliases" copy)))
      (when (vectorp aliases)
        (setf (gethash "aliases" copy)
              (map 'vector (lambda (alias) (%kgs-compact-string alias 96))
                   (subseq aliases 0 (min 3 (length aliases)))))))
    (let ((classifications (gethash "classifications" copy)))
      (when (vectorp classifications)
        (setf (gethash "classifications" copy)
              (map 'vector
                   (lambda (classification)
                     (%kgs-compact-string classification 64))
                   (subseq classifications
                           0 (min 5 (length classifications)))))))
    copy))

(defun %kgs-compact-exact-query-result (audit)
  (let ((copy (%kgs-copy-selected-fields
               audit
               '("query" "match_count" "returned_match_count"
                 "scan_complete" "non_exhaustive" "absence_confirmed"
                 "absence_note"))))
    (setf (gethash "matches" copy)
          (map 'vector #'%kgs-compact-node
               (gethash "matches" audit #())))
    copy))

(defun knowledge-graph-search-compact-result (result)
  "Deduplicate one rich traversal into its bounded presentation graph.

The storage result remains evidence-rich for internal consumers.  Native tools
and human visualization receive node and edge descriptors once, with opaque
identities preserved for exact follow-up traversal.  Large evidence/source
vectors and repeated per-path copies deliberately do not cross this boundary."
  (unless (and (hash-table-p result)
               (= 1 (gethash "schema_version" result -1))
               (vectorp (gethash "paths" result)))
    (error "Graph search result cannot be compacted"))
  (let ((nodes nil) (edges nil)
        (seen-nodes (make-hash-table :test #'equal))
        (seen-edges (make-hash-table :test #'equal)))
    (labels ((admit-node (node)
               (let ((key (cons (gethash "projection_name" node)
                                (gethash "node_id" node))))
                 (unless (gethash key seen-nodes)
                   (setf (gethash key seen-nodes) t)
                   (push (%kgs-compact-node node) nodes)))))
      ;; Lexically matched seeds are a distinct bounded result surface. A BFS
      ;; path budget may constrain expansion, but cannot make a true query
      ;; match disappear from the compact operator/model result.
      (loop for node across (gethash "query_match_nodes" result #())
            do (admit-node node))
      (loop for path across (gethash "paths" result)
            do (loop for node across (gethash "nodes" path #())
                     do (admit-node node))
               (loop for edge across (gethash "edges" path #())
                     for id = (gethash "edge_id" edge)
                     for key = (cons (gethash "projection_name" edge) id)
                     unless (gethash key seen-edges)
                       do (setf (gethash key seen-edges) t)
                          (let ((copy
                                  (%kgs-copy-selected-fields
                                   edge
                                   '("projection_name" "edge_id" "from_node_id"
                                     "predicate" "to_node_id"
                                     "traversal_direction" "status"
                                     "evidence_status" "valid_from" "valid_to"
                                     "origin_agent_ids"))))
                            ;; This is a deterministic epistemic indicator, not
                            ;; a model-calibrated probability.  It gives agents
                            ;; and human tools a stable path to confirm useful
                            ;; inferences without treating them as verified.
                            (setf (gethash "confirmation_recommended" copy)
                                  (if (string= "inference"
                                               (gethash "evidence_status" copy ""))
                                      t nil))
                            (push copy edges)))))
    (let ((node-origins (make-hash-table :test #'equal)))
      (dolist (edge edges)
        (loop for origin across (gethash "origin_agent_ids" edge #()) do
          (dolist (node-id (list (gethash "from_node_id" edge)
                                 (gethash "to_node_id" edge)))
            (pushnew origin (gethash node-id node-origins) :test #'string=))))
      (dolist (node nodes)
        (setf (gethash "origin_agent_ids" node)
              (coerce (sort (copy-list (gethash (gethash "node_id" node)
                                                 node-origins))
                            #'string<)
                      'vector))))
    (let* ((answer-status
             (gethash "answer_status" result
                      (cond ((plusp (gethash "matched_fact_count" result 0))
                             "query-facts")
                            ((string= "exact-identity"
                                      (gethash "query_match_kind" result ""))
                             "identity-only")
                            ((string= "related-suggestions"
                                      (gethash "query_match_kind" result ""))
                             "suggestions-only")
                            (t "no-match"))))
           (compact
            (obj "schema_version" 1
                 "search_revision" (gethash "search_revision" result)
                 "status" (gethash "status" result "empty")
                 "seed_count" (gethash "seed_count" result 0)
                  "path_count" (gethash "path_count" result 0)
                  "node_count" (length nodes)
                  "edge_count" (length edges)
                  "returned_node_count" (length nodes)
                  "returned_edge_count" (length edges)
                  "graph_entity_count"
                  (gethash "graph_entity_count" result :null)
                  "graph_fact_count"
                  (gethash "graph_fact_count" result :null)
                  "result_scope"
                  (gethash "result_scope" result "bounded-traversal-subset")
                  "scope_note"
                  (gethash
                   "scope_note" result
                   "node_count and edge_count are returned-subset counts, not graph totals. A zero-match absence is established only by an exact_query_results item whose absence_confirmed is true.")
                 "query_match_count"
                  (gethash "query_match_count" result 0)
                 "query_match_kind"
                  (gethash "query_match_kind" result "none")
                  "answer_status" answer-status
                  "matched_fact_count"
                  (gethash "matched_fact_count" result 0)
                  "suggestion_count"
                  (gethash "suggestion_count" result 0)
                  "omitted_reviewed_inference_count"
                  (gethash "omitted_reviewed_inference_count" result 0)
                  "retrieval_hint"
                  (gethash "retrieval_hint" result :null)
                  "answer_note"
                  (cond ((string= "suggestions-only" answer-status)
                         "Related suggestions do not establish the requested fact.")
                        ((string= "identity-with-relations" answer-status)
                         "Exact identity found with bounded adjacent relationships. Inspect each edge's evidence_status; inferred relationships are not verified facts. matched_fact_count counts separate factual-context matches, not these traversal edges.")
                        (t :null))
                 "nodes" (coerce (nreverse nodes) 'vector)
                 "edges" (coerce (nreverse edges) 'vector)
                 "exact_query_count"
                 (length (gethash "exact_query_results" result #()))
                 "exact_query_results"
                 (map 'vector #'%kgs-compact-exact-query-result
                      (gethash "exact_query_results" result #()))
                 "non_exhaustive" (if (gethash "non_exhaustive" result) t nil)
                 "database_write_count"
                 (gethash "database_write_count" result 0))))
      (dolist (field '("starting_node_status" "unresolved_starting_node_id"
                       "hybrid_link_seed_count"))
        (multiple-value-bind (value present-p) (gethash field result)
          (when present-p (setf (gethash field compact) value))))
      compact)))

(defun %kgs-path (projection nodes edges)
  (obj "projection_name" projection
       "depth" (length edges)
       "nodes" (coerce (mapcar #'%kgs-copy-object nodes) 'vector)
       "edges" (coerce (mapcar #'%kgs-copy-object edges) 'vector)))

(defun %kgs-path-key (path)
  (format nil "~3,'0d|~a|~{~a~^|~}"
          (gethash "depth" path)
          (gethash "projection_name" path)
          (mapcar (lambda (node) (gethash "node_id" node ""))
                  (%kgs-items (gethash "nodes" path)))))

(defparameter *kgs-query-stop-words*
  '("a" "an" "and" "are" "as" "at" "be" "been" "but" "by" "can"
    "could" "did" "do" "does" "for" "from" "had" "has" "have" "how"
    "i" "in" "is" "it" "me" "my" "of" "on" "or" "our" "please"
    "that" "the" "their" "them" "there" "these" "this" "to" "was"
    "we" "were" "what" "when" "which" "with" "would" "you" "your"))

(defun %kgs-query-terms (query)
  "Content words only; no schema keys, persona vocabulary or domain synonyms."
  (let ((tokens nil) (characters nil))
    (labels ((flush ()
               (when characters
                 (let ((word (string-downcase (coerce (nreverse characters) 'string))))
                   (unless (or (< (length word) 2)
                               (member word *kgs-query-stop-words* :test #'string=))
                     (pushnew word tokens :test #'string=)))
                 (setf characters nil))))
      (loop for character across (if (stringp query) query "")
            do (if (alphanumericp character) (push character characters) (flush)))
      (flush))
    (let ((ordered (nreverse tokens)))
      (subseq ordered 0 (min 12 (length ordered))))))

(defun %kgs-field-score (text terms weight)
  (if (stringp text)
      (* weight (count-if
                 (lambda (term)
                   (loop for start = 0 then (1+ position)
                         for position = (search term text :start2 start :test #'char-equal)
                         while position
                         thereis (and
                           (or (zerop position)
                               (not (alphanumericp (char text (1- position)))))
                           (or (= (+ position (length term)) (length text))
                               (not (alphanumericp (char text (+ position (length term)))))))))
                 terms))
      0))

(defun %kgs-node-relevance (node terms)
  (+ (%kgs-field-score (gethash "label" node) terms
                       (if (string= "episode" (gethash "node_kind" node "")) 2 8))
     (loop for alias in (%kgs-items (gethash "aliases" node))
           maximize (%kgs-field-score alias terms 8) into score
           finally (return (or score 0)))
     (loop for classification in
             (%kgs-items (gethash "classifications" node))
           maximize (%kgs-field-score classification terms 6) into score
           finally (return (or score 0)))
     (%kgs-field-score (gethash "retrieval_context" node) terms 4)
     (%kgs-field-score (gethash "summary" node) terms 2)
     (%kgs-field-score (gethash "evidence_note" node) terms 1)))

(defun %kgs-path-relevance (path terms)
  ;; Do not reward longer paths merely for repeating the same matching hub.
  (loop for node across (gethash "nodes" path)
        maximize (%kgs-node-relevance node terms)))

(defun %kgs-fair-projection-order (rows row-key &optional score-fn)
  "Round-robin across projections, relevance first within each projection."
  (let* ((ordered
           (stable-sort (copy-list rows)
             (lambda (left right)
               (let ((ls (if score-fn (funcall score-fn left) 0))
                     (rs (if score-fn (funcall score-fn right) 0)))
                 (if (/= ls rs) (> ls rs)
                     (string< (funcall row-key left) (funcall row-key right)))))))
         (projections
           (sort (remove-duplicates
                  (mapcar (lambda (row)
                            (gethash "projection_name" row ""))
                          ordered)
                  :test #'string=)
                 #'string<))
         (buckets
           (mapcar
            (lambda (projection)
              (cons projection
                    (remove-if-not
                     (lambda (row)
                       (string= projection
                                (gethash "projection_name" row "")))
                     ordered)))
            projections))
         (result nil)
         (progress t))
    (loop while progress
          do (setf progress nil)
             (dolist (bucket buckets)
               (when (cdr bucket)
                 (setf progress t)
                 (push (pop (cdr bucket)) result))))
    (nreverse result)))

(defun %kgs-select-paths-fairly (paths maximum &optional terms)
  (let* ((fair (%kgs-fair-projection-order paths #'%kgs-path-key
                  (lambda (path) (%kgs-path-relevance path terms))))
         (clipped (> (length fair) maximum)))
    (values (subseq fair 0 (min maximum (length fair))) clipped)))

(defun knowledge-graph-search-traverse
    (request seeds neighbor-fn &key (non-exhaustive-p nil))
  "Pure deterministic BFS over verified SEEDS and an injected read-only port.

NEIGHBOR-FN receives one node descriptor and the closed request, returning a
vector of {edge,node} rows and a truth value indicating truncation."
  (unless (and (knowledge-graph-search-request-valid-p request)
               (vectorp seeds) (functionp neighbor-fn)
               (every #'hash-table-p seeds))
    (error "Graph traversal inputs are invalid"))
  (let* ((maximum (gethash "maximum_paths" request))
         (terms (%kgs-query-terms (gethash "query" request)))
         (maximum-depth (gethash "maximum_depth" request))
         (ordered-seeds
           (%kgs-fair-projection-order
            (mapcar #'%kgs-copy-object (coerce seeds 'list))
            (lambda (row) (gethash "node_id" row ""))
            (lambda (row) (%kgs-node-relevance row terms))))
         (queue (mapcar (lambda (seed)
                          (list (gethash "projection_name" seed)
                                (list seed)
                                nil))
                        ordered-seeds))
         ;; Seed identities are useful fallback results, but they must not
         ;; consume the entire public result budget before BFS reaches an
         ;; edge. Relationship-bearing paths are selected first; remaining
         ;; capacity is then filled with deterministic depth-zero seeds.
         (seed-paths
           (mapcar (lambda (seed)
                     (%kgs-path (gethash "projection_name" seed)
                                (list seed) nil))
                   ordered-seeds))
         (relationship-paths nil)
         ;; The public path ceiling must not make the first projection in
         ;; deterministic order the only graph observed. This remains a hard
         ;; bound while allowing every bounded seed's first-hop relationships
         ;; to become candidates for fair final selection.
         (candidate-limit
           (max maximum
                (* *knowledge-graph-search-maximum-neighbors*
                   (max 1 (length ordered-seeds)))))
         (truncated non-exhaustive-p))
    (labels ((seen-node-p (node-id nodes)
               (find node-id nodes :test #'string=
                     :key (lambda (row) (gethash "node_id" row ""))))
             (enqueue-neighbor (projection nodes edges neighbor)
               (let* ((node (gethash "node" neighbor))
                      (edge (gethash "edge" neighbor))
                      (node-id (and (hash-table-p node)
                                    (gethash "node_id" node))))
                 (when (and (hash-table-p node)
                            (hash-table-p edge)
                            (stringp node-id)
                            (not (seen-node-p node-id nodes)))
                   (setf queue
                         (nconc queue
                                (list (list projection
                                            (append nodes (list node))
                                            (append edges (list edge))))))))))
      (loop while queue
            do (let* ((entry (pop queue))
                      (projection (first entry))
                      (nodes (second entry))
                      (edges (third entry))
                      (depth (length edges)))
                 (when (plusp depth)
                   (push (%kgs-path projection nodes edges)
                         relationship-paths)
                   (when (>= (length relationship-paths) candidate-limit)
                     (when queue (setf truncated t))
                     (setf queue nil)
                     (return)))
                 (when (< depth maximum-depth)
                   (multiple-value-bind (neighbors clipped-p)
                       (funcall neighbor-fn (car (last nodes)) request)
                     (when clipped-p
                       (setf truncated t))
                     (dolist (neighbor (%kgs-items neighbors))
                       (enqueue-neighbor projection nodes edges neighbor)))))))
    (multiple-value-bind (selected selection-clipped-p)
        (%kgs-select-paths-fairly (nreverse relationship-paths) maximum terms)
      (when selection-clipped-p
        (setf truncated t))
      (let ((remaining (- maximum (length selected))))
      (when (plusp remaining)
        (let ((fallback (subseq seed-paths 0 (min remaining
                                                  (length seed-paths)))))
          (setf selected (append selected fallback))
          (when (> (length seed-paths) remaining)
            (setf truncated t))))
      (let ((ordered
              (stable-sort selected
                (lambda (left right)
                  (let ((ls (%kgs-path-relevance left terms))
                        (rs (%kgs-path-relevance right terms)))
                    (if (/= ls rs) (> ls rs)
                        (string< (%kgs-path-key left) (%kgs-path-key right))))))))
      (obj "schema_version" 1
           "search_revision" *knowledge-graph-search-revision*
           "seed_count" (length ordered-seeds)
           "path_count" (length ordered)
           "paths" (coerce ordered 'vector)
           "non_exhaustive" (if truncated t nil)
           "database_write_count" 0))))))
