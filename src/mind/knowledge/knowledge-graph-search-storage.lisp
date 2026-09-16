;;;; knowledge-graph-search-storage.lisp -- verified zero-write KG3 adapter.

(in-package :agent)

(export '(knowledge-graph-search-storage))

(defun %kgss-like-pattern (text)
  (%kgfs-like-pattern text))

(defun %kgss-semantic-evidence-allowed-p (descriptor policy)
  (let ((status (gethash "evidence_status" descriptor "unreviewed")))
    (or (string= policy "all")
        (and (string= policy "inferred")
             (member status '("direct" "prior-graph" "inference")
                     :test #'string=))
        (and (string= policy "verified")
             (member status '("direct" "prior-graph") :test #'string=)))))

(defun %kgss-read-query-nodes (handle projection agent-id persona-id query policy)
  "Rank meaningful JSON values in SQLite before LIMIT, never JSON key names.
JSON extraction is a read expression, not a new persisted projection. Existing
row integrity and disclosure checks still apply to every returned descriptor."
  (let* ((terms (%kgs-query-terms query))
         (fields '(("label" . 8) ("aliases" . 8) ("classifications" . 6)
                   ("synopsis" . 2)
                   ("retrieval_cues" . 4) ("broader_categories" . 4)
                   ("evidence_note" . 1)))
         (search-columns
           (loop for (field . weight) in fields
                 collect (format nil "~a AS search_~a"
                   (%kgss-sql-words field) field)))
         (parts
           (loop for term in terms for index from 4
                 append (loop for (field . weight) in fields
                   collect (format nil
                     "CASE WHEN search_~a LIKE ?~d THEN ~d ELSE 0 END"
                     field index weight))))
         (score (if parts (format nil "(~{~a~^ + ~})" parts) "0"))
         (eligibility
           (cond ((or (string= policy "all")
                      (string= projection
                               *conversation-episode-graph-storage-projection-name*))
                  "1=1")
                 ((string= policy "inferred")
                  "json_extract(payload_json,'$.evidence_status') IN ('direct','prior-graph','inference')")
                 (t
                  "json_extract(payload_json,'$.evidence_status') IN ('direct','prior-graph')"))))
    (apply #'%cegs-read-rows handle
      (format nil
        "WITH searchable AS (SELECT *,~{~a~^,~} FROM pai_knowledge_graph_nodes WHERE projection_name=?1 AND agent_id=?2 AND persona_id=?3) SELECT projection_name,agent_id,persona_id,node_id,node_kind,canonical_key,payload_json,integrity_hash FROM searchable WHERE ~a AND ~a>0 ORDER BY ~a DESC,node_id LIMIT ~d"
        search-columns eligibility score score (1+ *knowledge-graph-search-maximum-seeds*))
      '("projection_name" "agent_id" "persona_id" "node_id" "node_kind"
        "canonical_key" "payload_json" "integrity_hash")
      nil :knowledge-graph-search projection agent-id persona-id
      (mapcar (lambda (term) (format nil "% ~a %" term)) terms))))

(defun %kgss-sql-words (field)
  "Word-boundary candidate matching over JSON values, excluding JSON syntax."
  (let ((expression (format nil "lower(COALESCE(json_extract(payload_json,'$.~a'),''))" field)))
    (loop for code in '(9 10 13 34 39 40 41 44 45 46 47 58 59 63 91 93 95 123 125)
          do (setf expression (format nil "replace(~a,char(~d),' ')" expression code)))
    (format nil "(' ' || ~a || ' ')" expression)))

(defun %kgss-read-nodes (handle projection agent-id persona-id clause bindings
                         &optional (limit 17))
  (apply #'%cegs-read-rows
         handle
         (format nil
                 "SELECT projection_name,agent_id,persona_id,node_id,node_kind,canonical_key,payload_json,integrity_hash FROM pai_knowledge_graph_nodes WHERE projection_name=?1 AND agent_id=?2 AND persona_id=?3 AND ~a ORDER BY node_id LIMIT ~d"
                 clause limit)
         '("projection_name" "agent_id" "persona_id" "node_id" "node_kind"
           "canonical_key" "payload_json" "integrity_hash")
         nil :knowledge-graph-search
         projection agent-id persona-id bindings))

(defun %kgss-read-edges (handle projection agent-id persona-id node-id direction)
  (let ((clause
          (cond ((string= direction "outgoing") "from_node_id=?4")
                ((string= direction "incoming") "to_node_id=?4")
                (t "(from_node_id=?4 OR to_node_id=?4)"))))
    (%cegs-read-rows
     handle
     (format nil
             "SELECT projection_name,agent_id,persona_id,edge_id,from_node_id,predicate,to_node_id,payload_json,integrity_hash FROM pai_knowledge_graph_edges WHERE projection_name=?1 AND agent_id=?2 AND persona_id=?3 AND ~a ORDER BY edge_id LIMIT ~d"
             clause (1+ *knowledge-graph-search-maximum-neighbors*))
     '("projection_name" "agent_id" "persona_id" "edge_id" "from_node_id"
       "predicate" "to_node_id" "payload_json" "integrity_hash")
     nil :knowledge-graph-search
     projection agent-id persona-id node-id)))

(defun %kgss-read-evidence
    (handle projection agent-id persona-id owner-kind owner-id)
  (%cegs-read-rows
   handle
   (format nil
           "SELECT projection_name,agent_id,persona_id,owner_kind,owner_id,evidence_event_id,evidence_role,evidence_ordinal FROM pai_knowledge_graph_evidence WHERE projection_name=?1 AND agent_id=?2 AND persona_id=?3 AND owner_kind=?4 AND owner_id=?5 ORDER BY evidence_event_id,evidence_role,evidence_ordinal LIMIT ~d"
           (1+ *knowledge-graph-search-maximum-evidence-per-owner*))
   '("projection_name" "agent_id" "persona_id" "owner_kind" "owner_id"
     "evidence_event_id" "evidence_role" "evidence_ordinal")
   '("evidence_event_id" "evidence_ordinal")
   :knowledge-graph-search projection agent-id persona-id owner-kind owner-id))

(defun %kgss-read-node-ids-by-evidence
    (handle projection agent-id persona-id event-ids)
  (if (null event-ids)
      #()
      (apply #'%cegs-read-rows
             handle
             (format nil
                     "SELECT DISTINCT owner_id FROM pai_knowledge_graph_evidence WHERE projection_name=?1 AND agent_id=?2 AND persona_id=?3 AND owner_kind='node' AND CAST(evidence_event_id AS TEXT) IN (~{?~d~^,~}) ORDER BY owner_id LIMIT ~d"
                     (loop for index from 4 below (+ 4 (length event-ids))
                           collect index)
                     (1+ *knowledge-graph-search-maximum-seeds*))
             '("owner_id") nil :knowledge-graph-search
             projection agent-id persona-id
             (mapcar #'princ-to-string event-ids))))

(defun %kgss-node (row projection agent-id persona-id)
  (unless (and (hash-table-p row)
               (string= projection (gethash "projection_name" row ""))
               (string= agent-id (gethash "agent_id" row ""))
               (string= persona-id (gethash "persona_id" row "")))
    (%kgfs-fail "Graph-search node partition is invalid"))
  (let* ((id (gethash "node_id" row))
         (kind (gethash "node_kind" row))
         (key (gethash "canonical_key" row))
         (json (gethash "payload_json" row))
         (payload (%storage-json-read json :knowledge-graph-search)))
    (cond
      ((string= projection *conversation-episode-graph-storage-projection-name*)
       (unless (and (member kind '("episode" "concept") :test #'string=)
                    (string= id (%conversation-episode-graph-id kind key))
                    (string= (gethash "integrity_hash" row "")
                             (%conversation-episode-graph-integrity
                              projection agent-id persona-id id kind key json))
                    (hash-table-p payload))
         (%kgfs-fail "KG1 graph-search node integrity is invalid"))
       (obj "projection_name" projection "node_id" id "node_kind" kind
            "label" (if (string= kind "concept")
                         (gethash "label" payload "")
                         (format nil "Conversation episode ~a"
                                 (gethash "episode_id" payload key)))
            "summary" (if (string= kind "episode")
                           (gethash "synopsis" payload "") "")
            "aliases" #() "status" "current" "disclosure_class" "private"
            "retrieval_context"
            (format nil "~{~a~^ ~}" (append
               (%kgs-items (gethash "retrieval_cues" payload))
               (%kgs-items (gethash "broader_categories" payload))))
            "evidence_status" "direct"
            "evidence_note" "deterministic sealed-episode projection"
            "source_episode_ids"
            (if (string= kind "episode")
                (vector (gethash "episode_id" payload)) #())))
      ((string= projection *knowledge-graph-formation-projection-name*)
       (unless (and (string= key id)
                    (uiop:string-prefix-p "kgf:entity:" id)
                    (string= (gethash "integrity_hash" row "")
                             (%kgf-integrity projection agent-id persona-id
                                             id kind key json))
                    (hash-table-p payload)
                    (string= id (gethash "node_id" payload ""))
                    (string= "current" (gethash "status" payload "")))
         (%kgfs-fail "KG2 graph-search node integrity/status is invalid"))
       (obj "projection_name" projection "node_id" id "node_kind" kind
            "label" (gethash "label" payload "")
            "aliases" (copy-seq (gethash "aliases" payload #()))
            "classifications"
            (copy-seq (gethash "classifications" payload #()))
            "status" (gethash "status" payload)
            "evidence_status" (gethash "evidence_status" payload "unreviewed")
            "evidence_note" (gethash "evidence_note" payload
                                     "legacy formation was not evidence-reviewed")
            "disclosure_class" (gethash "disclosure_class" payload)
            "source_episode_ids"
            (copy-seq (gethash "source_episode_ids" payload #()))
            "source_memory_node_ids"
            (copy-seq (gethash "source_memory_node_ids" payload #()))))
      (t (%kgfs-fail "Graph-search projection is unauthorized")))))

(defun %kgss-bound-seeds (seeds requested-start &optional query)
  "Deduplicate and fairly bound seeds across independently verified graphs."
  (let* ((unique
           (remove-duplicates
            seeds :test #'string=
            :key (lambda (row)
                   (format nil "~a|~a"
                           (gethash "projection_name" row)
                           (gethash "node_id" row)))))
         (terms (%kgs-query-terms query))
         (ordered
           (stable-sort unique
             (lambda (left right)
               (let ((ls (%kgs-node-relevance left terms))
                     (rs (%kgs-node-relevance right terms)))
                 (cond
                   ((and (stringp requested-start)
                         (string= requested-start (gethash "node_id" left))
                         (not (string= requested-start (gethash "node_id" right)))) t)
                   ((and (stringp requested-start)
                         (string= requested-start (gethash "node_id" right))) nil)
                   ((/= ls rs) (> ls rs))
                   (t (string< (gethash "node_id" left) (gethash "node_id" right))))))))
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
         (selected nil)
         (progress t))
    (loop while (and progress
                     (< (length selected)
                        *knowledge-graph-search-maximum-seeds*))
          do (setf progress nil)
             (dolist (bucket buckets)
               (when (and (cdr bucket)
                          (< (length selected)
                             *knowledge-graph-search-maximum-seeds*))
                 (setf progress t)
                 (push (pop (cdr bucket)) selected))))
    (values (nreverse selected)
            (> (length unique) *knowledge-graph-search-maximum-seeds*))))

(defun %kgss-edge (row projection agent-id persona-id)
  (let* ((id (gethash "edge_id" row))
         (from (gethash "from_node_id" row))
         (to (gethash "to_node_id" row))
         (predicate (gethash "predicate" row))
         (json (gethash "payload_json" row))
         (payload (%storage-json-read json :knowledge-graph-search)))
    (unless (and (string= projection (gethash "projection_name" row ""))
                 (string= agent-id (gethash "agent_id" row ""))
                 (string= persona-id (gethash "persona_id" row ""))
                 (hash-table-p payload))
      (%kgfs-fail "Graph-search edge partition is invalid"))
    (cond
      ((string= projection *conversation-episode-graph-storage-projection-name*)
       (unless (and (string= predicate "has-concept")
                    (string= (gethash "integrity_hash" row "")
                             (%conversation-episode-graph-integrity
                              projection agent-id persona-id id from predicate
                              to json)))
         (%kgfs-fail "KG1 graph-search edge integrity is invalid")))
      ((string= projection *knowledge-graph-formation-projection-name*)
       (unless (and (string= (gethash "integrity_hash" row "")
                             (%kgf-integrity projection agent-id persona-id id
                                             from predicate to json))
                    (string= "current" (gethash "status" payload "")))
         (%kgfs-fail "KG2 graph-search edge integrity/status is invalid")))
      (t (%kgfs-fail "Graph-search edge projection is unauthorized")))
    (obj "projection_name" projection "edge_id" id
         "from_node_id" from "predicate" predicate "to_node_id" to
         "traversal_direction" :null
         "status" (gethash "status" payload "current")
         "evidence_status"
         (if (string= projection
                      *conversation-episode-graph-storage-projection-name*)
             "direct"
             (gethash "evidence_status" payload "unreviewed"))
         "evidence_note"
         (if (string= projection
                      *conversation-episode-graph-storage-projection-name*)
             "deterministic sealed-episode projection"
             (gethash "evidence_note" payload
                      "legacy formation was not evidence-reviewed"))
         "origin_agent_ids"
         (%kgs-origin-agent-ids
          (let* ((grounding (gethash "grounding" payload))
                 (evidence (and (hash-table-p grounding)
                                (gethash "evidence" grounding #()))))
            (if (vectorp evidence)
                (map 'vector (lambda (citation)
                               (and (hash-table-p citation)
                                    (gethash "source_id" citation)))
                     evidence)
                #())))
         "valid_from" (gethash "valid_from" payload :null)
         "valid_to" (gethash "valid_to" payload :null))))

(defun %kgss-evidence (rows projection agent-id persona-id owner-kind owner-id)
  (let ((clipped (> (length rows)
                    *knowledge-graph-search-maximum-evidence-per-owner*)))
    (values
     (coerce
      (loop for row across rows
            repeat *knowledge-graph-search-maximum-evidence-per-owner*
            do (unless (and (string= projection
                                     (gethash "projection_name" row ""))
                            (string= agent-id (gethash "agent_id" row ""))
                            (string= persona-id (gethash "persona_id" row ""))
                            (string= owner-kind (gethash "owner_kind" row ""))
                            (string= owner-id (gethash "owner_id" row ""))
                            (integerp (gethash "evidence_event_id" row))
                            (member (gethash "evidence_role" row "")
                                    '("descriptor" "source") :test #'string=))
                 (%kgfs-fail "Graph-search evidence row is invalid"))
            collect (gethash "evidence_event_id" row))
      'vector)
     clipped)))

(defun knowledge-graph-search-storage
    (backend agent-id persona-id request
     &key event-storage-id (linked-source-ids #())
       (linked-evidence-event-ids #()))
  "Search the current grounded knowledge projection without writes or repair.

Sealed conversation episodes remain a separate episodic-retrieval source. The
legacy episode-to-concept graph is deliberately not an ordinary knowledge
surface: unioning it here lets stale generated summary tags crowd out grounded
typed facts and makes graph results grow with duplicate context."
  (unless (and (typep backend 'sqlite-derived-storage)
               (%kgs-present-string-p agent-id 180)
               (%kgs-present-string-p persona-id 120)
               (%kgs-present-string-p event-storage-id 240)
               (vectorp linked-source-ids) (<= (length linked-source-ids) 16)
               (every (lambda (id) (%kgs-present-string-p id 240))
                      linked-source-ids)
               (vectorp linked-evidence-event-ids)
               (<= (length linked-evidence-event-ids) 32)
               (every (lambda (id) (and (integerp id) (plusp id)))
                      linked-evidence-event-ids)
               (knowledge-graph-search-request-valid-p request))
    (error 'storage-error :operation :knowledge-graph-search
           :detail "Graph-search storage inputs are invalid"))
  (let ((projections nil) (unavailable nil) (seeds nil) (query-matches nil)
        (linked-seed-keys (make-hash-table :test #'equal))
        (seed-clipped nil) (watermarks nil)
        (starting-node-status "not-requested"))
    (dolist (projection
             (list *knowledge-graph-formation-projection-name*))
      (handler-case
          (multiple-value-bind (checkpoint state)
              (if (string= projection
                           *conversation-episode-graph-storage-projection-name*)
                  (%cegs-checkpoint-generation backend agent-id persona-id
                                               event-storage-id)
                  (%kgfs-checkpoint backend agent-id persona-id
                                    event-storage-id))
            (declare (ignore state))
            (push projection projections)
            (push (obj "projection_name" projection
                       "through_event_id" (gethash "through_event_id" checkpoint)
                       "through_storage_position"
                       (gethash "through_storage_position" checkpoint))
                  watermarks))
        (error (condition)
          (declare (ignore condition))
          (push projection unavailable))))
    (when (null projections)
      (error 'storage-integrity-error :operation :knowledge-graph-search
             :detail "No verified graph projection is available"))
    (bt:with-lock-held ((%sqlite-derived-lock backend))
      (let ((handle (%sqlite-derived-handle backend :knowledge-graph-search)))
        (dolist (projection (sort projections #'string<))
          (let ((start (gethash "starting_node_id" request))
                (query (gethash "query" request))
                (evidence-policy (gethash "evidence_policy" request)))
            (when (%kgs-present-string-p start 240)
              (let ((rows (%kgss-read-nodes
                           handle projection agent-id persona-id
                           "node_id=?4" (list start) 1)))
                (when (plusp (length rows))
                  (let ((node (%kgss-node (aref rows 0) projection
                                          agent-id persona-id)))
                    (when (%kgss-semantic-evidence-allowed-p
                           node evidence-policy)
                      (push node seeds))))))
            (when (%kgs-present-string-p query 1000)
              (let ((rows (%kgss-read-query-nodes
                           handle projection agent-id persona-id query evidence-policy)))
                (when (> (length rows) *knowledge-graph-search-maximum-seeds*)
                  (setf seed-clipped t))
                (loop for row across rows
                      repeat *knowledge-graph-search-maximum-seeds*
                      for node = (%kgss-node row projection agent-id persona-id)
                      when (%kgss-semantic-evidence-allowed-p
                            node evidence-policy)
                        do (push node seeds)
                           (push (%kgs-copy-object node) query-matches))))
            (when (plusp (length linked-source-ids))
              (let* ((clauses
                       (loop for index from 4
                             below (+ 4 (length linked-source-ids))
                             collect (format nil
                                             "lower(payload_json) LIKE ?~d ESCAPE '\\'"
                                             index)))
                     (rows
                       (%kgss-read-nodes
                        handle projection agent-id persona-id
                        (format nil "(~{~a~^ OR ~})" clauses)
                        (map 'list #'%kgss-like-pattern linked-source-ids)
                        (1+ *knowledge-graph-search-maximum-seeds*))))
                (when (> (length rows) *knowledge-graph-search-maximum-seeds*)
                  (setf seed-clipped t))
                (loop for row across rows
                      repeat *knowledge-graph-search-maximum-seeds*
                      for node = (%kgss-node row projection agent-id persona-id)
                      when (%kgss-semantic-evidence-allowed-p
                            node evidence-policy)
                        do (setf (gethash
                                (format nil "~a|~a" projection
                                        (gethash "node_id" node))
                                linked-seed-keys)
                               t)
                         (push node seeds))))
            (when (plusp (length linked-evidence-event-ids))
              (let ((rows
                      (%kgss-read-node-ids-by-evidence
                       handle projection agent-id persona-id
                       (coerce linked-evidence-event-ids 'list))))
                (when (> (length rows) *knowledge-graph-search-maximum-seeds*)
                  (setf seed-clipped t))
                (loop for row across rows
                      repeat *knowledge-graph-search-maximum-seeds*
                      for node-id = (gethash "owner_id" row)
                      for node-rows = (%kgss-read-nodes
                                       handle projection agent-id persona-id
                                       "node_id=?4" (list node-id) 1)
                      when (plusp (length node-rows))
                        do (let ((node (%kgss-node
                                        (aref node-rows 0) projection
                                        agent-id persona-id)))
                             (when (%kgss-semantic-evidence-allowed-p
                                    node evidence-policy)
                               (setf (gethash
                                      (format nil "~a|~a" projection node-id)
                                      linked-seed-keys)
                                     t)
                               (push node seeds))))))))
        (let* ((requested-start (gethash "starting_node_id" request))
               (start-requested-p (%kgs-present-string-p requested-start 240))
               (query-present-p
                 (%kgs-present-string-p (gethash "query" request) 1000))
               (start-found-p
                 (and start-requested-p
                      (find requested-start seeds :test #'string=
                            :key (lambda (row)
                                   (gethash "node_id" row ""))))))
          (setf starting-node-status
                (cond (start-found-p "resolved")
                      ((and start-requested-p query-present-p)
                       "unresolved-query-fallback")
                      (start-requested-p "unresolved")
                      (t "not-requested")))
          ;; Exact-ID-only lookup remains exact. A stale or guessed ID cannot
          ;; silently redirect traversal to hybrid candidates. With a valid
          ;; lexical query, however, the unusable hint is non-authoritative
          ;; and query-resolved seeds remain eligible.
          (when (and start-requested-p (not start-found-p)
                     (not query-present-p))
            (setf seeds nil)))
        (multiple-value-bind (bounded clipped)
            (%kgss-bound-seeds seeds (gethash "starting_node_id" request)
                               (gethash "query" request))
          (setf seeds bounded
                seed-clipped (or seed-clipped clipped)))
        (let ((result
                (knowledge-graph-search-traverse
                 request (coerce seeds 'vector)
                 (lambda (node closed-request)
                   (let* ((projection (gethash "projection_name" node))
                          (node-id (gethash "node_id" node))
                          (direction (gethash "direction" closed-request))
                          (predicates (coerce (gethash "predicates" closed-request)
                                              'list))
                          (rows (%kgss-read-edges handle projection agent-id
                                                  persona-id node-id direction))
                          (clipped (> (length rows)
                                      *knowledge-graph-search-maximum-neighbors*))
                          (neighbors nil))
                     (loop for row across rows
                           while (< (length neighbors)
                                    *knowledge-graph-search-maximum-neighbors*)
                           for edge = (%kgss-edge row projection agent-id persona-id)
                           for from = (gethash "from_node_id" edge)
                           for to = (gethash "to_node_id" edge)
                           for outgoing = (string= node-id from)
                           for adjacent = (if outgoing to from)
                           when (and (%kgss-semantic-evidence-allowed-p
                                      edge (gethash "evidence_policy"
                                                    closed-request))
                                     (or (null predicates)
                                         (member (gethash "predicate" edge)
                                                 predicates :test #'string=))
                                     (or (string= direction "both")
                                         (and outgoing
                                              (string= direction "outgoing"))
                                         (and (not outgoing)
                                              (string= direction "incoming"))))
                             do (let ((target-rows
                                       (%kgss-read-nodes
                                        handle projection agent-id persona-id
                                        "node_id=?4" (list adjacent) 1)))
                                  (when (plusp (length target-rows))
                                    (let ((target
                                            (%kgss-node
                                             (aref target-rows 0) projection
                                             agent-id persona-id)))
                                      (when (%kgss-semantic-evidence-allowed-p
                                             target
                                             (gethash "evidence_policy"
                                                      closed-request))
                                        (multiple-value-bind (node-evidence node-cut)
                                          (%kgss-evidence
                                           (%kgss-read-evidence
                                            handle projection agent-id persona-id
                                            "node" adjacent)
                                           projection agent-id persona-id
                                           "node" adjacent)
                                        (multiple-value-bind (edge-evidence edge-cut)
                                            (%kgss-evidence
                                             (%kgss-read-evidence
                                              handle projection agent-id persona-id
                                              "edge" (gethash "edge_id" edge))
                                             projection agent-id persona-id
                                             "edge" (gethash "edge_id" edge))
                                          (setf (gethash "evidence_event_ids" target)
                                                node-evidence
                                                (gethash "evidence_event_ids" edge)
                                                edge-evidence
                                                (gethash "traversal_direction" edge)
                                                (if outgoing "outgoing" "incoming"))
                                          (when (or node-cut edge-cut)
                                            (setf clipped t))
                                          (push (obj "node" target "edge" edge)
                                                neighbors))))))))
                     (values (coerce (nreverse neighbors) 'vector) clipped)))
                 :non-exhaustive-p (or seed-clipped unavailable))))
          ;; Seed evidence is attached after traversal so all paths share the
          ;; exact same independently checked descriptor.
          (loop for path across (gethash "paths" result)
                for first = (aref (gethash "nodes" path) 0)
                for seed-key = (format nil "~a|~a"
                                       (gethash "projection_name" first)
                                       (gethash "node_id" first))
                do (setf (gethash "hybrid_seed" path)
                         (if (gethash seed-key linked-seed-keys) t nil))
                unless (gethash "evidence_event_ids" first)
                  do (multiple-value-bind (ids clipped)
                         (%kgss-evidence
                          (%kgss-read-evidence
                           handle (gethash "projection_name" first)
                           agent-id persona-id "node" (gethash "node_id" first))
                          (gethash "projection_name" first) agent-id persona-id
                          "node" (gethash "node_id" first))
                       (setf (gethash "evidence_event_ids" first) ids)
                       (when clipped
                         (setf (gethash "non_exhaustive" result) t))))
          (setf (gethash "searched_projections" result)
                (coerce (sort (copy-list projections) #'string<) 'vector)
                (gethash "unavailable_projections" result)
                (coerce (sort unavailable #'string<) 'vector)
                (gethash "generation_watermarks" result)
                (coerce (sort watermarks #'string<
                              :key (lambda (row)
                                     (gethash "projection_name" row)))
                        'vector)
                (gethash "status" result)
                (if (plusp (gethash "path_count" result)) "available" "empty")
                (gethash "linked_source_ids" result)
                (copy-seq linked-source-ids)
                (gethash "linked_evidence_event_ids" result)
                (copy-seq linked-evidence-event-ids)
                (gethash "hybrid_link_seed_count" result)
                (hash-table-count linked-seed-keys)
                (gethash "query_match_nodes" result)
                (coerce (nreverse query-matches) 'vector)
                (gethash "query_match_count" result)
                (length query-matches)
                (gethash "starting_node_status" result)
                starting-node-status
                (gethash "unresolved_starting_node_id" result)
                (if (member starting-node-status
                            '("unresolved" "unresolved-query-fallback")
                            :test #'string=)
                    (gethash "starting_node_id" request)
                    :null))
          result)))))
