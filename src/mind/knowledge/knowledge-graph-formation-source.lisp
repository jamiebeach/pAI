;;;; knowledge-graph-formation-source.lisp -- deterministic KG2 episode source.
;;;;
;;;; Selection is pure apart from an injected bounded candidate lookup.  A
;;;; sealed episode receipt is itself the exact evidence root; its payload
;;;; retains the complete raw dialogue lineage without copying an unbounded
;;;; turn list into the provider request.

(in-package :agent)

(export '(knowledge-graph-formation-grounded-memory-evidence
          knowledge-graph-formation-discover-supplemental-memory-candidates
          knowledge-graph-formation-select-supplemental-memories
          knowledge-graph-formation-select-episode-source))

(defparameter *knowledge-graph-formation-attempt-revision*
  "recursive-grounded-knowledge-graph-v6")
(defparameter *knowledge-graph-formation-maximum-attempts-per-episode* 3)
(defparameter *knowledge-graph-formation-maximum-supplemental-memories* 8)
(defparameter *knowledge-graph-formation-maximum-supplemental-candidates* 64)
(defparameter *knowledge-graph-formation-maximum-discovered-memories* 50)
(defparameter *knowledge-graph-formation-supplemental-relevance-floor-milli* 650)

(defun %kgfs-event-payload (event)
  (and (hash-table-p event) (gethash "payload" event)))

(defun %kgfs-event-in-partition-p (event agent-id persona-id)
  (let ((payload (%kgfs-event-payload event)))
    (and (hash-table-p payload)
         (string= agent-id (gethash "agent_id" event ""))
         (string= persona-id (gethash "persona_id" payload "")))))

(defun %kgfs-covered-episode-ids (events agent-id persona-id)
  (let ((covered (make-hash-table :test #'equal))
        (terminals (make-hash-table :test #'eql))
        (opens (make-hash-table :test #'eql))
        (current-attempts (make-hash-table :test #'eql))
        (failed-attempts (make-hash-table :test #'equal)))
    (dolist (event events)
      (when (and (string= "knowledge-graph-formation-opened"
                          (gethash "type" event ""))
                 (%kgfs-event-in-partition-p event agent-id persona-id)
                 (integerp (gethash "id" event)))
        (setf (gethash (gethash "id" event) opens) event))
      (when (and (member (gethash "type" event "")
                         '("knowledge-graph-formation-sealed"
                           "knowledge-graph-formation-failed")
                         :test #'string=)
                 (integerp (gethash "caused_by" event)))
        (setf (gethash (gethash "caused_by" event) terminals) event))
      (let ((payload (%kgfs-event-payload event)))
        (when (and (string= "model-request" (gethash "type" event ""))
                   (integerp (gethash "caused_by" event))
                   (hash-table-p payload)
                   (gethash "knowledge_graph_formation" payload)
                   (string= *knowledge-graph-formation-attempt-revision*
                            (gethash "protocol_revision" payload "")))
          (setf (gethash (gethash "caused_by" event) current-attempts) t))))
    ;; A transient provider/protocol failure may be retried, but one bad
    ;; episode cannot churn forever. Count terminal attempts by episode.
    (maphash
     (lambda (opened-id opened)
       (let ((terminal (gethash opened-id terminals)))
         (when (and terminal (gethash opened-id current-attempts)
                    (string= "knowledge-graph-formation-failed"
                             (gethash "type" terminal "")))
           (dolist (episode-id
                    (%kgf-items
                     (gethash "source_episode_ids"
                              (%kgfs-event-payload opened))))
             (incf (gethash episode-id failed-attempts 0))))))
     opens)
    (dolist (event events covered)
      (when (and (string= "knowledge-graph-formation-opened"
                          (gethash "type" event ""))
                 (%kgfs-event-in-partition-p event agent-id persona-id))
        (let* ((payload (%kgfs-event-payload event))
               (terminal (gethash (gethash "id" event) terminals))
               (terminal-payload (%kgfs-event-payload terminal))
               (terminal-type (and terminal (gethash "type" terminal "")))
               (proposal (and (hash-table-p terminal-payload)
                              (gethash "proposal" terminal-payload)))
               ;; A successful v1 formation is deliberately reconsidered once
               ;; by the evidence-reviewed v2 protocol. Pending work and
               ;; failures remain covered, preserving the no-churn rule.
               (covered-p
                 (or (null terminal)
                     (and (string= terminal-type
                                   "knowledge-graph-formation-failed")
                          (gethash (gethash "id" event) current-attempts)
                          (some
                           (lambda (episode-id)
                             (>= (gethash episode-id failed-attempts 0)
                                 *knowledge-graph-formation-maximum-attempts-per-episode*))
                           (%kgf-items
                            (gethash "source_episode_ids" payload))))
                     (and (string= terminal-type
                                   "knowledge-graph-formation-sealed")
                          (hash-table-p terminal-payload)
                          (string= *knowledge-graph-formation-revision*
                                   (gethash "formation_revision"
                                            terminal-payload ""))
                          (hash-table-p proposal)
                          (= 3 (gethash "schema_version" proposal -1))))))
          (when covered-p
            (dolist (episode-id
                     (%kgf-items (gethash "source_episode_ids" payload)))
              (when (stringp episode-id)
                (setf (gethash episode-id covered) t)))))))))

(defun %kgfs-source-evidence (events payload persona-id)
  "Hydrate exact public utterances; generated episode summaries are navigation only."
  (let ((by-id (make-hash-table :test #'eql)) (rows nil))
    (dolist (event events)
      (when (integerp (gethash "id" event))
        (setf (gethash (gethash "id" event) by-id) event)))
    (loop for event-id across (gethash "source_event_ids" payload)
          for event = (gethash event-id by-id)
          for body = (and event (gethash "payload" event))
          for metadata = (and (hash-table-p body) (gethash "metadata" body))
          ;; Existing sealed receipts may point at pre-metadata public rows.
          ;; Preserve that hydration contract.  Only the new historical
          ;; receipt types require the closed migration provenance check.
          for role = (and event
                          (or (%conversation-episode-historical-role
                               event (gethash "agent_id" event "") persona-id)
                              (let ((type (gethash "type" event "")))
                                (cond ((string= type "user-message") "operator")
                                      ((string= type "agent-message") "assistant")))))
          for text = (and (hash-table-p body) (gethash "text" body))
          for migrated-p = (string= "historical-agent-migration-v1"
                                    (if (hash-table-p metadata)
                                        (gethash "source" metadata "") ""))
          for source-id = (if migrated-p
                              (format nil "migrated-event:~a:~d"
                                      (gethash "source_agent_id" metadata)
                                      (gethash "source_event_id" metadata))
                              (format nil "event:~d" event-id))
          when (and role (%kgf-required-string-p text 30000))
            do (push
                (obj "source_id" source-id
                     "speaker_id" (cond ((string= role "operator") "operator")
                                        ((string= role "assistant") persona-id)
                                        (t (format nil "source-agent:~a"
                                                   (gethash "source_agent_id"
                                                            metadata))))
                     "kind" (if (string= role "operator")
                                "original-utterance" "prior-agent-utterance")
                     "timestamp" (gethash "timestamp" event)
                     "text" text
                     "text_sha256" (%kgf-sha256 text))
                rows))
    (let ((result (coerce (nreverse rows) 'vector)))
      (unless (plusp (length result))
        (error "KG2 sealed episode has no hydratable original utterance"))
      result)))

(defun knowledge-graph-formation-grounded-memory-evidence
    (event agent-id persona-id)
  "Return exact evidence and its memory ID for one authoritative baseline row.

This validates provenance only.  It does not decide relevance, search memory,
or attach a memory to an episode; an injected source policy owns that choice."
  (unless (and (hash-table-p event)
               (string= "memory-baseline-node" (gethash "type" event ""))
               (string= agent-id (gethash "agent_id" event ""))
               (integerp (gethash "id" event)))
    (error "KG2 supplemental memory event is invalid or foreign"))
  (let* ((payload (%kgfs-event-payload event))
         (envelope (and (hash-table-p payload) (gethash "node" payload)))
         (source-origin (and (hash-table-p payload)
                             (gethash "source_origin" payload)))
         (source-agent-id
           (and (hash-table-p source-origin)
                (string= "migrated-semantic-memory"
                         (gethash "source_kind" source-origin ""))
                (gethash "source_agent_id" source-origin)))
         (encoded (and (hash-table-p envelope)
                       (gethash "scalar_json" envelope)))
         (memory
           (and (stringp encoded)
                (handler-case (shasht:read-json encoded)
                  (error () nil))))
         (metadata (and (hash-table-p memory)
                        (gethash "epistemic_metadata" memory)))
         (role (and (hash-table-p metadata) (gethash "role" metadata)))
         (content (and (hash-table-p memory) (gethash "content" memory)))
         (memory-id (and (hash-table-p memory) (gethash "id" memory))))
    (unless (and (hash-table-p memory)
                 (string= "grounded" (gethash "grounding_status" memory ""))
                 (member (gethash "origin_class" memory "")
                         '("lived-user" "lived-agent-action") :test #'string=)
                 (member role '("user" "assistant") :test #'string=)
                 (%kgf-required-string-p content 30000)
                 (%kgf-required-string-p memory-id 180))
      (error "KG2 supplemental memory lacks grounded lived evidence"))
    (values
     (obj "source_id" (if source-agent-id
                          (format nil "migrated-memory:~a:~d"
                                  source-agent-id (gethash "id" event))
                          (format nil "memory-event:~d" (gethash "id" event)))
          "speaker_id" (cond ((string= role "user") "operator")
                             (source-agent-id
                              (format nil "source-agent:~a" source-agent-id))
                             (t persona-id))
          "kind" (if (string= role "user")
                     "original-utterance" "prior-agent-utterance")
          "timestamp" (gethash "created_at" memory)
          "text" content "text_sha256" (%kgf-sha256 content))
     memory-id)))

(defun %kgfs-baseline-memory-id (event)
  "Read only the projected memory ID needed to join retrieval to its event."
  (let* ((payload (%kgfs-event-payload event))
         (envelope (and (hash-table-p payload) (gethash "node" payload)))
         (encoded (and (hash-table-p envelope)
                       (gethash "scalar_json" envelope)))
         (memory (and (stringp encoded)
                      (handler-case (shasht:read-json encoded)
                        (error () nil)))))
    (and (hash-table-p memory) (gethash "id" memory))))

(defun %kgfs-discovery-query (episode-event events)
  (let* ((payload (%kgfs-event-payload episode-event))
         (by-id (make-hash-table :test #'eql))
         (parts nil))
    (dolist (event events)
      (when (integerp (gethash "id" event))
        (setf (gethash (gethash "id" event) by-id) event)))
    (labels ((add (value)
               (when (%kgf-required-string-p value 30000)
                 (push value parts)))
             (add-items (value)
               (dolist (item (%kgf-items value)) (add item))))
      ;; Exact dialogue and episode-owned navigation cues are query evidence.
      ;; IDs and fixture-specific vocabulary never enter this policy.
      (loop for source-id across (gethash "source_event_ids" payload)
            for event = (gethash source-id by-id)
            for body = (%kgfs-event-payload event)
            do (when (hash-table-p body) (add (gethash "text" body))))
      (add (gethash "synopsis" payload))
      (dolist (key '("subjects" "entities" "retrieval_cues"
                     "broader_categories" "unresolved_threads"))
        (add-items (gethash key payload))))
    (let ((query (format nil "~{~a~^ ~}" (nreverse parts))))
      (subseq query 0 (min 4000 (length query))))))

(defun %kgfs-score-milli (value)
  (if (realp value)
      (max 0 (min 1000 (round (* 1000 value))))
      0))

(defun %kgfs-lexical-score-milli (row)
  (let ((sources (%kgf-items (gethash "candidate_sources" row))))
    (if (not (member "lexical" sources :test #'string=))
        0
        (case (gethash "lexical_tier" row 0)
          ;; Phrase matches are stronger, but a single broad token does not
          ;; reach the admission floor merely because it appeared at all.
          (2 (max 800 (%kgfs-score-milli
                       (gethash "lexical_coverage" row 0))))
          (1 (%kgfs-score-milli (gethash "lexical_coverage" row 0)))
          (otherwise 0)))))

(defun knowledge-graph-formation-discover-supplemental-memory-candidates
    (episode-event events search-fn
     &key (maximum *knowledge-graph-formation-maximum-discovered-memories*))
  "Discover bounded supplemental candidates through an injected read-only search.

SEARCH-FN has the MEMORY-SEARCH calling convention and must return its rows and
receipt.  Discovery joins row IDs to baseline events and translates retrieval
signals only; it does not grant provenance authority.  The independent grounded
memory validator still owns admission after relevance selection."
  (unless (and (hash-table-p episode-event)
               (string= "conversation-episode-sealed"
                        (gethash "type" episode-event ""))
               (conversation-episode-sealed-payload-valid-p
                (%kgfs-event-payload episode-event))
               (listp events) (functionp search-fn)
               (integerp maximum) (<= 1 maximum 50))
    (error "KG2 supplemental discovery inputs are invalid"))
  (let ((query (%kgfs-discovery-query episode-event events))
        (event-by-memory-id (make-hash-table :test #'equal)))
    (unless (plusp (length query))
      (error "KG2 supplemental discovery query is empty"))
    (dolist (event events)
      (when (string= "memory-baseline-node" (gethash "type" event ""))
        (let ((memory-id (%kgfs-baseline-memory-id event)))
          (when (and (%kgf-required-string-p memory-id 180)
                     (null (gethash memory-id event-by-memory-id)))
            (setf (gethash memory-id event-by-memory-id) event)))))
    (multiple-value-bind (rows search-report)
        (funcall search-fn query :k maximum :mode :conversation
                 :candidate-strategy :hybrid-explicit :require-grounded t)
      (let ((row-count (and (or (listp rows) (vectorp rows)) (length rows)))
            (union-count (and (hash-table-p search-report)
                              (gethash "union_candidate_count" search-report)))
            (returned-count (and (hash-table-p search-report)
                                 (gethash "returned_count" search-report))))
        (unless (and row-count (hash-table-p search-report)
                     (integerp union-count) (integerp returned-count)
                     (<= 0 returned-count union-count)
                     (= row-count returned-count)
                     (eql 0 (gethash "database_write_count" search-report)))
          (error "KG2 supplemental discovery requires a complete zero-write search receipt")))
      (let ((candidates nil) (unmapped 0))
        (dolist (row (%kgf-items rows))
          (let* ((memory-id (and (hash-table-p row) (gethash "id" row)))
                 (event (and (stringp memory-id)
                             (gethash memory-id event-by-memory-id))))
            (if (null event)
                (incf unmapped)
                (push
                 (obj "event" event "event_id" (gethash "id" event)
                      "memory_node_id" memory-id
                      "lexical_score_milli" (%kgfs-lexical-score-milli row)
                      "semantic_score_milli"
                      (%kgfs-score-milli (gethash "similarity" row))
                      "episodic_score_milli" 0 "graph_score_milli" 0)
                 candidates))))
        (setf candidates (nreverse candidates))
        (let* ((union-count (gethash "union_candidate_count" search-report 0))
               (returned-count (gethash "returned_count" search-report 0))
               (status (if (or (> union-count returned-count) (plusp unmapped))
                           "incomplete" "complete")))
          (values
           (coerce candidates 'vector)
           (obj "schema_version" 1 "status" status "query" query
                "mapped_candidate_count" (length candidates)
                "unmapped_candidate_count" unmapped
                "search" search-report)))))))

(defun %kgfs-supplemental-candidate-valid-p (candidate)
  (and
   (hash-table-p candidate)
   (= 7 (hash-table-count candidate))
   (every (lambda (key) (nth-value 1 (gethash key candidate)))
          '("event" "event_id" "memory_node_id" "lexical_score_milli"
            "semantic_score_milli" "episodic_score_milli"
            "graph_score_milli"))
   (hash-table-p (gethash "event" candidate))
   (integerp (gethash "event_id" candidate))
   (plusp (gethash "event_id" candidate))
   (eql (gethash "event_id" candidate)
        (gethash "id" (gethash "event" candidate)))
   (%kgf-required-string-p (gethash "memory_node_id" candidate) 180)
   (every (lambda (key)
            (let ((score (gethash key candidate)))
              (and (integerp score) (<= 0 score 1000))))
          '("lexical_score_milli" "semantic_score_milli"
            "episodic_score_milli" "graph_score_milli"))))

(defun %kgfs-supplemental-relevance-score (candidate)
  "Combine independent discovery signals without making any one retriever authority."
  (let* ((scores
           (mapcar (lambda (key) (gethash key candidate))
                   '("lexical_score_milli" "semantic_score_milli"
                     "episodic_score_milli" "graph_score_milli")))
         (strongest (apply #'max scores))
         (corroboration (- (reduce #'+ scores) strongest)))
    ;; A high-confidence vocabulary-gap semantic result can qualify alone;
    ;; weaker independent signals can corroborate it but never add more than
    ;; one quarter of their total weight.
    (min 1000 (+ strongest (floor corroboration 4)))))

(defun %kgfs-supplemental-row-before-p (left right)
  (let ((left-score (gethash "relevance_score_milli" left))
        (right-score (gethash "relevance_score_milli" right)))
    (or (> left-score right-score)
        (and (= left-score right-score)
             (or (< (gethash "event_id" left) (gethash "event_id" right))
                 (and (= (gethash "event_id" left) (gethash "event_id" right))
                      (string< (gethash "memory_node_id" left)
                               (gethash "memory_node_id" right))))))))

(defun knowledge-graph-formation-select-supplemental-memories
    (episode-event candidates
     &key (candidate-status "complete")
       (maximum *knowledge-graph-formation-maximum-supplemental-memories*))
  "Select relevant already-authorized memory candidates with a bounded receipt.

Discovery owns CANDIDATES and their lexical, semantic, episodic and graph
milli-scores.  This function owns deterministic combination, thresholding,
ordering and bounds.  It deliberately does not validate memory provenance;
KNOWLEDGE-GRAPH-FORMATION-GROUNDED-MEMORY-EVIDENCE remains the independent
authority gate after selection."
  (unless (and (hash-table-p episode-event)
               (string= "conversation-episode-sealed"
                        (gethash "type" episode-event ""))
               (vectorp candidates)
               (<= (length candidates)
                   *knowledge-graph-formation-maximum-supplemental-candidates*)
               (member candidate-status
                       '("complete" "incomplete" "unavailable") :test #'string=)
               (integerp maximum)
               (<= 0 maximum
                   *knowledge-graph-formation-maximum-supplemental-memories*)
               (every #'%kgfs-supplemental-candidate-valid-p candidates))
    (error "KG2 supplemental relevance selector inputs are invalid"))
  (let ((seen-events (make-hash-table :test #'eql))
        (seen-memories (make-hash-table :test #'equal))
        (ranked nil))
    (loop for candidate across candidates
          for event-id = (gethash "event_id" candidate)
          for memory-id = (gethash "memory_node_id" candidate)
          do (when (or (gethash event-id seen-events)
                       (gethash memory-id seen-memories))
               (error "KG2 supplemental relevance candidates contain duplicates"))
             (setf (gethash event-id seen-events) t
                   (gethash memory-id seen-memories) t)
             (let ((score (%kgfs-supplemental-relevance-score candidate)))
               (when (>= score
                         *knowledge-graph-formation-supplemental-relevance-floor-milli*)
                 (push
                  (obj "event" (gethash "event" candidate)
                       "event_id" event-id
                       "memory_node_id" memory-id
                       "signal_scores_milli"
                       (obj "lexical" (gethash "lexical_score_milli" candidate)
                            "semantic" (gethash "semantic_score_milli" candidate)
                            "episodic" (gethash "episodic_score_milli" candidate)
                            "graph" (gethash "graph_score_milli" candidate))
                       "relevance_score_milli" score)
                  ranked))))
    (setf ranked (sort ranked #'%kgfs-supplemental-row-before-p))
    (let* ((relevant-count (length ranked))
           (selected (subseq ranked 0 (min maximum relevant-count)))
           (clipped (> relevant-count maximum))
           (complete (and (string= candidate-status "complete") (not clipped)))
           (reasons
             (append (unless (string= candidate-status "complete")
                       (list (format nil "candidate-discovery-~a" candidate-status)))
                     (when clipped (list "selection-bound-clipped")))))
      (obj "schema_version" 1
           "status" (if complete "complete" "incomplete")
           "candidate_status" candidate-status
           "candidate_count" (length candidates)
           "relevant_candidate_count" relevant-count
           "selected_count" (length selected)
           "maximum" maximum
           "relevance_floor_milli"
           *knowledge-graph-formation-supplemental-relevance-floor-milli*
           "incompleteness_reasons" (coerce reasons 'vector)
           "selected" (coerce selected 'vector)))))

(defun knowledge-graph-formation-select-episode-source
    (events agent-id persona-id candidate-selector-fn
     &key supplemental-memory-selector-fn)
  "Select the oldest uncovered sealed episode as one bounded KG2 packet.

CANDIDATE-SELECTOR-FN receives the authoritative sealed episode event and must
return a vector of verified current-node descriptors. NIL means no candidates,
which is valid for the first formation generation."
  (unless (and (listp events) (%kgf-required-string-p agent-id 180)
               (%kgf-required-string-p persona-id 120)
               (functionp candidate-selector-fn))
    (error "KG2 episode source selector inputs are invalid"))
  (let* ((covered (%kgfs-covered-episode-ids events agent-id persona-id))
         (eligible
           (remove-if-not
            (lambda (event)
              (let ((payload (%kgfs-event-payload event)))
                (and (string= "conversation-episode-sealed"
                              (gethash "type" event ""))
                     (%kgfs-event-in-partition-p event agent-id persona-id)
                     (conversation-episode-sealed-payload-valid-p payload)
                     (not (gethash (gethash "episode_id" payload) covered)))))
            events))
         (selected
           (first (sort (copy-list eligible) #'<
                        :key (lambda (event) (gethash "id" event 0))))))
    (when selected
      (let* ((payload (%kgfs-event-payload selected))
             (episode-id (gethash "episode_id" payload))
             (evidence (%kgfs-source-evidence events payload persona-id))
             (supplemental
               (if supplemental-memory-selector-fn
                   (funcall supplemental-memory-selector-fn selected events)
                   #()))
             (supplemental-events
               (cond ((null supplemental) #())
                     ((vectorp supplemental) supplemental)
                     ((listp supplemental) (coerce supplemental 'vector))
                     (t (error "KG2 supplemental memory selection is invalid"))))
             (supplemental-evidence nil)
             (supplemental-event-ids nil)
             (supplemental-memory-ids nil)
             (candidates (funcall candidate-selector-fn selected))
             (packet nil))
        (when (> (length supplemental-events)
                 *knowledge-graph-formation-maximum-supplemental-memories*)
          (error "KG2 supplemental memory selection exceeds its bound"))
        (loop for event across supplemental-events
              do (multiple-value-bind (row memory-id)
                     (knowledge-graph-formation-grounded-memory-evidence
                      event agent-id persona-id)
                   (push row supplemental-evidence)
                   (push (gethash "id" event) supplemental-event-ids)
                   (push memory-id supplemental-memory-ids)))
        (unless (= (length supplemental-event-ids)
                   (length (remove-duplicates supplemental-event-ids
                                              :test #'eql)))
          (error "KG2 supplemental memory selection contains duplicates"))
        (setf packet
              (obj "schema_version" 1
                    ;; Keep both the sealed episode root and its exact raw
                    ;; utterances in the graph provenance chain.
                    "source_event_ids"
                    (coerce (append (list (gethash "id" selected))
                                    (coerce (gethash "source_event_ids" payload)
                                            'list)
                                    (nreverse supplemental-event-ids))
                            'vector)
                    "source_memory_node_ids"
                    (coerce (nreverse supplemental-memory-ids) 'vector)
                    "source_episode_ids" (vector episode-id)
                    "disclosure_class" "private"
                    "evidence_records"
                    (concatenate 'vector evidence
                                 (coerce (nreverse supplemental-evidence)
                                         'vector))
                    "eligible_existing_nodes" candidates))
        (unless (knowledge-graph-formation-source-packet-valid-p packet)
          (error "KG2 episode source selector produced an invalid packet (~a)"
                 (%kgfo-source-packet-diagnostic packet)))
        packet))))
