;;;; Pure standalone consumer of the shared authority protocol.
;;;; Kept private until the owner/access and new-generation entry points qualify.
(in-package :pai.context-graph)

(defun %cg-authority-revision-view (graph agent-id persona-id)
  (%cg-object "agent_id" agent-id "persona_id" persona-id
              "entity_versions" (context-graph-entity-versions graph)
              "current_entity_versions" (context-graph-current-entity-versions graph)))

(defun %cg-authority-projection-digest (graph agent-id persona-id)
  ;; Canonical full-projection checkpoint, not a bounded production tail hasher.
  ;; A maintained production checkpoint/index adapter is a separate gate.
  (%cg-authority-digest "standalone-authority-projection"
    (vector agent-id persona-id (context-graph-entities graph) (context-graph-entity-versions graph)
            (context-graph-current-entity-versions graph) (context-graph-facts graph)
            (context-graph-revision-lineage graph) (context-graph-application-receipts graph))))

(defun %cg-authority-watermark (graph agent-id persona-id)
  (when (and (context-graph-authority-partition graph)
             (not (%cg-authority-equal-p (context-graph-authority-partition graph)
                                        (%cg-object "agent_id" agent-id "persona_id" persona-id))))
    (%cg-authority-fail "SOURCE_PARTITION_MISMATCH"))
  (%cg-object "projection_revision" "grounded-knowledge-graph-formation-v7"
              "through_event_id" (context-graph-through-event-id graph)
              "state_digest" (or (context-graph-projection-digest graph)
                                 (%cg-authority-projection-digest graph agent-id persona-id))))

(defun %cg-authority-entity (graph node-id)
  (or (gethash node-id (context-graph-entity-versions graph)) (%cg-authority-fail "CURRENT_VERSION_CORRUPT")))

(defun %cg-authority-current-descriptor (graph entity-id)
  (let ((partition (context-graph-authority-partition graph)))
    (gethash "value" (context-graph-current-entity-view
                       entity-id (%cg-authority-revision-view graph (gethash "agent_id" partition) (gethash "persona_id" partition))))))

(defun %cg-authority-new-entity (graph descriptor context bindings refs)
  (let* ((ref (gethash "local_ref" descriptor))
         (binding (find ref bindings :test #'equal :key (lambda (b) (gethash "local_ref" b))))
         (id (if binding (gethash "entity_id" binding)
                 (concatenate 'string "cge:"
                              (%cg-authority-digest "ordinary-entity"
                                (vector (gethash "agent_id" context) (gethash "persona_id" context)
                                        (gethash "episode_id" context) ref)))))
         (prior (gethash id (context-graph-entities graph))))
    (unless (%cg-type-declared-p (context-graph-ontology graph) (gethash "kind" descriptor))
      (%cg-authority-fail "ENTITY_KIND_INVALID"))
    (if prior
        (unless (and binding
                     (equal (gethash "role" binding) (gethash "participant_role" prior))
                     (every (lambda (key) (%cg-authority-equal-p (gethash key descriptor) (gethash key prior))) '("kind" "label" "aliases")))
          (%cg-authority-fail "ENTITY_ID_CONFLICT"))
        (let ((entity (%cg-object "entity_id" id "node_id" id "kind" (gethash "kind" descriptor)
                                  "label" (gethash "label" descriptor) "aliases" (%cg-detach (gethash "aliases" descriptor))
                                  "classifications" (%cg-detach (gethash "classifications" descriptor))
                                  "participant_role" (if binding (gethash "role" binding) :null) "status" "current"
                                  "agent_id" (gethash "agent_id" context) "persona_id" (gethash "persona_id" context))))
          (setf (gethash "revision_digest" entity) (%cg-revision-descriptor-digest entity)
                (gethash id (context-graph-entities graph)) entity
                (gethash id (context-graph-entity-versions graph)) (%cg-detach entity)
                (gethash id (context-graph-current-entity-versions graph)) id)))
    (setf (gethash ref refs) (gethash "node_id" (gethash id (context-graph-entities graph))))))

(defun %cg-authority-link-entity (graph descriptor context refs)
  (let* ((node-id (gethash "existing_node_id" descriptor))
         (eligible (find node-id (gethash "eligible_entities" context) :test #'equal :key (lambda (e) (gethash "node_id" e))))
         (actual (gethash node-id (context-graph-entity-versions graph))))
    (unless (and eligible actual (%cg-eligible-entity-p actual)
                 (%cg-authority-equal-p eligible actual)
                 (eq :null (gethash "participant_role" actual))
                 (equal node-id (gethash (gethash "entity_id" actual) (context-graph-current-entity-versions graph)))
                 (equal (gethash "kind" descriptor) (gethash "kind" actual)))
      (%cg-authority-fail "LINK_TARGET_INVALID"))
    ;; A link names an identity. It never updates any descriptor field.
    (setf (gethash (gethash "local_ref" descriptor) refs) node-id)))

(defun %cg-authority-opposed-inference (graph relationship resolved)
  "Return the current inference contradicted by a directly evidenced claim.

Opposition is defined by the typed claim identity with polarity flipped.  This keeps
scope, attribution, endpoints and temporal identity exact.  RELATED_TO also
keeps its canonical statement, because that predicate is otherwise too weak to
identify a proposition safely."
  (let* ((grounding (gethash "grounding" relationship))
         (polarity (and (hash-table-p grounding)
                        (gethash "polarity" grounding))))
    (when (and (member *cg-claim-identity-protocol*
                       '("claim-identity-v2" "claim-identity-v3") :test #'equal)
               (equal "direct" (gethash "evidence_status" relationship))
               (equal "assertion" (gethash "scope" grounding))
               (member polarity '("positive" "negative") :test #'equal))
      (let* ((opposite (%cg-detach relationship))
             (opposite-grounding (gethash "grounding" opposite)))
        (setf (gethash "polarity" opposite-grounding)
              (if (equal polarity "positive") "negative" "positive"))
        (let* ((identity (gethash "value"
                                  (context-graph-claim-identity opposite resolved)))
               (fact-id (gethash (gethash "identity_key" identity)
                                 (context-graph-current-triples graph)))
               (fact (and fact-id
                          (gethash fact-id (context-graph-facts graph)))))
          (and fact
               (equal "current" (gethash "status" fact))
               (equal "inference" (gethash "evidence_status" fact))
               fact))))))

(defun %cg-authority-retire-opposed-inference
    (graph opposed replacement-fact-id boundary)
  "Close OPPOSED using the same durable application that installed its denial."
  (when opposed
    (remhash (gethash "identity_key" opposed)
             (context-graph-current-triples graph))
    (setf (gethash "status" opposed) "retired"
          (gethash "retired_at" opposed) (gethash "observed_at" boundary)
          (gethash "retired_by_application_event_id" opposed)
          (gethash "application_event_id" boundary)
          (gethash "replacement_fact_id" opposed) replacement-fact-id
          (gethash "retirement_basis" opposed) "direct-contradiction-v1"
          (gethash "through_event_id" opposed)
          (gethash "application_event_id" boundary)))
  opposed)

(defun %cg-authority-apply-claim (graph relationship context boundary refs)
  (let ((status (gethash "evidence_status" relationship)))
    (cond ((equal status "inference")
           (unless (%cg-authority-inference-evidence-p context relationship)
             (%cg-authority-fail "INFERENCE_PREMISE_INVALID")))
          ((not (%cg-authority-assertion-evidence-p context relationship))
           (%cg-authority-fail "ASSERTION_SOURCE_AUTHORITY_INVALID"))))
  (let* ((subject-id (gethash (gethash "subject_ref" relationship) refs))
         (object-id (gethash (gethash "object_ref" relationship) refs))
         (subject (%cg-authority-entity graph subject-id)) (object (%cg-authority-entity graph object-id))
         (grounding (gethash "grounding" relationship))
         (attributed-ref (gethash "attributed_to_ref" grounding))
         (attributed (unless (eq :null attributed-ref) (%cg-authority-entity graph (gethash attributed-ref refs))))
         (spans (map 'vector (lambda (citation) (or (%cg-citation-exact-span context citation) (%cg-authority-fail "SEALED_SPAN_INVALID")))
                     (gethash "evidence" grounding)))
         (basis (%cg-authority-source-basis spans))
         (resolved (%cg-object "subject_entity_id" (gethash "entity_id" subject) "object_entity_id" (gethash "entity_id" object)
                               "attributed_entity_id" (if attributed (gethash "entity_id" attributed) :null) "source_basis" basis))
          (identity (gethash "value" (context-graph-claim-identity relationship resolved)))
          (key (gethash "identity_key" identity))
          (existing-id (gethash key (context-graph-current-triples graph)))
          (id (or existing-id (concatenate 'string "cgf:" (gethash "identity_sha256" identity))))
          (opposed (%cg-authority-opposed-inference graph relationship resolved))
          (record (%cg-object "source_episode_id" (gethash "episode_id" context)
                              "application_event_id" (gethash "application_event_id" boundary)
                              "status" (gethash "evidence_status" relationship) "note" (gethash "evidence_note" relationship)
                              "accepted_sources" spans))
         (record-digest (%cg-authority-digest "claim-evidence" record)))
    (unless (%cg-signature-valid-p (context-graph-ontology graph) (gethash "predicate" relationship)
                                   (gethash "kind" subject) (gethash "kind" object))
      (%cg-authority-fail "RELATIONSHIP_KIND_INVALID"))
    (let ((fact (or (gethash id (context-graph-facts graph))
                    (%cg-object "fact_id" id "identity_key" key "identity_sha256" (gethash "identity_sha256" identity)
                                "subject_id" subject-id "object_id" object-id "predicate" (gethash "predicate" relationship)
                                "fact" (gethash "fact" relationship) "status" "current"
                                "grounding" (%cg-object "scope" (gethash "scope" grounding) "polarity" (gethash "polarity" grounding)
                                                        "attributed_entity_id" (gethash "attributed_entity_id" resolved) "source_basis" basis)
                                "temporal" (%cg-detach (gethash "temporal" relationship))
                                "observed_at" (gethash "observed_at" boundary) "through_event_id" (gethash "application_event_id" boundary)
                                "evidence_status" "unreviewed" "evidence_records" #() "evidence_digests" #()))))
      (unless (find record-digest (gethash "evidence_digests" fact) :test #'equal)
        (setf (gethash "evidence_records" fact) (concatenate 'vector (gethash "evidence_records" fact) (vector record))
              (gethash "evidence_digests" fact) (concatenate 'vector (gethash "evidence_digests" fact) (vector record-digest))))
      (when (> (%cg-evidence-rank (gethash "status" record)) (%cg-evidence-rank (gethash "evidence_status" fact)))
        (setf (gethash "evidence_status" fact) (gethash "status" record)))
      (let ((source-ids (remove-duplicates
                         (concatenate 'vector (gethash "accepted_source_ids" fact #())
                                      (map 'vector (lambda (span) (gethash "source_id" span)) spans)) :test #'equal)))
        ;; This compact prefix is only an anchor summary. No evidence is dropped.
        (setf (gethash "accepted_source_ids" fact) (subseq source-ids 0 (min 4 (length source-ids)))))
      (setf (gethash "through_event_id" fact) (gethash "application_event_id" boundary)
             (gethash "accepted_evidence_digest" fact) (%cg-authority-digest "accepted-claim-evidence" (gethash "evidence_records" fact))
             (gethash id (context-graph-facts graph)) fact
             (gethash key (context-graph-current-triples graph)) id)
      (%cg-authority-retire-opposed-inference graph opposed id boundary)
      (dolist (entity-id (remove-duplicates (list (gethash "entity_id" subject) (gethash "entity_id" object)) :test #'equal))
        (let ((adjacency (gethash entity-id (context-graph-entity-adjacency graph) #())))
          (unless (find id adjacency :test #'equal)
            (setf (gethash entity-id (context-graph-entity-adjacency graph))
                  (sort (concatenate 'vector adjacency (vector id)) #'string<))))) fact)))

(defun %cg-authority-partition-view (graph agent-id persona-id operator-entity-id)
  "Build a bounded scope input from the maintained enduring-identity adjacency.
Full accepted evidence stays in the projection; scopes receive its digest."
  (let* ((watermark (%cg-authority-watermark graph agent-id persona-id))
         (ids (gethash operator-entity-id (context-graph-entity-adjacency graph) #()))
         (claims (make-array (min 4096 (length ids)))))
    (dotimes (i (length claims))
      (let* ((fact (gethash (aref ids i) (context-graph-facts graph)))
             (grounding (gethash "grounding" fact)))
        (setf (aref claims i)
              (%cg-object "fact_id" (gethash "fact_id" fact) "identity_sha256" (gethash "identity_sha256" fact)
                          "subject_entity_id" (gethash "entity_id" (%cg-authority-entity graph (gethash "subject_id" fact)))
                          "object_entity_id" (gethash "entity_id" (%cg-authority-entity graph (gethash "object_id" fact)))
                          "predicate" (gethash "predicate" fact) "scope" (gethash "scope" grounding)
                          "polarity" (gethash "polarity" grounding) "source_basis" (gethash "source_basis" grounding)
                          "evidence_status" (gethash "evidence_status" fact) "accepted_source_ids" (%cg-detach (gethash "accepted_source_ids" fact))
                          "accepted_evidence_digest" (gethash "accepted_evidence_digest" fact)
                          "status" (gethash "status" fact) "through_event_id" (gethash "through_event_id" fact)))))
    (%cg-object "agent_id" agent-id "persona_id" persona-id "projection_watermark" watermark
                "entities" (context-graph-entities graph) "ontology" (context-graph-ontology graph)
                "operator_adjacency" claims "adjacency_complete" (if (<= (length ids) 4096) :true :false))))

(defun %cg-apply-authority-episode (graph boundary raw-proposal context review-receipt)
  "Private qualification entry: stage a complete reviewed formation atomically.
The runtime/access-aware owner is not wired to this entry yet. No live generation
can select it until its context authorization and storage adapters qualify."
  (unless (and (context-graph-p graph)
               (%cg-closed-keys-p boundary '("episode_id" "application_event_id" "opened_boundary_id" "observed_at"))
               (%cg-authority-string-p (gethash "episode_id" boundary) 180)
               (every (lambda (key) (and (integerp (gethash key boundary)) (plusp (gethash key boundary))))
                      '("application_event_id" "opened_boundary_id" "observed_at"))
               (< (gethash "opened_boundary_id" boundary) (gethash "application_event_id" boundary)))
    (%cg-authority-fail "APPLICATION_BOUNDARY_INVALID"))
  (when (and (null (context-graph-authority-profile graph))
             (or (plusp (context-graph-entity-count graph)) (plusp (context-graph-fact-count graph))
                 (plusp (hash-table-count (context-graph-episodes graph)))))
    (%cg-authority-fail "GENERATION_MIXING_FORBIDDEN"))
  (let* ((application-id (format nil "event:~d" (gethash "application_event_id" boundary)))
         (input-digest (%cg-authority-digest "authority-application" (vector boundary raw-proposal context review-receipt)))
         (prior (gethash application-id (context-graph-application-receipts graph))))
    ;; Replay identity is checked before current-state or target preconditions.
    (when prior
      (unless (equal input-digest (gethash "input_digest" prior)) (%cg-authority-fail "APPLICATION_CONFLICT"))
      (return-from %cg-apply-authority-episode (%cg-detach (%cg-object "schema_version" 2 "status" "already-applied"
                                                                       "formation_outcome" (gethash "formation_outcome" prior)))))
    (unless (> (gethash "application_event_id" boundary) (context-graph-through-event-id graph))
      (%cg-authority-fail "APPLICATION_ORDER_INVALID"))
    (context-graph-validate-authority-input context raw-proposal)
    (%cg-runtime-verify-correction-scans graph context)
    (unless (and (equal (gethash "episode_id" boundary) (gethash "episode_id" context))
                 (%cg-authority-equal-p (gethash "projection_watermark" context)
                                        (%cg-authority-watermark graph (gethash "agent_id" context) (gethash "persona_id" context)))
                 (or (eq :null review-receipt) (eql (gethash "opened_boundary_id" boundary) (gethash "opened_boundary_id" review-receipt))))
      (%cg-authority-fail "APPLICATION_CONTEXT_STALE"))
    (let* ((preparation (context-graph-prepare-authority context raw-proposal))
           (staged (%cg-stage-authority-state graph)))
      (unless (equal "accepted" (gethash "status" preparation))
        (return-from %cg-apply-authority-episode preparation))
      (setf (context-graph-authority-profile staged) "context-graph-authority-v1"
            (context-graph-authority-partition staged) (%cg-object "agent_id" (gethash "agent_id" context) "persona_id" (gethash "persona_id" context)))
      (let* ((prepared (gethash "proposal" (gethash "value" preparation)))
             (bindings (gethash "bindings" (gethash "value" preparation)))
             (views (coerce
                     (loop for revision across (gethash "entity_revisions" prepared)
                           for entity = (gethash (gethash "target_entity_id" revision) (context-graph-entities staged))
                           when entity collect (let ((view (%cg-detach entity)))
                                                 (setf (gethash "observed_at" view) (gethash "observed_at" boundary)
                                                       (gethash "application_id" view) application-id) view)) 'vector))
             (batch (%cg-decide-revision-batch context prepared review-receipt
                                                (remove-duplicates views :test #'equal :key (lambda (v) (gethash "entity_id" v)))))
             (outcome (gethash "formation_outcome" batch)) (refs (make-hash-table :test #'equal)))
        (when (equal "admitted" outcome)
          (loop for delta across (gethash "revision_deltas" batch) do (%cg-install-revision-delta staged delta))
          (loop for descriptor across (gethash "entities" prepared)
                for action = (gethash "identity_action" descriptor)
                do (cond ((equal "NEW" action) (%cg-authority-new-entity staged descriptor context bindings refs))
                         ((equal "LINK_EXISTING" action) (%cg-authority-link-entity staged descriptor context refs))
                         ((equal "REVISE_EXISTING" action)
                          (let* ((revision (find (gethash "local_ref" descriptor) (gethash "entity_revisions" prepared)
                                                 :test #'equal :key (lambda (r) (gethash "local_ref" r))))
                                 (id (gethash "target_entity_id" revision)))
                            (setf (gethash (gethash "local_ref" descriptor) refs) (gethash id (context-graph-current-entity-versions staged)))))))
          (loop for relationship across (gethash "relationships" prepared)
                do (%cg-authority-apply-claim staged relationship context boundary refs)))
        (setf (gethash application-id (context-graph-application-receipts staged))
              (%cg-object "application_id" application-id "input_digest" input-digest "episode_id" (gethash "episode_id" context)
                          "formation_outcome" outcome "prepared_authority" (gethash "value" preparation)
                          "review_receipt" (%cg-detach review-receipt) "batch" batch)
              (context-graph-entity-scan-index staged) (coerce (sort (loop for id being the hash-keys of (context-graph-entities staged) collect id) #'string<) 'vector)
              (context-graph-fact-scan-index staged) (coerce (sort (loop for id being the hash-keys of (context-graph-facts staged) collect id) #'string<) 'vector)
              (context-graph-through-event-id staged) (gethash "application_event_id" boundary)
              (context-graph-projection-digest staged) (%cg-authority-projection-digest staged (gethash "agent_id" context) (gethash "persona_id" context)))
        (%cg-install-staged-state graph staged)
        (%cg-detach (%cg-object "schema_version" 2 "status" "applied" "formation_outcome" outcome "batch" batch))))))

(defun %cg-authority-descriptor-tokens (descriptor)
  (%cg-query-tokens
   (format nil "~a ~a ~{~a~^ ~} ~{~a~^ ~}"
           (gethash "kind" descriptor) (gethash "label" descriptor)
           (coerce (gethash "aliases" descriptor) 'list) (coerce (gethash "classifications" descriptor) 'list))))

(defun %cg-authority-search (graph agent-id persona-id query &key (maximum-results 10) (scan-limit 4096))
  "Private qualification read, not an access-authorized memory search entry.
Rank current endpoint descriptors, never stale quoted evidence as current names.
The bounded prefix is explicit and incomplete results are never called complete."
  (unless (and (%cg-authority-string-p query 1000) (integerp maximum-results) (<= 1 maximum-results 50)
               (integerp scan-limit) (<= 1 scan-limit 4096)
               (equal "context-graph-authority-v1" (context-graph-authority-profile graph)))
    (%cg-authority-fail "SEARCH_INPUT_INVALID"))
  (%cg-authority-watermark graph agent-id persona-id)
  (let* ((tokens (%cg-query-tokens query)) (ids (context-graph-fact-scan-index graph))
         (count (min scan-limit (length ids))) (rows nil))
    (dotimes (i count)
      (let* ((fact (gethash (aref ids i) (context-graph-facts graph)))
             (grounding (gethash "grounding" fact)))
        (when (and (equal "current" (gethash "status" fact))
                   (equal "assertion" (gethash "scope" grounding))
                   (equal "positive" (gethash "polarity" grounding))
                   (member (gethash "evidence_status" fact) '("direct" "prior-graph") :test #'equal))
          (let* ((subject (%cg-authority-current-descriptor graph (gethash "entity_id" (%cg-authority-entity graph (gethash "subject_id" fact)))))
                 (object (%cg-authority-current-descriptor graph (gethash "entity_id" (%cg-authority-entity graph (gethash "object_id" fact)))))
                 (terms (append (%cg-authority-descriptor-tokens subject) (%cg-authority-descriptor-tokens object)
                                (%cg-query-tokens (gethash "predicate" fact))))
                 (score (count-if (lambda (token) (member token terms :test #'equal)) tokens)))
            (when (plusp score)
              (push (%cg-object "fact_id" (gethash "fact_id" fact) "score" score
                                "subject" subject "predicate" (gethash "predicate" fact) "object" object
                                "grounding" grounding "accepted_evidence_digest" (gethash "accepted_evidence_digest" fact)) rows))))))
    (setf rows (sort rows (lambda (a b) (if (= (gethash "score" a) (gethash "score" b))
                                           (string< (gethash "fact_id" a) (gethash "fact_id" b))
                                           (> (gethash "score" a) (gethash "score" b))))))
    (%cg-detach (%cg-object "schema_version" 1 "rows" (coerce (subseq rows 0 (min maximum-results (length rows))) 'vector)
                            "examined_count" count "scan_complete" (if (= count (length ids)) :true :false)))))
