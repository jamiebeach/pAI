;;;; Shared synthetic runtime inputs; never production authority.
(in-package :pai.context-graph)
(defun as-fixture (&optional (count 2))
  (let ((entities (make-hash-table :test #'equal)) (claims nil)
        (participants
          (map 'vector (lambda (role kind)
                         (%cg-object "role" role "speaker_id" (format nil "principal:~a" role)
                                     "principal_id" (format nil "principal:~a" role)
                                     "identity_binding_id" (format nil "binding:~a" role)
                                     "local_ref" (format nil "runtime:~a" role) "entity_id" (format nil "entity:~a" role)
                                     "kind" kind "label" role "aliases" #()))
               #("operator" "active-persona") #("person" "agent"))))
    (dotimes (i count)
      (let* ((id (format nil "entity:~3,'0d" i))
             (entity (%cg-object "entity_id" id "node_id" id "kind" "organism" "label" "Same label"
                                  "aliases" #() "classifications" #("cat") "participant_role" :null
                                  "status" "current" "agent_id" "agent:one" "persona_id" "persona:one")))
        (setf (gethash "revision_digest" entity) (%cg-revision-descriptor-digest entity)
              (gethash id entities) entity)
        (push (%cg-object "fact_id" (format nil "fact:~5,'0d" i) "identity_sha256" (%cg-sha256 (format nil "claim ~d" i))
                          "subject_entity_id" "entity:operator" "predicate" "owns" "object_entity_id" id
                          "scope" "assertion" "polarity" "positive" "source_basis" "original" "evidence_status" "direct"
                          "accepted_source_ids" #("source:historical") "accepted_evidence_digest" (%cg-sha256 "evidence")
                          "status" "current" "through_event_id" 10) claims)))
    (values
     (%cg-object "agent_id" "agent:one" "persona_id" "persona:one"
                 "projection_watermark" (%cg-object "projection_revision" "grounded-knowledge-graph-formation-v7"
                                                      "through_event_id" 10 "state_digest" (%cg-sha256 "state"))
                 "entities" entities "operator_adjacency" (coerce (nreverse claims) 'vector) "adjacency_complete" :true
                 "ontology" (%cg-object "entity_types" #("person" "agent" "organism")
                                         "edge_types" (vector (%cg-object "name" "owns" "subject_types" #("person" "agent")
                                                                          "object_types" #("organism")))))
     (%cg-object "policy_revision" "operator-conversational-label-correction-v1" "enabled" :true
                 "scope_definitions" (vector (%cg-object "definition_id" "operator-organisms" "predicate" "owns"
                                                         "operator_endpoint" "subject" "target_kind" "organism")))
     participants)))
(defun ar-fixture (&optional (label "Mira") (quote "The stored name is wrong; use Mira."))
  (let* ((operator (%cg-object "role" "operator" "speaker_id" "principal:operator"
                               "principal_id" "principal:operator" "identity_binding_id" "binding:operator"
                               "local_ref" "runtime:operator" "entity_id" "entity:operator"
                               "kind" "person" "label" "Operator" "aliases" #()))
         (persona (%cg-object "role" "active-persona" "speaker_id" "principal:persona"
                              "principal_id" "principal:persona" "identity_binding_id" "binding:persona"
                              "local_ref" "runtime:active-persona" "entity_id" "entity:persona"
                              "kind" "agent" "label" "Assistant" "aliases" #()))
         (source (%cg-object "source_id" "source:correction" "speaker_id" "principal:operator"
                             "kind" "original-utterance" "timestamp" 200 "text" quote "text_sha256" (%cg-sha256 quote)
                             "identity" (%cg-object "principal_id" "principal:operator" "binding_id" "binding:operator"
                                                     "conversation_id" "conversation:one" "role" "operator")
                             "resource_ref" (%cg-object "store" "event" "resource_id" "event:correction"
                                                         "version_id" "version:one" "component" "content")))
         (context (%cg-object "authority_revision" "context-graph-authority-v1"
                              "agent_id" "agent:one" "persona_id" "persona:one" "episode_id" "episode:one"
                              "participants" (vector operator persona) "primary_source_ids" #("source:correction")
                              "source_packet" (%cg-object "schema_version" 2 "sources" (vector source))))
         (view (%cg-object "agent_id" "agent:one" "persona_id" "persona:one" "entity_id" "entity:cat"
                           "node_id" "node:cat:v1" "kind" "organism" "label" "Mina"
                           "aliases" #("MINA" "Pet" "mira" "Little cat") "classifications" #("cat")
                           "participant_role" :null "status" "current" "observed_at" 200 "application_id" "application:one"))
         (grant (%cg-object "authority_revision" "context-graph-authority-v1"
                            "admission_policy_revision" "operator-conversational-label-correction-v1"
                            "admission_basis" "reviewed-operator-conversation" "semantic_status" "policy-accepted-interpretation"
                            "agent_id" "agent:one" "persona_id" "persona:one" "episode_id" "episode:one" "revision_ref" "revision:one"
                            "operation" "correct-primary-label" "entity_id" "entity:cat" "target_node_id" "node:cat:v1"
                            "replacement_label" label "kind" "organism" "target_scope_id" "scope:cats"
                            "target_scope_digest" (%cg-sha256 "scope") "anchor_claim_digests" (vector (%cg-sha256 "anchor"))
                            "review_id" (%cg-sha256 "review") "proposal_digest" (%cg-sha256 "proposal")
                            "operator_command_id" :null "evidence" (gethash "value" (context-graph-resolve-source-span source quote)))))
    (setf (gethash "revision_digest" view) (%cg-revision-descriptor-digest view)
          (gethash "expected_revision_digest" grant) (gethash "revision_digest" view)
          (gethash "grant_id" grant) (%cg-revision-grant-id grant))
    (values context grant view)))

(defun at-fixture (&optional (candidate-count 2))
  "Return full trusted context, raw correction proposal, qualified current view."
  (multiple-value-bind (partition policy participants) (as-fixture candidate-count)
    (multiple-value-bind (context unused-grant unused-view) (ar-fixture)
      (declare (ignore unused-grant unused-view))
      (let* ((scoped (gethash "value" (context-graph-build-correction-scopes partition policy participants)))
             (eligible (gethash "eligible_entities" scoped)) (target (aref eligible 0))
             (scope (aref (gethash "scopes" scoped) 0))
             (view (%cg-detach target))
             (entity (%cg-object "local_ref" "corrected" "kind" (gethash "kind" target) "label" "Mira"
                                  "aliases" (%cg-detach (gethash "aliases" target)) "classifications" (%cg-detach (gethash "classifications" target))
                                  "identity_action" "REVISE_EXISTING" "existing_node_id" (gethash "node_id" target)
                                  "evidence_status" "direct" "evidence_note" "synthetic extractor judgment"))
             (revision (%cg-object "schema_version" 1 "revision_ref" "revision:one" "operation" "correct-primary-label"
                                    "local_ref" "corrected" "requested_route" "conversation"
                                    "target_entity_id" (gethash "entity_id" target) "target_node_id" (gethash "node_id" target)
                                    "expected_revision_digest" (gethash "revision_digest" target) "target_scope_id" (gethash "scope_id" scope)
                                    "operator_command_id" :null "replacement_label" "Mira" "interpretation" "error-correction"
                                    "grounding" (%cg-object "schema_version" 2 "scope" "assertion" "polarity" "positive"
                                                            "attributed_to_ref" "runtime:operator"
                                                            "evidence" (vector (%cg-object "source_id" "source:correction"
                                                                                           "quote" "The stored name is wrong; use Mira.")))
                                    "context_evidence" #())))
        (setf (gethash "schema_version" context) 1
              (gethash "participants" context) participants
              (gethash "access_context" context)
              (%cg-object "schema_version" 1 "executor_principal_id" "principal:active-persona" "authority_principal_id" "principal:operator"
                          "recipient_principal_id" :null "recipient_binding_id" :null "recipient_set_digest" :null
                          "task_id" :null "purpose" "private-planning" "channel_id" "private-runtime"
                          "partition" (%cg-object "agent_id" "agent:one" "persona_id" "persona:one")
                          "action" "derive" "grant_ids" #() "now_utc" 200 "policy_epoch" 1)
              (gethash "access_snapshot_digest" context) (%cg-sha256 "synthetic-policy-snapshot")
              (gethash "projection_watermark" context) (gethash "projection_watermark" partition)
              (gethash "correction_policy" context) policy
              (gethash "correction_scopes" context) (gethash "scopes" scoped)
              (gethash "eligible_entities" context) eligible
              (gethash "operator_commands" context) #()
              (gethash "candidate_scan" context) (%cg-object "complete" :true "examined_count" candidate-count)
              (gethash "observed_at" view) 200 (gethash "application_id" view) "application:one")
        (values context (%cg-object "schema_version" 4 "ontology_revision" "personal-context-core-glm53-v1.2"
                                   "entities" (vector entity) "relationships" #() "entity_revisions" (vector revision)) view)))))

(defun at-raw-review (context prepared)
  (let ((request (gethash "request" (gethash "value" (context-graph-build-revision-review-input context prepared)))))
    (%cg-object "schema_version" 1 "reviews"
      (map 'vector
           (lambda (input)
             (let* ((revision (gethash "proposed_revision" input)) (scope (gethash "target_scope" input))
                    (target (gethash "target_entity_id" revision)))
               (%cg-object "revision_ref" (gethash "revision_ref" input) "proposal_digest" (gethash "proposal_digest" input)
                           "interpretation" "error-correction" "source_reading" "operator-assertion"
                           "target_scope_fit" "supported" "same_entity" "supported" "replacement_supported" "supported"
                           "competing_interpretation" "none-found"
                           "candidate_assessments"
                           (map 'vector (lambda (id)
                                          (%cg-object "entity_id" id "assessment" (if (equal id target) "target" "not-target")
                                                      "anchor_fact_ids" (if (equal id target)
                                                                            (vector (gethash "fact_id" (find id (gethash "anchor_claims" scope)
                                                                                                            :test #'equal :key (lambda (a) (gethash "object_entity_id" a))))) #())
                                                      "reason_code" (if (equal id target) "context-and-correction-agree" "context-excludes")))
                                (gethash "candidate_entity_ids" scope)))))
           (gethash "revisions" request)))))

(defun at-review-receipt (context prepared raw)
  (gethash "value"
    (context-graph-validate-revision-review
     context prepared raw
     (%cg-object "opened_boundary_id" 12
                 "request_digest" (gethash "request_digest" (gethash "value" (context-graph-build-revision-review-input context prepared)))
                 "response_digest" (%cg-authority-digest "revision-review-response" raw)))))
