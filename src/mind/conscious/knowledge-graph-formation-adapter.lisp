;;;; knowledge-graph-formation-adapter.lisp -- concrete recursive KG2 port.

(in-package :agent)

(export '(conscious-recursive-knowledge-graph-formation-step))

(defparameter *conscious-recursive-kg-formation-protocol-revision*
  *knowledge-graph-formation-attempt-revision*)
(defparameter *conscious-recursive-kg-formation-max-output-tokens* 8192)
(defparameter *conscious-recursive-kg-evidence-review-max-output-tokens* 8192)

(defun %recursive-kg-quote-words (text)
  (let ((words nil) (start nil) (length (length text)))
    (labels ((finish (end)
               (when start
                 (push (string-downcase (subseq text start end)) words)
                 (setf start nil))))
      (loop for index from 0 below length
            for character = (char text index)
            do (if (alphanumericp character)
                   (unless start (setf start index))
                   (finish index)))
      (finish length))
    (nreverse words)))

(defun %recursive-kg-source-day (source offset)
  (handler-case
      (let ((timestamp (%kgf-event-time
                        (obj "timestamp" (gethash "timestamp" source)))))
        (multiple-value-bind (second minute hour day month year)
            (decode-universal-time (+ timestamp (* offset 86400)) 0)
          (declare (ignore second minute hour))
          (format nil "~4,'0d-~2,'0d-~2,'0d" year month day)))
    (error () nil)))

(defun %recursive-kg-normalized-temporal (relationship opened-payload)
  "Make time source-owned: exact dates may survive; relative days are derived."
  (let* ((supplied (gethash "temporal" relationship))
         (grounding (gethash "grounding" relationship))
         (citations (gethash "evidence" grounding #()))
         (sources (gethash "evidence_records" opened-payload #()))
         (joined
           (format nil "~{~a~^ ~}"
                   (map 'list (lambda (row) (gethash "quote" row ""))
                        citations)))
         (words (%recursive-kg-quote-words joined))
         (normalized
           (obj "schema_version" 1
                "character" (gethash "character" supplied)
                "occurred_at" :null "valid_from" :null "valid_until" :null)))
    ;; Preserve only a model-supplied bound literally present in cited text.
    (dolist (field '("occurred_at" "valid_from" "valid_until"))
      (let ((value (gethash field supplied)))
        (when (and (stringp value) (search value joined :test #'char-equal))
          (setf (gethash field normalized) value))))
    ;; Relative dates are derived from the timestamp of their exact citation.
    (let* ((relative
             (cond ((find "yesterday" words :test #'string=) -1)
                   ((find "today" words :test #'string=) 0)
                   ((find "tomorrow" words :test #'string=) 1)))
           (citation (and relative (plusp (length citations))
                          (aref citations 0)))
           (source (and citation
                        (find (gethash "source_id" citation) sources
                              :test #'string=
                              :key (lambda (row)
                                     (gethash "source_id" row "")))))
           (day (and source (%recursive-kg-source-day source relative))))
      (when day
        (setf (gethash "occurred_at" normalized) day)))
    normalized))

(defun %recursive-kg-grounding-schema ()
  (obj "type" "object" "additionalProperties" nil
       "properties"
       (obj "schema_version" (obj "type" "integer" "enum" #(1))
            "scope" (obj "type" "string"
                         "enum" (coerce *knowledge-graph-claim-scopes* 'vector))
            "polarity" (obj "type" "string"
                            "enum" #( "positive" "negative" "unknown"))
            "attributed_to_ref"
            (obj "anyOf" (vector (obj "type" "string" "maxLength" 80)
                                   (obj "type" "null")))
            "evidence"
            (obj "type" "array" "minItems" 1 "maxItems" 4
                 "items"
                 (obj "type" "object" "additionalProperties" nil
                      "properties"
                      (obj "source_id" (obj "type" "string" "maxLength" 180)
                           "quote" (obj "type" "string" "maxLength" 1000))
                      "required" #( "source_id" "quote")))
            )
       "required" #( "schema_version" "scope" "polarity"
                      "attributed_to_ref" "evidence")))

(defun %recursive-kg-temporal-schema ()
  (obj "type" "object" "additionalProperties" nil
       "properties"
       (obj "schema_version" (obj "type" "integer" "enum" #(1))
            "character" (obj "type" "string"
                             "enum" (coerce *knowledge-graph-temporal-characters*
                                            'vector))
            "occurred_at" (obj "anyOf" (vector (obj "type" "string" "maxLength" 80)
                                                 (obj "type" "null")))
            "valid_from" (obj "anyOf" (vector (obj "type" "string" "maxLength" 80)
                                                (obj "type" "null")))
            "valid_until" (obj "anyOf" (vector (obj "type" "string" "maxLength" 80)
                                                 (obj "type" "null"))))
       "required" #( "schema_version" "character" "occurred_at"
                      "valid_from" "valid_until")))

(defun %recursive-kg-formation-schema ()
  (vector
   (obj "type" "function" "function"
        (obj "name" "write-knowledge-graph-formation" "strict" t
             "description"
             "Propose generic entities and typed relationships grounded only in the supplied evidence."
             "parameters"
             (obj
              "type" "object" "additionalProperties" nil
              "properties"
              (obj
               "schema_version" (obj "type" "integer" "enum" #(3))
               "ontology_revision"
               (obj "type" "string"
                    "enum" (vector *knowledge-graph-ontology-revision*))
               "entities"
               (obj "type" "array" "minItems" 1 "maxItems" 24
                    "items"
                    (obj "type" "object" "additionalProperties" nil
                         "properties"
                         (obj
                          "local_ref" (obj "type" "string" "maxLength" 80)
                          "kind"
                          (obj "type" "string"
                               "enum" (coerce *knowledge-graph-ontology-kinds*
                                              'vector))
                          "label" (obj "type" "string" "maxLength" 240)
                          "aliases"
                          (obj "type" "array" "maxItems" 8 "uniqueItems" t
                               "items" (obj "type" "string" "maxLength" 240))
                          "classifications"
                          (obj "type" "array" "maxItems" 8 "uniqueItems" t
                               "items" (obj "type" "string" "maxLength" 120))
                          "identity_action"
                          (obj "type" "string"
                               "enum" #( "NEW" "LINK_EXISTING"
                                         "REVISE_EXISTING"))
                          "existing_node_id"
                          (obj "anyOf"
                               (vector (obj "type" "string" "maxLength" 180)
                                       (obj "type" "null"))))
                         "required"
                         #( "local_ref" "kind" "label" "aliases"
                            "classifications"
                            "identity_action" "existing_node_id")))
               "relationships"
               (obj "type" "array" "maxItems" 48
                    "items"
                    (obj "type" "object" "additionalProperties" nil
                         "properties"
                         (obj
                          "subject_ref" (obj "type" "string" "maxLength" 80)
                          "predicate"
                          (obj "type" "string"
                               "enum"
                               (coerce (mapcar #'first
                                               *knowledge-graph-ontology-signatures*)
                                       'vector))
                          "object_ref" (obj "type" "string" "maxLength" 80)
                          "relationship_action"
                          (obj "type" "string" "enum" #( "ASSERT" "RETIRE"))
                          "fact" (obj "type" "string" "maxLength" 600)
                          "grounding" (%recursive-kg-grounding-schema)
                          "temporal" (%recursive-kg-temporal-schema))
                         "required"
                         #( "subject_ref" "predicate" "object_ref"
                            "relationship_action" "fact" "grounding"
                            "temporal"))))
              "required" #( "schema_version" "ontology_revision"
                             "entities" "relationships"))))))

(defun %recursive-kg-unreviewed-proposal (proposal)
  (obj "schema_version" 3
       "ontology_revision" (gethash "ontology_revision" proposal)
       "entities"
       (map 'vector
            (lambda (row)
              (let ((copy (%kgf-copy-object row)))
                (setf (gethash "evidence_status" copy) "unreviewed"
                      (gethash "evidence_note" copy) "pending evidence review")
                copy))
            (gethash "entities" proposal #()))
       "relationships"
       (map 'vector
            (lambda (row)
              (let ((copy (%kgf-copy-object row)))
                (setf (gethash "evidence_status" copy) "unreviewed"
                      (gethash "evidence_note" copy) "pending evidence review")
                copy))
            (gethash "relationships" proposal #()))))

(defun %recursive-kg-canonical-source-quote (quote source)
  "Use the shared fidelity owner under the existing generation's contract."
  (pai.context-graph:context-graph-resolve-legacy-source-quote source quote))

(defun %recursive-kg-canonical-relationship (relationship source-evidence)
  (let* ((copy (%kgf-copy-object relationship))
         (grounding-source (gethash "grounding" relationship))
         (grounding (and (hash-table-p grounding-source)
                         (%kgf-copy-object grounding-source)))
         (citations (and grounding (gethash "evidence" grounding)))
         (canonical nil))
    (when (vectorp citations)
      (loop for citation across citations
            for source-id = (gethash "source_id" citation)
            for source = (find source-id source-evidence :test #'string=
                               :key (lambda (row)
                                      (gethash "source_id" row "")))
            for exact = (and source
                             (%recursive-kg-canonical-source-quote
                              (gethash "quote" citation)
                              (gethash "text" source)))
            when exact
              do (let ((citation-copy (%kgf-copy-object citation)))
                   (setf (gethash "quote" citation-copy) exact)
                   (push citation-copy canonical))
            else do (return-from %recursive-kg-canonical-relationship nil))
      (setf (gethash "evidence" grounding)
            (coerce (nreverse canonical) 'vector)
            (gethash "grounding" copy) grounding)
      copy)))

(defun %recursive-kg-ontology-filtered-proposal (proposal opened-payload)
  "Retain the independently valid relationship subset of a model proposal.

Invalid references, ontology signatures and non-recoverable citations cannot
poison otherwise useful entities and facts.  No new semantic claim is created:
markdown-cleaned quotes are admitted only when they map unambiguously back to
one exact source substring."
  (let ((kinds (make-hash-table :test #'equal)))
    (loop for entity across (gethash "entities" proposal #())
          do (setf (gethash (gethash "local_ref" entity) kinds)
                   (gethash "kind" entity)))
    (let* ((entities (copy-seq (gethash "entities" proposal #())))
           (refs (loop for entity across entities
                       collect (gethash "local_ref" entity)))
           (source-evidence
             (gethash "evidence_records" opened-payload #())))
      (obj "schema_version" 3
           "ontology_revision" (gethash "ontology_revision" proposal)
           "entities" entities
           "relationships"
           (coerce
            (loop for relationship across
                    (gethash "relationships" proposal #())
                  for subject-kind =
                    (gethash (gethash "subject_ref" relationship) kinds)
                  for object-kind =
                    (gethash (gethash "object_ref" relationship) kinds)
                  for canonical =
                    (and subject-kind object-kind
                         (knowledge-graph-ontology-signature-valid-p
                          (gethash "predicate" relationship)
                          subject-kind object-kind)
                         (%recursive-kg-canonical-relationship
                          relationship source-evidence))
                  for checked = (and canonical (%kgf-copy-object canonical))
                  when checked
                    do (setf (gethash "evidence_status" checked) "unreviewed"
                             (gethash "evidence_note" checked)
                             "pending evidence review")
                  when (and checked
                            (%kgf-relationship-valid-p
                             checked refs kinds 3 source-evidence))
                    collect canonical)
            'vector)))))

(defun %recursive-kg-canonicalize-participants (proposal opened-payload)
  "Bind explicitly classified conversation participants to runtime-owned IDs."
  (let ((candidates
          (gethash "eligible_existing_nodes" opened-payload #())))
    (let ((descriptors nil))
      (dolist (role '("operator" "active-persona"))
        (let ((matches
                (loop for candidate across candidates
                      when (string= role
                                    (gethash "participant_role" candidate ""))
                        collect candidate)))
          (when (> (length matches) 1)
            (error "KG participant candidate is not unique"))
          (let ((match (first matches)))
            (push
             (obj "role" role
                  "kind" (if (string= role "operator") "person" "agent")
                  "label" (if match (gethash "label" match) :null)
                  "aliases" (if match
                                (copy-seq (gethash "aliases" match #())) #())
                  "existing_node_id"
                  (if match (gethash "node_id" match) :null))
             descriptors))))
      (pai.context-graph:context-graph-normalize-legacy-participants
       proposal (coerce (nreverse descriptors) 'vector)))))

(defun %recursive-kg-formation-proposal (message opened-payload)
  (let* ((calls (and (hash-table-p message) (gethash "tool_calls" message)))
         (call (and (vectorp calls) (= 1 (length calls)) (aref calls 0)))
         (function (and (hash-table-p call) (gethash "function" call)))
         (encoded (and (hash-table-p function) (gethash "arguments" function)))
         (raw-proposal (and (stringp encoded) (shasht:read-json encoded)))
         (proposal
           (and (hash-table-p raw-proposal)
                (%recursive-kg-canonicalize-participants
                 (%recursive-kg-ontology-filtered-proposal
                  raw-proposal opened-payload)
                 opened-payload)))
         (eligible
           (map 'vector (lambda (row) (gethash "node_id" row))
                (gethash "eligible_existing_nodes" opened-payload #())))
         (candidate
           (and proposal
                (obj "schema_version" 1
                     "persona_id" (gethash "persona_id" opened-payload)
                     "disclosure_class"
                     (gethash "disclosure_class" opened-payload)
                     "formation_revision" *knowledge-graph-formation-revision*
                     "source_event_ids"
                     (gethash "source_event_ids" opened-payload)
                     "source_memory_node_ids"
                     (gethash "source_memory_node_ids" opened-payload)
                     "source_episode_ids"
                     (gethash "source_episode_ids" opened-payload)
                     "source_evidence"
                     (gethash "evidence_records" opened-payload)
                     "eligible_existing_node_ids" eligible
                     "proposal" (%recursive-kg-unreviewed-proposal proposal)))))
    (unless (and (hash-table-p call) (hash-table-p function)
                 (string= "function" (gethash "type" call ""))
                 (string= "write-knowledge-graph-formation"
                          (gethash "name" function ""))
                 (knowledge-graph-formation-sealed-payload-valid-p candidate))
      (error "KG2 provider response violates the strict semantic contract"))
    proposal))

(defun %recursive-kg-formation-cues (episode-event)
  (let ((payload (gethash "payload" episode-event)) (cues nil))
    (dolist (key '("entities" "subjects" "retrieval_cues"
                   "broader_categories" "unresolved_threads"))
      (dolist (value (%recursive-items (gethash key payload)))
        (when (and (stringp value) (plusp (length value))
                   (<= (length value) 240))
          (pushnew value cues :test #'string-equal))))
    (let ((ordered (nreverse cues)))
      (coerce (subseq ordered 0 (min 24 (length ordered))) 'vector))))

(defun %recursive-kg-native-arguments (message expected-name)
  (let* ((calls (and (hash-table-p message) (gethash "tool_calls" message)))
         (call (and (vectorp calls) (= 1 (length calls)) (aref calls 0)))
         (function (and (hash-table-p call) (gethash "function" call)))
         (encoded (and (hash-table-p function) (gethash "arguments" function))))
    (unless (and (hash-table-p call) (hash-table-p function)
                 (string= "function" (gethash "type" call ""))
                 (string= expected-name (gethash "name" function ""))
                 (stringp encoded))
      (error "KG2 provider response has an invalid native-tool shape"))
    (shasht:read-json encoded)))

(defun %recursive-kg-evidence-claims (proposal)
  (let ((claims nil))
    (loop for entity across (gethash "entities" proposal)
          do (push (obj "claim_ref"
                        (format nil "entity:~a" (gethash "local_ref" entity))
                        "claim_kind" "entity" "claim" entity)
                   claims))
    (loop for relationship across (gethash "relationships" proposal)
          for index from 0
          do (push (obj "claim_ref" (format nil "relationship:~d" index)
                        "claim_kind" "relationship" "claim" relationship)
                   claims))
    (coerce (nreverse claims) 'vector)))

(defun %recursive-kg-evidence-review-schema (claim-count)
  (vector
   (obj "type" "function" "function"
        (obj "name" "review-knowledge-graph-evidence" "strict" t
             "description"
             "Classify every proposed graph claim against the sealed evidence."
             "parameters"
             (obj "type" "object" "additionalProperties" nil
                  "properties"
                  (obj "schema_version" (obj "type" "integer" "enum" #(1))
                       "claim_reviews"
                       (obj "type" "array" "minItems" claim-count
                            "maxItems" claim-count
                            "items"
                            (obj "type" "object" "additionalProperties" nil
                                 "properties"
                                 (obj "claim_ref"
                                      (obj "type" "string" "maxLength" 120)
                                      "verdict"
                                      (obj "type" "string"
                                           "enum"
                                           #( "DIRECTLY_EVIDENCED"
                                              "EXACT_PRIOR_GRAPH"
                                              "REASONABLE_INFERENCE"
                                              "UNSUPPORTED"))
                                      "evidence"
                                      (obj "type" "string" "maxLength" 600))
                                 "required" #( "claim_ref" "verdict"
                                                "evidence"))))
                  "required" #( "schema_version" "claim_reviews"))))))

(defun %recursive-kg-evidence-review (message proposal)
  (let* ((review (%recursive-kg-native-arguments
                  message "review-knowledge-graph-evidence"))
         (claims (%recursive-kg-evidence-claims proposal))
         (expected (map 'list (lambda (row) (gethash "claim_ref" row)) claims))
         (rows (and (hash-table-p review) (gethash "claim_reviews" review)))
         (index (make-hash-table :test #'equal)))
    (unless (and (hash-table-p review)
                 (= 1 (gethash "schema_version" review -1))
                 (= 2 (hash-table-count review))
                 (vectorp rows) (= (length rows) (length claims)))
      (error "KG2 evidence review violates its closed collection contract"))
    (loop for row across rows
          for ref = (and (hash-table-p row) (gethash "claim_ref" row))
          for verdict = (and (hash-table-p row) (gethash "verdict" row))
          for evidence = (and (hash-table-p row) (gethash "evidence" row))
          do (unless (and (= 3 (hash-table-count row))
                          (find ref expected :test #'string=)
                          (not (gethash ref index))
                          (member verdict
                                  '("DIRECTLY_EVIDENCED" "EXACT_PRIOR_GRAPH"
                                    "REASONABLE_INFERENCE" "UNSUPPORTED")
                                  :test #'string=)
                          (%kgf-required-string-p evidence 600))
               (error "KG2 evidence review contains an invalid claim row"))
             (setf (gethash ref index) row))
    (unless (= (hash-table-count index) (length expected))
      (error "KG2 evidence review does not cover every proposal claim"))
    index))

(defun %recursive-kg-reviewed-proposal (proposal reviews opened-payload)
  (let ((accepted-refs (make-hash-table :test #'equal))
        (eligible-node-ids
          (map 'list (lambda (row) (gethash "node_id" row))
               (gethash "eligible_existing_nodes" opened-payload #())))
        (entities nil) (relationships nil))
    (labels ((status (verdict)
               (cond ((string= verdict "DIRECTLY_EVIDENCED") "direct")
                     ((string= verdict "EXACT_PRIOR_GRAPH") "prior-graph")
                     (t nil)))
             (annotate (row review)
               (let ((copy (%kgf-copy-object row)))
                 (setf (gethash "evidence_status" copy)
                       (status (gethash "verdict" review))
                       (gethash "evidence_note" copy)
                       (gethash "evidence" review))
                 copy))
             (annotate-relationship (row review)
               (let ((copy (annotate row review)))
                 (setf (gethash "temporal" copy)
                       (%recursive-kg-normalized-temporal row opened-payload))
                 copy)))
      (loop for entity across (gethash "entities" proposal)
            for local-ref = (gethash "local_ref" entity)
            for review = (gethash (format nil "entity:~a" local-ref) reviews)
            for verdict = (gethash "verdict" review)
            do (when (and (string= verdict "EXACT_PRIOR_GRAPH")
                          (not (and
                                (string= "LINK_EXISTING"
                                         (gethash "identity_action" entity ""))
                                (find (gethash "existing_node_id" entity)
                                      eligible-node-ids :test #'string=))))
                 (error "KG2 exact-prior verdict lacks an eligible exact identity"))
            when (status verdict)
              do (setf (gethash local-ref accepted-refs) t)
                 (push (annotate entity review) entities))
      (loop for relationship across (gethash "relationships" proposal)
            for index from 0
            for review = (gethash (format nil "relationship:~d" index) reviews)
            do (when (string= "EXACT_PRIOR_GRAPH"
                              (gethash "verdict" review ""))
                 ;; The sealed packet currently supplies exact existing
                 ;; identities, not existing relationship facts.
                 (error "KG2 exact-prior relationship verdict lacks supplied fact authority"))
            when (and (status (gethash "verdict" review))
                      (gethash (gethash "subject_ref" relationship) accepted-refs)
                      (gethash (gethash "object_ref" relationship) accepted-refs))
              do (push (annotate-relationship relationship review)
                       relationships)))
    (unless entities
      (error "KG2 evidence review rejected every proposed entity"))
    (let* ((reviewed
             (obj "schema_version" 3
                  "ontology_revision" *knowledge-graph-ontology-revision*
                  "entities" (coerce (nreverse entities) 'vector)
                  "relationships" (coerce (nreverse relationships) 'vector)))
           (candidate
             (obj "schema_version" 1
                  "persona_id" (gethash "persona_id" opened-payload)
                  "disclosure_class" (gethash "disclosure_class" opened-payload)
                  "formation_revision" *knowledge-graph-formation-revision*
                  "source_event_ids" (gethash "source_event_ids" opened-payload)
                  "source_memory_node_ids"
                  (gethash "source_memory_node_ids" opened-payload)
                  "source_episode_ids" (gethash "source_episode_ids" opened-payload)
                  "source_evidence" (gethash "evidence_records" opened-payload)
                  "eligible_existing_node_ids"
                  (map 'vector (lambda (row) (gethash "node_id" row))
                       (gethash "eligible_existing_nodes" opened-payload #()))
                  "proposal" reviewed)))
      (unless (knowledge-graph-formation-sealed-payload-valid-p candidate)
        (error "KG2 reviewed proposal violates ontology or sealed contract"))
      reviewed)))

(defun %recursive-kg-model-call
    (messages tools opened-id thread-id phase output-tokens)
  (let ((*conscious-conversation-max-output-tokens* output-tokens))
  (when (%recursive-operator-pending-p)
    (return-from %recursive-kg-model-call :preempted))
  (unless (%recursive-selected-call-admissible-p messages tools t "required")
    (return-from %recursive-kg-model-call :paused-budget))
  (let ((model-call-id
          (format nil "model:knowledge-graph-~a:~a:~d" phase opened-id
                  (incf *conscious-recursive-mind-sequence*))))
    (%conversation-append-readable
     "model-request"
     (obj "thread_id" thread-id "model_call_id" model-call-id
          "runtime_revision" *conscious-recursive-mind-runtime-revision*
          "protocol_revision" *conscious-recursive-kg-formation-protocol-revision*
          "model" *conscious-recursive-mind-model* "tools_advertised" t
          "knowledge_graph_formation" t "formation_phase" phase
          "content_persisted" nil)
     :caused-by opened-id)
    (%recursive-notify "activity" (obj "channel" "private")
                       (obj "kind" (format nil "knowledge-graph-~a-request" phase)
                            "model_call_id" model-call-id))
    (handler-case
        (let ((response
                (%conversation-call-model-with-trace
                 messages
                 (obj "runtime_revision" *conscious-recursive-mind-runtime-revision*
                      "thread_id" thread-id "model_call_id" model-call-id
                      "knowledge_graph_formation" t "formation_phase" phase)
                 (lambda ()
                   (let ((*conscious-conversation-private-provider-call-p* t))
                     (%conversation-http-model-call-with-retry
                      messages *conscious-recursive-mind-endpoint*
                      *conscious-recursive-mind-model* 0.1d0
                      :tools tools :tool-choice "required"))))))
          (%conversation-append-readable
           "model-response"
           (obj "thread_id" thread-id "model_call_id" model-call-id
                "runtime_revision" *conscious-recursive-mind-runtime-revision*
                "status" "accepted" "content_persisted" t
                "knowledge_graph_formation" t "formation_phase" phase
                "assistant_message" (%conversation-response-message response)
                "usage" (%conversation-response-usage response))
           :caused-by opened-id)
          response)
      (error (condition)
        (multiple-value-bind (code reason status condition-type)
            (%conversation-provider-failure-details condition)
          (%conversation-append-readable
           "model-response"
           (obj "thread_id" thread-id "model_call_id" model-call-id
                "runtime_revision" *conscious-recursive-mind-runtime-revision*
                "status" "failed" "content_persisted" nil
                "knowledge_graph_formation" t "formation_phase" phase
                "error_code" code "reason" reason "http_status" status
                "condition_type" condition-type)
           :caused-by opened-id))
        (error condition))))))

(defun %recursive-kg-formation-provider
    (opened-payload opened-id)
  (let* ((thread-id (format nil "thread:knowledge-graph-formation:~a" opened-id))
         (tools (%recursive-kg-formation-schema))
         (persona (%conversation-persona-profile))
         (messages
           (list
            (obj "role" "system" "content"
                 "Form a small generic knowledge graph from the supplied original utterances. Generated summaries and retrieval cues are navigation only and are not evidence. Use only the fixed ontology and obey every typed predicate signature. Extract independently useful entities and relationships, not every noun. Preserve whether each proposition is an assertion, question, hypothesis, intention, reported speech, joke, or retrieval outcome; preserve polarity and attribution. A prior-agent utterance directly establishes only what the agent said, did, intended, or experienced; it does not establish the truth of an external or operator-personal claim unless the operator confirms it or exact prior graph evidence is supplied. Prefer the operator's explicit correction over a conflicting prior-agent assertion. When a correction explicitly identifies one supplied existing entity, use REVISE_EXISTING rather than creating a duplicate. Naming inspiration is a relationship to a distinct entity, not an alias unless the operator actually uses that name for the subject. The speaker_id operator is one stable participant across every episode: classify an entity representing that speaker as operator. The configured speaking persona is likewise one stable participant: classify an entity representing that agent as active-persona. If the eligible nodes contain either participant role, use its exact node ID; never create a second node for that participant. Do not apply these participant classifications to anyone else. Every relationship must cite an exact source_id and verbatim quote. Do not infer event time from the episode capture time: use null unless the quoted utterance states a supported time. Give independently useful entities a small set of broad, grounded classifications that aid later retrieval (for example organism, animal and pet; or health, medical and condition), but do not invent unstated traits. Domain kinds such as dog, plant, rain, or color are classifications rather than invented upper types. Reuse identity only with an exact supplied node ID. Labels and similarity are never identity authority. Call write-knowledge-graph-formation exactly once; the runtime owns IDs, persona, disclosure and provenance.")
            (obj "role" "user" "content"
                 (shasht:write-json
                  (obj "persona_policy"
                       (obj "persona_id" (gethash "persona_id" persona)
                            "revision" (gethash "revision" persona)
                            "fingerprint" (gethash "fingerprint" persona)
                            "identity" (gethash "identity" persona)
                            "voice" (gethash "voice" persona))
                       "ontology" (knowledge-graph-ontology-provider-descriptor)
                       "formation_source" opened-payload)
                  nil)))))
    (handler-case
        (let ((formation-response
                (%recursive-kg-model-call
                 messages tools opened-id thread-id "formation"
                 *conscious-recursive-kg-formation-max-output-tokens*)))
          (when (and (symbolp formation-response)
                     (member formation-response '(:preempted :paused-budget)))
            (return-from %recursive-kg-formation-provider formation-response))
          (let* ((proposal
                   (%recursive-kg-formation-proposal
                    (%conversation-response-message formation-response)
                    opened-payload))
                 (claims (%recursive-kg-evidence-claims proposal))
                 (review-tools
                   (%recursive-kg-evidence-review-schema (length claims)))
                 (review-messages
                   (list
                    (obj "role" "system" "content"
                         "Audit every proposed graph claim only against the original sealed utterances. Confirm exact quote, speaker/referent binding, scope, polarity, attribution and temporal meaning. The source speaker_id operator denotes one stable operator across episodes; operator classification is valid only for the entity representing that speaker. The configured speaking persona is one stable active-persona. A prior-agent utterance directly establishes only the agent's own statement, action, intention, or experience; an unconfirmed assistant recollection does not directly establish an external or operator-personal fact. The operator's explicit assertion or correction can directly establish the operator-personal fact it states and supersedes a conflicting assistant assertion. A relationship is direct only when the authoritative quoted utterance states it faithfully. Naming inspiration does not make the inspiration an alias. Questions, hypotheses, jokes, reports and failed retrievals must not become positive assertions. Episode capture time is not event time. Co-occurrence, topical association, authorship, tool use, or plausibility alone is not direct evidence. Use EXACT_PRIOR_GRAPH only for an exact supplied existing identity, never similarity. Mark plausible but unstated claims REASONABLE_INFERENCE and absent claims UNSUPPORTED. Return exactly one review for every claim_ref through review-knowledge-graph-evidence.")
                    (obj "role" "user" "content"
                         (shasht:write-json
                          (obj "persona_policy"
                               (obj "persona_id" (gethash "persona_id" persona)
                                    "revision" (gethash "revision" persona)
                                    "fingerprint" (gethash "fingerprint" persona)
                                    "identity" (gethash "identity" persona)
                                    "voice" (gethash "voice" persona))
                               "sealed_evidence"
                               (gethash "evidence_records" opened-payload)
                               "eligible_existing_nodes"
                               (gethash "eligible_existing_nodes" opened-payload)
                               "ontology"
                               (knowledge-graph-ontology-provider-descriptor)
                               "proposal" proposal "claims" claims)
                          nil))))
                 (review-response
                   (%recursive-kg-model-call
                    review-messages review-tools opened-id thread-id
                    "evidence-review"
                    *conscious-recursive-kg-evidence-review-max-output-tokens*)))
            (when (and (symbolp review-response)
                       (member review-response '(:preempted :paused-budget)))
              (return-from %recursive-kg-formation-provider review-response))
            (%recursive-kg-reviewed-proposal
             proposal
             (%recursive-kg-evidence-review
              (%conversation-response-message review-response) proposal)
             opened-payload)))
      (error (condition)
        (error "KG2 provider boundary failed: ~a" condition)))))

(defun conscious-recursive-knowledge-graph-formation-step
    (event-backend derived-backend agent-id persona-id)
  "Compose one pre-synchronized KG2 formation quantum."
  (when (%recursive-operator-pending-p)
    (return-from conscious-recursive-knowledge-graph-formation-step
      (obj "schema_version" 1 "status" "preempted")))
  (let ((pre-sync
          (knowledge-graph-formation-synchronize
           event-backend derived-backend agent-id persona-id)))
    ;; Drain one historical sealed receipt before opening new semantic work.
    (when (plusp (gethash "formation_event_count" pre-sync 0))
      (return-from conscious-recursive-knowledge-graph-formation-step
        (obj "schema_version" 1 "status" "synchronized"
             "synchronization" pre-sync)))
    (let* ((boundary (storage-authority-boundary
                      event-backend :agent-id agent-id))
           (events (%recursive-thread-events)))
      (knowledge-graph-formation-owner-step
       events agent-id persona-id
       (lambda (prior selected-agent selected-persona)
         (knowledge-graph-formation-select-episode-source
          prior selected-agent selected-persona
          (lambda (episode-event)
            (knowledge-graph-formation-current-candidates
             derived-backend selected-agent selected-persona
             (%recursive-kg-formation-cues episode-event)
             :event-storage-id (gethash "storage_id" boundary)))))
       #'%recursive-kg-formation-provider
       (lambda (type payload caused-by)
         (nth-value 1
                    (%conversation-append-readable
                     type payload :caused-by caused-by)))
       (lambda ()
         (knowledge-graph-formation-synchronize
          event-backend derived-backend agent-id persona-id))
       :operator-pending-p #'%recursive-operator-pending-p
       :budget-admissible-p (lambda () t)))))
