;;;; Private single-operator episode authority and event-ledger composition.
(in-package :agent)

(export '(conscious-context-graph-formation-step conscious-context-graph-search
          conscious-context-graph-confirmation-candidate
          conscious-context-graph-proposal-result
          conscious-context-graph-attention-context
          conscious-context-graph-coverage-inspect))

(defvar *conscious-context-graph-runtime* nil)
(defvar *conscious-context-graph-formation-owner* nil)
(defvar *conscious-context-graph-runtime-key* nil)
(defvar *conscious-context-graph-lock* (bt:make-lock "reviewed-context-graph"))
(defvar *conscious-context-graph-formation-lock*
  (bt:make-lock "reviewed-context-graph-formation")
  "Serialize formation owners while allowing stable graph reads during provider IO.")
(defvar *conscious-context-graph-semantic-vectors* (make-hash-table :test #'equal))
(defvar *conscious-context-graph-proposal-outcomes*
  (make-hash-table :test #'eql)
  "Rebuildable per-generation outcomes, including precise closed rejections.")
(defvar *conscious-context-graph-quarantined-opening-ids*
  (make-hash-table :test #'eql)
  "Opening receipts rejected by current replay authority.  The event log is
immutable; this rebuildable set prevents one obsolete opening and its dependent
receipts from jamming unrelated graph work.  Fresh openings remain strict.")
(defvar *conscious-context-graph-reported-quarantine-ids*
  (make-hash-table :test #'eql)
  "Process-local suppression for repeated diagnostics across cache rebuilds.")
(defvar *conscious-context-graph-semantic-similarity-fn* nil
  "Test adapter: (query descriptor-document) -> similarity in [0,1].")
(defvar *conscious-context-graph-semantic-query-embed-fn*
  #'embed-retrieval-query)
(defvar *conscious-context-graph-semantic-documents-embed-fn*
  #'embed-retrieval-documents)
(defvar *conscious-context-graph-now-fn* #'get-universal-time
  "Clock seam for durable retry qualification; production uses universal time.")
(defvar *conscious-context-graph-provider-profiles-path* nil
  "Optional contained-launch/test override for the provider profile document.
NIL resolves the active configuration relative to the PAI ASDF system.")
(defun %ccg-configured-provider-min-interval-seconds ()
  "Read the rebuild-only provider pacing interval.  The default preserves
normal runtime latency; the CLI opts isolated rebuild workers into pacing."
  (let ((raw (uiop:getenv "PAI_CONTEXT_GRAPH_PROVIDER_MIN_INTERVAL_SECONDS")))
    (if (or (null raw) (zerop (length raw))) 0
        (handler-case
            (let ((value (parse-integer raw :junk-allowed nil)))
              (unless (<= 0 value 300) (error "interval out of range"))
              value)
          (error ()
            (error "Invalid PAI_CONTEXT_GRAPH_PROVIDER_MIN_INTERVAL_SECONDS"))))))
(defun %ccg-configured-prior-exposure-microusd ()
  "Operator-supplied exposure from predecessor graph generations.  Refuse a
malformed value rather than accidentally restoring the complete allowance."
  (let ((raw (uiop:getenv "PAI_CONTEXT_GRAPH_PRIOR_EXPOSURE_MICROUSD")))
    (if (or (null raw) (zerop (length raw))) 0
        (handler-case
            (let ((value (parse-integer raw :junk-allowed nil)))
              (unless (<= 0 value) (error "negative exposure"))
              value)
          (error () (error "Invalid PAI_CONTEXT_GRAPH_PRIOR_EXPOSURE_MICROUSD"))))))

(defun %ccg-configured-generation-budget-microusd ()
  "Read the operator-selected cumulative ceiling, preserving the qualified
default when no override is present."
  (let ((raw (uiop:getenv "PAI_CONTEXT_GRAPH_GENERATION_BUDGET_MICROUSD")))
    (if (or (null raw) (zerop (length raw))) 2500000
        (handler-case
            (let ((value (parse-integer raw :junk-allowed nil)))
              (unless (<= 1 value 1000000000) (error "budget out of range"))
              value)
          (error () (error "Invalid PAI_CONTEXT_GRAPH_GENERATION_BUDGET_MICROUSD"))))))

(defun %ccg-configured-request-ceiling-microusd ()
  "Read the per-request guard, preserving the qualified default.

Every other graph budget knob here is operator-configurable; this one was a
bare literal, which made it a silent gate rather than a stated policy. A
request priced above it is refused before the provider is called, so on an
instance whose evidence payloads grow with its corpus the observable effect
was an empty graph and a rescheduling retry loop, not a visible budget
decision."
  (let ((raw (uiop:getenv "PAI_CONTEXT_GRAPH_REQUEST_CEILING_MICROUSD")))
    (if (or (null raw) (zerop (length raw))) 60000
        (handler-case
            (let ((value (parse-integer raw :junk-allowed nil)))
              (unless (<= 1 value 1000000000) (error "ceiling out of range"))
              value)
          (error ()
            (error "Invalid PAI_CONTEXT_GRAPH_REQUEST_CEILING_MICROUSD"))))))

(defparameter *conscious-context-graph-generation-budget-microusd*
  (%ccg-configured-generation-budget-microusd)
  "Cumulative durable exposure ceiling approved for the dev rebuild.")
(defparameter *conscious-context-graph-prior-exposure-microusd*
  (%ccg-configured-prior-exposure-microusd)
  "Charged or outcome-unknown exposure from predecessor graph generations.")
(defparameter *conscious-context-graph-request-ceiling-microusd*
  (%ccg-configured-request-ceiling-microusd)
  "Absolute per-request guard; exact request reservations are normally smaller.
Override with PAI_CONTEXT_GRAPH_REQUEST_CEILING_MICROUSD on an instance whose
evidence payloads have outgrown the default.")
(defparameter *conscious-context-graph-budget-authorization-id*
  (let ((raw (uiop:getenv "PAI_CONTEXT_GRAPH_BUDGET_AUTHORIZATION_ID")))
    (cond ((or (null raw) (zerop (length raw))) nil)
          ((and (<= 1 (length raw) 128)
                (every (lambda (character)
                         (or (alphanumericp character) (find character "-_.:"))) raw)) raw)
          (t (error "Invalid PAI_CONTEXT_GRAPH_BUDGET_AUTHORIZATION_ID"))))
  "Explicit shared cumulative authorization. NIL keeps fresh lab execution disabled.")
(defparameter *conscious-context-graph-provider-min-interval-seconds*
  (%ccg-configured-provider-min-interval-seconds)
  "Minimum interval between provider request starts for this process.")
(defvar *conscious-context-graph-last-provider-call-at* nil)

(defun %ccg-await-provider-call-slot ()
  "Pace provider request starts without altering reservations or durable state."
  (when (and (plusp *conscious-context-graph-provider-min-interval-seconds*)
             *conscious-context-graph-last-provider-call-at*)
    (loop for remaining =
            (- (+ *conscious-context-graph-last-provider-call-at*
                  *conscious-context-graph-provider-min-interval-seconds*)
               (get-universal-time))
          while (plusp remaining)
          do (sleep (min 1 remaining))))
  (setf *conscious-context-graph-last-provider-call-at* (get-universal-time)))

(defun %ccg-configured-runtime-profile ()
  "Select one atomic owner/protocol pair. A typo must never partially promote
formation or silently reuse the predecessor task namespace."
  (let ((value (or (uiop:getenv "PAI_CONTEXT_GRAPH_RUNTIME_PROFILE")
                   "direct-v6")))
    (unless (member value '("direct-v6" "reviewed-inference-v7"
                            "reviewed-inference-v8"
                            "reviewed-inference-v9")
                    :test #'equal)
      (error "Invalid PAI_CONTEXT_GRAPH_RUNTIME_PROFILE"))
    value))

(defparameter *conscious-context-graph-runtime-profile*
  (%ccg-configured-runtime-profile))
(defparameter *conscious-context-graph-owner-generation*
  (cond ((equal *conscious-context-graph-runtime-profile* "reviewed-inference-v9")
         "identity-formation-owner-v9")
        ((equal *conscious-context-graph-runtime-profile* "reviewed-inference-v8")
         "identity-formation-owner-v8")
        ((equal *conscious-context-graph-runtime-profile* "reviewed-inference-v7")
         "identity-formation-owner-v7")
        (t "identity-formation-owner-v6")))
(defparameter *conscious-context-graph-formation-protocol*
  (cond ((equal *conscious-context-graph-runtime-profile* "reviewed-inference-v9")
         "identity-formation-v14")
        ((equal *conscious-context-graph-runtime-profile* "reviewed-inference-v8")
         "identity-formation-v13")
        ((equal *conscious-context-graph-runtime-profile* "reviewed-inference-v7")
         "identity-formation-v10")
        (t "identity-formation-v9")))

(defun %ccg-runtime-ontology-revision ()
  (if (equal *conscious-context-graph-runtime-profile*
             "reviewed-inference-v9")
      *knowledge-graph-family-ontology-revision*
      *knowledge-graph-ontology-revision*))

(defun %ccg-create-owner (graph agent-id persona-id)
  (let ((revision (%ccg-runtime-ontology-revision)))
    (cond
    ((and (equal *conscious-context-graph-owner-generation*
                 "identity-formation-owner-v6")
          (equal *conscious-context-graph-formation-protocol*
                 "identity-formation-v9"))
     (pai.context-graph::%cgf-owner-create-v6
       graph agent-id persona-id revision))
    ((and (equal *conscious-context-graph-owner-generation*
                 "identity-formation-owner-v7")
          (equal *conscious-context-graph-formation-protocol*
                 "identity-formation-v10"))
     (pai.context-graph::%cgf-owner-create-v7
       graph agent-id persona-id revision))
    ((and (equal *conscious-context-graph-owner-generation*
                 "identity-formation-owner-v8")
          (equal *conscious-context-graph-formation-protocol*
                 "identity-formation-v13"))
     (pai.context-graph::%cgf-owner-create-v8
       graph agent-id persona-id revision))
    ((and (equal *conscious-context-graph-owner-generation*
                 "identity-formation-owner-v9")
          (equal *conscious-context-graph-formation-protocol*
                 "identity-formation-v14"))
     (pai.context-graph::%cgf-owner-create-v9
      graph agent-id persona-id revision))
    (t (error "Context graph runtime owner/protocol pair is invalid")))))

(defun %ccg-runtime-cache-key (boundary agent-id persona-id)
  (list (gethash "storage_id" boundary) agent-id persona-id
        *conscious-context-graph-runtime-profile*
        *conscious-context-graph-owner-generation*
        *conscious-context-graph-formation-protocol*))

(defun %ccg-participants (agent-id persona-id)
  ;; The sealed conversation owner binds these roles through authenticated
  ;; ledger partition + persona metadata, not names found in utterance text.
  (map 'vector
       (lambda (role kind)
         (let ((id (pai.context-graph::%cg-sha256 "private-conversation-principal-v1" agent-id persona-id role)))
           (obj "role" role "speaker_id" (concatenate 'string "principal:" id)
                "principal_id" (concatenate 'string "principal:" id)
                "identity_binding_id" (concatenate 'string "conversation-binding:" id)
                "local_ref" (concatenate 'string "runtime:" role)
                "entity_id" (concatenate 'string "participant:" id)
                "kind" kind "label" role "aliases" #())))
       #("operator" "active-persona") #("person" "agent")))

(defun %ccg-proposal-context
    (graph proposal-event event-index agent-id persona-id)
  "Build authority context from authenticated observations in one live turn.

The operator message, completed evidence-tool results, and the exact model call
that authored the proposal are eligible sources.  Agent-authored proposal text
is an inference premise, never direct evidence."
  (let* ((proposal-payload (gethash "payload" proposal-event))
         (source-event-id (and (hash-table-p proposal-payload)
                               (gethash "source_user_event_id"
                                        proposal-payload)))
         (event (and (integerp source-event-id)
                     (gethash source-event-id event-index)))
         (payload (and event (gethash "payload" event)))
         (metadata (and (hash-table-p payload)
                        (gethash "metadata" payload)))
         (text (and (hash-table-p payload) (gethash "text" payload)))
         (participants (%ccg-participants agent-id persona-id))
         (operator (aref participants 0))
         (persona (aref participants 1))
         (proposal-event-id (gethash "id" proposal-event))
         (model-call-id (and (hash-table-p proposal-payload)
                             (gethash "model_call_id" proposal-payload)))
         (tool-call-id (and (hash-table-p proposal-payload)
                            (gethash "tool_call_id" proposal-payload))))
    (unless (and event
                 (string= "user-message" (gethash "type" event ""))
                 (%conversation-episode-user-message-p
                  event agent-id persona-id)
                 (hash-table-p metadata)
                 (stringp text) (plusp (length text))
                 (<= (length text) 30000))
      (error "Conversation graph proposal source is not an authenticated operator message"))
    (let* ((source-id (format nil "event:~d" source-event-id))
           (digest (pai.context-graph::%cg-sha256 text))
           (source
             (obj "source_id" source-id
                  "speaker_id" (gethash "principal_id" operator)
                  "kind" "original-utterance"
                  "timestamp" (gethash "timestamp" event)
                  "text" text "text_sha256" digest
                  "identity"
                  (obj "principal_id" (gethash "principal_id" operator)
                       "binding_id" (gethash "identity_binding_id" operator)
                       "conversation_id"
                       (format nil "private-conversation:~a:~a"
                               agent-id persona-id)
                       "role" "operator")
                  "resource_ref"
                  (obj "store" "event" "resource_id" source-id
                       "version_id" digest "component" "content")))
           (additional-sources nil)
           (additional-characters 0)
           (access
             (obj "schema_version" 1
                  "executor_principal_id" (gethash "principal_id" persona)
                  "authority_principal_id" (gethash "principal_id" operator)
                  "recipient_principal_id" :null
                  "recipient_binding_id" :null
                  "recipient_set_digest" :null
                  "task_id" :null "purpose" "private-planning"
                  "channel_id" "private-runtime" "action" "derive"
                  "partition" (obj "agent_id" agent-id
                                   "persona_id" persona-id)
                  "grant_ids" #() "now_utc" (get-universal-time)
                  "policy_epoch" 1))
           (episode-id (format nil "conversation-event:~d" source-event-id))
           (model-event
             (loop for row being the hash-values of event-index
                   for row-payload = (gethash "payload" row)
                   when (and (< source-event-id (gethash "id" row -1)
                              proposal-event-id)
                             (eql source-event-id (gethash "caused_by" row))
                             (string= "model-response" (gethash "type" row ""))
                             (hash-table-p row-payload)
                             (equal model-call-id
                                    (gethash "model_call_id" row-payload)))
                     return row))
           (model-arguments
             (and model-event
                  (let* ((response-payload (gethash "payload" model-event))
                         (message (gethash "assistant_message"
                                           response-payload))
                         (calls (and (hash-table-p message)
                                     (gethash "tool_calls" message))))
                    (and (vectorp calls)
                         (loop for call across calls
                               for function = (and (hash-table-p call)
                                                   (gethash "function" call))
                               when (and (equal tool-call-id
                                                (gethash "id" call))
                                         (hash-table-p function)
                                         (string= "propose-graph-update"
                                                  (gethash "name" function "")))
                                 return (gethash "arguments" function))))))
           (sources nil)
           (tool-events
             (sort
              (loop for candidate being the hash-values of event-index
                    for candidate-payload = (gethash "payload" candidate)
                    for content = (and (hash-table-p candidate-payload)
                                       (gethash "content" candidate-payload))
                    when (and (< source-event-id
                                 (gethash "id" candidate -1)
                                 proposal-event-id)
                              (eql source-event-id
                                   (gethash "caused_by" candidate))
                              (string= "recursive-tool-result"
                                       (gethash "type" candidate ""))
                              (hash-table-p candidate-payload)
                              (string= "executed"
                                       (gethash "execution_status"
                                                candidate-payload ""))
                              (member (gethash "tool_name"
                                               candidate-payload "")
                                      *conscious-recursive-conversational-evidence-tools*
                                      :test #'string=)
                              (stringp content)
                              (plusp (length content))
                              (<= (length content) 12000))
                      collect candidate)
              #'> :key (lambda (row) (gethash "id" row)))))
      ;; External tool output is a runtime-authenticated observation.  Keep a
      ;; small newest-first envelope; arbitrary model prose cannot manufacture
      ;; this source kind.
      (dolist (row (subseq tool-events 0 (min 8 (length tool-events))))
        (let* ((row-payload (gethash "payload" row))
               (content (gethash "content" row-payload))
               (tool-name (gethash "tool_name" row-payload))
               (principal-id
                 (concatenate
                  'string "tool:"
                  (pai.context-graph::%cg-sha256
                   "conversation-tool-principal-v1" tool-name))))
          (when (<= (+ additional-characters (length content)) 60000)
            (incf additional-characters (length content))
            (push
             (obj "source_id" (format nil "event:~d" (gethash "id" row))
                  "speaker_id" principal-id "kind" "tool-observation"
                  "timestamp" (gethash "timestamp" row)
                  "text" content
                  "text_sha256" (pai.context-graph::%cg-sha256 content)
                  "identity"
                  (obj "principal_id" principal-id "binding_id" :null
                       "conversation_id"
                       (format nil "private-conversation:~a:~a"
                               agent-id persona-id)
                       "role" "other")
                  "resource_ref"
                  (obj "store" "event"
                       "resource_id" (format nil "event:~d"
                                             (gethash "id" row))
                       "version_id" (pai.context-graph::%cg-sha256 content)
                       "component" "content"))
             additional-sources))))
      ;; The exact proposal arguments are an authenticated active-persona
      ;; observation.  They may support an inference, but never a direct fact.
      (when (and model-event (stringp model-arguments)
                 (plusp (length model-arguments))
                 (<= (length model-arguments) 12000))
        (push
         (obj "source_id" (format nil "event:~d" (gethash "id" model-event))
              "speaker_id" (gethash "principal_id" persona)
              "kind" "prior-agent-utterance"
              "timestamp" (gethash "timestamp" model-event)
              "text" model-arguments
              "text_sha256" (pai.context-graph::%cg-sha256 model-arguments)
              "identity"
              (obj "principal_id" (gethash "principal_id" persona)
                   "binding_id" (gethash "identity_binding_id" persona)
                   "conversation_id"
                   (format nil "private-conversation:~a:~a"
                           agent-id persona-id)
                   "role" "active-persona")
              "resource_ref"
              (obj "store" "event"
                   "resource_id" (format nil "event:~d"
                                         (gethash "id" model-event))
                   "version_id" (pai.context-graph::%cg-sha256 model-arguments)
                   "component" "content"))
         additional-sources))
      (setf sources
            (coerce (cons source (nreverse additional-sources)) 'vector))
      (let ((snapshot
             (pai.context-graph::%cg-authority-digest
              "current-private-conversation-access-v1"
              (vector (obj "source_user_event_id" source-event-id
                           "agent_id" agent-id "persona_id" persona-id)
                      sources participants access))))
        (pai.context-graph::context-graph-runtime-context
         graph agent-id persona-id episode-id sources participants access
         snapshot)))))

(defun %ccg-conversation-raw-proposal (arguments revision context)
  "Expand the deliberately small native-tool shape into authority proposal v4."
  (let ((sources (gethash "sources" (gethash "source_packet" context))))
    (obj
     "schema_version" 4 "ontology_revision" revision
     "entities" (pai.context-graph::%cg-detach
                 (gethash "entities" arguments #()))
     "relationships"
     (map 'vector
          (lambda (row)
            (let* ((quote (gethash "quote" row))
                   (source
                     (find-if (lambda (candidate)
                                (search quote (gethash "text" candidate)
                                        :test #'char=))
                              sources))
                   (kind (and source (gethash "kind" source))))
              (unless source
                (error "Claim quote is not present in an authenticated current-turn operator, tool, or agent observation"))
              (when (and (string= "direct"
                                  (gethash "evidence_status" row))
                         (string= "prior-agent-utterance" kind))
                (error "An agent-authored observation must be inference, not direct evidence"))
              (obj "subject_ref" (gethash "subject_ref" row)
                 "predicate" (gethash "predicate" row)
                 "object_ref" (gethash "object_ref" row)
                 "relationship_action" "ASSERT"
                 "fact" (gethash "fact" row)
                 "grounding"
                 (obj "schema_version" 2 "scope" "assertion"
                      "polarity" (gethash "polarity" row)
                      "attributed_to_ref"
                      (if (string= "original-utterance" kind)
                          "runtime:operator" "runtime:active-persona")
                      "evidence"
                      (vector (obj "source_id" (gethash "source_id" source)
                                   "quote" (gethash "quote" row))))
                 "temporal"
                 (obj "schema_version" 1
                      "character" (gethash "temporal_character" row)
                      "occurred_at" :null "valid_from" :null
                      "valid_until" :null)
                 "evidence_status" (gethash "evidence_status" row)
                 "evidence_note" (gethash "evidence_note" row))))
          (gethash "relationships" arguments #()))
     "entity_revisions" #())))

(defun %ccg-source-context (graph episode-event-id now event-index agent-id persona-id)
  (let* ((episode (gethash episode-event-id event-index))
         (payload (and episode (gethash "payload" episode)))
         (participants (%ccg-participants agent-id persona-id))
         (operator (aref participants 0)) (persona (aref participants 1))
         (sources nil) (chars 0))
    (unless (and episode (equal "conversation-episode-sealed" (gethash "type" episode))
                 (%kgfs-event-in-partition-p episode agent-id persona-id)
                 (conversation-episode-sealed-payload-valid-p payload)
                 (<= 1 (length (gethash "source_event_ids" payload)) 128))
      (error "Runtime graph episode is not an authenticated sealed source"))
    (loop for id across (gethash "source_event_ids" payload)
          for event = (gethash id event-index) do
      (unless (and event (< id episode-event-id)
                   (%conversation-episode-public-message-p event agent-id persona-id))
        (error "Runtime graph source is missing, foreign or not a public conversation receipt"))
      (let* ((body (gethash "payload" event))
             (metadata (gethash "metadata" body))
             (text (gethash "text" body))
             (role (%conversation-episode-message-role
                    event agent-id persona-id))
             (user-p (string= role "operator"))
             (historical-assistant-p (string= role "historical-assistant"))
             (source-agent-id (and historical-assistant-p
                                   (gethash "source_agent_id" metadata)))
             (source-principal-id
               (and historical-assistant-p
                    (concatenate
                     'string "source-agent:"
                     (pai.context-graph::%cg-sha256
                      "historical-source-agent-v1" source-agent-id))))
             (participant
               (cond (user-p operator)
                     (historical-assistant-p
                      (obj "role" "other" "speaker_id" source-principal-id
                           "principal_id" source-principal-id
                           "identity_binding_id" :null))
                     (t persona)))
             (digest (pai.context-graph::%cg-sha256 text))
             (source-id
               (if (string= "historical-agent-migration-v1"
                            (gethash "source" metadata ""))
                   (format nil "migrated-event:~a:~d"
                           (gethash "source_agent_id" metadata)
                           (gethash "source_event_id" metadata))
                   (format nil "event:~d" id)))
             (conversation-id
               (if historical-assistant-p
                   (format nil "historical-conversation:~a" source-agent-id)
                   (format nil "private-conversation:~a:~a"
                           agent-id persona-id))))
        ;; A prior-agent utterance alone cannot authorize an operator-personal
        ;; assertion.  Exclude an anomalously large one wholesale rather than
        ;; truncating it into false exact evidence or blocking every later
        ;; episode.  Original operator evidence is never omitted here.
        (unless (and (not user-p) (> (length text) 30000))
          (incf chars (length text))
          (unless (and (<= (length text) 30000) (<= chars 70000))
            (error "Runtime graph operator source exceeds the evidence envelope"))
          (push (obj "source_id" source-id "speaker_id" (gethash "principal_id" participant)
                     "kind" (if user-p "original-utterance" "prior-agent-utterance")
                     "timestamp" (gethash "timestamp" event) "text" text "text_sha256" digest
                     "identity" (obj "principal_id" (gethash "principal_id" participant)
                                     "binding_id" (gethash "identity_binding_id" participant)
                                     "conversation_id" conversation-id
                                     "role" (gethash "role" participant))
                     "resource_ref" (obj "store" "event" "resource_id" source-id
                                         "version_id" digest "component" "content")) sources))))
    (setf sources (coerce (nreverse sources) 'vector))
    (let* ((access (obj "schema_version" 1 "executor_principal_id" (gethash "principal_id" persona)
                       "authority_principal_id" (gethash "principal_id" operator)
                       "recipient_principal_id" :null "recipient_binding_id" :null "recipient_set_digest" :null
                       "task_id" :null "purpose" "private-planning" "channel_id" "private-runtime" "action" "derive"
                       "partition" (obj "agent_id" agent-id "persona_id" persona-id)
                       "grant_ids" #() "now_utc" now "policy_epoch" 1))
           (snapshot (pai.context-graph::%cg-authority-digest "sealed-private-conversation-access-v1"
                       ;; Bind the authenticated authority root, not arbitrary
                       ;; navigation/transport fields in the native-JSON event.
                       ;; Native NIL nulls and float metadata are not canonical
                       ;; authority values and do not establish source access.
                       (vector (obj "episode_event_id" episode-event-id "agent_id" agent-id "persona_id" persona-id
                                    "episode_id" (gethash "episode_id" payload)
                                    "source_event_ids" (gethash "source_event_ids" payload))
                               sources participants access))))
      (pai.context-graph::context-graph-runtime-context graph agent-id persona-id
        (gethash "episode_id" payload) sources participants access snapshot))))

(defun %ccg-ontology-document ()
  (let* ((path (asdf:system-relative-pathname :pai "config/context-graph-upper-ontology-v1.2.json"))
         (document (with-open-file (stream path :external-format :utf-8)
                     (shasht:read-json stream))))
    (unless (equal *knowledge-graph-ontology-revision* (gethash "ontology_revision" document))
      (error "Context graph ontology revision mismatch"))
    document))

(defun %ccg-ontology ()
  ;; Same fixed signatures as the qualified lab, loaded from source policy.
  (let ((descriptor
          (knowledge-graph-ontology-provider-descriptor
           (%ccg-runtime-ontology-revision))))
    (obj "entity_types" (gethash "entity_types" descriptor)
         "edge_types" (map 'vector (lambda (p)
                            (obj "name" (gethash "predicate" p)
                                 "subject_types" (gethash "subject_types" p)
                                 "object_types" (gethash "object_types" p)))
                          (gethash "predicate_signatures" descriptor)))))

(defun %ccg-descriptor-guide
    (&optional (revision (%ccg-runtime-ontology-revision)))
  (let ((guide
          (pai.context-graph::%cg-detach
           (gethash "entity_types"
                    (gethash "ontology" (%ccg-ontology-document))))))
    (if (equal revision *knowledge-graph-family-ontology-revision*)
        (concatenate
         'vector guide
         (vector
          (obj "name" "attribute_value"
               "definition"
               "An explicit scalar or categorical value attached to an entity by a typed attribute predicate."
               "inclusion_rule"
               "Use only for a source-stated value such as an age or gender value that participates as the object of has_age or has_gender."
               "exclusion_rule"
               "Do not use for people, organisms, ordinary concepts, inferred values, computed ages, or free-standing conversational text.")))
        guide)))

(defun %ccg-apply-confirmation-resolution
    (runtime event event-index agent-id persona-id)
  "Fold one authenticated short answer into the rebuildable graph projection."
  (unless (string= "context-graph-confirmation-resolved"
                   (gethash "type" event ""))
    (return-from %ccg-apply-confirmation-resolution nil))
  (let* ((payload (gethash "payload" event))
         (event-id (gethash "id" event))
         (source-id (and (hash-table-p payload)
                         (gethash "source_user_event_id" payload)))
         (request-id (and (hash-table-p payload)
                          (gethash "request_event_id" payload)))
         (source (and (integerp source-id) (gethash source-id event-index)))
         (request (and (integerp request-id)
                       (gethash request-id event-index)))
         (source-payload (and source (%recursive-event-payload source)))
         (request-payload (and request (%recursive-event-payload request)))
         (source-metadata
           (and (hash-table-p source-payload)
                (gethash "metadata" source-payload)))
         (events
           (sort (loop for row being the hash-values of event-index collect row)
                 #'< :key (lambda (row) (gethash "id" row))))
         (publication
           (and (hash-table-p request)
                (hash-table-p request-payload)
                (stringp (gethash "statement" request-payload))
                (%recursive-confirmation-published-event
                 events request source-id)))
         (graph (pai.context-graph::context-graph-runtime-graph runtime))
         (fact
           (and (hash-table-p payload)
                (gethash (gethash "fact_id" payload)
                         (pai.context-graph::context-graph-facts graph)))))
    (unless
        (and (hash-table-p payload)
             (equal
              '("decision" "fact_id" "fact_identity_sha256"
                "ontology_revision" "request_event_id"
                "request_root_event_id" "resolved_at" "schema_version"
                "source_quote" "source_user_event_id")
              (%recursive-object-keys payload))
             (eql 1 (gethash "schema_version" payload))
             (integerp event-id)
             (integerp source-id) (integerp request-id)
             (< request-id source-id event-id)
             (eql source-id (gethash "caused_by" event))
             (equal agent-id (gethash "agent_id" event))
             (hash-table-p source)
             (hash-table-p request)
             (equal agent-id (gethash "agent_id" source))
             (equal agent-id (gethash "agent_id" request))
             (string= "user-message" (gethash "type" source ""))
             (%recursive-source-p source "recursive-mind-v1")
             (hash-table-p source-payload)
             (hash-table-p source-metadata)
             (equal persona-id (gethash "persona_id" source-metadata))
             (equal (gethash "source_quote" payload)
                    (gethash "text" source-payload))
             (equal (gethash "decision" payload)
                    (%recursive-graph-confirmation-decision
                     (gethash "text" source-payload)))
             (string= "context-graph-confirmation-requested"
                      (gethash "type" request ""))
             (hash-table-p request-payload)
             (eql (gethash "request_root_event_id" payload)
                  (gethash "caused_by" request))
             (equal (gethash "fact_id" payload)
                    (gethash "fact_id" request-payload))
             (equal (gethash "fact_identity_sha256" payload)
                    (gethash "fact_identity_sha256" request-payload))
             (equal (gethash "ontology_revision" payload)
                    (gethash "ontology_revision" request-payload))
             (equal (gethash "ontology_revision" payload)
                    (pai.context-graph::context-graph-runtime-revision runtime))
             (hash-table-p publication)
             (%conversation-episode-public-message-p
              publication agent-id persona-id)
             (notany
              (lambda (prior)
                (let ((prior-payload (%recursive-event-payload prior)))
                  (and (< (gethash "id" prior -1) event-id)
                       (string= "context-graph-confirmation-resolved"
                                (gethash "type" prior ""))
                       (hash-table-p prior-payload)
                       (eql request-id
                            (gethash "request_event_id" prior-payload)))))
              events)
             (hash-table-p fact)
             (equal "current" (gethash "status" fact))
             (equal "inference" (gethash "evidence_status" fact))
             (equal (gethash "fact_identity_sha256" payload)
                    (gethash "identity_sha256" fact))
             (equal (gethash "statement" request-payload)
                    (gethash "fact" fact))
             (> event-id
                (pai.context-graph::context-graph-through-event-id graph)))
      (error "Graph confirmation resolution violates authenticated authority"))
    (let* ((staged (pai.context-graph::%cg-stage-authority-state graph))
           (staged-fact
             (gethash (gethash "fact_id" payload)
                      (pai.context-graph::context-graph-facts staged)))
           (decision (gethash "decision" payload))
           (application-id (format nil "event:~d" event-id))
           (operator (aref (%ccg-participants agent-id persona-id) 0))
           (text (gethash "text" source-payload))
           (text-digest (pai.context-graph::%cg-sha256 text))
           (source-key (format nil "event:~d" source-id))
           (span
             (obj "source_id" source-key
                  "speaker_id" (gethash "principal_id" operator)
                  "source_kind" "original-utterance"
                  "timestamp" (gethash "timestamp" source)
                  "text_sha256" text-digest
                  "identity"
                  (obj "principal_id" (gethash "principal_id" operator)
                       "binding_id" (gethash "identity_binding_id" operator)
                       "conversation_id"
                       (format nil "private-conversation:~a:~a"
                               agent-id persona-id)
                       "role" "operator")
                  "resource_ref"
                  (obj "store" "event" "resource_id" source-key
                       "version_id" text-digest "component" "content")
                  "quote" text "start_char" 0 "end_char" (length text)
                  "method" "exact"
                  "resolver_revision" "exact-source-span-v1"))
           (evidence
             (obj "source_episode_id"
                  (format nil "confirmation-request:~d" request-id)
                  "application_event_id" event-id "status" "direct"
                  "note" "explicit operator confirmation"
                  "accepted_sources" (vector span)))
           (evidence-digest
             (pai.context-graph::%cg-authority-digest
              "claim-evidence" evidence)))
      (if (string= decision "confirm")
          (progn
            (unless (find evidence-digest
                          (gethash "evidence_digests" staged-fact)
                          :test #'equal)
              (setf (gethash "evidence_records" staged-fact)
                    (concatenate 'vector
                                 (gethash "evidence_records" staged-fact)
                                 (vector evidence))
                    (gethash "evidence_digests" staged-fact)
                    (concatenate 'vector
                                 (gethash "evidence_digests" staged-fact)
                                 (vector evidence-digest))))
            (setf (gethash "evidence_status" staged-fact) "direct"
                  (gethash "accepted_source_ids" staged-fact)
                  (coerce
                   (subseq
                    (remove-duplicates
                     (append (coerce
                              (gethash "accepted_source_ids" staged-fact #())
                              'list)
                             (list source-key))
                     :test #'equal)
                    0
                    (min 4
                         (length
                          (remove-duplicates
                           (append
                            (coerce
                             (gethash "accepted_source_ids" staged-fact #())
                             'list)
                            (list source-key))
                           :test #'equal))))
                   'vector)
                  (gethash "accepted_evidence_digest" staged-fact)
                  (pai.context-graph::%cg-authority-digest
                   "accepted-claim-evidence"
                   (gethash "evidence_records" staged-fact))
                  (gethash "through_event_id" staged-fact) event-id))
          (progn
            (remhash (gethash "identity_key" staged-fact)
                     (pai.context-graph::context-graph-current-triples staged))
            (setf (gethash "status" staged-fact) "retired"
                  (gethash "retired_at" staged-fact)
                  (gethash "resolved_at" payload)
                  (gethash "retired_by_application_event_id" staged-fact)
                  event-id
                  (gethash "replacement_fact_id" staged-fact) :null
                  (gethash "retirement_basis" staged-fact)
                  "operator-confirmation-rejection-v1"
                  (gethash "through_event_id" staged-fact) event-id)))
      (setf
       (gethash application-id
                (pai.context-graph::context-graph-application-receipts staged))
       (obj "application_id" application-id
            "input_digest"
            (pai.context-graph::%cg-authority-digest
             "confirmation-resolution" payload)
            "episode_id" (format nil "confirmation-request:~d" request-id)
            "formation_outcome"
            (if (string= decision "confirm")
                "confirmed-inference" "retired-inference")
            "prepared_authority" (pai.context-graph::%cg-detach payload)
            "review_receipt" :null
            "batch" (obj "decision" decision "fact_id"
                         (gethash "fact_id" payload)))
       (pai.context-graph::context-graph-through-event-id staged) event-id
       (pai.context-graph::context-graph-projection-digest staged)
       (pai.context-graph::%cg-authority-projection-digest
        staged agent-id persona-id))
      (pai.context-graph::%cg-install-staged-state graph staged)
      decision)))

(defun %ccg-apply-conversation-proposal
    (runtime event event-index agent-id persona-id)
  "Fold one native conversational proposal without any provider boundary.

The event is a proposal, not authority by itself.  Invalid or stale proposals
return a closed rejection and leave the projection untouched, so a model can
repair its next call without poisoning replay or failing the conversation."
  (unless (string= "context-graph-update-proposed"
                   (gethash "type" event ""))
    (return-from %ccg-apply-conversation-proposal nil))
  (handler-case
      (let* ((payload (gethash "payload" event))
             (event-id (gethash "id" event))
             (source-id (and (hash-table-p payload)
                             (gethash "source_user_event_id" payload)))
             (arguments (and (hash-table-p payload)
                             (gethash "proposal" payload)))
             (graph (pai.context-graph::context-graph-runtime-graph runtime)))
        (unless
            (and (hash-table-p payload)
                 (equal
                  '("model_call_id" "proposal" "proposed_at"
                    "runtime_revision" "schema_version"
                    "source_user_event_id" "thread_id" "tool_call_id")
                  (%recursive-object-keys payload))
                 (eql 1 (gethash "schema_version" payload))
                 (integerp event-id) (integerp source-id)
                 (< source-id event-id)
                 (eql source-id (gethash "caused_by" event))
                 (equal agent-id (gethash "agent_id" event))
                 (integerp (gethash "proposed_at" payload))
                 (plusp (gethash "proposed_at" payload))
                 (hash-table-p arguments)
                 (equal *conscious-recursive-mind-runtime-revision*
                        (gethash "runtime_revision" payload)))
          (error "Conversation graph proposal violates its event contract"))
        (let* ((context
                 (%ccg-proposal-context
                  graph event event-index agent-id persona-id))
               (revision
                 (pai.context-graph::context-graph-runtime-revision runtime))
               (proposal
                 (%ccg-conversation-raw-proposal
                  arguments revision context))
               (boundary
                 (obj "episode_id" (gethash "episode_id" context)
                      "application_event_id" event-id
                      "opened_boundary_id" source-id
                      "observed_at" (gethash "proposed_at" payload))))
          (pai.context-graph::%cg-apply-authority-episode
           graph boundary proposal context :null)))
    (error (condition)
      (obj "schema_version" 1 "status" "rejected"
           "reason" (subseq (format nil "~a" condition)
                            0 (min 1000
                                   (length (format nil "~a" condition))))))))

(defun %ccg-publish-owner-open-view (runtime owner)
  "Install the generation owner's lock-protected opening table on its facade.

Both objects are rebuildable members of one generation and every caller holds
the context-graph lock.  Keeping one table is therefore safe and avoids an
unbounded deep copy at every synchronization boundary."
  (setf (pai.context-graph::context-graph-runtime-opens runtime)
        (pai.context-graph::cgi-owner-opens owner))
  runtime)

(defun %ccg-sync (event-backend agent-id persona-id)
  "Cache only this generation; cold replay rechecks original source authority.
No derived SQL graph writes occur on this read path. Obsolete openings that no
longer satisfy current replay authority are quarantined with their dependents;
their source tasks are rebuilt only through the ordinary strict append path.
Caller holds lock."
  (let* ((boundary (storage-authority-boundary event-backend :agent-id agent-id))
         (key (%ccg-runtime-cache-key boundary agent-id persona-id))
         (events (%recursive-thread-events)) (index (make-hash-table :test #'eql)))
    (unless (equal key *conscious-context-graph-runtime-key*)
       (setf *conscious-context-graph-runtime* (pai.context-graph:context-graph-runtime-create
                   (%ccg-ontology) (%ccg-runtime-ontology-revision) agent-id persona-id)
            *conscious-context-graph-formation-owner*
              (%ccg-create-owner
               (pai.context-graph::context-graph-runtime-graph
                *conscious-context-graph-runtime*)
               agent-id persona-id)
            *conscious-context-graph-runtime-key* key)
      (clrhash *conscious-context-graph-semantic-vectors*)
      (clrhash *conscious-context-graph-proposal-outcomes*)
      (clrhash *conscious-context-graph-quarantined-opening-ids*))
    (dolist (event events) (setf (gethash (gethash "id" event) index) event))
    (let ((source-fn (lambda (graph episode now) (%ccg-source-context graph episode now index agent-id persona-id))))
      (dolist (event events)
        (when (> (gethash "id" event)
                 (pai.context-graph::cgi-owner-last-id *conscious-context-graph-formation-owner*))
          (let* ((type (gethash "type" event ""))
                 (id (gethash "id" event))
                 (cause (gethash "caused_by" event))
                 (dependent-p
                   (and (integerp cause)
                        (gethash cause
                                 *conscious-context-graph-quarantined-opening-ids*)))
                 (retry-of
                   (when (string= type "context-graph-identity-opened")
                     (handler-case
                         (gethash "retry_of"
                                  (pai.context-graph::%cgro-record event))
                       (error () nil))))
                 (obsolete-retry-p
                   (and (integerp retry-of)
                        (gethash retry-of
                                 *conscious-context-graph-quarantined-opening-ids*))))
            (cond
              ((or dependent-p obsolete-retry-p)
               (when (string= type "context-graph-identity-opened")
                 (setf (gethash id
                                *conscious-context-graph-quarantined-opening-ids*)
                       t)))
              (t
               (handler-case
                   (pai.context-graph::%cgi-owner-consume
                    *conscious-context-graph-formation-owner* event source-fn)
                 (pai.context-graph::context-graph-authority-input-error
                     (condition)
                   ;; Authority remains strict for every newly proposed
                   ;; opening.  This recovery applies only while replaying an
                   ;; immutable historical opening whose sealed source context
                   ;; is no longer admitted by the current projection rules.
                   (if (and (string= type "context-graph-identity-opened")
                            (string=
                             "IDENTITY_OPEN_INVALID"
                             (pai.context-graph::%cg-authority-error-code
                              condition)))
                       (progn
                         (setf (gethash id
                                        *conscious-context-graph-quarantined-opening-ids*)
                               t)
                         (unless (gethash id
                                          *conscious-context-graph-reported-quarantine-ids*)
                           (setf (gethash id
                                          *conscious-context-graph-reported-quarantine-ids*)
                                 t)
                           (format *error-output*
                                   "~&[knowledge graph replay] quarantined obsolete opening ~d; its task will be rebuilt under current authority~%"
                                   id)
                           (finish-output *error-output*)))
                       (error condition)))))))
          (%ccg-apply-confirmation-resolution
           *conscious-context-graph-runtime* event index agent-id persona-id)
          (let ((proposal-outcome
                  (%ccg-apply-conversation-proposal
                   *conscious-context-graph-runtime* event index
                   agent-id persona-id)))
            (when proposal-outcome
              (setf (gethash (gethash "id" event)
                             *conscious-context-graph-proposal-outcomes*)
                    proposal-outcome)))))
      ;; The runtime object is the read facade over the selected owner's
      ;; replacement projection. Other generations remain durable audit evidence
      ;; but do not mutate this graph.
      (setf (pai.context-graph::context-graph-runtime-last-event-id
              *conscious-context-graph-runtime*)
            (pai.context-graph::cgi-owner-last-id *conscious-context-graph-formation-owner*))
      (%ccg-publish-owner-open-view
       *conscious-context-graph-runtime*
       *conscious-context-graph-formation-owner*)
      (values *conscious-context-graph-runtime* source-fn events
              *conscious-context-graph-formation-owner*))))

(defun %ccg-provider-profile ()
  "Graph routing is independent of the conversation model, but shares its ledger."
  (let* ((path (or *conscious-context-graph-provider-profiles-path*
                   (asdf:system-relative-pathname
                    :pai "config/conscious-provider-profiles.json")))
         (document (with-open-file (stream path) (shasht:read-json stream)))
         (experiment-routing (gethash "experiment_routing" document))
         (profile-name
           (and (hash-table-p experiment-routing)
                (gethash "graph_profile" experiment-routing)))
         (profile
           (and (stringp profile-name) (plusp (length profile-name))
                (gethash profile-name (gethash "profiles" document))))
         (routing (and (hash-table-p profile)
                       (gethash "provider_routing" profile)))
         (purposes (and (hash-table-p profile)
                        (gethash "allowed_purposes" profile))))
    (unless (and (hash-table-p profile)
                 (equal "openrouter" (gethash "provider" profile))
                 (stringp (gethash "model" profile))
                 (plusp (length (gethash "model" profile)))
                 (stringp (gethash "endpoint" profile))
                 (plusp (length (gethash "endpoint" profile)))
                 (equal "proposal-only" (gethash "publication_role" profile))
                 (vectorp purposes)
                 (find "knowledge-graph-formation" purposes :test #'equal)
                 (eq t (gethash "requires_native_tool_calls" profile))
                 (hash-table-p routing)
                 (eq t (gethash "zdr" routing))
                 (equal "deny" (gethash "data_collection" routing))
                 (nth-value 1 (gethash "allow_fallbacks" routing))
                 (null (gethash "allow_fallbacks" routing)))
      (error "Configured graph provider profile is missing or unsafe"))
    profile))

(defun %ccg-phase-output-tokens (phase)
  (cond
    ;; Dense fact schemas can consume the entire 4K allowance in reasoning
    ;; before emitting the required tool call.  The reservation gate below
    ;; prices this larger phase-specific ceiling before any private source is
    ;; sent, so the operator's per-request cap remains authoritative.
    ((equal phase "facts") 8192)
    ((or (member phase '("new-identities" "new-identity-groups" "review")
                 :test #'equal)
         (and (stringp phase)
              (eql 0 (search "identity-page-" phase))))
     4096)
    (t 2048)))

(defun %ccg-model-request (phase spec)
  (let* ((name (gethash "tool_name" spec))
         ;; Decode canonical booleans into the transport's native JSON mapping.
         (schema (shasht:read-json
                  (pai.context-graph:context-graph-runtime-json (gethash "schema" spec))))
         (tools (vector (obj "type" "function" "function"
                             (obj "name" name "description" "Return the requested graph phase only."
                                  "parameters" schema))))
         (messages (list (obj "role" "system" "content" (gethash "system" spec))
                         (obj "role" "user" "content"
                              (pai.context-graph:context-graph-runtime-json (gethash "input" spec))))))
    (values messages tools name (%ccg-phase-output-tokens phase))))

(defun %ccg-model-reservation (phase spec digest opened-id maximum-microusd)
  (declare (ignore digest opened-id))
  (let* ((*conscious-conversation-provider-profile* (%ccg-provider-profile))
         (*conscious-recursive-mind-model* (gethash "model" *conscious-conversation-provider-profile*))
         (*conscious-recursive-mind-endpoint* (gethash "endpoint" *conscious-conversation-provider-profile*)))
    (multiple-value-bind (messages tools name output-tokens) (%ccg-model-request phase spec)
      (declare (ignore name))
      (declare (ignore output-tokens))
      (let* ((microusd (ceiling (* 1000000d0
                                  (%conversation-openrouter-request-cost-bound
                                    messages *conscious-recursive-mind-endpoint*
                                    *conscious-recursive-mind-model* 0.1d0 tools "required")))))
        (unless (<= 1 microusd maximum-microusd)
          (error 'pai.context-graph::context-graph-reservation-failure
                 :classification "request-shape-too-large"
                 :phase phase
                 :requested-microusd microusd
                 :ceiling-microusd maximum-microusd))
        microusd))))

(defun %ccg-model-call (phase spec digest opened-id &optional ceiling-microusd)
  (declare (ignore digest))
  (let* ((*conscious-conversation-provider-profile* (%ccg-provider-profile))
         (*conscious-recursive-mind-model* (gethash "model" *conscious-conversation-provider-profile*))
         (*conscious-recursive-mind-endpoint* (gethash "endpoint" *conscious-conversation-provider-profile*)))
    ;; Enforced by transport too; refuse before sending any private source.
    (unless (eq t (gethash "zdr" (%conversation-openrouter-provider-policy)))
      (error "Reviewed graph requires explicit ZDR routing"))
    (multiple-value-bind (messages tools name output-tokens) (%ccg-model-request phase spec)
      (declare (ignore output-tokens))
      (let* ((bound (%ccg-model-reservation phase spec nil opened-id
                                            (or ceiling-microusd
                                                *conscious-context-graph-request-ceiling-microusd*))))
        (%ccg-await-provider-call-slot)
        (let* ((*conscious-conversation-call-charge-usd* 0d0)
               (call-started-at (get-internal-real-time))
               (response
               (handler-case
                   (%recursive-kg-model-call messages tools opened-id
                     (format nil "thread:reviewed-graph-v2:~d" opened-id) phase nil)
                 (error (condition)
                   (let* ((elapsed-seconds
                            (/ (- (get-internal-real-time) call-started-at)
                               (float internal-time-units-per-second 1d0)))
                          (http-p (typep condition 'dex:http-request-failed))
                          (status (and http-p (dex:response-status condition)))
                          (message
                            (or (and http-p (%conversation-provider-http-message condition))
                                (%conversation-condition-summary condition)))
                          (retryable
                            (or (null status) (member status '(408 409 425 429))
                                (and (integerp status) (<= 500 status 599))))
                          (classification
                            (cond ((null status) "provider-outcome-ambiguous")
                                  (retryable "provider-transient")
                                  (t "provider-request-rejected")))
                          (known-charge
                            (if (and (integerp status)
                                     (member status *conscious-conversation-known-http-rejection-statuses*))
                                0 :null)))
                     (format *error-output*
                             "~&[knowledge graph provider] phase=~a elapsed=~,1fs http-status=~a classification=~a retryable=~a message=~a~%"
                             phase elapsed-seconds (or status "none") classification
                             (not (null retryable)) (or message "none"))
                     (finish-output *error-output*)
                     (error 'pai.context-graph::context-graph-call-failure
                            :classification classification :retryable-p (not (null retryable))
                            :charged-microusd known-charge))))))
        (when (member response '(:preempted :paused-budget))
          (return-from %ccg-model-call (values response 0)))
        ;; This call's own charge, not the session ledger's movement. Admission
        ;; checks reconcile pending settlements, so the ledger can advance by
        ;; another call's charge inside this one's extent; attributing that
        ;; here rejected valid responses as outcome-ambiguous.
        (let ((charge (round (* 1000000d0
                                *conscious-conversation-call-charge-usd*))))
          (when (> charge bound)
            ;; The caller's handler classifies any error here as
            ;; provider-outcome-ambiguous without recording why, so state the
            ;; reason before signalling or it is lost.
            (format *error-output*
                    "~&[knowledge graph] phase=~a charge-exceeded-reservation charged=~a reserved=~a~%"
                    phase charge bound)
            (finish-output *error-output*)
            (error "Reviewed graph charge ~a exceeded its reservation ~a"
                   charge bound))
          (let ((shasht:*read-default-true-value* :true)
                (shasht:*read-default-false-value* :false)
                (shasht:*read-default-null-value* :null))
            (values
              (handler-case
                  (%recursive-kg-native-arguments (%conversation-response-message response) name)
                (error ()
                  (error 'pai.context-graph::context-graph-call-failure
                         :classification "model-output-invalid" :retryable-p t
                         :charged-microusd charge)))
              charge))))))))

(defun %ccg-owner-exposure-microusd (owner)
  "Rebuild cumulative charged plus outcome-unknown exposure from durable phases."
  (loop for row being the hash-values of (pai.context-graph::cgi-owner-phases owner)
        sum (if (equal "request" (gethash "outcome" row))
                (gethash "reserved_microusd" row)
                (gethash "charged_microusd" row))))

(defun %ccg-owner-budget-remaining-microusd (owner)
  (max 0 (- *conscious-context-graph-generation-budget-microusd*
            *conscious-context-graph-prior-exposure-microusd*
            (%ccg-owner-exposure-microusd owner))))

(defun %ccg-owner-failure-summary (owner now)
  "Summarize only latest batch attempts; superseded failures remain exposure,
not unresolved work. Returns retryable, unresolved and next circuit-open time."
  (let ((retryable 0) (unresolved 0) (next-retry-at nil))
    (loop for opened being the hash-values of (pai.context-graph::cgi-owner-tasks owner)
          for terminal = (gethash opened (pai.context-graph::cgi-owner-terminals owner))
          for record = (and terminal (pai.context-graph::%cgro-record terminal))
          when (and record (gethash "reason" record)) do
            (if (eq :true (gethash "retryable" record))
                (progn
                  (incf retryable)
                  (let ((scheduled (gethash "next_retry_at" record)))
                    (when (and (integerp scheduled) (> scheduled now))
                      (setf next-retry-at (max (or next-retry-at 0) scheduled)))))
                (incf unresolved)))
    (values retryable unresolved next-retry-at)))

(defun %ccg-owner-next-task (owner source-fn events agent-id persona-id now)
  "Prefer due retries, then untouched batches. Completion, not opening, is the
coverage boundary for retry-aware generation."
  (let ((fresh nil) (blocked 0))
    (loop for episode in
            (sort (loop for event in events
                        when (and (equal "conversation-episode-sealed" (gethash "type" event))
                                  (%kgfs-event-in-partition-p event agent-id persona-id))
                          collect (gethash "id" event)) #'<) do
      (handler-case
          (let* ((full (funcall source-fn
                                (pai.context-graph::cgi-owner-graph owner)
                                episode now))
                 (batches
                   (length (pai.context-graph::%cgro-source-batches full))))
            (loop for batch below batches
                  for prior = (gethash (list episode batch)
                                       (pai.context-graph::cgi-owner-tasks owner))
                  do (cond
                       ((null prior)
                        (unless fresh
                          (setf fresh (list episode batch 1 :null))))
                       ((pai.context-graph::%cgi-owner-version-repairable-terminal-p
                         owner prior)
                        ;; A sealed request-shape upgrade gets one recovery
                        ;; attempt before untouched history consumes the
                        ;; remaining authorized rebuild budget.
                        (return-from %ccg-owner-next-task
                          (values
                           (list episode batch
                                 pai.context-graph::+cgi-owner-version-repair-attempt+
                                 prior)
                           blocked)))
                       ((pai.context-graph::%cgi-owner-retryable-terminal-p
                         owner prior now)
                        (return-from %ccg-owner-next-task
                          (values
                           (list episode batch
                                 (1+ (pai.context-graph::%cgi-owner-opening-attempt
                                      owner prior))
                                 prior)
                           blocked))))))
        (error ()
          ;; One malformed or unauthentic episode is a coverage block for that
          ;; source. It must not prevent inspection of later sealed source rows.
          (incf blocked))))
    (values fresh blocked)))

(defun %ccg-coverage-reference (kind agent-id persona-id id)
  (concatenate 'string kind ":"
               (pai.context-graph::%cg-sha256
                "context-graph-coverage-reference-v1"
                kind agent-id persona-id id)))

(defun conscious-context-graph-coverage-inspect
    (events agent-id persona-id &key episode-event-id now)
  "Rebuild and inspect formation coverage without mutation or source content.

EVENTS must be an authority-ordered snapshot (including any authorized
migration lane supplied by the caller).  Results contain only counts and
digest-derived references; a failed source envelope remains unknown/blocked
instead of being guessed from later graph state."
  (unless (and (listp events) (stringp agent-id) (plusp (length agent-id))
               (stringp persona-id) (plusp (length persona-id))
               (or (null episode-event-id)
                   (and (integerp episode-event-id) (plusp episode-event-id))))
    (error "Context graph coverage inspector inputs are invalid"))
  (let* ((clock (or now (funcall *conscious-context-graph-now-fn*)))
         (index (make-hash-table :test #'eql))
         (runtime (pai.context-graph:context-graph-runtime-create
                   (%ccg-ontology) *knowledge-graph-ontology-revision*
                   agent-id persona-id))
         (owner (%ccg-create-owner
                 (pai.context-graph::context-graph-runtime-graph runtime)
                 agent-id persona-id))
         (replay-refusals 0) (rows nil)
         (counts (obj "not-yet-sealed" 0 "queued" 0
                      "source-unavailable" 0 "source-envelope-rejected" 0
                      "retry-paused" 0 "budget-paused" 0 "phase-failed" 0
                      "completed-no-relevant-claim" 0 "review-rejected" 0
                      "applied" 0)))
    (unless (and (integerp clock) (<= 0 clock))
      (error "Context graph coverage inspector clock is invalid"))
    (dolist (event events)
      (when (and (hash-table-p event) (integerp (gethash "id" event)))
        (setf (gethash (gethash "id" event) index) event)))
    (labels ((source (graph episode observed-at)
               (%ccg-source-context graph episode observed-at index
                                    agent-id persona-id))
             (note (status episode batch &optional opened terminal)
               (incf (gethash status counts 0))
               (push (obj "episode_ref"
                          (%ccg-coverage-reference "episode" agent-id persona-id episode)
                          "batch_index" (or batch :null)
                          "status" status
                          "opened_ref"
                          (if opened
                              (%ccg-coverage-reference "opening" agent-id persona-id opened)
                              :null)
                          "terminal_ref"
                          (if terminal
                              (%ccg-coverage-reference
                               "terminal" agent-id persona-id (gethash "id" terminal))
                              :null))
                     rows)))
      ;; Replay uses the production owner and source authenticator.  One bad
      ;; receipt is counted but cannot make this read-only report disclose its
      ;; payload or invent a lifecycle outcome for another source.
      (dolist (event (sort (remove-if-not
                            (lambda (row)
                              (and (hash-table-p row)
                                   (integerp (gethash "id" row))
                                   (plusp (gethash "id" row))))
                            (copy-list events))
                           #'< :key (lambda (row) (gethash "id" row))))
        (handler-case
            (pai.context-graph::%cgi-owner-consume owner event #'source)
          (error () (incf replay-refusals))))
      (dolist (episode
               (sort
                (remove-if-not
                 (lambda (event)
                   (and (equal "conversation-episode-sealed"
                               (gethash "type" event))
                        (%kgfs-event-in-partition-p event agent-id persona-id)
                        (or (null episode-event-id)
                            (= episode-event-id (gethash "id" event)))))
                 (copy-list events))
                #'< :key (lambda (event) (gethash "id" event))))
        (let ((id (gethash "id" episode)))
          (handler-case
              (let* ((context (source (pai.context-graph::cgi-owner-graph owner)
                                      id clock))
                     (batches (length (pai.context-graph::%cgro-source-batches
                                       context))))
                (loop for batch below batches
                      for opened = (gethash (list id batch)
                                            (pai.context-graph::cgi-owner-tasks owner))
                      for terminal = (and opened
                                          (gethash opened
                                                   (pai.context-graph::cgi-owner-terminals owner)))
                      for terminal-record = (and terminal
                                                 (pai.context-graph::%cgro-record terminal))
                      for application = (and opened
                                             (gethash opened
                                                      (pai.context-graph::cgi-owner-applications owner)))
                      do
                         (cond
                           ((null opened) (note "queued" id batch))
                           ((null terminal) (note "queued" id batch opened))
                           ((equal "context-graph-identity-failed"
                                   (gethash "type" terminal))
                            (note (if (and (eq :true
                                               (gethash "retryable" terminal-record))
                                           (integerp (gethash "next_retry_at"
                                                              terminal-record))
                                           (> (gethash "next_retry_at" terminal-record)
                                              clock))
                                      "retry-paused" "phase-failed")
                                  id batch opened terminal))
                           ((and application
                                 (equal "empty"
                                        (gethash "status"
                                                 (gethash "value" application (obj)) "")))
                            (note "completed-no-relevant-claim"
                                  id batch opened terminal))
                           (application (note "applied" id batch opened terminal))
                           (t (note "completed-no-relevant-claim"
                                    id batch opened terminal)))))
            (error () (note "source-envelope-rejected" id nil)))))
      (obj "schema_version" 1
           "inspector_revision" "context-graph-coverage-inspector-v1"
           "generation" *conscious-context-graph-owner-generation*
           "formation_protocol" *conscious-context-graph-formation-protocol*
           "episode_count" (length rows)
           "replay_refusal_count" replay-refusals
           "stage_counts" counts
           "outcomes" (coerce (nreverse rows) 'vector)
           "database_write_count" 0))))

(defun conscious-context-graph-formation-step (event-backend derived-backend agent-id persona-id)
  (declare (ignore derived-backend))
  (when (%recursive-operator-pending-p)
    (return-from conscious-context-graph-formation-step (obj "status" "preempted")))
  (bt:with-lock-held (*conscious-context-graph-formation-lock*)
   (bt:with-lock-held (*conscious-context-graph-lock*)
    (multiple-value-bind (runtime source-fn events owner) (%ccg-sync event-backend agent-id persona-id)
      (declare (ignore runtime))
      (let ((now (funcall *conscious-context-graph-now-fn*))
            (source-blocked-count 0))
       (labels ((shared-policy ()
                  (and *conscious-context-graph-budget-authorization-id*
                       (obj "authorization_id" *conscious-context-graph-budget-authorization-id*
                            "agent_id" agent-id "persona_id" persona-id
                            "generation" *conscious-context-graph-owner-generation*
                            "prior_exposure_microusd" *conscious-context-graph-prior-exposure-microusd*
                            "ceiling_microusd" *conscious-context-graph-generation-budget-microusd*
                            "per_request_ceiling_microusd"
                            *conscious-context-graph-request-ceiling-microusd*)))
                (append-event (type payload cause)
                  (let ((policy (shared-policy)))
                    (if (and policy (equal type "context-graph-identity-phase"))
                        (progn
                          (unless (event-authority-owns-storage-p event-backend)
                            (error "Shared graph budget requires the installed SQLite event authority"))
                          (context-graph-budget-append-phase
                           event-backend policy payload cause
                           :append-fn
                           (lambda (head selected-type selected-payload selected-cause)
                             (multiple-value-bind (id durable event)
                                 (log-event selected-type selected-payload
                                            :caused-by selected-cause :expected-head head)
                               (declare (ignore id))
                               (unless (and durable event)
                                 (error "Shared graph budget conditional publication conflicted"))
                               event))))
                        (nth-value 1 (%conversation-append-readable type payload :caused-by cause)))))
               (report (status &optional opened)
                 (let* ((generation-spent (%ccg-owner-exposure-microusd owner))
                        (spent (+ *conscious-context-graph-prior-exposure-microusd*
                                  generation-spent)))
                   (multiple-value-bind (retryable unresolved next-retry-at)
                       (%ccg-owner-failure-summary owner now)
                     (obj "schema_version" 1 "status" status
                          "generation" *conscious-context-graph-owner-generation*
                          "formation_protocol" *conscious-context-graph-formation-protocol*
                          "opened_event_id" (or opened :null)
                          "applied_tasks" (hash-table-count (pai.context-graph::cgi-owner-applications owner))
                          "retryable_tasks" retryable "unresolved_tasks" unresolved
                          "source_blocked_count" source-blocked-count
                          "next_retry_at" (or next-retry-at :null)
                          "exposure_microusd" spent
                          "generation_exposure_microusd" generation-spent
                          "prior_exposure_microusd" *conscious-context-graph-prior-exposure-microusd*
                          "remaining_microusd" (max 0 (- *conscious-context-graph-generation-budget-microusd* spent)))))))
        (let ((opened
                (loop for id being the hash-keys of (pai.context-graph::cgi-owner-opens owner)
                      unless (gethash id (pai.context-graph::cgi-owner-terminals owner)) return id)))
          (unless opened
            (multiple-value-bind (retryable unresolved next-retry-at)
                (%ccg-owner-failure-summary owner now)
              (declare (ignore retryable))
              ;; A provider-transient failure opens a generation-wide circuit.
              ;; Do not sacrifice untouched source batches while the same
              ;; provider is known unavailable merely because their individual
              ;; task keys have no retry receipt yet.
              (when next-retry-at
                (return-from conscious-context-graph-formation-step
                  (report "paused-provider")))
              (let ((task nil))
                (multiple-value-setq (task source-blocked-count)
                  (%ccg-owner-next-task owner source-fn events agent-id persona-id now))
              (unless task
                (return-from conscious-context-graph-formation-step
                  (report (cond ((or (plusp unresolved)
                                     (plusp source-blocked-count)) "incomplete")
                                (t "idle")))))
              (let ((remaining (%ccg-owner-budget-remaining-microusd owner)))
                (when (< remaining *conscious-context-graph-request-ceiling-microusd*)
                  (return-from conscious-context-graph-formation-step (report "paused-budget")))
                (setf opened
                      (gethash "id"
                        (pai.context-graph::%cgf-owner-open-v2 owner source-fn #'append-event
                          (first task) (second task) remaining
                          *conscious-context-graph-request-ceiling-microusd* now
                          *conscious-context-graph-formation-protocol* (%ccg-descriptor-guide)
                          :attempt (third task) :retry-of (fourth task))))))))
          (let ((result (pai.context-graph::%cgi-owner-run
                          owner source-fn #'append-event
                          (lambda (phase spec digest selected-opened ceiling)
                            (%ccg-call-with-graph-unlocked
                             #'%ccg-model-call phase spec digest selected-opened ceiling))
                          opened
                          #'%ccg-model-reservation *conscious-context-graph-now-fn*)))
            (let ((terminal (pai.context-graph::%cgi-owner-terminal-record owner opened)))
              (report (cond ((equal "complete" (gethash "status" result)) "sealed")
                            ((and terminal (eq :true (gethash "retryable" terminal))) "retry-scheduled")
                            (t (gethash "status" result)))
                      opened))))))))))

(defun %ccg-call-with-graph-unlocked (call-fn phase spec digest opened ceiling)
  "Run remote provider IO without blocking readers of the stable projection.

The caller holds both the formation lock and graph lock.  The formation lock
prevents another owner step from interpreting the durable pending request while
CALL-FN is in flight; only the graph lock is released.  UNWIND-PROTECT restores
the graph-lock invariant before owner state or projection state is touched."
  (bt:release-lock *conscious-context-graph-lock*)
  (unwind-protect
       (funcall call-fn phase spec digest opened ceiling)
    (bt:acquire-lock *conscious-context-graph-lock*)))

(defun %ccg-retrieval-lexicon ()
  (let* ((path (asdf:system-relative-pathname :pai "config/context-graph-upper-ontology-v1.2.json"))
         (document (with-open-file (stream path :external-format :utf-8) (shasht:read-json stream))))
    (unless (equal *knowledge-graph-ontology-revision* (gethash "ontology_revision" document))
      (error "Retrieval ontology revision mismatch"))
    (let ((lexicon
            (pai.context-graph:context-graph-retrieval-lexicon
             (gethash "ontology" document))))
      ;; V1.3 is an opt-in runtime extension.  Keep the qualified V1.2 JSON
      ;; immutable and add only positive retrieval meanings for the new closed
      ;; vocabulary; replay of older generations continues to use V1.2 exactly.
      (when (equal (%ccg-runtime-ontology-revision)
                   *knowledge-graph-family-ontology-revision*)
        (setf (gethash "attribute_value" (gethash "entity_types" lexicon))
              "An explicit scalar or categorical value attached to a person or organism."
              (gethash "parent_of" (gethash "predicates" lexicon))
              "Subject is a parent of the object person."
              (gethash "daughter_of" (gethash "predicates" lexicon))
              "Subject is a daughter of the object person."
              (gethash "son_of" (gethash "predicates" lexicon))
              "Subject is a son of the object person."
              (gethash "spouse_of" (gethash "predicates" lexicon))
              "Subject and object people are spouses."
              (gethash "companion_of" (gethash "predicates" lexicon))
              "Subject is a companion of the object person or organism."
              (gethash "has_age" (gethash "predicates" lexicon))
              "Subject has the exact source-stated age represented by the object value."
              (gethash "has_gender" (gethash "predicates" lexicon))
              "Subject has the exact source-stated gender represented by the object value."))
      lexicon)))

(defun %ccg-title-name-p (text)
  (let ((words (uiop:split-string text :separator '(#\Space #\Tab #\-))))
    (and words
         (every (lambda (word)
                  (and (plusp (length word)) (upper-case-p (char word 0))
                       (every (lambda (character)
                                (or (not (alpha-char-p character))
                                    (lower-case-p character)))
                              (subseq word 1)))) words))))

(defun %ccg-redact-name (text name)
  (if (or (zerop (length name)) (not (search name text :test #'char-equal))) text
      (with-output-to-string (out)
        (loop with start = 0
              for position = (search name text :start2 start :test #'char-equal)
              do (if position
                     (progn (write-string text out :start start :end position)
                            (write-char #\Space out)
                            (setf start (+ position (length name))))
                     (progn (write-string text out :start start) (return)))))))

(defun %ccg-semantic-document (graph id descriptor)
  "Stable descriptor plus admitted factual text for candidate discovery."
  (let* ((label (gethash "label" descriptor))
         (aliases (coerce (gethash "aliases" descriptor) 'list))
         (proper-name-p (%ccg-title-name-p label))
         (statements nil) (used 0))
    (loop for fact-id across (sort (copy-seq (gethash id
                                      (pai.context-graph::context-graph-entity-adjacency graph) #())) #'string<)
          for fact = (gethash fact-id (pai.context-graph::context-graph-facts graph))
          for statement = (and fact (gethash "fact" fact))
          when (and fact (< (length statements) 4) (pai.context-graph::%cgr-factual-p fact)
                    (stringp statement) (<= (+ used (length statement)) 1600)) do
            (incf used (length statement))
            (push (if proper-name-p
                      (reduce #'%ccg-redact-name (cons label aliases) :initial-value statement)
                      statement) statements))
    (format nil "~{~a | ~}~{~a | ~}~a~{ | ~a~}"
            (if proper-name-p nil (cons label aliases))
            (coerce (gethash "classifications" descriptor) 'list)
            (gethash "kind" descriptor) (nreverse statements))))

(defun %ccg-semantic-encoder-revision ()
  (if *conscious-context-graph-semantic-similarity-fn*
      "fixture-semantic-query-factual-document-v2"
      (format nil "~a:query-factual-document-v2" *ollama-embed-model*)))

(defun %ccg-semantic-score-milli (graph query descriptor id encoder-revision query-vector)
  (let* ((document (%ccg-semantic-document graph id descriptor))
         (similarity
           (if *conscious-context-graph-semantic-similarity-fn*
               (funcall *conscious-context-graph-semantic-similarity-fn* query document)
               (let* ((digest (pai.context-graph::%cg-sha256 document))
                      (key (list encoder-revision id))
                      (cached (gethash key *conscious-context-graph-semantic-vectors*)))
                 (unless (and cached (equal digest (car cached)))
                   (setf cached (cons digest (embed-retrieval-document document))
                         (gethash key *conscious-context-graph-semantic-vectors*) cached))
                 (cosine-similarity query-vector (cdr cached))))))
    (unless (and (realp similarity) (<= -1 similarity 1))
      (error "Invalid context-graph semantic similarity"))
    (max 0 (min 1000 (round (* 1000 similarity))))))

(defun %ccg-cache-semantic-documents (graph ids encoder-revision)
  "Batch-fill only absent or changed semantic documents in stable ID order."
  (unless *conscious-context-graph-semantic-similarity-fn*
    (let ((missing nil))
      (loop for id across ids
            for descriptor =
              (pai.context-graph::%cg-authority-current-descriptor graph id)
            for document = (%ccg-semantic-document graph id descriptor)
            for digest = (pai.context-graph::%cg-sha256 document)
            for key = (list encoder-revision id)
            for cached = (gethash key
                                  *conscious-context-graph-semantic-vectors*)
            unless (and cached (equal digest (car cached)))
              do (push (list key digest document) missing))
      (when missing
        (setf missing (nreverse missing))
        (let ((embeddings
                (%memory-retrieval-time
                 "graph_semantic_document_embedding"
                 (lambda ()
                   (funcall
                    *conscious-context-graph-semantic-documents-embed-fn*
                    (mapcar #'third missing))))))
          (unless (= (length embeddings) (length missing))
            (error "Context-graph semantic batch count mismatch"))
          (loop for (key digest) in missing
                for embedding in embeddings
                do (setf (gethash key
                                  *conscious-context-graph-semantic-vectors*)
                         (cons digest embedding))))))))

(defun %ccg-semantic-receipt (runtime query source-packet)
  "Return a qualified semantic receipt or :NULL for safe lexical degradation."
  (handler-case
      (let* ((graph (pai.context-graph::context-graph-runtime-graph runtime))
             (ids (remove-if-not
                    (lambda (id)
                      (eq :null (gethash "participant_role"
                                  (pai.context-graph::%cg-authority-current-descriptor graph id))))
                    (pai.context-graph::context-graph-entity-scan-index graph))))
        (when (> (length ids) 1024) (return-from %ccg-semantic-receipt :null))
        (let* ((encoder-revision (%ccg-semantic-encoder-revision))
               (*embedding-fallback-policy* :error)
               (query-vector (unless *conscious-context-graph-semantic-similarity-fn*
                               (%memory-retrieval-time
                                "graph_semantic_query_embedding"
                                (lambda ()
                                  (funcall
                                   *conscious-context-graph-semantic-query-embed-fn*
                                   query))))))
          (%ccg-cache-semantic-documents graph ids encoder-revision)
          (let* ((candidates
                   (map 'vector
                        (lambda (id)
                          (let ((descriptor
                                  (pai.context-graph::%cg-authority-current-descriptor
                                   graph id)))
                            (obj "kind" "entity" "id" id "score_milli"
                                 (%ccg-semantic-score-milli
                                  graph query descriptor id encoder-revision
                                  query-vector))))
                        ids)))
            (pai.context-graph:context-graph-build-semantic-receipt
              graph (pai.context-graph::context-graph-runtime-agent-id runtime)
              (pai.context-graph::context-graph-runtime-persona-id runtime)
              query source-packet encoder-revision candidates))))
    (error () :null)))

(defun %ccg-retrieve (runtime query &optional (maximum-bytes 6000))
  (let ((graph (pai.context-graph::context-graph-runtime-graph runtime)))
    (if (zerop (pai.context-graph:context-graph-entity-count graph))
        (obj "context" (obj "entities" #() "facts" #() "sources" #())
             "candidates" (obj "entities" #() "facts" #() "sources" #()) "scan_complete" :true)
        ;; Source proximity uses only independently authenticated opened sources.
        ;; Bound discovery to the 1024 newest distinct source rows, select 128
        ;; by separate-term relevance. Oversized utterances are not truncated
        ;; into false full-source receipts; graph facts remain independently usable.
        (let ((sources (make-hash-table :test #'equal)) (ranked nil)
              (terms (pai.context-graph::%cgr-focus-terms (pai.context-graph::%cgr-tokens query))) (examined 0))
            (loop for opened in (sort (loop for e being the hash-values of (pai.context-graph::context-graph-runtime-opens runtime) collect e)
                                    #'> :key (lambda (e) (gethash "id" e)))
                while (< examined 1024) do
            (loop for source across (let* ((record (pai.context-graph::%cgro-record opened))
                                           (context (or (gethash "source_context" record)
                                                        (gethash "context" record))))
                                      (gethash "sources" (gethash "source_packet" context)))
                  for id = (gethash "source_id" source)
                  while (< examined 1024) unless (gethash id sources) do
                    (setf (gethash id sources) t) (incf examined)
                    (when (<= (length (gethash "text" source)) 12000)
                      (let ((score (count-if (lambda (term)
                                              (pai.context-graph::%cgr-term-in-range-p
                                               term (gethash "text" source) 0 (length (gethash "text" source)))) terms)))
                        (when (plusp score) (push (cons score source) ranked))))))
          (setf ranked (sort ranked (lambda (a b) (if (= (car a) (car b))
                              (string< (gethash "source_id" (cdr a)) (gethash "source_id" (cdr b))) (> (car a) (car b))))))
          (let* ((source-packet
                   (obj "schema_version" 2 "sources"
                        (coerce (mapcar #'cdr (subseq ranked 0 (min 128 (length ranked)))) 'vector)))
                 (ask (lambda (&optional (semantic-receipt :null))
                        (pai.context-graph::%cg-authority-retrieve graph
                          (pai.context-graph::context-graph-runtime-agent-id runtime)
                          (pai.context-graph::context-graph-runtime-persona-id runtime) query
                          :maximum-bytes maximum-bytes :candidate-limit 32
                          :focused t :lexicon (%ccg-retrieval-lexicon)
                          :query-specific-relations t :factual-entities t
                          :semantic-receipt semantic-receipt :source-packet source-packet)))
                 (lexical (funcall ask))
                 (covered
                   (remove-duplicates
                     (loop for row across (gethash "facts" (gethash "context" lexical))
                           append (coerce (gethash "matched_terms" row) 'list))
                     :test #'equal))
                 ;; Semantic relevance supplements a lexical result when the
                 ;; admitted facts cover only part of the question.  It remains
                 ;; unnecessary for a fully covered lexical query and never
                 ;; supplies provenance or graph admission authority.
                 (receipt (if (and terms
                                   (not (every (lambda (term)
                                                 (member term covered :test #'equal))
                                               terms)))
                              (%ccg-semantic-receipt runtime query source-packet) :null))
                 (result (if (and (not (eq :null receipt))
                                  (plusp (length (gethash "hits" receipt))))
                             (funcall ask receipt) lexical)))
            ;; Selection is intentionally non-exhaustive, even if the supplied
            ;; source envelope itself was completely scanned by the pure reader.
            (setf (gethash "source_discovery_non_exhaustive" result) t) result)))))

(defun %ccg-node (entity)
  (obj "projection_name" "reviewed-context-graph-v1" "node_id" (gethash "entity_id" entity)
       "node_kind" (gethash "kind" entity) "label" (gethash "label" entity)
       "aliases" (gethash "aliases" entity) "classifications" (gethash "classifications" entity)
       "status" "current" "disclosure_class" "private" "evidence_status" "reviewed-entity"))

(defun %ccg-fact-evidence-policy-allows-p (fact policy)
  "Keep verified recall strict while making reviewed inference opt-in."
  (let ((grounding (gethash "grounding" fact))
        (status (gethash "evidence_status" fact)))
    (and (equal "current" (gethash "status" fact))
         (equal "assertion" (gethash "scope" grounding))
         (equal "positive" (gethash "polarity" grounding))
         (cond ((equal policy "verified")
                (pai.context-graph::%cgr-factual-p fact))
               ((equal policy "inferred")
                (or (pai.context-graph::%cgr-factual-p fact)
                    (equal status "inference")))
               ((equal policy "all") t)
                (t nil)))))

(defun %ccg-omitted-reviewed-inference-count (graph seeds request)
  "Count adjacent reviewed inferences hidden by a verified-only traversal."
  (if (not (equal "verified" (gethash "evidence_policy" request))) 0
      (let ((seen (make-hash-table :test #'equal))
            (direction (gethash "direction" request))
            (predicates (gethash "predicates" request))
            (count 0))
        (dolist (node seeds count)
          (let* ((id (gethash "node_id" node))
                 (ids (gethash id
                               (pai.context-graph::context-graph-entity-adjacency graph)
                               #())))
            (dotimes (index (min 256 (length ids)))
              (let* ((fact-id (aref ids index))
                     (fact (gethash fact-id
                                    (pai.context-graph::context-graph-facts graph)))
                     (outgoing (equal id (gethash "subject_id" fact)))
                     (grounding (gethash "grounding" fact)))
                (when (and (not (gethash fact-id seen))
                           (equal "current" (gethash "status" fact))
                           (equal "inference" (gethash "evidence_status" fact))
                           (equal "assertion" (gethash "scope" grounding))
                           (equal "positive" (gethash "polarity" grounding))
                           (or (zerop (length predicates))
                               (find (gethash "predicate" fact) predicates :test #'equal))
                           (or (equal "both" direction)
                               (equal direction (if outgoing "outgoing" "incoming"))))
                  (setf (gethash fact-id seen) t)
                  (incf count)))))))))

(defun %ccg-exact-query-nodes (graph query &optional runtime-persona-id)
  "Return current entities whose complete label or alias equals QUERY.

This is an operator discovery lane, not factual-context admission.  It does not
promote semantic similarity, source text, or a reviewed-but-unapplied proposal
to graph authority. The authenticated runtime persona identifier resolves the
existing active-persona participant without changing its graph label or aliases.
The second value is true only when the complete identity scan index was examined."
  (let* ((needle (and (stringp query)
                      (string-trim '(#\Space #\Tab #\Newline #\Return) query)))
         (index (pai.context-graph::context-graph-entity-scan-index graph))
        (rows nil))
    (when (and needle (plusp (length needle)))
      (loop for id across index
            repeat 4096
            for entity = (pai.context-graph::%cg-authority-current-descriptor graph id)
            when (or (string-equal needle (gethash "label" entity))
                     (find needle (gethash "aliases" entity #()) :test #'string-equal)
                     (and (stringp runtime-persona-id)
                          (string-equal needle runtime-persona-id)
                          (equal "active-persona" (gethash "participant_role" entity))))
              do (push (%ccg-node entity) rows)))
    (values (coerce (nreverse rows) 'vector)
            (<= (length index) 4096))))

(defun %ccg-exact-query-audit-results (graph queries &optional runtime-persona-id)
  "Answer a bounded batch of exact label/alias audits without graph mutation."
  (map
   'vector
   (lambda (query)
     (multiple-value-bind (matches scan-complete-p)
         (%ccg-exact-query-nodes graph query runtime-persona-id)
       (let* ((match-count (length matches))
              (returned-count (min 8 match-count))
              (absence-confirmed (and scan-complete-p (zerop match-count))))
         (obj "query" query
              "match_count" match-count
              "returned_match_count" returned-count
              "matches" (subseq matches 0 returned-count)
              "scan_complete" (if scan-complete-p t nil)
              "non_exhaustive"
              (if (or (not scan-complete-p) (> match-count returned-count)) t nil)
              "absence_confirmed" (if absence-confirmed t nil)
              "absence_note"
              (cond (absence-confirmed
                     "No exact current label or alias matched in the complete identity index.")
                    ((zerop match-count)
                     "No match was found in the scanned prefix; absence is not established because the identity scan was incomplete.")
                    (t
                     "One or more exact current label or alias matches were found."))))))
   queries))

(defun %ccg-search (runtime request)
  (unless (knowledge-graph-search-request-valid-p request) (error "Invalid reviewed graph search request"))
  (let* ((graph (pai.context-graph::context-graph-runtime-graph runtime))
         (runtime-persona-id
           (pai.context-graph::context-graph-runtime-persona-id runtime))
         (query (gethash "query" request)) (start (gethash "starting_node_id" request))
         (exact-queries (gethash "exact_queries" request #()))
         (exact-query-results
           (%ccg-exact-query-audit-results graph exact-queries runtime-persona-id))
         (entity (and (stringp start) (gethash start (pai.context-graph::context-graph-entities graph))))
         (exact-matches (if (and (stringp query) (plusp (length query)))
                            (%ccg-exact-query-nodes graph query runtime-persona-id) #()))
         (retrieved (and (zerop (length exact-matches))
                         (stringp query) (plusp (length query))
                         (%ccg-retrieve runtime query)))
         (related-matches (when retrieved
                    (map 'list (lambda (row) (%ccg-node (gethash "descriptor" (gethash "value" row))))
                         (gethash "entities" (gethash "candidates" retrieved)))))
         (query-fact-count
           (if retrieved
               (length (gethash "facts" (gethash "context" retrieved) #()))
               0))
         (matches (if (plusp (length exact-matches))
                      (coerce exact-matches 'list) related-matches))
         (match-kind (cond ((plusp (length exact-matches)) "exact-identity")
                           (related-matches "related-suggestions")
                           (t "none")))
          (seeds (if entity (list (%ccg-node entity)) matches))
          (omitted-inferences
            (%ccg-omitted-reviewed-inference-count graph seeds request))
          (result
           (knowledge-graph-search-traverse request (coerce seeds 'vector)
             (lambda (node closed)
               (let* ((id (gethash "node_id" node))
                      (ids (gethash id (pai.context-graph::context-graph-entity-adjacency graph) #()))
                      (rows nil) (count (min 256 (length ids))))
                 (dotimes (i count)
                   (let* ((fact (gethash (aref ids i) (pai.context-graph::context-graph-facts graph)))
                          (subject (pai.context-graph::%cg-authority-current-descriptor graph
                                     (gethash "entity_id" (pai.context-graph::%cg-authority-entity graph (gethash "subject_id" fact)))))
                          (object (pai.context-graph::%cg-authority-current-descriptor graph
                                    (gethash "entity_id" (pai.context-graph::%cg-authority-entity graph (gethash "object_id" fact)))))
                          (outgoing (equal id (gethash "entity_id" subject)))
                          (direction (gethash "direction" closed)) (predicates (gethash "predicates" closed)))
                     (when (and (%ccg-fact-evidence-policy-allows-p
                                 fact (gethash "evidence_policy" closed))
                                (or (zerop (length predicates)) (find (gethash "predicate" fact) predicates :test #'equal))
                                (or (equal "both" direction) (equal direction (if outgoing "outgoing" "incoming"))))
                       (push (obj "node" (%ccg-node (if outgoing object subject))
                                  "edge" (obj "projection_name" "reviewed-context-graph-v1" "edge_id" (gethash "fact_id" fact)
                                              "from_node_id" (gethash "entity_id" subject) "to_node_id" (gethash "entity_id" object)
                                              "predicate" (gethash "predicate" fact) "status" "current"
                                              "evidence_status" (gethash "evidence_status" fact)
                                              "origin_agent_ids"
                                              (%kgs-origin-agent-ids
                                               (gethash "accepted_source_ids" fact #()))
                                              "traversal_direction" (if outgoing "outgoing" "incoming")
                                              "valid_from" (gethash "valid_from" (gethash "temporal" fact))
                                              "valid_to" (gethash "valid_until" (gethash "temporal" fact)))) rows))))
                 (let ((size (length rows)))
                   (values (coerce (subseq (nreverse rows) 0 (min 32 size)) 'vector)
                           (or (> (length ids) count) (> size 32))))))
             :non-exhaustive-p t)))
    (setf (gethash "search_revision" result) "reviewed-context-graph-search-v2"
          (gethash "status" result)
          (if (or (plusp (gethash "path_count" result))
                  (plusp (length exact-query-results)))
              "available" "empty")
          (gethash "graph_entity_count" result)
          (pai.context-graph:context-graph-entity-count graph)
          (gethash "graph_fact_count" result)
          (pai.context-graph:context-graph-fact-count graph)
          (gethash "result_scope" result) "bounded-traversal-subset"
           (gethash "scope_note" result)
           "node_count and edge_count are returned-subset counts, not graph totals. A zero-match absence is established only by an exact_query_results item whose absence_confirmed is true."
           (gethash "omitted_reviewed_inference_count" result) omitted-inferences
           (gethash "retrieval_hint" result)
           (if (plusp omitted-inferences)
               (format nil "~d adjacent reviewed inference~:p were omitted by evidence_policy verified. If the task is relationship discovery or graph inspection, consider one inferred search; keep verified for grounded factual answers."
                       omitted-inferences)
               :null)
          (gethash "exact_query_results" result) exact-query-results
          (gethash "query_match_kind" result) match-kind
          (gethash "query_match_nodes" result) (coerce matches 'vector)
          (gethash "query_match_count" result) (length matches)
          (gethash "matched_fact_count" result) query-fact-count
          (gethash "suggestion_count" result)
          (if (string= match-kind "related-suggestions") (length matches) 0)
          (gethash "answer_status" result)
          (cond ((plusp (length exact-query-results)) "exact-audit")
                ((plusp query-fact-count) "query-facts")
                ((and (string= match-kind "exact-identity")
                      (some (lambda (path) (plusp (length (gethash "edges" path #()))))
                            (gethash "paths" result #())))
                 "identity-with-relations")
                ((string= match-kind "exact-identity") "identity-only")
                ((string= match-kind "related-suggestions")
                 "suggestions-only")
                (t "no-match"))
          (gethash "starting_node_status" result) (cond (entity "resolved") ((stringp start) "unresolved-query-fallback") (t "not-requested"))
          (gethash "unresolved_starting_node_id" result) (if (and (stringp start) (not entity)) start :null))
    result))

(defun conscious-context-graph-search (event-backend agent-id persona-id request)
  (bt:with-lock-held (*conscious-context-graph-lock*)
    (%ccg-search (%ccg-sync event-backend agent-id persona-id) request)))

(defun %ccg-confirmation-candidate (runtime fact-id)
  "Build the closed detached view used by the public read port."
  (unless (and (stringp fact-id) (plusp (length fact-id))
               (<= (length fact-id) 256))
    (error "Graph confirmation requires one bounded fact_id"))
  (let* ((graph (pai.context-graph::context-graph-runtime-graph runtime))
         (fact (gethash fact-id
                        (pai.context-graph::context-graph-facts graph)))
         (grounding (and fact (gethash "grounding" fact))))
      (unless (and (hash-table-p fact)
                   (equal "current" (gethash "status" fact))
                   (equal "inference" (gethash "evidence_status" fact))
                   (hash-table-p grounding)
                   (equal "assertion" (gethash "scope" grounding))
                   (equal "positive" (gethash "polarity" grounding)))
        (error "fact_id does not identify a current positive assertion inference"))
      (let* ((subject
               (pai.context-graph::%cg-authority-current-descriptor
                graph
                (gethash "entity_id"
                         (pai.context-graph::%cg-authority-entity
                          graph (gethash "subject_id" fact)))))
             (object
               (pai.context-graph::%cg-authority-current-descriptor
                graph
                (gethash "entity_id"
                         (pai.context-graph::%cg-authority-entity
                          graph (gethash "object_id" fact))))))
        (obj "schema_version" 1
             "fact_id" (gethash "fact_id" fact)
             "identity_sha256" (gethash "identity_sha256" fact)
             "statement" (gethash "fact" fact)
             "predicate" (gethash "predicate" fact)
             "subject" (%ccg-node subject)
             "object" (%ccg-node object)
             "evidence_status" "inference"
             "through_event_id" (gethash "through_event_id" fact)
             "ontology_revision"
             (pai.context-graph::context-graph-runtime-revision
              runtime)))))

(defun conscious-context-graph-confirmation-candidate
    (event-backend agent-id persona-id fact-id)
  "Resolve one exact current inference for an append-only confirmation request.

This is a read port.  It neither upgrades evidence nor edits the graph.  A
subsequent authenticated operator answer returns through ordinary episode
formation, where claim identity can upgrade the same claim or retire it in
favour of a directly evidenced correction."
  (bt:with-lock-held (*conscious-context-graph-lock*)
    (%ccg-confirmation-candidate
     (%ccg-sync event-backend agent-id persona-id) fact-id)))

(defun conscious-context-graph-proposal-result
    (event-backend agent-id persona-id proposal-event-id)
  "Synchronize and return the durable application outcome for one proposal."
  (unless (and (integerp proposal-event-id) (plusp proposal-event-id))
    (error "Graph proposal result requires one positive event id"))
  (bt:with-lock-held (*conscious-context-graph-lock*)
    (multiple-value-bind (runtime ignored-source-fn events ignored-owner)
        (%ccg-sync event-backend agent-id persona-id)
      (declare (ignore ignored-source-fn ignored-owner))
      (let* ((graph (pai.context-graph::context-graph-runtime-graph runtime))
             (receipt
               (gethash (format nil "event:~d" proposal-event-id)
                        (pai.context-graph::context-graph-application-receipts
                         graph)))
             (event (find proposal-event-id events :test #'eql
                          :key (lambda (row) (gethash "id" row))))
             (index (make-hash-table :test #'eql)))
        (dolist (row events)
          (setf (gethash (gethash "id" row) index) row))
        (if receipt
            (obj "schema_version" 1 "status" "applied"
                 "proposal_event_id" proposal-event-id
                 "formation_outcome" (gethash "formation_outcome" receipt)
                 "graph_entity_count"
                 (pai.context-graph:context-graph-entity-count graph)
                 "graph_fact_count"
                 (pai.context-graph:context-graph-fact-count graph))
            (let ((outcome
                    (or (gethash proposal-event-id
                                 *conscious-context-graph-proposal-outcomes*)
                        (and event
                             (%ccg-apply-conversation-proposal
                              runtime event index agent-id persona-id)))))
              (obj "schema_version" 1 "status" "rejected"
                   "proposal_event_id" proposal-event-id
                   "reason"
                   (if (and (hash-table-p outcome)
                            (stringp (gethash "reason" outcome)))
                       (gethash "reason" outcome)
                       "Proposal did not satisfy graph authority."))))))))

(defun %ccg-context-value (kind value)
  (labels ((descriptor (e)
             (obj "entity_id" (gethash "entity_id" e) "label" (gethash "label" e)
                  "kind" (gethash "kind" e))))
    (cond ((equal kind "entities")
           (obj "descriptor" (descriptor (gethash "descriptor" value))
                "interpretation" "current-entity-not-a-relationship-assertion"))
          ((equal kind "facts")
           (obj "subject" (descriptor (gethash "subject" value)) "predicate" (gethash "predicate" value)
                "object" (descriptor (gethash "object" value)) "grounding" (gethash "grounding" value)
                "statement" (gethash "statement" value "")
                "evidence_excerpts" (gethash "evidence_excerpts" value #())
                "temporal" (gethash "temporal" value) "source_ids" (gethash "source_ids" value)))
          (t value))))

(defun %ccg-context-records (runtime frame character-budget)
  (when (< character-budget 128)
    (return-from %ccg-context-records
      (values #() (obj "status" "empty") #())))
  (let* ((result (%ccg-retrieve runtime (%kgac-query frame) (min 16000 (* 4 character-budget))))
         (packet (gethash "context" result)) (records nil) (metadata nil)
         (used 0)
         (through (pai.context-graph::context-graph-through-event-id (pai.context-graph::context-graph-runtime-graph runtime))))
    (dolist (kind '("facts" "sources" "entities"))
      (loop for row across (gethash kind packet) for rank from 1
            for payload = (gethash "value" row)
            for text = (pai.context-graph:context-graph-runtime-json (obj "kind" kind "value" (%ccg-context-value kind payload)))
            when (<= (+ used (length text)) character-budget) do
              (incf used (length text))
              (let* ((id (format nil "graph-context:~a:~a" kind (gethash "id" row)))
                     (source-ids (cond ((equal kind "facts") (gethash "source_ids" payload))
                                       ((equal kind "sources") (vector (gethash "source_id" payload)))
                                       (t (gethash "source_proximity_ids" payload #()))))
                     (evidence (loop for source-id across source-ids
                                     when (and (stringp source-id) (uiop:string-prefix-p "event:" source-id))
                                       collect (parse-integer source-id :start 6))))
                (push (obj "source_id" id "content" text
                           "provenance" (obj "descriptor_id" id "descriptor_event_id" through
                                             "evidence_event_ids" (coerce (remove-duplicates (cons through evidence)) 'vector)))
                      records)
                ;; Preserve the result kind as typed selector input.  An exact
                ;; entity or a source-neighbour row is useful discovery data,
                ;; but it is not itself an answer-bearing personal fact.
                (push (obj "source_id" id
                           "source_kind"
                           (cond ((string= kind "facts") "graph-fact")
                                 ((string= kind "sources") "graph-source")
                                 (t "graph-entity"))
                           "operator_support" (if (string= kind "facts") t nil)
                           "speaker_basis"
                           (if (string= kind "facts")
                               "reviewed-graph-fact"
                               "reviewed-graph-discovery")
                           "local_rank" rank)
                      metadata))))
    (values (coerce (nreverse records) 'vector)
            (obj "status" (if records "available" "empty") "used_characters" used
                 "character_budget" character-budget "database_write_count" 0 "provider_calls" 0)
            (coerce (nreverse metadata) 'vector))))

(defun conscious-context-graph-attention-context (event-backend agent-id persona-id frame character-budget)
  (bt:with-lock-held (*conscious-context-graph-lock*)
    (%ccg-context-records (%ccg-sync event-backend agent-id persona-id) frame character-budget)))
