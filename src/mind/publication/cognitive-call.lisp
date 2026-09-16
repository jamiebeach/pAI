;;;; cognitive-call.lisp -- purpose-typed private model calls.
;;;;
;;;; This is not a conversational model path. It sends no production prompt,
;;;; dynamic context sections, conversation history, or tool catalogue. Output
;;;; is data that must pass schema, identity, evidence, grounding, word-bound,
;;;; and novelty validation before a later committer may use it.

(in-package :agent)

(declaim (ftype function cosine-similarity embed-text memory-search))

(export '(cognitive-call cognitive-artifact-call cognitive-call-report))

(defparameter *cognitive-call-purposes*
  '("idle-association" "light-consolidation" "deep-reflection"
    "anticipation" "rumination" "curiosity-synthesis"
    "worldview-exploration" "episode-replay" "continuity-compaction"
    "initiative-proposal" "initiative-scoring" "latent-evolution"
    "deferred-intention" "public-render"))
(defparameter *cognitive-call-record-types*
  '("supported-inference" "hypothesis" "prediction"))
(defparameter *cognitive-artifact-purposes*
  '(("creative-story-outline" . "outline")
    ("creative-story-draft" . "draft")
    ("creative-story-revision" . "revise")))
(defparameter *cognitive-artifact-timeout-seconds* 45)
(defparameter *cognitive-call-duplicate-threshold* 0.90d0)
(defparameter *cognitive-call-loop-sensitive-duplicate-threshold* 0.82d0)
(defparameter *cognitive-call-loop-sensitive-purposes*
  '("idle-association" "light-consolidation" "deep-reflection" "rumination"
    "worldview-exploration" "deferred-intention"))
(defparameter *cognitive-call-default-max-words* 120)
(defparameter *cognitive-call-schema*
  (obj "speaker" "the agent" "human" "the operator" "purpose" "purpose-name"
       "record_type" "supported-inference|hypothesis|prediction"
       "content" "non-empty private cognitive record"
       "evidence_node_ids" (vector "node-id")
       "uncertainty" 0.0d0
       "novel_contribution" "what this adds beyond its evidence"
       "proposed_next_operation" "none"))
(defparameter *cognitive-artifact-schema*
  (obj "speaker" "the agent" "human" "the operator"
       "purpose" "creative-story-outline|creative-story-draft|creative-story-revision"
       "operation_type" "outline|draft|revise"
       "artifact_content" "private synthetic story artifact"
       "change_summary" "bounded description of the requested transformation"
       "visibility" "private" "content_class" "synthetic"))
(defvar *cognitive-call-model-fn* nil
  "Optional test/adapter function: (messages model temperature) -> response.")
(defvar *cognitive-call-recent-records-fn* nil
  "Optional test/adapter function: (topic purpose) -> typed record objects.")
(defvar *cognitive-call-similarity-fn* nil
  "Optional test function: (left-text right-text) -> cosine similarity.")
(defvar *cognitive-current-purpose* nil)
(defvar *cognitive-current-generation-id* nil)
(defvar *cognitive-model-call-sequence* 0)
(defvar *cognitive-model-call-lock* (bt:make-lock "cognitive-model-call-id"))
(defvar *cognitive-call-stats* (make-hash-table :test #'equal))
(defvar *cognitive-call-stats-lock* (bt:make-lock "cognitive-call-stats"))

(defun %cognitive-purpose (purpose)
  (let ((normalized (string-downcase (string purpose))))
    (unless (member normalized *cognitive-call-purposes* :test #'string=)
      (error "Unsupported cognitive purpose ~s." purpose))
    normalized))

(defun %cognitive-list (value)
  (cond ((null value) nil)
        ((listp value) value)
        ((vectorp value) (coerce value 'list))
        (t (list value))))

(defun %cognitive-word-count (text)
  (if (stringp text)
      (length (remove-if (lambda (part) (zerop (length part)))
                         (uiop:split-string text
                                            :separator '(#\Space #\Tab #\Newline #\Return))))
      0))

(defun %cognitive-stat (key)
  (bt:with-lock-held (*cognitive-call-stats-lock*)
    (incf (gethash key *cognitive-call-stats* 0))))

(defun cognitive-call-report ()
  (bt:with-lock-held (*cognitive-call-stats-lock*)
    (let ((counts (obj)))
      (maphash (lambda (key value) (setf (gethash key counts) value))
               *cognitive-call-stats*)
      (obj "schema_version" 1 "duplicate_threshold"
           *cognitive-call-duplicate-threshold*
           "loop_sensitive_duplicate_threshold"
           *cognitive-call-loop-sensitive-duplicate-threshold*
           "counts" counts))))

(defun %cognitive-http-call (messages model temperature)
  "Dedicated tool-free HTTP path. Never call CALL-MODEL/RAW-CALL-MODEL: both
belong to legacy/public wrapper chains and may attach tools or dynamic state."
  (let ((body (obj "model" (or model *model*)
                   "messages" (coerce messages 'vector)
                   "temperature" (or temperature 0.3d0)
                   "response_format" (obj "type" "json_object"))))
    (shasht:read-json
     (dex:post *endpoint*
               :headers `(("Authorization" . ,(format nil "Bearer ~a" *api-key*))
                          ("Content-Type" . "application/json"))
               :connect-timeout *http-connect-timeout*
               :read-timeout *http-read-timeout*
               :content (shasht:write-json body nil)))))

(defun %cognitive-log (type payload &key caused-by)
  (when (fboundp 'log-event)
    (ignore-errors (funcall 'log-event type payload :caused-by caused-by))))

(defun %cognitive-model-call-id ()
  (bt:with-lock-held (*cognitive-model-call-lock*)
    (format nil "model-~d-~d" (get-universal-time)
            (incf *cognitive-model-call-sequence*))))

(defun %cognitive-invoke-model (messages model temperature
                                &key (request-kind "primary"))
  (let* ((resolved-model
           (or model (and (boundp '*model*) *model*) "unknown"))
         (resolved-temperature (or temperature 0.3d0))
         (model-call-id (%cognitive-model-call-id))
         (purpose (or *cognitive-current-purpose* "unknown"))
         (generation-id (or *cognitive-current-generation-id* :null))
         (adapter-kind (if *cognitive-call-model-fn* "test-adapter" "http"))
         (request-event-id
           (%cognitive-log
            "model-request"
            (obj "model_call_id" model-call-id
                 "generation_id" generation-id
                 "purpose" purpose
                 "request_kind" request-kind
                 "adapter_kind" adapter-kind
                 "model" resolved-model
                 "temperature" resolved-temperature
                 "messages" (coerce messages 'vector)
                 "response_format" (obj "type" "json_object"))))
         (thunk (lambda ()
                  (if *cognitive-call-model-fn*
                      (funcall *cognitive-call-model-fn* messages model temperature)
                      (%cognitive-http-call messages model temperature)))))
    (handler-case
        (let ((response
                (if (fboundp 'call-with-timing-span)
                    (funcall 'call-with-timing-span "model.cognitive_request" thunk
                             :attributes
                             (obj "purpose" purpose
                                  "message_count" (length messages)
                                  "model" resolved-model))
                    (funcall thunk))))
          (%cognitive-log
           "model-response"
           (obj "model_call_id" model-call-id
                "generation_id" generation-id
                "purpose" purpose
                "request_kind" request-kind
                "status" "ok"
                "response" response
                "error_type" :null
                "error_message" :null)
           :caused-by request-event-id)
          response)
      (error (condition)
        (%cognitive-log
         "model-response"
         (obj "model_call_id" model-call-id
              "generation_id" generation-id
              "purpose" purpose
              "request_kind" request-kind
              "status" "error"
              "response" :null
              "error_type" (string-downcase
                              (symbol-name (type-of condition)))
              "error_message" (princ-to-string condition))
         :caused-by request-event-id)
        (error condition)))))

(defun %cognitive-response-content (response)
  (cond ((stringp response) response)
        ((hash-table-p response)
         (or (ignore-errors (ref response "choices" 0 "message" "content"))
             (gethash "content" response)))
        (t nil)))

(defun %cognitive-response-usage (response)
  (let ((usage (and (hash-table-p response) (gethash "usage" response))))
    (obj "prompt_tokens" (or (and usage (gethash "prompt_tokens" usage)) :null)
         "completion_tokens" (or (and usage (gethash "completion_tokens" usage)) :null)
         "total_tokens" (or (and usage (gethash "total_tokens" usage)) :null)
         "cost_usd" (or (and usage (gethash "cost" usage)) :null))))

(defun %cognitive-parse-json-object (text)
  (handler-case
      (let ((parsed (and (stringp text) (shasht:read-json text))))
        (and (hash-table-p parsed) parsed))
    (error () nil)))

(defun %cognitive-evidence-id (node) (and (hash-table-p node) (gethash "id" node)))

(defun %cognitive-evidence-valid-p (node)
  (and (hash-table-p node)
       (stringp (%cognitive-evidence-id node))
       (not (gethash "quarantined" node))
       (let ((origin (gethash "origin_class" node))
             (status (gethash "epistemic_status" node))
             (grounding (gethash "grounding_status" node)))
         (and (stringp origin) (not (string= origin "legacy-unclassified"))
              (stringp status)
              (not (member status '("legacy-unclassified" "rejected") :test #'string=))
              (member grounding '("grounded" "partially-grounded") :test #'string=)
              (or (not (string= origin "synthetic"))
                  (plusp (length (%cognitive-list
                                  (gethash "root_observation_ids" node)))))))))

(defun %cognitive-has-lived-root-p (nodes)
  (some (lambda (node)
          (or (member (gethash "origin_class" node)
                      '("lived-user" "lived-agent-action" "tool-result"
                        "external-signal") :test #'string=)
              (plusp (length (%cognitive-list
                              (gethash "root_observation_ids" node))))))
        nodes))

(defun %cognitive-identity-names ()
  "The agent's and operator's names, for identity-confusion detection.

   Derived rather than hardcoded. This was a literal list containing one
   instance's actual names, which cannot be right for a substrate: every
   deployment has different ones, and a literal list silently detects
   nothing for all of them but the original. Renaming the literals during
   de-personalization is what exposed it -- the detector kept matching, but
   against names no instance uses.

   Until identity resolution lands, these come from the configured ids.
   When it does, this is the single place that changes."
  (remove-duplicates
   (remove nil (list (and (boundp '*operator-id*) (symbol-value '*operator-id*))
                     (and (boundp '*agent-id*) (symbol-value '*agent-id*))
                     "operator" "agent"))
   :test #'string-equal))

(defun %cognitive-identity-patterns ()
  "Phrases that indicate the agent has confused itself with its operator."
  (let ((patterns (list "as an ai language model")))
    (dolist (raw (%cognitive-identity-names) patterns)
      ;; Match with and without a leading article. A resolved identity may be
      ;; a proper noun or a role word, and the surrounding phrasing differs:
      ;; a proper noun appears bare ("i am <name>") while a role word takes an
      ;; article ("i am the operator"). Generating both covers either.
      (dolist (name (list raw (concatenate 'string "the " raw)))
        (push (format nil "i am ~a" name) patterns)
        (push (format nil "i'm ~a" name) patterns)
        (push (format nil "~a is the assistant" name) patterns)
        (push (format nil "~a is the human" name) patterns)
        (push (format nil "is ~a a character" name) patterns)
        (push (format nil "goodnight, ~a" name) patterns)))))

(defun %cognitive-identity-confusion-p (content)
  (and (stringp content)
       (some (lambda (pattern) (search pattern content :test #'char-equal))
             (%cognitive-identity-patterns))))

(defun %cognitive-unsupported-experience-p (content cited-nodes)
  (let ((claim-p
          (and (stringp content)
               (some (lambda (pattern) (search pattern content :test #'char-equal))
                     '("while you were away, i" "i watched" "i saw"
                       "i read" "i searched" "i browsed" "i accessed"
                       "continuous subjective experience"))))
        (support-p
          (some (lambda (node)
                  (member (gethash "origin_class" node)
                          '("tool-result" "external-signal" "lived-agent-action")
                          :test #'string=))
                cited-nodes)))
    (and claim-p (not support-p))))

(defun %cognitive-similarity (left right)
  (if *cognitive-call-similarity-fn*
      (funcall *cognitive-call-similarity-fn* left right)
      (cosine-similarity (embed-text left) (embed-text right))))

(defun %cognitive-recent-records (topic purpose)
  (cond (*cognitive-call-recent-records-fn*
         (funcall *cognitive-call-recent-records-fn* topic purpose))
        ((and (fboundp 'memory-search) topic)
         (memory-search topic :k 10 :mode :cognitive-evidence
                        :kinds '("thought" "reflection" "prediction" "worldview")))
        (t nil)))

(defun %cognitive-duplicate-p (content topic purpose)
  "Return the matching recent record, not merely a boolean, for safe merge lineage."
  (let ((threshold
          (if (member purpose *cognitive-call-loop-sensitive-purposes*
                      :test #'string=)
              *cognitive-call-loop-sensitive-duplicate-threshold*
              *cognitive-call-duplicate-threshold*)))
    (find-if (lambda (record)
               (let ((prior (and (hash-table-p record) (gethash "content" record))))
                 (and (stringp prior)
                      (>= (%cognitive-similarity content prior) threshold))))
        (%cognitive-recent-records topic purpose))))

(defun %cognitive-validate (record purpose evidence max-words topic)
  "Return status and reason; never repair semantic/identity failures."
  (let* ((required '("speaker" "human" "purpose" "record_type" "content"
                     "evidence_node_ids" "uncertainty" "novel_contribution"
                     "proposed_next_operation"))
         (evidence-ids (remove nil (mapcar #'%cognitive-evidence-id evidence)))
         (cited (%cognitive-list (and record (gethash "evidence_node_ids" record))))
         (cited-nodes
           (remove-if-not
            (lambda (node) (member (%cognitive-evidence-id node) cited
                                   :test #'string=)) evidence))
         (content (and record (gethash "content" record)))
         (uncertainty (and record (gethash "uncertainty" record))))
    (cond
      ((or (not (hash-table-p record))
           (and (hash-table-p record) (/= (hash-table-count record) (length required)))
           (some (lambda (key) (not (nth-value 1 (gethash key record)))) required)
           (not (stringp (gethash "speaker" record)))
           (not (stringp (gethash "human" record)))
           (not (stringp (gethash "purpose" record)))
           (not (stringp (gethash "record_type" record)))
           (not (stringp content)) (zerop (length content))
           (not (stringp (gethash "novel_contribution" record)))
           (zerop (length (gethash "novel_contribution" record)))
           (not (stringp (gethash "proposed_next_operation" record)))
           (not (numberp uncertainty)) (< uncertainty 0) (> uncertainty 1)
           (not (member (gethash "record_type" record)
                        *cognitive-call-record-types* :test #'string=))
           (zerop (length cited))
           (some (lambda (id) (not (stringp id))) cited)
           (> (%cognitive-word-count content) max-words))
       (values "rejected-schema" "schema-or-word-bound"))
      ((or (not (string= "the agent" (gethash "speaker" record)))
           (not (string= "the operator" (gethash "human" record)))
           (not (string= purpose (gethash "purpose" record)))
           (%cognitive-identity-confusion-p content))
       (values "rejected-identity" "role-or-identity"))
      ((or (some (lambda (id) (not (member id evidence-ids :test #'string=))) cited)
           (some (lambda (id)
                   (let ((node (find id evidence :key #'%cognitive-evidence-id
                                               :test #'string=)))
                     (not (%cognitive-evidence-valid-p node))))
                 cited)
           (not (%cognitive-has-lived-root-p cited-nodes))
           (%cognitive-unsupported-experience-p content cited-nodes))
       (values "rejected-grounding" "evidence-or-lived-root"))
      ((let ((duplicate (%cognitive-duplicate-p content topic purpose)))
         (when duplicate
           (return-from %cognitive-validate
             (values "rejected-duplicate" "similarity-threshold"
                     (gethash "id" duplicate)))))
       (values "rejected-duplicate" "similarity-threshold"))
      (t (values "accepted" "validated")))))

(defun %cognitive-prompt (purpose evidence question topic max-words generation-id)
  (list
   (obj "role" "system" "content"
        (format nil
                "You are the agent performing private cognitive work for the operator. Speaker must be exactly the agent; human must be exactly the operator. Purpose is exactly ~a. Use only the typed evidence supplied. Do not invent elapsed experience, observations, tool access, or evidence. Return one JSON object matching this schema and nothing else: ~a. Content is private data, never a message to the operator, and must be at most ~a words."
                purpose (shasht:write-json *cognitive-call-schema* nil) max-words))
   (obj "role" "user" "content"
        (shasht:write-json
         (obj "generation_id" generation-id "purpose" purpose
              "question" (or question :null) "topic" (or topic :null)
              "evidence" (coerce evidence 'vector)) nil))))

(defun %cognitive-repair-prompt (malformed purpose)
  (list
   (obj "role" "system" "content"
        (format nil
                "Repair the supplied malformed output into exactly one JSON object. Preserve its intended content; add no facts. Purpose must be ~a. Required schema: ~a. Return JSON only."
                purpose (shasht:write-json *cognitive-call-schema* nil)))
   (obj "role" "user" "content" malformed)))

(defun cognitive-call (purpose evidence &key question topic
                                           (max-words *cognitive-call-default-max-words*)
                                           generation-id model temperature)
  "Run a private structured cognitive call. The result is explicitly marked
not delivery eligible and is never persisted by this function."
  (let* ((purpose-name (%cognitive-purpose purpose))
         (evidence-list (%cognitive-list evidence))
         (generation (or generation-id
                         (format nil "cog-~a-~a" (get-universal-time) (random 1000000))))
         (*cognitive-current-purpose* purpose-name)
         (*cognitive-current-generation-id* generation)
         (evidence-ids (remove nil (mapcar #'%cognitive-evidence-id evidence-list)))
         (start (get-internal-real-time))
         (retries 0)
         (response nil)
         (record nil)
         (status "model-error")
         (reason "model-error")
         (duplicate-of-node-id nil)
         (usage (obj "prompt_tokens" :null "completion_tokens" :null
                     "total_tokens" :null "cost_usd" :null)))
    (%cognitive-log "cognitive-call-start"
                    (obj "generation_id" generation "purpose" purpose-name
                         "evidence_node_ids" (coerce evidence-ids 'vector)))
    (handler-case
        (progn
          (setf response (%cognitive-invoke-model
                          (%cognitive-prompt purpose-name evidence-list question topic
                                             max-words generation)
                          model temperature)
                usage (%cognitive-response-usage response))
          (let ((content (%cognitive-response-content response)))
            (cond
              ((or (null content) (eq content :null)
                   (and (stringp content) (zerop (length content))))
               (setf status "empty" reason "empty-model-content"))
              (t
               (setf record (%cognitive-parse-json-object content))
               (unless record
                 (incf retries)
                 (let* ((repair-response
                          (%cognitive-invoke-model
                           (%cognitive-repair-prompt content purpose-name)
                           model (or temperature 0.0d0)
                           :request-kind "repair"))
                        (repair-content (%cognitive-response-content repair-response)))
                   (setf record (%cognitive-parse-json-object repair-content))))
               (if record
                   (multiple-value-setq (status reason duplicate-of-node-id)
                     (%cognitive-validate record purpose-name evidence-list
                                          max-words (or topic question)))
                   (setf status "rejected-schema" reason "malformed-after-repair"))))))
      (error (condition)
        (setf status "model-error"
              reason (string-downcase (symbol-name (type-of condition))))))
    (let ((duration-ms (* 1000.0d0
                          (/ (- (get-internal-real-time) start)
                             (float internal-time-units-per-second 1.0d0)))))
      (%cognitive-stat status)
      (%cognitive-log
       "cognitive-call-end"
       (obj "generation_id" generation "purpose" purpose-name "status" status
            "reason" reason "duration_ms" duration-ms "retries" retries
            "duplicate_of_node_id" (or duplicate-of-node-id :null)
            "evidence_node_ids" (coerce evidence-ids 'vector)
            "prompt_tokens" (gethash "prompt_tokens" usage)
            "completion_tokens" (gethash "completion_tokens" usage)
            "total_tokens" (gethash "total_tokens" usage)
            "cost_usd" (gethash "cost_usd" usage)))
      (obj "status" status "reason" reason "generation_id" generation
           "purpose" purpose-name "delivery_eligible" nil
           "duplicate_of_node_id" (or duplicate-of-node-id :null)
           "record" (if (string= status "accepted") record :null)
           "retries" retries "duration_ms" duration-ms "usage" usage))))

(defun %cognitive-artifact-purpose (purpose)
  (let* ((normalized (string-downcase (string purpose)))
         (entry (assoc normalized *cognitive-artifact-purposes* :test #'string=)))
    (unless entry (error "Unsupported cognitive artifact purpose ~s." purpose))
    (values (car entry) (cdr entry))))

(defun %cognitive-artifact-max-words (operation-type supplied)
  (or supplied (if (string= operation-type "outline") 800 2500)))

(defun %cognitive-artifact-usage-complete-p (usage)
  (and (hash-table-p usage)
       (integerp (gethash "prompt_tokens" usage))
       (>= (gethash "prompt_tokens" usage) 0)
       (integerp (gethash "completion_tokens" usage))
       (>= (gethash "completion_tokens" usage) 0)
       (numberp (gethash "cost_usd" usage))
       (>= (gethash "cost_usd" usage) 0)))

(defun %cognitive-artifact-planning-leak-p (content)
  (some (lambda (pattern) (search pattern content :test #'char-equal))
        '("PLAN:" "OUTLINE:" "REASONING:" "SYSTEM PROMPT"
          "EVIDENCE_NODE_IDS" "PROPOSED_NEXT_OPERATION")))

(defun %cognitive-artifact-validate (record purpose operation-type evidence
                                     max-words project-contract prior-artifact usage)
  (declare (ignore prior-artifact))
  (let ((required '("speaker" "human" "purpose" "operation_type"
                    "artifact_content" "change_summary" "visibility"
                    "content_class"))
        (content (and (hash-table-p record) (gethash "artifact_content" record))))
    (cond
      ((or (not (hash-table-p record))
           (/= (hash-table-count record) (length required))
           (some (lambda (key) (not (nth-value 1 (gethash key record)))) required)
           (not (every (lambda (key) (stringp (gethash key record))) required))
           (zerop (length content))
           (zerop (length (gethash "change_summary" record)))
           (> (%cognitive-word-count content) max-words)
           (not (%cognitive-artifact-usage-complete-p usage)))
       (values "rejected-schema" "artifact-schema-word-bound-or-usage"))
      ((or (not (string= "the agent" (gethash "speaker" record)))
           (not (string= "the operator" (gethash "human" record)))
           (not (string= purpose (gethash "purpose" record)))
           (not (string= operation-type (gethash "operation_type" record)))
           (not (string= "private" (gethash "visibility" record)))
           (not (string= "synthetic" (gethash "content_class" record)))
           (%cognitive-identity-confusion-p content))
       (values "rejected-identity" "artifact-role-purpose-or-class"))
      ((or (null evidence)
           (some (lambda (node) (not (%cognitive-evidence-valid-p node))) evidence)
           (not (%cognitive-has-lived-root-p evidence)))
       (values "rejected-grounding" "artifact-evidence-or-lived-root"))
      ((or (not (hash-table-p project-contract))
           (not (string= operation-type
                         (or (gethash "operation_type" project-contract) "")))
           (not (stringp (gethash "required_change" project-contract))))
       (values "rejected-contract" "missing-or-mismatched-project-contract"))
      ((%cognitive-artifact-planning-leak-p content)
       (values "rejected-content" "planning-label-leakage"))
      (t (values "accepted" "validated")))))

(defun %cognitive-artifact-evidence-view (evidence)
  (coerce
   (mapcar
    (lambda (node)
      (obj "id" (gethash "id" node)
           "origin_class" (gethash "origin_class" node)
           "epistemic_status" (gethash "epistemic_status" node)
           "grounding_status" (gethash "grounding_status" node)
           "excerpt" (or (gethash "content" node)
                         (gethash "summary" node) :null)))
    evidence)
   'vector))

(defun %cognitive-artifact-prompt (purpose operation-type evidence
                                   project-contract prior-artifact max-words generation-id)
  (list
   (obj "role" "system" "content"
        (format nil
                "Perform one private synthetic story-artifact operation for the agent. Return exactly one JSON object and no prose outside it. Speaker is the agent; human is the operator; purpose is ~a; operation_type is ~a; visibility is private; content_class is synthetic. The artifact is fictional text, not an epistemic record or a message to the operator. Use only the bounded inspiration and exact prior artifact supplied. Do not include planning labels. Maximum artifact words: ~a. Schema: ~a"
                purpose operation-type max-words
                (shasht:write-json *cognitive-artifact-schema* nil)))
   (obj "role" "user" "content"
        (shasht:write-json
         (obj "generation_id" generation-id
              "project_contract" project-contract
              "grounded_inspiration" (%cognitive-artifact-evidence-view evidence)
              "prior_artifact" (or prior-artifact :null)) nil))))

(defun cognitive-artifact-call
    (purpose evidence &key project-contract prior-artifact max-words
                        generation-id model temperature
                        (timeout-seconds *cognitive-artifact-timeout-seconds*))
  "Run one tool-free artifact-generation call. Unlike COGNITIVE-CALL, this API
can return only private synthetic artifact data and never an epistemic record."
  (multiple-value-bind (purpose-name operation-type)
      (%cognitive-artifact-purpose purpose)
    (unless (and (numberp timeout-seconds) (> timeout-seconds 0)
                 (<= timeout-seconds 120))
      (error "Artifact timeout must be greater than zero and at most 120 seconds."))
    (let* ((evidence-list (%cognitive-list evidence))
           (limit (%cognitive-artifact-max-words operation-type max-words))
           (generation (or generation-id
                           (format nil "artifact-~a-~a" (get-universal-time)
                                   (random 1000000))))
           (*cognitive-current-purpose* purpose-name)
           (*cognitive-current-generation-id* generation)
           (start (get-internal-real-time))
           (response nil)
           (record nil)
           (status "model-error")
           (reason "model-error")
           (preflight-reason
             (cond
               ((or (null evidence-list)
                    (some (lambda (node) (not (%cognitive-evidence-valid-p node)))
                          evidence-list)
                    (not (%cognitive-has-lived-root-p evidence-list)))
                "artifact-evidence-or-lived-root")
               ((or (not (hash-table-p project-contract))
                    (not (string= operation-type
                                  (or (gethash "operation_type" project-contract) "")))
                    (not (stringp (gethash "required_change" project-contract))))
                "missing-or-mismatched-project-contract")))
           (usage (obj "prompt_tokens" :null "completion_tokens" :null
                       "total_tokens" :null "cost_usd" :null)))
      (%cognitive-log
       "cognitive-artifact-call-start"
       (obj "generation_id" generation "purpose" purpose-name
            "operation_type" operation-type
            "evidence_node_ids"
            (coerce (remove nil (mapcar #'%cognitive-evidence-id evidence-list))
                    'vector)))
      (if preflight-reason
          (setf status (if (string= preflight-reason
                                   "artifact-evidence-or-lived-root")
                           "rejected-grounding" "rejected-contract")
                reason preflight-reason)
          (handler-case
            (progn
            (setf response
                  (sb-ext:with-timeout timeout-seconds
                    (%cognitive-invoke-model
                     (%cognitive-artifact-prompt
                      purpose-name operation-type evidence-list project-contract
                      prior-artifact limit generation)
                     model temperature :request-kind "artifact"))
                  usage (%cognitive-response-usage response)
                  record (%cognitive-parse-json-object
                          (%cognitive-response-content response)))
            (multiple-value-setq (status reason)
                (%cognitive-artifact-validate
                 record purpose-name operation-type evidence-list limit
                 project-contract prior-artifact usage)))
            (sb-ext:timeout ()
              (setf status "timed-out" reason "artifact-model-timeout"))
            (error (condition)
              (setf status "model-error"
                    reason (string-downcase (symbol-name (type-of condition)))))))
      (let ((duration-ms (* 1000.0d0
                            (/ (- (get-internal-real-time) start)
                               (float internal-time-units-per-second 1.0d0)))))
        (%cognitive-stat (format nil "artifact/~a" status))
        (%cognitive-log
         "cognitive-artifact-call-end"
         (obj "generation_id" generation "purpose" purpose-name
              "operation_type" operation-type "status" status "reason" reason
              "duration_ms" duration-ms
              "prompt_tokens" (gethash "prompt_tokens" usage)
              "completion_tokens" (gethash "completion_tokens" usage)
              "cost_usd" (gethash "cost_usd" usage)))
        (obj "status" status "reason" reason "generation_id" generation
             "purpose" purpose-name "operation_type" operation-type
             "delivery_eligible" nil
             "record" (if (string= status "accepted") record :null)
             "duration_ms" duration-ms "usage" usage)))))
