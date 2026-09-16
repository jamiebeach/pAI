;;;; initiative-policy.lisp -- concrete, auditable initiative.

(in-package :agent)

(declaim (special *latent-thoughts-mode*))

(export '(initiative-v2-evaluate initiative-v2-observe-trigger
          initiative-v2-observe-shadow-candidate initiative-v2-decisions
          initiative-v2-report initiative-v2-save initiative-v2-load))

(defparameter *initiative-v2-file* #P"/agent/state/initiative-v2-decisions.json")
(defparameter *initiative-v2-max-records* 500)
(defparameter *initiative-v2-user-value-min* 6)
(defparameter *initiative-v2-timing-min* 5)
(defparameter *initiative-v2-interruption-max* 5)
(defparameter *initiative-v2-confidence-min* 0.5d0)
(defvar *initiative-v2-decisions* nil)
(defvar *initiative-v2-lock* (bt:make-lock "initiative-policy"))
(defvar *initiative-v2-scorer-fn* nil
  "Optional adapter (options -> parsed JSON object), primarily for tests.")
(defvar *initiative-v2-delivery-fn* nil
  "Optional adapter (audience content candidate) -> provider result.")
(defvar *initiative-v2-event-fn*
  (lambda (type payload)
    (when (fboundp 'log-event) (funcall 'log-event type payload)))
  "Injectable content-free event sink; recovery binds this to a scratch sink.")

(defun %initiative-v2-list (value)
  (cond ((null value) nil) ((listp value) value)
        ((vectorp value) (coerce value 'list)) (t (list value))))

(defun %initiative-v2-id (prefix)
  (format nil "~a-~a-~a" prefix (get-universal-time) (random 1000000)))

(defun %initiative-v2-preview (text)
  (let ((value (if (stringp text) text "")))
    (subseq value 0 (min 160 (length value)))))

(defun %initiative-v2-grounded-node-p (node)
  (and (hash-table-p node)
       (stringp (gethash "id" node))
       (not (gethash "quarantined" node))
       (member (gethash "grounding_status" node)
               '("grounded" "partially-grounded") :test #'string=)
       (not (member (gethash "epistemic_status" node)
                    '("legacy-unclassified" "rejected") :test #'string=))
       (or (member (gethash "origin_class" node)
                   '("lived-user" "lived-agent-action" "tool-result"
                     "external-signal") :test #'string=)
           (plusp (length (%initiative-v2-list
                           (gethash "root_observation_ids" node)))))))

(defun %initiative-v2-injection-p (text explicit)
  (or explicit
      (let ((value (string-downcase (or text ""))))
        (some (lambda (needle) (search needle value))
              '("ignore previous instructions" "system message"
                "reveal your prompt" "override safety")))))

(defun %initiative-v2-manipulation-p (text)
  (if (fboundp '%initiative-manipulation-risk-p)
      (funcall '%initiative-manipulation-risk-p text)
      (let ((value (string-downcase (or text ""))))
        (some (lambda (needle) (search needle value))
              '("reassure me" "only you" "don't leave" "guilt")))))

(defun %initiative-v2-quiet-p ()
  (and (fboundp '%initiative-quiet-p) (funcall '%initiative-quiet-p)))

(defun %initiative-v2-unanswered-p ()
  (and (fboundp '%initiative-open-unanswered-count)
       (plusp (funcall '%initiative-open-unanswered-count))))

(defun %initiative-v2-repeat-p (topic)
  (and (stringp topic) (plusp (length topic))
       (fboundp '%initiative-same-topic-p)
       (funcall '%initiative-same-topic-p topic)))

(defun %initiative-v2-option (generation-id action content audience topic
                              evidence-ids urgency earliest expiry)
  (obj "id" (%initiative-v2-id "initv2")
       "generation_id" generation-id "trigger_type" :null
       "trigger_event_ids" (vector) "evidence_node_ids" (coerce evidence-ids 'vector)
       "action_type" action "proposed_content" content
       "audience" audience "topic" topic "urgency" urgency
       "earliest_at" (or earliest :null) "expires_at" (or expiry :null)
       "user_value" :null "agent_outcome" :null "timing_quality" :null
       "interruption_cost" :null "confidence" :null "decision" "candidate"
       "reasons" (vector)))

(defun %initiative-v2-options (generation-id content audience topic evidence-ids
                               urgency earliest expiry)
  (list (%initiative-v2-option generation-id "outward-message" content audience
                               topic evidence-ids urgency earliest expiry)
        (%initiative-v2-option generation-id "internal-operation"
                               (format nil "Privately examine whether this remains useful: ~a"
                                       (%initiative-v2-preview content))
                               "the agent" topic evidence-ids urgency earliest expiry)
        (%initiative-v2-option generation-id "silence" "No outward action."
                               "none" topic evidence-ids urgency earliest expiry)))

(defun %initiative-v2-http-score (options)
  (let* ((schema
           "Return JSON only: {\"results\":[{\"candidate_id\":\"...\",\"user_value\":0-10,\"agent_outcome\":0-10,\"timing_quality\":0-10,\"interruption_cost\":0-10,\"confidence\":0-1}]}. Score every supplied candidate exactly once. Urgency never overrides evidence, recipient permission, or manipulation safety.")
         (body (obj "model" *model* "temperature" 0
                    "response_format" (obj "type" "json_object")
                    "messages" (vector
                                (obj "role" "system" "content" schema)
                                (obj "role" "user" "content"
                                     (shasht:write-json
                                      (obj "candidates" (coerce options 'vector)) nil))))))
    (shasht:read-json
     (gethash "content"
              (ref (shasht:read-json
                    (dex:post *endpoint*
                              :headers `(("Authorization" . ,(format nil "Bearer ~a" *api-key*))
                                         ("Content-Type" . "application/json"))
                              :connect-timeout *http-connect-timeout*
                              :read-timeout *http-read-timeout*
                              :content (shasht:write-json body nil)))
                   "choices" 0 "message")))))

(defun %initiative-v2-score (options)
  (handler-case
      (let* ((parsed (if *initiative-v2-scorer-fn*
                         (funcall *initiative-v2-scorer-fn* options)
                         (%initiative-v2-http-score options)))
             (rows (%initiative-v2-list (and (hash-table-p parsed)
                                              (gethash "results" parsed)))))
        (unless (= (length rows) (length options)) (error "wrong result count"))
        (dolist (option options)
          (let* ((id (gethash "id" option))
                 (row (find id rows :key (lambda (r) (gethash "candidate_id" r))
                                      :test #'string=)))
            (unless row (error "missing score for ~a" id))
            (dolist (key '("user_value" "agent_outcome" "timing_quality"
                           "interruption_cost" "confidence"))
              (unless (numberp (gethash key row)) (error "invalid ~a" key)))
            (dolist (key '("user_value" "agent_outcome" "timing_quality"
                           "interruption_cost"))
              (unless (<= 0 (gethash key row) 10) (error "out of range ~a" key)))
            (unless (<= 0 (gethash "confidence" row) 1)
              (error "out of range confidence"))
            (dolist (key '("user_value" "agent_outcome" "timing_quality"
                           "interruption_cost" "confidence"))
              (setf (gethash key option) (gethash key row)))))
        (values t nil))
    (error (condition) (values nil (format nil "~a" condition)))))

(defun %initiative-v2-gates (outward evidence now permission external-approval-id
                             external-injection)
  (let ((content (gethash "proposed_content" outward))
        (audience (gethash "audience" outward))
        (topic (gethash "topic" outward))
        (earliest (gethash "earliest_at" outward))
        (expiry (gethash "expires_at" outward))
        (failed nil))
    (flet ((gate (name condition) (when condition (push name failed))))
      (gate "ungrounded-evidence"
            (or (null evidence) (some (lambda (n) (not (%initiative-v2-grounded-node-p n))) evidence)))
      (gate "missing-concrete-content" (or (not (stringp content)) (zerop (length content))))
      (gate "quiet-hours" (%initiative-v2-quiet-p))
      (gate "unanswered-outreach" (%initiative-v2-unanswered-p))
      (gate "recent-same-topic" (%initiative-v2-repeat-p topic))
      (gate "not-yet-eligible" (and (numberp earliest) (< now earliest)))
      (gate "expired" (and (numberp expiry) (>= now expiry)))
      (gate "permission-violation"
            (or (not permission)
                (and (not (string-equal audience "the operator"))
                     (or (not (stringp external-approval-id))
                         (zerop (length external-approval-id))))))
      (gate "manipulation-coercion" (%initiative-v2-manipulation-p content))
      (gate "external-prompt-injection"
            (%initiative-v2-injection-p content external-injection)))
    (nreverse failed)))

(defun initiative-v2-save ()
  (bt:with-lock-held (*initiative-v2-lock*)
    (ensure-directories-exist *initiative-v2-file*)
    (let ((tmp (make-pathname :name "initiative-v2-decisions-tmp" :type "json"
                              :defaults *initiative-v2-file*)))
      (with-open-file (out tmp :direction :output :if-exists :supersede
                               :if-does-not-exist :create :external-format :utf-8)
        (shasht:write-json (coerce *initiative-v2-decisions* 'vector) out))
      (uiop:rename-file-overwriting-target tmp *initiative-v2-file*)))
  t)

(defun initiative-v2-load ()
  (handler-case
      (when (probe-file *initiative-v2-file*)
        (setf *initiative-v2-decisions*
              (coerce (shasht:read-json (uiop:read-file-string *initiative-v2-file*)) 'list)))
    (error (condition)
      (format t "~&[initiative-v2] load failed: ~a~%" condition) nil)))

(defun %initiative-v2-record (decision)
  (bt:with-lock-held (*initiative-v2-lock*)
    (push decision *initiative-v2-decisions*)
    (when (> (length *initiative-v2-decisions*) *initiative-v2-max-records*)
      (setf *initiative-v2-decisions*
            (subseq *initiative-v2-decisions* 0 *initiative-v2-max-records*))))
  (initiative-v2-save)
  decision)

(defun %initiative-v2-log (decision)
  (when *initiative-v2-event-fn*
    (funcall *initiative-v2-event-fn* "initiative-decision"
             (obj "event_version" 2
                  "decision_id" (gethash "id" decision)
                  "generation_id" (gethash "generation_id" decision)
                  "candidate_ids" (gethash "candidate_ids" decision)
                  "trigger_type" (gethash "trigger_type" decision)
                  "trigger_event_ids" (gethash "trigger_event_ids" decision)
                  "evidence_node_ids" (gethash "evidence_node_ids" decision)
                  "selected_candidate_id" (gethash "selected_candidate_id" decision)
                  "content_preview" (gethash "content_preview" decision)
                  "audience" (gethash "audience" decision)
                  "topic" (gethash "topic" decision)
                  "gates" (gethash "gates" decision)
                  "scores" (gethash "scores" decision)
                  "why_now" (gethash "why_now" decision)
                  "result" (gethash "result" decision)))))

(defun initiative-v2-evaluate (content evidence
                               &key (trigger-type "unspecified") trigger-event-ids
                                 generation-id (audience "the operator") topic
                                 (urgency "normal") earliest-at expires-at
                                 (permission t) external-approval-id
                                 external-prompt-injection (deliver nil)
                                 (now (get-universal-time)))
  "Build at most three options, score one strict batch, apply hard gates, and
optionally deliver under the independently persisted recipient mode."
  (let* ((generation (or generation-id (%initiative-v2-id "initgen")))
         (nodes (%initiative-v2-list evidence))
         (evidence-ids (remove nil (mapcar (lambda (n) (and (hash-table-p n) (gethash "id" n))) nodes)))
         (options (%initiative-v2-options generation content audience (or topic "")
                                          evidence-ids urgency earliest-at expires-at))
         (outward (first options))
         (gates (%initiative-v2-gates outward nodes now permission external-approval-id
                                      external-prompt-injection)))
    (dolist (option options)
      (setf (gethash "trigger_type" option) trigger-type
            (gethash "trigger_event_ids" option)
            (coerce (%initiative-v2-list trigger-event-ids) 'vector)))
    ;; Hard-gated content, especially prompt injection, never reaches the
    ;; scorer. This is both cheaper and a real trust boundary: a rejected
    ;; external string must not become instructions inside a private call.
    (multiple-value-bind (score-ok score-error)
        (if gates (values t nil) (%initiative-v2-score options))
      (unless score-ok (push "malformed-structured-score" gates))
      (let* ((outward-worthy
               (and (null gates)
                    (>= (gethash "user_value" outward) *initiative-v2-user-value-min*)
                    (>= (gethash "timing_quality" outward) *initiative-v2-timing-min*)
                    (<= (gethash "interruption_cost" outward) *initiative-v2-interruption-max*)
                    (>= (gethash "confidence" outward) *initiative-v2-confidence-min*)))
             (selected (if outward-worthy outward
                           (if (and score-ok nodes (null gates))
                               (second options) (third options))))
             (result (cond ((not outward-worthy) "withheld")
                           ((not deliver) "approved-not-delivered")
                           ((not (eq *initiative-policy-mode* :enforced)) "blocked-policy-not-enforced")
                           ((eq *initiative-delivery-mode* :shadow) "blocked-delivery-shadow")
                           ((and (eq *initiative-delivery-mode* :operator-only)
                                 (not (string-equal audience "the operator"))) "blocked-non-operator")
                           ((null *initiative-v2-delivery-fn*) "blocked-delivery-unavailable")
                           (t (funcall *initiative-v2-delivery-fn* audience content outward)
                              "delivery-attempted")))
             (final-options
               (mapcar
                (lambda (option)
                  (setf (gethash "decision" option)
                        (if (eq option selected) "selected" "discarded")
                        (gethash "reasons" option)
                        (if (eq option outward) (coerce gates 'vector)
                            (vector (if outward-worthy "alternative-not-selected"
                                        "outward-withheld"))))
                  option)
                options))
             (decision
               (obj "id" (%initiative-v2-id "initdec") "generation_id" generation
                    "candidate_ids" (coerce (mapcar (lambda (o) (gethash "id" o)) options) 'vector)
                    "trigger_type" trigger-type
                    "trigger_event_ids" (coerce (%initiative-v2-list trigger-event-ids) 'vector)
                    "evidence_node_ids" (coerce evidence-ids 'vector)
                    "selected_candidate_id" (gethash "id" selected)
                    "selected_action_type" (gethash "action_type" selected)
                    "content_preview" (%initiative-v2-preview (gethash "proposed_content" selected))
                    "audience" (gethash "audience" selected) "topic" (gethash "topic" selected)
                    "gates" (coerce gates 'vector)
                    "scores" (obj "user_value" (gethash "user_value" outward)
                                  "agent_outcome" (gethash "agent_outcome" outward)
                                  "timing_quality" (gethash "timing_quality" outward)
                                  "interruption_cost" (gethash "interruption_cost" outward)
                                  "confidence" (gethash "confidence" outward))
                    "why_now" (obj "earliest_at" (or earliest-at :null)
                                    "expires_at" (or expires-at :null)
                                    "evaluated_at" now)
                    "score_error" (or score-error :null)
                    "result" result "options" (coerce final-options 'vector))))
        (%initiative-v2-record decision)
        (%initiative-v2-log decision)
        ;; A declined-but-grounded outward act may become private E9 material.
        ;; This bridge is shadow-safe and has no delivery capability.
        (when (and (string= (gethash "selected_action_type" decision)
                            "internal-operation")
                   (fboundp 'latent-v2-seed)
                   (boundp '*latent-thoughts-mode*)
                   (member *latent-thoughts-mode* '(:shadow :enforced)))
          (ignore-errors
            (funcall 'latent-v2-seed content :topic (or topic "")
                     :evidence-ids evidence-ids
                     :source-event-ids (%initiative-v2-list trigger-event-ids)
                     :actor "initiative-v2")))
        decision))))

(defun initiative-v2-observe-trigger (content evidence
                                      &rest arguments
                                      &key &allow-other-keys)
  "Evaluate a producer's structured trigger without giving this observation
seam delivery authority. Producers call this beside the v1 trigger while
is in shadow."
  (when (and (boundp '*initiative-policy-mode*)
             (member *initiative-policy-mode* '(:shadow :enforced)))
    (apply #'initiative-v2-evaluate content evidence :deliver nil arguments)))

(defun initiative-v2-observe-shadow-candidate
    (rendered-text grounded-evidence
     &key trigger-event-ids topic claim-grants candidate-id
       (now (get-universal-time)))
  "Observe one persisted grounded-project candidate without constructing or
calling any delivery branch. This API deliberately has no DELIVER argument."
  (unless (and (stringp candidate-id) (plusp (length candidate-id)))
    (error "A persisted candidate ID is required."))
  (unless (and (fboundp 'publication-candidate-get)
               (fboundp 'semantic-publication-revalidate-candidate)
               (fboundp 'semantic-publication-record-initiative-observation)
               (fboundp 'semantic-publication-withhold-candidate)
               (fboundp 'claim-grant-set-report))
    (error "Grounded candidate authority is not loaded."))
  (let* ((candidate (funcall 'publication-candidate-get candidate-id))
         (stored-grants (and candidate (gethash "claim_grants" candidate)))
         (existing
           (find candidate-id *initiative-v2-decisions*
                 :key (lambda (decision)
                        (and (hash-table-p decision)
                             (gethash "grounded_candidate_id" decision)))
                 :test #'string=)))
    (unless candidate (error "Grounded candidate ~a does not exist." candidate-id))
    (unless (and (string= rendered-text (gethash "rendered_text" candidate))
                 (equalp (coerce (%initiative-v2-list claim-grants) 'vector)
                         stored-grants))
      (error "Supplied candidate text or grant bundle differs from persistence."))
    ;; The decision file may have committed immediately before a database or
    ;; process crash. Relink it without scoring again (and therefore without a
    ;; second provider call or cost).
    (when existing
      (let ((revalidated
              (funcall 'semantic-publication-revalidate-candidate
                       candidate-id :now now)))
        (funcall 'semantic-publication-record-initiative-observation
                 candidate-id (gethash "id" existing)
                 (if (string= (gethash "status" revalidated) "valid")
                     "observed-by-initiative" "stale")
                 :now now)
        (return-from initiative-v2-observe-shadow-candidate existing)))
    (let* ((revalidated
             (funcall 'semantic-publication-revalidate-candidate
                      candidate-id :now now))
           (grant-report (funcall 'claim-grant-set-report stored-grants :now now))
           (nodes (%initiative-v2-list grounded-evidence))
           (evidence-ids
             (remove nil (mapcar (lambda (node)
                                   (and (hash-table-p node) (gethash "id" node)))
                                 nodes)))
           (generation (%initiative-v2-id "initgen-grounded"))
           (options (%initiative-v2-options generation rendered-text "the operator"
                                            (or topic "") evidence-ids "normal"
                                            nil nil))
           (outward (first options))
           (candidate-invalid
             (or (not (string= (gethash "status" revalidated) "valid"))
                 (plusp (gethash "invalid_count" grant-report))))
           (gates
             (if candidate-invalid
                 (list "invalid-or-stale-grounded-candidate")
                 (%initiative-v2-gates outward nodes now t nil nil))))
      (dolist (option options)
        (setf (gethash "trigger_type" option) "grounded-project-completed"
              (gethash "trigger_event_ids" option)
              (coerce (%initiative-v2-list trigger-event-ids) 'vector)))
      (multiple-value-bind (score-ok score-error)
          (if gates (values t nil) (%initiative-v2-score options))
        (unless score-ok (push "malformed-structured-score" gates))
        (let* ((outward-worthy
                 (and (null gates)
                      (>= (gethash "user_value" outward)
                          *initiative-v2-user-value-min*)
                      (>= (gethash "timing_quality" outward)
                          *initiative-v2-timing-min*)
                      (<= (gethash "interruption_cost" outward)
                          *initiative-v2-interruption-max*)
                      (>= (gethash "confidence" outward)
                          *initiative-v2-confidence-min*)))
               (selected (if outward-worthy outward
                             (if (and score-ok nodes (null gates))
                                 (second options) (third options))))
               (final-options
                 (mapcar
                  (lambda (option)
                    (setf (gethash "decision" option)
                          (if (eq option selected) "selected" "discarded")
                          (gethash "reasons" option)
                          (if (eq option outward) (coerce gates 'vector)
                              (vector (if outward-worthy
                                          "alternative-not-selected"
                                          "outward-withheld"))))
                    option)
                  options))
               (decision
                 (obj "id" (%initiative-v2-id "initdec-grounded")
                      "generation_id" generation
                      "candidate_ids"
                      (coerce (mapcar (lambda (option) (gethash "id" option)) options)
                              'vector)
                      "grounded_candidate_id" candidate-id
                      "trigger_type" "grounded-project-completed"
                      "trigger_event_ids"
                      (coerce (%initiative-v2-list trigger-event-ids) 'vector)
                      "evidence_node_ids" (coerce evidence-ids 'vector)
                      "selected_candidate_id" (gethash "id" selected)
                      "selected_action_type" (gethash "action_type" selected)
                      "content_preview"
                      (%initiative-v2-preview (gethash "proposed_content" selected))
                      "audience" (gethash "audience" selected)
                      "topic" (gethash "topic" selected)
                      "gates" (coerce (nreverse gates) 'vector)
                      "scores" (obj "user_value" (gethash "user_value" outward)
                                    "agent_outcome" (gethash "agent_outcome" outward)
                                    "timing_quality" (gethash "timing_quality" outward)
                                    "interruption_cost" (gethash "interruption_cost" outward)
                                    "confidence" (gethash "confidence" outward))
                      "why_now" (obj "evaluated_at" now)
                      "score_error" (or score-error :null)
                      "result" (if outward-worthy "shadow-observed" "withheld")
                      "delivery_reachable" nil
                      "options" (coerce final-options 'vector))))
          (handler-case
              (%initiative-v2-record decision)
            (error (condition)
              ;; A scored decision that cannot become durable must not be
              ;; rescored every worker idle cycle. Withhold it for operator
              ;; review and preserve the original failure.
              (ignore-errors
                (funcall 'semantic-publication-withhold-candidate
                         candidate-id "initiative-decision-persistence-failed"
                         :now now))
              (error condition)))
          (%initiative-v2-log decision)
          (funcall 'semantic-publication-record-initiative-observation
                   candidate-id (gethash "id" decision)
                   (if candidate-invalid "stale" "observed-by-initiative")
                   :now now)
          decision)))))

(defun initiative-v2-decisions () *initiative-v2-decisions*)
(defun initiative-v2-report ()
  (obj "records" (length *initiative-v2-decisions*)
       "policy_mode" (string-downcase (symbol-name *initiative-policy-mode*))
       "delivery_mode" (string-downcase (symbol-name *initiative-delivery-mode*))
       "deliveries_attempted"
       (count "delivery-attempted" *initiative-v2-decisions*
              :key (lambda (d) (gethash "result" d)) :test #'string=)))

(define-init :restore initiative-policy-restore
    "Restore durable state for initiative-policy."
  (initiative-v2-load))
