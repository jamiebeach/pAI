;;;; epistemic-critic.lisp -- bounded semantic entailment/removal gate.
;;;;
;;;; This is deliberately not a general model router. It sends one tool-free,
;;;; batched request containing only typed audit facts and the small temporal
;;;; fragments already admitted by PUBLICATION-CONTRACT. It never sees a user
;;;; prompt, conversation history, private context, or the factual nucleus.
;;;; Production defaults to :OFF. In :ENFORCED, the model may only remove
;;;; candidate fragments; it cannot author text or alter the code-owned factual
;;;; nucleus. A malformed response, model error, or exhausted budget rejects
;;;; all fragments, while :SHADOW leaves public speech byte-identical.

(in-package :agent)

(export '(epistemic-critic-review epistemic-critic-observe
          epistemic-critic-realize
          epistemic-critic-report))

(defvar *epistemic-critic-mode* :off)
(defparameter *epistemic-critic-model* "z-ai/glm-5.2")
(defparameter *epistemic-critic-call-cap-24h* 20)
(defparameter *epistemic-critic-cost-cap-usd-24h* 0.05d0)
(defparameter *epistemic-critic-reserved-cost-usd* 0.003d0)
(defparameter *epistemic-critic-max-fragments* 4)
(defparameter *epistemic-critic-temporal-max-fragments* 2)
(defparameter *epistemic-critic-max-tokens* 512)
(defparameter *epistemic-critic-reason-codes*
  '("entailed" "relational-only" "overclaim-no-evidence"
    "contradicts-facts" "unsupported-specific"))
(defparameter *epistemic-critic-check-in-risk-phrases*
  '("background" "while you were away" "while you were gone" "overnight"
    "last night" "earlier today" "this morning i " "kept things"
    "kept busy" "maintenance" "worked on" "completed" "finished"
    "updated" "reviewed" "researched" "monitored" "checked on"
    "looked into" "made progress" "took care of" "handled" "logged"
    "recorded" "remembered" "recalled" "my memory" "i did " "i was "
    "i spent " "i had " "i thought " "i noticed " "i've " "i have "
    "keeping " "kept " "staying busy" "been here" "kept the lights"
    "keeping the lights")
  "High-recall signals that a social draft crossed into evidence-bearing claims.")
(defvar *epistemic-critic-lock* (bt:make-lock "epistemic-critic"))
(defvar *epistemic-critic-model-fn* nil
  "Test/adapter function: (messages model temperature max-tokens) -> response.")
(defvar *epistemic-critic-events-fn*
  (lambda (&key from)
    (if (fboundp 'replay-events) (funcall 'replay-events :from from) nil)))
(defvar *epistemic-critic-event-fn*
  (lambda (type payload)
    (when (fboundp 'log-event) (funcall 'log-event type payload))))

(defun %epistemic-critic-mode ()
  (if (member *epistemic-critic-mode* '(:off :shadow :enforced))
      *epistemic-critic-mode*
      :off))

(defun %epistemic-critic-list (value)
  (cond ((null value) nil) ((listp value) value)
        ((vectorp value) (coerce value 'list)) (t (list value))))

(defun %epistemic-critic-number (value)
  (if (numberp value) (coerce value 'double-float) 0.0d0))

(defun %epistemic-critic-event-payload (event)
  (and (hash-table-p event) (gethash "payload" event)))

(defun %epistemic-critic-budget ()
  (let ((calls 0) (committed 0.0d0) (actual 0.0d0)
        (from (- (get-universal-time) (* 24 60 60))))
    (dolist (event (ignore-errors
                     (funcall *epistemic-critic-events-fn* :from from)))
      (let ((type (and (hash-table-p event) (gethash "type" event)))
            (payload (%epistemic-critic-event-payload event)))
        (when (and (stringp type) (hash-table-p payload))
          (cond ((string= type "epistemic-critic-call")
                 (incf calls)
                 (incf committed
                       (%epistemic-critic-number
                        (gethash "reserved_cost_usd" payload))))
                ((string= type "epistemic-critic-result")
                 (incf committed
                       (%epistemic-critic-number
                        (gethash "overage_cost_usd" payload)))
                 (incf actual
                       (%epistemic-critic-number
                        (gethash "actual_cost_usd" payload))))))))
    (obj "calls_24h" calls "committed_cost_usd_24h" committed
         "actual_cost_usd_24h" actual
         "call_cap_24h" *epistemic-critic-call-cap-24h*
         "cost_cap_usd_24h" *epistemic-critic-cost-cap-usd-24h*)))

(defun %epistemic-critic-budget-available-p (budget)
  (and (< (gethash "calls_24h" budget 0)
          *epistemic-critic-call-cap-24h*)
       (<= (+ (gethash "committed_cost_usd_24h" budget 0.0d0)
              *epistemic-critic-reserved-cost-usd*)
           *epistemic-critic-cost-cap-usd-24h*)))

(defun %epistemic-critic-facts (contract)
  (let* ((intent (and (hash-table-p contract)
                      (gethash "intent" contract "conversation")))
         (facts (and (hash-table-p contract) (gethash "facts" contract)))
         (status (and (hash-table-p facts)
                      (gethash "audit_status" facts "unavailable")))
         (count (and (hash-table-p facts) (gethash "event_count" facts 0)))
         (types (%epistemic-critic-list
                 (and (hash-table-p facts) (gethash "event_types" facts))))
         (items nil))
    (labels ((add (id text) (push (obj "id" id "text" text) items)))
      (cond ((string= intent "check-in")
             (add "intent-policy" "This is a present social check-in. Warm relational language and explicitly subjective present-tense perspective are allowed without external evidence.")
             (add "activity-boundary" "No historical or background activity, maintenance, memory, or process facts are supplied.")
             (add "self-disclosure-boundary" "A statement framed as current mood or perspective may be relational-only; a claim that background work, maintenance, or drift occurred is unsupported."))
            ((and (stringp status) (string= status "complete"))
             (add "audit-status" "The bounded audit completed for the configured event types and requested interval.")
             (add "audit-count" (format nil "The audit recorded ~d configured background-activity event~:p in that interval." count))
             (when types
               (add "audit-types"
                    (format nil "The recorded event types are: ~{~a~^, ~}." types)))
             (add "audit-boundary" "The bounded audit does not observe every possible process, state, thought, or subjective experience.")
             (add "audit-scope" "Only the configured background-event types are in scope. The audit cannot support an unqualified claim that nothing was logged, recorded, or happened."))
            (t
             (add "audit-status" "The bounded audit source was unavailable for the requested interval.")
             (add "audit-boundary" "No conclusion about recorded or unrecorded activity can be drawn from the unavailable audit.")))
      (add "inference-policy" "Absence of recorded events does not prove absence of activity, thought, state, or experience.")
      (nreverse items))))

(defun %epistemic-critic-candidates (draft contract)
  (let* ((intent (and (hash-table-p contract)
                      (gethash "intent" contract "conversation")))
         (limit (if (and (stringp intent)
                         (string= intent "temporal-report"))
                    *epistemic-critic-temporal-max-fragments*
                    *epistemic-critic-max-fragments*)))
    (if (and (member intent '("temporal-report" "check-in") :test #'string=)
             (fboundp '%publication-contract-safe-fragments))
        (loop for text in (funcall '%publication-contract-safe-fragments
                                   draft contract limit)
              for index from 1
              collect (obj "fragment_id" (format nil "fragment-~d" index)
                           "proposed_fragment" text))
        nil)))

(defun %epistemic-critic-check-in-risk-p (draft contract)
  ;; The deterministic publication boundary runs first. Do not spend a model
  ;; call on a risky fragment that code has already removed.
  (let ((text
          (string-downcase
           (format nil "~{~a~^ ~}"
                   (if (fboundp '%publication-contract-safe-fragments)
                       (funcall '%publication-contract-safe-fragments
                                draft contract
                                *epistemic-critic-max-fragments*)
                       nil)))))
    (some (lambda (phrase) (search phrase text))
          *epistemic-critic-check-in-risk-phrases*)))

(defun %epistemic-critic-review-required-p (draft contract)
  "Route only the evidence-bearing temporal domain to the specialist model.

Temporal reports always require semantic review. Social check-ins remain on the
primary conversational model and use deterministic removal for past/background
activity, maintenance, memory, or observation claims."
  (declare (ignore draft))
  (let ((intent (and (hash-table-p contract)
                     (gethash "intent" contract "conversation"))))
    (string= intent "temporal-report")))

(defun %epistemic-critic-http-call (messages)
  (let ((body (obj "model" *epistemic-critic-model*
                   "messages" (coerce messages 'vector)
                   "temperature" 0
                   "max_tokens" *epistemic-critic-max-tokens*
                   "reasoning" (obj "enabled" nil "exclude" t)
                   "response_format" (obj "type" "json_object"))))
    (shasht:read-json
     (dex:post *endpoint*
               :headers `(("Authorization" . ,(format nil "Bearer ~a" *api-key*))
                          ("Content-Type" . "application/json"))
               :connect-timeout *http-connect-timeout*
               :read-timeout *http-read-timeout*
               :content (shasht:write-json body nil)))))

(defun %epistemic-critic-call (messages)
  (let ((attributes (obj "purpose" "epistemic-critic"
                         "message_count" (length messages)
                         "model" *epistemic-critic-model*)))
    (labels ((invoke ()
               (let* ((response
                        (if *epistemic-critic-model-fn*
                            (funcall *epistemic-critic-model-fn*
                                     messages *epistemic-critic-model* 0
                                     *epistemic-critic-max-tokens*)
                            (%epistemic-critic-http-call messages)))
                      (usage (and (hash-table-p response)
                                  (gethash "usage" response))))
                 (when (hash-table-p usage)
                   (dolist (entry '(("prompt_tokens" "prompt_tokens")
                                    ("completion_tokens" "completion_tokens")
                                    ("total_tokens" "total_tokens")
                                    ("cost" "cost_usd")))
                     (let ((value (gethash (first entry) usage)))
                       (when (numberp value)
                         (setf (gethash (second entry) attributes) value)))))
                 response)))
      (if (fboundp 'call-with-timing-span)
          (funcall 'call-with-timing-span
                   "model.epistemic_critic_request" #'invoke
                   :attributes attributes)
          (invoke)))))

(defun %epistemic-critic-content (response)
  (cond ((stringp response) response)
        ((hash-table-p response)
         (or (ignore-errors (ref response "choices" 0 "message" "content"))
             (gethash "content" response)))
        (t nil)))

(defun %epistemic-critic-usage (response)
  (let ((usage (and (hash-table-p response) (gethash "usage" response))))
    (obj "prompt_tokens" (or (and usage (gethash "prompt_tokens" usage)) :null)
         "completion_tokens" (or (and usage (gethash "completion_tokens" usage)) :null)
         "total_tokens" (or (and usage (gethash "total_tokens" usage)) :null)
         "cost_usd" (or (and usage (gethash "cost" usage)) :null))))

(defun %epistemic-critic-fail-closed (status candidates &optional error-code)
  (obj "status" status "model" *epistemic-critic-model*
       "called" nil "error_code" (or error-code :null)
       "decisions"
       (coerce (mapcar (lambda (candidate)
                         (obj "fragment_id" (gethash "fragment_id" candidate)
                              "allow" nil "reason_code" "fail-closed"
                              "cited_fact_ids" #()))
                       candidates)
               'vector)))

(defun %epistemic-critic-validate (parsed candidates facts)
  (let* ((results (and (hash-table-p parsed) (gethash "results" parsed)))
         (rows (%epistemic-critic-list results))
         (candidate-ids (mapcar (lambda (x) (gethash "fragment_id" x)) candidates))
         (fact-ids (mapcar (lambda (x) (gethash "id" x)) facts))
         (seen nil) (valid t) (decisions nil))
    (unless (= (length rows) (length candidates)) (setf valid nil))
    (dolist (row rows)
      (let* ((id (and (hash-table-p row) (gethash "fragment_id" row)))
             (reason (and (hash-table-p row) (gethash "reason_code" row)))
             (cited (%epistemic-critic-list
                     (and (hash-table-p row) (gethash "cited_fact_ids" row)))))
        (multiple-value-bind (allow found-p)
            (if (hash-table-p row) (gethash "allow" row) (values nil nil))
          (unless (and found-p (or (eq allow t) (null allow))
                       (stringp id) (member id candidate-ids :test #'string=)
                       (not (member id seen :test #'string=))
                       (member reason *epistemic-critic-reason-codes* :test #'string=)
                       (every (lambda (fact-id)
                                (and (stringp fact-id)
                                     (member fact-id fact-ids :test #'string=)))
                              cited))
            (setf valid nil))
          (when (stringp id) (push id seen))
          (push (obj "fragment_id" (or id "invalid")
                     "allow" (if allow t nil)
                     "reason_code" (or reason "invalid")
                     "cited_fact_ids" (coerce cited 'vector))
                decisions))))
    (and valid
         (every (lambda (id) (member id seen :test #'string=)) candidate-ids)
         (coerce (nreverse decisions) 'vector))))

(defun epistemic-critic-review (draft contract)
  "Return a content-bearing decision to the caller; durable events remain content-free."
  (let ((candidates (%epistemic-critic-candidates draft contract)))
    (cond ((eq (%epistemic-critic-mode) :off)
           (%epistemic-critic-fail-closed "off" candidates))
          ((not (%epistemic-critic-review-required-p draft contract))
           (obj "status" "not-routed" "model" *epistemic-critic-model*
                "called" nil "error_code" :null "decisions" #()))
          ((null candidates)
           (obj "status" "no-candidates" "model" *epistemic-critic-model*
                "called" nil "error_code" :null "decisions" #()))
          (t
           (bt:with-lock-held (*epistemic-critic-lock*)
             (let ((budget (%epistemic-critic-budget)))
               (if (not (%epistemic-critic-budget-available-p budget))
                   (%epistemic-critic-fail-closed "budget-blocked" candidates
                                                  "rolling-budget-exhausted")
                   (let* ((facts (%epistemic-critic-facts contract))
                          (payload (obj "intent"
                                        (gethash "intent" contract "conversation")
                                        "relational_obligations"
                                        (gethash "relational_obligations"
                                                 contract #())
                                        "facts" (coerce facts 'vector)
                                        "fragments" (coerce candidates 'vector)))
                          (messages
                            (list
                             (obj "role" "system" "content"
                                  "You are a strict removal-only public-statement gate. Decide whether every proposed fragment may be retained under the supplied intent, obligations, and facts. Allow greetings, questions, warmth, and explicitly subjective present-tense mood or perspective as relational-only. Reject claims of historical/background activity, maintenance, memory, process state, thought, or experience unless supplied facts entail them. Preserve the bounded/configured scope: zero configured events does not support an unqualified claim that nothing was logged, recorded, or happened. You cannot add or rewrite prose. Return exactly one JSON object: {\"results\":[{\"fragment_id\":\"...\",\"allow\":true|false,\"cited_fact_ids\":[\"...\"],\"reason_code\":\"entailed|relational-only|overclaim-no-evidence|contradicts-facts|unsupported-specific\"}]}. Include every fragment exactly once and no other keys.")
                             (obj "role" "user" "content"
                                  (shasht:write-json payload nil))))
                          (reservation-recorded-p
                            (ignore-errors
                              (funcall *epistemic-critic-event-fn*
                                       "epistemic-critic-call"
                                       (obj "model" *epistemic-critic-model*
                                            "mode" (string-downcase
                                                     (symbol-name
                                                      (%epistemic-critic-mode)))
                                            "fragment_count" (length candidates)
                                            "reserved_cost_usd"
                                            *epistemic-critic-reserved-cost-usd*)))))
                     (if (not reservation-recorded-p)
                         (%epistemic-critic-fail-closed
                          "budget-blocked" candidates "budget-ledger-unavailable")
                         (handler-case
                         (let* ((response (%epistemic-critic-call messages))
                                (usage (%epistemic-critic-usage response))
                                (actual (%epistemic-critic-number
                                         (gethash "cost_usd" usage)))
                                (parsed
                                  (handler-case
                                      (shasht:read-json
                                       (%epistemic-critic-content response))
                                    (error () nil)))
                                (decisions (%epistemic-critic-validate
                                            parsed candidates facts))
                                (status (if decisions "ok" "schema-rejected")))
                           (ignore-errors
                             (funcall *epistemic-critic-event-fn*
                                      "epistemic-critic-result"
                                      (obj "model" *epistemic-critic-model*
                                           "status" status
                                           "fragment_count" (length candidates)
                                           "allowed_count"
                                           (if decisions
                                               (count-if (lambda (x)
                                                           (gethash "allow" x))
                                                         (%epistemic-critic-list decisions))
                                               0)
                                           "actual_cost_usd" actual
                                           "overage_cost_usd"
                                           (max 0.0d0 (- actual
                                                         *epistemic-critic-reserved-cost-usd*))
                                           "prompt_tokens" (gethash "prompt_tokens" usage)
                                           "completion_tokens" (gethash "completion_tokens" usage))))
                           (if decisions
                               (obj "status" "ok" "model" *epistemic-critic-model*
                                    "called" t "error_code" :null
                                    "usage" usage "decisions" decisions)
                               (%epistemic-critic-fail-closed
                                "schema-rejected" candidates "invalid-result-schema")))
                       (error ()
                         (ignore-errors
                           (funcall *epistemic-critic-event-fn*
                                    "epistemic-critic-result"
                                    (obj "model" *epistemic-critic-model*
                                         "status" "model-error"
                                         "fragment_count" (length candidates)
                                         "allowed_count" 0
                                         "actual_cost_usd" 0.0d0
                                         "overage_cost_usd" 0.0d0)))
                         (%epistemic-critic-fail-closed
                          "model-error" candidates "model-call-failed"))))))))))))

(defun epistemic-critic-observe (draft contract)
  "Run the shadow probe when enabled. Never returns replacement speech."
  (when (eq (%epistemic-critic-mode) :shadow)
    (epistemic-critic-review draft contract)))

(defun %epistemic-critic-decision-for (fragment-id decisions)
  (find fragment-id (%epistemic-critic-list decisions)
        :key (lambda (row)
               (and (hash-table-p row) (gethash "fragment_id" row)))
        :test #'string=))

(defun %epistemic-critic-join (parts)
  (format nil "~{~a~^ ~}" (remove-if-not #'stringp parts)))

(defun epistemic-critic-realize (draft contract)
  "Apply critic removals only when the bounded specialist route is required."
  (let ((intent (and (hash-table-p contract)
                     (gethash "intent" contract "conversation"))))
    (if (or (not (eq (%epistemic-critic-mode) :enforced))
            (not (member intent '("temporal-report" "check-in")
                         :test #'string=))
            (not (%epistemic-critic-review-required-p draft contract)))
        (funcall 'realize-publication-draft draft contract)
        (let* ((candidates (%epistemic-critic-candidates draft contract))
               (review (epistemic-critic-review draft contract))
               (decisions (gethash "decisions" review))
               (accepted
                 (loop for candidate in candidates
                       for id = (gethash "fragment_id" candidate)
                       for decision = (%epistemic-critic-decision-for
                                      id decisions)
                       when (and decision (gethash "allow" decision))
                         collect (gethash "proposed_fragment" candidate)))
               (fallback (funcall 'realize-publication-draft "" contract))
               (bridge
                 (if (fboundp 'publication-contract-epistemic-bridge)
                     (funcall 'publication-contract-epistemic-bridge contract)
                     ""))
               (candidate
                 (if (string= intent "temporal-report")
                     (%epistemic-critic-join
                      (append
                       (list (funcall 'publication-contract-factual-nucleus
                                      contract))
                       (when (plusp (length bridge)) (list bridge))
                       accepted))
                     (if accepted (%epistemic-critic-join accepted) fallback)))
               (violations
                 (funcall 'publication-contract-violations candidate contract))
               (final (if (zerop (length violations)) candidate fallback)))
          (ignore-errors
            (funcall *epistemic-critic-event-fn*
                     "epistemic-critic-application"
                     (obj "mode" "enforced" "intent" intent
                          "status" (gethash "status" review "unknown")
                          "fragment_count" (length candidates)
                          "allowed_count" (length accepted)
                          "final_violation_count" (length violations)
                          "fallback_used" (if (string= final fallback) t nil))))
          final))))

(defun epistemic-critic-report ()
  (let ((budget (%epistemic-critic-budget)))
    (obj "schema_version" 1 "mode" (string-downcase
                                      (symbol-name (%epistemic-critic-mode)))
         "model" *epistemic-critic-model*
         "max_fragments_per_turn" *epistemic-critic-max-fragments*
         "max_calls_per_turn" 1
         "max_tokens_per_call" *epistemic-critic-max-tokens*
         "reserved_cost_usd_per_call" *epistemic-critic-reserved-cost-usd*
         "budget" budget
         "public_output_mutation"
         (if (eq (%epistemic-critic-mode) :enforced) "removal-only" nil))))
