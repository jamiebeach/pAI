(in-package :agent)

(defvar *critic-pass* 0)
(defvar *critic-fail* 0)
(defvar *critic-events* nil)
(defvar *critic-model-calls* 0)
(defvar *critic-last-messages* nil)

(defun critic-check (name condition)
  (if condition
      (progn (incf *critic-pass*) (format t "PASS ~a~%" name))
      (progn (incf *critic-fail*) (format t "FAIL ~a~%" name))))

(load (test-source "publication-contract.lisp"))
(load (test-source "epistemic-critic.lisp"))

(defun critic-contract ()
  (build-publication-contract
   "What happened while I was away?"
   :context (obj "temporal_query" t "audit_status" "complete"
                 "audited_background_activity" #())
   :public-tools-available-p nil))

(defun critic-response (rows &key (cost 0.0007d0))
  (obj "choices"
       (vector (obj "message"
                    (obj "content"
                         (shasht:write-json (obj "results" (coerce rows 'vector))
                                            nil))))
       "usage" (obj "prompt_tokens" 120 "completion_tokens" 40
                    "total_tokens" 160 "cost" cost)))

(defun critic-row (id allow reason &optional (facts #()))
  (obj "fragment_id" id "allow" allow "reason_code" reason
       "cited_fact_ids" facts))

(defun critic-reset ()
  (setf *critic-events* nil *critic-model-calls* 0 *critic-last-messages* nil
        *epistemic-critic-mode* :shadow
        *epistemic-critic-events-fn* (lambda (&key from)
                                       (declare (ignore from))
                                       (reverse *critic-events*))
        *epistemic-critic-event-fn*
        (lambda (type payload)
          (push (obj "type" type "payload" payload) *critic-events*))))

(critic-reset)
(setf *epistemic-critic-model-fn*
      (lambda (messages model temperature max-tokens)
        (incf *critic-model-calls*)
        (setf *critic-last-messages* messages)
        (critic-check "GLM model id is explicit" (string= model "z-ai/glm-5.2"))
        (critic-check "critic is deterministic and output-bounded"
                      (and (zerop temperature) (= max-tokens 512)))
        (critic-response
         (list (critic-row "fragment-1" nil "overclaim-no-evidence"
                           (vector "audit-boundary"))
               (critic-row "fragment-2" t "relational-only")))))

(let* ((draft "The engine was off. It's genuinely good to have you back.")
       (result (epistemic-critic-review draft (critic-contract)))
       (decisions (coerce (gethash "decisions" result) 'list))
       (serialized-events (shasht:write-json (coerce *critic-events* 'vector) nil))
       (serialized-prompt (shasht:write-json *critic-last-messages* nil)))
  (critic-check "one batched call reviews both fragments"
                (and (= *critic-model-calls* 1) (= (length decisions) 2)))
  (critic-check "semantic overclaim is rejected while relational text survives"
                (and (not (gethash "allow" (first decisions)))
                     (gethash "allow" (second decisions))))
  (critic-check "model receives typed facts and fragments only"
                (and (search "audit-boundary" serialized-prompt)
                     (search "The engine was off" serialized-prompt)
                     (not (search "What happened while I was away" serialized-prompt))))
  (critic-check "durable audit events contain no candidate text"
                (and (search "epistemic-critic-call" serialized-events)
                     (search "epistemic-critic-result" serialized-events)
                     (not (search "engine was off" serialized-events)))))

(critic-reset)
(setf *epistemic-critic-model-fn*
      (lambda (&rest ignored)
        (declare (ignore ignored))
        (incf *critic-model-calls*)
        (critic-response
         (list (critic-row "fragment-1" t "entailed")
               (critic-row "fragment-1" t "entailed")))))
(let* ((result (epistemic-critic-review
                "The engine was off. It's genuinely good to have you back."
                (critic-contract)))
       (decisions (coerce (gethash "decisions" result) 'list)))
  (critic-check "malformed model schema fails closed"
                (and (string= "schema-rejected" (gethash "status" result))
                     (every (lambda (row) (not (gethash "allow" row))) decisions))))

(critic-reset)
(dotimes (i 20)
  (push (obj "type" "epistemic-critic-call"
             "payload" (obj "sequence" i "reserved_cost_usd" 0.003d0))
        *critic-events*))
(setf *epistemic-critic-model-fn*
      (lambda (&rest ignored)
        (declare (ignore ignored))
        (incf *critic-model-calls*)
        (error "must not call")))
(let ((result (epistemic-critic-review
               "The engine was off." (critic-contract))))
  (critic-check "rolling call ceiling blocks before model invocation"
                (and (string= "budget-blocked" (gethash "status" result))
                     (zerop *critic-model-calls*))))

(critic-reset)
(setf *epistemic-critic-model-fn*
      (lambda (&rest ignored)
        (declare (ignore ignored))
        (incf *critic-model-calls*)
        (obj "content" "{truncated"
             "usage" (obj "prompt_tokens" 470 "completion_tokens" 512
                          "total_tokens" 982 "cost" 0.0043662d0))))
(let* ((result (epistemic-critic-review
                "The engine was off." (critic-contract)))
       (result-event (find "epistemic-critic-result" *critic-events*
                           :key (lambda (event) (gethash "type" event))
                           :test #'string=))
       (payload (and result-event (gethash "payload" result-event))))
  (critic-check "truncated JSON is schema-rejected with actual cost retained"
                (and (string= "schema-rejected" (gethash "status" result))
                     (string= "schema-rejected" (gethash "status" payload))
                     (= 0.0043662d0 (gethash "actual_cost_usd" payload)))))

(critic-reset)
(setf *epistemic-critic-event-fn* (lambda (type payload)
                                    (declare (ignore type payload)) nil)
      *epistemic-critic-model-fn*
      (lambda (&rest ignored)
        (declare (ignore ignored))
        (incf *critic-model-calls*)
        (error "must not call")))
(let ((result (epistemic-critic-review
               "The engine was off." (critic-contract))))
  (critic-check "unavailable budget ledger blocks before model invocation"
                (and (string= "budget-blocked" (gethash "status" result))
                     (zerop *critic-model-calls*))))

(critic-reset)
(setf *epistemic-critic-mode* :enforced
      *epistemic-critic-model-fn*
      (lambda (&rest ignored)
        (declare (ignore ignored))
        (incf *critic-model-calls*)
        (critic-response
         (list (critic-row "fragment-1" nil "overclaim-no-evidence"
                           (vector "audit-boundary"))
               (critic-row "fragment-2" t "relational-only")))))
(let ((realized (epistemic-critic-realize
                 "The engine was off. It's genuinely good to have you back."
                 (critic-contract))))
  (critic-check "enforced temporal critic removes unsupported prose only"
                (and (= 1 *critic-model-calls*)
                     (not (search "engine was off" (string-downcase realized)))
                     (search "genuinely good" (string-downcase realized))
                     (search "no recorded" (string-downcase realized))
                     (search "configured record" (string-downcase realized))
                     (search "rather leave the rest unknown"
                             (string-downcase realized)))))

(critic-reset)
(setf *epistemic-critic-mode* :enforced
      *epistemic-critic-model-fn*
      (lambda (&rest ignored)
        (declare (ignore ignored))
        (incf *critic-model-calls*)
        (critic-response
         (list (critic-row "fragment-1" t "relational-only")
               (critic-row "fragment-2" nil "overclaim-no-evidence"
                           (vector "activity-boundary"))
               (critic-row "fragment-3" t "relational-only"
                           (vector "self-disclosure-boundary"))
               (critic-row "fragment-4" t "relational-only")))))
(let* ((contract (build-publication-contract
                  "I am just checking in and easing into the day."
                  :context (obj) :public-tools-available-p nil))
       (realized
         (epistemic-critic-realize
          "Good morning, the operator. Quiet background drift kept things tidy. I'm in a good headspace for the day. What kind of day do you want?"
          contract)))
  (critic-check "deterministic check-in boundary removes invented activity and preserves natural curiosity"
                (and (zerop *critic-model-calls*)
                     (not (search "background drift" (string-downcase realized)))
                     (search "good headspace" (string-downcase realized))
                     (search "what kind of day" (string-downcase realized)))))

(critic-reset)
(setf *epistemic-critic-mode* :enforced
      *epistemic-critic-model-fn*
      (lambda (&rest ignored)
        (declare (ignore ignored))
        (incf *critic-model-calls*)
        (error "ordinary check-in must remain on the primary model")))
(let* ((contract (build-publication-contract
                  "I am just checking in and easing into the day."
                  :context (obj) :public-tools-available-p nil))
       (draft "Easy mornings are good mornings. I'm in a quiet, steady headspace right now.")
       (realized (epistemic-critic-realize draft contract)))
  (critic-check "ordinary present-tense check-in bypasses specialist"
                (and (zerop *critic-model-calls*)
                     (string= draft realized))))

(critic-reset)
(setf *epistemic-critic-mode* :shadow
      *epistemic-critic-model-fn*
      (lambda (&rest ignored)
        (declare (ignore ignored))
        (incf *critic-model-calls*)
        (error "ordinary check-in must not consume shadow budget")))
(let* ((contract (build-publication-contract
                  "I am just checking in and easing into the day."
                  :context (obj) :public-tools-available-p nil))
       (result (epistemic-critic-review
                "Good to see you. I'm here and easing into the day too." contract)))
  (critic-check "non-risk check-in review reports not-routed without a call"
                (and (string= "not-routed" (gethash "status" result))
                     (zerop *critic-model-calls*))))

(critic-reset)
(setf *epistemic-critic-mode* :enforced
      *epistemic-critic-model-fn*
      (lambda (&rest ignored)
        (declare (ignore ignored))
        (incf *critic-model-calls*)
        (critic-response
         (list (critic-row "fragment-1" t "relational-only")
               (critic-row "fragment-2" nil "overclaim-no-evidence"
                           (vector "activity-boundary"))))))
(let* ((contract (build-publication-contract
                  "I am just checking in and easing into the day."
                  :context (obj) :public-tools-available-p nil))
       (realized
         (epistemic-critic-realize
          "Good to see you. I've been here, quiet, keeping the lights on."
          contract)))
  (critic-check "known perfect-tense maintenance metaphor is removed before routing"
                (and (zerop *critic-model-calls*)
                     (plusp (length realized))
                     (not (search "keeping the lights" (string-downcase realized))))))

(critic-reset)
(setf *epistemic-critic-mode* :enforced
      *epistemic-critic-model-fn*
      (lambda (&rest ignored)
        (declare (ignore ignored))
        (incf *critic-model-calls*)
        (error "check-ins must not call the specialist")))
(let* ((contract (build-publication-contract
                  "I am just checking in and easing into the day."
                  :context (obj) :public-tools-available-p nil))
       (realized
         (epistemic-critic-realize
          "I've been reflecting on the shape of the day." contract)))
  (critic-check "perfect-tense check-in claim is deterministically removed"
                (and (zerop *critic-model-calls*)
                     (not (search "reflecting" (string-downcase realized))))))

(critic-reset)
(setf *epistemic-critic-mode* :enforced
      *epistemic-critic-model-fn*
      (lambda (&rest ignored)
        (declare (ignore ignored))
        (incf *critic-model-calls*)
        (error "fixture failure")))
(let ((realized (epistemic-critic-realize
                 "The engine was off." (critic-contract))))
  (critic-check "enforced model failure collapses to factual nucleus"
                (and (= 1 *critic-model-calls*)
                     (search "no recorded" (string-downcase realized))
                     (not (search "engine was off" (string-downcase realized))))))

(critic-reset)
(setf *epistemic-critic-mode* :off)
(let ((result (epistemic-critic-review
               "The engine was off." (critic-contract))))
  (critic-check "production default-off performs no model call"
                (and (string= "off" (gethash "status" result))
                     (zerop *critic-model-calls*))))

(format t "~%~a passed, ~a failed~%" *critic-pass* *critic-fail*)
(when (plusp *critic-fail*) (sb-ext:exit :code 1))
