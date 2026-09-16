;;;; temporal-response-policy.lisp -- bounded pre-broadcast quality policy.
;;;;
;;;; Routine temporal answers have an authoritative context projection and no
;;;; tool schema. This layer validates the final model response before the
;;;; existing CALL-MODEL presentation wrapper can broadcast it. Shadow mode is
;;;; content-free observation only. Enforced mode permits at most one hidden
;;;; naturalization call. Deterministic code supplies constraints and validates
;;;; speech, but never authors the public reply.

(in-package :agent)

(defvar *temporal-response-policy-mode* :legacy)
(defvar *temporal-response-policy-context* nil)
(defvar *temporal-response-policy-correction-p* nil)
(defvar *temporal-response-policy-public-call-p* nil)
(defvar *temporal-response-policy-bypass-p* nil)
(defvar *publication-contract-current* nil)
(defvar *temporal-response-policy-installed-raw-wrapper* nil)
(defvar *timing-model-purpose* nil)

(defparameter *temporal-response-policy-event-fn*
  (lambda (type payload)
    (when (fboundp 'log-event) (funcall 'log-event type payload))))

(defparameter *temporal-response-policy-tool-promises*
  '("i'll check" "i will check" "let me check" "i can check"
    "i'll inspect" "i will inspect" "let me inspect"
    "i'll look through" "i will look through" "i'll search"
    "i will search" "let me search" "check the logs later"))

(defparameter *temporal-response-policy-forced-followups*
  '("what would you like me to" "do you want me to" "would you like me to"
    "shall i" "want me to" "should i check" "should i look"))

(defparameter *temporal-response-policy-positive-activity-claims*
  '("i spent time" "i worked on" "i explored" "i reflected on"
    "i was thinking about" "i developed" "i researched" "i investigated"))

(defparameter *temporal-response-policy-unsupported-absence-inferences*
  '("that stillness" "the stillness" "quiet on my end" "it was quiet"
    "everything was quiet" "nothing was going on" "nothing happened"
    "i was idle" "i've been idle" "i have been idle" "dormant" "inactive"
    "i didn't do anything" "i did not do anything"
    "i don't do anything" "i do not do anything"
    "i don't have activities" "i do not have activities"
    "i don't have experiences" "i do not have experiences"))

(defparameter *temporal-response-policy-correction-markers*
  '("you're right" "you are right" "sorry" "i should have"
    "i didn't answer" "i did not answer" "correction"))

(defparameter *temporal-response-policy-naturalization-system-prompt*
  "Write the agent's final conversational reply. Use the hidden contract only as factual and behavioral constraints. Return only natural speech to the operator. Private cognition and contextual-memory rows are evidence, not already-authored public prose: never emit labels such as unbidden thought, latent thought, private thought, internal thought, or raw thought, and never quote a raw private row verbatim. If a private perspective is genuinely relevant, integrate its meaning into ordinary first-person conversational speech. Correction obligations are mandatory: when correction_depth is 1, explicitly acknowledge the correction with natural language such as You're right or I had that wrong; when correction_depth is greater than 1, give the plain direct answer without another apology. When interaction_mode is meta-initiative-discussion, discuss the requested future reciprocity inside the current ordinary reply; do not imitate, demonstrate, or role-play an unsolicited check-in. When audit_status is complete and event_count is 0, say only that no configured activity was recorded or that no evidence is available. Never turn an empty record into a claim that the agent did nothing, was inactive, or lacks activities or experiences between conversations. When audit_status is complete and event_count is greater than 0, explicitly attribute every reported event to the record, for example by saying the record shows it; never present recorded events as directly observed experience. Do not mention a contract, validator, policy, audit machinery, context window, rewriting, or this instruction. Do not invent facts. Do not ask a forced follow-up question. Do not end with service-desk closure language such as let me know, I'm here if, anything else I can help with, or happy to help.")

(defun %temporal-response-policy-mode ()
  (if (member *temporal-response-policy-mode* '(:legacy :shadow :enforced))
      *temporal-response-policy-mode*
      :legacy))

(defun %temporal-response-policy-list (value)
  (cond ((null value) nil) ((vectorp value) (coerce value 'list))
        ((listp value) value) (t (list value))))

(defun %temporal-response-policy-contains-any (text phrases)
  (let ((lower (string-downcase (if (stringp text) text ""))))
    (some (lambda (phrase) (search phrase lower)) phrases)))

(defun %temporal-response-policy-question-only-p (text)
  (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return)
                               (if (stringp text) text "")))
         (has-question (position #\? trimmed))
         (has-statement (or (position #\. trimmed) (position #\! trimmed)
                            (position #\: trimmed))))
    (and has-question (not has-statement))))

(defun %temporal-response-policy-audit-events (context)
  (%temporal-response-policy-list
   (and (hash-table-p context)
        (gethash "audited_background_activity" context))))

(defun %temporal-response-policy-absence-answer-p (text)
  (%temporal-response-policy-contains-any
   text '("no configured" "no recorded" "nothing recorded" "none recorded"
          "don't have any record" "do not have any record" "no record of"
          "didn't record" "did not record" "don't have evidence"
          "do not have evidence" "no evidence" "nothing specific")))

(defun %temporal-response-policy-unavailable-answer-p (text)
  (%temporal-response-policy-contains-any
   text '("unavailable" "can't verify" "cannot verify" "couldn't verify"
          "could not verify" "don't know" "do not know" "not able to verify")))

(defun temporal-response-policy-violations (text context correction-p)
  "Return stable, content-free violation codes for a proposed final answer."
  (let* ((content (if (stringp text) text ""))
         (trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) content))
         (status (and (hash-table-p context)
                      (gethash "audit_status" context "unavailable")))
         (events (%temporal-response-policy-audit-events context))
         (violations nil))
    (when (< (length trimmed) 24) (push "missing-substantive-answer" violations))
    (when (%temporal-response-policy-question-only-p trimmed)
      (push "question-only" violations))
    (when (%temporal-response-policy-contains-any
           trimmed *temporal-response-policy-forced-followups*)
      (push "forced-followup" violations))
    (when (%temporal-response-policy-contains-any
           trimmed *temporal-response-policy-tool-promises*)
      (push "unavailable-tool-promise" violations))
    (when (search "```" trimmed) (push "code-block" violations))
    (when (and correction-p
               (not (%temporal-response-policy-contains-any
                     trimmed *temporal-response-policy-correction-markers*)))
      (push "missing-correction-acknowledgement" violations))
    (cond
      ((and (string= status "complete") (null events))
       (unless (%temporal-response-policy-absence-answer-p trimmed)
         (push "missing-empty-audit-answer" violations))
       (when (%temporal-response-policy-contains-any
              trimmed *temporal-response-policy-positive-activity-claims*)
         (push "unsupported-activity-claim" violations))
       (when (%temporal-response-policy-contains-any
              trimmed
              *temporal-response-policy-unsupported-absence-inferences*)
         (push "unsupported-absence-inference" violations)))
      ((string= status "unavailable")
       (unless (%temporal-response-policy-unavailable-answer-p trimmed)
         (push "missing-audit-limitation" violations))))
    (nreverse (remove-duplicates violations :test #'string=))))

(defun %temporal-response-policy-active-violations (text)
  (if (and (hash-table-p *publication-contract-current*)
           (fboundp 'publication-contract-violations))
      (coerce
       (funcall 'publication-contract-violations
                text *publication-contract-current*)
       'list)
      (temporal-response-policy-violations
       text *temporal-response-policy-context*
       *temporal-response-policy-correction-p*)))

;; Compatibility renderer for the uninstalled legacy implementation only.
(defun %temporal-response-policy-realize (draft)
  (funcall 'realize-publication-draft
           draft *publication-contract-current*))

(defun %temporal-response-policy-observe-critic (response)
  "Observe only the final public model response. The critic has no mutation API."
  (when (and (not *temporal-response-policy-bypass-p*)
             *temporal-response-policy-public-call-p*
             (hash-table-p *publication-contract-current*)
             (fboundp 'epistemic-critic-observe))
    (let* ((message (and (hash-table-p response)
                         (ignore-errors (ref response "choices" 0 "message"))))
           (tool-calls (and (hash-table-p message) (gethash "tool_calls" message)))
           (content (and (hash-table-p message)
                         (not (and (present-p tool-calls)
                                   (plusp (length tool-calls))))
                         (gethash "content" message))))
      (when (stringp content)
        (ignore-errors
          (funcall 'epistemic-critic-observe
                   content *publication-contract-current*))))))

(defun %temporal-response-policy-user-prompt (context)
  (let ((exchange (%temporal-response-policy-list
                   (gethash "current_exchange" context))))
    (if (and exchange (hash-table-p (first exchange)))
        (gethash "content" (first exchange) "")
        "")))

;; Retained only for the inert legacy implementation below, which is not
;; installed as a wrapper. Enforced publication never invokes these renderers.
(defun %temporal-response-policy-audit-facts (context)
  (let* ((status (gethash "audit_status" context "unavailable"))
         (events (%temporal-response-policy-audit-events context)))
    (cond
      ((string= status "unavailable")
       "The bounded audit source was unavailable; do not infer that no activity occurred.")
      ((null events)
       "The bounded audit completed and contains zero configured background-activity events after the relevant user boundary.")
      (t
       (format nil "The bounded audit completed with ~d configured event(s): ~{~a~^, ~}."
               (length events)
               (mapcar (lambda (event)
                         (if (hash-table-p event)
                             (gethash "type" event "unknown-event")
                             "unknown-event"))
                       events))))))

(defun %temporal-response-policy-fallback (context correction-p)
  (let* ((status (gethash "audit_status" context "unavailable"))
         (events (%temporal-response-policy-audit-events context))
         (prefix (if correction-p
                     "You're right—I didn't answer you directly. "
                     "Here is the direct answer: ")))
    (cond
      ((string= status "unavailable")
       (concatenate 'string prefix
                    "the bounded audit source was unavailable, so I can't verify what occurred during that interval. I won't invent an answer."))
      ((null events)
       (concatenate 'string prefix
                    "I don't have any configured background-activity events recorded in the bounded audit window after your last message. The honest answer is that I don't have evidence I did anything specific during that time."))
      (t
       (format nil "~aI have ~d configured background-activity event~:p recorded in the bounded audit: ~{~a~^, ~}. That is the activity I can support from the record."
               prefix (length events)
               (mapcar (lambda (event)
                         (gethash "type" event "unknown-event"))
                       events))))))

(defun %temporal-response-policy-repair (original context correction-p)
  (let* ((status (gethash "audit_status" context "unavailable"))
         (events (%temporal-response-policy-audit-events context))
         (required-audit-language
           (cond ((string= status "unavailable")
                  "Include the exact phrase: the audit source was unavailable")
                 ((null events)
                  "Include the exact phrase: no recorded background-activity events")
                 (t
                  "Explicitly state that the bounded audit contains recorded events")))
         (required-correction-language
           (if correction-p
               "Begin with the exact words: You're right"
               "No correction acknowledgement is required"))
         (messages
           (list
            (obj "role" "system" "content"
                 "Rewrite a companion's answer using only the supplied audit facts. Return only the final conversational reply. Answer directly and warmly. Obey the two exact-language requirements in the user message; they are validation anchors, not optional style suggestions. Do not claim unavailable tools or searches, invent activity, use a code block, end with a forced question, or promise later investigation.")
            (obj "role" "user" "content"
                 (format nil "USER QUESTION:~%~a~%~%AUDIT FACTS:~%~a~%~%REQUIRED AUDIT LANGUAGE: ~a.~%REQUIRED CORRECTION LANGUAGE: ~a.~%~%DRAFT TO REPAIR:~%~a"
                         (%temporal-response-policy-user-prompt context)
                         (%temporal-response-policy-audit-facts context)
                         required-audit-language required-correction-language
                         original))))
         (*temporal-response-policy-bypass-p* t)
         (*timing-model-purpose* "response-repair")
         (response (funcall 'raw-call-model messages))
         (message (and (hash-table-p response)
                       (ignore-errors (ref response "choices" 0 "message"))))
         (tool-calls (and (hash-table-p message)
                          (gethash "tool_calls" message)))
         (content (and (hash-table-p message)
                       (not (and (present-p tool-calls)
                                 (plusp (length tool-calls))))
                       (gethash "content" message))))
    (values response content)))

(defun %temporal-response-policy-naturalize (original)
  "Ask the model once for natural public speech constrained by the contract."
  (let* ((contract-json
           (if (hash-table-p *publication-contract-current*)
               (shasht:write-json *publication-contract-current* nil)
               "{}"))
         (messages
           (list
            (obj "role" "system" "content"
                 *temporal-response-policy-naturalization-system-prompt*)
            (obj "role" "user" "content"
                 (format nil
                         "OPERATOR'S MESSAGE:~%~a~%~%HIDDEN CONTRACT:~%~a~%~%REJECTED DRAFT:~%~a"
                         (%temporal-response-policy-user-prompt
                          *temporal-response-policy-context*)
                         contract-json
                         (if (stringp original) original "")))))
         (*temporal-response-policy-bypass-p* t)
         (*timing-model-purpose* "publication-naturalization")
         (response
           ;; This is private speech rewriting, never an agentic turn.  An
           ;; inherited public tool schema allowed the observed naturalizer to
           ;; request GENERATE-IMAGE after that tool had already succeeded,
           ;; which then failed validation and erased the completed turn from
           ;; conversational history.  Keep the existing wrapped provider path
           ;; for tracing/capture, but make its dynamically read schema empty.
           (let ((*tools* #()))
             (progv '(*call-model-reasoning-override*) '(:disabled)
               (funcall 'raw-call-model messages))))
         (message (and (hash-table-p response)
                       (ignore-errors (ref response "choices" 0 "message"))))
         (tool-calls (and (hash-table-p message)
                          (gethash "tool_calls" message)))
         (content (and (hash-table-p message)
                       (not (and (present-p tool-calls)
                                 (plusp (length tool-calls))))
                       (gethash "content" message))))
    (values response content)))

(defun %temporal-response-policy-current-turn-has-tool-result-p (messages)
  "True when the current user turn has already completed at least one tool.
Only the suffix after the most recent user message is relevant; an older tool
result must not weaken fail-closed publication on a later ordinary turn."
  (loop for message in (reverse messages)
        for role = (and (hash-table-p message)
                        (gethash "role" message ""))
        when (string= role "user") do (return nil)
        when (string= role "tool") do (return t)
        finally (return nil)))

(defun %temporal-response-policy-unavailable (reason)
  (if (find-class 'public-response-unavailable nil)
      (error 'public-response-unavailable :reason reason)
      (error "Public response unavailable: ~a" reason)))

(defun %temporal-response-policy-emit (mode action violations
                                       &key repair-attempted repair-valid
                                            repair-violations
                                            realization-applied
                                            realization-valid
                                            realization-violations)
  (ignore-errors
    (funcall *temporal-response-policy-event-fn*
             (if (eq mode :shadow)
                 "temporal-response-policy-shadow"
                 "temporal-response-policy-applied")
             (obj "mode" (string-downcase (symbol-name mode))
                  "turn_id" (if (fboundp '%context-projection-current-turn-id)
                                (funcall '%context-projection-current-turn-id)
                                :null)
                  "audit_status" (gethash "audit_status"
                                          *temporal-response-policy-context*
                                          "unavailable")
                  "audit_event_count"
                  (length (%temporal-response-policy-audit-events
                           *temporal-response-policy-context*))
                  "violation_codes" (coerce violations 'vector)
                  "violation_count" (length violations)
                  "repair_violation_codes"
                  (coerce (or repair-violations nil) 'vector)
                  "repair_violation_count" (length (or repair-violations nil))
                  "action" action
                  "repair_attempted" (if repair-attempted t nil)
                  "repair_valid" (if repair-valid t nil)
                  "contract_intent"
                  (if (hash-table-p *publication-contract-current*)
                      (gethash "intent" *publication-contract-current*
                               "conversation")
                      :null)
                  "realization_applied" (if realization-applied t nil)
                  "realization_valid" (if realization-valid t nil)
                  "realization_violation_codes"
                  (coerce (or realization-violations nil) 'vector)
                  "realization_violation_count"
                  (length (or realization-violations nil))))))

(defun %temporal-response-policy-legacy-raw-call-model (messages)
  "Historical implementation retained for rollback archaeology; never installed."
  (let* ((response (funcall 'pai-base-raw-call-model-temporal-policy messages))
         (mode (%temporal-response-policy-mode)))
    (%temporal-response-policy-observe-critic response)
    (if (or *temporal-response-policy-bypass-p*
            (not *temporal-response-policy-public-call-p*)
            (null *temporal-response-policy-context*)
            (eq mode :legacy))
        response
        (let* ((message (and (hash-table-p response)
                             (ignore-errors (ref response "choices" 0 "message"))))
               (tool-calls (and (hash-table-p message)
                                (gethash "tool_calls" message)))
               (final-p (not (and (present-p tool-calls)
                                  (plusp (length tool-calls)))))
               (content (and (hash-table-p message)
                             (gethash "content" message))))
          (if (not final-p)
              response
              (let ((violations
                      (%temporal-response-policy-active-violations content)))
                (cond
                  ((eq mode :shadow)
                   (%temporal-response-policy-emit
                    mode "observed" violations)
                   response)
                  ((null violations)
                   ;; A temporal contract still needs its code-owned factual
                   ;; nucleus even when the model draft happens to pass the
                   ;; old lexical validator. Non-temporal valid drafts remain
                   ;; byte-identical through REALIZE-PUBLICATION-DRAFT.
                   (if (and (hash-table-p *publication-contract-current*)
                            (fboundp 'realize-publication-draft))
                       (let* ((realized
                                (%temporal-response-policy-realize content))
                              (realization-violations
                                (%temporal-response-policy-active-violations
                                 realized)))
                         (when (hash-table-p message)
                           (setf (gethash "content" message) realized))
                         (%temporal-response-policy-emit
                          mode "realized" nil
                          :realization-applied t
                          :realization-valid (null realization-violations)
                          :realization-violations realization-violations)
                         response)
                       (progn
                         (%temporal-response-policy-emit mode "accepted" nil)
                         response)))
                  (t
                   (if (and (hash-table-p *publication-contract-current*)
                            (fboundp 'realize-publication-draft))
                       (let* ((realized
                                (%temporal-response-policy-realize content))
                              (realization-violations
                                (%temporal-response-policy-active-violations
                                 realized))
                              (final
                                (if (null realization-violations)
                                    realized
                                    (%temporal-response-policy-realize "")))
                              (final-violations
                                (%temporal-response-policy-active-violations
                                 final)))
                         (when (hash-table-p message)
                           (setf (gethash "content" message) final
                                 (gethash "tool_calls" message) :null))
                         (%temporal-response-policy-emit
                          mode
                          (if (null realization-violations)
                              "realized" "realization-fallback")
                          violations
                          :realization-applied t
                          :realization-valid (null final-violations)
                          :realization-violations final-violations)
                         response)
                       (multiple-value-bind (repair-response repair-content)
                           (%temporal-response-policy-repair
                            content *temporal-response-policy-context*
                            *temporal-response-policy-correction-p*)
                         (let ((repair-violations
                                 (temporal-response-policy-violations
                                  repair-content *temporal-response-policy-context*
                                  *temporal-response-policy-correction-p*)))
                           (if (null repair-violations)
                               (progn
                                 (%temporal-response-policy-emit
                                  mode "repaired" violations
                                  :repair-attempted t :repair-valid t)
                                 repair-response)
                               (progn
                                 (when (hash-table-p message)
                                   (setf (gethash "content" message)
                                         (%temporal-response-policy-fallback
                                          *temporal-response-policy-context*
                                          *temporal-response-policy-correction-p*)
                                         (gethash "tool_calls" message) :null))
                                 (%temporal-response-policy-emit
                                  mode "fallback" violations
                                  :repair-attempted t :repair-valid nil
                                  :repair-violations repair-violations)
                                 response)))))))))))))

(defun %temporal-response-policy-raw-call-model (messages)
  (let* ((response (funcall 'pai-base-raw-call-model-temporal-policy messages))
         (mode (%temporal-response-policy-mode)))
    (%temporal-response-policy-observe-critic response)
    (if (or *temporal-response-policy-bypass-p*
            (not *temporal-response-policy-public-call-p*)
            (null *temporal-response-policy-context*)
            (eq mode :legacy))
        response
        (let* ((message (and (hash-table-p response)
                             (ignore-errors (ref response "choices" 0 "message"))))
               (tool-calls (and (hash-table-p message)
                                (gethash "tool_calls" message)))
               (final-p (not (and (present-p tool-calls)
                                  (plusp (length tool-calls)))))
               (content (and (hash-table-p message)
                             (gethash "content" message))))
          (if (not final-p)
              response
              (let ((violations
                      (%temporal-response-policy-active-violations content)))
                (cond
                  ((eq mode :shadow)
                   (%temporal-response-policy-emit mode "observed" violations)
                   response)
                  ((null violations)
                   ;; A valid model draft is already the agent's natural voice.
                   ;; Preserve it byte-identically.
                   (%temporal-response-policy-emit mode "accepted" nil)
                   response)
                  (t
                    (multiple-value-bind
                        (naturalized-response naturalized-content)
                        (%temporal-response-policy-naturalize content)
                      (let ((naturalized-violations
                              (%temporal-response-policy-active-violations
                               naturalized-content)))
                        (cond
                          ((null naturalized-violations)
                           (%temporal-response-policy-emit
                            mode "naturalized" violations
                            :repair-attempted t :repair-valid t)
                            naturalized-response)
                          ((and (hash-table-p *publication-contract-current*)
                                (fboundp 'publication-contract-removal-only-draft)
                                (hash-table-p naturalized-response)
                                (let* ((naturalized-message
                                         (ignore-errors
                                           (ref naturalized-response "choices" 0 "message")))
                                       (naturalized-tool-calls
                                         (and (hash-table-p naturalized-message)
                                              (gethash "tool_calls" naturalized-message)))
                                       (filtered
                                         (and (not (and (present-p naturalized-tool-calls)
                                                        (plusp (length naturalized-tool-calls))))
                                              (funcall
                                               'publication-contract-removal-only-draft
                                               naturalized-content
                                               *publication-contract-current*))))
                                  (when filtered
                                    (setf (gethash "content" naturalized-message) filtered)
                                    t)))
                           (%temporal-response-policy-emit
                            mode "naturalized-removal-only" violations
                            :repair-attempted t :repair-valid t
                            :repair-violations naturalized-violations)
                           naturalized-response)
                          ((%temporal-response-policy-current-turn-has-tool-result-p
                            messages)
                           ;; Tool side effects are not transactional. If
                           ;; private polishing fails after a tool completed,
                           ;; return the original natural model response so
                           ;; AGENT-LOOP commits the whole tool transcript.
                           (%temporal-response-policy-emit
                            mode "post-tool-original-preserved" violations
                            :repair-attempted t :repair-valid nil
                            :repair-violations naturalized-violations)
                           response)
                          (t
                           (%temporal-response-policy-emit
                            mode "naturalization-failed" violations
                            :repair-attempted t :repair-valid nil
                            :repair-violations naturalized-violations)
                           (%temporal-response-policy-unavailable
                            "the single bounded naturalization failed publication validation")))))))))))))

(defun %temporal-response-policy-call-model (next messages)
  (let ((*temporal-response-policy-public-call-p* t))
    (funcall next messages)))

(defun %temporal-response-policy-unwrap-timing-base (timing-base installed
                                                     policy-base)
  "If timing was reloaded on top of this policy, remove the stale inner policy
before reinstalling the policy outside timing. This preserves POLICY -> TIMING
-> TRUE BASE without recursion or double validation."
  (when (and installed (fboundp timing-base) (fboundp policy-base)
             (eq (fdefinition timing-base) installed))
    (setf (fdefinition timing-base) (fdefinition policy-base))))

(defun %temporal-response-policy-install-wrapper (target timing-base policy-base
                                                   installed wrapper)
  (%temporal-response-policy-unwrap-timing-base timing-base installed policy-base)
  (let ((current (fdefinition target)))
    (unless (and installed (eq current installed))
      (setf (fdefinition policy-base) current))
    (setf (fdefinition target) wrapper)
    wrapper))

;; CALL-MODEL is a seam (P0c item 3): a registered layer, reload-safe by
;; construction, needing none of %TEMPORAL-RESPONSE-POLICY-INSTALL-WRAPPER's
;; unwrap-and-reinstall dance -- that dance existed only to keep POLICY ->
;; TIMING -> TRUE BASE ordering stable across the old idiom's reload hazards.
;; :ORDER 100 states the same ordering as data instead.
(register-layer call-model temporal-response-policy :order 100
  :function #'%temporal-response-policy-call-model)

(setf *temporal-response-policy-installed-raw-wrapper*
      (%temporal-response-policy-install-wrapper
       'raw-call-model 'pai-base-raw-call-model-timing
       'pai-base-raw-call-model-temporal-policy
       *temporal-response-policy-installed-raw-wrapper*
       #'%temporal-response-policy-raw-call-model))
