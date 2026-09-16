(in-package :agent)

(defvar *temporal-policy-passed* 0)
(defvar *temporal-policy-failed* 0)
(defvar *temporal-policy-responses* nil)
(defvar *temporal-policy-raw-calls* 0)
(defvar *temporal-policy-events* nil)
(defvar *temporal-policy-last-raw-messages* nil)
(defvar *temporal-policy-seen-tool-counts* nil)
(defvar *timing-installed-wrappers* nil)

(define-condition public-response-unavailable (error)
  ((reason :initarg :reason :reader public-response-unavailable-reason)))

(defun temporal-policy-check (name condition)
  (if condition
      (progn (incf *temporal-policy-passed*) (format t "PASS ~a~%" name))
      (progn (incf *temporal-policy-failed*) (format t "FAIL ~a~%" name))))

(defun temporal-policy-response (content &optional tool-calls)
  (obj "choices"
       (vector (obj "message"
                    (obj "role" "assistant" "content" content
                         "tool_calls" (or tool-calls :null))))))

(defun raw-call-model (messages)
  (setf *temporal-policy-last-raw-messages* messages)
  (push (if (and (boundp '*tools*) (vectorp *tools*))
            (length *tools*) 0)
        *temporal-policy-seen-tool-counts*)
  (incf *temporal-policy-raw-calls*)
  (or (pop *temporal-policy-responses*)
      (error "No fixture response queued")))

;; CALL-MODEL is a seam (P0c item 3): TEMPORAL-RESPONSE-POLICY.LISP below
;; registers a layer on it, which requires the seam to already exist. A
;; plain DEFUN would not create one, and REGISTER-LAYER would error "No seam
;; named CALL-MODEL" (gotcha 12).
(define-seam call-model (messages) (raw-call-model messages))

(load (test-source "publication-contract.lisp"))
(load (test-source "epistemic-critic.lisp"))
(load (test-source "temporal-response-policy.lisp"))

(setf *temporal-response-policy-event-fn*
      (lambda (type payload) (push (list type payload) *temporal-policy-events*)))

(defun temporal-policy-context (&key (status "complete") (events #()))
  (obj "audit_status" status
       "audited_background_activity" events
       "current_exchange"
       (vector (obj "speaker" "the operator" "content"
                    "What happened while I was away?"))))

(defun temporal-policy-run (mode responses &key
                                  (context (temporal-policy-context)) correction
                                  contract)
  (setf *temporal-response-policy-mode* mode
        *temporal-policy-responses* responses
        *temporal-policy-raw-calls* 0
        *temporal-policy-seen-tool-counts* nil
        *temporal-policy-events* nil)
  (let ((*temporal-response-policy-context* context)
        (*temporal-response-policy-correction-p* correction)
        (*publication-contract-current* contract))
    (call-model nil)))

(let* ((draft "I was thinking about authenticity. Want me to check later?")
       (response (temporal-policy-run :legacy
                                     (list (temporal-policy-response draft)))))
  (temporal-policy-check
   "legacy leaves response unchanged"
   (string= draft (ref response "choices" 0 "message" "content")))
  (temporal-policy-check "legacy uses one raw call"
                         (= 1 *temporal-policy-raw-calls*))
  (temporal-policy-check "legacy emits no event" (null *temporal-policy-events*)))

(let* ((draft "I was thinking about authenticity. Want me to check later?")
       (response (temporal-policy-run :shadow
                                     (list (temporal-policy-response draft))))
       (event (first *temporal-policy-events*)))
  (temporal-policy-check
   "shadow is byte-identical"
   (string= draft (ref response "choices" 0 "message" "content")))
  (temporal-policy-check
   "shadow observes without naturalization"
   (and (= 1 *temporal-policy-raw-calls*)
        (string= "observed" (gethash "action" (second event))))))

(let* ((valid "I have no recorded background activity in that bounded window, so I don't have evidence I did anything specific.")
       (response (temporal-policy-run :enforced
                                     (list (temporal-policy-response valid))))
       (payload (second (first *temporal-policy-events*))))
  (temporal-policy-check
   "valid model speech is accepted byte-identically"
   (string= valid (ref response "choices" 0 "message" "content")))
  (temporal-policy-check "valid speech uses no second call"
                         (= 1 *temporal-policy-raw-calls*))
  (temporal-policy-check "acceptance is audited"
                         (string= "accepted" (gethash "action" payload))))

(let* ((draft "Want me to check the logs later?")
       (naturalized "I have no recorded background activity in that bounded window, so I don't have evidence I did anything specific.")
       (response (temporal-policy-run
                  :enforced
                  (list (temporal-policy-response draft)
                        (temporal-policy-response naturalized))))
       (payload (second (first *temporal-policy-events*)))
       (prompt (gethash "content" (second *temporal-policy-last-raw-messages*))))
  (temporal-policy-check
   "invalid draft is replaced only by model-authored naturalization"
   (string= naturalized (ref response "choices" 0 "message" "content")))
  (temporal-policy-check "naturalization is bounded to one additional call"
                         (= 2 *temporal-policy-raw-calls*))
  (temporal-policy-check "naturalization cannot inherit public tools"
                         (zerop (first *temporal-policy-seen-tool-counts*)))
  (temporal-policy-check
   "naturalization success is audited"
   (and (string= "naturalized" (gethash "action" payload))
        (gethash "repair_attempted" payload)
        (gethash "repair_valid" payload)))
  (temporal-policy-check "naturalization receives the private inputs"
                         (and (search "HIDDEN CONTRACT" prompt)
                              (search "REJECTED DRAFT" prompt))))

(let* ((draft "I explored several ideas. Want me to tell you more?")
       (bad-naturalization "What would you like me to say?")
       (condition
         (handler-case
             (progn
               (temporal-policy-run
                :enforced
                (list (temporal-policy-response draft)
                      (temporal-policy-response bad-naturalization)))
               nil)
           (public-response-unavailable (caught) caught)))
       (payload (second (first *temporal-policy-events*))))
  (temporal-policy-check "invalid naturalization signals system failure"
                         (typep condition 'public-response-unavailable))
  (temporal-policy-check "failure is bounded to two total calls"
                         (= 2 *temporal-policy-raw-calls*))
  (temporal-policy-check
   "naturalization makes correction acknowledgement mandatory"
   (search "Correction obligations are mandatory"
           (gethash "content" (first *temporal-policy-last-raw-messages*))))
  (temporal-policy-check
   "naturalization keeps empty records epistemically bounded"
   (search "Never turn an empty record into a claim"
           (gethash "content" (first *temporal-policy-last-raw-messages*))))
  (temporal-policy-check
   "naturalization attributes positive events to records"
   (search "explicitly attribute every reported event to the record"
           (gethash "content" (first *temporal-policy-last-raw-messages*))))
  (temporal-policy-check
   "failure cannot become deterministic companion speech"
   (string= "naturalization-failed" (gethash "action" payload))))

(let* ((draft "I can help with that. What would you like me to do?")
       (naturalized
         "I understand what went wrong. Let me know if there is anything else I can help with.")
       (context (obj "temporal_query" nil "audit_status" "not-requested"
                     "audited_background_activity" #()))
       (contract (build-publication-contract "Please fix that." :context context))
       (response
         (temporal-policy-run
          :enforced
          (list (temporal-policy-response draft)
                (temporal-policy-response naturalized))
          :context context :contract contract))
       (content (ref response "choices" 0 "message" "content"))
       (payload (second (first *temporal-policy-events*))))
  (temporal-policy-check
   "failed naturalization can use model-authored removal-only speech"
   (string= "I understand what went wrong." content))
  (temporal-policy-check
   "removal-only naturalization is explicitly audited"
   (string= "naturalized-removal-only" (gethash "action" payload)))
  (temporal-policy-check
   "removal-only naturalization remains bounded to two calls"
   (= 2 *temporal-policy-raw-calls*)))

(let* ((context (obj "temporal_query" nil "audit_status" "not-requested"
                     "audited_background_activity" #()))
       (contract (build-publication-contract "Hey good morning the agent"
                                             :context context))
       (pseudo-tool
         "<tool_call><function=lisp-eval><parameter=code>(search-memory \"morning\")</parameter></function></tool_call>")
       (naturalized "Good morning! What would you like me to help you with?")
       (response
         (temporal-policy-run
          :enforced
          (list (temporal-policy-response pseudo-tool)
                (temporal-policy-response naturalized))
          :context context :contract contract))
       (content (ref response "choices" 0 "message" "content"))
       (payload (second (first *temporal-policy-events*))))
  (temporal-policy-check
   "pseudo-tool text is never published and safe greeting is retained"
   (string= "Good morning!" content))
  (temporal-policy-check
   "pseudo-tool recovery is audited as removal-only"
   (string= "naturalized-removal-only" (gethash "action" payload))))

(let* ((tool-call (obj "id" "naturalizer-tool" "function"
                       (obj "name" "lisp-eval" "arguments" "{}")))
       (condition
         (handler-case
             (progn
               (temporal-policy-run
                :enforced
                (list (temporal-policy-response "I'll check the logs later.")
                      (temporal-policy-response "I found nothing."
                                                (vector tool-call))))
               nil)
           (public-response-unavailable (caught) caught))))
  (temporal-policy-check "naturalization tool call fails closed"
                         (typep condition 'public-response-unavailable)))

(let* ((tool-call (obj "id" "completed-image" "function"
                       (obj "name" "generate-image" "arguments" "{}")))
       (messages
         (list (obj "role" "system" "content" "fixture")
               (obj "role" "user" "content" "Make something creative")
               (obj "role" "assistant" "content" "I have an idea."
                    "tool_calls" (vector tool-call))
               (obj "role" "tool" "tool_call_id" "completed-image"
                    "content" "https://example.test/image.jpg")))
       (draft "Here is the image. Let me know if I got anything wrong.")
       (bad-naturalization "What would you like me to do?")
       (context (obj "temporal_query" nil "audit_status" "not-requested"
                     "audited_background_activity" #()))
       (contract (build-publication-contract "Make something creative"
                                             :context context)))
  (setf *temporal-response-policy-mode* :enforced
        *temporal-policy-responses*
        (list (temporal-policy-response draft)
              (temporal-policy-response bad-naturalization))
        *temporal-policy-raw-calls* 0
        *temporal-policy-events* nil
        *temporal-policy-seen-tool-counts* nil)
  (let* ((*temporal-response-policy-context* context)
         (*publication-contract-current* contract)
         (response (call-model messages))
         (payload (second (first *temporal-policy-events*))))
    (temporal-policy-check
     "failed polishing preserves original response after completed tool"
     (string= draft (ref response "choices" 0 "message" "content")))
    (temporal-policy-check
     "post-tool preservation is explicitly audited"
     (string= "post-tool-original-preserved" (gethash "action" payload)))
    (temporal-policy-check
     "post-tool naturalization is tool-ineligible"
     (zerop (first *temporal-policy-seen-tool-counts*)))))

(let* ((tool-call (obj "id" "call-1" "function"
                       (obj "name" "lisp-eval" "arguments" "{}")))
       (response (temporal-policy-run
                  :enforced
                  (list (temporal-policy-response :null (vector tool-call))))))
  (temporal-policy-check "ordinary non-final tool response is untouched"
                         (= 1 (length (ref response "choices" 0 "message"
                                           "tool_calls"))))
  (temporal-policy-check "non-final response uses one raw call"
                         (= 1 *temporal-policy-raw-calls*)))

(let* ((context (obj "temporal_query" t "audit_status" "complete"
                     "audited_background_activity" #()
                     "current_exchange"
                     (vector (obj "speaker" "the operator" "content"
                                  "What happened while I was away?"))))
       (contract (build-publication-contract
                  "What happened while I was away?" :context context))
       (draft "I was thinking about authenticity. Want me to check the logs later?")
       (naturalized "I don't have evidence of any recorded background activity in that window. The gap does make me notice how much continuity matters between us.")
       (response (temporal-policy-run
                  :enforced
                  (list (temporal-policy-response draft)
                        (temporal-policy-response naturalized))
                  :context context :contract contract))
       (content (ref response "choices" 0 "message" "content"))
       (payload (second (first *temporal-policy-events*))))
  (temporal-policy-check "temporal contract uses one model naturalization"
                         (= 2 *temporal-policy-raw-calls*))
  (temporal-policy-check "naturalized temporal speech is returned unchanged"
                         (string= naturalized content))
  (temporal-policy-check "deterministic audit nucleus is not published"
                         (null (search "bounded audit records" content)))
  (temporal-policy-check
   "temporal naturalization records intent without content"
   (and (string= "naturalized" (gethash "action" payload))
        (string= "temporal-report" (gethash "contract_intent" payload)))))

(let* ((context (obj "temporal_query" nil "audit_status" "not-requested"
                     "audited_background_activity" #()))
       (contract (build-publication-contract
                  "I am just checking in and easing into the day."
                  :context context))
       (draft "Good morning. Let me know if there is anything else I can help with.")
       (naturalized "Good morning. I like this quiet edge of the day with you; it gives us room to notice what matters.")
       (response (temporal-policy-run
                  :enforced
                  (list (temporal-policy-response draft)
                        (temporal-policy-response naturalized))
                  :context context :contract contract)))
  (temporal-policy-check "check-in is naturalized rather than code-rewritten"
                         (string= naturalized
                                  (ref response "choices" 0 "message" "content"))))

(let* ((context (obj "temporal_query" nil "audit_status" "not-requested"
                     "audited_background_activity" #()))
       (prompt
         "I would like you to check in sometimes and initiate conversation spontaneously.")
       (contract (build-publication-contract prompt :context context))
       (draft "Hey, I just wanted to say hi because I felt like reaching out.")
       (naturalized
         "I understand the distinction: you want occasional reciprocity, not a demonstration folded into this reply. This message is still only my response to you now.")
       (response (temporal-policy-run
                  :enforced
                  (list (temporal-policy-response draft)
                        (temporal-policy-response naturalized))
                  :context context :contract contract))
       (payload (second (first *temporal-policy-events*))))
  (temporal-policy-check
   "meta-initiative imitation is naturalized into ordinary discussion"
   (string= naturalized (ref response "choices" 0 "message" "content")))
  (temporal-policy-check
   "meta-initiative repair is audited without granting delivery authority"
   (and (string= "naturalized" (gethash "action" payload))
        (find "simulated-initiative" (gethash "violation_codes" payload)
              :test #'string=))))

(let* ((context (obj "temporal_query" t "forensic_query" t
                     "audit_status" "complete"
                     "audited_background_activity" #()))
       (contract (build-publication-contract
                  "Please inspect the logs and tell me what happened."
                  :context context :public-tools-available-p t))
       (draft "I checked the logs. The observed record shows that the worker restarted once, and I found no second restart.")
       (response (temporal-policy-run
                  :enforced (list (temporal-policy-response draft))
                  :context context :contract contract))
       (payload (second (first *temporal-policy-events*))))
  (temporal-policy-check "valid forensic result remains byte-identical"
                         (string= draft
                                  (ref response "choices" 0 "message" "content")))
  (temporal-policy-check "valid forensic result is accepted"
                         (and (= 1 *temporal-policy-raw-calls*)
                              (string= "accepted" (gethash "action" payload)))))

(let* ((context (temporal-policy-context))
       (observed "I don't have any record of activity during that time.")
       (response (temporal-policy-run
                  :enforced (list (temporal-policy-response observed))
                  :context context)))
  (temporal-policy-check
   "observed no-record wording is accepted without naturalization"
   (and (= 1 *temporal-policy-raw-calls*)
        (string= observed (ref response "choices" 0 "message" "content")))))

(let* ((context (obj "temporal_query" nil "audit_status" "not-requested"
                     "audited_background_activity" #()))
       (contract (build-publication-contract
                  "Tell me what is on your mind right now." :context context))
       (draft "[unbidden thought: I keep returning to our quiet ritual.]")
       (naturalized
         "I keep returning to our quiet ritual because it feels meaningful to me.")
       (response (temporal-policy-run
                  :enforced
                  (list (temporal-policy-response draft)
                        (temporal-policy-response naturalized))
                  :context context :contract contract))
       (payload (second (first *temporal-policy-events*)))
       (system-prompt
         (gethash "content" (first *temporal-policy-last-raw-messages*))))
  (temporal-policy-check
   "private cognition label is naturalized into ordinary speech"
   (string= naturalized (ref response "choices" 0 "message" "content")))
  (temporal-policy-check
   "private cognition repair is audited by violation code"
   (and (string= "naturalized" (gethash "action" payload))
        (find "private-cognition-verbatim"
              (gethash "violation_codes" payload) :test #'string=)))
  (temporal-policy-check
   "naturalizer explicitly treats private rows as evidence rather than prose"
   (and (search "not already-authored public prose" system-prompt)
        (search "never quote a raw private row verbatim" system-prompt))))

(setf *temporal-policy-responses*
      (list (temporal-policy-response "internal raw response"))
      *temporal-policy-raw-calls* 0)
(let ((*temporal-response-policy-context* (temporal-policy-context))
      (*temporal-response-policy-mode* :enforced))
  (raw-call-model nil))
(temporal-policy-check "direct internal raw call bypasses public policy"
                       (= 1 *temporal-policy-raw-calls*))

(load (test-source "temporal-response-policy.lisp"))
(let* ((valid "I have no recorded background activity, so I don't have evidence I did anything specific.")
       (response (temporal-policy-run :enforced
                                     (list (temporal-policy-response valid)))))
  (temporal-policy-check "plain reload remains callable without recursion"
                         (string= valid
                                  (ref response "choices" 0 "message" "content"))))

(format t "~%TEMPORAL RESPONSE POLICY TESTS: ~d passed, ~d failed.~%"
        *temporal-policy-passed* *temporal-policy-failed*)
(when (plusp *temporal-policy-failed*) (uiop:quit 1))
