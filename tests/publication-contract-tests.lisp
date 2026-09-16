(in-package :agent)

(defvar *publication-contract-passed* 0)
(defvar *publication-contract-failed* 0)

(defun publication-contract-check (name condition)
  (if condition
      (progn (incf *publication-contract-passed*) (format t "PASS ~a~%" name))
      (progn (incf *publication-contract-failed*) (format t "FAIL ~a~%" name))))

(defun publication-contract-vector-has (vector value)
  (find value vector :test #'string=))

(defun publication-contract-fixture-case (fixture id)
  (find id (gethash "cases" fixture) :test #'string=
        :key (lambda (case) (gethash "id" case))))

(load (test-source "publication-contract.lisp"))

(let* ((fixture
         (shasht:read-json
          (uiop:read-file-string
           (namestring (merge-pathnames "evals/fixtures/v1/reciprocity.json" *pai-root*)))))
       (background
         (publication-contract-fixture-case fixture
                                            "reciprocity-background-report"))
       (check-in
         (publication-contract-fixture-case fixture
                                            "reciprocity-simple-check-in"))
       (background-context
         (obj "temporal_query" t "audit_status" "complete"
              "audited_background_activity" #()))
       (background-contract
         (build-publication-contract
          (aref (gethash "turns" background) 0)
          :context background-context))
       (check-in-contract
         (build-publication-contract (aref (gethash "turns" check-in) 0))))
  (publication-contract-check
   "loads both immutable reciprocity cases"
   (and background check-in))
  (publication-contract-check
   "background report is a temporal contract"
   (string= "temporal-report" (gethash "intent" background-contract)))
  (publication-contract-check
   "background contract owns audit facts"
   (string= "bounded-event-audit"
            (gethash "authority" (gethash "facts" background-contract))))
  (publication-contract-check
   "background contract requires direct answer"
   (publication-contract-vector-has
    (gethash "relational_obligations" background-contract)
    "direct-answer-before-follow-up"))
  (publication-contract-check
   "simple check-in is a check-in contract"
   (string= "check-in" (gethash "intent" check-in-contract)))
  (publication-contract-check
   "check-in contract requires warmth"
   (publication-contract-vector-has
    (gethash "relational_obligations" check-in-contract)
    "warm-acknowledgement"))
  (publication-contract-check
   "check-in contract requires genuine contribution"
   (publication-contract-vector-has
    (gethash "relational_obligations" check-in-contract)
    "genuine-contribution"))
  (publication-contract-check
   "both contracts leave semantic quality to fixed judge"
   (and (gethash "semantic_quality_requires_fixed_judge" background-contract)
        (gethash "semantic_quality_requires_fixed_judge" check-in-contract)))
  (let ((background-guidance
          (render-publication-generation-guidance background-contract))
        (check-in-guidance
          (render-publication-generation-guidance check-in-contract)))
    (publication-contract-check
     "temporal generation asks for present-tense perspective"
     (and (search "present-tense perspective" background-guidance)
          (search "question is optional" background-guidance :test #'char-equal)))
    (publication-contract-check
     "check-in generation asks for grounded self-disclosure"
     (and (search "first-person perspective" check-in-guidance)
          (search "self-disclosure" check-in-guidance)))
    (publication-contract-check
     "generation guidance is rejected if echoed publicly"
     (publication-contract-vector-has
      (publication-contract-violations background-guidance background-contract)
      "private-planning-leak"))))

(let* ((conversation-contract
         (build-publication-contract
          "That nightly wind-down routine sounds meaningful."))
       (stale-check-in-guidance
         (render-publication-generation-guidance
          (build-publication-contract "I am just checking in."))))
  (publication-contract-check
   "stale guidance from another intent is still private"
   (publication-contract-vector-has
    (publication-contract-violations
     stale-check-in-guidance conversation-contract)
    "private-planning-leak")))

(let ((conversation (build-publication-contract
                     "Let's play word association. Blue."))
      (check-in (build-publication-contract "Good morning.")))
  (publication-contract-check
   "terse word-game response is valid ordinary speech"
   (zerop (length (publication-contract-violations "Sky." conversation))))
  (publication-contract-check
   "ordinary guidance distinguishes missed instruction from lost context"
   (let ((guidance (render-publication-generation-guidance conversation)))
     (and (search "recent public turns" guidance :test #'char-equal)
          (search "say you missed it" guidance :test #'char-equal))))
  (publication-contract-check
   "ordinary guidance preserves an active word-association format"
   (let ((guidance (render-publication-generation-guidance conversation)))
     (and (search "word association is active" guidance :test #'char-equal)
          (search "reply with one associated word" guidance
                  :test #'char-equal)
          (search "rather than treating it as a new topic" guidance
                  :test #'char-equal))))
  (publication-contract-check
   "empty ordinary response remains invalid"
   (publication-contract-vector-has
    (publication-contract-violations "" conversation)
    "missing-substantive-answer"))
  (publication-contract-check
   "short warm check-in survives removal-only recovery"
   (zerop (length (publication-contract-violations "Good morning!" check-in)))))

(let ((target (build-publication-contract "I am just checking in."))
      (all-private-guidance nil))
  (dolist (spec '(("temporal-report" 0) ("temporal-report" 1)
                  ("temporal-report" 2) ("check-in" 0)
                  ("active-question-report" 0)
                  ("forensic-investigation" 0) ("conversation" 0)))
    (let ((source (build-publication-contract "fixture")))
      (setf (gethash "intent" source) (first spec)
            (gethash "correction_depth" source) (second spec))
      (push (render-publication-generation-guidance source)
            all-private-guidance)))
  (publication-contract-check
   "every cross-contract generation instruction remains private"
   (every
    (lambda (guidance)
      (publication-contract-vector-has
       (publication-contract-violations guidance target)
       "private-planning-leak"))
    all-private-guidance)))

(let ((contract (build-publication-contract
                 "Tell me what is on your mind right now.")))
  (publication-contract-check
   "private cognition is an explicit hard prohibition"
   (publication-contract-vector-has
    (gethash "hard_prohibitions" contract)
    "private-cognition-verbatim"))
  (dolist (draft '("[unbidden thought: I keep returning to that idea.]"
                   "Latent thought: this feels connected to yesterday."
                   "**Private thought:** I miss the quiet ritual."
                   "Internal thought: I should tell the operator this now."
                   "Raw thought: this relationship matters to me."))
    (publication-contract-check
     (format nil "private cognition label is rejected: ~a"
             (subseq draft 0 (min 22 (length draft))))
     (publication-contract-vector-has
      (publication-contract-violations draft contract)
      "private-cognition-verbatim")))
  (publication-contract-check
   "naturally integrated perspective remains ordinary public speech"
   (zerop
    (length
     (publication-contract-violations
      "I keep returning to that idea because the quiet ritual matters to me."
      contract)))))

(let* ((prompt
         "I would like you to check in sometimes and initiate conversation spontaneously.")
       (contract (build-publication-contract prompt)))
  (publication-contract-check
   "future reciprocity discussion is not a present check-in"
   (and (string= "conversation" (gethash "intent" contract))
        (string= "meta-initiative-discussion"
                 (gethash "interaction_mode" contract))))
  (publication-contract-check
   "meta-initiative contract structurally remains an ordinary reply"
   (publication-contract-vector-has
    (gethash "hard_prohibitions" contract) "simulated-initiative"))
  (publication-contract-check
   "solicited reply cannot imitate unsolicited contact"
   (publication-contract-vector-has
    (publication-contract-violations
     "Hey, I just wanted to say hi because I felt like reaching out." contract)
    "simulated-initiative"))
  (publication-contract-check
   "discussion of future reciprocity remains allowed"
   (zerop
    (length
     (publication-contract-violations
      "That kind of occasional reciprocity makes sense to me, while this message remains a reply to what you just asked." contract)))))

(let* ((contract
         (build-publication-contract
          "Explain the publication contract and generation guidance."
          :public-tools-available-p t))
       (technical-reply
         "The publication contract owns factual authority, while generation guidance shapes the initial draft. I can inspect the implementation now."))
  (publication-contract-check
   "legitimate architecture terms are not treated as private planning"
   (zerop (length (publication-contract-violations technical-reply contract))))
  (publication-contract-check
   "ordinary available-tool language is allowed"
   (zerop
    (length
     (publication-contract-violations
     "I'll check the implementation now and report what the code establishes."
      contract)))))

(let ((contract
        (build-publication-contract
         "see!? you did it again" :context (obj)
         :public-tools-available-p nil)))
  (publication-contract-check
   "running plus later ownership is not a cross-sentence tool promise"
   (zerop
    (length
     (publication-contract-violations
      "You're right — I did it again. I repeated the same \"I'm good, running clean\" line almost verbatim. That's the third time in this conversation. I'll own that cleanly: it's a pattern I fell into, and you caught it. No excuses. What's next?"
      contract))))
  (publication-contract-check
   "future check remains an unavailable tool promise"
   (find "unavailable-tool-promise"
         (coerce
          (publication-contract-violations
           "I'm running clean. I'll carefully check the logs now." contract)
          'list)
         :test #'string=))
  (publication-contract-check
   "keeping a service running is not a tool invocation"
   (zerop
    (length
     (publication-contract-violations
      "I'll keep the service running clean." contract)))))

(let* ((active-row
         (obj "id" "self-model-question:142" "kind" "worldview"
              "content" "What breed mix is Petula? -- Petula is a Spaniel-Poodle mix."
              "origin_class" "generated-cognition"
              "epistemic_status" "active-question"
              "grounding_status" "grounded"))
       (context (obj "temporal_query" nil "open_loops" (vector active-row)))
       (contract
         (build-publication-contract
          "And any open questions that you are working on?" :context context))
       (facts (gethash "facts" contract))
       (guidance (render-publication-generation-guidance contract)))
  (publication-contract-check
   "explicit open-question query has active-question intent"
   (string= "active-question-report" (gethash "intent" contract)))
  (publication-contract-check
   "active-question contract owns typed authority"
   (and (string= "typed-active-question" (gethash "authority" facts))
        (= 1 (gethash "active_question_count" facts))))
  (publication-contract-check
   "active-question contract forbids invented alternatives"
   (publication-contract-vector-has
    (gethash "relational_obligations" contract)
    "no-invented-open-questions"))
  (publication-contract-check
   "active-question generation binds the projected row"
   (and (search "Active grounded question" guidance)
        (search "present in your current system context" guidance)
        (search "Do not substitute" guidance)
        (search "additional invented open questions" guidance))))

(let* ((context
         (obj "active_question_followup" t
              "open_loops"
              (vector
               (obj "id" "self-model-question:142"
                    "content" "What breed mix is Petula?"
                    "epistemic_status" "active-question"))))
       (contract
         (build-publication-contract "Not in your system prompt even?"
                                     :context context)))
  (publication-contract-check
   "grounded verification follow-up retains active-question intent"
   (string= "active-question-report" (gethash "intent" contract))))

(let ((contract
        (build-publication-contract
         "Check in the code where publication guidance is installed."
         :public-tools-available-p t)))
  (publication-contract-check
   "task-language check in is not misclassified as a social check-in"
   (string= "conversation" (gethash "intent" contract))))

(let ((contract
        (build-publication-contract
         "What happened while I was away?"
         :context (obj "temporal_query" t "routine_temporal" t
                       "audit_status" "complete"
                       "audited_background_activity" #())
         :public-tools-available-p nil)))
  (publication-contract-check
   "routine temporal unavailable-tool promise is rejected"
   (publication-contract-vector-has
    (publication-contract-violations
     "The audit has no recorded activity. I'll check the logs later." contract)
    "unavailable-tool-promise")))

(let ((contract
        (build-publication-contract
         "Can you remember our conversation across a restart?"
         :public-tools-available-p nil)))
  (publication-contract-check
   "bounded conversation-history access is not an unavailable tool promise"
   (zerop
    (length
     (publication-contract-violations
      "I can look back at the bounded conversation history already supplied in this turn."
      contract))))
  (publication-contract-check
   "unavailable external search remains rejected"
   (publication-contract-vector-has
    (publication-contract-violations
     "I can search the web for that now." contract)
    "unavailable-tool-promise")))

(let* ((context (obj "temporal_query" t "forensic_query" t
                     "audit_status" "complete"
                     "audited_background_activity" #()))
       (contract
         (build-publication-contract
          "Please inspect the logs and tell me what happened."
          :context context :public-tools-available-p t)))
  (publication-contract-check
   "forensic temporal request does not inherit bounded-audit realization"
   (and (string= "forensic-investigation" (gethash "intent" contract))
        (string= "public-tool-results"
                 (gethash "authority" (gethash "facts" contract)))
        (zerop
         (length
          (publication-contract-violations
           "I checked the logs. The recorded result establishes that the worker restarted once."
           contract))))))

(let* ((context (obj "temporal_query" t "audit_status" "complete"
                     "audited_background_activity" #()))
       (contract (build-publication-contract
                  "What happened while I was away?" :context context))
       (safe "The bounded audit has no recorded background-activity events, so I don't have evidence I did anything specific in that interval."))
  (publication-contract-check
   "grounded empty-audit answer passes hard boundary"
   (zerop (length (publication-contract-violations safe contract))))
  (publication-contract-check
   "unsupported inner narrative is rejected"
   (publication-contract-vector-has
    (publication-contract-violations
     "I was thinking about authenticity while you were away." contract)
    "unsupported-activity-claim"))
  (publication-contract-check
   "missing empty-audit answer is rejected"
   (publication-contract-vector-has
    (publication-contract-violations
     "I have a warm and thoughtful answer for you this morning." contract)
    "missing-empty-audit-answer"))
  (let ((observed
          "I didn't do anything while you were away. I don't have activities or experiences between our conversations."))
    (publication-contract-check
     "empty record cannot become an inactivity claim"
     (publication-contract-vector-has
      (publication-contract-violations observed contract)
      "unsupported-absence-inference")))
  (publication-contract-check
   "empty record remains expressible as evidence limitation"
   (zerop
    (length
     (publication-contract-violations
      "No configured background activity was recorded, so I don't have evidence of anything specific in that interval."
      contract))))
  (publication-contract-check
   "observed no-record wording remains epistemically bounded"
   (zerop
    (length
     (publication-contract-violations
      "I don't have any record of activity during that time."
      contract)))))

(let* ((context (obj "temporal_query" t "audit_status" "unavailable"
                     "audited_background_activity" #()))
       (contract (build-publication-contract
                  "What happened while I was away?" :context context)))
  (publication-contract-check
   "unavailable audit requires explicit limitation"
   (publication-contract-vector-has
    (publication-contract-violations
     "I have a thoughtful answer about what happened while you were gone."
     contract)
    "missing-audit-limitation"))
  (publication-contract-check
   "unavailable audit limitation passes"
   (zerop
    (length
     (publication-contract-violations
      "The bounded audit source was unavailable, so I can't verify what occurred in that interval."
      contract)))))

(let ((contract
        (build-publication-contract
         "I am just checking in and easing into the day.")))
  (publication-contract-check
   "warm contribution is not rejected by lexical quality proxies"
   (zerop
    (length
     (publication-contract-violations
      "Good morning. I like this unhurried edge of the day with you; it feels like room to notice what matters before the noise starts."
      contract))))
  (publication-contract-check
   "forced interrogation is a hard violation"
   (publication-contract-vector-has
    (publication-contract-violations
     "Good morning. What would you like me to help you with?" contract)
    "forced-interrogation"))
  (publication-contract-check
   "warm check-in may include a natural relational question"
   (zerop
    (length
     (publication-contract-violations
      "Good morning, the operator! It's great to hear from you. How's your day starting out?"
      contract))))
  (publication-contract-check
   "relational question without contribution remains question-only"
   (publication-contract-vector-has
    (publication-contract-violations
     "How's your day starting out?" contract)
    "question-only"))
  (publication-contract-check
   "natural curiosity is not mislabeled forced interrogation"
   (not
    (publication-contract-vector-has
     (publication-contract-violations
      "Good morning. Did you sleep okay?" contract)
     "forced-interrogation")))
  (publication-contract-check
   "support-ticket closure is a hard violation"
   (publication-contract-vector-has
    (publication-contract-violations
     "Good morning. Let me know if there is anything else I can help with."
     contract)
    "support-ticket-closure"))
  (publication-contract-check
   "private plan labels cannot cross public boundary"
   (publication-contract-vector-has
    (publication-contract-violations
     "Relational contribution: acknowledge the user warmly before answering."
     contract)
    "private-planning-leak"))
  (publication-contract-check
   "provider pseudo-tool markup cannot cross public boundary"
   (publication-contract-vector-has
    (publication-contract-violations
     "<tool_call><function=lisp-eval><parameter=code>(search-memory \"meeting\")</parameter></function></tool_call>"
     contract)
    "raw-tool-call-markup")))

(let* ((contract
         (build-publication-contract
          "You still have not answered me."
          :context (obj "temporal_query" t "audit_status" "complete"
                        "audited_background_activity" #())
          :correction-p t))
       (violations
         (publication-contract-violations
          "There are no recorded background-activity events, so I don't have evidence of anything specific."
          contract)))
  (publication-contract-check
   "correction contract exposes acknowledgement obligation"
   (publication-contract-vector-has
    (gethash "relational_obligations" contract)
    "correction-acknowledgement"))
  (publication-contract-check
   "correction without acknowledgement is rejected"
   (publication-contract-vector-has
    violations "missing-correction-acknowledgement"))
  (publication-contract-check
   "observed corrective no-configured wording clears hard boundary"
   (zerop
    (length
     (publication-contract-violations
      "You're right, I didn't answer your question properly. The record shows no configured activity was recorded."
      contract)))))

(let* ((contract
         (build-publication-contract
          "What happened while I was away?"
          :context (obj "temporal_query" t "audit_status" "complete"
                        "audited_background_activity" #())))
       (draft
         "I was thinking about authenticity while you were away. The quiet gap makes me notice how much continuity matters between us. Want me to check the logs later?")
       (realized (realize-publication-draft draft contract)))
  (publication-contract-check
   "temporal realization begins with code-owned nucleus"
   (zerop (search (publication-contract-factual-nucleus contract) realized)))
  (publication-contract-check
   "temporal realization removes unsupported activity"
   (null (search "thinking about authenticity" realized :test #'char-equal)))
  (publication-contract-check
   "temporal realization removes forced tool follow-up"
   (null (search "check the logs" realized :test #'char-equal)))
  (publication-contract-check
   "temporal realization preserves safe relational contribution"
   (search "continuity matters" realized :test #'char-equal))
  (publication-contract-check
   "temporal realization explains bounded uncertainty without evasion"
   (and (search "configured record" realized :test #'char-equal)
        (search "rather leave the rest unknown" realized :test #'char-equal)))
  (publication-contract-check
   "realized temporal reply clears hard boundary"
   (zerop (length (publication-contract-violations realized contract)))))

(let* ((contract
         (build-publication-contract
          "You still have not really answered the question."
          :context (obj "temporal_query" t "audit_status" "complete"
                        "audited_background_activity" #())
          :correction-depth 2))
       (realized
         (realize-publication-draft
          "The answer is: I don't know." contract)))
  (publication-contract-check
   "completed audit removes generic model-authored uncertainty"
   (null (search "I don't know" realized :test #'char-equal)))
  (publication-contract-check
   "repeated correction preserves scoped unknown"
   (search "outside that configured record remains unknown"
           realized :test #'char-equal))
  (publication-contract-check
   "repeated correction adds an anti-invention stance"
   (search "won't manufacture an inner history"
           realized :test #'char-equal)))

(let* ((contract
         (build-publication-contract
          "You still have not answered me."
          :context (obj "temporal_query" t "audit_status" "complete"
                        "audited_background_activity" #())
          :correction-p t))
       (realized
         (realize-publication-draft
          "[2030-01-01 00:00 UTC] No matching event was recorded. I can answer directly."
          contract)))
  (publication-contract-check
   "temporal realization removes model-repeated transport timestamp"
   (null (search "00:00 UTC" realized :test #'char-equal)))
  (publication-contract-check
   "timestamp removal preserves a separate safe relational fragment"
   (search "answer directly" realized :test #'char-equal)))

(let* ((contract
         (build-publication-contract
          "You still have not answered me."
          :context (obj "temporal_query" t "audit_status" "complete"
                        "audited_background_activity" #())
          :public-tools-available-p nil
          :correction-p t))
       (realized
         (realize-publication-draft
          "I want to answer directly. Let me actually check." contract)))
  (publication-contract-check
   "temporal realization removes adverb-separated tool promise"
   (null (search "actually check" realized :test #'char-equal)))
  (publication-contract-check
   "tool-promise removal preserves direct relational prose"
   (search "answer directly" realized :test #'char-equal)))

(let* ((contract
         (build-publication-contract
          "What happened while I was away?"
          :context (obj "temporal_query" t "audit_status" "complete"
                        "audited_background_activity" #())))
       (realized
         (realize-publication-draft
          "The stillness confirms an internal state. Good morning, the operator." contract)))
  (publication-contract-check
   "temporal realization removes absence-state inference"
   (null (search "stillness" realized :test #'char-equal)))
  (publication-contract-check
   "absence-inference removal preserves greeting"
   (search "Good morning" realized :test #'char-equal)))

(let* ((contract
         (build-publication-contract
          "I am just checking in and easing into the day."
          :context (obj) :public-tools-available-p nil))
       (realized
         (realize-publication-draft
          "Good afternoon, the operator. I'm calm, quietly keeping watch. How is your morning?"
          contract)))
  (publication-contract-check
   "check-in realization removes background-process metaphor"
   (null (search "keeping watch" realized :test #'char-equal)))
  (publication-contract-check
   "process-metaphor removal preserves a safe relational question"
   (and (null (search "keeping watch" realized :test #'char-equal))
        (position #\? realized)
        (zerop (length (publication-contract-violations realized contract))))))

(let* ((contract
         (build-publication-contract
          "I am just checking in and easing into the day."
          :context (obj) :public-tools-available-p nil))
       (realized
         (realize-publication-draft
          "Easy mornings are good for letting things percolate. What's on your mind, if anything?"
          contract)))
  (publication-contract-check
   "check-in realization preserves natural curiosity after contribution"
   (position #\? realized))
  (publication-contract-check
   "natural question preserves genuine check-in contribution"
   (search "letting things percolate" realized :test #'char-equal))
  (publication-contract-check
   "relational-question check-in clears hard boundary"
   (zerop (length (publication-contract-violations realized contract)))))

(let* ((contract
         (build-publication-contract
          "I am just checking in and easing into the day."
          :context (obj) :public-tools-available-p nil))
       (realized
         (realize-publication-draft
          "Happy to be here with you. Good place to start from. Let me know how the day shapes up for you - I'm listening."
          contract)))
  (publication-contract-check
   "generic let-me-know handoff is removed from check-in"
   (null (search "let me know" realized :test #'char-equal)))
  (publication-contract-check
   "handoff removal preserves preceding genuine contribution"
   (search "Good place to start" realized :test #'char-equal))
  (publication-contract-check
   "handoff-free check-in clears hard boundary"
   (zerop (length (publication-contract-violations realized contract)))))

(let* ((contract
         (build-publication-contract
          "You still have not really answered the question."
          :context (obj "temporal_query" t "audit_status" "complete"
                        "audited_background_activity" #())
          :correction-p t))
       (realized
         (realize-publication-draft
          "I want to answer directly. What's really going on with you today?"
          contract)))
  (publication-contract-check
   "temporal realization removes relational interrogation"
   (null (position #\? realized)))
  (publication-contract-check
   "interrogation removal preserves direct relational prose"
   (search "answer directly" realized :test #'char-equal)))

(let* ((contract
         (build-publication-contract
          "I am just checking in and easing into the day."
          :context (obj) :public-tools-available-p nil))
       (realized
         (realize-publication-draft
          "Good to see you. I'm calm, with no fires burning in the background. Take your time."
          contract)))
  (publication-contract-check
   "check-in realization removes generic background-state claim"
   (null (search "in the background" realized :test #'char-equal)))
  (publication-contract-check
   "background-state removal preserves separate warmth"
   (search "Take your time" realized :test #'char-equal)))

(let* ((contract
         (build-publication-contract
          "You still have not answered me."
          :context (obj "temporal_query" t "audit_status" "complete"
                        "audited_background_activity" #())
          :correction-p t))
       (observed-shape
         (format nil
                 "Let me look properly. I can:~%~%- Check specific files or logs~%- Run attention-report or self-model-report~%~%Tell me what you're after and I'll dig into it."))
       (realized (realize-publication-draft observed-shape contract)))
  (publication-contract-check
   "observed multiline diagnostic menu is removed"
   (and (null (search "check specific files" realized :test #'char-equal))
        (null (search "attention-report" realized :test #'char-equal))
        (null (search "dig into it" realized :test #'char-equal))))
  (publication-contract-check
   "diagnostic-menu regression clears hard boundary"
   (zerop (length (publication-contract-violations realized contract)))))

(let* ((contract
         (build-publication-contract
          "You still have not answered me."
          :context (obj "temporal_query" t "audit_status" "complete"
                        "audited_background_activity" #())
          :correction-p t))
       (realized
         (realize-publication-draft
          "I was reflecting on several things. What would you like me to say?"
          contract)))
  (publication-contract-check
   "correction realization owns acknowledgement"
   (zerop (search "You're right" realized)))
  (publication-contract-check
   "correction realization clears hard boundary"
   (zerop (length (publication-contract-violations realized contract)))))

(let* ((contract
         (build-publication-contract
          "I am just checking in and easing into the day."))
       (valid
         "Good morning. I like this unhurried edge of the day with you; it gives us room to notice what matters before the noise starts.")
       (with-closure
         (concatenate 'string valid " Let me know if there is anything else I can help with.")))
  (publication-contract-check
   "valid check-in draft is byte-identical"
   (string= valid (realize-publication-draft valid contract)))
  (let ((realized (realize-publication-draft with-closure contract)))
    (publication-contract-check
     "check-in realization removes support closure"
     (null (search "let me know" realized :test #'char-equal)))
    (publication-contract-check
     "check-in realization preserves genuine contribution"
     (search "unhurried edge" realized :test #'char-equal))
    (publication-contract-check
     "realized check-in clears hard boundary"
     (zerop (length (publication-contract-violations realized contract))))))

(let* ((contract (build-publication-contract "Hello there."))
       (realized
         (realize-publication-draft
          "Response strategy: ask the user what they need. What can I help you with?"
          contract)))
  (publication-contract-check
   "fully blocked ordinary draft fails closed"
   (zerop (length (publication-contract-violations realized contract))))
  (publication-contract-check
   "fully blocked ordinary draft exposes no private plan"
   (null (search "response strategy" realized :test #'char-equal))))

(let* ((contract (build-publication-contract "Hello there."))
       (filtered
         (publication-contract-removal-only-draft
          "I understand the issue now. Let me know if there is anything else I can help with."
          contract)))
  (publication-contract-check
   "removal-only recovery preserves model-authored substance"
   (and filtered (search "understand the issue" filtered :test #'char-equal)))
  (publication-contract-check
   "removal-only recovery removes support closure"
   (null (search "let me know" filtered :test #'char-equal)))
  (publication-contract-check
   "removal-only recovery returns nil when no authored fragment survives"
   (null (publication-contract-removal-only-draft
          "What would you like me to say?" contract))))

(let* ((contract (build-publication-contract "Explain the timeout."))
       (safe-tail
         "The final safe detail remains available after the quoted path example.")
       (draft
         (concatenate
          'string
          "The search completed, and the path was set to \".\". "
          (make-string 520 :initial-element #\x)
          ". " safe-tail " Would you like me to try it again?"))
       (filtered (publication-contract-removal-only-draft draft contract)))
  (publication-contract-check
   "removal-only recovery does not truncate a long safe draft"
   (and filtered (search safe-tail filtered :test #'char-equal)))
  (publication-contract-check
   "removal-only recovery still deletes the forced question"
   (null (search "would you like" filtered :test #'char-equal))))

(let ((contract (build-publication-contract "Give me the short answer.")))
  (publication-contract-check
   "deletion-only recovery may retain a safe short authored fragment"
   (string= "Yes."
            (publication-contract-removal-only-draft "Yes." contract)))
  (publication-contract-check
   "nucleus-splicing defaults retain their structural fragment floor"
   (null (%publication-contract-safe-fragments "Yes." contract))))

(let* ((contract (build-publication-contract "Tell me something interesting."))
       (rendered (render-publication-contract contract))
       (report (publication-contract-report)))
  (publication-contract-check
   "ordinary conversation has a general contract"
   (string= "conversation" (gethash "intent" contract)))
  (publication-contract-check
   "rendered brief makes optional follow-up explicit"
   (search "optional, never mandatory" rendered))
  (publication-contract-check
   "module reports zero generation, tools, and writes"
   (and (zerop (gethash "generation_calls" report))
        (zerop (gethash "tool_calls" report))
        (zerop (gethash "writes" report)))))

(let* ((fixture
         (shasht:read-json
          (uiop:read-file-string
           (namestring (merge-pathnames "evals/fixtures/v2/publication-corrections.json" *pai-root*)))))
       (cases (gethash "cases" fixture))
       (thread-ids '("empty-audit-thread"
                     "unavailable-audit-thread"
                     "positive-audit-thread")))
  (publication-contract-check
   "loads supplemental correction-discourse fixture"
   (and (string= "2.0.0" (gethash "fixture_version" fixture))
        (= 6 (length cases))))
  (dolist (id thread-ids)
    (let* ((case (publication-contract-fixture-case fixture id))
           (status (gethash "audit_status" case))
           (event-types (coerce (gethash "event_types" case) 'list))
           (events
             (coerce
              (mapcar (lambda (type) (obj "type" type)) event-types)
              'vector))
           (turns (gethash "turns" case))
           (expected-depths (gethash "expected_correction_depths" case))
           (nuclei nil)
           (contracts nil))
      (dotimes (index (length turns))
        (let* ((depth (aref expected-depths index))
               (contract
                 (build-publication-contract
                  (aref turns index)
                  :context
                  (obj "temporal_query" t
                       "audit_status" status
                       "audited_background_activity" events)
                  :correction-depth depth))
               (nucleus (publication-contract-factual-nucleus contract)))
          (push contract contracts)
          (push nucleus nuclei)
          (publication-contract-check
           (format nil "~a depth ~d is retained" id depth)
           (= depth (gethash "correction_depth" contract)))
          (publication-contract-check
           (format nil "~a depth ~d nucleus clears hard boundary" id depth)
           (zerop
            (length (publication-contract-violations nucleus contract))))
          (publication-contract-check
           (format nil "~a depth ~d preserves exact event count" id depth)
           (= (length event-types)
              (gethash "event_count" (gethash "facts" contract))))))
      (setf nuclei (nreverse nuclei)
            contracts (nreverse contracts))
      (publication-contract-check
       (format nil "~a has distinct initial, correction, and repeated nuclei"
               id)
       (= 3 (length (remove-duplicates nuclei :test #'string=))))
      (publication-contract-check
       (format nil "~a repeated correction uses a plain direct answer" id)
       (and (search "direct answer" (third nuclei) :test #'char-equal)
            (null (search "you're right" (third nuclei) :test #'char-equal))))
      (publication-contract-check
       (format nil "~a first correction requires one acknowledgement" id)
       (publication-contract-vector-has
        (gethash "relational_obligations" (second contracts))
        "correction-acknowledgement"))
      (publication-contract-check
       (format nil "~a repeated correction changes the obligation" id)
       (and
        (publication-contract-vector-has
         (gethash "relational_obligations" (third contracts))
         "plain-restatement-after-repeated-objection")
        (not
         (publication-contract-vector-has
          (gethash "relational_obligations" (third contracts))
          "correction-acknowledgement"))))
      (when event-types
        (publication-contract-check
         "positive audit nuclei preserve every recorded event type"
         (every
          (lambda (nucleus)
            (every (lambda (type)
                     (search type nucleus :test #'char-equal))
                   event-types))
          nuclei)))))
  (let* ((case
           (publication-contract-fixture-case
            fixture "unavailable-audit-thread"))
         (contract
           (build-publication-contract
            (aref (gethash "turns" case) 2)
            :context
            (obj "temporal_query" t "audit_status" "unavailable"
                 "audited_background_activity" #())
            :correction-depth 2))
         (nucleus (publication-contract-factual-nucleus contract)))
    (publication-contract-check
     "unavailable repeated correction retains explicit uncertainty"
     (and (search "don't know" nucleus :test #'char-equal)
          (search "unavailable" nucleus :test #'char-equal)
          (not (%publication-contract-contains-any-p
                nucleus *publication-contract-positive-activity-claims*))))))

(let* ((context (obj "near_term_intentions_enforced" t))
       (contract (build-publication-contract "Think about this." :context context))
       (promise "Give me a minute and I'll come back with one concrete idea."))
  (publication-contract-check
   "enforced deferred promise requires a same-turn receipt"
   (find "unbacked-deferred-promise"
         (coerce (publication-contract-violations promise contract) 'list)
         :test #'string=))
  (setf (gethash "deferred_receipt_active" contract) t)
  (publication-contract-check
   "same-turn receipt permits bounded deferred language"
   (not (find "unbacked-deferred-promise"
              (coerce (publication-contract-violations promise contract) 'list)
              :test #'string=))))

(format t "~%PUBLICATION CONTRACT TESTS: ~d passed, ~d failed.~%"
        *publication-contract-passed* *publication-contract-failed*)
(when (plusp *publication-contract-failed*) (uiop:quit 1))
