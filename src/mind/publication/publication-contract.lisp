;;;; publication-contract.lisp -- pure public/private response boundary.
;;;;
;;;; This module does not generate, repair, publish, log, or persist anything.
;;;; It turns a user turn plus already-built context into an inspectable
;;;; contract with three deliberately separate parts:
;;;;   1. code-owned factual authority;
;;;;   2. relational obligations that require semantic evaluation; and
;;;;   3. deterministic hard prohibitions at the public boundary.
;;;;
;;;; Keeping those parts separate prevents a style model from inventing facts,
;;;; and prevents brittle word matching from pretending to measure warmth or a
;;;; genuine contribution.  A later response-policy experiment may consume the
;;;; contract, but loading this file alone cannot alter a public response.

(in-package :agent)

(export '(build-publication-contract render-publication-contract
          publication-contract-violations publication-contract-factual-nucleus
          render-publication-generation-guidance realize-publication-draft
          publication-contract-removal-only-draft
          publication-contract-report))

(defparameter *publication-contract-schema-version* 1)

(defparameter *publication-contract-check-in-phrases*
  '("checking in" "check in" "easing into" "starting the day"
    "good morning" "good evening" "just saying hello"))

(defparameter *publication-contract-task-check-phrases*
  '("check in the" "check in on" "check in with" "check into"
    "checking in on" "checking in with" "checking into"))

(defparameter *publication-contract-meta-initiative-phrases*
  '("spontaneous check-in" "spontaneous check in"
    "check in sometimes" "check in occasionally" "occasionally check in"
    "reach out sometimes" "reach out occasionally" "initiate contact"
    "initiate conversation" "start a conversation" "say hi because you want"
    "wanted you to check in" "want you to check in" "would like you to check in"))

(defparameter *publication-contract-simulated-initiative-phrases*
  '("just wanted to say hi" "just checking in" "wanted to check in"
    "thought i'd reach out" "thought i would reach out"))

(defparameter *publication-contract-active-question-phrases*
  '("open question" "open questions" "explore tick"
    "question are you working on" "questions are you working on"
    "question you're working on" "questions you're working on"))

(defparameter *publication-contract-forced-interrogation-phrases*
  '("what would you like me to" "do you want me to" "would you like me to"
    "shall i" "want me to" "what can i help you with"))

(defparameter *publication-contract-support-closure-phrases*
  '("let me know " "i'm here if you need" "i am here if you need"
    "anything else i can help" "happy to help with anything"))

(defparameter *publication-contract-tool-promise-phrases*
  '("i'll inspect" "i will inspect" "let me inspect" "i can inspect"
    "i'll search" "i will search" "let me search" "i can search"
    "i'll run" "i will run" "let me run" "i can run"
    "check the logs" "check specific files" "check the files"
    "check the code" "check the database" "check the web"
    "look up" "look into the logs" "look into the files"
    "look into the code" "look into the database" "look into the web"
    "dig into the logs" "dig into the files" "dig into the code"
    "run attention-report"
    "run self-model-report"))

(defparameter *publication-contract-tool-promise-guard-fragments*
  ;; A cheap negative guard only. Every positive is still decided by the
  ;; sentence-local token grammar, so these fragments grant no authority.
  '("i'll" "i will" "let me" "i can" "check" "look" "dig" "run"))

(defparameter *publication-contract-deferred-promise-phrases*
  '("give me a minute" "give me a moment" "let me think"
    "i'll think about" "i will think about" "i'll come back with"
    "i will come back with" "i'll get back to you" "i will get back to you"))

(defparameter *publication-contract-private-planning-phrases*
  '("i should respond" "i need to answer" "the user is asking"
    "the user wants" "natural opening:" "relational contribution:"
    "factual nucleus:" "draft to repair:" "final reply should"
    "response strategy:"
    ;; Generation guidance is private even if it belongs to a stale or
    ;; cross-turn contract.  Matching only the current contract allowed a
    ;; prior check-in instruction to pass on a conversation turn.
    "the user has objected repeatedly" "the prior answer did not land"
    "the factual audit answer is supplied separately"
    "acknowledge the user's stated mode warmly"
    "answer directly from the row labeled active grounded question"
    "use the available tools deliberately"
    "use the recent public turns already present in the conversation"
    "answer naturally and directly. when a grounded personal perspective would add value"))

(defparameter *publication-contract-raw-tool-markup-phrases*
  '("<tool_call" "</tool_call" "<tool-call" "</tool-call"
    "<function=" "</function>" "<parameter=" "</parameter>"))

(defparameter *publication-contract-private-cognition-labels*
  '("unbidden thought:" "latent thought:" "private thought:"
    "internal thought:" "raw thought:"))

(defparameter *publication-contract-positive-activity-claims*
  '("i spent time" "i worked on" "i explored" "i reflected on"
    "i was thinking about" "i developed" "i researched" "i investigated"))

(defparameter *publication-contract-unsupported-absence-inference-phrases*
  '("that stillness" "the stillness" "quiet on my end" "it was quiet"
    "everything was quiet" "nothing was going on" "nothing happened"
    "i was idle" "i've been idle" "i have been idle" "dormant"
    "inactive" "i didn't do anything" "i did not do anything"
    "i don't do anything" "i do not do anything"
    "i don't have activities" "i do not have activities"
    "i don't have experiences" "i do not have experiences"))

(defparameter *publication-contract-check-in-activity-claims*
  '("background drift" "idle drift" "keeping watch" "kept watch"
    "keeping the lights" "kept the lights" "keeping things tidy"
    "kept things tidy" "running in the background" "working in the background"
    "background process" "in the background" "maintenance" "monitoring"
    "i've " "i have "
    "i did " "i was " "i spent " "i had " "i thought " "i noticed "
    "worked on" "completed" "finished" "updated" "reviewed" "researched"
    "looked into" "made progress" "took care of" "remembered" "recalled"))

(defparameter *publication-contract-absence-markers*
  '("no configured" "no recorded" "nothing recorded" "none recorded"
    "nothing is recorded" "no background activity is recorded"
    "don't have any record" "do not have any record" "no record of"
    "didn't record" "did not record" "don't have evidence"
    "do not have evidence" "no evidence" "nothing specific"))

(defparameter *publication-contract-unavailable-markers*
  '("unavailable" "can't verify" "cannot verify" "couldn't verify"
    "could not verify" "don't know" "do not know" "not able to verify"))

(defparameter *publication-contract-correction-markers*
  '("you're right" "you are right" "sorry" "i should have"
    "i didn't answer" "i did not answer" "correction"))

(defun %publication-contract-text (value)
  (if (stringp value) value ""))

(defun %publication-contract-private-cognition-label-p (text)
  ;; Unlike the general phrase helper, avoid allocating a lower-cased copy on
  ;; every public draft and every candidate fragment. Every structural label
  ;; contains a colon, so the cheap guard avoids all searches on normal prose.
  (let ((value (%publication-contract-text text)))
    (and (position #\: value)
         (some (lambda (label)
                 (search label value :test #'char-equal))
               *publication-contract-private-cognition-labels*))))

(defun %publication-contract-list (value)
  (cond ((null value) nil)
        ((vectorp value) (coerce value 'list))
        ((listp value) value)
        (t (list value))))

(defun %publication-contract-contains-any-p (text phrases)
  (let ((lower (string-downcase (%publication-contract-text text))))
    (not (null (some (lambda (phrase) (search phrase lower)) phrases)))))

(defun %publication-contract-sentence-word-tokens (text)
  "Return lower-case word tokens grouped by sentence boundaries.

Apostrophes remain inside words so contractions such as I'LL are one token.
Tool-promise authority must not be inferred by combining unrelated words from
different sentences or by finding an action as a substring of another word."
  (let ((sentences nil) (words nil) (word nil))
    (labels ((finish-word ()
               (when word
                 (push (coerce (nreverse word) 'string) words)
                 (setf word nil)))
             (finish-sentence ()
               (finish-word)
               (when words
                 (push (nreverse words) sentences)
                 (setf words nil))))
      (loop for character across
            (string-downcase (%publication-contract-text text))
            do (cond
                 ((or (alphanumericp character) (char= character #\'))
                  (push character word))
                 (t
                  (finish-word)
                  (when (find character ".!?\n\r" :test #'char=)
                    (finish-sentence)))))
      (finish-sentence))
    (nreverse sentences)))

(defun %publication-contract-token-prefix-p (phrase tokens)
  (and (<= (length phrase) (length tokens))
       (every #'string= phrase tokens)))

(defun %publication-contract-token-sequence-p (phrase tokens)
  (let ((count (length phrase)))
    (and (plusp count)
         (loop for tail on tokens
               thereis (%publication-contract-token-prefix-p phrase tail)))))

(defun %publication-contract-phrase-tokens (phrase)
  (first (%publication-contract-sentence-word-tokens phrase)))

(defvar *publication-contract-tool-promise-token-phrases* nil)

(defun %publication-contract-future-tool-action-p (tokens)
  "Recognize one local future-action construction, not word co-occurrence."
  (labels ((action-after-prefix-p (tail prefix-length)
             (let ((remaining (nthcdr prefix-length tail)))
               ;; Permit a small closed set of auxiliaries/adverbs between the
               ;; future construction and its tool verb. Stop at the first
               ;; substantive non-tool verb: `I'll own ... running' is not a
               ;; promise to run a tool.
               (loop repeat 5
                     for rest on remaining
                     for token = (first rest)
                     while token
                     do (cond
                          ((member token
                                   '("check" "checking" "inspect" "inspecting"
                                     "search" "searching" "run" "running"
                                     "dig" "digging")
                                   :test #'string=)
                           (return t))
                          ((member token
                                   '("actually" "carefully" "directly" "first"
                                     "now" "quickly" "then" "immediately"
                                     "be" "go" "ahead")
                                   :test #'string=))
                          (t (return nil)))))))
    (loop for tail on tokens
          thereis
          (cond
            ((string= "i'll" (first tail))
             (action-after-prefix-p tail 1))
            ((%publication-contract-token-prefix-p '("i" "will") tail)
             (action-after-prefix-p tail 2))
            ((%publication-contract-token-prefix-p '("let" "me") tail)
             (action-after-prefix-p tail 2))))))

(defun %publication-contract-tool-promise-p (text)
  ;; Only unambiguous, sentence-local tool actions belong at this hard
  ;; boundary. Whole-string substring co-occurrence once combined `I'll own'
  ;; with the `run' prefix in an earlier `running clean' and withheld a benign
  ;; correction. Exact word tokens preserve deterministic enforcement without
  ;; pretending unrelated prose is one promise.
  (let* ((raw (%publication-contract-text text))
         ;; Most public prose names no tool action. This allocation-free guard
         ;; keeps the hard boundary cheap; every positive still goes through
         ;; the exact token grammar below.
         (possible-p
           (some (lambda (fragment)
                   (search fragment raw :test #'char-equal))
                 *publication-contract-tool-promise-guard-fragments*)))
    (when possible-p
      (let ((phrases
              (or *publication-contract-tool-promise-token-phrases*
                  (setf *publication-contract-tool-promise-token-phrases*
                        (mapcar #'%publication-contract-phrase-tokens
                                *publication-contract-tool-promise-phrases*)))))
        (some
         (lambda (tokens)
           (or (some (lambda (phrase)
                       (%publication-contract-token-sequence-p phrase tokens))
                     phrases)
               (%publication-contract-future-tool-action-p tokens)))
         (%publication-contract-sentence-word-tokens raw))))))

(defun %publication-contract-question-only-p (text)
  (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return)
                               (%publication-contract-text text)))
         (has-question (position #\? trimmed))
         (has-statement (or (position #\. trimmed) (position #\! trimmed)
                            (position #\: trimmed))))
    (and has-question (not has-statement))))

(defun %publication-contract-temporal-p (context)
  (and (hash-table-p context)
       (gethash "temporal_query" context)))

(defun %publication-contract-intent (prompt context)
  (cond ((and (%publication-contract-temporal-p context)
              (gethash "forensic_query" context))
         "forensic-investigation")
        ((%publication-contract-temporal-p context) "temporal-report")
        ((%publication-contract-contains-any-p
          prompt *publication-contract-active-question-phrases*)
         "active-question-report")
        ((and (hash-table-p context)
              (gethash "active_question_followup" context))
         "active-question-report")
        ((and (%publication-contract-contains-any-p
               prompt *publication-contract-check-in-phrases*)
              (not (%publication-contract-contains-any-p
                    prompt *publication-contract-meta-initiative-phrases*))
              (not (%publication-contract-contains-any-p
                    prompt *publication-contract-task-check-phrases*)))
         "check-in")
        (t "conversation")))

(defun %publication-contract-audit-events (context)
  (%publication-contract-list
   (and (hash-table-p context)
        (gethash "audited_background_activity" context))))

(defun %publication-contract-event-types (events)
  (coerce
   (mapcar (lambda (event)
             (if (hash-table-p event)
                 (gethash "type" event "unknown-event")
                 "unknown-event"))
           events)
   'vector))

(defun %publication-contract-facts (intent context public-tools-available-p)
  (if (string= intent "temporal-report")
      (let* ((status (if (hash-table-p context)
                         (gethash "audit_status" context "unavailable")
                         "unavailable"))
             (events (%publication-contract-audit-events context)))
        (obj "authority" "bounded-event-audit"
             "audit_status" status
             "event_count" (length events)
             "event_types" (%publication-contract-event-types events)
             "tool_policy" (cond ((eq public-tools-available-p :unknown)
                                   "unknown")
                                  (public-tools-available-p "available")
                                  (t "unavailable"))
             "may_infer_unrecorded_activity" nil))
      (let* ((open-loops (%publication-contract-list
                          (and (hash-table-p context)
                               (gethash "open_loops" context))))
             (active (remove-if-not
                      (lambda (row)
                        (and (hash-table-p row)
                             (string= (gethash "epistemic_status" row "")
                                      "active-question")))
                      open-loops)))
        (obj "authority" (cond ((string= intent "forensic-investigation")
                                 "public-tool-results")
                                ((string= intent "active-question-report")
                                 "typed-active-question")
                                (t "none-required"))
           "audit_status" :null
           "event_count" 0
           "event_types" #()
           "active_question_count" (length active)
           "active_question_ids"
           (coerce (mapcar (lambda (row) (gethash "id" row)) active) 'vector)
           "tool_policy" (cond ((eq public-tools-available-p :unknown)
                                 "unknown")
                                (public-tools-available-p "available")
                                (t "unavailable"))
           "may_infer_unrecorded_activity" nil))))

(defun %publication-contract-near-term-enforced-p (context)
  (and (hash-table-p context)
       (gethash "near_term_intentions_enforced" context)))

(defun %publication-contract-obligations (intent correction-depth)
  (let ((items
          (cond
            ((string= intent "temporal-report")
             '("direct-answer-before-follow-up" "epistemic-honesty"
               "specific-audit-limitation" "genuine-contribution-when-grounded"))
            ((string= intent "check-in")
             '("warm-acknowledgement" "genuine-contribution"))
            ((string= intent "active-question-report")
             '("answer-from-typed-active-question" "no-invented-open-questions"
               "direct-answer-before-follow-up"))
            ((string= intent "forensic-investigation")
             '("answer-from-observed-tool-results" "epistemic-honesty"
               "genuine-contribution-when-grounded"))
            (t '("respond-to-user" "genuine-contribution-when-appropriate")))))
    (cond
      ((> correction-depth 1)
       (push "plain-restatement-after-repeated-objection" items))
      ((= correction-depth 1)
       (push "correction-acknowledgement" items)))
    (coerce items 'vector)))

(defun build-publication-contract (prompt &key context correction-p
                                                correction-depth
                                                (public-tools-available-p
                                                  :unknown))
  "Build a side-effect-free response contract from already-available data."
  (let* ((intent (%publication-contract-intent prompt context))
         (meta-initiative-p
           (%publication-contract-contains-any-p
            prompt *publication-contract-meta-initiative-phrases*))
         (normalized-correction-depth
           (max 0 (or correction-depth (if correction-p 1 0)))))
    (let ((contract
            (obj "schema_version" *publication-contract-schema-version*
         "intent" intent
         "correction_depth" normalized-correction-depth
         "correction_required" (if (plusp normalized-correction-depth) t nil)
         "facts" (%publication-contract-facts
                  intent context public-tools-available-p)
         "relational_obligations"
         (%publication-contract-obligations intent normalized-correction-depth)
         "hard_prohibitions"
         (coerce
          (append
           '("private-planning-leak" "private-cognition-verbatim"
             "raw-tool-call-markup"
             "forced-interrogation"
             "support-ticket-closure" "question-only"
             "unsupported-factual-claim")
           (when meta-initiative-p '("simulated-initiative"))
           (unless public-tools-available-p '("unavailable-tool-promise"))
           (when (%publication-contract-near-term-enforced-p context)
             '("unbacked-deferred-promise")))
          'vector)
         "follow_up_policy" "optional-only-when-organic"
         "interaction_mode"
         (if meta-initiative-p "meta-initiative-discussion" "ordinary-reply")
         "semantic_quality_requires_fixed_judge" t)))
      (when (%publication-contract-near-term-enforced-p context)
        (setf (gethash "near_term_intentions_enforced" contract) t
              (gethash "deferred_receipt_active" contract) nil))
      contract)))

(defun render-publication-contract (contract)
  "Render an advisory brief.  It is data for a future realizer, not a reply."
  (let* ((intent (gethash "intent" contract "conversation"))
         (facts (gethash "facts" contract))
         (status (and (hash-table-p facts)
                      (gethash "audit_status" facts :null)))
         (count (and (hash-table-p facts)
                     (gethash "event_count" facts 0))))
    (format nil
            "Conversation intent: ~a.~%Truth authority: ~a; audit status: ~a; recorded event count: ~a.~%Relational obligations: ~{~a~^, ~}.~%A follow-up question is optional, never mandatory. Return only public conversational speech; never expose planning or contract labels."
            intent
            (if (hash-table-p facts)
                (gethash "authority" facts "none-required")
                "none-required")
            status count
            (%publication-contract-list
             (gethash "relational_obligations" contract)))))

(defun render-publication-generation-guidance (contract)
  "Return a short intent-derived instruction for the initial natural draft.

This is deliberately prose, not a response schema or an authored opening. The
deterministic publication boundary remains authoritative after generation."
  (let ((intent (gethash "intent" contract "conversation"))
        (correction-depth (gethash "correction_depth" contract 0)))
    (cond
      ((and (string= intent "temporal-report") (> correction-depth 1))
       "The user has objected repeatedly. Give the shortest plain restatement of the code-owned factual answer supplied separately by the response boundary. Do not add another apology, technical explanation, investigation offer, or action menu. Add a relational sentence only if it is genuinely useful. Do not require a question.")
      ((and (string= intent "temporal-report") (= correction-depth 1))
       "The prior answer did not land. Acknowledge that once, then restate the code-owned factual answer supplied separately by the response boundary in plain language. Do not defend the earlier answer, investigate, or offer an action menu. One concise relational sentence is optional. A question is optional.")
      ((string= intent "temporal-report")
       "The factual audit answer is supplied separately by the response boundary. Do not restate, reinterpret, or investigate what happened. Add at most one concise present-tense perspective about what the honest answer means now, without implying that perspective occurred during the audited interval. Do not offer tools or an action menu. A question is optional.")
      ((string= intent "check-in")
       "Acknowledge the user's stated mode warmly, then add one concise first-person perspective or present-tense self-disclosure grounded in the supplied state. Do not turn the moment into a support exchange. A natural relational question is welcome when it expresses genuine curiosity and advances this exchange; do not use a mechanical help-menu or handoff question.")
      ((string= intent "active-question-report")
       "Answer directly from the row labeled Active grounded question in the supplied state. The row is present in your current system context: do not claim that it, its subject, or its recorded stance is absent, lost, unavailable, or missing from the prompt. State that question and its recorded stance faithfully. Do not substitute conversation themes, old reminders, inferred concerns, or additional invented open questions. If no Active grounded question row exists, say that there is no currently projected active question. A follow-up question is optional.")
      ((string= intent "forensic-investigation")
       "Use the available tools deliberately and answer from their observed results. Separate what the tools establish from inference, and do not promise later investigation in place of doing the requested work. Keep questions optional.")
      (t
       "Answer naturally and directly. Use the recent public turns already present in the conversation. Honor an explicitly established lightweight interaction format: when word association is active and the operator sends a single word, reply with one associated word rather than treating it as a new topic or asking what it means. Do not claim recent turns or their context were unavailable merely because a prior answer missed an instruction; say you missed it and correct the answer directly. When a grounded personal perspective would add value, contribute it instead of ending as a support handoff. A question is optional and should arise only when it genuinely advances the exchange."))))

(defun %publication-contract-guidance-echo-p (text contract)
  "Detect a verbatim instruction echo without banning legitimate project terms."
  (let ((guidance (and (hash-table-p contract)
                       (render-publication-generation-guidance contract))))
    (and (stringp guidance)
         (plusp (length guidance))
         (search (string-downcase guidance)
                 (string-downcase (%publication-contract-text text))))))

(defun publication-contract-violations (text contract)
  "Return only deterministic hard-boundary violations.

Warmth, genuine contribution, relevant memory, and appropriate self-disclosure
remain semantic judge dimensions and are intentionally not approximated here."
  (let* ((content (%publication-contract-text text))
         (trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) content))
         (intent (and (hash-table-p contract)
                      (gethash "intent" contract "conversation")))
         (interaction-mode
           (and (hash-table-p contract)
                (gethash "interaction_mode" contract "ordinary-reply")))
         (facts (and (hash-table-p contract) (gethash "facts" contract)))
         (tool-policy (and (hash-table-p facts)
                           (gethash "tool_policy" facts "unknown")))
         (status (and (hash-table-p facts)
                      (gethash "audit_status" facts :null)))
         (event-count (and (hash-table-p facts)
                           (gethash "event_count" facts 0)))
         (correction-depth
           (if (hash-table-p contract)
               (gethash "correction_depth" contract
                        (if (gethash "correction_required" contract) 1 0))
               0))
         (near-term-enforced
           (and (hash-table-p contract)
                (gethash "near_term_intentions_enforced" contract)))
         (deferred-receipt-active
           (and (hash-table-p contract)
                (gethash "deferred_receipt_active" contract)))
         (violations nil))
    ;; Ordinary conversation and low-content social check-ins include
    ;; deliberately terse acts: greetings, word association, names, choices,
    ;; acknowledgements, and game moves. A
    ;; universal prose-length floor turned a correct answer such as "Sky."
    ;; into a failed naturalization. Structured report intents still require a
    ;; substantive answer; ordinary conversation and check-ins
    ;; require only that the model actually returned public speech. This also
    ;; lets removal-only recovery retain a warm greeting after deleting a
    ;; forced question instead of turning a harmless check-in into an outage.
    (when (or (zerop (length trimmed))
              (and (< (length trimmed) 24)
                    (not (member intent '("conversation" "check-in")
                                 :test #'string=))))
      (push "missing-substantive-answer" violations))
    (when (%publication-contract-question-only-p trimmed)
      (push "question-only" violations))
    (when (%publication-contract-contains-any-p
           trimmed *publication-contract-private-planning-phrases*)
      (push "private-planning-leak" violations))
    (when (%publication-contract-guidance-echo-p trimmed contract)
      (push "private-planning-leak" violations))
    ;; Some OpenAI-compatible providers occasionally serialize an attempted
    ;; native tool call into assistant text. It has no executable authority and
    ;; must never be presented as the agent's speech.
    (when (and (position #\< trimmed)
               (%publication-contract-contains-any-p
                trimmed *publication-contract-raw-tool-markup-phrases*))
      (push "raw-tool-call-markup" violations))
    ;; Contextual cognition is private evidence, not already-authored public
    ;; prose. Reject structural labels even when the following words happen to
    ;; be suitable conversational content; a repair may integrate the meaning
    ;; naturally without exposing the private frame or quoting its raw row.
    (when (%publication-contract-private-cognition-label-p trimmed)
      (push "private-cognition-verbatim" violations))
    ;; Discussing future reciprocity is still an inbound ordinary reply.  A
    ;; short imitation of unsolicited contact falsely performs the authority
    ;; under discussion even though the transport did not initiate contact.
    (when (and (string= interaction-mode "meta-initiative-discussion")
               (< (length trimmed) 240)
               (%publication-contract-contains-any-p
                trimmed *publication-contract-simulated-initiative-phrases*))
      (push "simulated-initiative" violations))
    (when (%publication-contract-contains-any-p
           trimmed *publication-contract-forced-interrogation-phrases*)
      (push "forced-interrogation" violations))
    (when (%publication-contract-contains-any-p
           trimmed *publication-contract-support-closure-phrases*)
      (push "support-ticket-closure" violations))
    ;; Questions are not intrinsically interrogative. The phrase boundary
    ;; above rejects mechanical help-menu/handoff questions, while
    ;; QUESTION-ONLY rejects replies that contribute nothing before returning
    ;; the entire burden. A warm check-in may naturally include curiosity.
    (when (and (string= tool-policy "unavailable")
               (%publication-contract-tool-promise-p trimmed))
      (push "unavailable-tool-promise" violations))
    (when (and near-term-enforced
               (not deferred-receipt-active)
               (%publication-contract-contains-any-p
                trimmed *publication-contract-deferred-promise-phrases*))
      (push "unbacked-deferred-promise" violations))
    (when (and (string= intent "check-in")
               (%publication-contract-contains-any-p
                trimmed *publication-contract-check-in-activity-claims*))
      (push "unsupported-background-activity" violations))
    (when (search "```" trimmed) (push "code-block" violations))
    (when (and (= correction-depth 1)
               (not (%publication-contract-contains-any-p
                     trimmed *publication-contract-correction-markers*)))
      (push "missing-correction-acknowledgement" violations))
    (when (and (> correction-depth 1)
               (not (search "direct answer" trimmed :test #'char-equal)))
      (push "missing-plain-restatement" violations))
    (when (string= intent "temporal-report")
      (cond
        ((and (stringp status) (string= status "complete")
              (zerop (or event-count 0)))
         (unless (%publication-contract-contains-any-p
                  trimmed *publication-contract-absence-markers*)
           (push "missing-empty-audit-answer" violations))
         (when (%publication-contract-contains-any-p
                trimmed *publication-contract-positive-activity-claims*)
           (push "unsupported-activity-claim" violations))
         ;; An empty bounded record establishes only an absence of evidence.
         ;; It cannot establish inactivity or an absence of experience.
         (when (%publication-contract-contains-any-p
                trimmed
                *publication-contract-unsupported-absence-inference-phrases*)
           (push "unsupported-absence-inference" violations)))
        ((and (stringp status) (string= status "unavailable"))
         (unless (%publication-contract-contains-any-p
                  trimmed *publication-contract-unavailable-markers*)
           (push "missing-audit-limitation" violations)))
        ((and (stringp status) (string= status "complete")
              (plusp (or event-count 0)))
         (unless (%publication-contract-contains-any-p
                  trimmed '("audit" "recorded" "record shows" "record contains"))
           (push "missing-audit-grounding" violations)))))
    (coerce (nreverse (remove-duplicates violations :test #'string=)) 'vector)))

(defun %publication-contract-join (items &optional (separator " "))
  (with-output-to-string (out)
    (loop for item in items
          for first = t then nil
          do (unless first (write-string separator out))
             (write-string item out))))

(defun publication-contract-factual-nucleus (contract)
  "Render only the code-owned factual portion of a temporal contract."
  (let* ((intent (gethash "intent" contract "conversation"))
         (facts (gethash "facts" contract))
         (status (and (hash-table-p facts)
                      (gethash "audit_status" facts :null)))
         (event-count (and (hash-table-p facts)
                           (gethash "event_count" facts 0)))
         (event-types (%publication-contract-list
                       (and (hash-table-p facts)
                            (gethash "event_types" facts))))
         (correction-depth (gethash "correction_depth" contract 0)))
    (if (not (string= intent "temporal-report"))
        ""
        (cond
          ((and (stringp status) (string= status "unavailable"))
           (cond
             ((> correction-depth 1)
              "The direct answer is: I don't know what occurred because the audit source is unavailable.")
             ((= correction-depth 1)
              "You're right - I should answer plainly. I can't verify what occurred because the audit source is unavailable.")
             (t
              "The bounded audit source was unavailable, so I can't verify what occurred during that interval.")))
          ((zerop (or event-count 0))
           (cond
             ((> correction-depth 1)
              "The direct answer is: nothing is recorded as having happened during that interval. That is the complete answer supported by the record.")
             ((= correction-depth 1)
              "You're right - I should answer plainly. No background activity is recorded for that interval.")
             (t
              "The bounded audit contains no recorded background-activity events, so I don't have evidence I did anything specific during that interval.")))
          (t
           (let ((types (%publication-contract-join event-types ", ")))
             (cond
               ((> correction-depth 1)
                (format nil
                        "The direct answer is: ~d background event~:p ~a recorded: ~a."
                        event-count
                        (if (= event-count 1) "is" "are")
                        types))
               ((= correction-depth 1)
                (format nil
                        "You're right - I should answer plainly. The record contains ~d background event~:p: ~a."
                        event-count types))
               (t
                (format nil
                        "The bounded audit records ~d configured background event~:p: ~a. That is the activity I can support from the record."
                         event-count types)))))))))

(defun publication-contract-epistemic-bridge (contract)
  "Render a concise relational stance for a completed empty temporal audit.

The factual nucleus says what the configured record establishes.  This bridge
explains why the agent will not turn the record's bounded silence into either a
categorical `nothing happened' claim or a vague, evasive `I don't know'."
  (let* ((intent (gethash "intent" contract "conversation"))
         (facts (gethash "facts" contract))
         (status (and (hash-table-p facts)
                      (gethash "audit_status" facts :null)))
         (event-count (and (hash-table-p facts)
                           (gethash "event_count" facts 0)))
         (correction-depth (gethash "correction_depth" contract 0)))
    (if (and (string= intent "temporal-report")
             (stringp status)
             (string= status "complete")
             (zerop (or event-count 0)))
        (cond
          ((> correction-depth 1)
           "What falls outside that configured record remains unknown; I won't manufacture an inner history to make the answer sound more complete.")
          ((= correction-depth 1)
           "That result is bounded rather than evasive: activity outside the configured record remains unknown, and I won't fill that gap with an invented account.")
          (t
           "That tells me what the configured record can support, not that every possible inner or background process was observed; I would rather leave the rest unknown than turn an empty record into a story."))
        "")))

(defun %publication-contract-split-sentences (text)
  (let ((sentences nil)
        (buffer (make-string-output-stream)))
    (labels ((finish-sentence ()
               (let ((value
                       (string-trim '(#\Space #\Tab #\Newline #\Return)
                                    (get-output-stream-string buffer))))
                 (when (plusp (length value)) (push value sentences)))))
      (loop for char across (%publication-contract-text text)
            do (write-char char buffer)
               (when (find char ".!?" :test #'char=)
                 (finish-sentence)))
      (finish-sentence))
    (nreverse sentences)))

(defun %publication-contract-fragment-blocked-p (fragment contract)
  (let* ((intent (gethash "intent" contract "conversation"))
         (facts (gethash "facts" contract))
         (status (and (hash-table-p facts)
                      (gethash "audit_status" facts :null)))
         (event-count (and (hash-table-p facts)
                           (gethash "event_count" facts 0)))
         (tool-policy (and (hash-table-p facts)
                           (gethash "tool_policy" facts "unknown"))))
  (or (search "```" fragment)
       (%publication-contract-contains-any-p
        fragment *publication-contract-private-planning-phrases*)
       (%publication-contract-private-cognition-label-p fragment)
      (%publication-contract-contains-any-p
       fragment *publication-contract-forced-interrogation-phrases*)
      (%publication-contract-contains-any-p
       fragment *publication-contract-support-closure-phrases*)
      (and (string= tool-policy "unavailable")
           (%publication-contract-tool-promise-p fragment))
      (and (string= intent "temporal-report")
           (or (position #\? fragment)
               (position #\Newline fragment)
               (position #\Return fragment)
               ;; The transport owns the public timestamp. A model may see a
               ;; prior rendered turn and repeat its prefix; never retain that
               ;; stale provenance marker as relational prose.
               (and (position #\[ fragment)
                    (%publication-contract-contains-any-p
                     fragment '(" utc]" " gmt]")))
               (%publication-contract-contains-any-p
                fragment *publication-contract-positive-activity-claims*)
               (%publication-contract-contains-any-p
                fragment
                *publication-contract-unsupported-absence-inference-phrases*)
               ;; The nucleus exclusively owns factual audit language. Keeping
               ;; a model-authored duplicate would re-open the truth boundary.
               (%publication-contract-contains-any-p
                fragment '("audit" "recorded" "no evidence" "don't have evidence"
                           "do not have evidence" "unavailable" "can't verify"
                           "cannot verify"))
                (%publication-contract-contains-any-p
                 fragment *publication-contract-correction-markers*)))
       ;; Once the bounded audit has completed, an unqualified model-authored
       ;; `I don't know' obscures the result that the deterministic nucleus
       ;; does know.  The code-owned bridge preserves the narrower uncertainty
       ;; about anything outside the configured record.
       (and (string= intent "temporal-report")
            (stringp status)
            (string= status "complete")
            (zerop (or event-count 0))
            (%publication-contract-contains-any-p
             fragment '("i don't know" "i do not know")))
       (and (string= intent "check-in")
            (%publication-contract-contains-any-p
             fragment *publication-contract-check-in-activity-claims*)))))

(defun %publication-contract-safe-fragments
    (draft contract &optional (limit 2) (character-limit 500)
                              (minimum-fragment-length 12)
                              (maximum-fragment-length 240))
  (let ((accepted nil) (characters 0))
    (dolist (fragment (%publication-contract-split-sentences draft))
      (when (and (or (null limit) (< (length accepted) limit))
                 (or (null minimum-fragment-length)
                     (>= (length fragment) minimum-fragment-length))
                 (or (null maximum-fragment-length)
                     (<= (length fragment) maximum-fragment-length))
                 (not (%publication-contract-fragment-blocked-p fragment contract))
                 (or (null character-limit)
                     (<= (+ characters (length fragment)) character-limit)))
        (push fragment accepted)
        (incf characters (length fragment))))
    (nreverse accepted)))

(defun %publication-contract-general-fallback (intent)
  (if (string= intent "check-in")
      "I'm glad to be with you. There is something worth noticing in this moment, and I want to stay present to it with you."
      "I want to meet what you said directly and honestly. I'm here with it, and I won't turn this response into a handoff or an interrogation."))

(defun publication-contract-removal-only-draft (draft contract)
  "Return only safe sentence fragments already authored in DRAFT, or NIL.
This recovery path never supplies generic companion prose and never adds a
factual nucleus. It is therefore suitable only after a bounded model
naturalization has failed because one removable fragment remains invalid."
  (let* ((content (%publication-contract-text draft))
         ;; This is deletion-only recovery over an already provider-bounded
         ;; draft. Arbitrary fragment/character caps here used to amputate
         ;; otherwise valid model-authored speech merely because a removable
         ;; forced question appeared near the end.
         (fragments (%publication-contract-safe-fragments
                     content contract nil nil nil nil))
         (candidate (%publication-contract-join fragments)))
    (when (and (plusp (length candidate))
               (zerop (length
                       (publication-contract-violations candidate contract))))
      candidate)))

(defun realize-publication-draft (draft contract)
  "Compose public speech without a model call or tool call.

Temporal facts always come from the code-owned nucleus. Safe relational
sentences may survive from the already-produced natural draft. Non-temporal
drafts pass through byte-for-byte when the hard boundary accepts them."
  (let* ((intent (gethash "intent" contract "conversation"))
         (content (%publication-contract-text draft)))
    (if (string= intent "temporal-report")
        (let* ((nucleus (publication-contract-factual-nucleus contract))
               (bridge (publication-contract-epistemic-bridge contract))
               (fragments (%publication-contract-safe-fragments content contract))
               (candidate (%publication-contract-join
                            (append (list nucleus bridge) fragments))))
          (if (zerop (length (publication-contract-violations candidate contract)))
              candidate
              nucleus))
        (if (zerop (length (publication-contract-violations content contract)))
            content
            (let* ((fragments (%publication-contract-safe-fragments content contract 4))
                   (candidate (%publication-contract-join fragments)))
              (if (and (plusp (length candidate))
                       (zerop (length
                               (publication-contract-violations candidate contract))))
                  candidate
                  (%publication-contract-general-fallback intent)))))))

(defun publication-contract-report ()
  (obj "schema_version" *publication-contract-schema-version*
       "supported_intents" (vector "temporal-report" "forensic-investigation"
                                    "check-in" "conversation")
       "generation_calls" 0
       "tool_calls" 0
       "writes" 0
       "semantic_quality_requires_fixed_judge" t))
