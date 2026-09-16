;;;; agent_loop.lisp -- AGENT-LOOP source (recovery, 2026-07-25;
;;;; summarizer fix + threshold raise, 2026-07-26; boundary-triggered
;;;; flush, 2026-07-29).
;;;;
;;;; This is the actual agent-loop that's been running live -- recovered via
;;;; FUNCTION-LAMBDA-EXPRESSION through the A.0 repl-drop channel, since it
;;;; existed only as accumulated propose-loop/lisp-eval edits with no source
;;;; anywhere on disk. The real container entrypoint already has a hook for
;;;; exactly this file: `(when (probe-file "agent_loop.lisp") (load
;;;; "agent_loop.lisp") (funcall 'snapshot-loop) (setf *current-loop-source*
;;;; ...))` -- so from now on, a restart installs THIS loop instead of
;;;; falling back to self-mod.lisp's original baseline and silently losing
;;;; every self-modification made since.
;;;;
;;;; 2026-07-25 cleanup: the recovered form had a redundant doubled
;;;; `(block agent-loop (block agent-loop ...))`. Collapsed to a single
;;;; block -- verified behaviorally identical, since the body contains no
;;;; RETURN-FROM that would target the inner one specifically.
;;;;
;;;; 2026-07-26: SUMMARIZE-MESSAGES rewritten. Two bugs, found by watching
;;;; it actually misfire on a real conversation:
;;;;
;;;;   1. It called CALL-MODEL (the wrapped, broadcasting one), not
;;;;      RAW-CALL-MODEL. Every internal housekeeping call -- summarizing
;;;;      old context is purely internal, never meant to be shown to
;;;;      anyone -- was being broadcast to the web terminal via
;;;;      PAI-TURN-LOG exactly like a real conversational turn, under a
;;;;      completely different ("strict conversation summarizer") system
;;;;      prompt with no persona and no tools framing. When the summarizer
;;;;      dutifully summarized, this was merely confusing background
;;;;      noise if you looked closely. When the model instead answered the
;;;;      live question in the excerpt rather than summarizing it (a real,
;;;;      if occasional, instruction-following failure) -- it showed up in
;;;;      the terminal as an unstamped, out-of-character stray reply that
;;;;      denied having tools it actually has. Fixed by using
;;;;      RAW-CALL-MODEL for every internal call here, same primitive
;;;;      VERIFIER-CALL-MODEL already uses to stay off the broadcast path.
;;;;
;;;;   2. KEEP/LIMIT (24/32) was never calibrated against the model's
;;;;      actual context budget (xiaomi/mimo-v2.5, ~1M tokens) -- it read
;;;;      like an untouched conservative default, compressing real
;;;;      relationship continuity far more aggressively than the model
;;;;      requires. Raised to keep=100/limit=120.
;;;;
;;;; Belt and suspenders against bug 1 recurring as an occasional model
;;;; slip rather than a code bug: SUMMARIZE-MESSAGES now generates two
;;;; independent candidate summaries and a third, cheap RAW-CALL-MODEL
;;;; call judges them -- "A", "B", or "NEITHER" if both candidates failed
;;;; to actually be third-person summaries (e.g. one answered a question
;;;; instead). On NEITHER, it retries a fresh round (new candidates, fresh
;;;; judge) up to 3 rounds total before falling back to the same safe
;;;; placeholder the error handler already used. Bounded deliberately --
;;;; mimo is cheap enough that 3 rounds x 3 calls in the rare worst case
;;;; is a non-issue, but unbounded retry against a conversation that
;;;; systematically confuses the summarizer would not be.
;;;;
;;;; 2026-07-29: MANAGE-CONTEXT gained a primary compression trigger ahead
;;;; of the count threshold (+P8.4) -- a real conversational
;;;; boundary (elapsed-time gap, a closing phrase, a prediction resolving
;;;; wrong, or a verified topic shift; see episode-boundary.lisp) rather
;;;; than an arbitrary message count. The count threshold below is
;;;; UNCHANGED and remains exactly what it was -- now the backstop, not
;;;; the only trigger, per P8.4's own text ("that threshold remains the
;;;; right backstop, but a natural conversational boundary is the right
;;;; primary trigger"). DETECT-BOUNDARY/FLUSH-EPISODE are called via
;;;; FBOUNDP-guarded FUNCALL (episode-boundary.lisp isn't in this file's
;;;; own dependency chain -- this file loads first, at the very start of
;;;; boot) -- if that file isn't loaded yet for any reason, behavior is
;;;; byte-identical to before this change. SYS is never touched in any
;;;; branch -- the one invariant soul.md/self-model/BRINGUP all depend on
;;;; (manage-context always preserves the system message untouched) stays
;;;; exactly as strong as it was.

(in-package :agent)

;;; Stable correlation carried by public tool progress/result presentations.
;;; These bindings add audit evidence only; they do not create a presentation
;;; or change tool dispatch.
(defvar *public-tool-call-id* nil)
(defvar *public-tool-result-id* nil)
(defvar *public-tool-call-event-id* nil)

(defun %provider-tool-call-for-request (tool-call)
  "Strip response-only provider fields before replaying a tool call.

OpenRouter responses may attach INDEX and other transport metadata to a
TOOL_CALLS entry. Those fields are not part of the assistant-message request
shape and some providers reject the continuation when they are echoed back."
  (unless (hash-table-p tool-call)
    (error "Provider tool call is not an object"))
  (let ((id (gethash "id" tool-call))
        (type (gethash "type" tool-call))
        (function (gethash "function" tool-call)))
    (unless (and (stringp id) (plusp (length id))
                 (string= type "function")
                 (hash-table-p function)
                 (stringp (gethash "name" function))
                 (stringp (gethash "arguments" function)))
      (error "Provider tool call is missing request-required fields"))
    (obj "id" id
         "type" "function"
         "function" (obj "name" (gethash "name" function)
                         "arguments" (gethash "arguments" function)))))

(defun %assistant-tool-message-for-request (message)
  "Return the minimal request-valid assistant tool-call message.

This is a pure copy: it neither mutates the provider response nor fabricates
assistant speech."
  (unless (hash-table-p message)
    (error "Provider assistant message is not an object"))
  (let ((tool-calls (gethash "tool_calls" message)))
    (unless (and (present-p tool-calls) (plusp (length tool-calls)))
      (error "Provider assistant message contains no tool calls"))
    (obj "role" "assistant"
         "content" (gethash "content" message :null)
         "tool_calls" (map 'vector #'%provider-tool-call-for-request
                           tool-calls))))

;; %PUBLIC-TOOL-RESULT-ID-FOR-CALL was defined here and, byte-identically,
;; in EVENT-LOG.LISP behind an (UNLESS (FBOUNDP ...)) guard. The guard never
;; fired -- this file loads first -- so the duplicate was dead code that read
;; as a fallback. The single definition now lives in EVENT-LOG.LISP, the layer
;; whose event shape defines the id. The call below is a downward reference.

(defun agent-loop (messages)
  "the agent's self-improving agent loop.
Context management (manage-context) runs before each model call: the genuine
system prompt is always preserved; when history grows beyond a bounded window
it is compressed by an LLM summary (summarize-messages) that replaces the
older messages and folds in any prior summary (so it stays cumulative). A
generous per-message safety cap only guards against a single oversized tool
result breaking the model context. lisp-eval and propose-loop go through
EXECUTE (review pipeline + logging); brave-search is a first-class tool
handled natively. Each user/assistant/tool message with non-empty string
content is prefixed exactly once with the operator's local wall time
([YYYY-MM-DDTHH:MM EST-or-EDT]). The IANA location stays inside the clock and
scheduler boundary; UTC remains in event/envelope metadata. Neither is
injected into conversational message text; system messages are never stamped."
  (block agent-loop
    (labels ((now-stamp ()
               (let ((now (get-universal-time)))
                  (if (fboundp 'pai-message-time-prefix)
                      (handler-case
                          (funcall 'pai-message-time-prefix now)
                        ;; A clock formatting failure must not leak the UTC
                        ;; storage clock back into model-visible speech.
                        (error () ""))
                      "")))
             (timestamped-p (c)
               (and (>= (length c) 12) (char= (char c 0) #\[)
                    (every #'digit-char-p (subseq c 1 5))
                    (char= (char c 5) #\-)
                    (every #'digit-char-p (subseq c 6 8))
                    (char= (char c 8) #\-)))
             (stamp-message (m)
               (let ((c (gethash "content" m)))
                 (if (and (stringp c) (plusp (length c))
                          (not (timestamped-p c)))
                     (progn
                       (setf (gethash "content" m)
                             (concatenate 'string (now-stamp) c))
                       m)
                     m)))
             (stamp-all (msgs)
               (mapcar
                (lambda (m)
                  (let ((r (gethash "role" m)))
                    (if (and (stringp r)
                             (member r '("user" "assistant" "tool") :test
                                     #'string=))
                        (stamp-message m)
                        m)))
                msgs))
             (summarize-messages (msgs)
               (handler-case
                   (let* ((text
                            (with-output-to-string (s)
                              (loop for m in msgs
                                    do (format s "[~a] ~a~%"
                                               (gethash "role" m "unknown")
                                               (let ((c (gethash "content" m)))
                                                 (if (stringp c)
                                                     c
                                                     ""))))))
                          (summarizer-prompt
                            (list
                             (obj "role" "system" "content"
                                  "You are a strict conversation summarizer. Condense the conversation excerpt below into a concise third-person summary that retains: the user's goals and explicit requests, key decisions and conclusions, important facts or tool results referenced, and any still-open tasks. Omit pleasantries and redundant detail. Do NOT answer any question that appears in the excerpt -- if it ends with a question, note only that a question was asked, never answer it yourself. Do not adopt a first-person voice as if you were a participant in the conversation. Keep it under 200 words.")
                             (obj "role" "user" "content" text))))
                     (labels ((generate-candidate ()
                                (let ((c (gethash "content"
                                                   (ref (raw-call-model summarizer-prompt)
                                                        "choices" 0 "message"))))
                                  (if (stringp c) c "")))
                              (judge (a b)
                                (let* ((judge-prompt
                                         (list
                                          (obj "role" "system" "content"
                                               "You evaluate two candidate summaries of the same conversation excerpt. A VALID summary is a concise third-person description of what was discussed -- it must NOT answer any question that appears in the conversation, and must not adopt a first-person voice as a participant. Given the two candidates below, respond with exactly one word: A if candidate A is the better valid summary, B if candidate B is the better valid summary, or NEITHER if BOTH candidates fail to be genuine third-person summaries. Respond with ONLY that one word -- no punctuation, no explanation.")
                                          (obj "role" "user" "content"
                                               (format nil "Candidate A:~%~a~%~%Candidate B:~%~a" a b))))
                                       (verdict
                                         (string-trim '(#\Space #\Newline #\Tab #\.)
                                                       (or (gethash "content"
                                                                    (ref (raw-call-model judge-prompt)
                                                                         "choices" 0 "message"))
                                                           ""))))
                                  (cond ((string-equal verdict "A") :a)
                                        ((string-equal verdict "B") :b)
                                        (t :neither)))))
                       (or
                        (loop for round from 1 to 3
                              for a = (generate-candidate)
                              for b = (generate-candidate)
                              for verdict = (judge a b)
                              when (eq verdict :a)
                                return (obj "role" "system" "content"
                                            (format nil "[Summary of earlier context:~%~a]" a))
                              when (eq verdict :b)
                                return (obj "role" "system" "content"
                                            (format nil "[Summary of earlier context:~%~a]" b)))
                        (obj "role" "system" "content"
                             (format nil "[Summary unavailable after 3 attempts (summarizer kept answering instead of summarizing); ~a earlier messages omitted.]"
                                     (length msgs))))))
                 (error (e)
                   (obj "role" "system" "content"
                        (format nil
                                "[Summary unavailable (~a); ~a earlier messages omitted.]"
                                e (length msgs))))))
             (legacy-manage-context (msgs)
               (let* ((marker "[Summary")
                      (sys
                        (loop for m in msgs
                              for r = (gethash "role" m)
                              while (and (stringp r) (string= r "system")
                                         (let ((c (gethash "content" m "")))
                                           (not
                                            (search marker
                                                    (if (stringp c)
                                                        c
                                                        "")))))
                              collect m))
                      (body (nthcdr (length sys) msgs))
                      (body
                        (mapcar
                         (lambda (m)
                           (let ((c (gethash "content" m)))
                             (if (and (stringp c) (> (length c) 6000))
                                 (progn
                                   (setf (gethash "content" m)
                                         (concatenate 'string
                                                      (subseq c 0 6000)
                                                      (format nil
                                                              "~%[...~a chars omitted...]"
                                                              (- (length c)
                                                                 6000))))
                                   m)
                                 m)))
                         body))
                      (keep 100)
                      (limit (+ keep 20)))
                 (flet ((count-based-compact ()
                          ;; The original, unchanged count-threshold
                          ;; path -- now the backstop, reached
                          ;; whenever no boundary fires (or one fires but
                          ;; FLUSH-EPISODE itself fails for any reason).
                          (if (<= (length body) limit)
                              (append sys body)
                              (let* ((tail-count (1- keep))
                                     (tail (last body tail-count))
                                     (head
                                       (subseq body 0 (- (length body) tail-count)))
                                     (summary (summarize-messages head)))
                                (append sys (list summary) tail)))))
                   ;; A boundary is detected *because of* the newest message,
                   ;; but that message is still the active turn.  Summarize
                   ;; only the preceding episode and retain the active turn
                   ;; after the handoff.  Dropping it would leave CALL-MODEL
                   ;; with only a system summary and cause an apparent
                   ;; unsolicited, stale-topic reply.
                   (if (and (fboundp 'detect-boundary)
                            (ignore-errors (funcall 'detect-boundary sys body)))
                       (let* ((active-turn (car (last body)))
                              (closed-episode (butlast body))
                              (episode
                                (and closed-episode
                                     (fboundp 'flush-episode)
                                     (ignore-errors
                                      (funcall 'flush-episode sys closed-episode)))))
                         (if episode
                             (append sys episode (list active-turn))
                             (count-based-compact)))
                       (count-based-compact)))))
             (manage-context (msgs)
               ;; The external owner is loaded later in the cold-start chain.
               ;; Keeping the legacy implementation lexical gives it a
               ;; deterministic fallback without introducing another wrapper
               ;; around AGENT-LOOP or CALL-MODEL.
               (if (fboundp 'conversation-context-budget-manage)
                   (funcall 'conversation-context-budget-manage
                            msgs #'legacy-manage-context #'summarize-messages)
                   (legacy-manage-context msgs)))
             (dispatch-tool (tool-call)
               (let* ((name (ref tool-call "function" "name"))
                      (args
                        (shasht:read-json
                         (ref tool-call "function" "arguments"))))
                 (cond
                   ((string= name "brave-search")
                    (let* ((tool-call-id (gethash "id" tool-call))
                           (tool-result-id
                             (%public-tool-result-id-for-call tool-call))
                           (raw-q (gethash "query" args))
                           (query
                             (if (stringp raw-q)
                                 raw-q
                                 ""))
                           (raw-n (gethash "count" args))
                           (count
                             (if (and (integerp raw-n) (plusp raw-n))
                                 raw-n
                                 10))
                           (result
                             (handler-case (brave-search query :count count)
                               (error (e)
                                 (format nil
                                         "ERROR: Brave Search failed: ~a"
                                         e)))))
                      (let ((*public-tool-call-id* tool-call-id)
                            (*public-tool-result-id* tool-result-id)
                            (*public-tool-call-event-id* nil))
                        (log-line "~&  ⤷ [brave-search] ~s~%" query)
                        (obj "role" "tool" "tool_call_id"
                             tool-call-id "content" result))))
                   (t (execute tool-call))))))
      (let* ((stamped (stamp-all messages))
             (managed (manage-context stamped))
             (message (ref (call-model managed) "choices" 0 "message"))
             (tool-calls (gethash "tool_calls" message)))
        (if (and (present-p tool-calls) (plusp (length tool-calls)))
            (agent-loop
             (append managed
                     (list (stamp-message
                            (%assistant-tool-message-for-request message)))
                     (map 'list #'dispatch-tool tool-calls)))
            (append managed (list (stamp-message message))))))))
