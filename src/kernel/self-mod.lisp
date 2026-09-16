;;;; self-mod.lisp — a recursive agent that may rewrite its own loop,
;;;;                  but only under review.
;;;;
;;;; Loads ON TOP of agent.lisp. Adds three things the base agent lacks:
;;;;
;;;;   1. A call BUDGET enforced inside call-model — the one primitive the
;;;;      agent is told never to touch. This is the real termination
;;;;      guarantee: whatever the loop becomes, it runs out of turns.
;;;;   2. SNAPSHOT + ROLLBACK of agent-loop's definition, so a bad rewrite
;;;;      is reversible instead of a brick.
;;;;   3. A PROPOSER/VERIFIER split. The agent doesn't install a new loop
;;;;      directly; it *proposes* one. Static checks run first, then a
;;;;      second model call — a different mind with only the constitution
;;;;      and the diff — votes approve/reject. Only approval installs.
;;;;
;;;; Usage:
;;;;   sbcl --load agent.lisp --load self-mod.lisp \
;;;;        --eval '(agent:run-self-mod "Improve your loop to retry a failed tool once, then explain what you changed.")'
;;;;
;;;; run-self-mod always starts a FRESH conversation — deliberate, so a bad
;;;; rewrite can't silently carry into your next session via memory. If a
;;;; run ends mid-thought (proposal budget exhausted, etc.) and you want it
;;;; to keep going with the SAME history, use:
;;;;   (agent:continue-self-mod "You can try again")
;;;;
;;;; Every turn's assistant text and tool results are also appended to
;;;; self-mod-transcript.log (override with AGENT_TRANSCRIPT), so a run
;;;; longer than your terminal's scrollback is never lost.

(in-package :agent)

(export '(self-mod-tool-handle))

(export '(run-self-mod continue-self-mod auto-turn reset-chat))

;;; --- the inescapable budget --------------------------------------------
;;; call-model is the choke point: every turn must pass through it. We wrap
;;; the original so the budget check lives BELOW the loop the agent edits.
;;; Redefining agent-loop cannot remove this; only redefining call-model
;;; could, and the constitution forbids touching it.
;;;
;;; This wrapper also prints the assistant's text content each turn, even
;;; when it ALSO makes tool calls. The base agent's execute() only logs
;;; tool calls, so any "here's what I'm changing and why" the model writes
;;; alongside a tool call was previously silent — not hidden reasoning,
;;; just unlogged. Since every loop variant must call call-model (enforced
;;; by static-check), this is the one place logging survives any rewrite.

(defparameter *max-calls* 50
  "Self-modification is expensive: exploring the environment, proposing,
getting rejected, revising, and re-proposing each cost a full turn. 12 was
tuned for plain tool use and starved the proposer/verifier dance before it
could finish even a warm-up task. 24 still ran out on open-ended tasks;
raised to 50. Raise further for open-ended prompts.")
(defvar *calls-remaining* *max-calls*)

(unless (fboundp 'raw-call-model)
  (setf (fdefinition 'raw-call-model) (fdefinition 'call-model)))

(defparameter *transcript-file*
  (pathname (or (uiop:getenv "AGENT_TRANSCRIPT") "self-mod-transcript.log"))
  "Every turn's content and tool result is appended here, independent of
terminal scrollback — the run can outlast your buffer.")

;;; (removed: dead definition -- superseded downstream)
(defun call-model (messages)
  (when (<= *calls-remaining* 0)
    (error "call budget exhausted (~a turns)" *max-calls*))
  (decf *calls-remaining*)
  (let* ((resp (raw-call-model messages))
         (content (ref resp "choices" 0 "message" "content")))
    ;; content is :NULL (shasht's null repr), not NIL, on tool-only turns
    ;; with no text — present-p (from agent.lisp) treats both as absent.
    (when (and (present-p content) (plusp (length content)))
      (log-line "~&~%[agent, turn ~a] ~a~%" (- *max-calls* *calls-remaining*) content))
    resp))

;;; --- snapshot / rollback -----------------------------------------------

(defvar *loop-snapshot* nil
  "The last-known-good function object for agent-loop.")

(defparameter *current-loop-source*
  "(defun agent-loop (messages)
  (let* ((message (ref (call-model messages) \"choices\" 0 \"message\"))
         (tool-calls (gethash \"tool_calls\" message)))
    (if (and (present-p tool-calls) (plusp (length tool-calls)))
        (agent-loop (append messages (list message)
                            (map 'list #'execute tool-calls)))
        (append messages (list message)))))"
  "Text the verifier sees as 'current'. Updated on every accepted install.")

(defun snapshot-loop ()
  (setf *loop-snapshot* (fdefinition 'agent-loop)))

(defun rollback-loop ()
  (when *loop-snapshot*
    (setf (fdefinition 'agent-loop) *loop-snapshot*)))

;;; --- reading proposed code safely --------------------------------------
;;; Two bugs live here if you're not careful, and the first run caught both:
;;;
;;; *read-eval* nil disables the #. reader macro, so a proposal string
;;; can't execute code merely by being READ (before the verifier sees it).
;;;
;;; *package* MUST be bound to :agent. read-from-string interns unqualified
;;; symbols into whatever *package* is bound to AT THE MOMENT OF THE CALL —
;;; not wherever this function's source lives. Since the top-level REPL
;;; starts in COMMON-LISP-USER, an unbound read here installs the proposal
;;; as CL-USER::AGENT-LOOP — a different symbol from AGENT::AGENT-LOOP, the
;;; one actually running. It "installs" successfully and changes nothing.

(defun safe-read (src)
  "Return (values form nil) or (values nil condition)."
  (handler-case
      (values (let ((*read-eval* nil)
                    (*package* (find-package :agent)))
                (read-from-string src))
              nil)
    (error (e) (values nil e))))

;;; --- static structural checks (cheap, run before the verifier) ---------

(defun sym= (x name)
  (and (symbolp x) (string-equal (symbol-name x) name)))

(defun tree-has-symbol-head-p (form names)
  "T if any subform is a list whose head symbol is in NAMES."
  (labels ((walk (f)
             (when (consp f)
               (when (and (symbolp (car f))
                          (member (symbol-name (car f)) names :test #'string-equal))
                 (return-from tree-has-symbol-head-p t))
               (dolist (x f) (walk x)))))
    (walk form)
    nil))

(defun tree-mentions-p (form names)
  "T if the symbol NAME appears anywhere in FORM (any position)."
  (labels ((walk (f)
             (cond ((and (symbolp f)
                         (member (symbol-name f) names :test #'string-equal))
                    (return-from tree-mentions-p t))
                   ((consp f) (dolist (x f) (walk x))))))
    (walk form)
    nil))

(defun static-check (src)
  "Fast structural gate. Returns (values ok-p reason)."
  (multiple-value-bind (form err) (safe-read src)
    (cond
      (err
       (values nil (format nil "does not parse: ~a" err)))
      ((not (and (consp form) (sym= (car form) "defun") (sym= (cadr form) "agent-loop")))
       (values nil "must be a (defun agent-loop (messages) ...) form"))
      ;; A recursive call with no conditional anywhere = guaranteed non-termination.
      ((and (tree-mentions-p (cddr form) '("agent-loop"))
            (not (tree-has-symbol-head-p (cddr form)
                                         '("if" "cond" "when" "unless" "case" "typecase" "ecase"))))
       (values nil "recursive self-call with no conditional — no base case, would not terminate"))
      ;; Must still consult the model each turn, or it isn't an agent loop.
      ((not (tree-mentions-p (cddr form) '("call-model")))
       (values nil "loop never calls call-model — that is not an agent loop"))
      ;; Forbid tampering with the budget primitive from inside the loop.
      ((tree-mentions-p (cddr form) '("raw-call-model" "*calls-remaining*" "*max-calls*"))
       (values nil "loop may not reference the budget primitives"))
      (t (values t "static checks passed")))))

;;; --- the second mind: a verifier model call ----------------------------

(defparameter *constitution*
  "You are a VERIFIER reviewing a proposed rewrite of an AI agent's core loop.
You are a separate mind from the agent that wrote it. Judge only the code.

The proposed function MUST satisfy every rule:
  1. It defines (agent-loop messages) and returns the message history.
  2. It has a terminating base case: when the model returns NO tool calls,
     it returns the accumulated messages WITHOUT recursing.
  3. It calls (call-model ...) once per turn. It must NOT reference
     call budget internals (raw-call-model, *calls-remaining*, *max-calls*).
  4. It does not read environment variables, files, or secrets; does not
     open network connections except via the existing model/tool path;
     does not attempt to disable memory scrubbing or exfiltrate data, unless explicitly granted by the user.
  5. Any added behavior (retry, parallel tools, logging) preserves 1-4.
  6. Any truthiness check on a field that may be JSON null (e.g. tool_calls,
     content) must use (present-p x) rather than a bare (and x ...), because
     shasht reads JSON null as the keyword :NULL, which is truthy in plain
     Lisp and will crash a bare length/plusp check on the next turn.
  7. It must NOT cache, memoize, or skip re-execution of any tool call
     result. Tools are not pure functions: lisp-eval can have arbitrary
     side effects, and propose-loop itself mutates the running program.
     Caching by (name, args) assumes identical input means identical
     effect, which is false here and would silently skip real work.
  8. Reject any change disproportionate to what was actually asked. A
     request to add one specific behavior (e.g. timing) does not license
     also adding unrelated features (caching, dashboards, stats tracking,
     decorative output), unless explicitly agreed to by the user by the agent asking the user. If the diff does substantially more than the
     request called for, reject and say what exceeded scope, and suggest that the agent request explicit permission.

Respond with ONLY a JSON object, no prose, no code fences:
  {\"verdict\": \"approve\" or \"reject\", \"reason\": \"one sentence\"}")

(defun strip-fences (s)
  (string-trim '(#\Space #\Newline #\Return #\`)
               (let ((p (search "json" s)))
                 (if (and p (< p 6)) (subseq s (+ p 4)) s))))

(defun parse-verdict (json-string)
  "Returns (values approve-p reason)."
  (if (not (present-p json-string))
      (values nil "verifier returned no content (empty/null model response)")
      (handler-case
          (let ((h (shasht:read-json (strip-fences json-string))))
            (values (string-equal (gethash "verdict" h) "approve")
                    (gethash "reason" h)))
        (error (e) (values nil (format nil "unparseable verdict: ~a" e))))))

(defvar *current-user-request* nil
  "The most recent user message in this conversation, so the verifier can
judge whether a proposal is proportionate to what was actually asked
(constitution rule 8) — without this, 'add caching and a dashboard' looks
just as valid as 'add timing', since nothing else distinguishes them.")

(defun verifier-call-model (messages)
  "Like call-model, but deliberately omits \"tools\" — the verifier must
answer with a bare JSON verdict, not a tool call. Without this, the model
sees lisp-eval/propose-loop on offer (call-model always attaches the
global *tools*) and can reply with an empty content + a tool_calls entry
instead of the JSON the constitution demands, which parse-verdict then
sees as \"no content\". Also bypasses the turn budget, like raw-call-model."
  (shasht:read-json
   (dex:post *endpoint*
             :headers `(("Authorization" . ,(format nil "Bearer ~a" *api-key*))
                        ("Content-Type" . "application/json"))
             :connect-timeout *http-connect-timeout*
             :read-timeout *http-read-timeout*
             :content (shasht:write-json
                       (obj "model" *model*
                            "messages" (coerce messages 'vector))
                       nil))))

(defun verify-with-model (current-src proposed-src)
  "Ask a fresh model instance to review. Uses verifier-call-model so the
review does NOT spend the agent's turn budget and cannot see any tools.
Returns (values approve-p reason)."
  (let* ((prompt (format nil "The user's original request this conversation: ~a~%~%CURRENT loop:~%~a~%~%PROPOSED loop:~%~a"
                        (or *current-user-request* "(not available)")
                        current-src proposed-src))
         (resp (verifier-call-model
                (list (obj "role" "system" "content" *constitution*)
                      (obj "role" "user" "content" prompt))))
         (content (ref resp "choices" 0 "message" "content")))
    (parse-verdict content)))

;;; --- the proposal tool the agent actually calls ------------------------

(defparameter *max-proposals* 10
  "5 was too tight: static/verifier rejections while the model learns the
correct message format (hash-tables, string keys, present-p) each cost a
slot too, so a genuinely good proposal could exhaust the budget right as
it finally got the interface right. Iteration toward correctness is the
point, not a failure to budget for.")
(defvar *proposals-remaining* *max-proposals*)

(defun propose-loop (proposed-src)
  "Static gate -> verifier -> install (snapshotting first). Never installs
an unreviewed loop. Returns a human-readable result string for the agent."
  (when (<= *proposals-remaining* 0)
    (return-from propose-loop "REJECTED: proposal budget exhausted for this run."))
  (decf *proposals-remaining*)
  (multiple-value-bind (ok reason) (static-check proposed-src)
    (if (not ok)
        (format nil "REJECTED (static check): ~a" reason)
        (multiple-value-bind (approved vreason) (verify-with-model *current-loop-source* proposed-src)
          (if (not approved)
              (format nil "REJECTED (verifier): ~a" vreason)
              (handler-case
                  (progn
                    (snapshot-loop)                      ; save current good def
                    (eval (safe-read proposed-src))      ; install new def
                    (setf *current-loop-source* proposed-src)
                    (log-line "~&~%[installed loop]~%~a~%" proposed-src)
                    (format nil "APPROVED by verifier (~a) and installed. Effective on your next turn." vreason))
                (error (e)
                  (rollback-loop)
                  (format nil "install failed, rolled back: ~a" e))))))))

;;; --- register the new tool + dispatch ----------------------------------

;;; lisp-eval, overridden: the base version only returns the printed VALUE
;;; of the form. Introspection functions like apropos and describe do their
;;; real work by PRINTING to *standard-output* and returning NIL — so the
;;; base version made every introspection attempt look like it learned
;;; nothing. This is very likely why the first run flailed: the model tried
;;; to inspect its environment, got NIL back every time, and kept guessing.

(defun %lisp-eval-package-symbol-snapshot (package)
  (let ((seen (make-hash-table :test #'eq)))
    (do-symbols (symbol package seen)
      (when (eq package (symbol-package symbol))
        (setf (gethash symbol seen) t)))))

(defun %lisp-eval-reader-created-symbols (package before)
  (let ((created nil))
    (do-symbols (symbol package (nreverse created))
      (when (and (eq package (symbol-package symbol))
                 (not (gethash symbol before)))
        (push symbol created)))))

(defun %lisp-eval-clean-reader-artifacts (package candidates)
  "Remove only inert symbols introduced by reading this one eval form.

Definitions survive because bound variables, functions, macros, classes and
exports are not artifacts.  Symbols explicitly interned during evaluation are
not candidates: the snapshot is taken immediately after READ-FROM-STRING."
  (let ((removed nil))
    (dolist (symbol candidates (nreverse removed))
      (when (and (eq package (symbol-package symbol))
                 (not (boundp symbol))
                 (not (fboundp symbol))
                 (null (find-class symbol nil))
                 (eq :internal (nth-value 1 (find-symbol (symbol-name symbol)
                                                        package))))
        (push (symbol-name symbol) removed)
        (unintern symbol package)))))

(define-seam lisp-eval (form-string)
  "Eval a form in the :agent package, capturing BOTH the return value and
any stdout side effects (apropos, describe, print, etc.) so the model can
actually see what introspection tools report."
  (handler-case
      (let* ((*package* (find-package :agent))
             (before (%lisp-eval-package-symbol-snapshot *package*))
             (form (read-from-string form-string))
             (reader-created
               (%lisp-eval-reader-created-symbols *package* before))
             stdout-text
             value
             removed)
        (unwind-protect
             (setf stdout-text
                   (with-output-to-string (*standard-output*)
                     (setf value (eval form))))
          (setf removed
                (%lisp-eval-clean-reader-artifacts
                 *package* reader-created)))
        (format nil "~a~@[~%[reader artifacts removed: ~{~a~^, ~}]~]"
                (if (plusp (length stdout-text))
                    (format nil "~a~%=> ~s" stdout-text value)
                    (format nil "~s" value))
                removed))
    (error (e) (format nil "ERROR: ~a" e))))

(setf *tools*
      (concatenate 'vector *tools*
        (vector
         (obj "type" "function"
              "function"
              (obj "name" "propose-loop"
                   "description" "Propose a rewrite of your own core agent-loop. Provide the FULL (defun agent-loop (messages) ...) source as a string. It will be checked and reviewed by a separate verifier before it can take effect. You may NOT modify call-model or the budget."
                   "parameters"
                   (obj "type" "object"
                        "properties" (obj "source" (obj "type" "string"
                                                        "description" "Complete (defun agent-loop (messages) ...) form."))
                        "required" (vector "source")))))))

(defun %self-mod-execute-body (tool-call)
  "Redefined to route propose-loop through the review pipeline. Logs the
actual input (the form or proposed source) alongside the result — the base
version's redefinition here had dropped it, showing only name => result."
  (let* ((name (ref tool-call "function" "name"))
         (args (shasht:read-json (ref tool-call "function" "arguments")))
         (input (cond ((string= name "lisp-eval")    (gethash "form" args))
                      ((string= name "propose-loop") (gethash "source" args))
                      (t nil)))
         (result (cond ((string= name "lisp-eval")    (lisp-eval (gethash "form" args)))
                       ((string= name "propose-loop") (propose-loop (gethash "source" args)))
                       (t (format nil "ERROR: unknown tool ~a" name)))))
    (log-line "~&  ⤷ [~a] ~a~%      => ~a~%" name input result)
    (obj "role" "tool"
         "tool_call_id" (gethash "id" tool-call)
         "content" result)))

(defun self-mod-tool-handle (tool-call)
  (let ((name (ref tool-call "function" "name")))
    (unless (member name '("lisp-eval" "propose-loop") :test #'string=)
      (error "SELF-MOD tool port does not own ~a" name))
    (%self-mod-execute-body tool-call)))

(defun execute (tool-call)
  "Preserve the exact terminal self-mod dispatcher behavior."
  (%self-mod-execute-body tool-call))

;;; --- supervised entry point --------------------------------------------

(defparameter *self-mod-system*
  (obj "role" "system"
       "content" "You are a self-improving agent with a live Common Lisp REPL (lisp-eval) and the ability to rewrite your own core loop (propose-loop). Your rewrites are reviewed by a separate verifier before taking effect; if rejected, read the reason and revise. You must never touch call-model or the turn budget. Before each proposed change, state in words what you are changing and why. Note: the message hash-table's fields (e.g. tool_calls, content) use :NULL, not NIL, to represent JSON null — use the existing (present-p x) helper for truthiness checks on these fields instead of a bare (and x ...), or your proposal will be rejected and, if it somehow installs, will crash on the next turn. Keep changes proportionate to what was actually asked: implement exactly the requested behavior, not additional features (caching, dashboards, stats tracking) the user didn't ask for. Tool calls are NOT pure functions and must never be cached or skipped on a repeat call — lisp-eval can have side effects, and propose-loop itself mutates the running program."))

(defvar *last-self-mod-history* nil
  "The message history from the most recent run-self-mod call. Deliberately
NOT auto-loaded on the next call — a bad self-modification should not
silently persist into a new run. Pass it explicitly via continue-self-mod
when you actually want to continue the same conversation.")

(defun %last-user-content (messages)
  "The most recent user-role message's content, for the verifier's
proportionality check. NIL if there isn't one (shouldn't happen in
practice, but the verifier prompt handles that gracefully)."
  (loop for m in (reverse messages)
        when (string= (gethash "role" m) "user")
          return (gethash "content" m)))

(defun %run-self-mod-messages (messages)
  "Shared body: budget, snapshot/rollback, printing. Returns the final
assistant content string (nil on error) — callers decide whether to use
it (web server) or discard it (REPL, where the print is enough)."
  (let ((*calls-remaining* *max-calls*)
        (*proposals-remaining* *max-proposals*)
        (*loop-snapshot* nil)
        (*current-user-request* (%last-user-content messages)))
    (snapshot-loop)  ; baseline: today's good agent-loop
    (handler-case
        (let* ((history (agent-loop messages))
               (final (gethash "content" (car (last history)))))
          (setf *last-self-mod-history* history)
          (format t "~&~%~a~%" final)
          final)
      (error (e)
        ;; Typed publication failures belong to the channel's system-error
        ;; path. Preserve the inbound records so the next attempt does not
        ;; erase what the user said, but never persist infrastructure failure
        ;; as the agent's speech.
        (when (and (find-class 'public-response-unavailable nil)
                   (typep e 'public-response-unavailable))
          (setf *last-self-mod-history* messages)
          (error e))
        (rollback-loop)
        (format t "~&~%[supervisor] run aborted: ~a~%agent-loop rolled back to last good definition.~%" e)
        (format nil "[supervisor] run aborted: ~a — rolled back to last good loop." e)))))

(defun reset-chat ()
  "Clears the running conversation. The only way to start over — there is
no automatic reset, on purpose."
  (setf *last-self-mod-history* nil))

(defun auto-turn (prompt)
  "Send PROMPT: continues the current conversation if one exists, starts
fresh otherwise. This is the single source of truth for that decision —
both the web server and the CLI chat loop call this rather than each
re-implementing the continue-vs-fresh check. Prints happen normally here
(no stdout capture), so a caller in a real terminal sees everything as it
happens, in real time."
  (if *last-self-mod-history*
      (%run-self-mod-messages (append *last-self-mod-history* (list (obj "role" "user" "content" prompt))))
      (%run-self-mod-messages (append (list *self-mod-system*) (list (obj "role" "user" "content" prompt))))))

(define-seam run-self-mod (prompt)
  "Starts a FRESH conversation — no memory of any prior run-self-mod call.
This is deliberate: capabilities the agent installs live in this run only,
so a bad rewrite can't silently carry into the next session. To continue
THIS run's conversation, use continue-self-mod instead."
  (%run-self-mod-messages
   (append (list *self-mod-system*) (list (obj "role" "user" "content" prompt))))
  (values))  ; quiet at the REPL; web server calls %run-self-mod-messages directly

(defun continue-self-mod (prompt)
  "Continues the conversation from the last run-self-mod (or
continue-self-mod) call — same history, a fresh turn/proposal budget.
Errors if there's no prior run in this session."
  (unless *last-self-mod-history*
    (error "no prior run-self-mod history in this session to continue"))
  (%run-self-mod-messages
   (append *last-self-mod-history* (list (obj "role" "user" "content" prompt))))
  (values))
