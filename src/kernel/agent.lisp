;;;; agent.lisp — a recursive agent loop in Common Lisp
;;;;
;;;; The agent's only tool is EVAL. Homoiconicity does the rest:
;;;; the model writes Lisp, the loop runs it, the result flows back.
;;;;
;;;; Usage:
;;;;   export OPENROUTER_API_KEY=sk-or-...
;;;;   sbcl --load agent.lisp --eval '(agent:run "What is the 30th Fibonacci number? Compute it.")'
;;;;
;;;; Memory: the full conversation persists to memory.json between runs.
;;;;   (agent:run "My name is the operator.")
;;;;   ...later, in a fresh process...
;;;;   (agent:run "What is my name?")   ; => it remembers
;;;;   (agent:forget)                   ; wipe the slate

(ql:quickload '(:dexador :shasht) :silent t)

(defpackage :agent
  (:use :cl)
  (:export #:run #:forget #:current-model #:set-model))

(in-package :agent)

;; The tool catalogue is loaded after the base loop in the historical serial
;; system. Call sites already guard availability; this declaration records the
;; late-bound function signature for the compiler.
(declaim (ftype function brave-search))

;;; Model provider.
;;;
;;; Environment-configurable, like the database and the embedding endpoint.
;;; These were plain literals, which meant an instance could be pointed at a
;;; different database but not at a different model -- so running against a
;;; local server, a second provider or a test double required editing source
;;; or reaching in and SETF-ing the global at runtime.
;;;
;;; Anything speaking the OpenAI chat-completions shape works, which includes
;;; Ollama (:11434/v1/chat/completions) and LM Studio (:1234/v1/chat/completions).
;;; Local servers ignore the bearer token; leaving PAI_API_KEY unset is how you
;;; guarantee a run cannot reach a paid provider.
(defparameter *endpoint*
  (or (uiop:getenv "PAI_MODEL_ENDPOINT")
      "https://openrouter.ai/api/v1/chat/completions"))
(defparameter *model*
  (or (uiop:getenv "PAI_MODEL") "xiaomi/mimo-v2.5"))
(defparameter *api-key*
  (or (uiop:getenv "PAI_API_KEY") (uiop:getenv "OPENROUTER_API_KEY")))

;;; Identity: storage-partition keys, not display names.
;;;
;;; Every durable row an instance owns is partitioned by these. They are
;;; therefore immutable for the life of an instance -- an adopted instance
;;; must keep whatever ids its existing rows already carry, or it queries a
;;; partition none of its memory belongs to and comes up empty with no error.
;;; Display names are a separate, supersedable concern resolved from genesis
;;; events; see docs/progress.md, P1 Track 2.
;;;
;;; These live in the kernel because :agent code needs them (the
;;; identity-confusion detector and the dashboard both read them) and the
;;; kernel cannot depend on the memory layer. PAI.MIND.MEMORY declares its
;;; own copies: it is a pure module, forbidden by its manifest from
;;; depending on :agent, so it reads the same environment independently
;;; rather than sharing a binding.
(defparameter *agent-id* (or (uiop:getenv "PAI_AGENT_ID") "default"))
(defparameter *operator-id* (or (uiop:getenv "PAI_OPERATOR_ID") "operator"))

;;; Source-tree location.
;;;
;;; Restratification moved every source file from one flat directory into
;;; src/<layer>/, and four separate places had quietly assumed the flat shape:
;;; the persona templates, the turn-trace fixtures, the transport-escape audit
;;; and the recovery contract. Each failed differently and none of them said
;;; "the layout changed" -- the audit reported success having scanned nothing,
;;; and the recovery contract declared every file missing and paused
;;; autonomous writes on every boot.
;;;
;;; This is the shared answer. PAI_SOURCE_ROOT overrides for a deployment that
;;; ships sources elsewhere; otherwise walk up from this file until src/
;;; appears, so the lookup survives further moves.
(defvar *pai-source-root-cache* nil)

(defvar *pai-state-root-cache* nil)

(defun pai-state-root ()
  "Directory holding mutable instance state.

   PAI_STATE_ROOT makes host development independent of the historical
   container path. The default is retained for deployed instances that have
   not opted into the portable path contract."
  (or *pai-state-root-cache*
      (setf *pai-state-root-cache*
            (uiop:ensure-directory-pathname
             (pathname (or (uiop:getenv "PAI_STATE_ROOT")
                           "/agent/state/"))))))

(defun pai-state-path (relative)
  "Resolve RELATIVE below the configured mutable state root."
  (merge-pathnames (pathname relative) (pai-state-root)))

(defun pai-source-root ()
  "Directory holding the source tree, or NIL if it cannot be located.

   Resolved lazily and cached. It must NOT be computed at load time from
   *LOAD-TRUENAME*: under ASDF this file is loaded from a compiled fasl in the
   cache directory, so *LOAD-TRUENAME* points there and walking up from it
   finds nothing. That failure is silent -- every lookup simply returns NIL --
   and it does not reproduce under the test harness, which LOADs source
   directly and therefore sees the path it expects.

   ASDF knows where the system came from, so ask it first."
  (or *pai-source-root-cache*
      (setf *pai-source-root-cache*
            (let ((configured (uiop:getenv "PAI_SOURCE_ROOT")))
              (cond
                ((and configured (plusp (length configured)))
                 (pathname (concatenate 'string configured "/")))
                ((find-package :asdf)
                 (let ((dir (ignore-errors
                             (funcall (find-symbol "SYSTEM-SOURCE-DIRECTORY" :asdf)
                                      :pai))))
                   (and dir (probe-file (merge-pathnames #P"src/" dir))
                        (merge-pathnames #P"src/" dir))))
                (t nil))))
      ;; Fallback for a plain LOAD of this file, which is how the suites run.
      (setf *pai-source-root-cache*
            (loop with dir = (make-pathname :name nil :type nil
                                            :defaults (or *load-truename*
                                                          *default-pathname-defaults*))
                  repeat 8
                  for candidate = (merge-pathnames #P"src/" dir)
                  when (probe-file candidate) return candidate
                  do (let ((parent (uiop:pathname-parent-directory-pathname dir)))
                       (when (equal parent dir) (return nil))
                       (setf dir parent))))))

(defvar *pai-source-index* nil)

(defun pai-source-file (name)
  "Absolute path of the source file called NAME, wherever it now lives.

   Callers hold flat basenames from when the tree was flat. Resolving them by
   basename keeps those lists working across layout changes; returns NIL when
   there is genuinely no such file, so a caller can tell 'moved' from 'gone'."
  (unless *pai-source-index*
    (let ((root (pai-source-root)))
      (setf *pai-source-index* (make-hash-table :test #'equalp))
      (when root
        ;; templates/ sits beside src/, not inside it, and some callers name it
        ;; with its directory ("templates/PAI-IDENTITY.default.md"). Indexing
        ;; both trees and looking up by FILE-NAMESTRING accepts either form.
        (let ((repo (uiop:pathname-parent-directory-pathname root)))
          (dolist (tree (list (merge-pathnames "**/*.*" root)
                              (merge-pathnames "templates/**/*.*" repo)))
            (dolist (p (directory tree))
              (setf (gethash (file-namestring p) *pai-source-index*) p)))))))
  (gethash (file-namestring (pathname name)) *pai-source-index*))

(defun current-model () *model*)

(defun set-model (name)
  "Switch the OpenRouter model used for every subsequent call-model
(agent, verifier, everything — there's only one *model*). Takes effect
on the next turn; does not touch the conversation in progress."
  (setf *model* name))

;;; --- tiny JSON helpers -------------------------------------------------
;;; shasht reads JSON objects as hash tables; OBJ builds them going out.

(defun obj (&rest kvs)
  (loop with h = (make-hash-table :test #'equal)
        for (k v) on kvs by #'cddr
        do (setf (gethash k h) v)
        finally (return h)))

(defun ref (table &rest keys)
  "Walk nested hash tables / vectors: (ref x \"choices\" 0 \"message\")"
  (reduce (lambda (acc key)
            (etypecase key
              (string (gethash key acc))
              (integer (aref acc key))))
          keys :initial-value table))

;;; --- the tool: a Lisp REPL ---------------------------------------------

(defparameter *tools*
  (vector
   (obj "type" "function"
        "function"
        (obj "name" "lisp-eval"
             "description" "Evaluate one bounded Common Lisp form for computation or explicit in-process introspection. Do not use it for web requests, file discovery, or document reading/writing when a dedicated tool is available."
             "parameters"
             (obj "type" "object"
                  "properties" (obj "form" (obj "type" "string"
                                                "description" "A single Common Lisp form, e.g. (reduce #'+ (loop for i from 1 to 100 collect i))"))
                  "required" (vector "form"))))))

(defun lisp-eval (form-string)
  "The agent's hands. Read a form, eval it, print what came back."
  (handler-case
      (format nil "~s" (eval (read-from-string form-string)))
    (error (e) (format nil "ERROR: ~a" e))))

(defun execute (tool-call)
  "Turn one tool-call from the model into a tool-result message."
  (let* ((name (ref tool-call "function" "name"))
         (args (shasht:read-json (ref tool-call "function" "arguments")))
         (result (if (string= name "lisp-eval")
                     (lisp-eval (gethash "form" args))
                     (format nil "ERROR: unknown tool ~a" name))))
    (format t "~&  ⤷ ~a => ~a~%" (gethash "form" args) result)
    (obj "role" "tool"
         "tool_call_id" (gethash "id" tool-call)
         "content" result)))

;;; --- talking to the model ----------------------------------------------

(defparameter *http-connect-timeout* 10
  "Seconds to wait for the TCP+TLS handshake. Short on purpose -- a dead
or unreachable endpoint should fail fast, not hang the whole agent (every
turn runs under a single global lock, so one stuck call-model blocks
everything else too).")

(defparameter *http-read-timeout* 120
  "Seconds to wait for a response once the request is sent. This bounds a
SINGLE HTTP call, not a whole multi-turn task -- a long agentic effort
just makes more calls, each with its own fresh budget of this many
seconds. Generous because free-tier OpenRouter models can genuinely take
a minute or more under load; the point is only to turn an indefinite
hang (dexador has no timeout by default) into a normal, catchable error.")

(defun call-model (messages)
  (shasht:read-json
   (dex:post *endpoint*
             :headers `(("Authorization" . ,(format nil "Bearer ~a" *api-key*))
                        ("Content-Type" . "application/json"))
             :connect-timeout *http-connect-timeout*
             :read-timeout *http-read-timeout*
             :content (shasht:write-json
                       (obj "model" *model*
                            "messages" (coerce messages 'vector)
                            "tools" *tools*)
                       nil))))

;;; --- the loop itself ----------------------------------------------------
;;; An agent is a recursive function over a growing list of messages.
;;; Base case: the model answers in words. Recursive case: it asks
;;; for tools, we run them, and recur with the enriched history.
;;;
;;; present-p exists because shasht reads JSON `null` as the keyword :NULL,
;;; which is truthy in Lisp — and OpenRouter sometimes sends an explicit
;;; "tool_calls": null on the final answer instead of omitting the key.
;;; Without this check, (and tool-calls (plusp (length tool-calls))) would
;;; see :NULL as truthy and crash trying to (length :NULL). Note this is
;;; deliberately NOT fixed by reading null as NIL globally: shasht's :NULL
;;; round-trips correctly back to JSON null on write, whereas NIL writes
;;; back as JSON false — which breaks re-sending a message with legitimately
;;; null content on the next turn. The fix belongs in the check, not the read.

(defun present-p (x)
  "T unless X is absent (NIL) or an explicit JSON null (:NULL)."
  (and x (not (eq x :null))))

(defun agent-loop (messages)
  "Returns the complete message history, final answer included.
The answer is just (gethash \"content\" (car (last messages)))."
  (let* ((message (ref (call-model messages) "choices" 0 "message"))
         (tool-calls (gethash "tool_calls" message)))
    (if (and (present-p tool-calls) (plusp (length tool-calls)))
        (agent-loop (append messages
                            (list message)
                            (map 'list #'execute tool-calls)))
        (append messages (list message)))))

;;; --- memory ---------------------------------------------------------------
;;; Messages are already a list of hash tables, i.e. already JSON.
;;; So memory is nothing more than writing that list down and reading it back.

(defparameter *memory-file*
  (pathname (or (uiop:getenv "AGENT_MEMORY") "memory.json")))

(defparameter *system-message*
  (obj "role" "system"
       "content" "You are a helpful agent with a live Common Lisp REPL. Prefer computing answers with lisp-eval over guessing. Your conversation history persists across sessions."))

(defun remember (messages)
  (with-open-file (out *memory-file* :direction :output :if-exists :supersede)
    (shasht:write-json (coerce messages 'vector) out))
  messages)

(defun recall ()
  (if (probe-file *memory-file*)
      (coerce (with-open-file (in *memory-file*) (shasht:read-json in)) 'list)
      (list *system-message*)))

(defun forget ()
  (when (probe-file *memory-file*) (delete-file *memory-file*))
  (format t "~&Memory wiped.~%"))

;;; --- entry point ------------------------------------------------------------

(defun run (prompt)
  (let ((history (remember
                  (agent-loop
                   (append (recall)
                           (list (obj "role" "user" "content" prompt)))))))
    (format t "~&~a~%" (gethash "content" (car (last history))))))
