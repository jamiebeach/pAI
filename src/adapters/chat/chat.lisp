;;;; chat.lisp — a plain chat REPL for the self-mod agent.
;;;;
;;;; Loads on top of agent.lisp + self-mod.lisp (web.lisp NOT required —
;;;; this is the alternative to it, not an addition on top).
;;;;
;;;; The problem this solves: the web UI buffers an entire turn into a
;;;; string before sending it back as one JSON response, so you only ever
;;;; see the model's narration and tool calls AFTER the turn finishes.
;;;; This loop does no such buffering — every (format t ...) inside the
;;;; agent, from turn narration to tool calls to timing lines, prints to
;;;; your terminal the instant it happens, because nothing is capturing
;;;; *standard-output* along the way.
;;;;
;;;; It also removes the ergonomics problem directly: type a plain message,
;;;; hit enter, no agent:run-self-mod / agent:continue-self-mod typing.
;;;; auto-turn (in self-mod.lisp) decides continue-vs-fresh for you, same
;;;; logic the web server uses.
;;;;
;;;; Usage:
;;;;   sbcl --load agent.lisp --load self-mod.lisp --load chat.lisp \
;;;;        --eval '(agent:chat)'
;;;;
;;;; By default /quit just returns you to the Lisp REPL (nothing shuts
;;;; down, so tests and interactive poking both work as expected). Call
;;;; (agent:chat :exit-on-quit t) — as the Docker entrypoint does — to
;;;; make /quit (and Ctrl-D) actually terminate the process, stopping the
;;;; web server first if it's running. That's what makes /quit gracefully
;;;; shut down a container: without it, /quit only exits the chat loop and
;;;; leaves the SBCL process (and container) running at a bare REPL prompt.
;;;;
;;;; Commands at the prompt: /new (reset conversation), /quit or /exit,
;;;; /help. Anything else is sent to the agent as a message. Pasting
;;;; multi-line text (e.g. Lisp source) is handled automatically — see
;;;; %chat-read-line.

(in-package :agent)

(export '(chat))

(defun %chat-read-line ()
  "Reads one logical message, auto-joining a multi-line paste into one.
A terminal delivers a pasted block to stdin in one burst, so once the
first line comes back, LISTEN sees the rest already buffered; a human
typing line by line doesn't produce that (each Enter is its own event
with nothing queued behind it). So: keep consuming buffered lines as
long as LISTEN says more is waiting, and join them with newlines into a
single message — otherwise each pasted line would fire off as its own
fragmentary turn."
  (format t "~%~%> ")
  (finish-output)
  (let ((first (read-line *standard-input* nil :eof)))
    (if (eq first :eof)
        :eof
        (let ((lines (list first)))
          (loop while (listen *standard-input*)
                do (push (read-line *standard-input* nil "") lines))
          (format nil "~{~a~^~%~}" (nreverse lines))))))

(defun %chat-help ()
  (format t "~&Commands:~%  /new           start a fresh conversation (forgets everything so far)~%  /model         show the current OpenRouter model~%  /model <name>  switch models, e.g. /model anthropic/claude-sonnet-4.5~%  /quit          exit (also stops the container, if running as one)~%Anything else is sent to the agent, continuing the current conversation.~%Pasting multi-line text (e.g. Lisp source) is joined into one message automatically.~%"))

(defun %chat-model-command (trimmed)
  "Handles /model and /model <name>. TRIMMED is the whole command line,
already trimmed of leading/trailing whitespace, still starting with
\"/model\"."
  (let ((arg (string-trim '(#\Space #\Tab) (subseq trimmed (length "/model")))))
    (if (zerop (length arg))
        (format t "~&Current model: ~a~%" (current-model))
        (progn
          (set-model arg)
          (format t "~&Model set to ~a~%(takes effect on your next message; the current conversation continues.)~%" arg)))))

(defun %chat-shutdown-or-return (exit-process)
  "If EXIT-PROCESS, stop the web server (if loaded) and terminate the SBCL
process outright — this is what actually stops a Docker container, since
otherwise /quit only exits the chat loop and leaves you at a live REPL
prompt with the process (and container) still running. If not, just
returns normally, which is what tests and interactive REPL use want."
  (finish-output)
  (when exit-process
    (when (fboundp 'stop-web) (ignore-errors (funcall 'stop-web)))
    (sb-ext:exit :code 0)))

(defun chat (&key exit-on-quit)
  "A plain chat loop: type a message, see everything print in real time,
no Lisp function calls required. /new resets, /quit exits.

EXIT-ON-QUIT defaults to NIL: /quit returns you to the Lisp REPL, same as
before. Pass T (as the Docker entrypoint does) to make /quit — and Ctrl-D
— actually terminate the process, which is what stops the container."
  (format t "~&Self-mod agent chat. Type a message, or /new, /quit, /help.~%")
  (loop
    (let ((line (%chat-read-line)))
      (cond
        ((or (eq line :eof) (null line))
         (format t "~%bye~%")
         (%chat-shutdown-or-return exit-on-quit)
         (return))
        (t (let ((trimmed (string-trim '(#\Space #\Tab) line)))
             (cond
               ((zerop (length trimmed)) nil)  ; blank line, just re-prompt
               ((member trimmed '("/quit" "/exit") :test #'string-equal)
                (format t "~%bye~%")
                (%chat-shutdown-or-return exit-on-quit)
                (return))
               ((string-equal trimmed "/new")
                (reset-chat)
                (format t "~%[new conversation started]~%"))
               ((string-equal trimmed "/help")
                (%chat-help))
               ((or (string-equal trimmed "/model")
                    (and (>= (length trimmed) 7) (string-equal (subseq trimmed 0 7) "/model ")))
                (%chat-model-command trimmed))
               (t (submit-stimulus trimmed :kind :user-message
                                           :wait-for-public-result t)))))))))
