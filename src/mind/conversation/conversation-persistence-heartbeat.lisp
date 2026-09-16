;;;; conversation-persistence-heartbeat.lisp -- a wrap-independent safety
;;;; net for conversation.json saving. 2026-07-28.
;;;;
;;;; ROOT CAUSE FOUND LIVE: conversation-persistence.lisp's save is a
;;;; wrap around %RUN-SELF-MOD-MESSAGES, four layers deep (self-mod.lisp
;;;; -> agent_print.lisp -> web-terminal.lisp -> conversation-persistence.lisp),
;;;; and its own header already documents the fragility -- it must load
;;;; AFTER web-terminal.lisp specifically to sit on top of the fully-composed
;;;; chain. What its header did NOT document: reloading pai-
;;;; enhancements.lisp live (which unconditionally reloads agent_print.lisp
;;;; inside itself, a documented gotcha for web-terminal.lisp's OWN wrap) has
;;;; the exact same effect one layer further out -- it silently resets
;;;; %RUN-SELF-MOD-MESSAGES back through agent_print.lisp, dropping BOTH
;;;; web-terminal.lisp's wrap AND conversation-persistence.lisp's wrap unless
;;;; every downstream file is reloaded, in order, afterward. Confirmed
;;;; live: this happened once already today (deploying the reasoning-
;;;; fallback fix) -- web-terminal.lisp got correctly reloaded per its own
;;;; documented warning, conversation-persistence.lisp did not, and
;;;; conversation.json silently stopped updating for roughly nine hours
;;;; with zero error anywhere. Nothing crashed; nothing looked wrong;
;;;; the conversation just quietly stopped being saved.
;;;;
;;;; Rather than relying on every future live-reload remembering the
;;;; correct, ever-growing chain of "if you touch X, also reload Y and Z
;;;; afterward" -- exactly the kind of thing that's already bitten this
;;;; codebase multiple times (agent_print.lisp/web-terminal.lisp, and now this)
;;;; -- this adds an independent background thread that saves
;;;; *LAST-SELF-MOD-HISTORY* on its own fixed schedule, with NO dependency
;;;; on any wrap chain surviving intact. The existing per-turn wrap stays
;;;; (lower latency: saves immediately after every completed turn, not
;;;; just every *CONV-HEARTBEAT-INTERVAL-SECONDS*) -- this is purely a
;;;; redundant safety net, bounding the worst case from "however long
;;;; until someone notices" to one heartbeat interval, regardless of what
;;;; gets reloaded on top of it later.
;;;;
;;;; Load live (no restart) via lisp-eval or repl-drop, AFTER
;;;; conversation-persistence.lisp:
;;;;   (load "/agent/state/conversation-persistence-heartbeat.lisp")

(in-package :agent)

(defparameter *conv-heartbeat-interval-seconds* 60)
(defvar *conv-heartbeat-thread* nil)
(defvar *conv-heartbeat-stop-requested* nil)
(defvar *conv-heartbeat-last-saved-length* nil
  "Message count at the last heartbeat save -- a cheap fingerprint to
skip a pointless disk write when nothing has changed since the last
save (whether that was this heartbeat or the per-turn wrap).")

(defun %conv-heartbeat-tick ()
  (when (and (boundp '*last-self-mod-history*) *last-self-mod-history*)
    (let ((n (length *last-self-mod-history*)))
      (unless (eql n *conv-heartbeat-last-saved-length*)
        (%conv-persist-write *last-self-mod-history*)
        (setf *conv-heartbeat-last-saved-length* n)))))

(defun conv-heartbeat-start ()
  (unless (and *conv-heartbeat-thread* (bt:thread-alive-p *conv-heartbeat-thread*))
    (setf *conv-heartbeat-stop-requested* nil)
    (setf *conv-heartbeat-thread*
          (bt:make-thread
           (lambda ()
             (loop until *conv-heartbeat-stop-requested*
                   do (sleep *conv-heartbeat-interval-seconds*)
                      (unless *conv-heartbeat-stop-requested*
                        (handler-case (%conv-heartbeat-tick)
                          (error (e) (format t "~&[conv-heartbeat] tick error: ~a~%" e))))))
           :name "conv-heartbeat")))
  (format t "~&[conv-heartbeat] running, independent save every ~as regardless of the wrap chain.~%"
          *conv-heartbeat-interval-seconds*))

(defun conv-heartbeat-stop (&optional (timeout 5))
  (setf *conv-heartbeat-stop-requested* t)
  (loop with deadline = (+ (get-internal-real-time) (* timeout internal-time-units-per-second))
        while (and *conv-heartbeat-thread* (bt:thread-alive-p *conv-heartbeat-thread*) (< (get-internal-real-time) deadline))
        do (sleep 0.05))
  (if (and *conv-heartbeat-thread* (bt:thread-alive-p *conv-heartbeat-thread*))
      (progn (ignore-errors (bt:destroy-thread *conv-heartbeat-thread*)) :force-killed)
      :stopped-cleanly))

(define-init :start conversation-persistence-heartbeat-start
    "Start background worker for conversation-persistence-heartbeat."
  (conv-heartbeat-start))
