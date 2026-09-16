;;;; turn-watchdog.lisp -- proactive check-ins during long-running turns.
;;;; 2026-07-28. Cheap version: raw activity text, no model call. Direct
;;;; request from the operator after the propose-loop rumination incident this
;;;; same day -- a person going silent for 10 minutes mid-task would
;;;; worry you too, and nothing currently distinguishes "still working,
;;;; all fine" from "something broke."
;;;;
;;;; Deliberately polling rather than another wrap on %RUN-SELF-MOD-
;;;; MESSAGES (already four layers deep: self-mod.lisp -> agent_print.lisp
;;;; -> web-terminal.lisp -> conversation-persistence.lisp) -- that chain's own
;;;; fragility is what caused the conversation-persistence incident
;;;; earlier the same day (an unrelated live reload silently dropped a
;;;; wrap several layers away, zero error). A fifth layer here would be
;;;; more of exactly that surface area. Instead: a background thread polls
;;;; WEB-V2.LISP's existing *V2-TURN-IN-FLIGHT* flag directly.
;;;;
;;;; CRITICAL DESIGN CONSTRAINT: the check-in message is sent via a DIRECT
;;;; TELEGRAM-SEND call, never through AUTO-TURN. The whole point of this
;;;; mechanism is that *SELF-MOD-LOCK* is ALREADY HELD by the long-running
;;;; turn -- routing the check-in through the normal turn pipeline would
;;;; just block behind the very turn it's supposed to report on, exactly
;;;; the deadlock-shaped symptom the operator flagged in the propose-loop
;;;; incident. The message text is raw, non-LLM-generated activity
;;;; content (the most recent WEB-V2 ring entry, itself populated
;;;; independent of the lock -- the same property that made it usable to
;;;; diagnose that incident) explicitly labeled as a status ping, not
;;;; written in its voice -- deliberately NOT pretending to be its talking
;;;; naturally, since it isn't model-generated. A richer, model-voiced
;;;; version is a planned follow-up, explicitly deferred.
;;;;
;;;; Load live (no restart) via lisp-eval or repl-drop, AFTER
;;;; web-terminal.lisp/telegram.lisp:
;;;;   (load "/agent/state/turn-watchdog.lisp")

(in-package :agent)

(defparameter *turn-watchdog-poll-seconds* 10
  "How often the watchdog checks whether a turn is still in flight.")
(defparameter *turn-watchdog-first-checkin-seconds* 60
  "A turn must have been in flight at least this long before the FIRST
check-in fires -- short/normal turns (the vast majority) should never
trigger this at all.")
(defparameter *turn-watchdog-checkin-interval-seconds* 60
  "Minimum spacing between repeated check-ins for the SAME still-running
turn.")

(defvar *turn-watchdog-thread* nil)
(defvar *turn-watchdog-stop-requested* nil)
(defvar *public-outbound-envelope* nil)
(defvar *turn-watchdog-turn-start-time* nil
  "Universal-time the currently in-flight turn was first OBSERVED (by
this watchdog's own poll, not by a wrap) to have started, or NIL if no
turn is currently in flight.")
(defvar *turn-watchdog-last-checkin-time* nil)

(defun %turn-watchdog-send (chat-id content)
  (let* ((now (get-universal-time))
         (alert-id (format nil "turn-watchdog:~a:~a"
                           *turn-watchdog-turn-start-time* now))
         (*public-outbound-envelope*
           (and (fboundp 'make-public-outbound-envelope)
                (funcall 'make-public-outbound-envelope
                         :kind :system-alert :channel "telegram" :content content
                         :source-event-ids
                         (list (format nil "turn-start:~a"
                                       *turn-watchdog-turn-start-time*))
                         :causal-event-ids (list alert-id)
                         :authorization-kind :active-turn-alert
                         :authorization-id alert-id
                         :legacy-authorization
                         (obj "alert_type" "turn-watchdog")
                         :source "turn-watchdog" :dedupe-key alert-id))))
    (funcall 'telegram-send chat-id content)))

(defun %turn-watchdog-last-activity-text ()
  "Best-effort, raw description of the most recent thing that happened --
no model call. Pulled from WEB-V2.LISP's *V2-RING*, populated
independent of *SELF-MOD-LOCK*."
  (when (and (boundp '*v2-ring*) *v2-ring*)
    (let ((entry (first *v2-ring*)))
      (when (hash-table-p entry)
        (let* ((type (gethash "type" entry))
               (data (gethash "data" entry))
               (text (cond ((stringp data) data)
                           ((hash-table-p data) (or (gethash "text" data) ""))
                           (t ""))))
          (when (and (stringp text) (plusp (length text)))
            (format nil "(~a) ~a" type (subseq text 0 (min 220 (length text))))))))))

(defun %turn-watchdog-tick ()
  (let ((in-flight (and (boundp '*v2-turn-in-flight*) *v2-turn-in-flight*))
        (now (get-universal-time)))
    (cond
      ((not in-flight)
       (setf *turn-watchdog-turn-start-time* nil)
       (setf *turn-watchdog-last-checkin-time* nil))
      ((null *turn-watchdog-turn-start-time*)
       (setf *turn-watchdog-turn-start-time* now))
      (t
       (let ((elapsed (- now *turn-watchdog-turn-start-time*)))
         (when (and (>= elapsed *turn-watchdog-first-checkin-seconds*)
                    (or (null *turn-watchdog-last-checkin-time*)
                        (>= (- now *turn-watchdog-last-checkin-time*) *turn-watchdog-checkin-interval-seconds*))
                    (boundp '*telegram-last-chat-id*) *telegram-last-chat-id*
                    (fboundp 'telegram-send))
           (setf *turn-watchdog-last-checkin-time* now)
           (let ((activity (or (%turn-watchdog-last-activity-text) "no recent activity captured")))
             (ignore-errors
              (funcall '%turn-watchdog-send *telegram-last-chat-id*
                       (format nil "⏳ [status check-in] Still working on your last message (~a seconds so far). Most recent step: ~a"
                               elapsed activity))))))))))

(defun turn-watchdog-start ()
  (unless (and *turn-watchdog-thread* (bt:thread-alive-p *turn-watchdog-thread*))
    (setf *turn-watchdog-stop-requested* nil)
    (setf *turn-watchdog-thread*
          (bt:make-thread
           (lambda ()
             (loop until *turn-watchdog-stop-requested*
                   do (sleep *turn-watchdog-poll-seconds*)
                      (unless *turn-watchdog-stop-requested*
                        (handler-case (%turn-watchdog-tick)
                          (error (e) (format t "~&[turn-watchdog] tick error: ~a~%" e))))))
           :name "turn-watchdog")))
  (format t "~&[turn-watchdog] running: polls every ~as, first check-in after ~as in flight, repeats every ~as.~%"
          *turn-watchdog-poll-seconds* *turn-watchdog-first-checkin-seconds* *turn-watchdog-checkin-interval-seconds*))

(defun turn-watchdog-stop (&optional (timeout 5))
  (setf *turn-watchdog-stop-requested* t)
  (loop with deadline = (+ (get-internal-real-time) (* timeout internal-time-units-per-second))
        while (and *turn-watchdog-thread* (bt:thread-alive-p *turn-watchdog-thread*) (< (get-internal-real-time) deadline))
        do (sleep 0.05))
  (if (and *turn-watchdog-thread* (bt:thread-alive-p *turn-watchdog-thread*))
      (progn (ignore-errors (bt:destroy-thread *turn-watchdog-thread*)) :force-killed)
      :stopped-cleanly))

(define-init :start turn-watchdog-start
    "Start background worker for turn-watchdog."
  (turn-watchdog-start))
