;;;; workout_nudge.lisp -- daily 6:15am user-local workout nudge via Telegram.
;;;; Loaded by boot-verifier.lisp (section 10) so the nudge survives restarts.
;;;; Idempotent: safe to load multiple times; re-arms the timer each load.
;;;; The next occurrence is recomputed after every fire, so DST cannot drift it.

(in-package :agent)

(defvar *workout-nudge-timer* nil)
(defvar *public-outbound-envelope* nil)

(defun workout-nudge-text ()
  (multiple-value-bind (sec min hour day month year dow)
      (pai-local-time-components (get-universal-time))
    (declare (ignore sec min hour day month year))
    (let ((workout (nth dow
            (list "Lifting + 15min spin bike (moderate pace)"
                  "Pool day -- 50 laps mixed. Your best cardio. Achilles-proof."
                  "Lifting + 15min spin bike (moderate pace)"
                  "Rest or easy walk -- recovery is part of the plan"
                  "Lifting + 15min treadmill incline walk (10-12% grade, 3.5mph)"
                  "Pool + Kettlebell complexes -- metabolic furnace day"
                  "Rest day. Stretch, hydrate, enjoy it.")))
          (day-name (nth dow (list "Monday" "Tuesday" "Wednesday" "Thursday"
                                   "Friday" "Saturday" "Sunday"))))
      (format nil "6:15am -- feet on floor, no snooze.~%~%Today (~a): ~a~%~%Clothes are laid out. Coffee comes after. Go get it. -- the agent"
              day-name workout))))

(defun workout-nudge-next-fire ()
  "Universal time of the next 06:15 in the operator's configured IANA timezone."
  (pai-cron-next-fire "15 6 * * *"))

(defun workout-nudge-arm ()
  (when (boundp (quote *workout-nudge-timer*))
    (ignore-errors (sb-ext:unschedule-timer *workout-nudge-timer*)))
  (setf *workout-nudge-timer*
        (sb-ext:schedule-timer
         (sb-ext:make-timer
          (lambda ()
            (handler-case
                (when (and (boundp (quote *telegram-last-chat-id*))
                           *telegram-last-chat-id*
                           (fboundp (quote telegram-send)))
                  (let* ((content (workout-nudge-text))
                         (now (get-universal-time))
                         (*public-outbound-envelope*
                           (and (fboundp 'make-public-outbound-envelope)
                                (funcall 'make-public-outbound-envelope
                                         :kind :scheduled :channel "telegram"
                                         :content content
                                         :source-event-ids (list "workout-nudge")
                                         :causal-event-ids
                                         (list (format nil "workout-nudge-fire:~a" now))
                                         :authorization-kind :schedule-job
                                         :authorization-id "workout-nudge"
                                         :legacy-authorization
                                         (obj "job_type" "daily-workout-nudge")
                                         :source "workout-nudge"
                                         :dedupe-key
                                         (format nil "workout-nudge:~a" now)))))
                    (funcall 'telegram-send *telegram-last-chat-id* content)))
              (error (e) (format t "~%WORKOUT-NUDGE ERROR: ~a~%" e)))
            ;; A fresh local occurrence avoids fixed-86400 DST drift.
            (ignore-errors (workout-nudge-arm)))
          :name "workout-nudge")
         (workout-nudge-next-fire)))
  (format t "~&[workout-nudge] armed. Next fire computed in ~a.~%"
          (pai-timezone-name)))

;;; Arm at load time, but only if Telegram is actually up; otherwise no-op
;;; with a clear note instead of erroring the boot sequence.
(if (and (fboundp (quote telegram-send))
         (fboundp (quote pai-cron-next-fire))
         (boundp (quote *telegram-last-chat-id*))
         *telegram-last-chat-id*)
    (workout-nudge-arm)
    (format t "~&[workout-nudge] Telegram not ready; not armed. Run (workout-nudge-arm) after Telegram starts.~%"))
