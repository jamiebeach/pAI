(in-package :agent)

(defvar *user-time-scheduler-pass* 0)
(defvar *user-time-scheduler-fail* 0)

(defun user-time-scheduler-check (name condition)
  (if condition
      (progn (incf *user-time-scheduler-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *user-time-scheduler-fail*) (format t "  FAIL ~a~%" name))))

(load (test-source "user-time.lisp"))
(load (test-source "scheduler.lisp"))
(load (test-source "workout_nudge.lisp"))

(format t "~%== timezone ==~%")
(let ((summer (pai-local-datetime-to-universal
               "2026-07-31T19:00:00" :timezone "America/New_York"))
      (winter (pai-local-datetime-to-universal
               "2026-01-31T19:00:00" :timezone "America/New_York")))
  (user-time-scheduler-check
   "reference-zone summer time includes EDT offset"
   (search "-04:00" (pai-format-local-time
                     :universal-time summer :timezone "America/New_York"
                     :style :iso)))
  (user-time-scheduler-check
   "reference-zone winter time includes EST offset"
   (search "-05:00" (pai-format-local-time
                     :universal-time winter :timezone "America/New_York"
                     :style :iso))))

(user-time-scheduler-check
 "spring-forward nonexistent wall time is rejected"
 (handler-case
     (progn (pai-local-datetime-to-universal
             "2026-03-08T02:30:00" :timezone "America/New_York") nil)
   (error () t)))

;; The configured zone must be set explicitly here: the shipped default is
;; UTC, since a substrate cannot know its operator's zone. Operators are
;; expected to configure PAI_TIMEZONE -- an agent reasoning in UTC about a
;; person living somewhere else gets time-of-day judgements wrong.
(setf *pai-timezone-name* "America/New_York")

(user-time-scheduler-check
 "model-facing local clock is concise and location-free"
 (let ((context (pai-current-time-context)))
   (and (search "Current local time:" context)
        ;; The zone DOES reach the model, as an abbreviation. That is the
        ;; point of this module: its header states it exists so the agent
        ;; never has to infer the operator's timezone from a trailing Z or
        ;; the container's UTC locale. Timezone materially changes how the
        ;; model reasons about "this morning" or "late".
        (search "EDT" context)
        ;; What is excluded is the raw IANA identifier and a second clock --
        ;; format hygiene, not disclosure. Two timestamps in one line invite
        ;; the model to reason about the wrong one.
        ;;
        ;; De-personalisation rewrote this argument to "UTC", making it a
        ;; byte-identical duplicate of the next check. The assertion did not
        ;; fail; it became a tautology and kept passing.
        (null (search "America/New_York" context))
        (null (search "UTC" context))
        (null (search "pai-schedule-once" context)))))

(user-time-scheduler-check
 "model-facing local clock states the weekday explicitly"
 (let* ((friday (pai-local-datetime-to-universal
                 "2026-08-07T06:45:00" :timezone "America/New_York"))
        (context (pai-current-time-context friday)))
   (and (search "Friday" context)
        (search "2026-08-07T06:45 EDT" context))))

(let* ((summer (pai-local-datetime-to-universal
                "2026-08-03T23:08:00" :timezone "America/New_York"))
       (winter (pai-local-datetime-to-universal
                "2026-01-31T19:00:00" :timezone "America/New_York"))
       (summer-prefix (pai-message-time-prefix summer))
       (winter-prefix (pai-message-time-prefix winter)))
  (user-time-scheduler-check
   "summer model timestamp uses EDT without location or UTC"
   (and (string= "[2026-08-03T23:08 EDT] " summer-prefix)
        (null (search "reference-zone" summer-prefix))
        (null (search "UTC" summer-prefix))))
  (user-time-scheduler-check
   "winter model timestamp uses EST without location or UTC"
   (and (string= "[2026-01-31T19:00 EST] " winter-prefix)
        (null (search "reference-zone" winter-prefix))
        (null (search "UTC" winter-prefix)))))

(let* ((after (pai-local-datetime-to-universal
               "2026-07-31T08:59:30" :timezone "America/New_York"))
       (next (pai-cron-next-fire
              "0 9 * * 1-5" :after after :timezone "America/New_York")))
  (user-time-scheduler-check
   "weekday cron resolves in the user timezone"
   (string= "2026-07-31T09:00"
            (pai-format-local-time :universal-time next
                                     :timezone "America/New_York"
                                     :style :slot))))

(user-time-scheduler-check
 "existing workout timer resolves to 06:15 user-local across DST"
 (let ((slot (pai-format-local-time
              :universal-time (workout-nudge-next-fire) :style :slot)))
   (string= "T06:15" (subseq slot 10))))

(let* ((after-first-fall-slot
         (encode-universal-time 0 30 5 1 11 2026 0))
       (next (pai-cron-next-fire
              "30 1 * * *" :after after-first-fall-slot
              :timezone "America/New_York"
              :last-local-slot "2026-11-01T01:30")))
  (user-time-scheduler-check
   "fall-back repeated wall minute is not fired twice"
   (string= "2026-11-02T01:30"
            (pai-format-local-time :universal-time next
                                     :timezone "America/New_York"
                                     :style :slot))))

(format t "~%== durable scheduler ==~%")
(let* ((now 4000000000)
       (deliveries nil)
       (*pai-schedules-file* #P"/tmp/pai-schedules-test.json")
       (*pai-scheduled-context-file* #P"/tmp/pai-scheduled-context-test.json")
       (*pai-schedules* (make-hash-table :test #'equal))
       (*pai-scheduled-context* nil)
       (*pai-scheduler-now-fn* (lambda () now))
       (*pai-scheduler-deliver-fn*
         (lambda (text) (push text deliveries) (values t "sent"))))
  (pai-schedule-in 5 "Take the bread out" :id "once-1")
  (user-time-scheduler-check
   "notification destination is pinned at creation"
   (let ((job (gethash "once-1" *pai-schedules*)))
     (eq :null (gethash "recipient_chat_id" job))))
  (setf now (+ now 5))
  (user-time-scheduler-check "due one-off fires once"
                             (= 1 (pai-scheduler-run-due now)))
  (user-time-scheduler-check "one-off exact text is delivered"
                             (equal deliveries '("Take the bread out")))
  (user-time-scheduler-check "completed one-off cannot refire"
                             (zerop (pai-scheduler-run-due (+ now 60))))
  (let ((pending (pai-scheduler-context-snapshot)))
    (user-time-scheduler-check "fire creates a typed pending context shift"
                               (= 1 (length pending)))
    (user-time-scheduler-check "context shift is not a user-message"
                               (and (string= "once-1"
                                             (gethash "schedule_id" (aref pending 0)))
                                    (null (gethash "role" (aref pending 0)))))
    (pai-scheduler-context-consume
     (list (gethash "id" (aref pending 0))) now)
    (user-time-scheduler-check "consumed context does not repeat"
                               (zerop (length
                                       (pai-scheduler-context-snapshot)))))
  ;; Prove JSON recovery reconstructs active jobs without executing anything.
  (pai-schedule-in 120 "Persist me" :id "persist-1" :mode :context)
  (setf *pai-schedules* (make-hash-table :test #'equal)
        *pai-scheduled-context* nil)
  (%scheduler-load)
  (user-time-scheduler-check
   "active schedule survives source-state reload"
   (and (gethash "persist-1" *pai-schedules*)
        (string= "active" (gethash "status"
                                    (gethash "persist-1" *pai-schedules*)))))
  (user-time-scheduler-check
   "arbitrary executable schedule modes fail closed"
   (handler-case
       (progn (pai-schedule-in 5 "bad" :mode :lisp) nil)
     (error () t))))

(format t "~%~a passed, ~a failed~%"
        *user-time-scheduler-pass* *user-time-scheduler-fail*)
(when (plusp *user-time-scheduler-fail*) (sb-ext:exit :code 1))
