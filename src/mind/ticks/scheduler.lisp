;;;; scheduler.lisp -- durable, timezone-aware one-off and cron triggers.
;;;;
;;;; Schedules contain data, never executable Lisp.  A trigger can send the
;;;; exact the operator-authored reminder text and/or queue a typed context shift for
;;;; The agent's next public turn.  It never masquerades as a user message.

(in-package :agent)

(export '(pai-schedule-once pai-schedule-in pai-schedule-cron
          pai-schedule-cancel pai-schedule-list pai-scheduler-report
          pai-scheduler-start pai-scheduler-stop
          pai-scheduler-context-snapshot pai-scheduler-context-consume
          pai-cron-next-fire))

(defparameter *pai-schedules-file* #P"/agent/state/schedules.json")
(defparameter *pai-scheduled-context-file*
  #P"/agent/state/scheduled-context.json")
(defparameter *pai-scheduler-poll-seconds* 1)
(defparameter *pai-scheduler-max-context-records* 100)
(defvar *pai-schedules* (make-hash-table :test #'equal))
(defvar *pai-scheduled-context* nil) ; newest first
(defvar *pai-scheduler-lock* (bt:make-lock "pai-scheduler"))
(defvar *pai-scheduler-thread* nil)
(defvar *pai-scheduler-stop-p* nil)
(defvar *pai-scheduler-now-fn* #'get-universal-time)
(defvar *pai-scheduler-deliver-fn* nil)
(defvar *public-outbound-envelope* nil)
(defvar *telegram-last-chat-id*) ; defined by telegram.lisp in production.

(defun %scheduler-now () (funcall *pai-scheduler-now-fn*))

(defun %scheduler-list (value)
  (cond ((null value) nil) ((listp value) value)
        ((vectorp value) (coerce value 'list)) (t (list value))))

(defun %scheduler-copy-hash (table)
  (let ((copy (make-hash-table :test (hash-table-test table))))
    (maphash (lambda (key value) (setf (gethash key copy) value)) table)
    copy))

(defun %scheduler-atomic-json-write (projection path value)
  (ensure-directories-exist path)
  (let* ((tmp (make-pathname :name (format nil "~a-tmp" (pathname-name path))
                             :type (pathname-type path) :defaults path))
         (content
           (concatenate
            'string
            (let ((*print-pretty* nil)) (shasht:write-json value nil))
            (string #\Newline))))
    (with-open-file (out tmp :direction :output :if-exists :supersede
                             :if-does-not-exist :create :external-format :utf-8)
      (write-string content out)
      (finish-output out))
    (multiple-value-prog1 (uiop:rename-file-overwriting-target tmp path)
      (when (fboundp 'log-projection-state)
        (ignore-errors
          (funcall 'log-projection-state projection path content))))))

(defun %scheduler-save-jobs ()
  (let ((rows nil))
    (maphash (lambda (id job) (declare (ignore id)) (push job rows))
             *pai-schedules*)
    (%scheduler-atomic-json-write
     "schedules"
     *pai-schedules-file*
     (obj "schema_version" 1
          "jobs" (coerce (sort rows #'string<
                               :key (lambda (job) (gethash "id" job)))
                          'vector)))))

(defun %scheduler-save-context ()
  (%scheduler-atomic-json-write
   "scheduled-context"
   *pai-scheduled-context-file*
   (obj "schema_version" 1
        "events" (coerce (reverse *pai-scheduled-context*) 'vector))))

(defun %scheduler-load ()
  (bt:with-lock-held (*pai-scheduler-lock*)
    (clrhash *pai-schedules*)
    (setf *pai-scheduled-context* nil)
    (handler-case
        (when (probe-file *pai-schedules-file*)
          (let ((data (shasht:read-json
                       (uiop:read-file-string *pai-schedules-file*))))
            (dolist (job (%scheduler-list (gethash "jobs" data)))
              (when (and (hash-table-p job) (stringp (gethash "id" job)))
                (setf (gethash (gethash "id" job) *pai-schedules*) job)))))
      (error (condition)
        (format t "~&[scheduler] job restore failed; starting empty: ~a~%"
                condition)))
    (handler-case
        (when (probe-file *pai-scheduled-context-file*)
          (let ((data (shasht:read-json
                       (uiop:read-file-string
                        *pai-scheduled-context-file*))))
            (setf *pai-scheduled-context*
                  (reverse (%scheduler-list (gethash "events" data))))))
      (error (condition)
        (format t "~&[scheduler] context restore failed; starting empty: ~a~%"
                condition))))
  t)

(defun %cron-integer (text label)
  (handler-case (parse-integer text :junk-allowed nil)
    (error () (error "Invalid cron ~a: ~s" label text))))

(defun %cron-field (text minimum maximum &key day-of-week)
  "Return a membership vector and whether TEXT was an unrestricted wildcard."
  (let ((allowed (make-array (1+ maximum) :initial-element nil))
        (wildcard (string= text "*")))
    (dolist (part (cl-ppcre:split "," text))
      (when (zerop (length part)) (error "Empty cron field component"))
      (let* ((slash (position #\/ part))
             (base (if slash (subseq part 0 slash) part))
             (step (if slash (%cron-integer (subseq part (1+ slash)) "step") 1))
             (dash (position #\- base))
             (start (cond ((string= base "*") minimum)
                          (dash (%cron-integer (subseq base 0 dash) "range"))
                          (t (%cron-integer base "value"))))
             (end (cond ((string= base "*") maximum)
                        (dash (%cron-integer (subseq base (1+ dash)) "range"))
                        (t start))))
        (when (or (<= step 0) (< start minimum) (> start maximum)
                  (< end minimum) (> end maximum) (> start end))
          (error "Cron field out of range: ~s" text))
        (loop for value from start to end by step
              for normalized = (if (and day-of-week (= value 7)) 0 value)
              do (setf (aref allowed normalized) t))))
    (values allowed wildcard)))

(defun %cron-parse (expression)
  (let ((fields (cl-ppcre:split "\\s+" (string-trim '(#\Space #\Tab) expression))))
    (unless (= (length fields) 5)
      (error "Cron expression requires five fields: minute hour day month weekday"))
    (multiple-value-bind (minutes minute-any) (%cron-field (nth 0 fields) 0 59)
      (declare (ignore minute-any))
      (multiple-value-bind (hours hour-any) (%cron-field (nth 1 fields) 0 23)
        (declare (ignore hour-any))
        (multiple-value-bind (days day-any) (%cron-field (nth 2 fields) 1 31)
          (multiple-value-bind (months month-any) (%cron-field (nth 3 fields) 1 12)
            (declare (ignore month-any))
            (multiple-value-bind (weekdays weekday-any)
                (%cron-field (nth 4 fields) 0 7 :day-of-week t)
              (list minutes hours days months weekdays day-any weekday-any))))))))

(defun %cron-matches-p (parsed universal timezone)
  (multiple-value-bind (second minute hour day month year weekday)
      (pai-local-time-components universal timezone)
    (declare (ignore second year))
    (destructuring-bind (minutes hours days months weekdays day-any weekday-any)
        parsed
      (let* ((cron-weekday (mod (1+ weekday) 7)) ; CL Monday=0; cron Sunday=0.
             (day-match (aref days day))
             (weekday-match (aref weekdays cron-weekday))
             (calendar-match
               (cond ((and day-any weekday-any) t)
                     (day-any weekday-match)
                     (weekday-any day-match)
                     (t (or day-match weekday-match)))))
        (and (aref minutes minute) (aref hours hour)
             (aref months month) calendar-match)))))

(defun pai-cron-next-fire (expression &key (after (%scheduler-now))
                                             (timezone (pai-timezone-name))
                                             last-local-slot)
  "Return the next matching UTC universal time after AFTER.
LAST-LOCAL-SLOT prevents a DST fall-back duplicate wall-clock firing."
  (let* ((parsed (%cron-parse expression))
         (candidate (* (1+ (floor after 60)) 60))
         (limit (+ candidate (* 5 366 24 60 60))))
    (loop while (<= candidate limit)
          when (and (%cron-matches-p parsed candidate timezone)
                    (or (null last-local-slot)
                        (not (string= last-local-slot
                                      (pai-format-local-time
                                       :universal-time candidate
                                       :timezone timezone :style :slot)))))
            do (return candidate)
          do (incf candidate 60)
          finally (error "No cron occurrence within five years: ~a" expression))))

(defun %scheduler-effective-timezone (job)
  (let ((timezone (gethash "timezone" job "user")))
    (if (string= timezone "user") (pai-timezone-name) timezone)))

(defun %scheduler-mode (mode)
  (let ((name (string-downcase (string mode))))
    (unless (member name '("notify" "context") :test #'string=)
      (error "Schedule mode must be :NOTIFY or :CONTEXT"))
    name))

(defun %scheduler-id (&optional id)
  (or id (format nil "schedule-~a-~6,'0x" (%scheduler-now) (random #x1000000))))

(defun %scheduler-current-recipient ()
  (if (and (boundp '*telegram-last-chat-id*)
           *telegram-last-chat-id*)
      *telegram-last-chat-id*
      :null))

(defun %scheduler-add (job)
  (bt:with-lock-held (*pai-scheduler-lock*)
    (let ((id (gethash "id" job)))
      (when (gethash id *pai-schedules*)
        (error "Schedule id already exists: ~a" id))
      (setf (gethash id *pai-schedules*) job)
      (%scheduler-save-jobs)))
  job)

(defun %scheduler-job-report (job)
  (let* ((next (gethash "next_fire_utc" job))
         (timezone (%scheduler-effective-timezone job)))
    (obj "id" (gethash "id" job)
         "kind" (gethash "kind" job)
         "mode" (gethash "mode" job)
         "status" (gethash "status" job)
         "timezone" timezone
         "cron" (or (gethash "cron" job) :null)
         "next_fire_utc" (or next :null)
         "next_fire_local"
         (if (numberp next)
             (pai-format-local-time :universal-time next :timezone timezone)
             :null)
         "text" (gethash "text" job))))

(defun pai-schedule-once (local-datetime text &key id (mode :notify)
                                                    timezone)
  "Schedule exact TEXT once at a local YYYY-MM-DDTHH:MM[:SS] wall time."
  (unless (and (stringp text) (plusp (length text)))
    (error "Scheduled text must be non-empty"))
  (let* ((zone (or timezone (pai-timezone-name)))
         (fire (pai-local-datetime-to-universal local-datetime :timezone zone))
         (job (obj "id" (%scheduler-id id) "kind" "once"
                   "mode" (%scheduler-mode mode) "text" text
                   "recipient_chat_id" (%scheduler-current-recipient)
                   "timezone" zone "created_at_utc" (%scheduler-now)
                   "next_fire_utc" fire "status" "active")))
    (when (<= fire (%scheduler-now))
      (error "One-off schedule must be in the future"))
    (%scheduler-add job)
    (%scheduler-job-report job)))

(defun pai-schedule-in (seconds text &key id (mode :notify))
  "Schedule exact TEXT once SECONDS from now."
  (unless (and (integerp seconds) (plusp seconds))
    (error "SECONDS must be a positive integer"))
  (unless (and (stringp text) (plusp (length text)))
    (error "Scheduled text must be non-empty"))
  (let ((job (obj "id" (%scheduler-id id) "kind" "once"
                  "mode" (%scheduler-mode mode) "text" text
                  "recipient_chat_id" (%scheduler-current-recipient)
                  "timezone" "user" "created_at_utc" (%scheduler-now)
                  "next_fire_utc" (+ (%scheduler-now) seconds)
                  "status" "active")))
    (%scheduler-add job)
    (%scheduler-job-report job)))

(defun pai-schedule-cron (expression text &key id (mode :notify) timezone)
  "Schedule exact TEXT on a standard five-field cron expression.
When TIMEZONE is omitted, the job follows the operator's configured timezone."
  (unless (and (stringp text) (plusp (length text)))
    (error "Scheduled text must be non-empty"))
  (let* ((zone-marker (or timezone "user"))
         (effective-zone (if (string= zone-marker "user")
                             (pai-timezone-name) zone-marker)))
    (%pai-timezone-object effective-zone)
    (let* ((next (pai-cron-next-fire expression :timezone effective-zone))
           (job (obj "id" (%scheduler-id id) "kind" "cron"
                     "mode" (%scheduler-mode mode) "text" text
                     "recipient_chat_id" (%scheduler-current-recipient)
                     "timezone" zone-marker "cron" expression
                     "created_at_utc" (%scheduler-now)
                     "next_fire_utc" next "status" "active")))
      (%scheduler-add job)
      (%scheduler-job-report job))))

(defun pai-schedule-cancel (id)
  (bt:with-lock-held (*pai-scheduler-lock*)
    (let ((job (gethash id *pai-schedules*)))
      (unless job (error "Unknown schedule: ~a" id))
      (setf (gethash "status" job) "cancelled"
            (gethash "cancelled_at_utc" job) (%scheduler-now))
      (%scheduler-save-jobs)
      (%scheduler-job-report job))))

(defun pai-schedule-list (&key active-only)
  (bt:with-lock-held (*pai-scheduler-lock*)
    (let ((rows nil))
      (maphash
       (lambda (id job)
         (declare (ignore id))
         (when (or (not active-only)
                   (string= (gethash "status" job "") "active"))
           (push (%scheduler-job-report job) rows)))
       *pai-schedules*)
      (coerce (sort rows #'string< :key (lambda (row) (gethash "id" row)))
              'vector))))

(defun %scheduler-enqueue-context (job scheduled-for now delivery-status)
  (let* ((timezone (%scheduler-effective-timezone job))
         (record
           (obj "id" (format nil "scheduled-context-~a-~6,'0x"
                             now (random #x1000000))
                "schedule_id" (gethash "id" job)
                "kind" (gethash "kind" job)
                "mode" (gethash "mode" job)
                "text" (gethash "text" job)
                "scheduled_for_utc" scheduled-for
                "scheduled_for_local"
                (pai-format-local-time :universal-time scheduled-for
                                         :timezone timezone)
                "fired_at_utc" now
                "fired_at_local"
                (pai-format-local-time :universal-time now
                                         :timezone timezone)
                "timezone" timezone
                "delivery_status" delivery-status
                "consumed_at_utc" :null)))
    (bt:with-lock-held (*pai-scheduler-lock*)
      (push record *pai-scheduled-context*)
      (when (> (length *pai-scheduled-context*)
               *pai-scheduler-max-context-records*)
        (setf *pai-scheduled-context*
              (subseq *pai-scheduled-context*
                      0 *pai-scheduler-max-context-records*)))
      (%scheduler-save-context))
    record))

(defun %scheduler-default-deliver (job)
  (let ((recipient (gethash "recipient_chat_id" job)))
    (if (and recipient (not (eq recipient :null))
             (fboundp 'telegram-send))
      (handler-case
          (let ((*public-outbound-envelope*
                  (and (fboundp 'make-public-outbound-envelope)
                       (funcall 'make-public-outbound-envelope
                                :kind :scheduled :channel "telegram"
                                :content (gethash "text" job)
                                :source-event-ids (list (gethash "id" job))
                                :causal-event-ids (list (gethash "id" job))
                                :authorization-kind :schedule-job
                                :authorization-id (gethash "id" job)
                                :legacy-authorization
                                (obj "job_id" (gethash "id" job)
                                     "mode" (gethash "mode" job))
                                :source "scheduler"
                                :dedupe-key
                                (format nil "schedule:~a:~a" (gethash "id" job)
                                        (gethash "next_fire_utc" job))))))
            (funcall 'telegram-send recipient (gethash "text" job))
                 (values t "sent"))
        (error (condition)
          (format t "~&[scheduler] Telegram delivery failed: ~a~%" condition)
          (values nil "failed")))
      (values nil "unavailable"))))

(defun %scheduler-fire (job scheduled-for now)
  (multiple-value-bind (delivered delivery-status)
      (if (string= (gethash "mode" job) "notify")
          (if *pai-scheduler-deliver-fn*
              (funcall *pai-scheduler-deliver-fn* (gethash "text" job))
              (%scheduler-default-deliver job))
          (values nil "context-only"))
    (let ((record (%scheduler-enqueue-context
                   job scheduled-for now delivery-status)))
      (when (fboundp 'log-event)
        (funcall 'log-event "schedule-fired"
                 (obj "schedule_id" (gethash "id" job)
                      "kind" (gethash "kind" job)
                      "mode" (gethash "mode" job)
                      "scheduled_for_utc" scheduled-for
                      "context_event_id" (gethash "id" record)
                      "delivery_status" delivery-status
                      "delivered" (if delivered t nil))))
      record)))

(defun %scheduler-claim-due (now)
  "Persist the next state before side effects, giving notification at-most-once semantics."
  (let ((claimed nil))
    (bt:with-lock-held (*pai-scheduler-lock*)
      (maphash
       (lambda (id job)
         (declare (ignore id))
         (let ((next (gethash "next_fire_utc" job)))
           (when (and (string= (gethash "status" job "") "active")
                      (numberp next) (<= next now))
             (let ((snapshot (%scheduler-copy-hash job))
                   (timezone (%scheduler-effective-timezone job)))
               (setf (gethash "last_fired_utc" job) next
                     (gethash "last_local_slot" job)
                     (pai-format-local-time :universal-time next
                                              :timezone timezone :style :slot))
               (if (string= (gethash "kind" job) "once")
                   (setf (gethash "status" job) "completed"
                         (gethash "next_fire_utc" job) :null)
                   (setf (gethash "next_fire_utc" job)
                         (pai-cron-next-fire
                          (gethash "cron" job) :after now :timezone timezone
                          :last-local-slot (gethash "last_local_slot" job))))
               (push (list snapshot next) claimed)))))
       *pai-schedules*)
      (when claimed (%scheduler-save-jobs)))
    (nreverse claimed)))

(defun pai-scheduler-run-due (&optional (now (%scheduler-now)))
  (let ((claimed (%scheduler-claim-due now)))
    (dolist (entry claimed)
      (%scheduler-fire (first entry) (second entry) now))
    (length claimed)))

(defun pai-scheduler-context-snapshot (&optional (limit 10))
  "Return oldest-first unconsumed typed scheduler events for context projection."
  (bt:with-lock-held (*pai-scheduler-lock*)
    (let ((pending
            (remove-if (lambda (record)
                         (numberp (gethash "consumed_at_utc" record)))
                       (reverse *pai-scheduled-context*))))
      (coerce (last pending (min limit (length pending))) 'vector))))

(defun pai-scheduler-context-consume (ids &optional (now (%scheduler-now)))
  (let ((wanted (%scheduler-list ids)) (changed 0))
    (bt:with-lock-held (*pai-scheduler-lock*)
      (dolist (record *pai-scheduled-context*)
        (when (and (member (gethash "id" record) wanted :test #'string=)
                   (not (numberp (gethash "consumed_at_utc" record))))
          (setf (gethash "consumed_at_utc" record) now)
          (incf changed)))
      (when (plusp changed) (%scheduler-save-context)))
    (when (and (plusp changed) (fboundp 'log-event))
      (funcall 'log-event "scheduled-context-consumed"
               (obj "count" changed "context_event_ids"
                    (coerce wanted 'vector))))
    changed))

(defun pai-scheduler-timezone-changed (before after)
  (declare (ignore before))
  (bt:with-lock-held (*pai-scheduler-lock*)
    (maphash
     (lambda (id job)
       (declare (ignore id))
       (when (and (string= (gethash "kind" job "") "cron")
                  (string= (gethash "timezone" job "") "user")
                  (string= (gethash "status" job "") "active"))
         (setf (gethash "next_fire_utc" job)
               (pai-cron-next-fire
                (gethash "cron" job) :after (%scheduler-now) :timezone after
                :last-local-slot (gethash "last_local_slot" job)))))
     *pai-schedules*)
    (%scheduler-save-jobs))
  t)

(defun pai-scheduler-report ()
  (bt:with-lock-held (*pai-scheduler-lock*)
    (let ((active 0) (completed 0) (cancelled 0) (pending-context 0))
      (maphash
       (lambda (id job)
         (declare (ignore id))
         (cond ((string= (gethash "status" job "") "active") (incf active))
               ((string= (gethash "status" job "") "completed") (incf completed))
               ((string= (gethash "status" job "") "cancelled") (incf cancelled))))
       *pai-schedules*)
      (dolist (record *pai-scheduled-context*)
        (unless (numberp (gethash "consumed_at_utc" record))
          (incf pending-context)))
      (obj "timezone" (pai-timezone-name)
           "thread_alive" (and *pai-scheduler-thread*
                               (bt:thread-alive-p *pai-scheduler-thread*))
           "active" active "completed" completed "cancelled" cancelled
           "pending_context" pending-context))))

(defun pai-scheduler-start ()
  (unless (and *pai-scheduler-thread*
               (bt:thread-alive-p *pai-scheduler-thread*))
    (setf *pai-scheduler-stop-p* nil
          *pai-scheduler-thread*
          (bt:make-thread
           (lambda ()
             (loop until *pai-scheduler-stop-p*
                   do (handler-case (pai-scheduler-run-due)
                        (error (condition)
                          (format t "~&[scheduler] loop error: ~a~%" condition)))
                      (sleep *pai-scheduler-poll-seconds*)))
           :name "pai-scheduler")))
  *pai-scheduler-thread*)

(defun pai-scheduler-stop ()
  (setf *pai-scheduler-stop-p* t)
  t)

(define-init :restore scheduler-restore
    "Restore durable state for scheduler."
  (%scheduler-load))
