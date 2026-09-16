;;;; user-time.lisp -- one explicit user-local clock boundary.
;;;;
;;;; Durable/audit timestamps remain UTC.  This module owns display, prompt,
;;;; and scheduling conversions so the agent never has to infer the operator's timezone
;;;; from a trailing Z timestamp or the container's UTC locale.

(in-package :agent)

(ql:quickload :local-time :silent t)

;; Load the IANA repository once so named zones (and travel-time changes) are
;; available. LOCAL-TIME ships a portable repository; deployments may point at
;; a system copy explicitly, but host development must not assume a Unix path.
(let ((configured (uiop:getenv "PAI_TIMEZONE_REPOSITORY")))
  (if configured
      (local-time:reread-timezone-repository
       :timezone-repository (pathname configured))
      (local-time:reread-timezone-repository)))

(export '(pai-timezone-name pai-set-timezone pai-format-local-time
          pai-message-time-prefix
          pai-current-time-context pai-local-datetime-to-universal
          pai-local-time-components))

(defparameter *pai-timezone-file*
  (if (fboundp 'pai-state-path)
      (funcall 'pai-state-path #P"user-timezone.json")
      #P"/agent/state/user-timezone.json"))
(defvar *pai-timezone-name* (or (uiop:getenv "PAI_TIMEZONE") "UTC")
  "Operator-local timezone. Deployment config, not identity. UTC default so a
   fresh instance is correct-but-generic rather than silently in someone
   else timezone.")
(defvar *pai-timezone-lock* (bt:make-lock "pai-timezone"))

(declaim (ftype function pai-format-local-time))

(defun %pai-timezone-object (&optional (name *pai-timezone-name*))
  (or (ignore-errors (local-time:find-timezone-by-location-name name))
      (error "Unknown IANA timezone: ~a" name)))

(defun pai-timezone-name () *pai-timezone-name*)

(defun %pai-save-timezone ()
  (ensure-directories-exist *pai-timezone-file*)
  (let ((tmp (make-pathname :name "user-timezone-tmp" :type "json"
                            :defaults *pai-timezone-file*)))
    (with-open-file (out tmp :direction :output :if-exists :supersede
                             :if-does-not-exist :create :external-format :utf-8)
      (let ((*print-pretty* nil))
        (write-string
         (shasht:write-json
          (obj "schema_version" 1 "timezone" *pai-timezone-name*) nil)
         out))
      (terpri out)
      (finish-output out))
    (uiop:rename-file-overwriting-target tmp *pai-timezone-file*))
  t)

(defun %pai-load-timezone ()
  (handler-case
      (when (probe-file *pai-timezone-file*)
        (let* ((data (shasht:read-json
                      (uiop:read-file-string *pai-timezone-file*)))
               (name (gethash "timezone" data)))
          (when (and (stringp name) (plusp (length name)))
            (%pai-timezone-object name)
            (setf *pai-timezone-name* name))))
    (error (condition)
      (format t "~&[user-time] invalid config; retaining ~a: ~a~%"
              *pai-timezone-name* condition)))
  *pai-timezone-name*)

(defun pai-set-timezone (name &key (actor "the operator"))
  "Persist a validated IANA timezone and recompute user-zone cron schedules."
  (unless (and (stringp name) (plusp (length name)))
    (error "Timezone must be a non-empty IANA name"))
  (%pai-timezone-object name)
  (bt:with-lock-held (*pai-timezone-lock*)
    (let ((before *pai-timezone-name*))
      (setf *pai-timezone-name* name)
      (handler-case (%pai-save-timezone)
        (error (condition)
          (setf *pai-timezone-name* before)
          (error condition)))
      (when (fboundp 'pai-scheduler-timezone-changed)
        (funcall 'pai-scheduler-timezone-changed before name))
      (when (fboundp 'log-event)
        (funcall 'log-event "user-timezone-changed"
                 (obj "before" before "after" name "actor" actor)))
      (obj "timezone" name
           "local_time" (pai-format-local-time)
           "previous_timezone" before))))

(defun pai-local-time-components (&optional (universal-time (get-universal-time))
                                              (timezone *pai-timezone-name*))
  "Return CL-style local components using an IANA zone, including DST."
  (local-time:decode-universal-time-with-tz
   universal-time :timezone (%pai-timezone-object timezone)))

(defun %pai-local-timestamp (&optional (universal-time (get-universal-time)))
  (local-time:universal-to-timestamp universal-time))

(defun pai-format-local-time (&key (universal-time (get-universal-time))
                                     (timezone *pai-timezone-name*)
                                     (style :human))
  "Render UNIVERSAL-TIME in TIMEZONE. STYLE is :HUMAN, :ISO, or :SLOT."
  (let ((timestamp (%pai-local-timestamp universal-time))
        (zone (%pai-timezone-object timezone)))
    (local-time:format-timestring
     nil timestamp :timezone zone
     :format
     (ecase style
       (:human '(:long-weekday ", " :long-month " " :day ", " :year
                 " at " :hour12 ":" (:min 2) " " :ampm " " :timezone
                 " (UTC" :gmt-offset ")"))
       (:iso '((:year 4) "-" (:month 2) "-" (:day 2) "T"
               (:hour 2) ":" (:min 2) ":" (:sec 2) :gmt-offset))
       (:slot '((:year 4) "-" (:month 2) "-" (:day 2) "T"
                (:hour 2) ":" (:min 2)))))))

(defun pai-message-time-prefix (&optional (universal-time (get-universal-time)))
  "Model-visible user-local timestamp with no geographic location cue."
  (let ((timestamp (%pai-local-timestamp universal-time))
        (zone (%pai-timezone-object)))
    (format nil "[~a ~a] "
            (pai-format-local-time
             :universal-time universal-time :style :slot)
            (local-time:format-timestring
             nil timestamp :timezone zone :format '(:timezone)))))

(defun pai-current-time-context (&optional (universal-time (get-universal-time)))
  "One model-facing local clock line. Geographic zone and scheduler API
documentation remain operational state, not conversational prompt material."
  (multiple-value-bind (second minute hour day month year weekday)
      (pai-local-time-components universal-time)
    (declare (ignore second))
    (format nil "Current local time: ~a, ~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d ~a."
            (aref #("Monday" "Tuesday" "Wednesday" "Thursday" "Friday"
                    "Saturday" "Sunday")
                  weekday)
            year month day hour minute
            (local-time:format-timestring
             nil (%pai-local-timestamp universal-time)
             :timezone (%pai-timezone-object) :format '(:timezone)))))

(defun %pai-user-time-block ()
  (format nil "<!-- USER-TIME:BEGIN -->~%### the operator's local clock~%~a~%<!-- USER-TIME:END -->"
          (pai-current-time-context)))

(defun %pai-replace-user-time-block (text)
  (let* ((value (or text ""))
         (begin "<!-- USER-TIME:BEGIN -->")
         (end "<!-- USER-TIME:END -->")
         (bp (search begin value))
         (ep (and bp (search end value :start2 (+ bp (length begin)))))
         (block (%pai-user-time-block)))
    (if (and bp ep)
        (concatenate 'string (subseq value 0 bp) block
                     (subseq value (+ ep (length end))))
        (format nil "~a~%~%~a" value block))))

(defun %pai-refresh-user-time-context ()
  ;; Refresh both the fresh-conversation seed and the active system message.
  (when (and (boundp '*self-mod-system*)
             (hash-table-p (symbol-value '*self-mod-system*)))
    (let ((message (symbol-value '*self-mod-system*)))
      (setf (gethash "content" message)
            (%pai-replace-user-time-block (gethash "content" message "")))))
  (when (boundp '*last-self-mod-history*)
    (let ((system (find "system" (symbol-value '*last-self-mod-history*)
                        :key (lambda (message) (gethash "role" message ""))
                        :test #'string=)))
      (when system
        (setf (gethash "content" system)
              (%pai-replace-user-time-block (gethash "content" system ""))))))
  t)

(defun %pai-parse-fixed-integer (text start end label)
  (handler-case (parse-integer text :start start :end end :junk-allowed nil)
    (error () (error "Invalid ~a in local datetime: ~a" label text))))

(defun pai-local-datetime-to-universal (text &key (timezone *pai-timezone-name*))
  "Parse YYYY-MM-DDTHH:MM[:SS] as local wall time in an IANA timezone."
  (unless (and (stringp text) (member (length text) '(16 19))
               (char= (char text 4) #\-)
               (char= (char text 7) #\-)
               (member (char text 10) '(#\T #\Space))
               (char= (char text 13) #\:)
               (or (= (length text) 16) (char= (char text 16) #\:)))
    (error "Expected local datetime YYYY-MM-DDTHH:MM[:SS], got ~s" text))
  (let* ((year (%pai-parse-fixed-integer text 0 4 "year"))
         (month (%pai-parse-fixed-integer text 5 7 "month"))
         (day (%pai-parse-fixed-integer text 8 10 "day"))
         (hour (%pai-parse-fixed-integer text 11 13 "hour"))
         (minute (%pai-parse-fixed-integer text 14 16 "minute"))
         (second (if (= (length text) 19)
                     (%pai-parse-fixed-integer text 17 19 "second") 0))
         (zone (%pai-timezone-object timezone))
         (timestamp (local-time:encode-timestamp
                     0 second minute hour day month year :timezone zone))
         (universal (local-time:timestamp-to-universal timestamp)))
    ;; Reject normalized impossible dates rather than silently scheduling a
    ;; different wall time (notably the DST spring-forward gap).
    (multiple-value-bind (actual-second actual-minute actual-hour actual-day
                          actual-month actual-year)
        (pai-local-time-components universal timezone)
      (unless (and (= second actual-second) (= minute actual-minute)
                   (= hour actual-hour) (= day actual-day)
                   (= month actual-month) (= year actual-year))
        (error "Local datetime does not exist in ~a: ~a" timezone text)))
    universal))

(define-init :configure user-time-configure
    "Read configuration for user-time."
  (%pai-load-timezone))

;; This clock is independent of STAB-06.  Shadow/legacy turns retain the
;; explicit USER-TIME block; enforced context projection replaces it with the
;; same typed value inside PAI-STATE.
(defvar *user-time-installed-auto-wrapper* nil)
(when (fboundp 'auto-turn)
  (let ((current (fdefinition 'auto-turn)))
    (unless (and *user-time-installed-auto-wrapper*
                 (eq current *user-time-installed-auto-wrapper*))
      (setf (fdefinition 'pai-base-auto-turn-user-time) current)))
  (defun auto-turn (prompt)
    (%pai-refresh-user-time-context)
    (funcall 'pai-base-auto-turn-user-time prompt))
  (setf *user-time-installed-auto-wrapper* (fdefinition 'auto-turn)))
