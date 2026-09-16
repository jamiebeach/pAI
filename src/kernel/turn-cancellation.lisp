;;;; turn-cancellation.lisp -- explicit ownership and cancellation of a turn.
;;;;
;;;; The public turn is serialized by *SELF-MOD-LOCK*. Track the one thread
;;;; that actually enters the pipeline (not every queued web request), so an
;;;; out-of-band operator can interrupt a blocked tool call without restarting
;;;; the process. REPL-DROP consumes structured *.request files without taking
;;;; that turn lock and calls CANCEL-ACTIVE-TURN.

(in-package :agent)

(export '(cancel-active-turn turn-cancellation-report))

(define-condition pai-turn-cancelled (error)
  ((reason :initarg :reason :reader pai-turn-cancelled-reason))
  (:report
   (lambda (condition stream)
     (format stream "Active turn cancelled: ~a"
             (pai-turn-cancelled-reason condition)))))

(defvar *active-public-turn-thread* nil)
(defvar *active-public-turn-started-at* nil)
(defvar *active-public-turn-cancel-count* 0)
(defvar *turn-cancellation-lock* (bt:make-lock "turn-cancellation-state"))
(defvar *turn-cancellation-installed-wrapper* nil)

(let ((current (fdefinition '%run-self-mod-messages)))
  (unless (and *turn-cancellation-installed-wrapper*
               (eq current *turn-cancellation-installed-wrapper*))
    (setf (fdefinition 'pai-base-run-self-mod-messages-cancellation)
          current)))

(defun %turn-cancellation-install-current-thread ()
  (bt:with-lock-held (*turn-cancellation-lock*)
    (setf *active-public-turn-thread* (bt:current-thread)
          *active-public-turn-started-at* (get-universal-time))))

(defun %turn-cancellation-clear-current-thread ()
  (bt:with-lock-held (*turn-cancellation-lock*)
    (when (eq *active-public-turn-thread* (bt:current-thread))
      (setf *active-public-turn-thread* nil
            *active-public-turn-started-at* nil))))

(defun %run-self-mod-messages (messages)
  (%turn-cancellation-install-current-thread)
  (unwind-protect
      (funcall 'pai-base-run-self-mod-messages-cancellation messages)
    (%turn-cancellation-clear-current-thread)))

(defun cancel-active-turn (&optional (reason "operator-request"))
  "Interrupt the one thread currently inside the serialized public pipeline.
Return a status keyword. This function never takes *SELF-MOD-LOCK*."
  (let ((target nil))
    (bt:with-lock-held (*turn-cancellation-lock*)
      (setf target *active-public-turn-thread*))
    (cond
      ((null target) :no-active-turn)
      ((eq target (bt:current-thread)) :refused-current-thread)
      ((not (bt:thread-alive-p target)) :stale-thread)
      (t
       (incf *active-public-turn-cancel-count*)
       (when (fboundp 'log-event)
         (ignore-errors
           (funcall 'log-event "turn-cancel-requested"
                    (obj "reason" (subseq reason 0 (min 160 (length reason)))))))
       (sb-thread:interrupt-thread
        target
        (lambda () (error 'pai-turn-cancelled :reason reason)))
       :interrupt-sent))))

(defun turn-cancellation-report ()
  (bt:with-lock-held (*turn-cancellation-lock*)
    (obj "schema_version" 1
         "active" (if (and *active-public-turn-thread*
                            (bt:thread-alive-p *active-public-turn-thread*))
                       t nil)
         "started_at" (or *active-public-turn-started-at* :null)
         "cancel_count" *active-public-turn-cancel-count*)))

(setf *turn-cancellation-installed-wrapper*
      (fdefinition '%run-self-mod-messages))
