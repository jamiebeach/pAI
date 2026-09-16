;;;; init.lisp -- phased initialization.
;;;;
;;;; Loading this system defines things. It does not start things.
;;;;
;;;; That separation did not exist before: ~55 top-level forms ran at load
;;;; time, starting nine background threads, restoring state from disk,
;;;; creating database schema, and installing wrappers. That was coherent
;;;; for exactly one deployment -- a container with storage, state and
;;;; network already present -- and broke as soon as there was a second
;;;; consumer. It made `save-lisp-and-die` unable to produce a clean image,
;;;; made CI require a live database to compile, and made it impossible to
;;;; load the system for testing without also starting its cognition.
;;;;
;;;; Files now register initialization actions instead of performing them.
;;;; The only load-time effect is adding an entry to a table.
;;;;
;;;;   (define-init :start pai-tick-loop
;;;;     "Background cognition."
;;;;     (tick-loop-start))
;;;;
;;;; Nothing runs until INITIALIZE is called.

(in-package :agent)

(export '(define-init initialize init-actions init-report
          *init-phases* init-phase-actions))

(defparameter *init-phases* '(:configure :install :restore :verify :start)
  "Initialization phases, in execution order.

   A phase exists only where a genuine precondition differs -- these five
   exhaust them, and more would be taxonomy rather than structure:

     :configure  read env and config files        needs filesystem
     :install    wrappers, observers, handlers    needs nothing
     :restore    durable state, projections       needs storage
     :verify     boot assertions, fail closed     needs the above
     :start      background workers and threads   needs a verified system

   Order is load-bearing. :install precedes :start because a worker started
   before its wrappers are installed runs unwrapped code -- a bug class this
   codebase has hit before. :configure precedes :restore because state paths
   come from configuration. :verify precedes :start so workers never launch
   on a system that failed its own boot checks.")

(defstruct (init-action (:constructor %make-init-action))
  phase name docstring thunk)

(defvar *init-actions* '()
  "Registered actions, in registration order (reversed until read).
   Registration order within a phase reproduces file load order, which is
   how the previous entrypoint's ordering is preserved exactly.")

(defvar *init-completed* '()
  "Names of actions that have run to completion, so INITIALIZE is idempotent
   at the runner level as well as per action.")

(defun init-actions (&optional phase)
  "Registered actions, optionally filtered to PHASE, in execution order."
  (let ((all (reverse *init-actions*)))
    (if phase
        (remove-if-not (lambda (a) (eq (init-action-phase a) phase)) all)
        all)))

(defun init-phase-actions ()
  "Alist of phase -> action names, for inspection and boot-parity evidence."
  (mapcar (lambda (p) (cons p (mapcar #'init-action-name (init-actions p))))
          *init-phases*))

(defun %register-init (phase name docstring thunk)
  (unless (member phase *init-phases*)
    (error "Unknown init phase ~s for action ~s. Known phases: ~s"
           phase name *init-phases*))
  ;; Re-registration replaces rather than duplicates, so reloading a single
  ;; file during development does not queue its action twice.
  (setf *init-actions*
        (remove-if (lambda (a) (eq (init-action-name a) name)) *init-actions*))
  (push (%make-init-action :phase phase :name name
                           :docstring docstring :thunk thunk)
        *init-actions*)
  name)

(defmacro define-init (phase name docstring &body body)
  "Register BODY to run during PHASE, under NAME.

   Registration happens at load time; BODY does not. Actions must be
   idempotent -- INITIALIZE may be called more than once, and re-running an
   action must not start a second thread or install a wrapper twice."
  (check-type name symbol)
  (check-type docstring string)
  `(%register-init ,phase ',name ,docstring (lambda () ,@body)))

(defun initialize (&key (phases *init-phases*) (stop-on-error t) (verbose t))
  "Run registered initialization actions for PHASES, in order.

   Returns an alist of (name . outcome), where outcome is :ok, :skipped, or
   the condition that was signalled. With STOP-ON-ERROR (the default) the
   first failure aborts -- a half-initialized agent is worse than one that
   refuses to start, and every phase after :verify assumes the ones before
   it succeeded."
  (let ((results '()))
    (dolist (phase phases (nreverse results))
      (dolist (action (init-actions phase))
        (let ((name (init-action-name action)))
          (cond
            ((member name *init-completed*)
             (push (cons name :skipped) results))
            (t
             (handler-case
                 (progn
                   (funcall (init-action-thunk action))
                   (push name *init-completed*)
                   (push (cons name :ok) results)
                   (when verbose
                     (format t "~&[init] ~(~a~) ~a~%" phase name)))
               (error (condition)
                 (push (cons name condition) results)
                 (format t "~&[init] ~(~a~) ~a FAILED: ~a~%" phase name condition)
                 (when stop-on-error
                   (return-from initialize (nreverse results))))))))))))

(defun init-report (&optional (results nil results-supplied-p))
  "Print what is registered, or the outcome of an INITIALIZE run."
  (if results-supplied-p
      (dolist (r results)
        (format t "~&~a ~a~%"
                (if (member (cdr r) '(:ok :skipped)) "  ok" "FAIL")
                (car r)))
      (dolist (phase *init-phases*)
        (let ((actions (init-actions phase)))
          (format t "~&~(~a~) (~d)~%" phase (length actions))
          (dolist (a actions)
            (format t "    ~a~@[ -- ~a~]~%"
                    (init-action-name a) (init-action-docstring a)))))))
