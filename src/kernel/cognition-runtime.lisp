;;;; cognition-runtime.lisp -- exclusive cognition runtime selection.
;;;;
;;;; The registry sits above both cognition loops.  It owns selection and
;;;; lifecycle, but knows nothing about either implementation: descriptor
;;;; functions are symbols resolved only when a selected phase runs.  This
;;;; keeps the kernel from linking upward into the mind or adapters.
;;;;
;;;; Loading defines descriptors and init actions.  It performs no I/O,
;;;; starts no worker and selects nothing; INITIALIZE owns those transitions.

(in-package :agent)

(export '(define-cognition-runtime register-cognition-runtime
          cognition-runtime-configure cognition-runtime-install
          cognition-runtime-restore cognition-runtime-verify
          cognition-runtime-start cognition-runtime-stop
          cognition-runtime-selected-p cognition-runtime-selected-name
          cognition-runtime-start-owned-worker submit-stimulus
          cognition-runtime-report cognition-runtime-assert
          *cognition-runtime-configured-name*
          *cognition-runtime-selected-name*
          *cognition-runtime-pinned-revision*
          *cognition-runtime-state*
          *cognition-runtime-in-flight*
          *cognition-runtime-projection*))

(defparameter *cognition-runtime-schema-version* 1)

(defstruct (cognition-runtime-descriptor
             (:constructor %make-cognition-runtime-descriptor))
  name revision owner entry configure install restore verify start stop report
  owned-workers required-capabilities recovery-probe)

(defvar *cognition-runtime-registry* (make-hash-table :test #'eq))
(defvar *cognition-runtime-configured-name* nil)
(defvar *cognition-runtime-selected-name* nil)
(defvar *cognition-runtime-pinned-revision* nil)
(defvar *cognition-runtime-pinned-descriptor* nil)
(defvar *cognition-runtime-state* :unconfigured)
(defvar *cognition-runtime-in-flight* 0)
(defvar *cognition-runtime-verified-p* nil)
(defvar *cognition-runtime-last-error* nil)
(defvar *cognition-runtime-projection* nil
  "Selected adapter's rebuildable projection, when it has one.")
(defvar *cognition-runtime-lock*
  (bt:make-lock "cognition-runtime"))

(defun %cognition-runtime-function (designator &key required)
  (cond ((functionp designator) designator)
        ((and (symbolp designator) (fboundp designator))
         (symbol-function designator))
        (required
         (error "Cognition runtime function ~s is unavailable" designator))
        (t nil)))

(defun %cognition-runtime-name (value)
  (cond ((keywordp value) value)
        ((and (symbolp value) value)
         (intern (string-upcase (symbol-name value)) :keyword))
        ((and (stringp value) (plusp (length value)))
         (intern (string-upcase value) :keyword))
        (t nil)))

(defun %cognition-runtime-worker-name (value)
  (string-downcase (string value)))

(defun %cognition-runtime-normalize-workers (workers)
  (mapcar
   (lambda (row)
     (unless (and (listp row) (= 2 (length row)))
       (error "Runtime worker must be (name liveness-probe), got ~s" row))
     (list (%cognition-runtime-worker-name (first row)) (second row)))
   workers))

(defun register-cognition-runtime
    (name &key revision owner entry configure install restore verify start stop
               report owned-workers required-capabilities recovery-probe)
  "Define or reload one runtime descriptor without selecting or starting it."
  (let* ((key (%cognition-runtime-name name))
         (workers (%cognition-runtime-normalize-workers owned-workers)))
    (unless key (error "Cognition runtime name must be non-empty: ~s" name))
    (unless (and (stringp revision) (plusp (length revision)))
      (error "Cognition runtime ~s needs a non-empty revision" key))
    (unless (and (stringp owner) (plusp (length owner)))
      (error "Cognition runtime ~s needs a named owner" key))
    (unless (symbolp entry)
      (error "Cognition runtime ~s entry must be a function symbol" key))
    (bt:with-lock-held (*cognition-runtime-lock*)
      (when (and *cognition-runtime-pinned-descriptor*
                 (eq key *cognition-runtime-selected-name*))
        ;; Even an apparently identical reload allocates a new descriptor and
        ;; could change a phase, capability or worker while retaining the same
        ;; revision/entry pair. Reject at the mutation boundary; Q2 permits
        ;; runtime changes only on a fresh boot.
        (error "Cannot reload installed cognition runtime ~s; fresh boot required"
               key))
      (setf (gethash key *cognition-runtime-registry*)
            (%make-cognition-runtime-descriptor
             :name key :revision revision :owner owner :entry entry
             :configure configure :install install :restore restore
             :verify verify :start start :stop stop :report report
             :owned-workers workers
             :required-capabilities (copy-list required-capabilities)
             :recovery-probe recovery-probe)))
    key))

(defmacro define-cognition-runtime
    (name &key revision owner entry configure install restore verify start stop
               report owned-workers required-capabilities recovery-probe)
  "Declare one cognition runtime. Declaration has no lifecycle side effect."
  `(register-cognition-runtime
    ,name :revision ,revision :owner ,owner :entry ',entry
    :configure ',configure :install ',install :restore ',restore
    :verify ',verify :start ',start :stop ',stop :report ',report
    :owned-workers ',owned-workers
    :required-capabilities ',required-capabilities
    :recovery-probe ',recovery-probe))

(defun cognition-runtime-selected-name ()
  *cognition-runtime-selected-name*)

(defun cognition-runtime-selected-p (name)
  (eq (%cognition-runtime-name name) *cognition-runtime-selected-name*))

(defun %cognition-runtime-descriptor (&optional
                                        (name *cognition-runtime-selected-name*))
  (and name (gethash name *cognition-runtime-registry*)))

(defun %cognition-runtime-call-phase (descriptor accessor)
  (let* ((designator (funcall accessor descriptor))
         (function (%cognition-runtime-function designator)))
    (when function (funcall function))))

(defun cognition-runtime-configure (&optional (requested nil requested-p))
  "Resolve deployment selection, defaulting compatibly to :AUTO.

An explicitly empty value is an error. Once installed, even an otherwise
valid different selection is refused; Q2 supports transitions only by fresh
boot, which is the fail-closed subset of the quiescence contract."
  (let* ((environment (and (not requested-p)
                           (uiop:getenv "PAI_COGNITION_RUNTIME")))
         (raw (cond (requested-p requested)
                    ((null environment) "auto")
                    (t environment)))
         (name (%cognition-runtime-name raw))
         (descriptor (and name (gethash name *cognition-runtime-registry*))))
    (unless name
      (error "PAI_COGNITION_RUNTIME is empty or invalid"))
    (unless descriptor
      (error "Unknown cognition runtime ~s" raw))
    (bt:with-lock-held (*cognition-runtime-lock*)
      (when (and *cognition-runtime-selected-name*
                 (not (eq name *cognition-runtime-selected-name*)))
        (error "Cognition runtime is installed as ~s; fresh boot required"
               *cognition-runtime-selected-name*))
      (when (plusp *cognition-runtime-in-flight*)
        (error "Cannot select a cognition runtime with ~d operation~:p in flight"
               *cognition-runtime-in-flight*))
      (setf *cognition-runtime-configured-name* name
            *cognition-runtime-state* :configured
            *cognition-runtime-verified-p* nil
            *cognition-runtime-last-error* nil))
    (%cognition-runtime-call-phase
     descriptor #'cognition-runtime-descriptor-configure)
    name))

(defun cognition-runtime-install ()
  (let ((descriptor (%cognition-runtime-descriptor
                     *cognition-runtime-configured-name*)))
    (unless descriptor
      (error "No configured cognition runtime to install"))
    (bt:with-lock-held (*cognition-runtime-lock*)
      (cond
        (*cognition-runtime-selected-name*
         (unless (and (eq *cognition-runtime-selected-name*
                          *cognition-runtime-configured-name*)
                      (string= *cognition-runtime-pinned-revision*
                               (cognition-runtime-descriptor-revision descriptor)))
           (error "Installed cognition runtime differs from configuration")))
        (t
         (setf *cognition-runtime-selected-name*
               *cognition-runtime-configured-name*
               *cognition-runtime-pinned-revision*
               (cognition-runtime-descriptor-revision descriptor)
               *cognition-runtime-pinned-descriptor* descriptor)))
      (setf *cognition-runtime-state* :installed))
    (%cognition-runtime-call-phase
     descriptor #'cognition-runtime-descriptor-install)
    *cognition-runtime-selected-name*))

(defun cognition-runtime-restore ()
  (let ((descriptor (%cognition-runtime-descriptor)))
    (unless descriptor (error "No installed cognition runtime to restore"))
    (let ((projection
            (%cognition-runtime-call-phase
             descriptor #'cognition-runtime-descriptor-restore)))
      (when projection (setf *cognition-runtime-projection* projection)))
    (setf *cognition-runtime-state* :restored)
    t))

(defun %cognition-runtime-worker-live-p (row)
  (let ((function (%cognition-runtime-function (second row))))
    (and function (ignore-errors (funcall function)) t)))

(defun %cognition-runtime-live-conflicts ()
  (let ((conflicts '()))
    (maphash
     (lambda (name descriptor)
       (unless (eq name *cognition-runtime-selected-name*)
         (dolist (worker (cognition-runtime-descriptor-owned-workers descriptor))
           (when (%cognition-runtime-worker-live-p worker)
             (push (list name (first worker)) conflicts)))))
     *cognition-runtime-registry*)
    (nreverse conflicts)))

(defun cognition-runtime-assert ()
  "Fail on missing selection, drift, descriptor mutation or a second owner."
  (let* ((selected *cognition-runtime-selected-name*)
         (descriptor (%cognition-runtime-descriptor selected))
         (conflicts (%cognition-runtime-live-conflicts)))
    (unless *cognition-runtime-configured-name*
      (error "No cognition runtime configured"))
    (unless selected (error "No cognition runtime installed"))
    (unless (eq selected *cognition-runtime-configured-name*)
      (error "Cognition runtime configured/live drift: ~s versus ~s"
             *cognition-runtime-configured-name* selected))
    (unless descriptor (error "Installed cognition runtime ~s is unknown" selected))
    (unless (and *cognition-runtime-pinned-descriptor*
                 (eq descriptor *cognition-runtime-pinned-descriptor*)
                 (string= *cognition-runtime-pinned-revision*
                          (cognition-runtime-descriptor-revision descriptor)))
      (error "Installed cognition runtime descriptor/revision drifted"))
    (dolist (capability
             (cognition-runtime-descriptor-required-capabilities descriptor))
      (unless (and (symbolp capability) (fboundp capability))
        (error "Cognition runtime ~s requires unavailable capability ~s"
               selected capability)))
    (when conflicts
      (error "Unselected cognition workers live: ~s" conflicts))
    (let ((probe (%cognition-runtime-function
                  (cognition-runtime-descriptor-recovery-probe descriptor))))
      (when (and probe (not (funcall probe)))
        (error "Cognition runtime ~s recovery probe failed" selected)))
    t))

(defun cognition-runtime-verify ()
  (handler-case
      (let* ((descriptor (%cognition-runtime-descriptor))
             (verify (and descriptor
                          (%cognition-runtime-function
                           (cognition-runtime-descriptor-verify descriptor)))))
        (cognition-runtime-assert)
        (when (and verify (not (funcall verify)))
          (error "Cognition runtime ~s verification failed"
                 *cognition-runtime-selected-name*))
        (setf *cognition-runtime-verified-p* t
              *cognition-runtime-state* :verified
              *cognition-runtime-last-error* nil)
        t)
    (error (condition)
      (setf *cognition-runtime-verified-p* nil
            *cognition-runtime-state* :failed-safe
            *cognition-runtime-last-error* (format nil "~a" condition))
      (error condition))))

(defun cognition-runtime-start ()
  (unless *cognition-runtime-verified-p*
    (error "Cognition runtime cannot start before verification"))
  (let* ((descriptor (%cognition-runtime-descriptor))
         (result (%cognition-runtime-call-phase
                  descriptor #'cognition-runtime-descriptor-start)))
    (setf *cognition-runtime-state*
          (or (and (keywordp result) result)
              (if (eq *cognition-runtime-selected-name* :conscious-state)
                  :idle :running)))
    *cognition-runtime-state*))

(defun cognition-runtime-stop ()
  (when (plusp *cognition-runtime-in-flight*)
    (error "Cannot stop cognition runtime with work in flight"))
  (let ((descriptor (%cognition-runtime-descriptor)))
    (when descriptor
      (%cognition-runtime-call-phase
       descriptor #'cognition-runtime-descriptor-stop)))
  (setf *cognition-runtime-state* :stopped
        *cognition-runtime-verified-p* nil)
  :stopped)

(defun %cognition-runtime-worker-declared-p (descriptor worker-name)
  (find (%cognition-runtime-worker-name worker-name)
        (cognition-runtime-descriptor-owned-workers descriptor)
        :key #'first :test #'string=))

(defun cognition-runtime-start-owned-worker (owner worker-name start-function)
  "Run START-FUNCTION only for the selected owner.

An undeclared worker is an error when its owner is selected. An unselected
owner is a deliberate no-op so existing init action names and ordering remain
stable while only the chosen runtime's workers start."
  (let* ((owner-name (%cognition-runtime-name owner))
         (descriptor (gethash owner-name *cognition-runtime-registry*)))
    (unless descriptor (error "Unknown worker owner ~s" owner))
    (if (not (eq owner-name *cognition-runtime-selected-name*))
        :not-selected
        (progn
          (unless (%cognition-runtime-worker-declared-p descriptor worker-name)
            (error "Worker ~s is not declared for cognition runtime ~s"
                   worker-name owner-name))
          (unless *cognition-runtime-verified-p*
            (error "Cannot start cognition worker before runtime verification"))
          (funcall start-function)))))

(defun submit-stimulus (stimulus &key (kind :user-message) metadata
                                      wait-for-public-result)
  "Submit through the one selected cognition entry boundary.

The adapter owns interpretation. This function owns exclusivity and preserves
all values from the adapter, which is required for byte/behavior-compatible
AUTO dispatch."
  (let (entry)
    (bt:with-lock-held (*cognition-runtime-lock*)
      (unless *cognition-runtime-verified-p*
        (error "Cognition runtime is not verified"))
      (let ((descriptor (%cognition-runtime-descriptor)))
        (unless descriptor (error "No selected cognition runtime"))
        (setf entry
              (%cognition-runtime-function
               (cognition-runtime-descriptor-entry descriptor) :required t)))
      (incf *cognition-runtime-in-flight*))
    (unwind-protect
         (multiple-value-prog1
             (funcall entry stimulus
                      :kind kind
                      :metadata metadata
                      :wait-for-public-result wait-for-public-result))
      (bt:with-lock-held (*cognition-runtime-lock*)
        (decf *cognition-runtime-in-flight*)))))

(defun %cognition-runtime-worker-report (runtime descriptor)
  (coerce
   (mapcar (lambda (worker)
             (obj "name" (first worker)
                  "runtime" (string-downcase (symbol-name runtime))
                  "live" (if (%cognition-runtime-worker-live-p worker) t nil)))
           (cognition-runtime-descriptor-owned-workers descriptor))
   'vector))

(defun cognition-runtime-report ()
  "Sanitized measured report. Worker liveness is recomputed, not declared."
  (let* ((descriptor (%cognition-runtime-descriptor))
         (configured *cognition-runtime-configured-name*)
         (selected *cognition-runtime-selected-name*)
         (drift (cond ((or (null configured) (null selected)) "unavailable")
                      ((eq configured selected) "match")
                      (t "mismatch")))
         (conflicts (%cognition-runtime-live-conflicts))
         (adapter-report
           (and descriptor
                (let ((function (%cognition-runtime-function
                                 (cognition-runtime-descriptor-report descriptor))))
                  (and function (ignore-errors (funcall function)))))))
    (obj
     "schema_version" *cognition-runtime-schema-version*
     "configured" (if configured
                       (string-downcase (symbol-name configured)) :null)
     "selected" (if selected (string-downcase (symbol-name selected)) :null)
     "revision" (or *cognition-runtime-pinned-revision* :null)
     "owner" (if descriptor
                 (cognition-runtime-descriptor-owner descriptor) :null)
     "state" (string-downcase (symbol-name *cognition-runtime-state*))
     "verified" (if *cognition-runtime-verified-p* t nil)
     "in_flight" *cognition-runtime-in-flight*
     "selection_drift" drift
     "conflict_count" (length conflicts)
     "conflicts" (coerce
                   (mapcar (lambda (row)
                             (obj "runtime"
                                  (string-downcase (symbol-name (first row)))
                                  "worker" (second row)))
                           conflicts)
                   'vector)
     "owned_workers" (if descriptor
                         (%cognition-runtime-worker-report selected descriptor)
                         (vector))
     "last_committed_pulse" :null
     "degraded" (if (eq *cognition-runtime-state* :failed-safe) t nil)
     "last_error" (or *cognition-runtime-last-error* :null)
     "adapter" (or adapter-report :null))))

(define-init :configure cognition-runtime-configure
    "Resolve the one deployment-selected cognition runtime."
  (cognition-runtime-configure))

(define-init :install cognition-runtime-install
    "Pin the selected cognition runtime descriptor and revision."
  (cognition-runtime-install))

(define-init :restore cognition-runtime-restore
    "Restore the selected runtime's rebuildable projection."
  (cognition-runtime-restore))

(define-init :verify cognition-runtime-verify
    "Fail closed on cognition selection drift, conflict or recovery failure."
  (cognition-runtime-verify))

(define-init :start cognition-runtime-start
    "Start only the selected cognition runtime lifecycle."
  (cognition-runtime-start))
