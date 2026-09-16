;;;; kernel-tool-dispatch-shadow-runtime.lisp -- installed dev shadow.
;;;; Loaded only by the labelled Rdev0 workbench lifecycle. The incumbent
;;;; EXECUTE function remains the sole dispatcher and is invoked exactly once.

(in-package :agent)

(export '(tool-dispatch-shadow-runtime-install
          tool-dispatch-shadow-runtime-uninstall
          tool-dispatch-shadow-runtime-report
          tool-dispatch-shadow-runtime-recent))

(declaim (ftype function tool-dispatch-shadow-runtime-report))

(defparameter *tool-dispatch-shadow-runtime-capacity* 64)
(defvar *tool-dispatch-shadow-runtime-lock*
  (bt:make-lock "tool-dispatch-shadow-runtime"))
(defvar *tool-dispatch-shadow-runtime-observations* nil)
(defvar *tool-dispatch-shadow-runtime-sequence* 0)
(defvar *tool-dispatch-shadow-runtime-installed* nil)
(defvar *tool-dispatch-shadow-runtime-incumbent* nil)
(defvar *tool-dispatch-shadow-runtime-wrapper* nil)

(defun %tool-dispatch-shadow-runtime-copy (observation)
  (obj "sequence" (gethash "sequence" observation)
       "classification" (gethash "classification" observation)
       "registry_status" (gethash "registry_status" observation)
       "handler_id" (gethash "handler_id" observation)
       "recognized_name" (gethash "recognized_name" observation)
       "advertisement_count" (gethash "advertisement_count" observation)))

(defun %tool-dispatch-shadow-runtime-sanitize (report)
  (let* ((plan (gethash "registry_plan" report))
         (advertisement (gethash "incumbent_advertisement" report))
         (status (gethash "status" plan))
         (advertised (gethash "advertised" advertisement))
         (resolved (string= status "resolved"))
         (name (if (or resolved advertised)
                   (gethash "exact_name" advertisement) :null)))
    (obj "classification" (gethash "classification" report)
         "registry_status" status
         "handler_id" (if resolved (gethash "handler_id" plan) :null)
         "recognized_name" name
         "advertisement_count" (gethash "count" advertisement))))

(defun %tool-dispatch-shadow-runtime-record (report)
  (let ((sanitized (%tool-dispatch-shadow-runtime-sanitize report)))
    (bt:with-lock-held (*tool-dispatch-shadow-runtime-lock*)
      (setf (gethash "sequence" sanitized)
            (incf *tool-dispatch-shadow-runtime-sequence*))
      (push sanitized *tool-dispatch-shadow-runtime-observations*)
      (when (> (length *tool-dispatch-shadow-runtime-observations*)
               *tool-dispatch-shadow-runtime-capacity*)
        (setf *tool-dispatch-shadow-runtime-observations*
              (subseq *tool-dispatch-shadow-runtime-observations*
                      0 *tool-dispatch-shadow-runtime-capacity*)))))
  nil)

(defparameter *tool-dispatch-shadow-runtime-sink-function*
  #'%tool-dispatch-shadow-runtime-record)

(defun %tool-dispatch-shadow-runtime-execute (tool-call)
  (call-with-tool-dispatch-shadow
   tool-call
   (lambda (call)
     (funcall *tool-dispatch-shadow-runtime-incumbent* call))
   :sink *tool-dispatch-shadow-runtime-sink-function*))

(defun tool-dispatch-shadow-runtime-install ()
  "Install one identity-checked outer development observer over EXECUTE."
  (unless (fboundp 'execute)
    (error "Cannot install tool-dispatch shadow: EXECUTE is unavailable."))
  (let ((current (fdefinition 'execute)))
    (cond
      (*tool-dispatch-shadow-runtime-installed*
       (unless (and *tool-dispatch-shadow-runtime-wrapper*
                    (eq current *tool-dispatch-shadow-runtime-wrapper*))
         (error "Cannot install tool-dispatch shadow: EXECUTE ownership conflict.")))
      (t
       (let ((wrapper (fdefinition '%tool-dispatch-shadow-runtime-execute)))
         (setf *tool-dispatch-shadow-runtime-incumbent* current
               *tool-dispatch-shadow-runtime-wrapper* wrapper
               (fdefinition 'execute) wrapper
               *tool-dispatch-shadow-runtime-installed* t)))))
  (tool-dispatch-shadow-runtime-report))

(defun tool-dispatch-shadow-runtime-uninstall ()
  "Restore the exact captured incumbent, or fail without overwriting a new owner."
  (when *tool-dispatch-shadow-runtime-installed*
    (unless (and (fboundp 'execute)
                 *tool-dispatch-shadow-runtime-wrapper*
                 (eq (fdefinition 'execute)
                     *tool-dispatch-shadow-runtime-wrapper*))
      (error "Cannot uninstall tool-dispatch shadow: EXECUTE ownership conflict."))
    (setf (fdefinition 'execute) *tool-dispatch-shadow-runtime-incumbent*
          *tool-dispatch-shadow-runtime-installed* nil
          *tool-dispatch-shadow-runtime-incumbent* nil
          *tool-dispatch-shadow-runtime-wrapper* nil))
  (tool-dispatch-shadow-runtime-report))

(defun tool-dispatch-shadow-runtime-recent ()
  (bt:with-lock-held (*tool-dispatch-shadow-runtime-lock*)
    (coerce (mapcar #'%tool-dispatch-shadow-runtime-copy
                    *tool-dispatch-shadow-runtime-observations*)
            'vector)))

(defun tool-dispatch-shadow-runtime-report ()
  (let* ((current (and (fboundp 'execute) (fdefinition 'execute)))
         (installed (and *tool-dispatch-shadow-runtime-installed*
                         *tool-dispatch-shadow-runtime-wrapper*
                         (eq current *tool-dispatch-shadow-runtime-wrapper*)))
         (conflict (and *tool-dispatch-shadow-runtime-installed*
                        (not installed))))
    (bt:with-lock-held (*tool-dispatch-shadow-runtime-lock*)
      (obj "schema_version" 1 "status" (cond (conflict "conflict")
                                              (installed "installed")
                                              (t "not-installed"))
           "installed" (if installed t nil)
           "ownership_conflict" (if conflict t nil)
           "capacity" *tool-dispatch-shadow-runtime-capacity*
           "observed_count" (length *tool-dispatch-shadow-runtime-observations*)
           "sequence" *tool-dispatch-shadow-runtime-sequence*
           "durable_writes" 0 "execution_trigger_available" nil
           "production_loaded" nil))))
