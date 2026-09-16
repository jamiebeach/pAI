;;;; kernel-tool-dispatch-runtime.lisp -- generic dev-only cutover.
;;;; Selects a closed enabled set through validated plans/bindings. Every
;;;; disabled, unknown or malformed call crosses the captured legacy chain once.

(in-package :agent)

(export '(tool-dispatch-runtime-install tool-dispatch-runtime-uninstall
          tool-dispatch-runtime-report))

(defvar *tool-dispatch-runtime-lock* (bt:make-lock "tool-dispatch-runtime"))
(defvar *tool-dispatch-runtime-installed* nil)
(defvar *tool-dispatch-runtime-incumbent* nil)
(defvar *tool-dispatch-runtime-wrapper* nil)
(defvar *tool-dispatch-runtime-enabled-handler-ids* nil)
(defvar *tool-dispatch-runtime-selected-counts* (make-hash-table :test #'equal))
(defvar *tool-dispatch-runtime-fallthrough-counts* (make-hash-table :test #'equal))
(defvar *tool-dispatch-runtime-composition-errors* 0)

(declaim (ftype function tool-dispatch-runtime-report))

(defun %tool-dispatch-runtime-increment-table (table key)
  (bt:with-lock-held (*tool-dispatch-runtime-lock*)
    (incf (gethash key table 0))))

(defun %tool-dispatch-runtime-increment-error ()
  (bt:with-lock-held (*tool-dispatch-runtime-lock*)
    (incf *tool-dispatch-runtime-composition-errors*)))

(defun %tool-dispatch-runtime-vector-list (value)
  (if (vectorp value) (coerce value 'list) value))

(defun %tool-dispatch-runtime-plan-valid-p (plan handler-id)
  (let ((around (%tool-dispatch-runtime-vector-list
                 (gethash "around_stages" plan)))
        (before (%tool-dispatch-runtime-vector-list
                 (gethash "before_stages" plan)))
        (after (%tool-dispatch-runtime-vector-list
                (gethash "after_stages" plan))))
    (and (string= (gethash "status" plan) "resolved")
         (string= (gethash "handler_id" plan) handler-id)
         (or (equal around '("timing"))
             (and (string= handler-id "propose-loop")
                  (equal around '("timing" "proposal-provenance"))))
         (equal before '("tool-call-event"))
         (equal after '("modulator-appraisal" "tool-result-event")))))

(defun %tool-dispatch-runtime-call-handler (binding tool-call provenance-p)
  (let ((port (symbol-function (getf binding :port))))
    (flet ((invoke (call) (funcall port call)))
      (observe-tool-result-appraisal
       (if provenance-p
           (call-with-proposal-provenance tool-call #'invoke)
           (invoke tool-call))))))

(defun %tool-dispatch-runtime-compose (binding plan tool-call)
  (let* ((handler-id (getf binding :handler-id))
         (name (getf binding :external-name))
         (around (%tool-dispatch-runtime-vector-list
                  (gethash "around_stages" plan)))
         (provenance-p (member "proposal-provenance" around :test #'string=)))
    (unless (%tool-dispatch-runtime-plan-valid-p plan handler-id)
      (error "Kernel observer plan drift for ~a." handler-id))
    (%tool-dispatch-runtime-increment-table
     *tool-dispatch-runtime-selected-counts* handler-id)
    (handler-bind ((error (lambda (condition)
                            (declare (ignore condition))
                            (%tool-dispatch-runtime-increment-error))))
      (call-with-timing-span
       (format nil "tool.~a" name)
       (lambda ()
         (call-with-tool-event-observation
          tool-call
          (lambda (call)
            (%tool-dispatch-runtime-call-handler binding call provenance-p))))
       :attributes (obj "tool" name)))))

(defun %tool-dispatch-runtime-execute (tool-call)
  (let* ((function (and (hash-table-p tool-call)
                        (gethash "function" tool-call)))
         (name (and (hash-table-p function) (gethash "name" function)))
         (report (tool-dispatch-shadow-inspect name))
         (plan (gethash "registry_plan" report))
         (advertisement (gethash "incumbent_advertisement" report))
         (handler-id (and (string= (gethash "status" plan) "resolved")
                          (gethash "handler_id" plan)))
         (binding (and handler-id (tool-handler-binding-lookup handler-id))))
    (if (and binding (= (gethash "count" advertisement) 1)
             (member handler-id *tool-dispatch-runtime-enabled-handler-ids*
                     :test #'string=))
        (%tool-dispatch-runtime-compose binding plan tool-call)
        (progn
          (%tool-dispatch-runtime-increment-table
           *tool-dispatch-runtime-fallthrough-counts*
           (or handler-id "unresolved"))
          (multiple-value-call #'values
            (funcall *tool-dispatch-runtime-incumbent* tool-call))))))

(defun %tool-dispatch-runtime-normalize-enabled (enabled-handler-ids)
  (unless (and (listp enabled-handler-ids)
               (= (length enabled-handler-ids)
                  (length (remove-duplicates enabled-handler-ids
                                             :test #'string=))))
    (error "Enabled handler IDs must be a unique list."))
  (dolist (id enabled-handler-ids)
    (let ((binding (tool-handler-binding-lookup id)))
      (unless binding (error "Unknown enabled handler ID ~a." id))
      (let* ((report (tool-dispatch-shadow-inspect
                      (getf binding :external-name)))
             (advertisement (gethash "incumbent_advertisement" report)))
        (unless (= (gethash "count" advertisement) 1)
          (error "Enabled handler ~a is not advertised exactly once." id)))))
  (copy-list enabled-handler-ids))

(defun tool-dispatch-runtime-install
    (&optional (enabled-handler-ids
                 (mapcar (lambda (row) (getf row :handler-id))
                         (tool-handler-binding-rows))))
  "Install the identity-checked generic development dispatcher."
  (unless (fboundp 'execute)
    (error "Cannot install kernel tool dispatch: EXECUTE is unavailable."))
  (dolist (port '(call-with-timing-span call-with-tool-event-observation
                  observe-tool-result-appraisal call-with-proposal-provenance))
    (unless (fboundp port)
      (error "Cannot install kernel tool dispatch: required port ~a is unavailable."
             port)))
  (tool-handler-bindings-initialize)
  (let ((enabled (%tool-dispatch-runtime-normalize-enabled enabled-handler-ids))
        (current (fdefinition 'execute)))
    (cond
      (*tool-dispatch-runtime-installed*
       (unless (and *tool-dispatch-runtime-wrapper*
                    (eq current *tool-dispatch-runtime-wrapper*)
                    (equal enabled *tool-dispatch-runtime-enabled-handler-ids*))
         (error "Cannot install kernel tool dispatch: ownership or enabled-set conflict.")))
      (t
       (let ((wrapper (fdefinition '%tool-dispatch-runtime-execute)))
         (setf *tool-dispatch-runtime-incumbent* current
               *tool-dispatch-runtime-wrapper* wrapper
               *tool-dispatch-runtime-enabled-handler-ids* enabled
               (fdefinition 'execute) wrapper
               *tool-dispatch-runtime-installed* t)))))
  (tool-dispatch-runtime-report))

(defun tool-dispatch-runtime-uninstall ()
  "Restore the exact captured incumbent; outer must be removed first."
  (when *tool-dispatch-runtime-installed*
    (unless (and (fboundp 'execute) *tool-dispatch-runtime-wrapper*
                 (eq (fdefinition 'execute) *tool-dispatch-runtime-wrapper*))
      (error "Cannot uninstall kernel tool dispatch: EXECUTE ownership conflict."))
    (setf (fdefinition 'execute) *tool-dispatch-runtime-incumbent*
          *tool-dispatch-runtime-installed* nil
          *tool-dispatch-runtime-incumbent* nil
          *tool-dispatch-runtime-wrapper* nil
          *tool-dispatch-runtime-enabled-handler-ids* nil))
  (tool-dispatch-runtime-report))

(defun %tool-dispatch-runtime-owned-p (current)
  (and *tool-dispatch-runtime-installed* *tool-dispatch-runtime-wrapper*
       (or (eq current *tool-dispatch-runtime-wrapper*)
           (and (boundp '*tool-dispatch-shadow-runtime-installed*)
                (symbol-value '*tool-dispatch-shadow-runtime-installed*)
                (boundp '*tool-dispatch-shadow-runtime-incumbent*)
                (eq (symbol-value '*tool-dispatch-shadow-runtime-incumbent*)
                    *tool-dispatch-runtime-wrapper*)
                (boundp '*tool-dispatch-shadow-runtime-wrapper*)
                (eq current
                    (symbol-value '*tool-dispatch-shadow-runtime-wrapper*))))))

(defun %tool-dispatch-runtime-count-rows (table)
  (coerce
   (mapcar (lambda (row)
             (obj "handler_id" (getf row :handler-id)
                  "external_name" (getf row :external-name)
                  "group" (getf row :group)
                  "count" (gethash (getf row :handler-id) table 0)))
           (tool-handler-binding-rows))
   'vector))

(defun tool-dispatch-runtime-report ()
  (let* ((current (and (fboundp 'execute) (fdefinition 'execute)))
         (installed (%tool-dispatch-runtime-owned-p current))
         (conflict (and *tool-dispatch-runtime-installed* (not installed))))
    (bt:with-lock-held (*tool-dispatch-runtime-lock*)
      (obj "schema_version" 2
           "status" (cond (conflict "conflict")
                          (installed "installed") (t "not-installed"))
           "installed" (if installed t nil)
           "ownership_conflict" (if conflict t nil)
           "enabled_handler_ids"
           (coerce (copy-list *tool-dispatch-runtime-enabled-handler-ids*) 'vector)
           "selected_counts"
           (%tool-dispatch-runtime-count-rows *tool-dispatch-runtime-selected-counts*)
           "fallthrough_counts"
           (%tool-dispatch-runtime-count-rows *tool-dispatch-runtime-fallthrough-counts*)
           "unresolved_fallthrough_count"
           (gethash "unresolved" *tool-dispatch-runtime-fallthrough-counts* 0)
           "composition_errors" *tool-dispatch-runtime-composition-errors*
           "production_loaded" nil))))
