;;;; kernel-tool-handler-bindings.lisp -- closed handler binding table.
;;;; Literal allowlisted symbols only; no request-derived lookup or interning.

(in-package :agent)

(export '(tool-handler-bindings-initialize tool-handler-binding-lookup
          tool-handler-bindings-report tool-handler-binding-rows))

(defparameter *tool-handler-binding-table*
  '((:handler-id "lisp-eval" :external-name "lisp-eval"
     :port self-mod-tool-handle :group "high-authority")
    (:handler-id "propose-loop" :external-name "propose-loop"
     :port self-mod-tool-handle :group "high-authority")
    (:handler-id "web-search" :external-name "web-search"
     :port pai-enhancements-tool-handle :group "external-provider")
    (:handler-id "broadcast-image" :external-name "broadcast-image"
     :port web-terminal-tool-handle :group "contained-dev-mutation")
    (:handler-id "upload-reference-image" :external-name "upload-reference-image"
     :port runware-tool-handle :group "external-provider")
    (:handler-id "generate-image" :external-name "generate-image"
     :port runware-tool-handle :group "external-provider")
    (:handler-id "view-image" :external-name "view-image"
     :port runware-tool-handle :group "external-provider")
    (:handler-id "find-state-files" :external-name "find-state-files"
     :port runware-tool-handle :group "local-read")
    (:handler-id "web-fetch" :external-name "web-fetch"
     :port bounded-work-tool-handle :group "external-provider")
    (:handler-id "write-deliverable" :external-name "write-deliverable"
     :port bounded-work-tool-handle :group "contained-dev-mutation")
    (:handler-id "read-deliverable" :external-name "read-deliverable"
     :port bounded-work-tool-handle :group "local-read")
    (:handler-id "search-memory" :external-name "search-memory"
     :port memory-search-tool-handle :group "local-read")
    (:handler-id "hold-near-term-thought" :external-name "hold-near-term-thought"
     :port near-term-intention-tool-handle :group "contained-dev-mutation")))

(defvar *tool-handler-bindings-initialized* nil)

(declaim (ftype function tool-handler-bindings-report))

(defun %tool-handler-binding-copy (row)
  (list :handler-id (getf row :handler-id)
        :external-name (getf row :external-name)
        :port (getf row :port)
        :group (getf row :group)))

(defun tool-handler-binding-rows ()
  (mapcar #'%tool-handler-binding-copy *tool-handler-binding-table*))

(defun tool-handler-binding-lookup (handler-id)
  (unless *tool-handler-bindings-initialized*
    (error "Tool-handler bindings are not initialized."))
  (find handler-id *tool-handler-binding-table* :test #'string=
        :key (lambda (row) (getf row :handler-id))))

(defun tool-handler-bindings-initialize ()
  "Validate the closed table against the initialized facade."
  (unless (fboundp 'tool-dispatch-shadow-inspect)
    (error "Cannot initialize handler bindings: facade is unavailable."))
  (unless (= (length *tool-handler-binding-table*) 13)
    (error "Handler binding table must contain exactly thirteen rows."))
  (let ((ids (mapcar (lambda (row) (getf row :handler-id))
                     *tool-handler-binding-table*))
        (names (mapcar (lambda (row) (getf row :external-name))
                       *tool-handler-binding-table*)))
    (unless (= (length ids) (length (remove-duplicates ids :test #'string=)))
      (error "Handler binding IDs must be unique."))
    (unless (= (length names) (length (remove-duplicates names :test #'string=)))
      (error "Handler binding names must be unique.")))
  (dolist (row *tool-handler-binding-table*)
    (let* ((name (getf row :external-name))
           (report (tool-dispatch-shadow-inspect name))
           (plan (gethash "registry_plan" report))
           (port (getf row :port)))
      (unless (and (string= (gethash "status" plan) "resolved")
                   (string= (gethash "external_name" plan) name)
                   (string= (gethash "handler_id" plan)
                            (getf row :handler-id)))
        (error "Handler binding does not match plan for ~a." name))
      (unless (and (symbolp port) (fboundp port))
        (error "Handler binding port is unavailable for ~a." name))))
  (setf *tool-handler-bindings-initialized* t)
  (tool-handler-bindings-report))

(defun tool-handler-bindings-report ()
  (obj "schema_version" 1
       "status" (if *tool-handler-bindings-initialized* "initialized"
                    "not-initialized")
       "initialized" (if *tool-handler-bindings-initialized* t nil)
       "binding_count" (length *tool-handler-binding-table*)
       "handler_ids"
       (coerce (mapcar (lambda (row) (getf row :handler-id))
                       *tool-handler-binding-table*) 'vector)
       "groups"
       (coerce (remove-duplicates
                (mapcar (lambda (row) (getf row :group))
                        *tool-handler-binding-table*) :test #'string=)
               'vector)
       "production_loaded" nil))
