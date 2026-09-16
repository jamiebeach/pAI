;;;; kernel-tool-dispatch-bootstrap.lisp -- cold kernel installation.

(in-package :agent)

;; These cold-kernel capabilities are loaded explicitly from the application
;; root only when kernel dispatch is selected. Declare the callable contract so
;; compiling the legacy-inactive bootstrap does not report false undefined
;; functions.
(declaim (ftype function tool-dispatch-shadow-initialize
                        tool-dispatch-shadow-inspect
                        tool-handler-binding-lookup
                        tool-handler-bindings-initialize
                        tool-dispatch-runtime-install
                        tool-dispatch-runtime-report))

(export '(kernel-tool-dispatch-bootstrap
          kernel-tool-dispatch-bootstrap-report))

(defvar *kernel-tool-dispatch-bootstrap-complete* nil)
(defvar *kernel-tool-dispatch-bootstrap-report* nil)

(defun %kernel-tool-dispatch-state-path (name)
  (let ((root (or (uiop:getenv "PAI_APPLICATION_ROOT")
                  (uiop:getenv "PAI_R3A_APPLICATION_ROOT"))))
    (merge-pathnames name
                     (if (and root (plusp (length root)))
                         (pathname root)
                         #P"/agent/state/"))))

(defun %kernel-tool-dispatch-load-required (name)
  (let ((path (%kernel-tool-dispatch-state-path name)))
    (unless (probe-file path)
      (error "required cold-boot source is absent: ~a" path))
    (load path)))

(defun %kernel-tool-dispatch-dev-catalogue-complete ()
  ;; The workbench deliberately exposes the mode-gated near-term tool so all
  ;; thirteen frozen routes can be exercised without changing production mode.
  (when (and (string-equal (or (uiop:getenv "PAI_DEV_WORKBENCH") "")
                           "enabled")
             (not (find "hold-near-term-thought" *tools* :test #'string=
                        :key (lambda (tool) (ref tool "function" "name")))))
    (setf *tools* (concatenate 'vector *tools*
                               (vector (%near-term-intention-tool-definition))))))

(defun %kernel-tool-dispatch-advertised-name (tool)
  (unless (hash-table-p tool)
    (error "advertised tool is not an object."))
  (let ((function (gethash "function" tool)))
    (unless (hash-table-p function)
      (error "advertised tool omits its function object."))
    (let ((name (gethash "name" function)))
      (unless (and (stringp name) (plusp (length name)))
        (error "advertised tool name is absent or empty."))
      name)))

(defun %kernel-tool-dispatch-enabled-handler-ids ()
  "Resolve the exact current catalogue through the frozen registry/bindings."
  (let* ((tools (if (boundp '*tools*) *tools* #()))
         (rows (if (vectorp tools) (coerce tools 'list) tools)))
    (unless (listp rows)
      (error "advertised tool catalogue is not a sequence."))
    (let ((names (mapcar #'%kernel-tool-dispatch-advertised-name rows)))
      (unless (= (length names) (length (remove-duplicates names :test #'string=)))
        (error "advertised tool names must be unique."))
      (mapcar
       (lambda (name)
         (let* ((report (tool-dispatch-shadow-inspect name))
                (plan (gethash "registry_plan" report))
                (advertisement (gethash "incumbent_advertisement" report))
                (handler-id (and (string= (gethash "status" plan) "resolved")
                                 (gethash "handler_id" plan)))
                (binding (and handler-id
                              (tool-handler-binding-lookup handler-id))))
           (unless (and (= (gethash "count" advertisement) 1)
                        binding
                        (string= (getf binding :external-name) name))
             (error "advertised tool does not resolve to one exact binding: ~a."
                    name))
           handler-id))
       names))))

(defun kernel-tool-dispatch-bootstrap ()
  (cond
    ((not (tool-dispatch-kernel-boot-p))
     (setf *kernel-tool-dispatch-bootstrap-report*
           (obj "schema_version" 1 "status" "legacy-inactive"
                "mode" "legacy" "installed" nil)))
    (*kernel-tool-dispatch-bootstrap-complete*
     *kernel-tool-dispatch-bootstrap-report*)
    (t
     (unless (eq (fdefinition 'execute)
                 (tool-dispatch-terminal-execute-identity))
       (error "clean boot failed: a legacy EXECUTE wrapper was installed."))
     (%kernel-tool-dispatch-dev-catalogue-complete)
     (%kernel-tool-dispatch-load-required "kernel-tool-dispatch-core.lisp")
     (%kernel-tool-dispatch-load-required "kernel-tool-dispatch-shadow.lisp")
     (%kernel-tool-dispatch-load-required "kernel-tool-handler-bindings.lisp")
     (%kernel-tool-dispatch-load-required "kernel-tool-dispatch-runtime.lisp")
     (tool-dispatch-shadow-initialize
      (%kernel-tool-dispatch-state-path "kernel-tool-dispatch-module.sexp"))
     (tool-handler-bindings-initialize)
     (let* ((enabled-handler-ids
              (%kernel-tool-dispatch-enabled-handler-ids))
            (advertised-count (length enabled-handler-ids)))
       (unless (plusp advertised-count)
         (error "clean boot requires a nonempty advertised tool catalogue."))
       (tool-dispatch-runtime-install enabled-handler-ids)
       (let ((runtime (tool-dispatch-runtime-report)))
       (unless (and (gethash "installed" runtime)
                    (not (gethash "ownership_conflict" runtime))
                    (zerop (gethash "composition_errors" runtime))
                    (= (length (gethash "enabled_handler_ids" runtime))
                       advertised-count))
         (error "kernel dispatcher did not install the exact advertised routes."))
       (setf *kernel-tool-dispatch-bootstrap-complete* t
             *kernel-tool-dispatch-bootstrap-report*
             (obj "schema_version" 1 "status" "kernel-installed"
                  "mode" "kernel" "installed" t
                  "legacy_wrappers_installed" nil
                  "advertised_tool_count" advertised-count
                  "enabled_handler_count" advertised-count))))))
  *kernel-tool-dispatch-bootstrap-report*)

(defun kernel-tool-dispatch-bootstrap-report ()
  (or *kernel-tool-dispatch-bootstrap-report*
      (obj "schema_version" 1 "status" "not-run" "installed" nil)))

(define-init :install kernel-tool-dispatch
    "Bind the registry-driven tool dispatch route table."
  (kernel-tool-dispatch-bootstrap))
