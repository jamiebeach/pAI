;;;; kernel-tool-dispatch-shadow.lisp -- thin :agent compatibility facade.
;;;; Dev/test integration only. It never installs over EXECUTE or invokes a
;;;; handler unless an explicit caller supplies an incumbent thunk.

(in-package :agent)

(export '(tool-dispatch-shadow-initialize tool-dispatch-shadow-inspect
          call-with-tool-dispatch-shadow
          tool-dispatch-shadow-capability-report))

(defparameter *tool-dispatch-shadow-max-name-chars* 128)
(defvar *tool-dispatch-shadow-registry* nil)
(defvar *tool-dispatch-shadow-manifest-path* nil)
(defvar *tool-dispatch-shadow-sink* nil)

(defun %tool-dispatch-shadow-read-one (path)
  (let ((*read-eval* nil))
    (with-open-file (stream path :direction :input)
      (let ((registry (read stream nil :eof))
            (trailing (read stream nil :eof)))
        (when (eq registry :eof)
          (error "Tool-dispatch manifest is empty."))
        (unless (eq trailing :eof)
          (error "Tool-dispatch manifest contains trailing forms."))
        registry))))

(defun tool-dispatch-shadow-initialize (manifest-path)
  "Read and validate one explicit manifest. Return a content-free report."
  (unless (or (stringp manifest-path) (pathnamep manifest-path))
    (error "Tool-dispatch manifest path must be explicit text or a pathname."))
  (let ((registry (%tool-dispatch-shadow-read-one manifest-path)))
    (pai.kernel.tool-dispatch:validate-registry registry)
    (setf *tool-dispatch-shadow-registry* registry
          *tool-dispatch-shadow-manifest-path* (namestring manifest-path))
    (obj "schema_version" 1 "status" "initialized"
         "module_id" (getf registry :module-id)
         "tool_count" (length (getf registry :tools))
         "production_loaded" nil)))

(defun %tool-dispatch-shadow-require-registry ()
  (unless *tool-dispatch-shadow-registry*
    (error "Tool-dispatch shadow facade is not initialized."))
  *tool-dispatch-shadow-registry*)

(defun %tool-dispatch-shadow-call (name)
  (let ((call (make-hash-table :test #'equal))
        (function (make-hash-table :test #'equal)))
    (setf (gethash "name" function) name
          (gethash "function" call) function)
    call))

(defun %tool-dispatch-shadow-advertised-name (tool)
  (when (hash-table-p tool)
    (let ((function (gethash "function" tool)))
      (when (hash-table-p function)
        (gethash "name" function)))))

(defun %tool-dispatch-shadow-advertisement-count (name)
  (if (and (stringp name) (plusp (length name))
           (<= (length name) *tool-dispatch-shadow-max-name-chars*))
      (let ((tools (if (boundp '*tools*) *tools* #())))
        (count name (if (vectorp tools) (coerce tools 'list) tools)
               :test #'string= :key #'%tool-dispatch-shadow-advertised-name))
      0))

(defun %tool-dispatch-shadow-classification (plan advertisement-count)
  (let ((status (gethash "status" plan)))
    (cond
      ((string= status "malformed-tool-call") "malformed")
      ((string= status "resolved")
       (cond ((= advertisement-count 1) "match")
             ((zerop advertisement-count) "registry-only")
             ;; One advertisement pairs with the registry; every additional
             ;; advertisement is incumbent-only drift.
             (t "incumbent-only")))
      ((plusp advertisement-count) "incumbent-only")
      (t "unknown"))))

(defun tool-dispatch-shadow-inspect (name)
  "Compare pure resolution with exact incumbent advertisement. Execute nothing."
  (let* ((registry (%tool-dispatch-shadow-require-registry))
         (bounded-name
           (if (and (stringp name)
                    (<= (length name) *tool-dispatch-shadow-max-name-chars*))
               name nil))
         (plan (pai.kernel.tool-dispatch:compose-dispatch-plan
                registry (%tool-dispatch-shadow-call bounded-name)))
         (advertisement-count
           (%tool-dispatch-shadow-advertisement-count bounded-name))
         (classification
           (%tool-dispatch-shadow-classification plan advertisement-count)))
    (obj "schema_version" 1 "status" "inspected"
         "classification" classification
         "registry_plan" plan
         "incumbent_advertisement"
         (obj "exact_name" (or bounded-name :null)
              "advertised" (if (plusp advertisement-count) t nil)
              "count" advertisement-count
              "unique" (if (= advertisement-count 1) t nil)
              "proves_executable_dispatch" nil)
         "execution_attempted" nil
         "provider_calls" 0 "model_calls" 0 "database_writes" 0
         "filesystem_writes" 0 "event_appends" 0
         "delivery_attempts" 0 "authority_mutations" 0)))

(defun call-with-tool-dispatch-shadow (tool-call incumbent-thunk
                                       &key (sink *tool-dispatch-shadow-sink*))
  "Inspect TOOL-CALL, notify an optional in-memory SINK, then call THUNK once.
Return all incumbent values unchanged; do not catch the incumbent error."
  (unless (functionp incumbent-thunk)
    (error "Incumbent tool dispatcher must be an explicit function."))
  (let* ((function (and (hash-table-p tool-call)
                        (gethash "function" tool-call)))
         (name (and (hash-table-p function) (gethash "name" function)))
         (report (tool-dispatch-shadow-inspect name)))
    (when sink
      (handler-case (funcall sink report)
        (error () nil)))
    (multiple-value-call #'values (funcall incumbent-thunk tool-call))))

(defun tool-dispatch-shadow-capability-report ()
  (obj "schema_version" 1 "status" "read-only-shadow"
       "initialized" (if *tool-dispatch-shadow-registry* t nil)
       "manifest_path_retained" (if *tool-dispatch-shadow-manifest-path* t nil)
       "handler_invocation_available" nil
       "execute_installed" nil "observer_execution_available" nil
       "provider_calls_available" nil "model_calls_available" nil
       "database_writes_available" nil "filesystem_writes_available" nil
       "event_appends_available" nil "transport_available" nil
       "delivery_authority" nil "self_modification_available" nil
       "authority_mutation_available" nil))
