(defpackage :pai.kernel.tool-dispatch
  (:use :cl)
  (:export :validate-registry :resolve-tool :compose-dispatch-plan
           :capability-report))

(in-package :pai.kernel.tool-dispatch)

(defparameter *required-registry-keys*
  '(:schema-version :module-id :layer :purpose
    :permitted-package-dependencies :forbidden-dependencies :public-interface
    :observer-stage-order :observer-stages :tools :execute-audit-files
    :effective-behavior :production-loaded))

(defparameter *required-tool-keys*
  '(:name :handler-id :handler-owner :schema-owner :availability-owner
    :observer-stage-ids))

(defparameter *required-stage-keys*
  '(:id :owner-file :position))

(defun %object (&rest pairs)
  (let ((object (make-hash-table :test #'equal)))
    (loop for (key value) on pairs by #'cddr
          do (setf (gethash key object) value))
    object))

(defun %plist-keys (plist)
  (loop for tail on plist by #'cddr collect (car tail)))

(defun %proper-plist-p (value)
  (and (listp value) (evenp (length value))))

(defun %exact-keys-p (plist required &optional optional)
  (and (%proper-plist-p plist)
       (let ((keys (%plist-keys plist)))
         (and (= (length keys) (length (remove-duplicates keys :test #'eq)))
              (null (set-difference required keys :test #'eq))
              (null (set-difference keys (append required optional)
                                    :test #'eq))))))

(defun %nonempty-string-p (value)
  (and (stringp value) (plusp (length value))))

(defun %unique-p (values test)
  (= (length values) (length (remove-duplicates values :test test))))

(defun validate-registry (registry)
  "Validate REGISTRY and return T; signal ERROR for any invalid contract data."
  (unless (%exact-keys-p registry *required-registry-keys*)
    (error "Tool-dispatch registry has invalid top-level keys."))
  (unless (and (= (getf registry :schema-version -1) 1)
               (string= (getf registry :module-id "")
                        "pai.kernel.tool-dispatch")
               (eq (getf registry :layer) :kernel)
               (equal (getf registry :permitted-package-dependencies)
                      '(:common-lisp))
               (eq (getf registry :effective-behavior) :none)
               (null (getf registry :production-loaded)))
    (error "Tool-dispatch registry identity or source-only contract is invalid."))
  (let* ((stages (getf registry :observer-stages))
         (tools (getf registry :tools))
         (stage-ids (mapcar (lambda (stage) (getf stage :id)) stages))
         (stage-order (getf registry :observer-stage-order))
         (names (mapcar (lambda (tool) (getf tool :name)) tools))
         (handler-ids (mapcar (lambda (tool) (getf tool :handler-id)) tools)))
    (unless (and (listp stages) stages (listp tools) tools)
      (error "Tool-dispatch stages and tools must be non-empty lists."))
    (dolist (stage stages)
      (unless (%exact-keys-p stage *required-stage-keys* '(:tool-names))
        (error "Observer stage has invalid keys."))
      (unless (and (keywordp (getf stage :id))
                   (%nonempty-string-p (getf stage :owner-file))
                   (member (getf stage :position) '(:around :before :after)))
        (error "Observer stage has invalid values."))
      (when (getf stage :tool-names)
        (unless (and (every #'%nonempty-string-p (getf stage :tool-names))
                     (%unique-p (getf stage :tool-names) #'string=))
          (error "Observer stage tool names are invalid."))))
    (unless (%unique-p stage-ids #'eq)
      (error "Observer stage IDs must be unique."))
    (unless (and (listp stage-order)
                 (= 1 (count :handler stage-order :test #'eq))
                 (%unique-p stage-order #'eq)
                 (equal (remove :handler stage-order :test #'eq) stage-ids))
      (error "Observer stage order must contain every stage and one handler."))
    (dolist (tool tools)
      (unless (%exact-keys-p tool *required-tool-keys*)
        (error "Tool record has invalid keys."))
      (unless (and (%nonempty-string-p (getf tool :name))
                   (keywordp (getf tool :handler-id))
                   (%nonempty-string-p (getf tool :handler-owner))
                   (%nonempty-string-p (getf tool :schema-owner))
                   (%nonempty-string-p (getf tool :availability-owner))
                   (listp (getf tool :observer-stage-ids))
                   (%unique-p (getf tool :observer-stage-ids) #'eq)
                   (every (lambda (id) (member id stage-ids :test #'eq))
                          (getf tool :observer-stage-ids)))
        (error "Tool record has invalid values or an unknown observer stage.")))
    (unless (%unique-p names #'string=)
      (error "External tool names must be unique."))
    (unless (%unique-p handler-ids #'eq)
      (error "Handler IDs must be unique."))
    (dolist (stage stages)
      (dolist (name (getf stage :tool-names))
        (unless (member name names :test #'string=)
          (error "Observer stage names an unknown tool."))))
    t))

(defun %extract-name (tool-call)
  (unless (hash-table-p tool-call)
    (return-from %extract-name (values nil :malformed-tool-call)))
  (let ((function (gethash "function" tool-call)))
    (unless (hash-table-p function)
      (return-from %extract-name (values nil :malformed-tool-call)))
    (let ((name (gethash "name" function)))
      (if (%nonempty-string-p name)
          (values name :well-formed)
          (values nil :malformed-tool-call)))))

(defun resolve-tool (registry tool-call)
  "Return TOOL, STATUS and external name without invoking or interning anything."
  (validate-registry registry)
  (multiple-value-bind (name shape-status) (%extract-name tool-call)
    (if (eq shape-status :malformed-tool-call)
        (values nil :malformed-tool-call nil)
        (let ((tool (find name (getf registry :tools) :test #'string=
                          :key (lambda (row) (getf row :name)))))
          (if tool
              (values tool :resolved name)
              (values nil :unknown-tool name))))))

(defun %stage-record (registry id)
  (find id (getf registry :observer-stages) :test #'eq
        :key (lambda (stage) (getf stage :id))))

(defun %stage-names-at (registry tool position)
  (coerce
   (loop for id in (getf tool :observer-stage-ids)
         for stage = (%stage-record registry id)
         when (eq (getf stage :position) position)
           collect (string-downcase (symbol-name id)))
   'vector))

(defun compose-dispatch-plan (registry tool-call)
  "Return a deterministic data-only plan. No handler or observer is executed."
  (multiple-value-bind (tool status name) (resolve-tool registry tool-call)
    (if (eq status :resolved)
        (%object "schema_version" 1 "status" "resolved"
                 "external_name" name
                 "handler_id"
                 (string-downcase (symbol-name (getf tool :handler-id)))
                 "handler_owner" (getf tool :handler-owner)
                 "schema_owner" (getf tool :schema-owner)
                 "availability_owner" (getf tool :availability-owner)
                 "around_stages" (%stage-names-at registry tool :around)
                 "before_stages" (%stage-names-at registry tool :before)
                 "after_stages" (%stage-names-at registry tool :after))
        (%object "schema_version" 1
                 "status" (string-downcase (symbol-name status))
                 "external_name" (or name :null)
                 "handler_id" :null
                 "handler_owner" :null
                 "schema_owner" :null
                 "availability_owner" :null
                 "around_stages" #()
                 "before_stages" #()
                 "after_stages" #()))))

(defun capability-report ()
  (%object "schema_version" 1
           "handler_invocation_available" nil
           "observer_execution_available" nil
           "provider_calls_available" nil
           "model_calls_available" nil
           "database_writes_available" nil
           "filesystem_writes_available" nil
           "event_appends_available" nil
           "transport_available" nil
           "delivery_authority" nil
           "self_modification_available" nil
           "authority_mutation_available" nil))
