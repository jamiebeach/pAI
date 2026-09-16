;;;; harness: bare
;;;;
;;;; This suite proves the module loads WITHOUT the legacy :agent package --
;;;; it asserts (null (find-package :agent)). Preloading the framework would
;;;; defeat that assertion for a reason unrelated to the module.
(in-package :cl-user)

(defvar *kernel-tool-dispatch-module-pass* 0)
(defvar *kernel-tool-dispatch-module-fail* 0)

(defun kernel-tool-dispatch-module-check (name condition)
  (if condition
      (progn (incf *kernel-tool-dispatch-module-pass*)
             (format t "PASS ~a~%" name))
      (progn (incf *kernel-tool-dispatch-module-fail*)
             (format t "FAIL ~a~%" name))))

(defun kernel-tool-dispatch-read-one (path)
  (with-open-file (stream path :direction :input)
    (let ((value (read stream nil :eof))
          (trailing (read stream nil :eof)))
      (unless (eq trailing :eof) (error "Manifest has trailing forms."))
      value)))

(defun kernel-tool-dispatch-plist-keys (plist)
  (loop for tail on plist by #'cddr collect (car tail)))

(defun kernel-tool-dispatch-walk-symbols (form)
  (cond ((symbolp form) (list form))
        ((consp form) (mapcan #'kernel-tool-dispatch-walk-symbols form))
        ((vectorp form)
         (loop for item across form
               append (kernel-tool-dispatch-walk-symbols item)))
        (t nil)))

(defun kernel-tool-dispatch-source-symbols (path)
  (with-open-file (stream path :direction :input)
    (loop for form = (read stream nil :eof)
          until (eq form :eof)
          append (kernel-tool-dispatch-walk-symbols form))))

(defun kernel-tool-dispatch-call (name)
  (let ((call (make-hash-table :test #'equal))
        (function (make-hash-table :test #'equal)))
    (setf (gethash "name" function) name
          (gethash "function" call) function)
    call))

(defparameter *kernel-tool-dispatch-agent-package-before-load*
  (find-package :agent))

(load (test-source "kernel-tool-dispatch-core.lisp"))

(let* ((manifest-path (namestring (test-source "kernel-tool-dispatch-module.sexp")))
       (core-path (namestring (test-source "kernel-tool-dispatch-core.lisp")))
       (manifest (kernel-tool-dispatch-read-one manifest-path))
       (expected-keys
         '(:schema-version :module-id :layer :purpose
           :permitted-package-dependencies :forbidden-dependencies
           :public-interface :observer-stage-order :observer-stages :tools
           :execute-audit-files :effective-behavior :production-loaded)))
  (kernel-tool-dispatch-module-check
   "manifest has exact top-level keys"
   (equal (kernel-tool-dispatch-plist-keys manifest) expected-keys))
  (kernel-tool-dispatch-module-check
   "manifest identifies the kernel dispatch module"
   (and (string= (getf manifest :module-id) "pai.kernel.tool-dispatch")
        (eq (getf manifest :layer) :kernel)))
  (kernel-tool-dispatch-module-check
   "manifest permits Common Lisp only"
   (equal (getf manifest :permitted-package-dependencies) '(:common-lisp)))
  (kernel-tool-dispatch-module-check
   "manifest freezes four public functions"
   (equal (getf manifest :public-interface)
          '("VALIDATE-REGISTRY" "RESOLVE-TOOL" "COMPOSE-DISPATCH-PLAN"
            "CAPABILITY-REPORT")))
  (kernel-tool-dispatch-module-check
   "manifest freezes exact semantic stage order"
   (equal (getf manifest :observer-stage-order)
          '(:timing :tool-call-event :proposal-provenance :handler
            :modulator-appraisal :tool-result-event)))
  (kernel-tool-dispatch-module-check
   "manifest is source-only"
   (and (eq (getf manifest :effective-behavior) :none)
        (null (getf manifest :production-loaded))))
  (kernel-tool-dispatch-module-check "agent package absent before pure load"
                                     (null *kernel-tool-dispatch-agent-package-before-load*))
  (kernel-tool-dispatch-module-check "agent package absent after pure load"
                                     (null (find-package :agent)))
  (kernel-tool-dispatch-module-check
   "registry validates"
   (pai.kernel.tool-dispatch:validate-registry manifest))
  (let* ((package (find-package :pai.kernel.tool-dispatch))
         (exports (sort
                   (loop for symbol being the external-symbols of package
                         collect (symbol-name symbol))
                   #'string<))
         (expected (sort (copy-list (getf manifest :public-interface))
                         #'string<)))
    (kernel-tool-dispatch-module-check
     "package exports exactly the manifest interface" (equal exports expected)))
  (let* ((symbols (kernel-tool-dispatch-source-symbols core-path))
         (forbidden-packages
           '("AGENT" "POSTMODERN" "POMO" "DEXADOR" "DRAKMA"
             "BORDEAUX-THREADS" "BT" "UIOP"))
         (forbidden-symbols
           '("WITH-OPEN-FILE" "OPEN" "RUN-PROGRAM" "LAUNCH-PROGRAM"
             "LOG-EVENT" "RAW-CALL-MODEL" "CALL-MODEL" "MAKE-THREAD"
             "EXECUTE" "INTERN" "FDEFINITION" "FUNCALL" "APPLY" "EVAL")))
    (kernel-tool-dispatch-module-check
     "pure source contains no forbidden package reference"
     (notany (lambda (symbol)
               (let ((package (symbol-package symbol)))
                 (and package
                      (member (package-name package) forbidden-packages
                              :test #'string=))))
             symbols))
    (kernel-tool-dispatch-module-check
     "pure source cannot perform or dynamically discover an operation"
     (notany (lambda (symbol)
               (member (symbol-name symbol) forbidden-symbols :test #'string=))
             symbols)))
  (let ((missing-key (copy-tree manifest))
        (bad-external-name (copy-tree manifest))
        (bad-name (copy-tree manifest))
        (bad-handler (copy-tree manifest))
        (bad-stage (copy-tree manifest))
        (bad-stage-order (copy-tree manifest)))
    (remf (first (getf missing-key :tools)) :schema-owner)
    (setf (getf (first (getf bad-external-name :tools)) :name) :lisp-eval)
    (setf (getf (second (getf bad-name :tools)) :name)
          (getf (first (getf bad-name :tools)) :name))
    (setf (getf (second (getf bad-handler :tools)) :handler-id)
          (getf (first (getf bad-handler :tools)) :handler-id))
    (setf (getf (first (getf bad-stage :tools)) :observer-stage-ids)
          '(:not-a-stage))
    (setf (getf bad-stage-order :observer-stage-order)
          '(:handler :timing))
    (dolist (case (list (cons "missing required tool key" missing-key)
                        (cons "non-string external name" bad-external-name)
                        (cons "duplicate name" bad-name)
                        (cons "duplicate handler" bad-handler)
                        (cons "unknown stage" bad-stage)
                        (cons "incomplete stage order" bad-stage-order)))
      (kernel-tool-dispatch-module-check
       (format nil "validation rejects ~a" (car case))
       (handler-case
           (progn (pai.kernel.tool-dispatch:validate-registry (cdr case)) nil)
         (error () t)))))
  (let ((report (pai.kernel.tool-dispatch:capability-report)))
    (dolist (key '("handler_invocation_available"
                   "observer_execution_available" "provider_calls_available"
                   "model_calls_available" "database_writes_available"
                   "filesystem_writes_available" "event_appends_available"
                   "transport_available" "delivery_authority"
                   "self_modification_available"
                   "authority_mutation_available"))
      (kernel-tool-dispatch-module-check
       (format nil "capability report denies ~a" key)
       (null (gethash key report)))))
  (let* ((call (kernel-tool-dispatch-call "lisp-eval"))
         (baseline
           (pai.kernel.tool-dispatch:compose-dispatch-plan manifest call))
         (stable t)
         (keys '("schema_version" "status" "external_name" "handler_id"
                 "handler_owner" "schema_owner" "availability_owner"
                 "around_stages" "before_stages" "after_stages")))
    (dotimes (index 1000)
      (declare (ignorable index))
      (let ((next
              (pai.kernel.tool-dispatch:compose-dispatch-plan manifest call)))
        (unless (every (lambda (key)
                         (equalp (gethash key baseline) (gethash key next)))
                       keys)
          (setf stable nil))))
    (kernel-tool-dispatch-module-check
     "one thousand resolutions are value-stable" stable)
    (kernel-tool-dispatch-module-check
     "one thousand resolutions leave input unchanged"
     (string= (gethash "name" (gethash "function" call)) "lisp-eval"))))

(format t "RESULT kernel-tool-dispatch-module: ~d passed, ~d failed~%"
        *kernel-tool-dispatch-module-pass* *kernel-tool-dispatch-module-fail*)
(when (plusp *kernel-tool-dispatch-module-fail*) (sb-ext:exit :code 1))
