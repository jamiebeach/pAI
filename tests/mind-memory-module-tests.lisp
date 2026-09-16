;;;; harness: bare
;;;;
;;;; This suite proves the module loads WITHOUT the legacy :agent package --
;;;; it asserts (null (find-package :agent)). Preloading the framework would
;;;; defeat that assertion for a reason unrelated to the module.
(ql:quickload '(:ironclad :babel) :silent t)

(in-package :cl-user)

(defvar *mind-memory-module-pass* 0)
(defvar *mind-memory-module-fail* 0)

(defun mind-memory-module-check (name condition)
  (if condition
      (progn (incf *mind-memory-module-pass*) (format t "PASS ~a~%" name))
      (progn (incf *mind-memory-module-fail*) (format t "FAIL ~a~%" name))))

(defun mind-memory-module-read-one (path)
  (with-open-file (stream path :direction :input)
    (let ((value (read stream nil :eof))
          (trailing (read stream nil :eof)))
      (unless (eq trailing :eof) (error "Manifest has trailing forms."))
      value)))

(defun mind-memory-module-plist-keys (plist)
  (loop for tail on plist by #'cddr collect (car tail)))

(defun mind-memory-module-walk-symbols (form)
  (cond ((symbolp form) (list form))
        ((consp form) (mapcan #'mind-memory-module-walk-symbols form))
        ((vectorp form)
         (loop for item across form append (mind-memory-module-walk-symbols item)))
        (t nil)))

(defun mind-memory-module-source-symbols (path)
  (with-open-file (stream path :direction :input)
    (loop for form = (read stream nil :eof)
          until (eq form :eof)
          append (mind-memory-module-walk-symbols form))))

(let* ((manifest-path (namestring (test-source "mind-memory-module.sexp")))
       (core-path (namestring (test-source "mind-memory-core.lisp")))
       (manifest (mind-memory-module-read-one manifest-path))
       (expected-keys
         '(:schema-version :module-id :layer :purpose :owned-projections
           :owned-event-contracts :permitted-package-dependencies
           :forbidden-dependencies :public-interface :adapter-ports
           :effective-behavior :production-loaded)))
  (mind-memory-module-check
   "manifest has exact top-level keys"
   (equal (mind-memory-module-plist-keys manifest) expected-keys))
  (mind-memory-module-check "manifest identifies memory mind module"
                            (and (string= (getf manifest :module-id)
                                          "pai.mind.memory")
                                 (eq (getf manifest :layer) :mind)))
  (mind-memory-module-check
   "manifest freezes exact pure dependencies"
   (equal (getf manifest :permitted-package-dependencies)
          '(:common-lisp :ironclad :babel)))
  (mind-memory-module-check
   "manifest declares six public functions"
   (equal (getf manifest :public-interface)
          '("VALIDATE-STATE" "ROW-ELIGIBLE-P" "BUILD-ATOM-MANIFEST"
            "BUILD-ATOM-REQUEST" "VALIDATE-ATOM-RESPONSE"
            "CAPABILITY-REPORT")))
  (mind-memory-module-check
   "manifest declares six adapter ports but implements none"
   (equal (getf manifest :adapter-ports)
          '(:projection-lookup :projection-mutation :embedding
            :model-invocation :clock :event-append)))
  (mind-memory-module-check "manifest is source-only"
                            (and (eq (getf manifest :effective-behavior) :none)
                                 (null (getf manifest :production-loaded))))

  (mind-memory-module-check "agent package absent before pure load"
                            (null (find-package :agent)))
  (load core-path)
  (mind-memory-module-check "agent package absent after pure load"
                            (null (find-package :agent)))
  (let* ((package (find-package :pai.mind.memory))
         (exports (sort
                   (loop for symbol being the external-symbols of package
                         collect (symbol-name symbol))
                   #'string<))
         (expected (sort (copy-list (getf manifest :public-interface))
                         #'string<)))
    (mind-memory-module-check "package exports exactly the manifest interface"
                              (equal exports expected)))

  (let* ((symbols (mind-memory-module-source-symbols core-path))
         (forbidden-packages
           '("AGENT" "POSTMODERN" "POMO" "DEXADOR" "DRAKMA"
             "BORDEAUX-THREADS" "BT" "UIOP"))
         (forbidden-symbols
           '("WITH-OPEN-FILE" "OPEN" "RUN-PROGRAM" "LAUNCH-PROGRAM"
             "LOG-EVENT" "MEMORY-WRITE-NODE" "MEMORY-ADMIT-NODE"
             "RAW-CALL-MODEL" "CALL-MODEL" "MAKE-THREAD")))
    (mind-memory-module-check
     "pure source contains no forbidden package reference"
     (notany (lambda (symbol)
               (let ((package (symbol-package symbol)))
                 (and package
                      (member (package-name package) forbidden-packages
                              :test #'string=))))
             symbols))
    (mind-memory-module-check
     "pure source contains no forbidden operation symbol"
     (notany (lambda (symbol)
               (member (symbol-name symbol) forbidden-symbols :test #'string=))
             symbols))))

(let ((report (pai.mind.memory:capability-report)))
  (mind-memory-module-check "capability report denies admission"
                            (null (gethash "admission_available" report)))
  (mind-memory-module-check "capability report denies provider calls"
                            (null (gethash "provider_calls_available" report)))
  (mind-memory-module-check "capability report denies database writes"
                            (null (gethash "database_writes_available" report)))
  (mind-memory-module-check "capability report denies delivery authority"
                            (null (gethash "delivery_authority" report))))

(format t "RESULT mind-memory-module: ~d passed, ~d failed~%"
        *mind-memory-module-pass* *mind-memory-module-fail*)
(when (plusp *mind-memory-module-fail*) (uiop:quit 1))
