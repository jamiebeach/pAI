(defpackage :agent (:use :cl))
(in-package :agent)

(defun obj (&rest pairs)
  (loop with table = (make-hash-table :test #'equal)
        for (key value) on pairs by #'cddr
        do (setf (gethash key table) value)
        finally (return table)))

(defvar *tools* #())

(in-package :cl-user)

(defvar *tool-dispatch-shadow-pass* 0)
(defvar *tool-dispatch-shadow-fail* 0)

(defun tool-dispatch-shadow-check (name condition)
  (if condition
      (progn (incf *tool-dispatch-shadow-pass*) (format t "PASS ~a~%" name))
      (progn (incf *tool-dispatch-shadow-fail*) (format t "FAIL ~a~%" name))))

(defun tool-dispatch-shadow-error (thunk)
  (handler-case (progn (funcall thunk) nil) (error (condition) condition)))

(defun tool-dispatch-shadow-read-one (path)
  (let ((*read-eval* nil))
    (with-open-file (stream path :direction :input) (read stream))))

(defun tool-dispatch-shadow-tool (name)
  (agent::obj "type" "function" "function" (agent::obj "name" name)))

(defun tool-dispatch-shadow-call (name)
  (agent::obj "id" "fixture" "function" (agent::obj "name" name)))

(load (test-source "kernel-tool-dispatch-core.lisp"))
(load (test-source "kernel-tool-dispatch-shadow.lisp"))

(let* ((manifest-path (namestring (test-source "kernel-tool-dispatch-module.sexp")))
       (manifest (tool-dispatch-shadow-read-one manifest-path))
       (names (mapcar (lambda (tool) (getf tool :name))
                      (getf manifest :tools))))
  (tool-dispatch-shadow-check
   "uninitialized inspection fails closed"
   (tool-dispatch-shadow-error
    (lambda () (agent:tool-dispatch-shadow-inspect "lisp-eval"))))
  (let ((report (agent:tool-dispatch-shadow-initialize manifest-path)))
    (tool-dispatch-shadow-check
     "explicit manifest initializes exact module"
     (and (string= (gethash "status" report) "initialized")
          (string= (gethash "module_id" report)
                   "pai.kernel.tool-dispatch")
          (= (gethash "tool_count" report) 13))))
  (setf agent::*tools* (coerce (mapcar #'tool-dispatch-shadow-tool names)
                               'vector))
  (dolist (name names)
    (let* ((shadow (agent:tool-dispatch-shadow-inspect name))
           (plan (gethash "registry_plan" shadow))
           (pure (pai.kernel.tool-dispatch:compose-dispatch-plan
                  manifest (tool-dispatch-shadow-call name))))
      (tool-dispatch-shadow-check
       (format nil "exact plan and advertisement match for ~a" name)
       (and (string= (gethash "classification" shadow) "match")
            (string= (gethash "handler_id" plan)
                     (gethash "handler_id" pure))
            (equalp (gethash "around_stages" plan)
                    (gethash "around_stages" pure))
            (equalp (gethash "before_stages" plan)
                    (gethash "before_stages" pure))
            (equalp (gethash "after_stages" plan)
                    (gethash "after_stages" pure))
            (= (gethash "count"
                        (gethash "incumbent_advertisement" shadow)) 1)
            (null (gethash "execution_attempted" shadow))))))
  (setf agent::*tools* #())
  (tool-dispatch-shadow-check
   "resolved but absent advertisement is registry-only"
   (string= "registry-only"
            (gethash "classification"
                     (agent:tool-dispatch-shadow-inspect "lisp-eval"))))
  (setf agent::*tools* (vector (tool-dispatch-shadow-tool "legacy-extra")))
  (tool-dispatch-shadow-check
   "advertised unknown name is incumbent-only"
   (string= "incumbent-only"
            (gethash "classification"
                     (agent:tool-dispatch-shadow-inspect "legacy-extra"))))
  (setf agent::*tools*
        (vector (tool-dispatch-shadow-tool "lisp-eval")
                (tool-dispatch-shadow-tool "lisp-eval")))
  (let* ((report (agent:tool-dispatch-shadow-inspect "lisp-eval"))
         (advertisement (gethash "incumbent_advertisement" report)))
    (tool-dispatch-shadow-check
     "duplicate advertisement is detected as incumbent drift"
     (and (string= (gethash "classification" report) "incumbent-only")
          (= (gethash "count" advertisement) 2)
          (null (gethash "unique" advertisement)))))
  (setf agent::*tools* #())
  (dolist (case (list (cons "unknown" "not-a-tool")
                      (cons "unknown" "LISP-EVAL")
                      (cons "malformed" "")
                      (cons "malformed" 42)
                      (cons "malformed" (make-string 129 :initial-element #\x))))
    (tool-dispatch-shadow-check
     (format nil "~s is classified ~a" (cdr case) (car case))
     (string= (gethash "classification"
                       (agent:tool-dispatch-shadow-inspect (cdr case)))
              (car case))))
  (setf agent::*tools* (vector (tool-dispatch-shadow-tool "lisp-eval")))
  (let ((calls 0) (seen nil))
    (multiple-value-bind (first second third)
        (agent:call-with-tool-dispatch-shadow
         (tool-dispatch-shadow-call "lisp-eval")
         (lambda (call)
           (declare (ignore call))
           (incf calls)
           (values :one 2 "three"))
         :sink (lambda (report) (setf seen report)))
      (tool-dispatch-shadow-check
       "pass-through calls incumbent exactly once and preserves values"
       (and (= calls 1) (eq first :one) (= second 2) (string= third "three")
            (string= (gethash "classification" seen) "match")))))
  (let ((calls 0) (original nil))
    (handler-case
        (agent:call-with-tool-dispatch-shadow
         (tool-dispatch-shadow-call "lisp-eval")
         (lambda (call)
           (declare (ignore call))
           (incf calls)
           (error "original-incumbent-error")))
      (error (condition) (setf original condition)))
    (tool-dispatch-shadow-check
     "pass-through preserves original error without retry"
     (and (= calls 1) original
          (search "original-incumbent-error" (princ-to-string original)))))
  (let ((calls 0))
    (tool-dispatch-shadow-check
     "sink error is isolated from incumbent result"
     (eq :legacy
         (agent:call-with-tool-dispatch-shadow
          (tool-dispatch-shadow-call "lisp-eval")
          (lambda (call) (declare (ignore call)) (incf calls) :legacy)
          :sink (lambda (report) (declare (ignore report))
                  (error "sink-failure")))))
    (tool-dispatch-shadow-check "sink failure does not retry incumbent"
                                (= calls 1)))
  (let ((trailing "/tmp/r2b-trailing.sexp")
        (reader-eval "/tmp/r2b-reader-eval.sexp")
        (invalid "/tmp/r2b-invalid.sexp"))
    (unwind-protect
        (progn
          (with-open-file (stream trailing :direction :output
                                  :if-exists :supersede)
            (prin1 manifest stream) (terpri stream) (prin1 :trailing stream))
          (with-open-file (stream reader-eval :direction :output
                                  :if-exists :supersede)
            (write-string "#.(error \"reader-eval-ran\")" stream))
          (let ((bad (copy-tree manifest)))
            (setf (getf bad :module-id) "wrong.module")
            (with-open-file (stream invalid :direction :output
                                    :if-exists :supersede)
              (prin1 bad stream)))
          (dolist (case (list (cons "trailing form" trailing)
                              (cons "reader evaluation" reader-eval)
                              (cons "invalid registry" invalid)))
            (tool-dispatch-shadow-check
             (format nil "initialization rejects ~a" (car case))
             (tool-dispatch-shadow-error
              (lambda ()
                (agent:tool-dispatch-shadow-initialize (cdr case)))))))
      (dolist (path (list trailing reader-eval invalid))
        (ignore-errors (delete-file path)))))
  ;; Restore the valid registry after negative initialization attempts.
  (agent:tool-dispatch-shadow-initialize manifest-path)
  (let ((report (agent:tool-dispatch-shadow-capability-report)))
    (dolist (key '("handler_invocation_available" "execute_installed"
                   "observer_execution_available" "provider_calls_available"
                   "model_calls_available" "database_writes_available"
                   "filesystem_writes_available" "event_appends_available"
                   "transport_available" "delivery_authority"
                   "self_modification_available"
                   "authority_mutation_available"))
      (tool-dispatch-shadow-check
       (format nil "capability report denies ~a" key)
       (null (gethash key report)))))
  (let ((source (uiop:read-file-string
                 (namestring (test-source "kernel-tool-dispatch-shadow.lisp")))))
    (tool-dispatch-shadow-check
     "facade does not define or install execute"
     (and (null (search "(defun execute" source :test #'char-equal))
          (null (search "(fdefinition 'execute" source :test #'char-equal))
          (null (search "setf (symbol-function" source :test #'char-equal))))
    (tool-dispatch-shadow-check
     "facade contains no provider persistence delivery or authority primitive"
     (every (lambda (needle) (null (search needle source :test #'char-equal)))
            '("raw-call-model" "(call-model" "postmodern" "pomo:"
              "log-event" "memory-write" "telegram-send"
              "public-outbound" "propose-loop" "lisp-eval")))))

(format t "RESULT kernel-tool-dispatch-shadow: ~d passed, ~d failed~%"
        *tool-dispatch-shadow-pass* *tool-dispatch-shadow-fail*)
(when (plusp *tool-dispatch-shadow-fail*) (sb-ext:exit :code 1))
