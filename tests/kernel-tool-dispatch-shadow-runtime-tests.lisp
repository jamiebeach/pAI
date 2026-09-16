(ql:quickload :bordeaux-threads :silent t)

(defpackage :agent (:use :cl))
(in-package :agent)

(defun obj (&rest pairs)
  (loop with table = (make-hash-table :test #'equal)
        for (key value) on pairs by #'cddr
        do (setf (gethash key table) value)
        finally (return table)))

(defvar *tools* #())
(defun execute (tool-call) tool-call)

(in-package :cl-user)

(defvar *r2c-pass* 0)
(defvar *r2c-fail* 0)
(defun r2c-check (name condition)
  (if condition
      (progn (incf *r2c-pass*) (format t "PASS ~a~%" name))
      (progn (incf *r2c-fail*) (format t "FAIL ~a~%" name))))
(defun r2c-error (thunk)
  (handler-case (progn (funcall thunk) nil) (error (condition) condition)))
(defun r2c-obj (&rest pairs) (apply #'agent::obj pairs))
(defun r2c-call (name &optional (arguments "{}"))
  (r2c-obj "id" "private-id" "function"
           (r2c-obj "name" name "arguments" arguments)))
(defun r2c-tool (name)
  (r2c-obj "type" "function" "function" (r2c-obj "name" name)))

(load (test-source "kernel-tool-dispatch-core.lisp"))
(load (test-source "kernel-tool-dispatch-shadow.lisp"))
(agent:tool-dispatch-shadow-initialize
 (namestring (test-source "kernel-tool-dispatch-module.sexp")))
(load (test-source "kernel-tool-dispatch-shadow-runtime.lisp"))

(setf agent::*tools* (vector (r2c-tool "lisp-eval")))

(let* ((calls 0)
       (incumbent (lambda (call)
                    (declare (ignore call))
                    (incf calls)
                    (values :first 2 "third"))))
  (setf (fdefinition 'agent::execute) incumbent)
  (let ((first-report (agent:tool-dispatch-shadow-runtime-install))
        (installed (fdefinition 'agent::execute)))
    (r2c-check "first install reports exact installed ownership"
               (and (gethash "installed" first-report)
                    (not (gethash "ownership_conflict" first-report))))
    (agent:tool-dispatch-shadow-runtime-install)
    (r2c-check "repeat install is idempotent and does not stack"
               (eq installed (fdefinition 'agent::execute)))
    (multiple-value-bind (one two three) (agent::execute (r2c-call "lisp-eval"))
      (r2c-check "installed wrapper preserves multiple values"
                 (and (eq one :first) (= two 2) (string= three "third"))))
    (r2c-check "installed wrapper invokes incumbent exactly once" (= calls 1))
    (agent:tool-dispatch-shadow-runtime-uninstall)
    (r2c-check "uninstall restores exact captured function identity"
               (eq incumbent (fdefinition 'agent::execute)))))

(let ((incumbent (lambda (call) (declare (ignore call)) (values))))
  (setf (fdefinition 'agent::execute) incumbent)
  (agent:tool-dispatch-shadow-runtime-install)
  (r2c-check "zero incumbent values remain zero values"
             (null (multiple-value-list (agent::execute (r2c-call "lisp-eval")))))
  (agent:tool-dispatch-shadow-runtime-uninstall))

(let* ((original (make-condition 'simple-error
                                  :format-control "same-condition"
                                  :format-arguments nil))
       (calls 0)
       (incumbent (lambda (call)
                    (declare (ignore call)) (incf calls) (error original)))
       (caught nil))
  (setf (fdefinition 'agent::execute) incumbent)
  (agent:tool-dispatch-shadow-runtime-install)
  (handler-case (agent::execute (r2c-call "lisp-eval"))
    (error (condition) (setf caught condition)))
  (r2c-check "wrapper exposes the exact incumbent condition object"
             (and (eq caught original) (= calls 1)))
  (agent:tool-dispatch-shadow-runtime-uninstall))

(let* ((event-trace nil)
       (incumbent (lambda (call)
                    (declare (ignore call))
                    (setf event-trace
                          (append event-trace '(:before :handler :after)))
                    :ok)))
  (setf (fdefinition 'agent::execute) incumbent)
  (agent:tool-dispatch-shadow-runtime-install)
  (let ((agent::*tool-dispatch-shadow-runtime-sink-function*
          (lambda (report)
            (setf event-trace (append event-trace '(:shadow)))
            (agent::%tool-dispatch-shadow-runtime-record report))))
    (agent::execute (r2c-call "lisp-eval")))
  (r2c-check "shadow observation precedes unchanged incumbent event order"
             (equal event-trace '(:shadow :before :handler :after)))
  (agent:tool-dispatch-shadow-runtime-uninstall))

(let* ((calls 0)
       (incumbent (lambda (call) (declare (ignore call)) (incf calls) :legacy)))
  (setf (fdefinition 'agent::execute) incumbent)
  (agent:tool-dispatch-shadow-runtime-install)
  (let ((agent::*tool-dispatch-shadow-runtime-sink-function*
          (lambda (report) (declare (ignore report)) (error "recorder failed"))))
    (r2c-check "recorder failure cannot change incumbent result"
               (eq :legacy (agent::execute (r2c-call "lisp-eval")))))
  (r2c-check "recorder failure cannot retry incumbent" (= calls 1))
  (agent:tool-dispatch-shadow-runtime-uninstall))

(let ((incumbent (lambda (call) (declare (ignore call)) :ok))
      (unexpected (lambda (call) (declare (ignore call)) :unexpected)))
  (setf (fdefinition 'agent::execute) incumbent)
  (agent:tool-dispatch-shadow-runtime-install)
  (let ((wrapper (fdefinition 'agent::execute)))
    (setf (fdefinition 'agent::execute) unexpected)
    (r2c-check "unexpected execute owner makes install fail closed"
               (r2c-error #'agent:tool-dispatch-shadow-runtime-install))
    (r2c-check "conflicting install does not overwrite unexpected owner"
               (eq unexpected (fdefinition 'agent::execute)))
    (r2c-check "conflicting uninstall fails closed"
               (r2c-error #'agent:tool-dispatch-shadow-runtime-uninstall))
    (r2c-check "conflicting uninstall preserves unexpected owner"
               (eq unexpected (fdefinition 'agent::execute)))
    (setf (fdefinition 'agent::execute) wrapper)
    (agent:tool-dispatch-shadow-runtime-uninstall)
    (r2c-check "resolved conflict can restore exact incumbent"
               (eq incumbent (fdefinition 'agent::execute)))))

(let ((saved (fdefinition 'agent::execute)))
  (fmakunbound 'agent::execute)
  (r2c-check "missing execute makes installation fail closed"
             (r2c-error #'agent:tool-dispatch-shadow-runtime-install))
  (setf (fdefinition 'agent::execute) saved))

;; Reset only fixture-owned in-memory telemetry, then overflow concurrently.
(bt:with-lock-held (agent::*tool-dispatch-shadow-runtime-lock*)
  (setf agent::*tool-dispatch-shadow-runtime-observations* nil
        agent::*tool-dispatch-shadow-runtime-sequence* 0))
(setf (fdefinition 'agent::execute)
      (lambda (call) (declare (ignore call)) :ok))
(agent:tool-dispatch-shadow-runtime-install)
(let ((threads
        (loop for worker below 8
              collect
              (bt:make-thread
               (lambda ()
                 (loop repeat 10 do
                   (agent::execute
                    (r2c-call (if (evenp worker) "lisp-eval" "secret-unknown")
                              "{\"secret\":\"do-not-retain\"}"))))))))
  (dolist (thread threads) (bt:join-thread thread)))
(let* ((report (agent:tool-dispatch-shadow-runtime-report))
       (recent (agent:tool-dispatch-shadow-runtime-recent))
       (sequences (loop for row across recent collect (gethash "sequence" row))))
  (r2c-check "concurrent overflow retains exactly the 64-entry bound"
             (and (= 64 (gethash "capacity" report))
                  (= 64 (gethash "observed_count" report))
                  (= 64 (length recent))
                  (= 80 (gethash "sequence" report))))
  (r2c-check "bounded ring is newest first with unique sequence numbers"
             (and (= 80 (first sequences))
                  (= 17 (car (last sequences)))
                  (= 64 (length (remove-duplicates sequences)))))
  (r2c-check "unknown input text and all call content are absent"
             (loop for row across recent
                   always (and (member (gethash "classification" row)
                                       '("match" "unknown") :test #'string=)
                               (not (equal (gethash "recognized_name" row)
                                           "secret-unknown"))
                               (null (gethash "id" row))
                               (null (gethash "arguments" row))
                               (null (gethash "result" row))
                               (null (gethash "error" row))
                               (null (gethash "timestamp" row)))))
  (when (plusp (length recent))
    (setf (gethash "classification" (aref recent 0)) "tampered")
    (r2c-check "recent endpoint returns copies of retained objects"
               (not (string= "tampered"
                             (gethash "classification"
                                      (aref (agent:tool-dispatch-shadow-runtime-recent)
                                            0)))))))
(agent:tool-dispatch-shadow-runtime-uninstall)

(let ((source (uiop:read-file-string
               (namestring (test-source "kernel-tool-dispatch-shadow-runtime.lisp")))))
  (r2c-check "runtime contains no impure adapter or authority primitive"
             (every (lambda (needle)
                      (null (search needle source :test #'char-equal)))
                    '("raw-call-model" "(call-model" "postmodern" "pomo:"
                      "log-event" "memory-write" "telegram-send"
                      "public-outbound" "propose-loop" "lisp-eval"))))

(format t "RESULT kernel-tool-dispatch-shadow-runtime: ~d passed, ~d failed~%"
        *r2c-pass* *r2c-fail*)
(when (plusp *r2c-fail*) (sb-ext:exit :code 1))
