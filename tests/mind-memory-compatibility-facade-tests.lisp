(ql:quickload '(:postmodern :shasht :ironclad :babel) :silent t)

(defpackage :agent (:use :cl))
(in-package :agent)

(defun obj (&rest pairs)
  (loop with table = (make-hash-table :test #'equal)
        for (key value) on pairs by #'cddr
        do (setf (gethash key table) value)
        finally (return table)))

(defmacro with-pg (&body body) `(progn ,@body))

(in-package :cl-user)

(defvar *mind-memory-facade-pass* 0)
(defvar *mind-memory-facade-fail* 0)

(defun mind-memory-facade-check (name condition)
  (if condition
      (progn (incf *mind-memory-facade-pass*) (format t "PASS ~a~%" name))
      (progn (incf *mind-memory-facade-fail*) (format t "FAIL ~a~%" name))))

(defun mind-memory-facade-read-forms (path)
  (with-open-file (stream path :direction :input)
    (loop for form = (read stream nil :eof)
          until (eq form :eof) collect form)))

(defun mind-memory-facade-definitions (name forms)
  (remove-if-not
   (lambda (form)
     (and (consp form) (eq (first form) 'defun)
          (symbolp (second form))
          (string= name (symbol-name (second form)))))
   forms))

(defun mind-memory-facade-thin-target-p (form target)
  (and (= 4 (length form))
       (consp (fourth form))
       (symbolp (first (fourth form)))
       (string= target (symbol-name (first (fourth form))))
       (let ((package (symbol-package (first (fourth form)))))
         (and package
              (string= "PAI.MIND.MEMORY" (package-name package))))))

(mind-memory-facade-check "pure package absent before direct owner load"
                          (null (find-package :pai.mind.memory)))
(load (test-source "memory-architecture.lisp"))
(mind-memory-facade-check "architecture owner loads pure package fallback"
                          (not (null (find-package :pai.mind.memory))))
(load (test-source "memory-atom-candidate.lisp"))

(let* ((architecture-forms
         (mind-memory-facade-read-forms
          (namestring (test-source "memory-architecture.lisp"))))
       (candidate-forms
         (mind-memory-facade-read-forms
          (namestring (test-source "memory-atom-candidate.lisp"))))
       (specs
         `(("MEMORY-ARCHITECTURE-VALIDATE-STATE" "VALIDATE-STATE"
            ,architecture-forms)
           ("MEMORY-ARCHITECTURE-ROW-ELIGIBLE-P" "ROW-ELIGIBLE-P"
            ,architecture-forms)
           ("MEMORY-ATOM-BUILD-MANIFEST" "BUILD-ATOM-MANIFEST"
            ,candidate-forms)
           ("MEMORY-ATOM-BUILD-REQUEST" "BUILD-ATOM-REQUEST"
            ,candidate-forms)
           ("MEMORY-ATOM-VALIDATE-RESPONSE" "VALIDATE-ATOM-RESPONSE"
            ,candidate-forms)
           ("MEMORY-ATOM-CANDIDATE-REPORT" "CAPABILITY-REPORT"
            ,candidate-forms))))
  (dolist (spec specs)
    (let ((definitions (mind-memory-facade-definitions
                        (first spec) (third spec))))
      (mind-memory-facade-check
       (format nil "~a has exactly one owner definition" (first spec))
       (= 1 (length definitions)))
      (mind-memory-facade-check
       (format nil "~a is a thin pure-core forwarder" (first spec))
       (and (= 1 (length definitions))
            (mind-memory-facade-thin-target-p
             (first definitions) (second spec)))))))

(mind-memory-facade-check
 "deleted predecessor functions are entirely absent"
 (every
  (lambda (suffix)
    (let ((name (concatenate 'string "%MEMORY-" suffix)))
      (multiple-value-bind (symbol status) (find-symbol name :agent)
        (declare (ignore status))
        (or (null symbol) (not (fboundp symbol))))))
  '("ARCHITECTURE-R1C-LEGACY-VALIDATE-STATE"
    "ARCHITECTURE-R1C-LEGACY-ROW-ELIGIBLE-P"
    "ATOM-R1C-LEGACY-BUILD-MANIFEST"
    "ATOM-R1C-LEGACY-BUILD-REQUEST"
    "ATOM-R1C-LEGACY-VALIDATE-RESPONSE"
    "ATOM-R1C-LEGACY-CANDIDATE-REPORT")))

(let ((row (agent::obj "agent_id" "test-agent"
                       "disclosure_class" "private"
                       "share_review_status" :null
                       "share_review_event_id" :null
                       "share_reviewed_at" :null)))
  (mind-memory-facade-check
   "architecture eligibility facade matches direct core call"
   (eql (not (null (agent::memory-architecture-row-eligible-p row)))
        (not (null (pai.mind.memory:row-eligible-p row))))))

(let ((report (agent::memory-atom-candidate-report)))
  (mind-memory-facade-check "candidate facade retains no-admission boundary"
                            (null (gethash "admission_available" report)))
  (mind-memory-facade-check "candidate facade retains no-provider boundary"
                            (null (gethash "provider_calls_available" report)))
  (mind-memory-facade-check "candidate facade retains no-delivery boundary"
                            (null (gethash "delivery_authority" report))))

(format t "RESULT mind-memory-compatibility-facade: ~d passed, ~d failed~%"
        *mind-memory-facade-pass* *mind-memory-facade-fail*)
(when (plusp *mind-memory-facade-fail*) (uiop:quit 1))
