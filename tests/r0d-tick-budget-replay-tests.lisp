(defpackage :agent (:use :cl))
(in-package :agent)

(ql:quickload '(:bordeaux-threads :shasht) :silent t)

(defvar *r0d-tick-pass* 0)
(defvar *r0d-tick-fail* 0)
(defvar *r0d-tick-replay-arguments* nil)
(defvar *r0d-tick-replay-calls* 0)
(defvar *event-ring* nil)

(defun r0d-tick-check (name condition)
  (if condition
      (progn (incf *r0d-tick-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *r0d-tick-fail*) (format t "  FAIL ~a~%" name))))

(defun obj (&rest kvs)
  (loop with table = (make-hash-table :test #'equal)
        for (key value) on kvs by #'cddr
        do (setf (gethash key table) value)
        finally (return table)))

(defun r0d-tick-eval-source-form (marker)
  "Evaluate one named top-level form without starting TICK-LOOP's thread."
  (with-open-file (source (namestring (test-source "tick-loop.lisp")))
    (let* ((text (make-string (file-length source)))
           (count (read-sequence text source))
           (start (search marker text :end2 count)))
      (unless start (error "Source form not found: ~a" marker))
      (with-input-from-string (form-source text :start start :end count)
        (eval (read form-source))))))

(dolist (marker '("(defun tick-cost-summary"
                  "(defvar *tick-budget-cost-records*"
                  "(defvar *tick-budget-cost-records-ready-p*"
                  "(defvar *tick-budget-cost-lock*"
                  "(defun %tick-budget-event-time"
                  "(defun %tick-budget-merge-events"
                  "(defun %tick-budget-ensure-cost-records"
                  "(defun %tick-budget-cost-summary"))
  (r0d-tick-eval-source-form marker))

(defun replay-events (&key from to limit types exclude-types)
  (declare (ignore to limit exclude-types))
  (incf *r0d-tick-replay-calls*)
  (setf *r0d-tick-replay-arguments* (list :from from :types types))
  (list (obj "id" 1 "timestamp_universal" 1000000
             "type" "tick-end"
             "payload" (obj "type" "maintenance" "cost" 0.25d0
                            "prompt_tokens" 10 "completion_tokens" 5))))

(format t "~%== tick-budget typed replay ==~%")
(let ((summary (tick-cost-summary :hours 24)))
  (r0d-tick-check
   "cost replay pushes exact tick types into the storage scan"
   (equal '("tick-end" "tick-terminal")
          (getf *r0d-tick-replay-arguments* :types)))
  (r0d-tick-check
   "24-hour bound is retained"
   (integerp (getf *r0d-tick-replay-arguments* :from)))
  (r0d-tick-check
   "typed replay preserves cost and token aggregation"
   (and (= 1 (gethash "tick_count" summary))
        (= 0.25d0 (gethash "total_cost" summary))
        (= 10 (gethash "total_prompt_tokens" summary))
        (= 5 (gethash "total_completion_tokens" summary)))))

(format t "~%== rebuildable tick-budget projection ==~%")
(setf *tick-budget-cost-records* (make-hash-table :test #'equal)
      *tick-budget-cost-records-ready-p* nil
      *r0d-tick-replay-calls* 0
      *event-ring* nil)
(let* ((first (%tick-budget-cost-summary 1000000))
       (second (%tick-budget-cost-summary 1000001)))
  (declare (ignore second))
  (r0d-tick-check "projection hydrates durable history exactly once"
                  (= 1 *r0d-tick-replay-calls*))
  (r0d-tick-check "hydrated cost is exact"
                  (= 0.25d0 (gethash "total_cost" first))))
(setf *event-ring*
      (list (obj "id" 2 "timestamp_universal" 1000002
                 "type" "tick-terminal"
                 "payload" (obj "type" "ruminate" "cost" 0.50d0
                                "prompt_tokens" 20
                                "completion_tokens" 10))))
(let ((merged (%tick-budget-cost-summary 1000002)))
  (r0d-tick-check "ring delta merges without another disk replay"
                  (and (= 1 *r0d-tick-replay-calls*)
                       (= 2 (gethash "tick_count" merged))
                       (= 0.75d0 (gethash "total_cost" merged)))))
(let ((expired (%tick-budget-cost-summary (+ 1000002 (* 24 3600) 1))))
  (r0d-tick-check "records expire after the exact 24-hour window"
                  (and (zerop (gethash "tick_count" expired))
                       (= 1 *r0d-tick-replay-calls*))))

(format t "~%R0D TICK BUDGET REPLAY TESTS: ~d passed, ~d failed.~%"
        *r0d-tick-pass* *r0d-tick-fail*)
(when (plusp *r0d-tick-fail*) (uiop:quit 1))
