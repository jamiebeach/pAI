(defpackage :agent (:use :cl))
(in-package :agent)

(ql:quickload '(:bordeaux-threads :shasht) :silent t)

(defvar *r0d-budget-probe-pass* 0)
(defvar *r0d-budget-probe-fail* 0)

(defun obj (&rest kvs)
  (loop with table = (make-hash-table :test #'equal)
        for (key value) on kvs by #'cddr
        do (setf (gethash key table) value)
        finally (return table)))

(defun r0d-budget-probe-check (name condition)
  (if condition
      (progn (incf *r0d-budget-probe-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *r0d-budget-probe-fail*) (format t "  FAIL ~a~%" name))))

(unless (fboundp 'auto-turn)
  (setf (fdefinition 'auto-turn) (lambda (prompt) prompt)))
(unless (fboundp 'execute)
  (setf (fdefinition 'execute)
        (lambda (&rest arguments) (declare (ignore arguments)) nil)))
(unless (fboundp 'propose-loop)
  (setf (fdefinition 'propose-loop)
        (lambda (&rest arguments) (declare (ignore arguments)) nil)))
(unless (boundp '*tools*) (defparameter *tools* (vector)))

(load (test-source "event-log.lisp"))

(defun r0d-budget-eval-source-form (marker)
  (with-open-file (source (namestring (test-source "tick-loop.lisp")))
    (let* ((text (make-string (file-length source)))
           (count (read-sequence text source))
           (start (search marker text :end2 count)))
      (unless start (error "Source form not found: ~a" marker))
      (with-input-from-string (form-source text :start start :end count)
        (eval (read form-source))))))

(dolist (marker '("(defvar *tick-budget-cost-records*"
                  "(defvar *tick-budget-cost-records-ready-p*"
                  "(defvar *tick-budget-cost-lock*"
                  "(defun %tick-budget-event-time"
                  "(defun %tick-budget-merge-events"
                  "(defun %tick-budget-ensure-cost-records"
                  "(defun %tick-budget-cost-summary"))
  (r0d-budget-eval-source-form marker))

(let* ((legacy #P"/workspace/state/events.jsonl")
       (segments #P"/workspace/state/event-log/segments/")
       (watermark #P"/workspace/state/event-log/watermark.json")
       (legacy-index #P"/workspace/state/event-log/legacy-index.json")
       (segment-indexes #P"/workspace/state/event-log/segment-indexes/")
       (legacy-before (%event-file-byte-length legacy))
       (segment-before
         (mapcar (lambda (path)
                   (cons (file-namestring path) (%event-file-byte-length path)))
                 (let ((*event-log-segment-directory* segments))
                   (%event-segment-paths)))))
  (let ((*event-log-file* legacy)
        (*event-log-segment-directory* segments)
        (*event-log-watermark-file* watermark)
        (*event-log-legacy-index-file* legacy-index)
        (*event-log-segment-index-directory* segment-indexes)
        (*event-log-segmentation-enabled* t)
        (*event-log-segmentation-ready-p* t)
        (*event-ring* nil))
    (format t "~%== production-ledger tick-budget allocation probe ==~%")
    (sb-ext:gc :full t)
    (let* ((real-replay (fdefinition 'replay-events))
           (replay-calls 0)
           (baseline-usage (sb-kernel:dynamic-usage))
           (maximum-usage 0)
           (reference nil)
           (stable-p t)
           (post-hydration-consed 0))
      (setf *tick-budget-cost-records* (make-hash-table :test #'equal)
            *tick-budget-cost-records-ready-p* nil
            *event-ring* nil
            (fdefinition 'replay-events)
            (lambda (&rest arguments)
              (incf replay-calls)
              (apply real-replay arguments)))
      (unwind-protect
          (progn
            (setf reference (%tick-budget-cost-summary))
            (let ((after-hydration (sb-ext:get-bytes-consed)))
              (dotimes (iteration 12)
                (declare (ignore iteration))
                (let ((summary (%tick-budget-cost-summary)))
                  (setf maximum-usage
                        (max maximum-usage (sb-kernel:dynamic-usage)))
                  (unless (and (= (gethash "tick_count" reference)
                                  (gethash "tick_count" summary))
                               (= (gethash "total_cost" reference)
                                  (gethash "total_cost" summary)))
                    (setf stable-p nil))))
              (setf post-hydration-consed
                    (- (sb-ext:get-bytes-consed) after-hydration))))
        (setf (fdefinition 'replay-events) real-replay))
      (format t
              "[probe] hydration_replays=~d rows=~d steady_calls=12 steady_bytes_consed=~d baseline=~d maximum=~d~%"
              replay-calls (gethash "tick_count" reference)
              post-hydration-consed baseline-usage maximum-usage)
      (r0d-budget-probe-check
       "production ledger hydrates exactly once"
       (= 1 replay-calls))
      (r0d-budget-probe-check
       "twelve steady-state budget reads are exact"
       stable-p)
      (r0d-budget-probe-check
       "twelve steady-state reads allocate less than 64 MiB"
       (< post-hydration-consed (* 64 1024 1024)))
      (r0d-budget-probe-check
       "projection remains below one GiB live dynamic usage"
       (< maximum-usage (* 1024 1024 1024))))
    (r0d-budget-probe-check
     "read-only probe preserves legacy and segment lengths"
     (and (= legacy-before (%event-file-byte-length legacy))
          (equal segment-before
                 (mapcar (lambda (path)
                           (cons (file-namestring path)
                                 (%event-file-byte-length path)))
                         (%event-segment-paths)))))))

(format t "~%R0D TICK BUDGET PRODUCTION PROBE: ~d passed, ~d failed.~%"
        *r0d-budget-probe-pass* *r0d-budget-probe-fail*)
(when (plusp *r0d-budget-probe-fail*) (uiop:quit 1))
