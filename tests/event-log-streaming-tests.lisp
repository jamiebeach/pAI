(in-package :agent)

(ql:quickload '(:bordeaux-threads :shasht) :silent t)

(defvar *event-stream-pass* 0)
(defvar *event-stream-fail* 0)
(defun event-stream-check (name condition)
  (if condition
      (progn (incf *event-stream-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *event-stream-fail*) (format t "  FAIL ~a~%" name))))

;; EVENT-LOG's wrappers need legacy call points, but this test exercises only
;; durable scan/replay behavior.
(unless (fboundp 'auto-turn)
  (setf (fdefinition 'auto-turn) (lambda (prompt) prompt)))
(unless (fboundp 'execute)
  (setf (fdefinition 'execute) (lambda (&rest arguments)
                                 (declare (ignore arguments)) nil)))
(unless (fboundp 'propose-loop)
  (setf (fdefinition 'propose-loop) (lambda (&rest arguments)
                                      (declare (ignore arguments)) nil)))
(unless (boundp '*tools*) (defparameter *tools* (vector)))

(load (test-source "event-log.lisp"))

(event-stream-check
 "event ID restoration is registered before initialization writes"
 (member 'event-log-restore-next-id
         (cdr (assoc :configure (init-phase-actions)))))

(let ((saved-log (fdefinition 'log-event))
      (saved-base (fdefinition 'pai-base-execute-eventlog))
      (events nil)
      (next-id 0)
      (call (obj "id" "port-equivalence" "function"
                 (obj "name" "fixture" "arguments" "{}"))))
  (unwind-protect
       (progn
         (setf (fdefinition 'log-event)
               (lambda (type payload &key caused-by)
                 (declare (ignore payload caused-by))
                 (push type events)
                 (incf next-id))
               (fdefinition 'pai-base-execute-eventlog)
               (lambda (tool-call) (declare (ignore tool-call)) :fixture))
         (let ((wrapper-result (execute call))
               (wrapper-events nil))
           (setf wrapper-events (reverse events) events nil)
           (let ((port-result
                   (call-with-tool-event-observation
                    call (lambda (tool-call)
                           (declare (ignore tool-call)) :fixture))))
             (event-stream-check
              "callable event port preserves legacy wrapper result and order"
              (and (eq wrapper-result port-result)
                   (equal wrapper-events (reverse events))
                   (equal wrapper-events '("tool-call" "tool-result")))))))
    (setf (fdefinition 'log-event) saved-log
          (fdefinition 'pai-base-execute-eventlog) saved-base)))

(let* ((path #P"/tmp/pai-event-streaming-tests.jsonl")
       (*event-log-file* path)
       (*event-next-id* 0)
       (*event-ring* nil))
  (ignore-errors (delete-file path))
  (unwind-protect
      (progn
        (let ((*event-next-id* 0))
          (multiple-value-bind (receipt-id persisted-p receipt)
              (log-event "early" (obj "value" 1))
            (event-stream-check
             "successful append returns a stamped durable receipt"
             (and (= 1 receipt-id) persisted-p
                  (hash-table-p receipt)
                  (= receipt-id (gethash "id" receipt))
                  (string= "early" (gethash "type" receipt)))))
          (sleep 1)
          (let ((boundary (get-universal-time)))
            (log-event "late-a" (obj "value" 2))
            (log-event "late-b" (obj "value" 3))
            (let ((bounded (replay-events :from boundary)))
              (event-stream-check "bounded replay retains chronological order"
                                  (equal '("late-a" "late-b")
                                         (mapcar (lambda (event)
                                                   (gethash "type" event))
                                                 bounded)))
              (event-stream-check "bounded replay excludes older rows"
                                  (= 2 (length bounded))))))
        (setf *event-next-id* 0)
        (event-stream-check "watermark restore scans without materializing replay"
                            (= 3 (%event-restore-next-id)))
        (event-stream-check "full replay remains backward compatible"
                            (= 3 (length (replay-events))))
        (with-open-file (out path :direction :output :if-exists :append)
          (write-line "{malformed" out))
        (event-stream-check "corrupt in-window row preserves empty fail-soft replay"
                            (null (replay-events)))
        (event-stream-check "corrupt ledger cannot restore a partial watermark"
                            (zerop (%event-restore-next-id))))
    (ignore-errors (delete-file path))))

(format t "~%EVENT STREAMING TESTS: ~d passed, ~d failed.~%"
        *event-stream-pass* *event-stream-fail*)
(when (plusp *event-stream-fail*) (uiop:quit 1))
