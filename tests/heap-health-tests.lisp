(in-package :agent)

(defvar *heap-health-test-passed* 0)
(defvar *heap-health-test-failed* 0)
(defvar *heap-health-autostart-p* nil)
(defvar *autonomous-write-mode* :normal)

(defun heap-health-test-check (name condition)
  (if condition
      (progn (incf *heap-health-test-passed*) (format t "PASS ~a~%" name))
      (progn (incf *heap-health-test-failed*) (format t "FAIL ~a~%" name))))

(load (test-source "heap-health.lisp"))

(heap-health-test-check "pressure guard samples at least once per minute"
                        (<= *heap-health-interval-seconds* 60))
(heap-health-test-check "incident guard samples every five seconds"
                        (= 5 *heap-health-interval-seconds*))
(heap-health-test-check "incident guard observes pressure with promotion headroom"
                        (= 0.30d0 *heap-health-warning-ratio*))
(heap-health-test-check "critical autonomy guard starts before heap exhaustion"
                        (= 0.65d0 *heap-health-critical-ratio*))
(heap-health-test-check "incident guard can recover a second burst promptly"
                        (= 30 *heap-health-full-gc-cooldown-seconds*))
(heap-health-test-check "sample history retains at least one day"
                        (>= (* *heap-health-interval-seconds*
                               *heap-health-sample-cap*)
                            86400))

(let* ((usage 10)
       (gc-count 0)
       (events nil)
       (*heap-health-samples* nil)
       (*heap-health-last-full-gc-at* 0)
       (*heap-health-usage-fn* (lambda () usage))
       (*heap-health-limit-fn* (lambda () 100))
       (*heap-health-consed-fn* (lambda () 1234))
       (*heap-health-gc-fn* (lambda () (incf gc-count) (setf usage 20)))
       (*heap-health-event-fn*
         (lambda (type payload) (push (list type payload) events))))
  (let ((normal (heap-health-sample :now 1000)))
    (heap-health-test-check "normal sample does not force GC" (zerop gc-count))
    (heap-health-test-check "normal sample reports ok"
                            (string= "ok" (gethash "status" normal))))
  (setf usage 70)
  (let ((recovered (heap-health-sample :allow-gc t :now 2000)))
    (heap-health-test-check "explicit diagnostic GC remains available" (= gc-count 1))
    (heap-health-test-check "post-GC recovery is distinguished"
                            (string= "recovered-after-gc"
                                     (gethash "status" recovered)))
    (heap-health-test-check "post-GC sample records both usages"
                            (and (= 70 (gethash "usage_before_bytes" recovered))
                                 (= 20 (gethash "usage_bytes" recovered)))))
  (setf usage 90
        *autonomous-write-mode* :normal
        *heap-health-gc-fn* (lambda () (incf gc-count) (setf usage 85)))
  (let ((critical (heap-health-sample :now 3000)))
    (heap-health-test-check "routine critical sample does not copy a replay generation"
                            (= gc-count 1))
    (heap-health-test-check "retained critical heap is explicit"
                            (string= "critical" (gethash "status" critical)))
    (heap-health-test-check "critical retained heap pauses autonomy"
                            (and (eq *autonomous-write-mode* :paused)
                                 (gethash "autonomy_paused" critical)))
    (heap-health-test-check "critical pressure has a distinct event"
                            (string= "heap-pressure" (first (first events)))))
  (let ((*heap-health-sample-cap* 3))
    (setf usage 10)
    (dotimes (i 5) (heap-health-sample :allow-gc nil :now (+ 4000 i)))
    (heap-health-test-check "sample history is bounded"
                            (= 3 (length *heap-health-samples*))))
  (let ((report (heap-health-report)))
    (heap-health-test-check "report exposes current usage and limit"
                            (and (= 10 (gethash "usage_bytes" report))
                                 (= 100 (gethash "limit_bytes" report))))
    (heap-health-test-check "report exposes paused fail-safe state"
                            (string= "paused"
                                     (gethash "autonomous_write_mode" report)))))

(format t "~%HEAP HEALTH TESTS: ~d passed, ~d failed.~%"
        *heap-health-test-passed* *heap-health-test-failed*)
(when (plusp *heap-health-test-failed*) (uiop:quit 1))
