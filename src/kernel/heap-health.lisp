;;;; heap-health.lisp -- bounded SBCL heap observability and fail-safe.
;;;;
;;;; Docker can report PID 1 as running after SBCL has entered its low-level
;;;; debugger. Sample the Lisp dynamic space directly, force a full GC while
;;;; there is still headroom, and pause autonomous generation if retained heap
;;;; remains critical. Conversation remains available; resuming autonomy is
;;;; deliberately an operator decision after the cause is understood.

(in-package :agent)

(export '(heap-health-report heap-health-sample
          heap-health-start heap-health-stop))

(defparameter *heap-health-interval-seconds* 5)
;; Warning is observational. A full copying GC at this boundary caused the
;; two September heap failures while a large replay generation was live.
(defparameter *heap-health-warning-ratio* 0.30d0)
(defparameter *heap-health-critical-ratio* 0.65d0)
(defparameter *heap-health-full-gc-cooldown-seconds* 30)
(defparameter *heap-health-sample-cap* 17280)

(defvar *heap-health-samples* nil "Newest first; numeric/content-free only.")
(defvar *heap-health-thread* nil)
(defvar *heap-health-stop-requested* nil)
(defvar *heap-health-last-full-gc-at* 0)
(defvar *heap-health-lock* (bt:make-lock "heap-health"))
(defvar *heap-health-autostart-p* t)

;; Injectable seams make threshold behavior deterministic without allocating
;; hundreds of megabytes or invoking a real full GC in tests.
(defvar *heap-health-usage-fn* #'sb-kernel:dynamic-usage)
(defvar *heap-health-limit-fn* #'sb-ext:dynamic-space-size)
(defvar *heap-health-consed-fn* #'sb-ext:get-bytes-consed)
(defvar *heap-health-gc-fn* (lambda () (sb-ext:gc :full t)))
(defvar *heap-health-event-fn* nil)

(defun %heap-health-ratio (usage limit)
  (if (and (numberp usage) (numberp limit) (plusp limit))
      (/ (float usage 1.0d0) (float limit 1.0d0))
      0.0d0))

(defun %heap-health-thread-alive-p ()
  (not (null (and *heap-health-thread*
                  (bt:thread-alive-p *heap-health-thread*)))))

(defun %heap-health-log (type payload)
  (handler-case
      (cond (*heap-health-event-fn* (funcall *heap-health-event-fn* type payload))
            ((fboundp 'log-event) (funcall 'log-event type payload)))
    (error () nil)))

(defun %heap-health-current ()
  (let* ((usage (funcall *heap-health-usage-fn*))
         (limit (funcall *heap-health-limit-fn*)))
    (values usage limit (%heap-health-ratio usage limit))))

(defun %heap-health-remember (sample)
  (bt:with-lock-held (*heap-health-lock*)
    (push sample *heap-health-samples*)
    (when (> (length *heap-health-samples*) *heap-health-sample-cap*)
      (setf *heap-health-samples*
            (subseq *heap-health-samples* 0 *heap-health-sample-cap*))))
  sample)

(defun heap-health-sample (&key (allow-gc nil) (now (get-universal-time)))
  "Record one content-free heap sample and return it.

Automatic sampling never initiates a full copying GC. ALLOW-GC is retained
only as an explicit diagnostic/test seam. At critical pressure autonomous
generation is paused before exhaustion; public conversation remains enabled."
  (multiple-value-bind (before limit before-ratio) (%heap-health-current)
    (let* ((gc-eligible
             (and allow-gc
                  (>= before-ratio *heap-health-warning-ratio*)
                  (>= (- now *heap-health-last-full-gc-at*)
                      *heap-health-full-gc-cooldown-seconds*)))
           (gc-ran nil)
           (after before)
           (after-ratio before-ratio))
      (when gc-eligible
        (setf gc-ran t
              *heap-health-last-full-gc-at* now)
        (funcall *heap-health-gc-fn*)
        (multiple-value-setq (after limit after-ratio) (%heap-health-current)))
      (let* ((critical (>= after-ratio *heap-health-critical-ratio*))
             (paused nil)
             (status (cond (critical "critical")
                           ((and gc-ran (< after-ratio *heap-health-warning-ratio*))
                            "recovered-after-gc")
                           ((>= after-ratio *heap-health-warning-ratio*) "warning")
                           (t "ok"))))
        (when (and critical (boundp '*autonomous-write-mode*)
                   (not (eq *autonomous-write-mode* :paused)))
          (setf *autonomous-write-mode* :paused
                paused t))
        (let ((sample
                (obj "schema_version" 1
                     "sampled_at" now
                     "status" status
                     "usage_before_bytes" before
                     "usage_bytes" after
                     "limit_bytes" limit
                     "ratio_before" before-ratio
                     "ratio" after-ratio
                     "bytes_consed_total" (funcall *heap-health-consed-fn*)
                     "full_gc_ran" (if gc-ran t nil)
                     "autonomy_paused" (if paused t nil))))
          (%heap-health-remember sample)
          (%heap-health-log (if critical "heap-pressure" "heap-health") sample)
          sample)))))

(defun heap-health-report ()
  (multiple-value-bind (usage limit ratio) (%heap-health-current)
    (bt:with-lock-held (*heap-health-lock*)
      (obj "schema_version" 1
           "usage_bytes" usage
           "limit_bytes" limit
           "ratio" ratio
           "warning_ratio" *heap-health-warning-ratio*
           "critical_ratio" *heap-health-critical-ratio*
           "thread_alive" (%heap-health-thread-alive-p)
           "sample_count" (length *heap-health-samples*)
           "latest" (or (first *heap-health-samples*) :null)
           "autonomous_write_mode"
           (if (boundp '*autonomous-write-mode*)
               (string-downcase (symbol-name *autonomous-write-mode*))
               "unknown")))))

(defun heap-health-start ()
  (unless (%heap-health-thread-alive-p)
    (setf *heap-health-stop-requested* nil
          *heap-health-thread*
          (bt:make-thread
           (lambda ()
             (loop until *heap-health-stop-requested*
                   do (handler-case (heap-health-sample)
                        (error (condition)
                          (format t "~&[heap-health] sample failed: ~a~%" condition)))
                      (sleep *heap-health-interval-seconds*)))
           :name "heap-health")))
  t)

(defun heap-health-stop (&optional (timeout 3))
  (setf *heap-health-stop-requested* t)
  (loop with deadline = (+ (get-internal-real-time)
                           (* timeout internal-time-units-per-second))
        while (and (%heap-health-thread-alive-p)
                   (< (get-internal-real-time) deadline))
        do (sleep 0.05d0))
  (not (%heap-health-thread-alive-p)))

(when *heap-health-autostart-p* (heap-health-start))
