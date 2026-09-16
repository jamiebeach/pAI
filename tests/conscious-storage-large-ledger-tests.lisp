(in-package :agent)

(ql:quickload '(:cffi :shasht :ironclad :bordeaux-threads) :silent t)

(defvar *csll-pass* 0)
(defvar *csll-fail* 0)

(defun csll-check (name condition)
  (if condition
      (progn (incf *csll-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *csll-fail*) (format t "  FAIL ~a~%" name))))

(defun csll-delete-db (path)
  (dolist (candidate (list path
                           (pathname (concatenate 'string (namestring path) "-wal"))
                           (pathname (concatenate 'string (namestring path) "-shm"))))
    (when (probe-file candidate) (delete-file candidate))))

(defun csll-measure (thunk)
  "Return values-list, elapsed ms, allocated bytes and sampled heap growth."
  (sb-ext:gc :full t)
  (let* ((running t)
         (baseline (sb-kernel:dynamic-usage))
         (peak baseline)
         (consed (sb-ext:get-bytes-consed))
         (started (get-internal-real-time))
         (sampler
           (bt:make-thread
            (lambda ()
              (loop while running
                    do (setf peak (max peak (sb-kernel:dynamic-usage)))
                       (sleep 0.005)))
            :name "projection-heap-sampler"))
         (result nil))
    (unwind-protect
        (setf result (multiple-value-list (funcall thunk)))
      (setf running nil)
      (bt:join-thread sampler))
    (values result
            (round (* 1000 (/ (- (get-internal-real-time) started)
                              internal-time-units-per-second)))
            (- (sb-ext:get-bytes-consed) consed)
            (max 0 (- peak baseline)))))

(dolist (file '("policy.lisp" "stimulus.lisp" "census.lisp" "concern.lisp"
                "codelets.lisp" "context.lisp" "inbox.lisp" "attention.lisp"
                "mind/conscious/lifecycle.lisp" "lifecycle-semantics.lisp" "state.lisp"
                "storage-substrate.lisp" "sqlite-storage.lisp"
                "sqlite-import.lisp" "storage-projection.lisp"))
  (load (test-source file)))

(format t "~%== conscious storage 59 MiB measurement ==~%")

(let* ((root (test-state-dir))
       (database (merge-pathnames "conscious-storage-large.sqlite3" root))
       (backend nil)
       (agent-id "large-ledger-dev")
       (payload-bytes (* 59 1024 1024))
       (checkpoint-ms 0) (checkpoint-consed 0) (checkpoint-peak 0)
       (restore-ms 0) (restore-consed 0) (restore-peak 0))
  (csll-delete-db database)
  (unwind-protect
      (progn
        (setf backend (make-sqlite-storage database))
        (storage-append-event
         backend "heap-health"
         (obj "status" "synthetic" "payload"
              (make-string payload-bytes :initial-element #\x))
         :agent-id agent-id)
        (storage-append-event
         backend "user-message"
         (obj "text" "outstanding after large journal row" "channel" "cli"
              "origin_runtime_revision" "conscious-q5-v2")
         :agent-id agent-id)
        (multiple-value-bind (values elapsed consed peak)
            (csll-measure
             (lambda ()
               (conscious-storage-build-checkpoint
                backend :agent-id agent-id :now 10000)))
          (setf checkpoint-ms elapsed checkpoint-consed consed
                checkpoint-peak peak)
          (let ((report (first values)))
            (csll-check "first checkpoint streams the complete large source"
                        (>= (gethash "source_json_bytes" report) payload-bytes))
            (csll-check "large terminal payload is absent from bounded prefix"
                        (and (< (gethash "capsule_event_json_bytes" report)
                                (* 1024 1024))
                             (<= (gethash "capsule_event_count" report) 3)))))
        (storage-append-event
         backend "heap-health" (obj "status" "tail") :agent-id agent-id)
        (multiple-value-bind (values elapsed consed peak)
            (csll-measure
             (lambda ()
               (conscious-storage-restore-checkpoint-tail
                backend :agent-id agent-id :now 10001)))
          (setf restore-ms elapsed restore-consed consed restore-peak peak)
          (let ((report (second values)))
            (csll-check "subsequent restore reads only the physical tail"
                        (and (= 1 (gethash "tail_event_count" report))
                             (= 0 (gethash "hydrated_prefix_reference_count"
                                           report)))))))
    (when backend (ignore-errors (storage-close backend)))
    (csll-delete-db database))
  (format t "  measurement checkpoint_ms=~d checkpoint_allocated=~d checkpoint_peak_growth=~d~%"
          checkpoint-ms checkpoint-consed checkpoint-peak)
  (format t "  measurement restore_ms=~d restore_allocated=~d restore_peak_growth=~d~%"
          restore-ms restore-consed restore-peak))

(format t "~%~d passed, ~d failed~%" *csll-pass* *csll-fail*)
(when (plusp *csll-fail*) (error "large-ledger storage tests failed"))
