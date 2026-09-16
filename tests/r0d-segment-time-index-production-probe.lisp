(in-package :agent)

(ql:quickload '(:bordeaux-threads :shasht) :silent t)

(defvar *r0d-segment-probe-pass* 0)
(defvar *r0d-segment-probe-fail* 0)

(defun r0d-segment-probe-check (name condition)
  (if condition
      (progn (incf *r0d-segment-probe-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *r0d-segment-probe-fail*) (format t "  FAIL ~a~%" name))))

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

(let* ((temporary-root #P"/tmp/pai-r0d-segment-time-index-probe/")
       (legacy #P"/workspace/state/events.jsonl")
       (segments #P"/workspace/state/event-log/segments/")
       (watermark #P"/workspace/state/event-log/watermark.json")
       (legacy-index #P"/workspace/state/event-log/legacy-index.json")
       (segment-indexes (merge-pathnames "segment-indexes/" temporary-root))
       (legacy-before (%event-file-byte-length legacy)))
  (when (probe-file temporary-root)
    (uiop:delete-directory-tree temporary-root :validate t
                                               :if-does-not-exist :ignore))
  (unwind-protect
      (let ((*event-log-file* legacy)
            (*event-log-segment-directory* segments)
            (*event-log-watermark-file* watermark)
            (*event-log-legacy-index-file* legacy-index)
            (*event-log-segment-index-directory* segment-indexes)
            (*event-log-segment-index-stride* 10)
            (*event-log-segmentation-enabled* t)
            (*event-log-segmentation-ready-p* nil)
            (*event-next-id* 0)
            (*event-ring* nil))
        (format t "~%== production-ledger segment time-index read-only probe ==~%")
        (let* ((paths (%event-segment-paths))
               (segment-before
                 (mapcar (lambda (path)
                           (cons (file-namestring path)
                                 (%event-file-byte-length path)))
                         paths))
               (started (get-internal-real-time))
               (rebuilt (event-log-ensure-segment-indexes))
               (seconds (/ (- (get-internal-real-time) started)
                           (float internal-time-units-per-second 1.0d0)))
               (indexes (mapcar #'%event-read-segment-index paths))
               (latest-time
                 (reduce #'max indexes
                         :key (lambda (index)
                                (gethash "maximum_timestamp_universal" index))))
               (from (- latest-time 3600))
               (legacy-start (%event-legacy-start-offset from))
               (planned-segment-bytes 0))
          (dolist (path paths)
            (multiple-value-bind (scan-p start)
                (%event-segment-scan-plan path from nil)
              (when scan-p
                (incf planned-segment-bytes
                      (- (%event-file-byte-length path) start)))))
          (format t "[probe] rebuilt=~d seconds=~,3f segments=~d legacy-tail=~d segment-tail=~d~%"
                  rebuilt seconds (length paths) (- legacy-before legacy-start)
                  planned-segment-bytes)
          (r0d-segment-probe-check
           "every production segment has an exact verified sidecar"
           (and paths
                (every #'identity indexes)
                (every (lambda (pair)
                         (= (gethash "source_byte_length" (car pair))
                            (%event-file-byte-length (cdr pair))))
                       (mapcar #'cons indexes paths))))
          (r0d-segment-probe-check
           "crash-era one-hour replay plans less than half the segment bytes"
           (< planned-segment-bytes
              (/ (reduce #'+ paths :key #'%event-file-byte-length) 2)))
          (r0d-segment-probe-check
           "a pre-segment time range skips every complete segment"
           (every
            (lambda (pair)
              (multiple-value-bind (scan-p start)
                  (%event-segment-scan-plan
                   (cdr pair) nil
                   (1- (gethash "minimum_timestamp_universal" (car pair))))
                (declare (ignore start))
                (not scan-p)))
            (mapcar #'cons indexes paths)))
          (sb-ext:gc :full t)
          (let ((before-consing (sb-ext:get-bytes-consed))
                (reference-ids nil)
                (stable-p t))
            (dotimes (iteration 12)
              (let* ((events
                       (replay-events
                        :from from :limit 5000
                        :exclude-types '("model-request" "model-response")))
                     (ids (mapcar (lambda (event) (gethash "id" event)) events)))
                (if (zerop iteration)
                    (setf reference-ids ids)
                    (unless (equal reference-ids ids)
                      (setf stable-p nil)))))
            (let ((consed (- (sb-ext:get-bytes-consed) before-consing)))
              (format t "[probe] twelve_replays rows=~d bytes_consed=~d~%"
                      (length reference-ids) consed)
              (r0d-segment-probe-check
               "twelve dashboard-shaped replays are deterministic and bounded"
               (and stable-p (<= (length reference-ids) 5000)))))
          (r0d-segment-probe-check
           "read-only probe leaves legacy and segment bytes exact"
           (and (= legacy-before (%event-file-byte-length legacy))
                (equal segment-before
                       (mapcar (lambda (path)
                                 (cons (file-namestring path)
                                       (%event-file-byte-length path)))
                               paths))))))
    (when (probe-file temporary-root)
      (uiop:delete-directory-tree temporary-root :validate t
                                                 :if-does-not-exist :ignore))))

(format t "~%R0D SEGMENT TIME-INDEX PROBE: ~d passed, ~d failed.~%"
        *r0d-segment-probe-pass* *r0d-segment-probe-fail*)
(when (plusp *r0d-segment-probe-fail*) (uiop:quit 1))
