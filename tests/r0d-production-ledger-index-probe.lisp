(in-package :agent)

(ql:quickload '(:bordeaux-threads :shasht) :silent t)

(defvar *r0d-ledger-pass* 0)
(defvar *r0d-ledger-fail* 0)

(defun r0d-ledger-check (name condition)
  (if condition
      (progn (incf *r0d-ledger-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *r0d-ledger-fail*) (format t "  FAIL ~a~%" name))))

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

(let* ((root #P"/tmp/pai-r0d-production-ledger-probe/")
       (legacy #P"/workspace/state/events.jsonl")
       (before-size (%event-file-byte-length legacy)))
  (when (probe-file root)
    (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))
  (unwind-protect
      (let ((*event-log-file* legacy)
            (*event-log-segment-directory* (merge-pathnames "segments/" root))
            (*event-log-watermark-file* (merge-pathnames "watermark.json" root))
            (*event-log-legacy-index-file*
              (merge-pathnames "legacy-index.json" root))
            (*event-checkpoint-directory* (merge-pathnames "checkpoints/" root))
            (*event-log-segmentation-enabled* t)
            (*event-log-segmentation-ready-p* nil)
            (*event-next-id* 0)
            (*event-ring* nil))
        (format t "~%== production-ledger read-only cutover probe ==~%")
        (let* ((started (get-internal-real-time))
               (watermark (event-log-initialize-segmentation))
               (seconds
                 (/ (- (get-internal-real-time) started)
                    (float internal-time-units-per-second 1.0d0)))
               (index (%event-read-legacy-index))
               (entries (and index (gethash "entries" index)))
               (two-hours-ago (- (get-universal-time) (* 2 3600)))
               (offset (%event-legacy-start-offset two-hours-ago)))
          (format t "[probe] cutover scan seconds=~,3f entries=~d seek=~d~%"
                  seconds (if entries (length entries) 0) offset)
          (r0d-ledger-check "cutover derives the production last id"
                            (= 91447 (gethash "last_reserved_id" watermark)))
          (r0d-ledger-check "verified sparse index covers the immutable ledger"
                            (and index (> (length entries) 900)
                                 (= before-size
                                    (gethash "source_byte_length" index))))
          (r0d-ledger-check "two-hour recovery seeks past historical bytes"
                            (> offset 180000000))
          (let ((conversation
                  (replay-events
                   :from two-hours-ago
                   :types '("user-message" "agent-message" "tool-call"
                            "tool-result" "turn-capture-ready"
                            "turn-capture-complete"))))
            (r0d-ledger-check "typed reconciliation replay is bounded and ordered"
                              (and conversation
                                   (< (length conversation) 5000)
                                   (apply #'<
                                          (mapcar (lambda (event)
                                                    (gethash "id" event))
                                                  conversation)))))
          (let ((dashboard
                  (replay-events
                   :from (- (get-universal-time) 3600)
                   :limit 1000
                   :exclude-types '("model-request" "model-response"))))
            (r0d-ledger-check "dashboard replay caps retained event objects"
                              (<= (length dashboard) 1000)))
          (r0d-ledger-check "read-only qualification leaves legacy bytes exact"
                            (= before-size (%event-file-byte-length legacy)))))
    (when (probe-file root)
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist :ignore))))

(format t "~%R0D PRODUCTION LEDGER PROBE: ~d passed, ~d failed.~%"
        *r0d-ledger-pass* *r0d-ledger-fail*)
(when (plusp *r0d-ledger-fail*) (uiop:quit 1))
