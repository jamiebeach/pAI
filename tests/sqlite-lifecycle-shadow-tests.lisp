;;;; harness: full-system
;;;; Q5 lifecycle rows: bounded build, parity, restart, tail and refusal.
(in-package :agent)

(defun sls-events (source)
  (let ((events nil))
    (storage-map-events
     source (lambda (event position)
              (declare (ignore position)) (push event events))
     :agent-id "lifecycle-fixture")
    (nreverse events)))

(defun sls-apply-all (derived source &optional (step #'conscious-lifecycle-shadow-step))
  (loop for row = (storage-shadow-lifecycle-apply-page
                   derived source step :agent-id "lifecycle-fixture" :limit 2)
        until (= (gethash "through_position" row)
                 (storage-head-position source :agent-id "lifecycle-fixture"))
        finally (return row)))

(defun sls-transition (name request source)
  (conscious-lifecycle-transition-payload
   "work:synthetic" name :request-id request
   :lifecycle-kind "model-work"
   :origin-runtime-revision "fixture-v1"
   :actor-runtime-revision "fixture-v1"
   :source-event-id source :reason-code "fixture"
   :occurred-at "2026-01-01T00:00:00Z"))

(defun sls-import-event (id type payload)
  (obj "schema_version" 2 "id" id
       "timestamp" "2026-01-01T00:00:00Z"
       "type" type "agent_id" "lifecycle-fixture"
       "caused_by" :null "payload" payload))

(let ((checks 0))
  (flet ((check (name value)
           (unless value (error "Lifecycle shadow check failed: ~a" name))
           (incf checks)))
    (uiop:call-with-temporary-file
     (lambda (source-path)
       (uiop:call-with-temporary-file
        (lambda (derived-path)
          (let ((source (make-sqlite-storage source-path))
                (derived (make-sqlite-derived-storage derived-path)))
            (unwind-protect
                 (progn
                   (storage-shadow-lifecycle-prepare derived)
                   (let ((root (storage-append-event
                                source "user-message" (obj "text" "fixture")
                                :agent-id "lifecycle-fixture")))
                     (storage-append-event
                      source "conscious-lifecycle-transition"
                      (sls-transition "open" "request:open" :null)
                      :agent-id "lifecycle-fixture")
                     (let ((result (storage-append-event
                                    source "model-response"
                                    (obj "text" "fixture-result")
                                    :agent-id "lifecycle-fixture")))
                       (storage-append-event
                        source "conscious-lifecycle-transition"
                        (sls-transition "complete" "request:complete"
                                        (gethash "id" result))
                        :agent-id "lifecycle-fixture"))
                     (storage-append-event
                      source "conscious-lifecycle-transition"
                      (sls-transition "open" "request:open" :null)
                      :agent-id "lifecycle-fixture")
                     (storage-append-event
                      source "conscious-lifecycle-transition"
                      (sls-transition "complete" "request:missing" 9999)
                      :agent-id "lifecycle-fixture")
                     (check "root is durable"
                            (= 1 (gethash "id" root))))
                   (sls-apply-all derived source)
                   (check "row projection matches full fold"
                          (equalp
                           (conscious-lifecycle-project
                            (sls-events source) :agent-id "lifecycle-fixture")
                           (storage-shadow-lifecycle-project
                            derived source :agent-id "lifecycle-fixture")))
                   (check "invalid duplicate and fabricated references persist"
                          (equalp #(5 6)
                                  (gethash "invalid_event_ids"
                                           (storage-shadow-lifecycle-project
                                            derived source
                                            :agent-id "lifecycle-fixture"))))
                   (storage-close derived)
                   (storage-close source)
                   (setf source (make-sqlite-storage source-path)
                         derived (make-sqlite-derived-storage derived-path))
                   (check "fresh storage reopen retains row parity"
                          (equalp
                           (conscious-lifecycle-project
                            (sls-events source) :agent-id "lifecycle-fixture")
                           (storage-shadow-lifecycle-project
                            derived source :agent-id "lifecycle-fixture")))
                   (storage-append-event
                    source "other-fixture" (obj "text" "tail")
                    :agent-id "lifecycle-fixture")
                   (sls-apply-all derived source)
                   (check "bounded tail catch-up preserves parity"
                          (equalp
                           (conscious-lifecycle-project
                            (sls-events source) :agent-id "lifecycle-fixture")
                           (storage-shadow-lifecycle-project
                            derived source :agent-id "lifecycle-fixture")))
                   (storage-append-event
                    source "conscious-lifecycle-transition"
                    (sls-transition "resume" "request:bad-tail" :null)
                    :agent-id "lifecycle-fixture")
                   (storage-append-event
                    source "conscious-lifecycle-transition"
                    (sls-transition "resume" "request:second-tail" :null)
                    :agent-id "lifecycle-fixture")
                   (let ((attempts 0))
                     (handler-case
                         (storage-shadow-lifecycle-apply-page
                          derived source
                          (lambda (&rest arguments)
                            (when (= 2 (incf attempts))
                              (error "injected projector failure after write"))
                            (apply #'conscious-lifecycle-shadow-step arguments))
                          :agent-id "lifecycle-fixture" :limit 2)
                       (error () nil))
                     (check "injected failure followed one row mutation"
                            (= 2 attempts)))
                   (check "failed page leaves cursor unchanged"
                          (= 7 (gethash "through_position"
                                        (storage-shadow-lifecycle-watermark
                                         derived source
                                         :agent-id "lifecycle-fixture"))))
                   (sls-apply-all derived source)
                   (check "rollback can be retried without replaying prior rows"
                          (equalp
                           (conscious-lifecycle-project
                            (sls-events source) :agent-id "lifecycle-fixture")
                           (storage-shadow-lifecycle-project
                            derived source :agent-id "lifecycle-fixture")))
                   (%sqlite-exec
                    (%sqlite-derived-handle derived :fixture)
                    "UPDATE pai_lifecycle_v1_requests SET integrity_hash='bad' WHERE agent_id='lifecycle-fixture' AND request_id='request:open'"
                    :fixture)
                   (check "request-row tampering fails closed on read"
                          (handler-case
                              (progn
                                (storage-shadow-lifecycle-project
                                 derived source :agent-id "lifecycle-fixture")
                                nil)
                            (storage-integrity-error () t))))
              (storage-close derived)
              (storage-close source))))
        :want-stream-p nil :type "sqlite"))
     :want-stream-p nil :type "sqlite")
    (uiop:call-with-temporary-file
     (lambda (jsonl-path)
       (with-open-file (stream jsonl-path :direction :output
                                      :if-exists :supersede)
         (dolist (event
                  (list
                   (sls-import-event
                    10 "conscious-lifecycle-transition"
                    (sls-transition "open" "request:future" 11))
                   (sls-import-event 11 "user-message"
                                     (obj "text" "prior source"))
                   (sls-import-event
                    12 "conscious-lifecycle-transition"
                    (sls-transition "open" "request:valid" 11))
                   (sls-import-event 11 "model-response"
                                     (obj "text" "duplicate source ID"))
                   (sls-import-event
                    13 "conscious-lifecycle-transition"
                    (sls-transition "complete" "request:terminal" 11))))
           (let ((json (%storage-json event)))
             (unless (hash-table-p (shasht:read-json json))
               (error "Synthetic import event did not encode as JSON object"))
             (write-line json stream))))
       (uiop:call-with-temporary-file
        (lambda (source-path)
          (uiop:call-with-temporary-file
           (lambda (derived-path)
             (let ((source (make-sqlite-storage source-path))
                   (derived (make-sqlite-derived-storage derived-path)))
               (unwind-protect
                    (progn
                      (check "verified imported duplicate retained"
                             (= 1 (gethash "duplicate_id_count"
                                           (sqlite-import-jsonl
                                            source jsonl-path))))
                      (storage-shadow-lifecycle-prepare derived)
                      (sls-apply-all derived source)
                      (check "imported future reference and duplicate-ID parity"
                             (equalp
                              (conscious-lifecycle-project
                               (sls-events source)
                               :agent-id "lifecycle-fixture")
                              (storage-shadow-lifecycle-project
                               derived source
                               :agent-id "lifecycle-fixture")))
                      (check "future reference rejected at physical frontier"
                             (equalp
                              #(10)
                              (gethash "invalid_event_ids"
                                       (storage-shadow-lifecycle-project
                                        derived source
                                        :agent-id "lifecycle-fixture")))))
                 (storage-close derived)
                 (storage-close source))))
           :want-stream-p nil :type "sqlite"))
        :want-stream-p nil :type "sqlite"))
     :want-stream-p nil :type "jsonl")
    (format t "Lifecycle shadow: ~d passed, 0 failed~%" checks)))
