(in-package :agent)

(ql:quickload '(:cffi :shasht :ironclad :bordeaux-threads) :silent t)

(defvar *storage-test-pass* 0)
(defvar *storage-test-fail* 0)

(defun storage-check (name condition)
  (if condition
      (progn (incf *storage-test-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *storage-test-fail*) (format t "  FAIL ~a~%" name))))

(defun storage-signals-p (condition-type thunk)
  (handler-case (progn (funcall thunk) nil)
    (condition (actual) (typep actual condition-type))))

(defun storage-obj (&rest fields)
  (loop with object = (make-hash-table :test #'equal)
        for (key value) on fields by #'cddr
        do (setf (gethash key object) value)
        finally (return object)))

(defun storage-delete-db-files (path)
  (dolist (candidate (list path
                           (pathname (concatenate 'string (namestring path) "-wal"))
                           (pathname (concatenate 'string (namestring path) "-shm"))))
    (when (probe-file candidate) (delete-file candidate))))

;; Load only the new seam and adapter. The test must not initialize the legacy
;; event log or the existing Postgres memory store.
(load (test-source "storage-substrate.lisp"))
(load (test-source "sqlite-storage.lisp"))

(format t "~%== storage substrate contract ==~%")
(let* ((state-json (concatenate 'string
                                "{\"unicode\":\"café ☕\",\"padding\":\""
                                (make-string 200000 :initial-element #\x)
                                "\"}"))
       (legacy (%storage-sha256
                (%storage-checkpoint-integrity-input
                 "projection" "agent" 42 43 "projector" "policy" state-json)))
       (streamed (%storage-checkpoint-integrity-sha256
                  "projection" "agent" 42 43 "projector" "policy" state-json)))
  (storage-check "streamed checkpoint hashing preserves the legacy digest"
                 (string= legacy streamed))
  (storage-check "streamed UTF-8 chunks preserve the legacy digest at boundaries"
                 (string= (%storage-sha256 (concatenate 'string "" state-json))
                          (%storage-sha256-string-parts
                           (list "" state-json) :chunk-size 7))))
(storage-check "invalid integrity chunk size fails closed"
               (storage-signals-p
                'storage-error
                (lambda () (%storage-sha256-string-parts (list "x")
                                                          :chunk-size 0))))
(storage-check "SQLite constructor is present"
               (fboundp 'make-sqlite-storage))

(let* ((path (merge-pathnames "storage-substrate.sqlite3" (test-state-dir)))
       (backend nil))
  (storage-delete-db-files path)
  (unwind-protect
      (progn
        (setf backend (make-sqlite-storage path))
        (%with-sqlite-statement
            (statement (%sqlite-storage-handle backend)
                       "PRAGMA busy_timeout" :test-busy-timeout)
          (%sqlite-step (%sqlite-storage-handle backend) statement
                        :test-busy-timeout +sqlite-row+)
          (storage-check "SQLite waits briefly for cross-process contention"
                         (= 5000 (%sqlite-column-int64 statement 0))))
        (let ((capabilities (storage-capabilities backend)))
          (storage-check "capabilities identify SQLite"
                         (string= "sqlite" (gethash "backend" capabilities)))
          (storage-check "event and checkpoint capabilities are honest"
                         (and (eq t (gethash "event_log" capabilities))
                              (eq t (gethash "projection_checkpoints" capabilities))
                              (null (gethash "vector_index" capabilities)))))
        (let* ((first (storage-append-event
                       backend "user-message" (storage-obj "content" "hello")
                       :agent-id "alpha" :occurred-at "2026-08-19T12:00:00Z"))
               (second (storage-append-event
                        backend "agent-message" (storage-obj "content" "hi")
                        :agent-id "alpha" :occurred-at "2026-08-19T12:00:01Z"
                        :caused-by (gethash "id" first)))
               (other (storage-append-event
                       backend "user-message" (storage-obj "content" "other")
                       :agent-id "beta" :occurred-at "2026-08-19T12:00:02Z")))
          (storage-check "backend atomically assigns monotonic global IDs"
                         (equal '(1 2 3)
                                (mapcar (lambda (event) (gethash "id" event))
                                        (list first second other))))
          (storage-check "exact stamped event reads back"
                         (string= "hello"
                                  (gethash "content"
                                           (gethash "payload"
                                                    (storage-read-event backend 1)))))
          (storage-check "agent mismatch cannot read an event"
                         (null (storage-read-event backend 1 :agent-id "beta")))
          (let ((rows (storage-scan-events backend :agent-id "alpha")))
            (storage-check "scan is ordered and agent-isolated"
                           (and (= 2 (length rows))
                                (= 1 (gethash "id" (aref rows 0)))
                                (= 2 (gethash "id" (aref rows 1))))))
          (let ((rows (storage-scan-events backend :agent-id "alpha"
                                                   :after-id 1
                                                   :event-type "agent-message"
                                                   :limit 1)))
            (storage-check "scan applies watermark, type, and limit"
                           (and (= 1 (length rows))
                                (= 2 (gethash "id" (aref rows 0))))))
          (let ((rows (storage-recent-events
                       backend '("user-message" "agent-message") 2
                       :agent-id "alpha")))
            (storage-check "recent event lookup is bounded and chronological"
                           (and (= 2 (length rows))
                                (= 1 (gethash "id" (first rows)))
                                (= 2 (gethash "id" (second rows))))))
          (let ((rows (storage-recent-events
                       backend '("user-message" "agent-message") 2
                       :agent-id "alpha" :before-event-id 2)))
            (storage-check "recent event lookup applies exclusive upper bound"
                           (and (= 1 (length rows))
                                (= 1 (gethash "id" (first rows))))))
          (multiple-value-bind (rows boundary)
              (storage-query-events
               backend :agent-id "alpha"
               :event-types '("user-message" "agent-message")
               :from (encode-universal-time 0 0 12 19 8 2026 0)
               :limit 1)
            (storage-check "indexed query retains newest match chronologically"
                           (and (= 1 (length rows))
                                (= 2 (gethash "id" (first rows)))
                                (= 2 boundary))))
          (let ((rows (storage-query-events
                       backend :agent-id "alpha"
                       :exclude-event-types '("user-message"))))
            (storage-check "indexed query applies exclusion filters"
                           (and (= 1 (length rows))
                                (= 2 (gethash "id" (first rows))))))
          (storage-check "range watermark is independent of content filters"
                         (= 2 (storage-range-max-event-id
                               backend :agent-id "alpha"
                               :after-id 1 :through-id 2)))
          (storage-check "agent maximum is distinct from global maximum"
                         (and (= 2 (storage-max-event-id backend :agent-id "alpha"))
                              (= 3 (storage-max-event-id backend))))
          (let* ((boundary (storage-authority-boundary
                            backend :agent-id "alpha"))
                 (binding
                   (storage-checkpoint-source-binding
                    backend :agent-id "alpha"
                    :through-event-id (gethash "through_event_id" boundary)
                    :through-position
                    (gethash "through_storage_position" boundary))))
            (storage-check "authority boundary atomically binds logical and physical head"
                           (and (= 2 (gethash "through_event_id" boundary))
                                (= 2 (gethash "through_storage_position" boundary))
                                (string= binding
                                         (gethash "source_binding" boundary)))))
          (let* ((state (storage-obj "active" (vector "i1")))
                 (published
                   (storage-publish-checkpoint
                    backend "conscious-state" state :agent-id "alpha"
                    :through-event-id 2 :projector-revision "q5"
                    :policy-revision "p1"))
                 (loaded (storage-load-checkpoint
                          backend "conscious-state" :agent-id "alpha")))
            (storage-check "checkpoint publishes and verifies exact metadata"
                           (and (= 2 (gethash "through_event_id" published))
                                (= 2 (gethash "through_event_id" loaded))
                                (string= "q5" (gethash "projector_revision" loaded))
                                (string= "i1" (aref (gethash "active"
                                                              (gethash "state" loaded))
                                                     0))))
            (storage-check "published checkpoint keeps the legacy on-disk hash"
                           (string=
                            (gethash "integrity_hash" published)
                            (%storage-sha256
                             (%storage-checkpoint-integrity-input
                              "conscious-state" "alpha" 2 2 "q5" "p1"
                              (%storage-json state)))))
            (storage-check "checkpoint cannot move backwards"
                           (storage-signals-p
                            'storage-conflict-error
                            (lambda ()
                              (storage-publish-checkpoint
                               backend "conscious-state" state :agent-id "alpha"
                               :through-event-id 1))))
            (storage-check "checkpoint physical position cannot move backwards"
                           (storage-signals-p
                            'storage-conflict-error
                            (lambda ()
                              (storage-publish-checkpoint
                               backend "conscious-state" state :agent-id "alpha"
                               :through-event-id 2 :through-position 1))))
            (storage-check "checkpoint cannot claim a future event"
                           (storage-signals-p
                            'storage-conflict-error
                            (lambda ()
                              (storage-publish-checkpoint
                               backend "other" state :agent-id "alpha"
                               :through-event-id 99))))))
        (storage-close backend)
        (setf backend (make-sqlite-storage path))
        (storage-check "close and reopen preserves durable history"
                       (and (= 3 (storage-max-event-id backend))
                            (= 2 (gethash "through_event_id"
                                         (storage-load-checkpoint
                                          backend "conscious-state"
                                          :agent-id "alpha")))))
        ;; Corruption probes operate below the public seam on disposable data.
        (%sqlite-exec (%sqlite-storage-handle backend)
                      "UPDATE pai_events SET event_json='{}' WHERE event_id=1"
                      :test-corruption)
        (storage-check "event corruption fails closed"
                       (storage-signals-p
                        'storage-integrity-error
                        (lambda () (storage-read-event backend 1))))
        (%sqlite-exec (%sqlite-storage-handle backend)
                      "UPDATE pai_events SET agent_id='damaged' WHERE event_id=2"
                      :test-corruption)
        (storage-check "indexed metadata corruption fails closed"
                       (storage-signals-p
                        'storage-integrity-error
                        (lambda () (storage-read-event backend 2))))
        (%sqlite-exec
         (%sqlite-storage-handle backend)
         "UPDATE pai_projection_checkpoints SET state_json='{}' WHERE projection_name='conscious-state' AND agent_id='alpha'"
         :test-corruption)
        (storage-check "checkpoint corruption fails closed"
                       (storage-signals-p
                        'storage-integrity-error
                        (lambda ()
                          (storage-load-checkpoint
                           backend "conscious-state" :agent-id "alpha"))))
        (multiple-value-bind (event receipt)
            (storage-append-event
             backend "receipt-probe" (storage-obj "status" "content-free")
             :agent-id "alpha" :occurred-at "2026-08-19T12:00:03Z")
          (storage-check "append returns an exact hash-bound durable receipt"
                         (and (= (gethash "id" event)
                                 (gethash "event_id" receipt))
                              (string= (gethash "integrity_hash" receipt)
                                       (%storage-sha256
                                        (gethash "event_json" receipt)))
                              (plusp (gethash "storage_position" receipt)))))
        (let ((receipts nil))
          (multiple-value-bind (complete-p head count)
              (storage-map-event-receipts
               backend (lambda (receipt) (push receipt receipts))
               :agent-id "alpha" :after-position 0
               :event-types '("receipt-probe"))
            (storage-check "receipt tail closes at its exact matching head"
                           (and complete-p (= 1 count) (= 1 (length receipts))
                                (= head (gethash "storage_position"
                                                 (first receipts)))))
            (let ((first-position head) (bounded nil))
              (storage-append-event
               backend "receipt-probe" (storage-obj "status" "newer")
               :agent-id "alpha" :occurred-at "2026-08-19T12:00:04Z")
              (multiple-value-bind (bounded-complete ignored bounded-count)
                  (storage-map-event-receipts
                   backend (lambda (receipt) (push receipt bounded))
                   :agent-id "alpha" :after-position 0
                   :through-position first-position
                   :event-types '("receipt-probe"))
                (declare (ignore ignored))
                (storage-check
                 "receipt tail excludes rows beyond its physical upper bound"
                 (and bounded-complete (= 1 bounded-count)
                      (= 1 (length bounded))
                      (= first-position
                         (gethash "storage_position" (first bounded))))))))))
    (when (and backend (not (%sqlite-storage-closed-p backend)))
      (ignore-errors (storage-close backend)))
    (storage-delete-db-files path)))

(let* ((path (merge-pathnames "storage-read-only.sqlite3" (test-state-dir)))
       (writer nil)
       (reader nil))
  (storage-delete-db-files path)
  (unwind-protect
      (progn
        (setf writer (make-sqlite-storage path))
        (storage-append-event
         writer "fixture-event" (storage-obj "value" "preserved")
         :agent-id "fixture" :occurred-at "2026-08-19T12:00:00Z")
        (storage-close writer)
        (setf writer nil
              reader (make-sqlite-storage-read-only path))
        (storage-check
         "read-only SQLite replay reads existing authority"
         (string=
          "preserved"
          (gethash "value"
                   (gethash "payload"
                            (storage-read-event reader 1
                                                :agent-id "fixture")))))
        (storage-check
         "read-only SQLite replay refuses event mutation"
         (storage-signals-p
          'storage-error
          (lambda ()
            (storage-append-event
             reader "fixture-event" (storage-obj "value" "forbidden")
             :agent-id "fixture"
             :occurred-at "2026-08-19T12:00:01Z")))))
    (when writer (ignore-errors (storage-close writer)))
    (when reader (ignore-errors (storage-close reader)))
    (storage-delete-db-files path)))

(format t "~%~d passed, ~d failed~%" *storage-test-pass* *storage-test-fail*)
(when (plusp *storage-test-fail*)
  (error "storage substrate tests failed"))
