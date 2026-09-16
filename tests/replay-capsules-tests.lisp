(in-package :agent)

(defvar *replay-test-pass* 0)
(defvar *replay-test-fail* 0)

(defun replay-test-check (name condition)
  (if condition
      (progn (incf *replay-test-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *replay-test-fail*) (format t "  FAIL ~a~%" name))))

(load (test-source "runtime-observer-registry.lisp"))

;; The replay source reads these effective modes but does not mutate them.
(defparameter *autonomous-write-mode* :paused)
(defparameter *conversation-context-budget-mode* :enforced)
(defparameter *initiative-policy-mode* :shadow)
(defparameter *initiative-delivery-mode* :shadow)
(defparameter *reciprocity-canary-mode* :shadow)
(defparameter *public-outbound-gateway-mode* :observe)
(defparameter *public-outbound-records* nil)
(defparameter *last-self-mod-history* nil)

(load (test-source "replay-capsules.lisp"))

(setf *replay-capsule-file* #P"/tmp/replay-capsules-a3-test.json"
      *replay-capsule-fixture-root* #P"/tmp/replay-capsule-fixtures/")

(defun replay-test-reset ()
  (ignore-errors (delete-file *replay-capsule-file*))
  (setf *replay-capsules* nil
        *replay-capsule-event-queue* nil
        *public-outbound-records* nil
        *replay-capsule-pruning*
        (obj "retention" 0 "record_cap" 0 "byte_ceiling" 0
             "oversize_rejected" 0 "duplicate_suppressed" 0
             "budget_suppressed" 0 "queue_dropped" 0 "last_prune_at" :null
             "last_prune_reason" :null "last_prune_status" "not-run"))
  (runtime-observer-reset)
  (replay-capsule-register-all)
  t)

(defun replay-test-emit (event payload)
  (prog1 (runtime-observer-emit event payload)
    (replay-capsule-worker-step :scheduled nil)))

(defun replay-test-event (id type payload &optional caused-by)
  (obj "id" id "type" type "payload" payload
       "caused_by" (or caused-by :null)))

(defun replay-test-serialized ()
  (shasht:write-json (coerce *replay-capsules* 'vector) nil))

(format t "~&== A3 bounded replay capsules ==~%")

(replay-test-reset)
(replay-test-check "all required A3 observers are uniquely wired"
                   (replay-capsule-assert))
(let* ((report (runtime-observer-report))
       (events (gethash "events" report)))
  (replay-test-check "nine lifecycle classes have replay consumers"
                     (= 9 (loop for rows being the hash-values of events
                                sum (count-if
                                     (lambda (row)
                                       (search "replay-capture-"
                                               (gethash "name" row)))
                                     (coerce rows 'list))))))

(let ((event (replay-test-event 101 "user-message"
                                (obj "text" "PRIVATE-USER-TEXT"))))
  (replay-test-emit "user-message" event)
  (replay-test-check "user reply produces one event capsule"
                     (= 1 (length *replay-capsules*)))
  (replay-test-check "capsule is schema v2"
                     (= 2 (gethash "schema_version" (first *replay-capsules*))))
  (replay-test-check "private user text is not duplicated"
                     (not (search "PRIVATE-USER-TEXT" (replay-test-serialized))))
  (replay-test-emit "user-message" event)
  (replay-test-check "same durable event is deduplicated"
                     (= 1 (length *replay-capsules*)))
  (replay-test-check "duplicate suppression is visible"
                     (= 1 (gethash "duplicate_suppressed"
                                   *replay-capsule-pruning*))))

(replay-test-reset)
(let ((*replay-capsule-event-queue-cap* 1))
  (runtime-observer-emit
   "user-message" (replay-test-event 111 "user-message"
                                     (obj "text" "QUEUE-PRIVATE-ONE")))
  (runtime-observer-emit
   "user-message" (replay-test-event 112 "user-message"
                                     (obj "text" "QUEUE-PRIVATE-TWO")))
  (replay-test-check "observer path enqueues without synchronous persistence"
                     (and (= 1 (length *replay-capsule-event-queue*))
                          (null *replay-capsules*)))
  (replay-test-check "bounded queue drop is visible"
                     (= 1 (gethash "queue_dropped"
                                   *replay-capsule-pruning*)))
  (replay-test-check "queued capsule contains no private event text"
                     (let ((encoded (shasht:write-json
                                     (first *replay-capsule-event-queue*) nil)))
                       (and (not (search "QUEUE-PRIVATE-ONE" encoded))
                            (not (search "QUEUE-PRIVATE-TWO" encoded)))))
  (replay-capsule-worker-step :scheduled nil))

(replay-test-emit
 "turn-capture-complete"
 (replay-test-event 102 "turn-capture-complete"
                    (obj "turn_id" "short" "entry_count" 1)))
(replay-test-check "non-substantive completed turn is skipped"
                   (= 1 (length *replay-capsules*)))
(replay-test-emit
 "turn-capture-complete"
 (replay-test-event 103 "turn-capture-complete"
                    (obj "turn_id" "turn-103" "entry_count" 2)))
(replay-test-check "substantive completed turn is captured"
                   (= 2 (length *replay-capsules*)))

(replay-test-emit
 "drive-near-threshold"
 (replay-test-event 104 "drive-near-threshold"
                    (obj "drive_id" "curiosity" "current" 0.61
                         "threshold" 0.6)))
(let* ((capsule (first *replay-capsules*))
       (refs (gethash "value" (gethash "references" capsule)))
       (ids (gethash "ids" refs)))
  (replay-test-check "drive threshold is captured before any decision"
                     (string= "drive-near-threshold"
                              (gethash "trigger_type" capsule)))
  (replay-test-check "drive capsule retains only its durable drive ID"
                     (string= "curiosity" (gethash "drive_id" ids))))

(replay-test-emit
 "public-outbound-completed"
 (obj "recorded_at" 200 "transport_status" "error"
      "envelope" (obj "id" "env-failed" "kind" "reply"
                      "content_sha256"
                      "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                      "redacted_content" "PRIVATE-OUTBOUND-TEXT")
      "canonical_public_act_id" "act-failed"))
(let ((capsule (first *replay-capsules*)))
  (replay-test-check "failed delivery is captured as negative outcome"
                     (string= "negative" (gethash "outcome" capsule)))
  (replay-test-check "outbound content is not duplicated"
                     (not (search "PRIVATE-OUTBOUND-TEXT"
                                  (replay-test-serialized)))))

(replay-test-reset)
(multiple-value-bind (capsule status) (replay-capsule-scheduled-step 1000)
  (replay-test-check "scheduled step captures when initially due"
                     (and capsule (string= status "captured")))
  (replay-test-check "empty interval is exact negative space"
                     (and (string= "negative" (gethash "outcome" capsule))
                          (gethash "no_public_outbound"
                                   (gethash "value"
                                            (gethash "negative_space" capsule))))))
(multiple-value-bind (capsule status) (replay-capsule-scheduled-step 1100)
  (declare (ignore capsule))
  (replay-test-check "scheduled interval prevents early duplicate capture"
                     (string= status "not-due")))

(push (obj "recorded_at" 1200 "transport_status" "returned"
           "envelope" (obj "id" "env-natural" "kind" "reply"))
      *public-outbound-records*)
(multiple-value-bind (capsule status) (replay-capsule-scheduled-step 2800)
  (declare (ignore status))
  (let ((negative (gethash "value" (gethash "negative_space" capsule))))
    (replay-test-check "scheduled interval sees positive outbound activity"
                       (and (string= "positive" (gethash "outcome" capsule))
                            (= 1 (gethash "outbound_record_count" negative))))
    (replay-test-check "scheduled activity stores bounded outbound IDs"
                       (string= "env-natural"
                                (aref (gethash "outbound_record_ids" negative) 0)))))

(replay-test-reset)
(let ((*replay-capsule-scheduled-max-per-day* 1))
  (replay-capsule-scheduled-step 1000)
  (multiple-value-bind (capsule status) (replay-capsule-scheduled-step 2800)
    (declare (ignore capsule))
    (replay-test-check "scheduled daily budget is independently enforced"
                       (string= status "daily-budget"))))

(replay-test-reset)
(let ((*replay-capsule-event-max-per-day* 2))
  (dolist (id '(201 202 203))
    (replay-test-emit "user-message"
                           (replay-test-event id "user-message" (obj))))
  (replay-test-check "event daily budget is independent and enforced"
                     (= 2 (length *replay-capsules*)))
  (replay-test-check "event budget suppression is visible"
                     (= 1 (gethash "budget_suppressed"
                                   *replay-capsule-pruning*))))

(replay-test-reset)
(let ((*replay-capsule-manual-max-per-day* 1))
  (replay-capsule-manual-capture "operator-check" :now 3000)
  (multiple-value-bind (capsule status)
      (replay-capsule-manual-capture "second-check" :now 3001)
    (declare (ignore capsule))
    (replay-test-check "manual budget is separate and enforced"
                       (string= status "daily-budget"))))
(replay-test-check "manual capture rejects prose"
                   (handler-case
                       (progn (replay-capsule-manual-capture "private prose here") nil)
                     (error () t)))

(replay-test-reset)
(let ((*replay-capsule-record-cap* 2))
  (dolist (id '(301 302 303))
    (replay-test-emit "user-message"
                           (replay-test-event id "user-message" (obj))))
  (replay-test-check "record cap prunes oldest capsules"
                     (= 2 (length *replay-capsules*)))
  (replay-test-check "record-cap pruning count is visible"
                     (= 1 (gethash "record_cap" *replay-capsule-pruning*))))

(replay-test-reset)
(let* ((now 5000000)
       (old (%replay-build-capsule
             "event" "user-reply-observed"
             (obj "ids" (obj "source_event_id" "old")) "observed"
             (- now *replay-capsule-retention-seconds* 1))))
  (setf *replay-capsules* (list old))
  (%replay-prune now)
  (replay-test-check "retention pruning removes expired capsules"
                     (null *replay-capsules*))
(replay-test-check "retention pruning count is visible"
                     (= 1 (gethash "retention" *replay-capsule-pruning*))))

(replay-test-reset)
(let ((*replay-capsule-disk-max-bytes* 2200)
      (*replay-capsule-shard-max-bytes* 2200))
  (dolist (id '(401 402 403 404))
    (replay-test-emit "user-message"
                           (replay-test-event id "user-message" (obj))))
  (replay-test-check "disk ceiling prunes oldest encoded records"
                     (and (<= (1+ (%replay-encoded-bytes
                                   (coerce *replay-capsules* 'vector)))
                              *replay-capsule-disk-max-bytes*)
                          (<= (%replay-file-bytes)
                              *replay-capsule-disk-max-bytes*)))
  (replay-test-check "disk-ceiling pruning count is visible"
                     (plusp (gethash "byte_ceiling"
                                     *replay-capsule-pruning*))))

(replay-test-reset)
(let ((*replay-capsule-record-max-bytes* 128))
  (multiple-value-bind (capsule status)
      (replay-capsule-manual-capture "oversize-check" :now 6000)
    (declare (ignore capsule))
    (replay-test-check "oversize record fails closed"
                       (string= status "record-too-large")))
  (replay-test-check "oversize rejection is visible"
                     (= 1 (gethash "oversize_rejected"
                                   *replay-capsule-pruning*))))

(replay-test-reset)
(replay-capsule-manual-capture "fixture-source" :trigger-id "case-1" :now 7000)
(let* ((id (gethash "id" (first *replay-capsules*)))
       (path (replay-capsule-extract-fixture id "captured-case"))
       (text (uiop:read-file-string path)))
  (replay-test-check "fixture extraction stays in fixed root"
                     (search "/tmp/replay-capsule-fixtures/" (namestring path)))
  (replay-test-check "fixture declares captured provenance"
                     (search "captured-production-event" text)))

(let ((report (replay-capsule-report)))
  (replay-test-check "dashboard report exposes byte ceilings"
                     (and (= 32768 (gethash "record_max_bytes" report))
                          (= 8388608 (gethash "disk_max_bytes" report))))
  (replay-test-check "dashboard report exposes coverage dimensions"
                     (hash-table-p (gethash "coverage" report)))
  (replay-test-check "report declares no content retention"
                     (not (gethash "retains_conversation_content" report)))
  (replay-test-check "report declares zero provider calls and no authority"
                     (and (zerop (gethash "provider_calls" report))
                          (not (gethash "delivery_authority" report)))))

(let ((*replay-capsule-scheduled-interval-seconds* 1)
      (*replay-capsule-sleep-fn*
        (lambda (seconds) (declare (ignore seconds)) (sleep 0.05))))
  (replay-capsule-start)
  (sleep 0.01)
  (replay-test-check "independent scheduled worker starts alive"
                     (and *replay-capsule-thread*
                          (bt:thread-alive-p *replay-capsule-thread*)))
  (replay-capsule-stop 1)
  (replay-test-check "independent scheduled worker stops cleanly"
                     (null *replay-capsule-thread*)))

(let ((source (string-downcase
               (uiop:read-file-string (namestring (test-source "replay-capsules.lisp"))))))
  (replay-test-check "A3 source has no model call"
                     (not (search "call-model" source)))
  (replay-test-check "A3 source has no Telegram send"
                     (not (search "telegram-send" source)))
  (replay-test-check "A3 source has no tool dispatch"
                     (not (search "dispatch-tool" source)))
  (replay-test-check "A3 source has no outbound transport base call"
                     (not (search "public-outbound-base" source))))

(let* ((source (string-downcase
                (uiop:read-file-string (namestring (test-source "drives.lisp")))))
       (fact (search "drive-near-threshold" source))
       (decision (and fact (search "initiative-v2-observe-trigger" source
                                   :start2 fact))))
  (replay-test-check "drive source emits the threshold fact"
                     (not (null fact)))
  (replay-test-check "drive threshold fact precedes initiative evaluation"
                     (and fact decision (< fact decision))))

(format t "~%A3 REPLAY CAPSULE TESTS: ~d passed, ~d failed.~%"
        *replay-test-pass* *replay-test-fail*)
(when (plusp *replay-test-fail*) (sb-ext:exit :code 1))
