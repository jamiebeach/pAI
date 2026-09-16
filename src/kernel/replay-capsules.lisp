;;;; replay-capsules.lisp -- bounded reconstruction evidence, not invented history.

(in-package :agent)

(export '(replay-capsule-capture replay-capsule-manual-capture
          replay-capsule-extract-fixture replay-capsule-report
          replay-capsule-records replay-capsule-due-p
          replay-capsule-scheduled-step replay-capsule-worker-step
          replay-capsule-register-all
          replay-capsule-assert replay-capsule-start replay-capsule-stop))

(defparameter *replay-capsule-file* #P"/agent/state/replay-capsules.json")
(defparameter *replay-capsule-fixture-root*
  #P"/agent/state/evals/fixtures/captured/")
(defparameter *replay-capsule-record-cap* 500)
(defparameter *replay-capsule-retention-seconds* (* 14 24 60 60))
(defparameter *replay-capsule-scheduled-interval-seconds* (* 30 60))
(defparameter *replay-capsule-scheduled-max-per-day* 48)
(defparameter *replay-capsule-event-max-per-day* 96)
(defparameter *replay-capsule-manual-max-per-day* 12)
(defparameter *replay-capsule-record-max-bytes* (* 32 1024))
(defparameter *replay-capsule-shard-max-bytes* (* 8 1024 1024))
(defparameter *replay-capsule-disk-max-bytes* (* 8 1024 1024))
(defparameter *replay-capsule-reference-id-cap* 20)
(defparameter *replay-capsule-event-queue-cap* 256)
(defparameter *replay-capsule-worker-poll-seconds* 1)
(defparameter *replay-capsule-autostart-p* t)

(defparameter *replay-capsule-event-specs*
  '(("turn-capture-complete" "public-turn-completed")
    ("user-message" "user-reply-observed")
    ("drive-near-threshold" "drive-near-threshold")
    ("initiative-decision" "initiative-decision-made")
    ("public-outbound-completed" "outbound-completed")
    ("artifact-validated" "project-artifact-validated")
    ("artifact-completed" "project-artifact-completed")
    ("agent-process-transition" "project-transition")
    ("near-term-item-transition" "deferred-intention-transition")))

(defvar *replay-capsules* nil)
(defvar *replay-capsule-lock* (bt:make-lock "replay-capsules"))
(defvar *replay-capsule-thread* nil)
(defvar *replay-capsule-stop-requested* nil)
(defvar *replay-capsule-event-queue* nil)
(defvar *replay-capsule-event-queue-lock*
  (bt:make-lock "replay-capsule-event-queue"))
(defvar *replay-capsule-next-scheduled-at* nil)
(defvar *replay-capsule-now-fn* #'get-universal-time)
(defvar *replay-capsule-sleep-fn* #'sleep)
(defvar *last-self-mod-history* nil)
(defvar *replay-capsule-pruning*
  (obj "retention" 0 "record_cap" 0 "byte_ceiling" 0
       "oversize_rejected" 0 "duplicate_suppressed" 0
       "budget_suppressed" 0 "queue_dropped" 0 "last_prune_at" :null
       "last_prune_reason" :null "last_prune_status" "not-run"))

(declaim (ftype function replay-capsule-scheduled-step))

(defun %replay-now () (funcall *replay-capsule-now-fn*))

(defun %replay-json-string (value)
  (let ((*print-pretty* nil)) (shasht:write-json value nil)))

(defun %replay-utf8-bytes (string)
  (length (sb-ext:string-to-octets string :external-format :utf-8)))

(defun %replay-encoded-bytes (value)
  (%replay-utf8-bytes (%replay-json-string value)))

(defun %replay-sha256 (value)
  (unless (find-package :ironclad)
    (ql:quickload :ironclad :silent t))
  (let* ((octets (sb-ext:string-to-octets (%replay-json-string value)
                                           :external-format :utf-8))
         (digest (funcall (intern "DIGEST-SEQUENCE" :ironclad)
                          :sha256 octets)))
    (string-downcase
     (funcall (intern "BYTE-ARRAY-TO-HEX-STRING" :ironclad) digest))))

(defun %replay-day (time)
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time time 0)
    (declare (ignore sec min hour))
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month day)))

(defun %replay-fidelity (value fidelity &optional note)
  (obj "value" value "fidelity" fidelity "note" (or note :null)))

(defun %replay-safe-id (value)
  (let ((rendered
          (cond ((stringp value) value)
                ((integerp value) (format nil "~d" value))
                (t nil))))
    (and rendered (plusp (length rendered)) (<= (length rendered) 160)
         rendered)))

(defun %replay-safe-id-vector (value)
  (let ((items (cond ((vectorp value) (coerce value 'list))
                     ((listp value) value)
                     ((null value) nil)
                     (t (list value))))
        (result nil))
    (dolist (item items (coerce (nreverse result) 'vector))
      (let ((safe (%replay-safe-id item)))
        (when safe
          (push safe result)
          (when (>= (length result) *replay-capsule-reference-id-cap*)
            (return (coerce (nreverse result) 'vector))))))))

(defun %replay-conversation-reference ()
  (if (boundp '*last-self-mod-history*)
      (%replay-fidelity
       (obj "message_count" (length *last-self-mod-history*)
            "storage" "conversation-persistence"
            "content_in_capsule" nil)
       "derived" "content remains in its authoritative persistence store")
      (%replay-fidelity :null "unavailable" "conversation state not bound")))

(defun %replay-mode-summary ()
  (let ((rows (obj)))
    (dolist (spec '(("autonomous_write" *autonomous-write-mode*)
                    ("conversation_context_budget" *conversation-context-budget-mode*)
                    ("initiative_policy" *initiative-policy-mode*)
                    ("initiative_delivery" *initiative-delivery-mode*)
                    ("reciprocity_canary" *reciprocity-canary-mode*)
                    ("public_outbound_gateway" *public-outbound-gateway-mode*)))
      (destructuring-bind (name symbol) spec
        (setf (gethash name rows)
              (if (boundp symbol)
                  (string-downcase (string (symbol-value symbol)))
                  :null))))
    (%replay-fidelity rows "exact")))

(defun %replay-runtime-reference ()
  (let ((identity (obj "container_id"
                       (or (uiop:getenv "HOSTNAME") :null)
                       "runtime_truth_schema"
                       (if (fboundp 'runtime-truth-manifest) 2 :null))))
    (%replay-fidelity identity
                      (if (uiop:getenv "HOSTNAME") "exact" "derived")
                      "full runtime manifest remains authoritative")))

(defun %replay-event-payload (event)
  (and (hash-table-p event) (gethash "payload" event)))

(defun %replay-add-reference-id (target key value)
  (let ((safe (%replay-safe-id value)))
    (when safe (setf (gethash key target) safe))))

(defun %replay-event-references (event trigger-type)
  (let* ((payload (%replay-event-payload event))
         (envelope (and (hash-table-p event) (gethash "envelope" event)))
         (nested-envelope (and (hash-table-p payload) (gethash "envelope" payload)))
         (ids (obj)))
    (%replay-add-reference-id ids "source_event_id"
                              (and (hash-table-p event) (gethash "id" event)))
    (%replay-add-reference-id ids "causal_event_id"
                              (and (hash-table-p event) (gethash "caused_by" event)))
    (dolist (key '("turn_id" "decision_id" "candidate_id" "generation_id"
                   "drive_id"
                   "operation_id" "process_id" "artifact_id" "item_id"
                   "schedule_id" "canonical_public_act_id"))
      (%replay-add-reference-id
       ids key
       (or (and (hash-table-p payload) (gethash key payload))
           (and (hash-table-p event) (gethash key event)))))
    (let ((candidate-ids (and (hash-table-p payload)
                              (gethash "candidate_ids" payload))))
      (when candidate-ids
        (setf (gethash "candidate_ids" ids)
              (%replay-safe-id-vector candidate-ids))))
    (let ((actual-envelope (or nested-envelope envelope)))
      (when (hash-table-p actual-envelope)
        (%replay-add-reference-id ids "envelope_id" (gethash "id" actual-envelope))
        (let ((kind (gethash "kind" actual-envelope)))
          (when (and (stringp kind) (<= (length kind) 32))
            (setf (gethash "envelope_kind" ids) kind)))
        (let ((sha (gethash "content_sha256" actual-envelope)))
          (when (and (stringp sha) (= (length sha) 64))
            (setf (gethash "content_sha256" ids) sha)))))
    (obj "trigger_type" trigger-type
         "authoritative_store" "events-and-runtime-ledgers"
         "ids" ids)))

(defun %replay-event-outcome (event trigger-type)
  (let ((payload (%replay-event-payload event)))
    (cond
      ((string= trigger-type "outbound-completed")
       (let ((status (and (hash-table-p event) (gethash "transport_status" event))))
         (if (member status '("returned" "success" "sent") :test #'string=)
             "positive" "negative")))
      ((string= trigger-type "initiative-decision-made")
       (let ((result (and (hash-table-p payload)
                          (or (gethash "result" payload)
                              (gethash "decision" payload)))))
         (if (member result '("execute-now" "permit" "would-permit")
                     :test #'string=)
             "positive" "negative")))
      (t "observed"))))

(defun %replay-last-state-sha ()
  (and *replay-capsules*
       (gethash "state_sha256" (first *replay-capsules*))))

(defun %replay-build-capsule (capture-class trigger-type references outcome now
                              &key negative-space)
  (let* ((reference-sha (%replay-sha256 references))
         (state (obj "modes" (%replay-mode-summary)
                     "runtime" (%replay-runtime-reference)))
         (state-sha (%replay-sha256 state)))
    (obj "schema_version" 2
         "id" (format nil "capsule-~d-~8,'0x" now (random #x100000000))
         "captured_at" now
         "captured_day" (%replay-day now)
         "capture_class" capture-class
         "trigger_type" trigger-type
         "outcome" outcome
         "references" (%replay-fidelity references "exact")
         "reference_sha256" reference-sha
         "conversation" (%replay-conversation-reference)
         "state" (%replay-fidelity state "derived"
                                   "bounded summary; authoritative state remains external")
         "state_sha256" state-sha
         "previous_state_sha256" (or (%replay-last-state-sha) :null)
         "negative_space" (or negative-space
                              (%replay-fidelity :null "unavailable"
                                                "not a scheduled interval")))))

(defun %replay-stat-inc (key &optional (amount 1))
  (incf (gethash key *replay-capsule-pruning* 0) amount))

(defun %replay-note-prune (now reason changed)
  (setf (gethash "last_prune_at" *replay-capsule-pruning*) now
        (gethash "last_prune_reason" *replay-capsule-pruning*) reason
        (gethash "last_prune_status" *replay-capsule-pruning*)
        (if changed "pruned" "within-limits")))

(defun %replay-prune (now)
  (let ((changed nil))
    (let* ((before (length *replay-capsules*))
           (retained
             (remove-if
              (lambda (row)
                (> (- now (gethash "captured_at" row 0))
                   *replay-capsule-retention-seconds*))
              *replay-capsules*)))
      (when (< (length retained) before)
        (%replay-stat-inc "retention" (- before (length retained)))
        (setf changed t))
      (setf *replay-capsules* retained))
    (when (> (length *replay-capsules*) *replay-capsule-record-cap*)
      (%replay-stat-inc "record_cap"
                        (- (length *replay-capsules*)
                           *replay-capsule-record-cap*))
      (setf *replay-capsules*
            (subseq *replay-capsules* 0 *replay-capsule-record-cap*)
            changed t))
    (loop while (and *replay-capsules*
                     (> (1+ (%replay-encoded-bytes
                             (coerce *replay-capsules* 'vector)))
                        *replay-capsule-disk-max-bytes*))
          do (setf *replay-capsules* (butlast *replay-capsules*))
             (%replay-stat-inc "byte_ceiling")
             (setf changed t))
    (%replay-note-prune now "retention-record-disk" changed)
    *replay-capsules*))

(defun %replay-save ()
  (ensure-directories-exist *replay-capsule-file*)
  (let* ((encoded (%replay-json-string (coerce *replay-capsules* 'vector)))
         (bytes (1+ (%replay-utf8-bytes encoded))))
    (when (> bytes *replay-capsule-shard-max-bytes*)
      (error "Replay capsule shard exceeds ~d bytes" *replay-capsule-shard-max-bytes*))
    (let ((tmp (make-pathname :name "replay-capsules-tmp" :type "json"
                              :defaults *replay-capsule-file*)))
      (with-open-file (out tmp :direction :output :if-exists :supersede
                               :if-does-not-exist :create :external-format :utf-8)
        (write-string encoded out) (terpri out) (finish-output out))
      (uiop:rename-file-overwriting-target tmp *replay-capsule-file*))))

(defun %replay-budget-count (capture-class now)
  (let ((day (%replay-day now)))
    (count-if (lambda (row)
                (and (string= capture-class
                              (gethash "capture_class" row ""))
                     (string= day (gethash "captured_day" row ""))))
              *replay-capsules*)))

(defun %replay-budget-limit (capture-class)
  (cond ((string= capture-class "scheduled")
         *replay-capsule-scheduled-max-per-day*)
        ((string= capture-class "event")
         *replay-capsule-event-max-per-day*)
        ((string= capture-class "manual")
         *replay-capsule-manual-max-per-day*)
        (t 0)))

(defun %replay-source-key (capsule)
  (let* ((wrapped (gethash "references" capsule))
         (refs (and (hash-table-p wrapped) (gethash "value" wrapped)))
         (ids (and (hash-table-p refs) (gethash "ids" refs))))
    (or (and (hash-table-p ids)
             (or (gethash "source_event_id" ids)
                 (gethash "envelope_id" ids)))
        (and (string= (gethash "capture_class" capsule "") "manual")
             (gethash "reference_sha256" capsule)))))

(defun %replay-store-capsule (capsule now)
  (bt:with-lock-held (*replay-capsule-lock*)
    ;; Bind the delta to the actual immediately preceding stored capsule, not
    ;; merely the capsule visible while this record was assembled outside the
    ;; lock. Concurrent observe-only callbacks therefore form one stable chain.
    (setf (gethash "previous_state_sha256" capsule)
          (or (%replay-last-state-sha) :null))
    (let ((bytes (%replay-encoded-bytes capsule)))
      (when (> bytes *replay-capsule-record-max-bytes*)
        (%replay-stat-inc "oversize_rejected")
        (return-from %replay-store-capsule (values nil "record-too-large")))
      (let* ((class (gethash "capture_class" capsule))
             (limit (%replay-budget-limit class))
             (key (%replay-source-key capsule)))
        (when (>= (%replay-budget-count class now) limit)
          (%replay-stat-inc "budget_suppressed")
          (return-from %replay-store-capsule (values nil "daily-budget")))
        (when (and key
                   (find key *replay-capsules* :test #'string=
                         :key #'%replay-source-key))
          (%replay-stat-inc "duplicate_suppressed")
          (return-from %replay-store-capsule (values nil "duplicate-source")))
        (push capsule *replay-capsules*)
        (%replay-prune now)
        (%replay-save))))
  (when (fboundp 'runtime-observer-emit)
    (runtime-observer-emit "replay-capsule-captured" capsule))
  (values capsule "captured"))

(defun %replay-event-admissible-p (actual event)
  (if (string= actual "turn-capture-complete")
      (let ((payload (%replay-event-payload event)))
        (and (hash-table-p payload)
             (>= (gethash "entry_count" payload 0) 2)))
      t))

(defun %replay-observe-event (actual trigger-type event)
  (when (%replay-event-admissible-p actual event)
    (let* ((now (%replay-now))
           (refs (%replay-event-references event trigger-type))
           (capsule (%replay-build-capsule
                     "event" trigger-type refs
                     (%replay-event-outcome event trigger-type) now)))
      ;; Never write the replay shard on the observed path. The callback has
      ;; already reduced EVENT to bounded IDs/hashes; only that content-free
      ;; capsule enters the bounded queue consumed by the independent worker.
      (bt:with-lock-held (*replay-capsule-event-queue-lock*)
        (if (>= (length *replay-capsule-event-queue*)
                *replay-capsule-event-queue-cap*)
            (bt:with-lock-held (*replay-capsule-lock*)
              (%replay-stat-inc "queue_dropped"))
            (setf *replay-capsule-event-queue*
                  (nconc *replay-capsule-event-queue* (list capsule)))))
      t)))

(defun %replay-drain-event-queue ()
  (bt:with-lock-held (*replay-capsule-event-queue-lock*)
    (prog1 *replay-capsule-event-queue*
      (setf *replay-capsule-event-queue* nil))))

(defun replay-capsule-worker-step (&key (scheduled t) (now (%replay-now)))
  "Persist queued safe capsules off-path, then run a due scheduled capture."
  (let ((stored 0))
    (dolist (capsule (%replay-drain-event-queue))
      (multiple-value-bind (record status)
          (%replay-store-capsule capsule (gethash "captured_at" capsule now))
        (declare (ignore status))
        (when record (incf stored))))
    (when (and scheduled *replay-capsule-next-scheduled-at*
               (>= now *replay-capsule-next-scheduled-at*))
      (replay-capsule-scheduled-step now)
      (setf *replay-capsule-next-scheduled-at*
            (+ now *replay-capsule-scheduled-interval-seconds*)))
    stored))

(defun %replay-observer-name (trigger-type)
  (format nil "replay-capture-~a" trigger-type))

(defun replay-capsule-register-all ()
  (dolist (spec *replay-capsule-event-specs*)
    (destructuring-bind (actual trigger-type) spec
      (let ((actual-copy actual) (trigger-copy trigger-type))
        (runtime-observer-register
         actual-copy (%replay-observer-name trigger-copy)
         (lambda (event) (%replay-observe-event actual-copy trigger-copy event))
         :capability :observe :required t))))
  t)

(defun replay-capsule-assert ()
  (unless (runtime-observer-assert)
    (error "Base runtime observer assertion failed for A3"))
  (dolist (spec *replay-capsule-event-specs* t)
    (destructuring-bind (actual trigger-type) spec
      (let* ((name (%replay-observer-name trigger-type))
             (matches (count name (gethash actual *runtime-observers*)
                             :test #'string=
                             :key (lambda (row) (gethash "name" row)))))
        (unless (= matches 1)
          (error "Required A3 observer ~a/~a count is ~d"
                 actual name matches))))))

(defun %replay-last-scheduled-at ()
  (loop for row in *replay-capsules*
        when (string= (gethash "capture_class" row "") "scheduled")
          do (return (gethash "captured_at" row 0))
        finally (return 0)))

(defun %replay-outbound-since (since)
  (let ((rows (if (boundp '*public-outbound-records*)
                  *public-outbound-records* nil))
        (ids nil) (count 0))
    (dolist (row rows)
      (when (> (gethash "recorded_at" row 0) since)
        (incf count)
        (let* ((envelope (gethash "envelope" row))
               (id (and (hash-table-p envelope) (gethash "id" envelope))))
          (when (and id (< (length ids) *replay-capsule-reference-id-cap*))
            (push (format nil "~a" id) ids)))))
    (values count (coerce (nreverse ids) 'vector))))

(defun replay-capsule-due-p (&optional (now (%replay-now)))
  (let ((last (%replay-last-scheduled-at)))
    (or (zerop last)
        (>= (- now last) *replay-capsule-scheduled-interval-seconds*))))

(defun replay-capsule-scheduled-step (&optional (now (%replay-now)))
  "Capture one exact elapsed-interval observation. Never calls a model."
  (unless (replay-capsule-due-p now)
    (return-from replay-capsule-scheduled-step (values nil "not-due")))
  (let ((since (%replay-last-scheduled-at)))
    (multiple-value-bind (count ids) (%replay-outbound-since since)
      (let* ((negative (zerop count))
             (refs (obj "interval_start" since "interval_end" now
                        "outbound_record_count" count
                        "outbound_record_ids" ids))
             (negative-space
               (%replay-fidelity
                (obj "no_public_outbound" (if negative t nil)
                     "outbound_record_count" count
                     "outbound_record_ids" ids)
                "exact" "derived from the bounded A1 outbound ledger"))
             (capsule (%replay-build-capsule
                       "scheduled" "scheduled-negative-space" refs
                       (if negative "negative" "positive") now
                       :negative-space negative-space)))
        (%replay-store-capsule capsule now)))))

(defun %replay-safe-slug-p (value)
  (and (stringp value)
       (cl-ppcre:scan "^[a-z0-9][a-z0-9-]{0,63}$" value)))

(defun replay-capsule-manual-capture (label &key trigger-id (now (%replay-now)))
  "Capture a bounded operator marker. LABEL and TRIGGER-ID are IDs, not prose."
  (unless (%replay-safe-slug-p label)
    (error "Replay manual label must be a lowercase slug of at most 64 characters"))
  (when (and trigger-id (not (%replay-safe-slug-p trigger-id)))
    (error "Replay manual trigger ID must be a lowercase slug"))
  (let* ((refs (obj "operator_label" label
                    "trigger_id" (or trigger-id :null)))
         (capsule (%replay-build-capsule
                   "manual" "manual-safe-capture" refs "observed" now)))
    (%replay-store-capsule capsule now)))

(defun replay-capsule-capture (reason &key trigger-type trigger-id
                                      (now (%replay-now)))
  "Compatibility entry point for bounded manual capture. Never accepts prose."
  (declare (ignore trigger-type))
  (replay-capsule-manual-capture reason :trigger-id trigger-id :now now))

(defun replay-capsule-extract-fixture (capsule-id fixture-name)
  "Copy one content-free capsule into the fixed captured-fixture root."
  (unless (and (%replay-safe-id capsule-id) (%replay-safe-slug-p fixture-name))
    (error "Invalid capsule or fixture identifier"))
  (let ((capsule (find capsule-id *replay-capsules* :test #'string=
                       :key (lambda (row) (gethash "id" row)))))
    (unless capsule (error "Replay capsule not found"))
    (ensure-directories-exist *replay-capsule-fixture-root*)
    (let ((path (merge-pathnames (format nil "~a.json" fixture-name)
                                 *replay-capsule-fixture-root*)))
      (with-open-file (out path :direction :output :if-exists :error
                                :if-does-not-exist :create :external-format :utf-8)
        (write-string
         (%replay-json-string
          (obj "schema_version" 1 "source" "captured-production-event"
               "capsule" capsule))
         out)
        (terpri out) (finish-output out))
      path)))

(defun replay-capsule-records ()
  (bt:with-lock-held (*replay-capsule-lock*)
    (copy-list *replay-capsules*)))

(defun %replay-count-table (key)
  (let ((counts (obj)))
    (dolist (row *replay-capsules*)
      (let ((value (format nil "~a" (gethash key row "unavailable"))))
        (incf (gethash value counts 0))))
    counts))

(defun %replay-fidelity-counts ()
  (let ((counts (obj "exact" 0 "derived" 0 "unavailable" 0)))
    (dolist (row *replay-capsules*)
      (dolist (key '("references" "conversation" "state" "negative_space"))
        (let* ((wrapped (gethash key row))
               (fidelity (and (hash-table-p wrapped)
                              (gethash "fidelity" wrapped))))
          (when (member fidelity '("exact" "derived" "unavailable")
                        :test #'string=)
            (incf (gethash fidelity counts 0))))))
    counts))

(defun %replay-file-bytes ()
  (if (probe-file *replay-capsule-file*)
      (with-open-file (in *replay-capsule-file* :direction :input
                                                :element-type '(unsigned-byte 8))
        (file-length in))
      0))

(defun replay-capsule-report ()
  (let ((queue-depth
          (bt:with-lock-held (*replay-capsule-event-queue-lock*)
            (length *replay-capsule-event-queue*))))
    (bt:with-lock-held (*replay-capsule-lock*)
      (let ((encoded-bytes (%replay-encoded-bytes
                            (coerce *replay-capsules* 'vector))))
      (obj "schema_version" 2 "records" (length *replay-capsules*)
           "record_cap" *replay-capsule-record-cap*
           "retention_hours" (/ *replay-capsule-retention-seconds* 3600)
           "scheduled_interval_seconds"
           *replay-capsule-scheduled-interval-seconds*
           "scheduled_max_per_day" *replay-capsule-scheduled-max-per-day*
           "event_max_per_day" *replay-capsule-event-max-per-day*
           "manual_max_per_day" *replay-capsule-manual-max-per-day*
           "event_queue_cap" *replay-capsule-event-queue-cap*
           "event_queue_depth" queue-depth
           "record_max_bytes" *replay-capsule-record-max-bytes*
           "shard_max_bytes" *replay-capsule-shard-max-bytes*
           "disk_max_bytes" *replay-capsule-disk-max-bytes*
           "encoded_bytes" encoded-bytes
           "file_bytes" (%replay-file-bytes)
           "autostart" (if *replay-capsule-autostart-p* t nil)
           "worker_alive" (if (and *replay-capsule-thread*
                                    (bt:thread-alive-p *replay-capsule-thread*))
                               t nil)
           "last_capture_at" (if *replay-capsules*
                                  (gethash "captured_at" (first *replay-capsules*))
                                  :null)
           "coverage"
           (obj "trigger_type" (%replay-count-table "trigger_type")
                "capture_class" (%replay-count-table "capture_class")
                "day" (%replay-count-table "captured_day")
                "outcome" (%replay-count-table "outcome")
                "fidelity" (%replay-fidelity-counts))
           "pruning" *replay-capsule-pruning*
           "retains_conversation_content" nil
           "provider_calls" 0 "delivery_authority" nil)))))

(defun replay-capsule-start ()
  (replay-capsule-assert)
  (unless (and *replay-capsule-thread*
               (bt:thread-alive-p *replay-capsule-thread*))
    (setf *replay-capsule-stop-requested* nil
          *replay-capsule-next-scheduled-at*
          (+ (%replay-now) *replay-capsule-scheduled-interval-seconds*)
          *replay-capsule-thread*
          (bt:make-thread
           (lambda ()
             (loop until *replay-capsule-stop-requested*
                   do (funcall *replay-capsule-sleep-fn*
                               *replay-capsule-worker-poll-seconds*)
                      (unless *replay-capsule-stop-requested*
                        (handler-case (replay-capsule-worker-step)
                          (error (condition)
                            (format t "~&[replay-capsules] scheduled step failed: ~a~%"
                                    condition))))))
           :name "replay-capsule-scheduler")))
  t)

(defun replay-capsule-stop (&optional (timeout 5))
  (setf *replay-capsule-stop-requested* t)
  (loop repeat (* 10 timeout)
        while (and *replay-capsule-thread*
                   (bt:thread-alive-p *replay-capsule-thread*))
        do (sleep 0.1))
  (when (and *replay-capsule-thread*
             (bt:thread-alive-p *replay-capsule-thread*))
    (ignore-errors (bt:destroy-thread *replay-capsule-thread*)))
  (setf *replay-capsule-thread* nil)
  ;; A final off-path drain prevents an orderly stop from losing already
  ;; sanitized, accepted observations. It performs no scheduled capture.
  (ignore-errors (replay-capsule-worker-step :scheduled nil))
  t)

(handler-case
    (when (probe-file *replay-capsule-file*)
      (setf *replay-capsules*
            (coerce (shasht:read-json
                     (uiop:read-file-string *replay-capsule-file*)) 'list))
      (setf *replay-capsules*
            (remove-if
             (lambda (row)
               (let ((oversize (> (%replay-encoded-bytes row)
                                  *replay-capsule-record-max-bytes*)))
                 (when oversize (%replay-stat-inc "oversize_rejected"))
                 oversize))
             *replay-capsules*))
      (%replay-prune (%replay-now)))
  (error (condition)
    (format t "~&[replay-capsules] load failed; starting empty: ~a~%" condition)
    (setf *replay-capsules* nil)))

(define-init :install replay-capsule-registry
    "Register replay-capsule capture observers."
  (replay-capsule-register-all))
(define-init :verify replay-capsule-assert-boot
    "Fail closed if capsule capture is not correctly wired."
  (replay-capsule-assert))
