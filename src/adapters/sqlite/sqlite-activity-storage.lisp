;;;; Original-ledger working windows; only explicit preparation writes an index.
(in-package :agent)

(define-condition storage-activity-window-limit (storage-error) ())

(defmethod storage-activity-index-ready-p ((backend sqlite-storage))
  (bt:with-lock-held ((%sqlite-storage-lock backend))
    (let ((handle (%sqlite-handle backend :activity-index-ready)))
      (%with-sqlite-statement
          (s handle "SELECT 1 FROM sqlite_master WHERE type='index' AND name='pai_activity_root_idx' LIMIT 1"
             :activity-index-ready)
        (let ((code (%sqlite-step-raw s)))
          (cond ((= code +sqlite-row+) t)
                ((= code +sqlite-done+) nil)
                (t (%sqlite-check code handle :activity-index-ready))))))))

(defmethod storage-root-has-event-type-p
    ((backend sqlite-storage) agent-id caused-by-root-id event-types
     &key through-position source-boundary newest-p)
  (%storage-required-string agent-id "agent-id")
  (%storage-positive-integer caused-by-root-id "caused-by-root-id")
  (let ((types (cond ((stringp event-types) (list event-types))
                     ((and (vectorp event-types) (not (stringp event-types)))
                      (coerce event-types 'list))
                     ((listp event-types) event-types)
                     (t nil))))
    (unless (and (<= 1 (length types) 128)
                 (every (lambda (type)
                          (and (stringp type) (<= 1 (length type) 256))) types)
                 (not (eq (not (null through-position))
                          (not (null source-boundary)))))
      (error 'storage-error :operation :root-type-presence
             :detail "Expected one to 128 event types and exactly one frontier"))
    (when through-position
      (%storage-positive-integer through-position "through-position"
                                 :zero-allowed t))
    (when source-boundary
      (unless (and (hash-table-p source-boundary)
                   (eql 1 (gethash "schema_version" source-boundary))
                   (equal agent-id (gethash "agent_id" source-boundary))
                   (stringp (gethash "storage_id" source-boundary))
                   (stringp (gethash "source_binding" source-boundary)))
        (error 'storage-error :operation :root-type-presence
               :detail "Invalid source boundary"))
      (setf through-position
            (%storage-positive-integer
             (gethash "through_storage_position" source-boundary)
             "through-storage-position" :zero-allowed t))
      (%storage-positive-integer (gethash "through_event_id" source-boundary)
                                 "through-event-id" :zero-allowed t))
    (bt:with-lock-held ((%sqlite-storage-lock backend))
      (let ((handle (%sqlite-handle backend :root-type-presence)))
        (%sqlite-exec handle "BEGIN" :root-type-presence)
        (handler-case
            (multiple-value-prog1
                (progn
                  ;; This read pins the snapshot, including the index catalogue.
                  (%with-sqlite-statement
                      (s handle "SELECT MAX(storage_sequence) FROM pai_events"
                         :root-type-presence)
                    (%sqlite-step handle s :root-type-presence +sqlite-row+)
                    (when (> through-position (%sqlite-column-int64 s 0))
                      (error 'storage-conflict-error :operation :root-type-presence
                             :detail "Frontier exceeds durable event history")))
                  (when source-boundary
                    (let ((boundary-id 0) (boundary-hash "empty")
                          (storage-id (%sqlite-storage-id-unlocked
                                       handle :root-type-presence)))
                      (when (plusp through-position)
                        (%with-sqlite-statement
                            (s handle "SELECT event_id,agent_id,partition_status,event_type,occurred_at,event_json,integrity_hash FROM pai_events WHERE storage_sequence=?1 AND agent_id=?2"
                               :root-type-presence)
                          (%sqlite-bind-int64 handle s 1 through-position
                                              :root-type-presence)
                          (%sqlite-bind-text handle s 2 agent-id
                                             :root-type-presence)
                          (%sqlite-step handle s :root-type-presence +sqlite-row+)
                          (let ((event (%sqlite-verified-event
                                        (%sqlite-column-int64 s 0)
                                        (%sqlite-column-text s 1)
                                        (%sqlite-column-text s 2)
                                        (%sqlite-column-text s 3)
                                        (%sqlite-column-text s 4)
                                        (%sqlite-column-text s 5)
                                        (%sqlite-column-text s 6)
                                        :root-type-presence)))
                            (setf boundary-id (gethash "id" event)
                                  boundary-hash (%sqlite-column-text s 6)))))
                      (unless (and (equal storage-id
                                          (gethash "storage_id" source-boundary))
                                   (= boundary-id
                                      (gethash "through_event_id" source-boundary))
                                   (equal (gethash "source_binding" source-boundary)
                                          (%storage-sha256
                                           (%storage-checkpoint-integrity-input
                                            "event-source" agent-id boundary-id
                                            through-position storage-id
                                            (write-to-string boundary-id)
                                            boundary-hash))))
                        (error 'storage-conflict-error :operation :root-type-presence
                               :detail "Source boundary does not match snapshot"))))
                  (let ((sql (format nil
                                     "SELECT event_id,agent_id,partition_status,event_type,occurred_at,event_json,integrity_hash,storage_sequence,json_extract(event_json,'$.caused_by') FROM pai_events INDEXED BY pai_activity_root_idx WHERE agent_id=?1 AND json_extract(event_json,'$.caused_by')=?2 AND event_type IN (~{?~d~^,~}) AND storage_sequence<=?~d ORDER BY ~a LIMIT 1"
                                     (loop for i from 3 repeat (length types)
                                           collect i)
                                     (+ 3 (length types))
                                     (if newest-p "storage_sequence DESC"
                                         "event_type,storage_sequence"))))
                    (%with-sqlite-statement (s handle sql :root-type-presence)
                      (%sqlite-bind-text handle s 1 agent-id :root-type-presence)
                      (%sqlite-bind-int64 handle s 2 caused-by-root-id
                                          :root-type-presence)
                      (loop for type in types for i from 3 do
                        (%sqlite-bind-text handle s i type :root-type-presence))
                      (%sqlite-bind-int64 handle s (+ 3 (length types))
                                          through-position :root-type-presence)
                      (let ((code (%sqlite-step-raw s)))
                        (cond
                          ((= code +sqlite-done+) (values nil nil))
                          ((= code +sqlite-row+)
                           (when (> (%sqlite-column-bytes-raw s 5) 8388608)
                             (error 'storage-error :operation :root-type-presence
                                    :detail "Witness exceeds byte bound"))
                           (let* ((indexed-agent (%sqlite-column-text s 1))
                                  (indexed-type (%sqlite-column-text s 3))
                                  (position (%sqlite-column-int64 s 7))
                                  (indexed-root (%sqlite-column-int64 s 8))
                                  (event (%sqlite-verified-event
                                          (%sqlite-column-int64 s 0) indexed-agent
                                          (%sqlite-column-text s 2) indexed-type
                                          (%sqlite-column-text s 4)
                                          (%sqlite-column-text s 5)
                                          (%sqlite-column-text s 6)
                                          :root-type-presence)))
                             (unless (and (equal agent-id indexed-agent)
                                          (member indexed-type types :test #'equal)
                                          (= caused-by-root-id indexed-root)
                                          (eql caused-by-root-id
                                               (gethash "caused_by" event))
                                          (<= position through-position))
                               (error 'storage-integrity-error
                                      :operation :root-type-presence
                                      :detail "Witness index fields disagree with request"))
                             (values t event)))
                          (t (%sqlite-check code handle :root-type-presence)))))))
              (%sqlite-exec handle "COMMIT" :root-type-presence))
          (error (condition)
            (ignore-errors (%sqlite-exec handle "ROLLBACK" :root-type-presence))
            (error condition)))))))

(defmethod storage-prepare-activity-index ((backend sqlite-storage))
  (bt:with-lock-held ((%sqlite-storage-lock backend))
    (%sqlite-exec
     (%sqlite-handle backend :activity-index)
     "CREATE INDEX IF NOT EXISTS pai_activity_root_idx ON pai_events(agent_id,json_extract(event_json,'$.caused_by'),event_type,storage_sequence)"
     :activity-index)
    (%sqlite-exec
     (%sqlite-handle backend :activity-index)
     "CREATE INDEX IF NOT EXISTS pai_activity_scope_idx ON pai_events(agent_id,json_extract(event_json,'$.payload.persona_id'),json_extract(event_json,'$.payload.channel'),json_extract(event_json,'$.payload.resource_id'),storage_sequence DESC) WHERE event_type='sustained-activity-revised'"
     :activity-index)
    (%sqlite-exec
     (%sqlite-handle backend :activity-index)
     "CREATE INDEX IF NOT EXISTS pai_activity_identity_idx ON pai_events(agent_id,json_extract(event_json,'$.payload.persona_id'),json_extract(event_json,'$.payload.channel'),json_extract(event_json,'$.payload.resource_id'),json_extract(event_json,'$.payload.activity_id'),storage_sequence DESC) WHERE event_type='sustained-activity-revised'"
     :activity-index))
  t)

(defmethod storage-latest-activity-reference
    ((backend sqlite-storage) agent-id persona-id channel resource-id &key activity-id)
  (dolist (value (list agent-id persona-id channel resource-id))
    (%storage-required-string value "activity scope"))
  (when activity-id (%storage-required-string activity-id "activity-id"))
  (bt:with-lock-held ((%sqlite-storage-lock backend))
    (let ((handle (%sqlite-handle backend :activity-latest)))
      (%with-sqlite-statement
          (s handle
             (concatenate 'string
               "SELECT event_id,agent_id,partition_status,event_type,occurred_at,event_json,integrity_hash FROM pai_events INDEXED BY "
               (if activity-id "pai_activity_identity_idx" "pai_activity_scope_idx")
               " WHERE event_type='sustained-activity-revised' AND agent_id=?1 AND json_extract(event_json,'$.payload.persona_id')=?2 AND json_extract(event_json,'$.payload.channel')=?3 AND json_extract(event_json,'$.payload.resource_id')=?4"
               (if activity-id " AND json_extract(event_json,'$.payload.activity_id')=?5" "")
               " ORDER BY storage_sequence DESC LIMIT 1") :activity-latest)
        (loop for value in (list agent-id persona-id channel resource-id) for i from 1
              do (%sqlite-bind-text handle s i value :activity-latest))
        (when activity-id (%sqlite-bind-text handle s 5 activity-id :activity-latest))
        (let ((code (%sqlite-step-raw s)))
          (cond ((= code +sqlite-done+) nil)
                ((= code +sqlite-row+)
                 (when (> (%sqlite-column-bytes-raw s 5) 65536)
                   (error 'storage-error :operation :activity-latest :detail "Oversized reference"))
                 (let ((event (%sqlite-verified-event
                               (%sqlite-column-int64 s 0) (%sqlite-column-text s 1)
                               (%sqlite-column-text s 2) (%sqlite-column-text s 3)
                               (%sqlite-column-text s 4) (%sqlite-column-text s 5)
                               (%sqlite-column-text s 6) :activity-latest)))
                   (validate-activity-reference (gethash "payload" event))
                   event))
                (t (%sqlite-check code handle :activity-latest))))))))

(defmethod storage-read-activity-context
    ((backend sqlite-storage) reference-event-id
     &key (agent-id "default") through-event-id (maximum-rows 4096)
          (maximum-bytes 8388608))
  (%storage-required-string agent-id "agent-id")
  (%storage-positive-integer reference-event-id "reference-event-id")
  (unless (and (integerp through-event-id) (>= through-event-id reference-event-id)
               (integerp maximum-rows) (<= 1 maximum-rows 4096)
               (integerp maximum-bytes) (<= 1 maximum-bytes 33554432))
    (error 'storage-error :operation :activity-read :detail "Invalid activity read bounds"))
  (bt:with-lock-held ((%sqlite-storage-lock backend))
    (let ((handle (%sqlite-handle backend :activity-read))
          (bytes 0) (count 0) (rows nil) (hashes nil) (positions nil))
      (labels
          ((decode (statement &optional (cap maximum-bytes))
             ;; Inspect byte count before copying or JSON-decoding the body.
             (let ((size (%sqlite-column-bytes-raw statement 5)))
               (when (or (> size cap) (> (+ bytes size) maximum-bytes)
                         (>= count maximum-rows))
                 (error 'storage-activity-window-limit :operation :activity-read
                        :detail "Activity exceeds row or byte allowance; no partial context returned"))
               (incf bytes size) (incf count))
             (push (%sqlite-column-text statement 6) hashes)
             (push (%sqlite-column-int64 statement 7) positions)
             (%sqlite-verified-event
              (%sqlite-column-int64 statement 0) (%sqlite-column-text statement 1)
              (%sqlite-column-text statement 2) (%sqlite-column-text statement 3)
              (%sqlite-column-text statement 4) (%sqlite-column-text statement 5)
              (%sqlite-column-text statement 6) :activity-read))
           (exact (id &optional (cap maximum-bytes))
             (%with-sqlite-statement
                 (s handle "SELECT event_id,agent_id,partition_status,event_type,occurred_at,event_json,integrity_hash,storage_sequence FROM pai_events WHERE agent_id=?1 AND event_id=?2 ORDER BY storage_sequence LIMIT 2" :activity-read)
               (%sqlite-bind-text handle s 1 agent-id :activity-read)
               (%sqlite-bind-int64 handle s 2 id :activity-read)
               (%sqlite-step handle s :activity-read +sqlite-row+)
               (let ((event (decode s cap)))
                 (%sqlite-step handle s :activity-read +sqlite-done+)
                 event))))
        (%sqlite-exec handle "BEGIN" :activity-read)
        (handler-case
            (let* ((reference-event (exact reference-event-id 65536))
                   (reference-position (first positions))
                   (reference-hash (first hashes))
                   (payload (gethash "payload" reference-event))
                   (boundary (exact through-event-id))
                   (boundary-position (first positions))
                   (boundary-hash (first hashes))
                   (storage-id (%sqlite-storage-id-unlocked handle :activity-read)))
              (declare (ignore boundary))
              (unless (and (equal "sustained-activity-revised" (gethash "type" reference-event))
                           (<= reference-position boundary-position))
                (error 'storage-error :operation :activity-read :detail "Invalid reference event/frontier"))
              (validate-activity-reference payload)
              (loop for id across (gethash "evidence_event_ids" payload) do
                (unless (< id reference-event-id)
                  (error 'storage-error :operation :activity-read :detail "Reference evidence must precede revision"))
                (exact id)
                (unless (< (first positions) reference-position)
                  (error 'storage-error :operation :activity-read :detail "Reference evidence is physically later")))
              (let ((previous (gethash "previous_reference_event_id" payload)))
                (when (integerp previous)
                  (unless (< previous reference-event-id)
                    (error 'storage-error :operation :activity-read :detail "Reference predecessor is not earlier"))
                  (let* ((prior (exact previous 65536)) (p (gethash "payload" prior)))
                    (unless (and (equal "sustained-activity-revised" (gethash "type" prior))
                                 (every (lambda (key) (equal (gethash key p) (gethash key payload)))
                                        '("activity_id" "persona_id" "channel" "resource_id")))
                      (error 'storage-error :operation :activity-read :detail "Reference predecessor scope mismatch")))))
              (loop for root across (gethash "root_event_ids" payload) do
                (unless (< root reference-event-id)
                  (error 'storage-error :operation :activity-read :detail "Activity root must precede reference"))
                (let* ((event (exact root)) (p (gethash "payload" event))
                       (metadata (and (hash-table-p p) (gethash "metadata" p))))
                  (unless (and (equal "user-message" (gethash "type" event))
                               (< (first positions) reference-position)
                               (hash-table-p metadata)
                               (equal (gethash "persona_id" payload) (gethash "persona_id" metadata))
                               (equal (gethash "channel" payload) (gethash "channel" p)))
                    (error 'storage-error :operation :activity-read :detail "Activity root scope mismatch"))
                  (push (cons (first positions) event) rows))
                (%with-sqlite-statement
                    (s handle "SELECT event_id,agent_id,partition_status,event_type,occurred_at,event_json,integrity_hash,storage_sequence FROM pai_events INDEXED BY pai_activity_root_idx WHERE agent_id=?1 AND json_extract(event_json,'$.caused_by')=?2 AND storage_sequence<=?3 AND event_type IN ('model-response','recursive-tool-result','agent-message','recursive-root-failed') ORDER BY event_type,storage_sequence LIMIT ?4" :activity-read)
                  (%sqlite-bind-text handle s 1 agent-id :activity-read)
                  (%sqlite-bind-int64 handle s 2 root :activity-read)
                  (%sqlite-bind-int64 handle s 3 boundary-position :activity-read)
                  (%sqlite-bind-int64 handle s 4 (1+ (- maximum-rows count)) :activity-read)
                  (loop for code = (%sqlite-step-raw s) while (= code +sqlite-row+) do
                    (let ((event (decode s)))
                      (push (cons (first positions) event) rows))
                    finally (unless (= code +sqlite-done+) (%sqlite-check code handle :activity-read)))))
              (let ((reference (%storage-json-read (%storage-json payload) :activity-read)))
                (setf (gethash "agent_id" reference) agent-id
                      (gethash "through_event_id" reference) through-event-id)
                (%sqlite-exec handle "COMMIT" :activity-read)
                (values reference (mapcar #'cdr (sort rows #'< :key #'car))
                        (%storage-object "status" "complete" "storage_id" storage-id
                                         "reference_event_id" reference-event-id
                                         "reference_hash" reference-hash
                                         "through_event_id" through-event-id
                                         "through_position" boundary-position
                                         "boundary_hash" boundary-hash
                                         "read_rows" count "read_bytes" bytes))))
          (storage-activity-window-limit (condition)
            (ignore-errors (%sqlite-exec handle "ROLLBACK" :activity-read))
            (values nil nil (%storage-object "status" "read-limit-exceeded"
                                             "reason" (storage-error-detail condition))))
          (error (condition)
            (ignore-errors (%sqlite-exec handle "ROLLBACK" :activity-read))
            (error condition)))))))
