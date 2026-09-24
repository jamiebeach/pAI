;;;; Source-bound, ordered attention inputs.  This is a rebuildable shadow,
;;;; not an authority and not yet a normal-runtime reader.  Selection belongs
;;;; to the caller; the adapter never imports cognitive admission policy.
(in-package :agent)

(export '(storage-shadow-attention-prepare
          storage-shadow-attention-apply-page
          storage-shadow-attention-report
          storage-shadow-attention-read-page))

(defparameter *attention-shadow-revision* "attention-selected-events-v3")

(defun %attention-shadow-hash (&rest values)
  (%storage-sha256-string-parts
   (loop for value in values
         for string = (if (stringp value) value (write-to-string value))
         append (list (write-to-string (length string)) ":" string))))

(defun storage-shadow-attention-prepare (derived)
  (check-type derived sqlite-derived-storage)
  (bt:with-lock-held ((%sqlite-derived-lock derived))
    (%sqlite-derived-in-transaction
     derived :attention-prepare
     (lambda (handle)
       (%sqlite-exec
        handle
        "CREATE TABLE IF NOT EXISTS pai_attention_v3_watermark (agent_id TEXT PRIMARY KEY, projector_revision TEXT NOT NULL, policy_revision TEXT NOT NULL, through_event_id INTEGER NOT NULL, through_position INTEGER NOT NULL, selected_count INTEGER NOT NULL, source_binding TEXT NOT NULL, integrity_hash TEXT NOT NULL); CREATE TABLE IF NOT EXISTS pai_attention_v3_events (agent_id TEXT NOT NULL, selected_ordinal INTEGER NOT NULL, storage_position INTEGER NOT NULL, previous_event_id INTEGER NOT NULL, event_id INTEGER NOT NULL, event_json TEXT NOT NULL, integrity_hash TEXT NOT NULL, PRIMARY KEY(agent_id,selected_ordinal), UNIQUE(agent_id,storage_position))"
        :attention-prepare))))
  t)

(defun %attention-shadow-watermark (handle agent-id)
  (%with-sqlite-statement
      (s handle "SELECT projector_revision,policy_revision,through_event_id,through_position,selected_count,source_binding,integrity_hash FROM pai_attention_v3_watermark WHERE agent_id=?1" :attention-watermark)
    (%sqlite-bind-text handle s 1 agent-id :attention-watermark)
    (let ((code (%sqlite-step-raw s)))
      (cond ((= code +sqlite-done+) nil)
            ((= code +sqlite-row+)
             (obj "projector_revision" (%sqlite-column-text s 0)
                  "policy_revision" (%sqlite-column-text s 1)
                  "through_event_id" (%sqlite-column-int64 s 2)
                  "through_position" (%sqlite-column-int64 s 3)
                  "selected_count" (%sqlite-column-int64 s 4)
                  "source_binding" (%sqlite-column-text s 5)
                  "integrity_hash" (%sqlite-column-text s 6)))
            (t (%sqlite-check code handle :attention-watermark))))))

(defun %attention-shadow-verify-watermark
    (row source agent-id policy-revision)
  (when row
    (unless (and (string= *attention-shadow-revision*
                           (gethash "projector_revision" row))
                 (string= policy-revision (gethash "policy_revision" row))
                 (string= (gethash "integrity_hash" row)
                          (%attention-shadow-hash
                           agent-id (gethash "projector_revision" row)
                           policy-revision (gethash "through_event_id" row)
                           (gethash "through_position" row)
                           (gethash "selected_count" row)
                           (gethash "source_binding" row)))
                 (string= (gethash "source_binding" row)
                          (storage-checkpoint-source-binding
                           source :agent-id agent-id
                           :through-event-id (gethash "through_event_id" row)
                           :through-position (gethash "through_position" row))))
      (error 'storage-conflict-error :operation :attention-shadow
             :detail "attention cursor revision, digest or source binding mismatch")))
  row)

(defun storage-shadow-attention-report
    (derived source &key (agent-id "default") policy-revision)
  (check-type derived sqlite-derived-storage)
  (%storage-required-string agent-id "agent-id")
  (%storage-required-string policy-revision "policy-revision")
  (%attention-shadow-verify-watermark
   (bt:with-lock-held ((%sqlite-derived-lock derived))
     (%attention-shadow-watermark
      (%sqlite-derived-handle derived :attention-report) agent-id))
   source agent-id policy-revision))

(defun storage-shadow-attention-apply-page
    (derived source selector &key (agent-id "default") policy-revision
                              (limit 128) (maximum-event-bytes 1048576))
  "Apply a bounded physical page. SELECTOR returns a source-identity-preserving
event object or NIL.  Selection may retain only policy-relevant events, while
PREVIOUS-EVENT-ID seals the exact journal gap before each retained event."
  (check-type derived sqlite-derived-storage)
  (unless (functionp selector)
    (error 'storage-error :operation :attention-apply :detail "selector required"))
  (%storage-required-string agent-id "agent-id")
  (%storage-required-string policy-revision "policy-revision")
  (unless (and (integerp limit) (<= 1 limit 512)
               (integerp maximum-event-bytes)
               (<= 1 maximum-event-bytes 8388608))
    (error 'storage-error :operation :attention-apply :detail "invalid page bounds"))
  (let* ((prior (storage-shadow-attention-report
                 derived source :agent-id agent-id
                 :policy-revision policy-revision))
         (after (if prior (gethash "through_position" prior) 0))
         (last-id (if prior (gethash "through_event_id" prior) 0))
         (last-position after) (rows nil) (page-bytes 0))
    (multiple-value-bind (complete-p ignored count)
        (storage-map-events
         source
         (lambda (event position)
           (let ((projected (funcall selector event)))
             (when projected
               (unless (and (hash-table-p projected)
                            (eql (gethash "id" event)
                                 (gethash "id" projected))
                            (equal (gethash "type" event)
                                   (gethash "type" projected)))
                 (error 'storage-error :operation :attention-apply
                        :detail "selector changed source event identity"))
               (let* ((json (%storage-json projected))
                      (bytes (length (babel:string-to-octets
                                      json :encoding :utf-8))))
                 (when (or (> bytes maximum-event-bytes)
                           (> (incf page-bytes bytes) 16777216))
                   (error 'storage-error :operation :attention-apply
                          :detail "selected page exceeds byte bound"))
                 (push (list position last-id (gethash "id" event) json)
                       rows))))
           (setf last-id (gethash "id" event)
                 last-position position))
         :agent-id agent-id :after-position after :limit limit)
      (declare (ignore ignored count))
      (unless complete-p
        (error 'storage-error :operation :attention-apply
               :detail "authority page incomplete")))
    (when (and prior (= last-position after))
      (return-from storage-shadow-attention-apply-page prior))
    (let ((binding
            (storage-checkpoint-source-binding
             source :agent-id agent-id :through-event-id last-id
             :through-position last-position)))
      (unless (stringp binding)
        (error 'storage-conflict-error :operation :attention-apply
               :detail "source frontier is not bindable"))
      (bt:with-lock-held ((%sqlite-derived-lock derived))
        (%sqlite-derived-in-transaction
         derived :attention-apply
         (lambda (handle)
           (let ((current (%attention-shadow-watermark handle agent-id)))
             (unless (and (eql after (if current
                                        (gethash "through_position" current) 0))
                          (or (null current)
                              (and prior
                                   (string= (gethash "source_binding" prior)
                                            (gethash "source_binding" current))
                                   (string= policy-revision
                                            (gethash "policy_revision" current)))))
               (error 'storage-conflict-error :operation :attention-apply
                      :detail "attention cursor changed during page read"))
             (let ((selected-count (if current
                                       (gethash "selected_count" current) 0)))
               (%with-sqlite-statement
                   (s handle "INSERT INTO pai_attention_v3_events(agent_id,selected_ordinal,storage_position,previous_event_id,event_id,event_json,integrity_hash) VALUES(?1,?2,?3,?4,?5,?6,?7)" :attention-apply)
                 (dolist (row (nreverse rows))
                   (%sqlite-reset-raw s)
                   (%sqlite-clear-bindings-raw s)
                   (let ((ordinal (incf selected-count)))
                     (%sqlite-bind-text handle s 1 agent-id :attention-apply)
                     (%sqlite-bind-int64 handle s 2 ordinal :attention-apply)
                     (%sqlite-bind-int64 handle s 3 (first row) :attention-apply)
                     (%sqlite-bind-int64 handle s 4 (second row) :attention-apply)
                     (%sqlite-bind-int64 handle s 5 (third row) :attention-apply)
                     (%sqlite-bind-text handle s 6 (fourth row) :attention-apply)
                     (%sqlite-bind-text handle s 7
                                        (%attention-shadow-hash
                                         agent-id ordinal (first row)
                                         (second row) (third row) (fourth row))
                                        :attention-apply)
                     (%sqlite-step handle s :attention-apply +sqlite-done+))))
               (%with-sqlite-statement
                   (s handle "INSERT INTO pai_attention_v3_watermark(agent_id,projector_revision,policy_revision,through_event_id,through_position,selected_count,source_binding,integrity_hash) VALUES(?1,?2,?3,?4,?5,?6,?7,?8) ON CONFLICT(agent_id) DO UPDATE SET through_event_id=excluded.through_event_id,through_position=excluded.through_position,selected_count=excluded.selected_count,source_binding=excluded.source_binding,integrity_hash=excluded.integrity_hash" :attention-apply)
                 (%sqlite-bind-text handle s 1 agent-id :attention-apply)
                 (%sqlite-bind-text handle s 2 *attention-shadow-revision*
                                    :attention-apply)
                 (%sqlite-bind-text handle s 3 policy-revision :attention-apply)
                 (%sqlite-bind-int64 handle s 4 last-id :attention-apply)
                 (%sqlite-bind-int64 handle s 5 last-position :attention-apply)
                 (%sqlite-bind-int64 handle s 6 selected-count :attention-apply)
                 (%sqlite-bind-text handle s 7 binding :attention-apply)
                 (%sqlite-bind-text handle s 8
                                    (%attention-shadow-hash
                                     agent-id *attention-shadow-revision*
                                     policy-revision last-id last-position
                                     selected-count binding)
                                    :attention-apply)
                 (%sqlite-step handle s :attention-apply +sqlite-done+)))))))
    (storage-shadow-attention-report
     derived source :agent-id agent-id :policy-revision policy-revision))))

(defun storage-shadow-attention-read-page
    (derived source &key (agent-id "default") policy-revision
                     (after-ordinal 0) (limit 128))
  "Read a bounded selected page and its sealed cursor.  Never return unverified
JSON or infer completion from a missing row."
  (unless (and (integerp after-ordinal) (<= 0 after-ordinal)
               (integerp limit) (<= 1 limit 512))
    (error 'storage-error :operation :attention-read :detail "invalid page bounds"))
  (let ((watermark
          (storage-shadow-attention-report
           derived source :agent-id agent-id :policy-revision policy-revision)))
    (unless watermark
      (error 'storage-conflict-error :operation :attention-read
             :detail "attention shadow has no sealed cursor"))
    (let ((rows nil))
      (bt:with-lock-held ((%sqlite-derived-lock derived))
        (let ((handle (%sqlite-derived-handle derived :attention-read)))
          (%with-sqlite-statement
              (s handle "SELECT selected_ordinal,storage_position,previous_event_id,event_id,event_json,integrity_hash FROM pai_attention_v3_events WHERE agent_id=?1 AND selected_ordinal>?2 ORDER BY selected_ordinal ASC LIMIT ?3" :attention-read)
            (%sqlite-bind-text handle s 1 agent-id :attention-read)
            (%sqlite-bind-int64 handle s 2 after-ordinal :attention-read)
            (%sqlite-bind-int64 handle s 3 limit :attention-read)
            (loop for code = (%sqlite-step-raw s)
                  while (= code +sqlite-row+)
                  for ordinal = (%sqlite-column-int64 s 0)
                  for position = (%sqlite-column-int64 s 1)
                  for previous-id = (%sqlite-column-int64 s 2)
                  for event-id = (%sqlite-column-int64 s 3)
                  for json = (%sqlite-column-text s 4)
                  for digest = (%sqlite-column-text s 5)
                  do (unless (and (= ordinal (+ after-ordinal
                                                (1+ (length rows))))
                                  (<= position
                                      (gethash "through_position" watermark))
                                  (string= digest
                                           (%attention-shadow-hash
                                            agent-id ordinal position
                                            previous-id event-id json)))
                       (error 'storage-integrity-error :operation :attention-read
                              :detail "attention selected-row gap or digest mismatch"))
                     (let ((event (%storage-json-read json :attention-read)))
                       (unless (eql event-id (gethash "id" event))
                         (error 'storage-integrity-error
                                :operation :attention-read
                                :detail "attention event identity mismatch"))
                       (push (list ordinal position previous-id event) rows))
                  finally (unless (= code +sqlite-done+)
                            (%sqlite-check code handle :attention-read))))))
      (setf rows (nreverse rows))
      (when (and (< (length rows) limit)
                 (< (+ after-ordinal (length rows))
                    (gethash "selected_count" watermark)))
        (error 'storage-integrity-error :operation :attention-read
               :detail "attention rows end before sealed selected count"))
      (unless (equal (gethash "source_binding" watermark)
                     (gethash "source_binding"
                              (storage-shadow-attention-report
                               derived source :agent-id agent-id
                               :policy-revision policy-revision)))
        (error 'storage-conflict-error :operation :attention-read
               :detail "attention cursor advanced during read"))
      (values rows watermark))))
