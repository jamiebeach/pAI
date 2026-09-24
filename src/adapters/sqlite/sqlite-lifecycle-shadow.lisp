;;;; Explicit, rebuildable Q5 lifecycle row projection. No load-time I/O.
;;;; The adapter owns storage and accepts a pure transition function; it does
;;;; not link upward to the cognitive lifecycle implementation.
(in-package :agent)

(export '(storage-shadow-lifecycle-prepare
          storage-shadow-lifecycle-apply-page
          storage-shadow-lifecycle-watermark
          storage-shadow-lifecycle-project))

(defparameter *lifecycle-shadow-revision* "lifecycle-rows-v1")

(defun %lifecycle-shadow-hash (&rest values)
  (%storage-sha256-string-parts
   (loop for value in values
         for string = (if (stringp value) value (write-to-string value))
         append (list (write-to-string (length string)) ":" string))))

(defun storage-shadow-lifecycle-prepare (derived)
  (check-type derived sqlite-derived-storage)
  (bt:with-lock-held ((%sqlite-derived-lock derived))
    (%sqlite-derived-in-transaction
     derived :lifecycle-prepare
     (lambda (handle)
       (%sqlite-exec
        handle
        "CREATE TABLE IF NOT EXISTS pai_lifecycle_v1_watermark (agent_id TEXT PRIMARY KEY, projector_revision TEXT NOT NULL, through_event_id INTEGER NOT NULL, through_position INTEGER NOT NULL, highest_event_id INTEGER NOT NULL, rejected_count INTEGER NOT NULL, source_rejected_count INTEGER NOT NULL, lifecycle_count INTEGER NOT NULL, request_count INTEGER NOT NULL, invalid_count INTEGER NOT NULL, source_binding TEXT NOT NULL, integrity_hash TEXT NOT NULL); CREATE TABLE IF NOT EXISTS pai_lifecycle_v1_rows (agent_id TEXT NOT NULL, lifecycle_id TEXT NOT NULL, row_json TEXT NOT NULL, integrity_hash TEXT NOT NULL, PRIMARY KEY(agent_id,lifecycle_id)); CREATE TABLE IF NOT EXISTS pai_lifecycle_v1_requests (agent_id TEXT NOT NULL, request_id TEXT NOT NULL, event_id INTEGER NOT NULL, integrity_hash TEXT NOT NULL, PRIMARY KEY(agent_id,request_id)); CREATE TABLE IF NOT EXISTS pai_lifecycle_v1_invalid (agent_id TEXT NOT NULL, storage_position INTEGER NOT NULL, event_id INTEGER NOT NULL, integrity_hash TEXT NOT NULL, PRIMARY KEY(agent_id,storage_position))"
        :lifecycle-prepare))))
  t)

(defun %lifecycle-shadow-watermark-unlocked (handle agent-id)
  (%with-sqlite-statement
      (s handle "SELECT projector_revision,through_event_id,through_position,highest_event_id,rejected_count,source_rejected_count,lifecycle_count,request_count,invalid_count,source_binding,integrity_hash FROM pai_lifecycle_v1_watermark WHERE agent_id=?1"
         :lifecycle-watermark)
    (%sqlite-bind-text handle s 1 agent-id :lifecycle-watermark)
    (let ((code (%sqlite-step-raw s)))
      (cond
        ((= code +sqlite-done+) nil)
        ((= code +sqlite-row+)
         (obj "projector_revision" (%sqlite-column-text s 0)
              "through_event_id" (%sqlite-column-int64 s 1)
              "through_position" (%sqlite-column-int64 s 2)
              "highest_event_id" (%sqlite-column-int64 s 3)
              "rejected_count" (%sqlite-column-int64 s 4)
              "source_rejected_count" (%sqlite-column-int64 s 5)
              "lifecycle_count" (%sqlite-column-int64 s 6)
              "request_count" (%sqlite-column-int64 s 7)
              "invalid_count" (%sqlite-column-int64 s 8)
              "source_binding" (%sqlite-column-text s 9)
              "integrity_hash" (%sqlite-column-text s 10)))
        (t (%sqlite-check code handle :lifecycle-watermark))))))

(defun %lifecycle-shadow-check-watermark (row source agent-id)
  (when row
    (unless (and
             (string= *lifecycle-shadow-revision*
                      (gethash "projector_revision" row))
             (string= (gethash "integrity_hash" row)
                      (%lifecycle-shadow-hash
                       agent-id (gethash "projector_revision" row)
                       (gethash "through_event_id" row)
                       (gethash "through_position" row)
                       (gethash "highest_event_id" row)
                       (gethash "rejected_count" row)
                       (gethash "source_rejected_count" row)
                       (gethash "lifecycle_count" row)
                       (gethash "request_count" row)
                       (gethash "invalid_count" row)
                       (gethash "source_binding" row)))
             (string= (gethash "source_binding" row)
                      (storage-checkpoint-source-binding
                       source :agent-id agent-id
                       :through-event-id (gethash "through_event_id" row)
                       :through-position (gethash "through_position" row))))
      (error 'storage-conflict-error :operation :lifecycle-watermark
             :detail "lifecycle cursor revision, digest or source binding mismatch")))
  row)

(defun storage-shadow-lifecycle-watermark
    (derived source &key (agent-id "default"))
  (check-type derived sqlite-derived-storage)
  (%storage-required-string agent-id "agent-id")
  (%lifecycle-shadow-check-watermark
   (bt:with-lock-held ((%sqlite-derived-lock derived))
     (%lifecycle-shadow-watermark-unlocked
      (%sqlite-derived-handle derived :lifecycle-watermark) agent-id))
   source agent-id))

(defun %lifecycle-shadow-row (handle agent-id lifecycle-id)
  (%with-sqlite-statement
      (s handle "SELECT row_json,integrity_hash FROM pai_lifecycle_v1_rows WHERE agent_id=?1 AND lifecycle_id=?2"
         :lifecycle-row)
    (%sqlite-bind-text handle s 1 agent-id :lifecycle-row)
    (%sqlite-bind-text handle s 2 lifecycle-id :lifecycle-row)
    (let ((code (%sqlite-step-raw s)))
      (cond
        ((= code +sqlite-done+) nil)
        ((= code +sqlite-row+)
         (let ((json (%sqlite-column-text s 0))
               (digest (%sqlite-column-text s 1)))
           (unless (string= digest (%lifecycle-shadow-hash
                                    agent-id lifecycle-id json))
             (error 'storage-integrity-error :operation :lifecycle-row
                    :detail "lifecycle row digest mismatch"))
           (%storage-json-read json :lifecycle-row)))
        (t (%sqlite-check code handle :lifecycle-row))))))

(defun %lifecycle-shadow-request (handle agent-id request-id)
  (%with-sqlite-statement
      (s handle "SELECT event_id,integrity_hash FROM pai_lifecycle_v1_requests WHERE agent_id=?1 AND request_id=?2"
         :lifecycle-request)
    (%sqlite-bind-text handle s 1 agent-id :lifecycle-request)
    (%sqlite-bind-text handle s 2 request-id :lifecycle-request)
    (let ((code (%sqlite-step-raw s)))
      (cond
        ((= code +sqlite-done+) nil)
        ((= code +sqlite-row+)
         (let ((id (%sqlite-column-int64 s 0))
               (digest (%sqlite-column-text s 1)))
           (unless (string= digest (%lifecycle-shadow-hash
                                    agent-id request-id id))
             (error 'storage-integrity-error :operation :lifecycle-request
                    :detail "lifecycle request digest mismatch"))
           id))
        (t (%sqlite-check code handle :lifecycle-request))))))

(defun %lifecycle-shadow-verified-request-count (handle agent-id)
  (%with-sqlite-statement
      (s handle "SELECT request_id,event_id,integrity_hash FROM pai_lifecycle_v1_requests WHERE agent_id=?1"
         :lifecycle-request-verify)
    (%sqlite-bind-text handle s 1 agent-id :lifecycle-request-verify)
    (loop with count = 0
          for code = (%sqlite-step-raw s)
          while (= code +sqlite-row+)
          for request-id = (%sqlite-column-text s 0)
          for event-id = (%sqlite-column-int64 s 1)
          for digest = (%sqlite-column-text s 2)
          do (unless (string= digest
                              (%lifecycle-shadow-hash
                               agent-id request-id event-id))
               (error 'storage-integrity-error
                      :operation :lifecycle-request-verify
                      :detail "lifecycle request digest mismatch"))
             (incf count)
          finally (progn
                    (unless (= code +sqlite-done+)
                      (%sqlite-check code handle :lifecycle-request-verify))
                    (return count)))))

(defun %lifecycle-shadow-write-row (handle agent-id lifecycle-id row)
  (let ((json (%storage-json row)))
    (%with-sqlite-statement
        (s handle "INSERT INTO pai_lifecycle_v1_rows(agent_id,lifecycle_id,row_json,integrity_hash) VALUES(?1,?2,?3,?4) ON CONFLICT(agent_id,lifecycle_id) DO UPDATE SET row_json=excluded.row_json,integrity_hash=excluded.integrity_hash"
           :lifecycle-write-row)
      (%sqlite-bind-text handle s 1 agent-id :lifecycle-write-row)
      (%sqlite-bind-text handle s 2 lifecycle-id :lifecycle-write-row)
      (%sqlite-bind-text handle s 3 json :lifecycle-write-row)
      (%sqlite-bind-text handle s 4
                         (%lifecycle-shadow-hash agent-id lifecycle-id json)
                         :lifecycle-write-row)
      (%sqlite-step handle s :lifecycle-write-row +sqlite-done+))))

(defun %lifecycle-shadow-write-request (handle agent-id request-id event-id)
  (%with-sqlite-statement
      (s handle "INSERT INTO pai_lifecycle_v1_requests(agent_id,request_id,event_id,integrity_hash) VALUES(?1,?2,?3,?4)"
         :lifecycle-write-request)
    (%sqlite-bind-text handle s 1 agent-id :lifecycle-write-request)
    (%sqlite-bind-text handle s 2 request-id :lifecycle-write-request)
    (%sqlite-bind-int64 handle s 3 event-id :lifecycle-write-request)
    (%sqlite-bind-text handle s 4
                       (%lifecycle-shadow-hash agent-id request-id event-id)
                       :lifecycle-write-request)
    (%sqlite-step handle s :lifecycle-write-request +sqlite-done+)))

(defun %lifecycle-shadow-write-invalid
    (handle agent-id position event-id)
  (%with-sqlite-statement
      (s handle "INSERT INTO pai_lifecycle_v1_invalid(agent_id,storage_position,event_id,integrity_hash) VALUES(?1,?2,?3,?4)"
         :lifecycle-write-invalid)
    (%sqlite-bind-text handle s 1 agent-id :lifecycle-write-invalid)
    (%sqlite-bind-int64 handle s 2 position :lifecycle-write-invalid)
    (%sqlite-bind-int64 handle s 3 event-id :lifecycle-write-invalid)
    (%sqlite-bind-text handle s 4
                       (%lifecycle-shadow-hash agent-id position event-id)
                       :lifecycle-write-invalid)
    (%sqlite-step handle s :lifecycle-write-invalid +sqlite-done+)))

(defun %lifecycle-shadow-write-watermark
    (handle agent-id event-id position highest rejected source-rejected
     lifecycle-count request-count invalid-count binding)
  (%with-sqlite-statement
      (s handle "INSERT INTO pai_lifecycle_v1_watermark(agent_id,projector_revision,through_event_id,through_position,highest_event_id,rejected_count,source_rejected_count,lifecycle_count,request_count,invalid_count,source_binding,integrity_hash) VALUES(?1,?2,?3,?4,?5,?6,?7,?8,?9,?10,?11,?12) ON CONFLICT(agent_id) DO UPDATE SET through_event_id=excluded.through_event_id,through_position=excluded.through_position,highest_event_id=excluded.highest_event_id,rejected_count=excluded.rejected_count,source_rejected_count=excluded.source_rejected_count,lifecycle_count=excluded.lifecycle_count,request_count=excluded.request_count,invalid_count=excluded.invalid_count,source_binding=excluded.source_binding,integrity_hash=excluded.integrity_hash"
         :lifecycle-write-watermark)
    (%sqlite-bind-text handle s 1 agent-id :lifecycle-write-watermark)
    (%sqlite-bind-text handle s 2 *lifecycle-shadow-revision*
                       :lifecycle-write-watermark)
    (loop for value in (list event-id position highest rejected source-rejected
                             lifecycle-count request-count invalid-count)
          for index from 3 do
            (%sqlite-bind-int64 handle s index value :lifecycle-write-watermark))
    (%sqlite-bind-text handle s 11 binding :lifecycle-write-watermark)
    (%sqlite-bind-text
     handle s 12
     (%lifecycle-shadow-hash
      agent-id *lifecycle-shadow-revision* event-id position highest
      rejected source-rejected lifecycle-count request-count invalid-count
      binding)
     :lifecycle-write-watermark)
    (%sqlite-step handle s :lifecycle-write-watermark +sqlite-done+)))

(defun storage-shadow-lifecycle-apply-page
    (derived source step &key (agent-id "default") (limit 128))
  "Apply one bounded physical authority page transactionally. STEP receives
EVENT, resolved source-presence, prior lifecycle row, and prior request ID;
it returns next row, new-request-p, invalid-p, rejected increment, and
source-rejected increment. No cognitive policy is duplicated by this adapter."
  (check-type derived sqlite-derived-storage)
  (unless (functionp step)
    (error 'storage-error :operation :lifecycle-apply :detail "step required"))
  (%storage-required-string agent-id "agent-id")
  (unless (and (integerp limit) (<= 1 limit 512))
    (error 'storage-error :operation :lifecycle-apply :detail "invalid page limit"))
  (let* ((prior (storage-shadow-lifecycle-watermark
                 derived source :agent-id agent-id))
         (boundary (storage-authority-boundary source :agent-id agent-id))
         (after (if prior (gethash "through_position" prior) 0))
         (last-id (if prior (gethash "through_event_id" prior) 0))
         (last-position after)
         (rows nil) (page-bytes 0))
    (multiple-value-bind (complete-p ignored count)
        (storage-map-events
         source
         (lambda (event position)
           (let ((bytes (length (babel:string-to-octets
                                 (%storage-json event) :encoding :utf-8))))
             (when (> (incf page-bytes bytes) 16777216)
               (error 'storage-error :operation :lifecycle-apply
                      :detail "authority page exceeds byte bound")))
           ;; STORAGE-MAP-EVENTS holds the source lock during the visitor.
           ;; Resolve references after it returns; re-entering here deadlocks.
           (push (list position event nil) rows)
           (setf last-id (gethash "id" event)
                 last-position position))
         :agent-id agent-id :after-position after
         :through-position (gethash "through_storage_position" boundary)
         :limit limit)
      (declare (ignore ignored))
      (unless complete-p
        (error 'storage-error :operation :lifecycle-apply
               :detail "authority page incomplete"))
      (when (< count limit)
        (setf last-id (gethash "through_event_id" boundary)
              last-position (gethash "through_storage_position" boundary)))
      (when (and prior (= last-position after))
        (return-from storage-shadow-lifecycle-apply-page prior)))
    (dolist (item rows)
      (destructuring-bind (position event ignored) item
        (declare (ignore ignored))
        (let* ((payload (gethash "payload" event))
               (type (gethash "type" event))
               (source-id (and (hash-table-p payload)
                               (gethash "source_event_id" payload))))
          (setf (third item)
                (and (equal agent-id (gethash "agent_id" event))
                     (member type
                             '("conscious-lifecycle-transition"
                               "conscious-lifecycle-result-rejected"
                               "conscious-lifecycle-source-rejected")
                             :test #'string=)
                     (integerp source-id) (plusp source-id)
                     (storage-read-event-before-position
                      source agent-id source-id position)
                     t)))))
    (let ((binding
            (storage-checkpoint-source-binding
             source :agent-id agent-id :through-event-id last-id
             :through-position last-position)))
      (bt:with-lock-held ((%sqlite-derived-lock derived))
        (%sqlite-derived-in-transaction
         derived :lifecycle-apply
         (lambda (handle)
           (let* ((current (%lifecycle-shadow-watermark-unlocked
                            handle agent-id))
                  (highest (if current (gethash "highest_event_id" current) 0))
                  (rejected (if current (gethash "rejected_count" current) 0))
                  (source-rejected
                    (if current (gethash "source_rejected_count" current) 0))
                  (lifecycle-count
                    (if current (gethash "lifecycle_count" current) 0))
                  (request-count
                    (if current (gethash "request_count" current) 0))
                  (invalid-count
                    (if current (gethash "invalid_count" current) 0)))
             (unless (and (eql after (if current
                                        (gethash "through_position" current) 0))
                          (or (null current)
                              (and prior
                                   (string= (gethash "source_binding" prior)
                                            (gethash "source_binding" current))
                                   (string= *lifecycle-shadow-revision*
                                            (gethash "projector_revision"
                                                     current)))))
               (error 'storage-conflict-error :operation :lifecycle-apply
                      :detail "lifecycle cursor changed during page read"))
             (dolist (item (nreverse rows))
               (destructuring-bind (position event source-present) item
                 (let* ((id (gethash "id" event))
                        (type (gethash "type" event))
                        (payload (gethash "payload" event))
                        (lifecycle-id
                          (and (hash-table-p payload)
                               (gethash "lifecycle_id" payload)))
                        (request-id
                          (and (hash-table-p payload)
                               (gethash "request_id" payload))))
                   ;; Legacy rows can be assigned an index partition without
                   ;; carrying agent_id in their immutable envelope. Preserve
                   ;; the pure fold's envelope-scoping semantics exactly.
                   (when (equal agent-id (gethash "agent_id" event))
                   (when (and (integerp id) (> id highest))
                     (setf highest id))
                   (when (member type
                                 '("conscious-lifecycle-transition"
                                   "conscious-lifecycle-result-rejected"
                                   "conscious-lifecycle-source-rejected")
                                 :test #'string=)
                     (let* ((valid-lifecycle-id
                              (and (stringp lifecycle-id)
                                   (<= 1 (length lifecycle-id) 256)
                                   lifecycle-id))
                            (valid-request-id
                              (and (stringp request-id)
                                   (<= 1 (length request-id) 256)
                                   request-id))
                            (old-row
                              (and valid-lifecycle-id
                                   (%lifecycle-shadow-row
                                    handle agent-id valid-lifecycle-id)))
                            (old-request
                              (and valid-request-id
                                   (%lifecycle-shadow-request
                                    handle agent-id valid-request-id))))
                       (multiple-value-bind
                             (new-row new-request-p invalid-p
                              rejected-delta source-rejected-delta)
                           (funcall step event source-present old-row old-request)
                         (when (and new-row valid-lifecycle-id
                                    (not (equalp old-row new-row)))
                           (%lifecycle-shadow-write-row
                            handle agent-id valid-lifecycle-id new-row)
                           (unless old-row (incf lifecycle-count)))
                         (when new-request-p
                           (unless valid-request-id
                             (error 'storage-error :operation :lifecycle-apply
                                    :detail "projector returned request without valid ID"))
                           (%lifecycle-shadow-write-request
                            handle agent-id valid-request-id id)
                           (incf request-count))
                         (when invalid-p
                           (%lifecycle-shadow-write-invalid
                            handle agent-id position id)
                           (incf invalid-count))
                         (incf rejected rejected-delta)
                         (incf source-rejected source-rejected-delta))))))))
             (%lifecycle-shadow-write-watermark
              handle agent-id last-id last-position highest rejected
              source-rejected lifecycle-count request-count invalid-count
              binding)))))
      (storage-shadow-lifecycle-watermark
       derived source :agent-id agent-id))))

(defun storage-shadow-lifecycle-project
    (derived source &key (agent-id "default"))
  "Read verified row state, not historical events. Caller must catch up cursor."
  (let ((watermark (storage-shadow-lifecycle-watermark
                    derived source :agent-id agent-id)))
    (unless watermark
      (error 'storage-conflict-error :operation :lifecycle-project
             :detail "lifecycle row generation is absent"))
    (bt:with-lock-held ((%sqlite-derived-lock derived))
      (let ((handle (%sqlite-derived-handle derived :lifecycle-project))
            (lifecycles (make-hash-table :test #'equal))
            (invalid nil) (active 0) (terminal 0))
        (%sqlite-exec handle "BEGIN" :lifecycle-project)
        (handler-case
            (progn
              (let ((current (%lifecycle-shadow-watermark-unlocked
                              handle agent-id)))
                (unless (and current
                             (string= (gethash "integrity_hash" current)
                                      (gethash "integrity_hash" watermark)))
                  (error 'storage-conflict-error :operation :lifecycle-project
                         :detail "lifecycle cursor changed before row snapshot")))
        (%with-sqlite-statement
            (s handle "SELECT lifecycle_id FROM pai_lifecycle_v1_rows WHERE agent_id=?1 ORDER BY lifecycle_id"
               :lifecycle-project)
          (%sqlite-bind-text handle s 1 agent-id :lifecycle-project)
          (loop for code = (%sqlite-step-raw s)
                while (= code +sqlite-row+)
                for id = (%sqlite-column-text s 0)
                for row = (%lifecycle-shadow-row handle agent-id id)
                do (setf (gethash id lifecycles) row)
                   (if (member (gethash "status" row)
                               '("cancelled" "completed" "failed")
                               :test #'string=)
                       (incf terminal) (incf active))
                finally (unless (= code +sqlite-done+)
                          (%sqlite-check code handle :lifecycle-project))))
        (%with-sqlite-statement
            (s handle "SELECT storage_position,event_id,integrity_hash FROM pai_lifecycle_v1_invalid WHERE agent_id=?1 ORDER BY storage_position"
               :lifecycle-project)
          (%sqlite-bind-text handle s 1 agent-id :lifecycle-project)
          (loop for code = (%sqlite-step-raw s)
                while (= code +sqlite-row+)
                for position = (%sqlite-column-int64 s 0)
                for id = (%sqlite-column-int64 s 1)
                for digest = (%sqlite-column-text s 2)
                do (unless (string= digest
                                    (%lifecycle-shadow-hash
                                     agent-id position id))
                     (error 'storage-integrity-error
                            :operation :lifecycle-project
                            :detail "invalid-row digest mismatch"))
                   (push id invalid)
                finally (unless (= code +sqlite-done+)
                          (%sqlite-check code handle :lifecycle-project))))
              (unless (and (= (hash-table-count lifecycles)
                              (gethash "lifecycle_count" watermark))
                           (= (length invalid)
                              (gethash "invalid_count" watermark))
                           (= (%lifecycle-shadow-verified-request-count
                               handle agent-id)
                              (gethash "request_count" watermark)))
                (error 'storage-integrity-error :operation :lifecycle-project
                       :detail "lifecycle row-family count mismatch"))
              (let ((projection
                      (obj "schema_version" 1 "agent_id" agent-id
                           "highest_event_id"
                           (gethash "highest_event_id" watermark)
                           "active_count" active "terminal_count" terminal
                           "invalid_event_ids"
                           (coerce (nreverse invalid) 'vector)
                           "rejected_result_count"
                           (gethash "rejected_count" watermark)
                           "source_rejected_count"
                           (gethash "source_rejected_count" watermark)
                           "lifecycles" lifecycles)))
                (%sqlite-exec handle "COMMIT" :lifecycle-project)
                projection))
          (error (condition)
            (ignore-errors (%sqlite-exec handle "ROLLBACK" :lifecycle-project))
            (error condition)))))))
