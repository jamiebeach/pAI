;;;; Rebuildable, source-bound scalar state for committed conscious pulses.
;;;; Explicit preparation and bounded application; no load-time database work.
(in-package :agent)

(export '(storage-shadow-conscious-pulse-prepare
          storage-shadow-conscious-pulse-apply-page
          storage-shadow-conscious-pulse-report))

(defparameter *conscious-pulse-row-revision* "conscious-pulse-scalar-v1")

(defun %sqlite-conscious-pulse-row-hash
    (agent-id revision event-id position maximum binding)
  (%storage-sha256-string-parts
   (loop for value in (list agent-id revision (write-to-string event-id)
                            (write-to-string position)
                            (write-to-string maximum) binding)
         append (list (write-to-string (length value)) ":" value))))

(defun storage-shadow-conscious-pulse-prepare (derived)
  (check-type derived sqlite-derived-storage)
  (bt:with-lock-held ((%sqlite-derived-lock derived))
    (%sqlite-derived-in-transaction
     derived :conscious-pulse-prepare
     (lambda (handle)
       (%sqlite-exec
        handle
        "CREATE TABLE IF NOT EXISTS pai_conscious_pulse_v1 (agent_id TEXT PRIMARY KEY, projector_revision TEXT NOT NULL, through_event_id INTEGER NOT NULL, through_position INTEGER NOT NULL, max_sequence INTEGER NOT NULL, source_binding TEXT NOT NULL, integrity_hash TEXT NOT NULL)"
        :conscious-pulse-prepare))))
  t)

(defun %sqlite-conscious-pulse-row (handle agent-id)
  (%with-sqlite-statement
      (s handle "SELECT projector_revision,through_event_id,through_position,max_sequence,source_binding,integrity_hash FROM pai_conscious_pulse_v1 WHERE agent_id=?1"
         :conscious-pulse-row)
    (%sqlite-bind-text handle s 1 agent-id :conscious-pulse-row)
    (let ((code (%sqlite-step-raw s)))
      (cond ((= code +sqlite-done+) nil)
            ((= code +sqlite-row+)
             (obj "projector_revision" (%sqlite-column-text s 0)
                  "through_event_id" (%sqlite-column-int64 s 1)
                  "through_position" (%sqlite-column-int64 s 2)
                  "max_sequence" (%sqlite-column-int64 s 3)
                  "source_binding" (%sqlite-column-text s 4)
                  "integrity_hash" (%sqlite-column-text s 5)))
            (t (%sqlite-check code handle :conscious-pulse-row))))))

(defun storage-shadow-conscious-pulse-report
    (derived source &key (agent-id "default"))
  "Return a verified scalar row, or NIL before explicit preparation/build."
  (check-type derived sqlite-derived-storage)
  (%storage-required-string agent-id "agent-id")
  (let ((row
          (bt:with-lock-held ((%sqlite-derived-lock derived))
            (%sqlite-conscious-pulse-row
             (%sqlite-derived-handle derived :conscious-pulse-report)
             agent-id))))
    (when row
      (unless (and (string= *conscious-pulse-row-revision*
                           (gethash "projector_revision" row))
                   (<= 0 (gethash "max_sequence" row))
                   (string= (gethash "integrity_hash" row)
                            (%sqlite-conscious-pulse-row-hash
                             agent-id (gethash "projector_revision" row)
                             (gethash "through_event_id" row)
                             (gethash "through_position" row)
                             (gethash "max_sequence" row)
                             (gethash "source_binding" row)))
                   (string= (gethash "source_binding" row)
                            (storage-checkpoint-source-binding
                             source :agent-id agent-id
                             :through-event-id
                             (gethash "through_event_id" row)
                             :through-position
                             (gethash "through_position" row))))
        (error 'storage-conflict-error :operation :conscious-pulse-report
               :detail "pulse row revision or source binding mismatch")))
    row))

(defun storage-shadow-conscious-pulse-apply-page
    (derived source &key (agent-id "default") (limit 128))
  "Apply one bounded filtered authority page to a scalar derived row.
The physical cursor advances across filtered gaps only at a captured source
frontier. A concurrent writer cannot overwrite a newer cursor."
  (check-type derived sqlite-derived-storage)
  (%storage-required-string agent-id "agent-id")
  (unless (and (integerp limit) (<= 1 limit 512))
    (error 'storage-error :operation :conscious-pulse-apply
           :detail "invalid page limit"))
  (let* ((prior (storage-shadow-conscious-pulse-report
                 derived source :agent-id agent-id))
         (boundary (storage-authority-boundary source :agent-id agent-id))
         (after (if prior (gethash "through_position" prior) 0))
         (last-id (if prior (gethash "through_event_id" prior) 0))
         (last-position after)
         (maximum (if prior (gethash "max_sequence" prior) 0)))
    (multiple-value-bind (complete-p ignored count)
        (storage-map-events
         source
         (lambda (event position)
           (let* ((payload (gethash "payload" event))
                  (sequence (and (hash-table-p payload)
                                 (gethash "pulse_sequence" payload))))
             (when (and (integerp sequence) (plusp sequence))
               (setf maximum (max maximum sequence))))
           (setf last-id (gethash "id" event)
                 last-position position))
         :agent-id agent-id :after-position after
         :through-position (gethash "through_storage_position" boundary)
         :event-types '("pulse-committed") :limit limit)
      (declare (ignore ignored))
      (unless complete-p
        (error 'storage-error :operation :conscious-pulse-apply
               :detail "authority page incomplete"))
      (when (< count limit)
        (setf last-id (gethash "through_event_id" boundary)
              last-position (gethash "through_storage_position" boundary)))
      (when (and prior (= last-position after))
        (return-from storage-shadow-conscious-pulse-apply-page prior)))
    (let ((binding
            (storage-checkpoint-source-binding
             source :agent-id agent-id :through-event-id last-id
             :through-position last-position)))
      (unless (stringp binding)
        (error 'storage-conflict-error :operation :conscious-pulse-apply
               :detail "source frontier is not bindable"))
      (bt:with-lock-held ((%sqlite-derived-lock derived))
        (%sqlite-derived-in-transaction
         derived :conscious-pulse-apply
         (lambda (handle)
           (let ((current (%sqlite-conscious-pulse-row handle agent-id)))
             (unless (and (eql after (if current
                                        (gethash "through_position" current)
                                        0))
                          (or (null current)
                              (and prior
                                   (string= (gethash "source_binding" prior)
                                            (gethash "source_binding" current))
                                   (string= *conscious-pulse-row-revision*
                                            (gethash "projector_revision"
                                                     current)))))
               (error 'storage-conflict-error :operation :conscious-pulse-apply
                      :detail "pulse row changed during page read"))
             (%with-sqlite-statement
                 (s handle "INSERT INTO pai_conscious_pulse_v1(agent_id,projector_revision,through_event_id,through_position,max_sequence,source_binding,integrity_hash) VALUES(?1,?2,?3,?4,?5,?6,?7) ON CONFLICT(agent_id) DO UPDATE SET through_event_id=excluded.through_event_id,through_position=excluded.through_position,max_sequence=excluded.max_sequence,source_binding=excluded.source_binding,integrity_hash=excluded.integrity_hash"
                    :conscious-pulse-apply)
               (%sqlite-bind-text handle s 1 agent-id :conscious-pulse-apply)
               (%sqlite-bind-text handle s 2 *conscious-pulse-row-revision*
                                  :conscious-pulse-apply)
               (%sqlite-bind-int64 handle s 3 last-id :conscious-pulse-apply)
               (%sqlite-bind-int64 handle s 4 last-position
                                   :conscious-pulse-apply)
               (%sqlite-bind-int64 handle s 5 maximum :conscious-pulse-apply)
               (%sqlite-bind-text handle s 6 binding :conscious-pulse-apply)
               (%sqlite-bind-text
                handle s 7
                (%sqlite-conscious-pulse-row-hash
                 agent-id *conscious-pulse-row-revision* last-id
                 last-position maximum binding)
                :conscious-pulse-apply)
               (%sqlite-step handle s :conscious-pulse-apply +sqlite-done+))))))
      (storage-shadow-conscious-pulse-report
       derived source :agent-id agent-id))))
