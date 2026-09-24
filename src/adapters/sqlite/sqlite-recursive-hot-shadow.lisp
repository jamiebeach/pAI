;;;; Explicit, rebuildable row projection. No runtime reader uses this yet.
;;;; The v2 shadow format uses contiguous selected-row ordinals. The old v1
;;;; tables are deliberately left untouched: their rows have no completeness
;;;; proof and must be rebuilt explicitly into v2, never silently adopted.
(in-package :agent)

(export '(storage-shadow-recursive-hot-prepare
          storage-shadow-recursive-hot-apply-page
          storage-shadow-recursive-hot-read-page
          storage-shadow-recursive-hot-read-root
          storage-shadow-recursive-hot-root-has-type-p
          storage-shadow-recursive-hot-report))

(defparameter *sqlite-recursive-hot-shadow-schema*
  "CREATE TABLE IF NOT EXISTS pai_recursive_hot_shadow_v2_watermark (agent_id TEXT PRIMARY KEY, projector_revision TEXT NOT NULL, policy_revision TEXT NOT NULL, through_event_id INTEGER NOT NULL, through_position INTEGER NOT NULL, selected_count INTEGER NOT NULL, source_binding TEXT NOT NULL); CREATE TABLE IF NOT EXISTS pai_recursive_hot_shadow_v2_roots (agent_id TEXT NOT NULL, root_event_id INTEGER NOT NULL, selected_count INTEGER NOT NULL, PRIMARY KEY(agent_id,root_event_id)); CREATE TABLE IF NOT EXISTS pai_recursive_hot_shadow_v2_events (agent_id TEXT NOT NULL, storage_position INTEGER NOT NULL, selected_ordinal INTEGER NOT NULL, event_id INTEGER NOT NULL, event_type TEXT NOT NULL, root_event_id INTEGER, root_ordinal INTEGER, event_json TEXT NOT NULL, integrity_hash TEXT NOT NULL, PRIMARY KEY(agent_id,storage_position), UNIQUE(agent_id,selected_ordinal)); CREATE INDEX IF NOT EXISTS pai_recursive_hot_shadow_v2_root_idx ON pai_recursive_hot_shadow_v2_events(agent_id,root_event_id,storage_position); CREATE INDEX IF NOT EXISTS pai_recursive_hot_shadow_v2_root_type_idx ON pai_recursive_hot_shadow_v2_events(agent_id,root_event_id,event_type,storage_position)")

(defun %sqlite-recursive-hot-row-hash
    (agent-id position ordinal event-id event-type root-id root-ordinal json)
  (%storage-sha256-string-parts
   (loop for value in (list agent-id (write-to-string position)
                            (write-to-string ordinal) (write-to-string event-id)
                            event-type
                            (if root-id (write-to-string root-id) "null")
                            (if root-ordinal (write-to-string root-ordinal) "null") json)
         append (list (write-to-string (length value)) ":" value))))

(defun storage-shadow-recursive-hot-prepare (derived)
  "Install the additive shadow schema explicitly, never at source load."
  (check-type derived sqlite-derived-storage)
  (bt:with-lock-held ((%sqlite-derived-lock derived))
    (%sqlite-derived-in-transaction
     derived :recursive-hot-prepare
     (lambda (handle)
       (%sqlite-exec handle *sqlite-recursive-hot-shadow-schema*
                     :recursive-hot-prepare))))
  t)

(defun %sqlite-recursive-hot-watermark (handle agent-id)
  (%with-sqlite-statement
      (s handle "SELECT projector_revision,policy_revision,through_event_id,through_position,selected_count,source_binding FROM pai_recursive_hot_shadow_v2_watermark WHERE agent_id=?1" :recursive-hot-watermark)
    (%sqlite-bind-text handle s 1 agent-id :recursive-hot-watermark)
    (let ((code (%sqlite-step-raw s)))
      (cond ((= code +sqlite-done+) nil)
            ((= code +sqlite-row+)
             (%storage-object
              "projector_revision" (%sqlite-column-text s 0)
              "policy_revision" (%sqlite-column-text s 1)
              "through_event_id" (%sqlite-column-int64 s 2)
              "through_position" (%sqlite-column-int64 s 3)
              "selected_count" (%sqlite-column-int64 s 4)
              "source_binding" (%sqlite-column-text s 5)))
            (t (%sqlite-check code handle :recursive-hot-watermark))))))

(defun %sqlite-recursive-hot-root-count (handle agent-id root-id)
  (%with-sqlite-statement
      (s handle "SELECT selected_count FROM pai_recursive_hot_shadow_v2_roots WHERE agent_id=?1 AND root_event_id=?2" :recursive-hot-root-count)
    (%sqlite-bind-text handle s 1 agent-id :recursive-hot-root-count)
    (%sqlite-bind-int64 handle s 2 root-id :recursive-hot-root-count)
    (let ((code (%sqlite-step-raw s)))
      (cond ((= code +sqlite-done+) 0)
            ((= code +sqlite-row+) (%sqlite-column-int64 s 0))
            (t (%sqlite-check code handle :recursive-hot-root-count))))))

(defun %sqlite-recursive-hot-check-watermark
    (watermark source agent-id projector-revision policy-revision)
  (when watermark
    (unless (and (string= projector-revision
                           (gethash "projector_revision" watermark))
                 (string= policy-revision (gethash "policy_revision" watermark))
                 (string= (gethash "source_binding" watermark)
                          (storage-checkpoint-source-binding
                           source :agent-id agent-id
                           :through-event-id (gethash "through_event_id" watermark)
                           :through-position (gethash "through_position" watermark))))
      (error 'storage-conflict-error :operation :recursive-hot-shadow
             :detail "shadow revision or source binding mismatch")))
  watermark)

(defun storage-shadow-recursive-hot-report
    (derived source &key (agent-id "default") projector-revision policy-revision)
  "Return a source-bound watermark. Reads check selected-row continuity. Not live-ready."
  (%storage-required-string agent-id "agent-id")
  (%storage-required-string projector-revision "projector-revision")
  (%storage-required-string policy-revision "policy-revision")
  (check-type derived sqlite-derived-storage)
  (let ((watermark
          (bt:with-lock-held ((%sqlite-derived-lock derived))
            (%sqlite-recursive-hot-watermark
             (%sqlite-derived-handle derived :recursive-hot-report) agent-id))))
    (%sqlite-recursive-hot-check-watermark
     watermark source agent-id projector-revision policy-revision)))

(defun storage-shadow-recursive-hot-apply-page
    (derived source selector
     &key (agent-id "default") projector-revision policy-revision
          (limit 128) (maximum-event-bytes 1048576) event-types)
  "Apply at most LIMIT authority events. SELECTOR receives EVENT and POSITION,
returning a compacted event (or NIL) and optional integer root id. It must not
call SOURCE recursively. EVENT-TYPES optionally excludes unrelated bodies in
SQL; a captured authority frontier still advances across filtered gaps. This
is deliberately not a live recursive projector."
  (check-type derived sqlite-derived-storage)
  (unless (functionp selector)
    (error 'storage-error :operation :recursive-hot-apply :detail "selector required"))
  (%storage-required-string agent-id "agent-id")
  (%storage-required-string projector-revision "projector-revision")
  (%storage-required-string policy-revision "policy-revision")
  (unless (and (integerp limit) (<= 1 limit 512)
               (integerp maximum-event-bytes)
               (<= 1 maximum-event-bytes 8388608))
    (error 'storage-error :operation :recursive-hot-apply :detail "invalid page bounds"))
  (when event-types
    (unless (and (or (listp event-types)
                     (and (vectorp event-types) (not (stringp event-types))))
                 (<= 1 (length event-types) 128)
                 (every (lambda (type)
                          (and (stringp type) (plusp (length type))))
                        event-types))
      (error 'storage-error :operation :recursive-hot-apply
             :detail "event-types must be one to 128 non-empty strings")))
  (let* ((prior (storage-shadow-recursive-hot-report
                 derived source :agent-id agent-id
                 :projector-revision projector-revision
                 :policy-revision policy-revision))
         (boundary (when event-types
                     (storage-authority-boundary source :agent-id agent-id)))
         (after (if prior (gethash "through_position" prior) 0))
         (last-id (if prior (gethash "through_event_id" prior) 0))
         (last-position after) (rows nil) (page-bytes 0))
    ;; Each mapped event is integrity-checked by the source adapter before the
    ;; selector sees it. Never accumulate more than one bounded page.
    (multiple-value-bind (complete-p ignored count)
        (storage-map-events
         source
         (lambda (event position)
           (multiple-value-bind (projected root-id)
               (funcall selector event position)
             (when projected
               (unless (and (hash-table-p projected)
                            (eql (gethash "id" event) (gethash "id" projected))
                            (equal (gethash "type" event) (gethash "type" projected))
                            (or (null root-id)
                                (and (integerp root-id) (plusp root-id))))
                 (error 'storage-error :operation :recursive-hot-apply
                        :detail "selector changed event identity or root is invalid"))
               (let* ((json (%storage-json projected))
                      (bytes (length (babel:string-to-octets json :encoding :utf-8))))
                 (when (or (> bytes maximum-event-bytes)
                           (> (incf page-bytes bytes) 16777216))
                   (error 'storage-error :operation :recursive-hot-apply
                          :detail "selected page exceeds byte bound"))
                 (push (list position (gethash "id" event)
                             (gethash "type" event) root-id json) rows))))
           (setf last-id (gethash "id" event) last-position position))
         :agent-id agent-id :after-position after :limit limit
         :through-position (and boundary
                                (gethash "through_storage_position" boundary))
         :event-types event-types)
      (declare (ignore ignored))
      (unless complete-p
        (error 'storage-error :operation :recursive-hot-apply
               :detail "authority page incomplete"))
      ;; If fewer than LIMIT selected rows were found, the captured source
      ;; frontier proves the intervening physical gap contains no selected
      ;; type. Advance the cursor without decoding those unrelated bodies.
      (when (and boundary (< count limit))
        (setf last-id (gethash "through_event_id" boundary)
              last-position (gethash "through_storage_position" boundary)))
      (when (and prior (= last-position after))
        (return-from storage-shadow-recursive-hot-apply-page prior)))
    (let ((binding (storage-checkpoint-source-binding
                    source :agent-id agent-id :through-event-id last-id
                    :through-position last-position)))
      (bt:with-lock-held ((%sqlite-derived-lock derived))
        (%sqlite-derived-in-transaction
         derived :recursive-hot-apply
         (lambda (handle)
           (let ((current (%sqlite-recursive-hot-watermark handle agent-id)))
             ;; A concurrent writer cannot commit a stale page over a newer
             ;; watermark. SQLite rollback covers both rows and watermark.
             (unless (and (eql after (if current
                                         (gethash "through_position" current) 0))
                          (or (null current)
                              (and (string= projector-revision
                                            (gethash "projector_revision" current))
                                   (string= policy-revision
                                            (gethash "policy_revision" current))
                                   (string= (gethash "source_binding" prior)
                                            (gethash "source_binding" current)))))
               (error 'storage-conflict-error :operation :recursive-hot-apply
                      :detail "shadow watermark changed during page read"))
             (let ((selected-count (if current (gethash "selected_count" current) 0)))
               (%with-sqlite-statement
                   (s handle "INSERT INTO pai_recursive_hot_shadow_v2_events(agent_id,storage_position,selected_ordinal,event_id,event_type,root_event_id,root_ordinal,event_json,integrity_hash) VALUES(?1,?2,?3,?4,?5,?6,?7,?8,?9)" :recursive-hot-apply)
                 (dolist (row (nreverse rows))
                   (%sqlite-reset-raw s)
                   (%sqlite-clear-bindings-raw s)
                   (let* ((root-id (fourth row))
                          (root-ordinal (when root-id
                                          (1+ (%sqlite-recursive-hot-root-count
                                               handle agent-id root-id))))
                          (ordinal (incf selected-count)))
                     (%sqlite-bind-text handle s 1 agent-id :recursive-hot-apply)
                     (%sqlite-bind-int64 handle s 2 (first row) :recursive-hot-apply)
                     (%sqlite-bind-int64 handle s 3 ordinal :recursive-hot-apply)
                     (%sqlite-bind-int64 handle s 4 (second row) :recursive-hot-apply)
                     (%sqlite-bind-text handle s 5 (third row) :recursive-hot-apply)
                     (when root-id
                       (%sqlite-bind-int64 handle s 6 root-id :recursive-hot-apply)
                       (%sqlite-bind-int64 handle s 7 root-ordinal :recursive-hot-apply))
                     (%sqlite-bind-text handle s 8 (fifth row) :recursive-hot-apply)
                     (%sqlite-bind-text
                      handle s 9
                      (%sqlite-recursive-hot-row-hash
                       agent-id (first row) ordinal (second row) (third row)
                       root-id root-ordinal (fifth row))
                      :recursive-hot-apply)
                     (%sqlite-step handle s :recursive-hot-apply +sqlite-done+)
                     (when root-id
                       (%with-sqlite-statement
                           (r handle "INSERT INTO pai_recursive_hot_shadow_v2_roots(agent_id,root_event_id,selected_count) VALUES(?1,?2,?3) ON CONFLICT(agent_id,root_event_id) DO UPDATE SET selected_count=excluded.selected_count" :recursive-hot-apply)
                         (%sqlite-bind-text handle r 1 agent-id :recursive-hot-apply)
                         (%sqlite-bind-int64 handle r 2 root-id :recursive-hot-apply)
                         (%sqlite-bind-int64 handle r 3 root-ordinal :recursive-hot-apply)
                         (%sqlite-step handle r :recursive-hot-apply +sqlite-done+))))))
               (%with-sqlite-statement
                 (s handle "INSERT INTO pai_recursive_hot_shadow_v2_watermark(agent_id,projector_revision,policy_revision,through_event_id,through_position,selected_count,source_binding) VALUES(?1,?2,?3,?4,?5,?6,?7) ON CONFLICT(agent_id) DO UPDATE SET through_event_id=excluded.through_event_id,through_position=excluded.through_position,selected_count=excluded.selected_count,source_binding=excluded.source_binding" :recursive-hot-apply)
               (loop for value in (list agent-id projector-revision
                                        policy-revision) for index from 1 do
                 (%sqlite-bind-text handle s index value :recursive-hot-apply))
               (%sqlite-bind-int64 handle s 4 last-id :recursive-hot-apply)
               (%sqlite-bind-int64 handle s 5 last-position :recursive-hot-apply)
               (%sqlite-bind-int64 handle s 6 selected-count :recursive-hot-apply)
               (%sqlite-bind-text handle s 7 binding :recursive-hot-apply)
               (%sqlite-step handle s :recursive-hot-apply +sqlite-done+)))))))
      (storage-shadow-recursive-hot-report
       derived source :agent-id agent-id :projector-revision projector-revision
       :policy-revision policy-revision))))

(defun %sqlite-recursive-hot-read
    (derived source agent-id projector-revision policy-revision
     after-position limit &optional root-id)
  (bt:with-lock-held ((%sqlite-derived-lock derived))
    (let ((handle (%sqlite-derived-handle derived :recursive-hot-read))
          (watermark nil) (rows nil) (total 0) (previous 0))
      ;; Pin the watermark, root count, and selected rows to one SQLite
      ;; snapshot. A second process may be advancing the same projection.
      (%sqlite-exec handle "BEGIN" :recursive-hot-read)
      (handler-case
          (progn
            (setf watermark (%sqlite-recursive-hot-watermark handle agent-id))
            (unless watermark
              (error 'storage-conflict-error :operation :recursive-hot-read
                     :detail "shadow projection has no sealed watermark"))
            (%sqlite-recursive-hot-check-watermark
             watermark source agent-id projector-revision policy-revision)
            (setf total (if root-id
                            (%sqlite-recursive-hot-root-count
                             handle agent-id root-id)
                            (gethash "selected_count" watermark)))
        (%with-sqlite-statement
            (p handle (if root-id
                          "SELECT root_ordinal FROM pai_recursive_hot_shadow_v2_events INDEXED BY pai_recursive_hot_shadow_v2_root_idx WHERE agent_id=?1 AND root_event_id=?2 AND storage_position<=?3 ORDER BY storage_position DESC LIMIT 1"
                          "SELECT selected_ordinal FROM pai_recursive_hot_shadow_v2_events WHERE agent_id=?1 AND storage_position<=?2 ORDER BY storage_position DESC LIMIT 1")
               :recursive-hot-read)
          (%sqlite-bind-text handle p 1 agent-id :recursive-hot-read)
          (when root-id (%sqlite-bind-int64 handle p 2 root-id :recursive-hot-read))
          (%sqlite-bind-int64 handle p (if root-id 3 2) after-position :recursive-hot-read)
          (let ((code (%sqlite-step-raw p)))
            (cond ((= code +sqlite-row+)
                   (setf previous (%sqlite-column-int64 p 0)))
                  ((/= code +sqlite-done+)
                   (%sqlite-check code handle :recursive-hot-read)))))
        (%with-sqlite-statement
          (s handle (if root-id
                          "SELECT storage_position,selected_ordinal,event_id,event_type,root_event_id,root_ordinal,event_json,integrity_hash FROM pai_recursive_hot_shadow_v2_events INDEXED BY pai_recursive_hot_shadow_v2_root_idx WHERE agent_id=?1 AND root_event_id=?2 AND storage_position>?3 AND storage_position<=?4 ORDER BY storage_position LIMIT ?5"
                          "SELECT storage_position,selected_ordinal,event_id,event_type,root_event_id,root_ordinal,event_json,integrity_hash FROM pai_recursive_hot_shadow_v2_events WHERE agent_id=?1 AND storage_position>?2 AND storage_position<=?3 ORDER BY storage_position LIMIT ?4")
               :recursive-hot-read)
          (%sqlite-bind-text handle s 1 agent-id :recursive-hot-read)
          (when root-id (%sqlite-bind-int64 handle s 2 root-id :recursive-hot-read))
          (let ((offset (if root-id 1 0)))
            (%sqlite-bind-int64 handle s (+ 2 offset) after-position :recursive-hot-read)
            (%sqlite-bind-int64 handle s (+ 3 offset)
                                (gethash "through_position" watermark) :recursive-hot-read)
            (%sqlite-bind-int64 handle s (+ 4 offset) limit :recursive-hot-read))
          (loop with bytes = 0
                for code = (%sqlite-step-raw s) while (= code +sqlite-row+) do
            (when (or (> (%sqlite-column-bytes-raw s 6) 8388608)
                      (> (incf bytes (%sqlite-column-bytes-raw s 6)) 16777216))
              (error 'storage-error :operation :recursive-hot-read
                     :detail "shadow read exceeds byte bound"))
            (let* ((position (%sqlite-column-int64 s 0))
                   (ordinal (%sqlite-column-int64 s 1))
                   (event-id (%sqlite-column-int64 s 2))
                   (event-type (%sqlite-column-text s 3))
                   (root (%sqlite-column-text s 4))
                   (row-root-id (and root (%sqlite-column-int64 s 4)))
                   (root-ordinal (and root (%sqlite-column-int64 s 5)))
                   (json (%sqlite-column-text s 6))
                   (hash (%sqlite-column-text s 7)))
              (unless (and (= (if root-id root-ordinal ordinal)
                              (1+ previous))
                           (<= (if root-id root-ordinal ordinal) total))
                (error 'storage-integrity-error :operation :recursive-hot-read
                       :detail "selected-row ordinal gap"))
              (setf previous (if root-id root-ordinal ordinal))
              (unless (string= hash (%sqlite-recursive-hot-row-hash
                                     agent-id position ordinal event-id event-type
                                     row-root-id root-ordinal json))
                (error 'storage-integrity-error :operation :recursive-hot-read
                       :detail "shadow row integrity mismatch"))
              (let ((event (%storage-json-read json :recursive-hot-read)))
                (unless (and (eql event-id (gethash "id" event))
                             (equal event-type (gethash "type" event)))
                  (error 'storage-integrity-error :operation :recursive-hot-read
                         :detail "shadow row identity mismatch"))
                (push (cons position event) rows)))
            finally (unless (= code +sqlite-done+)
                      (%sqlite-check code handle :recursive-hot-read))))
        (when (and (< (length rows) limit) (< previous total))
          (error 'storage-integrity-error :operation :recursive-hot-read
                 :detail "selected-row tail missing"))
            (%sqlite-exec handle "COMMIT" :recursive-hot-read)
            (values (nreverse rows) watermark))
        (error (condition)
          (ignore-errors (%sqlite-exec handle "ROLLBACK" :recursive-hot-read))
          (error condition))))))

(defun storage-shadow-recursive-hot-read-page
    (derived source &key (agent-id "default") projector-revision policy-revision
                       (after-position 0) (limit 128))
  (%storage-required-string agent-id "agent-id")
  (%storage-required-string projector-revision "projector-revision")
  (%storage-required-string policy-revision "policy-revision")
  (%storage-positive-integer after-position "after-position" :zero-allowed t)
  (unless (and (integerp limit) (<= 1 limit 512))
    (error 'storage-error :operation :recursive-hot-read :detail "invalid page limit"))
  (%sqlite-recursive-hot-read derived source agent-id projector-revision
                              policy-revision after-position limit))

(defun storage-shadow-recursive-hot-read-root
    (derived source root-id &key (agent-id "default") projector-revision
                            policy-revision (after-position 0) (limit 128))
  (%storage-positive-integer root-id "root-id")
  (%storage-required-string agent-id "agent-id")
  (%storage-required-string projector-revision "projector-revision")
  (%storage-required-string policy-revision "policy-revision")
  (%storage-positive-integer after-position "after-position" :zero-allowed t)
  (unless (and (integerp limit) (<= 1 limit 512))
    (error 'storage-error :operation :recursive-hot-read :detail "invalid page limit"))
  (%sqlite-recursive-hot-read derived source agent-id projector-revision
                              policy-revision after-position limit root-id))

(defun storage-shadow-recursive-hot-root-has-type-p
    (derived source root-id event-types
     &key (agent-id "default") projector-revision policy-revision)
  "Test one or at most 16 event types in a sealed root using indexed lookups.
Returns a second value, the checked watermark. A negative answer cannot prove
absence: deletion of the sole matching selected row can look like a normal
event-type gap. This is shadow-only and must not decide live settlement or
protection until a separate completeness proof exists."
  (check-type derived sqlite-derived-storage)
  (%storage-positive-integer root-id "root-id")
  (%storage-required-string agent-id "agent-id")
  (%storage-required-string projector-revision "projector-revision")
  (%storage-required-string policy-revision "policy-revision")
  (let ((types (if (stringp event-types) (list event-types) event-types)))
    (unless (and (listp types) (<= 1 (length types) 16)
                 (every (lambda (type)
                          (and (stringp type) (<= 1 (length type) 128)
                               (not (every (lambda (char)
                                             (find char '(#\Space #\Tab #\Newline)))
                                           type))))
                        types))
      (error 'storage-error :operation :recursive-hot-root-has-type
             :detail "invalid event type bounds"))
    (let ((watermark (storage-shadow-recursive-hot-report
                      derived source :agent-id agent-id
                      :projector-revision projector-revision
                      :policy-revision policy-revision)))
      (unless watermark
        (error 'storage-conflict-error :operation :recursive-hot-root-has-type
               :detail "shadow projection has no sealed watermark"))
      (bt:with-lock-held ((%sqlite-derived-lock derived))
        (let* ((handle (%sqlite-derived-handle derived :recursive-hot-root-has-type))
               (current (%sqlite-recursive-hot-watermark handle agent-id))
               (root-count (%sqlite-recursive-hot-root-count handle agent-id root-id)))
          (unless (and current
                       (= (gethash "through_position" watermark)
                          (gethash "through_position" current))
                       (= (gethash "selected_count" watermark)
                          (gethash "selected_count" current))
                       (string= (gethash "source_binding" watermark)
                                (gethash "source_binding" current))
                       (string= projector-revision
                                (gethash "projector_revision" current))
                       (string= policy-revision
                                (gethash "policy_revision" current)))
            (error 'storage-conflict-error :operation :recursive-hot-root-has-type
                   :detail "shadow watermark changed during presence read"))
          (when (plusp root-count)
            (%with-sqlite-statement
                (s handle "SELECT storage_position,selected_ordinal,event_id,event_type,root_ordinal,event_json,integrity_hash FROM pai_recursive_hot_shadow_v2_events INDEXED BY pai_recursive_hot_shadow_v2_root_type_idx WHERE agent_id=?1 AND root_event_id=?2 AND event_type=?3 AND storage_position<=?4 ORDER BY storage_position LIMIT 1" :recursive-hot-root-has-type)
              (%sqlite-bind-text handle s 1 agent-id :recursive-hot-root-has-type)
              (%sqlite-bind-int64 handle s 2 root-id :recursive-hot-root-has-type)
              (%sqlite-bind-int64 handle s 4
                                  (gethash "through_position" watermark)
                                  :recursive-hot-root-has-type)
              (dolist (type types)
                (%sqlite-reset-raw s)
                (%sqlite-bind-text handle s 3 type :recursive-hot-root-has-type)
                (let ((code (%sqlite-step-raw s)))
                  (cond
                    ((= code +sqlite-row+)
                     (when (> (%sqlite-column-bytes-raw s 5) 8388608)
                       (error 'storage-error :operation :recursive-hot-root-has-type
                              :detail "shadow row exceeds byte bound"))
                     (let* ((position (%sqlite-column-int64 s 0))
                            (ordinal (%sqlite-column-int64 s 1))
                            (event-id (%sqlite-column-int64 s 2))
                            (event-type (%sqlite-column-text s 3))
                            (root-ordinal (%sqlite-column-int64 s 4))
                            (json (%sqlite-column-text s 5))
                            (hash (%sqlite-column-text s 6))
                            (event (%storage-json-read json :recursive-hot-root-has-type)))
                       (unless (and (<= 1 root-ordinal root-count)
                                    (<= 1 ordinal
                                        (gethash "selected_count" watermark))
                                    (string= type event-type)
                                    (string= hash
                                             (%sqlite-recursive-hot-row-hash
                                              agent-id position ordinal event-id
                                              event-type root-id root-ordinal json))
                                    (eql event-id (gethash "id" event))
                                    (equal event-type (gethash "type" event)))
                         (error 'storage-integrity-error
                                :operation :recursive-hot-root-has-type
                                :detail "shadow presence row integrity mismatch"))
                       (return-from storage-shadow-recursive-hot-root-has-type-p
                         (values t watermark))))
                    ((/= code +sqlite-done+)
                     (%sqlite-check code handle :recursive-hot-root-has-type))))))))
      (values nil watermark)))))
