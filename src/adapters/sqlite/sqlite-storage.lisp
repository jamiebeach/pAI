;;;; sqlite-storage.lisp -- local event/checkpoint storage adapter.

(in-package :agent)

(export '(make-sqlite-storage make-sqlite-storage-read-only
          sqlite-storage-path))

(defconstant +sqlite-ok+ 0)
(defconstant +sqlite-row+ 100)
(defconstant +sqlite-done+ 101)
(defconstant +sqlite-open-readonly+ #x00000001)
(defconstant +sqlite-open-readwrite+ #x00000002)
(defconstant +sqlite-open-create+ #x00000004)
(defconstant +sqlite-open-uri+ #x00000040)
(defconstant +sqlite-open-fullmutex+ #x00010000)

(defvar *sqlite-library-loaded-p* nil)
(defvar *sqlite-library-lock* (bt:make-lock "sqlite-library"))

;; Declarations do not load the library or open a database. Resolution occurs
;; on first call, after %SQLITE-LOAD-LIBRARY has selected a library.
(cffi:defcfun ("sqlite3_open_v2" %sqlite-open-v2) :int
  (filename :string) (database :pointer) (flags :int) (vfs :pointer))
(cffi:defcfun ("sqlite3_close_v2" %sqlite-close-v2) :int (database :pointer))
(cffi:defcfun ("sqlite3_errmsg" %sqlite-errmsg) :pointer (database :pointer))
(cffi:defcfun ("sqlite3_exec" %sqlite-exec-raw) :int
  (database :pointer) (sql :string) (callback :pointer)
  (argument :pointer) (error-message :pointer))
(cffi:defcfun ("sqlite3_free" %sqlite-free) :void (pointer :pointer))
(cffi:defcfun ("sqlite3_prepare_v2" %sqlite-prepare-v2) :int
  (database :pointer) (sql :string) (bytes :int)
  (statement :pointer) (tail :pointer))
(cffi:defcfun ("sqlite3_finalize" %sqlite-finalize) :int (statement :pointer))
(cffi:defcfun ("sqlite3_bind_text" %sqlite-bind-text-raw) :int
  (statement :pointer) (index :int) (value :string) (bytes :int)
  (destructor :pointer))
(cffi:defcfun ("sqlite3_bind_int64" %sqlite-bind-int64-raw) :int
  (statement :pointer) (index :int) (value :int64))
(cffi:defcfun ("sqlite3_bind_blob" %sqlite-bind-blob-raw) :int
  (statement :pointer) (index :int) (value :pointer) (bytes :int)
  (destructor :pointer))
(cffi:defcfun ("sqlite3_step" %sqlite-step-raw) :int (statement :pointer))
(cffi:defcfun ("sqlite3_reset" %sqlite-reset-raw) :int (statement :pointer))
(cffi:defcfun ("sqlite3_clear_bindings" %sqlite-clear-bindings-raw) :int
  (statement :pointer))
(cffi:defcfun ("sqlite3_column_text" %sqlite-column-text-raw) :pointer
  (statement :pointer) (index :int))
(cffi:defcfun ("sqlite3_column_int64" %sqlite-column-int64-raw) :int64
  (statement :pointer) (index :int))
(cffi:defcfun ("sqlite3_column_blob" %sqlite-column-blob-raw) :pointer
  (statement :pointer) (index :int))
(cffi:defcfun ("sqlite3_column_bytes" %sqlite-column-bytes-raw) :int
  (statement :pointer) (index :int))

(defclass sqlite-storage (storage-backend)
  ((path :initarg :path :reader sqlite-storage-path)
   (handle :initarg :handle :accessor %sqlite-storage-handle)
   (lock :initform (bt:make-lock "sqlite-storage") :reader %sqlite-storage-lock)
   (closed-p :initform nil :accessor %sqlite-storage-closed-p)))

(defun %sqlite-load-library ()
  (bt:with-lock-held (*sqlite-library-lock*)
    (unless *sqlite-library-loaded-p*
      (let ((configured (uiop:getenv "PAI_SQLITE_LIBRARY")))
        (handler-case
            (progn
              (if (and configured (plusp (length configured)))
                  (cffi:load-foreign-library configured)
                  (cffi:load-foreign-library
                   '(:or "winsqlite3.dll" "sqlite3.dll"
                         "libsqlite3.so.0" "libsqlite3.so"
                         "libsqlite3.dylib")))
              (setf *sqlite-library-loaded-p* t))
          (error (condition)
            (error 'storage-unavailable-error :operation :sqlite-load
                   :detail condition)))))))

(defun %sqlite-error-text (handle)
  (let ((pointer (%sqlite-errmsg handle)))
    (if (cffi:null-pointer-p pointer) "unknown SQLite error"
        (cffi:foreign-string-to-lisp pointer :encoding :utf-8))))

(defun %sqlite-check (code handle operation &optional (expected +sqlite-ok+))
  (unless (= code expected)
    (error 'storage-error :operation operation
           :detail (format nil "SQLite code ~d: ~a" code
                           (%sqlite-error-text handle)))))

(defun %sqlite-exec (handle sql &optional (operation :execute))
  (cffi:with-foreign-object (error-pointer :pointer)
    (setf (cffi:mem-ref error-pointer :pointer) (cffi:null-pointer))
    (let ((code (%sqlite-exec-raw handle sql (cffi:null-pointer)
                                  (cffi:null-pointer) error-pointer)))
      (unless (= code +sqlite-ok+)
        (let* ((pointer (cffi:mem-ref error-pointer :pointer))
               (message (if (cffi:null-pointer-p pointer)
                            (%sqlite-error-text handle)
                            (cffi:foreign-string-to-lisp pointer :encoding :utf-8))))
          (unless (cffi:null-pointer-p pointer)
            (%sqlite-free pointer))
          (error 'storage-error :operation operation :detail message))))))

(defun %sqlite-prepare (handle sql operation)
  (cffi:with-foreign-objects ((statement-pointer :pointer) (tail :pointer))
    (setf (cffi:mem-ref statement-pointer :pointer) (cffi:null-pointer))
    (%sqlite-check
     (%sqlite-prepare-v2 handle sql -1 statement-pointer tail)
     handle operation)
    (cffi:mem-ref statement-pointer :pointer)))

(defmacro %with-sqlite-statement ((statement handle sql operation) &body body)
  `(let ((,statement (%sqlite-prepare ,handle ,sql ,operation)))
     (unwind-protect (progn ,@body)
       (unless (cffi:null-pointer-p ,statement)
         (%sqlite-finalize ,statement)))))

(defun %sqlite-bind-text (handle statement index value operation)
  (%sqlite-check
   (%sqlite-bind-text-raw
    statement index value -1
    (cffi:make-pointer
     (1- (ash 1 (* 8 (cffi:foreign-type-size :pointer))))))
   handle operation))

(defun %sqlite-bind-int64 (handle statement index value operation)
  (%sqlite-check
   (%sqlite-bind-int64-raw statement index value)
   handle operation))

(defun %sqlite-bind-blob (handle statement index value operation)
  (unless (typep value '(simple-array (unsigned-byte 8) (*)))
    (error 'storage-error :operation operation
           :detail "SQLite blob value must be a simple octet vector"))
  (cffi:with-pointer-to-vector-data (pointer value)
    (%sqlite-check
     (%sqlite-bind-blob-raw
      statement index pointer (length value)
      (cffi:make-pointer
       (1- (ash 1 (* 8 (cffi:foreign-type-size :pointer))))))
     handle operation)))

(defun %sqlite-column-text (statement index)
  (let ((pointer (%sqlite-column-text-raw statement index)))
    (unless (cffi:null-pointer-p pointer)
      (cffi:foreign-string-to-lisp pointer :encoding :utf-8))))

(defun %sqlite-column-int64 (statement index)
  (%sqlite-column-int64-raw statement index))

(defun %sqlite-column-blob (statement index)
  (let* ((length (%sqlite-column-bytes-raw statement index))
         (pointer (%sqlite-column-blob-raw statement index))
         (result (make-array length :element-type '(unsigned-byte 8))))
    (when (and (plusp length) (cffi:null-pointer-p pointer))
      (error 'storage-integrity-error :operation :read-blob
             :detail "SQLite returned a null pointer for a non-empty blob"))
    (dotimes (position length result)
      (setf (aref result position) (cffi:mem-aref pointer :uint8 position)))))

(defun %sqlite-step (handle statement operation expected)
  (%sqlite-check (%sqlite-step-raw statement)
                 handle operation expected))

(defun %sqlite-handle (backend operation)
  (when (%sqlite-storage-closed-p backend)
    (error 'storage-error :operation operation :detail "backend is closed"))
  (%sqlite-storage-handle backend))

(defun %sqlite-in-transaction (backend operation thunk)
  (let ((handle (%sqlite-handle backend operation)))
    (%sqlite-exec handle "BEGIN IMMEDIATE" operation)
    (handler-case
        (multiple-value-prog1 (funcall thunk handle)
          (%sqlite-exec handle "COMMIT" operation))
      (error (condition)
        (ignore-errors (%sqlite-exec handle "ROLLBACK" operation))
        (error condition)))))

(defun %sqlite-initialize-schema (handle)
  ;; One agent process remains the architectural default, but a bounded wait
  ;; makes an operator read transaction or backup overlap fail less abruptly.
  (%sqlite-exec handle "PRAGMA busy_timeout=5000" :initialize)
  (%sqlite-exec handle "PRAGMA journal_mode=WAL" :initialize)
  (%sqlite-exec handle "PRAGMA foreign_keys=ON" :initialize)
  (%sqlite-exec handle "PRAGMA synchronous=FULL" :initialize)
  (%sqlite-exec
   handle
   "CREATE TABLE IF NOT EXISTS pai_storage_meta (meta_key TEXT PRIMARY KEY, meta_value TEXT NOT NULL)"
   :initialize)
  (let ((version nil))
    (%with-sqlite-statement
        (statement handle
                   "SELECT meta_value FROM pai_storage_meta WHERE meta_key='format_version'"
                   :initialize)
      (let ((code (%sqlite-step-raw statement)))
        (cond ((= code +sqlite-row+)
               (setf version (%sqlite-column-text statement 0)))
              ((/= code +sqlite-done+)
               (%sqlite-check code handle :initialize)))))
    (cond
      ((null version)
       (%sqlite-exec
        handle
        "INSERT INTO pai_storage_meta(meta_key,meta_value) VALUES('format_version','4')"
        :initialize))
      ((not (string= version "4"))
       (error 'storage-conflict-error :operation :initialize
              :detail (format nil "unsupported SQLite storage format ~a; use an explicit offline migration"
                              version)))))
  ;; Derived checkpoints must never be paired accidentally with a different
  ;; event ledger. Existing format-4 databases gain this opaque identity
  ;; without changing an event row.
  (%sqlite-exec
   handle
   (format nil
           "INSERT OR IGNORE INTO pai_storage_meta(meta_key,meta_value) VALUES('storage_id','~a')"
           (%storage-sha256
            (format nil "~d:~d:~d" (get-universal-time)
                    (get-internal-real-time)
                    (random most-positive-fixnum))))
   :initialize)
  (%sqlite-exec
   handle
   "CREATE TABLE IF NOT EXISTS pai_events (storage_sequence INTEGER PRIMARY KEY AUTOINCREMENT, event_id INTEGER NOT NULL, storage_origin TEXT NOT NULL, agent_id TEXT NOT NULL, partition_status TEXT NOT NULL, event_type TEXT NOT NULL, occurred_at TEXT NOT NULL, event_json TEXT NOT NULL, integrity_hash TEXT NOT NULL); CREATE INDEX IF NOT EXISTS pai_events_id_idx ON pai_events(event_id,storage_sequence); CREATE INDEX IF NOT EXISTS pai_events_agent_id_idx ON pai_events(agent_id,event_id,storage_sequence); CREATE INDEX IF NOT EXISTS pai_events_agent_type_id_idx ON pai_events(agent_id,event_type,event_id,storage_sequence); CREATE TABLE IF NOT EXISTS pai_projection_checkpoints (projection_name TEXT NOT NULL, agent_id TEXT NOT NULL, through_event_id INTEGER NOT NULL, through_storage_position INTEGER NOT NULL, projector_revision TEXT NOT NULL, policy_revision TEXT NOT NULL, state_json TEXT NOT NULL, integrity_hash TEXT NOT NULL, created_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP, PRIMARY KEY(projection_name,agent_id));"
   :initialize)
  (%sqlite-exec
   handle
   "CREATE INDEX IF NOT EXISTS pai_events_agent_seq_idx ON pai_events(agent_id,storage_sequence); CREATE INDEX IF NOT EXISTS pai_events_agent_type_seq_idx ON pai_events(agent_id,event_type,storage_sequence); CREATE INDEX IF NOT EXISTS pai_events_agent_time_seq_idx ON pai_events(agent_id,occurred_at,storage_sequence); CREATE INDEX IF NOT EXISTS pai_events_agent_type_time_seq_idx ON pai_events(agent_id,event_type,occurred_at,storage_sequence)"
   :initialize)
  (let ((found nil))
    (%with-sqlite-statement
        (statement handle "PRAGMA table_info(pai_events)" :initialize)
      (loop for code = (%sqlite-step-raw statement)
            while (= code +sqlite-row+)
            when (string= "storage_sequence" (%sqlite-column-text statement 1))
              do (setf found t)
            finally (unless (= code +sqlite-done+)
                      (%sqlite-check code handle :initialize))))
    (unless found
      (error 'storage-conflict-error :operation :initialize
             :detail "SQLite format metadata and event schema disagree"))))

(defun make-sqlite-storage (path)
  (%sqlite-load-library)
  (let ((namestring (namestring (merge-pathnames path))))
    (ensure-directories-exist path)
    (cffi:with-foreign-object (handle-pointer :pointer)
      (setf (cffi:mem-ref handle-pointer :pointer) (cffi:null-pointer))
      (let* ((flags (logior +sqlite-open-readwrite+ +sqlite-open-create+
                            +sqlite-open-fullmutex+))
             (code (%sqlite-open-v2 namestring handle-pointer flags
                                    (cffi:null-pointer)))
             (handle (cffi:mem-ref handle-pointer :pointer)))
        (unless (= code +sqlite-ok+)
          (let ((message (if (cffi:null-pointer-p handle)
                             (format nil "SQLite open code ~d" code)
                             (%sqlite-error-text handle))))
            (unless (cffi:null-pointer-p handle)
              (%sqlite-close-v2 handle))
            (error 'storage-error :operation :open :detail message)))
        (handler-case
            (progn
              (%sqlite-initialize-schema handle)
              (make-instance 'sqlite-storage :path namestring :handle handle))
          (error (condition)
            (%sqlite-close-v2 handle)
            (error condition)))))))

(defun make-sqlite-storage-read-only (path)
  "Open an existing event database without schema creation or write authority.

This is intended for immutable replay, audit and laboratory snapshots.  The
SQLite connection is opened READONLY and also placed in query-only mode so a
caller accidentally reaching a mutation method fails at the storage boundary."
  (%sqlite-load-library)
  (let* ((resolved (merge-pathnames path))
         (namestring (namestring resolved)))
    (unless (probe-file resolved)
      (error 'storage-error :operation :open-read-only
             :detail "SQLite replay database does not exist"))
    (cffi:with-foreign-object (handle-pointer :pointer)
      (setf (cffi:mem-ref handle-pointer :pointer) (cffi:null-pointer))
      (let* ((flags (logior +sqlite-open-readonly+ +sqlite-open-uri+
                            +sqlite-open-fullmutex+))
             (uri (format nil "file:~a?immutable=1" namestring))
             (code (%sqlite-open-v2 uri handle-pointer flags
                                    (cffi:null-pointer)))
             (handle (cffi:mem-ref handle-pointer :pointer)))
        (unless (= code +sqlite-ok+)
          (let ((message (if (cffi:null-pointer-p handle)
                             (format nil "SQLite read-only open code ~d" code)
                             (%sqlite-error-text handle))))
            (unless (cffi:null-pointer-p handle)
              (%sqlite-close-v2 handle))
            (error 'storage-error :operation :open-read-only :detail message)))
        (handler-case
            (progn
              (%sqlite-exec handle "PRAGMA query_only=ON" :open-read-only)
              (make-instance 'sqlite-storage :path namestring :handle handle))
          (error (condition)
            (%sqlite-close-v2 handle)
            (error condition)))))))

(defmethod storage-capabilities ((backend sqlite-storage))
  (declare (ignore backend))
  (%storage-object "schema_version" 1 "backend" "sqlite"
                   "event_log" t "projection_checkpoints" t
                   "exact_event_receipts" t
                   "vector_index" nil "single_authority" t))

(defmethod storage-close ((backend sqlite-storage))
  (bt:with-lock-held ((%sqlite-storage-lock backend))
    (unless (%sqlite-storage-closed-p backend)
      (let ((handle (%sqlite-storage-handle backend)))
        (%sqlite-check (%sqlite-close-v2 handle)
                       handle :close)
        (setf (%sqlite-storage-closed-p backend) t
              (%sqlite-storage-handle backend) (cffi:null-pointer)))))
  t)

(defun %sqlite-max-event-id-unlocked (handle agent-id)
  (%with-sqlite-statement
      (statement handle
                 (if agent-id
                     "SELECT COALESCE(MAX(event_id),0) FROM pai_events WHERE agent_id=?1"
                     "SELECT COALESCE(MAX(event_id),0) FROM pai_events")
                 :max-event-id)
    (when agent-id (%sqlite-bind-text handle statement 1 agent-id :max-event-id))
    (%sqlite-step handle statement :max-event-id +sqlite-row+)
    (%sqlite-column-int64 statement 0)))

(defun %sqlite-legacy-import-present-p (handle agent-id)
  (%with-sqlite-statement
      (statement handle
                 "SELECT 1 FROM pai_events WHERE agent_id=?1 AND storage_origin='legacy-import' LIMIT 1"
                 :checkpoint-legacy-prefix)
    (%sqlite-bind-text handle statement 1 agent-id :checkpoint-legacy-prefix)
    (let ((code (%sqlite-step-raw statement)))
      (cond ((= code +sqlite-row+) t)
            ((= code +sqlite-done+) nil)
            (t (%sqlite-check code handle :checkpoint-legacy-prefix))))))

(defmethod storage-max-event-id ((backend sqlite-storage) &key agent-id)
  (when agent-id (%storage-required-string agent-id "agent-id"))
  (bt:with-lock-held ((%sqlite-storage-lock backend))
    (%sqlite-max-event-id-unlocked (%sqlite-handle backend :max-event-id)
                                   agent-id)))

(defun %sqlite-head-position-unlocked (handle agent-id)
  (%with-sqlite-statement
      (statement handle
                 (if agent-id
                     "SELECT COALESCE(MAX(storage_sequence),0) FROM pai_events WHERE agent_id=?1"
                     "SELECT COALESCE(MAX(storage_sequence),0) FROM pai_events")
                 :head-position)
    (when agent-id (%sqlite-bind-text handle statement 1 agent-id :head-position))
    (%sqlite-step handle statement :head-position +sqlite-row+)
    (%sqlite-column-int64 statement 0)))

(defmethod storage-head-position ((backend sqlite-storage) &key agent-id)
  (when agent-id (%storage-required-string agent-id "agent-id"))
  (bt:with-lock-held ((%sqlite-storage-lock backend))
    (%sqlite-head-position-unlocked (%sqlite-handle backend :head-position)
                                    agent-id)))

(defun %sqlite-storage-id-unlocked (handle operation)
  (%with-sqlite-statement
      (statement handle
                 "SELECT meta_value FROM pai_storage_meta WHERE meta_key='storage_id'"
                 operation)
    (%sqlite-step handle statement operation +sqlite-row+)
    (%sqlite-column-text statement 0)))

(defun %sqlite-event-receipt
    (storage-id position event-id agent-id event-type event-json event-hash)
  (%storage-object
   "schema_version" 1 "storage_id" storage-id
   "storage_position" position "event_id" event-id
   "agent_id" agent-id "event_type" event-type
   "event_json" event-json "integrity_hash" event-hash))

(defmethod storage-checkpoint-source-binding
    ((backend sqlite-storage) &key (agent-id "default") through-event-id
                              through-position)
  (%storage-required-string agent-id "agent-id")
  (%storage-positive-integer through-event-id "through-event-id"
                             :zero-allowed t)
  (%storage-positive-integer through-position "through-position"
                             :zero-allowed t)
  (bt:with-lock-held ((%sqlite-storage-lock backend))
    (let* ((handle (%sqlite-handle backend :checkpoint-source-binding))
           (maximum (%sqlite-max-event-id-unlocked handle agent-id))
           (head (%sqlite-head-position-unlocked handle agent-id))
           (storage-id nil) (boundary-id 0) (boundary-hash "empty"))
      (when (or (> through-event-id maximum) (> through-position head))
        (error 'storage-conflict-error :operation :checkpoint-source-binding
               :detail "checkpoint binding exceeds durable event history"))
      (setf storage-id
            (%sqlite-storage-id-unlocked handle :checkpoint-source-binding))
      (when (plusp through-position)
        (%with-sqlite-statement
            (statement handle
                       "SELECT event_id,integrity_hash FROM pai_events WHERE storage_sequence=?1 AND agent_id=?2"
                       :checkpoint-source-binding)
          (%sqlite-bind-int64 handle statement 1 through-position
                              :checkpoint-source-binding)
          (%sqlite-bind-text handle statement 2 agent-id
                             :checkpoint-source-binding)
          (let ((code (%sqlite-step-raw statement)))
            (unless (= code +sqlite-row+)
              (error 'storage-integrity-error
                     :operation :checkpoint-source-binding
                     :detail "checkpoint boundary row is absent"))
            (setf boundary-id (%sqlite-column-int64 statement 0)
                  boundary-hash (%sqlite-column-text statement 1)))))
      (%storage-sha256
       (%storage-checkpoint-integrity-input
       "event-source" agent-id through-event-id through-position
        storage-id (write-to-string boundary-id) boundary-hash)))))

(defmethod storage-authority-boundary
    ((backend sqlite-storage) &key (agent-id "default"))
  (%storage-required-string agent-id "agent-id")
  (bt:with-lock-held ((%sqlite-storage-lock backend))
    (let* ((handle (%sqlite-handle backend :authority-boundary))
           (storage-id
             (%sqlite-storage-id-unlocked handle :authority-boundary))
           (position (%sqlite-head-position-unlocked handle agent-id))
           (event-id 0)
           (event-hash "empty"))
      (when (plusp position)
        (%with-sqlite-statement
            (statement handle
                       "SELECT event_id,integrity_hash FROM pai_events WHERE storage_sequence=?1 AND agent_id=?2"
                       :authority-boundary)
          (%sqlite-bind-int64 handle statement 1 position :authority-boundary)
          (%sqlite-bind-text handle statement 2 agent-id :authority-boundary)
          (let ((code (%sqlite-step-raw statement)))
            (unless (= code +sqlite-row+)
              (error 'storage-integrity-error :operation :authority-boundary
                     :detail "authority boundary row is absent"))
            (setf event-id (%sqlite-column-int64 statement 0)
                  event-hash (%sqlite-column-text statement 1)))))
      (let ((binding
              (%storage-sha256
               (%storage-checkpoint-integrity-input
                "event-source" agent-id event-id position storage-id
                (write-to-string event-id) event-hash))))
        (%storage-object
         "schema_version" 1 "storage_id" storage-id "agent_id" agent-id
         "through_event_id" event-id "through_storage_position" position
         "source_binding" binding)))))

(defun %sqlite-append-event
    (backend type payload
     &key (agent-id "default") occurred-at (caused-by :null) (tick-id :null)
       (affect-snapshot :null) expected-head-position)
  (%storage-required-string type "type")
  (%storage-required-string agent-id "agent-id")
  (let ((timestamp (or occurred-at (write-to-string (get-universal-time)))))
    (%storage-required-string timestamp "occurred-at")
    (bt:with-lock-held ((%sqlite-storage-lock backend))
      (%sqlite-in-transaction
       backend :append-event
       (lambda (handle)
         (when (and expected-head-position
                    (/= expected-head-position (%sqlite-head-position-unlocked handle nil)))
           (error 'storage-conflict-error :operation :conditional-append
                  :detail "event head changed before admission"))
         (let* ((event-id (1+ (%sqlite-max-event-id-unlocked handle nil)))
                (event (%storage-object
                        "schema_version" 1 "id" event-id "agent_id" agent-id
                        "timestamp" timestamp "type" type "payload" payload
                        "caused_by" caused-by "tick_id" tick-id
                        "affect_snapshot" affect-snapshot))
                (json (%storage-json event))
                (hash (%storage-sha256 json)))
           (%with-sqlite-statement
               (statement handle
                          "INSERT INTO pai_events(event_id,storage_origin,agent_id,partition_status,event_type,occurred_at,event_json,integrity_hash) VALUES(?1,'native',?2,'verified',?3,?4,?5,?6)"
                          :append-event)
             (%sqlite-bind-int64 handle statement 1 event-id :append-event)
             (%sqlite-bind-text handle statement 2 agent-id :append-event)
             (%sqlite-bind-text handle statement 3 type :append-event)
             (%sqlite-bind-text handle statement 4 timestamp :append-event)
             (%sqlite-bind-text handle statement 5 json :append-event)
             (%sqlite-bind-text handle statement 6 hash :append-event)
             (%sqlite-step handle statement :append-event +sqlite-done+))
           (values
            event
            (%sqlite-event-receipt
             (%sqlite-storage-id-unlocked handle :append-event)
             (%sqlite-head-position-unlocked handle nil)
             event-id agent-id type json hash))))))))

(defmethod storage-append-event
    ((backend sqlite-storage) type payload
     &key (agent-id "default") occurred-at (caused-by :null) (tick-id :null) (affect-snapshot :null))
  (%sqlite-append-event backend type payload :agent-id agent-id :occurred-at occurred-at
                       :caused-by caused-by :tick-id tick-id :affect-snapshot affect-snapshot))

(defmethod storage-append-event-if-head
    ((backend sqlite-storage) expected-head-position type payload
     &key (agent-id "default") occurred-at (caused-by :null) (tick-id :null) (affect-snapshot :null))
  (unless (and (integerp expected-head-position) (<= 0 expected-head-position))
    (error 'storage-error :operation :conditional-append :detail "expected head must be a nonnegative integer"))
  (%sqlite-append-event backend type payload :agent-id agent-id :occurred-at occurred-at
                       :caused-by caused-by :tick-id tick-id :affect-snapshot affect-snapshot
                       :expected-head-position expected-head-position))

(defun %sqlite-verified-event
    (event-id agent-id partition-status event-type occurred-at json stored-hash
     operation)
  (unless (and json stored-hash
               (string= stored-hash (%storage-sha256 json)))
    (error 'storage-integrity-error :operation operation
           :detail "event integrity hash mismatch"))
  (let ((event (%storage-json-read json operation)))
    (unless (and (= event-id (gethash "id" event))
                 (or (and (string= partition-status "verified")
                          (stringp (gethash "agent_id" event))
                          (string= agent-id (gethash "agent_id" event)))
                     (and (string= partition-status "legacy-partition-assumed")
                          (not (stringp (gethash "agent_id" event)))))
                 (string= event-type (gethash "type" event))
                 (string= occurred-at (gethash "timestamp" event)))
      (error 'storage-integrity-error :operation operation
             :detail "event index columns disagree with stored envelope"))
    event))

(defmethod storage-read-event ((backend sqlite-storage) event-id
                               &key agent-id event-type)
  (%storage-positive-integer event-id "event-id")
  (when agent-id (%storage-required-string agent-id "agent-id"))
  (when event-type (%storage-required-string event-type "event-type"))
  (bt:with-lock-held ((%sqlite-storage-lock backend))
    (let ((handle (%sqlite-handle backend :read-event)))
      (%with-sqlite-statement
          (statement handle
                     (cond
                       ((and agent-id event-type)
                        "SELECT event_id,agent_id,partition_status,event_type,occurred_at,event_json,integrity_hash FROM pai_events WHERE event_id=?1 AND agent_id=?2 AND event_type=?3 ORDER BY storage_sequence DESC LIMIT 1")
                       (agent-id
                        "SELECT event_id,agent_id,partition_status,event_type,occurred_at,event_json,integrity_hash FROM pai_events WHERE event_id=?1 AND agent_id=?2 ORDER BY storage_sequence DESC LIMIT 1")
                       (event-type
                        "SELECT event_id,agent_id,partition_status,event_type,occurred_at,event_json,integrity_hash FROM pai_events WHERE event_id=?1 AND event_type=?2 ORDER BY storage_sequence DESC LIMIT 1")
                       (t
                        "SELECT event_id,agent_id,partition_status,event_type,occurred_at,event_json,integrity_hash FROM pai_events WHERE event_id=?1 ORDER BY storage_sequence DESC LIMIT 1"))
                     :read-event)
        (%sqlite-bind-int64 handle statement 1 event-id :read-event)
        (when agent-id (%sqlite-bind-text handle statement 2 agent-id :read-event))
        (when event-type
          (%sqlite-bind-text handle statement (if agent-id 3 2)
                             event-type :read-event))
        (let ((code (%sqlite-step-raw statement)))
          (cond ((= code +sqlite-done+) nil)
                ((= code +sqlite-row+)
                 (%sqlite-verified-event (%sqlite-column-int64 statement 0)
                                         (%sqlite-column-text statement 1)
                                         (%sqlite-column-text statement 2)
                                         (%sqlite-column-text statement 3)
                                         (%sqlite-column-text statement 4)
                                         (%sqlite-column-text statement 5)
                                         (%sqlite-column-text statement 6)
                                         :read-event))
                (t (%sqlite-check code handle :read-event))))))))

(defmethod storage-read-event-before-position
    ((backend sqlite-storage) agent-id event-id before-position)
  (%storage-required-string agent-id "agent-id")
  (%storage-positive-integer event-id "event-id")
  (%storage-positive-integer before-position "before-position")
  (bt:with-lock-held ((%sqlite-storage-lock backend))
    (let ((handle (%sqlite-handle backend :read-event-before-position)))
      (%with-sqlite-statement
          (statement handle
                     "SELECT event_id,agent_id,partition_status,event_type,occurred_at,event_json,integrity_hash FROM pai_events INDEXED BY pai_events_agent_id_idx WHERE agent_id=?1 AND event_id=?2 AND storage_sequence<?3 ORDER BY storage_sequence DESC LIMIT 1"
                     :read-event-before-position)
        (%sqlite-bind-text handle statement 1 agent-id :read-event-before-position)
        (%sqlite-bind-int64 handle statement 2 event-id :read-event-before-position)
        (%sqlite-bind-int64 handle statement 3 before-position
                            :read-event-before-position)
        (let ((code (%sqlite-step-raw statement)))
          (cond ((= code +sqlite-done+) nil)
                ((= code +sqlite-row+)
                 (%sqlite-verified-event (%sqlite-column-int64 statement 0)
                                         (%sqlite-column-text statement 1)
                                         (%sqlite-column-text statement 2)
                                         (%sqlite-column-text statement 3)
                                         (%sqlite-column-text statement 4)
                                         (%sqlite-column-text statement 5)
                                         (%sqlite-column-text statement 6)
                                         :read-event-before-position))
                (t (%sqlite-check code handle :read-event-before-position))))))))

(defmethod storage-event-position
    ((backend sqlite-storage) agent-id event-id)
  (%storage-required-string agent-id "agent-id")
  (%storage-positive-integer event-id "event-id")
  (bt:with-lock-held ((%sqlite-storage-lock backend))
    (let ((handle (%sqlite-handle backend :event-position)))
      (%with-sqlite-statement
          (statement handle
                     "SELECT storage_sequence FROM pai_events INDEXED BY pai_events_agent_id_idx WHERE agent_id=?1 AND event_id=?2 ORDER BY storage_sequence ASC LIMIT 2"
                     :event-position)
        (%sqlite-bind-text handle statement 1 agent-id :event-position)
        (%sqlite-bind-int64 handle statement 2 event-id :event-position)
        (let ((code (%sqlite-step-raw statement)))
          (cond ((= code +sqlite-done+) nil)
                ((= code +sqlite-row+)
                 (let ((position (%sqlite-column-int64 statement 0))
                       (next (%sqlite-step-raw statement)))
                   (unless (= next +sqlite-done+)
                     (if (= next +sqlite-row+)
                         (error 'storage-conflict-error
                                :operation :event-position
                                :detail "logical event ID is not unique")
                         (%sqlite-check next handle :event-position)))
                   position))
                (t (%sqlite-check code handle :event-position))))))))

(defmethod storage-max-event-id-through-position
    ((backend sqlite-storage) agent-id through-position)
  (%storage-required-string agent-id "agent-id")
  (%storage-positive-integer through-position "through-position"
                             :zero-allowed t)
  (bt:with-lock-held ((%sqlite-storage-lock backend))
    (let ((handle (%sqlite-handle backend :prefix-max-event-id)))
      (when (> through-position
               (%sqlite-head-position-unlocked handle agent-id))
        (error 'storage-conflict-error :operation :prefix-max-event-id
               :detail "requested physical prefix exceeds authority head"))
      (%with-sqlite-statement
          (statement handle
                     "SELECT COALESCE(MAX(event_id),0) FROM pai_events INDEXED BY pai_events_agent_seq_idx WHERE agent_id=?1 AND storage_sequence<=?2"
                     :prefix-max-event-id)
        (%sqlite-bind-text handle statement 1 agent-id :prefix-max-event-id)
        (%sqlite-bind-int64 handle statement 2 through-position
                            :prefix-max-event-id)
        (%sqlite-step handle statement :prefix-max-event-id +sqlite-row+)
        (%sqlite-column-int64 statement 0)))))

(defmethod storage-scan-events
    ((backend sqlite-storage) &key (agent-id "default") (after-id 0)
                              event-type (limit 1000))
  (%storage-required-string agent-id "agent-id")
  (%storage-positive-integer after-id "after-id" :zero-allowed t)
  (when event-type (%storage-required-string event-type "event-type"))
  (%storage-positive-integer limit "limit")
  (bt:with-lock-held ((%sqlite-storage-lock backend))
    (let ((handle (%sqlite-handle backend :scan-events))
          (rows nil))
      (%with-sqlite-statement
          (statement handle
                     (if event-type
                         "SELECT event_id,agent_id,partition_status,event_type,occurred_at,event_json,integrity_hash FROM pai_events WHERE agent_id=?1 AND event_id>?2 AND event_type=?3 ORDER BY storage_sequence ASC LIMIT ?4"
                         "SELECT event_id,agent_id,partition_status,event_type,occurred_at,event_json,integrity_hash FROM pai_events WHERE agent_id=?1 AND event_id>?2 ORDER BY storage_sequence ASC LIMIT ?3")
                     :scan-events)
        (%sqlite-bind-text handle statement 1 agent-id :scan-events)
        (%sqlite-bind-int64 handle statement 2 after-id :scan-events)
        (if event-type
            (progn (%sqlite-bind-text handle statement 3 event-type :scan-events)
                   (%sqlite-bind-int64 handle statement 4 limit :scan-events))
            (%sqlite-bind-int64 handle statement 3 limit :scan-events))
        (loop for code = (%sqlite-step-raw statement)
              while (= code +sqlite-row+)
              do (push (%sqlite-verified-event (%sqlite-column-int64 statement 0)
                                               (%sqlite-column-text statement 1)
                                               (%sqlite-column-text statement 2)
                                               (%sqlite-column-text statement 3)
                                               (%sqlite-column-text statement 4)
                                               (%sqlite-column-text statement 5)
                                               (%sqlite-column-text statement 6)
                                               :scan-events)
                       rows)
              finally (unless (= code +sqlite-done+)
                        (%sqlite-check code handle :scan-events)))
        (coerce (nreverse rows) 'vector)))))

(defmethod storage-map-events
    ((backend sqlite-storage) visitor
     &key (agent-id "default") (after-position 0) through-position
          event-type event-types limit)
  "Stream verified rows in immutable physical order. VISITOR receives EVENT
and STORAGE-POSITION and must not recursively call this backend while its
read lock is held."
  (unless (functionp visitor)
    (error 'storage-error :operation :map-events
           :detail "visitor must be a function"))
  (%storage-required-string agent-id "agent-id")
  (%storage-positive-integer after-position "after-position" :zero-allowed t)
  (when through-position
    (%storage-positive-integer through-position "through-position"
                               :zero-allowed t)
    (when (< through-position after-position)
      (error 'storage-error :operation :map-events
             :detail "through-position cannot precede after-position")))
  (when event-type (%storage-required-string event-type "event-type"))
  (let ((types (cond ((null event-types) nil)
                     ((vectorp event-types) (coerce event-types 'list))
                     ((listp event-types) event-types)
                     (t :invalid))))
    (when (and event-type types)
      (error 'storage-error :operation :map-events
             :detail "event-type and event-types are mutually exclusive"))
    (when (or (eq types :invalid) (> (length types) 128)
              (some (lambda (type)
                      (not (and (stringp type) (plusp (length type)))))
                    types))
      (error 'storage-error :operation :map-events
             :detail "event-types must contain at most 128 non-empty strings"))
  (when limit (%storage-positive-integer limit "limit"))
  (bt:with-lock-held ((%sqlite-storage-lock backend))
    (let ((handle (%sqlite-handle backend :map-events))
          (count 0) (source-bytes 0) (last-position nil))
      (let* ((type-count (if event-type 1 (length types)))
             (type-start-index (if through-position 4 3))
             (type-clause
               (cond
                 (event-type
                  (format nil " AND event_type=?~d" type-start-index))
                 (types
                  (format nil " AND event_type IN (~{?~d~^,~})"
                          (loop for index from type-start-index repeat type-count
                                collect index)))
                 (t "")))
             (limit-index (+ type-start-index type-count))
             (sql
               (format nil
                       "SELECT storage_sequence,event_id,agent_id,partition_status,event_type,occurred_at,event_json,integrity_hash,length(CAST(event_json AS BLOB)) FROM pai_events WHERE agent_id=?1 AND storage_sequence>?2~:[~; AND storage_sequence<=?3~]~a ORDER BY storage_sequence ASC~:[~; LIMIT ?~d~]"
                       through-position type-clause (and limit t) limit-index)))
        (%with-sqlite-statement
            (statement handle sql :map-events)
        (%sqlite-bind-text handle statement 1 agent-id :map-events)
        (%sqlite-bind-int64 handle statement 2 after-position :map-events)
        (when through-position
          (%sqlite-bind-int64 handle statement 3 through-position :map-events))
        (cond
          (event-type
           (%sqlite-bind-text handle statement type-start-index event-type
                              :map-events))
          (types
           (loop for type in types for index from type-start-index
                 do (%sqlite-bind-text handle statement index type
                                       :map-events))))
        (when limit
          (%sqlite-bind-int64 handle statement limit-index limit :map-events))
        (loop for code = (%sqlite-step-raw statement)
              while (= code +sqlite-row+)
              do (let* ((position (%sqlite-column-int64 statement 0))
                        (event
                          (%sqlite-verified-event
                           (%sqlite-column-int64 statement 1)
                           (%sqlite-column-text statement 2)
                           (%sqlite-column-text statement 3)
                           (%sqlite-column-text statement 4)
                           (%sqlite-column-text statement 5)
                           (%sqlite-column-text statement 6)
                           (%sqlite-column-text statement 7)
                           :map-events)))
                   (funcall visitor event position)
                   (incf source-bytes (%sqlite-column-int64 statement 8))
                   (setf last-position position)
                   (incf count))
              finally (unless (= code +sqlite-done+)
                        (%sqlite-check code handle :map-events)))
          (values t last-position count source-bytes)))))))

(defun %sqlite-query-types (value field)
  (let ((types (cond ((null value) nil)
                     ((vectorp value) (coerce value 'list))
                     ((listp value) value)
                     (t :invalid))))
    (when (or (eq types :invalid) (> (length types) 32)
              (some (lambda (type)
                      (not (and (stringp type) (plusp (length type)))))
                    types))
      (error 'storage-error :operation :query-events
             :detail (format nil "~a must contain at most 32 non-empty strings"
                             field)))
    types))

(defun %sqlite-universal-time-iso8601 (value field)
  (unless (and (integerp value) (not (minusp value)))
    (error 'storage-error :operation :query-events
           :detail (format nil "~a must be a non-negative universal time" field)))
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time value 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            year month day hour minute second)))

(defun %sqlite-query-placeholders (count)
  (format nil "~{~a~^,~}" (loop repeat count collect "?")))

(defmethod storage-map-event-receipts
    ((backend sqlite-storage) visitor
     &key (agent-id "default") (after-position 0) through-position event-types)
  (unless (functionp visitor)
    (error 'storage-error :operation :map-event-receipts
           :detail "visitor must be a function"))
  (%storage-required-string agent-id "agent-id")
  (%storage-positive-integer after-position "after-position" :zero-allowed t)
  (when through-position
    (%storage-positive-integer through-position "through-position"
                               :zero-allowed t)
    (when (< through-position after-position)
      (error 'storage-error :operation :map-event-receipts
             :detail "through-position cannot precede after-position")))
  (let ((types (%sqlite-query-types event-types "event-types")))
    (unless types
      (error 'storage-error :operation :map-event-receipts
             :detail "at least one event type is required"))
    (bt:with-lock-held ((%sqlite-storage-lock backend))
      (let* ((handle (%sqlite-handle backend :map-event-receipts))
             (storage-id
               (%sqlite-storage-id-unlocked handle :map-event-receipts))
             (last-position after-position)
             (count 0)
             (placeholders
               (format nil "~{?~d~^,~}"
                       (loop for index from (if through-position 4 3)
                             repeat (length types) collect index)))
             (sql
               (format nil
                       "SELECT storage_sequence,event_id,agent_id,partition_status,event_type,occurred_at,event_json,integrity_hash FROM pai_events WHERE agent_id=?1 AND storage_sequence>?2~:[~; AND storage_sequence<=?3~] AND event_type IN (~a) ORDER BY storage_sequence ASC"
                       through-position placeholders)))
        (%with-sqlite-statement
            (statement handle sql :map-event-receipts)
          (%sqlite-bind-text handle statement 1 agent-id :map-event-receipts)
          (%sqlite-bind-int64 handle statement 2 after-position
                              :map-event-receipts)
          (when through-position
            (%sqlite-bind-int64 handle statement 3 through-position
                                :map-event-receipts))
          (loop for type in types
                for index from (if through-position 4 3)
                do (%sqlite-bind-text handle statement index type
                                      :map-event-receipts))
          (loop for code = (%sqlite-step-raw statement)
                while (= code +sqlite-row+)
                do (let* ((position (%sqlite-column-int64 statement 0))
                          (event-id (%sqlite-column-int64 statement 1))
                          (row-agent (%sqlite-column-text statement 2))
                          (event-type (%sqlite-column-text statement 4))
                          (event-json (%sqlite-column-text statement 6))
                          (event-hash (%sqlite-column-text statement 7)))
                     (%sqlite-verified-event
                      event-id row-agent (%sqlite-column-text statement 3)
                      event-type (%sqlite-column-text statement 5)
                      event-json event-hash :map-event-receipts)
                     (funcall visitor
                              (%sqlite-event-receipt
                               storage-id position event-id row-agent
                               event-type event-json event-hash))
                     (setf last-position position)
                     (incf count))
                finally (unless (= code +sqlite-done+)
                          (%sqlite-check code handle :map-event-receipts))))
        (values t last-position count)))))

(defun %sqlite-range-max-event-id-unlocked
    (handle agent-id after-id through-id operation)
  (let ((sql (cond ((and after-id through-id)
                    "SELECT MAX(event_id) FROM pai_events WHERE agent_id=?1 AND event_id>?2 AND event_id<=?3")
                   (after-id
                    "SELECT MAX(event_id) FROM pai_events WHERE agent_id=?1 AND event_id>?2")
                   (through-id
                    "SELECT MAX(event_id) FROM pai_events WHERE agent_id=?1 AND event_id<=?2")
                   (t
                    "SELECT MAX(event_id) FROM pai_events WHERE agent_id=?1"))))
    (%with-sqlite-statement (statement handle sql operation)
      (%sqlite-bind-text handle statement 1 agent-id operation)
      (when after-id (%sqlite-bind-int64 handle statement 2 after-id operation))
      (when through-id
        (%sqlite-bind-int64 handle statement (if after-id 3 2) through-id
                            operation))
      (%sqlite-step handle statement operation +sqlite-row+)
      (let ((maximum (%sqlite-column-int64 statement 0)))
        (and (plusp maximum) maximum)))))

(defmethod storage-query-events
    ((backend sqlite-storage) &key (agent-id "default") after-id through-id
                              from to limit event-types exclude-event-types)
  (%storage-required-string agent-id "agent-id")
  (when after-id
    (%storage-positive-integer after-id "after-id" :zero-allowed t))
  (when through-id
    (%storage-positive-integer through-id "through-id" :zero-allowed t))
  (when (and after-id through-id (> after-id through-id))
    (error 'storage-error :operation :query-events
           :detail "after-id cannot exceed through-id"))
  (when limit (%storage-positive-integer limit "limit"))
  (let* ((types (%sqlite-query-types event-types "event-types"))
         (excluded (%sqlite-query-types exclude-event-types
                                        "exclude-event-types"))
         (clauses (list "agent_id=?"))
         (bindings (list (cons :text agent-id))))
    (labels ((add (clause kind value)
               (setf clauses (append clauses (list clause))
                     bindings (append bindings (list (cons kind value))))))
      (when after-id (add "event_id>?" :int after-id))
      (when through-id (add "event_id<=?" :int through-id))
      (when from
        (add "occurred_at>=?" :text
             (%sqlite-universal-time-iso8601 from "from")))
      (when to
        (add "occurred_at<=?" :text
             (%sqlite-universal-time-iso8601 to "to")))
      (when types
        (setf clauses
              (append clauses
                      (list (format nil "event_type IN (~a)"
                                    (%sqlite-query-placeholders
                                     (length types))))))
        (dolist (type types)
          (setf bindings (append bindings (list (cons :text type))))))
      (when excluded
        (setf clauses
              (append clauses
                      (list (format nil "event_type NOT IN (~a)"
                                    (%sqlite-query-placeholders
                                     (length excluded))))))
        (dolist (type excluded)
          (setf bindings (append bindings (list (cons :text type))))))
      (when limit
        (setf bindings (append bindings (list (cons :int limit)))))
      (bt:with-lock-held ((%sqlite-storage-lock backend))
        (let* ((handle (%sqlite-handle backend :query-events))
               (sql
                 (format nil
                         "SELECT event_id,agent_id,partition_status,event_type,occurred_at,event_json,integrity_hash FROM pai_events WHERE ~{~a~^ AND ~} ORDER BY storage_sequence ~a~@[ LIMIT ?~]"
                         clauses (if limit "DESC" "ASC") limit))
               (rows nil) (boundary nil))
          ;; The filtered rows and unfiltered ID watermark must describe one
          ;; SQLite snapshot or projection rebuild can falsely certify a
          ;; concurrent append it never delivered to its visitor.
          (%sqlite-exec handle "BEGIN" :query-events)
          (handler-case
              (progn
                (setf boundary
                      (%sqlite-range-max-event-id-unlocked
                       handle agent-id after-id through-id :query-events))
                (%with-sqlite-statement (statement handle sql :query-events)
                  (loop for binding in bindings for index from 1
                        do (ecase (car binding)
                             (:text (%sqlite-bind-text
                                     handle statement index (cdr binding)
                                     :query-events))
                             (:int (%sqlite-bind-int64
                                    handle statement index (cdr binding)
                                    :query-events))))
                  (loop for code = (%sqlite-step-raw statement)
                        while (= code +sqlite-row+)
                        do (push (%sqlite-verified-event
                                  (%sqlite-column-int64 statement 0)
                                  (%sqlite-column-text statement 1)
                                  (%sqlite-column-text statement 2)
                                  (%sqlite-column-text statement 3)
                                  (%sqlite-column-text statement 4)
                                  (%sqlite-column-text statement 5)
                                  (%sqlite-column-text statement 6)
                                  :query-events)
                                 rows)
                        finally (unless (= code +sqlite-done+)
                                  (%sqlite-check code handle :query-events))))
                (%sqlite-exec handle "COMMIT" :query-events)
                (values (if limit rows (nreverse rows)) boundary))
            (error (condition)
              (ignore-errors (%sqlite-exec handle "ROLLBACK" :query-events))
              (error condition))))))))

(defmethod storage-range-max-event-id
    ((backend sqlite-storage) &key (agent-id "default") after-id through-id)
  (%storage-required-string agent-id "agent-id")
  (when after-id
    (%storage-positive-integer after-id "after-id" :zero-allowed t))
  (when through-id
    (%storage-positive-integer through-id "through-id" :zero-allowed t))
  (when (and after-id through-id (> after-id through-id))
    (error 'storage-error :operation :range-max-event-id
           :detail "after-id cannot exceed through-id"))
  (bt:with-lock-held ((%sqlite-storage-lock backend))
    (%sqlite-range-max-event-id-unlocked
     (%sqlite-handle backend :range-max-event-id)
     agent-id after-id through-id :range-max-event-id)))

(defmethod storage-recent-events
    ((backend sqlite-storage) event-types limit
     &key (agent-id "default") before-event-id)
  "Read a bounded newest window by indexed type, returned chronologically."
  (%storage-required-string agent-id "agent-id")
  (%storage-positive-integer limit "limit")
  (when before-event-id
    (%storage-positive-integer before-event-id "before-event-id"))
  (let ((types (cond ((vectorp event-types) (coerce event-types 'list))
                     ((listp event-types) event-types)
                     (t nil))))
    (unless (and types (<= (length types) 32)
                 (every (lambda (type)
                          (and (stringp type) (plusp (length type))))
                        types))
      (error 'storage-error :operation :recent-events
             :detail "event-types must contain 1-32 strings"))
    (bt:with-lock-held ((%sqlite-storage-lock backend))
      (let* ((handle (%sqlite-handle backend :recent-events))
             (placeholders
               (format nil "~{~a~^,~}"
                       (loop for index from 3 below (+ 3 (length types))
                             collect (format nil "?~d" index))))
             (limit-index (+ 3 (length types)))
             (sql
               (format nil
                       "SELECT event_id,agent_id,partition_status,event_type,occurred_at,event_json,integrity_hash FROM pai_events WHERE agent_id=?1 AND event_id<?2 AND event_type IN (~a) ORDER BY storage_sequence DESC LIMIT ?~d"
                       placeholders limit-index))
             (rows nil))
        (%with-sqlite-statement (statement handle sql :recent-events)
          (%sqlite-bind-text handle statement 1 agent-id :recent-events)
          (%sqlite-bind-int64 handle statement 2
                              (or before-event-id most-positive-fixnum)
                              :recent-events)
          (loop for type in types for index from 3
                do (%sqlite-bind-text handle statement index type
                                      :recent-events))
          (%sqlite-bind-int64 handle statement limit-index limit :recent-events)
          (loop for code = (%sqlite-step-raw statement)
                while (= code +sqlite-row+)
                do (push (%sqlite-verified-event
                          (%sqlite-column-int64 statement 0)
                          (%sqlite-column-text statement 1)
                          (%sqlite-column-text statement 2)
                          (%sqlite-column-text statement 3)
                          (%sqlite-column-text statement 4)
                          (%sqlite-column-text statement 5)
                          (%sqlite-column-text statement 6)
                          :recent-events)
                         rows)
                finally (unless (= code +sqlite-done+)
                          (%sqlite-check code handle :recent-events)))
          rows)))))

(defun sqlite-experience-page (backend agent-id from to before-position limit)
  "Bounded newest-first experience page, using physical position for stable paging."
  (%storage-required-string agent-id "agent-id")
  (unless (and (integerp from) (integerp to) (<= 0 from to)
               (integerp limit) (<= 1 limit 20)
               (or (null before-position)
                   (and (integerp before-position) (plusp before-position))))
    (error "Invalid experience page bounds"))
  (bt:with-lock-held ((%sqlite-storage-lock backend))
    (let* ((handle (%sqlite-handle backend :experience-page))
           (sql "SELECT event_id,agent_id,partition_status,event_type,occurred_at,event_json,integrity_hash,storage_sequence FROM pai_events WHERE agent_id=?1 AND occurred_at>=?2 AND occurred_at<=?3 AND storage_sequence<?4 AND event_type IN ('user-message','agent-message','peer-message-received','recursive-tool-result','recursive-curiosity-result','recursive-curiosity-incorporation-completed','recursive-curiosity-briefing-completed','recursive-peer-message-result','recursive-stimulus-result') ORDER BY storage_sequence DESC LIMIT ?5")
           (rows nil) (positions nil))
      (%with-sqlite-statement (statement handle sql :experience-page)
        (%sqlite-bind-text handle statement 1 agent-id :experience-page)
        (%sqlite-bind-text handle statement 2
                           (%sqlite-universal-time-iso8601 from "from") :experience-page)
        (%sqlite-bind-text handle statement 3
                           (%sqlite-universal-time-iso8601 to "to") :experience-page)
        (%sqlite-bind-int64 handle statement 4
                            (or before-position most-positive-fixnum) :experience-page)
        (%sqlite-bind-int64 handle statement 5 (1+ limit) :experience-page)
        (loop for code = (%sqlite-step-raw statement)
              while (= code +sqlite-row+)
              do (push (%sqlite-verified-event
                        (%sqlite-column-int64 statement 0)
                        (%sqlite-column-text statement 1)
                        (%sqlite-column-text statement 2)
                        (%sqlite-column-text statement 3)
                        (%sqlite-column-text statement 4)
                        (%sqlite-column-text statement 5)
                        (%sqlite-column-text statement 6) :experience-page) rows)
                 (push (%sqlite-column-int64 statement 7) positions)
              finally (unless (= code +sqlite-done+)
                        (%sqlite-check code handle :experience-page))))
      (setf rows (nreverse rows) positions (nreverse positions))
      (let ((more (> (length rows) limit)))
        (values (subseq rows 0 (min limit (length rows)))
                (and more (nth (1- limit) positions)))))))

(defmethod storage-publish-checkpoint
    ((backend sqlite-storage) projection-name state
     &key (agent-id "default") through-event-id
       through-position
       (projector-revision "1") (policy-revision "1"))
  (%storage-required-string projection-name "projection-name")
  (%storage-required-string agent-id "agent-id")
  (%storage-positive-integer through-event-id "through-event-id" :zero-allowed t)
  (when through-position
    (%storage-positive-integer through-position "through-position"
                               :zero-allowed t))
  (%storage-required-string projector-revision "projector-revision")
  (%storage-required-string policy-revision "policy-revision")
  (bt:with-lock-held ((%sqlite-storage-lock backend))
    (%sqlite-in-transaction
     backend :publish-checkpoint
     (lambda (handle)
       (let* ((maximum (%sqlite-max-event-id-unlocked handle agent-id))
              (head (%sqlite-head-position-unlocked handle agent-id))
              (position (or through-position head)))
         (when (> through-event-id maximum)
           (error 'storage-conflict-error :operation :publish-checkpoint
                  :detail "checkpoint watermark is beyond durable agent history"))
         (when (and (< through-event-id maximum)
                    (%sqlite-legacy-import-present-p handle agent-id))
           (error 'storage-conflict-error :operation :publish-checkpoint
                  :detail "partial numeric checkpoint cannot represent imported legacy storage order"))
         (when (> position head)
           (error 'storage-conflict-error :operation :publish-checkpoint
                  :detail "checkpoint position is beyond durable agent history"))
         (when (and (%sqlite-legacy-import-present-p handle agent-id)
                    (/= position head))
           (error 'storage-conflict-error :operation :publish-checkpoint
                  :detail "partial physical checkpoint cannot represent imported legacy storage order"))
         (setf through-position position))
       (let ((current-event -1) (current-position -1))
         (%with-sqlite-statement
             (statement handle
                        "SELECT through_event_id,through_storage_position FROM pai_projection_checkpoints WHERE projection_name=?1 AND agent_id=?2"
                        :publish-checkpoint)
           (%sqlite-bind-text handle statement 1 projection-name :publish-checkpoint)
           (%sqlite-bind-text handle statement 2 agent-id :publish-checkpoint)
           (let ((code (%sqlite-step-raw statement)))
             (cond ((= code +sqlite-row+)
                    (setf current-event (%sqlite-column-int64 statement 0)
                          current-position (%sqlite-column-int64 statement 1)))
                   ((/= code +sqlite-done+)
                    (%sqlite-check code handle :publish-checkpoint)))))
         (when (< through-event-id current-event)
           (error 'storage-conflict-error :operation :publish-checkpoint
                  :detail "checkpoint event watermark would move backwards"))
         (when (< through-position current-position)
           (error 'storage-conflict-error :operation :publish-checkpoint
                  :detail "checkpoint storage position would move backwards")))
       (let* ((state-json (%storage-json state))
              (hash (%storage-checkpoint-integrity-sha256
                      projection-name agent-id through-event-id through-position
                      projector-revision policy-revision state-json)))
         (%with-sqlite-statement
             (statement handle
                        "INSERT INTO pai_projection_checkpoints(projection_name,agent_id,through_event_id,through_storage_position,projector_revision,policy_revision,state_json,integrity_hash) VALUES(?1,?2,?3,?4,?5,?6,?7,?8) ON CONFLICT(projection_name,agent_id) DO UPDATE SET through_event_id=excluded.through_event_id,through_storage_position=excluded.through_storage_position,projector_revision=excluded.projector_revision,policy_revision=excluded.policy_revision,state_json=excluded.state_json,integrity_hash=excluded.integrity_hash,created_at=CURRENT_TIMESTAMP"
                        :publish-checkpoint)
           (%sqlite-bind-text handle statement 1 projection-name :publish-checkpoint)
           (%sqlite-bind-text handle statement 2 agent-id :publish-checkpoint)
           (%sqlite-bind-int64 handle statement 3 through-event-id :publish-checkpoint)
           (%sqlite-bind-int64 handle statement 4 through-position :publish-checkpoint)
           (%sqlite-bind-text handle statement 5 projector-revision :publish-checkpoint)
           (%sqlite-bind-text handle statement 6 policy-revision :publish-checkpoint)
           (%sqlite-bind-text handle statement 7 state-json :publish-checkpoint)
           (%sqlite-bind-text handle statement 8 hash :publish-checkpoint)
           (%sqlite-step handle statement :publish-checkpoint +sqlite-done+))
         (%storage-object "schema_version" 1 "projection_name" projection-name
                          "agent_id" agent-id "through_event_id" through-event-id
                          "through_storage_position" through-position
                          "projector_revision" projector-revision
                          "policy_revision" policy-revision "state" state
                          "integrity_hash" hash))))))

(defmethod storage-load-checkpoint ((backend sqlite-storage) projection-name
                                    &key (agent-id "default"))
  (%storage-required-string projection-name "projection-name")
  (%storage-required-string agent-id "agent-id")
  (bt:with-lock-held ((%sqlite-storage-lock backend))
    (let ((handle (%sqlite-handle backend :load-checkpoint)))
      (%with-sqlite-statement
          (statement handle
                     "SELECT through_event_id,through_storage_position,projector_revision,policy_revision,state_json,integrity_hash FROM pai_projection_checkpoints WHERE projection_name=?1 AND agent_id=?2"
                     :load-checkpoint)
        (%sqlite-bind-text handle statement 1 projection-name :load-checkpoint)
        (%sqlite-bind-text handle statement 2 agent-id :load-checkpoint)
        (let ((code (%sqlite-step-raw statement)))
          (cond
            ((= code +sqlite-done+) nil)
            ((= code +sqlite-row+)
             (let* ((through (%sqlite-column-int64 statement 0))
                    (position (%sqlite-column-int64 statement 1))
                    (projector (%sqlite-column-text statement 2))
                    (policy (%sqlite-column-text statement 3))
                    (state-json (%sqlite-column-text statement 4))
                    (stored-hash (%sqlite-column-text statement 5))
                    (actual-hash
                      (%storage-checkpoint-integrity-sha256
                        projection-name agent-id through position projector policy
                        state-json)))
               (unless (string= stored-hash actual-hash)
                 (error 'storage-integrity-error :operation :load-checkpoint
                        :detail "checkpoint integrity hash mismatch"))
               (%storage-object
                "schema_version" 1 "projection_name" projection-name
                "agent_id" agent-id "through_event_id" through
                "through_storage_position" position
                "projector_revision" projector "policy_revision" policy
                "state" (%storage-json-read state-json :load-checkpoint)
                "integrity_hash" stored-hash)))
            (t (%sqlite-check code handle :load-checkpoint))))))))
