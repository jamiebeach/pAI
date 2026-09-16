(in-package :agent)

(ql:quickload '(:cffi :shasht :ironclad :bordeaux-threads) :silent t)

(defvar *storage-import-pass* 0)
(defvar *storage-import-fail* 0)

(defun storage-import-check (name condition)
  (if condition
      (progn (incf *storage-import-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *storage-import-fail*) (format t "  FAIL ~a~%" name))))

(defun storage-import-signals-p (condition-type thunk)
  (handler-case (progn (funcall thunk) nil)
    (condition (actual) (typep actual condition-type))))

(defun storage-import-write-lines (path lines)
  (ensure-directories-exist path)
  (with-open-file (output path :direction :output :if-exists :supersede
                               :if-does-not-exist :create
                               :external-format :utf-8)
    (dolist (line lines) (write-line line output))))

(defun storage-import-delete-files (&rest paths)
  (dolist (path paths)
    (dolist (candidate (list path
                             (pathname (concatenate 'string (namestring path) "-wal"))
                             (pathname (concatenate 'string (namestring path) "-shm"))))
      (when (probe-file candidate) (delete-file candidate)))))

(load (test-source "storage-substrate.lisp"))
(load (test-source "sqlite-storage.lisp"))
(load (test-source "sqlite-import.lisp"))

(format t "~%== offline SQLite import ==~%")
(storage-import-check "import entry point is present"
                      (fboundp 'sqlite-import-jsonl))
(storage-import-check "parity entry point is present"
                      (fboundp 'sqlite-audit-jsonl-import))

(let* ((root (test-state-dir))
       (source-a (merge-pathnames "storage-import-a.jsonl" root))
       (source-b (merge-pathnames "storage-import-b.jsonl" root))
       (database (merge-pathnames "storage-import.sqlite3" root))
       (rollback-source (merge-pathnames "storage-import-rollback.jsonl" root))
       (rollback-db (merge-pathnames "storage-import-rollback.sqlite3" root))
       (old-db (merge-pathnames "storage-import-format1.sqlite3" root))
       (duplicate-source (merge-pathnames "storage-import-duplicate.jsonl" root))
       (first-line
         "{ \"schema_version\": 2, \"id\": 2, \"timestamp\": \"2026-08-19T13:00:00Z\", \"type\": \"legacy\", \"payload\": {\"v\":1} }")
       (second-line
         "{\"schema_version\":2,\"id\":4,\"agent_id\":\"alpha\",\"timestamp\":\"2026-08-19T13:00:01Z\",\"type\":\"user-message\",\"payload\":{\"v\":2}}")
       (third-line
         "{\"schema_version\":2,\"id\":1,\"agent_id\":\"alpha\",\"timestamp\":\"2026-08-19T13:00:02Z\",\"type\":\"agent-message\",\"payload\":{\"v\":3}}")
       (fourth-line
         "{\"schema_version\":2,\"id\":4,\"agent_id\":\"alpha\",\"timestamp\":\"2026-08-19T13:00:03Z\",\"type\":\"later\",\"payload\":{\"v\":4}}")
       (backend nil)
       (rollback-backend nil))
  (storage-import-delete-files source-a source-b database rollback-source
                               rollback-db old-db duplicate-source)
  (unwind-protect
      (progn
        (storage-import-write-lines source-a (list first-line second-line))
        (storage-import-write-lines source-b (list "" third-line fourth-line))
        (setf backend (make-sqlite-storage database))
        (let ((report (sqlite-import-jsonl
                       backend (list source-a source-b)
                       :legacy-agent-id "alpha")))
          (storage-import-check "ordered files import with preserved IDs and gaps"
                                (and (= 4 (gethash "event_count" report))
                                     (= 2 (gethash "first_event_id" report))
                                     (= 4 (gethash "last_event_id" report))
                                     (= 4 (gethash "maximum_event_id" report))
                                     (= 3 (gethash "forward_hole_count" report))))
          (storage-import-check "duplicate and restarted ID sequences are preserved and reported"
                                (and (= 1 (gethash "duplicate_id_count" report))
                                     (= 1 (gethash "rewind_count" report))))
          (storage-import-check "partition provenance is counted"
                                (and (= 3 (gethash "verified_partition_count" report))
                                     (= 1 (gethash "assumed_partition_count" report)))))
        (let ((legacy (storage-read-event backend 2 :agent-id "alpha")))
          (storage-import-check "legacy partition is indexed without rewriting envelope"
                                (and legacy
                                     (not (stringp (gethash "agent_id" legacy)))
                                     (= 1 (gethash "v" (gethash "payload" legacy))))))
        (let ((rows (storage-scan-events backend :agent-id "alpha" :limit 10)))
          (storage-import-check "full replay follows immutable storage order"
                                (equal '(2 4 1 4)
                                       (loop for event across rows
                                             collect (gethash "id" event)))))
        (let ((audit (sqlite-audit-jsonl-import
                      backend (vector source-a source-b)
                      :legacy-agent-id "alpha")))
          (storage-import-check "independent exact-row parity succeeds"
                                (and (string= "verified" (gethash "status" audit))
                                     (= 4 (gethash "event_count" audit))
                                     (= 64 (length (gethash "row_hash_chain_sha256"
                                                            audit))))))
        (storage-import-check "duplicate lookup chooses newest exact partition match"
                              (= 4 (gethash "v" (gethash "payload"
                                                         (storage-read-event
                                                          backend 4
                                                          :agent-id "alpha")))))
        (storage-import-check "typed lookup can select the older duplicate"
                              (= 2 (gethash "v" (gethash "payload"
                                                         (storage-read-event
                                                          backend 4
                                                          :agent-id "alpha"
                                                          :event-type
                                                          "user-message")))))
        (storage-import-check "partial numeric checkpoint cannot skip a legacy rewind"
                              (storage-import-signals-p
                               'storage-conflict-error
                               (lambda ()
                                 (storage-publish-checkpoint
                                  backend "projection" (make-hash-table)
                                  :agent-id "alpha" :through-event-id 2))))
        (let ((new-event
                (storage-append-event
                 backend "post-import" (make-hash-table :test #'equal)
                 :agent-id "alpha" :occurred-at "2026-08-19T13:00:03Z")))
          (storage-import-check "new authority would continue after imported maximum"
                                (= 5 (gethash "id" new-event))))
        (storage-import-check "non-empty destination refuses a second import"
                              (storage-import-signals-p
                               'storage-conflict-error
                               (lambda ()
                                 (sqlite-import-jsonl
                                  backend source-a :legacy-agent-id "alpha"))))
        (storage-import-write-lines
         rollback-source
         (list
          "{\"id\":1,\"timestamp\":\"2026-08-19T13:01:00Z\",\"type\":\"a\"}"
          "{\"id\":3,\"timestamp\":\"2026-08-19T13:01:01Z\",\"type\":\"b\"}"))
        (setf rollback-backend (make-sqlite-storage rollback-db))
        (storage-import-check "interruption signals and rolls the whole transaction back"
                              (and (storage-import-signals-p
                                    'error
                                    (lambda ()
                                      (sqlite-import-jsonl
                                       rollback-backend rollback-source
                                       :legacy-agent-id "alpha"
                                       :progress-fn
                                       (lambda (count id)
                                         (declare (ignore id))
                                         (when (= count 2) (error "interrupt"))))))
                                   (zerop (storage-max-event-id rollback-backend))))
        (let ((retry (sqlite-import-jsonl
                      rollback-backend rollback-source
                      :legacy-agent-id "alpha")))
          (storage-import-check "retry after rollback has one unambiguous start state"
                                (= 2 (gethash "event_count" retry))))
        (storage-close rollback-backend)
        (setf rollback-backend nil)
        (storage-import-delete-files rollback-db)
        (setf rollback-backend (make-sqlite-storage rollback-db))
        (storage-import-write-lines
         duplicate-source
         (list
          "{\"id\":1,\"timestamp\":\"2026-08-19T13:02:00Z\",\"type\":\"a\"}"
          "{\"id\":0,\"timestamp\":\"2026-08-19T13:02:01Z\",\"type\":\"b\"}"))
        (storage-import-check "invalid source identity fails closed without partial import"
                              (and (storage-import-signals-p
                                    'storage-integrity-error
                                    (lambda ()
                                      (sqlite-import-jsonl
                                       rollback-backend duplicate-source
                                       :legacy-agent-id "alpha")))
                                   (zerop (storage-max-event-id rollback-backend))))
        (storage-import-check "missing legacy partition authority fails closed"
                              (storage-import-signals-p
                               'storage-conflict-error
                               (lambda ()
                                 (sqlite-import-jsonl
                                  rollback-backend rollback-source))))
        (cffi:with-foreign-object (handle-pointer :pointer)
          (setf (cffi:mem-ref handle-pointer :pointer) (cffi:null-pointer))
          (let ((code (%sqlite-open-v2
                       (namestring old-db) handle-pointer
                       (logior +sqlite-open-readwrite+ +sqlite-open-create+
                               +sqlite-open-fullmutex+)
                       (cffi:null-pointer))))
            (storage-import-check "format-1 fixture opens"
                                  (= code +sqlite-ok+))
            (let ((handle (cffi:mem-ref handle-pointer :pointer)))
              (%sqlite-exec
               handle
               "CREATE TABLE pai_storage_meta(meta_key TEXT PRIMARY KEY,meta_value TEXT NOT NULL); INSERT INTO pai_storage_meta VALUES('format_version','1'); CREATE TABLE pai_events(event_id INTEGER PRIMARY KEY,agent_id TEXT NOT NULL,event_type TEXT NOT NULL,occurred_at TEXT NOT NULL,event_json TEXT NOT NULL,integrity_hash TEXT NOT NULL)"
               :fixture)
              (%sqlite-close-v2 handle))))
        (storage-import-check "old experimental schema requires explicit migration"
                              (storage-import-signals-p
                               'storage-conflict-error
                               (lambda () (make-sqlite-storage old-db))))
        ;; The successful database now has an extra tail, so parity must fail
        ;; without writing or attempting to hide it.
        (storage-import-check "parity audit detects an extra destination tail"
                              (storage-import-signals-p
                               'storage-integrity-error
                               (lambda ()
                                 (sqlite-audit-jsonl-import
                                  backend (list source-a source-b)
                                  :legacy-agent-id "alpha")))))
    (when backend (ignore-errors (storage-close backend)))
    (when rollback-backend (ignore-errors (storage-close rollback-backend)))
    (storage-import-delete-files source-a source-b database rollback-source
                                 rollback-db old-db duplicate-source)))

(format t "~%~d passed, ~d failed~%" *storage-import-pass* *storage-import-fail*)
(when (plusp *storage-import-fail*)
  (error "storage import tests failed"))
