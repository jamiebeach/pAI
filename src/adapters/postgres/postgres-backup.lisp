;;;; postgres-backup.lisp -- daily pg_dump backups of pai-postgres.
;;;;
;;;; 2026-07-27, right after the Postgres migration: pai-postgres's
;;;; actual data files live in a Docker-managed named volume
;;;; (pai-postgres-data), NOT on the state bind mount -- none of the
;;;; existing backup story (which only ever copies the state directory) covers it. If
;;;; that volume were ever deleted, the memory would be gone regardless of
;;;; anything backed up from the state directory. This writes a real pg_dump into
;;;; /agent/state/postgres-backups/ once a day, so it rides along with
;;;; everything else's existing backup coverage.
;;;;
;;;; Uses the real pg_dump binary (postgresql-client-16, baked into the
;;;; image via the official PGDG apt repo -- Debian bookworm's own
;;;; postgresql-client package is v15, which refuses to run against a v16
;;;; server: "aborting because of server version mismatch", confirmed live
;;;; before adding the PGDG repo) via a subprocess through /bin/sh (so
;;;; PGPASSWORD scoping is the shell's problem, not a guess at
;;;; UIOP:RUN-PROGRAM's :ENVIRONMENT list format).
;;;;
;;;; Load live (no restart) via lisp-eval or repl-drop:
;;;;   (load "/agent/state/postgres-backup.lisp")
;;;; Requires memory-nodes.lisp already loaded (for *PG-HOST*/*PG-PORT*/
;;;; *PG-USER*/*PG-PASSWORD*/*PG-DATABASE*) and pg_dump on PATH (baked into
;;;; the image).

(in-package :agent)

(export '(pg-backup-now pg-backup-start pg-backup-stop))

(defparameter *pg-backup-dir*
  (let ((root (or (uiop:getenv "PAI_ARTIFACT_ROOT")
                  (uiop:getenv "PAI_R3A_ARTIFACT_ROOT"))))
    (if (and root (plusp (length root)))
        (merge-pathnames "runtime-backups/" (pathname root))
        #P"/agent/state/postgres-backups/")))
(defparameter *pg-backup-interval-seconds* (* 24 3600))
(defparameter *pg-backup-retain-count* 14
  "Keep the last 14 daily backups (~2 weeks); older ones are pruned after
each successful new backup, so this directory never grows unbounded.")

(defvar *pg-backup-thread* nil)
(defvar *pg-backup-stop-requested* nil)

(defun %pg-backup-now-iso8601 ()
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~a-~2,'0d-~2,'0dT~2,'0d~2,'0d~2,'0dZ" year month day hour min sec)))

(defun %pg-backup-prune ()
  ;; This subsystem owns only its timestamped daily files.  Maintenance,
  ;; redeploy, migration, and operator backups share the directory but have
  ;; independent retention contracts and must never be pruned here.
  (let ((files (sort (directory (merge-pathnames "pai-memory-20*.sql" *pg-backup-dir*))
                      #'string> :key #'namestring)))
    (dolist (f (nthcdr *pg-backup-retain-count* files))
      (ignore-errors (delete-file f)))))

(defun pg-backup-now ()
  "Runs pg_dump against pai-postgres right now, writing a timestamped
plain-SQL dump to *PG-BACKUP-DIR*. Returns the path on success, :ERROR on
failure -- logged either way via LOG-EVENT if loaded, never signals (a
backup failure must never take anything else down with it)."
  (ensure-directories-exist *pg-backup-dir*)
  (let* ((path (merge-pathnames (format nil "pai-memory-~a.sql" (%pg-backup-now-iso8601)) *pg-backup-dir*))
         (cmd (format nil "PGPASSWORD=~a pg_dump -h ~a -p ~a -U ~a -d ~a -f ~a"
                      *pg-password* *pg-host* *pg-port* *pg-user* *pg-database* (namestring path))))
    (handler-case
        (multiple-value-bind (output error-output exit-code)
            (uiop:run-program (list "/bin/sh" "-c" cmd) :output :string :error-output :string :ignore-error-status t)
          (declare (ignore output))
          (if (and (zerop exit-code) (probe-file path))
              (progn
                (%pg-backup-prune)
                (when (fboundp 'log-event)
                  (ignore-errors
                    (funcall 'log-event "pg-backup"
                             (obj "path" (namestring path)
                                  "size_bytes" (with-open-file (s path :element-type '(unsigned-byte 8)) (file-length s))))))
                (format t "~&[pg-backup] wrote ~a~%" path)
                path)
              (progn
                (format t "~&[pg-backup] pg_dump failed (exit ~a): ~a~%" exit-code error-output)
                (when (fboundp 'log-event)
                  (ignore-errors (funcall 'log-event "pg-backup" (obj "error" error-output "exit_code" exit-code))))
                :error)))
      (error (e)
        (format t "~&[pg-backup] failed: ~a~%" e)
        (when (fboundp 'log-event)
          (ignore-errors (funcall 'log-event "pg-backup" (obj "error" (format nil "~a" e)))))
        :error))))

(defun pg-backup-start ()
  "Idempotent, same pattern as every other background thread here. Runs
one backup immediately (not just on the first 24h tick -- otherwise a
fresh deploy would have zero backups until a full day had passed), then
recurs daily."
  (pg-backup-now)
  (unless (and *pg-backup-thread* (bt:thread-alive-p *pg-backup-thread*))
    (setf *pg-backup-stop-requested* nil)
    (setf *pg-backup-thread*
          (bt:make-thread
           (lambda ()
             (loop until *pg-backup-stop-requested*
                   do (sleep *pg-backup-interval-seconds*)
                      (unless *pg-backup-stop-requested*
                        (handler-case (pg-backup-now) (error (e) (format t "~&[pg-backup] loop error: ~a~%" e))))))
           :name "pg-backup")))
  (format t "~&[pg-backup] running, every ~a hours, keeping last ~a backups in ~a.~%"
          (/ *pg-backup-interval-seconds* 3600) *pg-backup-retain-count* *pg-backup-dir*))

(defun pg-backup-stop (&optional (timeout 5))
  (setf *pg-backup-stop-requested* t)
  (loop with deadline = (+ (get-internal-real-time) (* timeout internal-time-units-per-second))
        while (and *pg-backup-thread* (bt:thread-alive-p *pg-backup-thread*) (< (get-internal-real-time) deadline))
        do (sleep 0.05))
  (if (and *pg-backup-thread* (bt:thread-alive-p *pg-backup-thread*))
      (progn (ignore-errors (bt:destroy-thread *pg-backup-thread*)) :force-killed)
      :stopped-cleanly))

(define-init :start postgres-backup-start
    "Start background worker for postgres-backup.

     PAI_PG_BACKUP=off skips it. PG-BACKUP-START takes a full dump immediately
     rather than waiting for the first daily tick, which is right for a real
     deploy and wrong for a disposable clone booted repeatedly: three test runs
     against a restored copy wrote 474 MB of dumps of a database that is itself
     a throwaway copy, and filled the host disk.

     Default is on, so production behaviour is unchanged by this switch."
  (if (string-equal (or (uiop:getenv "PAI_PG_BACKUP") "on") "off")
      (progn (format t "~&[pg-backup] disabled by PAI_PG_BACKUP=off~%") t)
      (pg-backup-start)))
