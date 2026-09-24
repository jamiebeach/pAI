;;;; Explicit root-index preparation on a disposable SQLite authority copy.
;;;; Never point this at a running instance's state directory.

(require :asdf)
(defparameter *activity-index-probe-root*
  (or (uiop:getenv "PAI_SOURCE_ROOT") "/workspace/"))
(defparameter *activity-index-probe-snapshot*
  (uiop:getenv "PAI_GRAPH_PROBE_SNAPSHOT_DIR"))
(unless (and *activity-index-probe-snapshot*
             (search "/backups/graph-compat-"
                     *activity-index-probe-snapshot*)
             (not (search ".." *activity-index-probe-snapshot*)))
  (error "A disposable snapshot directory is required"))
(load (merge-pathnames "src/kernel/agent.lisp"
                       *activity-index-probe-root*))
(set (intern "*HEAP-HEALTH-AUTOSTART-P*" :agent) nil)
(asdf:load-asd (merge-pathnames "pai.asd" *activity-index-probe-root*))
(asdf:load-system :pai)

(let* ((directory
         (uiop:ensure-directory-pathname
          *activity-index-probe-snapshot*))
       (path (merge-pathnames "events.sqlite3" directory)))
  (unless (and (search "/backups/graph-compat-"
                       (namestring (truename directory)))
               (probe-file path))
    (error "Snapshot path is outside the disposable backup scope"))
  (let ((source (agent::make-sqlite-storage path)))
    (unwind-protect
         (let ((ready-before (agent::storage-activity-index-ready-p
                              source))
               (started (get-internal-real-time)))
           (agent::storage-prepare-activity-index source)
           (unless (agent::storage-activity-index-ready-p source)
             (error "Root activity index is absent after preparation"))
           (format t
                   "ACTIVITY-INDEX-SNAPSHOT-PASS ready_before=~a elapsed_ms=~,3f~%"
                   (if ready-before "true" "false")
                   (* 1000d0
                      (/ (- (get-internal-real-time) started)
                         internal-time-units-per-second))))
      (agent::storage-close source))))
