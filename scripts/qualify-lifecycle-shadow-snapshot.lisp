;;;; Offline lifecycle-row build on a disposable SQLite backup only.
;;;; Prints counts and timing, never source identities or event content.
(require :asdf)
(defparameter *lifecycle-probe-root*
  (or (uiop:getenv "PAI_SOURCE_ROOT") "/workspace/"))
(defparameter *lifecycle-probe-snapshot*
  (uiop:getenv "PAI_GRAPH_PROBE_SNAPSHOT_DIR"))
(unless (and *lifecycle-probe-snapshot*
             (search "/backups/graph-compat-" *lifecycle-probe-snapshot*)
             (not (search ".." *lifecycle-probe-snapshot*))
             (not (member *lifecycle-probe-snapshot*
                          '("/" "/var/lib/pai" "/private") :test #'string=)))
  (error "Disposable graph-compat snapshot path required"))
(load (merge-pathnames "src/kernel/agent.lisp" *lifecycle-probe-root*))
(set (intern "*HEAP-HEALTH-AUTOSTART-P*" :agent) nil)
(asdf:load-asd (merge-pathnames "pai.asd" *lifecycle-probe-root*))
(asdf:load-system :pai)

(let* ((directory (uiop:ensure-directory-pathname
                   *lifecycle-probe-snapshot*))
       (resolved (namestring (truename directory)))
       (event-path (merge-pathnames "events.sqlite3" directory))
       (derived-path (merge-pathnames "derived.sqlite3" directory)))
  (unless (and (search "/backups/graph-compat-" resolved)
               (probe-file event-path) (probe-file derived-path))
    (error "Snapshot files or path guard failed"))
  (let ((source (agent::make-sqlite-storage event-path))
        (derived (agent::make-sqlite-derived-storage derived-path)))
    (unwind-protect
         (let ((agent-id nil)
               (pages 0)
               (started (get-internal-real-time)))
           (bordeaux-threads:with-lock-held
               ((agent::%sqlite-derived-lock derived))
             (let ((handle (agent::%sqlite-derived-handle
                            derived :lifecycle-snapshot)))
               (agent::%with-sqlite-statement
                   (statement handle
                              "SELECT DISTINCT agent_id FROM pai_projection_checkpoints WHERE projection_name='reviewed-context-graph-v1'"
                              :lifecycle-snapshot)
                 (unless (= agent::+sqlite-row+
                            (agent::%sqlite-step-raw statement))
                   (error "Snapshot graph identity absent"))
                 (setf agent-id (agent::%sqlite-column-text statement 0))
                 (unless (= agent::+sqlite-done+
                            (agent::%sqlite-step-raw statement))
                   (error "Snapshot graph identity ambiguous")))))
           (agent::storage-shadow-lifecycle-prepare derived)
           (let ((head (agent::storage-head-position
                        source :agent-id agent-id)))
             (loop for row =
                     (agent::storage-shadow-lifecycle-apply-page
                      derived source #'agent::conscious-lifecycle-shadow-step
                      :agent-id agent-id :limit 128)
                   do (incf pages)
                      (when (zerop (mod pages 200))
                        (format t "LIFECYCLE-SHADOW-PROGRESS pages=~d through=~d head=~d~%"
                                pages (gethash "through_position" row) head)
                        (finish-output))
                   until (= (gethash "through_position" row) head))
             (let ((row (agent::storage-shadow-lifecycle-watermark
                         derived source :agent-id agent-id)))
               (unless (= head (gethash "through_position" row))
                 (error "Lifecycle shadow did not reach source head"))
               (format t
                       "LIFECYCLE-SHADOW-BUILD-PASS pages=~d head=~d lifecycles=~d requests=~d invalid=~d elapsed_ms=~,3f~%"
                       pages head (gethash "lifecycle_count" row)
                       (gethash "request_count" row)
                       (gethash "invalid_count" row)
                       (* 1000d0
                          (/ (- (get-internal-real-time) started)
                             internal-time-units-per-second))))))
      (agent::storage-close derived)
      (agent::storage-close source))))
