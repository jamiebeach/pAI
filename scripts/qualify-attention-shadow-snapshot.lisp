;;;; Offline, aggregate-only attention shadow build on a disposable backup.
(require :asdf)
(defparameter *attention-probe-root*
  (or (uiop:getenv "PAI_SOURCE_ROOT") "/workspace/"))
(defparameter *attention-probe-snapshot*
  (uiop:getenv "PAI_GRAPH_PROBE_SNAPSHOT_DIR"))
(unless (and *attention-probe-snapshot*
             (search "/backups/graph-compat-" *attention-probe-snapshot*)
             (not (search ".." *attention-probe-snapshot*))
             (not (member *attention-probe-snapshot*
                          '("/" "/var/lib/pai" "/private") :test #'string=)))
  (error "Disposable graph-compat snapshot path required"))
(load (merge-pathnames "src/kernel/agent.lisp" *attention-probe-root*))
(set (intern "*HEAP-HEALTH-AUTOSTART-P*" :agent) nil)
(asdf:load-asd (merge-pathnames "pai.asd" *attention-probe-root*))
(asdf:load-system :pai)

(let* ((directory (uiop:ensure-directory-pathname
                   *attention-probe-snapshot*))
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
                            derived :attention-snapshot)))
               (agent::%with-sqlite-statement
                   (statement handle
                              "SELECT DISTINCT agent_id FROM pai_projection_checkpoints WHERE projection_name='reviewed-context-graph-v1'"
                              :attention-snapshot)
                 (unless (= agent::+sqlite-row+
                            (agent::%sqlite-step-raw statement))
                   (error "Snapshot graph identity absent"))
                 (setf agent-id (agent::%sqlite-column-text statement 0))
                 (unless (= agent::+sqlite-done+
                            (agent::%sqlite-step-raw statement))
                   (error "Snapshot graph identity ambiguous")))))
           (agent::storage-shadow-attention-prepare derived)
           (let ((head (agent::storage-head-position
                        source :agent-id agent-id))
                 (revision
                   (agent::conscious-attention-shadow-policy-revision)))
             (loop for row =
                     (agent::storage-shadow-attention-apply-page
                      derived source
                      #'agent::conscious-attention-shadow-select
                      :agent-id agent-id :policy-revision revision
                      :limit 128 :maximum-event-bytes 8388608)
                   do (incf pages)
                      (when (zerop (mod pages 200))
                        (format t "ATTENTION-SHADOW-PROGRESS pages=~d through=~d head=~d~%"
                                pages (gethash "through_position" row) head)
                        (finish-output))
                   until (= (gethash "through_position" row) head))
             (let ((row (agent::storage-shadow-attention-report
                         derived source :agent-id agent-id
                         :policy-revision revision)))
               (unless (= head (gethash "through_position" row))
                 (error "Attention shadow did not reach source head"))
               (format t
                       "ATTENTION-SHADOW-BUILD-PASS pages=~d head=~d selected=~d elapsed_ms=~,3f~%"
                       pages head (gethash "selected_count" row)
                       (* 1000d0
                          (/ (- (get-internal-real-time) started)
                             internal-time-units-per-second))))))
      (agent::storage-close derived)
      (agent::storage-close source))))
