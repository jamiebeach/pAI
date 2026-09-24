;;;; Offline, aggregate-only indexed conscious-state read from a backup.
(require :asdf)
(defparameter *indexed-probe-root*
  (or (uiop:getenv "PAI_SOURCE_ROOT") "/workspace/"))
(defparameter *indexed-probe-snapshot*
  (uiop:getenv "PAI_GRAPH_PROBE_SNAPSHOT_DIR"))
(unless (and *indexed-probe-snapshot*
             (search "/backups/graph-compat-" *indexed-probe-snapshot*)
             (not (search ".." *indexed-probe-snapshot*))
             (not (member *indexed-probe-snapshot*
                          '("/" "/var/lib/pai" "/private") :test #'string=)))
  (error "Disposable graph-compat snapshot path required"))
(load (merge-pathnames "src/kernel/agent.lisp" *indexed-probe-root*))
(set (intern "*HEAP-HEALTH-AUTOSTART-P*" :agent) nil)
(asdf:load-asd (merge-pathnames "pai.asd" *indexed-probe-root*))
(asdf:load-system :pai)

(let* ((directory (uiop:ensure-directory-pathname
                   *indexed-probe-snapshot*))
       (resolved (namestring (truename directory)))
       (event-path (merge-pathnames "events.sqlite3" directory))
       (derived-path (merge-pathnames "derived.sqlite3" directory)))
  (unless (and (search "/backups/graph-compat-" resolved)
               (probe-file event-path) (probe-file derived-path))
    (error "Snapshot files or path guard failed"))
  (let ((source (agent::make-sqlite-storage event-path))
        (derived (agent::make-sqlite-derived-storage derived-path)))
    (unwind-protect
         (let ((agent-id nil))
           (bordeaux-threads:with-lock-held
               ((agent::%sqlite-derived-lock derived))
             (let ((handle (agent::%sqlite-derived-handle
                            derived :indexed-conscious-snapshot)))
               (agent::%with-sqlite-statement
                   (statement handle
                              "SELECT DISTINCT agent_id FROM pai_projection_checkpoints WHERE projection_name='reviewed-context-graph-v1'"
                              :indexed-conscious-snapshot)
                 (unless (= agent::+sqlite-row+
                            (agent::%sqlite-step-raw statement))
                   (error "Snapshot graph identity absent"))
                 (setf agent-id (agent::%sqlite-column-text statement 0))
                 (unless (= agent::+sqlite-done+
                            (agent::%sqlite-step-raw statement))
                   (error "Snapshot graph identity ambiguous")))))
           (let* ((started (get-internal-real-time))
                  (now (get-universal-time)))
             (multiple-value-bind (state context lifecycle inbox)
                 (agent::conscious-storage-indexed-state
                  source derived agent-id :now now)
               (declare (ignore context))
               (format t
                       "INDEXED-CONSCIOUS-CURRENT-PASS head=~d admitted=~d barriers=~d consumed=~d rejected=~d degraded=~a watermark=~d lifecycles=~d state_revision=~d elapsed_ms=~,3f~%"
                       (agent::storage-head-position
                        source :agent-id agent-id)
                       (gethash "admitted_count" inbox)
                       (gethash "barrier_count" inbox)
                       (gethash "consumed_count" inbox)
                       (length (gethash "rejected" inbox))
                       (if (gethash "degraded" inbox) "yes" "no")
                       (gethash "watermark" inbox)
                       (gethash "active_count" lifecycle)
                       (gethash "state_revision" state)
                       (* 1000d0
                          (/ (- (get-internal-real-time) started)
                             internal-time-units-per-second)))
               (let ((counts (make-hash-table :test #'equal)))
                 (map nil (lambda (stimulus)
                            (incf (gethash (gethash "kind" stimulus "unknown")
                                           counts 0)))
                      (gethash "admitted" inbox))
                 (maphash (lambda (kind count)
                            (format t "INDEXED-CONSCIOUS-KIND ~a=~d~%"
                                    kind count))
                          counts)))
           (let ((recent
                   (agent::storage-recent-events
                    source '("user-message") 1 :agent-id agent-id)))
             (when recent
               (let ((started (get-internal-real-time)))
                 (multiple-value-bind (state)
                     (agent::conscious-storage-indexed-state
                      source derived agent-id
                      :through-event-id (gethash "id" (first recent)))
                   (format t
                           "INDEXED-CONSCIOUS-ASOF-PASS state_revision=~d elapsed_ms=~,3f~%"
                           (gethash "state_revision" state)
                           (* 1000d0
                              (/ (- (get-internal-real-time) started)
                                 internal-time-units-per-second)))))))))
      (agent::storage-close derived)
      (agent::storage-close source))))
