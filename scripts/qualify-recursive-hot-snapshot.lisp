;;;; Bounded offline shadow build against a disposable pair of SQLite copies.
;;;; This does not select the shadow as a live reader or touch a live ledger.

(require :asdf)
(defparameter *hot-probe-root*
  (or (uiop:getenv "PAI_SOURCE_ROOT") "/workspace/"))
(defparameter *hot-probe-snapshot*
  (uiop:getenv "PAI_GRAPH_PROBE_SNAPSHOT_DIR"))
(unless (and *hot-probe-snapshot*
             (plusp (length *hot-probe-snapshot*))
             (search "/backups/graph-compat-" *hot-probe-snapshot*)
             (not (search ".." *hot-probe-snapshot*))
             (not (member *hot-probe-snapshot*
                          '("/" "/var/lib/pai" "/private")
                          :test #'string=)))
  (error "A disposable SQLite snapshot directory is required"))
(load (merge-pathnames "src/kernel/agent.lisp" *hot-probe-root*))
(set (intern "*HEAP-HEALTH-AUTOSTART-P*" :agent) nil)
(asdf:load-asd (merge-pathnames "pai.asd" *hot-probe-root*))
(asdf:load-system :pai)

(let* ((directory (uiop:ensure-directory-pathname *hot-probe-snapshot*))
       (event-path (merge-pathnames "events.sqlite3" directory))
       (derived-path (merge-pathnames "derived.sqlite3" directory)))
  (unless (search "/backups/graph-compat-"
                  (namestring (truename directory)))
    (error "Snapshot path resolves outside the disposable backup scope"))
  (unless (and (probe-file event-path) (probe-file derived-path))
    (error "Snapshot must contain event and derived databases"))
  (let ((source (agent::make-sqlite-storage event-path))
        (derived (agent::make-sqlite-derived-storage derived-path)))
    (unwind-protect
         (let ((agent-id nil) (pages 0)
               (started (get-internal-real-time)))
           (bordeaux-threads:with-lock-held
               ((agent::%sqlite-derived-lock derived))
             (let ((handle (agent::%sqlite-derived-handle
                            derived :recursive-hot-probe)))
               (agent::%with-sqlite-statement
                   (statement handle
                              "SELECT DISTINCT agent_id FROM pai_projection_checkpoints WHERE projection_name='reviewed-context-graph-v1'"
                              :recursive-hot-probe)
                 (unless (= agent::+sqlite-row+
                            (agent::%sqlite-step-raw statement))
                   (error "Snapshot has no reviewed graph identity"))
                 (setf agent-id (agent::%sqlite-column-text statement 0))
                 (unless (= agent::+sqlite-done+
                            (agent::%sqlite-step-raw statement))
                   (error "Snapshot has more than one graph identity")))))
           (agent::storage-shadow-recursive-hot-prepare derived)
           (let* ((head (agent::storage-head-position
                         source :agent-id agent-id))
                  (selector
                    (lambda (event position)
                      (declare (ignore position))
                      (when (agent::%recursive-thread-event-p event)
                        (values event
                                (let ((cause (gethash "caused_by" event)))
                                  (if (and (integerp cause) (plusp cause))
                                      cause (gethash "id" event))))))))
             (loop for report =
                     (agent::storage-shadow-recursive-hot-apply-page
                      derived source selector :agent-id agent-id
                      :projector-revision
                      agent::*conscious-recursive-thread-events-projector-revision*
                      :policy-revision
                      agent::*conscious-recursive-thread-events-policy-revision*
                      :event-types
                      agent::*conscious-recursive-thread-event-types*
                      :maximum-event-bytes 8388608 :limit 128)
                   do (incf pages)
                      (when (zerop (mod pages 200))
                        (format t "HOT-SHADOW-PROGRESS pages=~d through=~d head=~d~%"
                                pages (gethash "through_position" report) head)
                        (finish-output))
                   until (= (gethash "through_position" report) head)
                   finally
                     (format t
                             "HOT-SHADOW-BUILD-PASS pages=~d selected=~d head=~d elapsed_ms=~,3f~%"
                             pages (gethash "selected_count" report) head
                             (* 1000d0
                                (/ (- (get-internal-real-time) started)
                                   internal-time-units-per-second)))))
           (agent::storage-shadow-conscious-pulse-prepare derived)
           (let ((pulse-pages 0)
                 (head (agent::storage-head-position
                        source :agent-id agent-id)))
             (loop for row =
                     (agent::storage-shadow-conscious-pulse-apply-page
                      derived source :agent-id agent-id :limit 128)
                   do (incf pulse-pages)
                   until (= (gethash "through_position" row) head))
             (format t "PULSE-ROW-SNAPSHOT-PASS pages=~d head=~d~%"
                     pulse-pages head)))
      (agent::storage-close derived)
      (agent::storage-close source))))
