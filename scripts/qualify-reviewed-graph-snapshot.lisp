;;;; Content-free restore probe for a disposable SQLite authority snapshot.
;;;; The caller supplies a directory containing events.sqlite3 and
;;;; derived.sqlite3. Never point this at a live instance state directory.

(require :asdf)
(defparameter *reviewed-graph-probe-root*
  (or (uiop:getenv "PAI_SOURCE_ROOT") "/workspace/"))
(defparameter *reviewed-graph-probe-snapshot*
  (uiop:getenv "PAI_GRAPH_PROBE_SNAPSHOT_DIR"))
(unless (and *reviewed-graph-probe-snapshot*
             (plusp (length *reviewed-graph-probe-snapshot*))
             (search "/backups/graph-compat-"
                     *reviewed-graph-probe-snapshot*)
             (not (search ".." *reviewed-graph-probe-snapshot*))
             (not (member *reviewed-graph-probe-snapshot*
                          '("/" "/var/lib/pai" "/private")
                          :test #'string=)))
  (error "A disposable snapshot directory is required"))
(load (merge-pathnames "src/kernel/agent.lisp" *reviewed-graph-probe-root*))
(set (intern "*HEAP-HEALTH-AUTOSTART-P*" :agent) nil)
(asdf:load-asd (merge-pathnames "pai.asd" *reviewed-graph-probe-root*))
(asdf:load-system :pai)

(let ((directory
        (uiop:ensure-directory-pathname *reviewed-graph-probe-snapshot*)))
    (unless (search "/backups/graph-compat-"
                    (namestring (truename directory)))
      (error "Snapshot path resolves outside the disposable backup scope"))
    (unless (and (probe-file (merge-pathnames "events.sqlite3" directory))
                 (probe-file (merge-pathnames "derived.sqlite3" directory)))
      (error "Snapshot must contain both SQLite databases"))
    (let* ((source (agent::make-sqlite-storage
                    (merge-pathnames "events.sqlite3" directory)))
           (derived (agent::make-sqlite-derived-storage
                     (merge-pathnames "derived.sqlite3" directory))))
      (unwind-protect
           (let ((agent-id nil))
             (bordeaux-threads:with-lock-held
                 ((agent::%sqlite-derived-lock derived))
               (let ((handle (agent::%sqlite-derived-handle
                              derived :reviewed-graph-probe)))
                 (agent::%with-sqlite-statement
                     (statement handle
                                "SELECT DISTINCT agent_id FROM pai_projection_checkpoints WHERE projection_name='reviewed-context-graph-v1'"
                                :reviewed-graph-probe)
                   (unless (= agent::+sqlite-row+
                              (agent::%sqlite-step-raw statement))
                     (error "Reviewed graph receipt is absent"))
                   (setf agent-id (agent::%sqlite-column-text statement 0))
                   (unless (= agent::+sqlite-done+
                              (agent::%sqlite-step-raw statement))
                     (error "Snapshot contains multiple reviewed-graph agents")))))
             (let* ((checkpoint
                      (agent::storage-load-checkpoint
                       derived agent::*reviewed-context-graph-projection-name*
                       :agent-id agent-id))
                    (state (and checkpoint (gethash "state" checkpoint)))
                    (boundary (agent::storage-authority-boundary
                               source :agent-id agent-id))
                    (binding
                      (agent::storage-checkpoint-source-binding
                       source :agent-id agent-id
                       :through-event-id
                       (gethash "through_event_id" checkpoint)
                       :through-position
                       (gethash "through_storage_position" checkpoint))))
               (unless (and (hash-table-p state)
                            (string= (gethash "event_storage_id" state "")
                                     (gethash "storage_id" boundary "")))
                 (error "Reviewed graph source identity differs from snapshot authority"))
               (multiple-value-bind (graph owner ignored exposure)
                   (agent::reviewed-context-graph-restore
                    derived agent-id (gethash "persona_id" state)
                    (gethash "storage_id" boundary) binding)
                 (declare (ignore ignored exposure))
                 (unless (and graph owner)
                   (error "Reviewed graph did not restore"))
                 (format t "GRAPH-SNAPSHOT-PASS rows=~d head=~d entities=~d facts=~d~%"
                         (gethash "record_count" state)
                         (gethash "through_storage_position" checkpoint)
                         (pai.context-graph:context-graph-entity-count graph)
                         (pai.context-graph:context-graph-fact-count graph)))))
        (agent::storage-close derived)
        (agent::storage-close source))))
