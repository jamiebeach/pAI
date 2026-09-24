;;;; Read-only behavior probe on a guarded disposable SQLite copy.
;;;; The adapter may run idempotent schema preparation, but the probe never
;;;; imports, rebuilds or refreshes a conscious checkpoint.
(require :asdf)
(defparameter *ledger-probe-root*
  (or (uiop:getenv "PAI_SOURCE_ROOT") "/workspace/"))
(defparameter *ledger-probe-snapshot*
  (uiop:getenv "PAI_GRAPH_PROBE_SNAPSHOT_DIR"))
(unless (and *ledger-probe-snapshot*
             (search "/backups/graph-compat-" *ledger-probe-snapshot*)
             (not (search ".." *ledger-probe-snapshot*)))
  (error "Disposable graph-compat snapshot path required"))
(load (merge-pathnames "src/kernel/agent.lisp" *ledger-probe-root*))
(set (intern "*HEAP-HEALTH-AUTOSTART-P*" :agent) nil)
(asdf:load-asd (merge-pathnames "pai.asd" *ledger-probe-root*))
(asdf:load-system :pai)

(let* ((directory (uiop:ensure-directory-pathname *ledger-probe-snapshot*))
       (resolved (namestring (truename directory)))
       (event-path (merge-pathnames "events.sqlite3" directory))
       (derived-path (merge-pathnames "derived.sqlite3" directory)))
  (unless (and (search "/backups/graph-compat-" resolved)
               (probe-file event-path) (probe-file derived-path))
    (error "Snapshot files or path guard failed"))
  (let ((derived (agent::make-sqlite-derived-storage derived-path)))
    (unwind-protect
         (let ((agent-id nil) (checkpoint-before nil)
               (started (get-internal-real-time)))
           (labels ((checkpoint-metadata ()
                      (bordeaux-threads:with-lock-held
                          ((agent::%sqlite-derived-lock derived))
                        (let ((handle (agent::%sqlite-derived-handle
                                       derived :ledger-only-probe)))
                          (agent::%with-sqlite-statement
                              (statement handle
                                         "SELECT through_event_id,through_storage_position,projector_revision,integrity_hash FROM pai_projection_checkpoints WHERE projection_name='conscious-runtime' AND agent_id=?1"
                                         :ledger-only-probe)
                            (agent::%sqlite-bind-text
                             handle statement 1 agent-id :ledger-only-probe)
                            (let ((code (agent::%sqlite-step-raw statement)))
                              (cond ((= code agent::+sqlite-done+) nil)
                                    ((= code agent::+sqlite-row+)
                                     (list
                                      (agent::%sqlite-column-int64 statement 0)
                                      (agent::%sqlite-column-int64 statement 1)
                                      (agent::%sqlite-column-text statement 2)
                                      (agent::%sqlite-column-text statement 3)))
                                    (t (error "Checkpoint metadata read failed")))))))))
             (bordeaux-threads:with-lock-held
                 ((agent::%sqlite-derived-lock derived))
               (let ((handle (agent::%sqlite-derived-handle
                              derived :ledger-only-probe)))
                 (agent::%with-sqlite-statement
                     (statement handle
                                "SELECT DISTINCT agent_id FROM pai_projection_checkpoints WHERE projection_name='reviewed-context-graph-v1'"
                                :ledger-only-probe)
                   (unless (= agent::+sqlite-row+
                              (agent::%sqlite-step-raw statement))
                     (error "Snapshot identity absent"))
                   (setf agent-id (agent::%sqlite-column-text statement 0))
                   (unless (= agent::+sqlite-done+
                              (agent::%sqlite-step-raw statement))
                     (error "Snapshot identity ambiguous")))))
             (setf checkpoint-before (checkpoint-metadata))
             (multiple-value-bind (backend receipt)
                 (agent::sqlite-event-authority-prepare
                  event-path nil :derived-database derived-path
                  :agent-id agent-id :restore-projection-p nil)
               (unwind-protect
                    (progn
                      (unless (and
                               (string= "opened-ledger-only"
                                        (gethash "status" receipt))
                               (= (gethash "head_position"
                                           (agent::event-authority-report))
                                  (agent::storage-head-position
                                   backend :agent-id agent-id))
                               (handler-case
                                   (progn (agent::event-projection-events) nil)
                                 (agent::storage-unavailable-error () t)))
                        (error "Ledger-only authority did not open safely"))
                      (unless (equal checkpoint-before (checkpoint-metadata))
                        (error "Ledger-only open changed checkpoint metadata"))
                      (format t
                              "LEDGER-ONLY-BACKUP-PASS head=~d checkpoint_unchanged=1 elapsed_ms=~,3f~%"
                              (gethash "head_position"
                                       (agent::event-authority-report))
                              (* 1000d0
                                 (/ (- (get-internal-real-time) started)
                                    internal-time-units-per-second))))
                 (agent::event-authority-clear)))))
      (agent::storage-close derived))))
