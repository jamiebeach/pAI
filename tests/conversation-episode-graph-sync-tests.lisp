;;;; conversation-episode-graph-sync-tests.lisp -- bounded rebuild and tail.

(in-package :agent)

(ql:quickload '(:cffi :shasht :ironclad :bordeaux-threads) :silent t)

(dolist (file '("storage-substrate.lisp" "memory-storage.lisp"
                "sqlite-storage.lisp" "sqlite-derived-storage.lisp"
                "conversation-episode-graph.lisp"
                "conversation-episode-graph-storage.lisp"
                "conversation-episode-graph-sync.lisp"))
  (load (test-source file)))

(defvar *ceg-sync-pass* 0)
(defvar *ceg-sync-fail* 0)

(defun ceg-sync-check (name condition)
  (if condition
      (progn (incf *ceg-sync-pass*) (format t "PASS ~a~%" name))
      (progn (incf *ceg-sync-fail*) (format t "FAIL ~a~%" name))))

(defun ceg-sync-obj (&rest pairs)
  (let ((object (make-hash-table :test #'equal)))
    (loop for (key value) on pairs by #'cddr
          do (setf (gethash key object) value))
    object))

(defun ceg-sync-payload (episode-id source-start synopsis cues)
  (ceg-sync-obj
   "schema_version" 1 "episode_id" episode-id
   "persona_id" "sync-persona"
   "first_event_id" source-start "last_event_id" (1+ source-start)
   "first_timestamp" source-start "last_timestamp" (1+ source-start)
   "source_event_ids" (vector source-start (1+ source-start))
   "synopsis" synopsis "subjects" (vector) "entities" (vector)
   "retrieval_cues" (coerce cues 'vector)
   "broader_categories" (vector "shared")
   "unresolved_threads" (vector)))

(defun ceg-sync-delete-db (path)
  (dolist (candidate
           (list path
                 (pathname (concatenate 'string (namestring path) "-wal"))
                 (pathname (concatenate 'string (namestring path) "-shm"))))
    (when (probe-file candidate) (delete-file candidate))))

(defun ceg-sync-node-hash (backend canonical-key)
  (bt:with-lock-held ((%sqlite-derived-lock backend))
    (%with-sqlite-statement
        (statement (%sqlite-derived-handle backend :ceg-sync-fixture)
                   "SELECT integrity_hash FROM pai_knowledge_graph_nodes WHERE projection_name='conversation-episode-graph' AND agent_id='sync-agent' AND persona_id='sync-persona' AND node_kind='episode' AND canonical_key=?1"
                   :ceg-sync-fixture)
      (%sqlite-bind-text (%sqlite-derived-handle backend :ceg-sync-fixture)
                         statement 1 canonical-key :ceg-sync-fixture)
      (when (= +sqlite-row+ (%sqlite-step-raw statement))
        (%sqlite-column-text statement 0)))))

(format t "~%== conversation episode graph synchronization ==~%")

(let* ((event-db (merge-pathnames "ceg-sync-events.sqlite3" (test-state-dir)))
       (derived-db (merge-pathnames "ceg-sync-derived.sqlite3" (test-state-dir)))
       (events nil) (derived nil))
  (ceg-sync-delete-db event-db)
  (ceg-sync-delete-db derived-db)
  (unwind-protect
      (progn
        (setf events (make-sqlite-storage event-db)
              derived (make-sqlite-derived-storage derived-db))
        (storage-append-event
         events "conversation-episode-sealed"
         (ceg-sync-payload "episode:one" 100 "First episode." '("alpha"))
         :agent-id "sync-agent")
        (let ((report (conversation-episode-graph-synchronize
                       events derived "sync-agent" "sync-persona")))
          (ceg-sync-check "absent generation cold-rebuilds through one exact boundary"
                          (and (string= "cold-rebuild" (gethash "mode" report))
                               (= 1 (gethash "episode_count" report))
                               (= 1 (gethash "through_storage_position" report)))))
        (let ((first-hash (ceg-sync-node-hash derived "episode:one")))
          (storage-append-event events "operator-note"
                                (ceg-sync-obj "text" "irrelevant")
                                :agent-id "sync-agent")
          (storage-append-event
           events "conversation-episode-sealed"
           (ceg-sync-payload "episode:two" 200 "Second episode." '("beta"))
           :agent-id "sync-agent")
          (let ((report (conversation-episode-graph-synchronize
                         events derived "sync-agent" "sync-persona")))
            (ceg-sync-check "warm generation folds only relevant physical tail"
                            (and (string= "incremental-tail"
                                          (gethash "mode" report))
                                 (= 1 (gethash "episode_event_count" report))
                                 (= 1 (gethash "changed_episode_count" report))
                                 (zerop (gethash "full_generation_rows_read"
                                                 report -1))
                                 (= 3 (gethash "through_storage_position" report))))
            (ceg-sync-check "unaffected episode row remains byte-identical"
                            (string= first-hash
                                     (ceg-sync-node-hash derived
                                                         "episode:one")))))
        (storage-append-event
         events "conversation-episode-sealed"
         (ceg-sync-payload "episode:one" 300 "First episode revised."
                           '("gamma"))
         :agent-id "sync-agent")
        (let ((report (conversation-episode-graph-synchronize
                       events derived "sync-agent" "sync-persona")))
          (ceg-sync-check "reseal replaces one episode generation"
                           (and (string= "incremental-tail"
                                         (gethash "mode" report))
                                (= 1 (gethash "changed_episode_count" report))
                                (= 2 (gethash "episode_count" report)))))
        (multiple-value-bind (restored ignored)
            (conversation-episode-graph-restore
             derived "sync-agent" "sync-persona"
             :event-storage-id (gethash "storage_id"
                                        (storage-authority-boundary
                                         events :agent-id "sync-agent")))
          (declare (ignore ignored))
          (let ((json (shasht:write-json restored nil)))
            (ceg-sync-check "reseal removes old orphan concept and preserves peer"
                            (and (not (search "alpha" json))
                                 (search "gamma" json)
                                 (search "beta" json)))))
        (storage-append-event events "operator-note"
                              (ceg-sync-obj "text" "advance only")
                              :agent-id "sync-agent")
        (let ((report (conversation-episode-graph-synchronize
                       events derived "sync-agent" "sync-persona")))
          (ceg-sync-check "irrelevant authority tail advances without graph changes"
                          (and (string= "incremental-tail"
                                        (gethash "mode" report))
                               (zerop (gethash "episode_event_count" report))
                               (zerop (gethash "changed_episode_count" report))
                               (= 5 (gethash "through_storage_position" report)))))
        ;; Any untrusted derived generation is disposable. Corrupt the signed
        ;; checkpoint and prove synchronization rebuilds from ledger authority.
        (%sqlite-exec
         (%sqlite-derived-handle derived :ceg-sync-corrupt)
         "UPDATE pai_projection_checkpoints SET state_json='{}' WHERE projection_name='conversation-episode-graph' AND agent_id='sync-agent'"
         :ceg-sync-corrupt)
        (let ((report (conversation-episode-graph-synchronize
                       events derived "sync-agent" "sync-persona")))
          (ceg-sync-check "corrupt generation falls back to bounded cold rebuild"
                          (and (string= "cold-rebuild" (gethash "mode" report))
                               (stringp (gethash "fallback_reason" report))
                               (= 2 (gethash "episode_count" report)))))
        (let* ((boundary (storage-authority-boundary
                          events :agent-id "sync-agent"))
               (binding (storage-checkpoint-source-binding
                         events :agent-id "sync-agent"
                         :through-event-id (gethash "through_event_id" boundary)
                         :through-position
                         (gethash "through_storage_position" boundary))))
          (ceg-sync-check "captured authority boundary is internally exact"
                          (string= binding
                                   (gethash "source_binding" boundary)))))
    (when events (ignore-errors (storage-close events)))
    (when derived (ignore-errors (storage-close derived)))
    (ceg-sync-delete-db event-db)
    (ceg-sync-delete-db derived-db)))

(format t "~%~d passed, ~d failed~%" *ceg-sync-pass* *ceg-sync-fail*)
(when (plusp *ceg-sync-fail*)
  (error "Conversation episode graph synchronization tests failed"))
