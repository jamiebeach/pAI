;;;; conversation-episode-graph-sync.lisp -- event-bound KG1 maintenance.
;;;;
;;;; The append-only ledger is authority.  This owner captures an exact event
;;;; generation, streams only sealed-episode receipts through that generation,
;;;; and either rebuilds or advances the disposable derived graph.

(in-package :agent)

(export '(conversation-episode-graph-synchronize))

(defun %ceg-sync-events (event-backend agent-id after-position boundary)
  (let ((events nil)
        (storage-id (gethash "storage_id" boundary)))
    (multiple-value-bind (complete-p ignored-last count)
        (storage-map-event-receipts
         event-backend
         (lambda (receipt)
           (unless (string= storage-id (gethash "storage_id" receipt ""))
             (error 'storage-integrity-error
                    :operation :conversation-episode-graph-sync
                    :detail "tail receipt belongs to a different event store"))
           (push (%storage-json-read
                  (gethash "event_json" receipt "")
                  :conversation-episode-graph-sync)
                 events))
         :agent-id agent-id :after-position after-position
         :through-position (gethash "through_storage_position" boundary)
         :event-types '("conversation-episode-sealed"))
      (declare (ignore ignored-last))
      (unless complete-p
        (error 'storage-unavailable-error
               :operation :conversation-episode-graph-sync
               :detail "bounded episode receipt stream did not complete"))
      (values (nreverse events) count))))

(defun %ceg-sync-cold (event-backend derived-backend agent-id persona-id
                       boundary fallback-reason)
  (multiple-value-bind (events count)
      (%ceg-sync-events event-backend agent-id 0 boundary)
    (let* ((episodes (conversation-episode-project
                      events agent-id persona-id))
           (materialization
             (conversation-episode-graph-materialization
              episodes agent-id persona-id))
           (persist
             (conversation-episode-graph-persist
              derived-backend materialization
              :through-event-id (gethash "through_event_id" boundary)
              :through-position
              (gethash "through_storage_position" boundary)
              :event-storage-id (gethash "storage_id" boundary)
              :boundary-hash (gethash "source_binding" boundary))))
      (obj "schema_version" 1 "status" "synchronized"
           "mode" "cold-rebuild"
           "fallback_reason" (or fallback-reason :null)
           "episode_event_count" count
           "episode_count" (length episodes)
           "through_event_id" (gethash "through_event_id" boundary)
           "through_storage_position"
           (gethash "through_storage_position" boundary)
           "node_count" (gethash "node_count" persist)
           "edge_count" (gethash "edge_count" persist)
           "evidence_count" (gethash "evidence_count" persist)
           "event_write_count" 0 "memory_write_count" 0))))

(defun %ceg-sync-tail (event-backend derived-backend agent-id persona-id
                       boundary)
  (multiple-value-bind (checkpoint prior)
      (%cegs-checkpoint-generation
       derived-backend agent-id persona-id (gethash "storage_id" boundary))
    (let* ((old-event (gethash "through_event_id" checkpoint))
           (old-position (gethash "through_storage_position" checkpoint))
           (old-binding
             (storage-checkpoint-source-binding
              event-backend :agent-id agent-id
              :through-event-id old-event :through-position old-position)))
      (unless (and (<= old-position
                       (gethash "through_storage_position" boundary))
                   (<= old-event (gethash "through_event_id" boundary))
                   (string= old-binding (gethash "boundary_hash" prior "")))
        (error 'storage-integrity-error
               :operation :conversation-episode-graph-sync
               :detail "graph checkpoint is not a verified ledger prefix"))
      (multiple-value-bind (tail count)
          (%ceg-sync-events event-backend agent-id old-position boundary)
        (let* ((changed
                 (remove-duplicates
                  (loop for event in tail
                        for payload = (gethash "payload" event)
                        when (and (hash-table-p payload)
                                  (string= persona-id
                                           (gethash "persona_id" payload "")))
                          collect (gethash "episode_id" payload))
                  :test #'string=))
               (tail-episodes
                 (conversation-episode-project tail agent-id persona-id))
               (materialization
                 (conversation-episode-graph-materialization
                  tail-episodes agent-id persona-id))
               (persist
                 (conversation-episode-graph-persist-tail
                  derived-backend materialization changed
                  :through-event-id (gethash "through_event_id" boundary)
                  :through-position
                  (gethash "through_storage_position" boundary)
                  :event-storage-id (gethash "storage_id" boundary)
                  :boundary-hash (gethash "source_binding" boundary))))
          (obj "schema_version" 1 "status" "synchronized"
               "mode" "incremental-tail" "fallback_reason" :null
               "episode_event_count" count
               "changed_episode_count" (length changed)
               "episode_count" (gethash "episode_count" persist)
               "through_event_id" (gethash "through_event_id" boundary)
               "through_storage_position"
               (gethash "through_storage_position" boundary)
               "node_count" (gethash "node_count" persist)
               "edge_count" (gethash "edge_count" persist)
               "evidence_count" (gethash "evidence_count" persist)
               "full_generation_rows_read"
               (gethash "full_generation_rows_read" persist)
               "event_write_count" 0 "memory_write_count" 0))))))

(defun conversation-episode-graph-synchronize
    (event-backend derived-backend agent-id persona-id)
  "Synchronize KG1 to one captured ledger generation.

Any absent, foreign, corrupt, non-prefix or revision-stale derived generation
falls back to a full bounded rebuild.  Events appended after the captured
boundary are intentionally left for the next call."
  (%cegs-required-string agent-id "agent-id")
  (%cegs-required-string persona-id "persona-id")
  (let ((boundary (storage-authority-boundary
                   event-backend :agent-id agent-id)))
    (handler-case
        (%ceg-sync-tail event-backend derived-backend agent-id persona-id
                        boundary)
      (storage-error (condition)
        (%ceg-sync-cold
         event-backend derived-backend agent-id persona-id boundary
         (string-downcase (symbol-name (type-of condition))))))))
