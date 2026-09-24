;;;; Explicit attention selection -> ledger working window -> native messages.
(in-package :agent)

(defun sustained-activity-revise
    (backend reference-id expected-head
     &key agent-id persona-id channel resource-id actor reason evidence-event-id
          add-root-event-id attention-state completion-state
          (completion-confidence nil confidence-p))
  "Trusted lifecycle operation; append a revision, never mutate old evidence.
Caller supplies authenticated scope and a captured global physical head. A
concurrent append or superseded reference requires reselection, not blind retry.
This API grants no model/tool authority and does not choose the next activity."
  (unless (eql expected-head (storage-head-position backend))
    (error 'storage-conflict-error :operation :activity-revise :detail "Stale authority head"))
  (let* ((event (storage-read-event backend reference-id :agent-id agent-id
                                  :event-type "sustained-activity-revised"))
         (previous (and event (gethash "payload" event))))
    (validate-activity-reference previous)
    (unless (and (equal persona-id (gethash "persona_id" previous))
                 (equal channel (gethash "channel" previous))
                 (equal resource-id (gethash "resource_id" previous)))
      (error "Activity revision scope mismatch"))
    (let ((latest (storage-latest-activity-reference
                   backend agent-id persona-id channel resource-id
                   :activity-id (gethash "activity_id" previous))))
      (unless (and latest (= reference-id (gethash "id" latest)))
        (error 'storage-conflict-error :operation :activity-revise
               :detail "Reference superseded")))
    (unless (storage-read-event backend evidence-event-id :agent-id agent-id)
      (error "Activity revision needs durable causal evidence"))
    (let ((payload (%sac-copy previous)))
      (when add-root-event-id
        (let* ((root (storage-read-event backend add-root-event-id :agent-id agent-id
                                        :event-type "user-message"))
               (body (and root (gethash "payload" root)))
               (metadata (and body (gethash "metadata" body))))
          (unless (and root (> add-root-event-id reference-id)
                       (hash-table-p metadata)
                       (equal channel (gethash "channel" body))
                       (equal persona-id (gethash "persona_id" metadata))
                       (eql reference-id (gethash "activity_reference_event_id" metadata))
                       (not (find add-root-event-id (gethash "root_event_ids" previous))))
            (error "New activity member must carry this exact admitted selection"))
          (setf (gethash "root_event_ids" payload)
                (concatenate 'vector (gethash "root_event_ids" previous)
                             (vector add-root-event-id)))))
      (setf (gethash "previous_reference_event_id" payload) reference-id
            (gethash "actor" payload) actor
            (gethash "reason" payload) reason
            (gethash "evidence_event_ids" payload)
            (remove-duplicates (if add-root-event-id
                                   (vector evidence-event-id add-root-event-id)
                                   (vector evidence-event-id))))
      (when attention-state (setf (gethash "attention_state" payload) attention-state))
      (when completion-state (setf (gethash "completion_state" payload) completion-state))
      (when confidence-p (setf (gethash "completion_confidence" payload) completion-confidence))
      (validate-activity-reference payload)
      (if (event-authority-owns-storage-p backend)
          (nth-value 2 (log-event "sustained-activity-revised" payload
                                  :expected-head expected-head :caused-by evidence-event-id))
          (storage-append-event-if-head backend expected-head "sustained-activity-revised"
                                       payload :agent-id agent-id :caused-by evidence-event-id)))))

(defun sustained-activity-validate-selection (reference-id agent-id persona-id channel)
  "Check a trusted selector before admission; never infer membership from prose."
  (unless (functionp (getf *event-authority-port* :activity-read))
    (error "Sustained activity selection requires a bounded authority read port"))
  (unless (and (integerp reference-id) (plusp reference-id))
    (error "Activity selection requires a positive reference event ID"))
  (let* ((event (event-read-event reference-id :event-type "sustained-activity-revised"))
         (payload (and event (gethash "payload" event))))
    (unless (and event (equal agent-id (gethash "agent_id" event))
                 (hash-table-p payload)
                 (equal "sustained-activity-revised" (gethash "type" event))
                 (equal persona-id (gethash "persona_id" payload))
                 (equal channel (gethash "channel" payload)))
      (error "Activity selection is missing or outside this agent/persona/channel"))
    (validate-activity-reference payload)
    reference-id))

(defun sustained-activity-for-admitted-root (event agent-id persona-id channel)
  "Reconstruct the selected previous activity at this model boundary.
The selection itself is durable metadata on the admitted root. The current
root's native transcript stays owned by recursive recovery, never duplicated."
  (let* ((payload (and event (gethash "payload" event)))
         (metadata (and (hash-table-p payload) (gethash "metadata" payload)))
         (reference-id (and (hash-table-p metadata)
                            (gethash "activity_reference_event_id" metadata))))
    (when (or (null reference-id) (eq reference-id :null))
      (return-from sustained-activity-for-admitted-root nil))
    (unless (and (equal "user-message" (gethash "type" event))
                 (equal agent-id (gethash "agent_id" event))
                 (equal channel (gethash "channel" payload))
                 (equal persona-id (gethash "persona_id" metadata)))
      (error "Activity admission scope mismatch"))
    (sustained-activity-validate-selection reference-id agent-id persona-id channel)
    (unless (< reference-id (gethash "id" event))
      (error "Activity selection must precede the admitted root"))
    (let ((frontier (gethash "max_event_id" (event-authority-report))))
      (unless (and (integerp frontier) (>= frontier (gethash "id" event)))
        (error "Activity assembly needs an exact current authority frontier"))
      (multiple-value-bind (reference rows coverage)
          (event-read-activity-context reference-id frontier)
        (unless (and (hash-table-p coverage) (equal "complete" (gethash "status" coverage)))
          (error "Sustained activity cannot fit its bounded read; compaction is required"))
        (unless (and (equal agent-id (gethash "agent_id" reference))
                     (equal persona-id (gethash "persona_id" reference))
                     (equal channel (gethash "channel" reference))
                     (not (find (gethash "id" event) (gethash "root_event_ids" reference))))
          (error "Activity read scope overlaps or differs from the admitted root"))
        (let ((packet (project-sustained-activity-context reference rows :defer-budget-p t)))
          (unless (equal "ready" (gethash "status" packet))
            (error "Sustained activity is ~a; no partial context will be sent"
                   (gethash "status" packet)))
          (setf (gethash "coverage" packet) coverage)
          packet)))))

(defun sustained-activity-native-messages (packet)
  (unless (equal "ready" (gethash "status" packet))
    (error "Only a complete activity projection can be rendered"))
  (%sac-copy
   (coerce (loop for exchange across (gethash "exchanges" packet)
                 append (coerce (gethash "messages" exchange) 'list)) 'vector)))

(defun sustained-activity-report (packet)
  (when packet
    (let ((messages (sustained-activity-native-messages packet)))
      (obj "activity_id" (gethash "activity_id" packet)
           "coverage" (%sac-copy (gethash "coverage" packet))
           "root_event_ids" (map 'vector (lambda (e) (gethash "root_event_id" e))
                                  (gethash "exchanges" packet))
           "compacted_root_event_ids" (%sac-copy (gethash "compacted_root_event_ids" packet #()))
           "compaction_policy" (gethash "compaction_policy" packet :null)
           "original_characters" (gethash "original_characters" packet)
           "rendered_characters" (gethash "rendered_characters" packet)
           "budget_unit" (gethash "budget_unit" packet)
           "maximum_characters" (gethash "maximum_characters" packet)
           "trigger_characters" (gethash "trigger_characters" packet)
           "target_characters" (gethash "target_characters" packet)
           "target_reached" (gethash "target_reached" packet :false)
           "request_budget" (gethash "request_budget" packet :null)
           "message_count" (length messages)
           "content_hash" (%ca-fnv (shasht:write-json messages nil))
           "serialized_characters" (length (shasht:write-json messages nil))))))

(defun sustained-activity-replace-dialogue (spec)
  "High bandwidth uses its exact native activity, not duplicate short history."
  (setf (gethash "conversation-evidence" (gethash "sections" spec)) #())
  (when (hash-table-p *conscious-conversation-turn-history-report*)
    (let ((report (%sac-copy *conscious-conversation-turn-history-report*)))
      (setf (gethash "superseded_by_activity" report) t
            (gethash "superseded_record_count" report) (gethash "record_count" report 0)
            (gethash "record_count" report) 0
            (gethash "rendered_characters" report) 0
            (gethash "estimated_tokens" report) 0
            *conscious-conversation-turn-history-report* report)))
  spec)


