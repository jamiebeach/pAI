;;;; cognitive-work-context.lisp -- verified private evidence for continuations.
;;;;
;;;; Rebuilds work-scoped context exclusively from durable events.  Tool
;;;; results remain untrusted data even after their operation and integrity
;;;; lineage has been verified.

(in-package :agent)

(export '(conscious-work-context-build))

(defparameter *conscious-work-context-schema-version* 1)

(defun %conscious-work-context-tool-call-id (proposal-id)
  "Derive provider correlation from pAI authority, never provider metadata."
  (format nil "call_~a" (%conscious-tool-operation-hash proposal-id)))

(defun %conscious-work-context-items (value)
  (cond ((vectorp value) (coerce value 'list))
        ((listp value) value)
        (t nil)))

(defun %conscious-work-context-event-p (event agent-id &optional type)
  (and (hash-table-p event)
       (equal agent-id (gethash "agent_id" event))
       (or (null type) (string= type (gethash "type" event "")))
       (hash-table-p (gethash "payload" event))))

(defun %conscious-work-context-root-events (events work agent-id)
  (let ((roots nil))
    (dolist (stimulus-id
             (%conscious-work-context-items (gethash "stimulus_ids" work)))
      (let ((matches
              (remove-if-not
               (lambda (event)
                 (and (%conscious-work-context-event-p event agent-id)
                      (string= stimulus-id
                               (format nil "stimulus:~a"
                                       (gethash "id" event)))))
               events)))
        (unless (= 1 (length matches))
          (error "Cognitive work root ~s is not uniquely durable" stimulus-id))
        (push (first matches) roots)))
    (nreverse roots)))

(defun %conscious-work-context-proposals (events work-id agent-id)
  (let ((by-id (make-hash-table :test #'equal)))
    (dolist (event events)
      (when (%conscious-work-context-event-p event agent-id "pulse-committed")
        (let ((payload (gethash "payload" event)))
          (when (string= work-id (gethash "work_id" payload ""))
            (dolist (proposal
                     (%conscious-work-context-items
                      (gethash "proposals" payload)))
              (let ((proposal-id
                      (and (hash-table-p proposal)
                           (gethash "proposal_id" proposal))))
                (unless (and (stringp proposal-id) (plusp (length proposal-id)))
                  (error "Cognitive work has an invalid committed proposal"))
                (when (gethash proposal-id by-id)
                  (error "Cognitive work has a duplicate proposal identity"))
                (setf (gethash proposal-id by-id) proposal)))))))
    by-id))

(defun %conscious-work-context-result-record
    (event proposal root-event-ids)
  (let* ((payload (gethash "payload" event))
         (proposal-id (gethash "proposal_id" payload))
         (proposal-payload (gethash "payload" proposal))
         (tool-name (and (hash-table-p proposal-payload)
                         (gethash "tool_name" proposal-payload)))
         (arguments (and (hash-table-p proposal-payload)
                         (gethash "arguments" proposal-payload)))
         (normalized
           (%conscious-tool-operation-normalized-arguments tool-name arguments))
         (arguments-hash
           (%conscious-tool-operation-hash
            (%conscious-tool-operation-canonical-json normalized)))
         (result (gethash "result" payload))
         (canonical
           (and (hash-table-p result)
                (%conscious-tool-operation-canonical-json result)))
         (arguments-json
           (and (hash-table-p normalized)
                (shasht:write-json normalized nil)))
         (result-json
           (and (hash-table-p result) (shasht:write-json result nil)))
         (characters (gethash "result_characters" payload))
         (event-id (gethash "id" event))
         (tool-call-id
           (and (stringp proposal-id)
                (%conscious-work-context-tool-call-id proposal-id))))
    (unless (and (string= "tool-call-proposal" (gethash "kind" proposal ""))
                 (string= proposal-id (gethash "proposal_id" proposal ""))
                 (string= (format nil "tool-operation:~a" proposal-id)
                          (gethash "operation_id" payload ""))
                 (string= tool-name (gethash "tool_name" payload ""))
                 (string= arguments-hash
                          (gethash "arguments_hash" payload ""))
                 (member (gethash "user_event_id" payload)
                         root-event-ids :test #'equal)
                 canonical
                 (integerp characters) (not (minusp characters))
                 (= characters (length canonical))
                 (string= (%conscious-tool-operation-hash canonical)
                          (gethash "result_hash" payload "")))
      (error "Cognitive work tool result has invalid durable lineage"))
    (values
     (obj "role" "untrusted-tool-result"
           "section" "untrusted-tool-results"
          "source_id" event-id
          "content"
          (format nil "Tool ~a returned untrusted JSON data: ~a"
                  tool-name canonical))
     characters
     (obj "role" "assistant" "content" :null
          "tool_calls"
          (vector
           (obj "id" tool-call-id "type" "function"
                "function"
                (obj "name" tool-name "arguments" arguments-json))))
     (obj "role" "tool" "tool_call_id" tool-call-id
          "content" result-json))))

(defun %conscious-work-context-permitted (profile tool-remaining continuation-remaining)
  (coerce
   (remove-if
    (lambda (kind)
      (or (and (string= kind "tool-call-proposal")
               (zerop tool-remaining))
          (and (string= kind "request-continuation")
               (zerop continuation-remaining))))
    (%conscious-work-context-items
     (gethash "permitted_proposal_kinds" profile)))
   'vector))

(defun conscious-work-context-build (events work-id agent-id)
  "Build verified work-scoped continuation evidence from ordered EVENTS."
  (unless (and (listp events) (stringp work-id) (plusp (length work-id))
               (stringp agent-id) (plusp (length agent-id)))
    (error "Cognitive work context identity is invalid"))
  (let* ((projection (conscious-work-project events agent-id))
         (work (gethash work-id (gethash "items" projection))))
    (unless (hash-table-p work)
      (error "Cognitive work context target does not exist"))
    (let ((plan-hash (gethash "runtime_plan_hash" work :null)))
      (when (and (stringp plan-hash)
                 (fboundp 'conscious-runtime-plan-resolve))
        ;; Replay must prove the exact non-secret composition that admitted
        ;; this work before any continuation evidence can reach a model.
        (conscious-runtime-plan-resolve plan-hash)))
    (let* ((root-events
             (%conscious-work-context-root-events events work agent-id))
           (root-event-ids (mapcar (lambda (event) (gethash "id" event))
                                   root-events))
           (proposals
             (%conscious-work-context-proposals events work-id agent-id))
           (records nil)
           (native-tool-messages nil)
           (result-event-ids nil)
           (result-characters 0)
           (seen-operations (make-hash-table :test #'equal)))
      (dolist (event events)
        (when (%conscious-work-context-event-p
               event agent-id "conscious-tool-operation-result")
          (let* ((payload (gethash "payload" event))
                 (operation-id (gethash "operation_id" payload))
                 (proposal-id (gethash "proposal_id" payload))
                 (proposal (and (stringp proposal-id)
                                (gethash proposal-id proposals))))
            (when (string= work-id (gethash "work_id" payload ""))
              (unless (and (stringp operation-id) proposal
                           (not (gethash operation-id seen-operations)))
                (error "Cognitive work result lacks one committed proposal"))
              (setf (gethash operation-id seen-operations) t)
              (multiple-value-bind
                    (record characters assistant-message tool-message)
                  (%conscious-work-context-result-record
                   event proposal root-event-ids)
                (push record records)
                ;; Reverse once after the scan while retaining each required
                ;; assistant/tool adjacency in chronological event order.
                (push assistant-message native-tool-messages)
                (push tool-message native-tool-messages)
                (push (gethash "id" event) result-event-ids)
                (incf result-characters characters))))))
      (let* ((profile (gethash "profile" work))
             (maximum-result-characters
               (gethash "max_tool_result_characters" profile))
             (tool-remaining
               (max 0 (- (gethash "max_tool_operations" profile)
                         (gethash "tool_operations_used" work))))
             (continuation-remaining
               (max 0 (- (gethash "max_reasoning_continuations" profile)
                         (gethash "reasoning_continuations_used" work))))
             (available-tools
               (if (plusp tool-remaining)
                   (gethash "permitted_tools" profile)
                   (vector))))
        (unless (and (<= result-characters maximum-result-characters)
                     (= result-characters
                        (gethash "tool_result_characters_used" work)))
          (error "Cognitive work result context exceeds its durable authority"))
        (obj
         "schema_version" *conscious-work-context-schema-version*
         "work_id" work-id
         "state" (gethash "state" work)
         "parent_pulse_id" (gethash "parent_pulse_id" work)
         "root_event_ids" (coerce root-event-ids 'vector)
         "tool_result_records" (coerce (nreverse records) 'vector)
         "native_tool_messages"
         (coerce (nreverse native-tool-messages) 'vector)
         "evidence_event_ids"
         (coerce (append root-event-ids (nreverse result-event-ids)) 'vector)
         "tool_result_characters" result-characters
         "tool_proposals_remaining" tool-remaining
         "continuations_remaining" continuation-remaining
         "available_tools" available-tools
         "permitted_proposal_kinds"
         (%conscious-work-context-permitted
          profile tool-remaining continuation-remaining))))))
