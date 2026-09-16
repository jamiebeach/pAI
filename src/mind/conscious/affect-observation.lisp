;;;; Q5A0: analytic instrument, not disposition or behavior authority.
(in-package :agent)

(export '(conscious-affect-tool-observation conscious-affect-observation-report))

(defun conscious-affect-tool-observation (outcome mind-identity-id)
  "Runtime-owned boundary evidence. RETURNED does not mean task success.
The enclosing durable tool-result supplies event identity, time and provenance."
  (unless (and (stringp mind-identity-id) (< 0 (length mind-identity-id) 256)
               (member outcome '("returned" "raised-error" "refused" "suppressed" "unavailable")
                       :test #'equal))
    (return-from conscious-affect-tool-observation nil))
  (obj "schema_version" 1 "policy" "tool-boundary-observation-v1"
       "mind_identity_id" mind-identity-id "disclosure" "private"
       "provenance" "runtime-boundary" "boundary_outcome" outcome
       "coping_evidence" (cond ((equal outcome "unavailable") "means-unavailable")
                               ((equal outcome "raised-error") "execution-obstructed")
                               ((member outcome '("refused" "suppressed") :test #'equal)
                                "not-attempted")
                               (t "unassessed"))
       "certainty" "unassessed" "agency" "unassessed"))

(defun conscious-affect-observation-report (events agent-id mind-identity-id)
  "Inspect a caller-supplied event window, without I/O or changing any state.
No prose is interpreted. Missing historical instrumentation stays unassessed.
Partition authorization belongs to the caller; this adds exact agent/mind filters."
  (let ((rows nil) (seen (make-hash-table :test #'equal)) (invalid 0)
        (unattributed 0) (other-mind 0))
    (dolist (event events)
      (when (and (hash-table-p event)
                 (equal agent-id (gethash "agent_id" event))
                 (equal "recursive-tool-result" (gethash "type" event)))
        (let* ((payload (gethash "payload" event))
               (observation (and (hash-table-p payload)
                                 (gethash "affect_observation" payload)))
               (id (gethash "id" event)))
          ;; Coverage counts are over the supplied agent window. Missing rows
          ;; cannot truthfully be assigned to a particular mind.
          (unless (and (integerp id) (plusp id) (gethash id seen))
            (when (and (integerp id) (plusp id)) (setf (gethash id seen) t))
            (cond
              ((or (not (hash-table-p observation))
                   (not (stringp (gethash "mind_identity_id" observation))))
               (incf unattributed))
              ((not (equal mind-identity-id (gethash "mind_identity_id" observation)))
               (incf other-mind))
              (t
               (let* ((outcome (gethash "boundary_outcome" observation))
                      (expected (conscious-affect-tool-observation outcome mind-identity-id))
                      (status (gethash "execution_status" payload))
                      (valid (and expected (equalp expected observation)
                                  (or (and (equal status "executed")
                                           (member outcome '("returned" "raised-error") :test #'equal))
                                      (and (equal status "refused") (equal outcome "unavailable"))
                                      (equal status outcome))
                                  (integerp id) (plusp id))))
                 (cond ((not valid) (incf invalid))
                       (t
                        (push (obj "source_event_id" id
                                   "observed_at" (gethash "timestamp" event :null)
                                   "thread_id" (gethash "thread_id" payload :null)
                                   "tool_name" (gethash "tool_name" payload :null)
                                   "boundary_outcome" outcome
                                   "coping_evidence"
                                   (let ((process (gethash "process_outcome" payload)))
                                     (if (and (equal outcome "returned")
                                              (equal "bash" (gethash "tool_name" payload))
                                              (hash-table-p process)
                                              (= 2 (hash-table-count process))
                                              (equal "process-exit" (gethash "kind" process))
                                              (typep (gethash "exit_code" process) '(integer 0 255)))
                                         (if (zerop (gethash "exit_code" process))
                                             "process-completed" "process-failed")
                                         (gethash "coping_evidence" expected)))
                                   "process_exit_code"
                                   (let ((process (gethash "process_outcome" payload)))
                                     (if (hash-table-p process)
                                         (gethash "exit_code" process :null) :null)))
                              rows))))))))))
    (obj "policy" "tool-boundary-observation-v1" "mode" "instrument-only"
         "agent_id" agent-id "mind_identity_id" mind-identity-id
         "observations" (coerce (nreverse rows) 'vector)
         "invalid_observation_count" invalid
         "unattributed_agent_window_count" unattributed
         "other_mind_count" other-mind
         "coverage" (cond ((or (plusp invalid) (plusp unattributed)) "incomplete")
                          ((null rows) "no-observations")
                          (t "observed-window"))
         "disposition" :null "context_injection" :null)))
