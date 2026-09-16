;;;; conscious-lifecycle-semantics-tests.lisp -- Q5S semantic replay/disclosure.
;;;;
;;;; Failing-first probe: authored against the accepted final-consumer contract
;;;; before lifecycle-semantics.lisp existed.

(in-package :agent)

(defvar *clst-passed* 0)
(defvar *clst-failed* 0)

(defun clst-check (name condition)
  (if condition
      (progn (incf *clst-passed*) (format t "PASS ~a~%" name))
      (progn (incf *clst-failed*) (format t "FAIL ~a~%" name))))

(defun clst-signals-p (thunk)
  (handler-case (progn (funcall thunk) nil) (error () t)))

(defun clst-event (id type payload &optional (agent-id "semantic-dev"))
  (obj "schema_version" 1 "id" id "timestamp" (+ 1000 id)
       "type" type "agent_id" agent-id "caused_by" :null
       "payload" payload))

(defun clst-lifecycle-source (id intention-id state)
  (clst-event id
              (if (string= state "seeded")
                  "near-term-intention-created"
                  "near-term-intention-transition")
              (obj "schema_version" 1 "intention_id" intention-id
                   "receipt_id" (format nil "receipt:~a" intention-id)
                   "state" state "pass_count" 0 "detail" :null)))

(defun clst-transition (id source-id transition &optional (reason "near-term-seeded"))
  (clst-event
   id "conscious-lifecycle-transition"
   (conscious-lifecycle-transition-payload
    "near-term:intention:1" transition
    :request-id (format nil "transition:~a" id)
    :lifecycle-kind "deferred-intention"
    :origin-runtime-revision "near-term-intentions-v1"
    :actor-runtime-revision "conscious-q5-v2"
    :source-event-id source-id
    :checkpoint-ref (if (string= transition "checkpoint") "receipt:intention:1" :null)
    :reason-code reason :occurred-at (+ 1000 id))))

(format t "~%== Q5S semantic lifecycle context ==~%")

(let ((path (merge-pathnames "src/mind/conscious/lifecycle-semantics.lisp"
                             *pai-root*)))
  (clst-check "semantic lifecycle module exists" (probe-file path))
  (when (probe-file path)
    (load (merge-pathnames "src/mind/conscious/lifecycle.lisp" *pai-root*))
    (load path)
    (let* ((source (clst-lifecycle-source 1 "intention:1" "seeded"))
           (open (clst-transition 2 1 "open"))
           (payload
             (conscious-lifecycle-semantic-payload
              "semantic:intention:1" 1 :null
              "near-term:intention:1" "deferred-intention" "persona:dev"
              "topic" "doorbell interruption"
              (vector "topic:doorbell") "resume the interrupted conversation"
              :result-summary :null :result-receipt-event-id :null
              :source-revision "near-term-intentions-v1"
              :actor-runtime-revision "conscious-q5-v2"
              :disclosure-policy-ref "lifecycle-semantic-disclosure-v1"
              :disclosure-class "private-provider-eligible"
              :confidence "asserted" :staleness "current"
              :supporting-event-ids (vector 1)))
           (semantic (clst-event 3 "conscious-lifecycle-semantic-described" payload))
           (events (list source open semantic))
           (projection (conscious-lifecycle-semantic-project
                        events :agent-id "semantic-dev"))
           (current (conscious-lifecycle-semantic-current
                     projection "near-term:intention:1" "persona:dev")))
      (clst-check "S1 descriptor replays from append-only events"
                  (and current
                       (string= "doorbell interruption"
                                (gethash "subject_label" current))
                       (string= "resume the interrupted conversation"
                                (gethash "intended_outcome" current))))
      (clst-check "S2 descriptor carries bounded fields but no raw detail"
                  (and (= 0 (length (gethash "invalid_event_ids" projection)))
                       (null (nth-value 1 (gethash "detail" current)))
                       (null (search "ignore prior instructions"
                                     (gethash "subject_label" current)))))
      (multiple-value-bind (records refusals evidence)
          (conscious-lifecycle-semantic-context-records
           (vector (obj "lifecycle_id" "near-term:intention:1"
                        "lifecycle_kind" "deferred-intention" "status" "active"
                        "phase" "near-term-seeded" "checkpoint_ref" :null
                        "last_event_id" 2))
           projection :mind-identity-id "persona:dev" :purpose "respond"
           :audience "operator" :channel "terminal" :provider-class "remote-zdr")
        (clst-check "S1 final lifecycle record contains subject and outcome"
                    (and (= 1 (length records))
                         (search "doorbell interruption"
                                 (gethash "content" (aref records 0)))
                         (search "resume the interrupted conversation"
                                 (gethash "content" (aref records 0)))))
        (clst-check "descriptor provenance exposes content-free evidence IDs"
                    (and (= 0 (length refusals))
                         (member 1 (coerce evidence 'list))
                         (member 3 (coerce evidence 'list))
                         (hash-table-p
                          (gethash "provenance" (aref records 0))))))
      (multiple-value-bind (records refusals evidence)
          (conscious-lifecycle-semantic-context-records
           (vector (obj "lifecycle_id" "near-term:intention:1"
                        "lifecycle_kind" "deferred-intention" "status" "active"
                        "phase" "near-term-seeded" "checkpoint_ref" :null
                        "last_event_id" 2))
           projection :mind-identity-id "persona:other" :purpose "respond"
           :audience "operator" :channel "terminal" :provider-class "remote-zdr")
        (declare (ignore evidence))
        (clst-check "mind identity mismatch cannot disclose semantics"
                    (and (not (search "doorbell interruption"
                                      (gethash "content" (aref records 0))))
                         (= 1 (length refusals))
                         (string= "mind-identity-mismatch"
                                  (gethash "reason" (aref refusals 0))))))
      (let* ((local-payload
               (conscious-lifecycle-semantic-payload
                "semantic:intention:local" 1 :null
                "near-term:intention:1" "deferred-intention" "persona:dev"
                "topic" "private local subject" (vector) "consider privately"
                :result-summary :null :result-receipt-event-id :null
                :source-revision "near-term-intentions-v1"
                :actor-runtime-revision "conscious-q5-v3"
                :disclosure-policy-ref "lifecycle-semantic-disclosure-v1"
                :disclosure-class "local-only" :confidence "asserted"
                :staleness "current" :supporting-event-ids (vector 1)))
             (local-projection
               (conscious-lifecycle-semantic-project
                (list source open
                      (clst-event 4 "conscious-lifecycle-semantic-described"
                                  local-payload))
                :agent-id "semantic-dev")))
        (multiple-value-bind (records refusals ignored)
            (conscious-lifecycle-semantic-context-records
             (vector (obj "lifecycle_id" "near-term:intention:1"
                          "lifecycle_kind" "deferred-intention" "status" "active"
                          "phase" "near-term-seeded" "checkpoint_ref" :null
                          "last_event_id" 2))
             local-projection :mind-identity-id "persona:dev" :purpose "respond"
             :audience "operator" :channel "terminal" :provider-class "remote-zdr")
          (declare (ignore ignored))
          (clst-check "S4 local-only semantics are withheld from remote provider"
                      (and (not (search "private local subject"
                                        (gethash "content" (aref records 0))))
                           (string= "disclosure-policy-refused"
                                    (gethash "reason" (aref refusals 0)))))))
      (let* ((tampered (shasht:read-json (shasht:write-json payload nil))))
        (setf (gethash "subject_label" tampered) "tampered")
        (let ((bad (conscious-lifecycle-semantic-project
                    (list source open
                          (clst-event 5 "conscious-lifecycle-semantic-described"
                                      tampered))
                    :agent-id "semantic-dev")))
          (clst-check "S6 integrity mismatch leaves semantics unavailable"
                      (and (null (conscious-lifecycle-semantic-current
                                  bad "near-term:intention:1" "persona:dev"))
                           (= 1 (length (gethash "invalid_event_ids" bad)))))))
      (let* ((ready-source (clst-lifecycle-source 4 "intention:1" "ready"))
             (checkpoint (clst-transition 5 4 "checkpoint" "near-term-ready"))
             (result-payload
               (conscious-lifecycle-semantic-payload
                "semantic:intention:1" 2 3
                "near-term:intention:1" "deferred-intention" "persona:dev"
                "topic" "doorbell interruption" (vector "topic:doorbell")
                "resume the interrupted conversation"
                :result-summary "Conversation can resume at the prior question."
                :result-receipt-event-id 4
                :source-revision "near-term-intentions-v1"
                :actor-runtime-revision "conscious-q5-v3"
                :disclosure-policy-ref "lifecycle-semantic-disclosure-v1"
                :disclosure-class "private-provider-eligible"
                :confidence "asserted" :staleness "current"
                :supporting-event-ids (vector 1 4)))
             (updated-event
               (clst-event 6 "conscious-lifecycle-semantic-described"
                           result-payload))
             (updated
               (conscious-lifecycle-semantic-project
                (list source open semantic ready-source checkpoint updated-event)
                :agent-id "semantic-dev"))
             (row (conscious-lifecycle-semantic-current
                   updated "near-term:intention:1" "persona:dev")))
        (clst-check "S3 typed ready receipt admits result summary"
                    (and (= 2 (gethash "descriptor_revision" row))
                         (string= "Conversation can resume at the prior question."
                                  (gethash "result_summary" row))))
        (let* ((wrong-receipt
                 (shasht:read-json (shasht:write-json result-payload nil))))
          (setf (gethash "result_receipt_event_id" wrong-receipt) 1
                (gethash "semantic_integrity_hash" wrong-receipt)
                (conscious-lifecycle-semantic-integrity-hash wrong-receipt))
          (let ((bad (conscious-lifecycle-semantic-project
                      (list source open semantic
                            (clst-event 7 "conscious-lifecycle-semantic-described"
                                        wrong-receipt))
                      :agent-id "semantic-dev")))
            (clst-check "S3 non-ready receipt cannot admit result summary"
                        (eq :null
                            (gethash "result_summary"
                                     (conscious-lifecycle-semantic-current
                                      bad "near-term:intention:1"
                                      "persona:dev")))))))
      (let* ((replayed (conscious-lifecycle-semantic-project
                        events :agent-id "semantic-dev"))
             (row (conscious-lifecycle-semantic-current
                   replayed "near-term:intention:1" "persona:dev")))
        (clst-check "S5 actor revision does not alter replay identity"
                    (string= (gethash "semantic_integrity_hash" payload)
                             (gethash "semantic_integrity_hash" row))))
      (let* ((cancel (clst-transition 4 1 "cancel" "near-term-discarded"))
             (life (conscious-lifecycle-project
                    (list source open semantic cancel)
                    :agent-id "semantic-dev")))
        (clst-check "S7 cancellation removes lifecycle from active context"
                    (and (= 0 (length (conscious-lifecycle-awaiting life)))
                         (conscious-lifecycle-semantic-current
                          projection "near-term:intention:1" "persona:dev"))))
      (let* ((bad-update
               (conscious-lifecycle-semantic-payload
                "semantic:changed" 2 3
                "near-term:intention:1" "deferred-intention" "persona:dev"
                "topic" "changed identity" (vector) "invalid update"
                :result-summary :null :result-receipt-event-id :null
                :source-revision "near-term-intentions-v1"
                :actor-runtime-revision "conscious-q5-v3"
                :disclosure-policy-ref "lifecycle-semantic-disclosure-v1"
                :disclosure-class "private-provider-eligible"
                :confidence "asserted" :staleness "current"
                :supporting-event-ids (vector 1)))
             (bad (conscious-lifecycle-semantic-project
                   (append events
                           (list (clst-event 4
                                             "conscious-lifecycle-semantic-described"
                                             bad-update)))
                   :agent-id "semantic-dev")))
        (clst-check "S9 supersession cannot change descriptor identity"
                    (and (= 1 (length (gethash "invalid_event_ids" bad)))
                         (string= "semantic:intention:1"
                                  (gethash "descriptor_id"
                                           (conscious-lifecycle-semantic-current
                                            bad "near-term:intention:1"
                                            "persona:dev"))))))
      (let ((legacy (conscious-lifecycle-semantic-project
                     (list source open) :agent-id "semantic-dev")))
        (clst-check "S10 pre-Q5S history remains semantically unknown"
                    (null (conscious-lifecycle-semantic-current
                           legacy "near-term:intention:1" "persona:dev"))))
      (let ((source-text (uiop:read-file-string path)))
        (clst-check "Q5S adds no provider effect publication or worker route"
                    (notany (lambda (needle)
                              (search needle source-text :test #'char-equal))
                            '("(raw-call-model" "(call-model" "(auto-turn"
                              "(execute" "(telegram-send" "define-init")))))))

(format t "~%~d passed, ~d failed~%" *clst-passed* *clst-failed*)
(when (plusp *clst-failed*) (error "Q5S semantic lifecycle tests failed"))
