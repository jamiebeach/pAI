;;;; conscious-captured-runtime-tests.lisp -- Q4 two-step captured deliberation.
;;;; Written before the durable captured adapter existed; retain red evidence.

(in-package :agent)

(ql:quickload '(:bordeaux-threads :shasht) :silent t)

(defvar *ccr-passed* 0)
(defvar *ccr-failed* 0)
(defvar *ccr-events* '())
(defvar *ccr-provider-calls* 0)
(defvar *ccr-effect-calls* 0)
(defvar *ccr-publication-calls* 0)
(defvar *ccr-drop-types* '())
(defvar *ccr-log-receipt-p* nil)
(defvar *ccr-replay-count* 0)

(defun ccr-check (name condition)
  (if condition
      (progn (incf *ccr-passed*) (format t "PASS ~a~%" name))
      (progn (incf *ccr-failed*) (format t "FAIL ~a~%" name))))

(defun ccr-signals-p (thunk)
  (handler-case (progn (funcall thunk) nil) (error () t)))

(defun replay-events (&rest ignored)
  (declare (ignore ignored))
  (incf *ccr-replay-count*)
  *ccr-events*)
(defun log-event (type payload &key caused-by)
  (let* ((id (1+ (length *ccr-events*)))
         (event (obj "id" id "type" type "agent_id" "q4-dev"
                     "caused_by" (or caused-by :null) "payload" payload))
         (persisted-p (not (member type *ccr-drop-types* :test #'string=))))
    (when persisted-p
      (setf *ccr-events*
            (append *ccr-events* (list event))))
    (if *ccr-log-receipt-p*
        (values id persisted-p (and persisted-p event))
        id)))
(defun projection-context-p (value) (hash-table-p value))
(defun conscious-state-project (events &key context)
  (declare (ignore context))
  (let ((revision 0))
    (dolist (event events)
      (when (string= "pulse-committed" (gethash "type" event ""))
        (setf revision
              (max revision
                   (gethash "pulse_sequence" (gethash "payload" event) 0)))))
    (obj "state_revision" revision "observation_revision" (length events)
         "composition_hash" "state-composition"
         "focus" (obj "value" (obj "evidence_ids" (vector 10))))))

(defun raw-call-model (&rest ignored)
  (declare (ignore ignored)) (incf *ccr-provider-calls*))
(defun execute (&rest ignored)
  (declare (ignore ignored)) (incf *ccr-effect-calls*))
(defun publish (&rest ignored)
  (declare (ignore ignored)) (incf *ccr-publication-calls*))

(defun ccr-record (id content) (obj "source_id" id "content" content))
(defun ccr-assembly-spec ()
  (obj
   "audience" "operator" "total_character_budget" 512
   "section_character_budgets"
   (obj "identity-instructions" 80 "sensorium" 80
        "focus-lifecycles" 80 "triggering-stimuli" 120
        "conversation-evidence" 40 "memory-bundles" 40
        "untrusted-tool-results" 40 "tools-proposal-schema" 80
        "publication-constraints" 80)
   "sections"
   (obj "identity-instructions" (vector (ccr-record "policy:1" "fixture policy"))
        "sensorium" (vector (ccr-record "sensor:1" "runtime healthy"))
        "focus-lifecycles" (vector (ccr-record "focus:1" "respond"))
        "triggering-stimuli" (vector (ccr-record 10 "fixture current message"))
        "conversation-evidence" (vector)
        "memory-bundles" (vector)
        "untrusted-tool-results" (vector)
        "tools-proposal-schema" (vector (ccr-record "schema:1" "JSON proposals"))
        "publication-constraints" (vector (ccr-record "publication:1" "candidate only")))
   "eligible_evidence_ids" (vector "policy:1" "sensor:1" "focus:1" 10
                                   "schema:1" "publication:1")
   "available_tools" (vector "inspect-state")
   "permitted_proposal_kinds"
   (vector "tool-call-proposal" "publication-candidate" "yield" "abstain")
   "publication_constraints" (obj "audiences" (vector "operator"))
   "remaining_budget" (obj "tool_proposals" 1 "continuations" 0
                            "publication_candidates" 1)))

(defun ccr-row (manifest kind payload &optional (evidence (vector 10)) (index 1))
  (let ((pulse-id (gethash "pulse_id" manifest)))
    (obj "proposal_id" (format nil "~a:proposal:~d" pulse-id index)
         "pulse_id" pulse-id
         "runtime_revision" (gethash "runtime_revision" manifest)
         "conscious_state_revision"
         (gethash "conscious_state_revision" manifest)
         "kind" kind "created_at_stage" "model-deliberation"
         "confidence" 0.8d0 "evidence_event_ids" evidence
         "payload" payload)))

(defun ccr-response (&rest rows)
  (obj "schema_version" 1 "proposals" (coerce rows 'vector)))

(defun ccr-reset ()
  (setf *ccr-events* (list (obj "id" 10 "type" "user-message"
                                "agent_id" "q4-dev" "payload" (obj)))
        *ccr-provider-calls* 0 *ccr-effect-calls* 0
        *ccr-publication-calls* 0
        *ccr-drop-types* '()
        *ccr-log-receipt-p* nil *ccr-replay-count* 0
        *conscious-pulse-runtime-in-flight* nil
        *conscious-pulse-runtime-pending-deliberation* nil
        *conscious-pulse-runtime-last-plan* nil
        *conscious-pulse-runtime-last-terminal-id* nil
        *conscious-pulse-runtime-last-error* nil))

(format t "~%== Q4 captured runtime subject ==~%")
(let ((path (merge-pathnames "src/mind/conscious/pulse-runtime.lisp" *pai-root*)))
  (load (test-source "proposal.lisp"))
  (load (test-source "context-assembly.lisp"))
  (load (test-source "pulse.lisp"))
  (load path)
  (ccr-check "captured deliberation open entry exists"
             (fboundp 'conscious-pulse-runtime-open-captured))
  (ccr-check "captured deliberation submit entry exists"
             (fboundp 'conscious-pulse-runtime-submit-captured))
  (ccr-check "provider failure can terminalize an opened pulse"
             (fboundp 'conscious-pulse-runtime-fail-captured))
  (when (and (fboundp 'conscious-pulse-runtime-open-captured)
             (fboundp 'conscious-pulse-runtime-submit-captured))
    (ccr-reset)
    (setf *ccr-log-receipt-p* t)
    (let* ((state (conscious-state-project *ccr-events* :context (obj)))
           (assembled
             (conscious-pulse-runtime-open-captured
              state :projection-context (obj) :agent-id "q4-dev"
              :runtime-revision "conscious-q4-test" :purpose :respond
              :now 2000 :clock-identity "fixture-clock"
              :assembly-spec (ccr-assembly-spec)
              :event-snapshot *ccr-events*))
           (manifest (gethash "manifest" assembled))
           (private-request (gethash "private_request" assembled)))
      (ccr-check "open is durable before the private request is returned"
                 (= 1 (count "pulse-opened" *ccr-events*
                             :key (lambda (event) (gethash "type" event))
                             :test #'string=)))
      (ccr-check "open exposes bounded private request and safe manifest"
                 (and (plusp (length private-request))
                      (null (search "fixture current message"
                                    (shasht:write-json manifest nil)))))
      (ccr-check "a second pulse cannot open while deliberation is pending"
                 (ccr-signals-p
                  (lambda ()
                    (conscious-pulse-runtime-open-captured
                     state :projection-context (obj) :agent-id "q4-dev"
                     :runtime-revision "conscious-q4-test" :purpose :respond
                     :now 2000 :clock-identity "fixture-clock"
                     :assembly-spec (ccr-assembly-spec)))))
      (let* ((publication
               (ccr-row manifest "publication-candidate"
                        (obj "audience" "operator" "channel_class" "diagnostic"
                             "speech_act" "answer"
                             "content" "captured candidate content"
                             "evidence_event_ids" (vector 10)
                             "reason_to_speak_now" "direct-response")))
             (result
               (multiple-value-list
                (conscious-pulse-runtime-submit-captured
                 (ccr-response publication))))
             (plan (first result))
             (projected (second result))
             (commit (find "pulse-committed" *ccr-events*
                           :key (lambda (event) (gethash "type" event))
                           :test #'string=)))
        (ccr-check "validated publication handles its triggering stimulus"
                   (and commit (string= "completed" (gethash "status" plan))
                        (= 1 (gethash "state_revision" projected))
                        (equalp (vector "stimulus:10")
                                (gethash "consumed_stimulus_ids" plan))
                        (string= "handled" (gethash "disposition" plan))))
        (ccr-check "captured pulse records the full declared stage path"
                   (equalp (vector "selecting" "assembling" "deliberating"
                                   "validating" "committing" "completed")
                           (gethash "stage_history" plan)))
        (ccr-check "receipted captured open avoids replay and terminal projects once"
                   (= 1 *ccr-replay-count*))
        (ccr-check "safe report never exposes committed proposal content"
                   (null (search "captured candidate content"
                                 (shasht:write-json
                                  (conscious-pulse-runtime-report) nil))))
        (ccr-check "publication candidate causes no delivery or effect"
                   (and (zerop *ccr-provider-calls*)
                        (zerop *ccr-effect-calls*)
                        (zerop *ccr-publication-calls*)))))

    (format t "~%== all-or-nothing captured rejection ==~%")
    (ccr-reset)
    (let* ((state (conscious-state-project *ccr-events* :context (obj)))
           (assembled
             (conscious-pulse-runtime-open-captured
              state :projection-context (obj) :agent-id "q4-dev"
              :runtime-revision "conscious-q4-test" :purpose :respond
              :now 2000 :clock-identity "fixture-clock"
              :assembly-spec (ccr-assembly-spec)))
           (manifest (gethash "manifest" assembled)))
      (ccr-check "invalid member rejects and terminalizes the whole pulse"
                 (ccr-signals-p
                  (lambda ()
                    (conscious-pulse-runtime-submit-captured
                     (ccr-response
                      (ccr-row manifest "yield" (obj))
                      (ccr-row manifest "unknown-effect" (obj) (vector) 2))))))
      (ccr-check "rejected capture writes failure and no commit"
                 (and (= 1 (count "pulse-failed" *ccr-events*
                                  :key (lambda (event) (gethash "type" event))
                                  :test #'string=))
                      (null (find "pulse-committed" *ccr-events*
                                  :key (lambda (event) (gethash "type" event))
                                  :test #'string=))
                      (null *conscious-pulse-runtime-pending-deliberation*))))

    (format t "~%== inert tool and explicit silence ==~%")
    (dolist (case '("tool-call-proposal" "yield"))
      (ccr-reset)
      (let* ((state (conscious-state-project *ccr-events* :context (obj)))
             (assembled
               (conscious-pulse-runtime-open-captured
                state :projection-context (obj) :agent-id "q4-dev"
                :runtime-revision "conscious-q4-test" :purpose :respond
                :now 2000 :clock-identity "fixture-clock"
                :assembly-spec (ccr-assembly-spec)))
             (manifest (gethash "manifest" assembled))
             (payload (if (string= case "tool-call-proposal")
                          (obj "tool_name" "inspect-state" "arguments" (obj))
                          (obj)))
             (plan (conscious-pulse-runtime-submit-captured
                    (ccr-response (ccr-row manifest case payload
                                           (if (string= case "yield")
                                               (vector) (vector 10)))))))
        (ccr-check (format nil "captured ~a is a successful inert outcome" case)
                   (string= "completed" (gethash "status" plan)))))
    (ccr-check "tool proposal never dispatches"
               (and (zerop *ccr-effect-calls*) (zerop *ccr-provider-calls*)
                    (zerop *ccr-publication-calls*)))

    (format t "~%== truthful provider accounting ==~%")
    (ccr-reset)
    (let* ((state (conscious-state-project *ccr-events* :context (obj)))
           (assembled
             (conscious-pulse-runtime-open-captured
              state :projection-context (obj) :agent-id "q4-dev"
              :runtime-revision "conscious-q4-test" :purpose :respond
              :now 2000 :clock-identity "fixture-clock"
              :assembly-spec (ccr-assembly-spec) :model-call-budget 1))
           (manifest (gethash "manifest" assembled))
           (plan
             (conscious-pulse-runtime-submit-captured
              (ccr-response (ccr-row manifest "yield" (obj) (vector)))
              :model-calls 1)))
      (ccr-check "real provider use is recorded on the committed pulse"
                 (= 1 (gethash "model_calls" plan))))
    (ccr-reset)
    (conscious-pulse-runtime-open-captured
     (conscious-state-project *ccr-events* :context (obj))
     :projection-context (obj) :agent-id "q4-dev"
     :runtime-revision "conscious-q4-test" :purpose :respond
     :now 2000 :clock-identity "fixture-clock"
     :assembly-spec (ccr-assembly-spec))
    (when (fboundp 'conscious-pulse-runtime-fail-captured)
      (conscious-pulse-runtime-fail-captured "provider-call-failed")
      (ccr-check "provider failure writes one terminal and releases the slot"
                 (and (= 1 (count "pulse-failed" *ccr-events*
                                  :key (lambda (event) (gethash "type" event))
                                  :test #'string=))
                      (null *conscious-pulse-runtime-pending-deliberation*)
                      (null *conscious-pulse-runtime-in-flight*))))

    (format t "~%== open failure and immutable snapshot ==~%")
    (ccr-reset)
    (let ((bad (ccr-assembly-spec)))
      (remhash "remaining_budget" bad)
      (ccr-check "pre-open spec rejection does not wedge the runtime"
                 (and (ccr-signals-p
                       (lambda ()
                         (conscious-pulse-runtime-open-captured
                          (conscious-state-project *ccr-events* :context (obj))
                          :projection-context (obj) :agent-id "q4-dev"
                          :runtime-revision "conscious-q4-test" :purpose :respond
                          :now 2000 :clock-identity "fixture-clock"
                          :assembly-spec bad)))
                      (null *conscious-pulse-runtime-in-flight*)
                      (null *conscious-pulse-runtime-pending-deliberation*))))
    (ccr-reset)
    (setf *ccr-drop-types* '("pulse-opened"))
    (ccr-check "unreadable open append releases the local in-flight slot"
               (and (ccr-signals-p
                     (lambda ()
                       (conscious-pulse-runtime-open-captured
                        (conscious-state-project *ccr-events* :context (obj))
                        :projection-context (obj) :agent-id "q4-dev"
                        :runtime-revision "conscious-q4-test" :purpose :respond
                        :now 2000 :clock-identity "fixture-clock"
                        :assembly-spec (ccr-assembly-spec))))
                    (null *conscious-pulse-runtime-in-flight*)
                    (null *conscious-pulse-runtime-pending-deliberation*)))
    (ccr-reset)
    (let ((bad (ccr-assembly-spec)))
      (setf (gethash "eligible_evidence_ids" bad) (vector 10))
      (ccr-check "post-open assembly rejection terminalizes and clears"
                 (and (ccr-signals-p
                       (lambda ()
                         (conscious-pulse-runtime-open-captured
                          (conscious-state-project *ccr-events* :context (obj))
                          :projection-context (obj) :agent-id "q4-dev"
                          :runtime-revision "conscious-q4-test" :purpose :respond
                          :now 2000 :clock-identity "fixture-clock"
                          :assembly-spec bad)))
                      (= 1 (count "pulse-failed" *ccr-events*
                                  :key (lambda (event) (gethash "type" event))
                                  :test #'string=))
                      (null *conscious-pulse-runtime-in-flight*))))
    (ccr-reset)
    (let* ((state (conscious-state-project *ccr-events* :context (obj)))
           (assembled
             (conscious-pulse-runtime-open-captured
              state :projection-context (obj) :agent-id "q4-dev"
              :runtime-revision "conscious-q4-test" :purpose :respond
              :now 2000 :clock-identity "fixture-clock"
              :assembly-spec (ccr-assembly-spec)))
           (manifest (gethash "manifest" assembled)))
      (setf (gethash "state_revision" state) 99)
      (let ((plan
              (conscious-pulse-runtime-submit-captured
               (ccr-response (ccr-row manifest "yield" (obj) (vector))))))
        (ccr-check "pending deliberation retains an immutable state snapshot"
                   (zerop (gethash "conscious_state_revision" plan)))))

    (format t "~%== pending recovery ==~%")
    (ccr-reset)
    (conscious-pulse-runtime-open-captured
     (conscious-state-project *ccr-events* :context (obj))
     :projection-context (obj) :agent-id "q4-dev"
     :runtime-revision "conscious-q4-test" :purpose :respond
     :now 2000 :clock-identity "fixture-clock"
     :assembly-spec (ccr-assembly-spec))
    (ccr-check "recovery terminalizes and clears pending captured deliberation"
               (and (= 1 (conscious-pulse-runtime-recover
                           :agent-id "q4-dev"
                           :runtime-revision "conscious-q4-test"))
                    (null *conscious-pulse-runtime-pending-deliberation*)
                    (null *conscious-pulse-runtime-in-flight*)))

    (format t "~%== generic cognitive work pulse lineage ==~%")
    (ccr-reset)
    (let ((assembled
            (handler-case
                (conscious-pulse-runtime-open-captured
                 (conscious-state-project *ccr-events* :context (obj))
                 :projection-context (obj) :agent-id "q4-dev"
                 :runtime-revision "conscious-q4-test" :purpose :continue-work
                 :now 2000 :clock-identity "fixture-clock"
                 :assembly-spec (ccr-assembly-spec) :model-call-budget 1
                 :work-id "work:fixture" :parent-pulse-id "pulse:prior")
              (error () nil))))
      (ccr-check "captured pulse accepts generic work and parent lineage"
                 (hash-table-p assembled))
      (when (hash-table-p assembled)
        (let* ((manifest (gethash "manifest" assembled))
               (plan
                 (conscious-pulse-runtime-submit-captured
                  (ccr-response (ccr-row manifest "yield" (obj) (vector)))
                  :model-calls 1))
               (commit
                 (find "pulse-committed" *ccr-events*
                       :key (lambda (event) (gethash "type" event))
                       :test #'string=))
               (payload (and commit (gethash "payload" commit))))
          (declare (ignore plan))
          (ccr-check "committed pulse preserves generic work lineage"
                     (and (string= "work:fixture"
                                   (gethash "work_id" payload ""))
                          (string= "pulse:prior"
                                   (gethash "parent_pulse_id" payload "")))))))))

(format t "~%~d passed, ~d failed~%" *ccr-passed* *ccr-failed*)
(when (plusp *ccr-failed*) (uiop:quit 1))
