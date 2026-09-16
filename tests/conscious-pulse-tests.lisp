;;;; conscious-pulse-tests.lisp -- Q3 deterministic pulse engine.
;;;;
;;;; Written before the subject. The first run must demonstrate the
;;;; aggregator-only failure shape (the same winner repeats with no durable
;;;; terminal consumer), then fail by name on the absent pulse engine.

(in-package :agent)

(defvar *cp-passed* 0)
(defvar *cp-failed* 0)

(defun cp-check (name condition)
  (if condition
      (progn (incf *cp-passed*) (format t "PASS ~a~%" name))
      (progn (incf *cp-failed*) (format t "FAIL ~a~%" name))))

(load (test-source "policy.lisp"))
(load (test-source "stimulus.lisp"))
(load (test-source "census.lisp"))
(load (test-source "concern.lisp"))
(load (test-source "codelets.lisp"))
(load (test-source "context.lisp"))
(load (test-source "inbox.lisp"))
(load (test-source "attention.lisp"))
(load (test-source "state.lisp"))

(defun cp-event (type id &optional payload)
  (obj "schema_version" 1 "id" id "timestamp" (+ 1000 id)
       "type" type "agent_id" "q3-dev"
       "payload" (or payload (obj)) "caused_by" :null))

(dolist (name (codelet-names)) (unregister-codelet name))
(register-codelet
 "q3-direct" 10
 (lambda (stimulus context)
   (declare (ignore context))
   (when (string= "user-message" (gethash "kind" stimulus))
     (make-assessment
      :codelet "q3-direct" :concern "operator-input"
      :stimulus-id (gethash "stimulus_id" stimulus)
      :evidence-ids (coerce (gethash "source_event_ids" stimulus) 'list)
      :priority-class "direct" :urgency "interactive"
      :explanation-code "q3-direct-address")))
 :digest "q3-direct-fixture-v1")
(register-codelet
 "q3-health" 20
 (lambda (stimulus context)
   (declare (ignore context))
   (when (string= "runtime-health" (gethash "kind" stimulus))
     (make-assessment
      :codelet "q3-health" :concern "runtime-health"
      :stimulus-id (gethash "stimulus_id" stimulus)
      :evidence-ids (coerce (gethash "source_event_ids" stimulus) 'list)
      :priority-class "critical" :urgency "background"
      :explanation-code "q3-runtime-health")))
 :digest "q3-health-fixture-v1")

(format t "~%== aggregator-only failing shape ==~%")

(let* ((events (list (cp-event "user-message" 1 (obj "text" "first"))
                     (cp-event "user-message" 2 (obj "text" "second"))))
       (context (make-projection-context
                 :now 2000 :agent-id "q3-dev"
                 :runtime-revision "conscious-q3-test"
                 :consumer "conscious-state"))
       (first (conscious-state-project events :context context))
       (again (conscious-state-project events :context context))
       (first-focus (gethash "value" (gethash "focus" first)))
       (again-focus (gethash "value" (gethash "focus" again))))
  (cp-check "aggregator baseline selects a real first focus"
            (hash-table-p first-focus))
  (cp-check "without a commit consumer the same winner repeats"
            (equalp first-focus again-focus)))

(format t "~%== Q3 subject ==~%")

(let ((pulse-path (merge-pathnames "src/mind/conscious/pulse.lisp"
                                   *pai-root*)))
  (cp-check "deterministic pulse engine exists" (probe-file pulse-path))
  (when (probe-file pulse-path)
    (load pulse-path)

    (format t "~%== deterministic planning ==~%")
    (let* ((budget (make-deterministic-pulse-budget
                    :wall-milliseconds 1000 :context-characters 4096
                    :proposals 1 :cancellation-checks 8))
           (user-state
             (conscious-state-project
              (list (cp-event "user-message" 1 (obj "text" "first")))
              :now 2000 :agent-id "q3-dev"
              :current-revision "conscious-q3-test"))
           (health-state
             (conscious-state-project
              (list (cp-event "runtime-observer-error" 3
                              (obj "source" "fixture")))
              :now 2000 :agent-id "q3-dev"
              :current-revision "conscious-q3-test"))
           (user-plan
             (conscious-pulse-plan
              user-state :pulse-id "pulse:10" :agent-id "q3-dev"
              :runtime-revision "conscious-q3-test" :purpose :respond
              :opened-at 2000 :now 2000 :clock-identity "fixture-clock"
              :budget budget))
           (health-plan
             (conscious-pulse-plan
              health-state :pulse-id "pulse:11" :agent-id "q3-dev"
              :runtime-revision "conscious-q3-test" :purpose :orient
              :opened-at 2000 :now 2000 :clock-identity "fixture-clock"
              :budget budget)))
      (cp-check "the same explicit pulse input is deterministic"
                (equalp user-plan
                        (conscious-pulse-plan
                         user-state :pulse-id "pulse:10" :agent-id "q3-dev"
                         :runtime-revision "conscious-q3-test"
                         :purpose :respond :opened-at 2000 :now 2000
                         :clock-identity "fixture-clock" :budget budget)))
      (cp-check "a user barrier abstains without claiming it handled"
                (and (string= "completed" (gethash "status" user-plan))
                     (string= "abstain"
                              (gethash "kind"
                                       (aref (gethash "proposals" user-plan) 0)))
                     (zerop (length
                             (gethash "consumed_stimulus_ids" user-plan)))))
      (cp-check "providerless health materialization proposes a state update"
                (string= "state-update"
                         (gethash "kind"
                                  (aref (gethash "proposals" health-plan) 0))))
      (cp-check "providerless health materialization consumes one exact root"
                (equalp (vector "stimulus:3")
                        (gethash "consumed_stimulus_ids" health-plan)))
      (cp-check "deterministic pulses consume no model/tool/cost budget"
                (and (zerop (gethash "model_calls" health-plan))
                     (zerop (gethash "tool_proposals" health-plan))
                     (zerop (gethash "cost_microunits" health-plan))))

      (let ((cancelled
              (conscious-pulse-plan
               health-state :pulse-id "pulse:12" :agent-id "q3-dev"
               :runtime-revision "conscious-q3-test" :purpose :orient
               :opened-at 2000 :now 2000 :clock-identity "fixture-clock"
               :budget budget :cancelled-p t)))
        (cp-check "cancellation is terminal and does not consume its trigger"
                  (and (string= "cancelled" (gethash "status" cancelled))
                       (zerop (length (gethash "proposals" cancelled)))
                       (zerop (length
                               (gethash "consumed_stimulus_ids" cancelled))))))

      (let ((expired
              (conscious-pulse-plan
               health-state :pulse-id "pulse:13" :agent-id "q3-dev"
               :runtime-revision "conscious-q3-test" :purpose :orient
               :opened-at 2000 :now 2002 :clock-identity "fixture-clock"
               :budget (make-deterministic-pulse-budget
                        :wall-milliseconds 1000 :context-characters 4096
                        :proposals 1 :cancellation-checks 8))))
        (cp-check "deadline failure is terminal without a partial commit"
                  (and (string= "failed" (gethash "status" expired))
                       (string= "wall-budget-exhausted"
                                (gethash "terminal_reason" expired))
                       (zerop (length
                               (gethash "consumed_stimulus_ids" expired))))))

      (cp-check "Q3 refuses any nonzero model budget"
                (handler-case
                    (progn
                      (make-deterministic-pulse-budget :model-calls 1)
                      nil)
                  (error () t)))
      (let ((poisoned (shasht:read-json (shasht:write-json health-state nil))))
        (setf (gethash "executable" poisoned) (lambda () :ran))
        (cp-check "pulse input recursively rejects executable objects"
                  (handler-case
                      (progn
                        (conscious-pulse-plan
                         poisoned :pulse-id "pulse:14" :agent-id "q3-dev"
                         :runtime-revision "conscious-q3-test"
                         :purpose :orient :opened-at 2000 :now 2000
                         :clock-identity "fixture-clock" :budget budget)
                        nil)
                    (error () t))))
      (cp-check "pulse planning does not mutate its conscious-state input"
                (let ((before (shasht:write-json health-state nil)))
                  (conscious-pulse-plan
                   health-state :pulse-id "pulse:15" :agent-id "q3-dev"
                   :runtime-revision "conscious-q3-test" :purpose :orient
                   :opened-at 2000 :now 2000 :clock-identity "fixture-clock"
                   :budget budget)
                  (string= before (shasht:write-json health-state nil)))))

    (let ((source (uiop:read-file-string pulse-path)))
      (cp-check "pure pulse source contains no legacy cognition/effect call"
                (notany (lambda (needle)
                          (search needle source :test #'char-equal))
                        '("(raw-call-model" "(call-model" "(auto-turn"
                          "(execute" "(telegram-send" "(log-event"))))))

(format t "~%~d passed, ~d failed~%" *cp-passed* *cp-failed*)
(when (plusp *cp-failed*) (uiop:quit 1))
