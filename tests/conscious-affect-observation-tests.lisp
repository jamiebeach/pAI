;;;; Focused Q5A0 checks, loaded after the offline full system. No initialization.
;;;; harness: full-system
(in-package :agent)

(let ((checks 0))
  (flet ((check (value) (incf checks) (assert value)))
    (check (equal "unassessed"
                  (gethash "coping_evidence"
                           (conscious-affect-tool-observation "returned" "mind:test"))))
    (check (null (conscious-affect-tool-observation "success" "mind:test")))
    (check (null (conscious-affect-tool-observation "returned" nil)))
    ;; Exercise the actual production boundary with in-memory I/O ports only.
    (let* ((names '(%conversation-append-readable %recursive-notify
                    %recursive-validate-tool-arguments %conversation-time-phase
                    %conversation-persona-profile))
           (saved (mapcar (lambda (name) (cons name (symbol-function name))) names))
           (events nil)
           (*conscious-recursive-mind-agent-id* "mind:test")
           (*conscious-recursive-mind-tool-executor* nil)
           (projection (obj "thread_id" "thread:test" "model_call_id" "call:test"
                            "tool_call" (obj "id" "tool:test")
                            "tool_name" "fixture-tool" "tool_arguments" (obj))))
      (unwind-protect
           (progn
             (setf (symbol-function '%conversation-append-readable)
                   (lambda (type payload &key caused-by)
                     (declare (ignore caused-by))
                     (push (obj "id" (1+ (length events)) "type" type
                                "agent_id" "agent:test" "timestamp" 100
                                "payload" payload) events))
                   (symbol-function '%recursive-notify)
                   (lambda (&rest args) (declare (ignore args)))
                   (symbol-function '%recursive-validate-tool-arguments)
                   (lambda (name arguments) (declare (ignore name)) arguments)
                   (symbol-function '%conversation-time-phase)
                   (lambda (name fn) (declare (ignore name)) (funcall fn))
                   (symbol-function '%conversation-persona-profile)
                   (lambda () (obj "persona_id" "mind:test")))
             (setf *conscious-recursive-mind-tool-executor*
                   (lambda (&rest args) (declare (ignore args))
                     "ERROR: quoted document text, not a runtime exception"))
             (%recursive-tool-boundary projection 1 nil)
             (let* ((payload (gethash "payload" (first events)))
                    (observation (gethash "affect_observation" payload)))
               (check (equal "returned" (gethash "boundary_outcome" observation)))
               (check (equal "executed" (gethash "execution_status" payload))))
             (setf *conscious-recursive-mind-tool-executor*
                   (lambda (&rest args) (declare (ignore args)) (error "fixture failure")))
             (%recursive-tool-boundary projection 1 nil)
             (check (equal "raised-error"
                           (gethash "boundary_outcome"
                                    (gethash "affect_observation" (gethash "payload" (first events))))))
             (%recursive-refuse-tool projection 1 nil "fixture refusal")
             (%recursive-suppress-duplicate-tool projection 1 nil)
             (let* ((history (reverse events))
                    (report (conscious-affect-observation-report history "agent:test" "mind:test"))
                    (rows (gethash "observations" report)))
               (check (= 4 (length rows)))
               (check (equalp report (conscious-affect-observation-report history "agent:test" "mind:test")))
               (check (= 4 (length (gethash "observations"
                                           (conscious-affect-observation-report
                                            (append history history) "agent:test" "mind:test")))))
               (check (equal "not-attempted" (gethash "coping_evidence" (aref rows 2))))
               (check (equal "not-attempted" (gethash "coping_evidence" (aref rows 3))))
               (check (eq :null (gethash "disposition" report)))
               (check (zerop (length (gethash "observations"
                                            (conscious-affect-observation-report history "foreign" "mind:test")))))
               (check (zerop (length (gethash "observations"
                                            (conscious-affect-observation-report history "agent:test" "foreign")))))
               (setf (gethash "coping_evidence"
                              (gethash "affect_observation" (gethash "payload" (first events)))) "fabricated")
               (check (= 1 (gethash "invalid_observation_count"
                                   (conscious-affect-observation-report (reverse events) "agent:test" "mind:test")))))
             (setf (gethash "tool_name" projection) "bash"
                   *conscious-recursive-mind-tool-executor*
                   (lambda (&rest args) (declare (ignore args))
                     (values "exit_code: 99 (untrusted text)"
                             (obj "kind" "process-exit" "exit_code" 0))))
             (%recursive-tool-boundary projection 1 nil)
             (check (= 0 (gethash "exit_code" (gethash "process_outcome"
                                                      (gethash "payload" (first events))))))
             (check (equal "process-completed"
                           (gethash "coping_evidence"
                                    (aref (gethash "observations"
                                           (conscious-affect-observation-report
                                            (list (first events)) "agent:test" "mind:test")) 0))))
             ;; Persona identity is presentation, not motivation's runtime mind ID.
             (setf (symbol-function '%conversation-persona-profile) (lambda () (error "missing profile")))
             (check (equal "mind:test" (gethash "mind_identity_id"
                                                (%recursive-tool-affect-observation "returned"))))
             (setf *conscious-recursive-mind-agent-id* nil)
             (check (null (%recursive-tool-affect-observation "returned")))
             (setf *conscious-recursive-mind-agent-id* "mind:test"
                   *conscious-recursive-mind-tool-executor* nil
                   events nil)
             (check (eq :unavailable (%recursive-tool-boundary projection 1 nil)))
             (check (= 1 (length events)))
             (check (equal "means-unavailable"
                           (gethash "coping_evidence" (gethash "affect_observation"
                                                                (gethash "payload" (first events))))))
             (let* ((missing (obj "id" 90 "type" "recursive-tool-result"
                                  "agent_id" "agent:test" "payload" (obj)))
                    (other (obj "id" 91 "type" "recursive-tool-result"
                                "agent_id" "agent:test" "payload"
                                (obj "execution_status" "executed" "affect_observation"
                                     (conscious-affect-tool-observation "returned" "mind:other"))))
                    (report (conscious-affect-observation-report
                             (append events (list missing missing other)) "agent:test" "mind:test")))
               (check (= 1 (gethash "unattributed_agent_window_count" report)))
               (check (= 1 (gethash "other_mind_count" report)))
               (check (= 1 (length (gethash "observations" report))))
               (check (equal "incomplete" (gethash "coverage" report)))
               (check (equal "no-observations" (gethash "coverage"
                              (conscious-affect-observation-report nil "agent:test" "mind:test"))))))
        (dolist (entry saved) (setf (symbol-function (car entry)) (cdr entry))))))
  (format t "~&PASS: ~d focused affect observation checks.~%" checks))
