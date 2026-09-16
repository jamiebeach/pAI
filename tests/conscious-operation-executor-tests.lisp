;;;; conscious-operation-executor-tests.lisp -- separate tool worker boundary.

(in-package :agent)

(ql:quickload '(:shasht :ironclad) :silent t)

(defvar *cope-pass* 0)
(defvar *cope-fail* 0)

(defun cope-check (name condition)
  (if condition
      (progn (incf *cope-pass*) (format t "PASS ~a~%" name))
      (progn (incf *cope-fail*) (format t "FAIL ~a~%" name))))

(defun cope-event (id type payload)
  (obj "id" id "type" type "agent_id" "operation-fixture"
       "payload" payload))

(defun cope-profile ()
  (obj "profile_id" "operation-fixture" "revision" 1
       "max_model_calls" 4 "max_tool_operations" 3
       "max_reasoning_continuations" 2
       "max_tool_result_characters" 12000
       "permitted_proposal_kinds"
       (vector "tool-call-proposal" "publication-candidate"
               "request-continuation" "yield" "abstain")
       "permitted_tools" (vector "search-files")
       "budget_exhaustion" "suspend" "renewal_policy" "explicit-only"))

(defun cope-events ()
  (let ((proposal
          (obj "proposal_id" "pulse:1:proposal:1" "pulse_id" "pulse:1"
               "kind" "tool-call-proposal" "evidence_event_ids" (vector 1)
               "payload" (obj "tool_name" "search-files" "arguments"
                              (obj "query" "needle" "path" "."
                                   "max_results" 2)))))
    (list
     (cope-event 1 "user-message" (obj "text" "find needle"))
     (cope-event 2 "conscious-work-opened"
                 (obj "schema_version" 1 "work_id" "work:operation"
                      "concern_identity" "operator:search"
                      "stimulus_ids" (vector "stimulus:1")
                      "purpose" "respond" "priority_class" "direct"
                      "urgency_class" "interactive" "deadline" :null
                      "opened_at" 2 "profile" (cope-profile)))
     (cope-event 3 "model-request"
                 (obj "work_id" "work:operation" "pulse_id" "pulse:1"))
     (cope-event 4 "pulse-committed"
                 (obj "work_id" "work:operation" "pulse_id" "pulse:1"
                      "pulse_sequence" 1 "context_manifest"
                      (obj "schema_version" 1 "pulse_id" "pulse:1")
                      "proposals" (vector proposal))))))

(format t "~%== separate conscious operation executor ==~%")

(load (test-source "proposal.lisp"))
(load (test-source "conscious-file-search-tool.lisp"))
(load (test-source "cognitive-work.lisp"))
(load (test-source "boundary-outcome.lisp"))
(load (test-source "tool-operation-runtime.lisp"))
(load (test-source "cognitive-work-context.lisp"))
(load (test-source "cognitive-work-runtime.lisp"))
(load (test-source "cognitive-work-executor.lisp"))
(load (merge-pathnames
       "src/mind/conscious/cognitive-operation-executor.lisp" *pai-root*))

(let ((event-reads 0))
  (conscious-operation-executor-configure
   :agent-id "operation-fixture"
   :events-fn (lambda () (incf event-reads) nil)
   :projection-fn
   (lambda () (conscious-work-project nil "operation-fixture"))
   :operation-fn (lambda (&rest ignored)
                   (declare (ignore ignored)) (error "unreachable"))
   :transition-fn (lambda (&rest ignored)
                    (declare (ignore ignored)) (error "unreachable"))
   :cognition-wake-fn (lambda (&rest ignored) (declare (ignore ignored))))
  (conscious-operation-executor-start)
  (sleep 1.2)
  (conscious-operation-executor-stop)
  (cope-check "idle operation worker performs no periodic replay without a lease"
              (<= event-reads 1))
  (conscious-operation-executor-reset))

(let ((events (cope-events))
      (wake-count 0)
      (operation-count 0))
  (conscious-operation-executor-configure
   :agent-id "operation-fixture"
   :events-fn (lambda () events)
   :projection-fn
   (lambda () (conscious-work-project events "operation-fixture"))
   :operation-fn
   (lambda (proposal manifest &key work-id user-event-id &allow-other-keys)
     (incf operation-count)
     (cope-check "worker reconstructs committed proposal and manifest"
                 (and (string= "pulse:1:proposal:1"
                               (gethash "proposal_id" proposal))
                      (string= "pulse:1" (gethash "pulse_id" manifest))
                      (string= "work:operation" work-id)
                      (= 1 user-event-id)))
     (let* ((result (obj "schema_version" 1 "status" "ok"
                         "matches" (vector) "database_write_count" 0))
            (canonical (%conscious-tool-operation-canonical-json result)))
       (setf events
             (append
              events
              (list
               (cope-event
                5 "conscious-tool-operation-result"
                (obj "work_id" work-id
                     "proposal_id" (gethash "proposal_id" proposal)
                     "operation_id" "tool-operation:pulse:1:proposal:1"
                     "result" result
                     "result_characters" (length canonical)))))))
     (obj "schema_version" 1 "status" "ok"))
   :transition-fn (lambda (&rest ignored)
                    (declare (ignore ignored)) (error "unexpected transition"))
   :cognition-wake-fn
   (lambda (&key reason)
     (when (string= reason "operation-result") (incf wake-count))))
  (let ((receipt (conscious-operation-executor-run-one)))
    (cope-check "one operation advances a waiting work item"
                (and (string= "advanced" (gethash "status" receipt ""))
                     (= 1 operation-count)
                     (= 1 wake-count)
                     (string= "runnable"
                              (gethash
                               "state"
                               (gethash "work:operation"
                                        (gethash
                                         "items"
                                         (conscious-work-project
                                          events "operation-fixture"))) ""))))
    (cope-check "completed operation is not selected twice"
                (string= "idle"
                         (gethash "status"
                                  (conscious-operation-executor-run-one) ""))))
  (conscious-operation-executor-reset))

(let ((events (cope-events))
      (transition nil)
      (wake-count 0))
  (conscious-operation-executor-configure
   :agent-id "operation-fixture"
   :events-fn (lambda () events)
   :projection-fn
   (lambda () (conscious-work-project events "operation-fixture"))
   :operation-fn
   (lambda (&rest ignored)
     (declare (ignore ignored))
     ;; Execution crossed the durable claim boundary, so the absence of a
     ;; terminal receipt genuinely makes the outcome uncertain.
     (setf events
           (append events
                   (list
                    (cope-event
                     5 "conscious-tool-operation-claimed"
                     (obj "work_id" "work:operation"
                          "proposal_id" "pulse:1:proposal:1")))))
     (error "execution outcome uncertain"))
   :transition-fn
   (lambda (work-id state &key reason-code expected-state expected-revision)
     (declare (ignore expected-state expected-revision))
     (setf transition (list work-id state reason-code))
     (setf events
           (append events
                   (list
                    (cope-event 5 "conscious-work-outcome-unknown"
                                (obj "work_id" work-id
                                     "reason_code" reason-code))))))
   :cognition-wake-fn
   (lambda (&rest ignored)
     (declare (ignore ignored)) (incf wake-count)))
  (let ((receipt (conscious-operation-executor-run-one)))
    (cope-check "claim without a terminal becomes outcome unknown"
                (and (string= "outcome-unknown"
                               (gethash "status" receipt ""))
                     (string= "outcome-unknown"
                              (gethash "kind"
                                       (gethash "boundary_outcome" receipt)))
                     (equal transition
                            '("work:operation" "outcome-unknown"
                              "tool-operation-outcome-unknown"))
                     (zerop wake-count))))
  (conscious-operation-executor-reset))

(let ((events (cope-events))
      (transition nil))
  (conscious-operation-executor-configure
   :agent-id "operation-fixture"
   :events-fn (lambda () events)
   :projection-fn
   (lambda () (conscious-work-project events "operation-fixture"))
   :operation-fn
   (lambda (&rest ignored)
     (declare (ignore ignored))
     ;; Argument/proposal rejection occurs before any durable claim.
     (error "deterministic pre-claim rejection"))
   :transition-fn
   (lambda (work-id state &key reason-code expected-state expected-revision)
     (declare (ignore expected-state expected-revision))
     (setf transition (list work-id state reason-code)))
   :cognition-wake-fn (lambda (&rest ignored) (declare (ignore ignored))))
  (let ((receipt (conscious-operation-executor-run-one)))
    (cope-check "pre-claim rejection is failed rather than outcome unknown"
                (and (string= "failed" (gethash "status" receipt ""))
                     (string= "failed-before-claim"
                              (gethash "kind"
                                       (gethash "boundary_outcome" receipt)))
                     (equal transition
                            '("work:operation" "failed"
                              "tool-operation-rejected")))))
  (conscious-operation-executor-reset))

(let* ((events (cope-events))
       (claim
         (cope-event
          5 "conscious-tool-operation-claimed"
          (obj "work_id" "work:operation"
               "proposal_id" "pulse:1:proposal:1"
               "lease_expires_at" (+ (get-universal-time) 60))))
       (transitions 0))
  (setf events (append events (list claim)))
  (conscious-operation-executor-configure
   :agent-id "operation-fixture"
   :events-fn (lambda () events)
   :projection-fn
   (lambda () (conscious-work-project events "operation-fixture"))
   :operation-fn (lambda (&rest ignored)
                   (declare (ignore ignored))
                   (error "pre-existing claim remains owned"))
   :transition-fn
   (lambda (work-id state &key reason-code expected-state expected-revision)
     (declare (ignore work-id state reason-code expected-state expected-revision))
     (incf transitions))
   :cognition-wake-fn (lambda (&rest ignored) (declare (ignore ignored))))
  (let ((receipt (conscious-operation-executor-run-one)))
    (cope-check "unexpired durable claim remains leased without terminalization"
                (and (string= "leased" (gethash "status" receipt ""))
                     (zerop transitions))))
  (setf (gethash "lease_expires_at" (gethash "payload" claim))
        (1- (get-universal-time)))
  (let ((receipt (conscious-operation-executor-run-one)))
    (cope-check "expired durable claim becomes outcome unknown once"
                (and (string= "outcome-unknown"
                              (gethash "status" receipt ""))
                     (= 1 transitions))))
  (conscious-operation-executor-reset))

(format t "~%~d passed, ~d failed~%" *cope-pass* *cope-fail*)
(when (plusp *cope-fail*) (error "conscious operation executor tests failed"))
