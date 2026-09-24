;;;; Sustained activity: original-ledger projection, no model or live state.
;;;; harness: full-system
(in-package :agent)
(load (test-source "sustained-activity-context.lisp"))
(defvar *sac-checks* 0)
(defun sac-check (condition)
  (unless condition (error "Activity assertion ~d failed" (1+ *sac-checks*)))
  (incf *sac-checks*))
(defun sac-event (id type payload &optional root (agent "fixture"))
  (obj "id" id "type" type "agent_id" agent "payload" payload "caused_by" root))
(defun sac-reference (&optional (roots #(1 10)) (through 20))
  (obj "activity_id" "work:fixture" "agent_id" "fixture"
       "root_event_ids" roots "through_event_id" through))
(defun sac-fixture ()
  (list
   ;; This value is deliberately mutated by the detachment assertion below;
   ;; do not ask the implementation to modify a compiler-coalesced literal.
   (sac-event 1 "user-message"
              (obj "text" (copy-seq "Fix the parser and verify it.")))
   (sac-event 2 "model-response"
              (obj "status" "accepted" "model_call_id" "model:1"
                   "assistant_message"
                   (obj "role" "assistant" "content" "Inspecting the failing case."
                        "reasoning_content" "PROVIDER-PRIVATE"
                        "tool_calls" (vector (obj "id" "call:1" "type" "function"
                                                 "function" (obj "name" "bash"
                                                                 "arguments" "{\"command\":\"run-parser-test\"}"))))) 1)
   (sac-event 3 "recursive-tool-result"
              (obj "model_call_id" "model:1" "tool_call_id" "call:1" "tool_name" "bash"
                   "execution_status" "executed"
                   "process_outcome" (obj "kind" "process-exit" "exit_code" 1)
                   "content" (concatenate 'string (make-string 4000 :initial-element #\x)
                                          "FAILED: preserve empty field")) 1)
   (sac-event 4 "model-response"
              (obj "status" "accepted" "model_call_id" "model:2"
                   "assistant_message" (obj "content" "The empty field still fails.")) 1)
   (sac-event 5 "agent-message" (obj "text" "The empty field still fails.") 1)
   (sac-event 6 "user-message" (obj "text" "Unrelated private interruption") nil "other")
   (sac-event 10 "user-message" (obj "text" "Continue; keep the trailing delimiter too."))
   (sac-event 11 "model-response"
              (obj "status" "accepted" "model_call_id" "model:3"
                   "assistant_message"
                   (obj "content" :null "tool_calls"
                        (vector (obj "id" "call:2" "type" "function"
                                     "function" (obj "name" "bash" "arguments"
                                                     "{\"command\":\"run-updated-test\"}"))))) 10)
   (sac-event 12 "recursive-tool-result"
              (obj "model_call_id" "model:3" "tool_call_id" "call:2" "tool_name" "bash"
                   "execution_status" "executed"
                   "process_outcome" (obj "kind" "process-exit" "exit_code" 0)
                   "content" "Parser tests passed; integration not run.") 10)
   (sac-event 13 "agent-message" (obj "text" "Parser fixed; integration remains.") 10)))

(let* ((events (mapcar #'%sac-copy (sac-fixture)))
       (packet (project-sustained-activity-context (sac-reference) events))
       (exchanges (gethash "exchanges" packet))
       (first-messages (gethash "messages" (aref exchanges 0)))
       (second-messages (gethash "messages" (aref exchanges 1)))
       (json (shasht:write-json packet nil)))
  (sac-check (equal "ready" (gethash "status" packet)))
  (sac-check (= 2 (length exchanges)))
  (sac-check (= 4 (length first-messages)))
  (sac-check (> (length (gethash "content" (aref first-messages 2))) 4000))
  (sac-check (search "FAILED: preserve empty field" (gethash "content" (aref first-messages 2))))
  (sac-check (equal "{\"command\":\"run-parser-test\"}"
                    (gethash "arguments" (gethash "function"
                      (aref (gethash "tool_calls" (aref first-messages 1)) 0)))))
  (sac-check (equal "Continue; keep the trailing delimiter too."
                    (gethash "content" (aref second-messages 0))))
  (sac-check (not (search "PROVIDER-PRIVATE" json)))
  (sac-check (not (search "Unrelated private interruption" json)))
  (sac-check (equalp #(1 2 3 5) (gethash "source_event_ids" (aref exchanges 0))))
  (sac-check (= 1 (gethash "exit_code" (gethash "process_outcome"
                          (aref (gethash "tool_outcomes" (aref exchanges 0)) 0)))))
  (sac-check (equal "not-assessed" (gethash "activity_completion" packet)))
  ;; Equivalent serialization/reload of authority evidence yields the same packet.
  (sac-check (equalp packet (project-sustained-activity-context
                             (%sac-copy (sac-reference)) (%sac-copy (coerce events 'vector)))))
  ;; Returned packet is detached from mutable inputs.
  (setf (char (gethash "text" (gethash "payload" (first events))) 0) #\Z)
  (sac-check (equal "Fix the parser and verify it." (gethash "content" (aref first-messages 0)))))

(let* ((rows (sac-fixture))
       (packet (project-sustained-activity-context (sac-reference) rows :maximum-characters 100)))
  (sac-check (equal "compaction-required" (gethash "status" packet)))
  (sac-check (= 2 (length (gethash "exchanges" packet))))
  (sac-check (equalp #() (gethash "omitted_root_event_ids" packet)))
  (sac-check (> (gethash "rendered_characters" packet) 100))
  (let ((tiny (project-sustained-activity-context (sac-reference) rows :maximum-characters 1)))
    (sac-check (equal "compaction-required" (gethash "status" tiny)))
    (sac-check (= 2 (length (gethash "exchanges" tiny))))))

(let ((packet (project-sustained-activity-context (sac-reference #(1) 2) (sac-fixture))))
  (sac-check (equal "incomplete" (gethash "status" packet)))
  (sac-check (= 1 (gethash "pending_tool_count" (aref (gethash "exchanges" packet) 0))))
  (sac-check (equal "not-assessed" (gethash "activity_completion" packet))))

;; A pre-provider context failure closes that exact root without inventing an
;; assistant response, allowing a later explicitly linked retry to assemble
;; the preserved activity history.
(let* ((rows (list (sac-event 20 "user-message"
                              (obj "text" "Retry this exact request."))
                   (sac-event 21 "recursive-root-failed"
                              (obj "stage" "context-open"
                                   "error_code" "recursive-context-open-failed"
                                   "reason" "Fixture context failure") 20)))
       (packet (project-sustained-activity-context
                (sac-reference #(20) 21) rows))
       (exchange (aref (gethash "exchanges" packet) 0)))
  (sac-check (equal "ready" (gethash "status" packet)))
  (sac-check (eq t (gethash "settled" exchange)))
  (sac-check (= 1 (length (gethash "messages" exchange))))
  (sac-check (equalp #(20 21) (gethash "source_event_ids" exchange))))

(dolist (mutator
          (list (lambda (rows) (remove 3 rows :key (lambda (e) (gethash "id" e))))
                (lambda (rows) (cons (first rows) rows))
                (lambda (rows) (remove 1 rows :key (lambda (e) (gethash "id" e))))
                (lambda (rows)
                  (setf (gethash "tool_call_id" (gethash "payload" (third rows))) "wrong") rows)
                (lambda (rows)
                  (remhash "assistant_message" (gethash "payload" (second rows))) rows)))
  (sac-check (handler-case
                (progn (project-sustained-activity-context (sac-reference)
                                                         (funcall mutator (sac-fixture))) nil)
              (error () t))))

(sac-check (handler-case
               (progn (project-sustained-activity-context (sac-reference #(10 1)) (sac-fixture)) nil)
             (error () t)))

;; Refusal is exact observed evidence, never silently rewritten to success.
(let ((rows (sac-fixture)))
  (setf (gethash "execution_status" (gethash "payload" (third rows))) "refused"
        (gethash "content" (gethash "payload" (third rows))) "NOT EXECUTED: allowance closed"
        (gethash "process_outcome" (gethash "payload" (third rows))) :null)
  (let* ((packet (project-sustained-activity-context (sac-reference) rows))
         (exchange (aref (gethash "exchanges" packet) 0)))
    (sac-check (equal "refused" (gethash "execution_status" (aref (gethash "tool_outcomes" exchange) 0))))
    (sac-check (equal "NOT EXECUTED: allowance closed"
                      (gethash "content" (aref (gethash "messages" exchange) 2))))))

(format t "Sustained activity context: ~d passed, 0 failed~%" *sac-checks*)

