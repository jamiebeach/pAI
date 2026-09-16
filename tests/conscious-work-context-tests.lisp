;;;; conscious-work-context-tests.lisp -- durable private continuation evidence.

(in-package :agent)

(ql:quickload '(:shasht :ironclad) :silent t)

(defvar *cwctx-pass* 0)
(defvar *cwctx-fail* 0)

(defun cwctx-check (name condition)
  (if condition
      (progn (incf *cwctx-pass*) (format t "PASS ~a~%" name))
      (progn (incf *cwctx-fail*) (format t "FAIL ~a~%" name))))

(defun cwctx-signals-p (thunk)
  (handler-case (progn (funcall thunk) nil) (error () t)))

(defun cwctx-profile (&key (result-limit 12000))
  (obj "profile_id" "interactive-dev" "revision" 1
       "max_model_calls" 8 "max_tool_operations" 6
       "max_reasoning_continuations" 3
       "max_tool_result_characters" result-limit
       "permitted_proposal_kinds"
       (vector "tool-call-proposal" "publication-candidate"
               "request-continuation" "yield" "abstain")
       "permitted_tools" (vector "search-files")
       "budget_exhaustion" "suspend" "renewal_policy" "explicit-only"))

(defun cwctx-event (id type payload &optional (agent-id "context-fixture"))
  (obj "id" id "type" type "agent_id" agent-id "payload" payload))

(defun cwctx-events (&key (tamper nil) (result-limit 12000))
  (let* ((proposal
           (obj "proposal_id" "pulse:1:proposal:1" "pulse_id" "pulse:1"
                "runtime_revision" "fixture" "conscious_state_revision" 2
                "kind" "tool-call-proposal"
                "created_at_stage" "model-deliberation" "confidence" 0.9d0
                "evidence_event_ids" (vector 1)
                "payload" (obj "tool_name" "search-files" "arguments"
                               (obj "query" "needle" "path" "."
                                    "max_results" 3))))
         (result
           (obj "schema_version" 1 "status" "ok"
                "matches" (vector (obj "path" "notes.txt" "line" 4
                                       "text" "needle here"))
                "database_write_count" 0))
         (canonical (%conscious-tool-operation-canonical-json result))
         (arguments
           (%conscious-tool-operation-normalized-arguments
            "search-files" (gethash "arguments" (gethash "payload" proposal))))
         (arguments-hash
           (%conscious-tool-operation-hash
            (%conscious-tool-operation-canonical-json arguments))))
    (list
     (cwctx-event 1 "user-message"
                  (obj "text" "Find the needle" "channel" "terminal"))
     (cwctx-event
      2 "conscious-work-opened"
      (obj "schema_version" 1 "work_id" "work:fixture"
           "concern_identity" "operator:fixture"
           "stimulus_ids" (vector "stimulus:1") "purpose" "respond"
           "priority_class" "direct" "urgency_class" "interactive"
           "deadline" :null "opened_at" 2
           "profile" (cwctx-profile :result-limit result-limit)))
     (cwctx-event 3 "model-request"
                  (obj "work_id" "work:fixture" "pulse_id" "pulse:1"))
     (cwctx-event 4 "pulse-committed"
                  (obj "work_id" "work:fixture" "pulse_id" "pulse:1"
                       "pulse_sequence" 1 "proposals" (vector proposal)))
     (cwctx-event
      5 "conscious-tool-operation-result"
      (obj "schema_version" 1
           "operation_id" "tool-operation:pulse:1:proposal:1"
           "proposal_id" "pulse:1:proposal:1" "interaction_id" :null
           "work_id" "work:fixture" "user_event_id" 1
           "tool_name" "search-files" "arguments_hash" arguments-hash
           "result" result "result_characters" (length canonical)
           "result_hash"
           (if tamper "not-the-result-hash"
               (%conscious-tool-operation-hash canonical)))))))

(format t "~%== durable cognitive work continuation context ==~%")

(let ((subject (merge-pathnames "src/mind/conscious/cognitive-work-context.lisp"
                                *pai-root*)))
  (cwctx-check "cognitive work context source exists" (probe-file subject))
  (when (probe-file subject)
    (load (test-source "proposal.lisp"))
    (load (test-source "conscious-file-search-tool.lisp"))
    (load (test-source "tool-operation-runtime.lisp"))
    (load (test-source "cognitive-work.lisp"))
    (load subject)
    (let* ((context
             (conscious-work-context-build
              (cwctx-events) "work:fixture" "context-fixture"))
           (records (gethash "tool_result_records" context))
           (record (and (= 1 (length records)) (aref records 0))))
      (cwctx-check "work roots resolve to exact durable source events"
                   (equalp #(1) (gethash "root_event_ids" context)))
      (cwctx-check "only the matching durable tool result enters context"
                   (and record
                        (eql 5 (gethash "source_id" record))
                        (string= "untrusted-tool-results"
                                 (gethash "section" record ""))))
      (cwctx-check "tool output is explicitly untrusted evidence"
                   (and (string= "untrusted-tool-result"
                                 (gethash "role" record ""))
                        (search "needle here" (gethash "content" record ""))))
      (cwctx-check "remaining work authority is derived from its profile"
                   (and (= 5 (gethash "tool_proposals_remaining" context -1))
                        (= 3 (gethash "continuations_remaining" context -1))
                        (equalp #( "search-files")
                                (gethash "available_tools" context))))
      (cwctx-check "tool result event becomes eligible evidence"
                   (member 5 (coerce (gethash "evidence_event_ids" context)
                                     'list))))
    (cwctx-check "tampered result integrity fails closed"
                 (cwctx-signals-p
                  (lambda ()
                    (conscious-work-context-build
                     (cwctx-events :tamper t)
                     "work:fixture" "context-fixture"))))
    (cwctx-check "profile result-character authority is enforced"
                 (cwctx-signals-p
                  (lambda ()
                    (conscious-work-context-build
                     (cwctx-events :result-limit 4)
                     "work:fixture" "context-fixture"))))
    (let ((events (cwctx-events)))
      (setf (gethash "agent_id" (fifth events)) "another-mind")
      (let ((context
              (conscious-work-context-build
               events "work:fixture" "context-fixture")))
        (cwctx-check "another mind's result cannot enter continuation"
                     (zerop (length (gethash "tool_result_records" context))))))))

(format t "~%~d passed, ~d failed~%" *cwctx-pass* *cwctx-fail*)
(when (plusp *cwctx-fail*) (error "conscious work context tests failed"))
