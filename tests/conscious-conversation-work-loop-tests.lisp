;;;; conscious-conversation-work-loop-tests.lisp -- production-path Gate B loop.

(in-package :agent)

(ql:quickload '(:shasht :ironclad) :silent t)

(defvar *ccwl-pass* 0)
(defvar *ccwl-fail* 0)
(defvar *ccwl-events* nil)
(defvar *ccwl-canonical-events* nil)
(defvar *ccwl-event-lock* (bt:make-lock "conversation work fixture events"))
(defvar *ccwl-append-lock* (bt:make-lock "conversation work append authority"))
(defparameter *agent-id* "conversation-work-fixture")

(defun ccwl-check (name condition)
  (if condition
      (progn (incf *ccwl-pass*) (format t "PASS ~a~%" name))
      (progn (incf *ccwl-fail*) (format t "FAIL ~a~%" name))))

(defun replay-events (&rest ignored)
  (declare (ignore ignored))
  (bt:with-lock-held (*ccwl-event-lock*) (copy-list *ccwl-events*)))

(defun log-event (type payload &key caused-by)
  (bt:with-lock-held (*ccwl-event-lock*)
    (let* ((id (1+ (length *ccwl-events*)))
           (event (obj "id" id "type" type "agent_id" *agent-id*
                       "caused_by" (or caused-by :null) "payload" payload)))
      (setf *ccwl-events* (append *ccwl-events* (list event)))
      (values id t event))))

(defun log-event-if (predicate type payload &key caused-by)
  (bt:with-lock-held (*ccwl-append-lock*)
    (when (funcall predicate)
      (multiple-value-bind (id durable receipt)
          (log-event type payload :caused-by caused-by)
        (values id durable receipt t)))))

(defun ccwl-profile ()
  (obj "profile_id" "interactive-e2e" "revision" 1
       "max_model_calls" 4 "max_tool_operations" 2
       "max_reasoning_continuations" 2
       "max_tool_result_characters" 12000
       "permitted_proposal_kinds"
       (vector "tool-call-proposal" "publication-candidate"
               "request-continuation" "yield" "abstain")
       "permitted_tools" (vector "search-files")
       "budget_exhaustion" "suspend" "renewal_policy" "explicit-only"))

(defun ccwl-runtime-plan ()
  (let* ((body
           (obj "schema_version" 1
                "compatibility_revision" "dedicated-untrusted-tool-results-v1"
                "work_profile" (ccwl-profile)))
         (hash (%conscious-runtime-plan-hash-text
                (%conscious-work-canonical-json body))))
    (obj "schema_version" 1 "plan_hash" hash "canonical_plan" body)))

(defun ccwl-manifest (pulse-id)
  (obj "pulse_id" pulse-id "runtime_revision" "e2e"
       "conscious_state_revision" 1 "evidence_event_ids" (vector 1)
       "available_tools" (vector "search-files")
       "permitted_proposal_kinds"
       (vector "tool-call-proposal" "publication-candidate"
               "request-continuation" "yield" "abstain")
       "remaining_budget"
       (obj "tool_proposals" 2 "continuations" 2
            "publication_candidates" 1)))

(defun ccwl-with-timing (result context-ms provider-ms)
  (setf (gethash "timing_ms" result)
        (obj "total" (+ context-ms provider-ms)
             "context_open" context-ms "provider" provider-ms
             "unattributed" 0))
  result)

(format t "~%== production-path durable conversation work loop ==~%")

(load (test-source "policy.lisp"))
(load (test-source "stimulus.lisp"))
(load (test-source "proposal.lisp"))
(load (test-source "conscious-file-search-tool.lisp"))
(load (test-source "cognitive-work.lisp"))
(load (test-source "cognitive-work-runtime.lisp"))
(load (test-source "boundary-outcome.lisp"))
(load (test-source "runtime-composition.lisp"))
(load (test-source "tool-operation-runtime.lisp"))
(load (test-source "cognitive-work-context.lisp"))
(load (test-source "cognitive-work-executor.lisp"))
(load (merge-pathnames
       "src/mind/conscious/cognitive-operation-executor.lisp" *pai-root*))
(load (merge-pathnames
       "src/mind/conscious/conversation-work-loop.lisp" *pai-root*))

(let* ((work-id "work:timing-origin-probe")
       (now (%conscious-conversation-work-now))
       (started (- now (round internal-time-units-per-second 20))))
  (%conscious-conversation-work-timing-start work-id started)
  (%conscious-conversation-work-timing-quantum-start work-id)
  (ccwl-check "scheduler timing begins before durable work opening"
              (>= (gethash "scheduler_handoff"
                           (gethash work-id
                                    *conscious-conversation-work-timings*) 0)
                  40))
  (bt:with-lock-held (*conscious-conversation-work-lock*)
    (remhash work-id *conscious-conversation-work-timings*)))

(setf *ccwl-events* nil)
(setf (gethash "user-message" *stimulus-kind-map*)
      '("user-message" "channel" "interactive" t))
(log-event
 "user-message"
 (obj "text" "Find the conversation work entry point"
      "channel" "terminal"
      "metadata" (obj "source" "q4.5-conversation"
                      "interaction_id" "interaction:e2e")))
(conscious-runtime-plan-retain (ccwl-runtime-plan))
(conscious-file-search-configure *pai-root*)

(let ((model-quanta 0)
      (saw-durable-tool-context nil)
      (embedding-scopes nil)
      (progress nil))
  (conscious-conversation-work-configure
   :agent-id *agent-id* :profile (ccwl-profile)
   :runtime-plan (ccwl-runtime-plan)
   :progress-fn
   (lambda (status phase elapsed-ms)
     (declare (ignore elapsed-ms))
     (push (list status phase) progress))
   :turn-fn
   (lambda (prompt &key admitted-event-id channel interaction-id work-id)
     (declare (ignore prompt channel interaction-id))
     (incf model-quanta)
     (push *embedding-turn-cache* embedding-scopes)
     (let* ((pulse-id (format nil "pulse:e2e:~d" model-quanta))
            (manifest (ccwl-manifest pulse-id)))
       (log-event "model-request"
                  (obj "work_id" work-id "pulse_id" pulse-id)
                  :caused-by admitted-event-id)
       (if (= model-quanta 1)
           (let ((proposal
                   (obj "proposal_id" "pulse:e2e:1:proposal:1"
                        "pulse_id" pulse-id "runtime_revision" "e2e"
                        "conscious_state_revision" 1
                        "kind" "tool-call-proposal"
                        "created_at_stage" "model-deliberation"
                        "confidence" 0.9d0
                        "evidence_event_ids" (vector admitted-event-id)
                        "payload"
                        (obj "tool_name" "search-files" "arguments"
                             (obj "query" "conscious-conversation-work-configure"
                                  "path" "src/mind/conscious"
                                  "max_results" 3)))))
             (log-event "pulse-committed"
                        (obj "work_id" work-id "pulse_id" pulse-id
                             "pulse_sequence" 1 "context_manifest" manifest
                             "proposals" (vector proposal))
                        :caused-by admitted-event-id)
             (ccwl-with-timing
              (obj "schema_version" 1 "status" "tool-proposed"
                   "proposal_id" (gethash "proposal_id" proposal)
                   "content" :null)
              11 13))
           (let* ((context
                    (conscious-work-context-build
                     (replay-events) work-id *agent-id*))
                  (records (gethash "tool_result_records" context))
                  (proposal
                    (obj "proposal_id" "pulse:e2e:2:proposal:1"
                         "pulse_id" pulse-id "kind" "publication-candidate"
                         "evidence_event_ids"
                         (gethash "evidence_event_ids" context))))
             (setf saw-durable-tool-context
                   (and (= 1 (length records))
                        (search "conversation-work-loop.lisp"
                                (gethash "content" (aref records 0)))))
             (log-event "pulse-committed"
                        (obj "work_id" work-id "pulse_id" pulse-id
                             "pulse_sequence" 2 "context_manifest" manifest
                             "proposals" (vector proposal))
                        :caused-by admitted-event-id)
             (multiple-value-bind (agent-id ignored stored)
                 (log-event "agent-message"
                            (obj "text" "I found the durable work entry point."
                                 "channel" "terminal")
                            :caused-by admitted-event-id)
               (declare (ignore ignored stored))
               (ccwl-with-timing
                (obj "schema_version" 1 "status" "replied"
                     "content" "I found the durable work entry point."
                     "agent_event_id" agent-id)
                17 19)))))))
  (unwind-protect
       (progn
         (conscious-conversation-work-start)
         (let ((result
                 (conscious-conversation-work-run
                  "Find the conversation work entry point"
                  :admitted-event-id 1 :channel "terminal"
                  :interaction-id "interaction:e2e" :timeout 20)))
           (ccwl-check "one admitted message closes a two-quantum tool loop"
                       (and (string= "replied" (gethash "status" result ""))
                            (= 2 model-quanta)))
           (ccwl-check "second quantum receives verified durable tool evidence"
                       saw-durable-tool-context)
           (ccwl-check "one work reuses one ephemeral embedding cache across quanta"
                       (and (= 2 (length embedding-scopes))
                            (eq (first embedding-scopes)
                                (second embedding-scopes))
                            (zerop
                             (hash-table-count
                              *conscious-conversation-work-embedding-caches*))))
           (let ((timing (gethash "timing_ms" result)))
             (ccwl-check "final timing closes both quanta and tool handoff"
                         (and (hash-table-p timing)
                              (= 2 (gethash "quantum_count" timing -1))
                              (= 28 (gethash "context_open" timing -1))
                              (= 32 (gethash "provider" timing -1))
                              (integerp (gethash "tool_execution" timing))
                              (integerp (gethash "scheduler_handoff" timing))
                              (integerp (gethash "boundary_settlement" timing))
                              (not (nth-value 1
                                      (gethash "%work_started_at" result)))
                              ;; TOTAL is measured by the runtime clock while
                              ;; phase values above are synthetic provider
                              ;; receipts. Their magnitudes are deliberately
                              ;; independent in this fixture.
                              (not (minusp (gethash "total" timing -1))))))
           (let* ((projection (conscious-work-runtime-project))
                  (work
                    (loop for item being the hash-values of
                          (gethash "items" projection) return item)))
             (ccwl-check "publication completes the same durable work lineage"
                         (and (string= "completed" (gethash "state" work ""))
                              (= 1 (gethash "tool_operations_used" work -1))
                              (= 2 (gethash "model_calls_used" work -1)))))
           (ccwl-check "real search is claimed and terminalized exactly once"
                       (and (= 1 (count "conscious-tool-operation-claimed"
                                        *ccwl-events*
                                        :key (lambda (event)
                                               (gethash "type" event))
                                        :test #'string=))
                            (= 1 (count "conscious-tool-operation-result"
                                        *ccwl-events*
                                        :key (lambda (event)
                                               (gethash "type" event))
                                        :test #'string=))))
           (ccwl-check "content-free progress spans cognition and tool execution"
                       (and (find '("started" "cognitive_quantum") progress
                                  :test #'equal)
                            (find '("started" "tool_execution") progress
                                  :test #'equal)
                            (find '("completed" "tool_execution") progress
                                  :test #'equal)))
           (setf *ccwl-canonical-events* (copy-list *ccwl-events*))
           (let ((before-count (length *ccwl-events*)))
             (conscious-conversation-work-stop)
             (conscious-conversation-work-configure
              :agent-id *agent-id* :profile (ccwl-profile)
              :runtime-plan (ccwl-runtime-plan)
              :turn-fn (lambda (&rest ignored)
                         (declare (ignore ignored))
                         (error "model must not run during recovery")))
             (conscious-conversation-work-start)
             (let ((recovered
                     (conscious-conversation-work-run
                      "Find the conversation work entry point"
                      :admitted-event-id 1 :channel "terminal"
                      :interaction-id "interaction:e2e" :timeout 5)))
               (ccwl-check "restart returns durable publication without replaying work"
                           (and (string= "replied"
                                         (gethash "status" recovered ""))
                                (gethash "recovered" recovered)
                                (= before-count (length *ccwl-events*))))))))
    (conscious-conversation-work-stop)))

;; A provider response can be durably received yet invalid as a captured
;; proposal. Its pulse failure must release model ownership so a later
;; interaction can run normally in the same process.
(let ((quanta 0))
  (multiple-value-bind (failed-root ignored failed-event)
      (log-event "user-message"
                 (obj "text" "invalid fixture" "channel" "terminal"
                      "metadata" (obj "source" "q4.5-conversation"
                                      "interaction_id" "interaction:invalid")))
    (declare (ignore ignored failed-event))
    (conscious-conversation-work-configure
     :agent-id *agent-id* :profile (ccwl-profile)
     :runtime-plan (ccwl-runtime-plan)
     :turn-fn
     (lambda (prompt &key admitted-event-id channel interaction-id work-id)
       (declare (ignore prompt channel interaction-id))
       (incf quanta)
       (let ((pulse-id (format nil "pulse:failure:~d" quanta)))
         (log-event "model-request"
                    (obj "work_id" work-id "pulse_id" pulse-id)
                    :caused-by admitted-event-id)
         (if (= quanta 1)
             (progn
               (log-event "pulse-failed"
                          (obj "work_id" work-id "pulse_id" pulse-id
                               "terminal_reason"
                               "captured-deliberation-rejected")
                          :caused-by admitted-event-id)
               (obj "schema_version" 1 "status" "provider-response-invalid"
                    "content" :null))
             (progn
               (log-event "pulse-committed"
                          (obj "work_id" work-id "pulse_id" pulse-id
                               "pulse_sequence" 4
                               "proposals"
                               (vector (obj "proposal_id" "recovery:p"
                                            "kind" "publication-candidate")))
                          :caused-by admitted-event-id)
               (multiple-value-bind (agent-id ignored stored)
                   (log-event "agent-message"
                              (obj "text" "The later turn completed."
                                   "channel" "terminal")
                              :caused-by admitted-event-id)
                 (declare (ignore ignored stored agent-id))
                 (obj "schema_version" 1 "status" "replied"
                      "content" "The later turn completed.")))))))
    (unwind-protect
         (progn
           (conscious-conversation-work-start)
           (let ((failed
                   (conscious-conversation-work-run
                    "invalid fixture" :admitted-event-id failed-root
                    :channel "terminal" :interaction-id "interaction:invalid"
                    :timeout 10)))
             (ccwl-check "invalid captured response terminalizes its work"
                         (string= "provider-response-invalid"
                                  (gethash "status" failed ""))))
           (multiple-value-bind (next-root ignored next-event)
               (log-event "user-message"
                          (obj "text" "later fixture" "channel" "terminal"
                               "metadata"
                               (obj "source" "q4.5-conversation"
                                    "interaction_id" "interaction:later")))
             (declare (ignore ignored next-event))
             (let ((later
                     (conscious-conversation-work-run
                      "later fixture" :admitted-event-id next-root
                      :channel "terminal" :interaction-id "interaction:later"
                      :timeout 10)))
               (ccwl-check "a later turn runs after an invalid provider response"
                           (and (string= "replied" (gethash "status" later ""))
                                (= 2 quanta))))))
      (conscious-conversation-work-stop))))

;; Context assembly can durably fail the pulse before returning an error. The
;; work loop must consume that already-terminal boundary rather than attempting
;; a second transition and leaving its synchronous waiter to time out.
(multiple-value-bind (root-id ignored root-event)
    (log-event "user-message"
               (obj "text" "context failure fixture" "channel" "terminal"
                    "metadata" (obj "source" "q4.5-conversation"
                                    "interaction_id" "interaction:context-fail")))
  (declare (ignore ignored root-event))
  (conscious-conversation-work-configure
   :agent-id *agent-id* :profile (ccwl-profile)
   :runtime-plan (ccwl-runtime-plan)
   :turn-fn
   (lambda (prompt &key admitted-event-id channel interaction-id work-id)
     (declare (ignore prompt channel interaction-id))
     (let ((pulse-id "pulse:context-fail"))
       (log-event "pulse-opened"
                  (obj "work_id" work-id "pulse_id" pulse-id)
                  :caused-by admitted-event-id)
       (log-event "pulse-failed"
                  (obj "work_id" work-id "pulse_id" pulse-id
                       "terminal_reason" "context-assembly-rejected")
                  :caused-by admitted-event-id)
       (error "context assembly rejected"))))
  (unwind-protect
       (progn
         (conscious-conversation-work-start)
         (let ((result
                 (conscious-conversation-work-run
                  "context failure fixture" :admitted-event-id root-id
                  :channel "terminal" :interaction-id
                  "interaction:context-fail" :timeout 2)))
           (ccwl-check "durably failed context assembly returns without timeout"
                       (string= "failed" (gethash "status" result "")))))
    (conscious-conversation-work-stop)))

;; A deterministic local/provider-policy preflight failure before any durable
;; model-request cannot be labelled outcome-unknown. No request escaped.
(multiple-value-bind (root-id ignored root-event)
    (log-event "user-message"
               (obj "text" "preflight failure fixture" "channel" "terminal"
                    "metadata" (obj "source" "q4.5-conversation"
                                    "interaction_id"
                                    "interaction:preflight-fail")))
  (declare (ignore ignored root-event))
  (conscious-conversation-work-configure
   :agent-id *agent-id* :profile (ccwl-profile)
   :runtime-plan (ccwl-runtime-plan)
   :turn-fn
   (lambda (&rest ignored)
     (declare (ignore ignored))
     (error "provider preflight rejected before request append")))
  (unwind-protect
       (progn
         (conscious-conversation-work-start)
         (let* ((result
                  (conscious-conversation-work-run
                   "preflight failure fixture" :admitted-event-id root-id
                   :channel "terminal" :interaction-id
                   "interaction:preflight-fail" :timeout 2))
                (work-id (gethash "work_id" result))
                (work-events
                  (remove-if-not
                   (lambda (event)
                     (let ((payload (gethash "payload" event)))
                       (and (hash-table-p payload)
                            (equal work-id (gethash "work_id" payload)))))
                   *ccwl-events*)))
           (ccwl-check "pre-request exception is failed rather than outcome-unknown"
                       (and (string= "failed" (gethash "status" result ""))
                            (string= "conversation-quantum-local-failure"
                                     (gethash "error_code" result ""))
                            (null (find "model-request" work-events
                                        :key (lambda (event)
                                               (gethash "type" event ""))
                                        :test #'string=))))))
    (conscious-conversation-work-stop)))

;; A presentation deadline detaches without inventing cognitive state. The
;; same admitted root can be inspected and rejoined after its durable owner
;; finishes.
(multiple-value-bind (root-id ignored root-event)
    (log-event "user-message"
               (obj "text" "detach fixture" "channel" "terminal"
                    "metadata" (obj "source" "q4.5-conversation"
                                    "interaction_id" "interaction:detach")))
  (declare (ignore ignored root-event))
  (conscious-conversation-work-configure
   :agent-id *agent-id* :profile (ccwl-profile)
   :runtime-plan (ccwl-runtime-plan)
   :turn-fn
   (lambda (prompt &key admitted-event-id channel interaction-id work-id)
     (declare (ignore prompt channel interaction-id))
     (let ((pulse-id "pulse:detach"))
       (log-event "model-request"
                  (obj "work_id" work-id "pulse_id" pulse-id)
                  :caused-by admitted-event-id)
       (sleep 0.2)
       (log-event "pulse-failed"
                  (obj "work_id" work-id "pulse_id" pulse-id
                       "terminal_reason" "contained-detach-fixture")
                  :caused-by admitted-event-id)
       (obj "schema_version" 1 "status" "failed" "content" :null))))
  (unwind-protect
       (progn
         (conscious-conversation-work-start)
         (let* ((before
                  (count-if
                   (lambda (event)
                     (member (gethash "type" event)
                             '("conscious-work-failed"
                               "conscious-work-outcome-unknown")
                             :test #'string=))
                   *ccwl-events*))
                (detached
                  (conscious-conversation-work-run
                   "detach fixture" :admitted-event-id root-id
                   :channel "terminal" :interaction-id "interaction:detach"
                   :timeout 0.02))
                (work-id (gethash "work_id" detached))
                (after
                  (count-if
                   (lambda (event)
                     (member (gethash "type" event)
                             '("conscious-work-failed"
                               "conscious-work-outcome-unknown")
                             :test #'string=))
                   *ccwl-events*)))
           (ccwl-check "presentation expiry detaches without terminal event"
                       (and (string= "detached"
                                     (gethash "status" detached ""))
                            (gethash "can_rejoin" detached)
                            (= before after)
                            (member (gethash "observed_state" detached)
                                    '("runnable" "deliberating")
                                    :test #'string=)))
           (sleep 0.3)
           (let ((inspection
                   (conscious-conversation-work-inspect work-id))
                 (rejoined
                   (conscious-conversation-work-rejoin root-id :timeout 1)))
             (ccwl-check "later observer inspects and rejoins durable terminal"
                         (and (string= "failed"
                                       (gethash "state" inspection ""))
                              (string= "failed"
                                       (gethash "status" rejoined "")))))))
    (conscious-conversation-work-stop)))

;; Replaying every prefix immediately before and after the canonical append
;; boundaries models process loss without a test-only recovery implementation.
;; Each prefix must be a legal projection, and no prefix may invent a second
;; terminal owner.
(let* ((types '("model-request" "pulse-committed"
                "conscious-tool-operation-claimed"
                "conscious-tool-operation-result" "agent-message"
                "conscious-work-completed"))
       (events *ccwl-canonical-events*)
       (positions
         (loop for event in events for index from 0
               when (member (gethash "type" event "") types :test #'string=)
                 collect index))
       (expected-counts '(("model-request" . 2) ("pulse-committed" . 2)
                          ("conscious-tool-operation-claimed" . 1)
                          ("conscious-tool-operation-result" . 1)
                          ("agent-message" . 1)
                          ("conscious-work-completed" . 1)))
       (legal t)
       (single-terminal t))
  (dolist (entry expected-counts)
    (unless (= (cdr entry)
               (count (car entry) events
                      :key (lambda (event) (gethash "type" event ""))
                      :test #'string=))
      (setf legal nil)))
  (dolist (position positions)
    (dolist (end (list position (1+ position)))
      (handler-case
          (let* ((prefix (subseq events 0 end))
                 (projection (conscious-work-project prefix *agent-id*))
                 (work
                   (loop for value being the hash-values of
                         (gethash "items" projection) return value))
                 (terminals
                   (count-if
                    (lambda (event)
                      (member (gethash "type" event "")
                              '("conscious-work-completed"
                                "conscious-work-failed"
                                "conscious-work-outcome-unknown")
                              :test #'string=))
                    prefix)))
            (unless (and (hash-table-p work)
                         (member (gethash "state" work "")
                                 '("runnable" "deliberating"
                                   "waiting-operation" "completing"
                                   "completed")
                                 :test #'string=))
              (format t "FAULT-PREFIX-DIAGNOSTIC event=~a side=~a state=~a~%"
                      (gethash "type" (nth position events) "")
                      (if (= end position) "before" "after")
                      (and work (gethash "state" work "")))
              (setf legal nil))
            (unless (<= terminals 1) (setf single-terminal nil)))
        (error (condition)
          (format t "FAULT-PREFIX-DIAGNOSTIC event=~a side=~a error=~a~%"
                  (gethash "type" (nth position events) "")
                  (if (= end position) "before" "after") condition)
          (setf legal nil)))))
  (ccwl-check "canonical append fault prefixes all reconstruct legally" legal)
  (ccwl-check "canonical append fault prefixes preserve one terminal owner"
              single-terminal)
  (ccwl-check "fault matrix covers every request/proposal/claim/result/publication boundary"
              (= 8 (length positions))))

(format t "~%~d passed, ~d failed~%" *ccwl-pass* *ccwl-fail*)
(when (plusp *ccwl-fail*)
  (error "conscious conversation work loop tests failed"))
