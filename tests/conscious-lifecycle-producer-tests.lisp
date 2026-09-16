;;;; conscious-lifecycle-producer-tests.lisp -- Q5 real producer connection.
;;;;
;;;; Written before lifecycle-sources.lisp. The first run must fail because the
;;;; producer adapter is absent; implementation then satisfies the full path.

(in-package :agent)

(defvar *clpt-passed* 0)
(defvar *clpt-failed* 0)
(defvar *clpt-events* '())
(defvar *clpt-next-id* 0)
(defvar *clpt-provider-calls* 0)
(defvar *clpt-effect-calls* 0)
(defvar *clpt-publication-calls* 0)
(defvar *clpt-fail-types* '())
(defparameter *agent-id* "q5-producer-dev")

(defun clpt-check (name condition)
  (if condition
      (progn (incf *clpt-passed*) (format t "PASS ~a~%" name))
      (progn (incf *clpt-failed*) (format t "FAIL ~a~%" name))))

(defun clpt-signals-p (thunk)
  (handler-case (progn (funcall thunk) nil) (error () t)))

(defun replay-events (&rest ignored)
  (declare (ignore ignored))
  (copy-list *clpt-events*))

(defun log-event (type payload &key caused-by)
  (when (member type *clpt-fail-types* :test #'string=)
    (error "fixture append failure for ~a" type))
  (let* ((id (incf *clpt-next-id*))
         (event (obj "schema_version" 1 "id" id "timestamp" (+ 1000 id)
                     "type" type "agent_id" *agent-id*
                     "caused_by" (or caused-by :null) "payload" payload)))
    (setf *clpt-events* (append *clpt-events* (list event)))
    id))

(defun clpt-source (type intention-id receipt state)
  (log-event type
             (obj "schema_version" 1 "intention_id" intention-id
                  "receipt_id" receipt "state" state "pass_count" 0
                  "detail" :null)))

(defun raw-call-model (&rest ignored)
  (declare (ignore ignored)) (incf *clpt-provider-calls*))
(defun execute (&rest ignored)
  (declare (ignore ignored)) (incf *clpt-effect-calls*))
(defun telegram-send (&rest ignored)
  (declare (ignore ignored)) (incf *clpt-publication-calls*))

(defun clpt-sections (lifecycle-records)
  (obj "identity-instructions"
       (vector (obj "source_id" "policy" "content" "Return structured data."))
       "sensorium" (vector)
       "focus-lifecycles" lifecycle-records
       "triggering-stimuli" (vector)
       "conversation-evidence" (vector)
       "memory-bundles" (vector)
       "untrusted-tool-results" (vector)
       "tools-proposal-schema" (vector)
       "publication-constraints" (vector)))

(defun clpt-context (sections)
  (let ((eligible
          (cons "policy"
                (loop for row across (gethash "focus-lifecycles" sections)
                      collect (gethash "source_id" row)))))
    (make-conscious-assembly-context
     :pulse-id "pulse:q5-producer" :purpose "orient" :audience "operator"
     :runtime-revision "conscious-q5-v2" :conscious-state-revision 1
     :clock-identity "fixture-clock" :sections sections
     :eligible-evidence-ids (coerce eligible 'vector)
     :section-character-budgets
     (obj "identity-instructions" 128 "sensorium" 64
          "focus-lifecycles" 512 "triggering-stimuli" 64
          "conversation-evidence" 64 "memory-bundles" 64
          "untrusted-tool-results" 64 "tools-proposal-schema" 64
          "publication-constraints" 64)
     :total-character-budget 1024 :available-tools (vector)
     :permitted-proposal-kinds (vector "yield")
     :publication-constraints (obj "audiences" (vector "operator"))
     :remaining-budget (obj "tool_proposals" 0 "continuations" 0
                            "publication_candidates" 0))))

(format t "~%== Q5 near-term producer integration subject ==~%")

(let ((source-path
        (merge-pathnames "src/mind/conscious/lifecycle-sources.lisp" *pai-root*)))
  (clpt-check "near-term lifecycle source adapter exists" (probe-file source-path))
  (when (probe-file source-path)
    (load (merge-pathnames "src/mind/conscious/lifecycle.lisp" *pai-root*))
    (load (merge-pathnames "src/mind/conscious/lifecycle-runtime.lisp" *pai-root*))
    (load (merge-pathnames "src/mind/conscious/context-assembly.lisp" *pai-root*))
    (load source-path)

    (setf *clpt-events* '() *clpt-next-id* 0
          *clpt-provider-calls* 0 *clpt-effect-calls* 0
          *clpt-publication-calls* 0 *clpt-fail-types* '())
    (let ((created-id
            (clpt-source "near-term-intention-created"
                         "intention:doorbell" "receipt:doorbell" "seeded")))
      (declare (ignore created-id))
      (let* ((report
               (conscious-lifecycle-runtime-reconcile-producer-events
                *agent-id* :actor-runtime-revision "conscious-q5-v2"))
             (projection (conscious-lifecycle-runtime-project *agent-id*))
             (awaiting (conscious-lifecycle-awaiting projection)))
        (clpt-check "created producer event opens one awaited lifecycle"
                    (and (= 1 (gethash "appended_count" report))
                         (= 1 (length awaiting))
                         (string= "near-term:intention:doorbell"
                                  (gethash "lifecycle_id" (aref awaiting 0))))))

      (clpt-source "near-term-intention-transition"
                   "intention:doorbell" "receipt:doorbell" "evolving")
      (conscious-lifecycle-runtime-reconcile-producer-events
       *agent-id* :actor-runtime-revision "conscious-q5-v2")
      (let* ((evolving-projection
               (conscious-lifecycle-runtime-project *agent-id*))
             (evolving-awaiting
               (conscious-lifecycle-awaiting evolving-projection))
             (evolving-records
               (conscious-lifecycle-context-records evolving-awaiting))
             (evolving-content
               (gethash "content" (aref evolving-records 0))))
        (clpt-source "near-term-intention-transition"
                     "intention:doorbell" "receipt:doorbell" "ready")
        (conscious-lifecycle-runtime-reconcile-producer-events
         *agent-id* :actor-runtime-revision "conscious-q5-v2")
        (let* ((projection (conscious-lifecycle-runtime-project *agent-id*))
             (awaiting (conscious-lifecycle-awaiting projection))
             (records (conscious-lifecycle-context-records awaiting))
             (assembly
               (conscious-context-assemble
                (obj "state_revision" 1 "composition_hash" "q5-fixture")
                (clpt-context (clpt-sections records))))
             (messages (gethash "private_request" assembly))
             (json (shasht:write-json messages nil)))
          (clpt-check "ready producer transition creates a durable checkpoint"
                      (and (= 1 (length awaiting))
                           (string= "receipt:doorbell"
                                    (gethash "checkpoint_ref" (aref awaiting 0)))))
          (clpt-check "evolving and ready remain distinguishable downstream"
                      (and (not (string= evolving-content
                                         (gethash "content" (aref records 0))))
                           (search "near-term-ready"
                                   (gethash "content" (aref records 0)))))
          (clpt-check "bounded lifecycle data reaches real context assembly"
                      (and (search "near-term:intention:doorbell" json)
                           (search "near-term-ready" json)
                           (not (search "detail" json :test #'char-equal))))))

      (let ((before (length *clpt-events*)))
        (setf *conscious-lifecycle-runtime-projection* nil)
        (let ((report
                (conscious-lifecycle-runtime-reconcile-producer-events
                 *agent-id* :actor-runtime-revision "conscious-q5-v2")))
          (clpt-check "restart reconciliation is idempotent"
                      (and (zerop (gethash "appended_count" report))
                           (= before (length *clpt-events*))))))

      (let ((before (length *clpt-events*)) report signalled)
        (handler-case
            (setf report
                  (conscious-lifecycle-runtime-reconcile-producer-events
                   *agent-id* :actor-runtime-revision "conscious-q5-v3"))
          (error () (setf signalled t)))
        (clpt-check "runtime revision bump recovers identity without rewriting provenance"
                    (and (not signalled)
                         (zerop (gethash "appended_count" report -1))
                         (= 3 (gethash "recovered_count" report))
                         (= before (length *clpt-events*)))))

      (clpt-source "near-term-intention-transition"
                   "intention:doorbell" "receipt:doorbell" "discarded")
      (conscious-lifecycle-runtime-reconcile-producer-events
       *agent-id* :actor-runtime-revision "conscious-q5-v2")
      (clpt-check "discarded intention cancels and leaves awaited state"
                  (zerop
                   (length
                    (conscious-lifecycle-awaiting
                     (conscious-lifecycle-runtime-project *agent-id*)))))

      (clpt-source "near-term-intention-transition"
                   "intention:doorbell" "receipt:doorbell" "expressed")
      (let* ((report
               (conscious-lifecycle-runtime-reconcile-producer-events
                *agent-id* :actor-runtime-revision "conscious-q5-v2"))
             (event-count (length *clpt-events*))
             (projection (conscious-lifecycle-runtime-project *agent-id*))
             (row
               (conscious-lifecycle-current
                projection
                "near-term:intention:doorbell")))
        (let ((recovery-report
                (conscious-lifecycle-runtime-reconcile-producer-events
                 *agent-id* :actor-runtime-revision "conscious-q5-v3")))
          (clpt-check "late producer completion cannot cross cancellation"
                      (and (= 1 (gethash "rejected_count" report))
                           (zerop (gethash "rejected_count" recovery-report))
                           (string= "cancelled" (gethash "status" row))
                           (= 1 (gethash "source_rejected_count" projection))
                           (= event-count (length *clpt-events*))
                           (= 1 (count "conscious-lifecycle-source-rejected"
                                       *clpt-events*
                                       :key (lambda (event)
                                              (gethash "type" event))
                                       :test #'string=))))))

      (clpt-source "near-term-intention-transition"
                   "intention:malformed" "receipt:malformed" "invented-state")
      (let ((report
              (conscious-lifecycle-runtime-reconcile-producer-events
               *agent-id* :actor-runtime-revision "conscious-q5-v2")))
        (clpt-check "unsupported producer state is diagnosed without lifecycle"
                    (and (= 1 (gethash "invalid_count" report))
                         (null
                          (conscious-lifecycle-current
                           (conscious-lifecycle-runtime-project *agent-id*)
                           "near-term:intention:malformed")))))

      (let ((expressed-source
              (progn
                (clpt-source "near-term-intention-created"
                             "intention:expressed" "receipt:expressed" "seeded")
                (clpt-source "near-term-intention-transition"
                             "intention:expressed" "receipt:expressed"
                             "expressed"))))
        (conscious-lifecycle-runtime-reconcile-producer-events
         *agent-id* :actor-runtime-revision "conscious-q5-v2")
        (let ((row
                (conscious-lifecycle-current
                 (conscious-lifecycle-runtime-project *agent-id*)
                 "near-term:intention:expressed")))
          (clpt-check "expressed intention completes with its source receipt"
                      (and (string= "completed" (gethash "status" row))
                           (= expressed-source
                              (gethash "terminal_source_event_id" row)))))))

      ;; The preserved legacy log may contain duplicate numeric IDs. Only the
      ;; newest exact source can be named by today's ID-only receipt schema.
      (let ((older
              (obj "schema_version" 1 "id" 500 "timestamp" 1500
                   "type" "near-term-intention-created" "agent_id" *agent-id*
                   "caused_by" :null "payload"
                   (obj "schema_version" 1 "intention_id" "duplicate:old"
                        "receipt_id" "duplicate-receipt:old" "state" "seeded"
                        "pass_count" 0 "detail" :null)))
            (newer
              (obj "schema_version" 1 "id" 500 "timestamp" 1501
                   "type" "near-term-intention-created" "agent_id" *agent-id*
                   "caused_by" :null "payload"
                   (obj "schema_version" 1 "intention_id" "duplicate:new"
                        "receipt_id" "duplicate-receipt:new" "state" "seeded"
                        "pass_count" 0 "detail" :null))))
        (setf *clpt-events* (list older newer) *clpt-next-id* 500)
        (let ((report
                (conscious-lifecycle-runtime-reconcile-producer-events
                 *agent-id* :actor-runtime-revision "conscious-q5-v2")))
          (clpt-check "legacy duplicate IDs map only the newest exact source"
                      (and (= 1 (gethash "invalid_count" report))
                           (= 1 (gethash "appended_count" report))
                           (null
                            (conscious-lifecycle-current
                             (conscious-lifecycle-runtime-project *agent-id*)
                             "near-term:duplicate:old"))
                           (hash-table-p
                            (conscious-lifecycle-current
                             (conscious-lifecycle-runtime-project *agent-id*)
                             "near-term:duplicate:new"))))))

      (setf *clpt-events* '() *clpt-next-id* 0)
      (clpt-source "near-term-intention-created"
                   "intention:outage" "receipt:outage" "seeded")
      (setf *clpt-fail-types* '("conscious-lifecycle-transition"))
      (clpt-check "durable append outage propagates instead of becoming rejection"
                  (clpt-signals-p
                   (lambda ()
                     (conscious-lifecycle-runtime-reconcile-producer-events
                      *agent-id* :actor-runtime-revision "conscious-q5-v2"))))
      (let ((report (conscious-lifecycle-source-report)))
        (clpt-check "operational outage is reported as failed and never reconciled"
                    (and (string= "failed" (gethash "state" report))
                         (= 1 (gethash "operational_failure_count" report))
                         (zerop (count "conscious-lifecycle-transition"
                                       *clpt-events*
                                       :key (lambda (event) (gethash "type" event))
                                       :test #'string=)))))
      (setf *clpt-fail-types* '() *clpt-events* '() *clpt-next-id* 0)
      (dotimes (index 80)
        (clpt-source "near-term-intention-transition"
                     (format nil "invalid:~a" index)
                     (format nil "receipt:~a" index) "invented-state"))
      (let ((report
              (conscious-lifecycle-runtime-reconcile-producer-events
               *agent-id* :actor-runtime-revision "conscious-q5-v2")))
        (clpt-check "operator diagnostic IDs are bounded with explicit truncation"
                    (and (= 80 (gethash "invalid_count" report))
                         (<= (length (gethash "invalid_source_event_ids" report)) 32)
                         (gethash "invalid_ids_truncated" report))))

    (clpt-check "producer reconciliation calls no provider effect or publication"
                (and (zerop *clpt-provider-calls*)
                     (zerop *clpt-effect-calls*)
                     (zerop *clpt-publication-calls*)))
    (let ((source (uiop:read-file-string source-path)))
      (clpt-check "source adapter has no forbidden runtime route"
                  (notany (lambda (needle)
                            (search needle source :test #'char-equal))
                          '("(raw-call-model" "(call-model" "(execute"
                            "(telegram-send" "(auto-turn")))
      (clpt-check "producer reconciliation owns an explicit loop lock"
                  (and (search "*conscious-lifecycle-source-lock*" source
                               :test #'char-equal)
                       (search "bt:with-lock-held" source :test #'char-equal))))))

(format t "~%~d passed, ~d failed~%" *clpt-passed* *clpt-failed*)
(when (plusp *clpt-failed*) (uiop:quit 1))
