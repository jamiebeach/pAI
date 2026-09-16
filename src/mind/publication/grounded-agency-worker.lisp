;;;; grounded-agency-worker.lisp -- Slice B isolated deterministic runner.
;;;;
;;;; This file defines a single cooperative worker and injected private
;;;; operation adapters. It is deliberately not loaded by production boot and
;;;; has no tick, initiative, delivery, or provider integration.

(in-package :agent)

(declaim (ftype function grounded-agency-worker-report))

(export '(grounded-agency-claim-next-operation
          grounded-agency-register-operation-adapter
          grounded-agency-clear-operation-adapters
          grounded-agency-worker-signal
          grounded-agency-worker-start grounded-agency-worker-stop
          grounded-agency-worker-run-once grounded-agency-worker-report))

(defparameter *grounded-agency-worker-lease-seconds* 60)
(defparameter *grounded-agency-worker-heartbeat-seconds* 15)
(defparameter *grounded-agency-worker-idle-seconds* 0.25d0)
(defvar *grounded-agency-operation-adapters* (make-hash-table :test #'equal))
(defvar *grounded-agency-worker-thread* nil)
(defvar *grounded-agency-worker-stop-requested* nil)
(defvar *grounded-agency-worker-owner*
  (format nil "worker-boot-~a-~6,'0x" (get-universal-time) (random #x1000000)))
(defvar *grounded-agency-worker-lock* (bt:make-lock "grounded-agency-worker"))
(defvar *grounded-agency-worker-condition* (bt:make-condition-variable))
(defvar *grounded-agency-worker-stats* (make-hash-table :test #'equal))
(defvar *grounded-agency-worker-stats-lock*
  (bt:make-lock "grounded-agency-worker-stats"))
(defvar *grounded-agency-current-usage* nil
  "Dynamically records model usage so rejected attempts still consume budget.")
(defvar *grounded-agency-after-operation-hook* nil
  "Optional worker-thread hook called after a successful terminal operation.")
(defvar *grounded-agency-idle-hook* nil
  "Optional worker-thread reconciliation hook called only when no operation exists.")
(defvar *grounded-agency-cancellation-p-fn*
  (lambda (operation)
    (declare (ignore operation))
    (or (and (boundp '*v2-turn-in-flight*)
             (symbol-value '*v2-turn-in-flight*))
        (and (boundp '*active-public-turn-thread*)
             (symbol-value '*active-public-turn-thread*)
             (bt:thread-alive-p
              (symbol-value '*active-public-turn-thread*)))))
  "Injected public-turn priority/cancellation predicate.")

(defun %gaw-stat (key)
  (bt:with-lock-held (*grounded-agency-worker-stats-lock*)
    (incf (gethash key *grounded-agency-worker-stats* 0))))

(defun %gaw-log (type payload)
  (%fpe-log type payload))

(defun %gaw-operation-cancelled-p (operation)
  (or *grounded-agency-worker-stop-requested*
      (funcall *grounded-agency-cancellation-p-fn* operation)))

(defun grounded-agency-register-operation-adapter (operation-type function)
  "Install an injected Slice B adapter for OUTLINE, DRAFT, or REVISE.
The adapter receives OPERATION, CONTEXT, CANCELLED-P, and HEARTBEAT functions.
It must return a hash table containing content, usage, and optional metadata."
  (let ((type (string-downcase (string operation-type))))
    (unless (member type '("outline" "draft" "revise") :test #'string=)
      (%fpe-reject "adapter-operation-type" "no injected model adapter is allowed for ~a" type))
    (unless (functionp function)
      (%fpe-reject "invalid-adapter" "operation adapter must be callable"))
    (setf (gethash type *grounded-agency-operation-adapters*) function)
    type))

(defun grounded-agency-clear-operation-adapters ()
  (clrhash *grounded-agency-operation-adapters*)
  t)

(defun %gaw-operation-by-status (&key claimed-or-expired)
  (with-pg
    (%fpe-operation-row-object
     (pomo:query
      (concatenate
       'string *fpe-operation-select*
       (if claimed-or-expired
           " WHERE status='claimed' OR (status='running' AND lease_expires_at<=now()) ORDER BY CASE status WHEN 'claimed' THEN 0 ELSE 1 END,claimed_at,id LIMIT 1"
           " WHERE status='claimed' ORDER BY claimed_at,id LIMIT 1"))
      :row))))

(defun %gaw-latest-operation (process-id operation-type)
  (with-pg
    (%fpe-operation-row-object
     (pomo:query
      (format nil "~a WHERE process_id=$1 AND operation_type=$2 ORDER BY claimed_at DESC,id DESC LIMIT 1"
              *fpe-operation-select*)
      process-id operation-type :row))))

(defun %gaw-claim-input (project operation-type)
  (if (string= operation-type "outline")
      (values nil nil)
      (let* ((artifact-id (gethash "current_artifact_id" project))
             (artifact (and (stringp artifact-id)
                            (agent-artifact-get artifact-id))))
        (unless artifact
          (%fpe-reject "missing-current-artifact"
                       "~a requires the project's current artifact" operation-type))
        (values artifact-id (gethash "current_version" artifact)))))

(defun grounded-agency-worker-signal ()
  (bt:with-lock-held (*grounded-agency-worker-lock*)
    (bt:condition-notify *grounded-agency-worker-condition*))
  t)

(defun grounded-agency-claim-next-operation (process-id scheduler-cycle-id)
  "Claim exactly the project's persisted next operation and wake the worker.
This function only performs the short enqueue transaction; it runs no adapter."
  (%fpe-require-write-mode)
  (let* ((process (agent-process-get process-id))
         (project (and process (with-pg (%fpe-project-current process-id))))
         (type (and project (gethash "next_operation_type" project))))
    (unless (and process project type)
      (%fpe-reject "project-not-runnable" "process has no runnable project stage"))
    (multiple-value-bind (artifact-id artifact-version)
        (%gaw-claim-input project type)
      (let* ((latest (%gaw-latest-operation process-id type))
             (failed-p (and latest
                            (member (gethash "status" latest)
                                    '("failed" "rejected" "interrupted")
                                    :test #'string=)))
             (retry-id
               (when failed-p
                 (unless (= 1 (gethash "attempt" latest))
                   (%fpe-reject "retry-exhausted"
                                "the persisted ~a retry has already been consumed" type))
                 (gethash "id" latest)))
             (operation
               (agent-operation-claim
                process-id scheduler-cycle-id type (%fpe-operation-class type)
                :input-artifact-id artifact-id
                :input-artifact-version artifact-version
                :retry-of-operation-id retry-id)))
        (grounded-agency-worker-signal)
        operation))))

(defun %gaw-context (operation)
  (let* ((process-id (gethash "process_id" operation))
         (process (agent-process-get process-id))
         (project (with-pg (%fpe-project-current process-id)))
         (input-id (gethash "input_artifact_id" operation))
         (input-version (gethash "input_artifact_version" operation))
         (input (and (stringp input-id)
                     (integerp input-version)
                     (agent-artifact-get input-id :version input-version
                                                  :include-content t)))
         (inspirations
           (map 'vector
                (lambda (id)
                  (let ((node (funcall *first-person-evidence-node-fn* id)))
                    (unless (%fpe-grounded-node-p node)
                      (%fpe-reject "inspiration-became-ungrounded"
                                   "inspiration ~a is no longer grounded" id))
                    node))
                (gethash "inspiration_node_ids" process))))
    (obj "process_id" process-id
         "operation_id" (gethash "id" operation)
         "operation_type" (gethash "operation_type" operation)
         "scheduler_cycle_id" (gethash "scheduler_cycle_id" operation)
         "why_cares" (gethash "why_cares" project)
         "inspirations" inspirations
         "input_artifact" (or input :null))))

(defun %gaw-result-usage (result)
  (let ((usage (and (hash-table-p result) (gethash "usage" result))))
    (unless (and (hash-table-p usage)
                 (stringp (gethash "model_name" usage))
                 (integerp (gethash "prompt_tokens" usage))
                 (>= (gethash "prompt_tokens" usage) 0)
                 (integerp (gethash "completion_tokens" usage))
                 (>= (gethash "completion_tokens" usage) 0)
                 (numberp (gethash "cost" usage))
                 (>= (gethash "cost" usage) 0))
      (%fpe-reject "adapter-usage" "model adapter must return complete non-negative usage"))
    usage))

(defun %gaw-word-count (content)
  (length (remove-if (lambda (part) (zerop (length part)))
                     (uiop:split-string content
                                        :separator '(#\Space #\Tab #\Newline #\Return)))))

(defun %gaw-validate-adapter-result (operation result)
  (let* ((type (gethash "operation_type" operation))
         (content (and (hash-table-p result) (gethash "content" result)))
         (minimum (if (string= type "outline") 3 8))
         (maximum (if (string= type "outline") 800 2500)))
    (%fpe-string content "adapter artifact content")
    (let ((words (%gaw-word-count content)))
      (unless (<= minimum words maximum)
        (%fpe-reject "artifact-word-bounds"
                     "~a content must contain ~a through ~a words" type minimum maximum))
      (values content (%gaw-result-usage result)
              (obj "passed" t "validation_version" "story-runner-structure-v1"
                   "word_count" words "violation_codes" (vector))))))

(defun %gaw-heartbeat-function (operation owner)
  (let ((last-heartbeat (get-internal-real-time)))
    (lambda (&key force)
      (let ((now (get-internal-real-time)))
        (when (or force
                  (>= (- now last-heartbeat)
                      (* *grounded-agency-worker-heartbeat-seconds*
                         internal-time-units-per-second)))
          (agent-operation-heartbeat
           (gethash "id" operation) owner
           *grounded-agency-worker-lease-seconds*)
          (setf last-heartbeat now)
          t)))))

(defun %gaw-run-model-operation (operation owner)
  (let* ((type (gethash "operation_type" operation))
         (adapter (gethash type *grounded-agency-operation-adapters*)))
    (unless adapter
      (%fpe-reject "missing-adapter" "no private operation adapter is installed for ~a" type))
    (let* ((heartbeat (%gaw-heartbeat-function operation owner))
           (cancelled-p (lambda () (%gaw-operation-cancelled-p operation)))
           (result (funcall adapter operation (%gaw-context operation)
                            cancelled-p heartbeat)))
      (setf *grounded-agency-current-usage* (%gaw-result-usage result))
      (let ((adapter-status (and (hash-table-p result)
                                 (gethash "adapter_status" result))))
        (when (and (stringp adapter-status)
                   (not (string= adapter-status "accepted")))
          (%fpe-reject (or (gethash "adapter_reason" result)
                           "adapter-rejected")
                       "artifact adapter rejected output with status ~a"
                       adapter-status)))
      (when (funcall cancelled-p)
        (agent-operation-interrupt (gethash "id" operation) owner
                                   "public-turn-preemption")
        (return-from %gaw-run-model-operation :interrupted))
      (funcall heartbeat :force t)
      (multiple-value-bind (content usage validation)
          (%gaw-validate-adapter-result operation result)
        (when (fboundp 'creative-project-validate-artifact-output)
          (setf validation
                (funcall 'creative-project-validate-artifact-output
                         operation content (gethash "metadata" result))))
        (agent-artifact-commit-operation
         (gethash "id" operation) owner content
         :artifact-type (if (string= type "outline")
                            "story-outline" "story-draft")
         :validation validation :usage usage
         :metadata (or (gethash "metadata" result)
                       (obj "visibility" "private"
                            "content_class" "synthetic")))))))

(defun %gaw-run-validation (operation owner)
  (let* ((artifact
           (agent-artifact-get
            (gethash "input_artifact_id" operation)
            :version (gethash "input_artifact_version" operation)
            :include-content t))
         (content (and artifact (gethash "content" artifact)))
         (metadata (and artifact (gethash "metadata" artifact)))
         (source-validation
           (and artifact
                (with-pg
                  (%fpe-json-read
                   (pomo:query
                    "SELECT o.validation::text FROM agent_artifact_versions v JOIN agent_process_operations o ON o.id=v.source_operation_id WHERE v.artifact_id=$1 AND v.version=$2"
                    (gethash "id" artifact) (gethash "version" artifact)
                    :single)
                   (obj)))))
         (codes nil))
    (unless (and (stringp content) (plusp (%gaw-word-count content)))
      (push "empty-artifact" codes))
    (unless (and (hash-table-p metadata)
                 (string= (or (gethash "visibility" metadata) "") "private")
                 (string= (or (gethash "content_class" metadata) "") "synthetic"))
      (push "missing-private-synthetic-marker" codes))
    (when (%gaw-operation-cancelled-p operation)
      (agent-operation-interrupt (gethash "id" operation) owner
                                 "public-turn-preemption")
      (return-from %gaw-run-validation :interrupted))
    (if codes
        (agent-operation-fail
         (gethash "id" operation) owner "validation-failed"
         :validation (obj "passed" nil "validation_version" "story-runner-structure-v1"
                          "violation_codes" (coerce (nreverse codes) 'vector)))
        (let ((validation
                (obj "passed" t "artifact_sha256" (gethash "sha256" artifact)
                     "validation_version" "story-runner-structure-v1"
                     "word_count" (%gaw-word-count content)
                     "maximum_prior_version_similarity"
                     (or (and source-validation
                              (gethash "maximum_prior_version_similarity"
                                       source-validation)) 0.0d0)
                     "novelty_method"
                     (or (and source-validation
                              (gethash "novelty_method" source-validation))
                         "not-applicable")
                     "violation_codes" (vector))))
          (agent-validation-commit-operation
           (gethash "id" operation) owner validation)))))

(defun %gaw-run-operation (operation owner)
  (let ((type (gethash "operation_type" operation)))
    (cond ((member type '("outline" "draft" "revise") :test #'string=)
           (%gaw-run-model-operation operation owner))
          ((string= type "validate") (%gaw-run-validation operation owner))
          ((string= type "complete")
           (agent-process-complete-from-validation
            (gethash "id" operation) owner))
          (t (%fpe-reject "unknown-operation" "cannot run operation type ~a" type)))))

(defun %gaw-call-with-trace (operation thunk)
  (if (fboundp 'call-with-timing-trace)
      (funcall 'call-with-timing-trace thunk
               :generation-id (gethash "id" operation)
               :origin "grounded-agency"
               :root-span "grounded-agency.operation"
               :sampled-p t)
      (funcall thunk)))

(defun grounded-agency-worker-run-once (&key (owner *grounded-agency-worker-owner*) now)
  "Lease and execute at most one claimed or expired operation. Return NIL when
there is no work, :PREEMPTED before lease acquisition, or the terminal object."
  (declare (ignore now))
  (%fpe-require-write-mode)
  (let ((candidate (%gaw-operation-by-status :claimed-or-expired t)))
    (unless candidate (return-from grounded-agency-worker-run-once nil))
    (when (%gaw-operation-cancelled-p candidate)
      (%gaw-stat "preempted-before-lease")
      (return-from grounded-agency-worker-run-once :preempted))
    (let* ((process (agent-process-get (gethash "process_id" candidate)))
           (operation
             (agent-operation-acquire-lease
              (gethash "id" candidate) owner
              *grounded-agency-worker-lease-seconds*
              :expected-process-version (gethash "version" process))))
      (%gaw-stat (if (string= (gethash "status" candidate) "running")
                     "lease-recovered" "leased"))
      (%gaw-call-with-trace
       operation
       (lambda ()
         (let ((*grounded-agency-current-usage* nil))
           (handler-case
               (let ((result (%gaw-run-operation operation owner)))
                 (%gaw-stat (if (eq result :interrupted) "interrupted" "completed"))
                 (when (and *grounded-agency-after-operation-hook*
                            (not (eq result :interrupted)))
                   (handler-case
                       (funcall *grounded-agency-after-operation-hook* operation result)
                     (error (hook-error)
                       (%gaw-stat "after-operation-hook-error")
                       (%gaw-log
                        "grounded-agency-after-operation-error"
                        (obj "schema_version" 1
                             "operation_id" (gethash "id" operation)
                             "error_type"
                             (string-downcase
                              (symbol-name (type-of hook-error))))))))
                 result)
             (first-person-evidence-error (condition)
               (unless (string= (first-person-evidence-error-code condition) "lease-lost")
                 (ignore-errors
                   (agent-operation-fail
                    (gethash "id" operation) owner
                    (first-person-evidence-error-code condition)
                    :usage *grounded-agency-current-usage*)))
               (%gaw-stat "failed")
               condition)
             (error (condition)
               (ignore-errors
                 (agent-operation-fail (gethash "id" operation) owner
                                       "adapter-error"
                                       :usage *grounded-agency-current-usage*))
               (%gaw-stat "failed")
               condition))))))))

(defun %gaw-worker-loop ()
  (loop until *grounded-agency-worker-stop-requested*
        do (handler-case
               (let ((result (grounded-agency-worker-run-once)))
                 (when (and (null result) *grounded-agency-idle-hook*)
                   (handler-case
                       (funcall *grounded-agency-idle-hook*)
                     (error (hook-error)
                       (%gaw-stat "idle-hook-error")
                       (%gaw-log
                        "grounded-agency-idle-hook-error"
                        (obj "schema_version" 1 "error_type"
                             (string-downcase
                              (symbol-name (type-of hook-error))))))))
                 (when (or (null result) (eq result :preempted))
                   (bt:with-lock-held (*grounded-agency-worker-lock*)
                     (unless *grounded-agency-worker-stop-requested*
                       (bt:condition-wait
                        *grounded-agency-worker-condition*
                        *grounded-agency-worker-lock*
                        :timeout *grounded-agency-worker-idle-seconds*)))))
             (error (condition)
               (%gaw-stat "loop-error")
               (%gaw-log "grounded-agency-worker-error"
                         (obj "schema_version" 1
                              "error_type"
                              (string-downcase
                               (symbol-name (type-of condition)))))
               (sleep *grounded-agency-worker-idle-seconds*)))))

(defun grounded-agency-worker-start ()
  (%fpe-require-write-mode)
  (bt:with-lock-held (*grounded-agency-worker-lock*)
    (unless (and *grounded-agency-worker-thread*
                 (bt:thread-alive-p *grounded-agency-worker-thread*))
      (setf *grounded-agency-worker-stop-requested* nil
            *grounded-agency-worker-thread*
            (bt:make-thread #'%gaw-worker-loop
                            :name "grounded-agency-worker"))))
  (grounded-agency-worker-report))

(defun grounded-agency-worker-stop (&optional (timeout 3))
  (unless (and (numberp timeout) (>= timeout 0) (<= timeout 30))
    (%fpe-reject "invalid-timeout" "worker stop timeout must be between 0 and 30 seconds"))
  (setf *grounded-agency-worker-stop-requested* t)
  (grounded-agency-worker-signal)
  (loop with deadline = (+ (get-internal-real-time)
                           (* timeout internal-time-units-per-second))
        while (and *grounded-agency-worker-thread*
                   (bt:thread-alive-p *grounded-agency-worker-thread*)
                   (< (get-internal-real-time) deadline))
        do (sleep 0.02d0))
  (not (and *grounded-agency-worker-thread*
            (bt:thread-alive-p *grounded-agency-worker-thread*))))

(defun grounded-agency-worker-report ()
  (let ((stats (obj)))
    (bt:with-lock-held (*grounded-agency-worker-stats-lock*)
      (maphash (lambda (key value) (setf (gethash key stats) value))
               *grounded-agency-worker-stats*))
    (obj "schema_version" 1
         "running" (if (and *grounded-agency-worker-thread*
                             (bt:thread-alive-p *grounded-agency-worker-thread*))
                        t nil)
         "stop_requested" (if *grounded-agency-worker-stop-requested* t nil)
         "owner" *grounded-agency-worker-owner*
         "lease_seconds" *grounded-agency-worker-lease-seconds*
         "heartbeat_seconds" *grounded-agency-worker-heartbeat-seconds*
         "adapter_count" (hash-table-count *grounded-agency-operation-adapters*)
         "stats" stats)))

(when (fboundp 'grounded-agency-install-worker-hooks)
  (grounded-agency-install-worker-hooks))
