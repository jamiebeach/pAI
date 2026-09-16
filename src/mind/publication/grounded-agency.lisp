;;;; grounded-agency.lisp -- Slice F enqueue-only integration and shadow handoff.
;;;;
;;;; The tick wrapper never runs a model or project operation. Completed
;;;; project post-processing runs on the dedicated worker thread and can only
;;;; reach initiative-v2's observation-only API.

(in-package :agent)

;; Forward declaration for the worker-owned cancellation port loaded next in
;; the serial system. DEFVAR without an initializer proclaims it special but
;; leaves the worker responsible for installing the default function.
(defvar *grounded-agency-cancellation-p-fn*)

(export '(grounded-agency-observe-tick grounded-agency-build-sharing-candidate
          grounded-agency-observe-shadow-candidate grounded-agency-report
          grounded-agency-alerts grounded-agency-state-fingerprint
          grounded-agency-reconcile-completions
          grounded-agency-install-worker-hooks
          grounded-agency-mark-recovery-ready))

(defvar *grounded-agency-recovery-ready-p* nil)
(defvar *grounded-agency-installed-tick-wrapper* nil)
(defvar *grounded-agency-base-tick-once* nil)
(defvar *grounded-agency-stats* (make-hash-table :test #'equal))
(defvar *grounded-agency-stats-lock* (bt:make-lock "grounded-agency-stats"))

(defun %grounded-agency-stat (key)
  (bt:with-lock-held (*grounded-agency-stats-lock*)
    (incf (gethash key *grounded-agency-stats* 0))))

(defun grounded-agency-mark-recovery-ready (&optional (ready t))
  (setf *grounded-agency-recovery-ready-p* (if ready t nil)))

(defun %grounded-agency-enabled-p ()
  (and (%fpe-mode-ready-p) *grounded-agency-recovery-ready-p*))

(defun %grounded-agency-preempted-p ()
  (and (functionp *grounded-agency-cancellation-p-fn*)
       (funcall *grounded-agency-cancellation-p-fn* nil)))

(defun %grounded-agency-cycle-id (tick-result)
  (or (and (hash-table-p tick-result)
           (or (gethash "scheduler_cycle_id" tick-result)
               (gethash "generation_id" tick-result)))
      (format nil "grounded-tick-~a-~6,'0x"
              (get-universal-time) (random #x1000000))))

(defun grounded-agency-observe-tick (tick-result scheduler-cycle-id)
  "Observe one completed tick and enqueue at most one persisted operation.
TICK-RESULT is never modified and no adapter/provider is called here."
  (declare (ignore tick-result))
  (let ((start (get-internal-real-time)))
    (labels ((observe ()
               (cond
                 ((not (%grounded-agency-enabled-p))
                  (%grounded-agency-stat "tick-disabled")
                  (obj "status" "skipped" "reason" "mode-or-recovery"))
                 ((%grounded-agency-preempted-p)
                  (%grounded-agency-stat "tick-preempted")
                  (obj "status" "skipped" "reason" "public-turn-preemption"))
                 (t
                  (let ((claimed nil) (last-reason "no-eligible-project"))
                    (dolist (process
                              (agent-process-list
                               :type "creative-story"
                               :states '("scoped" "active" "incubating" "blocked")
                               :limit 20))
                      (unless claimed
                        (handler-case
                            (setf claimed
                                  (grounded-agency-claim-next-operation
                                   (gethash "id" process) scheduler-cycle-id))
                          (first-person-evidence-error (condition)
                            (setf last-reason
                                  (first-person-evidence-error-code condition))))))
                    (if claimed
                        (progn
                          (%grounded-agency-stat "tick-enqueued")
                          (obj "status" "enqueued"
                               "operation_id" (gethash "id" claimed)
                               "process_id" (gethash "process_id" claimed)))
                        (progn
                          (%grounded-agency-stat "tick-noop")
                          (obj "status" "skipped" "reason" last-reason))))))))
      (let ((thunk #'observe))
        (multiple-value-prog1
            (if (fboundp 'call-with-timing-span)
                (funcall 'call-with-timing-span "grounded-agency.tick-observed"
                         thunk :attributes
                         (obj "scheduler_cycle_id" scheduler-cycle-id))
                (funcall thunk))
          (%grounded-agency-stat
           (format nil "tick-observe-ms/~d"
                   (floor (* 1000.0d0
                             (/ (- (get-internal-real-time) start)
                                (float internal-time-units-per-second 1.0d0)))))))))))

(defun grounded-agency-build-sharing-candidate (process-id &key (now (get-universal-time)))
  (%fpe-require-write-mode)
  (let ((existing
          (with-pg
            (%publication-candidate-row-object
             (pomo:query
              (format nil "~a WHERE process_id=$1 AND status IN ('valid','observed-by-initiative') ORDER BY created_at DESC LIMIT 1"
                      *publication-candidate-select*)
              process-id :row)))))
    (when existing (return-from grounded-agency-build-sharing-candidate existing)))
  (unless (agent-appraisal-current process-id :now now)
    (agent-appraisal-compute process-id :now now))
  (let ((grants (compile-claim-grants process-id *claim-grant-purpose* :now now)))
    (semantic-publication-compile-candidate process-id grants :now now)))

(defun %grounded-agency-latest-candidate (process-id)
  (with-pg
    (%publication-candidate-row-object
     (pomo:query
      (format nil "~a WHERE process_id=$1 ORDER BY created_at DESC,id DESC LIMIT 1"
              *publication-candidate-select*)
      process-id :row))))

(defun grounded-agency-observe-shadow-candidate (candidate-id
                                                  &key (now (get-universal-time)))
  (%fpe-require-write-mode)
  (when (%grounded-agency-preempted-p)
    (%fpe-reject "public-turn-preemption"
                 "candidate observation deferred for the public turn"))
  (let* ((candidate (publication-candidate-get candidate-id))
         (process (and candidate (agent-process-get
                                  (gethash "process_id" candidate)))))
    (unless (and candidate process)
      (%fpe-reject "missing-candidate" "candidate/process does not exist"))
    (let ((evidence
            (map 'vector
                 (lambda (id)
                   (let ((node (funcall *first-person-evidence-node-fn* id)))
                     (unless (%fpe-grounded-node-p node)
                       (%fpe-reject "ungrounded-inspiration"
                                    "candidate inspiration ~a is invalid" id))
                     node))
                 (gethash "inspiration_node_ids" process))))
      (let ((decision
              (initiative-v2-observe-shadow-candidate
               (gethash "rendered_text" candidate) evidence
               :trigger-event-ids (vector)
               :topic (gethash "process_id" candidate)
               :claim-grants (gethash "claim_grants" candidate)
               :candidate-id candidate-id :now now)))
        (when (and decision
                   (fboundp 'reciprocity-canary-consider-observation))
          (funcall 'reciprocity-canary-consider-observation
                   "grounded-project" (gethash "rendered_text" candidate)
                   evidence decision :source-id candidate-id :now now
                   :artifact-class "rendered-draft"
                   :generation-contract
                   (or (gethash "renderer_version" candidate)
                       "semantic-publication-legacy")))
        decision))))

(defun grounded-agency-reconcile-completions (&key (now (get-universal-time)))
  "On the worker only, recover at most one completed project whose candidate
or initiative linkage was interrupted. Existing decisions are relinked without
rescoring by INITIATIVE-V2-OBSERVE-SHADOW-CANDIDATE."
  (unless (and (%grounded-agency-enabled-p)
               (not (%grounded-agency-preempted-p)))
    (return-from grounded-agency-reconcile-completions :deferred))
  (dolist (process (agent-process-list :type "creative-story"
                                       :states '("completed") :limit 20)
                   :none)
    (let ((candidate
            (or (%grounded-agency-latest-candidate (gethash "id" process))
                (grounded-agency-build-sharing-candidate
                 (gethash "id" process) :now now))))
      (when (string= (gethash "status" candidate) "valid")
        (grounded-agency-observe-shadow-candidate
         (gethash "id" candidate) :now now)
        (return :reconciled)))))

(defun %grounded-agency-after-operation (operation result)
  (declare (ignore result))
  (when (and (%grounded-agency-enabled-p)
             (string= (gethash "operation_type" operation) "complete"))
    (let ((candidate
            (grounded-agency-build-sharing-candidate
             (gethash "process_id" operation))))
      (when (string= (gethash "status" candidate) "valid")
        (grounded-agency-observe-shadow-candidate (gethash "id" candidate))))))

(defun grounded-agency-install-worker-hooks ()
  "Connect worker-owned extension points after both modules are loaded."
  (unless (and (boundp '*grounded-agency-after-operation-hook*)
               (boundp '*grounded-agency-idle-hook*))
    (return-from grounded-agency-install-worker-hooks nil))
  (setf (symbol-value '*grounded-agency-after-operation-hook*)
        #'%grounded-agency-after-operation
        (symbol-value '*grounded-agency-idle-hook*)
        #'grounded-agency-reconcile-completions)
  t)

(define-init :install grounded-agency-hooks
    "Install grounded-agency worker hooks."
  (grounded-agency-install-worker-hooks))

(when (fboundp 'tick-once)
  (let ((current (fdefinition 'tick-once)))
    (unless (and *grounded-agency-installed-tick-wrapper*
                 (eq current *grounded-agency-installed-tick-wrapper*))
      (setf *grounded-agency-base-tick-once* current)
      (setf (fdefinition 'tick-once)
            (lambda ()
              (let ((result (funcall *grounded-agency-base-tick-once*)))
                (handler-case
                    (grounded-agency-observe-tick
                     result (%grounded-agency-cycle-id result))
                  (error (condition)
                    (%grounded-agency-stat "tick-contained-error")
                    (%fpe-log
                     "grounded-agency-tick-error"
                     (obj "schema_version" 1 "error_type"
                          (string-downcase (symbol-name (type-of condition)))))))
                result)))
      (setf *grounded-agency-installed-tick-wrapper*
            (fdefinition 'tick-once)))))

(defun %grounded-agency-count-map (sql)
  (let ((result (obj)))
    (dolist (row (pomo:query sql))
      (setf (gethash (first row) result) (second row)))
    result))

(defun %grounded-agency-alert-row (kind entity-id &optional detail)
  (obj "kind" kind "entity_id" entity-id "detail" (or detail :null)))

(defun %grounded-agency-delivery-attempt-count ()
  "Count process-local grounded decisions that reached a delivery branch."
  (if (boundp '*initiative-v2-decisions*)
      (count-if
       (lambda (decision)
         (and (hash-table-p decision)
              (gethash "grounded_candidate_id" decision)
              (or (gethash "delivery_reachable" decision)
                  (string= (gethash "result" decision "")
                           "delivery-attempted"))))
       *initiative-v2-decisions*)
      0))

(defun grounded-agency-alerts ()
  "Evaluate durable grounded-agency invariants without reading private text."
  (handler-case
      (with-pg
        (let ((alerts nil))
          (labels ((collect-query (kind sql)
                     (handler-case
                         (dolist (row (pomo:query sql))
                           (push (%grounded-agency-alert-row kind (first row)
                                                             (second row))
                                 alerts))
                       (error (condition)
                         (push (%grounded-agency-alert-row
                                (format nil "observability-query-failed/~a" kind)
                                "grounded-agency"
                                (string-downcase
                                 (symbol-name (type-of condition))))
                               alerts)))))
            (collect-query
             "artifact-operation-missing-attestation"
             "SELECT o.id,o.operation_type FROM agent_process_operations o WHERE o.status='completed' AND o.operation_type IN ('outline','draft','revise') AND (NOT EXISTS (SELECT 1 FROM agent_attestations a WHERE a.operation_id=o.id AND a.attestation_type='process-progress') OR NOT EXISTS (SELECT 1 FROM agent_attestations a WHERE a.operation_id=o.id AND a.attestation_type IN ('artifact-created','artifact-revised'))) ORDER BY o.id")
            (collect-query
             "validation-operation-missing-attestation"
             "SELECT o.id,o.operation_type FROM agent_process_operations o WHERE o.status='completed' AND o.operation_type='validate' AND NOT EXISTS (SELECT 1 FROM agent_attestations a WHERE a.operation_id=o.id AND a.attestation_type='artifact-validated') ORDER BY o.id")
            (collect-query
             "completion-operation-missing-attestation"
             "SELECT o.id,o.operation_type FROM agent_process_operations o WHERE o.status='completed' AND o.operation_type='complete' AND (NOT EXISTS (SELECT 1 FROM agent_attestations a WHERE a.operation_id=o.id AND a.attestation_type='process-completed') OR NOT EXISTS (SELECT 1 FROM agent_attestations a WHERE a.operation_id=o.id AND a.attestation_type='artifact-completed')) ORDER BY o.id")
            (collect-query
             "completed-process-missing-artifact"
             "SELECT p.id,p.state FROM agent_processes p WHERE p.state IN ('completed','shared') AND NOT EXISTS (SELECT 1 FROM agent_artifacts a WHERE a.process_id=p.id AND a.status='completed') ORDER BY p.id")
            (collect-query
             "artifact-current-version-inconsistent"
             "SELECT a.id,a.current_version::text FROM agent_artifacts a LEFT JOIN agent_artifact_versions v ON v.artifact_id=a.id AND v.version=a.current_version WHERE a.current_version>0 AND (v.version IS NULL OR length(v.sha256)<>64) ORDER BY a.id")
            (collect-query
             "expired-transient-candidate"
             "SELECT id,status FROM publication_candidates WHERE status IN ('valid','observed-by-initiative') AND candidate_valid_until IS NOT NULL AND candidate_valid_until<=now() ORDER BY id")
            (collect-query
             "invalid-valid-candidate"
             "SELECT id,status FROM publication_candidates WHERE status='valid' AND (coalesce((validation->>'clause_valid')::boolean,false)=false OR coalesce((validation->>'composition_valid')::boolean,false)=false) ORDER BY id")
            (collect-query
             "delivery-authority-violation"
             "SELECT id,status FROM publication_candidates WHERE delivery_allowed=true ORDER BY id")
            (collect-query
             "stuck-operation-lease"
             "SELECT id,status FROM agent_process_operations WHERE (status='claimed' AND claimed_at<now()-interval '5 minutes') OR (status='running' AND lease_expires_at<=now()) ORDER BY id")
            (collect-query
             "proposal-admission-inconsistent"
             "SELECT id,status FROM grounded_project_proposals WHERE (status='admitted')<>(admitted_process_id IS NOT NULL) ORDER BY id")
            (collect-query
             "project-budget-exceeded"
             "SELECT p.id,p.state FROM agent_processes p WHERE (SELECT count(*) FROM agent_process_operations o WHERE o.process_id=p.id) > coalesce((p.budget->>'max_operations')::integer,0) OR (SELECT coalesce(sum(o.cost),0) FROM agent_process_operations o WHERE o.process_id=p.id) > coalesce((p.budget->>'max_cost')::double precision,0) ORDER BY p.id")
            (collect-query
             "candidate-missing-attestation"
             "SELECT DISTINCT c.id,c.status FROM publication_candidates c CROSS JOIN LATERAL jsonb_array_elements(c.claim_grants) AS grants(claim_grant) CROSS JOIN LATERAL jsonb_array_elements_text(grants.claim_grant->'attestation_ids') AS ids(attestation_id) LEFT JOIN agent_attestations a ON a.id=ids.attestation_id WHERE a.id IS NULL ORDER BY c.id"))
          (when (boundp '*initiative-v2-decisions*)
            (dolist (decision *initiative-v2-decisions*)
              (when (and (hash-table-p decision)
                         (gethash "grounded_candidate_id" decision)
                         (or (gethash "delivery_reachable" decision)
                             (string= (gethash "result" decision "")
                                      "delivery-attempted")))
                (push (%grounded-agency-alert-row
                       "grounded-delivery-attempt"
                       (gethash "grounded_candidate_id" decision)
                       (gethash "id" decision))
                      alerts))))
          (coerce (nreverse alerts) 'vector)))
    (error (condition)
      (vector (%grounded-agency-alert-row
               "observability-query-failed" "grounded-agency"
               (string-downcase (symbol-name (type-of condition))))))))

(defun %grounded-agency-db-report ()
  (with-pg
    (let* ((project-metrics
             (map 'vector
                  (lambda (row)
                    (destructuring-bind
                        (id state progress-age operations model-calls prompt
                         completion cost retries artifacts versions)
                        row
                      (obj "process_id" id "state" state
                           "last_progress_age_seconds" (or progress-age :null)
                           "operation_count" operations "model_calls" model-calls
                           "prompt_tokens" prompt "completion_tokens" completion
                           "cost" cost "retries" retries
                           "artifact_count" artifacts "artifact_version_count" versions)))
                  (pomo:query
                   "SELECT p.id,p.state,CASE WHEN p.last_meaningful_progress_at IS NULL THEN NULL ELSE extract(epoch from(now()-p.last_meaningful_progress_at))::bigint END,count(o.id),count(o.id) FILTER (WHERE o.model_name IS NOT NULL),coalesce(sum(o.prompt_tokens),0),coalesce(sum(o.completion_tokens),0),coalesce(sum(o.cost),0),count(o.id) FILTER (WHERE o.retry_of_operation_id IS NOT NULL),(SELECT count(*) FROM agent_artifacts a WHERE a.process_id=p.id),(SELECT count(*) FROM agent_artifact_versions v JOIN agent_artifacts a ON a.id=v.artifact_id WHERE a.process_id=p.id) FROM agent_processes p LEFT JOIN agent_process_operations o ON o.process_id=p.id GROUP BY p.id,p.state,p.last_meaningful_progress_at ORDER BY p.created_at,p.id")))
           (appraisals
             (map 'vector
                  (lambda (row)
                    (obj "process_id" (first row) "label" (second row)
                         "valid_until" (third row)))
                  (pomo:query
                   "SELECT process_id,facts->>'label',valid_until::text FROM agent_attestations WHERE attestation_type='subjective-appraisal' AND superseded_at IS NULL ORDER BY issued_at DESC")))
           (active
             (pomo:query
              "SELECT id,process_id,operation_type,status,lease_owner,CASE WHEN heartbeat_at IS NULL THEN NULL ELSE extract(epoch from(now()-heartbeat_at))::bigint END FROM agent_process_operations WHERE status IN ('claimed','running') ORDER BY claimed_at,id LIMIT 1"
              :row))
           (completed-model
             (pomo:query
              "SELECT count(*) FILTER (WHERE meaningful_progress=true),count(*) FROM agent_process_operations WHERE status='completed' AND operation_type IN ('outline','draft','revise')"
              :row)))
      (obj
       "proposal_counts" (%grounded-agency-count-map
                           "SELECT status,count(*) FROM grounded_project_proposals GROUP BY status ORDER BY status")
       "process_counts" (%grounded-agency-count-map
                          "SELECT state,count(*) FROM agent_processes GROUP BY state ORDER BY state")
       "operation_type_status" (%grounded-agency-count-map
                                 "SELECT operation_type||'/'||status,count(*) FROM agent_process_operations GROUP BY operation_type,status ORDER BY operation_type,status")
       "candidate_counts" (%grounded-agency-count-map
                            "SELECT status,count(*) FROM publication_candidates GROUP BY status ORDER BY status")
       "attestation_counts" (%grounded-agency-count-map
                              "SELECT attestation_type,count(*) FROM agent_attestations GROUP BY attestation_type ORDER BY attestation_type")
       "active_count" (pomo:query
                        "SELECT count(*) FROM agent_processes WHERE state='active'" :single)
       "active_limit" 2
       "incubating_count" (pomo:query
                            "SELECT count(*) FROM agent_processes WHERE state='incubating'" :single)
       "incubating_limit" 3
       "queue_depth" (pomo:query
                       "SELECT count(*) FROM agent_process_operations WHERE status='claimed'" :single)
       "active_operation"
       (if active
           (obj "operation_id" (first active) "process_id" (second active)
                "operation_type" (third active) "status" (fourth active)
                "lease_owner" (or (fifth active) :null)
                "heartbeat_age_seconds" (or (sixth active) :null))
           :null)
       "meaningful_progress_ratio"
       (if (and completed-model (plusp (second completed-model)))
           (/ (first completed-model) (float (second completed-model) 1.0d0))
           :null)
       "projects" project-metrics
       "current_appraisals" appraisals
       "initiative_correlated_candidates"
       (pomo:query
        "SELECT count(*) FROM publication_candidates WHERE initiative_decision_id IS NOT NULL" :single)
       "delivery_attempts" (%grounded-agency-delivery-attempt-count)))))

(defun grounded-agency-report ()
  (let ((stats (obj)))
    (bt:with-lock-held (*grounded-agency-stats-lock*)
      (maphash (lambda (key value) (setf (gethash key stats) value))
               *grounded-agency-stats*))
    (obj "schema_version" 1
         "mode" (if (boundp '*grounded-agency-mode*)
                    (string-downcase (symbol-name *grounded-agency-mode*))
                    "legacy")
         "recovery_ready" (if *grounded-agency-recovery-ready-p* t nil)
         "authority" (if (fboundp 'first-person-evidence-report)
                         (first-person-evidence-report) :null)
         "worker" (if (fboundp 'grounded-agency-worker-report)
                      (grounded-agency-worker-report) :null)
         "projects" (if (fboundp 'creative-project-report)
                        (creative-project-report) :null)
         "publication" (if (fboundp 'semantic-publication-report)
                            (semantic-publication-report) :null)
         "durable" (handler-case (%grounded-agency-db-report)
                     (error (condition)
                       (obj "unavailable" t "error_class"
                            (string-downcase (symbol-name (type-of condition))))))
         "alerts" (grounded-agency-alerts)
         "stats" stats)))

(defun grounded-agency-state-fingerprint ()
  "Return durable grounded-agency state only; exclude threads, leases, and
process-local counters so pre/post recovery equivalence remains meaningful."
  (handler-case
      (with-pg
        (let ((counts (obj)))
          (dolist
              (row
                (pomo:query
                 "SELECT kind,count FROM (SELECT 'proposal/'||status AS kind,count(*) AS count FROM grounded_project_proposals GROUP BY status UNION ALL SELECT 'process/'||state,count(*) FROM agent_processes GROUP BY state UNION ALL SELECT 'operation/'||status,count(*) FROM agent_process_operations GROUP BY status UNION ALL SELECT 'candidate/'||status,count(*) FROM publication_candidates GROUP BY status) summary ORDER BY kind"))
            (setf (gethash (first row) counts) (second row)))
          (obj "schema_version" 1
               "counts" counts
               "latest_admitted_proposal_id"
               (or (pomo:query
                    "SELECT id FROM grounded_project_proposals WHERE status='admitted' ORDER BY reviewed_at DESC,id DESC LIMIT 1"
                    :single)
                   :null)
               "latest_completed_operation_id"
               (or (pomo:query
                    "SELECT id FROM agent_process_operations WHERE status='completed' ORDER BY finished_at DESC,id DESC LIMIT 1"
                    :single) :null)
               "artifact_count" (pomo:query "SELECT count(*) FROM agent_artifacts" :single)
               "artifact_version_count" (pomo:query "SELECT count(*) FROM agent_artifact_versions" :single)
               "latest_artifact_version_hash"
               (or (pomo:query
                    "SELECT sha256 FROM agent_artifact_versions ORDER BY created_at DESC,artifact_id DESC,version DESC LIMIT 1"
                    :single) :null)
               "attestation_counts"
               (%grounded-agency-count-map
                "SELECT attestation_type,count(*) FROM agent_attestations GROUP BY attestation_type ORDER BY attestation_type")
               "latest_candidate_id"
               (or (pomo:query
                    "SELECT id FROM publication_candidates ORDER BY created_at DESC,id DESC LIMIT 1"
                    :single) :null)
               "latest_candidate_status"
               (or (pomo:query
                    "SELECT status FROM publication_candidates ORDER BY created_at DESC,id DESC LIMIT 1"
                    :single) :null)
               "mode" (if (boundp '*grounded-agency-mode*)
                          (string-downcase (symbol-name *grounded-agency-mode*))
                          "legacy"))))
    (error () (obj "schema_version" 1 "unavailable" t))))
