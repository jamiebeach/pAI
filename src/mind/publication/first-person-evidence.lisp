;;;; first-person-evidence.lisp -- grounded-agency Slice A durable authority.
;;;;
;;;; Runtime attestations, immutable artifact versions, and the operation
;;;; ledger are authoritative for the agent's own process claims. This module has
;;;; no model, tick, publication, initiative, or delivery path.

(in-package :agent)

(declaim (ftype function first-person-evidence-schema-report
                %fpe-artifact-current))

(ql:quickload '(:ironclad :babel) :silent t)

(export '(ensure-first-person-evidence-schema
          first-person-evidence-schema-report first-person-evidence-report
          grounded-project-proposal-create grounded-project-proposal-review
          agent-process-start-from-approved-proposal
          agent-process-transition agent-process-get agent-process-list
          agent-operation-claim agent-operation-acquire-lease
          agent-operation-heartbeat agent-operation-fail
          agent-operation-interrupt
          agent-artifact-commit-operation agent-validation-commit-operation
          agent-process-complete-from-validation agent-artifact-get
          agent-attestation-query first-person-evidence-error))

(define-condition first-person-evidence-error (error)
  ((code :initarg :code :reader first-person-evidence-error-code)
   (detail :initarg :detail :reader first-person-evidence-error-detail))
  (:report (lambda (condition stream)
             (format stream "First-person evidence rejected [~a]: ~a"
                     (first-person-evidence-error-code condition)
                     (first-person-evidence-error-detail condition)))))

(defparameter *first-person-evidence-schema-version* 1)
(defparameter *first-person-evidence-default-budget*
  (obj "max_operations" 6
       "max_model_operations" 4
       "max_retries_per_operation" 1
       "max_prompt_tokens" 12000
       "max_completion_tokens" 6000
       "max_cost" 0.05d0
       "expires_after_seconds" 604800
       "minimum_seconds_between_operations" 900))
(defparameter *first-person-evidence-operation-classes*
  '(("outline" . "model-generation")
    ("draft" . "model-generation")
    ("revise" . "model-generation")
    ("validate" . "validation")
    ("complete" . "deterministic")))
(defparameter *first-person-evidence-active-states*
  '("sparked" "considering" "scoped" "active" "incubating" "blocked"))
(defparameter *first-person-evidence-terminal-states*
  '("completed" "shared" "abandoned"))
(defparameter *first-person-evidence-transitions*
  '(("sparked" "considering" "abandoned")
    ("considering" "scoped" "abandoned")
    ("scoped" "active" "blocked" "abandoned")
    ("active" "incubating" "blocked" "completed" "abandoned")
    ("incubating" "active" "abandoned")
    ("blocked" "active" "abandoned")
    ("completed" "shared")))
(defvar *first-person-evidence-node-fn*
  (lambda (id)
    (and (fboundp 'memory-get-node) (funcall 'memory-get-node id)))
  "Injected grounded-memory resolver used by proposal admission tests.")
(defvar *first-person-evidence-event-fn*
  (lambda (type payload)
    (when (fboundp 'log-event) (funcall 'log-event type payload)))
  "Content-free event sink.")
(defvar *first-person-evidence-before-terminal-hook* nil
  "Test-only hook called inside a terminal transaction before its final writes.")
(defvar *first-person-evidence-stats* (make-hash-table :test #'equal))
(defvar *first-person-evidence-stats-lock*
  (bt:make-lock "first-person-evidence-stats"))

(defun %fpe-reject (code control &rest arguments)
  (error 'first-person-evidence-error
         :code code :detail (apply #'format nil control arguments)))

(defun %fpe-stat (key)
  (bt:with-lock-held (*first-person-evidence-stats-lock*)
    (incf (gethash key *first-person-evidence-stats* 0))))

(defun %fpe-list (value)
  (cond ((null value) nil)
        ((listp value) (copy-list value))
        ((vectorp value) (coerce value 'list))
        (t (list value))))

(defun %fpe-vector (value)
  (coerce (%fpe-list value) 'vector))

(defun %fpe-json (value)
  (let ((*print-pretty* nil))
    (shasht:write-json value nil)))

(defun %fpe-json-read (value &optional fallback)
  (if (and (stringp value) (plusp (length value)))
      (handler-case (shasht:read-json value) (error () fallback))
      fallback))

(defun %fpe-sql-null (value)
  (if (null value) :null value))

(defun %fpe-id (prefix)
  (format nil "~a-~a-~6,'0x" prefix (get-universal-time) (random #x1000000)))

(defun %fpe-mode-ready-p ()
  (and (boundp '*grounded-agency-mode*)
       (eq *grounded-agency-mode* :shadow)
       (boundp '*autonomous-write-mode*)
       (eq *autonomous-write-mode* :normal)))

(defun %fpe-require-write-mode ()
  (unless (%fpe-mode-ready-p)
    (%fpe-reject "mode-blocked"
                 "writes require grounded-agency :shadow and autonomous-write :normal")))

(defun %fpe-string (value label)
  (unless (and (stringp value) (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) value))))
    (%fpe-reject "invalid-field" "~a must be a non-empty string" label))
  value)

(defun %fpe-copy-object (value)
  (let ((copy (obj)))
    (when (hash-table-p value)
      (maphash (lambda (key item) (setf (gethash key copy) item)) value))
    copy))

(defun %fpe-budget (budget)
  (let ((result (%fpe-copy-object *first-person-evidence-default-budget*)))
    (when (hash-table-p budget)
      (maphash (lambda (key value)
                 (when (nth-value 1 (gethash key result))
                   (unless (and (numberp value) (>= value 0))
                     (%fpe-reject "invalid-budget" "~a must be non-negative" key))
                   (when (> value (gethash key *first-person-evidence-default-budget*))
                     (%fpe-reject "budget-expansion"
                                  "~a cannot exceed the Slice A ceiling" key))
                   (setf (gethash key result) value)))
               budget))
    result))

(defun %fpe-serialization-failure-p (condition)
  (if (fboundp '%epistemic-serialization-failure-p)
      (funcall '%epistemic-serialization-failure-p condition)
      nil))

(defun %fpe-run-serializable (thunk &key (max-attempts 8))
  (loop for attempt from 1 to max-attempts
        do (handler-case (return (funcall thunk))
             (error (condition)
               (unless (and (< attempt max-attempts)
                            (%fpe-serialization-failure-p condition))
                 (error condition))
               (%fpe-stat "serialization-retry")
               (sleep (min 0.05d0 (* attempt 0.005d0)))))))

(defun %fpe-log (type payload)
  (ignore-errors (funcall *first-person-evidence-event-fn* type payload)))

(defun %fpe-grounded-row-primary-key (table row)
  (cond
    ((member table '("grounded_project_proposals" "agent_processes"
                     "creative_projects" "agent_process_operations"
                     "agent_artifacts" "agent_attestations"
                     "publication_candidates") :test #'string=)
     (obj "id" (gethash "id" row)))
    ((string= table "agent_artifact_versions")
     (obj "artifact_id" (gethash "artifact_id" row)
          "version" (gethash "version" row)))
    (t (error "Unsupported grounded-agency event table ~a" table))))

(defun %fpe-emit-grounded-row-json (table row-json)
  (when (and row-json (fboundp 'log-postgres-row-state))
    (ignore-errors
      (let ((row (if (stringp row-json)
                     (shasht:read-json row-json)
                     row-json)))
        (funcall 'log-postgres-row-state
                 table "upsert" (%fpe-grounded-row-primary-key table row) row
                 (and (stringp row-json) row-json))))))

(defun %fpe-emit-grounded-query (table sql parameter)
  "Fail-isolated post-commit row observation. SQL strings are fixed call-site
literals; callers never pass user-controlled table or predicate text."
  (when (fboundp 'log-postgres-row-state)
    (ignore-errors
      (let ((rows
              (with-pg
                (pomo:query sql parameter :column))))
        (dolist (row rows) (%fpe-emit-grounded-row-json table row))))))

(defun %fpe-emit-proposal-state (proposal-id)
  (%fpe-emit-grounded-query
   "grounded_project_proposals"
   "SELECT row_to_json(t)::text FROM grounded_project_proposals t WHERE id=$1"
   proposal-id))

(defun %fpe-emit-operation-state (operation-id)
  (%fpe-emit-grounded-query
   "agent_process_operations"
   "SELECT row_to_json(t)::text FROM agent_process_operations t WHERE id=$1"
   operation-id))

(defun %fpe-emit-attestation-state (attestation-id)
  (%fpe-emit-grounded-query
   "agent_attestations"
   "SELECT row_to_json(t)::text FROM agent_attestations t WHERE id=$1"
   attestation-id))

(defun %fpe-emit-process-aggregate-state (process-id)
  "Emit only the bounded rows owned by one grounded creative process."
  (%fpe-emit-grounded-query
   "grounded_project_proposals"
   "SELECT row_to_json(t)::text FROM grounded_project_proposals t WHERE admitted_process_id=$1"
   process-id)
  (%fpe-emit-grounded-query
   "agent_processes"
   "SELECT row_to_json(t)::text FROM agent_processes t WHERE id=$1"
   process-id)
  (%fpe-emit-grounded-query
   "creative_projects"
   "SELECT row_to_json(t)::text FROM creative_projects t WHERE process_id=$1"
   process-id)
  (%fpe-emit-grounded-query
   "agent_process_operations"
   "SELECT row_to_json(t)::text FROM agent_process_operations t WHERE process_id=$1 ORDER BY claimed_at,id"
   process-id)
  (%fpe-emit-grounded-query
   "agent_artifacts"
   "SELECT row_to_json(t)::text FROM agent_artifacts t WHERE process_id=$1 ORDER BY created_at,id"
   process-id)
  (%fpe-emit-grounded-query
   "agent_artifact_versions"
   "SELECT row_to_json(v)::text FROM agent_artifact_versions v JOIN agent_artifacts a ON a.id=v.artifact_id WHERE a.process_id=$1 ORDER BY v.artifact_id,v.version"
   process-id)
  (%fpe-emit-grounded-query
   "agent_attestations"
   "SELECT row_to_json(t)::text FROM agent_attestations t WHERE process_id=$1 ORDER BY issued_at,id"
   process-id))

(defmacro %fpe-with-transaction (options &body body)
  "Use normal Postmodern transactions, except inside the executable recovery
probe where the complete authority chain is already covered by one outer
rollback transaction."
  `(if (and (boundp '*pai-pg-reuse-current-transaction-p*)
            *pai-pg-reuse-current-transaction-p*)
       (progn ,@body)
       (pomo:with-transaction ,options ,@body)))

(defun %fpe-ddl-constraint (table name body)
  (format nil
          "DO $$ BEGIN IF NOT EXISTS (SELECT 1 FROM pg_constraint WHERE conname='~a') THEN ALTER TABLE ~a ADD CONSTRAINT ~a ~a; END IF; END $$"
          name table name body))

(defun ensure-first-person-evidence-schema ()
  "Create the Slice A schema idempotently. Existing unrelated data is untouched."
  (with-pg
    (%fpe-with-transaction ()
      (dolist
          (ddl
            (list
             "CREATE TABLE IF NOT EXISTS grounded_project_proposals (id text PRIMARY KEY, proposal_type text NOT NULL CHECK (proposal_type='creative-story'), status text NOT NULL, why_cares text NOT NULL, source_event_ids jsonb NOT NULL DEFAULT '[]'::jsonb, inspiration_node_ids jsonb NOT NULL DEFAULT '[]'::jsonb, proposed_at timestamptz NOT NULL DEFAULT now(), reviewed_at timestamptz, reviewed_by text, review_reason text, admitted_process_id text UNIQUE, metadata jsonb NOT NULL DEFAULT '{}'::jsonb, CHECK (length(btrim(why_cares)) > 0), CHECK (jsonb_typeof(inspiration_node_ids)='array' AND jsonb_array_length(inspiration_node_ids)>0), CHECK (status IN ('proposed','approved','rejected','admitted')), CHECK ((status='proposed' AND reviewed_at IS NULL AND reviewed_by IS NULL) OR (status<>'proposed' AND reviewed_at IS NOT NULL AND reviewed_by IS NOT NULL)), CHECK ((status='admitted' AND admitted_process_id IS NOT NULL) OR (status<>'admitted' AND admitted_process_id IS NULL)))"
             "CREATE TABLE IF NOT EXISTS agent_processes (id text PRIMARY KEY, process_type text NOT NULL, state text NOT NULL, created_at timestamptz NOT NULL DEFAULT now(), updated_at timestamptz NOT NULL DEFAULT now(), started_at timestamptz, last_meaningful_progress_at timestamptz, ended_at timestamptz, source_event_ids jsonb NOT NULL DEFAULT '[]'::jsonb, inspiration_node_ids jsonb NOT NULL DEFAULT '[]'::jsonb, budget jsonb NOT NULL, metadata jsonb NOT NULL DEFAULT '{}'::jsonb, version integer NOT NULL DEFAULT 0 CHECK (version>=0), CHECK (process_type='creative-story'), CHECK (jsonb_typeof(source_event_ids)='array'), CHECK (jsonb_typeof(inspiration_node_ids)='array' AND jsonb_array_length(inspiration_node_ids)>0), CHECK (state IN ('sparked','considering','scoped','active','incubating','blocked','completed','shared','abandoned')), CHECK ((state IN ('completed','shared','abandoned') AND ended_at IS NOT NULL) OR (state NOT IN ('completed','shared','abandoned') AND ended_at IS NULL)), CHECK (started_at IS NULL OR started_at>=created_at), CHECK (ended_at IS NULL OR ended_at>=created_at))"
             "CREATE INDEX IF NOT EXISTS agent_processes_state_idx ON agent_processes(process_type,state,updated_at DESC)"
             "CREATE TABLE IF NOT EXISTS creative_projects (id text PRIMARY KEY, process_id text NOT NULL UNIQUE REFERENCES agent_processes(id) ON DELETE RESTRICT, title text, why_cares text NOT NULL, intended_form text NOT NULL DEFAULT 'short-story', next_operation_type text, current_artifact_id text, sharing_condition text NOT NULL DEFAULT 'completed-and-validated', uncertainty double precision NOT NULL DEFAULT 0.5 CHECK (uncertainty>=0 AND uncertainty<=1), created_at timestamptz NOT NULL DEFAULT now(), updated_at timestamptz NOT NULL DEFAULT now(), metadata jsonb NOT NULL DEFAULT '{}'::jsonb, CHECK (next_operation_type IS NULL OR next_operation_type IN ('outline','draft','revise','validate','complete')))"
             "CREATE TABLE IF NOT EXISTS agent_process_operations (id text PRIMARY KEY, process_id text NOT NULL REFERENCES agent_processes(id) ON DELETE RESTRICT, scheduler_cycle_id text NOT NULL, retry_of_operation_id text REFERENCES agent_process_operations(id) ON DELETE RESTRICT, operation_type text NOT NULL, execution_class text NOT NULL, status text NOT NULL, attempt integer NOT NULL DEFAULT 1 CHECK (attempt IN (1,2)), claimed_at timestamptz NOT NULL DEFAULT now(), started_at timestamptz, finished_at timestamptz, lease_owner text, lease_expires_at timestamptz, heartbeat_at timestamptz, input_artifact_id text, input_artifact_version integer, output_artifact_id text, output_artifact_version integer, meaningful_progress boolean, model_name text, prompt_tokens integer, completion_tokens integer, cost double precision, failure_code text, validation jsonb NOT NULL DEFAULT '{}'::jsonb, UNIQUE(process_id,scheduler_cycle_id), CHECK (operation_type IN ('outline','draft','revise','validate','complete')), CHECK (execution_class IN ('deterministic','retrieval','model-generation','tool-use','validation','artifact-transform')), CHECK ((operation_type IN ('outline','draft','revise') AND execution_class='model-generation') OR (operation_type='validate' AND execution_class='validation') OR (operation_type='complete' AND execution_class='deterministic')), CHECK (status IN ('claimed','running','completed','failed','cancelled','interrupted','rejected')), CHECK ((status='claimed' AND started_at IS NULL AND finished_at IS NULL) OR (status='running' AND started_at IS NOT NULL AND finished_at IS NULL AND lease_owner IS NOT NULL AND lease_expires_at IS NOT NULL AND heartbeat_at IS NOT NULL) OR (status IN ('completed','failed','cancelled','interrupted','rejected') AND started_at IS NOT NULL AND finished_at IS NOT NULL)), CHECK (status<>'completed' OR meaningful_progress IS NOT NULL), CHECK (status='completed' OR meaningful_progress IS DISTINCT FROM true), CHECK (meaningful_progress IS DISTINCT FROM true OR operation_type IN ('outline','draft','revise')), CHECK ((retry_of_operation_id IS NULL AND attempt=1) OR (retry_of_operation_id IS NOT NULL AND attempt=2)), CHECK ((input_artifact_id IS NULL)=(input_artifact_version IS NULL)), CHECK ((output_artifact_id IS NULL)=(output_artifact_version IS NULL)), CHECK (operation_type IN ('outline','draft','revise') OR output_artifact_id IS NULL), CHECK (status<>'completed' OR operation_type NOT IN ('outline','draft','revise') OR output_artifact_id IS NOT NULL), CHECK (cost IS NULL OR cost>=0), CHECK (prompt_tokens IS NULL OR prompt_tokens>=0), CHECK (completion_tokens IS NULL OR completion_tokens>=0))"
             "CREATE INDEX IF NOT EXISTS agent_process_operations_process_idx ON agent_process_operations(process_id,started_at DESC)"
             "CREATE UNIQUE INDEX IF NOT EXISTS agent_process_operations_one_retry_uq ON agent_process_operations(retry_of_operation_id) WHERE retry_of_operation_id IS NOT NULL"
             "CREATE UNIQUE INDEX IF NOT EXISTS agent_process_operations_one_active_uq ON agent_process_operations(process_id) WHERE status IN ('claimed','running')"
             "CREATE TABLE IF NOT EXISTS agent_artifacts (id text PRIMARY KEY, process_id text NOT NULL REFERENCES agent_processes(id) ON DELETE RESTRICT, artifact_type text NOT NULL, status text NOT NULL, current_version integer NOT NULL DEFAULT 0 CHECK (current_version>=0), created_at timestamptz NOT NULL DEFAULT now(), completed_at timestamptz, metadata jsonb NOT NULL DEFAULT '{}'::jsonb, CHECK (artifact_type IN ('story-outline','story-draft')), CHECK (status IN ('active','completed','abandoned')), CHECK ((status='completed' AND completed_at IS NOT NULL) OR (status<>'completed' AND completed_at IS NULL)))"
             "CREATE TABLE IF NOT EXISTS agent_artifact_versions (artifact_id text NOT NULL REFERENCES agent_artifacts(id) ON DELETE RESTRICT, version integer NOT NULL CHECK (version>0), content text NOT NULL, sha256 text NOT NULL CHECK (length(sha256)=64), byte_length integer NOT NULL CHECK (byte_length>=0), source_operation_id text NOT NULL UNIQUE REFERENCES agent_process_operations(id) ON DELETE RESTRICT, created_at timestamptz NOT NULL DEFAULT now(), metadata jsonb NOT NULL DEFAULT '{}'::jsonb, PRIMARY KEY(artifact_id,version), UNIQUE(artifact_id,sha256))"
             "CREATE TABLE IF NOT EXISTS agent_attestations (id text PRIMARY KEY, process_id text REFERENCES agent_processes(id) ON DELETE RESTRICT, operation_id text REFERENCES agent_process_operations(id) ON DELETE RESTRICT, attestation_type text NOT NULL, epistemic_status text NOT NULL, issued_by text NOT NULL, issued_at timestamptz NOT NULL DEFAULT now(), valid_from timestamptz NOT NULL DEFAULT now(), valid_until timestamptz, subject text NOT NULL DEFAULT 'the agent', facts jsonb NOT NULL, source_event_ids jsonb NOT NULL DEFAULT '[]'::jsonb, evidence_node_ids jsonb NOT NULL DEFAULT '[]'::jsonb, artifact_id text, artifact_version integer, derivation_version text, supersedes_attestation_id text REFERENCES agent_attestations(id) ON DELETE RESTRICT, superseded_at timestamptz, CHECK (attestation_type IN ('process-started','process-progress','process-paused','process-completed','process-failed','artifact-created','artifact-revised','artifact-validated','artifact-completed','project-inspired-by','subjective-appraisal','attention-selection')), CHECK (epistemic_status IN ('runtime-attested','derived-current-state')), CHECK (issued_by IN ('first-person-evidence','creative-projects','grounded-appraisal','grounded-agency')), CHECK (valid_until IS NULL OR valid_until>valid_from), CHECK ((artifact_id IS NULL)=(artifact_version IS NULL)), CHECK (attestation_type NOT IN ('process-started','process-progress','process-paused','process-completed','process-failed','project-inspired-by','subjective-appraisal','attention-selection') OR process_id IS NOT NULL), CHECK (attestation_type NOT IN ('artifact-created','artifact-revised','artifact-validated','artifact-completed') OR (artifact_id IS NOT NULL AND artifact_version IS NOT NULL)), CHECK (attestation_type NOT IN ('process-progress','process-completed','process-failed','artifact-created','artifact-revised','artifact-validated','artifact-completed') OR operation_id IS NOT NULL), CHECK (attestation_type<>'subjective-appraisal' OR (epistemic_status='derived-current-state' AND derivation_version IS NOT NULL AND valid_until IS NOT NULL)), UNIQUE(operation_id,attestation_type))"
             "CREATE INDEX IF NOT EXISTS agent_attestations_process_idx ON agent_attestations(process_id,issued_at DESC)"
             "CREATE INDEX IF NOT EXISTS agent_attestations_validity_idx ON agent_attestations(attestation_type,valid_until,issued_at DESC)"
             "CREATE UNIQUE INDEX IF NOT EXISTS agent_attestations_process_terminal_uq ON agent_attestations(process_id,attestation_type) WHERE attestation_type IN ('process-started','process-completed')"
             "CREATE UNIQUE INDEX IF NOT EXISTS agent_attestations_artifact_state_uq ON agent_attestations(artifact_id,artifact_version,attestation_type) WHERE attestation_type IN ('artifact-created','artifact-revised','artifact-validated','artifact-completed')"
             "CREATE UNIQUE INDEX IF NOT EXISTS agent_attestations_current_appraisal_uq ON agent_attestations(process_id,derivation_version) WHERE attestation_type='subjective-appraisal' AND superseded_at IS NULL"
             "CREATE TABLE IF NOT EXISTS publication_candidates (id text PRIMARY KEY, process_id text NOT NULL REFERENCES agent_processes(id) ON DELETE RESTRICT, artifact_id text NOT NULL REFERENCES agent_artifacts(id) ON DELETE RESTRICT, artifact_version integer NOT NULL CHECK (artifact_version>0), purpose text NOT NULL, renderer_version text NOT NULL, validator_version text NOT NULL, claim_grants jsonb NOT NULL, response_plan jsonb NOT NULL, rendered_text text NOT NULL, validation jsonb NOT NULL, status text NOT NULL, delivery_allowed boolean NOT NULL DEFAULT false CHECK (delivery_allowed=false), initiative_decision_id text, compiled_at timestamptz NOT NULL DEFAULT now(), candidate_valid_until timestamptz, revalidated_at timestamptz, created_at timestamptz NOT NULL DEFAULT now(), CHECK (purpose='private-project-sharing-shadow'), CHECK (status IN ('valid','invalid','stale','observed-by-initiative','withheld')), CHECK (candidate_valid_until IS NULL OR candidate_valid_until>compiled_at))"
             (%fpe-ddl-constraint "grounded_project_proposals" "grounded_project_proposals_process_fk" "FOREIGN KEY(admitted_process_id) REFERENCES agent_processes(id) ON DELETE RESTRICT DEFERRABLE INITIALLY DEFERRED")
             (%fpe-ddl-constraint "creative_projects" "creative_projects_current_artifact_fk" "FOREIGN KEY(current_artifact_id) REFERENCES agent_artifacts(id) ON DELETE RESTRICT DEFERRABLE INITIALLY DEFERRED")
             (%fpe-ddl-constraint "agent_process_operations" "agent_operations_input_artifact_version_fk" "FOREIGN KEY(input_artifact_id,input_artifact_version) REFERENCES agent_artifact_versions(artifact_id,version) ON DELETE RESTRICT DEFERRABLE INITIALLY DEFERRED")
             (%fpe-ddl-constraint "agent_process_operations" "agent_operations_output_artifact_version_fk" "FOREIGN KEY(output_artifact_id,output_artifact_version) REFERENCES agent_artifact_versions(artifact_id,version) ON DELETE RESTRICT DEFERRABLE INITIALLY DEFERRED")
             (%fpe-ddl-constraint "agent_attestations" "agent_attestations_artifact_version_fk" "FOREIGN KEY(artifact_id,artifact_version) REFERENCES agent_artifact_versions(artifact_id,version) ON DELETE RESTRICT DEFERRABLE INITIALLY DEFERRED")
             (%fpe-ddl-constraint "publication_candidates" "publication_candidates_artifact_version_fk" "FOREIGN KEY(artifact_id,artifact_version) REFERENCES agent_artifact_versions(artifact_id,version) ON DELETE RESTRICT DEFERRABLE INITIALLY DEFERRED")))
        (pomo:execute ddl))))
  (first-person-evidence-schema-report))

(defun first-person-evidence-schema-report ()
  (with-pg
    (let ((tables
            (pomo:query
             "SELECT table_name FROM information_schema.tables WHERE table_schema='public' AND table_name IN ('grounded_project_proposals','agent_processes','creative_projects','agent_process_operations','agent_artifacts','agent_artifact_versions','agent_attestations','publication_candidates') ORDER BY table_name"
             :column))
          (constraints
            (pomo:query
             "SELECT conname FROM pg_constraint WHERE conname IN ('grounded_project_proposals_process_fk','creative_projects_current_artifact_fk','agent_operations_input_artifact_version_fk','agent_operations_output_artifact_version_fk','agent_attestations_artifact_version_fk','publication_candidates_artifact_version_fk') ORDER BY conname"
             :column)))
      (obj "schema_version" *first-person-evidence-schema-version*
           "table_count" (length tables) "tables" (coerce tables 'vector)
           "composite_constraint_count" (length constraints)
           "composite_constraints" (coerce constraints 'vector)))))

(defun %fpe-grounded-node-p (node)
  (and (hash-table-p node)
       (stringp (gethash "id" node))
       (not (gethash "quarantined" node))
       (member (gethash "grounding_status" node)
               '("grounded" "partially-grounded") :test #'string=)
       (not (member (gethash "epistemic_status" node)
                    '("legacy-unclassified" "rejected") :test #'string=))
       (or (member (gethash "origin_class" node)
                   '("lived-user" "lived-agent-action" "tool-result"
                     "external-signal") :test #'string=)
           (plusp (length (%fpe-list (gethash "root_observation_ids" node)))))))

(defun %fpe-resolve-inspiration (ids)
  (let ((values (remove-duplicates (%fpe-list ids) :test #'string=)))
    (unless values (%fpe-reject "missing-inspiration" "at least one inspiration node is required"))
    (dolist (id values)
      (%fpe-string id "inspiration node id")
      (unless (%fpe-grounded-node-p (funcall *first-person-evidence-node-fn* id))
        (%fpe-reject "ungrounded-inspiration" "node ~a is not grounded" id)))
    values))

(defun %fpe-proposal-row-object (row)
  (when row
    (destructuring-bind (id type status why source-json inspiration-json proposed
                         reviewed reviewer reason process-id metadata-json)
        row
      (obj "id" id "proposal_type" type "status" status "why_cares" why
           "source_event_ids" (%fpe-json-read source-json (vector))
           "inspiration_node_ids" (%fpe-json-read inspiration-json (vector))
           "proposed_at" proposed "reviewed_at" (or reviewed :null)
           "reviewed_by" (or reviewer :null) "review_reason" (or reason :null)
           "admitted_process_id" (or process-id :null)
           "metadata" (%fpe-json-read metadata-json (obj))))))

(defun %fpe-proposal-current (id &key for-update)
  (%fpe-proposal-row-object
   (pomo:query
    (concatenate
     'string
     "SELECT id,proposal_type,status,why_cares,source_event_ids::text,inspiration_node_ids::text,proposed_at::text,reviewed_at::text,reviewed_by,review_reason,admitted_process_id,metadata::text FROM grounded_project_proposals WHERE id=$1"
     (if for-update " FOR UPDATE" ""))
    id :row)))

(defun grounded-project-proposal-create (inspiration-nodes why-cares
                                         &key source-event-ids title metadata)
  (%fpe-require-write-mode)
  (%fpe-string why-cares "why-cares")
  (let* ((inspirations (%fpe-resolve-inspiration inspiration-nodes))
         (id (%fpe-id "project-proposal"))
         (meta (%fpe-copy-object metadata)))
    (when title (setf (gethash "title" meta) title))
    (with-pg
      (pomo:execute
       "INSERT INTO grounded_project_proposals(id,proposal_type,status,why_cares,source_event_ids,inspiration_node_ids,metadata) VALUES($1,'creative-story','proposed',$2,$3::jsonb,$4::jsonb,$5::jsonb)"
       id why-cares (%fpe-json (%fpe-vector source-event-ids))
       (%fpe-json (%fpe-vector inspirations)) (%fpe-json meta)))
    (%fpe-stat "proposal-created")
    (%fpe-log "grounded-project-proposal-created"
              (obj "schema_version" 1 "proposal_id" id
                   "proposal_type" "creative-story"
                   "inspiration_count" (length inspirations)))
    (let ((result (with-pg (%fpe-proposal-current id))))
      (%fpe-emit-proposal-state id)
      result)))

(defun grounded-project-proposal-review (proposal-id decision &key actor reason)
  (%fpe-require-write-mode)
  (%fpe-string actor "actor")
  (let ((target (string-downcase (string decision))))
    (unless (member target '("approved" "rejected") :test #'string=)
      (%fpe-reject "invalid-review" "decision must be approved or rejected"))
    (%fpe-run-serializable
     (lambda ()
       (with-pg
         (%fpe-with-transaction (:serializable)
           (let ((proposal (%fpe-proposal-current proposal-id :for-update t)))
             (unless proposal (%fpe-reject "missing-proposal" "proposal ~a does not exist" proposal-id))
             (unless (string= (gethash "status" proposal) "proposed")
               (%fpe-reject "proposal-already-reviewed" "proposal ~a is ~a"
                            proposal-id (gethash "status" proposal)))
             (pomo:execute
              "UPDATE grounded_project_proposals SET status=$2,reviewed_at=now(),reviewed_by=$3,review_reason=$4 WHERE id=$1"
               proposal-id target actor (%fpe-sql-null reason)))))))
    (%fpe-stat (format nil "proposal-~a" target))
    (%fpe-log "grounded-project-proposal-reviewed"
              (obj "schema_version" 1 "proposal_id" proposal-id
                   "before" "proposed" "after" target
                   "reviewed_by" actor "reason_code" (or reason :null)))
    (let ((result (with-pg (%fpe-proposal-current proposal-id))))
      (%fpe-emit-proposal-state proposal-id)
      result)))

(defun %fpe-attest-current (id process-id operation-id type status issuer facts
                            &key source-event-ids evidence-node-ids artifact-id
                              artifact-version derivation-version valid-until)
  (pomo:execute
   "INSERT INTO agent_attestations(id,process_id,operation_id,attestation_type,epistemic_status,issued_by,facts,source_event_ids,evidence_node_ids,artifact_id,artifact_version,derivation_version,valid_until) VALUES($1,$2,$3,$4,$5,$6,$7::jsonb,$8::jsonb,$9::jsonb,$10,$11,$12,$13)"
   id (%fpe-sql-null process-id) (%fpe-sql-null operation-id)
   type status issuer (%fpe-json facts)
   (%fpe-json (%fpe-vector source-event-ids))
   (%fpe-json (%fpe-vector evidence-node-ids))
   (%fpe-sql-null artifact-id) (%fpe-sql-null artifact-version)
   (%fpe-sql-null derivation-version) (%fpe-sql-null valid-until))
  id)

(defun %fpe-process-row-object (row)
  (when row
    (destructuring-bind (id type state created updated started last-progress ended
                         source-json inspiration-json budget-json metadata-json version)
        row
      (obj "id" id "process_type" type "state" state
           "created_at" created "updated_at" updated
           "started_at" (or started :null)
           "last_meaningful_progress_at" (or last-progress :null)
           "ended_at" (or ended :null)
           "source_event_ids" (%fpe-json-read source-json (vector))
           "inspiration_node_ids" (%fpe-json-read inspiration-json (vector))
           "budget" (%fpe-json-read budget-json (obj))
           "metadata" (%fpe-json-read metadata-json (obj))
           "version" version))))

(defun %fpe-process-current (id &key for-update)
  (%fpe-process-row-object
   (pomo:query
    (concatenate
     'string
     "SELECT id,process_type,state,created_at::text,updated_at::text,started_at::text,last_meaningful_progress_at::text,ended_at::text,source_event_ids::text,inspiration_node_ids::text,budget::text,metadata::text,version FROM agent_processes WHERE id=$1"
     (if for-update " FOR UPDATE" ""))
    id :row)))

(defun agent-process-get (process-id)
  (with-pg (%fpe-process-current process-id)))

(defun agent-process-list (&key type states (limit 50))
  (unless (and (integerp limit) (plusp limit) (<= limit 500))
    (%fpe-reject "invalid-limit" "limit must be an integer from 1 through 500"))
  (with-pg
    (mapcar
     #'%fpe-process-row-object
     (pomo:query
      "SELECT id,process_type,state,created_at::text,updated_at::text,started_at::text,last_meaningful_progress_at::text,ended_at::text,source_event_ids::text,inspiration_node_ids::text,budget::text,metadata::text,version FROM agent_processes WHERE ($1::text IS NULL OR process_type=$1) AND ($2::jsonb='[]'::jsonb OR $2::jsonb ? state) ORDER BY created_at DESC LIMIT $3"
      (%fpe-sql-null type) (%fpe-json (%fpe-vector states)) limit))))

(defun %fpe-project-current (process-id &key for-update)
  (let ((row
          (pomo:query
           (concatenate
            'string
            "SELECT id,process_id,title,why_cares,next_operation_type,current_artifact_id,sharing_condition,uncertainty,metadata::text FROM creative_projects WHERE process_id=$1"
            (if for-update " FOR UPDATE" ""))
           process-id :row)))
    (when row
      (destructuring-bind (id process title why next artifact sharing uncertainty metadata-json) row
        (obj "id" id "process_id" process "title" (or title :null)
             "why_cares" why "next_operation_type" (or next :null)
             "current_artifact_id" (or artifact :null)
             "sharing_condition" sharing "uncertainty" uncertainty
             "metadata" (%fpe-json-read metadata-json (obj)))))))

(defun agent-process-start-from-approved-proposal (proposal-id &key budget metadata)
  (%fpe-require-write-mode)
  (let ((process-id (%fpe-id "process"))
        (project-id (%fpe-id "story-project"))
        (result nil))
    (%fpe-run-serializable
     (lambda ()
       (with-pg
         (%fpe-with-transaction (:serializable)
           (let ((proposal (%fpe-proposal-current proposal-id :for-update t)))
             (unless proposal (%fpe-reject "missing-proposal" "proposal ~a does not exist" proposal-id))
             (unless (string= (gethash "status" proposal) "approved")
               (%fpe-reject "proposal-not-approved" "proposal ~a is ~a"
                            proposal-id (gethash "status" proposal)))
             (let* ((source (%fpe-list (gethash "source_event_ids" proposal)))
                    (inspiration (%fpe-list (gethash "inspiration_node_ids" proposal)))
                    (proposal-meta (gethash "metadata" proposal))
                    (combined (%fpe-copy-object proposal-meta))
                    (title (and (hash-table-p proposal-meta)
                                (gethash "title" proposal-meta))))
               (when (hash-table-p metadata)
                 (maphash (lambda (key value) (setf (gethash key combined) value)) metadata))
               (pomo:execute
                "INSERT INTO agent_processes(id,process_type,state,source_event_ids,inspiration_node_ids,budget,metadata) VALUES($1,'creative-story','scoped',$2::jsonb,$3::jsonb,$4::jsonb,$5::jsonb)"
                process-id (%fpe-json (%fpe-vector source))
                (%fpe-json (%fpe-vector inspiration))
                (%fpe-json (%fpe-budget budget)) (%fpe-json combined))
               (pomo:execute
                "INSERT INTO creative_projects(id,process_id,title,why_cares,next_operation_type,metadata) VALUES($1,$2,$3,$4,'outline',$5::jsonb)"
                project-id process-id (%fpe-sql-null title)
                (gethash "why_cares" proposal)
                (%fpe-json combined))
               (%fpe-attest-current
                (%fpe-id "att") process-id nil "process-started"
                "runtime-attested" "first-person-evidence"
                (obj "process_type" "creative-story" "state" "scoped"
                     "admission_path" (vector "sparked" "considering" "scoped")
                     "proposal_id" proposal-id)
                :source-event-ids source :evidence-node-ids inspiration)
               (%fpe-attest-current
                (%fpe-id "att") process-id nil "project-inspired-by"
                "runtime-attested" "first-person-evidence"
                (obj "project_id" project-id "relation_type" "inspired-by"
                     "rendering_class" "bounded-shared-origin")
                :source-event-ids source :evidence-node-ids inspiration)
               (pomo:execute
                "UPDATE grounded_project_proposals SET status='admitted',admitted_process_id=$2 WHERE id=$1"
                proposal-id process-id)
               (setf result (%fpe-process-current process-id))))))))
    (dolist (transition '(("sparked" "considering") ("considering" "scoped")))
      (%fpe-log "agent-process-transition"
                (obj "schema_version" 1 "process_id" process-id
                     "process_type" "creative-story" "before" (first transition)
                     "after" (second transition) "reason" "operator-approved-admission"
                     "process_version" 0)))
    (%fpe-stat "process-admitted")
    (%fpe-emit-process-aggregate-state process-id)
    result))

(defun %fpe-transition-allowed-p (before after)
  (member after (cdr (assoc before *first-person-evidence-transitions*
                            :test #'string=)) :test #'string=))

(defun agent-process-transition (process-id target-state &key expected-version reason)
  (%fpe-require-write-mode)
  (let ((target (string-downcase (string target-state))) (before nil) (version nil))
    (when (member target '("completed" "shared") :test #'string=)
      (%fpe-reject "protected-transition" "~a requires its dedicated API" target))
    (%fpe-run-serializable
     (lambda ()
       (with-pg
         (%fpe-with-transaction (:serializable)
           (let ((process (%fpe-process-current process-id :for-update t)))
             (unless process (%fpe-reject "missing-process" "process ~a does not exist" process-id))
             (setf before (gethash "state" process))
             (when (and expected-version (/= expected-version (gethash "version" process)))
               (%fpe-reject "stale-process-version" "expected ~a, found ~a"
                            expected-version (gethash "version" process)))
             (unless (%fpe-transition-allowed-p before target)
               (%fpe-reject "invalid-transition" "~a -> ~a is not allowed" before target))
             (when (string= target "active")
               (when (>= (pomo:query "SELECT count(*) FROM agent_processes WHERE state='active' AND id<>$1" process-id :single) 2)
                 (%fpe-reject "active-cap" "two active projects already exist")))
             (when (string= target "incubating")
               (when (>= (pomo:query "SELECT count(*) FROM agent_processes WHERE state='incubating' AND id<>$1" process-id :single) 3)
                 (%fpe-reject "incubating-cap" "three incubating projects already exist")))
             (pomo:execute
              "UPDATE agent_processes SET state=$2,updated_at=now(),ended_at=CASE WHEN $2 IN ('abandoned') THEN now() ELSE ended_at END,version=version+1 WHERE id=$1"
              process-id target)
             (setf version (1+ (gethash "version" process)))
             (when (string= target "incubating")
               (%fpe-attest-current
                (%fpe-id "att") process-id nil "process-paused"
                "runtime-attested" "first-person-evidence"
                (obj "before" before "after" target "reason" (or reason :null)))))))))
    (%fpe-log "agent-process-transition"
              (obj "schema_version" 1 "process_id" process-id
                   "process_type" "creative-story" "before" before "after" target
                   "reason" (or reason :null) "process_version" version))
    (let ((result (agent-process-get process-id)))
      (%fpe-emit-process-aggregate-state process-id)
      result)))

(defun %fpe-operation-row-object (row)
  (when row
    (destructuring-bind
        (id process cycle retry type class status attempt claimed started finished
         owner expires heartbeat input-id input-version output-id output-version
         meaningful model prompt completion cost failure validation-json)
        row
      (obj "id" id "process_id" process "scheduler_cycle_id" cycle
           "retry_of_operation_id" (or retry :null) "operation_type" type
           "execution_class" class "status" status "attempt" attempt
           "claimed_at" claimed "started_at" (or started :null)
           "finished_at" (or finished :null) "lease_owner" (or owner :null)
           "lease_expires_at" (or expires :null) "heartbeat_at" (or heartbeat :null)
           "input_artifact_id" (or input-id :null)
           "input_artifact_version" (or input-version :null)
           "output_artifact_id" (or output-id :null)
           "output_artifact_version" (or output-version :null)
           "meaningful_progress" (if (null meaningful) :null (if meaningful t nil))
           "model_name" (or model :null) "prompt_tokens" (or prompt :null)
           "completion_tokens" (or completion :null) "cost" (or cost :null)
           "failure_code" (or failure :null)
           "validation" (%fpe-json-read validation-json (obj))))))

(defparameter *fpe-operation-select*
  "SELECT id,process_id,scheduler_cycle_id,retry_of_operation_id,operation_type,execution_class,status,attempt,claimed_at::text,started_at::text,finished_at::text,lease_owner,lease_expires_at::text,heartbeat_at::text,input_artifact_id,input_artifact_version,output_artifact_id,output_artifact_version,meaningful_progress,model_name,prompt_tokens,completion_tokens,cost,failure_code,validation::text FROM agent_process_operations")

(defun %fpe-operation-current (id &key for-update)
  (%fpe-operation-row-object
   (pomo:query (format nil "~a WHERE id=$1~a" *fpe-operation-select*
                       (if for-update " FOR UPDATE" ""))
               id :row)))

(defun %fpe-budget-value (budget key)
  (or (and (hash-table-p budget) (gethash key budget))
      (gethash key *first-person-evidence-default-budget*)))

(defun %fpe-operation-usage-current (process-id)
  (destructuring-bind (attempts model-calls prompts completions cost)
      (pomo:query
       "SELECT count(*),count(*) FILTER (WHERE execution_class='model-generation'),coalesce(sum(prompt_tokens),0),coalesce(sum(completion_tokens),0),coalesce(sum(cost),0) FROM agent_process_operations WHERE process_id=$1"
       process-id :row)
    (obj "attempt_count" attempts "model_operation_count" model-calls
         "prompt_tokens_used" prompts "completion_tokens_used" completions
         "cost_used" cost)))

(defun %fpe-require-budget-current (process &key next-class
                                               for-new-operation-p new-usage)
  (let* ((budget (gethash "budget" process))
         (usage (%fpe-operation-usage-current (gethash "id" process)))
         (prompt-new (or (and (hash-table-p new-usage)
                              (gethash "prompt_tokens" new-usage)) 0))
         (completion-new (or (and (hash-table-p new-usage)
                                  (gethash "completion_tokens" new-usage)) 0))
         (cost-new (or (and (hash-table-p new-usage)
                            (gethash "cost" new-usage)) 0.0d0)))
    (when (if for-new-operation-p
              (>= (gethash "attempt_count" usage)
                  (%fpe-budget-value budget "max_operations"))
              (> (gethash "attempt_count" usage)
                 (%fpe-budget-value budget "max_operations")))
      (%fpe-reject "operation-budget" "operation attempt ceiling reached"))
    (when (and (string= (or next-class "") "model-generation")
               (if for-new-operation-p
                   (>= (gethash "model_operation_count" usage)
                       (%fpe-budget-value budget "max_model_operations"))
                   (> (gethash "model_operation_count" usage)
                      (%fpe-budget-value budget "max_model_operations"))))
      (%fpe-reject "model-budget" "model operation ceiling reached"))
    (when (> (+ (gethash "prompt_tokens_used" usage) prompt-new)
             (%fpe-budget-value budget "max_prompt_tokens"))
      (%fpe-reject "prompt-budget" "prompt token ceiling reached"))
    (when (> (+ (gethash "completion_tokens_used" usage) completion-new)
             (%fpe-budget-value budget "max_completion_tokens"))
      (%fpe-reject "completion-budget" "completion token ceiling reached"))
    (when (> (+ (gethash "cost_used" usage) cost-new)
             (%fpe-budget-value budget "max_cost"))
      (%fpe-reject "cost-budget" "cost ceiling reached"))
    usage))

(defun %fpe-operation-class (operation-type)
  (cdr (assoc operation-type *first-person-evidence-operation-classes*
              :test #'string=)))

(defun agent-operation-claim (process-id scheduler-cycle-id operation-type
                              execution-class &key input-artifact-id
                                input-artifact-version retry-of-operation-id)
  (%fpe-require-write-mode)
  (%fpe-string scheduler-cycle-id "scheduler-cycle-id")
  (let* ((type (string-downcase (string operation-type)))
         (class (string-downcase (string execution-class)))
         (expected-class (%fpe-operation-class type))
         (id (%fpe-id "operation"))
         (attempt 1)
         (transition-before nil)
         (transition-version nil)
         (result nil))
    (unless (and expected-class (string= class expected-class))
      (%fpe-reject "operation-class" "~a requires execution class ~a" type expected-class))
    (%fpe-run-serializable
     (lambda ()
       (with-pg
         (%fpe-with-transaction (:serializable)
           (let* ((process (%fpe-process-current process-id :for-update t))
                  (project (and process (%fpe-project-current process-id :for-update t))))
             (unless (and process project)
               (%fpe-reject "missing-project" "process/project ~a does not exist" process-id))
             (unless (member (gethash "state" process)
                             *first-person-evidence-active-states* :test #'string=)
               (%fpe-reject "terminal-process" "process ~a is ~a" process-id
                            (gethash "state" process)))
             (unless (string= type (gethash "next_operation_type" project))
               (%fpe-reject "wrong-next-operation" "expected ~a, got ~a"
                            (gethash "next_operation_type" project) type))
             (if (string= type "outline")
                 (when (or input-artifact-id input-artifact-version)
                   (%fpe-reject "unexpected-input" "outline takes no artifact input"))
                 (progn
                   (unless (and (stringp input-artifact-id)
                                (integerp input-artifact-version)
                                (plusp input-artifact-version))
                     (%fpe-reject "missing-input" "~a requires an exact artifact/version" type))
                   (unless (string= input-artifact-id
                                    (gethash "current_artifact_id" project))
                     (%fpe-reject "stale-input" "input is not the project's current artifact"))
                   (let ((artifact (%fpe-artifact-current
                                    input-artifact-id :version input-artifact-version
                                    :for-update t)))
                     (unless (and artifact
                                  (= input-artifact-version
                                     (gethash "current_version" artifact))
                                  (string= (gethash "artifact_type" artifact)
                                           (if (string= type "draft")
                                               "story-outline" "story-draft")))
                       (%fpe-reject "stale-input"
                                    "input version/type does not match the current project stage")))))
             (when (pomo:query
                    "SELECT 1 FROM agent_process_operations WHERE process_id=$1 AND status IN ('claimed','running') LIMIT 1"
                    process-id :single)
               (%fpe-reject "operation-in-flight" "process already has claimed/running work"))
             (%fpe-require-budget-current process :next-class class
                                                  :for-new-operation-p t)
             (let ((budget (gethash "budget" process)))
               (when (pomo:query
                      "SELECT now() >= created_at + ($2::text||' seconds')::interval FROM agent_processes WHERE id=$1"
                      process-id
                      (%fpe-budget-value budget "expires_after_seconds") :single)
                 (%fpe-reject "project-expired" "project lifetime has expired"))
               (when (pomo:query
                      "SELECT last_meaningful_progress_at IS NOT NULL AND now() < last_meaningful_progress_at + ($2::text||' seconds')::interval FROM agent_processes WHERE id=$1"
                      process-id
                      (%fpe-budget-value budget "minimum_seconds_between_operations")
                      :single)
                 (%fpe-reject "operation-cooldown" "minimum operation interval has not elapsed")))
             (when retry-of-operation-id
               (let ((parent (%fpe-operation-current retry-of-operation-id :for-update t)))
                 (unless parent (%fpe-reject "missing-retry-parent" "retry parent does not exist"))
                 (unless (and (string= (gethash "process_id" parent) process-id)
                              (string= (gethash "operation_type" parent) type)
                              (member (gethash "status" parent)
                                      '("failed" "rejected" "interrupted") :test #'string=)
                              (= (gethash "attempt" parent) 1)
                              (not (string= (gethash "scheduler_cycle_id" parent)
                                            scheduler-cycle-id)))
                   (%fpe-reject "invalid-retry" "retry must follow a failed first attempt in a later cycle"))
                 (when (pomo:query
                        "SELECT 1 FROM agent_process_operations WHERE retry_of_operation_id=$1"
                        retry-of-operation-id :single)
                   (%fpe-reject "retry-exhausted" "retry parent already has a retry"))
                 (setf attempt 2)))
             (pomo:execute
              "INSERT INTO agent_process_operations(id,process_id,scheduler_cycle_id,retry_of_operation_id,operation_type,execution_class,status,attempt,input_artifact_id,input_artifact_version) VALUES($1,$2,$3,$4,$5,$6,'claimed',$7,$8,$9)"
              id process-id scheduler-cycle-id (%fpe-sql-null retry-of-operation-id)
              type class attempt (%fpe-sql-null input-artifact-id)
              (%fpe-sql-null input-artifact-version))
             (when (member (gethash "state" process)
                           '("scoped" "incubating" "blocked") :test #'string=)
               (when (>= (pomo:query
                          "SELECT count(*) FROM agent_processes WHERE state='active' AND id<>$1"
                          process-id :single)
                         2)
                 (%fpe-reject "active-cap"
                              "two active projects already exist"))
               (setf transition-before (gethash "state" process)
                     transition-version (1+ (gethash "version" process)))
               (pomo:execute
                "UPDATE agent_processes SET state='active',started_at=coalesce(started_at,now()),updated_at=now(),version=version+1 WHERE id=$1"
                process-id))
             (setf result (%fpe-operation-current id)))))))
    (%fpe-stat "operation-claimed")
    (%fpe-log "agent-operation-claimed"
              (obj "schema_version" 1 "operation_id" id "process_id" process-id
                   "scheduler_cycle_id" scheduler-cycle-id "operation_type" type
                   "attempt" attempt "retry_of_operation_id"
                   (or retry-of-operation-id :null)))
    (when transition-before
      (%fpe-log "agent-process-transition"
                (obj "schema_version" 1 "process_id" process-id
                     "process_type" "creative-story"
                     "before" transition-before "after" "active"
                     "reason" "first-operation-claimed"
                     "process_version" transition-version)))
    (%fpe-emit-process-aggregate-state process-id)
    result))

(defun agent-operation-acquire-lease (operation-id owner lease-seconds
                                      &key expected-process-version)
  (%fpe-require-write-mode)
  (%fpe-string owner "lease owner")
  (unless (and (integerp lease-seconds) (plusp lease-seconds) (<= lease-seconds 300))
    (%fpe-reject "invalid-lease" "lease seconds must be an integer from 1 to 300"))
  (let ((result nil) (action "acquired"))
    (%fpe-run-serializable
     (lambda ()
       (with-pg
         (%fpe-with-transaction (:serializable)
           (let ((operation (%fpe-operation-current operation-id :for-update t)))
             (unless operation (%fpe-reject "missing-operation" "operation does not exist"))
              (unless (or (string= (gethash "status" operation) "claimed")
                          (and (string= (gethash "status" operation) "running")
                               (not (pomo:query
                                     "SELECT lease_expires_at>now() FROM agent_process_operations WHERE id=$1"
                                     operation-id :single))))
                (%fpe-reject "operation-not-claimable"
                             "operation is ~a with an active or terminal lease"
                             (gethash "status" operation)))
              (when (string= (gethash "status" operation) "running")
                (setf action "recovered"))
             (let ((process (%fpe-process-current (gethash "process_id" operation)
                                                  :for-update t)))
               (when (and expected-process-version
                          (/= expected-process-version (gethash "version" process)))
                 (%fpe-reject "stale-process-version" "expected ~a, found ~a"
                              expected-process-version (gethash "version" process))))
             (pomo:execute
               "UPDATE agent_process_operations SET status='running',started_at=coalesce(started_at,now()),lease_owner=$2,lease_expires_at=now()+($3::text||' seconds')::interval,heartbeat_at=now() WHERE id=$1"
              operation-id owner lease-seconds)
             (setf result (%fpe-operation-current operation-id)))))))
    (%fpe-stat "lease-acquired")
    (%fpe-log "agent-operation-lease"
              (obj "schema_version" 1 "operation_id" operation-id
                   "process_id" (gethash "process_id" result)
                   "scheduler_cycle_id" (gethash "scheduler_cycle_id" result)
                   "attempt" (gethash "attempt" result)
                   "retry_of_operation_id" (gethash "retry_of_operation_id" result)
                   "action" action "lease_owner" owner
                   "lease_expires_at" (gethash "lease_expires_at" result)))
    (%fpe-emit-operation-state operation-id)
    result))

(defun agent-operation-heartbeat (operation-id owner lease-seconds)
  (%fpe-require-write-mode)
  (%fpe-string owner "lease owner")
  (unless (and (integerp lease-seconds) (plusp lease-seconds) (<= lease-seconds 300))
    (%fpe-reject "invalid-lease" "lease seconds must be an integer from 1 to 300"))
  (with-pg
    (%fpe-with-transaction ()
      (unless (= 1 (pomo:execute
                    "UPDATE agent_process_operations SET heartbeat_at=now(),lease_expires_at=now()+($3::text||' seconds')::interval WHERE id=$1 AND status='running' AND lease_owner=$2 AND lease_expires_at>now()"
                    operation-id owner lease-seconds))
        (%fpe-reject "lease-lost" "operation lease is absent, expired, or owned elsewhere"))))
  (let ((result (with-pg (%fpe-operation-current operation-id))))
    (%fpe-emit-operation-state operation-id)
    result))

(defun %fpe-require-owned-running-current (operation-id owner)
  (let ((operation (%fpe-operation-current operation-id :for-update t)))
    (unless operation (%fpe-reject "missing-operation" "operation ~a does not exist" operation-id))
    (unless (and (string= (gethash "status" operation) "running")
                 (stringp (gethash "lease_owner" operation))
                 (string= (gethash "lease_owner" operation) owner)
                 (pomo:query
                  "SELECT 1 FROM agent_process_operations WHERE id=$1 AND status='running' AND lease_owner=$2 AND lease_expires_at>now()"
                  operation-id owner :single))
      (%fpe-reject "lease-lost" "only the owner of an unexpired running lease may commit"))
    operation))

(defun %fpe-usage-values (usage)
  (values (and (hash-table-p usage) (gethash "model_name" usage))
          (or (and (hash-table-p usage) (gethash "prompt_tokens" usage)) 0)
          (or (and (hash-table-p usage) (gethash "completion_tokens" usage)) 0)
          (or (and (hash-table-p usage) (gethash "cost" usage)) 0.0d0)))

(defun %fpe-log-terminal-operation
    (operation-id status meaningful-progress
     &key artifact-id artifact-version artifact-sha256 failure-code)
  "Emit the canonical content-free terminal event from the persisted ledger."
  (let ((operation (with-pg (%fpe-operation-current operation-id))))
    (when operation
      (let* ((validation (gethash "validation" operation))
             (codes (or (and (hash-table-p validation)
                             (gethash "violation_codes" validation))
                        (and (hash-table-p validation)
                             (gethash "codes" validation))
                        (vector))))
        (%fpe-log
         "agent-operation-terminal"
         (obj "schema_version" 1 "operation_id" operation-id
              ;; Required for asynchronous internal results by the
              ;; conscious-state runtime's stimulus envelope. Without it a
              ;; result returning after a revision change cannot be told from
              ;; a current one, so the stale-result rule cannot be honoured --
              ;; and the gap was not merely historical, since new terminals
              ;; were also being written unstamped. An unbound runtime reads
              ;; as UNKNOWN, never as current: treating unknown as current is
              ;; exactly what would let a stale result pass as fresh.
              "origin_runtime_revision"
              (if (and (boundp '*pai-runtime-revision*)
                       (symbol-value '*pai-runtime-revision*))
                  (symbol-value '*pai-runtime-revision*)
                  :null)
              "process_id" (gethash "process_id" operation)
              "scheduler_cycle_id" (gethash "scheduler_cycle_id" operation)
              "operation_type" (gethash "operation_type" operation)
              "status" status "meaningful_progress"
              (if meaningful-progress t nil)
              "artifact_id" (or artifact-id :null)
              "artifact_version" (or artifact-version :null)
              "artifact_sha256" (or artifact-sha256 :null)
              "failure_code" (or failure-code :null)
              "validation_codes" (coerce (%fpe-list codes) 'vector)
              "usage" (obj "model_name" (gethash "model_name" operation)
                           "prompt_tokens" (or (gethash "prompt_tokens" operation) 0)
                           "completion_tokens"
                           (or (gethash "completion_tokens" operation) 0)
                           "cost" (or (gethash "cost" operation) 0.0d0))))))))

(defun %fpe-terminal-failure (operation-id owner status code validation usage)
  (%fpe-require-write-mode)
  (%fpe-string owner "lease owner")
  (%fpe-string code "failure code")
  (let ((process-id nil))
    (%fpe-run-serializable
     (lambda ()
       (with-pg
         (%fpe-with-transaction (:serializable)
           (let ((operation (%fpe-require-owned-running-current operation-id owner)))
             (setf process-id (gethash "process_id" operation))
             (multiple-value-bind (model prompt completion cost) (%fpe-usage-values usage)
               (pomo:execute
                "UPDATE agent_process_operations SET status=$3,finished_at=now(),meaningful_progress=false,model_name=$4,prompt_tokens=$5,completion_tokens=$6,cost=$7,failure_code=$8,validation=$9::jsonb WHERE id=$1 AND lease_owner=$2"
                operation-id owner status (%fpe-sql-null model)
                prompt completion cost code
                (%fpe-json (or validation (obj)))))
             (%fpe-attest-current
              (%fpe-id "att") process-id operation-id "process-failed"
              "runtime-attested" "first-person-evidence"
              (obj "operation_type" (gethash "operation_type" operation)
                   "terminal_status" status "failure_code" code)))))))
    (%fpe-stat (format nil "operation-~a" status))
    (%fpe-log-terminal-operation operation-id status nil :failure-code code)
    (let ((result (with-pg (%fpe-operation-current operation-id))))
      (%fpe-emit-process-aggregate-state process-id)
      result)))

(defun agent-operation-fail (operation-id owner failure-code &key validation usage)
  (%fpe-terminal-failure operation-id owner "failed" failure-code validation usage))

(defun agent-operation-interrupt (operation-id owner reason)
  (%fpe-terminal-failure operation-id owner "interrupted" reason nil nil))

(defun %fpe-normalize-nfc (text)
  (let* ((package (find-package :sb-unicode))
         (symbol (and package (find-symbol "NORMALIZE-STRING" package))))
    (unless (and symbol (fboundp symbol))
      (%fpe-reject "unicode-normalization-unavailable"
                   "SBCL NFC normalization is required for artifact identity"))
    (funcall symbol text :nfc)))

(defun %fpe-canonical-content (content)
  (%fpe-string content "artifact content")
  (let* ((nfc (%fpe-normalize-nfc content))
         (lf (with-output-to-string (out)
               (loop with start = 0
                     for pos = (position #\Return nfc :start start)
                     do (write-string nfc out :start start :end (or pos (length nfc)))
                     while pos
                     do (when (and (< (1+ pos) (length nfc))
                                   (char= (char nfc (1+ pos)) #\Newline))
                          (incf pos))
                        (write-char #\Newline out)
                        (setf start (1+ pos)))))
         (lines (uiop:split-string lf :separator '(#\Newline)))
         (trimmed (mapcar (lambda (line)
                            (string-right-trim '(#\Space #\Tab) line))
                          lines)))
    (format nil "~{~a~^~%~}~%"
            (loop while (and trimmed (zerop (length (car (last trimmed)))))
                  do (setf trimmed (butlast trimmed))
                  finally (return trimmed)))))

(defun %fpe-content-digest (canonical)
  (let* ((octets (babel:string-to-octets canonical :encoding :utf-8))
         (digest (ironclad:digest-sequence :sha256 octets)))
    (values (string-downcase (ironclad:byte-array-to-hex-string digest))
            (length octets))))

(defun %fpe-artifact-current (id &key for-update include-content version)
  (let* ((where-version (if version " AND v.version=$2" " AND v.version=a.current_version"))
         (lock (if for-update " FOR UPDATE OF a" ""))
         (sql
           (format nil
                   "SELECT a.id,a.process_id,a.artifact_type,a.status,a.current_version,a.created_at::text,a.completed_at::text,a.metadata::text,v.version,v.sha256,v.byte_length,v.created_at::text~a FROM agent_artifacts a LEFT JOIN agent_artifact_versions v ON v.artifact_id=a.id WHERE a.id=$1~a~a"
                   (if include-content ",v.content" "") where-version lock))
         (row (if version
                  (pomo:query sql id version :row)
                  (pomo:query sql id :row))))
    (when row
      (let ((base (subseq row 0 12))
            (content (and include-content (nth 12 row))))
        (destructuring-bind (artifact-id process type status current created completed
                             metadata-json actual-version sha bytes version-created)
            base
          (let ((result
                  (obj "id" artifact-id "process_id" process "artifact_type" type
                       "status" status "current_version" current "created_at" created
                       "completed_at" (or completed :null)
                       "metadata" (%fpe-json-read metadata-json (obj))
                       "version" (or actual-version :null) "sha256" (or sha :null)
                       "byte_length" (or bytes :null)
                       "version_created_at" (or version-created :null))))
            (when include-content (setf (gethash "content" result) content))
            result))))))

(defun agent-artifact-get (artifact-id &key version include-content)
  (with-pg (%fpe-artifact-current artifact-id :version version
                                   :include-content include-content)))

(defun %fpe-terminal-hook (stage operation-id)
  (when *first-person-evidence-before-terminal-hook*
    (funcall *first-person-evidence-before-terminal-hook* stage operation-id)))

(defun agent-artifact-commit-operation
    (operation-id owner content &key artifact-id artifact-type validation usage metadata)
  (%fpe-require-write-mode)
  (%fpe-string owner "lease owner")
  (let* ((canonical (%fpe-canonical-content content))
         (sha nil) (bytes nil) (result nil) (process-id nil)
         (final-artifact-id artifact-id) (version nil))
    (multiple-value-setq (sha bytes) (%fpe-content-digest canonical))
    (%fpe-run-serializable
     (lambda ()
       (with-pg
         (%fpe-with-transaction (:serializable)
           (let* ((operation (%fpe-require-owned-running-current operation-id owner))
                  (type (gethash "operation_type" operation))
                  (process (%fpe-process-current (gethash "process_id" operation)
                                                 :for-update t))
                  (project (%fpe-project-current (gethash "process_id" operation)
                                                 :for-update t)))
             (setf process-id (gethash "id" process))
             (unless (member type '("outline" "draft" "revise") :test #'string=)
               (%fpe-reject "not-artifact-operation" "~a cannot commit artifact content" type))
             (unless (string= type (gethash "next_operation_type" project))
               (%fpe-reject "stale-operation" "project now expects ~a"
                            (gethash "next_operation_type" project)))
             (%fpe-require-budget-current process :new-usage usage)
             (let* ((expected-type (if (string= type "outline")
                                       "story-outline" "story-draft"))
                    (existing-id
                      (cond ((string= type "revise")
                             (gethash "current_artifact_id" project))
                            (artifact-id artifact-id)
                            (t nil))))
               (when (and artifact-type (not (string= artifact-type expected-type)))
                 (%fpe-reject "artifact-type" "~a requires ~a" type expected-type))
               (if existing-id
                   (let ((artifact (%fpe-artifact-current existing-id :for-update t)))
                     (unless (and artifact
                                  (string= (gethash "process_id" artifact) process-id)
                                  (string= (gethash "artifact_type" artifact) expected-type)
                                  (string= (gethash "status" artifact) "active"))
                       (%fpe-reject "invalid-artifact" "artifact is missing, foreign, typed differently, or terminal"))
                     (setf final-artifact-id existing-id
                           version (1+ (gethash "current_version" artifact))))
                   (progn
                     (setf final-artifact-id (%fpe-id "artifact") version 1)
                     (pomo:execute
                      "INSERT INTO agent_artifacts(id,process_id,artifact_type,status,metadata) VALUES($1,$2,$3,'active',$4::jsonb)"
                      final-artifact-id process-id expected-type
                      (%fpe-json (or metadata (obj))))))
               (when (pomo:query
                      "SELECT 1 FROM agent_artifact_versions WHERE artifact_id=$1 AND sha256=$2"
                      final-artifact-id sha :single)
                 (%fpe-reject "duplicate-artifact" "canonical artifact content already exists"))
               (%fpe-terminal-hook "before-artifact-version" operation-id)
               (pomo:execute
                "INSERT INTO agent_artifact_versions(artifact_id,version,content,sha256,byte_length,source_operation_id,metadata) VALUES($1,$2,$3,$4,$5,$6,$7::jsonb)"
                final-artifact-id version canonical sha bytes operation-id
                (%fpe-json (or metadata (obj))))
               (pomo:execute
                "UPDATE agent_artifacts SET current_version=$2 WHERE id=$1"
                final-artifact-id version)
               (multiple-value-bind (model prompt completion cost) (%fpe-usage-values usage)
                 (pomo:execute
                  "UPDATE agent_process_operations SET status='completed',finished_at=now(),meaningful_progress=true,model_name=$3,prompt_tokens=$4,completion_tokens=$5,cost=$6,output_artifact_id=$7,output_artifact_version=$8,validation=$9::jsonb WHERE id=$1 AND lease_owner=$2"
                  operation-id owner (%fpe-sql-null model) prompt completion cost final-artifact-id
                  version (%fpe-json (or validation (obj)))))
               (pomo:execute
                "UPDATE agent_processes SET last_meaningful_progress_at=now(),updated_at=now(),version=version+1 WHERE id=$1"
                process-id)
               (pomo:execute
                "UPDATE creative_projects SET current_artifact_id=$2,next_operation_type=$3,updated_at=now() WHERE process_id=$1"
                process-id final-artifact-id
                (cond ((string= type "outline") "draft")
                      ((string= type "draft") "revise")
                      (t "validate")))
               (%fpe-attest-current
                (%fpe-id "att") process-id operation-id
                (if (= version 1) "artifact-created" "artifact-revised")
                "runtime-attested" "first-person-evidence"
                (obj "operation_type" type "sha256" sha "byte_length" bytes)
                :artifact-id final-artifact-id :artifact-version version)
               (%fpe-attest-current
                (%fpe-id "att") process-id operation-id "process-progress"
                "runtime-attested" "first-person-evidence"
                (obj "operation_type" type "meaningful_progress" t
                     "artifact_sha256" sha)
                :artifact-id final-artifact-id :artifact-version version)
               (%fpe-terminal-hook "after-attestations" operation-id)
               (setf result (%fpe-artifact-current final-artifact-id
                                                   :include-content nil))))))))
    (%fpe-stat "artifact-operation-completed")
    (%fpe-log-terminal-operation
     operation-id "completed" t :artifact-id final-artifact-id
     :artifact-version version :artifact-sha256 sha)
    (%fpe-emit-process-aggregate-state process-id)
    result))

(defun agent-validation-commit-operation (operation-id owner validation-result &key usage)
  (%fpe-require-write-mode)
  (%fpe-string owner "lease owner")
  (unless (and (hash-table-p validation-result)
               (eq (gethash "passed" validation-result) t)
               (stringp (gethash "artifact_sha256" validation-result)))
    (%fpe-reject "invalid-validation" "validation must pass and name the exact artifact hash"))
  (let ((process-id nil) (artifact-id nil) (artifact-version nil) (result nil))
    (%fpe-run-serializable
     (lambda ()
       (with-pg
         (%fpe-with-transaction (:serializable)
           (let* ((operation (%fpe-require-owned-running-current operation-id owner))
                  (process (%fpe-process-current (gethash "process_id" operation)
                                                 :for-update t))
                  (project (%fpe-project-current (gethash "process_id" operation)
                                                 :for-update t)))
             (unless (and (string= (gethash "operation_type" operation) "validate")
                          (string= (gethash "next_operation_type" project) "validate"))
               (%fpe-reject "not-validation-operation" "operation/project is not ready for validation"))
             (setf process-id (gethash "id" process)
                   artifact-id (gethash "input_artifact_id" operation)
                   artifact-version (gethash "input_artifact_version" operation))
             (let ((artifact (%fpe-artifact-current artifact-id :version artifact-version
                                                    :for-update t)))
               (unless (and artifact
                            (string= (gethash "artifact_type" artifact) "story-draft")
                            (= artifact-version (gethash "current_version" artifact))
                            (string= (gethash "sha256" artifact)
                                     (gethash "artifact_sha256" validation-result)))
                 (%fpe-reject "validation-artifact-mismatch"
                              "validation does not match the current draft version/hash")))
             (%fpe-require-budget-current process :new-usage usage)
             (%fpe-terminal-hook "before-validation-attestation" operation-id)
             (multiple-value-bind (model prompt completion cost) (%fpe-usage-values usage)
               (pomo:execute
                "UPDATE agent_process_operations SET status='completed',finished_at=now(),meaningful_progress=false,model_name=$3,prompt_tokens=$4,completion_tokens=$5,cost=$6,validation=$7::jsonb WHERE id=$1 AND lease_owner=$2"
                operation-id owner (%fpe-sql-null model) prompt completion cost
                (%fpe-json validation-result)))
             (%fpe-attest-current
              (%fpe-id "att") process-id operation-id "artifact-validated"
              "runtime-attested" "first-person-evidence" validation-result
              :artifact-id artifact-id :artifact-version artifact-version)
             (pomo:execute
              "UPDATE creative_projects SET next_operation_type='complete',updated_at=now() WHERE process_id=$1"
              process-id)
             (setf result (%fpe-operation-current operation-id)))))))
    (%fpe-stat "validation-operation-completed")
    (%fpe-log-terminal-operation
     operation-id "completed" nil :artifact-id artifact-id
     :artifact-version artifact-version)
    ;; Emit only after the serializable transaction committed. The lifecycle
    ;; observer receives the durable event wrapper and no artifact content.
    (%fpe-log
     "artifact-validated"
     (obj "operation_id" operation-id "process_id" process-id
          "artifact_id" artifact-id "artifact_version" artifact-version))
    (%fpe-emit-process-aggregate-state process-id)
    result))

(defun %fpe-required-operation-count-current (process-id type &key meaningful)
  (pomo:query
   (if meaningful
       "SELECT count(*) FROM agent_process_operations WHERE process_id=$1 AND operation_type=$2 AND status='completed' AND meaningful_progress=true"
       "SELECT count(*) FROM agent_process_operations WHERE process_id=$1 AND operation_type=$2 AND status='completed'")
   process-id type :single))

(defun agent-process-complete-from-validation (operation-id owner
                                               &key validation-operation-id)
  (%fpe-require-write-mode)
  (%fpe-string owner "lease owner")
  (let ((process-id nil) (artifact-id nil) (artifact-version nil)
        (sha nil) (result nil))
    (%fpe-run-serializable
     (lambda ()
       (with-pg
         (%fpe-with-transaction (:serializable)
           (let* ((operation (%fpe-require-owned-running-current operation-id owner))
                  (process (%fpe-process-current (gethash "process_id" operation)
                                                 :for-update t))
                  (project (%fpe-project-current (gethash "process_id" operation)
                                                 :for-update t)))
             (unless (and (string= (gethash "operation_type" operation) "complete")
                          (string= (gethash "next_operation_type" project) "complete"))
               (%fpe-reject "not-completion-operation" "operation/project is not ready to complete"))
             (setf process-id (gethash "id" process)
                   artifact-id (gethash "input_artifact_id" operation)
                   artifact-version (gethash "input_artifact_version" operation))
             (let ((artifact (%fpe-artifact-current artifact-id :version artifact-version
                                                    :for-update t)))
               (unless (and artifact
                            (string= (gethash "artifact_type" artifact) "story-draft")
                            (string= (gethash "status" artifact) "active")
                            (= artifact-version (gethash "current_version" artifact))
                            (>= artifact-version 2))
                 (%fpe-reject "completion-artifact" "current draft v2+ is required"))
               (setf sha (gethash "sha256" artifact)))
             (let ((validation
                     (pomo:query
                      "SELECT operation_id FROM agent_attestations WHERE process_id=$1 AND artifact_id=$2 AND artifact_version=$3 AND attestation_type='artifact-validated' AND ($4::text IS NULL OR operation_id=$4)"
                      process-id artifact-id artifact-version
                      (%fpe-sql-null validation-operation-id)
                      :single)))
               (unless validation
                 (%fpe-reject "missing-validation" "exact artifact validation attestation is required")))
             (dolist (required '("outline" "draft" "revise" "validate"))
               (unless (= 1 (%fpe-required-operation-count-current
                             process-id required
                             :meaningful (member required '("outline" "draft" "revise")
                                                 :test #'string=)))
                 (%fpe-reject "incomplete-sequence" "exactly one completed ~a is required"
                              required)))
             (unless (= 4 (pomo:query
                            "SELECT count(DISTINCT scheduler_cycle_id) FROM agent_process_operations WHERE process_id=$1 AND status='completed' AND operation_type IN ('outline','draft','revise','validate')"
                            process-id :single))
               (%fpe-reject "cycles-not-separated" "prior operations require four distinct cycles"))
             (%fpe-require-budget-current process)
             (%fpe-terminal-hook "before-completion" operation-id)
             (pomo:execute
              "UPDATE agent_process_operations SET status='completed',finished_at=now(),meaningful_progress=false,prompt_tokens=0,completion_tokens=0,cost=0,validation=jsonb_build_object('passed',true,'artifact_sha256',$3::text) WHERE id=$1 AND lease_owner=$2"
              operation-id owner sha)
             (pomo:execute
              "UPDATE agent_artifacts SET status='completed',completed_at=now() WHERE id=$1"
              artifact-id)
             (pomo:execute
              "UPDATE agent_processes SET state='completed',updated_at=now(),ended_at=now(),version=version+1 WHERE id=$1"
              process-id)
             (pomo:execute
              "UPDATE creative_projects SET next_operation_type=NULL,updated_at=now() WHERE process_id=$1"
              process-id)
             (%fpe-attest-current
              (%fpe-id "att") process-id operation-id "artifact-completed"
              "runtime-attested" "first-person-evidence"
              (obj "sha256" sha "final_version" artifact-version)
              :artifact-id artifact-id :artifact-version artifact-version)
             (%fpe-attest-current
              (%fpe-id "att") process-id operation-id "process-completed"
              "runtime-attested" "first-person-evidence"
              (obj "artifact_id" artifact-id "artifact_version" artifact-version
                   "artifact_sha256" sha))
             (setf result (%fpe-process-current process-id)))))))
    (%fpe-stat "process-completed")
    (%fpe-log-terminal-operation
     operation-id "completed" nil :artifact-id artifact-id
     :artifact-version artifact-version :artifact-sha256 sha)
    ;; Completion is likewise a post-commit, content-free lifecycle fact.
    (%fpe-log
     "artifact-completed"
     (obj "operation_id" operation-id "process_id" process-id
          "artifact_id" artifact-id "artifact_version" artifact-version
          "artifact_sha256" sha))
    (%fpe-emit-process-aggregate-state process-id)
    result))

(defun %fpe-attestation-row-object (row)
  (when row
    (destructuring-bind (id process operation type status issuer issued from until
                         subject facts-json source-json evidence-json artifact
                         version derivation supersedes superseded)
        row
      (obj "id" id "process_id" (or process :null)
           "operation_id" (or operation :null) "attestation_type" type
           "epistemic_status" status "issued_by" issuer "issued_at" issued
           "valid_from" from "valid_until" (or until :null) "subject" subject
           "facts" (%fpe-json-read facts-json (obj))
           "source_event_ids" (%fpe-json-read source-json (vector))
           "evidence_node_ids" (%fpe-json-read evidence-json (vector))
           "artifact_id" (or artifact :null) "artifact_version" (or version :null)
           "derivation_version" (or derivation :null)
           "supersedes_attestation_id" (or supersedes :null)
           "superseded_at" (or superseded :null)))))

(defun agent-attestation-query (&key process-id types valid-at (limit 100))
  (unless (and (integerp limit) (plusp limit) (<= limit 500))
    (%fpe-reject "invalid-limit" "limit must be an integer from 1 through 500"))
  (with-pg
    (mapcar
     #'%fpe-attestation-row-object
     (pomo:query
      "SELECT id,process_id,operation_id,attestation_type,epistemic_status,issued_by,issued_at::text,valid_from::text,valid_until::text,subject,facts::text,source_event_ids::text,evidence_node_ids::text,artifact_id,artifact_version,derivation_version,supersedes_attestation_id,superseded_at::text FROM agent_attestations WHERE ($1::text IS NULL OR process_id=$1) AND ($2::jsonb='[]'::jsonb OR $2::jsonb ? attestation_type) AND ($3::timestamptz IS NULL OR (valid_from<=$3 AND (valid_until IS NULL OR valid_until>$3))) ORDER BY issued_at DESC LIMIT $4"
      (%fpe-sql-null process-id) (%fpe-json (%fpe-vector types))
      (%fpe-sql-null valid-at) limit))))

(defun first-person-evidence-report ()
  (with-pg
    (let ((counts (obj)) (stats (obj)))
      (dolist (row
                (pomo:query
                 "SELECT 'proposal/'||status,count(*) FROM grounded_project_proposals GROUP BY status UNION ALL SELECT 'process/'||state,count(*) FROM agent_processes GROUP BY state UNION ALL SELECT 'operation/'||status,count(*) FROM agent_process_operations GROUP BY status UNION ALL SELECT 'attestation/'||attestation_type,count(*) FROM agent_attestations GROUP BY attestation_type ORDER BY 1"))
        (setf (gethash (first row) counts) (second row)))
      (bt:with-lock-held (*first-person-evidence-stats-lock*)
        (maphash (lambda (key value) (setf (gethash key stats) value))
                 *first-person-evidence-stats*))
      (obj "schema_version" *first-person-evidence-schema-version*
           "mode" (if (boundp '*grounded-agency-mode*)
                      (string-downcase (symbol-name *grounded-agency-mode*))
                      "legacy")
           "write_ready" (if (%fpe-mode-ready-p) t nil)
           "counts" counts "stats" stats))))
