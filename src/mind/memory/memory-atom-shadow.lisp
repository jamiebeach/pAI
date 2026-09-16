;;;; memory-atom-shadow.lisp -- N1 asynchronous, non-admitting decomposition.
;;;;
;;;; Raw turn roots are already durable before this module sees them. This
;;;; worker owns its own queue, lease, provider budget and anomaly latch. Its
;;;; output is a private candidate record only: it never writes MEMORY_NODES,
;;;; changes retrieval, composes a public reply, ticks, transports or delivers.

(in-package :agent)

(ql:quickload '(:ironclad :babel) :silent t)

(export '(ensure-memory-atom-shadow-schema memory-atom-shadow-schema-report
          memory-atom-shadow-initialize-rollout
          memory-atom-shadow-enqueue-turn memory-atom-shadow-worker-step
          memory-atom-shadow-worker-start memory-atom-shadow-worker-stop
          memory-atom-shadow-worker-alive-p memory-atom-shadow-report
          memory-atom-shadow-current-private-review
          memory-atom-shadow-reconcile))

(defparameter *memory-atom-shadow-model* "openai/gpt-oss-120b")
(defparameter *memory-atom-shadow-temperature* 0.0d0)
(defparameter *memory-atom-shadow-max-tokens* 1600)
(defparameter *memory-atom-shadow-timeout-seconds* 15)
(defparameter *memory-atom-shadow-queue-cap* 1000)
(defparameter *memory-atom-shadow-max-evidence* 12)
(defvar *memory-atom-shadow-model-fn* nil)
(defvar *memory-atom-shadow-rollout-id* nil)
(defvar *memory-atom-shadow-worker* nil)
(defvar *memory-atom-shadow-stop-requested* nil)
(defvar *memory-atom-shadow-worker-lock* (bt:make-lock "memory-atom-shadow-worker"))
(defvar *memory-atom-shadow-stats* (make-hash-table :test #'equal))

(declaim (ftype function memory-atom-shadow-schema-report
                memory-atom-shadow-report))

(defun %memory-atom-shadow-mode ()
  (if (boundp '*memory-atom-decomposition-mode*)
      *memory-atom-decomposition-mode* :off))

(defun %memory-atom-shadow-stat (key &optional (amount 1))
  (bt:with-lock-held (*memory-atom-shadow-worker-lock*)
    (incf (gethash key *memory-atom-shadow-stats* 0) amount)))

(defun %memory-atom-shadow-json (value)
  (shasht:write-json value nil))

(defun %memory-atom-shadow-list (value)
  (cond ((null value) nil)
        ((vectorp value) (coerce value 'list))
        ((listp value) value)
        (t (error "Memory atom array field is not an array."))))

(defun %memory-atom-shadow-nonempty-string-p (value &optional maximum)
  (and (stringp value)
       (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                   value)))
       (or (null maximum) (<= (length value) maximum))))

(defun %memory-atom-shadow-safe-id-p (value &key (colon t))
  (and (%memory-atom-shadow-nonempty-string-p value 160)
       (every (lambda (character)
                (or (alphanumericp character)
                    (member character
                            (if colon '(#\. #\_ #\- #\:)
                                '(#\. #\_ #\-))
                            :test #'char=)))
              value)))

(defun %memory-atom-shadow-canonical-field (value)
  (let ((text (if (eq value :null) "<null>" (format nil "~a" value))))
    (format nil "~d:~a" (length text) text)))

(defun %memory-atom-shadow-sha256 (&rest fields)
  (let* ((canonical
           (format nil "~{~a~^|~}"
                   (mapcar #'%memory-atom-shadow-canonical-field fields)))
         (octets (babel:string-to-octets canonical :encoding :utf-8)))
    (string-downcase
     (ironclad:byte-array-to-hex-string
      (ironclad:digest-sequence :sha256 octets)))))

(defun %memory-atom-shadow-manifest-map (manifest)
  (let ((table (make-hash-table :test #'equal)))
    (dolist (row (%memory-atom-shadow-list (gethash "evidence" manifest)))
      (setf (gethash (gethash "id" row) table) row))
    table))

(defun %memory-atom-shadow-row-primary-key (table row)
  (cond
    ((string= table "memory_atom_rollouts")
     (obj "rollout_id" (gethash "rollout_id" row)))
    ((string= table "memory_atom_jobs")
     (obj "id" (gethash "id" row)))
    ((string= table "memory_atom_candidates")
     (obj "candidate_id" (gethash "candidate_id" row)))
    ((string= table "memory_atom_candidate_roots")
     (obj "candidate_id" (gethash "candidate_id" row)
          "evidence_id" (gethash "evidence_id" row)))
    (t (error "Unsupported memory-atom event table ~a" table))))

(defun %memory-atom-shadow-emit-row (table row-json)
  "Emit a captured post-commit row image. Event failure never changes SQL."
  (when (and row-json (fboundp 'log-postgres-row-state))
    (ignore-errors
      (let ((row (if (stringp row-json)
                     (shasht:read-json row-json)
                     row-json)))
        (funcall 'log-postgres-row-state
                 table "upsert"
                 (%memory-atom-shadow-row-primary-key table row)
                 row (and (stringp row-json) row-json))))))

(defun %memory-atom-shadow-emit-rows (rows)
  (dolist (entry rows)
    (%memory-atom-shadow-emit-row (car entry) (cdr entry))))

(defun ensure-memory-atom-shadow-schema ()
  "Explicit idempotent migration. Never called by loading this source file."
  (ensure-memory-architecture-schema)
  (with-pg
    (pomo:with-transaction ()
      (pomo:execute
       "CREATE TABLE IF NOT EXISTS memory_atom_rollouts (rollout_id text PRIMARY KEY, model text NOT NULL, max_requests integer NOT NULL CHECK(max_requests>0), max_cost_credits double precision NOT NULL CHECK(max_cost_credits>0), stop_on_first_anomaly boolean NOT NULL DEFAULT true, request_count integer NOT NULL DEFAULT 0 CHECK(request_count>=0 AND request_count<=max_requests), cost_credits double precision NOT NULL DEFAULT 0 CHECK(cost_credits>=0 AND cost_credits<=max_cost_credits), anomaly_latched boolean NOT NULL DEFAULT false, created_at timestamptz NOT NULL DEFAULT now(), updated_at timestamptz NOT NULL DEFAULT now())")
      (pomo:execute
       "CREATE TABLE IF NOT EXISTS memory_atom_jobs (id bigserial PRIMARY KEY, agent_id text NOT NULL DEFAULT 'default', turn_id text NOT NULL, captured_at timestamptz NOT NULL, evidence_ids jsonb NOT NULL CHECK(jsonb_typeof(evidence_ids)='array' AND jsonb_array_length(evidence_ids)>0), status text NOT NULL DEFAULT 'pending' CHECK(status IN ('pending','leased','completed','no-atoms','blocked','rejected','anomaly')), attempt_count integer NOT NULL DEFAULT 0 CHECK(attempt_count>=0), lease_owner text, lease_until timestamptz, rollout_id text REFERENCES memory_atom_rollouts(rollout_id) ON DELETE RESTRICT, request_sha256 text, metrics jsonb NOT NULL DEFAULT '{}'::jsonb, last_error_code text, created_at timestamptz NOT NULL DEFAULT now(), updated_at timestamptz NOT NULL DEFAULT now(), UNIQUE(agent_id,turn_id))")
      (pomo:execute
       "CREATE TABLE IF NOT EXISTS memory_atom_candidates (candidate_id text PRIMARY KEY, job_id bigint NOT NULL REFERENCES memory_atom_jobs(id) ON DELETE RESTRICT, claim_key text NOT NULL, idempotency_key text NOT NULL UNIQUE, memory_form text NOT NULL, subject text NOT NULL, predicate text NOT NULL, atom jsonb NOT NULL, evidence_ids jsonb NOT NULL, created_at timestamptz NOT NULL DEFAULT now())")
      (pomo:execute
       "CREATE TABLE IF NOT EXISTS memory_atom_candidate_roots (candidate_id text NOT NULL REFERENCES memory_atom_candidates(candidate_id) ON DELETE RESTRICT, evidence_id text NOT NULL REFERENCES memory_nodes(id) ON DELETE RESTRICT, ordinality integer NOT NULL CHECK(ordinality>0), evidence_sha256 text NOT NULL CHECK(length(evidence_sha256)=64), PRIMARY KEY(candidate_id,evidence_id), UNIQUE(candidate_id,ordinality))")
      (pomo:execute "CREATE INDEX IF NOT EXISTS memory_atom_jobs_status_idx ON memory_atom_jobs(status,created_at,id)")
      (pomo:execute "CREATE INDEX IF NOT EXISTS memory_atom_candidates_job_idx ON memory_atom_candidates(job_id,created_at,candidate_id)")))
  (memory-atom-shadow-schema-report))

(defun memory-atom-shadow-schema-report ()
  (with-pg
    (let* ((tables (pomo:query "SELECT tablename FROM pg_tables WHERE schemaname='public' AND tablename IN ('memory_atom_rollouts','memory_atom_jobs','memory_atom_candidates','memory_atom_candidate_roots') ORDER BY tablename" :column))
           (ready (= 4 (length tables))))
      (obj "schema_version" 1 "ready" (if ready t nil)
           "tables" (coerce tables 'vector)))))

(defun memory-atom-shadow-initialize-rollout
    (rollout-id &key (model *memory-atom-shadow-model*) max-requests
                     max-cost-credits (stop-on-first-anomaly t))
  "Create one sealed budget. Only legal while decomposition mode is OFF."
  (unless (eq (%memory-atom-shadow-mode) :off)
    (error "A memory-atom rollout can only be initialized while mode is off."))
  (unless (and (%memory-atom-shadow-safe-id-p rollout-id)
               (integerp max-requests)
               (plusp max-requests) (numberp max-cost-credits)
               (> max-cost-credits 0))
    (error "Invalid memory-atom rollout parameters."))
  (let ((row
          (with-pg
            (pomo:query
             "INSERT INTO memory_atom_rollouts(rollout_id,model,max_requests,max_cost_credits,stop_on_first_anomaly) VALUES($1,$2,$3,$4,$5) RETURNING row_to_json(memory_atom_rollouts)::text"
             rollout-id model max-requests max-cost-credits
             (if stop-on-first-anomaly t nil) :single))))
    (%memory-atom-shadow-emit-row "memory_atom_rollouts" row))
  (setf *memory-atom-shadow-rollout-id* rollout-id
        *memory-atom-shadow-model* model)
  (memory-atom-shadow-report))

(defun memory-atom-shadow-enqueue-turn (context episode-id node-ids)
  (declare (ignore episode-id))
  (if (not (eq (%memory-atom-shadow-mode) :shadow))
      :disabled
      (let ((turn-id (and (hash-table-p context) (gethash "turn_id" context))))
        (unless (and (%memory-atom-shadow-safe-id-p turn-id) node-ids
                     (<= (length node-ids)
                         *memory-atom-shadow-max-evidence*))
          (error "Completed turn does not satisfy the atom queue contract."))
        (let ((inserted
                (with-pg
                  (pomo:with-transaction ()
                    ;; Serialize only the bounded depth check, not model work.
                    (pomo:query "SELECT pg_advisory_xact_lock(7211041)" :single)
                    (let ((depth (pomo:query
                                  "SELECT count(*) FROM memory_atom_jobs WHERE status IN ('pending','leased')"
                                  :single)))
                      (when (>= depth *memory-atom-shadow-queue-cap*)
                        (error "Memory-atom shadow queue is full.")))
                    (pomo:query
                     "INSERT INTO memory_atom_jobs(agent_id,turn_id,captured_at,evidence_ids,status) VALUES('pai',$1,now(),$2::jsonb,'pending') ON CONFLICT(agent_id,turn_id) DO NOTHING RETURNING row_to_json(memory_atom_jobs)::text"
                     turn-id (%memory-atom-shadow-json (coerce node-ids 'vector))
                     :single)))))
          (when inserted
            (%memory-atom-shadow-stat "enqueued")
            (%memory-atom-shadow-emit-row "memory_atom_jobs" inserted))
          (if inserted :enqueued :duplicate)))))

(defun %memory-atom-shadow-evidence (job-id)
  (with-pg
    (let ((rows
            (pomo:query
             "SELECT n.id,n.epistemic_metadata->>'role',n.epistemic_metadata->>'sequence',to_char(n.created_at AT TIME ZONE 'UTC','YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"'),n.content FROM memory_atom_jobs j CROSS JOIN LATERAL jsonb_array_elements_text(j.evidence_ids) WITH ORDINALITY e(id,ord) JOIN memory_nodes n ON n.id=e.id AND n.agent_id=j.agent_id WHERE j.id=$1 ORDER BY e.ord"
             job-id)))
      (mapcar (lambda (row)
                (obj "id" (first row) "role" (second row)
                     "sequence" (parse-integer (third row))
                     "observed_at" (fourth row) "content" (fifth row)))
              rows))))

(defun %memory-atom-shadow-http-call (messages model temperature max-tokens)
  (unless (and (boundp '*api-key*) (stringp *api-key*)
               (plusp (length *api-key*)))
    (error "OpenRouter API key is unavailable."))
  (shasht:read-json
   (dex:post *endpoint*
             :headers `(("Authorization" . ,(format nil "Bearer ~a" *api-key*))
                        ("Content-Type" . "application/json")
                        ("X-OpenRouter-Title" . "the agent Memory Atom Shadow"))
             :connect-timeout (min *http-connect-timeout*
                                   *memory-atom-shadow-timeout-seconds*)
             :read-timeout *memory-atom-shadow-timeout-seconds*
             :content (shasht:write-json
                       (obj "model" model "messages" (coerce messages 'vector)
                            "temperature" temperature "max_tokens" max-tokens
                            "stream" nil
                            "response_format" (obj "type" "json_object")) nil))))

(defun %memory-atom-shadow-invoke (messages model)
  (if *memory-atom-shadow-model-fn*
      (funcall *memory-atom-shadow-model-fn* messages model
               *memory-atom-shadow-temperature* *memory-atom-shadow-max-tokens*)
      (%memory-atom-shadow-http-call messages model
                                    *memory-atom-shadow-temperature*
                                    *memory-atom-shadow-max-tokens*)))

(defun %memory-atom-shadow-reserve (rollout-id)
  (let ((result
          (with-pg
            (pomo:query
             "UPDATE memory_atom_rollouts SET request_count=request_count+1,updated_at=now() WHERE rollout_id=$1 AND anomaly_latched=false AND request_count<max_requests AND cost_credits<max_cost_credits RETURNING model,row_to_json(memory_atom_rollouts)::text"
             rollout-id :row))))
    (when result
      (%memory-atom-shadow-emit-row "memory_atom_rollouts" (second result))
      (first result))))

(defun %memory-atom-shadow-lease ()
  (let ((result
          (with-pg
            (pomo:with-transaction ()
              (pomo:query
               "WITH chosen AS (SELECT id FROM memory_atom_jobs WHERE status='pending' ORDER BY created_at,id FOR UPDATE SKIP LOCKED LIMIT 1) UPDATE memory_atom_jobs j SET status='leased',attempt_count=attempt_count+1,lease_owner=$1,lease_until=now()+interval '2 minutes',rollout_id=$2,updated_at=now() FROM chosen WHERE j.id=chosen.id RETURNING j.id,j.turn_id,to_char(j.captured_at AT TIME ZONE 'UTC','YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"'),jsonb_array_length(j.evidence_ids),row_to_json(j)::text"
               (format nil "worker-~x" (random #x1000000))
               *memory-atom-shadow-rollout-id* :row)))))
    (when result
      (%memory-atom-shadow-emit-row "memory_atom_jobs" (fifth result))
      (subseq result 0 4))))

(defun %memory-atom-shadow-response (response elapsed-ms model)
  (let* ((content (and (hash-table-p response)
                       (ignore-errors (ref response "choices" 0 "message" "content"))))
         (usage (and (hash-table-p response) (gethash "usage" response)))
         (cost (and usage (gethash "cost" usage))))
    (unless (and (stringp content) (plusp (length content)))
      (error "Memory-atom response has no usable content."))
    (unless (and (numberp cost) (>= cost 0))
      (error "Memory-atom response omitted usable cost accounting."))
    (values (shasht:read-json content)
            (obj "latency_ms" elapsed-ms "model" model
                 "prompt_tokens" (or (gethash "prompt_tokens" usage) :null)
                 "completion_tokens" (or (gethash "completion_tokens" usage) :null)
                 "total_tokens" (or (gethash "total_tokens" usage) :null)
                 "cost_credits" cost))))

(defun %memory-atom-shadow-latch (job-id stage)
  (let ((rows nil))
    (with-pg
      (pomo:with-transaction ()
        (when *memory-atom-shadow-rollout-id*
          (let ((row
                  (pomo:query
                   "UPDATE memory_atom_rollouts SET anomaly_latched=true,updated_at=now() WHERE rollout_id=$1 AND stop_on_first_anomaly=true RETURNING row_to_json(memory_atom_rollouts)::text"
                   *memory-atom-shadow-rollout-id* :single)))
            (when row (push (cons "memory_atom_rollouts" row) rows))))
        (let ((row
                (pomo:query
                 "UPDATE memory_atom_jobs SET status='anomaly',last_error_code=$2,lease_owner=NULL,lease_until=NULL,updated_at=now() WHERE id=$1 RETURNING row_to_json(memory_atom_jobs)::text"
                 job-id stage :single)))
          (when row (push (cons "memory_atom_jobs" row) rows)))))
    (%memory-atom-shadow-emit-rows (nreverse rows))))

(defun %memory-atom-shadow-block-job (job-id)
  (let ((row
          (with-pg
            (pomo:query
             "UPDATE memory_atom_jobs SET status='blocked',last_error_code='budget-unavailable',lease_owner=NULL,lease_until=NULL,updated_at=now() WHERE id=$1 RETURNING row_to_json(memory_atom_jobs)::text"
             job-id :single))))
    (%memory-atom-shadow-emit-row "memory_atom_jobs" row)
    row))

(defun memory-atom-shadow-worker-step ()
  "Process at most one job. No automatic provider retry is possible."
  (unless (eq (%memory-atom-shadow-mode) :shadow) (return-from memory-atom-shadow-worker-step nil))
  (unless *memory-atom-shadow-rollout-id* (return-from memory-atom-shadow-worker-step nil))
  (let ((job (%memory-atom-shadow-lease)))
    (unless job (return-from memory-atom-shadow-worker-step nil))
    (let ((job-id (first job)) (turn-id (second job)) (captured-at (third job))
          (expected-evidence (fourth job)) (stage "evidence"))
      (handler-case
          (let* ((evidence (%memory-atom-shadow-evidence job-id))
                 (_ (unless (= expected-evidence (length evidence))
                      (error "One or more immutable evidence roots are unavailable.")))
                 (manifest (memory-atom-build-manifest turn-id captured-at evidence))
                 (messages (memory-atom-build-request manifest))
                 (request-sha
                   (%memory-atom-shadow-sha256
                    (%memory-atom-shadow-json messages)))
                 (model (or (%memory-atom-shadow-reserve *memory-atom-shadow-rollout-id*)
                            (progn
                              (%memory-atom-shadow-block-job job-id)
                              (return-from memory-atom-shadow-worker-step t))))
                 (started (get-internal-real-time)))
            (declare (ignore _))
            (setf stage "provider")
            (let ((response (%memory-atom-shadow-invoke messages model)))
              (multiple-value-bind (parsed metrics)
                  (%memory-atom-shadow-response
                   response
                   (round (* 1000 (/ (- (get-internal-real-time) started)
                                      internal-time-units-per-second)))
                   model)
                (setf stage "validation")
                (let* ((validated (memory-atom-validate-response parsed manifest))
                       (atoms (%memory-atom-shadow-list
                               (gethash "atoms" validated)))
                       (evidence-map
                         (%memory-atom-shadow-manifest-map manifest))
                       (cost (gethash "cost_credits" metrics))
                       (event-rows nil))
                  (with-pg
                    (pomo:with-transaction ()
                      (let ((row
                              (pomo:query
                               "UPDATE memory_atom_rollouts SET cost_credits=cost_credits+$2,updated_at=now() WHERE rollout_id=$1 AND cost_credits+$2<=max_cost_credits RETURNING row_to_json(memory_atom_rollouts)::text"
                               *memory-atom-shadow-rollout-id* cost :single)))
                        (unless row
                          (error "Memory-atom rollout cost ceiling would be exceeded."))
                        (push (cons "memory_atom_rollouts" row) event-rows))
                      (dolist (atom atoms)
                        (let ((inserted
                                (pomo:query
                                 "INSERT INTO memory_atom_candidates(candidate_id,job_id,claim_key,idempotency_key,memory_form,subject,predicate,atom,evidence_ids) VALUES($1,$2,$3,$4,$5,$6,$7,$8::jsonb,$9::jsonb) ON CONFLICT(idempotency_key) DO NOTHING RETURNING candidate_id,row_to_json(memory_atom_candidates)::text"
                                 (gethash "candidate_id" atom) job-id
                                 (gethash "claim_key" atom)
                                 (gethash "idempotency_key" atom)
                                 (gethash "memory_form" atom)
                                 (gethash "subject" atom)
                                 (gethash "predicate" atom)
                                 (%memory-atom-shadow-json atom)
                                 (%memory-atom-shadow-json
                                  (gethash "evidence_ids" atom)) :row)))
                          (when inserted
                            (push (cons "memory_atom_candidates" (second inserted))
                                  event-rows)
                            (loop for root-id in (%memory-atom-shadow-list
                                                  (gethash "evidence_ids" atom))
                                  for ordinality from 1
                                  for root = (gethash root-id evidence-map)
                                  for root-row =
                                    (pomo:query
                                     "INSERT INTO memory_atom_candidate_roots(candidate_id,evidence_id,ordinality,evidence_sha256) VALUES($1,$2,$3,$4) RETURNING row_to_json(memory_atom_candidate_roots)::text"
                                     (first inserted) root-id ordinality
                                     (%memory-atom-shadow-sha256
                                      (gethash "content" root)) :single)
                                  do (push (cons "memory_atom_candidate_roots"
                                                 root-row)
                                           event-rows)))))
                      (let ((row
                              (pomo:query
                               "UPDATE memory_atom_jobs SET status=$2,request_sha256=$3,metrics=$4::jsonb,lease_owner=NULL,lease_until=NULL,updated_at=now() WHERE id=$1 RETURNING row_to_json(memory_atom_jobs)::text"
                               job-id (if atoms "completed" "no-atoms") request-sha
                               (%memory-atom-shadow-json metrics) :single)))
                        (push (cons "memory_atom_jobs" row) event-rows))))
                  (%memory-atom-shadow-emit-rows (nreverse event-rows))
                  (%memory-atom-shadow-stat (if atoms "completed" "no-atoms"))))))
        (error (condition)
          (declare (ignore condition))
          (%memory-atom-shadow-stat "anomaly")
          (%memory-atom-shadow-latch job-id stage)))
      t)))

(defun memory-atom-shadow-worker-alive-p ()
  (not (null (and *memory-atom-shadow-worker*
                  (bt:thread-alive-p *memory-atom-shadow-worker*)))))

(defun memory-atom-shadow-reconcile ()
  "Fail closed after an expired lease and recover missed post-rollout hooks."
  (let ((event-rows nil) (expired nil) (recovered nil))
    (with-pg
      (pomo:with-transaction ()
        (setf expired
              (pomo:query
               "UPDATE memory_atom_jobs SET status='anomaly',last_error_code='expired-lease',lease_owner=NULL,lease_until=NULL,updated_at=now() WHERE status='leased' AND lease_until<now() RETURNING rollout_id,row_to_json(memory_atom_jobs)::text"))
        (dolist (entry expired)
          (push (cons "memory_atom_jobs" (second entry)) event-rows))
        (dolist (rollout-id
                  (remove-duplicates (remove nil (mapcar #'first expired))
                                     :test #'string=))
          (let ((row
                  (pomo:query
                   "UPDATE memory_atom_rollouts SET anomaly_latched=true,updated_at=now() WHERE rollout_id=$1 AND stop_on_first_anomaly=true RETURNING row_to_json(memory_atom_rollouts)::text"
                   rollout-id :single)))
            (when row (push (cons "memory_atom_rollouts" row) event-rows))))
        (setf recovered
              (if (null *memory-atom-shadow-rollout-id*) nil
                  (pomo:query
                   "WITH capacity AS (SELECT greatest(0,$2::integer - count(*)::integer) AS slots FROM memory_atom_jobs WHERE status IN ('pending','leased')), missing AS (SELECT n.agent_id,n.epistemic_metadata->>'turn_id' AS turn_id,max(n.created_at) AS captured_at,jsonb_agg(n.id ORDER BY (n.epistemic_metadata->>'sequence')::integer) AS evidence_ids FROM memory_nodes n JOIN memory_atom_rollouts r ON r.rollout_id=$1 WHERE n.agent_id='default' AND n.created_at>=r.created_at AND n.epistemic_metadata->>'role' IN ('user','assistant','tool') AND n.epistemic_metadata->>'sequence' ~ '^[0-9]+$' AND NULLIF(n.epistemic_metadata->>'turn_id','') IS NOT NULL AND NOT EXISTS (SELECT 1 FROM memory_atom_jobs j WHERE j.agent_id=n.agent_id AND j.turn_id=n.epistemic_metadata->>'turn_id') GROUP BY n.agent_id,n.epistemic_metadata->>'turn_id' HAVING count(*) BETWEEN 1 AND $3 ORDER BY max(n.created_at) LIMIT (SELECT slots FROM capacity)) INSERT INTO memory_atom_jobs(agent_id,turn_id,captured_at,evidence_ids,status) SELECT agent_id,turn_id,captured_at,evidence_ids,'pending' FROM missing ON CONFLICT(agent_id,turn_id) DO NOTHING RETURNING id,row_to_json(memory_atom_jobs)::text"
                   *memory-atom-shadow-rollout-id* *memory-atom-shadow-queue-cap*
                   *memory-atom-shadow-max-evidence*)))
        (dolist (entry recovered)
          (push (cons "memory_atom_jobs" (second entry)) event-rows))))
    (%memory-atom-shadow-emit-rows (nreverse event-rows))
    (when recovered (%memory-atom-shadow-stat "reconciled" (length recovered)))
    (+ (length expired) (length recovered))))

(defun memory-atom-shadow-worker-start ()
  (unless (eq (%memory-atom-shadow-mode) :shadow)
    (error "Memory-atom worker requires shadow mode."))
  (unless (gethash "ready" (memory-atom-shadow-schema-report))
    (error "Memory-atom shadow schema is not ready."))
  (unless *memory-atom-shadow-rollout-id*
    (let ((active
            (with-pg
              (pomo:query
               "SELECT rollout_id,model FROM memory_atom_rollouts WHERE anomaly_latched=false AND request_count<max_requests AND cost_credits<max_cost_credits ORDER BY created_at DESC LIMIT 1"
               :row))))
      (when active
        (setf *memory-atom-shadow-rollout-id* (first active)
              *memory-atom-shadow-model* (second active)))))
  (unless *memory-atom-shadow-rollout-id*
    (error "Memory-atom shadow rollout is not initialized."))
  (memory-atom-shadow-reconcile)
  (unless (memory-atom-shadow-worker-alive-p)
    (setf *memory-atom-shadow-stop-requested* nil
          *memory-atom-shadow-worker*
          (bt:make-thread
           (lambda ()
             (loop until *memory-atom-shadow-stop-requested*
                   do (unless (memory-atom-shadow-worker-step) (sleep 0.25d0))))
           :name "memory-atom-shadow-worker")))
  t)

(defun memory-atom-shadow-worker-stop (&optional (timeout 3))
  (setf *memory-atom-shadow-stop-requested* t)
  (loop with deadline = (+ (get-internal-real-time)
                           (* timeout internal-time-units-per-second))
        while (and (memory-atom-shadow-worker-alive-p)
                   (< (get-internal-real-time) deadline))
        do (sleep 0.05d0))
  (not (memory-atom-shadow-worker-alive-p)))

(defun memory-atom-shadow-report ()
  (let ((counts (obj)) (durable :null))
    (bt:with-lock-held (*memory-atom-shadow-worker-lock*)
      (maphash (lambda (key value) (setf (gethash key counts) value))
               *memory-atom-shadow-stats*))
    (setf durable
          (handler-case
              (with-pg
                (let ((jobs (pomo:query
                             "SELECT status,count(*) FROM memory_atom_jobs GROUP BY status ORDER BY status"))
                      (candidate-count
                        (pomo:query "SELECT count(*) FROM memory_atom_candidates" :single))
                      (rollout
                        (and *memory-atom-shadow-rollout-id*
                             (pomo:query
                              "SELECT max_requests,request_count,max_cost_credits,cost_credits,anomaly_latched FROM memory_atom_rollouts WHERE rollout_id=$1"
                              *memory-atom-shadow-rollout-id* :row))))
                  (obj "status" "available"
                       "job_counts"
                       (coerce (mapcar (lambda (row)
                                        (obj "status" (first row)
                                             "count" (second row))) jobs) 'vector)
                       "candidate_count" candidate-count
                       "rollout"
                       (if rollout
                           (obj "max_requests" (first rollout)
                                "request_count" (second rollout)
                                "max_cost_credits" (third rollout)
                                "cost_credits" (fourth rollout)
                                "anomaly_latched" (if (fifth rollout) t nil))
                           :null))))
            (error () (obj "status" "unavailable"))))
    (obj "schema_version" 1
         "mode" (string-downcase (symbol-name (%memory-atom-shadow-mode)))
         "model" *memory-atom-shadow-model*
         "rollout_id" (or *memory-atom-shadow-rollout-id* :null)
         "worker_alive" (memory-atom-shadow-worker-alive-p)
         "admission_available" nil "retrieval_effect" nil
         "public_response_effect" nil "delivery_authority" nil
         "counts" counts "durable" durable)))

(defun memory-atom-shadow-current-private-review ()
  "Authenticated-admin consumer. Deliberately exposes roots only here."
  (handler-case
      (with-pg
        (let ((row (pomo:query
                "SELECT c.candidate_id,c.atom::text,j.turn_id,c.evidence_ids::text FROM memory_atom_candidates c JOIN memory_atom_jobs j ON j.id=c.job_id ORDER BY c.created_at DESC,c.candidate_id DESC LIMIT 1"
                :row)))
          (if (null row)
              (obj "schema_version" 1 "status" "unavailable")
              (let* ((ids (shasht:read-json (fourth row)))
                     (roots (mapcar
                             (lambda (id)
                               (let ((root (pomo:query
                                            "SELECT n.id,n.epistemic_metadata->>'role',n.content,r.evidence_sha256 FROM memory_atom_candidate_roots r JOIN memory_nodes n ON n.id=r.evidence_id AND n.agent_id='default' WHERE r.candidate_id=$1 AND r.evidence_id=$2"
                                            (first row) id :row)))
                                 (let ((current-sha
                                         (%memory-atom-shadow-sha256
                                          (third root))))
                                   (obj "id" (first root) "role" (second root)
                                        "content" (third root)
                                        "captured_sha256" (fourth root)
                                        "current_sha256" current-sha
                                        "content_matches_capture"
                                        (string= current-sha (fourth root))))))
                             (%memory-atom-shadow-list ids))))
                (obj "schema_version" 1 "status" "available"
                     "candidate_id" (first row) "turn_id" (third row)
                     "candidate" (shasht:read-json (second row))
                     "raw_roots" (coerce roots 'vector))))))
    (error () (obj "schema_version" 1 "status" "unavailable"))))

;; File load installs only an off-gated local callback. It performs no SQL.
(when (fboundp 'turn-capture-register-complete-hook)
  (turn-capture-register-complete-hook
   'memory-atom-shadow #'memory-atom-shadow-enqueue-turn))
