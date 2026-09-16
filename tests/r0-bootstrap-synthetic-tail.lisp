(defpackage :agent (:use :cl))
(in-package :agent)

(ql:quickload '(:bordeaux-threads :shasht :ironclad :uiop :postmodern) :silent t)

(defun r0-bootstrap-tail-object (&rest pairs)
  (loop with result = (make-hash-table :test #'equal)
        for (key value) on pairs by #'cddr
        do (setf (gethash key result) value)
        finally (return result)))

(setf (fdefinition 'obj) #'r0-bootstrap-tail-object
      (fdefinition 'auto-turn) (lambda (prompt) prompt)
      (fdefinition 'execute) (lambda (&rest values)
                               (declare (ignore values)) nil)
      (fdefinition 'propose-loop) (lambda (&rest values)
                                    (declare (ignore values)) nil))
(defparameter *tools* (vector))

(load (test-source "event-log.lisp"))
(load (test-source "projection-rebuild.lisp"))

(defparameter *r0-bootstrap-pg*
  (list (or (uiop:getenv "PAI_PG_DATABASE") "pai_memory")
        (or (uiop:getenv "PAI_PG_USER") "pai")
        (or (uiop:getenv "PAI_PG_PASSWORD") "r0_candidate_only")
        (or (uiop:getenv "PAI_PG_HOST")
            "pai-r0-bootstrap-pg-20260809-214607")
        :port (parse-integer (or (uiop:getenv "PAI_PG_PORT") "5432"))))

(defun r0-bootstrap-tail-json (value)
  (let ((*print-pretty* nil)) (shasht:write-json value nil)))

(let* ((boundary
         (parse-integer
          (or (uiop:getenv "R0_BOOTSTRAP_BOUNDARY")
              (error "R0_BOOTSTRAP_BOUNDARY is required"))))
       (candidate-root #P"/agent/state/")
       (file-events 0)
       (row-events 0)
       (rows nil))
  (unless (= *event-next-id* boundary)
    (error "Candidate ledger moved: expected last id ~d, found ~d"
           boundary *event-next-id*))

  ;; A bounded no-op replacement tail proves all file folds without changing
  ;; the quiescent bytes captured at the boundary.
  (dolist (spec (projection-rebuild-file-specs))
    (let* ((name (getf spec :name))
           (file (getf spec :file))
           (path (merge-pathnames file candidate-root))
           (content (uiop:read-file-string path)))
      (if (string= name "conversation-history")
          (log-conversation-history-transform path content)
          (log-projection-state name path content))
      (incf file-events)))

  ;; Insert one valid linked row in each previously empty atom table in a
  ;; single clone transaction, then emit exact post-commit row images.
  (pomo:with-connection *r0-bootstrap-pg*
    (let* ((root-id
             (pomo:query "SELECT id FROM memory_nodes ORDER BY id LIMIT 1"
                         :single))
           (evidence-json
             (r0-bootstrap-tail-json (vector root-id)))
           (atom-json
             (r0-bootstrap-tail-json
              (obj "subject" "synthetic-bootstrap"
                   "predicate" "qualifies"
                   "object" "r0-tail"))))
      (pomo:with-transaction ()
        (let* ((rollout-json
                 (pomo:query
                  "INSERT INTO memory_atom_rollouts(rollout_id,model,max_requests,max_cost_credits,stop_on_first_anomaly) VALUES('r0-bootstrap-rollout-20260809','synthetic/no-provider',1,0.001,true) RETURNING row_to_json(memory_atom_rollouts)::text"
                  :single))
               (job-json
                 (pomo:query
                  "INSERT INTO memory_atom_jobs(agent_id,turn_id,captured_at,evidence_ids,status,rollout_id) VALUES('pai','r0-bootstrap-turn-20260809',now(),$1::jsonb,'completed','r0-bootstrap-rollout-20260809') RETURNING row_to_json(memory_atom_jobs)::text"
                  evidence-json :single))
               (job-row (shasht:read-json job-json))
               (job-id (gethash "id" job-row))
               (candidate-json
                 (pomo:query
                  "INSERT INTO memory_atom_candidates(candidate_id,job_id,claim_key,idempotency_key,memory_form,subject,predicate,atom,evidence_ids) VALUES('r0-bootstrap-candidate-20260809',$1,'r0-bootstrap-claim-20260809','r0-bootstrap-idempotency-20260809','semantic','synthetic-bootstrap','qualifies',$2::jsonb,$3::jsonb) RETURNING row_to_json(memory_atom_candidates)::text"
                  job-id atom-json evidence-json :single))
               (root-json
                 (pomo:query
                  "INSERT INTO memory_atom_candidate_roots(candidate_id,evidence_id,ordinality,evidence_sha256) VALUES('r0-bootstrap-candidate-20260809',$1,1,$2) RETURNING row_to_json(memory_atom_candidate_roots)::text"
                  root-id (make-string 64 :initial-element #\0) :single)))
          (setf rows
                (list
                 (list "memory_atom_rollouts"
                       (shasht:read-json rollout-json) rollout-json)
                 (list "memory_atom_jobs" job-row job-json)
                 (list "memory_atom_candidates"
                       (shasht:read-json candidate-json) candidate-json)
                 (list "memory_atom_candidate_roots"
                       (shasht:read-json root-json) root-json)))))))
  (dolist (entry rows)
    (let* ((table (first entry))
           (row (second entry))
           (row-json (third entry))
           (key
             (cond
               ((string= table "memory_atom_rollouts")
                (obj "rollout_id" (gethash "rollout_id" row)))
               ((string= table "memory_atom_jobs")
                (obj "id" (gethash "id" row)))
               ((string= table "memory_atom_candidates")
                (obj "candidate_id" (gethash "candidate_id" row)))
               (t
                (obj "candidate_id" (gethash "candidate_id" row)
                     "evidence_id" (gethash "evidence_id" row))))))
      (log-postgres-row-state table "upsert" key row row-json)
      (incf row-events)))

  (unless (and (= file-events 8) (= row-events 4)
               (= *event-next-id* (+ boundary 12)))
    (error "Synthetic tail contract failed"))
  (format t
          "R0_BOOTSTRAP_TAIL_PASS boundary=~d through=~d file_events=~d row_events=~d provider_calls=0~%"
          boundary *event-next-id* file-events row-events))
