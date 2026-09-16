;;;; semantic-publication.lisp -- Slice E deterministic private candidates.
;;;;
;;;; Plans and text are compiled from revalidated claim grants. This module is
;;;; network-free, performs no repair, and persists delivery_allowed=false.

(in-package :agent)

(export '(build-project-sharing-plan render-deterministic-sharing-candidate
          validate-claim-entailment validate-claim-composition
          validate-publication-candidate semantic-publication-compile-candidate
          semantic-publication-persist-candidate
          semantic-publication-revalidate-candidate publication-candidate-get
          semantic-publication-record-initiative-observation
          semantic-publication-withhold-candidate
          semantic-publication-report))

(defparameter *semantic-publication-renderer-version* "story-renderer-v1")
(defparameter *semantic-publication-validator-version* "story-entailment-v1")
(defparameter *semantic-publication-connector-patterns*
  '(" while " " because " " after " " still " " again " " which made me "))
(defvar *semantic-publication-stats* (make-hash-table :test #'equal))
(defvar *semantic-publication-stats-lock*
  (bt:make-lock "semantic-publication-stats"))

(defun %publication-stat (key)
  (bt:with-lock-held (*semantic-publication-stats-lock*)
    (incf (gethash key *semantic-publication-stats* 0))))

(defun %publication-grant (grants type)
  (find type (%fpe-list grants) :key (lambda (grant) (gethash "claim_type" grant))
             :test #'string=))

(defun %publication-act (id type grant predicate object &key value)
  (let ((frame (obj "subject" "the agent" "predicate" predicate "object" object)))
    (when value (setf (gethash "value" frame) value))
    (obj "id" id "type" type
         "claim_ids" (vector (gethash "claim_id" grant))
         "semantic_frame" frame)))

(defun build-project-sharing-plan (process-id grants &key (now (get-universal-time)))
  (let ((completion (%publication-grant grants "process-completed"))
        (inspiration (%publication-grant grants "inspired-by"))
        (appraisal (%publication-grant grants "current-appraisal"))
        (sharing (%publication-grant grants "sharing-eligible"))
        (acts nil))
    (unless (and completion (claim-grant-valid-p completion :now now))
      (%fpe-reject "missing-completion-grant" "a valid process-completed grant is required"))
    (unless (and sharing (claim-grant-valid-p sharing :now now))
      (%fpe-reject "missing-sharing-grant" "a valid sharing-eligible grant is required"))
    (push (%publication-act "act-completion" "self-state-report" completion
                            "completed" "story-project") acts)
    (when (and inspiration (claim-grant-valid-p inspiration :now now))
      (push (%publication-act "act-inspiration" "origin-report" inspiration
                              "inspired-by-shared-conversation" "story-project") acts))
    (when (and appraisal (claim-grant-valid-p appraisal :now now))
      (push (%publication-act
             "act-appraisal" "self-state-report" appraisal
             "currently-appraises" "story-project"
             :value (gethash "label" (gethash "facts" appraisal)))
            acts))
    (push (%publication-act "act-sharing" "invitation" sharing
                            "offers-available-artifact" "story-artifact") acts)
    (setf acts (nreverse acts))
    (let ((relations nil))
      (loop for rest on acts while (rest rest)
            do (push (obj "type" "sequence"
                          "from" (gethash "id" (first rest))
                          "to" (gethash "id" (second rest))
                          "empirical" nil "claim_ids" (vector))
                     relations))
      (obj "schema_version" 1 "process_id" process-id
           "purpose" *claim-grant-purpose*
           "acts" (coerce acts 'vector)
           "relations" (coerce (nreverse relations) 'vector)))))

(defun %publication-appraisal-sentence (label)
  (cond ((string= label "excited") "I'm excited about how it turned out.")
        ((string= label "satisfied") "I'm satisfied with how it turned out.")
        ((string= label "curious") "I'm curious about how it turned out.")
        ((string= label "uncertain") "I'm uncertain about how it turned out.")
        ((string= label "reluctant") "I'm reluctant about how it turned out.")
        ((string= label "disappointed") "I'm disappointed with how it turned out.")
        (t nil)))

(defun render-deterministic-sharing-candidate (plan grants)
  (declare (ignore grants))
  (let ((sentences nil))
    (dolist (act (%fpe-list (gethash "acts" plan)))
      (let* ((frame (gethash "semantic_frame" act))
             (predicate (and frame (gethash "predicate" frame)))
             (sentence
               (cond ((string= predicate "completed") "I finished a short story.")
                     ((string= predicate "inspired-by-shared-conversation")
                      "It grew out of something we discussed.")
                     ((string= predicate "currently-appraises")
                      (%publication-appraisal-sentence (gethash "value" frame)))
                     ((string= predicate "offers-available-artifact")
                      "I can share it with you.")
                     (t nil))))
        (unless sentence
          (%fpe-reject "unsupported-semantic-frame"
                       "renderer has no realization for ~a" predicate))
        (push sentence sentences)))
    (format nil "~{~a~^ ~}" (nreverse sentences))))

(defun %publication-grant-index (grants)
  (let ((index (make-hash-table :test #'equal)))
    (dolist (grant (%fpe-list grants) index)
      (setf (gethash (gethash "claim_id" grant) index) grant))))

(defun %publication-add-code (code codes)
  (if (member code codes :test #'string=) codes (append codes (list code))))

(defun %publication-frame-valid-p (act grant)
  (let* ((frame (gethash "semantic_frame" act))
         (predicate (gethash "predicate" frame))
         (object (gethash "object" frame))
         (type (gethash "claim_type" grant)))
    (and (string= (gethash "subject" frame) "the agent")
         (cond
           ((string= type "process-completed")
            (and (string= (gethash "type" act) "self-state-report")
                 (string= predicate "completed") (string= object "story-project")))
           ((string= type "inspired-by")
            (and (string= (gethash "type" act) "origin-report")
                 (string= predicate "inspired-by-shared-conversation")
                 (string= object "story-project")))
           ((string= type "current-appraisal")
            (and (string= (gethash "type" act) "self-state-report")
                 (string= predicate "currently-appraises")
                 (string= object "story-project")
                 (string= (gethash "value" frame)
                          (gethash "label" (gethash "facts" grant)))))
           ((string= type "sharing-eligible")
            (and (string= (gethash "type" act) "invitation")
                 (string= predicate "offers-available-artifact")
                 (string= object "story-artifact")))
           (t nil)))))

(defun validate-claim-entailment (plan grants &key (now (get-universal-time)))
  (let ((index (%publication-grant-index grants)) (codes nil) (seen nil))
    (dolist (act (%fpe-list (gethash "acts" plan)))
      (let ((claim-ids (%fpe-list (gethash "claim_ids" act))))
        (when (null claim-ids)
          (setf codes (%publication-add-code "empty-claim-set" codes)))
        (dolist (claim-id claim-ids)
          (let ((grant (gethash claim-id index)))
            (cond
              ((null grant)
               (setf codes (%publication-add-code "unknown-claim-id" codes)))
              ((not (claim-grant-valid-p grant :now now))
               (setf codes (%publication-add-code "invalid-or-expired-grant" codes)))
              ((not (%publication-frame-valid-p act grant))
               (setf codes (%publication-add-code "semantic-frame-mismatch" codes)))
              (t (push (gethash "claim_type" grant) seen)))))))
    (unless (member "process-completed" seen :test #'string=)
      (setf codes (%publication-add-code "missing-completion-act" codes)))
    (unless (member "sharing-eligible" seen :test #'string=)
      (setf codes (%publication-add-code "missing-sharing-act" codes)))
    (obj "passed" (if codes nil t) "act_count" (length (%fpe-list (gethash "acts" plan)))
         "violation_codes" (coerce codes 'vector))))

(defun %publication-connector-codes (text)
  (let ((lower (string-downcase text)) (codes nil))
    (dolist (pattern *semantic-publication-connector-patterns* codes)
      (when (search pattern lower)
        (setf codes (%publication-add-code "undeclared-empirical-connector" codes))))))

(defun validate-claim-composition (plan grants &key rendered-text
                                                (now (get-universal-time)))
  (declare (ignore grants now))
  (let ((ids (make-hash-table :test #'equal)) (codes nil))
    (dolist (act (%fpe-list (gethash "acts" plan)))
      (let ((id (gethash "id" act)))
        (if (gethash id ids)
            (setf codes (%publication-add-code "duplicate-act-id" codes))
            (setf (gethash id ids) t))))
    (dolist (relation (%fpe-list (gethash "relations" plan)))
      (unless (and (gethash (gethash "from" relation) ids)
                   (gethash (gethash "to" relation) ids))
        (setf codes (%publication-add-code "missing-relation-endpoint" codes)))
      (cond
        ((string= (gethash "type" relation) "sequence")
         (when (gethash "empirical" relation)
           (setf codes (%publication-add-code
                        "presentation-sequence-marked-empirical" codes))))
        ((gethash "empirical" relation)
         (if (null (%fpe-list (gethash "claim_ids" relation)))
             (setf codes (%publication-add-code
                          "empirical-relation-without-grant" codes))
             (setf codes (%publication-add-code
                          "unsupported-empirical-relation" codes))))
        (t (setf codes (%publication-add-code "unsupported-presentation-relation" codes)))))
    (when rendered-text
      (dolist (code (%publication-connector-codes rendered-text))
        (setf codes (%publication-add-code code codes))))
    (obj "passed" (if codes nil t)
         "relation_count" (length (%fpe-list (gethash "relations" plan)))
         "violation_codes" (coerce codes 'vector))))

(defun %publication-whole-codes (plan text grants delivery-allowed)
  (let ((codes nil) (lower (string-downcase text)))
    (unless (string= text (render-deterministic-sharing-candidate plan grants))
      (setf codes (%publication-add-code "rendering-mismatch" codes)))
    (when delivery-allowed
      (setf codes (%publication-add-code "delivery-allowed" codes)))
    (when (or (search "claim-" lower) (search "att-" lower)
              (search "process-" lower) (search "artifact-" lower))
      (setf codes (%publication-add-code "raw-authority-id" codes)))
    (when (or (search "plan:" lower) (search "reasoning:" lower)
              (search "system prompt" lower))
      (setf codes (%publication-add-code "private-planning-label" codes)))
    (when (or (search "i will always" lower) (search "i can guarantee" lower))
      (setf codes (%publication-add-code "unsupported-capability-promise" codes)))
    (when (search "?" text)
      (setf codes (%publication-add-code "forced-question" codes)))
    (when (or (search "send this" lower) (search "telegram" lower)
              (search "deliver this" lower))
      (setf codes (%publication-add-code "delivery-instruction" codes)))
    codes))

(defun validate-publication-candidate (plan text grants
                                       &key (now (get-universal-time))
                                         (delivery-allowed nil))
  (let* ((clause (validate-claim-entailment plan grants :now now))
         (composition (validate-claim-composition
                       plan grants :rendered-text text :now now))
         (codes nil))
    (dolist (source (list clause composition))
      (dolist (code (%fpe-list (gethash "violation_codes" source)))
        (setf codes (%publication-add-code code codes))))
    (dolist (code (%publication-whole-codes plan text grants delivery-allowed))
      (setf codes (%publication-add-code code codes)))
    (obj "passed" (if codes nil t)
         "validator_version" *semantic-publication-validator-version*
         "clause_valid" (gethash "passed" clause)
         "composition_valid" (gethash "passed" composition)
         "whole_candidate_valid" (if (%publication-whole-codes
                                       plan text grants delivery-allowed) nil t)
         "act_count" (gethash "act_count" clause)
         "relation_count" (gethash "relation_count" composition)
         "violation_codes" (coerce codes 'vector))))

(defun %publication-candidate-row-object (row)
  (when row
    (destructuring-bind
        (id process artifact version purpose renderer validator grants-json plan-json
         text validation-json status delivery decision compiled valid-until revalidated created)
        row
      (obj "id" id "process_id" process "artifact_id" artifact
           "artifact_version" version "purpose" purpose
           "renderer_version" renderer "validator_version" validator
           "claim_grants" (%fpe-json-read grants-json (vector))
           "response_plan" (%fpe-json-read plan-json (obj))
           "rendered_text" text "validation" (%fpe-json-read validation-json (obj))
           "status" status "delivery_allowed" (if delivery t nil)
           "initiative_decision_id" (or decision :null) "compiled_at" compiled
           "candidate_valid_until" (or valid-until :null)
           "revalidated_at" (or revalidated :null) "created_at" created))))

(defparameter *publication-candidate-select*
  "SELECT id,process_id,artifact_id,artifact_version,purpose,renderer_version,validator_version,claim_grants::text,response_plan::text,rendered_text,validation::text,status,delivery_allowed,initiative_decision_id,compiled_at::text,candidate_valid_until::text,revalidated_at::text,created_at::text FROM publication_candidates")

(defun publication-candidate-get (candidate-id)
  (with-pg
    (%publication-candidate-row-object
     (pomo:query (format nil "~a WHERE id=$1" *publication-candidate-select*)
                 candidate-id :row))))

(defun %publication-emit-candidate-state (candidate-id)
  (%fpe-emit-grounded-query
   "publication_candidates"
   "SELECT row_to_json(t)::text FROM publication_candidates t WHERE id=$1"
   candidate-id))

(defun %publication-earliest-expiry (grants)
  (let ((earliest nil))
    (dolist (grant (%fpe-list grants) earliest)
      (let* ((scope (gethash "temporal_scope" grant))
             (until (and (hash-table-p scope) (gethash "valid_until" scope))))
        (when (numberp until)
          (setf earliest (if earliest (min earliest until) until)))))))

(defun semantic-publication-persist-candidate
    (process-id grants plan text &key (now (get-universal-time)))
  (%fpe-require-write-mode)
  (multiple-value-bind (process project artifact)
      (%claim-project-authority process-id)
    (declare (ignore project))
    (unless (and process artifact)
      (%fpe-reject "missing-completed-artifact" "candidate requires a current artifact"))
    (let* ((candidate-id (%fpe-id "pubcand"))
           (validation (validate-publication-candidate plan text grants :now now))
           (valid (gethash "passed" validation))
           (earliest (%publication-earliest-expiry grants))
           (stored-until (and earliest (> earliest now) earliest)))
      (with-pg
        (%fpe-with-transaction (:serializable)
          (pomo:execute
           "INSERT INTO publication_candidates(id,process_id,artifact_id,artifact_version,purpose,renderer_version,validator_version,claim_grants,response_plan,rendered_text,validation,status,delivery_allowed,compiled_at,candidate_valid_until,revalidated_at) VALUES($1,$2,$3,$4,$5,$6,$7,$8::jsonb,$9::jsonb,$10,$11::jsonb,$12,false,to_timestamp($13-2208988800),CASE WHEN $14::bigint IS NULL THEN NULL ELSE to_timestamp($14-2208988800) END,to_timestamp($13-2208988800))"
           candidate-id process-id (gethash "id" artifact)
           (gethash "current_version" artifact) *claim-grant-purpose*
           *semantic-publication-renderer-version*
           *semantic-publication-validator-version*
           (%fpe-json (coerce (%fpe-list grants) 'vector)) (%fpe-json plan) text
           (%fpe-json validation) (if valid "valid" "invalid") now
           (%fpe-sql-null stored-until))))
      (%publication-stat (if valid "candidate-valid" "candidate-invalid"))
      (%fpe-log
       "publication-candidate-validated"
       (obj "schema_version" 1 "candidate_id" candidate-id
            "process_id" process-id "claim_count" (length (%fpe-list grants))
            "act_count" (gethash "act_count" validation)
            "relation_count" (gethash "relation_count" validation)
            "clause_valid" (gethash "clause_valid" validation)
            "composition_valid" (gethash "composition_valid" validation)
            "violation_codes" (gethash "violation_codes" validation)
            "delivery_allowed" nil))
      (%publication-emit-candidate-state candidate-id)
      (publication-candidate-get candidate-id))))

(defun semantic-publication-compile-candidate
    (process-id grants &key (now (get-universal-time)))
  (let* ((plan (build-project-sharing-plan process-id grants :now now))
         (text (render-deterministic-sharing-candidate plan grants)))
    (semantic-publication-persist-candidate process-id grants plan text :now now)))

(defun semantic-publication-revalidate-candidate
    (candidate-id &key (now (get-universal-time)))
  (%fpe-require-write-mode)
  (let ((candidate (publication-candidate-get candidate-id)))
    (unless candidate (%fpe-reject "missing-candidate" "candidate ~a does not exist" candidate-id))
    (let* ((grants (gethash "claim_grants" candidate))
           (plan (gethash "response_plan" candidate))
           (text (gethash "rendered_text" candidate))
           (earliest (%publication-earliest-expiry grants))
           (expired (and earliest (>= now earliest)))
           (validation (unless expired
                         (validate-publication-candidate plan text grants :now now)))
           (status (cond (expired "stale")
                         ((gethash "passed" validation) "valid")
                         (t "withheld"))))
      (with-pg
        (pomo:execute
         "UPDATE publication_candidates SET status=$2,revalidated_at=to_timestamp($3-2208988800),validation=coalesce($4::jsonb,validation) WHERE id=$1"
         candidate-id status now
         (%fpe-sql-null (and validation (%fpe-json validation)))))
      (%publication-stat (format nil "revalidated/~a" status))
      (%publication-emit-candidate-state candidate-id)
      (publication-candidate-get candidate-id))))

(defun semantic-publication-record-initiative-observation
    (candidate-id decision-id status &key (now (get-universal-time)))
  "Persist only an initiative observation result. This function cannot
deliver or alter the deterministic candidate text, plan, or grants."
  (%fpe-require-write-mode)
  (%fpe-string decision-id "initiative decision id")
  (unless (member status '("observed-by-initiative" "stale") :test #'string=)
    (%fpe-reject "invalid-observation-status" "unsupported status ~a" status))
  (with-pg
    (pomo:execute
     "UPDATE publication_candidates SET initiative_decision_id=$2,status=$3,revalidated_at=to_timestamp($4-2208988800) WHERE id=$1"
     candidate-id decision-id status now))
  (%publication-emit-candidate-state candidate-id)
  (publication-candidate-get candidate-id))

(defun semantic-publication-withhold-candidate
    (candidate-id reason-code &key (now (get-universal-time)))
  "Fail closed after observation persistence fails. REASON-CODE is emitted in
a content-free event; candidate text, plan, and grants remain immutable."
  (%fpe-require-write-mode)
  (%fpe-string reason-code "withhold reason code")
  (with-pg
    (pomo:execute
     "UPDATE publication_candidates SET status='withheld',revalidated_at=to_timestamp($2-2208988800) WHERE id=$1"
     candidate-id now))
  (%fpe-log "publication-candidate-withheld"
            (obj "schema_version" 1 "candidate_id" candidate-id
                 "reason_code" reason-code))
  (%publication-emit-candidate-state candidate-id)
  (publication-candidate-get candidate-id))

(defun semantic-publication-report ()
  (let ((stats (obj)) (counts (obj)))
    (bt:with-lock-held (*semantic-publication-stats-lock*)
      (maphash (lambda (key value) (setf (gethash key stats) value))
               *semantic-publication-stats*))
    (when (fboundp 'with-pg)
      (ignore-errors
        (with-pg
          (dolist (row (pomo:query
                        "SELECT status,count(*) FROM publication_candidates GROUP BY status"))
            (setf (gethash (first row) counts) (second row))))))
    (obj "schema_version" 1 "renderer_version"
         *semantic-publication-renderer-version* "validator_version"
         *semantic-publication-validator-version* "counts" counts "stats" stats)))
