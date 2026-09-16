;;;; claim-grants.lisp -- Slice D deterministic claim authority compiler.
;;;;
;;;; Grants are compiled only from current durable attestations and exact
;;;; artifact rows. They are data for a later private candidate, never model
;;;; output and never a delivery permission.

(in-package :agent)

(export '(compile-claim-grants claim-grant-valid-p claim-grant-set-report))

(defparameter *claim-grant-purpose* "private-project-sharing-shadow")
(defparameter *claim-grant-progress-ttl-seconds* 86400)
(defparameter *claim-grant-candidate-ttl-seconds* 7200)
(defvar *claim-grant-stats* (make-hash-table :test #'equal))
(defvar *claim-grant-stats-lock* (bt:make-lock "claim-grant-stats"))

(defun %claim-stat (key)
  (bt:with-lock-held (*claim-grant-stats-lock*)
    (incf (gethash key *claim-grant-stats* 0))))

(defun %claim-vector (&rest values)
  (coerce (remove nil values) 'vector))

(defun %claim-grant (type predicate object-id process-id attestation-ids rendering
                     &key valid-until facts evidence-node-ids artifact-id
                       artifact-version artifact-sha256)
  (obj "claim_id" (%fpe-id "claim") "claim_type" type
       "purpose" *claim-grant-purpose* "subject" "the agent"
       "predicate" predicate "object_id" object-id "process_id" process-id
       "attestation_ids" (coerce (%fpe-list attestation-ids) 'vector)
       "temporal_scope" (obj "valid_until" (or valid-until :null))
       "canonical_rendering" rendering
       "facts" (or facts (obj))
       "evidence_node_ids" (coerce (%fpe-list evidence-node-ids) 'vector)
       "artifact_id" (or artifact-id :null)
       "artifact_version" (or artifact-version :null)
       "artifact_sha256" (or artifact-sha256 :null)))

(defun %claim-attestation-current (process-id type &key artifact-id artifact-version)
  (with-pg
    (%fpe-attestation-row-object
     (pomo:query
      (format nil
              "~a WHERE process_id=$1 AND attestation_type=$2 AND ($3::text IS NULL OR artifact_id=$3) AND ($4::integer IS NULL OR artifact_version=$4) AND superseded_at IS NULL ORDER BY issued_at DESC LIMIT 1"
              *appraisal-attestation-select*)
      process-id type (%fpe-sql-null artifact-id)
      (%fpe-sql-null artifact-version) :row))))

(defun %claim-shared-conversation-root-p (node)
  (and (%fpe-grounded-node-p node)
       (gethash "shared_conversation" node)
       (let ((participants (%fpe-list (gethash "participants" node))))
         (and (member "the operator" participants :test #'string=)
              (member "the agent" participants :test #'string=)))))

(defun %claim-inspiration-authorized-p (attestation)
  (and attestation
       (some (lambda (id)
               (%claim-shared-conversation-root-p
                (funcall *first-person-evidence-node-fn* id)))
             (%fpe-list (gethash "evidence_node_ids" attestation)))))

(defun %claim-project-authority (process-id)
  (with-pg
    (let* ((process (%fpe-process-current process-id))
           (project (and process (%fpe-project-current process-id)))
           (artifact-id (and project (gethash "current_artifact_id" project)))
           (artifact (and (stringp artifact-id)
                          (%fpe-artifact-current artifact-id))))
      (values process project artifact))))

(defun compile-claim-grants (process-id purpose &key (now (get-universal-time)))
  (%fpe-require-write-mode)
  (unless (string= (string-downcase (string purpose)) *claim-grant-purpose*)
    (%fpe-reject "claim-purpose" "unsupported claim-grant purpose ~a" purpose))
  (multiple-value-bind (process project artifact)
      (%claim-project-authority process-id)
    (unless (and process project)
      (%fpe-reject "missing-project" "process/project ~a does not exist" process-id))
    (let* ((project-id (gethash "id" project))
           (artifact-id (and artifact (gethash "id" artifact)))
           (artifact-version (and artifact (gethash "current_version" artifact)))
           (artifact-sha (and artifact (gethash "sha256" artifact)))
           (started (%claim-attestation-current process-id "process-started"))
           (progress (%claim-attestation-current process-id "process-progress"))
           (completed (%claim-attestation-current process-id "process-completed"))
           (artifact-completed
             (and artifact (%claim-attestation-current
                            process-id "artifact-completed"
                            :artifact-id artifact-id
                            :artifact-version artifact-version)))
           (artifact-state
             (and artifact
                  (or artifact-completed
                      (%claim-attestation-current
                       process-id (if (> artifact-version 1)
                                      "artifact-revised" "artifact-created")
                       :artifact-id artifact-id
                       :artifact-version artifact-version))))
           (validation
             (and artifact (%claim-attestation-current
                            process-id "artifact-validated"
                            :artifact-id artifact-id
                            :artifact-version artifact-version)))
           (inspiration (%claim-attestation-current process-id "project-inspired-by"))
           (appraisal (agent-appraisal-current process-id :now now))
           (grants nil))
      (when started
        (push (%claim-grant
               "project-exists" "exists" project-id process-id
               (%claim-vector (gethash "id" started)) "I started a private story project.")
              grants))
      (when (and progress
                 (member (gethash "state" process)
                         '("active" "incubating" "blocked") :test #'string=))
        (let* ((issued
                 (with-pg
                   (round
                    (pomo:query
                     "SELECT extract(epoch from issued_at) FROM agent_attestations WHERE id=$1"
                     (gethash "id" progress) :single))))
               (expires (+ issued 2208988800
                           *claim-grant-progress-ttl-seconds*)))
          (when (> expires now)
            (push (%claim-grant
                   "process-progressed" "progressed" project-id process-id
                   (%claim-vector (gethash "id" progress))
                   "I made progress on the story."
                   :valid-until expires)
                  grants))))
      (when (and completed artifact-completed
                 (string= (gethash "state" process) "completed")
                 (string= (gethash "status" artifact) "completed"))
        (push (%claim-grant
               "process-completed" "completed" project-id process-id
               (%claim-vector (gethash "id" completed)
                              (gethash "id" artifact-completed))
               "I finished the story."
               :artifact-id artifact-id :artifact-version artifact-version
               :artifact-sha256 artifact-sha)
              grants))
      (when artifact-state
        (push (%claim-grant
               "artifact-exists" "exists" artifact-id process-id
               (%claim-vector (gethash "id" artifact-state))
               "The private story artifact exists."
               :artifact-id artifact-id :artifact-version artifact-version
               :artifact-sha256 artifact-sha)
              grants)
        (let ((version-count
                (with-pg
                  (pomo:query
                   "SELECT count(*) FROM agent_artifact_versions WHERE artifact_id=$1"
                   artifact-id :single)))
              (artifact-attestations
                (with-pg
                  (pomo:query
                   "SELECT id FROM agent_attestations WHERE process_id=$1 AND artifact_id=$2 AND attestation_type IN ('artifact-created','artifact-revised') ORDER BY artifact_version"
                   process-id artifact-id :column))))
          (push (%claim-grant
                 "artifact-version-count" "has-version-count" artifact-id process-id
                 artifact-attestations
                 (format nil "The story has ~a durable version~:p." version-count)
                 :facts (obj "version_count" version-count)
                 :artifact-id artifact-id :artifact-version artifact-version
                 :artifact-sha256 artifact-sha)
                grants)))
      (when appraisal
        (let* ((facts (gethash "facts" appraisal))
               (expires (gethash "valid_until_universal" facts)))
          (push (%claim-grant
                 "current-appraisal" "currently-appraises" project-id process-id
                 (%claim-vector (gethash "id" appraisal))
                 (format nil "My current appraisal of the project is ~a."
                         (gethash "label" facts))
                 :valid-until expires
                 :facts (obj "label" (gethash "label" facts)
                             "intensity" (gethash "intensity" facts)
                             "confidence" (gethash "confidence" facts)))
                grants)))
      (when (%claim-inspiration-authorized-p inspiration)
        (push (%claim-grant
               "inspired-by" "inspired-by-shared-conversation" project-id process-id
               (%claim-vector (gethash "id" inspiration))
               "The story was inspired by something we discussed."
               :evidence-node-ids (gethash "evidence_node_ids" inspiration))
              grants))
      (when (and completed artifact-completed validation
                 (string= (gethash "sharing_condition" project)
                          "completed-and-validated")
                 (string= (gethash "state" process) "completed")
                 (string= (gethash "status" artifact) "completed"))
        (push (%claim-grant
               "sharing-eligible" "sharing-eligible" artifact-id process-id
               (%claim-vector (gethash "id" completed)
                              (gethash "id" artifact-completed)
                              (gethash "id" validation))
               "I can make the completed private story available."
               :valid-until (+ now *claim-grant-candidate-ttl-seconds*)
               :artifact-id artifact-id :artifact-version artifact-version
               :artifact-sha256 artifact-sha)
              grants))
      (let ((result (coerce (nreverse grants) 'vector)))
        (%claim-stat "compiled")
        (%claim-stat (format nil "grant-count/~a" (length result)))
        result))))

(defun %claim-attestations-by-id (ids)
  (mapcar
   (lambda (id)
     (with-pg
       (%fpe-attestation-row-object
        (pomo:query (format nil "~a WHERE id=$1" *appraisal-attestation-select*)
                    id :row))))
   (%fpe-list ids)))

(defun %claim-expired-p (grant now)
  (let* ((scope (gethash "temporal_scope" grant))
         (until (and (hash-table-p scope) (gethash "valid_until" scope))))
    (and (numberp until) (>= now until))))

(defun %claim-artifact-matches-p (grant)
  (let ((artifact-id (gethash "artifact_id" grant))
        (version (gethash "artifact_version" grant))
        (sha (gethash "artifact_sha256" grant)))
    (and (stringp artifact-id) (integerp version) (stringp sha)
         (let ((artifact (agent-artifact-get artifact-id :version version)))
           (and artifact (string= sha (gethash "sha256" artifact)))))))

(defun %claim-artifact-attestations-match-p (grant attestations &key allow-prior-versions)
  (let ((artifact-id (gethash "artifact_id" grant))
        (version (gethash "artifact_version" grant)))
    (every
     (lambda (attestation)
       (if (member (gethash "attestation_type" attestation)
                   '("artifact-created" "artifact-revised" "artifact-validated"
                     "artifact-completed") :test #'string=)
           (and (string= artifact-id (gethash "artifact_id" attestation))
                (integerp (gethash "artifact_version" attestation))
                (if allow-prior-versions
                    (<= (gethash "artifact_version" attestation) version)
                    (= (gethash "artifact_version" attestation) version)))
           t))
     attestations)))

(defun claim-grant-valid-p (grant &key (now (get-universal-time)))
  (handler-case
      (let* ((type (and (hash-table-p grant) (gethash "claim_type" grant)))
             (process-id (and (hash-table-p grant) (gethash "process_id" grant)))
             (attestations
               (and (hash-table-p grant)
                    (%claim-attestations-by-id (gethash "attestation_ids" grant))))
             (types (remove nil (mapcar (lambda (item)
                                         (and item (gethash "attestation_type" item)))
                                       attestations))))
        (and (stringp type) (stringp process-id)
             (string= (gethash "purpose" grant) *claim-grant-purpose*)
             (string= (gethash "subject" grant) "the agent")
             (not (%claim-expired-p grant now))
             (every #'identity attestations)
             (every (lambda (attestation)
                      (string= process-id (gethash "process_id" attestation)))
                    attestations)
             (multiple-value-bind (process project artifact)
                 (%claim-project-authority process-id)
               (and process project
                    (cond
                      ((string= type "project-exists")
                       (and (string= (gethash "predicate" grant) "exists")
                            (string= (gethash "object_id" grant) (gethash "id" project))
                            (member "process-started" types :test #'string=)))
                      ((string= type "process-progressed")
                       (and (string= (gethash "predicate" grant) "progressed")
                            (string= (gethash "object_id" grant) (gethash "id" project))
                            (member (gethash "state" process)
                                    '("active" "incubating" "blocked") :test #'string=)
                            (member "process-progress" types :test #'string=)))
                      ((string= type "process-completed")
                       (and (string= (gethash "predicate" grant) "completed")
                            (string= (gethash "object_id" grant) (gethash "id" project))
                            (string= (gethash "state" process) "completed")
                            (member "process-completed" types :test #'string=)
                            (member "artifact-completed" types :test #'string=)
                            (%claim-artifact-matches-p grant)
                            (%claim-artifact-attestations-match-p grant attestations)))
                      ((string= type "artifact-exists")
                       (and (string= (gethash "predicate" grant) "exists")
                            (string= (gethash "object_id" grant)
                                     (gethash "artifact_id" grant))
                            (member-if (lambda (item)
                                         (member item '("artifact-created" "artifact-revised"
                                                        "artifact-completed") :test #'string=))
                                       types)
                            (%claim-artifact-matches-p grant)
                            (%claim-artifact-attestations-match-p grant attestations)))
                      ((string= type "artifact-version-count")
                       (and (string= (gethash "object_id" grant)
                                     (gethash "artifact_id" grant))
                            (%claim-artifact-matches-p grant)
                            (%claim-artifact-attestations-match-p
                             grant attestations :allow-prior-versions t)
                            (= (gethash "version_count" (gethash "facts" grant))
                               (with-pg
                                 (pomo:query
                                  "SELECT count(*) FROM agent_artifact_versions WHERE artifact_id=$1"
                                  (gethash "artifact_id" grant) :single)))))
                      ((string= type "current-appraisal")
                       (let ((current (agent-appraisal-current process-id :now now)))
                         (and current
                              (= 1 (length attestations))
                              (string= (gethash "id" current)
                                       (gethash "id" (first attestations)))
                              (string= (gethash "predicate" grant)
                                       "currently-appraises")
                              (string= (gethash "object_id" grant)
                                       (gethash "id" project))
                              (string= (gethash "label" (gethash "facts" grant))
                                       (gethash "label" (gethash "facts" current))))))
                      ((string= type "inspired-by")
                       (and (string= (gethash "predicate" grant)
                                     "inspired-by-shared-conversation")
                            (string= (gethash "object_id" grant)
                                     (gethash "id" project))
                            (= 1 (length attestations))
                            (string= (first types) "project-inspired-by")
                            (equalp (gethash "evidence_node_ids" grant)
                                    (gethash "evidence_node_ids" (first attestations)))
                            (%claim-inspiration-authorized-p (first attestations))))
                      ((string= type "sharing-eligible")
                       (and artifact
                            (string= (gethash "predicate" grant) "sharing-eligible")
                            (string= (gethash "object_id" grant)
                                     (gethash "artifact_id" grant))
                            (string= (gethash "sharing_condition" project)
                                     "completed-and-validated")
                            (string= (gethash "state" process) "completed")
                            (string= (gethash "status" artifact) "completed")
                            (member "process-completed" types :test #'string=)
                            (member "artifact-completed" types :test #'string=)
                            (member "artifact-validated" types :test #'string=)
                            (%claim-artifact-matches-p grant)
                            (%claim-artifact-attestations-match-p grant attestations)))
                      (t nil))))))
    (error () nil)))

(defun claim-grant-set-report (grants &key (now (get-universal-time)))
  (let ((counts (obj)) (valid 0) (invalid 0) (earliest nil))
    (dolist (grant (%fpe-list grants))
      (incf (gethash (gethash "claim_type" grant) counts 0))
      (if (claim-grant-valid-p grant :now now) (incf valid) (incf invalid))
      (let* ((scope (gethash "temporal_scope" grant))
             (until (and (hash-table-p scope) (gethash "valid_until" scope))))
        (when (numberp until) (setf earliest (if earliest (min earliest until) until)))))
    (obj "schema_version" 1 "grant_count" (+ valid invalid)
         "valid_count" valid "invalid_count" invalid
         "earliest_valid_until" (or earliest :null) "counts" counts)))
