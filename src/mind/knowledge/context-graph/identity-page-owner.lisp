;;;; Frozen discovery and durable identity comparison; never graph admission.
(in-package :pai.context-graph)

(declaim (special *cgf-protocol*))

(defun %cgi-inference-identity-anchors (graph entity-id candidate)
  "Expose bounded current inferred incident facts as comparison-only context."
  (let ((rows
          (sort
           (loop for fact-id across
                   (gethash entity-id
                            (context-graph-entity-adjacency graph) #())
                 for fact = (gethash fact-id (context-graph-facts graph))
                 when (and fact
                           (equal "inference"
                                  (gethash "evidence_status" fact))
                           (or (equal "current" (gethash "status" fact))
                               (eq t (gethash "current" fact))))
                   collect
                   (let* ((subject-p
                            (equal entity-id (gethash "subject_id" fact)))
                          (other-id
                            (gethash (if subject-p "object_id" "subject_id")
                                     fact))
                          (other
                            (gethash other-id
                                     (context-graph-entities graph))))
                     (%cg-object
                      "candidate" candidate
                      "direction" (if subject-p "subject" "object")
                      "predicate" (gethash "predicate" fact)
                      "other_label" (gethash "label" other)
                      "statement" (gethash "fact" fact)
                      "evidence_status" "inference")))
           #'string< :key (lambda (row) (gethash "statement" row)))))
    (coerce (subseq rows 0 (min 4 (length rows))) 'vector)))

(defun %cgi-discover (graph full batch mentions)
  (let* ((base (%cgro-batch-context graph full batch "staged" "bounded-v3"))
         (sources (gethash "sources" (gethash "source_packet" base)))
         (query (format nil "~{~a~^ ~}" (map 'list (lambda (s) (gethash "text" s)) sources)))
         (index (context-graph-entity-scan-index graph)) (rows nil) (exact-count 0) (total 0)
         (lexicon (%cg-object "entity_types" (make-hash-table :test #'equal)))
         (terms (%cgr-tokens query)))
    (unless (%cg-authority-string-p query 70000) (%cg-authority-fail "IDENTITY_SOURCE_LIMIT"))
    (when (> (length index) 1024) (%cg-authority-fail "IDENTITY_SCAN_LIMIT"))
    (loop for id across index for entity = (gethash id (context-graph-entities graph)) do
      (when (eq :null (gethash "participant_role" entity))
        (incf total)
        (let ((exact (some (lambda (name) (%cgr-term-in-range-p name query 0 (length query)))
                           (cons (gethash "label" entity) (coerce (gethash "aliases" entity) 'list))))
              (score (%cgr-score terms (%cgr-entity-fields entity lexicon))))
          (when exact (incf exact-count))
          (when (or exact (plusp score))
            (push (%cg-object "id" id "score" score "exact" (if exact :true :false)) rows)))))
    (when (> exact-count 96) (%cg-authority-fail "IDENTITY_CANDIDATE_LIMIT"))
    (setf rows (sort rows (lambda (a b)
                          (if (eq (gethash "exact" a) (gethash "exact" b)) (%cgr-before-p a b)
                              (eq :true (gethash "exact" a))))))
    (let* ((selected (sort (mapcar (lambda (r) (gethash "id" r))
                          (subseq rows 0 (min (length rows) (max 48 exact-count)))) #'string<))
           (pages nil))
      (loop for start from 0 below (max 1 (length selected)) by 12 do
        (let ((copy (%cg-detach base)) (ids (subseq selected start (min (length selected) (+ start 12)))))
          (setf (gethash "schema_version" copy) 1
                (gethash "episode_id" copy) (concatenate 'string "identity-batch:"
                  (%cg-authority-digest "identity-source-batch-v1"
                    (vector (gethash "episode_id" full) batch (gethash "primary_source_ids" base))))
                (gethash "eligible_entities" copy) (coerce (mapcar (lambda (id) (%cg-detach (gethash id (context-graph-entities graph)))) ids) 'vector)
                (gethash "correction_scopes" copy) #()
                (gethash "candidate_scan" copy) (%cg-object "complete" (if (= (length ids) total) :true :false) "examined_count" (length index)))
          (remhash "correction_scans" copy)
          (push copy pages)))
      (let ((plan (%cgi-plan (coerce (nreverse pages) 'vector) mentions)))
        (when *cgi-inference-identity-anchors-p*
          (let ((anchors
                  (coerce
                   (loop for entry across (gethash "candidates" plan)
                         for descriptor = (gethash "descriptor" entry)
                         when (and descriptor
                                   (not (gethash "role" descriptor)))
                           append
                           (coerce
                            (%cgi-inference-identity-anchors
                             graph (gethash "entity_id" descriptor)
                             (gethash "candidate" (gethash "card" entry)))
                            'list))
                   'vector)))
            ;; Absence preserves every earlier request digest exactly.
            (when (plusp (length anchors))
              (setf (gethash "candidate_identity_anchors" plan) anchors))))
        (%cg-object "policy_revision" "identity-discovery-v1"
                    "examined_count" (length index)
                    "exact_match_count" exact-count
                    "selected_count" (length selected)
                    "all_nonparticipants_selected"
                    (if (= total (length selected)) :true :false)
                    "plan" plan)))))

(defstruct (cgi-owner (:constructor %cgi-owner-create (graph agent-id persona-id &optional (protocol "identity-owner-v1"))))
  graph agent-id persona-id (protocol "identity-owner-v1") revision (last-id 0)
  (opens (make-hash-table :test #'eql)) (terminals (make-hash-table :test #'eql))
  (phases (make-hash-table :test #'equal)) (tasks (make-hash-table :test #'equal))
  (applications (make-hash-table :test #'eql)))

(define-condition context-graph-call-failure (error)
  ((classification :initarg :classification :reader context-graph-call-failure-classification)
   (retryable-p :initarg :retryable-p :reader context-graph-call-failure-retryable-p)
   (charged-microusd :initarg :charged-microusd :initform :null
                     :reader context-graph-call-failure-charged-microusd))
  (:report (lambda (condition stream)
             (format stream "Context graph call failed (~a)"
                     (context-graph-call-failure-classification condition)))))

(define-condition context-graph-reservation-failure (error)
  ((classification :initarg :classification
                   :reader context-graph-reservation-failure-classification)
   (phase :initarg :phase :reader context-graph-reservation-failure-phase)
   (requested-microusd :initarg :requested-microusd
                       :reader context-graph-reservation-failure-requested-microusd)
   (ceiling-microusd :initarg :ceiling-microusd
                     :reader context-graph-reservation-failure-ceiling-microusd))
  (:report (lambda (condition stream)
             (format stream
                     "Context graph ~a reservation ~d exceeds ceiling ~d"
                     (context-graph-reservation-failure-phase condition)
                     (context-graph-reservation-failure-requested-microusd condition)
                     (context-graph-reservation-failure-ceiling-microusd condition)))))

(defparameter +cgi-owner-retry-delays-seconds+ #(30 120 600))
(defparameter +cgi-owner-maximum-attempts+ 4)
(defparameter +cgi-owner-version-repair-attempt+ 5)

(defparameter +cgi-owner-model-output-authority-errors+
  '("FORMATION_MENTIONS_INVALID" "FORMATION_GROUP_INVALID"
    "FORMATION_GROUP_EVIDENCE_INVALID" "FORMATION_GROUP_OVERLAP"
    "FORMATION_GROUP_REPRESENTATIVE_INVALID"
    "FORMATION_NEW_IDENTITY_INVALID" "FORMATION_RESPONSE_INVALID"
    "STAGED_ENTITIES_INVALID" "STAGED_FACTS_INVALID"
    "IDENTITY_PAGE_RESPONSE_INVALID" "IDENTITY_PAGE_DUPLICATE"
    "IDENTITY_RESOLUTION_INVALID"))

(defun %cgi-owner-retry-generation-p (owner)
  (member (cgi-owner-protocol owner)
          '("identity-formation-owner-v6" "identity-formation-owner-v7"
            "identity-formation-owner-v8" "identity-formation-owner-v9")
          :test #'equal))

(defun %cgi-owner-formation-p (owner)
  (member (cgi-owner-protocol owner)
          '("identity-formation-owner-v1" "identity-formation-owner-v2"
            "identity-formation-owner-v3" "identity-formation-owner-v4"
            "identity-formation-owner-v5" "identity-formation-owner-v6"
            "identity-formation-owner-v7" "identity-formation-owner-v8"
            "identity-formation-owner-v9")
          :test #'equal))

(defun %cgi-owner-versioned-formation-p (owner)
  (member (cgi-owner-protocol owner)
          '("identity-formation-owner-v2" "identity-formation-owner-v3"
            "identity-formation-owner-v4" "identity-formation-owner-v5"
            "identity-formation-owner-v6" "identity-formation-owner-v7"
            "identity-formation-owner-v8" "identity-formation-owner-v9")
          :test #'equal))

(defun %cgi-owner-formation-pair-valid-p (owner formation-protocol)
  "Replacement namespaces bind one formation protocol. Older namespaces retain
their historically admitted combinations for durable replay."
  (cond ((equal "identity-formation-owner-v7" (cgi-owner-protocol owner))
         (equal "identity-formation-v10" formation-protocol))
        ((equal "identity-formation-owner-v8" (cgi-owner-protocol owner))
         (equal "identity-formation-v13" formation-protocol))
        ((equal "identity-formation-owner-v9" (cgi-owner-protocol owner))
         (equal "identity-formation-v14" formation-protocol))
        (t t)))

(defun %cgi-owner-formation-protocol (owner record)
  (if (%cgi-owner-versioned-formation-p owner)
      (gethash "formation_protocol" record)
      "identity-formation-v1"))

(defun %cgi-owner-retryable-model-output-error-p (owner record code)
  "Reviewed formation may retry invalid model output, never stale authority."
  (and (%cgi-owner-retry-generation-p owner)
        (member (%cgi-owner-formation-protocol owner record)
                '("identity-formation-v10" "identity-formation-v11"
                  "identity-formation-v12" "identity-formation-v13"
                  "identity-formation-v14")
               :test #'equal)
       (member code +cgi-owner-model-output-authority-errors+ :test #'equal)))

(defun %cgi-owner-formation-guide (owner record)
  (if (%cgi-owner-versioned-formation-p owner)
      (gethash "descriptor_guide" record)
      nil))

(defun %cgi-owner-formation-call-limit (owner record)
  (if (%cgi-owner-formation-p owner)
      (if (member (%cgi-owner-formation-protocol owner record)
                  '("identity-formation-v3" "identity-formation-v4"
                    "identity-formation-v5" "identity-formation-v6"
                    "identity-formation-v7" "identity-formation-v8"
                    "identity-formation-v9" "identity-formation-v10"
                    "identity-formation-v11" "identity-formation-v12"
                    "identity-formation-v13" "identity-formation-v14")
                  :test #'equal)
          14 13)
      nil))

(defun %cgi-owner-exposure (owner opened)
  (loop for key being the hash-keys of (cgi-owner-phases owner) using (hash-value row)
        when (= opened (first key)) sum
          (if (equal "request" (gethash "outcome" row)) (gethash "reserved_microusd" row)
              (gethash "charged_microusd" row))))

(defun %cgi-owner-fresh-p (owner opened)
  (let* ((record (%cgro-record (gethash opened (cgi-owner-opens owner))))
         (context (if (%cgi-owner-formation-p owner) (gethash "source_context" record)
                      (aref (gethash "contexts" (gethash "plan" (gethash "discovery" record))) 0))))
    (%cg-authority-equal-p (gethash "projection_watermark" context)
                          (%cg-authority-watermark (cgi-owner-graph owner) (cgi-owner-agent-id owner) (cgi-owner-persona-id owner)))))

(defun %cgi-owner-next (owner opened)
  (let* ((record (%cgro-record (gethash opened (cgi-owner-opens owner))))
         (plan (unless (%cgi-owner-formation-p owner) (gethash "plan" (gethash "discovery" record)))))
    (catch 'identity-next
      (labels ((receipt (phase spec digest)
            (let ((saved (gethash (list opened phase) (cgi-owner-phases owner))))
              (when (and saved (equal "overrun" (gethash "outcome" saved)))
                (%cg-authority-fail "IDENTITY_CHARGE_OVERRUN"))
              (when (and saved (not (equal digest (gethash "request_digest" saved))))
                (%cg-authority-fail "IDENTITY_RECEIPT_DIGEST_MISMATCH"))
              (if (and saved (equal "response" (gethash "outcome" saved)))
                  (%cg-detach (gethash "response" saved))
                  (throw 'identity-next (%cg-object "status" "request" "phase" phase "spec" spec "request_digest" digest
                                                    "pending" (if (and saved (equal "request" (gethash "outcome" saved))) :true :false)))))))
        (%cg-object "status" "complete" "result"
          (if (%cgi-owner-formation-p owner)
              (let ((*cgf-protocol* (%cgi-owner-formation-protocol owner record))
                    (*cgt-fact-input-revision*
                      (or (gethash "fact_input_revision" record)
                          "full-candidates-v1")))
                (%cgf-generate (cgi-owner-graph owner) (gethash "source_context" record) (gethash "batch_index" record)
                               (context-graph-ontology (cgi-owner-graph owner)) (cgi-owner-revision owner) #'receipt
                               :descriptor-guide (%cgi-owner-formation-guide owner record)))
              (%cgi-run (gethash "contexts" plan) (gethash "mentions" plan) #'receipt
                        :max-calls (1+ (length (gethash "pages" plan))))))))))

(defun %cgi-owner-opening-attempt (owner opened)
  (let ((record (%cgro-record (gethash opened (cgi-owner-opens owner)))))
    (if (%cgi-owner-retry-generation-p owner) (gethash "attempt" record) 1)))

(defun %cgi-owner-terminal-record (owner opened)
  (let ((event (gethash opened (cgi-owner-terminals owner))))
    (and event (%cgro-record event))))

(defun %cgi-owner-retryable-terminal-p (owner opened &optional now)
  (let ((record (%cgi-owner-terminal-record owner opened)))
    (and record (eq :true (gethash "retryable" record))
         (or (null now) (<= (gethash "next_retry_at" record) now)))))

(defun %cgi-owner-version-repairable-terminal-p (owner opened)
  "True only for one exhausted V14 task sealed before the current request-shape
revision.  This does not extend retries for an unchanged request shape."
  (let* ((opening-event (gethash opened (cgi-owner-opens owner)))
         (opening (and opening-event (%cgro-record opening-event)))
         (terminal (%cgi-owner-terminal-record owner opened)))
    (and opening terminal
         (equal "identity-formation-owner-v9" (cgi-owner-protocol owner))
         (equal "identity-formation-v14"
                (%cgi-owner-formation-protocol owner opening))
         (member (gethash "fact_input_revision" opening)
                 '("selected-entities-v2" "selected-signatures-v3")
                 :test #'equal)
         (eql +cgi-owner-maximum-attempts+ (gethash "attempt" terminal))
         (eq :false (gethash "retryable" terminal))
         (eq :null (gethash "next_retry_at" terminal)))))

(defun %cgi-owner-version-repair-opening-record-p (owner record)
  (let ((prior (gethash "retry_of" record)))
    (and (eql +cgi-owner-version-repair-attempt+
              (gethash "attempt" record))
         (integerp prior)
         (equal "selected-signatures-v4"
                (gethash "fact_input_revision" record))
         (%cgi-owner-version-repairable-terminal-p owner prior)
         (>= (gethash "observed_at" record)
             (gethash "failed_at" (%cgi-owner-terminal-record owner prior))))))

(defun %cgi-owner-version-repair-opening-p (owner opened)
  (let ((event (gethash opened (cgi-owner-opens owner))))
    (and event
         (%cgi-owner-version-repair-opening-record-p
          owner (%cgro-record event)))))

(defun %cgi-owner-consume (owner event source-fn)
  "Fold trusted events. Comparison v1 is unreviewed selection; the separately
configured formation v1 reconstructs and applies shared admission on completion."
  (let* ((id (gethash "id" event)) (type (gethash "type" event)) (payload (gethash "payload" event)))
    (unless (and (integerp id) (> id (cgi-owner-last-id owner))) (%cg-authority-fail "IDENTITY_EVENT_ORDER_INVALID"))
    (when (and (equal (cgi-owner-agent-id owner) (gethash "agent_id" event))
               (hash-table-p payload) (equal (cgi-owner-persona-id owner) (gethash "persona_id" payload))
               (member (cgi-owner-protocol owner)
                       '("identity-owner-v1" "identity-formation-owner-v1"
                         "identity-formation-owner-v2" "identity-formation-owner-v3"
                         "identity-formation-owner-v4" "identity-formation-owner-v5"
                          "identity-formation-owner-v6"
                          "identity-formation-owner-v7"
                          "identity-formation-owner-v8"
                          "identity-formation-owner-v9") :test #'equal)
               (equal (cgi-owner-protocol owner) (gethash "generation" payload)))
      (let* ((record (%cgro-record event)) (parent (gethash "caused_by" event))
             (opened (gethash parent (cgi-owner-opens owner))))
        (cond
          ((equal type "context-graph-identity-opened")
           (unless (and (%cg-closed-keys-p record
                         (if (%cgi-owner-retry-generation-p owner)
                             (if (nth-value 1 (gethash "fact_input_revision" record))
                                 '("episode_event_id" "batch_index" "observed_at" "source_context" "ontology_revision"
                                   "formation_protocol" "descriptor_guide" "fact_input_revision"
                                   "budget_microusd" "request_ceiling_microusd" "attempt" "retry_of")
                                 '("episode_event_id" "batch_index" "observed_at" "source_context" "ontology_revision"
                                   "formation_protocol" "descriptor_guide" "budget_microusd" "request_ceiling_microusd"
                                   "attempt" "retry_of"))
                             (if (%cgi-owner-versioned-formation-p owner)
                             '("episode_event_id" "batch_index" "observed_at" "source_context" "ontology_revision"
                               "formation_protocol" "descriptor_guide" "budget_microusd" "request_ceiling_microusd")
                             (if (%cgi-owner-formation-p owner)
                                 '("episode_event_id" "batch_index" "observed_at" "source_context" "ontology_revision" "budget_microusd" "request_ceiling_microusd")
                                 '("episode_event_id" "batch_index" "observed_at" "mentions" "discovery" "budget_microusd" "request_ceiling_microusd")))))
                        (integerp parent) (plusp parent) (< parent id) (equal parent (gethash "episode_event_id" record))
                        (integerp (gethash "observed_at" record)) (<= 0 (gethash "observed_at" record))
                        (or (not (nth-value 1 (gethash "fact_input_revision" record)))
                            (and (equal "identity-formation-v14" (gethash "formation_protocol" record))
                                 (member (gethash "fact_input_revision" record)
                                         '("selected-entities-v2" "selected-signatures-v3"
                                           "selected-signatures-v4")
                                         :test #'equal)))
                        (integerp (gethash "budget_microusd" record)) (plusp (gethash "budget_microusd" record))
                        (integerp (gethash "request_ceiling_microusd" record)) (plusp (gethash "request_ceiling_microusd" record)))
             (%cg-authority-fail "IDENTITY_OPEN_INVALID"))
           (let* ((full (funcall source-fn (cgi-owner-graph owner) parent (gethash "observed_at" record)))
                  (discovery (unless (%cgi-owner-formation-p owner)
                               (%cgi-discover (cgi-owner-graph owner) full (gethash "batch_index" record) (gethash "mentions" record))))
                  (calls (if (%cgi-owner-formation-p owner)
                             (%cgi-owner-formation-call-limit owner record)
                             (1+ (length (gethash "pages" (gethash "plan" discovery)))))))
             (unless (and (equal (cgi-owner-agent-id owner) (gethash "agent_id" full))
                          (equal (cgi-owner-persona-id owner) (gethash "persona_id" full))
                          (if (%cgi-owner-formation-p owner)
                              (and (%cg-authority-string-p (cgi-owner-revision owner) 120)
                                   (equal (cgi-owner-revision owner) (gethash "ontology_revision" record))
                                   (%cg-authority-equal-p full (gethash "source_context" record))
                                   (%cgro-batch-context (cgi-owner-graph owner) full (gethash "batch_index" record) "staged" "bounded-v3")
                                   (or (not (%cgi-owner-versioned-formation-p owner))
                                       (let ((*cgf-protocol* (%cgi-owner-formation-protocol owner record)))
                                         (and (member *cgf-protocol*
                                                      '("identity-formation-v1" "identity-formation-v2"
                                                        "identity-formation-v3" "identity-formation-v4"
                                                        "identity-formation-v5" "identity-formation-v6"
                                                        "identity-formation-v7" "identity-formation-v8"
                                                        "identity-formation-v9"
                                                        "identity-formation-v10"
                                                         "identity-formation-v11"
                                                         "identity-formation-v12"
                                                         "identity-formation-v13"
                                                         "identity-formation-v14")
                                                      :test #'equal)
                                              (%cgi-owner-formation-pair-valid-p
                                               owner *cgf-protocol*)
                                              (if (member *cgf-protocol*
                                                          '("identity-formation-v4" "identity-formation-v5"
                                                            "identity-formation-v6" "identity-formation-v7"
                                                            "identity-formation-v8" "identity-formation-v9"
                                                            "identity-formation-v10" "identity-formation-v11"
                                                            "identity-formation-v12" "identity-formation-v13"
                                                            "identity-formation-v14") :test #'equal)
                                                  (%cgf-validate-descriptor-guide
                                                   (gethash "descriptor_guide" record)
                                                   (context-graph-ontology (cgi-owner-graph owner)))
                                                  (eq :null (gethash "descriptor_guide" record)))))))
                              (%cg-authority-equal-p discovery (gethash "discovery" record)))
                          (if (%cgi-owner-versioned-formation-p owner)
                              (<= (gethash "request_ceiling_microusd" record)
                                  (gethash "budget_microusd" record))
                              (<= (* calls (gethash "request_ceiling_microusd" record))
                                  (gethash "budget_microusd" record))))
               (%cg-authority-fail "IDENTITY_OPEN_INVALID")))
           (let* ((task-key (list parent (gethash "batch_index" record)))
                  (prior (gethash task-key (cgi-owner-tasks owner)))
                  (attempt (and (%cgi-owner-retry-generation-p owner) (gethash "attempt" record)))
                  (retry-of (and (%cgi-owner-retry-generation-p owner) (gethash "retry_of" record))))
             (when (or
                    (loop for prior-open being the hash-keys of (cgi-owner-opens owner)
                          thereis (not (gethash prior-open (cgi-owner-terminals owner))))
                    (if (%cgi-owner-retry-generation-p owner)
                        (if prior
                            (not (or
                                  (and (integerp attempt)
                                       (<= 2 attempt +cgi-owner-maximum-attempts+)
                                       (= attempt (1+ (%cgi-owner-opening-attempt owner prior)))
                                       (eql retry-of prior)
                                       (%cgi-owner-retryable-terminal-p
                                        owner prior (gethash "observed_at" record)))
                                  (%cgi-owner-version-repair-opening-record-p
                                   owner record)))
                            (not (and (eql attempt 1) (eq retry-of :null))))
                        prior))
             (%cg-authority-fail "IDENTITY_CONCURRENT_OR_REPEATED_TASK"))
             (setf (gethash id (cgi-owner-opens owner)) (%cg-detach event)
                   (gethash task-key (cgi-owner-tasks owner)) id)))
          ((member type '("context-graph-identity-phase" "context-graph-identity-completed" "context-graph-identity-failed") :test #'equal)
           (unless (and opened (not (gethash parent (cgi-owner-terminals owner)))) (%cg-authority-fail "IDENTITY_TERMINAL_ORDER_INVALID"))
           (cond
             ((equal type "context-graph-identity-phase")
              (let* ((key (list parent (gethash "phase" record))) (prior (gethash key (cgi-owner-phases owner)))
                     (outcome (gethash "outcome" record)) (next (%cgi-owner-next owner parent)))
                (unless (and (equal "request" (gethash "status" next))
                             (equal (gethash "phase" record) (gethash "phase" next))
                             (equal (gethash "request_digest" record) (gethash "request_digest" next)))
                  (%cg-authority-fail "IDENTITY_PHASE_ORDER_INVALID"))
                (if (equal outcome "request")
                    (unless (and (%cgi-owner-fresh-p owner parent)
                                 (%cg-closed-keys-p record '("phase" "request_digest" "outcome" "reserved_microusd"))
                                 (eq :false (gethash "pending" next))
                                 (integerp (gethash "reserved_microusd" record))
                                 (if (%cgi-owner-versioned-formation-p owner)
                                     (<= 1 (gethash "reserved_microusd" record)
                                         (gethash "request_ceiling_microusd" (%cgro-record opened)))
                                     (eql (gethash "reserved_microusd" record)
                                          (gethash "request_ceiling_microusd" (%cgro-record opened))))
                                 (<= (+ (%cgi-owner-exposure owner parent) (gethash "reserved_microusd" record))
                                     (gethash "budget_microusd" (%cgro-record opened))))
                      (%cg-authority-fail "IDENTITY_RESERVATION_INVALID"))
                    (unless (and (%cg-closed-keys-p record '("phase" "request_digest" "outcome" "response" "charged_microusd"))
                                 (eq :true (gethash "pending" next))
                                 (integerp (gethash "charged_microusd" record))
                                 (<= 0 (gethash "charged_microusd" record))
                                 (or (and (equal outcome "response") (hash-table-p (gethash "response" record))
                                          (<= (gethash "charged_microusd" record) (gethash "reserved_microusd" prior)))
                                     (and (equal outcome "paused") (eq :null (gethash "response" record))
                                          (zerop (gethash "charged_microusd" record)))
                                     (and (equal outcome "rejected") (eq :null (gethash "response" record))
                                          (<= (gethash "charged_microusd" record)
                                              (gethash "reserved_microusd" prior)))
                                     (and (equal outcome "overrun") (eq :null (gethash "response" record))
                                          (> (gethash "charged_microusd" record) (gethash "reserved_microusd" prior)))))
                      (%cg-authority-fail "IDENTITY_RESPONSE_INVALID")))
                (setf (gethash key (cgi-owner-phases owner)) (%cg-detach record))))
             ((equal type "context-graph-identity-completed")
              (let ((next (%cgi-owner-next owner parent)))
                (unless (and (%cgi-owner-fresh-p owner parent)
                             (%cg-closed-keys-p record '("result")) (equal "complete" (gethash "status" next))
                             (%cg-authority-equal-p (gethash "result" record) (gethash "result" next)))
                  (%cg-authority-fail "IDENTITY_COMPLETION_BINDING_INVALID")))
              (when (%cgi-owner-formation-p owner)
                (let* ((opening (%cgro-record opened)) (envelope (gethash "result" record))
                       (boundary (%cg-object "episode_id" (if (equal "empty" (gethash "status" envelope))
                                                             (gethash "episode_id" (gethash "source_context" opening))
                                                             (gethash "episode_id" (gethash "context" envelope)))
                                             "opened_boundary_id" parent "application_event_id" id "observed_at" (gethash "observed_at" opening))))
                  (setf (gethash parent (cgi-owner-applications owner))
                        (%cgf-apply (cgi-owner-graph owner) boundary (gethash "source_context" opening) envelope
                                    :descriptor-guide (%cgi-owner-formation-guide owner opening)))))
              (setf (gethash parent (cgi-owner-terminals owner)) (%cg-detach event)))
             (t (unless (and (%cg-closed-keys-p record
                                   (if (%cgi-owner-retry-generation-p owner)
                                       '("reason" "failure_class" "retryable" "attempt" "failed_at" "next_retry_at")
                                       '("reason")))
                             (%cg-authority-string-p (gethash "reason" record) 120)
                             (or (not (%cgi-owner-retry-generation-p owner))
                                 (and (%cg-authority-string-p (gethash "failure_class" record) 120)
                                      (member (gethash "retryable" record) '(:true :false))
                                      (eql (gethash "attempt" record) (%cgi-owner-opening-attempt owner parent))
                                      (integerp (gethash "attempt" record))
                                      (or (<= 1 (gethash "attempt" record)
                                              +cgi-owner-maximum-attempts+)
                                          (and (eql +cgi-owner-version-repair-attempt+
                                                    (gethash "attempt" record))
                                               (%cgi-owner-version-repair-opening-p
                                                owner parent)))
                                      (integerp (gethash "failed_at" record))
                                      (<= 0 (gethash "failed_at" record))
                                      (if (eq :true (gethash "retryable" record))
                                          (and (< (gethash "attempt" record) +cgi-owner-maximum-attempts+)
                                               (eql (gethash "next_retry_at" record)
                                                    (+ (gethash "failed_at" record)
                                                       (aref +cgi-owner-retry-delays-seconds+
                                                             (1- (gethash "attempt" record))))))
                                          (eq :null (gethash "next_retry_at" record))))))
                  (%cg-authority-fail "IDENTITY_FAILURE_INVALID"))
                (setf (gethash parent (cgi-owner-terminals owner)) (%cg-detach event))))))))
    (setf (cgi-owner-last-id owner) id))
  owner)

(defun %cgi-owner-append (owner source-fn append-fn type record cause)
  (let ((event (funcall append-fn type (%cg-object "persona_id" (cgi-owner-persona-id owner)
                         "generation" (cgi-owner-protocol owner) "record_json" (context-graph-runtime-json record)) cause)))
    (%cgi-owner-consume owner event source-fn) event))

(defun %cgi-owner-open (owner source-fn append-fn episode batch mentions budget ceiling now)
  "Caller authorizes budget and supplies a conservative per-request charge
ceiling. Neither a provider price lookup nor a paid transport is installed."
  (when (%cgi-owner-formation-p owner) (%cg-authority-fail "IDENTITY_OWNER_PROTOCOL_INVALID"))
  (let* ((full (funcall source-fn (cgi-owner-graph owner) episode now))
         (record (%cg-object "episode_event_id" episode "batch_index" batch "observed_at" now
                             "mentions" (%cg-detach mentions) "discovery" (%cgi-discover (cgi-owner-graph owner) full batch mentions)
                             "budget_microusd" budget "request_ceiling_microusd" ceiling)))
    (%cgi-owner-seal-open owner source-fn append-fn record)))

(defun %cgi-owner-seal-open (owner source-fn append-fn record)
  (let* ((episode (gethash "episode_event_id" record))
         ;; Validate before the first durable append, using an inert owner copy.
         (probe (copy-cgi-owner owner))
         (event (%cg-object "id" (1+ (max episode (cgi-owner-last-id owner))) "type" "context-graph-identity-opened"
                            "agent_id" (cgi-owner-agent-id owner) "caused_by" episode
                            "payload" (%cg-object "persona_id" (cgi-owner-persona-id owner) "generation" (cgi-owner-protocol owner)
                                                  "record_json" (context-graph-runtime-json record)))))
    (setf (cgi-owner-opens probe) (%cg-detach (cgi-owner-opens owner))
          (cgi-owner-terminals probe) (%cg-detach (cgi-owner-terminals owner))
          (cgi-owner-tasks probe) (%cg-detach (cgi-owner-tasks owner)))
    (%cgi-owner-consume probe event source-fn)
    (%cgi-owner-append owner source-fn append-fn "context-graph-identity-opened" record episode)))

(defun %cgi-owner-run (owner source-fn append-fn call-fn opened
                       &optional reservation-fn (now-fn #'get-universal-time))
  "CALL-FN(phase,spec,digest,opened,ceiling) returns response and actual charge
in integer micro-USD. Pauses are allowed only before spending (charge zero).
V6 makes an unapplied task retryable as a new attempt after a durable delay;
the outcome-unknown reservation remains charged to conservative exposure."
  (when (gethash opened (cgi-owner-terminals owner)) (%cg-authority-fail "IDENTITY_TASK_TERMINAL"))
  (let* ((event (gethash opened (cgi-owner-opens owner))) (record (and event (%cgro-record event))))
    (unless record (%cg-authority-fail "IDENTITY_OPEN_MISSING"))
    (labels ((fail (reason &optional retryable failure-class)
               (let* ((attempt (%cgi-owner-opening-attempt owner opened))
                      (failed-at (funcall now-fn))
                      (will-retry (and (%cgi-owner-retry-generation-p owner) retryable
                                       (< attempt +cgi-owner-maximum-attempts+)))
                      (record (if (%cgi-owner-retry-generation-p owner)
                                  (%cg-object "reason" reason
                                              "failure_class" (or failure-class "non-retryable")
                                              "retryable" (if will-retry :true :false)
                                              "attempt" attempt "failed_at" failed-at
                                              "next_retry_at"
                                              (if will-retry
                                                  (+ failed-at (aref +cgi-owner-retry-delays-seconds+
                                                                   (1- attempt)))
                                                  :null))
                                  (%cg-object "reason" reason))))
                 (%cgi-owner-append owner source-fn append-fn "context-graph-identity-failed" record opened))
               (return-from %cgi-owner-run (%cg-object "status" "failed" "reason" reason))))
      (loop
        (unless (%cgi-owner-fresh-p owner opened) (fail "IDENTITY_GRAPH_CHANGED"))
        (let ((next
                (handler-case
                    (%cgi-owner-next owner opened)
                  (context-graph-authority-input-error (condition)
                    (let ((code (%cg-authority-error-code condition)))
                      (if (%cgi-owner-retryable-model-output-error-p
                           owner record code)
                          (fail code t "model-output-invalid")
                          (fail code)))))))
          (when (equal "complete" (gethash "status" next))
            (%cgi-owner-append owner source-fn append-fn "context-graph-identity-completed" (%cg-object "result" (gethash "result" next)) opened)
            (return next))
          (when (eq :true (gethash "pending" next))
            (fail "IDENTITY_CALL_OUTCOME_AMBIGUOUS" t "provider-outcome-ambiguous"))
          (let* ((phase (gethash "phase" next)) (digest (gethash "request_digest" next))
                 (maximum (gethash "request_ceiling_microusd" record))
                 (ceiling (if reservation-fn
                              (handler-case
                                  (funcall reservation-fn phase (%cg-detach (gethash "spec" next))
                                           digest opened maximum)
                                (context-graph-reservation-failure (condition)
                                  (fail "IDENTITY_RESERVATION_FAILED" t
                                        (context-graph-reservation-failure-classification
                                         condition))))
                              maximum)))
            (unless (and (integerp ceiling) (<= 1 ceiling maximum))
              (fail "IDENTITY_RESERVATION_INVALID"))
            (%cgi-owner-append owner source-fn append-fn "context-graph-identity-phase"
              (%cg-object "phase" phase "request_digest" digest "outcome" "request" "reserved_microusd" ceiling) opened)
            (multiple-value-bind (response charge)
                (handler-case (funcall call-fn phase (%cg-detach (gethash "spec" next)) digest opened ceiling)
                  (context-graph-call-failure (condition)
                    (let ((known-charge (context-graph-call-failure-charged-microusd condition)))
                      (when (integerp known-charge)
                        (%cgi-owner-append owner source-fn append-fn "context-graph-identity-phase"
                          (%cg-object "phase" phase "request_digest" digest "outcome" "rejected"
                                      "response" :null "charged_microusd" known-charge) opened)))
                    (fail "IDENTITY_CALL_FAILED"
                          (context-graph-call-failure-retryable-p condition)
                          (context-graph-call-failure-classification condition)))
                  (error () (fail "IDENTITY_CALL_OUTCOME_AMBIGUOUS" t
                                  "provider-outcome-ambiguous")))
              ;; A broken price/transport contract cannot be undone, but its
              ;; reported charge must not disappear behind a smaller reservation.
              (when (and (integerp charge) (> charge ceiling))
                (%cgi-owner-append owner source-fn append-fn "context-graph-identity-phase"
                  (%cg-object "phase" phase "request_digest" digest "outcome" "overrun" "response" :null "charged_microusd" charge) opened)
                (fail "IDENTITY_CHARGE_OVERRUN"))
              (unless (and (integerp charge) (<= 0 charge ceiling)
                           (or (hash-table-p response) (and (member response '(:preempted :paused-budget)) (zerop charge))))
                (fail "IDENTITY_CALL_OUTCOME_AMBIGUOUS"))
              (let ((paused (member response '(:preempted :paused-budget))))
                (%cgi-owner-append owner source-fn append-fn "context-graph-identity-phase"
                  (%cg-object "phase" phase "request_digest" digest "outcome" (if paused "paused" "response")
                              "response" (if paused :null response) "charged_microusd" charge) opened)
                (when paused (return (%cg-object "status" (if (eq response :preempted) "preempted" "paused-budget"))))))))))))
