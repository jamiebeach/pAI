;;;; harness: bare
(load (merge-pathnames "context-graph-identity-formation-tests.lisp" *load-truename*))
(in-package :pai.context-graph)

(let* ((plan (%cg-object
              "mentions"
              (vector (%cg-object "mention" "mention_1")
                      (%cg-object "mention" "mention_2")
                      (%cg-object "mention" "mention_3"))))
       (collision (%cg-object
                   "resolutions"
                   (vector (%cg-object "mention" "mention_1"
                                       "candidate" "candidate_1")
                           (%cg-object "mention" "mention_1"
                                       "candidate" :null)
                           (%cg-object "mention" "mention_2"
                                       "candidate" "candidate_2"))))
       (normalized (%cgi-normalize-resolution-collision plan collision))
       (rows (gethash "resolutions" normalized))
       (valid (%cg-object
               "resolutions"
               (vector (%cg-object "mention" "mention_1" "candidate" :null)
                       (%cg-object "mention" "mention_2" "candidate" :null)
                       (%cg-object "mention" "mention_3" "candidate" :null))))
       (wrong-length (%cg-object
                      "resolutions"
                      (vector (%cg-object "mention" "mention_1"
                                          "candidate" :null)))))
  ;; A duplicate and its missing counterpart both become unresolved; the
  ;; uniquely supplied row is preserved.  Closed valid and unrelated invalid
  ;; shapes are not rewritten.
  (assert (= 3 (length rows)))
  (assert (eq :null (gethash "candidate" (aref rows 0))))
  (assert (equal "candidate_2" (gethash "candidate" (aref rows 1))))
  (assert (eq :null (gethash "candidate" (aref rows 2))))
  (assert (%cg-authority-equal-p
           valid (%cgi-normalize-resolution-collision plan valid)))
  (assert (%cg-authority-equal-p
           wrong-length
           (%cgi-normalize-resolution-collision plan wrong-length))))

(let* ((ontology (gethash "ontology" (as-fixture))) (revision "personal-context-core-glm53-v1.2")
       (graph (make-context-graph ontology)) (owner (%cgf-owner-create graph "lab-agent" "lab-persona" revision))
       (episodes (vector (sm-episode "formation-owner-first" "I own a cat named Mina." 100)
                         (sm-episode "formation-owner-next" "I own a cat named Mina." 200)))
       (events nil) (sequence 100) (calls 0) (pause t))
  (labels ((source (g id now) (declare (ignore now)) (lab-authority-context g (aref episodes (1- id)) (1- id)))
           (append-event (type payload cause)
             (let ((e (context-graph-runtime-read-json (context-graph-runtime-json
                        (%cg-object "id" (incf sequence) "agent_id" "lab-agent" "type" type "payload" payload "caused_by" cause)))))
               (push e events) e))
           (model (phase spec digest opened ceiling)
             (declare (ignore digest)) (assert (= 5 ceiling))
             (when (and pause (equal phase "facts")) (setf pause nil) (return-from model (values :paused-budget 0)))
             (incf calls)
             (values (gf-model phase spec :reuse (= 2 (gethash "episode_event_id" (%cgro-record (gethash opened (cgi-owner-opens owner)))))) 2)))
    (let ((opened (gethash "id" (%cgf-owner-open owner #'source #'append-event 1 0 65 5 100))))
      (assert (equal "paused-budget" (gethash "status" (%cgi-owner-run owner #'source #'append-event #'model opened))))
      (assert (= 4 calls))
      (assert (zerop (context-graph-entity-count graph)))
      ;; Restart both projection and owner, then replay durable JSON receipts.
      (setf graph (make-context-graph ontology) owner (%cgf-owner-create graph "lab-agent" "lab-persona" revision))
      (dolist (event (reverse events)) (%cgi-owner-consume owner event #'source))
      (assert (equal "complete" (gethash "status" (%cgi-owner-run owner #'source #'append-event #'model opened))))
      (assert (= 6 calls))
      (assert (= 12 (%cgi-owner-exposure owner opened)))
      (assert (equal "accepted" (gethash "status" (gethash opened (cgi-owner-applications owner)))))
      (assert (plusp (length (gethash "facts" (gethash "context" (%cg-authority-retrieve graph "lab-agent" "lab-persona" "Mina cat")))))))
    (let ((count (context-graph-entity-count graph))
          (opened (gethash "id" (%cgf-owner-open owner #'source #'append-event 2 0 65 5 200))))
      (assert (equal "complete" (gethash "status" (%cgi-owner-run owner #'source #'append-event #'model opened))))
      (assert (= 11 calls))
      (assert (= 10 (%cgi-owner-exposure owner opened)))
      (assert (= count (context-graph-entity-count graph))))
    (let* ((expected (%cg-authority-watermark graph "lab-agent" "lab-persona"))
           (fresh-graph (make-context-graph ontology)) (fresh (%cgf-owner-create fresh-graph "lab-agent" "lab-persona" revision)))
      (dolist (event (reverse events)) (%cgi-owner-consume fresh event #'source))
      (assert (= 2 (hash-table-count (cgi-owner-applications fresh))))
      (assert (%cg-authority-equal-p expected (%cg-authority-watermark fresh-graph "lab-agent" "lab-persona")))
      (assert (= 11 calls))
      (assert (zerop (length (gethash "facts" (gethash "context" (%cg-authority-retrieve fresh-graph "lab-agent" "lab-persona" "asteroid")))))))
    ;; New protocol source context cannot be forged at task opening.
    (let* ((fresh (%cgf-owner-create (make-context-graph ontology) "lab-agent" "lab-persona" revision))
           (bad (%cg-detach (first (reverse events)))) (record (%cgro-record bad)))
      (setf (gethash "access_snapshot_digest" (gethash "source_context" record)) (%cg-sha256 "forged")
            (gethash "record_json" (gethash "payload" bad)) (context-graph-runtime-json record))
      (assert (sm-error (lambda () (%cgi-owner-consume fresh bad #'source)) "IDENTITY_OPEN_INVALID")))))

(defun gf-owner-v6-model (phase spec)
  (let ((response
          (if (equal phase "new-identity-groups")
              (%cg-object "groups" (vector (%cg-object "mentions" #("mention_1")
                                                        "source" "source_1" "quote" "Mina")))
              (gf-model phase spec))))
    (when (equal phase "review")
      (let ((rows (make-hash-table :test #'equal)))
        (loop for row across (gethash "claim_reviews" response) do
          (let ((copy (%cg-detach row)) (ref (gethash "claim_ref" row)))
            (remhash "claim_ref" copy)
            (when (search "entity:" ref)
              (remhash "source_reading" copy)
              (dolist (key (rest +cgq-checks+))
                (remhash key (gethash "quality_checks" copy)))
              (setf (gethash "canonical_label" (gethash "quality_checks" copy)) "supported"))
            (setf (gethash ref rows) copy)))
        (setf (gethash "claim_reviews" response) rows)))
    response))

(let* ((ontology (gethash "ontology" (as-fixture))) (revision "personal-context-core-glm53-v1.2")
       (guide (map 'vector (lambda (name)
                            (%cg-object "name" name "definition" "Fixture meaning."
                                        "inclusion_rule" "Source-supported."
                                        "exclusion_rule" "Other kinds."))
                   (gethash "entity_types" ontology)))
       (episode (sm-episode "versioned-formation-owner" "I own a cat named Mina." 100))
       (graph (make-context-graph ontology))
       (owner (%cgf-owner-create-v2 graph "lab-agent" "lab-persona" revision))
       (events nil) (sequence 200) (calls 0))
  (labels ((source (g id now)
             (declare (ignore id now))
             (lab-authority-context g episode 0))
           (append-event (type payload cause)
             (let ((event (%cg-object "id" (incf sequence) "agent_id" "lab-agent"
                                      "type" type "payload" payload "caused_by" cause)))
               (push event events) event))
           (model (phase spec digest opened ceiling)
             (declare (ignore digest opened))
             (assert (= 3 ceiling))
             (incf calls)
             (values (gf-owner-v6-model phase spec) 2))
           (reserve (phase spec digest opened maximum)
             (declare (ignore phase spec digest opened))
             (assert (= 10 maximum))
             3))
    (let* ((opened-event (%cgf-owner-open-v2 owner #'source #'append-event 1 0 100 10 100
                                             "identity-formation-v6" guide))
           (opened (gethash "id" opened-event))
           (opening (%cgro-record opened-event)))
      (assert (equal "identity-formation-v6" (gethash "formation_protocol" opening)))
      (assert (%cg-authority-equal-p guide (gethash "descriptor_guide" opening)))
      (assert (equal "complete" (gethash "status"
                                  (%cgi-owner-run owner #'source #'append-event #'model opened #'reserve))))
      (assert (= 7 calls))
      (assert (= 14 (%cgi-owner-exposure owner opened)))
      (assert (= 1 (context-graph-fact-count graph)))
      (let* ((expected (%cg-authority-watermark graph "lab-agent" "lab-persona"))
             (fresh-graph (make-context-graph ontology))
             (fresh (%cgf-owner-create-v2 fresh-graph "lab-agent" "lab-persona" revision)))
        (dolist (event (reverse events)) (%cgi-owner-consume fresh event #'source))
        (assert (%cg-authority-equal-p expected
                  (%cg-authority-watermark fresh-graph "lab-agent" "lab-persona")))
        (assert (= 7 calls)))
      (let* ((ordered (reverse events)) (bad-open (%cg-detach (first ordered)))
             (record (%cgro-record bad-open))
             (fresh (%cgf-owner-create-v2 (make-context-graph ontology) "lab-agent" "lab-persona" revision)))
        (setf (gethash "formation_protocol" record) "identity-formation-v4"
              (gethash "record_json" (gethash "payload" bad-open)) (context-graph-runtime-json record))
        (%cgi-owner-consume fresh bad-open #'source)
        (assert (sm-error (lambda () (%cgi-owner-consume fresh (second ordered) #'source))
                          "IDENTITY_PHASE_ORDER_INVALID"))))))
(format t "FORMATION-OWNER-V2 sealed v6 policy, exact reservations, cold replay and protocol-drift refusal passed~%")

(let* ((ontology (gethash "ontology" (as-fixture)))
       (owner (%cgf-owner-create-v3 (make-context-graph ontology)
                                    "lab-agent" "lab-persona"
                                    "personal-context-core-glm53-v1.2")))
  (assert (equal "identity-formation-owner-v3" (cgi-owner-protocol owner)))
  (assert (%cgi-owner-formation-p owner))
  (assert (%cgi-owner-versioned-formation-p owner)))
(format t "FORMATION-OWNER-V3 isolates replay-visible replacement generation passed~%")

(let* ((ontology (gethash "ontology" (as-fixture)))
       (owner (%cgf-owner-create-v4 (make-context-graph ontology)
                                    "lab-agent" "lab-persona"
                                    "personal-context-core-glm53-v1.2")))
  (assert (equal "identity-formation-owner-v4" (cgi-owner-protocol owner)))
  (assert (%cgi-owner-formation-p owner))
  (assert (%cgi-owner-versioned-formation-p owner)))
(format t "FORMATION-OWNER-V4 isolates conservative comparison generation passed~%")

(let* ((ontology (gethash "ontology" (as-fixture)))
       (owner (%cgf-owner-create-v5 (make-context-graph ontology)
                                    "lab-agent" "lab-persona"
                                    "personal-context-core-glm53-v1.2")))
  (assert (equal "identity-formation-owner-v5" (cgi-owner-protocol owner)))
  (assert (%cgi-owner-formation-p owner))
  (assert (%cgi-owner-versioned-formation-p owner)))
(format t "FORMATION-OWNER-V5 isolates authenticated participant comparison generation passed~%")

(let* ((ontology (gethash "ontology" (as-fixture)))
       (owner (%cgf-owner-create-v7 (make-context-graph ontology)
                                    "lab-agent" "lab-persona"
                                    "personal-context-core-glm53-v1.2")))
  (assert (equal "identity-formation-owner-v7" (cgi-owner-protocol owner)))
  (assert (%cgi-owner-formation-p owner))
  (assert (%cgi-owner-versioned-formation-p owner))
  (assert (%cgi-owner-retry-generation-p owner))
  (assert (%cgi-owner-formation-pair-valid-p owner "identity-formation-v10"))
  (assert (not (%cgi-owner-formation-pair-valid-p owner "identity-formation-v9"))))
(format t "FORMATION-OWNER-V7 isolates the reviewed-inference replacement generation passed~%")

(let* ((ontology (gethash "ontology" (as-fixture)))
       (owner (%cgf-owner-create-v8 (make-context-graph ontology)
                                    "lab-agent" "lab-persona"
                                    "personal-context-core-glm53-v1.2")))
  (assert (equal "identity-formation-owner-v8" (cgi-owner-protocol owner)))
  (assert (%cgi-owner-formation-p owner))
  (assert (%cgi-owner-versioned-formation-p owner))
  (assert (%cgi-owner-retry-generation-p owner))
  (assert (%cgi-owner-formation-pair-valid-p owner "identity-formation-v13"))
  (assert (not (%cgi-owner-formation-pair-valid-p owner "identity-formation-v10"))))
(format t "FORMATION-OWNER-V8 isolates the designated durable-review generation passed~%")

(let* ((ontology (gethash "ontology" (as-fixture)))
       (revision "personal-context-core-glm53-v1.3")
       (owner (%cgf-owner-create-v9
               (make-context-graph ontology) "lab-agent" "lab-persona"
               revision)))
  (assert (equal "identity-formation-owner-v9" (cgi-owner-protocol owner)))
  (assert (%cgi-owner-formation-p owner))
  (assert (%cgi-owner-versioned-formation-p owner))
  (assert (%cgi-owner-retry-generation-p owner))
  (assert (%cgi-owner-formation-pair-valid-p owner "identity-formation-v14"))
  (assert (not (%cgi-owner-formation-pair-valid-p
                owner "identity-formation-v13"))))
(format t "FORMATION-OWNER-V9 isolates the typed-family generation passed~%")

(let* ((ontology (gethash "ontology" (as-fixture)))
       (revision "personal-context-core-glm53-v1.2")
       (guide (map 'vector (lambda (name)
                            (%cg-object "name" name "definition" "Fixture meaning."
                                        "inclusion_rule" "Source-supported."
                                        "exclusion_rule" "Other kinds."))
                   (gethash "entity_types" ontology)))
       (episode (sm-episode "participant-owner-v5"
                            "My name is Rowan. Hello active persona." 700))
       (graph (make-context-graph ontology))
       (owner (%cgf-owner-create-v5 graph "lab-agent" "lab-persona" revision))
       (events nil) (sequence 700) (calls 0))
  (labels ((source (g id now)
             (declare (ignore id now))
             (lab-authority-context g episode 0))
           (append-event (type payload cause)
             (let ((event (%cg-object "id" (incf sequence) "agent_id" "lab-agent"
                                      "type" type "payload" payload "caused_by" cause)))
               (push event events) event))
           (model (phase spec digest opened ceiling)
             (declare (ignore digest opened))
             (assert (= 3 ceiling))
             (incf calls)
             (values
               (cond
                 ((equal phase "mentions")
                  (%cg-object "mentions"
                    (vector (%cg-object "source" "source_1" "quote" "My name is Rowan")
                            (%cg-object "source" "source_1" "quote" "Hello active persona"))))
                 ((equal phase "identity-page-1")
                  (let ((cards (gethash "candidates" (gethash "input" spec))))
                    (assert (equal "operator" (gethash "participant_role" (aref cards 0))))
                    (assert (equal "active-persona" (gethash "participant_role" (aref cards 1))))
                    (%cg-object "mentions"
                      (vector (%cg-object "mention" "mention_1" "status" "possible" "candidates" #("candidate_1"))
                              (%cg-object "mention" "mention_2" "status" "possible" "candidates" #("candidate_2"))))))
                 ((equal phase "identity-resolve")
                  (%cg-object "resolutions"
                    (vector (%cg-object "mention" "mention_1" "candidate" "candidate_1")
                            (%cg-object "mention" "mention_2" "candidate" "candidate_2"))))
                 ((equal phase "facts")
                  (let ((bindings (gethash "mention_bindings" (gethash "input" spec))))
                    (assert (equal "operator" (gethash "entity" (aref bindings 0))))
                    (assert (equal "active_persona" (gethash "entity" (aref bindings 1))))
                    (%cg-object "facts" #() "name_corrections" #())))
                 ((equal phase "review")
                  (%cg-object "schema_version" 2 "revision_reviews" #()
                              "claim_reviews" (make-hash-table :test #'equal)))
                 (t (error "Participant references reached unexpected phase ~a" phase)))
               2))
           (reserve (phase spec digest opened maximum)
             (declare (ignore phase spec digest opened))
             (assert (= 10 maximum))
             3))
    (let* ((opened-event (%cgf-owner-open-v2 owner #'source #'append-event 1 0 100 10 700
                                             "identity-formation-v8" guide))
           (opened (gethash "id" opened-event)))
      (assert (equal "complete" (gethash "status"
                                  (%cgi-owner-run owner #'source #'append-event #'model opened #'reserve))))
      (assert (= 5 calls))
      (assert (= 10 (%cgi-owner-exposure owner opened)))
      (assert (= 2 (context-graph-entity-count graph)))
      (assert (equal '("active-persona" "operator")
                     (sort (loop for entity being the hash-values of (context-graph-entities graph)
                                 collect (gethash "participant_role" entity)) #'string<)))
      (assert (zerop (context-graph-fact-count graph)))
      (let* ((fresh-graph (make-context-graph ontology))
             (fresh (%cgf-owner-create-v5 fresh-graph "lab-agent" "lab-persona" revision)))
        (dolist (event (reverse events)) (%cgi-owner-consume fresh event #'source))
        (assert (= 1 (hash-table-count (cgi-owner-applications fresh))))
        (assert (= 2 (context-graph-entity-count fresh-graph)))
        (assert (every (lambda (entity) (not (eq :null (gethash "participant_role" entity))))
                       (loop for entity being the hash-values of (context-graph-entities fresh-graph)
                             collect entity)))
        (assert (= 5 calls))))))
(format t "FORMATION-OWNER-V5 binds self-reference and direct persona address without duplicate entities, then cold-replays passed~%")

(let* ((ontology (gethash "ontology" (as-fixture)))
       (revision "personal-context-core-glm53-v1.2")
       (guide (map 'vector (lambda (name)
                            (%cg-object "name" name "definition" "Fixture meaning."
                                        "inclusion_rule" "Source-supported."
                                        "exclusion_rule" "Other kinds."))
                   (gethash "entity_types" ontology)))
       (episode (sm-episode "retry-owner-v6" "I own a cat named Mina." 900))
       (graph (make-context-graph ontology))
       (owner (%cgf-owner-create-v6 graph "lab-agent" "lab-persona" revision))
       (events nil) (sequence 900) (calls 0) (model-error nil))
  (labels ((source (g id now)
             (declare (ignore id now))
             (lab-authority-context g episode 0))
           (append-event (type payload cause)
             (let ((event (%cg-object "id" (incf sequence) "agent_id" "lab-agent"
                                      "type" type "payload" payload "caused_by" cause)))
               (push event events) event))
           (reserve (phase spec digest opened maximum)
             (declare (ignore phase spec digest opened))
             (assert (= 10 maximum))
             3)
           (transient (phase spec digest opened ceiling)
             (declare (ignore phase spec digest opened ceiling))
             (incf calls)
             (error 'context-graph-call-failure
                    :classification "provider-transient" :retryable-p t
                    :charged-microusd 0))
           (model (phase spec digest opened ceiling)
             (declare (ignore digest opened))
             (incf calls)
             (handler-case (values (gf-owner-v6-model phase spec) (min 2 ceiling))
               (error (condition)
                 (setf model-error (format nil "~a: ~a" phase condition))
                 (error condition)))))
    (assert (equal "identity-formation-owner-v6" (cgi-owner-protocol owner)))
    (let* ((first-event (%cgf-owner-open-v2 owner #'source #'append-event 1 0 100 10 1000
                                             "identity-formation-v8" guide :attempt 1 :retry-of :null))
           (first (gethash "id" first-event)))
      (assert (equal "failed" (gethash "status"
                                (%cgi-owner-run owner #'source #'append-event #'transient first
                                                 #'reserve (lambda () 1000)))))
      (let ((failure (%cgi-owner-terminal-record owner first)))
        (assert (eq :true (gethash "retryable" failure)))
        (assert (= 1 (gethash "attempt" failure)))
        (assert (= 1030 (gethash "next_retry_at" failure))))
      (assert (zerop (%cgi-owner-exposure owner first)))
      (let ((count (length events)))
        (assert (sm-error
                  (lambda ()
                    (%cgf-owner-open-v2 owner #'source #'append-event 1 0 100 10 1029
                      "identity-formation-v8" guide :attempt 2 :retry-of first))
                  "IDENTITY_CONCURRENT_OR_REPEATED_TASK"))
        (assert (= count (length events))))
      (let* ((second-event (%cgf-owner-open-v2 owner #'source #'append-event 1 0 100 10 1030
                                                "identity-formation-v8" guide :attempt 2 :retry-of first))
             (second (gethash "id" second-event)))
        (let ((result (%cgi-owner-run owner #'source #'append-event #'model second
                                      #'reserve (lambda () 1030))))
          (unless (equal "complete" (gethash "status" result))
            (error "V6 retry fixture failed: ~a / ~a"
                   (gethash "status" result)
                   (or model-error (gethash "reason" result)))))
        (assert (= 1 (hash-table-count (cgi-owner-applications owner))))
        (assert (= 3 (context-graph-entity-count graph)))
        (assert (= 1 (context-graph-fact-count graph)))
        (assert (plusp (%cgi-owner-exposure owner second)))
        (let* ((fresh-graph (make-context-graph ontology))
               (fresh (%cgf-owner-create-v6 fresh-graph "lab-agent" "lab-persona" revision)))
          (dolist (event (reverse events)) (%cgi-owner-consume fresh event #'source))
          (assert (= 1 (hash-table-count (cgi-owner-applications fresh))))
          (assert (= 3 (context-graph-entity-count fresh-graph)))
          (assert (= 1 (context-graph-fact-count fresh-graph)))
          (assert (= second (gethash '(1 0) (cgi-owner-tasks fresh)))))))))
(format t "FORMATION-OWNER-V6 durably delays and retries an unapplied batch, preserves exposure, and cold-replays passed~%")

(let* ((ontology (gethash "ontology" (as-fixture)))
       (revision "personal-context-core-glm53-v1.2")
       (guide (map 'vector (lambda (name)
                            (%cg-object "name" name "definition" "Fixture meaning."
                                        "inclusion_rule" "Source-supported."
                                        "exclusion_rule" "Other kinds."))
                   (gethash "entity_types" ontology)))
       (episode (sm-episode "retry-exhaustion-v6" "I own a cat named Mina." 950))
       (graph (make-context-graph ontology))
       (owner (%cgf-owner-create-v6 graph "lab-agent" "lab-persona" revision))
       (events nil) (sequence 950) (prior :null) (now 2000))
  (labels ((source (g id observed) (declare (ignore id observed)) (lab-authority-context g episode 0))
           (append-event (type payload cause)
             (let ((event (%cg-object "id" (incf sequence) "agent_id" "lab-agent"
                                      "type" type "payload" payload "caused_by" cause)))
               (push event events) event))
           (reserve (phase spec digest opened maximum)
             (declare (ignore phase spec digest opened maximum)) 2)
           (fail-call (phase spec digest opened ceiling)
             (declare (ignore phase spec digest opened ceiling))
             (error 'context-graph-call-failure
                    :classification "provider-outcome-ambiguous" :retryable-p t)))
    (dotimes (zero-based-attempt 4)
      (let* ((attempt (1+ zero-based-attempt))
             (event (%cgf-owner-open-v2 owner #'source #'append-event 1 0 100 10 now
                                         "identity-formation-v8" guide
                                         :attempt attempt :retry-of prior))
             (opened (gethash "id" event)))
        (%cgi-owner-run owner #'source #'append-event #'fail-call opened #'reserve (lambda () now))
        (let ((failure (%cgi-owner-terminal-record owner opened)))
          (if (< attempt +cgi-owner-maximum-attempts+)
              (progn
                (assert (eq :true (gethash "retryable" failure)))
                (setf now (gethash "next_retry_at" failure)))
              (progn
                (assert (eq :false (gethash "retryable" failure)))
                (assert (eq :null (gethash "next_retry_at" failure)))))
          (setf prior opened))))
    (assert (= 8 (loop for opened being the hash-keys of (cgi-owner-opens owner)
                       sum (%cgi-owner-exposure owner opened))))
    (assert (zerop (hash-table-count (cgi-owner-applications owner))))
    (assert (zerop (context-graph-entity-count graph)))
    (assert (zerop (context-graph-fact-count graph)))))
(format t "FORMATION-OWNER-V6 bounds retries at four attempts and leaves exhausted work visible passed~%")

(let* ((ontology (gethash "ontology" (as-fixture)))
       (revision "personal-context-core-glm53-v1.3")
       (guide (map 'vector (lambda (name)
                            (%cg-object "name" name "definition" "Fixture meaning."
                                        "inclusion_rule" "Source-supported."
                                        "exclusion_rule" "Other kinds."))
                   (gethash "entity_types" ontology)))
       (episode (sm-episode "version-repair-v9" "I own a cat named Mina." 965))
       (owner (%cgf-owner-create-v9 (make-context-graph ontology)
                                    "lab-agent" "lab-persona" revision))
       (sequence 965) (prior :null) (now 3000))
  (labels ((source (g id observed)
             (declare (ignore id observed))
             (lab-authority-context g episode 0))
           (append-event (type payload cause)
             (%cg-object "id" (incf sequence) "agent_id" "lab-agent"
                         "type" type "payload" payload "caused_by" cause))
           (reserve (phase spec digest opened maximum)
             (declare (ignore phase spec digest opened maximum)) 2)
           (fail-call (phase spec digest opened ceiling)
             (declare (ignore phase spec digest opened ceiling))
             (error 'context-graph-call-failure
                    :classification "provider-transient" :retryable-p t)))
    (let ((*cgf-v14-new-fact-input-revision* "selected-signatures-v3"))
      (dotimes (zero-based-attempt 4)
        (let* ((attempt (1+ zero-based-attempt))
               (event (%cgf-owner-open-v2
                       owner #'source #'append-event 1 0 100 10 now
                       "identity-formation-v14" guide
                       :attempt attempt :retry-of prior))
               (opened (gethash "id" event)))
          (%cgi-owner-run owner #'source #'append-event #'fail-call opened
                           #'reserve (lambda () now))
          (let ((failure (%cgi-owner-terminal-record owner opened)))
            (when (eq :true (gethash "retryable" failure))
              (setf now (gethash "next_retry_at" failure))))
          (setf prior opened))))
    (assert (%cgi-owner-version-repairable-terminal-p owner prior))
    (let ((*cgf-v14-new-fact-input-revision* "selected-signatures-v4"))
      (let* ((repair (%cgf-owner-open-v2
                      owner #'source #'append-event 1 0 100 10 now
                      "identity-formation-v14" guide
                      :attempt +cgi-owner-version-repair-attempt+
                      :retry-of prior))
             (opened (gethash "id" repair)))
        (%cgi-owner-run owner #'source #'append-event #'fail-call opened
                         #'reserve (lambda () now))
        (assert (eq :false
                    (gethash "retryable"
                             (%cgi-owner-terminal-record owner opened))))
        (assert (not (%cgi-owner-version-repairable-terminal-p owner opened)))))))
(format t "FORMATION-OWNER-V9 permits one V4 repair of an exhausted pre-V4 task passed~%")

(let* ((ontology (gethash "ontology" (as-fixture)))
       (revision "personal-context-core-glm53-v1.2")
       (guide (map 'vector (lambda (name)
                            (%cg-object "name" name "definition" "Fixture meaning."
                                        "inclusion_rule" "Source-supported."
                                        "exclusion_rule" "Other kinds."))
                   (gethash "entity_types" ontology)))
       (episode (sm-episode "semantic-retry-v10" "I own a cat named Mina." 975))
       (graph (make-context-graph ontology))
       (owner (%cgf-owner-create-v6 graph "lab-agent" "lab-persona" revision))
       (events nil) (sequence 975))
  (labels ((source (target id observed)
             (declare (ignore id observed))
             (lab-authority-context target episode 0))
           (append-event (type payload cause)
             (let ((event (%cg-object "id" (incf sequence)
                                      "agent_id" "lab-agent" "type" type
                                      "payload" payload "caused_by" cause)))
               (push event events)
               event))
           (reserve (phase spec digest opened maximum)
             (declare (ignore phase spec digest opened maximum))
             2)
           (model (phase spec digest opened ceiling)
             (declare (ignore digest opened))
             (values
              (if (equal phase "new-identity-groups")
                  ;; Schema-valid, source-present, but not anchored to the
                  ;; representative mention.  This reproduces the historical
                  ;; semantic-envelope failure class after the paid response
                  ;; has already become durable.
                  (%cg-object
                   "groups"
                   (vector (%cg-object "mentions" #("mention_1")
                                       "source" "source_1"
                                       "quote" "I own a cat")))
                  (gf-owner-v6-model phase spec))
              (min 1 ceiling))))
    (let* ((opened-event
             (%cgf-owner-open-v2 owner #'source #'append-event 1 0 100 10 2100
                                  "identity-formation-v10" guide
                                  :attempt 1 :retry-of :null))
           (opened (gethash "id" opened-event))
           (result (%cgi-owner-run owner #'source #'append-event #'model opened
                                   #'reserve (lambda () 2100)))
           (failure (%cgi-owner-terminal-record owner opened)))
      (assert (equal "failed" (gethash "status" result)))
      (assert (equal "FORMATION_GROUP_REPRESENTATIVE_INVALID"
                     (gethash "reason" failure)))
      (assert (equal "model-output-invalid"
                     (gethash "failure_class" failure)))
      (assert (eq :true (gethash "retryable" failure)))
      (assert (= 2130 (gethash "next_retry_at" failure)))
      (assert (zerop (context-graph-entity-count graph)))
      (assert (zerop (context-graph-fact-count graph))))))
(format t "FORMATION-OWNER-V10 retries semantic model-output failures without applying partial graph state passed~%")

(let* ((ontology (gethash "ontology" (as-fixture)))
       (revision "personal-context-core-glm53-v1.2")
       (guide (map 'vector (lambda (name)
                            (%cg-object "name" name "definition" "Fixture meaning."
                                        "inclusion_rule" "Source-supported."
                                        "exclusion_rule" "Other kinds."))
                   (gethash "entity_types" ontology)))
       (episode (sm-episode "reservation-retry" "I own a cat named Mina." 990))
       (graph (make-context-graph ontology))
       (owner (%cgf-owner-create-v6 graph "lab-agent" "lab-persona" revision))
       (sequence 990))
  (labels ((source (target id observed)
             (declare (ignore id observed))
             (lab-authority-context target episode 0))
           (append-event (type payload cause)
             (%cg-object "id" (incf sequence) "agent_id" "lab-agent"
                         "type" type "payload" payload "caused_by" cause))
           (reserve (phase spec digest opened maximum)
             (declare (ignore spec digest opened))
             (error 'context-graph-reservation-failure
                    :classification "request-shape-too-large"
                    :phase phase :requested-microusd (1+ maximum)
                    :ceiling-microusd maximum))
           (model (&rest ignored)
             (declare (ignore ignored))
             (error "Provider must not be called after reservation refusal")))
    (let* ((opened-event
             (%cgf-owner-open-v2 owner #'source #'append-event 1 0 100 10 5000
                                  "identity-formation-v10" guide
                                  :attempt 1 :retry-of :null))
           (opened (gethash "id" opened-event))
           (result (%cgi-owner-run owner #'source #'append-event #'model opened
                                   #'reserve (lambda () 5000)))
           (failure (%cgi-owner-terminal-record owner opened)))
      (assert (equal "failed" (gethash "status" result)))
      (assert (equal "IDENTITY_RESERVATION_FAILED" (gethash "reason" failure)))
      (assert (equal "request-shape-too-large" (gethash "failure_class" failure)))
      (assert (eq :true (gethash "retryable" failure)))
      (assert (= 5030 (gethash "next_retry_at" failure)))
      (assert (zerop (%cgi-owner-exposure owner opened))))))
(format t "FORMATION-OWNER reservation refusal is no-send, terminal, and retryable passed~%")

(let* ((ontology (gethash "ontology" (as-fixture))) (graph (make-context-graph ontology))
       (revision "personal-context-core-glm53-v1.2") (quote "I might own a cat named Mina.")
       (episode (sm-episode "formation-hypothesis" quote 100)))
  (multiple-value-bind (full boundary) (lab-authority-context graph episode 0)
    (let ((envelope (%cgf-generate graph full 0 ontology revision
                      (lambda (phase spec digest) (declare (ignore digest))
                        (gf-model phase spec :source-reading "hypothesis" :quote quote)))))
      (setf (gethash "episode_id" boundary) (gethash "episode_id" (gethash "context" envelope)))
      (%cgf-apply graph boundary full envelope)
      (assert (zerop (context-graph-fact-count graph)))
      (assert (zerop (context-graph-entity-count graph))))))
(format t "FORMATION-OWNER durable end-to-end new/reuse, restart, cold graph replay, source tamper and unsupported factual scope refusal passed~%")

;; Personal-recall formation coverage uses the current ontology and the current
;; durable owner protocol.  Each case starts only with authenticated sealed
;; source text, runs scripted mention/identity/fact/review responses, and then
;; reconstructs a fresh graph solely by consuming the durable owner events.
(defun personal-formation-ontology-fixture ()
  (let* ((path (merge-pathnames "../config/context-graph-upper-ontology-v1.2.json"
                                *load-truename*))
         (document (with-open-file (stream path :external-format :utf-8)
                     (shasht:read-json stream)))
         (source (gethash "ontology" document)))
    (values
     (%cg-object
      "entity_types" (map 'vector (lambda (row) (gethash "name" row))
                          (gethash "entity_types" source))
      "edge_types" (map 'vector
                        (lambda (row)
                          (%cg-object "name" (gethash "name" row)
                                      "subject_types" (gethash "subject_types" row)
                                      "object_types" (gethash "object_types" row)))
                        (gethash "predicates" source)))
     (%cg-detach (gethash "entity_types" source))
     (context-graph-retrieval-lexicon source))))

(defun personal-formation-review-response (spec)
  ;; V8 keys claim reviews by their immutable references.  The controlled
  ;; reviewer still checks every descriptor field and the exact source quote.
  (let* ((response (gf-review spec))
         (rows (make-hash-table :test #'equal)))
    (loop for row across (gethash "claim_reviews" response) do
      (let ((copy (%cg-detach row))
            (reference (gethash "claim_ref" row)))
        (remhash "claim_ref" copy)
        (when (search "entity:" reference)
          (remhash "source_reading" copy)
          (dolist (key (rest +cgq-checks+))
            (remhash key (gethash "quality_checks" copy)))
          (setf (gethash "canonical_label" (gethash "quality_checks" copy))
                "supported"))
        (setf (gethash reference rows) copy)))
    (setf (gethash "claim_reviews" response) rows)
    response))

(defun personal-formation-model-response (phase spec case)
  (let* ((names (getf case :names))
         (kinds (getf case :kinds))
         (categories (getf case :categories))
         (statements (getf case :statements))
         (source-text (getf case :text))
         (predicate (getf case :predicate))
         (time (getf case :time))
         (mention-ids (loop for index from 1 to (length names)
                            collect (format nil "mention_~d" index))))
    (cond
      ((equal phase "mentions")
       (%cg-object
        "mentions"
        (map 'vector (lambda (name)
                       (%cg-object "source" "source_1" "quote" name))
             names)))
      ((uiop:string-prefix-p "identity-page-" phase)
       (%cg-object
        "mentions"
        (map 'vector (lambda (id)
                       (%cg-object "mention" id "status" "none"
                                   "candidates" #()))
             mention-ids)))
      ((equal phase "identity-resolve")
       (%cg-object
        "resolutions"
        (map 'vector (lambda (id)
                       (%cg-object "mention" id "candidate" :null))
             mention-ids)))
      ((equal phase "new-identity-groups")
       (%cg-object
        "groups"
        (map 'vector
             (lambda (id name)
               (%cg-object "mentions" (vector id) "source" "source_1"
                           "quote" name))
             mention-ids names)))
      ((equal phase "new-identities")
       (let ((eligible (gethash "eligible_mentions" (gethash "input" spec))))
         (%cg-object
          "new_entities"
          (map 'vector
               (lambda (id)
                 (let ((index (position id mention-ids :test #'equal)))
                   (assert index)
                   (%cg-object "mention" id "name" (nth index names)
                               "kind" (nth index kinds) "alternate_names" #()
                               "categories" (coerce (nth index categories)
                                                    'vector))))
               eligible))))
      ((equal phase "facts")
       (let ((bindings (gethash "mention_bindings" (gethash "input" spec))))
         (%cg-object
          "facts"
          (map 'vector
               (lambda (id statement)
                 (let ((binding (find id bindings :test #'equal
                                     :key (lambda (row)
                                            (gethash "mention" row)))))
                   (assert binding)
                   (%cg-object
                    "subject" "operator" "object" (gethash "entity" binding)
                    "predicate" predicate "statement" statement
                    "scope" "assertion" "polarity" "positive"
                    "attributed_to" "operator" "time" (%cg-detach time)
                    "evidence" (vector (%cg-object "source" "source_1"
                                                   "quote" source-text)))))
               mention-ids statements)
          "name_corrections" #())))
      ((equal phase "review")
       (personal-formation-review-response spec))
      (t (error "Personal formation fixture reached unexpected phase ~a" phase)))))

(multiple-value-bind (ontology guide lexicon)
    (personal-formation-ontology-fixture)
  (let ((cases
          (list
           (list :id "personal-pets" :text "My pets are cats named Pet-A and Pet-B."
                 :names '("Pet-A" "Pet-B") :kinds '("organism" "organism")
                 :categories '(("cat") ("cat")) :predicate "owns"
                 :statements '("The operator owns pet Pet-A."
                               "The operator owns pet Pet-B.")
                 :query "Which animals are my pets?"
                 :time (%cg-object "character" "ongoing-state" "occurred_at" :null
                                   "valid_from" :null "valid_until" :null))
           (list :id "personal-spouse" :text "My spouse is named Partner-A."
                 :names '("Partner-A") :kinds '("person")
                 :categories '(("spouse")) :predicate "related_to"
                 :statements '("Partner-A is the operator's spouse.")
                 :query "What is the name of my spouse?"
                 :time (%cg-object "character" "ongoing-state" "occurred_at" :null
                                   "valid_from" :null "valid_until" :null))
           (list :id "personal-children"
                 :text "My children are named Child-A and Child-B."
                 :names '("Child-A" "Child-B") :kinds '("person" "person")
                 :categories '(("child") ("child")) :predicate "related_to"
                 :statements '("Child-A is the operator's child."
                               "Child-B is the operator's child.")
                 :query "What are my children's names?"
                 :time (%cg-object "character" "ongoing-state" "occurred_at" :null
                                   "valid_from" :null "valid_until" :null))
           (list :id "personal-prior-health"
                 :text "From 2026-01-10 until 2026-02-20, I had Condition-A."
                 :names '("Condition-A") :kinds '("condition")
                 :categories '(("medical condition")) :predicate "has_condition"
                 :statements '("The operator previously had Condition-A.")
                 :query "Which prior condition did I report?"
                 :time (%cg-object "character" "temporary-state"
                                   "occurred_at" "2026-01-10"
                                   "valid_from" "2026-01-10"
                                   "valid_until" "2026-02-20")))))
    (dolist (case cases)
      (let* ((episode (sm-episode (getf case :id) (getf case :text) 1000))
             (graph (make-context-graph ontology))
             (owner (%cgf-owner-create-v6 graph "lab-agent" "lab-persona"
                                           "personal-context-core-glm53-v1.2"))
             (events nil)
             (sequence 10000)
             (model-calls 0))
        (labels ((source (target id now)
                   (declare (ignore id now))
                   (lab-authority-context target episode 0))
                 (append-event (type payload cause)
                   (let ((event (%cg-object "id" (incf sequence)
                                            "agent_id" "lab-agent"
                                            "type" type "payload" payload
                                            "caused_by" cause)))
                     (push event events)
                     event))
                 (reserve (phase call-spec digest opened maximum)
                   (declare (ignore phase call-spec digest opened))
                   (assert (= 10 maximum))
                   3)
                 (model (phase call-spec digest opened ceiling)
                   (declare (ignore digest opened))
                   (incf model-calls)
                   (values (personal-formation-model-response phase call-spec case)
                           (min 2 ceiling))))
          (let* ((opened-event
                   (%cgf-owner-open-v2 owner #'source #'append-event 1 0 100 10
                                       1000 "identity-formation-v9" guide))
                 (opened (gethash "id" opened-event))
                 (result (%cgi-owner-run owner #'source #'append-event #'model
                                          opened #'reserve))
                 (expected (%cg-authority-watermark graph "lab-agent"
                                                    "lab-persona"))
                 (fresh-graph (make-context-graph ontology))
                 (fresh-owner (%cgf-owner-create-v6
                               fresh-graph "lab-agent" "lab-persona"
                               "personal-context-core-glm53-v1.2")))
            (assert (equal "complete" (gethash "status" result)))
            (assert (= 7 model-calls))
            (assert (= (length (getf case :names))
                       (context-graph-fact-count graph)))
            (dolist (event (reverse events))
              (%cgi-owner-consume fresh-owner event #'source))
            (assert (%cg-authority-equal-p
                     expected
                     (%cg-authority-watermark fresh-graph "lab-agent"
                                              "lab-persona")))
            (let* ((retrieval (%cg-authority-retrieve
                               fresh-graph "lab-agent" "lab-persona"
                               (getf case :query) :lexicon lexicon :focused t
                               :query-specific-relations t :factual-entities t
                               :context-limits #(6 6 0)))
                   (packet (gethash "context" retrieval))
                   (wire (%cg-authority-canonical-json packet)))
              (assert (= (length (getf case :names))
                         (length (gethash "facts" packet))))
              (dolist (name (getf case :names))
                (assert (search name wire :test #'char-equal)))
              (assert (search (getf case :id)
                              (%cg-authority-canonical-json
                               (context-graph-facts fresh-graph))
                               :test #'char-equal)))))))))
(format t "FORMATION-OWNER-V6 personal pets, spouse, children and prior-health histories form through scripted production phases and cold-replay with provenance passed~%")

(defun typed-family-ontology-fixture ()
  (multiple-value-bind (ontology guide lexicon)
      (personal-formation-ontology-fixture)
    (setf (gethash "entity_types" ontology)
          (concatenate 'vector (gethash "entity_types" ontology)
                       #("attribute_value"))
          (gethash "edge_types" ontology)
          (concatenate
           'vector (gethash "edge_types" ontology)
           (vector
            (%cg-object "name" "parent_of" "subject_types" #("person")
                        "object_types" #("person"))
            (%cg-object "name" "daughter_of" "subject_types" #("person")
                        "object_types" #("person"))
            (%cg-object "name" "son_of" "subject_types" #("person")
                        "object_types" #("person"))
            (%cg-object "name" "spouse_of" "subject_types" #("person")
                        "object_types" #("person"))
            (%cg-object "name" "companion_of"
                        "subject_types" #("person" "organism")
                        "object_types" #("person" "organism"))
            (%cg-object "name" "has_age"
                        "subject_types" #("person" "organism")
                        "object_types" #("attribute_value"))
            (%cg-object "name" "has_gender"
                        "subject_types" #("person" "organism")
                        "object_types" #("attribute_value"))))
          guide
          (concatenate
           'vector guide
           (vector
            (%cg-object
             "name" "attribute_value"
             "definition" "An exact scalar or categorical attribute value."
             "inclusion_rule"
             "Use only as the object of a typed attribute predicate."
             "exclusion_rule" "Do not use for a person or organism."))))
    (values ontology guide lexicon)))

(defun typed-family-case-index (case mention-id)
  (position mention-id (getf case :mention-ids) :test #'equal))

(defun typed-family-matching-candidate (input designation)
  (let ((matches
          (loop for row across (gethash "candidates" input #())
                when (string-equal designation (gethash "name" row ""))
                  collect (gethash "candidate" row))))
    (and (= 1 (length matches)) (first matches))))

(defun typed-family-review-response (spec)
  (let ((response (personal-formation-review-response spec)))
    (loop for reference being the hash-keys of
          (gethash "claim_reviews" response)
          using (hash-value row)
          unless (uiop:string-prefix-p "entity:" reference)
            do (setf (gethash "durable_relevance"
                              (gethash "quality_checks" row))
                     "supported"))
    response))

(defun typed-family-model-response (phase spec case)
  (let* ((input (gethash "input" spec))
         (names (getf case :names))
         (mention-ids (getf case :mention-ids)))
    (labels ((name-for (id)
               (nth (typed-family-case-index case id) names))
             (binding-for (name)
               (let* ((id (nth (position name names :test #'equal)
                               mention-ids))
                      (row (find id (gethash "mention_bindings" input)
                                 :test #'equal
                                 :key (lambda (item)
                                        (gethash "mention" item)))))
                 (and row (gethash "entity" row))))
             (endpoint (value)
               (if (equal value "operator") "operator"
                   (or (binding-for value)
                       (error "No binding for typed-family endpoint ~a" value)))))
      (cond
        ((equal phase "mentions")
         (%cg-object
          "mentions"
          (map 'vector
               (lambda (name)
                 (%cg-object "source" "source_1" "quote" name
                             "designation" name))
               names)))
        ((uiop:string-prefix-p "identity-page-" phase)
         (%cg-object
          "mentions"
          (map 'vector
               (lambda (mention)
                 (let* ((id (gethash "mention" mention))
                        (candidate
                          (typed-family-matching-candidate
                           input (gethash "designation" mention))))
                   (%cg-object "mention" id
                               "status" (if candidate "possible" "none")
                               "candidates" (if candidate
                                                (vector candidate) #()))))
               (gethash "mentions" input))))
        ((equal phase "identity-resolve")
         (%cg-object
          "resolutions"
          (map 'vector
               (lambda (mention)
                 (let* ((id (gethash "mention" mention))
                        (candidate
                          (typed-family-matching-candidate
                           input (gethash "designation" mention))))
                   (%cg-object "mention" id
                               "candidate" (or candidate :null))))
               (gethash "mentions" input))))
        ((equal phase "new-identity-groups")
         (%cg-object
          "groups"
          (map 'vector
               (lambda (id)
                 (%cg-object "mentions" (vector id) "source" "source_1"
                             "quote" (name-for id)))
               (gethash "eligible_mentions" input))))
        ((equal phase "new-identities")
         (%cg-object
          "new_entities"
          (map 'vector
               (lambda (id)
                 (let ((index (typed-family-case-index case id)))
                   (%cg-object "mention" id "name" (nth index names)
                               "kind" (nth index (getf case :kinds))
                               "alternate_names" #() "categories" #())))
               (gethash "eligible_mentions" input))))
        ((equal phase "facts")
         (%cg-object
          "facts"
          (map 'vector
               (lambda (row)
                 (%cg-object
                  "subject" (endpoint (getf row :subject))
                  "object" (endpoint (getf row :object))
                  "predicate" (getf row :predicate)
                  "statement" (getf row :statement)
                  "scope" "assertion" "polarity" "positive"
                  "attributed_to" "operator"
                  "time" (%cg-object "character" "ongoing-state"
                                      "occurred_at" :null "valid_from" :null
                                      "valid_until" :null)
                  "evidence"
                  (vector (%cg-object "source" "source_1"
                                      "quote" (getf case :text)))))
               (getf case :facts))
          "name_corrections" #()))
        ((equal phase "review")
         (typed-family-review-response spec))
        (t (error "Typed-family fixture reached unexpected phase ~a" phase))))))

(multiple-value-bind (ontology guide lexicon)
    (typed-family-ontology-fixture)
  (declare (ignore guide lexicon))
  (let* ((*cgf-protocol* "identity-formation-v14")
         (graph (make-context-graph ontology))
         (episode (sm-episode
                   "typed-age-mention"
                   "Person-A is 15 yo and Person-B is 12 year old."
                   1999))
         (context (lab-authority-context graph episode 0))
         (response
           (%cg-object
            "mentions"
            (vector
             (%cg-object "source" "source_1" "quote" "Person-A"
                         "designation" "Person-A")
             (%cg-object "source" "source_1" "quote" "Person-B"
                         "designation" "Person-B"))))
         (mentions (%cgf-mentions context response))
         (designations
           (loop for mention across mentions
                 collect (gethash "designation" mention)))
         (groups
           (%cgf-normalize-typed-groups
            mentions
            (vector
             (%cg-object "mentions" #("mention_1" "mention_3")
                         "source" "source_1" "quote" "Person-A is 15 yo")
             (%cg-object "mentions" #("mention_2" "mention_4")
                         "source" "source_1"
                         "quote" "Person-B is 12 year old")))))
    (assert (= 4 (length mentions)))
    (assert (member "15 yo" designations :test #'equal))
    (assert (member "12 year old" designations :test #'equal))
    (assert (= 4 (length groups)))
    (assert (every (lambda (group)
                     (= 1 (length (gethash "mentions" group))))
                   groups))))
(format t "FORMATION-V14 exact age phrases are deterministically exposed as inert source mentions passed~%")

(multiple-value-bind (ontology guide lexicon)
    (typed-family-ontology-fixture)
  (let* ((cases
           (vector
            (list :id "typed-family-first"
                  :text "My children are Child-A and Child-B."
                  :names '("Child-A" "Child-B")
                  :mention-ids '("mention_1" "mention_2")
                  :kinds '("person" "person")
                  :facts
                  (list
                   (list :subject "operator" :object "Child-A"
                         :predicate "parent_of"
                         :statement "The operator is the parent of Child-A.")
                   (list :subject "operator" :object "Child-B"
                         :predicate "parent_of"
                         :statement "The operator is the parent of Child-B.")))
            (list :id "typed-family-details"
                  :text "Child-A is my daughter, a girl, and is 17 years old; Child-B is my son, a boy, and is 14 years old."
                  :names '("Child-A" "Child-B" "girl" "boy"
                           "17 years old" "14 years old")
                  :mention-ids '("mention_1" "mention_2" "mention_3"
                                 "mention_4" "mention_5" "mention_6")
                  :kinds '("person" "person" "attribute_value"
                           "attribute_value" "attribute_value"
                           "attribute_value")
                  :facts
                  (list
                   (list :subject "Child-A" :object "operator"
                         :predicate "daughter_of"
                         :statement "Child-A is the operator's daughter.")
                   (list :subject "Child-B" :object "operator"
                         :predicate "son_of"
                         :statement "Child-B is the operator's son.")
                   (list :subject "Child-A" :object "girl"
                         :predicate "has_gender"
                         :statement "Child-A is a girl.")
                   (list :subject "Child-B" :object "boy"
                         :predicate "has_gender"
                         :statement "Child-B is a boy.")
                   (list :subject "Child-A" :object "17 years old"
                         :predicate "has_age"
                         :statement "Child-A is 17 years old.")
                   (list :subject "Child-B" :object "14 years old"
                         :predicate "has_age"
                         :statement "Child-B is 14 years old.")))))
         (episodes
           (map 'vector
                (lambda (case ordinal)
                  (sm-episode (getf case :id) (getf case :text)
                              (+ 2000 ordinal)))
                cases #(0 1)))
         (graph (make-context-graph ontology))
         (owner (%cgf-owner-create-v9
                 graph "lab-agent" "lab-persona"
                 "personal-context-core-glm53-v1.3"))
         (events nil) (sequence 20000) (model-calls 0))
    (labels ((source (target id now)
               (declare (ignore now))
               (lab-authority-context target (aref episodes (1- id))
                                      (1- id)))
             (append-event (type payload cause)
               (let ((event (%cg-object "id" (incf sequence)
                                        "agent_id" "lab-agent" "type" type
                                        "payload" payload "caused_by" cause)))
                 (push event events) event))
             (reserve (phase call-spec digest opened maximum)
               (declare (ignore phase call-spec digest opened))
               (assert (= 14 maximum))
               3)
             (model (phase call-spec digest opened ceiling)
               (declare (ignore digest ceiling))
               (incf model-calls)
               (let* ((record (gethash opened
                                       (cgi-owner-opens owner)))
                      (episode-id
                        (gethash "episode_event_id" (%cgro-record record))))
                 (values
                  (typed-family-model-response
                   phase call-spec (aref cases (1- episode-id)))
                  2))))
      (dotimes (index 2)
        (let* ((episode-id (1+ index))
               (opened-event
                 (%cgf-owner-open-v2
                  owner #'source #'append-event episode-id 0
                  (+ 3000 index) 14 (+ 2000 index)
                  "identity-formation-v14" guide))
               (result
                 (%cgi-owner-run owner #'source #'append-event #'model
                                  (gethash "id" opened-event) #'reserve)))
          (unless (equal "complete" (gethash "status" result))
            (error "Typed-family owner failed: ~a"
                   (%cg-authority-canonical-json
                   (%cg-object "result" result
                                "latest_events"
                                (coerce
                                 (loop for event in events repeat 5
                                       collect
                                       (%cg-object
                                        "type" (gethash "type" event)
                                        "payload" (gethash "payload" event)))
                                 'vector)))))))
      (assert (= 14 model-calls))
      (assert (= 8 (context-graph-fact-count graph)))
      (assert (= 2 (count "parent_of"
                          (loop for row being the hash-values of
                                (context-graph-facts graph) collect row)
                          :test #'equal
                          :key (lambda (row) (gethash "predicate" row)))))
      (assert (zerop (count "related_to"
                            (loop for row being the hash-values of
                                  (context-graph-facts graph) collect row)
                            :test #'equal
                            :key (lambda (row) (gethash "predicate" row)))))
      (dolist (predicate '("daughter_of" "son_of" "has_age" "has_gender"))
        (assert (find predicate
                      (loop for row being the hash-values of
                            (context-graph-facts graph) collect row)
                      :test #'equal
                      :key (lambda (row) (gethash "predicate" row)))))
      (let* ((expected (%cg-authority-watermark graph "lab-agent" "lab-persona"))
             (fresh-graph (make-context-graph ontology))
             (fresh-owner (%cgf-owner-create-v9
                           fresh-graph "lab-agent" "lab-persona"
                           "personal-context-core-glm53-v1.3")))
        (dolist (event (reverse events))
          (%cgi-owner-consume fresh-owner event #'source))
        (assert (%cg-authority-equal-p
                 expected
                 (%cg-authority-watermark fresh-graph
                                          "lab-agent" "lab-persona")))
        (let* ((retrieval
                 (%cg-authority-retrieve
                  fresh-graph "lab-agent" "lab-persona"
                  "Which daughter is 17 years old?"
                  :lexicon lexicon :focused t :query-specific-relations t
                  :factual-entities t :context-limits #(8 8 0)))
               (wire (%cg-authority-canonical-json
                      (gethash "context" retrieval))))
          (assert (search "Child-A" wire :test #'char-equal))
          (assert (search "17 years old" wire :test #'char-equal)))))))
(format t "FORMATION-OWNER-V9 typed parent/child roles and exact age/gender values update reused identities and cold-replay passed~%")
