;;;; harness: bare
(load (merge-pathnames "context-graph-identity-pages-tests.lisp" *load-truename*))
(in-package :pai.context-graph)

(defun gf-review (spec &key reject-identity source-reading)
  (assert (gethash "identity_comparison" (gethash "input" spec)))
  (%cg-object "schema_version" 2 "revision_reviews" #()
    "claim_reviews" (map 'vector (lambda (claim)
      (let ((entity (equal "entity" (gethash "claim_kind" claim))))
        (%cg-object "claim_ref" (gethash "claim_ref" claim) "verdict" "DIRECTLY_EVIDENCED"
                    "evidence" "Controlled source fixture judgment, not actual model qualification."
                    "source_reading" (if entity "not-applicable" (or source-reading "assertion"))
                    "quality_checks" (apply #'%cg-object (loop for key in +cgq-checks+ append
                      (list key (cond ((and entity (not (equal key "endpoint_identity"))) "not-applicable")
                                      ((and entity reject-identity) "unsupported") (t "supported"))))))))
      (gethash "claims" (gethash "input" spec)))))

(defun gf-model (phase spec &key reuse uncertain bad-new fallback reject-identity source-reading quote)
  (let ((input (gethash "input" spec)))
    (cond ((equal phase "mentions") (%cg-object "mentions" (vector (%cg-object "source" "source_1" "quote" "Mina"))))
          ((uiop:string-prefix-p "identity-page-" phase)
           (%cg-object "mentions" (vector (%cg-object "mention" "mention_1" "status"
                          (cond (uncertain "uncertain") (reuse "possible") (t "none"))
                          "candidates" (if (or reuse uncertain) #("candidate_1") #())))))
          ((equal phase "identity-resolve") (%cg-object "resolutions" (vector (%cg-object "mention" "mention_1" "candidate" (if (and reuse (not uncertain)) "candidate_1" :null)))))
          ((equal phase "new-identities")
           (let ((entity (%cg-detach (aref (gethash "new_entities" (sm-proposal)) 0))))
             (setf (gethash "mention" entity) "mention_1")
             (when bad-new (setf (gethash "name" entity) "Nora"))
             (%cg-object "new_entities" (vector entity))))
          ((equal phase "facts")
           (let* ((fact (%cg-detach (aref (gethash "facts" (sm-proposal)) 0)))
                  (binding (gethash "entity" (aref (gethash "mention_bindings" input) 0))))
             (setf (gethash "object" fact) (if fallback "new_1" binding))
             (when quote (setf (gethash "quote" (aref (gethash "evidence" fact) 0)) quote))
             (%cg-object "facts" (if (and (eq binding :null) (not fallback)) #() (vector fact)) "name_corrections" #())))
          ((equal phase "review") (gf-review spec :reject-identity reject-identity :source-reading source-reading))
          (t (error "Unexpected phase ~a" phase)))))

(let* ((ontology (gethash "ontology" (as-fixture))) (revision "personal-context-core-glm53-v1.2")
       (graph (make-context-graph ontology)) (history nil))
  (dotimes (step 2)
    (let* ((episode (sm-episode (format nil "formation-~d" step) "I own a cat named Mina." (+ 100 step)))
           (cache (make-hash-table :test #'equal)) (calls 0) (pause (= step 0)))
      (multiple-value-bind (full boundary) (lab-authority-context graph episode step)
        (let ((before (%cg-authority-watermark graph "lab-agent" "lab-persona")) (count (context-graph-entity-count graph)))
          (labels ((port (phase spec digest)
                     (let ((saved (gethash phase cache)))
                       (when saved (assert (equal digest (car saved))) (return-from port (%cg-detach (cdr saved)))))
                     (when (and pause (equal phase "facts")) (setf pause nil) (return-from port :paused-budget))
                     (incf calls)
                     (let ((response (context-graph-runtime-read-json (context-graph-runtime-json (gf-model phase spec :reuse (= step 1))))))
                       (setf (gethash phase cache) (cons digest response)) response)))
            (when (= step 0)
              (assert (eq :paused-budget (%cgf-generate graph full 0 ontology revision #'port)))
              (assert (= 4 calls)))
            (let* ((envelope (context-graph-runtime-read-json (context-graph-runtime-json (%cgf-generate graph full 0 ontology revision #'port))))
                   (target (gethash "context" envelope)))
              (assert (= (if (= step 0) 6 5) calls))
              (assert (%cg-authority-equal-p before (%cg-authority-watermark graph "lab-agent" "lab-persona")))
              (setf (gethash "episode_id" boundary) (gethash "episode_id" target))
              (let ((bad (%cg-detach envelope)))
                (setf (gethash "request_digest" (aref (gethash "calls" bad) 1)) (%cg-sha256 "forged"))
                (assert (sm-error (lambda () (%cgf-apply graph boundary full bad)) "FORMATION_RECEIPT_MISMATCH")))
              (let ((result (%cgf-apply graph boundary full envelope)))
                (assert (equal "accepted" (gethash "status" result))))
              (when (= step 1) (assert (= count (context-graph-entity-count graph))))
              (assert (plusp (length (gethash "facts" (gethash "context" (%cg-authority-retrieve graph "lab-agent" "lab-persona" "Mina cat"))))))
              (assert (zerop (length (gethash "facts" (gethash "context" (%cg-authority-retrieve graph "lab-agent" "lab-persona" "asteroid"))))))
              (push (list episode boundary envelope step) history)))))))
  (let ((fresh (make-context-graph ontology)) (expected (%cg-authority-watermark graph "lab-agent" "lab-persona")))
    (dolist (row (reverse history))
      (destructuring-bind (episode boundary envelope step) row
        (%cgf-apply fresh boundary (lab-authority-context fresh episode step) envelope)))
    (assert (%cg-authority-equal-p expected (%cg-authority-watermark fresh "lab-agent" "lab-persona"))))
  ;; An uncertain existing identity cannot be replaced by new_1 in fact output.
  (let ((episode (sm-episode "formation-unresolved" "I own a cat named Mina." 300)))
    (multiple-value-bind (full boundary) (lab-authority-context graph episode 2)
      (declare (ignore boundary))
      (let ((before (%cg-authority-watermark graph "lab-agent" "lab-persona")) (new-called nil))
        (assert (sm-error (lambda () (%cgf-generate graph full 0 ontology revision
          (lambda (phase spec digest) (declare (ignore digest))
            (when (equal phase "new-identities") (setf new-called t))
            (gf-model phase spec :reuse t :uncertain t :fallback t)))) "FORMATION_RESPONSE_INVALID"))
        (assert (not new-called))
        (assert (%cg-authority-equal-p before (%cg-authority-watermark graph "lab-agent" "lab-persona"))))))
  ;; Deliberately wrong all-none comparisons still face independent identity review.
  (let ((episode (sm-episode "formation-false-negative" "I own a cat named Mina." 400)))
    (multiple-value-bind (full boundary) (lab-authority-context graph episode 3)
      (let* ((count (context-graph-entity-count graph))
             (envelope (%cgf-generate graph full 0 ontology revision
                          (lambda (phase spec digest) (declare (ignore digest)) (gf-model phase spec :reject-identity t)))))
        (setf (gethash "episode_id" boundary) (gethash "episode_id" (gethash "context" envelope)))
        (%cgf-apply graph boundary full envelope)
        (assert (= count (context-graph-entity-count graph)))))))

(let* ((ontology (gethash "ontology" (as-fixture))) (graph (make-context-graph ontology))
       (revision "personal-context-core-glm53-v1.2") (episode (sm-episode "formation-negative" "I own a cat named Mina." 100)))
  (multiple-value-bind (full boundary) (lab-authority-context graph episode 0)
    (declare (ignore boundary))
    (assert (sm-error (lambda () (%cgf-generate graph full 0 ontology revision
      (lambda (phase spec digest) (declare (ignore digest)) (gf-model phase spec :bad-new t)))) "FORMATION_NEW_IDENTITY_INVALID"))
    (let ((calls 0))
      (assert (sm-error (lambda () (%cgf-generate graph full 0 ontology revision
        (lambda (phase spec digest) (declare (ignore digest)) (incf calls) (gf-model phase spec)) :max-calls 3)) "FORMATION_CALL_LIMIT"))
      (assert (= 1 calls)))
    (let ((empty (%cgf-generate graph full 0 ontology revision
                   (lambda (phase spec digest) (declare (ignore spec digest)) (assert (equal phase "mentions")) (%cg-object "mentions" #())))))
      (assert (equal "empty" (gethash "status" empty))))))
;; Mention selection retains grounded rows when one schema-valid row attaches a
;; repeated exact quote to the wrong local source in a later source batch.
(let* ((ontology (gethash "ontology" (as-fixture))) (graph (make-context-graph ontology))
       (episode (sm-episode "mention-row-isolation" "Fixture source zero." 500)))
  (setf (gethash "sources" episode)
        (coerce (loop for i below 14 collect
                  (%cg-object "source_id" (format nil "fixture-source:~d" i)
                              "speaker_id" "operator" "kind" "original-utterance"
                              "text" (case i
                                       (9 "A grounded mention names Cedar.")
                                       (10 "A repeated phrase appears here.")
                                       (13 "A repeated phrase appears here.")
                                       (t (format nil "Fixture source ~d." i)))))
                'vector))
  (multiple-value-bind (full boundary) (lab-authority-context graph episode 0)
    (declare (ignore boundary))
    (let* ((context (%cgro-batch-context graph full 1 "staged" "bounded-v3"))
           (response (%cg-object "mentions"
                       (vector (%cg-object "source" "source_2" "quote" "Cedar")
                               (%cg-object "source" "source_4" "quote" "A repeated phrase")
                               (%cg-object "source" "source_2" "quote" "Cedar"))))
           (mentions (%cgf-mentions context response)))
      (assert (= 1 (length mentions)))
      (assert (equal "mention_1" (gethash "mention" (aref mentions 0))))
      (assert (equal "source_2" (gethash "source" (aref mentions 0)))))))
(format t "IDENTITY-FORMATION invalid and duplicate mention-row isolation passed~%")

;; V11 makes the referent machine-readable, so one exact source sentence can
;; yield two independently resolvable people without duplicating an ambiguous
;; (source, quote) key.
(let* ((*cgf-protocol* "identity-formation-v11")
       (ontology (gethash "ontology" (as-fixture)))
       (graph (make-context-graph ontology))
       (quote "Avery and Morgan are my children."))
  (multiple-value-bind (full boundary)
      (lab-authority-context graph (sm-episode "designated-mentions" quote 600) 0)
    (declare (ignore boundary))
    (let* ((context (%cgro-batch-context graph full 0 "staged" "bounded-v3"))
           (spec (%cgf-mention-spec context))
           (response (%cg-object "mentions"
                       (vector
                        (%cg-object "source" "source_1" "quote" quote
                                    "designation" "Avery")
                        (%cg-object "source" "source_1" "quote" quote
                                    "designation" "Morgan"))))
           (mentions (%cgf-mentions context response))
           (plan (%cgi-plan (vector context) mentions)))
      (assert (= 2 (length mentions)))
      (assert (equal "Avery" (gethash "designation" (aref mentions 0))))
      (assert (equal "Morgan" (gethash "designation" (aref mentions 1))))
      (assert (equal "identity-pages-v3" (gethash "protocol" plan)))
      (assert (search "Prioritize every explicitly named person or organism"
                      (gethash "system" (gethash "value" spec))))
      (assert (search "designation" (gethash "system"
                                     (gethash "value" (%cgi-page-spec plan 0)))))
      (assert (sm-error
               (lambda ()
                 (%cgf-mentions context
                   (%cg-object "mentions"
                     (vector (%cg-object "source" "source_1" "quote" quote)))))
               "FORMATION_MENTIONS_INVALID")))))
(format t "IDENTITY-FORMATION designated multi-referent mention contract passed~%")

;; V12 adds a sealed reviewer-owned cross-turn relevance judgment without
;; changing the V11 mention contract.
(let* ((*cgf-protocol* "identity-formation-v12")
       (*cgm-review-admission-policy* "reviewed-inference-v1")
       (*cgq-durable-relevance-policy* "cross-turn-v1")
       (ontology (gethash "ontology" (as-fixture)))
       (graph (make-context-graph ontology))
       (revision "personal-context-core-glm53-v1.2"))
  (multiple-value-bind (context boundary)
      (lab-authority-context
       graph (sm-episode "durable-review" "I own a cat named Mina." 700) 0)
    (declare (ignore boundary))
    (let* ((raw (%cgs-expand context ontology revision (sm-proposal)))
           (trace (%cg-object "mentions" #() "bindings" #()
                              "all_nonparticipants_selected" :true
                              "candidates" #() "page_responses" #()
                              "resolutions" #() "new_identity_groups" #()))
           (built (%cgf-review-spec context raw revision trace))
           (spec (gethash "value" built))
           (rows (gethash "properties"
                          (gethash "claim_reviews"
                                   (gethash "properties"
                                            (gethash "schema" spec)))))
           (entity-checks
             (gethash "properties"
                      (gethash "quality_checks"
                               (gethash "properties"
                                        (gethash "entity:new_1" rows)))))
           (relationship-checks
             (gethash "properties"
                      (gethash "quality_checks"
                               (gethash "properties"
                                        (gethash "relationship:0" rows))))))
      (assert (null (gethash "durable_relevance" entity-checks)))
      (assert (gethash "durable_relevance" relationship-checks))
      (assert (search "Embedded durable details do not rescue"
                      (gethash "system" spec))))))
(format t "IDENTITY-FORMATION V12 durable review contract passed~%")

;; V13 may use an already-reviewed inference as bounded identity-comparison
;; context.  It is not factual authority and must not disappear between the
;; discovery plan and the page/resolution requests.
(let* ((*cgf-protocol* "identity-formation-v13")
       (ontology (gethash "ontology" (as-fixture)))
       (graph (make-context-graph ontology))
       (revision "personal-context-core-glm53-v1.2")
       (first (sm-episode "inference-anchor-first" "I own a cat named Mina." 800)))
  (multiple-value-bind (full boundary) (lab-authority-context graph first 0)
    (let* ((*cgf-protocol* "identity-formation-v1")
           (envelope
             (%cgf-generate graph full 0 ontology revision
               (lambda (phase spec digest)
                 (declare (ignore digest))
                 (gf-model phase spec)))))
      (setf (gethash "episode_id" boundary)
            (gethash "episode_id" (gethash "context" envelope)))
      (%cgf-apply graph boundary full envelope)))
  (let ((fact (loop for row being the hash-values of (context-graph-facts graph)
                    return row)))
    (setf (gethash "evidence_status" fact) "inference"))
  (multiple-value-bind (full boundary)
      (lab-authority-context
       graph (sm-episode "inference-anchor-later" "Mina is asleep." 900) 1)
    (declare (ignore boundary))
    (let* ((*cgi-inference-identity-anchors-p* t)
           (context (%cgro-batch-context graph full 0 "staged" "bounded-v3"))
           (mentions
             (vector (%cg-object "mention" "mention_1" "source" "source_1"
                                 "quote" "Mina" "designation" "Mina")))
           (discovery (%cgi-discover graph full 0 mentions))
           (plan (gethash "plan" discovery))
           (anchors (gethash "candidate_identity_anchors" plan))
           (page-input (gethash "input" (gethash "value" (%cgi-page-spec plan 0)))))
      (assert (= 1 (length anchors)))
      (assert (equal "inference" (gethash "evidence_status" (aref anchors 0))))
      (assert (equal "owns" (gethash "predicate" (aref anchors 0))))
      (assert (= 1 (length (gethash "candidate_identity_anchors" page-input)))))))
(format t "IDENTITY-FORMATION V13 inference identity anchors passed~%")

(format t "IDENTITY-FORMATION new/reused identity, useful retrieval, pause/cache, cold graph replay, ambiguous fallback, source anchoring and review rejection passed~%")
