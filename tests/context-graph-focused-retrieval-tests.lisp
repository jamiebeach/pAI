;;;; harness: bare
(load (merge-pathnames "context-graph-retrieval-tests.lisp" *load-truename*))
(in-package :pai.context-graph)

;; Different health condition and vocabulary from the private development audit.
;; Expected positives/negatives are explicit; no instance IDs or answer lookup.
(multiple-value-bind (graph packet lexicon bundle) (rt-fixture)
  (setf (gethash "retrieval_policy" bundle) "focused-kg-v1"
        (gethash "queries" bundle) #("What health issues have I mentioned?" "What is my blood type?" "server health"))
  (let ((queries (gethash "queries" (gethash "value" (al-run bundle)))))
    (rt-check "actual lab subprocess uses focused production reader"
              (and (equal "focused-kg-v1" (gethash "retrieval_revision" (aref queries 0)))
                   (= 1 (length (rt-rows (aref queries 0) "facts")))
                   (zerop (length (rt-rows (aref queries 1) "entities")))
                   (zerop (length (rt-rows (aref queries 2) "facts"))))))
  (let ((before (%cg-authority-projection-digest graph "lab-agent" "lab-persona")))
    (labels ((ask (q) (%cg-authority-retrieve graph "lab-agent" "lab-persona" q
                                           :source-packet packet :lexicon lexicon :focused t)))
      (dolist (q '("What health issues have I mentioned?" "medical concerns" "HEALTH problems"))
        (let ((result (ask q)))
          (rt-check (format nil "focused health positive: ~a" q)
                    (search "B12 deficiency" (gethash "statement" (gethash "value" (aref (rt-rows result "facts") 0)))))
          (rt-check "health context excludes incidental entities and repeated source"
                    (and (zerop (length (rt-rows result "entities"))) (zerop (length (rt-rows result "sources")))))))
      (dolist (q '("What is my blood type?" "server health" "medical allergy" "volcanic apparatus" "minaret"))
        (rt-check (format nil "focused realistic negative: ~a" q)
                  (every (lambda (key) (zerop (length (rt-rows (ask q) key)))) '("entities" "facts" "sources"))))
      (rt-check "plural category discovers isolated entity"
                (equal "Mina" (rt-label (aref (rt-rows (ask "cats") "entities") 0))))
      (rt-check "alias discovery survives focused selection"
                (equal "Star Atlas" (rt-label (aref (rt-rows (ask "skybook") "entities") 0))))
      (rt-check "original source-only discovery survives"
                (search "playful" (gethash "text" (gethash "value" (aref (rt-rows (ask "playful") "sources") 0)))))
      (rt-check "focused exact evidence retained"
                (equal "I have B12 deficiency."
                       (gethash "quote" (aref (gethash "evidence_excerpts" (gethash "value" (aref (rt-rows (ask "health") "facts") 0))) 0))))
      (rt-check "focused bounded repeat deterministic"
                (equal (%cg-authority-canonical-json (ask "health")) (%cg-authority-canonical-json (ask "health"))))
      (rt-check "generation tokenizer remains unchanged" (equal '("cats") (%cgr-tokens "cats")))
      (rt-check "reporting scaffolding does not become retrieval evidence"
                (equal '("cat")
                       (%cgr-focus-terms
                         (%cgr-tokens "What did the operator say about their cat?"))))
      (rt-check "name paraphrases share a bounded retrieval family"
                (and (member "identifie" (%cgr-expand-query-terms '("name")) :test #'equal)
                     (member "name" (%cgr-expand-query-terms '("called")) :test #'equal)
                     (equal '("asteroid") (%cgr-expand-query-terms '("asteroid")))))
      (rt-check "interrogative evidence is not answer-bearing retrieval text"
                (and (null (%cgr-answer-evidence-tokens
                             (%cg-object "quote" "Do you remember my name?")))
                     (equal '("name" "rowan")
                            (%cgr-answer-evidence-tokens
                              (%cg-object "quote" "My name is Rowan.")))))
      (rt-check "focused reads leave projection unchanged"
                (equal before (%cg-authority-projection-digest graph "lab-agent" "lab-persona"))))))
(format t "FOCUSED RETRIEVAL checks passed~%")

;; End-to-end paraphrase qualification.  This uses a generic synthetic name;
;; no private label or fixture identifier is encoded in selection logic.
(multiple-value-bind (graph ignored-packet lexicon ignored-bundle) (rt-fixture)
  (declare (ignore ignored-packet ignored-bundle))
  (setf (gethash "edge_types" (context-graph-ontology graph))
        (concatenate 'vector (gethash "edge_types" (context-graph-ontology graph))
          (vector (%cg-object "name" "related_to"
                              "subject_types" #("person" "agent" "organism" "condition" "artifact")
                              "object_types" #("person" "agent" "organism" "condition" "artifact")))))
  (let* ((episode (sm-episode "name-paraphrase" "I identify as Rowan." 300))
         (simple (%cg-object
                   "new_entities" (vector (%cg-object "name" "Rowan" "kind" "person"
                                            "alternate_names" #() "categories" #()))
                   "facts" (vector (%cg-object "subject" "operator" "predicate" "related_to"
                                      "object" "new_1" "statement" "The operator identifies as Rowan."
                                      "scope" "assertion" "polarity" "positive"
                                      "attributed_to" "operator"
                                      "time" (%cg-object "character" "ongoing-state" "occurred_at" :null
                                                         "valid_from" :null "valid_until" :null)
                                      "evidence" (vector (%cg-object "source" "source_1"
                                                                     "quote" "I identify as Rowan."))))
                   "name_corrections" #()))
         (step (%cg-object "episode" episode "proposal" simple "review" :null "request_digest" :null)))
    (multiple-value-bind (context boundary) (lab-authority-context graph episode 1)
      (declare (ignore boundary))
      (let ((raw (%cgs-expand context (context-graph-ontology graph)
                              "personal-context-core-glm53-v1.2" simple)))
        (setf (gethash "request_digest" step)
                (gethash "request_digest" (lab-authority-step graph step 1
                                              "personal-context-core-glm53-v1.2" t))
              (gethash "review" step) (sm-review context raw)))
      (rt-check "synthetic personal identifier admitted"
                (equal "accepted" (gethash "status" (lab-authority-step graph step 1
                                                       "personal-context-core-glm53-v1.2" t))))
      (labels ((ask (query)
                 (%cg-authority-retrieve graph "lab-agent" "lab-persona" query
                   :source-packet (gethash "source_packet" context) :lexicon lexicon
                   :focused t :query-specific-relations t :factual-entities t)))
        (dolist (query '("What is my name?" "What is the operator called?"))
          (rt-check (format nil "name paraphrase retrieves admitted identifier: ~a" query)
                    (let ((facts (rt-rows (ask query) "facts")))
                      (and (= 1 (length facts))
                           (search "Rowan" (gethash "statement" (gethash "value" (aref facts 0))))
                           (find "evidence" (gethash "signals" (aref facts 0)) :test #'equal)))))
        (dolist (query '("What is the server called?" "What is the asteroid called?"))
          (rt-check (format nil "name-family negative stays empty: ~a" query)
                    (zerop (length (rt-rows (ask query) "facts")))))))))

;; Selector regression independent of private episodes: an object/category
;; anchor must defeat an incidental shared verb in a different statement.
(let* ((entity (%cgr-row "entity" "fixture-object" 7 #("database") #("category") (%cg-object)))
       (wanted (%cgr-row "fact" "fixture-wanted" 8 #("database") #("statement") (%cg-object)))
       (noise (%cgr-row "fact" "fixture-noise" 8 #("keep") #("statement") (%cg-object))))
  (multiple-value-bind (entities facts sources omitted)
      (%cgr-focused-pools (vector entity) (vector noise wanted) #() '("database" "keep"))
    (declare (ignore entities sources omitted))
    (rt-check "descriptor anchor excludes incidental verb statement"
              (and (= 1 (length facts)) (equal "fixture-wanted" (gethash "id" (aref facts 0)))))))

;; A topic-label anchor must not suppress a stronger, independently grounded
;; answer that uses the question's relationship words rather than the label.
(let* ((entity (%cgr-row "entity" "fixture-topic" 12 #("implementation") #("label") (%cg-object)))
       (topic (%cgr-row "fact" "fixture-topic-fact" 10 #("implementation") #("label") (%cg-object)))
       (answer (%cgr-row "fact" "fixture-answer" 20 #("doing" "work") #("evidence") (%cg-object))))
  (multiple-value-bind (entities facts sources omitted)
      (%cgr-focused-pools (vector entity) (vector answer topic) #()
                          '("doing" "implementation" "work"))
    (declare (ignore entities sources omitted))
    (rt-check "grounded multi-term answer bypasses descriptor anchor narrowing"
              (find "fixture-answer" facts :test #'equal :key (lambda (row) (gethash "id" row))))))
