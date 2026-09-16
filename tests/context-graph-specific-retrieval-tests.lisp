;;;; harness: bare
(load (merge-pathnames "context-graph-focused-retrieval-tests.lisp" *load-truename*))
(in-package :pai.context-graph)

(labels ((endpoint (id &optional (role :null)) (%cg-object "entity_id" id "participant_role" role))
         (fact (id predicate subject object terms)
           (%cgr-row "fact" id 10 terms #("category")
                     (%cg-object "subject" subject "predicate" predicate "object" object))))
  (let* ((a (endpoint "a")) (b (endpoint "b")) (operator (endpoint "speaker" "operator"))
         (generic (fact "generic" "related_to" a b #("cat")))
         (first (fact "first" "owns" operator a #("cat")))
         (second (fact "second" "owns" operator b #("cat"))))
    (rt-check "specific relations cover both distinct endpoints"
      (equalp (vector first second) (%cgr-specific-facts (vector generic first second))))
    (rt-check "one uncovered endpoint retains generic relation"
      (= 2 (length (%cgr-specific-facts (vector generic first)))))
    (rt-check "generic-only information survives"
      (= 1 (length (%cgr-specific-facts (vector generic)))))
    (setf (gethash "matched_terms" generic) #("cat" "separate"))
    (rt-check "unique relational query evidence survives"
      (= 3 (length (%cgr-specific-facts (vector generic first second)))))
    (setf (gethash "matched_terms" generic) #("cat")
          (gethash "subject" (gethash "value" generic)) operator)
    (rt-check "participant relation not treated as incidental identity edge"
      (= 3 (length (%cgr-specific-facts (vector generic first second))))))

  (let* ((a (endpoint "a")) (b (endpoint "b")) (operator (endpoint "speaker" "operator"))
         (generic (fact "generic" "related_to" a b #("maple")))
         (specific (fact "specific" "owns" operator a #("maple"))))
    (rt-check "query-specific policy needs only the matched endpoint"
      (equalp (vector specific) (%cgr-query-specific-facts (vector generic specific))))
    (setf (gethash "signals" generic) #("statement"))
    (rt-check "generic statement evidence survives query-specific policy"
      (= 2 (length (%cgr-query-specific-facts (vector generic specific)))))
    (setf (gethash "signals" generic) #("predicate"))
    (rt-check "explicit relation query survives query-specific policy"
      (= 2 (length (%cgr-query-specific-facts (vector generic specific)))))
    (setf (gethash "signals" generic) #("label")
          (gethash "matched_terms" generic) #("maple" "separate"))
    (rt-check "uncovered query evidence preserves generic relation"
      (= 2 (length (%cgr-query-specific-facts (vector generic specific)))))))

(multiple-value-bind (graph packet lexicon bundle) (rt-fixture)
  (declare (ignore packet))
  (let* ((before (%cg-authority-projection-digest graph "lab-agent" "lab-persona"))
         (result (%cg-authority-retrieve graph "lab-agent" "lab-persona" "health"
                   :lexicon lexicon :focused t :specific-relations t)))
    (rt-check "new policy explicit and useful" (and (equal "focused-kg-v2" (gethash "retrieval_revision" result))
                                                  (= 1 (length (rt-rows result "facts")))))
    (setf (gethash "retrieval_policy" bundle) "focused-kg-v2")
    (rt-check "new policy wired through lab subprocess"
      (equal "focused-kg-v2" (gethash "retrieval_revision" (aref (gethash "queries" (gethash "value" (al-run bundle))) 0))))
    (let ((isolated (%cg-authority-retrieve graph "lab-agent" "lab-persona" "Mina"
                      :lexicon lexicon :focused t :query-specific-relations t :factual-entities t)))
      (rt-check "factual context omits unsupported entity card"
        (zerop (length (rt-rows isolated "entities"))))
      (rt-check "unsupported entity remains inspectable as a candidate"
        (= 1 (length (gethash "entities" (gethash "candidates" isolated))))))
    (let ((supported (%cg-authority-retrieve graph "lab-agent" "lab-persona" "health"
                      :lexicon lexicon :focused t :query-specific-relations t :factual-entities t)))
      (rt-check "factual context retains grounded fact"
        (and (equal "focused-kg-v5" (gethash "retrieval_revision" supported))
             (= 1 (length (rt-rows supported "facts"))))))
    (rt-check "old and new relation policies cannot be combined ambiguously"
      (sm-error (lambda () (%cg-authority-retrieve graph "lab-agent" "lab-persona" "health"
                             :lexicon lexicon :focused t :specific-relations t :factual-entities t))
                "RETRIEVAL_INPUT_INVALID"))
    (setf (gethash "retrieval_policy" bundle) "focused-kg-v5")
    (rt-check "factual policy wired through lab subprocess"
      (equal "focused-kg-v5" (gethash "retrieval_revision" (aref (gethash "queries" (gethash "value" (al-run bundle))) 0))))
    (rt-check "read leaves projection unchanged"
      (equal before (%cg-authority-projection-digest graph "lab-agent" "lab-persona")))))

(let* ((path (merge-pathnames "fixtures/context-graph-semantic-relevance-cases.json" *load-truename*))
       (fixture (with-open-file (stream path :external-format :utf-8) (shasht:read-json stream))))
  (rt-check "semantic fixture selects declared policy revision"
    (equal +cgr-semantic-selector-revision+ (gethash "selector_revision" fixture)))
  (loop for case across (gethash "cases" fixture)
        for selected = (%cgr-select-semantic-hits (gethash "scores" case))
        do (rt-check (format nil "semantic fixture ~a" (gethash "name" case))
             (equalp (gethash "expected_ids" case)
                     (map 'vector (lambda (hit) (gethash "id" hit)) selected)))))

(multiple-value-bind (graph packet lexicon bundle) (rt-fixture)
  (declare (ignore bundle))
  (let* ((ids (remove-if-not
                (lambda (id) (eq :null (gethash "participant_role"
                                           (%cg-authority-current-descriptor graph id))))
                (context-graph-entity-scan-index graph)))
         (candidates
           (map 'vector
                (lambda (id)
                  (let ((descriptor (%cg-authority-current-descriptor graph id)))
                    (%cg-object "kind" "entity" "id" id "score_milli"
                                (if (equal "condition" (gethash "kind" descriptor)) 667 450))))
                ids))
         (before (%cg-authority-projection-digest graph "lab-agent" "lab-persona"))
         (receipt (context-graph-build-semantic-receipt
                    graph "lab-agent" "lab-persona" "headaches" packet
                    "fixture-nomic-query-factual-document-v2" candidates))
         (result (%cg-authority-retrieve graph "lab-agent" "lab-persona" "headaches"
                   :source-packet packet :lexicon lexicon :semantic-receipt receipt
                   :focused t :query-specific-relations t :factual-entities t)))
    (rt-check "semantic producer binds separated selection"
      (and (= 1 (length (gethash "hits" receipt)))
           (equal +cgr-semantic-selector-revision+
                  (gethash "semantic_selection_revision" result))))
    (rt-check "semantic candidate discovers grounded paraphrase fact"
      (= 1 (length (rt-rows result "facts"))))
    (rt-check "semantic discovery remains read-only"
      (equal before (%cg-authority-projection-digest graph "lab-agent" "lab-persona")))
    (rt-check "semantic producer refuses incomplete entity universe"
      (sm-error (lambda ()
                  (context-graph-build-semantic-receipt
                    graph "lab-agent" "lab-persona" "headaches" packet
                    "fixture-nomic-query-factual-document-v2"
                    (subseq candidates 0 (1- (length candidates)))))
                "RETRIEVAL_SEMANTIC_PRODUCER_INCOMPLETE"))
    (setf (gethash "selector_revision" receipt) "unknown-selector")
    (rt-check "unknown semantic selector revision fails closed"
      (sm-error (lambda ()
                  (%cg-authority-retrieve graph "lab-agent" "lab-persona" "headaches"
                    :source-packet packet :lexicon lexicon :semantic-receipt receipt))
                "RETRIEVAL_SEMANTIC_BINDING_INVALID"))))
(format t "SPECIFIC RETRIEVAL checks passed~%")
