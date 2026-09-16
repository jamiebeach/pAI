;;;; search.lisp -- compact fact-first query surface for the standalone graph.

(in-package :pai.context-graph)

(defun %cg-query-tokens (query)
  (remove-duplicates
   (remove-if
    (lambda (token) (< (length token) 2))
    (uiop:split-string
     (map 'string
          (lambda (character)
            (if (or (alphanumericp character) (char= character #\_))
                (char-downcase character) #\Space))
          query)
     :separator '(#\Space)))
   :test #'string=))

(defun %cg-entity-text (entity)
  (format nil "~a ~a ~{~a~^ ~} ~{~a~^ ~}"
          (gethash "entity_type" entity "")
          (gethash "name" entity "")
          (%cg-items (gethash "aliases" entity))
          (%cg-items (gethash "classifications" entity))))

(defun %cg-token-score (tokens text)
  (let ((haystack (string-downcase text)))
    (count-if (lambda (token) (search token haystack :test #'char=)) tokens)))

(defun %cg-search-row (fact subject object score)
  (%cg-object
   "fact_id" (gethash "fact_id" fact)
   "score" score
   "subject" (%cg-object "entity_id" (gethash "entity_id" subject)
                          "type" (gethash "entity_type" subject)
                          "name" (gethash "name" subject))
   "predicate" (gethash "predicate" fact)
   "object" (%cg-object "entity_id" (gethash "entity_id" object)
                         "type" (gethash "entity_type" object)
                         "name" (gethash "name" object))
   "fact" (gethash "fact" fact)
   "grounding" (%cg-detach (gethash "grounding" fact))
   "temporal" (%cg-detach (gethash "temporal" fact))
   "observed_at" (gethash "observed_at" fact (gethash "created_at" fact))
   "valid_at" (gethash "valid_at" fact)
   "invalid_at" (gethash "invalid_at" fact)
   "evidence_status" (gethash "evidence_status" fact "unreviewed")
   "evidence_records" (%cg-copy-vector
                        (gethash "evidence_records" fact))
   "source_episode_ids" (%cg-copy-vector
                         (gethash "source_episode_ids" fact))))

(defun %cg-evidence-policy-allows-p (fact policy)
  (let ((status (gethash "evidence_status" fact "unreviewed")))
    (cond ((string= policy "all") t)
          ((string= policy "reviewed")
           (member status '("direct" "prior-graph" "inference") :test #'string=))
          ((string= policy "inferred")
           (member status '("direct" "prior-graph" "inference") :test #'string=))
          ((string= policy "verified")
           (member status '("direct" "prior-graph") :test #'string=))
          ((string= policy "direct-only") (string= status "direct"))
          (t nil))))

(defun %cg-parse-time (value)
  (handler-case
      (cond ((integerp value) value)
            ((and (stringp value) (plusp (length value))
                  (every #'digit-char-p value))
             (parse-integer value))
            ((and (stringp value) (>= (length value) 19))
             (encode-universal-time
              (parse-integer value :start 17 :end 19)
              (parse-integer value :start 14 :end 16)
              (parse-integer value :start 11 :end 13)
              (parse-integer value :start 8 :end 10)
              (parse-integer value :start 5 :end 7)
              (parse-integer value :start 0 :end 4) 0))
            (t nil))
    (error () nil)))

(defun %cg-temporal-salience (fact reference-time half-life)
  (let* ((temporal (gethash "temporal" fact))
         (character (and temporal (gethash "character" temporal)))
         (anchor (and temporal
                      (%cg-parse-time (gethash "occurred_at" temporal))))
         (reference (%cg-parse-time reference-time)))
    (if (and (member character '("event" "temporary-state") :test #'string=)
             (realp half-life) (plusp half-life) anchor reference
             (> reference anchor))
        (expt 0.5 (/ (- reference anchor) half-life))
        1.0)))

(defun context-graph-search
    (graph query &key (maximum-results 10) (include-superseded-p nil)
                      (evidence-policy "all") (claim-policy "factual")
                      (generic-edge-penalty 2) reference-time
                      temporal-half-life-seconds)
  "Return bounded compact facts; raw episode content is never copied."
  (%cg-require-legacy-profile graph)
  (unless (and (context-graph-p graph) (%cg-present-string-p query 1000)
               (integerp maximum-results) (<= 1 maximum-results 50)
               (member claim-policy '("all" "factual" "grounded-assertions") :test #'equal)
               (member evidence-policy
                       '("all" "reviewed" "inferred" "verified" "direct-only")
                       :test #'string=)
               (realp generic-edge-penalty)
               (not (minusp generic-edge-penalty))
               (or (%cg-null-p temporal-half-life-seconds)
                   (and (realp temporal-half-life-seconds)
                        (plusp temporal-half-life-seconds))))
    (error "Context graph search request is invalid"))
  (let ((tokens (%cg-query-tokens query)) (ranked nil))
    (maphash
     (lambda (ignored fact)
       (declare (ignore ignored))
       (when (and (or include-superseded-p (gethash "current" fact))
                  (%cg-evidence-policy-allows-p fact evidence-policy)
                  (%cg-claim-policy-allows-p fact claim-policy))
         (let* ((subject (gethash (gethash "subject_id" fact)
                                  (context-graph-entities graph)))
                (object (gethash (gethash "object_id" fact)
                                 (context-graph-entities graph)))
                (lexical-score
                  (%cg-token-score
                   tokens
                   (format nil "~a ~a ~a ~a"
                           (%cg-entity-text subject)
                           (gethash "predicate" fact)
                           (%cg-entity-text object)
                           (gethash "fact" fact))))
                (score (- (+ (* 10 lexical-score)
                             (* 10 (%cg-temporal-salience
                                    fact reference-time
                                    temporal-half-life-seconds)))
                          (if (string= "related_to" (gethash "predicate" fact))
                              generic-edge-penalty 0))))
           (when (plusp lexical-score)
             (push (%cg-search-row fact subject object score) ranked)))))
     (context-graph-facts graph))
    (setf ranked
          (sort ranked
                (lambda (left right)
                  (or (> (gethash "score" left) (gethash "score" right))
                      (and (= (gethash "score" left) (gethash "score" right))
                           (string< (gethash "fact_id" left)
                                    (gethash "fact_id" right)))))))
    (let ((selected (subseq ranked 0 (min maximum-results (length ranked)))))
      (%cg-object "schema_version" 1 "query" query
                  "claim_policy" claim-policy
                  "evidence_policy" evidence-policy
                  "result_count" (length selected)
                  "non_exhaustive" (> (length ranked) maximum-results)
                  "facts" (coerce selected 'vector)))))
