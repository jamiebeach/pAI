;;;; harness: bare
(require :asdf)
(unless (find-package :ql)
  (load (or (uiop:getenv "PAI_QUICKLISP_SETUP") "/opt/quicklisp/setup.lisp")))
(asdf:load-asd
 (pathname (or (uiop:getenv "PAI_CONTEXT_GRAPH_ASD")
               (merge-pathnames "../pai-context-graph.asd" *load-truename*))))
(asdf:load-system :pai-context-graph :force t)
(in-package :pai.context-graph)

(defvar *cgl-pass* 0)
(defvar *cgl-fail* 0)
(defun cgl-check (name thunk)
  (handler-case
      (if (funcall thunk)
          (progn (incf *cgl-pass*) (format t "PASS ~a~%" name))
          (progn (incf *cgl-fail*) (format t "FAIL ~a~%" name)))
    (error (condition)
      (incf *cgl-fail*) (format t "FAIL ~a: ~a~%" name condition))))
(defun cgl-error-p (thunk)
  (handler-case (progn (funcall thunk) nil) (error () t)))
(defun cgl-graph ()
  (make-context-graph
   (%cg-object
    "entity_types" #( "person" "concept" "event")
    "edge_types"
    (vector
     (%cg-object "name" "uses" "subject_types" #( "person")
                 "object_types" #( "concept"))
     (%cg-object "name" "related_to" "subject_types"
                 #( "person" "concept" "event") "object_types"
                 #( "person" "concept" "event"))))))
(defun cgl-entity (ref type name &optional (action "NEW") existing)
  (%cg-object "local_ref" ref "type" type "name" name "aliases" #()
              "action" action "existing_id" (or existing :null)
              "evidence_status" "direct" "evidence_note" "Exact fixture source."))
(defun cgl-temporal (character occurred)
  (%cg-object "schema_version" 1 "character" character
              "occurred_at" occurred "valid_from" occurred "valid_until" :null))
(defun cgl-apply (graph id text object-name &key (scope "assertion")
                    (predicate "uses") (character "standing-disposition"))
  (let* ((source (%cg-object "source_id" (format nil "source:~a" id)
                             "speaker_id" "speaker:operator"
                             "kind" "original-utterance" "text" text
                             "text_sha256" (%cg-sha256 text)))
         (packet (%cg-object "schema_version" 1 "sources" (vector source)))
         (episode (%cg-object "episode_id" id "occurred_at" "2026-01-01T00:00:00Z"
                              "learned_at" "2026-01-02T00:00:00Z" "content" text))
         (proposal
           (%cg-object
            "entities" (vector (cgl-entity "p" "person" "Operator")
                               (cgl-entity "o" (if (string= predicate "uses")
                                                   "concept" "event") object-name))
            "facts" (vector
                     (%cg-object "subject_ref" "p" "predicate" predicate
                                 "object_ref" "o" "fact" text
                                 "supersedes_fact_id" :null
                                 "evidence_status" "direct"
                                 "evidence_note" "Exact fixture source."
                                 "temporal" (cgl-temporal
                                             character "2026-01-01T00:00:00Z")
                                 "grounding"
                                 (%cg-object "schema_version" 1 "scope" scope
                                             "polarity" "positive"
                                             "attributed_to_ref" "p"
                                             "evidence"
                                             (vector (%cg-object
                                                      "source_id" (format nil "source:~a" id)
                                                      "quote" text))))))))
    (context-graph-apply-episode graph episode proposal :source-packet packet)))
(defun cgl-correction (id target replacement action &optional source-episode-id)
  (%cg-object "schema_version" 1 "correction_id" id
              "observed_at" "2026-02-01T00:00:00Z" "action" action
              "target_fact_id" target "replacement_fact_id" (or replacement :null)
              "reason" "The operator corrected the earlier graph claim."
              "source_episode_id"
              (or source-episode-id (if replacement "new" "old"))))

(cgl-check "joke scope preserves the utterance without asserting its premise"
  (lambda ()
    (let ((graph (cgl-graph)))
      (cgl-apply graph "joke" "I joked that this should be secret."
                 "a secret" :scope "joke")
      (and (zerop (gethash "result_count" (context-graph-search graph "secret")))
           (= 1 (gethash "result_count"
                         (context-graph-search graph "secret" :claim-policy "all")))))))

(cgl-check "proposal scope is not promoted to an established fact"
  (lambda ()
    (let ((graph (cgl-graph)))
      (cgl-apply graph "proposal" "I propose using a layered retrieval model."
                 "layered retrieval model" :scope "proposal")
      (zerop (gethash "result_count" (context-graph-search graph "layered"))))))

(cgl-check "semantic time is retained independently from observation time"
  (lambda ()
    (let ((graph (cgl-graph)))
      (cgl-apply graph "trip" "The operator attended an outing."
                 "outing" :predicate "related_to" :character "event"
                 )
      (let* ((row (aref (gethash "facts" (context-graph-search graph "outing")) 0))
             (temporal (gethash "temporal" row)))
        (and (string= "event" (gethash "character" temporal))
             (string= "2026-01-01T00:00:00Z" (gethash "occurred_at" temporal))
             (string= "2026-01-02T00:00:00Z" (gethash "observed_at" row)))))))

(cgl-check "generic and temporal penalties change ranking score but not truth"
  (lambda ()
    (let ((graph (cgl-graph)))
      (cgl-apply graph "old" "The operator attended an outing."
                 "outing" :predicate "related_to" :character "event"
                 )
      (let* ((unpenalized (aref (gethash "facts"
                                  (context-graph-search graph "outing"
                                                        :generic-edge-penalty 0)) 0))
             (penalized (aref (gethash "facts"
                                (context-graph-search graph "outing"
                                  :reference-time "2026-01-11T00:00:00Z"
                                  :temporal-half-life-seconds 86400)) 0)))
        (and (> (gethash "score" unpenalized) (gethash "score" penalized))
             (= 1 (context-graph-fact-count graph))
             (gethash "current" (gethash (gethash "fact_id" penalized)
                                         (context-graph-facts graph))))))))

(cgl-check "correction supersedes an old claim while retaining history"
  (lambda ()
    (let ((graph (cgl-graph)))
      (cgl-apply graph "old" "The operator uses the first plan." "first plan")
      (let ((old-id (gethash "fact_id" (aref (gethash "facts"
                          (context-graph-search graph "first plan")) 0))))
        (cgl-apply graph "new" "The operator uses the corrected plan." "corrected plan")
        (let* ((new-id (gethash "fact_id" (aref (gethash "facts"
                              (context-graph-search graph "corrected plan")) 0)))
               (correction (cgl-correction "correction:1" old-id new-id "supersede")))
          (context-graph-apply-correction graph correction)
          (let ((current (context-graph-search graph "first plan"))
                (history (context-graph-search graph "first plan"
                                               :include-superseded-p t)))
          (and (not (find old-id (gethash "facts" current)
                          :key (lambda (row) (gethash "fact_id" row))
                          :test #'string=))
               (find old-id (gethash "facts" history)
                     :key (lambda (row) (gethash "fact_id" row))
                     :test #'string=)
               (= 1 (gethash "correction_count" (context-graph-snapshot graph)))
               (string= "already-applied"
                        (gethash "status"
                                 (context-graph-apply-correction graph correction))))))))))

(cgl-check "invalid correction fails without partial mutation"
  (lambda ()
    (let ((graph (cgl-graph)))
      (cgl-apply graph "old" "The operator uses the first plan." "first plan")
      (let* ((row (aref (gethash "facts" (context-graph-search graph "first")) 0))
             (id (gethash "fact_id" row)))
        (and (cgl-error-p (lambda ()
                 (context-graph-apply-correction
                  graph (cgl-correction "bad" id "missing" "supersede"))))
             (gethash "current" (gethash id (context-graph-facts graph)))
             (zerop (hash-table-count (context-graph-corrections graph))))))))

(cgl-check "withdrawal closes a grounded fact without deleting history"
  (lambda ()
    (let ((graph (cgl-graph)))
      (cgl-apply graph "old" "The operator uses the first plan." "first plan")
      (let* ((row (aref (gethash "facts" (context-graph-search graph "first")) 0))
             (id (gethash "fact_id" row)))
        (context-graph-apply-correction
         graph (cgl-correction "withdraw:1" id nil "withdraw" "old"))
        (and (zerop (gethash "result_count"
                             (context-graph-search graph "first")))
             (= 1 (gethash "result_count"
                           (context-graph-search graph "first"
                                                 :include-superseded-p t)))
             (= 1 (context-graph-fact-count graph)))))))

(cgl-check "correction requires an existing grounded source episode"
  (lambda ()
    (let ((graph (cgl-graph)))
      (cgl-apply graph "old" "The operator uses the first plan." "first plan")
      (let* ((row (aref (gethash "facts" (context-graph-search graph "first")) 0))
             (id (gethash "fact_id" row)))
        (and (cgl-error-p
              (lambda ()
                (context-graph-apply-correction
                 graph (cgl-correction "withdraw:bad" id nil "withdraw"
                                       "missing-source"))))
             (gethash "current" (gethash id (context-graph-facts graph)))
             (zerop (hash-table-count (context-graph-corrections graph))))))))

(cgl-check "reassessment nominates generic edges and isolated nodes without mutation"
  (lambda ()
    (let ((graph (cgl-graph)))
      (cgl-apply graph "edge" "The operator attended an outing."
                 "outing" :predicate "related_to" :character "event")
      (context-graph-apply-episode
       graph (%cg-object "episode_id" "isolated" "occurred_at" "2026-01-01"
                         "learned_at" "2026-01-01" "content" "A loose mention.")
       (%cg-object "entities" (vector (cgl-entity "x" "concept" "Loose mention"))
                   "facts" #()))
      (let* ((before (shasht:write-json (context-graph-snapshot graph) nil))
             (result (context-graph-reassessment-candidates graph))
             (reasons (map 'list (lambda (row) (gethash "reason" row))
                           (gethash "candidates" result))))
        (and (find "generic-edge" reasons :test #'string=)
             (find "isolated-node" reasons :test #'string=)
             (string= before (shasht:write-json (context-graph-snapshot graph) nil)))))))

(cgl-check "reassessment offset rotates a bounded queue"
  (lambda ()
    (let ((graph (cgl-graph)))
      (context-graph-apply-episode
       graph (%cg-object "episode_id" "isolated" "occurred_at" "2026-01-01"
                         "learned_at" "2026-01-01" "content" "Loose mentions.")
       (%cg-object "entities"
                   (vector (cgl-entity "a" "concept" "First loose mention")
                           (cgl-entity "b" "concept" "Second loose mention"))
                   "facts" #()))
      (let ((first (context-graph-reassessment-candidates graph :maximum 1 :offset 0))
            (second (context-graph-reassessment-candidates graph :maximum 1 :offset 1)))
        (not (string= (gethash "target_id" (aref (gethash "candidates" first) 0))
                      (gethash "target_id" (aref (gethash "candidates" second) 0))))))))

(cgl-check "classification is stored separately from identity aliases"
  (lambda ()
    (let* ((graph (cgl-graph))
           (entity (cgl-entity "x" "concept" "Field notebook")))
      (setf (gethash "classifications" entity) #( "stationery"))
      (context-graph-apply-episode
       graph (%cg-object "episode_id" "classification" "occurred_at" "2026-01-01"
                         "learned_at" "2026-01-01" "content" "A field notebook.")
       (%cg-object "entities" (vector entity) "facts" #()))
      (let ((stored (aref (gethash "entities" (context-graph-snapshot graph)) 0)))
        (and (find "stationery" (gethash "classifications" stored) :test #'string=)
             (not (find "stationery" (gethash "aliases" stored) :test #'string=)))))))

(cgl-check "explicit compatible types widen candidates without merging them"
  (lambda ()
    (let ((graph (cgl-graph)))
      (context-graph-apply-episode
       graph (%cg-object "episode_id" "facets" "occurred_at" "2026-01-01"
                         "learned_at" "2026-01-01" "content" "Two role facets.")
       (%cg-object "entities"
                   (vector (cgl-entity "p" "person" "Shared identity")
                           (cgl-entity "c" "concept" "Shared identity"))
                   "facts" #()) :reuse-exact-identities-p nil)
      (let* ((descriptor (%cg-object "local_ref" "incoming" "type" "person"
                                     "name" "Shared identity" "aliases" #()))
             (exact (context-graph-entity-candidates graph descriptor))
             (wide (context-graph-entity-candidates
                    graph descriptor :compatible-types #( "concept")))
             (rows (gethash "candidates" wide)))
        (and (= 1 (gethash "eligible_count" exact))
             (= 2 (gethash "eligible_count" wide))
             (find "compatible" rows :test #'string=
                   :key (lambda (row) (gethash "type_compatibility" row)))
             (= 2 (context-graph-entity-count graph)))))))

(format t "~%Context graph lifecycle: ~d passed, ~d failed.~%"
        *cgl-pass* *cgl-fail*)
(when (plusp *cgl-fail*) (uiop:quit 1))
