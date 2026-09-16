;;;; harness: bare
(require :asdf)
(unless (find-package :ql)
  (load (or (uiop:getenv "PAI_QUICKLISP_SETUP") "/opt/quicklisp/setup.lisp")))
(asdf:load-asd (pathname (or (uiop:getenv "PAI_CONTEXT_GRAPH_ASD")
                           (merge-pathnames "../pai-context-graph.asd" *load-truename*))))
(asdf:load-system :pai-context-graph)
(in-package :pai.context-graph)

(defun cgpr-check (name value)
  (unless value (error "FAIL ~a" name))
  (format t "PASS ~a~%" name))

(let* ((proposal
         (%cg-object
          "entities"
          (vector
           (%cg-object "local_ref" "speaker-a" "kind" "person"
                       "label" "The operator" "aliases" #()
                       "classifications" #( "operator")
                       "identity_action" "NEW" "existing_node_id" :null)
           (%cg-object "local_ref" "speaker-b" "kind" "person"
                       "label" "Configured operator" "aliases" #()
                       "classifications" #( "operator")
                       "identity_action" "NEW" "existing_node_id" :null)
           (%cg-object "local_ref" "topic" "kind" "concept"
                       "label" "A topic" "aliases" #()
                       "classifications" #()
                       "identity_action" "NEW" "existing_node_id" :null))
          "relationships"
          (vector
           (%cg-object "subject_ref" "speaker-a" "predicate" "related_to"
                       "object_ref" "topic"
                       "grounding"
                       (%cg-object "attributed_to_ref" "speaker-b")))))
       (descriptors
         (vector (%cg-object "role" "operator" "kind" "person"
                             "label" "Configured operator"
                             "aliases" #( "operator")
                             "existing_node_id" "kgf:operator"))))
  (multiple-value-bind (normalized repairs)
      (context-graph-normalize-legacy-participants proposal descriptors)
    (let* ((entities (gethash "entities" normalized))
           (participant (aref entities 0))
           (relationship (aref (gethash "relationships" normalized) 0)))
      (cgpr-check "participant normalization collapses duplicate explicit roles"
             (= 2 (length entities)))
      (cgpr-check "participant normalization applies runtime identity authority"
             (and (= 1 (length repairs))
                  (string= "Configured operator"
                           (gethash "label" participant))
                  (string= "LINK_EXISTING"
                           (gethash "identity_action" participant))
                  (string= "kgf:operator"
                           (gethash "existing_node_id" participant))))
      (cgpr-check "participant normalization rewrites edges and attribution"
             (and (string= (gethash "local_ref" participant)
                           (gethash "subject_ref" relationship))
                  (string= (gethash "local_ref" participant)
                           (gethash "attributed_to_ref"
                                    (gethash "grounding" relationship))))))))

(let* ((proposal
         (%cg-object
          "entities"
          (vector
           (%cg-object "local_ref" "one" "kind" "person" "label" "Same"
                       "classifications" #() "identity_action" "NEW"
                       "existing_node_id" :null)
           (%cg-object "local_ref" "two" "kind" "person" "label" "Same"
                       "classifications" #() "identity_action" "NEW"
                       "existing_node_id" :null))
          "relationships" #()))
       (normalized
         (context-graph-normalize-legacy-participants
          proposal
          (vector (%cg-object "role" "operator" "kind" "person"
                              "label" "Same" "aliases" #()
                              "existing_node_id" :null)))))
  (cgpr-check "equal labels never grant participant merge authority"
         (= 2 (length (gethash "entities" normalized)))))
(let* ((ontology (%cg-object "entity_types" #("person" "object")
                    "edge_types" (vector (%cg-object "name" "uses"
                       "subject_types" #("person") "object_types" #("object")))))
       (graph (make-context-graph ontology))
       (checks 0))
  (labels ((check (name value)
             (unless value (error "FAIL ~a" name))
             (incf checks) (format t "PASS ~a~%" name))
           (entity (ref type name &optional id)
             (%cg-object "local_ref" ref "type" type "name" name "aliases" #()
                         "action" (if id "LINK_EXISTING" "NEW")
                         "existing_id" (or id :null)
                         "evidence_status" "direct" "evidence_note" "Fixture source."))
           (apply-one (id entities &optional (facts #()))
             (context-graph-apply-episode graph
               (%cg-object "episode_id" id "occurred_at" "2026-01-01"
                           "learned_at" "2026-01-01" "content" "Fixture source.")
               (%cg-object "entities" entities "facts" facts)
               :reuse-exact-identities-p nil)))
    (apply-one "a" (vector (entity "p" "person" "Shared name")))
    (apply-one "b" (vector (entity "p" "person" "Shared name")))
    (check "explicit NEW preserves same-name distinct people" (= 2 (context-graph-entity-count graph)))
    (let* ((set (context-graph-entity-candidates graph (entity "p" "person" "Shared name")))
           (rows (gethash "candidates" set))
           (id (gethash "entity_id" (aref rows 0))))
      (check "homonyms both reach resolver" (= 2 (length rows)))
      (apply-one "c" (vector (entity "p" "person" "A different reference" id)))
      (check "explicit reuse does not create duplicate" (= 2 (context-graph-entity-count graph)))
      (check "reuse retains alias" (find "A different reference"
          (gethash "aliases" (gethash id (context-graph-entities graph))) :test #'string=))
      (check "type incompatible link rejected"
        (handler-case (progn (apply-one "bad" (vector (entity "p" "object" "Thing" id))) nil)
          (error () t)))
      (apply-one "d" (vector (entity "p" "person" "Shared name" id)
                              (entity "o" "object" "Reference item"))
        (vector (%cg-object "subject_ref" "p" "predicate" "uses" "object_ref" "o"
                            "fact" "Source reports a use." "supersedes_fact_id" :null
                            "evidence_status" "direct" "evidence_note" "Reported, not independently verified.")))
      (let* ((compact (context-graph-compact-search graph "Reference item" :character-budget 1500))
             (text (shasht:write-json compact nil)))
        (check "compact retains qualification" (search "not independently verified" text))
        (check "compact serialized budget" (<= (length text) 1500))
        (check "compact excludes raw episode" (not (search "Fixture source" text))))
      (check "tight budget drops complete facts" (zerop (length (gethash "facts"
         (context-graph-compact-search graph "Reference item" :character-budget 256)))))
      (check "unknown query is empty" (zerop (length (gethash "facts"
         (context-graph-compact-search graph "zzzxxyyqq")))))
      (check "candidate limit explicitly reports incompleteness"
         (gethash "non_exhaustive"
           (context-graph-entity-candidates graph (entity "p" "person" "Shared name") :maximum 1)))))
  (format t "RESOLUTION-CORE ~d passed, 0 failed~%" checks))
