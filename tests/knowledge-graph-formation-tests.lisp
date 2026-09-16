;;;; knowledge-graph-formation-tests.lisp -- pure KG2 contract fixtures.

(in-package :agent)

(ql:quickload '(:ironclad) :silent t)
(load (test-source "knowledge-graph-ontology.lisp"))
(load (test-source "knowledge-graph-formation.lisp"))

(defvar *kgf-passed* 0)
(defvar *kgf-failed* 0)

(defun kgf-check (name condition)
  (if condition
      (progn (incf *kgf-passed*) (format t "PASS ~a~%" name))
      (progn (incf *kgf-failed*) (format t "FAIL ~a~%" name))))

(defun kgf-entity (ref label &key (kind "person")
                   (action "NEW") (existing :null) (aliases #())
                   (classifications #()))
  (obj "local_ref" ref "kind" kind "label" label "aliases" aliases
       "classifications" classifications
       "identity_action" action "existing_node_id" existing
       "evidence_status" "direct" "evidence_note" "fixture evidence"))

(defun kgf-relation (from predicate to &optional (action "ASSERT"))
  (obj "subject_ref" from "predicate" predicate "object_ref" to
       "relationship_action" action
       "fact" (format nil "~a ~a ~a" from predicate to)
       "grounding"
       (obj "schema_version" 1 "scope" "assertion" "polarity" "positive"
            "attributed_to_ref" :null
            "evidence" (vector (obj "source_id" "event:fixture"
                                    "quote" "Fixture directly states the relationship.")))
       "temporal"
       (obj "schema_version" 1 "character" "unspecified"
            "occurred_at" :null "valid_from" :null "valid_until" :null)
       "evidence_status" "direct" "evidence_note" "fixture evidence"))

(defun kgf-event (id entities relationships &key (eligible #())
                  (persona "fixture") (agent "kg-fixture"))
  (obj "id" id "type" "knowledge-graph-formation-sealed"
       "agent_id" agent "timestamp" (+ 4000000000 id)
       "payload"
       (obj "schema_version" 1 "persona_id" persona
            "disclosure_class" "private"
            "formation_revision" *knowledge-graph-formation-revision*
            "source_event_ids" (vector (+ 100 id))
            "source_memory_node_ids" (vector (format nil "memory:~d" id))
            "source_episode_ids" (vector (format nil "episode:~d" id))
            "source_evidence"
            (vector (obj "source_id" "event:fixture" "speaker_id" "operator"
                         "kind" "original-utterance" "timestamp" 4000000000
                         "text" "Fixture directly states the relationship."
                         "text_sha256"
                         (%kgf-sha256 "Fixture directly states the relationship.")))
            "eligible_existing_node_ids" eligible
            "proposal"
            (obj "schema_version" 3
                 "ontology_revision" *knowledge-graph-ontology-revision*
                 "entities" entities
                 "relationships" relationships))))

(defun kgf-json (value)
  (shasht:write-json value nil))

(defun kgf-v2-entity (ref kind label status)
  (let ((entity (kgf-entity ref label :kind kind)))
    (setf (gethash "evidence_status" entity) status)
    entity))

(defun kgf-v2-relation (from predicate to status)
  (let ((relationship (kgf-relation from predicate to)))
    (setf (gethash "evidence_status" relationship) status)
    relationship))

(defun kgf-v2-event (id entities relationships)
  (obj "id" id "type" "knowledge-graph-formation-sealed"
       "agent_id" "kg-fixture" "timestamp" (+ 4000000000 id)
       "payload"
       (obj "schema_version" 1 "persona_id" "fixture"
            "disclosure_class" "private"
            "formation_revision" *knowledge-graph-formation-revision*
            "source_event_ids" (vector (+ 100 id))
            "source_memory_node_ids" #()
            "source_episode_ids" (vector (format nil "episode:~d" id))
            "source_evidence"
            (vector (obj "source_id" "event:fixture" "speaker_id" "operator"
                         "kind" "original-utterance" "timestamp" 4000000000
                         "text" "Fixture directly states the relationship."
                         "text_sha256"
                         (%kgf-sha256 "Fixture directly states the relationship.")))
            "eligible_existing_node_ids" #()
            "proposal"
            (obj "schema_version" 3
                 "ontology_revision" *knowledge-graph-ontology-revision*
                 "entities" entities "relationships" relationships))))

(format t "~%== KG2 generic formation and identity ==~%")

(let* ((event
         (kgf-v2-event
          5
          (vector (kgf-v2-entity "fixtureoperator" "person" "FixtureOperator" "direct")
                  (kgf-v2-entity "vision" "condition"
                                 "seasonal pollen sensitivity" "direct"))
          (vector (kgf-v2-relation "fixtureoperator" "has_condition" "vision"
                                   "direct"))))
       (state (knowledge-graph-formation-project
               (list event) "kg-fixture" "fixture")))
  (kgf-check "v3 grounding and evidence status survive canonical replay"
             (and (= 2 (length (gethash "nodes" state)))
                  (= 1 (length (gethash "edges" state)))
                  (every (lambda (row)
                           (string= "direct"
                                    (gethash "evidence_status" row "")))
                         (gethash "nodes" state))
                  (string= "direct"
                           (gethash "evidence_status"
                                    (aref (gethash "edges" state) 0) "")))))

(let* ((invalid-kind
         (kgf-v2-event 6 (vector (kgf-v2-entity "x" "plant" "Fern" "direct"))
                       #()))
       (invalid-signature
         (kgf-v2-event
          7
          (vector (kgf-v2-entity "place" "place" "Campground" "direct")
                  (kgf-v2-entity "condition" "condition" "Anemia" "direct"))
          (vector (kgf-v2-relation "place" "has_condition" "condition"
                                   "direct")))))
  (kgf-check "v3 rejects undeclared upper kinds"
             (not (knowledge-graph-formation-sealed-payload-valid-p
                   (gethash "payload" invalid-kind))))
  (kgf-check "v3 rejects invalid typed predicate signatures"
             (not (knowledge-graph-formation-sealed-payload-valid-p
                   (gethash "payload" invalid-signature)))))

(let* ((first (kgf-event 10 (vector (kgf-entity "j" "Jordan")) #()))
       (second (kgf-event 11 (vector (kgf-entity "j" "Jordan")) #()))
       (state (knowledge-graph-formation-project
               (list first second) "kg-fixture" "fixture"))
       (nodes (gethash "nodes" state)))
  (kgf-check "same label does not silently merge identities"
             (and (= 2 (length nodes))
                  (not (string= (gethash "node_id" (aref nodes 0))
                                (gethash "node_id" (aref nodes 1)))))))

(let* ((participant-id
         (%kgf-participant-id "kg-fixture" "fixture" "operator"))
       (first
         (kgf-event
          12
          (vector (kgf-entity "operator" "FixtureOperator"
                              :classifications #("operator")))
          #()))
       (second
         (kgf-event
          13
          (vector (kgf-entity "operator" "FixtureOperator"
                              :action "LINK_EXISTING"
                              :existing participant-id
                              :classifications #("operator")))
          #() :eligible (vector participant-id)))
       (state (knowledge-graph-formation-project
               (list first second) "kg-fixture" "fixture"))
       (nodes (gethash "nodes" state)))
  (kgf-check "known operator has one stable runtime-owned identity"
             (and (= 1 (length nodes))
                  (string= participant-id
                           (gethash "node_id" (aref nodes 0)))
                  (string= "operator"
                           (gethash "participant_role" (aref nodes 0)))))
  (handler-case
      (progn
        (knowledge-graph-formation-project
         (list first
               (kgf-event
                14
                (vector (kgf-entity "operator" "FixtureOperator again"
                                    :classifications #("operator")))
                #()))
         "kg-fixture" "fixture")
        (kgf-check "second operator NEW identity fails closed" nil))
    (error ()
      (kgf-check "second operator NEW identity fails closed" t))))

(let* ((participant-id
         (%kgf-participant-id "kg-fixture" "fixture" "active-persona"))
       (state
         (knowledge-graph-formation-project
          (list
           (kgf-event
            15
            (vector (kgf-entity "persona" "FixtureAgent" :kind "agent"
                                :classifications #("active-persona")))
            #()))
          "kg-fixture" "fixture"))
       (node (aref (gethash "nodes" state) 0)))
  (kgf-check "active persona also receives one stable runtime-owned identity"
             (and (string= participant-id (gethash "node_id" node))
                  (string= "active-persona"
                           (gethash "participant_role" node)))))

(let* ((seed (kgf-event 20 (vector (kgf-entity "operator" "Operator")) #()))
       (seed-state (knowledge-graph-formation-project
                    (list seed) "kg-fixture" "fixture"))
       (operator-id (gethash "node_id" (aref (gethash "nodes" seed-state) 0)))
       (linked
         (kgf-event
          21
          (vector (kgf-entity "operator" "Operator"
                              :action "LINK_EXISTING" :existing operator-id)
                  (kgf-entity "need" "Color-accessible visual encoding"
                              :kind "concept"))
          (vector (kgf-relation "operator" "related_to" "need"))
          :eligible (vector operator-id)))
       (full (knowledge-graph-formation-project
              (list seed linked) "kg-fixture" "fixture"))
       (folded (knowledge-graph-formation-fold
                seed-state linked "kg-fixture" "fixture")))
  (kgf-check "explicit eligible link reuses one existing identity"
             (= 2 (length (gethash "nodes" full))))
  (kgf-check "relationship assertion links supplied local references"
             (let ((edge (aref (gethash "edges" full) 0)))
               (and (string= "related_to" (gethash "predicate" edge))
                    (string= operator-id (gethash "from_node_id" edge)))))
  (kgf-check "one-event fold is canonical-full-replay equivalent"
             (string= (kgf-json full) (kgf-json folded)))
  (let* ((materialization
           (knowledge-graph-formation-materialization full))
         (node-row (aref (gethash "nodes" materialization) 0))
         (payload (shasht:read-json
                   (gethash "payload_json" node-row))))
    (kgf-check "materialization emits independent generic graph partition"
               (and (string= "grounded-knowledge-graph"
                             (gethash "projection_name" materialization))
                    (= 2 (length (gethash "nodes" materialization)))
                    (= 1 (length (gethash "edges" materialization)))
                    (string= (gethash "node_id" node-row)
                             (gethash "node_id" payload))))
    (kgf-check "materialization preserves descriptor and source evidence"
               (= 6 (length (gethash "evidence" materialization)))))
  (let* ((edge (aref (gethash "edges" full) 0))
         (descriptor (gethash "descriptor_event_id" edge)))
    ;; A provider can repeat the same assertion within one sealed proposal.
    ;; The fold treats the second occurrence as reinforcement, so descriptor
    ;; and reinforcement may intentionally name the same formation event.
    (setf (gethash "reinforcement_event_ids" edge) (vector descriptor))
    (let* ((materialization
             (knowledge-graph-formation-materialization full))
           (rows (coerce (gethash "evidence" materialization) 'list))
           (identities
             (mapcar (lambda (row)
                       (list (gethash "owner_kind" row)
                             (gethash "owner_id" row)
                             (gethash "evidence_event_id" row)
                             (gethash "evidence_role" row)))
                     rows)))
      (kgf-check "duplicate descriptor reinforcement has one storage identity"
                 (= (length identities)
                    (length (remove-duplicates identities :test #'equal))))
      (kgf-check "duplicate descriptor reinforcement retains lowest ordinal"
                 (zerop
                  (gethash
                   "evidence_ordinal"
                   (find descriptor rows
                         :key (lambda (row)
                                (and (string= "edge"
                                              (gethash "owner_kind" row ""))
                                     (string= "descriptor"
                                              (gethash "evidence_role" row ""))
                                     (gethash "evidence_event_id" row)))))))))
  (let* ((need-id
           (gethash "node_id"
                    (find "concept" (coerce (gethash "nodes" full) 'list)
                          :key (lambda (row) (gethash "node_kind" row ""))
                          :test #'string=)))
         (retirement
           (kgf-event
            24
            (vector (kgf-entity "operator" "Operator"
                                :action "LINK_EXISTING"
                                :existing operator-id)
                    (kgf-entity "need" "Color-accessible visual encoding"
                                :kind "concept"
                                :action "LINK_EXISTING" :existing need-id))
            (vector (kgf-relation "operator" "related_to" "need" "RETIRE"))
            :eligible (vector operator-id need-id)))
         (retired (knowledge-graph-formation-fold
                   full retirement "kg-fixture" "fixture"))
         (edge (find "related_to" (coerce (gethash "edges" retired) 'list)
                     :key (lambda (row) (gethash "predicate" row ""))
                     :test #'string=)))
    (kgf-check "relationship retirement preserves the edge and closes validity"
               (and (string= "retired" (gethash "status" edge))
                    (integerp (gethash "valid_to" edge)))))
  (handler-case
      (progn
        (knowledge-graph-formation-project
         (list seed
               (kgf-event
                22
                (vector (kgf-entity "x" "Foreign"
                                    :action "LINK_EXISTING"
                                    :existing "kgf:entity:invented"))
                #() :eligible (vector "kgf:entity:invented")))
         "kg-fixture" "fixture")
        (kgf-check "invented eligible-looking ID fails closed" nil))
    (error ()
      (kgf-check "invented eligible-looking ID fails closed" t)))
  (let* ((revision
           (kgf-event
            23
            (vector (kgf-entity "operator" "Operator, corrected"
                                :action "REVISE_EXISTING"
                                :existing operator-id))
            #() :eligible (vector operator-id)))
         (revised (knowledge-graph-formation-project
                   (list seed revision) "kg-fixture" "fixture"))
         (rows (coerce (gethash "nodes" revised) 'list))
         (old (find operator-id rows :key (lambda (row)
                                            (gethash "node_id" row))
                    :test #'string=))
         (new (find operator-id rows :key (lambda (row)
                                            (gethash "node_id" row))
                    :test-not #'string=)))
    (kgf-check "correction retains old node with deterministic temporal close"
               (and (= 2 (length rows))
                    (string= "superseded" (gethash "status" old))
                    (integerp (gethash "valid_to" old))))
    (kgf-check "correction creates current replacement and lineage"
               (and new
                    (string= "current" (gethash "status" new))
                    (string= operator-id
                             (gethash "supersedes_node_id" new))))))

(let* ((foreign (kgf-event 30 (vector (kgf-entity "x" "Other")) #()
                           :persona "other"))
       (state (knowledge-graph-formation-project
               (list foreign) "kg-fixture" "fixture")))
  (kgf-check "foreign persona events do not enter the partition"
             (zerop (length (gethash "nodes" state)))))

(let* ((valid (kgf-event 40 (vector (kgf-entity "x" "Exact")) #()))
       (payload (gethash "payload" valid)))
  (setf (gethash "model_authored_node_id" payload) "forbidden")
  (kgf-check "unknown sealed keys fail the closed contract"
             (not (knowledge-graph-formation-sealed-payload-valid-p payload))))

(let ((underscored
        (kgf-event
         41
         (vector (kgf-entity "operator" "Operator" :kind "person")
                 (kgf-entity "need" "Accessible palette"
                             :kind "concept"))
         (vector (kgf-relation "operator" "related_to" "need")))))
  (kgf-check "declared semantic tokens may use underscores"
             (knowledge-graph-formation-sealed-payload-valid-p
              (gethash "payload" underscored))))

(let* ((bad-action
         (kgf-event 41 (vector (kgf-entity "x" "Bad" :action 7)) #()))
       (bad-schema
         (kgf-event 42 (vector (kgf-entity "x" "Bad")) #())))
  (setf (gethash "schema_version" (gethash "payload" bad-schema)) "one")
  (kgf-check "wrong provider field types reject without signalling"
             (and
              (handler-case
                  (not (knowledge-graph-formation-sealed-payload-valid-p
                        (gethash "payload" bad-action)))
                (error () nil))
              (handler-case
                  (not (knowledge-graph-formation-sealed-payload-valid-p
                        (gethash "payload" bad-schema)))
                (error () nil)))))

(let* ((duplicate
         (kgf-event 43
                    (vector (kgf-entity "same" "First")
                            (kgf-entity "same" "Second")) #())))
  (kgf-check "duplicate local references fail the closed contract"
             (not (knowledge-graph-formation-sealed-payload-valid-p
                   (gethash "payload" duplicate)))))

(format t "~%KG2 formation: ~d passed, ~d failed.~%"
        *kgf-passed* *kgf-failed*)
(when (plusp *kgf-failed*) (uiop:quit 1))
