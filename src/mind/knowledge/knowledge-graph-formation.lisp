;;;; knowledge-graph-formation.lisp -- pure KG2 formation and identity.
;;;;
;;;; Sealed ledger events are authority.  This module validates their closed
;;;; semantic contract and deterministically derives identity, temporal state,
;;;; correction lineage and evidence.  It performs no IO or provider calls.

(in-package :agent)

(export '(knowledge-graph-formation-sealed-payload-valid-p
          knowledge-graph-formation-project
          knowledge-graph-formation-fold
          knowledge-graph-formation-materialization))

(defparameter *knowledge-graph-formation-revision*
  "grounded-knowledge-graph-formation-v5")
(defparameter *knowledge-graph-formation-projection-name*
  "grounded-knowledge-graph")

(defparameter *knowledge-graph-claim-scopes*
  '("assertion" "question" "hypothesis" "intention" "reported-speech"
    "joke" "retrieval-outcome"))

(defparameter *knowledge-graph-temporal-characters*
  '("event" "temporary-state" "ongoing-state" "standing-disposition"
    "timeless" "unspecified"))

;; Current episode sealing is smaller, but the dev ledger contains earlier
;; valid episodes with as many as 100 short public messages. Reformation keeps
;; their exact evidence complete under an explicit historical migration bound.
(defparameter *knowledge-graph-formation-maximum-source-evidence-records* 128)
(defparameter *knowledge-graph-formation-maximum-source-evidence-characters*
  70000)

(defun %kgf-items (value)
  (cond ((null value) nil)
        ((vectorp value) (coerce value 'list))
        ((listp value) value)
        (t nil)))

(defun %kgf-exact-keys-p (table expected)
  (and (hash-table-p table)
       (= (hash-table-count table) (length expected))
       (every (lambda (key) (nth-value 1 (gethash key table))) expected)))

(defun %kgf-required-string-p (value &optional (maximum 240))
  (and (stringp value) (plusp (length value)) (<= (length value) maximum)))

(defun %kgf-token-p (value)
  (and (%kgf-required-string-p value 80)
       (every (lambda (character)
                (or (and (char>= character #\a) (char<= character #\z))
                    (digit-char-p character)
                    (find character "_-.:" :test #'char=)))
              value)))

(defun %kgf-distinct-vector-p (value predicate &key (maximum 64))
  (and (vectorp value) (<= (length value) maximum)
       (every predicate value)
       (= (length value)
          (length (remove-duplicates (coerce value 'list) :test #'equal)))))

(defun %kgf-null-p (value)
  (or (null value) (eq value :null)))

(defun %kgf-sha256 (text)
  (string-downcase
   (ironclad:byte-array-to-hex-string
    (ironclad:digest-sequence
     :sha256 (sb-ext:string-to-octets text :external-format :utf-8)))))

(defun %kgf-evidence-status-p (value)
  (and (stringp value)
       (member value '("direct" "prior-graph" "inference" "unreviewed")
               :test #'string=)))

(defun %kgf-evidence-rank (status)
  (or (position status '("unreviewed" "inference" "prior-graph" "direct")
                :test #'string=)
      0))

(defun %kgf-row-evidence-status (row)
  (gethash "evidence_status" row "unreviewed"))

(defun %kgf-row-evidence-note (row)
  (gethash "evidence_note" row "legacy formation was not evidence-reviewed"))

(defun %kgf-participant-role (entity)
  "Return the explicit runtime participant role carried by ENTITY, if any."
  (let ((classes (%kgf-items (gethash "classifications" entity #()))))
    (cond ((find "operator" classes :test #'string-equal) "operator")
          ((find "active-persona" classes :test #'string-equal)
           "active-persona")
          (t nil))))

(defun %kgf-participant-id (agent-id persona-id role)
  (format nil "kgf:entity:~a"
          (%kgf-sha256 (format nil "participant|~a|~a|~a"
                               agent-id persona-id role))))

(defun %kgf-current-participant (nodes persona-id role)
  (let ((matches nil))
    (maphash
     (lambda (ignored node)
       (declare (ignore ignored))
       (when (and (string= persona-id (gethash "persona_id" node ""))
                  (string= "current" (gethash "status" node ""))
                  (string= role (gethash "participant_role" node "")))
         (push node matches)))
     nodes)
    (when (> (length matches) 1)
      (error "KG participant identity is not unique"))
    (first matches)))

(defun %kgf-evidence-fields-valid-p (row schema-version)
  (if (= schema-version 1)
      t
      (and (%kgf-evidence-status-p (gethash "evidence_status" row))
           (%kgf-required-string-p (gethash "evidence_note" row) 600))))

(defun %kgf-entity-valid-p (entity eligible-ids schema-version)
  (and (%kgf-exact-keys-p
        entity (if (= schema-version 1)
                   '("local_ref" "kind" "label" "aliases"
                     "identity_action" "existing_node_id")
                   (if (= schema-version 2)
                       '("local_ref" "kind" "label" "aliases"
                         "identity_action" "existing_node_id"
                         "evidence_status" "evidence_note")
                       '("local_ref" "kind" "label" "aliases"
                         "classifications" "identity_action"
                         "existing_node_id" "evidence_status"
                         "evidence_note"))))
       (%kgf-required-string-p (gethash "local_ref" entity) 80)
       (%kgf-token-p (gethash "kind" entity))
       (or (= schema-version 1)
           (knowledge-graph-ontology-kind-p (gethash "kind" entity)))
       (%kgf-required-string-p (gethash "label" entity))
       (%kgf-distinct-vector-p
        (gethash "aliases" entity)
        (lambda (value) (%kgf-required-string-p value)) :maximum 8)
       (or (< schema-version 3)
           (%kgf-distinct-vector-p
            (gethash "classifications" entity)
            (lambda (value) (%kgf-required-string-p value 120)) :maximum 8))
       (let ((action (gethash "identity_action" entity))
             (existing (gethash "existing_node_id" entity)))
         (and (stringp action)
          (cond
           ((string= action "NEW") (%kgf-null-p existing))
           ((member action '("LINK_EXISTING" "REVISE_EXISTING")
                    :test #'string=)
            (and (%kgf-required-string-p existing 160)
                 (find existing eligible-ids :test #'string=)))
           (t nil))))
       (%kgf-evidence-fields-valid-p entity schema-version)))

(defun %kgf-grounding-valid-p (grounding refs source-evidence)
  (and (%kgf-exact-keys-p
        grounding '("schema_version" "scope" "polarity"
                    "attributed_to_ref" "evidence"))
       (= 1 (gethash "schema_version" grounding -1))
       (member (gethash "scope" grounding) *knowledge-graph-claim-scopes*
               :test #'string=)
       (member (gethash "polarity" grounding)
               '("positive" "negative" "unknown") :test #'string=)
       (let ((attributed (gethash "attributed_to_ref" grounding)))
         (or (%kgf-null-p attributed)
             (find attributed refs :test #'string=)))
       (%kgf-distinct-vector-p
        (gethash "evidence" grounding)
        (lambda (item)
          (and (%kgf-exact-keys-p item '("source_id" "quote"))
               (%kgf-required-string-p (gethash "source_id" item) 180)
               (%kgf-required-string-p (gethash "quote" item) 1000)
               (let ((source
                       (find (gethash "source_id" item)
                             (%kgf-items source-evidence) :test #'string=
                             :key (lambda (row) (gethash "source_id" row "")))))
                 (and source
                      (search (gethash "quote" item)
                              (gethash "text" source "") :test #'char=)))))
        :maximum 4)))

(defun %kgf-temporal-valid-p (temporal)
  (and (%kgf-exact-keys-p
        temporal '("schema_version" "character" "occurred_at"
                   "valid_from" "valid_until"))
       (= 1 (gethash "schema_version" temporal -1))
       (member (gethash "character" temporal)
               *knowledge-graph-temporal-characters* :test #'string=)
       (every (lambda (key)
                (let ((value (gethash key temporal)))
                  (or (%kgf-null-p value)
                      (%kgf-required-string-p value 80))))
              '("occurred_at" "valid_from" "valid_until"))))

(defun %kgf-source-evidence-row-valid-p (row)
  (and (%kgf-exact-keys-p
        row '("source_id" "speaker_id" "kind" "timestamp" "text"
              "text_sha256"))
       (%kgf-required-string-p (gethash "source_id" row) 180)
       (%kgf-required-string-p (gethash "speaker_id" row) 180)
       (member (gethash "kind" row)
               '("original-utterance" "prior-agent-utterance")
               :test #'string=)
       (or (integerp (gethash "timestamp" row))
           (%kgf-required-string-p (gethash "timestamp" row) 80))
       (%kgf-required-string-p (gethash "text" row) 30000)
       (let ((digest (gethash "text_sha256" row)))
         (and (stringp digest) (= 64 (length digest))
              (string= digest (%kgf-sha256 (gethash "text" row)))))))

(defun %kgf-relationship-valid-p
    (relationship refs kinds schema-version source-evidence)
  (and (%kgf-exact-keys-p
        relationship
        (cond
          ((= schema-version 1)
           '("subject_ref" "predicate" "object_ref" "relationship_action"))
          ((= schema-version 2)
           '("subject_ref" "predicate" "object_ref" "relationship_action"
             "evidence_status" "evidence_note"))
          (t
           '("subject_ref" "predicate" "object_ref" "relationship_action"
             "fact" "grounding" "temporal" "evidence_status"
             "evidence_note"))))
       (find (gethash "subject_ref" relationship) refs :test #'string=)
       (find (gethash "object_ref" relationship) refs :test #'string=)
       (%kgf-token-p (gethash "predicate" relationship))
       (or (= schema-version 1)
           (knowledge-graph-ontology-signature-valid-p
            (gethash "predicate" relationship)
            (gethash (gethash "subject_ref" relationship) kinds)
            (gethash (gethash "object_ref" relationship) kinds)))
       (let ((action (gethash "relationship_action" relationship)))
         (and (stringp action)
              (member action '("ASSERT" "RETIRE") :test #'string=)))
       (or (< schema-version 3)
           (and (%kgf-required-string-p (gethash "fact" relationship) 600)
                (%kgf-grounding-valid-p (gethash "grounding" relationship)
                                        refs source-evidence)
                (%kgf-temporal-valid-p (gethash "temporal" relationship))))
       (%kgf-evidence-fields-valid-p relationship schema-version)))

(defun %kgf-proposal-valid-p (proposal eligible-ids source-evidence)
  (and (member (gethash "schema_version" proposal) '(1 2 3))
       (%kgf-exact-keys-p
        proposal (if (= 1 (gethash "schema_version" proposal))
                     '("schema_version" "entities" "relationships")
                     '("schema_version" "ontology_revision" "entities"
                       "relationships")))
       (or (= 1 (gethash "schema_version" proposal))
           (string= *knowledge-graph-ontology-revision*
                    (gethash "ontology_revision" proposal "")))
       (let ((entities (gethash "entities" proposal))
             (relationships (gethash "relationships" proposal))
             (schema-version (gethash "schema_version" proposal)))
         (and (vectorp entities) (<= 1 (length entities) 24)
              (vectorp relationships) (<= (length relationships) 48)
              (let ((refs (loop for entity across entities
                                collect (gethash "local_ref" entity)))
                    (kinds (make-hash-table :test #'equal)))
                (loop for entity across entities
                      do (setf (gethash (gethash "local_ref" entity) kinds)
                               (gethash "kind" entity)))
                (and (= (length refs)
                        (length (remove-duplicates refs :test #'equal)))
                     (every (lambda (entity)
                              (%kgf-entity-valid-p entity eligible-ids
                                                   schema-version))
                            entities)
                     (every (lambda (relationship)
                              (%kgf-relationship-valid-p
                               relationship refs kinds schema-version
                               source-evidence))
                            relationships)))))))

(defun knowledge-graph-formation-sealed-payload-valid-p (payload)
  "Validate the closed semantic receipt consumed by the KG2 projector."
  (and (%kgf-exact-keys-p
        payload '("schema_version" "persona_id" "disclosure_class"
                  "formation_revision" "source_event_ids"
                  "source_memory_node_ids" "source_episode_ids"
                  "source_evidence" "eligible_existing_node_ids" "proposal"))
       (eql 1 (gethash "schema_version" payload))
       (%kgf-required-string-p (gethash "persona_id" payload) 120)
       (member (gethash "disclosure_class" payload)
               '("private" "personal-shareable" "public") :test #'string=)
       (let ((revision (gethash "formation_revision" payload)))
         (and (stringp revision)
              (string= *knowledge-graph-formation-revision* revision)))
       (%kgf-distinct-vector-p (gethash "source_event_ids" payload)
                               (lambda (value)
                                 (and (integerp value) (plusp value))))
       (plusp (length (gethash "source_event_ids" payload)))
       (%kgf-distinct-vector-p (gethash "source_memory_node_ids" payload)
                               (lambda (value)
                                 (%kgf-required-string-p value 180)))
       (%kgf-distinct-vector-p (gethash "source_episode_ids" payload)
                               (lambda (value)
                                 (%kgf-required-string-p value 180)))
       (%kgf-distinct-vector-p (gethash "eligible_existing_node_ids" payload)
                               (lambda (value)
                                 (%kgf-required-string-p value 180)))
       (vectorp (gethash "source_evidence" payload))
       (plusp (length (gethash "source_evidence" payload)))
       (%kgf-distinct-vector-p
        (gethash "source_evidence" payload)
        #'%kgf-source-evidence-row-valid-p
        :maximum *knowledge-graph-formation-maximum-source-evidence-records*)
       (<= (loop for row across (gethash "source_evidence" payload)
                 sum (length (gethash "text" row "")))
           *knowledge-graph-formation-maximum-source-evidence-characters*)
       (%kgf-proposal-valid-p
        (gethash "proposal" payload)
        (gethash "eligible_existing_node_ids" payload)
        (gethash "source_evidence" payload))
       (= 3 (gethash "schema_version" (gethash "proposal" payload) -1))))

(defun %kgf-id (kind persona-id event-id ordinal)
  (format nil "kgf:~a:~a"
          kind
          (%kgf-sha256
           (format nil "~a|~a|~a|~d|~d"
                   *knowledge-graph-formation-revision* kind persona-id
                   event-id ordinal))))

(defun %kgf-copy-object (object)
  (let ((copy (make-hash-table :test #'equal)))
    (maphash (lambda (key value)
               (setf (gethash key copy)
                     (if (vectorp value) (copy-seq value) value)))
             object)
    copy))

(defun %kgf-vector-union (left right)
  (coerce (remove-duplicates
           (append (%kgf-items left) (%kgf-items right)) :test #'equal)
          'vector))

(defun %kgf-vector-append (vector value)
  (coerce (append (%kgf-items vector) (list value)) 'vector))

(defun %kgf-index (rows key)
  (let ((index (make-hash-table :test #'equal)))
    (dolist (row (%kgf-items rows))
      (setf (gethash (gethash key row) index) (%kgf-copy-object row)))
    index))

(defun %kgf-sorted-values (index key)
  (let ((rows nil))
    (maphash (lambda (ignored row) (declare (ignore ignored)) (push row rows))
             index)
    (coerce (sort rows #'string< :key (lambda (row) (gethash key row "")))
            'vector)))

(defun %kgf-state (agent-id persona-id nodes edges)
  (obj "schema_version" 1
       "projection_revision" *knowledge-graph-formation-revision*
       "agent_id" agent-id "persona_id" persona-id
       "nodes" (%kgf-sorted-values nodes "node_id")
       "edges" (%kgf-sorted-values edges "edge_id")))

(defun %kgf-event-time (event)
  (let* ((value (gethash "timestamp" event))
         (parsed
           (cond
             ((and (integerp value) (plusp value)) value)
             ((and (stringp value)
                   (plusp (length value))
                   (every #'digit-char-p value))
              (handler-case (parse-integer value) (error () nil)))
             ((and (stringp value) (>= (length value) 19))
              (handler-case
                  (encode-universal-time
                   (parse-integer value :start 17 :end 19)
                   (parse-integer value :start 14 :end 16)
                   (parse-integer value :start 11 :end 13)
                   (parse-integer value :start 8 :end 10)
                   (parse-integer value :start 5 :end 7)
                   (parse-integer value :start 0 :end 4) 0)
                (error () nil)))
             (t nil))))
    (unless (and (integerp parsed) (plusp parsed))
      (error "KG formation event timestamp is invalid"))
    parsed))

(defun %kgf-current-node (nodes id persona-id)
  (let ((node (gethash id nodes)))
    (and node (string= persona-id (gethash "persona_id" node ""))
         (string= "current" (gethash "status" node "")) node)))

(defun %kgf-apply-event (event agent-id persona-id nodes edges)
  (let ((payload (gethash "payload" event)))
    (unless (and (hash-table-p event)
                 (string= "knowledge-graph-formation-sealed"
                          (gethash "type" event ""))
                 (equal agent-id (gethash "agent_id" event))
                 (knowledge-graph-formation-sealed-payload-valid-p payload)
                 (string= persona-id (gethash "persona_id" payload ""))
                 (integerp (gethash "id" event)))
      (error "KG formation event is invalid or belongs to another partition"))
    (let* ((event-id (gethash "id" event))
           (timestamp (%kgf-event-time event))
           (proposal (gethash "proposal" payload))
           (refs (make-hash-table :test #'equal)))
      (loop for entity across (gethash "entities" proposal)
            for ordinal from 0
            for action = (gethash "identity_action" entity)
            for existing = (gethash "existing_node_id" entity)
            for participant-role = (%kgf-participant-role entity)
            for current-participant =
              (and participant-role
                   (%kgf-current-participant nodes persona-id participant-role))
            do
               (when participant-role
                 (if current-participant
                     (unless (and (string= action "LINK_EXISTING")
                                  (string= existing
                                           (gethash "node_id"
                                                    current-participant)))
                       (error "KG participant must link its canonical identity"))
                     (unless (string= action "NEW")
                       (error "KG participant has no canonical identity to link"))))
               (cond
                 ((string= action "LINK_EXISTING")
                  (unless (%kgf-current-node nodes existing persona-id)
                    (error "KG link target is absent, foreign, or non-current"))
                  (setf (gethash (gethash "local_ref" entity) refs) existing))
                 (t
                  (let* ((old (and (string= action "REVISE_EXISTING")
                                   (%kgf-current-node nodes existing persona-id)))
                         (id (if participant-role
                                 (%kgf-participant-id agent-id persona-id
                                                      participant-role)
                                 (%kgf-id "entity" persona-id event-id ordinal))))
                    (when (and (string= action "REVISE_EXISTING") (null old))
                      (error "KG revision target is absent, foreign, or non-current"))
                    (when old
                      (setf (gethash "status" old) "superseded"
                            (gethash "valid_to" old) timestamp
                            (gethash "superseded_by_event_id" old) event-id))
                    (setf (gethash id nodes)
                          (obj "node_id" id "persona_id" persona-id
                               "node_kind" (gethash "kind" entity)
                               "label" (gethash "label" entity)
                               "aliases" (copy-seq (gethash "aliases" entity))
                               "classifications"
                               (copy-seq (gethash "classifications" entity #()))
                               "participant_role" (or participant-role :null)
                               "evidence_status" (%kgf-row-evidence-status entity)
                               "evidence_note" (%kgf-row-evidence-note entity)
                               "status" "current" "valid_from" timestamp
                               "valid_to" :null
                               "supersedes_node_id" (or existing :null)
                               "disclosure_class"
                               (gethash "disclosure_class" payload)
                               "descriptor_event_id" event-id
                               "source_event_ids"
                               (copy-seq (gethash "source_event_ids" payload))
                               "source_memory_node_ids"
                               (copy-seq
                                (gethash "source_memory_node_ids" payload))
                               "source_episode_ids"
                               (copy-seq (gethash "source_episode_ids" payload))))
                    (setf (gethash (gethash "local_ref" entity) refs) id)
                    (when old
                      (let ((edge-id (%kgf-id "supersedes" persona-id
                                              event-id ordinal)))
                        (setf (gethash edge-id edges)
                              (obj "edge_id" edge-id "persona_id" persona-id
                                   "from_node_id" id "predicate" "supersedes"
                                   "to_node_id" existing "status" "current"
                                   "evidence_status"
                                   (%kgf-row-evidence-status entity)
                                   "evidence_note"
                                   "runtime-authored exact supersession lineage"
                                   "valid_from" timestamp "valid_to" :null
                                   "validity_intervals" #()
                                   "descriptor_event_id" event-id
                                   "reinforcement_event_ids" #()
                                   "source_event_ids"
                                   (copy-seq
                                    (gethash "source_event_ids" payload))))))))))
      (loop for relationship across (gethash "relationships" proposal)
            for ordinal from 0
            for from = (gethash (gethash "subject_ref" relationship) refs)
            for to = (gethash (gethash "object_ref" relationship) refs)
            for predicate = (gethash "predicate" relationship)
            for action = (gethash "relationship_action" relationship)
            do
               (let ((matches nil))
                 (maphash
                  (lambda (id edge)
                    (when (and (string= from (gethash "from_node_id" edge ""))
                               (string= to (gethash "to_node_id" edge ""))
                               (string= predicate (gethash "predicate" edge "")))
                      (push id matches)))
                  edges)
                 (if (string= action "ASSERT")
                     (cond
                       ((null matches)
                        (let ((edge-id (%kgf-id "relation" persona-id
                                                event-id ordinal)))
                          (setf (gethash edge-id edges)
                                (obj "edge_id" edge-id
                                     "persona_id" persona-id
                                     "from_node_id" from
                                     "predicate" predicate
                                     "to_node_id" to "status" "current"
                                     "fact" (gethash "fact" relationship)
                                     "grounding"
                                     (%kgf-copy-object
                                      (gethash "grounding" relationship))
                                     "temporal"
                                     (%kgf-copy-object
                                      (gethash "temporal" relationship))
                                     "evidence_status"
                                     (%kgf-row-evidence-status relationship)
                                     "evidence_note"
                                     (%kgf-row-evidence-note relationship)
                                     "valid_from" timestamp "valid_to" :null
                                     "validity_intervals" #()
                                     "descriptor_event_id" event-id
                                     "reinforcement_event_ids" #()
                                     "source_event_ids"
                                     (copy-seq
                                      (gethash "source_event_ids" payload))))))
                       ((= 1 (length matches))
                        (let ((edge (gethash (first matches) edges)))
                          (when (string= "retired" (gethash "status" edge ""))
                            (setf (gethash "status" edge) "current"
                                  (gethash "valid_from" edge) timestamp
                                  (gethash "valid_to" edge) :null))
                          (when (> (%kgf-evidence-rank
                                    (%kgf-row-evidence-status relationship))
                                   (%kgf-evidence-rank
                                    (gethash "evidence_status" edge
                                             "unreviewed")))
                            (setf (gethash "evidence_status" edge)
                                  (%kgf-row-evidence-status relationship)
                                  (gethash "evidence_note" edge)
                                  (%kgf-row-evidence-note relationship)))
                          (setf (gethash "reinforcement_event_ids" edge)
                                (%kgf-vector-append
                                 (gethash "reinforcement_event_ids" edge #())
                                 event-id)
                                (gethash "source_event_ids" edge)
                                (%kgf-vector-union
                                 (gethash "source_event_ids" edge #())
                                 (gethash "source_event_ids" payload)))))
                       (t (error "KG assertion target is ambiguous")))
                     (progn
                       (setf matches
                             (remove-if-not
                              (lambda (id)
                                (string= "current"
                                         (gethash
                                          "status" (gethash id edges) "")))
                              matches))
                     (unless (= 1 (length matches))
                       (error "KG retirement target is absent or ambiguous"))
                     (let ((edge (gethash (first matches) edges)))
                       (setf (gethash "validity_intervals" edge)
                             (%kgf-vector-append
                              (gethash "validity_intervals" edge #())
                              (obj "valid_from" (gethash "valid_from" edge)
                                   "valid_to" timestamp))
                             (gethash "status" edge) "retired"
                             (gethash "valid_to" edge) timestamp
                             (gethash "retired_by_event_id" edge)
                             event-id)))))))))

(defun knowledge-graph-formation-fold (state event agent-id persona-id)
  "Apply one already sealed event without replaying earlier events."
  (unless (and (hash-table-p state)
               (string= agent-id (gethash "agent_id" state ""))
               (string= persona-id (gethash "persona_id" state ""))
               (string= *knowledge-graph-formation-revision*
                        (gethash "projection_revision" state "")))
    (error "KG formation prior state is invalid"))
  (let ((nodes (%kgf-index (gethash "nodes" state) "node_id"))
        (edges (%kgf-index (gethash "edges" state) "edge_id")))
    (%kgf-apply-event event agent-id persona-id nodes edges)
    (%kgf-state agent-id persona-id nodes edges)))

(defun knowledge-graph-formation-project (events agent-id persona-id)
  "Replay sealed formation receipts into one pure canonical state."
  (let ((state (%kgf-state agent-id persona-id
                           (make-hash-table :test #'equal)
                           (make-hash-table :test #'equal))))
    (dolist (event (sort (copy-list (%kgf-items events)) #'<
                         :key (lambda (row) (gethash "id" row 0))))
      (when (and (hash-table-p event)
                 (string= "knowledge-graph-formation-sealed"
                          (gethash "type" event ""))
                 (equal agent-id (gethash "agent_id" event))
                 (let ((payload (gethash "payload" event)))
                   (and (hash-table-p payload)
                        (string= persona-id
                                 (gethash "persona_id" payload ""))
                        (knowledge-graph-formation-sealed-payload-valid-p
                         payload))))
        (setf state (knowledge-graph-formation-fold
                     state event agent-id persona-id))))
    state))

(defun %kgf-integrity (&rest fields)
  (%kgf-sha256
   (with-output-to-string (stream)
     (dolist (field fields)
       (let ((text (princ-to-string field)))
         (format stream "~d:~a" (length text) text))))))

(defun %kgf-node-row (agent-id persona-id node-id kind key payload)
  (let* ((json (shasht:write-json payload nil))
         (integrity
           (%kgf-integrity *knowledge-graph-formation-projection-name*
                           agent-id persona-id node-id kind key json)))
    (obj "projection_name" *knowledge-graph-formation-projection-name*
         "agent_id" agent-id "persona_id" persona-id "node_id" node-id
         "node_kind" kind "canonical_key" key "payload_json" json
         "integrity_hash" integrity)))

(defun %kgf-edge-row (agent-id persona-id edge-id from predicate to payload)
  (let* ((json (shasht:write-json payload nil))
         (integrity
           (%kgf-integrity *knowledge-graph-formation-projection-name*
                           agent-id persona-id edge-id from predicate to json)))
    (obj "projection_name" *knowledge-graph-formation-projection-name*
         "agent_id" agent-id "persona_id" persona-id "edge_id" edge-id
         "from_node_id" from "predicate" predicate "to_node_id" to
         "payload_json" json "integrity_hash" integrity)))

(defun %kgf-evidence-row
    (agent-id persona-id owner-kind owner-id event-id role ordinal)
  (obj "projection_name" *knowledge-graph-formation-projection-name*
       "agent_id" agent-id "persona_id" persona-id
       "owner_kind" owner-kind "owner_id" owner-id
       "evidence_event_id" event-id "evidence_role" role
       "evidence_ordinal" ordinal))

(defun knowledge-graph-formation-materialization (state)
  "Convert pure KG2 state into canonical generic rows with exact evidence."
  (unless (and (hash-table-p state)
               (string= *knowledge-graph-formation-revision*
                        (gethash "projection_revision" state "")))
    (error "KG formation state is invalid"))
  (let* ((agent-id (gethash "agent_id" state))
         (persona-id (gethash "persona_id" state))
         (nodes nil) (edges nil) (evidence nil))
    (dolist (node (%kgf-items (gethash "nodes" state)))
      (let ((id (gethash "node_id" node)))
        (push (%kgf-node-row agent-id persona-id id
                             (gethash "node_kind" node) id node)
              nodes)
        (let ((descriptor (gethash "descriptor_event_id" node))
              (sources (gethash "source_event_ids" node #())))
          (push (%kgf-evidence-row agent-id persona-id "node" id
                                   descriptor "descriptor" 0)
                evidence)
          (loop for source across sources for ordinal from 0
                do (push (%kgf-evidence-row agent-id persona-id "node" id
                                            source "source" ordinal)
                         evidence)))))
    (dolist (edge (%kgf-items (gethash "edges" state)))
      (let ((id (gethash "edge_id" edge)))
        (push (%kgf-edge-row agent-id persona-id id
                             (gethash "from_node_id" edge)
                             (gethash "predicate" edge)
                             (gethash "to_node_id" edge) edge)
              edges)
        (let ((descriptor (gethash "descriptor_event_id" edge))
              (reinforcements
                (gethash "reinforcement_event_ids" edge #()))
              (sources (gethash "source_event_ids" edge #())))
          (push (%kgf-evidence-row agent-id persona-id "edge" id
                                   descriptor "descriptor" 0)
                evidence)
          (loop for reinforcement across reinforcements
                for ordinal from 1
                do (push (%kgf-evidence-row agent-id persona-id "edge" id
                                            reinforcement "descriptor" ordinal)
                         evidence))
          (loop for source across sources for ordinal from 0
                do (push (%kgf-evidence-row agent-id persona-id "edge" id
                                            source "source" ordinal)
                         evidence)))))
    (labels ((evidence-key (row)
               (format nil "~a|~a|~20,'0d|~a"
                       (gethash "owner_kind" row) (gethash "owner_id" row)
                       (gethash "evidence_event_id" row)
                       (gethash "evidence_role" row)))
             (evidence-sort-key (row)
               (format nil "~a|~20,'0d"
                       (evidence-key row)
                       (gethash "evidence_ordinal" row)))
             (canonical-evidence (rows)
               ;; Descriptor reinforcement can legitimately encounter the
               ;; same formation event more than once.  The storage identity
               ;; deliberately excludes ordinal, so canonicalization must do
               ;; the same before SQLite sees the generation.  Retain the
               ;; lowest ordinal deterministically.
               (let ((seen (make-hash-table :test #'equal))
                     (result nil))
                 (dolist (row (sort rows #'string< :key #'evidence-sort-key))
                   (let ((key (evidence-key row)))
                     (unless (gethash key seen)
                       (setf (gethash key seen) t)
                       (push row result))))
                 (nreverse result))))
      (obj "schema_version" 1
           "projection_name" *knowledge-graph-formation-projection-name*
           "projection_revision" *knowledge-graph-formation-revision*
           "agent_id" agent-id "persona_id" persona-id
           "nodes" (coerce (sort nodes #'string<
                                  :key (lambda (row) (gethash "node_id" row)))
                           'vector)
           "edges" (coerce (sort edges #'string<
                                  :key (lambda (row) (gethash "edge_id" row)))
                           'vector)
           "evidence" (coerce (canonical-evidence evidence) 'vector)))))
