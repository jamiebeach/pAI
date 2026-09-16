;;;; core.lisp -- standalone typed, temporal, provenance-linked graph state.

(in-package :pai.context-graph)

(defun %cg-object (&rest pairs)
  (let ((result (make-hash-table :test #'equal)))
    (loop for (key value) on pairs by #'cddr
          do (setf (gethash key result) value))
    result))

(defun %cg-items (value)
  (cond ((null value) nil)
        ((vectorp value) (coerce value 'list))
        ((listp value) value)
        (t (error "Context graph expected a sequence"))))

(defun %cg-present-string-p (value &optional (maximum 1000))
  (and (stringp value) (plusp (length value)) (<= (length value) maximum)))

(defun %cg-null-p (value)
  (or (null value) (eq value :null)))

(defun %cg-canonical (text)
  (string-downcase
   (string-trim '(#\Space #\Tab #\Newline #\Return) text)))

(defun %cg-sha256 (&rest parts)
  (string-downcase
   (ironclad:byte-array-to-hex-string
    (ironclad:digest-sequence
     :sha256
     (sb-ext:string-to-octets
      (format nil "~{~a~^|~}" parts) :external-format :utf-8)))))

(defstruct (context-graph (:constructor %make-context-graph))
  ontology
  (entities (make-hash-table :test #'equal))
  (entity-index (make-hash-table :test #'equal))
  (facts (make-hash-table :test #'equal))
  (current-triples (make-hash-table :test #'equal))
  (episodes (make-hash-table :test #'equal))
  (corrections (make-hash-table :test #'equal))
  ;; New-authority projections keep enduring identity distinct from versions.
  ;; Legacy application never populates or interprets these indexes.
  (authority-profile nil)
  (authority-partition nil)
  (entity-versions (make-hash-table :test #'equal))
  (current-entity-versions (make-hash-table :test #'equal))
  (revision-lineage (make-hash-table :test #'equal))
  (application-receipts (make-hash-table :test #'equal))
  (entity-scan-index #())
  (fact-scan-index #())
  (entity-adjacency (make-hash-table :test #'equal))
  (through-event-id 0)
  (projection-digest nil))

(defparameter +cg-temporal-characters+
  '("event" "temporary-state" "ongoing-state" "standing-disposition"
    "timeless" "unspecified"))

(defun %cg-time-value-p (value)
  (or (%cg-null-p value) (%cg-present-string-p value 80)))

(defun %cg-temporal-metadata (descriptor occurred-at)
  "Validate semantic time separately from graph observation time."
  (let ((given (gethash "temporal" descriptor)))
    (if (null given)
        (%cg-object "schema_version" 1 "character" "unspecified"
                    "occurred_at" occurred-at "valid_from" occurred-at
                    "valid_until" :null)
        (progn
          (unless (and (hash-table-p given)
                       (%cg-closed-keys-p
                        given '("schema_version" "character" "occurred_at"
                                "valid_from" "valid_until"))
                       (eql 1 (gethash "schema_version" given))
                       (member (gethash "character" given)
                               +cg-temporal-characters+ :test #'string=)
                       (%cg-time-value-p (gethash "occurred_at" given))
                       (%cg-time-value-p (gethash "valid_from" given))
                       (%cg-time-value-p (gethash "valid_until" given)))
            (error "Context graph semantic temporal metadata is invalid"))
          (%cg-detach given)))))

(defun %cg-edge-types (ontology)
  (gethash "edge_types" ontology))

(defun %cg-entity-types (ontology)
  (gethash "entity_types" ontology))

(defun %cg-edge-definition (ontology predicate)
  (find predicate (%cg-items (%cg-edge-types ontology))
        :test #'string= :key (lambda (row) (gethash "name" row ""))))

(defun %cg-type-declared-p (ontology type)
  (find type (%cg-items (%cg-entity-types ontology)) :test #'string=))

(defun %cg-signature-valid-p (ontology predicate subject-type object-type)
  (let ((definition (%cg-edge-definition ontology predicate)))
    (and definition
         (find subject-type (%cg-items (gethash "subject_types" definition))
               :test #'string=)
         (find object-type (%cg-items (gethash "object_types" definition))
               :test #'string=))))

(defun %cg-validate-ontology (ontology)
  (unless (and (hash-table-p ontology)
               (vectorp (%cg-entity-types ontology))
               (plusp (length (%cg-entity-types ontology)))
               (every #'%cg-present-string-p (%cg-entity-types ontology))
               (vectorp (%cg-edge-types ontology))
               (every
                (lambda (edge)
                  (and (hash-table-p edge)
                       (%cg-present-string-p (gethash "name" edge) 120)
                       (vectorp (gethash "subject_types" edge))
                       (vectorp (gethash "object_types" edge))))
                (%cg-edge-types ontology)))
    (error "Context graph ontology is invalid"))
  ontology)

(defun make-context-graph (ontology)
  (%make-context-graph :ontology (%cg-validate-ontology ontology)))

(defun context-graph-entity-count (graph)
  (hash-table-count (context-graph-entities graph)))

(defun context-graph-fact-count (graph)
  (hash-table-count (context-graph-facts graph)))

(defun %cg-copy-vector (value)
  (if (vectorp value) (copy-seq value) #()))

(defparameter +cg-evidence-statuses+
  '("direct" "prior-graph" "inference" "unreviewed"))

(defun %cg-evidence-status (descriptor)
  (let ((status (gethash "evidence_status" descriptor "unreviewed")))
    (unless (member status +cg-evidence-statuses+ :test #'string=)
      (error "Context graph evidence status is invalid"))
    status))

(defun %cg-evidence-rank (status)
  (or (position status
                '("unreviewed" "inference" "prior-graph" "direct")
                :test #'string=)
      0))

(defun %cg-evidence-record (descriptor episode-id)
  (let ((note (gethash "evidence_note" descriptor "not evidence-reviewed")))
    (unless (%cg-present-string-p note 600)
      (error "Context graph evidence note is invalid"))
    (%cg-object "source_episode_id" episode-id
                "status" (%cg-evidence-status descriptor)
                "grounding" (%cg-detach (gethash "grounding" descriptor))
                "note" note)))

(defun %cg-add-evidence-record (row descriptor episode-id)
  (let* ((records (%cg-items (gethash "evidence_records" row)))
         (record (%cg-evidence-record descriptor episode-id))
         (status (gethash "status" record)))
    (unless (find-if
             (lambda (existing)
               (and (string= episode-id (gethash "source_episode_id" existing))
                    (string= status (gethash "status" existing))))
             records)
      (setf (gethash "evidence_records" row)
            (coerce (append records (list record)) 'vector)))
    (when (> (%cg-evidence-rank status)
             (%cg-evidence-rank (gethash "evidence_status" row "unreviewed")))
      (setf (gethash "evidence_status" row) status)))
  row)

(defun %cg-entity-key (type name)
  (format nil "~a|~a" type (%cg-canonical name)))

(defun %cg-resolve-entity
    (graph descriptor episode-id learned-at ordinal refs reuse-exact-p)
  (let* ((type (gethash "type" descriptor))
         (name (gethash "name" descriptor))
         (action (gethash "action" descriptor))
         (existing-id (gethash "existing_id" descriptor))
         (local-ref (gethash "local_ref" descriptor))
         (ontology (context-graph-ontology graph)))
    (unless (and (%cg-present-string-p local-ref 80)
                 (%cg-present-string-p type 120)
                 (%cg-type-declared-p ontology type)
                 (%cg-present-string-p name 240)
                 (vectorp (gethash "aliases" descriptor))
                 (vectorp (gethash "classifications" descriptor #()))
                 (every (lambda (value) (%cg-present-string-p value 120))
                        (gethash "classifications" descriptor #()))
                 (member action '("NEW" "LINK_EXISTING") :test #'string=))
      (error "Context graph entity descriptor is invalid"))
    (let* ((canonical-key (%cg-entity-key type name))
           (exact-id (gethash canonical-key (context-graph-entity-index graph)))
           (id
             (cond
               ((string= action "LINK_EXISTING")
                (unless (and (%cg-present-string-p existing-id 180)
                             (gethash existing-id
                                      (context-graph-entities graph))
                             (string= type (gethash "entity_type"
                               (gethash existing-id (context-graph-entities graph)))))
                  (error "Context graph exact entity link is unavailable"))
                existing-id)
               ((and reuse-exact-p exact-id (%cg-null-p existing-id)) exact-id)
               ((%cg-null-p existing-id)
                (format nil "cge:~a"
                        (%cg-sha256 type name episode-id ordinal)))
               (t (error "NEW entity may not invent an existing ID")))))
      (unless (gethash id (context-graph-entities graph))
        (let ((entity
                (%cg-object
                 "entity_id" id "entity_type" type "name" name
                 "canonical_name" (%cg-canonical name)
                 "aliases" (%cg-copy-vector (gethash "aliases" descriptor))
                 "classifications"
                 (%cg-copy-vector (gethash "classifications" descriptor #()))
                 "created_from_episode_id" episode-id
                 "created_at" learned-at
                 "last_supported_at" learned-at
                 "evidence_status" "unreviewed"
                 "evidence_records" #())))
          (setf (gethash id (context-graph-entities graph)) entity
                (gethash canonical-key (context-graph-entity-index graph)) id)))
      (let* ((entity (gethash id (context-graph-entities graph)))
             (aliases (append (%cg-items (gethash "aliases" entity))
                              (%cg-items (gethash "aliases" descriptor))
                              (unless (string= name (gethash "name" entity))
                                (list name))))
             (classifications
               (append (%cg-items (gethash "classifications" entity))
                       (%cg-items (gethash "classifications" descriptor #())))))
        (setf (gethash "aliases" entity)
              (coerce (remove-duplicates aliases :test #'string-equal) 'vector)
              (gethash "classifications" entity)
              (coerce (remove-duplicates classifications :test #'string-equal)
                      'vector)
              (gethash "last_supported_at" entity) learned-at))
      (%cg-add-evidence-record
       (gethash id (context-graph-entities graph)) descriptor episode-id)
      (setf (gethash local-ref refs) id)
      id)))

(defun %cg-add-provenance (fact descriptor episode-id)
  (let ((existing (%cg-items (gethash "source_episode_ids" fact))))
    (unless (find episode-id existing :test #'string=)
      (setf (gethash "source_episode_ids" fact)
            (coerce (append existing (list episode-id)) 'vector))))
  (%cg-add-evidence-record fact descriptor episode-id)
  fact)

(defun %cg-apply-fact (graph descriptor episode refs ordinal)
  (let* ((subject-id (gethash (gethash "subject_ref" descriptor) refs))
         (object-id (gethash (gethash "object_ref" descriptor) refs))
         (predicate (gethash "predicate" descriptor))
         (statement (gethash "fact" descriptor))
         (supersedes (gethash "supersedes_fact_id" descriptor))
         (entities (context-graph-entities graph))
         (subject (and subject-id (gethash subject-id entities)))
         (object (and object-id (gethash object-id entities)))
         (episode-id (gethash "episode_id" episode))
         (occurred-at (gethash "occurred_at" episode))
         (learned-at (gethash "learned_at" episode))
         (temporal (%cg-temporal-metadata descriptor occurred-at))
         (grounding (%cg-resolve-grounding descriptor refs))
         (triple-key (%cg-fact-key subject-id predicate object-id statement grounding)))
    (unless (and subject object (%cg-present-string-p predicate 120)
                 (%cg-present-string-p statement 1000)
                 (%cg-signature-valid-p
                  (context-graph-ontology graph) predicate
                  (gethash "entity_type" subject)
                  (gethash "entity_type" object)))
      (error "Context graph fact violates its typed edge signature"))
    (when (not (%cg-null-p supersedes))
      (let ((prior (gethash supersedes (context-graph-facts graph))))
        (when (or grounding (and prior (gethash "grounding" prior)))
          (error "Grounded supersession requires separately qualified temporal authority"))
        (unless (and prior (gethash "current" prior)
                     (string= subject-id (gethash "subject_id" prior))
                     (string= predicate (gethash "predicate" prior)))
          (error "Context graph supersession is not an exact current fact"))
        (setf (gethash "current" prior) nil
              (gethash "invalid_at" prior) occurred-at
              (gethash "expired_at" prior) learned-at)
        (remhash (gethash "identity_key" prior)
                 (context-graph-current-triples graph))))
    (let ((duplicate-id
            (gethash triple-key (context-graph-current-triples graph))))
      (if duplicate-id
          (%cg-add-provenance
           (gethash duplicate-id (context-graph-facts graph)) descriptor episode-id)
          (let* ((id (format nil "cgf:~a"
                             (if grounding (%cg-sha256 triple-key episode-id ordinal)
                                 (%cg-sha256 subject-id predicate object-id episode-id ordinal))))
                 (fact
                   (%cg-object
                    "fact_id" id "subject_id" subject-id
                    "predicate" predicate "object_id" object-id
                    "fact" statement "current" t
                    "grounding" (%cg-detach grounding) "identity_key" triple-key
                    "temporal" temporal "observed_at" learned-at
                    "occurred_at" (gethash "occurred_at" temporal)
                    "valid_at" (gethash "valid_from" temporal) "invalid_at" :null
                    "created_at" learned-at "expired_at" :null
                    "supersedes_fact_id" (or supersedes :null)
                    "source_episode_ids" (vector episode-id)
                    "evidence_status" "unreviewed"
                    "evidence_records" #())))
            (%cg-add-evidence-record fact descriptor episode-id)
            (setf (gethash id (context-graph-facts graph)) fact
                  (gethash triple-key
                           (context-graph-current-triples graph)) id)
            fact)))))

(defun %cg-apply-episode (graph episode proposal
                                   &key (reuse-exact-identities-p t))
  "Apply one already-formed proposal to GRAPH under deterministic authority."
  (unless (and (context-graph-p graph) (hash-table-p episode)
               (hash-table-p proposal)
               (%cg-present-string-p (gethash "episode_id" episode) 180)
               (%cg-present-string-p (gethash "occurred_at" episode) 80)
               (%cg-present-string-p (gethash "learned_at" episode) 80)
               (%cg-present-string-p (gethash "content" episode) 12000)
               (vectorp (gethash "entities" proposal))
               (vectorp (gethash "facts" proposal)))
    (error "Context graph episode or proposal is invalid"))
  (let ((episode-id (gethash "episode_id" episode)))
    (when (gethash episode-id (context-graph-episodes graph))
      (return-from %cg-apply-episode
        (%cg-object "status" "already-applied" "episode_id" episode-id)))
    (let ((refs (make-hash-table :test #'equal)))
      (loop for entity across (gethash "entities" proposal)
            for ordinal from 0
            do (%cg-resolve-entity graph entity episode-id
                                   (gethash "learned_at" episode) ordinal refs
                                   reuse-exact-identities-p))
      (loop for fact across (gethash "facts" proposal)
            for ordinal from 0
            do (%cg-apply-fact graph fact episode refs ordinal))
      (setf (gethash episode-id (context-graph-episodes graph))
            (%cg-object "episode_id" episode-id
                        "occurred_at" (gethash "occurred_at" episode)
                        "learned_at" (gethash "learned_at" episode)))
      (%cg-object "status" "applied" "episode_id" episode-id
                  "entity_count" (context-graph-entity-count graph)
                  "fact_count" (context-graph-fact-count graph)))))

(defun %cg-sorted-values (table key)
  (sort (loop for value being the hash-values of table collect value)
        #'string< :key (lambda (row) (gethash key row ""))))

(defun context-graph-snapshot (graph)
  (%cg-require-legacy-profile graph)
  (%cg-object
   "schema_version" 1
   "entity_count" (context-graph-entity-count graph)
   "fact_count" (context-graph-fact-count graph)
   "correction_count" (hash-table-count (context-graph-corrections graph))
   "entities" (coerce (%cg-sorted-values
                       (context-graph-entities graph) "entity_id") 'vector)
   "facts" (coerce (%cg-sorted-values
                    (context-graph-facts graph) "fact_id") 'vector)
   "corrections" (coerce (%cg-sorted-values
                          (context-graph-corrections graph) "correction_id") 'vector)))

(defun %cg-require-legacy-profile (graph)
  (when (context-graph-authority-profile graph)
    (error "Legacy graph API cannot interpret an authority projection")))
