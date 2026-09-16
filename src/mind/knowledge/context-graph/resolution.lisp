;;;; resolution.lisp -- bounded candidates, never similarity-based merge authority.
(in-package :pai.context-graph)

(defun context-graph-normalize-legacy-participants (proposal descriptors)
  "Collapse explicitly classified runtime participants in a raw formation.

DESCRIPTORS is a sequence of closed runtime-owned records with role, kind and
optional existing identity fields.  Equal labels are deliberately irrelevant:
only an explicit `operator` or `active-persona` classification grants this
normalization authority.  The returned proposal is detached from its input."
  (unless (and (hash-table-p proposal)
               (vectorp (gethash "entities" proposal))
               (vectorp (gethash "relationships" proposal))
               (or (vectorp descriptors) (listp descriptors)))
    (error "Context graph participant normalization input is invalid"))
  (let ((result (%cg-detach proposal))
        (replacements (make-hash-table :test #'equal))
        (removed (make-hash-table :test #'equal))
        (repairs nil))
    (dolist (descriptor (%cg-items descriptors))
      (unless (and (hash-table-p descriptor)
                   (%cg-present-string-p (gethash "role" descriptor) 40)
                   (member (gethash "role" descriptor)
                           '("operator" "active-persona") :test #'string=)
                   (%cg-present-string-p (gethash "kind" descriptor) 40)
                   (or (%cg-null-p (gethash "existing_node_id" descriptor))
                       (%cg-present-string-p
                        (gethash "existing_node_id" descriptor) 180)))
        (error "Context graph participant descriptor is invalid"))
      (let* ((role (gethash "role" descriptor))
             (kind (gethash "kind" descriptor))
             (matches
               (loop for entity across (gethash "entities" result)
                     when (and
                           (string= kind (gethash "kind" entity ""))
                           (find role (%cg-items
                                       (gethash "classifications" entity #()))
                                 :test #'string-equal))
                       collect entity)))
        (when matches
          (let* ((label (gethash "label" descriptor))
                 (primary
                   (or (and (%cg-present-string-p label 240)
                            (find label matches :test #'string-equal
                                  :key (lambda (row)
                                         (gethash "label" row ""))))
                       (first matches)))
                 (primary-ref (gethash "local_ref" primary))
                 (existing (gethash "existing_node_id" descriptor)))
            (dolist (entity matches)
              (let ((ref (gethash "local_ref" entity)))
                (setf (gethash ref replacements) primary-ref)
                (unless (eq entity primary) (setf (gethash ref removed) t))))
            (when (%cg-present-string-p label 240)
              (setf (gethash "label" primary) label))
            (when (vectorp (gethash "aliases" descriptor))
              (setf (gethash "aliases" primary)
                    (%cg-copy-vector (gethash "aliases" descriptor))))
            (setf (gethash "classifications" primary)
                  (coerce
                   (remove-duplicates
                    (append (%cg-items
                             (gethash "classifications" primary #()))
                            (list role))
                    :test #'string-equal)
                   'vector)
                  (gethash "identity_action" primary)
                  (if (%cg-null-p existing) "NEW" "LINK_EXISTING")
                  (gethash "existing_node_id" primary)
                  (if (%cg-null-p existing) :null existing))
            (push (%cg-object
                   "role" role "canonical_ref" primary-ref
                   "collapsed_refs"
                   (coerce (mapcar (lambda (row) (gethash "local_ref" row))
                                   matches)
                           'vector))
                  repairs)))))
    (setf (gethash "entities" result)
          (coerce
           (loop for entity across (gethash "entities" result)
                 unless (gethash (gethash "local_ref" entity) removed)
                   collect entity)
           'vector))
    (let ((relationships nil) (seen (make-hash-table :test #'equal)))
      (loop for relationship across (gethash "relationships" result)
            for copy = (%cg-detach relationship)
            do (dolist (field '("subject_ref" "object_ref"))
                 (let ((replacement (gethash (gethash field copy) replacements)))
                   (when replacement (setf (gethash field copy) replacement))))
               (let* ((grounding (gethash "grounding" copy))
                      (attributed (and (hash-table-p grounding)
                                       (gethash "attributed_to_ref" grounding)))
                      (replacement (and attributed
                                        (gethash attributed replacements))))
                 (when replacement
                   (setf (gethash "attributed_to_ref" grounding) replacement)))
               (let ((key (shasht:write-json copy nil)))
                 (unless (gethash key seen)
                   (setf (gethash key seen) t)
                   (push copy relationships))))
      (setf (gethash "relationships" result)
            (coerce (nreverse relationships) 'vector)))
    (values result (coerce (nreverse repairs) 'vector))))

(defun context-graph-entity-candidates
    (graph descriptor &key (maximum 8) (compatible-types #()))
  "Retrieve exact or explicitly compatible type candidates.

Compatibility only widens what a semantic resolver may inspect. It never
authorizes an automatic cross-type merge."
  (%cg-require-legacy-profile graph)
  (unless (and (context-graph-p graph) (hash-table-p descriptor)
               (%cg-present-string-p (gethash "type" descriptor))
               (%cg-present-string-p (gethash "name" descriptor))
               (integerp maximum) (<= 1 maximum 16)
               (vectorp compatible-types)
               (every #'%cg-present-string-p compatible-types))
    (error "Invalid entity candidate request"))
  (let* ((name (gethash "name" descriptor))
         (tokens (%cg-query-tokens
                  (format nil "~a ~{~a~^ ~}" name
                          (%cg-items (gethash "aliases" descriptor)))))
         (rows nil))
    (maphash
     (lambda (id entity)
       (when (or (string= (gethash "type" descriptor)
                          (gethash "entity_type" entity))
                 (find (gethash "entity_type" entity) compatible-types
                       :test #'string=))
         (let* ((names (cons (gethash "name" entity)
                             (%cg-items (gethash "aliases" entity))))
                (exact (find (%cg-canonical name) names :test #'string=
                             :key #'%cg-canonical))
                (score (+ (if exact 100 0)
                          (%cg-token-score tokens (%cg-entity-text entity))))
                (relations nil))
           (dolist (fact (%cg-sorted-values (context-graph-facts graph) "fact_id"))
             (when (and (< (length relations) 3) (gethash "current" fact)
                        (or (equal id (gethash "subject_id" fact))
                            (equal id (gethash "object_id" fact))))
               (let* ((other-id (if (equal id (gethash "subject_id" fact))
                                    (gethash "object_id" fact)
                                    (gethash "subject_id" fact)))
                      (other (gethash other-id (context-graph-entities graph))))
                 (push (%cg-object "predicate" (gethash "predicate" fact)
                                   "direction" (if (equal id (gethash "subject_id" fact))
                                                    "outgoing" "incoming")
                                   "other" (gethash "name" other)
                                   "grounding" (%cg-detach (gethash "grounding" fact))
                                   "evidence_status" (gethash "evidence_status" fact))
                       relations))))
           (push (%cg-object "entity_id" id "type" (gethash "entity_type" entity)
                             "type_compatibility"
                             (if (string= (gethash "type" descriptor)
                                          (gethash "entity_type" entity))
                                 "exact" "compatible")
                             "name" (gethash "name" entity)
                             "aliases" (copy-seq (gethash "aliases" entity))
                             "classifications"
                             (%cg-copy-vector (gethash "classifications" entity))
                             "score" score "relationships" (coerce (nreverse relations) 'vector))
                 rows))))
     (context-graph-entities graph))
    (setf rows (stable-sort rows
                 (lambda (a b) (if (= (gethash "score" a) (gethash "score" b))
                                    (string< (gethash "entity_id" a) (gethash "entity_id" b))
                                    (> (gethash "score" a) (gethash "score" b))))))
    (%cg-object "local_ref" (gethash "local_ref" descriptor)
                "strategy" "declared-compatible-type-lexical-with-bounded-fallback"
                "eligible_count" (length rows)
                "non_exhaustive" (> (length rows) maximum)
                "candidates" (coerce (subseq rows 0 (min maximum (length rows))) 'vector))))

(defun context-graph-compact-search (graph query &key (character-budget 4000) (claim-policy "factual"))
  "Return whole typed facts under a total serialized envelope budget."
  (unless (and (integerp character-budget) (<= 256 character-budget 16000))
    (error "Invalid compact graph budget"))
  (let* ((raw (context-graph-search graph query :maximum-results 50
                                   :evidence-policy "verified" :claim-policy claim-policy))
         (output (%cg-object "schema_version" 1 "facts" #()
                             "non_exhaustive" t "omitted_count" 0))
         (selected nil) (omitted 0))
    (loop for row across (gethash "facts" raw)
          for compact = (%cg-object
                         "fact_id" (gethash "fact_id" row)
                         "fact" (gethash "fact" row)
                         "grounding" (%cg-detach (gethash "grounding" row))
                         "subject" (gethash "subject" row)
                         "predicate" (gethash "predicate" row)
                         "object" (gethash "object" row)
                         "evidence_status" (gethash "evidence_status" row)
                         "evidence_notes"
                         (coerce (remove-duplicates
                                  (mapcar (lambda (record) (gethash "note" record))
                                          (%cg-items (gethash "evidence_records" row)))
                                  :test #'string=) 'vector)
                         "valid_at" (gethash "valid_at" row)
                         "temporal" (%cg-detach (gethash "temporal" row))
                         "source_episode_ids" (gethash "source_episode_ids" row))
          do (setf (gethash "facts" output) (coerce (append selected (list compact)) 'vector))
             (if (<= (+ 16 (length (shasht:write-json output nil))) character-budget)
                 (setf selected (append selected (list compact)))
                 (incf omitted)))
    (setf (gethash "facts" output) (coerce selected 'vector)
          (gethash "omitted_count" output) omitted)
    output))
