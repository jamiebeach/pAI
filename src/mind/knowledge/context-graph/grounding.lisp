;;;; grounding.lisp -- source-bound claim scope for the standalone laboratory.
;;;; Source integrity is not semantic entailment; classification needs evaluation.
(in-package :pai.context-graph)

(defparameter +cg-claim-scopes+
  '("assertion" "question" "hypothesis" "intention" "reported-speech"
    "retrieval-outcome" "joke" "proposal" "unresolved"))

(defun %cg-detach (value)
  (cond ((hash-table-p value)
         (let ((copy (make-hash-table :test #'equal)))
           (maphash (lambda (k v) (setf (gethash (if (stringp k) (copy-seq k) k) copy) (%cg-detach v))) value) copy))
        ((stringp value) (copy-seq value))
        ((vectorp value) (map 'vector #'%cg-detach value))
        (t value)))

(defun %cg-closed-keys-p (row keys)
  (and (hash-table-p row) (= (hash-table-count row) (length keys))
       (every (lambda (key) (nth-value 1 (gethash key row))) keys)))

(defun %cg-canonical-tree (value)
  (cond ((hash-table-p value)
         (vector "map" (coerce (loop for key in (sort (loop for key being the hash-keys of value collect key) #'string<)
                                     collect (vector key (%cg-canonical-tree (gethash key value)))) 'vector)))
        ((stringp value) value)
        ((vectorp value) (vector "array" (map 'vector #'%cg-canonical-tree value)))
        (t value)))

(defun %cg-valid-source-row-shape-p (source)
  "Accept the original packet shape and the production evidence timestamp extension."
  (or (%cg-closed-keys-p source
                         '("source_id" "speaker_id" "kind" "text" "text_sha256"))
      (and (%cg-closed-keys-p source
                              '("source_id" "speaker_id" "kind" "timestamp"
                                "text" "text_sha256"))
           (let ((timestamp (gethash "timestamp" source)))
             (or (%cg-null-p timestamp)
                 (%cg-present-string-p timestamp 180)
                 (and (integerp timestamp) (not (minusp timestamp))))))))

(defun %cg-prepare-grounding (proposal packet)
  "Return a detached proposal, resolving quotes only against explicit source bytes."
  (let ((copy (%cg-detach proposal)) (sources (make-hash-table :test #'equal)))
    (when packet
      (unless (and (%cg-closed-keys-p packet '("schema_version" "sources"))
                   (eql 1 (gethash "schema_version" packet))
                   (vectorp (gethash "sources" packet))
                   (<= 1 (length (gethash "sources" packet)) 32))
        (error "Invalid graph source packet"))
      (loop for source across (gethash "sources" packet)
            for id = (and (hash-table-p source) (gethash "source_id" source))
            do (unless (and (%cg-valid-source-row-shape-p source)
                            (%cg-present-string-p id 180) (not (gethash id sources))
                            (%cg-present-string-p (gethash "speaker_id" source) 180)
                            (%cg-present-string-p (gethash "text" source) 12000)
                            (member (gethash "kind" source)
                                    '("original-utterance" "prior-agent-utterance"
                                      "tool-observation" "generated-summary"
                                      "retrieval-metadata")
                                    :test #'equal)
                            (equal (%cg-sha256 (gethash "text" source)) (gethash "text_sha256" source)))
                 (error "Graph source identity or text hash invalid"))
               (setf (gethash id sources) source)))
    (loop for fact across (gethash "facts" copy)
          for grounding = (gethash "grounding" fact)
          do (cond
               ((and (null packet) grounding) (error "Grounded claim requires a source packet"))
               (packet
                (unless (and (%cg-closed-keys-p grounding '("schema_version" "scope" "polarity" "attributed_to_ref" "evidence"))
                             (eql 1 (gethash "schema_version" grounding))
                             (member (gethash "scope" grounding) +cg-claim-scopes+ :test #'equal)
                             (member (gethash "polarity" grounding) '("positive" "negative" "unknown") :test #'equal)
                             (or (%cg-null-p (gethash "attributed_to_ref" grounding))
                                 (find (gethash "attributed_to_ref" grounding) (gethash "entities" copy)
                                       :key (lambda (e) (gethash "local_ref" e)) :test #'equal))
                             (vectorp (gethash "evidence" grounding))
                             (<= 1 (length (gethash "evidence" grounding)) 8))
                  (error "Invalid closed graph claim scope"))
                (let ((bound nil))
                  (loop for evidence across (gethash "evidence" grounding)
                        for source = (gethash (gethash "source_id" evidence) sources)
                        for quote = (gethash "quote" evidence)
                        do (unless (and (%cg-closed-keys-p evidence '("source_id" "quote"))
                                        source (%cg-present-string-p quote 12000)
                                        (search quote (gethash "text" source) :test #'char=))
                             (error "Claim quote is not in its exact supplied source"))
                           (push (%cg-object "source_id" (gethash "source_id" source)
                                  "speaker_id" (gethash "speaker_id" source)
                                  "source_kind" (gethash "kind" source)
                                  "text_sha256" (gethash "text_sha256" source)
                                  "quote" quote) bound))
                  (setf bound (nreverse bound))
                  (setf (gethash "sources" grounding) (coerce bound 'vector)
                        (gethash "source_basis" grounding)
                        (if (every (lambda (s) (member (gethash "source_kind" s)
                                           '("original-utterance" "tool-observation") :test #'equal)) bound)
                            "original" "derived"))
                  (remhash "evidence" grounding)))))
    copy))

(defun %cg-resolve-grounding (descriptor refs)
  (let ((g (gethash "grounding" descriptor)))
    (when g
      (setf (gethash "attributed_entity_id" g)
            (or (gethash (gethash "attributed_to_ref" g) refs) :null))
      (remhash "attributed_to_ref" g))
    g))

(defun %cg-fact-key (subject predicate object statement grounding)
  (if grounding
      (shasht:write-json (vector subject predicate object (%cg-canonical statement)
                                (gethash "scope" grounding) (gethash "polarity" grounding)
                                (gethash "attributed_entity_id" grounding)
                                (gethash "source_basis" grounding)) nil)
      (format nil "~a|~a|~a" subject predicate object)))

(defun %cg-claim-policy-allows-p (fact policy)
  (let* ((g (gethash "grounding" fact))
         (asserted (and g (equal "original" (gethash "source_basis" g))
                        (equal "assertion" (gethash "scope" g))
                        (member (gethash "polarity" g) '("positive" "negative") :test #'equal))))
    (cond ((equal policy "all") t)
          ((equal policy "factual") (or (null g) asserted))
          ((equal policy "grounded-assertions") asserted))))

(defun context-graph-apply-episode (graph episode proposal
                                   &key (reuse-exact-identities-p t) source-packet)
  "Atomically apply a proposal. Legacy inputs remain explicitly ungrounded."
  (%cg-require-legacy-profile graph)
  (let* ((prepared (%cg-prepare-grounding proposal source-packet))
         (episode-id (gethash "episode_id" episode))
         (prior (gethash episode-id (context-graph-episodes graph)))
         (digest (when source-packet
                   (%cg-sha256 (shasht:write-json
                                (%cg-canonical-tree (vector episode proposal source-packet)) nil))))
         (staged (copy-context-graph graph)))
    (when (and prior (or source-packet (gethash "grounded_input_sha256" prior)))
      (unless (and digest (equal digest (gethash "grounded_input_sha256" prior)))
        (error "Grounded episode replay conflicts with its accepted input"))
      (return-from context-graph-apply-episode
        (%cg-object "status" "already-applied" "episode_id" episode-id)))
    (setf (context-graph-entities staged) (%cg-detach (context-graph-entities graph))
          (context-graph-entity-index staged) (%cg-detach (context-graph-entity-index graph))
          (context-graph-facts staged) (%cg-detach (context-graph-facts graph))
          (context-graph-current-triples staged) (%cg-detach (context-graph-current-triples graph))
          (context-graph-episodes staged) (%cg-detach (context-graph-episodes graph))
          (context-graph-corrections staged) (%cg-detach (context-graph-corrections graph)))
    (let ((result (%cg-apply-episode staged episode prepared
                                   :reuse-exact-identities-p reuse-exact-identities-p)))
      (when digest
        (setf (gethash "grounded_input_sha256" (gethash episode-id (context-graph-episodes staged))) digest))
      (setf (context-graph-entities graph) (context-graph-entities staged)
            (context-graph-entity-index graph) (context-graph-entity-index staged)
            (context-graph-facts graph) (context-graph-facts staged)
            (context-graph-current-triples graph) (context-graph-current-triples staged)
            (context-graph-episodes graph) (context-graph-episodes staged)
            (context-graph-corrections graph) (context-graph-corrections staged))
      result)))
