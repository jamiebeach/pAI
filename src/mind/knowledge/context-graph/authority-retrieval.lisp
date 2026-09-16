;;;; Read-only candidate discovery and bounded context selection. No providers.
(in-package :pai.context-graph)

(defparameter +cgr-stop-terms+
  '("a" "an" "the" "and" "or" "of" "to" "in" "on" "at" "for" "from"
    "is" "are" "was" "were" "be" "been" "it" "its" "this" "that" "these"
    "those" "i" "me" "my" "we" "our" "you" "your" "what" "which" "who"
    "when" "where" "how" "do" "does" "did" "can" "could" "would" "please"
    "tell" "say" "said" "about" "remember" "recall" "any" "some" "with"
    "as" "has" "have" "their"))

(defparameter +cgr-semantic-selector-revision+ "separated-nonparticipant-top-cluster-v2")
(defparameter +cgr-semantic-selector-legacy-revisions+ '("separated-top-cluster-v1"))
(defparameter +cgr-semantic-floor-milli+ 590)
(defparameter +cgr-semantic-cluster-band-milli+ 60)
(defparameter +cgr-semantic-separation-milli+ 80)
(defparameter +cgr-semantic-maximum-hits+ 4)

(defun %cgr-semantic-hit-valid-p (hit)
  (and (%cg-closed-keys-p hit '("kind" "id" "score_milli"))
       (equal "entity" (gethash "kind" hit))
       (%cg-authority-string-p (gethash "id" hit) 180)
       (integerp (gethash "score_milli" hit))
       (<= 0 (gethash "score_milli" hit) 1000)))

(defun %cgr-semantic-hit-before-p (left right)
  (or (> (gethash "score_milli" left) (gethash "score_milli" right))
      (and (= (gethash "score_milli" left) (gethash "score_milli" right))
           (string< (gethash "id" left) (gethash "id" right)))))

(defun %cgr-select-semantic-hits (candidates)
  "Select a small separated top cluster from a complete entity score set.
Scores discover candidates only; they never establish a fact or its source."
  (unless (and (%cg-authority-array-p candidates 1024)
               (every #'%cgr-semantic-hit-valid-p candidates))
    (%cg-authority-fail "RETRIEVAL_SEMANTIC_CANDIDATES_INVALID"))
  (let ((seen (make-hash-table :test #'equal)) (rows nil))
    (loop for candidate across candidates for id = (gethash "id" candidate) do
      (when (gethash id seen)
        (%cg-authority-fail "RETRIEVAL_SEMANTIC_CANDIDATES_INVALID"))
      (setf (gethash id seen) t)
      (push (%cg-detach candidate) rows))
    (setf rows (sort rows #'%cgr-semantic-hit-before-p))
    (if (or (null rows) (< (gethash "score_milli" (first rows)) +cgr-semantic-floor-milli+))
        #()
        (let* ((top (gethash "score_milli" (first rows)))
               (cluster (remove-if (lambda (row)
                                     (< (gethash "score_milli" row)
                                        (- top +cgr-semantic-cluster-band-milli+))) rows))
               (outside (nth (length cluster) rows)))
          (if (and outside
                   (< (- top (gethash "score_milli" outside))
                      +cgr-semantic-separation-milli+))
              #()
              (coerce (subseq cluster 0 (min +cgr-semantic-maximum-hits+ (length cluster))) 'vector))))))

(defun context-graph-build-semantic-receipt
    (graph agent-id persona-id query source-packet encoder-revision candidates
     &key (candidate-status "complete"))
  "Bind a complete, externally scored current-entity set to one read request.
The scoring adapter is impure; selection and authority binding remain pure here."
  (unless (and (context-graph-p graph)
               (%cg-authority-string-p query 1000)
               (%cg-authority-string-p encoder-revision 180)
               (equal "complete" candidate-status)
               (%cg-closed-keys-p source-packet '("schema_version" "sources"))
               (eql 2 (gethash "schema_version" source-packet))
               (%cg-authority-array-p (gethash "sources" source-packet) 128)
               (%cg-authority-array-p candidates 1024)
               (every #'%cgr-semantic-hit-valid-p candidates))
    (%cg-authority-fail "RETRIEVAL_SEMANTIC_PRODUCER_INVALID"))
  (let ((entity-ids
          (remove-if-not
           (lambda (id)
             (eq :null (gethash "participant_role"
                         (%cg-authority-current-descriptor graph id))))
           (context-graph-entity-scan-index graph)))
        (supplied (make-hash-table :test #'equal)))
    (unless (= (length candidates) (length entity-ids))
      (%cg-authority-fail "RETRIEVAL_SEMANTIC_PRODUCER_INCOMPLETE"))
    (loop for hit across candidates for id = (gethash "id" hit) do
      (when (or (gethash id supplied) (not (find id entity-ids :test #'equal)))
        (%cg-authority-fail "RETRIEVAL_SEMANTIC_PRODUCER_INVALID"))
      (setf (gethash id supplied) t))
    (%cg-object
     "query_sha256" (%cg-sha256 query)
     "projection_watermark" (%cg-authority-watermark graph agent-id persona-id)
     "source_packet_digest" (%cg-authority-digest "retrieval-sources" source-packet)
     "encoder_revision" encoder-revision
     "selector_revision" +cgr-semantic-selector-revision+
     "hits" (%cgr-select-semantic-hits candidates))))

(defun %cgr-tokens (text)
  (remove-if (lambda (term) (member term +cgr-stop-terms+ :test #'equal))
             (%cg-query-tokens (substitute #\Space #\_ text))))

(defun %cgr-focus-terms (terms)
  "Read-path normalization only; never used to build generation contexts."
  (remove-duplicates
   (loop for term in terms
         unless (member term '("issue" "issues" "mentioned" "mention" "type" "types"
                               "operator" "persona" "active" "many" "already"
                               "problem" "problems" "concern" "concerns") :test #'equal)
           collect (if (and (> (length term) 3) (char= #\s (char term (1- (length term))))
                            (not (member (char term (- (length term) 2)) '(#\s #\i #\u))))
                       (subseq term 0 (1- (length term))) term)) :test #'equal))

(defun %cgr-name-family-term-p (term)
  (member term '("name" "named" "called" "identity" "identify"
                 "identifie" "identified") :test #'equal))

(defun %cgr-expand-query-terms (terms)
  "Small, general lexical equivalences for answer-bearing relationship text."
  (remove-duplicates
   (append terms
           (when (some #'%cgr-name-family-term-p terms)
             '("name" "named" "called" "identity" "identify" "identifie" "identified")))
   :test #'equal))

(defun %cgr-name-query-row-p (row original-terms)
  "A lexical name synonym cannot satisfy an unmatched specific qualifier.
`my name` has no surviving qualifier; `server called` must also match server."
  (let ((qualifiers (remove-if #'%cgr-name-family-term-p original-terms)))
    (or (notany #'%cgr-name-family-term-p original-terms)
        (null qualifiers)
        (some (lambda (term) (find term (gethash "matched_terms" row) :test #'equal))
              qualifiers))))

(defun %cgr-read-score (terms fields focused)
  (%cgr-score terms (if focused
                       (mapcar (lambda (field) (list (first field) (second field)
                                                     (%cgr-focus-terms (third field)))) fields)
                       fields)))

(defun %cgr-focused-pools (entities facts sources terms)
  "Prefer answer-bearing facts; proximity alone is not conversation relevance."
  (labels ((anchored (row)
             (and (some (lambda (signal)
                     (member signal '("label" "alias" "category" "statement" "evidence" "type-meaning"
                                      "semantic" "semantic-endpoint") :test #'equal))
                   (gethash "signals" row))
                  ;; Definition-only matches answer broad category questions,
                  ;; not an unmatched specific attribute or technical qualifier.
                  (or (some (lambda (signal)
                              (member signal '("label" "alias" "category" "statement" "evidence" "semantic" "semantic-endpoint") :test #'equal))
                            (gethash "signals" row))
                      (= (length terms) (length (gethash "matched_terms" row)))))))
    (let* ((anchors (loop for row across entities
                         when (some (lambda (signal) (member signal '("label" "alias" "category") :test #'equal))
                                    (gethash "signals" row))
                           append (coerce (gethash "matched_terms" row) 'list)))
           (kept-facts
             (remove-if-not
              (lambda (row)
                (and (anchored row)
                     (or (null anchors)
                         (some (lambda (term) (member term anchors :test #'equal))
                               (gethash "matched_terms" row))
                         ;; An entity-label anchor is a useful noise filter, but
                         ;; it must not hide a stronger answer stated in accepted
                         ;; evidence using different words from the topic label.
                         (and (>= (length (gethash "matched_terms" row)) 2)
                              (some (lambda (signal)
                                      (member signal '("statement" "evidence") :test #'equal))
                                    (gethash "signals" row))))))
              facts))
           (floor (if (plusp (length kept-facts)) (ceiling (gethash "score" (aref kept-facts 0)) 2) 0))
           (kept-facts (remove-if (lambda (r) (< (gethash "score" r) floor)) kept-facts))
           (covered (loop for row across kept-facts append (coerce (gethash "matched_terms" row) 'list)))
           (kept-sources
             (remove-if-not
              (lambda (row)
                (or (find "semantic" (gethash "signals" row) :test #'equal)
                    (and (equal "original-utterance" (gethash "source_kind" (gethash "value" row)))
                     (>= (length (gethash "matched_terms" row)) (min 2 (length terms)))
                     (some (lambda (term) (not (member term covered :test #'equal))) (gethash "matched_terms" row))))) sources))
           (kept-entities (if (plusp (length kept-facts)) #() (remove-if-not #'anchored entities))))
      (values kept-entities kept-facts kept-sources
              (- (+ (length entities) (length facts) (length sources))
                 (+ (length kept-entities) (length kept-facts) (length kept-sources)))))))

(defun %cgr-specific-facts (facts)
  "Omit a generic relation only when specific facts cover its query evidence
and BOTH nonparticipant endpoints. This is context utility, never admission or
deletion. A unique relation/statement query term preserves the generic fact."
  (remove-if
   (lambda (row)
     (let* ((value (gethash "value" row))
            (endpoints (list (gethash "subject" value) (gethash "object" value))))
       (and (equal "related_to" (gethash "predicate" value))
            (every (lambda (endpoint)
              (and (eq :null (gethash "participant_role" endpoint))
                   (some (lambda (other)
                     (let ((specific (gethash "value" other)))
                       (and (not (equal "related_to" (gethash "predicate" specific)))
                            (subsetp (coerce (gethash "matched_terms" row) 'list)
                                     (coerce (gethash "matched_terms" other) 'list) :test #'equal)
                            (some (lambda (key)
                              (equal (gethash "entity_id" endpoint)
                                     (gethash "entity_id" (gethash key specific)))) '("subject" "object"))))) facts)))
              endpoints)))) facts))

(defun %cgr-query-specific-facts (facts)
  "Prefer specific answer facts when they cover the query evidence carried by
a generic edge. Predicate, statement and semantic matches keep the generic edge."
  (remove-if
   (lambda (row)
     (let* ((value (gethash "value" row))
            (signals (coerce (gethash "signals" row) 'list))
            (endpoint-ids (loop for key in '("subject" "object")
                                for endpoint = (gethash key value)
                                when (eq :null (gethash "participant_role" endpoint))
                                  collect (gethash "entity_id" endpoint)))
            (covered
              (remove-duplicates
               (loop for other across facts for specific = (gethash "value" other)
                     unless (equal "related_to" (gethash "predicate" specific))
                       when (some (lambda (key)
                                    (member (gethash "entity_id" (gethash key specific)) endpoint-ids :test #'equal))
                                  '("subject" "object"))
                         append (coerce (gethash "matched_terms" other) 'list)) :test #'equal)))
       (and (equal "related_to" (gethash "predicate" value)) endpoint-ids
            (notany (lambda (signal)
                      (member signal '("predicate" "predicate-meaning" "statement" "evidence" "semantic" "semantic-endpoint")
                              :test #'equal)) signals)
            (subsetp (coerce (gethash "matched_terms" row) 'list) covered :test #'equal)))) facts))

(defun context-graph-retrieval-lexicon (ontology)
  "Derive positive meaning terms from policy definitions, never exclusions.
ONTOLOGY is the full versioned ontology body, not the reduced runtime signature."
  (let ((types (make-hash-table :test #'equal)) (predicates (make-hash-table :test #'equal)))
    (loop for key in '("entity_types" "predicates") for table in (list types predicates) do
      (unless (%cg-authority-array-p (gethash key ontology) 128) (%cg-authority-fail "RETRIEVAL_LEXICON_INVALID"))
      (loop for row across (gethash key ontology) do
        (unless (and (hash-table-p row) (%cg-authority-string-p (gethash "name" row) 80)
                     (%cg-authority-string-p (gethash "definition" row) 2000)
                     (not (gethash (gethash "name" row) table)))
          (%cg-authority-fail "RETRIEVAL_LEXICON_INVALID"))
        (setf (gethash (gethash "name" row) table) (gethash "definition" row))))
    (%cg-object "schema_version" 1 "entity_types" types "predicates" predicates)))

(defun %cgr-validate-lexicon (graph lexicon)
  (unless (and (%cg-closed-keys-p lexicon '("schema_version" "entity_types" "predicates"))
               (eql 1 (gethash "schema_version" lexicon))) (%cg-authority-fail "RETRIEVAL_LEXICON_INVALID"))
  (loop for key in '("entity_types" "predicates") for table = (gethash key lexicon) do
    (unless (and (hash-table-p table) (<= (hash-table-count table) 128)) (%cg-authority-fail "RETRIEVAL_LEXICON_INVALID"))
    (maphash (lambda (name definition)
               (unless (and (%cg-authority-string-p name 80) (%cg-authority-string-p definition 2000)
                            (if (equal key "entity_types") (%cg-type-declared-p (context-graph-ontology graph) name)
                                (find name (gethash "edge_types" (context-graph-ontology graph))
                                      :test #'equal :key (lambda (e) (gethash "name" e)))))
                 (%cg-authority-fail "RETRIEVAL_LEXICON_INVALID"))) table)))

(defun %cgr-entity-fields (entity lexicon)
  (list (list "label" 8 (%cgr-tokens (gethash "label" entity)))
        (list "alias" 7 (mapcan #'%cgr-tokens (coerce (gethash "aliases" entity) 'list)))
        (list "category" 5 (mapcan #'%cgr-tokens (coerce (gethash "classifications" entity) 'list)))
        (list "kind" 3 (%cgr-tokens (gethash "kind" entity)))
        (list "type-meaning" 2 (%cgr-tokens (gethash (gethash "kind" entity) (gethash "entity_types" lexicon) "")))))

(defun %cgr-score (terms fields)
  ;; Each distinct term contributes once, at its strongest field weight.
  (let ((score 0) (matched nil) (signals nil))
    (dolist (term terms)
      (let ((best 0) (signal nil))
        (dolist (field fields)
          (when (and (member term (third field) :test #'equal) (> (second field) best))
            (setf best (second field) signal (first field))))
        (when (plusp best) (incf score (+ 2 best)) (push term matched) (pushnew signal signals :test #'equal))))
    (values score (coerce (nreverse matched) 'vector) (coerce (sort signals #'string<) 'vector))))

(defun %cgr-before-p (a b)
  (or (> (gethash "score" a) (gethash "score" b))
      (and (= (gethash "score" a) (gethash "score" b)) (string< (gethash "id" a) (gethash "id" b)))))

(defun %cgr-term-in-range-p (term text start end)
  ;; Check original boundaries, not artificial excerpt boundaries.
  (loop for position = (search term text :start2 start :end2 end :test #'char-equal)
        then (search term text :start2 (1+ position) :end2 end :test #'char-equal)
        while position thereis
          (and (or (zerop position) (not (alphanumericp (char text (1- position)))))
               (let ((after (+ position (length term))))
                 (or (= after (length text)) (not (alphanumericp (char text after))))))))

(defun %cgr-window (source terms)
  "Choose a bounded exact window with the best distinct-term coverage."
  (let* ((text (gethash "text" source)) (length (length text)) (best nil) (best-score 0))
    (loop for start from 0 below length by 240
          for end = (min length (+ start 480))
          for excerpt = (subseq text start end)
          for hits = (remove-if-not (lambda (term) (%cgr-term-in-range-p term text start end)) terms)
          when (> (length hits) best-score) do
            (setf best-score (length hits)
                  best (%cg-object "source_id" (gethash "source_id" source) "speaker_role" (gethash "role" (gethash "identity" source))
                                   "source_kind" (gethash "kind" source) "timestamp" (gethash "timestamp" source)
                                   "text_sha256" (gethash "text_sha256" source) "start_char" start "end_char" end
                                   "text" excerpt "partial" (if (or (> start 0) (< end length)) :true :false)
                                   "interpretation" "source-text-not-an-admitted-fact")))
    (values best best-score)))

(defun %cgr-factual-p (fact)
  (let ((g (gethash "grounding" fact)))
    (and (equal "current" (gethash "status" fact)) (equal "assertion" (gethash "scope" g))
         (equal "positive" (gethash "polarity" g)) (equal "original" (gethash "source_basis" g))
         (member (gethash "evidence_status" fact) '("direct" "prior-graph") :test #'equal))))

(defun %cgr-proximity (entity source terms window)
  (let ((text (gethash "text" source)) (start (gethash "start_char" window)) (end (gethash "end_char" window)) (best 0))
    (dolist (name (cons (gethash "label" entity) (coerce (gethash "aliases" entity) 'list)))
      (when (%cgr-term-in-range-p name text start end)
        (dolist (term terms)
          (when (%cgr-term-in-range-p term text start end)
            (let ((distance (abs (- (search name text :start2 start :end2 end :test #'char-equal)
                                    (search term text :start2 start :end2 end :test #'char-equal)))))
              (setf best (max best (cond ((<= distance 32) 4) ((<= distance 96) 2) ((<= distance 160) 1) (t 0))))))))) best))

(defun %cgr-row (kind id score matched signals payload)
  (%cg-object "kind" kind "id" id "score" score "matched_terms" matched "signals" signals "value" payload))

(defun %cgr-bytes (value)
  (length (sb-ext:string-to-octets (%cg-authority-canonical-json value) :external-format :utf-8)))

(defun %cgr-claim-excerpts (fact)
  "Bounded, exact admitted spans, not newly inferred support or rewritten text."
  (let ((rows nil) (seen nil) (used 0))
    (loop for record across (gethash "evidence_records" fact) repeat 16 while (< (length rows) 2) do
      (loop for span across (gethash "accepted_sources" record) repeat 128 while (< (length rows) 2)
            for quote = (gethash "quote" span)
            for key = (list (gethash "source_id" span) (gethash "start_char" span) (gethash "end_char" span))
            when (and (< (length rows) 2) (<= (+ used (length quote)) 1024)
                      (not (member key seen :test #'equal))) do
              (push key seen) (incf used (length quote))
              (push (%cg-object "source_id" (first key) "start_char" (second key) "end_char" (third key)
                                "text_sha256" (gethash "text_sha256" span)
                                "source_kind" (gethash "source_kind" span) "quote" quote) rows)))
    (coerce (nreverse rows) 'vector)))

(defun %cgr-answer-evidence-tokens (excerpt)
  "Accepted evidence remains provenance, but an interrogative excerpt is not
itself an answer-bearing retrieval statement."
  (let ((quote (gethash "quote" excerpt "")))
    (if (find #\? quote) nil (%cgr-tokens quote))))

(defun %cgr-context-pools (entities facts sources)
  "Keep broad discovery out of context unless it adds corroborated relevance."
  (let* ((direct (remove-if-not (lambda (e) (some (lambda (s) (not (equal s "source-proximity"))) (gethash "signals" e))) entities))
         (entity-pool (if (plusp (length direct)) direct entities))
         (entity-floor (if (plusp (length entity-pool))
                           (if (plusp (length direct)) (ceiling (gethash "score" (aref entity-pool 0)) 3)
                               (gethash "score" (aref entity-pool 0))) 0))
         (kept-entities (remove-if (lambda (e) (< (gethash "score" e) entity-floor)) entity-pool))
         (fact-floor (if (plusp (length facts)) (ceiling (gethash "score" (aref facts 0)) 3) 0))
         (kept-facts (remove-if (lambda (f) (< (gethash "score" f) fact-floor)) facts))
         (source-ids (append (loop for e across kept-entities append (coerce (gethash "source_proximity_ids" (gethash "value" e)) 'list))
                             (loop for f across kept-facts append (coerce (gethash "source_ids" (gethash "value" f)) 'list))))
         (kept-sources (if (zerop (length direct)) sources
                          (remove-if-not (lambda (s) (or (member (gethash "id" s) source-ids :test #'equal)
                                                        (>= (length (gethash "matched_terms" s)) 2)
                                                        (find "semantic" (gethash "signals" s) :test #'equal))) sources))))
    (values kept-entities kept-facts kept-sources
            (- (+ (length entities) (length facts) (length sources))
               (+ (length kept-entities) (length kept-facts) (length kept-sources))))))

(defun %cgr-select (entities facts sources maximum-bytes limits)
  "Return the exact packet to inject, never the broader discovery diagnostics."
  (let* ((packet (%cg-object "schema_version" 1 "entities" #() "facts" #() "sources" #()))
         (queues (list (coerce entities 'list) (coerce facts 'list) (coerce sources 'list)))
         (keys '("entities" "facts" "sources")) (skipped 0) (seen (make-hash-table :test #'equal)))
    (loop while (some #'identity queues) do
      (loop for tail on queues for key in keys for limit across limits do
        (when (first tail)
          (let* ((row (pop (first tail))) (prior (gethash key packet)) (value (gethash "value" row))
                 (identity (if (equal key "facts")
                               (%cg-authority-digest "context-fact" (vector (gethash "entity_id" (gethash "subject" value))
                                 (gethash "predicate" value) (gethash "entity_id" (gethash "object" value))
                                 (gethash "grounding" value) (gethash "temporal" value)
                                 (gethash "statement" value)))
                               (concatenate 'string key ":" (gethash "id" row)))))
            (if (or (gethash identity seen) (>= (length prior) limit)) (incf skipped)
                (progn
                  (setf (gethash key packet) (concatenate 'vector prior (vector row)))
                  (if (> (%cgr-bytes packet) maximum-bytes)
                      (progn (setf (gethash key packet) prior) (incf skipped))
                      (setf (gethash identity seen) t))))))))
    (values packet skipped)))

(defun %cg-authority-retrieve (graph agent-id persona-id query
                             &key (source-packet (%cg-object "schema_version" 2 "sources" #()))
                               (lexicon (%cg-object "schema_version" 1 "entity_types" (make-hash-table :test #'equal)
                                                    "predicates" (make-hash-table :test #'equal)))
                               (semantic-receipt :null) (scan-limit 1024) (source-limit 128)
                               (candidate-limit 12) (maximum-bytes 6000) (context-limits #(4 4 3)) (focused nil)
                               (specific-relations nil) (query-specific-relations nil) (factual-entities nil))
  "Private owner-side read on an already-authorized partition and source packet.
No access grant, embedding request, writes, inference or identity resolution.
The owner supplies complete source/semantic bindings; scans are explicitly bounded."
  (unless (and (context-graph-p graph) (equal "context-graph-authority-v1" (context-graph-authority-profile graph))
               (%cg-authority-string-p query 1000) (member focused '(t nil))
               (member specific-relations '(t nil)) (or (not specific-relations) focused)
               (member query-specific-relations '(t nil)) (member factual-entities '(t nil))
               (or (not query-specific-relations) focused) (or (not factual-entities) focused)
               (not (and specific-relations (or query-specific-relations factual-entities)))
               (integerp scan-limit) (<= 1 scan-limit 4096) (integerp source-limit) (<= 1 source-limit 128)
               (integerp candidate-limit) (<= 1 candidate-limit 32) (integerp maximum-bytes) (<= 128 maximum-bytes 16000)
               (%cg-authority-array-p context-limits 3 3) (every (lambda (n) (and (integerp n) (<= 0 n 8))) context-limits)
               (%cg-closed-keys-p source-packet '("schema_version" "sources")) (eql 2 (gethash "schema_version" source-packet))
               (%cg-authority-array-p (gethash "sources" source-packet) 128))
    (%cg-authority-fail "RETRIEVAL_INPUT_INVALID"))
  (%cgr-validate-lexicon graph lexicon)
  (let* ((watermark (%cg-authority-watermark graph agent-id persona-id))
         (original-terms (if focused (%cgr-focus-terms (%cgr-tokens query)) (%cgr-tokens query)))
         (all-terms (if focused (%cgr-expand-query-terms original-terms)
                        (%cgr-tokens query)))
         (terms (subseq all-terms 0 (min 32 (length all-terms))))
         (entity-ids (context-graph-entity-scan-index graph)) (fact-ids (context-graph-fact-scan-index graph))
         (entity-count (min scan-limit (length entity-ids))) (fact-count (min scan-limit (length fact-ids)))
         (source-rows (gethash "sources" source-packet)) (source-count (min source-limit (length source-rows)))
         (source-index (make-hash-table :test #'equal)) (windows (make-hash-table :test #'equal))
         (semantic (make-hash-table :test #'equal)) (factual-entity-ids (make-hash-table :test #'equal))
         (ignored-semantic 0)
         (entities nil) (facts nil) (sources nil))
    ;; Bound the entire envelope before hashing optional semantic bindings.
    ;; Lexical scanning and fidelity validation still inspect only the prefix.
    (unless (every (lambda (s) (and (hash-table-p s) (stringp (gethash "text" s))
                                   (<= (length (gethash "text" s)) 12000)
                                   (%cg-span-source-valid-p s))) source-rows)
      (%cg-authority-fail "RETRIEVAL_SOURCE_INVALID"))
    (dotimes (i source-count)
      (let ((source (aref source-rows i)))
        (unless (and (%cg-span-source-valid-p source) (<= (length (gethash "text" source)) 12000)
                     (equal (%cg-sha256 (gethash "text" source)) (gethash "text_sha256" source))
                     (not (gethash (gethash "source_id" source) source-index)))
          (%cg-authority-fail "RETRIEVAL_SOURCE_INVALID"))
        (setf (gethash (gethash "source_id" source) source-index) source)))
    (unless (eq :null semantic-receipt)
      (let ((selector-revision (gethash "selector_revision" semantic-receipt :null)))
      (unless (and (%cg-closed-keys-p semantic-receipt
                     (if (eq :null selector-revision)
                         '("query_sha256" "projection_watermark" "source_packet_digest" "encoder_revision" "hits")
                         '("query_sha256" "projection_watermark" "source_packet_digest" "encoder_revision" "selector_revision" "hits")))
                   (equal (%cg-sha256 query) (gethash "query_sha256" semantic-receipt))
                   (%cg-authority-equal-p watermark (gethash "projection_watermark" semantic-receipt))
                   (equal (%cg-authority-digest "retrieval-sources" source-packet) (gethash "source_packet_digest" semantic-receipt))
                   (%cg-authority-string-p (gethash "encoder_revision" semantic-receipt) 180)
                   (or (eq :null selector-revision)
                       (equal +cgr-semantic-selector-revision+ selector-revision)
                       (member selector-revision +cgr-semantic-selector-legacy-revisions+ :test #'equal))
                   (%cg-authority-array-p (gethash "hits" semantic-receipt) 128))
        (%cg-authority-fail "RETRIEVAL_SEMANTIC_BINDING_INVALID"))
      (loop for hit across (gethash "hits" semantic-receipt) do
        (unless (and (%cg-closed-keys-p hit '("kind" "id" "score_milli"))
                     (member (gethash "kind" hit) '("entity" "source") :test #'equal)
                     (%cg-authority-string-p (gethash "id" hit) 180)
                     (integerp (gethash "score_milli" hit)) (<= 0 (gethash "score_milli" hit) 1000))
          (%cg-authority-fail "RETRIEVAL_SEMANTIC_INPUT_INVALID"))
        (let* ((id (gethash "id" hit)) (kind (gethash "kind" hit)) (key (concatenate 'string kind ":" id)))
          (if (if (equal kind "entity") (find id entity-ids :end entity-count :test #'equal) (gethash id source-index))
              (setf (gethash key semantic) (max (gethash key semantic 0) (gethash "score_milli" hit)))
              (incf ignored-semantic))))))
    (labels ((semantic-score (kind id)
               (let ((score (gethash (concatenate 'string kind ":" id) semantic 0)))
                 (if (>= score (if (and (not (eq :null semantic-receipt))
                                        (member (gethash "selector_revision" semantic-receipt :null)
                                                (cons +cgr-semantic-selector-revision+
                                                      +cgr-semantic-selector-legacy-revisions+)
                                                :test #'equal))
                                   +cgr-semantic-floor-milli+ 750))
                     (floor score 100) 0))))
      (dotimes (i source-count)
        (let* ((source (aref source-rows i)) (id (gethash "source_id" source)) (sem (semantic-score "source" id)))
          (multiple-value-bind (window hits) (%cgr-window source terms)
            ;; Semantic-only source excerpts start at zero and are explicitly
            ;; not query-local evidence. Never claim a lexical match for them.
            (when (and (null window) (plusp sem))
              (let ((text (gethash "text" source)))
                (setf window (%cg-object "source_id" id "speaker_role" (gethash "role" (gethash "identity" source))
                                        "source_kind" (gethash "kind" source) "timestamp" (gethash "timestamp" source)
                                        "text_sha256" (gethash "text_sha256" source) "start_char" 0 "end_char" (min 480 (length text))
                                        "text" (subseq text 0 (min 480 (length text))) "partial" (if (> (length text) 480) :true :false)
                                        "interpretation" "semantic-candidate-not-an-admitted-fact"))))
            (when window
              (setf (gethash id windows) window)
              (push (%cgr-row "source" id (+ (* 4 hits) sem)
                              (coerce (remove-if-not (lambda (term) (%cgr-term-in-range-p term (gethash "text" source)
                                                       (gethash "start_char" window) (gethash "end_char" window))) terms) 'vector)
                              (coerce (append (when (plusp hits) '("source-text")) (when (plusp sem) '("semantic"))) 'vector) window) sources)))))
      (dotimes (i entity-count)
        (let* ((id (aref entity-ids i)) (entity (%cg-authority-current-descriptor graph id))
               (sem (semantic-score "entity" id)) (near nil) (proximity 0))
          (loop for source-id in (sort (loop for k being the hash-keys of windows collect k) #'string<)
                for window = (gethash source-id windows)
                for score = (%cgr-proximity entity (gethash source-id source-index) terms window)
                when (plusp score) do
                  (setf proximity (max proximity score))
                  (push (cons source-id score) near))
          (setf near (subseq (sort near (lambda (a b) (if (= (cdr a) (cdr b)) (string< (car a) (car b)) (> (cdr a) (cdr b)))))
                            0 (min 2 (length near))))
          (multiple-value-bind (score matched signals) (%cgr-read-score terms (%cgr-entity-fields entity lexicon) focused)
            (when (or (plusp score) (plusp sem) near)
              (push (%cgr-row "entity" id (+ score sem proximity) matched
                              (concatenate 'vector signals (if (plusp sem) #("semantic") #()) (if near #("source-proximity") #()))
                              (%cg-object "descriptor" entity "source_proximity_ids" (coerce (mapcar #'car near) 'vector)
                                          "interpretation" "current-entity-not-a-relationship-assertion")) entities)))))
    (dotimes (i fact-count)
      (let ((fact (gethash (aref fact-ids i) (context-graph-facts graph))))
        (when (%cgr-factual-p fact)
          (let* ((subject (%cg-authority-current-descriptor graph (gethash "entity_id" (%cg-authority-entity graph (gethash "subject_id" fact)))))
                 (object (%cg-authority-current-descriptor graph (gethash "entity_id" (%cg-authority-entity graph (gethash "object_id" fact)))))
                 (fields (append (%cgr-entity-fields subject lexicon) (%cgr-entity-fields object lexicon)
                                 (list (list "statement" 6 (%cgr-tokens (gethash "fact" fact)))
                                       ;; Accepted excerpts are already bound to this admitted fact by
                                       ;; provenance validation.  They are answer-bearing retrieval text,
                                       ;; not a new admission path.  Prefer an exact supporting utterance
                                       ;; over a different fact whose summary happens to share one term.
                                       (list "evidence" 8
                                             (mapcan #'%cgr-answer-evidence-tokens
                                               (coerce (%cgr-claim-excerpts fact) 'list)))
                                       (list "predicate" 5 (%cgr-tokens (gethash "predicate" fact)))
                                       (list "predicate-meaning" 2 (%cgr-tokens (gethash (gethash "predicate" fact)
                                                                                         (gethash "predicates" lexicon) "")))))))
            (setf (gethash (gethash "entity_id" subject) factual-entity-ids) t
                  (gethash (gethash "entity_id" object) factual-entity-ids) t)
            (multiple-value-bind (score matched signals) (%cgr-read-score terms fields focused)
              (let ((sem (max (semantic-score "entity" (gethash "entity_id" subject))
                              (semantic-score "entity" (gethash "entity_id" object)))))
                (when (plusp sem) (incf score sem) (setf signals (concatenate 'vector signals #("semantic-endpoint")))))
              (when (plusp score)
                (push (%cgr-row "fact" (gethash "fact_id" fact) score matched signals
                                (%cg-object "subject" subject "predicate" (gethash "predicate" fact) "object" object
                                            "grounding" (gethash "grounding" fact) "source_ids" (gethash "accepted_source_ids" fact)
                                            "temporal" (gethash "temporal" fact)
                                            "statement" (gethash "fact" fact)
                                            "evidence_excerpts" (%cgr-claim-excerpts fact)
                                            "accepted_evidence_digest" (gethash "accepted_evidence_digest" fact))) facts)))))))
    (let* ((counts (vector (length entities) (length facts) (length sources)))
           (ranked (mapcar (lambda (rows) (let ((ordered (sort rows #'%cgr-before-p)))
                                           (coerce (subseq ordered 0 (min candidate-limit (length ordered))) 'vector)))
                           (list entities facts sources))))
      (multiple-value-bind (pool-entities pool-facts pool-sources relevance-omitted)
          (if focused (%cgr-focused-pools (first ranked) (second ranked) (third ranked) terms)
              (%cgr-context-pools (first ranked) (second ranked) (third ranked)))
       (when specific-relations
         (let ((kept (%cgr-specific-facts pool-facts)))
           (incf relevance-omitted (- (length pool-facts) (length kept)))
           (setf pool-facts kept)))
       (when query-specific-relations
         (let ((kept (%cgr-query-specific-facts pool-facts)))
           (incf relevance-omitted (- (length pool-facts) (length kept)))
           (setf pool-facts kept)))
       (when focused
         (let ((kept (remove-if-not (lambda (row) (%cgr-name-query-row-p row original-terms))
                                    pool-facts)))
           (incf relevance-omitted (- (length pool-facts) (length kept)))
           (setf pool-facts kept)))
       (when factual-entities
         (let ((kept (remove-if-not (lambda (row) (gethash (gethash "id" row) factual-entity-ids)) pool-entities)))
           (incf relevance-omitted (- (length pool-entities) (length kept)))
           (setf pool-entities kept)))
       (multiple-value-bind (packet skipped) (%cgr-select pool-entities pool-facts pool-sources maximum-bytes context-limits)
        (%cg-detach
         (%cg-object "schema_version" 1 "retrieval_revision"
                     (cond ((and query-specific-relations factual-entities) "focused-kg-v5")
                           (query-specific-relations "focused-specific-relations-v2")
                           (factual-entities "focused-factual-entities-v1")
                           (specific-relations "focused-kg-v2") (focused "focused-kg-v1") (t "bounded-hybrid-kg-v2"))
                     "query_terms" (coerce terms 'vector)
                     "projection_watermark" watermark "context" packet "context_bytes" (%cgr-bytes packet)
                     "query_terms_truncated" (if (> (length all-terms) 32) :true :false)
                     "candidates" (%cg-object "entities" (first ranked) "facts" (second ranked) "sources" (third ranked))
                     "candidate_counts" counts "selection_skipped" skipped
                     "semantic_selection_revision"
                     (if (eq :null semantic-receipt) :null
                         (gethash "selector_revision" semantic-receipt :null))
                     "context_relevance_omitted" relevance-omitted
                     "candidates_truncated" (if (some (lambda (n) (> n candidate-limit)) counts) :true :false)
                     "scan_complete" (if (and (= entity-count (length entity-ids)) (= fact-count (length fact-ids))
                                              (= source-count (length source-rows))) :true :false)
                     "examined" (vector entity-count fact-count source-count) "ignored_semantic_hits" ignored-semantic
                     "database_write_count" 0 "provider_calls" 0))))))))
