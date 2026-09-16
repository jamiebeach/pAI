;;;; authority.lisp -- pure, bounded authority primitives.
;;;; A recovered quote is evidence text, never a semantic admission decision.
(in-package :pai.context-graph)

;; Formation protocols bind this policy while rebuilding and applying their
;; receipts.  The default preserves the authority meaning of older receipts.
(defvar *cg-assertion-evidence-policy* "operator-utterance-v1")
(defvar *cg-claim-identity-protocol* "claim-identity-v1")

(defun %cg-claim-temporal-identity (claim temporal)
  "Coalesce only unbounded enduring family roles in the candidate V3 identity.

Event, temporary and date-bounded claims retain their exact temporal character."
  (let ((character (and temporal (gethash "character" temporal))))
    (if (and (equal *cg-claim-identity-protocol* "claim-identity-v3")
             (member (gethash "predicate" claim)
                     '("parent_of" "daughter_of" "son_of" "spouse_of")
                     :test #'equal)
             (member character '("ongoing-state" "standing-disposition")
                     :test #'equal)
             (every (lambda (key) (eq :null (gethash key temporal)))
                    '("occurred_at" "valid_from" "valid_until")))
        "unbounded-enduring-family"
        character)))

(defparameter +cg-source-kinds+
  '("original-utterance" "prior-agent-utterance" "tool-observation"
    "generated-summary" "retrieval-metadata"))

(define-condition context-graph-authority-input-error (error)
  ((code :initarg :code :reader %cg-authority-error-code))
  (:report (lambda (condition stream)
             (format stream "Invalid graph authority input: ~a"
                     (%cg-authority-error-code condition)))))

(defun %cg-authority-fail (code)
  (error 'context-graph-authority-input-error :code code))

(defun %cg-authority-result (status &optional value code)
  (%cg-detach (%cg-object
   "schema_version" 1 "authority_revision" "context-graph-authority-v1"
   "status" status "value" (if value value :null)
   "diagnostics" (if code
                     (vector (%cg-object "code" code "entity_ordinal" :null
                                         "relationship_ordinal" :null
                                         "source_ordinal" :null))
                     #())
   "omitted_diagnostic_count" 0)))

(defun %cg-authority-string-p (value maximum)
  (and (%cg-present-string-p value maximum)
       (find-if (lambda (ch) (not (find ch '(#\Space #\Tab #\Newline #\Return))))
                value)))

(defun %cg-authority-digest-p (value)
  (and (stringp value) (= 64 (length value))
       (every (lambda (ch) (find ch "0123456789abcdef")) value)))

(defun %cg-span-source-valid-p (source)
  ;; Shape/fidelity validation does not authenticate the supplied principal or
  ;; authorize access. Those decisions require the trusted policy snapshot.
  (and (%cg-closed-keys-p
        source '("source_id" "speaker_id" "kind" "timestamp" "text"
                 "text_sha256" "identity" "resource_ref"))
       (every (lambda (key) (%cg-authority-string-p (gethash key source) 180))
              '("source_id" "speaker_id"))
       (member (gethash "kind" source) +cg-source-kinds+ :test #'equal)
       (let ((timestamp (gethash "timestamp" source)))
         (or (and (integerp timestamp) (not (minusp timestamp)))
             (%cg-authority-string-p timestamp 180)))
       (stringp (gethash "text" source))
       (%cg-authority-digest-p (gethash "text_sha256" source))
       (let ((identity (gethash "identity" source)))
         (and (%cg-closed-keys-p
               identity '("principal_id" "binding_id" "conversation_id" "role"))
              (%cg-authority-string-p (gethash "conversation_id" identity) 180)
              (member (gethash "role" identity)
                      '("operator" "active-persona" "other" "unknown") :test #'equal)
              (every (lambda (key)
                       (or (eq :null (gethash key identity))
                           (%cg-authority-string-p (gethash key identity) 180)))
                     '("principal_id" "binding_id"))))
       (let ((ref (gethash "resource_ref" source)))
         (and (%cg-closed-keys-p ref '("store" "resource_id" "version_id" "component"))
              (member (gethash "store" ref)
                      '("event" "episodic" "semantic" "graph" "artifact" "cache")
                      :test #'equal)
              (member (gethash "component" ref)
                      '("content" "descriptor" "claim" "evidence" "lineage" "embedding")
                      :test #'equal)
              (every (lambda (key) (%cg-authority-string-p (gethash key ref) 180))
                     '("resource_id" "version_id"))))))

(defun %cg-authority-direct-observation-p (span)
  "Recognize evidence classes that can directly support a factual assertion.
The source packet and its resource reference are runtime-authenticated before
this predicate runs; a model cannot grant a source its kind or identity."
  (let ((kind (gethash "source_kind" span))
        (role (gethash "role" (gethash "identity" span))))
    (or (and (equal kind "original-utterance") (equal role "operator"))
        (and (equal kind "tool-observation")
             (member role '("other" "unknown") :test #'equal)))))

(defun %cg-authority-source-basis (spans)
  "Preserve the established original/derived vocabulary for direct evidence."
  (let ((direct (count-if #'%cg-authority-direct-observation-p spans)))
    (cond ((= direct (length spans)) "original")
          ((zerop direct) "derived")
          (t "mixed"))))

(defun %cg-authority-assertion-evidence-p (context relationship)
  "Assertions require exact evidence with protocol-owned authority semantics.
Non-assertions keep their original scope and are not factual retrieval hits."
  (let* ((grounding (gethash "grounding" relationship))
         (citations (gethash "evidence" grounding)))
    (or (not (equal "assertion" (gethash "scope" grounding)))
        (and (plusp (length citations))
             (every (lambda (citation)
                      (let ((span (%cg-citation-exact-span context citation)))
                        (and span
                             (if (equal *cg-assertion-evidence-policy*
                                        "direct-observation-v2")
                                 (%cg-authority-direct-observation-p span)
                                 (and (equal "original-utterance"
                                             (gethash "source_kind" span))
                                      (equal "operator"
                                             (gethash "role"
                                                      (gethash "identity" span))))))))
                    citations)))))

(defun %cg-authority-inference-evidence-p (context relationship)
  "Require exact, authenticated premises without pretending they entail a fact.

Inference may use an operator utterance, a tool observation, or a prior-agent
utterance as a premise. Generated summaries and retrieval metadata may provide
context, but cannot be the only premise for a durable inferred assertion."
  (let* ((grounding (gethash "grounding" relationship))
         (citations (and (hash-table-p grounding)
                         (gethash "evidence" grounding)))
         (spans (and (vectorp citations)
                     (map 'vector
                          (lambda (citation)
                            (%cg-citation-exact-span context citation))
                          citations))))
    (and (equal "assertion" (gethash "scope" grounding))
         (vectorp spans)
         (plusp (length spans))
         (every #'identity spans)
         (find-if
          (lambda (span)
            (member (gethash "source_kind" span)
                    '("original-utterance" "tool-observation"
                      "prior-agent-utterance")
                    :test #'equal))
          spans))))

(defun %cg-span-search-form (text)
  "Return normalized text and half-open original character intervals.
Only the existing emphasis/backtick and whitespace presentation rules apply."
  (let ((characters nil) (starts nil) (ends nil)
        (space-start nil) (space-end nil))
    (loop for ch across text for index from 0
          do (cond
               ((find ch '(#\* #\`) :test #'char=))
               ((find ch '(#\Space #\Tab #\Newline #\Return) :test #'char=)
                (when characters
                  (unless space-start (setf space-start index))
                  (setf space-end (1+ index))))
               (t
                (when space-start
                  (push #\Space characters)
                  (push space-start starts) (push space-end ends)
                  (setf space-start nil space-end nil))
                (push ch characters) (push index starts) (push (1+ index) ends))))
    (values (coerce (nreverse characters) 'string)
            (coerce (nreverse starts) 'vector)
            (coerce (nreverse ends) 'vector))))

(defun %cg-span-occurrences (needle haystack starts ends)
  "Enumerate every overlapping occurrence, or explicitly abandon bounded work."
  (let ((rows nil) (cursor 0))
    (when (zerop (length needle))
      (return-from %cg-span-occurrences (values nil nil)))
    (loop for position = (search needle haystack :start2 cursor :test #'char-equal)
          while position
          do (when (= 128 (length rows))
               (return-from %cg-span-occurrences (values nil t)))
             (push (cons (aref starts position)
                         (aref ends (1- (+ position (length needle))))) rows)
             (setf cursor (1+ position)))
    (values (nreverse rows) nil)))

(defun %cg-span-fragments (quote)
  "Recognize only isolated ASCII triples or Unicode ellipsis, never dot runs."
  (let ((fragments nil) (start 0) (index 0) (size (length quote)))
    (labels ((separator (end)
               (let ((fragment (string-trim '(#\Space #\Tab #\Newline #\Return)
                                            (subseq quote start index))))
                 (when (zerop (length fragment))
                   (return-from %cg-span-fragments nil))
                 (push fragment fragments)
                 (setf index end start end))))
      (loop while (< index size)
            do (cond
                 ((char= (char quote index) (code-char #x2026))
                  (separator (1+ index)))
                 ((char= (char quote index) #\.)
                  (let ((end index))
                    (loop while (and (< end size) (char= (char quote end) #\.))
                          do (incf end))
                    (cond ((= 3 (- end index)) (separator end))
                          ((> (- end index) 3) (return-from %cg-span-fragments nil))
                          (t (setf index end)))))
                 (t (incf index))))
      (let ((last (string-trim '(#\Space #\Tab #\Newline #\Return)
                               (subseq quote start))))
        (unless (and (plusp (length last)) (<= 1 (length fragments) 5))
          (return-from %cg-span-fragments nil))
        (nreverse (cons last fragments))))))

(defun %cg-span-elision (layers)
  "Return one distinct original interval. Multiple alignments may share it."
  (let ((result nil) (work 0))
    (dolist (first (first layers))
      (let ((reachable (list first)))
        (dolist (layer (rest layers))
          (let ((next nil))
            (dolist (candidate layer)
              (dolist (previous reachable)
                (incf work)
                (when (> work 100000)
                  (return-from %cg-span-elision (values nil "incomplete" "SPAN_LIMIT")))
                (when (and (<= (cdr previous) (car candidate))
                           (<= (- (car candidate) (cdr previous)) 512)
                           (<= (- (cdr candidate) (car first)) 1000))
                  (push candidate next)
                  (return))))
            (setf reachable (nreverse next))))
        (dolist (last reachable)
          (let ((interval (cons (car first) (cdr last))))
            (when (and result (not (equal interval result)))
              (return-from %cg-span-elision (values nil "rejected" "SPAN_AMBIGUOUS")))
            (setf result interval)))))
    (if result (values result "accepted" nil)
        (values nil "rejected" "SPAN_NOT_FOUND"))))

(defun %cg-resolve-text-span (source quote &key (allow-elision-p t))
  "Shared fidelity engine, also used by the explicitly legacy adapter.
Returns interval, method, status and diagnostic; never assigns source authority."
  (unless (and (stringp source) (stringp quote))
    (return-from %cg-resolve-text-span (values nil nil "rejected" "SPAN_INVALID")))
  (when (or (> (length source) 30000) (> (length quote) 1000))
    (return-from %cg-resolve-text-span (values nil nil "incomplete" "SPAN_LIMIT")))
  (unless (%cg-authority-string-p quote 1000)
    (return-from %cg-resolve-text-span (values nil nil "rejected" "SPAN_INVALID")))
  (let ((position (search quote source :test #'char=)))
    (when position
      (return-from %cg-resolve-text-span
        (values (cons position (+ position (length quote))) "exact" "accepted" nil))))
  (multiple-value-bind (source-form starts ends) (%cg-span-search-form source)
    (let ((quote-form (%cg-span-search-form quote)))
      (multiple-value-bind (matches over-limit)
          (%cg-span-occurrences quote-form source-form starts ends)
        (when over-limit
          (return-from %cg-resolve-text-span (values nil nil "incomplete" "SPAN_LIMIT")))
        (when (rest matches)
          (return-from %cg-resolve-text-span (values nil nil "rejected" "SPAN_AMBIGUOUS")))
        (when matches
          (if (> (- (cdar matches) (caar matches)) 1000)
              (return-from %cg-resolve-text-span (values nil nil "incomplete" "SPAN_LIMIT"))
              (return-from %cg-resolve-text-span
                (values (first matches) "presentation" "accepted" nil))))))
    (unless allow-elision-p
      (return-from %cg-resolve-text-span (values nil nil "rejected" "SPAN_NOT_FOUND")))
    (let ((fragments (%cg-span-fragments quote)) (layers nil))
      (unless fragments
        (return-from %cg-resolve-text-span (values nil nil "rejected" "SPAN_NOT_FOUND")))
      (dolist (fragment fragments)
        (let ((normalized (%cg-span-search-form fragment)))
          (unless (find-if #'alphanumericp normalized)
            (return-from %cg-resolve-text-span (values nil nil "rejected" "SPAN_INVALID")))
          (multiple-value-bind (matches over-limit)
              (%cg-span-occurrences normalized source-form starts ends)
            (when over-limit
              (return-from %cg-resolve-text-span (values nil nil "incomplete" "SPAN_LIMIT")))
            (unless matches
              (return-from %cg-resolve-text-span (values nil nil "rejected" "SPAN_NOT_FOUND")))
            (push matches layers))))
      (multiple-value-bind (interval status code) (%cg-span-elision (nreverse layers))
        (values interval (and interval "ellipsis") status code)))))

(defun context-graph-resolve-source-span (source proposed-quote)
  "Resolve exact text and retain protected provenance; this grants no access."
  (unless (%cg-span-source-valid-p source)
    (%cg-authority-fail "SOURCE_INVALID"))
  ;; Check length before hashing so oversized input does not imply unbounded work.
  (when (> (length (gethash "text" source)) 30000)
    (return-from context-graph-resolve-source-span
      (%cg-authority-result "incomplete" nil "SPAN_LIMIT")))
  (unless (equal (%cg-sha256 (gethash "text" source)) (gethash "text_sha256" source))
    (%cg-authority-fail "SOURCE_HASH_INVALID"))
  (multiple-value-bind (interval method status code)
      (%cg-resolve-text-span (gethash "text" source) proposed-quote)
    (%cg-authority-result
     status
     (when interval
       (%cg-object
        "source_id" (gethash "source_id" source)
        "speaker_id" (gethash "speaker_id" source)
        "source_kind" (gethash "kind" source)
        "timestamp" (gethash "timestamp" source)
        "text_sha256" (gethash "text_sha256" source)
        "identity" (gethash "identity" source)
        "resource_ref" (gethash "resource_ref" source)
        "quote" (subseq (gethash "text" source) (car interval) (cdr interval))
        "start_char" (car interval) "end_char" (cdr interval)
        "method" method "resolver_revision" "exact-source-span-v1"))
     code)))

(defun context-graph-resolve-legacy-source-quote (source quote)
  "Compatibility for the old formation generation; grants no new authority.
Elision is intentionally unavailable until v4 preparation and review are wired."
  (multiple-value-bind (interval) (%cg-resolve-text-span source quote :allow-elision-p nil)
    (when interval (subseq source (car interval) (cdr interval)))))

(defun %cg-authority-array-p (value maximum &optional (minimum 0))
  (and (vectorp value) (not (stringp value)) (<= minimum (length value) maximum)))

(defun %cg-authority-strings-p (value maximum length-limit)
  (and (%cg-authority-array-p value maximum)
       (every (lambda (item) (%cg-authority-string-p item length-limit)) value)
       (= (length value) (length (remove-duplicates value :test #'equal)))))

(defun %cg-authority-canonical-json (value)
  (handler-case (pai.memory-access:memory-access-canonical-json value)
    (pai.memory-access:memory-access-input-error () (%cg-authority-fail "CANONICAL_INPUT_INVALID"))))

(defun %cg-authority-digest (tag value)
  (%cg-sha256 (%cg-authority-canonical-json (vector tag value))))

(defun %cg-authority-equal-p (left right)
  ;; EQUALP is case-insensitive on strings, including strings inside tables.
  (equal (%cg-authority-canonical-json left) (%cg-authority-canonical-json right)))

(defun %cg-validate-sources (packet)
  "Validate new-profile shape and fidelity. No policy or semantic authority."
  (unless (and (%cg-closed-keys-p packet '("schema_version" "sources"))
               (eql 2 (gethash "schema_version" packet))
               (%cg-authority-array-p (gethash "sources" packet) 128 1))
    (%cg-authority-fail "SOURCE_INVALID"))
  (let ((index (make-hash-table :test #'equal)) (size 0))
    (loop for source across (gethash "sources" packet)
          do (unless (and (%cg-span-source-valid-p source)
                          (<= (length (gethash "text" source)) 30000)
                          (<= (incf size (length (gethash "text" source))) 70000)
                          (not (gethash (gethash "source_id" source) index))
                          (equal (%cg-sha256 (gethash "text" source)) (gethash "text_sha256" source)))
               (%cg-authority-fail "SOURCE_INVALID"))
             (setf (gethash (gethash "source_id" source) index) source))
    index))

(defun %cg-validate-participant-registry (context)
  (unless (hash-table-p context) (%cg-authority-fail "CONTEXT_INVALID"))
  (let ((participants (gethash "participants" context)))
    (unless (%cg-authority-array-p participants 2 2)
      (%cg-authority-fail "PARTICIPANTS_INVALID"))
    (loop for role in '("operator" "active-persona")
          for expected-kind in '("person" "agent")
          for matches = (loop for p across participants
                              when (and (hash-table-p p) (equal role (gethash "role" p))) collect p)
          do (unless (= 1 (length matches)) (%cg-authority-fail "PARTICIPANTS_INVALID"))
             (let ((p (first matches)))
               (unless (and (%cg-closed-keys-p
                             p '("role" "speaker_id" "principal_id" "identity_binding_id"
                                 "local_ref" "entity_id" "kind" "label" "aliases"))
                            (every (lambda (key) (%cg-authority-string-p (gethash key p) 180))
                                   '("speaker_id" "principal_id" "identity_binding_id" "entity_id"))
                            (equal (gethash "speaker_id" p) (gethash "principal_id" p))
                            (equal (gethash "local_ref" p) (format nil "runtime:~a" role))
                            (equal (gethash "kind" p) expected-kind)
                            (%cg-authority-string-p (gethash "label" p) 240)
                            (%cg-authority-strings-p (gethash "aliases" p) 8 240))
                 (%cg-authority-fail "PARTICIPANTS_INVALID"))))
    (dolist (key '("entity_id" "principal_id" "identity_binding_id"))
      (when (equal (gethash key (aref participants 0)) (gethash key (aref participants 1)))
        (%cg-authority-fail "PARTICIPANTS_INVALID")))
    participants))

(defun %cg-validate-source-participants (sources participants)
  ;; Verify runtime speaker bindings independently of model attribution.
  (maphash
   (lambda (id source)
     (declare (ignore id))
     (let* ((identity (gethash "identity" source))
            (role (gethash "role" identity))
            (participant (find (gethash "principal_id" identity) participants
                               :key (lambda (p) (gethash "principal_id" p)) :test #'equal)))
       (when (or participant (member role '("operator" "active-persona") :test #'equal))
         (unless (and participant
                      (equal role (gethash "role" participant))
                      (equal (gethash "binding_id" identity) (gethash "identity_binding_id" participant))
                      (equal (gethash "speaker_id" source) (gethash "principal_id" participant))
                      (equal (gethash "kind" source)
                             (if (equal role "operator") "original-utterance" "prior-agent-utterance")))
           (%cg-authority-fail "SOURCE_PARTITION_MISMATCH")))))
   sources)
  t)

(defun context-graph-normalize-participants (proposal context)
  "Inject trusted reserved identities; model classifications never bind a person.
CONTEXT is supplied by the authenticated runtime, never a provider object.
This primitive verifies binding consistency; access admission is a separate gate."
  (let* ((participants (%cg-validate-participant-registry context))
         (sources (%cg-validate-sources (gethash "source_packet" context)))
         (repairs nil) (entities nil) (bindings nil)
         (refs (make-hash-table :test #'equal)))
    (unless (and (%cg-closed-keys-p proposal '("schema_version" "ontology_revision" "entities"
                                               "relationships" "entity_revisions"))
                 (eql 4 (gethash "schema_version" proposal))
                 (%cg-authority-string-p (gethash "ontology_revision" proposal) 180)
                 (%cg-authority-array-p (gethash "entities" proposal) 24)
                 (%cg-authority-array-p (gethash "relationships" proposal) 48)
                 (%cg-authority-array-p (gethash "entity_revisions" proposal) 8))
      (%cg-authority-fail "PROPOSAL_INVALID"))
    (%cg-validate-source-participants sources participants)
    (loop for entity across (gethash "entities" proposal) for ordinal from 0
          do (unless (and (%cg-closed-keys-p entity '("local_ref" "kind" "label" "aliases" "classifications"
                                                     "identity_action" "existing_node_id" "evidence_status" "evidence_note"))
                          (%cg-authority-string-p (gethash "local_ref" entity) 80)
                          (%cg-authority-string-p (gethash "kind" entity) 80)
                          (%cg-authority-string-p (gethash "label" entity) 240)
                          (%cg-authority-strings-p (gethash "aliases" entity) 8 240)
                          (%cg-authority-strings-p (gethash "classifications" entity) 8 120)
                          (member (gethash "identity_action" entity) '("NEW" "LINK_EXISTING" "REVISE_EXISTING") :test #'equal)
                          (member (gethash "evidence_status" entity) '("unreviewed" "direct" "prior-graph" "inference") :test #'equal)
                          (%cg-authority-string-p (gethash "evidence_note" entity) 600))
               (%cg-authority-fail "ENTITY_INVALID"))
             (let ((ref (gethash "local_ref" entity)) (target (gethash "existing_node_id" entity)))
               (when (or (and (<= 8 (length ref)) (string-equal "runtime:" ref :end2 8))
                         (gethash ref refs)
                         (find target participants :key (lambda (p) (gethash "entity_id" p)) :test #'equal))
                 (return-from context-graph-normalize-participants
                   (%cg-authority-result "rejected" nil "RESERVED_REF_CONFLICT")))
               (unless (if (equal "NEW" (gethash "identity_action" entity))
                           (eq :null target) (%cg-authority-string-p target 180))
                 (%cg-authority-fail "ENTITY_INVALID"))
               (setf (gethash ref refs) t))
             (let* ((copy (%cg-detach entity))
                    (classes (gethash "classifications" copy))
                    (filtered (remove-if (lambda (c) (member c '("operator" "active-persona") :test #'string-equal)) classes)))
               (unless (= (length classes) (length filtered))
                 (push (%cg-object "code" "ROLE_CLAIM_IGNORED" "entity_ordinal" ordinal
                                   "relationship_ordinal" :null "source_ordinal" :null) repairs))
               (setf (gethash "classifications" copy) filtered) (push copy entities)))
    (dolist (role '("operator" "active-persona"))
      (let* ((p (find role participants :key (lambda (p) (gethash "role" p)) :test #'equal))
             (ref (gethash "local_ref" p))
             (source-ids (sort (loop for id being the hash-keys of sources using (hash-value source)
                                    when (equal (gethash "principal_id" p)
                                                (gethash "principal_id" (gethash "identity" source))) collect id) #'string<)))
        (push (%cg-object "local_ref" ref "kind" (gethash "kind" p) "label" (gethash "label" p)
                          "aliases" (%cg-detach (gethash "aliases" p)) "classifications" #()
                          "identity_action" "NEW" "existing_node_id" :null
                          "evidence_status" "unreviewed" "evidence_note" "Runtime participant registry") entities)
        (push (%cg-object "role" role "local_ref" ref "entity_id" (gethash "entity_id" p)
                          "source_ids" (coerce source-ids 'vector) "basis" "runtime-participant-registry") bindings)))
    (let* ((copy (%cg-detach proposal))
           (relationships (gethash "relationships" copy))
           (seen (make-hash-table :test #'equal)) (unique nil))
      ;; This deduplication grants no correctness to endpoints or attribution.
      ;; The ordinary ontology/claim validators still run after preparation.
      (loop for relationship across relationships
            for key = (%cg-authority-canonical-json relationship)
            unless (gethash key seen) do (setf (gethash key seen) t) (push relationship unique))
      (setf (gethash "entities" copy) (coerce (nreverse entities) 'vector)
            (gethash "relationships" copy) (coerce (nreverse unique) 'vector))
      (let ((result (%cg-authority-result "accepted"
                                         (%cg-object "proposal" copy "bindings" (coerce (nreverse bindings) 'vector)))))
        (setf (gethash "diagnostics" result) (coerce (nreverse repairs) 'vector)) result))))

(defun %cg-revision-descriptor-digest (view)
  "Hash versioned descriptor fields, never search renderings or timestamps."
  (let ((descriptor (make-hash-table :test #'equal)))
    (dolist (key '("entity_id" "node_id" "kind" "label" "aliases"
                   "classifications" "participant_role" "status"))
      (setf (gethash key descriptor) (gethash key view)))
    (%cg-authority-digest "entity-revision" descriptor)))

(defun %cg-watermark-p (value)
  (and (%cg-closed-keys-p value '("projection_revision" "through_event_id" "state_digest"))
       (%cg-authority-string-p (gethash "projection_revision" value) 180)
       (integerp (gethash "through_event_id" value)) (<= 0 (gethash "through_event_id" value))
       (%cg-authority-digest-p (gethash "state_digest" value))))

(defun %cg-eligible-entity-p (view)
  (and (%cg-closed-keys-p view '("entity_id" "node_id" "revision_digest" "kind" "label"
                                "aliases" "classifications" "participant_role" "status" "agent_id" "persona_id"))
       (every (lambda (key) (%cg-authority-string-p (gethash key view) 180))
              '("entity_id" "node_id" "agent_id"))
       (%cg-authority-string-p (gethash "persona_id" view) 120)
       (%cg-authority-string-p (gethash "kind" view) 80)
       (%cg-authority-string-p (gethash "label" view) 240)
       (%cg-authority-strings-p (gethash "aliases" view) 8 240)
       (%cg-authority-strings-p (gethash "classifications" view) 8 120)
       (member (gethash "participant_role" view) '(:null "operator" "active-persona") :test #'equal)
       (equal "current" (gethash "status" view))
       (%cg-authority-digest-p (gethash "revision_digest" view))
       (equal (gethash "revision_digest" view) (%cg-revision-descriptor-digest view))))

(defun %cg-source-reference-scope-p (scope)
  (and (hash-table-p scope) (equal "explicit-source-reference-v1" (gethash "scope_basis" scope))))

(defun %cg-correction-policy-supported-p (policy)
  (member (gethash "policy_revision" policy)
          '("operator-conversational-label-correction-v1" "operator-source-reference-label-correction-v2") :test #'equal))

(defun %cg-correction-policy-p (policy)
  (and (%cg-closed-keys-p policy '("policy_revision" "enabled" "scope_definitions"))
       (%cg-authority-string-p (gethash "policy_revision" policy) 180)
       (member (gethash "enabled" policy) '(:true :false))
       (%cg-authority-array-p (gethash "scope_definitions" policy) 8)
       (every (lambda (definition)
                (and (if (%cg-source-reference-scope-p definition)
                         (and (equal "operator-source-reference-label-correction-v2" (gethash "policy_revision" policy))
                              (%cg-closed-keys-p definition '("definition_id" "scope_basis" "target_kind")))
                         (and (%cg-closed-keys-p definition '("definition_id" "predicate" "operator_endpoint" "target_kind"))
                              (%cg-authority-string-p (gethash "predicate" definition) 80)
                              (member (gethash "operator_endpoint" definition) '("subject" "object") :test #'equal)))
                     (%cg-authority-string-p (gethash "definition_id" definition) 180)
                     (%cg-authority-string-p (gethash "target_kind" definition) 80)))
              (gethash "scope_definitions" policy))
       (let ((definitions (gethash "scope_definitions" policy)))
         (= (length definitions) (length (remove-duplicates definitions :test #'equal
                                           :key (lambda (d) (gethash "definition_id" d))))))))

(defun %cg-adjacency-claim-p (claim)
  ;; Compact qualified projection input. The evidence digest binds the complete
  ;; accepted evidence history, not a truncated/name-rendered search snippet.
  (and (%cg-closed-keys-p
        claim '("fact_id" "identity_sha256" "subject_entity_id" "predicate" "object_entity_id"
                 "scope" "polarity" "source_basis" "evidence_status" "accepted_source_ids"
                 "accepted_evidence_digest" "status" "through_event_id"))
       (every (lambda (key) (%cg-authority-string-p (gethash key claim) 180))
              '("fact_id" "subject_entity_id" "object_entity_id"))
       (%cg-authority-string-p (gethash "predicate" claim) 80)
       (%cg-authority-digest-p (gethash "identity_sha256" claim))
       (%cg-authority-digest-p (gethash "accepted_evidence_digest" claim))
       (member (gethash "scope" claim) +cg-claim-scopes+ :test #'equal)
       (member (gethash "polarity" claim) '("positive" "negative" "unknown") :test #'equal)
       (member (gethash "source_basis" claim) '("original" "derived" "mixed") :test #'equal)
       (member (gethash "evidence_status" claim) +cg-evidence-statuses+ :test #'equal)
       (%cg-authority-strings-p (gethash "accepted_source_ids" claim) 4 180)
       (member (gethash "status" claim) '("current" "superseded" "retired") :test #'equal)
       (integerp (gethash "through_event_id" claim)) (<= 0 (gethash "through_event_id" claim))))

(defun context-graph-build-correction-scopes (partition-view policy participants)
  "Enumerate complete known operator relation scopes, not lexical top matches.
The runtime supplies a partition/watermark-bound current entity hash index and
sorted operator adjacency vector. Inspect at most 4096 claims, once total;
extra adjacency or an incomplete upstream index makes every scope incomplete."
  (unless (and (%cg-closed-keys-p partition-view '("agent_id" "persona_id" "projection_watermark"
                                                  "ontology" "entities" "operator_adjacency" "adjacency_complete"))
               (%cg-authority-string-p (gethash "agent_id" partition-view) 180)
               (%cg-authority-string-p (gethash "persona_id" partition-view) 120)
               (%cg-watermark-p (gethash "projection_watermark" partition-view))
               (hash-table-p (gethash "entities" partition-view))
               (vectorp (gethash "operator_adjacency" partition-view))
               (not (stringp (gethash "operator_adjacency" partition-view)))
               (member (gethash "adjacency_complete" partition-view) '(:true :false))
               (%cg-correction-policy-p policy))
    (%cg-authority-fail "SCOPE_INPUT_INVALID"))
  (%cg-validate-participant-registry (%cg-object "participants" participants))
  (let* ((operator (find "operator" participants :key (lambda (p) (gethash "role" p)) :test #'equal))
         (operator-id (gethash "entity_id" operator))
         (ontology (gethash "ontology" partition-view))
         (definitions (sort (copy-seq (gethash "scope_definitions" policy)) #'string<
                            :key (lambda (d) (gethash "definition_id" d))))
         (adjacency (gethash "operator_adjacency" partition-view))
         (entities (gethash "entities" partition-view))
         (watermark (gethash "projection_watermark" partition-view))
         (examined (min 4096 (length adjacency)))
         (complete (and (eq :true (gethash "adjacency_complete" partition-view)) (<= (length adjacency) 4096)))
         (scopes nil) (union (make-hash-table :test #'equal))
         (claim-index (make-hash-table :test #'equal)) (prior-id nil))
    ;; The ontology is runtime policy; a provider cannot supply this view.
    (handler-case (%cg-validate-ontology ontology) (error () (%cg-authority-fail "SCOPE_INPUT_INVALID")))
    (loop for d across definitions
          for subjectp = (equal "subject" (gethash "operator_endpoint" d))
          do (unless (%cg-signature-valid-p ontology (gethash "predicate" d)
                                           (if subjectp (gethash "kind" operator) (gethash "target_kind" d))
                                           (if subjectp (gethash "target_kind" d) (gethash "kind" operator)))
               (%cg-authority-fail "SCOPE_POLICY_INVALID")))
    (unless (and (eq :true (gethash "enabled" policy))
                 (equal "operator-conversational-label-correction-v1" (gethash "policy_revision" policy)))
      (return-from context-graph-build-correction-scopes
        (%cg-authority-result "rejected" nil "POLICY_UNAVAILABLE")))
    ;; Validate only the bounded prefix, never scan lifetime history to validate
    ;; a limit. Sortedness is also a maintained runtime-index invariant.
    (dotimes (i examined)
      (let ((claim (aref adjacency i)))
        (unless (and (%cg-adjacency-claim-p claim)
                     (or (equal operator-id (gethash "subject_entity_id" claim))
                         (equal operator-id (gethash "object_entity_id" claim)))
                     (<= (gethash "through_event_id" claim) (gethash "through_event_id" watermark))
                     (or (null prior-id) (string< prior-id (gethash "fact_id" claim))))
          (%cg-authority-fail "SCOPE_INPUT_INVALID"))
        (setf prior-id (gethash "fact_id" claim))
        (push claim (gethash (gethash "predicate" claim) claim-index))))
    (maphash (lambda (predicate claims) (setf (gethash predicate claim-index) (nreverse claims))) claim-index)
    (loop for definition across definitions
          do (let ((candidates (make-hash-table :test #'equal)) (anchors nil) (scope-complete complete))
               (dolist (claim (gethash (gethash "predicate" definition) claim-index))
                 (let* ((subjectp (equal "subject" (gethash "operator_endpoint" definition)))
                        (operator-key (if subjectp "subject_entity_id" "object_entity_id"))
                        (target-key (if subjectp "object_entity_id" "subject_entity_id")))
                   (when (and (equal "current" (gethash "status" claim))
                              (equal "assertion" (gethash "scope" claim))
                              (equal "positive" (gethash "polarity" claim))
                              (equal "original" (gethash "source_basis" claim))
                              (member (gethash "evidence_status" claim) '("direct" "prior-graph") :test #'equal)
                              (plusp (length (gethash "accepted_source_ids" claim)))
                              (equal (gethash "predicate" definition) (gethash "predicate" claim))
                              (equal operator-id (gethash operator-key claim)))
                     (let* ((target (gethash target-key claim)) (entity (gethash target entities)))
                       (unless (and (%cg-eligible-entity-p entity) (equal target (gethash "entity_id" entity))
                                    (every (lambda (key) (equal (gethash key partition-view) (gethash key entity)))
                                           '("agent_id" "persona_id")))
                         (%cg-authority-fail "SCOPE_INPUT_INVALID"))
                       (when (and (eq :null (gethash "participant_role" entity))
                                  (not (find target participants :key (lambda (p) (gethash "entity_id" p)) :test #'equal))
                                  (equal (gethash "kind" entity) (gethash "target_kind" definition)))
                         (cond
                           ((and (not (gethash target candidates))
                                 (or (= 16 (hash-table-count candidates))
                                     (and (not (gethash target union)) (= 64 (hash-table-count union)))))
                            (setf scope-complete nil))
                           (t
                            (setf (gethash target union) entity)
                            (let ((count (gethash target candidates 0)))
                              (when (< count 2)
                                (let ((anchor (make-hash-table :test #'equal)))
                                  (dolist (key '("fact_id" "subject_entity_id" "predicate" "object_entity_id"
                                                 "scope" "polarity" "source_basis" "evidence_status" "accepted_source_ids"))
                                    (setf (gethash key anchor) (gethash key claim)))
                                  (setf (gethash "claim_digest" anchor) (%cg-authority-digest "anchor-claim" claim))
                                  (push anchor anchors)))
                              (setf (gethash target candidates) (1+ count))))))))))
               (push (%cg-object
                      "scope_id" (%cg-authority-digest "correction-scope"
                                    (vector (gethash "agent_id" partition-view) (gethash "persona_id" partition-view)
                                            watermark (gethash "definition_id" definition)))
                      "definition_id" (gethash "definition_id" definition)
                      "agent_id" (gethash "agent_id" partition-view) "persona_id" (gethash "persona_id" partition-view)
                      "projection_watermark" watermark "operator_entity_id" operator-id
                      "predicate" (gethash "predicate" definition) "operator_endpoint" (gethash "operator_endpoint" definition)
                      "target_kind" (gethash "target_kind" definition) "complete" (if scope-complete :true :false)
                      "examined_claim_count" examined
                      "candidate_entity_ids" (coerce (sort (loop for id being the hash-keys of candidates collect id) #'string<) 'vector)
                      "anchor_claims" (coerce (nreverse anchors) 'vector)) scopes)))
    (%cg-authority-result
     "accepted" (%cg-object "scopes" (coerce (nreverse scopes) 'vector)
                             "eligible_entities" (coerce (sort (loop for e being the hash-values of union collect e)
                                                               #'string< :key (lambda (e) (gethash "entity_id" e))) 'vector)
                             "examined_claim_count" examined)) ))

(defun %cg-current-revision-view-p (view)
  (and (%cg-closed-keys-p
        view '("agent_id" "persona_id" "entity_id" "node_id" "revision_digest"
               "kind" "label" "aliases" "classifications" "participant_role"
               "status" "observed_at" "application_id"))
       (every (lambda (key) (%cg-authority-string-p (gethash key view) 180))
              '("agent_id" "entity_id" "node_id" "application_id"))
       (%cg-authority-string-p (gethash "persona_id" view) 120)
       (%cg-authority-string-p (gethash "kind" view) 80)
       (%cg-authority-string-p (gethash "label" view) 240)
       (%cg-authority-strings-p (gethash "aliases" view) 8 240)
       (%cg-authority-strings-p (gethash "classifications" view) 8 120)
       (member (gethash "participant_role" view) '(:null "operator" "active-persona") :test #'equal)
       (member (gethash "status" view) '("current" "superseded") :test #'equal)
       (integerp (gethash "observed_at" view)) (<= 0 (gethash "observed_at" view))
       (%cg-authority-digest-p (gethash "revision_digest" view))
       (equal (gethash "revision_digest" view) (%cg-revision-descriptor-digest view))))

(defun %cg-revision-grant-id (grant)
  (let ((copy (%cg-detach grant)))
    (remhash "grant_id" copy)
    (%cg-authority-digest "revision-grant" copy)))

(defun %cg-revision-grant-p (grant)
  (and (%cg-closed-keys-p
        grant '("grant_id" "authority_revision" "admission_policy_revision"
                "admission_basis" "semantic_status" "agent_id" "persona_id"
                "episode_id" "revision_ref" "operation" "entity_id"
                "target_node_id" "expected_revision_digest" "replacement_label"
                "kind" "target_scope_id" "target_scope_digest" "anchor_claim_digests"
                "review_id" "proposal_digest" "operator_command_id" "evidence"))
       (equal "context-graph-authority-v1" (gethash "authority_revision" grant))
       (member (gethash "admission_policy_revision" grant)
               '("operator-conversational-label-correction-v1" "operator-source-reference-label-correction-v2") :test #'equal)
       (equal "correct-primary-label" (gethash "operation" grant))
       (every (lambda (key) (%cg-authority-string-p (gethash key grant) 180))
              '("agent_id" "episode_id" "entity_id" "target_node_id"))
       (%cg-authority-string-p (gethash "persona_id" grant) 120)
       (%cg-authority-string-p (gethash "revision_ref" grant) 80)
       (%cg-authority-string-p (gethash "replacement_label" grant) 240)
       (%cg-authority-string-p (gethash "kind" grant) 80)
       (every (lambda (key) (%cg-authority-digest-p (gethash key grant)))
              '("grant_id" "expected_revision_digest" "proposal_digest"))
       (%cg-authority-strings-p (gethash "anchor_claim_digests" grant) 2 64)
       (every #'%cg-authority-digest-p (gethash "anchor_claim_digests" grant))
       (cond
         ((equal "reviewed-explicit-source-reference" (gethash "admission_basis" grant))
          (and (equal "operator-source-reference-label-correction-v2" (gethash "admission_policy_revision" grant))
               (equal "policy-accepted-interpretation" (gethash "semantic_status" grant))
               (%cg-authority-string-p (gethash "target_scope_id" grant) 180)
               (%cg-authority-digest-p (gethash "target_scope_digest" grant))
               (%cg-authority-digest-p (gethash "review_id" grant))
               (zerop (length (gethash "anchor_claim_digests" grant)))
               (eq :null (gethash "operator_command_id" grant))))
         ((equal "reviewed-operator-conversation" (gethash "admission_basis" grant))
          (and (equal "policy-accepted-interpretation" (gethash "semantic_status" grant))
               (%cg-authority-string-p (gethash "target_scope_id" grant) 180)
               (%cg-authority-digest-p (gethash "target_scope_digest" grant))
               (%cg-authority-digest-p (gethash "review_id" grant))
               (plusp (length (gethash "anchor_claim_digests" grant)))
               (eq :null (gethash "operator_command_id" grant))))
         ((equal "explicit-operator-command" (gethash "admission_basis" grant))
          (and (equal "explicit-operation" (gethash "semantic_status" grant))
               (%cg-authority-string-p (gethash "operator_command_id" grant) 180)
               (every (lambda (key) (eq :null (gethash key grant)))
                      '("target_scope_id" "target_scope_digest" "review_id"))
               (zerop (length (gethash "anchor_claim_digests" grant))))))
       (equal (gethash "grant_id" grant) (%cg-revision-grant-id grant))))

(defun %cg-grant-span-valid-p (span sources primary-source-ids operator)
  "Validate the exact interval against runtime provenance, not proposed text."
  (and (%cg-closed-keys-p
        span '("source_id" "speaker_id" "source_kind" "timestamp" "text_sha256"
               "identity" "resource_ref" "quote" "start_char" "end_char"
               "method" "resolver_revision"))
       (member (gethash "method" span) '("exact" "presentation" "ellipsis") :test #'equal)
       (equal "exact-source-span-v1" (gethash "resolver_revision" span))
       (let* ((source (gethash (gethash "source_id" span) sources))
              (start (gethash "start_char" span)) (end (gethash "end_char" span)))
         (and source (find (gethash "source_id" span) primary-source-ids :test #'equal)
              (equal "original-utterance" (gethash "kind" source))
              (equal "operator" (gethash "role" (gethash "identity" source)))
              (equal (gethash "principal_id" operator) (gethash "principal_id" (gethash "identity" source)))
              (equal (gethash "identity_binding_id" operator) (gethash "binding_id" (gethash "identity" source)))
              (equal (gethash "speaker_id" operator) (gethash "speaker_id" source))
              (integerp start) (integerp end) (<= 0 start) (< start end)
              (<= end (length (gethash "text" source))) (<= (- end start) 1000)
              (equal (subseq (gethash "text" source) start end) (gethash "quote" span))
              (equal (gethash "kind" source) (gethash "source_kind" span))
              (every (lambda (key) (%cg-authority-equal-p (gethash key source) (gethash key span)))
                     '("speaker_id" "timestamp" "text_sha256" "identity" "resource_ref"))))))

(defun context-graph-plan-entity-revision (context grant current-view)
  "Mechanically plan an already admitted grant against a qualified current view.
This is NOT admission: the owner must recompute the complete reviewed batch
before installation. A matching digest is integrity, never authentication.
No facts are traversed or rewritten; no supplied object is mutated."
  (unless (and (hash-table-p context) (%cg-revision-grant-p grant)
               (%cg-current-revision-view-p current-view))
    (%cg-authority-fail "REVISION_INPUT_INVALID"))
  (let* ((participants (%cg-validate-participant-registry context))
         (sources (%cg-validate-sources (gethash "source_packet" context)))
         (primary (gethash "primary_source_ids" context))
         (operator (find "operator" participants :key (lambda (p) (gethash "role" p)) :test #'equal))
         (label (gethash "replacement_label" grant))
         (old-label (gethash "label" current-view))
         (evidence (gethash "evidence" grant)))
    (unless (and (%cg-authority-strings-p primary 128 180) (plusp (length primary))
                 (every (lambda (id) (gethash id sources)) primary)
                 (equal (gethash "authority_revision" context) (gethash "authority_revision" grant))
                 (every (lambda (key) (equal (gethash key grant) (gethash key context)))
                        '("agent_id" "persona_id" "episode_id"))
                 (every (lambda (key) (equal (gethash key grant) (gethash key current-view)))
                        '("agent_id" "persona_id" "entity_id" "kind"))
                 (eq :null (gethash "participant_role" current-view))
                 (not (find (gethash "entity_id" grant) participants
                            :key (lambda (p) (gethash "entity_id" p)) :test #'equal)))
      (%cg-authority-fail "REVISION_INPUT_INVALID"))
    (unless (%cg-grant-span-valid-p evidence sources primary operator)
      (%cg-authority-fail "CORRECTION_SOURCE_INVALID"))
    (when (and (member (gethash "admission_basis" grant)
                       '("reviewed-operator-conversation" "reviewed-explicit-source-reference") :test #'equal)
               (not (search label (gethash "quote" evidence) :test #'char=)))
      (%cg-authority-fail "REVISION_CONTENT_INVALID"))
    (unless (and (equal "current" (gethash "status" current-view))
                 (equal (gethash "target_node_id" grant) (gethash "node_id" current-view))
                 (equal (gethash "expected_revision_digest" grant) (gethash "revision_digest" current-view)))
      (return-from context-graph-plan-entity-revision (%cg-authority-result "incomplete" nil "TARGET_STALE")))
    ;; A no-op decision has no grant and must not call the planner.
    (when (equal label old-label) (%cg-authority-fail "LABEL_ALREADY_CURRENT"))
    (let* ((entity-id (gethash "entity_id" grant))
           (old-node (gethash "node_id" current-view))
           (grant-id (gethash "grant_id" grant))
           (observed-at (gethash "observed_at" current-view))
           (operation-id (%cg-authority-digest
                          "revision-operation"
                          (vector (gethash "agent_id" grant) (gethash "persona_id" grant)
                                  (gethash "episode_id" grant) (gethash "revision_ref" grant) grant-id)))
           (node-id (concatenate 'string "kgf:entity:"
                                 (%cg-authority-digest "revision-node" (vector entity-id operation-id))))
           (aliases (remove-if (lambda (alias)
                                 (or (equal (%cg-canonical alias) (%cg-canonical old-label))
                                     (equal (%cg-canonical alias) (%cg-canonical label))))
                               (gethash "aliases" current-view))))
      (%cg-authority-result
       "accepted"
       (%cg-object
        "operation_id" operation-id "entity_id" entity-id
        "expected_node_id" old-node "expected_revision_digest" (gethash "revision_digest" current-view)
        "close_version" (%cg-object "node_id" old-node "status" "superseded" "observed_at" observed-at)
        "create_version" (%cg-object "node_id" node-id "entity_id" entity-id "kind" (gethash "kind" current-view)
                                      "label" label "aliases" aliases
                                      "classifications" (gethash "classifications" current-view)
                                      "participant_role" :null "status" "current" "supersedes_node_id" old-node
                                      "correction_grant_id" grant-id "observed_at" observed-at)
        "lineage" (%cg-object "new_node_id" node-id "old_node_id" old-node
                               "relation" "supersedes" "correction_grant_id" grant-id)
        "current_version" (%cg-object "entity_id" entity-id "node_id" node-id)
        "evidence" evidence)))))

(defun %cg-anchor-p (anchor)
  (and (%cg-closed-keys-p anchor '("fact_id" "claim_digest" "subject_entity_id" "predicate" "object_entity_id"
                                  "scope" "polarity" "source_basis" "evidence_status" "accepted_source_ids"))
       (every (lambda (key) (%cg-authority-string-p (gethash key anchor) 180))
              '("fact_id" "subject_entity_id" "object_entity_id"))
       (%cg-authority-digest-p (gethash "claim_digest" anchor))
       (%cg-authority-string-p (gethash "predicate" anchor) 80)
       (equal "assertion" (gethash "scope" anchor)) (equal "positive" (gethash "polarity" anchor))
       (equal "original" (gethash "source_basis" anchor))
       (member (gethash "evidence_status" anchor) '("direct" "prior-graph") :test #'equal)
       (%cg-authority-strings-p (gethash "accepted_source_ids" anchor) 4 180)
       (plusp (length (gethash "accepted_source_ids" anchor)))))

(defun %cg-target-scope-p (scope)
  (when (%cg-source-reference-scope-p scope)
    (return-from %cg-target-scope-p
      (and (%cg-closed-keys-p scope '("scope_id" "definition_id" "scope_basis" "agent_id" "persona_id"
                                     "projection_watermark" "operator_entity_id" "target_kind" "complete"
                                     "candidate_entity_ids" "anchor_claims"))
           (every (lambda (key) (%cg-authority-string-p (gethash key scope) 180))
                  '("scope_id" "definition_id" "agent_id" "operator_entity_id"))
           (%cg-authority-string-p (gethash "persona_id" scope) 120)
           (%cg-watermark-p (gethash "projection_watermark" scope))
           (%cg-authority-string-p (gethash "target_kind" scope) 80)
           (member (gethash "complete" scope) '(:true :false))
           (%cg-authority-strings-p (gethash "candidate_entity_ids" scope) 16 180)
           (%cg-authority-array-p (gethash "anchor_claims" scope) 0))))
  (and (%cg-closed-keys-p scope '("scope_id" "definition_id" "agent_id" "persona_id" "projection_watermark"
                                 "operator_entity_id" "predicate" "operator_endpoint" "target_kind" "complete"
                                 "examined_claim_count" "candidate_entity_ids" "anchor_claims"))
       (every (lambda (key) (%cg-authority-string-p (gethash key scope) 180))
              '("scope_id" "definition_id" "agent_id" "operator_entity_id"))
       (%cg-authority-string-p (gethash "persona_id" scope) 120)
       (%cg-watermark-p (gethash "projection_watermark" scope))
       (%cg-authority-string-p (gethash "predicate" scope) 80)
       (%cg-authority-string-p (gethash "target_kind" scope) 80)
       (member (gethash "operator_endpoint" scope) '("subject" "object") :test #'equal)
       (member (gethash "complete" scope) '(:true :false))
       (integerp (gethash "examined_claim_count" scope)) (<= 0 (gethash "examined_claim_count" scope) 4096)
       (%cg-authority-strings-p (gethash "candidate_entity_ids" scope) 16 180)
       (%cg-authority-array-p (gethash "anchor_claims" scope) 32)
       (every #'%cg-anchor-p (gethash "anchor_claims" scope))
       (let ((anchors (gethash "anchor_claims" scope)) (candidates (gethash "candidate_entity_ids" scope))
             (subjectp (equal "subject" (gethash "operator_endpoint" scope))))
         (and (= (length anchors) (length (remove-duplicates anchors :key (lambda (a) (gethash "fact_id" a)) :test #'equal)))
              (every (lambda (a)
                       (and (equal (gethash "predicate" scope) (gethash "predicate" a))
                            (equal (gethash "operator_entity_id" scope) (gethash (if subjectp "subject_entity_id" "object_entity_id") a))
                            (find (gethash (if subjectp "object_entity_id" "subject_entity_id") a) candidates :test #'equal))) anchors)
              (every (lambda (id) (<= 1 (count id anchors :test #'equal
                                              :key (lambda (a) (gethash (if subjectp "object_entity_id" "subject_entity_id") a))) 2)) candidates)))))

(defun %cg-correction-scans-valid-p (context)
  (if (eql 1 (gethash "schema_version" context)) t
      (let ((scans (gethash "correction_scans" context)))
        (and (%cg-authority-array-p scans 8)
             (every (lambda (row)
                      (and (%cg-closed-keys-p row '("target_kind" "complete" "examined_count" "candidate_entity_ids"))
                           (%cg-authority-string-p (gethash "target_kind" row) 80)
                           (member (gethash "complete" row) '(:true :false))
                           (integerp (gethash "examined_count" row))
                           (<= 0 (gethash "examined_count" row) 1024)
                           (%cg-authority-strings-p (gethash "candidate_entity_ids" row) 16 180)
                           (= (length (gethash "candidate_entity_ids" row))
                              (length (remove-duplicates (gethash "candidate_entity_ids" row) :test #'equal))))) scans)
             (= (length scans) (length (remove-duplicates scans :test #'equal :key (lambda (row) (gethash "target_kind" row)))))))))

(defun %cg-correction-scan-complete-p (context kind ids)
  (if (eql 1 (gethash "schema_version" context))
      (eq :true (gethash "complete" (gethash "candidate_scan" context)))
      (let ((scan (find kind (gethash "correction_scans" context) :test #'equal :key (lambda (row) (gethash "target_kind" row)))))
        (and scan (eq :true (gethash "complete" scan))
             (equal (sort (coerce ids 'list) #'string<)
                    (sort (coerce (gethash "candidate_entity_ids" scan) 'list) #'string<))))))

(defun %cg-authority-context-valid-p (context)
  (and (%cg-closed-keys-p context (append '("schema_version" "authority_revision" "agent_id" "persona_id" "episode_id"
                                   "source_packet" "primary_source_ids" "access_context" "access_snapshot_digest"
                                   "projection_watermark" "correction_policy" "correction_scopes" "participants"
                                   "eligible_entities" "operator_commands" "candidate_scan")
                                          (when (eql 2 (gethash "schema_version" context)) '("correction_scans"))))
       (member (gethash "schema_version" context) '(1 2))
       (%cg-correction-scans-valid-p context)
       (%cg-authority-string-p (gethash "authority_revision" context) 180)
       (every (lambda (key) (%cg-authority-string-p (gethash key context) 180)) '("agent_id" "episode_id"))
       (%cg-authority-string-p (gethash "persona_id" context) 120)
       (%cg-authority-strings-p (gethash "primary_source_ids" context) 128 180)
       (plusp (length (gethash "primary_source_ids" context)))
       (%cg-authority-digest-p (gethash "access_snapshot_digest" context))
       (handler-case (pai.memory-access:memory-access-validate-context (gethash "access_context" context))
         (pai.memory-access:memory-access-input-error () nil))
       (every (lambda (key) (equal (gethash key context) (gethash key (gethash "partition" (gethash "access_context" context)))))
              '("agent_id" "persona_id"))
       (%cg-watermark-p (gethash "projection_watermark" context))
       (%cg-correction-policy-p (gethash "correction_policy" context))
       (%cg-authority-array-p (gethash "eligible_entities" context) 64)
       (every #'%cg-eligible-entity-p (gethash "eligible_entities" context))
       (%cg-authority-array-p (gethash "correction_scopes" context) 8)
       (every #'%cg-target-scope-p (gethash "correction_scopes" context))
       ;; Optional command route is unavailable until an authenticated routing
       ;; adapter and whole-message parser have been qualified. No fake records.
       (%cg-authority-array-p (gethash "operator_commands" context) 0)
       (let ((scan (gethash "candidate_scan" context)))
         (and (%cg-closed-keys-p scan '("complete" "examined_count"))
              (member (gethash "complete" scan) '(:true :false))
              (integerp (gethash "examined_count" scan)) (<= 0 (gethash "examined_count" scan) 1024)))))

(defun %cg-validate-authority-context (context)
  (unless (%cg-authority-context-valid-p context) (%cg-authority-fail "CONTEXT_INVALID"))
  (let* ((participants (%cg-validate-participant-registry context))
         (sources (%cg-validate-sources (gethash "source_packet" context)))
         (eligible (gethash "eligible_entities" context)) (scopes (gethash "correction_scopes" context))
         (definitions (gethash "scope_definitions" (gethash "correction_policy" context)))
         (operator (find "operator" participants :key (lambda (p) (gethash "role" p)) :test #'equal)))
    (%cg-validate-source-participants sources participants)
    (unless (and (every (lambda (id) (gethash id sources)) (gethash "primary_source_ids" context))
                 (= (length eligible) (length (remove-duplicates eligible :key (lambda (e) (gethash "entity_id" e)) :test #'equal)))
                 (= (length scopes) (length (remove-duplicates scopes :key (lambda (s) (gethash "scope_id" s)) :test #'equal))))
      (%cg-authority-fail "CONTEXT_INVALID"))
    (loop for entity across eligible do
      (unless (every (lambda (key) (equal (gethash key context) (gethash key entity))) '("agent_id" "persona_id"))
        (%cg-authority-fail "SOURCE_PARTITION_MISMATCH")))
    (loop for scope across scopes
          for definition = (find (gethash "definition_id" scope) definitions :test #'equal :key (lambda (d) (gethash "definition_id" d)))
          do (unless (and definition
                          (every (lambda (key) (equal (gethash key context) (gethash key scope))) '("agent_id" "persona_id"))
                          (%cg-authority-equal-p (gethash "projection_watermark" context) (gethash "projection_watermark" scope))
                          (equal (gethash "entity_id" operator) (gethash "operator_entity_id" scope))
                          (every (lambda (key) (equal (gethash key definition) (gethash key scope)))
                                 (if (%cg-source-reference-scope-p scope)
                                     '("scope_basis" "target_kind") '("predicate" "operator_endpoint" "target_kind")))
                          (or (not (%cg-source-reference-scope-p scope))
                              (and (%cg-source-reference-scope-p definition)
                                   (let ((ids (gethash "candidate_entity_ids" scope)))
                                     (and (= (length ids) (length (remove-duplicates ids :test #'equal)))
                                          (or (eq :false (gethash "complete" scope))
                                              (and (%cg-correction-scan-complete-p context (gethash "target_kind" scope) ids)
                                                   (every (lambda (e)
                                                            (or (not (and (eq :null (gethash "participant_role" e))
                                                                          (equal (gethash "kind" e) (gethash "target_kind" scope))))
                                                                (find (gethash "entity_id" e) ids :test #'equal))) eligible)))))))
                          (equal (gethash "scope_id" scope)
                                 (%cg-authority-digest "correction-scope"
                                   (vector (gethash "agent_id" context) (gethash "persona_id" context)
                                           (gethash "projection_watermark" context) (gethash "definition_id" scope))))
                          (every (lambda (id)
                                   (let ((entity (find id eligible :key (lambda (e) (gethash "entity_id" e)) :test #'equal)))
                                     (and entity (eq :null (gethash "participant_role" entity))
                                          (equal (gethash "kind" entity) (gethash "target_kind" scope)))))
                                 (gethash "candidate_entity_ids" scope)))
               (%cg-authority-fail "CONTEXT_INVALID")))
    sources))

(defun context-graph-enable-source-reference-corrections (context &optional target-kinds)
  "Explicit opt-in composition on a frozen, access-qualified candidate scan.
This does not expand access, retrieve candidates, or mutate the supplied context.
Schema 1 requires whole-partition candidate_scan completeness. Schema 2 uses
separate per-kind correction_scans, rechecked against the graph at application."
  (%cg-validate-authority-context context)
  (unless (and (eq :true (gethash "enabled" (gethash "correction_policy" context)))
               (%cg-correction-policy-supported-p (gethash "correction_policy" context)))
    (%cg-authority-fail "POLICY_UNAVAILABLE"))
  (let* ((copy (%cg-detach context))
         (eligible (gethash "eligible_entities" copy))
         (configured (or target-kinds (map 'vector (lambda (d) (gethash "target_kind" d))
                                          (gethash "scope_definitions" (gethash "correction_policy" context)))))
         (kinds (progn
                  (unless (%cg-authority-strings-p configured 8 80) (%cg-authority-fail "SOURCE_REFERENCE_SCOPE_LIMIT"))
                  (sort (remove-duplicates (coerce configured 'list) :test #'equal) #'string<)))
         (operator (find "operator" (gethash "participants" copy) :test #'equal :key (lambda (p) (gethash "role" p))))
         (definitions nil) (scopes nil))
    (when (> (length kinds) 8) (%cg-authority-fail "SOURCE_REFERENCE_SCOPE_LIMIT"))
    (dolist (kind kinds)
      (let* ((definition-id (concatenate 'string "explicit-reference:" kind))
             (ids (sort (loop for e across eligible
                              when (and (eq :null (gethash "participant_role" e)) (equal kind (gethash "kind" e)))
                                collect (gethash "entity_id" e)) #'string<)))
        (push (%cg-object "definition_id" definition-id "scope_basis" "explicit-source-reference-v1" "target_kind" kind) definitions)
        (push (%cg-object "scope_id" (%cg-authority-digest "correction-scope"
                                       (vector (gethash "agent_id" copy) (gethash "persona_id" copy)
                                               (gethash "projection_watermark" copy) definition-id))
                          "definition_id" definition-id "scope_basis" "explicit-source-reference-v1"
                          "agent_id" (gethash "agent_id" copy) "persona_id" (gethash "persona_id" copy)
                          "projection_watermark" (gethash "projection_watermark" copy)
                          "operator_entity_id" (gethash "entity_id" operator) "target_kind" kind
                          "complete" (if (and (<= (length ids) 16)
                                               (%cg-correction-scan-complete-p copy kind (coerce ids 'vector))) :true :false)
                          "candidate_entity_ids" (coerce (subseq ids 0 (min 16 (length ids))) 'vector)
                          "anchor_claims" #()) scopes)))
    (setf (gethash "correction_policy" copy)
          (%cg-object "policy_revision" "operator-source-reference-label-correction-v2" "enabled" :true
                      "scope_definitions" (coerce (nreverse definitions) 'vector))
          (gethash "correction_scopes" copy) (coerce (nreverse scopes) 'vector))
    (%cg-validate-authority-context copy)
    copy))

(defun %cg-source-reference-term-p (term text)
  "Case-insensitive whole-term containment; substring names are not references."
  (loop for start = (search term text :test #'char-equal) then (search term text :start2 (1+ start) :test #'char-equal)
        while start thereis (and (or (zerop start) (not (alphanumericp (char text (1- start)))))
                                 (let ((end (+ start (length term))))
                                   (or (= end (length text)) (not (alphanumericp (char text end))))))))

(defun %cg-explicit-source-target-p (context scope target replacement quote)
  ;; This is a prerequisite for semantic review, never identity authority alone.
  (let ((matches (remove-if-not
                  (lambda (entity)
                    (and (%cg-source-reference-term-p (gethash "label" entity) quote)
                         (some (lambda (category) (%cg-source-reference-term-p category quote))
                               (gethash "classifications" entity))))
                  (%cg-scope-descriptors context scope))))
    (and (%cg-source-reference-term-p replacement quote)
         (= 1 (length matches))
         (equal target (gethash "entity_id" (aref matches 0))))))

(defun %cg-citation-p (citation prepared-p)
  (and (%cg-closed-keys-p citation (if prepared-p '("source_id" "quote" "start_char" "end_char") '("source_id" "quote")))
       (%cg-authority-string-p (gethash "source_id" citation) 180)
       (%cg-authority-string-p (gethash "quote" citation) 1000)
       (or (not prepared-p)
           (and (integerp (gethash "start_char" citation)) (integerp (gethash "end_char" citation))
                (<= 0 (gethash "start_char" citation)) (< (gethash "start_char" citation) (gethash "end_char" citation))
                (= (length (gethash "quote" citation)) (- (gethash "end_char" citation) (gethash "start_char" citation)))))))

(defun %cg-grounding-v2-p (grounding prepared-p &optional revision-p)
  (and (%cg-closed-keys-p grounding '("schema_version" "scope" "polarity" "attributed_to_ref" "evidence"))
       (eql 2 (gethash "schema_version" grounding))
       (member (gethash "scope" grounding) +cg-claim-scopes+ :test #'equal)
       (member (gethash "polarity" grounding) '("positive" "negative" "unknown") :test #'equal)
       (or (eq :null (gethash "attributed_to_ref" grounding)) (%cg-authority-string-p (gethash "attributed_to_ref" grounding) 80))
       (%cg-authority-array-p (gethash "evidence" grounding) (if revision-p 1 4) 1)
       (every (lambda (c) (%cg-citation-p c prepared-p)) (gethash "evidence" grounding))
       (or (not revision-p) (and (equal "assertion" (gethash "scope" grounding))
                                 (equal "positive" (gethash "polarity" grounding))
                                 (equal "runtime:operator" (gethash "attributed_to_ref" grounding))))))

(defun %cg-revision-proposal-p (revision prepared-p)
  (and (%cg-closed-keys-p revision '("schema_version" "revision_ref" "operation" "local_ref" "requested_route"
                                    "target_entity_id" "target_node_id" "expected_revision_digest" "target_scope_id"
                                    "operator_command_id" "replacement_label" "interpretation" "grounding" "context_evidence"))
       (eql 1 (gethash "schema_version" revision))
       (every (lambda (key) (%cg-authority-string-p (gethash key revision) 80)) '("revision_ref" "operation" "local_ref"))
       (every (lambda (key) (%cg-authority-string-p (gethash key revision) 180)) '("target_entity_id" "target_node_id"))
       (%cg-authority-digest-p (gethash "expected_revision_digest" revision))
       (%cg-authority-string-p (gethash "replacement_label" revision) 240)
       (member (gethash "interpretation" revision) '("error-correction" "actual-name-change" "mere-mention" "reported-correction" "uncertain") :test #'equal)
       (cond ((equal "conversation" (gethash "requested_route" revision))
              (and (%cg-authority-string-p (gethash "target_scope_id" revision) 180) (eq :null (gethash "operator_command_id" revision))))
             ((equal "operator-command" (gethash "requested_route" revision))
              (and (eq :null (gethash "target_scope_id" revision)) (%cg-authority-string-p (gethash "operator_command_id" revision) 180))))
       (%cg-grounding-v2-p (gethash "grounding" revision) prepared-p t)
       (%cg-authority-array-p (gethash "context_evidence" revision) 4)
       (every (lambda (c) (%cg-citation-p c prepared-p)) (gethash "context_evidence" revision))))

(defun %cg-validate-proposal-v4 (proposal &key prepared-p)
  (unless (and (%cg-closed-keys-p proposal '("schema_version" "ontology_revision" "entities" "relationships" "entity_revisions"))
               (eql 4 (gethash "schema_version" proposal)) (%cg-authority-string-p (gethash "ontology_revision" proposal) 180)
               (%cg-authority-array-p (gethash "entities" proposal) (if prepared-p 26 24))
               (%cg-authority-array-p (gethash "relationships" proposal) 48)
               (%cg-authority-array-p (gethash "entity_revisions" proposal) 8))
    (%cg-authority-fail "PROPOSAL_INVALID"))
  (let ((refs (make-hash-table :test #'equal)) (revisions (gethash "entity_revisions" proposal)))
    (loop for e across (gethash "entities" proposal)
          do (unless (and (%cg-closed-keys-p e '("local_ref" "kind" "label" "aliases" "classifications" "identity_action"
                                                 "existing_node_id" "evidence_status" "evidence_note"))
                          (%cg-authority-string-p (gethash "local_ref" e) 80) (%cg-authority-string-p (gethash "kind" e) 80)
                          (%cg-authority-string-p (gethash "label" e) 240) (%cg-authority-strings-p (gethash "aliases" e) 8 240)
                          (%cg-authority-strings-p (gethash "classifications" e) 8 120)
                          (member (gethash "identity_action" e) '("NEW" "LINK_EXISTING" "REVISE_EXISTING") :test #'equal)
                          (if (equal "NEW" (gethash "identity_action" e)) (eq :null (gethash "existing_node_id" e))
                              (%cg-authority-string-p (gethash "existing_node_id" e) 180))
                          (member (gethash "evidence_status" e) +cg-evidence-statuses+ :test #'equal)
                          (%cg-authority-string-p (gethash "evidence_note" e) 600)
                          (not (gethash (gethash "local_ref" e) refs)))
               (%cg-authority-fail "ENTITY_INVALID"))
             (setf (gethash (gethash "local_ref" e) refs) e))
    (loop for r across (gethash "relationships" proposal)
          do (unless (and (%cg-closed-keys-p r '("subject_ref" "predicate" "object_ref" "relationship_action" "fact"
                                                 "grounding" "temporal" "evidence_status" "evidence_note"))
                          (every (lambda (key) (%cg-authority-string-p (gethash key r) 80)) '("subject_ref" "predicate" "object_ref"))
                          (every (lambda (key) (or (gethash (gethash key r) refs)
                                                  (member (gethash key r) '("runtime:operator" "runtime:active-persona") :test #'equal)))
                                 '("subject_ref" "object_ref"))
                          (equal "ASSERT" (gethash "relationship_action" r))
                          (%cg-authority-string-p (gethash "fact" r) 1000)
                          (%cg-grounding-v2-p (gethash "grounding" r) prepared-p)
                          (let ((ref (gethash "attributed_to_ref" (gethash "grounding" r))))
                            (or (eq :null ref) (gethash ref refs) (member ref '("runtime:operator" "runtime:active-persona") :test #'equal)))
                          (let ((temporal (gethash "temporal" r)))
                            (and (%cg-closed-keys-p temporal '("schema_version" "character" "occurred_at" "valid_from" "valid_until"))
                                 (eql 1 (gethash "schema_version" temporal))
                                 (member (gethash "character" temporal) +cg-temporal-characters+ :test #'equal)
                                 (every (lambda (key) (or (eq :null (gethash key temporal)) (%cg-authority-string-p (gethash key temporal) 80)))
                                        '("occurred_at" "valid_from" "valid_until"))))
                          (member (gethash "evidence_status" r) +cg-evidence-statuses+ :test #'equal)
                          (%cg-authority-string-p (gethash "evidence_note" r) 600))
               (%cg-authority-fail "RELATIONSHIP_INVALID")))
    (unless (and (every (lambda (r) (%cg-revision-proposal-p r prepared-p)) revisions)
                 (= (length revisions) (length (remove-duplicates revisions :key (lambda (r) (gethash "revision_ref" r)) :test #'equal))))
      (%cg-authority-fail "REVISION_INPUT_INVALID"))
    (loop for r across revisions for e = (gethash (gethash "local_ref" r) refs)
          do (unless (and e (equal "REVISE_EXISTING" (gethash "identity_action" e))
                          (equal (gethash "label" e) (gethash "replacement_label" r))
                          (equal (gethash "existing_node_id" e) (gethash "target_node_id" r)))
               (%cg-authority-fail "REVISION_INPUT_INVALID")))
    (loop for e across (gethash "entities" proposal) when (equal "REVISE_EXISTING" (gethash "identity_action" e))
          do (unless (= 1 (count (gethash "local_ref" e) revisions :key (lambda (r) (gethash "local_ref" r)) :test #'equal))
               (%cg-authority-fail "REVISION_INPUT_INVALID")))
    t))

(defun context-graph-validate-authority-input (context proposal)
  "Validate raw structure and trusted consistency. Does not grant access."
  (%cg-validate-authority-context context)
  (%cg-validate-proposal-v4 proposal)
  (%cg-authority-result "accepted" (%cg-object "valid" :true)))

(defun context-graph-prepare-authority (context proposal)
  "Freeze exact evidence before semantic review. No provider or graph mutation.
Raw quotes remain bound by per-item hashes; runtime spans carry full provenance.
An unresolved revision never falls back to a new entity or an alias update."
  (context-graph-validate-authority-input context proposal)
  (let* ((normalized (context-graph-normalize-participants proposal context))
         (sources (%cg-validate-sources (gethash "source_packet" context)))
         (receipts nil) (diagnostics nil) (kept nil))
    (unless (equal "accepted" (gethash "status" normalized)) (return-from context-graph-prepare-authority normalized))
    (let* ((value (gethash "value" normalized)) (prepared (gethash "proposal" value)))
      (labels ((prepare-citations (citations kind ordinal)
                 (let ((result nil) (local-receipts nil))
                   (loop for citation across citations for citation-ordinal from 0
                         for source = (gethash (gethash "source_id" citation) sources)
                         do (unless source (%cg-authority-fail "SOURCE_INVALID"))
                            (let ((resolved (context-graph-resolve-source-span source (gethash "quote" citation))))
                              (unless (equal "accepted" (gethash "status" resolved))
                                (return-from prepare-citations (values nil resolved)))
                              (let ((span (gethash "value" resolved)))
                                (push (%cg-object "source_id" (gethash "source_id" span) "quote" (gethash "quote" span)
                                                  "start_char" (gethash "start_char" span) "end_char" (gethash "end_char" span)) result)
                                (push (%cg-object "item_kind" kind "item_ordinal" ordinal "citation_ordinal" citation-ordinal
                                                  "proposed_quote_sha256" (%cg-sha256 (gethash "quote" citation))
                                                  "accepted_span" span) local-receipts))))
                   (setf receipts (nconc receipts (nreverse local-receipts)))
                   (values (coerce (nreverse result) 'vector) nil))))
        (loop for relationship across (gethash "relationships" prepared) for ordinal from 0
              do (multiple-value-bind (citations failure)
                     (prepare-citations (gethash "evidence" (gethash "grounding" relationship)) "relationship" ordinal)
                   (if failure
                       (push (%cg-object "code" "RELATIONSHIP_SPAN_OMITTED" "entity_ordinal" :null
                                         "relationship_ordinal" ordinal "source_ordinal" :null) diagnostics)
                       (progn (setf (gethash "evidence" (gethash "grounding" relationship)) citations) (push relationship kept)))))
        (setf (gethash "relationships" prepared) (coerce (nreverse kept) 'vector))
        (loop for revision across (gethash "entity_revisions" prepared) for ordinal from 0
              do (multiple-value-bind (citations failure)
                     (prepare-citations (gethash "evidence" (gethash "grounding" revision)) "entity-revision" ordinal)
                   (when failure (return-from context-graph-prepare-authority failure))
                   (setf (gethash "evidence" (gethash "grounding" revision)) citations))
                 (multiple-value-bind (citations failure)
                     (prepare-citations (gethash "context_evidence" revision) "revision-context" ordinal)
                   (when failure (return-from context-graph-prepare-authority failure))
                   (setf (gethash "context_evidence" revision) citations))))
      (%cg-validate-proposal-v4 prepared :prepared-p t)
      (let* ((all-diagnostics (concatenate 'vector (gethash "diagnostics" normalized) (coerce (nreverse diagnostics) 'vector)))
             (result (%cg-authority-result "accepted" (%cg-object "proposal" prepared "bindings" (gethash "bindings" value)
                                                                    "span_receipts" (coerce receipts 'vector)))))
        (setf (gethash "diagnostics" result) (subseq all-diagnostics 0 (min 64 (length all-diagnostics)))
              (gethash "omitted_diagnostic_count" result) (max 0 (- (length all-diagnostics) 64)))
        result))))

(defun %cg-revisions-sorted-by-ref (proposal)
  (sort (copy-seq (gethash "entity_revisions" proposal)) #'string< :key (lambda (r) (gethash "revision_ref" r))))

(defun %cg-validate-prepared-participants (context proposal)
  "Prepared reserved rows must equal runtime injection, not provider descriptors."
  (let* ((empty (%cg-object "schema_version" 4 "ontology_revision" (gethash "ontology_revision" proposal)
                            "entities" #() "relationships" #() "entity_revisions" #()))
         (expected (gethash "entities" (gethash "proposal" (gethash "value" (context-graph-normalize-participants empty context))))))
    (loop for entity across expected
          for actual = (find (gethash "local_ref" entity) (gethash "entities" proposal) :test #'equal :key (lambda (e) (gethash "local_ref" e)))
          do (unless (and actual (%cg-authority-equal-p actual entity)) (%cg-authority-fail "PARTICIPANTS_INVALID")))
    (loop for entity across (gethash "entities" proposal) for ref = (gethash "local_ref" entity)
          when (and (<= 8 (length ref)) (string-equal "runtime:" ref :end2 8))
            do (unless (find ref expected :test #'equal :key (lambda (e) (gethash "local_ref" e)))
                 (%cg-authority-fail "RESERVED_REF_CONFLICT"))) t))

(defun %cg-paired-revision-entity (proposal revision)
  (find (gethash "local_ref" revision) (gethash "entities" proposal) :test #'equal :key (lambda (e) (gethash "local_ref" e))))

(defun %cg-revision-proposal-digest (proposal revision)
  (%cg-authority-digest "revision-proposal" (vector revision (%cg-paired-revision-entity proposal revision))))

(defun %cg-revision-scope (context revision)
  (find (gethash "target_scope_id" revision) (gethash "correction_scopes" context)
        :test #'equal :key (lambda (s) (gethash "scope_id" s))))

(defun %cg-scope-descriptors (context scope)
  (map 'vector (lambda (id) (find id (gethash "eligible_entities" context) :test #'equal :key (lambda (e) (gethash "entity_id" e))))
       (sort (copy-seq (gethash "candidate_entity_ids" scope)) #'string<)))

(defun %cg-target-scope-digest (context scope)
  (%cg-authority-digest "target-scope" (vector scope (%cg-scope-descriptors context scope))))

(defun %cg-duplicate-revision-targets (rows)
  (let ((seen (make-hash-table :test #'equal)) (conflicts nil))
    (loop for row across rows for id = (gethash "target_entity_id" row)
          do (if (gethash id seen) (pushnew id conflicts :test #'equal) (setf (gethash id seen) t)))
    (sort conflicts #'string<)))

(defun %cg-citation-exact-span (context citation)
  "Validate prepared coordinates; no quote repair during review or fold."
  (let* ((source (find (gethash "source_id" citation) (gethash "sources" (gethash "source_packet" context))
                       :test #'equal :key (lambda (s) (gethash "source_id" s))))
         (start (gethash "start_char" citation)) (end (gethash "end_char" citation)))
    (when (and source (%cg-citation-p citation t) (<= end (length (gethash "text" source)))
               (equal (gethash "quote" citation) (subseq (gethash "text" source) start end)))
      (%cg-object "source_id" (gethash "source_id" source) "speaker_id" (gethash "speaker_id" source)
                  "source_kind" (gethash "kind" source) "timestamp" (gethash "timestamp" source)
                  "text_sha256" (gethash "text_sha256" source) "identity" (gethash "identity" source)
                  "resource_ref" (gethash "resource_ref" source) "quote" (gethash "quote" citation)
                  "start_char" start "end_char" end "method" "exact" "resolver_revision" "exact-source-span-v1"))))

(defun %cg-frozen-current-view (context revision)
  (let ((entity (find (gethash "target_entity_id" revision) (gethash "eligible_entities" context)
                      :test #'equal :key (lambda (e) (gethash "entity_id" e)))))
    (if entity
        (let ((view (%cg-detach entity)))
          ;; These fields are not used by preflight. They must never be used
          ;; as the observation/application boundary for planning installation.
          (setf (gethash "observed_at" view) 0 (gethash "application_id" view) "frozen-preflight") view)
        :null)))

(defun %cg-revision-preflight (context proposal revision current-view)
  "Return the first ordered deterministic failure, or NIL/NIL and exact evidence."
  (labels ((fail (outcome reason) (return-from %cg-revision-preflight (values outcome reason nil))))
    (unless (and (equal "context-graph-authority-v1" (gethash "authority_revision" context))
                 (%cg-correction-policy-supported-p (gethash "correction_policy" context))
                 (eq :true (gethash "enabled" (gethash "correction_policy" context))))
      (fail "reject" "POLICY_UNAVAILABLE"))
    (unless (and (equal "correct-primary-label" (gethash "operation" revision))
                 (equal "conversation" (gethash "requested_route" revision)))
      (fail "reject" "OPERATION_UNSUPPORTED"))
    (let* ((evidence (%cg-citation-exact-span context (aref (gethash "evidence" (gethash "grounding" revision)) 0)))
           (operator (find "operator" (gethash "participants" context) :test #'equal :key (lambda (p) (gethash "role" p))))
           (sources (%cg-validate-sources (gethash "source_packet" context)))
           (eligible (find (gethash "target_entity_id" revision) (gethash "eligible_entities" context)
                           :test #'equal :key (lambda (e) (gethash "entity_id" e))))
           (entity (%cg-paired-revision-entity proposal revision)))
      (unless (and evidence (%cg-grant-span-valid-p evidence sources (gethash "primary_source_ids" context) operator)
                   (every (lambda (c) (and (%cg-citation-exact-span context c)
                                          (find (gethash "source_id" c) (gethash "primary_source_ids" context) :test #'equal)))
                          (gethash "context_evidence" revision)))
        (fail "reject" "CORRECTION_SOURCE_INVALID"))
      (unless (and eligible (eq :null (gethash "participant_role" eligible))
                   (not (find (gethash "target_entity_id" revision) (gethash "participants" context)
                              :test #'equal :key (lambda (p) (gethash "entity_id" p)))))
        (fail "reject" "TARGET_INELIGIBLE"))
      (unless (and (not (eq :null current-view))
                   (equal "current" (gethash "status" current-view))
                   (equal (gethash "entity_id" eligible) (gethash "entity_id" current-view))
                   (equal (gethash "node_id" eligible) (gethash "node_id" current-view))
                   (equal (gethash "revision_digest" eligible) (gethash "revision_digest" current-view))
                   (equal (gethash "target_node_id" revision) (gethash "node_id" current-view))
                   (equal (gethash "expected_revision_digest" revision) (gethash "revision_digest" current-view))
                   (every (lambda (key) (equal (gethash key context) (gethash key current-view))) '("agent_id" "persona_id")))
        (fail "defer" "TARGET_STALE"))
      (unless (and (equal (gethash "kind" entity) (gethash "kind" current-view))
                   (%cg-authority-equal-p (gethash "aliases" entity) (gethash "aliases" current-view))
                   (%cg-authority-equal-p (gethash "classifications" entity) (gethash "classifications" current-view))
                   (search (gethash "replacement_label" revision) (gethash "quote" evidence) :test #'char=))
        (fail "reject" "REVISION_CONTENT_INVALID"))
      (let ((scope (%cg-revision-scope context revision)))
        (unless (and scope (find (gethash "target_entity_id" revision) (gethash "candidate_entity_ids" scope) :test #'equal))
          (fail "defer" "TARGET_CONTEXT_MISSING"))
        (unless (eq :true (gethash "complete" scope)) (fail "defer" "TARGET_CONTEXT_INCOMPLETE"))
        (when (and (%cg-source-reference-scope-p scope)
                   (not (%cg-explicit-source-target-p context scope (gethash "target_entity_id" revision)
                                                     (gethash "replacement_label" revision) (gethash "quote" evidence))))
          (fail "defer" "EXPLICIT_SOURCE_REFERENCE_UNRESOLVED")))
      (values nil nil evidence))))

(defun context-graph-build-revision-review-input (context prepared-proposal)
  "Build one frozen review packet; do not clip alternatives or complete sources."
  (%cg-validate-authority-context context)
  (%cg-validate-proposal-v4 prepared-proposal :prepared-p t)
  (%cg-validate-prepared-participants context prepared-proposal)
  (let ((rows (%cg-revisions-sorted-by-ref prepared-proposal)) (inputs nil) (source-ids nil) (failures nil))
    (when (%cg-duplicate-revision-targets rows)
      (return-from context-graph-build-revision-review-input (%cg-authority-result "rejected" nil "REVISION_CONFLICT")))
    (loop for r across rows do
      (multiple-value-bind (outcome code) (%cg-revision-preflight context prepared-proposal r (%cg-frozen-current-view context r))
        (when outcome (push (cons outcome code) failures))))
    (when failures
      (let ((failure (or (find "reject" (reverse failures) :key #'car :test #'equal) (car (last failures)))))
        (return-from context-graph-build-revision-review-input
          (%cg-authority-result (if (equal "reject" (car failure)) "rejected" "incomplete") nil (cdr failure)))))
    (loop for revision across rows for scope = (%cg-revision-scope context revision)
          do (push (%cg-object "revision_ref" (gethash "revision_ref" revision)
                               "proposal_digest" (%cg-revision-proposal-digest prepared-proposal revision)
                               "proposed_revision" revision "target_scope" scope
                               "candidate_descriptors" (%cg-scope-descriptors context scope)) inputs)
             (loop for citation across (concatenate 'vector (gethash "evidence" (gethash "grounding" revision))
                                                           (gethash "context_evidence" revision))
                   do (pushnew (gethash "source_id" citation) source-ids :test #'equal)))
    (when (zerop (length rows))
      (return-from context-graph-build-revision-review-input
        (%cg-authority-result "accepted" (%cg-object "request" :null "request_digest" :null))))
    (let* ((request (%cg-object "schema_version" 1 "review_protocol_revision" "kg-revision-review-v1"
                                "authority_revision" "context-graph-authority-v1"
                                "admission_policy_revision" (gethash "policy_revision" (gethash "correction_policy" context))
                                "source_records" (map 'vector (lambda (id) (find id (gethash "sources" (gethash "source_packet" context))
                                                                                 :test #'equal :key (lambda (s) (gethash "source_id" s))))
                                                       (sort source-ids #'string<))
                                "revisions" (coerce (nreverse inputs) 'vector)))
           (json (%cg-authority-canonical-json request)))
      (when (> (length (sb-ext:string-to-octets json :external-format :utf-8)) 131072)
        (return-from context-graph-build-revision-review-input (%cg-authority-result "incomplete" nil "REVIEW_CONTEXT_LIMIT")))
      (%cg-authority-result "accepted" (%cg-object "request" request "request_digest" (%cg-authority-digest "revision-review-request" request))))))

(defun %cg-review-row-p (row)
  (and (%cg-closed-keys-p row '("revision_ref" "proposal_digest" "interpretation" "source_reading" "target_scope_fit"
                               "same_entity" "replacement_supported" "competing_interpretation" "candidate_assessments"))
       (%cg-authority-string-p (gethash "revision_ref" row) 80) (%cg-authority-digest-p (gethash "proposal_digest" row))
       (member (gethash "interpretation" row) '("error-correction" "actual-name-change" "mere-mention" "reported-correction" "uncertain") :test #'equal)
       (member (gethash "source_reading" row) '("operator-assertion" "reported" "hypothetical" "joke" "uncertain") :test #'equal)
       (every (lambda (key) (member (gethash key row) '("supported" "unsupported" "uncertain") :test #'equal))
              '("target_scope_fit" "same_entity" "replacement_supported"))
       (member (gethash "competing_interpretation" row) '("none-found" "present" "uncertain") :test #'equal)
       (%cg-authority-array-p (gethash "candidate_assessments" row) 16 1)
       (every (lambda (candidate)
                (and (%cg-closed-keys-p candidate '("entity_id" "assessment" "anchor_fact_ids" "reason_code"))
                     (%cg-authority-string-p (gethash "entity_id" candidate) 180)
                     (member (gethash "assessment" candidate) '("target" "not-target" "uncertain") :test #'equal)
                     (%cg-authority-strings-p (gethash "anchor_fact_ids" candidate) 2 180)
                     (member (gethash "reason_code" candidate) '("context-and-correction-agree" "context-excludes" "insufficient-context" "conflicting-context") :test #'equal)))
              (gethash "candidate_assessments" row))))

(defun context-graph-validate-revision-review (context prepared-proposal raw-review response-binding)
  "Bind actual provider judgments to the complete request; never fabricate rows."
  (let* ((built (context-graph-build-revision-review-input context prepared-proposal))
         (rows (%cg-revisions-sorted-by-ref prepared-proposal)))
    (unless (equal "accepted" (gethash "status" built)) (%cg-authority-fail "REVIEW_PREFLIGHT_INVALID"))
    (unless (and (%cg-closed-keys-p raw-review '("schema_version" "reviews"))
                 (eql 1 (gethash "schema_version" raw-review)) (%cg-authority-array-p (gethash "reviews" raw-review) 8)
                 (every #'%cg-review-row-p (gethash "reviews" raw-review))
                 (= (length rows) (length (gethash "reviews" raw-review)))
                 (%cg-closed-keys-p response-binding '("opened_boundary_id" "request_digest" "response_digest"))
                 (integerp (gethash "opened_boundary_id" response-binding)) (plusp (gethash "opened_boundary_id" response-binding))
                 (equal (gethash "request_digest" response-binding) (gethash "request_digest" (gethash "value" built)))
                 (equal (gethash "response_digest" response-binding) (%cg-authority-digest "revision-review-response" raw-review)))
      (%cg-authority-fail "REVIEW_BINDING_INVALID"))
    (loop for revision across rows
          for matches = (remove (gethash "revision_ref" revision) (gethash "reviews" raw-review)
                                :test-not #'equal :key (lambda (r) (gethash "revision_ref" r)))
          do (unless (= 1 (length matches)) (%cg-authority-fail "REVIEW_BINDING_INVALID"))
             (let* ((review (aref matches 0)) (scope (%cg-revision-scope context revision))
                    (assessments (gethash "candidate_assessments" review)) (candidates (gethash "candidate_entity_ids" scope)))
               (unless (and (equal (gethash "proposal_digest" review) (%cg-revision-proposal-digest prepared-proposal revision))
                            (= (length candidates) (length assessments))
                            (every (lambda (id) (= 1 (count id assessments :test #'equal :key (lambda (a) (gethash "entity_id" a))))) candidates)
                            (every (lambda (assessment)
                                     (every (lambda (id)
                                              (let ((anchor (find id (gethash "anchor_claims" scope) :test #'equal :key (lambda (a) (gethash "fact_id" a)))))
                                                (and anchor (equal (gethash "entity_id" assessment)
                                                                   (gethash (if (equal "subject" (gethash "operator_endpoint" scope))
                                                                                "object_entity_id" "subject_entity_id") anchor)))))
                                            (gethash "anchor_fact_ids" assessment))) assessments))
                 (%cg-authority-fail "REVIEW_BINDING_INVALID"))))
    (let* ((protocol "kg-revision-review-v1") (boundary (gethash "opened_boundary_id" response-binding))
           (request-digest (gethash "request_digest" response-binding)) (response-digest (gethash "response_digest" response-binding)))
      (%cg-authority-result "accepted"
        (%cg-object "review_id" (%cg-authority-digest "revision-review-receipt" (vector boundary protocol request-digest response-digest))
                    "review_protocol_revision" protocol "opened_boundary_id" boundary
                    "request_digest" request-digest "response_digest" response-digest
                    ;; Preserve raw order: response digest binds these exact rows.
                    "reviews" (gethash "reviews" raw-review))))))

(defun %cg-revision-decision (context proposal revision outcome code &optional grant)
  (let* ((scope (%cg-revision-scope context revision))
         (clarify-p (member code '("TARGET_AMBIGUOUS" "TARGET_CONTEXT_MISSING"
                                  "CORRECTION_MEANING_UNRESOLVED" "OPERATION_UNSUPPORTED") :test #'equal))
         (clarification
           (when clarify-p
             (%cg-object "request_id" (%cg-authority-digest "revision-clarification"
                                        (vector (gethash "agent_id" context) (gethash "persona_id" context)
                                                (gethash "episode_id" context) (gethash "authority_revision" context)
                                                (gethash "revision_ref" revision) (%cg-revision-proposal-digest proposal revision) code))
                         "revision_ref" (gethash "revision_ref" revision) "reason_code" code
                         "candidate_entity_ids" (if scope (gethash "candidate_entity_ids" scope) #())
                         "source_id" (gethash "source_id" (aref (gethash "evidence" (gethash "grounding" revision)) 0))
                         "replacement_label" (gethash "replacement_label" revision)))))
    (%cg-detach (%cg-object "schema_version" 1 "revision_ref" (gethash "revision_ref" revision)
                            "outcome" outcome "reason_code" code "grant" (or grant :null)
                            "clarification" (or clarification :null)))))

(defun %cg-revalidate-review-receipt (context proposal receipt)
  (unless (and (%cg-closed-keys-p receipt '("review_id" "review_protocol_revision" "opened_boundary_id"
                                           "request_digest" "response_digest" "reviews"))
               (equal "kg-revision-review-v1" (gethash "review_protocol_revision" receipt)))
    (%cg-authority-fail "REVIEW_BINDING_INVALID"))
  (let* ((raw (%cg-object "schema_version" 1 "reviews" (gethash "reviews" receipt)))
         (binding (%cg-object "opened_boundary_id" (gethash "opened_boundary_id" receipt)
                              "request_digest" (gethash "request_digest" receipt) "response_digest" (gethash "response_digest" receipt)))
         (expected (gethash "value" (context-graph-validate-revision-review context proposal raw binding))))
    (unless (%cg-authority-equal-p expected receipt) (%cg-authority-fail "REVIEW_BINDING_INVALID"))
    receipt))

(defun context-graph-decide-entity-revision (context prepared-proposal revision-ref review-receipt current-view)
  "Apply the ordered conversational correction policy to frozen reviewed inputs.
The caller additionally checks the live projection watermark and the authenticated
opened boundary before sealing/applying. Hashes here only bind those inputs."
  (%cg-validate-authority-context context)
  (%cg-validate-proposal-v4 prepared-proposal :prepared-p t)
  (%cg-validate-prepared-participants context prepared-proposal)
  (unless (or (eq :null current-view) (%cg-current-revision-view-p current-view))
    (%cg-authority-fail "REVISION_INPUT_INVALID"))
  (let ((revision (find revision-ref (gethash "entity_revisions" prepared-proposal) :test #'equal :key (lambda (r) (gethash "revision_ref" r)))))
    (unless revision (%cg-authority-fail "REVISION_INPUT_INVALID"))
    (labels ((decision (outcome code &optional grant)
               (return-from context-graph-decide-entity-revision
                 (%cg-revision-decision context prepared-proposal revision outcome code grant))))
      (multiple-value-bind (outcome code evidence) (%cg-revision-preflight context prepared-proposal revision current-view)
        (when outcome (decision outcome code))
        ;; Complete-batch conflicts precede review construction. No array order
        ;; can turn conflicting proposals into a first-writer-wins operation.
        (when (%cg-duplicate-revision-targets (gethash "entity_revisions" prepared-proposal))
          (decision "reject" "REVISION_CONFLICT"))
        (let ((built (context-graph-build-revision-review-input context prepared-proposal)))
          (unless (equal "accepted" (gethash "status" built))
            (decision "defer" (if (equal "REVIEW_CONTEXT_LIMIT" (gethash "code" (aref (gethash "diagnostics" built) 0)))
                                  "REVIEW_CONTEXT_LIMIT" "REVIEW_UNAVAILABLE"))))
        (when (eq :null review-receipt) (decision "defer" "REVIEW_UNAVAILABLE"))
        (handler-case (%cg-revalidate-review-receipt context prepared-proposal review-receipt)
          (context-graph-authority-input-error () (decision "reject" "REVIEW_BINDING_INVALID")))
        (let* ((review (find revision-ref (gethash "reviews" review-receipt) :test #'equal :key (lambda (r) (gethash "revision_ref" r))))
               (interpretation (gethash "interpretation" revision)) (review-interpretation (gethash "interpretation" review))
               (scope (%cg-revision-scope context revision)) (target (gethash "target_entity_id" revision)))
          (unless (and (equal "error-correction" interpretation) (equal "error-correction" review-interpretation))
            (if (and (member interpretation '("mere-mention" "reported-correction") :test #'equal)
                     (member review-interpretation '("mere-mention" "reported-correction") :test #'equal))
                (decision "reject" "NOT_A_CORRECTION")
                (decision "defer" "CORRECTION_MEANING_UNRESOLVED")))
          (when (equal "reported" (gethash "source_reading" review)) (decision "reject" "NOT_A_CORRECTION"))
          (unless (and (equal "operator-assertion" (gethash "source_reading" review))
                       (every (lambda (key) (equal "supported" (gethash key review))) '("target_scope_fit" "same_entity" "replacement_supported"))
                       (equal "none-found" (gethash "competing_interpretation" review)))
            (decision "defer" "CORRECTION_MEANING_UNRESOLVED"))
          (let* ((assessments (gethash "candidate_assessments" review))
                 (chosen (find target assessments :test #'equal :key (lambda (a) (gethash "entity_id" a)))))
            (unless (and (= 1 (count "target" assessments :test #'equal :key (lambda (a) (gethash "assessment" a))))
                         (equal "target" (gethash "assessment" chosen))
                         (if (%cg-source-reference-scope-p scope)
                             (zerop (length (gethash "anchor_fact_ids" chosen)))
                             (plusp (length (gethash "anchor_fact_ids" chosen))))
                         (equal "context-and-correction-agree" (gethash "reason_code" chosen))
                         (every (lambda (a) (or (equal target (gethash "entity_id" a))
                                                (and (equal "not-target" (gethash "assessment" a))
                                                     (equal "context-excludes" (gethash "reason_code" a))))) assessments))
              (decision "defer" "TARGET_AMBIGUOUS"))
            (when (equal (gethash "replacement_label" revision) (gethash "label" current-view))
              (decision "noop" "LABEL_ALREADY_CURRENT"))
            (let* ((anchors (map 'vector (lambda (id) (gethash "claim_digest" (find id (gethash "anchor_claims" scope)
                                                                                 :test #'equal :key (lambda (a) (gethash "fact_id" a)))))
                                 (sort (copy-seq (gethash "anchor_fact_ids" chosen)) #'string<)))
                   (grant (%cg-object "authority_revision" "context-graph-authority-v1"
                                      "admission_policy_revision" (gethash "policy_revision" (gethash "correction_policy" context))
                                      "admission_basis" (if (%cg-source-reference-scope-p scope)
                                                            "reviewed-explicit-source-reference" "reviewed-operator-conversation")
                                      "semantic_status" "policy-accepted-interpretation"
                                      "agent_id" (gethash "agent_id" context) "persona_id" (gethash "persona_id" context)
                                      "episode_id" (gethash "episode_id" context) "revision_ref" revision-ref
                                      "operation" "correct-primary-label" "entity_id" target
                                      "target_node_id" (gethash "target_node_id" revision) "expected_revision_digest" (gethash "expected_revision_digest" revision)
                                      "replacement_label" (gethash "replacement_label" revision) "kind" (gethash "kind" current-view)
                                      "target_scope_id" (gethash "scope_id" scope) "target_scope_digest" (%cg-target-scope-digest context scope)
                                      "anchor_claim_digests" anchors "review_id" (gethash "review_id" review-receipt)
                                      "proposal_digest" (%cg-revision-proposal-digest prepared-proposal revision) "operator_command_id" :null "evidence" evidence)))
              (setf (gethash "grant_id" grant) (%cg-revision-grant-id grant))
              (decision "accept" "CONVERSATIONAL_POLICY_SATISFIED" grant))))))))

(defun %cg-decide-revision-batch (context prepared-proposal review-receipt current-views)
  "No active grants/deltas unless the complete formation is admissible."
  (%cg-validate-authority-context context)
  (%cg-validate-proposal-v4 prepared-proposal :prepared-p t)
  (%cg-validate-prepared-participants context prepared-proposal)
  (unless (and (%cg-authority-array-p current-views 8)
               (every #'%cg-current-revision-view-p current-views)
               (= (length current-views) (length (remove-duplicates current-views :test #'equal :key (lambda (v) (gethash "entity_id" v))))))
    (%cg-authority-fail "REVISION_INPUT_INVALID"))
  (let* ((rows (%cg-revisions-sorted-by-ref prepared-proposal))
         (conflicts (%cg-duplicate-revision-targets rows))
         (decisions
           (map 'vector
                (lambda (revision)
                  (if conflicts (%cg-revision-decision context prepared-proposal revision "reject" "REVISION_CONFLICT")
                      (context-graph-decide-entity-revision
                       context prepared-proposal (gethash "revision_ref" revision) review-receipt
                       (or (find (gethash "target_entity_id" revision) current-views :test #'equal :key (lambda (v) (gethash "entity_id" v))) :null)))) rows))
         (outcome (cond ((find "reject" decisions :test #'equal :key (lambda (d) (gethash "outcome" d))) "rejected")
                        ((find "defer" decisions :test #'equal :key (lambda (d) (gethash "outcome" d))) "deferred") (t "admitted")))
         (grants nil) (deltas nil))
    (if (equal "admitted" outcome)
        (loop for decision across decisions when (equal "accept" (gethash "outcome" decision))
              do (let* ((grant (gethash "grant" decision))
                        (view (find (gethash "entity_id" grant) current-views :test #'equal :key (lambda (v) (gethash "entity_id" v))))
                        (planned (context-graph-plan-entity-revision context grant view)))
                   (unless (equal "accepted" (gethash "status" planned)) (%cg-authority-fail "TARGET_STALE"))
                   (push grant grants) (push (gethash "value" planned) deltas)))
        (loop for decision across decisions do (setf (gethash "grant" decision) :null)))
    (%cg-detach (%cg-object "formation_outcome" outcome "decisions" decisions
                            "revision_grants" (coerce (nreverse grants) 'vector) "revision_deltas" (coerce (nreverse deltas) 'vector)))))

(defun context-graph-claim-identity (claim resolved-refs)
  "Identity for an already qualified claim, never only an endpoint triple.
RESOLVED-REFS is the trusted resolution result with enduring endpoints and
attribution plus source basis derived from validated provenance. No allocation,
source-speaker inference, evidence promotion or mutable graph lookup occurs."
  (unless (and (hash-table-p claim)
               (%cg-authority-string-p (gethash "predicate" claim) 80)
               (%cg-authority-string-p (gethash "fact" claim) 1000)
               (%cg-grounding-v2-p (gethash "grounding" claim) t)
               (%cg-closed-keys-p resolved-refs '("subject_entity_id" "object_entity_id" "attributed_entity_id" "source_basis"))
               (%cg-authority-string-p (gethash "subject_entity_id" resolved-refs) 180)
               (%cg-authority-string-p (gethash "object_entity_id" resolved-refs) 180)
               (or (eq :null (gethash "attributed_entity_id" resolved-refs)) (%cg-authority-string-p (gethash "attributed_entity_id" resolved-refs) 180))
               (eq (eq :null (gethash "attributed_to_ref" (gethash "grounding" claim)))
                   (eq :null (gethash "attributed_entity_id" resolved-refs)))
               (member (gethash "source_basis" resolved-refs) '("original" "derived" "mixed") :test #'equal))
    (%cg-authority-fail "CLAIM_IDENTITY_INVALID"))
  (let* ((grounding (gethash "grounding" claim))
         (temporal (gethash "temporal" claim))
         (identity
           (if (member *cg-claim-identity-protocol*
                       '("claim-identity-v2" "claim-identity-v3") :test #'equal)
               ;; Provenance is evidence about a proposition, not part of the
               ;; proposition.  A later direct observation can therefore
               ;; promote the same typed inference.  RELATED_TO retains its
               ;; statement because the generic predicate carries no other
               ;; semantics.  Temporal fields prevent distinct events or
               ;; bounded states from collapsing into one fact.
               (vector (gethash "subject_entity_id" resolved-refs)
                       (gethash "predicate" claim)
                       (gethash "object_entity_id" resolved-refs)
                       (if (equal "related_to" (gethash "predicate" claim))
                           (%cg-canonical (gethash "fact" claim))
                           "typed-relation")
                       (gethash "scope" grounding)
                       (gethash "polarity" grounding)
                       (gethash "attributed_entity_id" resolved-refs)
                       (%cg-claim-temporal-identity claim temporal)
                       (and temporal (gethash "occurred_at" temporal))
                       (and temporal (gethash "valid_from" temporal))
                       (and temporal (gethash "valid_until" temporal)))
               (vector (gethash "subject_entity_id" resolved-refs)
                       (gethash "predicate" claim)
                       (gethash "object_entity_id" resolved-refs)
                       (%cg-canonical (gethash "fact" claim))
                       (gethash "scope" grounding)
                       (gethash "polarity" grounding)
                       (gethash "attributed_entity_id" resolved-refs)
                       (gethash "source_basis" resolved-refs)))))
    (%cg-authority-result
     "accepted"
     (%cg-object "identity_key" (%cg-authority-canonical-json identity)
                 "identity_sha256"
                 ;; V1 predates the named protocol variable.  Keep its original
                 ;; digest domain exactly so a cold replay rebuilds durable fact
                 ;; IDs and every later projection watermark byte-for-byte.
                 (%cg-authority-digest
                  (cond ((equal *cg-claim-identity-protocol* "claim-identity-v3")
                         "claim-identity-v3")
                        ((equal *cg-claim-identity-protocol* "claim-identity-v2")
                         "claim-identity-v2")
                        (t "claim-identity"))
                  identity)))))

(defun context-graph-current-entity-view (entity-id revision-view)
  "One indexed current-descriptor lookup. Never walk or compress lineage on read.
The qualified view has partition IDs, entity_versions and current_entity_versions
hash indexes. Index construction/installation enforces unique current versions."
  (unless (and (%cg-authority-string-p entity-id 180)
               (%cg-closed-keys-p revision-view '("agent_id" "persona_id" "entity_versions" "current_entity_versions"))
               (%cg-authority-string-p (gethash "agent_id" revision-view) 180)
               (%cg-authority-string-p (gethash "persona_id" revision-view) 120)
               (hash-table-p (gethash "entity_versions" revision-view))
               (hash-table-p (gethash "current_entity_versions" revision-view)))
    (%cg-authority-fail "REVISION_VIEW_INVALID"))
  (let* ((node-id (gethash entity-id (gethash "current_entity_versions" revision-view)))
         (descriptor (and node-id (gethash node-id (gethash "entity_versions" revision-view)))))
    (unless (and (%cg-eligible-entity-p descriptor)
                 (equal entity-id (gethash "entity_id" descriptor)) (equal node-id (gethash "node_id" descriptor))
                 (every (lambda (key) (equal (gethash key revision-view) (gethash key descriptor))) '("agent_id" "persona_id")))
      (%cg-authority-fail "CURRENT_VERSION_CORRUPT"))
    (%cg-authority-result "accepted" descriptor)))
