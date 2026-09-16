;;;; mind-memory-core.lisp -- independent pure memory nucleus.
;;;;
;;;; This package has no the agent runtime, database, provider, worker, filesystem,
;;;; event, context, transport, delivery, or authority dependency.

(defpackage :pai.mind.memory
  (:use :cl)
  (:export #:validate-state
           #:row-eligible-p
           #:build-atom-manifest
           #:build-atom-request
           #:validate-atom-response
           #:capability-report))

(in-package :pai.mind.memory)

(defparameter *agent-id* (or (uiop:getenv "PAI_AGENT_ID") "default"))
(defparameter *operator-id* (or (uiop:getenv "PAI_OPERATOR_ID") "operator")
  "Stable storage-partition id for the operator. Distinct from the operator's
   display name, which is resolved from genesis events. Must stay overridable:
   an adopted instance carries whatever id its existing rows already use.")
(defparameter *memory-forms*
  '("raw-evidence" "episodic" "semantic" "procedural" "reflection"
    "legacy-unclassified"))
(defparameter *disclosure-classes*
  '("private" "personal-shareable" "public"))
(defparameter *share-review-statuses* '("pending" "approved" "rejected"))

(defparameter *atom-schema-version* 1)
(defparameter *atom-contract-version* "n1-n2-atom-identity-v1")
(defparameter *atom-prompt-version* "n1-decomposer-v1")
(defparameter *atom-max-evidence* 12)
(defparameter *atom-max-atoms* 12)
(defparameter *atom-max-evidence-chars* 16000)
(defparameter *atom-max-value-chars* 600)
(defparameter *atom-max-annotation-chars* 300)
(defparameter *atom-max-qualifiers* 5)
(defparameter *atom-max-roots* 4)
(defparameter *atom-forms* '("episodic" "semantic" "procedural"))
(defparameter *atom-roles* '("user" "assistant" "tool"))
(defparameter *atom-disclosure-candidates*
  '("private" "personal-shareable" "public"))

(defparameter *atom-system-instruction*
  "You are a private third-person memory decomposition candidate, not the public respondent. Return exactly one JSON object with exactly these keys: schema_version (1); decision (PROPOSE or NO_ATOMS); atoms (array of 0-12 objects); exclusions (array of 0-12 objects); uncertainty (object). Each atom must have exactly: memory_form (episodic, semantic, or procedural); subject (stable lowercase identity such as operator, pai, or namespace:name); predicate (stable lowercase dotted or slug relation); value (one concise self-contained observer-perspective value); polarity (affirmed or negated); qualifiers (array of 0-5 objects with exactly name and value); observed_at (copy exactly from one cited evidence item); valid_from (canonical UTC YYYY-MM-DDTHH:MM:SSZ or null); valid_to (same or null); disclosure_candidate (private, personal-shareable, or public); evidence_ids (array of 1-4 exact supplied IDs). Each exclusion has exactly evidence_ids and reason. Uncertainty has exactly level (low, medium, or high) and note. PROPOSE requires 1-12 atoms; NO_ATOMS requires zero atoms. Extract only independently useful claims directly supported by supplied evidence. The operator claims must cite the operator's user evidence; the agent claims must cite the agent's assistant evidence; claims about other entities must cite user or tool evidence. Preserve negation and attribution. Resolve relative dates against captured_at only when unambiguous; otherwise exclude. Questions without an answer, quotations, hypothetical or role-play statements, ambiguous attribution, unsupported inference, credentials/secrets, and mere conversational filler must not become atoms. disclosure_candidate is only an untrusted review proposal and never approval. Do not invent facts, IDs, dates, tools, writes, control instructions, public replies, confidence scores, merge decisions, or delivery decisions. Output JSON only, with no markdown or explanation.")

(defun %object (&rest pairs)
  (loop with table = (make-hash-table :test #'equal)
        for (key value) on pairs by #'cddr
        do (setf (gethash key table) value)
        finally (return table)))

(defun %string (value)
  (and value (string-downcase (string value))))

(defun %nonempty-string-p (value &optional maximum)
  (and (stringp value)
       (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                   value)))
       (or (null maximum) (<= (length value) maximum))))

(defun %agent-id-p (value)
  (and (%nonempty-string-p value 64)
       (let ((first (char value 0)))
         (or (and (char>= first #\a) (char<= first #\z))
             (digit-char-p first)))
       (every (lambda (character)
                (or (and (char>= character #\a) (char<= character #\z))
                    (digit-char-p character)
                    (member character '(#\. #\_ #\-) :test #'char=)))
              value)))

(defun validate-state
    (&key (agent-id *agent-id*) (memory-form "legacy-unclassified")
          (disclosure-class "private") share-review-status
          share-review-event-id share-reviewed-at valid-from valid-to)
  "Return validation reasons for one memory row state; NIL means valid."
  (let ((agent (%string agent-id))
        (form (%string memory-form))
        (disclosure (%string disclosure-class))
        (review (%string share-review-status))
        (reasons nil))
    (unless (%agent-id-p agent) (push "invalid agent_id" reasons))
    (unless (member form *memory-forms* :test #'string=)
      (push "invalid memory_form" reasons))
    (unless (member disclosure *disclosure-classes* :test #'string=)
      (push "invalid disclosure_class" reasons))
    (when (and review
               (not (member review *share-review-statuses* :test #'string=)))
      (push "invalid share_review_status" reasons))
    (when (and disclosure (not (string= disclosure "private"))
               (not (and (string= (or review "") "approved")
                         (%nonempty-string-p share-review-event-id)
                         share-reviewed-at)))
      (push "non-private disclosure requires an approved durable review"
            reasons))
    (when (and valid-from valid-to (not (< valid-from valid-to)))
      (push "valid_to must be later than valid_from" reasons))
    (nreverse (remove-duplicates reasons :test #'string=))))

(defun row-eligible-p (row &key (agent-id *agent-id*) (audience :operator))
  "Apply only the pure partition/disclosure fence to ROW."
  (when (hash-table-p row)
    (let ((row-agent (%string (gethash "agent_id" row)))
          (disclosure (%string (gethash "disclosure_class" row)))
          (review (%string (gethash "share_review_status" row)))
          (event-id (gethash "share_review_event_id" row))
          (reviewed-at (gethash "share_reviewed_at" row)))
      (and (string= (or row-agent "") (%string agent-id))
           (case audience
             (:operator
              (not (null (member disclosure *disclosure-classes*
                                 :test #'string=))))
             (:external-party
              (and (member disclosure '("personal-shareable" "public")
                           :test #'string=)
                   (string= (or review "") "approved")
                   (%nonempty-string-p event-id)
                   reviewed-at))
             (otherwise nil))))))

(defun %list (value)
  (cond ((null value) nil)
        ((vectorp value) (coerce value 'list))
        ((listp value) value)
        (t (error "Memory atom array field is not an array."))))

(defun %exact-keys (table allowed label)
  (unless (hash-table-p table) (error "~a is not an object." label))
  (loop for key being the hash-keys of table
        unless (member key allowed :test #'string=)
          do (error "Unknown ~a key ~a." label key))
  (dolist (key allowed)
    (unless (nth-value 1 (gethash key table))
      (error "Missing required ~a key ~a." label key)))
  table)

(defun %safe-id-p (value &key (colon t))
  (and (%nonempty-string-p value 160)
       (every (lambda (character)
                (or (alphanumericp character)
                    (member character
                            (if colon '(#\. #\_ #\- #\:)
                                '(#\. #\_ #\-))
                            :test #'char=)))
              value)))

(defun %leap-year-p (year)
  (and (zerop (mod year 4))
       (or (not (zerop (mod year 100))) (zerop (mod year 400)))))

(defun %days-in-month (year month)
  (case month
    ((1 3 5 7 8 10 12) 31)
    ((4 6 9 11) 30)
    (2 (if (%leap-year-p year) 29 28))
    (otherwise 0)))

(defun %utc-timestamp-p (value)
  (and (stringp value) (= (length value) 20)
       (char= (char value 4) #\-) (char= (char value 7) #\-)
       (char= (char value 10) #\T) (char= (char value 13) #\:)
       (char= (char value 16) #\:) (char= (char value 19) #\Z)
       (every #'digit-char-p
              (loop for index in '(0 1 2 3 5 6 8 9 11 12 14 15 17 18)
                    collect (char value index)))
       (let ((year (parse-integer value :start 0 :end 4))
             (month (parse-integer value :start 5 :end 7))
             (day (parse-integer value :start 8 :end 10))
             (hour (parse-integer value :start 11 :end 13))
             (minute (parse-integer value :start 14 :end 16))
             (second (parse-integer value :start 17 :end 19)))
         (and (<= 1970 year 9999) (<= 1 month 12)
              (<= 1 day (%days-in-month year month))
              (<= 0 hour 23) (<= 0 minute 59) (<= 0 second 59)))))

(defun %nullable-timestamp (value label)
  (cond ((or (null value) (eq value :null)) :null)
        ((%utc-timestamp-p value) value)
        (t (error "~a must be null or a canonical UTC timestamp." label))))

(defun %normalize-text (value)
  (string-downcase
   (with-output-to-string (stream)
     (let ((space-p nil))
       (loop for character across
             (string-trim '(#\Space #\Tab #\Newline #\Return) value)
             do (if (member character '(#\Space #\Tab #\Newline #\Return))
                    (setf space-p t)
                    (progn
                      (when space-p (write-char #\Space stream))
                      (setf space-p nil)
                      (write-char character stream))))))))

(defun %canonical-field (value)
  (let ((text (if (eq value :null) "<null>" (format nil "~a" value))))
    (format nil "~d:~a" (length text) text)))

(defun %sha256 (&rest fields)
  (let* ((canonical (format nil "~{~a~^|~}"
                            (mapcar #'%canonical-field fields)))
         (octets (babel:string-to-octets canonical :encoding :utf-8)))
    (string-downcase
     (ironclad:byte-array-to-hex-string
      (ironclad:digest-sequence :sha256 octets)))))

(defun %validate-evidence (evidence)
  (%exact-keys evidence '("id" "role" "sequence" "observed_at" "content")
               "evidence")
  (let ((id (gethash "id" evidence)) (role (gethash "role" evidence))
        (sequence (gethash "sequence" evidence))
        (observed-at (gethash "observed_at" evidence))
        (content (gethash "content" evidence)))
    (unless (%safe-id-p id) (error "Evidence id is invalid."))
    (unless (member role *atom-roles* :test #'string=)
      (error "Evidence role is invalid."))
    (unless (and (integerp sequence) (<= 0 sequence 9999))
      (error "Evidence sequence is invalid."))
    (unless (%utc-timestamp-p observed-at)
      (error "Evidence observed_at is not canonical UTC."))
    (unless (%nonempty-string-p content *atom-max-evidence-chars*)
      (error "Evidence content is empty or too large."))
    evidence))

(defun build-atom-manifest (turn-id captured-at evidence)
  (unless (%safe-id-p turn-id) (error "Turn id is invalid."))
  (unless (%utc-timestamp-p captured-at)
    (error "Capture time is not canonical UTC."))
  (let ((rows (%list evidence)))
    (unless (<= 1 (length rows) *atom-max-evidence*)
      (error "Evidence count is outside the bounded range."))
    (mapc #'%validate-evidence rows)
    (unless (every (lambda (row)
                     (not (string< captured-at (gethash "observed_at" row))))
                   rows)
      (error "Evidence cannot be observed after the captured turn time."))
    (let ((ids (mapcar (lambda (row) (gethash "id" row)) rows))
          (sequences (mapcar (lambda (row) (gethash "sequence" row)) rows)))
      (unless (= (length ids) (length (remove-duplicates ids :test #'string=)))
        (error "Evidence ids must be unique."))
      (unless (= (length sequences) (length (remove-duplicates sequences)))
        (error "Evidence sequences must be unique."))
      (unless (equal sequences (sort (copy-list sequences) #'<))
        (error "Evidence must be supplied in sequence order.")))
    (%object "schema_version" *atom-schema-version*
             "contract_version" *atom-contract-version*
             "prompt_version" *atom-prompt-version*
             "agent_id" *agent-id* "turn_id" turn-id
             "captured_at" captured-at "evidence" (coerce rows 'vector))))

(defun %write-json-string (value stream)
  (write-char #\" stream)
  (loop for character across value do
    (case character
      (#\" (write-string "\\\"" stream))
      (#\\ (write-string "\\\\" stream))
      (#\Backspace (write-string "\\b" stream))
      (#\Page (write-string "\\f" stream))
      (#\Newline (write-string "\\n" stream))
      (#\Return (write-string "\\r" stream))
      (#\Tab (write-string "\\t" stream))
      (otherwise
       (if (< (char-code character) 32)
           (format stream "\\u~4,'0x" (char-code character))
           (write-char character stream)))))
  (write-char #\" stream))

(defun %json-indent (stream depth)
  (write-string (make-string (* 2 depth) :initial-element #\Space) stream))

(defun %write-json (value stream &optional (depth 0))
  (cond
    ((hash-table-p value)
     (write-char #\{ stream)
     (unless (zerop (hash-table-count value))
       (terpri stream)
       (let ((first t))
         (maphash (lambda (key item)
                    (unless first (progn (write-char #\, stream)
                                         (terpri stream)))
                    (setf first nil)
                    (%json-indent stream (1+ depth))
                    (%write-json-string key stream)
                    (write-string ": " stream)
                    (%write-json item stream (1+ depth)))
                  value))
       (terpri stream)
       (%json-indent stream depth))
     (write-char #\} stream))
    ((stringp value) (%write-json-string value stream))
    ((vectorp value)
     (write-char #\[ stream)
     (unless (zerop (length value))
       (terpri stream)
       (loop for item across value for index from 0 do
         (when (plusp index) (progn (write-char #\, stream) (terpri stream)))
         (%json-indent stream (1+ depth))
         (%write-json item stream (1+ depth)))
       (terpri stream)
       (%json-indent stream depth))
     (write-char #\] stream))
    ((eq value :null) (write-string "null" stream))
    ((eq value t) (write-string "true" stream))
    ((null value) (write-string "false" stream))
    ((numberp value) (princ value stream))
    (t (error "Unsupported pure JSON value ~s." value))))

(defun %json (value)
  (with-output-to-string (stream) (%write-json value stream)))

(defun build-atom-request (manifest)
  (%exact-keys manifest
               '("schema_version" "contract_version" "prompt_version"
                 "agent_id" "turn_id" "captured_at" "evidence") "manifest")
  (unless (and (= (gethash "schema_version" manifest -1) 1)
               (string= (gethash "contract_version" manifest "")
                        *atom-contract-version*)
               (string= (gethash "prompt_version" manifest "")
                        *atom-prompt-version*)
               (string= (gethash "agent_id" manifest "") *agent-id*))
    (error "Manifest version or agent does not match the atom contract."))
  (build-atom-manifest (gethash "turn_id" manifest)
                       (gethash "captured_at" manifest)
                       (gethash "evidence" manifest))
  (vector (%object "role" "system" "content" *atom-system-instruction*)
          (%object "role" "user" "content" (%json manifest))))

(defun %manifest-map (manifest)
  (let ((table (make-hash-table :test #'equal)))
    (dolist (row (%list (gethash "evidence" manifest)))
      (setf (gethash (gethash "id" row) table) row))
    table))

(defun %string-array (value maximum label &key safe-ids)
  (let ((items (%list value)))
    (unless (<= (length items) maximum) (error "~a exceeds its bound." label))
    (dolist (item items)
      (unless (if safe-ids (%safe-id-p item)
                  (%nonempty-string-p item *atom-max-annotation-chars*))
        (error "~a contains an invalid string." label)))
    (unless (= (length items) (length (remove-duplicates items :test #'string=)))
      (error "~a contains duplicates." label))
    items))

(defun %normalize-qualifiers (value)
  (let ((items (%list value)))
    (when (> (length items) *atom-max-qualifiers*)
      (error "Atom qualifier count exceeds its bound."))
    (let ((normalized
            (mapcar
             (lambda (item)
               (%exact-keys item '("name" "value") "qualifier")
               (let ((name (gethash "name" item))
                     (item-value (gethash "value" item)))
                 (unless (and (%safe-id-p name :colon nil)
                              (%nonempty-string-p item-value
                                                  *atom-max-value-chars*))
                   (error "Atom qualifier is invalid."))
                 (%object "name" (string-downcase name) "value" item-value)))
             items)))
      (setf normalized
            (sort normalized #'string<
                  :key (lambda (item)
                         (format nil "~a=~a" (gethash "name" item)
                                 (%normalize-text (gethash "value" item))))))
      (let ((names (mapcar (lambda (item) (gethash "name" item)) normalized)))
        (unless (= (length names)
                   (length (remove-duplicates names :test #'string=)))
          (error "Atom qualifier names must be unique.")))
      normalized)))

(defun %attribution-valid-p (subject evidence-rows)
  (cond ((string= subject *operator-id*)
         (find "user" evidence-rows :test #'string=
               :key (lambda (row) (gethash "role" row))))
        ((string= subject *agent-id*)
         (find "assistant" evidence-rows :test #'string=
               :key (lambda (row) (gethash "role" row))))
        (t (find-if (lambda (row)
                      (member (gethash "role" row) '("user" "tool")
                              :test #'string=))
                    evidence-rows))))

(defun %qualifier-canonical-text (qualifiers)
  (format nil "~{~a~^;~}"
          (mapcar (lambda (item)
                    (format nil "~a=~a" (gethash "name" item)
                            (%normalize-text (gethash "value" item))))
                  qualifiers)))

(defun %normalize-one (atom manifest evidence-map)
  (%exact-keys atom
               '("memory_form" "subject" "predicate" "value" "polarity"
                 "qualifiers" "observed_at" "valid_from" "valid_to"
                 "disclosure_candidate" "evidence_ids") "atom")
  (let* ((form (gethash "memory_form" atom))
         (subject (gethash "subject" atom))
         (predicate (gethash "predicate" atom))
         (value (gethash "value" atom))
         (polarity (gethash "polarity" atom))
         (disclosure (gethash "disclosure_candidate" atom))
         (roots (%string-array (gethash "evidence_ids" atom) *atom-max-roots*
                               "atom evidence_ids" :safe-ids t))
         (evidence-rows (mapcar (lambda (id) (gethash id evidence-map)) roots))
         (qualifiers (%normalize-qualifiers (gethash "qualifiers" atom)))
         (observed-at (gethash "observed_at" atom))
         (valid-from (%nullable-timestamp (gethash "valid_from" atom)
                                          "valid_from"))
         (valid-to (%nullable-timestamp (gethash "valid_to" atom)
                                        "valid_to")))
    (unless (member form *atom-forms* :test #'string=)
      (error "Atom memory_form is invalid."))
    (unless (and (%safe-id-p subject)
                 (string= subject (string-downcase subject)))
      (error "Atom subject is invalid or not normalized."))
    (unless (and (%safe-id-p predicate :colon nil)
                 (string= predicate (string-downcase predicate)))
      (error "Atom predicate is invalid or not normalized."))
    (unless (%nonempty-string-p value *atom-max-value-chars*)
      (error "Atom value is empty or too large."))
    (unless (member polarity '("affirmed" "negated") :test #'string=)
      (error "Atom polarity is invalid."))
    (unless (member disclosure *atom-disclosure-candidates* :test #'string=)
      (error "Atom disclosure candidate is invalid."))
    (unless (and roots (every #'identity evidence-rows))
      (error "Atom cites an unknown evidence id."))
    (unless (%attribution-valid-p subject evidence-rows)
      (error "Atom attribution is not supported by an eligible evidence role."))
    (unless (and (%utc-timestamp-p observed-at)
                 (find observed-at evidence-rows :test #'string=
                       :key (lambda (row) (gethash "observed_at" row))))
      (error "Atom observed_at must copy a cited evidence timestamp."))
    (when (and (not (eq valid-from :null)) (not (eq valid-to :null))
               (not (string< valid-from valid-to)))
      (error "Atom valid_to must be later than valid_from."))
    (let* ((claim-key (%sha256 (gethash "agent_id" manifest) form subject
                               predicate (%normalize-text value) polarity
                               (%qualifier-canonical-text qualifiers)
                               valid-from valid-to))
           (sorted-roots (sort (copy-list roots) #'string<))
           (idempotency-key
             (%sha256 *atom-contract-version* *atom-prompt-version*
                      (gethash "turn_id" manifest) claim-key
                      (format nil "~{~a~^,~}" sorted-roots))))
      (%object "candidate_id" (format nil "atom-candidate:~a"
                                       (subseq idempotency-key 0 32))
               "claim_key" claim-key "idempotency_key" idempotency-key
               "agent_id" *agent-id* "memory_form" form "subject" subject
               "predicate" predicate "value" value "polarity" polarity
               "qualifiers" (coerce qualifiers 'vector)
               "observed_at" observed-at "valid_from" valid-from
               "valid_to" valid-to "evidence_ids" (coerce roots 'vector)
               "disclosure_candidate" disclosure
               "persistence_projection"
               (%object "memory_form" form "disclosure_class" "private"
                        "share_review_status"
                        (if (string= disclosure "private") :null "pending"))))))

(defun %normalize-exclusion (item evidence-map)
  (%exact-keys item '("evidence_ids" "reason") "exclusion")
  (let ((roots (%string-array (gethash "evidence_ids" item) *atom-max-roots*
                              "exclusion evidence_ids" :safe-ids t))
        (reason (gethash "reason" item)))
    (unless (and roots (every (lambda (id) (gethash id evidence-map)) roots))
      (error "Exclusion cites an unknown evidence id."))
    (unless (%nonempty-string-p reason *atom-max-annotation-chars*)
      (error "Exclusion reason is invalid."))
    (%object "evidence_ids" (coerce roots 'vector) "reason" reason)))

(defun validate-atom-response (response manifest)
  (build-atom-request manifest)
  (%exact-keys response
               '("schema_version" "decision" "atoms" "exclusions"
                 "uncertainty") "response")
  (unless (= (gethash "schema_version" response -1) 1)
    (error "Unsupported atom response schema."))
  (let* ((decision (gethash "decision" response))
         (atom-items (%list (gethash "atoms" response)))
         (exclusion-items (%list (gethash "exclusions" response)))
         (uncertainty (gethash "uncertainty" response))
         (evidence-map (%manifest-map manifest)))
    (unless (member decision '("PROPOSE" "NO_ATOMS") :test #'string=)
      (error "Atom response decision is invalid."))
    (unless (<= (length atom-items) *atom-max-atoms*)
      (error "Atom response exceeds the atom bound."))
    (unless (<= (length exclusion-items) *atom-max-evidence*)
      (error "Atom response exceeds the exclusion bound."))
    (when (or (and (string= decision "PROPOSE") (null atom-items))
              (and (string= decision "NO_ATOMS") atom-items))
      (error "Atom decision and atom count disagree."))
    (%exact-keys uncertainty '("level" "note") "uncertainty")
    (let ((level (gethash "level" uncertainty))
          (note (gethash "note" uncertainty)))
      (unless (member level '("low" "medium" "high") :test #'string=)
        (error "Atom uncertainty level is invalid."))
      (unless (%nonempty-string-p note *atom-max-annotation-chars*)
        (error "Atom uncertainty note is invalid."))
      (let* ((atoms (mapcar (lambda (atom)
                              (%normalize-one atom manifest evidence-map))
                            atom-items))
             (claim-keys (mapcar (lambda (atom) (gethash "claim_key" atom))
                                 atoms))
             (exclusions (mapcar (lambda (item)
                                   (%normalize-exclusion item evidence-map))
                                 exclusion-items)))
        (unless (= (length claim-keys)
                   (length (remove-duplicates claim-keys :test #'string=)))
          (error "Atom response contains duplicate structural claims."))
        (%object "schema_version" 1 "decision" decision
                 "atoms" (coerce atoms 'vector)
                 "exclusions" (coerce exclusions 'vector)
                 "uncertainty" (%object "level" level "note" note))))))

(defun capability-report ()
  (%object "schema_version" 1 "contract_version" *atom-contract-version*
           "prompt_version" *atom-prompt-version*
           "memory_forms" (coerce *atom-forms* 'vector)
           "raw_evidence_immutable" t "admission_available" nil
           "provider_calls_available" nil "database_writes_available" nil
           "ticks_available" nil "delivery_authority" nil))
