;;;; context-assembly.lisp -- Q4 pure, bounded conscious context rendering.
;;;;
;;;; The caller supplies every record and policy input explicitly. This file
;;;; performs no retrieval and no I/O. Private rendered messages are returned
;;;; separately from a content-free manifest suitable for durable diagnostics.

(in-package :agent)

(export '(make-conscious-assembly-context conscious-assembly-context-p
          conscious-context-assemble conscious-context-manifest-report
          *conscious-context-section-order*))

(defparameter *conscious-context-assembly-schema-version* 3)
(defparameter *conscious-context-compatibility-revision*
  "dedicated-untrusted-tool-results-v1")
(defparameter *conscious-context-legacy-tool-section-revision*
  "legacy-unresolved-effects-conclusions-v1")
(defparameter *conscious-context-section-order*
  (vector "identity-instructions" "sensorium" "focus-lifecycles"
          "triggering-stimuli" "conversation-evidence" "memory-bundles"
          "untrusted-tool-results" "tools-proposal-schema"
          "publication-constraints"))
(defparameter *conscious-context-required-sections*
  '("untrusted-tool-results"))

(defparameter *conscious-context-section-roles*
  '(("identity-instructions" . "governing-instructions")
    ("sensorium" . "measured-runtime-data")
    ("focus-lifecycles" . "active-lifecycle-data")
    ("triggering-stimuli" . "current-stimulus")
    ("conversation-evidence" . "historical-conversation-data")
    ("memory-bundles" . "memory-data")
    ("untrusted-tool-results" . "untrusted-model-data")
    ("tools-proposal-schema" . "capability-schema")
    ("publication-constraints" . "publication-policy")))

(defun %ca-exact-keys (table keys label)
  (unless (hash-table-p table) (error "~a must be an object" label))
  (loop for key being the hash-keys of table
        unless (member key keys :test #'string=)
          do (error "Unknown ~a key ~s" label key))
  (dolist (key keys)
    (unless (nth-value 1 (gethash key table))
      (error "Missing ~a key ~s" label key)))
  table)

(defun %ca-items (value label &optional (maximum 256))
  (let ((items (cond ((vectorp value) (coerce value 'list))
                     ((listp value) (copy-list value))
                     (t (error "~a must be an array" label)))))
    (unless (<= (length items) maximum)
      (error "~a exceeds its item bound" label))
    items))

(defun %ca-data-only-p (value &optional (active (make-hash-table :test #'eq)))
  (cond
    ((or (null value) (eq value t) (stringp value) (keywordp value)
         (and (realp value) (or (not (floatp value)) (= value value)))) t)
    ((or (functionp value) (streamp value) (pathnamep value)) nil)
    ((hash-table-p value)
     (and (not (gethash value active))
          (progn
            (setf (gethash value active) t)
            (prog1
                (loop for key being the hash-keys of value using (hash-value item)
                      always (and (stringp key) (%ca-data-only-p item active)))
              (remhash value active)))))
    ((vectorp value)
     (and (<= (length value) 1024) (not (gethash value active))
          (progn
            (setf (gethash value active) t)
            (prog1 (loop for item across value
                         always (%ca-data-only-p item active))
              (remhash value active)))))
    ((listp value)
     (let ((length (ignore-errors (list-length value))))
       (and length (<= length 1024) (not (gethash value active))
            (progn
              (setf (gethash value active) t)
              (prog1 (every (lambda (item) (%ca-data-only-p item active)) value)
                (remhash value active))))))
    (t nil)))

(defun %ca-text (value label maximum)
  (unless (and (stringp value) (plusp (length value))
               (<= (length value) maximum))
    (error "~a must be non-empty bounded text" label))
  value)

(defun %ca-id-p (value)
  (or (and (stringp value) (plusp (length value)) (<= (length value) 256))
      (and (integerp value) (plusp value))))

(defun %ca-fnv (text)
  (let ((hash 14695981039346656037))
    (declare (type (unsigned-byte 64) hash))
    (loop for character across text
          do (setf hash
                   (ldb (byte 64 0)
                        (* (logxor hash (char-code character))
                           1099511628211))))
    (format nil "~(~16,'0x~)" hash)))

(defun %ca-section-names ()
  (coerce *conscious-context-section-order* 'list))

(defun %ca-shallow-copy-object (value label)
  (unless (hash-table-p value) (error "~a must be an object" label))
  (let ((copy (make-hash-table :test (hash-table-test value))))
    (maphash (lambda (key item) (setf (gethash key copy) item)) value)
    copy))

(defun %ca-normalize-section-inputs (budgets sections revision)
  "Read the retired tool section only under its named migration revision."
  (unless (member revision
                  (list *conscious-context-compatibility-revision*
                        *conscious-context-legacy-tool-section-revision*)
                  :test #'string=)
    (error "Unknown conscious context compatibility revision"))
  (if (string= revision *conscious-context-legacy-tool-section-revision*)
      (let ((normalized-budgets (%ca-shallow-copy-object budgets "section budgets"))
            (normalized-sections (%ca-shallow-copy-object sections "context sections"))
            (legacy "unresolved-effects-conclusions")
            (current "untrusted-tool-results"))
        (unless (and (nth-value 1 (gethash legacy normalized-budgets))
                     (nth-value 1 (gethash legacy normalized-sections))
                     (not (nth-value 1 (gethash current normalized-budgets)))
                     (not (nth-value 1 (gethash current normalized-sections))))
          (error "Legacy context revision requires exactly its retired tool section"))
        (setf (gethash current normalized-budgets)
              (gethash legacy normalized-budgets)
              (gethash current normalized-sections)
              (gethash legacy normalized-sections))
        (remhash legacy normalized-budgets)
        (remhash legacy normalized-sections)
        (values normalized-budgets normalized-sections))
      (progn
        (when (or (nth-value 1
                            (gethash "unresolved-effects-conclusions" budgets))
                  (nth-value 1
                            (gethash "unresolved-effects-conclusions" sections)))
          (error "Retired tool section requires the explicit legacy revision"))
        (values budgets sections))))

(defun make-conscious-assembly-context
    (&key pulse-id purpose audience runtime-revision conscious-state-revision
          clock-identity total-character-budget section-character-budgets
          sections eligible-evidence-ids available-tools
          permitted-proposal-kinds publication-constraints remaining-budget
          (compatibility-revision
            *conscious-context-compatibility-revision*)
          (pre-render-refusals (vector)))
  "Capture every varying Q4 assembly input without consulting later globals."
  (multiple-value-bind (normalized-budgets normalized-sections)
      (%ca-normalize-section-inputs
       section-character-budgets sections compatibility-revision)
    (obj "schema_version" *conscious-context-assembly-schema-version*
         "compatibility_revision" compatibility-revision
         "pulse_id" pulse-id "purpose" purpose "audience" audience
         "runtime_revision" runtime-revision
         "conscious_state_revision" conscious-state-revision
         "clock_identity" clock-identity
         "total_character_budget" total-character-budget
         "section_character_budgets" normalized-budgets
         "sections" normalized-sections
         "eligible_evidence_ids" eligible-evidence-ids
         "available_tools" available-tools
         "permitted_proposal_kinds" permitted-proposal-kinds
         "publication_constraints" publication-constraints
         "remaining_budget" remaining-budget
         "pre_render_refusals" pre-render-refusals)))

(defun conscious-assembly-context-p (value)
  (and (hash-table-p value)
       (eql *conscious-context-assembly-schema-version*
            (gethash "schema_version" value))))

(defun %ca-validate-context (context state)
  (%ca-exact-keys
   context
   '("schema_version" "compatibility_revision" "pulse_id" "purpose"
     "audience" "runtime_revision"
     "conscious_state_revision" "clock_identity" "total_character_budget"
     "section_character_budgets" "sections" "eligible_evidence_ids"
     "available_tools" "permitted_proposal_kinds" "publication_constraints"
     "remaining_budget" "pre_render_refusals")
   "assembly context")
  (unless (and (conscious-assembly-context-p context) (%ca-data-only-p context))
    (error "Assembly context must be bounded data"))
  (unless (member (gethash "compatibility_revision" context)
                  (list *conscious-context-compatibility-revision*
                        *conscious-context-legacy-tool-section-revision*)
                  :test #'string=)
    (error "Assembly context compatibility revision is invalid"))
  (dolist (key '("pulse_id" "purpose" "audience" "runtime_revision"
                 "clock_identity"))
    (%ca-text (gethash key context) key 256))
  (unless (and (hash-table-p state) (%ca-data-only-p state))
    (error "Conscious state must be bounded data"))
  (unless (and (integerp (gethash "conscious_state_revision" context))
               (eql (gethash "conscious_state_revision" context)
                    (gethash "state_revision" state)))
    (error "Assembly context does not match the conscious-state revision"))
  (unless (and (integerp (gethash "total_character_budget" context))
               (plusp (gethash "total_character_budget" context))
               (<= (gethash "total_character_budget" context) 1000000))
    (error "Total context character budget is invalid"))
  (let ((budgets (gethash "section_character_budgets" context))
        (sections (gethash "sections" context)))
    (%ca-exact-keys budgets (%ca-section-names) "section budgets")
    (%ca-exact-keys sections (%ca-section-names) "context sections")
    (dolist (name (%ca-section-names))
      (unless (and (integerp (gethash name budgets))
                   (<= 0 (gethash name budgets) 1000000))
        (error "Context section budget ~a is invalid" name))
      (%ca-items (gethash name sections) name 128)))
  (let ((eligible (%ca-items (gethash "eligible_evidence_ids" context)
                             "eligible evidence" 512)))
    (unless (and (every #'%ca-id-p eligible)
                 (= (length eligible)
                    (length (remove-duplicates eligible :test #'equal))))
      (error "Eligible evidence IDs must be unique bounded identifiers")))
  (dolist (key '("available_tools" "permitted_proposal_kinds"))
    (let ((items (%ca-items (gethash key context) key 128)))
      (unless (and (every (lambda (item)
                            (and (stringp item) (plusp (length item))
                                 (<= (length item) 128)))
                          items)
                   (= (length items)
                      (length (remove-duplicates items :test #'string=))))
        (error "~a must contain unique bounded text names" key))))
  (when (boundp '*conscious-proposal-kinds*)
    (unless (every (lambda (kind)
                     (member kind (symbol-value '*conscious-proposal-kinds*)
                             :test #'string=))
                   (%ca-items (gethash "permitted_proposal_kinds" context)
                              "permitted proposal kinds" 32))
      (error "Assembly context advertises an unknown proposal kind")))
  (let ((constraints (gethash "publication_constraints" context)))
    (%ca-exact-keys constraints '("audiences") "publication constraints")
    (let ((audiences (%ca-items (gethash "audiences" constraints)
                                "publication audiences" 32)))
      (unless (and (every (lambda (item)
                            (and (stringp item) (plusp (length item))
                                 (<= (length item) 128)))
                          audiences)
                   (member (gethash "audience" context) audiences
                           :test #'string=))
        (error "Context audience is not authorized by publication constraints"))))
  (let ((remaining (gethash "remaining_budget" context)))
    (%ca-exact-keys remaining
                    '("tool_proposals" "continuations" "publication_candidates")
                    "remaining budget")
    (dolist (key '("tool_proposals" "continuations" "publication_candidates"))
      (unless (and (integerp (gethash key remaining))
                   (<= 0 (gethash key remaining) 64))
        (error "Remaining ~a budget is invalid" key))))
  context)

(defun %ca-section-role (name)
  (or (cdr (assoc name *conscious-context-section-roles* :test #'string=))
      (error "No role is defined for context section ~a" name)))

(defun %ca-context-record-provenance (record eligible)
  (let ((count (hash-table-count record)))
    (unless (or (and (= count 2)
                     (nth-value 1 (gethash "source_id" record))
                     (nth-value 1 (gethash "content" record)))
                (and (= count 3)
                     (nth-value 1 (gethash "source_id" record))
                     (nth-value 1 (gethash "content" record))
                     (nth-value 1 (gethash "provenance" record))))
      (error "Context record has unknown or missing keys")))
  (let ((provenance (gethash "provenance" record)))
    (when provenance
      (%ca-exact-keys provenance
                      '("descriptor_id" "descriptor_event_id"
                        "evidence_event_ids")
                      "context record provenance")
      (%ca-text (gethash "descriptor_id" provenance) "descriptor ID" 256)
      (unless (%ca-id-p (gethash "descriptor_event_id" provenance))
        (error "Context descriptor event ID is invalid"))
      (let ((ids (%ca-items (gethash "evidence_event_ids" provenance)
                            "descriptor evidence" 64)))
        (unless (and (plusp (length ids)) (every #'%ca-id-p ids)
                     (= (length ids)
                        (length (remove-duplicates ids :test #'equal)))
                     (every (lambda (id) (member id eligible :test #'equal)) ids)
                     (member (gethash "descriptor_event_id" provenance)
                             ids :test #'equal))
          (error "Context descriptor provenance is invalid"))))
    provenance))

(defun %ca-pre-render-refusals (context eligible)
  (let ((allowed-reasons
          '("semantic-unavailable" "mind-identity-mismatch"
            "disclosure-policy-refused" "descriptor-invalid"))
        (items (%ca-items (gethash "pre_render_refusals" context)
                          "pre-render refusals" 128)))
    (dolist (row items)
      (%ca-exact-keys row '("source_id" "section" "reason" "descriptor_id")
                      "pre-render refusal")
      (unless (and (%ca-id-p (gethash "source_id" row))
                   (member (gethash "source_id" row) eligible :test #'equal)
                   (member (gethash "section" row) (%ca-section-names)
                           :test #'string=)
                   (member (gethash "reason" row) allowed-reasons
                           :test #'string=)
                   (let ((descriptor (gethash "descriptor_id" row)))
                     (or (null descriptor) (eq descriptor :null)
                         (and (stringp descriptor) (plusp (length descriptor))
                              (<= (length descriptor) 256)))))
        (error "Pre-render refusal is invalid")))
    items))

(defun conscious-context-assemble (state context)
  "Return a detached private request and a content-free deterministic manifest."
  (%ca-validate-context context state)
  (let* ((eligible (%ca-items (gethash "eligible_evidence_ids" context)
                              "eligible evidence" 512))
         (budgets (gethash "section_character_budgets" context))
         (section-inputs (gethash "sections" context))
         (total-budget (gethash "total_character_budget" context))
         (total-used 0)
         (messages '())
         (section-manifests '())
         (included-evidence '())
         (pre-render-refusals (%ca-pre-render-refusals context eligible))
         (required-size
           (loop for name in *conscious-context-required-sections*
                 sum (loop for record in
                           (%ca-items (gethash name section-inputs) name 128)
                           sum (length (%ca-text (gethash "content" record)
                                                 "Required context record content"
                                                 65536)))))
         (hash-parts
           (mapcar (lambda (row)
                     (format nil "refusal/~a/~a/~a/~a"
                             (gethash "section" row) (gethash "source_id" row)
                             (gethash "reason" row)
                             (gethash "descriptor_id" row)))
                   pre-render-refusals)))
    (dolist (name *conscious-context-required-sections*)
      (when (> required-size (gethash name budgets))
        (error "Required context section ~a exceeds its section budget" name)))
    (when (> required-size total-budget)
      (error "Required context evidence exceeds the total budget"))
    (dolist (name (%ca-section-names))
      (let ((section-used 0)
            (section-budget (gethash name budgets))
            (included '())
            (included-provenance '())
            (refused '())
            (content-hashes '()))
        (dolist (record (%ca-items (gethash name section-inputs) name 128))
          (let ((source-id (gethash "source_id" record))
                (content (gethash "content" record))
                (provenance (%ca-context-record-provenance record eligible)))
            (unless (%ca-id-p source-id)
              (error "Context record has an invalid source ID"))
            (unless (member source-id eligible :test #'equal)
              (error "Context attempted to render undeclared source ~s" source-id))
            (%ca-text content "Context record content" 65536)
            (let ((size (length content)))
              (cond
                ((> (+ section-used size) section-budget)
                 (push (obj "source_id" source-id
                            "reason" "section-budget-exhausted") refused))
                 ((> (+ total-used size)
                     (if (member name *conscious-context-required-sections*
                                 :test #'string=)
                         total-budget
                         (- total-budget required-size)))
                  (push (obj "source_id" source-id
                             "reason" "total-budget-exhausted") refused))
                (t
                 (incf section-used size)
                 (incf total-used size)
                 (push source-id included)
                 (pushnew source-id included-evidence :test #'equal)
                 (when provenance
                   (push provenance included-provenance)
                   (dolist (id (coerce (gethash "evidence_event_ids" provenance)
                                       'list))
                     (pushnew id included-evidence :test #'equal)))
                 (let ((content-hash (%ca-fnv content)))
                   (push content-hash content-hashes)
                   (push (format nil "~a/~a/~a" name source-id content-hash)
                         hash-parts))
                 (push (obj "role" (%ca-section-role name)
                            "section" name "source_id" source-id
                            "content" content)
                       messages))))))
        (push (obj "name" name "role" (%ca-section-role name)
                   "budget_characters" section-budget
                   "used_characters" section-used
                   "included_source_ids" (coerce (nreverse included) 'vector)
                   "included_provenance"
                   (coerce (nreverse included-provenance) 'vector)
                   "refused" (coerce (nreverse refused) 'vector)
                   "content_hash"
                   (%ca-fnv (format nil "~{~a~^|~}" (nreverse content-hashes))))
              section-manifests)))
    (let* ((sections (coerce (nreverse section-manifests) 'vector))
           (composition-hash
             (%ca-fnv
              (format nil "schema=~a;compatibility=~a;pulse=~a;runtime=~a;state=~a;purpose=~a;audience=~a;clock=~a;state-composition=~a;content=~{~a~^;~}"
                      *conscious-context-assembly-schema-version*
                      (gethash "compatibility_revision" context)
                      (gethash "pulse_id" context)
                      (gethash "runtime_revision" context)
                      (gethash "conscious_state_revision" context)
                      (gethash "purpose" context) (gethash "audience" context)
                      (gethash "clock_identity" context)
                      (gethash "composition_hash" state :null)
                      (nreverse hash-parts))))
           (manifest
             (obj "schema_version" *conscious-context-assembly-schema-version*
                  "compatibility_revision"
                  (gethash "compatibility_revision" context)
                  "pulse_id" (gethash "pulse_id" context)
                  "purpose" (gethash "purpose" context)
                  "audience" (gethash "audience" context)
                  "runtime_revision" (gethash "runtime_revision" context)
                  "conscious_state_revision"
                  (gethash "conscious_state_revision" context)
                  "clock_identity" (gethash "clock_identity" context)
                  "composition_hash" composition-hash
                  "state_composition_hash"
                  (gethash "composition_hash" state :null)
                  "sections" sections
                  "total_character_budget" total-budget
                  "rendered_characters" total-used
                  "evidence_event_ids" (coerce (nreverse included-evidence) 'vector)
                  "available_tools" (gethash "available_tools" context)
                  "permitted_proposal_kinds"
                  (gethash "permitted_proposal_kinds" context)
                  "publication_constraints"
                  (gethash "publication_constraints" context)
                  "remaining_budget" (gethash "remaining_budget" context)
                  "pre_render_refusals"
                  (coerce pre-render-refusals 'vector))))
      ;; JSON round-tripping detaches both outputs from mutable caller input.
      (shasht:read-json
       (shasht:write-json
        (obj "schema_version" *conscious-context-assembly-schema-version*
             "private_request" (coerce (nreverse messages) 'vector)
             "manifest" manifest)
        nil)))))

(defun conscious-context-manifest-report (manifest)
  "Return content-free assembly diagnostics."
  (unless (hash-table-p manifest) (error "Context manifest must be an object"))
  (let ((sections (%ca-items (gethash "sections" manifest)
                             "manifest sections" 9)))
    (obj "schema_version" (gethash "schema_version" manifest)
         "compatibility_revision"
         (gethash "compatibility_revision" manifest)
         "pulse_id" (gethash "pulse_id" manifest)
         "purpose" (gethash "purpose" manifest)
         "audience" (gethash "audience" manifest)
         "runtime_revision" (gethash "runtime_revision" manifest)
         "conscious_state_revision"
         (gethash "conscious_state_revision" manifest)
         "composition_hash" (gethash "composition_hash" manifest)
         "section_count" (length sections)
         "included_evidence_count"
         (length (gethash "evidence_event_ids" manifest))
         "refused_count"
         (reduce #'+ sections
                 :key (lambda (section) (length (gethash "refused" section)))
                 :initial-value 0)
         "pre_render_refusal_count"
         (length (gethash "pre_render_refusals" manifest (vector)))
         "total_character_budget" (gethash "total_character_budget" manifest)
         "rendered_characters" (gethash "rendered_characters" manifest)
         "available_tool_count" (length (gethash "available_tools" manifest))
         "permitted_proposal_kind_count"
         (length (gethash "permitted_proposal_kinds" manifest)))))
