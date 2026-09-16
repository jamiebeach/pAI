;;;; Runtime orchestration of the qualified asks; transport and ledger are ports.
(in-package :pai.context-graph)

(defparameter +cgq-checks+ '("endpoint_identity" "direction" "scope" "statement_fidelity" "time"))

(defun %cgq-review-checks ()
  "Return the sealed semantic dimensions for the active review generation."
  (append +cgq-checks+
          (when (%cgq-durable-relevance-p) '("durable_relevance"))))

(defun %cgq-time-p (value)
  "New protocol accepts null, calendar dates, or UTC second-precision timestamps."
  (or (eq value :null)
      (and (stringp value) (member (length value) '(10 20))
           (char= (char value 4) #\-) (char= (char value 7) #\-)
           (or (= (length value) 10)
               (and (char= (char value 10) #\T) (char= (char value 13) #\:)
                    (char= (char value 16) #\:) (char= (char value 19) #\Z)))
           (loop for i below (length value)
                 always (or (member i '(4 7 10 13 16 19)) (digit-char-p (char value i))))
           (handler-case
               (let* ((year (parse-integer value :end 4))
                      (month (parse-integer value :start 5 :end 7))
                      (day (parse-integer value :start 8 :end 10))
                      (hour (if (= 20 (length value)) (parse-integer value :start 11 :end 13) 0))
                      (minute (if (= 20 (length value)) (parse-integer value :start 14 :end 16) 0))
                      (second (if (= 20 (length value)) (parse-integer value :start 17 :end 19) 0)))
                 (and (<= 1 year 9999) (<= 1 month 12) (<= 1 day 31)
                      (<= 0 hour 23) (<= 0 minute 59) (<= 0 second 59)
                      (multiple-value-bind (s m h d mo y)
                          (decode-universal-time (encode-universal-time second minute hour day month year 0) 0)
                        (equal (list s m h d mo y) (list second minute hour day month year)))))
             (error () nil)))))

(defun %cgq-review-input (context raw revision)
  (unless (loop for relationship across (gethash "relationships" raw)
                always (loop for key in '("occurred_at" "valid_from" "valid_until")
                             always (%cgq-time-p (gethash key (gethash "temporal" relationship)))))
    (%cg-authority-fail "QUALITY_TEMPORAL_INVALID"))
  (let* ((base (%cgm-review-input context raw revision))
         (spec (and (equal "accepted" (gethash "status" base)) (%cg-detach (gethash "value" base)))))
    (unless spec (return-from %cgq-review-input base))
    (let* ((schema (gethash "schema" spec))
           (row (gethash "items" (gethash "claim_reviews" (gethash "properties" schema)))))
      (setf (gethash "quality_checks" (gethash "properties" row))
            (apply #'%cgm-record (loop for key in (%cgq-review-checks)
                                      append (list key (%cgm-enum "supported" "unsupported" "uncertain" "not-applicable"))))
            (gethash "source_reading" (gethash "properties" row))
            (apply #'%cgm-enum (append +cg-claim-scopes+ '("uncertain" "not-applicable")))
            (gethash "required" row) (concatenate 'vector (gethash "required" row) #("quality_checks" "source_reading"))))
    (setf (gethash "adapter_revision" spec) "kg-semantic-review-v2"
          (gethash "system" spec)
          (concatenate 'string (gethash "system" spec)
           " Additionally judge each quality_checks dimension independently. For relationships, supported is required for every dimension: the exact endpoints are identified by the evidence; subject-to-object predicate direction is correct; scope and polarity match the full source; the stored statement and typed relationship describe the same claim without losing values or conditions; and temporal precision is supported. A fluent statement cannot rescue a reversed tuple. related_to does not justify ownership, deployment, or factual status. Read surrounding source for jokes, questions, intentions, and hypotheses, not merely the quoted substring. source_reading describes the source, not the extractor's label. Unsupported or uncertain dimensions must prevent DIRECTLY_EVIDENCED. Never mark a relationship dimension not-applicable (null time is supported only when no unsupported time is asserted). For entities require supported endpoint_identity after checking identity reuse and all descriptor attributes; other dimensions and source_reading must be not-applicable. Shared names alone never prove identity. Do not repair claims in review."
           (if (%cgq-durable-relevance-p)
               " For durable_relevance, supported means the exact proposed relationship has useful cross-turn value as entity knowledge, a stable or recurring relationship/attribute/condition/preference, or another independently useful fact. Mark it unsupported for greetings, thanks, direct-address bookkeeping, one-turn affect scores, reminders or task state already owned by a purpose-built subsystem, UI/tool/response/audit/telemetry events, generated-output handles, or a claim whose only meaning is that a conversation occurred. Embedded durable details do not rescue a conversational-act tuple: require the extractor to propose those details directly. Do not reject a durable external fact merely because it is unrelated to the operator."
               "")))
    (let ((result (%cgm-spec (gethash "tool_name" spec) (gethash "schema" spec)
                             (gethash "system" spec) (gethash "input" spec))))
      (setf (gethash "adapter_revision" (gethash "value" result)) "kg-semantic-review-v2")
      result)))

(defun %cgq-apply-reviewed (graph boundary context raw review revision binding)
  (let* ((built (%cgq-review-input context raw revision)) (spec (gethash "value" built))
         (legacy (%cg-detach review))
         (claims (%cgm-claims (gethash "proposal" (gethash "value" (%cgm-prepare context raw revision))))))
    (unless (and (equal "accepted" (gethash "status" built))
                 (%cgs-schema-valid-p review (gethash "schema" spec))
                 (%cg-closed-keys-p binding '("request_digest" "response_digest"))
                 (equal (gethash "request_digest" binding) (%cg-authority-digest "model-review-input" spec))
                 (equal (gethash "response_digest" binding) (%cg-authority-digest "model-review-output" review)))
      (%cg-authority-fail "QUALITY_REVIEW_INVALID"))
    (loop for row across (gethash "claim_reviews" legacy)
          for claim = (find (gethash "claim_ref" row) claims :test #'equal :key (lambda (c) (gethash "claim_ref" c)))
          for entity-p = (and claim (equal "entity" (gethash "claim_kind" claim)))
          for checks = (gethash "quality_checks" row)
          for reading = (gethash "source_reading" row)
          do (unless (and claim
                          (every (lambda (key) (equal (if (and entity-p (not (equal key "endpoint_identity")))
                                                          "not-applicable" "supported") (gethash key checks))) +cgq-checks+)
                          (or (not (%cgq-durable-relevance-p))
                              (equal (if entity-p "not-applicable" "supported")
                                     (gethash "durable_relevance" checks)))
                          (if entity-p (equal "not-applicable" reading)
                              (or (equal (gethash "scope" (gethash "grounding" (gethash "claim" claim))) reading)
                                  ;; A prior-agent report is not direct authority,
                                  ;; but it may be an authenticated inference premise.
                                  ;; Preserve that distinction for the shared
                                  ;; admission layer instead of pretending the
                                  ;; source was a direct observation.
                                  (and (%cgq-durable-relevance-p)
                                       (%cgm-inference-admission-p)
                                       (equal "reported-speech" reading)
                                       (equal "assertion"
                                              (gethash "scope"
                                                       (gethash "grounding"
                                                                (gethash "claim" claim))))
                                       (not (%cg-authority-assertion-evidence-p
                                             context (gethash "claim" claim)))
                                       (%cg-authority-inference-evidence-p
                                        context (gethash "claim" claim))))))
               (setf (gethash "verdict" row) "UNSUPPORTED"
                     (gethash "evidence" row) "Semantic review dimensions or source reading do not support the proposed claim."))
             (remhash "quality_checks" row) (remhash "source_reading" row))
    ;; Original dimensional review stays in the envelope. Normalize only after
    ;; validating its binding, then reuse unchanged dependency-aware admission.
    (%cgm-apply-reviewed graph boundary context raw legacy revision
      (%cg-object "request_digest" (%cg-authority-digest "model-review-input" (gethash "value" (%cgm-review-input context raw revision)))
                  "response_digest" (%cg-authority-digest "model-review-output" legacy)))))

(defun context-graph-generate-reviewed (context ontology revision call-fn
                                        &key (mode :staged) (quality-review nil))
  "Build an inert replay envelope. CALL-FN receives PHASE, SPEC and request digest.
The owner must authenticate CONTEXT and durably record each provider outcome.
This function neither persists nor applies a graph. Pauses are propagated without
automatic retries. A completed envelope is still subject to shared admission."
  (unless (and (functionp call-fn) (member mode '(:staged :correction)) (member quality-review '(t nil)))
    (%cg-authority-fail "RUNTIME_GENERATION_INPUT_INVALID"))
  (%cg-validate-authority-context context)
  (let ((frozen (%cg-detach context)) (signature (%cg-detach ontology)) (calls nil))
    (block generate
      (labels ((ask (phase built)
                 (unless (equal "accepted" (gethash "status" built))
                   (return-from generate built))
                 (let* ((spec (gethash "value" built))
                        (unused (when (and quality-review (equal phase "facts"))
                                  (setf (gethash "system" spec)
                                        (concatenate 'string (gethash "system" spec)
                                         " Temporal values must be null, YYYY-MM-DD, or YYYY-MM-DDTHH:MM:SSZ. Use null when precision is not supported; never emit source handles or relative phrases as dates. Check the subject-to-object direction against the statement, preserving values and conditions. Jokes and possible future mechanisms are not present factual assertions."))))
                        (bounded (when (> (length (sb-ext:string-to-octets (%cg-authority-canonical-json spec)
                                                                         :external-format :utf-8)) 131072)
                                   (%cg-authority-fail "MODEL_CONTEXT_LIMIT")))
                        (request-digest (%cg-authority-digest "runtime-generation-request-v1"
                                          (%cg-object "phase" phase "context" frozen "spec" spec)))
                        (response (funcall call-fn phase (%cg-detach spec) request-digest)))
                   (declare (ignore unused bounded))
                   (when (member response '(:preempted :paused-budget))
                     (return-from generate response))
                   (unless (hash-table-p response)
                     (%cg-authority-fail "RUNTIME_GENERATION_RESPONSE_INVALID"))
                   (push (%cg-object "phase" phase "request_digest" request-digest
                                     "response_digest" (%cg-authority-digest "runtime-generation-response-v1" response)) calls)
                   (%cg-detach response))))
        (let* ((raw
                 (if (eq mode :correction)
                     (%cgt-correction-expand frozen signature revision
                       (ask "correction" (%cgt-correction-input frozen signature revision)))
                     (let* ((selection (ask "entities" (%cgt-entity-input frozen signature revision)))
                            (facts (ask "facts" (%cgt-fact-input frozen signature revision selection))))
                       (%cgs-expand frozen signature revision (%cgt-combine frozen signature selection facts)))))
               (validated (%cgm-validate-ontology frozen raw signature revision)))
          (unless (equal "accepted" (gethash "status" validated))
            (return-from generate validated))
          (let* ((review-input (if quality-review (%cgq-review-input frozen raw revision) (%cgm-review-input frozen raw revision)))
                 (review (ask "review" review-input)))
            (%cg-authority-result "accepted"
              (%cg-object "schema_version" 1 "generation_revision" (if quality-review "kg-runtime-reviewed-v2" "kg-runtime-reviewed-v1")
                          "mode" (if (eq mode :staged) "staged" "correction")
                          "authority_context" frozen "ontology_revision" revision
                          "proposal" raw "review" review
                          "review_binding"
                          (%cg-object "request_digest" (%cg-authority-digest "model-review-input" (gethash "value" review-input))
                                      "response_digest" (%cg-authority-digest "model-review-output" review))
                          "calls" (coerce (nreverse calls) 'vector)))))))))

(defun context-graph-apply-reviewed-generation (graph boundary envelope)
  "Recompute shared admission from a sealed generation, never trust a saved delta.
The caller owns event authentication, partition access and durable serialization."
  (unless (and (%cg-closed-keys-p envelope '("schema_version" "generation_revision" "mode" "authority_context"
                                            "ontology_revision" "proposal" "review" "review_binding" "calls"))
               (eql 1 (gethash "schema_version" envelope))
               (member (gethash "generation_revision" envelope) '("kg-runtime-reviewed-v1" "kg-runtime-reviewed-v2") :test #'equal)
               (member (gethash "mode" envelope) '("staged" "correction") :test #'equal)
               (%cg-authority-array-p (gethash "calls" envelope) 3 2)
               (equal (map 'list (lambda (row) (and (hash-table-p row) (gethash "phase" row))) (gethash "calls" envelope))
                      (if (equal "staged" (gethash "mode" envelope)) '("entities" "facts" "review") '("correction" "review")))
               (every (lambda (row)
                        (and (%cg-closed-keys-p row '("phase" "request_digest" "response_digest"))
                             (%cg-authority-digest-p (gethash "request_digest" row))
                             (%cg-authority-digest-p (gethash "response_digest" row))))
                      (gethash "calls" envelope)))
    (%cg-authority-fail "RUNTIME_GENERATION_ENVELOPE_INVALID"))
  (funcall (if (equal "kg-runtime-reviewed-v2" (gethash "generation_revision" envelope)) #'%cgq-apply-reviewed #'%cgm-apply-reviewed)
                       graph boundary (gethash "authority_context" envelope)
                       (gethash "proposal" envelope) (gethash "review" envelope)
                       (gethash "ontology_revision" envelope) (gethash "review_binding" envelope)))
