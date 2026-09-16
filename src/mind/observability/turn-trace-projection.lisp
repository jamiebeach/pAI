;;;; turn-trace-projection.lisp -- Rdev1 pure, content-safe trace projection.

(in-package :agent)

(export '(turn-trace-project turn-trace-fixture turn-trace-fixture-index
          turn-trace-safe-id-p))

(defparameter *turn-trace-projection-schema-version* 1)
(defparameter *turn-trace-safe-attribute-keys*
  '("channel" "message_count" "purpose" "model" "provider" "service_tier"
    "finish_reason" "tool" "k" "mode" "input_chars" "event_type"
    "urgency" "user_visible" "node_count" "evidence_count"
    "memory_spec_count" "tick_type" "prompt_tokens" "completion_tokens"
    "reasoning_tokens" "total_tokens" "cost_usd"))
(defun %turn-trace-fixture-directory ()
  "Locate tests/fixtures/turn-traces/, which lives at the repository root.

   This used to be resolved against this file's own directory, which was the
   repository root under the original flat layout and is several levels down
   from it now. Walk up until the directory appears, so the lookup survives
   the file moving again. PAI_TURN_TRACE_FIXTURES overrides."
  (let ((configured (uiop:getenv "PAI_TURN_TRACE_FIXTURES")))
    (if configured
        (pathname (concatenate 'string configured "/"))
        (loop with dir = (make-pathname :name nil :type nil
                                        :defaults (or *load-truename*
                                                      *default-pathname-defaults*))
              repeat 8
              for candidate = (merge-pathnames #P"tests/fixtures/turn-traces/" dir)
              when (probe-file candidate) return candidate
              do (let ((parent (uiop:pathname-parent-directory-pathname dir)))
                   (when (equal parent dir) (return nil))
                   (setf dir parent))))))

(defparameter *turn-trace-fixture-directory* (%turn-trace-fixture-directory))
(defvar *turn-trace-projection-diagnostics* nil)

(defun %turn-trace-list (value)
  (cond ((null value) nil)
        ((listp value) value)
        ((vectorp value) (coerce value 'list))
        (t nil)))

(defun %turn-trace-real (value &optional default)
  (if (and (realp value) (= value value) (>= value 0)) value default))

(defun %turn-trace-string (value &optional default)
  (if (stringp value) value default))

(defun turn-trace-safe-id-p (value)
  (and (stringp value) (<= 1 (length value) 160)
       (every (lambda (character)
                (or (alphanumericp character)
                    (find character "-_.:" :test #'char=)))
              value)))

(defun %turn-trace-copy-safe-attributes (attributes)
  (let ((copy (obj)))
    (when (hash-table-p attributes)
      (dolist (key *turn-trace-safe-attribute-keys*)
        (multiple-value-bind (value present-p) (gethash key attributes)
          (when (and present-p
                     (or (stringp value) (numberp value)
                         (eq value t) (eq value nil) (eq value :null)))
            (setf (gethash key copy) value)))))
    copy))

(defun %turn-trace-category (name)
  (cond ((search "queue" name) "queue")
        ((or (search "retriev" name) (search "memory" name)
             (search "embed" name)) "retrieval")
        ((search "model" name) "model")
        ((search "tool" name) "tool")
        ((or (search "persist" name) (search "commit" name)) "persistence")
        ((or (search "broadcast" name) (search "public_output" name)) "output")
        ((search "context" name) "context")
        (t "local")))

(defun %turn-trace-safe-span (span trace-duration)
  (if (not (hash-table-p span))
      (progn (push "malformed-span" *turn-trace-projection-diagnostics*) nil)
      (let* ((name (%turn-trace-string (gethash "name" span) "unknown"))
             (start (%turn-trace-real (gethash "start_offset_ms" span) 0))
             (duration (%turn-trace-real (gethash "duration_ms" span) 0))
             (bounded-start (min start trace-duration))
             (bounded-duration (min duration (max 0 (- trace-duration bounded-start)))))
        (when (or (/= start bounded-start) (/= duration bounded-duration))
          (push "span-clamped-to-trace" *turn-trace-projection-diagnostics*))
        (obj "span_id" (or (%turn-trace-string (gethash "span_id" span)) :null)
             "parent_span_id" (or (%turn-trace-string
                                    (gethash "parent_span_id" span)) :null)
             "name" name "category" (%turn-trace-category name)
             "start_offset_ms" bounded-start "duration_ms" bounded-duration
             "status" (%turn-trace-string (gethash "status" span) "unknown")
             "error_class" (or (%turn-trace-string (gethash "error" span)) :null)
             "attributes" (%turn-trace-copy-safe-attributes
                            (gethash "attributes" span))))))

(defun %turn-trace-event-payload (event)
  (and (hash-table-p event) (gethash "payload" event)))

(defun %turn-trace-matching-p (payload turn-id trace-id)
  (and (hash-table-p payload)
       (or (and turn-id (string= turn-id (or (gethash "turn_id" payload) "")))
           (and trace-id (string= trace-id (or (gethash "trace_id" payload) ""))))))

(defun %turn-trace-response-for-call (responses call-id turn-id)
  (find-if
   (lambda (event)
     (let ((payload (%turn-trace-event-payload event)))
       (and (hash-table-p payload)
            (string= call-id (or (gethash "model_call_id" payload) ""))
            (or (null turn-id)
                (string= turn-id (or (gethash "turn_id" payload) ""))))))
   responses))

(defun %turn-trace-choice-finish-reason (response)
  (let* ((choices (and (hash-table-p response)
                       (%turn-trace-list (gethash "choices" response))))
         (choice (first choices)))
    (and (hash-table-p choice)
         (%turn-trace-string (gethash "finish_reason" choice)))))

(defun %turn-trace-provider-attempts (events turn-id)
  (let* ((requests
           (remove-if-not
            (lambda (event)
              (let ((payload (%turn-trace-event-payload event)))
                (and (hash-table-p event) (hash-table-p payload)
                     (string= "model-request" (or (gethash "type" event) ""))
                     (string= turn-id (or (gethash "turn_id" payload) "")))))
            (%turn-trace-list events)))
         (responses
           (remove-if-not
            (lambda (event)
              (string= "model-response" (or (and (hash-table-p event)
                                                   (gethash "type" event)) "")))
            (%turn-trace-list events)))
         (seen (make-hash-table :test #'equal))
         (attempts nil))
    (dolist (request requests)
      (let* ((payload (%turn-trace-event-payload request))
             (call-id (%turn-trace-string (gethash "model_call_id" payload)))
             (request-body (gethash "request" payload))
             (response-event (and call-id
                                  (%turn-trace-response-for-call responses call-id turn-id)))
             (response-payload (%turn-trace-event-payload response-event))
             (response (and (hash-table-p response-payload)
                            (gethash "response" response-payload)))
             (usage (and (hash-table-p response) (gethash "usage" response)))
             (details (and (hash-table-p usage)
                           (gethash "completion_tokens_details" usage))))
        (cond
          ((null call-id) (push "model-request-missing-call-id"
                                *turn-trace-projection-diagnostics*))
          ((gethash call-id seen) (push "duplicate-model-call-id"
                                        *turn-trace-projection-diagnostics*))
          (t
           (setf (gethash call-id seen) t)
           (unless response-event
             (push "unpaired-model-request" *turn-trace-projection-diagnostics*))
           (push
            (obj "model_call_id" call-id
                 "trace_id" (or (%turn-trace-string (gethash "trace_id" payload)) :null)
                 "turn_id" turn-id
                 "purpose" (%turn-trace-string (gethash "purpose" payload) "unknown")
                 "request_kind" (%turn-trace-string
                                  (gethash "request_kind" payload) "unknown")
                 "adapter_kind" (%turn-trace-string
                                  (gethash "adapter_kind" payload) "unknown")
                 "status" (if response-event
                              (%turn-trace-string
                               (gethash "status" response-payload) "unknown")
                              "missing-response")
                 "duration_ms" (or (and (hash-table-p response-payload)
                                         (%turn-trace-real
                                          (gethash "duration_ms" response-payload)))
                                    :null)
                 "model" (or (and (hash-table-p request-body)
                                    (%turn-trace-string (gethash "model" request-body)))
                               (and (hash-table-p response)
                                    (%turn-trace-string (gethash "model" response)))
                               :null)
                 "provider" (or (and (hash-table-p response)
                                      (%turn-trace-string (gethash "provider" response)))
                                 :null)
                 "service_tier" (or (and (hash-table-p response)
                                          (%turn-trace-string
                                           (gethash "service_tier" response)))
                                     :null)
                 "finish_reason" (or (%turn-trace-choice-finish-reason response) :null)
                 "prompt_tokens" (or (and (hash-table-p usage)
                                           (%turn-trace-real
                                            (gethash "prompt_tokens" usage))) :null)
                 "completion_tokens" (or (and (hash-table-p usage)
                                               (%turn-trace-real
                                                (gethash "completion_tokens" usage))) :null)
                 "reasoning_tokens" (or (and (hash-table-p details)
                                              (%turn-trace-real
                                               (gethash "reasoning_tokens" details))) :null)
                 "total_tokens" (or (and (hash-table-p usage)
                                          (%turn-trace-real
                                           (gethash "total_tokens" usage))) :null)
                 "cost_usd" (or (and (hash-table-p usage)
                                      (%turn-trace-real (gethash "cost" usage))) :null))
            attempts)))))
    (coerce (nreverse attempts) 'vector)))

(defun %turn-trace-sum (rows key)
  (loop for row across rows for value = (gethash key row)
        when (numberp value) sum value))

(defun %turn-trace-span-total (spans predicate)
  (loop for span across spans when (funcall predicate span)
        sum (gethash "duration_ms" span 0)))

(defun %turn-trace-tool-counts (spans)
  (let ((counts (obj)))
    (loop for span across spans
          for name = (gethash "name" span "")
          when (string= "tool." name :end2 (min 5 (length name)))
            do (incf (gethash (subseq name 5) counts 0)))
    counts))

(defun %turn-trace-repeated-tools (counts)
  (let ((rows nil))
    (maphash (lambda (name count)
               (when (> count 1)
                 (push (obj "tool" name "count" count) rows)))
             counts)
    (coerce (sort rows #'> :key (lambda (row) (gethash "count" row))) 'vector)))

(defun turn-trace-project (events &key turn-id trace-id)
  (unless (or (turn-trace-safe-id-p turn-id) (turn-trace-safe-id-p trace-id))
    (error "A safe turn_id or trace_id is required."))
  (let* ((event-list (%turn-trace-list events))
         (trace-events
           (remove-if-not
            (lambda (event)
              (and (hash-table-p event)
                   (string= "timing-trace" (or (gethash "type" event) ""))
                   (%turn-trace-matching-p (%turn-trace-event-payload event)
                                           turn-id trace-id)))
            event-list))
         (*turn-trace-projection-diagnostics* nil))
    (unless (= 1 (length trace-events))
      (error "Expected exactly one matching timing trace; found ~d."
             (length trace-events)))
    (let* ((trace (%turn-trace-event-payload (first trace-events)))
           (resolved-turn (%turn-trace-string (gethash "turn_id" trace)))
           (resolved-trace (%turn-trace-string (gethash "trace_id" trace)))
           (duration (%turn-trace-real (gethash "duration_ms" trace) 0))
           (spans-list
             (remove nil
                     (mapcar (lambda (span)
                               (%turn-trace-safe-span span duration))
                             (%turn-trace-list (gethash "spans" trace)))))
           (spans
             (coerce (stable-sort spans-list #'<
                                  :key (lambda (span)
                                         (gethash "start_offset_ms" span 0)))
                     'vector))
           (attempts
             (if resolved-turn
                 (%turn-trace-provider-attempts event-list resolved-turn)
                 (progn (push "trace-missing-turn-id"
                              *turn-trace-projection-diagnostics*) #())))
           (tool-counts (%turn-trace-tool-counts spans))
           (public-spans
             (remove-if-not
              (lambda (span) (string= "model.public_request"
                                      (gethash "name" span "")))
              (coerce spans 'list)))
           (prompt-values
             (remove-if-not #'numberp
                            (mapcar (lambda (span)
                                      (gethash "prompt_tokens"
                                               (gethash "attributes" span)))
                                    public-spans)))
           (message-values
             (remove-if-not #'numberp
                            (mapcar (lambda (span)
                                      (gethash "message_count"
                                               (gethash "attributes" span)))
                                    public-spans)))
           (first-output
             (find "turn.first_public_output" spans
                   :key (lambda (span) (gethash "name" span))
                   :test #'string=))
           (attempt-cost (%turn-trace-sum attempts "cost_usd"))
           (attempt-tokens (%turn-trace-sum attempts "total_tokens")))
      (when (zerop (length attempts))
        (push "legacy-unlinked-physical-attempts"
              *turn-trace-projection-diagnostics*))
      (obj
       "schema_version" *turn-trace-projection-schema-version*
       "status" "projected"
       "trace_id" (or resolved-trace :null)
       "turn_id" (or resolved-turn :null)
       "origin" (%turn-trace-string (gethash "origin" trace) "unknown")
       "trace_status" (%turn-trace-string (gethash "status" trace) "unknown")
       "duration_ms" duration
       "first_public_output_ms" (or (and first-output
                                          (gethash "start_offset_ms" first-output))
                                     :null)
       "spans" spans
       "provider_attempts" attempts
       "physical_attempts_status" (if (plusp (length attempts)) "exact" "legacy-unlinked")
       "totals"
       (obj "model_request_duration_ms"
            (%turn-trace-span-total
             spans (lambda (span)
                     (let ((name (gethash "name" span "")))
                       (and (search "model." name) (search "_request" name)))))
            "tool_duration_ms"
            (%turn-trace-span-total
             spans (lambda (span) (search "tool." (gethash "name" span ""))))
            "memory_search_duration_ms"
            (%turn-trace-span-total
             spans (lambda (span) (string= "memory.search"
                                           (gethash "name" span ""))))
            "context_duration_ms"
            (%turn-trace-span-total
             spans (lambda (span) (string= "context.projection"
                                           (gethash "name" span ""))))
            "provider_attempt_cost_usd" attempt-cost
            "provider_attempt_total_tokens" attempt-tokens)
       "growth"
       (obj "public_model_call_count" (length public-spans)
            "first_prompt_tokens" (or (first prompt-values) :null)
            "last_prompt_tokens" (or (car (last prompt-values)) :null)
            "maximum_prompt_tokens" (or (and prompt-values
                                                  (apply #'max prompt-values)) :null)
            "first_message_count" (or (first message-values) :null)
            "last_message_count" (or (car (last message-values)) :null))
       "tool_counts" tool-counts
       "repeated_tools" (%turn-trace-repeated-tools tool-counts)
       "diagnostics" (coerce (remove-duplicates
                               (nreverse *turn-trace-projection-diagnostics*)
                                                  :test #'string=)
                               'vector)
       "private_content_included" nil))))

(defun turn-trace-fixture-index ()
  (vector (obj "id" "ordinary" "label" "Ordinary single-call turn")
          (obj "id" "pathological-memory-loop"
               "label" "Repeated memory-tool turn")))

(defun turn-trace-fixture (fixture-id)
  (unless (member fixture-id '("ordinary" "pathological-memory-loop")
                  :test #'string=)
    (error "Unknown turn-trace fixture."))
  (let* ((path (merge-pathnames (format nil "~a.json" fixture-id)
                                *turn-trace-fixture-directory*))
         (events (shasht:read-json (uiop:read-file-string path))))
    (turn-trace-project events :turn-id
                        (if (string= fixture-id "ordinary")
                            "turn-fixture-ordinary"
                            "turn-fixture-pathological"))))
