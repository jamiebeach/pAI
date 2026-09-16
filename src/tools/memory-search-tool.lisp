;;;; memory-search-tool.lisp -- bounded read-only conscious memory access.

(in-package :agent)

(export '(search-memory memory-search-tool-report memory-search-tool-handle))

(defparameter *search-memory-max-query-chars* 1000)
(defparameter *search-memory-max-results* 5)
(defparameter *search-memory-result-character-budget* 6000)
(defvar *search-memory-tool-searches* 0)
(defvar *search-memory-tool-errors* 0)
(defvar *search-memory-recursive-ordinary-reply-authorized-p* nil
  "Dynamically true only while the recursive runtime executes a native tool
for an admitted operator-conversation root.")
(defvar *search-memory-recursive-private-cognition-authorized-p* nil
  "Dynamically true only while the recursive runtime executes a native tool
for a private cognitive root owned by the same configured mind.")
(defvar *search-memory-conversation-events* nil
  "Dynamically bound read-only event generation for recursive recall.")
(defvar *search-memory-conversation-agent-id* nil)
(defvar *search-memory-conversation-persona-id* nil)

(defun %search-memory-as-of ()
  "Return the code-owned turn clock as canonical UTC for retrieval and output."
  (let* ((context (and (boundp '*turn-capture-context*)
                       (symbol-value '*turn-capture-context*)))
         (value (or (and (hash-table-p context)
                         (gethash "as_of" context))
                    (get-universal-time))))
    (multiple-value-bind (second minute hour day month year)
        (decode-universal-time value 0)
      (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
              year month day hour minute second))))

(defun %search-memory-sensitive-content-p (text)
  (let ((value (string-downcase (if (stringp text) text ""))))
    (or (search "public-system-prompt:begin" value)
        (search "# pai — system prompt" value)
        (search "# operational constitution" value)
        (search "openrouter_api_key" value)
        (search "telegram_bot_token" value)
        (search "brave_api_key" value))))

(defun %search-memory-runtime-authorized-p ()
  (let ((capture (and (boundp '*turn-capture-context*)
                      (symbol-value '*turn-capture-context*)))
        (contract (and (boundp '*publication-contract-current*)
                       (symbol-value '*publication-contract-current*))))
    (or *search-memory-recursive-ordinary-reply-authorized-p*
        *search-memory-recursive-private-cognition-authorized-p*
        (and (hash-table-p capture)
             (string= "conversation" (gethash "origin" capture ""))
             (hash-table-p contract)
             (string= "ordinary-reply"
                      (gethash "interaction_mode" contract ""))))))

(defun %search-memory-json (value)
  (with-output-to-string (stream)
    (shasht:write-json value stream)))

(defun %search-memory-bounded-json (value)
  "Enforce the complete serialized tool-result budget, not just row content."
  (let ((refused 0))
    ;; Keep measurement fields present while measuring and iterate to a stable
    ;; digit width.  The reported value is therefore the exact final JSON size,
    ;; not the pre-wrapper size from one serialization earlier.
    (setf (gethash "result_characters" value) 0
          (gethash "serialization_refusal_count" value) 0)
    (loop
      for rendered = (%search-memory-json value)
      for measured = (length rendered)
      do (setf (gethash "serialization_refusal_count" value) refused)
         (cond
           ((/= measured (gethash "result_characters" value))
            (setf (gethash "result_characters" value) measured))
           ((<= measured *search-memory-result-character-budget*)
            (return rendered))
           (t
            (let ((semantic (gethash "results" value #()))
               (conversation (gethash "conversation_results" value #())))
              (cond ((plusp (length semantic))
                     (setf (gethash "results" value)
                           (subseq semantic 0 (1- (length semantic)))
                           (gethash "semantic_result_count" value)
                           (1- (gethash "semantic_result_count" value))))
                    ((plusp (length conversation))
                     (setf (gethash "conversation_results" value)
                           (subseq conversation 0 (1- (length conversation)))
                           (gethash "conversation_result_count" value)
                           (1- (gethash "conversation_result_count" value))))
                    (t (error "search-memory metadata exceeds its result budget")))
              (incf refused)
              (decf (gethash "selected_count" value))
              (let ((report (gethash "selection_report" value)))
                (when (hash-table-p report)
                  (setf (gethash "selected_count" report)
                        (gethash "selected_count" value)
                        (gethash "non_exhaustive" report) t)))))))))

(defun %search-memory-current-exclusions ()
  "Return content-free stable turn/event identities for current-turn anti-echo."
  (let* ((context (and (boundp '*turn-capture-context*)
                       (symbol-value '*turn-capture-context*)))
         (turn-id (and (hash-table-p context) (gethash "turn_id" context)))
         (user-event (and (hash-table-p context)
                          (gethash "user_event_id" context)))
         (entries (and (hash-table-p context) (gethash "entries" context)))
         (entry-events
           (loop for entry in (cond ((listp entries) entries)
                                    ((vectorp entries) (coerce entries 'list))
                                    (t nil))
                 for event-id = (and (hash-table-p entry)
                                     (gethash "event_id" entry))
                 when (and (stringp event-id) (plusp (length event-id)))
                   collect event-id)))
    (values
     (if (and (stringp turn-id) (plusp (length turn-id)))
         (list turn-id) nil)
     (remove-duplicates
      (append (if (and (stringp user-event) (plusp (length user-event)))
                  (list user-event) nil)
              entry-events)
      :test #'string=))))

(defun %search-memory-conversation-evidence (query limit)
  "Discover sealed and unsealed evidence independently of the output limit."
  (declare (ignore limit))
  (if (and *search-memory-conversation-events*
           (stringp *search-memory-conversation-agent-id*)
           (stringp *search-memory-conversation-persona-id*)
           (fboundp 'conversation-unsealed-dialogue-context-records)
           (fboundp 'conversation-episode-project)
           (fboundp 'conversation-episode-context-records))
      (multiple-value-bind (raw-records raw-ids coverage)
          (conversation-unsealed-dialogue-context-records
           *search-memory-conversation-events*
           *search-memory-conversation-agent-id*
           *search-memory-conversation-persona-id* query
           :maximum 8 :character-budget *search-memory-result-character-budget*
           :record-character-limit 1800)
        (multiple-value-bind (sealed-records sealed-ids sealed-report)
            (conversation-episode-context-records
             (conversation-episode-project
              *search-memory-conversation-events*
              *search-memory-conversation-agent-id*
              *search-memory-conversation-persona-id*)
             query :maximum 12
             :character-budget *search-memory-result-character-budget*
             :record-character-limit 1800)
          (setf (gethash "selected_count" coverage)
                (+ (length raw-records) (length sealed-records))
                (gethash "selected_ids" coverage)
                (coerce (append raw-ids sealed-ids) 'vector)
                (gethash "raw_candidate_count" coverage) (length raw-records)
                (gethash "sealed_candidate_count" coverage)
                (length sealed-records)
                (gethash "rendered_characters" coverage)
                (+ (gethash "rendered_characters" coverage 0)
                   (gethash "rendered_characters" sealed-report 0)))
          (values raw-records raw-ids sealed-records sealed-ids coverage)))
      (values (vector) nil (vector) nil
              (obj "schema_version" 1 "status" "unavailable"
                   "selected_count" 0 "selected_ids" (vector)
                   "database_write_count" 0))))

(defun %search-memory-operator-support-p (row)
  (or (string= "lived-user" (gethash "origin_class" row ""))
      (member "user" (coerce (gethash "member_roles" row #()) 'list)
              :test #'string=)))

(defun %search-memory-candidates (plan source-kind records &key semantic-p)
  (loop for row in (if (vectorp records) (coerce records 'list) records)
        for rank from 1
        collect
        (recall-selection-candidate
         plan source-kind row
         :candidate-id (or (gethash "source_id" row) (gethash "id" row))
         :support-key (or (gethash "source_id" row) (gethash "id" row))
         :local-rank rank
         :semantic-rank (and semantic-p rank)
         :lexical-rank (and (plusp (gethash "lexical_tier" row 0)) rank)
         :operator-support-p
         (if semantic-p (%search-memory-operator-support-p row) t)
         :speaker-basis
         (cond ((string= source-kind "raw-dialogue") "operator-dialogue")
               ((string= source-kind "sealed-episode") "generated-episode-synopsis")
               (t (gethash "origin_class" row "unknown")))
         ;; Rendered dialogue pairs and episode synopses can contain a repeated
         ;; question.  Without an exact typed operator assertion span they are
         ;; supplemental category evidence, never direct answer evidence.
         :maximum-relevance-class
         (and (gethash "operator_fact_query" plan)
              (member source-kind '("raw-dialogue" "sealed-episode")
                      :test #'string=)
              2)
         :observed-at (or (gethash "observed_at" row)
                          (gethash "created_at" row))
         :supported-match-p
         (or (not semantic-p)
             (plusp (gethash "lexical_tier" row 0))
             (>= (gethash "similarity" row 0.0d0)
                 *context-projection-memory-similarity-floor*)))))

(defun %search-memory-result-row (row)
  (obj "id" (gethash "id" row)
       "kind" (gethash "kind" row)
       "content" (gethash "content" row)
       "origin_class" (gethash "origin_class" row)
       "epistemic_status" (gethash "epistemic_status" row)
       "grounding_status" (gethash "grounding_status" row)
       "observed_at" (or (gethash "observed_at" row)
                           (gethash "created_at" row) :null)
       "valid_from" (gethash "valid_from" row :null)
       "valid_to" (gethash "valid_to" row :null)
       "supersedes_node_id" (gethash "supersedes_node_id" row :null)
       "turn_id" (gethash "turn_id" row :null)
       "anchor_id" (gethash "anchor_id" row :null)
       "member_roles" (gethash "member_roles" row (vector))
       "evidence_node_ids" (gethash "evidence_node_ids" row (vector))
       "candidate_sources" (gethash "candidate_sources" row (vector))
       "lexical_tier" (gethash "lexical_tier" row 0)
       "lexical_match_count" (gethash "lexical_match_count" row 0)
       "lexical_coverage" (gethash "lexical_coverage" row 0.0d0)
       "lexical_terms" (gethash "lexical_terms" row (vector))
       "similarity" (or (gethash "similarity" row) :null)
       "retrieval_score" (or (gethash "retrieval_score" row) :null)))

(defun search-memory (query &key (limit 3))
  "Return bounded typed evidence without rehearsal or any durable write."
  (unless (and (stringp query) (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) query))))
    (error "search-memory requires a non-empty query"))
  (unless (<= (length query) *search-memory-max-query-chars*)
    (error "search-memory query exceeds ~d characters"
           *search-memory-max-query-chars*))
  (unless (and (integerp limit) (<= 1 limit *search-memory-max-results*))
    (error "search-memory limit must be from 1 through ~d"
           *search-memory-max-results*))
  (unless (%search-memory-runtime-authorized-p)
    (error "search-memory is available only inside an authorized recursive cognition boundary"))
  (incf *search-memory-tool-searches*)
  (multiple-value-bind (excluded-turns excluded-events)
      (%search-memory-current-exclusions)
    (multiple-value-bind (raw-records raw-ids sealed-records sealed-ids
                          conversation-report)
        (%search-memory-conversation-evidence query limit)
      (declare (ignore raw-ids sealed-ids))
      (let* ((as-of (%search-memory-as-of))
             (rows nil) (retrieval-report nil) (retrieval-error nil))
        (handler-case
            (multiple-value-setq (rows retrieval-report)
              (memory-search query
                             :k *context-projection-memory-candidate-results*
                             :mode :conversation :as-of as-of
                             :candidate-strategy :hybrid-explicit
                             :exclude-turn-ids excluded-turns
                             :exclude-source-event-ids excluded-events))
          (error (condition)
            (setf retrieval-error condition
                  rows nil
                  retrieval-report (obj))))
        (when (and retrieval-error (zerop (+ (length raw-records)
                                              (length sealed-records))))
          (error retrieval-error))
      (let* ((eligible
               (remove-if-not #'%context-projection-eligible-memory-p rows))
             (content-safe
               (remove-if
                (lambda (row)
                  (%search-memory-sensitive-content-p
                   (gethash "content" row "")))
                eligible))
             ;; Explicit recall must preserve speaker grounding. Isolated
             ;; assistant replies are evidence that the agent spoke, not authority
             ;; for the operator's claim. Reuse the qualified same-turn bundler.
             (bundle-result
               (if (fboundp '%context-projection-turn-bundles)
                   (multiple-value-list
                    (%context-projection-turn-bundles query content-safe))
                   (list nil 0 0 0 :atomic-fallback)))
             (semantic-evidence
               (if (eq (fifth bundle-result) :bundled)
                   (first bundle-result)
                   (remove-if-not #'%context-projection-direct-memory-p
                                  content-safe)))
             (plan (build-recall-query-plan
                    query
                    :agent-id *search-memory-conversation-agent-id*
                    :persona-id *search-memory-conversation-persona-id*
                    :as-of as-of))
             (candidates
               (append (%search-memory-candidates plan "raw-dialogue" raw-records)
                       (%search-memory-candidates plan "sealed-episode"
                                                  sealed-records)
                       (%search-memory-candidates plan "semantic-bundle"
                                                  semantic-evidence
                                                  :semantic-p t)))
             (selection-result
               (multiple-value-list
                (recall-selection-select
                 candidates limit *search-memory-result-character-budget*)))
             (selected (first selection-result))
             (selection-report (second selection-result))
             (selected-conversation
               (remove-if (lambda (candidate)
                            (string= "semantic-bundle"
                                     (gethash "source_kind" candidate)))
                          selected))
             (selected-semantic
               (remove-if-not (lambda (candidate)
                                (string= "semantic-bundle"
                                         (gethash "source_kind" candidate)))
                              selected))
             (conversation-records
               (coerce (mapcar (lambda (candidate)
                                 (gethash "record" candidate))
                               selected-conversation)
                       'vector))
             (bounded
               (mapcar (lambda (candidate) (gethash "record" candidate))
                       selected-semantic)))
          (%search-memory-bounded-json
         (obj "schema_version" 2
              "status" (if selected
                           "available" "empty")
              "as_of" as-of
              "query_characters" (length query)
              "candidate_strategy" "hybrid-explicit"
              "candidate_count" (length rows)
              "semantic_candidate_count"
              (gethash "semantic_candidate_count" retrieval-report 0)
              "lexical_candidate_count"
              (gethash "lexical_candidate_count" retrieval-report 0)
              "union_candidate_count"
              (gethash "union_candidate_count" retrieval-report 0)
              "lexical_terms"
              (gethash "lexical_terms" retrieval-report (vector))
              "eligible_count" (length eligible)
              "sensitive_filtered_count" (- (length eligible)
                                             (length content-safe))
              "identity_exclusions"
              (obj "turn_ids" (coerce excluded-turns 'vector)
                   "source_event_ids" (coerce excluded-events 'vector))
              "selected_count" (length selected)
              "semantic_result_count" (length bounded)
              "database_write_count" 0
              "semantic_retrieval_status"
              (if retrieval-error "unavailable" "available")
              "conversation_coverage" conversation-report
              "conversation_result_count" (length conversation-records)
              "conversation_results" conversation-records
              "selection_report" selection-report
              "results"
              (coerce
               (mapcar #'%search-memory-result-row bounded)
               'vector))))))))

(defun memory-search-tool-report ()
  (obj "schema_version" 1
       "read_only" t
       "max_query_characters" *search-memory-max-query-chars*
       "max_results" *search-memory-max-results*
       "searches" *search-memory-tool-searches*
       "errors" *search-memory-tool-errors*
       "delivery_authority" nil))

(let ((tool
        (obj "type" "function" "function"
             (obj "name" "search-memory"
                  "description" "Search grounded personal history and persona-scoped conversation evidence before claiming an earlier interaction is unavailable. Use a concise content-specific query; this historical tool does not report current runtime work or private cognition. Results include sealed coverage and may include exact recent unsealed dialogue when episodic indexing lags. Absence is non-exhaustive; null validity means current validity is unknown. Read-only: it cannot message anyone or change memory."
                  "parameters"
                  (obj "type" "object"
                       "properties"
                       (obj "query" (obj "type" "string"
                                         "description" "A concise semantic description of the earlier interaction or fact to retrieve.")
                            "limit" (obj "type" "integer" "minimum" 1
                                         "maximum" 5 "default" 3))
                       "required" (vector "query"))))))
  (unless (find "search-memory" *tools*
                :key (lambda (candidate) (ref candidate "function" "name"))
                :test #'string=)
    (setf *tools* (concatenate 'vector *tools* (vector tool)))))

(defun memory-search-tool-handle (tool-call)
  "Run the incumbent SEARCH-MEMORY handler body for TOOL-CALL."
  (handler-case
      (let* ((args (shasht:read-json
                    (ref tool-call "function" "arguments")))
             (limit (gethash "limit" args 3)))
        (obj "role" "tool" "tool_call_id" (gethash "id" tool-call)
             "content" (search-memory (gethash "query" args)
                                       :limit limit)))
    (error (condition)
      (incf *search-memory-tool-errors*)
      (obj "role" "tool" "tool_call_id" (gethash "id" tool-call)
           "content" (format nil "ERROR: memory search unavailable: ~a"
                             condition)))))

(when (or (not (fboundp 'tool-dispatch-legacy-wrapper-enabled-p))
          (funcall 'tool-dispatch-legacy-wrapper-enabled-p))
  (unless (fboundp 'pai-base-execute-memory-search-tool)
    (setf (fdefinition 'pai-base-execute-memory-search-tool)
          (fdefinition 'execute)))
  (defun execute (tool-call)
    (let ((name (ref tool-call "function" "name")))
      (if (string= name "search-memory")
          (memory-search-tool-handle tool-call)
          (funcall 'pai-base-execute-memory-search-tool tool-call)))))
