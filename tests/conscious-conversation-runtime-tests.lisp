;;;; conscious-conversation-runtime-tests.lisp -- Q4.5 solicited conversation.
;;;; Failing probe written before conversation-runtime.lisp existed.

(in-package :agent)

(ql:quickload '(:shasht :ironclad) :silent t)

(defvar *q45-passed* 0)
(defvar *q45-failed* 0)
(defvar *q45-events* nil)
(defvar *q45-open-spec* nil)
(defvar *q45-provider-messages* nil)
(defvar *q45-observed-provider-request* nil)
(defvar *q45-trace-messages* nil)
(defvar *q45-trace-metadata* nil)
(defvar *q45-contract-violations* nil)
(defvar *q45-repaired-publication* nil)
(defvar *q45-manifest* nil)
(defvar *q45-submitted-captured* nil)
(defvar *q45-lifecycle-rows* nil)
(defvar *q45-before-assembly-fn* nil)
(defvar *q45-open-through-event-id* nil)
(defvar *q45-open-work-id* nil)
(defvar *q45-work-pulse-sequence* 0)
(defvar *q45-log-receipt-p* nil)
(defvar *q45-replay-count* 0)
(defvar *q45-event-lock* (bt:make-lock "q45 fixture events"))
(defvar *q45-append-lock* (bt:make-lock "q45 fixture append authority"))
(defvar *conscious-lifecycle-runtime-projection* nil)
(defvar *agent-id* "q45-dev")

(defun q45-check (name condition)
  (if condition
      (progn (incf *q45-passed*) (format t "PASS ~a~%" name))
      (progn (incf *q45-failed*) (format t "FAIL ~a~%" name))))

(defun replay-events (&rest ignored)
  (declare (ignore ignored))
  (bt:with-lock-held (*q45-event-lock*)
    (incf *q45-replay-count*)
    (copy-list *q45-events*)))
(defun log-event (type payload &key caused-by)
  (bt:with-lock-held (*q45-event-lock*)
    (let* ((id (1+ (length *q45-events*)))
           (event (obj "id" id "type" type "agent_id" *agent-id*
                       "caused_by" (or caused-by :null) "payload" payload)))
      (setf *q45-events*
            (append *q45-events* (list event)))
      (if *q45-log-receipt-p*
          (values id t event)
          id))))
(defun log-event-if (predicate type payload &key caused-by)
  (bt:with-lock-held (*q45-append-lock*)
    (when (funcall predicate)
      (multiple-value-bind (id durable receipt)
          (log-event type payload :caused-by caused-by)
        (values id durable receipt t)))))

(defun submit-stimulus (text &key kind metadata wait-for-public-result)
  (declare (ignore kind wait-for-public-result))
  (let ((id (log-event
             "user-message"
             (obj "text" text "channel" "terminal"
                  "metadata" metadata))))
    (values nil :accepted id)))
(defun cognition-runtime-selected-p (name) (eq name :conscious-state))
(defun conscious-lifecycle-awaiting (projection)
  (declare (ignore projection))
  *q45-lifecycle-rows*)
(defun conscious-lifecycle-context-records (awaiting) awaiting)
(defun conscious-cognition-runtime-open-captured
    (&key assembly-spec assembly-spec-fn assembly-spec-events-fn through-event-id
          work-id &allow-other-keys)
  (setf *q45-open-through-event-id* through-event-id
        *q45-open-work-id* work-id)
  (when *q45-before-assembly-fn* (funcall *q45-before-assembly-fn*))
  (setf *q45-open-spec* (or assembly-spec
                            (and assembly-spec-fn
                                 (funcall assembly-spec-fn))
                            (and assembly-spec-events-fn
                                 (funcall
                                  assembly-spec-events-fn
                                  (if through-event-id
                                      (remove-if
                                       (lambda (event)
                                         (> (gethash "id" event 0)
                                            through-event-id))
                                       *q45-events*)
                                      *q45-events*)))))
  (let ((manifest
          (obj "pulse_id" (if work-id
                               (format nil "pulse:work:~d"
                                       (1+ *q45-work-pulse-sequence*))
                               "pulse:10")
               "runtime_revision" "conscious-q4-v1"
               "conscious_state_revision" 3 "audience" "operator"
               "evidence_event_ids" (gethash "eligible_evidence_ids" *q45-open-spec*)
               "sections"
               (let* ((sections (gethash "sections" *q45-open-spec*))
                      (records
                        (and (hash-table-p sections)
                             (gethash "untrusted-tool-results" sections))))
                 (vector
                  (obj "name" "untrusted-tool-results"
                       "included_source_ids"
                       (map 'vector
                            (lambda (record) (gethash "source_id" record))
                            (or records (vector))))))
               "permitted_proposal_kinds"
               (gethash "permitted_proposal_kinds" *q45-open-spec*)
               "available_tools" (gethash "available_tools" *q45-open-spec*)
               "remaining_budget" (gethash "remaining_budget" *q45-open-spec*)
               "composition_hash" "fixture-composition")))
    (setf *q45-manifest* manifest)
    (let* ((sections (gethash "sections" *q45-open-spec*))
           (identity (and sections
                          (gethash "identity-instructions" sections)))
           (tool-results (and work-id sections
                              (gethash "untrusted-tool-results" sections)))
           (governing
             (map 'list
                  (lambda (row)
                    (obj "role" "governing-instructions"
                         "section" "identity-instructions"
                         "source_id" (gethash "source_id" row)
                         "content" (gethash "content" row)))
                  (or identity (vector)))))
      (obj "private_request"
           (coerce
            (append governing
                    (map 'list
                         (lambda (row)
                           (obj "role" "untrusted-model-data"
                                "section" "untrusted-tool-results"
                                "source_id" (gethash "source_id" row)
                                "content" (gethash "content" row)))
                         (or tool-results (vector)))
                    (list (obj "role" "current-stimulus"
                               "content" "fixture private request")))
            'vector)
         "manifest" manifest))))
(defun conscious-cognition-runtime-submit-captured (captured &key model-calls)
  ;; Match the production captured runtime's strict Q4 boundary. Without this
  ;; the conversation subject cannot prove its commit-rejection diagnostics.
  (conscious-proposals-validate captured *q45-manifest*)
  (setf *q45-submitted-captured* captured)
  (when *q45-open-work-id*
    (incf *q45-work-pulse-sequence*)
    (log-event "pulse-committed"
               (obj "work_id" *q45-open-work-id*
                    "pulse_id" (gethash "pulse_id" *q45-manifest*)
                    "pulse_sequence" *q45-work-pulse-sequence*
                    "context_manifest" *q45-manifest*
                    "proposals" (gethash "proposals" captured))))
  (obj "status" "completed" "model_calls" model-calls
       "proposals" (gethash "proposals" captured)))
(defun conscious-pulse-runtime-fail-captured (reason)
  (log-event "pulse-failed"
             (obj "work_id" (or *q45-open-work-id* :null)
                  "pulse_id" (if *q45-manifest*
                                  (gethash "pulse_id" *q45-manifest*)
                                  :null)
                  "terminal_reason" reason)))
(defun build-publication-contract (&rest ignored)
  (declare (ignore ignored)) (obj "fixture" t))
(defun publication-contract-violations (&rest ignored)
  (declare (ignore ignored)) (coerce *q45-contract-violations* 'vector))
(defun publication-contract-removal-only-draft (&rest ignored)
  (declare (ignore ignored)) *q45-repaired-publication*)

(defun q45-publication-response (content)
  (obj "choices"
       (vector (obj "message"
                    (obj "role" "assistant" "content" content)))
       "usage" (obj "prompt_tokens" 321 "completion_tokens" 45
                    "total_tokens" 366)))

(defun q45-reset ()
  (setf *q45-events* nil *q45-open-spec* nil *q45-provider-messages* nil
        *q45-trace-messages* nil *q45-trace-metadata* nil
        *q45-contract-violations* nil *q45-repaired-publication* nil
        *q45-manifest* nil *q45-submitted-captured* nil
        *q45-lifecycle-rows* nil *q45-before-assembly-fn* nil
        *q45-open-work-id* nil *q45-work-pulse-sequence* 0
        *q45-log-receipt-p* nil *q45-replay-count* 0))

(format t "~%== Q4.5 conversation runtime subject ==~%")
(load (test-source "proposal.lisp"))
(load (test-source "conscious-file-search-tool.lisp"))
(load (test-source "cognitive-work.lisp"))
(load (test-source "cognitive-work-runtime.lisp"))
(load (test-source "tool-operation-runtime.lisp"))
(load (test-source "cognitive-work-context.lisp"))
(load (test-source "context-assembly.lisp"))
(load (test-source "user-time.lisp"))
(load (test-source "recall-selection.lisp"))
(unless (fboundp '%event-parse-ts-string)
  (defun %event-parse-ts-string (value)
    ;; The production parser belongs to the event adapter.  This subject test
    ;; supplies its pure timestamp boundary without loading storage.
    (and (stringp value) (plusp (length value))
         (encode-universal-time 0 23 17 29 8 2026 0))))
(load (merge-pathnames "src/mind/conscious/conversation-runtime.lisp" *pai-root*))

(q45-reset)
(setf *q45-log-receipt-p* t)
(multiple-value-bind (id stored)
    (%conversation-append-readable "receipt-probe" (obj "value" 1))
  (q45-check "durable append receipt avoids a complete ledger replay"
             (and (= 1 id) (hash-table-p stored)
                  (zerop *q45-replay-count*))))

(conscious-conversation-set-persona-profile
 "fixture-a" 3 "Fixture identity: thoughtful cartographer."
 "Fixture voice: warm, concise, and curious." :source "test-fixture")

(defun q45-budget-profile ()
  (obj "max_input_characters" 7001
       "history_max_events" 11
       "history_character_budget" 5001
       "history_event_character_limit" 3001
       "history_target_estimated_tokens" 1250
       "history_min_recent_events" 6
       "total_character_budget" 23001
       "section_character_budgets"
       (obj "identity-instructions" 2901 "sensorium" 901
            "focus-lifecycles" 901 "triggering-stimuli" 7001
            "conversation-evidence" 11001 "memory-bundles" 0
            "untrusted-tool-results" 0
            "tools-proposal-schema" 3901
            "publication-constraints" 1901)))

(setf *conscious-conversation-budget-profile* (q45-budget-profile))

(let* ((document
         (shasht:read-json
          (uiop:read-file-string
           (merge-pathnames "config/conscious-context-profiles.json"
                            *pai-root*))))
       (profile (gethash "solicited-conversation-dev"
                         (gethash "profiles" document)))
       (sections (gethash "section_character_budgets" profile))
       (work-document
         (shasht:read-json
          (uiop:read-file-string
           (merge-pathnames "config/conscious-work-profiles.json"
                            *pai-root*))))
       (work-profile (gethash "interactive-dev"
                              (gethash "profiles" work-document))))
  (q45-check "production conversation profile can carry bounded tool results"
             (>= (gethash "untrusted-tool-results" sections 0)
                 (+ (* (gethash "tool_result_wrapper_characters_per_record"
                                profile)
                       (gethash "max_tool_operations" work-profile))
                    (gethash "max_tool_result_characters" work-profile)))))

(q45-check "history assembler exists" (fboundp 'conscious-conversation-history))
(q45-check "solicited turn entry exists" (fboundp 'conscious-conversation-turn))
(q45-check "conversation declares an explicit memory projection seam"
           (boundp '*conscious-conversation-memory-projection-fn*))

;; Provenance is admitted with its owning record, rather than overloading the
;; one-record/one-selection-ID interface.  This covers the intermediate seam
;; that a producer-only and assembler-only test cannot exercise.
(let* ((source-id "conversation-raw:30:31")
       (record
         (obj "source_id" source-id
              "content" "operator: campfire assistant: enjoy the fire"
              "provenance"
              (obj "descriptor_id" source-id
                   "descriptor_event_id" 31
                   "evidence_event_ids" #(30 31))))
       (merged
         (multiple-value-list
          (%conversation-merge-memory-records
           (vector record) (list source-id) (vector) nil 1000)))
       (records (first merged))
       (selection-ids (second merged))
       (admitted-ids (sixth merged)))
  (q45-check "memory merger preserves one selection identity per record"
             (and (= 1 (length records))
                  (equal (list source-id) selection-ids)))
  (q45-check "admitted record carries exact provenance into eligibility"
             (and (member source-id admitted-ids :test #'equal)
                  (member 30 admitted-ids :test #'equal)
                  (member 31 admitted-ids :test #'equal)))
  (q45-check "budget-rejected record cannot authorize provenance evidence"
             (let ((rejected
                     (multiple-value-list
                      (%conversation-merge-memory-records
                       (vector record) (list source-id) (vector) nil 1))))
               (and (zerop (length (first rejected)))
                    (null (sixth rejected))))))

(when (boundp '*conscious-conversation-memory-projection-fn*)
  (let* ((profile (q45-budget-profile))
         (sections (gethash "section_character_budgets" profile))
         (memory-symbol '*conscious-conversation-memory-projection-fn*))
    (setf (gethash "memory-bundles" sections) 1400
          (gethash "memory_max_results" profile) 2
          (gethash "memory_record_character_limit" profile) 600
          (gethash "memory_provider_classes" profile) (vector "local"))
    (progv
        (list memory-symbol)
        (list
         (lambda (prompt)
           (declare (ignore prompt))
           (obj
            "relevant_shared_memory"
            (vector
             (obj "id" "memory-fixture-1" "kind" "turn-bundle"
                  "content" "The operator previously chose cobalt as the test word."
                  "origin_class" "derived-lived"
                  "epistemic_status" "grounded-turn-bundle"
                  "grounding_status" "grounded"
                  "label" "Grounded conversation exchange")
             (obj "id" "memory-unsafe-2" "kind" "thought"
                  "content" "Unclassified private speculation."
                  "origin_class" "legacy-unclassified"
                  "epistemic_status" "legacy-unclassified"
                  "grounding_status" "unclassified"
                  "label" "unknown"))
            "memory_retrieval"
            (obj "candidate_count" 7 "eligible_count" 5
                 "selected_count" 2 "database_write_count" 0))))
      (let* ((spec (%conversation-assembly-spec
                    nil 41 "What was the test word?" *agent-id* profile
                    "local" "terminal"))
             (memory (gethash "memory-bundles" (gethash "sections" spec)))
             (sensorium (gethash "sensorium" (gethash "sections" spec)))
             (rendered (shasht:write-json memory nil)))
        (q45-check "selected shared memory enters the bounded memory section"
                   (and (= 1 (length memory))
                        (search "cobalt" rendered)
                        (search "not automatically the selected persona's firsthand experience"
                                rendered)))
        (q45-check "memory source identity is eligible context evidence"
                   (find "memory:memory-fixture-1"
                         (coerce (gethash "eligible_evidence_ids" spec) 'list)
                         :test #'string=))
        (q45-check "provider-egress consumer refuses unsafe projected memory"
                   (null (search "Unclassified private speculation" rendered)))
        (q45-check "sensorium truthfully reports semantic memory availability"
                   (search "semantic memory retrieval selected 1"
                           (shasht:write-json sensorium nil)))))))

;; The same bounded private-state projection is present on every ordinary
;; conversation assembly. No prompt-text branch or inspect command activates
;; it, and the exact source event remains eligible evidence.
(let* ((symbol 'conscious-recursive-private-cognition-context-records)
       (had-function (fboundp symbol))
       (original (and had-function (symbol-function symbol)))
       (observed-request nil)
       (ordinary-events
         (list (obj "id" 41 "type" "user-message" "agent_id" *agent-id*
                    "timestamp" 3998300000
                    "payload" (obj "text" "Discuss an unrelated ordinary topic.")))))
  (unwind-protect
       (progn
         (setf (symbol-function symbol)
               (lambda (&rest arguments &key &allow-other-keys)
                 (setf observed-request arguments)
                 (vector
                  (obj "source_id" 314
                       "content"
                       "Current private focus (still under investigation): fixture question"))))
         (let* ((spec (%conversation-assembly-spec
                       ordinary-events 41 "Discuss an unrelated ordinary topic."
                       *agent-id* (q45-budget-profile) "local" "terminal"))
                (focus (gethash "focus-lifecycles"
                                (gethash "sections" spec))))
           (q45-check "ordinary assembly supplies the explicit continuity boundary"
                      (and (equal *agent-id*
                                  (getf observed-request :mind-identity-id))
                           (eq ordinary-events (getf observed-request :events))
                           (= 3998300000 (getf observed-request :as-of))
                           (= 41 (getf observed-request :boundary-source-id))
                           (string= "operator-conversation"
                                    (getf observed-request :boundary-kind))))
           (q45-check "ordinary assembly includes bounded private cognition"
                      (search "fixture question"
                              (shasht:write-json focus nil)))
           (q45-check "private cognition source remains exact eligible evidence"
                      (find 314
                            (coerce (gethash "eligible_evidence_ids" spec)
                                    'list)
                            :test #'equal))))
    (if had-function
        (setf (symbol-function symbol) original)
        (fmakunbound symbol))))

(let* ((profile (q45-budget-profile))
       (sections (gethash "section_character_budgets" profile))
       (work-profile
         (obj "profile_id" "conversation-fixture" "revision" 2
              "max_model_calls" 8 "max_tool_operations" 6
              "max_reasoning_continuations" 0
              "max_tool_result_characters" 12000
              "permitted_proposal_kinds"
              (vector "tool-call-proposal" "publication-candidate")
              "permitted_tools" (vector "search-files")
              "budget_exhaustion" "suspend"
              "renewal_policy" "explicit-only"))
       (arguments (obj "query" "needle" "path" "." "max_results" 3))
       (normalized (%conscious-tool-operation-normalized-arguments
                    "search-files" arguments))
       (arguments-hash
         (%conscious-tool-operation-hash
          (%conscious-tool-operation-canonical-json normalized)))
       (tool-result
         (obj "schema_version" 1 "status" "ok"
              "matches" (vector (obj "path" "notes.txt" "line" 4
                                      "text" "needle here"))
              "database_write_count" 0))
       (canonical-result
         (%conscious-tool-operation-canonical-json tool-result))
       (proposal
         (obj "proposal_id" "pulse:1:proposal:1" "pulse_id" "pulse:1"
              "runtime_revision" "fixture" "conscious_state_revision" 2
              "kind" "tool-call-proposal"
              "created_at_stage" "model-deliberation" "confidence" 0.9d0
              "evidence_event_ids" (vector 41)
              "payload" (obj "tool_name" "search-files"
                             "arguments" arguments)))
       (work-events
         (list
          (obj "id" 41 "type" "user-message" "agent_id" *agent-id*
               "payload" (obj "text" "Find the needle"))
          (obj "id" 42 "type" "conscious-work-opened" "agent_id" *agent-id*
               "payload"
               (obj "schema_version" 1 "work_id" "work:fixture"
                    "concern_identity" "operator:fixture"
                    "stimulus_ids" (vector "stimulus:41") "purpose" "respond"
                    "priority_class" "direct" "urgency_class" "interactive"
                    "deadline" :null "opened_at" 42 "profile" work-profile))
          (obj "id" 43 "type" "model-request" "agent_id" *agent-id*
               "payload" (obj "work_id" "work:fixture"
                              "pulse_id" "pulse:1"))
          (obj "id" 44 "type" "pulse-committed" "agent_id" *agent-id*
               "payload" (obj "work_id" "work:fixture"
                              "pulse_id" "pulse:1" "pulse_sequence" 1
                              "proposals" (vector proposal)))
          (obj "id" 55 "type" "conscious-tool-operation-result"
               "agent_id" *agent-id*
               "payload"
               (obj "schema_version" 1
                    "operation_id" "tool-operation:pulse:1:proposal:1"
                    "proposal_id" "pulse:1:proposal:1"
                    "interaction_id" :null "work_id" "work:fixture"
                    "user_event_id" 41 "tool_name" "search-files"
                    "arguments_hash" arguments-hash "result" tool-result
                    "result_characters" (length canonical-result)
                    "result_hash"
                    (%conscious-tool-operation-hash canonical-result))))))
  (setf (gethash "untrusted-tool-results" sections) 1200)
  (let* ((*q45-events* work-events)
         ;; The captured cognitive snapshot correctly stops at the triggering
         ;; user event. Work-open and result receipts are necessarily later.
         (captured-prefix (list (first work-events)))
         (spec (%conversation-assembly-spec
                captured-prefix 41 "Find the needle" *agent-id* profile
                "local" "terminal" "work:fixture"))
         (work-context
           (%conversation-work-context-validate
            (conscious-work-context-build work-events "work:fixture" *agent-id*)))
         (work-records
           (gethash "untrusted-tool-results" (gethash "sections" spec)))
         (remaining (gethash "remaining_budget" spec)))
    (q45-check "work-scoped durable tool result enters the untrusted section"
               (and (= 1 (length work-records))
                    (= 2 (hash-table-count (aref work-records 0)))
                    (not (nth-value 1 (gethash "role" (aref work-records 0))))
                    (not (nth-value 1
                                    (gethash "section"
                                             (aref work-records 0))))
                    (search "untrusted JSON" (gethash "content"
                                                       (aref work-records 0)))))
    (let* ((assembly-context
             (make-conscious-assembly-context
              :pulse-id "pulse:continuation:fixture"
              :purpose "respond" :audience (gethash "audience" spec)
              :runtime-revision "fixture"
              :conscious-state-revision 8 :clock-identity "fixture-clock"
              :total-character-budget
              (gethash "total_character_budget" spec)
              :section-character-budgets
              (gethash "section_character_budgets" spec)
              :sections (gethash "sections" spec)
              :eligible-evidence-ids (gethash "eligible_evidence_ids" spec)
              :available-tools (gethash "available_tools" spec)
              :permitted-proposal-kinds
              (gethash "permitted_proposal_kinds" spec)
              :publication-constraints
              (gethash "publication_constraints" spec)
              :remaining-budget (gethash "remaining_budget" spec)
              :pre-render-refusals
              (gethash "pre_render_refusals" spec (vector))))
           (assembled
             (conscious-context-assemble
              (obj "state_revision" 8 "composition_hash" "fixture-state")
              assembly-context))
           (tool-message
             (find "untrusted-tool-results"
                   (gethash "private_request" assembled)
                   :key (lambda (message) (gethash "section" message ""))
                   :test #'string=))
           (direct-messages
             (%conversation-model-messages assembled nil "Find the needle")))
      (q45-check "real assembler accepts and labels verified continuation data"
                 (and tool-message
                      (string= "untrusted-model-data"
                               (gethash "role" tool-message ""))
                      (search "untrusted JSON"
                              (gethash "content" tool-message ""))))
      (q45-check "direct model request ends with the exact current stimulus"
                 (let ((last (car (last direct-messages))))
                   (and (= 3 (length direct-messages))
                        (string= "user" (gethash "role" last ""))
                        (search "Find the needle"
                                (gethash "content" last ""))
                        (search "[20" (gethash "content" last "")))))
      (q45-check "open-ended model instruction does not suppress reasoning or invent a cap"
                 (let ((system (gethash "content" (first direct-messages) "")))
                   (and (null (search "/no_think" system :test #'char-equal))
                        (null (search "within the output bound"
                                      system :test #'char-equal)))))
      (q45-check "native current stimulus must match assembled evidence"
                 (handler-case
                     (progn
                       (%conversation-model-messages assembled nil "wrong turn")
                       nil)
                   (error () t))))
    (q45-check "work profile controls advertised tools and proposal kinds"
               (and (equalp #( "search-files")
                            (gethash "available_tools" spec))
                    (find "tool-call-proposal"
                          (coerce (gethash "permitted_proposal_kinds" spec)
                                  'list)
                          :test #'string=)))
    (q45-check "work counters become exact manifest remaining authority"
               (and (= 5 (gethash "tool_proposals" remaining -1))
                    (zerop (gethash "continuations" remaining -1))))
    (q45-check "durable tool result identity becomes eligible evidence"
               (member 55 (coerce (gethash "eligible_evidence_ids" spec) 'list)))
    (let* ((manifest
             (obj "pulse_id" "pulse:work" "runtime_revision" "fixture"
                  "conscious_state_revision" 8
                  "evidence_event_ids" (gethash "eligible_evidence_ids" spec)
                  "permitted_proposal_kinds"
                  (gethash "permitted_proposal_kinds" spec)
                  "available_tools" (gethash "available_tools" spec)))
           (messages
             (%conversation-model-messages
              (obj "private_request" (vector) "manifest" manifest)
              work-context))
           (system (gethash "content" (first messages))))
      (q45-check "work-scoped model contract uses native tool semantics"
                 (and (search "tool_calls" system)
                      (null (search "REQUIRED_DECISION" system))
                      (= 4 (length messages))
                      (string= "assistant"
                               (gethash "role" (third messages) ""))
                      (string= "tool"
                               (gethash "role" (fourth messages) ""))
                      (string=
                       (gethash "id"
                                (aref (gethash "tool_calls" (third messages)) 0))
                       (gethash "tool_call_id" (fourth messages) "")))))
    (multiple-value-bind (identity-id voice-id)
        (%conversation-persona-source-ids)
      (declare (ignore identity-id))
      (let ((manifest
              (obj "pulse_id" "pulse:work" "runtime_revision" "fixture"
                   "conscious_state_revision" 8
                   "evidence_event_ids"
                   (coerce
                    (remove voice-id
                            (coerce (gethash "eligible_evidence_ids" spec)
                                    'list)
                            :test #'equal)
                    'vector)
                   "permitted_proposal_kinds"
                   (gethash "permitted_proposal_kinds" spec)
                   "available_tools" (gethash "available_tools" spec))))
        (q45-check "model boundary rejects silently omitted persona voice"
                   (handler-case
                       (progn
                         (%conversation-model-messages
                          (obj "private_request" (vector)
                               "manifest" manifest))
                         nil)
                     (error () t)))))
      (let ((variant-manifest
              (obj "pulse_id" "pulse:work" "runtime_revision" "fixture"
                   "conscious_state_revision" 8
                   "audience" "operator"
                   "evidence_event_ids" (vector 41 42)
                   "sections"
                   (vector
                    (obj "name" "untrusted-tool-results"
                         "included_source_ids" (vector 42)))
                   "permitted_proposal_kinds"
                   (gethash "permitted_proposal_kinds" spec)
                   "available_tools" (gethash "available_tools" spec)
                   "remaining_budget" (gethash "remaining_budget" spec))))
        (q45-check "native content and tool-call branches both validate"
                   (every
                    (lambda (response)
                      (handler-case
                          (progn
                            (conscious-proposals-validate
                             (%conversation-native-response-captured
                              response variant-manifest 41)
                             variant-manifest)
                            t)
                        (error () nil)))
                    (list
                     (obj "choices"
                          (vector (obj "message"
                                       (obj "role" "assistant"
                                            "content" "Natural answer."))))
                     (obj "choices"
                          (vector
                           (obj "message"
                                (obj
                                 "role" "assistant" "content" :null
                                 "tool_calls"
                                 (vector
                                  (obj
                                   "id" "provider-call-is-not-authority"
                                   "type" "function"
                                   "function"
                                   (obj "name" "search-files"
                                        "arguments"
                                        (shasht:write-json arguments nil)))))))))))
        (q45-check "multiple native tool calls fail before proposal commit"
                   (handler-case
                       (let ((call
                               (obj "id" "provider-parallel" "type" "function"
                                    "function"
                                    (obj "name" "search-files"
                                         "arguments"
                                         (shasht:write-json arguments nil)))))
                         (%conversation-native-response-captured
                          (obj "choices"
                               (vector
                                (obj "message"
                                     (obj "role" "assistant" "content" :null
                                          "tool_calls" (vector call call)))))
                          variant-manifest 41)
                         nil)
                     (error () t)))
        (q45-check "unknown native tool calls fail before proposal commit"
                   (handler-case
                       (progn
                         (%conversation-native-response-captured
                          (obj "choices"
                               (vector
                                (obj "message"
                                     (obj "role" "assistant" "content" :null
                                          "tool_calls"
                                          (vector
                                           (obj "id" "provider-unknown"
                                                "type" "function"
                                                "function"
                                                (obj "name" "unknown-tool"
                                                     "arguments" "{}")))))))
                          variant-manifest 41)
                         nil)
                     (error () t)))
        (q45-check "provider call identity is absent from committed authority"
                   (let ((captured
                           (%conversation-native-response-captured
                            (obj "choices"
                                 (vector
                                  (obj "message"
                                       (obj "role" "assistant" "content" :null
                                            "tool_calls"
                                            (vector
                                             (obj
                                              "id" "provider-call-is-not-authority"
                                              "type" "function"
                                              "function"
                                              (obj "name" "search-files"
                                                   "arguments"
                                                   (shasht:write-json
                                                    arguments nil))))))))
                            variant-manifest 41)))
                     (null (search "provider-call-is-not-authority"
                                   (shasht:write-json captured nil)))))
        (q45-check "runtime derives current-turn and verified tool evidence"
                   (let* ((captured
                            (%conversation-native-response-captured
                             (obj "choices"
                                  (vector
                                   (obj "message"
                                        (obj "role" "assistant"
                                             "content" "Natural answer."))))
                             variant-manifest 41))
                          (proposal
                            (aref (gethash "proposals" captured) 0)))
                     (and (equalp (vector 41 42)
                                  (gethash "evidence_event_ids" proposal))
                          (null (search "provider-call-is-not-authority"
                                        (shasht:write-json captured nil)))))))
    (let ((fabricated-prefix
            (shasht:read-json
             (shasht:write-json captured-prefix nil))))
      (setf (gethash "id" (aref fabricated-prefix 0)) 999)
      (q45-check "conversation joins later work receipts through durable authority"
                  (let* ((rebuilt
                           (%conversation-assembly-spec
                            (coerce fabricated-prefix 'list) 41
                            "Find the needle" *agent-id* profile
                            "local" "terminal" "work:fixture"))
                         (ids (coerce (gethash "eligible_evidence_ids" rebuilt)
                                      'list)))
                    (and (member 55 ids) (not (member 999 ids))))))))
(q45-check "open-ended HTTP requests delegate completion stopping to the model"
           (and (fboundp '%conversation-http-request-payload)
                (let ((payload (%conversation-http-request-payload
                                (list (obj "role" "user" "content" "hi"))
                                "fixture-model" 0.3d0)))
                  (and (not (nth-value 1 (gethash "max_tokens" payload)))
                       (not (nth-value 1
                                       (gethash "max_completion_tokens"
                                                payload)))))))
(q45-check "bounded artifact protocols may opt into a completion limit"
           (let* ((*conscious-conversation-max-output-tokens* 8192)
                  (payload
                    (%conversation-http-request-payload
                     (list (obj "role" "user" "content" "bounded artifact"))
                     "fixture-model" 0.3d0)))
             (= 8192 (gethash "max_completion_tokens" payload))))
(let* ((tools (vector (conscious-file-search-openai-tool-schema)))
       (payload
         (%conversation-http-request-payload
          (list (obj "role" "system" "content" "policy")
                (obj "role" "user" "content" "context"))
          "qwen/qwen3.5-9b" 0.3d0
          "http://127.0.0.1:1234/v1/chat/completions" tools)))
  (q45-check "OpenAI-compatible request carries the strict native tool schema"
             (let* ((tool (aref (gethash "tools" payload) 0))
                    (function (gethash "function" tool))
                    (parameters (gethash "parameters" function)))
               (and (= 1 (length (gethash "tools" payload)))
                    (string= "auto" (gethash "tool_choice" payload ""))
                    (nth-value 1 (gethash "parallel_tool_calls" payload))
                    (null (gethash "parallel_tool_calls" payload))
                    (eq t (gethash "strict" function))
                    (nth-value 1 (gethash "additionalProperties" parameters))
                    (null (gethash "additionalProperties" parameters))
                    (null (gethash "response_format" payload))
                    (null (gethash "enable_thinking"
                                   (gethash "chat_template_kwargs" payload))))))
  (let* ((forced-choice
           (obj "type" "function"
                "function" (obj "name" "search-files")))
         (forced
           (%conversation-http-request-payload
            (list (obj "role" "user" "content" "find it"))
            "qwen/qwen3.5-9b" 0.3d0
            "http://127.0.0.1:1234/v1/chat/completions" tools
            forced-choice)))
    (q45-check "one semantic boundary may force an exact native function"
               (equalp forced-choice (gethash "tool_choice" forced))))
  (q45-check "LM Studio native endpoint refuses a lossy tool adaptation"
             (handler-case
                 (progn
                   (%conversation-http-request-payload
                    nil "fixture" 0.3d0
                    "http://127.0.0.1:1234/api/v1/chat" tools)
                   nil)
               (error () t)))
  (q45-check "LM Studio receives the final native current stimulus"
             (let ((native
                     (%conversation-http-request-payload
                      (list (obj "role" "system" "content" "policy")
                            (obj "role" "user" "content" "context envelope")
                            (obj "role" "user" "content" "current turn"))
                      "fixture" 0.3d0
                      "http://127.0.0.1:1234/api/v1/chat")))
               (string= "current turn" (gethash "input" native "")))))

(let* ((*conscious-conversation-provider-profile*
         (obj "provider" "openrouter"
              "endpoint" "https://openrouter.ai/api/v1/chat/completions"
              "reasoning" (obj "enabled" nil)
              "supports_parallel_tool_calls_parameter" nil
              "provider_routing"
              (obj "sort" "price" "require_parameters" t
                   "data_collection" "deny" "zdr" t
                   "max_price_usd_per_million"
                   (obj "prompt" 0.20d0 "completion" 0.40d0))))
       (endpoint "https://openrouter.ai/api/v1/chat/completions")
       (messages (list (obj "role" "user" "content" "bounded fixture")))
       (tools (vector (conscious-file-search-openai-tool-schema)))
       (payload (%conversation-http-request-payload
                 messages "xiaomi/mimo-v2.5" 0.3d0 endpoint tools)))
  (q45-check "declared OpenRouter profile is the only remote endpoint"
             (and (%conversation-authorized-endpoint-p endpoint)
                  (not (%conversation-authorized-endpoint-p
                        "https://example.com/v1/chat/completions"))))
  (q45-check "OpenRouter request carries declared routing policy"
             (hash-table-p (gethash "provider" payload)))
  (let* ((relaxed-profile
           (obj "provider" "openrouter"
                "endpoint" endpoint
                "reasoning" (obj "enabled" nil)
                "supports_parallel_tool_calls_parameter" nil
                "provider_routing"
                (obj "sort" "price" "require_parameters" t
                     "data_collection" "allow" "zdr" nil
                     "max_price_usd_per_million"
                     (obj "prompt" 0.20d0 "completion" 0.40d0))))
         (*conscious-conversation-provider-profile* relaxed-profile)
         (relaxed
           (gethash "provider"
                    (%conversation-http-request-payload
                     messages "alternate/model" 0.3d0 endpoint tools))))
    (q45-check "OpenRouter request carries explicit relaxed privacy routing"
               (and (string= "allow"
                             (gethash "data_collection" relaxed ""))
                    (null (gethash "zdr" relaxed))
                    (eq t (gethash "require_parameters" relaxed)))))
  (q45-check "OpenRouter request carries declared reasoning policy"
             (hash-table-p (gethash "reasoning" payload)))
  (let* ((native-reasoning-profile
           (obj "provider" "openrouter"
                "endpoint" endpoint
                "supports_parallel_tool_calls_parameter" nil
                "provider_routing" (gethash "provider_routing"
                                             *conscious-conversation-provider-profile*)))
         (*conscious-conversation-provider-profile* native-reasoning-profile)
         (native-reasoning
           (%conversation-http-request-payload
            messages "mandatory/reasoning-model" 0.3d0 endpoint tools)))
    (q45-check "OpenRouter model-default reasoning omits the reasoning member"
               (not (nth-value 1 (gethash "reasoning" native-reasoning)))))
  (q45-check "OpenRouter uses native tools without a custom JSON response"
             (and (= 1 (length (gethash "tools" payload)))
                  (not (nth-value 1
                                  (gethash "parallel_tool_calls" payload)))
                  (null (gethash "response_format" payload))))
  (q45-check "remote Qwen names do not receive local server template hints"
             (null (gethash "chat_template_kwargs"
                            (%conversation-http-request-payload
                             messages "qwen/remote-model" 0.3d0 endpoint))))
  (q45-check "OpenRouter request has a positive profile-priced admission bound"
             (plusp (%conversation-openrouter-request-cost-bound
                     messages endpoint "xiaomi/mimo-v2.5" 0.3d0 tools)))
  ;; The reserve prices prompt tokens from payload bytes, and it is an
  ;; admission gate rather than telemetry: counting one byte per token priced
  ;; a request roughly three times above its true cost, which refused
  ;; reviewed-graph identity formation on a large corpus before the model was
  ;; ever called.
  (q45-check "prompt reserve scales with the bytes-per-token divisor"
             (let ((at-one
                     (let ((*conversation-request-bytes-per-prompt-token* 1))
                       (%conversation-openrouter-request-cost-bound
                        messages endpoint "xiaomi/mimo-v2.5" 0.3d0 tools)))
                   (at-three
                     (let ((*conversation-request-bytes-per-prompt-token* 3))
                       (%conversation-openrouter-request-cost-bound
                        messages endpoint "xiaomi/mimo-v2.5" 0.3d0 tools))))
               (and (plusp at-three) (< at-three at-one))))
  (q45-check "bytes-per-prompt-token stays a positive over-pricing divisor"
             (and (integerp *conversation-request-bytes-per-prompt-token*)
                  (<= 1 *conversation-request-bytes-per-prompt-token* 4))))

;; A reviewed-graph identity payload of the size a large corpus actually
;; produces must price below the per-request ceiling, or entity extraction is
;; gated off entirely and the graph silently stays empty while retryable
;; refusals reschedule against the cognitive lock.
(let* ((endpoint "https://openrouter.ai/api/v1/chat/completions")
       (bulk (make-string 170000 :initial-element #\a))
       (messages (list (obj "role" "system" "content" "graph phase")
                       (obj "role" "user" "content" bulk)))
       (tools (vector))
       (*conscious-conversation-provider-profile*
         (obj "model" "meta/muse-glimmer-30b"
              "endpoint" endpoint
              "provider_routing"
              (obj "max_price_usd_per_million"
                   (obj "prompt" 0.33d0 "completion" 1.21d0))))
       ;; The densest reviewed-graph phase reserves this many output tokens.
       (*conscious-conversation-max-output-tokens* 8192))
  (let* ((bound (%conversation-openrouter-request-cost-bound
                 messages endpoint "meta/muse-glimmer-30b" 0.3d0 tools))
         (microusd (ceiling (* 1000000d0 bound))))
    (q45-check "a 170KB reviewed-graph request prices under the 60000 ceiling"
               (<= 1 microusd 60000))))


(let ((*conscious-conversation-cost-ceiling-usd* 0.01d0)
      (*conscious-conversation-provider-attempts* 0)
      (*conscious-conversation-provider-spent-usd* 0d0)
      (*conscious-conversation-provider-budget-uncertain-p* nil))
  (q45-check "OpenRouter request count is telemetry rather than authority"
             (and (%conversation-openrouter-budget-ready-p)
                  (progn
                    (setf *conscious-conversation-provider-attempts* 1)
                    (%conversation-openrouter-budget-ready-p)))))

(let ((*conscious-conversation-cost-ceiling-usd* 0.01d0)
      (*conscious-conversation-provider-attempts* 0)
      (*conscious-conversation-provider-spent-usd* 0d0)
      (*conscious-conversation-provider-budget-uncertain-p* nil)
      (*conscious-conversation-accounting-anomaly-count* 0)
      (*conscious-conversation-last-accounting-anomaly* nil)
      (*conscious-conversation-most-recent-accounting-anomaly* nil))
  (%conversation-openrouter-charge
   (obj "usage" (obj "prompt_tokens" 1 "cost" "unexpected")) 0.001d0 t)
  (q45-check "unexpected OpenRouter cost type charges the bound and continues"
             (and (= 0.001d0
                     *conscious-conversation-provider-spent-usd*)
                  (= 1 *conscious-conversation-accounting-anomaly-count*)
                  (string= "bounded-fallback"
                           (gethash
                            "status"
                            *conscious-conversation-last-accounting-anomaly*))
                  (string= "provider-cost-type-invalid"
                           (gethash
                            "reason"
                            *conscious-conversation-last-accounting-anomaly*))
                  (not *conscious-conversation-provider-budget-uncertain-p*)
                  (%conversation-openrouter-budget-ready-p))))

(let* ((*conscious-conversation-provider-profile*
         (obj "provider" "openrouter"
              "model" "xiaomi/mimo-v2.5"
              "endpoint" "https://openrouter.ai/api/v1/chat/completions"
              "reasoning" (obj "enabled" nil)
              "supports_parallel_tool_calls_parameter" nil
              "provider_routing"
              (obj "sort" "price" "require_parameters" t
                   "data_collection" "deny" "zdr" t
                   "max_price_usd_per_million"
                   (obj "prompt" 0.20d0 "completion" 0.40d0))))
       (*conscious-conversation-cost-ceiling-usd* 0.01d0)
       (*conscious-conversation-provider-attempts* 0)
       (*conscious-conversation-provider-spent-usd* 0d0)
       (*conscious-conversation-private-provider-call-p* t)
       (*conscious-conversation-private-provider-attempts* 0)
       (*conscious-conversation-private-provider-spent-usd* 0d0)
       (*conscious-conversation-provider-budget-uncertain-p* nil)
       (*q45-observed-provider-request* nil)
       (calls 0)
       (response
         (progv
             (list (intern
                    "*CONSCIOUS-CONVERSATION-PROVIDER-REQUEST-OBSERVER*"
                    :agent))
             (list (lambda (payload)
                     (setf *q45-observed-provider-request* payload)))
           (%conversation-http-model-call
            (list (obj "role" "user" "content" "budgeted fixture"))
            "https://openrouter.ai/api/v1/chat/completions"
            "xiaomi/mimo-v2.5" 0.3d0
            :transport-fn
            (lambda (&rest ignored)
              (declare (ignore ignored))
              (incf calls)
              (obj "choices" (vector) "usage" (obj "cost" 0.0001d0)))))))
  (declare (ignore response))
  (q45-check "injected model transport remains below remote budget gate"
             (and (= calls 1)
                  (= *conscious-conversation-provider-attempts* 1)
                  (= *conscious-conversation-private-provider-attempts* 1)
                  (plusp *conscious-conversation-provider-spent-usd*)
                  (= *conscious-conversation-provider-spent-usd*
                     *conscious-conversation-private-provider-spent-usd*)
                  (not *conscious-conversation-provider-budget-uncertain-p*)))
  (let ((observed-request *q45-observed-provider-request*))
    (q45-check "provider request observer receives the transmitted JSON body"
               (hash-table-p observed-request))
    (q45-check "observed provider request retains model and temperature"
               (and (hash-table-p observed-request)
                    (string= "xiaomi/mimo-v2.5"
                             (gethash "model" observed-request ""))
                    (= 0.3d0 (gethash "temperature" observed-request -1))))
    (q45-check "observed provider request retains the exact message vector"
               (and (hash-table-p observed-request)
                    (= 1 (length (gethash "messages" observed-request
                                          (vector))))))
    (q45-check "observed provider request retains reasoning and routing policy"
               (and (hash-table-p observed-request)
                    (hash-table-p (gethash "reasoning" observed-request))
                    (hash-table-p (gethash "provider" observed-request))))))

(let* ((*conscious-conversation-provider-profile*
         (obj "provider" "openrouter"
              "model" "xiaomi/mimo-v2.5"
              "endpoint" "https://openrouter.ai/api/v1/chat/completions"
              "reasoning" (obj "enabled" nil)
              "supports_parallel_tool_calls_parameter" nil
              "provider_routing"
              (obj "sort" "price" "require_parameters" t
                   "data_collection" "deny" "zdr" t
                   "max_price_usd_per_million"
                   (obj "prompt" 0.20d0 "completion" 0.40d0))))
       (*conscious-conversation-cost-ceiling-usd* 0.01d0)
       (*conscious-conversation-provider-attempts* 0)
       (*conscious-conversation-provider-spent-usd* 0d0)
       (*conscious-conversation-provider-budget-uncertain-p* nil)
       (endpoint "https://openrouter.ai/api/v1/chat/completions")
       (rejection
         (make-condition
          'dex:http-request-not-found
          :body
          "{\"error\":{\"message\":\"No endpoints support every requested parameter\"}}"
          :status 404 :headers nil :uri (quri:uri endpoint) :method :post))
       (caught nil))
  (handler-case
      (%conversation-http-model-call
       (list (obj "role" "user" "content" "bounded fixture"))
       endpoint "xiaomi/mimo-v2.5" 0.3d0
       :transport-fn
       (lambda (&rest ignored)
         (declare (ignore ignored))
         (error rejection)))
    (dex:http-request-failed (condition) (setf caught condition)))
  (multiple-value-bind (code reason status condition-type)
      (%conversation-provider-failure-details caught)
    (q45-check "known OpenRouter 4xx rejection remains retryable and diagnosed"
               (and (= 1 *conscious-conversation-provider-attempts*)
                    (not *conscious-conversation-provider-budget-uncertain-p*)
                    (not (member
                          408
                          *conscious-conversation-known-http-rejection-statuses*))
                    (string= "provider-http-404" code)
                    (= 404 status)
                    (string= "http-request-not-found" condition-type)
                    (search "No endpoints support every requested parameter"
                            reason)))))

(let* ((*conscious-conversation-provider-profile*
         (obj "provider" "openrouter"
              "model" "xiaomi/mimo-v2.5"
              "endpoint" "https://openrouter.ai/api/v1/chat/completions"
              "reasoning" (obj "enabled" nil)
              "supports_parallel_tool_calls_parameter" nil
              "provider_routing"
              (obj "sort" "price" "require_parameters" t
                   "data_collection" "deny" "zdr" t
                   "max_price_usd_per_million"
                   (obj "prompt" 0.20d0 "completion" 0.40d0))))
       (*conscious-conversation-cost-ceiling-usd* 0.01d0)
       (*conscious-conversation-provider-attempts* 0)
       (*conscious-conversation-provider-spent-usd* 0d0)
       (*conscious-conversation-provider-budget-uncertain-p* nil)
       (*conscious-conversation-accounting-anomaly-count* 0)
       (*conscious-conversation-last-accounting-anomaly* nil)
       (caught nil))
  (handler-case
      (%conversation-http-model-call
       (list (obj "role" "user" "content" "ambiguous fixture"))
       "https://openrouter.ai/api/v1/chat/completions"
       "xiaomi/mimo-v2.5" 0.3d0
       :transport-fn
       (lambda (&rest ignored)
         (declare (ignore ignored))
         (error "fixture transport outcome unknown")))
    (error (condition) (setf caught condition)))
  (q45-check "ambiguous unbounded transport freezes later calls honestly"
             (and caught
                  (= 1 *conscious-conversation-provider-attempts*)
                  (zerop *conscious-conversation-provider-spent-usd*)
                  (= 1 *conscious-conversation-accounting-anomaly-count*)
                  (string= "provider-outcome-ambiguous"
                           (gethash
                            "reason"
                            *conscious-conversation-last-accounting-anomaly*))
                  *conscious-conversation-provider-budget-uncertain-p*
                  (not (%conversation-openrouter-budget-ready-p)))))

(let* ((*conscious-conversation-provider-profile*
         (obj "provider" "openrouter"
              "model" "xiaomi/mimo-v2.5"
              "endpoint" "https://openrouter.ai/api/v1/chat/completions"
              "context_capacity_tokens" 1000000
              "reasoning" (obj "enabled" t "effort" "medium")
              "supports_parallel_tool_calls_parameter" nil
              "provider_routing"
              (obj "sort" "price" "require_parameters" t
                   "data_collection" "deny" "zdr" t
                   "max_price_usd_per_million"
                   (obj "prompt" 0.20d0 "completion" 0.40d0))))
       (*conscious-conversation-cost-ceiling-usd* 1d0)
       (*conscious-conversation-provider-attempts* 0)
       (*conscious-conversation-provider-spent-usd* 0d0)
       (*conscious-conversation-private-provider-call-p* t)
       (*conscious-conversation-private-provider-spent-usd* 0d0)
       (*conscious-conversation-provider-budget-uncertain-p* nil)
       (*conscious-conversation-accounting-anomaly-count* 0)
       (*conscious-conversation-last-accounting-anomaly* nil)
       (*conscious-conversation-openrouter-generation-lookup-fn*
         (lambda (generation-id api-key)
           (declare (ignore api-key))
           (and (string= generation-id "gen-fixture-123") 0.00321d0)))
       (caught nil))
  (handler-case
      (%conversation-http-model-call
       (list (obj "role" "user" "content" "interrupted stream fixture"))
       "https://openrouter.ai/api/v1/chat/completions"
       "xiaomi/mimo-v2.5" 0.3d0
       :transport-fn
       (lambda (&rest ignored)
         (declare (ignore ignored))
         (%conversation-provider-progress-observe
          "gen-fixture-123" 12 1000 600 200 0)
         (error "fixture stream interrupted")))
    (error (condition) (setf caught condition)))
  (q45-check "interrupted stream charges authoritative generation cost"
             (and caught
                  (= 0.00321d0
                     *conscious-conversation-provider-spent-usd*)
                  (= *conscious-conversation-provider-spent-usd*
                     *conscious-conversation-private-provider-spent-usd*)
                  (not *conscious-conversation-provider-budget-uncertain-p*)
                  (%conversation-openrouter-budget-ready-p)
                  (string= "generation-reconciled"
                           (gethash
                            "status"
                            *conscious-conversation-last-accounting-anomaly*))
                  (string= "gen-fixture-123"
                           (gethash
                            "generation_id"
                            *conscious-conversation-last-accounting-anomaly*)))))

(let* ((*conscious-conversation-provider-profile*
         (obj "provider" "openrouter"
              "model" "xiaomi/mimo-v2.5"
              "endpoint" "https://openrouter.ai/api/v1/chat/completions"
              "context_capacity_tokens" 1000000
              "reasoning" (obj "enabled" t "effort" "medium")
              "supports_parallel_tool_calls_parameter" nil
              "provider_routing"
              (obj "sort" "price" "require_parameters" t
                   "data_collection" "deny" "zdr" t
                   "max_price_usd_per_million"
                   (obj "prompt" 0.20d0 "completion" 0.40d0))))
       (*conscious-conversation-cost-ceiling-usd* 1d0)
       (*conscious-conversation-provider-spent-usd* 0d0)
       (*conscious-conversation-private-provider-call-p* t)
       (*conscious-conversation-private-provider-spent-usd* 0d0)
       (*conscious-conversation-provider-budget-uncertain-p* nil)
       (*conscious-conversation-pending-generation-settlements* nil)
       (*conscious-conversation-last-accounting-anomaly* nil)
       (lookups 0)
       (*conscious-conversation-openrouter-generation-lookup-fn*
         (lambda (generation-id api-key)
           (declare (ignore generation-id api-key))
           (when (> (incf lookups) 1) 0.00456d0)))
       (caught nil))
  (handler-case
      (%conversation-http-model-call
       (list (obj "role" "user" "content" "delayed settlement fixture"))
       "https://openrouter.ai/api/v1/chat/completions"
       "xiaomi/mimo-v2.5" 0.3d0
       :transport-fn
       (lambda (&rest ignored)
         (declare (ignore ignored))
         (%conversation-provider-progress-observe
          "gen-delayed-456" 12 1000 600 200 0)
         (error "fixture stream interrupted")))
    (error (condition) (setf caught condition)))
  (q45-check "delayed generation settlement books no fictitious spend"
             (and caught
                  (zerop *conscious-conversation-provider-spent-usd*)
                  *conscious-conversation-provider-budget-uncertain-p*
                  (= 1 (length
                        *conscious-conversation-pending-generation-settlements*))
                  (string= "generation-reconciliation-pending"
                           (gethash
                            "status"
                            *conscious-conversation-last-accounting-anomaly*))))
  (q45-check "next admission reconciles delayed generation and resumes"
             (and (%conversation-openrouter-budget-ready-p)
                  (= 0.00456d0
                     *conscious-conversation-provider-spent-usd*)
                  (= *conscious-conversation-provider-spent-usd*
                     *conscious-conversation-private-provider-spent-usd*)
                  (not *conscious-conversation-provider-budget-uncertain-p*)
                  (null
                   *conscious-conversation-pending-generation-settlements*)
                  (string= "generation-reconciled"
                           (gethash
                            "status"
                            *conscious-conversation-last-accounting-anomaly*)))))

(let* ((*conscious-conversation-provider-profile*
         (obj "provider" "openrouter"
              "model" "xiaomi/mimo-v2.5"
              "endpoint" "https://openrouter.ai/api/v1/chat/completions"
              "context_capacity_tokens" 1000000
              "reasoning" (obj "enabled" t "effort" "medium")
              "supports_parallel_tool_calls_parameter" nil
              "provider_routing"
              (obj "sort" "price" "require_parameters" t
                   "data_collection" "deny" "zdr" t
                   "max_price_usd_per_million"
                   (obj "prompt" 0.20d0 "completion" 0.40d0))))
       (*conscious-conversation-cost-ceiling-usd* 1d0)
       (*conscious-conversation-provider-spent-usd* 0d0)
       (*conscious-conversation-private-provider-call-p* t)
       (*conscious-conversation-private-provider-spent-usd* 0d0)
       (*conscious-conversation-provider-budget-uncertain-p* nil)
       (*conscious-conversation-pending-generation-settlements* nil)
       (*conscious-conversation-last-accounting-anomaly* nil)
       (*conscious-conversation-openrouter-generation-lookup-fn*
         (lambda (generation-id api-key)
           (declare (ignore generation-id api-key))
           nil))
       (caught nil))
  (handler-case
      (%conversation-http-model-call
       (list (obj "role" "user" "content" "unsettled generation fixture"))
       "https://openrouter.ai/api/v1/chat/completions"
       "xiaomi/mimo-v2.5" 0.3d0
       :transport-fn
       (lambda (&rest ignored)
         (declare (ignore ignored))
         (%conversation-provider-progress-observe
          "gen-never-settles-789" 12 1000 600 200 0)
         (error "fixture stream interrupted")))
    (error (condition) (setf caught condition)))
  (let* ((pending
           (first *conscious-conversation-pending-generation-settlements*))
         (fallback (and pending (gethash "fallback_cost_usd" pending))))
    (q45-check "unsettled generation retains a finite capacity fallback"
               (and caught (realp fallback) (plusp fallback)
                    *conscious-conversation-provider-budget-uncertain-p*))
    (q45-check "later admission charges fallback and restores liveness"
               (and (%conversation-openrouter-budget-ready-p)
                    (= fallback *conscious-conversation-provider-spent-usd*)
                    (= fallback
                       *conscious-conversation-private-provider-spent-usd*)
                    (null
                     *conscious-conversation-pending-generation-settlements*)
                    (not *conscious-conversation-provider-budget-uncertain-p*)
                    (string= "generation-capacity-fallback"
                             (gethash
                              "status"
                              *conscious-conversation-last-accounting-anomaly*))))))

;;; --- provider retry with backoff -----------------------------------------

(q45-check "a timeout has no HTTP status and is retryable"
           (%conversation-provider-retryable-failure-p
            "provider-call-timeout" :null))
(q45-check "a generic transport failure has no HTTP status and is retryable"
           (%conversation-provider-retryable-failure-p
            "provider-transport-failed" :null))
(q45-check "a 429 rate limit is retryable"
           (%conversation-provider-retryable-failure-p "provider-http-429" 429))
(q45-check "a 500 is retryable"
           (%conversation-provider-retryable-failure-p "provider-http-500" 500))
(q45-check "a 503 is retryable"
           (%conversation-provider-retryable-failure-p "provider-http-503" 503))
(q45-check "a 400 bad request is not retryable"
           (not (%conversation-provider-retryable-failure-p
                 "provider-http-400" 400)))
(q45-check "a 404 not-found is not retryable"
           (not (%conversation-provider-retryable-failure-p
                 "provider-http-404" 404)))
(q45-check "a 401 unauthorized is not retryable"
           (not (%conversation-provider-retryable-failure-p
                 "provider-http-401" 401)))

(let* ((*conscious-conversation-provider-profile*
         (obj "provider" "openrouter"
              "model" "xiaomi/mimo-v2.5"
              "endpoint" "https://openrouter.ai/api/v1/chat/completions"
              "context_capacity_tokens" 1000000
              "reasoning" (obj "enabled" nil)
              "supports_parallel_tool_calls_parameter" nil
              "provider_routing"
              (obj "sort" "price" "require_parameters" t
                   "data_collection" "deny" "zdr" t
                   "max_price_usd_per_million"
                   (obj "prompt" 0.20d0 "completion" 0.40d0))))
       (*conscious-conversation-cost-ceiling-usd* 1d0)
       (*conscious-conversation-provider-attempts* 0)
       (*conscious-conversation-provider-spent-usd* 0d0)
       (*conscious-conversation-provider-budget-uncertain-p* nil)
       (*conscious-conversation-provider-retry-limit* 3)
       (*conscious-conversation-provider-retry-backoff-seconds* 0d0)
       (calls 0)
       (attempt-log nil)
       (forged-response
         (obj "choices"
              (vector
               (obj "message" (obj "role" "assistant" "content" "ok")))
              "usage" (obj "cost" 0.0001d0)))
       (result
         (%conversation-http-model-call-with-retry
          (list (obj "role" "user" "content" "retry-then-succeed fixture"))
          "https://openrouter.ai/api/v1/chat/completions"
          "xiaomi/mimo-v2.5" 0.3d0
          :transport-fn
          (lambda (&rest ignored)
            (declare (ignore ignored))
            (incf calls)
            (if (< calls 3)
                (error "connection reset by peer")
                forged-response))
          :on-attempt-failure
          (lambda (attempt failure-code reason http-status condition-type)
            (declare (ignore reason condition-type))
            (push (list attempt failure-code http-status) attempt-log)))))
  (q45-check "a call that fails twice then succeeds returns the live response"
             (eq result forged-response))
  (q45-check "each failed attempt before success is reported once, in order"
             (equal (reverse attempt-log)
                    '((1 "provider-transport-failed" :null)
                      (2 "provider-transport-failed" :null))))
  (q45-check "three transport attempts were actually made"
             (= 3 calls)))

(let* ((*conscious-conversation-provider-profile*
         (obj "provider" "openrouter"
              "model" "xiaomi/mimo-v2.5"
              "endpoint" "https://openrouter.ai/api/v1/chat/completions"
              "context_capacity_tokens" 1000000
              "reasoning" (obj "enabled" nil)
              "supports_parallel_tool_calls_parameter" nil
              "provider_routing"
              (obj "sort" "price" "require_parameters" t
                   "data_collection" "deny" "zdr" t
                   "max_price_usd_per_million"
                   (obj "prompt" 0.20d0 "completion" 0.40d0))))
       (*conscious-conversation-cost-ceiling-usd* 1d0)
       (*conscious-conversation-provider-attempts* 0)
       (*conscious-conversation-provider-spent-usd* 0d0)
       (*conscious-conversation-provider-budget-uncertain-p* nil)
       (*conscious-conversation-provider-retry-limit* 3)
       (*conscious-conversation-provider-retry-backoff-seconds* 0d0)
       (calls 0)
       (caught nil))
  (handler-case
      (%conversation-http-model-call-with-retry
       (list (obj "role" "user" "content" "exhausted retries fixture"))
       "https://openrouter.ai/api/v1/chat/completions"
       "xiaomi/mimo-v2.5" 0.3d0
       :transport-fn
       (lambda (&rest ignored)
         (declare (ignore ignored))
         (incf calls)
         (error "connection reset by peer")))
    (error (condition) (setf caught condition)))
  (q45-check "exhausting every retry still surfaces the failure"
             (and caught (= 3 calls))))

(let* ((*conscious-conversation-provider-profile*
         (obj "provider" "openrouter"
              "model" "xiaomi/mimo-v2.5"
              "endpoint" "https://openrouter.ai/api/v1/chat/completions"
              "context_capacity_tokens" 1000000
              "reasoning" (obj "enabled" nil)
              "supports_parallel_tool_calls_parameter" nil
              "provider_routing"
              (obj "sort" "price" "require_parameters" t
                   "data_collection" "deny" "zdr" t
                   "max_price_usd_per_million"
                   (obj "prompt" 0.20d0 "completion" 0.40d0))))
       (*conscious-conversation-cost-ceiling-usd* 1d0)
       (*conscious-conversation-provider-attempts* 0)
       (*conscious-conversation-provider-spent-usd* 0d0)
       (*conscious-conversation-provider-budget-uncertain-p* nil)
       (*conscious-conversation-provider-retry-limit* 5)
       (*conscious-conversation-provider-retry-backoff-seconds* 0d0)
       (calls 0)
       (rejection
         (make-condition
          'dex:http-request-not-found
          :body "{\"error\":{\"message\":\"model not found\"}}"
          :status 404 :headers nil
          :uri (quri:uri "https://openrouter.ai/api/v1/chat/completions")
          :method :post))
       (caught nil))
  (handler-case
      (%conversation-http-model-call-with-retry
       (list (obj "role" "user" "content" "non-retryable rejection fixture"))
       "https://openrouter.ai/api/v1/chat/completions"
       "xiaomi/mimo-v2.5" 0.3d0
       :transport-fn
       (lambda (&rest ignored)
         (declare (ignore ignored))
         (incf calls)
         (error rejection)))
    (error (condition) (setf caught condition)))
  (q45-check "a definitive 404 rejection is not retried"
             (and caught (= 1 calls))))

(let* ((*conscious-conversation-provider-profile*
         (obj "provider" "openrouter"
              "model" "xiaomi/mimo-v2.5"
              "endpoint" "https://openrouter.ai/api/v1/chat/completions"
              "context_capacity_tokens" 1000000
              "reasoning" (obj "enabled" nil)
              "supports_parallel_tool_calls_parameter" nil
              "provider_routing"
              (obj "sort" "price" "require_parameters" t
                   "data_collection" "deny" "zdr" t
                   "max_price_usd_per_million"
                   (obj "prompt" 0.20d0 "completion" 0.40d0))))
       (*conscious-conversation-cost-ceiling-usd* 1d0)
       (*conscious-conversation-provider-attempts* 0)
       (*conscious-conversation-provider-spent-usd* 0d0)
       (*conscious-conversation-provider-budget-uncertain-p* nil)
       (*conscious-conversation-provider-retry-limit* 5)
       (*conscious-conversation-provider-retry-backoff-seconds* 0d0)
       (calls 0)
       (caught nil))
  ;; A prior failed attempt can leave the session budget uncertain (an
  ;; unsettled generation with no chargeable bound -- see the "unsettled
  ;; generation" fixture above). Retrying into that state would either bill
  ;; past the session ceiling or hit the same admission refusal the fresh
  ;; attempt would raise anyway, just less legibly, so the loop must stop.
  (handler-case
      (%conversation-http-model-call-with-retry
       (list (obj "role" "user" "content" "budget uncertain fixture"))
       "https://openrouter.ai/api/v1/chat/completions"
       "xiaomi/mimo-v2.5" 0.3d0
       :transport-fn
       (lambda (&rest ignored)
         (declare (ignore ignored))
         (incf calls)
         (setf *conscious-conversation-provider-budget-uncertain-p* t)
         (error "connection reset by peer")))
    (error (condition) (setf caught condition)))
  (q45-check "a failure that leaves the budget uncertain is not retried"
             (and caught (= 1 calls)
                  *conscious-conversation-provider-budget-uncertain-p*)))

(let* ((endpoint "http://localhost:1234/v1/chat/completions")
       (*conscious-conversation-provider-retry-limit* 5)
       (*conscious-conversation-provider-retry-backoff-seconds* 0d0)
       (calls 0)
       (caught nil))
  (handler-case
      (%conversation-http-model-call-with-retry
       (list (obj "role" "user" "content" "loopback fixture"))
       endpoint "fixture-model" 0.3d0
       :transport-fn
       (lambda (&rest ignored)
         (declare (ignore ignored))
         (incf calls)
         (error "fixture transport failure")))
    (error (condition) (setf caught condition)))
  (q45-check "a loopback endpoint never retries; it is a test seam, not a network"
             (and caught (= 1 calls))))

;; Charging a whole context capacity as completion for one unobserved outcome
;; is pessimistic past usefulness: at a million-token capacity it bills about
;; half a dollar per ambiguous call, three of which exhausted a live
;; instance's entire private cost share and paused its private cognition --
;; the freeze this fallback exists to prevent.
(let* ((*conscious-conversation-provider-profile*
         (obj "provider" "openrouter"
              "model" "xiaomi/mimo-v2.5"
              "endpoint" "https://openrouter.ai/api/v1/chat/completions"
              "context_capacity_tokens" 1048576
              "provider_routing"
              (obj "max_price_usd_per_million"
                   (obj "prompt" 0.20d0 "completion" 0.40d0)))))
  (let* ((prompt-reserve 0.001d0)
         (bound (%conversation-openrouter-unbounded-outcome-cost-bound
                 prompt-reserve))
         (completion-part (- bound prompt-reserve))
         (capacity-part (* (/ 1048576 1000000d0) 0.40d0))
         (cap-part (* (/ *conversation-unbounded-outcome-completion-token-cap*
                         1000000d0)
                      0.40d0)))
    (q45-check "ambiguous outcome charges the token cap, not context capacity"
               (and (realp bound) (plusp bound)
                    (< (abs (- completion-part cap-part)) 1d-9)
                    (< completion-part capacity-part)))
    (q45-check "ambiguous outcome charge stays far under a session ceiling"
               (< bound 0.05d0))))

;; A deferred settlement must carry a fallback the reconciler can charge.
;; Reconciliation settles a pending generation only on an exact cost or a real
;; fallback, so deferring without one pends forever and latches budget
;; uncertainty permanently.
(let* ((*conscious-conversation-provider-profile*
         (obj "provider" "openrouter"
              "model" "xiaomi/mimo-v2.5"
              "endpoint" "https://openrouter.ai/api/v1/chat/completions"
              ;; No context_capacity_tokens: the outcome bound is incomputable.
              "provider_routing"
              (obj "max_price_usd_per_million"
                   (obj "prompt" 0.20d0 "completion" 0.40d0))))
       (*conscious-conversation-cost-ceiling-usd* 1d0)
       (*conscious-conversation-provider-spent-usd* 0d0)
       (*conscious-conversation-private-provider-call-p* t)
       (*conscious-conversation-private-provider-spent-usd* 0d0)
       (*conscious-conversation-provider-budget-uncertain-p* nil)
       (*conscious-conversation-pending-generation-settlements* nil)
       (*conscious-conversation-last-accounting-anomaly* nil)
       (*conscious-conversation-openrouter-generation-lookup-fn*
         (lambda (generation-id api-key)
           (declare (ignore generation-id api-key))
           nil))
       (caught nil))
  (q45-check "an incomputable outcome bound is the condition guarded against"
             (null (%conversation-openrouter-unbounded-outcome-cost-bound
                    0.001d0)))
  (handler-case
      (%conversation-http-model-call
       (list (obj "role" "user" "content" "no capacity fixture"))
       "https://openrouter.ai/api/v1/chat/completions"
       "xiaomi/mimo-v2.5" 0.3d0
       :transport-fn
       (lambda (&rest ignored)
         (declare (ignore ignored))
         (%conversation-provider-progress-observe
          "gen-no-capacity-321" 12 1000 600 200 0)
         (error "fixture stream interrupted")))
    (error (condition) (setf caught condition)))
  (let* ((pending
           (first *conscious-conversation-pending-generation-settlements*))
         (fallback (and pending (gethash "fallback_cost_usd" pending))))
    (q45-check "a pending settlement always carries a chargeable fallback"
               (and caught (realp fallback) (not (minusp fallback))))
    (q45-check "liveness is restorable without an outcome bound"
               (and (%conversation-openrouter-budget-ready-p)
                    (null
                     *conscious-conversation-pending-generation-settlements*)
                    (not *conscious-conversation-provider-budget-uncertain-p*)))))

(when (and (fboundp 'conscious-conversation-history)
           (fboundp 'conscious-conversation-turn))
  (let* ((events
         (list (obj "id" 1 "type" "user-message" "agent_id" "q45-dev"
                      "timestamp" "2026-08-29T17:23:00Z"
                      "payload" (obj "text" "remember blue"
                                     "metadata"
                                     (obj "source" "q4.5-conversation"
                                          "persona_id" "fixture-a"
                                          "persona_fingerprint"
                                          (gethash "fingerprint"
                                                   *conscious-conversation-persona-profile*))))
                 (obj "id" 2 "type" "agent-message" "agent_id" "q45-dev"
                      "timestamp" "2026-08-29T17:24:00Z"
                      "payload" (obj "text" "I will remember blue"
                                     "metadata"
                                     (obj "source" "q4.5-conversation"
                                          "persona_id" "fixture-a"
                                          "persona_fingerprint"
                                          (gethash "fingerprint"
                                                   *conscious-conversation-persona-profile*))))
                 (obj "id" 3 "type" "user-message" "agent_id" "other"
                      "payload" (obj "text" "foreign secret"))))
         (first (conscious-conversation-history
                 events "q45-dev" :max-events 11 :character-budget 5001
                 :event-character-limit 3001))
         ;; A fresh copy simulates restart: no conversation global is retained.
         (second (conscious-conversation-history
                  (shasht:read-json (shasht:write-json events nil)) "q45-dev"
                  :max-events 11 :character-budget 5001
                 :event-character-limit 3001)))
    (q45-check "history keeps only the same partition"
               (and (= 2 (length first))
                    (null (search "foreign secret" (shasht:write-json first nil)))))
    (q45-check "history preserves dialogue order"
               (search "remember blue" (gethash "content" (aref first 0))))
    (q45-check "history exposes durable date and 24-hour local clock time"
               (and (search "[2026-08-29T"
                            (gethash "content" (aref first 0)))
                    (search ":23 "
                            (gethash "content" (aref first 0)))))
    (q45-check "history is reconstructed identically after restart"
               (string= (shasht:write-json first nil)
                        (shasht:write-json second nil))))

  (let* ((events
           (list
            (obj "id" 4 "type" "user-message" "agent_id" "q45-dev"
                 "payload"
                 (obj "text" "recursive before restart" "metadata"
                      (obj "source" "recursive-mind-v1"
                           "persona_id" "fixture-a")))
            (obj "id" 5 "type" "agent-message" "agent_id" "q45-dev"
                 "caused_by" 4
                 "payload"
                 (obj "text" "durable recursive reply" "metadata"
                      (obj "source" "recursive-mind-v1"
                           "persona_id" "fixture-a")))))
         (history
           (conscious-conversation-history
            (shasht:read-json (shasht:write-json events nil)) "q45-dev"
            :max-events 11 :character-budget 5001
            :event-character-limit 3001)))
    (q45-check "recursive dialogue is reconstructed after restart"
               (and (= 2 (length history))
                    (search "recursive before restart"
                            (gethash "content" (aref history 0)))
                    (search "durable recursive reply"
                            (gethash "content" (aref history 1))))))

  (let ((history
          (conscious-conversation-history
           (list
            (obj "id" 51 "type" "agent-message" "agent_id" "q45-dev"
                 "payload"
                 (obj "text" "I found something worth sharing."
                      "metadata"
                      (obj "source" "recursive-curiosity-reach-out-v1"
                           "persona_id" "fixture-a"))))
           "q45-dev" :max-events 11 :character-budget 5001
           :event-character-limit 3001)))
    (q45-check "autonomous curiosity reach-out becomes later dialogue history"
               (and (= 1 (length history))
                    (search "worth sharing"
                            (gethash "content" (aref history 0))))))

  (let ((history
          (conscious-conversation-history
           (list
            (obj "id" 6 "type" "user-message" "agent_id" "q45-dev"
                 "payload"
                 (obj "text" "interrupted recursive turn" "metadata"
                      (obj "source" "recursive-mind-v1"
                           "persona_id" "fixture-a"))))
           "q45-dev" :max-events 11 :character-budget 5001
           :event-character-limit 3001)))
    (q45-check "interrupted recursive root is absent from later history"
               (zerop (length history))))

  (let* ((events
           (list
            (obj "id" 61 "type" "user-message" "agent_id" "q45-dev"
                 "payload"
                 (obj "text" "What action tools can curiosity use?"
                      "metadata"
                      (obj "source" "recursive-mind-v1"
                           "persona_id" "fixture-a")))
            (obj "id" 62 "type" "model-response" "agent_id" "q45-dev"
                 "caused_by" 61
                 "payload"
                 (obj "status" "failed" "content_persisted" nil
                      "error_code" "provider-response-invalid"))))
         (first
           (conscious-conversation-history
            events "q45-dev" :before-event-id 63 :max-events 11
            :character-budget 5001 :event-character-limit 3001))
         (restarted
           (conscious-conversation-history
            (shasht:read-json (shasht:write-json events nil))
            "q45-dev" :before-event-id 63 :max-events 11
            :character-budget 5001 :event-character-limit 3001)))
    (q45-check "durably failed recursive root remains truthful history"
               (and (= 1 (length first))
                    (search "What action tools can curiosity use?"
                            (gethash "content" (aref first 0)))
                    (search "no assistant reply was committed"
                            (gethash "content" (aref first 0)))))
    (q45-check "failed-root history is identical after restart"
               (string= (shasht:write-json first nil)
                        (shasht:write-json restarted nil))))

  (let ((history
          (conscious-conversation-history
           (list
            (obj "id" 63 "type" "user-message" "agent_id" "q45-dev"
                 "payload"
                 (obj "text" "not failed at the historical boundary"
                      "metadata"
                      (obj "source" "recursive-mind-v1"
                           "persona_id" "fixture-a")))
            (obj "id" 65 "type" "model-response" "agent_id" "q45-dev"
                 "caused_by" 63
                 "payload" (obj "status" "failed")))
           "q45-dev" :before-event-id 64 :max-events 11
           :character-budget 5001 :event-character-limit 3001)))
    (q45-check "future failure cannot backfill an as-of history projection"
               (zerop (length history))))

  (let ((history
          (conscious-conversation-history
           (list
            (obj "id" 7 "type" "user-message" "agent_id" "q45-dev"
                 "payload"
                 (obj "text" "not complete at the historical boundary"
                      "metadata"
                      (obj "source" "recursive-mind-v1"
                           "persona_id" "fixture-a")))
            (obj "id" 9 "type" "agent-message" "agent_id" "q45-dev"
                 "caused_by" 7
                 "payload"
                 (obj "text" "completed only after the boundary"
                      "metadata"
                      (obj "source" "recursive-mind-v1"
                           "persona_id" "fixture-a"))))
           "q45-dev" :before-event-id 8 :max-events 11
           :character-budget 5001 :event-character-limit 3001)))
    (q45-check "future reply cannot backfill an as-of history projection"
               (zerop (length history))))

  (let* ((events
           (list
            (obj "id" 10 "type" "user-message" "agent_id" "q45-dev"
                 "payload"
                 (obj "text" "private lifecycle subject"
                      "channel" "q5-lifecycle-cli"
                      "metadata" (obj "purpose"
                                      "contained-lifecycle-scenario")))
            (obj "id" 11 "type" "user-message" "agent_id" "q45-dev"
                 "payload"
                 (obj "text" "genuine operator turn" "channel" "terminal"
                      "metadata"
                      (obj "source" "q4.5-conversation"
                           "persona_id" "fixture-a"
                           "persona_fingerprint"
                           (gethash "fingerprint"
                                    *conscious-conversation-persona-profile*))))))
         (history
           (conscious-conversation-history
            events "q45-dev" :max-events 11 :character-budget 5001
            :event-character-limit 3001)))
    (q45-check "dialogue history rejects same-partition lifecycle pseudo-messages"
               (and (= 1 (length history))
                    (search "genuine operator turn"
                            (gethash "content" (aref history 0)))
                    (null (search "private lifecycle subject"
                                  (shasht:write-json history nil))))))

  (q45-reset)
  (setf *q45-lifecycle-rows*
        (vector (obj "source_id" 89
                     "content" "Lifecycle near-term:i1 is active; phase near-term-seeded."))
        *q45-before-assembly-fn*
        (lambda ()
          (setf *q45-lifecycle-rows*
                (vector
                 (obj "source_id" 90
                      "content"
                      "Lifecycle near-term:i1 is active; phase near-term-ready.")))))
  (setf *q45-log-receipt-p* t)
  (let ((*conscious-conversation-context-trace-fn*
          (lambda (messages metadata provider-thunk)
            (setf *q45-trace-messages* messages
                  *q45-trace-metadata* metadata)
            (funcall provider-thunk)))
        (*conscious-conversation-model-call-fn*
          (lambda (messages endpoint model temperature)
            (declare (ignore endpoint model temperature))
            (setf *q45-provider-messages* messages)
            (q45-publication-response "The durable reply."))))
    (let ((result (conscious-conversation-turn
                   "Can we continue?" :endpoint "http://127.0.0.1:1234/v1/chat/completions"
                   :model "fixture-model")))
      (q45-check "valid reply is appended before it is returned"
                 (and (string= "replied" (gethash "status" result))
                      (string= "The durable reply." (gethash "content" result))
                      (find "agent-message" *q45-events*
                            :key (lambda (event) (gethash "type" event))
                            :test #'string=)))
      (q45-check "receipted conversation journals cause no fallback replay"
                 (zerop *q45-replay-count*))
      (q45-check "durable dialogue is stamped with persona provenance"
                 (let* ((event (find "agent-message" *q45-events*
                                     :key (lambda (row) (gethash "type" row))
                                     :test #'string=))
                        (metadata (and event
                                       (gethash "metadata"
                                                (gethash "payload" event)))))
                   (and (hash-table-p metadata)
                        (string= (gethash "fingerprint"
                                          *conscious-conversation-persona-profile*)
                                 (gethash "persona_fingerprint" metadata "")))))
      (q45-check "conversation prompt advertises no tool authority"
                 (and *q45-provider-messages*
                      (zerop (length (gethash "available_tools" *q45-manifest*)))
                      (null (member "tool-call-proposal"
                                    (coerce (gethash "permitted_proposal_kinds"
                                                     *q45-manifest*) 'list)
                                    :test #'string=))))
      (q45-check "private trace port observes the exact final provider messages"
                 (and *q45-trace-messages*
                      (eq *q45-trace-messages* *q45-provider-messages*)))
      (q45-check "private trace metadata links context to its durable request"
                 (and (hash-table-p *q45-trace-metadata*)
                      (search "q45-model:"
                              (gethash "model_call_id"
                                       *q45-trace-metadata* ""))
                      (string= "fixture-composition"
                               (gethash "context_composition_hash"
                                        *q45-trace-metadata* ""))
                      (let ((history-count
                              (gethash "history_record_count"
                                       *q45-trace-metadata* -1)))
                        (and (integerp history-count)
                             (not (minusp history-count))))))
      (q45-check "conversation assembly consumes the explicit budget profile"
                 (and (= 23001 (gethash "total_character_budget"
                                        *q45-open-spec*))
                      (= 11001
                         (gethash "conversation-evidence"
                                  (gethash "section_character_budgets"
                                           *q45-open-spec*)))))
      (q45-check "sensorium states durable history and disabled semantic retrieval"
                 (let* ((sections (gethash "sections" *q45-open-spec*))
                        (sensorium (gethash "sensorium" sections))
                        (rendered (shasht:write-json sensorium nil)))
                   (and (search "across Lisp process restarts" rendered)
                        (search "Semantic memory retrieval is disabled"
                                rendered))))
      (q45-check "conversation assembly includes current lifecycle context"
                 (let* ((sections (gethash "sections" *q45-open-spec*))
                        (focus (gethash "focus-lifecycles" sections)))
                   (and (search "near-term-ready"
                                (shasht:write-json focus nil))
                        (find 90 (coerce (gethash "eligible_evidence_ids"
                                                  *q45-open-spec*)
                                         'list)
                              :test #'equal))))
      (q45-check "selected persona identity and voice reach final context"
                 (let* ((sections (gethash "sections" *q45-open-spec*))
                        (identity (gethash "identity-instructions" sections))
                        (rendered (shasht:write-json identity nil)))
                   (and (search "thoughtful cartographer" rendered)
                        (search "warm, concise, and curious" rendered))))
      (q45-check "persona content remains absent from content-free manifest"
                 (let ((rendered (shasht:write-json *q45-manifest* nil)))
                   (and (null (search "thoughtful cartographer" rendered))
                        (null (search "warm, concise, and curious" rendered)))))
      (q45-check "native response contract is separated from context data"
                 (let ((system (gethash "content"
                                        (first *q45-provider-messages*)))
                       (user (gethash "content"
                                     (second *q45-provider-messages*))))
                   (and (search "answer the operator directly" system)
                        (null (search "REQUIRED_DECISION" system))
                        (null (search "tool_calls" user)))))
      (q45-check "model applies only pAI-selected governing persona rows"
                 (let ((system (gethash "content"
                                        (first *q45-provider-messages*)))
                       (user (gethash "content"
                                      (second *q45-provider-messages*))))
                   (and (search "governing-instructions" system)
                        (search "thoughtful cartographer" user)
                        (search "warm, concise, and curious" user))))
      (q45-check "provider token usage is returned without response content"
                 (let ((usage (gethash "usage" result)))
                   (and (hash-table-p usage)
                        (= 321 (gethash "input_tokens" usage))
                        (= 45 (gethash "output_tokens" usage))
                        (= 366 (gethash "total_tokens" usage)))))
      (q45-check "turn result reports bounded dialogue context without content"
                 (let ((history (gethash "history_context" result)))
                   (and (hash-table-p history)
                        (integerp (gethash "record_count" history))
                        (not (minusp (gethash "record_count" history)))
                        (integerp (gethash "rendered_characters" history))
                        (integerp (gethash "estimated_tokens" history))
                        (integerp (gethash "omitted_record_count" history))
                        (= 9 (hash-table-count history))
                        (null (search "thoughtful cartographer"
                                      (shasht:write-json history nil))))))
      (q45-check "turn result exposes bounded phase timing diagnostics"
                 (let ((timing (gethash "timing_ms" result)))
                   (and (hash-table-p timing)
                        (every (lambda (key)
                                 (let ((value (gethash key timing)))
                                   (and (integerp value) (not (minusp value)))))
                               '("total" "admission" "context_open"
                                 "request_journal" "provider"
                                 "response_journal" "captured_parse"
                                 "captured_commit" "publication_validation"
                                 "reply_commit" "unattributed")))))))

  (q45-reset)
  (setf *q45-log-receipt-p* t)
  (let* ((prior-user-id
           (log-event
            "user-message"
            (obj "text" "Is the bounded result available?"
                 "channel" "terminal"
                 "metadata"
                 (obj "source" "q4.5-conversation"
                      "persona_id" "fixture-a"
                      "persona_fingerprint"
                      (gethash "fingerprint"
                               *conscious-conversation-persona-profile*)))))
         (prior-id
           (log-event
            "agent-message"
            (obj "text" "The prior bounded result is available."
                 "channel" "terminal"
                 "metadata"
                 (obj "source" "q4.5-conversation"
                      "persona_id" "fixture-a"
                      "persona_fingerprint"
                      (gethash "fingerprint"
                               *conscious-conversation-persona-profile*)))
            :caused-by prior-user-id))
         (*conscious-conversation-model-call-fn*
           (lambda (&rest ignored)
             (declare (ignore ignored))
             ;; Reproduce the causal shape of the lived failure without asking
             ;; the provider to transcribe any event identity.
             (q45-publication-response "Yes, the bounded result can be read."))))
    (let* ((result
             (conscious-conversation-turn
              "Can you read it?"
              :endpoint "http://localhost:1234/v1/chat/completions"
              :model "fixture-model"))
           (current-id (gethash "user_event_id" result))
           (proposal
             (and *q45-submitted-captured*
                  (aref (gethash "proposals" *q45-submitted-captured*) 0)))
           (evidence
             (and proposal
                  (coerce (gethash "evidence_event_ids" proposal) 'list))))
      (q45-check "prior history cannot detach reply from current turn"
                 (and (string= "replied" (gethash "status" result ""))
                      (member current-id evidence :test #'equal)
                      (not (member prior-id evidence :test #'equal))))
      (q45-check "trusted adapter stamps canonical proposal metadata"
                 (and (hash-table-p proposal)
                      (string= (gethash "pulse_id" *q45-manifest*)
                               (gethash "pulse_id" proposal ""))
                      (string= "conscious-q4-v1"
                               (gethash "runtime_revision" proposal ""))
                      (string= "model-deliberation"
                               (gethash "created_at_stage" proposal ""))
                      (string= "operator"
                               (gethash "audience"
                                        (gethash "payload" proposal) ""))
                      (= 1 (length evidence))))))

  (q45-reset)
  (let ((forged-response
          (obj "choices"
               (vector
                (obj "message"
                     (obj "role" "assistant"
                          "content"
                          "{\"pulse_id\":\"provider-forged-pulse\",\"content\":\"ordinary speech\"}"))))))
    (let ((*conscious-conversation-model-call-fn*
            (lambda (&rest ignored)
              (declare (ignore ignored))
              forged-response)))
      (let ((result
              (conscious-conversation-turn
               "Do not trust provider metadata."
               :endpoint "http://localhost:1234/v1/chat/completions"
               :model "fixture-model")))
        (q45-check "provider cannot add or override mechanical proposal metadata"
                   (let ((proposal
                           (and *q45-submitted-captured*
                                (aref (gethash "proposals"
                                               *q45-submitted-captured*) 0))))
                     (and (string= "replied" (gethash "status" result ""))
                          (string= (gethash "pulse_id" *q45-manifest*)
                                   (gethash "pulse_id" proposal ""))
                          (null (search "provider-forged-pulse"
                                        (gethash "proposal_id" proposal "")))))))))

  (q45-reset)
  (let ((*conscious-conversation-model-call-fn*
          (lambda (&rest ignored)
            (declare (ignore ignored))
            (error "fixture provider transport rejected the request"))))
    (let* ((result
             (conscious-conversation-turn
              "Expose the transport failure."
              :endpoint "http://localhost:1234/v1/chat/completions"
              :model "fixture-model"))
           (failed-response
             (find "model-response" *q45-events*
                   :from-end t
                   :key (lambda (event) (gethash "type" event ""))
                   :test #'string=))
           (failure-payload
             (and failed-response (gethash "payload" failed-response))))
      (q45-check "provider transport failure returns a bounded diagnostic"
                 (and (string= "provider-call-failed"
                               (gethash "status" result ""))
                      (string= "provider-transport-failed"
                               (gethash "error_code" result ""))
                      (search "fixture provider transport rejected"
                              (gethash "reason" result ""))))
      (q45-check "durable failed response records only content-free diagnostics"
                 (and (hash-table-p failure-payload)
                      (string= "provider-transport-failed"
                               (gethash "failure_code" failure-payload ""))
                      (string= "simple-error"
                               (gethash "condition_type" failure-payload ""))
                      (null (gethash "failure_message" failure-payload))))))

  (q45-reset)
  (let ((invalid-response
          (obj "choices"
               (vector
                (obj "message" (obj "role" "assistant" "content" ""))))))
    (let ((*conscious-conversation-model-call-fn*
            (lambda (&rest ignored)
              (declare (ignore ignored))
              invalid-response)))
      (let ((result
              (conscious-conversation-turn
               "Expose the structural failure reason."
               :endpoint "http://localhost:1234/v1/chat/completions"
               :model "fixture-model")))
        (q45-check "proposal rejection returns a bounded structural diagnostic"
                   (and (string= "provider-response-invalid"
                                 (gethash "status" result ""))
                        (search "neither a tool call nor content"
                                (gethash "reason" result "")))))))

  (let* ((events
           (list
            (obj "id" 31 "type" "user-message" "agent_id" "q45-dev"
                 "payload"
                 (obj "text" "same persona"
                      "metadata"
                      (obj "source" "q4.5-conversation"
                           "persona_id" "fixture-a"
                           "persona_fingerprint" "prior-profile-revision")))
            (obj "id" 32 "type" "agent-message" "agent_id" "q45-dev"
                 "payload"
                 (obj "text" "different persona"
                      "metadata"
                      (obj "source" "q4.5-conversation"
                           "persona_id" "fixture-b"
                           "persona_fingerprint" "other-profile")))))
         (history
           (conscious-conversation-history
            events "q45-dev" :max-events 11 :character-budget 5001
            :event-character-limit 3001)))
    (q45-check "history survives profile revisions and isolates persona ids"
               (and (= 1 (length history))
                    (search "same persona"
                            (gethash "content" (aref history 0)))
                    (null (search "different persona"
                                  (shasht:write-json history nil))))))

  (q45-reset)
  (setf *q45-contract-violations* '("fixture-hard-prohibition")
        *q45-repaired-publication*
        "The durable analysis is complete and the supported conclusion is clear.")
  (let* ((calls 0)
        (*conscious-conversation-model-call-fn*
          (lambda (&rest ignored)
            (declare (ignore ignored))
            (incf calls)
            (q45-publication-response
             "The durable analysis is complete and the supported conclusion is clear. Let me know if there is anything else I can help with."))))
    (let ((result (conscious-conversation-turn
                   "Give me the supported conclusion."
                   :endpoint "http://localhost:1234/v1/chat/completions"
                   :model "fixture-model")))
      (q45-check "removal-only publication recovery uses no second model call"
                 (= calls 1))
      (q45-check "removable invalid fragment does not withhold safe speech"
                 (string= "replied" (gethash "status" result)))
      (q45-check "removal-only recovery preserves already-authored safe content"
                 (search "supported conclusion"
                         (gethash "content" result "")))
      (q45-check "removal-only recovery deletes the violating fragment"
                 (null (search "Let me know"
                               (gethash "content" result ""))))
      (q45-check "publication result reports generic removal-only validation"
                 (string= "removal-only"
                          (gethash "publication_validation" result "")))
      (q45-check "durable reply audits removal-only validation without rejected prose"
                 (let* ((event (find "agent-message" *q45-events*
                                     :key (lambda (row) (gethash "type" row))
                                     :test #'string=))
                        (metadata (and event
                                       (gethash "metadata"
                                                (gethash "payload" event)))))
                   (and (string= "removal-only"
                                 (gethash "publication_validation" metadata ""))
                        (equalp #("fixture-hard-prohibition")
                                (gethash "publication_original_violation_codes"
                                         metadata)))))))

  (q45-reset)
  (setf *q45-contract-violations* '("fixture-hard-prohibition"))
  (let ((*conscious-conversation-model-call-fn*
          (lambda (&rest ignored)
            (declare (ignore ignored))
            (q45-publication-response "Fully blocked fixture."))))
    (let ((result (conscious-conversation-turn
                   "Give the result."
                   :endpoint "http://localhost:1234/v1/chat/completions"
                   :model "fixture-model")))
      (q45-check "fully blocked publication still fails closed with codes"
                 (and (string= "withheld" (gethash "status" result))
                      (= 366 (gethash "total_tokens"
                                      (gethash "usage" result)))
                      (equalp #("fixture-hard-prohibition")
                              (gethash "violation_codes" result))
                      (null (find "agent-message" *q45-events*
                                  :key (lambda (row) (gethash "type" row))
                                  :test #'string=))))))

  (q45-reset)
  (let ((*conscious-conversation-model-call-fn*
          (lambda (&rest ignored)
            (declare (ignore ignored))
            (obj "choices"
                 (vector
                  (obj "message"
                       (obj "role" "assistant"
                            "content" "I should search for that file.")))))))
    (let ((result (conscious-conversation-turn
                   "Do you need a tool?" :endpoint "http://localhost:1234/v1/chat/completions"
                   :model "fixture-model")))
      (q45-check "prose resembling tool intent never creates a tool edge"
                 (and (string= "replied" (gethash "status" result))
                      (string= "publication-candidate"
                               (gethash "kind"
                                        (aref (gethash "proposals"
                                                       *q45-submitted-captured*) 0)
                                        ""))
                      (null (find "conscious-tool-operation-claimed"
                                  *q45-events*
                                  :key (lambda (event) (gethash "type" event))
                                  :test #'string=))))))

  (q45-reset)
  (let ((*conscious-conversation-max-output-tokens* 2048)
        (*conscious-conversation-model-call-fn*
          (lambda (&rest ignored)
            (declare (ignore ignored))
            (obj "choices"
                 (vector
                  (obj "message"
                       (obj "role" "assistant" "content" :null
                            "tool_calls"
                            (vector
                             (obj "id" "truncated-call" "type" "function"
                                  "function"
                                  (obj "name" "search-files"
                                       "arguments" "{"))))))
                 "usage"
                 (obj "prompt_tokens" 100
                      "completion_tokens"
                      *conscious-conversation-max-output-tokens*
                      "total_tokens"
                      (+ 100 *conscious-conversation-max-output-tokens*))))))
    (let ((result (conscious-conversation-turn
                   "long response" :endpoint "http://localhost:1234/v1/chat/completions"
                   :model "fixture-model")))
      (q45-check "cap exhaustion is reported as truncated native arguments"
                 (string= "provider-response-truncated"
                          (gethash "status" result)))))

  (q45-reset)
  (setf *q45-log-receipt-p* t)
  (let* ((interaction-id "interaction:q45-dev:fixture")
         (user-id
           (log-event
            "user-message"
            (obj "text" "already admitted" "channel" "web"
                 "metadata"
                 (obj "source" "q4.5-conversation"
                      "interaction_id" interaction-id)))))
    (let ((*conscious-conversation-model-call-fn*
            (lambda (&rest ignored)
              (declare (ignore ignored))
              (q45-publication-response "one reply"))))
      (let ((result
              (conscious-conversation-turn
               "already admitted"
               :endpoint "http://localhost:1234/v1/chat/completions"
               :model "fixture-model" :channel "web"
               :admitted-event-id user-id :interaction-id interaction-id)))
        (q45-check "coordinator execution reuses the exact admitted user event"
                   (and (string= "replied" (gethash "status" result))
                        (= 1 (count "user-message" *q45-events*
                                    :key (lambda (event)
                                           (gethash "type" event ""))
                                    :test #'string=))))
        (q45-check "coordinator pins captured cognition through its admitted event"
                   (equal user-id *q45-open-through-event-id*)))))

  (q45-reset)
  (q45-check "non-loopback providers are refused before admission"
             (handler-case
                 (progn (conscious-conversation-turn
                         "do not send" :endpoint "https://openrouter.ai/api/v1/chat/completions"
                         :model "remote") nil)
               (error () (null *q45-events*)))))

  (let* ((samples 0)
         (*conscious-conversation-memory-sample-fn*
          (lambda () (incf samples) (obj "status" "ok"))))
    (conscious-conversation-memory-boundary)
    (q45-check "post-turn memory boundary invokes the contained heap guard"
               (= 1 samples)))

  ;; Live CLI diagnostics are a content-free observer over the same measured
  ;; phase boundaries. They must report both entry and exit without changing
  ;; the value or failure behavior of the observed work.
  (let ((observed nil)
        (*conscious-conversation-turn-timing-ms*
          (%conversation-new-turn-timing)))
    (unwind-protect
         (progn
           (conscious-conversation-progress-configure
            (lambda (status phase elapsed-ms)
              (push (list status phase elapsed-ms) observed)))
           (q45-check "content-free progress observation preserves phase values"
                      (string= "phase-value"
                               (%conversation-time-phase
                                "provider" (lambda () "phase-value"))))
           (q45-check "content-free progress observation reports phase boundaries"
                      (and (find '("started" "provider" 0) observed
                                 :test #'equal)
                           (find-if
                            (lambda (entry)
                              (and (string= "completed" (first entry))
                                   (string= "provider" (second entry))
                                   (integerp (third entry))))
                            observed))))
      (conscious-conversation-progress-configure nil)))

;; Matrix item 0: the canonical production entry must exercise the same
;; coordinator, proposal executor, durable tool evidence, and publication
;; boundary that the CLI wires. Only the provider transport and captured
;; cognition adapter are contained fixtures here.
(format t "~%== canonical conversation work entry ==~%")
(load (test-source "policy.lisp"))
(load (test-source "stimulus.lisp"))
(load (test-source "boundary-outcome.lisp"))
(load (test-source "runtime-composition.lisp"))
(load (test-source "cognitive-work-executor.lisp"))
(load (merge-pathnames
       "src/mind/conscious/cognitive-operation-executor.lisp" *pai-root*))
(load (merge-pathnames
       "src/mind/conscious/conversation-work-loop.lisp" *pai-root*))

(defun q45-json-document (relative)
  (shasht:read-json
   (uiop:read-file-string (merge-pathnames relative *pai-root*))))

(defun q45-runtime-capabilities (work-profile)
  (let ((tools (make-hash-table :test #'equal))
        (proposals (make-hash-table :test #'equal)))
    (dolist (tool (coerce (gethash "permitted_tools" work-profile) 'list))
      (setf (gethash tool tools)
            (obj "consumer" "conscious-tool-operation-runtime"
                 "authority_class" "bounded-read-only"
                 "max_result_characters"
                 (gethash "max_tool_result_characters" work-profile))))
    (dolist (kind
             (coerce (gethash "permitted_proposal_kinds" work-profile) 'list))
      (setf (gethash kind proposals)
            (vector (if (string= kind "tool-call-proposal")
                        "cognitive-operation-executor"
                        "conversation-work-loop"))))
    (obj "tool_consumers" tools "proposal_consumers" proposals)))

(defun q45-tool-response (query)
  (obj
   "choices"
   (vector
    (obj "message"
         (obj "role" "assistant" "content" :null
              "tool_calls"
              (vector
               (obj "id" (format nil "provider-call-~a" query)
                    "type" "function"
                    "function"
                    (obj "name" "search-files"
                         "arguments"
                         (shasht:write-json
                          (obj "query" query
                               "path" "src/mind/conscious"
                               "max_results" 3)
                          nil)))))))
   "usage" (obj "prompt_tokens" 120 "completion_tokens" 40
                "total_tokens" 160)))

(q45-reset)
(setf *q45-log-receipt-p* t)
(setf (gethash "user-message" *stimulus-kind-map*)
      '("user-message" "channel" "interactive" t))
(let* ((contexts
         (gethash "profiles"
                  (q45-json-document "config/conscious-context-profiles.json")))
       (works
         (gethash "profiles"
                  (q45-json-document "config/conscious-work-profiles.json")))
       (context-profile (gethash "solicited-conversation-dev" contexts))
       (work-profile (gethash "interactive-dev" works))
       (plan
         (conscious-runtime-plan-compile
          context-profile work-profile
          (q45-runtime-capabilities work-profile)
          (obj "profile_id" "contained-provider" "revision" 1
               "max_requests" (gethash "max_model_calls" work-profile)
               "max_input_characters"
               (gethash "total_character_budget" context-profile))
          (obj "profile_id" "solicited-publication" "revision" 1
               "channels" (vector "terminal"))
          (obj "profile_id" "terminal-transport" "revision" 1
               "channel" "terminal")))
       (interaction-id "interaction:q45:canonical")
       (prompt "Find the durable conversation work entry point.")
       (root-id
         (log-event "user-message"
                    (obj "text" prompt "channel" "terminal"
                         "metadata"
                         (obj "source" "q4.5-conversation"
                              "interaction_id" interaction-id))))
       (provider-calls 0)
       (second-quantum-saw-tool-result nil)
       (second-quantum-saw-native-pair nil)
       (third-quantum-saw-both-tool-results nil))
  (q45-check "production persona budget admits identity and voice together"
             (>= (gethash "identity-instructions"
                          (gethash "section_character_budgets"
                                   context-profile))
                 5000))
  (conscious-runtime-plan-retain plan)
  (conscious-file-search-configure *pai-root*)
  ;; The executor owns a separate thread, so install the contained transport
  ;; at the process fixture seam rather than relying on a thread-local binding.
  (setf *conscious-conversation-model-call-fn*
        (lambda (messages endpoint model temperature)
          (declare (ignore endpoint model temperature))
          (incf provider-calls)
          (when (= provider-calls 2)
            (setf second-quantum-saw-tool-result
                  (search "conversation-work-loop.lisp"
                          (shasht:write-json messages nil))
                  second-quantum-saw-native-pair
                  (and (= 4 (length messages))
                       (string= "assistant"
                                (gethash "role" (third messages) ""))
                       (string= "tool"
                                (gethash "role" (fourth messages) ""))
                       (let* ((call
                                (aref (gethash "tool_calls"
                                               (third messages)) 0))
                              (call-id (gethash "id" call)))
                         (and (string= call-id
                                       (gethash "tool_call_id"
                                                (fourth messages) ""))
                              (null (search "provider-call" call-id)))))))
          (when (= provider-calls 3)
            (let ((rendered (shasht:write-json messages nil)))
              (setf third-quantum-saw-both-tool-results
                    (and (search "conversation-work-loop.lisp" rendered)
                         (search "conversation-runtime.lisp" rendered)))))
          (case provider-calls
            (1 (q45-tool-response "conscious-conversation-work-configure"))
            (2 (q45-tool-response "conscious-conversation-turn"))
            (otherwise
             (q45-publication-response
              "The durable conversation work entry point is implemented.")))))
  (conscious-conversation-work-configure
     :agent-id *agent-id* :profile work-profile :runtime-plan plan
     :turn-fn
     (lambda (turn-prompt &key admitted-event-id channel interaction-id work-id)
       (conscious-conversation-turn
        turn-prompt
        :endpoint "http://127.0.0.1:1234/v1/chat/completions"
        :model "fixture-model" :channel channel
        :budget-profile context-profile
        :admitted-event-id admitted-event-id :interaction-id interaction-id
        :work-id work-id)))
    (unwind-protect
         (progn
           (conscious-conversation-work-start)
           (let* ((result
                    (conscious-conversation-work-run
                     prompt :admitted-event-id root-id :channel "terminal"
                     :interaction-id interaction-id :timeout 20))
                  (projection (conscious-work-runtime-project))
                  (final-proposal
                    (and *q45-submitted-captured*
                         (aref (gethash "proposals"
                                        *q45-submitted-captured*) 0)))
                  (work
                    (loop for value being the hash-values of
                          (gethash "items" projection)
                          when (= root-id
                                  (parse-integer
                                   (aref (gethash "stimulus_ids" value) 0)
                                   :start 9))
                            return value)))
             (unless (and (string= "replied" (gethash "status" result ""))
                          (= 3 provider-calls))
               (format t "CANONICAL-DIAGNOSTIC result=~a calls=~d events=~a~%"
                       (shasht:write-json result nil) provider-calls
                       (mapcar (lambda (event) (gethash "type" event ""))
                               *q45-events*)))
             (q45-check "canonical entry completes a real many-quantum tool loop"
                        (and (string= "replied" (gethash "status" result ""))
                             (= 3 provider-calls)
                             (hash-table-p work)
                             (string= "completed" (gethash "state" work ""))))
             (q45-check "canonical entry carries durable tool output into quantum two"
                        second-quantum-saw-tool-result)
             (q45-check "canonical continuation reconstructs a runtime-owned native pair"
                        second-quantum-saw-native-pair)
             (q45-check "canonical entry accumulates both durable tool results"
                        third-quantum-saw-both-tool-results)
             (q45-check "canonical publication evidence includes both tool results"
                        (and (hash-table-p final-proposal)
                             (= 3 (length (gethash "evidence_event_ids"
                                                   final-proposal)))))
             (q45-check "canonical entry claims and terminalizes each real tool once"
                        (and (= 2 (count "conscious-tool-operation-claimed"
                                         *q45-events*
                                         :key (lambda (event)
                                                (gethash "type" event ""))
                                         :test #'string=))
                             (= 2 (count "conscious-tool-operation-result"
                                         *q45-events*
                                         :key (lambda (event)
                                                (gethash "type" event ""))
                                         :test #'string=))))
             (q45-check "canonical work is stamped with its retained plan hash"
                        (string= (conscious-runtime-plan-hash plan)
                                 (gethash "runtime_plan_hash" work "")))))
      (conscious-conversation-work-stop)))

;; Conversation egress consumes the wider candidate pool, refuses an
;; oversized unit whole, and continues to later evidence.  It also preserves
;; code-known speaker authority instead of treating every grounded row as an
;; operator assertion.
(let* ((long-content (make-string 700 :initial-element #\L))
       (projection
         (obj
          "relevant_shared_memory"
          (vector (obj "id" "legacy-only" "kind" "observation"
                       "content" "Legacy clipped row."
                       "origin_class" "lived-user"
                       "epistemic_status" "user-report"
                       "grounding_status" "grounded"))
          "relevant_shared_memory_candidates"
          (vector
           (obj "id" "too-long" "kind" "observation"
                "content" long-content "origin_class" "lived-user"
                "epistemic_status" "user-report"
                "grounding_status" "grounded")
           (obj "id" "assistant-source" "kind" "observation"
                "content" "An independently grounded assistant-side source."
                "origin_class" "external-source"
                "epistemic_status" "external-source"
                "grounding_status" "grounded")
           (obj "id" "operator-source" "kind" "observation"
                "content" "The operator reported the surviving detail."
                "origin_class" "lived-user"
                "epistemic_status" "user-report"
                "grounding_status" "grounded"))
          "memory_retrieval"
          (obj "candidate_count" 3 "eligible_count" 3
               "evidence_candidate_count" 3 "database_write_count" 0)))
       (selection
         (multiple-value-list
          (%conversation-render-memory-projection projection 3 400 1200)))
       (records (first selection))
       (report (third selection))
       (metadata (fourth selection)))
  (q45-check "memory renderer prefers the global candidate pool"
             (and (= 2 (length records))
                  (null (search "Legacy clipped row"
                                (shasht:write-json records nil)))))
  (q45-check "oversized semantic evidence is refused whole and later rows refill"
             (and (= 1 (gethash "budget_refusal_count" report))
                  (search "surviving detail"
                          (gethash "content" (aref records 1)))
                  (null (search long-content
                                (shasht:write-json records nil)))))
  (q45-check "semantic candidate metadata preserves operator speaker basis"
             (and (null (gethash "operator_support" (aref metadata 0)))
                  (eq t (gethash "operator_support" (aref metadata 1))))))

(format t "~%~d passed, ~d failed~%" *q45-passed* *q45-failed*)
(when (plusp *q45-failed*) (uiop:quit 1))
