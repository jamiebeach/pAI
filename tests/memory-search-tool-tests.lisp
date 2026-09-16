(in-package :agent)

(defvar *memory-search-tool-test-pass* 0)
(defvar *memory-search-tool-test-fail* 0)
(defvar *memory-search-tool-test-writes* 0)
(defvar *tools* (vector))
(defvar *turn-capture-context* nil)
(defvar *publication-contract-current* nil)

(defun memory-search-tool-test-check (name condition)
  (if condition
      (progn (incf *memory-search-tool-test-pass*)
             (format t "  ok   ~a~%" name))
      (progn (incf *memory-search-tool-test-fail*)
             (format t "  FAIL ~a~%" name))))

(defparameter *context-projection-memory-candidate-results* 50)
(defparameter *context-projection-max-shared-memory-results* 3)
(defparameter *context-projection-memory-similarity-floor* 0.52d0)
(defvar *memory-search-tool-test-rows* nil)
(defvar *memory-search-tool-test-k* nil)
(defvar *memory-search-tool-test-as-of* nil)
(defvar *memory-search-tool-test-bundle-anchors* nil)
(defvar *memory-search-tool-test-strategy* nil)
(defvar *memory-search-tool-test-excluded-turns* nil)
(defvar *memory-search-tool-test-excluded-events* nil)

(defun conversation-unsealed-dialogue-context-records
    (events agent-id persona-id query &key maximum character-budget
                                      record-character-limit)
  (declare (ignore agent-id persona-id maximum character-budget
                   record-character-limit))
  (if (and events (search "campfire" query :test #'char-equal))
      (values
       (vector
        (obj "source_id" "conversation-raw:33493:33496"
             "content"
             "operator smelled like campfire smoke; assistant said go smell less like campfire"))
       '("conversation-raw:33493:33496")
       (obj "schema_version" 1 "status" "selected"
            "sealed_through_event_id" 33337
            "pending_episode_count" 1
            "rendered_characters" 84 "database_write_count" 0))
      (values (vector) nil
              (obj "schema_version" 1 "status" "empty"
                   "sealed_through_event_id" 33337
                   "pending_episode_count" 1
                   "rendered_characters" 0 "database_write_count" 0))))

(defun conversation-episode-project (events agent-id persona-id)
  (declare (ignore events agent-id persona-id))
  (vector))

(defun conversation-episode-context-records
    (episodes query &key maximum character-budget record-character-limit)
  (declare (ignore episodes query maximum character-budget
                   record-character-limit))
  (values (vector) nil (obj "rendered_characters" 0)))

(defun memory-search (query &key k mode as-of candidate-strategy
                                  exclude-turn-ids exclude-source-event-ids)
  (declare (ignore query mode))
  (setf *memory-search-tool-test-k* k
        *memory-search-tool-test-as-of* as-of
        *memory-search-tool-test-strategy* candidate-strategy
        *memory-search-tool-test-excluded-turns* exclude-turn-ids
        *memory-search-tool-test-excluded-events* exclude-source-event-ids)
  (let ((rows
          (remove-if
           (lambda (row)
             (let ((metadata (gethash "epistemic_metadata" row)))
               (or (member (and (hash-table-p metadata)
                                (gethash "turn_id" metadata))
                           exclude-turn-ids :test #'string=)
                   (member (gethash "source_event_id" row)
                           exclude-source-event-ids :test #'string=))))
           *memory-search-tool-test-rows*)))
    (values rows
            (obj "strategy" "hybrid-explicit"
                 "semantic_candidate_count" 4
                 "lexical_candidate_count" 3
                 "union_candidate_count" (length rows)
                 "lexical_terms" (vector "bedtime" "routine")
                 "database_write_count" 0))))

(defun %context-projection-eligible-memory-p (row)
  (and (hash-table-p row)
       (string= "grounded" (gethash "grounding_status" row ""))))

(defun %context-projection-direct-memory-p (row)
  (member (gethash "origin_class" row "")
          '("lived-user" "lived-agent-action" "tool-result" "external-source")
          :test #'string=))

(defun %context-projection-select-shared-memory (rows)
  (let ((ranked (stable-sort (copy-list rows) #'>
                             :key (lambda (row)
                                    (gethash "similarity" row 0.0d0)))))
    (subseq ranked 0 (min (length ranked)
                          *context-projection-max-shared-memory-results*))))

(defun %context-projection-turn-bundles (query anchors)
  (declare (ignore query))
  (setf *memory-search-tool-test-bundle-anchors* anchors)
  (values
   (loop for row in anchors
         for id = (gethash "id" row)
         when (member id '("turn-bedtime" "turn-appearance" "turn-children")
                      :test #'string=)
           collect
           (obj "id" (format nil "turn-bundle:~a" id)
                "kind" "turn-bundle" "content" (gethash "content" row)
                "origin_class" "derived-lived"
                "epistemic_status" "grounded-turn-bundle"
                "grounding_status" "grounded"
                "observed_at" (or (gethash "observed_at" row)
                                  (gethash "created_at" row))
                "valid_from" (gethash "valid_from" row :null)
                "valid_to" (gethash "valid_to" row :null)
                "supersedes_node_id" (gethash "supersedes_node_id" row :null)
                "turn_id" id "member_roles" (vector "user")
                "evidence_node_ids" (vector id)
                "similarity" (gethash "similarity" row)
                "retrieval_score" (gethash "similarity" row)
                "candidate_sources" (gethash "candidate_sources" row (vector))
                "lexical_tier" (gethash "lexical_tier" row 0)
                "lexical_match_count" (gethash "lexical_match_count" row 0)
                "lexical_coverage" (gethash "lexical_coverage" row 0.0d0)
                "lexical_terms" (gethash "lexical_terms" row (vector))))
   0 2 2 :bundled))

(defun execute (tool-call)
  (obj "role" "tool" "tool_call_id" (gethash "id" tool-call)
       "content" "base-executor"))

(load (test-source "recall-selection.lisp"))
(load (test-source "memory-search-tool.lisp"))

(setf *memory-search-tool-test-rows*
      (list
       (obj "id" "turn-current" "content" "Remember our bedtime routine"
            "origin_class" "lived-user" "epistemic_status" "user-report"
            "grounding_status" "grounded" "similarity" 0.99d0
            "created_at" "2026-08-07T15:59:00Z"
            "source_event_id" "event-current-user"
            "epistemic_metadata"
            (obj "turn_id" "turn-memory-test" "role" "user"))
       (obj "id" "system-secret" "content" "# Operational Constitution private"
            "origin_class" "lived-user" "epistemic_status" "user-report"
            "grounding_status" "grounded" "similarity" 0.98d0)
       (obj "id" "turn-bedtime" "content"
            "Remember our bedtime routine: the operator likes chatting with the agent before going to sleep."
            "origin_class" "lived-user" "epistemic_status" "user-report"
            "grounding_status" "grounded" "similarity" 0.87d0
            "candidate_sources" (vector "semantic" "lexical")
            "lexical_tier" 1 "lexical_match_count" 2
            "lexical_coverage" 1.0d0
            "lexical_terms" (vector "bedtime" "routine")
            "created_at" "2026-08-06T22:00:00Z"
            "observed_at" "2026-08-06T21:59:00Z"
            "valid_from" "2026-08-06T21:59:00Z" "valid_to" :null
            "supersedes_node_id" :null)
       (obj "id" "unsafe" "content" "An ungrounded guess."
            "origin_class" "generated-cognition" "epistemic_status" "hypothesis"
            "grounding_status" "unclassified" "similarity" 0.95d0)
       (obj "id" "turn-appearance" "content"
            "the operator described the agent with blonde hair and blue eyes."
            "origin_class" "lived-user" "epistemic_status" "user-report"
            "grounding_status" "grounded" "similarity" 0.82d0
            "created_at" "2026-08-05T12:00:00Z")
       (obj "id" "turn-children" "content"
            "The operator reported that Child-A and Child-B are their children."
            "origin_class" "lived-user" "epistemic_status" "user-report"
            "grounding_status" "grounded" "similarity" 0.91d0
            "lexical_tier" 1 "lexical_match_count" 1
            "lexical_coverage" 0.5d0 "created_at" "2026-01-03T12:00:00Z")))

(let* ((*turn-capture-context*
         (obj "origin" "conversation" "turn_id" "turn-memory-test"
              "user_event_id" "event-current-user"
              "as_of" (encode-universal-time 0 0 16 7 8 2026 0)))
       (*publication-contract-current*
         (obj "interaction_mode" "ordinary-reply"))
       (call (obj "id" "call-memory-1" "function"
                  (obj "name" "search-memory"
                       "arguments"
                       (shasht:write-json
                        (obj "query" "Remember our bedtime routine"
                             "limit" 2) nil))))
       (result (execute call))
       (port-result (memory-search-tool-handle call))
       (content (shasht:read-json (gethash "content" result)))
       (rows (coerce (gethash "results" content) 'list)))
  (memory-search-tool-test-check "tool is registered"
                                 (find "search-memory" *tools*
                                       :key (lambda (tool)
                                              (ref tool "function" "name"))
                                       :test #'string=))
  (memory-search-tool-test-check
   "callable handler port preserves wrapper result"
   (and (string= (gethash "role" result) (gethash "role" port-result))
        (string= (gethash "tool_call_id" result)
                 (gethash "tool_call_id" port-result))
        (string= (gethash "content" result) (gethash "content" port-result))))
  (let ((tool (find "search-memory" *tools*
                    :key (lambda (candidate)
                           (ref candidate "function" "name"))
                    :test #'string=)))
    (memory-search-tool-test-check
     "tool description forbids unknown-current promotion"
     (search "null validity means current validity is unknown"
             (ref tool "function" "description"))))
  (memory-search-tool-test-check "tool overfetches semantic candidates"
                                 (= 50 *memory-search-tool-test-k*))
  (memory-search-tool-test-check "tool requests explicit hybrid candidates"
                                 (eq :hybrid-explicit
                                     *memory-search-tool-test-strategy*))
  (memory-search-tool-test-check "bounded result limit is honored"
                                 (= 2 (gethash "selected_count" content)))
  (memory-search-tool-test-check "complete serialized result honors its character budget"
                                 (and
                                  (<= (length (gethash "content" result))
                                      *search-memory-result-character-budget*)
                                  (= (length (gethash "content" result))
                                     (gethash "result_characters" content))))
  (memory-search-tool-test-check
   "tool uses and exposes the code-owned turn clock"
   (and (string= "2026-08-07T16:00:00Z"
                 *memory-search-tool-test-as-of*)
        (string= *memory-search-tool-test-as-of*
                 (gethash "as_of" content))))
  (memory-search-tool-test-check
   "current turn and source event are excluded by stable identity"
   (and (equal '("turn-memory-test")
               *memory-search-tool-test-excluded-turns*)
        (equal '("event-current-user")
               *memory-search-tool-test-excluded-events*)
        (not (find "turn-current" *memory-search-tool-test-bundle-anchors*
                   :key (lambda (row) (gethash "id" row)) :test #'string=))))
  (memory-search-tool-test-check "system prompt material is excluded"
                                 (not (find "system-secret"
                                            *memory-search-tool-test-bundle-anchors*
                                            :key (lambda (row) (gethash "id" row))
                                            :test #'string=)))
  (memory-search-tool-test-check "grounded bedtime evidence is returned"
                                 (find "turn-bundle:turn-bedtime" rows
                                       :key (lambda (row) (gethash "id" row))
                                       :test #'string=))
  (memory-search-tool-test-check "unrelated appearance evidence is not filler"
                                 (not (find "turn-bundle:turn-appearance" rows
                                            :key (lambda (row) (gethash "id" row))
                                            :test #'string=)))
  (let ((bedtime (find "turn-bundle:turn-bedtime" rows
                       :key (lambda (row) (gethash "id" row))
                       :test #'string=)))
    (memory-search-tool-test-check
     "tool preserves observation validity and supersession provenance"
     (and bedtime
          (string= "2026-08-06T21:59:00Z"
                   (gethash "observed_at" bedtime))
          (string= "2026-08-06T21:59:00Z"
                   (gethash "valid_from" bedtime))
          (eq :null (gethash "valid_to" bedtime))
          (eq :null (gethash "supersedes_node_id" bedtime))
          (equal '("user")
                 (coerce (gethash "member_roles" bedtime) 'list))
          (equal '("turn-bedtime")
                 (coerce (gethash "evidence_node_ids" bedtime) 'list))
          (equal '("semantic" "lexical")
                 (coerce (gethash "candidate_sources" bedtime) 'list))
          (= 1 (gethash "lexical_tier" bedtime)))))
  (memory-search-tool-test-check "adapter reports zero database writes"
                                 (and (zerop (gethash "database_write_count" content))
                                      (zerop *memory-search-tool-test-writes*))))

(let* ((*turn-capture-context* (obj "origin" "initiative"))
       (*publication-contract-current* (obj "interaction_mode" "ordinary-reply"))
       (call (obj "id" "call-unauthorized" "function"
                  (obj "name" "search-memory"
                       "arguments" "{\"query\":\"bedtime\"}")))
       (result (execute call)))
  (memory-search-tool-test-check "non-conversation origin fails closed"
                                 (search "only inside an authorized recursive cognition boundary"
                                         (gethash "content" result))))

(let* ((*turn-capture-context* nil)
       (*publication-contract-current* nil)
       (*search-memory-recursive-ordinary-reply-authorized-p* t)
       (call (obj "id" "call-recursive-public" "function"
                  (obj "name" "search-memory"
                       "arguments" "{\"query\":\"bedtime\",\"limit\":1}")))
       (result (memory-search-tool-handle call)))
  (memory-search-tool-test-check
   "recursive public boundary can authorize bounded memory search"
   (let ((content (shasht:read-json (gethash "content" result))))
     (= 1 (gethash "selected_count" content)))))

(let* ((*turn-capture-context* nil)
       (*publication-contract-current* nil)
       (*search-memory-recursive-private-cognition-authorized-p* t)
       (call (obj "id" "call-recursive-private" "function"
                  (obj "name" "search-memory"
                       "arguments" "{\"query\":\"bedtime\",\"limit\":1}")))
       (result (memory-search-tool-handle call)))
  (memory-search-tool-test-check
   "recursive private boundary has the same bounded memory search"
   (let ((content (shasht:read-json (gethash "content" result))))
     (= 1 (gethash "selected_count" content)))))

(let* ((*turn-capture-context* nil)
       (*publication-contract-current* nil)
       (*search-memory-recursive-ordinary-reply-authorized-p* t)
       (*search-memory-conversation-events* (list (obj "id" 33493)))
       (*search-memory-conversation-agent-id* "fixture-agent")
       (*search-memory-conversation-persona-id* "fixtureagent")
       (call (obj "id" "call-recent-dialogue" "function"
                  (obj "name" "search-memory"
                       "arguments"
                       "{\"query\":\"campfire smoke\",\"limit\":3}")))
       (result (memory-search-tool-handle call))
       (content (shasht:read-json (gethash "content" result)))
       (coverage (gethash "conversation_coverage" content)))
  (memory-search-tool-test-check
   "recursive memory search includes recent unsealed dialogue and frontier"
   (and (= 1 (gethash "conversation_result_count" content))
        (= 33337 (gethash "sealed_through_event_id" coverage))
        (= 1 (gethash "pending_episode_count" coverage))
        (search "go smell less like campfire"
                (gethash "content"
                         (aref (gethash "conversation_results" content) 0))
                :test #'char-equal))))

(let* ((*turn-capture-context* nil)
       (*publication-contract-current* nil)
       (*search-memory-recursive-ordinary-reply-authorized-p* t)
       (*search-memory-conversation-events* (list (obj "id" 33493)))
       (*search-memory-conversation-agent-id* "fixture-agent")
       (*search-memory-conversation-persona-id* "fixtureagent")
       (content
         (shasht:read-json
          (search-memory "campfire smoke" :limit 1))))
  (memory-search-tool-test-check
   "one result limit is shared by dialogue and semantic memory"
   (and (= 1 (gethash "conversation_result_count" content))
        (= 1 (gethash "selected_count" content))
        (zerop (gethash "semantic_result_count" content))
        (zerop (length (gethash "results" content)))
        (= 1 (+ (gethash "conversation_result_count" content)
                (gethash "semantic_result_count" content))))))

(let ((original-raw (fdefinition 'conversation-unsealed-dialogue-context-records)))
  (unwind-protect
       (progn
         (setf (fdefinition 'conversation-unsealed-dialogue-context-records)
               (lambda (&rest ignored)
                 (declare (ignore ignored))
                 (values
                  (vector
                   (obj "source_id" "conversation-raw:echo-1"
                        "content" "operator asked for children's names; assistant found none")
                   (obj "source_id" "conversation-raw:echo-2"
                        "content" "operator repeated the children's names question")
                   (obj "source_id" "conversation-raw:echo-3"
                        "content" "assistant discussed searching for children's names"))
                  '("conversation-raw:echo-1" "conversation-raw:echo-2"
                    "conversation-raw:echo-3")
                  (obj "schema_version" 1 "status" "selected"
                       "rendered_characters" 160 "database_write_count" 0))))
         (let* ((*turn-capture-context* nil)
                (*publication-contract-current* nil)
                (*search-memory-recursive-ordinary-reply-authorized-p* t)
                (*search-memory-conversation-events* (list (obj "id" 1)))
                (*search-memory-conversation-agent-id* "fixture-agent")
                (*search-memory-conversation-persona-id* "fixture-persona")
                (content (shasht:read-json
                          (search-memory "What are my children's names?"
                                         :limit 1))))
           (memory-search-tool-test-check
            "dense recent recall echoes cannot starve the historical positive"
            (and (= 1 (gethash "selected_count" content))
                 (= 1 (gethash "semantic_result_count" content))
                 (string= "turn-bundle:turn-children"
                          (gethash "id" (aref (gethash "results" content) 0)))))))
    (setf (fdefinition 'conversation-unsealed-dialogue-context-records)
          original-raw)))

(let* ((call (obj "id" "call-base" "function"
                  (obj "name" "some-other-tool" "arguments" "{}")))
       (result (execute call)))
  (memory-search-tool-test-check "nonmatching calls fall through"
                                 (string= "base-executor"
                                          (gethash "content" result))))

(let* ((call (obj "id" "call-invalid" "function"
                  (obj "name" "search-memory"
                       "arguments"
                       (shasht:write-json
                        (obj "query" "anything" "limit" 6) nil))))
       (result (execute call)))
  (memory-search-tool-test-check "invalid result limit fails closed"
                                 (search "ERROR: memory search unavailable"
                                         (gethash "content" result))))

(let ((original (fdefinition 'memory-search)))
  (unwind-protect
       (progn
         (setf (fdefinition 'memory-search)
               (lambda (&rest args)
                 (declare (ignore args))
                 (error "fixture backend unavailable")))
         (let* ((*turn-capture-context* (obj "origin" "conversation"))
                (*publication-contract-current*
                  (obj "interaction_mode" "ordinary-reply"))
                (call (obj "id" "call-unavailable" "function"
                           (obj "name" "search-memory"
                                "arguments" "{\"query\":\"earlier discussion\"}")))
                (result (execute call)))
           (memory-search-tool-test-check "retrieval outage returns bounded tool error"
                                          (search "ERROR: memory search unavailable"
                                                  (gethash "content" result)))))
    (setf (fdefinition 'memory-search) original)))

(let ((report (memory-search-tool-report))
      (source (uiop:read-file-string (namestring (test-source "memory-search-tool.lisp")))))
  (memory-search-tool-test-check "report declares read-only behavior"
                                 (and (gethash "read_only" report)
                                      (null (gethash "delivery_authority" report))))
  (memory-search-tool-test-check "tool has no outbound delivery primitive"
                                 (and (null (search "send-message" source
                                                    :test #'char-equal))
                                      (null (search "telegram-send" source
                                                    :test #'char-equal)))))

(format t "~%MEMORY-SEARCH-TOOL TESTS: ~a passed, ~a failed.~%"
        *memory-search-tool-test-pass* *memory-search-tool-test-fail*)
(when (plusp *memory-search-tool-test-fail*) (sb-ext:exit :code 1))
