;;;; Explicit durable selection through real storage, assembler and wire builder.
;;;; harness: full-system
(in-package :agent)
(defvar *sar-checks* 0)
(defmacro sar-check (form)
  `(progn (unless ,form (error "Activity runtime check ~d failed: ~s" (1+ *sar-checks*) ',form))
          (incf *sar-checks*)))
(defun sar-error-p (fn) (handler-case (progn (funcall fn) nil) (error () t)))
(defun sar-append (backend type payload &optional root)
  (storage-append-event backend type payload :agent-id "runtime-fixture"
                        :caused-by (or root :null) :occurred-at "2026-01-01T12:00:00Z"))
(defun sar-spec (root prompt)
  (let ((sections (make-hash-table :test #'equal)) (budgets (make-hash-table :test #'equal)))
    (dolist (name (%ca-section-names))
      (setf (gethash name sections) #() (gethash name budgets) 8000))
    (setf (gethash "identity-instructions" sections)
          (vector (%conversation-record "persona:fixture:identity" "Synthetic coding assistant")
                  (%conversation-record "persona:fixture:voice" "Be precise"))
          (gethash "triggering-stimuli" sections) (vector (%conversation-record root prompt))
          (gethash "conversation-evidence" sections)
          (vector (%conversation-record 1 "DUPLICATE-SHORT-HISTORY")))
    (obj "audience" "operator" "sections" sections "section_character_budgets" budgets
         "total_character_budget" 32000
         "eligible_evidence_ids" (vector "persona:fixture:identity" "persona:fixture:voice" root 1)
         "publication_constraints" (obj "audiences" #("operator"))
         "remaining_budget" (obj "tool_proposals" 30 "continuations" 30 "publication_candidates" 1))))

(let* ((path (merge-pathnames (format nil "runtime-activity-~d-~d-~a.sqlite3"
                                     (get-universal-time) (get-internal-real-time) (gensym))
                             (test-state-dir)))
       (backend (make-sqlite-storage path))
       (preview nil)
       (*conscious-conversation-persona-profile*
         (obj "persona_id" "fixture-persona" "fingerprint" "fixture" "revision" 1))
       (*conscious-recursive-mind-agent-id* "runtime-fixture")
       (*conscious-recursive-mind-endpoint* "http://localhost:1234/v1/chat/completions")
       (saved (mapcar (lambda (s) (cons s (symbol-function s)))
                      '(submit-stimulus %conversation-context-budget-profile %recursive-notify
                        %conscious-runtime-events %conscious-runtime-install-projections
                        %recursive-thread-events %conversation-assembly-spec %recursive-attach-recent-activity))))
  (unwind-protect
       (progn
         (sar-append backend "user-message"
                     (obj "text" "Fix the empty-field parser" "channel" "web"
                          "metadata" (obj "persona_id" "fixture-persona")))
         (sar-append backend "model-response"
                     (obj "status" "accepted" "model_call_id" "m:1"
                          "assistant_message"
                          (obj "content" :null "reasoning_content" "PRIVATE-REASONING"
                               "tool_calls" (vector (obj "id" "c:1" "type" "function"
                                                        "function" (obj "name" "bash" "arguments"
                                                                        "{\"command\":\"run-parser-tests\"}"))))) 1)
         (sar-append backend "recursive-tool-result"
                     (obj "model_call_id" "m:1" "tool_call_id" "c:1" "tool_name" "bash"
                          "execution_status" "executed"
                          "content" (concatenate 'string (make-string 4000 :initial-element #\x)
                                                 "FAILED-empty-field")) 1)
         (sar-append backend "agent-message" (obj "text" "The failing case is empty fields.") 1)
         (sar-append backend "sustained-activity-revised"
                     (obj "schema_version" 1 "activity_id" "work:parser" "persona_id" "fixture-persona"
                          "channel" "web" "resource_id" "fixture-conversation" "root_event_ids" #(1)
                          "evidence_event_ids" #(1) "previous_reference_event_id" :null
                          "actor" "operator" "reason" "Continue parser exercise" "retention_policy" "high-bandwidth"
                          "attention_state" "active" "completion_state" "open" "completion_confidence" :null) 1)
         (storage-prepare-activity-index backend)
         ;; Build tomorrow's request before admitting it, through read-only storage.
         (let* ((head (storage-head-position backend))
                (reader (progn (storage-close backend)
                               (make-sqlite-storage-read-only path)))
                (spec (sar-spec 6 "Continue; preserve whitespace too"))
                (original (%sac-copy spec)))
           (unwind-protect
                (labels ((preview-window (&rest options)
                           (apply #'conscious-recursive-preview-activity
                                  reader 5 5 spec (obj "state_revision" 1)
                                  "Continue; preserve whitespace too"
                                  :agent-id "runtime-fixture" :persona-id "fixture-persona"
                                  :channel "web" :thread-id "fixture:root"
                                  :model "fixture-model" options)))
                  (setf preview (preview-window :resource-id "fixture-conversation"))
                  (sar-check (equal "ready" (gethash "status" preview)))
                  (sar-check (= 8 (gethash "message_count" preview)))
                  (sar-check (equalp spec original))
                  (sar-check (= head (storage-head-position reader)))
                  (sar-check (equalp preview (preview-window :resource-id "fixture-conversation")))
                  (sar-check (equal "over-budget"
                                    (gethash "status" (preview-window :resource-id "fixture-conversation"
                                                                      :maximum-request-characters 1))))
                  (sar-check (sar-error-p (lambda () (preview-window :resource-id "unrelated")))))
             (storage-close reader)
             (setf backend (make-sqlite-storage path))))
         (%sqlite-authority-install backend path backend path "runtime-fixture")
         (sar-check (null (sustained-activity-for-admitted-root
                           nil "runtime-fixture" "fixture-persona" "web")))
         (let ((*event-authority-port* nil))
           (sar-check (sar-error-p (lambda () (sustained-activity-validate-selection
                                                5 "runtime-fixture" "fixture-persona" "web")))))
         (let ((*conscious-conversation-turn-history-report*
                 (obj "record_count" 3 "rendered_characters" 100 "estimated_tokens" 25)))
           (sustained-activity-replace-dialogue (sar-spec 1 "test"))
           (sar-check (= 0 (gethash "record_count" *conscious-conversation-turn-history-report*)))
           (sar-check (= 3 (gethash "superseded_record_count" *conscious-conversation-turn-history-report*))))
         ;; Isolate ingress and unrelated cognitive projections. Storage, activity
         ;; selection, final assembly, native message ordering and wire builder are real.
         (setf (symbol-function 'submit-stimulus)
               (lambda (prompt &key kind metadata)
                 (declare (ignore kind))
                 (values nil :accepted
                         (gethash "id" (sar-append backend "user-message"
                                                   (obj "text" prompt "channel" "web" "metadata" metadata)))))
               (symbol-function '%conversation-context-budget-profile)
               (lambda (&rest ignored) (declare (ignore ignored)) (obj "max_input_characters" 8000))
               (symbol-function '%recursive-notify)
               (lambda (&rest ignored) (declare (ignore ignored)) nil)
               (symbol-function '%conscious-runtime-events) (lambda () nil)
               (symbol-function '%conscious-runtime-install-projections)
               (lambda (&rest ignored) (declare (ignore ignored)) (obj "state_revision" 1))
               (symbol-function '%recursive-thread-events)
               (lambda () (storage-query-events backend :agent-id "runtime-fixture"))
               (symbol-function '%conversation-assembly-spec)
               (lambda (events id prompt &rest ignored)
                 (declare (ignore events ignored)) (sar-spec id prompt))
               (symbol-function '%recursive-attach-recent-activity)
               (lambda (&rest ignored) (declare (ignore ignored))
                 (error "Old snippet path must not run for a selected activity")))
         (let ((head (storage-head-position backend)))
           (sar-check (sar-error-p (lambda () (%recursive-operator-admit "wrong scope" "terminal"
                                                     :activity-reference-event-id 5))))
           (sar-check (= head (storage-head-position backend))))
         (multiple-value-bind (root ignored)
             (%recursive-operator-admit "Continue; preserve whitespace too" "web" :activity-reference-event-id 5)
           (declare (ignore ignored))
           (sar-check (= 5 (gethash "activity_reference_event_id"
                                   (gethash "metadata" (gethash "payload" (event-read-event root))))))
           (let* ((projection (obj "root_kind" "operator" "thread_id" "fixture:root"))
                  (opened (%recursive-open-model-context projection nil root "Continue; preserve whitespace too" "web"
                                                         :capture-inputs-p t))
                  (messages (%recursive-base-model-messages opened "Continue; preserve whitespace too" nil nil))
                  (wire (%conversation-http-request-payload messages "fixture-model" 0.3d0
                                                           "http://localhost:1234/v1/chat/completions"))
                  (sent (gethash "messages" wire))
                  (report (gethash "sustained_activity" (gethash "manifest" opened))))
             (let* ((snapshot (gethash "captured_assembly_inputs" opened))
                    (copy (%sac-copy snapshot))
                    (head (storage-head-position backend))
                    (rebuilt (conscious-recursive-preview-activity
                              backend 5 (gethash "through_event_id" (gethash "activity_coverage" copy))
                              (gethash "spec" copy) (gethash "state" copy) (gethash "prompt" copy)
                              :agent-id (gethash "agent_id" copy) :persona-id (gethash "persona_id" copy)
                              :channel (gethash "channel" copy) :resource-id "fixture-conversation"
                              :thread-id (gethash "thread_id" copy) :model "fixture-model")))
               (sar-check (equalp wire (gethash "wire_payload" rebuilt)))
               (sar-check (= head (storage-head-position backend)))
               (sar-check (= 1 (length (gethash "conversation-evidence"
                                              (gethash "sections" (gethash "spec" snapshot))))))
               (sar-check (equalp snapshot copy))
               (setf (gethash "prompt" copy) "Changed detached copy")
               (sar-check (not (equal (gethash "prompt" copy) (gethash "prompt" snapshot)))))
             (sar-check (equalp wire (gethash "wire_payload" preview)))
             (sar-check (equal "over-budget"
                               (gethash "status"
                                        (nth-value 1
                                          (%recursive-fit-working-request
                                           opened "Continue; preserve whitespace too" nil
                                           (list (obj "role" "assistant" "content" (make-string 200001 :initial-element #\x)))
                                           #() :profile nil)))))
             (sar-check (= 5 (gethash "through_event_id"
                                     (gethash "coverage" (gethash "sustained_activity"
                                                                  (gethash "manifest" preview))))))
             (sar-check (equalp #("system" "user" "user" "assistant" "tool" "assistant" "system" "user")
                                (map 'vector (lambda (m) (gethash "role" m)) sent)))
             (sar-check (search "Continue; preserve whitespace too" (gethash "content" (aref sent 7))))
             (sar-check (search "historical evidence" (gethash "content" (aref sent 6))))
             (sar-check (equal "Fix the empty-field parser" (gethash "content" (aref sent 2))))
             (sar-check (search "FAILED-empty-field" (gethash "content" (aref sent 4))))
             (sar-check (> (length (gethash "content" (aref sent 4))) 4000))
             (sar-check (search "run-parser-tests" (shasht:write-json (aref sent 3) nil)))
             (sar-check (not (search "DUPLICATE-SHORT-HISTORY" (shasht:write-json wire nil))))
             (sar-check (not (search "PRIVATE-REASONING" (shasht:write-json wire nil))))
             (sar-check (= 4 (gethash "message_count" report)))
             (sar-check (= 5 (gethash "reference_event_id" (gethash "coverage" report))))
             (let ((*conscious-recursive-mind-endpoint* "http://localhost:1234/api/v1/chat"))
               (sar-check (sar-error-p (lambda () (%recursive-base-model-messages
                                                   opened "Continue" nil nil)))))
             (let ((*event-authority-port* (copy-list *event-authority-port*)))
               (setf (getf *event-authority-port* :activity-read)
                     (lambda (&rest ignored) (declare (ignore ignored))
                       (values nil nil (obj "status" "read-limit-exceeded"))))
               (sar-check (sar-error-p (lambda () (%recursive-open-model-context
                                                   projection nil root "Continue" "web")))))
             (let* ((current (list (obj "role" "assistant" "content" "CURRENT-STEP")))
                    (continued (%recursive-base-model-messages opened "Continue; preserve whitespace too" nil current)))
               (sar-check (= 9 (length continued)))
               (sar-check (equal "CURRENT-STEP" (gethash "content" (car (last continued))))))
             ;; Re-read from original authority after reopen, not from OPENED.
             (event-authority-clear)
             (setf backend (make-sqlite-storage-read-only path))
             (%sqlite-authority-install backend path backend path "runtime-fixture")
             (let ((again (%recursive-open-model-context projection nil root "Continue; preserve whitespace too" "web")))
               (sar-check (equalp (gethash "sustained_activity" opened) (gethash "sustained_activity" again))))
             (let ((bad (%sac-copy (gethash "sustained_activity" opened))))
               (setf (gethash "status" bad) "compaction-required")
               (sar-check (sar-error-p (lambda () (sustained-activity-native-messages bad))))))))
    (dolist (entry saved) (setf (symbol-function (car entry)) (cdr entry)))
    (event-authority-clear)
    (storage-close backend)))
(format t "Sustained activity runtime: ~d passed, 0 failed~%" *sar-checks*)


