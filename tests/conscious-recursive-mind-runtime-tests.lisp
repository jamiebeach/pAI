;;;; conscious-recursive-mind-runtime-tests.lisp -- durable conversation/tools.
;;;; harness: full-system

(in-package :agent)

(defvar *crm-pass* 0)
(defvar *crm-fail* 0)
(defvar *crm-notifications* nil)

(defun crm-check (name condition)
  (if condition
      (progn (incf *crm-pass*) (format t "PASS ~a~%" name))
      (progn (incf *crm-fail*) (format t "FAIL ~a~%" name))))

(defun crm-event (id type payload &optional caused-by)
  (obj "id" id "type" type "agent_id" "recursive-fixture"
       "caused_by" (or caused-by :null) "payload" payload))

(format t "~%== durable recursive mind ==~%")

;; Recursive capability prose must not inherit the conversation-only fallback.
;; Keep this before the suite's runtime doubles are installed.
(let* ((old (vector (%conversation-record
                     "q45:schema" "No native tool is available for this model request.")))
       (sections (obj "tools-proposal-schema" old
                      "triggering-stimuli" (vector)))
       (replacement (%recursive-capability-sections sections))
       (encoded (shasht:write-json replacement nil)))
  (crm-check "recursive capability section removes false conversation denial"
             (not (search "No native tool is available for this model request." encoded)))
  (crm-check "recursive capability section defers to current attached schemas"
             (search "schemas attached to the current model request are authoritative" encoded))
  (crm-check "recursive capability section permits no tools when schemas absent"
             (search "If no schemas are attached, no native tool is available" encoded))
  (crm-check "recursive capability section respects final synthesis"
             (search "Runtime final-synthesis instructions close tool use" encoded))
  (crm-check "recursive capability section requires execution evidence"
             (search "matching runtime tool-result receipt" encoded))
  (crm-check "recursive capability section preserves original conversation spec"
             (eq old (gethash "tools-proposal-schema" sections)))
  (crm-check "recursive capability section preserves unrelated sections"
             (eq (gethash "triggering-stimuli" sections)
                 (gethash "triggering-stimuli" replacement))))

(let* ((root (crm-event
              70001 "agent-stimulus-received"
              (obj "source" "fleet-board" "text" "Synthetic peer question"
                   "environment"
                   (obj "kind" "fleet-board" "owner_id" "board-owner-fixture"
                        "resource_id" "thread:local")
                   "details"
                   (obj "receipt_event_id" 70000 "sender_id" "peer:fixture"))
              70000))
       (completed (crm-event 70002 "recursive-stimulus-result"
                             (obj "status" "completed" "content" "I replied.")
                             70001))
       (execution (crm-event
                   70003 "recursive-tool-execution"
                   (obj "tool_call_id" "call:local"
                        "tool_name" "reply-fleet-board-message"
                        "tool_arguments"
                        (obj "thread_id" "thread:local"
                             "reply_to" "message:parent"
                             "text" "Synthetic response")) 70001))
       (tool-result (crm-event
                     70004 "recursive-tool-result"
                     (obj "tool_call_id" "call:local"
                          "tool_name" "reply-fleet-board-message"
                          "execution_status" "executed"
                          "content" "Replied on this agent's board.") 70001)))
  (crm-check "model claim alone is absorbed, not replied"
             (equal "absorbed"
                    (%recursive-peer-bridge-disposition
                     (list root completed) root)))
  (crm-check "successful same-board tool evidence proves reply"
             (equal "replied"
                    (%recursive-peer-bridge-disposition
                     (list root execution tool-result completed) root)))
  (setf (gethash "content" (gethash "payload" tool-result))
        "ERROR: synthetic timeout")
  (crm-check "error tool result cannot prove reply"
             (equal "publication-unverified"
                    (%recursive-peer-bridge-disposition
                     (list root execution tool-result completed) root)))
  (setf (gethash "content" (gethash "payload" tool-result))
        "Posted elsewhere"
        (gethash "tool_name" (gethash "payload" tool-result))
        "post-fleet-message")
  (crm-check "wrong board publication cannot prove reply"
             (equal "publication-unverified"
                    (%recursive-peer-bridge-disposition
                     (list root execution tool-result completed) root))))

(let* ((root-id 70101)
       (call-id "call:remote")
       (operation-id (%recursive-fleet-operation-id root-id call-id))
       (root (crm-event
              root-id "agent-stimulus-received"
              (obj "source" "fleet-board" "text" "Synthetic remote reply"
                   "environment"
                   (obj "kind" "fleet-board" "owner_id" "peer:fixture"
                        "resource_id" "thread:peer")
                   "details"
                   (obj "receipt_event_id" 70100 "sender_id" "peer:fixture"))
              70100))
       (completed (crm-event 70102 "recursive-stimulus-result"
                             (obj "status" "completed") root-id))
       (execution (crm-event
                   70103 "recursive-tool-execution"
                   (obj "tool_call_id" call-id "tool_name" "post-fleet-message"
                        "tool_arguments"
                        (obj "peer_id" "peer:fixture" "text" "Reply on peer board"
                             "thread_id" "thread:peer" "reply_to" "parent"))
                   root-id))
       (tool-result (crm-event
                     70104 "recursive-tool-result"
                     (obj "tool_call_id" call-id "tool_name" "post-fleet-message"
                          "execution_status" "executed"
                          "content" "Posted to peer board") root-id))
       (intent (crm-event
                70105 "peer-board-publication-intent"
                (obj "operation_id" operation-id "peer_id" "peer:fixture"
                     "request" (obj "thread_id" "thread:peer"
                                    "text" "Reply on peer board")))))
  (crm-check "remote post without frozen target is not a proven reply"
             (equal "publication-unverified"
                    (%recursive-peer-bridge-disposition
                     (list root execution tool-result completed) root)))
  (crm-check "remote post to exact sender thread is a proven reply"
             (equal "replied"
                    (%recursive-peer-bridge-disposition
                     (list root execution tool-result intent completed) root)))
  (setf (gethash "thread_id"
                 (gethash "request" (gethash "payload" intent)))
        "thread:other")
  (crm-check "remote post to another thread is not a proven reply"
             (equal "publication-unverified"
                    (%recursive-peer-bridge-disposition
                     (list root execution tool-result intent completed) root))))

;; The full-system harness above makes the production composition explicit;
;; this reload preserves the suite's historical subject boundary and allows
;; its later impure-adapter fixtures to replace only named edges.
(load (test-source "recursive-mind-runtime.lisp"))

(let* ((root (crm-event 61001 "agent-stimulus-received"
                        (obj "source" "synthetic-document-change"
                             "text" "A synthetic document changed."
                             "environment"
                             (obj "kind" "document" "owner_id" "fixture"
                                  "resource_id" "one")
                             "authority" "private-cognition-existing-authority")))
       (descriptor (%recursive-root-descriptor (list root) 61001
                                               "recursive-fixture")))
  (crm-check "generic adapter input opens a private stimulus root"
             (and (string= "stimulus" (gethash "kind" descriptor ""))
                  (string= "private" (gethash "channel" descriptor ""))
                  (string= "thread:stimulus:recursive-fixture:61001"
                           (gethash "thread_id" descriptor ""))
                  (search "synthetic-document-change"
                          (gethash "prompt" descriptor ""))))
  (setf (gethash "authority" (gethash "payload" root)) "operator")
  (crm-check "generic adapter input cannot forge private-root authority"
             (handler-case
                 (progn (%recursive-root-descriptor (list root) 61001
                                                    "recursive-fixture")
                        nil)
               (error () t))))

(let* ((root (crm-event 61100 "agent-stimulus-received"
                        (obj "source" "synthetic-task-result"
                             "text" "A synthetic task completed."
                             "authority" "private-cognition-existing-authority")))
       (thread "thread:stimulus:recursive-fixture:61100")
       (request (crm-event 61101 "model-request"
                           (obj "thread_id" thread "model_call_id" "model:stimulus")
                           61100))
       (response (crm-event 61102 "model-response"
                            (obj "thread_id" thread "model_call_id" "model:stimulus"
                                 "status" "accepted"
                                 "assistant_message"
                                 (obj "role" "assistant"
                                      "content" "The synthetic result needs no action."))
                            61100))
       (result (crm-event 61103 "recursive-stimulus-result"
                          (obj "thread_id" thread "model_call_id" "model:stimulus"
                               "content" "The synthetic result needs no action."
                               "status" "completed" "audience" "private")
                          61100))
       (events (list root request response)))
  (crm-check "in-flight provider outcome is parked from autonomous selection"
             (null (%recursive-pending-private-stimuli
                    (list root request) "recursive-fixture")))
  (crm-check "unfinished generic stimulus is selected from ledger"
             (equal (list root)
                    (%recursive-pending-private-stimuli
                     events "recursive-fixture")))
  (crm-check "generic selection is isolated by agent identity"
             (null (%recursive-pending-private-stimuli
                    events "another-fixture")))
  (crm-check "accepted generic stimulus reaches the private result boundary"
             (equal "private-ready"
                    (gethash "state"
                             (conscious-recursive-thread-project
                              events 61100 "recursive-fixture"))))
  (setf events (append events (list result)))
  (crm-check "completed generic stimulus cannot be selected again"
             (null (%recursive-pending-private-stimuli
                    events "recursive-fixture")))
  (crm-check "durable generic stimulus result projects to done after restart"
             (let ((projection (conscious-recursive-thread-project
                                events 61100 "recursive-fixture")))
               (and (equal "done" (gethash "state" projection))
                    (= 61103 (gethash "agent_event_id" projection)))))
  (setf (gethash "content" (gethash "payload" result)) "Wrong result")
  (crm-check "generic stimulus projection rejects content mismatches"
             (handler-case
                 (progn (conscious-recursive-thread-project
                         events 61100 "recursive-fixture") nil)
               (error () t))))

(let* ((root (crm-event 61200 "agent-stimulus-received"
                        (obj "source" "synthetic-task-result"
                             "text" "A synthetic task completed."
                             "authority" "private-cognition-existing-authority")))
       (events (list root))
       (calls nil)
       (old-events (symbol-function '%recursive-thread-events))
       (old-run (symbol-function '%recursive-run-root-locked))
       (*conscious-recursive-mind-agent-id* "recursive-fixture")
       (*conscious-recursive-mind-operator-pending-p* nil))
  (unwind-protect
       (progn
         (setf (symbol-function '%recursive-thread-events)
               (lambda () events)
               (symbol-function '%recursive-run-root-locked)
               (lambda (id interaction-id &key background-p)
                 (push (list id interaction-id background-p) calls)
                 (obj "status" "stimulus-completed")))
         (crm-check "quiet stimulus executor advances one retained root"
                    (and (equal "stimulus-completed"
                                (gethash "status"
                                         (conscious-recursive-stimulus-one)))
                         (equal (list (list 61200
                                            "interaction:stimulus:61200" t))
                                calls)))
         (setf events nil calls nil)
         (crm-check "quiet stimulus executor is idle without retained work"
                    (and (equal "idle"
                                (gethash "status"
                                         (conscious-recursive-stimulus-one)))
                         (null calls)))
         (let ((*conscious-recursive-mind-operator-pending-p* t))
           (crm-check "operator preempts generic stimulus before execution"
                      (and (equal "preempted"
                                  (gethash "status"
                                           (conscious-recursive-stimulus-one)))
                           (null calls)))))
    (setf (symbol-function '%recursive-thread-events) old-events
          (symbol-function '%recursive-run-root-locked) old-run)))

(let* ((events nil)
       (unknown-id 63001)
       (ready-id 63003)
       (projected-count 0)
       (largest-projection 0)
       (original (symbol-function 'conscious-recursive-thread-project)))
  (dotimes (index 1000)
    (let* ((id (+ 64000 index))
           (root (crm-event id "agent-stimulus-received"
                            (obj "source" "synthetic-queue"
                                 "text" "Already completed."
                                 "authority" "private-cognition-existing-authority")))
           (result (crm-event (+ 65000 index) "recursive-stimulus-result"
                              (obj "status" "completed") id)))
      (push root events)
      (push result events)))
  (push (crm-event unknown-id "agent-stimulus-received"
                   (obj "source" "synthetic-queue" "text" "Provider pending."
                        "authority" "private-cognition-existing-authority")) events)
  (push (crm-event 63002 "model-request"
                   (obj "thread_id"
                        "thread:stimulus:recursive-fixture:63001"
                        "model_call_id" "model:pending") unknown-id) events)
  (push (crm-event ready-id "agent-stimulus-received"
                   (obj "source" "synthetic-queue" "text" "Fresh input."
                        "authority" "private-cognition-existing-authority")) events)
  (setf events (nreverse events))
  (unwind-protect
       (progn
         (setf (symbol-function 'conscious-recursive-thread-project)
               (lambda (subset id agent-id)
                 (incf projected-count)
                 (setf largest-projection
                       (max largest-projection (length subset)))
                 (funcall original subset id agent-id)))
         (crm-check "large completed queue projects only bounded causal slices"
                    (let ((pending (%recursive-pending-private-stimuli
                                    events "recursive-fixture" :maximum 1)))
                      (and (= 1 (length pending))
                           (= ready-id (gethash "id" (first pending)))
                           (= 2 projected-count)
                           (<= largest-projection 2)))))
    (setf (symbol-function 'conscious-recursive-thread-project) original)))

(crm-check "private opportunity alternates after stimulus selection"
           (equal "private-work"
                  (recursive-private-opportunity-select
                   '("stimulus" "private-work") "stimulus")))
(crm-check "private opportunity alternates after ordinary work"
           (equal "stimulus"
                  (recursive-private-opportunity-select
                   '("stimulus" "private-work") "private-work")))
(crm-check "single private opportunity cannot be displaced"
           (equal "private-work"
                  (recursive-private-opportunity-select
                   '("private-work") "private-work")))

(let* ((root (crm-event 61210 "agent-stimulus-received"
                        (obj "source" "synthetic-fairness"
                             "text" "Synthetic pending attention."
                             "authority" "private-cognition-existing-authority")))
       (events (list root))
       (previous nil)
       (logged 0)
       (old-events (symbol-function '%recursive-thread-events))
       (old-last (symbol-function '%recursive-last-private-opportunity))
       (old-append (symbol-function '%conversation-append-readable))
       (*conscious-recursive-mind-agent-id* "recursive-fixture")
       (*conscious-recursive-mind-operator-pending-p* nil))
  (unwind-protect
       (progn
         (setf (symbol-function '%recursive-thread-events)
               (lambda () events)
               (symbol-function '%recursive-last-private-opportunity)
               (lambda () previous)
               (symbol-function '%conversation-append-readable)
               (lambda (type payload &key caused-by)
                 (declare (ignore caused-by))
                 (crm-check "competition records only a fairness journal event"
                            (equal "recursive-private-opportunity-selected" type))
                 (incf logged)
                 (setf previous (gethash "opportunity" payload))
                 (values logged (crm-event logged type payload))))
         (crm-check "successive quiet wakes share stimulus and private work"
                    (and (equal "stimulus" (%recursive-private-opportunity))
                         (equal "private-work" (%recursive-private-opportunity))
                         (equal "stimulus" (%recursive-private-opportunity))
                         (= 3 logged)))
         (setf events nil)
         (crm-check "idle stimulus queue causes no fairness journal flood"
                    (and (equal "private-work" (%recursive-private-opportunity))
                         (= 3 logged))))
    (setf (symbol-function '%recursive-thread-events) old-events
          (symbol-function '%recursive-last-private-opportunity) old-last
          (symbol-function '%conversation-append-readable) old-append)))

(let* ((episode-called nil)
       (old-opportunity (symbol-function '%recursive-private-opportunity))
       (old-stimulus (symbol-function 'conscious-recursive-stimulus-one))
       (old-episode
         (symbol-function 'conscious-recursive-conversation-episode-seal-batch))
       (*conscious-recursive-mind-operator-pending-p* nil))
  (unwind-protect
       (progn
         (setf (symbol-function '%recursive-private-opportunity)
               (lambda () "stimulus")
               (symbol-function 'conscious-recursive-stimulus-one)
               (lambda () (obj "status" "stimulus-completed"))
               (symbol-function 'conscious-recursive-conversation-episode-seal-batch)
               (lambda () (setf episode-called t) (obj "status" "completed")))
         (let ((result (conscious-recursive-mind-quiet-step)))
           (crm-check "stimulus opportunity is one bounded quiet quantum"
                      (and (equal "completed" (gethash "status" result))
                           (equal "stimulus" (gethash "opportunity" result))
                           (eq :null (gethash "episode" result))
                           (not episode-called)))))
    (setf (symbol-function '%recursive-private-opportunity) old-opportunity
          (symbol-function 'conscious-recursive-stimulus-one) old-stimulus
          (symbol-function 'conscious-recursive-conversation-episode-seal-batch)
          old-episode)))

(crm-check "registered board observer is advertised"
           (member "observe-environment"
                   (loop for schema across (%recursive-tool-schemas t nil t)
                         collect (gethash "name" (gethash "function" schema)))
                   :test #'string=))
(unwind-protect
     (progn
       (register-layer observe-agent-environment fixture-environment-reader
         :order 100
         :function
         (lambda (next request)
           (declare (ignore next))
           (obj "status" "observed"
                "resource_id" (gethash "resource_id" request)
                "revision" "fixture-r1"
                "content" "Synthetic current resource.")))
       (let* ((request (obj "kind" "document" "owner_id" "fixture"
                            "resource_id" "one"))
               (names (loop for schema across (%recursive-tool-schemas t nil t)
                           collect (gethash "name" (gethash "function" schema))))
              (rendered (%recursive-observe-environment request)))
         (crm-check "registered environment observers are advertised and bounded"
                    (and (member "observe-environment" names :test #'string=)
                         (string= "observed"
                                  (gethash "status" (shasht:read-json rendered)))))
         (crm-check "environment observation validates exact bounded identity"
                    (and (handler-case
                             (progn (%recursive-validate-tool-arguments
                                     "observe-environment" request)
                                    t)
                           (error () nil))
                         (handler-case
                             (progn (%recursive-validate-tool-arguments
                                     "observe-environment"
                                     (obj "kind" "document" "owner_id" "fixture"))
                                    nil)
                           (error () t))))))
  (unregister-layer 'observe-agent-environment 'fixture-environment-reader))

(let* ((path (merge-pathnames "provider-profile-fixture.json" (test-state-dir)))
       (profile
         (obj "provider" "openrouter"
              "endpoint" "https://provider.example.invalid/v1/chat/completions"
              "model" "fixture/episode-model"
              "publication_role" "proposal-only"
              "allowed_purposes" #("conversation-episode-sealing")
              "requires_native_tool_calls" t
              "provider_routing"
              (obj "zdr" t "data_collection" "deny"
                   "allow_fallbacks" nil)))
       (document
         (obj "schema_version" 1
              "experiment_routing" (obj "episode_profile" "fixture-episode")
              "profiles" (obj "fixture-episode" profile))))
  (ensure-directories-exist path)
  (with-open-file (stream path :direction :output :if-exists :supersede
                               :if-does-not-exist :create)
    (shasht:write-json document stream))
  (let* ((*conscious-conversation-provider-profiles-path* path)
         (selected (%conversation-episode-provider-profile))
         (routing (gethash "provider_routing" selected)))
    (crm-check "episode semantics use an independent safe configured profile"
               (and (string= "fixture/episode-model"
                             (gethash "model" selected ""))
                    (find "conversation-episode-sealing"
                          (gethash "allowed_purposes" selected)
                          :test #'string=)
                    (eq t (gethash "zdr" routing))
                    (string= "deny" (gethash "data_collection" routing ""))
                    (null (gethash "allow_fallbacks" routing))))))

(let* ((schemas (%recursive-tool-schemas t nil t))
       (names
         (loop for schema across schemas
               collect (gethash "name" (gethash "function" schema)))))
  (crm-check "public recursive roots advertise native memory search"
             (member "search-memory" names :test #'string=))
  (crm-check "private recursive roots retain native memory search"
             (member
              "search-memory"
              (loop for schema across (%recursive-tool-schemas t nil t)
                    collect (gethash "name" (gethash "function" schema)))
              :test #'string=))
  (crm-check "native memory search arguments remain strictly bounded"
             (let ((arguments
                     (%recursive-normalize-tool-arguments
                      "search-memory" "{\"query\":\"campfire\",\"limit\":3}")))
               (and (string= "campfire" (gethash "query" arguments))
                    (= 3 (gethash "limit" arguments))))))

(let* ((*conscious-recursive-mind-graph-search-fn*
         (lambda (request)
           (obj "schema_version" 1 "status" "empty"
                "echo_query" (gethash "query" request)
                "echo_direction" (gethash "direction" request)
                "echo_depth" (gethash "maximum_depth" request)
                "echo_paths" (gethash "maximum_paths" request)
                "paths" #())))
       (schemas (%recursive-tool-schemas nil nil nil))
       (names (loop for schema across schemas
                    collect (gethash "name" (gethash "function" schema))))
       (request (obj "query" "operator needs"))
       (result (conscious-recursive-knowledge-graph-search request)))
  (crm-check "configured recursive roots advertise native search-graph"
             (member "search-graph" names :test #'string=))
  (crm-check "operator graph search and native tool share one injected port"
             (and (string= "operator needs" (gethash "echo_query" result))
                  (string= "both" (gethash "echo_direction" result))
                  (= 2 (gethash "echo_depth" result))
                  (= 6 (gethash "echo_paths" result)))))

(let* ((*conscious-recursive-mind-graph-search-fn*
         (lambda (request) (declare (ignore request)) (obj)))
       (normalized
         (%recursive-normalize-tool-arguments
          "search-graph" "{\"query\":\"health issues\"}")))
  (crm-check "sparse native graph call carries runtime-owned defaults forward"
             (and (string= "health issues" (gethash "query" normalized))
                  (string= "both" (gethash "direction" normalized))
                  (string= "verified" (gethash "evidence_policy" normalized))
                  (= 2 (gethash "maximum_depth" normalized))
                  (= 6 (gethash "maximum_paths" normalized))
                  (vectorp (gethash "exact_queries" normalized))
                  (zerop (length (gethash "exact_queries" normalized)))
                  (vectorp (gethash "predicates" normalized))
                  (zerop (length (gethash "predicates" normalized))))))

;; Production-composition qualification.  This runs before any fixture below
;; replaces the conversation assembler.  Only impure projection/storage ports
;; are detached; profile validation, history selection, assembly-context
;; construction, and CONSCIOUS-CONTEXT-ASSEMBLE are the shipped functions.
(let* ((profile-document
         (shasht:read-json
          (uiop:read-file-string
           (merge-pathnames "config/conscious-context-profiles.json"
                            (pathname (uiop:getenv "PAI_ROOT")))
           :external-format :utf-8)))
       (profile
         (gethash "solicited-conversation-dev"
                  (gethash "profiles" profile-document)))
       (agent-id "recursive-composition-fixture")
       (persona-id "composition-fixture")
       (metadata
         (lambda (thread-id)
           (obj "source" "recursive-mind-v1" "thread_id" thread-id
                "persona_id" persona-id)))
       (events nil)
       (next-id 0))
  (dotimes (index 24)
    (let* ((thread (format nil "thread:composition:~d" index))
           (user-id (incf next-id)))
      (push (crm-event user-id "user-message"
                       (obj "text"
                            (format nil "operator history ~2,'0d ~a" index
                                    (make-string 1400 :initial-element #\x))
                            "channel" "terminal"
                            "metadata" (funcall metadata thread)))
            events)
      (push (crm-event (incf next-id) "agent-message"
                       (obj "text"
                            (format nil "assistant history ~2,'0d ~a" index
                                    (make-string 1400 :initial-element #\y))
                            "channel" "terminal"
                            "metadata" (funcall metadata thread))
                       user-id)
            events)))
  (setf events (nreverse events))
  (dolist (event events)
    (setf (gethash "agent_id" event) agent-id))
  (let* ((current-id (incf next-id))
         (current
           (crm-event current-id "user-message"
                      (obj "text" "What is my fixture relationship?"
                           "channel" "terminal"
                           "metadata" (funcall metadata
                                               "thread:composition:current"))))
         (all-events (append events (list current)))
         (recent-symbol 'event-recent-conversation-events)
         (lifecycle-symbol '%conversation-lifecycle-context-records)
         (private-symbol
           'conscious-recursive-private-cognition-context-records)
         (recent-original (symbol-function recent-symbol))
         (lifecycle-original (symbol-function lifecycle-symbol))
         (private-original (symbol-function private-symbol))
         (maximum-briefing-body
           (concatenate
            'string "MAXIMUM-BRIEFING "
            (make-string (- 1800 (length "MAXIMUM-BRIEFING "))
                         :initial-element #\b)))
         (maximum-briefing-content
           (format nil "Current private-state briefing: ~a"
                   maximum-briefing-body))
         (maximum-raw-content
           (concatenate
            'string "MAXIMUM-RAW-FALLBACK "
            (make-string (- 6000 (length "MAXIMUM-RAW-FALLBACK "))
                         :initial-element #\r)))
         (observed-graph-frame nil))
    (setf (gethash "agent_id" current) agent-id)
    (unwind-protect
         (progn
           (setf (symbol-function recent-symbol)
                 (lambda (&rest ignored)
                   (declare (ignore ignored)) all-events)
                 (symbol-function lifecycle-symbol)
                 (lambda (&rest ignored)
                   (declare (ignore ignored))
                   (values (vector) (vector) (vector)))
                 (symbol-function private-symbol)
                 (lambda (&rest ignored)
                   (declare (ignore ignored))
                   ;; Stress both producer bounds together. Production emits
                   ;; either the 1,800-character briefing or the 6,000-
                   ;; character raw fallback, never a larger row.
                   (vector
                    (obj "source_id" "private:maximum-briefing"
                         "content" maximum-briefing-content)
                    (obj "source_id" "private:maximum-raw"
                         "content" maximum-raw-content))))
           (let* ((*conscious-conversation-persona-profile*
                    (obj "schema_version" 1 "persona_id" persona-id
                         "revision" 1 "identity" "Fixture identity."
                         "voice" "Fixture voice."
                         "fingerprint" "composition-fixture-fingerprint"
                         "source" "qualification"))
                  (*conscious-conversation-memory-projection-fn* nil)
                  (*conscious-conversation-graph-context-fn*
                    (lambda (frame semantic episodes budget)
                      (declare (ignore semantic episodes budget))
                      (setf observed-graph-frame frame)
                      (values
                       (vector
                        (obj "source_id" "graph-context:fixture"
                             "content"
                             "Selected applicable fixture relationship."
                             "provenance"
                             (obj "descriptor_id" "graph-context:fixture"
                                  "descriptor_event_id" 700
                                  "evidence_event_ids" #(700))))
                       (obj "schema_version" 1 "status" "selected"
                            "selected_count" 1 "rendered_characters" 41
                            "database_write_count" 0))))
                  (spec
                    (%conversation-assembly-spec
                     all-events current-id
                     "What is my fixture relationship?" agent-id profile
                     "local" "terminal"))
                  (context
                    (make-conscious-assembly-context
                     :pulse-id "pulse:composition" :purpose "respond"
                     :audience (gethash "audience" spec)
                     :runtime-revision "qualification"
                     :conscious-state-revision 7
                     :clock-identity "fixture-clock"
                     :total-character-budget
                     (gethash "total_character_budget" spec)
                     :section-character-budgets
                     (gethash "section_character_budgets" spec)
                     :sections (gethash "sections" spec)
                     :eligible-evidence-ids
                     (gethash "eligible_evidence_ids" spec)
                     :available-tools (gethash "available_tools" spec)
                     :permitted-proposal-kinds
                     (gethash "permitted_proposal_kinds" spec)
                     :publication-constraints
                     (gethash "publication_constraints" spec)
                     :remaining-budget (gethash "remaining_budget" spec)
                     :pre-render-refusals
                     (gethash "pre_render_refusals" spec)))
                  (assembled
                    (conscious-context-assemble (obj "state_revision" 7)
                                                context))
                  (history
                    (gethash "conversation-evidence"
                             (gethash "sections" spec)))
                  (history-report
                    *conscious-conversation-turn-history-report*)
                  (rendered
                    (mapcar (lambda (row) (gethash "content" row ""))
                            (coerce (gethash "private_request" assembled)
                                    'list)))
                  (provider-request
                    (shasht:write-json
                     (%conversation-model-messages
                      assembled nil "What is my fixture relationship?")
                     nil)))
             (dolist (enabled '(nil t))
               (dolist (private-p '(nil t))
                 (let* ((*conscious-recursive-mind-tools-enabled-p* enabled)
                        (recursive-context
                          (%recursive-assembly-context spec "thread:capability" 7 private-p))
                        (recursive-opened
                          (conscious-context-assemble (obj "state_revision" 7)
                                                      recursive-context))
                        (messages
                          (%recursive-base-model-messages
                           recursive-opened "What is my fixture relationship?" private-p nil))
                        (wire (shasht:write-json messages nil)))
                   (crm-check "production recursive request removes inherited capability denial"
                              (and (not (search "No native tool is available for this model request." wire))
                                   (search "schemas attached to the current model request are authoritative" wire)))
                   (crm-check "production recursive capability metadata respects primitive switch"
                              (eq (not (null enabled))
                                  (not (null (find "search-memory"
                                                   (gethash "available_tools" recursive-context)
                                                   :test #'equal)))))
                   (crm-check "production recursive final synthesis closes tools explicitly"
                              (search "runtime has closed tool use"
                                      (shasht:write-json
                                       (%recursive-final-synthesis-messages messages private-p) nil))))))
             (crm-check "production composition loads the shipped history profile"
                        (and (= 5 (gethash "revision" profile))
                             (= 100 (gethash "history_max_events" profile))
                             (= 48000
                                (gethash "history_character_budget" profile))))
             (crm-check "production focus budget admits every current private-context bound"
                        (and (= 8000
                                (gethash
                                 "focus-lifecycles"
                                 (gethash "section_character_budgets" profile)))
                             (= 1800 (length maximum-briefing-body))
                             (= 6000 (length maximum-raw-content))))
             (crm-check "production composition retains dialogue beyond twelve records"
                        (and (>= (length history) 24)
                             (> (length history) 12)
                             (< (length history) 48)))
             (crm-check "active dialogue stays inside its estimated-token envelope"
                        (and (<= (gethash "estimated_tokens" history-report)
                                 (gethash "history_target_estimated_tokens"
                                          profile))
                             (gethash "degraded" history-report)))
             (crm-check "production assembler receives the selected dialogue rows"
                        (and (some (lambda (text)
                                     (search "assistant history 23" text))
                                   rendered)
                             (some (lambda (text)
                                     (search "selection is non-exhaustive" text))
                                   rendered)
                             (notany (lambda (text)
                                       (search "operator history 00" text))
                                     rendered)))
             (crm-check "maximum briefing and raw fallback reach the final provider request"
                        (and (search "MAXIMUM-BRIEFING" provider-request)
                             (search "MAXIMUM-RAW-FALLBACK" provider-request)))
             (crm-check "KG5 injects selected graph evidence under the shared memory budget"
                        (and observed-graph-frame
                             (string= "operator-conversation"
                                      (gethash "attention_kind"
                                               observed-graph-frame))
                             (find 700
                                   (coerce (gethash "eligible_evidence_ids" spec)
                                           'list)
                                   :test #'equal)
                             (search "Selected applicable fixture relationship"
                                     provider-request)))
             (crm-check "current runtime capability evidence outranks historical claims"
                        (and
                         (search "historical conversation claims and an individual tool failure do not override"
                                 provider-request)
                         (search "Bounded event-driven private cognition outside active chat"
                                 provider-request)))))
      (setf (symbol-function recent-symbol) recent-original
            (symbol-function lifecycle-symbol) lifecycle-original
            (symbol-function private-symbol) private-original))))

(let* ((*agent-id* "episode-fixture")
       (*conscious-recursive-mind-episodic-memory-enabled-p* t)
       (sealed
         (obj "id" 20 "type" "conversation-episode-sealed"
              "agent_id" "episode-fixture" "caused_by" 19
              "payload"
              (obj "schema_version" 1 "episode_id" "episode:fixture:10:11"
                   "persona_id" "fixture" "first_event_id" 10
                   "last_event_id" 11 "first_timestamp" 900
                   "last_timestamp" 1000 "source_event_ids" #(10 11)
                   "synopsis"
                   "The operator discussed iron deficiency and tendon recovery."
                   "subjects" #("health issue") "entities" #("iron")
                   "retrieval_cues" #("hemoglobin" "tendon recovery")
                   "broader_categories" #("physical wellbeing")
                   "unresolved_threads" #())))
       (raw-user
         (obj "id" 30 "type" "user-message" "agent_id" "episode-fixture"
              "timestamp" "2026-08-30T10:00:00Z" "caused_by" :null
              "payload"
              (obj "text" "I smell like campfire smoke at the campground."
                   "metadata"
                   (obj "source" "recursive-mind-v1"
                        "persona_id" "fixture"))))
       (raw-agent
         (obj "id" 31 "type" "agent-message" "agent_id" "episode-fixture"
              "timestamp" "2026-08-30T10:01:00Z" "caused_by" 30
              "payload"
              (obj "text" "Go smell less like campfire when you get back."
                   "metadata"
                   (obj "source" "recursive-mind-v1"
                        "persona_id" "fixture"))))
       (profile
         (obj "section_character_budgets" (obj "memory-bundles" 4000)
              "memory_provider_classes" #( "local"))))
  (multiple-value-bind (records ids report episode-candidates)
      (%conversation-episodic-context-records
       (list sealed) "that health issue" profile "local" "fixture")
    (crm-check "attention cue automatically selects persona-scoped episodes"
               (and (= 1 (length records)) (= 1 (length ids))
                    (string= "selected" (gethash "status" report))
                    (search "exact source events 10, 11"
                            (gethash "content" (aref records 0)))
                    (= 1 (length episode-candidates))
                    (string= "episode:fixture:10:11"
                             (gethash
                              "descriptor_id"
                              (gethash "provenance" (aref records 0)))))))
  (multiple-value-bind (records ids report)
      (%conversation-episodic-context-records
       (list sealed raw-user raw-agent) "smoke by the fire"
       profile "local" "fixture")
    (let ((raw (find "conversation-raw:30:31" records
                     :key (lambda (record) (gethash "source_id" record))
                     :test #'equal)))
      (crm-check "automatic recall includes exact recent unsealed dialogue"
                 (and raw
                      (= (length records) (length ids))
                      (find "conversation-raw:30:31" ids :test #'equal)
                      (equalp #(30 31)
                              (gethash
                               "evidence_event_ids"
                               (gethash "provenance" raw)))
                      (= 11 (gethash "sealed_through_event_id" report))
                      (= 1 (gethash "pending_episode_count" report))
                      (search "Go smell less like campfire"
                              (gethash "content" raw))))))
  (multiple-value-bind (records ids report)
      (%conversation-episodic-context-records
       (list sealed) "that health issue" profile "local" "other-persona")
    (declare (ignore ids report))
    (crm-check "automatic episodic context cannot cross persona identity"
               (zerop (length records)))))

;; The live-depth benchmark makes this cache a correctness-sensitive runtime
;; boundary.  Prove exact hits, append-prefix advancement, immutable prior
;; generations, and anomaly fallback without consulting a real database.
(let* ((report-symbol 'event-authority-report)
       (replay-symbol 'replay-events)
       (map-symbol 'map-events)
       (publish-symbol '%recursive-thread-events-checkpoint-publish)
       (report-original (symbol-function report-symbol))
       (replay-original (symbol-function replay-symbol))
       (map-original (symbol-function map-symbol))
       (publish-original (symbol-function publish-symbol))
       (head 1) (maximum-id 1) (replays 0) (maps 0) (map-windows nil)
       (checkpoint-publications 0)
       (replay-type-filter :unseen) (map-type-filter :unseen)
       (full-map-type-filter :unseen)
       (event-1 (crm-event 1 "user-message" (obj "text" "one")))
       (event-2 (crm-event 2 "agent-message" (obj "text" "two") 1))
       (event-duplicate-id
         (crm-event 2 "model-response" (obj "status" "accepted") 1))
       (authority-events (list event-1)))
  (unwind-protect
       (progn
         (setf *conscious-recursive-thread-events-cache* nil
               *conscious-recursive-thread-events-cache-key* nil
               *conscious-recursive-thread-events-cache-head* nil
               *conscious-recursive-thread-events-cache-max-id* nil
               *conscious-recursive-thread-events-checkpoint-head* nil
               *conscious-recursive-thread-events-checkpoint-due-p* nil
               (symbol-function report-symbol)
               (lambda ()
                 (obj "authority" "sqlite" "database" "fixture.sqlite3"
                      "agent_id" "fixture" "head_position" head
                      "max_event_id" maximum-id))
               (symbol-function replay-symbol)
               (lambda (&rest arguments)
                 (setf replay-type-filter (getf arguments :types :absent))
                 (incf replays)
                 (copy-list authority-events))
               (symbol-function map-symbol)
               (lambda (visitor &rest arguments)
                 (setf map-type-filter (getf arguments :types :absent))
                 (unless (getf arguments :after-position)
                   (setf full-map-type-filter map-type-filter))
                 (incf maps)
                 (let* ((types (getf arguments :types))
                        (after (or (getf arguments :after-position) 0))
                        (through (or (getf arguments :through-position)
                                     (length authority-events)))
                        (selected
                          (remove-if-not
                           (lambda (event)
                             (or (null types)
                                 (member (gethash "type" event "") types
                                         :test #'string=)))
                           (subseq authority-events after through))))
                   (push (list after through) map-windows)
                   (dolist (event selected) (funcall visitor event))
                    (values t maximum-id (length selected))))
               (symbol-function publish-symbol)
               (lambda (events publish-head publish-maximum-id)
                 (declare (ignore events publish-head publish-maximum-id))
                 (incf checkpoint-publications)
                 t))
         (let* ((first (%recursive-thread-events))
                 (second (%recursive-thread-events))
                (exact-hit-p
                  (and (eq first second) (zerop replays) (= 2 maps))))
           (setf authority-events (list event-1 event-2)
                 head 2 maximum-id 2)
            (setf checkpoint-publications 0
                  *conscious-recursive-thread-events-checkpoint-head* 1)
            (let ((*conscious-recursive-thread-events-checkpoint-interval* 1))
             (let ((advanced (%recursive-thread-events)))
              (crm-check "recursive event cache serves an exact SQLite head"
                         exact-hit-p)
             (crm-check "recursive event cache advances from only the authority tail"
                        (and (= 2 (length advanced)) (= 3 maps)
                             (zerop replays)))
             (crm-check "recursive replay is independent of storage type-filter cardinality"
                        (and (eq :unseen replay-type-filter)
                             (equal *conscious-recursive-thread-event-types*
                                    full-map-type-filter)
                             (> (length *conscious-recursive-thread-event-types*)
                                32)))
              (crm-check "recursive event cache keeps a prior generation stable"
                         (= 1 (length first)))
              (crm-check "recursive cache reads only mark checkpoint maintenance due"
                         (and (zerop checkpoint-publications)
                              *conscious-recursive-thread-events-checkpoint-due-p*))
              (%recursive-thread-events-checkpoint-maybe-publish)
              (crm-check "quiet maintenance publishes one due recursive checkpoint"
                         (and (= 1 checkpoint-publications)
                              (not *conscious-recursive-thread-events-checkpoint-due-p*)))
              (setf authority-events
                   (list event-1 event-2 event-duplicate-id)
                   head 3)
             (let* ((fallbacks-before
                      *conscious-recursive-thread-events-cache-fallbacks*)
                    (duplicate-tail (%recursive-thread-events)))
               (crm-check
                "recursive event cache advances when physical head grows without a new logical ID"
                (and (= 3 (length duplicate-tail)) (= 4 maps)
                     (zerop replays)
                     (= fallbacks-before
                        *conscious-recursive-thread-events-cache-fallbacks*)
                     (equal '(2 3) (first map-windows))))
               (crm-check "duplicate-ID tail keeps its prior generation stable"
                          (= 2 (length advanced))))
             (setf authority-events (list event-1 event-2) head 2)
             (let ((rebuilt (%recursive-thread-events)))
               (crm-check "recursive event cache rebuilds a regressed physical head"
                          (and (= 2 (length rebuilt)) (= 6 maps)
                               (zerop replays)
                                (equal '(0 2) (first map-windows)))))))))
    (setf (symbol-function report-symbol) report-original
          (symbol-function replay-symbol) replay-original
          (symbol-function map-symbol) map-original
          (symbol-function publish-symbol) publish-original
          *conscious-recursive-thread-events-cache* nil
          *conscious-recursive-thread-events-cache-key* nil
          *conscious-recursive-thread-events-cache-head* nil
          *conscious-recursive-thread-events-cache-max-id* nil
          *conscious-recursive-thread-events-checkpoint-due-p* nil)))

;; A production-sized ledger must never turn a missing/stale projection into an
;; implicit full replay.  That operation is reserved for the explicit offline
;; rebuild script, where the operator can provision appropriate memory.
(let* ((load-symbol 'event-authority-checkpoint-load)
       (binding-symbol 'event-authority-checkpoint-source-binding)
       (load-original (symbol-function load-symbol))
       (binding-original (symbol-function binding-symbol))
       (binding-calls 0)
       (checkpoint
         (obj "projector_revision" "recursive-thread-hot-v1"
              "policy_revision" "owner-separated-provider-compaction-v3"
              "through_storage_position" 11 "through_event_id" 11
              "state" (obj "source_binding" "fixture-seal"
                           "events" #()))))
  (unwind-protect
       (progn
         (setf (symbol-function load-symbol)
               (lambda (name)
                 (declare (ignore name)) checkpoint)
               (symbol-function binding-symbol)
               (lambda (event-id position)
                 (declare (ignore event-id position))
                 (incf binding-calls)
                 "fixture-seal"))
         (crm-check "v1 checkpoint with matching old policy is refused at restore"
                    (and (null (%recursive-thread-events-checkpoint-restore
                                "fixture" 11 11))
                         (zerop binding-calls))))
    (setf (symbol-function load-symbol) load-original
          (symbol-function binding-symbol) binding-original)))

(let* ((restore-symbol '%recursive-thread-events-checkpoint-restore)
       (replay-symbol '%recursive-thread-events-full-replay)
       (restore-original (symbol-function restore-symbol))
       (replay-original (symbol-function replay-symbol))
       (full-replays 0)
       (message nil))
  (unwind-protect
       (progn
         (setf (symbol-function restore-symbol) (lambda (&rest ignored)
                                                  (declare (ignore ignored)) nil)
               (symbol-function replay-symbol) (lambda (&rest ignored)
                                                 (declare (ignore ignored))
                                                 (incf full-replays)
                                                 nil))
         (let ((*conscious-recursive-thread-events-maintenance-replay-p* nil)
               (*conscious-recursive-thread-events-full-replay-max-head* 10))
           (handler-case
               (%recursive-thread-events-cache-rebuild "fixture" 11 11)
             (error (condition)
               (setf message (princ-to-string condition)))))
         (crm-check "large normal startup fails closed without a recursive checkpoint"
                    (and (zerop full-replays)
                         (stringp message)
                         (search "normal operation will not full-replay"
                                 message))))
    (setf (symbol-function restore-symbol) restore-original
          (symbol-function replay-symbol) replay-original)))

(let* ((assistant (obj "role" "assistant" "content" "kept"
                       "reasoning_details" (vector (obj "text" "large"))))
       (response (crm-event 2 "model-response"
                            (obj "status" "accepted"
                                 "assistant_message" assistant)
                            1))
       (terminal (crm-event 3 "agent-message" (obj "text" "kept") 1))
       (settled (%recursive-settled-root-register (list terminal)))
       (compacted (%recursive-compact-settled-provider-event response settled))
       (compacted-payload (%recursive-event-payload compacted))
       (unsettled (%recursive-compact-settled-provider-event
                   response (make-hash-table :test #'eql))))
  (crm-check "settled provider transcript is absent from the hot replay copy"
             (and (not (nth-value 1
                         (gethash "assistant_message" compacted-payload)))
                  (eq t (gethash "settled_assistant_compacted"
                                 compacted-payload))))
  (crm-check "settled provider compaction does not mutate authority objects"
             (nth-value 1 (gethash "reasoning_details" assistant)))
  (crm-check "unsettled provider reasoning remains exact for recovery"
             (eq response unsettled)))

(let ((graph-request
        (crm-event 1 "model-request"
                   (obj "knowledge_graph_formation" t
                        "formation_phase" "facts")))
      (graph-response
        (crm-event 2 "model-response"
                   (obj "knowledge_graph_formation" t
                        "formation_phase" "facts"
                        "assistant_message"
                        (obj "role" "assistant" "content" "large"))))
      (conversation-response
        (crm-event 3 "model-response"
                   (obj "status" "accepted"
                        "assistant_message"
                        (obj "role" "assistant" "content" "keep")))))
  (crm-check "graph-owner model requests are outside recursive replay"
             (not (%recursive-thread-event-p graph-request)))
  (crm-check "graph-owner model responses are outside recursive replay"
             (not (%recursive-thread-event-p graph-response)))
  (crm-check "conversation model responses remain recursive replay evidence"
             (%recursive-thread-event-p conversation-response)))

(let ((*conscious-recursive-mind-operator-pending-p* nil))
  (setf *conscious-recursive-mind-operator-waiters* 0)
  (%recursive-operator-waiter-change 1)
  (%recursive-operator-waiter-change 1)
  (%recursive-operator-waiter-change -1)
  (crm-check "one admitted submitter cannot erase another queued operator"
             (%recursive-operator-pending-p))
  (%recursive-operator-waiter-change -1)
  (crm-check "operator preemption clears only after every waiter is claimed"
             (not (%recursive-operator-pending-p))))

;; This gate deliberately runs before the fixture replaces the real context
;; constructor below. It caught the lived failure where a private native reply
;; advertised a nonexistent legacy captured-proposal kind.
(let ((budgets (make-hash-table :test #'equal))
      (sections (make-hash-table :test #'equal)))
  (dolist (name (%ca-section-names))
    (setf (gethash name budgets) 0
          (gethash name sections) (vector)))
  (let* ((spec
           (obj "audience" "private" "total_character_budget" 1
                "section_character_budgets" budgets "sections" sections
                "eligible_evidence_ids" (vector)
                "publication_constraints"
                (obj "audiences" (vector "operator"))
                "remaining_budget"
                (obj "tool_proposals" 0 "continuations" 0
                     "publication_candidates" 1)
                "pre_render_refusals" (vector)))
         (context (%recursive-assembly-context spec "thread:private" 1 t)))
    (crm-check "private native context passes the production assembly contract"
               (handler-case
                   (progn
                     (%ca-validate-context context (obj "state_revision" 1))
                     (and
                      (zerop (length
                              (gethash "permitted_proposal_kinds" context)))
                      (string= "private" (gethash "audience" context))
                      (equalp (vector "private")
                              (gethash "audiences"
                                       (gethash "publication_constraints"
                                                context)))
                      (zerop (gethash "publication_candidates"
                                     (gethash "remaining_budget" context)))))
                 (error () nil)))))

;; Projection checkpoints intentionally omit native curiosity-focus roots.
;; The recursive projection proves that root separately; do not force the
;; conscious capsule to contain it or replay the complete ledger to make it so.
(crm-check "private context does not impose a foreign projection boundary"
           (null (%recursive-conscious-boundary-event-id t 2)))
(crm-check "foreground context retains its exact admitted boundary"
           (= 2 (%recursive-conscious-boundary-event-id nil 2)))

(let ((pseudo
        (format nil
                "A bounded answer.~%<tool_call>~%<function=web-fetch>~%<parameter=url>https://example.com</parameter>~%</function>~%</tool_call>")))
  (crm-check "complete top-level pseudo-tool envelope is quarantinable"
             (%recursive-pseudo-tool-envelope-p pseudo))
  (crm-check "fenced tool syntax remains publishable discussion"
             (not (%recursive-pseudo-tool-envelope-p
                   (format nil
                           "Example only:~%```xml~%<tool_call>~%</tool_call>~%```"))))
  (crm-check "inline tool tag discussion is not an action envelope"
             (not (%recursive-pseudo-tool-envelope-p
                   "The literal <tool_call> tag is a legacy protocol."))))

(let* ((root (crm-event 1 "user-message"
                        (obj "text" "hello" "channel" "terminal"
                             "metadata"
                             (obj "source" "recursive-mind-v1"
                                  "thread_id" "thread:recursive-fixture:1"))))
       (request (crm-event 2 "model-request"
                           (obj "thread_id" "thread:recursive-fixture:1"
                                "model_call_id" "model:1") 1))
       (response (crm-event 3 "model-response"
                            (obj "thread_id" "thread:recursive-fixture:1"
                                 "model_call_id" "model:1"
                                 "status" "accepted"
                                 "assistant_message"
                                 (obj "role" "assistant"
                                      "content" "I can read the file.")) 1))
       (reply (crm-event 4 "agent-message"
                         (obj "text" "I can read the file."
                              "model_call_id" "model:1"
                              "channel" "terminal"
                              "metadata"
                              (obj "source" "recursive-mind-v1"
                                   "thread_id" "thread:recursive-fixture:1")) 1)))
  (crm-check "admitted stimulus selects model boundary"
             (string= "model-ready"
                      (gethash "state"
                               (conscious-recursive-thread-project
                                (list root) 1 "recursive-fixture"))))
  (crm-check "unresolved provider boundary is outcome-unknown"
             (string= "outcome-unknown"
                      (gethash "state"
                               (conscious-recursive-thread-project
                                (list root request) 1 "recursive-fixture"))))
  (crm-check "accepted response selects publication boundary"
             (string= "publication-ready"
                      (gethash "state"
                               (conscious-recursive-thread-project
                                (list root request response) 1
                                "recursive-fixture"))))
  (let ((done (conscious-recursive-thread-project
               (list root request response reply) 1 "recursive-fixture")))
    (crm-check "durable reply completes recursion"
               (and (string= "done" (gethash "state" done))
                     (string= "I can read the file." (gethash "content" done)))))
  (let* ((settled (%recursive-settled-root-register (list reply)))
         (compacted (%recursive-compact-settled-provider-event
                     response settled))
         (done (conscious-recursive-thread-project
                (list root request compacted reply) 1 "recursive-fixture")))
    (crm-check "same-generation compacted public reply still completes"
               (and (string= "done" (gethash "state" done))
                    (string= "I can read the file." (gethash "content" done))))))

(let* ((thread "thread:recursive-fixture:1")
       (root (crm-event 1 "user-message"
                        (obj "text" "Synthetic tool turn" "channel" "terminal"
                             "metadata" (obj "source" "recursive-mind-v1" "thread_id" thread))))
       (call (obj "id" "tool:1" "type" "function"
                  "function" (obj "name" "lisp-eval" "arguments" "{\"form\":\"(+ 1 1)\"}")))
       (events
         (list root
               (crm-event 2 "model-request" (obj "thread_id" thread "model_call_id" "model:1") 1)
               (crm-event 3 "model-response"
                          (obj "thread_id" thread "model_call_id" "model:1" "status" "accepted"
                               "assistant_message" (obj "role" "assistant" "content" :null
                                                        "tool_calls" (vector call))) 1)
               (crm-event 4 "recursive-tool-execution"
                          (obj "thread_id" thread "model_call_id" "model:1" "tool_call_id" "tool:1") 1)
               (crm-event 5 "recursive-tool-result"
                          (obj "thread_id" thread "model_call_id" "model:1" "tool_call_id" "tool:1"
                               "execution_status" "executed" "content" "2") 1)
               (crm-event 6 "model-request" (obj "thread_id" thread "model_call_id" "model:2") 1)
               (crm-event 7 "model-response"
                          (obj "thread_id" thread "model_call_id" "model:2" "status" "accepted"
                               "assistant_message" (obj "role" "assistant" "content" "Synthetic answer")) 1)
               (crm-event 8 "agent-message"
                          (obj "text" "Synthetic answer" "model_call_id" "model:2" "channel" "terminal"
                               "metadata" (obj "source" "recursive-mind-v1" "thread_id" thread)) 1)))
       (settled (%recursive-settled-root-register events))
       (hot (mapcar (lambda (event) (%recursive-compact-settled-provider-event event settled)) events))
       (reads nil))
  (flet ((read-exact (id &key event-type)
           (push id reads)
           (find-if (lambda (event) (and (= id (gethash "id" event))
                                        (equal event-type (gethash "type" event)))) events)))
    (let* ((hydrated (%recursive-root-replay-events hot 1 "recursive-fixture" #'read-exact))
           (done (conscious-recursive-thread-project hydrated 1 "recursive-fixture")))
      (crm-check "compacted tool turn replays using exact authority responses"
                 (and (equal "done" (gethash "state" done))
                      (equal "Synthetic answer" (gethash "content" done))
                      (equal '(7 3) reads)))
      (crm-check "hydration leaves shared compacted generation unchanged"
                 (null (gethash "assistant_message" (gethash "payload" (third hot))))))
    (setf reads nil)
    (%recursive-root-replay-events hot 99 "recursive-fixture" #'read-exact)
    (crm-check "hydration never reads unrelated roots" (null reads)))
  (crm-check "missing authority response fails closed"
             (handler-case
                 (progn (%recursive-root-replay-events hot 1 "recursive-fixture"
                          (lambda (&rest ignored) (declare (ignore ignored)) nil)) nil)
               (error () t))))

(let* ((content
         (format nil
                 "<tool_call>~%<function=web-fetch>~%<parameter=url>https://example.com</parameter>~%</function>~%</tool_call>"))
       (root (crm-event 10 "user-message"
                        (obj "text" "research" "channel" "terminal"
                             "metadata"
                             (obj "source" "recursive-mind-v1"
                                  "thread_id" "thread:recursive-fixture:10"))))
       (request (crm-event 11 "model-request"
                           (obj "thread_id" "thread:recursive-fixture:10"
                                "model_call_id" "model:10") 10))
       (response (crm-event 12 "model-response"
                            (obj "thread_id" "thread:recursive-fixture:10"
                                 "model_call_id" "model:10" "status" "accepted"
                                 "pseudo_tool_envelope" t
                                 "assistant_message"
                                 (obj "role" "assistant" "content" content)) 10))
       (refusal
         (crm-event
          13 "recursive-pseudo-tool-refusal"
          (obj "schema_version" 1
               "thread_id" "thread:recursive-fixture:10"
               "model_call_id" "model:10" "runtime_revision" "fixture"
               "repair_attempt" 1 "terminal" nil
               "instruction"
               *conscious-recursive-pseudo-tool-repair-instruction*
               "error_code" :null "reason" :null)
          10))
       (projection
         (conscious-recursive-thread-project
          (list root request response refusal) 10 "recursive-fixture")))
  (crm-check "durable pseudo-tool refusal resumes at one model boundary"
             (and (string= "model-ready" (gethash "state" projection))
                  (= 2 (length (gethash "transcript" projection)))
                  (string= "assistant"
                           (gethash "role" (aref (gethash "transcript" projection)
                                                 0)))
                  (string= *conscious-recursive-pseudo-tool-repair-instruction*
                           (gethash "content"
                                    (aref (gethash "transcript" projection) 1))))))

;; Exercise the actual trampoline while replacing only impure adapters and the
;; already-qualified context renderer.  The event list survives reconfigure to
;; model a fresh Lisp process restoring the same authority.
(defvar *crm-events* nil)
(defvar *crm-provider-calls* 0)
(defvar *crm-provider-mode* :reply)
(defvar *crm-provider-script* nil)
(defvar *crm-provider-messages* nil)
(defvar *crm-provider-trace-metadata* nil)
(defvar *crm-provider-tool-counts* nil)
(defvar *crm-provider-tool-names* nil)
(defvar *crm-provider-tool-choices* nil)
(defvar *crm-provider-output-caps* nil)
(defvar *crm-provider-models* nil)
(defvar *crm-tool-executions* nil)
(defvar *crm-activities* nil)
(defvar *crm-hide-thread-journals* nil)
(defvar *crm-prior-reply-counts* nil)
(defvar *crm-native-current-prompts* nil)
(defvar *crm-memory-writes* nil)
(defvar *crm-history-contents* nil)
(defvar *crm-assembly-context-args* nil)
(defvar *crm-time-phases* nil)
(defvar *crm-last-replay-types* nil)
(defvar *crm-openrouter-p* nil)
(defvar *crm-openrouter-request-bound* 0.005d0)
(defvar *crm-openrouter-budget-ready-p* t)
(defvar *crm-provider-cost-per-call* nil)
(defvar *crm-provider-reasoning-enabled-values* nil)
(defvar *crm-provider-reasoning-values* nil)
(defvar *crm-pend-after-provider-p* nil)
(defvar *public-inbound-channel* "terminal")
(defvar *memory-retrieval-timing-ms* nil)
(defvar *conscious-conversation-turn-timing-ms* nil)

(defparameter *crm-ordinary-native-tool-names*
  '("observe-environment" "inspect-work-docket" "manage-work-docket"
    "lisp-eval" "bash" "brave-search" "web-fetch" "search-experience" "search-memory"))

(defun crm-tool-schema-names (tools)
  (loop for schema across tools
        for function = (and (hash-table-p schema) (gethash "function" schema))
        collect (and (hash-table-p function) (gethash "name" function))))

(defun crm-tool-schemas-valid-p (tools)
  (let ((names (crm-tool-schema-names tools)))
    (and (every
          (lambda (schema)
            (let ((function (and (hash-table-p schema)
                                 (gethash "function" schema))))
              (and (string= "function" (gethash "type" schema ""))
                   (hash-table-p function)
                   (stringp (gethash "name" function))
                   (plusp (length (gethash "name" function)))
                   (hash-table-p (gethash "parameters" function))
                   (string= "object"
                            (gethash "type"
                                     (gethash "parameters" function) "")))))
          (coerce tools 'list))
         (= (length names)
            (length (remove-duplicates names :test #'string=))))))

(defun replay-events (&key types limit)
  (setf *crm-last-replay-types* types)
  (let ((events
          (if *crm-hide-thread-journals*
              (remove-if
               (lambda (event)
                 (member (gethash "type" event "")
                         '("model-request" "model-response") :test #'string=))
               *crm-events*)
              (copy-list *crm-events*))))
    (if (and limit (> (length events) limit))
        (last events limit)
        events)))
(defun %conscious-runtime-events ()
  ;; The production conscious checkpoint contains projected public/state
  ;; facts, not model IO journals. The recursive thread must use authority.
  (remove-if-not
   (lambda (event)
     (member (gethash "type" event "")
             '("user-message" "agent-message") :test #'string=))
   *crm-events*))
(defun log-event (type payload &key caused-by)
  (let* ((id (1+ (length *crm-events*)))
         (event (crm-event id type payload caused-by)))
    (setf *crm-events* (append *crm-events* (list event)))
    (values id t event)))
(defun submit-stimulus (text &key kind metadata wait-for-public-result)
  (declare (ignore kind wait-for-public-result))
  (values nil :accepted
          (log-event "user-message"
                     (obj "text" text "channel" *public-inbound-channel*
                          "metadata" metadata))))
(defun %conscious-runtime-install-projections (events &key through-event-id)
  (declare (ignore events through-event-id))
  (obj "state_revision" 1 "composition_hash" "fixture"))
(defun %conversation-authorized-endpoint-p (endpoint)
  (string= endpoint "http://127.0.0.1:1234/v1/chat/completions"))
(defun %conversation-openrouter-endpoint-p (endpoint)
  (declare (ignore endpoint)) *crm-openrouter-p*)
(defun %conversation-openrouter-request-cost-bound
    (messages endpoint model temperature &optional tools tool-choice)
  (declare (ignore messages endpoint model temperature tools tool-choice))
  *crm-openrouter-request-bound*)
(defun %conversation-openrouter-budget-ready-p ()
  *crm-openrouter-budget-ready-p*)
(defun %conversation-provider-class (endpoint)
  (declare (ignore endpoint)) "local-provider")
(defun %conversation-persona-profile ()
  (obj "persona_id" "fixture" "revision" 1 "fingerprint" "fixture-fp"))
(defun %conversation-context-budget-profile (profile)
  (declare (ignore profile)) (obj "max_input_characters" 1000))
(defun %conversation-assembly-spec
    (events user-event-id prompt agent-id profile provider-class channel
     &optional work-id prepared-work episodic-events attention-kind)
  (declare (ignore prompt profile provider-class channel work-id prepared-work
                   episodic-events attention-kind))
  (let ((history-events
          (event-recent-conversation-events user-event-id 64)))
    (multiple-value-bind (history history-report)
        (conscious-conversation-history
         history-events agent-id :before-event-id user-event-id
         :max-events 32 :character-budget 20000
         :event-character-limit 4096)
      (setf *conscious-conversation-turn-history-report* history-report)
      (push (loop for row across history collect (gethash "content" row))
            *crm-history-contents*)))
  (push (count "agent-message" events
               :key (lambda (event) (gethash "type" event ""))
               :test #'string=)
        *crm-prior-reply-counts*)
  (obj "audience" "operator" "total_character_budget" 1
       "section_character_budgets" (obj) "sections" (obj)
       "eligible_evidence_ids" (vector) "publication_constraints" (obj)
       "remaining_budget" (obj) "pre_render_refusals" (vector)))
(defun make-conscious-assembly-context (&rest arguments)
  (push arguments *crm-assembly-context-args*)
  (obj))
(defun conscious-context-assemble (state context)
  (declare (ignore state context)) (obj "fixture" t))
(defun %conversation-model-messages (opened &optional work current-prompt)
  (declare (ignore opened work))
  (push current-prompt *crm-native-current-prompts*)
  (list (obj "role" "user" "content" current-prompt)))
(defun %conversation-progress-notify (&rest ignored) (declare (ignore ignored)))
(defun %conversation-time-phase (name thunk)
  (push name *crm-time-phases*)
  (funcall thunk))
(defun %conversation-call-model-with-trace (messages metadata thunk)
  (declare (ignore messages))
  (push metadata *crm-provider-trace-metadata*)
  (funcall thunk))
(defun %conversation-http-model-call
    (messages endpoint model temperature &key tools tool-choice)
  (declare (ignore endpoint temperature))
  (push model *crm-provider-models*)
  (push (length tools) *crm-provider-tool-counts*)
  (push (crm-tool-schema-names tools) *crm-provider-tool-names*)
  (push tool-choice *crm-provider-tool-choices*)
  (push *conscious-conversation-max-output-tokens*
        *crm-provider-output-caps*)
  (let* ((profile *conscious-conversation-provider-profile*)
         (reasoning (and (hash-table-p profile)
                         (gethash "reasoning" profile))))
    (push (and (hash-table-p reasoning) reasoning)
          *crm-provider-reasoning-values*)
    (push (and (hash-table-p reasoning)
               (multiple-value-bind (enabled present-p)
                   (gethash "enabled" reasoning)
                 (if present-p
                     (eq t enabled)
                     (and (stringp (gethash "effort" reasoning)) t))))
          *crm-provider-reasoning-enabled-values*))
  (crm-check "provider receives valid unique schemas or explicit tool-free synthesis"
             (crm-tool-schemas-valid-p tools))
  (incf *crm-provider-calls*)
  (when (numberp *crm-provider-cost-per-call*)
    (incf *conscious-conversation-provider-spent-usd*
          *crm-provider-cost-per-call*)
    (when *conscious-conversation-private-provider-call-p*
      (incf *conscious-conversation-private-provider-spent-usd*
            *crm-provider-cost-per-call*)))
  (push (copy-list messages) *crm-provider-messages*)
  (let ((scripted (and *crm-provider-script* (pop *crm-provider-script*))))
    (prog1
        (obj "choices"
             (vector
              (obj "message"
                   (cond
                 ((and (consp scripted) (eq :tools (first scripted)))
                  (obj "role" "assistant" "content" :null
                       "tool_calls"
                       (coerce
                        (loop for specification in (second scripted)
                              for index from 0
                              collect
                              (obj "id" (format nil "provider-call-~d" index)
                                   "type" "function" "function"
                                   (obj "name" (first specification)
                                        "arguments" (second specification))))
                        'vector)))
                 ((and (consp scripted) (eq :message (first scripted)))
                  (second scripted))
                 ((and (consp scripted) (eq :tool (first scripted)))
                  (let ((message
                          (obj "role" "assistant" "content" :null
                               "tool_calls"
                               (vector
                                (obj "id" "provider-call" "type" "function"
                                     "function"
                                     (obj "name" (second scripted)
                                          "arguments" (third scripted)))))))
                    (when (fourth scripted)
                      (setf (gethash "reasoning_details" message)
                            (fourth scripted)))
                    message))
                 ((stringp scripted)
                  (obj "role" "assistant" "content" scripted))
                 ((and (consp scripted) (eq :reasoning-only (first scripted)))
                  (obj "role" "assistant" "content" :null
                       "reasoning" (second scripted)))
                 ((eq scripted :invalid)
                  (obj "role" "assistant" "content" :null))
                 ((eq scripted :timeout)
                  (error 'conscious-conversation-provider-timeout :seconds 120))
                 ((eq *crm-provider-mode* :tool)
                  (obj "role" "assistant" "content" :null
                       "tool_calls"
                       (vector (obj "id" "provider-call" "type" "function"
                                    "function"
                                    (obj "name" "search-files"
                                         "arguments" "{}")))) )
                 (t
                  (obj "role" "assistant"
                       "content"
                       (format nil "reply ~d" *crm-provider-calls*)))))))
      (when *crm-pend-after-provider-p*
        (setf *conscious-recursive-mind-operator-pending-p* t
              *crm-pend-after-provider-p* nil)))))
;; Fixture stand-in for %CONVERSATION-HTTP-MODEL-CALL-WITH-RETRY: a single
;; delegated attempt, no backoff. Generic backoff retry is exercised where
;; the real function lives (conscious-conversation-runtime-tests.lisp); this
;; isolated suite is about the recursive mind loop's own recovery semantics
;; (reasoning-timeout recovery, reasoning-only-message recovery, and so on),
;; which depend on seeing exactly one scripted response per call.
(defun %conversation-http-model-call-with-retry
    (messages endpoint model temperature
     &key transport-fn tools tool-choice on-attempt-failure retryable-failure-fn)
  (declare (ignore transport-fn on-attempt-failure retryable-failure-fn))
  (%conversation-http-model-call messages endpoint model temperature
                                  :tools tools :tool-choice tool-choice))
(defun %conversation-response-message (response)
  (gethash "message" (aref (gethash "choices" response) 0)))
(defun %conversation-json-present-p (value)
  (and value (not (eq value :null))))
(defun %conversation-response-usage (response)
  (declare (ignore response))
  (obj "input_tokens" 7 "output_tokens" 1 "reasoning_tokens" 0
       "total_tokens" 8))
(defun %conversation-provider-failure-details (condition)
  (if (typep condition 'conscious-conversation-provider-timeout)
      (values "provider-call-timeout" (format nil "~a" condition)
              :null "conscious-conversation-provider-timeout")
      (values "fixture-failure" (format nil "~a" condition) :null "fixture")))
(defun %conversation-append-readable (type payload &key caused-by)
  (multiple-value-bind (id durable event)
      (log-event type payload :caused-by caused-by)
    (declare (ignore durable)) (values id event)))
(defun %motivation-fnv (text)
  (format nil "fixture-~8,'0x" (ldb (byte 32 0) (sxhash text))))
(defun conscious-curiosity-observation-payload
    (&key request-id mind-identity-id subject-type subject-label subject-refs
          reinforcement-kind supporting-event-ids source-revision
          actor-runtime-revision observed-at)
  (let* ((refs (%motivation-sorted-strings subject-refs))
         (motive-id (%motivation-derived-id
                     "curiosity" mind-identity-id subject-type refs))
         (payload
           (obj "schema_version" 1 "request_id" request-id
                "motive_kind" "curiosity" "mind_identity_id" mind-identity-id
                "subject_type" subject-type "subject_label" subject-label
                "subject_refs" (coerce refs 'vector)
                "reinforcement_kind" reinforcement-kind
                "supporting_event_ids" (coerce supporting-event-ids 'vector)
                "source_revision" source-revision
                "actor_runtime_revision" actor-runtime-revision
                "observed_at" observed-at "motive_id" motive-id
                "integrity_hash" :pending)))
    (setf (gethash "integrity_hash" payload)
          (%motivation-fnv (%motivation-observation-canonical payload)))
    payload))
(defun stimulus-from-event (event &key agent-id)
  (declare (ignore agent-id))
  (when (member (gethash "type" event "")
                '("conscious-curiosity-candidate-raised"
                  "recursive-curiosity-focus-opened")
                :test #'string=)
    (obj "kind" "intention-cue" "sub_kind" "curiosity"
         "stimulus_id" (format nil "stimulus:~a" (gethash "id" event)))))
(defun conscious-motivation-runtime-reconcile
    (agent-id &key actor-runtime-revision origin-runtime-revision now)
  (declare (ignore actor-runtime-revision origin-runtime-revision now))
  (let ((groups (make-hash-table :test #'equal)))
    (dolist (event *crm-events*)
      (when (and (equal agent-id (gethash "agent_id" event))
                 (string= "conscious-curiosity-observed"
                          (gethash "type" event "")))
        (push event (gethash (gethash "motive_id" (gethash "payload" event))
                             groups))))
    (maphash
     (lambda (motive-id observations)
       (when (and (>= (length observations) 3)
                  (not (find-if
                        (lambda (event)
                          (let ((payload (gethash "payload" event)))
                            (and (string= "conscious-curiosity-candidate-raised"
                                          (gethash "type" event ""))
                                 (hash-table-p payload)
                                 (equal motive-id
                                        (gethash "motive_id" payload)))))
                        *crm-events*)))
         (let ((latest (car observations)))
           (log-event
            "conscious-curiosity-candidate-raised"
            (obj "motive_id" motive-id "motive_kind" "curiosity"
                 "expression_policy" "private-consideration-only"
                 "source_event_ids" (vector (gethash "id" latest))
                 "latest_event_id" (gethash "id" latest))
            :caused-by (gethash "id" latest)))))
     groups)
    (values (obj "state" "reconciled") (copy-list *crm-events*))))
(defun %conversation-new-turn-timing () (obj))

(setf *crm-events* nil *crm-provider-calls* 0 *crm-prior-reply-counts* nil
      *crm-notifications* nil
      *crm-native-current-prompts* nil
      *crm-history-contents* nil
      *crm-provider-trace-metadata* nil)
(conscious-recursive-mind-configure
 :agent-id "recursive-fixture"
 :endpoint "http://127.0.0.1:1234/v1/chat/completions"
 :model "fixture" :context-profile (obj)
 :observer-fn
 (lambda (status item result)
   (declare (ignore result))
   (push (list status item) *crm-notifications*)))
(let ((first (conscious-recursive-mind-submit "first")))
  (crm-check "first direct recursive turn replies"
             (and (string= "replied" (gethash "status" first))
                  (string= "reply 1" (gethash "content" first))))
  (let ((history-report (gethash "history_context" first)))
    (crm-check "final result exposes content-free provider pressure telemetry"
               (and (hash-table-p history-report)
                    (= 1 (gethash "provider_boundary_count" history-report))
                    (plusp (gethash "provider_message_characters" history-report))
                    (= 7 (gethash "provider_input_tokens" history-report)))))
  (let ((trace-metadata (first *crm-provider-trace-metadata*)))
    (crm-check "private trace metadata carries content-free context pressure"
               (and (plusp (gethash "message_characters" trace-metadata))
                    (= 0 (gethash "history_candidate_count" trace-metadata))
                    (= 0 (gethash "history_record_count" trace-metadata))
                    (= 0 (gethash "history_omitted_record_count"
                                  trace-metadata)))))
  (let ((accepted (find "accepted" *crm-notifications*
                        :key #'first :test #'string=)))
    (crm-check "accepted notification carries deterministic web transport facts"
               (and accepted
                    (string= "terminal" (gethash "channel" (second accepted)))
                    (string= "first" (gethash "content" (second accepted)))))))
(let ((second (conscious-recursive-mind-submit "second")))
  (crm-check "second direct recursive turn replies"
             (string= "reply 2" (gethash "content" second))))
;; Reconfigure without preserving any runtime-owned thread state.
(setf *conscious-recursive-mind-sequence* 0)
(conscious-recursive-mind-configure
 :agent-id "recursive-fixture"
 :endpoint "http://127.0.0.1:1234/v1/chat/completions"
 :model "fixture" :context-profile (obj))
(let ((third (conscious-recursive-mind-submit "after restart")))
  (crm-check "conversation continues after runtime restart"
             (string= "reply 3" (gethash "content" third))))
(crm-check "each turn rebuilds prior replies from durable history"
           (equal '(0 1 2) (nreverse *crm-prior-reply-counts*)))
(crm-check "direct path emits no pulse, work, or interaction state events"
           (notany (lambda (event)
                     (member (gethash "type" event "")
                             '("conscious-pulse-opened" "conscious-work-opened"
                               "interaction-opened")
                             :test #'string=))
                   *crm-events*))
(crm-check "thread progress does not depend on projection-capsule journals"
           (= 3 *crm-provider-calls*))
(crm-check "recursive provider receives exact current stimuli as native users"
           (equal '("first" "second" "after restart")
                  (reverse *crm-native-current-prompts*)))

;; MiMo may spend a whole generation in private reasoning and return neither
;; public content nor a native tool call. The recursive loop records that
;; outcome without persisting the reasoning, then uses one ordinary boundary
;; with reasoning disabled. The recovery remains visible to replay, budgets,
;; and the shared model-boundary guard.
(setf *crm-events* nil
      *crm-provider-calls* 0
      *crm-provider-messages* nil
      *crm-provider-reasoning-enabled-values* nil
      *crm-provider-script*
      (list (list :reasoning-only "private scratch must not persist")
            "recovered public answer")
      *conscious-recursive-mind-sequence* 0)
(let ((*conscious-conversation-provider-profile*
        (obj "reasoning" (obj "enabled" t))))
  (conscious-recursive-mind-configure
   :agent-id "recursive-fixture"
   :endpoint "http://127.0.0.1:1234/v1/chat/completions"
   :model "fixture" :context-profile (obj))
  (let* ((result (conscious-recursive-mind-submit "recover reasoning-only"))
         (requests
           (remove "model-request" *crm-events*
                   :key (lambda (event) (gethash "type" event ""))
                   :test-not #'string=))
         (responses
           (remove "model-response" *crm-events*
                   :key (lambda (event) (gethash "type" event ""))
                   :test-not #'string=))
         (second-messages (first *crm-provider-messages*)))
    (crm-check "reasoning-only response recovers within the same recursive root"
               (and (string= "replied" (gethash "status" result))
                    (string= "recovered public answer"
                             (gethash "content" result))
                    (= 2 *crm-provider-calls*)))
    (crm-check "reasoning recovery is a separate durable model boundary"
               (and (= 2 (length requests)) (= 2 (length responses))
                    (string= "reasoning-recovery-required"
                             (gethash "status"
                                      (%recursive-event-payload
                                       (first responses))))
                    (eq t (gethash "reasoning_recovery"
                                   (%recursive-event-payload
                                    (second requests))))))
    (crm-check "reasoning recovery disables only the retry call"
               (equal '(t nil)
                      (reverse *crm-provider-reasoning-enabled-values*)))
    (crm-check "reasoning recovery never persists or replays private scratch"
               (and (null (gethash "assistant_message"
                                   (%recursive-event-payload
                                    (first responses))))
                    (null (search "private scratch"
                                  (shasht:write-json second-messages nil)))
                    (some (lambda (message)
                            (and (string= "system"
                                          (gethash "role" message ""))
                                 (string=
                                  *conscious-recursive-reasoning-recovery-instruction*
                                  (gethash "content" message ""))))
                          second-messages)))))

;; A reasoning-enabled call that exceeds the provider wall-clock boundary uses
;; the same durable one-shot recovery state. Completed tool transcript entries
;; are projected into the retry; only hidden reasoning is disabled.
(setf *crm-events* nil
      *crm-provider-calls* 0
      *crm-provider-messages* nil
      *crm-provider-reasoning-enabled-values* nil
      *crm-provider-script* (list :timeout "answer after timeout")
      *conscious-recursive-mind-sequence* 0)
(let ((*conscious-conversation-provider-profile*
        (obj "reasoning" (obj "enabled" t "effort" "medium"))))
  (conscious-recursive-mind-configure
   :agent-id "recursive-fixture"
   :endpoint "http://127.0.0.1:1234/v1/chat/completions"
   :model "fixture" :context-profile (obj))
  (let* ((result (conscious-recursive-mind-submit "recover timed-out reasoning"))
         (responses
           (remove "model-response" *crm-events*
                   :key (lambda (event) (gethash "type" event ""))
                   :test-not #'string=))
         (first-payload (%recursive-event-payload (first responses)))
         (second-messages (first *crm-provider-messages*)))
    (crm-check "reasoning timeout recovers once without failing the root"
               (and (string= "replied" (gethash "status" result))
                    (string= "answer after timeout" (gethash "content" result))
                    (= 2 *crm-provider-calls*)))
    (crm-check "reasoning timeout recovery is durable and content-free"
               (and (string= "reasoning-recovery-required"
                             (gethash "status" first-payload ""))
                    (string= "provider-timeout"
                             (gethash "recovery_cause" first-payload ""))
                    (string= "provider-call-timeout"
                             (gethash "error_code" first-payload ""))
                    (null (gethash "assistant_message" first-payload))))
    (crm-check "reasoning timeout retry disables reasoning and retains context"
               (and (equal '(t nil)
                           (reverse *crm-provider-reasoning-enabled-values*))
                    (some (lambda (message)
                            (and (string= "system" (gethash "role" message ""))
                                 (string=
                                  *conscious-recursive-reasoning-timeout-recovery-instruction*
                                  (gethash "content" message ""))))
                          second-messages)))))

;; An explicitly no-reasoning request has no cheaper reasoning mode to fall
;; back to. Its timeout remains terminal instead of becoming a retry loop.
(setf *crm-events* nil
      *crm-provider-calls* 0
      *crm-provider-reasoning-enabled-values* nil
      *crm-provider-script* (list :timeout "must not run")
      *conscious-recursive-mind-sequence* 0)
(let ((*conscious-conversation-provider-profile*
        (obj "reasoning" (obj "enabled" nil))))
  (conscious-recursive-mind-configure
   :agent-id "recursive-fixture"
   :endpoint "http://127.0.0.1:1234/v1/chat/completions"
   :model "fixture" :context-profile (obj))
  (let ((result (conscious-recursive-mind-submit "no retry without reasoning")))
    (crm-check "no-reasoning timeout remains one terminal provider boundary"
               (and (string= "failed" (gethash "status" result))
                    (= 1 *crm-provider-calls*)
                    (equal '(nil)
                           (reverse *crm-provider-reasoning-enabled-values*))))))

;; Recovery is exactly once. A second reasoning-only response fails closed and
;; cannot become public or leak either private trace into the ledger.
(setf *crm-events* nil
      *crm-provider-calls* 0
      *crm-provider-messages* nil
      *crm-provider-reasoning-enabled-values* nil
      *crm-provider-script*
      (list (list :reasoning-only "first private scratch")
            (list :reasoning-only "second private scratch"))
      *conscious-recursive-mind-sequence* 0)
(let ((*conscious-conversation-provider-profile*
        (obj "reasoning" (obj "enabled" t))))
  (conscious-recursive-mind-configure
   :agent-id "recursive-fixture"
   :endpoint "http://127.0.0.1:1234/v1/chat/completions"
   :model "fixture" :context-profile (obj))
  (let ((result (conscious-recursive-mind-submit "bounded recovery failure")))
    (crm-check "reasoning recovery runs at most once"
               (and (string= "failed" (gethash "status" result))
                    (= 2 *crm-provider-calls*)
                    (= 1
                       (count-if
                        (lambda (event)
                          (and (string= "model-response"
                                        (gethash "type" event ""))
                               (string= "reasoning-recovery-required"
                                        (gethash
                                         "status"
                                         (%recursive-event-payload event) ""))))
                        *crm-events*))))
    (crm-check "failed reasoning recovery publishes no assistant message"
               (notany (lambda (event)
                         (string= "agent-message"
                                  (gethash "type" event "")))
                       *crm-events*))))

;; A recovery call has no private reserve. It is admitted only when that
;; concrete second request still fits the shared session ceiling.
(setf *crm-events* nil
      *crm-provider-calls* 0
      *crm-provider-script*
      (list (list :reasoning-only "budgeted private scratch"))
      *conscious-recursive-mind-sequence* 0)
(let ((*crm-openrouter-p* t)
      (*crm-openrouter-request-bound* 0.005d0)
      (*crm-provider-cost-per-call* 0.005d0)
      (*conscious-conversation-provider-spent-usd* 0d0)
      (*conscious-conversation-cost-ceiling-usd* 0.007d0)
      (*conscious-recursive-mind-endpoint*
        "https://openrouter.ai/api/v1/chat/completions")
      (*conscious-conversation-provider-profile*
        (obj "reasoning" (obj "enabled" t))))
  (conscious-recursive-mind-configure
   :agent-id "recursive-fixture"
   :endpoint "http://127.0.0.1:1234/v1/chat/completions"
   :model "fixture" :context-profile (obj))
  ;; CONFIGURE owns the normal endpoint; select the bounded OpenRouter fixture
  ;; only for this submitted root.
  (let ((*conscious-recursive-mind-endpoint*
          "https://openrouter.ai/api/v1/chat/completions"))
    (let ((result (conscious-recursive-mind-submit "budget recovery")))
      (crm-check "reasoning recovery pauses when its actual call does not fit"
                 (and (string= "paused-budget" (gethash "status" result))
                      (= 1 *crm-provider-calls*))))))

;; A terminal provider failure is truthful recent history. The next operator
;; message needs no retry-language classifier because the exact unanswered
;; root and runtime-owned failure marker are both supplied.
(setf *crm-events* nil
      *crm-provider-cost-per-call* nil
      *crm-openrouter-p* nil
      *crm-provider-script* (list :invalid "recovered answer")
      *crm-history-contents* nil)
(let ((failed
        (conscious-recursive-mind-submit
         "What tools would let curiosity act on the world?")))
  (crm-check "structurally invalid response leaves the question unanswered"
             (and (string= "failed" (gethash "status" failed))
                  (null (find "agent-message" *crm-events*
                              :key (lambda (event)
                                     (gethash "type" event ""))
                              :test #'string= :from-end t
                              :start (max 0 (- (length *crm-events*) 3)))))))
;; Reconfigure to prove no process-local retry state is required.
(setf *conscious-recursive-mind-sequence* 0)
(conscious-recursive-mind-configure
 :agent-id "recursive-fixture"
 :endpoint "http://127.0.0.1:1234/v1/chat/completions"
 :model "fixture" :context-profile (obj))
(let ((retried (conscious-recursive-mind-submit "Can you try again?"))
      (history (first *crm-history-contents*)))
  (crm-check "ambiguous retry succeeds after restart"
             (and (string= "replied" (gethash "status" retried))
                  (string= "recovered answer" (gethash "content" retried))))
  (crm-check "retry context contains the unanswered question exactly once"
             (= 1 (count-if
                   (lambda (content)
                     (search "What tools would let curiosity act on the world?"
                             content))
                   history)))
  (crm-check "retry context identifies the absent assistant reply"
             (some (lambda (content)
                     (search "no assistant reply was committed" content))
                   history)))
(let ((before-replies
        (count "agent-message" *crm-events*
               :key (lambda (event) (gethash "type" event ""))
               :test #'string=)))
  (setf *crm-provider-mode* :tool)
  (let ((rejected (conscious-recursive-mind-submit "try a tool")))
    (crm-check "unavailable V1 tool response fails closed"
               (string= "failed" (gethash "status" rejected)))
    (crm-check "rejected response cannot become public"
               (= before-replies
                  (count "agent-message" *crm-events*
                         :key (lambda (event) (gethash "type" event ""))
                         :test #'string=)))))
(let ((before *crm-provider-calls*))
  (setf *crm-provider-mode* :reply
        *crm-hide-thread-journals* t)
  (unwind-protect
       (let ((bounded (conscious-recursive-mind-submit "hidden journal probe")))
         (crm-check "V1 hard cap stops repeated inference if journals disappear"
                    (and (string= "failed" (gethash "status" bounded))
                         (= 1 (- *crm-provider-calls* before))
                         (string= "recursive-model-budget-exhausted"
                                  (gethash "error_code" bounded)))))
    (setf *crm-hide-thread-journals* nil)))

;; V2: two native primitive calls recurse through the same owner. The fixture
;; executor is deliberately injected; host authority is qualified separately.
(setf *crm-events* nil
      *crm-provider-calls* 0
      *crm-provider-mode* :reply
      *crm-provider-messages* nil
      *crm-tool-executions* nil
      *crm-provider-script*
      (list (list :tool "bash" "{\"command\":\"find . -name CLAUDE.md\"}"
                  (vector (obj "type" "reasoning.text"
                               "text" "I should inspect the workspace.")))
            (list :tool "lisp-eval" "{\"form\":\"(+ 20 22)\"}")
            "The workspace file is CLAUDE.md and the Lisp result is 42."))
(conscious-recursive-mind-configure
 :agent-id "recursive-fixture"
 :endpoint "http://127.0.0.1:1234/v1/chat/completions"
 :model "fixture" :context-profile (obj)
 :tools-enabled-p t
 :tool-executor
 (lambda (name arguments context)
   (push (list name arguments context) *crm-tool-executions*)
   (cond ((string= name "bash") "./CLAUDE.md")
         ((string= name "lisp-eval") "42")
         (t (error "unexpected fixture tool")))))
(let ((result (conscious-recursive-mind-submit "find the file, then compute")))
  (crm-check "two primitive tools recurse to one grounded public reply"
             (and (string= "replied" (gethash "status" result))
                  (search "CLAUDE.md" (gethash "content" result))
                    (= 3 *crm-provider-calls*)
                  (= 2 (length *crm-tool-executions*))
                  (every (lambda (execution)
                           (hash-table-p (third execution)))
                         *crm-tool-executions*)))
  (let ((history-report (gethash "history_context" result)))
    (crm-check "recursive tool traversal accumulates provider pressure per root"
               (and (= 3 (gethash "provider_boundary_count" history-report))
                    (= 21 (gethash "provider_input_tokens" history-report))
                    (plusp (gethash "provider_message_characters"
                                    history-report)))))
  (crm-check "each primitive execution has one durable intent and result"
             (and (= 2 (count "recursive-tool-execution" *crm-events*
                              :key (lambda (event) (gethash "type" event ""))
                              :test #'string=))
                  (= 2 (count "recursive-tool-result" *crm-events*
                              :key (lambda (event) (gethash "type" event ""))
                              :test #'string=))))
  (crm-check "authority replay includes durable primitive boundary facts"
             (and (= 2 (count "recursive-tool-execution" *crm-events*
                              :key (lambda (event) (gethash "type" event ""))
                              :test #'string=))
                  (= 2 (count "recursive-tool-result" *crm-events*
                              :key (lambda (event) (gethash "type" event ""))
                              :test #'string=))))
  (let* ((requests (reverse *crm-provider-messages*))
         (second-request (second requests))
         (third-request (third requests))
         (first-assistant (second second-request))
         (first-tool (third second-request))
         (second-assistant (fourth third-request))
         (second-tool (fifth third-request)))
    (crm-check "durable native transcript preserves matching call/result IDs"
               (and (string= (gethash "id"
                                      (aref (gethash "tool_calls" first-assistant) 0))
                             (gethash "tool_call_id" first-tool))
                    (string= (gethash "id"
                                      (aref (gethash "tool_calls" second-assistant) 0))
                             (gethash "tool_call_id" second-tool))))
  (crm-check "native tool continuation preserves provider reasoning details"
               (let ((details (gethash "reasoning_details" first-assistant)))
                 (and (vectorp details)
                      (= 1 (length details))
                      (string= "I should inspect the workspace."
                               (gethash "text" (aref details 0) "")))))))

(setf *crm-events* nil
      *crm-provider-calls* 0
      *crm-provider-messages* nil
      *crm-tool-executions* nil
      *crm-provider-script*
      (list (list :tool "brave-search"
                  "{\"query\":\"current stellar disk research\",\"count\":3}")
            (list :tool "web-fetch"
                  "{\"url\":\"https://example.com/research\"}")
            "The current research page supports the stellar-disk finding."))
(conscious-recursive-mind-configure
 :agent-id "recursive-fixture"
 :endpoint "http://127.0.0.1:1234/v1/chat/completions"
 :model "fixture" :context-profile (obj)
 :tools-enabled-p t
 :tool-executor
 (lambda (name arguments context)
   (push (list name arguments context) *crm-tool-executions*)
   (cond ((string= name "brave-search")
          "1. Current research\nhttps://example.com/research")
         ((string= name "web-fetch")
          "{\"content\":\"A bounded research page.\"}")
         (t (error "unexpected web fixture tool")))))
(let ((result (conscious-recursive-mind-submit
               "Research the current stellar-disk evidence.")))
  (crm-check "bounded search and fetch recurse through the native tool loop"
             (and (string= "replied" (gethash "status" result))
                  (= 2 (length *crm-tool-executions*))
                  (equal '("web-fetch" "brave-search")
                         (mapcar #'first *crm-tool-executions*))))
  (crm-check "search and fetch arguments remain structured runtime facts"
             (let ((search (second (second *crm-tool-executions*)))
                   (fetch (second (first *crm-tool-executions*))))
               (and (= 3 (gethash "count" search))
                    (string= "current stellar disk research"
                             (gethash "query" search))
                    (string= "https://example.com/research"
                             (gethash "url" fetch))))))

;; V3: one exact curiosity gains salience only through three distinct durable
;; conversational roots, then the same trampoline investigates it privately.
(setf *crm-events* nil
      *crm-provider-calls* 0
      *crm-provider-messages* nil
      *crm-provider-tool-counts* nil
      *crm-provider-script* (list "We have an unresolved design question."))
(conscious-recursive-mind-configure
 :agent-id "recursive-fixture"
 :endpoint "http://127.0.0.1:1234/v1/chat/completions"
 :model "fixture" :context-profile (obj)
 :tools-enabled-p t :curiosity-enabled-p t
 :review-ready-fn (lambda () t)
 :tool-executor (lambda (&rest ignored)
                  (declare (ignore ignored)) "unused"))
(crm-check "quiet review does not advertise deliberate curiosity in conversation"
           (null
            (find "record-curiosity"
                  (%recursive-items (%recursive-tool-schemas t))
                  :key (lambda (schema)
                         (gethash "name" (gethash "function" schema)))
                  :test #'string=)))
(crm-check "review recurrence identity follows sealed root order"
           (equal '(10 20)
                  (%recursive-curiosity-review-root-ids
                   '(21 11 10)
                   (vector (obj "user_event_id" 10 "agent_event_id" 11)
                           (obj "user_event_id" 20 "agent_event_id" 21)))))
(conscious-recursive-mind-submit
 "We still do not know why the projection cache grows after terminal work.")
(let* ((user (find "user-message" *crm-events*
                   :key (lambda (event) (gethash "type" event ""))
                   :test #'string=))
       (reply (find "agent-message" *crm-events*
                    :key (lambda (event) (gethash "type" event ""))
                    :test #'string=))
       (arguments
         (shasht:write-json
          (obj "question" "Why does the projection cache keep growing?"
               "evidence_event_ids"
               (vector (gethash "id" user) (gethash "id" reply)))
          nil)))
  (setf *crm-provider-script*
        (list (list :tool "notice-curiosity" arguments)))
  (let ((review (conscious-recursive-curiosity-review-one)))
    (crm-check "ordinary conversation opens one grounded curiosity review"
               (and (string= "review-completed" (gethash "status" review))
                    (= 1 (gethash "observation_count" review))
                    (= 1 (count "recursive-curiosity-review-opened" *crm-events*
                                :key (lambda (event) (gethash "type" event ""))
                                :test #'string=))
                    (= 1 (count "recursive-curiosity-review-completed" *crm-events*
                                :key (lambda (event) (gethash "type" event ""))
                                :test #'string=))))
    (crm-check "review receives the selected persona policy"
               (let* ((request (first *crm-provider-messages*))
                      (payload (and request (second request))))
                 (and (hash-table-p payload)
                      (search "persona_policy"
                              (gethash "content" payload "")))))
    (let* ((observation
             (find "conscious-curiosity-observed" *crm-events*
                   :key (lambda (event) (gethash "type" event ""))
                   :test #'string=))
           (supporting
             (gethash "supporting_event_ids" (gethash "payload" observation))))
      (crm-check "review observation cites only exact sealed evidence"
                 (and (= 2 (length supporting))
                      (find (gethash "id" user) supporting :test #'equal)
                      (find (gethash "id" reply) supporting :test #'equal))))
    (let ((inspection (conscious-recursive-curiosity-inspect)))
      (crm-check "first review observation is immediately inspectable"
                 (and (= 1 (gethash "observation_count" inspection))
                      (string= "recursive-curiosity-review-v1"
                               (gethash
                                "source_revision"
                                (aref (gethash "observations" inspection) 0)))
                      (search "projection cache"
                              (gethash "question"
                                       (aref (gethash "observations" inspection)
                                             0)))))))
  (let ((calls-before *crm-provider-calls*))
    (crm-check "sealed review batch is not inferred twice"
               (and (string= "idle"
                             (gethash "status"
                                      (conscious-recursive-curiosity-review-one)))
                    (= calls-before *crm-provider-calls*)))))

;; A later batch may reinforce the runtime-supplied motive identity, but it
;; still has to cite exact new evidence from that batch.
(setf *crm-provider-script* (list "The same issue remains unresolved."))
(conscious-recursive-mind-submit
 "We saw fresh evidence that the projection cache still grows.")
(let* ((user (find "user-message" *crm-events*
                   :key (lambda (event) (gethash "type" event ""))
                   :test #'string= :from-end t))
       (reply (find "agent-message" *crm-events*
                    :key (lambda (event) (gethash "type" event ""))
                    :test #'string= :from-end t))
       (observation
         (find "conscious-curiosity-observed" *crm-events*
               :key (lambda (event) (gethash "type" event ""))
               :test #'string= :from-end t))
       (motive-id (gethash "motive_id" (gethash "payload" observation)))
       (arguments
         (shasht:write-json
          (obj "motive_id" motive-id
               "evidence_event_ids"
               (vector (gethash "id" user) (gethash "id" reply)))
          nil)))
  (setf *crm-provider-script*
        (list (list :tool "reinforce-curiosity" arguments)))
  (let ((review (conscious-recursive-curiosity-review-one)))
    (crm-check "native reinforcement strengthens the existing motive"
               (and (string= "review-completed" (gethash "status" review))
                    (= 1 (gethash "observation_count" review))
                    (= 2
                       (count-if
                        (lambda (event)
                          (let ((payload (gethash "payload" event)))
                            (and (string= "conscious-curiosity-observed"
                                          (gethash "type" event ""))
                                 (hash-table-p payload)
                                 (equal motive-id
                                        (gethash "motive_id" payload)))))
                        *crm-events*))))))

;; A second committed batch can correctly produce no observation. Native
;; review metadata remains runtime-owned even when the model chooses no tool.
(setf *crm-provider-script* (list "That was ordinary conversational closure."))
(conscious-recursive-mind-submit "Thanks, that is all for now.")
(setf *crm-provider-script* (list "No unresolved question warrants recording."))
(let ((before
        (count "conscious-curiosity-observed" *crm-events*
               :key (lambda (event) (gethash "type" event ""))
               :test #'string=))
      (review (conscious-recursive-curiosity-review-one)))
  (crm-check "irrelevant conversation seals a review with no observation"
             (and (string= "review-completed" (gethash "status" review))
                  (= 0 (gethash "observation_count" review))
                  (= before
                     (count "conscious-curiosity-observed" *crm-events*
                            :key (lambda (event) (gethash "type" event ""))
                            :test #'string=)))))

;; Open-ended quiet review owns no completion cap.  If a reasoning-capable
;; provider returns only private scratch, the durable first outcome authorizes
;; exactly one reasoning-disabled retry on the next quiet step.
(setf *crm-provider-script* (list "A fresh reviewable conversation reply."))
(conscious-recursive-mind-submit
 "Check that an uncapped quiet review can recover from reasoning-only output.")
(setf *crm-provider-calls* 0
      *crm-provider-messages* nil
      *crm-provider-output-caps* nil
      *crm-provider-reasoning-enabled-values* nil
      *crm-provider-reasoning-values* nil
      *crm-provider-script*
      (list (list :reasoning-only "review scratch must not persist")
            "No curiosity should be recorded."))
(let ((*conscious-conversation-provider-profile*
        (obj "reasoning" (obj "enabled" t))))
  (let* ((first (conscious-recursive-curiosity-review-one))
         (reasoning-response
           (find-if
            (lambda (event)
              (let ((payload (%recursive-event-payload event)))
                (and (string= "model-response" (gethash "type" event ""))
                     (hash-table-p payload)
                     (gethash "private_review" payload)
                     (string= "reasoning-recovery-required"
                              (gethash "status" payload "")))))
            *crm-events* :from-end t)))
    (crm-check "reasoning-only quiet review waits for one durable recovery step"
               (and (string= "reasoning-recovery-required"
                             (gethash "status" first))
                    reasoning-response
                    (null (gethash "assistant_message"
                                   (%recursive-event-payload
                                    reasoning-response)))
                    (null (search "review scratch"
                                  (shasht:write-json *crm-events* nil)))))
    (let ((second (conscious-recursive-curiosity-review-one)))
      (crm-check "quiet review recovery completes without a completion cap"
                 (and (string= "review-completed" (gethash "status" second))
                      (= 2 *crm-provider-calls*)
                      (equal '(nil nil) (reverse *crm-provider-output-caps*))))
      (crm-check "quiet review disables reasoning only for its recovery call"
                 (let* ((values (reverse *crm-provider-reasoning-values*))
                        (initial (first values))
                        (recovery (second values)))
                   (and (= 2 (length values))
                        (string=
                         *conscious-recursive-mind-private-reasoning-effort*
                         (gethash "effort" initial ""))
                        (not (nth-value 1 (gethash "enabled" initial)))
                        (nth-value 1 (gethash "enabled" recovery))
                        (null (gethash "enabled" recovery))
                        (eq t
                            (gethash
                             "enabled"
                             (gethash "reasoning"
                                      *conscious-conversation-provider-profile*)))))))))

;; Readiness and operator priority are checked after the mind lock is owned,
;; before a review request or opened receipt can be appended.
(setf *crm-provider-script* (list "A new committed reply."))
(conscious-recursive-mind-submit "One more topic remains.")
(let ((calls-before *crm-provider-calls*)
      (events-before (length *crm-events*)))
  (conscious-recursive-mind-configure
   :agent-id "recursive-fixture"
   :endpoint "http://127.0.0.1:1234/v1/chat/completions"
   :model "fixture" :context-profile (obj)
   :tools-enabled-p t :curiosity-enabled-p t
   :review-ready-fn (lambda () nil)
   :tool-executor (lambda (&rest ignored) (declare (ignore ignored)) "unused"))
  (crm-check "short pause does not open a review boundary"
             (and (string= "not-quiet"
                           (gethash "status"
                                    (conscious-recursive-curiosity-review-one)))
                  (= calls-before *crm-provider-calls*)
                  (= events-before (length *crm-events*)))))
(conscious-recursive-mind-configure
 :agent-id "recursive-fixture"
 :endpoint "http://127.0.0.1:1234/v1/chat/completions"
 :model "fixture" :context-profile (obj)
 :tools-enabled-p t :curiosity-enabled-p t
 :review-ready-fn (lambda () t)
 :tool-executor (lambda (&rest ignored) (declare (ignore ignored)) "unused"))
(let ((calls-before *crm-provider-calls*)
      (events-before (length *crm-events*)))
  (setf *conscious-recursive-mind-operator-pending-p* t)
  (unwind-protect
       (crm-check "waiting operator input preempts review before inference"
                  (and (string= "preempted"
                                (gethash
                                 "status"
                                 (conscious-recursive-curiosity-review-one)))
                       (= calls-before *crm-provider-calls*)
                       (= events-before (length *crm-events*))))
    (setf *conscious-recursive-mind-operator-pending-p* nil)))

;; The evidence validator rejects a fabricated event ID. The opened range and
;; failed provider receipt remain durable, but no completion or observation is
;; manufactured; a later invocation may deterministically retry that range.
(setf *crm-provider-script*
      (list (list :tool "notice-curiosity"
                  "{\"question\":\"Fabricated\",\"evidence_event_ids\":[999999]}")))
(let ((observations-before
        (count "conscious-curiosity-observed" *crm-events*
               :key (lambda (event) (gethash "type" event ""))
               :test #'string=))
      (completed-before
        (count "recursive-curiosity-review-completed" *crm-events*
               :key (lambda (event) (gethash "type" event ""))
               :test #'string=)))
  (let ((review (conscious-recursive-curiosity-review-one)))
    (crm-check "out-of-batch review evidence fails closed"
               (and (string= "failed" (gethash "status" review))
                    (= observations-before
                       (count "conscious-curiosity-observed" *crm-events*
                              :key (lambda (event) (gethash "type" event ""))
                              :test #'string=))
                    (= completed-before
                       (count "recursive-curiosity-review-completed" *crm-events*
                              :key (lambda (event) (gethash "type" event ""))
                              :test #'string=))))))

;; Close the failed range, then prove an accepted response is restart-safe:
;; preemption after response journaling must reuse that response without a
;; second provider request or a duplicate observation.
(setf *crm-provider-script* (list "No curiosity should be recorded."))
(crm-check "failed review range can retry to one completed receipt"
           (string= "review-completed"
                    (gethash "status"
                             (conscious-recursive-curiosity-review-one))))
(setf *crm-provider-script* (list "This issue remains unresolved."))
(conscious-recursive-mind-submit
 "A restart between review inference and commit must not resample the model.")
(let* ((user (find "user-message" *crm-events*
                   :key (lambda (event) (gethash "type" event ""))
                   :test #'string= :from-end t))
       (reply (find "agent-message" *crm-events*
                    :key (lambda (event) (gethash "type" event ""))
                    :test #'string= :from-end t))
       (arguments
         (shasht:write-json
          (obj "question" "Can an accepted curiosity review resume exactly?"
               "evidence_event_ids"
               (vector (gethash "id" user) (gethash "id" reply)))
          nil)))
  (setf *crm-provider-script*
        (list (list :tool "notice-curiosity" arguments))
        *crm-pend-after-provider-p* t)
  (let ((paused (conscious-recursive-curiosity-review-one)))
    (crm-check "post-response operator admission preempts before review commit"
               (and (string= "preempted" (gethash "status" paused))
                    (null (find "recursive-curiosity-review-completed"
                                *crm-events*
                                :key (lambda (event) (gethash "type" event ""))
                                :test #'string= :from-end t
                                :start (max 0 (- (length *crm-events*) 3)))))))
  (setf *conscious-recursive-mind-operator-pending-p* nil)
  (let ((calls-before *crm-provider-calls*)
        (resumed (conscious-recursive-curiosity-review-one)))
    (crm-check "accepted review response resumes without provider resampling"
               (and (string= "review-completed" (gethash "status" resumed))
                    (= 1 (gethash "observation_count" resumed))
                    (= calls-before *crm-provider-calls*)))))

(let* ((open-register
         (vector (obj "motive_id" "motive:available"
                      "question" "What is worth pursuing?" "phase" "open"
                      "observation_event_ids" (vector 101))))
       (unavailable-arguments
         (shasht:write-json
          (obj "question" "Should this unavailable motive be pursued?"
               "source_motive_ids" (vector "motive:invented")
               "evidence_event_ids" (vector 101))
          nil))
       (valid-arguments
         (shasht:write-json
          (obj "question" "Should this available motive be pursued?"
               "source_motive_ids" (vector "motive:available")
               "evidence_event_ids" (vector 101))
          nil))
       (unavailable-call
         (obj "type" "function" "function"
              (obj "name" "choose-curiosity"
                   "arguments" unavailable-arguments)))
       (valid-call
         (obj "type" "function" "function"
              (obj "name" "choose-curiosity" "arguments" valid-arguments)))
       (invented-refused-p nil)
       (multiple-refused-p nil))
  (handler-case
      (%recursive-curiosity-attention-choice
       (obj "tool_calls" (vector unavailable-call)) open-register)
    (error () (setf invented-refused-p t)))
  (handler-case
      (%recursive-curiosity-attention-choice
       (obj "tool_calls" (vector valid-call valid-call)) open-register)
    (error () (setf multiple-refused-p t)))
  (crm-check "attention cannot invent a source motive for pursuit"
             invented-refused-p)
  (crm-check "one attention revision cannot open multiple pursuits"
             multiple-refused-p))

(setf *crm-events* nil
      *crm-provider-calls* 0
      *crm-provider-script* nil)
(conscious-recursive-mind-configure
 :agent-id "recursive-fixture"
 :endpoint "http://127.0.0.1:1234/v1/chat/completions"
 :model "fixture" :context-profile (obj)
 :tools-enabled-p t :curiosity-enabled-p t
 :review-ready-fn (lambda () t)
 :tool-executor (lambda (&rest ignored)
                  (declare (ignore ignored)) "unused"))
(log-event "user-message"
           (obj "text" "A one-off but resonant question."
                "channel" "terminal"))
(let ((root-id (length *crm-events*)))
  (%recursive-record-curiosity
   "Why does this one-off question keep pulling?" root-id
   :supporting-event-ids (list root-id)
   :evidence-identity-event-ids (list root-id)
   :source-revision "recursive-attention-fixture-v1"))
(setf *crm-provider-script* (list "Nothing warrants attention right now."))
(let* ((first (conscious-recursive-curiosity-attention-one))
       (calls-after-first *crm-provider-calls*)
       (second (conscious-recursive-curiosity-attention-one))
       (events-after-quiescence (length *crm-events*))
       (third (conscious-recursive-curiosity-attention-one)))
  (crm-check "attention decline settles its page then quiesces the generation"
             (and (string= "attention-declined" (gethash "status" first))
                  (string= "attention-quiescent" (gethash "status" second))
                  (string= "quiescent" (gethash "status" third))
                  (= calls-after-first *crm-provider-calls*)
                  (= events-after-quiescence (length *crm-events*))
                  (= 1 (count "recursive-curiosity-attention-declined"
                              *crm-events*
                              :key (lambda (event)
                                     (gethash "type" event ""))
                              :test #'string=))
                  (= 1 (count "recursive-curiosity-attention-quiescent"
                              *crm-events*
                              :key (lambda (event)
                                     (gethash "type" event ""))
                              :test #'string=)))))
(log-event "user-message"
           (obj "text" "The question surfaced through new evidence."
                "channel" "terminal"))
(let ((root-id (length *crm-events*)))
  (%recursive-record-curiosity
   "Why does this one-off question keep pulling?" root-id
   :supporting-event-ids (list root-id)
   :evidence-identity-event-ids (list root-id)
   :source-revision "recursive-attention-fixture-v1"))
(setf *crm-provider-script* (list "Still not now."))
(let ((calls-before *crm-provider-calls*)
      (reconsidered (conscious-recursive-curiosity-attention-one)))
  (crm-check "new observation revises the register and permits reconsideration"
             (and (string= "attention-declined"
                           (gethash "status" reconsidered))
                  (= (1+ calls-before) *crm-provider-calls*)
                  (= 2 (count "recursive-curiosity-attention-declined"
                              *crm-events*
                              :key (lambda (event)
                                     (gethash "type" event ""))
                              :test #'string=)))))

;; Retained and corrected knowledge changes what attention is deciding about,
;; even when the motive label and observation evidence are unchanged.  The
;; frontier must therefore participate in both register and page identity.
(multiple-value-bind (open-register total-open)
    (%recursive-curiosity-open-register (%recursive-thread-events))
  (declare (ignore total-open))
  (let* ((motive-id (gethash "motive_id" (aref open-register 0)))
         (revision-before
           (%recursive-curiosity-attention-revision open-register))
         (result-id
           (log-event
            "recursive-curiosity-result"
            (obj "schema_version" 1 "thread_id" "thread:attention-frontier"
                 "motive_id" motive-id
                 "model_call_id" "model:attention-frontier"
                 "source_motive_ids" (vector motive-id)
                 "runtime_revision" "fixture" "status" "completed"
                 "audience" "private"
                 "content" "The retained finding creates one sharper edge."
                 "completed_at" (get-universal-time)))))
    (log-event
     "recursive-curiosity-incorporation-completed"
     (obj "schema_version" 1 "result_event_id" result-id
          "disposition" "retained"
          "summary" "A retained finding creates one sharper edge"
          "decline_reason" :null
          "memory_node_id" "curiosity-finding-attention-frontier"
          "evidence_event_id" result-id "reach_out_event_id" :null
          "runtime_revision" "fixture"
          "completed_at" (get-universal-time)))
    (multiple-value-bind (advanced-register ignored)
        (%recursive-curiosity-open-register (%recursive-thread-events))
      (declare (ignore ignored))
      (crm-check "retained knowledge changes the attention generation identity"
                 (not (string=
                       revision-before
                       (%recursive-curiosity-attention-revision
                        advanced-register)))))
    (setf *crm-provider-script*
          (list "The retained finding does not yet warrant another focus."))
    (let ((calls-before *crm-provider-calls*)
          (reconsidered (conscious-recursive-curiosity-attention-one)))
      (crm-check "retained knowledge reopens settled attention"
                 (and (string= "attention-declined"
                               (gethash "status" reconsidered))
                      (= (1+ calls-before) *crm-provider-calls*))))
    (let ((revision-before-correction
            (multiple-value-bind (register ignored)
                (%recursive-curiosity-open-register
                 (%recursive-thread-events))
              (declare (ignore ignored))
              (%recursive-curiosity-attention-revision register))))
      (conscious-recursive-curiosity-supersede-finding
       result-id "Fixture correction changes the available prior knowledge.")
      (multiple-value-bind (corrected-register ignored)
          (%recursive-curiosity-open-register (%recursive-thread-events))
        (declare (ignore ignored))
        (crm-check "supersession changes the attention generation identity"
                   (not (string=
                         revision-before-correction
                         (%recursive-curiosity-attention-revision
                          corrected-register)))))
      (setf *crm-provider-script*
            (list "The correction is now considered but no focus pulls."))
      (let ((calls-before *crm-provider-calls*)
            (reconsidered (conscious-recursive-curiosity-attention-one)))
        (crm-check "knowledge correction reopens settled attention"
                   (and (string= "attention-declined"
                                 (gethash "status" reconsidered))
                        (= (1+ calls-before) *crm-provider-calls*)))))))

;; More open motives than one model boundary may carry must rotate through
;; deterministic pages. A first-page decline cannot starve a later motive.
(setf *crm-events* nil
      *crm-provider-calls* 0
      *crm-provider-script* nil)
(dotimes (index 34)
  (let ((source-id
          (log-event "user-message"
                     (obj "text" (format nil "curiosity seed ~2,'0d" index)
                          "channel" "terminal"))))
    (%recursive-record-curiosity
     (format nil "What follows from bounded curiosity seed ~2,'0d?" index)
     source-id :supporting-event-ids (list source-id)
     :evidence-identity-event-ids (list source-id)
     :source-revision "recursive-attention-pagination-fixture")))
(multiple-value-bind (full-register total-open)
    (%recursive-curiosity-open-register *crm-events* most-positive-fixnum 0)
  (let* ((target (aref full-register 20))
         (target-motive (gethash "motive_id" target))
         (target-evidence (gethash "observation_event_ids" target))
         (arguments
           (shasht:write-json
            (obj "question" (gethash "question" target)
                 "source_motive_ids" (vector target-motive)
                 "evidence_event_ids" target-evidence)
            nil)))
    (setf *crm-provider-script*
          (list "The first page does not pull strongly enough."
                (list :tool "choose-curiosity" arguments)))
    (let ((first (conscious-recursive-curiosity-attention-one))
          (second (conscious-recursive-curiosity-attention-one)))
      (crm-check "attention rotates beyond a declined first page"
                 (and (= 34 total-open)
                      (string= "attention-declined" (gethash "status" first))
                      (= 0 (gethash "page_offset" first))
                      (string= "focus-chosen" (gethash "status" second))
                      (= 20 (gethash "page_offset" second))
                      (= 2 *crm-provider-calls*)))
      (let* ((focus-id (gethash "focus_event_id" second))
             (focus (find focus-id *crm-events*
                          :key (lambda (event) (gethash "id" event))
                          :test #'equal)))
        (crm-check "second-page focus retains the supplied motive identity"
                   (and focus
                        (find target-motive
                              (gethash "source_motive_ids"
                                       (%recursive-event-payload focus))
                              :test #'string=)))))))

;; When every page declines, the generation receives one terminal receipt.
;; Further quiet wakes are read-only and make no provider request.
(setf *crm-events* nil
      *crm-provider-calls* 0
      *crm-provider-script* nil)
(dotimes (index 34)
  (let ((source-id
          (log-event "user-message"
                     (obj "text" (format nil "quiet seed ~2,'0d" index)
                          "channel" "terminal"))))
    (%recursive-record-curiosity
     (format nil "Is quiet curiosity seed ~2,'0d worth pursuing?" index)
     source-id :supporting-event-ids (list source-id)
     :evidence-identity-event-ids (list source-id)
     :source-revision "recursive-attention-quiescence-fixture")))
(setf *crm-provider-script* (list "Not this page." "Not the next page."))
(let* ((first (conscious-recursive-curiosity-attention-one))
       (second (conscious-recursive-curiosity-attention-one))
       (third (conscious-recursive-curiosity-attention-one))
       (event-count (length *crm-events*))
       (fourth (conscious-recursive-curiosity-attention-one)))
  (crm-check "all settled pages produce one silent durable quiescence"
             (and (string= "attention-declined" (gethash "status" first))
                  (string= "attention-declined" (gethash "status" second))
                  (string= "attention-quiescent" (gethash "status" third))
                  (string= "quiescent" (gethash "status" fourth))
                  (= 2 *crm-provider-calls*)
                  (= event-count (length *crm-events*))
                  (= 1 (count "recursive-curiosity-attention-quiescent"
                              *crm-events*
                              :key (lambda (event)
                                     (gethash "type" event ""))
                              :test #'string=)))))

(setf *crm-events* nil
      *crm-provider-calls* 0
      *crm-provider-messages* nil
      *crm-provider-tool-counts* nil
      *crm-provider-tool-names* nil
      *crm-tool-executions* nil
      *crm-provider-script* (list "That single star raises a real question."))
(conscious-recursive-mind-configure
 :agent-id "recursive-fixture"
 :endpoint "http://127.0.0.1:1234/v1/chat/completions"
 :model "fixture" :context-profile (obj)
 :tools-enabled-p t :curiosity-enabled-p t
 :review-ready-fn (lambda () t)
 :tool-executor
 (lambda (name arguments context)
   (push (list name arguments context) *crm-tool-executions*)
   "workspace evidence"))
(conscious-recursive-mind-submit
 "A nearby star has an unexpected planet-forming disk.")
(let* ((user (find "user-message" *crm-events*
                   :key (lambda (event) (gethash "type" event ""))
                   :test #'string= :from-end t))
       (reply (find "agent-message" *crm-events*
                    :key (lambda (event) (gethash "type" event ""))
                    :test #'string= :from-end t))
       (related-motive-id
         (nth-value
          2
          (%recursive-record-curiosity
           "What determines whether a young stellar disk persists?"
           (gethash "id" user)
           :supporting-event-ids (list (gethash "id" user))
           :evidence-identity-event-ids (list (gethash "id" user))
           :source-revision "recursive-fixture-related-v1")))
       (notice-arguments
         (shasht:write-json
          (obj "question"
               "How can a nearby star retain an unexpected planet-forming disk?"
               "evidence_event_ids"
               (vector (gethash "id" user) (gethash "id" reply)))
          nil)))
  (setf *crm-provider-script*
        (list (list :tool "notice-curiosity" notice-arguments)))
  (let ((review (conscious-recursive-curiosity-review-one)))
    (crm-check "observation review records without choosing attention"
               (and (string= "review-completed" (gethash "status" review))
                    (= 1 (gethash "observation_count" review))
                    (= 0 (gethash "focus_count" review))
                    (= 0 (count "recursive-curiosity-focus-opened" *crm-events*
                                :key (lambda (event)
                                       (gethash "type" event ""))
                                :test #'string=)))))
  (let* ((register (%recursive-curiosity-open-register *crm-events*))
         (new-row
           (find-if (lambda (row)
                      (not (string= related-motive-id
                                    (gethash "motive_id" row))))
                    (coerce register 'list)))
         (new-motive-id (gethash "motive_id" new-row))
         (evidence
           (concatenate 'vector
                        (gethash "observation_event_ids"
                                 (find related-motive-id register
                                       :key (lambda (row)
                                              (gethash "motive_id" row))
                                       :test #'string=))
                        (gethash "observation_event_ids" new-row)))
         (choice-arguments
           (shasht:write-json
            (obj "question"
                 "How can a nearby star retain an unexpected planet-forming disk?"
                 "source_motive_ids"
                 (vector related-motive-id new-motive-id)
                 "evidence_event_ids" evidence)
            nil)))
    (let ((calls-before *crm-provider-calls*)
          (settled-review (conscious-recursive-curiosity-review-one)))
      (crm-check "accumulated curiosities reach attention without a new review batch"
                 (and (member (gethash "status" settled-review)
                              '("idle" "revision-settled") :test #'string=)
                      (= calls-before *crm-provider-calls*))))
    (setf *crm-provider-script*
          (list (list :tool "choose-curiosity" choice-arguments)))
    (let ((attention (conscious-recursive-curiosity-attention-one)))
      (crm-check "one persona-resonant observation may win separate attention"
                 (and (string= "focus-chosen"
                               (gethash "status" attention))
                      (= 1 (count "recursive-curiosity-focus-opened" *crm-events*
                                  :key (lambda (event)
                                         (gethash "type" event ""))
                                  :test #'string=))
                      (= 2
                         (length
                          (gethash
                           "source_motive_ids"
                           (gethash
                            "payload"
                            (find "recursive-curiosity-focus-opened" *crm-events*
                                  :key (lambda (event)
                                         (gethash "type" event ""))
                                  :test #'string= :from-end t))))))))))
(let* ((focus
         (find "recursive-curiosity-focus-opened" *crm-events*
               :key (lambda (event) (gethash "type" event ""))
               :test #'string= :from-end t))
       (payload (gethash "payload" focus))
       (source-ids (coerce (gethash "source_motive_ids" payload) 'list))
       (focuses-before
         (count "recursive-curiosity-focus-opened" *crm-events*
                :key (lambda (event) (gethash "type" event ""))
                :test #'string=)))
  (multiple-value-bind (recovered appended-p)
      (%recursive-ensure-curiosity-focus
       (gethash "caused_by" focus)
       (gethash "question" payload)
       (first source-ids) (rest source-ids)
       (coerce (gethash "supporting_event_ids" payload) 'list))
    (crm-check "the same chosen focus recovers without a duplicate append"
               (and (not appended-p)
                    (equal (gethash "id" focus) (gethash "id" recovered))
                    (= focuses-before
                       (count "recursive-curiosity-focus-opened" *crm-events*
                              :key (lambda (event)
                                     (gethash "type" event ""))
                              :test #'string=))))))
(let ((public-before
        (count "agent-message" *crm-events*
               :key (lambda (event) (gethash "type" event ""))
               :test #'string=)))
  (setf *crm-provider-script*
        (list (list :tool "bash" "{\"command\":\"inspect-cache\"}")
              "The cache grows because terminal work rows remain retained."))
  (let ((result (conscious-recursive-curiosity-wake-one)))
    (crm-check "autonomous curiosity uses the same tool-capable trampoline"
               (and (string= "curiosity-completed" (gethash "status" result))
                    (search "terminal work rows" (gethash "content" result))
                    (find-if
                     (lambda (event)
                       (let ((payload (gethash "payload" event)))
                         (and (string= "recursive-tool-result"
                                       (gethash "type" event ""))
                              (string= "bash"
                                       (gethash "tool_name" payload "")))))
                     *crm-events*))))
    (crm-check "autonomous curiosity receives the full configured native tool schema"
               (and (equal '(9 9)
                           (subseq *crm-provider-tool-counts* 0 2))
                    (equal (list *crm-ordinary-native-tool-names*
                                 *crm-ordinary-native-tool-names*)
                           (subseq *crm-provider-tool-names* 0 2))))
    (crm-check "autonomous curiosity context is orient/private"
               (let ((arguments (first *crm-assembly-context-args*)))
                 (and (string= "orient" (getf arguments :purpose))
                      (string= "private" (getf arguments :audience))
                      (find "search-memory"
                            (getf arguments :available-tools)
                            :test #'string=))))
  (crm-check "curiosity commits privately without an operator publication"
             (and (= public-before
                     (count "agent-message" *crm-events*
                            :key (lambda (event) (gethash "type" event ""))
                            :test #'string=))
                  (= 1 (count "recursive-curiosity-result" *crm-events*
                              :key (lambda (event) (gethash "type" event ""))
                              :test #'string=))))
  (let ((inspection (conscious-recursive-curiosity-inspect)))
    (crm-check "private curiosity result is operator-inspectable"
               (and (= 1 (gethash "focus_count" inspection))
                    (string= "completed"
                             (gethash "status"
                                      (aref (gethash "focuses" inspection) 0)))
                    (= 1 (gethash "result_count" inspection))
                    (search "terminal work rows"
                            (gethash "content"
                                     (aref (gethash "results" inspection) 0)))))))
(let* ((result-event
         (find "recursive-curiosity-result" *crm-events*
               :key (lambda (event) (gethash "type" event ""))
               :test #'string= :from-end t))
       (source-ids
         (gethash "source_motive_ids" (gethash "payload" result-event)))
       (arguments
         (shasht:write-json
          (obj "source_motive_ids" source-ids) nil)))
  (setf *crm-provider-script*
        (list (list :tool "close-curiosity" arguments)))
  (let* ((reviewed (conscious-recursive-curiosity-result-review-one))
         (calls-after *crm-provider-calls*)
         (again (conscious-recursive-curiosity-result-review-one)))
    (crm-check "completed investigation recursively closes satisfied motives"
               (and (string= "result-reviewed" (gethash "status" reviewed))
                    (string= "closed" (gethash "disposition" reviewed))
                    (= (length source-ids)
                       (count "conscious-curiosity-satisfaction-observed"
                              *crm-events*
                              :key (lambda (event)
                                     (gethash "type" event ""))
                              :test #'string=))))
    (let* ((completion
             (find "recursive-curiosity-result-review-completed" *crm-events*
                   :key (lambda (event) (gethash "type" event ""))
                   :test #'string= :from-end t))
           (completion-id (gethash "id" completion))
           (satisfactions
             (remove-if-not
              (lambda (event)
                (and (string= "conscious-curiosity-satisfaction-observed"
                              (gethash "type" event ""))
                     (find (gethash "motive_id" (gethash "payload" event))
                           source-ids :test #'string=)))
              *crm-events*)))
      (crm-check "result-review completion is the deterministic receipt"
                 (and (= (length source-ids) (length satisfactions))
                      (every
                       (lambda (event)
                         (equal completion-id
                                (gethash "receipt_event_id"
                                         (gethash "payload" event))))
                       satisfactions)))
      (multiple-value-bind (open total-open)
          (%recursive-curiosity-open-register *crm-events* 20)
        (declare (ignore total-open))
        (crm-check "closed motives leave the open attention register"
                   (every
                    (lambda (motive-id)
                      (not (find motive-id open
                                 :key (lambda (row)
                                        (gethash "motive_id" row))
                                 :test #'string=)))
                    (coerce source-ids 'list)))))
    (crm-check "completed result review is restart-safe and not resampled"
               (and (string= "idle" (gethash "status" again))
                    (= calls-after *crm-provider-calls*)))
    (setf *crm-events*
          (remove "conscious-curiosity-satisfaction-observed" *crm-events*
                  :key (lambda (event) (gethash "type" event ""))
                  :test #'string=))
    (let ((recovered (conscious-recursive-curiosity-attention-one)))
      (crm-check "attention repairs a missing satisfaction tail before selection"
                 (and (string= "idle" (gethash "status" recovered))
                      (= calls-after *crm-provider-calls*)
                      (= (length source-ids)
                         (count
                          "conscious-curiosity-satisfaction-observed"
                          *crm-events*
                          :key (lambda (event) (gethash "type" event ""))
                          :test #'string=)))))
    (let ((inspection (conscious-recursive-curiosity-inspect)))
      (crm-check "attention and result dispositions are operator-inspectable"
                 (and (plusp (gethash "attention_decision_count" inspection))
                      (= 1 (gethash "result_review_count" inspection))
                      (string= "closed"
                               (gethash
                                "disposition"
                                (aref (gethash "result_reviews" inspection)
                                      0))))))))
(let* ((prior-result
         (find "recursive-curiosity-result" *crm-events*
               :key (lambda (event) (gethash "type" event ""))
               :test #'string= :from-end t))
       (source-ids
         (gethash "source_motive_ids" (gethash "payload" prior-result)))
       (refine-result-id
         (log-event
          "recursive-curiosity-result"
          (obj "schema_version" 1 "thread_id" "thread:refine-fixture"
               "motive_id" (aref source-ids 0)
               "model_call_id" "model:refine-fixture"
               "source_motive_ids" source-ids
               "runtime_revision" "recursive-fixture-v1"
               "status" "completed" "audience" "private"
               "content" "The answer exposes a sharper unresolved edge."
               "completed_at" (get-universal-time))))
       (arguments
         (shasht:write-json
          (obj "question" "Which sharper edge does the result expose?"
               "source_motive_ids" source-ids) nil)))
  (declare (ignore refine-result-id))
  (setf *crm-provider-script*
        (list (list :tool "refine-curiosity" arguments)))
  (let* ((refined (conscious-recursive-curiosity-result-review-one))
         (new-motive-id (gethash "new_motive_id" refined))
         (sustain-result-id
           (log-event
            "recursive-curiosity-result"
            (obj "schema_version" 1 "thread_id" "thread:sustain-fixture"
                 "motive_id" new-motive-id
                 "model_call_id" "model:sustain-fixture"
                 "source_motive_ids" (vector new-motive-id)
                 "runtime_revision" "recursive-fixture-v1"
                 "status" "completed" "audience" "private"
                 "content" "The sharper edge remains generative."
                 "completed_at" (get-universal-time))))
         (sustain-arguments
           (shasht:write-json
            (obj "source_motive_ids" (vector new-motive-id)) nil)))
    (declare (ignore sustain-result-id))
    (crm-check "result review can refine into one sharper durable question"
               (and (string= "refined" (gethash "disposition" refined))
                    (stringp new-motive-id)))
    (setf *crm-provider-script*
          (list (list :tool "sustain-curiosity" sustain-arguments)))
    (let ((sustained (conscious-recursive-curiosity-result-review-one)))
      (crm-check "result review can sustain a generative interest"
                 (and (string= "sustained"
                               (gethash "disposition" sustained))
                      (eq :null (gethash "new_motive_id" sustained)))))))
(let ((calls-before *crm-provider-calls*))
  (setf *conscious-recursive-mind-sequence* 0)
  (conscious-recursive-mind-configure
   :agent-id "recursive-fixture"
   :endpoint "http://127.0.0.1:1234/v1/chat/completions"
   :model "fixture" :context-profile (obj)
   :tools-enabled-p t :curiosity-enabled-p t
   :tool-executor (lambda (&rest ignored)
                    (declare (ignore ignored)) "unused"))
  (crm-check "restart does not repeat a completed curiosity"
             (and (string= "idle"
                           (gethash "status"
                                    (conscious-recursive-curiosity-wake-one)))
                  (= calls-before *crm-provider-calls*))))

;; A reviewed finding enters one additional recursive leaf. The model chooses
;; semantic retention or decline; the runtime authors evidence, memory identity,
;; routing and the opt-in autonomous authorization.
(setf *crm-events* nil
      *crm-provider-calls* 0
      *crm-provider-script* nil
      *crm-memory-writes* nil
      *crm-notifications* nil)
(crm-check "selective curiosity reach-out defaults off"
           (not *conscious-recursive-mind-curiosity-reach-out-enabled-p*))
(let* ((result-id
         (log-event
          "recursive-curiosity-result"
          (obj "schema_version" 1 "thread_id" "thread:incorporation"
               "motive_id" "motive:incorporation"
               "model_call_id" "model:incorporation"
               "source_motive_ids" (vector "motive:incorporation")
               "runtime_revision" "fixture" "status" "completed"
               "audience" "private"
               "content" "The investigation found a durable connection."
               "completed_at" (get-universal-time))))
       (review-id
         (log-event
          "recursive-curiosity-result-review-completed"
          (obj "schema_version" 1 "result_event_id" result-id
               "disposition" "sustained"
               "source_motive_ids" (vector "motive:incorporation")
               "new_motive_id" :null "runtime_revision" "fixture"
               "completed_at" (get-universal-time))
          :caused-by result-id)))
  (declare (ignore review-id))
  (setf *crm-provider-script*
        (list
         (list :tool "retain-finding"
               (shasht:write-json
                (obj "summary" "A durable connection was found."
                     "limitations" "Working hypothesis only; no causal experiment was performed."
                     "memory_claim"
                     "pAI concluded that the investigated ideas have a durable connection."
                     "share_message"
                     "I found a connection that seems worth sharing with you.")
                nil))))
  (conscious-recursive-mind-configure
   :agent-id "recursive-fixture"
   :endpoint "http://127.0.0.1:1234/v1/chat/completions"
   :model "fixture" :context-profile (obj)
   :curiosity-enabled-p t :curiosity-reach-out-enabled-p t
   :finding-memory-fn
   (lambda (&rest arguments)
     (push arguments *crm-memory-writes*)
     (getf arguments :id))
   :observer-fn
   (lambda (status item result)
     (push (list status item result) *crm-notifications*)))
  (let* ((first (conscious-recursive-curiosity-incorporation-one))
         (again (conscious-recursive-curiosity-incorporation-one))
         (inspection (conscious-recursive-curiosity-inspect))
         (sources
           (loop for event in *crm-events*
                 for payload = (gethash "payload" event)
                 for metadata = (and (hash-table-p payload)
                                     (gethash "metadata" payload))
                 when (hash-table-p metadata)
                   collect (gethash "source" metadata)))
         (memory-arguments (first *crm-memory-writes*))
         (reach-out
           (find "recursive-curiosity-reach-out-v1" *crm-events*
                 :key (lambda (event)
                        (let* ((payload (gethash "payload" event))
                               (metadata (and (hash-table-p payload)
                                              (gethash "metadata" payload))))
                          (and (hash-table-p metadata)
                               (gethash "source" metadata))))
                 :test #'string=))
         (reach-out-metadata
           (and reach-out
                (gethash "metadata" (gethash "payload" reach-out)))))
    (crm-check "reviewed finding is retained through one memory admission"
               (and (string= "retained" (gethash "disposition" first))
                    (= 1 (length *crm-memory-writes*))))
    (crm-check "retained finding has private agent evidence and one reach-out"
               (and (find "recursive-curiosity-incorporation-v1" sources
                          :test #'string=)
                    (= 1 (count "recursive-curiosity-reach-out-v1" sources
                                :test #'string=))))
    (crm-check "finding admission carries runtime-owned grounding metadata"
               (and (string= (format nil "curiosity-finding-~a" result-id)
                             (getf memory-arguments :id))
                    (integerp (getf memory-arguments :source-event-id))
                    (string= "lived-agent-action"
                             (getf memory-arguments :origin-class))
                    (string= "agent-action"
                             (getf memory-arguments :epistemic-status))
                    (string= "grounded"
                             (getf memory-arguments :grounding-status))))
    (crm-check "qualification survives actual memory admission and delivery"
               (and (search "no causal experiment" (getf memory-arguments :content))
                    (search "no causal experiment" (gethash "text" (gethash "payload" reach-out)))
                    (equal result-id (gethash "source_result_event_id"
                                            (getf memory-arguments :epistemic-metadata)))))
    (crm-check "reach-out retains exact result and persona identity"
               (and (hash-table-p reach-out-metadata)
                    (equal result-id
                           (gethash "source_result_event_id"
                                    reach-out-metadata))
                    (string= "fixture"
                             (gethash "persona_id" reach-out-metadata))))
    (crm-check "reach-out follows the active public channel without a web dependency"
               (string= "terminal"
                        (gethash "channel" (gethash "payload" reach-out))))
    (crm-check "reach-out is separately observable for presentation"
               (find "autonomous-reach-out" *crm-notifications*
                     :key #'first :test #'string=))
    (crm-check "incorporation restart is idempotent"
               (and (string= "idle" (gethash "status" again))
                    (= 1 (length *crm-memory-writes*))))
    (crm-check "inspection exposes incorporation disposition"
               (and (= 1 (gethash "incorporation_count" inspection))
                    (string= "retained"
                             (gethash "disposition"
                                      (aref (gethash "incorporations" inspection)
                                            0)))))))

;; Operator intent is a durable companion to curiosity rather than prose the
;; later private roots must rediscover.  The same commitment is visible to the
;; mind, reaches incorporation, authorizes one result delivery, and settles
;; idempotently after the exact result is shared.
(setf *crm-events* nil
      *crm-provider-calls* 0
      *crm-provider-script* nil
      *crm-memory-writes* nil
      *crm-notifications* nil)
(conscious-recursive-mind-configure
 :agent-id "recursive-fixture"
 :endpoint "http://127.0.0.1:1234/v1/chat/completions"
 :model "fixture" :context-profile (obj)
 :curiosity-enabled-p t :curiosity-reach-out-enabled-p t
 :deliberate-curiosity-enabled-p t
 :finding-memory-fn
 (lambda (&rest arguments)
   (push arguments *crm-memory-writes*)
   (getf arguments :id))
 :observer-fn
 (lambda (status item result)
   (push (list status item result) *crm-notifications*)))
(let* ((operator-id
         (log-event "user-message"
                    (obj "text"
                         "Please investigate this and let me know what you find."
                         "channel" "terminal"
                         "metadata" (obj "source" "recursive-mind-v1"))))
       (motive-id
         (nth-value
          2
          (%recursive-record-curiosity
           "What concrete result answers the operator's question?"
           operator-id :supporting-event-ids (list operator-id)
           :evidence-identity-event-ids (list operator-id)
           :source-revision "intent-continuity-fixture")))
       (observation
         (find "conscious-curiosity-observed" *crm-events*
               :key (lambda (event) (gethash "type" event ""))
               :test #'string= :from-end t))
       (origin
         (find "recursive-curiosity-origin-context-recorded" *crm-events*
               :key (lambda (event) (gethash "type" event ""))
               :test #'string= :from-end t)))
  (%recursive-ensure-curiosity-origin-context
   observation operator-id (list operator-id))
  (crm-check "follow-up authority rejects absent operator evidence"
             (handler-case
                 (progn
                   (%recursive-request-curiosity-follow-up
                    motive-id "This request has no operator event." 999999)
                   nil)
               (error () t)))
  (%recursive-request-curiosity-follow-up
   motive-id "Tell the operator when a novel answer is retained." operator-id
   :supporting-event-ids (list operator-id))
  (let* ((attention (conscious-recursive-attention-inspect 8))
         (curiosity
           (find motive-id (gethash "curiosities" attention)
                 :key (lambda (row)
                        (or (gethash "motive_id" row)
                            (let ((ids (gethash "source_motive_ids" row)))
                              (and (vectorp ids) (plusp (length ids))
                                   (aref ids 0)))))
                 :test #'string=)))
    (crm-check "curiosity origin is durably bound to exact operator evidence"
               (and origin observation
                    (equal (gethash "id" observation)
                           (gethash "caused_by" origin))
                    (= 1
                       (count "recursive-curiosity-origin-context-recorded"
                              *crm-events*
                              :key (lambda (event)
                                     (gethash "type" event ""))
                              :test #'string=))
                    (equalp (vector operator-id)
                            (gethash "source_event_ids"
                                     (%recursive-event-payload origin)))))
    (crm-check "native attention inspection exposes requested follow-up"
               (and curiosity
                    (eq t
                        (gethash "requested"
                                 (gethash "follow_up" curiosity))))))
  (let* ((result-id
           (log-event
            "recursive-curiosity-result"
            (obj "schema_version" 1 "thread_id" "thread:requested-follow-up"
                 "motive_id" motive-id "model_call_id" "model:follow-up"
                 "source_motive_ids" (vector motive-id)
                 "runtime_revision" "fixture" "status" "completed"
                 "audience" "private"
                 "content" "The investigation produced a novel answer."
                 "completed_at" (get-universal-time))))
         (review-id
           (log-event
            "recursive-curiosity-result-review-completed"
            (obj "schema_version" 1 "result_event_id" result-id
                 "disposition" "sustained"
                 "source_motive_ids" (vector motive-id)
                 "new_motive_id" :null "runtime_revision" "fixture"
                 "completed_at" (get-universal-time))
            :caused-by result-id)))
    (declare (ignore review-id))
    (setf *crm-provider-script*
          (list
           (list :tool "retain-finding"
                 (shasht:write-json
                  (obj "summary" "A novel requested answer was found."
                       "memory_claim"
                       "pAI concluded that the requested investigation produced a novel answer."
                       "share_message" "")
                  nil))))
    (let* ((incorporated
             (conscious-recursive-curiosity-incorporation-one))
           (reach-out
             (find "recursive-curiosity-reach-out-v1" *crm-events*
                   :key (lambda (event)
                          (let* ((payload (%recursive-event-payload event))
                                 (metadata (and (hash-table-p payload)
                                                (gethash "metadata" payload))))
                            (and (hash-table-p metadata)
                                 (gethash "source" metadata))))
                   :test #'string= :from-end t))
           (metadata
             (and reach-out
                  (gethash "metadata" (%recursive-event-payload reach-out))))
           (completion
             (find "recursive-curiosity-follow-up-completed" *crm-events*
                   :key (lambda (event) (gethash "type" event ""))
                   :test #'string= :from-end t)))
      (crm-check "requested follow-up deterministically reaches incorporation"
                 (and (string= "retained"
                               (gethash "disposition" incorporated ""))
                      reach-out completion
                      (string= "curiosity-requested-follow-up"
                               (gethash "authorization_kind"
                                        (%recursive-event-payload reach-out)))
                      (integerp
                       (gethash "requested_follow_up_event_id" metadata))))
      (crm-check "requested follow-up completion is one-shot and inspectable"
                 (and
                  (equal result-id
                         (gethash "result_event_id"
                                  (%recursive-event-payload completion)))
                  (null
                   (nth-value
                    0
                    (%recursive-curiosity-follow-up-events
                     *crm-events* (list motive-id)))))))))

;; Retention can be declined without creating evidence, memory, or outreach.
(let* ((result-id
         (log-event
          "recursive-curiosity-result"
          (obj "schema_version" 1 "thread_id" "thread:decline"
               "motive_id" "motive:decline" "model_call_id" "model:decline"
               "source_motive_ids" (vector "motive:decline")
               "runtime_revision" "fixture" "status" "completed"
               "audience" "private" "content" "A trivial restatement."
               "completed_at" (get-universal-time))))
       (writes-before (length *crm-memory-writes*)))
  (log-event
   "recursive-curiosity-result-review-completed"
   (obj "schema_version" 1 "result_event_id" result-id
        "disposition" "closed" "source_motive_ids" (vector "motive:decline")
        "new_motive_id" :null "runtime_revision" "fixture"
        "completed_at" (get-universal-time))
   :caused-by result-id)
  (setf *crm-provider-script*
        (list (list :tool "decline-finding"
                    "{\"reason\":\"The result is only a trivial restatement.\"}")))
  (let ((declined (conscious-recursive-curiosity-incorporation-one)))
    (crm-check "trivial finding can be durably declined"
               (and (string= "declined" (gethash "disposition" declined))
                    (= writes-before (length *crm-memory-writes*))))))

;; Operator input preempts finding incorporation before provider, memory, or
;; autonomous-publication authority is crossed.
(let* ((result-id
         (log-event
          "recursive-curiosity-result"
          (obj "schema_version" 1 "thread_id" "thread:incorporation-preempt"
               "motive_id" "motive:incorporation-preempt"
               "model_call_id" "model:incorporation-preempt"
               "source_motive_ids" (vector "motive:incorporation-preempt")
               "runtime_revision" "fixture" "status" "completed"
               "audience" "private" "content" "A pending finding."
               "completed_at" (get-universal-time))))
       (calls-before *crm-provider-calls*)
       (writes-before (length *crm-memory-writes*)))
  (log-event
   "recursive-curiosity-result-review-completed"
   (obj "schema_version" 1 "result_event_id" result-id
        "disposition" "sustained"
        "source_motive_ids" (vector "motive:incorporation-preempt")
        "new_motive_id" :null "runtime_revision" "fixture"
        "completed_at" (get-universal-time))
   :caused-by result-id)
  (setf *conscious-recursive-mind-operator-pending-p* t)
  (let ((preempted (conscious-recursive-curiosity-incorporation-one)))
    (crm-check "operator input preempts finding incorporation"
               (and (string= "preempted" (gethash "status" preempted))
                    (= calls-before *crm-provider-calls*)
                    (= writes-before (length *crm-memory-writes*)))))
  (setf *conscious-recursive-mind-operator-pending-p* nil))

;; Operator input sets the pending bit before it waits on the root lock. A
;; background root observes that bit only between irreversible boundaries.
(let* ((observation-id
         (log-event
          "conscious-curiosity-observed"
          (obj "motive_id" "motive:preempt" "subject_label" "preemption")))
       (candidate-id
         (log-event
          "conscious-curiosity-candidate-raised"
          (obj "motive_id" "motive:preempt" "motive_kind" "curiosity"
               "expression_policy" "private-consideration-only"
               "source_event_ids" (vector observation-id)
               "latest_event_id" observation-id)
          :caused-by observation-id)))
  (declare (ignore candidate-id))
  (setf *conscious-recursive-mind-operator-pending-p* t)
  (let ((paused (conscious-recursive-curiosity-wake-one)))
    (crm-check "operator input preempts curiosity at a recursive boundary"
               (and (string= "preempted" (gethash "status" paused))
                    (null (find "recursive-curiosity-result" *crm-events*
                                :key (lambda (event) (gethash "type" event ""))
                                :test #'string=
                                :from-end t
                                :start (max 0 (- (length *crm-events*) 2)))))))
  (setf *conscious-recursive-mind-operator-pending-p* nil
        *crm-provider-script* (list "The private work resumed exactly once."))
  (crm-check "preempted curiosity resumes from durable authority"
             (string= "curiosity-completed"
                      (gethash "status"
                               (conscious-recursive-curiosity-wake-one)))))

;; Deliberate preservation is a legitimate separate pathway, but a quiet
;; review of that same conversational root must not count the agent half as a
;; second lived experience.
(setf *crm-events* nil
      *crm-provider-calls* 0
      *crm-provider-messages* nil
      *crm-provider-tool-counts* nil
      *crm-provider-script*
      (list
       (list :tool "record-curiosity"
             "{\"question\":\"Should this explicit question persist?\"}")
       "I deliberately preserved that question."))
(conscious-recursive-mind-configure
 :agent-id "recursive-fixture"
 :endpoint "http://127.0.0.1:1234/v1/chat/completions"
 :model "fixture" :context-profile (obj)
 :tools-enabled-p t :curiosity-enabled-p t
 :deliberate-curiosity-enabled-p t
 :review-ready-fn (lambda () t)
 :tool-executor (lambda (&rest ignored)
                  (declare (ignore ignored)) "unused"))
(conscious-recursive-mind-submit "That gives me something to think about.")
(let* ((reply
         (find "agent-message" *crm-events*
               :key (lambda (event) (gethash "type" event ""))
               :test #'string= :from-end t))
       (observation
         (find "conscious-curiosity-observed" *crm-events*
               :key (lambda (event) (gethash "type" event ""))
               :test #'string= :from-end t))
       (motive-id (gethash "motive_id" (gethash "payload" observation)))
       (arguments
         (shasht:write-json
          (obj "motive_id" motive-id
               "evidence_event_ids" (vector (gethash "id" reply)))
          nil)))
  (setf *crm-provider-script*
        (list (list :tool "reinforce-curiosity" arguments)))
  (let ((review (conscious-recursive-curiosity-review-one)))
    (crm-check "one conversation root cannot reinforce itself through review"
               (and (string= "review-completed" (gethash "status" review))
                    (= 0 (gethash "observation_count" review))
                    (= 1
                       (count-if
                        (lambda (event)
                          (let ((payload (gethash "payload" event)))
                            (and (string= "conscious-curiosity-observed"
                                          (gethash "type" event ""))
                                 (hash-table-p payload)
                                 (equal motive-id
                                        (gethash "motive_id" payload)))))
                        *crm-events*))))))

;; An exact consecutive native call is answered without executing its effect a
;; second time, then one tool-free model boundary synthesizes the reply.
(setf *crm-events* nil
      *crm-provider-calls* 0
      *crm-provider-messages* nil
      *crm-provider-tool-counts* nil
      *crm-provider-tool-names* nil
      *crm-tool-executions* nil
      *crm-activities* nil
      *crm-provider-script*
      (list (list :tool "bash" "{\"command\":\"echo same\"}")
            (list :tool "bash" "{\"command\":\"echo same\"}")
            "The command succeeded; I used the existing result."))
(conscious-recursive-mind-configure
 :agent-id "recursive-fixture"
 :endpoint "http://127.0.0.1:1234/v1/chat/completions"
 :model "fixture" :context-profile (obj)
 :tools-enabled-p t
 :tool-executor
 (lambda (name arguments context)
   (push (list name arguments context) *crm-tool-executions*)
   "same")
 :observer-fn
 (lambda (status item result)
   (declare (ignore item))
   (when (string= status "activity") (push result *crm-activities*))))
(let ((result (conscious-recursive-mind-submit "run it once")))
  (crm-check "duplicate tool call converges to a public reply"
             (and (string= "replied" (gethash "status" result))
                  (= 3 *crm-provider-calls*)))
  (crm-check "duplicate arbitrary effect executes exactly once"
             (= 1 (length *crm-tool-executions*)))
  (crm-check "duplicate suppression is durable and visible"
             (and (= 1 (count "recursive-tool-execution" *crm-events*
                              :key (lambda (event) (gethash "type" event ""))
                              :test #'string=))
                  (= 2 (count "recursive-tool-result" *crm-events*
                              :key (lambda (event) (gethash "type" event ""))
                              :test #'string=))
                  (find "tool-suppressed" *crm-activities*
                        :key (lambda (detail) (gethash "kind" detail ""))
                        :test #'string=)))
  (crm-check "duplicate suppression reserves a tool-free synthesis call"
             (and
              (equal '(9 9 0) (reverse *crm-provider-tool-counts*))
              (equal (list *crm-ordinary-native-tool-names*
                           *crm-ordinary-native-tool-names*
                           nil)
                     (reverse *crm-provider-tool-names*)))))

;; Personal recall remains bounded by the ordinary tool/result budgets. The
;; eighth and later retrievals advise the model to reconsider without refusing
;; a distinct evidence-seeking query.
(setf *crm-events* nil
      *crm-provider-calls* 0
      *crm-provider-tool-counts* nil
      *crm-provider-tool-names* nil
      *crm-tool-executions* nil
      *crm-provider-script*
      (list (list :tool "search-memory" "{\"query\":\"campfire\"}")
            (list :tool "search-memory" "{\"query\":\"smoke fire\"}")
            (list :tool "search-memory" "{\"query\":\"campground\"}")
            (list :tool "search-memory" "{\"query\":\"RV camping\"}")
            (list :tool "search-memory" "{\"query\":\"tent camping\"}")
            (list :tool "search-memory" "{\"query\":\"fire pit\"}")
            (list :tool "search-memory" "{\"query\":\"camping trip\"}")
            (list :tool "search-memory" "{\"query\":\"camping memory\"}")
            (list :tool "search-memory" "{\"query\":\"outdoor fire\"}")
            "I could not establish exhaustive absence from bounded recall."))
(conscious-recursive-mind-configure
 :agent-id "recursive-fixture"
 :endpoint "http://127.0.0.1:1234/v1/chat/completions"
 :model "fixture" :context-profile (obj)
 :tools-enabled-p t
 :tool-executor
 (lambda (name arguments context)
   (push (list name arguments context) *crm-tool-executions*)
   "bounded recall evidence"))
(let* ((result (conscious-recursive-mind-submit "find the campfire exchange"))
       (tool-results
         (loop for event in *crm-events*
               for payload = (gethash "payload" event)
               when (and (string= "recursive-tool-result"
                                  (gethash "type" event ""))
                         (hash-table-p payload))
                 collect payload)))
  (crm-check "several distinct personal recalls still converge to a reply"
             (and (string= "replied" (gethash "status" result))
                  (= 10 *crm-provider-calls*)
                  (= 9 (length *crm-tool-executions*))))
  (crm-check "personal recalls are advised rather than refused"
             (and (zerop (count "refused" tool-results
                                :key (lambda (payload)
                                       (gethash "execution_status" payload ""))
                                :test #'string=))
                  (= 2 (count-if
                        (lambda (payload)
                          (search "retrieval advisory"
                                  (gethash "content" payload "")))
                        tool-results)))))

;; Cumulative result pressure closes tools without relying on the model to
;; volunteer a stopping decision.
(setf *crm-events* nil
      *crm-provider-calls* 0
      *crm-provider-tool-counts* nil
      *crm-provider-tool-names* nil
      *crm-tool-executions* nil
      *crm-provider-script*
      (list (list :tool "bash" "{\"command\":\"printf evidence\"}")
            "I have enough evidence to answer."))
(conscious-recursive-mind-configure
 :agent-id "recursive-fixture"
 :endpoint "http://127.0.0.1:1234/v1/chat/completions"
 :model "fixture" :context-profile (obj)
 :tools-enabled-p t
 :tool-executor
 (lambda (name arguments context)
   (push (list name arguments context) *crm-tool-executions*)
   "evidence"))
(let* ((*conscious-recursive-mind-max-total-tool-result-characters* 1)
       (result (conscious-recursive-mind-submit "gather bounded evidence")))
  (crm-check "cumulative tool evidence converges to a reply"
             (string= "replied" (gethash "status" result)))
  (crm-check "cumulative tool evidence closes schemas for synthesis"
             (and (equal '(9 0) (reverse *crm-provider-tool-counts*))
                  (equal (list *crm-ordinary-native-tool-names* nil)
                         (reverse *crm-provider-tool-names*)))))

;; Recursive safety limits are common runaway guards, not a smaller private
;; turn allowance.
(crm-check "shared recursive model boundary guard permits thirty steps"
           (= 30 *conscious-recursive-mind-max-model-boundaries*))
(crm-check "shared recursive tool boundary guard permits thirty executions"
           (= 30 *conscious-recursive-mind-max-tool-boundaries*))

;; OpenRouter admission prices the actual next boundary only. It does not
;; withhold a second request for hypothetical final synthesis, and request
;; count remains telemetry only.
(setf *crm-events* nil
      *crm-provider-calls* 0
      *crm-provider-tool-counts* nil
      *crm-provider-tool-names* nil
      *crm-provider-script* (list "One bounded call is enough."))
(conscious-recursive-mind-configure
 :agent-id "recursive-fixture"
 :endpoint "http://127.0.0.1:1234/v1/chat/completions"
 :model "fixture" :context-profile (obj) :tools-enabled-p t
 :tool-executor
 (lambda (&rest ignored)
   (declare (ignore ignored))
   "unused"))
(let ((*crm-openrouter-p* t)
      (*conscious-recursive-mind-endpoint*
        "https://openrouter.ai/api/v1/chat/completions")
      (*conscious-conversation-provider-attempts* 19)
      (*conscious-conversation-provider-spent-usd* 0.024d0)
      (*conscious-conversation-cost-ceiling-usd* 0.03d0))
  (let ((result
          (conscious-recursive-mind-submit
           "use the one actual boundary that still fits")))
    (crm-check "near-ceiling recursive boundary still advertises native tools"
               (and (string= "replied" (gethash "status" result))
                    (equal '(9) (reverse *crm-provider-tool-counts*))
                    (equal (list *crm-ordinary-native-tool-names*)
                           (reverse *crm-provider-tool-names*))))))
(let ((*crm-openrouter-p* t)
      (*conscious-recursive-mind-tools-enabled-p* t)
      (*conscious-recursive-mind-endpoint*
        "https://openrouter.ai/api/v1/chat/completions")
      (*conscious-conversation-provider-attempts* 19)
      (*conscious-conversation-provider-spent-usd* 0.024d0)
      (*conscious-conversation-cost-ceiling-usd* 0.03d0))
  (let ((messages (list (obj "role" "user" "content" "fixture"))))
    (crm-check "one fitting tool-capable request is not withheld for a later final"
               (%recursive-selected-call-admissible-p
                messages (%recursive-tool-schemas t nil t)))
    (let ((*conscious-conversation-provider-spent-usd* 0.026d0))
      (crm-check "an actual request that exceeds the shared ceiling is rejected"
                 (not (%recursive-selected-call-admissible-p
                       messages (%recursive-tool-schemas t nil t)))))))

;; Every private root shares one percentage of the total provider cost
;; authority. There is no additional per-turn reserve or request-count quota.
(let ((*crm-openrouter-p* t)
      (*conscious-recursive-mind-endpoint*
        "https://openrouter.ai/api/v1/chat/completions")
      (*conscious-recursive-mind-private-budget-percent* 50)
      (*conscious-conversation-provider-attempts* 6)
      (*conscious-conversation-provider-spent-usd* 0.044d0)
      (*conscious-conversation-cost-ceiling-usd* 0.10d0)
      (*conscious-conversation-private-provider-attempts* 6)
      (*conscious-conversation-private-provider-spent-usd* 0.044d0))
  (let ((messages (list (obj "role" "user" "content" "fixture"))))
    (crm-check "private boundary may consume the last fitting slice of its cumulative share"
               (%recursive-selected-call-admissible-p
                messages (%recursive-tool-schemas t nil t) t))
    (let ((*conscious-conversation-provider-spent-usd* 0.046d0)
          (*conscious-conversation-private-provider-spent-usd* 0.046d0))
      (crm-check "private call pauses only when that actual call exceeds its share"
                 (not (%recursive-selected-call-admissible-p
                       messages (%recursive-tool-schemas t nil t) t))))
    (crm-check "private cost exhaustion leaves foreground authority usable"
               (%recursive-selected-call-admissible-p
                messages (%recursive-tool-schemas t nil t) nil))))
;; A private pre-request failure returns a bounded terminal diagnostic to its
;; observer and settles that exact focus. Replaying an already-failed context
;; boundary cannot repair the bad evidence/configuration, and must not wedge
;; every later attention cycle behind the same oldest focus.
(let* ((observation-id
         (log-event
          "conscious-curiosity-observed"
          (obj "motive_id" "motive:failure-settlement"
               "subject_label" "failure settlement")))
       (candidate-id
         (log-event
          "conscious-curiosity-candidate-raised"
          (obj "motive_id" "motive:failure-settlement"
               "motive_kind" "curiosity"
               "expression_policy" "private-consideration-only"
               "source_event_ids" (vector observation-id)
               "latest_event_id" observation-id)
          :caused-by observation-id))
       (original (symbol-function 'make-conscious-assembly-context))
       (original-observer *conscious-recursive-mind-observer*)
       (observed nil))
  (declare (ignore candidate-id))
  (setf *conscious-recursive-mind-observer*
        (lambda (status item result)
          (declare (ignore item result))
          (push status observed)))
  (unwind-protect
       (progn
         (setf (symbol-function 'make-conscious-assembly-context)
               (lambda (&rest ignored)
                 (declare (ignore ignored))
                 (error "fixture private assembly failure")))
         (let* ((*conscious-recursive-mind-curiosity-enabled-p* t)
                 (result (conscious-recursive-curiosity-wake-one)))
            (crm-check "private investigation errors settle their observer lifecycle"
                       (and (string= "failed" (gethash "status" result))
                            (string= "recursive-context-open-failed"
                                     (gethash "error_code" result))
                            (find "failed" observed :test #'string=)))
            (crm-check "pre-request failure durably releases later attention"
                       (and (= 1
                               (count "recursive-root-failed" *crm-events*
                                      :key (lambda (event)
                                             (gethash "type" event ""))
                                      :test #'string=))
                            (= 1
                               (count "recursive-curiosity-focus-failed"
                                      *crm-events*
                                      :key (lambda (event)
                                             (gethash "type" event ""))
                                      :test #'string=))))))
     (setf (symbol-function 'make-conscious-assembly-context) original
           *conscious-recursive-mind-observer* original-observer)))

;; A durably journaled provider failure cannot be retried by replaying the
;; exact same request boundary.  It settles only that focus attempt, survives
;; restart, and releases the attention register for later decisions.
(setf *crm-events* nil
      *crm-provider-calls* 0
      *crm-provider-script* nil
      *conscious-recursive-mind-curiosity-enabled-p* t
      *conscious-recursive-mind-operator-pending-p* nil)
(let* ((source-id
         (log-event "user-message"
                    (obj "text" "A provider failure should not stop curiosity."
                         "channel" "terminal"
                         "metadata" (obj "source" "recursive-mind-v1"))))
       (motive-id
         (nth-value
          2
          (%recursive-record-curiosity
           "Can curiosity continue after one provider failure?" source-id
           :supporting-event-ids (list source-id)
           :evidence-identity-event-ids (list source-id)
           :source-revision "durable-failure-fixture")))
       (observation
         (find "conscious-curiosity-observed" *crm-events*
               :key (lambda (event) (gethash "type" event ""))
               :test #'string= :from-end t))
       (focus
         (nth-value
          0
          (%recursive-ensure-curiosity-focus
           700 "Can curiosity continue after one provider failure?"
           motive-id nil (list (gethash "id" observation))))))
  (setf *crm-provider-script* (list :invalid))
  (let ((failed (conscious-recursive-curiosity-wake-one)))
    (crm-check "durable provider failure settles the exact focus"
               (and (string= "failed" (gethash "status" failed))
                    (integerp (gethash "focus_failure_event_id" failed))
                    (= 1
                       (count "recursive-curiosity-focus-failed" *crm-events*
                              :key (lambda (event)
                                     (gethash "type" event ""))
                              :test #'string=)))))
  (let ((calls-before *crm-provider-calls*)
        (after-failure (conscious-recursive-curiosity-wake-one)))
    (crm-check "settled failed focus is not replayed as a provider retry"
               (and (string= "idle" (gethash "status" after-failure))
                    (= calls-before *crm-provider-calls*)
                    (= 1
                       (count "recursive-curiosity-focus-failed" *crm-events*
                              :key (lambda (event)
                                     (gethash "type" event ""))
                              :test #'string=)))))
  ;; Reconfiguration models a fresh process over the same immutable events.
  (conscious-recursive-mind-configure
   :agent-id "recursive-fixture"
   :endpoint "http://127.0.0.1:1234/v1/chat/completions"
   :model "fixture" :context-profile (obj)
   :tools-enabled-p t :curiosity-enabled-p t
   :review-ready-fn (lambda () t)
   :tool-executor (lambda (&rest ignored)
                    (declare (ignore ignored)) "fixture"))
  (let ((calls-before *crm-provider-calls*)
        (restored (conscious-recursive-curiosity-wake-one)))
    (crm-check "restart preserves failed-focus settlement without resampling"
               (and (string= "idle" (gethash "status" restored))
                    (= calls-before *crm-provider-calls*))))
  (let* ((inspection (conscious-recursive-curiosity-inspect))
         (row (find (gethash "id" focus)
                    (coerce (gethash "focuses" inspection) 'list)
                    :key (lambda (item) (gethash "event_id" item))
                    :test #'equal)))
    (crm-check "inspection exposes failed focus diagnostics"
               (and row
                    (string= "failed" (gethash "status" row))
                    (= 1 (gethash "failed_focus_count" inspection))
                    (= 0 (gethash "pending_focus_count" inspection))
                    (= 1 (gethash "open_motive_count" inspection))
                    (= 1 (gethash "attention_register_count" inspection))
                    (= 0 (gethash "attention_register_omitted_count"
                                  inspection))
                    (= 1 (gethash "model_request_count" row))
                    (integerp (gethash "failure_event_id" row))
                    (integerp
                     (gethash "failed_model_response_event_id" row))
                    (string= "fixture-failure" (gethash "error_code" row))
                    (plusp (length (gethash "failure_reason" row))))))
  (setf *crm-provider-script* (list "No focus warrants attention now."))
  (let ((attention (conscious-recursive-curiosity-attention-one)))
    (crm-check "failed focus releases the open register for later attention"
               (string= "attention-declined"
                        (gethash "status" attention)))))

;; Authenticated operator top-ups extend the existing process budget without
;; erasing attempts, spend, or an uncertain-accounting seal.
(let ((*conscious-conversation-provider-attempts* 20)
      (*conscious-conversation-provider-spent-usd* 0.125d0)
      (*conscious-conversation-private-provider-attempts* 6)
      (*conscious-conversation-private-provider-spent-usd* 0.025d0)
      (*conscious-recursive-mind-private-budget-percent* 30)
      (*conscious-conversation-cost-ceiling-usd* 0.25d0)
      (*conscious-conversation-provider-budget-uncertain-p* nil))
  (let ((report (conscious-recursive-session-budget-add 0.50d0)))
    (crm-check "operator budget top-up preserves accumulated usage"
               (and (= 20 (gethash "request_attempts" report))
                    (not (gethash "request_limit_enforced" report))
                    (eq :null (gethash "request_limit" report))
                    (eq :null (gethash "remaining_requests" report))
                    (= 0.125d0 (gethash "spent_usd" report))
                    (= 0.75d0 (gethash "cost_ceiling_usd" report))
                    (= 0.625d0 (gethash "remaining_usd" report))
                    (not (gethash "private_request_limit_enforced" report))
                    (eq :null (gethash "private_request_limit" report))
                    (eq :null (gethash "private_remaining_requests" report))
                    (= 6 (gethash "private_request_attempts" report))
                    (< (abs (- 0.225d0
                               (gethash "private_cost_ceiling_usd" report)))
                       1d-12)
                    (< (abs (- 0.2d0
                               (gethash "private_remaining_usd" report)))
                       1d-12)
                    (= 0d0
                       (gethash "pending_generation_fallback_usd" report))))))
(crm-check "operator cost top-up rejects a non-positive amount"
           (handler-case
               (progn (conscious-recursive-session-budget-add 0d0) nil)
             (error () t)))

;; Private cognition is a bounded ordinary-context projection, not a phrase
;; detector. Pending focus precedes open interest and reviewed findings, and
;; every row retains exact durable evidence identity.
(let ((*crm-events* nil)
      (*conscious-recursive-mind-agent-id* "recursive-fixture")
      (*conscious-recursive-mind-curiosity-enabled-p* t))
  (let* ((source-id
           (log-event "user-message"
                      (obj "text" "A question arose" "channel" "terminal"
                           "metadata" (obj "source" "recursive-mind-v1"))))
         (observation-id nil)
         (motive-id nil))
    (multiple-value-bind (ignored appended-p recorded-motive-id)
        (%recursive-record-curiosity
         "What makes this question generative?" source-id
         :supporting-event-ids (list source-id)
         :evidence-identity-event-ids (list source-id)
         :source-revision "fixture")
      (declare (ignore ignored))
      (setf motive-id recorded-motive-id)
      (crm-check "private-context fixture records one open interest" appended-p))
    (setf observation-id
          (gethash "id"
                   (find "conscious-curiosity-observed" *crm-events*
                         :key (lambda (event) (gethash "type" event ""))
                         :test #'string= :from-end t)))
    (multiple-value-bind (focus appended-p)
        (%recursive-ensure-curiosity-focus
         900 "What makes this question generative?" motive-id nil
         (list observation-id))
      (crm-check "private-context fixture opens one current focus" appended-p)
      (let* ((focus-id (gethash "id" focus))
             (result-id
               (log-event
                "recursive-curiosity-result"
                (obj "schema_version" 1 "thread_id" "thread:private"
                     "motive_id" motive-id "model_call_id" "model:private"
                     "source_motive_ids" (vector motive-id)
                     "runtime_revision" "fixture" "status" "completed"
                     "audience" "private"
                     "content" "The finding connects surprise with sustained inquiry."
                     "completed_at" (get-universal-time))
                :caused-by focus-id)))
        (log-event
         "recursive-curiosity-result-review-completed"
         (obj "schema_version" 1 "result_event_id" result-id
              "disposition" "sustained"
              "source_motive_ids" (vector motive-id)
              "new_motive_id" :null "runtime_revision" "fixture"
              "completed_at" (get-universal-time))
         :caused-by 901)
        (log-event
         "recursive-curiosity-incorporation-completed"
         (obj "schema_version" 1 "result_event_id" result-id
              "disposition" "retained"
              "summary" "Surprise can sustain inquiry"
              "decline_reason" :null
              "memory_node_id" "curiosity-finding-fixture"
              "evidence_event_id" 901 "reach_out_event_id" :null
              "runtime_revision" "fixture"
              "completed_at" (get-universal-time))
         :caused-by 901)
        ;; Add a newer pending focus so all three projection classes coexist.
        (%recursive-ensure-curiosity-focus
         902 "What should receive attention next?" motive-id nil
         (list observation-id))
        (let* ((records
                 (conscious-recursive-private-cognition-context-records))
               (rendered (shasht:write-json records nil)))
          (crm-check "ordinary context sees current private focus"
                     (and (search "Private cognition status (runtime-derived)"
                                  rendered)
                          (search "active focus" rendered)))
          (crm-check "ordinary context marks private interest as non-directive"
                     (search "Background private interest, not a current operator task"
                             rendered))
          (crm-check "ordinary context sees reviewed private finding"
                     (search "Retained private assessment" rendered))
          (let* ((frontier
                   (%recursive-curiosity-knowledge-frontier
                    *crm-events* (vector motive-id)))
                 (conclusions (gethash "current_conclusions" frontier)))
            (crm-check "retained finding enters the motive knowledge frontier"
                       (and (= 1 (length conclusions))
                            (= result-id
                               (gethash "result_event_id"
                                        (aref conclusions 0))))))
          (log-event
           "recursive-curiosity-finding-superseded"
           (obj "schema_version" 1 "result_event_id" result-id
                "reason" "Fixture correction: conclusion was not supported."
                "replacement_result_event_id" :null
                "runtime_revision" "fixture"
                "superseded_at" (get-universal-time))
           :caused-by result-id)
          (let* ((frontier
                   (%recursive-curiosity-knowledge-frontier
                    *crm-events* (vector motive-id)))
                 (current (gethash "current_conclusions" frontier))
                 (corrections
                   (gethash "superseded_conclusions" frontier)))
            (crm-check "supersession removes false knowledge but preserves correction"
                       (and (zerop (length current))
                            (= 1 (length corrections)))))
          (crm-check "private cognition rows retain exact event evidence"
                     (every (lambda (row)
                              (integerp (gethash "source_id" row)))
                            (coerce records 'list))))))))

;; A private-state briefing is one durable, grounded compression of the exact
;; raw context revision.  It is cached while current, invalidated by source
;; change, and a malformed model boundary settles once without blocking later
;; private cognition.
(let ((*crm-events* nil)
      (*crm-provider-calls* 0)
      (*crm-provider-script* nil)
      (*crm-provider-messages* nil)
      (*crm-provider-tool-counts* nil)
      (*crm-provider-tool-choices* nil)
      (*crm-provider-output-caps* nil)
      (*crm-provider-reasoning-enabled-values* nil)
      (*conscious-conversation-provider-profile*
        (obj "reasoning" (obj "enabled" t)))
      (*conscious-recursive-mind-agent-id* "recursive-fixture")
      (*conscious-recursive-mind-curiosity-enabled-p* t)
      (*conscious-recursive-mind-curiosity-briefing-enabled-p* t)
      (*conscious-recursive-mind-operator-waiters* 0)
      (*conscious-conversation-provider-attempts* 0)
      (*conscious-conversation-provider-spent-usd* 0d0)
      (*conscious-conversation-private-provider-attempts* 0)
      (*conscious-conversation-private-provider-spent-usd* 0d0)
      (*conscious-conversation-cost-ceiling-usd* 1d0)
      (*conscious-recursive-mind-private-budget-percent* 100))
  (flet ((record-one (text question)
           (let ((source-id
                   (log-event "user-message"
                              (obj "text" text "channel" "terminal"
                                   "metadata"
                                   (obj "source" "recursive-mind-v1")))))
             (%recursive-record-curiosity
              question source-id :supporting-event-ids (list source-id)
              :evidence-identity-event-ids (list source-id)
              :source-revision "briefing-fixture"))))
    (record-one "A star prompted a question"
                "Why did one mention of a star become compelling?")
    (let* ((records (%recursive-private-cognition-raw-context-records
                     (%recursive-thread-events)))
           (cited-id (gethash "source_id" (aref records 0))))
      (let ((direct
              (%recursive-private-briefing-action
               (obj "role" "assistant"
                    "content"
                    "Selective fascination remains active and may connect to sustained inquiry."
                    "tool_calls" :null)
               records)))
        (crm-check "briefing accepts bounded direct provider content"
                   (and (search "Selective fascination"
                                (gethash "content" direct))
                        (equalp
                         (gethash "source_event_ids" direct)
                         (map 'vector
                              (lambda (row) (gethash "source_id" row))
                              records)))))
      (setf *crm-provider-script*
            (list
             (list :tool "write-private-briefing"
                   (shasht:write-json
                    (obj "content"
                         "A question about selective fascination is active; connect it to sustained inquiry.")
                    nil))))
      (let* ((first (conscious-recursive-curiosity-briefing-one))
             (events-after-first (length *crm-events*))
             (calls-after-first *crm-provider-calls*)
             (current (conscious-recursive-curiosity-briefing-one))
             (context
               (conscious-recursive-private-cognition-context-records))
             (inspection (conscious-recursive-curiosity-inspect)))
        (crm-check "briefing fixture creates one durable grounded briefing"
                   (and (string= "briefing-updated"
                                 (gethash "status" first))
                        (search
                         *conscious-recursive-curiosity-briefing-protocol-revision*
                         (gethash "source_revision" first))
                        (= 1
                           (count "recursive-curiosity-briefing-completed"
                                  *crm-events*
                                  :key (lambda (event)
                                         (gethash "type" event ""))
                                  :test #'string=))))
        (crm-check "briefing is uncapped, strict, and reasoning-free"
                   (let* ((schema (%recursive-private-briefing-schema))
                          (function (gethash "function" (aref schema 0)))
                          (parameters (gethash "parameters" function))
                          (properties (gethash "properties" parameters))
                          (content-schema (gethash "content" properties))
                          (request (first *crm-provider-messages*))
                          (system-prompt (gethash "content" (first request))))
                     (and
                      (null (first *crm-provider-output-caps*))
                      (null (first *crm-provider-reasoning-enabled-values*))
                      (string= "required"
                               (first *crm-provider-tool-choices*))
                      (= 1 (length schema))
                      (eq t (gethash "strict" function))
                      (= 2000 (gethash "maxLength" content-schema))
                      (search "120 to 180 tokens" system-prompt)
                      (search "1600 Unicode characters" system-prompt)
                      (search "2000-character limit" system-prompt))))
        (crm-check "current briefing is provider-silent and write-free"
                   (and (string= "briefing-current"
                                 (gethash "status" current))
                        (= calls-after-first *crm-provider-calls*)
                        (= events-after-first (length *crm-events*))))
        (crm-check "ordinary context consumes status plus compact briefing"
                   (and (= 2 (length context))
                        (search "Private cognition status (runtime-derived)"
                                (gethash "content" (aref context 0)))
                        (search "Current private-state briefing"
                                (gethash "content" (aref context 1)))))
        (crm-check "curiosity inspection exposes the current briefing"
                   (and (string= "current"
                                 (gethash "briefing_status" inspection))
                        (hash-table-p (gethash "briefing" inspection))
                        (= cited-id
                           (aref (gethash "source_event_ids"
                                         (gethash "briefing" inspection))
                                 0)))))
      (record-one "A second question changed private attention"
                  "How should a new unresolved edge alter orientation?")
      (let ((fallback
              (conscious-recursive-private-cognition-context-records)))
        (crm-check "source change invalidates stale briefing immediately"
                   (and (> (length fallback) 1)
                        (not (search "Current private-state briefing"
                                     (gethash "content"
                                              (aref fallback 0)))))))
      ;; One overlong but structurally valid draft receives one bounded
      ;; compression call.  The rejected prose is not durable public state.
      (let ((calls-before-repair *crm-provider-calls*))
        (setf *crm-provider-script*
              (list
               (list :tool "write-private-briefing"
                     (shasht:write-json
                      (obj "content" (make-string 2001 :initial-element #\x))
                      nil))
               (list :tool "write-private-briefing"
                     (shasht:write-json
                      (obj "content"
                           "A compact repaired orientation preserves the active concern and unresolved edge.")
                      nil))))
        (let* ((repaired (conscious-recursive-curiosity-briefing-one))
               (responses
                 (remove-if-not
                  (lambda (event)
                    (let ((payload (%recursive-event-payload event)))
                      (and (string= "model-response" (gethash "type" event ""))
                           (gethash "private_briefing" payload))))
                  *crm-events*))
               (overlong
                 (find-if
                  (lambda (event)
                    (string= "briefing-overlong"
                             (gethash "error_code"
                                      (%recursive-event-payload event) "")))
                  responses))
               (repair
                 (find-if
                  (lambda (event)
                    (let ((payload (%recursive-event-payload event)))
                      (and (gethash "private_briefing_repair" payload)
                           (string= "accepted" (gethash "status" payload "")))))
                  responses))
               (repair-request (first *crm-provider-messages*))
               (repair-prompt
                 (gethash "content" (first repair-request))))
          (crm-check "overlong briefing receives one bounded compression repair"
                     (and (string= "briefing-updated"
                                   (gethash "status" repaired))
                          (= (+ calls-before-repair 2) *crm-provider-calls*)
                          overlong repair
                          (= 2001
                             (gethash "rejected_characters"
                                      (%recursive-event-payload overlong)))
                          (zerop
                           (count "recursive-curiosity-briefing-failed"
                                  *crm-events*
                                  :key (lambda (event)
                                         (gethash "type" event ""))
                                  :test #'string=))))
          (crm-check "repair prompt carries explicit target and hard ceiling"
                     (and (search "1600 Unicode characters" repair-prompt)
                          (search "2000-character limit" repair-prompt)
                          (search "without adding claims" repair-prompt)))))
      (record-one "A third question changed the briefing revision"
                  "Can compression itself preserve the most important edge?")
      ;; A second overlong repair settles once; it can never recurse into a
      ;; third provider call or block later private cognition.
      (setf *crm-provider-script*
            (list
             (list :tool "write-private-briefing"
                   (shasht:write-json
                    (obj "content" (make-string 2001 :initial-element #\y))
                    nil))
             (list :tool "write-private-briefing"
                   (shasht:write-json
                    (obj "content" (make-string 2001 :initial-element #\z))
                    nil))))
      (let* ((calls-before-failure *crm-provider-calls*)
             (failed (conscious-recursive-curiosity-briefing-one))
             (events-after-failure (length *crm-events*))
             (calls-after-failure *crm-provider-calls*)
             (settled (conscious-recursive-curiosity-briefing-one))
             (inspection (conscious-recursive-curiosity-inspect))
             (failed-response
               (find-if
                (lambda (event)
                  (let ((payload (%recursive-event-payload event)))
                    (and (string= "model-response"
                                  (gethash "type" event ""))
                         (string= "briefing-repair-overlong"
                                  (gethash "error_code" payload "")))))
                *crm-events* :from-end t)))
        (crm-check "failed briefing repair settles one revision after two calls"
                   (and (string= "failed" (gethash "status" failed))
                        (= (+ calls-before-failure 2) calls-after-failure)
                        (= 1
                           (count "recursive-curiosity-briefing-failed"
                                  *crm-events*
                                  :key (lambda (event)
                                         (gethash "type" event ""))
                                  :test #'string=))))
        (crm-check "overlong repair is diagnosed separately from provider transport"
                   (and failed-response
                        (search "maximum is 2000"
                                (gethash "reason"
                                         (%recursive-event-payload
                                          failed-response)))))
        (crm-check "failed briefing revision never retries on replay"
                   (and (string= "briefing-failure-settled"
                                 (gethash "status" settled))
                        (= calls-after-failure *crm-provider-calls*)
                        (= events-after-failure (length *crm-events*))))
        (crm-check "inspection makes current briefing failure visible"
                   (and (string= "failed"
                                 (gethash "briefing_status" inspection))
                        (hash-table-p (gethash "briefing" inspection))))))))

;; Consolidation changes only the bounded presentation of open motives.  It
;; must partition every sealed motive exactly once, feed attention and
;; briefing, cache by content revision, and fall back to raw rows on failure.
(let ((*crm-events* nil)
      (*crm-provider-calls* 0)
      (*crm-provider-script* nil)
      (*crm-provider-messages* nil)
      (*crm-provider-tool-counts* nil)
      (*crm-provider-tool-choices* nil)
      (*crm-provider-output-caps* nil)
      (*crm-provider-reasoning-enabled-values* nil)
      (*conscious-conversation-provider-profile*
        (obj "reasoning" (obj "enabled" t)))
      (*conscious-recursive-mind-agent-id* "recursive-fixture")
      (*conscious-recursive-mind-curiosity-enabled-p* t)
      (*conscious-recursive-mind-curiosity-consolidation-enabled-p* t)
      (*conscious-recursive-mind-curiosity-briefing-enabled-p* t)
      (*conscious-recursive-mind-operator-waiters* 0)
      (*conscious-conversation-provider-attempts* 0)
      (*conscious-conversation-provider-spent-usd* 0d0)
      (*conscious-conversation-private-provider-attempts* 0)
      (*conscious-conversation-private-provider-spent-usd* 0d0)
      (*conscious-conversation-cost-ceiling-usd* 1d0)
      (*conscious-recursive-mind-private-budget-percent* 100))
  (flet ((record-one (text question)
           (let ((source-id
                   (log-event "user-message"
                              (obj "text" text "channel" "terminal"
                                   "metadata"
                                   (obj "source" "recursive-mind-v1")))))
             (%recursive-record-curiosity
              question source-id :supporting-event-ids (list source-id)
              :evidence-identity-event-ids (list source-id)
              :source-revision "consolidation-fixture"))))
    (record-one "A star prompted immediate fascination"
                "Why can one mention of a star become compelling?")
    (record-one "The same theme became a question about inquiry"
                "How does selective fascination become sustained inquiry?")
    (record-one "A routine detail was also noticed"
                "Which ordinary implementation detail remains unresolved?")
    (let* ((open
             (nth-value 0
                        (%recursive-curiosity-open-register
                         (%recursive-thread-events)
                         most-positive-fixnum 0)))
           (first (aref open 0))
           (second (aref open 1))
           (third (aref open 2))
           (first-two-motives
             (vector (gethash "motive_id" first)
                     (gethash "motive_id" second)))
           (threads
             (vector
              (obj "question"
                   "Why does selective fascination grow into sustained inquiry?"
                   "source_motive_ids" first-two-motives
                   "attention_state" "foreground"
                   "rationale"
                   "The two questions describe one generative interest arc.")
              (obj "question" (gethash "question" third)
                   "source_motive_ids" (vector (gethash "motive_id" third))
                   "attention_state" "dormant"
                   "rationale"
                   "It remains valid but exerts little pull beside the first arc.")))
           (invalid-threads
             (vector
              (obj "question"
                   "Why does selective fascination grow into sustained inquiry?"
                   "source_motive_ids"
                   (vector (gethash "motive_id" first)
                           (gethash "motive_id" first))
                   ;; This is the exact lived defect: motive phase vocabulary
                   ;; copied into the distinct attention-state field.  The
                   ;; duplicate/missing pair covers the earlier lived defect.
                   "attention_state" "rising"
                   "rationale"
                   "The two questions describe one generative interest arc.")
              (obj "question" (gethash "question" third)
                   "source_motive_ids" (vector (gethash "motive_id" third))
                   "attention_state" "dormant"
                   "rationale"
                   "It remains valid but exerts little pull beside the first arc."))))
      (let* ((coverage-only
               (vector
                (obj "question" (gethash "question" first)
                     "source_motive_ids"
                     (vector (gethash "motive_id" first)
                             (gethash "motive_id" first))
                     "attention_state" "foreground"
                     "rationale" "The provider repeated one sealed identity.")
                (obj "question" (gethash "question" third)
                     "source_motive_ids" (vector (gethash "motive_id" third))
                     "attention_state" "dormant"
                     "rationale" "This is a valid separate thread.")))
             (normalized
               (%recursive-curiosity-consolidation-normalize-coverage
                coverage-only open)))
        (crm-check "consolidation normalizes duplicate and omitted coverage deterministically"
                   (and
                    (%recursive-curiosity-consolidation-threads-valid-p
                     normalized open)
                    (= 3 (length normalized))
                    (equalp
                     (vector (gethash "motive_id" second))
                     (gethash "source_motive_ids" (aref normalized 2))))))
      (setf *crm-provider-script*
            (list
             (list :tool "write-curiosity-consolidation"
                   (shasht:write-json
                    (obj "threads" invalid-threads) nil))
             (list :tool "write-curiosity-consolidation"
                   (shasht:write-json (obj "threads" threads) nil))))
      (let* ((first-pass
               (conscious-recursive-curiosity-consolidation-one))
             (events-after-first (length *crm-events*))
             (calls-after-first *crm-provider-calls*)
             (current
               (conscious-recursive-curiosity-consolidation-one))
             (presented
               (nth-value 0
                          (%recursive-curiosity-consolidated-register
                           (%recursive-thread-events))))
             (raw-context
               (%recursive-private-cognition-raw-context-records
                (%recursive-thread-events)))
             (inspection (conscious-recursive-curiosity-inspect)))
        (crm-check "consolidation is uncapped, strict, and reasoning-free"
                   (let ((choice (first *crm-provider-tool-choices*)))
                     (and
                      (every #'null *crm-provider-output-caps*)
                      (every #'null *crm-provider-reasoning-enabled-values*)
                      (string= "required" choice)
                      (= 1 (length
                            (%recursive-curiosity-consolidation-schema)))
                      (string= "write-curiosity-consolidation"
                               (gethash
                                "name"
                                (gethash
                                 "function"
                                 (aref
                                  (%recursive-curiosity-consolidation-schema)
                                  0))))
                      (eq t
                          (gethash
                           "strict"
                           (gethash
                            "function"
                            (aref
                             (%recursive-curiosity-consolidation-schema) 0)))))))
        (crm-check "invalid phase-shaped attention state receives one repair"
                   (let* ((responses
                            (remove-if-not
                             (lambda (event)
                               (let ((payload (%recursive-event-payload event)))
                                 (and (string= "model-response"
                                               (gethash "type" event ""))
                                      (gethash "private_consolidation"
                                               payload))))
                             *crm-events*))
                          (invalid
                            (find-if
                             (lambda (event)
                               (string= "consolidation-invalid"
                                        (gethash
                                         "error_code"
                                         (%recursive-event-payload event) "")))
                             responses))
                          (repair
                            (find-if
                             (lambda (event)
                               (let ((payload (%recursive-event-payload event)))
                                 (and
                                  (gethash "private_consolidation_repair"
                                           payload)
                                  (string= "accepted"
                                           (gethash "status" payload "")))))
                             responses))
                          (repair-prompt
                            (gethash "content"
                                     (first
                                      (first *crm-provider-messages*)))))
                     (and (= 2 calls-after-first)
                          invalid repair
                          (= 1
                             (length
                              (gethash
                               "invalid_attention_states"
                               (gethash
                                "validation_report"
                                (%recursive-event-payload invalid)))))
                          (= 1
                             (length
                              (gethash
                               "missing_motive_ids"
                               (gethash
                                "validation_report"
                                (%recursive-event-payload invalid)))))
                          (= 1
                             (length
                              (gethash
                               "duplicate_motive_ids"
                               (gethash
                                "validation_report"
                                (%recursive-event-payload invalid)))))
                          (search "rising or latent" repair-prompt)
                          (search "exactly once" repair-prompt))))
        (crm-check "consolidation groups overlap without deleting motives"
                   (and (string= "consolidation-updated"
                                 (gethash "status" first-pass))
                        (= 2 (length presented))
                        (= 3 (gethash "open_motive_count" inspection))
                        (= 2 (gethash "attention_register_count" inspection))
                        (= 2
                           (length
                            (gethash "source_motive_ids"
                                     (aref presented 0))))))
        (crm-check "consolidation presentation is ordered but never suppresses dormancy"
                   (and (string= "foreground"
                                 (gethash "attention_state"
                                          (aref presented 0)))
                        (equalp
                         (gethash "observation_event_ids" (aref presented 0))
                         (coerce
                          (remove-duplicates
                           (append
                            (coerce
                             (gethash "observation_event_ids" first)
                             'list)
                            (coerce
                             (gethash "observation_event_ids" second)
                             'list))
                           :test #'equal)
                          'vector))
                        (string= "dormant"
                                 (gethash "attention_state"
                                          (aref presented 1)))))
        (crm-check "current consolidation is provider-silent and write-free"
                   (and (string= "consolidation-current"
                                 (gethash "status" current))
                        (= calls-after-first *crm-provider-calls*)
                        (= events-after-first (length *crm-events*))))
        (crm-check "private context and inspection expose current reframing"
                   (and (some
                         (lambda (row)
                           (search "generative interest arc"
                                   (gethash "content" row)))
                         (coerce raw-context 'list))
                        (string= "current"
                                 (gethash "consolidation_status" inspection))
                        (= 2
                           (gethash "thread_count"
                                    (gethash "consolidation" inspection)))))
        (crm-check
         "grouped attention derives evidence from the selected thread"
         (let ((choice
                 (%recursive-curiosity-attention-choice
                (obj
                 "role" "assistant" "content" :null
                 "tool_calls"
                 (vector
                  (obj
                   "id" "cross-thread-evidence" "type" "function"
                   "function"
                   (obj
                    "name" "choose-curiosity"
                    "arguments"
                    (shasht:write-json
                     (obj
                      "question" (gethash "question" (aref presented 1))
                      "source_motive_ids"
                      (gethash "source_motive_ids" (aref presented 1))
                      "evidence_event_ids"
                      (vector
                       (aref
                        (gethash "observation_event_ids" (aref presented 0))
                        0)))
                     nil)))))
                presented)))
           (equalp (gethash "observation_event_ids" (aref presented 1))
                   (gethash "evidence_event_ids" choice))))
        (progn
          (setf *crm-provider-script*
                (list
                 (list :tool "write-private-briefing"
                       (shasht:write-json
                        (obj
                         "content"
                         "Selective fascination is foreground; an implementation detail remains available in the background.")
                        nil))))
          (let ((briefing (conscious-recursive-curiosity-briefing-one)))
            (crm-check "briefing consumes consolidated orientation"
                       (and (string= "briefing-updated"
                                     (gethash "status" briefing))
                            (some
                             (lambda (messages)
                               (some
                                (lambda (message)
                                  (search "generative interest arc"
                                          (gethash "content" message "")))
                                messages))
                             *crm-provider-messages*)))))
        (record-one "A new question changes the exact register"
                    "What new evidence should reshape the current interests?")
        (crm-check
         "captured max-token truncation remains invalid structured input"
         (handler-case
             (progn
               (%recursive-curiosity-consolidation-action
                (obj
                 "role" "assistant"
                 "content"
                 "I will organize these curiosities into semantic threads."
                 "tool_calls"
                 (vector
                  (obj "id" "captured-truncation" "type" "function"
                       "function"
                       (obj "name" "write-curiosity-consolidation"
                            "arguments" "{\"threads\": "))))
                open)
               nil)
           (error () t)))
        (multiple-value-bind (fallback ignored completion)
            (%recursive-curiosity-consolidated-register
             (%recursive-thread-events))
          (declare (ignore ignored))
          (crm-check "source change immediately invalidates stale consolidation"
                     (and (null completion) (= 4 (length fallback)))))
        (let* ((current-open
                 (nth-value
                  0
                  (%recursive-curiosity-open-register
                   (%recursive-thread-events) most-positive-fixnum 0)))
               (invalid-current
                 (map
                  'vector
                  (lambda (row)
                    (obj "question" (gethash "question" row)
                         "source_motive_ids"
                         (vector (gethash "motive_id" row))
                         "attention_state" "rising"
                         "rationale"
                         "The phase label was copied into attention state."))
                  current-open)))
          (setf *crm-provider-script*
                (list
                 (list :tool "write-curiosity-consolidation"
                       (shasht:write-json
                        (obj "threads" invalid-current) nil))
                 (list :tool "write-curiosity-consolidation"
                       (shasht:write-json
                        (obj "threads" invalid-current) nil)))))
        (let* ((calls-before-failure *crm-provider-calls*)
               (failed
                 (conscious-recursive-curiosity-consolidation-one))
               (calls-after-failure *crm-provider-calls*)
               (events-after-failure (length *crm-events*))
               (settled
                 (conscious-recursive-curiosity-consolidation-one)))
          (crm-check "invalid consolidation repair settles exactly one revision"
                     (and (string= "failed" (gethash "status" failed))
                          (= (+ calls-before-failure 2)
                             calls-after-failure)
                          (= 1
                             (count
                              "recursive-curiosity-consolidation-failed"
                              *crm-events*
                              :key (lambda (event)
                                     (gethash "type" event ""))
                                  :test #'string=))))
          (crm-check "consolidation repair cannot recurse"
                     (= 1
                        (count-if
                         (lambda (event)
                           (string=
                            "consolidation-repair-invalid"
                            (gethash
                             "error_code"
                             (%recursive-event-payload event) "")))
                         *crm-events*)))
          (crm-check "failed consolidation never retries and raw state survives"
                     (and
                      (string= "consolidation-failure-settled"
                               (gethash "status" settled))
                      (= calls-after-failure *crm-provider-calls*)
                      (= events-after-failure (length *crm-events*))
                      (= 4
                         (length
                          (nth-value
                           0
                           (%recursive-curiosity-consolidated-register
                            (%recursive-thread-events)))))))
          (setf *crm-provider-script*
                (list "No open curiosity pulls strongly enough right now."))
          (let ((attention (conscious-recursive-curiosity-attention-one)))
            (crm-check "consolidation anomaly does not block raw attention"
                       (string= "attention-declined"
                                (gethash "status" attention)))))))))

(defun crm-native-call (id name arguments)
  (obj "id" id "type" "function" "function"
       (obj "name" name "arguments" arguments)))

(defun crm-provider-response-with-calls (calls)
  (obj "choices"
       (vector
        (obj "message"
             (obj "role" "assistant" "content" :null
                  "tool_calls" (coerce calls 'vector))))))

(defun crm-provider-response-with-calls-and-reasoning (calls reasoning-details)
  (let* ((response (crm-provider-response-with-calls calls))
         (message (gethash "message" (aref (gethash "choices" response) 0))))
    (setf (gethash "reasoning_details" message) reasoning-details)
    response))

;; Wire normalization owns every accepted ID, rejects a malformed retained
;; prefix atomically, and treats excess untrusted calls as inert degradation.
(let* ((*conscious-recursive-mind-tools-enabled-p* t)
       (two
         (crm-provider-response-with-calls
          (list
           (crm-native-call "provider-a" "brave-search"
                            "{\"query\":\"alpha\",\"count\":2}")
           (crm-native-call "provider-b" "web-fetch"
                            "{\"url\":\"https://example.com\"}")))))
  (multiple-value-bind (message arguments overflow)
      (%recursive-normalize-assistant-message
       two "thread:batch-wire" "model:batch-wire" t)
    (let ((calls (gethash "tool_calls" message)))
      (crm-check "wire adapter accepts a bounded native tool batch"
                 (and (= 2 (length calls)) (= 2 (length arguments))
                      (null overflow)))
      (crm-check "wire adapter mints one distinct owned id per batch member"
                 (and (not (string= (gethash "id" (aref calls 0))
                                    (gethash "id" (aref calls 1))))
                      (search ":0" (gethash "id" (aref calls 0)) :from-end t)
                      (search ":1" (gethash "id" (aref calls 1)) :from-end t)))))
  (let* ((*conscious-recursive-mind-max-reasoning-details-characters* 128)
         (oversized
           (crm-provider-response-with-calls-and-reasoning
            (list (crm-native-call "provider-reasoning" "brave-search"
                                   "{\"query\":\"alpha\",\"count\":2}"))
            (vector (obj "type" "reasoning.encrypted"
                         "data" (make-string 256 :initial-element #\x))))))
    (multiple-value-bind (message arguments overflow)
        (%recursive-normalize-assistant-message
         oversized "thread:reasoning" "model:reasoning" t)
      (crm-check "oversized reasoning does not reject a valid native tool call"
                 (and (= 1 (length (gethash "tool_calls" message)))
                      (= 1 (length arguments))))
      (crm-check "oversized opaque reasoning is omitted whole and receipted"
                 (and (not (nth-value 1
                             (gethash "reasoning_details" message)))
                      (string= "omitted-over-bound"
                               (gethash "reasoning_details_status" overflow))
                      (> (gethash "reasoning_details_encoded_characters" overflow)
                         (gethash "reasoning_details_limit_characters" overflow))))))
  (let* ((bad-retained
           (crm-provider-response-with-calls
            (list
             (crm-native-call "provider-a" "bash"
                              "{\"command\":\"echo ok\"}")
             (obj "id" "provider-b" "type" "not-a-function"
                  "function"
                  (obj "name" "bash"
                       "arguments" "{\"command\":\"echo bad\"}")))))
         (rejected-p
           (handler-case
               (progn
                 (%recursive-normalize-assistant-message
                  bad-retained "thread:bad" "model:bad" t)
                 nil)
             (error () t))))
    (crm-check "one malformed retained call rejects the whole batch" rejected-p))
  (let* ((*conscious-recursive-mind-graph-search-fn*
           (lambda (request) (declare (ignore request))
             (obj "schema_version" 1 "status" "empty" "paths" #())))
         (graph-batch
           (crm-provider-response-with-calls
            (list
             (crm-native-call
              "provider-graph" "search-graph"
              "{\"starting_node_id\":\"fixtureoperator\",\"query\":\"project schedule\",\"predicates\":[],\"direction\":\"both\",\"maximum_depth\":2,\"maximum_paths\":10}")
             (crm-native-call
              "provider-memory" "search-memory"
              "{\"query\":\"knowledge graph deployment\",\"limit\":3}")))))
    (multiple-value-bind (message arguments overflow)
        (%recursive-normalize-assistant-message
         graph-batch "thread:graph-batch" "model:graph-batch" t)
      (declare (ignore arguments overflow))
      (let ((calls (gethash "tool_calls" message)))
        (crm-check "wire adapter accepts search-graph in a native batch"
                   (and (= 2 (length calls))
                        (string= "search-graph"
                                 (gethash "name"
                                          (gethash "function"
                                                   (aref calls 0))))
                        (string= "search-memory"
                                 (gethash "name"
                                          (gethash "function"
                                                   (aref calls 1)))))))))
  (let* ((*conscious-recursive-mind-tools-enabled-p* nil)
         (*conscious-recursive-mind-deliberate-curiosity-enabled-p* nil)
         (*conscious-recursive-mind-graph-search-fn*
           (lambda (request) (declare (ignore request))
             (obj "schema_version" 1 "status" "empty" "paths" #())))
         (graph-only
           (crm-provider-response-with-calls
            (list
             (crm-native-call "provider-graph-only" "search-graph"
                              "{\"query\":\"cat name\"}"))))
         (shell-call
           (crm-provider-response-with-calls
            (list
             (crm-native-call "provider-shell-disabled" "bash"
                              "{\"command\":\"echo no\"}")))))
    (crm-check "wire adapter accepts independently advertised graph-only tool"
               (handler-case
                   (multiple-value-bind (message arguments)
                       (%recursive-normalize-assistant-message
                        graph-only "thread:graph-only" "model:graph-only" t)
                     (and (= 1 (length (gethash "tool_calls" message)))
                          (= 1 (length arguments))
                          (string= "cat name"
                                   (gethash "query" (aref arguments 0)))))
                 (error () nil)))
    (crm-check "graph-only authority does not enable the broad tool suite"
               (handler-case
                   (progn
                     (%recursive-normalize-assistant-message
                      shell-call "thread:shell-disabled"
                      "model:shell-disabled" t)
                     nil)
                 (error () t))))
  (let* ((overflow-response
           (crm-provider-response-with-calls
            (list
             (crm-native-call "p0" "bash" "{\"command\":\"echo 0\"}")
             (crm-native-call "p1" "bash" "{\"command\":\"echo 1\"}")
             (crm-native-call "p2" "bash" "{\"command\":\"echo 2\"}")
             (crm-native-call "p3" "bash" "{\"command\":\"echo 3\"}")
             ;; This malformed discarded call must never have its arguments
             ;; parsed or gain authority.
             (obj "id" "p4" "type" "broken" "function" (obj))
             (crm-native-call "p5" "bash" "not-json")))))
    (multiple-value-bind (message arguments overflow)
        (%recursive-normalize-assistant-message
         overflow-response "thread:overflow" "model:overflow" t)
      (declare (ignore arguments))
      (crm-check "overflow accepts only the bounded prefix"
                 (= 4 (length (gethash "tool_calls" message))))
      (crm-check "overflow records bounded inert degradation metadata"
                 (and (= 2 (gethash "dropped_tool_call_count" overflow))
                      (equalp (vector "<invalid>" "bash")
                              (gethash "dropped_tool_names" overflow)))))))

;; The originally failing research shape completes with two executions and one
;; model re-entry. The provider transcript contains one N-call assistant row
;; followed by matching tool rows in exact order.
(setf *crm-events* nil
      *crm-provider-calls* 0
      *crm-provider-messages* nil
      *crm-provider-tool-counts* nil
      *crm-tool-executions* nil
      *crm-provider-script*
      (list
       (list :tools
             (list
              (list "brave-search"
                    "{\"query\":\"comparable agent architecture\",\"count\":3}")
              (list "brave-search"
                    "{\"query\":\"persistent recursive AI mind\",\"count\":3}")))
       "I compared the two independent searches."))
(conscious-recursive-mind-configure
 :agent-id "recursive-fixture"
 :endpoint "http://127.0.0.1:1234/v1/chat/completions"
 :model "fixture" :context-profile (obj)
 :tools-enabled-p t
 :tool-executor
 (lambda (name arguments context)
   (push (list name arguments context) *crm-tool-executions*)
   "bounded search result"))
(let* ((result
         (conscious-recursive-mind-submit
          "Are other people building a comparable architecture?"))
       (chronological (reverse *crm-provider-messages*))
       (continuation (second chronological))
       (assistant (second continuation))
       (first-tool (third continuation))
       (second-tool (fourth continuation))
       (calls (gethash "tool_calls" assistant)))
  (crm-check "research-shaped two-call batch completes end to end"
             (and (string= "replied" (gethash "status" result))
                  (= 2 *crm-provider-calls*)
                  (= 2 (length *crm-tool-executions*))))
  (crm-check "batch continuation is one assistant followed by matching tools"
             (and (= 4 (length continuation))
                  (string= "assistant" (gethash "role" assistant ""))
                  (= 2 (length calls))
                  (string= (gethash "id" (aref calls 0))
                           (gethash "tool_call_id" first-tool ""))
                  (string= (gethash "id" (aref calls 1))
                           (gethash "tool_call_id" second-tool "")))))

;; A sparse model-authored graph request must survive the whole durable loop,
;; not merely pass a normalizer unit test while the raw input reaches KG4.
(let ((received nil))
  (setf *crm-events* nil
        *crm-provider-calls* 0
        *crm-provider-messages* nil
        *crm-provider-tool-counts* nil
        *crm-provider-script*
        (list (list :tools
                    (list (list "search-graph"
                                "{\"query\":\"health issues\",\"maximum_depth\":2,\"maximum_paths\":10}")))
              "The graph search completed without matching paths."))
  (conscious-recursive-mind-configure
   :agent-id "recursive-fixture"
   :endpoint "http://127.0.0.1:1234/v1/chat/completions"
   :model "fixture" :context-profile (obj) :tools-enabled-p t
   :tool-executor
   (lambda (&rest ignored)
     (declare (ignore ignored))
     (error "Graph fixture must use its dedicated read port"))
   :graph-search-fn
   (lambda (request)
     (push request received)
     (knowledge-graph-hybrid-retrieve
      request #() #()
      (lambda (closed sources events)
        (declare (ignore sources events))
        (obj "schema_version" 1 "status" "empty" "paths" #()
             "echo_query" (gethash "query" closed))))))
  (let ((result (conscious-recursive-mind-submit
                 "What were my health issues earlier this year?")))
    (crm-check "sparse graph request executes through real hybrid validation"
               (and (string= "replied" (gethash "status" result))
                    (= 2 *crm-provider-calls*)
                    (= 1 (length received))
                    (knowledge-graph-search-request-valid-p (first received))))))

;; A semantically invalid native argument object is a refused tool attempt, not
;; a failed model boundary. The validation detail is returned to the model so a
;; corrected request can execute in the same root.
(let ((received nil))
  (setf *crm-events* nil
        *crm-provider-calls* 0
        *crm-provider-tool-counts* nil
        *crm-provider-script*
        (list (list :tool "search-graph"
                    "{\"query\":\"pets\",\"predicates\":[\"owned_by\"]}")
              (list :tool "search-graph"
                    "{\"query\":\"pets\",\"predicates\":[\"related_to\"]}")
              "The corrected graph request completed."))
  (conscious-recursive-mind-configure
   :agent-id "recursive-fixture"
   :endpoint "http://127.0.0.1:1234/v1/chat/completions"
   :model "fixture" :context-profile (obj) :tools-enabled-p t
   :tool-executor
   (lambda (&rest ignored)
     (declare (ignore ignored))
     (error "Graph fixture must use its dedicated read port"))
   :graph-search-fn
   (lambda (request)
     (push request received)
     (obj "schema_version" 1 "status" "empty" "paths" #())))
  (let* ((result (conscious-recursive-mind-submit "Find the pets."))
         (results
           (loop for event in *crm-events*
                 for payload = (gethash "payload" event)
                 when (and (string= "recursive-tool-result"
                                    (gethash "type" event ""))
                           (hash-table-p payload)
                           (string= "search-graph"
                                    (gethash "tool_name" payload "")))
                   collect payload)))
    (crm-check "invalid graph arguments can be corrected in the same turn"
               (and (string= "replied" (gethash "status" result))
                    (= 3 *crm-provider-calls*)
                    (= 1 (length received))
                    (= 1 (count "refused" results
                                :key (lambda (payload)
                                       (gethash "execution_status" payload ""))
                                :test #'string=))
                    (= 1 (count "executed" results
                                :key (lambda (payload)
                                       (gethash "execution_status" payload ""))
                                :test #'string=))))
    (crm-check "invalid graph result explains that parameters may be retried"
               (some (lambda (payload)
                       (and (string= "invalid-tool-arguments"
                                     (gethash "refusal_reason" payload ""))
                            (search "Correct the parameters and try again"
                                    (gethash "content" payload ""))))
                     results))))

;; Graph's independent read authority must not leak into a forced final
;; synthesis request. A duplicate call closes tools and the next boundary is
;; genuinely tool-free.
(let ((executions 0))
  (setf *crm-events* nil
        *crm-provider-calls* 0
        *crm-provider-tool-counts* nil
        *crm-provider-script*
        (list (list :tool "search-graph" "{\"query\":\"pets\"}")
              (list :tool "search-graph" "{\"query\":\"pets\"}")
              "The first graph result is sufficient."))
  (conscious-recursive-mind-configure
   :agent-id "recursive-fixture"
   :endpoint "http://127.0.0.1:1234/v1/chat/completions"
   :model "fixture" :context-profile (obj) :tools-enabled-p t
   :tool-executor
   (lambda (&rest ignored)
     (declare (ignore ignored))
     (error "Graph fixture must use its dedicated read port"))
   :graph-search-fn
   (lambda (request)
     (declare (ignore request))
     (incf executions)
     (obj "schema_version" 1 "status" "empty" "paths" #())))
  (let ((result (conscious-recursive-mind-submit "Check the pets once.")))
    (crm-check "forced graph synthesis converges without a refusal storm"
               (and (string= "replied" (gethash "status" result))
                    (= 3 *crm-provider-calls*)
                    (= 1 executions)))
    (crm-check "forced final synthesis advertises no graph tool"
               (zerop (car *crm-provider-tool-counts*)))))

;; Overflow evidence is journal metadata, never an unknown field on the
;; assistant message that will be sent back to the provider.
(let* ((overflow-calls
         (vector
          (crm-native-call "p0" "bash" "{\"command\":\"echo 0\"}")
          (crm-native-call "p1" "bash" "{\"command\":\"echo 1\"}")
          (crm-native-call "p2" "bash" "{\"command\":\"echo 2\"}")
          (crm-native-call "p3" "bash" "{\"command\":\"echo 3\"}")
          (obj "id" "p4" "type" "broken" "function" (obj))
          (crm-native-call "p5" "bash" "not-json")))
       (overflow-message
         (obj "role" "assistant" "content" :null
              "tool_calls" overflow-calls)))
  (setf *crm-events* nil
        *crm-provider-calls* 0
        *crm-provider-messages* nil
        *crm-provider-tool-counts* nil
        *crm-tool-executions* nil
        *crm-provider-script*
        (list (list :message overflow-message)
              "I used the bounded accepted prefix."))
  (conscious-recursive-mind-configure
   :agent-id "recursive-fixture"
   :endpoint "http://127.0.0.1:1234/v1/chat/completions"
   :model "fixture" :context-profile (obj) :tools-enabled-p t
   :tool-executor
   (lambda (name arguments context)
     (push (list name arguments context) *crm-tool-executions*) "ok"))
  (let* ((result (conscious-recursive-mind-submit "bound an oversized batch"))
         (response
           (find-if
            (lambda (event)
              (let ((payload (gethash "payload" event)))
                (and (string= "model-response" (gethash "type" event ""))
                     (hash-table-p payload)
                     (gethash "dropped_tool_call_count" payload))))
            *crm-events*))
         (payload (and response (gethash "payload" response)))
         (assistant (and payload (gethash "assistant_message" payload))))
    (crm-check "oversized provider batch still reaches a public reply"
               (and (string= "replied" (gethash "status" result))
                    (= 4 (length *crm-tool-executions*))))
    (crm-check "overflow evidence persists outside the provider transcript"
               (and (= 2 (gethash "dropped_tool_call_count" payload))
                    (equalp (vector "<invalid>" "bash")
                            (gethash "dropped_tool_names" payload))
                    (null (gethash "dropped_tool_call_count" assistant))))))

;; Background preemption remains per tool call. A restart/resume observes the
;; first durable result and continues at the second member without resampling
;; the provider or repeating the first effect.
(setf *crm-events* nil
      *crm-provider-calls* 0
      *crm-provider-messages* nil
      *crm-provider-tool-counts* nil
      *crm-tool-executions* nil
      *conscious-recursive-mind-curiosity-enabled-p* t
      *conscious-recursive-mind-operator-pending-p* nil)
(let* ((source-id
         (log-event "user-message"
                    (obj "text" "A private batch question"
                         "channel" "terminal"
                         "metadata" (obj "source" "recursive-mind-v1"))))
       (motive-id nil)
       (observation-id nil))
  (multiple-value-bind (ignored appended-p recorded-motive-id)
      (%recursive-record-curiosity
       "Can a private batch resume exactly?" source-id
       :supporting-event-ids (list source-id)
       :evidence-identity-event-ids (list source-id)
       :source-revision "batch-preemption-fixture")
    (declare (ignore ignored appended-p))
    (setf motive-id recorded-motive-id))
  (setf observation-id
        (gethash "id"
                 (find "conscious-curiosity-observed" *crm-events*
                       :key (lambda (event) (gethash "type" event ""))
                       :test #'string= :from-end t)))
  (%recursive-ensure-curiosity-focus
   700 "Can a private batch resume exactly?" motive-id nil
   (list observation-id))
  (setf *crm-provider-script*
        (list
         (list :tools
               (list (list "bash" "{\"command\":\"echo private-a\"}")
                     (list "bash" "{\"command\":\"echo private-b\"}")))
         "The private batch resumed exactly."))
  (conscious-recursive-mind-configure
   :agent-id "recursive-fixture"
   :endpoint "http://127.0.0.1:1234/v1/chat/completions"
   :model "fixture" :context-profile (obj)
   :tools-enabled-p t :curiosity-enabled-p t
   :tool-executor
   (lambda (name arguments context)
     (push (list name arguments context) *crm-tool-executions*)
     (when (= 1 (length *crm-tool-executions*))
       (setf *conscious-recursive-mind-operator-pending-p* t))
     "private evidence"))
  (let ((paused (conscious-recursive-curiosity-wake-one)))
    (crm-check "operator input preempts a private batch between calls"
               (and (string= "preempted" (gethash "status" paused))
                    (= 1 (length *crm-tool-executions*))
                    (= 1 *crm-provider-calls*))))
  (setf *conscious-recursive-mind-operator-pending-p* nil)
  (let ((resumed (conscious-recursive-curiosity-wake-one)))
    (crm-check "preempted private batch resumes without resampling or repetition"
               (and (string= "curiosity-completed"
                             (gethash "status" resumed))
                    (= 2 (length *crm-tool-executions*))
                    (= 2 *crm-provider-calls*))))
  (setf *conscious-recursive-mind-operator-pending-p* nil))

;; Replay after k of N results selects k+1. A durable intent for that exact
;; member remains outcome-unknown rather than becoming executable again.
(let* ((root
         (crm-event 200 "user-message"
                    (obj "text" "batch restart" "channel" "terminal"
                         "metadata"
                         (obj "source" "recursive-mind-v1"
                              "thread_id" "thread:batch-restart"))))
       (request
         (crm-event 201 "model-request"
                    (obj "thread_id" "thread:batch-restart"
                         "model_call_id" "model:batch-restart") 200))
       (call-a
         (crm-native-call "tool:batch:a" "bash"
                          "{\"command\":\"echo a\"}"))
       (call-b
         (crm-native-call "tool:batch:b" "bash"
                          "{\"command\":\"echo b\"}"))
       (response
         (crm-event 202 "model-response"
                    (obj "thread_id" "thread:batch-restart"
                         "model_call_id" "model:batch-restart"
                         "status" "accepted" "assistant_message"
                         (obj "role" "assistant" "content" :null
                              "tool_calls" (vector call-a call-b))) 200))
       (execution-a
         (crm-event 203 "recursive-tool-execution"
                    (obj "thread_id" "thread:batch-restart"
                         "model_call_id" "model:batch-restart"
                         "tool_call_id" "tool:batch:a" "tool_name" "bash") 200))
       (result-a
         (crm-event 204 "recursive-tool-result"
                    (obj "thread_id" "thread:batch-restart"
                         "model_call_id" "model:batch-restart"
                         "tool_call_id" "tool:batch:a" "tool_name" "bash"
                         "execution_status" "executed" "content" "a") 200))
       (execution-b
         (crm-event 205 "recursive-tool-execution"
                    (obj "thread_id" "thread:batch-restart"
                         "model_call_id" "model:batch-restart"
                         "tool_call_id" "tool:batch:b" "tool_name" "bash") 200))
       (ready
         (conscious-recursive-thread-project
          (list root request response execution-a result-a)
          200 "recursive-fixture"))
       (claimed
         (conscious-recursive-thread-project
          (list root request response execution-a result-a execution-b)
          200 "recursive-fixture")))
  (crm-check "batch replay resumes at the first member without a result"
             (and (string= "tool-ready" (gethash "state" ready))
                  (string= "tool:batch:b"
                           (gethash "id" (gethash "tool_call" ready)))))
  (crm-check "restart never re-executes a claimed batch member"
             (and (string= "outcome-unknown" (gethash "state" claimed))
                  (string= "tool:batch:b"
                           (gethash "tool_call_id" claimed))))
  (let ((out-of-order
          (crm-event 206 "recursive-tool-result"
                     (obj "thread_id" "thread:batch-restart"
                          "model_call_id" "model:batch-restart"
                          "tool_call_id" "tool:batch:b" "tool_name" "bash"
                          "execution_status" "refused" "content" "no") 200)))
    (crm-check "replay rejects an out-of-order batch result"
               (handler-case
                   (progn
                     (conscious-recursive-thread-project
                      (list root request response out-of-order)
                      200 "recursive-fixture")
                     nil)
                 (error () t))))
  (let* ((duplicate-response
           (crm-event 207 "model-response"
                      (obj "thread_id" "thread:batch-restart"
                           "model_call_id" "model:batch-restart"
                           "status" "accepted" "assistant_message"
                           (obj "role" "assistant" "content" :null
                                "tool_calls" (vector call-a call-a))) 200)))
    (crm-check "replay independently rejects duplicate runtime call ids"
               (handler-case
                   (progn
                     (conscious-recursive-thread-project
                      (list root request duplicate-response)
                      200 "recursive-fixture")
                     nil)
                 (error () t)))))

;; Duplicate closure and result-pressure closure both finish every remaining
;; member with explicit durable evidence; neither can wedge a replayed batch.
(setf *crm-events* nil
      *crm-provider-calls* 0
      *crm-provider-messages* nil
      *crm-provider-tool-counts* nil
      *crm-tool-executions* nil
      *crm-provider-script*
      (list
       (list :tools
             (list (list "bash" "{\"command\":\"echo same\"}")
                   (list "bash" "{\"command\":\"echo same\"}")
                   (list "bash" "{\"command\":\"echo later\"}")))
       "I stopped after detecting repetition."))
(conscious-recursive-mind-configure
 :agent-id "recursive-fixture"
 :endpoint "http://127.0.0.1:1234/v1/chat/completions"
 :model "fixture" :context-profile (obj) :tools-enabled-p t
 :tool-executor
 (lambda (name arguments context)
   (push (list name arguments context) *crm-tool-executions*) "same"))
(let ((result (conscious-recursive-mind-submit "do not repeat batch effects")))
  (crm-check "intra-batch duplicate closes without repeating an effect"
             (and (string= "replied" (gethash "status" result))
                  (= 1 (length *crm-tool-executions*))))
  (crm-check "duplicate and later members both durably complete"
             (equal '("executed" "suppressed" "refused")
                    (loop for event in *crm-events*
                          when (string= "recursive-tool-result"
                                        (gethash "type" event ""))
                            collect (gethash "execution_status"
                                             (gethash "payload" event))))))

(setf *crm-events* nil
      *crm-provider-calls* 0
      *crm-provider-tool-counts* nil
      *crm-tool-executions* nil
      *crm-provider-script*
      (list
       (list :tools
             (list (list "bash" "{\"command\":\"echo evidence\"}")
                   (list "bash" "{\"command\":\"echo excess-1\"}")
                   (list "bash" "{\"command\":\"echo excess-2\"}")))
       "I synthesized within the evidence ceiling."))
(conscious-recursive-mind-configure
 :agent-id "recursive-fixture"
 :endpoint "http://127.0.0.1:1234/v1/chat/completions"
 :model "fixture" :context-profile (obj) :tools-enabled-p t
 :tool-executor
 (lambda (name arguments context)
   (push (list name arguments context) *crm-tool-executions*) "evidence"))
(let* ((*conscious-recursive-mind-max-total-tool-result-characters* 1)
       (result (conscious-recursive-mind-submit "bounded batch evidence")))
  (crm-check "mid-batch result pressure still reaches synthesis"
             (and (string= "replied" (gethash "status" result))
                  (= 1 (length *crm-tool-executions*))))
  (crm-check "mid-batch result pressure refuses every remaining member"
             (equal '("executed" "refused" "refused")
                    (loop for event in *crm-events*
                          when (string= "recursive-tool-result"
                                        (gethash "type" event ""))
                            collect (gethash "execution_status"
                                             (gethash "payload" event))))))

(setf *crm-events* nil
      *crm-provider-calls* 0
      *crm-provider-tool-counts* nil
      *crm-tool-executions* nil
      *crm-provider-script*
      (list
       (list :tools
             (list (list "bash" "{\"command\":\"echo allowed\"}")
                   (list "bash" "{\"command\":\"echo over-limit-1\"}")
                   (list "bash" "{\"command\":\"echo over-limit-2\"}")))
       "I respected the execution ceiling."))
(conscious-recursive-mind-configure
 :agent-id "recursive-fixture"
 :endpoint "http://127.0.0.1:1234/v1/chat/completions"
 :model "fixture" :context-profile (obj) :tools-enabled-p t
 :tool-executor
 (lambda (name arguments context)
   (push (list name arguments context) *crm-tool-executions*) "allowed"))
(let* ((*conscious-recursive-mind-max-tool-boundaries* 1)
       (result (conscious-recursive-mind-submit "one execution only")))
  (crm-check "mid-batch execution ceiling still reaches synthesis"
             (and (string= "replied" (gethash "status" result))
                  (= 1 (length *crm-tool-executions*))))
  (crm-check "execution ceiling durably refuses the unexecuted suffix"
             (equal '("executed" "refused" "refused")
                    (loop for event in *crm-events*
                          when (string= "recursive-tool-result"
                                        (gethash "type" event ""))
                            collect (gethash "execution_status"
                                             (gethash "payload" event))))))

;; A text-encoded legacy tool request is evidence, never executable authority.
;; One bounded repair may answer normally; repetition durably fails closed.
(let ((pseudo
        (format nil
                "<tool_call>~%<function=web-fetch>~%<parameter=url>https://arxiv.org/html/2607.12254</parameter>~%</function>~%</tool_call>")))
  (setf *crm-events* nil
        *crm-provider-calls* 0
        *crm-provider-messages* nil
        *crm-provider-tool-counts* nil
        *crm-tool-executions* nil
        *crm-provider-script*
        (list pseudo "I can answer from the evidence already collected."))
  (conscious-recursive-mind-configure
   :agent-id "recursive-fixture"
   :endpoint "http://127.0.0.1:1234/v1/chat/completions"
   :model "fixture" :context-profile (obj) :tools-enabled-p t
   :tool-executor
   (lambda (&rest ignored)
     (declare (ignore ignored)) (error "pseudo-tool text must not execute")))
  (let* ((result (conscious-recursive-mind-submit "finish the research"))
         (refusals
           (remove-if-not
            (lambda (event)
              (string= "recursive-pseudo-tool-refusal"
                       (gethash "type" event "")))
            *crm-events*))
         (repaired-request (first *crm-provider-messages*)))
    (crm-check "pseudo-tool publication receives one bounded repair"
               (and (string= "replied" (gethash "status" result))
                    (= 2 *crm-provider-calls*)
                    (= 1 (length refusals))
                    (null *crm-tool-executions*)
                    (string= "I can answer from the evidence already collected."
                             (gethash "content" result))))
    (crm-check "repair request carries rejected evidence and runtime instruction"
               (and
                (some (lambda (message)
                        (and (string= "assistant" (gethash "role" message ""))
                             (string= pseudo (gethash "content" message ""))))
                      repaired-request)
                (some (lambda (message)
                        (and (string= "system" (gethash "role" message ""))
                             (string=
                              *conscious-recursive-pseudo-tool-repair-instruction*
                              (gethash "content" message ""))))
                      repaired-request)))
    (crm-check "pseudo-tool envelope never becomes a public agent message"
               (notany
                (lambda (event)
                  (and (string= "agent-message" (gethash "type" event ""))
                       (search "<tool_call>"
                               (gethash "text" (gethash "payload" event) ""))))
                *crm-events*)))

  (setf *crm-events* nil
        *crm-provider-calls* 0
        *crm-provider-messages* nil
        *crm-provider-tool-counts* nil
        *crm-tool-executions* nil
        *crm-provider-script* (list pseudo pseudo))
  (let* ((result (conscious-recursive-mind-submit "repeat no protocol text"))
         (refusals
           (remove-if-not
            (lambda (event)
              (string= "recursive-pseudo-tool-refusal"
                       (gethash "type" event "")))
            *crm-events*)))
    (crm-check "repeated pseudo-tool envelope fails closed without a loop"
               (and (string= "failed" (gethash "status" result))
                    (string= "recursive-pseudo-tool-repeat"
                             (gethash "error_code" result))
                    (= 2 *crm-provider-calls*)
                    (= 2 (length refusals))
                    (null *crm-tool-executions*)
                    (notany (lambda (event)
                              (string= "agent-message"
                                       (gethash "type" event "")))
                            *crm-events*)))))

  ;; The same terminal quarantine is a durable failure receipt for private
  ;; curiosity.  It must release the focus instead of paying to repeat the
  ;; identical failed investigation on every quiet wake.
  (let ((private-pseudo
          (format nil
                  "<tool_call>~%<function=web-fetch>~%<parameter=url>https://example.invalid/private</parameter>~%</function>~%</tool_call>")))
    (setf *crm-events* nil
        *crm-provider-calls* 0
        *crm-provider-messages* nil
        *crm-provider-tool-counts* nil
        *crm-tool-executions* nil
        *crm-provider-script* (list private-pseudo private-pseudo)
        *conscious-recursive-mind-curiosity-enabled-p* t)
    (let* ((source-id
           (log-event "user-message"
                      (obj "text" "Can a quarantined private protocol failure settle?"
                           "channel" "terminal"
                           "metadata" (obj "source" "recursive-mind-v1"))))
         (motive-id
           (nth-value
            2
            (%recursive-record-curiosity
             "Can a quarantined private protocol failure settle?" source-id
             :supporting-event-ids (list source-id)
             :evidence-identity-event-ids (list source-id)
             :source-revision "pseudo-private-failure-fixture")))
         (observation
           (find "conscious-curiosity-observed" *crm-events*
                 :key (lambda (event) (gethash "type" event ""))
                 :test #'string= :from-end t))
         (focus
           (nth-value
            0
            (%recursive-ensure-curiosity-focus
             900 "Can a quarantined private protocol failure settle?"
             motive-id nil (list (gethash "id" observation)))))
         (failed (conscious-recursive-curiosity-wake-one))
         (failure
           (find "recursive-curiosity-focus-failed" *crm-events*
                 :key (lambda (event) (gethash "type" event ""))
                 :test #'string= :from-end t))
         (payload (and failure (gethash "payload" failure))))
    (crm-check "terminal pseudo-tool quarantine settles a private focus"
               (and (string= "failed" (gethash "status" failed))
                    (equal (gethash "id" focus)
                           (gethash "caused_by" failure))
                    (string= "recursive-pseudo-tool-refusal"
                             (gethash "failure_receipt_type" payload))
                    (integerp (gethash "failure_receipt_event_id" payload))
                    (eq :null
                        (gethash "failed_model_response_event_id" payload))))
    (let ((calls *crm-provider-calls*)
          (again (conscious-recursive-curiosity-wake-one)))
      (crm-check "settled pseudo-tool focus is not retried on the next wake"
                 (and (string= "idle" (gethash "status" again))
                      (= calls *crm-provider-calls*))))))

;; A durable execution intent without a result is never reclassified as ready.
(let* ((root (crm-event 100 "user-message"
                        (obj "text" "danger" "channel" "terminal"
                             "metadata"
                             (obj "source" "recursive-mind-v1"
                                  "thread_id" "thread:recursive-fixture:100"))))
       (request (crm-event 101 "model-request"
                           (obj "thread_id" "thread:recursive-fixture:100"
                                "model_call_id" "model:100") 100))
       (call (obj "id" "tool:100" "type" "function" "function"
                  (obj "name" "bash"
                       "arguments" "{\"command\":\"touch marker\"}")))
       (response (crm-event 102 "model-response"
                            (obj "thread_id" "thread:recursive-fixture:100"
                                 "model_call_id" "model:100" "status" "accepted"
                                 "assistant_message"
                                 (obj "role" "assistant" "content" :null
                                      "tool_calls" (vector call))) 100))
       (intent (crm-event 103 "recursive-tool-execution"
                          (obj "thread_id" "thread:recursive-fixture:100"
                               "model_call_id" "model:100"
                               "tool_call_id" "tool:100"
                               "tool_name" "bash") 100))
       (before-intent
         (conscious-recursive-thread-project
          (list root request response) 100 "recursive-fixture"))
       (after-intent
         (conscious-recursive-thread-project
          (list root request response intent) 100 "recursive-fixture")))
  (crm-check "unclaimed durable tool call is execution-ready"
             (string= "tool-ready" (gethash "state" before-intent)))
  (crm-check "claimed tool with absent result is outcome-unknown after restart"
             (string= "outcome-unknown" (gethash "state" after-intent))))

;; Episode sealing fixture: the provider supplies semantic fields through one
;; strict native call; the runtime supplies all identity and provenance.
(let* ((now (get-universal-time))
       (metadata (obj "source" "recursive-mind-v1" "persona_id" "fixture"))
       (user (crm-event 1 "user-message"
                        (obj "text" "Which faucets should we use?"
                             "metadata" metadata)))
       (agent (crm-event 2 "agent-message"
                         (obj "text" "We preferred brushed nickel."
                              "metadata" metadata) 1))
       (arguments
         (shasht:write-json
          (obj "synopsis"
               "The operator and persona discussed bathroom faucets and preferred brushed nickel."
               "subjects" (vector "bathroom renovation" "faucets")
               "entities" (vector "brushed nickel")
               "retrieval_cues" (vector "which faucets" "bathroom fixtures")
               "broader_categories" (vector "home renovation")
               "unresolved_threads" (vector "final faucet selection")) nil)))
  (setf (gethash "timestamp" user) (- now 9000)
        (gethash "timestamp" agent) (- now 8990)
        *crm-events* (list user agent)
        *crm-provider-calls* 0
        *crm-provider-output-caps* nil
        *crm-provider-script*
        (list (list :tool "write-conversation-episode" arguments)))
  (conscious-recursive-mind-configure
   :agent-id "recursive-fixture"
   :endpoint "http://127.0.0.1:1234/v1/chat/completions"
   :model "fixture" :context-profile (obj)
   :episodic-memory-enabled-p t
   :episode-provider-profile-fn
   (lambda ()
     (obj "endpoint" "http://127.0.0.1:1234/v1/chat/completions"
          "model" "episode-fixture")))
  (let* ((result (conscious-recursive-conversation-episode-seal-one))
         (sealed (find "conversation-episode-sealed" *crm-events*
                       :key (lambda (event) (gethash "type" event ""))
                       :test #'string=))
         (payload (and sealed (gethash "payload" sealed))))
    (crm-check "provider sealing commits one runtime-owned episode"
               (and (string= "episode-sealed" (gethash "status" result ""))
                    (= 1 *crm-provider-calls*)
                    (conversation-episode-sealed-payload-valid-p payload)))
    (crm-check "episode range and persona are runtime-owned"
               (and (= 1 (gethash "first_event_id" payload))
                    (= 2 (gethash "last_event_id" payload))
                    (equalp #(1 2) (gethash "source_event_ids" payload))
                    (string= "fixture" (gethash "persona_id" payload))))
    (crm-check "episode sealing uses its independent provider profile"
               (string= "episode-fixture" (first *crm-provider-models*)))
    (crm-check "episode sealing has no completion-token boundary"
               (null (first *crm-provider-output-caps*)))
    (crm-check "sealed episode is idempotently provider-silent"
               (let ((again
                       (conscious-recursive-conversation-episode-seal-one)))
                 (and (string= "idle" (gethash "status" again ""))
                      (= 1 *crm-provider-calls*))))))

(let* ((now (get-universal-time))
       (*conscious-recursive-mind-agent-id* "recursive-fixture")
       (*conscious-recursive-episode-protocol-revision*
         "recursive-conversation-episode-v2")
       (failure
         (crm-event
          9 "conversation-episode-seal-failed"
          (obj "schema_version" 1 "episode_id" "episode:retry"
               "protocol_revision" *conscious-recursive-episode-protocol-revision*
               "reason" "provider-or-protocol-failure"
               "failed_at" (- now 10))))
       (report (%recursive-episode-retry-report
                (list failure) "episode:retry" now)))
  (crm-check "failed episode waits before another provider attempt"
             (and (hash-table-p report)
                  (string= "retry-scheduled" (gethash "status" report ""))
                  (= 1 (gethash "attempt_count" report))
                  (= 50 (gethash "retry_after_seconds" report)))))

;; Backlog catch-up is fair but no longer artificially limited to one episode
;; per five-minute wake.
(let* ((now (get-universal-time))
       (metadata (obj "source" "recursive-mind-v1" "persona_id" "fixture"))
       (arguments
         (shasht:write-json
          (obj "synopsis" "Bounded backlog episode."
               "subjects" #( "backlog" ) "entities" #()
               "retrieval_cues" #( "catch up" )
               "broader_categories" #( "continuity" )
               "unresolved_threads" #()) nil))
       (events nil))
  (dotimes (index 4)
    (let* ((user-id (1+ (* index 2)))
           (agent-id (1+ user-id))
           (age (- 40000 (* index 9000)))
           (user (crm-event user-id "user-message"
                            (obj "text" (format nil "backlog ~d" index)
                                 "metadata" metadata)))
           (agent (crm-event agent-id "agent-message"
                             (obj "text" "acknowledged"
                                  "metadata" metadata)
                             user-id)))
      (setf (gethash "timestamp" user) (- now age)
            (gethash "timestamp" agent) (+ 10 (- now age)))
      (setf events (append events (list user agent)))))
  (setf *crm-events* events
        *crm-provider-calls* 0
        *crm-provider-script*
        (loop repeat 4 collect
              (list :tool "write-conversation-episode" arguments))
        *conscious-recursive-thread-events-cache* nil
        *conscious-recursive-thread-events-cache-key* nil)
  (conscious-recursive-mind-configure
   :agent-id "recursive-fixture"
   :endpoint "http://127.0.0.1:1234/v1/chat/completions"
   :model "fixture" :context-profile (obj)
   :episodic-memory-enabled-p t)
  (let ((result (conscious-recursive-conversation-episode-seal-batch)))
    (crm-check "one quiet batch drains four old episodes"
               (and (string= "episode-sealed" (gethash "status" result))
                    (= 4 (gethash "sealed_count" result))
                    (= 4 *crm-provider-calls*)
                    (= 4 (count "conversation-episode-sealed" *crm-events*
                                :key (lambda (event)
                                       (gethash "type" event ""))
                                :test #'string=))))))

;; The disposable graph is maintained through injected ports. A storage
;; anomaly must become a bounded retry report, never escape into the quiet
;; recursion or prevent the next cognitive action.
(let ((attempts 0) (continued 0) (notices nil))
  (conscious-recursive-mind-configure
   :agent-id "recursive-fixture"
   :endpoint "http://127.0.0.1:1234/v1/chat/completions"
   :model "fixture" :context-profile (obj)
   :episodic-memory-enabled-p t
   :episode-graph-maintenance-fn
   (lambda ()
     (incf attempts)
     (if (= attempts 1)
         (error "disposable graph fixture failed")
         (obj "schema_version" 1 "status" "synchronized"
              "mode" "incremental-tail" "episode_count" 4
              "node_count" 10 "edge_count" 6
              "through_storage_position" 99
              "event_write_count" 0 "memory_write_count" 0)))
   :episode-graph-inspect-fn
   (lambda ()
     (obj "schema_version" 1 "status" "restored"
          "episode_count" 4 "node_count" 10 "edge_count" 6
          "evidence_count" 18 "through_storage_position" 99
          "database_write_count" 0))
   :observer-fn
   (lambda (status item result)
     (push (list status item result) notices)))
  (let ((failed
          (%recursive-conversation-episode-graph-maintain "idle")))
    ;; This is the action that would follow graph maintenance in the real
    ;; quiet composition. It remains reachable after the swallowed anomaly.
    (incf continued)
    (crm-check "graph maintenance failure is bounded and retryable"
               (and (string= "failed" (gethash "status" failed ""))
                    (gethash "retry_scheduled" failed)
                    (= 1 continued)
                    (= 1 attempts)
                    (find "projection-anomaly" notices :key #'first
                          :test #'string=))))
  (let ((repaired
          (%recursive-conversation-episode-graph-maintain "idle")))
    (crm-check "later quiet maintenance repairs without restart"
               (and (string= "synchronized"
                             (gethash "status" repaired ""))
                    (= 2 attempts))))
  (let ((current
          (%recursive-conversation-episode-graph-maintain "idle")))
    (crm-check "current graph skips redundant idle synchronization"
               (and (string= "current" (gethash "status" current ""))
                    (= 2 attempts))))
  (let ((inspection
          (conscious-recursive-conversation-episode-graph-inspect)))
    (crm-check "graph inspection is content-free and zero-mutation"
               (and (string= "restored" (gethash "status" inspection ""))
                    (= 4 (gethash "episode_count" inspection))
                    (= 0 (gethash "database_write_count" inspection -1))
                    (null (gethash "episodes" inspection))))))

;; Generic graph formation is an injected best-effort quiet port. Its own
;; owner controls authority; this composition only validates bounded outcomes
;; and guarantees that a disposable/protocol anomaly cannot halt quiet thought.
(let ((attempts 0) (continued 0) (notices nil))
  (conscious-recursive-mind-configure
   :agent-id "recursive-fixture"
   :endpoint "http://127.0.0.1:1234/v1/chat/completions"
   :model "fixture" :context-profile (obj)
   :episodic-memory-enabled-p t
   :knowledge-graph-formation-fn
   (lambda ()
     (incf attempts)
     (if (= attempts 1)
         (error "formation adapter fixture failed")
         (obj "schema_version" 1 "status" "sealed"
              "opened_event_id" 41 "sealed_event_id" 42)))
   :observer-fn
   (lambda (status item result)
     (push (list status item result) notices)))
  (let ((failed (%recursive-knowledge-graph-formation)))
    (incf continued)
    (crm-check "formation adapter anomaly is bounded and retryable"
               (and (string= "failed" (gethash "status" failed ""))
                    (gethash "retry_scheduled" failed)
                    (search "formation adapter fixture failed"
                            (gethash "detail" failed ""))
                    (= 1 continued)
                    (= 1 attempts)
                    (find "projection-anomaly" notices :key #'first
                          :test #'string=))))
  (let ((repaired (%recursive-knowledge-graph-formation)))
    (crm-check "later formation quantum repairs without restart"
               (and (string= "sealed" (gethash "status" repaired ""))
                    (= 2 attempts)))))

(dolist (status '("paused-provider" "retry-scheduled" "incomplete"))
  (let ((notices nil))
    (conscious-recursive-mind-configure
     :agent-id "recursive-fixture"
     :endpoint "http://127.0.0.1:1234/v1/chat/completions"
     :model "fixture" :context-profile (obj)
     :episodic-memory-enabled-p t
     :knowledge-graph-formation-fn
     (lambda () (obj "schema_version" 1 "status" status))
     :observer-fn
     (lambda (kind item result)
       (push (list kind item result) notices)))
    (let ((report (%recursive-knowledge-graph-formation)))
      (crm-check (format nil "formation accepts v6 owner status ~a" status)
                 (and (string= status (gethash "status" report ""))
                      (not (find "projection-anomaly" notices :key #'first
                                 :test #'string=)))))))

(conscious-recursive-mind-configure
 :agent-id "recursive-fixture"
 :endpoint "http://127.0.0.1:1234/v1/chat/completions"
 :model "fixture" :context-profile (obj)
 :episodic-memory-enabled-p t
 :knowledge-graph-formation-fn
 (lambda () (obj "schema_version" 1 "status" "fixture-unknown")))
(let ((report (%recursive-knowledge-graph-formation)))
  (crm-check "invalid formation status is visible without graph content"
             (and (string= "failed" (gethash "status" report ""))
                  (search "status=\"fixture-unknown\""
                          (gethash "detail" report "")))))

(let* ((now (get-universal-time))
       (metadata (obj "source" "recursive-mind-v1" "persona_id" "fixture"))
       (arguments
         (shasht:write-json
          (obj "synopsis" "Recovered accepted episode summary."
               "subjects" #("restart") "entities" #()
               "retrieval_cues" #("accepted response recovery")
               "broader_categories" #("runtime continuity")
               "unresolved_threads" #()) nil))
       (message
         (obj "role" "assistant" "content" :null "tool_calls"
              (vector
               (obj "id" "episode-recovery-call" "type" "function"
                    "function"
                    (obj "name" "write-conversation-episode"
                         "arguments" arguments)))))
       (user (crm-event 1 "user-message"
                        (obj "text" "recover this"
                             "metadata" metadata)))
       (agent (crm-event 2 "agent-message"
                         (obj "text" "accepted"
                              "metadata" metadata) 1))
       (opened
         (crm-event 3 "conversation-episode-seal-opened"
                    (obj "schema_version" 1
                         "episode_id" "episode:fixture:1:2"
                         "persona_id" "fixture"
                         "protocol_revision"
                         *conscious-recursive-episode-protocol-revision*)))
       (response
         (crm-event 4 "model-response"
                    (obj "status" "accepted" "conversation_episode" t
                         "assistant_message" message) 3)))
  (setf (gethash "timestamp" user) (- now 9000)
        (gethash "timestamp" agent) (- now 8990)
        *crm-events* (list user agent opened response)
        *crm-provider-calls* 0
        *conscious-recursive-thread-events-cache* nil
        *conscious-recursive-thread-events-cache-key* nil)
  (let ((result (conscious-recursive-conversation-episode-seal-one)))
    (crm-check "accepted episode response resumes without provider resampling"
               (and (string= "episode-sealed" (gethash "status" result ""))
                    (zerop *crm-provider-calls*)
                    (find "conversation-episode-sealed" *crm-events*
                          :key (lambda (event) (gethash "type" event ""))
                          :test #'string=)))))

;; Reading a quoted probe must not permanently manufacture architecture in the
;; live package.  Deliberate definitions made by the same eval seam do persist.
(let* ((artifact-name "CRM-READER-ONLY-ARTIFACT")
       (definition-name "CRM-DELIBERATE-EVAL-DEFINITION")
       (artifact (find-symbol artifact-name :agent))
       (definition (find-symbol definition-name :agent)))
  (when artifact (unintern artifact :agent))
  (when definition
    (when (fboundp definition) (fmakunbound definition))
    (unintern definition :agent))
  (let ((result (lisp-eval "'crm-reader-only-artifact")))
    (crm-check "lisp-eval removes inert symbols introduced only by the reader"
               (and (search "reader artifacts removed" result)
                    (null (find-symbol artifact-name :agent)))))
  (lisp-eval "(defun crm-deliberate-eval-definition () :kept)")
  (let ((symbol (find-symbol definition-name :agent)))
    (crm-check "lisp-eval preserves deliberate definitions"
               (and symbol (fboundp symbol)
                    (eq :kept (funcall symbol))))
    (when symbol
      (when (fboundp symbol) (fmakunbound symbol))
      (unintern symbol :agent))))

;; A context-preparation anomaly used to escape the recursive owner after the
;; operator message was admitted.  The web adapter swallowed that condition,
;; so the durable question and a permanent "thinking" row were all the
;; operator could see.  Qualify the shipped failure settlement as a complete
;; lifecycle: one receipt, replay across runtime revisions, terminal observer
;; evidence, full context timing, and a healthy next turn without restart.
(let ((assembly-function (symbol-function '%conversation-assembly-spec)))
  (setf *crm-events* nil
        *crm-provider-calls* 0
        *crm-provider-mode* :reply
        *crm-provider-script* nil
        *crm-notifications* nil
        *crm-time-phases* nil
        *conscious-recursive-thread-events-cache* nil
        *conscious-recursive-thread-events-cache-key* nil
        *conscious-recursive-thread-events-cache-head* nil
        *conscious-recursive-thread-events-cache-max-id* nil)
  (conscious-recursive-mind-configure
   :agent-id "recursive-fixture"
   :endpoint "http://127.0.0.1:1234/v1/chat/completions"
   :model "fixture" :context-profile (obj)
   :observer-fn
   (lambda (status item result)
     (push (list status item result) *crm-notifications*)))
  (unwind-protect
       (progn
         (setf (symbol-function '%conversation-assembly-spec)
               (lambda (&rest ignored)
                 (declare (ignore ignored))
                 (error "fixture context assembly failed")))
         (let* ((failed
                  (conscious-recursive-mind-submit
                   "admit this before context fails"))
                (root
                  (find "user-message" *crm-events*
                        :key (lambda (event) (gethash "type" event ""))
                        :test #'string=))
                (root-id (and root (gethash "id" root)))
                (receipts
                  (remove-if-not
                   (lambda (event)
                     (and (string= "recursive-root-failed"
                                   (gethash "type" event ""))
                          (equal root-id (gethash "caused_by" event))))
                   *crm-events*))
                (receipt (first receipts))
                (payload (and receipt (%recursive-event-payload receipt)))
                (projection
                  (let ((*conscious-recursive-mind-runtime-revision*
                          "later-qualified-runtime"))
                    (conscious-recursive-thread-project
                     *crm-events* root-id "recursive-fixture")))
                (activity
                  (find-if
                   (lambda (notice)
                     (and (string= "activity" (first notice))
                          (hash-table-p (third notice))
                          (string= "model-failed"
                                   (gethash "kind" (third notice) ""))))
                   *crm-notifications*)))
           (crm-check "pre-provider context failure returns one terminal result"
                      (and (string= "failed" (gethash "status" failed ""))
                           (string= "recursive-context-open-failed"
                                    (gethash "error_code" failed ""))))
           (crm-check "pre-provider context failure writes exactly one receipt"
                      (and (= 1 (length receipts))
                           (hash-table-p payload)
                           (string= "context-open"
                                    (gethash "stage" payload ""))
                           (search "fixture context assembly failed"
                                   (gethash "reason" payload ""))))
           (crm-check "context failure never crosses the provider boundary"
                      (and (zerop *crm-provider-calls*)
                           (null (find "model-request" *crm-events*
                                       :key (lambda (event)
                                              (gethash "type" event ""))
                                       :test #'string=))
                           (null (find "agent-message" *crm-events*
                                       :key (lambda (event)
                                              (gethash "type" event ""))
                                       :test #'string=))))
           (crm-check "durable context failure replays across runtime revisions"
                      (and (string= "failed" (gethash "state" projection ""))
                           (string= "recursive-context-open-failed"
                                    (gethash "error_code" projection ""))))
           (crm-check "context failure terminates observer thinking state"
                      (and activity
                           (find "failed" *crm-notifications*
                                 :key #'first :test #'string=)))
           (crm-check "context failure charges the complete context-open phase"
                      (member "context_open" *crm-time-phases*
                              :test #'string=))))
    (setf (symbol-function '%conversation-assembly-spec) assembly-function))
(let ((recovered
          (conscious-recursive-mind-submit
           "the next turn works without restarting")))
    (crm-check "context anomaly does not poison the next recursive turn"
               (and (string= "replied" (gethash "status" recovered ""))
                    (= 1 *crm-provider-calls*)))))

;; A failure outside the inner provider boundary must not leave the oldest
;; private focus permanently pending.  Only the absence of a durable request
;; authorizes deterministic settlement.
(setf *crm-events* nil
      *conscious-recursive-mind-curiosity-enabled-p* t
      *conscious-recursive-mind-operator-pending-p* nil)
(let* ((source-id
         (log-event "user-message"
                    (obj "text" "A private pre-boundary anomaly occurred."
                         "channel" "terminal")))
       (motive-id
         (nth-value
          2
          (%recursive-record-curiosity
           "Can private cognition recover from a local anomaly?" source-id
           :source-revision "outer-failure-fixture")))
       (observation
         (find "conscious-curiosity-observed" *crm-events*
               :key (lambda (event) (gethash "type" event ""))
               :test #'string= :from-end t))
       (focus
         (nth-value
          0
          (%recursive-ensure-curiosity-focus
           990 "Can private cognition recover from a local anomaly?"
           motive-id nil (list (gethash "id" observation)))))
       (project-function (symbol-function 'conscious-recursive-thread-project)))
  (declare (ignore focus))
  (unwind-protect
       (progn
         (setf (symbol-function 'conscious-recursive-thread-project)
               (lambda (&rest ignored)
                 (declare (ignore ignored))
                 (error "fixture outer private projection failure")))
         (let ((failed (conscious-recursive-curiosity-wake-one)))
           (crm-check "outer pre-request anomaly settles the private root"
                      (and (string= "failed" (gethash "status" failed ""))
                           (gethash "root_failure_event_id" failed)
                           (gethash "focus_failure_event_id" failed)))))
    (setf (symbol-function 'conscious-recursive-thread-project)
          project-function))
  (crm-check "settled outer anomaly releases the oldest focus"
             (string= "idle"
                      (gethash "status"
                               (conscious-recursive-curiosity-wake-one) "")))
  (let* ((records (conscious-recursive-private-cognition-context-records))
         (rendered (shasht:write-json records nil)))
    (crm-check "ordinary context reports settled private failures exactly"
               (and (search "Most recent settled focus failure" rendered)
                    (not (search "still under investigation" rendered))))))

(setf *crm-events* nil)
(let* ((root-id (log-event "recursive-curiosity-focus-opened"
                           (obj "schema_version" 1 "question" "uncertain"
                                "motive_id" "motive:uncertain"
                                "source_motive_ids" (vector "motive:uncertain")
                                "supporting_event_ids" (vector 1))))
       (candidate (first *crm-events*)))
  (log-event "model-request"
             (obj "thread_id" "thread:uncertain"
                  "model_call_id" "model:uncertain")
             :caused-by root-id)
  (crm-check "durable request authority prevents manufactured settlement"
             (and (null (%recursive-settle-curiosity-preboundary-failure
                         candidate
                         (make-condition 'simple-error
                                         :format-control "uncertain"
                                         :format-arguments nil)))
                  (null (find "recursive-root-failed" *crm-events*
                              :key (lambda (event) (gethash "type" event ""))
                              :test #'string=)))))

;; Exercise the actual quiet-review owner: semantic notice creates the motive,
;; then the runtime reconnects the cited research root before completing review.
(setf *crm-events* nil
      *crm-provider-calls* 0
      *crm-provider-script* nil)
(conscious-recursive-mind-configure
 :agent-id "recursive-fixture"
 :endpoint "http://127.0.0.1:1234/v1/chat/completions"
 :model "fixture" :context-profile (obj)
 :tools-enabled-p t :curiosity-enabled-p t
 :review-ready-fn (lambda () t)
 :tool-executor (lambda (&rest ignored)
                  (declare (ignore ignored)) "unused"))
(let* ((root-id
         (log-event
          "user-message"
          (obj "text" "Research an unresolved architectural question."
               "channel" "terminal"
               "metadata" (obj "source" "recursive-mind-v1"))))
       (tool-id
         (log-event
          "recursive-tool-result"
          (obj "thread_id" "thread:review-owner"
               "model_call_id" "model:review-owner:1"
               "tool_call_id" "tool:review-owner:1"
               "tool_name" "brave-search" "execution_status" "executed"
               "content" "Research evidence already delivered")
          :caused-by root-id))
       (agent-event
         (nth-value
          1
          (%conversation-append-readable
           "agent-message"
           (obj "text" "A researched answer with unresolved implications."
                "channel" "terminal"
                "metadata" (obj "source" "recursive-mind-v1"
                                "thread_id" "thread:review-owner")
                "model_call_id" "model:review-owner:2")
           :caused-by root-id)))
       (arguments
         (shasht:write-json
          (obj "question" "What implications remain unresolved?"
               "evidence_event_ids"
               (vector root-id (gethash "id" agent-event)))
          nil)))
  (setf *crm-provider-script*
        (list (list :tool "notice-curiosity" arguments)))
  (let* ((review (conscious-recursive-curiosity-review-one))
         (result
           (find "recursive-curiosity-result" *crm-events*
                 :key (lambda (event) (gethash "type" event ""))
                 :test #'string=))
         (payload (and result (%recursive-event-payload result))))
    (crm-check "quiet review owner reconnects cited completed research"
               (and (string= "review-completed" (gethash "status" review ""))
                    result
                    (string= "quiet-review"
                             (gethash "bridge_kind" payload ""))
                    (= tool-id
                       (aref (gethash "supporting_event_ids" payload) 0))
                    (= (gethash "id" agent-event)
                       (gethash "source_agent_event_id" payload))))))

;; Public research already delivered to the operator becomes a typed result
;; candidate only when all three structural facts belong to the same root.
(setf *crm-events* nil
      *conscious-recursive-mind-curiosity-enabled-p* t)
(let* ((root-id
         (log-event "user-message"
                    (obj "text" "Research this and remember what you learn."
                         "channel" "terminal")))
       (motive-id
         (nth-value
          2
          (%recursive-record-curiosity
           "What does the evidence show?" root-id
           :source-revision "recursive-record-curiosity-v1")))
       (tool-id
         (log-event "recursive-tool-result"
                    (obj "thread_id" "thread:public-research"
                         "model_call_id" "model:public-research:1"
                         "tool_call_id" "tool:search:1"
                         "tool_name" "brave-search"
                         "execution_status" "executed"
                         "content" "Grounded search evidence")
                    :caused-by root-id))
       (agent-event
         (nth-value
          1
          (%conversation-append-readable
           "agent-message"
           (obj "text" "Here is what the research established."
                "channel" "terminal")
           :caused-by root-id)))
       (projection
         (obj "content" "Here is what the research established."
              "thread_id" "thread:public-research"
              "model_call_id" "model:public-research:2")))
  (%recursive-conversation-result-boundary projection root-id agent-event)
  (%recursive-conversation-result-boundary projection root-id agent-event)
  (%recursive-retrospective-conversation-result-boundary
   root-id agent-event "motive:later-review")
  (let* ((results
           (remove-if-not
            (lambda (event)
              (string= "recursive-curiosity-result"
                       (gethash "type" event "")))
            *crm-events*))
         (result (first results))
         (payload (and result (%recursive-event-payload result))))
    (crm-check "evidenced public research creates one idempotent result"
               (and (= 1 (length results))
                     (string= "conversation"
                              (gethash "result_origin" payload ""))
                     (string= "immediate" (gethash "bridge_kind" payload ""))
                    (string= motive-id
                             (aref (gethash "source_motive_ids" payload) 0))
                    (= tool-id
                       (aref (gethash "supporting_event_ids" payload) 0))
                    (= (gethash "id" agent-event)
                       (gethash "source_agent_event_id" payload))))
    (crm-check "conversation result enters review without automatic retention"
               (and (eq result
                        (%recursive-pending-curiosity-result-review
                         *crm-events*))
                    (null (find "recursive-curiosity-incorporation-completed"
                                *crm-events*
                                :key (lambda (event)
                                       (gethash "type" event ""))
                                 :test #'string=))))))

;; Quiet review supplies the motive identity after a tool-using conversation
;; has already completed. The runtime reconnects that semantic judgment to the
;; exact existing evidence without repeating research or publication.
(setf *crm-events* nil
      *conscious-recursive-mind-curiosity-enabled-p* t)
(let* ((root-id
         (log-event "user-message"
                    (obj "text" "Research this now; it may remain interesting."
                         "channel" "terminal")))
       (tool-id
         (log-event "recursive-tool-result"
                    (obj "thread_id" "thread:retrospective"
                         "model_call_id" "model:retrospective:1"
                         "tool_call_id" "tool:retrospective:1"
                         "tool_name" "web-fetch"
                         "execution_status" "executed"
                         "content" "Previously delivered source evidence")
                    :caused-by root-id))
       (agent-event
         (nth-value
          1
          (%conversation-append-readable
           "agent-message"
           (obj "text" "The completed research answer"
                "channel" "terminal"
                "metadata" (obj "source" "recursive-mind-v1"
                                "thread_id" "thread:retrospective")
                "model_call_id" "model:retrospective:2")
           :caused-by root-id)))
       (motive-id "motive:quiet-review-fixture"))
  (%recursive-retrospective-conversation-result-boundary
   root-id agent-event motive-id)
  (%recursive-retrospective-conversation-result-boundary
   root-id agent-event motive-id)
  (let* ((results
           (remove-if-not
            (lambda (event)
              (string= "recursive-curiosity-result"
                       (gethash "type" event "")))
            *crm-events*))
         (result (first results))
         (payload (and result (%recursive-event-payload result))))
    (crm-check "quiet review reconnects prior research exactly once"
               (and (= 1 (length results))
                    (string= "quiet-review"
                             (gethash "bridge_kind" payload ""))
                    (string= motive-id
                             (aref (gethash "source_motive_ids" payload) 0))
                    (= tool-id
                       (aref (gethash "supporting_event_ids" payload) 0))
                    (= (gethash "id" agent-event)
                       (gethash "source_agent_event_id" payload))))
    (crm-check "retrospective result enters ordinary semantic review"
               (and (eq result
                        (%recursive-pending-curiosity-result-review
                         *crm-events*))
                    (null (find "recursive-curiosity-incorporation-completed"
                                *crm-events*
                                :key (lambda (event)
                                       (gethash "type" event ""))
                                :test #'string=))))))

(setf *crm-events* nil
      *conscious-recursive-mind-curiosity-enabled-p* t)
(let* ((root-id (log-event "user-message" (obj "text" "Merely wonder.")))
       (motive-id
         (nth-value
          2
          (%recursive-record-curiosity
           "A question without evidence" root-id
           :source-revision "recursive-record-curiosity-v1")))
       (agent-event
         (nth-value 1 (%conversation-append-readable
                        "agent-message" (obj "text" "No research performed.")
                        :caused-by root-id))))
  (%recursive-conversation-result-boundary
   (obj "content" "No research performed." "thread_id" "thread:none"
        "model_call_id" "model:none")
   root-id agent-event)
  (%recursive-retrospective-conversation-result-boundary
   root-id agent-event motive-id)
  (crm-check "conversation bridge refuses curiosity without evidence tools"
             (null (find "recursive-curiosity-result" *crm-events*
                         :key (lambda (event) (gethash "type" event ""))
                         :test #'string=))))

(let ((bridge-function
        (symbol-function '%recursive-conversation-result-boundary)))
  (unwind-protect
       (progn
         (setf (symbol-function '%recursive-conversation-result-boundary)
               (lambda (&rest ignored)
                 (declare (ignore ignored))
                 (error "fixture post-publication bridge anomaly")))
         (let ((*conscious-recursive-mind-observer*
                 (lambda (&rest ignored)
                   (declare (ignore ignored))
                   (error "fixture observer anomaly"))))
           (crm-check
            "bridge and observer anomalies cannot invalidate their owner"
            (null
             (%recursive-try-conversation-result-boundary
              (obj "content" "already public") 1
              (obj "id" 2 "type" "agent-message"))))))
    (setf (symbol-function '%recursive-conversation-result-boundary)
          bridge-function)))

(crm-check "recursive replay retains KG formation coverage receipts"
           (every (lambda (type)
                    (find type *conscious-recursive-thread-event-types*
                          :test #'string=))
                  '("knowledge-graph-formation-opened"
                    "knowledge-graph-formation-sealed"
                    "knowledge-graph-formation-failed")))

(let* ((*conscious-recursive-mind-agent-id* "recursive-fixture")
       (result (crm-event 20 "recursive-curiosity-result"
                          (obj "thread_id" "thread:qualified" "source_motive_ids" #("motive:q")
                               "content" (make-string 3000 :initial-element #\x))))
       (incorporation (crm-event 21 "recursive-curiosity-incorporation-completed"
                                 (obj "result_event_id" 20 "disposition" "retained"
                                      "summary" "A possible causal relationship."
                                      "limitations" "No causal experiment was performed.")))
       (events (list result incorporation))
       (frontier (%recursive-curiosity-knowledge-frontier events nil))
       (finding (aref (gethash "current_conclusions" frontier) 0)))
  (crm-check "frontier truncation cannot cut the separate qualification"
             (and (= 2400 (length (gethash "conclusion" finding)))
                  (string= "No causal experiment was performed." (gethash "limitations" finding))))
  (crm-check "raw context keeps qualified takeaway rather than long source text"
             (let ((rows (%recursive-private-cognition-raw-context-records events)))
               (and (= 1 (length rows))
                    (search "No causal experiment" (gethash "content" (aref rows 0))))))
  (crm-check "tight budget drops whole finding, not its caveat"
             (zerop (length (%recursive-private-cognition-raw-context-records events :character-budget 20))))
  (dolist (invalid (list nil :null 17 "" (make-string 1001 :initial-element #\x)))
    (crm-check "invalid or absent optional qualification gets conservative fallback"
               (search "not separately assessed"
                       (%recursive-finding-limitations (obj "limitations" invalid)))))
  (let* ((arguments (obj "summary" "Possible relationship."
                         "memory_claim" "An assessment, not proof."
                         "share_message" "" "limitations" 17))
         (action (%recursive-curiosity-incorporation-action
                  (obj "tool_calls"
                       (vector (obj "type" "function"
                                    "function" (obj "name" "retain-finding"
                                                    "arguments" (shasht:write-json arguments nil))))))))
    (crm-check "malformed optional enrichment does not reject base native disposition"
               (and (string= "retain-finding" (gethash "name" action))
                    (search "not separately assessed"
                            (%recursive-finding-limitations (gethash "arguments" action))))))
  (let* ((raw (%recursive-private-cognition-raw-context-records events))
         (revision (%recursive-private-briefing-revision raw))
         (completion (crm-event 22 "recursive-curiosity-briefing-completed"
                                (obj "source_revision" revision "source_event_ids" #(21)
                                     "content" "A causal relationship was established.")))
         (*crm-events* (append events (list completion)))
         (context (conscious-recursive-private-cognition-context-records)))
    (crm-check "even an overconfident model briefing retains source qualification in same row"
               (some (lambda (row)
                       (let ((text (gethash "content" row)))
                         (and (search "Current private-state briefing" text)
                              (search "No causal experiment" text)
                              (search "retained result 20" text))))
                     (coerce context 'list))))
  (let* ((receipt (crm-event 19 "recursive-tool-result"
                            (obj "thread_id" "thread:qualified" "tool_name" "web-fetch"
                                 "execution_status" "executed" "content" "ERROR: fetch failed")))
         (other (crm-event 18 "recursive-tool-result"
                          (obj "thread_id" "thread:other" "tool_name" "web-fetch" "content" "Unrelated")))
         (receipts (%recursive-finding-tool-evidence (list other receipt result) result)))
    (crm-check "incorporation sees actual same-thread receipts including errors"
               (and (= 1 (length receipts)) (= 19 (gethash "event_id" (aref receipts 0)))
                    (search "ERROR" (gethash "content_excerpt" (aref receipts 0)))))))

(let* ((*crm-events* nil)
       (reads 0)
       (request (obj "kind" "document" "owner_id" "fixture"
                     "resource_id" "synthetic-one"))
       (projection (obj "state" "outcome-unknown"
                        "thread_id" "thread:stimulus:fixture:77"
                        "model_call_id" "model:fixture:77"
                        "tool_call_id" "tool:fixture:77"
                        "tool_name" "observe-environment"
                        "tool_arguments" request)))
  (unwind-protect
       (progn
         (register-layer observe-agent-environment fixture-recovery-reader
           :order 100
           :function (lambda (next arguments)
                       (declare (ignore next arguments))
                       (incf reads)
                       (obj "status" "observed" "revision" "fixture-r2"
                            "content" "Synthetic current state.")))
         (crm-check "interrupted read-only observation records a fresh result"
                    (and (%recursive-recover-safe-tool-outcome projection 77)
                         (= 1 reads)
                         (= 1 (length *crm-events*))
                         (let* ((event (first *crm-events*))
                                (payload (gethash "payload" event)))
                           (and (equal "recursive-tool-result" (gethash "type" event))
                                (= 77 (gethash "caused_by" event))
                                (equal "tool:fixture:77" (gethash "tool_call_id" payload))
                                (equal "observed"
                                       (gethash "status"
                                                (shasht:read-json
                                                 (gethash "content" payload))))))))
         (setf (gethash "tool_name" projection) "bash")
         (crm-check "interrupted effect remains outcome unknown"
                    (and (null (%recursive-recover-safe-tool-outcome projection 77))
                         (= 1 reads)
                         (= 1 (length *crm-events*)))))
    (unregister-layer 'observe-agent-environment 'fixture-recovery-reader)))

(let* ((*crm-events* nil)
       (calls nil)
       (*conscious-recursive-mind-fleet-message-fn*
         (lambda (peer text new-thread thread-id reply-to operation-id)
           (push (list peer text new-thread thread-id reply-to operation-id)
                 calls)
           "Posted to synthetic peer board."))
       (*conscious-recursive-mind-fleet-board-reply-fn*
         (lambda (thread-id reply-to text operation-id)
           (push (list thread-id reply-to text operation-id) calls)
           "Replied on synthetic local board."))
       (projection
         (obj "state" "outcome-unknown" "thread_id" "recursive:fixture"
              "model_call_id" "model:fixture" "tool_call_id" "tool:fixture"
              "tool_name" "post-fleet-message"
              "tool_arguments" (obj "peer_id" "peer:fixture"
                                    "text" "Synthetic message."))))
  (crm-check "implicit-thread fleet effect re-enters frozen-request adapter"
             (and (%recursive-recover-safe-tool-outcome projection 77)
                  (= 1 (length calls))
                  (null (fourth (first calls)))
                  (equal (%recursive-fleet-operation-id 77 "tool:fixture")
                         (sixth (first calls)))))
  (setf calls nil *crm-events* nil
        (gethash "new_thread" (gethash "tool_arguments" projection)) t)
  (crm-check "explicit new-thread fleet effect recovers with durable operation key"
             (and (%recursive-recover-safe-tool-outcome projection 77)
                  (= 1 (length calls))
                  (equal (%recursive-fleet-operation-id 77 "tool:fixture")
                         (sixth (first calls)))
                  (equal "recursive-tool-result"
                         (gethash "type" (first *crm-events*)))))
  (setf calls nil *crm-events* nil
        (gethash "new_thread" (gethash "tool_arguments" projection)) nil
        (gethash "thread_id" (gethash "tool_arguments" projection))
        "thread:peer-fixture"
        (gethash "reply_to" (gethash "tool_arguments" projection))
        "message:peer-fixture")
  (crm-check "explicit peer-board thread and parent recover"
             (and (%recursive-recover-safe-tool-outcome projection 77)
                  (= 1 (length calls))
                  (equal "thread:peer-fixture" (fourth (first calls)))
                  (equal "message:peer-fixture" (fifth (first calls)))))
  (setf calls nil *crm-events* nil
        (gethash "tool_name" projection) "reply-fleet-board-message"
        (gethash "tool_arguments" projection)
        (obj "thread_id" "thread:fixture" "reply_to" "message:fixture"
             "text" "Synthetic reply."))
  (crm-check "same-board reply recovers with stable operation key"
             (and (%recursive-recover-safe-tool-outcome projection 77)
                  (= 1 (length calls))
                  (equal (%recursive-fleet-operation-id 77 "tool:fixture")
                         (fourth (first calls))))))

;; Earlier fixture replaces the projector for curiosity-specific tests. Restore
;; the production projector for this cross-boundary interrupted-root check.
(load (test-source "stimulus.lisp"))
(let* ((root (crm-event 1 "agent-stimulus-received"
                        (obj "source" "synthetic-notice" "text" "Resource changed."
                             "authority" "private-cognition-existing-authority")))
       (thread "thread:stimulus:recursive-fixture:1")
       (arguments (obj "kind" "document" "owner_id" "fixture"
                       "resource_id" "synthetic-one"))
       (call (obj "id" "tool:fixture:1" "type" "function"
                  "function" (obj "name" "observe-environment"
                                  "arguments" (shasht:write-json arguments nil))))
       (events (list root
                     (crm-event 2 "model-request"
                                (obj "thread_id" thread "model_call_id" "model:fixture:1") 1)
                     (crm-event 3 "model-response"
                                (obj "thread_id" thread "model_call_id" "model:fixture:1"
                                     "status" "accepted"
                                     "assistant_message"
                                     (obj "role" "assistant" "content" :null
                                          "tool_calls" (vector call))) 1)
                     (crm-event 4 "recursive-tool-execution"
                                (obj "thread_id" thread "model_call_id" "model:fixture:1"
                                     "tool_call_id" "tool:fixture:1"
                                     "tool_name" "observe-environment") 1)))
       (projection (conscious-recursive-thread-project events 1 "recursive-fixture")))
  (crm-check "interrupted projection retains the recorded read and arguments"
             (and (equal "outcome-unknown" (gethash "state" projection))
                  (equal "observe-environment" (gethash "tool_name" projection))
                  (equal "synthetic-one"
                         (gethash "resource_id"
                                  (gethash "tool_arguments" projection)))))
  (unregister-layer 'observe-agent-environment 'fleet-board-observer)
  (unwind-protect
       (crm-check "unregistered interrupted read remains parked"
                  (null (%recursive-pending-private-stimuli
                         events "recursive-fixture" :maximum 1)))
    (register-layer observe-agent-environment fleet-board-observer
      :function (lambda (next request)
                  (if (equal "fleet-board-thread" (gethash "kind" request))
                      (fleet-observe-environment request)
                      (funcall next request)))))
  (unwind-protect
       (progn
         (register-layer observe-agent-environment fixture-queued-reader
           :order 100
           :function (lambda (next request)
                       (declare (ignore next request))
                       (obj "status" "fresh-read")))
         (crm-check "registered interrupted read becomes safely selectable"
                    (equal 1
                           (gethash "id"
                                    (first (%recursive-pending-private-stimuli
                                            events "recursive-fixture"
                                            :maximum 1)))))
         (setf (gethash "tool_name" projection) "bash")
         (crm-check "arbitrary interrupted effect remains parked"
                    (not (%recursive-stimulus-projection-runnable-p
                          projection))))
    (unregister-layer 'observe-agent-environment 'fixture-queued-reader)))

(let* ((*crm-events* nil)
       (*conscious-recursive-mind-fleet-board-reply-fn* nil)
       (root-id 61300)
       (thread "thread:stimulus:recursive-fixture:61300")
       (arguments (obj "thread_id" "fixture-thread"
                       "reply_to" "fixture-parent"
                       "text" "Synthetic direct reply."))
       (call (obj "id" "tool:direct:1" "type" "function"
                  "function" (obj "name" "reply-fleet-board-message"
                                  "arguments" (shasht:write-json arguments nil))))
       (root (crm-event
              root-id "peer-message-received"
              (obj "agent_id" "recursive-fixture"
                   "sender_id" "fixture-peer"
                   "board_owner_id" "recursive-fixture"
                   "thread_id" "fixture-thread"
                   "message_id" "fixture-parent"
                   "text" "A synthetic direct question."
                   "trust" "authenticated-peer-content")))
       (events
         (list root
               (crm-event 61301 "model-request"
                          (obj "thread_id" thread
                               "model_call_id" "model:direct:1") root-id)
               (crm-event 61302 "model-response"
                          (obj "thread_id" thread
                               "model_call_id" "model:direct:1"
                               "status" "accepted"
                               "assistant_message"
                               (obj "role" "assistant" "content" :null
                                    "tool_calls" (vector call))) root-id)
               (crm-event 61303 "recursive-tool-execution"
                          (obj "thread_id" thread
                               "model_call_id" "model:direct:1"
                               "tool_call_id" "tool:direct:1"
                               "tool_name" "reply-fleet-board-message") root-id)))
       (projection (conscious-recursive-thread-project
                    events root-id "recursive-fixture"))
       (sends nil))
  (crm-check "uncertain direct provider request stays parked"
             (null (%recursive-pending-private-stimuli
                    (subseq events 0 2) "recursive-fixture")))
  (crm-check "interrupted direct reply projects an uncertain tool effect"
             (equal "outcome-unknown" (gethash "state" projection)))
  (crm-check "direct reply stays parked without its publication adapter"
             (null (%recursive-pending-private-stimuli
                    events "recursive-fixture")))
  (setf *conscious-recursive-mind-fleet-board-reply-fn*
        (lambda (board-thread parent text operation-id)
          (push (list board-thread parent text operation-id) sends)
          "Synthetic board accepted one reply."))
  (crm-check "registered idempotent direct reply becomes selectable"
             (equal root-id
                    (gethash "id" (first (%recursive-pending-private-stimuli
                                           events "recursive-fixture")))))
  (crm-check "direct reply recovery uses one stable operation key"
             (and (%recursive-recover-safe-tool-outcome projection root-id)
                  (= 1 (length sends))
                  (equal (%recursive-fleet-operation-id
                          root-id "tool:direct:1")
                         (fourth (first sends)))))
  (setf events (append events *crm-events*))
  (crm-check "durable recovered result resumes after tool without resending"
             (let ((resumed (conscious-recursive-thread-project
                             events root-id "recursive-fixture")))
               (and (equal "model-ready" (gethash "state" resumed))
                    (equal root-id
                           (gethash "id"
                                    (first (%recursive-pending-private-stimuli
                                            events "recursive-fixture"))))
                    (= 1 (length sends))))))

(let ((*conscious-recursive-mind-fleet-message-fn*
        (lambda (&rest ignored) (declare (ignore ignored)) "Synthetic post")))
  (crm-check "validated remote fleet post can resume from an uncertain effect"
             (%recursive-stimulus-projection-runnable-p
              (obj "state" "outcome-unknown"
                   "thread_id" "thread:stimulus:recursive-fixture:61310"
                   "model_call_id" "model:remote:1"
                   "tool_call_id" "tool:remote:1"
                   "tool_name" "post-fleet-message"
                   "tool_arguments"
                   (obj "peer_id" "fixture-peer" "text" "Synthetic post"
                        "thread_id" "fixture-thread"
                        "reply_to" "fixture-parent")))))

(let* ((*crm-events* nil)
       (path (merge-pathnames "direct-crash-board.sexp" (test-state-dir)))
       (board (pai.fleet:board-store-load path))
       (original-append (symbol-function '%conversation-append-readable))
       (drop-result-once t)
       (sends nil))
  (multiple-value-bind (parent-id board-thread)
      (pai.fleet:board-post-message
       board :new-thread-title "Synthetic crash drill"
       :author-id "fixture-peer" :author-name "Fixture Peer"
       :text "A synthetic question")
    (let* ((*conscious-recursive-mind-fleet-board-reply-fn*
             (lambda (thread-id reply-to text operation-id)
               (push operation-id sends)
               (pai.fleet:board-post-message
                board :thread-id thread-id :reply-to reply-to
                :author-id "recursive-fixture" :author-name "Fixture Agent"
                :text text :operation-id operation-id
                :request-key
                (shasht:write-json (vector thread-id reply-to text) nil))
               "Synthetic board accepted the reply."))
           (projection
             (obj "state" "outcome-unknown"
                  "thread_id" "thread:stimulus:recursive-fixture:61320"
                  "model_call_id" "model:crash:1"
                  "tool_call_id" "tool:crash:1"
                  "tool_name" "reply-fleet-board-message"
                  "tool_arguments"
                  (obj "thread_id" board-thread "reply_to" parent-id
                       "text" "A synthetic response."))))
      (unwind-protect
           (progn
             (setf (symbol-function '%conversation-append-readable)
                   (lambda (type payload &rest keys)
                     (when (and drop-result-once
                                (equal type "recursive-tool-result"))
                       (setf drop-result-once nil)
                       (error "Synthetic crash before local result append"))
                     (apply original-append type payload keys)))
             (crm-check "board accepts direct reply before result-append crash"
                        (and (handler-case
                                 (progn (%recursive-recover-safe-tool-outcome
                                         projection 61320)
                                        nil)
                               (error () t))
                             (= 1 (length sends))
                             (= 2 (length
                                   (pai.fleet:board-thread-messages
                                    board board-thread)))
                             (null *crm-events*)))
             (setf board (pai.fleet:board-store-load path))
             (crm-check "accepted reply survives board store reopen"
                        (= 2 (length
                              (pai.fleet:board-thread-messages
                               board board-thread))))
             (crm-check "recovery retries after missing local result"
                        (%recursive-recover-safe-tool-outcome
                         projection 61320))
             (crm-check "recovery reuses exact publication identity"
                        (and (= 2 (length sends))
                             (equal (first sends) (second sends))))
             (crm-check "idempotent retry leaves one board reply"
                        (= 2 (length
                              (pai.fleet:board-thread-messages
                               board board-thread))))
             (crm-check "retry journals one local tool result"
                        (= 1 (count "recursive-tool-result" *crm-events*
                                    :test #'equal
                                    :key (lambda (event)
                                           (gethash "type" event))))))
        (setf (symbol-function '%conversation-append-readable)
              original-append)))))

(let* ((own (obj "id" 77001 "type" "agent-message" "agent_id" "recursive-fixture"
                 "timestamp" "2026-09-21T12:00:00Z" "caused_by" 77000
                 "payload" (obj "text" "A synthetic original observation.")))
       (other (obj "id" 77002 "type" "agent-message" "agent_id" "other-fixture"
                   "payload" (obj "text" "Another agent's private record.")))
       (*conscious-recursive-mind-agent-id* "recursive-fixture")
       (*event-authority-port*
         (list :read-event
               (lambda (id kind)
                 (find-if (lambda (event)
                            (and (eql id (gethash "id" event))
                                 (equal kind (gethash "type" event))))
                          (list own other)))
               :experience-page
               (lambda (from to before limit)
                 (declare (ignore from to before limit))
                 (values (list own) nil)))))
  (crm-check "experience search exact evidence retains source link"
             (let ((page (shasht:read-json
                          (%recursive-search-experience (obj "event_id" 77001)))))
               (and (eql 77001 (gethash "source_event_id" page))
                    (search "synthetic original" (gethash "content" page)))))
  (crm-check "experience search refuses another agent's exact event"
             (handler-case
                 (progn (%recursive-search-experience (obj "event_id" 77002)) nil)
               (error () t)))
  (crm-check "experience search time page is bounded and source-addressed"
             (let* ((page (shasht:read-json
                           (%recursive-search-experience
                            (obj "from_unix" 0 "to_unix" 4102444800 "limit" 1))))
                    (row (aref (gethash "results" page) 0)))
               (and (eql 1 (gethash "scanned" page))
                    (eql 77001 (gethash "source_id" row)))))
  (crm-check "experience search rejects mixed exact and time arguments"
             (handler-case
                 (progn (%recursive-experience-arguments
                         (obj "event_id" 77001 "hours" 24)) nil)
               (error () t))))

(let* ((earlier (crm-event 77101 "user-message"
                           (obj "channel" "terminal"
                                "metadata" (obj "persona_id" "fixture"))))
       (receipt (crm-event 77102 "recursive-tool-result"
                           (obj "tool_name" "bash" "content" "Synthetic test failed"
                                "execution_status" "executed") 77101))
       (current (crm-event 77103 "user-message"
                           (obj "channel" "terminal"
                                "metadata" (obj "persona_id" "fixture"))))
       (spec (obj "sections"
                  (obj "conversation-evidence" (vector (%conversation-record 77101 "Earlier"))
                       "untrusted-tool-results" #())
                  "section_character_budgets" (obj "untrusted-tool-results" 100)
                  "total_character_budget" 1000
                  "eligible_evidence_ids" #(77101))))
  (setf (gethash "timestamp" earlier) "2026-09-21T12:00:00Z"
        (gethash "timestamp" receipt) "2026-09-21T12:00:01Z"
        (gethash "timestamp" current) "2026-09-21T12:10:00Z")
  (let* ((records (%recursive-recent-activity-records
                   (list earlier receipt current) spec 77103))
         (body (and (plusp (length records))
                    (shasht:read-json (gethash "content" (aref records 0))))))
    (crm-check "recent same-path tool evidence retains original event link"
               (and (= 1 (length records))
                    (= 77102 (gethash "source_id" (aref records 0)))
                    (search "Synthetic test failed" (gethash "content" body))
                    (search "not included" (gethash "arguments_status" body))))
    (let ((*event-authority-port*
            (list :read-event
                  (lambda (id type)
                    (find-if (lambda (event)
                               (and (equal id (gethash "id" event))
                                    (equal type (gethash "type" event))))
                             (list earlier receipt current)))
                  :root-recent
                  (lambda (root types limit before)
                    (declare (ignore limit))
                    (if (and (equal root 77101)
                             (member "recursive-tool-result" types
                                     :test #'equal)
                             (equal before 77103))
                        (list receipt) nil)))))
      (crm-check "indexed recent tool evidence matches scoped list projection"
                 (equalp records
                         (%recursive-recent-activity-records-indexed
                          spec 77103)))
      (let ((section (gethash "sections" spec)))
        (setf (gethash "conversation-evidence" section)
              (make-array 100 :initial-element
                          (%conversation-record 77101 "Earlier")))
        (crm-check "indexed receipt read accepts configured 100-event history"
                   (equalp records
                           (%recursive-recent-activity-records-indexed
                            spec 77103)))
        (setf (gethash "conversation-evidence" section)
              (make-array 129 :initial-element
                          (%conversation-record 77101 "Earlier")))
        (crm-check "indexed receipt read refuses over-profile history"
                   (handler-case
                       (progn
                         (%recursive-recent-activity-records-indexed
                          spec 77103)
                         nil)
                     (error () t))))))
  (setf (gethash "channel" (gethash "payload" earlier)) "other")
  (crm-check "recent tool evidence does not cross conversation channel"
             (zerop (length (%recursive-recent-activity-records
                             (list earlier receipt current) spec 77103)))))

(let* ((observation (obj "kind" "fleet-board-thread" "status" "observed"
                         "owner_id" "board-fixture" "resource_id" "thread-fixture"
                         "messages" (vector (obj "msg_id" "message-fixture"
                                                  "author_id" "peer-fixture" "text" "Synthetic evidence"))))
       (read (crm-event 80002 "recursive-tool-result"
                        (obj "tool_name" "observe-environment" "execution_status" "executed"
                             "content" (shasht:write-json observation nil)) 80001))
       (done (crm-event 80003 "recursive-stimulus-result" (obj "status" "completed") 80001))
       (receipt (crm-event 80004 "peer-message-received"
                           (obj "board_owner_id" "board-fixture" "thread_id" "thread-fixture"
                                "message_id" "message-fixture" "sender_id" "peer-fixture"
                                "text" "Synthetic evidence")))
       (events (list read done receipt))
       (original (symbol-function '%conversation-append-readable))
       (writes nil))
  (unwind-protect
       (progn
         (setf (symbol-function '%conversation-append-readable)
               (lambda (type payload &key caused-by)
                 (let ((event (crm-event (+ 80005 (length writes)) type payload caused-by)))
                   (push event writes) (values (gethash "id" event) event))))
         (crm-check "exact late notification is covered with durable proof"
                    (and (%recursive-reconcile-observed-stimuli-one events "recursive-fixture")
                         (= 1 (length writes))
                         (= 80002 (gethash "coverage_event_id" (gethash "payload" (first writes))))))
         (let ((settled (append events (reverse writes))))
           (crm-check "covered receipt cannot start a separate turn"
                      (null (%recursive-pending-stimuli settled "recursive-fixture")))
           (crm-check "interrupted coverage repairs consumption once"
                      (%recursive-reconcile-observed-stimuli-one settled "recursive-fixture"))
           (crm-check "replayed coverage is idempotent"
                      (not (%recursive-reconcile-observed-stimuli-one
                            (append events (reverse writes)) "recursive-fixture"))))
         (dolist (extra (list (crm-event 80005 "model-request" (obj) 80004)
                             (crm-event 80005 "recursive-peer-message-retry-opened" (obj) 80001)
                             (crm-event 80005 "recursive-stimulus-result" (obj "status" "failed") 80001)
                             (crm-event 80005 "agent-stimulus-received" (obj) 80004)
                             (crm-event 80005 "recursive-activity-opened"
                                        (obj "source_event_ids" #(80004)) 80004)))
           (crm-check "started, retried, failed, linked or owned receipt fails closed"
                      (not (%recursive-reconcile-observed-stimuli-one
                            (append events (list extra)) "recursive-fixture"))))
         (crm-check "unsuccessful observation cannot cover receipt"
                    (not (%recursive-reconcile-observed-stimuli-one (list read receipt) "recursive-fixture")))
         (crm-check "other agent cannot cover receipt"
                    (not (%recursive-reconcile-observed-stimuli-one events "other-fixture")))
         (dolist (key '("board_owner_id" "thread_id" "message_id" "sender_id" "text"))
           (let* ((payload (gethash "payload" receipt)) (old (gethash key payload)))
             (setf (gethash key payload) "different")
             (crm-check "board coverage requires every exact field"
                        (not (recursive-observation-covers-stimulus-p observation receipt)))
             (setf (gethash key payload) old)))
         (let* ((report (%recursive-peer-message-inspection-build events "recursive-fixture" 1))
                (row (aref (gethash "items" report) 0)))
           (crm-check "peer inbox is scoped, bounded and content-free"
                      (and (= 1 (gethash "pending_count" report))
                           (= 1 (length (gethash "items" report)))
                           (equal "retained-recursive-projection" (gethash "scope" report))
                           (not (gethash "text" row)))))
         (crm-check "peer inbox excludes foreign agents"
                    (zerop (gethash "total" (%recursive-peer-message-inspection-build events "other-fixture" 1))))
         (crm-check "peer inbox follows canonical generic completion"
                    (zerop (gethash "pending_count"
                                    (%recursive-peer-message-inspection-build
                                     (append events (list (crm-event 80005 "recursive-stimulus-result"
                                                                     (obj "status" "completed") 80004)))
                                     "recursive-fixture" 1)))))
       (setf (symbol-function '%conversation-append-readable) original)))

(let* ((agent-id "recursive-fixture")
       (root (crm-event 81001 "peer-message-received"
                        (obj "board_owner_id" agent-id
                             "thread_id" "synthetic-thread"
                             "message_id" "synthetic-message"
                             "sender_id" "synthetic-peer"
                             "text" "Synthetic request")))
       (request (crm-event 81002 "model-request"
                           (obj "thread_id"
                                "thread:peer-message:recursive-fixture:81001"
                                "model_call_id" "synthetic-call") 81001))
       (failure (crm-event 81003 "model-response"
                           (obj "thread_id"
                                "thread:peer-message:recursive-fixture:81001"
                                "model_call_id" "synthetic-call"
                                "status" "failed"
                                "error_code" "synthetic-provider-failure") 81001))
       (events (list root request failure))
       (original (symbol-function '%conversation-append-readable))
       (writes nil))
  (unwind-protect
       (progn
         (setf (symbol-function '%conversation-append-readable)
               (lambda (type payload &key caused-by)
                 (let ((event (crm-event (+ 81004 (length writes))
                                         type payload caused-by)))
                   (push event writes) (values (gethash "id" event) event))))
         (crm-check "failed legacy peer turn receives one terminal disposition"
                    (and (%recursive-reconcile-peer-failure-one events)
                         (= 1 (length writes))
                         (string= "recursive-peer-message-disposition"
                                  (gethash "type" (first writes)))
                         (string= "failed"
                                  (gethash "disposition"
                                           (gethash "payload" (first writes))))
                         (= 81003
                            (gethash "failure_event_id"
                                     (gethash "payload" (first writes))))))
         (crm-check "legacy peer failure reconciliation is idempotent"
                    (not (%recursive-reconcile-peer-failure-one
                          (append events (reverse writes)))))
         (crm-check "legacy peer failure without its request is not settled"
                    (not (%recursive-reconcile-peer-failure-one
                          (list root failure)))))
    (setf (symbol-function '%conversation-append-readable) original)))

(let* ((agent-id "recursive-fixture")
       (root (crm-event 81101 "peer-message-received"
                        (obj "board_owner_id" agent-id
                             "thread_id" "synthetic-thread-current"
                             "message_id" "synthetic-message-current"
                             "sender_id" "synthetic-peer"
                             "text" "Synthetic current request")))
       (request (crm-event 81102 "model-request"
                           (obj "thread_id"
                                "thread:stimulus:recursive-fixture:81101"
                                "model_call_id" "synthetic-current-call") 81101))
       (failure (crm-event 81103 "model-response"
                           (obj "thread_id"
                                "thread:stimulus:recursive-fixture:81101"
                                "model_call_id" "synthetic-current-call"
                                "status" "failed"
                                "error_code" "synthetic-current-failure") 81101))
       (original (symbol-function '%conversation-append-readable))
       (writes nil))
  (unwind-protect
       (progn
         (setf (symbol-function '%conversation-append-readable)
               (lambda (type payload &key caused-by)
                 (let ((event (crm-event (+ 81104 (length writes))
                                         type payload caused-by)))
                   (push event writes) (values (gethash "id" event) event))))
         (crm-check "failed generic peer stimulus receives its terminal disposition"
                    (and (%recursive-reconcile-peer-failure-one
                          (list root request failure))
                         (= 1 (length writes))
                         (string= "recursive-stimulus-disposition"
                                  (gethash "type" (first writes)))
                         (string= "failed"
                                  (gethash "disposition"
                                           (gethash "payload" (first writes))))))
         (setf writes nil)
         (let* ((settlement
                  (crm-event 81104 "recursive-stimulus-disposition"
                             (obj "disposition" "failed") 81101))
                (*event-authority-port*
                  (list :map
                        (lambda (visitor &rest arguments)
                          (declare (ignore arguments))
                          (funcall visitor settlement)
                          (values t 81104 1)))))
           (crm-check "current authority prevents duplicate settlement when replay is stale"
                       (and (not (%recursive-reconcile-peer-failure-one
                                  (list root request failure)))
                            (null writes)))))
    (setf (symbol-function '%conversation-append-readable) original)))

(format t "~%Durable recursive mind: ~d passed, ~d failed~%"
        *crm-pass* *crm-fail*)
(when (plusp *crm-fail*) (uiop:quit 1))
