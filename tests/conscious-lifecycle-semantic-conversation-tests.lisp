;;;; conscious-lifecycle-semantic-conversation-tests.lisp -- Q5S final consumer.

(in-package :agent)

(defvar *clsc-passed* 0)
(defvar *clsc-failed* 0)
(defparameter *agent-id* "q5s-conversation-dev")

(defun clsc-check (name condition)
  (if condition
      (progn (incf *clsc-passed*) (format t "PASS ~a~%" name))
      (progn (incf *clsc-failed*) (format t "FAIL ~a~%" name))))

(defun clsc-event (id type payload)
  (obj "schema_version" 1 "id" id "timestamp" (+ 2000 id)
       "type" type "agent_id" *agent-id* "caused_by" :null
       "payload" payload))

(defun clsc-budget ()
  (obj "profile_id" "q5s-fixture" "max_input_characters" 4096
       "history_max_events" 16 "history_character_budget" 4096
       "history_event_character_limit" 1024
       "history_target_estimated_tokens" 1024
       "history_min_recent_events" 8 "total_character_budget" 8192
       "section_character_budgets"
       (obj "identity-instructions" 2048 "sensorium" 1024
            "focus-lifecycles" 2048 "triggering-stimuli" 1024
            "conversation-evidence" 1024 "memory-bundles" 0
            "untrusted-tool-results" 256
            "tools-proposal-schema" 512 "publication-constraints" 512)))

(defun clsc-assembly-context (spec)
  (make-conscious-assembly-context
   :pulse-id "pulse:q5s-final" :purpose "respond" :audience "operator"
   :runtime-revision "conscious-q5-v2" :conscious-state-revision 7
   :clock-identity "fixture-clock"
   :total-character-budget (gethash "total_character_budget" spec)
   :section-character-budgets (gethash "section_character_budgets" spec)
   :sections (gethash "sections" spec)
   :eligible-evidence-ids (gethash "eligible_evidence_ids" spec)
   :available-tools (gethash "available_tools" spec)
   :permitted-proposal-kinds (gethash "permitted_proposal_kinds" spec)
   :publication-constraints (gethash "publication_constraints" spec)
   :remaining-budget (gethash "remaining_budget" spec)
   :pre-render-refusals (gethash "pre_render_refusals" spec)))

(format t "~%== Q5S final conversation consumer ==~%")

(load (test-source "mind/conscious/lifecycle.lisp"))
(load (test-source "lifecycle-runtime.lisp"))
(load (test-source "lifecycle-sources.lisp"))
(load (test-source "lifecycle-semantics.lisp"))
(load (test-source "context-assembly.lisp"))
(load (test-source "recall-selection.lisp"))
(load (test-source "conversation-runtime.lisp"))

(conscious-conversation-set-persona-profile
 "fixture-persona" 1 "A neutral fixture identity." "Clear fixture voice.")

(let* ((source
         (clsc-event
          1 "near-term-intention-created"
          (obj "schema_version" 1 "intention_id" "fixture-intention"
               "receipt_id" "fixture-receipt" "state" "seeded"
               "pass_count" 0 "detail" :null)))
       (open
         (clsc-event
          2 "conscious-lifecycle-transition"
          (conscious-lifecycle-transition-payload
           "near-term:fixture-intention" "open" :request-id "fixture-open"
           :lifecycle-kind "deferred-intention"
           :origin-runtime-revision "near-term-intentions-v1"
           :actor-runtime-revision "conscious-q5-v2" :source-event-id 1
           :checkpoint-ref :null :reason-code "near-term-seeded"
           :occurred-at 2002)))
       (semantic-payload
         (conscious-lifecycle-semantic-payload
          "semantic:fixture-intention" 1 :null
          "near-term:fixture-intention" "deferred-intention"
          "fixture-persona" "topic" "interrupted design discussion"
          (vector "topic:design") "resume at the unresolved architecture choice"
          :result-summary :null :result-receipt-event-id :null
          :source-revision "near-term-intentions-v1"
          :actor-runtime-revision "conscious-q5-v2"
          :disclosure-policy-ref "lifecycle-semantic-disclosure-v1"
          :disclosure-class "private-provider-eligible"
          :confidence "asserted" :staleness "current"
          :supporting-event-ids (vector 1)))
       (semantic
         (clsc-event 3 "conscious-lifecycle-semantic-described"
                     semantic-payload))
       (command
         (clsc-event
          4 "conscious-lifecycle-command-requested"
          (obj "schema_version" 1 "command" "create" "content_persisted" nil
               "malicious_text" "alternate private command route")))
       (profile *conscious-conversation-persona-profile*)
       (user
         (clsc-event
          5 "user-message"
          (obj "text" "What were we going to resume?" "channel" "terminal"
               "metadata"
               (obj "source" "q4.5-conversation"
                    "persona_id" "fixture-persona"
                    "persona_revision" 1
                    "persona_fingerprint" (gethash "fingerprint" profile)))))
       (events (list source open semantic command user)))
  (setf *conscious-lifecycle-runtime-projection*
        (conscious-lifecycle-project events :agent-id *agent-id*)
        *conscious-lifecycle-semantic-runtime-projection*
        (conscious-lifecycle-semantic-project events :agent-id *agent-id*))
  (let* ((spec (%conversation-assembly-spec
                events 5 "What were we going to resume?" *agent-id*
                (clsc-budget) "remote-zdr" "terminal"))
         (focus (gethash "focus-lifecycles" (gethash "sections" spec)))
         (focus-json (shasht:write-json focus nil))
         (assembled
           (conscious-context-assemble
            (obj "state_revision" 7 "composition_hash" "fixture-state")
            (clsc-assembly-context spec)))
         (manifest (gethash "manifest" assembled))
         (manifest-json (shasht:write-json manifest nil)))
    (clsc-check "S1 final provider context contains typed subject and outcome"
                (and (search "interrupted design discussion" focus-json)
                     (search "resume at the unresolved architecture choice"
                             focus-json)))
    (clsc-check "S2 final context excludes producer detail and alternate command text"
                (and (null (search "alternate private command route" focus-json))
                     (null (search "malicious_text" focus-json))))
    (clsc-check "manifest carries descriptor and evidence IDs without semantic text"
                (and (search "semantic:fixture-intention" manifest-json)
                     (find 1 (coerce (gethash "evidence_event_ids" manifest) 'list))
                     (find 3 (coerce (gethash "evidence_event_ids" manifest) 'list))
                     (null (search "interrupted design discussion" manifest-json))))
    (clsc-check "S11 command event cannot masquerade as conversation evidence"
                (null (search "alternate private command route"
                              (shasht:write-json
                               (gethash "conversation-evidence"
                                        (gethash "sections" spec)) nil)))))

  (let* ((local-payload
           (conscious-lifecycle-semantic-payload
            "semantic:fixture-intention" 1 :null
            "near-term:fixture-intention" "deferred-intention"
            "fixture-persona" "topic" "local confidential topic" (vector)
            "keep this on the local provider"
            :result-summary :null :result-receipt-event-id :null
            :source-revision "near-term-intentions-v1"
            :actor-runtime-revision "conscious-q5-v2"
            :disclosure-policy-ref "lifecycle-semantic-disclosure-v1"
            :disclosure-class "local-only" :confidence "asserted"
            :staleness "current" :supporting-event-ids (vector 1)))
         (local-events
           (list source open
                 (clsc-event 3 "conscious-lifecycle-semantic-described"
                             local-payload)
                 user)))
    (setf *conscious-lifecycle-semantic-runtime-projection*
          (conscious-lifecycle-semantic-project
           local-events :agent-id *agent-id*))
    (let* ((spec (%conversation-assembly-spec
                  local-events 5 "What were we going to resume?" *agent-id*
                  (clsc-budget) "remote-zdr" "terminal"))
           (rendered (shasht:write-json spec nil))
           (refusals (gethash "pre_render_refusals" spec))
           (assembled
             (conscious-context-assemble
              (obj "state_revision" 7 "composition_hash" "fixture-state")
              (clsc-assembly-context spec)))
           (manifest (gethash "manifest" assembled)))
      (clsc-check "S4 remote route omits local-only semantics"
                  (and (null (search "local confidential topic" rendered))
                       (= 1 (length refusals))
                       (string= "disclosure-policy-refused"
                                (gethash "reason" (aref refusals 0)))))
      (clsc-check "S4 refusal is preserved in the content-free manifest"
                  (and (= 1 (length (gethash "pre_render_refusals" manifest)))
                       (null (search "local confidential topic"
                                     (shasht:write-json manifest nil)))))))

  ;; S8 is asserted at the actual Q4 budget consumer: two deterministic rows,
  ;; a section budget that admits exactly the first, and identical replay.
  (let* ((provenance-a
           (obj "descriptor_id" "semantic:a" "descriptor_event_id" 10
                "evidence_event_ids" (vector 10)))
         (provenance-b
           (obj "descriptor_id" "semantic:b" "descriptor_event_id" 11
                "evidence_event_ids" (vector 11)))
         (sections
           (obj "identity-instructions" (vector) "sensorium" (vector)
                "focus-lifecycles"
                (vector (obj "source_id" 10 "content" "first semantic row"
                             "provenance" provenance-a)
                        (obj "source_id" 11 "content" "second semantic row"
                             "provenance" provenance-b))
                "triggering-stimuli" (vector) "conversation-evidence" (vector)
                "memory-bundles" (vector)
                "untrusted-tool-results" (vector)
                "tools-proposal-schema" (vector)
                "publication-constraints" (vector)))
         (budgets
           (obj "identity-instructions" 0 "sensorium" 0
                "focus-lifecycles" 20 "triggering-stimuli" 0
                "conversation-evidence" 0 "memory-bundles" 0
                "untrusted-tool-results" 0
                "tools-proposal-schema" 0 "publication-constraints" 0))
         (context
           (make-conscious-assembly-context
            :pulse-id "pulse:q5s-budget" :purpose "respond"
            :audience "operator" :runtime-revision "conscious-q5-v2"
            :conscious-state-revision 7 :clock-identity "fixture-clock"
            :total-character-budget 20 :section-character-budgets budgets
            :sections sections :eligible-evidence-ids (vector 10 11)
            :available-tools (vector) :permitted-proposal-kinds (vector "yield")
            :publication-constraints (obj "audiences" (vector "operator"))
            :remaining-budget (obj "tool_proposals" 0 "continuations" 0
                                   "publication_candidates" 0)))
         (state (obj "state_revision" 7 "composition_hash" "fixture-state"))
         (first (conscious-context-assemble state context))
         (second (conscious-context-assemble state context)))
    (clsc-check "S8 semantic rows truncate deterministically under section budget"
                (and (string= (shasht:write-json first nil)
                              (shasht:write-json second nil))
                     (search "first semantic row"
                             (shasht:write-json
                              (gethash "private_request" first) nil))
                     (null (search "second semantic row"
                                   (shasht:write-json
                                    (gethash "private_request" first) nil)))
                     (= 1 (length
                           (gethash "refused"
                                    (aref (gethash "sections"
                                                   (gethash "manifest" first))
                                          2))))))))

(format t "~%~d passed, ~d failed~%" *clsc-passed* *clsc-failed*)
(when (plusp *clsc-failed*)
  (error "Q5S final conversation consumer tests failed"))
