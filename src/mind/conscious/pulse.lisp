;;;; pulse.lisp -- Q3 pure deterministic cognitive pulse planning.
;;;;
;;;; This file owns records, budgets and the providerless baseline decision.
;;;; It is deliberately unable to append events or cross an effect boundary.
;;;; The runtime adapter supplies durable identifiers and commits a validated
;;;; plan later. Loading this file defines data and pure functions only.

(in-package :agent)

(export '(make-deterministic-pulse-budget make-captured-pulse-budget
          conscious-pulse-plan
          conscious-pulse-plan-report *conscious-pulse-schema-version*))

(defparameter *conscious-pulse-schema-version* 1)
(defparameter *conscious-pulse-purposes*
  '(:respond :orient :continue-work :integrate-result :reconsider :recover))
(defparameter *conscious-pulse-budget-keys*
  '("wall_milliseconds" "context_characters" "proposals"
    "cancellation_checks" "model_calls" "tool_proposals"
    "continuations" "publication_candidates" "tokens" "cost_microunits"))

(defun %pulse-finite-real-p (value)
  (and (realp value)
       (or (not (floatp value))
           (and (= value value)
                (<= (- most-positive-double-float)
                    value most-positive-double-float)))))

(defun %pulse-data-only-p (value &optional (seen (make-hash-table :test #'eq)))
  "True when VALUE is bounded JSON-like data rather than executable state."
  (cond
    ((or (null value) (eq value t) (stringp value) (keywordp value)) t)
    ((realp value) (%pulse-finite-real-p value))
    ((or (functionp value) (streamp value) (pathnamep value)) nil)
    ((hash-table-p value)
     (if (gethash value seen)
         nil
         (progn
           (setf (gethash value seen) t)
           (prog1
               (loop for key being the hash-keys of value using (hash-value item)
                     always (and (stringp key)
                                 (%pulse-data-only-p item seen)))
             (remhash value seen)))))
    ((vectorp value)
     (if (gethash value seen)
         nil
         (progn
           (setf (gethash value seen) t)
           (prog1
               (and (<= (length value) 1024)
                    (loop for item across value
                          always (%pulse-data-only-p item seen)))
             (remhash value seen)))))
    ((consp value)
     (let ((length (ignore-errors (list-length value))))
       (if (or (null length) (> length 1024) (gethash value seen))
           nil
           (progn
             (setf (gethash value seen) t)
             (prog1
                 (every (lambda (item) (%pulse-data-only-p item seen)) value)
               (remhash value seen))))))
    (t nil)))

(defun %pulse-positive-bounded-integer (value name &optional (maximum 10000000))
  (unless (and (integerp value) (plusp value) (<= value maximum))
    (error "Pulse budget ~a must be a positive integer no greater than ~d"
           name maximum))
  value)

(defun %pulse-zero (value name)
  (unless (and (integerp value) (zerop value))
    (error "Deterministic Q3 pulse budget ~a must be zero" name))
  value)

(defun make-deterministic-pulse-budget
    (&key (wall-milliseconds 1000) (context-characters 4096)
          (proposals 1) (cancellation-checks 8)
          (model-calls 0) (tool-proposals 0) (continuations 0)
          (publication-candidates 0) (tokens 0) (cost-microunits 0))
  "Construct the closed Q3 budget. Capabilities deferred to Q4/Q5 are zero."
  (obj
   "wall_milliseconds"
   (%pulse-positive-bounded-integer wall-milliseconds "wall_milliseconds")
   "context_characters"
   (%pulse-positive-bounded-integer context-characters "context_characters")
   "proposals" (%pulse-positive-bounded-integer proposals "proposals" 64)
   "cancellation_checks"
   (%pulse-positive-bounded-integer cancellation-checks
                                    "cancellation_checks" 1024)
   "model_calls" (%pulse-zero model-calls "model_calls")
   "tool_proposals" (%pulse-zero tool-proposals "tool_proposals")
   "continuations" (%pulse-zero continuations "continuations")
   "publication_candidates"
   (%pulse-zero publication-candidates "publication_candidates")
   "tokens" (%pulse-zero tokens "tokens")
   "cost_microunits" (%pulse-zero cost-microunits "cost_microunits")))

(defun make-captured-pulse-budget
    (&key (wall-milliseconds 1000) (context-characters 4096)
          (proposals 8) (cancellation-checks 8) (model-calls 0)
          (tool-proposals 0) (continuations 0) (publication-candidates 0))
  "Construct a captured-output budget, optionally authorizing one provider call."
  (obj "wall_milliseconds"
       (%pulse-positive-bounded-integer wall-milliseconds "wall_milliseconds")
       "context_characters"
       (%pulse-positive-bounded-integer context-characters "context_characters")
       "proposals" (%pulse-positive-bounded-integer proposals "proposals" 64)
       "cancellation_checks"
       (%pulse-positive-bounded-integer cancellation-checks
                                        "cancellation_checks" 1024)
   "model_calls" model-calls
       "tool_proposals" tool-proposals "continuations" continuations
       "publication_candidates" publication-candidates
       "tokens" 0 "cost_microunits" 0))

(defun %pulse-validate-budget (budget &key captured-p)
  (unless (hash-table-p budget) (error "Pulse budget must be an object"))
  (loop for key being the hash-keys of budget
        unless (member key *conscious-pulse-budget-keys* :test #'string=)
          do (error "Unknown pulse budget key ~s" key))
  (dolist (key *conscious-pulse-budget-keys*)
    (unless (nth-value 1 (gethash key budget))
      (error "Pulse budget is missing ~s" key)))
  (%pulse-positive-bounded-integer
   (gethash "wall_milliseconds" budget) "wall_milliseconds")
  (%pulse-positive-bounded-integer
   (gethash "context_characters" budget) "context_characters")
  (%pulse-positive-bounded-integer
   (gethash "proposals" budget) "proposals" 64)
  (%pulse-positive-bounded-integer
   (gethash "cancellation_checks" budget) "cancellation_checks" 1024)
  (if captured-p
      (unless (and (integerp (gethash "model_calls" budget))
                   (<= 0 (gethash "model_calls" budget) 1))
        (error "Captured pulse model_calls must be zero or one"))
      (%pulse-zero (gethash "model_calls" budget) "model_calls"))
  (dolist (key '("tokens" "cost_microunits"))
    (%pulse-zero (gethash key budget) key))
  (dolist (key '("tool_proposals" "continuations" "publication_candidates"))
    (if captured-p
        (unless (and (integerp (gethash key budget))
                     (<= 0 (gethash key budget) 64))
          (error "Captured pulse budget ~a must be a bounded non-negative integer"
                 key))
        (%pulse-zero (gethash key budget) key)))
  budget)

(defun %pulse-focus (state)
  (let ((slot (and (hash-table-p state) (gethash "focus" state))))
    (and (hash-table-p slot)
         (let ((value (gethash "value" slot)))
           (and (hash-table-p value) value)))))

(defun %pulse-event-ids (focus)
  (let ((ids (and focus (gethash "evidence_ids" focus))))
    ;; A coalition may carry the same root through both its member and its
    ;; assessment. One durable fact must never become two acknowledgements.
    (coerce
     (remove-duplicates
      (cond ((vectorp ids) (coerce ids 'list))
            ((listp ids) (copy-list ids))
            (t '()))
      :test #'equal :from-end nil)
     'vector)))

(defun %pulse-stimulus-ids (event-ids)
  ;; Normalize before de-duplicating: legacy fixtures can carry the same root
  ;; once as integer 3 and once as text "3". Both name stimulus:3.
  (coerce
   (remove-duplicates
    (map 'list
         (lambda (id)
           (if (and (stringp id) (search "stimulus:" id :test #'char-equal))
               id
               (format nil "stimulus:~a" id)))
         event-ids)
    :test #'string= :from-end nil)
   'vector))

(defun %pulse-proposal (kind pulse-id runtime-revision state-revision
                        evidence-event-ids payload)
  (obj "schema_version" 1
       "proposal_id" (format nil "~a:proposal:1" pulse-id)
       "pulse_id" pulse-id
       "runtime_revision" runtime-revision
       "conscious_state_revision" state-revision
       "kind" kind "created_at_stage" "deterministic-deliberation"
       "confidence" 1.0d0
       "evidence_event_ids" evidence-event-ids
       "payload" payload))

(defun %pulse-kind-count (proposals kind)
  (count kind proposals :test #'string=
         :key (lambda (proposal) (gethash "kind" proposal))))

(defun %pulse-captured-publication-consumption (proposals context-manifest)
  "Return triggering stimuli actually evidenced by a publication candidate.

Captured deliberation used to commit every successful direct response with an
empty consumption vector.  Since user messages are non-droppable barriers,
that made every conversation turn remain active forever.  Replaying a mature
ledger then built and copied an unbounded conscious state until the heap was
exhausted.

The manifest is the trusted record of which durable IDs were rendered in the
triggering-stimuli section.  Intersecting that closed set with the validated
publication candidate's evidence means unrelated memory or lifecycle evidence
cannot acknowledge a user stimulus, and inert tool/yield proposals consume
nothing."
  (let ((triggering '()) (publication-evidence '()))
    (when (hash-table-p context-manifest)
      (map nil
           (lambda (section)
             (when (and (hash-table-p section)
                        (string= "triggering-stimuli"
                                 (gethash "name" section "")))
               (map nil (lambda (id) (pushnew id triggering :test #'equal))
                    (gethash "included_source_ids" section (vector)))))
           (gethash "sections" context-manifest (vector))))
    (map nil
         (lambda (proposal)
           (when (and (hash-table-p proposal)
                      (string= "publication-candidate"
                               (gethash "kind" proposal "")))
             (map nil
                  (lambda (id)
                    (when (member id triggering :test #'equal)
                      (pushnew id publication-evidence :test #'equal)))
                  (gethash "evidence_event_ids" proposal (vector)))))
         proposals)
    (%pulse-stimulus-ids (nreverse publication-evidence))))

(defun %pulse-terminal-plan
    (state pulse-id agent-id runtime-revision purpose opened-at now
     clock-identity budget status reason proposals consumed disposition
     &key context-manifest stage-history)
  (let ((state-revision (gethash "state_revision" state)))
    (obj
     "schema_version" *conscious-pulse-schema-version*
     "pulse_id" pulse-id "parent_pulse_id" :null
     "agent_id" agent-id "runtime_revision" runtime-revision
     "conscious_state_revision" state-revision
     "purpose" (string-downcase (symbol-name purpose))
     "opened_at" opened-at "closed_at" now
     "clock_identity" clock-identity
     "status" status "terminal_reason" reason
     "input_event_ids"
     (let ((focus (%pulse-focus state))) (%pulse-event-ids focus))
     "budget_snapshot" budget
     "selection"
     (let ((slot (gethash "selection" state)))
       (if (hash-table-p slot) (gethash "value" slot) :null))
     "stage_history"
     (or stage-history
         (vector "selecting" "committing"
                 (if (string= status "completed") "completed" status)))
     "context_manifest"
     (or context-manifest
         (obj "composition_hash" (gethash "composition_hash" state :null)
              "evidence_event_ids"
              (let ((focus (%pulse-focus state))) (%pulse-event-ids focus))
              "character_budget" (gethash "context_characters" budget)
              "rendered_characters" 0))
     "proposals" proposals
     "consumed_stimulus_ids" consumed
     "disposition" disposition
     "model_calls" 0
     "tool_proposals" (%pulse-kind-count proposals "tool-call-proposal")
     "continuations" (%pulse-kind-count proposals "request-continuation")
     "publication_candidates"
     (%pulse-kind-count proposals "publication-candidate")
     "tokens" 0 "cost_microunits" 0)))

(defun conscious-pulse-plan
    (state &key pulse-id agent-id runtime-revision purpose opened-at now
                clock-identity budget cancelled-p validated-proposals
                context-manifest (model-calls 0))
  "Produce one complete providerless pulse plan from explicit immutable data."
  (unless (and (hash-table-p state) (%pulse-data-only-p state))
    (error "Conscious state must be data-only"))
  (unless (and (stringp pulse-id) (plusp (length pulse-id)))
    (error "Pulse id must be non-empty text"))
  (unless (and (stringp agent-id) (plusp (length agent-id)))
    (error "Pulse agent id must be non-empty text"))
  (unless (and (stringp runtime-revision) (plusp (length runtime-revision)))
    (error "Pulse runtime revision must be non-empty text"))
  (unless (member purpose *conscious-pulse-purposes*)
    (error "Unknown pulse purpose ~s" purpose))
  (unless (and (numberp opened-at) (numberp now) (>= now opened-at))
    (error "Pulse clock values are invalid"))
  (unless (and (stringp clock-identity) (plusp (length clock-identity)))
    (error "Pulse clock identity must be non-empty text"))
  (%pulse-validate-budget budget :captured-p (and validated-proposals t))
  (unless (and (integerp model-calls) (<= 0 model-calls 1)
               (<= model-calls (gethash "model_calls" budget)))
    (error "Pulse provider accounting exceeds its model-call budget"))
  (when (or validated-proposals context-manifest)
    (unless (and (hash-table-p validated-proposals)
                 (hash-table-p context-manifest))
      (error "Captured deliberation requires proposals and its exact manifest")))
  (let* ((focus (%pulse-focus state))
         (event-ids (%pulse-event-ids focus))
         (elapsed-milliseconds (* 1000 (- now opened-at))))
    (cond
      (cancelled-p
       (%pulse-terminal-plan
        state pulse-id agent-id runtime-revision purpose opened-at now
        clock-identity budget "cancelled" "cancellation-requested"
        (vector) (vector) :null))
      ((> elapsed-milliseconds (gethash "wall_milliseconds" budget))
       (%pulse-terminal-plan
        state pulse-id agent-id runtime-revision purpose opened-at now
        clock-identity budget "failed" "wall-budget-exhausted"
        (vector) (vector) :null))
      (validated-proposals
       ;; Revalidate at the planner boundary. The adapter validates first for
       ;; a closed rejection reason, but the pure consumer must not trust that
       ;; its caller remembered to do so.
       (let* ((validated
                (conscious-proposals-validate validated-proposals
                                              context-manifest))
              (proposals (gethash "proposals" validated))
              (consumed
                (%pulse-captured-publication-consumption
                 proposals context-manifest))
              (plan
                (%pulse-terminal-plan
                 state pulse-id agent-id runtime-revision purpose opened-at now
                 clock-identity budget "completed" "captured-deliberation-validated"
                 proposals consumed
                 (if (plusp (length consumed)) "handled" :null)
                 :context-manifest context-manifest
                 :stage-history
                 (vector "selecting" "assembling" "deliberating" "validating"
                         "committing" "completed"))))
         (setf (gethash "model_calls" plan) model-calls)
         plan))
      ((null focus)
       (%pulse-terminal-plan
        state pulse-id agent-id runtime-revision purpose opened-at now
        clock-identity budget "completed" "no-eligible-focus"
        (vector (%pulse-proposal
                 "yield" pulse-id runtime-revision
                 (gethash "state_revision" state) event-ids
                 (obj "reason" "no-eligible-focus")))
        (vector) :null))
      ((eq purpose :respond)
       ;; Q3 has no language/publication authority. Preserve the barrier and
       ;; say exactly what happened instead of treating selection as a reply.
       (%pulse-terminal-plan
        state pulse-id agent-id runtime-revision purpose opened-at now
        clock-identity budget "completed" "requires-q4-deliberation"
        (vector (%pulse-proposal
                 "abstain" pulse-id runtime-revision
                 (gethash "state_revision" state) event-ids
                 (obj "reason" "public-deliberation-unavailable-q3")))
        (vector) :null))
      (t
       (let ((stimulus-ids (%pulse-stimulus-ids event-ids)))
         (%pulse-terminal-plan
          state pulse-id agent-id runtime-revision purpose opened-at now
          clock-identity budget "completed" "materialized"
          (vector (%pulse-proposal
                   "state-update" pulse-id runtime-revision
                   (gethash "state_revision" state) event-ids
                   (obj "operation" "materialize-selected-focus"
                        "coalition_key" (gethash "coalition_key" focus))))
          stimulus-ids "handled"))))))

(defun conscious-pulse-plan-report (plan)
  "Content-free diagnostics for a validated deterministic plan."
  (unless (and (hash-table-p plan) (%pulse-data-only-p plan))
    (error "Pulse plan must be data-only"))
  (obj "schema_version" (gethash "schema_version" plan)
       "pulse_id" (gethash "pulse_id" plan)
       "status" (gethash "status" plan)
       "terminal_reason" (gethash "terminal_reason" plan)
       "purpose" (gethash "purpose" plan)
       "proposal_count" (length (gethash "proposals" plan))
       "consumed_count" (length (gethash "consumed_stimulus_ids" plan))
       "model_calls" (gethash "model_calls" plan)
       "tool_proposals" (gethash "tool_proposals" plan)
       "cost_microunits" (gethash "cost_microunits" plan)))
