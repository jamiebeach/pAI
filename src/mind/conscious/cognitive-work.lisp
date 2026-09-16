;;;; cognitive-work.lisp -- pure durable cognitive work projection/scheduling.
;;;;
;;;; A work item spans pulses. This module performs no I/O, model call, tool,
;;;; publication or clock read. It derives resumable state and named scheduling
;;;; decisions solely from caller-supplied durable events.

(in-package :agent)

(export '(conscious-work-profile-validate conscious-work-project
          conscious-work-select conscious-work-select-operation
          *conscious-work-priority-classes*
          *conscious-work-urgency-ranks*))

(defparameter *conscious-work-schema-version* 1)
(defparameter *conscious-work-priority-classes*
  '(("critical" . 0) ("direct" . 1) ("committed" . 2)
    ("relevant" . 3) ("ambient" . 4)))
(defparameter *conscious-work-urgency-ranks*
  '(("interactive" . 0) ("timely" . 1) ("background" . 2)))
(defparameter *conscious-work-profile-keys*
  '("profile_id" "revision" "max_model_calls" "max_tool_operations"
    "max_reasoning_continuations" "max_tool_result_characters"
    "permitted_proposal_kinds" "permitted_tools"
    "budget_exhaustion" "renewal_policy"))
(defparameter *conscious-work-terminal-states*
  '("completed" "failed" "outcome-unknown"))
(defparameter *conscious-work-private-item-keys*
  '("pending_model_pulses" "seen_tool_operations"
    "%fold_state" "%fold_waiting_reason"))

(defun %conscious-work-items (value)
  (cond ((vectorp value) (coerce value 'list))
        ((listp value) (copy-list value))
        (t nil)))

(defun %conscious-work-copy (value)
  (cond
    ((hash-table-p value)
     (let ((copy (make-hash-table :test #'equal)))
       (maphash (lambda (key item)
                  (setf (gethash key copy) (%conscious-work-copy item)))
                value)
       copy))
    ((and (vectorp value) (not (stringp value)))
     (map 'vector #'%conscious-work-copy value))
    ((listp value) (mapcar #'%conscious-work-copy value))
    (t value)))

(defun %conscious-work-bounded-text-p (value maximum)
  (and (stringp value) (plusp (length value)) (<= (length value) maximum)))

(defun %conscious-work-canonical-json (value)
  "Canonicalize closed durable data without trusting derived byte counts."
  (labels ((emit (item stream)
             (cond
               ((hash-table-p item)
                (let ((keys nil))
                  (maphash (lambda (key ignored)
                             (declare (ignore ignored)) (push key keys))
                           item)
                  (write-char #\{ stream)
                  (loop for key in (sort keys #'string< :key #'princ-to-string)
                        for first = t then nil
                        do (unless first (write-char #\, stream))
                           (format stream "~s:" (princ-to-string key))
                           (emit (gethash key item) stream))
                  (write-char #\} stream)))
               ((and (vectorp item) (not (stringp item)))
                (write-char #\[ stream)
                (loop for child across item for first = t then nil
                      do (unless first (write-char #\, stream))
                         (emit child stream))
                (write-char #\] stream))
               ((stringp item) (format stream "~s" item))
               (t (format stream "~a" item)))))
    (with-output-to-string (stream) (emit value stream))))

(defun conscious-work-profile-validate (profile)
  "Validate and detach one explicit cognitive work-lease profile."
  (unless (hash-table-p profile) (error "Cognitive work profile must be an object"))
  (loop for key being the hash-keys of profile
        unless (member key *conscious-work-profile-keys* :test #'string=)
          do (error "Unknown cognitive work profile key ~s" key))
  (dolist (key *conscious-work-profile-keys*)
    (unless (nth-value 1 (gethash key profile))
      (error "Cognitive work profile is missing ~s" key)))
  (unless (%conscious-work-bounded-text-p (gethash "profile_id" profile) 128)
    (error "Cognitive work profile id is invalid"))
  (unless (and (integerp (gethash "revision" profile))
               (not (minusp (gethash "revision" profile))))
    (error "Cognitive work profile revision is invalid"))
  ;; These are malformed-configuration fences, not normal cognitive policy.
  ;; The much smaller operational values are supplied by the selected profile.
  (dolist (entry '(("max_model_calls" 10000)
                   ("max_tool_operations" 10000)
                   ("max_reasoning_continuations" 10000)
                   ("max_tool_result_characters" 10000000)))
    (let ((value (gethash (first entry) profile)))
      (unless (and (integerp value) (<= 0 value (second entry)))
        (error "Cognitive work profile has invalid ~a" (first entry)))))
  (unless (string= "suspend" (gethash "budget_exhaustion" profile ""))
    (error "Cognitive work budget exhaustion must suspend"))
  (unless (string= "explicit-only" (gethash "renewal_policy" profile ""))
    (error "Cognitive work budget renewal must be explicit"))
  (let ((proposal-kinds
          (%conscious-work-items (gethash "permitted_proposal_kinds" profile)))
        (tools (%conscious-work-items (gethash "permitted_tools" profile))))
    (unless (and proposal-kinds (<= (length proposal-kinds) 16)
                 (= (length proposal-kinds)
                    (length (remove-duplicates proposal-kinds :test #'string=)))
                 (every (lambda (kind)
                          (member kind
                                  '("tool-call-proposal" "publication-candidate"
                                    "request-continuation" "yield" "abstain")
                                  :test #'string=))
                        proposal-kinds))
      (error "Cognitive work permitted proposal kinds are invalid"))
    (unless (and (<= (length tools) 32)
                 (= (length tools)
                    (length (remove-duplicates tools :test #'string=)))
                 (every (lambda (tool)
                          (%conscious-work-bounded-text-p tool 128))
                        tools))
      (error "Cognitive work permitted tools are invalid")))
  (%conscious-work-copy profile))

(defun %conscious-work-open-item (event payload)
  (let ((work-id (gethash "work_id" payload))
        (stimuli (%conscious-work-items (gethash "stimulus_ids" payload)))
        (priority (gethash "priority_class" payload))
        (urgency (gethash "urgency_class" payload))
        (profile (conscious-work-profile-validate (gethash "profile" payload))))
    (unless (and (= *conscious-work-schema-version*
                    (gethash "schema_version" payload -1))
                 (%conscious-work-bounded-text-p work-id 256)
                 (%conscious-work-bounded-text-p
                  (gethash "concern_identity" payload) 256)
                 stimuli (<= (length stimuli) 64)
                 (every (lambda (id) (%conscious-work-bounded-text-p id 256))
                        stimuli)
                 (%conscious-work-bounded-text-p
                  (gethash "purpose" payload) 80)
                 (assoc priority *conscious-work-priority-classes* :test #'string=)
                 (assoc urgency *conscious-work-urgency-ranks* :test #'string=)
                 (or (eq :null (gethash "deadline" payload))
                     (numberp (gethash "deadline" payload)))
                 (or (eq :null (gethash "runtime_plan_hash" payload :null))
                     (and (stringp (gethash "runtime_plan_hash" payload))
                          (= 64 (length (gethash "runtime_plan_hash" payload)))))
                 (numberp (gethash "opened_at" payload)))
      (error "Conscious work open event is invalid"))
    (obj "work_id" work-id
         "concern_identity" (gethash "concern_identity" payload)
         "stimulus_ids" (coerce stimuli 'vector)
         "purpose" (gethash "purpose" payload)
         "priority_class" priority "urgency_class" urgency
         "deadline" (gethash "deadline" payload)
         "opened_at" (gethash "opened_at" payload)
         "opened_event_id" (gethash "id" event)
         "projection_revision" (gethash "id" event)
         "runtime_plan_hash" (gethash "runtime_plan_hash" payload :null)
         "profile" profile "state" "runnable" "waiting_reason" :null
         "parent_pulse_id" :null "model_calls_used" 0
         "tool_operations_used" 0 "reasoning_continuations_used" 0
         "tool_result_characters_used" 0 "service_count" 0
         "last_served_pulse" :null "pending_model_pulses" (make-hash-table :test #'equal)
         "pending_model_lease_expires_at" :null
         "pending_tool_proposal_id" :null "seen_tool_operations"
         (make-hash-table :test #'equal) "last_transition_kind" :null)))

(defun %conscious-work-proposal (payload)
  (let ((proposals (%conscious-work-items (gethash "proposals" payload))))
    (unless (= 1 (length proposals))
      (error "A cognitive work pulse must commit exactly one proposal"))
    (let ((proposal (first proposals)))
      (unless (and (hash-table-p proposal)
                   (%conscious-work-bounded-text-p
                    (gethash "proposal_id" proposal) 256)
                   (%conscious-work-bounded-text-p (gethash "kind" proposal) 80))
        (error "Cognitive work pulse proposal is invalid"))
      proposal)))

(defun %conscious-work-apply-pulse (work payload)
  (let* ((pulse-id (gethash "pulse_id" payload))
         (pending (gethash "pending_model_pulses" work))
         (proposal (%conscious-work-proposal payload))
         (kind (gethash "kind" proposal)))
    (unless (%conscious-work-bounded-text-p pulse-id 256)
      (error "Cognitive work pulse id is invalid"))
    (when (gethash pulse-id pending) (remhash pulse-id pending))
    (when (zerop (hash-table-count pending))
      (setf (gethash "pending_model_lease_expires_at" work) :null))
    (incf (gethash "service_count" work))
    (setf (gethash "last_served_pulse" work)
          (gethash "pulse_sequence" payload :null)
          (gethash "parent_pulse_id" work) pulse-id
          (gethash "waiting_reason" work) :null
          (gethash "last_transition_kind" work) kind)
    (cond
      ((string= kind "tool-call-proposal")
       (setf (gethash "state" work) "waiting-operation"
             (gethash "pending_tool_proposal_id" work)
             (gethash "proposal_id" proposal)))
      ((string= kind "request-continuation")
       (incf (gethash "reasoning_continuations_used" work))
       (setf (gethash "state" work) "runnable"))
      ((string= kind "publication-candidate")
       (setf (gethash "state" work) "completing"
             (gethash "waiting_reason" work) "publication"))
      ((string= kind "yield")
       (setf (gethash "state" work) "suspended"
             (gethash "waiting_reason" work) "yield"))
      ((string= kind "abstain")
       (setf (gethash "state" work) "completing"
             (gethash "waiting_reason" work) "abstention"))
      (t
       ;; Other proposal families require an owning adapter before the work
       ;; projector may claim a state transition.
        (setf (gethash "state" work) "waiting-operation"
              (gethash "waiting_reason" work) "proposal-adapter")))))

(defun %conscious-work-apply-pulse-failure (work payload)
  (let* ((pulse-id (gethash "pulse_id" payload))
         (pending (gethash "pending_model_pulses" work)))
    (unless (%conscious-work-bounded-text-p pulse-id 256)
      (error "Cognitive work failed pulse id is invalid"))
    ;; Assembly may fail before model-request. When a request was durable, its
    ;; ownership is explicitly released by this terminal receipt.
    (when (gethash pulse-id pending) (remhash pulse-id pending))
    (when (zerop (hash-table-count pending))
      (setf (gethash "pending_model_lease_expires_at" work) :null))
    (setf (gethash "state" work) "failed"
          (gethash "parent_pulse_id" work) pulse-id
          (gethash "last_transition_kind" work) "pulse-failed"
          (gethash "waiting_reason" work)
          (gethash "terminal_reason" payload "pulse-failed"))))

(defun %conscious-work-apply-tool-result (work payload)
  (let* ((proposal-id (gethash "proposal_id" payload))
         (expected (gethash "pending_tool_proposal_id" work))
         (operation-id (or (gethash "operation_id" payload) proposal-id))
         (seen (gethash "seen_tool_operations" work)))
    (unless (and (stringp proposal-id) (stringp expected)
                 (string= proposal-id expected))
      (error "Cognitive work tool result does not match its waiting proposal"))
    (unless (gethash operation-id seen)
      (setf (gethash operation-id seen) t)
      (incf (gethash "tool_operations_used" work))
      (let* ((result (gethash "result" payload))
             (characters
               (and (hash-table-p result)
                    (length (%conscious-work-canonical-json result)))))
        (unless characters
          (error "Cognitive work tool result is invalid"))
        (incf (gethash "tool_result_characters_used" work) characters)))
    (setf (gethash "pending_tool_proposal_id" work) :null
          (gethash "state" work) "runnable"
          (gethash "waiting_reason" work) :null
          (gethash "last_transition_kind" work) "tool-result")))

(defun %conscious-work-explicit-state (work type payload)
  (cond
    ((string= type "conscious-work-suspended")
     (setf (gethash "state" work) "suspended"
           (gethash "waiting_reason" work)
           (gethash "reason_code" payload "explicit")))
    ((string= type "conscious-work-resumed")
     (unless (string= "suspended" (gethash "state" work ""))
       (error "Only suspended cognitive work may resume"))
     (setf (gethash "state" work) "runnable"
           (gethash "waiting_reason" work) :null))
    ((string= type "conscious-work-completed")
     (setf (gethash "state" work) "completed"
           (gethash "waiting_reason" work) :null))
    ((string= type "conscious-work-failed")
     (setf (gethash "state" work) "failed"
           (gethash "waiting_reason" work)
           (gethash "reason_code" payload "failed")))
    ((string= type "conscious-work-outcome-unknown")
     (setf (gethash "state" work) "outcome-unknown"
           (gethash "waiting_reason" work)
           (gethash "reason_code" payload "outcome-unknown")))))

(defun %conscious-work-apply-budget (work)
  (when (string= "runnable" (gethash "state" work ""))
    (let ((profile (gethash "profile" work)))
      (cond
        ((>= (gethash "model_calls_used" work)
             (gethash "max_model_calls" profile))
         (setf (gethash "state" work) "budget-exhausted"
               (gethash "waiting_reason" work) "model-calls"))
        ((and (string= "request-continuation"
                       (gethash "last_transition_kind" work ""))
              (>= (gethash "reasoning_continuations_used" work)
                  (gethash "max_reasoning_continuations" profile)))
         (setf (gethash "state" work) "budget-exhausted"
               (gethash "waiting_reason" work) "reasoning-continuations"))
        ((> (gethash "tool_result_characters_used" work)
            (gethash "max_tool_result_characters" profile))
         (setf (gethash "state" work) "failed"
               (gethash "waiting_reason" work) "tool-result-budget-violated"))))))

(defun %conscious-work-event-work-id (event)
  (let ((payload (and (hash-table-p event) (gethash "payload" event))))
    (and (hash-table-p payload) (gethash "work_id" payload))))

(defun %conscious-work-public-item-copy (work)
  (let ((copy (%conscious-work-copy work)))
    (dolist (key *conscious-work-private-item-keys*) (remhash key copy))
    copy))

(defun %conscious-work-sanitize-projection (projection)
  "Remove private sufficient-state fields from one caller-owned projection."
  (maphash
   (lambda (ignored work)
     (declare (ignore ignored))
     (dolist (key *conscious-work-private-item-keys*) (remhash key work)))
   (gethash "items" projection))
  projection)

(defun %conscious-work-public-projection-copy (projection)
  (let ((copy (make-hash-table :test #'equal))
        (items (make-hash-table :test #'equal)))
    (maphash
     (lambda (work-id work)
       (setf (gethash work-id items) (%conscious-work-public-item-copy work)))
     (gethash "items" projection))
    (maphash
     (lambda (key value)
       (unless (string= key "items")
         (setf (gethash key copy) (%conscious-work-copy value))))
     projection)
    (setf (gethash "items" copy) items)
    copy))

(defun %conscious-work-restore-fold-state (work)
  (when (nth-value 1 (gethash "%fold_state" work))
    (setf (gethash "state" work) (gethash "%fold_state" work)
          (gethash "waiting_reason" work)
          (gethash "%fold_waiting_reason" work))
    (remhash "%fold_state" work)
    (remhash "%fold_waiting_reason" work))
  work)

(defun %conscious-work-finish-fold-state (work)
  ;; Budget exhaustion is a derived view, not a durable event. Retain the
  ;; pre-budget fold state privately so a later tail is exactly equivalent to
  ;; replaying the complete event prefix before deriving the budget view.
  (setf (gethash "%fold_state" work) (gethash "state" work)
        (gethash "%fold_waiting_reason" work)
        (gethash "waiting_reason" work))
  (%conscious-work-apply-budget work)
  work)

(defun %conscious-work-shallow-item-table-copy (items)
  (let ((copy (make-hash-table :test #'equal)))
    (maphash (lambda (work-id work) (setf (gethash work-id copy) work)) items)
    copy))

(defun %conscious-work-apply-child-event (work event type payload)
  (unless (member (gethash "state" work)
                  *conscious-work-terminal-states* :test #'string=)
    (cond
      ((string= type "model-request")
       (let ((pulse-id (gethash "pulse_id" payload)))
         (unless (%conscious-work-bounded-text-p pulse-id 256)
           (error "Cognitive work model request lacks a pulse"))
         (unless (string= "runnable" (gethash "state" work ""))
           (error "Only runnable cognitive work may request a model"))
         (setf (gethash pulse-id (gethash "pending_model_pulses" work)) t
               (gethash "state" work) "deliberating"
               (gethash "pending_model_lease_expires_at" work)
               (gethash "lease_expires_at" payload :null))
         (incf (gethash "model_calls_used" work))))
      ((string= type "pulse-committed")
       (%conscious-work-apply-pulse work payload))
      ((string= type "pulse-failed")
       (%conscious-work-apply-pulse-failure work payload))
      ((string= type "conscious-tool-operation-result")
       (%conscious-work-apply-tool-result work payload))
      ((member type '("conscious-work-suspended" "conscious-work-resumed"
                      "conscious-work-completed" "conscious-work-failed"
                      "conscious-work-outcome-unknown")
               :test #'string=)
       (%conscious-work-explicit-state work type payload)))
    (unless (member (gethash "state" work)
                    *conscious-work-terminal-states* :test #'string=)
      (setf (gethash "projection_revision" work) (gethash "id" event)))
    (when (and (member (gethash "state" work)
                       *conscious-work-terminal-states* :test #'string=)
               (< (gethash "projection_revision" work 0)
                  (gethash "id" event)))
      (setf (gethash "projection_revision" work) (gethash "id" event))))
  work)

(defun %conscious-work-project-advance-sufficient
    (projection events agent-id)
  "Copy-on-write fold EVENTS onto one private sufficient-state projection."
  (unless (%conscious-work-bounded-text-p agent-id 256)
    (error "Cognitive work projection requires an agent partition"))
  (unless (and (hash-table-p projection)
               (= *conscious-work-schema-version*
                  (gethash "schema_version" projection -1))
               (equal agent-id (gethash "agent_id" projection))
               (hash-table-p (gethash "items" projection))
               (integerp (gethash "event_count" projection))
               (not (minusp (gethash "event_count" projection))))
    (error "Cognitive work incremental projection state is invalid"))
  (let* ((items
           (%conscious-work-shallow-item-table-copy
            (gethash "items" projection)))
         (event-count (gethash "event_count" projection))
         (touched (make-hash-table :test #'equal)))
    (dolist (event (%conscious-work-items events))
      (when (and (hash-table-p event)
                 (equal agent-id (gethash "agent_id" event)))
        (incf event-count)
        (let* ((type (gethash "type" event ""))
               (payload (gethash "payload" event))
               (work-id (%conscious-work-event-work-id event)))
          (when (and (stringp work-id) (hash-table-p payload))
            (if (string= type "conscious-work-opened")
                (if (gethash work-id items)
                    (error "Duplicate cognitive work identity ~s" work-id)
                    (setf (gethash work-id items)
                          (%conscious-work-open-item event payload)
                          (gethash work-id touched) t))
                (let ((work (gethash work-id items)))
                  (unless work
                    (error "Cognitive work child precedes its open event"))
                  (unless (gethash work-id touched)
                    (setf work (%conscious-work-copy work)
                          (gethash work-id items) work
                          (gethash work-id touched) t)
                    (%conscious-work-restore-fold-state work))
                  (%conscious-work-apply-child-event work event type payload)))))))
    (maphash
     (lambda (work-id ignored)
       (declare (ignore ignored))
       (%conscious-work-finish-fold-state (gethash work-id items)))
     touched)
    (obj "schema_version" *conscious-work-schema-version*
         "agent_id" agent-id "event_count" event-count
         "item_count" (hash-table-count items) "items" items)))

(defun %conscious-work-project-sufficient (events agent-id)
  (%conscious-work-project-advance-sufficient
   (obj "schema_version" *conscious-work-schema-version*
        "agent_id" agent-id "event_count" 0 "item_count" 0
        "items" (make-hash-table :test #'equal))
   events agent-id))

(defun conscious-work-project (events agent-id)
  "Project a detached public cognitive-work view from ordered durable events."
  (%conscious-work-sanitize-projection
   (%conscious-work-project-sufficient events agent-id)))

(defun %conscious-work-rank (name table label)
  (let ((entry (assoc name table :test #'string=)))
    (unless entry (error "Unknown cognitive work ~a ~s" label name))
    (cdr entry)))

(defun %conscious-work-compare (left right priorities urgencies)
  (labels ((decide (less rule) (values less rule)))
    (let ((lp (%conscious-work-rank (gethash "priority_class" left)
                                    priorities "priority class"))
          (rp (%conscious-work-rank (gethash "priority_class" right)
                                    priorities "priority class")))
      (cond
        ((/= lp rp) (decide (< lp rp) "priority-class"))
        (t
         (let ((lu (%conscious-work-rank (gethash "urgency_class" left)
                                         urgencies "urgency class"))
               (ru (%conscious-work-rank (gethash "urgency_class" right)
                                         urgencies "urgency class")))
           (cond
             ((/= lu ru) (decide (< lu ru) "urgency-class"))
             ((not (equal (numberp (gethash "deadline" left))
                          (numberp (gethash "deadline" right))))
              (decide (numberp (gethash "deadline" left)) "deadline-present"))
             ((and (numberp (gethash "deadline" left))
                   (/= (gethash "deadline" left) (gethash "deadline" right)))
              (decide (< (gethash "deadline" left)
                         (gethash "deadline" right)) "deadline-soonest"))
             ((/= (if (zerop (gethash "service_count" left)) 0 1)
                   (if (zerop (gethash "service_count" right)) 0 1))
              (decide (zerop (gethash "service_count" left)) "never-served"))
             ((and (numberp (gethash "last_served_pulse" left))
                   (numberp (gethash "last_served_pulse" right))
                   (/= (gethash "last_served_pulse" left)
                       (gethash "last_served_pulse" right)))
              (decide (< (gethash "last_served_pulse" left)
                         (gethash "last_served_pulse" right))
                      "least-recently-served"))
             ((/= (gethash "opened_at" left) (gethash "opened_at" right))
              (decide (< (gethash "opened_at" left)
                         (gethash "opened_at" right)) "oldest-opened"))
             (t (decide (string< (gethash "work_id" left)
                                 (gethash "work_id" right))
                        "work-id-lexical")))))))))

(defun %conscious-work-select-state
    (projection state &key
                  (priority-classes *conscious-work-priority-classes*)
                  (urgency-ranks *conscious-work-urgency-ranks*))
  (unless (and (hash-table-p projection)
               (= *conscious-work-schema-version*
                  (gethash "schema_version" projection -1))
               (hash-table-p (gethash "items" projection)))
    (error "Cognitive work selection requires a valid projection"))
  (let ((candidates
          (loop for work being the hash-values of (gethash "items" projection)
                when (string= state (gethash "state" work ""))
                  collect work)))
    (if (null candidates)
        (obj "schema_version" 1 "status" "idle" "work_id" :null
             "decided_by" :null "candidate_count" 0)
        (let* ((ordered
                 (stable-sort
                  candidates
                  (lambda (left right)
                    (nth-value 0
                     (%conscious-work-compare left right
                                             priority-classes urgency-ranks)))))
               (winner (first ordered))
               (runner-up (second ordered)))
          (obj "schema_version" 1 "status" "selected"
               "work_id" (gethash "work_id" winner)
               "decided_by"
               (if runner-up
                   (nth-value 1
                    (%conscious-work-compare winner runner-up
                                            priority-classes urgency-ranks))
                   "sole-candidate")
               "candidate_count" (length ordered))))))

(defun conscious-work-select
    (projection &key
                  (priority-classes *conscious-work-priority-classes*)
                  (urgency-ranks *conscious-work-urgency-ranks*))
  "Select one runnable work item and name the deciding rule."
  (%conscious-work-select-state
   projection "runnable" :priority-classes priority-classes
   :urgency-ranks urgency-ranks))

(defun conscious-work-select-operation
    (projection &key
                  (priority-classes *conscious-work-priority-classes*)
                  (urgency-ranks *conscious-work-urgency-ranks*))
  "Select one durably waiting operation without running model work."
  (%conscious-work-select-state
   projection "waiting-operation" :priority-classes priority-classes
   :urgency-ranks urgency-ranks))
