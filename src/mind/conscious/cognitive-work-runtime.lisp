;;;; cognitive-work-runtime.lisp -- exact durable cognitive work lifecycle.

(in-package :agent)

(export '(conscious-work-runtime-open-selected
          conscious-work-runtime-open-direct-event
          conscious-work-runtime-events-for-work
          conscious-work-runtime-project conscious-work-runtime-transition
          conscious-work-runtime-reap-expired-model-leases
          conscious-work-runtime-configure-head-position
          conscious-work-runtime-report))

(defparameter *conscious-work-runtime-revision* "conscious-work-runtime-v1")
(defvar *conscious-work-runtime-opens* 0)
(defvar *conscious-work-runtime-recoveries* 0)
(defvar *conscious-work-runtime-transitions* 0)
(defvar *conscious-work-runtime-head-position-fn* nil)
(defvar *conscious-work-runtime-tail-events-fn* nil)
(defvar *conscious-work-runtime-start-position* nil)
(defvar *conscious-work-runtime-projection-cache* nil)
(defvar *conscious-work-runtime-projection-cache-lock*
  (bt:make-lock "conscious work projection cache"))
(defvar *conscious-work-runtime-projection-cache-hits* 0)
(defvar *conscious-work-runtime-projection-cache-misses* 0)
(defvar *conscious-work-runtime-projection-cache-rebuilds* 0)
(defvar *conscious-work-runtime-projection-cache-advances* 0)
(defvar *conscious-work-runtime-projection-cache-fallbacks* 0)
(defvar *conscious-work-runtime-projection-cache-tail-events* 0)
(defparameter *conscious-work-runtime-transition-lock*
  (bt:make-lock "conscious work conditional transitions"))

(defun conscious-work-runtime-configure-head-position
    (function &optional tail-function start-position)
  "Install physical head/tail readers and discard any prior memoized view.

TAIL-FUNCTION accepts AFTER-POSITION, THROUGH-POSITION and event types, then
returns exact ordered events and the completed physical upper bound.
START-POSITION is an optional sealed import boundary: cognitive work before it
remains durable history but is not live recovery authority."
  (unless (and (or (null function) (functionp function))
               (or (null tail-function) (functionp tail-function))
               (or function (null tail-function))
               (or (null start-position)
                   (and (integerp start-position)
                        (not (minusp start-position))
                        function tail-function)))
    (error "Cognitive work head-position reader must be a function or NIL"))
  (bt:with-lock-held (*conscious-work-runtime-projection-cache-lock*)
    (setf *conscious-work-runtime-head-position-fn* function
          *conscious-work-runtime-tail-events-fn* tail-function
          *conscious-work-runtime-start-position* start-position
          *conscious-work-runtime-projection-cache* nil))
  function)

(defparameter *conscious-work-runtime-event-types*
  '("conscious-work-opened" "conscious-work-suspended"
    "conscious-work-resumed" "conscious-work-completed"
    "conscious-work-failed" "conscious-work-outcome-unknown"
    "model-request" "pulse-committed" "pulse-failed"
    "conscious-tool-operation-result"))

(defun %conscious-work-runtime-agent-id ()
  (unless (and (boundp '*agent-id*) (stringp *agent-id*)
               (plusp (length *agent-id*)))
    (error "Cognitive work runtime requires an agent partition"))
  *agent-id*)

(defun %conscious-work-runtime-events
    (&optional (event-types *conscious-work-runtime-event-types*))
  (unless (fboundp 'replay-events)
    (error "Cognitive work runtime requires durable event replay"))
  (let ((head (and (functionp *conscious-work-runtime-head-position-fn*)
                   (funcall *conscious-work-runtime-head-position-fn*))))
    (if (and (integerp *conscious-work-runtime-start-position*)
             (integerp head)
             (<= *conscious-work-runtime-start-position* head)
             (functionp *conscious-work-runtime-tail-events-fn*))
        (multiple-value-bind (events through-position)
            (funcall *conscious-work-runtime-tail-events-fn*
                     *conscious-work-runtime-start-position* head event-types)
          (unless (and (or (listp events) (vectorp events))
                       (eql through-position head))
            (error "Cognitive work boundary read is incomplete"))
          events)
        (funcall 'replay-events :types event-types))))

(defun %conscious-work-runtime-root-event (stimulus-id agent-id)
  (unless (and (stringp stimulus-id) (> (length stimulus-id) 9)
               (string= "stimulus:" stimulus-id :end2 9))
    (error "Cognitive work stimulus identity is invalid"))
  (let* ((event-id (parse-integer stimulus-id :start 9 :junk-allowed nil))
         (event
           (if (fboundp 'event-read-event)
               (funcall 'event-read-event event-id)
               (find event-id (funcall 'replay-events)
                     :key (lambda (row) (gethash "id" row)) :test #'equal))))
    (unless (and (hash-table-p event)
                 (equal agent-id (gethash "agent_id" event)))
      (error "Cognitive work stimulus is not durably readable"))
    event))

(defun conscious-work-runtime-events-for-work (work-id)
  "Read one work lineage and its exact durable stimulus roots.

The caller's cognitive snapshot may deliberately end at the triggering event,
before the work-open receipt. Continuation evidence therefore joins through
WORK-ID at this authority boundary rather than trusting or widening that
snapshot."
  (unless (%conscious-work-bounded-text-p work-id 256)
    (error "Cognitive work lookup requires a bounded work ID"))
  (let* ((agent-id (%conscious-work-runtime-agent-id))
         (events (%conscious-work-runtime-events))
         (owned
           (remove-if-not
            (lambda (event)
              (let ((payload
                      (and (hash-table-p event) (gethash "payload" event))))
                (and (hash-table-p payload)
                     (equal agent-id (gethash "agent_id" event))
                     (string= work-id (gethash "work_id" payload "")))))
            events))
         (opened
           (remove-if-not
            (lambda (event)
              (string= "conscious-work-opened" (gethash "type" event "")))
            owned)))
    (unless (= 1 (length opened))
      (error "Cognitive work lookup requires one durable open receipt"))
    (let ((roots
            (mapcar
             (lambda (stimulus-id)
               (%conscious-work-runtime-root-event stimulus-id agent-id))
             (%conscious-work-items
              (gethash "stimulus_ids" (gethash "payload" (first opened)))))))
      (sort (append roots owned) #'<
            :key (lambda (event) (gethash "id" event))))))

(defun %conscious-work-runtime-append (type payload caused-by)
  (unless (fboundp 'log-event)
    (error "Cognitive work runtime requires durable event append"))
  (let* ((values (multiple-value-list
                  (funcall 'log-event type payload :caused-by caused-by)))
         (id (first values)) (durable (second values)) (receipt (third values)))
    (unless (and (>= (length values) 3) id durable (hash-table-p receipt)
                 (equal id (gethash "id" receipt))
                 (string= type (gethash "type" receipt ""))
                 (equal (%conscious-work-runtime-agent-id)
                        (gethash "agent_id" receipt)))
      (error "Cognitive work ~a has no exact durable receipt" type))
    (values id receipt)))

(defun %conscious-work-runtime-append-conditional
    (predicate type payload caused-by)
  "Append TYPE only while PREDICATE still holds at durable append authority."
  (if (fboundp 'log-event-if)
      (let* ((values
               (multiple-value-list
                (funcall 'log-event-if predicate type payload
                         :caused-by caused-by)))
             (id (first values)) (durable (second values))
             (receipt (third values)) (accepted (fourth values)))
        (unless accepted
          (error "Conditional cognitive work transition no longer matches"))
        (unless (and id durable (hash-table-p receipt)
                     (equal id (gethash "id" receipt))
                     (string= type (gethash "type" receipt "")))
          (error "Conditional cognitive work transition has no durable receipt"))
        (values id receipt))
      ;; Focused unit fixtures historically supply only LOG-EVENT. Serialize
      ;; their complete check+append boundary with the same runtime lock.
      (bt:with-lock-held (*conscious-work-runtime-transition-lock*)
        (unless (funcall predicate)
          (error "Conditional cognitive work transition no longer matches"))
        (%conscious-work-runtime-append type payload caused-by))))

(defun %conscious-work-runtime-focus (state)
  (let* ((slot (and (hash-table-p state) (gethash "focus" state)))
         (focus (and (hash-table-p slot) (gethash "value" slot))))
    (unless (and (hash-table-p focus)
                 (%conscious-work-bounded-text-p
                  (gethash "coalition_key" focus) 512)
                 (%conscious-work-bounded-text-p
                  (gethash "priority_class" focus) 80))
      (error "Cognitive work requires a selected conscious focus"))
    focus))

(defun %conscious-work-runtime-stimulus-ids (focus)
  (let ((ids
          (remove-if-not
           (lambda (value)
             (and (stringp value) (> (length value) 9)
                  (string= "stimulus:" value :end2 9)))
           (%conscious-work-items (gethash "evidence_ids" focus)))))
    (setf ids (sort (remove-duplicates ids :test #'string=) #'string<))
    (unless (and ids (<= (length ids) 64))
      (error "Selected focus has no bounded stimulus roots"))
    ids))

(defun %conscious-work-runtime-source-stimuli (ids events agent-id)
  (let ((found nil))
    (dolist (event events)
      (when (and (hash-table-p event) (equal agent-id (gethash "agent_id" event)))
        (let ((stimulus (stimulus-from-event event :agent-id agent-id)))
          (when (and stimulus
                     (member (gethash "stimulus_id" stimulus) ids
                             :test #'string=))
            (push stimulus found)))))
    (unless (= (length ids)
               (length (remove-duplicates found :test #'string=
                                          :key (lambda (row)
                                                 (gethash "stimulus_id" row)))))
      (error "Selected cognitive work stimulus is absent from durable authority"))
    found))

(defun %conscious-work-runtime-urgency (stimuli)
  (let ((winner nil) (winner-rank most-positive-fixnum))
    (dolist (stimulus stimuli)
      (let* ((name (gethash "urgency_class" stimulus))
             (entry (assoc name *conscious-work-urgency-ranks* :test #'string=)))
        (unless entry (error "Selected stimulus has unknown urgency"))
        (when (< (cdr entry) winner-rank)
          (setf winner name winner-rank (cdr entry)))))
    (or winner (error "Selected cognitive work has no urgency"))))

(defun %conscious-work-runtime-id (agent-id coalition-key stimulus-ids)
  (multiple-value-bind (digest ignored-size)
      (%stimulus-payload-digest
       (obj "agent_id" agent-id "coalition_key" coalition-key
            "stimulus_ids" (coerce stimulus-ids 'vector)))
    (declare (ignore ignored-size))
    (format nil "work:~a" digest)))

(defun %conscious-work-runtime-open-event (events work-id agent-id)
  (find-if
   (lambda (event)
     (let ((payload (and (hash-table-p event) (gethash "payload" event))))
       (and (hash-table-p payload)
            (equal agent-id (gethash "agent_id" event))
            (string= "conscious-work-opened" (gethash "type" event ""))
            (string= work-id (gethash "work_id" payload "")))))
   events :from-end t))

(defun %conscious-work-runtime-retry-identity (payload)
  ;; OPENED_AT is the time of the first durable open and runtime revision is
  ;; actor provenance. Neither identifies the selected work across restart or
  ;; upgrade. The profile snapshot, roots and attention classification do.
  (obj "schema_version" (gethash "schema_version" payload)
       "work_id" (gethash "work_id" payload)
       "concern_identity" (gethash "concern_identity" payload)
       "stimulus_ids" (gethash "stimulus_ids" payload)
       "purpose" (gethash "purpose" payload)
       "priority_class" (gethash "priority_class" payload)
        "urgency_class" (gethash "urgency_class" payload)
        "deadline" (gethash "deadline" payload)
        "runtime_plan_hash" (gethash "runtime_plan_hash" payload :null)
        "profile" (gethash "profile" payload)))

(defun conscious-work-runtime-open-selected
    (state profile &key purpose opened-at (runtime-plan-hash :null))
  "Open or recover work for the exact focus selected in STATE."
  (unless (%conscious-work-bounded-text-p purpose 80)
    (error "Cognitive work purpose is invalid"))
  (unless (numberp opened-at) (error "Cognitive work opened-at is invalid"))
  (let* ((agent-id (%conscious-work-runtime-agent-id))
         (focus (%conscious-work-runtime-focus state))
         (stimulus-ids (%conscious-work-runtime-stimulus-ids focus))
         (events
           (append (%conscious-work-runtime-events)
                   (mapcar (lambda (id)
                             (%conscious-work-runtime-root-event id agent-id))
                           stimulus-ids)))
         (stimuli (%conscious-work-runtime-source-stimuli
                   stimulus-ids events agent-id))
         (coalition-key (gethash "coalition_key" focus))
         (work-id (%conscious-work-runtime-id agent-id coalition-key stimulus-ids))
         (payload
           (obj "schema_version" *conscious-work-schema-version*
                "work_id" work-id "concern_identity" coalition-key
                "stimulus_ids" (coerce stimulus-ids 'vector)
                "purpose" purpose
                "priority_class" (gethash "priority_class" focus)
                "urgency_class" (%conscious-work-runtime-urgency stimuli)
                 "deadline" :null "opened_at" opened-at
                 "runtime_plan_hash" runtime-plan-hash
                 "profile" (conscious-work-profile-validate profile)
                "runtime_revision" *conscious-work-runtime-revision*))
         (existing (%conscious-work-runtime-open-event events work-id agent-id)))
    (when existing
      (unless (string=
               (%stimulus-canonical-json
                (%conscious-work-runtime-retry-identity payload))
               (%stimulus-canonical-json
                (%conscious-work-runtime-retry-identity
                 (gethash "payload" existing))))
        (error "Cognitive work identity conflicts with durable history"))
      (incf *conscious-work-runtime-recoveries*)
      (return-from conscious-work-runtime-open-selected
        (obj "schema_version" 1 "status" "recovered" "work_id" work-id
             "event_id" (gethash "id" existing))))
    (multiple-value-bind (event-id ignored-receipt)
        (%conscious-work-runtime-append
         "conscious-work-opened" payload
         (aref (gethash "source_event_ids" (first stimuli)) 0))
      (declare (ignore ignored-receipt))
      (incf *conscious-work-runtime-opens*)
      (obj "schema_version" 1 "status" "opened" "work_id" work-id
           "event_id" event-id))))

(defun conscious-work-runtime-open-direct-event
    (event-id profile &key (purpose "respond") opened-at
                           (runtime-plan-hash :null))
  "Open or recover exact direct work rooted in one durable admitted event."
  (unless (and (integerp event-id) (plusp event-id)
               (%conscious-work-bounded-text-p purpose 80)
               (numberp opened-at))
    (error "Direct cognitive work identity is invalid"))
  (let* ((agent-id (%conscious-work-runtime-agent-id))
         (events (%conscious-work-runtime-events))
         (root
           (if (fboundp 'event-read-event)
               (funcall 'event-read-event event-id)
               (find event-id (funcall 'replay-events)
                     :key (lambda (row) (gethash "id" row)) :test #'equal))))
    (unless (and (hash-table-p root)
                 (equal agent-id (gethash "agent_id" root)))
      (error "Direct cognitive work root is not uniquely durable"))
    (let* ((stimulus (stimulus-from-event root :agent-id agent-id))
           (stimulus-id (and stimulus (gethash "stimulus_id" stimulus)))
           (concern (format nil "direct:~a" stimulus-id)))
      (unless (%conscious-work-bounded-text-p stimulus-id 256)
        (error "Direct cognitive work root is not an admitted stimulus"))
      (let* ((stimulus-ids (list stimulus-id))
             (work-id (%conscious-work-runtime-id
                       agent-id concern stimulus-ids))
             (payload
               (obj "schema_version" *conscious-work-schema-version*
                    "work_id" work-id "concern_identity" concern
                    "stimulus_ids" (coerce stimulus-ids 'vector)
                    "purpose" purpose "priority_class" "direct"
                    "urgency_class" (%conscious-work-runtime-urgency
                                      (list stimulus))
                     "deadline" :null "opened_at" opened-at
                     "runtime_plan_hash" runtime-plan-hash
                     "profile" (conscious-work-profile-validate profile)
                    "runtime_revision" *conscious-work-runtime-revision*))
             (existing
               (%conscious-work-runtime-open-event events work-id agent-id)))
        (when existing
          (unless (string=
                   (%stimulus-canonical-json
                    (%conscious-work-runtime-retry-identity payload))
                   (%stimulus-canonical-json
                    (%conscious-work-runtime-retry-identity
                     (gethash "payload" existing))))
            (error "Direct cognitive work conflicts with durable history"))
          (incf *conscious-work-runtime-recoveries*)
          (return-from conscious-work-runtime-open-direct-event
            (obj "schema_version" 1 "status" "recovered"
                 "work_id" work-id "event_id" (gethash "id" existing))))
        (multiple-value-bind (opened-id ignored)
            (%conscious-work-runtime-append
             "conscious-work-opened" payload event-id)
          (declare (ignore ignored))
          (incf *conscious-work-runtime-opens*)
          (obj "schema_version" 1 "status" "opened"
               "work_id" work-id "event_id" opened-id))))))

(defun %conscious-work-runtime-project-shared ()
  "Return one private immutable generation for audited internal readers."
  (let* ((agent-id (%conscious-work-runtime-agent-id))
         (head (and (functionp *conscious-work-runtime-head-position-fn*)
                    (funcall *conscious-work-runtime-head-position-fn*))))
    (if (not (and (integerp head) (<= 0 head)))
        (%conscious-work-project-sufficient
         (%conscious-work-runtime-events) agent-id)
        (bt:with-lock-held (*conscious-work-runtime-projection-cache-lock*)
          (let ((cached *conscious-work-runtime-projection-cache*))
            (if (and (hash-table-p cached)
                     (equal agent-id (gethash "agent_id" cached))
                     (eql head (gethash "head_position" cached)))
                (progn
                  (incf *conscious-work-runtime-projection-cache-hits*)
                  (gethash "projection" cached))
                (progn
                  (incf *conscious-work-runtime-projection-cache-misses*)
                  (labels ((rebuild ()
                             (let ((projection
                                     (%conscious-work-project-sufficient
                                      (%conscious-work-runtime-events)
                                      agent-id)))
                               (incf
                                *conscious-work-runtime-projection-cache-rebuilds*)
                               (setf *conscious-work-runtime-projection-cache*
                                     (obj "agent_id" agent-id
                                          "head_position" head
                                          "projection" projection))
                               projection)))
                    (let ((old-head
                            (and (hash-table-p cached)
                                 (equal agent-id (gethash "agent_id" cached))
                                 (gethash "head_position" cached))))
                      (if (and (integerp old-head) (< old-head head)
                               (functionp *conscious-work-runtime-tail-events-fn*))
                          (handler-case
                              (multiple-value-bind (tail through-position)
                                  (funcall
                                   *conscious-work-runtime-tail-events-fn*
                                   old-head head
                                   *conscious-work-runtime-event-types*)
                                (unless (and (or (listp tail) (vectorp tail))
                                             (eql through-position head))
                                  (error "Cognitive work tail is incomplete"))
                                (let ((projection
                                        (%conscious-work-project-advance-sufficient
                                         (gethash "projection" cached)
                                         tail agent-id)))
                                  (incf
                                   *conscious-work-runtime-projection-cache-advances*)
                                  (incf
                                   *conscious-work-runtime-projection-cache-tail-events*
                                   (length tail))
                                  (setf *conscious-work-runtime-projection-cache*
                                        (obj "agent_id" agent-id
                                             "head_position" head
                                             "projection" projection))
                                  projection))
                            (error ()
                              (incf
                               *conscious-work-runtime-projection-cache-fallbacks*)
                              (rebuild)))
                          (progn
                            (when cached
                              (incf
                               *conscious-work-runtime-projection-cache-fallbacks*))
                            (rebuild))))))))))))

(defun conscious-work-runtime-project ()
  "Return a detached projection; private cache state never crosses this API."
  (%conscious-work-public-projection-copy
   (%conscious-work-runtime-project-shared)))

(defun %conscious-work-runtime-item-shared (work-id)
  (gethash work-id
           (gethash "items" (%conscious-work-runtime-project-shared))))

(defun %conscious-work-runtime-item (work-id)
  (let ((item (%conscious-work-runtime-item-shared work-id)))
    (and item (%conscious-work-public-item-copy item))))

(defun %conscious-work-runtime-transition-type (transition)
  (cdr (assoc transition
              '(("suspended" . "conscious-work-suspended")
                ("resumed" . "conscious-work-resumed")
                ("completed" . "conscious-work-completed")
                ("failed" . "conscious-work-failed")
                ("outcome-unknown" . "conscious-work-outcome-unknown"))
              :test #'string=)))

(defun conscious-work-runtime-transition
    (work-id transition &key reason-code expected-state expected-revision)
  "Append one legal explicit work transition and return the rebuilt item."
  (unless (and (%conscious-work-bounded-text-p work-id 256)
               (%conscious-work-bounded-text-p reason-code 128)
               (%conscious-work-bounded-text-p expected-state 64)
               (integerp expected-revision) (plusp expected-revision))
    (error "Cognitive work transition identity is invalid"))
  (let ((type (%conscious-work-runtime-transition-type transition))
        (opened-event-id nil))
    (unless type (error "Cognitive work transition target is invalid"))
    (labels ((current-match-p ()
               (let* ((projection (%conscious-work-runtime-project-shared))
                      (work (gethash work-id (gethash "items" projection)))
                      (state (and work (gethash "state" work)))
                      (revision (and work (gethash "projection_revision" work))))
                 (unless (and work (string= state expected-state)
                              (eql revision expected-revision))
                   (return-from current-match-p nil))
                 (cond
                   ((string= transition "suspended")
                    (unless (string= state "runnable")
                      (error "Only runnable cognitive work may suspend")))
                   ((string= transition "resumed")
                    (unless (string= state "suspended")
                      (error "Only suspended cognitive work may resume")))
                   ((string= transition "completed")
                    (unless (string= state "completing")
                      (error "Cognitive work cannot complete before its disposition")))
                   ((member transition '("failed" "outcome-unknown")
                            :test #'string=)
                    (when (member state *conscious-work-terminal-states*
                                  :test #'string=)
                      (error "Terminal cognitive work cannot transition again"))))
                 (setf opened-event-id (gethash "opened_event_id" work))
                 t)))
      (%conscious-work-runtime-append-conditional
       #'current-match-p type
       (obj "schema_version" 1 "work_id" work-id
            "transition" transition "reason_code" reason-code
            "expected_state" expected-state
            "expected_revision" expected-revision
            "runtime_revision" *conscious-work-runtime-revision*)
       ;; The predicate sets this before the append occurs under the same lock.
       (or opened-event-id
           (let* ((projection (%conscious-work-runtime-project-shared))
                  (work (gethash work-id (gethash "items" projection))))
             (and work (gethash "opened_event_id" work))))))
    (incf *conscious-work-runtime-transitions*)
    (%conscious-work-runtime-item work-id)))

(defun conscious-work-runtime-reap-expired-model-leases
    (&optional (now (get-universal-time)))
  "Conditionally terminalize model claims whose durable lease cannot continue."
  (unless (and (integerp now) (plusp now))
    (error "Model lease recovery clock is invalid"))
  (let ((projection (%conscious-work-runtime-project-shared))
        (reaped 0))
    (maphash
     (lambda (work-id work)
       (let ((state (gethash "state" work ""))
             (expires (gethash "pending_model_lease_expires_at" work :null)))
         (when (and (string= state "deliberating")
                    (or (eq expires :null)
                        (and (integerp expires) (<= expires now))))
           (handler-case
               (progn
                 (conscious-work-runtime-transition
                  work-id "outcome-unknown"
                  :reason-code
                  (if (eq expires :null)
                      "provider-lease-unversioned"
                      "provider-lease-expired")
                  :expected-state state
                  :expected-revision (gethash "projection_revision" work))
                 (incf reaped))
             ;; A terminal arriving at the lease boundary wins through the
             ;; conditional append; the reaper writes nothing in that case.
             (error () nil)))))
     (gethash "items" projection))
    (obj "schema_version" 1 "status" "reaped"
         "reaped_count" reaped "observed_at" now)))

(defun conscious-work-runtime-report ()
  (let* ((projection (%conscious-work-runtime-project-shared))
         (selection (conscious-work-select projection)))
    (obj "schema_version" 1
         "runtime_revision" *conscious-work-runtime-revision*
         "item_count" (gethash "item_count" projection)
         "runnable_count"
         (loop for work being the hash-values of (gethash "items" projection)
               count (string= "runnable" (gethash "state" work "")))
         "selected_work_id" (gethash "work_id" selection :null)
         "opens" *conscious-work-runtime-opens*
         "recoveries" *conscious-work-runtime-recoveries*
         "transitions" *conscious-work-runtime-transitions*
         "projection_cache_hits"
         *conscious-work-runtime-projection-cache-hits*
         "projection_cache_misses"
         *conscious-work-runtime-projection-cache-misses*
         "projection_cache_rebuilds"
         *conscious-work-runtime-projection-cache-rebuilds*
         "projection_cache_advances"
         *conscious-work-runtime-projection-cache-advances*
         "projection_cache_fallbacks"
         *conscious-work-runtime-projection-cache-fallbacks*
         "projection_cache_tail_events"
         *conscious-work-runtime-projection-cache-tail-events*)))
