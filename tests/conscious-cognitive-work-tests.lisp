;;;; conscious-cognitive-work-tests.lisp -- generic durable work projection.

(in-package :agent)

(defvar *ccw-pass* 0)
(defvar *ccw-fail* 0)

(defun ccw-check (name condition)
  (if condition
      (progn (incf *ccw-pass*) (format t "PASS ~a~%" name))
      (progn (incf *ccw-fail*) (format t "FAIL ~a~%" name))))

(defun ccw-signals-p (thunk)
  (handler-case (progn (funcall thunk) nil) (error () t)))

(defun ccw-profile (&key (models 8) (tools 6) (continuations 3))
  (obj "profile_id" "interactive-dev" "revision" 1
       "max_model_calls" models "max_tool_operations" tools
       "max_reasoning_continuations" continuations
       "max_tool_result_characters" 12000
       "permitted_proposal_kinds"
       (vector "tool-call-proposal" "publication-candidate"
               "request-continuation" "yield" "abstain")
       "permitted_tools" (vector "search-files")
       "budget_exhaustion" "suspend" "renewal_policy" "explicit-only"))

(defun ccw-event (id type payload)
  (obj "id" id "type" type "agent_id" "work-fixture"
       "payload" payload))

(defun ccw-open (id work-id &key (priority "direct")
                                  (urgency "interactive") (opened-at id))
  (ccw-event
   id "conscious-work-opened"
   (obj "schema_version" 1 "work_id" work-id
        "concern_identity" (format nil "concern:~a" work-id)
        "stimulus_ids" (vector (format nil "stimulus:~d" id))
        "purpose" "respond" "priority_class" priority
        "urgency_class" urgency "deadline" :null
        "opened_at" opened-at "profile" (ccw-profile))))

(format t "~%== generic durable cognitive work loop ==~%")

(let ((subject (merge-pathnames "src/mind/conscious/cognitive-work.lisp"
                                *pai-root*)))
  (ccw-check "cognitive work source exists" (probe-file subject))
  (when (probe-file subject)
    (load subject)
    (ccw-check
     "unknown work budget fields fail closed"
     (ccw-signals-p
      (lambda ()
        (conscious-work-profile-validate
         (let ((profile (ccw-profile)))
           (setf (gethash "surprise" profile) 1)
           profile)))))
    (let* ((events
             (list
              (ccw-open 1 "work:deep")
              (ccw-event 2 "model-request"
                         (obj "work_id" "work:deep" "pulse_id" "pulse:1"))
              (ccw-event 3 "pulse-committed"
                         (obj "work_id" "work:deep" "pulse_id" "pulse:1"
                              "pulse_sequence" 1
                              "proposals"
                              (vector (obj "proposal_id" "p1"
                                           "kind" "tool-call-proposal"))))
              (ccw-event 4 "conscious-tool-operation-result"
                         (obj "work_id" "work:deep" "proposal_id" "p1"
                              "result" (obj "status" "ok")))
              (ccw-event 5 "model-request"
                         (obj "work_id" "work:deep" "pulse_id" "pulse:2"))
              (ccw-event 6 "pulse-committed"
                         (obj "work_id" "work:deep" "pulse_id" "pulse:2"
                              "pulse_sequence" 2
                              "proposals"
                              (vector (obj "proposal_id" "p2"
                                           "kind" "request-continuation"))))
              (ccw-event 7 "model-request"
                         (obj "work_id" "work:deep" "pulse_id" "pulse:3"))
              (ccw-event 8 "pulse-committed"
                         (obj "work_id" "work:deep" "pulse_id" "pulse:3"
                              "pulse_sequence" 3
                              "proposals"
                              (vector (obj "proposal_id" "p3"
                                           "kind" "tool-call-proposal"))))
              (ccw-event 9 "conscious-tool-operation-result"
                         (obj "work_id" "work:deep" "proposal_id" "p3"
                              "result" (obj "status" "ok")))))
           (projection (conscious-work-project events "work-fixture"))
           (work (gethash "work:deep" (gethash "items" projection))))
      (ccw-check "more than two pulses remain one work lineage"
                 (and (= 3 (gethash "model_calls_used" work -1))
                      (= 2 (gethash "tool_operations_used" work -1))
                      (= 1 (gethash "reasoning_continuations_used" work -1))))
      (ccw-check "durable results make deep work runnable after restart"
                 (string= "runnable" (gethash "state" work "")))
      (ccw-check "next pulse retains its durable parent"
                 (string= "pulse:3" (gethash "parent_pulse_id" work ""))))
    (let* ((events
             (list
              (ccw-open 1 "work:background" :priority "ambient"
                        :urgency "background")
              (ccw-event 2 "model-request"
                         (obj "work_id" "work:background"
                              "pulse_id" "pulse:bg"))
              (ccw-event 3 "pulse-committed"
                         (obj "work_id" "work:background"
                              "pulse_id" "pulse:bg" "pulse_sequence" 7
                              "proposals"
                              (vector (obj "proposal_id" "bg:continue"
                                           "kind" "request-continuation"))))
              (ccw-open 4 "work:user" :priority "direct"
                        :urgency "interactive")))
           (projection (conscious-work-project events "work-fixture"))
           (selection (conscious-work-select projection)))
      (ccw-check "interactive stimulus preempts background continuation"
                 (and (string= "work:user" (gethash "work_id" selection ""))
                      (string= "priority-class"
                               (gethash "decided_by" selection "")))))
    (let* ((events
             (list
              (ccw-open 1 "work:served")
              (ccw-event 2 "model-request"
                         (obj "work_id" "work:served" "pulse_id" "pulse:s"))
              (ccw-event 3 "pulse-committed"
                         (obj "work_id" "work:served" "pulse_id" "pulse:s"
                              "pulse_sequence" 8
                              "proposals"
                              (vector (obj "proposal_id" "s:continue"
                                           "kind" "request-continuation"))))
              (ccw-open 4 "work:never")))
           (projection (conscious-work-project events "work-fixture"))
           (selection (conscious-work-select projection)))
      (ccw-check "never-served equal-class work prevents monopolization"
                 (and (string= "work:never" (gethash "work_id" selection ""))
                      (string= "never-served"
                               (gethash "decided_by" selection "")))))
    (let* ((events
             (list (ccw-open 1 "work:bounded")
                   (ccw-event 2 "model-request"
                              (obj "work_id" "work:bounded"
                                   "pulse_id" "pulse:1"))
                   (ccw-event 3 "pulse-committed"
                              (obj "work_id" "work:bounded"
                                   "pulse_id" "pulse:1" "pulse_sequence" 1
                                   "proposals"
                                   (vector (obj "proposal_id" "b1"
                                                "kind" "request-continuation"))))
                   (ccw-event 4 "model-request"
                              (obj "work_id" "work:bounded"
                                   "pulse_id" "pulse:2"))
                   (ccw-event 5 "pulse-committed"
                              (obj "work_id" "work:bounded"
                                   "pulse_id" "pulse:2" "pulse_sequence" 2
                                   "proposals"
                                   (vector (obj "proposal_id" "b2"
                                                "kind" "request-continuation"))))))
           (profile (ccw-profile :models 2))
           (open-payload (gethash "payload" (first events))))
      (setf (gethash "profile" open-payload) profile)
      (let* ((projection (conscious-work-project events "work-fixture"))
             (work (gethash "work:bounded" (gethash "items" projection))))
        (ccw-check "budget exhaustion is projected before another model call"
                   (and (string= "budget-exhausted"
                                 (gethash "state" work ""))
                        (string= "model-calls"
                                 (gethash "waiting_reason" work ""))))))
    (let* ((open (ccw-open 1 "work:no-reasoning"))
           (payload (gethash "payload" open)))
      (setf (gethash "profile" payload)
            (ccw-profile :models 2 :tools 1 :continuations 0))
      (let* ((projection
               (conscious-work-project (list open) "work-fixture"))
             (work (gethash "work:no-reasoning"
                            (gethash "items" projection))))
        (ccw-check "zero reasoning budget still permits an initial pulse"
                   (string= "runnable" (gethash "state" work "")))))
    (let* ((events
             (list (ccw-open 1 "work:unknown")
                   (ccw-event 2 "model-request"
                              (obj "work_id" "work:unknown"
                                   "pulse_id" "pulse:unknown"))))
           (projection (conscious-work-project events "work-fixture"))
           (work (gethash "work:unknown" (gethash "items" projection))))
      (ccw-check "provider request remains deliberating until lease authority acts"
                 (string= "deliberating" (gethash "state" work ""))))
    (let* ((events
             (list (ccw-open 1 "work:invalid")
                   (ccw-event 2 "model-request"
                              (obj "work_id" "work:invalid"
                                   "pulse_id" "pulse:invalid"))
                   (ccw-event 3 "pulse-failed"
                              (obj "work_id" "work:invalid"
                                   "pulse_id" "pulse:invalid"
                                   "terminal_reason"
                                   "captured-deliberation-rejected"))
                   (ccw-open 4 "work:next")))
           (projection (conscious-work-project events "work-fixture"))
           (failed (gethash "work:invalid" (gethash "items" projection)))
           (selection (conscious-work-select projection)))
      (ccw-check "failed pulse releases model ownership for later work"
                 (and (string= "failed" (gethash "state" failed ""))
                      (string= "work:next"
                               (gethash "work_id" selection "")))))
    (let ((events
            (list
             (ccw-open 1 "work:waiting")
             (ccw-event 2 "model-request"
                        (obj "work_id" "work:waiting" "pulse_id" "pulse:w"))
             (ccw-event 3 "pulse-committed"
                        (obj "work_id" "work:waiting" "pulse_id" "pulse:w"
                             "pulse_sequence" 1
                             "proposals"
                             (vector (obj "proposal_id" "waiting:p"
                                          "kind" "tool-call-proposal"))))
             (ccw-event 4 "model-request"
                        (obj "work_id" "work:waiting" "pulse_id" "pulse:bad")))))
      (ccw-check "waiting work cannot start a competing model call"
                 (ccw-signals-p
                  (lambda () (conscious-work-project events "work-fixture")))))
    (let* ((result (obj "schema_version" 1 "status" "ok"
                        "matches" (vector) "database_write_count" 0))
           (events
             (list
              (ccw-open 1 "work:derived-count")
              (ccw-event 2 "model-request"
                         (obj "work_id" "work:derived-count"
                              "pulse_id" "pulse:derived"))
              (ccw-event 3 "pulse-committed"
                         (obj "work_id" "work:derived-count"
                              "pulse_id" "pulse:derived" "pulse_sequence" 1
                              "proposals"
                              (vector (obj "proposal_id" "derived:p"
                                           "kind" "tool-call-proposal"))))
              (ccw-event 4 "conscious-tool-operation-result"
                         (obj "work_id" "work:derived-count"
                              "proposal_id" "derived:p"
                              "operation_id" "tool-operation:derived:p"
                              "result" result
                              ;; Deliberately false: authority is the result.
                              "result_characters" 1))))
           (projection (conscious-work-project events "work-fixture"))
           (work (gethash "work:derived-count" (gethash "items" projection))))
      (ccw-check "tool result accounting is derived from durable result bytes"
                 (> (gethash "tool_result_characters_used" work 0) 1)))
    (let* ((events
             (list
              (ccw-open 1 "work:older-served")
              (ccw-event 2 "model-request"
                         (obj "work_id" "work:older-served" "pulse_id" "pulse:o"))
              (ccw-event 3 "pulse-committed"
                         (obj "work_id" "work:older-served" "pulse_id" "pulse:o"
                              "pulse_sequence" 3
                              "proposals"
                              (vector (obj "proposal_id" "o:c"
                                           "kind" "request-continuation"))))
              (ccw-open 4 "work:newer-served")
              (ccw-event 5 "model-request"
                         (obj "work_id" "work:newer-served" "pulse_id" "pulse:n"))
              (ccw-event 6 "pulse-committed"
                         (obj "work_id" "work:newer-served" "pulse_id" "pulse:n"
                              "pulse_sequence" 8
                              "proposals"
                              (vector (obj "proposal_id" "n:c"
                                           "kind" "request-continuation"))))))
           (projection (conscious-work-project events "work-fixture"))
           (selection (conscious-work-select projection)))
      (ccw-check "least recently served wins among served equals"
                 (and (string= "work:older-served"
                               (gethash "work_id" selection ""))
                      (string= "least-recently-served"
                               (gethash "decided_by" selection "")))))
    (let* ((events
             (list
              (ccw-open 1 "work:tail")
              (ccw-event 2 "model-request"
                         (obj "work_id" "work:tail" "pulse_id" "tail:1"))
              (ccw-event 3 "pulse-committed"
                         (obj "work_id" "work:tail" "pulse_id" "tail:1"
                              "pulse_sequence" 1 "proposals"
                              (vector (obj "proposal_id" "tail:p1"
                                           "kind" "tool-call-proposal"))))
              (ccw-event 4 "conscious-tool-operation-result"
                         (obj "work_id" "work:tail" "proposal_id" "tail:p1"
                              "operation_id" "tail:op1"
                              "result" (obj "status" "ok")))
              (ccw-event 5 "model-request"
                         (obj "work_id" "work:tail" "pulse_id" "tail:2"))
              (ccw-event 6 "pulse-committed"
                         (obj "work_id" "work:tail" "pulse_id" "tail:2"
                              "pulse_sequence" 2 "proposals"
                              (vector (obj "proposal_id" "tail:p2"
                                           "kind" "publication-candidate"))))
              (ccw-event 7 "conscious-work-completed"
                         (obj "work_id" "work:tail" "reason_code" "reply"))))
           (generation (%conscious-work-project-sufficient nil "work-fixture"))
           (held nil) (held-json nil) (prefix nil) (equivalent t))
      (dolist (event events)
        (setf prefix (append prefix (list event))
              generation
              (%conscious-work-project-advance-sufficient
               generation (list event) "work-fixture"))
        (unless held
          (setf held generation
                held-json (%conscious-work-canonical-json generation)))
        (unless (string=
                 (%conscious-work-canonical-json
                  (%conscious-work-public-projection-copy generation))
                 (%conscious-work-canonical-json
                  (conscious-work-project prefix "work-fixture")))
          (setf equivalent nil)))
      (ccw-check "physical-tail fold equals full replay after every boundary"
                 equivalent)
      (ccw-check "copy-on-write leaves a held generation unchanged"
                 (string= held-json (%conscious-work-canonical-json held)))
      (let* ((public (%conscious-work-public-projection-copy generation))
             (work (gethash "work:tail" (gethash "items" public))))
        (ccw-check "public work removes sufficient-state metadata"
                   (every (lambda (key)
                            (not (nth-value 1 (gethash key work))))
                          *conscious-work-private-item-keys*))))
    (let* ((open (ccw-open 1 "work:budget-tail"))
           (payload (gethash "payload" open))
           (tail
             (list
              (ccw-event 2 "model-request"
                         (obj "work_id" "work:budget-tail"
                              "pulse_id" "budget:1"))
              (ccw-event 3 "pulse-committed"
                         (obj "work_id" "work:budget-tail"
                              "pulse_id" "budget:1" "pulse_sequence" 1
                              "proposals"
                              (vector (obj "proposal_id" "budget:p1"
                                           "kind" "request-continuation"))))
              (ccw-event 4 "conscious-work-failed"
                         (obj "work_id" "work:budget-tail"
                              "reason_code" "bounded-failure")))))
      (setf (gethash "profile" payload) (ccw-profile :models 1))
      (let* ((prefix (append (list open) (subseq tail 0 2)))
             (generation
               (%conscious-work-project-sufficient prefix "work-fixture"))
             (advanced
               (%conscious-work-project-advance-sufficient
                generation (last tail) "work-fixture"))
             (full (conscious-work-project (append (list open) tail)
                                           "work-fixture")))
        (ccw-check "tail fold restores pre-budget state before later events"
                   (string=
                    (%conscious-work-canonical-json
                     (%conscious-work-public-projection-copy advanced))
                    (%conscious-work-canonical-json full)))))
    (let* ((events (list (ccw-open 1 "work:mine")
                         (let ((other (ccw-open 2 "work:other")))
                           (setf (gethash "agent_id" other) "another-mind")
                           other)))
           (projection (conscious-work-project events "work-fixture")))
      (ccw-check "work projection is partition isolated"
                 (and (= 1 (gethash "item_count" projection -1))
                      (null (gethash "work:other"
                                     (gethash "items" projection))))))))

(format t "~%~d passed, ~d failed~%" *ccw-pass* *ccw-fail*)
(when (plusp *ccw-fail*) (error "conscious cognitive work tests failed"))
