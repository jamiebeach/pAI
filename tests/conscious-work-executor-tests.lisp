;;;; conscious-work-executor-tests.lisp -- one-owner, boundary-driven work loop.

(in-package :agent)

(defvar *cwex-pass* 0)
(defvar *cwex-fail* 0)

(defun cwex-check (name condition)
  (if condition
      (progn (incf *cwex-pass*) (format t "PASS ~a~%" name))
      (progn (incf *cwex-fail*) (format t "FAIL ~a~%" name))))

(defun cwex-signals-p (thunk)
  (handler-case (progn (funcall thunk) nil) (error () t)))

(defun cwex-profile ()
  (obj "profile_id" "executor-fixture" "revision" 1
       "max_model_calls" 8 "max_tool_operations" 6
       "max_reasoning_continuations" 3
       "max_tool_result_characters" 12000
       "permitted_proposal_kinds"
       (vector "tool-call-proposal" "publication-candidate"
               "request-continuation" "yield" "abstain")
       "permitted_tools" (vector "search-files")
       "budget_exhaustion" "suspend" "renewal_policy" "explicit-only"))

(defun cwex-event (id type payload)
  (obj "id" id "type" type "agent_id" "executor-fixture"
       "payload" payload))

(defun cwex-idle-prepare ()
  (obj "schema_version" 1 "status" "idle"))

(defun cwex-open (id work-id &key (priority "direct")
                                  (urgency "interactive"))
  (cwex-event
   id "conscious-work-opened"
   (obj "schema_version" 1 "work_id" work-id
        "concern_identity" (format nil "concern:~a" work-id)
        "stimulus_ids" (vector (format nil "stimulus:~d" id))
        "purpose" "respond" "priority_class" priority
        "urgency_class" urgency "deadline" :null "opened_at" id
        "profile" (cwex-profile))))

(format t "~%== wakeable cognitive work executor ==~%")

(let ((subject (merge-pathnames
                "src/mind/conscious/cognitive-work-executor.lisp" *pai-root*)))
  (cwex-check "cognitive work executor source exists" (probe-file subject))
  (when (probe-file subject)
    (load (merge-pathnames "src/mind/conscious/cognitive-work.lisp" *pai-root*))
    (load subject)
    (let ((projection-count 0))
      (conscious-work-executor-configure
       :agent-id "executor-fixture"
       :prepare-fn #'cwex-idle-prepare
       :projection-fn
       (lambda ()
         (incf projection-count)
         (conscious-work-project nil "executor-fixture"))
       :quantum-fn (lambda (&rest ignored)
                     (declare (ignore ignored)) (error "unreachable")))
      (conscious-work-executor-start)
      (sleep 1.2)
      (conscious-work-executor-stop)
      (cwex-check "idle worker performs no periodic projection without a lease"
                  (<= projection-count 1))
      (conscious-work-executor-reset))
    (let ((events (list (cwex-open 1 "work:deep")))
          (calls nil)
          (private-state-escaped nil)
          (projection-count 0)
          (latest-projection nil))
      (conscious-work-executor-configure
       :agent-id "executor-fixture"
       :prepare-fn #'cwex-idle-prepare
       :projection-fn
       (lambda ()
         (incf projection-count)
         (setf latest-projection
               (%conscious-work-project-sufficient
                events "executor-fixture")))
       :quantum-fn
       (lambda (work selection)
         (declare (ignore selection))
         (push (gethash "work_id" work) calls)
         (when (some (lambda (key) (nth-value 1 (gethash key work)))
                     *conscious-work-private-item-keys*)
           (setf private-state-escaped t))
         (setf events
               (append events
                       (list
                        (cwex-event
                         2 "model-request"
                         (obj "work_id" "work:deep" "pulse_id" "pulse:1"))
                        (cwex-event
                         3 "pulse-committed"
                         (obj "work_id" "work:deep" "pulse_id" "pulse:1"
                              "pulse_sequence" 1
                              "proposals"
                              (vector (obj "proposal_id" "p1"
                                           "kind" "request-continuation")))))))
         (obj "schema_version" 1 "status" "committed-boundary"
              "work_id" "work:deep" "boundary_kind" "pulse-committed")))
      (let ((result (conscious-work-executor-run-one)))
        (cwex-check "one run executes exactly one quantum"
                    (and (= 1 (length calls))
                         (string= "advanced" (gethash "status" result ""))))
        (cwex-check "durable state is rebuilt after the quantum"
                    (and (= 2 projection-count)
                         (= 1 (gethash "model_calls_used"
                                       (gethash "work" result) -1))))
        (cwex-check "executor callback and report hide sufficient state"
                    (and (not private-state-escaped)
                         (notany
                          (lambda (key)
                            (nth-value 1 (gethash key (gethash "work" result))))
                          *conscious-work-private-item-keys*)))
        (setf (gethash "state" (gethash "work" result)) "caller-mutated")
        (cwex-check "executor reports detach work from projection ownership"
                    (not (string=
                          "caller-mutated"
                          (gethash
                           "state"
                           (gethash "work:deep"
                                    (gethash "items" latest-projection))))))))
    (let ((events (list (cwex-open 1 "work:background"
                                  :priority "ambient" :urgency "background")))
          (order nil)
          (next-id 1)
          (operator-pending nil))
      (conscious-work-executor-configure
       :agent-id "executor-fixture"
       :prepare-fn
       (lambda ()
         (if operator-pending
             (progn
               (setf events (append events
                                    (list (cwex-open (incf next-id)
                                                     "work:operator")))
                     operator-pending nil)
               (obj "schema_version" 1 "status" "opened"))
             (cwex-idle-prepare)))
       :projection-fn (lambda ()
                        (conscious-work-project events "executor-fixture"))
       :quantum-fn
       (lambda (work selection)
         (declare (ignore selection))
         (let ((work-id (gethash "work_id" work)))
           (push work-id order)
           (cond
             ((string= work-id "work:background")
              (setf events
                    (append events
                            (list
                             (cwex-event
                              (incf next-id) "model-request"
                              (obj "work_id" work-id "pulse_id" "pulse:bg"))
                             (cwex-event
                              (incf next-id) "pulse-committed"
                              (obj "work_id" work-id "pulse_id" "pulse:bg"
                                   "pulse_sequence" 1
                                   "proposals"
                                   (vector (obj "proposal_id" "bg:continue"
                                                "kind" "request-continuation"))))
                             )))
              ;; This models a direct stimulus admitted while the first
              ;; quantum was in flight.  PREPARE opens it only after that
              ;; quantum has reached its safe boundary.
              (setf operator-pending t))
             (t
              (setf events
                    (append events
                            (list
                             (cwex-event
                              (incf next-id) "model-request"
                              (obj "work_id" work-id "pulse_id" "pulse:user"))
                             (cwex-event
                              (incf next-id) "pulse-committed"
                              (obj "work_id" work-id "pulse_id" "pulse:user"
                                   "pulse_sequence" 2
                                   "proposals"
                                   (vector (obj "proposal_id" "user:yield"
                                                "kind" "yield")))))))))
           (obj "schema_version" 1 "status" "committed-boundary"
                "work_id" work-id "boundary_kind" "pulse-committed"))))
      (conscious-work-executor-run-one)
      (conscious-work-executor-run-one)
      (cwex-check "new direct work preempts continuation only at a boundary"
                  (equal (reverse order)
                         '("work:background" "work:operator"))))
    (let ((events (list (cwex-open 1 "work:timer"
                                  :priority "relevant" :urgency "timely")))
          (calls 0))
      (conscious-work-executor-configure
       :agent-id "executor-fixture"
       :prepare-fn #'cwex-idle-prepare
       :projection-fn (lambda ()
                        (conscious-work-project events "executor-fixture"))
       :quantum-fn
       (lambda (work selection)
         (declare (ignore selection))
         (incf calls)
         (setf events
               (append events
                       (list
                        (cwex-event
                         2 "model-request"
                         (obj "work_id" (gethash "work_id" work)
                              "pulse_id" "pulse:timer"))
                        (cwex-event
                         3 "pulse-committed"
                         (obj "work_id" (gethash "work_id" work)
                              "pulse_id" "pulse:timer" "pulse_sequence" 1
                              "proposals"
                              (vector (obj "proposal_id" "timer:tool"
                                           "kind" "tool-call-proposal")))))))
         (obj "schema_version" 1 "status" "committed-boundary"
              "work_id" (gethash "work_id" work)
              "boundary_kind" "pulse-committed")))
      (let ((first (conscious-work-executor-run-one))
            (second (conscious-work-executor-run-one)))
        (cwex-check "asynchronous work uses the same scheduler"
                    (and (= 1 calls)
                         (string= "waiting-operation"
                                  (gethash "state" (gethash "work" first) ""))
                         (string= "idle" (gethash "status" second ""))))))
    (let ((events (list (cwex-open 1 "work:no-proof"))))
      (conscious-work-executor-configure
       :agent-id "executor-fixture"
       :prepare-fn #'cwex-idle-prepare
       :projection-fn (lambda ()
                        (conscious-work-project events "executor-fixture"))
       :quantum-fn
       (lambda (work selection)
         (declare (ignore work selection))
         (obj "schema_version" 1 "status" "committed-boundary"
              "work_id" "work:no-proof" "boundary_kind" "pulse-committed")))
      (cwex-check "an asserted boundary without durable progress fails closed"
                  (cwex-signals-p #'conscious-work-executor-run-one)))
    (let ((events (list (cwex-open 1 "work:prepare-failure"))))
      (conscious-work-executor-configure
       :agent-id "executor-fixture"
       :prepare-fn (lambda () (error "prepare failed"))
       :projection-fn (lambda ()
                        (conscious-work-project events "executor-fixture"))
       :quantum-fn (lambda (&rest ignored)
                     (declare (ignore ignored)) (error "unreachable")))
      (let ((before (gethash "failures" (conscious-work-executor-report))))
        (cwex-check "prepare failures are counted by the executor"
                    (and (cwex-signals-p #'conscious-work-executor-run-one)
                         (= (1+ before)
                            (gethash "failures"
                                     (conscious-work-executor-report)))))))
    (let ((events (list (cwex-open 1 "work:wrong-owner"))))
      (conscious-work-executor-configure
       :agent-id "executor-fixture"
       :prepare-fn #'cwex-idle-prepare
       :projection-fn (lambda ()
                        (conscious-work-project events "executor-fixture"))
       :quantum-fn
       (lambda (work selection)
         (declare (ignore work selection))
         (obj "schema_version" 1 "status" "committed-boundary"
              "work_id" "work:other" "boundary_kind" "pulse-committed")))
      (cwex-check "a quantum cannot claim progress for another work item"
                  (cwex-signals-p #'conscious-work-executor-run-one)))
    (let ((events (list (cwex-open 1 "work:observer-safe")))
          (reentry-refused nil))
      (conscious-work-executor-configure
       :agent-id "executor-fixture"
       :prepare-fn #'cwex-idle-prepare
       :projection-fn (lambda ()
                        (conscious-work-project events "executor-fixture"))
       :quantum-fn
       (lambda (work selection)
         (declare (ignore selection))
         (setf events
               (append events
                       (list
                        (cwex-event 2 "model-request"
                                    (obj "work_id" (gethash "work_id" work)
                                         "pulse_id" "pulse:observer"))
                        (cwex-event 3 "pulse-committed"
                                    (obj "work_id" (gethash "work_id" work)
                                         "pulse_id" "pulse:observer"
                                         "pulse_sequence" 1
                                         "proposals"
                                         (vector (obj "proposal_id" "observer:y"
                                                      "kind" "yield")))))))
         (obj "schema_version" 1 "status" "committed-boundary"
              "work_id" (gethash "work_id" work)
              "boundary_kind" "pulse-committed"))
       :observer-fn
       (lambda (status report)
         (declare (ignore report))
         (when (string= status "selected")
           (setf reentry-refused
                 (cwex-signals-p #'conscious-work-executor-run-one)))))
      (conscious-work-executor-run-one)
      (cwex-check "observer callbacks cannot reenter the single owner"
                  reentry-refused))
    (let ((events (list (cwex-open 1 "work:wakeable")))
          (calls 0))
      (conscious-work-executor-configure
       :agent-id "executor-fixture"
       :prepare-fn #'cwex-idle-prepare
       :projection-fn (lambda ()
                        (conscious-work-project events "executor-fixture"))
       :quantum-fn
       (lambda (work selection)
         (declare (ignore selection))
         (incf calls)
         (setf events
               (append events
                       (list
                        (cwex-event
                         2 "model-request"
                         (obj "work_id" (gethash "work_id" work)
                              "pulse_id" "pulse:wake"))
                        (cwex-event
                         3 "pulse-committed"
                         (obj "work_id" (gethash "work_id" work)
                              "pulse_id" "pulse:wake" "pulse_sequence" 1
                              "proposals"
                              (vector (obj "proposal_id" "wake:yield"
                                           "kind" "yield")))))))
         (obj "schema_version" 1 "status" "committed-boundary"
              "work_id" (gethash "work_id" work)
              "boundary_kind" "pulse-committed")))
      (let ((before-boundaries
              (gethash "committed_boundaries"
                       (conscious-work-executor-report))))
        (conscious-work-executor-start)
        (conscious-work-executor-wake :reason "timer-fired")
        (loop repeat 100 until (= calls 1) do (sleep 0.01))
        (conscious-work-executor-stop)
        (let ((report (conscious-work-executor-report)))
          (cwex-check "a content-free wake drives the single worker"
                      (and (= calls 1)
                           (= (1+ before-boundaries)
                              (gethash "committed_boundaries" report -1))
                           (not (gethash "running" report)))))))
    (conscious-work-executor-reset)))

(format t "~%~d passed, ~d failed~%" *cwex-pass* *cwex-fail*)
(when (plusp *cwex-fail*) (error "conscious work executor tests failed"))
