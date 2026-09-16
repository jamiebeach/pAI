;;;; conscious-lifecycle-runtime-tests.lisp -- Q5 durable transition adapter.
;;;;
;;;; Failing-first coverage for readable appends, idempotency, restart,
;;;; cancellation, completion receipts and stale asynchronous results.

(in-package :agent)

(defvar *clrt-passed* 0)
(defvar *clrt-failed* 0)

(defun clrt-check (name condition)
  (if condition
      (progn (incf *clrt-passed*) (format t "PASS ~a~%" name))
      (progn (incf *clrt-failed*) (format t "FAIL ~a~%" name))))

(defun clrt-signals-p (thunk)
  (handler-case (progn (funcall thunk) nil) (error () t)))

(defvar *clrt-events* '())
(defvar *clrt-next-id* 0)
(defvar *clrt-drop-types* '())
(defvar *clrt-effect-calls* 0)
(defvar *clrt-publication-calls* 0)
(defvar *clrt-provider-calls* 0)
(defparameter *agent-id* "q5-dev")

(defun clrt-reset (&optional events)
  (setf *clrt-events* (copy-list events)
        *clrt-next-id* (loop for event in events
                             maximize (or (gethash "id" event) 0) into maximum
                             finally (return (or maximum 0)))
        *clrt-drop-types* '()
        *clrt-effect-calls* 0
        *clrt-publication-calls* 0
        *clrt-provider-calls* 0))

(defun replay-events (&rest arguments)
  (declare (ignore arguments))
  (copy-list *clrt-events*))

(defun log-event (type payload &key caused-by)
  (let* ((id (incf *clrt-next-id*))
         (event (obj "schema_version" 1 "id" id "timestamp" (+ 1000 id)
                     "type" type "agent_id" "q5-dev"
                     "caused_by" (or caused-by :null) "payload" payload)))
    (unless (member type *clrt-drop-types* :test #'string=)
      (setf *clrt-events* (append *clrt-events* (list event))))
    id))

(defun execute (&rest arguments)
  (declare (ignore arguments)) (incf *clrt-effect-calls*))
(defun telegram-send (&rest arguments)
  (declare (ignore arguments)) (incf *clrt-publication-calls*))
(defun raw-call-model (&rest arguments)
  (declare (ignore arguments)) (incf *clrt-provider-calls*))

(defun clrt-result (id lifecycle-id revision status)
  (obj "schema_version" 1 "id" id "timestamp" (+ 1000 id)
       "type" "agent-operation-terminal" "agent_id" "q5-dev"
       "caused_by" :null
       "payload"
       (obj "operation_id" lifecycle-id "status" status
            "origin_runtime_revision" revision
            "artifact_id" (format nil "artifact:~a" id)
            "artifact_sha256" "fixture-digest")))

(format t "~%== Q5 lifecycle runtime subject ==~%")

(let ((pure-path (merge-pathnames "src/mind/conscious/lifecycle.lisp" *pai-root*))
      (runtime-path
        (merge-pathnames "src/mind/conscious/lifecycle-runtime.lisp" *pai-root*)))
  (clrt-check "durable lifecycle module exists"
              (and (probe-file pure-path) (probe-file runtime-path)))
  (when (and (probe-file pure-path) (probe-file runtime-path))
    (load pure-path)
    (load runtime-path)

    (clrt-reset)
    (let ((open-id
            (conscious-lifecycle-runtime-transition
             "work:interrupt" "open" :request-id "open:interrupt"
             :lifecycle-kind "private-exploration"
             :origin-runtime-revision "revision:a"
             :actor-runtime-revision "revision:a" :now 2000)))
      (let ((checkpoint-source
              (log-event "fixture-checkpoint-source" (obj "fixture" t))))
        (conscious-lifecycle-runtime-transition
         "work:interrupt" "checkpoint" :request-id "checkpoint:interrupt"
         :lifecycle-kind "private-exploration"
         :origin-runtime-revision "revision:a"
         :actor-runtime-revision "revision:a"
         :source-event-id checkpoint-source
         :checkpoint-ref "artifact:criteria" :reason-code "bounded-progress"
         :now 2001))
      (let ((suspend-source
              (log-event "fixture-interruption-source" (obj "fixture" t))))
        (conscious-lifecycle-runtime-transition
         "work:interrupt" "suspend" :request-id "suspend:interrupt"
         :lifecycle-kind "private-exploration"
         :origin-runtime-revision "revision:a"
         :actor-runtime-revision "revision:a" :source-event-id suspend-source
         :reason-code "interactive-barrier" :now 2002))
      (let* ((before-restart (conscious-lifecycle-runtime-project "q5-dev"))
             (before-json (shasht:write-json before-restart nil)))
        (setf *conscious-lifecycle-runtime-projection* nil)
        (let ((after-restart (conscious-lifecycle-runtime-restore "q5-dev")))
          (clrt-check "interruption survives a projection-destroying restart"
                      (and (string= before-json
                                    (shasht:write-json after-restart nil))
                           (string= "suspended"
                                    (gethash "status"
                                             (conscious-lifecycle-current
                                              after-restart "work:interrupt")))))
          (let ((resume-source
                  (log-event "fixture-resume-source" (obj "fixture" t))))
            (conscious-lifecycle-runtime-transition
             "work:interrupt" "resume" :request-id "resume:interrupt"
             :lifecycle-kind "private-exploration"
             :origin-runtime-revision "revision:a"
             :actor-runtime-revision "revision:a" :source-event-id resume-source
             :reason-code "barrier-cleared" :now 2003))
          (clrt-check "resume continues from the durable checkpoint"
                      (let ((row (conscious-lifecycle-current
                                  *conscious-lifecycle-runtime-projection*
                                  "work:interrupt")))
                        (and (string= "active" (gethash "status" row))
                             (string= "artifact:criteria"
                                      (gethash "checkpoint_ref" row)))))))
      (let ((count (length *clrt-events*)))
        (clrt-check "exact retry returns the original open event without append"
                    (and (= open-id
                            (conscious-lifecycle-runtime-transition
                             "work:interrupt" "open"
                             :request-id "open:interrupt"
                             :lifecycle-kind "private-exploration"
                             :origin-runtime-revision "revision:a"
                             :actor-runtime-revision "revision:b" :now 2000))
                         (= count (length *clrt-events*)))))
      (clrt-check "conflicting reuse of an idempotency request fails closed"
                  (clrt-signals-p
                   (lambda ()
                     (conscious-lifecycle-runtime-transition
                      "work:different" "open" :request-id "open:interrupt"
                      :lifecycle-kind "private-exploration"
                      :origin-runtime-revision "revision:a"
                      :actor-runtime-revision "revision:a" :now 2000)))))

    (clrt-reset)
    (conscious-lifecycle-runtime-transition
     "work:fabricated-receipt" "open" :request-id "open:fabricated-receipt"
     :lifecycle-kind "tool-work" :origin-runtime-revision "revision:a"
     :actor-runtime-revision "revision:a" :now 2050)
    (let ((before (length *clrt-events*)))
      (clrt-check "transition cannot cite a fabricated durable source receipt"
                  (and
                   (clrt-signals-p
                    (lambda ()
                      (conscious-lifecycle-runtime-transition
                       "work:fabricated-receipt" "complete"
                       :request-id "complete:fabricated-receipt"
                       :lifecycle-kind "tool-work"
                       :origin-runtime-revision "revision:a"
                       :actor-runtime-revision "revision:a"
                       :source-event-id 9999 :reason-code "fixture"
                       :now 2051)))
                   (= before (length *clrt-events*)))))

    (clrt-reset)
    (setf *clrt-drop-types* '("conscious-lifecycle-transition"))
    (clrt-check "returned but unreadable lifecycle append fails closed"
                (clrt-signals-p
                 (lambda ()
                   (conscious-lifecycle-runtime-transition
                    "work:unreadable" "open" :request-id "open:unreadable"
                    :lifecycle-kind "tool-work"
                    :origin-runtime-revision "revision:a"
                    :actor-runtime-revision "revision:a" :now 2100))))

    (clrt-reset)
    (conscious-lifecycle-runtime-transition
     "operation:current" "open" :request-id "open:current"
     :lifecycle-kind "tool-work" :origin-runtime-revision "revision:a"
     :actor-runtime-revision "revision:a" :now 2200)
    (let ((result (clrt-result 50 "operation:current" "revision:a" "completed")))
      (setf *clrt-next-id* 50
            *clrt-events* (append *clrt-events* (list result)))
      (let ((terminal-id
              (conscious-lifecycle-runtime-reconcile-result
               "operation:current" 50 :request-id "result:current"
               :current-runtime-revision "revision:a" :now 2201)))
        (clrt-check "current revision result completes exactly once with receipt"
                    (let* ((projection *conscious-lifecycle-runtime-projection*)
                           (row (conscious-lifecycle-current projection
                                                              "operation:current")))
                      (and (integerp terminal-id)
                           (string= "completed" (gethash "status" row))
                           (= 50 (gethash "terminal_source_event_id" row)))))
        (let ((count (length *clrt-events*)))
          (clrt-check "replayed result reconciliation is idempotent"
                      (and (= terminal-id
                              (conscious-lifecycle-runtime-reconcile-result
                               "operation:current" 50
                               :request-id "result:current"
                               :current-runtime-revision "revision:a" :now 2201))
                           (= count (length *clrt-events*)))))))

    (clrt-reset)
    (conscious-lifecycle-runtime-transition
     "operation:stale" "open" :request-id "open:stale"
     :lifecycle-kind "model-work" :origin-runtime-revision "revision:a"
     :actor-runtime-revision "revision:a" :now 2300)
    (let ((result (clrt-result 60 "operation:stale" "revision:old" "completed")))
      (setf *clrt-next-id* 60
            *clrt-events* (append *clrt-events* (list result)))
      (conscious-lifecycle-runtime-reconcile-result
       "operation:stale" 60 :request-id "result:stale"
       :current-runtime-revision "revision:a" :now 2301)
      (clrt-check "stale result is journaled and cannot complete lifecycle"
                  (and (string= "active"
                                (gethash "status"
                                         (conscious-lifecycle-current
                                          *conscious-lifecycle-runtime-projection*
                                          "operation:stale")))
                       (= 1 (count "conscious-lifecycle-result-rejected"
                                   *clrt-events*
                                   :key (lambda (event) (gethash "type" event))
                                   :test #'string=)))))

    (clrt-reset)
    (conscious-lifecycle-runtime-transition
     "schedule:cancelled" "open" :request-id "open:cancelled"
     :lifecycle-kind "scheduled-wake" :origin-runtime-revision "revision:a"
     :actor-runtime-revision "revision:a" :now 2400)
    (let ((cancel-source
            (log-event "turn-cancel-requested" (obj "fixture" t))))
      (conscious-lifecycle-runtime-transition
       "schedule:cancelled" "cancel" :request-id "cancel:cancelled"
       :lifecycle-kind "scheduled-wake" :origin-runtime-revision "revision:a"
       :actor-runtime-revision "revision:a" :source-event-id cancel-source
       :reason-code "operator-cancelled" :now 2401))
    (let ((late (clrt-result 71 "schedule:cancelled" "revision:a" "completed")))
      (setf *clrt-next-id* 71
            *clrt-events* (append *clrt-events* (list late)))
      (clrt-check "late result cannot cross a cancelled terminal"
                  (clrt-signals-p
                   (lambda ()
                     (conscious-lifecycle-runtime-reconcile-result
                      "schedule:cancelled" 71 :request-id "result:late"
                      :current-runtime-revision "revision:a" :now 2402))))
      (clrt-check "cancelled state remains terminal without duplicate completion"
                  (string= "cancelled"
                           (gethash "status"
                                    (conscious-lifecycle-current
                                     *conscious-lifecycle-runtime-projection*
                                     "schedule:cancelled")))))

    (clrt-check "lifecycle adapter calls no provider effect or publication"
                (and (zerop *clrt-provider-calls*)
                     (zerop *clrt-effect-calls*)
                     (zerop *clrt-publication-calls*)))

    (let ((source (uiop:read-file-string runtime-path)))
      (clrt-check "adapter source has no forbidden runtime route"
                  (notany (lambda (needle)
                            (search needle source :test #'char-equal))
                          '("(raw-call-model" "(call-model" "(execute"
                            "(telegram-send" "(auto-turn"))))))

(format t "~%~d passed, ~d failed~%" *clrt-passed* *clrt-failed*)
(when (plusp *clrt-failed*) (uiop:quit 1))
