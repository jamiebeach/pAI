;;;; conscious-pulse-runtime-tests.lisp -- Q3 durable pulse adapter.
;;;;
;;;; Written before PULSE-RUNTIME.LISP. The first run fails on the absent
;;;; subject; after it exists these fixtures prove that commit and consumption
;;;; are one durably reread fact, never a best-effort pair of appends.

(in-package :agent)

(defvar *cprt-passed* 0)
(defvar *cprt-failed* 0)

(defun cprt-check (name condition)
  (if condition
      (progn (incf *cprt-passed*) (format t "PASS ~a~%" name))
      (progn (incf *cprt-failed*) (format t "FAIL ~a~%" name))))

(defun cprt-signals-p (thunk)
  (handler-case (progn (funcall thunk) nil) (error () t)))

(load (test-source "policy.lisp"))
(load (test-source "stimulus.lisp"))
(load (test-source "census.lisp"))
(load (test-source "concern.lisp"))
(load (test-source "codelets.lisp"))
(load (test-source "context.lisp"))
(load (test-source "inbox.lisp"))
(load (test-source "attention.lisp"))
(load (test-source "state.lisp"))
(load (test-source "pulse.lisp"))

(defvar *cprt-events* '())
(defvar *cprt-next-id* 0)
(defvar *cprt-drop-types* '())
(defvar *cprt-provider-calls* 0)
(defvar *cprt-effect-calls* 0)
(defvar *cprt-publication-calls* 0)
(defparameter *agent-id* "q3-dev")

(defun cprt-event (type id &optional payload)
  (obj "schema_version" 1 "id" id "timestamp" (+ 1000 id)
       "type" type "agent_id" "q3-dev"
       "payload" (or payload (obj)) "caused_by" :null))

(defun cprt-reset (&optional events)
  (setf *cprt-events* (copy-list events)
        *cprt-next-id* (loop for event in events
                             maximize (gethash "id" event) into maximum
                             finally (return (or maximum 0)))
        *cprt-drop-types* '()
        *cprt-provider-calls* 0
        *cprt-effect-calls* 0
        *cprt-publication-calls* 0)
  t)

(defun replay-events (&rest arguments)
  (declare (ignore arguments))
  (copy-list *cprt-events*))

(defun log-event (type payload &key caused-by)
  (let* ((id (incf *cprt-next-id*))
         (event (cprt-event type id payload)))
    (setf (gethash "caused_by" event) (or caused-by :null))
    ;; Model the real event-log contract: an ID may be returned even if the
    ;; durable append failed. The adapter must prove readability itself.
    (unless (member type *cprt-drop-types* :test #'string=)
      (setf *cprt-events* (append *cprt-events* (list event))))
    id))

(defun raw-call-model (&rest arguments)
  (declare (ignore arguments)) (incf *cprt-provider-calls*))
(defun execute (&rest arguments)
  (declare (ignore arguments)) (incf *cprt-effect-calls*))
(defun telegram-send (&rest arguments)
  (declare (ignore arguments)) (incf *cprt-publication-calls*))

(dolist (name (codelet-names)) (unregister-codelet name))
(register-codelet
 "q3-health" 10
 (lambda (stimulus context)
   (declare (ignore context))
   (when (string= "runtime-health" (gethash "kind" stimulus))
     (make-assessment
      :codelet "q3-health" :concern "runtime-health"
      :stimulus-id (gethash "stimulus_id" stimulus)
      :evidence-ids (coerce (gethash "source_event_ids" stimulus) 'list)
      :priority-class "critical" :urgency "background"
      :explanation-code "q3-runtime-health")))
 :digest "q3-health-runtime-fixture-v1")
(register-codelet
 "q3-project" 20
 (lambda (stimulus context)
   (declare (ignore context))
   (when (string= "project-change" (gethash "kind" stimulus))
     (make-assessment
      :codelet "q3-project" :concern "project-change"
      :stimulus-id (gethash "stimulus_id" stimulus)
      :evidence-ids (coerce (gethash "source_event_ids" stimulus) 'list)
      :priority-class "ambient" :urgency "background"
      :explanation-code "q3-project-change")))
 :digest "q3-project-runtime-fixture-v1")

(defun cprt-context (&optional (now 2000))
  (make-projection-context
   :now now :agent-id "q3-dev" :runtime-revision "conscious-q3-test"
   :consumer "conscious-state"))

(defun cprt-state (events &optional (context (cprt-context)))
  (conscious-state-project events :context context))

(defun cprt-budget ()
  (make-deterministic-pulse-budget
   :wall-milliseconds 1000 :context-characters 4096
   :proposals 1 :cancellation-checks 8))

(format t "~%== Q3 durable adapter subject ==~%")

(let ((path (merge-pathnames "src/mind/conscious/pulse-runtime.lisp"
                             *pai-root*)))
  (cprt-check "durable pulse adapter exists" (probe-file path))
  (when (probe-file path)
    (load path)

    (format t "~%== atomic commit and advancement ==~%")
    (let* ((source-events
             (list (cprt-event "runtime-observer-error" 1
                               (obj "source" "fixture"))
                   (cprt-event "episode-boundary-detected" 2
                               (obj "turn_id" "turn-2"))))
           (context (cprt-context))
           (before (cprt-state source-events context)))
      (cprt-reset source-events)
      (multiple-value-bind (plan after)
          (conscious-pulse-runtime-run
           before :projection-context context :agent-id "q3-dev"
           :runtime-revision "conscious-q3-test" :purpose :orient
           :now 2000 :clock-identity "fixture-clock" :budget (cprt-budget))
        (declare (ignore plan))
        (let* ((types (mapcar (lambda (event) (gethash "type" event))
                              *cprt-events*))
               (commit (find "pulse-committed" *cprt-events*
                             :key (lambda (event) (gethash "type" event))
                             :test #'string=))
               (payload (and commit (gethash "payload" commit)))
               (focus (gethash "value" (gethash "focus" after))))
          (cprt-check "one terminal event is both commit and consumption"
                      (and (= 1 (count "pulse-committed" types :test #'string=))
                           (zerop (count "stimulus-consumed" types
                                        :test #'string=))
                           (equalp (vector "stimulus:1")
                                   (gethash "stimulus_ids" payload))))
          (cprt-check "commit acknowledgement is partitioned and scoped"
                      (and (string= "q3-dev" (gethash "agent_id" payload))
                           (string= "conscious-state"
                                    (gethash "consumer" payload))
                           (string= "handled" (gethash "disposition" payload))))
          (cprt-check "commit-derived consumption advances to the next focus"
                      (and (= 1 (gethash "state_revision" after))
                           (search "2" (shasht:write-json focus nil)))))

      (format t "~%== event-derived sequence ==~%")
      (let ((before-second (cprt-state *cprt-events* context)))
        (multiple-value-bind (second after-second)
            (conscious-pulse-runtime-run
             before-second :projection-context context :agent-id "q3-dev"
             :runtime-revision "conscious-q3-test" :purpose :orient
             :now 2000 :clock-identity "fixture-clock" :budget (cprt-budget))
          (cprt-check "second commit derives sequence two from replay"
                      (and (= 2 (gethash "pulse_sequence" second))
                           (= 2 (gethash "state_revision" after-second)))))))

    (format t "~%== durable reread failure ==~%")
    (let* ((events (list (cprt-event "runtime-observer-error" 1
                                     (obj "source" "fixture"))))
           (context (cprt-context))
           (state (cprt-state events context))
           (fingerprint (shasht:write-json state nil)))
      (cprt-reset events)
      (setf *cprt-drop-types* '("pulse-committed"))
      (cprt-check "returned but unreadable terminal ID fails closed"
                  (cprt-signals-p
                   (lambda ()
                     (conscious-pulse-runtime-run
                      state :projection-context context :agent-id "q3-dev"
                      :runtime-revision "conscious-q3-test" :purpose :orient
                      :now 2000 :clock-identity "fixture-clock"
                      :budget (cprt-budget)))))
      (cprt-check "failed durable terminal does not mutate the input state"
                  (string= fingerprint (shasht:write-json state nil))))

    (format t "~%== cancellation ==~%")
    (let* ((events (list (cprt-event "runtime-observer-error" 1
                                     (obj "source" "fixture"))))
           (context (cprt-context))
           (state (cprt-state events context)))
      (cprt-reset events)
      (multiple-value-bind (cancelled after)
          (conscious-pulse-runtime-run
           state :projection-context context :agent-id "q3-dev"
           :runtime-revision "conscious-q3-test" :purpose :orient
           :now 2000 :clock-identity "fixture-clock" :budget (cprt-budget)
           :cancelled-p t)
        (cprt-check "cancelled pulse writes a terminal but no commit"
                    (and (string= "cancelled" (gethash "status" cancelled))
                         (= 1 (count "pulse-cancelled" *cprt-events*
                                     :key (lambda (event)
                                            (gethash "type" event))
                                     :test #'string=))
                         (null (find "pulse-committed" *cprt-events*
                                     :key (lambda (event)
                                            (gethash "type" event))
                                     :test #'string=))))
        (cprt-check "cancellation leaves committed state unchanged"
                    (and (zerop (gethash "state_revision" after))
                         (= 1 (gethash "observation_revision" after))))))

    (format t "~%== planning failure terminal ==~%")
    (let* ((events (list (cprt-event "runtime-observer-error" 1
                                     (obj "source" "fixture"))))
           (context (cprt-context))
           (state (cprt-state events context))
           (bad-budget (cprt-budget)))
      (setf (gethash "model_calls" bad-budget) 1)
      (cprt-reset events)
      (cprt-check "planning rejection signals to the caller"
                  (cprt-signals-p
                   (lambda ()
                     (conscious-pulse-runtime-run
                      state :projection-context context :agent-id "q3-dev"
                      :runtime-revision "conscious-q3-test" :purpose :orient
                      :now 2000 :clock-identity "fixture-clock"
                      :budget bad-budget))))
      (cprt-check "planning rejection durably terminalizes its open"
                  (and (= 1 (count "pulse-opened" *cprt-events*
                                   :key (lambda (event) (gethash "type" event))
                                   :test #'string=))
                       (= 1 (count "pulse-failed" *cprt-events*
                                   :key (lambda (event) (gethash "type" event))
                                   :test #'string=)))))

    (format t "~%== orphan recovery ==~%")
    (let* ((events (list (cprt-event "pulse-opened" 9
                                     (obj "runtime_revision"
                                          "conscious-q3-test"))))
           (context (cprt-context)))
      (declare (ignore context))
      (cprt-reset events)
      (cprt-check "orphaned durable open is recovered once"
                  (= 1 (conscious-pulse-runtime-recover
                        :agent-id "q3-dev"
                        :runtime-revision "conscious-q3-test")))
      (cprt-check "recovery is idempotent after its terminal record"
                  (zerop (conscious-pulse-runtime-recover
                          :agent-id "q3-dev"
                          :runtime-revision "conscious-q3-test"))))

    (cprt-check "durable adapter calls no provider, effect or publication"
                (and (zerop *cprt-provider-calls*)
                     (zerop *cprt-effect-calls*)
                     (zerop *cprt-publication-calls*)))

    (let ((source (uiop:read-file-string path)))
      (cprt-check "adapter source has no forbidden runtime route"
                  (notany (lambda (needle)
                            (search needle source :test #'char-equal))
                          '("(raw-call-model" "(call-model" "(auto-turn"
                            "(execute" "(telegram-send"))))))

    (format t "~%== selected conscious runtime integration ==~%")
    (load (test-source "cognition-runtime.lisp"))
    (load (test-source "mind/conscious/lifecycle.lisp"))
    (load (test-source "lifecycle-runtime.lisp"))
    (load (test-source "lifecycle-sources.lisp"))
    (load (test-source "lifecycle-semantics.lisp"))
    (load (test-source "motivation.lisp"))
    (load (test-source "motivation-runtime.lisp"))
    (load (test-source "runtime.lisp"))
    (cprt-check "conscious runtime exposes an explicit manual pulse"
                (fboundp 'conscious-cognition-runtime-pulse))
    (when (fboundp 'conscious-cognition-runtime-pulse)
      (setf *cognition-runtime-registry* (make-hash-table :test #'eq)
            *cognition-runtime-configured-name* nil
            *cognition-runtime-selected-name* nil
            *cognition-runtime-pinned-revision* nil
            *cognition-runtime-pinned-descriptor* nil
            *cognition-runtime-state* :unconfigured
            *cognition-runtime-in-flight* 0
            *cognition-runtime-verified-p* nil
            *cognition-runtime-projection* nil)
      ;; Reload only on the fresh registry. Installed descriptor reloads remain
      ;; forbidden by the Q2 pinning contract.
      (load (test-source "runtime.lisp"))
      (let ((events (list (cprt-event "runtime-observer-error" 1
                                      (obj "source" "fixture")))))
        (cprt-reset events)
        (cognition-runtime-configure :conscious-state)
        (cognition-runtime-install)
        (cognition-runtime-restore)
        (cprt-check "runtime projection does not retain the full replay payload"
                    (null *conscious-runtime-last-events*))
        (cognition-runtime-verify)
        (let ((base (symbol-function
                     'conscious-lifecycle-runtime-reconcile-producer-events))
              (motivation-base
                (symbol-function 'conscious-motivation-runtime-reconcile))
              (calls 0) (motivation-calls 0))
          (unwind-protect
               (progn
                 (setf (symbol-function
                        'conscious-lifecycle-runtime-reconcile-producer-events)
                        (lambda (&rest arguments)
                          (incf calls)
                          (apply base arguments)))
                 (setf (symbol-function 'conscious-motivation-runtime-reconcile)
                       (lambda (&rest arguments)
                         (incf motivation-calls)
                         (apply motivation-base arguments)))
                 (submit-stimulus "ordinary hot-path fixture"
                                  :kind :user-message)
                 (cprt-check "ordinary stimulus submit does not rescan producers"
                              (and (zerop calls) (zerop motivation-calls))))
            (setf (symbol-function
                    'conscious-lifecycle-runtime-reconcile-producer-events)
                   base
                  (symbol-function 'conscious-motivation-runtime-reconcile)
                  motivation-base)))
        (setf *conscious-motivation-runtime-last-report* nil)
        (multiple-value-bind (plan after)
            (conscious-cognition-runtime-pulse
             :purpose :orient :now 2000 :clock-identity "fixture-clock"
             :budget (cprt-budget))
          (let* ((report (conscious-cognition-runtime-report))
                  (pulse (gethash "pulse" report))
                  (motivation (gethash "motivation_candidates" report)))
            (cprt-check "manual runtime pulse installs only committed projection"
                        (and (string= "completed" (gethash "status" plan))
                             (eq after *cognition-runtime-projection*)
                             (= 1 (gethash "state_revision" after))))
            (cprt-check "runtime report exposes content-free Q3/Q4 pulse truth"
                        (and (string= "manual-q5-lifecycle"
                                      (gethash "pulse_worker" report))
                             (hash-table-p pulse)
                             (hash-table-p motivation)
                             (string= "reconciled"
                                      (gethash "state" motivation ""))
                             (null (gethash "provider_route" pulse))
                             (null (gethash "effect_route" pulse))
                             (null (gethash "publication_route" pulse)))))))
      (cprt-check "selected runtime exposes the Q5 lifecycle boundary"
                  (fboundp 'conscious-cognition-runtime-lifecycle-transition))
      (multiple-value-bind (lifecycle-event-id after-lifecycle)
          (conscious-cognition-runtime-lifecycle-transition
           "work:q5-integration" "open" :request-id "open:q5-integration"
           :lifecycle-kind "private-exploration" :now 2001)
        (let* ((awaited (gethash "awaited" after-lifecycle))
               (rows (gethash "value" awaited))
               (report (conscious-cognition-runtime-report))
               (lifecycle-report (gethash "lifecycle" report)))
          (cprt-check "Q5 transition reaches the conscious awaited consumer"
                      (and (integerp lifecycle-event-id)
                           (string= "open" (gethash "lifecycle" awaited))
                           (= 1 (length rows))
                           (string= "work:q5-integration"
                                    (gethash "lifecycle_id" (aref rows 0)))))
          (cprt-check "runtime report exposes content-free lifecycle truth"
                      (and (= 1 (gethash "active_count" lifecycle-report))
                           (not (search "work:q5-integration"
                                        (shasht:write-json lifecycle-report nil)))))))
      (setf *cognition-runtime-projection* nil
            *conscious-lifecycle-runtime-projection* nil)
      (cognition-runtime-restore)
      (cprt-check "selected runtime restore rebuilds Q5 awaited state"
                  (= 1 (length
                        (gethash "value"
                                 (gethash "awaited"
                                          *cognition-runtime-projection*)))))
      (log-event
       "near-term-intention-created"
       (obj "schema_version" 1 "intention_id" "fixture-deferred"
            "receipt_id" "fixture-receipt" "state" "seeded"
            "pass_count" 0 "detail" :null))
      (cognition-runtime-restore)
      (let* ((rows (gethash "value"
                            (gethash "awaited" *cognition-runtime-projection*)))
             (report (conscious-cognition-runtime-report))
             (sources (gethash "lifecycle_sources" report)))
        (cprt-check "real near-term source is reconciled through runtime restore"
                    (and (= 2 (length rows))
                         (= 1 (gethash "appended_count" sources))
                         (find "near-term:fixture-deferred" rows
                               :key (lambda (row) (gethash "lifecycle_id" row))
                               :test #'string=))))
      (let ((installed *cognition-runtime-projection*))
        (log-event
         "near-term-intention-created"
         (obj "schema_version" 1 "intention_id" "fixture-outage"
              "receipt_id" "fixture-outage-receipt" "state" "seeded"
              "pass_count" 0 "detail" :null))
        (setf *cprt-drop-types* '("conscious-lifecycle-transition"))
        (let ((signalled (cprt-signals-p #'cognition-runtime-restore))
              (source-report (conscious-lifecycle-source-report)))
          (cprt-check "source append outage preserves the installed projection"
                      (and signalled
                           (eq installed *cognition-runtime-projection*)
                           (string= "failed" (gethash "state" source-report))
                           (= 1 (gethash "operational_failure_count"
                                         source-report)))))
        (setf *cprt-drop-types* '()))
      (cprt-reset
       (list (cprt-event "pulse-opened" 9
                         (obj "runtime_revision"
                              *conscious-cognition-runtime-revision*))))
      (setf *cognition-runtime-projection* nil)
      (cognition-runtime-restore)
      (cprt-check "runtime restore terminalizes orphaned opens before projection"
                  (= 1 (count "pulse-recovered" *cprt-events*
                              :key (lambda (event) (gethash "type" event))
                              :test #'string=)))))

(format t "~%~d passed, ~d failed~%" *cprt-passed* *cprt-failed*)
(when (plusp *cprt-failed*) (uiop:quit 1))
