;;;; cognition-runtime-tests.lisp -- Q2 registry and exclusive lifecycle.
;;;;
;;;; This suite was written and run before the registry existed. Its first
;;;; run failed while resolving cognition-runtime.lisp and the source census
;;;; also found all six direct AUTO-TURN consumers. Keep the probes concrete:
;;;; exact dispatch count/values, live worker functions, and tripwire calls.

(in-package :agent)

(defvar *crt-passed* 0)
(defvar *crt-failed* 0)

(defun crt-check (name condition)
  (if condition
      (progn (incf *crt-passed*) (format t "PASS ~a~%" name))
      (progn (incf *crt-failed*) (format t "FAIL ~a~%" name))))

(defun crt-signals-p (thunk)
  (handler-case (progn (funcall thunk) nil)
    (error () t)))

(defun crt-source-text (name)
  (uiop:read-file-string (test-source name)))

(defun crt-repo-text (relative-path)
  (uiop:read-file-string (merge-pathnames relative-path *pai-root*)))

(defun crt-direct-auto-turn-p (name pattern)
  (search pattern (crt-source-text name) :test #'char-equal))

(format t "~%== failing-first consumer probe ==~%")

(dolist (probe '(("telegram.lisp" "(reply (auto-turn")
                 ("chat.lisp" "(t (auto-turn")
                 ("web.lisp" "(auto-turn prompt)")
                 ("web-terminal.lisp" "handler-case (auto-turn")
                 ("agent_helpers.lisp" "(let ((reply (auto-turn")
                 ("drives.lisp" "(reply (auto-turn")))
  (destructuring-bind (file pattern) probe
    (crt-check (format nil "~a has no direct AUTO-TURN consumer" file)
               (not (crt-direct-auto-turn-p file pattern)))))

;; Clone qualification is part of the runtime contract, not an exemption from
;; it. These were found only after the six live consumers had been converted.
(dolist (probe '(("scripts/clone-turn.lisp" "(call \"auto-turn\"")
                 ("scripts/clone-tools.lisp" "(call \"auto-turn\"")
                 ("scripts/clone-start.lisp" "(call \"auto-turn\"")
                 ("scripts/clone-presentation-port.lisp"
                  "(auto-turn \"say the single word")))
  (destructuring-bind (file pattern) probe
    (crt-check (format nil "~a qualifies through SUBMIT-STIMULUS" file)
               (and (search "submit-stimulus" (crt-repo-text file)
                            :test #'char-equal)
                    (not (search pattern (crt-repo-text file)
                                 :test #'char-equal))))))

(crt-check "tick init action routes through selected worker ownership"
           (search "cognition-runtime-start-owned-worker"
                   (crt-source-text "tick-loop.lisp") :test #'char-equal))
(crt-check "drives init action routes through selected worker ownership"
           (search "cognition-runtime-start-owned-worker"
                   (crt-source-text "drives.lisp") :test #'char-equal))

;; The core is intentionally loaded after the static probe: before Q2 this
;; suite reports the bypasses and then fails on the absent subject, rather
;; than dying before it says which consumer census was red.
(load (test-source "init.lisp"))
(load (test-source "cognition-runtime.lisp"))

(defun crt-reset ()
  (setf *cognition-runtime-registry* (make-hash-table :test #'eq)
        *cognition-runtime-configured-name* nil
        *cognition-runtime-selected-name* nil
        *cognition-runtime-pinned-revision* nil
        *cognition-runtime-pinned-descriptor* nil
        *cognition-runtime-state* :unconfigured
        *cognition-runtime-in-flight* 0
        *cognition-runtime-verified-p* nil
        *cognition-runtime-last-error* nil
        *cognition-runtime-projection* nil)
  t)

(defparameter *crt-auto-calls* 0)
(defparameter *crt-effect-calls* 0)
(defparameter *crt-provider-calls* 0)
(defparameter *crt-publication-calls* 0)
(defparameter *crt-auto-worker-live* nil)

(defun crt-auto-entry (stimulus &key kind metadata wait-for-public-result)
  (declare (ignore kind metadata))
  (incf *crt-auto-calls*)
  (values (list :exact stimulus wait-for-public-result) :published))

(defun crt-conscious-entry (stimulus &key kind metadata wait-for-public-result)
  (declare (ignore stimulus kind metadata wait-for-public-result))
  (values nil :accepted))

(defun crt-ok () t)
(defun crt-auto-worker-live-p () *crt-auto-worker-live*)
(defun crt-conscious-report () (obj "state" "idle" "degraded" nil))

(defun crt-register-fixtures ()
  (define-cognition-runtime :auto
    :revision "auto-test-r1"
    :owner "final-auto-turn"
    :entry crt-auto-entry
    :restore crt-ok
    :verify crt-ok
    :report crt-conscious-report
    :recovery-probe crt-ok
    :owned-workers (("tick-loop" crt-auto-worker-live-p)))
  (define-cognition-runtime :conscious-state
    :revision "conscious-test-r1"
    :owner "conscious-pulse"
    :entry crt-conscious-entry
    :restore crt-ok
    :verify crt-ok
    :report crt-conscious-report
    :recovery-probe crt-ok
    :owned-workers ()))

(format t "~%== closed selection and pinning ==~%")

(crt-reset)
(crt-register-fixtures)
(crt-check "unknown runtime is refused during configure"
           (crt-signals-p (lambda () (cognition-runtime-configure "unknown"))))
(crt-check "explicitly empty runtime is refused during configure"
           (crt-signals-p (lambda () (cognition-runtime-configure ""))))

(crt-reset)
(crt-register-fixtures)
(cognition-runtime-configure :auto)
(cognition-runtime-install)
(crt-check "configure and install pin auto"
           (and (eq :auto (cognition-runtime-selected-name))
                (string= "auto-test-r1" *cognition-runtime-pinned-revision*)))
(crt-check "installed selection cannot be changed"
           (crt-signals-p
            (lambda () (cognition-runtime-configure :conscious-state))))
(crt-check "installed descriptor cannot be re-registered in place"
           (crt-signals-p
            (lambda ()
              (define-cognition-runtime :auto
                :revision "auto-test-r1"
                :owner "final-auto-turn"
                :entry crt-auto-entry
                :restore crt-ok
                :verify crt-ok
                :report crt-conscious-report
                :recovery-probe crt-ok
                :owned-workers (("tick-loop" crt-auto-worker-live-p))))))

(format t "~%== exact opaque-auto parity ==~%")

(setf *crt-auto-calls* 0)
(cognition-runtime-restore)
(cognition-runtime-verify)
(multiple-value-bind (value outcome)
    (submit-stimulus "hello" :wait-for-public-result t)
  (crt-check "auto adapter entry is called exactly once" (= 1 *crt-auto-calls*))
  (crt-check "auto returns exact entry value"
             (equal '(:exact "hello" t) value))
  (crt-check "auto returns exact outcome" (eq :published outcome)))
(crt-check "in-flight count is released" (zerop *cognition-runtime-in-flight*))

(format t "~%== selected worker ownership ==~%")

(let ((started 0))
  (cognition-runtime-start-owned-worker
   :auto "tick-loop" (lambda () (incf started)))
  (crt-check "selected auto worker starts" (= 1 started))
  (crt-check "undeclared auto worker is refused"
             (crt-signals-p
              (lambda ()
                (cognition-runtime-start-owned-worker
                 :auto "not-declared" (lambda () (incf started)))))))

(crt-reset)
(crt-register-fixtures)
(cognition-runtime-configure :conscious-state)
(cognition-runtime-install)
(cognition-runtime-restore)
(cognition-runtime-verify)
(let ((started 0))
  (cognition-runtime-start-owned-worker
   :auto "tick-loop" (lambda () (incf started)))
  (crt-check "unselected auto worker does not start" (zerop started)))

(format t "~%== drift and live conflict ==~%")

(setf *cognition-runtime-selected-name* :auto)
(crt-check "configured/live selection drift fails verification"
           (crt-signals-p #'cognition-runtime-verify))
(setf *cognition-runtime-selected-name* :conscious-state)
(setf *crt-auto-worker-live* t)
(crt-check "live worker owned by unselected runtime is a conflict"
           (crt-signals-p #'cognition-runtime-verify))
(setf *crt-auto-worker-live* nil)
(cognition-runtime-verify)

(format t "~%== conscious adapter is effectless ==~%")

;; Replace the fixture entry with a tripwire-free acceptor and install fresh.
;; The forbidden operations are defined solely so an accidental call is
;; observable rather than failing for an unrelated undefined-function reason.
(setf *crt-effect-calls* 0 *crt-provider-calls* 0 *crt-publication-calls* 0)
(defun raw-call-model (&rest args) (declare (ignore args))
  (incf *crt-provider-calls*) "provider")
(defun execute (&rest args) (declare (ignore args))
  (incf *crt-effect-calls*) "effect")
(defun telegram-send (&rest args) (declare (ignore args))
  (incf *crt-publication-calls*) "publication")
(multiple-value-bind (value outcome)
    (submit-stimulus "queued" :wait-for-public-result t)
  (crt-check "conscious entry returns no public result" (null value))
  (crt-check "conscious entry accepts without publishing" (eq :accepted outcome)))
(crt-check "conscious submission calls no provider"
           (zerop *crt-provider-calls*))
(crt-check "conscious submission calls no effect"
           (zerop *crt-effect-calls*))
(crt-check "conscious submission calls no publication"
           (zerop *crt-publication-calls*))

(format t "~%== loaded Q2 adapters ==~%")

(defparameter *crt-real-auto-calls* 0)
(defparameter *crt-events* '())
(defparameter *conscious-state-schema-version* 1)
(defvar *conscious-lifecycle-runtime-projection* nil)
(defvar *conscious-lifecycle-runtime-agent-id* nil)
(defvar *conscious-lifecycle-semantic-runtime-projection* nil)

;; Q3 makes these explicit capabilities of the conscious adapter. This Q2
;; registry suite intentionally uses a fake projector/event log, so give it
;; matching inert adapter ports rather than accidentally testing load order.
(defun projection-context-p (value) (hash-table-p value))
(defun conscious-pulse-runtime-recover (&key agent-id runtime-revision)
  (declare (ignore agent-id runtime-revision)) 0)
(defun conscious-pulse-runtime-run (&rest arguments)
  (declare (ignore arguments))
  (error "Q2 registry fixture does not execute Q3 pulses"))
(defun conscious-pulse-runtime-open-captured (&rest arguments)
  (declare (ignore arguments))
  (error "Q2 registry fixture does not open Q4 deliberation"))
(defun conscious-pulse-runtime-submit-captured (&rest arguments)
  (declare (ignore arguments))
  (error "Q2 registry fixture does not submit Q4 deliberation"))
(defun conscious-pulse-runtime-report ()
  (obj "in_flight" nil "last_pulse" :null "last_error" :null
       "provider_route" nil "effect_route" nil "publication_route" nil))
(defun conscious-lifecycle-project (events &key agent-id)
  (declare (ignore events))
  (obj "schema_version" 1 "agent_id" agent-id "active_count" 0
       "terminal_count" 0 "invalid_event_ids" (vector)
       "rejected_result_count" 0 "source_rejected_count" 0
       "lifecycles" (make-hash-table :test #'equal)))
(defun conscious-lifecycle-awaiting (projection &key bound)
  (declare (ignore projection bound)) (vector))
(defun conscious-lifecycle-semantic-project (events &key agent-id)
  (declare (ignore events))
  (obj "schema_version" 1 "agent_id" (or agent-id :null)
       "descriptor_count" 0 "invalid_event_ids" (vector)
       "descriptors" (make-hash-table :test #'equal)))
(defun conscious-lifecycle-current (projection lifecycle-id)
  (declare (ignore projection lifecycle-id)) nil)
(defun conscious-lifecycle-runtime-transition (&rest arguments)
  (declare (ignore arguments))
  (error "Q2 registry fixture does not mutate Q5 lifecycles"))
(defun conscious-lifecycle-runtime-reconcile-result (&rest arguments)
  (declare (ignore arguments))
  (error "Q2 registry fixture does not reconcile Q5 results"))
(defun conscious-lifecycle-runtime-reconcile-producer-events
    (agent-id &key actor-runtime-revision)
  (declare (ignore agent-id actor-runtime-revision))
  (obj "schema_version" 1 "state" "reconciled"
       "examined_count" 0 "appended_count" 0 "recovered_count" 0
       "invalid_count" 0 "rejected_count" 0
       "operational_failure_count" 0
       "invalid_source_event_ids" (vector)
       "rejected_source_event_ids" (vector)))
(defun conscious-lifecycle-source-report ()
  (conscious-lifecycle-runtime-reconcile-producer-events "fixture"
   :actor-runtime-revision "fixture"))
(defun conscious-lifecycle-runtime-report ()
  (obj "schema_version" 1 "state" "unavailable" "active_count" 0
       "terminal_count" 0 "invalid_event_count" 0
       "rejected_result_count" 0 "source_rejected_count" 0))
(defun conscious-motivation-runtime-reconcile
    (agent-id &key actor-runtime-revision origin-runtime-revision now)
  (declare (ignore agent-id actor-runtime-revision origin-runtime-revision now))
  ;; Q2 exercises registry ownership, not motivational projection. Return one
  ;; value so runtime.lisp takes its documented replay fallback.
  (obj "schema_version" 1 "state" "reconciled"
       "examined_count" 0 "appended_count" 0 "invalid_count" 0))
(defun conscious-motivation-runtime-report ()
  (obj "schema_version" 1 "state" "unavailable"
       "active_count" 0 "invalid_event_count" 0))

(defun make-projection-context (&key now agent-id runtime-revision
                                     (soft-bound 64) (hard-bound 256)
                                     (secondary-bound 7)
                                     &allow-other-keys)
  (obj "now" now
       "agent_id" agent-id
       "runtime_revision" runtime-revision
       "bounds" (obj "soft" soft-bound "hard" hard-bound
                     "secondary" secondary-bound)))

(defun auto-turn (prompt)
  (incf *crt-real-auto-calls*)
  (values (list :opaque-final prompt) :legacy-second-value))

(defun replay-events (&rest args)
  (declare (ignore args))
  (reverse *crt-events*))

(defun log-event (type payload &key caused-by)
  (let* ((id (1+ (length *crt-events*)))
         (event (obj "schema_version" 1 "id" id "timestamp" id
                     "type" type "agent_id" "default" "payload" payload
                     "caused_by" (or caused-by :null))))
    (push event *crt-events*)
    id))

(defun conscious-state-project (events &key now agent-id current-revision context
                                            &allow-other-keys)
  (declare (ignore now agent-id))
  ;; Match Q1's real slot shape.  The first Q2 test used a convenient
  ;; top-level "degradation" lookalike which the actual projector never
  ;; emits, and therefore certified a report path that could not work live.
  (let ((degraded (not (null events))))
    (obj "schema_version" 1
         "state_revision" (length events)
         "runtime_revision" (if (hash-table-p context)
                                (gethash "runtime_revision" context)
                                current-revision)
         "focus" (obj "lifecycle" (if events "active" "idle"))
         "sensorium" (obj "value" (obj "admitted" (length events)
                                           "watermark" 0))
         "flags" (obj "value" (obj "degraded" degraded
                                      "degraded_reason"
                                      (if degraded "barrier-overflow" :null))))))

(crt-reset)
(load (test-source "runtime.lisp"))
(cognition-runtime-configure :auto)
(cognition-runtime-install)
(cognition-runtime-restore)
(cognition-runtime-verify)
(setf *crt-real-auto-calls* 0)
(multiple-value-bind (value second)
    (submit-stimulus "real-auto" :kind :user-message
                                 :wait-for-public-result t)
  (crt-check "loaded auto adapter resolves the final function once"
             (= 1 *crt-real-auto-calls*))
  (crt-check "loaded auto adapter preserves first value"
             (equal '(:opaque-final "real-auto") value))
  (crt-check "loaded auto adapter preserves additional values"
             (eq :legacy-second-value second)))

(crt-reset)
(setf *crt-events* '())
(load (test-source "runtime.lisp"))
(cognition-runtime-configure :conscious-state)
(cognition-runtime-install)
(cognition-runtime-restore)
(cognition-runtime-verify)
(let* ((report (cognition-runtime-report))
       (adapter (gethash "adapter" report)))
  (crt-check "loaded conscious adapter restores idle"
             (and (hash-table-p adapter)
                  (string= "idle" (gethash "state" adapter)))))
(setf *crt-effect-calls* 0 *crt-provider-calls* 0 *crt-publication-calls* 0)
(multiple-value-bind (value outcome)
    (submit-stimulus "real-conscious" :kind :user-message
                                      :wait-for-public-result t)
  (crt-check "loaded conscious adapter returns no publication" (null value))
  (crt-check "loaded conscious adapter reports acceptance" (eq :accepted outcome)))
(crt-check "loaded conscious adapter logged the admitted kind"
           (and (= 1 (length *crt-events*))
                (string= "user-message" (gethash "type" (first *crt-events*)))))
(crt-check "loaded conscious adapter calls no provider"
           (zerop *crt-provider-calls*))
(crt-check "loaded conscious adapter calls no effect"
           (zerop *crt-effect-calls*))
(crt-check "loaded conscious adapter calls no publication"
           (zerop *crt-publication-calls*))
(let* ((report (cognition-runtime-report))
       (adapter (gethash "adapter" report))
       (bounds (and (hash-table-p adapter)
                    (gethash "queue_bounds" adapter))))
  (crt-check "loaded conscious report reads Q1 degradation slot"
             (and (hash-table-p adapter)
                  (gethash "degraded" adapter)
                  (string= "barrier-overflow"
                           (gethash "queue_bound_status" adapter ""))))
  (crt-check "loaded conscious report uses its pinned projection bounds"
             (and (hash-table-p bounds)
                  (= 64 (gethash "soft" bounds))
                  (= 256 (gethash "hard" bounds)))))

(format t "~%~d passed, ~d failed~%" *crt-passed* *crt-failed*)
(when (plusp *crt-failed*) (uiop:quit 1))
