;;;; conscious-concern-tests.lisp -- Q1b concern identity and history.
;;;;
;;;; Closes the codelet-context gap: insistence is a fact about repeated
;;;; encounters, which a function of a single stimulus cannot know. The
;;;; property under test is that it is knowable WITHOUT any component holding
;;;; a private counter -- cross-pulse state stays event-derived.

(in-package :agent)

(defvar *cn-passed* 0)
(defvar *cn-failed* 0)

(defun cn-check (name condition)
  (if condition
      (progn (incf *cn-passed*) (format t "PASS ~a~%" name))
      (progn (incf *cn-failed*) (format t "FAIL ~a~%" name))))

(load (test-source "policy.lisp"))
(load (test-source "stimulus.lisp"))
(load (test-source "census.lisp"))
(load (test-source "concern.lisp"))
(load (test-source "codelets.lisp"))
(load (test-source "context.lisp"))
(load (test-source "inbox.lisp"))
(load (test-source "attention.lisp"))

(defun cn-event (type &key id payload (timestamp 1000))
  (obj "id" (or id 1) "type" type "timestamp" timestamp
       "payload" (or payload (obj))))

(defun cn-stimulus (type &key id payload (timestamp 1000))
  (stimulus-from-event (cn-event type :id id :payload payload :timestamp timestamp)))

(format t "~%== concern identity is stable across pulses ==~%")

;; Two results about the same operation are the SAME concern recurring, even
;; though they are different stimuli in different pulses.
(let ((a (cn-stimulus "agent-operation-terminal" :id 1 :payload (obj "operation_id" "op-7")))
      (b (cn-stimulus "agent-operation-terminal" :id 2 :payload (obj "operation_id" "op-7"))))
  (cn-check "same correlation gives the same concern identity"
            (string= (concern-identity a) (concern-identity b))))

(let ((a (cn-stimulus "agent-operation-terminal" :id 1 :payload (obj "operation_id" "op-1")))
      (b (cn-stimulus "agent-operation-terminal" :id 2 :payload (obj "operation_id" "op-2"))))
  (cn-check "different correlations are different concerns"
            (not (string= (concern-identity a) (concern-identity b)))))

;; A one-off with nothing to correlate against must never accumulate
;; insistence -- otherwise unrelated messages would look like one nagging
;; concern, which is the merge-on-weak-identity defect a fourth time.
(let ((a (cn-stimulus "user-message" :id 1))
      (b (cn-stimulus "user-message" :id 2)))
  (cn-check "uncorrelated stimuli are distinct concerns, never merged"
            (not (string= (concern-identity a) (concern-identity b)))))

(let ((a (cn-stimulus "agent-operation-terminal" :id 1 :payload (obj "operation_id" "op-1")))
      (b (cn-stimulus "schedule-fired" :id 2 :payload (obj "schedule_id" "op-1"))))
  (cn-check "identity is scoped by kind, so unrelated ids do not collide"
            (not (string= (concern-identity a) (concern-identity b)))))

(format t "~%== history is derived from events, not counted in a codelet ==~%")

(let* ((events (list (cn-event "agent-operation-terminal" :id 1 :timestamp 100
                               :payload (obj "operation_id" "op-1"))
                     (cn-event "agent-operation-terminal" :id 2 :timestamp 200
                               :payload (obj "operation_id" "op-1"))
                     (cn-event "agent-operation-terminal" :id 3 :timestamp 300
                               :payload (obj "operation_id" "op-1"))))
       (history (concern-history-project events))
       (entry (concern-history-for history
                                   (cn-stimulus "agent-operation-terminal" :id 9
                                                :payload (obj "operation_id" "op-1")))))
  (cn-check "recurrence is counted"
            (= 3 (gethash "observation_count" entry)))
  (cn-check "first observation is retained"
            (eql 100 (gethash "first_observed" entry)))
  (cn-check "last observation advances"
            (eql 300 (gethash "last_observed" entry))))

;; The determinism that makes this safe: the same log always yields the same
;; history, so replay reproduces insistence rather than re-accumulating it.
(let* ((events (list (cn-event "agent-operation-terminal" :id 1 :payload (obj "operation_id" "op"))
                     (cn-event "agent-operation-terminal" :id 2 :payload (obj "operation_id" "op"))))
       (a (concern-history-project events))
       (b (concern-history-project events)))
  (cn-check "history projection is deterministic"
            (= (gethash "concern_count" a) (gethash "concern_count" b))))

(let* ((events (list (cn-event "user-message" :id 1)))
       (sizes (mapcar #'hash-table-count events)))
  (concern-history-project events)
  (cn-check "projecting does not mutate source events"
            (equal sizes (mapcar #'hash-table-count events))))

(format t "~%== unavailable is distinguishable from zero ==~%")

;; A consumer must be able to tell "never selected" from "selection is not
;; tracked yet". Those are identical in the data, so completeness is stated.
(let* ((history (concern-history-project (list (cn-event "user-message" :id 1))))
       (completeness (gethash "history_completeness" history)))
  (cn-check "observation is reported as derived"
            (string= "derived" (gethash "observation" completeness)))
  (cn-check "selection is reported unavailable, not silently zero"
            (string= "unavailable-until-q3" (gethash "selection" completeness))))

(let* ((events (list (cn-event "user-message" :id 1)
                     (cn-event "pulse-committed" :id 2
                               :payload (obj "concern_identity" "concern:user-message:x"
                                             "outcome" "selected" "at" 500))))
       (history (concern-history-project events))
       (completeness (gethash "history_completeness" history)))
  (cn-check "once outcome events exist, selection reports derived"
            (string= "derived" (gethash "selection" completeness)))
  (cn-check "and the selection is folded in"
            (= 1 (gethash "selection_count"
                          (gethash "concern:user-message:x" (gethash "concerns" history))))))

(format t "~%== deferral and cooldown fold correctly ==~%")

(let* ((id "concern:tool-result:op-1")
       (events (list (cn-event "concern-deferred" :id 1
                               :payload (obj "concern_identity" id "outcome" "deferred"))
                     (cn-event "concern-deferred" :id 2
                               :payload (obj "concern_identity" id "outcome" "deferred"))
                     (cn-event "pulse-committed" :id 3
                               :payload (obj "concern_identity" id "outcome" "selected" "at" 900))))
       (entry (gethash id (gethash "concerns" (concern-history-project events)))))
  (cn-check "consecutive deferrals accumulate"
            (numberp (gethash "consecutive_deferrals" entry)))
  (cn-check "and selection resets them -- fatigue is about being passed over, not about age"
            (zerop (gethash "consecutive_deferrals" entry)))
  (cn-check "last selected is recorded"
            (eql 900 (gethash "last_selected" entry))))

(let* ((id "concern:x")
       (events (list (cn-event "concern-presented" :id 1
                               :payload (obj "concern_identity" id "outcome" "presented"
                                             "cooldown_until" 5000))))
       (entry (gethash id (gethash "concerns" (concern-history-project events)))))
  (cn-check "a cooldown deadline is retained for fatigue control"
            (eql 5000 (gethash "cooldown_until" entry))))

(format t "~%== a missing concern reads as nothing-known, never NIL ==~%")

(let ((entry (concern-history-for (obj) (cn-stimulus "user-message" :id 1))))
  (cn-check "an absent projection yields a blank entry, not NIL"
            (and entry (zerop (gethash "observation_count" entry))))
  (cn-check "and the blank entry still names its concern"
            (stringp (gethash "concern_identity" entry))))

(format t "~%== codelets propose transitions, they never write them ==~%")

(let ((tr (make-concern-transition :concern-identity "concern:a" :kind "cooldown"
                                   :reason "presented-recently" :cooldown-until 900)))
  (cn-check "a transition is inert on construction"
            (null (gethash "materialized" tr))))

;; A codelet's proposed transition is re-keyed to the concern of the stimulus
;; it was actually assessing, so it cannot reach into another concern's state.
(dolist (n (codelet-names)) (unregister-codelet n))
(register-codelet
 "insistence" 1
 (lambda (s ctx) (declare (ignore ctx))
   (make-assessment :codelet "insistence" :concern "c"
                    :stimulus-id (gethash "stimulus_id" s)
                    :priority-class "ambient" :urgency "background"
                    :explanation-code "e"
                    :transitions (list (make-concern-transition
                                        :concern-identity "concern:SOMEONE-ELSE"
                                        :kind "cooldown" :reason "r"))))
                    :digest "insistence-fixture-v1")
(let* ((inbox (inbox-project (list (cn-event "user-message" :id 1)) :now 2000))
       (a (aref (attention-assess (coerce (gethash "admitted" inbox) 'list)) 0))
       (tr (aref (gethash "transitions" a) 0)))
  (cn-check "a proposed transition survives normalization"
            (string= "cooldown" (gethash "transition" tr)))
  (cn-check "but is re-keyed to the assessed stimulus's own concern"
            (not (string= "concern:SOMEONE-ELSE" (gethash "concern_identity" tr))))
  (cn-check "and remains unmaterialized -- attention applies nothing"
            (null (gethash "materialized" tr))))

(format t "~%== the context carries history for codelets to read ==~%")

(let* ((events (list (cn-event "agent-operation-terminal" :id 1 :payload (obj "operation_id" "op"))
                     (cn-event "agent-operation-terminal" :id 2 :payload (obj "operation_id" "op"))))
       (ctx (make-projection-context :now 2000
                                     :concern-history (concern-history-project events))))
  (cn-check "context exposes the concern history"
            (= 1 (gethash "concern_count" (gethash "concern_history" ctx))))
  (cn-check "and the report states its completeness"
            (hash-table-p (gethash "history_completeness" (projection-context-report ctx)))))

;; Concern history is composition-independent DATA, not policy: two contexts
;; differing only in history must still be comparable, or every new event
;; would look like a rules change.
(let ((a (make-projection-context :now 2000))
      (b (make-projection-context
          :now 2000
          :concern-history (concern-history-project (list (cn-event "user-message" :id 1))))))
  (cn-check "history does not alter the composition hash"
            (string= (projection-context-hash a) (projection-context-hash b))))

(format t "~%== Q1g: the window is measured in pulses, not selections ==~%")

;; Review: truncating the most recent selection timestamps implements
;; "last 20 selections", not "selections during the last 20 pulses". A concern
;; selected once and then passed over for a thousand pulses kept counting that
;; selection, so fatigue never decayed. The two definitions diverge exactly
;; when a concern goes quiet -- the case the window exists for.
(let* ((id "concern:internal:op")
       (events (list (cn-event "pulse-committed" :id 1
                               :payload (obj "concern_identity" id "outcome" "selected"
                                             "at" 100 "pulse_sequence" 1))
                     ;; ...many pulses later, this concern never selected again
                     (cn-event "pulse-committed" :id 2
                               :payload (obj "concern_identity" "concern:other"
                                             "outcome" "selected"
                                             "at" 900 "pulse_sequence" 500))))
       (history (concern-history-project events))
       (entry (gethash id (gethash "concerns" history))))
  (cn-check "a selection far outside the window ages out"
            (zerop (gethash "selection_count" entry)))
  (cn-check "and the latest pulse sequence is recorded"
            (= 500 (gethash "latest_pulse_sequence" history))))

(let* ((id "concern:internal:op")
       (events (list (cn-event "pulse-committed" :id 1
                               :payload (obj "concern_identity" id "outcome" "selected"
                                             "at" 100 "pulse_sequence" 10))
                     (cn-event "pulse-committed" :id 2
                               :payload (obj "concern_identity" id "outcome" "selected"
                                             "at" 200 "pulse_sequence" 12))))
       (entry (gethash id (gethash "concerns" (concern-history-project events)))))
  (cn-check "selections inside the window are retained"
            (= 2 (gethash "selection_count" entry))))

;; Without pulse sequences the window cannot decay, and the projection says so
;; rather than implying a decay that is not happening.
(let* ((history (concern-history-project
                 (list (cn-event "pulse-committed" :id 1
                                 :payload (obj "concern_identity" "c" "outcome" "selected")))))
       (completeness (gethash "history_completeness" history)))
  (cn-check "window basis reports unavailable without pulse sequences"
            (string= "unavailable-until-q3" (gethash "window_basis" completeness))))

(format t "~%== Q1g: history is built under the pinned admission policy ==~%")

;; Review: concern history called the global admission policy, so insistence
;; could be accumulated from a different classification than the inbox saw --
;; and a payload-journalized event would still contaminate it.
(let* ((events (list (cn-event "schedule-fired" :id 1
                               :payload (obj "mode" "notify" "schedule_id" "s"))))
       (history (concern-history-project events)))
  (cn-check "a payload-journalized event contributes no concern history"
            (zerop (gethash "concern_count" history))))

(let* ((events (list (cn-event "user-message" :id 1)))
       (empty-policy (make-hash-table :test #'equal))
       (history (concern-history-project events :kind-map empty-policy)))
  (cn-check "history honours a supplied admission policy over the global"
            (zerop (gethash "concern_count" history))))

(format t "~%CONSCIOUS CONCERN TESTS: ~d passed, ~d failed.~%"
        *cn-passed* *cn-failed*)
(when (plusp *cn-failed*) (uiop:quit 1))
