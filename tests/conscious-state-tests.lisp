;;;; conscious-state-tests.lisp -- Q1 conscious-state projection.
;;;;
;;;; Q1's exit condition is "exact rebuild and bounds pass over adversarial
;;;; streams; source events are never lost or rewritten." The rebuild
;;;; assertions below are the load-bearing ones.

(in-package :agent)

(defvar *cs-passed* 0)
(defvar *cs-failed* 0)

(defun cs-check (name condition)
  (if condition
      (progn (incf *cs-passed*) (format t "PASS ~a~%" name))
      (progn (incf *cs-failed*) (format t "FAIL ~a~%" name))))

(load (test-source "policy.lisp"))
(load (test-source "stimulus.lisp"))
(load (test-source "census.lisp"))
(load (test-source "concern.lisp"))
(load (test-source "codelets.lisp"))
(load (test-source "context.lisp"))
(load (test-source "inbox.lisp"))
(load (test-source "attention.lisp"))
(load (test-source "state.lisp"))

(defun cs-event (type &key id payload (timestamp 1000))
  (obj "id" (or id 1) "type" type "timestamp" timestamp
       "payload" (or payload (obj))))

(defun cs-codelets ()
  (dolist (n (codelet-names)) (unregister-codelet n))
  (register-codelet
   "direct-address" 10
   (lambda (s ctx) (declare (ignore ctx))
     (when (string= (gethash "kind" s) "user-message")
       (make-assessment :codelet "direct-address" :concern "operator waiting"
                        :stimulus-id (gethash "stimulus_id" s)
                        :evidence-ids (coerce (gethash "source_event_ids" s) 'list)
                        :priority-class "direct" :urgency "interactive"
                        :explanation-code "user-addressed-agent")))
                    :digest "direct-address-fixture-v1")
  (register-codelet
   "novelty" 40
   (lambda (s ctx) (declare (ignore ctx))
     (when (string= (gethash "kind" s) "project-change")
       (make-assessment :codelet "novelty" :concern "changed"
                        :stimulus-id (gethash "stimulus_id" s)
                        :priority-class "ambient" :urgency "background"
                        :explanation-code "project-changed")))
                    :digest "novelty-fixture-v1")
  (register-codelet
   "cancellation" 5
   (lambda (s ctx) (declare (ignore ctx))
     (when (string= (gethash "kind" s) "cancellation")
       (make-assessment :codelet "cancellation" :concern "cancelled"
                        :stimulus-id (gethash "stimulus_id" s)
                        :priority-class "critical" :urgency "interactive"
                        :explanation-code "operator-cancelled")))
                    :digest "cancellation-fixture-v1"))

(cs-codelets)

(defun cs-stream (n &key (start 1))
  (loop for i from start below (+ start n)
        collect (cs-event "episode-boundary-detected" :id i :timestamp (* i 10)
                          :payload (obj "turn_id" (format nil "t-~a" i)))))

(format t "~%== every slot explains itself ==~%")

(let ((state (conscious-state-project (list (cs-event "user-message" :id 1)) :now 2000)))
  (dolist (slot '("focus" "selection" "secondary" "awaited" "evidence_roots"
                  "sensorium" "interruption" "next_wake" "flags"))
    (let ((s (gethash slot state)))
      (cs-check (format nil "~a carries all four facts" slot)
                (and (hash-table-p s)
                     (nth-value 1 (gethash "provenance" s))
                     (nth-value 1 (gethash "freshness" s))
                     (nth-value 1 (gethash "lifecycle" s))
                     (stringp (gethash "reason" s)))))))

;; A slot that cannot say why it is present must not be constructible.
(cs-check "a slot missing a required fact cannot be built"
          (handler-case (progn (agent::%slot 1 :provenance "p" :freshness "f"
                                             :lifecycle "l")
                               nil)
            (error () t)))

(format t "~%== exact rebuild ==~%")

;; The Q1 exit condition. Same events and same NOW must produce byte-identical
;; state; there is no hidden accumulator to drift.
(let* ((events (append (cs-stream 6) (list (cs-event "user-message" :id 99 :timestamp 900))))
       (a (conscious-state-project events :now 5000))
       (b (conscious-state-project events :now 5000)))
  (cs-check "identical input rebuilds identical state"
            (string= (shasht:write-json a nil) (shasht:write-json b nil))))

;; Codex review: the old version of this projected a prefix and DISCARDED the
;; result, so it only re-ran the same computation and proved nothing about
;; checkpoint parity. Both directions are now asserted.
(let* ((events (append (cs-stream 8) (list (cs-event "user-message" :id 99 :timestamp 900))))
       (prefix (subseq events 0 4))
       (prefix-first (conscious-state-project prefix :now 5000))
       (full-direct (conscious-state-project events :now 5000))
       ;; project the prefix AGAIN after the full stream, and compare
       (prefix-again (conscious-state-project prefix :now 5000))
       (full-again (conscious-state-project events :now 5000)))
  (cs-check "a prefix projects identically before and after the full stream"
            (string= (shasht:write-json prefix-first nil)
                     (shasht:write-json prefix-again nil)))
  (cs-check "the full stream projects identically before and after a prefix"
            (string= (shasht:write-json full-direct nil)
                     (shasht:write-json full-again nil)))
  (cs-check "and prefix and full states genuinely differ, so the test is not vacuous"
            (not (string= (shasht:write-json prefix-first nil)
                          (shasht:write-json full-direct nil)))))

(format t "~%== committed and observation revisions are monotonic ==~%")

(let ((s1 (conscious-state-project (cs-stream 3) :now 5000))
      (s2 (conscious-state-project (cs-stream 6) :now 5000)))
  (cs-check "a superset of events never reports an earlier revision"
            (and (<= (gethash "state_revision" s1)
                     (gethash "state_revision" s2))
                 (<= (gethash "observation_revision" s1)
                     (gethash "observation_revision" s2)))))

(let ((s (conscious-state-project '() :now 5000)))
  (cs-check "an empty stream has revision zero, not an error"
            (eql 0 (gethash "state_revision" s))))

(format t "~%== Q3 observation and commit revisions are distinct ==~%")

(let* ((events (list (cs-event "user-message" :id 1)
                     (cs-event "pulse-failed" :id 2
                               :payload (obj "pulse_id" "pulse:1"
                                             "pulse_sequence" 1))))
       (state (conscious-state-project events :now 2000)))
  (cs-check "a failed pulse advances observation but not committed state"
            (and (numberp (gethash "observation_revision" state))
                 (= 2 (gethash "observation_revision" state))
                 (zerop (gethash "state_revision" state)))))

(let* ((events (list (cs-event "user-message" :id 1)
                     (cs-event "pulse-committed" :id 2
                               :payload (obj "pulse_id" "pulse:1"
                                             "pulse_sequence" 7))))
       (state (conscious-state-project events :now 2000)))
  (cs-check "only a committed pulse advances committed state revision"
            (and (numberp (gethash "observation_revision" state))
                 (= 2 (gethash "observation_revision" state))
                 (= 7 (gethash "state_revision" state)))))

(format t "~%== references, never content ==~%")

;; Payload text must never reach the state. If it did it would escape the
;; per-pulse audience and budget rules that only apply at render time.
(let* ((events (list (cs-event "user-message" :id 1
                               :payload (obj "text" "SECRET-PAYLOAD-MARKER"
                                             "turn_id" "t-1"))))
       (state (conscious-state-project events :now 2000))
       (json (shasht:write-json state nil)))
  (cs-check "stimulus payload text does not appear in state"
            (not (search "SECRET-PAYLOAD-MARKER" json)))
  (cs-check "but its identity is retained as a reference"
            (search "stimulus:1" json)))

(format t "~%== bounds ==~%")

(let* ((events (cs-stream 40))
       (state (conscious-state-project events :now 9000
                                       :secondary-bound 5
                                       :soft-bound 100 :hard-bound 200)))
  (cs-check "secondary items are bounded"
            (<= (length (gethash "value" (gethash "secondary" state))) 5)))

(let* ((events (cs-stream 40))
       (state (conscious-state-project events :now 9000 :secondary-bound 3
                                       :soft-bound 100 :hard-bound 200))
       (focus-key (gethash "coalition_key" (gethash "value" (gethash "focus" state))))
       (secondary-keys (map 'list (lambda (x) (gethash "coalition_key" x))
                            (gethash "value" (gethash "secondary" state)))))
  (cs-check "focus is never duplicated into secondary"
            (not (member focus-key secondary-keys :test #'equal))))

(format t "~%== focus follows attention, and cannot disagree with it ==~%")

(let* ((events (append (cs-stream 20)
                       (list (cs-event "user-message" :id 500 :timestamp 8000))))
       (state (conscious-state-project events :now 9000 :soft-bound 100 :hard-bound 200)))
  (cs-check "a direct address takes focus over twenty ambient items"
            (string= "direct" (gethash "priority_class"
                                       (gethash "value" (gethash "focus" state)))))
  (cs-check "focus reason names the codelet's explanation"
            (string= "user-addressed-agent" (gethash "reason" (gethash "focus" state))))
  (cs-check "selection decision agrees with focus presence"
            (string= "pulse-now"
                     (gethash "decision" (gethash "value" (gethash "selection" state))))))

(let ((state (conscious-state-project '() :now 2000)))
  (cs-check "an empty stream leaves focus idle, not absent"
            (and (eq :null (gethash "value" (gethash "focus" state)))
                 (string= "idle" (gethash "lifecycle" (gethash "focus" state)))
                 (string= "no-eligible-coalition" (gethash "reason" (gethash "focus" state)))))
  (cs-check "and next_wake waits for a new stimulus"
            (string= "new-eligible-stimulus"
                     (gethash "condition" (gethash "value" (gethash "next_wake" state))))))

(format t "~%== awaited is event-derived or honestly empty ==~%")

(let ((state (conscious-state-project (list (cs-event "user-message" :id 1)) :now 2000)))
  (cs-check "awaited reports unavailable while no lifecycle is active"
            (string= "unavailable" (gethash "lifecycle" (gethash "awaited" state))))
  (cs-check "with a reason naming the empty projection"
            (string= "no-active-lifecycle"
                     (gethash "reason" (gethash "awaited" state))))
  (cs-check "and an empty value rather than a guess"
            (zerop (length (gethash "value" (gethash "awaited" state))))))

;; Supplied as DATA on the context, not as a function the projection calls.
(let* ((ctx (make-projection-context
             :now 2000 :lifecycle
             (vector (obj "lifecycle_id" "op-1" "last_event_id" 91))))
       (state (conscious-state-project (list (cs-event "user-message" :id 1)) :context ctx)))
  (cs-check "a lifecycle projection supplied on the context is used"
            (and (string= "open" (gethash "lifecycle" (gethash "awaited" state)))
                 (= 1 (length (gethash "value" (gethash "awaited" state))))))
  (cs-check "and its provenance names the lifecycle event, not a global"
            (equalp (vector 91)
                    (gethash "provenance" (gethash "awaited" state)))))

(format t "~%== interruption and degradation are surfaced ==~%")

(let ((state (conscious-state-project (list (cs-event "turn-cancel-requested" :id 1))
                                      :now 2000)))
  (cs-check "a cancellation is pending in the interruption slot"
            (eq t (gethash "cancellation_pending"
                           (gethash "value" (gethash "interruption" state)))))
  (cs-check "and the slot lifecycle reflects it"
            (string= "pending" (gethash "lifecycle" (gethash "interruption" state)))))

(let ((state (conscious-state-project (list (cs-event "user-message" :id 1)) :now 2000)))
  (cs-check "no cancellation leaves interruption clear"
            (string= "clear" (gethash "lifecycle" (gethash "interruption" state)))))

(let* ((events (loop for i from 1 to 8 collect (cs-event "user-message" :id i)))
       (state (conscious-state-project events :now 2000 :hard-bound 3)))
  (cs-check "barrier overflow raises the degraded flag"
            (eq t (gethash "degraded" (gethash "value" (gethash "flags" state)))))
  (cs-check "and next_wake waits for the degradation to clear"
            (string= "degradation-cleared"
                     (gethash "condition" (gethash "value" (gethash "next_wake" state))))))

(let* ((events (list (cs-event "agent-operation-terminal" :id 1)))
       (state (conscious-state-project events :now 2000 :current-revision "rev-1")))
  (cs-check "unverified-revision items are counted, not hidden"
            (= 1 (gethash "unverified_revision_items"
                          (gethash "value" (gethash "flags" state))))))

(format t "~%== freshness bands ==~%")

(let ((state (conscious-state-project (list (cs-event "user-message" :id 1 :timestamp 1000))
                                      :now 1030)))
  (cs-check "a just-observed stimulus is current"
            (string= "current" (gethash "band" (gethash "freshness" (gethash "focus" state))))))
(let ((state (conscious-state-project (list (cs-event "user-message" :id 1 :timestamp 1000))
                                      :now 100000)))
  (cs-check "a long-past stimulus is stale"
            (string= "stale" (gethash "band" (gethash "freshness" (gethash "focus" state))))))

(format t "~%== purity ==~%")

(let* ((events (append (cs-stream 3) (list (cs-event "user-message" :id 9))))
       (sizes (mapcar #'hash-table-count events)))
  (conscious-state-project events :now 5000)
  (cs-check "projecting does not mutate source events"
            (equal sizes (mapcar #'hash-table-count events))))

(format t "~%== report is content-free ==~%")

(let* ((events (list (cs-event "user-message" :id 1
                               :payload (obj "text" "ANOTHER-SECRET-MARKER"))))
       (state (conscious-state-project events :now 2000))
       (report (conscious-state-report state))
       (json (shasht:write-json report nil)))
  (cs-check "report names the decision"
            (string= "pulse-now" (gethash "decision" report)))
  (cs-check "report distinguishes committed and observed revisions"
            (and (zerop (gethash "state_revision" report))
                 (eql 1 (gethash "observation_revision" report))))
  (cs-check "report leaks no payload content"
            (not (search "ANOTHER-SECRET-MARKER" json))))

(format t "~%== F2: composition is pinned, not read from globals ==~%")

;; The review finding: "same events + same now" was true only while five
;; mutable globals happened not to move. Replaying yesterday's events after a
;; codelet changed would silently produce today's interpretation -- and replay
;; is the primary evidence mechanism for this workstream, so a replay that
;; reinterprets is worse than none.

(let ((state (conscious-state-project (list (cs-event "user-message" :id 1)) :now 2000)))
  (cs-check "state records the composition it was produced under"
            (stringp (gethash "composition_hash" state))))

(let* ((ctx-a (make-projection-context :now 2000))
       (hash-a (projection-context-hash ctx-a)))
  (register-codelet "extra" 99 (lambda (s c) (declare (ignore s c)) nil)
                    :digest "extra-fixture-v1")
  (let ((hash-b (projection-context-hash (make-projection-context :now 2000))))
    (unregister-codelet "extra")
    (cs-check "changing the codelet set changes the composition hash"
              (not (string= hash-a hash-b))))
  (cs-check "and restoring it restores the hash"
            (string= hash-a (projection-context-hash (make-projection-context :now 2000)))))

(let* ((ctx-a (make-projection-context :now 2000))
       (ctx-b (make-projection-context :now 2000 :soft-bound 3)))
  (cs-check "changing a bound changes the composition hash"
            (not (string= (projection-context-hash ctx-a)
                          (projection-context-hash ctx-b)))))

;; The property that makes replay meaningful: a pinned context reproduces its
;; original interpretation even after the live registry has moved on.
(let* ((events (list (cs-event "user-message" :id 1)))
       (pinned (make-projection-context :now 2000))
       (before (conscious-state-project events :context pinned)))
  (dolist (n (codelet-names)) (unregister-codelet n))
  (let ((after (conscious-state-project events :context pinned)))
    (cs-check "a pinned context replays identically after the registry is emptied"
              (string= (shasht:write-json before nil) (shasht:write-json after nil))))
  (let ((live (conscious-state-project events :now 2000)))
    (cs-check "while an unpinned projection correctly reflects the new registry"
              (not (string= (shasht:write-json before nil)
                            (shasht:write-json live nil)))))
  (cs-codelets))

(format t "~%== Q1d: the composition hash covers ALL interpretation policy ==~%")

;; Review demonstration, reproduced as a fixture: mutating a live
;; discriminator changed the same event from non-barrier to barrier under the
;; SAME pinned context, with an unchanged hash. Each policy below is asserted
;; to move the hash, so a change to any of them makes two runs incomparable
;; rather than silently different.
(let ((base (projection-context-hash (make-projection-context :now 2000))))
  (cs-check "changing an admission row changes the hash"
            (let ((km (make-hash-table :test #'equal)))
              (maphash (lambda (k v) (setf (gethash k km) v)) *stimulus-kind-map*)
              (setf (gethash "user-message" km) (list "user-message" "channel" "background" nil))
              (not (string= base (projection-context-hash
                                  (make-projection-context :now 2000 :kind-map km))))))
  (cs-check "changing the discriminator set changes the hash"
            (let ((d (make-hash-table :test #'equal)))
              (not (string= base (projection-context-hash
                                  (make-projection-context :now 2000 :discriminators d))))))
  (cs-check "changing priority classes changes the hash"
            (not (string= base (projection-context-hash
                                (make-projection-context
                                 :now 2000
                                 :priority-classes '(("only" . 0)))))))
  (cs-check "changing urgency ranks changes the hash"
            (not (string= base (projection-context-hash
                                (make-projection-context
                                 :now 2000
                                 :urgency-ranks (obj "interactive" 2 "timely" 1 "background" 0))))))
  (cs-check "changing tie-breaks changes the hash"
            (not (string= base (projection-context-hash
                                (make-projection-context
                                 :now 2000 :tie-breaks '("priority-class"))))))
  (cs-check "changing the normalization version changes the hash"
            (not (string= base (projection-context-hash
                                (make-projection-context :now 2000 :normalization-version 99)))))
  (cs-check "changing a bound changes the hash"
            (not (string= base (projection-context-hash
                                (make-projection-context :now 2000 :soft-bound 3)))))
  (cs-check "changing a codelet DIGEST changes the hash"
            (progn
              (register-codelet "hashprobe" 95 (lambda (s c) (declare (ignore s c)) nil)
                                :digest "v1")
              (let ((h1 (projection-context-hash (make-projection-context :now 2000))))
                (register-codelet "hashprobe" 95 (lambda (s c) (declare (ignore s c)) nil)
                                  :digest "v2")
                (let ((h2 (projection-context-hash (make-projection-context :now 2000))))
                  (unregister-codelet "hashprobe")
                  (not (string= h1 h2))))))
  (cs-check "an unchanged composition still hashes identically"
            (string= base (projection-context-hash (make-projection-context :now 2000)))))

;; The end-to-end property: a pinned context must interpret events by the
;; policy it captured, even after the live globals move.
(let* ((events (list (cs-event "schedule-fired" :id 1
                               :payload (obj "mode" "context" "schedule_id" "s"))))
       (pinned (make-projection-context :now 2000))
       (before (conscious-state-project events :context pinned)))
  (setf (gethash "schedule-fired" *stimulus-discriminators*)
        (lambda (p) (declare (ignore p)) :journal))
  (let ((after-pinned (conscious-state-project events :context pinned))
        (after-live (conscious-state-project events :now 2000)))
    (census-load)
    (cs-check "a pinned context is unaffected by mutating live policy"
              (string= (shasht:write-json before nil)
                       (shasht:write-json after-pinned nil)))
    (cs-check "while an unpinned projection does change, proving the test is not vacuous"
              (not (string= (shasht:write-json before nil)
                            (shasht:write-json after-live nil))))))

(format t "~%== Q1f: pinned policy is EXECUTED, not merely hashed ==~%")

;; Third review, reproduced as a fixture. The context hashed priority classes,
;; urgency ranks and tie-breaks -- and then normalization and selection read
;; the LIVE globals anyway. Mutating the live priority table flipped focus
;; from `direct` to `ambient` under the same pinned context with an unchanged
;; hash. The hash described a policy the code was not running.
;;
;; Two competing coalitions of different classes are required: with a single
;; candidate, rank order cannot change the outcome and the bypass is
;; invisible. My first probe made exactly that mistake and reported the bug
;; as fixed.
(let* ((events (list (cs-event "user-message" :id 1 :timestamp 100)
                     (cs-event "episode-boundary-detected" :id 2 :timestamp 200
                               :payload (obj "turn_id" "t"))))
       (ctx (make-projection-context :now 2000))
       (focus-class (lambda ()
                      (gethash "priority_class"
                               (gethash "value"
                                        (gethash "focus"
                                                 (conscious-state-project events :context ctx))))))
       (before (funcall focus-class))
       (saved *attention-priority-classes*))
  (setf *attention-priority-classes*
        '(("ambient" . 0) ("critical" . 1) ("committed" . 2)
          ("relevant" . 3) ("direct" . 4)))
  (let ((after (funcall focus-class)))
    (setf *attention-priority-classes* saved)
    (cs-check "focus is unchanged when live priority policy is mutated"
              (equal before after))
    (cs-check "and the pinned focus is the one the captured policy implies"
              (string= "direct" before)))
  ;; Not vacuous: a context built from the mutated policy DOES differ.
  (setf *attention-priority-classes*
        '(("ambient" . 0) ("critical" . 1) ("committed" . 2)
          ("relevant" . 3) ("direct" . 4)))
  (let* ((mutated-ctx (make-projection-context :now 2000))
         (mutated (gethash "priority_class"
                           (gethash "value"
                                    (gethash "focus"
                                             (conscious-state-project events :context mutated-ctx))))))
    (setf *attention-priority-classes* saved)
    (cs-check "while a context capturing the mutated policy does change focus"
              (string= "ambient" mutated))))

;; Urgency ranks are likewise read from the capture, not the global.
(let* ((events (list (cs-event "user-message" :id 1 :timestamp 100)))
       (ctx (make-projection-context :now 2000))
       (saved *attention-urgency-rank*)
       (before (shasht:write-json (conscious-state-project events :context ctx) nil)))
  (setf *attention-urgency-rank* (obj "interactive" 9 "timely" 5 "background" 0))
  (let ((after (shasht:write-json (conscious-state-project events :context ctx) nil)))
    (setf *attention-urgency-rank* saved)
    (cs-check "mutating live urgency ranks does not change a pinned projection"
              (string= before after))))

;; A tie-break absent from the captured list cannot decide a selection.
(let* ((events (list (cs-event "episode-boundary-detected" :id 1 :timestamp 100
                               :payload (obj "turn_id" "a"))
                     (cs-event "episode-boundary-detected" :id 2 :timestamp 900
                               :payload (obj "turn_id" "b"))))
       (full (make-projection-context :now 2000))
       (narrowed (make-projection-context
                  :now 2000
                  :tie-breaks '("sole-candidate" "priority-class"
                                "coalition-key-lexical"))))
  (cs-check "with oldest-waiting declared, it decides"
            (string= "oldest-waiting"
                     (gethash "decided_by"
                              (gethash "value"
                                       (gethash "selection"
                                                (conscious-state-project events :context full))))))
  (cs-check "with it withdrawn, selection falls through to a declared rule"
            (string= "coalition-key-lexical"
                     (gethash "decided_by"
                              (gethash "value"
                                       (gethash "selection"
                                                (conscious-state-project events :context narrowed)))))))

(format t "~%CONSCIOUS STATE TESTS: ~d passed, ~d failed.~%"
        *cs-passed* *cs-failed*)
(when (plusp *cs-failed*) (uiop:quit 1))
