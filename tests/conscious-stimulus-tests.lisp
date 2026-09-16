;;;; conscious-stimulus-tests.lisp -- Q1 stimulus admission, fixtures only.

(in-package :agent)

(defvar *stim-passed* 0)
(defvar *stim-failed* 0)

(defun stim-check (name condition)
  (if condition
      (progn (incf *stim-passed*) (format t "PASS ~a~%" name))
      (progn (incf *stim-failed*) (format t "FAIL ~a~%" name))))

(load (test-source "policy.lisp"))
(load (test-source "stimulus.lisp"))
(load (test-source "census.lisp"))

(defun stim-event (type &key (id 100) payload caused-by timestamp tick-id)
  (let ((e (obj "id" id "type" type "timestamp" (or timestamp 1000)
                "payload" (or payload (obj)))))
    (when caused-by (setf (gethash "caused_by" e) caused-by))
    (when tick-id (setf (gethash "tick_id" e) tick-id))
    e))

(format t "~%== admission is an allowlist ==~%")

(stim-check "a declared type is admissible"
            (stimulus-admissible-p "user-message"))
(stim-check "an undeclared journal type is not"
            (not (stimulus-admissible-p "pg-backup")))
(stim-check "timing-trace is journal-only"
            (not (stimulus-admissible-p "timing-trace")))
(stim-check "an unknown future type defaults to exclusion"
            (not (stimulus-admissible-p "some-type-invented-later")))
(stim-check "a non-string type is not admissible"
            (not (stimulus-admissible-p nil)))
(stim-check "journal-only events project to NIL, not a degenerate stimulus"
            (null (stimulus-from-event (stim-event "pg-backup"))))
(stim-check "a non-hash-table input is refused"
            (null (stimulus-from-event "not-an-event")))

(format t "~%== envelope completeness ==~%")

;; Every field the frozen schema names must be PRESENT, even when the legacy
;; envelope cannot supply it. A consumer must never have to distinguish
;; absent-because-unsupported from absent-because-unset.
(let ((s (stimulus-from-event (stim-event "user-message" :id 42))))
  (dolist (field '("schema_version" "stimulus_id" "source_event_ids" "kind"
                   "source" "occurred_at" "observed_at" "agent_id" "audience"
                   "correlation_id" "causation_id" "payload_ref" "trust"
                   "grounding" "urgency_class" "expires_at" "coalescing_key"
                   "barrier" "origin_runtime_revision"))
    (stim-check (format nil "field present: ~a" field)
                (nth-value 1 (gethash field s)))))

(format t "~%== identity and causality ==~%")

(let ((s (stimulus-from-event (stim-event "user-message" :id 42 :caused-by 7))))
  (stim-check "stimulus id derives from the event id"
              (string= "stimulus:42" (gethash "stimulus_id" s)))
  (stim-check "source event id is retained as an immutable root"
              (equalp (vector 42) (gethash "source_event_ids" s)))
  (stim-check "causation carries caused_by"
              (eql 7 (gethash "causation_id" s))))

(let ((s (stimulus-from-event (stim-event "user-message" :id 42))))
  (stim-check "absent caused_by is :null, not NIL"
              (eq :null (gethash "causation_id" s))))

(format t "~%== occurred_at / observed_at separation ==~%")

;; The scheduler is the reason this separation exists: a schedule that fires
;; late is a different fact from one that fires on time, and the gap is
;; information. Freezing it means the gap survives projection.
(let* ((payload (obj "scheduled_for_utc" 500 "fired_at_utc" 900
                     "schedule_id" "sched-1"))
       (s (stimulus-from-event (stim-event "schedule-fired" :payload payload
                                           :timestamp 900))))
  (stim-check "occurred_at is when it was DUE" (eql 500 (gethash "occurred_at" s)))
  (stim-check "observed_at is when it FIRED"   (eql 900 (gethash "observed_at" s)))
  (stim-check "a late schedule preserves the gap"
              (= 400 (- (gethash "observed_at" s) (gethash "occurred_at" s))))
  (stim-check "schedule correlation prefers schedule_id"
              (string= "sched-1" (gethash "correlation_id" s))))

(let ((s (stimulus-from-event (stim-event "user-message" :timestamp 1234))))
  (stim-check "kinds without a distinct due-time set both equal, not one absent"
              (and (eql 1234 (gethash "occurred_at" s))
                   (eql 1234 (gethash "observed_at" s)))))

(format t "~%== urgency is deterministic and source-derived ==~%")

(stim-check "a person waiting is interactive"
            (string= "interactive"
                     (gethash "urgency_class"
                              (stimulus-from-event (stim-event "user-message")))))
(stim-check "an internal advisory signal is background"
            (string= "background"
                     (gethash "urgency_class"
                              (stimulus-from-event (stim-event "prediction-resolved")))))
;; A stimulus must not be able to raise its own authority. Payload content
;; claiming urgency changes nothing, because the class comes from a static
;; table keyed on type.
(let ((s (stimulus-from-event
          (stim-event "prediction-resolved"
                      :payload (obj "urgency_class" "interactive"
                                    "audience" "public"
                                    "trust" "verified")))))
  (stim-check "payload cannot raise its own urgency"
              (string= "background" (gethash "urgency_class" s)))
  (stim-check "payload cannot widen its own audience"
              (string= "operator" (gethash "audience" s)))
  (stim-check "payload cannot assert its own trust"
              (string= "internal" (gethash "trust" s))))

(format t "~%== barriers and coalescing are mutually exclusive ==~%")

(stim-check "a user message is a barrier"
            (eq t (gethash "barrier" (stimulus-from-event (stim-event "user-message")))))
(stim-check "an advisory signal is not a barrier"
            (null (gethash "barrier" (stimulus-from-event (stim-event "prediction-resolved")))))

;; Collapsing a barrier is exactly the loss the flag exists to prevent, so no
;; barrier may carry a coalescing key.
(let ((violations '()))
  (maphash
   (lambda (type spec)
     (declare (ignore spec))
     (let ((s (stimulus-from-event (stim-event type))))
       (when (and s (gethash "barrier" s)
                  (not (eq :null (gethash "coalescing_key" s))))
         (push type violations))))
   *stimulus-kind-map*)
  (stim-check "no barrier stimulus is coalescable"
              (null violations)))

(let ((s (stimulus-from-event
          (stim-event "episode-boundary-detected"
                      :payload (obj "turn_id" "t-9")))))
  (stim-check "a coalescable kind gets a stable key"
              (string= "project-change:t-9" (gethash "coalescing_key" s))))

;; Regression: an uncorrelated advisory stimulus must NOT get a key. An
;; earlier version formatted the key from :NULL, so every uncorrelated event
;; of the same kind shared one key and collapsed into a single survivor --
;; silently discarding unrelated resolutions whose only fault was having
;; nothing to correlate against. Coalescing asserts "this supersedes that";
;; with no correlation there is no basis for the claim.
(let ((s (stimulus-from-event (stim-event "prediction-resolved"))))
  (stim-check "an uncorrelated advisory stimulus is independent, not coalescable"
              (eq :null (gethash "coalescing_key" s))))
(let ((a (stimulus-from-event (stim-event "prediction-resolved" :id 1)))
      (b (stimulus-from-event (stim-event "prediction-resolved" :id 2))))
  (stim-check "two uncorrelated advisories do not share a key"
              (and (eq :null (gethash "coalescing_key" a))
                   (eq :null (gethash "coalescing_key" b)))))

(format t "~%== stale-result safety ==~%")

;; The Q0 freeze: a historical row means UNKNOWN revision. Reading it as
;; CURRENT is the mistake that lets a stale async result pass as fresh.
(let ((s (stimulus-from-event (stim-event "agent-operation-terminal"))))
  (stim-check "missing origin_runtime_revision reads as unknown"
              (eq :null (gethash "origin_runtime_revision" s))))
(let ((s (stimulus-from-event
          (stim-event "agent-operation-terminal"
                      :payload (obj "origin_runtime_revision" "rev-abc")))))
  (stim-check "a stamped revision is preserved verbatim"
              (string= "rev-abc" (gethash "origin_runtime_revision" s))))

(format t "~%== payload_ref generalises artifact_sha256 ==~%")

(let ((s (stimulus-from-event
          (stim-event "agent-operation-terminal"
                      :payload (obj "artifact_sha256" "deadbeef"
                                    "artifact_id" "art-1"
                                    "artifact_version" 3)))))
  (stim-check "content-addressed when a digest exists"
              (and (string= "content-addressed" (gethash "kind" (gethash "payload_ref" s)))
                   (string= "deadbeef" (gethash "sha256" (gethash "payload_ref" s))))))
;; Review: the previous shape emitted a bare {"kind":"inline"} with neither a
;; bounded payload nor a reference -- a consumer holding it could neither
;; retrieve the content nor verify it. It named a category and supplied
;; nothing.
(let* ((s (stimulus-from-event (stim-event "user-message" :id 42
                                           :payload (obj "text" "hello"))))
       (ref (gethash "payload_ref" s)))
  (stim-check "with no artifact digest, the ref points at the source event"
              (and (string= "event-reference" (gethash "kind" ref))
                   (eql 42 (gethash "source_event_id" ref))))
  (stim-check "and carries a digest a later retrieval can verify against"
              (and (stringp (gethash "payload_digest" ref))
                   (plusp (length (gethash "payload_digest" ref)))))
  (stim-check "and states the payload size"
              (and (numberp (gethash "payload_size" ref))
                   (plusp (gethash "payload_size" ref))))
  (stim-check "a small payload is marked bounded"
              (eq t (gethash "bounded" ref))))

;; The digest must actually distinguish payloads, or it verifies nothing.
(let ((a (gethash "payload_digest"
                  (gethash "payload_ref"
                           (stimulus-from-event
                            (stim-event "user-message" :id 1 :payload (obj "text" "one"))))))
      (b (gethash "payload_digest"
                  (gethash "payload_ref"
                           (stimulus-from-event
                            (stim-event "user-message" :id 1 :payload (obj "text" "two")))))))
  (stim-check "different payloads give different digests"
              (not (string= a b))))

(let* ((big (make-string 2000 :initial-element #\x))
       (ref (gethash "payload_ref"
                     (stimulus-from-event
                      (stim-event "user-message" :id 1 :payload (obj "text" big))))))
  (stim-check "a payload past the budget is not marked bounded"
              (null (gethash "bounded" ref))))

(format t "~%== purity ==~%")

;; A projection that mutates its source would corrupt the log it derives from.
(let* ((payload (obj "turn_id" "t-1"))
       (event (stim-event "user-message" :payload payload))
       (before (hash-table-count event))
       (payload-before (hash-table-count payload)))
  (stimulus-from-event event)
  (stim-check "projecting does not mutate the event"
              (= before (hash-table-count event)))
  (stim-check "projecting does not mutate the payload"
              (= payload-before (hash-table-count payload))))

;; Determinism: same input, same output, with no clock or randomness.
(let ((a (stimulus-from-event (stim-event "user-message" :id 5)))
      (b (stimulus-from-event (stim-event "user-message" :id 5))))
  (stim-check "projection is deterministic"
              (and (string= (gethash "stimulus_id" a) (gethash "stimulus_id" b))
                   (equal (gethash "occurred_at" a) (gethash "occurred_at" b))
                   (string= (gethash "urgency_class" a) (gethash "urgency_class" b)))))

(format t "~%== report is inspectable ==~%")

(let ((r (stimulus-kind-map-report)))
  (stim-check "report counts the admitted types"
              (= (hash-table-count *stimulus-kind-map*)
                 (gethash "admitted_event_types" r)))
  (stim-check "report lists kinds"
              (plusp (length (gethash "kinds" r))))
  (stim-check "report rows are sorted for stable diffing"
              (let* ((rows (coerce (gethash "rows" r) 'list))
                     (types (mapcar (lambda (x) (gethash "event_type" x)) rows)))
                (equal types (sort (copy-list types) #'string<)))))

(format t "~%== Q1c: payload discrimination ==~%")

;; Reclassified on review: a delivered scheduler notification is an
;; already-authorized EFFECT, not a cognitive producer. It previously became a
;; schedule-due barrier, keeping a message the agent had ALREADY SENT eligible
;; to trigger another pulse and blocking the consumption watermark until
;; acknowledged. The discriminator now returns :journal for that payload.
(stim-check "a delivered notification produces no stimulus"
            (null (stimulus-from-event
                   (stim-event "schedule-fired"
                               :payload (obj "mode" "notify" "schedule_id" "s1")))))
(let ((pending (stimulus-from-event
                (stim-event "schedule-fired"
                            :payload (obj "mode" "context" "schedule_id" "s2")))))
  (stim-check "a pending context reminder is still a barrier"
              (eq t (gethash "barrier" pending)))
  (stim-check "and carries the pending sub_kind"
              (string= "pending" (gethash "sub_kind" pending))))

;; Failure is not a quieter success.
(let ((ok (stimulus-from-event
           (stim-event "agent-operation-terminal"
                       :payload (obj "status" "succeeded" "operation_id" "op"))))
      (bad (stimulus-from-event
            (stim-event "agent-operation-terminal"
                        :payload (obj "status" "failed" "operation_id" "op")))))
  (stim-check "a succeeded operation is a tool-result"
              (string= "tool-result" (gethash "kind" ok)))
  (stim-check "a failed operation is a tool-failure, a different kind"
              (string= "tool-failure" (gethash "kind" bad)))
  (stim-check "and the failure status is retained as sub_kind"
              (string= "failed" (gethash "sub_kind" bad))))

(let ((unknown (stimulus-from-event
                (stim-event "agent-operation-terminal" :payload (obj "operation_id" "op")))))
  (stim-check "a terminal with no status is a failure, not assumed success"
              (and (string= "tool-failure" (gethash "kind" unknown))
                   (string= "unknown" (gethash "sub_kind" unknown)))))

;; Discrimination must come from the PINNED policy, not a live global. This is
;; the defect that made the previous context pin nothing: mutating the live
;; discriminator changed the interpretation of the same event under the same
;; context, with an unchanged hash.
(let* ((pinned (let ((h (make-hash-table :test #'equal)))
                 (maphash (lambda (k v) (setf (gethash k h) v)) *stimulus-discriminators*)
                 h))
       (before (stimulus-from-event
                (stim-event "schedule-fired" :payload (obj "mode" "notify"))
                :discriminators pinned)))
  (setf (gethash "schedule-fired" *stimulus-discriminators*)
        (lambda (p) (declare (ignore p)) (list "schedule-due" "forced" "interactive" t)))
  (let ((after-pinned (stimulus-from-event
                       (stim-event "schedule-fired" :payload (obj "mode" "notify"))
                       :discriminators pinned))
        (after-live (stimulus-from-event
                     (stim-event "schedule-fired" :payload (obj "mode" "notify")))))
    (census-load)   ; restore the real policy
    (stim-check "a pinned discriminator set is unaffected by mutating the global"
                (and (null before) (null after-pinned)))
    (stim-check "while the live global does change interpretation, proving the test is not vacuous"
                (and after-live (eq t (gethash "barrier" after-live))))))

(format t "~%== Q1c: census additions ==~%")

;; Previously absent, leaving the runtime-health codelet named in spec 11.2
;; with no possible input.
(stim-check "heap pressure is admitted as runtime-health"
            (string= "runtime-health"
                     (gethash "kind" (stimulus-from-event (stim-event "heap-pressure")))))
(stim-check "but a routine heap sample stays journal-only"
            (not (stimulus-admissible-p "heap-health")))
(stim-check "a stalled regulatory loop is runtime-health"
            (string= "runtime-health"
                     (gethash "kind" (stimulus-from-event
                                      (stim-event "modulator-watchdog-triggered")))))
(stim-check "an intention transition is an intention-cue"
            (string= "intention-cue"
                     (gethash "kind" (stimulus-from-event
                                      (stim-event "near-term-intention-transition")))))
(stim-check "an intention transition is a barrier -- a dropped one leaves a promise mistracked"
            (eq t (gethash "barrier" (stimulus-from-event
                                      (stim-event "near-term-intention-transition")))))

(let ((focus
        (stimulus-from-event
         (stim-event "recursive-curiosity-focus-opened"
                     :id 45
                     :payload
                     (obj "motive_kind" "curiosity"
                          "expression_policy" "private-consideration-only")))))
  (stim-check "a chosen recursive curiosity focus is a private background cue"
              (and focus
                   (string= "intention-cue" (gethash "kind" focus))
                   (string= "curiosity" (gethash "sub_kind" focus))
                   (string= "background" (gethash "urgency_class" focus))
                   (not (gethash "barrier" focus)))))

(stim-check "a failed private focus is terminal journal evidence, not a new cue"
            (null
             (stimulus-from-event
              (stim-event "recursive-curiosity-focus-failed" :id 46
                          :payload (obj "focus_event_id" 45
                                        "error_code" "provider-failed")))))

(let ((briefing-failure
        (stimulus-from-event
         (stim-event "recursive-curiosity-briefing-failed" :id 47
                     :payload
                     (obj "source_revision" "private-briefing:fixture"
                          "reason" "provider-or-protocol-failure")))))
  (stim-check "a briefing anomaly is observable runtime health, not a pause"
              (and briefing-failure
                   (string= "runtime-health"
                            (gethash "kind" briefing-failure))
                   (string= "background"
                            (gethash "urgency_class" briefing-failure))
                   (not (gethash "barrier" briefing-failure)))))

(let ((consolidation-failure
        (stimulus-from-event
         (stim-event "recursive-curiosity-consolidation-failed" :id 48
                     :payload
                     (obj "source_revision" "curiosity-consolidation:fixture"
                          "reason" "provider-or-protocol-failure")))))
  (stim-check "a consolidation anomaly is observable without blocking thought"
              (and consolidation-failure
                   (string= "runtime-health"
                            (gethash "kind" consolidation-failure))
                   (string= "background"
                            (gethash "urgency_class" consolidation-failure))
                   (not (gethash "barrier" consolidation-failure)))))

(stim-check "episode sealing journals cannot recursively wake cognition"
            (every (lambda (type)
                     (null (stimulus-from-event
                            (stim-event type :id 49
                                        :payload (obj "schema_version" 1)))))
                   '("conversation-episode-seal-opened"
                     "conversation-episode-sealed"
                     "conversation-episode-seal-failed")))

;; Consuming a stimulus must never produce one.
(stim-check "the consumption event type is never admissible"
            (not (stimulus-admissible-p "stimulus-consumed")))

(format t "~%== Q1c: a stimulus without a usable id is refused ==~%")

;; Previously this produced stimulus:NIL with roots #(NIL) -- non-empty, so
;; the malformed check passed, while violating the stable-id and non-empty
;; causal-root semantics the envelope promises.
(let ((e (obj "type" "user-message" "timestamp" 1000 "payload" (obj))))
  (stim-check "an event with no id yields no stimulus"
              (null (stimulus-from-event e))))
(let ((e (obj "id" "" "type" "user-message" "timestamp" 1000 "payload" (obj))))
  (stim-check "an event with an empty id yields no stimulus"
              (null (stimulus-from-event e))))
(let ((e (obj "id" :null "type" "user-message" "timestamp" 1000 "payload" (obj))))
  (stim-check "an event with a :null id yields no stimulus"
              (null (stimulus-from-event e))))

(format t "~%== the census manifest IS the admission policy ==~%")

;; The previous fixture compared a Lisp constant to a Lisp hash table while
;; being described as comparing document to code. It could not fail on a
;; document edit, on swapping one admitted type for another, or on a changed
;; classification -- only on a count change. The table is now BUILT from the
;; manifest, so agreement is structural and what remains to test is that the
;; build is faithful and fails loudly.

(stim-check "the manifest loaded"
            (and (numberp *census-version*) (plusp (length (census-entries)))))

;; Every :stimulus entry reaches the table; every :journal entry does not.
(let ((wrong '()))
  (dolist (e (census-entries))
    (let* ((type (getf e :type))
           (admitted (nth-value 1 (gethash type *stimulus-kind-map*)))
           (should (eq (getf e :class) :stimulus)))
      (unless (eq (and admitted t) should) (push type wrong))))
  (stim-check "every manifest classification is reflected in the table"
              (null wrong)))

;; The table carries exactly what the manifest declared -- not a default.
(let ((mismatched '()))
  (dolist (e (census-entries))
    (when (eq (getf e :class) :stimulus)
      (let ((row (gethash (getf e :type) *stimulus-kind-map*)))
        (unless (and (equal (first row) (getf e :kind))
                     (equal (second row) (getf e :source))
                     (equal (third row) (getf e :urgency))
                     (eq (and (fourth row) t) (and (getf e :barrier) t)))
          (push (getf e :type) mismatched)))))
  (stim-check "every admitted row matches its manifest entry field for field"
              (null mismatched)))

(stim-check "every entry states a reason, including every exclusion"
            (every (lambda (e) (stringp (getf e :reason))) (census-entries)))

(stim-check "every produced kind is in the spec 9.1 vocabulary declared by the manifest"
            (every (lambda (k) (member k (list "user-message" "channel-state"
                                               "schedule-due" "timer" "tool-result"
                                               "tool-failure" "model-result"
                                               "model-failure" "memory-result"
                                               "intention-cue" "project-change"
                                               "prediction-due" "runtime-health"
                                               "cancellation" "operator-control"
                                               "self-mod-result")
                                         :test #'string=))
                   (stimulus-kinds)))

;; A manifest that admits nothing must fail loudly rather than produce a
;; runtime that silently never wakes -- gotcha 25 applied to the wake path.
(stim-check "a manifest admitting nothing is refused"
            (handler-case
                (let ((tmp (merge-pathnames "empty-census.sexp" (test-state-dir))))
                  (with-open-file (o tmp :direction :output :if-exists :supersede)
                    (write-string "(:census-version 9 :spec-kinds (\"user-message\") :entries ((:type \"x\" :class :journal :reason \"r\")))" o))
                  (census-load tmp)
                  nil)
              (error () t)))
;; Restore the real policy after that probe.
(census-load)

(stim-check "an entry with no reason is refused"
            (handler-case
                (let ((tmp (merge-pathnames "noreason-census.sexp" (test-state-dir))))
                  (with-open-file (o tmp :direction :output :if-exists :supersede)
                    (write-string "(:census-version 9 :spec-kinds (\"user-message\") :entries ((:type \"user-message\" :class :stimulus :kind \"user-message\" :source \"channel\" :urgency \"interactive\" :barrier t)))" o))
                  (census-load tmp)
                  nil)
              (error () t)))
(census-load)

(stim-check "an entry declaring an off-vocabulary kind is refused"
            (handler-case
                (let ((tmp (merge-pathnames "badkind-census.sexp" (test-state-dir))))
                  (with-open-file (o tmp :direction :output :if-exists :supersede)
                    (write-string "(:census-version 9 :spec-kinds (\"user-message\") :entries ((:type \"x\" :class :stimulus :kind \"invented\" :source \"channel\" :urgency \"timely\" :barrier t :reason \"r\")))" o))
                  (census-load tmp)
                  nil)
              (error () t)))
(census-load)

(format t "~%== a discriminator may reclassify a payload as journal ==~%")

;; Q0 classified scheduler notification as an already-authorized EFFECT, not a
;; cognitive producer. Admitting the delivered variant kept it eligible to
;; trigger another pulse and blocked the consumption watermark until
;; acknowledged.
(stim-check "a delivered scheduler notification produces no stimulus at all"
            (null (stimulus-from-event
                   (stim-event "schedule-fired"
                               :payload (obj "mode" "notify" "schedule_id" "s1")))))
(stim-check "while a pending context reminder still does"
            (let ((s (stimulus-from-event
                      (stim-event "schedule-fired"
                                  :payload (obj "mode" "context" "schedule_id" "s2")))))
              (and s (string= "schedule-due" (gethash "kind" s))
                   (eq t (gethash "barrier" s)))))

(format t "~%CONSCIOUS STIMULUS TESTS: ~d passed, ~d failed.~%"
        *stim-passed* *stim-failed*)
(when (plusp *stim-failed*) (uiop:quit 1))
