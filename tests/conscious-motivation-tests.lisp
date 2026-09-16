(in-package :agent)

(dolist (file '("policy.lisp" "stimulus.lisp" "census.lisp"
                "codelets.lisp" "context.lisp" "mind/conscious/lifecycle.lisp"
                "lifecycle-semantics.lisp" "motivation.lisp"))
  (load (test-source file)))

(defvar *q5m-pass* 0)
(defvar *q5m-fail* 0)

(defun q5m-check (name condition)
  (if condition
      (progn (incf *q5m-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *q5m-fail*) (format t "  FAIL ~a~%" name))))

(defun q5m-event (id type payload &optional (caused-by :null))
  (obj "schema_version" 1 "id" id "type" type "payload" payload
       "agent_id" "default" "timestamp" id "caused_by" caused-by
       "tick_id" :null "affect_snapshot" :null))

(defun q5m-evidence (id)
  (q5m-event id "fixture-evidence" (obj "evidence_id" id)))

(defun q5m-observation (id request refs kind roots at &optional (label "Topic"))
  (q5m-event
   id "conscious-curiosity-observed"
   (conscious-curiosity-observation-payload
    :request-id request :mind-identity-id "dev-persona"
    :subject-type "topic" :subject-label label :subject-refs refs
    :reinforcement-kind kind :supporting-event-ids roots
    :source-revision "q5m-fixture-v1"
    :actor-runtime-revision "q5m-runtime-v1" :observed-at at)))

(defun q5m-row (projection motive-id)
  (find motive-id (coerce (gethash "motives" projection) 'list)
        :key (lambda (row) (gethash "motive_id" row)) :test #'string=))

(format t "~%== Q5M curiosity semantic accumulation ==~%")

(let* ((first (q5m-observation
               2 "obs-1" '("topic:b" "topic:a")
               "novel-observation" '(1) 100))
       (motive-id (gethash "motive_id" (gethash "payload" first)))
       ;; Same evidence and semantic identity, but a new request and label.
       (overlap (q5m-observation
                 3 "obs-overlap" '("topic:a" "topic:b")
                 "unresolved-recurrence" '(1) 110 "Renamed topic"))
       (events
         (vector
          (q5m-evidence 1) first overlap
          (q5m-evidence 4)
          (q5m-observation 5 "obs-2" '("topic:a" "topic:b")
                           "operator-interest" '(4) 200)
          (q5m-evidence 6)
          (q5m-observation 7 "obs-3" '("topic:a" "topic:b")
                           "unresolved-recurrence" '(6) 300)
          (q5m-evidence 8)
          (q5m-observation 9 "obs-other" '("topic:other")
                           "novel-observation" '(8) 310 "Similar words")))
       (projection (conscious-motivation-project
                    events :now 400 :agent-id "default"))
       (row (q5m-row projection motive-id))
       (candidates (conscious-motivation-candidates projection)))
  (q5m-check "independent recurrence coalesces into one reinforced motive"
             (and (= 2 (gethash "motive_count" projection))
                  (= 3 (gethash "reinforcement_count" row))))
  (q5m-check "overlapping evidence neither reinforces nor replaces label"
             (and (= 660 (gethash "activation_milliunits" row))
                  (string= "Topic" (gethash "subject_label" row))))
  (q5m-check "different stable subject references remain separate"
             (= 2 (length (remove-duplicates
                           (map 'list (lambda (item)
                                        (gethash "motive_id" item))
                                (gethash "motives" projection))
                           :test #'string=))))
  (q5m-check "only the salient motive emits one candidate"
             (and (string= "salient" (gethash "phase" row))
                  (= 1 (length candidates))))
  (let ((candidate (aref candidates 0)))
    (q5m-check "candidate contains identity/evidence but no subject prose"
               (and (string= motive-id (gethash "motive_id" candidate))
                    (plusp (length (gethash "source_event_ids" candidate)))
                    (not (nth-value 1 (gethash "subject_label" candidate)))
                    (not (nth-value 1 (gethash "subject_refs" candidate))))))
  (q5m-check "restart replay is canonical under the same composition"
             (string= (%stimulus-canonical-json projection)
                      (%stimulus-canonical-json
                       (conscious-motivation-project
                        events :now 400 :agent-id "default"))))

  (let* ((blocked-event
           (q5m-event
            11 "conscious-curiosity-opportunity-observed"
            (conscious-curiosity-opportunity-payload
             :request-id "opp-blocked" :motive-id motive-id
             :mind-identity-id "dev-persona" :opportunity-state "unsuitable"
             :reason-code "operator-conversation-active"
             :supporting-event-ids '(10)
             :actor-runtime-revision "q5m-runtime-v1" :observed-at 410)))
         (blocked-events
           (concatenate 'vector events (vector (q5m-evidence 10) blocked-event)))
         (blocked (conscious-motivation-project
                   blocked-events :now 410 :agent-id "default")))
    (q5m-check "unsuitable opportunity inhibits without a candidate"
               (and (string= "inhibited"
                             (gethash "phase" (q5m-row blocked motive-id)))
                    (zerop (length (conscious-motivation-candidates blocked)))))
    (let* ((ready-event
             (q5m-event
              13 "conscious-curiosity-opportunity-observed"
              (conscious-curiosity-opportunity-payload
               :request-id "opp-ready" :motive-id motive-id
               :mind-identity-id "dev-persona" :opportunity-state "suitable"
               :reason-code "private-cognition-slot"
               :supporting-event-ids '(12)
               :actor-runtime-revision "q5m-runtime-v1" :observed-at 420)))
           (ready-events
             (concatenate 'vector blocked-events
                          (vector (q5m-evidence 12) ready-event)))
           (ready (conscious-motivation-project
                   ready-events :now 420 :agent-id "default")))
      (q5m-check "suitable opportunity makes a salient motive ready"
                 (string= "ready-for-opportunity"
                          (gethash "phase" (q5m-row ready motive-id))))
      (let* ((fake
               (q5m-event
                14 "conscious-curiosity-satisfaction-observed"
                (conscious-curiosity-satisfaction-payload
                 :request-id "sat-fake" :motive-id motive-id
                 :mind-identity-id "dev-persona" :degree "full"
                 :receipt-event-id 999
                 :actor-runtime-revision "q5m-runtime-v1" :observed-at 500)))
             (fake-result
               (conscious-motivation-project
                (concatenate 'vector ready-events (vector fake))
                :now 500 :agent-id "default")))
        (q5m-check "fabricated satisfaction is invalid and changes no phase"
                   (and (= 1 (gethash "invalid_count" fake-result))
                        (string= "ready-for-opportunity"
                                 (gethash "phase"
                                          (q5m-row fake-result motive-id))))))
        (let* ((source (q5m-evidence 15))
               (open (q5m-event
                      16 "conscious-lifecycle-transition"
                      (conscious-lifecycle-transition-payload
                       "curiosity-work" "open" :request-id "life-open"
                       :lifecycle-kind "private-exploration"
                       :origin-runtime-revision "q5m-runtime-v1"
                       :actor-runtime-revision "q5m-runtime-v1"
                       :source-event-id 15 :reason-code "curiosity-engaged"
                       :occurred-at 600)))
               (complete (q5m-event
                          17 "conscious-lifecycle-transition"
                          (conscious-lifecycle-transition-payload
                           "curiosity-work" "complete"
                           :request-id "life-complete"
                           :lifecycle-kind "private-exploration"
                           :origin-runtime-revision "q5m-runtime-v1"
                           :actor-runtime-revision "q5m-runtime-v1"
                           :source-event-id 15 :reason-code "answer-inspected"
                           :occurred-at 700)))
               (satisfaction
                 (q5m-event
                  18 "conscious-curiosity-satisfaction-observed"
                  (conscious-curiosity-satisfaction-payload
                   :request-id "sat-valid" :motive-id motive-id
                   :mind-identity-id "dev-persona" :degree "full"
                   :receipt-event-id 17
                   :actor-runtime-revision "q5m-runtime-v1" :observed-at 700)))
               (satisfied-events
                 (concatenate 'vector ready-events
                              (vector source open complete satisfaction)))
               (at-receipt (conscious-motivation-project
                            satisfied-events :now 700 :agent-id "default"))
               (refractory (conscious-motivation-project
                            satisfied-events :now 800 :agent-id "default")))
          (q5m-check "legal lifecycle completion supports satisfaction"
                     (string= "satisfied"
                              (gethash "phase"
                                       (q5m-row at-receipt motive-id))))
          (q5m-check "satisfaction becomes refractory and suppresses candidate"
                     (and (string= "refractory"
                                   (gethash "phase"
                                            (q5m-row refractory motive-id)))
                          (zerop (length
                                  (conscious-motivation-candidates
                                   refractory)))))
          (q5m-check "refractory expiry returns to latent"
                     (string= "latent"
                              (gethash
                               "phase"
                               (q5m-row
                                (conscious-motivation-project
                                 satisfied-events :now 5000
                                 :agent-id "default")
                                motive-id))))))))

(let* ((context (make-projection-context :now 400 :agent-id "default"))
       (changed (copy-tree *curiosity-motivation-policy*)))
  (setf (cdr (assoc :salient-threshold changed)) 900)
  (q5m-check "motivation policy changes the pinned composition hash"
             (not (string=
                   (projection-context-hash context)
                   (projection-context-hash
                    (make-projection-context
                     :now 400 :agent-id "default"
                     :motivation-policy changed))))))

(let ((bad (conscious-motivation-project
            (vector (q5m-event 1 "conscious-curiosity-observed"
                               (obj "schema_version" 1)))
            :now 1 :agent-id "default")))
  (q5m-check "malformed source produces bounded diagnostic, not a motive"
             (and (zerop (gethash "motive_count" bad))
                  (= 1 (gethash "invalid_count" bad))
                  (equalp #(1) (gethash "invalid_event_ids" bad)))))

(let* ((observed
         (q5m-observation 2 "review-observation" '("topic:review")
                          "novel-observation" '(1) 100 "Review topic"))
       (motive-id (gethash "motive_id" (gethash "payload" observed)))
       (result
         (q5m-event
          3 "recursive-curiosity-result"
          (obj "schema_version" 1 "source_motive_ids" (vector motive-id)
               "status" "completed" "audience" "private")))
       (opened
         (q5m-event
          4 "recursive-curiosity-result-review-opened"
          (obj "schema_version" 1 "result_event_id" 3
               "runtime_revision" "q5m-runtime-v1" "opened_at" 110)
          3))
       (completed
         (q5m-event
          5 "recursive-curiosity-result-review-completed"
          (obj "schema_version" 1 "result_event_id" 3
               "disposition" "closed"
               "source_motive_ids" (vector motive-id)
               "new_motive_id" :null
               "runtime_revision" "q5m-runtime-v1" "completed_at" 120)
          4))
       (satisfaction
         (q5m-event
          6 "conscious-curiosity-satisfaction-observed"
          (conscious-curiosity-satisfaction-payload
           :request-id "review-satisfaction" :motive-id motive-id
           :mind-identity-id "dev-persona" :degree "full"
           :receipt-event-id 5
           :actor-runtime-revision "q5m-runtime-v1" :observed-at 120)
          5))
       (valid-events
         (vector (q5m-evidence 1) observed result opened completed satisfaction))
       (valid (conscious-motivation-project
               valid-events :now 120 :agent-id "default"))
       (out-of-order
         (conscious-motivation-project
          (vector (q5m-evidence 1) observed result opened satisfaction completed)
          :now 120 :agent-id "default")))
  (q5m-check "closed recursive result review is a satisfaction receipt"
             (and (zerop (gethash "invalid_count" valid))
                  (string= "full"
                           (gethash "satisfaction_state"
                                    (q5m-row valid motive-id)))))
  (q5m-check "result review receipt must precede satisfaction durably"
             (and (= 1 (gethash "invalid_count" out-of-order))
                  (string= "none"
                           (gethash "satisfaction_state"
                                    (q5m-row out-of-order motive-id))))))

(let* ((policy (copy-tree *curiosity-motivation-policy*))
       (ignored (setf (cdr (assoc :motive-bound policy)) 2))
       (first
         (q5m-observation 2 "retire-first" '("topic:first")
                          "novel-observation" '(1) 100 "First"))
       (first-id (gethash "motive_id" (gethash "payload" first)))
       (result
         (q5m-event 3 "recursive-curiosity-result"
                    (obj "schema_version" 1
                         "source_motive_ids" (vector first-id)
                         "status" "completed" "audience" "private")))
       (review-opened
         (q5m-event 4 "recursive-curiosity-result-review-opened"
                    (obj "schema_version" 1 "result_event_id" 3
                         "runtime_revision" "q5m-runtime-v1" "opened_at" 110)
                    3))
       (review-completed
         (q5m-event 5 "recursive-curiosity-result-review-completed"
                    (obj "schema_version" 1 "result_event_id" 3
                         "disposition" "closed"
                         "source_motive_ids" (vector first-id)
                         "new_motive_id" :null
                         "runtime_revision" "q5m-runtime-v1"
                         "completed_at" 120)
                    4))
       (satisfaction
         (q5m-event
          6 "conscious-curiosity-satisfaction-observed"
          (conscious-curiosity-satisfaction-payload
           :request-id "retire-satisfaction" :motive-id first-id
           :mind-identity-id "dev-persona" :degree "full"
           :receipt-event-id 5 :actor-runtime-revision "q5m-runtime-v1"
           :observed-at 120)
          5))
       (second
         (q5m-observation 8 "retire-second" '("topic:second")
                          "novel-observation" '(7) 130 "Second"))
       (third
         (q5m-observation 10 "retire-third" '("topic:third")
                          "novel-observation" '(9) 140 "Third"))
       (second-id (gethash "motive_id" (gethash "payload" second)))
       (third-id (gethash "motive_id" (gethash "payload" third)))
       (projection
         (conscious-motivation-project
          (vector (q5m-evidence 1) first result review-opened review-completed
                  satisfaction (q5m-evidence 7) second (q5m-evidence 9) third)
          :now 140 :agent-id "default" :policy policy)))
  (declare (ignore ignored))
  (q5m-check "capacity retires oldest fully satisfied motive without loss"
             (and (= 2 (gethash "motive_count" projection))
                  (= 1 (gethash "retired_motive_count" projection))
                  (equalp (vector first-id)
                          (gethash "retired_motive_ids" projection))
                  (null (q5m-row projection first-id))
                  (q5m-row projection second-id)
                  (q5m-row projection third-id)
                  (zerop (gethash "invalid_count" projection)))))

(let* ((policy (copy-tree *curiosity-motivation-policy*))
       (ignored (setf (cdr (assoc :motive-bound policy)) 2))
       (first
         (q5m-observation 2 "retire-latent-first" '("topic:latent-first")
                          "novel-observation" '(1) 100 "Latent first"))
       (second
         (q5m-observation 4 "retire-latent-second" '("topic:latent-second")
                          "novel-observation" '(3) 110 "Latent second"))
       (third
         (q5m-observation 6 "retire-latent-third" '("topic:latent-third")
                          "novel-observation" '(5) 120 "Latent third"))
       (first-id (gethash "motive_id" (gethash "payload" first)))
       (second-id (gethash "motive_id" (gethash "payload" second)))
       (third-id (gethash "motive_id" (gethash "payload" third)))
       (projection
         (conscious-motivation-project
          (vector (q5m-evidence 1) first (q5m-evidence 3) second
                  (q5m-evidence 5) third)
          :now 120 :agent-id "default" :policy policy)))
  (declare (ignore ignored))
  (q5m-check "capacity falls back to oldest lowest non-salient motive"
             (and (= 2 (gethash "motive_count" projection))
                  (= 1 (gethash "retired_motive_count" projection))
                  (equalp (vector first-id)
                          (gethash "retired_motive_ids" projection))
                  (null (q5m-row projection first-id))
                  (q5m-row projection second-id)
                  (q5m-row projection third-id)
                  (zerop (gethash "invalid_count" projection)))))

(format t "~%~d passed, ~d failed~%" *q5m-pass* *q5m-fail*)
(when (plusp *q5m-fail*)
  (error "Q5M curiosity projection tests failed"))
