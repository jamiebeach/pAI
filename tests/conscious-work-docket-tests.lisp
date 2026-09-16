;;;; Focused event-derived work docket checks. Load after the full system.
;;;; harness: full-system
(in-package :agent)

(let ((checks 0))
  (labels ((check (value) (incf checks) (assert value))
           (event (id type payload &optional caused-by)
             (obj "id" id "type" type "agent_id" "mind:test"
                  "caused_by" (or caused-by :null) "payload" payload))
           (opened (id work-id at)
             (event id "conscious-work-docket-opened"
                    (obj "schema_version" 1 "work_id" work-id
                         "title" "Maintain a useful project"
                         "purpose" "Carry useful work beyond one turn."
                         "operator_benefit" "The project continues without repeated prompting."
                         "next_step" "Inspect the current implementation evidence."
                         "priority" "normal"
                         "authority" "private-cognition-existing-authority"
                         "source_event_ids" (vector 1)
                         "opened_at" at "next_eligible_at" at)
                    1))
           (transition (id work-id state note next at)
             (event id "conscious-work-docket-transitioned"
                    (obj "schema_version" 1 "work_id" work-id
                         "state" state "note" note "next_step" next
                         "next_eligible_at" at "observed_at" at)
                    1)))
    (let* ((events (list (opened 2 "work:test" 100)
                         (transition 3 "work:test" "waiting"
                                     "Evidence inspected."
                                     "Run one focused qualification." 200)))
           (report (conscious-work-docket-project events :agent-id "mind:test"))
           (row (aref (gethash "items" report) 0)))
      (check (= 1 (gethash "item_count" report)))
      (check (string= "waiting" (gethash "state" row)))
      (check (= 2 (gethash "revision" row)))
      (check (= 200 (gethash "last_progress_at" row)))
      (check (string= "Run one focused qualification."
                      (gethash "next_step" row)))
      ;; Rebuilding the same event stream yields the same JSON projection.
      (check (string=
              (shasht:write-json report nil)
              (shasht:write-json
               (conscious-work-docket-project events :agent-id "mind:test") nil))))

    (let* ((high (opened 4 "work:high" 90))
           (high-payload (gethash "payload" high))
           (events (list (opened 2 "work:normal" 50) high)))
      (setf (gethash "priority" high-payload) "high")
      (let* ((report (conscious-work-docket-project events :agent-id "mind:test"))
             (rows (%work-docket-items (gethash "items" report)))
             (selected (find "high" rows
                             :key (lambda (row) (gethash "priority" row))
                             :test #'string=)))
        (check (string= "work:high" (gethash "work_id" selected)))))

    (let* ((focus-payload
             (obj "schema_version" 1 "work_id" "work:test"
                  "work_revision" 2 "title" "Maintain a useful project"
                  "purpose" "Carry useful work beyond one turn."
                  "operator_benefit" "The project continues."
                  "next_step" "Run one focused qualification."
                  "authority" "private-cognition-existing-authority"
                  "source_event_ids" (vector 1)
                  "runtime_revision" "work-docket-v1" "opened_at" 300))
           (events (list (event 10 "recursive-work-docket-focus-opened"
                                focus-payload 3)))
           (projection
             (conscious-recursive-thread-project events 10 "mind:test")))
      (check (string= "work-docket" (gethash "root_kind" projection)))
      (check (string= "private" (gethash "channel" projection)))
      (check (string= "work:test" (gethash "work_id" projection)))
      (check (string= "model-ready" (gethash "state" projection))))

    (let* ((thread-id "thread:work-docket:mind:test:10")
           (focus-payload
             (obj "schema_version" 1 "work_id" "work:test"
                  "work_revision" 2 "title" "Maintain a useful project"
                  "purpose" "Carry useful work beyond one turn."
                  "operator_benefit" "The project continues."
                  "next_step" "Run one focused qualification."
                  "authority" "private-cognition-existing-authority"
                  "source_event_ids" (vector 1)
                  "runtime_revision" "work-docket-v1" "opened_at" 300))
           (events
             (list
              (event 10 "recursive-work-docket-focus-opened" focus-payload 3)
              (event 11 "model-request"
                     (obj "thread_id" thread-id
                          "model_call_id" "model:10:1") 10)
              (event 12 "model-response"
                     (obj "thread_id" thread-id
                          "model_call_id" "model:10:1" "status" "accepted"
                          "assistant_message"
                          (obj "role" "assistant" "content" "Bounded result.")
                          "usage" (obj)) 10)
              (event 13 "recursive-work-docket-result"
                     (obj "schema_version" 1 "thread_id" thread-id
                          "work_id" "work:test"
                          "model_call_id" "model:10:1"
                          "runtime_revision" "conscious-recursive-mind-v6"
                          "status" "completed" "audience" "private"
                          "content" "Bounded result." "completed_at" 301)
                     10)))
           (projection
             (conscious-recursive-thread-project events 10 "mind:test")))
      (check (string= "done" (gethash "state" projection)))
      (check (string= "Bounded result." (gethash "content" projection)))
      (check (= 13 (gethash "agent_event_id" projection))))

    (let* ((schemas (%recursive-tool-schemas nil nil nil))
           (names (loop for schema across schemas
                        collect (gethash "name" (gethash "function" schema)))))
      (check (member "inspect-work-docket" names :test #'string=))
      (check (member "manage-work-docket" names :test #'string=)))

    (check (%recursive-private-root-p "work-docket"))
    (check (not (%recursive-private-root-p "conversation")))

    (check (member "recursive-work-docket-result"
                   *dashboard-activity-event-types* :test #'string=))
    (check (string= "private_cognition"
                    (%dashboard-event-category
                     "recursive-work-docket-focus-opened")))
    (check (search "id=\"work-docket\""
                   (uiop:read-file-string
                    (merge-pathnames
                     "src/adapters/web/assets/observability.html"
                     *pai-root*)))))
  (format t "~&PASS: ~d focused work-docket checks.~%" checks))
