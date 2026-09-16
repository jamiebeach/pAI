;;;; Focused temporal-continuity plumbing checks. Load after the full system.
;;;; harness: full-system
(in-package :agent)

(let ((checks 0))
  (labels ((check (value)
             (incf checks)
             (unless value
               (error "continuity capsule check ~d failed" checks)))
           (event (id type at)
             (obj "id" id "type" type "agent_id" "mind:test"
                  "timestamp" at "payload" (obj))))
    (check (string= "less than a minute"
                    (continuity-capsule-format-elapsed 0)))
    (check (string= "1 minute" (continuity-capsule-format-elapsed 60)))
    (check (string= "2 hours" (continuity-capsule-format-elapsed 7200)))
    (check (string= "2 days" (continuity-capsule-format-elapsed 172800)))

    (clrhash *continuity-capsule-contributors*)
    (continuity-capsule-register-contributor
     "later" (lambda (request owner-context)
               (declare (ignore request owner-context)) (vector))
     :order 200 :revision "v1")
    (continuity-capsule-register-contributor
     "grounded"
     (lambda (request owner-context)
       (declare (ignore owner-context))
       (check (null (gethash "events" request)))
       (vector (obj "kind" "fixture-state" "status" "present"
                    "source_id" 1 "observed_at" 100
                    "content" "A grounded fixture state remains present.")))
     :order 100 :revision "v1")
    ;; Reload-safe replacement retains one registration.
    (continuity-capsule-register-contributor
     "later" (lambda (request owner-context)
               (declare (ignore request owner-context)) (error "fixture"))
     :order 200 :revision "v2")
    (let* ((report (continuity-capsule-contributor-report))
           (registered (gethash "contributors" report)))
      (check (= 2 (gethash "contributor_count" report)))
      (check (string= "grounded" (gethash "name" (aref registered 0))))
      (check (string= "v2" (gethash "revision" (aref registered 1)))))

    (let* ((events (list (event 1 "agent-message" 100)
                         (event 2 "user-message" 3700)))
           (before (shasht:write-json events nil))
           (capsule
             (continuity-capsule-build
              :mind-identity-id "mind:test" :as-of 3700
              :clock-identity "fixture-clock"
              :boundary-kind "operator-conversation"
              :boundary-source-id 2
              :time-context "Current local time: fixture."
              :events events))
           (rows (gethash "contributions" capsule))
           (context (continuity-capsule-context-records capsule)))
      (check (string= "partial" (gethash "status" capsule)))
      (check (= 2 (length rows)))
      (check (= 1 (length (gethash "errors" capsule))))
      (check (= 2 (gethash "source_id" (aref rows 0))))
      (check (= 1 (gethash "source_id" (aref rows 1))))
      (check (search "Current local time: fixture"
                     (gethash "content" (aref context 0))))
      (check (search "A grounded fixture state"
                     (gethash "content" (aref context 1))))
      (check (string= before (shasht:write-json events nil))))

    (clrhash *continuity-capsule-contributors*)
    (continuity-capsule-register-contributor
     "required-broken"
     (lambda (request owner-context)
       (declare (ignore request owner-context)) (error "required fixture"))
     :required t)
    (let ((capsule
            (continuity-capsule-build
             :mind-identity-id "mind:test" :as-of 3700
             :clock-identity "fixture-clock" :boundary-kind "private"
             :boundary-source-id 2 :time-context "Current time: fixture."
             :events (list (event 2 "user-message" 3700)))))
      (check (string= "unavailable" (gethash "status" capsule)))
      (check (search "coverage: unavailable"
                     (gethash "content"
                              (aref (gethash "contributions" capsule) 0)))))

    ;; The production contributor and ordinary cognition context compose
    ;; automatically; the model does not need to invoke INSPECT-ATTENTION.
    (clrhash *continuity-capsule-contributors*)
    (conscious-recursive-mind-configure
     :agent-id "mind:test"
     :endpoint "http://127.0.0.1:1234/v1/chat/completions"
     :model "fixture" :context-profile (obj))
    (let ((report (continuity-capsule-contributor-report)))
      (check (= 3 (gethash "contributor_count" report)))
      (check (string= "durable-dialogue"
                      (gethash "name"
                               (aref (gethash "contributors" report) 0))))
      (check (string= "maintained-work"
                      (gethash "name"
                               (aref (gethash "contributors" report) 1))))
      (check (string= "recursive-cognition"
                      (gethash "name"
                               (aref (gethash "contributors" report) 2)))))
    (let* ((events (list (event 1 "agent-message" 100)
                          (event 2 "user-message" 3700)))
           (saved (symbol-function '%recursive-thread-events))
           (saved-recent
             (symbol-function 'event-recent-conversation-events))
           (projection-reads 0)
           (*conscious-recursive-mind-agent-id* "mind:test"))
      (unwind-protect
           (progn
              (setf (symbol-function '%recursive-thread-events)
                    (lambda () (incf projection-reads) events)
                    (symbol-function 'event-recent-conversation-events)
                    (lambda (before-event-id limit)
                      (declare (ignore before-event-id limit)) events))
              (let ((records
                      (conscious-recursive-private-cognition-context-records
                      :as-of 3700 :clock-identity "fixture-clock"
                      :boundary-kind "operator-conversation"
                      :boundary-source-id 2
                      :time-context "Current local time: fixture.")))
               (check (= 2 (length records)))
               (check (search "Temporal orientation"
                              (gethash "content" (aref records 0))))
               (check (search "last durable agent reply was 1 hour"
                              (gethash "content" (aref records 1))))
               (check (not (search "inspect-attention"
                                   (gethash "content" (aref records 1))))))
             (check (= 1 projection-reads))
              (check (zerop
                      (length
                       (conscious-recursive-private-cognition-context-records)))))
        (setf (symbol-function '%recursive-thread-events) saved
              (symbol-function 'event-recent-conversation-events)
              saved-recent)))

    ;; The conscious checkpoint may retain only an identity/time stub for a
    ;; recursive reply. Dialogue authority still owns the exact recent event,
    ;; so its source can contribute without weakening checkpoint compaction or
    ;; recursive recovery boundaries.
    (let* ((projection-events
             (list (event 20 "projection-accounted" 100)
                   (event 21 "user-message" 3700)))
           (dialogue-events (list (event 20 "agent-message" 100)))
           (saved-recent
             (symbol-function 'event-recent-conversation-events))
           (*conscious-recursive-mind-agent-id* "mind:test"))
      (unwind-protect
           (progn
             (setf (symbol-function 'event-recent-conversation-events)
                   (lambda (before-event-id limit)
                     (declare (ignore before-event-id limit)) dialogue-events))
             (let* ((records
                      (conscious-recursive-private-cognition-context-records
                       :events projection-events
                       :as-of 3700 :clock-identity "fixture-clock"
                       :boundary-kind "operator-conversation"
                       :boundary-source-id 21
                       :time-context "Current local time: fixture."))
                    (orientation (aref records 0))
                    (reply (aref records 1)))
               (check (= 2 (length records)))
                (check (search "3 registered sources, 1 available contribution"
                               (gethash "content" orientation)))
               (check (search "last durable agent reply was 1 hour"
                              (gethash "content" reply)))))
        (setf (symbol-function 'event-recent-conversation-events)
              saved-recent)))

    ;; Production event timestamps are ISO strings, not fixture integers.
    (let* ((boundary-at (%event-parse-ts-string "2026-09-13T16:00:00Z"))
           (events (list (event 10 "agent-message" "2026-09-13T15:00:00Z")
                         (event 11 "user-message" "2026-09-13T16:00:00Z")))
           (*conscious-recursive-mind-agent-id* "mind:test")
           (rows (%recursive-continuity-capsule-contributions
                  (obj "as_of" boundary-at "boundary_source_id" 11
                       "mind_identity_id" "mind:test")
                  events)))
      (check (= 1 (length rows)))
      (check (search "last durable agent reply was 1 hour"
                     (gethash "content" (aref rows 0)))))

    ;; Intervening private cognition is supplied automatically and only as
    ;; bounded lifecycle metadata; private model content is not copied into
    ;; the capsule.
    (let* ((events
             (list
              (event 30 "user-message" 100)
              (obj "id" 31 "type" "model-response"
                   "agent_id" "mind:test" "timestamp" 220
                   "payload"
                   (obj "thread_id" "thread:curiosity:mind:test:30"
                        "status" "accepted"
                        "assistant_message"
                        (obj "role" "assistant"
                             "content" "private fixture content")))
              (event 32 "user-message" 400)))
           (*conscious-recursive-mind-agent-id* "mind:test")
           (rows (%recursive-continuity-capsule-contributions
                  (obj "as_of" 400 "boundary_source_id" 32
                       "mind_identity_id" "mind:test")
                  events))
           (activity
             (find "intervening-private-activity" rows :test #'string=
                   :key (lambda (row) (gethash "kind" row "")))))
      (check activity)
      (check (= 31 (gethash "source_id" activity)))
      (check (search "1 model response" (gethash "content" activity)))
      (check (not (search "private fixture content"
                          (gethash "content" activity)))))

    (check
     (handler-case
         (progn
           (continuity-capsule-build
            :mind-identity-id "mind:test" :as-of 1
            :clock-identity "fixture" :boundary-kind "private"
            :boundary-source-id 99 :time-context "Current time: fixture."
            :events (list (event 1 "user-message" 1)))
           nil)
       (error () t))))
  (format t "~&PASS: ~d focused continuity capsule checks.~%" checks))
