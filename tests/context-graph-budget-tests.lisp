;;;; harness: full-system
(in-package :agent)

(let* ((policy (obj "authorization_id" "fixture-auth" "agent_id" "fixture-agent"
                    "persona_id" "fixture-persona" "generation" "fixture-generation"
                    "prior_exposure_microusd" 100 "ceiling_microusd" 1000
                    "per_request_ceiling_microusd" 100))
       (events #()))
  (labels ((event (id type &rest fields)
             (obj "id" id "type" type "agent_id" "fixture-agent" "caused_by" 1
                  "payload" (apply #'obj "persona_id" "fixture-persona" "generation" "fixture-generation"
                                   "authorization_id" "fixture-auth" fields)))
           (project () (context-graph-budget-project events policy)))
    (setf events (vector
      (event 2 "context-graph-identity-phase" "record_json"
             (pai.context-graph:context-graph-runtime-json
              (obj "phase" "facts" "outcome" "request" "request_digest" "legacy" "reserved_microusd" 50)))
      (event 3 "context-graph-budget-reserved" "reservation_id" "lab-one" "request_digest" "selected" "reserved_microusd" 60)))
    (assert (= 210 (gethash "exposure_microusd" (project))))
    (assert (= 2 (gethash "pending_count" (project))))
    (let ((bad (pai.context-graph::%cg-detach events)))
      (remhash "reserved_microusd" (gethash "payload" (aref bad 1)))
      (assert (handler-case (progn (context-graph-budget-project bad policy) nil) (error () t))))
    (let ((bad (pai.context-graph::%cg-detach events)))
      (setf (gethash "authorization_id" (gethash "payload" (aref bad 1))) "other-authority")
      (assert (handler-case (progn (context-graph-budget-project bad policy) nil) (error () t))))
    ;; A verified settlement replaces its reservation; it never adds a second charge.
    (setf events (concatenate 'vector events
      (vector (event 4 "context-graph-budget-settled" "reservation_id" "lab-one"
                     "request_digest" "selected" "charged_microusd" 20))))
    (assert (= 170 (gethash "exposure_microusd" (project))))
    (assert (= 1 (gethash "pending_count" (project))))
    (let ((bad (pai.context-graph::%cg-detach events)))
      (setf (gethash "request_digest" (gethash "payload" (aref bad 2))) "wrong")
      (assert (handler-case (progn (context-graph-budget-project bad policy) nil) (error () t))))
    (setf (gethash "charged_microusd" (gethash "payload" (aref events 2))) 70)
    (assert (eq :true (gethash "poisoned" (project))))
    (assert (= 220 (gethash "exposure_microusd" (project))))
    (let ((duplicate (pai.context-graph::%cg-detach (aref events 2))))
      (setf (gethash "id" duplicate) 5)
      (assert (handler-case
                  (progn (context-graph-budget-project (concatenate 'vector events (vector duplicate)) policy) nil)
                (error () t))))))
(format t "GRAPH-BUDGET combined exposure, unknown reservations, settlement replacement, mismatch and overrun gates passed~%")

(uiop:call-with-temporary-file
 (lambda (path)
   (let ((backend (make-sqlite-storage path))
         (policy (obj "authorization_id" "scan-auth" "agent_id" "scan-agent"
                      "persona_id" "scan-persona" "generation" "scan-generation"
                      "prior_exposure_microusd" 100 "ceiling_microusd" 1000
                      "per_request_ceiling_microusd" 100)))
     (unwind-protect
          (progn
            (storage-append-event backend "unrelated" (obj) :agent-id "other-agent")
            (storage-append-event backend "context-graph-budget-reserved"
                                  (obj "authorization_id" "scan-auth" "persona_id" "scan-persona"
                                       "generation" "scan-generation" "reservation_id" "one"
                                       "request_digest" "digest-one" "reserved_microusd" 60)
                                  :agent-id "scan-agent")
            ;; Global head includes unrelated partitions; filtered last row does not.
            (storage-append-event backend "unrelated" (obj) :agent-id "other-agent")
            (multiple-value-bind (projection head events) (context-graph-budget-snapshot backend policy)
              (assert (= 160 (gethash "exposure_microusd" projection)))
              (assert (= 1 (length events)))
              (assert (= head (storage-head-position backend)))
              (storage-append-event backend "unrelated" (obj) :agent-id "other-agent")
              (assert (handler-case
                          (progn (storage-append-event-if-head backend head "fixture-denied" (obj)) nil)
                        (storage-conflict-error () t)))))
       (storage-close backend))))
 :want-stream-p nil :type "sqlite")
(format t "GRAPH-BUDGET bounded verified snapshot uses global head; intervening write refuses stale admission~%")

(uiop:call-with-temporary-file
 (lambda (path)
   (let ((first (make-sqlite-storage path)) (second nil)
         (policy (obj "authorization_id" "atomic-auth" "agent_id" "atomic-agent"
                      "persona_id" "atomic-persona" "generation" "atomic-generation"
                      "prior_exposure_microusd" 0 "ceiling_microusd" 100
                      "per_request_ceiling_microusd" 100)))
     (unwind-protect
          (progn
            (setf second (make-sqlite-storage path))
            (let* ((start (sb-thread:make-semaphore :count 0))
                   (results (make-array 2 :initial-element :pending))
                   (threads
                     (loop for backend in (list first second) for index from 0 collect
                       (let ((target backend) (slot index))
                         (sb-thread:make-thread
                          (lambda ()
                            (sb-thread:wait-on-semaphore start)
                            (setf (aref results slot)
                                  (handler-case
                                      (progn (context-graph-budget-reserve target policy
                                               (format nil "attempt-~d" slot) "exact-request" "review" 60)
                                             :accepted)
                                    (error () :refused)))))))))
              (sb-thread:signal-semaphore start 2)
              (dolist (thread threads) (sb-thread:join-thread thread :timeout 10 :default :timeout))
              (assert (= 1 (count :accepted results)))
              (assert (= 1 (count :refused results)))
              (let ((winner (format nil "attempt-~d" (position :accepted results))))
                ;; Simulate a crash/unknown provider outcome: reopen, do not settle.
                (storage-close second)
                (setf second (make-sqlite-storage path))
                (assert (= 60 (gethash "exposure_microusd" (context-graph-budget-snapshot second policy))))
                (assert (handler-case
                            (progn (context-graph-budget-reserve second policy winner "exact-request" "review" 60) nil)
                          (error () t)))
                (assert (handler-case
                            (progn (context-graph-budget-settle second policy winner "different" 20 "verified-one") nil)
                          (error () t)))
                (context-graph-budget-settle second policy winner "exact-request" 20 "verified-one")
                (assert (= 20 (gethash "exposure_microusd" (context-graph-budget-snapshot first policy))))
                (assert (handler-case
                            (progn (context-graph-budget-settle first policy winner "exact-request" 20 "verified-one") nil)
                          (error () t)))
                (context-graph-budget-reserve first policy "overrun-attempt" "next-request" "facts" 60)
                (context-graph-budget-settle second policy "overrun-attempt" "next-request" 70 "verified-two")
                (assert (eq :true (gethash "poisoned" (context-graph-budget-snapshot first policy))))
                (assert (handler-case
                            (progn (context-graph-budget-reserve first policy "after-overrun" "third" "facts" 1) nil)
                          (error () t))))))
       (when second (storage-close second))
       (storage-close first))))
 :want-stream-p nil :type "sqlite")
(format t "GRAPH-BUDGET atomic admission, crash persistence, no resend, exact settlement and overrun refusal passed~%")

(uiop:call-with-temporary-file
 (lambda (path)
   (let ((backend (make-sqlite-storage path))
         (policy (obj "authorization_id" "mixed-auth" "agent_id" "mixed-agent"
                      "persona_id" "mixed-persona" "generation" "mixed-generation"
                      "prior_exposure_microusd" 0 "ceiling_microusd" 100
                      "per_request_ceiling_microusd" 100)))
     (unwind-protect
          (labels ((append-phase (outcome amount)
                     (context-graph-budget-append-phase backend policy
                       (obj "persona_id" "mixed-persona" "generation" "mixed-generation"
                            "record_json" (pai.context-graph:context-graph-runtime-json
                              (obj "phase" "facts" "request_digest" "native-digest" "outcome" outcome
                                   (if (equal outcome "request") "reserved_microusd" "charged_microusd") amount))) 1))
                   (exposure () (gethash "exposure_microusd" (context-graph-budget-snapshot backend policy))))
            (context-graph-budget-reserve backend policy "lab-attempt" "lab-digest" "review" 60)
            (assert (handler-case (progn (append-phase "request" 60) nil) (error () t)))
            (assert (= 60 (exposure)))
            (append-phase "request" 40)
            (assert (= 100 (exposure)))
            (append-phase "paused" 0)
            (assert (= 60 (exposure)))
            (append-phase "request" 40)
            (append-phase "rejected" 10)
            (append-phase "request" 30)
            ;; Preserve the earlier rejected charge, not just the latest phase.
            (assert (= 100 (exposure)))
            (append-phase "response" 20)
            (assert (= 90 (exposure)))
            (assert (handler-case (progn (append-phase "request" 1) nil) (error () t))))
       (storage-close backend))))
 :want-stream-p nil :type "sqlite")
(format t "GRAPH-BUDGET native/lab shared cap, pause/resume, retained rejected charge and no double reservation passed~%")

(uiop:call-with-temporary-file
 (lambda (path)
   (let ((backend (make-sqlite-storage path))
         (policy (obj "authorization_id" "lineage-auth" "agent_id" "lineage-agent"
                      "persona_id" "lineage-persona" "generation" "lineage-generation"
                      "prior_exposure_microusd" 0 "ceiling_microusd" 100
                      "per_request_ceiling_microusd" 100)))
     (unwind-protect
          (progn
            ;; Another explicit authorization is outside this ledger and ignored.
            (storage-append-event backend "context-graph-budget-reserved"
                                  (obj "authorization_id" "other-auth" "persona_id" "other-persona"
                                       "generation" "other-generation" "reservation_id" "other"
                                       "request_digest" "other-digest" "reserved_microusd" 100)
                                  :agent-id "lineage-agent")
            (assert (= 0 (gethash "exposure_microusd"
                                  (context-graph-budget-snapshot backend policy))))
            ;; The same grant reused under another generation must refuse, not disappear.
            (storage-append-event backend "context-graph-budget-reserved"
                                  (obj "authorization_id" "lineage-auth" "persona_id" "lineage-persona"
                                       "generation" "wrong-generation" "reservation_id" "wrong"
                                       "request_digest" "wrong-digest" "reserved_microusd" 10)
                                  :agent-id "lineage-agent")
            (assert (handler-case
                        (progn (context-graph-budget-snapshot backend policy) nil)
                      (error () t))))
       (storage-close backend))))
 :want-stream-p nil :type "sqlite")
(format t "GRAPH-BUDGET foreign grants are isolated; same authorization outside its lineage refuses~%")
(format t "PASS context-graph-budget-tests~%")
