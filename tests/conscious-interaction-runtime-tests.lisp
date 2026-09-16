;;;; conscious-interaction-runtime-tests.lisp -- durable serialized ingress.
;;;; Failing probes precede the interaction coordinator implementation.

(in-package :agent)

(ql:quickload '(:bordeaux-threads) :silent t)

(defvar *cir-pass* 0)
(defvar *cir-fail* 0)
(defvar *cir-events* nil)
(defvar *cir-next-id* 0)
(defvar *cir-active* 0)
(defvar *cir-maximum-active* 0)
(defvar *cir-executed* nil)
(defparameter *agent-id* "interaction-fixture")
(defvar *public-inbound-channel* "internal")
(defvar *cir-event-lock* (bt:make-lock "interaction fixture events"))
(defvar *cir-delay-first-admission-p* nil)

(defun cir-check (name value)
  (if value
      (progn (incf *cir-pass*) (format t "PASS ~a~%" name))
      (progn (incf *cir-fail*) (format t "FAIL ~a~%" name))))

(defun cir-event (type payload &key caused-by)
  (obj "schema_version" 1 "id" (incf *cir-next-id*)
       "timestamp" (+ 1000 *cir-next-id*) "type" type
       "agent_id" *agent-id* "payload" payload
       "caused_by" (or caused-by :null)))

(defun replay-events (&rest ignored)
  (declare (ignore ignored))
  (bt:with-lock-held (*cir-event-lock*)
    (copy-list *cir-events*)))

(defun log-event (type payload &key caused-by)
  (bt:with-lock-held (*cir-event-lock*)
    (let ((event (cir-event type payload :caused-by caused-by)))
      (setf *cir-events* (append *cir-events* (list event)))
      (values (gethash "id" event) t event))))

(defun submit-stimulus (prompt &key kind metadata wait-for-public-result)
  (declare (ignore kind wait-for-public-result))
  (let ((event (log-event
                "user-message"
                (obj "text" prompt "channel" *public-inbound-channel*
                     "metadata" metadata))))
    (when (and *cir-delay-first-admission-p* (string= prompt "slow-first"))
      (sleep 0.08))
    (values nil :accepted event)))

(format t "~%== durable per-mind interaction coordinator ==~%")
(load (test-source "interaction-runtime.lisp"))

(let ((web (uiop:read-file-string (test-source "web-terminal.lisp")))
      (legacy-web (uiop:read-file-string (test-source "web.lisp")))
      (driver (uiop:read-file-string
               (merge-pathnames "scripts/conscious-conversation.lisp"
                                *pai-root*))))
  (cir-check "web ingress delegates through an explicit selected-mind seam"
             (and (search "web-terminal-configure-submit" web)
                  (null (search "conscious-interaction-admit" web))))
  (cir-check "web slash commands use a deterministic runtime seam"
             (and (search "web-terminal-configure-command" web)
                  (search "*v2-command-fn*" web)
                  (search "#'%conversation-run-web-command" driver)
                  (search "conscious-recursive-curiosity-inspect" driver)
                  (search "conscious-recursive-conversation-episode-graph-inspect"
                          driver)
                  (search "/graph-inspect" driver)
                  (search "conscious-recursive-curiosity-supersede-finding" driver)
                  (search "conscious-recursive-session-budget-add" driver)
                  (search "/budget-add USD" driver)
                  (search "Private cost share (~d%)" driver)
                  (search "pending_generation_settlement_count" driver)
                  (search "pending_generation_fallback_usd" driver)
                  (search "Admission remains paused until exact accounting or fallback settlement" driver)
                  (null (search "Private cost share (~d%%)" driver))
                  (null (search "/budget-add REQUESTS USD" driver))))
  (cir-check "web ingress no longer creates one thread per message"
             (null (search ":name \"v2-turn\"" web)))
  (cir-check "legacy chat runtime and routes are deleted"
             (and (null (search "chat-turn" legacy-web))
                  (null (search "api/chat" legacy-web))
                  (null (search "last-self-mod-history" web))
                  (null (search "pai-base-run-self-mod-messages" web))
                  (null (search "register-layer pai-turn-log" web))
                  (search "v2-root :uri \"/\"" web)))
  (cir-check "cold history reads only exact pAI conversation events"
             (and (search "replay-events" web)
                  (search "recursive-mind-v1" web)
                  (search "q4.5-conversation" web)
                  (search "user-message" web)
                  (search "agent-message" web)))
  (cir-check "canonical CLI waits through the same coordinator"
             (search "conscious-interaction-submit-and-wait" driver))
  (cir-check "private cognition cannot own the operator progress slot"
             (search
              "(unless (string= \"private\" (gethash \"channel\" item \"\"))"
              driver))
  (cir-check "quiet cognition does not depend on presentation progress"
             (null (search "(not *conversation-progress-active-p*)"
                           driver))))

(defun cir-reset ()
  (ignore-errors (conscious-interaction-stop))
  (setf *cir-events* nil *cir-next-id* 0 *cir-active* 0
        *cir-maximum-active* 0 *cir-executed* nil)
  (conscious-interaction-reset-for-tests)
  (conscious-interaction-configure
   :agent-id *agent-id* :queue-capacity 8
   :metadata-fn (lambda () (obj "source" "q4.5-conversation"))
   :executor-fn
   (lambda (prompt &key admitted-event-id channel interaction-id)
     (declare (ignore channel))
     (bt:with-lock-held (*conscious-interaction-lock*)
       (incf *cir-active*)
       (setf *cir-maximum-active* (max *cir-maximum-active* *cir-active*)))
     (sleep 0.02)
     (bt:with-lock-held (*conscious-interaction-lock*)
       (push (list interaction-id prompt admitted-event-id) *cir-executed*)
       (decf *cir-active*))
     (obj "schema_version" 1 "status" "replied"
          "content" (format nil "reply:~a" prompt)
          "agent_event_id" (+ 100 admitted-event-id)))))

(cir-reset)
(conscious-interaction-start)
(let* ((one (conscious-interaction-admit "one" :channel "web"))
       (two (conscious-interaction-admit "two" :channel "web"))
       (three (conscious-interaction-admit "three" :channel "web"))
       (results (mapcar (lambda (receipt)
                          (conscious-interaction-wait
                           (gethash "interaction_id" receipt) :timeout 3))
                        (list one two three))))
  (cir-check "three rapid messages are durably accepted"
             (every (lambda (receipt)
                      (member (gethash "status" receipt)
                              '("accepted" "queued") :test #'string=))
                    (list one two three)))
  (cir-check "one inference is active for the mind"
             (= 1 *cir-maximum-active*))
  (cir-check "rapid messages execute once in admission order"
             (equal '("one" "two" "three")
                    (mapcar #'second (reverse *cir-executed*))))
  (cir-check "all waiters receive their own reply"
             (equal '("reply:one" "reply:two" "reply:three")
                    (mapcar (lambda (result) (gethash "content" result))
                            results))))
(conscious-interaction-stop)

;; The first durable append is deliberately delayed before it can enqueue.
;; A second request must not overtake it at the worker boundary.
(cir-reset)
(conscious-interaction-start)
(let ((receipts (make-hash-table :test #'equal)))
  (setf *cir-delay-first-admission-p* t)
  (unwind-protect
  (let ((first
          (bt:make-thread
           (lambda ()
             (setf (gethash "first" receipts)
                   (conscious-interaction-admit "slow-first" :channel "web")))))
        (second nil))
    (sleep 0.02)
    (setf second
          (bt:make-thread
           (lambda ()
             (setf (gethash "second" receipts)
                   (conscious-interaction-admit "fast-second" :channel "web")))))
    (bt:join-thread first)
    (bt:join-thread second)
    (dolist (key '("first" "second"))
      (conscious-interaction-wait
       (gethash "interaction_id" (gethash key receipts)) :timeout 3))
    (cir-check "concurrent admissions execute in durable ledger order"
               (equal '("slow-first" "fast-second")
                      (mapcar #'second (reverse *cir-executed*)))))
    (setf *cir-delay-first-admission-p* nil)))
(conscious-interaction-stop)

;; A crash after durable model-request is not safe to infer again. Recovery
;; must terminalize it as outcome-unknown while still draining an unclaimed
;; later admission.
(cir-reset)
(let* ((first (conscious-interaction-admit "uncertain" :channel "web"))
       (first-id (gethash "user_event_id" first))
       (first-interaction (gethash "interaction_id" first)))
  ;; Keep the worker stopped and create the exact crash boundary.
  (log-event "conscious-interaction-claimed"
             (obj "interaction_id" first-interaction
                  "user_event_id" first-id "attempt" 1)
             :caused-by first-id)
  (log-event "model-request" (obj "model_call_id" "fixture-call")
             :caused-by first-id)
  (let* ((second (conscious-interaction-admit "pending" :channel "web"))
         (second-interaction (gethash "interaction_id" second)))
    (setf *cir-executed* nil)
    (conscious-interaction-recover)
    (conscious-interaction-start)
    (let ((uncertain (conscious-interaction-wait first-interaction :timeout 2))
          (pending (conscious-interaction-wait second-interaction :timeout 2)))
      (cir-check "provider-start crash becomes explicit outcome unknown"
                 (string= "outcome-unknown" (gethash "status" uncertain)))
      (cir-check "outcome-unknown interaction is never blindly re-executed"
                 (not (find "uncertain" *cir-executed* :key #'second
                            :test #'string=)))
      (cir-check "unclaimed durable admission resumes after restart"
                 (and (string= "replied" (gethash "status" pending))
                      (find "pending" *cir-executed* :key #'second
                            :test #'string=)))))
  (conscious-interaction-stop))

;; A durable pulse commit proves the provider outcome even when the interaction
;; coordinator did not yet reach its terminal. Recovery must resume proposal
;; consumption rather than classify the already-known call as uncertain.
(cir-reset)
(let* ((receipt (conscious-interaction-admit "committed-proposal" :channel "web"))
       (user-id (gethash "user_event_id" receipt))
       (interaction-id (gethash "interaction_id" receipt)))
  (log-event "conscious-interaction-claimed"
             (obj "interaction_id" interaction-id
                  "user_event_id" user-id "attempt" 1)
             :caused-by user-id)
  (log-event "model-request"
             (obj "model_call_id" "known-call" "pulse_id" "pulse:known"
                  "interaction_id" interaction-id)
             :caused-by user-id)
  (log-event "pulse-committed"
             (obj "pulse_id" "pulse:known" "work_id" "work:known"
                  "proposals"
                  (vector (obj "proposal_id" "known:p1"
                               "kind" "request-continuation")))
             :caused-by user-id)
  (setf *cir-executed* nil)
  (conscious-interaction-recover)
  (conscious-interaction-start)
  (let ((result (conscious-interaction-wait interaction-id :timeout 2)))
    (cir-check "committed provider outcome resumes interaction consumption"
               (and (string= "replied" (gethash "status" result ""))
                    (find "committed-proposal" *cir-executed*
                          :key #'second :test #'string=))))
  (conscious-interaction-stop))

(format t "~%~d passed, ~d failed~%" *cir-pass* *cir-fail*)
(when (plusp *cir-fail*) (error "interaction runtime tests failed"))
