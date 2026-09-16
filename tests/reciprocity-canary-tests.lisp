(in-package :agent)

(ql:quickload '(:bordeaux-threads :shasht) :silent t)

(defvar *reciprocity-test-pass* 0)
(defvar *reciprocity-test-fail* 0)
(defvar *reciprocity-test-deliveries* nil)
(defvar *reciprocity-test-events* nil)
(defvar *reciprocity-canary-mode* :shadow)
(defvar *autonomous-write-mode* :normal)
(defvar *initiative-policy-mode* :shadow)
(defvar *initiative-delivery-mode* :shadow)

(defun reciprocity-test-check (name condition)
  (if condition
      (progn (incf *reciprocity-test-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *reciprocity-test-fail*) (format t "  FAIL ~a~%" name))))

(setf (fdefinition 'log-event)
      (lambda (type payload &key caused-by)
        (declare (ignore caused-by))
        (push (list type payload) *reciprocity-test-events*)))

(load (test-source "candidate-representation.lisp"))
(load (test-source "reciprocity-canary.lisp"))

(setf *reciprocity-canary-file* #P"/tmp/reciprocity-canary-test.json"
      *reciprocity-canary-delivery-fn*
      (lambda (content decision-id now)
        (push (list content decision-id now) *reciprocity-test-deliveries*)
        (obj "status" "delivered" "reason" "fixture-transport-returned"
             "delivery_receipt_id" (format nil "delivery-~a" decision-id))))

(defun reciprocity-test-reset (&optional (mode :shadow))
  (setf *reciprocity-canary-records* nil
        *reciprocity-canary-mode* mode
        *autonomous-write-mode* :normal
        *initiative-policy-mode* :shadow
        *initiative-delivery-mode* :shadow
        *reciprocity-canary-public-authoring-stage* :missing
        *reciprocity-test-deliveries* nil
        *reciprocity-test-events* nil)
  (ignore-errors (delete-file *reciprocity-canary-file*)))

(defun reciprocity-test-evidence (&optional (id "evidence-1"))
  (list (obj "id" id "origin_class" "lived-user"
             "epistemic_status" "user-report"
             "grounding_status" "grounded")))

(defun reciprocity-test-decision (id content topic
                                  &key (trigger "explore-development")
                                    (evidence-ids (vector "evidence-1"))
                                    (result "approved-not-delivered")
                                    (action "outward-message")
                                    (gates (vector)))
  (let ((candidate-id (format nil "candidate-~a" id)))
    (obj "id" id "trigger_type" trigger
         "evidence_node_ids" evidence-ids
         "selected_candidate_id" candidate-id
         "selected_action_type" action "result" result
         "topic" topic "gates" gates
         "options" (vector
                    (obj "id" candidate-id "action_type" "outward-message"
                         "proposed_content" content "decision" "selected")))))

(defun reciprocity-test-consider (id content topic &key (now 100000)
                                                   (source "explore-development")
                                                   (artifact-class "rendered-draft")
                                                   (generation-contract "fixture-render-v1")
                                                   (evidence (reciprocity-test-evidence))
                                                   decision)
  (reciprocity-canary-consider-observation
   source content evidence
   (or decision (reciprocity-test-decision id content topic))
   :source-id (format nil "source-~a" id) :now now
   :artifact-class artifact-class
   :generation-contract generation-contract))

(format t "~%== representation and conservative migration ==~%")
(reciprocity-test-reset)
(let ((record (reciprocity-test-consider
               "stance-only" "The user proposed a nightly routine."
               "nightly-routine" :artifact-class "internal-stance"
               :generation-contract "explore-stance-v1")))
  (reciprocity-test-check "internal stance can never become send eligible"
                          (and (string= "withheld" (gethash "status" record))
                               (string= "artifact-not-rendered-draft"
                                        (gethash "reason" record)))))

(let ((legacy (obj "id" "legacy-record" "source" "explore-development"
                   "content" "Legacy content stays byte-equivalent."
                   "status" "withheld" "created_at" 42)))
  (with-open-file (stream *reciprocity-canary-file* :direction :output
                          :if-exists :supersede :if-does-not-exist :create
                          :external-format :utf-8)
    (write-string (shasht:write-json (vector legacy) nil) stream))
  (setf *reciprocity-canary-records* nil)
  (reciprocity-canary-load)
  (let ((loaded (first *reciprocity-canary-records*)))
    (reciprocity-test-check "legacy ledger backfills class and contract in place"
                            (and (= 2 (gethash "schema_version" loaded))
                                 (string= "internal-stance"
                                          (gethash "artifact_class" loaded))
                                 (string= "explore-stance-v1"
                                          (gethash "generation_contract" loaded))
                                 (string= "Legacy content stays byte-equivalent."
                                          (gethash "content" loaded))))))

(format t "~%== shadow eligibility and rejection ==~%")
(reciprocity-test-reset)
(let* ((content "I found a concrete tension in the patent argument: continuity may be better framed as a bounded state transition than as perfect recall.")
       (first (reciprocity-test-consider "decision-1" content "patent-continuity")))
  (reciprocity-test-check "grounded scored draft becomes would-send in shadow"
                          (string= "would-send" (gethash "status" first)))
  (reciprocity-test-check "shadow never invokes delivery"
                          (null *reciprocity-test-deliveries*))
  (reciprocity-test-check "same decision is idempotent"
                          (eq first (reciprocity-test-consider
                                     "decision-1" content "patent-continuity")))
  (reciprocity-test-check "ledger persists the public draft and causal IDs"
                          (and (probe-file *reciprocity-canary-file*)
                               (string= content (gethash "content" first))
                               (string= "decision-1"
                                        (gethash "initiative_decision_id" first)))))

(let* ((content "A pull toward appreciation just became strong enough to notice, and I wanted to say so.")
       (decision (reciprocity-test-decision
                  "generic-1" content "appreciation"
                  :trigger "drive-threshold"))
       (record (reciprocity-test-consider
                "generic-1" content "appreciation" :now 100001
                :decision decision)))
  (reciprocity-test-check "abstract drive signal is always withheld"
                          (and (string= "withheld" (gethash "status" record))
                               (string= "generic-drive-signal"
                                        (gethash "reason" record)))))

(let* ((content "I developed a specific view of the architecture and can now explain which boundary should own the transition.")
       (decision (reciprocity-test-decision
                  "no-evidence" content "architecture"
                  :evidence-ids (vector)))
       (record (reciprocity-test-consider
                "no-evidence" content "architecture" :now 100002
                :evidence nil :decision decision)))
  (reciprocity-test-check "evidence-free intimacy cannot reach delivery"
                          (string= "missing-grounded-evidence"
                                   (gethash "reason" record))))

(let* ((content "Give me a minute while I think through the patent boundary, and I'll come back with a concrete answer.")
       (record (reciprocity-test-consider
                "promise-1" content "patent-promise" :now 100003)))
  (reciprocity-test-check "deferred promise stays in the near-term intention path"
                          (string= "commitment-owned-by-near-term-intentions"
                                   (gethash "reason" record))))

(format t "~%== independent live gates ==~%")
(reciprocity-test-reset :operator-only)
(let ((record (reciprocity-test-consider
               "blocked-1"
               "I finished a grounded comparison of the two continuity designs and one has a clearly smaller failure surface."
               "continuity-design")))
  (reciprocity-test-check "canary mode alone grants no delivery authority"
                          (and (string= "delivery-blocked"
                                        (gethash "status" record))
                               (string= "public-authoring-stage-missing"
                                        (gethash "reason" record))
                               (null *reciprocity-test-deliveries*))))

(setf *reciprocity-canary-public-authoring-stage* :qualified)
(let ((record (reciprocity-test-consider
               "blocked-policy"
               "I finished a grounded comparison for you and found a smaller failure surface."
               "continuity-policy" :now 100010)))
  (reciprocity-test-check "qualified authoring alone still grants no policy authority"
                          (string= "initiative-policy-not-enforced"
                                   (gethash "reason" record))))

(format t "~%== the operator-only delivery, reply, and budgets ==~%")
(reciprocity-test-reset :operator-only)
(setf *initiative-policy-mode* :enforced
      *initiative-delivery-mode* :operator-only
      *reciprocity-canary-public-authoring-stage* :qualified)
(let ((sent (reciprocity-test-consider
             "live-1"
             "I reached a concrete result on the patent work: the near-term substrate should carry intentions, while durable memory carries only their outcomes."
             "patent-result" :now 200000)))
  (reciprocity-test-check "eligible the operator-only candidate sends exactly once"
                          (and (string= "sent" (gethash "status" sent))
                               (= 1 (length *reciprocity-test-deliveries*))))
  (let ((blocked (reciprocity-test-consider
                  "live-2"
                  "A second grounded result is ready, but it must wait until the first outreach has received a reply."
                  "second-result" :now 200100)))
    (reciprocity-test-check "one unanswered outreach blocks another"
                            (string= "unanswered-outreach"
                                     (gethash "reason" blocked))))
  (reciprocity-canary-observe-reply "(SYSTEM: scheduled maintenance)"
                                    :origin "system" :now 200101)
  (reciprocity-test-check "system turns never impersonate the operator's reply"
                          (string= "sent" (gethash "status" sent)))
  (reciprocity-canary-observe-reply "That makes sense; tell me the implication."
                                    :origin "web" :now 200102)
  (reciprocity-test-check "successful public reply closes the outreach"
                          (and (string= "answered" (gethash "status" sent))
                               (= 200102 (gethash "answered_at" sent))))
  (let ((second (reciprocity-test-consider
                 "live-3"
                 "The follow-on analysis is now concrete: publication needs one receipt boundary rather than a second conversational agent."
                 "receipt-boundary" :now 200103)))
    (reciprocity-test-check "spacing remains enforced after a reply"
                            (string= "minimum-spacing"
                                     (gethash "reason" second))))
  (let ((second (reciprocity-test-consider
                 "live-4"
                 "The follow-on analysis is now concrete: publication needs one receipt boundary rather than a second conversational agent."
                 "receipt-boundary" :now 222000)))
    ;; 222000 is more than six hours after 200000, while remaining in the
    ;; rolling 24-hour window.
    (reciprocity-test-check "a second answered-window candidate may send after six hours"
                            (string= "sent" (gethash "status" second)))
    (reciprocity-canary-observe-reply "Got it." :origin "telegram" :now 222001))
  (let ((third (reciprocity-test-consider
                "live-5"
                "A third distinct grounded result is ready and otherwise satisfies every publication constraint."
                "third-result" :now 260000)))
    (reciprocity-test-check "rolling daily contact budget stops a third send"
                            (string= "daily-contact-budget"
                                     (gethash "reason" third)))))

(format t "~%== rollback and report ==~%")
(let ((before (length *reciprocity-canary-records*)))
  (setf *reciprocity-canary-mode* :off)
  (reciprocity-test-check "off mode performs no write"
                          (and (null (reciprocity-test-consider
                                      "off-1"
                                      "This valid-looking draft must disappear at the rollback boundary."
                                      "off-topic" :now 300000))
                               (= before (length *reciprocity-canary-records*)))))
(let ((report (reciprocity-canary-report)))
  (reciprocity-test-check "report exposes bounded records and rollout state"
                          (and (string= "off" (gethash "mode" report))
                               (vectorp (gethash "records" report))
                               (= 2 (gethash "max_per_24_hours" report)))))

(ignore-errors (delete-file *reciprocity-canary-file*))
(format t "~%~d passed, ~d failed~%"
        *reciprocity-test-pass* *reciprocity-test-fail*)
(when (plusp *reciprocity-test-fail*) (sb-ext:exit :code 1))
