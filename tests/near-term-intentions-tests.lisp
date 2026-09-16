(in-package :agent)

(defvar *near-term-intention-test-pass* 0)
(defvar *near-term-intention-test-fail* 0)
(defun near-term-intention-test-check (name condition)
  (if condition
      (progn (incf *near-term-intention-test-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *near-term-intention-test-fail*) (format t "  FAIL ~a~%" name))))

(unless (boundp '*near-term-intentions-mode*)
  (defvar *near-term-intentions-mode* :off))
(unless (boundp '*autonomous-write-mode*)
  (defvar *autonomous-write-mode* :normal))

(defvar *near-term-test-delivery-ready-p* t)
(defvar *near-term-test-readiness-calls* 0)
(defun initiative-committed-delivery-readiness (&key audience)
  (declare (ignore audience))
  (incf *near-term-test-readiness-calls*)
  (values *near-term-test-delivery-ready-p*
          (unless *near-term-test-delivery-ready-p* "fixture-delivery-unavailable")))

(load (test-source "near-term-workspace.lisp"))
(load (test-source "near-term-intentions.lisp"))
(load (test-source "near-term-workspace-adapters.lisp"))

(let* ((*near-term-intention-file* #P"/tmp/near-term-intentions-test.json")
       (*near-term-intention-records* nil)
       (*near-term-intentions-mode* :off)
       (*autonomous-write-mode* :normal)
       (cognitive-calls 0)
       (delivery-calls 0)
       (*near-term-intention-delivery-fn*
         (lambda (record)
           (incf delivery-calls)
           (obj "status" "delivered" "reason" "fixture-transport"
                "delivery_receipt_id"
                (format nil "delivery:~a" (gethash "id" record))))))
  (when (probe-file *near-term-intention-file*)
    (delete-file *near-term-intention-file*))

  (format t "~%== fail-closed receipt gates ==~%")
  (multiple-value-bind (record reason)
      (near-term-intention-create "patent" "develop an idea" "turn-1"
                                  '(101) 180 :now 1000)
    (near-term-intention-test-check "off mode rejects receipt" (null record))
    (near-term-intention-test-check "off reason is explicit"
                                    (string= reason "near-term-intentions-not-enforced")))
  (setf *near-term-intentions-mode* :shadow)
  (multiple-value-bind (record reason)
      (near-term-intention-create "patent" "develop an idea" "turn-1"
                                  '(101) 30 :now 1000)
    (near-term-intention-test-check "short window is rejected" (null record))
    (near-term-intention-test-check "window rejection is explicit"
                                    (string= reason "unsupported-return-window")))
  (multiple-value-bind (record reason)
      (near-term-intention-create "patent" "develop an idea" nil nil 120 :now 1000)
    (near-term-intention-test-check "missing origin is rejected" (null record))
    (near-term-intention-test-check "origin rejection is explicit"
                                    (string= reason "missing-causal-origin")))

  (let ((*near-term-intentions-mode* :enforced)
        (*near-term-test-delivery-ready-p* nil)
        (*near-term-test-readiness-calls* 0))
    (multiple-value-bind (record reason)
        (near-term-intention-create "gated" "prove readiness" "turn-gate"
                                    '(100) 180 :now 999)
      (near-term-intention-test-check
       "custom delivery hook cannot bypass producer readiness"
       (and (null record)
            (string= reason "fixture-delivery-unavailable")
            (= 1 *near-term-test-readiness-calls*)))))

  (format t "~%== durable receipt and workspace projection ==~%")
  (setf *near-term-intentions-mode* :enforced)
  (multiple-value-bind (created reason)
      (near-term-intention-create
       "Patent idea from our conversation" "develop one concrete claim direction"
       "turn-1" '(101) 180 :now 1000)
    (near-term-intention-test-check "valid receipt is created"
                                    (and created (string= reason "created")))
    (near-term-intention-test-check "receipt is persisted before return"
                                    (probe-file *near-term-intention-file*))
    (near-term-intention-test-check "receipt retains causal turn"
                                    (string= "turn-1" (gethash "origin_turn_id" created)))
    (near-term-intention-test-check "receipt has bounded deadline"
                                    (= 1180 (gethash "response_deadline" created)))
    (multiple-value-bind (second second-reason)
        (near-term-intention-create "other" "other aim" "turn-2" '(102) 180
                                    :now 1001)
      (near-term-intention-test-check "one-active limit rejects second receipt"
                                      (null second))
      (near-term-intention-test-check "one-active reason is explicit"
                                      (string= second-reason
                                               "active-intention-limit")))
    (let* ((snapshot (near-term-workspace-shadow-snapshot :now 1001))
           (items (coerce (gethash "items" snapshot) 'list))
           (item (find "deferred-intention" items
                       :key (lambda (row) (gethash "item_type" row))
                       :test #'string=)))
      (near-term-intention-test-check "workspace receives conversational intention"
                                      (not (null item)))
      (near-term-intention-test-check "dashboard view exposes bounded progress"
                                      (and item
                                           (= 0 (gethash "pass_count" item))
                                           (= 2 (gethash "max_passes" item))
                                           (string= "receipt-created"
                                                    (gethash "latest_transition" item))))
      (near-term-intention-test-check "dashboard view grants no action permission"
                                      (and item
                                           (null (gethash "action_permission" item)))))

    (format t "~%== task-directed bounded advancement ==~%")
    (let ((*near-term-intention-cognitive-fn*
            (lambda (record evidence question)
              (declare (ignore record question))
              (incf cognitive-calls)
              (obj "status" "accepted" "record"
                   (obj "content" "A concrete patent claim direction grounded in the conversation."
                        "evidence_node_ids"
                        (coerce (mapcar (lambda (node) (gethash "id" node)) evidence)
                                'vector)))))
          (*near-term-intention-evidence-fn*
            (lambda (record)
              (declare (ignore record))
              (list (obj "id" "memory-101" "content" "the operator requested patent thinking.")))))
      (setf *near-term-intentions-mode* :shadow)
      (let ((shadow-result (near-term-intention-process-due :now 1000)))
        (near-term-intention-test-check "shadow mode performs no private call"
                                        (and (string= "skipped"
                                                      (gethash "status" shadow-result))
                                             (zerop cognitive-calls))))
      (setf *near-term-intentions-mode* :enforced)
      (let ((result (near-term-intention-process-due :now 1000)))
        (near-term-intention-test-check "due enforced item makes one bounded call"
                                        (and (= cognitive-calls 1)
                                             (string= "delivered"
                                                      (gethash "status" result))))
        (let ((stored (first (near-term-intention-records :now 1001))))
          (near-term-intention-test-check "accepted result is delivered through one seam"
                                          (and (= delivery-calls 1)
                                               (string= "expressed"
                                                        (gethash "state" stored))))
          (near-term-intention-test-check "real pass count is recorded"
                                          (= 1 (gethash "pass_count" stored)))
          (near-term-intention-test-check "only conclusion is retained"
                                          (search "concrete patent claim"
                                                  (gethash "artifact_summary" stored))))))

    (near-term-intention-test-check "delivery closes the commitment"
                                    (null (near-term-intention-active :now 1001))))

  (multiple-value-bind (created reason)
      (near-term-intention-create "Patent follow-up" "state the grounded result"
                                  "turn-observe" '(103) 180 :now 1500)
    (declare (ignore reason))
    (near-term-intention-transition
     (gethash "id" created) "ready" "fixture-ready"
     :artifact-summary "The concrete patent claim direction is grounded in our conversation."
     :now 1501)
    (multiple-value-bind (ignored observe-reason)
        (near-term-intention-observe-public-reply
         "The weather is clear today." "turn-unrelated" :now 1502)
      (declare (ignore ignored))
      (near-term-intention-test-check "unrelated reply cannot falsely close commitment"
                                      (string= observe-reason "result-not-observed")))
    (near-term-intention-observe-public-reply
     "The concrete patent claim direction is grounded in our conversation."
     "turn-3" :now 1503)
    (near-term-intention-test-check "observed public result closes the commitment"
                                    (null (near-term-intention-active :now 1504))))

  (format t "~%== no-evidence no-cost suppression and restart ==~%")
  (setf *near-term-intentions-mode* :enforced)
  (multiple-value-bind (created reason)
      (near-term-intention-create "another subject" "another grounded result"
                                  "turn-4" '(104) 180 :now 2000)
    (declare (ignore reason))
    (let ((*near-term-intention-evidence-fn* (lambda (record)
                                                (declare (ignore record)) nil))
          (*near-term-intention-cognitive-fn*
            (lambda (&rest args) (declare (ignore args)) (incf cognitive-calls))))
      (let ((before cognitive-calls)
            (result (near-term-intention-process-due :now 2000)))
        (near-term-intention-test-check "missing evidence blocks explicitly"
                                        (string= "blocked" (gethash "status" result)))
        (near-term-intention-test-check "missing evidence spends no model call"
                                        (= before cognitive-calls))))
    (let ((expected-id (gethash "id" created)))
      (setf *near-term-intention-records* nil)
      (near-term-intention-load)
      (near-term-intention-test-check "restart reload retains the record"
                                      (find expected-id *near-term-intention-records*
                                            :key (lambda (row) (gethash "id" row))
                                            :test #'string=))))
  (when (probe-file *near-term-intention-file*)
    (delete-file *near-term-intention-file*)))

(format t "~%~a passed, ~a failed~%"
        *near-term-intention-test-pass* *near-term-intention-test-fail*)
(when (plusp *near-term-intention-test-fail*) (sb-ext:exit :code 1))
