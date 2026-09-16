(in-package :agent)

(defvar *committed-delivery-test-pass* 0)
(defvar *committed-delivery-test-fail* 0)
(defvar *committed-delivery-test-sends* nil)
(defvar *committed-delivery-test-scores* 0)

(defun committed-delivery-test-check (name condition)
  (if condition
      (progn (incf *committed-delivery-test-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *committed-delivery-test-fail*) (format t "  FAIL ~a~%" name))))

(unless (boundp '*initiative-policy-mode*)
  (defvar *initiative-policy-mode* :shadow))
(unless (boundp '*initiative-delivery-mode*)
  (defvar *initiative-delivery-mode* :shadow))
(unless (boundp '*telegram-last-chat-id*)
  (defvar *telegram-last-chat-id* nil))
(unless (fboundp '%drives-event-initiate)
  (setf (fdefinition '%drives-event-initiate)
        (lambda (&rest args) (declare (ignore args)) nil)))

(load (test-source "candidate-policy.lisp"))

(setf (fdefinition 'telegram-send)
      (lambda (chat-id content)
        (push (list chat-id content) *committed-delivery-test-sends*)
        t)
      (fdefinition '%score-candidate)
      (lambda (&rest args)
        (declare (ignore args))
        (incf *committed-delivery-test-scores*)
        (values 10 10)))

(let ((*initiative-candidate-file* #P"/tmp/committed-delivery-test.json")
      (*initiative-candidates* nil)
      (*initiative-policy-mode* :shadow)
      (*initiative-delivery-mode* :shadow)
      (*telegram-last-chat-id* 4242)
      (*initiative-quiet-hours* nil)
      (*committed-delivery-test-sends* nil)
      (*committed-delivery-test-scores* 0))
  (when (probe-file *initiative-candidate-file*)
    (delete-file *initiative-candidate-file*))

  (format t "~%== receipt preflight ==~%")
  (multiple-value-bind (ready reason)
      (initiative-committed-delivery-readiness)
    (committed-delivery-test-check "shadow policy is not receipt-ready"
                                   (and (null ready)
                                        (string= reason "blocked-policy-not-enforced"))))
  (committed-delivery-test-check
   "shadow delivery sends nothing"
   (let ((result (initiative-deliver-committed-result
                  "A grounded result." "receipt-shadow" :now 1000)))
     (and (string= "blocked" (gethash "status" result))
          (null *committed-delivery-test-sends*))))

  (format t "~%== the operator-only fulfillment ==~%")
  (setf *initiative-policy-mode* :enforced
        *initiative-delivery-mode* :operator-only)
  (multiple-value-bind (ready reason)
      (initiative-committed-delivery-readiness)
    (committed-delivery-test-check "configured the operator route is receipt-ready"
                                   (and ready (null reason))))
  (let ((result (initiative-deliver-committed-result
                 "The grounded patent direction is concrete." "receipt-1"
                 :audience "the operator" :now 1001)))
    (committed-delivery-test-check "fulfilled receipt uses one existing send seam"
                                   (and (string= "delivered" (gethash "status" result))
                                        (= 1 (length *committed-delivery-test-sends*))
                                        (= 4242 (caar *committed-delivery-test-sends*))
                                        (string= "The grounded patent direction is concrete."
                                                 (cadar *committed-delivery-test-sends*)))))
  (committed-delivery-test-check "receipt fulfillment invokes no desirability scorer"
                                 (zerop *committed-delivery-test-scores*))

  (format t "~%== already-scored initiative delivery ==~%")
  (setf *initiative-candidates* nil)
  (let* ((before (length *committed-delivery-test-sends*))
         (result (initiative-deliver-approved-message
                  "A scored, grounded thought is ready to share."
                  "initiative-decision-1" :now 1002)))
    (committed-delivery-test-check
     "approved v2 draft uses the same the operator-only send boundary"
     (and (string= "delivered" (gethash "status" result))
          (= (1+ before) (length *committed-delivery-test-sends*))
          (string= "initiative-decision-1"
                   (gethash "initiative_v2_decision_id"
                            (first *initiative-candidates*)))))
    (committed-delivery-test-check
     "approved draft is not scored a second time"
     (zerop *committed-delivery-test-scores*)))

  (format t "~%== gates are rechecked at send time ==~%")
  (setf *initiative-candidates* nil)
  (push (obj "id" "open-1" "status" "sent" "answered_at" :null
             "updated_at" 1002 "topic" "other")
        *initiative-candidates*)
  (let ((before (length *committed-delivery-test-sends*))
        (result (initiative-deliver-committed-result
                 "A second result." "receipt-2" :now 1002)))
    (committed-delivery-test-check "unanswered outreach blocks fulfillment"
                                   (and (string= "unanswered-outreach"
                                                 (gethash "reason" result))
                                        (= before
                                           (length *committed-delivery-test-sends*)))))
  (setf *initiative-candidates* nil)
  (let ((before (length *committed-delivery-test-sends*))
        (result (initiative-deliver-committed-result
                 "You need to reassure me." "receipt-3" :now 1003)))
    (committed-delivery-test-check "manipulation gate blocks fulfillment"
                                   (and (string= "manipulation-risk"
                                                 (gethash "reason" result))
                                        (= before
                                           (length *committed-delivery-test-sends*)))))
  (ignore-errors (delete-file *initiative-candidate-file*)))

(format t "~%~a passed, ~a failed~%"
        *committed-delivery-test-pass* *committed-delivery-test-fail*)
(when (plusp *committed-delivery-test-fail*) (sb-ext:exit :code 1))
