(in-package :agent)

(ql:quickload '(:bordeaux-threads :shasht) :silent t)

(defvar *pull-integration-pass* 0)
(defvar *pull-integration-fail* 0)
(defvar *pull-integration-events* nil)
(defvar *pull-integration-base-calls* 0)
(defvar *pull-integration-web-broadcasts* nil)
(defvar *pull-integration-records* nil)

(defun pull-integration-check (name condition)
  (if condition
      (progn (incf *pull-integration-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *pull-integration-fail*) (format t "  FAIL ~a~%" name))))

(defun pull-integration-log-event (type payload &key caused-by)
  (let ((id (1+ (length *pull-integration-events*))))
    (push (list id type payload caused-by) *pull-integration-events*) id))

(defun auto-turn (prompt)
  (incf *pull-integration-base-calls*)
  (format nil "base:~a" prompt))

(defun propose-loop (&rest args) (declare (ignore args)) :fixture)

(defun %v2-broadcast (type data)
  (push (list type data) *pull-integration-web-broadcasts*)
  t)

(defun reciprocity-canary-snapshot ()
  (obj "status" "available" "record_cap" 200 "at_cap" nil
       "pruned_count" :null
       "records"
       (vector
        (obj "id" "recip-integration" "initiative_decision_id" "v2-integration"
             "source" "grounded-project" "topic" "integration"
             "content" "A specific grounded candidate shown only after an inbound request."
             "artifact_class" "internal-stance"
             "generation_contract" "explore-stance-v1"
             "evidence_node_ids" (vector "evidence-integration")
             "status" "withheld" "reason" "v2-hard-gate"))))

(load (test-source "candidate-representation.lisp"))
(load (test-source "pull-reciprocity.lisp"))
(setf *pull-reciprocity-review-file* #P"/tmp/pull-integration-review.json"
      *pull-reciprocity-label-file* #P"/tmp/pull-integration-labels.json"
      *pull-reciprocity-review* nil *pull-reciprocity-descriptions* nil
      *pull-reciprocity-batches* nil *pull-reciprocity-labels* nil
      *pull-reciprocity-active-label* nil)
(ignore-errors (delete-file *pull-reciprocity-review-file*))
(ignore-errors (delete-file *pull-reciprocity-label-file*))

(load (test-source "event-log.lisp"))
(setf (fdefinition 'log-event) #'pull-integration-log-event
      *pull-integration-events* nil)

(let ((*public-inbound-channel* "telegram"))
  (let ((reply (auto-turn "/mind raw 1")))
    (pull-integration-check "Telegram inbound command returns reply text"
                            (search "specific grounded candidate" reply :test #'char-equal))
    (pull-integration-check "command bypasses model/base turn"
                            (zerop *pull-integration-base-calls*))))

(pull-integration-check "ordinary prompt still falls through exactly once"
                        (and (string= "base:ordinary prompt" (auto-turn "ordinary prompt"))
                             (= 1 *pull-integration-base-calls*)))

(setf *pull-reciprocity-review* nil *pull-integration-web-broadcasts* nil)
(let ((*public-inbound-channel* "web"))
  (let ((reply (auto-turn "/mind")))
    (pull-integration-check "web ordinary reply describes but does not quote internal stance"
                            (and (search "internal material" reply :test #'char-equal)
                                 (not (search "specific grounded candidate" reply
                                              :test #'char-equal))))
    (pull-integration-check "web command publishes one final and no initiative"
                            (and (= 1 (length *pull-integration-web-broadcasts*))
                                 (string= "final"
                                          (first (first *pull-integration-web-broadcasts*)))))))

(pull-integration-check "reply event is causally paired with inbound user event"
                        (let* ((agent (find "agent-message" *pull-integration-events*
                                            :test #'string= :key #'second))
                               (cause (and agent (fourth agent))))
                          (and (integerp cause)
                               (find cause *pull-integration-events* :key #'first))))

(pull-integration-check "integration added no outbound initiator"
                        (and (= 1 *pull-integration-base-calls*)
                             (null *pull-integration-records*)))

(ignore-errors (delete-file *pull-reciprocity-review-file*))
(ignore-errors (delete-file *pull-reciprocity-label-file*))

(format t "~&pull reciprocity integration tests: ~d passed, ~d failed.~%"
        *pull-integration-pass* *pull-integration-fail*)
(when (plusp *pull-integration-fail*) (sb-ext:exit :code 1))
