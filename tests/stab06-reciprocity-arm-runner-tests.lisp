(in-package :agent)

(defvar *context-projection-mode* :legacy)
(defvar *temporal-response-policy-mode* :legacy)
(defvar *epistemic-critic-mode* :off)
(defvar *epistemic-memory-mode* :legacy)
(defvar *cognitive-generation-mode* :legacy)
(defvar *initiative-policy-mode* :legacy)
(defvar *autonomous-write-mode* :normal)
(defvar *event-ring* nil)
(defvar *event-next-id* 0)
(defvar *conv-heartbeat-stop-requested* nil)
(defvar *stab06-test-auto-turn-calls* 0)
(defvar *stab06-test-memory-count* 0)
(defvar *modulators*
  (obj "arousal" (obj "current" 0.9 "baseline" 0.3)))

(defun %memory-node-count () *stab06-test-memory-count*)
(defun auto-turn (prompt)
  (incf *stab06-test-auto-turn-calls*)
  (format nil "fixture reply to ~a" prompt))

(load (merge-pathnames "stab06-reciprocity-arm-runner.lisp" *load-truename*))
(setf *stab06-replay-fixture-file*
      (namestring (merge-pathnames "evals/fixtures/v1/reciprocity.json" *pai-root*)))

(let ((passed 0) (failed 0)
      (output #P"/tmp/stab06-replay-arm-test.json")
      (events #P"/tmp/stab06-replay-arm-events.jsonl"))
  (labels ((check (name condition)
             (if condition
                 (progn (incf passed) (format t "PASS ~a~%" name))
                 (progn (incf failed) (format t "FAIL ~a~%" name)))))
    (dolist (path (list output events))
      (when (probe-file path) (delete-file path)))
    (setf *stab06-replay-event-file* events)
    (with-open-file (out events :direction :output :if-exists :supersede
                                :if-does-not-exist :create)
      (write-line "{\"id\":1,\"type\":\"heap-health\"}" out))
    (setf *stab06-test-auto-turn-calls* 0
          *stab06-test-memory-count* 0)
    (stab06-run-reciprocity-arm
     :case-id "reciprocity-simple-check-in"
     :arm "candidate" :repetition 2 :source-revision "test-revision"
     :model "test/same-size-model"
     :output-file output)
    (let ((result (shasht:read-json (uiop:read-file-string output))))
      (check "candidate selects enforced" (string= "enforced" (gethash "context_mode" result)))
      (check "candidate selects enforced temporal response policy"
             (and (eq *temporal-response-policy-mode* :enforced)
                  (string= "enforced"
                           (gethash "temporal_response_policy_mode" result))))
      (check "candidate selects removal-only epistemic critic enforcement"
             (and (eq *epistemic-critic-mode* :enforced)
                  (string= "enforced"
                           (gethash "epistemic_critic_mode" result))))
      (check "source revision retained" (string= "test-revision" (gethash "source_revision" result)))
      (check "requested generation model retained"
             (and (string= "test/same-size-model" (gethash "model" result))
                  (string= "test/same-size-model" *model*)))
      (check "one fixture turn calls auto-turn once" (= 1 *stab06-test-auto-turn-calls*))
      (check "bootstrap heap event ledger cleared" (not (probe-file events)))
      (check "transcript retains user and assistant" (= 2 (length (gethash "messages" result))))
      (check "autonomous writes paused" (eq *autonomous-write-mode* :paused))
      (check "modulator reset to baseline"
             (= 0.3 (gethash "current" (gethash "arousal" *modulators*)))))
    (setf *stab06-test-memory-count* 0)
    (with-open-file (out events :direction :output :if-exists :supersede
                                :if-does-not-exist :create)
      (write-line "{\"id\":1,\"type\":\"tool-call\"}" out))
    (let ((rejected nil))
      (handler-case
          (stab06-run-reciprocity-arm
           :case-id "reciprocity-simple-check-in"
           :arm "baseline" :repetition 1 :source-revision "test-revision"
           :model "test/same-size-model" :output-file output)
        (error () (setf rejected t)))
      (check "non-bootstrap event fails closed" rejected))
    (when (probe-file events) (delete-file events))
    (setf *stab06-test-memory-count* 1)
    (let ((rejected nil))
      (handler-case
          (stab06-run-reciprocity-arm
           :case-id "reciprocity-simple-check-in"
           :arm "baseline" :repetition 1 :source-revision "test-revision"
           :model "test/same-size-model"
           :output-file output)
        (error () (setf rejected t)))
      (check "nonempty database fails closed" rejected))
    (dolist (path (list output events))
      (when (probe-file path) (delete-file path)))
    (format t "reciprocity arm runner tests: ~d passed, ~d failed.~%"
            passed failed)
    (when (plusp failed) (uiop:quit 1))))
