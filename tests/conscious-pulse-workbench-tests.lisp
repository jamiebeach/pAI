;;;; conscious-pulse-workbench-tests.lisp -- Q3 contained manual endpoints.

(in-package :agent)

(ql:quickload '(:hunchentoot :shasht :ironclad :babel) :silent t)

(defvar *cpw-passed* 0)
(defvar *cpw-failed* 0)
(defvar *cpw-events* '())
(defvar *cpw-pulse-calls* 0)
(defvar *cpw-recover-calls* 0)
(defvar *cpw-restore-calls* 0)
(defvar *cpw-captured-open-calls* 0)
(defvar *cpw-captured-submit-calls* 0)
(defvar *cpw-last-captured-response* nil)
(defparameter *autonomous-write-mode* :paused)
(defparameter *memory-atom-decomposition-mode* :off)
(defparameter *agent-id* "q3-workbench")
(defparameter *conscious-cognition-runtime-revision* "conscious-q3-test")

(defun cpw-check (name condition)
  (if condition
      (progn (incf *cpw-passed*) (format t "PASS ~a~%" name))
      (progn (incf *cpw-failed*) (format t "FAIL ~a~%" name))))

(defun cpw-signals-p (thunk)
  (handler-case (progn (funcall thunk) nil) (error () t)))

(defun cognition-runtime-selected-p (name) (eq name :conscious-state))
(defun log-event (type payload &key caused-by)
  (declare (ignore caused-by))
  (let ((id (1+ (length *cpw-events*))))
    (setf *cpw-events*
          (append *cpw-events*
                  (list (obj "id" id "type" type "agent_id" *agent-id*
                             "payload" payload))))
    id))
(defun replay-events (&rest args) (declare (ignore args)) *cpw-events*)
(defun cognition-runtime-restore () (incf *cpw-restore-calls*) t)
(defun conscious-cognition-runtime-pulse (&rest args)
  (declare (ignore args))
  (incf *cpw-pulse-calls*)
  (values (obj "pulse_id" "pulse:3" "status" "completed"
               "pulse_sequence" 1 "consumed_stimulus_ids"
               (vector "stimulus:1"))
          (obj "state_revision" 1 "observation_revision" 4)))
(defun conscious-cognition-runtime-recover ()
  (incf *cpw-recover-calls*) 1)
(defun conscious-cognition-runtime-open-captured (&rest args)
  (declare (ignore args))
  (incf *cpw-captured-open-calls*)
  (obj "private_request"
       (vector (obj "role" "current-stimulus" "section" "triggering-stimuli"
                    "source_id" 3 "content" "fixture question"))
       "manifest"
       (obj "pulse_id" "pulse:4" "runtime_revision" "conscious-q4-test"
            "conscious_state_revision" 1 "audience" "operator"
            "evidence_event_ids" (vector 3)
            "available_tools" (vector "inspect-state")
            "permitted_proposal_kinds"
            (vector "tool-call-proposal" "publication-candidate" "yield" "abstain")
            "remaining_budget"
            (obj "tool_proposals" 1 "continuations" 0
                 "publication_candidates" 1))))
(defun conscious-cognition-runtime-submit-captured (captured)
  (incf *cpw-captured-submit-calls*)
  (setf *cpw-last-captured-response* captured)
  (values (obj "pulse_id" "pulse:4" "status" "completed"
               "terminal_reason" "captured-deliberation-validated"
               "pulse_sequence" 2 "proposals" (gethash "proposals" captured)
               "consumed_stimulus_ids" (vector)
               "model_calls" 0 "tool_proposals" 0 "cost_microunits" 0)
          (obj "state_revision" 2 "observation_revision" 5)))
(defun conscious-cognition-runtime-report ()
  (obj "state" "projected" "state_revision" 1
       "observation_revision" 4 "pulse_worker" "manual-q3"
       "pulse" (obj "in_flight" nil "last_terminal_event_id" 4
                    "provider_route" nil "effect_route" nil
                    "publication_route" nil)))
(defun conscious-pulse-plan-report (plan)
  (obj "pulse_id" (gethash "pulse_id" plan)
       "status" (gethash "status" plan)
       "pulse_sequence" (gethash "pulse_sequence" plan)
       "consumed_count" (length (gethash "consumed_stimulus_ids" plan))))
(defun make-deterministic-pulse-budget (&rest args)
  (declare (ignore args)) (obj "fixture" t))

(let ((manifest "/tmp/q3-workbench-manifest.json"))
  (with-open-file (stream manifest :direction :output :if-exists :supersede
                                   :if-does-not-exist :create)
    (write-string
     "{\"schema_version\":1,\"run_id\":\"q3-workbench\",\"disposable\":true,\"database_label_verified\":true,\"network_internal\":true,\"ingress_scope\":\"loopback-only\",\"provider_egress\":\"disabled\",\"delivery_authority\":\"absent\"}"
     stream))
  (setf (uiop:getenv "PAI_DEV_WORKBENCH") "enabled"
        (uiop:getenv "PAI_DEV_RUN_ID") "q3-workbench"
        (uiop:getenv "PAI_DEV_MANIFEST_FILE") manifest)
  (unwind-protect
       (progn
         (load (test-source "dev-workbench.lisp"))
         (format t "~%== Q3 workbench subject ==~%")
         (dolist (name '(dev-workbench-conscious-submit-fixture
                         dev-workbench-conscious-pulse
                         dev-workbench-conscious-open-captured
                         dev-workbench-conscious-submit-captured-fixture
                         dev-workbench-conscious-recover
                         dev-workbench-conscious-state))
           (cpw-check (format nil "~a exists" name) (fboundp name)))
         (when (fboundp 'dev-workbench-conscious-submit-fixture)
           (setf *cpw-events* '() *cpw-restore-calls* 0)
           (let ((result (dev-workbench-conscious-submit-fixture
                          (obj "fixture_id" "q3-two-item"))))
             (cpw-check "closed fixture appends its two declared input types"
                        (and (= 2 (gethash "event_count" result))
                             (equal '("runtime-observer-error"
                                      "episode-boundary-detected")
                                    (mapcar (lambda (event)
                                              (gethash "type" event))
                                            *cpw-events*))
                             (= 1 *cpw-restore-calls*)))
             (cpw-check "fixture result contains IDs but no fixture content"
                        (and (= 2 (length (gethash "event_ids" result)))
                             (null (search "source" (shasht:write-json result nil)
                                           :test #'char-equal)))))
           (cpw-check "unknown fixture is rejected before append"
                      (cpw-signals-p
                       (lambda ()
                         (dev-workbench-conscious-submit-fixture
                          (obj "fixture_id" "arbitrary")))))
           (let ((*autonomous-write-mode* :running))
             (cpw-check "fixture is blocked outside paused disposable runtime"
                        (cpw-signals-p
                         (lambda ()
                           (dev-workbench-conscious-submit-fixture
                            (obj "fixture_id" "q3-two-item")))))))
         (when (fboundp 'dev-workbench-conscious-pulse)
           (let ((result (dev-workbench-conscious-pulse
                          (obj "now" 2000 "cancelled" nil))))
             (cpw-check "pulse endpoint delegates once and returns safe summary"
                        (and (= 1 *cpw-pulse-calls*)
                             (string= "completed" (gethash "status" result))
                             (= 1 (gethash "state_revision" result))
                             (null (search "consumed_stimulus_ids"
                                           (shasht:write-json result nil)
                                           :test #'char-equal))))))
         (when (fboundp 'dev-workbench-conscious-recover)
           (cpw-check "recovery endpoint delegates once"
                      (= 1 (gethash "recovered_count"
                                    (dev-workbench-conscious-recover (obj)))))
           (cpw-check "recovery adapter was called exactly once"
                      (= 1 *cpw-recover-calls*)))
         (when (fboundp 'dev-workbench-conscious-state)
           (let ((report (dev-workbench-conscious-state)))
             (cpw-check "state endpoint remains content-free and route-free"
                        (and (= 1 (gethash "state_revision" report))
                             (null (gethash "provider_route"
                                            (gethash "pulse" report)))
                             (null (search "text" (shasht:write-json report nil)
                                           :test #'char-equal)))))))
         (when (and (fboundp 'dev-workbench-conscious-open-captured)
                    (fboundp 'dev-workbench-conscious-submit-captured-fixture))
           (dev-workbench-conscious-submit-fixture
            (obj "fixture_id" "q4-user-message"))
           (let ((opened (dev-workbench-conscious-open-captured
                          (obj "now" 2000))))
             (cpw-check "captured open delegates once and exposes private request"
                        (and (= 1 *cpw-captured-open-calls*)
                             (plusp (length (gethash "private_request" opened)))))
             (let ((submitted
                     (dev-workbench-conscious-submit-captured-fixture
                      (obj "fixture_id" "q4-publication-candidate"))))
               (cpw-check "named captured fixture delegates structured output once"
                          (and (= 1 *cpw-captured-submit-calls*)
                               (string= "completed" (gethash "status" submitted))
                               (string= "publication-candidate"
                                        (gethash "kind"
                                                 (aref (gethash "proposals"
                                                                *cpw-last-captured-response*)
                                                       0)))))))
           (cpw-check "unknown captured fixture is rejected before submit"
                      (and (cpw-signals-p
                            (lambda ()
                              (dev-workbench-conscious-submit-captured-fixture
                               (obj "fixture_id" "arbitrary"))))
                           (= 1 *cpw-captured-submit-calls*)))
           (let ((*autonomous-write-mode* :running))
             (cpw-check "captured open is blocked outside paused disposable runtime"
                        (cpw-signals-p
                         (lambda ()
                           (dev-workbench-conscious-open-captured
                             (obj "now" 2000)))))))
    (ignore-errors (delete-file manifest))))

(format t "~%~d passed, ~d failed~%" *cpw-passed* *cpw-failed*)
(when (plusp *cpw-failed*) (uiop:quit 1))
