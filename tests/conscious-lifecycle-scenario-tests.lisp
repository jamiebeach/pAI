;;;; conscious-lifecycle-scenario-tests.lisp -- Q5 operator restart scenario.

(in-package :agent)

(defvar *clst-passed* 0)
(defvar *clst-failed* 0)
(defvar *clst-events* '())
(defvar *clst-next-id* 0)
(defvar *clst-provider-calls* 0)
(defvar *clst-effect-calls* 0)
(defvar *clst-delivery-calls* 0)
(defvar *clst-delivery-readiness-calls* 0)
(defvar *clst-replay-calls* 0)
(defparameter *agent-id* "q5-lifecycle-dev")
(defvar *near-term-intentions-mode* :off)
(defvar *autonomous-write-mode* :normal)
(defvar *near-term-intention-delivery-fn* nil)
(defvar *near-term-intention-records* nil)
(defvar *near-term-intention-file* nil)
(defvar *conscious-lifecycle-runtime-projection* nil)

(defun clst-check (name condition)
  (if condition
      (progn (incf *clst-passed*) (format t "PASS ~a~%" name))
      (progn (incf *clst-failed*) (format t "FAIL ~a~%" name))))

(defun replay-events (&rest ignored)
  (declare (ignore ignored))
  (incf *clst-replay-calls*)
  (copy-list *clst-events*))

(defun log-event (type payload &key caused-by)
  (let* ((id (incf *clst-next-id*))
         (event (obj "schema_version" 1 "id" id "timestamp" (+ 1000 id)
                     "type" type "agent_id" *agent-id*
                     "caused_by" (or caused-by :null) "payload" payload)))
    (setf *clst-events* (append *clst-events* (list event)))
    id))

(defun raw-call-model (&rest ignored)
  (declare (ignore ignored)) (incf *clst-provider-calls*))
(defun execute (&rest ignored)
  (declare (ignore ignored)) (incf *clst-effect-calls*))
(defun tick-budget-status () :ok)
(defun initiative-committed-delivery-readiness (&key audience)
  (declare (ignore audience))
  (incf *clst-delivery-readiness-calls*)
  (values t nil))

(format t "~%== Q5 contained operator scenario subject ==~%")

(let* ((core-path
         (merge-pathnames "scripts/conscious-lifecycle-scenario-core.lisp"
                          *pai-root*))
       (state-file
         (merge-pathnames "q5-lifecycle-scenario-intentions.json"
                          (test-state-dir)))
       (base-time (get-universal-time)))
  (clst-check "operator scenario core exists" (probe-file core-path))
  (when (probe-file core-path)
    (load (test-source "near-term-intentions.lisp"))
    (load (test-source "mind/conscious/lifecycle.lisp"))
    (load (test-source "lifecycle-runtime.lisp"))
    (load (test-source "lifecycle-sources.lisp"))
    (load (test-source "lifecycle-semantics.lisp"))
    (load core-path)
    (when (probe-file state-file) (delete-file state-file))
    (let ((*near-term-intention-file* state-file)
          (*near-term-intention-records* nil)
          (*near-term-intentions-mode* :enforced)
          (*autonomous-write-mode* :normal)
          (*near-term-intention-delivery-fn* nil))
      (setf *clst-events* '() *clst-next-id* 0
            *clst-provider-calls* 0 *clst-effect-calls* 0
            *clst-delivery-calls* 0 *clst-delivery-readiness-calls* 0)
      (let* ((created
               (conscious-lifecycle-scenario-run
                "create" :subject "restart fixture"
                :aim "retain one bounded intention" :now base-time))
             (intention-id (gethash "intention_id" created))
             (created-events (length *clst-events*)))
        (clst-check "real producer creates seeded model-visible lifecycle"
                    (and (string= "ok" (gethash "status" created))
                         (stringp intention-id)
                         (string= "near-term-seeded"
                                  (gethash "phase" created))
                         (= 1 (gethash "awaiting_count" created))
                         (search "near-term-seeded"
                                 (shasht:write-json
                                 (gethash "context_records" created) nil))))
        (clst-check "scenario command origin is content-free and typed"
                    (let ((origin (first *clst-events*)))
                      (and (string= "conscious-lifecycle-command-requested"
                                    (gethash "type" origin ""))
                           (null (search "restart fixture"
                                         (shasht:write-json origin nil))))))
        (clst-check "scenario records manual observation completion mode"
                    (string= "manual-observation"
                             (gethash "completion_mode"
                                      (first *near-term-intention-records*) "")))
        (let* ((source
                 (find "near-term-intention-created" *clst-events*
                       :key (lambda (event) (gethash "type" event ""))
                       :test #'string=))
               (before (length *clst-events*))
               (recovered-id
                 (conscious-lifecycle-semantic-runtime-describe
                  (format nil "near-term:~a" intention-id)
                  "deferred-intention" "dev" "topic" "restart fixture"
                  "retain one bounded intention" (gethash "id" source)
                  :source-revision "near-term-intentions-v1"
                  :actor-runtime-revision "conscious-q5-v3"
                  :disclosure-class "private-provider-eligible")))
          (clst-check "semantic retry survives actor runtime revision bump"
                      (and recovered-id (= before (length *clst-events*)))))
        (let ((before *clst-replay-calls*))
          (conscious-lifecycle-scenario-run "inspect" :now (1+ base-time))
          (conscious-lifecycle-scenario-run "inspect" :now (1+ base-time))
          (clst-check "repeated inspection reuses the installed projection"
                      (= before *clst-replay-calls*)))
        (setf *near-term-intention-records* nil
              *conscious-lifecycle-runtime-projection* nil
              *conscious-lifecycle-semantic-runtime-projection* nil)
        (near-term-intention-load)
        (let ((inspected
                (conscious-lifecycle-scenario-run
                 "inspect" :now (1+ base-time))))
          (clst-check "projection-destroying restart recovers without append"
                      (and (string= intention-id
                                    (gethash "intention_id" inspected))
                           (string= "near-term-seeded"
                                    (gethash "phase" inspected))
                           (search "restart fixture"
                                   (shasht:write-json
                                    (gethash "context_records" inspected) nil))
                           (= created-events (length *clst-events*)))))
        (let ((ready
                (conscious-lifecycle-scenario-run
                 "ready" :result-summary "bounded fixture conclusion"
                 :now (+ base-time 2))))
          (clst-check "ready producer state reaches bounded context"
                      (and (string= "near-term-ready"
                                    (gethash "phase" ready))
                           (search "near-term-ready"
                                   (shasht:write-json
                                    (gethash "context_records" ready) nil))
                           (search "bounded fixture conclusion"
                                   (shasht:write-json
                                    (gethash "context_records" ready) nil))
                           (search "provenance"
                                   (shasht:write-json
                                    (gethash "context_records" ready) nil)))))
        (setf *near-term-intention-records* nil
              *conscious-lifecycle-runtime-projection* nil
              *conscious-lifecycle-semantic-runtime-projection* nil)
        (near-term-intention-load)
        (let ((cancelled
                (conscious-lifecycle-scenario-run
                 "cancel" :now (+ base-time 3))))
          (clst-check "post-restart cancellation terminalizes exactly once"
                      (and (string= "cancelled"
                                    (gethash "lifecycle_status" cancelled))
                           (zerop (gethash "awaiting_count" cancelled))
                           (= 1 (count-if
                                 (lambda (event)
                                   (and (string=
                                         "near-term-intention-transition"
                                         (gethash "type" event ""))
                                        (string= "discarded"
                                         (gethash "state"
                                                  (gethash "payload" event)))))
                                 *clst-events*)))))
        (let ((artifact "Durable lifecycle completion is recorded."))
          (conscious-lifecycle-scenario-run
           "create" :subject "durable lifecycle completion"
           :aim "record bounded completion" :now (+ base-time 4))
          (conscious-lifecycle-scenario-run
           "ready" :result-summary artifact :now (+ base-time 5))
          (let ((completed
                  (conscious-lifecycle-scenario-run
                   "complete" :observed-reply artifact
                   :now (+ base-time 6))))
            (clst-check "observed public result terminalizes as completed"
                        (and (string= "expressed"
                                      (gethash "producer_state" completed))
                             (string= "completed"
                                      (gethash "lifecycle_status" completed))
                             (zerop (gethash "awaiting_count" completed))))
            (clst-check "completion result content stays out of context"
                        (not (search artifact
                                     (shasht:write-json
                                      (gethash "context_records" completed)
                                      nil))))))
        (clst-check "scenario spends no provider effect or delivery authority"
                    (and (zerop *clst-provider-calls*)
                         (zerop *clst-effect-calls*)
                         (zerop *clst-delivery-calls*)
                         (zerop *clst-delivery-readiness-calls*)))))
    (when (probe-file state-file) (delete-file state-file))))

(format t "~%~d passed, ~d failed~%" *clst-passed* *clst-failed*)
(when (plusp *clst-failed*) (uiop:quit 1))
