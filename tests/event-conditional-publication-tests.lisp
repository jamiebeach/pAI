;;;; harness: full-system
(in-package :agent)

(uiop:call-with-temporary-file
 (lambda (path)
   (let ((backend (make-sqlite-storage path))
         (*event-authority-port* nil) (*event-ring* nil) (*event-next-id* 0)
         (*runtime-observers* (make-hash-table :test #'equal)) (observed nil)
         (*sqlite-event-authority-backend* nil) (*sqlite-event-authority-agent-id* nil)
         (*sqlite-event-authority-checkpoint-backend* nil)
         (*sqlite-event-authority-database* nil) (*sqlite-event-authority-derived-database* nil))
     (unwind-protect
          (progn
            (%sqlite-authority-install backend path backend path "conditional-fixture")
            (runtime-observer-register "conditional-fixture" "fixture-observer"
                                       (lambda (event) (push event observed)))
            (multiple-value-bind (id durable event)
                (log-event "conditional-fixture" (obj "value" 1) :expected-head 0)
              (assert durable)
              (assert (= id (gethash "id" event)))
              (assert (= id *event-next-id*))
              (assert (eq event (first *event-ring*)))
              (assert (equal observed (list event)))
              (assert (= id (gethash "id" (storage-read-event backend id :agent-id "conditional-fixture")))))
            (let ((ring *event-ring*) (head (storage-head-position backend)))
              (multiple-value-bind (id durable event)
                  (log-event "conditional-fixture" (obj) :expected-head 0)
                (assert (null id)) (assert (null durable)) (assert (null event)))
              (assert (eq ring *event-ring*))
              (assert (= 1 (length observed)))
              (assert (= head (storage-head-position backend))))
            ;; Ordinary append is still supported after a conditional refusal.
            (assert (nth-value 1 (log-event "ordinary-fixture" (obj))))
            (let ((budget-observed 0)
                  (policy (obj "authorization_id" "publication-auth"
                               "agent_id" "conditional-fixture"
                               "persona_id" "fixture-persona"
                               "generation" "fixture-generation"
                               "prior_exposure_microusd" 0
                               "ceiling_microusd" 100
                               "per_request_ceiling_microusd" 100)))
              (runtime-observer-register
               "context-graph-identity-phase" "budget-observer"
               (lambda (event) (declare (ignore event)) (incf budget-observed)))
              (let ((event
                      (context-graph-budget-append-phase
                       backend policy
                       (obj "persona_id" "fixture-persona" "generation" "fixture-generation"
                            "record_json" (pai.context-graph:context-graph-runtime-json
                                            (obj "phase" "facts" "request_digest" "budget-digest"
                                                 "outcome" "request" "reserved_microusd" 40)))
                       1
                       :append-fn
                       (lambda (head type payload cause)
                         (multiple-value-bind (id durable stored)
                             (log-event type payload :caused-by cause :expected-head head)
                           (declare (ignore id))
                           (and durable stored))))))
                (assert (equal "context-graph-identity-phase" (gethash "type" event)))
                (assert (= 1 budget-observed))
                (assert (= 40 (gethash "exposure_microusd"
                                      (context-graph-budget-snapshot backend policy))))))
            (let ((*event-authority-port* nil))
              (assert (handler-case (progn (log-event "must-not-fallback" (obj) :expected-head 0) nil)
                        (error () t)))))
       (event-authority-clear))))
 :want-stream-p nil :type "sqlite")
(format t "CONDITIONAL-PUBLICATION durable receipts and ring preserved; stale/unsupported writes do not fall back~%")
(format t "PASS event-conditional-publication-tests~%")
