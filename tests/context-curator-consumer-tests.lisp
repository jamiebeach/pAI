(in-package :agent)

(defvar *curator-consumer-pass* 0)
(defvar *curator-consumer-fail* 0)
(defvar *curator-consumer-calls* 0)
(defvar *curator-consumer-events* nil)

(defun curator-consumer-check (name condition)
  (if condition
      (progn (incf *curator-consumer-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *curator-consumer-fail*) (format t "  FAIL ~a~%" name))))

(defun log-event (type payload &key caused-by)
  (declare (ignore caused-by))
  (push (list type payload) *curator-consumer-events*)
  (length *curator-consumer-events*))

(load (test-source "context-curator-candidate.lisp"))
(load (test-source "context-curator-consumer.lisp"))

(defparameter *curator-consumer-rows*
  (list (obj "id" "appearance-1" "kind" "observation"
             "origin_class" "lived-user" "epistemic_status" "user-report"
             "grounding_status" "grounded"
             "content" "the agent is imagined with blonde hair and blue eyes.")))

(defun curator-consumer-response (content &key (cost 0.00001d0))
  (obj "id" "fixture-generation" "model" "openai/gpt-oss-120b"
       "provider" "fixture"
       "choices" (vector (obj "message" (obj "content" content)))
       "usage" (obj "prompt_tokens" 100 "completion_tokens" 50
                    "total_tokens" 150 "cost" cost)))

(defparameter *curator-consumer-select-json*
  "{\"schema_version\":1,\"decision\":\"SELECT\",\"active_task\":\"Answer the appearance question\",\"response_obligations\":[\"Use remembered appearance details\"],\"selected_context_ids\":[\"appearance-1\"],\"possible_context\":[{\"content\":\"the agent is imagined with blonde hair and blue eyes.\",\"evidence_ids\":[\"appearance-1\"]}],\"recommended_tools\":[],\"continuity_risks\":[],\"uncertainty\":{\"level\":\"low\",\"note\":\"Direct user report\"}}")

(defparameter *curator-consumer-none-json*
  "{\"schema_version\":1,\"decision\":\"NO_EXTRA_CONTEXT\",\"active_task\":null,\"response_obligations\":[],\"selected_context_ids\":[],\"possible_context\":[],\"recommended_tools\":[],\"continuity_risks\":[],\"uncertainty\":{\"level\":\"low\",\"note\":\"Greeting needs no memory\"}}")

(setf *context-curator-budget-file*
      #P"/tmp/pai-context-curator-budget-test.json"
      *context-curator-rollout-id* "fixture-rollout"
      *context-curator-mode* :off
      *context-curator-max-process-cost-credits* 1.0d0
      *context-curator-process-cost-credits* 0.0d0
      *context-curator-max-process-requests* 20
      *context-curator-process-request-count* 0
      *context-curator-anomaly-latched-p* nil
      *context-curator-stop-on-first-anomaly-p* t
      *context-curator-model-fn*
      (lambda (&rest ignored)
        (declare (ignore ignored))
        (incf *curator-consumer-calls*)
        (curator-consumer-response *curator-consumer-select-json*)))
(ignore-errors (delete-file *context-curator-budget-file*))
(context-curator-initialize-budget-ledger)
(setf *context-curator-mode* :enforced)

(let ((result (context-curator-consume "What do you look like?"
                                       *curator-consumer-rows*)))
  (curator-consumer-check "valid SELECT is consumed"
                          (string= "selected" (gethash "status" result)))
  (curator-consumer-check "concrete observer context reaches compiled block"
                          (search "blonde hair and blue eyes"
                                  (gethash "compiled_context_block" result)))
  (curator-consumer-check "one request causes exactly one model call"
                          (= 1 *curator-consumer-calls*)))

(curator-consumer-check
 "selected result populates the independent inspection seam"
 (string= "selected"
          (gethash "status" (context-curator-last-selected-private-result))))

(setf *context-curator-model-fn*
      (lambda (&rest ignored)
        (declare (ignore ignored))
        (incf *curator-consumer-calls*)
        (curator-consumer-response *curator-consumer-none-json*)))
(let ((result (context-curator-consume "Hello" *curator-consumer-rows*)))
  (curator-consumer-check "NO_EXTRA_CONTEXT is consumed"
                          (string= "no-extra-context"
                                   (gethash "status" result)))
  (curator-consumer-check
   "later abstention does not overwrite the last selected result"
   (string= "selected"
            (gethash "status" (context-curator-last-selected-private-result)))))

(let ((before *curator-consumer-calls*))
  (setf *context-curator-model-fn*
        (lambda (&rest ignored)
          (declare (ignore ignored))
          (incf *curator-consumer-calls*)
          (curator-consumer-response "not-json")))
  (let ((result (context-curator-consume "Recall this" *curator-consumer-rows*)))
    (curator-consumer-check "invalid provider output falls back"
                            (string= "fallback" (gethash "status" result)))
    (curator-consumer-check "invalid JSON identifies response-parse stage"
                            (and (string= "response-parse"
                                          (gethash "reason" result))
                                 (string= "shasht-invalid-char"
                                          (gethash "condition_type" result))
                                 (gethash "request_reserved" result)
                                 (gethash "provider_call_started" result)))
    (curator-consumer-check "invalid output is never retried"
                            (= 1 (- *curator-consumer-calls* before)))
    (curator-consumer-check "first anomaly latches the consumer off"
                            *context-curator-anomaly-latched-p*)))

(let ((before *curator-consumer-calls*))
  (let ((result (context-curator-consume "Recall this" *curator-consumer-rows*)))
    (curator-consumer-check "latched anomaly blocks the next call"
                            (string= "stop-on-first-anomaly"
                                     (gethash "reason" result)))
    (curator-consumer-check "latched anomaly makes no provider call"
                            (= before *curator-consumer-calls*))))

(let ((saved-count *context-curator-process-request-count*))
  (setf *context-curator-process-request-count* 0
        *context-curator-anomaly-latched-p* nil
        *context-curator-budget-ledger-ready-p* nil)
  (%context-curator-load-budget-ledger)
  (curator-consumer-check "durable ledger restores request count after reload"
                          (= saved-count
                             *context-curator-process-request-count*))
  (curator-consumer-check "durable ledger restores anomaly latch after reload"
                          *context-curator-anomaly-latched-p*))

(let ((before *curator-consumer-calls*))
  (setf *context-curator-anomaly-latched-p* nil
        *context-curator-process-cost-credits* 0.0d0
        *context-curator-max-process-cost-credits* 1.0d0
        *context-curator-model-fn*
        (lambda (&rest ignored)
          (declare (ignore ignored))
          (incf *curator-consumer-calls*)
          (curator-consumer-response *curator-consumer-select-json*
                                     :cost :null)))
  (let ((result (context-curator-consume "Recall this" *curator-consumer-rows*)))
    (curator-consumer-check "missing provider cost accounting falls back"
                            (string= "fallback" (gethash "status" result)))
    (curator-consumer-check "missing cost identifies accounting stage"
                            (string= "cost-accounting"
                                     (gethash "reason" result)))
    (curator-consumer-check "unknown accounting exhausts the process boundary"
                            (= *context-curator-process-cost-credits*
                               *context-curator-max-process-cost-credits*))
    (curator-consumer-check "unknown accounting makes exactly one model call"
                            (= 1 (- *curator-consumer-calls* before)))))

(let ((before *curator-consumer-calls*))
  (setf *context-curator-anomaly-latched-p* nil
        *context-curator-max-process-cost-credits* 0.0d0)
  (let ((result (context-curator-consume "Recall this" *curator-consumer-rows*)))
    (curator-consumer-check "zero runtime budget fails closed"
                            (string= "runtime-cost-boundary"
                                     (gethash "reason" result)))
    (curator-consumer-check "budget fallback makes no model call"
                            (= before *curator-consumer-calls*))))

(let ((before *curator-consumer-calls*))
  (setf *context-curator-max-process-cost-credits* 1.0d0
        *context-curator-process-cost-credits* 0.0d0
        *context-curator-max-process-requests*
        *context-curator-process-request-count*)
  (let ((result (context-curator-consume "Recall this" *curator-consumer-rows*)))
    (curator-consumer-check "exact request ceiling fails closed"
                            (string= "runtime-request-boundary"
                                     (gethash "reason" result)))
    (curator-consumer-check "request ceiling makes no provider call"
                            (= before *curator-consumer-calls*))))

(let ((source (uiop:read-file-string
               (namestring (test-source "context-curator-consumer.lisp")))))
  (curator-consumer-check
   "consumer source has no contact initiation or delivery adapter"
   (notany (lambda (needle) (search needle source :test #'char-equal))
           '("initiative-deliver" "send-telegram" "start-telegram"
             "scheduler" "public-outbound-gateway"))))

(curator-consumer-check
 "events contain metrics but no private context payload"
 (every (lambda (entry)
          (let ((payload (second entry)))
            (and (null (gethash "query" payload))
                 (null (gethash "content" payload))
                 (null (gethash "validated_response" payload)))))
        *curator-consumer-events*))

(let ((archive #P"/tmp/context-curator-budget-fixture-rollout-complete.json"))
  (ignore-errors (delete-file archive))
  (setf *context-curator-mode* :off
        *context-curator-rollout-id* "fixture-rollout"
        *context-curator-max-process-requests* 20
        *context-curator-process-request-count* 20
        *context-curator-max-process-cost-credits* 1.0d0
        *context-curator-process-cost-credits* 0.5d0
        *context-curator-stop-on-first-anomaly-p* t
        *context-curator-anomaly-latched-p* nil)
  (bt:with-lock-held (*context-curator-stats-lock*)
    (%context-curator-save-budget-ledger-locked))
  (setf *context-curator-rollout-id* "fixture-next"
        *context-curator-max-process-requests* 5
        *context-curator-max-process-cost-credits* 0.01d0)
  (let ((result (context-curator-rollover-budget-ledger)))
    (curator-consumer-check "completed prior ledger rolls over"
                            (string= "rolled-over" (gethash "status" result)))
    (curator-consumer-check "rollover preserves the prior ledger archive"
                            (not (null (probe-file archive))))
    (curator-consumer-check "new rollout begins with a durable zero ledger"
                            (and *context-curator-budget-ledger-ready-p*
                                 (zerop *context-curator-process-request-count*)
                                 (zerop *context-curator-process-cost-credits*))))
  (ignore-errors (delete-file archive)))

(let ((archive #P"/tmp/context-curator-budget-fixture-anomaly-anomaly.json"))
  (ignore-errors (delete-file archive))
  (setf *context-curator-mode* :off
        *context-curator-rollout-id* "fixture-anomaly"
        *context-curator-max-process-requests* 5
        *context-curator-process-request-count* 3
        *context-curator-max-process-cost-credits* 0.01d0
        *context-curator-process-cost-credits* 0.0025d0
        *context-curator-stop-on-first-anomaly-p* t
        *context-curator-anomaly-latched-p* t)
  (bt:with-lock-held (*context-curator-stats-lock*)
    (%context-curator-save-budget-ledger-locked))
  (setf *context-curator-rollout-id* "fixture-v7c"
        *context-curator-max-process-requests* 5
        *context-curator-max-process-cost-credits* 0.01d0)
  (let ((result (context-curator-archive-anomalous-budget-ledger)))
    (curator-consumer-check
     "bounded anomalous prior ledger archives explicitly"
     (string= "archived-anomaly" (gethash "status" result)))
    (curator-consumer-check
     "anomalous archival preserves the prior ledger"
     (let ((prior (and (probe-file archive)
                       (shasht:read-json (uiop:read-file-string archive)))))
       (and prior
            (string= "fixture-anomaly" (gethash "rollout_id" prior))
            (gethash "anomaly_latched" prior))))
    (curator-consumer-check
     "anomalous archival begins a distinct durable zero ledger"
     (and *context-curator-budget-ledger-ready-p*
          (string= "fixture-v7c" *context-curator-rollout-id*)
          (zerop *context-curator-process-request-count*)
          (zerop *context-curator-process-cost-credits*)
          (not *context-curator-anomaly-latched-p*))))
  (let ((before (uiop:read-file-string *context-curator-budget-file*))
        (rejected nil))
    (handler-case (context-curator-archive-anomalous-budget-ledger)
      (error () (setf rejected t)))
    (curator-consumer-check
     "anomaly archival refuses to reset the fresh envelope"
     (and rejected
          (string= before
                   (uiop:read-file-string *context-curator-budget-file*)))))
  (ignore-errors (delete-file archive)))

(format t "~%~a passed, ~a failed~%"
        *curator-consumer-pass* *curator-consumer-fail*)
(ignore-errors (delete-file *context-curator-budget-file*))
(when (plusp *curator-consumer-fail*) (sb-ext:exit :code 1))
