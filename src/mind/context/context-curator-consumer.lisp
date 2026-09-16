;;;; context-curator-consumer.lisp -- optional bounded V7b pre-render call.

(in-package :agent)

(export '(context-curator-consume context-curator-report
          context-curator-current-private-result
          context-curator-last-selected-private-result
          context-curator-initialize-budget-ledger
          context-curator-rollover-budget-ledger
          context-curator-archive-anomalous-budget-ledger))

(defvar *context-curator-mode* :off)
(defparameter *context-curator-model*
  (or (uiop:getenv "PAI_CONTEXT_CURATOR_MODEL") "openai/gpt-oss-120b"))
(defparameter *context-curator-temperature* 0.0d0)
(defparameter *context-curator-max-tokens* 1200)
(defparameter *context-curator-timeout-seconds* 12)
(defun %context-curator-cost-limit-from-environment ()
  (let ((raw (uiop:getenv "PAI_CONTEXT_CURATOR_MAX_COST_CREDITS")))
    (when (and raw (plusp (length raw)))
      (let ((*read-eval* nil))
        (multiple-value-bind (value position)
            (read-from-string raw nil nil)
          (and (numberp value) (= position (length raw))
               (float value 1.0d0)))))))
(defparameter *context-curator-max-process-cost-credits*
  (or (ignore-errors (%context-curator-cost-limit-from-environment))
      0.0d0)
  "Zero disables paid curator calls until deployment supplies a runtime cap.")
(defun %context-curator-positive-integer-environment (name)
  (let ((raw (uiop:getenv name)))
    (and raw
         (ignore-errors
           (let ((value (parse-integer raw :junk-allowed nil)))
             (and (plusp value) value))))))
(defparameter *context-curator-max-process-requests*
  (or (%context-curator-positive-integer-environment
       "PAI_CONTEXT_CURATOR_MAX_REQUESTS")
      0)
  "Zero disables paid curator calls until an exact request budget is supplied.")
(defparameter *context-curator-stop-on-first-anomaly-p*
  (string-equal (or (uiop:getenv
                     "PAI_CONTEXT_CURATOR_STOP_ON_FIRST_ANOMALY")
                    "true")
                "true"))
(defparameter *context-curator-rollout-id*
  (or (uiop:getenv "PAI_CONTEXT_CURATOR_ROLLOUT_ID") "disabled"))
(defparameter *context-curator-budget-file*
  (pathname (or (uiop:getenv "PAI_CONTEXT_CURATOR_BUDGET_FILE")
                "/agent/state/context-curator-budget.json")))
(defvar *context-curator-model-fn* nil
  "Deterministic test seam: (messages model temperature max-tokens) -> response.")
(defvar *context-curator-stats* (make-hash-table :test #'equal))
(defvar *context-curator-stats-lock* (bt:make-lock "context-curator-stats"))
(defvar *context-curator-last-private-result* nil)
(defvar *context-curator-last-selected-private-result* nil)
(defvar *context-curator-process-cost-credits* 0.0d0)
(defvar *context-curator-process-request-count* 0)
(defvar *context-curator-anomaly-latched-p* nil)
(defvar *context-curator-budget-ledger-ready-p* nil)
(defvar *context-curator-budget-ledger-status* "not-loaded")

(defun %context-curator-consumer-mode ()
  (if (boundp '*context-curator-mode*) *context-curator-mode* :off))

(defun %context-curator-budget-object ()
  (obj "schema_version" 1
       "rollout_id" *context-curator-rollout-id*
       "max_requests" *context-curator-max-process-requests*
       "max_cost_credits" *context-curator-max-process-cost-credits*
       "stop_on_first_anomaly"
       (if *context-curator-stop-on-first-anomaly-p* t nil)
       "request_count" *context-curator-process-request-count*
       "cost_credits" *context-curator-process-cost-credits*
       "anomaly_latched" (if *context-curator-anomaly-latched-p* t nil)))

(defun %context-curator-save-budget-ledger-locked ()
  (let ((temporary
          (make-pathname :name "context-curator-budget-tmp" :type "json"
                         :defaults *context-curator-budget-file*)))
    (ensure-directories-exist *context-curator-budget-file*)
    (with-open-file (stream temporary :direction :output :if-exists :supersede
                            :if-does-not-exist :create
                            :external-format :utf-8)
      (write-string (shasht:write-json (%context-curator-budget-object) nil)
                    stream)
      (terpri stream)
      (finish-output stream))
    (uiop:rename-file-overwriting-target temporary
                                          *context-curator-budget-file*)
    (setf *context-curator-budget-ledger-ready-p* t
          *context-curator-budget-ledger-status* "ready")))

(defun %context-curator-load-budget-ledger ()
  (bt:with-lock-held (*context-curator-stats-lock*)
    (setf *context-curator-budget-ledger-ready-p* nil)
    (cond
      ((not (probe-file *context-curator-budget-file*))
       (setf *context-curator-budget-ledger-status* "missing"))
      (t
       (handler-case
           (let ((data (shasht:read-json
                        (uiop:read-file-string
                         *context-curator-budget-file*))))
             (unless (and (= (gethash "schema_version" data -1) 1)
                          (string= (gethash "rollout_id" data "")
                                   *context-curator-rollout-id*)
                          (= (gethash "max_requests" data -1)
                             *context-curator-max-process-requests*)
                          (= (gethash "max_cost_credits" data -1)
                             *context-curator-max-process-cost-credits*)
                          (eq (not (null (gethash "stop_on_first_anomaly" data)))
                              *context-curator-stop-on-first-anomaly-p*))
               (error "Curator budget ledger does not match this rollout."))
             (setf *context-curator-process-request-count*
                   (gethash "request_count" data 0)
                   *context-curator-process-cost-credits*
                   (gethash "cost_credits" data 0.0d0)
                   *context-curator-anomaly-latched-p*
                   (not (null (gethash "anomaly_latched" data)))
                   *context-curator-budget-ledger-ready-p* t
                   *context-curator-budget-ledger-status* "ready"))
         (error ()
           (setf *context-curator-anomaly-latched-p* t
                 *context-curator-budget-ledger-status* "invalid")))))))

(defun context-curator-initialize-budget-ledger ()
  "Out-of-band, idempotent rollout initialization; forbidden once enabled."
  (unless (eq (%context-curator-consumer-mode) :off)
    (error "Curator budget ledger can be initialized only while curator mode is OFF."))
  (unless (and (stringp *context-curator-rollout-id*)
               (plusp (length *context-curator-rollout-id*))
               (not (string= *context-curator-rollout-id* "disabled"))
               (plusp *context-curator-max-process-requests*)
               (plusp *context-curator-max-process-cost-credits*)
               *context-curator-stop-on-first-anomaly-p*)
    (error "Curator rollout envelope is unavailable or unsafe."))
  (bt:with-lock-held (*context-curator-stats-lock*)
    (when (probe-file *context-curator-budget-file*)
      (error "Curator budget ledger already exists; refusing to reset it."))
    (setf *context-curator-process-request-count* 0
          *context-curator-process-cost-credits* 0.0d0
          *context-curator-anomaly-latched-p* nil)
    (%context-curator-save-budget-ledger-locked)
    (%context-curator-budget-object)))

(defun %context-curator-safe-rollout-id-p (value)
  (and (stringp value) (plusp (length value))
       (every (lambda (character)
                (or (alphanumericp character) (char= character #\-)))
              value)))

(defun context-curator-rollover-budget-ledger ()
  "Archive one completed, anomaly-free prior envelope and initialize this one.
Out-of-band only. The prior ledger is never reset or overwritten."
  (unless (eq (%context-curator-consumer-mode) :off)
    (error "Curator budget rollover requires curator mode OFF."))
  (unless (probe-file *context-curator-budget-file*)
    (error "Prior curator budget ledger is missing."))
  (let* ((prior (shasht:read-json
                 (uiop:read-file-string *context-curator-budget-file*)))
         (prior-id (gethash "rollout_id" prior ""))
         (prior-max (gethash "max_requests" prior 0))
         (prior-count (gethash "request_count" prior -1))
         (prior-cost-cap (gethash "max_cost_credits" prior 0))
         (prior-cost (gethash "cost_credits" prior -1))
         (archive
           (make-pathname
            :name (format nil "context-curator-budget-~a-complete" prior-id)
            :type "json" :defaults *context-curator-budget-file*)))
    (unless (and (= (gethash "schema_version" prior -1) 1)
                 (%context-curator-safe-rollout-id-p prior-id)
                 (not (string= prior-id *context-curator-rollout-id*))
                 (integerp prior-max) (plusp prior-max)
                 (integerp prior-count) (= prior-count prior-max)
                 (numberp prior-cost-cap) (plusp prior-cost-cap)
                 (numberp prior-cost) (<= 0 prior-cost prior-cost-cap)
                 (gethash "stop_on_first_anomaly" prior)
                 (not (gethash "anomaly_latched" prior)))
      (error "Prior curator ledger is not a completed anomaly-free envelope."))
    (when (probe-file archive)
      (error "Completed curator ledger archive already exists."))
    (bt:with-lock-held (*context-curator-stats-lock*)
      (uiop:rename-file-overwriting-target *context-curator-budget-file* archive)
      (handler-case
          (progn
            (setf *context-curator-process-request-count* 0
                  *context-curator-process-cost-credits* 0.0d0
                  *context-curator-anomaly-latched-p* nil
                  *context-curator-budget-ledger-ready-p* nil
                  *context-curator-budget-ledger-status* "rollover-in-progress")
            (%context-curator-save-budget-ledger-locked)
            (obj "schema_version" 1 "status" "rolled-over"
                 "prior_rollout_id" prior-id
                 "prior_request_count" prior-count
                 "prior_cost_credits" prior-cost
                 "new_rollout_id" *context-curator-rollout-id*
                 "new_request_count" 0 "new_cost_credits" 0.0d0
                 "archive_file" (file-namestring archive)))
        (error (condition)
          (ignore-errors (delete-file *context-curator-budget-file*))
          (uiop:rename-file-overwriting-target archive
                                                *context-curator-budget-file*)
          (setf *context-curator-budget-ledger-ready-p* nil
                *context-curator-budget-ledger-status* "rollover-failed")
          (error condition))))))

(defun context-curator-archive-anomalous-budget-ledger ()
  "Archive one bounded anomaly-latched envelope and initialize this one.
Out-of-band only. The anomalous ledger is preserved and never reset in place."
  (unless (eq (%context-curator-consumer-mode) :off)
    (error "Anomalous curator budget archival requires curator mode OFF."))
  (unless (probe-file *context-curator-budget-file*)
    (error "Prior anomalous curator budget ledger is missing."))
  (let* ((prior (shasht:read-json
                 (uiop:read-file-string *context-curator-budget-file*)))
         (prior-id (gethash "rollout_id" prior ""))
         (prior-max (gethash "max_requests" prior 0))
         (prior-count (gethash "request_count" prior -1))
         (prior-cost-cap (gethash "max_cost_credits" prior 0))
         (prior-cost (gethash "cost_credits" prior -1))
         (archive
           (make-pathname
            :name (format nil "context-curator-budget-~a-anomaly" prior-id)
            :type "json" :defaults *context-curator-budget-file*)))
    (unless (and (= (gethash "schema_version" prior -1) 1)
                 (%context-curator-safe-rollout-id-p prior-id)
                 (not (string= prior-id *context-curator-rollout-id*))
                 (integerp prior-max) (plusp prior-max)
                 (integerp prior-count) (<= 0 prior-count prior-max)
                 (numberp prior-cost-cap) (plusp prior-cost-cap)
                 (numberp prior-cost) (<= 0 prior-cost prior-cost-cap)
                 (gethash "stop_on_first_anomaly" prior)
                 (gethash "anomaly_latched" prior))
      (error "Prior curator ledger is not a bounded anomaly-latched envelope."))
    (when (probe-file archive)
      (error "Anomalous curator ledger archive already exists."))
    (bt:with-lock-held (*context-curator-stats-lock*)
      (uiop:rename-file-overwriting-target *context-curator-budget-file* archive)
      (handler-case
          (progn
            (setf *context-curator-process-request-count* 0
                  *context-curator-process-cost-credits* 0.0d0
                  *context-curator-anomaly-latched-p* nil
                  *context-curator-budget-ledger-ready-p* nil
                  *context-curator-budget-ledger-status* "rollover-in-progress")
            (%context-curator-save-budget-ledger-locked)
            (obj "schema_version" 1 "status" "archived-anomaly"
                 "prior_rollout_id" prior-id
                 "prior_request_count" prior-count
                 "prior_cost_credits" prior-cost
                 "new_rollout_id" *context-curator-rollout-id*
                 "new_request_count" 0 "new_cost_credits" 0.0d0
                 "archive_file" (file-namestring archive)))
        (error (condition)
          (ignore-errors (delete-file *context-curator-budget-file*))
          (uiop:rename-file-overwriting-target archive
                                                *context-curator-budget-file*)
          (setf *context-curator-budget-ledger-ready-p* nil
                *context-curator-budget-ledger-status* "rollover-failed")
          (error condition))))))

(defun %context-curator-budget-boundary-reason ()
  (cond
    ((not *context-curator-budget-ledger-ready-p*)
     "budget-ledger-unavailable")
    (*context-curator-anomaly-latched-p* "stop-on-first-anomaly")
    ((or (not (integerp *context-curator-max-process-requests*))
         (<= *context-curator-max-process-requests* 0)
         (>= *context-curator-process-request-count*
             *context-curator-max-process-requests*))
     "runtime-request-boundary")
    ((or (not (numberp *context-curator-max-process-cost-credits*))
         (<= *context-curator-max-process-cost-credits* 0)
         (>= *context-curator-process-cost-credits*
             *context-curator-max-process-cost-credits*))
     "runtime-cost-boundary")
    (t nil)))

(defun %context-curator-reserve-request ()
  "Atomically recheck the durable envelope and reserve one provider call."
  (bt:with-lock-held (*context-curator-stats-lock*)
    (let ((reason (%context-curator-budget-boundary-reason)))
      (if reason
          (values nil reason)
          (progn
            (incf *context-curator-process-request-count*)
            ;; Persistence precedes the external side effect. If this fails,
            ;; no provider call occurs and the handler latches the rollout.
            (%context-curator-save-budget-ledger-locked)
            (values t nil))))))

(defun %context-curator-consumer-stat (key &optional (amount 1))
  (bt:with-lock-held (*context-curator-stats-lock*)
    (incf (gethash key *context-curator-stats* 0) amount)))

(defun %context-curator-http-call (messages model temperature max-tokens)
  (unless (and (boundp '*api-key*) (stringp *api-key*)
               (plusp (length *api-key*)))
    (error "OpenRouter API key is unavailable."))
  (let ((body (obj "model" model "messages" (coerce messages 'vector)
                   "temperature" temperature "max_tokens" max-tokens
                   "stream" nil
                   "response_format" (obj "type" "json_object"))))
    (shasht:read-json
     (dex:post *endpoint*
               :headers `(("Authorization" . ,(format nil "Bearer ~a" *api-key*))
                          ("Content-Type" . "application/json")
                          ("X-OpenRouter-Title" . "the agent Context Curator"))
               :connect-timeout (min *http-connect-timeout*
                                     *context-curator-timeout-seconds*)
               :read-timeout *context-curator-timeout-seconds*
               :content (shasht:write-json body nil)))))

(defun %context-curator-invoke (messages)
  (if *context-curator-model-fn*
      (funcall *context-curator-model-fn* messages *context-curator-model*
               *context-curator-temperature* *context-curator-max-tokens*)
      (%context-curator-http-call messages *context-curator-model*
                                  *context-curator-temperature*
                                  *context-curator-max-tokens*)))

(defun %context-curator-response-content (response)
  (let ((content
          (and (hash-table-p response)
               (ignore-errors (ref response "choices" 0 "message" "content")))))
    (unless (and (stringp content) (plusp (length content)))
      (error "Curator response has no usable assistant content."))
    content))

(defun %context-curator-response-metrics (response elapsed-ms)
  (let ((usage (and (hash-table-p response) (gethash "usage" response))))
    (obj "latency_ms" elapsed-ms
         "model" (or (and (hash-table-p response) (gethash "model" response))
                     *context-curator-model*)
         "provider" (or (and (hash-table-p response) (gethash "provider" response))
                        :null)
         "generation_id" (or (and (hash-table-p response) (gethash "id" response))
                             :null)
         "prompt_tokens" (or (and usage (gethash "prompt_tokens" usage)) :null)
         "completion_tokens"
         (or (and usage (gethash "completion_tokens" usage)) :null)
         "total_tokens" (or (and usage (gethash "total_tokens" usage)) :null)
         "cost_credits" (or (and usage (gethash "cost" usage)) :null))))

(defun %context-curator-store-result (result)
  (bt:with-lock-held (*context-curator-stats-lock*)
    (setf *context-curator-last-private-result* result)
    (when (string= (gethash "status" result "") "selected")
      (setf *context-curator-last-selected-private-result* result)))
  result)

(defun context-curator-current-private-result ()
  "Authenticated admin callers only; includes private validated annotations."
  (bt:with-lock-held (*context-curator-stats-lock*)
    (or *context-curator-last-private-result*
        (obj "schema_version" 1 "status" "unavailable"))))

(defun context-curator-last-selected-private-result ()
  "Authenticated admin inspection seam; abstentions never overwrite it."
  (bt:with-lock-held (*context-curator-stats-lock*)
    (or *context-curator-last-selected-private-result*
        (obj "schema_version" 1 "status" "unavailable"))))

(defun context-curator-report ()
  (bt:with-lock-held (*context-curator-stats-lock*)
    (let ((counts (obj)))
      (maphash (lambda (key value) (setf (gethash key counts) value))
               *context-curator-stats*)
      (obj "schema_version" 1
           "mode" (string-downcase
                    (symbol-name (%context-curator-consumer-mode)))
           "model" *context-curator-model*
           "max_tokens" *context-curator-max-tokens*
           "timeout_seconds" *context-curator-timeout-seconds*
           "max_process_cost_credits" *context-curator-max-process-cost-credits*
           "process_cost_credits" *context-curator-process-cost-credits*
           "max_process_requests" *context-curator-max-process-requests*
           "process_request_count" *context-curator-process-request-count*
           "stop_on_first_anomaly"
           (if *context-curator-stop-on-first-anomaly-p* t nil)
           "anomaly_latched" (if *context-curator-anomaly-latched-p* t nil)
           "rollout_id" *context-curator-rollout-id*
           "budget_ledger_status" *context-curator-budget-ledger-status*
           "counts" counts))))

(defun context-curator-consume (query candidate-rows &key tools
                                                      (as-of (get-universal-time)))
  "Make at most one tool-free curator call and return validated context data.
Failure is represented as FALLBACK and never calls another model."
  (let ((mode (%context-curator-consumer-mode))
        (failure-stage "manifest-build")
        (request-reserved-p nil)
        (provider-call-started-p nil))
    (when (eq mode :off)
      (return-from context-curator-consume
        (obj "schema_version" 1 "status" "disabled"
             "compiled_context_block" :null)))
    (handler-case
        (let (manifest messages started response elapsed-ms metrics content
              parsed validated decision compiled cost)
          (setf manifest (context-curator-build-manifest
                          query candidate-rows :tools tools :as-of as-of)
                failure-stage "request-build")
          (setf messages (context-curator-build-request manifest)
                failure-stage "budget-reservation")
          (multiple-value-bind (reserved-p reason)
              (%context-curator-reserve-request)
            (unless reserved-p
              (%context-curator-consumer-stat "budget-fallback")
              (return-from context-curator-consume
                (obj "schema_version" 1 "status" "fallback"
                     "reason" reason "compiled_context_block" :null)))
            (setf request-reserved-p t))
          (setf started (get-internal-real-time)
                failure-stage "provider-call"
                provider-call-started-p t
                response (%context-curator-invoke messages)
                elapsed-ms
                (* 1000.0d0
                   (/ (- (get-internal-real-time) started)
                      internal-time-units-per-second))
                failure-stage "response-metrics"
                metrics (%context-curator-response-metrics response elapsed-ms)
                failure-stage "response-content"
                content (%context-curator-response-content response)
                failure-stage "response-parse"
                parsed (shasht:read-json content)
                failure-stage "response-validation"
                validated (context-curator-validate-response parsed manifest)
                decision (gethash "decision" validated)
                failure-stage "response-compilation"
                compiled (context-curator-compile-block validated manifest)
                failure-stage "cost-accounting"
                cost (gethash "cost_credits" metrics))
          (unless (and (numberp cost) (>= cost 0))
            ;; Unknown accounting cannot safely remain enabled for later calls.
            (bt:with-lock-held (*context-curator-stats-lock*)
              (setf *context-curator-process-cost-credits*
                    *context-curator-max-process-cost-credits*)
              (%context-curator-save-budget-ledger-locked))
            (error "Curator provider omitted usable cost accounting."))
          (setf failure-stage "ledger-commit")
          (bt:with-lock-held (*context-curator-stats-lock*)
            (incf *context-curator-process-cost-credits* cost)
            (%context-curator-save-budget-ledger-locked))
          (%context-curator-consumer-stat
           (if (string= decision "SELECT") "selected" "no-extra-context"))
          (let ((result
                  (obj "schema_version" 1
                       "status" (if (string= decision "SELECT")
                                    "selected" "no-extra-context")
                       "mode" (string-downcase (symbol-name mode))
                       "validated_response" validated
                       "metrics" metrics
                       "compiled_context_block" compiled)))
            (ignore-errors
              (when (fboundp 'log-event)
                (funcall 'log-event "context-curator-consumed"
                         (obj "status" (gethash "status" result)
                              "model" *context-curator-model*
                              "latency_ms" elapsed-ms
                              "prompt_tokens" (gethash "prompt_tokens" metrics)
                              "completion_tokens"
                              (gethash "completion_tokens" metrics)
                              "cost_credits" cost
                              "selected_count"
                              (length (%context-curator-list
                                       (gethash "selected_context_ids"
                                                validated))))))
            (%context-curator-store-result result))))
      (error (condition)
        (%context-curator-consumer-stat "fallback-errors")
        (%context-curator-consumer-stat
         (format nil "fallback-error-~a" failure-stage))
        (when *context-curator-stop-on-first-anomaly-p*
          (bt:with-lock-held (*context-curator-stats-lock*)
            (setf *context-curator-anomaly-latched-p* t)
            (when *context-curator-budget-ledger-ready-p*
              (ignore-errors (%context-curator-save-budget-ledger-locked)))))
        (let ((result
                (obj "schema_version" 1 "status" "fallback"
                     "reason" failure-stage
                     "condition_type" (string-downcase
                                       (symbol-name (type-of condition)))
                     "request_reserved" (if request-reserved-p t nil)
                     "provider_call_started"
                     (if provider-call-started-p t nil)
                     "compiled_context_block" :null)))
          (ignore-errors
            (when (fboundp 'log-event)
              (funcall 'log-event "context-curator-fallback"
                       (obj "reason" (gethash "reason" result)
                            "condition_type" (gethash "condition_type" result)
                            "request_reserved"
                            (gethash "request_reserved" result)
                            "provider_call_started"
                            (gethash "provider_call_started" result)
                            "model" *context-curator-model*))))
          (%context-curator-store-result result))))))

(define-init :restore context-curator-consumer-restore
    "Restore durable state for context-curator-consumer."
  (%context-curator-load-budget-ledger))
