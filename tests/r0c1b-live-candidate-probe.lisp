(in-package :agent)

;; Candidate-only live-image probe. This file is dropped through REPL-DROP,
;; whose self-mod lock prevents overlap with a public turn. Every provider
;; boundary is replaced by a bounded in-memory adapter and restored before the
;; probe returns.
(let* ((probe-purpose "r0c1b-candidate-probe-v10")
       (error-purpose "r0c1b-candidate-error-v10")
       (messages (list (obj "role" "user" "content" "synthetic candidate probe")))
       (fixture-tools (vector (obj "type" "function"
                                   "function" (obj "name" "candidate-probe"))))
       (ordinary-response
         (obj "choices" (vector (obj "message" (obj "content" "ordinary-ok")))
              "usage" (obj "total_tokens" 2)))
       (override-response
         (obj "choices" (vector (obj "message" (obj "content" "override-ok")))
              "usage" (obj "total_tokens" 3)))
       (expected-error
         (make-condition 'simple-error :format-control "candidate adapter error"
                         :format-arguments nil))
       (saved-base (fdefinition 'pai-base-raw-http-call-modulator))
       (ordinary-result nil)
       (override-result nil)
       (caught-error nil)
       (captured-override-body nil)
       (ordinary-adapter-calls 0)
       (override-adapter-calls 0))
  (unwind-protect
      (progn
        (setf (fdefinition 'pai-base-raw-http-call-modulator)
              (lambda (actual-messages)
                (assert (equal actual-messages messages))
                (incf ordinary-adapter-calls)
                ordinary-response))
        (let ((*timing-model-purpose* probe-purpose)
              (*call-model-temperature-override* nil)
              (*call-model-reasoning-override* nil)
              (*tools* fixture-tools))
          (setf ordinary-result
                (pai-base-raw-call-model-reasoning-fallback messages)))
        (let ((*timing-model-purpose* probe-purpose)
              (*call-model-temperature-override* 0.37d0)
              (*call-model-reasoning-override* nil)
              (*tools* fixture-tools)
              (*modulator-http-post-fn*
                (lambda (body)
                  (incf override-adapter-calls)
                  (setf captured-override-body body)
                  override-response)))
          (setf override-result
                (pai-base-raw-call-model-reasoning-fallback messages)))
        (let ((*timing-model-purpose* error-purpose)
              (*call-model-temperature-override* 0.19d0)
              (*call-model-reasoning-override* :disabled)
              (*tools* fixture-tools)
              (*modulator-http-post-fn*
                (lambda (body)
                  (declare (ignore body))
                  (incf override-adapter-calls)
                  (error expected-error))))
          (handler-case
              (pai-base-raw-call-model-reasoning-fallback messages)
            (error (condition) (setf caught-error condition)))))
    (setf (fdefinition 'pai-base-raw-http-call-modulator) saved-base))
  (let* ((events (replay-events))
         (probe-events
           (remove-if-not
            (lambda (event)
              (let* ((payload (gethash "payload" event))
                     (purpose (and payload (gethash "purpose" payload))))
                (and (stringp purpose)
                     (member purpose (list probe-purpose error-purpose)
                             :test #'string=))))
            events))
         (requests
           (remove-if-not (lambda (event)
                            (string= "model-request" (gethash "type" event)))
                          probe-events))
         (responses
           (remove-if-not (lambda (event)
                            (string= "model-response" (gethash "type" event)))
                          probe-events))
         (ordinary-request (first requests))
         (override-request (second requests))
         (error-request (third requests))
         (ordinary-response-event (first responses))
         (override-response-event (second responses))
         (error-response-event (third responses))
         (ordinary-body (gethash "request" (gethash "payload" ordinary-request)))
         (override-body (gethash "request" (gethash "payload" override-request)))
         (error-body (gethash "request" (gethash "payload" error-request)))
         (serialized (shasht:write-json (coerce probe-events 'vector) nil)))
    (assert (eq saved-base (fdefinition 'pai-base-raw-http-call-modulator)))
    (assert (and (eq ordinary-result ordinary-response)
                 (eq override-result override-response)))
    (assert (eq caught-error expected-error))
    (assert (and (= ordinary-adapter-calls 1) (= override-adapter-calls 2)))
    (assert (and (= (length probe-events) 6)
                 (= (length requests) 3) (= (length responses) 3)))
    (assert (and (= (gethash "id" ordinary-request)
                    (gethash "caused_by" ordinary-response-event))
                 (= (gethash "id" override-request)
                    (gethash "caused_by" override-response-event))
                 (= (gethash "id" error-request)
                    (gethash "caused_by" error-response-event))))
    (assert (and (string= *model* (gethash "model" ordinary-body))
                 (equalp fixture-tools (gethash "tools" ordinary-body))
                 (equalp (coerce messages 'vector)
                         (gethash "messages" ordinary-body))))
    (let ((override-match-p
            (labels ((json-equal-p (left right)
                (cond
                  ((and (hash-table-p left) (hash-table-p right))
                   (and (= (hash-table-count left) (hash-table-count right))
                        (loop for key being the hash-keys of left
                              using (hash-value left-value)
                              always
                              (multiple-value-bind (right-value found-p)
                                  (gethash key right)
                                (and found-p
                                     (json-equal-p left-value right-value))))))
                  ((and (vectorp left) (vectorp right))
                   (and (= (length left) (length right))
                        (loop for index below (length left)
                              always (json-equal-p (aref left index)
                                                   (aref right index)))))
                  ((and (numberp left) (numberp right))
                   (<= (abs (- (coerce left 'double-float)
                               (coerce right 'double-float)))
                       1.0d-8))
                  (t (equal left right)))))
              (json-equal-p captured-override-body override-body))))
      (unless override-match-p
        (error "submitted/replayed override mismatch: submitted=~a replayed=~a"
               (shasht:write-json captured-override-body nil)
               (shasht:write-json override-body nil))))
    (assert (and (<= (abs (- 0.19d0
                             (coerce (gethash "temperature" error-body)
                                     'double-float)))
                      1.0d-8)
                 (string= "none"
                          (gethash "effort" (gethash "reasoning" error-body)))))
    (assert (and (string= "ok"
                          (gethash "status" (gethash "payload"
                                                     ordinary-response-event)))
                 (string= "ok"
                          (gethash "status" (gethash "payload"
                                                     override-response-event)))
                 (string= "error"
                          (gethash "status" (gethash "payload"
                                                     error-response-event)))))
    (assert (and (not (search "Authorization" serialized :test #'char-equal))
                 (not (search "Bearer " serialized :test #'char-equal))))
    (shasht:write-json
     (obj "status" "pass" "checks" 11
          "durable_events" (length probe-events)
          "synthetic_adapter_calls" (+ ordinary-adapter-calls
                                       override-adapter-calls)
          "provider_calls" 0)
     nil)))
