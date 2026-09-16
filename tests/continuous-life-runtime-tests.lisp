;;;; Focused provider recovery and quiescence-reappraisal checks.
;;;; harness: full-system

(in-package :agent)

(let ((checks 0))
  (labels ((check (name value)
             (incf checks)
             (unless value (error "Continuous-life check failed: ~a" name)))
           (event (id type timestamp payload)
             (obj "id" id "type" type "agent_id" "mind:test"
                  "timestamp" timestamp "payload" payload)))
    ;; One wall-clock deadline encloses the complete transport, rather than
    ;; relying only on a socket's per-read idle timeout.
    (let ((*conscious-conversation-provider-call-timeout-seconds* 0.05d0)
          (*conscious-conversation-max-output-tokens* nil))
      (check "provider wall-clock timeout"
       (handler-case
           (progn
             (%conversation-http-model-call
              (list (obj "role" "user" "content" "fixture"))
              "http://127.0.0.1:1234/v1/chat/completions"
              "fixture" 0d0
              :transport-fn
              (lambda (&rest ignored)
                (declare (ignore ignored))
                (sleep 0.25d0)
                (obj)))
             nil)
         (conscious-conversation-provider-timeout (condition)
           (multiple-value-bind (code reason status condition-type)
               (%conversation-provider-failure-details condition)
             (check "provider timeout is an ordinary error"
                    (typep condition 'error))
             (check "provider timeout has a durable failure code"
                    (and (string= "provider-call-timeout" code)
                         (eq :null status)
                         (string= "conscious-conversation-provider-timeout"
                                  condition-type)
                         (search "deadline exceeded" reason)))
             t)))))

    (check "provider call quantum defaults to 120 seconds"
           (= 120 *conscious-conversation-provider-call-timeout-seconds*))

    (let* ((*conscious-conversation-persona-profile*
             (obj "fingerprint" "continuous-life-fixture"))
           (prompt "private investigation")
           (identity-id nil)
           (voice-id nil))
      (multiple-value-setq (identity-id voice-id)
        (%conversation-persona-source-ids))
      (let* ((opened
               (obj
                "private_request"
                (vector (obj "section" "triggering-stimuli"
                             "content" prompt))
                "manifest"
                (obj "evidence_event_ids" (vector identity-id voice-id))))
             (messages
               (%recursive-base-model-messages opened prompt t (vector)))
             (discipline (car (last messages))))
        (check "private quantum preserves exact evidenced stimulus"
               (= 4 (length messages)))
        (check "private quantum discipline is a separate system instruction"
               (and (string= "system" (gethash "role" discipline ""))
                    (search "Return control to Lisp promptly"
                            (gethash "content" discipline ""))
                    (search "later model boundary or quiet cycle"
                            (gethash "content" discipline ""))))))

    (labels ((stream-wire (&rest chunks)
               (with-output-to-string (stream)
                 (dolist (chunk chunks)
                   (let ((*print-pretty* nil))
                     (format stream "data: ~a~%~%"
                             (shasht:write-json chunk nil))))
                 (format stream "data: [DONE]~%~%"))))
      (let* ((usage (obj "prompt_tokens" 20 "completion_tokens" 30
                         "total_tokens" 50 "cost" 0.001d0))
             (wire
               (stream-wire
                (obj "id" "generation:test" "model" "fixture"
                     "provider" "fixture-provider"
                     "choices"
                     (vector
                      (obj "index" 0
                           "delta" (obj "role" "assistant"
                                        "reasoning" "considering ")
                           "finish_reason" :null)))
                (obj "id" "generation:test"
                     "choices"
                     (vector
                      (obj "index" 0
                           "delta" (obj
                                    "reasoning_details"
                                    (vector
                                     (obj "type" "reasoning.text"
                                          "text" "structured evidence"
                                          "id" "reasoning:1" "index" 0))
                                    "content" "Hello ")
                           "finish_reason" :null)))
                (obj "id" "generation:test" "usage" usage
                     "choices"
                     (vector
                      (obj "index" 0 "delta" (obj "content" "world")
                           "finish_reason" "stop")))))
             (progress nil)
             (response
               (let ((*conscious-conversation-provider-progress-observer*
                       (lambda (detail) (push detail progress))))
                 (%conversation-openrouter-stream-response
                  (make-string-input-stream wire))))
             (message (%conversation-response-message response)))
        (check "stream reconstructs public content"
               (string= "Hello world" (gethash "content" message)))
        (check "stream reconstructs reasoning"
               (string= "considering "
                        (gethash "reasoning" message)))
        (check "stream preserves structured reasoning sequence"
               (= 1 (length (gethash "reasoning_details" message))))
        (check "stream retains authoritative usage"
               (= 30 (gethash "completion_tokens"
                              (gethash "usage" response))))
        (check "stream emits content-free progress"
               (and (= 3 (length progress))
                    (every (lambda (detail)
                             (not (or (gethash "content" detail)
                                      (gethash "reasoning" detail))))
                           progress)
                    (>= (gethash "reasoning_characters" (first progress) 0)
                        (length "considering structured evidence"))
                    (hash-table-p (gethash "usage" (first progress)))))))

    (labels ((stream-wire (&rest chunks)
               (with-output-to-string (stream)
                 (dolist (chunk chunks)
                   (let ((*print-pretty* nil))
                     (format stream "data: ~a~%~%"
                             (shasht:write-json chunk nil))))
                 (format stream "data: [DONE]~%~%"))))
      (let* ((wire
               (stream-wire
                (obj "id" "generation:tool"
                     "choices"
                     (vector
                      (obj "index" 0
                           "delta"
                           (obj "role" "assistant"
                                "tool_calls"
                                (vector
                                 (obj "index" 0 "id" "call:1"
                                      "type" "function"
                                      "function"
                                      (obj "name" "inspect-"
                                           "arguments" "{\"limit\":"))))
                           "finish_reason" :null)))
                (obj "id" "generation:tool"
                     "choices"
                     (vector
                      (obj "index" 0
                           "delta"
                           (obj "tool_calls"
                                (vector
                                 (obj "index" 0
                                      "function"
                                      (obj "name" "attention"
                                           "arguments" "10}"))))
                           "finish_reason" "tool_calls")))))
             (response
               (%conversation-openrouter-stream-response
                (make-string-input-stream wire)))
             (message (%conversation-response-message response))
             (call (aref (gethash "tool_calls" message) 0))
             (function (gethash "function" call)))
        (check "stream reconstructs tool name"
               (string= "inspect-attention" (gethash "name" function)))
        (check "stream reconstructs tool arguments"
               (string= "{\"limit\":10}"
                        (gethash "arguments" function)))))

    ;; An open-ended ambiguous call still has a conservative model-capacity
    ;; bound even though no artificial completion parameter is transported.
    (let ((*conscious-conversation-provider-profile*
            (obj "context_capacity_tokens" 1000000
                 "provider_routing"
                 (obj "max_price_usd_per_million"
                      (obj "prompt" 0.2d0 "completion" 0.4d0)))))
      (check "model-capacity ambiguity bound"
             (< (abs (- 0.41d0
                        (%conversation-openrouter-unbounded-outcome-cost-bound
                         0.01d0)))
                1d-9)))

    ;; A register is quiet during its lease, then becomes eligible for a new
    ;; attention pass. Old page receipts do not settle the new pass.
    (let* ((*conscious-recursive-mind-agent-id* "mind:test")
           (*conscious-recursive-curiosity-quiescent-reappraisal-seconds* 1800)
           (now 5000)
           (quiescence
             (event 10 "recursive-curiosity-attention-quiescent" (- now 60)
                    (obj "register_revision" "register:r")))
           (old-terminal
             (event 9 "recursive-curiosity-attention-declined" (- now 120)
                    (obj "register_revision" "register:r"
                         "page_revision" "page:p")))
           (new-terminal
             (event 11 "recursive-curiosity-attention-declined" (+ now 1801)
                    (obj "register_revision" "register:r"
                         "page_revision" "page:p"))))
      (check "fresh quiescence"
             (%recursive-curiosity-attention-quiescent-p
              (list quiescence) "register:r" now))
      (check "expired quiescence"
             (not (%recursive-curiosity-attention-quiescent-p
                   (list quiescence) "register:r" (+ now 1801))))
      (check "old page receipt excluded"
             (not (%recursive-curiosity-attention-page-settled-p
                   (list old-terminal quiescence)
                   "register:r" "page:p" 10)))
      (check "new page receipt accepted"
             (%recursive-curiosity-attention-page-settled-p
              (list old-terminal quiescence new-terminal)
              "register:r" "page:p" 10)))

    (format t "~&PASS: ~d focused continuous-life runtime checks.~%" checks)))
