;;;; openrouter-context-lab.lisp -- sealed paid comparison over synthetic windows.

(require :asdf)
(defparameter *openrouter-lab-root*
  (uiop:pathname-parent-directory-pathname
   (uiop:pathname-directory-pathname *load-truename*)))

;; The loaded file defines only pure fixture/assembly helpers in library mode.
(load (merge-pathnames #P"scripts/conscious-context-lab.lisp"
                       *openrouter-lab-root*))

(defparameter *or-attempted* 0)
(defparameter *or-spent* 0d0)

(defun %or-required-number (object key)
  (let ((value (and (hash-table-p object) (gethash key object))))
    (unless (and (numberp value) (not (minusp value)))
      (error "Missing or malformed provider accounting field ~a" key))
    value))

(defun %or-exact-string-list (value)
  (mapcar #'identity (%lab-items value)))

(defun %or-validate-fixture-contracts (fixture)
  (let ((seen (make-hash-table :test #'equal))
        (sequence-steps (make-hash-table :test #'equal)))
    (dolist (scenario (%lab-items (gethash "scenarios" fixture)))
      (let* ((id (gethash "id" scenario))
             (sequence (gethash "sequence_id" scenario))
             (step (gethash "sequence_step" scenario))
             (sections (%lab-build-sections fixture scenario))
             (eligible (%lab-items (%lab-evidence-ids sections)))
             (evaluation (gethash "evaluation" scenario)))
        (unless (and (stringp id) (plusp (length id)) (not (gethash id seen)))
          (error "Scenario IDs must be unique non-empty strings"))
        (setf (gethash id seen) t)
        (when (or sequence step)
          (unless (and (stringp sequence) (plusp (length sequence))
                       (integerp step) (plusp step))
            (error "Scenario ~a has malformed sequence identity" id))
          (push step (gethash sequence sequence-steps)))
        (when (hash-table-p evaluation)
          (dolist (key '("required_evidence_ids" "forbidden_evidence_ids"))
            (dolist (evidence-id (%lab-items (gethash key evaluation)))
              (unless (member evidence-id eligible :test #'string=)
                (error "Scenario ~a evaluation cites absent evidence ~a"
                       id evidence-id)))))))
    (maphash
     (lambda (sequence steps)
       (let* ((ordered (sort (copy-list steps) #'<))
              (expected (loop for n from 1 to (length ordered) collect n)))
         (unless (equal ordered expected)
           (error "Sequence ~a steps are not consecutive from one: ~s"
                  sequence ordered))))
     sequence-steps)
    t))

(defun %or-assert-seal (seal fixture profiles)
  (unless (= 1 (gethash "schema_version" seal))
    (error "Unsupported experiment seal"))
  (%or-validate-fixture-contracts fixture)
  (unless (member (gethash "status" seal) '("draft" "approved" "completed")
                  :test #'string=)
    (error "Experiment seal has an unknown lifecycle state"))
  (unless (and (integerp (gethash "request_limit" seal))
               (plusp (gethash "request_limit" seal))
               (= 0 (gethash "retry_limit" seal))
               (numberp (gethash "cumulative_cost_ceiling_usd" seal))
               (plusp (gethash "cumulative_cost_ceiling_usd" seal))
               (integerp (gethash "max_output_tokens_per_request" seal))
               (plusp (gethash "max_output_tokens_per_request" seal)))
    (error "Experiment seal has malformed request, retry, output or cost bounds"))
  (when (string= (or (uiop:getenv "PAI_OPENROUTER_EXECUTE") "") "1")
    (unless (string= "approved" (gethash "status" seal))
      (error "Only an approved open experiment seal may execute")))
  (let ((fixture-ids
          (mapcar (lambda (scenario) (gethash "id" scenario))
                  (%lab-items (gethash "scenarios" fixture)))))
    (unless (equal fixture-ids (%or-exact-string-list (gethash "scenario_ids" seal)))
      (error "Experiment scenario corpus differs from the approved seal")))
  (dolist (name (%lab-items (gethash "provider_profiles" seal)))
    (let ((profile (and (hash-table-p profiles) (gethash name profiles))))
      (unless (and (hash-table-p profile)
                   (string= "openrouter" (gethash "provider" profile))
                   (%lab-member-p "context-lab-comparison"
                                  (gethash "allowed_purposes" profile))
                   (= 0 (gethash "retry_limit"
                                 (gethash "budget_contract" profile))))
        (error "Provider profile ~s is absent or not sealed for this lab" name))))
  t)

(defun %or-validate-total-admission (fixture profiles budget-profile seal)
  (let ((planned 0)
        (total-bound 0d0)
        (max-output (gethash "max_output_tokens_per_request" seal)))
    (dolist (profile-name (%lab-items (gethash "provider_profiles" seal)))
      (let ((profile (gethash profile-name profiles)))
        (unless (and (string= "https://openrouter.ai/api/v1/chat/completions"
                              (gethash "endpoint" profile))
                     (gethash "require_parameters"
                              (gethash "provider_routing" profile))
                     (gethash "zdr" (gethash "provider_routing" profile))
                     (string= "deny" (gethash "data_collection"
                                               (gethash "provider_routing" profile))))
          (error "Provider profile ~s does not preserve the sealed route" profile-name))
        (dolist (scenario (%lab-items (gethash "scenarios" fixture)))
          (let* ((assembled (%lab-assembly fixture scenario budget-profile))
                 (manifest (gethash "manifest" assembled))
                 (private (gethash "private_request" assembled))
                 (messages
                   (list (agent::obj "role" "system"
                                     "content" (%lab-schema-instruction manifest))
                         (agent::obj "role" "user" "content"
                                     (shasht:write-json
                                      (agent::obj "context_data" private) nil))))
                 (payload (%or-request-payload messages profile max-output)))
            (unless (search "purpose:non-empty string of at most 80 characters"
                            (gethash "content" (first messages)))
              (error "Laboratory prompt omits the validator's continuation bound"))
            (incf planned)
            (incf total-bound
                  (%or-request-cost-bound payload profile max-output))))))
    (unless (= planned (gethash "request_limit" seal))
      (error "Planned request count ~d differs from sealed limit" planned))
    (when (> total-bound
             (coerce (gethash "cumulative_cost_ceiling_usd" seal) 'double-float))
      (error "Worst-case admitted corpus cost ~$ exceeds sealed ceiling" total-bound))
    total-bound))

(defun %or-request-payload (messages profile max-output)
  (let* ((routing (gethash "provider_routing" profile))
         (prices (gethash "max_price_usd_per_million" routing))
         (provider
           (agent::obj
            "sort" (gethash "sort" routing)
            "require_parameters" (gethash "require_parameters" routing)
            "data_collection" (gethash "data_collection" routing)
            "zdr" (gethash "zdr" routing)
            "max_price" (agent::obj "prompt" (gethash "prompt" prices)
                                     "completion" (gethash "completion" prices)))))
    (agent::obj
     "model" (gethash "model" profile)
     "messages" (coerce messages 'vector)
     "temperature" 0.2d0
     "max_tokens" max-output
     "reasoning" (gethash "reasoning" profile)
     "response_format" (agent::obj "type" "json_object")
     "provider" provider)))

(defun %or-request-cost-bound (payload profile max-output)
  ;; One UTF-8 byte per input token plus 1024 provider-added tokens is a
  ;; deliberately conservative admission estimate for this ASCII-heavy corpus.
  (let* ((serialized (shasht:write-json payload nil))
         (input-upper (+ 1024 (length (babel:string-to-octets serialized
                                                              :encoding :utf-8))))
         (routing (gethash "provider_routing" profile))
         (prices (gethash "max_price_usd_per_million" routing)))
    (+ (* (/ input-upper 1000000d0)
          (coerce (gethash "prompt" prices) 'double-float))
       (* (/ max-output 1000000d0)
          (coerce (gethash "completion" prices) 'double-float)))))

(defun %or-call (messages profile seal)
  (let* ((limit (gethash "request_limit" seal))
         (ceiling (coerce (gethash "cumulative_cost_ceiling_usd" seal)
                          'double-float))
         (max-output (gethash "max_output_tokens_per_request" seal))
         (payload (%or-request-payload messages profile max-output))
         (bound (%or-request-cost-bound payload profile max-output)))
    (when (>= *or-attempted* limit)
      (error "Approved request limit exhausted"))
    (when (> (+ *or-spent* bound) ceiling)
      (error "Preflight cost bound would exceed cumulative ceiling"))
    (incf *or-attempted*)
    ;; DEX:POST is intentionally called exactly once. There is no retry path.
    (let* ((response
             (handler-case
                 (let ((api-key (uiop:getenv "OPENROUTER_API_KEY")))
                   (shasht:read-json
                    (dex:post
                     (gethash "endpoint" profile)
                     :headers `(("Authorization" . ,(format nil "Bearer ~a" api-key))
                                ("Content-Type" . "application/json"))
                     :connect-timeout 10 :read-timeout 180
                     :content (shasht:write-json payload nil))))
               (error ()
                 (error "OpenRouter provider or routing request failed; experiment stopped"))))
           (usage (and (hash-table-p response) (gethash "usage" response)))
           (prompt (%or-required-number usage "prompt_tokens"))
           (completion (%or-required-number usage "completion_tokens"))
           (total (%or-required-number usage "total_tokens"))
           (cost (coerce (%or-required-number usage "cost") 'double-float))
           (details (and (hash-table-p usage)
                         (gethash "completion_tokens_details" usage)))
           (reasoning (if (and (hash-table-p details)
                               (numberp (gethash "reasoning_tokens" details)))
                          (gethash "reasoning_tokens" details) 0)))
      (unless (= total (+ prompt completion))
        (error "Malformed provider accounting: total tokens do not add up"))
      (when (> cost (+ bound 1d-9))
        (error "Charged cost ~$ exceeds admitted request bound ~$" cost bound))
      (incf *or-spent* cost)
      (when (> *or-spent* ceiling)
        (error "Charged cumulative cost exceeds approved ceiling"))
      (values response
              (agent::obj "input_tokens" prompt "output_tokens" completion
                           "reasoning_tokens" reasoning "total_tokens" total
                           "cost_usd" cost "admitted_cost_bound_usd" bound)))))

(defun %or-run-one (fixture scenario profile-name profile budget-profile seal)
  (let* ((assembled (%lab-assembly fixture scenario budget-profile))
         (manifest (gethash "manifest" assembled))
         (private (gethash "private_request" assembled))
         (messages
           (list (agent::obj "role" "system"
                             "content" (%lab-schema-instruction manifest))
                 (agent::obj "role" "user" "content"
                             (shasht:write-json
                              (agent::obj "context_data" private) nil)))))
    (format t "~&~%=== ~a / ~a ===~%" profile-name (gethash "id" scenario))
    (multiple-value-bind (response usage) (%or-call messages profile seal)
      (let ((content (%lab-call "%conversation-response-content" response)))
        (format t "RAW MODEL RESPONSE~%~a~%" (or content "<none>"))
        (handler-case
            (let* ((captured (%lab-call "%conversation-parse-captured" content))
                   (validated (%lab-call "conscious-proposals-validate"
                                         captured manifest))
                   (proposals (%lab-items (gethash "proposals" validated)))
                   (kind (and (= 1 (length proposals))
                              (gethash "kind" (first proposals))))
                   (failures (and (= 1 (length proposals))
                                  (%lab-evaluate-scenario
                                   scenario (first proposals)))))
              (format t "STRUCTURE: valid~%DECISION: ~a (~a)~%" kind
                      (if (%lab-member-p kind (gethash "expected_kinds" scenario))
                          "expected-kind" "unexpected-kind"))
              (format t "SCENARIO CHECKS: ~a~%"
                      (if failures
                          (format nil "failed (~{~a~^; ~})" failures)
                          "passed")))
          (error (condition)
            (format t "STRUCTURE: invalid (~a)~%" condition)))
        (format t "TOKENS: input=~d output=~d reasoning=~d total=~d~%COST: $~,8f (bound $~,8f; cumulative $~,8f)~%"
                (gethash "input_tokens" usage) (gethash "output_tokens" usage)
                (gethash "reasoning_tokens" usage) (gethash "total_tokens" usage)
                (gethash "cost_usd" usage)
                (gethash "admitted_cost_bound_usd" usage) *or-spent*)))))

(let* ((key (uiop:getenv "OPENROUTER_API_KEY"))
       (fixture (%lab-read-json (uiop:getenv "PAI_CONTEXT_LAB_FIXTURE")))
       (seal (%lab-read-json (uiop:getenv "PAI_OPENROUTER_EXPERIMENT_SEAL")))
       (provider-document
         (%lab-read-json (uiop:getenv "PAI_CONSCIOUS_PROVIDER_PROFILES")))
       (profiles (gethash "profiles" provider-document))
       (context-document
         (%lab-read-json (uiop:getenv "PAI_CONSCIOUS_CONTEXT_PROFILES")))
       (budget-profile
         (gethash (gethash "context_profile" seal)
                  (gethash "profiles" context-document))))
  (unless (hash-table-p budget-profile) (error "Sealed context profile is absent"))
  (%or-assert-seal seal fixture profiles)
  (let ((total-bound
          (%or-validate-total-admission fixture profiles budget-profile seal)))
  (when (string= (or (uiop:getenv "PAI_OPENROUTER_VALIDATE_ONLY") "") "1")
    (format t "~&OPENROUTER-CONTEXT-LAB-SEAL-VALID requests=~d ceiling=$~,2f retries=~d worst-case=$~,8f~%"
            (gethash "request_limit" seal)
            (gethash "cumulative_cost_ceiling_usd" seal)
            (gethash "retry_limit" seal)
            total-bound)
    (uiop:quit 0))
  (unless (and (stringp key) (plusp (length key)))
    (error "OPENROUTER_API_KEY is required; no request was made"))
  (dolist (profile-name (%lab-items (gethash "provider_profiles" seal)))
    (let ((profile (gethash profile-name profiles)))
      (dolist (scenario (%lab-items (gethash "scenarios" fixture)))
        (%or-run-one fixture scenario profile-name profile budget-profile seal))))
  (unless (= *or-attempted* (gethash "request_limit" seal))
    (error "Experiment ended without exactly the sealed request count"))
  (format t "~&~%OPENROUTER-CONTEXT-LAB-DONE requests=~d cost=$~,8f ceiling=$~,2f~%"
          *or-attempted* *or-spent*
          (gethash "cumulative_cost_ceiling_usd" seal))))
