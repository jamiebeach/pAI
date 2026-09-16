;;;; curiosity-lab.lisp -- disposable event-history laboratory for curiosity.

(require :asdf)
(load (uiop:getenv "PAI_QUICKLISP_SETUP"))
(push (pathname (format nil "~a/" (uiop:getenv "PAI_ROOT")))
      asdf:*central-registry*)
;; The lab is a source-qualification boundary.  A mounted worktree can have
;; older mtimes than a reusable container cache, so accepting ASDF's cached
;; top-level system here can silently validate yesterday's implementation.
(asdf:load-system "pai" :force t)

(in-package :agent)

(defparameter *curiosity-lab-scenario*
  (or (uiop:getenv "PAI_CURIOSITY_LAB_SCENARIO")
      "attention-starvation"))
(defparameter *curiosity-lab-model*
  (or (uiop:getenv "PAI_CURIOSITY_LAB_MODEL")
      "scripted-native-tools-v1"))
(defparameter *curiosity-lab-provider*
  (or (uiop:getenv "PAI_CURIOSITY_LAB_PROVIDER") "mock"))
(defparameter *curiosity-lab-execute-p*
  (string= "1" (or (uiop:getenv "PAI_CURIOSITY_LAB_EXECUTE") "0")))
(defparameter *curiosity-lab-validate-p*
  (string= "1" (or (uiop:getenv "PAI_CURIOSITY_LAB_VALIDATE") "0")))
(defparameter *curiosity-lab-artifact-dir*
  (uiop:ensure-directory-pathname
   (pathname (or (uiop:getenv "PAI_CURIOSITY_LAB_ARTIFACT_DIR")
                 (error "PAI_CURIOSITY_LAB_ARTIFACT_DIR is required")))))
(defvar *curiosity-lab-events* nil)
(defvar *curiosity-lab-model-requests* nil)
(defvar *curiosity-lab-model-responses* nil)
(defvar *curiosity-lab-provider-calls* 0)
(defvar *curiosity-lab-target-motive-id* nil)
(defvar *curiosity-lab-target-question* nil)
(defvar *curiosity-lab-target-evidence* nil)
(defvar *curiosity-lab-checks* nil)
(defvar *curiosity-lab-briefing-source-ids* nil)
(defvar *curiosity-lab-consolidation-register* nil)
(defvar *curiosity-lab-production-model-call*
  (symbol-function '%conversation-http-model-call))

(defun %curiosity-lab-env-number (name &optional (default 0d0))
  (let ((text (uiop:getenv name)))
    (if (and text (plusp (length text)))
        (let ((*read-eval* nil))
          (multiple-value-bind (value position) (read-from-string text)
            (unless (and (realp value) (= position (length text)))
              (error "~a must contain one number" name))
            (coerce value 'double-float)))
        default)))

(defun %curiosity-lab-env-integer (name)
  (let ((text (or (uiop:getenv name) "0")))
    (parse-integer text :junk-allowed nil)))

(defun %curiosity-lab-read-json (path)
  (shasht:read-json
   (uiop:read-file-string path :external-format :utf-8)))

(defun %curiosity-lab-configure-provider ()
  (unless (member *curiosity-lab-provider* '("mock" "openrouter")
                  :test #'string=)
    (error "Unknown curiosity lab provider ~s" *curiosity-lab-provider*))
  (when (string= *curiosity-lab-provider* "openrouter")
    (unless (not (eq *curiosity-lab-execute-p* *curiosity-lab-validate-p*))
      (error "OpenRouter lab requires exactly one of execute or validate"))
    (let* ((request-limit
             (%curiosity-lab-env-integer
              "PAI_CURIOSITY_LAB_REQUEST_LIMIT"))
           (ceiling
             (%curiosity-lab-env-number
              "PAI_CURIOSITY_LAB_COST_CEILING_USD"))
           (prompt-price
             (%curiosity-lab-env-number
              "PAI_CURIOSITY_LAB_MAX_PROMPT_PRICE"))
           (completion-price
             (%curiosity-lab-env-number
              "PAI_CURIOSITY_LAB_MAX_COMPLETION_PRICE"))
           (document
             (%curiosity-lab-read-json
              (merge-pathnames #P"config/conscious-provider-profiles.json"
                               (pathname (format nil "~a/"
                                                 (uiop:getenv "PAI_ROOT"))))))
           (original
             (gethash "openrouter-human-renderer-mimo-v2.5"
                      (gethash "profiles" document)))
           (ignored
             (unless (hash-table-p original)
               (error "OpenRouter MiMo provider profile is unavailable")))
           (profile
             (shasht:read-json (shasht:write-json original nil)))
           (routing (gethash "provider_routing" profile))
           (zdr-policy
             (or (uiop:getenv "PAI_CURIOSITY_LAB_ZDR") "require"))
           (prices (gethash "max_price_usd_per_million" routing)))
      (declare (ignore ignored))
      (unless (and (= request-limit 2) (plusp ceiling)
                   (plusp prompt-price) (plusp completion-price)
                   (member zdr-policy '("require" "allow-non-zdr")
                           :test #'string=)
                   (hash-table-p routing) (hash-table-p prices))
        (error "OpenRouter lab seal requires two requests and positive cost/price bounds"))
      (setf (gethash "model" profile) *curiosity-lab-model*
            (gethash "zdr" routing) (string= zdr-policy "require")
            (gethash "prompt" prices) prompt-price
            (gethash "completion" prices) completion-price
            *conscious-conversation-provider-profile* profile
            *conscious-conversation-cost-ceiling-usd* ceiling
            *conscious-conversation-provider-attempts* 0
            *conscious-conversation-provider-spent-usd* 0d0)
      (when *curiosity-lab-execute-p*
        (unless (plusp (length (or (uiop:getenv "OPENROUTER_API_KEY") "")))
          (error "OPENROUTER_API_KEY is required; no request was made")))
      (when *curiosity-lab-validate-p*
        (let* ((messages
                 (list
                  (obj "role" "system" "content"
                       "Synthetic private briefing validation request.")
                  (obj "role" "user" "content"
                       (make-string 6000 :initial-element #\x))))
               (briefing-bound
                 (%conversation-openrouter-request-cost-bound
                  messages "https://openrouter.ai/api/v1/chat/completions"
                  *curiosity-lab-model* 0.2d0
                  (%recursive-private-briefing-schema)))
               (consolidation-bound
                 (when (string= *curiosity-lab-scenario*
                                "curiosity-consolidation")
                   (let ((*conscious-conversation-max-output-tokens*
                           *conscious-recursive-curiosity-consolidation-max-output-tokens*))
                     (%conversation-openrouter-request-cost-bound
                      messages "https://openrouter.ai/api/v1/chat/completions"
                      *curiosity-lab-model* 0.2d0
                      (%recursive-curiosity-consolidation-schema)
                      (%recursive-curiosity-consolidation-tool-choice)))))
               (bound (max briefing-bound (or consolidation-bound 0d0))))
          (unless (<= (* request-limit bound) ceiling)
            (error "Worst-case sealed requests exceed the cumulative ceiling"))
          (format t
                  "OPENROUTER-CURIOSITY-LAB-VALID model=~a requests=~d retries=0 worst-case=$~,8f ceiling=$~,8f~%"
                  *curiosity-lab-model* request-limit (* request-limit bound)
                  ceiling)
          (uiop:quit 0))))))

(defun %curiosity-lab-write (name content)
  (with-open-file
      (out (merge-pathnames name *curiosity-lab-artifact-dir*)
           :direction :output :if-exists :supersede
           :if-does-not-exist :create :external-format :utf-8)
    (write-string content out)))

(defun %curiosity-lab-json (name value)
  (%curiosity-lab-write name (shasht:write-json value nil)))

(defun %curiosity-lab-jsonl (name rows)
  (%curiosity-lab-write
   name
   (with-output-to-string (out)
     (dolist (row rows)
       (write-string (shasht:write-json row nil) out)
       (terpri out)))))

(defun %curiosity-lab-check (name condition)
  (push (cons name (if condition t nil)) *curiosity-lab-checks*)
  (format t "~a ~a~%" (if condition "PASS" "FAIL") name)
  condition)

(defun %curiosity-lab-event (id type payload &optional caused-by)
  (obj "schema_version" 2 "id" id "type" type
       "agent_id" "curiosity-lab" "caused_by" (or caused-by :null)
       "payload" payload))

(defun %curiosity-lab-log-event (type payload &key caused-by)
  (let* ((id (1+ (length *curiosity-lab-events*)))
         (event (%curiosity-lab-event id type payload caused-by)))
    (setf *curiosity-lab-events*
          (append *curiosity-lab-events* (list event)))
    (values id t event)))

(defun %curiosity-lab-message-without-tool ()
  (obj "role" "assistant" "content" "No supplied interest pulls now."))

(defun %curiosity-lab-message-with-target ()
  (obj "role" "assistant" "content" :null
       "tool_calls"
       (vector
        (obj "id" "mock-choice" "type" "function"
             "function"
             (obj "name" "choose-curiosity"
                  "arguments"
                  (shasht:write-json
                   (obj "question" *curiosity-lab-target-question*
                        "source_motive_ids"
                        (vector *curiosity-lab-target-motive-id*)
                        "evidence_event_ids"
                        *curiosity-lab-target-evidence*)
                   nil))))))

(defun %curiosity-lab-message-with-briefing ()
  (obj "role" "assistant" "content" :null
       "tool_calls"
       (vector
        (obj "id" "mock-briefing" "type" "function"
             "function"
             (obj "name" "write-private-briefing"
                  "arguments"
                  (shasht:write-json
                   (obj "content"
                        "A small cluster of questions is active around how private curiosity becomes durable attention. The current edge is whether compact orientation can preserve useful connections without forcing disclosure. Keep the investigation grounded in its exact events and revisit it only when the source state changes.")
                   nil))))))

(defun %curiosity-lab-message-with-consolidation ()
  (let* ((first (aref *curiosity-lab-consolidation-register* 0))
         (second (aref *curiosity-lab-consolidation-register* 1))
         (third (aref *curiosity-lab-consolidation-register* 2))
         (first-two-evidence
           (concatenate
            'vector
            (gethash "observation_event_ids" first)
            (gethash "observation_event_ids" second))))
    (obj
     "role" "assistant" "content" :null
     "tool_calls"
     (vector
      (obj
       "id" "mock-consolidation" "type" "function"
       "function"
       (obj
        "name" "write-curiosity-consolidation"
        "arguments"
        (shasht:write-json
         (obj
          "threads"
          (vector
           (obj
            "question"
            "How do overlapping private observations become useful attention?"
            "source_motive_ids"
            (vector (gethash "motive_id" first)
                    (gethash "motive_id" second))
            "evidence_event_ids" first-two-evidence
            "attention_state" "foreground"
            "rationale"
            "Two distinct observations point at the same active design edge.")
           (obj
            "question" (gethash "question" third)
            "source_motive_ids" (vector (gethash "motive_id" third))
            "evidence_event_ids"
            (gethash "observation_event_ids" third)
            "attention_state" "dormant"
            "rationale"
            "The question remains available without competing for attention.")))
         nil)))))))

(defun %curiosity-lab-model-call
    (messages endpoint model temperature &key tools tool-choice)
  (incf *curiosity-lab-provider-calls*)
  (push (obj "call" *curiosity-lab-provider-calls* "model" model
             "messages" (coerce messages 'vector) "tools" tools
             "tool_choice" (or tool-choice :null)
             "max_output_tokens"
             *conscious-conversation-max-output-tokens*)
        *curiosity-lab-model-requests*)
  (let* ((response
           (if (string= *curiosity-lab-provider* "openrouter")
               (funcall *curiosity-lab-production-model-call*
                        messages endpoint model temperature :tools tools
                        :tool-choice tool-choice)
               (let ((message
                       (cond
                         ((and (string= *curiosity-lab-scenario*
                                        "attention-starvation")
                               (= *curiosity-lab-provider-calls* 2))
                          (%curiosity-lab-message-with-target))
                         ((and (string= *curiosity-lab-scenario*
                                        "private-briefing")
                               (= *curiosity-lab-provider-calls* 1))
                          (%curiosity-lab-message-with-briefing))
                         ((and (string= *curiosity-lab-scenario*
                                        "private-briefing")
                               (= *curiosity-lab-provider-calls* 2))
                          (obj "role" "assistant" "content" :null))
                         ((and (string= *curiosity-lab-scenario*
                                        "curiosity-consolidation")
                               (= *curiosity-lab-provider-calls* 1))
                          (%curiosity-lab-message-with-consolidation))
                         ((and (string= *curiosity-lab-scenario*
                                        "curiosity-consolidation")
                               (= *curiosity-lab-provider-calls* 2))
                          (%curiosity-lab-message-with-briefing))
                         (t (%curiosity-lab-message-without-tool)))))
                 (obj "choices" (vector (obj "message" message))
                      "usage" (obj "prompt_tokens" 0 "completion_tokens" 0
                                   "total_tokens" 0 "cost" 0))))))
    (push (obj "call" *curiosity-lab-provider-calls* "response" response)
          *curiosity-lab-model-responses*)
    response))

(defun %curiosity-lab-seed (count prefix)
  (dotimes (index count)
    (let ((source-id
            (%curiosity-lab-log-event
             "user-message"
             (obj "text" (format nil "~a evidence ~2,'0d" prefix index)
                  "channel" "lab"))))
      (%recursive-record-curiosity
       (format nil "What follows from ~a curiosity ~2,'0d?" prefix index)
       source-id :supporting-event-ids (list source-id)
       :evidence-identity-event-ids (list source-id)
       :source-revision "curiosity-lab-v1"))))

(defun %curiosity-lab-install-ports ()
  (setf (symbol-function 'log-event) #'%curiosity-lab-log-event
        (symbol-function 'replay-events)
        (lambda (&rest ignored)
          (declare (ignore ignored)) *curiosity-lab-events*)
        (symbol-function '%recursive-thread-events)
        (lambda () *curiosity-lab-events*)
        (symbol-function '%conversation-http-model-call)
        #'%curiosity-lab-model-call
        (symbol-function '%conversation-call-model-with-trace)
        (lambda (messages metadata thunk)
          (declare (ignore messages metadata)) (funcall thunk))
        (symbol-function '%conversation-persona-profile)
        (lambda ()
          (obj "persona_id" "curiosity-lab" "revision" 1
               "fingerprint" "curiosity-lab-v1"
               "identity" "Disposable continuing-mind fixture."
               "voice" "Direct."))))

(defun %curiosity-lab-run-starvation ()
  (%curiosity-lab-seed 34 "starvation")
  (let ((input (copy-list *curiosity-lab-events*)))
    (%curiosity-lab-json "input-events.json" (coerce input 'vector)))
  (multiple-value-bind (register total)
      (%recursive-curiosity-open-register
       *curiosity-lab-events* most-positive-fixnum 0)
    (let ((target (aref register 20)))
      (setf *curiosity-lab-target-motive-id* (gethash "motive_id" target)
            *curiosity-lab-target-question* (gethash "question" target)
            *curiosity-lab-target-evidence*
            (gethash "observation_event_ids" target))
      (let ((first (conscious-recursive-curiosity-attention-one))
            (second (conscious-recursive-curiosity-attention-one)))
        (%curiosity-lab-check "fixture has 34 open motives" (= total 34))
        (%curiosity-lab-check
         "first page declines"
         (and (string= "attention-declined" (gethash "status" first))
              (= 0 (gethash "page_offset" first))))
        (%curiosity-lab-check
         "second page receives a model boundary"
         (and (string= "focus-chosen" (gethash "status" second))
              (= 20 (gethash "page_offset" second))
              (= 2 *curiosity-lab-provider-calls*)))
        (%curiosity-lab-check
         "chosen focus is the supplied second-page motive"
         (let* ((focus-id (gethash "focus_event_id" second))
                (focus (find focus-id *curiosity-lab-events*
                             :key (lambda (event) (gethash "id" event))
                             :test #'equal)))
           (and focus
                (find *curiosity-lab-target-motive-id*
                      (gethash "source_motive_ids"
                               (%recursive-event-payload focus))
                      :test #'string=))))))))

(defun %curiosity-lab-run-quiescence ()
  (%curiosity-lab-seed 34 "quiescence")
  (let ((input (copy-list *curiosity-lab-events*)))
    (%curiosity-lab-json "input-events.json" (coerce input 'vector)))
  (let* ((first (conscious-recursive-curiosity-attention-one))
         (second (conscious-recursive-curiosity-attention-one))
         (third (conscious-recursive-curiosity-attention-one))
         (event-count (length *curiosity-lab-events*))
         (fourth (conscious-recursive-curiosity-attention-one)))
    (%curiosity-lab-check
     "both bounded pages decline"
     (and (string= "attention-declined" (gethash "status" first))
          (string= "attention-declined" (gethash "status" second))
          (= 0 (gethash "page_offset" first))
          (= 20 (gethash "page_offset" second))))
    (%curiosity-lab-check
     "all pages append one quiescent receipt"
     (and (string= "attention-quiescent" (gethash "status" third))
          (= 1 (count "recursive-curiosity-attention-quiescent"
                      *curiosity-lab-events*
                      :key (lambda (event) (gethash "type" event ""))
                      :test #'string=))))
    (%curiosity-lab-check
     "settled replay is silent and read-only"
     (and (string= "quiescent" (gethash "status" fourth))
          (= 2 *curiosity-lab-provider-calls*)
          (= event-count (length *curiosity-lab-events*))))))

(defun %curiosity-lab-run-briefing ()
  (%curiosity-lab-seed 3 "briefing")
  ;; A model may omit the caveat while compressing. The real final consumer
  ;; must still carry it, not merely the fixture's scripted model response.
  (let* ((motive-id (gethash "motive_id"
                           (aref (nth-value 0 (%recursive-curiosity-open-register
                                               *curiosity-lab-events* 20 0)) 0)))
         (result-id
          (log-event "recursive-curiosity-result"
                     (obj "source_motive_ids" (vector motive-id)
                          "content" "A possible relationship, not causal proof."))))
    (log-event "recursive-curiosity-result-review-completed"
               (obj "result_event_id" result-id "disposition" "sustained"
                    "source_motive_ids" (vector motive-id)))
    (log-event "recursive-curiosity-incorporation-completed"
               (obj "result_event_id" result-id "disposition" "retained"
                    "summary" "A relationship may exist."
                    "limitations" "No causal experiment was performed.")))
  (let* ((input (copy-list *curiosity-lab-events*))
         (raw (%recursive-private-cognition-raw-context-records
               *curiosity-lab-events*)))
    (%curiosity-lab-json "input-events.json" (coerce input 'vector))
    (setf *curiosity-lab-briefing-source-ids*
          (map 'vector (lambda (row) (gethash "source_id" row)) raw))
    (let* ((first (conscious-recursive-curiosity-briefing-one))
           (calls-after-first *curiosity-lab-provider-calls*)
           (events-after-first (length *curiosity-lab-events*))
           (current (conscious-recursive-curiosity-briefing-one))
           (context (conscious-recursive-private-cognition-context-records))
           (completion
             (find "recursive-curiosity-briefing-completed"
                   *curiosity-lab-events*
                   :key (lambda (event) (gethash "type" event ""))
                   :test #'string= :from-end t)))
      (%curiosity-lab-json "private-briefing.json"
                           (%recursive-event-payload completion))
      (%curiosity-lab-json "conversation-context.json" context)
      (%curiosity-lab-check
       "qualification survives model compression into conversation context"
       (some (lambda (row) (search "No causal experiment" (gethash "content" row "")))
             (coerce context 'list)))
      (%curiosity-lab-check
       "sealed rows produce one durable compact briefing"
       (and (string= "briefing-updated" (gethash "status" first))
            completion
            (= 1 calls-after-first)
            (<= (length (gethash "content"
                                 (%recursive-event-payload completion)))
                1800)))
      (%curiosity-lab-check
       "unchanged briefing revision is provider-silent and write-free"
       (and (string= "briefing-current" (gethash "status" current))
            (= calls-after-first *curiosity-lab-provider-calls*)
            (= events-after-first (length *curiosity-lab-events*))))
      (%curiosity-lab-check
       "ordinary conversation context consumes exact status plus durable briefing"
       (and (= 2 (length context))
            (search "Private cognition status" (gethash "content" (aref context 0)))
            (search "Current private-state briefing"
                    (gethash "content" (aref context 1)))))
      (%curiosity-lab-seed 1 "changed")
      (let ((fallback
              (conscious-recursive-private-cognition-context-records)))
        (%curiosity-lab-check
         "source change invalidates stale briefing and restores raw context"
         (and (> (length fallback) 1)
              (notany
               (lambda (row)
                 (search "Current private-state briefing"
                         (gethash "content" row "")))
               (coerce fallback 'list))))
        (setf *curiosity-lab-briefing-source-ids*
              (map 'vector (lambda (row) (gethash "source_id" row)) fallback)))
      (let* ((changed (conscious-recursive-curiosity-briefing-one))
             (events-after-changed (length *curiosity-lab-events*))
             (settled (conscious-recursive-curiosity-briefing-one)))
        (%curiosity-lab-check
         "second revision reaches one bounded terminal disposition"
         (if (string= *curiosity-lab-provider* "mock")
             (and (string= "failed" (gethash "status" changed))
                  (string= "briefing-failure-settled"
                           (gethash "status" settled)))
             (and (string= "briefing-updated" (gethash "status" changed))
                  (string= "briefing-current" (gethash "status" settled)))))
        (%curiosity-lab-check
         "terminal replay does not retry or append"
         (and (= 2 *curiosity-lab-provider-calls*)
              (= events-after-changed (length *curiosity-lab-events*))))))))

(defun %curiosity-lab-run-consolidation ()
  (%curiosity-lab-seed 3 "consolidation")
  (multiple-value-bind (raw total-open)
      (%recursive-curiosity-open-register
       *curiosity-lab-events* most-positive-fixnum 0)
    (declare (ignore total-open))
    (setf *curiosity-lab-consolidation-register* raw)
    (%curiosity-lab-json "input-events.json"
                         (coerce *curiosity-lab-events* 'vector))
    (let* ((updated (conscious-recursive-curiosity-consolidation-one))
           (calls-after-update *curiosity-lab-provider-calls*)
           (events-after-update (length *curiosity-lab-events*))
           (current (conscious-recursive-curiosity-consolidation-one)))
      (multiple-value-bind (presented total completion revision sealed)
          (%recursive-curiosity-consolidated-register *curiosity-lab-events*)
        (declare (ignore revision sealed))
        (%curiosity-lab-json
         "consolidation.json" (%recursive-event-payload completion))
        (%curiosity-lab-check
         "three durable motives become a valid presentation partition"
         (and (string= "consolidation-updated" (gethash "status" updated))
              (= 3 total) (plusp (length presented)) completion
              (= (length presented) (gethash "thread_count" updated))
              (if (string= *curiosity-lab-provider* "mock")
                  (= 2 (length presented))
                  (<= (length presented) 3))))
        (%curiosity-lab-check
         "semantic partition preserves every source motive"
         (and
          (= 3 (length
                (remove-duplicates
                 (loop for row across presented
                       append
                       (coerce (gethash "source_motive_ids" row) 'list))
                 :test #'string=)))
          (or (not (string= *curiosity-lab-provider* "mock"))
              (= 2 (length
                    (gethash "source_motive_ids" (aref presented 0)))))))
        (%curiosity-lab-check
         "unchanged consolidation is provider-silent and write-free"
         (and (string= "consolidation-current" (gethash "status" current))
              (= 1 calls-after-update)
              (= calls-after-update *curiosity-lab-provider-calls*)
              (= events-after-update (length *curiosity-lab-events*))))
        (let ((records
                (%recursive-private-cognition-raw-context-records
                 *curiosity-lab-events*)))
          (%curiosity-lab-check
           "private context consumes the current thread orientations"
           (if (string= *curiosity-lab-provider* "mock")
               (and (some (lambda (row)
                            (search "foreground" (gethash "content" row "")))
                          records)
                    (some (lambda (row)
                            (search "dormant" (gethash "content" row "")))
                          records))
               (every
                (lambda (thread)
                  (some
                   (lambda (row)
                     (search (gethash "attention_state" thread)
                             (gethash "content" row "")))
                   records))
                (coerce presented 'list))))
          (setf *curiosity-lab-briefing-source-ids*
                (map 'vector (lambda (row) (gethash "source_id" row)) records))
          (let* ((briefing (conscious-recursive-curiosity-briefing-one))
                 (context
                   (conscious-recursive-private-cognition-context-records)))
            (%curiosity-lab-json "conversation-context.json" context)
            (%curiosity-lab-check
             "briefing recursively compacts the consolidated orientation"
             (and (string= "briefing-updated" (gethash "status" briefing))
                  (= 2 *curiosity-lab-provider-calls*)
                  (= 1 (length context))
                  (search "Current private-state briefing"
                          (gethash "content" (aref context 0))))))))
      (%curiosity-lab-seed 1 "changed")
      (multiple-value-bind (fallback new-total completion)
          (%recursive-curiosity-consolidated-register *curiosity-lab-events*)
        (%curiosity-lab-check
         "new source state invalidates the frame and restores raw motives"
         (and (= 4 new-total) (= 4 (length fallback)) (null completion)))))))

(defun %curiosity-lab-finish ()
  (let* ((checks (nreverse *curiosity-lab-checks*))
         (passed (count t checks :key #'cdr))
         (failed (- (length checks) passed))
         (inspection (conscious-recursive-curiosity-inspect 100)))
    (%curiosity-lab-jsonl "model-requests.jsonl"
                          (nreverse *curiosity-lab-model-requests*))
    (%curiosity-lab-jsonl "model-responses.jsonl"
                          (nreverse *curiosity-lab-model-responses*))
    (%curiosity-lab-jsonl "event-trace.jsonl" *curiosity-lab-events*)
    (%curiosity-lab-json "final-projection.json" inspection)
    (%curiosity-lab-write
     "report.md"
     (with-output-to-string (out)
       (format out "# Curiosity lab: ~a~%~%" *curiosity-lab-scenario*)
       (format out "- Provider: ~a~%- Model: ~a~%"
               *curiosity-lab-provider* *curiosity-lab-model*)
       (format out "- Requests: ~d~%- Passed: ~d~%- Failed: ~d~%~%"
               *curiosity-lab-provider-calls* passed failed)
       (when (string= *curiosity-lab-provider* "openrouter")
         (format out "- Charged cost: $~,8f~%- Retry limit: 0~%~%"
                 *conscious-conversation-provider-spent-usd*))
       (dolist (check checks)
         (format out "- [~a] ~a~%" (if (cdr check) "x" " ") (car check)))))
    (format t "CURIOSITY-LAB-DONE scenario=~a passed=~d failed=~d requests=~d~%"
            *curiosity-lab-scenario* passed failed
            *curiosity-lab-provider-calls*)
    (uiop:quit (if (zerop failed) 0 1))))

(uiop:ensure-all-directories-exist (list *curiosity-lab-artifact-dir*))
(%curiosity-lab-install-ports)
(%curiosity-lab-configure-provider)
(setf *conscious-recursive-mind-agent-id* "curiosity-lab"
      *conscious-recursive-mind-endpoint*
      (if (string= *curiosity-lab-provider* "openrouter")
          "https://openrouter.ai/api/v1/chat/completions"
          "http://127.0.0.1:1/v1/chat/completions")
      *conscious-recursive-mind-model* *curiosity-lab-model*
      *conscious-recursive-mind-curiosity-enabled-p* t
      *conscious-recursive-mind-curiosity-briefing-enabled-p* t
      *conscious-recursive-mind-curiosity-consolidation-enabled-p* t
      *conscious-recursive-mind-private-budget-percent* 100
      *conscious-recursive-mind-tools-enabled-p* t
      *conscious-recursive-mind-operator-pending-p* nil)
(cond
  ((string= *curiosity-lab-scenario* "attention-starvation")
   (%curiosity-lab-run-starvation))
  ((string= *curiosity-lab-scenario* "attention-quiescence")
   (%curiosity-lab-run-quiescence))
  ((string= *curiosity-lab-scenario* "private-briefing")
   (%curiosity-lab-run-briefing))
  ((string= *curiosity-lab-scenario* "curiosity-consolidation")
   (%curiosity-lab-run-consolidation))
  (t (error "Unknown curiosity lab scenario ~s" *curiosity-lab-scenario*)))
(%curiosity-lab-finish)
