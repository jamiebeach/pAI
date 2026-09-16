;;;; observability-tracing.lisp -- timing substrate.
;;;;
;;;; Nested spans are accumulated in memory and emitted as ONE timing-trace
;;;; event when the root operation completes.  LOG-EVENT opens and flushes
;;;; events.jsonl per call; emitting every span separately would make the
;;;; observer a material source of the latency it is trying to measure.
;;;;
;;;; Load last among behavior wrappers. Reload-safe installation detects
;;;; whether the current target is our previous wrapper or a newly reloaded
;;;; underlying chain, and only recaptures the base in the latter case.

(in-package :agent)

(export '(with-timing-trace with-timing-span call-with-timing-trace
          call-with-timing-span timing-enqueue-context
          call-with-timing-enqueue-context timing-trace-report
          timing-observability-report))

(defparameter *timing-schema-version* 1)
(defparameter *timing-background-sample-rate* 0.25d0)
(defparameter *timing-recent-cap* 200)
(defparameter *timing-safe-attribute-keys*
  '("channel" "message_count" "purpose" "model" "provider" "service_tier"
    "finish_reason" "tool" "k" "mode"
    "input_chars" "event_type" "urgency" "user_visible" "node_count"
    "evidence_count" "memory_spec_count" "tick_type" "prompt_tokens"
    "completion_tokens" "reasoning_tokens" "total_tokens" "cost_usd")
  "Explicit metadata allowlist. Unknown keys are dropped, not sanitized.")

(defvar *timing-id-counter* 0)
(defvar *timing-id-lock* (bt:make-lock "timing-id"))
(defvar *timing-recent-lock* (bt:make-lock "timing-recent"))
(defvar *timing-recent-traces* nil "Newest first, bounded, non-authoritative.")
(defvar *timing-event-sink* nil
  "Optional test sink called with the complete trace payload instead of LOG-EVENT.")
(defvar *timing-installed-wrappers* (make-hash-table :test #'eq))

(defvar *timing-trace-id* nil)
(defvar *timing-turn-id* nil)
(defvar *timing-generation-id* nil)
(defvar *timing-origin* nil)
(defvar *timing-current-span-id* nil)
(defvar *timing-spans* nil)
(defvar *timing-trace-sampled-p* nil)
(defvar *timing-trace-start-ticks* nil)
(defvar *timing-model-purpose* nil)
(defvar *timing-flushing-p* nil)
(defvar *timing-first-public-output-seen-p* nil)

(defun %timing-now-ticks () (get-internal-real-time))

(defun %timing-ms-between (start end)
  (* 1000.0d0 (/ (- end start) (float internal-time-units-per-second 1.0d0))))

(defun %timing-new-id (prefix)
  (bt:with-lock-held (*timing-id-lock*)
    (format nil "~a-~a-~a" prefix (get-universal-time) (incf *timing-id-counter*))))

(defun %timing-safe-value (value)
  (cond ((or (numberp value) (eq value t) (eq value nil) (eq value :null)) value)
        ((stringp value) (subseq value 0 (min 120 (length value))))
        ((symbolp value) (string-downcase (symbol-name value)))
        (t (format nil "<~a>" (type-of value)))))

(defun %timing-safe-attributes (attributes)
  (let ((safe (obj)))
    (when (hash-table-p attributes)
      (maphash (lambda (key value)
                 (let ((normalized (string-downcase (string key))))
                   (when (member normalized *timing-safe-attribute-keys* :test #'string=)
                     (setf (gethash normalized safe)
                           (%timing-safe-value value)))))
               attributes))
    safe))

(defun %timing-error-class (condition)
  "Return only the condition class. Error messages can contain prompts,
tool arguments, endpoints, or provider details and do not belong in traces."
  (string-downcase (symbol-name (type-of condition))))

(defun %timing-record-span (name span-id parent-id start end status error-text attributes)
  (when *timing-trace-sampled-p*
    (push (obj "span_id" span-id
               "parent_span_id" (or parent-id :null)
               "name" name
               "start_offset_ms" (%timing-ms-between *timing-trace-start-ticks* start)
               "duration_ms" (%timing-ms-between start end)
               "status" status
               "error" (or error-text :null)
               "attributes" (%timing-safe-attributes attributes))
          *timing-spans*)))

(defun call-with-timing-span (name thunk &key attributes)
  "Call THUNK and record a nested span when a sampled trace is active.
All values and errors from THUNK pass through unchanged."
  (if (or (not *timing-trace-id*) (not *timing-trace-sampled-p*) *timing-flushing-p*)
      (funcall thunk)
      (let* ((span-id (%timing-new-id "span"))
             (parent-id *timing-current-span-id*)
             (start (%timing-now-ticks))
             (completed nil)
             (error-text nil))
        (unwind-protect
             (let ((*timing-current-span-id* span-id))
               (handler-bind ((error (lambda (e) (setf error-text (%timing-error-class e)))))
                 (multiple-value-prog1 (funcall thunk)
                   (setf completed t))))
          (%timing-record-span name span-id parent-id start (%timing-now-ticks)
                               (if completed "ok" "error") error-text attributes)))))

(defmacro with-timing-span ((name &key attributes) &body body)
  `(call-with-timing-span ,name (lambda () ,@body) :attributes ,attributes))

(defun %timing-default-sampled-p (origin)
  (if (string= (or origin "") "tick")
      (< (random 1.0d0) *timing-background-sample-rate*)
      t))

(defun %timing-remember-trace (payload)
  (bt:with-lock-held (*timing-recent-lock*)
    (push payload *timing-recent-traces*)
    (when (> (length *timing-recent-traces*) *timing-recent-cap*)
      (setf *timing-recent-traces*
            (subseq *timing-recent-traces* 0 *timing-recent-cap*)))))

(defun %timing-flush-trace (trace-id origin start end status error-text sampled-p)
  (when sampled-p
    (let ((payload
            (obj "event_version" *timing-schema-version*
                 "trace_id" trace-id
                 "turn_id" (or *timing-turn-id* :null)
                 "generation_id" (or *timing-generation-id* :null)
                 "origin" (or origin "unknown")
                 "duration_ms" (%timing-ms-between start end)
                 "status" status
                 "error" (or error-text :null)
                 "spans" (coerce (nreverse *timing-spans*) 'vector))))
      (%timing-remember-trace payload)
      (let ((*timing-flushing-p* t))
        (handler-case
            (cond (*timing-event-sink* (funcall *timing-event-sink* payload))
                  ((fboundp 'log-event) (funcall 'log-event "timing-trace" payload)))
          (error () nil))))))

(defun call-with-timing-trace (thunk &key trace-id turn-id generation-id
                                           (origin "internal")
                                           (root-span "operation.total")
                                           enqueued-at-ticks sampled-p)
  "Run THUNK inside a root trace. Nested calls reuse an existing trace."
  (if *timing-trace-id*
      (funcall thunk)
      (let* ((id (or trace-id (%timing-new-id "trace")))
             (start (%timing-now-ticks))
             (sample (if (null sampled-p) (%timing-default-sampled-p origin) sampled-p))
             (*timing-trace-id* id)
             (*timing-turn-id* turn-id)
             (*timing-generation-id* generation-id)
             (*timing-origin* origin)
             (*timing-current-span-id* nil)
             (*timing-spans* nil)
             (*timing-trace-sampled-p* sample)
             (*timing-trace-start-ticks* start)
             (*timing-first-public-output-seen-p* nil)
             (completed nil)
             (error-text nil))
        (when (and sample enqueued-at-ticks (<= enqueued-at-ticks start))
          (%timing-record-span "turn.queue_wait" (%timing-new-id "span") nil
                               enqueued-at-ticks start "ok" nil
                               (obj "channel" origin)))
        (unwind-protect
             (handler-bind ((error (lambda (e) (setf error-text (%timing-error-class e)))))
               (multiple-value-prog1
                   (call-with-timing-span root-span thunk)
                 (setf completed t)))
          ;; Errors are always observable even when a healthy background trace
          ;; would not have been sampled. Such a trace contains the root only.
          (unless (or sample completed)
            (setf sample t *timing-trace-sampled-p* t)
            (%timing-record-span root-span (%timing-new-id "span") nil start
                                 (%timing-now-ticks) "error" error-text nil))
          (%timing-flush-trace id origin start (%timing-now-ticks)
                               (if completed "ok" "error") error-text sample)))))

(defmacro with-timing-trace ((&key trace-id turn-id generation-id
                                   (origin "internal")
                                   (root-span "operation.total")
                                   enqueued-at-ticks sampled-p)
                             &body body)
  `(call-with-timing-trace
    (lambda () ,@body)
    :trace-id ,trace-id :turn-id ,turn-id :generation-id ,generation-id
    :origin ,origin :root-span ,root-span
    :enqueued-at-ticks ,enqueued-at-ticks :sampled-p ,sampled-p))

(defun timing-enqueue-context (&optional (origin "web"))
  "Capture receipt time before work is handed to another thread."
  (obj "trace_id" (%timing-new-id "trace")
       "turn_id" (%timing-new-id "turn")
       "origin" origin
       "enqueued_at_ticks" (%timing-now-ticks)))

(defun call-with-timing-enqueue-context (context thunk)
  (call-with-timing-trace
   thunk
   :trace-id (and context (gethash "trace_id" context))
   :turn-id (and context (gethash "turn_id" context))
   :origin (or (and context (gethash "origin" context)) "web")
   :root-span "turn.total"
   :enqueued-at-ticks (and context (gethash "enqueued_at_ticks" context))
   :sampled-p t))

(defun timing-trace-report (trace-id)
  (bt:with-lock-held (*timing-recent-lock*)
    (find trace-id *timing-recent-traces*
          :key (lambda (trace) (gethash "trace_id" trace)) :test #'string=)))

(defun timing-observability-report ()
  (bt:with-lock-held (*timing-recent-lock*)
    (obj "schema_version" *timing-schema-version*
         "recent_trace_count" (length *timing-recent-traces*)
         "background_sample_rate" *timing-background-sample-rate*
         "installed_wrappers"
         (let ((names nil))
           (maphash (lambda (target fn) (declare (ignore fn))
                      (push (string-downcase (symbol-name target)) names))
                    *timing-installed-wrappers*)
           (coerce (sort names #'string<) 'vector)))))

;;; --- Reload-safe wrapper installation ---------------------------------

(defun %timing-install-wrapper (target base wrapper)
  (when (and (fboundp target) (fboundp wrapper))
    (let* ((current (fdefinition target))
           (previous-wrapper (gethash target *timing-installed-wrappers*))
           (new-wrapper (fdefinition wrapper)))
      ;; If CURRENT is our previous wrapper, retain the existing base. If an
      ;; underlying chain was reloaded, CURRENT changed and must be recaptured.
      (unless (and previous-wrapper (eq current previous-wrapper))
        (setf (fdefinition base) current))
      (setf (fdefinition target) new-wrapper
            (gethash target *timing-installed-wrappers*) new-wrapper)
      t)))

(defun %timing-auto-turn (prompt)
  (if *timing-trace-id*
      (call-with-timing-span "turn.process"
                             (lambda () (funcall 'pai-base-auto-turn-timing prompt)))
      (call-with-timing-trace
       (lambda () (funcall 'pai-base-auto-turn-timing prompt))
       :turn-id (%timing-new-id "turn") :origin "direct"
       :root-span "turn.total" :sampled-p t)))

(defun %timing-call-model (next messages)
  (call-with-timing-span
   "model.public_pipeline"
   (lambda () (funcall next messages))
   :attributes (obj "message_count" (length messages))))

(defun %timing-raw-call-model (messages)
  (let* ((purpose (or *timing-model-purpose*
                      (if *timing-turn-id* "public" "internal")))
         (span-name (cond ((string= purpose "public") "model.public_request")
                          ((string= purpose "tick") "model.cognitive_request")
                          (t "model.internal_request")))
         (attributes (obj "purpose" purpose
                          "message_count" (length messages)
                          "model" (if (boundp '*model*) *model* "unknown"))))
    (call-with-timing-span
     span-name
     (lambda ()
       (let* ((response
                (if (fboundp 'llm-debug-capture-call)
                    (funcall 'llm-debug-capture-call messages purpose
                             (lambda ()
                               (funcall 'pai-base-raw-call-model-timing
                                        messages)))
                    (funcall 'pai-base-raw-call-model-timing messages)))
              (usage (and (hash-table-p response) (gethash "usage" response))))
         ;; ATTRIBUTES is sanitized only when the span closes, so usage can be
         ;; attached after the response without another event or wrapper.
         (when (hash-table-p usage)
           (dolist (entry '(("prompt_tokens" "prompt_tokens")
                            ("completion_tokens" "completion_tokens")
                            ("total_tokens" "total_tokens")
                            ("cost" "cost_usd")))
             (let ((value (gethash (first entry) usage)))
               (when (numberp value)
                 (setf (gethash (second entry) attributes) value)))))
         (when (hash-table-p response)
           (dolist (key '("provider" "model" "service_tier"))
             (let ((value (gethash key response)))
               (when (or (stringp value) (symbolp value))
                 (setf (gethash key attributes) value))))
           (let* ((choices (gethash "choices" response))
                  (first-choice (cond ((and (vectorp choices) (plusp (length choices)))
                                       (aref choices 0))
                                      ((consp choices) (first choices)))))
             (when (hash-table-p first-choice)
               (let ((finish-reason (gethash "finish_reason" first-choice)))
                 (when (or (stringp finish-reason) (symbolp finish-reason))
                   (setf (gethash "finish_reason" attributes) finish-reason))))))
         (let* ((details (and (hash-table-p usage)
                              (gethash "completion_tokens_details" usage)))
                (reasoning (and (hash-table-p details)
                                (gethash "reasoning_tokens" details))))
           (when (numberp reasoning)
             (setf (gethash "reasoning_tokens" attributes) reasoning)))
         response))
     :attributes attributes)))

(defun %timing-execute (tool-call)
  (let ((name (or (ignore-errors (ref tool-call "function" "name")) "unknown")))
    (call-with-timing-span
     (format nil "tool.~a" name)
     (lambda () (funcall 'pai-base-execute-timing tool-call))
     :attributes (obj "tool" name))))

(defun %timing-memory-recall (&rest args)
  (let ((k (or (getf (rest args) :k) :null)))
    (call-with-timing-span
     "memory.recall"
     (lambda () (apply 'pai-base-memory-recall-timing args))
     :attributes (obj "k" k))))

(defun %timing-memory-search (&rest args)
  (let ((k (or (getf (rest args) :k) 8))
        (mode (or (getf (rest args) :mode) :conversation)))
    (call-with-timing-span
     "memory.search"
     (lambda () (apply 'pai-base-memory-search-timing args))
     :attributes (obj "k" k "mode" mode))))

(defun %timing-memory-record-use (&rest args)
  (let* ((ids (first args))
         (node-count (cond ((null ids) 0)
                           ((or (listp ids) (vectorp ids)) (length ids))
                           (t 1))))
    (call-with-timing-span
     "memory.record_use"
     (lambda () (apply 'pai-base-memory-record-use-timing args))
     :attributes (obj "node_count" node-count
                      "user_visible" (if (getf (rest args) :user-visible-p) t nil)))))

(defun %timing-cognitive-call (&rest args)
  (call-with-timing-span
   "cognitive.total"
   (lambda () (apply 'pai-base-cognitive-call-timing args))
   :attributes (obj "purpose" (or (first args) "unknown")
                    "evidence_count"
                    (let ((evidence (second args)))
                      (cond ((null evidence) 0)
                            ((or (listp evidence) (vectorp evidence)) (length evidence))
                            (t 1))))))

(defun %timing-tick-commit-apply (&rest args)
  (let* ((proposal (first args))
         (memories (and (hash-table-p proposal) (gethash "memory_specs" proposal))))
    (call-with-timing-span
     "tick.commit"
     (lambda () (apply 'pai-base-tick-commit-apply-timing args))
     :attributes (obj "tick_type" (or (and (hash-table-p proposal)
                                            (gethash "tick_type" proposal))
                                       "unknown")
                      "memory_spec_count"
                      (cond ((null memories) 0)
                            ((or (listp memories) (vectorp memories))
                             (length memories))
                            (t 1))))))

(defun %timing-embed-text (text)
  (call-with-timing-span
   "embeddings.query"
   (lambda () (funcall 'pai-base-embed-text-timing text))
   :attributes (obj "input_chars" (if (stringp text) (length text) 0))))

(defun %timing-conversation-persist (history)
  (call-with-timing-span
   "conversation.persist"
   (lambda () (funcall 'pai-base-conv-persist-write-timing history))
   :attributes (obj "message_count" (length history))))

(defun %timing-v2-broadcast (type content &rest keys)
  (multiple-value-prog1
      (call-with-timing-span
       "web.broadcast"
       (lambda () (apply 'pai-base-v2-broadcast-timing type content keys))
       :attributes (obj "event_type" type))
    (when (and *timing-turn-id* *timing-trace-sampled-p*
               (not *timing-first-public-output-seen-p*)
               (member type '("thinking" "tool" "final" "image" "error")
                       :test #'string=))
      (setf *timing-first-public-output-seen-p* t)
      (let ((now (%timing-now-ticks)))
        (%timing-record-span "turn.first_public_output" (%timing-new-id "span")
                             *timing-current-span-id* now now "ok" nil
                             (obj "event_type" type))))))

(defun %timing-tick-once ()
  (if *timing-trace-id*
      (funcall 'pai-base-tick-once-timing)
      (let ((*timing-model-purpose* "tick"))
        (call-with-timing-trace
         (lambda () (funcall 'pai-base-tick-once-timing))
         :generation-id (%timing-new-id "generation")
         :origin "tick" :root-span "tick.total"))))

(defun %timing-initiative (reason &optional (urgency :normal))
  (if *timing-trace-id*
      (call-with-timing-span
       "initiative.total"
       (lambda () (funcall 'pai-base-drives-event-initiate-timing reason urgency))
       :attributes (obj "urgency" urgency))
      (call-with-timing-trace
       (lambda () (funcall 'pai-base-drives-event-initiate-timing reason urgency))
       :generation-id (%timing-new-id "initiative")
       :origin "initiative" :root-span "initiative.total" :sampled-p t)))

(define-init :install observability-timing-wrappers
    "Install timing wrappers on the turn, model, memory and tick seams.
     Must run before :start -- a worker launched ahead of its wrapper runs
     unwrapped code."
  (%timing-install-wrapper 'auto-turn 'pai-base-auto-turn-timing '%timing-auto-turn)
  ;; CALL-MODEL is a seam (P0c item 3); a registered layer, not a
  ;; %TIMING-INSTALL-WRAPPER entry -- register-layer is reload-safe by
  ;; construction, so there is no saved-original symbol to maintain here.
  (register-layer call-model timing :order 200 :function #'%timing-call-model)
  (%timing-install-wrapper 'raw-call-model 'pai-base-raw-call-model-timing '%timing-raw-call-model)
  (when (or (not (fboundp 'tool-dispatch-legacy-wrapper-enabled-p))
            (funcall 'tool-dispatch-legacy-wrapper-enabled-p))
    (%timing-install-wrapper 'execute 'pai-base-execute-timing '%timing-execute))
  (%timing-install-wrapper 'memory-recall 'pai-base-memory-recall-timing '%timing-memory-recall)
  (%timing-install-wrapper 'memory-search 'pai-base-memory-search-timing '%timing-memory-search)
  (%timing-install-wrapper 'memory-record-use 'pai-base-memory-record-use-timing '%timing-memory-record-use)
  (%timing-install-wrapper 'cognitive-call 'pai-base-cognitive-call-timing '%timing-cognitive-call)
  (%timing-install-wrapper 'tick-commit-apply 'pai-base-tick-commit-apply-timing
                           '%timing-tick-commit-apply)
  (%timing-install-wrapper 'embed-text 'pai-base-embed-text-timing '%timing-embed-text)
  (%timing-install-wrapper '%conv-persist-write 'pai-base-conv-persist-write-timing '%timing-conversation-persist)
  (%timing-install-wrapper '%v2-broadcast 'pai-base-v2-broadcast-timing '%timing-v2-broadcast)
  (%timing-install-wrapper 'tick-once 'pai-base-tick-once-timing '%timing-tick-once)
  (%timing-install-wrapper '%drives-event-initiate 'pai-base-drives-event-initiate-timing '%timing-initiative)
  (format t "~&observability-tracing loaded: ~a wrapper(s), public/error traces 100%, healthy ticks ~,0f%.~%"
          (hash-table-count *timing-installed-wrappers*)
          (* 100 *timing-background-sample-rate*))
)
