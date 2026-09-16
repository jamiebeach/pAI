;;;; conversation-turn-capture.lisp -- complete, typed public turns.
;;;;
;;;; The old episodic recorder saw only AUTO-TURN's final return string. This
;;;; module accumulates every public assistant segment and direct tool result,
;;;; journals an ordered content-free capture manifest, and persists typed
;;;; lived records on a background worker so capture latency cannot suppress or
;;;; delay the public reply. Deterministic node ids make recovery replay safe.

(in-package :agent)

(export '(turn-capture-report turn-capture-worker-start
          turn-capture-worker-stop turn-capture-reconcile
          turn-capture-register-complete-hook
          turn-capture-handles-current-turn-p
          %turn-capture-register-user-event
          %turn-capture-register-completion
          %turn-capture-register-tool-call-event
          %turn-capture-register-tool-result-event))

(defvar *near-term-intention-public-reply-observer* nil
  "Port: (TEXT TURN-ID) -> ignored, or NIL when no near-term-intention layer
is present. Registered by NEAR-TERM-INTENTIONS.LISP at :INSTALL.")
(defvar *turn-capture-context* nil)
(defvar *turn-capture-origin* nil)
(defvar *public-inbound-ordinary-reply-p* nil)
(defvar *turn-capture-persist-fn* nil)
(defvar *turn-capture-event-fn* nil)
(defvar *turn-capture-queue* nil)
(defvar *turn-capture-queued-ids* (make-hash-table :test #'equal))
(defvar *turn-capture-completed-ids* (make-hash-table :test #'equal))
(defvar *turn-capture-queue-lock* (bt:make-lock "turn-capture-queue"))
(defvar *turn-capture-worker* nil)
(defvar *turn-capture-stop-requested* nil)
(defvar *turn-capture-stats* (make-hash-table :test #'equal))
(defvar *turn-capture-stats-lock* (bt:make-lock "turn-capture-stats"))
(defvar *turn-capture-installed-auto-wrapper* nil)
(defvar *turn-capture-complete-hooks* nil
  "Post-persistence observers. Each receives CONTEXT, EPISODE-ID and NODE-IDS.")

(defun %turn-capture-stat (name &optional (delta 1))
  (bt:with-lock-held (*turn-capture-stats-lock*)
    (incf (gethash name *turn-capture-stats* 0) delta)))

(defun turn-capture-report ()
  (let ((counts (obj)) (queue-depth 0))
    (bt:with-lock-held (*turn-capture-stats-lock*)
      (maphash (lambda (key value) (setf (gethash key counts) value))
               *turn-capture-stats*))
    (bt:with-lock-held (*turn-capture-queue-lock*)
      (setf queue-depth (length *turn-capture-queue*)))
    (obj "schema_version" 1
         "queue_depth" queue-depth
         "worker_alive"
         (not (null (and *turn-capture-worker*
                         (bt:thread-alive-p *turn-capture-worker*))))
         "counts" counts)))

(defun %turn-capture-id ()
  (format nil "turn-~a-~6,'0x" (get-universal-time) (random #x1000000)))

(defun %turn-capture-origin ()
  (let* ((explicit-origin *turn-capture-origin*)
         (origin
           (or explicit-origin
               (and (boundp '*timing-origin*) (symbol-value '*timing-origin*))
               "conversation")))
    ;; OBSERVABILITY-TRACING calls an ordinary inbound AUTO-TURN entry through
    ;; a trace whose operational origin is "direct". That describes how the
    ;; trace began, not the publication authority of the turn. Web and other
    ;; adapters may likewise use a channel label for tracing. Trusted inbound
    ;; adapters therefore bind PUBLIC-INBOUND-ORDINARY-REPLY-P around the
    ;; complete AUTO-TURN; explicit capture origins still win so a background
    ;; turn cannot be converted into a user reply by an ambient binding.
    (cond (explicit-origin origin)
          (*public-inbound-ordinary-reply-p* "conversation")
          ((and (stringp origin) (string= origin "direct")) "conversation")
          (t origin))))

(defun %turn-capture-text (value)
  (cond
    ((stringp value) value)
    ((vectorp value)
     (or (loop for part across value
               when (and (hash-table-p part)
                         (string= (gethash "type" part "") "text")
                         (stringp (gethash "text" part)))
                 return (gethash "text" part))
         ""))
    (t (format nil "~a" value))))

(defun %turn-capture-nonempty-text (value)
  (let ((text (%turn-capture-text value)))
    (and (stringp text)
         (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) text)))
         text)))

(defun %turn-capture-log (type payload &key caused-by)
  (cond
    (*turn-capture-event-fn*
     (funcall *turn-capture-event-fn* type payload caused-by))
    ((fboundp 'log-event)
     (funcall 'log-event type payload :caused-by caused-by))
    (t nil)))

(defun turn-capture-register-complete-hook (name function)
  "Idempotently install a local observer of durable completed captures."
  (unless (and (symbolp name) (functionp function))
    (error "A capture hook requires a symbolic name and function."))
  (setf *turn-capture-complete-hooks*
        (acons name function (remove name *turn-capture-complete-hooks*
                                     :key #'car :test #'eq)))
  name)

(defun %turn-capture-notify-complete (context episode-id node-ids complete-event)
  (dolist (hook (copy-list *turn-capture-complete-hooks*))
    (handler-case
        (funcall (cdr hook) context episode-id (copy-list node-ids))
      (error (condition)
        (%turn-capture-stat "completion-hook-error")
        (%turn-capture-log
         "turn-capture-completion-hook-error"
         (obj "turn_id" (gethash "turn_id" context)
              "hook" (string-downcase (symbol-name (car hook)))
              "error_type" (string-downcase (symbol-name (type-of condition))))
         :caused-by complete-event)))))

(defun %turn-capture-new-context (prompt)
  ;; owns the public-turn identity when a timing trace is active.
  ;; Reusing it here keeps the durable episode, model attempts, and browser
  ;; ring exactly joinable without changing capture behavior.
  (obj "turn_id" (or (and (boundp '*timing-turn-id*)
                               (stringp (symbol-value '*timing-turn-id*))
                               (symbol-value '*timing-turn-id*))
                          (%turn-capture-id))
       "as_of" (get-universal-time)
       "origin" (%turn-capture-origin)
       "user_text" (or (%turn-capture-nonempty-text prompt) "")
       "user_event_id" :null
       "next_sequence" 0
       "entries" nil
       "pending_tools" (make-hash-table :test #'equal)
       "tool_results_seen" (make-hash-table :test #'equal)
       "journaled" nil))

(defun turn-capture-handles-current-turn-p ()
  (hash-table-p *turn-capture-context*))

(defun %turn-capture-next-sequence (context)
  (prog1 (gethash "next_sequence" context 0)
    (incf (gethash "next_sequence" context))))

(defun %turn-capture-add-entry (context role content event-id
                                &key tool-call-id tool-name final-p)
  (let ((entry (obj "sequence" (%turn-capture-next-sequence context)
                    "role" role "content" content "event_id" event-id)))
    (when tool-call-id (setf (gethash "tool_call_id" entry) tool-call-id))
    (when tool-name (setf (gethash "tool_name" entry) tool-name))
    (when final-p (setf (gethash "final" entry) t))
    (push entry (gethash "entries" context))
    entry))

(defun %turn-capture-register-user-event (event-id prompt)
  "Called by EVENT-LOG immediately after the authoritative user event exists."
  (when *turn-capture-context*
    (setf (gethash "user_event_id" *turn-capture-context*) event-id)
    (let ((text (or (%turn-capture-nonempty-text prompt)
                    (gethash "user_text" *turn-capture-context* ""))))
      (setf (gethash "user_text" *turn-capture-context*) text)
      (unless (find "user" (gethash "entries" *turn-capture-context*)
                    :key (lambda (entry) (gethash "role" entry))
                    :test #'string=)
        (%turn-capture-add-entry *turn-capture-context* "user" text event-id)))
    t))

(defun %turn-capture-pending-tool (context tool-call)
  (let* ((id (and (hash-table-p tool-call) (gethash "id" tool-call)))
         (result-id
           (if (fboundp '%public-tool-result-id-for-call)
               (funcall '%public-tool-result-id-for-call tool-call)
               (and id (format nil "tool-result:~a" id))))
         (name (ignore-errors (ref tool-call "function" "name")))
         (arguments (ignore-errors (ref tool-call "function" "arguments"))))
    (when id
      (setf (gethash id (gethash "pending_tools" context))
            (obj "tool_call_id" id "tool_result_id" (or result-id :null)
                 "tool_name" (or name "unknown")
                 "arguments" (or arguments :null)
                 "call_event_id" :null "result_event_id" :null)))
    id))

(defun %turn-capture-register-tool-call-event (event-id tool-call)
  "Attach EVENT-LOG's existing generic-tool event to the pending call."
  (when *turn-capture-context*
    (let* ((id (or (and (hash-table-p tool-call) (gethash "id" tool-call))
                   (%turn-capture-pending-tool *turn-capture-context* tool-call)))
           (pending (and id (gethash id (gethash "pending_tools"
                                                  *turn-capture-context*)))))
      (when pending (setf (gethash "call_event_id" pending) event-id))
      (not (null pending)))))

(defun %turn-capture-register-tool-result-event (event-id tool-call result)
  "Attach EVENT-LOG's existing result and append the ordered direct result."
  (when *turn-capture-context*
    (let* ((id (and (hash-table-p tool-call) (gethash "id" tool-call)))
           (pending (and id (gethash id (gethash "pending_tools"
                                                  *turn-capture-context*))))
           (content (and (hash-table-p result) (gethash "content" result))))
      (when (and pending (stringp content)
                 (not (gethash id (gethash "tool_results_seen"
                                           *turn-capture-context*))))
        (setf (gethash "result_event_id" pending) event-id
              (gethash id (gethash "tool_results_seen" *turn-capture-context*)) t)
        (%turn-capture-add-entry
         *turn-capture-context* "tool" content event-id
         :tool-call-id id :tool-name (gethash "tool_name" pending))
        t))))

(defun %turn-capture-ingest-direct-tool-results (messages)
  "Capture tool messages whose dispatcher bypassed EVENT-LOG (notably Brave)."
  (when *turn-capture-context*
    (dolist (message messages)
      (when (and (hash-table-p message)
                 (string= (gethash "role" message "") "tool"))
        (let* ((id (gethash "tool_call_id" message))
               (content (gethash "content" message))
               (pending (and id (gethash id (gethash "pending_tools"
                                                      *turn-capture-context*)))))
          (when (and pending (stringp content)
                     (not (gethash id (gethash "tool_results_seen"
                                               *turn-capture-context*))))
            (let* ((user-event (gethash "user_event_id" *turn-capture-context*))
                   (call-event
                     (%turn-capture-log
                      "tool-call"
                      (obj "name" (gethash "tool_name" pending)
                           "arguments" (gethash "arguments" pending)
                           "turn_id" (gethash "turn_id" *turn-capture-context*)
                           "tool_call_id" id
                           "tool_result_id" (gethash "tool_result_id" pending))
                      :caused-by (unless (eq user-event :null) user-event)))
                   (result-event
                     (%turn-capture-log
                      "tool-result"
                      (obj "name" (gethash "tool_name" pending)
                           "content" content
                           "turn_id" (gethash "turn_id" *turn-capture-context*)
                           "tool_call_id" id
                           "tool_result_id" (gethash "tool_result_id" pending))
                      :caused-by call-event)))
              (setf (gethash "call_event_id" pending) call-event
                    (gethash "result_event_id" pending) result-event
                    (gethash id (gethash "tool_results_seen"
                                         *turn-capture-context*)) t)
              (%turn-capture-add-entry
               *turn-capture-context* "tool" content result-event
               :tool-call-id id :tool-name (gethash "tool_name" pending)))))))))

(defun %turn-capture-register-assistant-response (response)
  (when *turn-capture-context*
    (let* ((message (ignore-errors (ref response "choices" 0 "message")))
           (content (and (hash-table-p message) (gethash "content" message)))
           (tool-calls (and (hash-table-p message) (gethash "tool_calls" message)))
           (has-tools (and tool-calls (plusp (length tool-calls))))
           (text (%turn-capture-nonempty-text content)))
      ;; Tool-bearing assistant segments have already crossed the public
      ;; progress seam and belong in the ordered transcript immediately.
      ;; A tool-free model response is only a draft until AUTO-TURN returns:
      ;; the publication boundary may naturalize it or reject the turn.  Log
      ;; that final speech only from REGISTER-COMPLETION so rejected drafts
      ;; cannot become lived public memory and wrapper re-entry cannot create
      ;; two final assistant events.
      (when (and text has-tools)
        (let* ((user-event (gethash "user_event_id" *turn-capture-context*))
               (sequence (gethash "next_sequence" *turn-capture-context* 0))
               (event-id
                 (%turn-capture-log
                  "agent-message"
                  (obj "text" text
                       "turn_id" (gethash "turn_id" *turn-capture-context*)
                       "segment_sequence" sequence
                       "final" nil)
                  :caused-by (unless (eq user-event :null) user-event))))
          (%turn-capture-add-entry *turn-capture-context* "assistant" text event-id)))
      (when has-tools
        (map nil (lambda (tool-call)
                   (%turn-capture-pending-tool *turn-capture-context* tool-call))
             tool-calls)))))

(defun %turn-capture-register-completion (reply user-event-id)
  "EVENT-LOG completion hook. Ensure supervisor/fallback replies are captured."
  (when *turn-capture-context*
    (when (eq (gethash "user_event_id" *turn-capture-context*) :null)
      (%turn-capture-register-user-event user-event-id
                                         (gethash "user_text" *turn-capture-context*)))
    (let* ((text (%turn-capture-nonempty-text reply))
           (latest (first (gethash "entries" *turn-capture-context*))))
      (when (and text
                 (not (and latest
                           (string= (gethash "role" latest "") "assistant")
                           (string= (gethash "content" latest "") text))))
        (let ((event-id
                (%turn-capture-log
                 "agent-message"
                 (obj "text" text
                      "turn_id" (gethash "turn_id" *turn-capture-context*)
                      "segment_sequence"
                      (gethash "next_sequence" *turn-capture-context* 0)
                      "final" t)
                 :caused-by user-event-id)))
          (%turn-capture-add-entry *turn-capture-context* "assistant" text event-id
                                   :final-p t)))
      (when text
        (cond (*near-term-intention-public-reply-observer*
               (ignore-errors
                 (funcall *near-term-intention-public-reply-observer*
                          text (gethash "turn_id" *turn-capture-context*))))
              ((fboundp 'near-term-intention-observe-public-reply)
               (ignore-errors
                 (funcall 'near-term-intention-observe-public-reply
                          text (gethash "turn_id" *turn-capture-context*))))))
      t)))

(defun %turn-capture-ordered-entries (context)
  (sort (copy-list (gethash "entries" context)) #'<
        :key (lambda (entry) (gethash "sequence" entry))))

(defun %turn-capture-journal (context)
  (unless (gethash "journaled" context)
    (let* ((entries (%turn-capture-ordered-entries context))
           (manifest
             (coerce
              (mapcar
               (lambda (entry)
                 (let ((item (obj "sequence" (gethash "sequence" entry)
                                  "role" (gethash "role" entry)
                                  "event_id" (gethash "event_id" entry))))
                   (when (gethash "tool_call_id" entry)
                     (setf (gethash "tool_call_id" item)
                           (gethash "tool_call_id" entry)))
                   (when (gethash "tool_name" entry)
                     (setf (gethash "tool_name" item)
                           (gethash "tool_name" entry)))
                   item))
               entries)
              'vector))
           (user-event (gethash "user_event_id" context))
           (ready-id
             (%turn-capture-log
              "turn-capture-ready"
              (obj "turn_id" (gethash "turn_id" context)
                   "origin" (gethash "origin" context)
                   "entry_count" (length entries)
                   "entries" manifest)
              :caused-by (unless (eq user-event :null) user-event))))
      (setf (gethash "ready_event_id" context) ready-id
            (gethash "journaled" context) t)
      ready-id)))

(defun %turn-capture-node-id (turn-id role sequence)
  (format nil "~a-~a-~4,'0d" turn-id role sequence))

(defun %turn-capture-persistence-content (content)
  "Make event text safe for PostgreSQL without hiding invalid bytes. The
authoritative event remains unchanged; persisted memory uses an explicit
<NUL> marker and records the replacement count in epistemic metadata."
  (let* ((text (or content ""))
         (nul (code-char 0))
         (count (count nul text)))
    (values
     (if (zerop count)
         text
         (with-output-to-string (out)
           (loop for character across text
                 do (if (char= character nul)
                        (write-string "<NUL>" out)
                        (write-char character out)))))
     count)))

(defparameter *turn-capture-tool-memory-max-chars* 4000
  "Maximum tool-result characters copied into semantic memory. The complete
result remains authoritative in the correlated tool-result/turn-ready event.")

(defun %turn-capture-memory-content (role content event-id)
  "Return bounded semantic-memory text plus NUL/truncation metadata. Only tool
results are compacted; lived user and assistant speech remain exact."
  (multiple-value-bind (safe-content nul-replacements)
      (%turn-capture-persistence-content content)
    (if (or (not (string= role "tool"))
            (<= (length safe-content) *turn-capture-tool-memory-max-chars*))
        (values safe-content nul-replacements nil (length safe-content))
        (let* ((original-chars (length safe-content))
               (notice
                 (format nil
                         "[Tool result compacted for semantic memory; full evidence event: ~a; original characters: ~d]~%"
                         event-id original-chars))
               (available (max 0 (- *turn-capture-tool-memory-max-chars*
                                    (length notice) 40)))
               (head (floor (* available 4) 5))
               (tail (- available head)))
          (values
           (format nil "~a~a~%[...middle omitted...]~%~a"
                   notice (subseq safe-content 0 head)
                   (subseq safe-content (- original-chars tail)))
           nul-replacements t original-chars)))))

(defun %turn-capture-persist-default (context)
  (unless (fboundp 'memory-admit-node)
    (error "Complete turn capture requires MEMORY-ADMIT-NODE"))
  (let* ((turn-id (gethash "turn_id" context))
         (origin (gethash "origin" context))
         (entries (%turn-capture-ordered-entries context))
         (node-ids nil)
         (previous nil))
    (dolist (entry entries)
      (let* ((role (gethash "role" entry))
             (sequence (gethash "sequence" entry))
             (event-id (gethash "event_id" entry))
             (node-id (%turn-capture-node-id turn-id role sequence))
             (tool-name (gethash "tool_name" entry))
             (direct
               (cond
                 ((string= role "user")
                  (list "lived-user" "user-report"
                        (format nil "conversation-turn-capture/~a" origin) 0.60d0))
                 ((string= role "assistant")
                  (list "lived-agent-action" "agent-action"
                        (format nil "conversation-turn-capture/~a" origin) 0.50d0))
                 ((string= role "tool")
                  (list "tool-result" "direct-event" (or tool-name "unknown-tool")
                        0.45d0))
                 (t (error "Unsupported capture role ~a" role)))))
        (multiple-value-bind (safe-content nul-replacements truncated-p
                              original-content-chars)
            (%turn-capture-memory-content role (gethash "content" entry)
                                          event-id)
          (let ((metadata
                  (obj "turn_id" turn-id "sequence" sequence "role" role
                       "origin" origin
                       "tool_call_id"
                       (or (gethash "tool_call_id" entry) :null))))
            (when (plusp nul-replacements)
              (setf (gethash "content_nul_replacements" metadata)
                    nul-replacements))
            (when truncated-p
              (setf (gethash "tool_content_compacted" metadata) t
                    (gethash "original_content_characters" metadata)
                    original-content-chars
                    (gethash "authoritative_event_id" metadata) event-id))
            (memory-admit-node
             :id node-id :kind "observation" :content safe-content
             :importance (fourth direct) :arousal 0.30d0
             :source-event-id event-id :origin-class (first direct)
             :epistemic-status (second direct) :producer (third direct)
             :confidence 1.0d0 :grounding-status "grounded"
             :epistemic-metadata metadata)))
        (when previous (memory-add-edge node-id previous "follows"))
        (setf previous node-id)
        (push node-id node-ids)))
    (setf node-ids (nreverse node-ids))
    (when node-ids
      (let ((episode-id (format nil "~a-episode" turn-id)))
        (memory-admit-node
         :id episode-id :kind "episode"
         :content (format nil "Public conversation turn ~a with ~a ordered event(s)."
                          turn-id (length node-ids))
         :importance 0.55d0 :arousal 0.30d0
         :source-event-id (gethash "ready_event_id" context)
         :origin-class "synthetic" :epistemic-status "supported-inference"
         :producer "conversation-turn-capture" :model-purpose "turn-container"
         :confidence 1.0d0 :grounding-status "grounded"
         :lineage-parent-ids node-ids
         :epistemic-metadata (obj "turn_id" turn-id "entry_count" (length node-ids)))
        (dolist (node-id node-ids) (memory-add-edge episode-id node-id "contains"))
        (values episode-id node-ids)))))

(defun %turn-capture-persist (context)
  (funcall (or *turn-capture-persist-fn* #'%turn-capture-persist-default) context))

(defun %turn-capture-enqueue (context)
  (%turn-capture-journal context)
  (let ((turn-id (gethash "turn_id" context)))
    (bt:with-lock-held (*turn-capture-queue-lock*)
      (unless (or (gethash turn-id *turn-capture-queued-ids*)
                  (gethash turn-id *turn-capture-completed-ids*))
        (setf (gethash turn-id *turn-capture-queued-ids*) t)
        (setf *turn-capture-queue*
              (append *turn-capture-queue* (list context)))
        (%turn-capture-stat "queued"))))
  context)

(defun %turn-capture-pop ()
  (bt:with-lock-held (*turn-capture-queue-lock*)
    (when *turn-capture-queue*
      (pop *turn-capture-queue*))))

(defun %turn-capture-worker-step ()
  (let ((context (%turn-capture-pop)))
    (when context
      (let ((turn-id (gethash "turn_id" context)))
        (multiple-value-bind (episode-id node-ids persisted-p)
            (handler-case
                (multiple-value-bind (episode-id node-ids)
                    (if (fboundp 'call-with-timing-trace)
                        (funcall 'call-with-timing-trace
                                 (lambda () (%turn-capture-persist context))
                                 :turn-id turn-id :generation-id turn-id
                                 :origin "turn-capture-worker"
                                 :root-span "turn_capture.persist" :sampled-p t)
                        (%turn-capture-persist context))
                  (values episode-id node-ids t))
              (error (condition)
                (let ((attempt (1+ (gethash "persistence_attempts" context 0))))
                  (setf (gethash "persistence_attempts" context) attempt)
                  (bt:with-lock-held (*turn-capture-queue-lock*)
                    (remhash turn-id *turn-capture-queued-ids*)
                    (when (< attempt 3)
                      (setf (gethash turn-id *turn-capture-queued-ids*) t
                            *turn-capture-queue*
                            (append *turn-capture-queue* (list context)))))
                  (when (< attempt 3) (%turn-capture-stat "persistence-retry")))
                (%turn-capture-stat "persistence-error")
                (%turn-capture-log
                 "turn-capture-persistence-error"
                 (obj "turn_id" turn-id "error_type"
                      (string-downcase (symbol-name (type-of condition))))
                 :caused-by (gethash "ready_event_id" context))
                (values nil nil nil)))
          (when persisted-p
              (bt:with-lock-held (*turn-capture-queue-lock*)
                (remhash turn-id *turn-capture-queued-ids*)
                (setf (gethash turn-id *turn-capture-completed-ids*) t))
              (%turn-capture-stat "persisted")
              (let* ((entries (gethash "entries" context))
                     (complete-event
                       (%turn-capture-log
                        "turn-capture-complete"
                        (obj "turn_id" turn-id
                             "entry_count" (length entries))
                        :caused-by (gethash "ready_event_id" context)))
                     (tool-count
                       (count "tool" entries :test #'string=
                              :key (lambda (entry) (gethash "role" entry "")))))
                (when (plusp tool-count)
                  (%turn-capture-log
                   "tool-turn-committed"
                   (obj "turn_id" turn-id "tool_entry_count" tool-count
                        "entry_count" (length entries))
                   :caused-by complete-event))
                (%turn-capture-notify-complete context episode-id node-ids
                                               complete-event)))))
      t)))

(defun turn-capture-worker-start ()
  (unless (and *turn-capture-worker* (bt:thread-alive-p *turn-capture-worker*))
    (setf *turn-capture-stop-requested* nil
          *turn-capture-worker*
          (bt:make-thread
           (lambda ()
             (loop until *turn-capture-stop-requested*
                   do (unless (%turn-capture-worker-step) (sleep 0.25d0))))
           :name "turn-capture-worker")))
  t)

(defun turn-capture-worker-stop (&optional (timeout 3))
  (setf *turn-capture-stop-requested* t)
  (loop with deadline = (+ (get-internal-real-time)
                           (* timeout internal-time-units-per-second))
        while (and *turn-capture-worker*
                   (bt:thread-alive-p *turn-capture-worker*)
                   (< (get-internal-real-time) deadline))
        do (sleep 0.05d0))
  (not (and *turn-capture-worker* (bt:thread-alive-p *turn-capture-worker*))))

(defun %turn-capture-event-index (events)
  (let ((index (make-hash-table)))
    (dolist (event events index)
      (setf (gethash (gethash "id" event) index) event))))

(defun %turn-capture-context-from-ready (ready event-index)
  (let* ((payload (gethash "payload" ready))
         (context (obj "turn_id" (gethash "turn_id" payload)
                       "as_of" (get-universal-time)
                       "origin" (gethash "origin" payload "recovery")
                       "user_text" "" "user_event_id" (or (gethash "caused_by" ready) :null)
                       "next_sequence" 0 "entries" nil
                       "pending_tools" (make-hash-table :test #'equal)
                       "tool_results_seen" (make-hash-table :test #'equal)
                       "journaled" t "ready_event_id" (gethash "id" ready))))
    (map nil
         (lambda (item)
           (let* ((event (gethash (gethash "event_id" item) event-index))
                  (event-payload (and event (gethash "payload" event)))
                  (role (gethash "role" item))
                  (content
                    (and event-payload
                         (if (string= role "tool")
                             (gethash "content" event-payload)
                             (gethash "text" event-payload)))))
             (when (stringp content)
               (let ((entry (obj "sequence" (gethash "sequence" item)
                                 "role" role "content" content
                                 "event_id" (gethash "event_id" item))))
                 (when (gethash "tool_call_id" item)
                   (setf (gethash "tool_call_id" entry)
                         (gethash "tool_call_id" item)))
                 (when (gethash "tool_name" item)
                   (setf (gethash "tool_name" entry) (gethash "tool_name" item)))
                 (push entry (gethash "entries" context))
                 (setf (gethash "next_sequence" context)
                       (max (gethash "next_sequence" context)
                            (1+ (gethash "sequence" item))))))))
         (gethash "entries" payload))
    context))

(defun turn-capture-reconcile ()
  "Requeue journaled captures with no completion event. Content is rebuilt
from authoritative user/agent/tool events, not from a prose episode summary."
  (if (not (fboundp 'replay-events))
      0
      ;; Recovery is for captures interrupted near the previous shutdown, not
      ;; an archival repair pass. Keeping this to two hours prevents startup
      ;; from materializing days of large model telemetry.
      (let* ((from (- (get-universal-time) (* 2 3600)))
             (events
               (funcall 'replay-events
                        :from from
                        :types '("user-message" "agent-message"
                                 "tool-call" "tool-result"
                                 "turn-capture-ready"
                                 "turn-capture-complete")))
             (index (%turn-capture-event-index events))
             (ready (make-hash-table :test #'equal))
             (complete (make-hash-table :test #'equal)))
        (dolist (event events)
          (let ((type (gethash "type" event)) (payload (gethash "payload" event)))
            (cond
              ((string= type "turn-capture-ready")
               (setf (gethash (gethash "turn_id" payload) ready) event))
              ((string= type "turn-capture-complete")
               (setf (gethash (gethash "turn_id" payload) complete) t)))))
        (let ((count 0))
          (maphash
           (lambda (turn-id event)
             (if (gethash turn-id complete)
                 (setf (gethash turn-id *turn-capture-completed-ids*) t)
                 (let ((context (%turn-capture-context-from-ready event index)))
                   (when (gethash "entries" context)
                     (%turn-capture-enqueue context)
                     (incf count)))))
           ready)
          (%turn-capture-stat "reconciled" count)
          count))))

;;; Capture conversational CALL-MODEL only. COGNITIVE-CALL uses its dedicated
;;; tool-free request primitive and never enters this wrapper.
(register-layer call-model turn-capture :order 300
  :function (lambda (next messages)
    (when *turn-capture-context*
      (ignore-errors (%turn-capture-ingest-direct-tool-results messages)))
    (let ((response (funcall next messages)))
      (when *turn-capture-context*
        (ignore-errors (%turn-capture-register-assistant-response response)))
      response)))

;;; AUTO-TURN owns the lifecycle and queues persistence only after the complete
;;; public turn unwinds. The worker never controls the return value.
(let* ((current (fdefinition 'auto-turn))
       (effective
         (if (and (fboundp '%timing-auto-turn)
                  (eq current (fdefinition '%timing-auto-turn))
                  (fboundp 'pai-base-auto-turn-timing))
             (fdefinition 'pai-base-auto-turn-timing)
             current)))
  (unless (and *turn-capture-installed-auto-wrapper*
               (or (eq current *turn-capture-installed-auto-wrapper*)
                   (eq effective *turn-capture-installed-auto-wrapper*)))
    (setf (fdefinition 'pai-base-auto-turn-turn-capture) effective)))

(defun auto-turn (prompt)
  (let* ((context (%turn-capture-new-context prompt))
         (*turn-capture-context* context))
    (unwind-protect
        (multiple-value-prog1
            (funcall 'pai-base-auto-turn-turn-capture prompt)
          ;; Observe only a successfully completed inbound public turn.  The
          ;; canary independently rejects system/initiative origins, so this
          ;; hook cannot turn a background AUTO-TURN into a fake user reply.
          (when (fboundp 'reciprocity-canary-observe-reply)
            (ignore-errors
              (funcall 'reciprocity-canary-observe-reply
                       prompt :origin (%turn-capture-origin)))))
      (handler-case
          (when (gethash "entries" context) (%turn-capture-enqueue context))
        (error (condition)
          (%turn-capture-stat "journal-error")
          (format t "~&[turn-capture] journal failed; reply unaffected: ~a~%"
                  condition))))))

(setf *turn-capture-installed-auto-wrapper* (fdefinition 'auto-turn))

(define-init :restore conversation-turn-capture-restore
    "Restore durable state for conversation-turn-capture."
  (turn-capture-reconcile))
(define-init :start conversation-turn-capture-start
    "Start background worker for conversation-turn-capture."
  (turn-capture-worker-start))

(define-init :install conversation-turn-capture-ports
    "Register turn-capture as the event log's capture observer.
EVENT-LOG.LISP records the raw event stream and does not know how a typed
public turn is assembled; it previously reached up for these four functions
by bare symbol. Registered at :INSTALL, alongside the other declared soft
edges (see soft-edge-port-registry.lisp) -- with no turn-capture layer the
ports stay NIL and the raw event log is unaffected."
  (setf *turn-capture-user-event-fn* #'%turn-capture-register-user-event
        *turn-capture-completion-fn* #'%turn-capture-register-completion
        *turn-capture-tool-call-fn* #'%turn-capture-register-tool-call-event
        *turn-capture-tool-result-fn* #'%turn-capture-register-tool-result-event)
  t)
