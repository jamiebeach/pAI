;;;; conversation-context-budget.lisp -- one owner for the public chat prompt.
;;;;
;;;; The legacy loop treated every episode handoff as a permanent system
;;;; message.  Those handoffs accumulated ahead of the live dialogue and were
;;;; therefore invisible to the old 100/120 body-message backstop.  This owner
;;;; recognizes that shape, folds one clean continuity summary into the
;;;; canonical system message, and budgets the remaining dialogue as complete
;;;; protocol records.  SHADOW computes and reports the candidate without
;;;; changing the prompt or making an extra model call.  ENFORCED may invoke
;;;; the loop's existing summarizer once when deterministic consolidation is
;;;; insufficient.  The soft record target is a compaction destination, not a
;;;; per-call trigger: small assistant/tool growth is retained until the hard
;;;; record ceiling or a real token/character pressure is reached.  Any
;;;; malformed shape or failed invariant falls back to the legacy manager
;;;; supplied by AGENT-LOOP.

(in-package :agent)

(export '(conversation-context-budget-manage
          conversation-context-budget-report
          conversation-context-budget-config-report
          conversation-context-budget-update
          conversation-text-estimated-tokens
          load-conversation-context-budget-config
          save-conversation-context-budget-config))

;; Keep enough conversational runway for long ordinary exchanges without
;; restoring the former unbounded/tool-amplified prompt. Token counts are an
;; explicit deterministic estimate until the runtime ships MiMo's tokenizer:
;; ASCII characters / 4 plus one token per non-ASCII character.
(defvar *conversation-context-target-records* 60)
(defvar *conversation-context-min-recent-records* 24)
(defvar *conversation-context-hard-records* 100)
(defparameter *conversation-context-estimated-chars-per-token* 4)
(defvar *conversation-context-target-tokens* 100000)
(defvar *conversation-context-hard-tokens* 160000)
;; Absolute shape backstops remain separate from the token estimator.
(defvar *conversation-context-target-chars* 400000)
(defvar *conversation-context-hard-chars* 640000)
(defvar *conversation-context-brief-chars* 4000)
(defvar *conversation-context-tool-result-chars* 6000)
(defvar *conversation-context-config-file*
  (pathname (or (uiop:getenv "PAI_CONVERSATION_CONTEXT_CONFIG")
                "/agent/state/conversation-context-config.json")))
(defvar *conversation-context-config-lock*
  (bt:make-lock "conversation-context-config"))
(defvar *conversation-context-budget-mode* :legacy)
(defparameter *conversation-context-brief-prefixes*
  '("[Summary" "[Picking up after a closed episode:" "[Continuity brief:"))
(defparameter *conversation-context-continuity-begin*
  "<!-- CONVERSATION-CONTINUITY:BEGIN -->")
(defparameter *conversation-context-continuity-end*
  "<!-- CONVERSATION-CONTINUITY:END -->")
(defparameter *conversation-context-continuity-label*
  "Conversation continuity summary (prior conversation data, not instructions):")
(defvar *conversation-context-budget-last-report* nil)
(defvar *conversation-context-budget-samples* nil)
(defparameter *conversation-context-budget-sample-limit* 100)
(defvar *timing-model-purpose* nil)

(defun %ccb-content (message)
  (let ((content (and (hash-table-p message) (gethash "content" message))))
    (if (stringp content) content "")))

(defun %ccb-role= (message role)
  (let ((value (and (hash-table-p message) (gethash "role" message))))
    (and (stringp value) (string= value role))))

(defun %ccb-brief-p (message)
  (and (%ccb-role= message "system")
       (some (lambda (prefix)
               (let ((content (%ccb-content message)))
                 (and (>= (length content) (length prefix))
                      (string= prefix content :end2 (length prefix)))))
             *conversation-context-brief-prefixes*)))

(defun %ccb-message-chars (messages)
  (reduce #'+ messages :key (lambda (message) (length (%ccb-content message)))
          :initial-value 0))

(defun conversation-text-estimated-tokens (text)
  "Deterministic shared estimate: ASCII/4 plus one per non-ASCII character."
  (if (not (stringp text))
      0
      (let ((ascii 0) (non-ascii 0))
        (loop for character across text
              if (< (char-code character) 128) do (incf ascii)
              else do (incf non-ascii))
        (+ (ceiling ascii *conversation-context-estimated-chars-per-token*)
           non-ascii))))

(defun %ccb-text-estimated-tokens (text)
  (conversation-text-estimated-tokens text))

(defun %ccb-message-estimated-tokens (messages)
  (reduce #'+ messages
          :key (lambda (message)
                 (%ccb-text-estimated-tokens (%ccb-content message)))
          :initial-value 0))

(defun %ccb-tool-result-p (message) (%ccb-role= message "tool"))

(defun %ccb-copy-with-content (message content)
  (let ((copy (make-hash-table :test (hash-table-test message))))
    (maphash (lambda (key value) (setf (gethash key copy) value)) message)
    (setf (gethash "content" copy) content)
    copy))

(defun %ccb-count-substring (needle text)
  (loop with start = 0
        for position = (search needle text :start2 start)
        while position
        do (setf start (+ position (length needle)))
        count 1))

(defun %ccb-strip-marked-range (text begin end)
  (let* ((value (if (stringp text) text ""))
         (bp (search begin value))
         (ep (and bp (search end value :start2 (+ bp (length begin))))))
    (if (and bp ep)
        (string-right-trim
         '(#\Space #\Tab #\Newline #\Return)
         (concatenate 'string (subseq value 0 bp)
                      (subseq value (+ ep (length end)))))
        value)))

(defun %ccb-embedded-continuity (message)
  (let* ((content (%ccb-content message))
         (begin *conversation-context-continuity-begin*)
         (end *conversation-context-continuity-end*)
         (bp (search begin content))
         (ep (and bp (search end content :start2 (+ bp (length begin))))))
    (when (and bp ep)
      (let* ((raw (string-trim '(#\Space #\Tab #\Newline #\Return)
                               (subseq content (+ bp (length begin)) ep)))
             (label *conversation-context-continuity-label*))
        (string-trim
         '(#\Space #\Tab #\Newline #\Return)
         (if (and (>= (length raw) (length label))
                  (string= label raw :end2 (length label)))
             (subseq raw (length label))
             raw))))))

(defun %ccb-canonical-without-continuity (message)
  (%ccb-copy-with-content
   message
   (%ccb-strip-marked-range (%ccb-content message)
                            *conversation-context-continuity-begin*
                            *conversation-context-continuity-end*)))

(defun %ccb-corrupt-brief-text-p (text)
  (or (> (%ccb-count-substring "[Continuity brief:" text) 1)
      (search "Older continuity omitted" text :test #'char-equal)
      (search "[...middle omitted...]" text :test #'char-equal)))

(defun %ccb-strip-legacy-brief-wrapper (text)
  "Return plain summary data, or NIL for a recursively damaged legacy brief."
  (when (and (stringp text) (plusp (length text))
             (not (%ccb-corrupt-brief-text-p text)))
    (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) text))
           (prefix
             (find-if (lambda (candidate)
                        (and (>= (length trimmed) (length candidate))
                             (string= candidate trimmed :end2 (length candidate))))
                      '("[Summary of earlier context:"
                        "[Picking up after a closed episode:"
                        "[Continuity brief:"
                        "[Summary")))
           (start (and prefix (length prefix)))
           (body
             (if start
                 (subseq trimmed start
                         (if (and (> (length trimmed) start)
                                  (char= (char trimmed (1- (length trimmed))) #\]))
                             (1- (length trimmed))
                             (length trimmed)))
                 trimmed)))
      (unless (and prefix (search "[Summary unavailable" trimmed
                                  :test #'char-equal))
        (string-trim '(#\Space #\Tab #\Newline #\Return) body)))))

(defun %ccb-bounded-text (text limit label)
  (if (<= (length text) limit)
      text
      (let* ((notice (format nil "[~a; original characters: ~d]~%" label
                             (length text)))
             (available (max 0 (- limit (length notice) 40)))
             (head (floor (* available 4) 5))
             (tail (- available head)))
        (format nil "~a~a~%[...middle omitted...]~%~a"
                notice (subseq text 0 head)
                (subseq text (- (length text) tail))))))

(defun %ccb-compact-message (message)
  (if (and (%ccb-tool-result-p message)
           (> (length (%ccb-content message))
              *conversation-context-tool-result-chars*))
      (%ccb-copy-with-content
       message
       (%ccb-bounded-text (%ccb-content message)
                          *conversation-context-tool-result-chars*
                          "Tool result compacted for conversational context"))
      message))

(defun %ccb-compact-body (body)
  (mapcar #'%ccb-compact-message body))

(defun %ccb-assistant-tool-call-p (message)
  (and (%ccb-role= message "assistant")
       (let ((calls (gethash "tool_calls" message)))
         (and calls (ignore-errors (plusp (length calls)))))))

(defun %ccb-active-exchange-start (body)
  (or (position "user" body :from-end t
                :key (lambda (message) (gethash "role" message ""))
                :test #'string=)
      (max 0 (1- (length body)))))

(defun %ccb-safe-tail (body wanted)
  "Return a newest-record tail without beginning at a tool result or splitting
an assistant tool-call from its immediately following tool results.  Never
drop any record in the current user exchange merely to hit the soft target."
  (let* ((length (length body))
         (soft-start (max 0 (- length wanted)))
         (active-start (%ccb-active-exchange-start body))
         (start (min soft-start active-start)))
    (loop while (and (> start 0) (< start length)
                     (%ccb-tool-result-p (nth start body)))
          do (decf start))
    (when (and (> start 0) (< start length)
               (%ccb-assistant-tool-call-p (nth (1- start) body)))
      (decf start))
    (subseq body start)))

(defun %ccb-compaction-pressure (messages body)
  "Return the first actual pressure requiring compaction, or NIL.  Record
hysteresis prevents a 61st/62nd small record from paying for semantic
summarization merely to return to the 60-record destination."
  (cond ((> (length body) *conversation-context-hard-records*) "hard-records")
        ((> (%ccb-message-estimated-tokens messages)
            *conversation-context-target-tokens*)
         "target-estimated-tokens")
        ((> (%ccb-message-chars messages) *conversation-context-target-chars*)
         "target-characters")
        (t nil)))

(defun %ccb-split (messages)
  "Return canonical, briefs, body, valid-p.  Unknown leading system records
are rejected: silently guessing which identity instruction is canonical is
more dangerous than retaining the legacy prompt."
  (let ((leading nil) (rest messages))
    (loop while (and rest (%ccb-role= (first rest) "system"))
          do (push (pop rest) leading))
    (setf leading (nreverse leading))
    (let* ((canonical (remove-if #'%ccb-brief-p leading))
           (legacy-briefs (remove-if-not #'%ccb-brief-p leading))
           (canonical-message (first canonical))
           (embedded (and canonical-message
                          (%ccb-embedded-continuity canonical-message)))
           (briefs (append (if (and embedded (plusp (length embedded)))
                               (list (obj "role" "system" "content" embedded))
                               nil)
                           legacy-briefs)))
      (values (and canonical-message
                   (%ccb-canonical-without-continuity canonical-message))
              briefs rest (= 1 (length canonical))))))

(defun %ccb-deterministic-summary (briefs)
  (when briefs
    ;; Keep only complete, plain summary units. Arbitrary head/tail clipping
    ;; created orphaned research fragments and recursive closing brackets.
    (let ((selected nil)
          (used 0))
      (dolist (brief (reverse briefs))
        (let* ((plain (%ccb-strip-legacy-brief-wrapper (%ccb-content brief)))
               (cost (and plain (+ (length plain) (if selected 2 0)))))
          (when (and cost (<= (+ used cost) *conversation-context-brief-chars*))
            (pushnew plain selected :test #'string=)
            (incf used cost))))
      (when selected (format nil "~{~a~^~%~%~}" selected)))))

(defun %ccb-canonical-with-summary (canonical summary)
  (let ((stable (%ccb-canonical-without-continuity canonical)))
    (if (and (stringp summary) (plusp (length summary)))
        (%ccb-copy-with-content
         stable
         (format nil
                 "~a~%~%~a~%~a~%~a~%~a"
                 (%ccb-content stable)
                 *conversation-context-continuity-begin*
                 *conversation-context-continuity-label*
                 summary
                 *conversation-context-continuity-end*))
        stable)))

(defun %ccb-drop-oldest-group (messages)
  (when messages
    (let ((rest (rest messages)))
      (if (%ccb-assistant-tool-call-p (first messages))
          (progn
            (loop while (and rest (%ccb-tool-result-p (first rest)))
                  do (pop rest))
            rest)
          rest))))

(defun %ccb-fit-target (canonical summary body)
  "Drop only records older than the current user exchange toward the soft
estimated-token target, while preserving a protocol-safe recent-dialogue
floor. Fixed system/continuity overhead never erases that floor."
  (let* ((prefix (list (%ccb-canonical-with-summary canonical summary)))
         (start (%ccb-active-exchange-start body))
         (older (subseq body 0 start))
         (active (subseq body start)))
    (loop while (and older
                     (> (%ccb-message-estimated-tokens
                         (append prefix older active))
                        *conversation-context-target-tokens*))
          do (let ((next (%ccb-drop-oldest-group older)))
               (if (>= (+ (length next) (length active))
                       *conversation-context-min-recent-records*)
                   (setf older next)
                   (return))))
    (append prefix older active)))

(defun %ccb-valid-candidate-p (candidate)
  (multiple-value-bind (canonical briefs body shape-p) (%ccb-split candidate)
    (declare (ignore canonical briefs))
    (and shape-p
         (= 1 (count "system" candidate
                     :key (lambda (message) (gethash "role" message ""))
                     :test #'string=))
         (<= (length body) *conversation-context-hard-records*)
         (<= (%ccb-message-chars candidate) *conversation-context-hard-chars*)
         (<= (%ccb-message-estimated-tokens candidate)
             *conversation-context-hard-tokens*)
         (or (null body)
             (not (%ccb-tool-result-p (first body)))))))

(defun %ccb-record-report (report)
  (setf *conversation-context-budget-last-report* report)
  (push report *conversation-context-budget-samples*)
  (when (> (length *conversation-context-budget-samples*)
           *conversation-context-budget-sample-limit*)
    (setf *conversation-context-budget-samples*
          (subseq *conversation-context-budget-samples* 0
                  *conversation-context-budget-sample-limit*)))
  (when (fboundp 'log-event)
    (ignore-errors (funcall 'log-event "conversation-context-budget" report)))
  report)

(defun %ccb-mode ()
  (if (boundp '*conversation-context-budget-mode*)
      *conversation-context-budget-mode*
      :legacy))

(defun %conversation-context-budget-manage
    (messages legacy-manager summarizer)
  "Apply the configured context budget.  LEGACY-MANAGER and SUMMARIZER are
lexical callbacks owned by AGENT-LOOP, avoiding a second conversation loop or
another model-call wrapper."
  (let ((mode (%ccb-mode)))
    (if (eq mode :legacy)
        (funcall legacy-manager messages)
        (multiple-value-bind (canonical briefs body shape-p) (%ccb-split messages)
          (let* ((legacy nil)
                 (legacy-computed-p nil)
                 (pressure (%ccb-compaction-pressure messages body))
                 (selected-body
                   (if pressure
                       (%ccb-safe-tail body
                                       *conversation-context-target-records*)
                       body))
                 (tail (%ccb-compact-body selected-body))
                 (head-count (- (length body) (length tail)))
                 (det-summary (%ccb-deterministic-summary briefs))
                  (det-candidate (and shape-p
                                      (%ccb-fit-target canonical det-summary tail)))
                 (needs-summary (and det-candidate
                                     (or (plusp head-count)
                                         (> (%ccb-message-estimated-tokens
                                             det-candidate)
                                            *conversation-context-target-tokens*))))
                 (candidate det-candidate)
                 (summary-used nil)
                 (fallback-reason nil))
            ;; Shadow must be observational only.  Enforced reuses the tested
            ;; legacy summarizer at most once for all displaced records.
            (when (and (eq mode :enforced) needs-summary shape-p)
              (let* ((older-body (subseq body 0 head-count))
                     (material (append briefs older-body))
                     ;; A context summary is internal housekeeping even when
                     ;; requested while a public turn is in flight.  This
                     ;; binding prevents it from replacing the admin console's
                     ;; latest actual public-inference snapshot.
                     (summary
                       (and material
                            (let ((*timing-model-purpose* "context-summary"))
                              (funcall summarizer material)))))
                (setf summary-used (not (null summary))
                      candidate (%ccb-fit-target
                                 canonical
                                 (or (and summary
                                          (%ccb-strip-legacy-brief-wrapper
                                           (%ccb-content summary)))
                                     det-summary)
                                 tail))))
            (unless shape-p (setf fallback-reason "invalid-leading-system-shape"))
            (when (and candidate (not (%ccb-valid-candidate-p candidate)))
              (setf fallback-reason "candidate-hard-limit-or-protocol-invariant"))
            (let* ((enforce-p (and (eq mode :enforced) candidate
                                   (null fallback-reason)))
                   (candidate-body-records
                     (if candidate
                         (multiple-value-bind
                               (ignored-canonical ignored-briefs retained valid-p)
                             (%ccb-split candidate)
                           (declare (ignore ignored-canonical ignored-briefs))
                           (if valid-p (length retained) :null))
                         :null)))
              ;; Shadow must return the real legacy result.  Enforced computes
              ;; it lazily only on fail-closed fallback, avoiding a second
              ;; summarization/model call on the successful path.
              (when (or (eq mode :shadow) (not enforce-p))
                (setf legacy (funcall legacy-manager messages)
                      legacy-computed-p t))
              (%ccb-record-report
               (obj "schema_version" 1
                    "mode" (string-downcase (symbol-name mode))
                    "input_records" (length messages)
                    "input_chars" (%ccb-message-chars messages)
                    "input_estimated_tokens"
                    (%ccb-message-estimated-tokens messages)
                    "legacy_records" (if legacy-computed-p (length legacy) :null)
                    "legacy_chars" (if legacy-computed-p
                                        (%ccb-message-chars legacy) :null)
                    "candidate_records" (if candidate (length candidate) :null)
                    "candidate_chars" (if candidate (%ccb-message-chars candidate) :null)
                    "candidate_estimated_tokens"
                    (if candidate (%ccb-message-estimated-tokens candidate) :null)
                    "leading_briefs" (length briefs)
                    "body_records" (length body)
                    "retained_body_records" candidate-body-records
                    "target_records" *conversation-context-target-records*
                    "minimum_recent_records"
                    *conversation-context-min-recent-records*
                    "hard_records" *conversation-context-hard-records*
                    "target_estimated_tokens"
                    *conversation-context-target-tokens*
                    "hard_estimated_tokens"
                    *conversation-context-hard-tokens*
                    "token_estimator"
                    "ascii-characters/4-plus-non-ascii-characters"
                    "target_met"
                    (if (and candidate
                             (<= (%ccb-message-estimated-tokens candidate)
                                 *conversation-context-target-tokens*))
                        t nil)
                    "summary_required" (if needs-summary t nil)
                    "summary_used" (if summary-used t nil)
                    "compaction_pressure" (or pressure :null)
                    "record_hysteresis" "soft-target-to-hard-ceiling"
                    "enforced" (if enforce-p t nil)
                    "fallback_reason" (or fallback-reason :null)))
              (cond (enforce-p candidate)
                    ((eq mode :enforced)
                     (error "Enforced context projection refused malformed or over-hard-limit prompt: ~a"
                            (or fallback-reason "candidate-unavailable")))
                    (t legacy))))))))

(defun %ccb-parameter-values ()
  (list *conversation-context-target-records*
        *conversation-context-min-recent-records*
        *conversation-context-hard-records*
        *conversation-context-target-tokens*
        *conversation-context-hard-tokens*
        *conversation-context-target-chars*
        *conversation-context-hard-chars*
        *conversation-context-brief-chars*
        *conversation-context-tool-result-chars*))

(defun conversation-context-budget-manage (messages legacy-manager summarizer)
  "Run one prompt assembly against an atomic snapshot of the live parameters."
  (let ((values (bt:with-lock-held (*conversation-context-config-lock*)
                  (%ccb-parameter-values))))
    (destructuring-bind
        (target-records minimum-recent-records hard-records target-tokens
         hard-tokens target-chars hard-chars brief-chars tool-result-chars)
        values
      (let ((*conversation-context-target-records* target-records)
            (*conversation-context-min-recent-records* minimum-recent-records)
            (*conversation-context-hard-records* hard-records)
            (*conversation-context-target-tokens* target-tokens)
            (*conversation-context-hard-tokens* hard-tokens)
            (*conversation-context-target-chars* target-chars)
            (*conversation-context-hard-chars* hard-chars)
            (*conversation-context-brief-chars* brief-chars)
            (*conversation-context-tool-result-chars* tool-result-chars))
        (%conversation-context-budget-manage messages legacy-manager summarizer)))))

(defun conversation-context-budget-config-report ()
  (bt:with-lock-held (*conversation-context-config-lock*)
    (obj "schema_version" 1
         "target_records" *conversation-context-target-records*
         "minimum_recent_records" *conversation-context-min-recent-records*
         "hard_records" *conversation-context-hard-records*
         "target_estimated_tokens" *conversation-context-target-tokens*
         "hard_estimated_tokens" *conversation-context-hard-tokens*
         "target_chars" *conversation-context-target-chars*
         "hard_chars" *conversation-context-hard-chars*
         "brief_chars" *conversation-context-brief-chars*
         "tool_result_chars" *conversation-context-tool-result-chars*
         "token_estimator" "ascii-characters/4-plus-non-ascii-characters")))

(defun %ccb-config-integer (data key minimum maximum)
  (let ((value (and (hash-table-p data) (gethash key data))))
    (unless (and (integerp value) (<= minimum value maximum))
      (error "~a must be an integer from ~d through ~d." key minimum maximum))
    value))

(defun %ccb-validated-config-values (data)
  (let* ((target-records (%ccb-config-integer data "target_records" 4 200))
         (minimum-recent (%ccb-config-integer data "minimum_recent_records" 4 200))
         (hard-records (%ccb-config-integer data "hard_records" 4 300))
         (target-tokens (%ccb-config-integer data "target_estimated_tokens" 4096 1000000))
         (hard-tokens (%ccb-config-integer data "hard_estimated_tokens" 4096 1000000))
         (target-chars (%ccb-config-integer data "target_chars" 16384 4000000))
         (hard-chars (%ccb-config-integer data "hard_chars" 16384 4000000))
         (brief-chars (%ccb-config-integer data "brief_chars" 512 64000))
         (tool-result-chars (%ccb-config-integer data "tool_result_chars" 512 128000)))
    (unless (<= minimum-recent target-records hard-records)
      (error "Record limits must satisfy minimum_recent_records <= target_records <= hard_records."))
    (unless (<= target-tokens hard-tokens)
      (error "Token limits must satisfy target_estimated_tokens <= hard_estimated_tokens."))
    (unless (<= target-chars hard-chars)
      (error "Character limits must satisfy target_chars <= hard_chars."))
    (unless (and (<= target-tokens target-chars)
                 (<= hard-tokens hard-chars))
      (error "Character backstops may not be smaller than their token limits."))
    (unless (and (<= brief-chars target-chars)
                 (<= tool-result-chars target-chars))
      (error "Brief and tool-result limits may not exceed target_chars."))
    (list target-records minimum-recent hard-records target-tokens hard-tokens
          target-chars hard-chars brief-chars tool-result-chars)))

(defun %ccb-set-parameter-values (values)
  (destructuring-bind
      (target-records minimum-recent hard-records target-tokens hard-tokens
       target-chars hard-chars brief-chars tool-result-chars)
      values
    (setf *conversation-context-target-records* target-records
          *conversation-context-min-recent-records* minimum-recent
          *conversation-context-hard-records* hard-records
          *conversation-context-target-tokens* target-tokens
          *conversation-context-hard-tokens* hard-tokens
          *conversation-context-target-chars* target-chars
          *conversation-context-hard-chars* hard-chars
          *conversation-context-brief-chars* brief-chars
          *conversation-context-tool-result-chars* tool-result-chars)))

(defun %ccb-config-object-from-values (values)
  (destructuring-bind
      (target-records minimum-recent hard-records target-tokens hard-tokens
       target-chars hard-chars brief-chars tool-result-chars)
      values
    (obj "schema_version" 1 "target_records" target-records
         "minimum_recent_records" minimum-recent "hard_records" hard-records
         "target_estimated_tokens" target-tokens
         "hard_estimated_tokens" hard-tokens "target_chars" target-chars
         "hard_chars" hard-chars "brief_chars" brief-chars
         "tool_result_chars" tool-result-chars)))

(defun %ccb-write-config-values (values)
  (ensure-directories-exist *conversation-context-config-file*)
  (let ((tmp (make-pathname :name "conversation-context-config-tmp"
                            :type "json"
                            :defaults *conversation-context-config-file*)))
    (with-open-file (out tmp :direction :output :if-exists :supersede
                             :if-does-not-exist :create :external-format :utf-8)
      (write-string (shasht:write-json (%ccb-config-object-from-values values) nil)
                    out)
      (terpri out)
      (finish-output out))
    (uiop:rename-file-overwriting-target tmp *conversation-context-config-file*)))

(defun save-conversation-context-budget-config ()
  (bt:with-lock-held (*conversation-context-config-lock*)
    (%ccb-write-config-values (%ccb-parameter-values)))
  t)

(defun conversation-context-budget-update (data &key (actor "admin-api"))
  "Validate, atomically persist, and then publish one complete parameter set."
  (let ((values (%ccb-validated-config-values data)))
    (bt:with-lock-held (*conversation-context-config-lock*)
      ;; Persist first: a failed write leaves both live values and the previous
      ;; boot configuration untouched.
      (%ccb-write-config-values values)
      (%ccb-set-parameter-values values))
    (when (fboundp 'log-event)
      (ignore-errors
        (funcall 'log-event "conversation-context-config-changed"
                 (obj "actor" actor "config" (%ccb-config-object-from-values values)))))
    (conversation-context-budget-config-report)))

(defun load-conversation-context-budget-config ()
  (when (probe-file *conversation-context-config-file*)
    (handler-case
        (let* ((data (shasht:read-json
                      (uiop:read-file-string *conversation-context-config-file*)))
               (values (%ccb-validated-config-values data)))
          (bt:with-lock-held (*conversation-context-config-lock*)
            (%ccb-set-parameter-values values)))
      (error (condition)
        (format t "~&[conversation-context-config] load failed; retaining bound defaults: ~a~%"
                condition)
        nil)))
  (conversation-context-budget-config-report))

(defun conversation-context-budget-report ()
  (or *conversation-context-budget-last-report*
      (obj "schema_version" 1
           "mode" (string-downcase (symbol-name (%ccb-mode)))
           "status" "no-samples"
           "target_records" *conversation-context-target-records*
           "minimum_recent_records" *conversation-context-min-recent-records*
           "hard_records" *conversation-context-hard-records*
           "target_estimated_tokens" *conversation-context-target-tokens*
           "hard_estimated_tokens" *conversation-context-hard-tokens*
           "token_estimator" "ascii-characters/4-plus-non-ascii-characters"
           "target_chars" *conversation-context-target-chars*
           "hard_chars" *conversation-context-hard-chars*)))

(define-init :configure conversation-context-budget-configure
    "Read configuration for conversation-context-budget."
  (load-conversation-context-budget-config))
