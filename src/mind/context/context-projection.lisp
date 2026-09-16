;;;; context-projection.lisp -- single bounded dynamic context.
;;;;
;;;; LEGACY leaves the existing prompt writers untouched. SHADOW builds and
;;;; audits the candidate but never mutates the conversation. ENFORCED makes
;;;; this module the sole dynamic-state writer; legacy refresh functions call
;;;; CONTEXT-PROJECTION-LEGACY-MUTATION-ENABLED-P before touching the prompt.

(in-package :agent)

(defvar *retrieval-embedding-mode* :legacy)
(defvar *context-curator-mode* :off)

(export '(build-context-projection render-context-projection
          context-projection-report context-projection-strip-legacy
          context-projection-legacy-mutation-enabled-p))

(defparameter *context-projection-current-budget* 1500)
(defparameter *context-projection-memory-budget* 1800)
(defparameter *context-projection-curator-budget* 2600)
(defparameter *context-projection-audit-budget* 1200)
(defparameter *context-projection-open-loop-budget* 900)
(defparameter *context-projection-affect-budget* 400)
(defparameter *context-projection-publication-budget* 450)
(defparameter *context-projection-scheduled-budget* 700)
(defparameter *context-projection-near-term-budget* 700)
(defparameter *context-projection-total-budget* 7200)
(defparameter *context-projection-max-memory-results* 10)
(defparameter *context-projection-memory-candidate-results* 50)
(defparameter *context-projection-max-shared-memory-results* 3)
(defparameter *context-projection-memory-similarity-floor* 0.52d0)
(defparameter *context-projection-memory-best-window* 0.22d0)
(defparameter *context-projection-audit-window-seconds* 86400)

(defparameter *context-projection-dynamic-markers*
  '(("<!-- CONTINUITY:BEGIN -->" "<!-- CONTINUITY:END -->")
    ("<!-- AFFECT:BEGIN -->" "<!-- AFFECT:END -->")
    ("<!-- INTRUSIONS:BEGIN -->" "<!-- INTRUSIONS:END -->")
    ("<!-- WANTS:BEGIN -->" "<!-- WANTS:END -->")
    ("<!-- SOUL:BEGIN -->" "<!-- SOUL:END -->")
    ("<!-- BRINGUP:BEGIN -->" "<!-- BRINGUP:END -->")
    ("<!-- LATENT:BEGIN -->" "<!-- LATENT:END -->")
    ("<!-- SHARED-MEMORY:BEGIN -->" "<!-- SHARED-MEMORY:END -->")
    ("<!-- USER-TIME:BEGIN -->" "<!-- USER-TIME:END -->")
    ("<!-- PAI-STATE:BEGIN -->" "<!-- PAI-STATE:END -->")))

(defparameter *context-projection-unmarked-legacy-markers*
  '("RELATIONSHIP CONTEXT" "=== PAI'S MEMORY ==="))

(defparameter *context-projection-temporal-request-phrases*
  '("what did you do" "what have you done" "what happened while"
    "what happened since" "what were you doing" "anything happen"
    "anything happened" "anything you did"))

(defparameter *context-projection-temporal-markers*
  '("while i was away" "while i was gone" "since we last" "overnight"
    "last night" "earlier today" "since i left" "since my last"))

(defparameter *context-projection-temporal-request-cues*
  '("what " "what?" "did you" "have you" "were you" "was there"
    "anything " "tell me" "can you tell" "could you tell" "how did"))

(defparameter *context-projection-temporal-followup-phrases*
  '("actually occurred" "actually happened" "answered the question"
    "answer the question" "still have not" "still haven't"
    "still does not" "still doesn't" "what i asked"
    "be specific about that" "not for a polished theme" "direct answer"))

(defparameter *context-projection-forensic-phrases*
  '("forensic" "investigate" "check the logs" "inspect the logs"
    "search the logs" "look through the logs" "verify from the logs"
    "check the database" "inspect the database" "debug this"))

;; Dynamically bound across the public model pipeline. The separately loaded
;; temporal response policy consumes this typed projection without querying
;; state again; in legacy mode the binding is inert.
(defvar *temporal-response-policy-context* nil)
(defvar *temporal-response-policy-correction-p* nil)
(defvar *publication-contract-current* nil)
(defvar *call-model-reasoning-override* nil
  "Dynamic public-call reasoning policy. :DISABLED is reserved for explicit
simple social check-ins; all other turn types retain the model default.")
(defvar *context-projection-scheduled-context-ids* nil)
(defvar *near-term-intentions-mode* :off)

(defparameter *context-projection-audit-event-types*
  '("tick-terminal" "tick-end" "initiative-decision"
    "initiative-candidate-scored" "reflection-no-novelty"
    "explore-novelty-deferred" "latent-thought-incubated"
    "latent-thought-merged" "latent-thought-ready" "self-mod-proposed"
    "self-mod-rejected" "self-mod-executed" "turn-capture-complete"
    "turn-capture-persistence-error"))

(defvar *context-projection-stats* (make-hash-table :test #'equal))
(defvar *context-projection-stats-lock*
  (bt:make-lock "context-projection-stats"))
(defvar *embedding-turn-cache* nil)
(defvar *embedding-turn-cache-turn-hits* 0)
(defvar *embedding-turn-cache-turn-misses* 0)

;; Injectable seams keep projection tests deterministic and let a retrieval or
;; audit failure fall back without changing the public turn.
(defvar *context-projection-memory-search-fn*
  ;; Ordinary context is allowed the same bounded lexical rescue as explicit
  ;; recall.  The result is still filtered, ranked and rendered by this
  ;; projection; retrieval remains read-only and cannot wake attention.
  (lambda (query k)
    (memory-search query :k k :mode :conversation
                   :candidate-strategy :hybrid-explicit)))
(defvar *context-projection-turn-neighborhood-fn*
  (lambda (query anchors)
    (when (fboundp 'memory-search-turn-neighborhood)
      (funcall 'memory-search-turn-neighborhood query anchors)))
  "Read-only relational expansion seam. NIL preserves the atomic fallback.")

(defun %context-projection-ring-events (from to)
  "Return two values: bounded oldest-first ring events and whether the ring
provably contains the latest user boundary. A newest-first ring containing any
USER-MESSAGE necessarily contains every newer event; without one, callers must
fall back to the durable ledger."
  (let ((ring (and (boundp '*event-ring*)
                   (listp (symbol-value '*event-ring*))
                   (copy-list (symbol-value '*event-ring*)))))
    (if (and ring
             (find "user-message" ring
                   :key (lambda (event) (gethash "type" event ""))
                   :test #'string=))
        (values
         (remove-if-not
          (lambda (event)
            (if (fboundp '%event-parse-ts)
                (let ((timestamp (funcall '%event-parse-ts event)))
                  (and (or (null from) (>= timestamp from))
                       (or (null to) (<= timestamp to))))
                t))
          (nreverse ring))
         t)
        (values nil nil))))

(defun %context-projection-event-source (from to)
  (multiple-value-bind (events complete-p)
      (%context-projection-ring-events from to)
    (if complete-p events (replay-events :from from :to to))))

;; DEFPARAMETER intentionally restores the canonical source on a hot reload;
;; tests and adapters dynamically bind this seam rather than mutating it.
(defparameter *context-projection-events-fn*
  #'%context-projection-event-source)
(defvar *context-projection-modulator-fn*
  (lambda () (and (fboundp 'modulator-state) (modulator-state))))
(defvar *context-projection-drive-fn*
  (lambda () (and (fboundp 'drive-state) (drive-state))))
(defvar *context-projection-soul-fn*
  (lambda ()
    (if (and (boundp '*soul-entries*) (listp (symbol-value '*soul-entries*)))
        (copy-list (symbol-value '*soul-entries*))
        nil)))
(defvar *context-projection-grounded-fn*
  (lambda (node-id)
    (and (fboundp 'memory-grounded-p)
         (funcall 'memory-grounded-p node-id))))
(defvar *context-projection-active-questions-fn*
  (lambda ()
    (when (fboundp 'self-model-active-open-questions)
      (funcall 'self-model-active-open-questions :limit 1))))
(defvar *context-projection-event-fn*
  (lambda (type payload)
    (when (fboundp 'log-event) (log-event type payload))))

(defun %context-projection-mode ()
  (if (boundp '*context-projection-mode*)
      (symbol-value '*context-projection-mode*)
      :legacy))

(defun context-projection-legacy-mutation-enabled-p ()
  "True unless the unified projection is authoritative. Structured legacy
maintenance may continue, but old injectors must not mutate the prompt."
  (not (eq (%context-projection-mode) :enforced)))

(define-init :install context-projection-mutation-policy-port
    "Answer the memory layer's prompt-mutation question.
CONVERSATION-EPISODIC-MEMORY.LISP maintains a section of the system prompt and
must stand down once the unified projection is authoritative. It used to call
the predicate below directly; registering here keeps the policy owned by the
layer that defines it and leaves memory able to load without it."
  (setf *conversation-prompt-mutation-allowed-fn*
        #'context-projection-legacy-mutation-enabled-p)
  t)

(defun %context-projection-stat (name &optional (amount 1))
  (bt:with-lock-held (*context-projection-stats-lock*)
    (incf (gethash name *context-projection-stats* 0) amount)))

(defun context-projection-report ()
  (bt:with-lock-held (*context-projection-stats-lock*)
    (let ((counts (obj)))
      (maphash (lambda (key value) (setf (gethash key counts) value))
               *context-projection-stats*)
      (obj "schema_version" 1
           "mode" (string-downcase (symbol-name (%context-projection-mode)))
           "total_budget" *context-projection-total-budget*
           "counts" counts))))

(defun %context-projection-text (value)
  (cond ((stringp value) value)
        ((or (null value) (eq value :null)) "")
        (t (format nil "~a" value))))

(defun %context-projection-truncate (text limit)
  (let ((value (%context-projection-text text)))
    (cond ((not (plusp limit)) "")
          ((<= (length value) limit) value)
          ((<= limit 3) (subseq value 0 limit))
          (t (concatenate 'string (subseq value 0 (- limit 3)) "...")))))

(defun %context-projection-temporal-p (prompt)
  (let ((lower (string-downcase (%context-projection-text prompt))))
    (or (some (lambda (phrase) (search phrase lower))
              *context-projection-temporal-request-phrases*)
        (and (some (lambda (marker) (search marker lower))
                   *context-projection-temporal-markers*)
             (or (position #\? lower)
                 (some (lambda (cue) (search cue lower))
                       *context-projection-temporal-request-cues*))))))

(defun %context-projection-temporal-followup-p (prompt)
  (let ((lower (string-downcase (%context-projection-text prompt))))
    (some (lambda (phrase) (search phrase lower))
          *context-projection-temporal-followup-phrases*)))

(defun %context-projection-recent-user-prompts (&optional (limit 3))
  (when (and (boundp '*last-self-mod-history*)
             (listp *last-self-mod-history*))
    (loop for message in (reverse *last-self-mod-history*)
          when (and (hash-table-p message)
                    (string= (gethash "role" message "") "user"))
            collect (gethash "content" message "") into prompts
            and count 1 into seen
          when (>= seen limit) return prompts
          finally (return prompts))))

(defun %context-projection-prior-user-prompts (prompt)
  "Return newest-first prior user prompts, excluding at most one newest copy of
PROMPT. The live history can be observed either before or after the current
user message is appended, so both forms must produce the same result."
  (let ((current (%context-projection-text prompt))
        (prompts nil))
    (when (and (boundp '*last-self-mod-history*)
               (listp *last-self-mod-history*))
      (let* ((history *last-self-mod-history*)
             (latest (car (last history)))
             (history-includes-current-p
               (and (hash-table-p latest)
                    (string= (gethash "role" latest "") "user")
                    (string= (gethash "content" latest "") current))))
        (dolist (message (reverse history))
          (when (and (hash-table-p message)
                     (string= (gethash "role" message "") "user"))
            (push (gethash "content" message "") prompts)))
        (setf prompts (nreverse prompts))
        (if history-includes-current-p (rest prompts) prompts)))))

(defun %context-projection-temporal-correction-depth (prompt)
  "Return the correction depth for a contiguous temporal user thread.

The initial explicit temporal request has depth zero. A corrective follow-up
has depth one, with each immediately preceding corrective user turn increasing
the depth. Any unrelated user turn terminates carryover."
  (when (%context-projection-temporal-followup-p prompt)
    (let ((depth 1))
      (dolist (prior (%context-projection-prior-user-prompts prompt))
        (cond
          ((%context-projection-temporal-followup-p prior)
           (incf depth))
          ((%context-projection-temporal-p prior)
           ;; A forensic turn is authoritative only through observed public
           ;; tool results. Do not silently convert its follow-up into a
           ;; passive bounded-audit answer that could overwrite those facts.
           (return-from %context-projection-temporal-correction-depth
             (unless (%context-projection-forensic-request-p prior) depth)))
          (t
           (return-from %context-projection-temporal-correction-depth nil))))
      nil)))

(defun %context-projection-effective-temporal-p (prompt)
  "Carry temporal intent only through explicit corrective follow-ups.
This keeps a multi-turn answer grounded without making an unrelated next
question inherit the previous audit mode."
  (or (%context-projection-temporal-p prompt)
      (not (null (%context-projection-temporal-correction-depth prompt)))))

(defun %context-projection-forensic-request-p (prompt)
  (let ((lower (string-downcase (%context-projection-text prompt))))
    (some (lambda (phrase) (search phrase lower))
          *context-projection-forensic-phrases*)))

(defun %context-projection-list (value)
  (cond ((null value) nil)
        ((vectorp value) (coerce value 'list))
        ((listp value) value)
        (t (list value))))

(defparameter *context-projection-active-question-followup-phrases*
  '("are you sure" "really" "not in your context" "in your context"
    "system prompt" "you should know" "don't you know" "do you not know"
    "isn't available" "is not available" "unavailable to you"))

(defparameter *context-projection-active-question-explicit-phrases*
  '("open question" "open questions" "explore tick"
    "question are you working on" "questions are you working on"
    "question you're working on" "questions you're working on"))

(defparameter *context-projection-followup-stop-words*
  '("about" "after" "again" "could" "current" "grounded" "question"
    "recorded" "should" "their" "there" "these" "thing" "think"
    "those" "would" "working" "what" "where" "which" "with"))

(defparameter *context-projection-followup-meta-words*
  '("available" "context" "even" "know" "missing" "prompt" "really"
    "should" "sure" "system" "unavailable" "your" "you" "not" "isn" "don"))

(defun %context-projection-words (value)
  (let ((words nil) (buffer (make-string-output-stream)))
    (labels ((finish-word ()
               (let ((word (string-downcase
                            (get-output-stream-string buffer))))
                 (when (plusp (length word)) (push word words)))))
      (loop for character across (%context-projection-text value)
            do (if (alphanumericp character)
                   (write-char character buffer)
                   (finish-word)))
      (finish-word))
    (nreverse words)))

(defun %context-projection-recent-user-content (&optional (limit 4))
  (when (boundp '*last-self-mod-history*)
    (let ((user-messages
            (remove-if-not
             (lambda (message)
               (and (hash-table-p message)
                    (string= "user" (gethash "role" message ""))))
             *last-self-mod-history*)))
      (mapcar (lambda (message) (gethash "content" message ""))
              (last user-messages (min limit (length user-messages)))))))

(defun %context-projection-active-question-followup-p
    (query active-questions)
  "Carry active-question authority through a short verification exchange only
when recent user language overlaps the currently projected typed question."
  (let ((lower (string-downcase (%context-projection-text query))))
    (and active-questions
         (some (lambda (phrase) (search phrase lower))
               *context-projection-active-question-followup-phrases*)
         (let* ((recent (%context-projection-recent-user-content))
                (recent-text (format nil "~{~a~^ ~}" recent))
                (recent-lower (string-downcase recent-text))
                (question-words
                  (remove-duplicates
                   (mapcan
                    (lambda (row)
                      (%context-projection-words (gethash "content" row "")))
                    active-questions)
                   :test #'string=))
                (query-topic-words
                  (remove-if
                   (lambda (word)
                     (or (< (length word) 5)
                         (member word
                                 *context-projection-followup-stop-words*
                                 :test #'string=)
                         (member word
                                 *context-projection-followup-meta-words*
                                 :test #'string=)))
                   (%context-projection-words query)))
                (recent-match-p
                  (or (some (lambda (phrase) (search phrase recent-lower))
                            *context-projection-active-question-explicit-phrases*)
                      (some
                       (lambda (word)
                         (and (>= (length word) 5)
                              (not (member
                                    word
                                    *context-projection-followup-stop-words*
                                    :test #'string=))
                              (search word recent-lower)))
                       question-words)))
                (current-topic-compatible-p
                  (or (null query-topic-words)
                      (some (lambda (word)
                              (member word question-words :test #'string=))
                            query-topic-words))))
           (and recent-match-p current-topic-compatible-p)))))

(defun %context-projection-safe-call (thunk fallback stat)
  (handler-case (funcall thunk)
    (error (condition)
      (%context-projection-stat stat)
      (ignore-errors
        (funcall *context-projection-event-fn* "context-projection-source-error"
                 (obj "source" stat
                      "condition" (string-downcase
                                     (symbol-name (type-of condition))))))
      fallback)))

(defun %context-projection-with-timing (mode thunk)
  (if (fboundp 'call-with-timing-span)
      (funcall 'call-with-timing-span
               "context.projection" thunk
               :attributes (obj "mode" (string-downcase (symbol-name mode))))
      (funcall thunk)))

(defun %context-projection-publication-contract
    (prompt projection correction-depth public-tools-available-p)
  (when (fboundp 'build-publication-contract)
    (let ((thunk
            (lambda ()
              (funcall 'build-publication-contract prompt
                       :context projection :correction-depth correction-depth
                       :public-tools-available-p public-tools-available-p))))
      (if (fboundp 'call-with-timing-span)
          (funcall 'call-with-timing-span "publication.contract" thunk
                   :attributes
                   (obj "temporal" (if (gethash "temporal_query" projection)
                                        t nil)))
          (funcall thunk)))))

(defun %context-projection-current-turn-id ()
  (if (and (boundp '*timing-turn-id*)
           (symbol-value '*timing-turn-id*))
      (symbol-value '*timing-turn-id*)
      :null))

(defun %context-projection-direct-memory-p (row)
  (member (gethash "origin_class" row "")
          '("lived-user" "lived-agent-action" "tool-result" "external-source")
          :test #'string=))

(defun %context-projection-sensitive-memory-content-p (row)
  "Exclude credentials and prompt payloads even when an upstream adapter
misclassifies their provenance as direct evidence."
  (let ((text (string-downcase
               (%context-projection-text (gethash "content" row "")))))
    (or (search "public-system-prompt:begin" text)
        (search "# operational constitution" text)
        (and (search "# pai" text) (search "system prompt" text))
        (search "openrouter_api_key" text)
        (search "telegram_bot_token" text)
        (search "brave_api_key" text))))

(defun %context-projection-raw-turn-memory-p (row)
  "Raw complete-turn evidence stays durable but never becomes system prose."
  (let ((id (gethash "id" row ""))
        (producer (gethash "producer" row "")))
    (or (and (stringp id) (>= (length id) 5)
             (string= "turn-" id :end2 5))
        (and (stringp producer)
             (search "conversation-turn-capture/" producer
                     :test #'char-equal)))))

(defun %context-projection-open-memory-p (row)
  (or (member (gethash "kind" row "")
              '("prediction" "thought" "worldview" "reflection")
              :test #'string=)
      (member (gethash "epistemic_status" row "")
              '("hypothesis" "reflection" "prediction") :test #'string=)))

(defun %context-projection-eligible-memory-p (row)
  "Defense in depth: MEMORY-SEARCH owns the safe-mode policy, but projection
must not render an unsafe row even if a future adapter or test double errs."
  (and (hash-table-p row)
       (member (gethash "grounding_status" row "")
               '("grounded" "partially-grounded") :test #'string=)
       (not (string= (gethash "origin_class" row "")
                     "legacy-unclassified"))
       (not (member (gethash "epistemic_status" row "")
                    '("legacy-unclassified" "rejected") :test #'string=))
       (not (gethash "quarantined" row))))

(defun %context-projection-shareable-evidence-p (row)
  "Accept direct evidence plus only the pure, grounded turn-bundle carrier.
This deliberately does not make arbitrary derived-lived rows shareable."
  (or (%context-projection-direct-memory-p row)
      (and (string= (gethash "kind" row "") "turn-bundle")
           (string= (gethash "epistemic_status" row "")
                    "grounded-turn-bundle")
           (%context-projection-eligible-memory-p row))))

(defun %context-projection-memory-label (row)
  (let ((origin (gethash "origin_class" row ""))
        (status (gethash "epistemic_status" row "")))
    (cond ((string= origin "lived-user") "the operator reported")
          ((string= origin "lived-agent-action") "the agent said or did")
          ((string= origin "tool-result") "A tool returned")
          ((string= origin "external-source") "An external source returned")
          ((string= status "prediction") "Prediction, not observed")
          ((string= status "hypothesis") "Hypothesis, not observed")
          (t "Grounded derived memory"))))

(defun %context-projection-copy-memory (row)
  (let ((copy
          (obj "id" (gethash "id" row)
               "kind" (gethash "kind" row)
               "content" (gethash "content" row)
               "origin_class" (gethash "origin_class" row)
               "epistemic_status" (gethash "epistemic_status" row)
               "grounding_status" (gethash "grounding_status" row)
               "label" (%context-projection-memory-label row))))
    ;; Preserve typed discovery and authority metadata for the later global
    ;; recall selector.  Rendering remains a separate bounded operation; these
    ;; fields are never copied into the model-visible record by themselves.
    (dolist (field '("turn_id" "anchor_id" "member_count"
                     "member_roles" "evidence_node_ids"
                     "similarity" "lexical_tier" "lexical_coverage"
                     "observed_at" "valid_from" "valid_to" "supersedes"
                     "polarity" "scope" "source_event_id"
                     "source_agent_id"))
      (multiple-value-bind (value present-p) (gethash field row)
        (when present-p (setf (gethash field copy) value))))
    copy))

(defun %context-projection-public-memory-score (row)
  "Similarity leads public-context selection. Provenance is only a small tie
influence; recent activation/importance from the database rank cannot swamp a
substantially more relevant grounded fact."
  (+ (* 2.0d0 (or (gethash "lexical_tier" row) 0))
     (or (gethash "lexical_coverage" row) 0.0d0)
     (or (gethash "similarity" row) 0.0d0)
     (cond ((string= (gethash "origin_class" row "") "lived-user") 0.03d0)
           ((member (gethash "origin_class" row "")
                    '("tool-result" "external-source") :test #'string=)
            0.02d0)
           (t 0.0d0))))

(defun %context-projection-select-shared-memory
    (rows &optional (maximum *context-projection-max-shared-memory-results*))
  "Select a bounded semantic evidence set after safety/echo filtering.
Raw turn IDs remain provenance; they are no longer discarded after TOP-K."
  (let* ((direct (remove-if-not #'%context-projection-shareable-evidence-p rows))
         (ranked (stable-sort (copy-list direct) #'>
                              :key #'%context-projection-public-memory-score))
         (best (and ranked (or (gethash "similarity" (first ranked)) 0.0d0)))
         (selected
           (remove-if-not
            (lambda (row)
              (let ((similarity (or (gethash "similarity" row) 0.0d0))
                    (lexical-tier (or (gethash "lexical_tier" row) 0)))
                ;; A grounded lexical hit is independent evidence, not a
                ;; reason to pretend its embedding crossed the semantic
                ;; floor.  This makes directly relevant personal facts enter
                ;; ordinary context without requiring a conscious tool call.
                (or (plusp lexical-tier)
                    (and (>= similarity
                             *context-projection-memory-similarity-floor*)
                         (or (null best)
                             (>= similarity
                                 (- best
                                    *context-projection-memory-best-window*)))))))
            ranked)))
    (subseq selected 0 (min maximum (length selected)))))

(defun %context-projection-turn-bundles (query anchors)
  "Materialize safe same-turn evidence around already-ranked semantic anchors.
Returns bundles, suppressed count, considered count, safe-neighbor count and a
status keyword. Adapter absence/failure preserves the exact atomic fallback."
  (if (not (and anchors
                (fboundp 'turn-bundle-build-candidates)
                *context-projection-turn-neighborhood-fn*))
      (values nil 0 0 0 :atomic-fallback)
      (let* ((failure (gensym "TURN-NEIGHBORHOOD-FAILURE"))
             (rows
               (%context-projection-safe-call
                (lambda ()
                  (funcall *context-projection-turn-neighborhood-fn*
                           query anchors))
                failure "turn-neighborhood-errors")))
        (if (eq rows failure)
            (values nil 0 0 0 :atomic-fallback)
            (let* ((safe-neighbors
                     (remove-if
                      (lambda (row)
                        (or (not (%context-projection-eligible-memory-p row))
                            (%context-projection-sensitive-memory-content-p row)))
                      rows))
                   (corpus
                     (remove-duplicates (append safe-neighbors anchors)
                                        :test #'string=
                                        :key (lambda (row)
                                               (gethash "id" row "")))))
              (if (null safe-neighbors)
                  (values nil 0 0 0 :atomic-fallback)
                  (let* ((ranked
                           (stable-sort
                            (mapcar
                             (lambda (row)
                               (list row
                                     (gethash "similarity" row 0.0d0)
                                     (%context-projection-public-memory-score row)
                                     (gethash "retrieval_embedding" row)))
                             anchors)
                            #'> :key #'third))
                         (result
                           (%context-projection-safe-call
                            (lambda ()
                              (multiple-value-list
                               (turn-bundle-build-candidates
                                ranked corpus :cluster-cap 1)))
                            failure "turn-bundle-errors")))
                    (if (or (eq result failure) (null (first result)))
                        (values nil 0 0 (length safe-neighbors)
                                :atomic-fallback)
                        (values (first result) (second result) (third result)
                                (length safe-neighbors) :bundled)))))))))

(defun %context-projection-safe-soul-rows ()
  (let ((entries (%context-projection-safe-call
                  *context-projection-soul-fn* nil "soul-errors"))
        (rows nil))
    (dolist (entry entries (nreverse rows))
      (let ((evidence (%context-projection-list
                       (gethash "evidence-node-ids" entry))))
        (when (and evidence
                   (every (lambda (id)
                            (ignore-errors
                              (funcall *context-projection-grounded-fn* id)))
                          evidence))
          (push (obj "id" (format nil "soul:~a" (gethash "id" entry))
                     "kind" "soul"
                     "content" (gethash "statement" entry)
                     "origin_class" "derived-lived"
                     "epistemic_status" "grounded-disposition"
                     "grounding_status" "grounded"
                     "label" "Grounded identity disposition"
                     "evidence_node_ids" (coerce evidence 'vector))
                rows))))))

(defun %context-projection-active-question-rows ()
  "Project at most one explicit active question, but only when its cited
evidence exists and is grounded. This replaces the legacy BRINGUP path while
typed context projection is enforced."
  (let ((entries (%context-projection-list
                  (%context-projection-safe-call
                   *context-projection-active-questions-fn* nil
                   "active-question-errors")))
        (rows nil))
    (dolist (entry (subseq entries 0 (min 1 (length entries))) (nreverse rows))
      (let* ((statement (gethash "statement" entry))
             (evidence (%context-projection-list
                        (or (gethash "root-evidence-node-ids" entry)
                            (gethash "evidence-node-ids" entry)))))
        (when (and (stringp statement) (plusp (length statement)) evidence
                   (every (lambda (id)
                            (ignore-errors
                              (funcall *context-projection-grounded-fn* id)))
                          evidence))
          (push (obj "id" (format nil "self-model-question:~a"
                                   (gethash "id" entry "unknown"))
                     "kind" "worldview"
                     "content" statement
                     "origin_class" "generated-cognition"
                     "epistemic_status" "active-question"
                     "grounding_status" "grounded"
                     "label" "Active grounded question"
                     "evidence_node_ids" (coerce evidence 'vector))
                rows))))))

(defun %context-projection-last-user-boundary (events)
  (let ((last-position nil))
    (loop for event in events for position from 0
          when (string= (gethash "type" event "") "user-message")
            do (setf last-position position))
    (values (if last-position (nthcdr (1+ last-position) events) events)
            (if last-position t nil))))

(defun %context-projection-audit-events (temporal-query-p now)
  (if (not temporal-query-p)
      (values nil "not-requested" nil)
      ;; NIL is a valid, meaningful result here (a successfully audited empty
      ;; interval), so use a unique sentinel to keep source failure distinct.
      (let* ((failure (gensym "AUDIT-SOURCE-FAILURE"))
             (events (%context-projection-safe-call
                      (lambda ()
                        (funcall *context-projection-events-fn*
                                 (- now *context-projection-audit-window-seconds*) now))
                      failure "audit-errors")))
        (if (eq events failure)
            (values nil "unavailable" nil)
            (multiple-value-bind (since-user boundary-found-p)
                (%context-projection-last-user-boundary events)
              (let ((eligible
                      (remove-if-not
                       (lambda (event)
                         (member (gethash "type" event "")
                                 *context-projection-audit-event-types*
                                 :test #'string=))
                       since-user)))
                (values (last eligible (min 12 (length eligible)))
                        "complete" boundary-found-p)))))))

(defun build-context-projection
    (prompt &key
              (now (or (and (boundp '*turn-capture-context*)
                            (let ((context
                                    (symbol-value '*turn-capture-context*)))
                              (and (hash-table-p context)
                                   (gethash "as_of" context))))
                       (get-universal-time)))
              (mode (%context-projection-mode)))
  "Build a read-only, typed projection object. MODE is recorded for replay;
it does not itself authorize prompt mutation."
  (let* ((query (%context-projection-text prompt))
         (correction-depth
           (or (%context-projection-temporal-correction-depth query) 0))
         (temporal-query-p (or (%context-projection-temporal-p query)
                               (plusp correction-depth)))
         (forensic-query-p (%context-projection-forensic-request-p query))
         (routine-temporal-p (and temporal-query-p (not forensic-query-p)))
         ;; A routine temporal answer is sourced from the bounded event audit.
         ;; Semantic memory, soul, affect, and drives describe other time
         ;; horizons and can both delay and contaminate that factual answer.
         ;; Explicit forensic requests retain the full projection.
         (rows (unless routine-temporal-p
                 (%context-projection-safe-call
                  (lambda ()
                    (funcall *context-projection-memory-search-fn*
                             query
                             *context-projection-memory-candidate-results*))
                  nil "retrieval-errors")))
         (eligible (remove-if-not #'%context-projection-eligible-memory-p rows))
         (content-safe
           (remove-if #'%context-projection-sensitive-memory-content-p eligible))
         (non-echo
           (remove-if
            (lambda (row)
              (let ((content (gethash "content" row "")))
                (and (plusp (length query)) (stringp content)
                     (search query content :test #'char-equal))))
            content-safe))
         (direct-anchors
           (stable-sort
            (copy-list
             (remove-if-not #'%context-projection-direct-memory-p non-echo))
            #'> :key #'%context-projection-public-memory-score))
         (materialization-applicable-p
           (and (not routine-temporal-p)
                (member mode '(:shadow :enforced))
                (boundp '*retrieval-embedding-mode*)
                (eq *retrieval-embedding-mode* :enforced)))
         (materialization-result
           (if materialization-applicable-p
               (multiple-value-list
                (%context-projection-turn-bundles query direct-anchors))
               (list nil 0 0 0 :not-applicable)))
         (materialized-bundles (first materialization-result))
         (materialization-suppressed-count (or (second materialization-result) 0))
         (materialization-considered-count (or (third materialization-result) 0))
         (materialization-safe-neighbor-count
           (or (fourth materialization-result) 0))
         (materialization-status (or (fifth materialization-result)
                                     :atomic-fallback))
         (evidence-candidates
           (if (eq materialization-status :bundled)
               materialized-bundles
               direct-anchors))
         (curator-input evidence-candidates)
         ;; The deterministic salience floor is the evidence-need gate. It is
         ;; topic-agnostic: no regex or special bedtime/appearance branch.
         ;; Keep the established projection output cap for legacy consumers,
         ;; while exposing a separately bounded pre-allocation pool to the
         ;; conversation-wide selector.  This prevents the old maximum of
         ;; three from deciding global source allocation prematurely.
         (shared-candidates
           (%context-projection-select-shared-memory
            evidence-candidates *context-projection-max-memory-results*))
         (shared
           (subseq shared-candidates
                   0 (min *context-projection-max-shared-memory-results*
                          (length shared-candidates))))
         (curator-result
           (cond
             ((or (not (eq mode :enforced))
                  (not (fboundp 'context-curator-consume))
                  (not (boundp '*context-curator-mode*))
                  (eq *context-curator-mode* :off))
              (obj "schema_version" 1 "status" "disabled"
                   "compiled_context_block" :null))
             ((or (not (boundp '*retrieval-embedding-mode*))
                  (not (eq *retrieval-embedding-mode* :enforced)))
              (obj "schema_version" 1 "status" "fallback"
                   "reason" "typed-retrieval-not-enforced"
                   "compiled_context_block" :null))
             ((or routine-temporal-p (null shared) (null curator-input))
              (obj "schema_version" 1 "status" "not-eligible"
                   "reason" "no-salient-grounded-evidence"
                   "compiled_context_block" :null))
             (t
              (funcall 'context-curator-consume
                       query curator-input :tools nil :as-of now))))
         ;; Preserve the historical open-loop horizon while direct evidence
         ;; overfetches for post-filter refill.
         (open-source (subseq non-echo 0 (min (length non-echo)
                                              *context-projection-max-memory-results*)))
         (open (remove-if-not #'%context-projection-open-memory-p open-source))
         (active-questions
           (unless routine-temporal-p
             (%context-projection-active-question-rows)))
         (active-question-followup-p
           (%context-projection-active-question-followup-p
            query active-questions))
         (soul (unless routine-temporal-p
                 (%context-projection-safe-soul-rows)))
         (audit-result
           (multiple-value-list
            (%context-projection-audit-events temporal-query-p now)))
         (audit (first audit-result))
         (audit-status (or (second audit-result) "unavailable"))
         (audit-boundary-found-p (third audit-result))
         (modulators (unless routine-temporal-p
                       (%context-projection-safe-call
                        *context-projection-modulator-fn* nil "affect-errors")))
         (drives (unless routine-temporal-p
                   (%context-projection-safe-call
                    *context-projection-drive-fn* nil "drive-errors")))
         (scheduled
           (%context-projection-safe-call
            (lambda ()
              (if (fboundp 'pai-scheduler-context-snapshot)
                  (funcall 'pai-scheduler-context-snapshot)
                  (make-array 0)))
            (make-array 0) "scheduled-context-errors"))
         (near-term
           (if (and (boundp '*near-term-intentions-mode*)
                    (eq *near-term-intentions-mode* :enforced)
                    (fboundp 'near-term-intention-events)
                    (fboundp 'near-term-workspace-for-prompt))
               (%context-projection-safe-call
                (lambda ()
                  (funcall 'near-term-workspace-for-prompt
                           (funcall 'near-term-intention-events :now now)
                           query :now now))
                nil "near-term-context-errors")
               nil))
         (curator-selected-ids
           (when (and (hash-table-p curator-result)
                      (string= (gethash "status" curator-result "")
                               "selected"))
             (%context-projection-list
              (gethash "selected_context_ids"
                       (gethash "validated_response" curator-result (obj))))))
         (source-ids
           (remove-duplicates
            (mapcan
             (lambda (row)
               (let ((evidence (gethash "evidence_node_ids" row)))
                 (if evidence (%context-projection-list evidence)
                     (list (gethash "id" row)))))
             (append (if curator-selected-ids
                         (remove-if-not
                          (lambda (row)
                            (member (gethash "id" row) curator-selected-ids
                                    :test #'string=))
                          evidence-candidates)
                         shared)
                     active-questions open soul))
            :test #'string=)))
    (obj "schema_version" 1
         "mode" (string-downcase (symbol-name mode))
         "built_at" now
         "user_local_time"
         (if (fboundp 'pai-current-time-context)
             (funcall 'pai-current-time-context now)
             "User-local timezone is unavailable; do not infer it from UTC.")
         "temporal_query" (if temporal-query-p t nil)
         "correction_depth" correction-depth
         "routine_temporal" (if routine-temporal-p t nil)
         "forensic_query" (if forensic-query-p t nil)
         "near_term_intentions_enforced"
         (if (and (boundp '*near-term-intentions-mode*)
                  (eq *near-term-intentions-mode* :enforced)) t nil)
         "active_question_followup"
         (if active-question-followup-p t nil)
         "audit_status" audit-status
         "audit_window_seconds" *context-projection-audit-window-seconds*
         "audit_user_boundary_found" (if audit-boundary-found-p t nil)
         "current_exchange"
         (vector (obj "speaker" "the operator" "epistemic_status" "user-report"
                      "content" query))
         "relevant_shared_memory"
         (coerce (mapcar #'%context-projection-copy-memory shared) 'vector)
         "relevant_shared_memory_candidates"
         (coerce (mapcar #'%context-projection-copy-memory shared-candidates)
                 'vector)
         "context_curator" curator-result
         "memory_retrieval"
         (obj "candidate_count" (length rows)
              "eligible_count" (length eligible)
              "eligibility_filtered_count" (- (length rows) (length eligible))
              "sensitive_filtered_count" (- (length eligible)
                                                (length content-safe))
              "echo_filtered_count" (- (length content-safe)
                                         (length non-echo))
              "materialization_status"
              (string-downcase (symbol-name materialization-status))
              "materialization_atomic_anchor_count" (length direct-anchors)
              "materialization_safe_neighbor_count"
              materialization-safe-neighbor-count
              "materialization_considered_bundle_count"
              materialization-considered-count
              "materialization_selected_bundle_count"
              (length materialized-bundles)
              "materialization_near_duplicate_suppression_count"
              materialization-suppressed-count
              "evidence_candidate_count" (length evidence-candidates)
              "selected_count" (length shared)
              "selected_ids"
              (coerce (mapcar (lambda (row) (gethash "id" row)) shared)
                      'vector)
              "selected_scores"
              (coerce
               (mapcar
                (lambda (row)
                  (obj "id" (gethash "id" row)
                       "similarity" (or (gethash "similarity" row) :null)
                       "public_score"
                       (%context-projection-public-memory-score row)))
                shared)
               'vector)
              "database_write_count" 0)
         "audited_background_activity" (coerce audit 'vector)
         "open_loops"
         (coerce (remove-duplicates
                  (append active-questions
                          (mapcar #'%context-projection-copy-memory open) soul)
                  :test #'string= :key (lambda (row) (gethash "content" row "")))
                 'vector)
         "affect" (or modulators (obj))
         "drives" (or drives (obj))
         "scheduled_context_shifts" scheduled
         "near_term_context" (coerce near-term 'vector)
         "source_node_ids" (coerce source-ids 'vector))))

(defun %context-projection-render-memory (rows)
  (if (null rows)
      "- No eligible grounded memory surfaced."
      (format nil "~{- ~a: ~a [id ~a]~%~}"
              (mapcan
               (lambda (row)
                 (list (gethash "label" row)
                       (%context-projection-text (gethash "content" row))
                       (gethash "id" row)))
               rows))))

(defun %context-projection-render-audit (events status window-seconds
                                         boundary-found-p)
  (cond
    ((string= status "not-requested")
     "- Not requested for this non-temporal exchange.")
    ((string= status "unavailable")
     "- The audit source was unavailable. Do not infer that no activity occurred; state this limitation honestly.")
    ((null events)
     (format nil
             "- Audit complete: no configured background-activity events were recorded ~a within the last ~a seconds. This is authoritative for that bounded event set. Answer from it directly. Do not claim you searched logs, invent activity, answer only with a question, or force a follow-up. If correcting an earlier answer, acknowledge the correction. Do not search logs, state files, databases, or source code unless the operator explicitly asks for a forensic investigation."
             (if boundary-found-p
                 "after the latest user-message boundary"
                 "in the available event window (no user-message boundary was present)")
             window-seconds))
    (t
     (with-output-to-string (stream)
       (format stream
               "- Audit complete for the bounded event set. Answer from these records directly. Do not claim you searched logs, invent activity, answer only with a question, or force a follow-up. If correcting an earlier answer, acknowledge the correction. Do not search logs, state files, databases, or source code unless the operator explicitly asks for a forensic investigation.~%")
       (dolist (event events)
         (let ((payload (gethash "payload" event (obj))))
           (format stream "- [~a] ~a~@[; status=~a~]~@[; reason=~a~]~@[; tick=~a~]~%"
                   (let ((raw (gethash "timestamp" event "unknown-time")))
                     (if (and (fboundp 'pai-format-local-time)
                              (fboundp '%event-parse-ts))
                         (handler-case
                             (funcall 'pai-format-local-time
                                      :universal-time
                                      (funcall '%event-parse-ts event))
                           (error () raw))
                         raw))
                   (gethash "type" event "unknown-event")
                   (let ((value (gethash "status" payload)))
                     (and (stringp value) value))
                   (let ((value (or (gethash "reason" payload)
                                    (gethash "result" payload))))
                     (and (stringp value) value))
                   (let ((value (or (gethash "tick_type" payload)
                                    (gethash "type" payload))))
                     (and (stringp value) value)))))))))

(defun %context-projection-render-state (affect drives)
  (labels ((sorted-pairs (table)
             (sort (loop for key being the hash-keys of table
                         using (hash-value value)
                         collect (cons key value))
                   #'string< :key (lambda (pair)
                                    (string-downcase
                                     (%context-projection-text (car pair)))))))
  (with-output-to-string (stream)
    (format stream "Affect/process state (not an external fact):")
    (if (and (hash-table-p affect) (plusp (hash-table-count affect)))
        (dolist (pair (sorted-pairs affect))
          (format stream " ~a=~,2f" (car pair) (cdr pair)))
        (format stream " unavailable"))
    (format stream "~%Drives/process state (not an instruction):")
    (if (and (hash-table-p drives) (plusp (hash-table-count drives)))
        (dolist (pair (sorted-pairs drives))
          (format stream " ~a=~a" (car pair)
                  (if (hash-table-p (cdr pair))
                      (gethash "current" (cdr pair))
                      (cdr pair))))
        (format stream " unavailable")))))

(defun %context-projection-section (title text budget)
  (%context-projection-truncate
   (format nil "### ~a~%~a" title text) budget))

(defun %context-projection-render-scheduled (records)
  (if (null records)
      "- No pending scheduled context shift."
      (with-output-to-string (stream)
        (format stream
                "These are real scheduler triggers, not messages the operator just sent. Use them as a context shift; do not rewrite them as user speech or claim unscheduled work.~%")
        (dolist (record records)
          (format stream "- [~a] ~a (schedule ~a; delivery ~a)~%"
                  (gethash "fired_at_local" record "unknown local time")
                  (gethash "text" record "")
                  (gethash "schedule_id" record "unknown")
                  (gethash "delivery_status" record "unknown"))))))

(defun %context-projection-render-near-term (records)
  (if (null records)
      "- No active conversational commitment."
      (with-output-to-string (stream)
        (format stream
                "Private bounded working-state conclusions. They are not user messages, do not prove unrecorded thought, and grant no permission to send or use tools.~%")
        (dolist (record records)
          (format stream "- [~a] ~a~@[; result: ~a~] (receipt ~a; passes ~a/~a)~%"
                  (gethash "state" record "unknown")
                  (gethash "summary" record "")
                  (let ((artifact (gethash "artifact_summary" record)))
                    (and (stringp artifact) artifact))
                  (gethash "commitment_receipt_id" record "unknown")
                  (gethash "pass_count" record 0)
                  (gethash "max_passes" record 0))))))

(defun render-context-projection (projection)
  "Render exactly one bounded, idempotent PAI-STATE block."
  (let* ((shared (%context-projection-list
                  (gethash "relevant_shared_memory" projection)))
         (curator (gethash "context_curator" projection))
         (curator-status
           (and (hash-table-p curator) (gethash "status" curator)))
         (memory-rendering
           (cond
             ((and curator-status (string= curator-status "selected"))
              (%context-projection-text
               (gethash "compiled_context_block" curator "")))
             ((and curator-status (string= curator-status "no-extra-context"))
              "- The private curator selected no additional memory context for this turn.")
             (t (%context-projection-render-memory shared))))
         (audit (%context-projection-list
                 (gethash "audited_background_activity" projection)))
         (open (%context-projection-list (gethash "open_loops" projection)))
         (scheduled (%context-projection-list
                     (gethash "scheduled_context_shifts" projection)))
         (near-term (%context-projection-list
                     (gethash "near_term_context" projection)))
         (publication-guidance
           (%context-projection-text
            (gethash "publication_guidance" projection "")))
         (sections
           (list
            (%context-projection-section
             "Conversational shape for this turn"
             (if (plusp (length publication-guidance))
                 publication-guidance
                 "No additional turn-specific guidance.")
             *context-projection-publication-budget*)
            (%context-projection-section
             "Current local time"
             (gethash "user_local_time" projection
                      "Current local time unavailable.")
             *context-projection-current-budget*)
            (%context-projection-section
             (if (and curator-status
                      (member curator-status '("selected" "no-extra-context")
                              :test #'string=))
                 "Curated relevant context (typed, grounded, and validated)"
                 "Relevant shared memory (typed and grounded)")
             memory-rendering
             (if (and curator-status (string= curator-status "selected"))
                 *context-projection-curator-budget*
                 *context-projection-memory-budget*))
            (%context-projection-section
             "Audited background activity"
             (%context-projection-render-audit
              audit
              (gethash "audit_status" projection "unavailable")
              (gethash "audit_window_seconds" projection
                       *context-projection-audit-window-seconds*)
              (gethash "audit_user_boundary_found" projection))
             *context-projection-audit-budget*)
            (%context-projection-section
             "Open loops and generated cognition (not observations)"
             (%context-projection-render-memory open)
             *context-projection-open-loop-budget*)
            (%context-projection-section
             "Scheduled context shifts"
             (%context-projection-render-scheduled scheduled)
             *context-projection-scheduled-budget*)
            (%context-projection-section
             "Near-term conversational commitments"
             (%context-projection-render-near-term near-term)
             *context-projection-near-term-budget*)
            (%context-projection-section
             "Affect and drives"
             (%context-projection-render-state
              (gethash "affect" projection) (gethash "drives" projection))
             *context-projection-affect-budget*)))
         (begin "<!-- PAI-STATE:BEGIN -->")
         (end "<!-- PAI-STATE:END -->")
         (body (format nil "~{~a~^~%~%~}" sections))
         (available (- *context-projection-total-budget*
                       (length begin) (length end) 2)))
    (format nil "~a~%~a~%~a" begin
            (%context-projection-truncate body available) end)))

(defun %context-projection-strip-range (text begin end)
  (loop with value = text
        for bp = (search begin value)
        while bp
        for ep = (search end value :start2 (+ bp (length begin)))
        do (setf value
                 (if ep
                     (concatenate 'string (subseq value 0 bp)
                                  (subseq value (+ ep (length end))))
                     (subseq value 0 bp)))
        finally (return value)))

(defun %context-projection-replace-all (text old new)
  (loop with value = (%context-projection-text text)
        with start = 0
        for position = (search old value :start2 start)
        while position
        do (setf value
                 (concatenate 'string (subseq value 0 position) new
                              (subseq value (+ position (length old))))
                 start (+ position (length new)))
        finally (return value)))

(defun %context-projection-normalize-tool-policy (text)
  "Migrate persisted prompts from unconditional tool use to the actual
per-turn availability contract. Exact replacements are idempotent."
  (let ((value (%context-projection-text text)))
    (dolist
        (pair
          '(("Competent and direct. I operate the tools; I don't hesitate to use them."
             "Competent and direct. When tools are available for the current turn, I use them deliberately.")
            ("I prefer tools over speculation."
             "When tools are available and evidence has not already been supplied authoritatively, I prefer tools over speculation.")
            ("## My tools (I always prefer using them over guessing)"
             "## My currently available tools")
            ("## Self-knowledge tools (call these via lisp-eval, don't guess)"
             "## Self-knowledge tools (when lisp-eval is available for the current turn)")
            ("I call this when asked what"
             "When lisp-eval is available, I call this when asked what")
            ("When asked which of my own changes worked, I call this instead"
             "When lisp-eval is available and I am asked which of my own changes worked, I call this instead")
            ("my first move is to inspect"
             "my first move, when lisp-eval is available, is to inspect")))
      (setf value (%context-projection-replace-all
                   value (first pair)
                   ;; Several old phrases are suffixes of their replacements.
                   ;; Do not re-expand an already migrated policy on each turn.
                   (if (search (second pair) value)
                       (first pair)
                       (second pair)))))
    value))

(defun context-projection-strip-legacy (text)
  "Remove every known legacy dynamic block and the unmarked boot-time
journal/legacy-graph suffix. Stable persona, safety, and TOOLS remain."
  (let ((value (%context-projection-text text)))
    (dolist (markers *context-projection-dynamic-markers*)
      (setf value (%context-projection-strip-range
                   value (first markers) (second markers))))
    (let ((cut nil))
      (dolist (marker *context-projection-unmarked-legacy-markers*)
        (let ((position (search marker value)))
          (when (and position (or (null cut) (< position cut)))
            (setf cut position))))
      (string-right-trim '(#\Space #\Tab #\Newline #\Return)
                         (if cut (subseq value 0 cut) value)))))

(defun %context-projection-install (projection)
  (let ((sysmsg
          (and (boundp '*last-self-mod-history*)
               (find "system" *last-self-mod-history*
                     :key (lambda (message) (gethash "role" message))
                     :test #'string=))))
    (when sysmsg
      (let* ((before (%context-projection-text (gethash "content" sysmsg)))
             ;; The permanent conversation is evidence, not a stable-prompt
             ;; template.  Once the deterministic owner is loaded, rebuild
             ;; stable instructions from its explicit fragments every turn.
             ;; Only the bounded continuity data fragment may cross forward
             ;; from the previous rendered record.
             (continuity
               (and (fboundp '%ccb-embedded-continuity)
                    (funcall '%ccb-embedded-continuity sysmsg)))
             (stable
               (if (fboundp 'public-system-prompt-render-stable)
                   (funcall 'public-system-prompt-render-stable)
                   (%context-projection-normalize-tool-policy
                    (context-projection-strip-legacy before))))
             (rendered (render-context-projection projection))
             (assembled (format nil "~a~%~%~a" stable rendered)))
        (setf (gethash "content" sysmsg)
              assembled)
        (when (and continuity (plusp (length continuity))
                   (fboundp '%ccb-canonical-with-summary))
          (setf (gethash "content" sysmsg)
                (gethash "content"
                         (funcall '%ccb-canonical-with-summary
                                  sysmsg continuity))))
        (values t (length before) (length rendered))))))

(defun %context-projection-current-system-content ()
  (let ((sysmsg
          (and (boundp '*last-self-mod-history*)
               (find "system" *last-self-mod-history*
                     :key (lambda (message) (gethash "role" message))
                     :test #'string=))))
    (and sysmsg (%context-projection-text (gethash "content" sysmsg)))))

(defun %context-projection-copy-hash-table (table)
  "Copy a message object before projection mutates its CONTENT field."
  (let ((copy (make-hash-table :test (hash-table-test table))))
    (maphash (lambda (key value) (setf (gethash key copy) value)) table)
    copy))

(defun %context-projection-ensure-initial-system-history (mode)
  "Seed a fresh ENFORCED conversation before projection installation.
The base AUTO-TURN normally inserts *SELF-MOD-SYSTEM* only after this wrapper
runs, which made the first candidate turn silently behave like legacy. Use a
copy so installing dynamic state never mutates the canonical fresh-chat seed."
  (when (and (eq mode :enforced)
             (boundp '*last-self-mod-history*)
             (null *last-self-mod-history*)
             (boundp '*self-mod-system*)
             (hash-table-p (symbol-value '*self-mod-system*)))
    ;; The legacy first-turn wrapper normally refreshes this section, but
    ;; seeding history here means that wrapper will see a non-empty history.
    (when (fboundp '%pai-refresh-tools-section)
      (funcall '%pai-refresh-tools-section))
    (setf *last-self-mod-history*
          (list (%context-projection-copy-hash-table
                 (symbol-value '*self-mod-system*))))
    t))

(defun %context-projection-replace-tools-block (text replacement)
  (let* ((value (%context-projection-text text))
         (begin "<!-- TOOLS:BEGIN -->")
         (end "<!-- TOOLS:END -->")
         (bp (search begin value))
         (ep (and bp (search end value :start2 (+ bp (length begin))))))
    (if (and bp ep)
        (concatenate 'string (subseq value 0 bp) replacement
                     (subseq value (+ ep (length end))))
        value)))

(defun %context-projection-canonical-tools-block ()
  (if (fboundp 'public-system-prompt-render-tools-block)
      (funcall 'public-system-prompt-render-tools-block)
      (when (and (boundp '*self-mod-system*)
                 (hash-table-p (symbol-value '*self-mod-system*)))
        (let* ((content (gethash "content" (symbol-value '*self-mod-system*) ""))
               (begin "<!-- TOOLS:BEGIN -->")
               (end "<!-- TOOLS:END -->")
               (bp (search begin content))
               (ep (and bp (search end content :start2 (+ bp (length begin))))))
          (and bp ep (subseq content bp (+ ep (length end))))))))

(defun %context-projection-sync-tools-guidance (suppress-p)
  "Keep the prompt's advertised tools identical to the turn's tool schema."
  (let ((sysmsg
          (and (boundp '*last-self-mod-history*)
               (find "system" *last-self-mod-history*
                     :key (lambda (message) (gethash "role" message))
                     :test #'string=))))
    (when sysmsg
      (let ((replacement
              (if suppress-p
                  (format nil
                          "<!-- TOOLS:BEGIN -->~%No tools are available for this routine temporal answer. Use the audited state directly; do not promise to check tools or logs later.~%<!-- TOOLS:END -->")
                  (%context-projection-canonical-tools-block))))
        (when replacement
          (setf (gethash "content" sysmsg)
                (%context-projection-replace-tools-block
                 (gethash "content" sysmsg "") replacement))
          t)))))

(defvar *context-projection-installed-auto-wrapper* nil)
(let* ((current (fdefinition 'auto-turn))
       (effective
         (if (and (boundp '*timing-installed-wrappers*)
                 (hash-table-p (symbol-value '*timing-installed-wrappers*))
                 (gethash 'auto-turn
                          (symbol-value '*timing-installed-wrappers*))
                 (eq current
                     (gethash 'auto-turn
                              (symbol-value '*timing-installed-wrappers*)))
                 (fboundp 'pai-base-auto-turn-timing))
             (fdefinition 'pai-base-auto-turn-timing)
             current)))
  (unless (and *context-projection-installed-auto-wrapper*
               (or (eq current *context-projection-installed-auto-wrapper*)
                   (eq effective *context-projection-installed-auto-wrapper*)))
    (setf (fdefinition 'pai-base-auto-turn-context-projection) effective)))

(defun auto-turn (prompt)
  (let ((mode (%context-projection-mode)))
    (let ((*embedding-turn-cache*
            (if (eq mode :legacy)
                *embedding-turn-cache*
                (make-hash-table :test #'equal)))
          (*embedding-turn-cache-turn-hits* 0)
          (*embedding-turn-cache-turn-misses* 0)
          (*temporal-response-policy-context* nil)
          (*temporal-response-policy-correction-p* nil)
          (*publication-contract-current* nil)
          (*context-projection-scheduled-context-ids* nil))
      (if (eq mode :legacy)
          (funcall 'pai-base-auto-turn-context-projection prompt)
          (unwind-protect
              (progn
                (let ((seeded-initial-system
                        (%context-projection-ensure-initial-system-history mode))
                      (suppress-public-tools nil)
                      (disable-public-reasoning nil))
                  ;; Only candidate construction/mutation is inside the
                  ;; projection timing and fallback boundary. The public turn
                  ;; is deliberately outside: a model/tool error must
                  ;; propagate normally and must never invoke it twice.
                  (handler-case
                      (%context-projection-with-timing
                       mode
                       (lambda ()
                         (let* ((projection
                                  (build-context-projection prompt :mode mode))
                                (scheduled-records
                                  (%context-projection-list
                                   (gethash "scheduled_context_shifts"
                                            projection)))
                                (correction-depth
                                  (gethash "correction_depth" projection 0))
                                (correction-p (plusp correction-depth))
                                (tools-configured-p
                                  (and (boundp '*tools*)
                                       (plusp (length *tools*))))
                                (provisional-contract
                                  (%context-projection-publication-contract
                                   prompt projection correction-depth
                                   tools-configured-p))
                                (check-in-p
                                  (and (hash-table-p provisional-contract)
                                       (string= "check-in"
                                                (gethash "intent"
                                                         provisional-contract
                                                         "conversation"))))
                                (suppress-tools-for-turn
                                  (and (eq mode :enforced)
                                       (or check-in-p
                                           (and
                                            (gethash "temporal_query" projection)
                                            (not
                                             (%context-projection-forensic-request-p
                                              prompt))))))
                                (public-tools-available-p
                                  (and (not suppress-tools-for-turn)
                                       tools-configured-p))
                                (publication-contract
                                  (if (eq public-tools-available-p
                                          tools-configured-p)
                                      provisional-contract
                                      (%context-projection-publication-contract
                                       prompt projection correction-depth
                                       public-tools-available-p)))
                                (publication-guidance
                                  (and (hash-table-p publication-contract)
                                       (fboundp
                                        'render-publication-generation-guidance)
                                       (funcall
                                        'render-publication-generation-guidance
                                        publication-contract)))
                                (publication-guidance-chars
                                  (if publication-guidance
                                      (progn
                                        (setf (gethash "publication_guidance"
                                                       projection)
                                              publication-guidance)
                                        (length publication-guidance))
                                      0))
                                (rendered
                                  (render-context-projection projection))
                                (source-ids
                                  (gethash "source_node_ids" projection))
                                (installed nil)
                                (current-system
                                  (%context-projection-current-system-content))
                                (legacy-chars
                                  (length (or current-system "")))
                                (candidate-system-chars
                                  (+ (length
                                      (%context-projection-normalize-tool-policy
                                       (context-projection-strip-legacy
                                        (or current-system ""))))
                                     2 (length rendered))))
                           (setf *context-projection-scheduled-context-ids*
                                 (remove nil
                                         (mapcar (lambda (record)
                                                   (gethash "id" record))
                                                 scheduled-records)))
                           (setf suppress-public-tools suppress-tools-for-turn)
                           (setf disable-public-reasoning
                                 (and (eq mode :enforced) check-in-p))
                           ;; The general publication boundary needs the
                           ;; contract on check-ins and ordinary turns too.
                           ;; Temporal correction remains narrowly classified.
                           (setf *temporal-response-policy-context* projection
                                 *temporal-response-policy-correction-p*
                                 correction-p
                                 *publication-contract-current*
                                 publication-contract)
                           (when (eq mode :enforced)
                             (multiple-value-setq (installed legacy-chars)
                               (%context-projection-install projection))
                             (when installed
                               (%context-projection-sync-tools-guidance
                                suppress-public-tools)
                               ;; In enforced mode report the exact installed
                               ;; prompt after normalization and tool-policy
                               ;; synchronization, not a pre-install estimate.
                               (setf candidate-system-chars
                                     (length
                                      (or (%context-projection-current-system-content)
                                          "")))))
                           (%context-projection-stat
                            (if (eq mode :shadow)
                                "shadow-built" "enforced-built"))
                           (ignore-errors
                             (funcall
                              *context-projection-event-fn*
                              (if (eq mode :shadow)
                                  "context-projection-shadow"
                                  "context-projection-applied")
                              (obj
                               "mode" (string-downcase (symbol-name mode))
                               "turn_id" (%context-projection-current-turn-id)
                               "projection_chars" (length rendered)
                               "publication_guidance_chars"
                               publication-guidance-chars
                               "legacy_system_chars" legacy-chars
                               "candidate_system_chars" candidate-system-chars
                               "system_char_delta"
                               (- candidate-system-chars legacy-chars)
                               "source_node_ids" source-ids
                               "source_node_count" (length source-ids)
                               "temporal_query"
                               (if (gethash "temporal_query" projection) t nil)
                               "correction_depth" correction-depth
                               "routine_temporal"
                               (if (gethash "routine_temporal" projection) t nil)
                               "forensic_query"
                               (if (gethash "forensic_query" projection) t nil)
                               "seeded_initial_system"
                               (if seeded-initial-system t nil)
                               "public_tools_suppressed"
                               (if suppress-public-tools t nil)
                               "public_reasoning_disabled"
                               (if disable-public-reasoning t nil)
                               "installed" (if installed t nil)))))))
                    (error (condition)
                      (%context-projection-stat "fallback-errors")
                      (ignore-errors
                        (funcall
                         *context-projection-event-fn*
                         "context-projection-fallback"
                         (obj "mode" (string-downcase (symbol-name mode))
                              "turn_id" (%context-projection-current-turn-id)
                               "condition"
                               (string-downcase
                                (symbol-name (type-of condition))))))))
                ;; Advisory prompt text did not reliably stop temporal
                ;; filesystem/database scavenging. Enforce the same authority
                ;; boundary in the tool schema, while preserving an explicit
                ;; forensic opt-in path.
                (let ((*call-model-reasoning-override*
                        (if disable-public-reasoning
                            :disabled
                            *call-model-reasoning-override*)))
                  (let ((result
                          (if (and suppress-public-tools (boundp '*tools*))
                              (progv '(*tools*) (list (make-array 0))
                                (funcall 'pai-base-auto-turn-context-projection
                                         prompt))
                              (funcall 'pai-base-auto-turn-context-projection
                                       prompt))))
                    (when (and (eq mode :enforced)
                               *context-projection-scheduled-context-ids*
                               (fboundp 'pai-scheduler-context-consume))
                      (funcall 'pai-scheduler-context-consume
                               *context-projection-scheduled-context-ids*))
                    result))))
            (ignore-errors
              (funcall *context-projection-event-fn*
                      "embedding-turn-cache-turn"
                      (obj "turn_id" (%context-projection-current-turn-id)
                           "mode" (string-downcase (symbol-name mode))
                           "hits" *embedding-turn-cache-turn-hits*
                           "misses" *embedding-turn-cache-turn-misses*
                            "entry_count"
                            (if (hash-table-p *embedding-turn-cache*)
                               (hash-table-count *embedding-turn-cache*) 0)))))))))

(setf *context-projection-installed-auto-wrapper* (fdefinition 'auto-turn))
