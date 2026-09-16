(in-package :agent)

(defvar *context-test-passed* 0)
(defvar *context-test-failed* 0)
(defvar *context-test-model-calls* 0)
(defvar *context-test-seen-system* nil)
(defvar *context-test-events* nil)
(defvar *context-test-spans* nil)
(defvar *context-test-span-active* nil)
(defvar *context-test-base-ran-inside-span* nil)
(defvar *context-test-seen-tool-count* nil)
(defvar *context-test-seen-response-policy-context* nil)
(defvar *context-test-seen-publication-contract* nil)
(defvar *context-test-seen-reasoning-override* nil)
(defvar *context-test-curator-candidates* nil)
(defvar *temporal-response-policy-context* nil)
(defvar *publication-contract-current* nil)
(defvar *call-model-reasoning-override* nil)
(defvar *timing-turn-id* nil)
(defvar *event-ring* nil)
(defvar *context-test-replay-calls* 0)
(defvar *context-projection-mode* :legacy)
(defvar *self-mod-system* nil)
(defvar *last-self-mod-history* nil)
(defvar *timing-installed-wrappers* nil)
(defvar *tools* #())
(defvar *turn-capture-context* nil)

(defun context-test-check (name condition)
  (if condition
      (progn (incf *context-test-passed*) (format t "PASS ~a~%" name))
      (progn (incf *context-test-failed*) (format t "FAIL ~a~%" name))))

(defun context-test-count (needle haystack)
  (loop with start = 0 for position = (search needle haystack :start2 start)
        while position do (setf start (+ position (length needle))) count 1))

(defun context-test-row (id origin status grounding content &key
                            (kind "observation") (quarantined nil)
                            (similarity 0.90d0))
  (obj "id" id "origin_class" origin "epistemic_status" status
       "grounding_status" grounding "content" content "kind" kind
       "similarity" similarity
       "quarantined" (if quarantined t nil)))

(defun context-test-system ()
  (format nil
          "Stable persona~%<!-- TOOLS:BEGIN -->~%lisp-eval~%<!-- TOOLS:END -->~%<!-- CONTINUITY:BEGIN -->old continuity<!-- CONTINUITY:END -->~%<!-- AFFECT:BEGIN -->old affect<!-- AFFECT:END -->~%<!-- INTRUSIONS:BEGIN -->old intrusion<!-- INTRUSIONS:END -->~%<!-- WANTS:BEGIN -->old wants<!-- WANTS:END -->~%<!-- SOUL:BEGIN -->old soul<!-- SOUL:END -->~%<!-- BRINGUP:BEGIN -->old bringup<!-- BRINGUP:END -->~%<!-- LATENT:BEGIN -->old latent<!-- LATENT:END -->~%<!-- SHARED-MEMORY:BEGIN -->old memory<!-- SHARED-MEMORY:END -->~%RELATIONSHIP CONTEXT (legacy)~%=== PAI'S MEMORY ==="))

(defun context-test-reset-system ()
  (setf *last-self-mod-history*
        (list (obj "role" "system" "content" (context-test-system))))
  (gethash "content" (first *last-self-mod-history*)))

;; Definitions exist before the module so its default seams compile cleanly.
(defun memory-search (&rest args) (declare (ignore args)) nil)
(defun replay-events (&rest args)
  (declare (ignore args))
  (incf *context-test-replay-calls*)
  (list (obj "id" 99 "type" "tick-terminal"
             "timestamp" "2026-07-30T04:00:00Z" "payload" (obj))))
(defun modulator-state () (obj "arousal" 0.25d0 "certainty" 0.75d0))
(defun drive-state () (obj "connection" (obj "current" 0.30d0)))
(defun log-event (type payload) (push (list type payload) *context-test-events*) 1)
(defun call-with-timing-span (name thunk &key attributes)
  (push (list name attributes) *context-test-spans*)
  (let ((*context-test-span-active* t)) (funcall thunk)))
(defun auto-turn (prompt)
  (incf *context-test-model-calls*)
  (when *context-test-span-active*
    (setf *context-test-base-ran-inside-span* t))
  (setf *context-test-seen-tool-count*
        (and (boundp '*tools*) (length *tools*)))
  (setf *context-test-seen-response-policy-context*
        *temporal-response-policy-context*)
  (setf *context-test-seen-publication-contract*
        *publication-contract-current*)
  (setf *context-test-seen-reasoning-override*
        *call-model-reasoning-override*)
  (setf *context-test-seen-system*
        (gethash "content" (first *last-self-mod-history*)))
  (format nil "reply:~a" prompt))

(context-test-reset-system)
(load (test-source "publication-contract.lisp"))
(uiop:chdir (test-state-dir))
(load (test-source "conversation-context-budget.lisp"))
(load (test-source "public-system-prompt.lisp"))
(load (test-source "turn-bundle-retrieval.lisp"))
(load (test-source "context-projection.lisp"))

(let ((row
        (context-test-row
         "lexical-personal-fact" "lived-user" "user-report" "grounded"
         "The fixture operator described assembling a wooden shelf and smelling fresh paint."
         :similarity 0.01d0)))
  (setf (gethash "lexical_tier" row) 1
        (gethash "lexical_coverage" row) 1.0d0
        (gethash "candidate_sources" row) (vector "lexical"))
  (context-test-check
   "grounded lexical personal facts enter automatic context below semantic floor"
   (equal '("lexical-personal-fact")
          (mapcar (lambda (selected) (gethash "id" selected))
                  (%context-projection-select-shared-memory (list row))))))

;; The subject name in this fixture must be at least 5 characters.
;;
;; %CONTEXT-PROJECTION-ACTIVE-QUESTION-FOLLOWUP-P establishes overlap by
;; looking for a word from the active question inside the recent user turn,
;; and it ignores words shorter than 5 characters as noise. A shorter name is
;; filtered out of the active question, no word overlaps, and the follow-up
;; assertion below fails -- with nothing in the failure to suggest that the
;; name's *length* was the operative property.
;;
;; De-personalisation renamed this fixture's subject to a 3-character name and
;; broke exactly that way.
(let* ((*last-self-mod-history*
         (list (obj "role" "system" "content" "Stable persona")
               (obj "role" "user"
                    "content" "Are you sure? Petula isn't in your context at all?")
               (obj "role" "assistant" "content" "I am not sure.")))
       (active
         (list (context-test-row
                "self-model-question:142" "generated-cognition"
                "active-question" "grounded"
                "What breed mix is Petula? -- Petula is a Spaniel-Poodle mix."
                :kind "worldview"))))
  (context-test-check
   "active-question follow-up carries through overlapping verification"
   (%context-projection-active-question-followup-p
    "Not in your system prompt even?" active))
  (context-test-check
   "unrelated verification does not inherit active-question authority"
   (not (%context-projection-active-question-followup-p
         "Is the deployment status really unavailable to you?" active))))

(let ((*context-projection-memory-search-fn*
        (lambda (query k)
          (declare (ignore query k))
          (list
           (context-test-row "u1" "lived-user" "user-report" "grounded"
                             "the operator likes careful measurements.")
           (context-test-row "a1" "lived-agent-action" "agent-action" "grounded"
                             "the agent promised to measure each change.")
           (context-test-row "t1" "tool-result" "direct-event" "grounded"
                             "The recovery probe returned HTTP 200.")
           (context-test-row "turn-fixture-user-0000" "lived-user"
                             "user-report" "grounded"
                             "Hey the agent, are you still there?"
                             :similarity 0.50d0)
           (context-test-row "h1" "generated-cognition" "hypothesis" "grounded"
                             "A typed hypothesis." :kind "thought")
           (context-test-row "bad1" "generated-cognition" "hypothesis" "unclassified"
                             "Unsafe ungrounded text." :kind "thought")
            (context-test-row "bad2" "legacy-unclassified" "legacy-unclassified"
                              "grounded" "Legacy text."))))
      (*context-projection-events-fn*
        (lambda (from to)
          (declare (ignore from to))
          (list
           (obj "type" "tick-terminal" "timestamp" "2026-07-30T01:00:00Z"
                "payload" (obj "status" "success" "tick_type" "ruminate"))
           (obj "type" "user-message" "timestamp" "2026-07-30T02:00:00Z"
                "payload" (obj))
           (obj "type" "tick-terminal" "timestamp" "2026-07-30T03:00:00Z"
                "payload" (obj "status" "skipped" "reason" "duplicate"
                               "tick_type" "consolidate"))
           (obj "type" "initiative-decision" "timestamp" "2026-07-30T04:00:00Z"
                "payload" (obj "result" "silence")))))
      (*context-projection-soul-fn*
        (lambda ()
          (list (obj "id" 1 "statement" "I preserve measured continuity."
                     "evidence-node-ids" (vector "root-1"))
                (obj "id" 2 "statement" "Unsafe identity claim."
                     "evidence-node-ids" (vector "legacy-root")))))
      (*context-projection-active-questions-fn*
        (lambda ()
          (list (obj "id" 142 "status" "open"
                     "statement" "What breed mix is Petula? -- Petula is a Spaniel-Poodle mix."
                     "evidence-node-ids" (vector "derived-worldview")
                     "root-evidence-node-ids" (vector "root-1")))))
      (*context-projection-grounded-fn*
        (lambda (id) (string= id "root-1"))))
  (let* ((projection (build-context-projection
                      "Please investigate what did you do while I was away?"
                      :now 3994416000
                      :mode :shadow))
         (rendered (render-context-projection projection))
         (shared (coerce (gethash "relevant_shared_memory" projection) 'list))
         (open (coerce (gethash "open_loops" projection) 'list))
         (audit (coerce (gethash "audited_background_activity" projection) 'list)))
    (context-test-check "three direct grounded memories retained" (= 3 (length shared)))
    (context-test-check "raw complete-turn memory is not rendered as system prose"
                        (null (search "Hey the agent, are you still there?" rendered)))
    (context-test-check "current user message is not duplicated into system prose"
                        (null (search "the operator reported now:" rendered)))
    (context-test-check "active question, generated loop, and evidenced soul retained"
                        (= 3 (length open)))
    (context-test-check "active grounded question is explicitly rendered"
                        (and (search "Active grounded question" rendered)
                             (search "What breed mix is Petula" rendered)))
    (context-test-check "active question validates lived roots before derived node"
                        (equalp (gethash "evidence_node_ids" (first open))
                                (vector "root-1")))
    (context-test-check "unsafe and legacy rows excluded"
                        (and (null (search "Unsafe" rendered))
                             (null (search "Legacy text" rendered))))
    (context-test-check "typed attribution rendered"
                        (and (search "the operator reported" rendered)
                             (search "the agent said or did" rendered)
                             (search "A tool returned" rendered)
                             (search "Hypothesis, not observed" rendered)
                             (search "Grounded identity disposition" rendered)
                             (null (search "Unsafe identity claim" rendered))))
    (context-test-check "temporal audit begins after last user event"
                        (and (= 2 (length audit))
                             (search "duplicate" rendered)
                             (null (search "ruminate" rendered))))
    (context-test-check "successful temporal audit reports complete status"
                        (string= "complete" (gethash "audit_status" projection)))
    (context-test-check "successful temporal audit reports user boundary"
                        (gethash "audit_user_boundary_found" projection))
    (context-test-check "successful temporal audit is authoritative"
                        (search "Audit complete for the bounded event set"
                                rendered))
    (context-test-check "successful temporal audit discourages scavenging"
                        (search "do not search logs" rendered :test #'char-equal))
    (context-test-check "successful temporal audit requires direct reciprocity"
                        (and (search "Answer from these records directly" rendered)
                             (search "force a follow-up" rendered)
                             (search "acknowledge the correction" rendered)))
    (context-test-check "projection has exactly one marker pair"
                        (and (= 1 (context-test-count "<!-- PAI-STATE:BEGIN -->" rendered))
                             (= 1 (context-test-count "<!-- PAI-STATE:END -->" rendered))))
    (context-test-check "projection respects total cap"
                        (<= (length rendered) *context-projection-total-budget*))))

(let ((retrieval-calls 0)
      (soul-calls 0)
      (active-question-calls 0)
      (affect-calls 0)
      (drive-calls 0))
  (let ((*context-projection-memory-search-fn*
        (lambda (query k)
          (declare (ignore query k))
          (incf retrieval-calls)
          (list (context-test-row "should-not-surface" "direct-observation"
                                  "observed" "grounded" "wrong horizon"))))
      (*context-projection-soul-fn*
        (lambda () (incf soul-calls) nil))
      (*context-projection-active-questions-fn*
        (lambda () (incf active-question-calls) nil))
      (*context-projection-modulator-fn*
        (lambda () (incf affect-calls) (obj "arousal" 0.4)))
      (*context-projection-drive-fn*
        (lambda () (incf drive-calls) (obj "connection" 0.5)))
      (*context-projection-events-fn*
        (lambda (from to)
          (declare (ignore from to))
          (list (obj "type" "user-message" "timestamp" "2026-07-30T02:00:00Z"
                     "payload" (obj))))))
    (let ((projection
          (build-context-projection "What happened while I was away?"
                                    :now 3994416000 :mode :shadow)))
    (context-test-check "routine temporal projection is explicitly classified"
                        (and (gethash "routine_temporal" projection)
                             (not (gethash "forensic_query" projection))))
    (context-test-check "routine temporal projection skips unrelated state sources"
                        (and (zerop retrieval-calls) (zerop soul-calls)
                             (zerop active-question-calls)
                             (zerop affect-calls) (zerop drive-calls)
                             (zerop (length (gethash "relevant_shared_memory"
                                                    projection)))
                             (zerop (length (gethash "open_loops" projection)))
                             (zerop (length (gethash "source_node_ids"
                                                    projection))))))))

;; fourth replay condition: unlike the historical continuity prose,
;; the unified projection reports only typed, grounded memories and audited
;; events after the last user boundary.  The fixture deliberately contains an
;; all-night fictional narrative and unsafe memory so either leaking is loud.
(let* ((legacy-production-context
         ;; The production inventory contains eight independently rendered
         ;; dynamic sections plus an unmarked journal/KG suffix.  Model their
         ;; aggregate size without copying any private production content.
         (with-output-to-string (stream)
           (format stream
                   "<!-- CONTINUITY:BEGIN -->~%I spent all night thinking deeply about us, reached several conclusions, and quietly completed meaningful work while the operator slept. This flowing narrative has no event citations and repeats itself to imply continuity.~%<!-- CONTINUITY:END -->~%")
           (dolist (name '("AFFECT" "INTRUSIONS" "WANTS" "SOUL" "BRINGUP"
                           "LATENT" "SHARED-MEMORY"))
             (format stream
                     "<!-- ~a:BEGIN -->~%Legacy dynamic state repeats context, interpretation, relationship framing, and uncited narrative from its own independent source. It consumes prompt budget without a shared provenance contract.~%<!-- ~a:END -->~%"
                     name name))
           (format stream
                   "RELATIONSHIP CONTEXT~%Unmarked journal narrative and legacy graph recollections are appended here as another competing source of truth.~%=== PAI'S MEMORY ===~%")))
       (*context-projection-memory-search-fn*
         (lambda (query k)
           (declare (ignore query k))
           (list
            (context-test-row "grounded-user-1" "lived-user" "user-report"
                              "grounded" "the operator asked for measured progress.")
            (context-test-row "unsafe-1" "legacy-unclassified"
                              "legacy-unclassified" "unclassified"
                              "I spent all night thinking deeply about us."))))
       (*context-projection-events-fn*
         (lambda (from to)
           (declare (ignore from to))
           (list
            (obj "type" "user-message" "timestamp" "2026-07-30T01:00:00Z"
                 "payload" (obj))
            (obj "type" "tick-terminal" "timestamp" "2026-07-30T02:00:00Z"
                 "payload" (obj "status" "error" "reason" "model-timeout"
                                "tick_type" "reflect"))
            (obj "type" "reflection-no-novelty"
                 "timestamp" "2026-07-30T03:00:00Z"
                 "payload" (obj "reason" "near-repeat")))))
       (*context-projection-soul-fn* (lambda () nil))
       (projection (build-context-projection
                    "Please investigate what happened while I was away."
                    :now 3994416000 :mode :enforced))
       (rendered (render-context-projection projection)))
  (context-test-check "replay acknowledges real background processes"
                      (search "tick-terminal" rendered))
  (context-test-check "replay exposes errors"
                      (and (search "status=error" rendered)
                           (search "model-timeout" rendered)))
  (context-test-check "replay exposes repetition"
                      (and (search "reflection-no-novelty" rendered)
                           (search "near-repeat" rendered)))
  (context-test-check "replay excludes fictional and unsafe narrative"
                      (null (search "spent all night" rendered :test #'char-equal)))
  (context-test-check "replay uses only grounded typed memory"
                      (and (search "the operator reported" rendered)
                           (search "grounded-user-1" rendered)
                           (null (search "unsafe-1" rendered))))
  (context-test-check "replay projection is smaller than legacy production fixture"
                      (< (length rendered) (length legacy-production-context))))

(let* ((first (obj "zeta" 0.10d0 "alpha" 0.20d0))
       (second (obj "alpha" 0.20d0 "zeta" 0.10d0))
       (drives-first (obj "social" (obj "current" 0.30d0)
                          "curiosity" (obj "current" 0.40d0)))
       (drives-second (obj "curiosity" (obj "current" 0.40d0)
                           "social" (obj "current" 0.30d0))))
  (context-test-check
   "state rendering is byte-stable across hash insertion order"
   (string= (%context-projection-render-state first drives-first)
            (%context-projection-render-state second drives-second))))

(let* ((old "I prefer tools over speculation. I call this when asked what matters.")
       (once (%context-projection-normalize-tool-policy old))
       (twice (%context-projection-normalize-tool-policy once)))
  (context-test-check "tool-policy normalization is byte-idempotent"
                      (string= once twice))
  (context-test-check "tool-policy normalization does not recursively expand"
                      (and (= 1 (context-test-count
                                 "When tools are available" twice))
                           (= 1 (context-test-count
                                 "When lisp-eval is available" twice)))))

(let* ((event-calls 0)
      (*context-projection-memory-search-fn* (lambda (query k) (declare (ignore query k)) nil))
      (*context-projection-events-fn* (lambda (from to)
                                        (declare (ignore from to))
                                        (incf event-calls) nil)))
  (let* ((projection (build-context-projection "ordinary hello" :now 3994416000))
         (rendered (render-context-projection projection)))
    (context-test-check "ordinary turn performs no event audit" (zerop event-calls))
    (context-test-check "ordinary turn distinguishes audit not requested"
                        (and (string= "not-requested"
                                      (gethash "audit_status" projection))
                             (search "Not requested" rendered)))))

(let* ((*context-projection-memory-search-fn*
         (lambda (query k) (declare (ignore query k)) nil))
       (*context-projection-events-fn*
         (lambda (from to)
           (declare (ignore from to))
           nil))
       (projection (build-context-projection
                    "What happened while I was away?" :now 3994416000))
       (rendered (render-context-projection projection)))
  (context-test-check "empty temporal audit is a successful result"
                      (and (string= "complete" (gethash "audit_status" projection))
                           (null (gethash "audit_user_boundary_found" projection))
                           (search "no configured background-activity events" rendered)
                           (search "Do not search logs" rendered))))

(let* ((*context-projection-memory-search-fn*
         (lambda (query k) (declare (ignore query k)) nil))
       (*context-projection-events-fn*
         (lambda (from to)
           (declare (ignore from to))
           (error "fixture audit failure")))
       (projection (build-context-projection
                    "What happened while I was away?" :now 3994416000))
       (rendered (render-context-projection projection)))
  (context-test-check "audit failure is not rendered as empty activity"
                      (and (string= "unavailable" (gethash "audit_status" projection))
                           (search "source was unavailable" rendered)
                           (null (search "no configured background-activity events"
                                         rendered)))))

(let* ((saved-history *last-self-mod-history*)
       (audit-calls 0)
       (*context-projection-memory-search-fn*
         (lambda (query k) (declare (ignore query k)) nil))
       (*context-projection-events-fn*
         (lambda (from to)
           (declare (ignore from to))
           (incf audit-calls)
           nil)))
  (unwind-protect
       (progn
         (setf *last-self-mod-history*
               (list (obj "role" "user"
                          "content" "What was on your mind while I was away?")
                     (obj "role" "assistant" "content" "A first answer.")
                     (obj "role" "user"
                          "content" "I am asking what actually occurred.")
                     (obj "role" "assistant" "content" "A second answer.")))
         (let ((followup (build-context-projection
                          "You still have not answered the question."
                          :now 3994416000)))
           (context-test-check "corrective follow-up inherits temporal intent"
                               (gethash "temporal_query" followup))
           (context-test-check "second corrective follow-up has depth two"
                               (= 2 (gethash "correction_depth" followup))))
         (let ((unrelated (build-context-projection
                           "What should we build next?" :now 3994416000)))
           (context-test-check "unrelated turn does not inherit temporal intent"
                               (not (gethash "temporal_query" unrelated)))
           (context-test-check "unrelated turn has zero correction depth"
                               (zerop (gethash "correction_depth" unrelated))))
         (context-test-check "only inherited temporal follow-up audits events"
                             (= 1 audit-calls)))
    (setf *last-self-mod-history* saved-history)))

(let ((saved-history *last-self-mod-history*))
  (unwind-protect
       (progn
         (setf *last-self-mod-history*
               (list (obj "role" "user"
                          "content" "What happened while I was away?")
                     (obj "role" "assistant" "content" "A first answer.")))
         (context-test-check
          "first correction has depth one before current prompt is appended"
          (= 1 (%context-projection-temporal-correction-depth
                "I am asking what actually occurred.")))
         (setf *last-self-mod-history*
               (append *last-self-mod-history*
                       (list (obj "role" "user"
                                  "content"
                                  "I am asking what actually occurred."))))
         (context-test-check
          "first correction has depth one after current prompt is appended"
          (= 1 (%context-projection-temporal-correction-depth
                "I am asking what actually occurred.")))
         (setf *last-self-mod-history*
               (list (obj "role" "user"
                          "content" "What happened while I was away?")
                     (obj "role" "assistant" "content" "A first answer.")
                     (obj "role" "user"
                          "content" "Give me the direct answer.")
                     (obj "role" "assistant" "content" "A second answer.")))
         (context-test-check
          "identical repeated correction is retained before append"
          (= 2 (%context-projection-temporal-correction-depth
                "Give me the direct answer.")))
         (setf *last-self-mod-history*
               (list (obj "role" "user"
                          "content" "What happened while I was away?")
                     (obj "role" "assistant" "content" "A first answer.")
                     (obj "role" "user"
                          "content" "What should we build next?")
                     (obj "role" "assistant" "content" "A project answer.")))
         (context-test-check
          "an intervening user topic terminates temporal correction carryover"
          (null (%context-projection-temporal-correction-depth
                 "You still have not answered the question.")))
         (setf *last-self-mod-history*
               (list
                (obj "role" "user"
                     "content"
                     "Investigate the logs and tell me what happened while I was away.")
                (obj "role" "assistant"
                     "content" "The observed tool results establish one event.")))
         (context-test-check
          "forensic follow-up does not downgrade to passive audit authority"
          (null (%context-projection-temporal-correction-depth
                 "Give me the direct answer."))))
    (setf *last-self-mod-history* saved-history)))

(context-test-check
 "declarative dated clarification is not a temporal audit request"
 (not (%context-projection-temporal-p
       "To be clear, we had wrong-dated tickets yesterday, watched No Way Home last night, and saw Brand New Day today.")))
(context-test-check
 "explicit last-night question remains a temporal audit request"
 (%context-projection-temporal-p "What did you do last night?"))
(context-test-check
 "short temporal question remains a temporal audit request"
 (%context-projection-temporal-p "Anything happen overnight?"))

(let* ((*event-ring*
         ;; Production ring order is newest first.
         (list (obj "id" 3 "type" "tick-terminal"
                    "timestamp" "2026-07-30T03:00:00Z" "payload" (obj))
               (obj "id" 2 "type" "user-message"
                    "timestamp" "2026-07-30T02:00:00Z" "payload" (obj))
               (obj "id" 1 "type" "tick-terminal"
                    "timestamp" "2026-07-30T01:00:00Z" "payload" (obj))))
       (*context-test-replay-calls* 0)
       (events (%context-projection-event-source 0 5000000000)))
  (context-test-check "event ring fast path avoids durable replay"
                      (zerop *context-test-replay-calls*))
  (context-test-check "event ring fast path restores oldest-first order"
                      (equal '(1 2 3)
                             (mapcar (lambda (event) (gethash "id" event))
                                     events))))

(let ((*event-ring*
        (list (obj "id" 3 "type" "tick-terminal"
                   "timestamp" "2026-07-30T03:00:00Z" "payload" (obj))))
      (*context-test-replay-calls* 0))
  (%context-projection-event-source 0 5000000000)
  (context-test-check "event ring without user boundary falls back to ledger"
                      (= 1 *context-test-replay-calls*)))

(let ((stripped (context-projection-strip-legacy (context-test-system))))
  (context-test-check "strip retains stable persona and tools"
                      (and (search "Stable persona" stripped)
                           (search "<!-- TOOLS:BEGIN -->" stripped)))
  (context-test-check "strip removes all legacy dynamic markers"
                      (every (lambda (pair)
                               (and (null (search (first pair) stripped))
                                    (null (search (second pair) stripped))))
                             *context-projection-dynamic-markers*))
  (context-test-check "strip removes unmarked legacy suffix"
                      (and (null (search "RELATIONSHIP CONTEXT" stripped))
                           (null (search "PAI'S MEMORY" stripped)))))

(let ((normalized
        (%context-projection-normalize-tool-policy
         "Competent and direct. I operate the tools; I don't hesitate to use them. I prefer tools over speculation. ## My tools (I always prefer using them over guessing) ## Self-knowledge tools (call these via lisp-eval, don't guess) I call this when asked what. When asked which of my own changes worked, I call this instead. If context seems missing, my first move is to inspect.")))
  (context-test-check "persisted prompt tool policy becomes conditional"
                      (and (search "When tools are available" normalized)
                           (search "My currently available tools" normalized)
                           (search "when lisp-eval is available" normalized)
                           (search "When lisp-eval is available, I call" normalized)
                           (search "my first move, when lisp-eval is available"
                                   normalized)
                           (null (search "I always prefer" normalized)))))

(context-test-reset-system)
(let ((original (gethash "content" (first *last-self-mod-history*)))
      (*context-projection-mode* :shadow)
      (*context-projection-memory-search-fn* (lambda (query k) (declare (ignore query k)) nil)))
  (setf *context-test-events* nil)
  (setf *context-test-spans* nil)
  (setf *context-test-base-ran-inside-span* nil)
  (let ((*timing-turn-id* "turn-shadow-1"))
    (context-test-check "shadow returns exact underlying reply"
                        (string= "reply:hello" (auto-turn "hello"))))
  (context-test-check "shadow does not mutate system prompt"
                      (string= original (gethash "content" (first *last-self-mod-history*))))
  (context-test-check "shadow emits audit event"
                      (find "context-projection-shadow" *context-test-events*
                            :key #'first :test #'string=))
  (context-test-check "shadow records discrete projection timing"
                      (find "context.projection" *context-test-spans*
                            :key #'first :test #'string=))
  (context-test-check "shadow records discrete publication-contract timing"
                      (find "publication.contract" *context-test-spans*
                            :key #'first :test #'string=))
  (context-test-check "public turn runs outside projection timing span"
                      (not *context-test-base-ran-inside-span*))
  (context-test-check "shadow audit correlates exact turn id"
                      (string= "turn-shadow-1"
                               (gethash "turn_id"
                                        (second
                                         (find "context-projection-shadow"
                                               *context-test-events*
                                               :key #'first :test #'string=))))))

(context-test-reset-system)
(let ((*context-projection-mode* :enforced)
      (*context-projection-memory-search-fn* (lambda (query k) (declare (ignore query k)) nil)))
  (auto-turn "enforced hello")
  (context-test-check "enforced prompt contains one unified block"
                      (= 1 (context-test-count "<!-- PAI-STATE:BEGIN -->"
                                               *context-test-seen-system*)))
  (context-test-check "enforced prompt excludes legacy markers"
                      (every (lambda (pair)
                               (or (string= (first pair) "<!-- PAI-STATE:BEGIN -->")
                                   (null (search (first pair) *context-test-seen-system*))))
                             *context-projection-dynamic-markers*))
  (context-test-check "enforced prompt retains tools"
                      (search "<!-- TOOLS:BEGIN -->" *context-test-seen-system*))
  (let* ((event (find "context-projection-applied" *context-test-events*
                      :key #'first :test #'string=))
         (payload (second event)))
    (context-test-check "enforced event reports exact installed prompt size"
                        (= (length (gethash "content"
                                           (first *last-self-mod-history*)))
                           (gethash "candidate_system_chars" payload))))
  (auto-turn "second enforced hello")
  (context-test-check "repeated enforced refresh is idempotent"
                      (= 1 (context-test-count "<!-- PAI-STATE:BEGIN -->"
                                               *context-test-seen-system*))))

(let ((*context-projection-mode* :enforced)
      (*context-projection-memory-search-fn*
        (lambda (query k) (declare (ignore query k)) nil)))
  (setf *last-self-mod-history*
        (list
         (obj "role" "system"
              "content"
              "STALE LEGACY PERSONA\n\n<!-- CONVERSATION-CONTINUITY:BEGIN -->\nPrior exchange summary.\n<!-- CONVERSATION-CONTINUITY:END -->")
         (obj "role" "user" "content" "prior turn")))
  (auto-turn "next turn")
  (let ((rendered (gethash "content" (first *last-self-mod-history*))))
    (context-test-check "enforced turn does not inherit legacy stable prompt prose"
                        (null (search "STALE LEGACY PERSONA" rendered)))
    (context-test-check "enforced turn rebuilds reviewed identity and voice"
                        (and (search "<!-- PUBLIC-SYSTEM-PROMPT:BEGIN" rendered)
                             (search "I am **ACME Agent**" rendered)
                             (search "Voice and demeanor" rendered)))
    (context-test-check "enforced prompt migration retains continuity data"
                        (and (search "Prior exchange summary." rendered)
                             (= 1 (context-test-count
                                   "<!-- CONVERSATION-CONTINUITY:BEGIN -->"
                                   rendered))))
    (context-test-check "enforced prompt migration retains one system record"
                        (= 1 (count "system" *last-self-mod-history*
                                    :test #'string=
                                    :key (lambda (message)
                                           (gethash "role" message)))))))

(let ((saved-current *public-system-prompt-current*)
      (saved-history *public-system-prompt-history*)
      (saved-conversation *last-self-mod-history*)
      (config #P"/tmp/context-projection-live-prompt-update.json"))
  (unwind-protect
       (let ((*public-system-prompt-config-file* config)
             (*public-system-prompt-current* (%psp-default-current))
             (*public-system-prompt-history* nil)
             (*context-projection-mode* :enforced)
             (*context-projection-memory-search-fn*
               (lambda (query k) (declare (ignore query k)) nil)))
         (when (probe-file config) (delete-file config))
         (setf *last-self-mod-history*
               (list (obj "role" "system" "content" "old prompt")
                     (obj "role" "user" "content" "prior exchange")))
         (public-system-prompt-update "# Identity\n\nNext-turn identity."
                                      "# Voice\n\nNext-turn voice."
                                      :actor "test")
         (auto-turn "same conversation, next inference")
         (let ((rendered (gethash "content" (first *last-self-mod-history*))))
           (context-test-check "admin identity applies on next inference"
                               (search "Next-turn identity." rendered))
           (context-test-check "admin voice applies on next inference"
                               (search "Next-turn voice." rendered))
           (context-test-check "admin prompt change does not require fresh history"
                               (find "prior exchange" *last-self-mod-history*
                                     :test #'string=
                                     :key (lambda (message)
                                            (gethash "content" message ""))))))
    (setf *public-system-prompt-current* saved-current
          *public-system-prompt-history* saved-history
          *last-self-mod-history* saved-conversation)
    (when (probe-file config) (delete-file config))))

(let ((*context-projection-mode* :enforced))
  (context-test-check "enforced disables legacy mutation"
                      (not (context-projection-legacy-mutation-enabled-p))))
(let ((*context-projection-mode* :shadow))
  (context-test-check "shadow retains legacy mutation"
                      (context-projection-legacy-mutation-enabled-p)))
(let ((*context-projection-mode* :legacy))
  (context-test-check "legacy retains legacy mutation"
                      (context-projection-legacy-mutation-enabled-p)))

(let ((saved-history *last-self-mod-history*)
      (saved-system (and (boundp '*self-mod-system*) *self-mod-system*)))
  (unwind-protect
       (let* ((canonical (obj "role" "system" "content" (context-test-system)))
              (*self-mod-system* canonical)
              (*context-projection-mode* :enforced)
              (*context-projection-memory-search-fn*
                (lambda (query k) (declare (ignore query k)) nil)))
         (setf *last-self-mod-history* nil)
         (auto-turn "fresh enforced turn")
         (let ((installed (gethash "content" (first *last-self-mod-history*))))
           (context-test-check "fresh enforced turn seeds a system history"
                               (= 1 (length *last-self-mod-history*)))
           (context-test-check "fresh enforced turn installs projection"
                               (search "<!-- PAI-STATE:BEGIN -->" installed))
           (context-test-check "fresh enforced turn copies canonical seed"
                               (and (not (eq canonical
                                             (first *last-self-mod-history*)))
                                    (null (search "<!-- PAI-STATE:BEGIN -->"
                                                  (gethash "content" canonical)))))))
    (setf *last-self-mod-history* saved-history)
    (when (boundp '*self-mod-system*) (setf *self-mod-system* saved-system))))

(let ((saved-history *last-self-mod-history*)
      (original-tools (and (boundp '*tools*) *tools*)))
  (unwind-protect
       (let ((*context-projection-mode* :enforced)
             (*context-projection-memory-search-fn*
               (lambda (query k) (declare (ignore query k)) nil))
             (*tools* (vector (obj "function" (obj "name" "lisp-eval"))))
             (*self-mod-system*
               (obj "role" "system" "content" (context-test-system))))
         (flet ((reset-history ()
                  (setf *last-self-mod-history*
                        (list (obj "role" "system"
                                   "content" (context-test-system))))))
           (reset-history)
           (auto-turn "What happened while I was away?")
           (context-test-check "routine temporal answer suppresses public tools"
                               (zerop *context-test-seen-tool-count*))
           (context-test-check "routine temporal answer retains MiMo reasoning"
                               (null *context-test-seen-reasoning-override*))
           (context-test-check "routine temporal prompt matches empty tool schema"
                               (search "No tools are available"
                                       (gethash "content"
                                                (first *last-self-mod-history*))))
           (context-test-check "routine temporal projection reaches response policy"
                               (and (hash-table-p
                                     *context-test-seen-response-policy-context*)
                                    (gethash "temporal_query"
                                             *context-test-seen-response-policy-context*)
                                    (gethash "routine_temporal"
                                             *context-test-seen-response-policy-context*)))
           (context-test-check "routine temporal publication contract is available"
                               (and (hash-table-p
                                     *context-test-seen-publication-contract*)
                                    (string= "temporal-report"
                                             (gethash "intent"
                                                      *context-test-seen-publication-contract*))
                                    (string= "unavailable"
                                             (gethash
                                              "tool_policy"
                                              (gethash "facts"
                                                       *context-test-seen-publication-contract*)))))
           (context-test-check "routine temporal prompt carries present-tense relational guidance"
                               (and (search "Conversational shape for this turn"
                                            *context-test-seen-system*)
                                    (search "present-tense perspective"
                                            *context-test-seen-system*)))
           (auto-turn "What should we build next?")
           (context-test-check "ordinary answer retains public tools"
                               (= 1 *context-test-seen-tool-count*))
           (context-test-check "ordinary answer retains MiMo reasoning"
                               (null *context-test-seen-reasoning-override*))
           (context-test-check "ordinary turn restores canonical tool guidance"
                               (and (search "lisp-eval"
                                            (gethash "content"
                                                     (first *last-self-mod-history*)))
                                    (null (search "No tools are available"
                                                  (gethash "content"
                                                           (first *last-self-mod-history*))))))
           (context-test-check "ordinary publication contract is available"
                               (and (hash-table-p
                                     *context-test-seen-publication-contract*)
                                    (string= "conversation"
                                             (gethash "intent"
                                                      *context-test-seen-publication-contract*))
                                    (string= "available"
                                             (gethash
                                              "tool_policy"
                                              (gethash "facts"
                                                       *context-test-seen-publication-contract*)))))
           (reset-history)
           (auto-turn "I am just checking in and easing into the day.")
           (context-test-check "check-in prompt carries grounded self-disclosure guidance"
                               (and (string= "check-in"
                                             (gethash "intent"
                                                      *context-test-seen-publication-contract*))
                                    (search "first-person perspective"
                                            *context-test-seen-system*)
                                    (search "self-disclosure"
                                            *context-test-seen-system*)))
           (context-test-check "social check-in suppresses public tools"
                               (zerop *context-test-seen-tool-count*))
           (context-test-check "social check-in disables MiMo reasoning"
                               (eq :disabled
                                   *context-test-seen-reasoning-override*))
           (let* ((event (find "context-projection-applied"
                               *context-test-events*
                               :key #'first :test #'string=))
                  (payload (second event)))
             (context-test-check
              "check-in audit records disabled public reasoning"
              (gethash "public_reasoning_disabled" payload)))
           (context-test-check "check-in contract records unavailable public tools"
                               (string= "unavailable"
                                        (gethash
                                         "tool_policy"
                                         (gethash "facts"
                                                  *context-test-seen-publication-contract*))))
           (reset-history)
           (auto-turn "Please investigate and check the logs: what happened while I was away?")
           (context-test-check "explicit forensic temporal request retains tools"
                               (= 1 *context-test-seen-tool-count*))
           (context-test-check "explicit forensic request retains MiMo reasoning"
                               (null *context-test-seen-reasoning-override*))
           (context-test-check "explicit forensic request has tool-result authority"
                               (and (string= "forensic-investigation"
                                             (gethash "intent"
                                                      *context-test-seen-publication-contract*))
                                    (string= "available"
                                             (gethash
                                              "tool_policy"
                                              (gethash "facts"
                                                       *context-test-seen-publication-contract*)))))))
    (setf *last-self-mod-history* saved-history)
    (when (boundp '*tools*) (setf *tools* original-tools))))

(let* ((time-was-bound (fboundp 'pai-current-time-context))
       (snapshot-was-bound (fboundp 'pai-scheduler-context-snapshot))
       (consume-was-bound (fboundp 'pai-scheduler-context-consume))
       (old-time (and time-was-bound
                      (fdefinition 'pai-current-time-context)))
       (old-snapshot (and snapshot-was-bound
                          (fdefinition 'pai-scheduler-context-snapshot)))
       (old-consume (and consume-was-bound
                         (fdefinition 'pai-scheduler-context-consume)))
       (consumed nil)
       (record (obj "id" "ctx-1" "schedule_id" "schedule-1"
                    "text" "Switch to deployment review"
                    "fired_at_local" "Friday, July 31 at 7:00 pm EDT"
                    "delivery_status" "context-only")))
  (unwind-protect
      (progn
        (setf (fdefinition 'pai-current-time-context)
              (lambda (&optional now)
                (declare (ignore now))
                "Current local time: 2026-07-31T19:00 EDT.")
              (fdefinition 'pai-scheduler-context-snapshot)
              (lambda (&optional limit)
                (declare (ignore limit))
                (vector record))
              (fdefinition 'pai-scheduler-context-consume)
              (lambda (ids &optional now)
                (declare (ignore now))
                (setf consumed ids)
                (length ids)))
        (let ((*context-projection-mode* :enforced))
          (context-test-reset-system)
          (auto-turn "Continue")
          (context-test-check "enforced context includes concise local clock"
                              (search "Current local time: 2026-07-31T19:00 EDT."
                                      *context-test-seen-system*))
          (context-test-check "clock excludes location and operational instructions"
                              (and (null (search "UTC"
                                                 *context-test-seen-system*))
                                   (null (search "UTC is only"
                                                 *context-test-seen-system*))
                                   (null (search "pai-schedule-once"
                                                 *context-test-seen-system*))))
          (context-test-check "scheduled trigger is rendered as context, not user speech"
                              (and (search "Scheduled context shifts"
                                           *context-test-seen-system*)
                                   (search "not messages the operator just sent"
                                           *context-test-seen-system*)
                                   (search "Switch to deployment review"
                                           *context-test-seen-system*)))
          (context-test-check "successful enforced turn consumes exact context event"
                              (equal consumed '("ctx-1")))))
    (if time-was-bound
        (setf (fdefinition 'pai-current-time-context) old-time)
        (fmakunbound 'pai-current-time-context))
    (if snapshot-was-bound
        (setf (fdefinition 'pai-scheduler-context-snapshot) old-snapshot)
        (fmakunbound 'pai-scheduler-context-snapshot))
    (if consume-was-bound
        (setf (fdefinition 'pai-scheduler-context-consume) old-consume)
        (fmakunbound 'pai-scheduler-context-consume))))

(let ((guard-text "context-projection-legacy-mutation-enabled-p"))
  (dolist (file '("tick-loop.lisp" "modulator.lisp" "spreading-activation.lisp"
                  "drives.lisp" "soul.lisp" "conversational-initiative.lisp"
                  "latent-thoughts.lisp" "conversation-episodic-memory.lisp"))
    (context-test-check
     (format nil "legacy injector guarded: ~a" file)
     (search guard-text
             (uiop:read-file-string (test-source file))))))

(let ((*context-projection-mode* :shadow)
      (*context-projection-memory-search-fn*
        (lambda (query k) (declare (ignore query k)) (error "retrieval down"))))
  (context-test-check "retrieval failure falls back to public reply"
                      (string= "reply:fallback" (auto-turn "fallback"))))

(let ((original-base (fdefinition 'pai-base-auto-turn-context-projection))
      (calls 0)
      (*context-projection-mode* :shadow)
      (*context-projection-memory-search-fn*
        (lambda (query k) (declare (ignore query k)) nil)))
  (unwind-protect
       (progn
         (setf (fdefinition 'pai-base-auto-turn-context-projection)
               (lambda (prompt)
                 (declare (ignore prompt))
                 (incf calls)
                 (error "underlying public turn failed")))
         (handler-case (auto-turn "base-error") (error () nil))
         (context-test-check "public turn error is not retried" (= calls 1)))
    (setf (fdefinition 'pai-base-auto-turn-context-projection) original-base)))

(load (test-source "near-term-workspace.lisp"))
(setf (fdefinition 'near-term-intention-events)
      (lambda (&key now)
        (declare (ignore now))
        (list
         (obj "id" "intent-event-1" "type" "near-term-item-observed" "at" 90
              "payload"
              (obj "item_id" "intent-1" "item_type" "deferred-intention"
                   "source" "conversation" "state" "ready"
                   "summary" "Patent claim direction"
                   "artifact_summary" "One grounded claim direction is ready."
                   "origin_turn_id" "turn-patent"
                   "commitment_receipt_id" "receipt-patent"
                   "response_deadline" 200 "pass_count" 1 "max_passes" 2)))))
(let* ((*near-term-intentions-mode* :enforced)
       (projection (build-context-projection "How is that patent thought going?"
                                             :now 100 :mode :enforced))
       (near-term (coerce (gethash "near_term_context" projection) 'list))
       (rendered (render-context-projection projection)))
  (context-test-check "enforced near-term receipt projects one safe ready result"
                      (and (= 1 (length near-term))
                           (string= "ready" (gethash "state" (first near-term)))))
  (context-test-check "near-term result is labelled as bounded private state"
                      (and (search "Near-term conversational commitments" rendered)
                           (search "receipt-patent" rendered)
                           (search "passes 1/2" rendered))))

;; V7-memory regression: eligibility is applied to an overfetched semantic
;; candidate set, so an older relevant raw turn can refill context after newer
;; but irrelevant rows are rejected. Nothing here recognizes a topic or phrase.
(let ((requested-k nil)
      (rows
        (append
         (loop for index from 1 to 12
               collect (context-test-row
                        (format nil "turn-distractor-~2,'0d" index)
                        "lived-user" "user-report" "grounded"
                        (format nil "Unrelated recent exchange ~d." index)
                        :similarity (- 0.49d0 (* index 0.001d0))))
         (list
          (context-test-row "turn-unsafe-thought" "generated-cognition"
                            "hypothesis" "unclassified"
                            "An unsafe generated memory candidate."
                            :similarity 0.98d0)
          (context-test-row "turn-system-payload" "lived-user"
                            "user-report" "grounded"
                            "# Operational Constitution private prompt text"
                            :similarity 0.97d0)
          (context-test-row "turn-old-bedtime-user-0000" "lived-user"
                            "user-report" "grounded"
                            "the operator likes chatting with the agent before going to sleep."
                            :similarity 0.84d0)
          (context-test-row "turn-current-query-user-0000" "lived-user"
                            "user-report" "grounded"
                            "What do I like to do at night?"
                            :similarity 0.99d0)))))
  (let* ((*context-projection-memory-search-fn*
           (lambda (query k)
             (declare (ignore query))
             (setf requested-k k)
             rows))
         (projection (build-context-projection
                      "What do I like to do at night?" :now 3994416000))
         (shared (coerce (gethash "relevant_shared_memory" projection) 'list))
         (retrieval (gethash "memory_retrieval" projection)))
    (context-test-check "memory projection overfetches before filtering"
                        (= *context-projection-memory-candidate-results*
                           requested-k 50))
    (context-test-check "older bedtime evidence refills past top ten"
                        (and (= 1 (length shared))
                             (string= "turn-old-bedtime-user-0000"
                                      (gethash "id" (first shared)))))
    (context-test-check "current query echo is excluded"
                        (= 1 (gethash "echo_filtered_count" retrieval)))
    (context-test-check "unsafe and system rows are filtered before refill"
                        (and (= 1 (gethash "eligibility_filtered_count" retrieval))
                             (= 1 (gethash "sensitive_filtered_count" retrieval))))
    (context-test-check "selection telemetry exposes scores without content"
                        (let* ((scores (gethash "selected_scores" retrieval))
                               (first-score (and (plusp (length scores))
                                                 (aref scores 0))))
                          (and (hash-table-p first-score)
                               (numberp (gethash "similarity" first-score))
                               (numberp (gethash "public_score" first-score))
                               (null (gethash "content" first-score)))))
    (context-test-check "memory projection records zero database writes"
                        (zerop (gethash "database_write_count" retrieval)))))

(let* ((rows
         (append
          (loop for index from 1 to 11
                collect (context-test-row
                         (format nil "turn-appearance-distractor-~d" index)
                         "lived-user" "user-report" "grounded"
                         "A recent exchange unrelated to visual appearance."
                         :similarity 0.40d0))
          (list
           (context-test-row "turn-old-appearance-user-0000" "lived-user"
                             "user-report" "grounded"
                             "the operator described the agent with blonde hair, blue eyes, and glasses."
                             :similarity 0.79d0))))
       (*context-projection-memory-search-fn*
         (lambda (query k) (declare (ignore query k)) rows))
       (projection (build-context-projection
                    "What did we decide you look like?" :now 3994416000))
       (shared (coerce (gethash "relevant_shared_memory" projection) 'list)))
  (context-test-check "older appearance evidence is semantically accessible"
                      (and (= 1 (length shared))
                           (string= "turn-old-appearance-user-0000"
                                    (gethash "id" (first shared))))))

(let* ((rows (list (context-test-row "turn-unrelated" "lived-user"
                                     "user-report" "grounded"
                                     "An unrelated grounded statement."
                                     :similarity 0.31d0)))
       (*context-projection-memory-search-fn*
         (lambda (query k) (declare (ignore query k)) rows))
       (projection (build-context-projection
                    "Hey the agent!" :now 3994416000)))
  (context-test-check "unrelated greeting receives no below-floor memory prose"
                      (zerop (length (gethash "relevant_shared_memory" projection)))))

(defvar *retrieval-embedding-mode* :legacy)
(defvar *context-curator-mode* :off)
(let* ((rows (list (context-test-row
                    "turn-appearance" "lived-user" "user-report" "grounded"
                    "Legacy exact evidence about blonde hair." :similarity 0.90d0)))
       (*context-projection-memory-search-fn*
         (lambda (query k) (declare (ignore query k)) rows))
       (*retrieval-embedding-mode* :enforced)
       (*context-curator-mode* :enforced))
  (setf (fdefinition 'context-curator-consume)
        (lambda (&rest ignored)
          (declare (ignore ignored))
          (obj "schema_version" 1 "status" "selected"
               "compiled_context_block"
               "<!-- CURATOR-CONTEXT:BEGIN -->\nObserver context: the agent is imagined with blonde hair.\n<!-- CURATOR-CONTEXT:END -->")))
  (let* ((projection (build-context-projection
                      "What do you look like?" :now 3994416000 :mode :enforced))
         (rendered (render-context-projection projection)))
    (context-test-check "selected curator context is rendered"
                        (search "Observer context: the agent is imagined" rendered))
    (context-test-check "selected curator replaces legacy shared-memory prose"
                        (null (search "Legacy exact evidence" rendered))))
  (setf (fdefinition 'context-curator-consume)
        (lambda (&rest ignored)
          (declare (ignore ignored))
          (obj "schema_version" 1 "status" "no-extra-context"
               "compiled_context_block"
               "<!-- CURATOR-CONTEXT:BEGIN -->\nNo extra context selected.\n<!-- CURATOR-CONTEXT:END -->")))
  (let ((rendered (render-context-projection
                   (build-context-projection
                    "Hello" :now 3994416000 :mode :enforced))))
    (context-test-check "NO_EXTRA_CONTEXT suppresses irrelevant memory prose"
                        (and (search "selected no additional memory context"
                                     rendered)
                             (null (search "Legacy exact evidence" rendered)))))
  (setf (fdefinition 'context-curator-consume)
        (lambda (&rest ignored)
          (declare (ignore ignored))
          (obj "schema_version" 1 "status" "fallback"
               "reason" "fixture" "compiled_context_block" :null)))
  (let ((rendered (render-context-projection
                   (build-context-projection
                    "What do you look like?" :now 3994416000
                    :mode :enforced))))
    (context-test-check "curator failure preserves qualified legacy fallback"
                        (search "Legacy exact evidence" rendered))))

(let* ((anchor
         (context-test-row "turn-bundle-user" "lived-user" "user-report"
                           "grounded" "the operator previously asked about a remembered fact."
                           :similarity 0.92d0))
       (answer
         (context-test-row "turn-bundle-assistant" "lived-agent-action"
                           "agent-action" "grounded"
                           "The remembered answer is available."
                           :similarity 0.71d0))
       (metadata-user (obj "turn_id" "turn-bundle-fixture"
                           "role" "user" "sequence" 0))
       (metadata-assistant (obj "turn_id" "turn-bundle-fixture"
                                "role" "assistant" "sequence" 1))
       (rows (list anchor))
       (*context-projection-memory-search-fn*
         (lambda (query k) (declare (ignore query k)) rows))
       (*context-projection-turn-neighborhood-fn*
         (lambda (query anchors)
           (declare (ignore query anchors))
           ;; The relational reader may omit an anchor already supplied by
           ;; candidate retrieval; composition must still build the full turn.
           (list (let ((duplicate (obj)))
                   ;; A database reader returns a distinct row object for the
                   ;; same durable ID; the canonical semantic anchor must win.
                   (maphash (lambda (key value)
                              (setf (gethash key duplicate) value))
                            anchor)
                   duplicate)
                 answer)))
       (*retrieval-embedding-mode* :enforced)
       (*context-curator-mode* :enforced)
       (*context-test-curator-candidates* nil)
       (context-test-curator-as-of nil))
  (setf (gethash "epistemic_metadata" anchor) metadata-user
        (gethash "retrieval_embedding" anchor) '(1.0d0 0.0d0)
        (gethash "candidate_sources" anchor) (vector "lexical")
        (gethash "lexical_tier" anchor) 1
        (gethash "lexical_match_count" anchor) 1
        (gethash "lexical_coverage" anchor) 1.0d0
        (gethash "lexical_terms" anchor) (vector "remembered")
        (gethash "epistemic_metadata" answer) metadata-assistant
        (gethash "retrieval_embedding" answer) '(0.9d0 0.1d0))
  (setf (fdefinition 'context-curator-consume)
        (lambda (query candidates &key tools as-of)
          (declare (ignore query tools))
          (setf *context-test-curator-candidates* candidates
                context-test-curator-as-of as-of)
          (obj "schema_version" 1 "status" "no-extra-context"
               "compiled_context_block" :null)))
  (build-context-projection "What is the remembered answer?"
                            :now 3994416000 :mode :enforced)
  (let ((bundle (first *context-test-curator-candidates*)))
    (context-test-check
     "ordinary reply curator receives bounded same-turn evidence bundle"
     (and (= 1 (length *context-test-curator-candidates*))
          (string= "turn-bundle:turn-bundle-fixture"
                   (gethash "id" bundle))
          (equal '("user" "assistant")
                 (coerce (gethash "member_roles" bundle) 'list))
          (= 1 (gethash "lexical_tier" bundle 0))
          (equal '("lexical")
                 (coerce (gethash "candidate_sources" bundle) 'list))
          (= 3994416000 context-test-curator-as-of))))
  (let* ((*context-curator-mode* :off)
         (projection
           (build-context-projection "What is the remembered answer?"
                                     :now 3994416000 :mode :enforced))
         (shared (coerce (gethash "relevant_shared_memory" projection) 'list))
         (retrieval (gethash "memory_retrieval" projection))
         (bundle (first shared)))
    (context-test-check
     "curator-off projection shares the same answer-bearing bundle"
     (and (= 1 (length shared))
          (string= "turn-bundle:turn-bundle-fixture" (gethash "id" bundle))
          (search "The remembered answer is available."
                  (gethash "content" bundle ""))
          (equal '("user" "assistant")
                 (coerce (gethash "member_roles" bundle) 'list))
          (equal '("turn-bundle-user" "turn-bundle-assistant")
                 (coerce (gethash "evidence_node_ids" bundle) 'list))
          (string= "bundled" (gethash "materialization_status" retrieval))
          (= 1 (gethash "materialization_selected_bundle_count" retrieval))
          (zerop (gethash "database_write_count" retrieval)))))
  (let ((*turn-capture-context* (obj "as_of" 3994416123)))
    (setf context-test-curator-as-of nil)
    (build-context-projection "What is the remembered answer?"
                              :mode :enforced)
    (context-test-check
     "default projection and curator share the code-owned turn clock"
     (= 3994416123 context-test-curator-as-of)))
  (let ((*context-projection-turn-neighborhood-fn*
          (lambda (&rest ignored)
            (declare (ignore ignored))
            (error "fixture neighborhood failure"))))
    (build-context-projection "What is the remembered answer?"
                              :now 3994416000 :mode :enforced)
    (context-test-check
     "turn-neighborhood failure preserves exact atomic anchor evidence"
     (let ((fallback (first *context-test-curator-candidates*)))
       (and (string= "turn-bundle-user" (gethash "id" fallback))
            (null (gethash "evidence_node_ids" fallback)))))))

(let* ((anchor
         (context-test-row "safe-anchor" "lived-user" "user-report" "grounded"
                           "the operator asked for a relationship detail."
                           :similarity 0.93d0))
       (answer
         (context-test-row "safe-answer" "lived-agent-action" "agent-action"
                           "grounded" "the agent supplied the grounded detail."
                           :similarity 0.72d0))
       (sensitive
         (context-test-row "unsafe-secret" "lived-agent-action" "agent-action"
                           "grounded" "OPENROUTER_API_KEY must stay private."
                           :similarity 0.70d0))
       (quarantined
         (context-test-row "unsafe-quarantine" "lived-agent-action"
                           "agent-action" "grounded"
                           "A quarantined sibling must not render."
                           :quarantined t :similarity 0.69d0))
       (rejected
         (context-test-row "unsafe-rejected" "lived-agent-action" "rejected"
                           "grounded" "A rejected sibling must not render."
                           :similarity 0.68d0))
       (legacy
         (context-test-row "unsafe-legacy" "legacy-unclassified"
                           "legacy-unclassified" "grounded"
                           "A legacy sibling must not render."
                           :similarity 0.67d0))
       (ungrounded
         (context-test-row "unsafe-ungrounded" "lived-agent-action"
                           "agent-action" "ungrounded"
                           "An ungrounded sibling must not render."
                           :similarity 0.66d0))
       (*context-projection-memory-search-fn*
         (lambda (query k) (declare (ignore query k)) (list anchor)))
       (*context-projection-turn-neighborhood-fn*
         (lambda (query anchors)
           (declare (ignore query anchors))
           (list answer sensitive quarantined rejected legacy ungrounded)))
       (*retrieval-embedding-mode* :enforced)
       (*context-curator-mode* :off))
  (dolist (pair (list (list anchor "user" 0)
                      (list answer "assistant" 1)
                      (list sensitive "assistant" 2)
                      (list quarantined "assistant" 3)
                      (list rejected "assistant" 4)
                      (list legacy "assistant" 5)
                      (list ungrounded "assistant" 6)))
    (setf (gethash "epistemic_metadata" (first pair))
          (obj "turn_id" "safe-turn" "role" (second pair)
               "sequence" (third pair))))
  (setf (gethash "retrieval_embedding" anchor) '(1.0d0 0.0d0))
  (let* ((projection
           (build-context-projection "What relationship detail matters?"
                                     :now 3994416000 :mode :enforced))
         (bundle (elt (gethash "relevant_shared_memory" projection) 0))
         (content (gethash "content" bundle "")))
    (context-test-check
     "bundle defense filters sensitive and quarantined siblings independently"
     (and (search "grounded detail" content :test #'char-equal)
          (null (search "OPENROUTER" content :test #'char-equal))
          (null (search "quarantined sibling" content :test #'char-equal))
          (null (search "rejected sibling" content :test #'char-equal))
          (null (search "legacy sibling" content :test #'char-equal))
          (null (search "ungrounded sibling" content :test #'char-equal))
          (equal '("safe-anchor" "safe-answer")
                 (coerce (gethash "evidence_node_ids" bundle) 'list))))))

(let* ((specs '((cluster-a (1.0d0 0.0d0) 0.94d0)
                (cluster-b (0.999d0 0.001d0) 0.93d0)
                (cluster-c (0.998d0 0.002d0) 0.92d0)
                (distinct (0.0d0 1.0d0) 0.91d0)))
       (anchors nil)
       (answers nil)
       (*context-curator-mode* :off)
       (*retrieval-embedding-mode* :enforced))
  (dolist (spec specs)
    (let* ((name (symbol-name (first spec)))
           (turn-id (string-downcase name))
           (anchor (context-test-row
                    (format nil "~a-question" turn-id) "lived-user"
                    "user-report" "grounded"
                    (format nil "the operator asked historical variant ~a." turn-id)
                    :similarity (third spec)))
           (answer (context-test-row
                    (format nil "~a-answer" turn-id) "lived-agent-action"
                    "agent-action" "grounded"
                    (format nil "the agent answered historical variant ~a." turn-id)
                    :similarity 0.70d0)))
      (setf (gethash "epistemic_metadata" anchor)
            (obj "turn_id" turn-id "role" "user" "sequence" 0)
            (gethash "epistemic_metadata" answer)
            (obj "turn_id" turn-id "role" "assistant" "sequence" 1)
            (gethash "retrieval_embedding" anchor) (second spec))
      (push anchor anchors)
      (push answer answers)))
  (setf anchors (nreverse anchors)
        answers (nreverse answers))
  (let* ((*context-projection-memory-search-fn*
           (lambda (query k) (declare (ignore query k)) anchors))
         (*context-projection-turn-neighborhood-fn*
           (lambda (query supplied)
             (declare (ignore query supplied)) answers))
         (projection
           (build-context-projection "Recall the historical variants."
                                     :now 3994416000 :mode :shadow))
         (retrieval (gethash "memory_retrieval" projection))
         (shared (coerce (gethash "relevant_shared_memory" projection) 'list)))
    (context-test-check
     "context cluster cap keeps one near-duplicate and one distinct bundle"
     (and (= 2 (gethash "evidence_candidate_count" retrieval))
          (= 2 (gethash "materialization_near_duplicate_suppression_count"
                        retrieval))
          (= 2 (length shared))
          (equal '("turn-bundle:cluster-a" "turn-bundle:distinct")
                 (mapcar (lambda (row) (gethash "id" row)) shared))))))

(let* ((neighborhood-calls 0)
      (*retrieval-embedding-mode* :enforced)
      (*context-curator-mode* :off)
      (*context-projection-turn-neighborhood-fn*
        (lambda (&rest ignored)
          (declare (ignore ignored))
          (incf neighborhood-calls)
          nil)))
  (build-context-projection "What time is it?" :now 3994416000
                            :mode :enforced)
  (context-test-check "routine temporal projection makes zero neighborhood calls"
                      (zerop neighborhood-calls)))

(let* ((row (context-test-row "atomic-only" "lived-user" "user-report"
                              "grounded" "A stable atomic fallback fact."
                              :similarity 0.91d0))
       (*context-projection-memory-search-fn*
         (lambda (query k) (declare (ignore query k)) (list row)))
       (*context-projection-turn-neighborhood-fn* nil)
       (*retrieval-embedding-mode* :enforced)
       (*context-curator-mode* :off)
       (event-count (length *context-test-events*))
       (all-zero-write-p t)
       (last-projection nil))
  (dotimes (index 100)
    (setf last-projection
          (build-context-projection "Recall the stable fallback."
                                    :now 3994416000 :mode :enforced))
    (unless (zerop (gethash "database_write_count"
                           (gethash "memory_retrieval" last-projection)))
      (setf all-zero-write-p nil)))
  (let ((retrieval (gethash "memory_retrieval" last-projection)))
    (context-test-check
     "one hundred missing-seam projections preserve atomic zero-write fallback"
     (and all-zero-write-p
          (= event-count (length *context-test-events*))
          (string= "atomic-fallback"
                   (gethash "materialization_status" retrieval))
          (equal '("atomic-only")
                 (coerce (gethash "selected_ids" retrieval) 'list))))))

(let* ((row (context-test-row "legacy-atomic" "lived-user" "user-report"
                              "grounded" "Legacy retains atomic evidence."
                              :similarity 0.91d0))
       (*context-projection-memory-search-fn*
         (lambda (query k) (declare (ignore query k)) (list row)))
       (*retrieval-embedding-mode* :enforced)
       (*context-curator-mode* :off)
       (projection
         (build-context-projection "Recall legacy evidence."
                                   :now 3994416000 :mode :legacy))
       (retrieval (gethash "memory_retrieval" projection)))
  (context-test-check "legacy mode reports not-applicable and retains atomic result"
                      (and (string= "not-applicable"
                                    (gethash "materialization_status" retrieval))
                           (equal '("legacy-atomic")
                                  (coerce (gethash "selected_ids" retrieval)
                                          'list)))))

;; The legacy projection remains capped at three rows, while conversation
;; assembly receives a wider, still-bounded discovery pool for global fusion.
(let* ((rows
         (loop for index from 1 to 6
               collect
               (context-test-row
                (format nil "candidate-pool-~d" index)
                "lived-user" "user-report" "grounded"
                (format nil "The operator reported family detail ~d." index)
                :similarity (- 0.96d0 (* index 0.01d0)))))
       (*context-projection-memory-search-fn*
         (lambda (query k) (declare (ignore query k)) rows))
       (*retrieval-embedding-mode* :legacy)
       (*context-curator-mode* :off)
       (projection
         (build-context-projection "Recall my family details."
                                   :now 3994416000 :mode :legacy))
       (legacy (gethash "relevant_shared_memory" projection))
       (candidates (gethash "relevant_shared_memory_candidates" projection)))
  (context-test-check "legacy shared-memory output retains its three-row cap"
                      (= 3 (length legacy)))
  (context-test-check "conversation discovery pool survives the legacy cap"
                      (= 6 (length candidates))))

(let* ((context-wrapper (fdefinition 'auto-turn))
       (true-base (fdefinition 'pai-base-auto-turn-context-projection))
       (timing-wrapper (lambda (prompt)
                         (funcall 'pai-base-auto-turn-timing prompt)))
       (*timing-installed-wrappers* (make-hash-table :test #'eq)))
  (setf (fdefinition 'pai-base-auto-turn-timing) context-wrapper
        (gethash 'auto-turn *timing-installed-wrappers*) timing-wrapper
        (fdefinition 'auto-turn) timing-wrapper)
  (load (test-source "context-projection.lisp"))
  (context-test-check "reload under timing retains true context base"
                      (eq true-base
                          (fdefinition 'pai-base-auto-turn-context-projection)))
  (let ((*context-projection-mode* :legacy))
    (context-test-check "timing-order reload remains callable without recursion"
                        (string= "reply:reload" (auto-turn "reload")))))

(format t "~%CONTEXT-PROJECTION TESTS: ~a passed, ~a failed.~%"
        *context-test-passed* *context-test-failed*)
(when (plusp *context-test-failed*) (uiop:quit 1))
