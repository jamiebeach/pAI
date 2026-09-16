;;;; attention.lisp -- staged attention: codelets, coalitions, selection.
;;;;
;;;; Workstream Q, slice Q1. Stages B, C and D of the attention model
;;;; (Stage A, deterministic eligibility, lives in inbox.lisp). Same isolation
;;;; as the rest of this subsystem: pure, no model, no tools, no publication,
;;;; no I/O, no clock read that is not passed in, registers nothing.
;;;;
;;;; NO UNIVERSAL SCALAR
;;;;
;;;; The single most important constraint here is that selection must be
;;;; explainable by naming the rule that fired -- not by reporting that
;;;; something scored 0.72. A scalar collapses incommensurable reasons into
;;;; one number and then cannot say why. "A person is waiting" and "this is
;;;; mildly novel" are not two magnitudes of one quantity; treating them as
;;;; such is how an agent ends up ignoring its operator because six weak
;;;; signals summed higher.
;;;;
;;;; So priority is an ORDERED SET OF NAMED CLASSES, compared
;;;; lexicographically, with every tie-break itself a named rule. Any
;;;; selection can be rendered as a sentence. A learned scorer may later
;;;; advise within a class (spec 11.4), but it can never reorder the classes
;;;; or cross an eligibility gate.
;;;;
;;;; WHAT A CODELET MAY NOT DO -- AND WHAT IS ACTUALLY ENFORCED
;;;;
;;;; An earlier version of this comment claimed codelets are pure because "a
;;;; codelet receives immutable inputs and returns an assessment, so there is
;;;; nowhere for a side effect to go." That was false, and stating it was
;;;; worse than saying nothing: a reader would have trusted it. Common Lisp
;;;; hash tables are mutable, and a registered function can perform I/O, mutate
;;;; globals, or write files. Nothing here prevents any of that.
;;;;
;;;; What IS enforced, mechanically:
;;;;
;;;;   - A codelet's OUTPUT cannot grant it authority. Priority rank is
;;;;     recomputed from a validated class, stimulus id and codelet name are
;;;;     forced, evidence is intersected with the stimulus's own roots,
;;;;     coalition keys must match a real correlation, and explanation codes
;;;;     must be code-shaped and bounded. See %ATTENTION-NORMALIZE-ASSESSMENT.
;;;;   - A codelet cannot corrupt the projection's INPUT: it is handed a
;;;;     defensive copy of each stimulus, so mutating what it was given
;;;;     changes nothing downstream.
;;;;   - A codelet that signals is contained and named, so one broken codelet
;;;;     cannot make the agent unable to notice its operator.
;;;;
;;;; What is NOT enforced: freedom from side effects. A codelet can still call
;;;; out, and no arrangement of this file would stop it. That has to come from
;;;; qualification and sandboxing of the code being registered -- Q6's
;;;; territory -- and until then it is a property of the codelets we write,
;;;; not a property of this mechanism.
;;;;
;;;; The distinction matters because attention runs on every wake, including
;;;; under degradation. Knowing exactly which guarantees hold is what lets a
;;;; reader reason about that path; a blanket claim of purity would have them
;;;; reasoning about a system that does not exist.

(in-package :agent)

(export '(attention-assess attention-coalitions attention-select
          attention-decide *attention-tie-breaks*))

(defun %attention-defensive-copy (stimulus)
  "A shallow copy of STIMULUS for handing to a codelet.

Codelets are untrusted code holding a mutable hash table. Without a copy, one
could rewrite the urgency, audience or evidence of the very stimulus the next
codelet is about to assess -- and every later stage would read the altered
version, with no record that anything had changed. Shallow is sufficient
because the projection's own fields are flat; nested payload content is
deliberately not present here (references, not content)."
  (if (hash-table-p stimulus)
      (let ((copy (make-hash-table :test (hash-table-test stimulus)
                                   :size (max 1 (hash-table-count stimulus)))))
        (maphash (lambda (k v) (setf (gethash k copy) v)) stimulus)
        copy)
      stimulus))

(defun attention-assess (stimuli &key context)
  "Stage B. Run every registered codelet over every stimulus.

Returns a vector of assessments. A codelet returning NIL means 'no concern',
which is not an error and is not recorded -- absence of a match is the common
case and logging it would drown the signal.

Codelet errors are contained: a broken codelet yields no assessment and is
named in the returned error list rather than aborting attention. Attention
must degrade, not fail; a single bad codelet cannot make the agent unable to
notice its operator."
  (let ((assessments '()) (errors '())
        ;; Codelets come from the pinned context when one is supplied, so a
        ;; replay runs the composition that was recorded rather than whatever
        ;; the live registry holds now. Falling back to the registry keeps
        ;; direct calls working; the context is what makes replay meaningful.
        (entries (if (projection-context-p context)
                     (mapcar (lambda (e) (cons (first e) (third e)))
                             (gethash "codelets" context))
                     (mapcar (lambda (n) (cons n (second (gethash n *attention-codelets*))))
                             (codelet-names)))))
    (dolist (entry entries)
      (let ((name (car entry))
            (fn (cdr entry)))
        (map nil
             (lambda (s)
               (handler-case
                   ;; The codelet sees a copy; normalization is given the
                   ;; ORIGINAL, so a codelet mutating its copy cannot widen
                   ;; what its assessment is validated against.
                   (let* ((raw (funcall fn (%attention-defensive-copy s) context))
                          (a (%attention-normalize-assessment
                              raw :codelet-name name :stimulus s
                              :priority-classes
                              (projection-context-policy context "priority_classes"
                                                         *attention-priority-classes*)
                              :explanation-max
                              (projection-context-policy context "explanation_max"
                                                         *attention-explanation-max*))))
                     (when a (push a assessments)))
                 (error (c)
                   (pushnew (obj "codelet" name "error" (princ-to-string c))
                            errors
                            :test #'equal
                            :key (lambda (e) (gethash "codelet" e))))))
             stimuli)))
    (values (coerce (nreverse assessments) 'vector)
            (coerce (nreverse errors) 'vector))))

;;; --- coalition formation -------------------------------------------------

(defun %attention-fence-key (stimulus)
  "Hard fences that must match before anything may be grouped. Agent,
audience and grounding are authority-adjacent: merging across them would let
one item's provenance be attributed to another. Grouping is a presentation
convenience and must never launder a claim."
  (format nil "~a|~a|~a"
          (gethash "agent_id" stimulus :null)
          (gethash "audience" stimulus "operator")
          (gethash "grounding" stimulus "unknown")))

(defun attention-coalitions (assessments stimuli-by-id)
  "Stage C. Group assessments that share an explicit identifier.

Grouping is by EXPLICIT causal / task / lifecycle identity only -- a
codelet-proposed coalition key, else the stimulus correlation id. Semantic
similarity is deliberately not implemented here: the spec permits it only
after these hard fences, and a first implementation that guesses would make
the fences untestable.

Every member's provenance is retained. A coalition is a bag of assessments
with a shared key, never a merged claim."
  (let ((groups (make-hash-table :test #'equal))
        (order '()))
    (map nil
         (lambda (a)
           (let* ((sid (gethash "stimulus_id" a))
                  (s (and sid (gethash sid stimuli-by-id)))
                  (explicit (let ((k (gethash "coalition_key" a)))
                              (if (stringp k) k
                                  (let ((c (and s (gethash "correlation_id" s))))
                                    (and (stringp c) c)))))
                  ;; With no explicit identity an assessment is its own
                  ;; coalition, and the solo key must therefore include the
                  ;; STIMULUS id -- not just the codelet name. Keying on the
                  ;; codelet alone grouped every uncorrelated stimulus that
                  ;; the same codelet matched, so three separate user messages
                  ;; became one coalition. That is the third appearance of one
                  ;; defect: a key derived from insufficient identity silently
                  ;; merges unrelated things. See %STIMULUS-COALESCING-KEY for
                  ;; the first two.
                  (key (format nil "~a#~a"
                               (if s (%attention-fence-key s) "nofence")
                               (or explicit
                                   (format nil "solo:~a:~a"
                                           (gethash "codelet" a) sid)))))
             (unless (gethash key groups) (push key order))
             (push a (gethash key groups))))
         assessments)
    (coerce
     (mapcar
      (lambda (key)
        (let* ((members (nreverse (gethash key groups)))
               (best (reduce (lambda (x y)
                               (if (<= (gethash "priority_rank" x)
                                       (gethash "priority_rank" y))
                                   x y))
                             members)))
          (obj "coalition_key" key
               "member_count" (length members)
               "members" (coerce members 'vector)
               ;; Soonest member deadline, or :null. Without this the
               ;; `deadline-soonest` tie-break was dead code: assessments
               ;; carried deadlines, selection read a COALITION deadline, and
               ;; nothing ever put one there. A tie-break that cannot fire is
               ;; worse than an absent one -- it appears in the audit list and
               ;; implies a guarantee the code does not provide.
               "deadline"
               (let ((deadlines (loop for m in members
                                      for d = (gethash "deadline" m)
                                      when (numberp d) collect d)))
                 (if deadlines (reduce #'min deadlines) :null))
               ;; A coalition inherits its strongest member's class. It does
               ;; not accumulate: five ambient members remain ambient. This is
               ;; the no-summing rule applied to grouping.
               "priority_class" (gethash "priority_class" best)
               "priority_rank" (gethash "priority_rank" best)
               "explanation_code" (gethash "explanation_code" best)
               ;; Provenance is DERIVED, not trusted to the codelet. Member
               ;; stimulus ids are always included, so a coalition retains
               ;; every member's provenance even when a codelet supplied no
               ;; evidence of its own. "A coalition retains every member's
               ;; provenance" is a structural guarantee; leaving it to each
               ;; codelet to remember would make it a convention.
               "evidence_ids"
               (coerce (remove-duplicates
                        (append
                         (loop for m in members
                               for sid = (gethash "stimulus_id" m)
                               when sid collect sid)
                         (loop for m in members
                               append (coerce (gethash "evidence_ids" m) 'list)))
                        :test #'equal)
                       'vector))))
      (nreverse order))
     'vector)))

;;; --- selection -----------------------------------------------------------

(defparameter *attention-tie-breaks*
  '("sole-candidate" "priority-class" "urgency-class" "deadline-soonest"
    "oldest-waiting" "coalition-key-lexical")
  "The complete, ordered list of comparisons selection may use. Every
selection record names which of these decided it. A comparison not on this
list cannot influence the outcome, which is what keeps the baseline
auditable.")

(defparameter *attention-urgency-rank*
  (obj "interactive" 0 "timely" 1 "background" 2))

(defun %attention-urgency-of (coalition stimuli-by-id
                              &optional (ranks *attention-urgency-rank*))
  (let* ((members (coerce (gethash "members" coalition) 'list))
         (ranks (loop for m in members
                      for s = (gethash (gethash "stimulus_id" m) stimuli-by-id)
                      when s collect (or (gethash (gethash "urgency_class" s "background")
                                                  ranks) 9))))
    (if ranks (reduce #'min ranks) 9)))

(defun %attention-oldest-observed (coalition stimuli-by-id)
  (let* ((members (coerce (gethash "members" coalition) 'list))
         (times (loop for m in members
                      for s = (gethash (gethash "stimulus_id" m) stimuli-by-id)
                      for o = (and s (gethash "observed_at" s))
                      when (numberp o) collect o)))
    (if times (reduce #'min times) most-positive-fixnum)))

(defun %attention-compare (a b stimuli-by-id
                           &key (urgency-ranks *attention-urgency-rank*)
                                (tie-breaks *attention-tie-breaks*))
  "Compare two coalitions. Returns (values A-BEFORE-B-P DECIDING-RULE).

The rule name is returned rather than accumulated in a closure variable. An
earlier version set the rule from inside the sort predicate, which was
wrong in a way worth recording: a comparison sort calls its predicate many
times, so the variable ended up holding whichever comparison happened to run
last -- not the one that decided the winner. The reported reason was
therefore arbitrary while looking authoritative, which is the worst
combination for an audit field. The deciding rule is now derived by comparing
the winner against the runner-up, once, after ordering."
  ;; Every comparison this may use must be declared in TIE-BREAKS. A rule
  ;; absent from the captured list cannot influence the outcome, which is what
  ;; keeps the baseline auditable -- and which was previously only true by
  ;; the sequence below happening to match the list.
  (flet ((allowed (rule) (member rule tie-breaks :test #'string=)))
  (let ((ra (gethash "priority_rank" a)) (rb (gethash "priority_rank" b)))
    (if (and (/= ra rb) (allowed "priority-class"))
        (values (< ra rb) "priority-class")
        (let ((ua (%attention-urgency-of a stimuli-by-id urgency-ranks))
              (ub (%attention-urgency-of b stimuli-by-id urgency-ranks)))
          (if (and (/= ua ub) (allowed "urgency-class"))
              (values (< ua ub) "urgency-class")
              (let ((da (gethash "deadline" a :null))
                    (db (gethash "deadline" b :null)))
                (if (and (numberp da) (numberp db) (/= da db)
                         (allowed "deadline-soonest"))
                    (values (< da db) "deadline-soonest")
                    (let ((oa (%attention-oldest-observed a stimuli-by-id))
                          (ob (%attention-oldest-observed b stimuli-by-id)))
                      (if (and (/= oa ob) (allowed "oldest-waiting"))
                          (values (< oa ob) "oldest-waiting")
                          (values (string< (gethash "coalition_key" a "")
                                           (gethash "coalition_key" b ""))
                                  "coalition-key-lexical")))))))))))

(defun attention-select (coalitions stimuli-by-id
                         &key (urgency-ranks *attention-urgency-rank*)
                              (tie-breaks *attention-tie-breaks*))
  "Stage D ordering. Returns (values winner reason ordered) or NIL when there
is nothing eligible.

REASON names which tie-break separated the winner from the runner-up, so the
selection is explainable without re-running the comparison."
  (when (plusp (length coalitions))
    (let* ((ordered (stable-sort
                     (coerce coalitions 'list)
                     (lambda (a b)
                       (values (%attention-compare a b stimuli-by-id)))))
           (winner (first ordered))
           (runner-up (second ordered))
           (reason (if runner-up
                       (nth-value 1 (%attention-compare winner runner-up stimuli-by-id
                                                        :urgency-ranks urgency-ranks
                                                        :tie-breaks tie-breaks))
                       "sole-candidate")))
      (values winner reason (coerce ordered 'vector)))))

(defun attention-decide (inbox &key context now pulse-in-flight)
  "Stages B through D over an INBOX projection. Returns a wake decision.

Decisions (spec 11.4) are named outcomes, never a bare boolean:

  pulse-now                  deliberate immediately
  interrupt-at-boundary      something outranks a running pulse; interrupt at
                             its next safe boundary rather than preempting
                             mid-commit
  materialize-only           record state without a model call
  wait-for-more              nothing yet warrants a pulse
  ignore-candidacy           eligible but deliberately not acted on, with a
                             reason and the source event retained

ROUTE-TO-EXISTING-LIFECYCLE is defined in the spec but not emitted here: it
needs lifecycle owners that do not exist until Q5, and emitting a decision
nothing can honour would be worse than not emitting it."
  (let* ((admitted (coerce (gethash "admitted" inbox) 'list))
         (by-id (let ((h (make-hash-table :test #'equal)))
                  (dolist (s admitted h)
                    (setf (gethash (gethash "stimulus_id" s) h) s)))))
    (multiple-value-bind (assessments errors) (attention-assess admitted :context context)
      (let ((coalitions (attention-coalitions assessments by-id)))
        (multiple-value-bind (winner reason ordered)
            (attention-select coalitions by-id
                              :urgency-ranks
                              (projection-context-policy context "urgency_ranks"
                                                         *attention-urgency-rank*)
                              :tie-breaks
                              (projection-context-policy context "tie_breaks"
                                                         *attention-tie-breaks*))
          (let* ((degraded (gethash "degraded" inbox))
                 (decision
                   (cond
                     ;; Degradation is tested FIRST, before the no-winner
                     ;; case. An earlier version had these reversed, so a
                     ;; degraded inbox with no matching codelet reported
                     ;; `wait-for-more` and the degradation went unrecorded --
                     ;; the state that most needs materializing is exactly the
                     ;; one where attention found nothing to say about it. The
                     ;; test passed only because the fixture happened to have
                     ;; a codelet that matched.
                     (degraded "materialize-only")
                     ;; Nothing matched. Not an error -- silence is a valid
                     ;; outcome and the common one.
                     ((null winner) "wait-for-more")
                     ;; Something outranks the running pulse. Interrupt at a
                     ;; boundary; never preempt mid-commit.
                     ((and pulse-in-flight
                           (<= (gethash "priority_rank" winner)
                               (%attention-class-rank
                                "direct"
                                (projection-context-policy
                                 context "priority_classes"
                                 *attention-priority-classes*))))
                      "interrupt-at-boundary")
                     (pulse-in-flight "wait-for-more")
                     (t "pulse-now"))))
            (obj "schema_version" *attention-schema-version*
                 "decision" decision
                 "decided_by" (if winner reason :null)
                 "winner" (or winner :null)
                 "coalition_count" (length coalitions)
                 "assessment_count" (length assessments)
                 "codelet_errors" errors
                 "degraded" (if degraded t nil)
                 "evaluated_at" (or now :null)
                 ;; The ACTUAL coalitions this decision was made from, not a
                 ;; recomputation. A caller that rebuilt them would run every
                 ;; codelet a second time, and any nondeterminism would let
                 ;; the reported winner disagree with the coalitions shown
                 ;; beside it -- a state that contradicts its own selection.
                 "coalitions" (or ordered (vector))
                 "ordered_keys"
                 (coerce (map 'list (lambda (c) (gethash "coalition_key" c))
                              (or ordered #()))
                         'vector))))))))
