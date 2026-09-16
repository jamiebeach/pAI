;;;; context.lisp -- the immutable projection context.
;;;;
;;;; Workstream Q, slice Q1a. Added after review found that the "pure"
;;;; conscious-state projection in fact read five mutable globals -- the
;;;; codelet registry, the stimulus kind map, the bounds parameters,
;;;; *AGENT-ID*, and a lifecycle port that it CALLED from inside the
;;;; projection.
;;;;
;;;; WHY THIS MATTERS MORE THAN TIDINESS
;;;;
;;;; The rebuild guarantee was stated as "same events and same NOW produce the
;;;; same state." That was true only while the globals happened not to move.
;;;; The consequence is specific and serious: replaying yesterday's events
;;;; after a self-modification changed a codelet produces TODAY's
;;;; interpretation of yesterday, silently. Replay is the primary evidence
;;;; mechanism for this whole workstream (spec section 20.1), so a replay that
;;;; quietly reinterprets is worse than no replay -- it manufactures agreement.
;;;;
;;;; A projection therefore takes everything that can change as an explicit,
;;;; immutable argument. The rule this file enforces: if it can vary between
;;;; two runs, it is in the context, not in a global.
;;;;
;;;; COMPOSITION HASH
;;;;
;;;; The context carries a digest over every policy input that can alter
;;;; interpretation. Two projections are comparable only if their composition
;;;; hashes match. That turns "did the rules change under us?" from an
;;;; assumption into a check -- and it is the field a replay harness compares
;;;; before claiming two runs agree.
;;;;
;;;; HONEST LIMIT
;;;;
;;;; The codelet snapshot captures live function objects, so a context is
;;;; comparable but NOT serializable, and replay cannot yet reconstruct a
;;;; historical codelet set from a revision id alone. That needs the revision
;;;; archive in Q6. Recording the hash now means a future replay can at least
;;;; DETECT that it is not reproducing the original composition, rather than
;;;; silently substituting the current one.

(in-package :agent)

(export '(make-projection-context projection-context-p
          projection-context-hash projection-context-report
          *projection-context-schema-version*
          projection-context-policy))

(defparameter *projection-context-schema-version* 1)

(defun %pc-canonical-composition (codelets kind-map discriminators bounds
                                  priority-classes urgency-ranks tie-breaks
                                  normalization-version explanation-max
                                  runtime-revision motivation-policy)
  "A stable string over EVERY policy input that can change interpretation.

The previous version covered codelet names, kind-map rows and bounds. It
omitted the discriminators, priority classes, urgency ranks, tie-breaks and
normalization version -- and hashed codelets by name and order rather than by
implementation. The consequence was demonstrated rather than theorised:
mutating a live discriminator turned the same event from non-barrier to
barrier under the SAME pinned context, with an unchanged hash.

Sorted throughout: an unsorted digest would change when a hash table rehashed
and report a composition change that never happened."
  (with-output-to-string (out)
    (format out "schema=~a;norm=~a;runtime=~a;"
            *projection-context-schema-version* normalization-version
            (or runtime-revision "unknown"))
    ;; Codelets by NAME, ORDER and DIGEST. The digest is what makes replacing
    ;; an implementation under the same name visible.
    (format out "codelets=")
    (dolist (entry (sort (copy-list codelets)
                         (lambda (a b)
                           (if (= (second a) (second b))
                               (string< (first a) (first b))
                               (< (second a) (second b))))))
      (format out "~a@~a#~a," (first entry) (second entry) (fourth entry)))
    (format out ";kinds=")
    (let ((types '()))
      (maphash (lambda (k v) (push (cons k v) types)) kind-map)
      (dolist (entry (sort types #'string< :key #'car))
        (destructuring-bind (kind source urgency barrier-p) (cdr entry)
          (format out "~a>~a/~a/~a/~a," (car entry) kind source urgency
                  (if barrier-p "b" "-")))))
    ;; Discriminators by name. Their identity comes from the census manifest
    ;; version, which is folded in below -- a changed discriminator body
    ;; requires a manifest change, and the manifest version covers it.
    (format out ";discriminators=")
    (let ((names '()))
      (maphash (lambda (k v) (declare (ignore v)) (push k names)) discriminators)
      (dolist (n (sort names #'string<)) (format out "~a," n)))
    ;; Manifest CONTENT, not just its declared version. A discriminator's
    ;; behaviour lives in the manifest, and hashing the version alone would
    ;; trust an author to bump it.
    ;; Manifest content AND discriminator implementation. The manifest names
    ;; discriminators; their behaviour lives in census.lisp, so hashing only
    ;; the manifest left "what a discriminator does" outside the composition.
    (format out ";census=~a/~a/~a;" (or *census-version* "none")
            (or *census-content-digest* "nodigest")
            (or *census-implementation-digest* "noimpl"))
    (format out "priority=")
    (dolist (c priority-classes) (format out "~a:~a," (car c) (cdr c)))
    (format out ";urgency=")
    (let ((keys '()))
      (maphash (lambda (k v) (declare (ignore v)) (push k keys)) urgency-ranks)
      (dolist (k (sort keys #'string<))
        (format out "~a:~a," k (gethash k urgency-ranks))))
    (format out ";tiebreaks=")
    (dolist (tb tie-breaks) (format out "~a," tb))
    (format out ";explmax=~a" explanation-max)
    (format out ";motivation=")
    (prin1 motivation-policy out)
    (format out ";bounds=")
    (dolist (key (sort (let ((ks '()))
                         (maphash (lambda (k v) (declare (ignore v)) (push k ks)) bounds)
                         ks)
                       #'string<))
      (format out "~a=~a," key (gethash key bounds)))))

(defun %pc-digest (string)
  "FNV-1a 64-bit over the canonical composition string.

Pure Common Lisp on purpose. A cryptographic digest would be a heavier
promise than this field makes: the hash exists to DETECT that a composition
changed, not to resist an adversary forging one, and the composition is
locally derived rather than attacker-supplied. Avoiding the dependency also
keeps this file loadable under the minimal test harness, which does not pull
external systems.

SXHASH is unsuitable -- it is permitted to vary between images, and a hash
that differs across processes would report a composition change on every
restart, which is precisely the false alarm that trains people to ignore the
field."
  (let ((hash 14695981039346656037))          ; FNV offset basis
    (declare (type (unsigned-byte 64) hash))
    (loop for ch across string
          do (setf hash (ldb (byte 64 0)
                             (* (logxor hash (char-code ch))
                                1099511628211))))   ; FNV prime
    (format nil "~(~16,'0x~)" hash)))

(defun make-projection-context
    (&key now agent-id runtime-revision
          (codelets :snapshot)
          (kind-map *stimulus-kind-map*)
          (discriminators *stimulus-discriminators*)
          (priority-classes *attention-priority-classes*)
          (urgency-ranks *inbox-urgency-rank*)
          (tie-breaks *attention-tie-breaks*)
          (normalization-version *policy-normalization-version*)
          (explanation-max *attention-explanation-max*)
          (motivation-policy *curiosity-motivation-policy*)
          (soft-bound *inbox-soft-bound*) (hard-bound *inbox-hard-bound*)
          (secondary-bound *conscious-secondary-bound*)
          (lifecycle (vector)) concern-history context-data consumer)
  "Build an immutable projection context.

Captures EVERY policy that changes interpretation, and hashes all of it. A
projection given this context reads its policy from here and never from the
globals; the globals are defaults for BUILDING a context, not a back channel
around one.

CODELETS defaults to :SNAPSHOT, capturing the registry as it stands, each
entry as (NAME ORDER FUNCTION DIGEST). Pass an explicit list to pin a
composition -- that is what a replay does.

LIFECYCLE and CONCERN-HISTORY are event-derived projections supplied as DATA.
The projection must not invoke arbitrary code from inside itself, and must not
hold counters a rebuild could not reconstruct.

CONTEXT-DATA is an open extension slot, opaque here and passed through
unchanged."
  (let* ((snapshot
           (if (eq codelets :snapshot)
               (mapcar (lambda (name)
                         (let ((entry (gethash name *attention-codelets*)))
                           (list name (first entry) (second entry) (third entry))))
                       (codelet-names))
               codelets))
         (bounds (obj "soft" soft-bound "hard" hard-bound "secondary" secondary-bound))
         (canonical (%pc-canonical-composition
                     snapshot kind-map discriminators bounds priority-classes
                     urgency-ranks tie-breaks normalization-version
                     explanation-max runtime-revision motivation-policy))
         (hash (%pc-digest canonical)))
    (obj "schema_version" *projection-context-schema-version*
         "now" (or now :null)
         "agent_id" (or agent-id :null)
         "runtime_revision" (or runtime-revision :null)
         ;; Which consumer this projection speaks for. Acknowledgements from
         ;; other consumers are ignored, so this identifies WHOSE work the
         ;; inbox is allowed to retire. Not part of the composition hash: two
         ;; consumers running identical policy are comparable, and hashing
         ;; identity would make every consumer look like a different ruleset.
         "consumer" (or consumer :null)
         "composition_hash" hash
         ;; The captured policy. Projection code reads THESE.
         ;; Copied, not shared. See %PC-COPY-TABLE.
         "codelets" (copy-tree snapshot)
         "kind_map" (%pc-copy-table kind-map)
         "discriminators" (%pc-copy-table discriminators)
         "priority_classes" (copy-alist priority-classes)
         "urgency_ranks" (%pc-copy-table urgency-ranks)
         "tie_breaks" (copy-list tie-breaks)
         "explanation_max" explanation-max
         "normalization_version" normalization-version
         "motivation_policy" (copy-tree motivation-policy)
         "bounds" bounds
         ;; Event-derived data, deliberately NOT part of the composition hash:
         ;; two contexts differing only in observed history are still running
         ;; the same rules, and hashing history would make every new event look
         ;; like a policy change.
         "lifecycle" lifecycle
         "concern_history" (or concern-history
                               (obj "concerns" (make-hash-table :test #'equal)
                                    "concern_count" 0
                                    "history_completeness" (obj "observation" "not-supplied")))
         "context_data" (or context-data (obj)))))

(defun projection-context-policy (ctx key &optional default)
  "Read a captured policy from CTX, falling back to DEFAULT when CTX is not a
context. Every projection stage uses this rather than touching a global, so
that running without a context is an explicit default rather than a silent
divergence from the pinned one."
  (if (projection-context-p ctx)
      (gethash key ctx default)
      default))

(defun %pc-copy-table (table)
  "Shallow copy of a policy table.

A context that stored the global hash table itself was not immutable: it held
the same OBJECT, so mutating the global mutated the context. A pinned replay
then reinterpreted events by the new rules while reporting the old hash --
the exact defect the context was introduced to prevent, reproduced one level
down. Copying at capture is what makes `pinned` mean anything."
  (let ((copy (make-hash-table :test (hash-table-test table)
                               :size (max 1 (hash-table-count table)))))
    (maphash (lambda (k v) (setf (gethash k copy) v)) table)
    copy))

(defun projection-context-p (x)
  (and (hash-table-p x) (stringp (gethash "composition_hash" x))))

(defun projection-context-hash (ctx) (gethash "composition_hash" ctx))

(defun projection-context-report (ctx)
  "Content-free. Names the composition without exposing codelet functions or
lifecycle payloads."
  (obj "schema_version" (gethash "schema_version" ctx)
       "composition_hash" (gethash "composition_hash" ctx)
       "runtime_revision" (gethash "runtime_revision" ctx)
       "agent_id" (gethash "agent_id" ctx)
       "codelet_count" (length (gethash "codelets" ctx))
       "codelet_names" (coerce (mapcar #'first (gethash "codelets" ctx)) 'vector)
       "kind_map_size" (hash-table-count (gethash "kind_map" ctx))
       "motivation_policy_version"
       (cdr (assoc :schema-version (gethash "motivation_policy" ctx)))
       "bounds" (gethash "bounds" ctx)
       "lifecycle_count" (length (gethash "lifecycle" ctx))
       "concern_count" (gethash "concern_count" (gethash "concern_history" ctx) 0)
       "history_completeness"
       (gethash "history_completeness" (gethash "concern_history" ctx))))
