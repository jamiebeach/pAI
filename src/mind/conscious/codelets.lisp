;;;; codelets.lisp -- the attention codelet registry and assessment type.
;;;;
;;;; Workstream Q, slice Q1a. Split out of attention.lisp so it loads BEFORE
;;;; context.lisp, which snapshots the registry when it builds a projection
;;;; context.
;;;;
;;;; The split is structural, not cosmetic. Leaving the registry in
;;;; attention.lisp meant context.lisp -- loaded earlier -- referenced
;;;; CODELET-NAMES, which happened to work only because the reference was
;;;; evaluated at call time rather than load time. That is the same
;;;; accidental-ordering pattern the substrate has been paying down all along
;;;; (see docs/gotchas.md 1). A registry that something else must snapshot is
;;;; a dependency of that thing and belongs beneath it.
;;;;
;;;; Registration is data and is inspectable at runtime, so "what can wake
;;;; this agent" is answered by lookup rather than by reading source -- the
;;;; same property the stimulus admission table provides one stage earlier.

(in-package :agent)

(export '(register-codelet unregister-codelet codelet-names codelet-report
          make-assessment *attention-schema-version*
          *attention-priority-classes*))

(defparameter *attention-schema-version* 1)

;;; --- priority classes ----------------------------------------------------
;;;
;;; Ordered, lowest number wins. These are the only comparison currency in
;;; selection. Adding a class is a design decision with a fixture, not a
;;; tuning knob.

(defparameter *attention-priority-classes*
  '(("critical"  . 0)   ; cancellation, operator control, runtime anomaly
    ("direct"    . 1)   ; a person addressed the agent and is waiting
    ("committed" . 2)   ; a result the agent is awaiting, or a promise it made
    ("relevant"  . 3)   ; bears on an active goal, or contradicts something held
    ("ambient"   . 4))  ; novelty, social information, background change
  "Alist of class name -> rank. Lower ranks are selected first, absolutely:
no quantity of ambient candidates outranks one direct address.")

(defun %attention-class-rank (class &optional (classes *attention-priority-classes*))
  (or (cdr (assoc class classes :test #'string=)) 99))

;;; --- codelet registry ----------------------------------------------------

(defvar *attention-codelets* (make-hash-table :test #'equal)
  "Codelet name -> (order . function). Registration is data and is
inspectable at runtime, so 'what can wake this agent' is answered by lookup
rather than by reading source -- the same property the stimulus admission
table provides one stage earlier.")

(defun register-codelet (name order function &key digest)
  "Register FUNCTION under NAME with a content DIGEST.

DIGEST identifies the IMPLEMENTATION, not the slot. Without it, replacing a
function under the same name and order produced an identical composition hash
-- so a replay could run different code and report that it had reproduced the
original. That was demonstrated, not hypothesised.

Common Lisp cannot hash a closure's behaviour, so the digest is a DECLARATION
the caller makes, in the same spirit as the wrap-chain registry. What the
registry CAN enforce, and does: re-registering the same name and digest with a
different function object signals an error. So within an image, a silent
implementation swap is impossible -- you must either change the digest or be
registering the same function.

FUNCTION takes (STIMULUS CONTEXT) and returns an assessment or NIL."
  (unless (and (stringp digest) (plusp (length digest)))
    (error "Codelet ~a registered without a digest. A digest identifies the ~
            IMPLEMENTATION; without one a pinned composition cannot detect ~
            that it is running different code, and defaulting to `undeclared` ~
            made every unlabelled codelet indistinguishable across restarts." name))
  (let ((existing (gethash name *attention-codelets*)))
    (when (and existing
               (equal (third existing) digest)
               (not (eq (second existing) function)))
      (error "Codelet ~a re-registered with digest ~s but a different ~
              function object. Change the digest when the implementation ~
              changes, or a pinned composition will claim to reproduce code ~
              it is not running." name digest))
    (setf (gethash name *attention-codelets*) (list order function digest)))
  name)

(defun unregister-codelet (name)
  (remhash name *attention-codelets*)
  name)

(defun codelet-names ()
  "Registered codelet names in declared execution order."
  (let ((entries '()))
    (maphash (lambda (name entry) (push (cons name (first entry)) entries))
             *attention-codelets*)
    (mapcar #'car (sort entries (lambda (a b)
                                  (if (= (cdr a) (cdr b))
                                      (string< (car a) (car b))
                                      (< (cdr a) (cdr b))))))))

(defun codelet-report ()
  (obj "schema_version" *attention-schema-version*
       "count" (hash-table-count *attention-codelets*)
       "names" (coerce (codelet-names) 'vector)
       "digests" (coerce (mapcar (lambda (n) (third (gethash n *attention-codelets*)))
                                 (codelet-names))
                         'vector)
       "priority_classes"
       (coerce (mapcar (lambda (c) (obj "class" (car c) "rank" (cdr c)))
                       *attention-priority-classes*)
               'vector)))

;;; --- assessment ----------------------------------------------------------

(defun make-assessment (&key codelet concern stimulus-id evidence-ids
                             priority-class urgency (deadline :null)
                             (confidence 1.0) (coalition-key :null)
                             explanation-code transitions)
  "A codelet's typed verdict. EXPLANATION-CODE is required and is what makes
a selection renderable as a sentence rather than a number."
  (obj "schema_version" *attention-schema-version*
       "codelet" codelet
       "concern" concern
       "stimulus_id" stimulus-id
       "evidence_ids" (coerce (or evidence-ids '()) 'vector)
       "priority_class" priority-class
       "priority_rank" (%attention-class-rank priority-class)
       "urgency" urgency
       "deadline" deadline
       "confidence" confidence
       "coalition_key" coalition-key
       "explanation_code" explanation-code
       ;; Inert. A codelet proposing a change to its own insistence/fatigue
       ;; state says so here; nothing in attention applies it. Materializing
       ;; a transition as an event is a later stage's decision, which is what
       ;; keeps cross-pulse state event-derived instead of accumulating
       ;; inside a codelet where no rebuild could reconstruct it.
       "transitions" (coerce (or transitions '()) 'vector)))

(defparameter *attention-explanation-max* 120
  "Explanation codes reach the operator-facing report. Bounding the length
stops a codelet using the field as an exfiltration channel for context it
was given for assessment.")

(defun %attention-code-shaped-p (string)
  "True when STRING looks like an explanation CODE rather than prose.

Bounding the length alone was not enough. The field reaches the
operator-facing report, so 120 characters of free text is still a channel
for a codelet to copy out context it was handed for assessment. Requiring
code shape -- lowercase, digits, hyphens, no whitespace -- keeps the field
greppable and diffable while removing its capacity to carry a sentence.

A codelet with something to say should say it in a declared code and let the
renderer expand it, which is the same separation the rest of this subsystem
draws between structured state and rendered prose."
  (and (stringp string)
       (plusp (length string))
       (every (lambda (c)
                (or (char<= #\a c #\z) (char<= #\0 c #\9) (char= c #\-)))
              string)))

(defun %attention-normalize-assessment (assessment &key codelet-name stimulus
                                                        (priority-classes
                                                         *attention-priority-classes*)
                                                        (explanation-max
                                                         *attention-explanation-max*))
  "Validate and re-derive a codelet's return value. Returns NIL to discard.

A codelet is untrusted input to this stage. Nothing it returns that could
grant it authority is taken on trust:

  priority_class   must be one of the declared classes, else discarded
  priority_rank    always RECOMPUTED from the validated class, never read
  stimulus_id      forced to the stimulus actually assessed, so a codelet
                   cannot attribute its verdict to a different stimulus
  codelet          forced to the registered name
  evidence_ids     intersected with the stimulus's own roots plus its id,
                   so evidence cannot be fabricated
  explanation_code bounded, since it reaches the safe report

The earlier version accepted any hash table a registered function returned,
which meant the 'closed' priority-class policy was closed only by
convention: a codelet could simply return rank 0 and outrank everything."
  (unless (hash-table-p assessment) (return-from %attention-normalize-assessment nil))
  (let* ((class (gethash "priority_class" assessment))
         ;; The PINNED class table, not the live global. Reading the global
         ;; here meant a projection under a pinned context validated and
         ;; ranked against whatever the process happened to hold -- so
         ;; mutating the live table changed focus from `direct` to `ambient`
         ;; under an unchanged composition hash. Third round of the same
         ;; defect: policy captured at the entry point, consumed from a global
         ;; deeper in.
         (declared (and (stringp class)
                        (assoc class priority-classes :test #'string=))))
    (unless declared (return-from %attention-normalize-assessment nil))
    (let* ((sid (gethash "stimulus_id" stimulus))
           (roots (coerce (or (gethash "source_event_ids" stimulus) (vector)) 'list))
           (permitted (cons sid (mapcar #'princ-to-string roots)))
           (claimed (coerce (or (gethash "evidence_ids" assessment) (vector)) 'list))
           (evidence (remove-if-not
                      (lambda (e) (member (princ-to-string e) permitted
                                          :test #'string= :key #'princ-to-string))
                      claimed))
           (explanation (let ((e (gethash "explanation_code" assessment)))
                          (if (and (stringp e)
                                   (<= (length e) explanation-max)
                                   (%attention-code-shaped-p e))
                              e
                              "unspecified")))
           (key (gethash "coalition_key" assessment))
           (deadline (gethash "deadline" assessment :null)))
      (obj "schema_version" *attention-schema-version*
           "codelet" codelet-name
           "concern" (let ((c (gethash "concern" assessment)))
                       (if (stringp c) (subseq c 0 (min (length c) explanation-max)) ""))
           "stimulus_id" sid
           "evidence_ids" (coerce (or evidence (list sid)) 'vector)
           "priority_class" class
           "priority_rank" (cdr declared)
           ;; The envelope field is URGENCY_CLASS. Reading "urgency" returned
           ;; NIL for every stimulus, so every normalized assessment reported
           ;; "background" regardless -- silently disabling the urgency-class
           ;; tie-break, which could then never separate two coalitions.
           "urgency" (or (gethash "urgency_class" stimulus) "background")
           "deadline" (if (numberp deadline) deadline :null)
           "confidence" (let ((c (gethash "confidence" assessment)))
                          (if (and (realp c) (<= 0 c 1)) c 1.0))
           ;; A codelet-proposed key must name an identity the STIMULUS
           ;; actually has. An arbitrary string is not proof of shared causal,
           ;; task or lifecycle identity -- accepting one would let a codelet
           ;; merge unrelated stimuli into one coalition, which is the
           ;; merge-on-weak-identity defect with a different author.
           "coalition_key"
           (let ((correlation (gethash "correlation_id" stimulus)))
             (if (and (stringp key) (stringp correlation) (string= key correlation))
                 key
                 :null))
           "explanation_code" explanation
           ;; Transitions pass through but are re-keyed to the concern the
           ;; assessed stimulus actually belongs to, so a codelet cannot
           ;; propose a change to some other concern's fatigue state.
           "transitions"
           (let ((identity (concern-identity stimulus)))
             (coerce
              (loop for tr across (or (gethash "transitions" assessment) (vector))
                    when (hash-table-p tr)
                      collect (obj "schema_version" (gethash "schema_version" tr 1)
                                   "concern_identity" identity
                                   "transition" (gethash "transition" tr)
                                   "reason" (gethash "reason" tr)
                                   "cooldown_until" (gethash "cooldown_until" tr :null)
                                   "materialized" nil))
              'vector))))))
