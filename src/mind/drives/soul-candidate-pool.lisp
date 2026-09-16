;;;; soul-candidate-pool.lisp -- P8.7, 2026-07-29.
;;;;
;;;; "Arousal at encoding should nominate, not admit." Nobody knows at
;;;; the time which moments will define them -- formative status is
;;;; conferred by years of returning to something, not intensity in the
;;;; moment. Dissolves the "loudness problem" (an intense-but-shallow
;;;; event permanently occupying a soul.md slot) without a content rule.
;;;;
;;;; Nomination: any memory node written with arousal_at_encoding above
;;;; *CANDIDATE-AROUSAL-THRESHOLD* enters the pool -- wraps
;;;; MEMORY-WRITE-NODE (rename-and-fall-through), the one place every
;;;; kind of node passes through regardless of source.
;;;;
;;;; Retrospective evidence, concretely: "the node keeps being retrieved
;;;; in self-relevant contexts, and its retrieval has coincided with
;;;; changes to the self-model" -- rather than inventing a separate
;;;; general-purpose retrieval tracker, this wraps
;;;; SELF-MODEL-PROPOSE-REVISION directly: whenever a candidate's node
;;;; id appears in a NEW self-model entry's evidence-node-ids, that IS
;;;; "retrieval coincided with a self-model change," exactly and
;;;; precisely, with no approximation needed.
;;;;
;;;; Promotion runs on the MAINTENANCE tick (already exists, already
;;;; periodic) rather than a literal calendar month or a dedicated "self-
;;;; revision tick" (no such single tick type exists -- self-model
;;;; revision happens via the explore tick and P5.2's resolve-prediction
;;;; hook, not one ticker) -- event-driven-on-the-existing-schedule,
;;;; gated by *CANDIDATE-MIN-AGE-SECONDS* so promotion still requires
;;;; real elapsed time to have passed, matching "conferred by years of
;;;; returning to something" in spirit. A candidate promotes once it's
;;;; old enough AND has accumulated enough coincidence; ages out of the
;;;; pool (untouched as a memory node -- only removed from CANDIDACY) if
;;;; it gets too old without ever accumulating enough.
;;;;
;;;; Load live (no restart) via lisp-eval or repl-drop, after
;;;; memory-nodes.lisp (MEMORY-WRITE-NODE), self-model.lisp
;;;; (SELF-MODEL-PROPOSE-REVISION), soul.lisp (SOUL-ADD-ENTRY),
;;;; tick-loop.lisp (%TICK-HANDLE-MAINTENANCE, CONTINUITY-BUFFER-APPEND):
;;;;   (load "/agent/state/soul-candidate-pool.lisp")

(in-package :agent)

(export '(candidate-pool-report))

(defparameter *candidate-arousal-threshold* 0.7)
(defparameter *candidate-promotion-coincidence-threshold* 2
  "How many separate self-model revisions must cite a candidate as
evidence before it's promoted to soul.md.")
(defparameter *candidate-min-age-seconds* (* 7 86400)
  "Minimum time in the pool before promotion is even considered, no
matter how fast coincidence accumulates -- formative status isn't
conferred instantly.")
(defparameter *candidate-max-age-seconds* (* 60 86400)
  "Candidates that haven't accumulated enough coincidence by this age
are removed from the pool -- they age out on their own, per the
deliverable's own words.")

(defvar *candidate-pool* nil
  "List of {node-id, content, first-seen-at, coincidence-count,
coinciding-entry-ids}.")
(defparameter *candidate-pool-file* #P"/agent/state/candidate-pool.json")

;;; --- nomination: wrap MEMORY-WRITE-NODE, rename-and-fall-through --------

(defun %candidate-find (node-id)
  (find node-id *candidate-pool* :key (lambda (c) (gethash "node-id" c)) :test #'equal))

(defun %candidate-nominate (node-id content)
  (unless (%candidate-find node-id)
    (push (obj "node-id" node-id "content" (or content "") "first-seen-at" (get-universal-time)
               "coincidence-count" 0 "coinciding-entry-ids" (vector))
          *candidate-pool*)
    (ignore-errors (save-candidate-pool))
    (when (fboundp 'log-event)
      (ignore-errors (funcall 'log-event "candidate-nominated" (obj "node-id" node-id))))))

(register-layer memory-write-node candidate-pool-nomination :order 200
  ;; Inner of the two MEMORY-WRITE-NODE layers: nomination should see
  ;; whatever id REFLECTION-NOVELTY (outer, :order 100) ultimately let
  ;; through, matching the original rename-and-fall-through order.
  :function (lambda (next &rest args &key kind content (arousal 0.3) &allow-other-keys)
    (declare (ignore kind))
    (let ((id (apply next args)))
      (when (and (numberp arousal) (>= arousal *candidate-arousal-threshold*))
        (ignore-errors (%candidate-nominate id content)))
      id)))

;;; --- retrospective evidence: wrap SELF-MODEL-PROPOSE-REVISION ------------

(unless (fboundp 'pai-base-self-model-propose-revision-candidates)
  (setf (fdefinition 'pai-base-self-model-propose-revision-candidates) (fdefinition 'self-model-propose-revision)))
(defun self-model-propose-revision (section statement evidence-node-ids)
  (multiple-value-bind (entry reason)
      (funcall 'pai-base-self-model-propose-revision-candidates section statement evidence-node-ids)
    (when entry
      (dolist (nid (coerce evidence-node-ids 'list))
        (let ((cand (%candidate-find nid)))
          (when cand
            (incf (gethash "coincidence-count" cand))
            (setf (gethash "coinciding-entry-ids" cand)
                  (concatenate 'vector (gethash "coinciding-entry-ids" cand) (vector (gethash "id" entry))))
            (when (fboundp 'log-event)
              (ignore-errors
               (funcall 'log-event "candidate-coincidence"
                        (obj "node-id" nid "coincidence-count" (gethash "coincidence-count" cand))))))))
      (ignore-errors (save-candidate-pool)))
    (values entry reason)))

;;; --- promotion / aging-out, run from the maintenance tick ---------------

(defun %candidate-promote (candidate)
  (handler-case
      (let* ((resp (raw-call-model
                    (list (obj "role" "system" "content"
                               "Given this memory -- something that keeps genuinely mattering to real self-understanding over time, not just intense when it happened -- write ONE short first-person disposition statement for an identity document: 'I am the kind of agent that...'. Under 30 words.")
                          (obj "role" "user" "content" (gethash "content" candidate)))))
             (statement (gethash "content" (ref resp "choices" 0 "message"))))
        (when (and (stringp statement) (plusp (length statement)))
          (let ((evidence (cons (gethash "node-id" candidate) (coerce (gethash "coinciding-entry-ids" candidate) 'list))))
            (multiple-value-bind (soul-entry reason) (soul-add-entry statement (coerce (remove-duplicates evidence :test #'equal) 'vector))
              (declare (ignore reason))
              (when soul-entry
                (ignore-errors (continuity-buffer-append (format nil "Something that kept mattering became part of who I am: ~a" statement)))
                (when (fboundp 'log-event)
                  (ignore-errors
                   (funcall 'log-event "candidate-promoted"
                            (obj "node-id" (gethash "node-id" candidate) "statement" statement)))))
              (and soul-entry t)))))
    (error (e) (format t "~&[soul-candidate-pool] promotion failed: ~a~%" e) nil)))

(defun %candidate-maybe-resolve (candidate)
  "Returns :promoted, :aged-out, or :pending -- caller decides what to
keep in the pool."
  (let ((age (- (get-universal-time) (gethash "first-seen-at" candidate))))
    (cond
      ((and (>= age *candidate-min-age-seconds*)
            (>= (gethash "coincidence-count" candidate) *candidate-promotion-coincidence-threshold*))
       (if (%candidate-promote candidate) :promoted :pending))
      ((>= age *candidate-max-age-seconds*)
       (when (fboundp 'log-event)
         (ignore-errors (funcall 'log-event "candidate-aged-out" (obj "node-id" (gethash "node-id" candidate)))))
       :aged-out)
      (t :pending))))

(defun %maybe-process-candidate-pool ()
  (setf *candidate-pool*
        (remove-if (lambda (c) (not (eq (%candidate-maybe-resolve c) :pending))) *candidate-pool*))
  (ignore-errors (save-candidate-pool)))

(defun candidate-pool-report ()
  "Plain summary of the current pool, for introspection."
  (mapcar (lambda (c) (obj "node-id" (gethash "node-id" c)
                           "age-days" (round (/ (- (get-universal-time) (gethash "first-seen-at" c)) 86400))
                           "coincidence-count" (gethash "coincidence-count" c)))
          *candidate-pool*))

;;; --- wrap %TICK-HANDLE-MAINTENANCE, rename-and-fall-through --------------

(unless (fboundp 'pai-base-tick-handle-maintenance-candidates)
  (setf (fdefinition 'pai-base-tick-handle-maintenance-candidates) (fdefinition '%tick-handle-maintenance)))
(defun %tick-handle-maintenance ()
  (funcall 'pai-base-tick-handle-maintenance-candidates)
  (ignore-errors (%maybe-process-candidate-pool)))

;;; --- persistence ----------------------------------------------------------

(defun save-candidate-pool ()
  (let ((tmp (make-pathname :name "candidate-pool-tmp" :type "json" :defaults *candidate-pool-file*)))
    (with-open-file (out tmp :direction :output :if-exists :supersede
                         :if-does-not-exist :create :external-format :utf-8)
      (let ((*print-pretty* nil)) (shasht:write-json (coerce *candidate-pool* 'vector) out)))
    (rename-file tmp *candidate-pool-file*)))

(defun load-candidate-pool ()
  (handler-case
      (when (probe-file *candidate-pool-file*)
        (with-open-file (in *candidate-pool-file*)
          (setf *candidate-pool* (coerce (shasht:read-json in) 'list))))
    (error (e) (format t "~&[soul-candidate-pool] load failed, starting empty: ~a~%" e) nil)))

(define-init :restore soul-candidate-pool-restore
    "Restore durable state for soul-candidate-pool."
  (load-candidate-pool))
