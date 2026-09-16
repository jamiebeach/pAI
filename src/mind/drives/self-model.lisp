;;;; self-model.lisp -- P5.1, 2026-07-29.
;;;;
;;;; Distinct from the system prompt (what it was given) and from
;;;; soul.md/P8.6 (a small, hard-capped, always-injected identity anchor
;;;; -- disposition statements only). This is broader: a structured
;;;; record of what it has CONCLUDED about itself, organized into the
;;;; backlog's own six fixed sections (tendencies, values, known-
;;;; weaknesses, current-preoccupations, open-questions, relationships).
;;;; Not verbatim-injected into every turn like soul.md -- that would be
;;;; redundant bloat on top of an already-injected identity layer; this
;;;; is queryable/introspectable and meant to be the substrate P5.2
;;;; (surprise-triggered revision) and P5.3 (attention schema) read from
;;;; and write to, not a second copy of the same "always visible" idea.
;;;;
;;;; Same evidence-grounding discipline as soul.lisp: every entry must
;;;; cite at least one real, existing memory node, validated against
;;;; Postgres at write time -- "no untraceable self-descriptions" is
;;;; this file's own acceptance criterion, not a suggestion.
;;;;
;;;; SCOPE NOTE: the backlog specifies entries should be "editable only
;;;; by a dedicated self-revision tick, never mid-conversation." That
;;;; tick is P5.2, which doesn't exist yet -- deliberately not built in
;;;; this pass (P5.1 is the foundation P5.2 needs to exist first).
;;;; SELF-MODEL-PROPOSE-REVISION is written and documented as the
;;;; function that tick will call, but is not yet hard-restricted to
;;;; only run from a tick context (there's no tick to restrict it to
;;;; yet) -- same staged approach as P8.6/P8.7 (build the document and
;;;; its evidence discipline first, build the more sophisticated
;;;; automatic trigger mechanism as a clearly separate, later piece).
;;;;
;;;; Load live (no restart) via lisp-eval or repl-drop, after
;;;; memory-nodes.lisp (needs MEMORY-GET-NODE to validate evidence):
;;;;   (load "/agent/state/self-model.lisp")

(in-package :agent)

(export '(self-model-entries self-model-report self-model-entry-evidence self-model-propose-revision))

(defparameter *self-model-file* #P"/agent/state/self-model.json")
(defparameter *self-model-sections*
  '("tendencies" "values" "known-weaknesses" "current-preoccupations" "open-questions" "relationships")
  "Fixed, small set of sections, matching the backlog's own list exactly
-- not extensible ad hoc. A self-model with an unbounded, arbitrary set
of categories isn't a model, it's a diary.")

(defvar *self-model*
  (let ((h (obj))) (dolist (s *self-model-sections*) (setf (gethash s h) nil)) h)
  "Hash-table: section name -> list of entry hash-tables {id, statement,
evidence-node-ids, created-at}. Newest-first within each section.")
(defvar *self-model-lock* (bt:make-lock "self-model"))
(defvar *self-model-entry-counter* 0)

(defun %self-model-validate-section (section)
  (member section *self-model-sections* :test #'string=))

(defun %self-model-validate-evidence (evidence-node-ids)
  "Identical discipline to soul.lisp's %SOUL-VALIDATE-EVIDENCE -- every
cited id must actually exist as a real memory node, checked here rather
than trusted to caller discipline."
  (cond
    ((or (null evidence-node-ids) (zerop (length evidence-node-ids)))
     (values nil "must cite at least one memory node id as evidence"))
    ((not (fboundp 'memory-get-node))
     (values nil "cannot validate evidence -- memory-nodes.lisp not loaded"))
    (t (dolist (id (coerce evidence-node-ids 'list))
         (unless (and (stringp id) (ignore-errors (memory-get-node id)))
           (return-from %self-model-validate-evidence
             (values nil (format nil "evidence node ~a does not exist" id)))))
       (values t nil))))

(defun self-model-entries (&optional section)
  "All entries in SECTION, or (if SECTION is NIL) every section's
entries, for introspection."
  (if section
      (copy-list (gethash section *self-model*))
      (let ((out (obj)))
        (dolist (s *self-model-sections*) (setf (gethash s out) (copy-list (gethash s *self-model*))))
        out)))

;;; (removed: dead definition -- superseded downstream)
(defun self-model-entry-evidence (section id)
  "The real memory nodes behind entry ID in SECTION -- so a self-model
claim can be explained from evidence, matching the same acceptance
criterion as soul.lisp."
  (let ((entry (find id (gethash section *self-model*) :key (lambda (e) (gethash "id" e)))))
    (and entry
         (mapcar (lambda (nid)
                   (or (and (fboundp 'memory-get-node) (ignore-errors (memory-get-node nid)))
                       nid))
                 (coerce (gethash "evidence-node-ids" entry) 'list)))))

(defun self-model-propose-revision (section statement evidence-node-ids)
  "Adds a new self-model entry to SECTION. Intended to be called from a
dedicated self-revision tick, not mid-conversation -- see file
header for why that's not yet hard-enforced. Returns (values new-entry
nil) on success, (values nil reason) on rejection. No cap/demotion here
(unlike soul.lisp) -- the backlog doesn't specify scarcity for this
document, only evidence-grounding; sections can grow as understanding
genuinely accumulates."
  (cond
    ((not (%self-model-validate-section section))
     (values nil (format nil "unknown section ~s -- must be one of ~a" section *self-model-sections*)))
    (t
     (multiple-value-bind (ok reason) (%self-model-validate-evidence evidence-node-ids)
       (if (not ok)
           (values nil reason)
           (bt:with-lock-held (*self-model-lock*)
             (incf *self-model-entry-counter*)
             (let ((entry (obj "id" *self-model-entry-counter* "statement" statement
                                "evidence-node-ids" (coerce evidence-node-ids 'vector)
                                "created-at" (get-universal-time))))
               (push entry (gethash section *self-model*))
               (ignore-errors (save-self-model))
               (when (fboundp 'log-event)
                 (ignore-errors
                  (funcall 'log-event "self-model-revised"
                           (obj "section" section "id" (gethash "id" entry) "statement" statement))))
               (values entry nil))))))))

(defun save-self-model ()
  (let ((tmp (make-pathname :name (concatenate 'string (pathname-name *self-model-file*) "-tmp")
                            :type (pathname-type *self-model-file*) :defaults *self-model-file*)))
    (with-open-file (out tmp :direction :output :if-exists :supersede
                         :if-does-not-exist :create :external-format :utf-8)
      ;; *PRINT-PRETTY* NIL -- the same standing SHASHT:WRITE-JSON/JSONL
      ;; gotcha found repeatedly elsewhere this session.
      (let ((*print-pretty* nil)) (shasht:write-json *self-model* out)))
    (rename-file tmp *self-model-file*)))

(defun load-self-model ()
  (handler-case
      (when (probe-file *self-model-file*)
        (with-open-file (in *self-model-file*)
          (let ((data (shasht:read-json in)))
            (dolist (s *self-model-sections*)
              (when (gethash s data) (setf (gethash s *self-model*) (coerce (gethash s data) 'list))))
            (setf *self-model-entry-counter*
                  (reduce #'max (loop for s in *self-model-sections* append (gethash s *self-model*))
                          :key (lambda (e) (gethash "id" e)) :initial-value 0)))))
    (error (e) (format t "~&[self-model] load failed, starting empty: ~a~%" e) nil)))

(define-init :restore self-model-restore
    "Restore durable state for self-model."
  (load-self-model))
