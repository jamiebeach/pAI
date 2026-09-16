;;;; soul.lisp -- P8.6, soul.md as identity anchor. 2026-07-28.
;;;;
;;;; A small, hard-capped set of first-person disposition statements
;;;; ("I am the kind of agent that..."), each required to cite at least
;;;; one REAL, existing memory node as evidence -- never invented.
;;;; Injected verbatim into the SAME system message every turn (the
;;;; SOUL:BEGIN/END marker, same refresh idiom as CONTINUITY/AFFECT/WANTS/
;;;; INTRUSIONS). This inherits "never summarized, never compacted" for
;;;; free: AGENT_LOOP.LISP's MANAGE-CONTEXT already always preserves the
;;;; system message untouched (only the BODY after it ever gets
;;;; compacted) -- no separate exclusion mechanism needed, since anything
;;;; living in the system message's content is already exempt.
;;;;
;;;; The hard cap (*SOUL-MAX-ENTRIES*) is the whole point, per the
;;;; backlog's own framing: admitting a new entry costs something. Per
;;;; explicit instruction, demotion targets the WEAKEST entry -- fewest
;;;; cited evidence nodes, oldest as a tiebreak -- not oldest-first. An
;;;; entry backed by only one memory is more expendable than one backed
;;;; by several, regardless of when either was added.
;;;;
;;;; Deliberately scoped narrower than P8.6's full intent, matching the
;;;; backlog's own dependency note (P8.6 depends on P5.1's self-model,
;;;; which doesn't exist yet) and P8.7 (candidate-pool/retrospective
;;;; promotion, a separate, later, more sophisticated admission policy):
;;;; entry ADMISSION here is a deliberate, explicit act (SOUL-ADD-ENTRY,
;;;; callable via lisp-eval) with real, validated evidence -- not an
;;;; automatic pipeline. Starts empty on first load; no seeded/invented
;;;; entries, since "every entry traces to evidence" would be violated by
;;;; fabricating an initial set.
;;;;
;;;; Load live (no restart) via lisp-eval or repl-drop, after
;;;; memory-nodes.lisp (needs MEMORY-GET-NODE to validate evidence):
;;;;   (load "/agent/state/soul.lisp")

(in-package :agent)

(export '(soul-entries soul-add-entry soul-entry-evidence soul-state))

(defparameter *soul-file* #P"/agent/state/soul.json")
(defparameter *soul-max-entries* 25
  "Hard cap. The scarcity is what makes an entry mean something --
admitting a new one always costs the weakest existing one.")

(defvar *soul-entries* nil
  "List of entry hash-tables: {id, statement, evidence-node-ids,
created-at}. Order is not load-bearing -- all current entries are always
injected together.")
(defvar *soul-lock* (bt:make-lock "soul"))
(defvar *soul-entry-counter* 0)

(defun %soul-entry-weaker-p (a b)
  "T if A is more expendable than B: fewer cited evidence nodes, or (on a
tie) older -- having had more time to accumulate further support and
still not gaining any makes it the more expendable of the two, not the
more protected one."
  (let ((ea (length (coerce (gethash "evidence-node-ids" a) 'list)))
        (eb (length (coerce (gethash "evidence-node-ids" b) 'list))))
    (cond ((< ea eb) t)
          ((> ea eb) nil)
          (t (< (gethash "created-at" a) (gethash "created-at" b))))))

(defun %soul-validate-evidence (evidence-node-ids)
  "Every cited id must actually exist as a real memory node -- 'traces to
at least one real memory node' is enforced here, not left to caller
discipline. Returns (values ok-p reason)."
  (cond
    ((or (null evidence-node-ids) (zerop (length evidence-node-ids)))
     (values nil "must cite at least one memory node id as evidence"))
    ((not (fboundp 'memory-get-node))
     (values nil "cannot validate evidence -- memory-nodes.lisp not loaded"))
    (t (dolist (id (coerce evidence-node-ids 'list))
         (unless (and (stringp id) (ignore-errors (memory-get-node id)))
           (return-from %soul-validate-evidence
             (values nil (format nil "evidence node ~a does not exist" id)))))
       (values t nil))))

(defun soul-entries ()
  "All current entries, for introspection."
  (copy-list *soul-entries*))

(defun soul-state ()
  (obj "count" (length *soul-entries*) "cap" *soul-max-entries*
       "statements" (coerce (mapcar (lambda (e) (gethash "statement" e)) *soul-entries*) 'vector)))

(defun soul-entry-evidence (id)
  "The real memory nodes behind entry ID, so a claim can be explained
from evidence rather than asserted -- P8.6's own acceptance criterion:
it should be able to answer why an entry is there."
  (let ((entry (find id *soul-entries* :key (lambda (e) (gethash "id" e)))))
    (and entry
         (mapcar (lambda (nid)
                   (or (and (fboundp 'memory-get-node) (ignore-errors (memory-get-node nid)))
                       nid))
                 (coerce (gethash "evidence-node-ids" entry) 'list)))))

(defun soul-add-entry (statement evidence-node-ids)
  "Adds a new soul entry. STATEMENT should be a short, first-person
disposition ('I am the kind of agent that...'). EVIDENCE-NODE-IDS must be
a non-empty list of real, existing memory node ids -- validated here. If
already at the cap, demotes the single weakest existing entry (see
%SOUL-ENTRY-WEAKER-P) to make room. Returns (values new-entry demoted-
entry-or-nil) on success, (values nil reason) on rejection."
  (multiple-value-bind (ok reason) (%soul-validate-evidence evidence-node-ids)
    (if (not ok)
        (values nil reason)
        (bt:with-lock-held (*soul-lock*)
          (let ((demoted nil))
            (when (>= (length *soul-entries*) *soul-max-entries*)
              (let ((weakest (reduce (lambda (a b) (if (%soul-entry-weaker-p a b) a b)) *soul-entries*)))
                (setf *soul-entries* (remove weakest *soul-entries*))
                (setf demoted weakest)))
            (incf *soul-entry-counter*)
            (let ((entry (obj "id" *soul-entry-counter* "statement" statement
                               "evidence-node-ids" (coerce evidence-node-ids 'vector)
                               "created-at" (get-universal-time))))
              (push entry *soul-entries*)
              (ignore-errors (save-soul))
              (when (fboundp 'log-event)
                (ignore-errors
                 (funcall 'log-event "soul-entry-added"
                          (obj "id" (gethash "id" entry) "statement" statement
                               "demoted" (if demoted (gethash "statement" demoted) :null)))))
              (values entry demoted)))))))

(defun save-soul ()
  (let ((tmp (make-pathname :name (concatenate 'string (pathname-name *soul-file*) "-tmp")
                            :type (pathname-type *soul-file*) :defaults *soul-file*)))
    (with-open-file (out tmp :direction :output :if-exists :supersede
                         :if-does-not-exist :create :external-format :utf-8)
      ;; *PRINT-PRETTY* NIL -- the same standing SHASHT:WRITE-JSON/JSONL
      ;; gotcha found (again) in self-mod-phase4.lisp earlier the same day.
      (let ((*print-pretty* nil)) (shasht:write-json (coerce *soul-entries* 'vector) out)))
    (rename-file tmp *soul-file*)))

(defun load-soul ()
  (handler-case
      (when (probe-file *soul-file*)
        (with-open-file (in *soul-file*)
          (setf *soul-entries* (coerce (shasht:read-json in) 'list))
          (setf *soul-entry-counter*
                (reduce #'max *soul-entries* :key (lambda (e) (gethash "id" e)) :initial-value 0))))
    (error (e) (format t "~&[soul] load failed, starting empty: ~a~%" e) nil)))

;;; --- injection, verbatim, into the system message --------------------

(defun %soul-render-text ()
  (if (null *soul-entries*)
      "(nothing yet -- no identity-defining entries have been added)"
      (format nil "~{- ~a~%~}" (mapcar (lambda (e) (gethash "statement" e)) (reverse *soul-entries*)))))

(defun %soul-refresh-section ()
  (when (and (fboundp 'context-projection-legacy-mutation-enabled-p)
             (not (context-projection-legacy-mutation-enabled-p)))
    (return-from %soul-refresh-section nil))
  (let ((sysmsg (find "system" *last-self-mod-history* :key (lambda (m) (gethash "role" m)) :test #'string=)))
    (when sysmsg
      (let* ((content (gethash "content" sysmsg))
             (begin "<!-- SOUL:BEGIN -->") (end "<!-- SOUL:END -->")
             (bp (and (stringp content) (search begin content)))
             (ep (and (stringp content) (search end content)))
             (text (%soul-render-text)))
        (if (and bp ep (< bp ep))
            (setf (gethash "content" sysmsg)
                  (concatenate 'string (subseq content 0 (+ bp (length begin)))
                               (format nil "~%~a~%" text) (subseq content ep)))
            (when (stringp content)
              (setf (gethash "content" sysmsg)
                    (format nil "~a~%~%## Who I am (stable, never summarized away, each line grounded in real memory)~%~a~%~a~%~a"
                            content begin text end))))))))

(unless (fboundp 'pai-base-auto-turn-soul)
  (setf (fdefinition 'pai-base-auto-turn-soul) (fdefinition 'auto-turn)))
(defun auto-turn (prompt)
  (ignore-errors (%soul-refresh-section))
  (funcall 'pai-base-auto-turn-soul prompt))

(define-init :restore soul-restore
    "Restore durable state for soul."
  (load-soul))
