;;;; prediction-journal.lisp -- P5.4 (+ P5.2's surprise hook), 2026-07-28.
;;;;
;;;; before a non-trivial action, write a `prediction` memory node
;;;; carrying an explicit numeric confidence (the existing ANTICIPATE tick
;;;; already wrote prose predictions with no queryable confidence at all --
;;;; this gives that a real number). Later, RESOLVE-PREDICTION writes a
;;;; linked `prediction-outcome` node and records whether it was accurate.
;;;; PREDICTION-CALIBRATION-REPORT aggregates predicted-confidence buckets
;;;; against observed accuracy -- the backlog's own acceptance criterion.
;;;;
;;;; P5.2 is not a separate file: "on mismatch, queue a self-model
;;;; revision with the discrepancy as evidence" has no infrastructure of
;;;; its own beyond connecting an inaccurate RESOLVE-PREDICTION call to
;;;; SELF-MODEL-PROPOSE-REVISION (self-model.lisp, P5.1) -- see the
;;;; UNLESS ACCURATE-P branch below. That branch alone is the entire P5.2
;;;; deliverable: a real behavioural surprise produces an unprompted
;;;; self-model update, without anyone telling its it happened.
;;;;
;;;; SCOPE NOTE: full automatic resolution (deciding on its own, later,
;;;; whether a past prediction came true) would need its own judgment call
;;;; -- comparing a past guess against a fuzzy, open-ended present is a
;;;; real NLP problem, not a formality. Deliberately not built here.
;;;; RESOLVE-PREDICTION is exposed as a plain callable (same pattern as
;;;; SOUL-ADD-ENTRY / SELF-MODEL-PROPOSE-REVISION) for its to call via
;;;; lisp-eval, or a future tick, when it notices a past prediction was
;;;; confirmed or refuted -- manual/deliberate for now, staged the same
;;;; way as every other Phase 5/8 item this session.
;;;;
;;;; Structured fields (confidence, resolution status) live in their own
;;;; JSONL-adjacent journal file, not squeezed into memory_nodes' prose
;;;; content column or a schema migration -- same pattern as
;;;; loop-versions.jsonl / self-model.json / soul.json. ACCURATE-P is
;;;; stored as a STRING status ("unresolved"/"accurate"/"inaccurate"),
;;;; not a Lisp boolean -- shasht's T/NIL round-trip through JSON
;;;; true/false/null is exactly the kind of ambiguity that bit this
;;;; codebase multiple times already this session; a string sidesteps it
;;;; entirely.
;;;;
;;;; Load live (no restart) via lisp-eval or repl-drop, after
;;;; memory-nodes.lisp (MEMORY-WRITE-NODE/MEMORY-ADD-EDGE) and
;;;; self-model.lisp (SELF-MODEL-PROPOSE-REVISION, for the P5.2 hook --
;;;; guarded with FBOUNDP so load order isn't a hard requirement):
;;;;   (load "/agent/state/prediction-journal.lisp")

(in-package :agent)

(export '(write-prediction resolve-prediction unresolved-predictions
          prediction-calibration-report prediction-entry))

(defparameter *prediction-journal-file* #P"/agent/state/prediction-journal.json")

(defvar *predictions* nil
  "List of entry hash-tables: {id, node-id, content, confidence,
created-at, status, outcome-node-id, resolved-at, note}. Newest-first.")
(defvar *prediction-lock* (bt:make-lock "prediction-journal"))
(defvar *prediction-counter* 0)

(defun %prediction-find (id)
  (find id *predictions* :key (lambda (e) (gethash "id" e))))

(defun prediction-entry (id)
  "Raw entry for ID, or NIL -- for introspection/debugging."
  (%prediction-find id))

(defun write-prediction (content confidence)
  "Writes a `prediction`-kind memory node for CONTENT (prose, for the
existing memory/spreading-activation machinery), plus a structured
journal entry carrying an explicit numeric CONFIDENCE (clamped 0.0-1.0).
Returns the new journal entry's integer id -- pass it to
RESOLVE-PREDICTION once the outcome is known."
  (let* ((clamped (max 0.0d0 (min 1.0d0 (float confidence 0.0d0))))
         (node-id (memory-write-node :kind "prediction" :content content)))
    (bt:with-lock-held (*prediction-lock*)
      (incf *prediction-counter*)
      (let ((entry (obj "id" *prediction-counter* "node-id" node-id "content" content
                         "confidence" clamped "created-at" (get-universal-time)
                         "status" "unresolved" "outcome-node-id" :null
                         "resolved-at" :null "note" :null)))
        (push entry *predictions*)
        (ignore-errors (save-predictions))
        (when (fboundp 'log-event)
          (ignore-errors (funcall 'log-event "prediction-written"
                                   (obj "id" (gethash "id" entry) "confidence" clamped))))
        (gethash "id" entry)))))

(defun unresolved-predictions ()
  "Every prediction not yet resolved, newest-first -- for introspection
or manual review of what's still open."
  (remove-if-not (lambda (e) (string= (gethash "status" e) "unresolved")) *predictions*))

(defun resolve-prediction (id accurate-p &optional note)
  "Resolves prediction ID: writes a linked `prediction-outcome` memory
node (edge type \"resolves\", prediction -> outcome), records
accurate/inaccurate against the journal entry. On a genuine miss
(ACCURATE-P NIL) -- P5.2 -- automatically queues a self-model revision
under \"current-preoccupations\" citing both nodes as evidence: a real
surprise, self-reported without being told. Returns (values entry nil)
on success, (values nil reason) if ID is unknown or already resolved."
  (let ((entry (%prediction-find id)))
    (cond
      ((not entry) (values nil (format nil "no prediction with id ~a" id)))
      ((not (string= (gethash "status" entry) "unresolved"))
       (values nil (format nil "prediction ~a already resolved (~a)" id (gethash "status" entry))))
      (t
       (bt:with-lock-held (*prediction-lock*)
         (let* ((status (if accurate-p "accurate" "inaccurate"))
                (outcome-content (format nil "Prediction (~a): ~a~%Outcome: ~a~@[ -- ~a~]"
                                          status (gethash "content" entry)
                                          (if accurate-p "confirmed as expected" "did not happen as expected")
                                          note))
                (outcome-node-id (memory-write-node :kind "prediction-outcome" :content outcome-content)))
           (memory-add-edge (gethash "node-id" entry) outcome-node-id "resolves")
           (setf (gethash "status" entry) status
                 (gethash "outcome-node-id" entry) outcome-node-id
                 (gethash "resolved-at" entry) (get-universal-time)
                 (gethash "note" entry) (or note :null))
           (ignore-errors (save-predictions))
           (when (fboundp 'log-event)
             (ignore-errors (funcall 'log-event "prediction-resolved" (obj "id" id "status" status))))
           (unless accurate-p
             (when (fboundp 'self-model-propose-revision)
               (ignore-errors
                (funcall 'self-model-propose-revision "current-preoccupations"
                         (format nil "I expected \"~a\" (~,0f% confidence) and was wrong~@[: ~a~]. Worth noticing when my own predictions about myself or what's coming miss."
                                 (gethash "content" entry) (* 100 (gethash "confidence" entry)) note)
                         (list (gethash "node-id" entry) outcome-node-id)))))
           (values entry nil)))))))

(defun prediction-calibration-report ()
  "Aggregates every resolved prediction into confidence buckets (low
<0.34, medium <0.67, high >=0.67) against observed accuracy in that
bucket -- the backlog's own acceptance criterion. An empty bucket
reports N 0 and a NIL accuracy rather than a fabricated 0%."
  (flet ((bucket (e) (let ((c (gethash "confidence" e)))
                        (cond ((< c 0.34d0) "low") ((< c 0.67d0) "medium") (t "high")))))
    (let* ((resolved (remove-if (lambda (e) (string= (gethash "status" e) "unresolved")) *predictions*))
           (out (obj)))
      (dolist (b '("low" "medium" "high"))
        (let* ((in-bucket (remove-if-not (lambda (e) (string= (bucket e) b)) resolved))
               (n (length in-bucket))
               (accurate (count-if (lambda (e) (string= (gethash "status" e) "accurate")) in-bucket)))
          (setf (gethash b out)
                (obj "n" n "accuracy" (if (plusp n) (/ (float accurate 1.0d0) n) :null)))))
      (setf (gethash "total-resolved" out) (length resolved))
      (setf (gethash "total-unresolved" out) (length (unresolved-predictions)))
      out)))

(defun save-predictions ()
  (let ((tmp (make-pathname :name (concatenate 'string (pathname-name *prediction-journal-file*) "-tmp")
                            :type (pathname-type *prediction-journal-file*) :defaults *prediction-journal-file*)))
    (with-open-file (out tmp :direction :output :if-exists :supersede
                         :if-does-not-exist :create :external-format :utf-8)
      ;; *PRINT-PRETTY* NIL -- the standing SHASHT:WRITE-JSON/JSONL gotcha,
      ;; recurred multiple times already this session.
      (let ((*print-pretty* nil)) (shasht:write-json (coerce *predictions* 'vector) out)))
    (rename-file tmp *prediction-journal-file*)))

(defun load-predictions ()
  (handler-case
      (when (probe-file *prediction-journal-file*)
        (with-open-file (in *prediction-journal-file*)
          (let ((data (shasht:read-json in)))
            (setf *predictions* (coerce data 'list))
            (setf *prediction-counter*
                  (reduce #'max *predictions* :key (lambda (e) (gethash "id" e)) :initial-value 0)))))
    (error (e) (format t "~&[prediction-journal] load failed, starting empty: ~a~%" e) nil)))

(define-init :restore prediction-journal-restore
    "Restore durable state for prediction-journal."
  (load-predictions))

;;; --- retrofit the ANTICIPATE tick to record a real confidence ----------
;;; Previously wrote a bare `prediction` memory node with prose only.
;;; Same rename-and-fall-through idiom as everywhere else in this
;;; codebase; this function isn't currently wrapped by anything else
;;; (not in *WRAP-CHAINS*), so this is the first and only wrap on it.

(unless (fboundp 'pai-base-tick-handle-anticipate)
  (setf (fdefinition 'pai-base-tick-handle-anticipate) (fdefinition '%tick-handle-anticipate)))

(defun %tick-handle-anticipate ()
  (let ((tail (last *last-self-mod-history* 6)))
    (if (< (length tail) 2)
        (continuity-buffer-append "Tried to anticipate what's next, but there's not much conversation yet to go on.")
        (handler-case
            (let* ((text (format nil "~{[~a] ~a~%~}"
                                  (loop for m in tail collect (gethash "role" m)
                                        collect (let ((c (gethash "content" m))) (if (stringp c) c "")))))
                   (resp (raw-call-model
                          (list (obj "role" "system" "content"
                                     "Given this recent conversation excerpt, write ONE short first-person guess at what your human might want to talk about or need next. Under 25 words. If genuinely nothing suggests itself, reply with exactly NOTHING. On a SECOND line, write only a number 0-100: your confidence that this guess is right.")
                                (obj "role" "user" "content" text))))
                   (raw (gethash "content" (ref resp "choices" 0 "message")))
                   (lines (and (stringp raw) (remove "" (uiop:split-string raw :separator '(#\Newline)) :test #'string=)))
                   (prediction (and lines (string-trim '(#\Space #\.) (first lines))))
                   (conf-line (and (> (length lines) 1) (second lines)))
                   (confidence (or (and conf-line (ignore-errors (/ (parse-integer conf-line :junk-allowed t) 100.0d0)))
                                   0.5d0)))
              (when (and (stringp prediction) (not (string-equal prediction "NOTHING")) (plusp (length prediction)))
                (write-prediction prediction confidence)
                (continuity-buffer-append (format nil "Found myself anticipating: ~a (~,0f% confidence)" prediction (* 100 confidence)))))
          (error (e) (format t "~&[tick-loop] anticipate failed: ~a~%" e))))))
