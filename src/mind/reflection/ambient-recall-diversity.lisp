;;;; ambient-recall-diversity.lisp -- 2026-07-29.
;;;;
;;;; Root-caused via a read-only review of a full day's TICK-NOTE/
;;;; SELF-MODEL-REVISED/REFLECTION-PASS output at the operator's request: three
;;;; unrelated tick types (explore, consolidate/light, consolidate/full-
;;;; reflection) independently spent 7+ hours circling ONE memory
;;;; ("devotion to the unfinished..."), producing dozens of cosmetically
;;;; reworded restatements dressed up as new insight. Root cause is
;;;; structural, not a prompting problem: %TICK-HANDLE-LIGHT-CONSOLIDATE,
;;;; %TICK-HANDLE-FULL-REFLECTION, and %EXPLORE-PICK-QUESTION all seed
;;;; themselves with a FIXED literal query string ("what matters most
;;;; right now" / "what has been happening recently") rather than
;;;; anything that varies -- so MEMORY-RECALL's similarity ranking picks
;;;; the same node(s) every time, and MEMORY-RECALL's own retrieval-boosts-
;;;; activation design (+0.2 per surfacing, memory-nodes.lisp) then makes
;;;; that same node win even harder next time. A closed feedback loop with
;;;; no external pressure to break it.
;;;;
;;;; This does NOT touch real conversational recall (a live chat turn
;;;; legitimately may need the same relevant memory to resurface across
;;;; several turns -- that's correct behavior, not the bug) and does NOT
;;;; touch the activation-boost mechanism itself (used correctly elsewhere,
;;;; e.g. soul-candidate-pool.lisp's retrospective-evidence tracking).
;;;; Scoped narrowly to the three call sites that use a FIXED, ownerless
;;;; query string to ask "what should I think about" with no other
;;;; grounding -- exactly the pattern that has no natural variety to draw
;;;; on and needs an explicit nudge.
;;;;
;;;; Mechanism: %RECALL-AMBIENT wraps MEMORY-RECALL's new EXCLUDE-IDS
;;;; keyword (memory-nodes.lisp) with a simple rolling cooldown -- any
;;;; node surfaced via an ambient call in the last
;;;; *AMBIENT-COOLDOWN-SECONDS* is excluded from the candidate pool on the
;;;; NEXT ambient call. Long enough (3 hours) to force real variety across
;;;; a tick-dense afternoon; short enough that a genuinely important,
;;;; recurring theme can still resurface within the same day, not banished
;;;; forever. If every candidate happens to be in cooldown (a small memory
;;;; store, early on), falls back to the un-excluded call rather than
;;;; returning nothing -- degraded, not broken, same principle as
;;;; %EMBED-WORD-OVERLAP-FALLBACK.
;;;;
;;;; %TICK-HANDLE-LIGHT-CONSOLIDATE and %TICK-HANDLE-FULL-REFLECTION are
;;;; wrapped here (rename-and-fall-through) with full replacement bodies
;;;; -- consistent with this codebase's convention of wrapping FROM other
;;;; files rather than editing tick-loop.lisp directly, even when the new
;;;; body doesn't call through to the old one for its main logic. Only the
;;;; single fixed-query recall call in each is changed; everything else
;;;; (thresholds, model prompts, edge-writing, continuity-buffer text) is
;;;; identical to the original. %EXPLORE-PICK-QUESTION is edited directly
;;;; in conversational-initiative.lisp instead, since that file is this
;;;; session's own and nothing else wraps that function.
;;;;
;;;; Load live (no restart) via lisp-eval or repl-drop, after
;;;; memory-nodes.lisp (MEMORY-RECALL's EXCLUDE-IDS) and tick-loop.lisp
;;;; (%TICK-HANDLE-LIGHT-CONSOLIDATE, %TICK-HANDLE-FULL-REFLECTION):
;;;;   (load "/agent/state/ambient-recall-diversity.lisp")

(in-package :agent)

(export '(ambient-recall-report))

(defparameter *ambient-cooldown-seconds* (* 3 3600)
  "How long a node stays excluded from ambient (fixed-query) recall after
surfacing. Long enough to break a multi-hour monoculture; short enough
that a genuinely recurring theme isn't banished for days.")
(defparameter *ambient-history-file* #P"/agent/state/ambient-recall-history.json")

(defvar *ambient-history* nil
  "Alist of (node-id . universal-time-last-surfaced), persisted.")

(defun %ambient-prune ()
  (let ((cutoff (- (get-universal-time) *ambient-cooldown-seconds*)))
    (setf *ambient-history* (remove-if (lambda (e) (< (cdr e) cutoff)) *ambient-history*))))

(defun %ambient-excluded-ids ()
  (%ambient-prune)
  (mapcar #'car *ambient-history*))

(defun %ambient-record (ids)
  (let ((now (get-universal-time)))
    (dolist (id ids)
      (let ((existing (assoc id *ambient-history* :test #'equal)))
        (if existing (setf (cdr existing) now) (push (cons id now) *ambient-history*)))))
  (ignore-errors (save-ambient-history)))

(defun %recall-ambient (query &key (k 5) debug kinds)
  "MEMORY-RECALL, but excluding anything surfaced by a prior ambient call
within the cooldown window, then recording whatever comes back for the
next one. Always requests DEBUG internally (need ids to record regardless
of what the caller wants back), then reshapes to the caller's own DEBUG
convention on the way out, matching MEMORY-RECALL's own dual return
contract exactly."
  (let* ((excluded (%ambient-excluded-ids))
         (top (memory-recall query :k k :debug t :exclude-ids excluded
                             :kinds kinds)))
    (when (< (length top) (min 2 k))
      ;; Cooldown starved the pool (small memory store, or genuinely
      ;; nothing else relevant) -- fall back to the un-excluded call
      ;; rather than staying stuck with too little to work from. Degraded,
      ;; not broken.
      (setf top (memory-recall query :k k :debug t :kinds kinds)))
    (%ambient-record (mapcar (lambda (n) (gethash "id" n)) top))
    (if debug top (mapcar (lambda (n) (gethash "content" n)) top))))

(defun ambient-recall-report ()
  (%ambient-prune)
  (obj "nodes-in-cooldown" (length *ambient-history*)
       "cooldown-seconds" *ambient-cooldown-seconds*))

;;; --- %TICK-HANDLE-LIGHT-CONSOLIDATE, full replacement --------------------

(unless (fboundp 'pai-base-tick-handle-light-consolidate-ambient)
  (setf (fdefinition 'pai-base-tick-handle-light-consolidate-ambient) (fdefinition '%tick-handle-light-consolidate)))

(defun %tick-handle-light-consolidate ()
  (let ((top (%recall-ambient "what matters most right now" :k 5)))
    (if (< (length top) 2)
        (continuity-buffer-append "Tried to consolidate, but there isn't enough in memory yet to draw a thread through.")
        (handler-case
            (let* ((resp (raw-call-model
                          (list (obj "role" "system" "content"
                                     "Given these memories, write ONE third-person sentence synthesizing a pattern or connection across them -- a genuine small insight, not a list. Under 30 words.")
                                (obj "role" "user" "content" (format nil "~{- ~a~%~}" top)))))
                   (reflection (gethash "content" (ref resp "choices" 0 "message"))))
              (when (stringp reflection)
                (memory-write-node :kind "reflection" :content reflection)
                (continuity-buffer-append (format nil "Took a moment to consolidate recent memory: ~a" reflection))))
          (error (e) (format t "~&[tick-loop] light consolidate failed: ~a~%" e))))))

;;; --- %TICK-HANDLE-FULL-REFLECTION, full replacement -----------------------
;;; Only the initial "recent" seed recall (the fixed-string one) is
;;; changed to %RECALL-AMBIENT. The per-question evidence recall further
;;; down is left as plain MEMORY-RECALL -- its query is the model-generated
;;; question text, which already varies per pass, so it isn't the fixed-
;;; query monoculture risk this file targets, and suppressing genuinely
;;; the-best-evidence nodes there would risk shallower reflections for no
;;; real benefit.

(unless (fboundp 'pai-base-tick-handle-full-reflection-ambient)
  (setf (fdefinition 'pai-base-tick-handle-full-reflection-ambient) (fdefinition '%tick-handle-full-reflection)))

(defun %tick-handle-full-reflection ()
  (handler-case
      (let* ((recent (%recall-ambient "what has been happening recently" :k 8)))
        (if (< (length recent) 2)
            (continuity-buffer-append "Tried a deeper reflection pass, but there isn't enough in memory yet.")
            (let* ((questions-resp
                     (raw-call-model
                      (list (obj "role" "system" "content"
                                 "Given these recent memories, name the 3 most salient QUESTIONS worth reflecting on -- genuine open questions the memories raise, not facts already answered. Respond with exactly 3 lines, one question per line, nothing else.")
                            (obj "role" "user" "content" (format nil "~{- ~a~%~}" recent)))))
                   (questions-text (gethash "content" (ref questions-resp "choices" 0 "message")))
                   (questions (and (stringp questions-text)
                                   (let ((trimmed (mapcar (lambda (f) (string-trim '(#\Space #\Newline #\- #\Tab) f))
                                                           (uiop:split-string questions-text :separator '(#\?)))))
                                     (mapcar (lambda (f) (concatenate 'string f "?"))
                                             (remove-if (lambda (f) (zerop (length f))) trimmed)))))
                   (reflections-made 0))
              (if (< (length questions) 1)
                  (continuity-buffer-append "Tried a deeper reflection pass, but nothing salient enough came up.")
                  (dolist (q questions)
                    (let ((evidence (memory-recall q :k 5 :debug t)))
                      (when (>= (length evidence) 1)
                        (let* ((evidence-text (format nil "~{- ~a~%~}" (mapcar (lambda (e) (gethash "content" e)) evidence)))
                               (resp (raw-call-model
                                      (list (obj "role" "system" "content"
                                                 "Given this question and the evidence below, write ONE third-person sentence synthesizing an answer or insight -- grounded specifically in the evidence, not a generic statement. Under 30 words.")
                                            (obj "role" "user" "content" (format nil "Question: ~a~%~%Evidence:~%~a" q evidence-text)))))
                               (reflection (gethash "content" (ref resp "choices" 0 "message"))))
                          (when (stringp reflection)
                            (let ((reflection-id (memory-write-node :kind "reflection" :content reflection)))
                              (dolist (e evidence)
                                (ignore-errors (memory-add-edge reflection-id (gethash "id" e) "evidence-for")))
                              (incf reflections-made)
                              (continuity-buffer-append (format nil "Reflected on \"~a\" and concluded: ~a" q reflection)))))))))
              (setf *importance-since-last-reflection* 0.0)
              (when (fboundp 'log-event)
                (ignore-errors (funcall 'log-event "reflection-pass" (obj "questions" (length questions) "reflections_made" reflections-made)))))))
    (error (e)
      (format t "~&[tick-loop] full reflection failed: ~a~%" e)
      (continuity-buffer-append "Tried a deeper reflection pass but it didn't come together."))))

;;; --- persistence -----------------------------------------------------------

(defun save-ambient-history ()
  (let* ((tmp (make-pathname :name "ambient-recall-history-tmp" :type "json" :defaults *ambient-history-file*))
         (content
           (let ((*print-pretty* nil))
             (shasht:write-json
              (coerce
               (mapcar (lambda (e) (obj "id" (car e) "at" (cdr e)))
                       *ambient-history*)
               'vector)
              nil))))
    (with-open-file (out tmp :direction :output :if-exists :supersede
                         :if-does-not-exist :create :external-format :utf-8)
      (write-string content out))
    (multiple-value-prog1 (rename-file tmp *ambient-history-file*)
      (when (fboundp 'log-projection-state)
        (ignore-errors
          (funcall 'log-projection-state
                   "ambient-recall-history" *ambient-history-file*
                   content))))))

(defun load-ambient-history ()
  (handler-case
      (when (probe-file *ambient-history-file*)
        (with-open-file (in *ambient-history-file*)
          (setf *ambient-history*
                (mapcar (lambda (e) (cons (gethash "id" e) (gethash "at" e)))
                        (coerce (shasht:read-json in) 'list)))))
    (error (e) (format t "~&[ambient-recall-diversity] load failed, starting empty: ~a~%" e) nil)))

(define-init :restore ambient-recall-diversity-restore
    "Restore durable state for ambient-recall-diversity."
  (load-ambient-history))
