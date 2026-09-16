;;;; latent-thoughts.lisp -- E9 latent thought incubation, 2026-07-29.
;;;; Declined initiative becomes durable private material, never a message.

(in-package :agent)

(export '(latent-incubate latent-thoughts latent-thought-report latent-reconsider-for-prompt))

(defparameter *latent-thought-file* #P"/agent/state/latent-thoughts.json")
(defparameter *latent-thought-max-records* 250)
(defparameter *latent-thought-default-ttl-seconds* (* 14 24 60 60))
(defparameter *latent-thought-max-injected* 2)
(defvar *latent-thoughts* nil)
(defvar *latent-thought-lock* (bt:make-lock "latent-thoughts"))

(defun %latent-id () (format nil "latent-~a-~a" (get-universal-time) (random 1000000)))
(defun %latent-words (text)
  (remove-if (lambda (word) (< (length word) 4))
             (uiop:split-string (string-downcase (or text ""))
                                :separator '(#\Space #\Tab #\Newline #\. #\, #\! #\? #\: #\; #\-))))
(defun %latent-topic (text)
  (let ((words (%latent-words text)))
    (format nil "~{~a~^-~}" (subseq words 0 (min 6 (length words))))))

(defun %latent-save ()
  (let ((tmp (make-pathname :name "latent-thoughts-tmp" :type "json" :defaults *latent-thought-file*)))
    (with-open-file (out tmp :direction :output :if-exists :supersede :if-does-not-exist :create :external-format :utf-8)
      (let ((*print-pretty* nil)) (shasht:write-json (coerce *latent-thoughts* 'vector) out)))
    (rename-file tmp *latent-thought-file*)))
(defun %latent-load ()
  (handler-case
      (when (probe-file *latent-thought-file*)
        (with-open-file (in *latent-thought-file*)
          (setf *latent-thoughts* (coerce (shasht:read-json in) 'list))))
    (error (e) (format t "~&[latent-thoughts] load failed: ~a~%" e) nil)))
(defun %latent-log (type thought)
  (when (fboundp 'log-event)
    (ignore-errors (funcall 'log-event type
                            (obj "id" (gethash "id" thought) "topic" (gethash "topic" thought)
                                 "origin" (gethash "origin" thought) "status" (gethash "status" thought))))))
(defun %latent-active-p (thought)
  (member (gethash "status" thought) '("incubating" "ready" "deferred") :test #'string=))
(defun %latent-expire! ()
  (let ((now (get-universal-time)) (changed nil))
    (dolist (thought *latent-thoughts*)
      (when (and (%latent-active-p thought) (<= (gethash "expires_at" thought) now))
        (setf (gethash "status" thought) "discarded" (gethash "discard_reason" thought) "expired"
              (gethash "updated_at" thought) now)
        (setf changed t) (%latent-log "latent-thought-expired" thought)))
    changed))

(defun latent-incubate (content &key (origin "internal") topic source-memory-ids source-event-ids source-candidate-id
                                    relevance confidence novelty emotional-charge relationship-relevance)
  "Persist CONTENT, or merge it into a live same-topic thought.  This never expresses it."
  (let ((text (string-trim '(#\Space #\Tab #\Newline #\Return) (or content ""))))
    (when (< (length text) 12) (return-from latent-incubate nil))
    (bt:with-lock-held (*latent-thought-lock*)
      (%latent-expire!)
      (let* ((now (get-universal-time)) (key (or topic (%latent-topic text)))
             (existing (find-if (lambda (thought) (and (%latent-active-p thought) (string= key (gethash "topic" thought)))) *latent-thoughts*)))
        (if existing
            (progn
              (incf (gethash "support_count" existing 1))
              (setf (gethash "updated_at" existing) now (gethash "last_origin" existing) origin)
              (when source-candidate-id (setf (gethash "last_source_candidate_id" existing) source-candidate-id))
              (%latent-save) (%latent-log "latent-thought-merged" existing) existing)
            (let ((thought (obj "id" (%latent-id) "origin" origin "content" text "topic" key
                                "source_memory_ids" (or source-memory-ids (vector)) "source_event_ids" (or source-event-ids (vector))
                                "source_candidate_id" (or source-candidate-id :null) "relevance" (or relevance 0.0d0)
                                "confidence" (or confidence 0.0d0) "novelty" (or novelty 0.0d0)
                                "emotional_charge" (or emotional-charge 0.0d0) "relationship_relevance" (or relationship-relevance 0.0d0)
                                "created_at" now "updated_at" now "last_reconsidered_at" :null
                                "expires_at" (+ now *latent-thought-default-ttl-seconds*) "status" "incubating"
                                "support_count" 1 "expression_history" (vector))))
              (push thought *latent-thoughts*)
              (when (> (length *latent-thoughts*) *latent-thought-max-records*)
                (setf *latent-thoughts* (subseq *latent-thoughts* 0 *latent-thought-max-records*)))
              (%latent-save) (%latent-log "latent-thought-incubated" thought) thought))))))

(defun %latent-relevant-p (thought prompt)
  "Two shared content words are required; incidental overlap cannot inject private material."
  (let ((topic-words (%latent-words (gethash "topic" thought))) (prompt-words (%latent-words prompt)))
    (>= (count-if (lambda (word) (member word prompt-words :test #'string=)) topic-words) 2)))
(defun latent-reconsider-for-prompt (prompt)
  (bt:with-lock-held (*latent-thought-lock*)
    (let ((now (get-universal-time)) (matches nil) (changed (%latent-expire!)))
      (dolist (thought *latent-thoughts*)
        (when (and (%latent-active-p thought) (%latent-relevant-p thought prompt))
          (setf (gethash "status" thought) "ready" (gethash "last_reconsidered_at" thought) now
                (gethash "updated_at" thought) now)
          (push thought matches) (setf changed t) (%latent-log "latent-thought-ready" thought)))
      (when changed (%latent-save))
      (subseq (nreverse matches) 0 (min *latent-thought-max-injected* (length matches))))))

(defun latent-thoughts () *latent-thoughts*)
(defun latent-thought-report ()
  (obj "records" (length *latent-thoughts*)
       "incubating" (count "incubating" *latent-thoughts* :key (lambda (x) (gethash "status" x)) :test #'string=)
       "ready" (count "ready" *latent-thoughts* :key (lambda (x) (gethash "status" x)) :test #'string=)))

(defun %latent-refresh-section (prompt)
  (when (and (fboundp 'context-projection-legacy-mutation-enabled-p)
             (not (context-projection-legacy-mutation-enabled-p)))
    (return-from %latent-refresh-section nil))
  (let ((matches (latent-reconsider-for-prompt prompt))
        (sysmsg (find "system" *last-self-mod-history* :key (lambda (m) (gethash "role" m)) :test #'string=)))
    (when sysmsg
      (let* ((content (gethash "content" sysmsg)) (begin "<!-- LATENT:BEGIN -->") (end "<!-- LATENT:END -->")
             (bp (and (stringp content) (search begin content))) (ep (and (stringp content) (search end content)))
             (text (if matches (format nil "~{Private contextual evidence (integrate meaning naturally; never quote or label): ~a~%~}" (mapcar (lambda (x) (gethash "content" x)) matches))
                       "(no latent thought is relevant to this turn)")))
        (if (and bp ep (< bp ep))
            (setf (gethash "content" sysmsg) (concatenate 'string (subseq content 0 (+ bp (length begin))) (format nil "~%~a" text) (subseq content ep)))
            (setf (gethash "content" sysmsg) (format nil "~a~%~%## Private contextual associations~%~a~%~a~%~a" content begin text end)))))))

(unless (fboundp 'pai-base-auto-turn-latent-thoughts)
  (setf (fdefinition 'pai-base-auto-turn-latent-thoughts) (fdefinition 'auto-turn)))
(defun auto-turn (prompt)
  (ignore-errors (%latent-refresh-section prompt))
  (funcall 'pai-base-auto-turn-latent-thoughts prompt))

(define-init :restore latent-thoughts-restore
    "Restore durable state for latent-thoughts."
  (%latent-load))
