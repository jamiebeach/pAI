;;;; latent-thoughts-v2.lisp -- auditable E9 incubation state machine.

(in-package :agent)

;; Provider-side forward declaration for the legacy-audit port. The consumer
;; later owns its NIL default; this file only registers a function at :INSTALL.
(defvar *legacy-audit-hash-fn*)

(export '(latent-v2-seed latent-v2-transition latent-v2-process-pass
          latent-v2-for-prompt latent-v2-mark-expressed
          latent-v2-thoughts latent-v2-report
          latent-v2-save latent-v2-load))

(defparameter *latent-v2-file* #P"/agent/state/latent-thoughts-v2.json")
(defparameter *latent-v2-max-records* 500)
(defparameter *latent-v2-default-ttl-seconds* (* 14 24 60 60))
(defparameter *latent-v2-topic-cooldown-seconds* (* 6 60 60))
(defparameter *latent-v2-max-depth-without-evidence* 3)
(defparameter *latent-v2-max-projected* 2)
(defparameter *latent-v2-states*
  '("seeded" "grounding-needed" "scheduled" "evolving" "ready"
    "expressed" "merged" "discarded" "expired" "blocked"))
(defparameter *latent-v2-operations*
  '("connect-evidence" "differentiate" "research" "form-question"
    "wait-for-cue" "draft" "merge" "discard"))
(defvar *latent-v2-thoughts* nil)
(defvar *latent-v2-lock* (bt:make-lock "latent-thoughts-v2"))

(defun %latent-v2-list (value)
  (cond ((null value) nil) ((listp value) value)
        ((vectorp value) (coerce value 'list)) (t (list value))))

(defun %latent-v2-id ()
  (format nil "latentv2-~a-~a" (get-universal-time) (random 1000000)))

(defun %latent-v2-words (text)
  (remove-if (lambda (word) (< (length word) 4))
             (uiop:split-string (string-downcase (or text ""))
                                :separator '(#\Space #\Tab #\Newline #\. #\, #\! #\? #\: #\; #\-))))

(defun %latent-v2-topic (text)
  (let ((words (%latent-v2-words text)))
    (format nil "~{~a~^-~}" (subseq words 0 (min 6 (length words))))))

(defun %latent-v2-hash (text)
  "Stable FNV-1a content fingerprint; this is an audit identity, not a secret."
  (let ((value #xCBF29CE484222325))
    (loop for character across (or text "")
          do (setf value (ldb (byte 64 0)
                              (* (logxor value (char-code character))
                                 #x100000001B3))))
    (format nil "~16,'0x" value)))

(defun %latent-v2-active-p (thought)
  (member (gethash "state" thought)
          '("seeded" "grounding-needed" "scheduled" "evolving" "ready")
          :test #'string=))

(defun %latent-v2-log (type thought &optional transition)
  (when (fboundp 'log-event)
    (ignore-errors
      (funcall 'log-event type
               (obj "event_version" 2 "latent_id" (gethash "id" thought)
                    "topic" (gethash "topic" thought)
                    "state" (gethash "state" thought)
                    "transition" (or transition :null))))))

(defun %latent-v2-save-unlocked ()
  (ensure-directories-exist *latent-v2-file*)
  (let ((tmp (make-pathname :name "latent-thoughts-v2-tmp" :type "json"
                            :defaults *latent-v2-file*)))
    (with-open-file (out tmp :direction :output :if-exists :supersede
                             :if-does-not-exist :create :external-format :utf-8)
      (shasht:write-json (coerce *latent-v2-thoughts* 'vector) out))
    (uiop:rename-file-overwriting-target tmp *latent-v2-file*))
  t)

(defun latent-v2-save ()
  (bt:with-lock-held (*latent-v2-lock*) (%latent-v2-save-unlocked)))

(defun latent-v2-load ()
  (handler-case
      (when (probe-file *latent-v2-file*)
        (setf *latent-v2-thoughts*
              (coerce (shasht:read-json
                       (uiop:read-file-string *latent-v2-file*)) 'list)))
    (error (condition)
      (format t "~&[latent-v2] load failed: ~a~%" condition) nil)))

(defun %latent-v2-find (id)
  (find id *latent-v2-thoughts* :key (lambda (thought) (gethash "id" thought))
                                 :test #'string=))

(defun %latent-v2-expire! (&optional (now (get-universal-time)))
  (let ((changed nil))
    (dolist (thought *latent-v2-thoughts*)
      (when (and (%latent-v2-active-p thought)
                 (numberp (gethash "expires_at" thought))
                 (<= (gethash "expires_at" thought) now))
        (setf (gethash "state" thought) "expired"
              (gethash "updated_at" thought) now)
        (%latent-v2-log "latent-transition" thought
                        (obj "operation" "expire" "actor" "clock"))
        (setf changed t)))
    changed))

(defun %latent-v2-overlap (left right)
  (let* ((a (remove-duplicates (%latent-v2-words left) :test #'string=))
         (b (remove-duplicates (%latent-v2-words right) :test #'string=))
         (intersection (count-if (lambda (word) (member word b :test #'string=)) a))
         (union (- (+ (length a) (length b)) intersection)))
    (if (zerop union) 0.0d0 (/ intersection (float union 1.0d0)))))

(defun %latent-v2-near-repeat (content topic)
  (find-if (lambda (thought)
             (and (%latent-v2-active-p thought)
                  (or (string= topic (gethash "topic" thought))
                      (>= (%latent-v2-overlap content (gethash "content" thought)) 0.8d0))))
           *latent-v2-thoughts*))

(defun latent-v2-seed (content &key topic evidence-ids source-event-ids
                                    (actor "internal") (now (get-universal-time))
                                    expires-at)
  "Create a seed or merge a near repeat. This function never sends."
  (let* ((text (string-trim '(#\Space #\Tab #\Newline #\Return) (or content "")))
         (key (or topic (%latent-v2-topic text))))
    (when (< (length text) 12) (return-from latent-v2-seed (values nil "missing-content")))
    (bt:with-lock-held (*latent-v2-lock*)
      (%latent-v2-expire! now)
      (let ((repeat (%latent-v2-near-repeat text key)))
        (if repeat
            (progn
              (incf (gethash "merge_count" repeat 0))
              (setf (gethash "updated_at" repeat) now
                    (gethash "last_evolved_at" repeat) now)
              (%latent-v2-log "latent-transition" repeat
                              (obj "operation" "merge" "actor" actor
                                   "merged_content_hash" (%latent-v2-hash text)))
              (%latent-v2-save-unlocked)
              (values repeat "merged"))
            (let* ((ids (remove-duplicates (%latent-v2-list evidence-ids) :test #'string=))
                   (state (if ids "seeded" "grounding-needed"))
                   (thought
                     (obj "id" (%latent-v2-id) "state" state "topic" key
                          "content" text "content_hash" (%latent-v2-hash text)
                          "evidence_ids" (coerce ids 'vector)
                          "source_event_ids" (coerce (%latent-v2-list source-event-ids) 'vector)
                          "novelty" 1.0d0 "depth" 0 "depth_without_new_evidence" 0
                          "next_reconsideration" now
                          "expires_at" (or expires-at (+ now *latent-v2-default-ttl-seconds*))
                          "created_at" now "updated_at" now "last_evolved_at" :null
                          "merge_count" 0 "transitions" (vector))))
              (push thought *latent-v2-thoughts*)
              (when (> (length *latent-v2-thoughts*) *latent-v2-max-records*)
                (setf *latent-v2-thoughts*
                      (subseq *latent-v2-thoughts* 0 *latent-v2-max-records*)))
              (%latent-v2-save-unlocked)
              (%latent-v2-log "latent-seeded" thought)
              (values thought "seeded")))))))

(defun %latent-v2-next-state (operation)
  (cond ((member operation '("connect-evidence" "differentiate" "research") :test #'string=) "evolving")
        ((member operation '("form-question" "wait-for-cue") :test #'string=) "scheduled")
        ((string= operation "draft") "ready")
        ((string= operation "merge") "merged")
        ((string= operation "discard") "discarded")))

(defun latent-v2-transition (id operation &key content evidence-ids novelty
                                          next-reconsideration (actor "internal")
                                          (now (get-universal-time)))
  "Apply one material transition and append a complete audit record."
  (unless (member operation *latent-v2-operations* :test #'string=)
    (return-from latent-v2-transition (values nil "invalid-operation")))
  (bt:with-lock-held (*latent-v2-lock*)
    (let ((thought (%latent-v2-find id)))
      (unless thought (return-from latent-v2-transition (values nil "missing-thought")))
      (unless (%latent-v2-active-p thought)
        (return-from latent-v2-transition (values nil "terminal-state")))
      (let* ((before-content (gethash "content" thought))
             (before-hash (gethash "content_hash" thought))
             (old-evidence (%latent-v2-list (gethash "evidence_ids" thought)))
             (added (set-difference (%latent-v2-list evidence-ids) old-evidence :test #'string=))
             (content-change (and (stringp content) (not (string= content before-content))))
             (schedule-change (and next-reconsideration
                                   (not (equal next-reconsideration
                                               (gethash "next_reconsideration" thought)))))
             (novelty-change (and (numberp novelty)
                                  (not (equal novelty (gethash "novelty" thought)))))
             (operation-change (member operation '("merge" "discard") :test #'string=)))
        (unless (or content-change added schedule-change novelty-change operation-change)
          (return-from latent-v2-transition (values nil "no-state-change")))
        (when (and (null added)
                   (>= (gethash "depth_without_new_evidence" thought 0)
                       *latent-v2-max-depth-without-evidence*)
                   (not operation-change))
          (setf (gethash "state" thought) "blocked")
          (%latent-v2-save-unlocked)
          (return-from latent-v2-transition (values thought "depth-cap")))
        (let ((before-state (gethash "state" thought)))
        (when content-change
          (setf (gethash "content" thought) content
                (gethash "content_hash" thought) (%latent-v2-hash content)))
        (when added
          (setf (gethash "evidence_ids" thought)
                (coerce (append old-evidence added) 'vector)))
        (when novelty-change (setf (gethash "novelty" thought) novelty))
        (when next-reconsideration
          (setf (gethash "next_reconsideration" thought) next-reconsideration))
        (incf (gethash "depth" thought 0))
        (if added
            (setf (gethash "depth_without_new_evidence" thought) 0)
            (incf (gethash "depth_without_new_evidence" thought 0)))
        (setf (gethash "state" thought) (%latent-v2-next-state operation)
              (gethash "updated_at" thought) now
              (gethash "last_evolved_at" thought) now)
        (let ((transition
                (obj "operation" operation "actor" actor "at" now
                     "before_hash" before-hash
                     "after_hash" (gethash "content_hash" thought)
                     "evidence_added" (coerce added 'vector)
                     "novelty" (gethash "novelty" thought)
                     "next_reconsideration" (gethash "next_reconsideration" thought)
                     "from_state" before-state "to_state" (gethash "state" thought))))
          (setf (gethash "transitions" thought)
                (concatenate 'vector (gethash "transitions" thought) (vector transition)))
          (%latent-v2-save-unlocked)
          (%latent-v2-log "latent-transition" thought transition)
          (values thought nil)))))))

(defun latent-v2-process-pass (&key (now (get-universal-time)))
  "Select one eligible seed after expiring records. Recently evolved topics
remain on cooldown; merged records are terminal and therefore cannot win."
  (bt:with-lock-held (*latent-v2-lock*)
    (when (%latent-v2-expire! now) (%latent-v2-save-unlocked))
    (find-if (lambda (thought)
               (and (%latent-v2-active-p thought)
                    (numberp (gethash "next_reconsideration" thought))
                    (<= (gethash "next_reconsideration" thought) now)
                    (or (eq (gethash "last_evolved_at" thought) :null)
                        (>= (- now (gethash "last_evolved_at" thought))
                            *latent-v2-topic-cooldown-seconds*))))
             (reverse *latent-v2-thoughts*))))

(defun %latent-v2-lived-cue-p (cue)
  (and (hash-table-p cue)
       (member (gethash "origin_class" cue)
               '("lived-user" "lived-agent-action" "tool-result" "external-signal")
               :test #'string=)))

(defun %latent-v2-relevant-cue-p (thought cue)
  (and (%latent-v2-lived-cue-p cue)
       (>= (%latent-v2-overlap (gethash "content" thought)
                               (gethash "content" cue)) 0.2d0)))

(defun latent-v2-for-prompt (cues)
  "Return labelled prior-thought projection records. No delivery seam exists."
  (let ((matches nil))
    (dolist (thought *latent-v2-thoughts*)
      (when (and (string= (gethash "state" thought) "ready")
                 (some (lambda (cue) (%latent-v2-relevant-cue-p thought cue))
                       (%latent-v2-list cues)))
        (push (obj "label" "prior private thought"
                   "latent_id" (gethash "id" thought)
                   "content" (gethash "content" thought)) matches)))
    (subseq (nreverse matches) 0 (min *latent-v2-max-projected* (length matches)))))

(defun latent-v2-mark-expressed (id turn-id &key (actor "turn-capture")
                                            (now (get-universal-time)))
  "Record confirmed downstream expression. This observes a completed turn;
it does not deliver or generate one."
  (bt:with-lock-held (*latent-v2-lock*)
    (let ((thought (%latent-v2-find id)))
      (unless thought (return-from latent-v2-mark-expressed (values nil "missing-thought")))
      (unless (string= (gethash "state" thought) "ready")
        (return-from latent-v2-mark-expressed (values nil "not-ready")))
      (let ((transition
              (obj "operation" "expression-observed" "actor" actor "at" now
                   "before_hash" (gethash "content_hash" thought)
                   "after_hash" (gethash "content_hash" thought)
                   "evidence_added" (vector) "novelty" (gethash "novelty" thought)
                   "next_reconsideration" :null "from_state" "ready"
                   "to_state" "expressed" "turn_id" turn-id)))
        (setf (gethash "state" thought) "expressed"
              (gethash "updated_at" thought) now
              (gethash "transitions" thought)
              (concatenate 'vector (gethash "transitions" thought) (vector transition)))
        (%latent-v2-save-unlocked)
        (%latent-v2-log "latent-transition" thought transition)
        (values thought nil)))))

(defun latent-v2-thoughts () *latent-v2-thoughts*)
(defun latent-v2-report ()
  (let ((states (obj)))
    (dolist (state *latent-v2-states*)
      (setf (gethash state states)
            (count state *latent-v2-thoughts*
                   :key (lambda (thought) (gethash "state" thought)) :test #'string=)))
    (obj "records" (length *latent-v2-thoughts*) "states" states
         "direct_delivery_capability" nil)))

(define-init :restore latent-thoughts-v2-restore
    "Restore durable state for latent-thoughts-v2."
  (latent-v2-load))

(define-init :install latent-thoughts-v2-hash-port
    "Register latent-thoughts-v2's content hash for the legacy memory audit.
LEGACY-MEMORY-AUDIT.LISP fingerprints candidate rows and previously reached
up for %LATENT-V2-HASH by bare symbol. With no v2 latent-thoughts layer the
port stays NIL and the audit falls back to a bare SXHASH."
  (setf *legacy-audit-hash-fn* #'%latent-v2-hash)
  t)
