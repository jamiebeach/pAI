;;;; conversation-persistence.lisp -- durable *LAST-SELF-MOD-HISTORY*.
;;;;
;;;; *LAST-SELF-MOD-HISTORY* (self-mod.lisp) is the actual message-by-
;;;; message conversation -- literally what was just being talked about --
;;;; and until now it existed ONLY in RAM. Every restart, planned or not,
;;;; started it at NIL: the recovered agent-loop still knows who the operator is
;;;; (journal, knowledge graph), but not what the two of them were
;;;; mid-sentence about. This wraps %RUN-SELF-MOD-MESSAGES so the history
;;;; is written to disk after every COMPLETED top-level turn, and reloaded
;;;; on boot if a fresh process finds one waiting and hasn't started a
;;;; conversation of its own yet.
;;;;
;;;; %RUN-SELF-MOD-MESSAGES is already three layers deep by the time this
;;;; loads (self-mod.lisp's original -> agent_print.lisp's full CLI-aware
;;;; replacement -> web-terminal.lisp's *V2-TURN-IN-FLIGHT* wrap) -- this is a
;;;; fourth, same rename-and-fall-through idiom as everywhere else, and it
;;;; must load AFTER web-terminal.lisp so it wraps the fully-composed chain
;;;; rather than bypassing it.
;;;;
;;;; Loss window: at most the one turn that was actually in flight when
;;;; something died. A turn can involve several tool calls and model
;;;; round-trips before it resolves; *LAST-SELF-MOD-HISTORY* itself only
;;;; updates once, at the very end of all of them, so that's the natural
;;;; granularity here -- everything from every PRIOR completed turn
;;;; survives regardless.
;;;;
;;;; Write is atomic (temp file, then RENAME-FILE into place) rather than
;;;; a truncating overwrite, specifically because a truncate-then-write
;;;; pattern is exactly what corrupted memory-graph.json earlier this
;;;; session -- a crash mid-write there left a 0-byte file in place. Here,
;;;; a crash mid-write leaves the temp file damaged and the real
;;;; conversation.json untouched, still the last complete good version. A
;;;; corrupt, unreadable, or structurally invalid file on load is treated as
;;;; "nothing saved" rather than aborting boot. Structural rejection never
;;;; rewrites the source file: it remains available for forensic recovery.

(in-package :agent)

(export '(conversation-persistence-ready-p))

(defparameter *conversation-file* (pai-state-path "conversation.json"))
(defparameter *conversation-backup-dir*
  (pai-state-path "conversation-backups/"))
(defparameter *conversation-backup-retain-count* 20)
(defvar *conversation-persistence-ready* nil
  "True only after the durable conversation has been backed up, restored,
and the persistence wrapper has been installed. External input transports
must not accept work before this becomes true.")

(defun conversation-persistence-ready-p ()
  (and *conversation-persistence-ready* t))

(defun %conv-backup-utc-stamp ()
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0d~2,'0d~2,'0dT~2,'0d~2,'0d~2,'0dZ"
            year month day hour min sec)))

(defun %conv-prune-boot-backups ()
  (let ((files
          (sort (directory (merge-pathnames "conversation-*.json"
                                             *conversation-backup-dir*))
                #'string> :key #'namestring)))
    (dolist (file (nthcdr *conversation-backup-retain-count* files))
      (ignore-errors (delete-file file)))))

(defun %conv-backup-durable-at-boot ()
  "Snapshot the last valid durable history before any boot-time transport
can mutate it. This is deliberately independent of the heartbeat: a startup
race must leave a byte-for-byte recovery source even if it later overwrites
CONVERSATION.JSON."
  (handler-case
      (let ((history (funcall '%conv-persist-load)))
        (when history
          (ensure-directories-exist *conversation-backup-dir*)
          (let ((path
                  (merge-pathnames
                   (format nil "conversation-~a-~a.json"
                           (%conv-backup-utc-stamp) (length history))
                   *conversation-backup-dir*)))
            (uiop:copy-file *conversation-file* path)
            (%conv-prune-boot-backups)
            (format t "~&[conversation-persistence] boot backup wrote ~a (~a messages).~%"
                    path (length history))
            path)))
    (error (e)
      (format t "~&[conversation-persistence] boot backup failed: ~a~%" e)
      nil)))

(defun %conv-persist-write (history)
  "Best-effort. A save failure must never lose the turn's actual answer,
so this only ever logs and returns -- callers do not propagate it."
  (let* ((final-path *conversation-file*)
         (tmp-path (make-pathname :name (concatenate 'string (pathname-name final-path) "-tmp")
                                   :type (pathname-type final-path)
                                   :defaults final-path))
         ;; Serialize once so the event carries the exact bytes promoted by
         ;; the atomic rename, including every nested message/tool field.
         (content
           (with-output-to-string (out)
             (shasht:write-json (coerce history 'vector) out))))
    (with-open-file (out tmp-path :direction :output :if-exists :supersede
                         :if-does-not-exist :create :external-format :utf-8)
      (write-string content out)
      (finish-output out))
    (multiple-value-prog1 (rename-file tmp-path final-path)
      (when (fboundp 'log-conversation-history-transform)
        (ignore-errors
          (funcall 'log-conversation-history-transform final-path content))))))

(defun %conv-message-content-valid-p (content)
  "True when CONTENT has one of the shapes used by durable provider messages."
  (or (stringp content)
      (null content)
      (eq content :null)
      (and (vectorp content)
           (every #'hash-table-p content))))

(defun %conv-message-row-valid-p (row)
  "Validate the stable structural boundary needed by conversation consumers.

The persistence file is private provider history, not arbitrary JSON. Every
row must be an object with a supported role and a present content field.
Assistant tool-call rows may carry JSON null content; multimodal user content
may be a vector of part objects. Unknown or malformed shapes fail the complete
restore closed instead of being partially admitted."
  (and (hash-table-p row)
       (let ((role (gethash "role" row))
             (content nil)
             (content-present-p nil)
             (tool-calls nil)
             (tool-calls-present-p nil))
         (multiple-value-setq (content content-present-p)
           (gethash "content" row))
         (multiple-value-setq (tool-calls tool-calls-present-p)
           (gethash "tool_calls" row))
         (and (stringp role)
              (member role '("system" "user" "assistant" "tool")
                      :test #'string=)
              content-present-p
              (%conv-message-content-valid-p content)
              (or (not tool-calls-present-p)
                  (and (or (vectorp tool-calls) (listp tool-calls))
                       (every #'hash-table-p tool-calls)))))))

(defun %conv-history-valid-p (history)
  "True only for a complete sequence of structurally valid message objects."
  (and (or (vectorp history) (listp history))
       (every #'%conv-message-row-valid-p history)))

(defun %conv-persist-load ()
  "Return saved history as a list, or NIL when it cannot be safely restored.

Syntax-valid JSON with the wrong schema is treated as absent just like a
truncated file. Rejection is read-only: the durable source is left unchanged
and is not eligible for the validated boot-backup path."
  (handler-case
      (when (probe-file *conversation-file*)
        (with-open-file (in *conversation-file*)
          (let ((decoded (shasht:read-json in)))
            (if (%conv-history-valid-p decoded)
                (coerce decoded 'list)
                (progn
                  (format t "~&[conversation-persistence] ignored schema-invalid history in ~a; source left unchanged.~%"
                          *conversation-file*)
                  nil)))))
    (error (e)
      (format t "~&[conversation-persistence] could not read ~a, starting fresh: ~a~%"
              *conversation-file* e)
      nil)))

;; This must run before restoration and, by contract, before any external
;; input transport is allowed to start.
(define-init :restore conversation-persistence-restore
    "Restore durable state for conversation-persistence."
  (%conv-backup-durable-at-boot))

(unless (fboundp '%conv-base-run-self-mod-messages)
  (setf (fdefinition '%conv-base-run-self-mod-messages) (fdefinition '%run-self-mod-messages)))

(defun %run-self-mod-messages (messages)
  (handler-case
      (let ((result (funcall '%conv-base-run-self-mod-messages messages)))
        (handler-case (%conv-persist-write *last-self-mod-history*)
          (error (e)
            (format t "~&[conversation-persistence] save failed (turn result unaffected): ~a~%" e)))
        result)
    (error (condition)
      ;; A typed publication failure deliberately leaves
      ;; *LAST-SELF-MOD-HISTORY* ending at the inbound user record. Persist it
      ;; before the channel renders its system error. Other errors retain the
      ;; historical behavior because the inner runner does not replace the
      ;; last good history for them.
      (handler-case (%conv-persist-write *last-self-mod-history*)
        (error (save-error)
          (format t "~&[conversation-persistence] failure-path save failed: ~a~%"
                  save-error)))
      (error condition))))

;;; Restore on load, but ONLY if nothing's already in progress in THIS
;;; live image -- never clobber a live conversation with a possibly-stale
;;; on-disk copy just because this file happens to get reloaded.
(unless *last-self-mod-history*
  (let ((restored (%conv-persist-load)))
    (when restored
      (setf *last-self-mod-history* restored)
      (format t "~&[conversation-persistence] restored ~a saved messages from ~a~%"
              (length restored) *conversation-file*))))

;;; Seed *V2-RING* (web-terminal.lisp) with the just-restored history too, if
;;; it's loaded and still empty. Found live, 2026-07-27: /api/v2/history
;;; serves ONLY from the ring the instant anything's been broadcast into
;;; it -- even one proactive check-in message -- since the ring starts
;;; genuinely empty on every fresh boot. Whatever broadcasts first (often
;;; a proactive nudge, or the very next message either side sends)
;;; permanently hides the real backlog for the rest of that boot's life,
;;; even though the full conversation is sitting right here in
;;; *LAST-SELF-MOD-HISTORY*. Must run HERE, not inside web-terminal.lisp itself:
;;; web-terminal.lisp loads BEFORE this file restores *LAST-SELF-MOD-HISTORY*
;;; from disk, so seeding there would always seed from an empty
;;; conversation. *V2-NEXT-ID* is advanced to match, so the next live
;;; broadcast continues the id sequence instead of colliding with ids the
;;; seeded batch already used.
(when (and (boundp '*v2-ring*) (null *v2-ring*)
           (fboundp '%v2-classify-history) *last-self-mod-history*)
  (let ((seeded (%v2-classify-history *last-self-mod-history*)))
    (when seeded
      (setf *v2-ring* (reverse seeded))
      (when (boundp '*v2-next-id*)
        (setf *v2-next-id*
              (reduce #'max seeded :key (lambda (e) (gethash "id" e)) :initial-value *v2-next-id*)))
      (format t "~&[conversation-persistence] seeded web terminal ring with ~a historical event(s).~%"
              (length seeded)))))

(setf *conversation-persistence-ready* t)
