;;;; agent_print.lisp -- CLI presentation layer + thinking log.
;;;;
;;;; Loaded AFTER self-mod.lisp / enhancements.lisp (see boot-verifier.lisp).
;;;;
;;;; What it does:
;;;;  - Thinking turns print to the console in MUTED GREY, truncated to
;;;;    *thinking-preview-chars* with a trailing "...". The FINAL answer
;;;;    prints in WHITE, in full, exactly ONCE (the thinking stream no
;;;;    longer echoes the final turn, so no double-print).
;;;;  - EVERYTHING (full, untruncated) is mirrored to *thinking-log-file*
;;;;    so you can hand a subsection to Claude Code / Codex / etc.
;;;;  - Privacy masking (secrets + user name -> placeholders) is folded
;;;;    back into call-model here, so the thinking log never writes real
;;;;    secrets to disk, and the wire stays masked.
;;;;  - The web path (stdout captured) stays PLAIN + FULL via *pai-capture-p*,
;;;;    so the browser's turn log is unaffected.

(in-package :agent)

(defparameter *esc* (code-char 27) "ANSI escape character.")
(defparameter *thinking-log-file*
  (let ((root (or (uiop:getenv "PAI_ARTIFACT_ROOT")
                  (uiop:getenv "PAI_R3A_ARTIFACT_ROOT"))))
    (merge-pathnames
     (if (and root (plusp (length root)))
         "private-diagnostics/pai-thinking.log"
         "pai-thinking.log")
     (if (and root (plusp (length root)))
         (pathname root)
         #P"/agent/state/")))
  "Full, untruncated thinking stream for hand-off to other tools.")
(defparameter *thinking-preview-chars* 500
  "Console preview length for a thinking turn before '...'.")
(defvar *pai-capture-p* nil
  "T when *standard-output* is being captured (web path) -- skip ANSI/truncation.")

(defun %now-stamp ()
  (if (fboundp 'pai-format-local-time)
      (funcall 'pai-format-local-time :style :iso)
      (multiple-value-bind (s m h d mo y)
          (decode-universal-time (get-universal-time) 0)
        (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0dZ"
                y mo d h m s))))

(defun pai-thinking-write (text)
  "Append TEXT (full) to the thinking log file."
  (handler-case
      (with-open-file (out *thinking-log-file* :direction :output
                           :if-exists :append :if-does-not-exist :create
                           :external-format :utf-8)
        (write-string text out))
    (error (e) (format t "~&[thinking-log-err ~a]~%" e)))
  (values))

(defun %grey (s) (format nil "~c[38;5;245m~a~c[0m" *esc* s *esc*))
(defun %white (s) (format nil "~c[97m~a~c[0m" *esc* s *esc*))
(defun %trunc (s n)
  (if (> (length s) n) (concatenate 'string (subseq s 0 n) "...") s))

(define-seam pai-turn-log (content turn final-p)
  "Full text -> thinking log. Console: grey+truncated for THINKING turns;
the FINAL turn is NOT printed here (the loop prints it white, once). The
content passed in is already masked (see call-model), so the file is safe."
  (pai-thinking-write (format nil "~&~%[~a] [agent, turn ~a] ~a~%"
                                (%now-stamp) turn content))
  (unless final-p
    (let ((s content))
      (unless *pai-capture-p*
        (setf s (%grey (%trunc s *thinking-preview-chars*))))
      (format t "~&~a" s)))
  (values))

;;; --- call-model: budget wrapper + privacy masking + thinking log ------
;;; Restores the masking that the base loop lost, and routes thinking text
;;; through pai-turn-log. The loop still calls (call-model ...), so the
;;; verifier's contract is unchanged.
;;;
;;; This is the seam's BASE, not merely another wrap, even though three
;;; earlier files (self-mod.lisp, enhancements.lisp, modulator.lisp) also
;;; each `(defun call-model ...)`. All three call through a saved original
;;; by symbol; this file instead calls RAW-CALL-MODEL directly and
;;; reimplements budget-checking, masking and logging itself -- found while
;;; converting this to a seam (P0c item 3) that in current production those
;;; three earlier definitions are consequently never reached: this file's
;;; plain DEFUN simply replaced whatever CALL-MODEL was bound to, the same
;;; way CONVERSATION-TURN-CAPTURE.LISP's wrap below replaces this one. Confirmed
;;; by tracing what each layer's saved-original variable actually captured at
;;; its own load time -- CONVERSATION-TURN-CAPTURE.LISP's *TURN-CAPTURE-INSTALLED-CALL-WRAPPER*
;;; capture (below) is this function, not self-mod/enhancements/modulator's.
;;; Registered here as a DEFINE-SEAM base rather than resurrecting the three
;;; orphaned layers as registered ones, which would change behaviour
;;; (modulator.lisp's resolution-level-driven temperature override is
;;; currently inert -- *CALL-MODEL-TEMPERATURE-OVERRIDE* is bound nowhere
;;; else) rather than merely declaring it. See docs/gotchas.md.
(define-seam call-model (messages)
  (when (<= *calls-remaining* 0)
    (error "call budget exhausted (~a turns)" *max-calls*))
  (decf *calls-remaining*)
  ;; --- masking (reuse helpers from pai-enhancements) ---
  (when (fboundp 'load-vault) (load-vault))
  (when (fboundp 'refresh-user-name) (refresh-user-name))
  (when (and (boundp '*brave-api-key*) (stringp *brave-api-key*)
             (> (length *brave-api-key*) 0)
             (fboundp 'get-or-create-mask))
    (get-or-create-mask *brave-api-key* "KEY"))
  (let* ((masked (if (fboundp 'mask-message)
                     (mapcar #'mask-message messages)
                     messages))
         (resp (raw-call-model masked))
         (message (ref resp "choices" 0 "message")))
    (when (and (present-p message) (fboundp 'unmask-message))
      (unmask-message message))
    (let* ((content (ref message "content"))
           (tool-calls (gethash "tool_calls" message))
           (final-p (not (and (present-p tool-calls)
                              (plusp (length tool-calls))))))
      (when (and (present-p content) (plusp (length content)))
        (pai-turn-log content (- *max-calls* *calls-remaining*) final-p)))
    resp))

;;; --- log-line (tool-call lines): grey+truncated+file; plain when captured.

(define-seam log-line (fmt &rest args)
  "Write one line of thinking output. Extension point: layers may observe
   or redirect it -- web-terminal broadcasts to connected clients."
  (let ((line (apply #'format nil fmt args)))
    (pai-thinking-write line)
    (let ((s line))
      (unless *pai-capture-p*
        (setf s (%grey (%trunc s *thinking-preview-chars*))))
      (format t "~&~a" s))
    (values)))

;;; --- end-of-run print: final answer WHITE, once (thinking already streamed).
;;;
;;; *self-mod-lock* guards against chat/web/telegram racing on the shared
;;; *last-self-mod-history* (Dockerfile runs all three at once). Using
;;; DEFVAR (not DEFPARAMETER) here is deliberate and is the actual bug fix:
;;; a prior version assumed self-mod.lisp already defined this lock, which
;;; it doesn't in this build, so BT:WITH-LOCK-HELD tried to read an unbound
;;; variable before the turn's HANDLER-CASE was even entered -- unrecoverable,
;;; straight into the debugger. DEFVAR only binds it if it isn't already
;;; bound, so this file is self-sufficient regardless of load order or what
;;; self-mod.lisp does or doesn't define.

(defvar *self-mod-lock* (bt:make-lock "self-mod-turn"))

(defun %run-self-mod-messages (messages)
  (bt:with-lock-held (*self-mod-lock*)
    (let ((*calls-remaining* *max-calls*)
          (*proposals-remaining* *max-proposals*)
          (*loop-snapshot* nil)
          (*current-user-request* (%last-user-content messages)))
      (snapshot-loop)
      (handler-case
          (let* ((history (agent-loop messages))
                 (final (gethash "content" (car (last history)))))
            (setf *last-self-mod-history* history)
            (when (and (present-p final) (plusp (length final)))
              (format t "~&~%~a"
                      (if *pai-capture-p* final (%white final))))
            final)
        (error (e)
          ;; Keep typed publication failures on the transport's system-error
          ;; path.  Treating one as the agent's final speech previously broadcast a
          ;; misleading supervisor message.  Preserve the already-received
          ;; inbound records, but do not invent or commit an assistant turn.
          ;; The outer conversation-persistence wrapper durably writes this
          ;; input-only history before re-signalling the typed failure.
          (when (and (find-class 'public-response-unavailable nil)
                     (typep e 'public-response-unavailable))
            (setf *last-self-mod-history* messages)
            (error e))
          (rollback-loop)
          (let ((msg (format nil "[supervisor] run aborted: ~a -- rolled back to last good loop." e)))
            (format t "~&~%~a~%"
                    (if *pai-capture-p* msg (%white msg)))
            msg))))))

;;; --- session delimiter in the thinking log so restarts are separable ----

(pai-thinking-write
 (format nil "~%~%===== the agent session started ~a (CLI presentation active) =====~%"
         (%now-stamp)))
