;;;; repl-drop.lisp -- out-of-band REPL channel (, directory-
;;;; watcher variant, chosen over Swank: no new network exposure, no
;;;; client software needed, and usable directly through Claude's own
;;;; file/docker-exec tools without a Swank protocol client).
;;;;
;;;; Watches /agent/state/repl-drop/ for dropped .lisp files, evaluates
;;;; each one's forms in THIS live image's :agent package, and writes the
;;;; result to a companion .result file. This is deliberately independent
;;;; of the agent's own lisp-eval tool -- the whole point of A.0 is a channel
;;;; that doesn't route through the thing being rescued. It still
;;;; acquires *self-mod-lock*, the same lock CLI/web/Telegram/its own
;;;; turns already share, so an eval dropped here can't race a live turn
;;;; on shared state.
;;;;
;;;; Usage: write a .lisp file into /agent/state/repl-drop/, e.g.
;;;;   (+ 1 1)
;;;; saved as ping.lisp. Within *repl-drop-poll-seconds*, ping.lisp.result
;;;; appears with the captured output + return value, and ping.lisp is
;;;; renamed to ping.lisp.done. Nothing is ever silently reprocessed --
;;;; the .done files are the audit trail the backlog asks for.
;;;;
;;;; Load live, this ONE time, through the agent -- per the backlog's own
;;;; note, everything after A.0 lands should route through the channel
;;;; A.0 creates instead of back through its:
;;;;   (load "/agent/state/repl-drop.lisp")

(in-package :agent)

(defparameter *repl-drop-dir* #P"/agent/state/repl-drop/")
(defparameter *repl-drop-poll-seconds* 1)
(defvar *repl-drop-thread* nil)
(defvar *repl-drop-stop-requested* nil)

(defun %repl-drop-cancel-request-p (path)
  (let ((name (pathname-name path)))
    (and (stringp name)
         (search "cancel-active-turn-" name :test #'char-equal)
         (zerop (search "cancel-active-turn-" name :test #'char-equal)))))

(defun %repl-drop-process-cancel-request (path)
  "Process a structured cancellation request without *SELF-MOD-LOCK*.
The file body is a bounded operator reason, never Lisp source."
  (let* ((raw (string-trim '(#\Space #\Tab #\Newline #\Return)
                           (uiop:read-file-string path)))
         (reason (if (plusp (length raw))
                     (subseq raw 0 (min 160 (length raw)))
                     "repl-drop-operator-request"))
         (status (if (fboundp 'cancel-active-turn)
                     (funcall 'cancel-active-turn reason)
                     :cancellation-unavailable))
         (result-path (make-pathname :type "result" :defaults path))
         (done-path (make-pathname :type "done" :defaults path)))
    (with-open-file (out result-path :direction :output :if-exists :supersede
                         :if-does-not-exist :create :external-format :utf-8)
      (format out "~s~%" status))
    (uiop:rename-file-overwriting-target path done-path)
    (format t "~&[repl-drop] cancellation request ~a => ~a~%"
            (file-namestring path) status)))

(defun %repl-drop-eval-string (source)
  "Evaluate every top-level form in SOURCE in sequence, in :AGENT. Returns
a string: captured stdout, then the printed value(s) of the LAST form. On
error, names which form (by index, 1-based) failed and why -- forms
before it have already run and their side effects stand, same as LOAD."
  (let ((*package* (find-package :agent))
        (stream (make-string-input-stream source))
        (form-index 0)
        (last-value nil))
    (handler-case
        (let ((output
                (with-output-to-string (*standard-output*)
                  (loop
                    (let ((form (read stream nil :eof)))
                      (when (eq form :eof) (return))
                      (incf form-index)
                      (setf last-value (multiple-value-list (eval form))))))))
          (format nil "~a~%=> ~{~s~^ ~}" output last-value))
      (error (e)
        (format nil "ERROR (form ~a): ~a" form-index e)))))

(defun %repl-drop-process-one (path)
  ;; Never let the rescue watcher itself block behind a stuck public turn.
  ;; Leave ordinary Lisp drops queued and retry on the next scan.
  (when (bt:acquire-lock *self-mod-lock* nil)
    (unwind-protect
        (let* ((source (uiop:read-file-string path))
               (result (%repl-drop-eval-string source))
               (result-path
                 (make-pathname :directory (pathname-directory path)
                                :name (file-namestring path) :type "result"))
               (done-path
                 (make-pathname :directory (pathname-directory path)
                                :name (file-namestring path) :type "done")))
          (with-open-file (out result-path :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create
                               :external-format :utf-8)
            (write-string result out))
          (rename-file path done-path)
          (format t "~&[repl-drop] processed ~a~%" (file-namestring path)))
      (bt:release-lock *self-mod-lock*))))

(defun %repl-drop-scan ()
  (ensure-directories-exist *repl-drop-dir*)
  (dolist (path (directory (merge-pathnames "*.request" *repl-drop-dir*)))
    (when (%repl-drop-cancel-request-p path)
      (handler-case (%repl-drop-process-cancel-request path)
        (error (e)
          (format t "~&[repl-drop] cancellation request failed on ~a: ~a~%"
                  path e)))))
  (dolist (path (directory (merge-pathnames "*.lisp" *repl-drop-dir*)))
    (handler-case (%repl-drop-process-one path)
      (error (e) (format t "~&[repl-drop] failed on ~a: ~a~%" path e)))))

(defun repl-drop-start ()
  "Idempotent: safe to call again after reloading this file -- won't spawn
a second watcher if one is already alive."
  (unless (and *repl-drop-thread* (bt:thread-alive-p *repl-drop-thread*))
    (ensure-directories-exist *repl-drop-dir*)
    (setf *repl-drop-stop-requested* nil)
    (setf *repl-drop-thread*
          (bt:make-thread
           (lambda ()
             (loop until *repl-drop-stop-requested*
                   do (handler-case (%repl-drop-scan)
                        (error (e) (format t "~&[repl-drop] scan error: ~a~%" e)))
                      (sleep *repl-drop-poll-seconds*)))
           :name "repl-drop-watcher")))
  (format t "~&[repl-drop] watching ~a (poll every ~as).~%" *repl-drop-dir* *repl-drop-poll-seconds*))

(defun repl-drop-stop (&optional (timeout 5))
  "Gracefully stop the watcher: request stop, then wait (up to TIMEOUT
seconds) for the thread to actually exit on its own. Critical for A.5 --
the watcher only checks the stop flag between scans, never mid-file, so
whatever it's currently processing (write .result, rename to .done)
always finishes cleanly first. A bare BT:DESTROY-THREAD can catch it
mid-rename, which is exactly what let a dropped form get silently
reprocessed after a resume during A.5 testing."
  (setf *repl-drop-stop-requested* t)
  (loop with deadline = (+ (get-internal-real-time) (* timeout internal-time-units-per-second))
        while (and *repl-drop-thread* (bt:thread-alive-p *repl-drop-thread*)
                   (< (get-internal-real-time) deadline))
        do (sleep 0.05))
  (if (and *repl-drop-thread* (bt:thread-alive-p *repl-drop-thread*))
      (progn (ignore-errors (bt:destroy-thread *repl-drop-thread*)) :force-killed)
      :stopped-cleanly))

(define-init :start repl-drop-start
    "Start background worker for repl-drop."
  (repl-drop-start))
