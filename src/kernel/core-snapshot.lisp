;;;; core-snapshot.lisp -- core image snapshot.
;;;;
;;;; SAVE-LISP-AND-DIE requires exactly one thread alive at the moment
;;;; it's called, and it must be called BY that thread. The agent's main
;;;; thread is permanently blocked reading stdin (chat.lisp's read loop),
;;;; so there is no way to type a save command into it directly, and no
;;;; other thread can call SAVE-LISP-AND-DIE on the main thread's behalf.
;;;; The fix: SB-THREAD:INTERRUPT-THREAD injects a function call into a
;;;; target thread asynchronously, even while it's blocked in a syscall --
;;;; A5-SAVE-CORE (called from repl-drop, i.e. NOT the main thread) uses
;;;; it to make the main thread itself run the quiesce-and-save sequence.
;;;;
;;;; This consolidates what the backlog describes as separate
;;;; SB-EXT:*SAVE-HOOKS*/*INIT-HOOKS* registrations into one explicit
;;;; function on each side (A5-QUIESCE-AND-SAVE, RESUME) -- same intent
;;;; (quiesce before the dump, re-establish sockets/threads after), but a
;;;; single controlled entry point is easier to reason about and test than
;;;; logic scattered across global hook lists, for a codebase this size.
;;;;
;;;; IMPORTANT: SAVE-LISP-AND-DIE does not return in the process that
;;;; calls it -- once A5-QUIESCE-AND-SAVE reaches that call, THIS PROCESS
;;;; ENDS. The dumped executable becomes a new process when launched.
;;;; Test this file's mechanics on a disposable replica before ever
;;;; calling A5-SAVE-CORE against the real live image.

(in-package :agent)

(export '(resume a5-save-core))

(defparameter *a5-core-dir*
  (let ((root (or (uiop:getenv "PAI_ARTIFACT_ROOT")
                  (uiop:getenv "PAI_R3A_ARTIFACT_ROOT"))))
    (if (and root (plusp (length root)))
        (merge-pathnames "cores/" (pathname root))
        #P"/agent/state/cores/")))

(defun %a5-now-iso8601 ()
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~a-~2,'0d-~2,'0dT~2,'0d~2,'0d~2,'0dZ" year month day hour min sec)))

(defun %a5-stop-other-threads ()
  "Forcibly destroys every thread except the caller. Blunt, but this only
runs immediately before the process ends anyway -- no in-progress work on
any other thread matters after this point, only the heap state, which
DESTROY-THREAD doesn't touch."
  (let ((me (bt:current-thread)))
    (dolist (th (sb-thread:list-all-threads))
      (unless (eq th me)
        (ignore-errors (bt:destroy-thread th))))
    (loop repeat 100
          while (> (length (remove me (sb-thread:list-all-threads))) 0)
          do (sleep 0.1))))

(defun a5-quiesce-and-save (&optional core-path)
  "The actual save sequence. Must run on whichever thread ends up calling
SAVE-LISP-AND-DIE -- normally injected into the main thread via
SB-THREAD:INTERRUPT-THREAD (see A5-SAVE-CORE), since that's the only
thread SBCL will permit to remain once every other thread is stopped."
  (ensure-directories-exist *a5-core-dir*)
  (let ((path (or core-path
                  (merge-pathnames (format nil "agent-~a.core" (%a5-now-iso8601)) *a5-core-dir*))))
    ;; REPL-DROP-STOP first and separately from the blunt thread-destroy
    ;; below: it's the thread that's very likely mid-write on the exact
    ;; file that triggered this call (A5-SAVE-CORE returns almost
    ;; instantly after sending the interrupt, so the dropped file's
    ;; result-write + rename-to-.done is probably still in flight right
    ;; now). Graceful stop lets that finish; destroying it mid-rename left
    ;; a form to be silently reprocessed after resume during A.5 testing.
    (ignore-errors (when (fboundp 'repl-drop-stop) (repl-drop-stop)))
    (ignore-errors (when (fboundp 'stop-telegram) (stop-telegram)))
    (ignore-errors (when (fboundp 'stop-web) (stop-web)))
    (ignore-errors (when (fboundp 'workout-nudge-cancel) (workout-nudge-cancel)))
    (%a5-stop-other-threads)
    (sb-ext:gc :full t)
    (sb-ext:save-lisp-and-die (namestring path) :executable t :toplevel #'resume)))

(defun a5-save-core ()
  "Call this from repl-drop (never from the main thread itself, and never
from the agent's own lisp-eval). Locates the main thread and interrupts it to
run A5-QUIESCE-AND-SAVE on its behalf. Returns immediately after sending
the interrupt -- the actual save happens asynchronously on the main
thread, and this process ends when it completes."
  (let ((main (find "main thread" (sb-thread:list-all-threads)
                     :key #'bt:thread-name :test #'string=)))
    (unless main (error "could not find a thread named \"main thread\""))
    (sb-thread:interrupt-thread main #'a5-quiesce-and-save)
    :interrupt-sent))

(defun resume ()
  "Toplevel entry point for a resumed core/executable. Everything in the
heap -- every closure, every dynamic binding, *last-self-mod-history*,
*memory-graph*, agent-loop, all of it -- is already exactly as it was at
save time; nothing here reloads anything from disk. This only
re-establishes what genuinely cannot survive a heap dump: open sockets,
threads, timers."
  (format t "~&~%=== the agent resumed from core image ===~%")
  (ignore-errors (when (fboundp 'start-web) (start-web)))
  (ignore-errors (when (and (fboundp 'telegram-token) (telegram-token) (fboundp 'start-telegram))
                   (start-telegram)))
  (ignore-errors (when (fboundp 'repl-drop-start) (repl-drop-start)))
  (ignore-errors (when (fboundp 'workout-nudge-arm) (workout-nudge-arm)))
  (ignore-errors (when (fboundp 'pai-scheduler-start)
                   (pai-scheduler-start)))
  (format t "~&Resume complete.~%")
  (if (fboundp 'chat)
      (funcall 'chat :exit-on-quit t)
      (sb-ext:exit :code 0)))
