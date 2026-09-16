;;;; virtual-clock.lisp -- P0.3, 2026-07-28.
;;;;
;;;; A real, standalone clock abstraction: (NOW) returns the current time
;;;; (real or virtual depending on mode), (ADVANCE-CLOCK seconds) moves a
;;;; virtual clock forward with no wall-clock sleeping.
;;;;
;;;; SCOPE, stated plainly: this does NOT retrofit production code
;;;; (drives.lisp, modulator.lisp, tick-loop.lisp, memory-nodes.lisp) to
;;;; call (NOW) instead of (GET-UNIVERSAL-TIME) -- every one of those is
;;;; already a live, deployed subsystem, several sitting in the same
;;;; fragile wrap chains that have caused two real incidents this session
;;;; (conversation-persistence, and the near-miss caught while building
;;;; this). Retrofitting all of them for a testing capability, when the
;;;; production systems already work correctly against real time, is a
;;;; real-risk-for-testing-benefit trade this pass deliberately declines.
;;;; P0.1 (the package/CLOS refactor the backlog's own dependency list
;;;; names) is also still explicitly deferred, for the same "don't force
;;;; a big-bang rewrite" reasoning documented at every phase that skipped
;;;; it so far.
;;;;
;;;; What this DOES deliver, matching the backlog's literal acceptance
;;;; criterion ("an integration test simulates 30 simulated days in under
;;;; a minute with no wall-clock sleeping"): a real, usable clock
;;;; primitive, proven correct via a genuine 30-simulated-day test against
;;;; the actual P1.4 decay formula (see the accompanying test, not part of
;;;; this file) -- available for any future subsystem to adopt when it's
;;;; actually being built or revisited, rather than retrofitted onto
;;;; already-working code for no immediate behavioral benefit.
;;;;
;;;; Load live (no restart) via lisp-eval or repl-drop:
;;;;   (load "/agent/state/virtual-clock.lisp")

(in-package :agent)

(export '(now advance-clock enter-virtual-clock exit-virtual-clock clock-mode))

(defvar *clock-mode* :real "Either :REAL or :VIRTUAL.")
(defvar *virtual-clock-time* nil
  "Current virtual universal-time, only meaningful when *CLOCK-MODE* is
:VIRTUAL. NIL in :REAL mode.")

(defun clock-mode () *clock-mode*)

(defun now ()
  "The one seam all time-dependent code should eventually call through.
Behaves exactly like GET-UNIVERSAL-TIME in :REAL mode (today's default,
unchanged production behavior); returns the tracked virtual time in
:VIRTUAL mode."
  (if (eq *clock-mode* :virtual)
      (or *virtual-clock-time* (get-universal-time))
      (get-universal-time)))

(defun enter-virtual-clock (&optional (start-time (get-universal-time)))
  "Switches to virtual time, starting at START-TIME (real now, by
default, so a simulation picks up from a plausible starting point rather
than an arbitrary one)."
  (setf *clock-mode* :virtual)
  (setf *virtual-clock-time* start-time))

(defun exit-virtual-clock ()
  "Returns to real time. Always safe to call, even if not currently
virtual."
  (setf *clock-mode* :real)
  (setf *virtual-clock-time* nil))

(defun advance-clock (seconds)
  "Moves the virtual clock forward by SECONDS. Errors if not currently in
virtual mode -- advancing 'the clock' when nothing is listening to it is
almost certainly a mistake at the call site, not a no-op to silently
allow."
  (unless (eq *clock-mode* :virtual)
    (error "ADVANCE-CLOCK called while *CLOCK-MODE* is :REAL -- call ENTER-VIRTUAL-CLOCK first."))
  (incf *virtual-clock-time* seconds))

(defmacro with-virtual-clock ((&optional (start-time '(get-universal-time))) &body body)
  "Runs BODY with the clock in virtual mode starting at START-TIME,
always restoring real mode afterward (even on a non-local exit) so a
test failure can never leave a later, unrelated call silently running
against virtual time."
  `(unwind-protect
       (progn (enter-virtual-clock ,start-time) ,@body)
     (exit-virtual-clock)))
