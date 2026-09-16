;;;; modulator-watchdog.lisp -- P8.8, 2026-07-29.
;;;;
;;;; Supersedes P2.5's original "hard-ceiling-on-magnitude" design
;;;; (already documented as superseded in the backlog itself). P2.5 asked
;;;; for hard bounds (already exist -- every modulator's own min/max,
;;;; MODULATOR-SET already clamps to them), a rate limiter on change per
;;;; unit time, and a watchdog that RESETS TO BASELINE and logs when
;;;; pinned too long. P8.8's whole argument: if large excursions are
;;;; where identity formation happens, capping the peak caps exactly
;;;; that -- so this file deliberately does NOT add a rate limiter and
;;;; does NOT reset anything. It only OBSERVES: tracks how long a
;;;; modulator has sat continuously within epsilon of its min/max, and
;;;; logs a real event if that persists past a threshold -- "the
;;;; watchdog fires on sustained pinning rather than on peak value."
;;;; Natural return-to-baseline is already handled by
;;;; MODULATOR-DECAY-TICK's existing exponential decay; nothing new
;;;; needed there. Cost during a genuine excursion (e.g. a burst of
;;;; ticks) is already bounded by the existing tick-budget governor
;;;; (P3.5, tick-loop.lisp's TICK-BUDGET-STATUS) -- not this file's job,
;;;; and deliberately not duplicated here.
;;;;
;;;; Load live (no restart) via lisp-eval or repl-drop, after
;;;; modulator.lisp (MODULATOR-DECAY-TICK, *MODULATORS*):
;;;;   (load "/agent/state/modulator-watchdog.lisp")

(in-package :agent)

(export '(modulator-watchdog-report))

(defparameter *modulator-pin-epsilon* 0.03
  "How close to min or max counts as 'pinned.'")
(defparameter *modulator-pin-window-seconds* (* 30 60)
  "How long a modulator must sit continuously pinned before the watchdog
fires -- a single intense excursion that resolves within this window
never triggers anything; that's the whole point.")

(defvar *modulator-pin-started-at* (make-hash-table :test #'equal)
  "Modulator name -> universal-time first observed pinned this episode.
Absent (no entry) means not currently pinned.")
(defvar *modulator-watchdog-fired* (make-hash-table :test #'equal)
  "Modulator name -> T once the watchdog has already fired for the
CURRENT pinning episode -- prevents re-logging every single decay tick
while a modulator stays pinned, without suppressing a genuinely new
episode later.")

(defun %modulator-pinned-p (name)
  (let* ((m (gethash name *modulators*)) (v (gethash "current" m))
         (mn (gethash "min" m)) (mx (gethash "max" m)))
    (or (<= (- v mn) *modulator-pin-epsilon*) (<= (- mx v) *modulator-pin-epsilon*))))

(defun modulator-watchdog-report ()
  "Which modulators are currently pinned and for how long -- for
introspection, and for testing without waiting out the real window."
  (let ((out nil) (now (get-universal-time)))
    (maphash (lambda (name started)
               (push (obj "modulator" name "pinned-seconds" (- now started)
                          "fired" (and (gethash name *modulator-watchdog-fired*) t))
                     out))
             *modulator-pin-started-at*)
    out))

(defun %modulator-watchdog-check ()
  (maphash
   (lambda (name m)
     (declare (ignore m))
     (if (%modulator-pinned-p name)
         (let ((started (gethash name *modulator-pin-started-at*)))
           (unless started
             (setf started (get-universal-time))
             (setf (gethash name *modulator-pin-started-at*) started))
           (when (and (>= (- (get-universal-time) started) *modulator-pin-window-seconds*)
                      (not (gethash name *modulator-watchdog-fired*)))
             (setf (gethash name *modulator-watchdog-fired*) t)
             (when (fboundp 'log-event)
               (ignore-errors
                (funcall 'log-event "modulator-watchdog-triggered"
                         (obj "modulator" name "value" (modulator-value name)
                              "pinned-seconds" (- (get-universal-time) started)))))))
         (progn (remhash name *modulator-pin-started-at*)
                (remhash name *modulator-watchdog-fired*))))
   *modulators*))

;;; --- wrap MODULATOR-DECAY-TICK, rename-and-fall-through -----------------
;;; Already runs every *MODULATOR-DECAY-INTERVAL-SECONDS* (60s) on its own
;;; background thread -- no new thread needed, just piggyback the check.

(unless (fboundp 'pai-base-modulator-decay-tick-watchdog)
  (setf (fdefinition 'pai-base-modulator-decay-tick-watchdog) (fdefinition 'modulator-decay-tick)))
(defun modulator-decay-tick ()
  (funcall 'pai-base-modulator-decay-tick-watchdog)
  (ignore-errors (%modulator-watchdog-check)))
