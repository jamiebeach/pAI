;;;; self-mod-sandbox.lisp -- P4.2's actual missing piece: a real
;;;; sandboxed compile-and-diagnose step, ahead of static-check/verifier/
;;;; install. 2026-07-28.
;;;;
;;;; Today PROPOSE-LOOP goes static-check -> verifier -> EVAL directly
;;;; into the live :AGENT package. Nothing captures real compiler
;;;; diagnostics before that install, or proves the form is even callable
;;;; in isolation first. This closes that gap with one addition: compile
;;;; the proposed form inside a throwaway package (mirroring :AGENT's
;;;; symbols) rather than the live package itself, catching genuine
;;;; compile-time errors before they ever reach real installation.
;;;;
;;;; Deliberately calibrated narrow, same lesson as the verifier fix:
;;;; only a genuine compile ERROR blocks promotion. WARNING/STYLE-WARNING
;;;; (e.g. "undefined function: X") is recorded for visibility but NEVER
;;;; rejects -- this codebase's own established cross-file forward-
;;;; reference idiom (a function calling another not yet loaded, safe at
;;;; runtime once it is, used throughout drives.lisp/spreading-
;;;; activation.lisp/tick-loop.lisp) triggers exactly that warning class
;;;; routinely and correctly. Treating it as blocking would reintroduce
;;;; the same reflexive over-rejection already fixed once this session.
;;;;
;;;; Does NOT implement the "smoke battery of representative agent
;;;; tasks" half of P4.2's literal deliverable -- deliberately deferred.
;;;; Actually running the proposed loop, even in a sandbox, means real
;;;; CALL-MODEL/EXECUTE calls (network requests, real tool side effects)
;;;; unless every one of those is mocked first, and a half-safe mock
;;;; harness built in a hurry is exactly the kind of thing that could
;;;; itself cause the unintended side effects this whole phase exists to
;;;; prevent. Left as an explicit, flagged gap rather than rushed.
;;;;
;;;; Two real bugs found and fixed during isolated testing, before this
;;;; ever reached a live process -- worth recording since both are subtle
;;;; enough to bite again elsewhere in this codebase:
;;;;
;;;;   1. self-mod.lisp's SAFE-READ hardcodes (*PACKAGE* (find-package
;;;;      :agent)) INSIDE ITSELF, ignoring any caller binding entirely.
;;;;      An earlier version of this file rebound *PACKAGE* around a
;;;;      call to SAFE-READ expecting it to intern symbols into the
;;;;      sandbox package -- it had NO effect. The AGENT-LOOP symbol in
;;;;      the read form was still the real, live AGENT::AGENT-LOOP the
;;;;      whole time, meaning the "sandbox" was silently redefining the
;;;;      live function directly. Fixed with %SANDBOX-SAFE-READ, a local
;;;;      reader with the same *READ-EVAL* NIL safety property but no
;;;;      hardcoded package.
;;;;
;;;;   2. FBOUNDP is not a reliable signal that compilation actually
;;;;      succeeded. Probed directly against the real SBCL in this image:
;;;;      on a genuinely malformed special-form use (a zero-argument
;;;;      (IF)), SBCL prints a fatal-looking diagnostic but still binds
;;;;      *something* callable to the function name -- a stub that only
;;;;      signals an error if actually INVOKED at runtime, so the rest of
;;;;      a file/form can keep loading. FBOUNDP was T in that case despite
;;;;      the real compile error. Fixed by using COMPILE's own standard
;;;;      (values function warnings-p failure-p) return -- FAILURE-P is
;;;;      the actual, documented signal for "the compiler encountered a
;;;;      real error", not a condition-catching approximation of it.
;;;;
;;;; Load live (no restart) via lisp-eval or repl-drop, AFTER
;;;; self-mod-phase4.lisp:
;;;;   (load "/agent/state/self-mod-sandbox.lisp")

(in-package :agent)

(defun %ensure-sandbox-package ()
  "A package that mirrors every symbol actually interned in :AGENT
(not just its exported ones -- CALL-MODEL/EXECUTE/PRESENT-P/REF etc. are
all internal, and :USE alone only pulls EXTERNAL symbols, which would
otherwise make every real proposal look like it references undefined
functions) except AGENT-LOOP itself, which is SHADOWed so a proposed
redefinition of it can never be the same symbol as the live
AGENT::AGENT-LOOP. Safe to call repeatedly -- re-importing an already-
accessible identical symbol is a no-op, not an error."
  (let ((pkg (or (find-package :agent-sandbox) (make-package :agent-sandbox :use '(:cl)))))
    (do-symbols (sym (find-package :agent))
      (when (and (eq (symbol-package sym) (find-package :agent))
                 (not (string= (symbol-name sym) "AGENT-LOOP")))
        (ignore-errors (import sym pkg))))
    (unless (find-symbol "AGENT-LOOP" pkg) (shadow 'agent-loop pkg))
    pkg))

(defun %sandbox-safe-read (src package)
  "Same safety property as self-mod.lisp's SAFE-READ (*READ-EVAL* NIL, so
a proposal can't execute code merely by being read) but interns symbols
into PACKAGE rather than SAFE-READ's hardcoded :AGENT -- see file header,
bug 1."
  (handler-case
      (values (let ((*read-eval* nil) (*package* package))
                (read-from-string src))
              nil)
    (error (e) (values nil e))))

(defun sandbox-compile-check (src)
  "Returns (values ok-p diagnostics-list). OK-P is false only on a
genuine ERROR (parse failure, wrong top-level form shape, or a real
compile-time FAILURE-P from COMPILE -- see file header, bug 2, for why
FBOUNDP alone isn't trustworthy here). WARNING/STYLE-WARNING is recorded
for visibility but never blocks -- expect one such note on essentially
every real proposal, harmlessly: AGENT-LOOP is compiled as an anonymous
LAMBDA (needed to get COMPILE's own FAILURE-P return value), so its own
recursive self-call always looks like a forward reference to an
undefined function at compile time, identical in kind to any other
genuine forward reference elsewhere in this codebase."
  (let ((sandbox-pkg (%ensure-sandbox-package)))
    (multiple-value-bind (form err) (%sandbox-safe-read src sandbox-pkg)
      (cond
        (err (values nil (list (format nil "does not parse: ~a" err))))
        ((not (and (consp form) (sym= (car form) "DEFUN") (sym= (cadr form) "AGENT-LOOP")))
         (values nil (list "must be a (defun agent-loop (messages) ...) form")))
        (t
         (let* ((*package* sandbox-pkg)
                (lambda-list (third form))
                (body (cdddr form))
                (lambda-form `(lambda ,lambda-list ,@body))
                (diagnostics nil))
           (handler-case
               (multiple-value-bind (fn warnings-p failure-p)
                   (handler-bind
                       ((warning (lambda (w)
                                   (push (format nil "note: ~a" w) diagnostics)
                                   (muffle-warning w))))
                     (compile nil lambda-form))
                 (declare (ignore fn warnings-p))
                 (values (not failure-p) (reverse diagnostics)))
             (error (e)
               (push (format nil "ERROR: ~a" e) diagnostics)
               (values nil (reverse diagnostics))))))))))

(unless (fboundp 'pai-base-propose-loop-p42)
  (setf (fdefinition 'pai-base-propose-loop-p42) (fdefinition 'propose-loop)))
(defun propose-loop (proposed-src)
  "Adds a sandboxed compile-check AHEAD of everything self-mod.lisp and
self-mod-phase4.lisp already do (static-check, verifier, whitelist,
install, journal) -- a proposal that fails to even compile cleanly is
rejected before spending a proposal-budget slot or reaching the verifier
at all, on genuinely mechanical, unambiguous grounds."
  (multiple-value-bind (compile-ok diagnostics) (sandbox-compile-check proposed-src)
    (if (not compile-ok)
        (format nil "REJECTED (sandbox compile): ~{~a~^; ~}" diagnostics)
        (funcall 'pai-base-propose-loop-p42 proposed-src))))
