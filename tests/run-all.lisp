;;;; run-all.lisp -- run the deterministic suites and report per file.
;;;;
;;;; Usage:
;;;;   sbcl --script tests/run-all.lisp [tests-dir]
;;;;
;;;; Reports each suite as PASS / FAIL / ERROR / SKIP with a reason, then a
;;;; summary. Per-file rather than aggregate on purpose: a single "37 failed"
;;;; is not actionable, and these suites were written against a container
;;;; layout so some will legitimately not run here yet. Distinguishing
;;;; "this broke" from "this needs a service" is the entire value.
;;;;
;;;; Exit status is non-zero only when a suite that RAN reported failures.
;;;; A suite that cannot run yet is reported, not silently counted as green
;;;; -- the whole point of running these is to find out what is actually
;;;; verified, and a runner that hides its own gaps would defeat that.

;; DEFECTIVE RUNNER -- deliberately disabled.
;;
;; The suites replace named runtime edges as part of their fixtures. Running
;; all of them in one Lisp image contaminates every later suite and produces a
;; large, convincing, false regression report. It can also leak this script's
;; argv into suites that interpret argv themselves. See docs/gotchas.md 7-9.
;;
;; Keep this entry point as an explicit diagnostic so old instructions and
;; automation fail visibly instead of silently returning misleading evidence.
(format *error-output*
        "~&DEFECTIVE TEST RUNNER: tests/run-all.lisp is disabled because shared-image suite execution produces false results.~%Use tests/run-isolated.sh so every suite runs in a fresh Lisp process.~%")
(sb-ext:exit :code 2)

(require :asdf)
(require :uiop)

;; --script skips the SBCL init file, so Quicklisp is not registered and the
;; system's :depends-on cannot resolve. Load it explicitly if present; if it
;; is absent the dependency failure below reports it plainly.
(let ((setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file setup) (load setup)))

(defvar *tests-dir*
  (or (second sb-ext:*posix-argv*) "/pai/tests/"))
(defvar *pai-root* (or (uiop:getenv "PAI_ROOT") #P"/pai/"))

(push (pathname *pai-root*) asdf:*central-registry*)

(format t "~&loading system...~%")
(handler-case
    (let ((*standard-output* (make-broadcast-stream))
          (*error-output* (make-broadcast-stream)))
      (asdf:load-system "pai"))
  (error (e)
    (format t "~&FATAL: system will not load: ~a~%" e)
    (sb-ext:quit :unix-status 2)))
(format t "system loaded.~%~%")

(load (merge-pathnames "helpers.lisp" *tests-dir*))

(defvar *results* '())

(defun classify (condition)
  "Distinguish a suite that cannot run here from one that is broken."
  (let ((text (format nil "~a" condition)))
    (cond
      ((search "does not exist under" text) :stale-reference)
      ((or (search "Connection refused" text)
           (search "getaddrinfo" text)
           (search "Database error" text)) :needs-database)
      ((search "does not exist" text) :missing-fixture)
      (t :error))))

;; Suites end with (uiop:quit 1) when they fail. QUIT is not a condition, so
;; HANDLER-CASE cannot see it and the first failing suite would take the
;; runner down with it -- reporting nothing about the 141 after it. Intercept
;; it for the duration of the run and turn it into a THROW we can catch.
;; Three spellings are in use across the suites: (uiop:quit N),
;; (sb-ext:exit :code N) and (sb-ext:quit :unix-status N). All three must be
;; trapped or the run stops at the first suite using an untrapped one -- which
;; is exactly what happened, silently, with exit status 0.
(defvar *real-quits*
  (list (cons 'uiop:quit   (fdefinition 'uiop:quit))
        (cons 'sb-ext:exit (fdefinition 'sb-ext:exit))
        (cons 'sb-ext:quit (fdefinition 'sb-ext:quit))))

(defun install-quit-trap ()
  ;; SB-EXT is a locked package; redefining EXIT/QUIT needs it unlocked.
  ;; Scoped to this runner process, which exists only to run tests.
  (ignore-errors (sb-ext:unlock-package (find-package :sb-ext)))
  (dolist (entry *real-quits*)
    (setf (fdefinition (car entry))
          (lambda (&rest args)
            (throw 'suite-quit
              (or (getf args :code) (getf args :unix-status) (first args) 0))))))

(defun remove-quit-trap ()
  (dolist (entry *real-quits*)
    (setf (fdefinition (car entry)) (cdr entry))))

(defun run-suite (path)
  (let* ((name (file-namestring path))
         (start (get-internal-real-time))
         (out (make-string-output-stream)))
    (flet ((elapsed ()
             (round (/ (- (get-internal-real-time) start)
                       (/ internal-time-units-per-second 1000)))))
      (handler-case
          (let ((quit-code
                  (catch 'suite-quit
                    (let ((*standard-output* out)
                          ;; Many suites re-declare the :agent package with a
                          ;; partial export list, which SBCL reports as package
                          ;; variance. A real smell, but not what is being
                          ;; measured here, and fatal under --script.
                          (sb-ext:*on-package-variance* '(:warn t))
                          (*error-output* (make-broadcast-stream)))
                      (load path))
                    nil)))
            (let* ((text (get-output-stream-string out))
                   (failed (or (and quit-code (not (eql quit-code 0)))
                               (search "FAIL " text)))
                   ;; Keep the failing assertion lines. The suites already
                   ;; print exactly what went wrong; discarding that and then
                   ;; reaching for tracing would be building machinery to
                   ;; reproduce information we were throwing away.
                   (detail (when failed
                             (with-output-to-string (s)
                               (with-input-from-string (in text)
                                 (loop for line = (read-line in nil nil)
                                       while line
                                       when (search "FAIL" line)
                                         do (format s "~a~%" line)))))))
              (push (list name (if failed :fail :pass) (elapsed) detail)
                    *results*)))
        (serious-condition (e)
          (push (list name (classify e) (elapsed) (format nil "~a" e))
                *results*))))))

(let ((suites (sort (remove-if
                     (lambda (p)
                       (member (file-namestring p)
                               '("run-all.lisp" "helpers.lisp")
                               :test #'string=))
                     (directory (merge-pathnames "*.lisp" *tests-dir*)))
                    #'string< :key #'file-namestring)))
  (format t "running ~d suite~:p~%~%" (length suites))
  (install-quit-trap)
  (unwind-protect (dolist (s suites) (run-suite s))
    (remove-quit-trap)))

(setf *results* (nreverse *results*))

(let ((pass 0) (fail 0) (stale 0) (db 0) (fixture 0) (err 0))
  (dolist (r *results*)
    (destructuring-bind (name status ms detail) r
      (declare (ignore ms))
      (case status
        (:pass (incf pass))
        (:fail (incf fail) (format t "FAIL           ~a~%~a" name (or detail "")))
        (:stale-reference (incf stale)
         (format t "STALE-REF      ~a~%" name))
        (:needs-database (incf db))
        (:missing-fixture (incf fixture)
         (format t "MISSING-FIXTURE ~a~%" name))
        (t (incf err)
           (format t "ERROR          ~a~%    ~a~%" name
                   (subseq detail 0 (min 160 (length detail))))))))
  (format t "~%~64,,,'-a~%" "")
  (format t "passed            ~4d~%" pass)
  (format t "failed            ~4d   (ran, reported failures)~%" fail)
  (format t "errored           ~4d   (did not complete)~%" err)
  (format t "stale reference   ~4d   (wants a source file that no longer exists)~%" stale)
  (format t "needs database    ~4d   (not runnable offline)~%" db)
  (format t "missing fixture   ~4d~%" fixture)
  (format t "~64,,,'-a~%" "")
  (format t "total             ~4d~%" (length *results*))
  (when (plusp (+ fail err)) (sb-ext:quit :unix-status 1)))
