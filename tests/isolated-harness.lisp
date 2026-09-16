;;;; isolated-harness.lisp -- load one suite the way it was written to run.
;;;;
;;;; The inherited suites each load their own subject and were qualified
;;;; standalone. Running them together in one image after loading the whole
;;;; system lets them redefine each other and hit gates that only exist
;;;; under full load -- which produced a false regression report once
;;;; already (docs/gotchas.md 7). One process per suite, minimal preload.
;;;;
;;;; Invoked by tests/run-isolated.sh with SUITE set.
;;;;
;;;; BARE MODE
;;;;
;;;; A suite may declare that it needs a clean image with no :agent package,
;;;; by containing the marker
;;;;
;;;;     ;;;; harness: bare
;;;;
;;;; Module-purity suites do exactly this: they assert (null (find-package
;;;; :agent)) to prove a module loads without the legacy catch-all package.
;;;; Preloading the framework silently defeats that -- the assertion fails
;;;; for a reason that has nothing to do with the module under test.
;;;;
;;;; FULL-SYSTEM MODE
;;;;
;;;; A suite whose subject is the production composition rather than one
;;;; isolated module may declare, in the same first forty lines:
;;;;
;;;;     ;;;; harness: full-system
;;;;
;;;; The harness then loads PAI through ASDF before loading the suite. This is
;;;; explicit and exceptional; it does not weaken isolation for other suites.

(in-package :cl-user)

(defvar *pai-root* (or (uiop:getenv "PAI_ROOT") #P"/pai/"))
(defvar *suite* (or (uiop:getenv "SUITE")
                    (namestring (merge-pathnames "tests/reciprocity-canary-tests.lisp"
                                                 *pai-root*))))

(defvar *src-index* nil)

(defun test-source (name)
  "Resolve a source file by basename, wherever it now lives under src/.
   Indexes every file type, not only .lisp -- suites also reference .sexp
   module manifests by path. A name containing a directory separator is
   resolved relative to src/ so callers can disambiguate duplicate basenames."
  (when (or (find #\/ name) (find #\\ name))
    (let ((path (merge-pathnames name (merge-pathnames "src/" *pai-root*))))
      (unless (probe-file path)
        (error "test-source: no source file at src/~a under ~a" name *pai-root*))
      (return-from test-source path)))
  (unless *src-index*
    (setf *src-index* (make-hash-table :test #'equalp))
    (dolist (root '("src/**/*.*" "templates/**/*.*"))
      (dolist (p (directory (merge-pathnames root *pai-root*)))
        (let* ((key (file-namestring p))
               (existing (gethash key *src-index*)))
          (setf (gethash key *src-index*)
                (if existing :ambiguous p))))))
  (let ((path (gethash name *src-index*)))
    (cond ((eq path :ambiguous)
           (error "test-source: ~a is ambiguous; use a path relative to src/"
                  name))
          (path path)
          (t (error "test-source: no source file named ~a under ~a"
                    name *pai-root*)))))

(defun test-state-dir ()
  "Writable scratch directory for suites that create files.

   Several suites CHDIR before loading their subject. Under the original
   layout that directory was the source root, which was also writable; here
   the repository is mounted read-only, so cwd has to be scratch instead.
   Nothing reads sources relative to cwd any more -- TEST-SOURCE resolves
   absolute paths -- so the only remaining job of cwd is to give relative
   writes somewhere legal to land."
  (let ((dir (pathname
              (concatenate 'string
                           (or (uiop:getenv "PAI_TEST_STATE") "/tmp/pai-test-state")
                           "/"))))
    (ensure-directories-exist dir)
    dir))

(defun %suite-marker-p (path marker)
  (with-open-file (s path :direction :input :external-format :utf-8)
    (loop for line = (read-line s nil nil)
          for n from 0 below 40
          while line
          thereis (search marker line))))

(let ((bare-p (%suite-marker-p *suite* "harness: bare"))
      (full-system-p (%suite-marker-p *suite* "harness: full-system")))
  (when (and bare-p full-system-p)
    (error "A suite cannot request both bare and full-system harness modes"))
  (cond
    (bare-p nil)
    (full-system-p
     ;; A full production load resolves a few adapter-owned paths at load
     ;; time. Point every such path at the suite's disposable state before
     ;; ASDF sees the system; never let a qualification load fall through to
     ;; the production /agent/state default.
     (let ((state (test-state-dir)))
       (setf (uiop:getenv "PAI_STATE_ROOT") (namestring state))
       (uiop:chdir state))
     ;; Qualification defines the image; it must not start the inherited
     ;; load-time heap sampler or append background events during a test.
     (load (merge-pathnames "src/kernel/agent.lisp" *pai-root*))
     (set (intern "*HEAP-HEALTH-AUTOSTART-P*" :agent) nil)
     (asdf:load-asd (merge-pathnames "pai.asd" *pai-root*))
     (asdf:load-system :pai))
    (t
     ;; Order matters: DEFINE-SEAM and DEFINE-INIT are macros and self-mod uses
     ;; them. Loading self-mod first parses the macro call as a function call
     ;; and its first argument as a variable -- "the variable LISP-EVAL is
     ;; unbound", which points nowhere near the cause. Same trap recorded in
     ;; pai.asd, and tripped again while writing this file.
     (load (merge-pathnames "src/kernel/agent.lisp" *pai-root*))
     ;; Several independently loaded storage/conversation modules name the
     ;; lower context-graph package at read time. Defining the package carries
     ;; no runtime behavior and is part of their minimal compile boundary.
     (load (merge-pathnames
            "src/mind/knowledge/context-graph/package.lisp" *pai-root*))
     (load (merge-pathnames "src/kernel/init.lisp" *pai-root*))
     (load (merge-pathnames "src/kernel/seams.lisp" *pai-root*))
     (load (merge-pathnames "src/kernel/assets.lisp" *pai-root*))
     (load (merge-pathnames "src/kernel/self-mod.lisp" *pai-root*))))
  (unless bare-p
    ;; Suites run in :agent and call these unqualified.
    (import 'test-source :agent)
    (import 'test-state-dir :agent)
    ;; A full production load already owns AGENT::*PAI-ROOT*. Keep that
    ;; canonical symbol and only give it the harness root; minimal-load suites
    ;; still import the helper binding as before.
    (if full-system-p
        (set (find-symbol "*PAI-ROOT*" :agent) *pai-root*)
        (import '*pai-root* :agent))))

(handler-case (load *suite*)
  (error (e) (format t "~%HARNESS-ERR: ~a~%" e)))
