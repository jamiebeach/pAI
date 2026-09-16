;;;; helpers.lisp -- shared test support.
;;;;
;;;; The suites were written to run inside a container where every source
;;;; file sat flat in one directory, so each test bootstraps its subject
;;;; with an absolute path: (load (test-source "event-log.lisp")). There
;;;; are 394 such references across 134 files, and the restratification
;;;; moved every one of them.
;;;;
;;;; Rather than rewrite 394 absolute paths into 394 different relative
;;;; ones -- which would break again at the next move -- tests now ask for a
;;;; file by name and TEST-SOURCE finds it:
;;;;
;;;;   (load (test-source "event-log.lisp"))
;;;;
;;;; Resolution is by basename across the source tree, which is safe because
;;;; basenames are unique there (the file-layer map was built on that
;;;; assumption and a duplicate would be a mapping error worth failing on).

(in-package :agent)

(export (list (quote test-source) (quote test-state-path)
              (quote *pai-root*)))

(defvar *pai-root*
  (or (uiop:getenv "PAI_ROOT")
      #P"/pai/")
  "Repository root. Overridable so the suites can run against a checkout
   anywhere, rather than only the path they were originally written for.")

(defvar *pai-source-index* nil
  "basename -> full path, built once on first use.")

(defun %index-sources ()
  (let ((index (make-hash-table :test #'equalp)))
    (dolist (path (directory (merge-pathnames "src/**/*.*" *pai-root*)))
      (let ((name (file-namestring path)))
        (when (gethash name index)
          (warn "Duplicate source basename ~a -- test resolution is ambiguous."
                name))
        (setf (gethash name index) path)))
    index))

(defun test-source (name)
  "Full path of source file NAME, wherever it now lives under src/."
  (unless *pai-source-index*
    (setf *pai-source-index* (%index-sources)))
  (or (gethash name *pai-source-index*)
      (error "Test wants source file ~s, which does not exist under ~a.~@
              Either the file was renamed or removed, or the test is stale."
             name *pai-root*)))

(defun test-state-path (name)
  "Path for a runtime state file a test wants to read or write.

   Tests originally used /agent/state/ directly. That directory belongs to
   a running instance, not to a test run, so this resolves under a
   disposable root instead -- a test that writes into real instance state
   is a test that can corrupt it."
  (merge-pathnames name
                   (or (uiop:getenv "PAI_TEST_STATE")
                       #P"/tmp/pai-test-state/")))

;; A handful of suites never enter the :agent package and call these from
;; CL-USER. Import rather than duplicate, so there is one definition.
(import '(test-source test-state-path) :cl-user)
