;;;; assets.lisp -- frontend files on disk, embedded at compile time.
;;;;
;;;; HTML, CSS and JavaScript used to live inside .lisp files as quoted
;;;; string literals -- roughly a thousand lines across three files. That
;;;; cost real things: no syntax highlighting, no linter, no formatter, no
;;;; way to open the markup in a browser; escaping hazards where a stray ~
;;;; in CSS becomes a FORMAT error inside something that reads as content;
;;;; and diffs that render a one-line style change as a modification to a
;;;; DEFPARAMETER. It also put a Common Lisp string-escaping lesson in front
;;;; of anyone who wanted to improve the UI, which is the most contributable
;;;; surface in the project.
;;;;
;;;; Assets are now ordinary files under an assets/ directory beside the
;;;; source that uses them, read at COMPILE time:
;;;;
;;;;   (defparameter *index-html* (asset "assets/index.html"))
;;;;
;;;; Compile time rather than runtime is deliberate. The standalone
;;;; executable is built with SAVE-LISP-AND-DIE and must be self-contained;
;;;; a runtime file read would mean shipping the assets alongside the binary
;;;; and adding a lookup that can be missing or stale in the field. Reading
;;;; during compilation gives editable files on disk AND a literal in the
;;;; image.
;;;;
;;;; ASDF must be told about the dependency (:static-file components), or an
;;;; edited asset silently will not take effect on rebuild -- the same class
;;;; of silent staleness this codebase has found repeatedly.

(in-package :agent)

(export '(asset asset-path *asset-root*))

(defvar *asset-root* nil
  "Optional development override. When bound to a directory, ASSET-AT
   re-reads from disk at call time so the UI can be iterated without
   recompiling.

   Defaults to NIL -- OFF -- on purpose. The compiled literal is the
   shipping path, and a default-on override would let the built image and
   the running system disagree about what the frontend is.")

(defun %asset-read (path)
  "Read PATH as a string, exactly, without interpreting its contents."
  (with-open-file (s path :direction :input :external-format :utf-8)
    (let* ((len (file-length s))
           (buf (make-string len))
           (n (read-sequence buf s)))
      (subseq buf 0 n))))

(defmacro asset (relative-path)
  "Embed the contents of RELATIVE-PATH, resolved against the compiling
   file, as a literal string at compile time."
  (let* ((base (or *compile-file-pathname* *load-pathname*))
         (full (merge-pathnames relative-path base)))
    (unless (probe-file full)
      (error "Asset not found at compile time: ~a~@
              (resolved from ~s relative to ~a)" full relative-path base))
    (%asset-read full)))

(defun asset-at (relative-path fallback)
  "FALLBACK is the compile-time literal. When *ASSET-ROOT* is set, re-read
   from disk instead -- development convenience only."
  (if *asset-root*
      (let ((p (merge-pathnames relative-path *asset-root*)))
        (if (probe-file p)
            (%asset-read p)
            fallback))
      fallback))
