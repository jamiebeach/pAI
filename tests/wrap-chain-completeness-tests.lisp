;;;; wrap-chain-completeness-tests.lisp -- the registry must be complete.
;;;;
;;;; This codebase redefines functions across files on purpose: the
;;;; rename-and-fall-through wrap idiom layers behaviour without editing the
;;;; original. wrap-chain-registry.lisp records those chains, because
;;;; reloading one file mid-chain silently drops every wrap above it. That
;;;; registry's own header documents the idiom costing user-facing
;;;; functionality twice in a single day -- once leaving the agent unable to
;;;; generate an image while still advertising the tool.
;;;;
;;;; The registry was documentation, and documentation drifts. Nothing
;;;; checked it was complete, which is how one file came to redefine seven
;;;; functions from three others while appearing in it zero times.
;;;;
;;;; This test makes the invariant enforceable:
;;;;
;;;;   Every function defined in more than one file, within one package,
;;;;   must be declared -- either as a wrap chain in the registry, or in
;;;;   *ACCEPTED-REPLACEMENTS* below with a reason.
;;;;
;;;; Note what this deliberately does NOT do: it does not try to detect
;;;; whether a redefinition falls through to a saved original. An earlier
;;;; audit attempted that and produced nine false positives out of eleven,
;;;; because the saved original is sometimes named after the file rather
;;;; than the function (pai-base-lisp-eval-safety wrapping LISP-EVAL). A
;;;; heuristic you cannot trust is worse than a declaration you must write.
;;;; Declaring is cheap; guessing is not.
;;;;
;;;; Static source analysis only -- no system load, no services. Runnable in
;;;; CI. Point it at a source root:
;;;;
;;;;   sbcl --script tests/wrap-chain-completeness-tests.lisp [src-root]

(defvar *wcc-passed* 0)
(defvar *wcc-failed* 0)

(defun wcc-check (name condition)
  (if condition
      (progn (incf *wcc-passed*) (format t "PASS ~a~%" name))
      (progn (incf *wcc-failed*) (format t "FAIL ~a~%" name))))

;;; Redefinitions that are intentional replacements rather than wraps.
;;; Each entry needs a reason. An entry here is a decision on the record,
;;; not a way to silence the test.
(defparameter *accepted-replacements*
  '(("agent-loop"
     . "Self-modification by design: the agent's own rewritten loop in
        agent_loop.lisp supersedes the default in agent.lisp. Replacing it
        is the entire point of the self-modification pipeline.")
    ;; Both tick handlers below are captured into handler tables via #' at
    ;; load time, so the table holds a function OBJECT taken BEFORE the later
    ;; redefinition. Their earlier definitions are therefore live -- deleting
    ;; them broke the build, which is how the capture was found.
    ;;
    ;; OPEN QUESTION, recorded not resolved: if the table holds the earlier
    ;; object, the LATER redefinition may be unreachable through that path,
    ;; meaning the containment behaviour could be dead in practice. This is
    ;; exactly the ambiguity that makes the wrap idiom untenable -- you
    ;; cannot tell by reading which definition runs.
    ("%tick-handle-ruminate"
     . "Captured via #' in tick-loop's handler table; redefined later by
        feedback-loop-containment. See the note above.")
    ("%tick-handle-explore"
     . "Captured via #' in a handler table; defined in conversational-
        initiative and explore-novelty, redefined by
        feedback-loop-containment. See the note above.")
    ("call-model"
     . "agent.lisp (the true original), self-mod.lisp, enhancements.lisp and
        modulator.lisp each (defun call-model ...); all four are dead in
        production, superseded without delegation by agent_print.lisp's
        later plain redefinition -- found while converting call-model to a
        seam (P0c item 3, 2026-08-16). agent_print.lisp is now the seam's
        base (define-seam, not defun, so it does not appear in this scan);
        the remaining live layers are registered via register-layer. See
        wrap-chain-registry.lisp's call-model note for the full trace."))
  "Alist of function-name -> why replacement (not wrapping) is correct here.")

(defun %wcc-read-file (path)
  (with-open-file (s path :direction :input :external-format :utf-8)
    (let ((text (make-string (file-length s))))
      (subseq text 0 (read-sequence text s)))))

(defun %wcc-lines (text)
  (let ((lines '()) (start 0))
    (loop for i from 0 below (length text)
          when (char= (char text i) #\Newline)
            do (push (subseq text start i) lines) (setf start (1+ i)))
    (push (subseq text start) lines)
    (nreverse lines)))

(defun %wcc-token-after (line prefix)
  "If LINE starts with PREFIX, return the next whitespace-delimited token."
  (when (and (>= (length line) (length prefix))
             (string= prefix line :end2 (length prefix)))
    (let* ((rest (subseq line (length prefix)))
           (start (position-if-not (lambda (c) (member c '(#\Space #\Tab))) rest)))
      (when start
        (let ((end (or (position-if (lambda (c)
                                      (member c '(#\Space #\Tab #\( #\))))
                                    rest :start start)
                       (length rest))))
          (string-downcase (subseq rest start end)))))))

(defun %wcc-lisp-files (root)
  (remove-if-not
   (lambda (p) (equal (pathname-type p) "lisp"))
   (directory (merge-pathnames "**/*.*" (truename root)))))

(defun %wcc-scan (root)
  "Return (values defs packages) where DEFS maps name -> list of files."
  (let ((defs (make-hash-table :test #'equal))
        (pkgs (make-hash-table :test #'equal)))
    (dolist (path (%wcc-lisp-files root))
      (let ((name (file-namestring path))
            (pkg "?"))
        (dolist (line (%wcc-lines (%wcc-read-file path)))
          (let ((p (%wcc-token-after line "(in-package")))
            (when p (setf pkg (string-left-trim ":" p))))
          (let ((fn (%wcc-token-after line "(defun")))
            (when fn
              (push (cons name pkg) (gethash fn defs)))))
        (setf (gethash name pkgs) pkg)))
    defs))

(defun %wcc-registry-names (root)
  "Function names the wrap-chain registry declares, read as source text."
  (let ((path (merge-pathnames "kernel/wrap-chain-registry.lisp" (truename root)))
        (names '()))
    (when (probe-file path)
      (let ((text (%wcc-read-file path)) (i 0))
        ;; Chain keys are the (obj "name" ...) / bare "name" string literals
        ;; that precede a (vector ...) of file names.
        (loop while (setf i (search "(vector" text :start2 i))
              do (let* ((before (subseq text 0 i))
                        (q2 (position #\" before :from-end t)))
                   (when q2
                     (let ((q1 (position #\" before :from-end t :end q2)))
                       (when q1
                         (push (string-downcase (subseq before (1+ q1) q2)) names))))
                   (incf i 7)))))
    (remove-duplicates names :test #'string=)))

(defun run-wrap-chain-completeness (&optional (root "src/"))
  (let* ((defs (%wcc-scan root))
         (registry (%wcc-registry-names root))
         (undeclared '()))
    (maphash
     (lambda (fn entries)
       ;; Same-package redefinitions only. Identical names in different
       ;; packages are separate functions, not a chain.
       (let* ((by-pkg (make-hash-table :test #'equal)))
         (dolist (e entries) (push (car e) (gethash (cdr e) by-pkg)))
         (maphash
          (lambda (pkg files)
            (let ((unique (remove-duplicates files :test #'string=)))
              (when (> (length unique) 1)
                (unless (or (member fn registry :test #'string=)
                            (assoc fn *accepted-replacements* :test #'string=))
                  (push (list fn pkg (sort unique #'string<)) undeclared)))))
          by-pkg)))
     defs)
    (setf undeclared (sort undeclared #'string< :key #'first))
    (format t "~&scanned ~a; registry declares ~d chain(s); ~
               accepted replacements ~d~%"
            root (length registry) (length *accepted-replacements*))
    (when undeclared
      (format t "~&~%UNDECLARED multi-file redefinitions:~%")
      (dolist (u undeclared)
        (format t "  ~a  [~a]~%" (first u) (second u))
        (dolist (f (third u)) (format t "      ~a~%" f)))
      (format t "~&~%Each must either be added to wrap-chain-registry.lisp as a~%~
                 chain, or to *ACCEPTED-REPLACEMENTS* with a reason.~%~%"))
    (wcc-check "every same-package multi-file redefinition is declared"
               (null undeclared))
    (format t "~&WRAP-CHAIN COMPLETENESS: ~d passed, ~d failed.~%"
            *wcc-passed* *wcc-failed*)
    (null undeclared)))

;;; Script entry point.
;;; Entry point.
;;;
;;; Prefer the repository root the harness knows over argv. When run under
;;; run-all, argv[1] is run-all's OWN argument (the tests directory), which
;;; this would otherwise scan as if it were the source tree -- reporting the
;;; suites' shared helper names as undeclared redefinitions. argv is only
;;; meaningful when this file is the script being run.
(let* ((from-harness
         (and (find-package :agent)
              (let ((s (find-symbol "*PAI-ROOT*" :agent)))
                (and s (boundp s) (merge-pathnames "src/" (symbol-value s))))))
       (root (or from-harness (second sb-ext:*posix-argv*) "src/")))
  (unless (run-wrap-chain-completeness root)
    (sb-ext:quit :unix-status 1)))
