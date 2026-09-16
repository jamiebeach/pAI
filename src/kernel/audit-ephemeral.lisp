;;;; audit-ephemeral.lisp -- ephemeral definition audit.
;;;;
;;;; Read-only. Walks every symbol in :AGENT (present-symbols: internal +
;;;; external), and for each fbound function, boundp non-constant
;;;; variable, and defined class, resolves its definition source via
;;;; SB-INTROSPECT and classifies it:
;;;;   :on-disk   -- source file recorded and present on disk
;;;;   :stale     -- source file recorded but missing
;;;;   :ephemeral -- no source location at all (eval'd into existence,
;;;;                 e.g. via propose-loop or a live lisp-eval, with
;;;;                 nothing on disk to rebuild it from)
;;;;
;;;; Writes both an s-expr (for programmatic use in A.3) and a
;;;; human-readable .md report to /agent/state/audit/, timestamped.
;;;;
;;;; Run via the A.0 repl-drop channel, not through the agent's own eval seam
;;;; -- this file's only job when dropped is to call RUN-EPHEMERAL-AUDIT.

(in-package :agent)

(require :sb-introspect)

(defparameter *audit-dir*
  (let ((root (or (uiop:getenv "PAI_ARTIFACT_ROOT")
                  (uiop:getenv "PAI_R3A_ARTIFACT_ROOT"))))
    (if (and root (plusp (length root)))
        (merge-pathnames "runtime-audit/" (pathname root))
        #P"/agent/state/audit/")))

(defun %source-classify (kind sym)
  "Returns (values classification path) for SYM's KIND (:function
:variable :class)."
  (let* ((src (ignore-errors (first (sb-introspect:find-definition-sources-by-name sym kind))))
         (path (and src (ignore-errors (sb-introspect:definition-source-pathname src)))))
    (cond
      ((null src) (values :ephemeral nil))
      ((null path) (values :ephemeral nil))
      ((probe-file path) (values :on-disk path))
      (t (values :stale path)))))

(defun %truncate-print (value &optional (max 200))
  (let ((s (handler-case (format nil "~s" value) (error (e) (format nil "<unprintable: ~a>" e)))))
    (if (> (length s) max) (concatenate 'string (subseq s 0 max) "...") s)))

(defun run-ephemeral-audit (&optional (package :agent))
  (ensure-directories-exist *audit-dir*)
  (let ((functions nil) (variables nil) (classes nil))
    (loop for sym being the present-symbols of (find-package package)
          do (progn
               (when (fboundp sym)
                 (multiple-value-bind (classification path) (%source-classify :function sym)
                   (push (list :symbol sym
                               :package (package-name (symbol-package sym))
                               :classification classification
                               :path (and path (namestring path))
                               :arglist (ignore-errors
                                         (sb-introspect:function-lambda-list
                                          (if (fboundp sym) (symbol-function sym) nil)))
                               :docstring (ignore-errors (documentation sym 'function)))
                         functions)))
               (when (and (boundp sym) (not (keywordp sym)) (not (constantp sym)))
                 (multiple-value-bind (classification path) (%source-classify :variable sym)
                   (push (list :symbol sym
                               :package (package-name (symbol-package sym))
                               :classification classification
                               :path (and path (namestring path))
                               :value-preview (%truncate-print (symbol-value sym)))
                         variables)))
               (let ((cls (find-class sym nil)))
                 (when cls
                   (multiple-value-bind (classification path) (%source-classify :class sym)
                     (push (list :symbol sym
                                 :package (package-name (symbol-package sym))
                                 :classification classification
                                 :path (and path (namestring path)))
                           classes))))))
    (setf functions (nreverse functions) variables (nreverse variables) classes (nreverse classes))
    (let* ((stamp (format nil "~a" (get-universal-time)))
           (sexp-path (merge-pathnames (format nil "ephemeral-audit-~a.sexp" stamp) *audit-dir*))
           (report-path (merge-pathnames (format nil "ephemeral-audit-~a.md" stamp) *audit-dir*)))
      (with-open-file (out sexp-path :direction :output :if-exists :supersede :external-format :utf-8)
        (let ((*print-pretty* t) (*print-length* nil) (*print-level* nil))
          (prin1 (list :functions functions :variables variables :classes classes) out)))
      (with-open-file (out report-path :direction :output :if-exists :supersede :external-format :utf-8)
        (format out "# Ephemeral Definition Audit -- ~a~%~%" stamp)
        (flet ((section (title items)
                 (format out "## ~a (~a)~%~%" title (length items))
                 (dolist (item (sort (copy-list items) #'string<
                                      :key (lambda (i) (string (getf i :symbol)))))
                   (format out "- **~a** [~a] ~a~%"
                           (getf item :symbol) (getf item :classification)
                           (or (getf item :path) "(no source location)")))
                 (format out "~%")))
          (section "Functions" functions)
          (section "Variables" variables)
          (section "Classes" classes)))
      (let ((summary
              (list :functions-total (length functions)
                    :functions-ephemeral (count :ephemeral functions :key (lambda (i) (getf i :classification)))
                    :functions-stale (count :stale functions :key (lambda (i) (getf i :classification)))
                    :variables-total (length variables)
                    :variables-ephemeral (count :ephemeral variables :key (lambda (i) (getf i :classification)))
                    :variables-stale (count :stale variables :key (lambda (i) (getf i :classification)))
                    :classes-total (length classes)
                    :classes-ephemeral (count :ephemeral classes :key (lambda (i) (getf i :classification)))
                    :sexp-path (namestring sexp-path)
                    :report-path (namestring report-path))))
        (format t "~&~a~%" summary)
        summary))))

(define-init :verify audit-ephemeral-verify
    "Boot assertion for audit-ephemeral; fails closed."
  (run-ephemeral-audit))
