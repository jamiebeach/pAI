;;;; conscious-file-search-tool-tests.lisp -- bounded read-only capability.

(in-package :agent)

(ql:quickload '(:shasht) :silent t)

(defvar *cfst-pass* 0)
(defvar *cfst-fail* 0)

(defun cfst-check (name condition)
  (if condition
      (progn (incf *cfst-pass*) (format t "PASS ~a~%" name))
      (progn (incf *cfst-fail*) (format t "FAIL ~a~%" name))))

(defun cfst-signals-p (thunk)
  (handler-case (progn (funcall thunk) nil) (error () t)))

(format t "~%== bounded conscious file-search capability ==~%")

(let ((subject (merge-pathnames "src/tools/conscious-file-search-tool.lisp"
                                *pai-root*)))
  (cfst-check "file-search capability source exists" (probe-file subject))
  (when (probe-file subject)
    (load subject)
    (let* ((root (merge-pathnames "conscious-file-search/" (test-state-dir)))
           (nested (merge-pathnames "nested/" root)))
      (ensure-directories-exist nested)
      (with-open-file (stream (merge-pathnames "one.lisp" root)
                              :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
        (write-line "alpha needle omega" stream))
      (with-open-file (stream (merge-pathnames "two.md" nested)
                              :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
        (write-line "needle in nested evidence" stream)
        (write-line "another needle" stream))
      (with-open-file (stream (merge-pathnames "CLAUDE.md" root)
                              :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
        (write-line "This content deliberately omits the filename." stream))
      (conscious-file-search-configure root)
      (let* ((before-one (file-write-date (merge-pathnames "one.lisp" root)))
             (result (conscious-file-search
                      (obj "query" "needle" "path" "." "max_results" 2)))
             (matches (gethash "matches" result)))
        (cfst-check "literal search returns bounded nested matches"
                    (and (string= "ok" (gethash "status" result ""))
                         (= 2 (length matches))
                         (gethash "truncated" result)))
        (cfst-check "search reports zero durable writes"
                    (zerop (gethash "database_write_count" result -1)))
        (cfst-check "read-only search leaves fixture bytes unchanged"
                    (= before-one
                       (file-write-date (merge-pathnames "one.lisp" root)))))
      (cfst-check "traversal is rejected before search"
                  (cfst-signals-p
                   (lambda ()
                     (conscious-file-search
                     (obj "query" "needle" "path" "../" "max_results" 2)))))
      (let* ((result (conscious-file-search
                      (obj "query" "CLAUDE.md" "path" "." "max_results" 2)))
             (matches (gethash "matches" result)))
        (cfst-check "literal search discovers matching filenames"
                    (find-if
                     (lambda (match)
                       (and (string= "filename" (gethash "match_kind" match ""))
                            (string= "CLAUDE.md" (gethash "path" match ""))))
                     (coerce matches 'list))))
      (cfst-check "absolute path is rejected before search"
                  (cfst-signals-p
                   (lambda ()
                     (conscious-file-search
                      (obj "query" "needle" "path" (namestring root)
                           "max_results" 2)))))
      (cfst-check "unknown argument keys fail closed"
                  (cfst-signals-p
                   (lambda ()
                     (conscious-file-search
                      (obj "query" "needle" "path" "." "max_results" 2
                           "write" t))))))))

(format t "~%~d passed, ~d failed~%" *cfst-pass* *cfst-fail*)
(when (plusp *cfst-fail*) (error "conscious file-search tests failed"))
