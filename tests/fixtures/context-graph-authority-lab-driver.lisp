(defparameter *authority-lab-root* (truename (merge-pathnames "../../" (uiop:pathname-directory-pathname *load-truename*))))
(defun al-run (bundle)
  (uiop:call-with-temporary-file
   (lambda (input)
     (uiop:call-with-temporary-file (lambda (output) (al-run-files bundle input output)) :want-stream-p nil :type "json"))
   :want-stream-p nil :type "json"))
(defun al-run-files (bundle input output)
  (let* ((keys '("PAI_CONTEXT_GRAPH_BUNDLE" "PAI_CONTEXT_GRAPH_OUTPUT" "PAI_CONTEXT_GRAPH_ASD" "PAI_QUICKLISP_SETUP"))
         (prior (mapcar #'uiop:getenv keys)))
    (unwind-protect
         (progn
           (with-open-file (stream input :direction :output :if-exists :supersede :external-format :utf-8)
             (write-string (%cg-authority-canonical-json bundle) stream))
           (setf (uiop:getenv "PAI_CONTEXT_GRAPH_BUNDLE") (namestring input)
                 (uiop:getenv "PAI_CONTEXT_GRAPH_OUTPUT") (namestring output)
                 (uiop:getenv "PAI_CONTEXT_GRAPH_ASD") (namestring (merge-pathnames "pai-context-graph.asd" *authority-lab-root*))
                 (uiop:getenv "PAI_QUICKLISP_SETUP") (or (fourth prior) "/opt/quicklisp/setup.lisp"))
           (multiple-value-bind (log ignored exit-code)
               (uiop:run-program (list "sbcl" "--eval" "(setf *compile-verbose* nil *compile-print* nil)"
                                      "--eval" "(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))"
                                      "--script" (namestring (merge-pathnames "scripts/context-graph-lab.lisp" *authority-lab-root*)))
                                 :output :string :error-output :output :ignore-error-status t)
             (declare (ignore ignored))
             (unless (and (zerop exit-code) (search "CONTEXT-GRAPH-AUTHORITY-OK" log))
               (let ((start (or (search "Unhandled" log) (max 0 (- (length log) 1600)))))
                 (error "Lab subprocess failed (~d): ~a" exit-code (subseq log start (min (length log) (+ start 2400)))))))
           (let ((shasht:*read-default-true-value* :true) (shasht:*read-default-false-value* :false) (shasht:*read-default-null-value* :null))
             (shasht:read-json (uiop:read-file-string output :external-format :utf-8))))
      (loop for key in keys for value in prior do (setf (uiop:getenv key) (or value ""))))))
