;;;; Fast reader-only check. Does not evaluate or load the input files.
;;;; This complements, and never replaces, compilation and runtime fixtures.
(let ((failed nil))
  (dolist (path (cdr sb-ext:*posix-argv*))
    (handler-case
        (with-open-file (stream path :direction :input :external-format :utf-8)
          (let ((*read-suppress* t) (*read-eval* nil) (eof (gensym)) (count 0))
            (handler-case
                (loop until (eq eof (read stream nil eof)) do (incf count))
              (error (condition)
                (error "At byte ~d after ~d forms: ~a" (file-position stream) count condition)))
            (format t "READER-OK ~a (~d forms)~%" path count)))
      (error (condition)
        (setf failed t) (format t "READER-FAIL ~a: ~a~%" path condition))))
  (sb-ext:exit :code (if failed 1 0)))
