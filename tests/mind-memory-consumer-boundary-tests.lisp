(in-package :cl-user)

(defvar *mind-memory-consumer-boundary-pass* 0)
(defvar *mind-memory-consumer-boundary-fail* 0)

(defun mind-memory-consumer-boundary-check (name condition)
  (if condition
      (progn (incf *mind-memory-consumer-boundary-pass*)
             (format t "PASS ~a~%" name))
      (progn (incf *mind-memory-consumer-boundary-fail*)
             (format t "FAIL ~a~%" name))))

(defun mind-memory-consumer-boundary-text (path)
  (with-open-file (stream path :direction :input)
    (let ((text (make-string (file-length stream))))
      (read-sequence text stream)
      (string-upcase text))))

(defun mind-memory-consumer-boundary-absent-p (text token)
  (null (search (string-upcase token) text :test #'char=)))

(let ((shadow (mind-memory-consumer-boundary-text
               (namestring (test-source "memory-atom-shadow.lisp"))))
      (lab (mind-memory-consumer-boundary-text
            (namestring (test-source "lab.lisp")))))
  (dolist (token '("%MEMORY-ATOM-SAFE-ID-P"
                   "%MEMORY-ATOM-LIST"
                   "%MEMORY-ATOM-SHA256"
                   "%MEMORY-ATOM-MANIFEST-MAP"
                   "*MEMORY-ATOM-MAX-EVIDENCE*"))
    (mind-memory-consumer-boundary-check
     (format nil "shadow does not depend on private ~a" token)
     (mind-memory-consumer-boundary-absent-p shadow token)))
  (dolist (token '("%MEMORY-ATOM-EXACT-KEYS"
                   "*MEMORY-ATOM-MAX-ATOMS*"
                   "*MEMORY-ATOM-FORMS*"))
    (mind-memory-consumer-boundary-check
     (format nil "Lab does not depend on private ~a" token)
     (mind-memory-consumer-boundary-absent-p lab token)))
  (dolist (token '("MEMORY-ATOM-BUILD-MANIFEST"
                   "MEMORY-ATOM-BUILD-REQUEST"
                   "MEMORY-ATOM-VALIDATE-RESPONSE"))
    (mind-memory-consumer-boundary-check
     (format nil "shadow retains public semantic dependency ~a" token)
     (not (mind-memory-consumer-boundary-absent-p shadow token))))
  (mind-memory-consumer-boundary-check
   "Lab retains public manifest validation dependency"
   (not (mind-memory-consumer-boundary-absent-p
         lab "MEMORY-ATOM-BUILD-MANIFEST"))))

(format t "RESULT mind-memory-consumer-boundary: ~d passed, ~d failed~%"
        *mind-memory-consumer-boundary-pass*
        *mind-memory-consumer-boundary-fail*)
(when (plusp *mind-memory-consumer-boundary-fail*) (uiop:quit 1))
