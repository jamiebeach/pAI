;;;; immutable-source boot verifier.
;;;; Loaded before every other the agent source file in the disposable candidate.

(in-package :cl-user)

(load #P"/root/quicklisp/setup.lisp")
(funcall (intern "QUICKLOAD" :ql) :ironclad :silent t)

(defparameter *r3a-app-root* #P"/opt/pai/app/")
(defparameter *r3a-manifest-path*
  #P"/opt/pai/app/r3a-source-manifest.sexp")
(defparameter *r3a-manifest-seal-path*
  #P"/opt/pai/app/r3a-source-manifest.sha256")

(defun r3a-fail (control &rest arguments)
  (error "R3A_BOOT_REJECTED: ~?" control arguments))

(defun r3a-sha256 (path)
  (string-upcase
   (ironclad:byte-array-to-hex-string
    (ironclad:digest-file :sha256 path))))

(defun r3a-safe-relative-path-p (path)
  (and (stringp path)
       (> (length path) 0)
       (not (member (char path 0) '(#\/ #\\)))
       (not (find #\\ path))
       (not (member ".." (uiop:split-string path :separator '(#\/))
                    :test #'string=))
       (every (lambda (part) (> (length part) 0))
              (uiop:split-string path :separator '(#\/)))))

(defun r3a-read-manifest ()
  (unless (probe-file *r3a-manifest-path*)
    (r3a-fail "source manifest is absent"))
  (unless (probe-file *r3a-manifest-seal-path*)
    (r3a-fail "immutable manifest seal is absent"))
  (let* ((expected (string-trim '(#\Space #\Tab #\Return #\Newline)
                                (uiop:read-file-string *r3a-manifest-seal-path*)))
         (environment (sb-ext:posix-getenv "PAI_R3A_SOURCE_MANIFEST_SHA256")))
    (unless (and (= (length expected) 64)
                 (string= expected (r3a-sha256 *r3a-manifest-path*)))
      (r3a-fail "source manifest hash does not match sealed image metadata"))
    (unless (and environment (string= expected environment))
      (r3a-fail "runtime manifest hash differs from immutable image seal")))
  (let ((*read-eval* nil))
    (with-open-file (stream *r3a-manifest-path* :direction :input)
      (let ((value (read stream nil :eof)))
        (when (or (eq value :eof) (not (eq (read stream nil :eof) :eof)))
          (r3a-fail "source manifest must contain exactly one form"))
        value))))

(defun r3a-lisp-files (root)
  (labels ((walk (directory)
             (append
              (remove-if-not
               (lambda (path)
                 (string-equal (or (pathname-type path) "") "lisp"))
               (uiop:directory-files directory))
              (mapcan #'walk (uiop:subdirectories directory)))))
    (walk root)))

(defun r3a-relative-namestring (path)
  (uiop:unix-namestring (enough-namestring path *r3a-app-root*)))

(defun r3a-verify-source-manifest ()
  (let* ((manifest (r3a-read-manifest))
         (schema (getf manifest :schema-version))
         (files (getf manifest :files))
         (seen (make-hash-table :test #'equal))
         (declared-lisp (make-hash-table :test #'equal)))
    (unless (and (eql schema 1) (listp files) files)
      (r3a-fail "unsupported or empty source manifest"))
    (dolist (entry files)
      (let ((path (getf entry :path))
            (required (getf entry :required))
            (expected (getf entry :sha256)))
        (unless (r3a-safe-relative-path-p path)
          (r3a-fail "unsafe source path ~s" path))
        (when (gethash path seen)
          (r3a-fail "duplicate source path ~s" path))
        (setf (gethash path seen) t)
        (unless (and (stringp expected) (= (length expected) 64))
          (r3a-fail "invalid hash for ~s" path))
        (let ((absolute (merge-pathnames path *r3a-app-root*)))
          (cond
            ((probe-file absolute)
             (unless (string= expected (r3a-sha256 absolute))
               (r3a-fail "source hash mismatch for ~s" path)))
            (required (r3a-fail "required source is absent: ~s" path)))
          (when (string-equal (or (pathname-type absolute) "") "lisp")
            (setf (gethash path declared-lisp) t)))))
    (dolist (absolute (r3a-lisp-files *r3a-app-root*))
      (let ((relative (r3a-relative-namestring absolute)))
        (unless (gethash relative declared-lisp)
          (r3a-fail "undeclared executable source ~s" relative))))
    (format t "~&R3A_SOURCE_MANIFEST_OK schema=1 files=~d hash=~a~%"
            (length files)
            (string-trim '(#\Space #\Tab #\Return #\Newline)
                         (uiop:read-file-string *r3a-manifest-seal-path*)))
    t))

(define-init :verify boot-verifier-verify
    "Boot assertion for boot-verifier; fails closed."
  (r3a-verify-source-manifest))
