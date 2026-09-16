;;;; bootstrap-local-lisp.lisp -- install project-local Quicklisp dependencies.

(in-package :cl-user)

(require :asdf)

(defparameter *bootstrap-repo-root*
  (uiop:pathname-parent-directory-pathname
   (uiop:pathname-directory-pathname *load-truename*)))
(defparameter *bootstrap-tools-root*
  (merge-pathnames #P".tools/" *bootstrap-repo-root*))
(defparameter *bootstrap-quicklisp-root*
  (merge-pathnames #P"quicklisp/" *bootstrap-tools-root*))
(unless (probe-file (merge-pathnames #P"setup.lisp" *bootstrap-quicklisp-root*))
  (load (merge-pathnames #P"quicklisp.lisp" *bootstrap-tools-root*))
  (funcall (find-symbol "INSTALL" :quicklisp-quickstart)
           :path (namestring *bootstrap-quicklisp-root*)))

(load (merge-pathnames #P"setup.lisp" *bootstrap-quicklisp-root*))
(ql:quickload '(:dexador :shasht :hunchentoot :postmodern :local-time
                :ironclad :babel :bordeaux-threads)
              :silent nil)

(format t "~&PAI-LOCAL-LISP-READY~%")
(finish-output)
