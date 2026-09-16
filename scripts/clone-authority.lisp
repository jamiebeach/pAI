;;;; clone-authority.lisp -- diagnose the RUNTIME-TRUTH-ASSERT authority gate.
;;;;
;;;; RUNTIME-TRUTH-ASSERT compares, per seam, the declared effective authority
;;;; (from RUNTIME-AUTHORITY-REPORT) against the expected final owner (from the
;;;; runtime-truth manifest). It fails when they disagree, when the seam has no
;;;; declaration at all, or when more than one authority claims it.
;;;;
;;;; The failure message names only the seam, so this prints all three inputs
;;;; side by side.

(in-package :cl-user)
(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(push #P"/pai/" asdf:*central-registry*)
(let ((*standard-output* (make-broadcast-stream))) (asdf:load-system :pai))

(defun sym (name) (intern (string-upcase name) :agent))
(defun call (name &rest args)
  (let ((s (sym name))) (if (fboundp s) (apply s args) :not-defined)))

(funcall (sym "initialize") :phases '(:configure :install :restore)
                            :stop-on-error nil :verbose nil)

(let* ((truth (call "runtime-truth-manifest"))
       (owners (gethash "final_owners" truth))
       (report (call "runtime-authority-report"))
       (classes (and (hash-table-p report)
                     (gethash "decision_classes" report))))
  (format t "~&== final owners (runtime truth) ==~%")
  (loop for row across owners do
    (format t "~&  seam=~22a status=~24a expected=~a~%     loaded=~a~%"
            (gethash "seam" row) (gethash "status" row)
            (gethash "expected_final_owner" row)
            (gethash "loaded_final_owner" row)))

  (format t "~&~%== declared authorities ==~%")
  (if classes
      (loop for row across classes do
        (format t "~&  class=~22a authorities=~a~%"
                (gethash "decision_class" row)
                (coerce (gethash "effective_authorities" row) 'list)))
      (format t "~&  (no authority report)~%"))

  (format t "~&~%== per-seam verdict ==~%")
  (loop for row across owners
        for seam = (gethash "seam" row)
        for arow = (and classes
                        (find seam classes :test #'string=
                              :key (lambda (r) (gethash "decision_class" r))))
        for auth = (and arow (coerce (gethash "effective_authorities" arow) 'list))
        do (format t "~&  ~22a ~a~%" seam
                   (cond ((null arow) "NO AUTHORITY DECLARED")
                         ((/= 1 (length auth))
                          (format nil "~d authorities: ~a" (length auth) auth))
                         ((string= (first auth) (gethash "expected_final_owner" row))
                          "ok")
                         (t (format nil "declared=~a expected=~a"
                                    (first auth)
                                    (gethash "expected_final_owner" row)))))))

(format t "~&~%CLONE-AUTHORITY-DONE~%")
(finish-output)
