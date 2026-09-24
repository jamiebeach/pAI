(in-package :agent)

(ql:quickload :bordeaux-threads :silent t)

(defvar *repl-rescue-passed* 0)
(defvar *repl-rescue-failed* 0)
(defvar *repl-rescue-cancel-reason* nil)
(defvar *self-mod-lock* (bt:make-lock "repl-rescue-test-lock"))

(defun repl-rescue-check (name condition)
  (if condition
      (progn (incf *repl-rescue-passed*) (format t "PASS ~a~%" name))
      (progn (incf *repl-rescue-failed*) (format t "FAIL ~a~%" name))))

(defun cancel-active-turn (reason)
  (setf *repl-rescue-cancel-reason* reason)
  :interrupt-sent)

(defun repl-rescue-write (path content)
  (with-open-file (out path :direction :output :if-exists :supersede
                            :if-does-not-exist :create
                            :external-format :utf-8)
    (write-string content out)))

(load (test-source "repl-drop.lisp"))
(repl-drop-stop)

(let* ((root
         (merge-pathnames
          (format nil "pai-repl-rescue-~a/" (random 1000000000))
          (test-state-dir)))
       (*repl-drop-dir* root)
       (ordinary (merge-pathnames "ordinary.lisp" root))
       (ordinary-result
         (make-pathname :directory (pathname-directory ordinary)
                        :name (file-namestring ordinary) :type "result"))
       (ordinary-done
         (make-pathname :directory (pathname-directory ordinary)
                        :name (file-namestring ordinary) :type "done"))
       (cancel (merge-pathnames "cancel-active-turn-fixture.request" root))
       (cancel-result (make-pathname :type "result" :defaults cancel))
       (cancel-done (make-pathname :type "done" :defaults cancel)))
  (dolist (candidate (list ordinary ordinary-result ordinary-done
                           cancel cancel-result cancel-done))
    (when (probe-file candidate) (delete-file candidate)))
  (ensure-directories-exist ordinary)
  (repl-rescue-write ordinary "(+ 1 2)")
  (setf *repl-rescue-cancel-reason* nil)
  (repl-rescue-write cancel "operator fixture")
  (let* ((locked nil)
        (release nil)
        (blocker
          (bt:make-thread
           (lambda ()
             (bt:with-lock-held (*self-mod-lock*)
               (setf locked t)
               (loop until release do (sleep 0.01))))
           :name "repl-rescue-lock-holder")))
    (loop repeat 100 until locked do (sleep 0.01))
    (%repl-drop-process-one ordinary)
    ;; The scan sees the structured request before retrying ordinary Lisp.
    (%repl-drop-scan)
    (setf release t)
    (bt:join-thread blocker))
  (repl-rescue-check "ordinary drop remains queued while turn lock is held"
                     (and (probe-file ordinary)
                          (not (probe-file ordinary-result))
                          (not (probe-file ordinary-done))))
  (repl-rescue-check "structured cancel request bypasses held turn lock"
                     (and (string= "operator fixture"
                                   *repl-rescue-cancel-reason*)
                          (probe-file cancel-result)
                          (probe-file cancel-done)
                          (search "INTERRUPT-SENT"
                                  (uiop:read-file-string cancel-result)
                                  :test #'char-equal)))
  (repl-rescue-check "cancel request body is data, not evaluated Lisp"
                     (not (probe-file cancel)))
  (%repl-drop-process-one ordinary)
  (repl-rescue-check "ordinary drop processes after lock becomes available"
                     (and (probe-file ordinary-result)
                          (probe-file ordinary-done)
                          (search "=> 3"
                                  (uiop:read-file-string ordinary-result))))
  )

(format t "~%REPL-DROP RESCUE TESTS: ~d passed, ~d failed.~%"
        *repl-rescue-passed* *repl-rescue-failed*)
(when (plusp *repl-rescue-failed*) (uiop:quit 1))
