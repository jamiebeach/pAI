(defpackage :agent (:use :cl))
(in-package :agent)

(ql:quickload '(:dexador :shasht) :silent t)

(defvar *runware-test-pass* 0)
(defvar *runware-test-fail* 0)
(defun runware-check (name condition)
  (if condition
      (progn (incf *runware-test-pass*) (format t "PASS ~a~%" name))
      (progn (incf *runware-test-fail*) (format t "FAIL ~a~%" name))))
(defun obj (&rest kvs)
  (loop with table = (make-hash-table :test #'equal)
        for (key value) on kvs by #'cddr
        do (setf (gethash key table) value)
        finally (return table)))
(defun ref (table &rest keys)
  (reduce (lambda (value key)
            (etypecase key
              (string (gethash key value))
              (integer (aref value key))))
          keys :initial-value table))
(defun present-p (value) (not (or (null value) (eq value :null))))
(defvar *tools* (vector))
(defvar *http-connect-timeout* 1)
(defun execute (tool-call) (declare (ignore tool-call)) nil)
(defun raw-call-model (messages) (declare (ignore messages)) (obj))

(load (test-source "runware.lisp"))

(let* ((task (%runware-image-task "a sunny portrait" *runware-default-model*
                                  1024 1024 1 nil))
       (json (shasht:write-json (vector task) nil)))
  (runware-check "native task stores nested SHASHT JSON-false sentinel"
                 (eq :false (gethash "checkContent" (gethash "safety" task))))
  (runware-check "native task serializes safety.checkContent as false, not null"
                 (let ((field (search "\"checkContent\"" json)))
                   (and field
                        (search "false" json :start2 field)
                        (not (search "null" json :start2 field))))))

(let* ((root #P"/tmp/pai-runware-contract/")
       (*state-file-root* root))
  (ensure-directories-exist (merge-pathnames "uploads/portrait.jpg" root))
  (with-open-file (stream (merge-pathnames "uploads/portrait.jpg" root)
                          :direction :output :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string "fixture" stream))
  (let* ((encoded (find-state-files :subdirectory "uploads" :extension "jpg"))
         (report (shasht:read-json encoded))
         (files (gethash "files" report)))
    (runware-check "bounded discovery finds an uploaded reference"
                   (and (= 1 (length files))
                        (search "uploads/portrait.jpg"
                                (gethash "relative_path" (aref files 0)))))
    (runware-check "bounded discovery reports file metadata"
                   (= 7 (gethash "bytes" (aref files 0)))))
  (runware-check "bounded discovery rejects parent traversal"
                 (search "ERROR:" (find-state-files :subdirectory "../")))
  (ensure-directories-exist (merge-pathnames "captured-functions/internal.lisp" root))
  (with-open-file (stream (merge-pathnames "captured-functions/internal.lisp" root)
                          :direction :output :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string "internal" stream))
  (ensure-directories-exist (merge-pathnames "deliverables/public.md" root))
  (with-open-file (stream (merge-pathnames "deliverables/public.md" root)
                          :direction :output :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string "public" stream))
  (let ((root-result (find-state-files :max-depth 2 :max-results 20)))
    (runware-check "default root search excludes operational archives"
                   (not (search "captured-functions" root-result)))
    (runware-check "default root search retains artifact directories"
                   (search "deliverables/public.md" root-result)))
  (runware-check "operational archive remains available when explicitly named"
                 (search "captured-functions/internal.lisp"
                         (find-state-files :subdirectory "captured-functions"
                                           :max-depth 0)))
  (let ((original (fdefinition 'uiop:directory-files)))
    (unwind-protect
         (progn
           (setf (fdefinition 'uiop:directory-files)
                 (lambda (&rest arguments)
                   (declare (ignore arguments)) (sleep 0.05) nil))
           (let ((*state-file-search-timeout-seconds* 0.01))
             (runware-check "state discovery has a hard elapsed-time failure"
                            (search "exceeded the 0.01-second timeout"
                                    (find-state-files
                                     :subdirectory "uploads")))))
      (setf (fdefinition 'uiop:directory-files) original))))

(runware-check "file-discovery tool is advertised"
               (find "find-state-files" *tools*
                     :key (lambda (tool) (ref tool "function" "name"))
                     :test #'string=))

(format t "~%RUNWARE CONTRACT: ~d passed, ~d failed.~%"
        *runware-test-pass* *runware-test-fail*)
(when (plusp *runware-test-fail*) (uiop:quit 1))
