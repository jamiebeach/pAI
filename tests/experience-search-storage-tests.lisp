(in-package :agent)
(ql:quickload '(:cffi :shasht :ironclad :bordeaux-threads) :silent t)
(load (test-source "storage-substrate.lisp"))
(load (test-source "sqlite-storage.lisp"))

(defvar *experience-search-pass* 0)
(defun experience-search-check (condition)
  (unless condition (error "Experience search assertion failed"))
  (incf *experience-search-pass*))

(let* ((path (merge-pathnames
              (format nil "experience-~a.sqlite3" (symbol-name (gensym)))
              (test-state-dir)))
       (backend nil)
       (at (encode-universal-time 0 0 12 21 9 2026 0)))
  (when (probe-file path) (delete-file path))
  (setf backend (make-sqlite-storage path))
  (unwind-protect
       (progn
         (dotimes (i 3)
           (storage-append-event backend "user-message"
                                 (%storage-object "text" (format nil "entry ~d" i))
                                 :agent-id "fixture-a"
                                 :occurred-at "2026-09-21T12:00:00Z"))
         (storage-append-event backend "user-message" (%storage-object "text" "other")
                               :agent-id "fixture-b"
                               :occurred-at "2026-09-21T12:00:00Z")
         (storage-append-event backend "model-request" (%storage-object "text" "internal")
                               :agent-id "fixture-a"
                               :occurred-at "2026-09-21T12:00:00Z")
         (multiple-value-bind (page cursor)
             (sqlite-experience-page backend "fixture-a" at at nil 2)
           (experience-search-check
            (equal '(3 2) (mapcar (lambda (event) (gethash "id" event)) page)))
           (experience-search-check (integerp cursor))
           (storage-append-event backend "user-message" (%storage-object "text" "late")
                                 :agent-id "fixture-a"
                                 :occurred-at "2026-09-21T12:00:00Z")
           (multiple-value-bind (rest next)
               (sqlite-experience-page backend "fixture-a" at at cursor 2)
             (experience-search-check
              (equal '(1) (mapcar (lambda (event) (gethash "id" event)) rest)))
             (experience-search-check (null next))))
         (experience-search-check
          (null (sqlite-experience-page backend "fixture-a" (1+ at) (+ at 10) nil 2)))
         (experience-search-check
          (handler-case
              (progn (sqlite-experience-page backend "fixture-a" at at nil 21) nil)
            (error () t))))
    (storage-close backend)))
(format t "~%~d passed, 0 failed~%" *experience-search-pass*)
