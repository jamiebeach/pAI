;;;; recursive-primitive-tools-tests.lisp -- bounded primitive review facts.

(in-package :agent)

(defvar *recursive-primitive-test-passed* 0)
(defvar *recursive-primitive-test-failed* 0)
(defvar *eval-journal-file*)

(defun recursive-primitive-test-check (name condition)
  (if condition
      (progn
        (incf *recursive-primitive-test-passed*)
        (format t "PASS ~a~%" name))
      (progn
        (incf *recursive-primitive-test-failed*)
        (format t "FAIL ~a~%" name))))

(defun recursive-primitive-test-read-records (path)
  (with-open-file (stream path :direction :input :external-format :utf-8)
    (loop for line = (read-line stream nil nil)
          while line
          collect (shasht:read-json line))))

(format t "~%== recursive primitive tools ==~%")

(load (test-source "recursive-primitive-tools.lisp"))

(let* ((state-root
         (merge-pathnames
          (format nil "recursive-primitive-state-~d-~d/"
                  (get-universal-time) (random 1000000))
          (test-state-dir)))
       (journal
         (merge-pathnames
          (format nil "recursive-primitive-review-~d-~d.jsonl"
                  (get-universal-time) (random 1000000))
          (test-state-dir)))
       (arguments (obj "form" "(+ 20 22)"))
       (context (obj "thread_id" "thread:fixture"
                     "model_call_id" "model:fixture"
                     "tool_call_id" "tool:fixture"
                     "user_event_id" 17)))
  (unwind-protect
       (let ((*pai-state-root-cache* state-root))
         (ensure-directories-exist state-root)
         (load (test-source "eval-journal.lisp"))
         (recursive-primitive-test-check
          "inner eval journal resolves below the selected durable state root"
          (equal (namestring *eval-journal-file*)
                 (namestring (merge-pathnames "eval-journal.jsonl" state-root))))
         (let ((*recursive-primitive-lisp-eval-review-log* journal))
           (recursive-primitive-test-check
            "harmless lisp evaluation returns its bounded result"
            (string= "42"
                     (recursive-primitive-tool-execute
                      "lisp-eval" arguments context)))
           (let ((records (recursive-primitive-test-read-records journal)))
             (recursive-primitive-test-check
              "one evaluation writes exactly intent and completion"
              (and (= 2 (length records))
                   (equal '("intent" "completed")
                          (mapcar (lambda (record) (gethash "phase" record))
                                  records))))
             (recursive-primitive-test-check
              "review records preserve runtime correlation and exact form"
              (every
               (lambda (record)
                 (and (string= "thread:fixture" (gethash "thread_id" record))
                      (string= "model:fixture" (gethash "model_call_id" record))
                      (string= "tool:fixture" (gethash "tool_call_id" record))
                      (= 17 (gethash "user_event_id" record))
                      (string= "(+ 20 22)" (gethash "form" record))))
               records))
             (recursive-primitive-test-check
              "completed review record contains the observed outcome"
              (string= "42" (gethash "outcome" (second records))))))
         (let ((records
                 (recursive-primitive-test-read-records *eval-journal-file*)))
           (recursive-primitive-test-check
            "inner fail-closed journal records the form in durable state"
            (and (= 1 (length records))
                 (string= "lisp-eval" (gethash "kind" (first records)))
                 (string= "(+ 20 22)" (gethash "form" (first records))))))))
    (when (probe-file journal)
      (delete-file journal))
    (let ((inner (merge-pathnames "eval-journal.jsonl" state-root)))
      (when (probe-file inner)
        (delete-file inner))))

(format t "~%RECURSIVE PRIMITIVE TOOLS: ~d passed, ~d failed.~%"
        *recursive-primitive-test-passed*
        *recursive-primitive-test-failed*)
(when (plusp *recursive-primitive-test-failed*)
  (uiop:quit 1))
