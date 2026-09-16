;;;; recursive-primitive-tools.lisp -- recursive tool leaves under OS authority.
;;;;
;;;; This module does not advertise itself or attest containment.  The
;;;; operator entry point may pass its executor to the recursive mind only
;;;; after an external launch profile has established the process authority.

(in-package :agent)

(defvar *recursive-primitive-workspace-root* nil)
(defvar *recursive-primitive-bash-executable* nil)
(defvar *recursive-primitive-lisp-eval-review-log* nil)
(defparameter *recursive-primitive-bash-timeout-seconds* 30)
(defparameter *recursive-primitive-lisp-timeout-seconds* 30)

(defun recursive-primitive-tools-configure
    (&key workspace-root bash-executable lisp-eval-review-log)
  (let ((root (and workspace-root (probe-file workspace-root)))
        (bash (and bash-executable (probe-file bash-executable)))
        (review (and lisp-eval-review-log (pathname lisp-eval-review-log))))
    (unless (and root (uiop:directory-pathname-p root))
      (error "Recursive primitive tools require an existing workspace directory"))
    (unless (and bash (not (uiop:directory-pathname-p bash)))
      (error "Recursive primitive tools require an exact Bash executable"))
    (unless review
      (error "Recursive Lisp evaluation requires a review journal path"))
    (setf *recursive-primitive-workspace-root* (truename root)
          *recursive-primitive-bash-executable* (truename bash)
          *recursive-primitive-lisp-eval-review-log* review)
    t))

(defun %recursive-primitive-review-text (value &optional (maximum 8192))
  (let ((text (if (stringp value) value (format nil "~s" value))))
    (if (<= (length text) maximum) text (subseq text 0 maximum))))

(defun %recursive-primitive-record-lisp-eval
    (phase form context &optional outcome)
  "Append one non-authoritative developer review fact. Failure is raised so a
live evaluation is never started without its durable-on-disk intent record."
  (unless *recursive-primitive-lisp-eval-review-log*
    (error "Lisp evaluation review journal is not configured"))
  (let ((record
          (obj "schema_version" 1
               "record_kind" "lisp-eval-review"
               "authority" "derived-from-recursive-tool-events"
               "phase" phase
               "recorded_at_universal_time" (get-universal-time)
               "thread_id" (gethash "thread_id" context "")
               "model_call_id" (gethash "model_call_id" context "")
               "tool_call_id" (gethash "tool_call_id" context "")
               "user_event_id" (gethash "user_event_id" context :null)
               "form" form)))
    (when outcome
      (setf (gethash "outcome" record)
            (%recursive-primitive-review-text outcome)))
    (ensure-directories-exist *recursive-primitive-lisp-eval-review-log*)
    (with-open-file (stream *recursive-primitive-lisp-eval-review-log*
                            :direction :output :if-exists :append
                            :if-does-not-exist :create
                            :external-format :utf-8)
      (let ((*print-pretty* nil))
        (shasht:write-json record stream))
      (terpri stream)
      (finish-output stream)))
  t)

(defun %recursive-primitive-bash (command)
  (unless (and *recursive-primitive-workspace-root*
               *recursive-primitive-bash-executable*)
    (error "Recursive Bash is not configured"))
  (handler-case
      (sb-ext:with-timeout *recursive-primitive-bash-timeout-seconds*
        (multiple-value-bind (stdout stderr exit-code)
            (uiop:run-program
             (list (namestring *recursive-primitive-bash-executable*)
                   "--noprofile" "--norc" "-c" command)
             :directory *recursive-primitive-workspace-root*
             :input nil :output :string :error-output :string
             :ignore-error-status t)
          (values (format nil "exit_code: ~d~%stdout:~%~a~%stderr:~%~a"
                          exit-code (or stdout "") (or stderr ""))
                  (obj "kind" "process-exit" "exit_code" exit-code))))
    (sb-ext:timeout ()
      (format nil "ERROR: bash exceeded the ~d-second execution timeout"
              *recursive-primitive-bash-timeout-seconds*))))

(defun recursive-primitive-tool-execute (name arguments &optional context)
  "Execute one already-validated primitive call inside current OS authority."
  (unless (hash-table-p arguments)
    (error "Recursive primitive arguments must be an object"))
  (cond
    ((string= name "lisp-eval")
     (unless (hash-table-p context)
       (error "Recursive Lisp evaluation requires runtime correlation"))
     (let ((form (gethash "form" arguments)))
       (%recursive-primitive-record-lisp-eval "intent" form context)
       (handler-case
           (let ((result
                   (sb-ext:with-timeout
                       *recursive-primitive-lisp-timeout-seconds*
                     (lisp-eval form))))
             (%recursive-primitive-record-lisp-eval
              "completed" form context result)
             result)
         (sb-ext:timeout ()
           (let ((result
                   (format nil
                           "ERROR: lisp-eval exceeded the ~d-second execution timeout"
                           *recursive-primitive-lisp-timeout-seconds*)))
             (%recursive-primitive-record-lisp-eval
              "timed-out" form context result)
             result)))))
    ((string= name "bash")
     (%recursive-primitive-bash (gethash "command" arguments)))
    ((string= name "brave-search")
     (unless (fboundp 'brave-search)
       (error "Brave Search adapter is unavailable"))
     (funcall 'brave-search (gethash "query" arguments)
              :count (gethash "count" arguments)))
    ((string= name "web-fetch")
     (unless (fboundp 'web-fetch)
       (error "Bounded web fetch adapter is unavailable"))
     (funcall 'web-fetch (gethash "url" arguments)))
    ((string= name "search-memory")
     (unless (fboundp 'search-memory)
       (error "Memory search adapter is unavailable"))
     (funcall 'search-memory (gethash "query" arguments)
              :limit (gethash "limit" arguments 3)))
    (t (error "Unknown recursive primitive tool ~a" name))))
