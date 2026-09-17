(defpackage :agent
  (:use :cl))

(in-package :agent)

(defvar *reasoning-isolation-passed* 0)
(defvar *reasoning-isolation-failed* 0)

(defun reasoning-isolation-check (name condition)
  (if condition
      (progn (incf *reasoning-isolation-passed*) (format t "PASS ~a~%" name))
      (progn (incf *reasoning-isolation-failed*) (format t "FAIL ~a~%" name))))

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

(defun response (&key content reasoning reasoning-details tool-calls)
  (let ((message (obj "role" "assistant" "content" content)))
    (when reasoning (setf (gethash "reasoning" message) reasoning))
    (when reasoning-details
      (setf (gethash "reasoning_details" message) reasoning-details))
    (when tool-calls
      (setf (gethash "tool_calls" message) tool-calls))
    (obj "choices" (vector (obj "message" message "finish_reason" "stop")))))

;; Load the exact production forms under test without reading or triggering the
;; unrelated top-level behavior in enhancements.lisp. Restricting the
;; reader to this marked block also keeps the fixture independent of optional
;; packages referenced elsewhere in that file.
(let* ((source
         (with-open-file (stream (test-source "enhancements.lisp"))
           (let ((text (make-string (file-length stream))))
             (read-sequence text stream)
             text)))
       (start (search "(defparameter *reasoning-render-system-prompt*" source))
       (end (search ";;; First attempt at this fix" source :start2 start))
        (wanted '(*reasoning-render-system-prompt*
                  *reasoning-transcript-finalizer-system-prompt*
                  *reasoning-isolation-fail-closed-message*
                 public-response-unavailable
                  %reasoning-isolation-call-with-reasoning-disabled
                  %reasoning-isolation-usable-message-p
                  %reasoning-isolation-details-text
                 %reasoning-isolation-reasoning-source
                 %reasoning-isolation-duplicate-assistant-content-p
                  %reasoning-isolation-try-render
                  %reasoning-isolation-try-finalize-transcript
                 %reasoning-isolation-fix)))
  (assert (and start end (< start end)))
  (with-input-from-string (stream (subseq source start end))
    (loop for form = (read stream nil :eof)
          until (eq form :eof)
          for operator = (and (consp form) (first form))
          for name = (and (consp form) (second form))
          when (and (member operator '(defun defparameter define-condition) :test #'eq)
                    (member name wanted :test #'eq))
            do (eval form))))

(defvar *probe-responses* nil)
(defvar *probe-overrides* nil)
(defvar *probe-messages* nil)
(defvar *probe-tools* nil)
;; DEFPARAMETER, not DEFVAR: this is a stub, and the assertions below count
;; its entries. DEFVAR leaves an already-bound *TOOLS* alone, so under any
;; loader that pulls in the kernel first the suite silently measures the real
;; tool registry instead of this one-element stub and the count assertions
;; fail for a reason that has nothing to do with reasoning isolation.
(defparameter *tools* (vector (obj "type" "function")))

(defun pai-base-raw-call-model-reasoning-fallback (messages)
  (push (and (boundp '*call-model-reasoning-override*)
             (symbol-value '*call-model-reasoning-override*))
        *probe-overrides*)
  (push messages *probe-messages*)
  (push *tools* *probe-tools*)
  (or (pop *probe-responses*) (response :content nil)))

(defun reset-probe (&rest responses)
  (setf *probe-responses* responses
        *probe-overrides* nil
        *probe-messages* nil
        *probe-tools* nil))

(let* ((initial (response :content "ordinary answer"))
       (result (%reasoning-isolation-fix initial (list (obj "role" "user" "content" "hello")))))
  (reasoning-isolation-check "usable content passes through unchanged" (eq result initial))
  (reasoning-isolation-check "usable content makes no recovery call" (null *probe-overrides*)))

(let* ((tool-call (obj "id" "call-1" "function" (obj "name" "lisp-eval")))
       (initial (response :content nil :tool-calls (vector tool-call)))
       (result (%reasoning-isolation-fix initial (list (obj "role" "user" "content" "hello")))))
  (reasoning-isolation-check "null-content tool call passes through unchanged" (eq result initial))
  (reasoning-isolation-check "tool call passthrough makes no recovery call" (null *probe-overrides*)))

(reset-probe (response :content "recovered answer"))
(let* ((initial (response :content nil :reasoning "private draft"))
       (retry (first *probe-responses*))
       (result (%reasoning-isolation-fix initial (list (obj "role" "user" "content" "hello")))))
  (reasoning-isolation-check "no-reasoning retry response replaces empty response" (eq result retry))
  (reasoning-isolation-check "retry dynamically disables reasoning"
                             (equal '(:disabled) (reverse *probe-overrides*))))

(reset-probe
 (response :content "[09:12] Generic prior greeting")
 (response :content "[09:12] Personal answer grounded in last night"))
(let* ((messages
         (list (obj "role" "assistant"
                    "content" "[09:12] Generic prior greeting")
               (obj "role" "user" "content" "[09:12] how are you?")))
       (initial
         (response
          :content nil
          :reasoning
          "Private analysis followed by [09:12] Personal answer grounded in last night"))
       (result (%reasoning-isolation-fix initial messages)))
  (reasoning-isolation-check
   "exact stale assistant retry is rejected in favor of clean rendering"
   (and (eq result initial)
        (string= "[09:12] Personal answer grounded in last night"
                 (ref result "choices" 0 "message" "content"))))
  (reasoning-isolation-check
   "stale-retry recovery and renderer both disable reasoning"
   (equal '(:disabled :disabled) (reverse *probe-overrides*))))

(reset-probe (response :content "same short answer"))
(let* ((messages
         (list (obj "role" "assistant" "content" "same short answer")
               (obj "role" "user" "content" "repeat that")))
       (retry (first *probe-responses*))
       (result (%reasoning-isolation-fix (response :content nil) messages)))
  (reasoning-isolation-check
   "duplicate retry remains usable when no private reasoning answer exists"
   (eq result retry)))

(let ((tool-call (obj "id" "call-2" "function" (obj "name" "web-search"))))
  (reset-probe (response :content nil :tool-calls (vector tool-call)))
  (let* ((retry (first *probe-responses*))
         (result (%reasoning-isolation-fix
                  (response :content nil)
                  (list (obj "role" "user" "content" "search")))))
    (reasoning-isolation-check "reasoning-disabled retry may return a tool call"
                               (eq result retry))
    (reasoning-isolation-check "retry retains ordinary tool availability"
                               (= 1 (length (first *probe-tools*))))))

(let* ((details (vector
                 (obj "type" "reasoning.encrypted" "data" "opaque")
                 (obj "type" "reasoning.summary" "summary" "summary part")
                 (obj "type" "reasoning.text" "text" "text part")))
       (message (ref (response :content nil :reasoning-details details)
                     "choices" 0 "message")))
  (reasoning-isolation-check
   "structured reasoning text and summary are available for clean rendering"
   (string= (format nil "summary part~%text part")
            (%reasoning-isolation-details-text message)))
  (reasoning-isolation-check
   "encrypted reasoning detail is never treated as plaintext"
   (not (search "opaque" (%reasoning-isolation-details-text message)))))

;; A streamed response delivers one detail per delta chunk, all sharing one
;; type and index, each a continuation fragment carrying its own leading
;; space. Joining those by line split words mid-token ("work do" + "cket")
;; and corrupted every rendered reasoning string for streaming providers
;; that publish no plaintext REASONING alias.
(let* ((details (vector
                 (obj "type" "reasoning.text" "index" 0 "text" "the work do")
                 (obj "type" "reasoning.text" "index" 0 "text" "cket is")
                 (obj "type" "reasoning.text" "index" 0 "text" " waiting")))
       (message (ref (response :content nil :reasoning-details details)
                     "choices" 0 "message")))
  (reasoning-isolation-check
   "streamed reasoning fragments of one block are concatenated, not lined"
   (string= "the work docket is waiting"
            (%reasoning-isolation-details-text message))))

(let* ((details (vector
                 (obj "type" "reasoning.text" "index" 0 "text" "first")
                 (obj "type" "reasoning.text" "index" 0 "text" " block")
                 (obj "type" "reasoning.text" "index" 1 "text" "second")
                 (obj "type" "reasoning.text" "index" 1 "text" " block")))
       (message (ref (response :content nil :reasoning-details details)
                     "choices" 0 "message")))
  (reasoning-isolation-check
   "distinct reasoning blocks remain separated by one line"
   (string= (format nil "first block~%second block")
            (%reasoning-isolation-details-text message))))

(reset-probe
 (response :content nil
           :reasoning-details (vector (obj "type" "reasoning.text"
                                           "text" "retry intended answer")))
 (response :content "clean rendered answer"))
(let* ((initial (response :content nil))
       (result (%reasoning-isolation-fix initial (list (obj "role" "user" "content" "hello")))))
  (reasoning-isolation-check
   "reasoning_details can recover through the clean renderer"
   (string= "clean rendered answer" (ref result "choices" 0 "message" "content")))
  (reasoning-isolation-check
   "retry and renderer both disable reasoning"
   (equal '(:disabled :disabled) (reverse *probe-overrides*))))

(reset-probe (response :content nil)
             (response :content "grounded final answer"))
(let* ((messages
         (list (obj "role" "user" "content" "calculate")
               (obj "role" "assistant" "content" nil
                    "tool_calls" (vector (obj "id" "done-1")))
               (obj "role" "tool" "tool_call_id" "done-1" "content" "42")))
       (result (%reasoning-isolation-fix (response :content nil) messages))
       (finalizer-messages (first *probe-messages*)))
  (reasoning-isolation-check
   "encrypted-or-absent reasoning recovers from completed transcript"
   (string= "grounded final answer" (ref result "choices" 0 "message" "content")))
  (reasoning-isolation-check
   "transcript finalizer disables all tools"
   (zerop (length (first *probe-tools*))))
  (reasoning-isolation-check
   "transcript finalizer retains the completed tool result"
   (find "42" finalizer-messages :key (lambda (message) (gethash "content" message))
         :test #'equal))
  (reasoning-isolation-check
   "transcript finalizer has a private grounding instruction"
   (search "completed transcript"
           (gethash "content" (first finalizer-messages)))))

(reset-probe (response :content nil) (response :content "NO-CLEAN-REPLY-FOUND"))
(let ((signalled nil))
  (handler-case
      (%reasoning-isolation-fix
       (response :content nil)
       (list (obj "role" "user" "content" "hello")))
    (public-response-unavailable (condition) (setf signalled condition)))
  (reasoning-isolation-check
   "unrecoverable response signals a typed system error"
   signalled)
  (reasoning-isolation-check
   "unrecoverable response cannot become assistant content"
   (and (typep signalled 'public-response-unavailable)
        (search "no usable final answer"
                (public-response-unavailable-reason signalled)))))

(format t "~%Reasoning-isolation tests: ~d passed, ~d failed.~%"
        *reasoning-isolation-passed* *reasoning-isolation-failed*)
(when (plusp *reasoning-isolation-failed*)
  (sb-ext:exit :code 1))
(sb-ext:exit :code 0)
