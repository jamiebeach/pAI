(in-package :agent)

(defvar *agent-loop-tool-message-test-pass* 0)
(defvar *agent-loop-tool-message-test-fail* 0)

(defun agent-loop-tool-message-test-check (name condition)
  (if condition
      (progn (incf *agent-loop-tool-message-test-pass*)
             (format t "~&PASS ~a~%" name))
      (progn (incf *agent-loop-tool-message-test-fail*)
             (format t "~&FAIL ~a~%" name))))

(load (test-source "agent_loop.lisp"))

(let* ((function (obj "name" "search-memory"
                      "arguments" "{\"query\":\"prior meeting\"}"
                      "provider_extension" "remove-me"))
       (tool-call (obj "index" 0 "id" "call-validation-1"
                       "type" "function" "function" function
                       "provider_extension" "remove-me"))
       (message (obj "role" "assistant" "content" :null
                     "refusal" :null "reasoning" "private"
                     "tool_calls" (vector tool-call)))
       (normalized (%assistant-tool-message-for-request message))
       (normalized-call (aref (gethash "tool_calls" normalized) 0))
       (normalized-function (gethash "function" normalized-call)))
  (agent-loop-tool-message-test-check
   "assistant request contains only request fields"
   (equal (sort (loop for key being the hash-keys of normalized collect key)
                #'string<)
          '("content" "role" "tool_calls")))
  (agent-loop-tool-message-test-check
   "response-only tool-call index is removed"
   (not (nth-value 1 (gethash "index" normalized-call))))
  (agent-loop-tool-message-test-check
   "tool call contains only request fields"
   (equal (sort (loop for key being the hash-keys of normalized-call collect key)
                #'string<)
          '("function" "id" "type")))
  (agent-loop-tool-message-test-check
   "function contains only name and arguments"
   (equal (sort (loop for key being the hash-keys of normalized-function collect key)
                #'string<)
          '("arguments" "name")))
  (agent-loop-tool-message-test-check
   "tool-call identity and arguments are preserved"
   (and (string= "call-validation-1" (gethash "id" normalized-call))
        (string= "search-memory" (gethash "name" normalized-function))
        (string= "{\"query\":\"prior meeting\"}"
                 (gethash "arguments" normalized-function))))
  (agent-loop-tool-message-test-check
   "normalization does not mutate provider response"
   (and (= 0 (gethash "index" tool-call))
        (string= "remove-me" (gethash "provider_extension" function))))
  (agent-loop-tool-message-test-check
   "normalization does not fabricate speech"
   (eq :null (gethash "content" normalized))))

(handler-case
    (progn
      (%provider-tool-call-for-request
       (obj "index" 0 "type" "function"
            "function" (obj "name" "search-memory" "arguments" "{}")))
      (agent-loop-tool-message-test-check
       "missing request-required tool-call ID fails closed" nil))
  (error ()
    (agent-loop-tool-message-test-check
     "missing request-required tool-call ID fails closed" t)))

(format t "~&agent-loop tool-message tests: ~d passed, ~d failed~%"
        *agent-loop-tool-message-test-pass* *agent-loop-tool-message-test-fail*)
(when (plusp *agent-loop-tool-message-test-fail*)
  (sb-ext:exit :code 1))
