(defpackage :agent (:use :cl))
(in-package :agent)
(ql:quickload '(:bordeaux-threads :shasht) :silent t)

(defvar *llm-debug-test-pass* 0)
(defvar *llm-debug-test-fail* 0)
(defun obj (&rest kvs)
  (loop with table = (make-hash-table :test #'equal)
        for (key value) on kvs by #'cddr
        do (setf (gethash key table) value)
        finally (return table)))
(defun llm-debug-test-check (name condition)
  (if condition
      (progn (incf *llm-debug-test-pass*) (format t "PASS ~a~%" name))
      (progn (incf *llm-debug-test-fail*) (format t "FAIL ~a~%" name))))

(load (test-source "llm-debug-capture.lisp"))

(llm-debug-test-check "trace mode parser defaults to off"
                      (eq :off (%llm-debug-capture-parse-mode nil)))
(llm-debug-test-check "trace mode parser accepts metadata"
                      (eq :metadata
                          (%llm-debug-capture-parse-mode "metadata")))
(llm-debug-test-check "trace mode parser accepts full"
                      (eq :full (%llm-debug-capture-parse-mode "full")))
(llm-debug-test-check
 "trace mode parser rejects ambiguous values"
 (handler-case (progn (%llm-debug-capture-parse-mode "verbose") nil)
   (error () t)))

(let* ((root (pathname (format nil "/tmp/pai-llm-debug-~d/"
                               (get-universal-time))))
       (*llm-debug-capture-directory* root)
       (*llm-debug-capture-current-file* nil)
       (*llm-debug-capture-sequence* 0)
       (*llm-debug-capture-max-file-bytes* 102400)
       (*llm-debug-capture-max-line-chars* 35000)
       (secret (concatenate 'string "sk-" "or-fixture-secret-value"))
       (messages (list (obj "role" "system" "content" "private context")
                       (obj "role" "user" "content" secret))))
  (let ((*llm-debug-capture-mode* :off))
    (llm-debug-test-check
     "disabled capture is transparent"
     (string= "ok" (llm-debug-capture-call messages "public"
                                            (lambda () "ok"))))
    (llm-debug-test-check "disabled capture writes nothing"
                          (zerop (length (llm-debug-capture-index))))
    (llm-debug-test-check
     "disabled disk capture still remembers latest public request in memory"
     (let ((snapshot (llm-debug-current-public-context)))
       (and (string= "available" (gethash "status" snapshot))
            (= 2 (length (gethash "messages" snapshot)))))))

  (let ((*llm-debug-capture-mode* :metadata))
    (llm-debug-capture-call
     messages "public" (lambda () (obj "choices" (vector)))
     :metadata (obj "model_call_id" "call:metadata"
                    "context_composition_hash" "hash:metadata")))
  (let ((*llm-debug-capture-mode* :full))
    (let ((response
            (llm-debug-capture-call
             messages "public"
             (lambda ()
               (let ((observer
                       (symbol-value
                        (intern
                         "*CONSCIOUS-CONVERSATION-PROVIDER-REQUEST-OBSERVER*"
                         :agent))))
                 (funcall observer
                          (obj "model" "fixture-model"
                               "messages" (coerce messages 'vector)
                               "temperature" 0.25d0
                               "api_key" secret)))
               (obj "choices" (vector (obj "content" "reply"))
                    "api_key" secret))
             :metadata (obj "model_call_id" "call:full"
                            "context_composition_hash" "hash:full"))))
      (llm-debug-test-check "enabled capture preserves exact return object"
                            (string= "reply"
                                     (gethash "content"
                                              (aref (gethash "choices" response) 0))))))
  (let* ((index (llm-debug-capture-index))
         (name (gethash "name" (aref index 0)))
         (private (llm-debug-capture-read name :reveal t))
         (metadata (llm-debug-capture-read name :reveal nil))
         (private-json (shasht:write-json private nil))
         (metadata-row
           (find "metadata" private :key (lambda (row)
                                            (gethash "capture_mode" row ""))
                                    :test #'string=))
         (full-row
           (find "full" private :key (lambda (row)
                                        (gethash "capture_mode" row ""))
                                :test #'string=)))
    (llm-debug-test-check "capture creates an indexed shard" (= 1 (length index)))
    (llm-debug-test-check
     "metadata mode records linkage without private request or response"
     (and (hash-table-p metadata-row)
          (null (gethash "messages" metadata-row))
          (null (gethash "response" metadata-row))
          (string= "call:metadata"
                   (gethash "model_call_id"
                            (gethash "request_metadata" metadata-row)))))
    (llm-debug-test-check
     "full mode records the final request and linked response"
     (and (hash-table-p full-row)
          (= 2 (length (gethash "messages" full-row)))
          (string= "fixture-model"
                   (gethash "model" (gethash "request" full-row)))
          (hash-table-p (gethash "response" full-row))
          (string= "call:full"
                   (gethash "model_call_id"
                            (gethash "request_metadata" full-row)))))
    (llm-debug-test-check "credential-shaped strings are redacted"
                          (and (search "[REDACTED]" private-json)
                               (null (search secret private-json))))
    (llm-debug-test-check "sensitive response fields are redacted"
                          (null (search "fixture-secret-value" private-json)))
    (llm-debug-test-check "metadata view omits private request, messages and response"
                          (let ((row (aref metadata 0)))
                            (and (null (gethash "request" row))
                                 (null (gethash "messages" row))
                                 (null (gethash "response" row)))))
    (llm-debug-test-check
     "path traversal is rejected"
     (handler-case (progn (llm-debug-capture-read "../memory.json" :reveal t)
                          nil)
       (error () t))))

  (let ((*llm-debug-capture-mode* :on)
        (*llm-debug-capture-max-file-bytes* 1200)
        (*llm-debug-capture-max-line-chars* 500))
    (dotimes (index 8)
      (llm-debug-capture-call
       (list (obj "role" "user" "content"
                  (make-string 240 :initial-element (code-char (+ 65 index)))))
       "rotation-test" (lambda () (obj "result" "ok"))))
    (llm-debug-capture-call
     (list (obj "role" "system" "content"
                (make-string 900 :initial-element #\S))
           (obj "role" "user" "content" "correlated request"))
     "correlation-test"
     (lambda ()
       (funcall
        (symbol-value
         (intern "*CONSCIOUS-CONVERSATION-PROVIDER-REQUEST-OBSERVER*" :agent))
        (obj "model" "chunked-model"
             "messages" (vector (obj "role" "user"
                                      "content" "correlated request"))))
       (obj "result" "correlated response"))
     :metadata (obj "model_call_id" "model:fixture:chunked"))
    (let ((files (llm-debug-capture-index)))
      (llm-debug-test-check "small configured limit rotates shards"
                            (> (length files) 1))
      (llm-debug-test-check "rotated shards stay within configured limit"
                            (every (lambda (row) (<= (gethash "bytes" row) 1200))
                                   (coerce files 'list))))
    (let ((found (llm-debug-capture-find-model-call
                  "model:fixture:chunked" :reveal t))
          (metadata-only (llm-debug-capture-find-model-call
                          "model:fixture:chunked" :reveal nil)))
      (llm-debug-test-check
       "exact model-call lookup reassembles a capture across rotated shards"
       (and (= 2 (length (gethash "messages" found)))
            (string= "chunked-model"
                     (gethash "model" (gethash "request" found)))
            (string= "correlated response"
                     (gethash "result" (gethash "response" found)))))
      (llm-debug-test-check
       "exact model-call metadata lookup keeps private bodies closed"
       (and (null (gethash "request" metadata-only))
            (null (gethash "messages" metadata-only))
            (null (gethash "response" metadata-only))))))

  ;; FAT/Windows timestamp granularity can round a just-written file up by
  ;; two seconds. Put the synthetic cutoff safely beyond that resolution.
  (let ((*llm-debug-capture-retention-seconds* -10))
    (llm-debug-capture-prune)
    (llm-debug-test-check "retention prunes only expired capture shards"
                          (zerop (length (llm-debug-capture-index)))))

  (let ((*llm-debug-capture-mode* :on)
        (*llm-debug-capture-directory* #p"/dev/null/not-a-directory/")
        (*llm-debug-capture-current-file* nil))
    (llm-debug-test-check
     "capture write failure cannot fail the model call"
     (string= "still-available"
              (llm-debug-capture-call nil "failure-test"
                                      (lambda () "still-available"))))))

(format t "~%LLM DEBUG CAPTURE TESTS: ~d passed, ~d failed.~%"
        *llm-debug-test-pass* *llm-debug-test-fail*)
(when (plusp *llm-debug-test-fail*) (uiop:quit 1))
