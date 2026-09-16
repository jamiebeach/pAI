(defpackage :agent (:use :cl))
(in-package :agent)
(ql:quickload '(:bordeaux-threads :shasht :hunchentoot) :silent t)

(defvar *admin-test-pass* 0)
(defvar *admin-test-fail* 0)
(defun admin-test-check (name condition)
  (if condition
      (progn (incf *admin-test-pass*) (format t "PASS ~a~%" name))
      (progn (incf *admin-test-fail*) (format t "FAIL ~a~%" name))))
(defun obj (&rest kvs)
  (loop with table = (make-hash-table :test #'equal)
        for (key value) on kvs by #'cddr
        do (setf (gethash key table) value)
        finally (return table)))

(uiop:chdir (test-state-dir))
(load (test-source "conversation-context-budget.lisp"))
(load (test-source "public-system-prompt.lisp"))
(load (test-source "llm-debug-capture.lisp"))
(load (test-source "admin-console.lisp"))

(let ((*admin-console-token* nil))
  (admin-test-check "admin API is disabled without a configured token"
                    (not (admin-console-configured-p)))
  (admin-test-check "disabled admin rejects every bearer value"
                    (not (%admin-authorized-p "Bearer anything"))))

(let ((*admin-console-token* "0123456789abcdef0123456789abcdef"))
  (admin-test-check "exact bearer token authenticates"
                    (%admin-authorized-p
                     "Bearer 0123456789abcdef0123456789abcdef"))
  (admin-test-check "wrong bearer token fails closed"
                    (not (%admin-authorized-p
                          "Bearer 0123456789abcdef0123456789abcdeg")))
  (admin-test-check "token is not embedded in admin HTML"
                    (null (search *admin-console-token* *admin-console-html*))))

(admin-test-check
 "admin page exposes context controls and raw JSON without authority controls"
 (and (search "target_estimated_tokens" *admin-console-html*)
      (search "/api/admin/context/current" *admin-console-html*)
      (search "/api/admin/prompt/config" *admin-console-html*)
      (search "Public identity and voice" *admin-console-html*)
      (search "Download JSON" *admin-console-html*)
      (null (search "initiative_delivery" *admin-console-html*))
      (null (search "autonomous_write" *admin-console-html*))
      (null (search "public_outbound_gateway" *admin-console-html*))))

(admin-test-check
 "authenticated curator evidence endpoint is wired outside public dashboard"
 (let ((source (uiop:read-file-string (namestring (test-source "admin-console.lisp")))))
   (and (search "/api/admin/curator/current" source)
        (search "last_selected" source)
        (search "context-curator-last-selected-private-result" source))))

(let* ((config-path
         (pathname (format nil "/tmp/pai-admin-config-~d.json"
                           (get-universal-time))))
       (*conversation-context-config-file* config-path)
       (body (shasht:write-json
              (obj "target_records" 80 "minimum_recent_records" 32
                   "hard_records" 140 "target_estimated_tokens" 140000
                   "hard_estimated_tokens" 220000 "target_chars" 560000
                   "hard_chars" 880000 "brief_chars" 6000
                   "tool_result_chars" 8000)
              nil))
       (result (%admin-update-context-config body)))
  (admin-test-check "authenticated update parser applies validated values"
                    (and (= 80 (gethash "target_records" result))
                         (= 140000 (gethash "target_estimated_tokens" result))))
  (admin-test-check "authenticated update parser persists values"
                    (probe-file config-path))
  (admin-test-check
   "oversized update body is rejected before JSON parsing"
   (handler-case
       (let ((*admin-console-max-request-chars* 4))
         (%admin-update-context-config body)
         nil)
     (error () t))))

(let* ((config-path
         (pathname (format nil "/tmp/pai-admin-prompt-~d.json"
                           (get-universal-time))))
       (*public-system-prompt-config-file* config-path)
       (*public-system-prompt-current* (%psp-default-current))
       (*public-system-prompt-history* nil)
       (body (shasht:write-json
              (obj "identity" "# Identity\n\nAdmin-edited the agent."
                   "voice" "# Voice\n\nWarm and exact.") nil))
       (result (%admin-update-prompt-config body)))
  (admin-test-check "authenticated prompt update versions and persists"
                    (and (= 1 (gethash "revision" result))
                         (probe-file config-path)))
  (admin-test-check "admin prompt report includes next-turn preview and hashes"
                    (let ((report (%admin-prompt-config-response)))
                      (and (search "Admin-edited the agent."
                                   (gethash "rendered_stable_prompt" report))
                           (= 64 (length (gethash "identity_sha256" report)))
                           (= 64 (length
                                  (gethash "rendered_stable_sha256" report))))))
  (admin-test-check "admin prompt rollback restores the previous revision"
                    (progn
                      (public-system-prompt-rollback :actor "test")
                      (search "I am **ACME Agent**"
                              (public-system-prompt-render-stable))))
  (when (probe-file config-path) (delete-file config-path)))

(let* ((*llm-debug-capture-mode* :off)
       (*llm-debug-current-public-context* nil)
       (secret (concatenate 'string "sk-" "admin-context-secret"))
       (messages (list (obj "role" "system" "content" "identity")
                       (obj "role" "user" "content" secret))))
  (llm-debug-capture-call messages "public" (lambda () "reply"))
  (let* ((snapshot (llm-debug-current-public-context))
         (json (shasht:write-json snapshot nil)))
    (admin-test-check "latest public request is available while disk capture is off"
                      (and (string= "available" (gethash "status" snapshot))
                           (= 2 (length (gethash "messages" snapshot)))))
    (admin-test-check "credential-shaped prompt text is redacted"
                      (and (search "[REDACTED]" json)
                           (null (search secret json)))))
  (llm-debug-capture-call (list (obj "role" "user" "content" "private tick"))
                          "tick" (lambda () "private"))
  (admin-test-check "private model calls cannot replace public context snapshot"
                    (= 2 (length (gethash "messages"
                                          (llm-debug-current-public-context)))))
  (llm-debug-capture-call
   (list (obj "role" "system" "content" "summary judge")
         (obj "role" "user" "content" "candidate summaries"))
   "context-summary" (lambda () "A"))
  (admin-test-check "context summaries cannot replace public context snapshot"
                    (= 2 (length (gethash "messages"
                                          (llm-debug-current-public-context))))))

(let ((*llm-debug-current-public-context* nil)
      (*llm-debug-current-context-max-json-chars* 2))
  (llm-debug-capture-call (list (obj "role" "user" "content" "too large"))
                          "public" (lambda () "reply"))
  (admin-test-check "oversized current context fails closed without truncation"
                    (let ((snapshot (llm-debug-current-public-context)))
                      (and (string= "unavailable" (gethash "status" snapshot))
                           (null (gethash "messages" snapshot))))))

(let ((source (uiop:read-file-string (namestring (test-source "admin-console.lisp")))))
  (admin-test-check "memory atom report has an authenticated admin route"
                    (and (search "/api/admin/memory-atoms/report" source)
                         (search "%admin-require-authorization" source)))
  (admin-test-check "raw-root review has an authenticated admin route"
                    (search "/api/admin/memory-atoms/current" source)))

(format t "~%ADMIN CONSOLE TESTS: ~d passed, ~d failed.~%"
        *admin-test-pass* *admin-test-fail*)
(when (plusp *admin-test-fail*) (uiop:quit 1))
