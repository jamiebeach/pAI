(in-package :agent)

(defvar *publication-failure-history-passed* 0)
(defvar *publication-failure-history-failed* 0)

(defun publication-failure-history-check (name condition)
  (if condition
      (progn
        (incf *publication-failure-history-passed*)
        (format t "PASS ~a~%" name))
      (progn
        (incf *publication-failure-history-failed*)
        (format t "FAIL ~a~%" name))))

(unless (find-class 'public-response-unavailable nil)
  (define-condition public-response-unavailable (error)
    ((reason :initarg :reason :reader public-response-unavailable-reason))))

(load (test-source "agent_print.lisp"))

(let* ((messages
         (list (obj "role" "system" "content" "identity")
               (obj "role" "user" "content" "blue")))
       (original-agent-loop (fdefinition 'agent-loop))
       (caught nil))
  (unwind-protect
      (progn
        (setf *last-self-mod-history* nil
              (fdefinition 'agent-loop)
              (lambda (history)
                (declare (ignore history))
                (error 'public-response-unavailable :reason "fixture")))
        (handler-case (%run-self-mod-messages messages)
          (public-response-unavailable (condition)
            (setf caught condition)))
        (publication-failure-history-check
         "typed publication failure is re-signalled" caught)
        (publication-failure-history-check
         "failed turn preserves the inbound history"
         (equal *last-self-mod-history* messages))
        (publication-failure-history-check
         "failed turn invents no assistant record"
         (and (= 2 (length *last-self-mod-history*))
              (string= "user"
                       (gethash "role" (car (last *last-self-mod-history*)))))))
    (setf (fdefinition 'agent-loop) original-agent-loop)))

(load (test-source "conversation-persistence.lisp"))

(let* ((path #P"/tmp/publication-failure-conversation.json")
       (messages
         (list (obj "role" "system" "content" "identity")
               (obj "role" "user" "content" "blue")))
       (caught nil))
  (ignore-errors (delete-file path))
  (setf *conversation-file* path
        *last-self-mod-history* nil
        (fdefinition '%conv-base-run-self-mod-messages)
        (lambda (history)
          (setf *last-self-mod-history* history)
          (error 'public-response-unavailable :reason "fixture")))
  (handler-case (%run-self-mod-messages messages)
    (public-response-unavailable (condition)
      (setf caught condition)))
  (publication-failure-history-check
   "persistence wrapper re-signals failure" caught)
  (publication-failure-history-check
   "input-only failed turn is durable" (probe-file path))
  (let ((stored (and (probe-file path)
                     (shasht:read-json (uiop:read-file-string path)))))
    (publication-failure-history-check
     "durable failed turn ends at inbound user"
     (and stored
          (= 2 (length stored))
          (string= "user" (gethash "role" (aref stored 1)))
          (string= "blue" (gethash "content" (aref stored 1))))))
  (ignore-errors (delete-file path)))

(format t "~%PUBLICATION FAILURE HISTORY: ~d passed, ~d failed.~%"
        *publication-failure-history-passed*
        *publication-failure-history-failed*)
(when (plusp *publication-failure-history-failed*) (uiop:quit 1))
