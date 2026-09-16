(in-package :agent)

(defvar *runtime-observer-audit-test-pass* 0)
(defvar *runtime-observer-audit-test-fail* 0)

(defun runtime-observer-audit-test-check (name condition)
  (if condition
      (progn (incf *runtime-observer-audit-test-pass*)
             (format t "  ok   ~a~%" name))
      (progn (incf *runtime-observer-audit-test-fail*)
             (format t "  FAIL ~a~%" name))))

(load (test-source "runtime-observer-registry.lisp"))
(runtime-observer-reset)
(load (test-source "runtime-observer-audit.lisp"))
(runtime-observer-audit-reset)
;; Observer registration is a DEFINE-INIT :install action now, not a
;; load-time side effect. Under the original entrypoint the observers were
;; registered by the time any suite ran, so this was never stated; the
;; assertion below checks they are registered and fails closed without it.
(initialize :phases (list :install) :stop-on-error nil :verbose nil)


(runtime-observer-audit-test-check
 "all ten lifecycle classes register as required observe-only consumers"
 (let ((report (runtime-observer-audit-report)))
   (and (= 10 (gethash "required_classes" report))
        (= 10 (gethash "observer_count" (runtime-observer-report)))
        (every (lambda (row)
                 (and (gethash "required" row)
                      (string= "observe" (gethash "capability" row))))
               (coerce (gethash "coverage" report) 'list)))))

(runtime-observer-audit-test-check
 "required lifecycle assertion passes when fully wired"
 (runtime-observer-audit-assert))

(dolist (spec *runtime-observer-audit-specs*)
  (let ((actual (first spec)))
    (runtime-observer-emit
     actual
     (if (member actual '("public-outbound-evaluated"
                          "public-outbound-completed") :test #'string=)
         (obj "envelope" (obj "id" (format nil "env-~a" actual))
              "canonical_public_act_id" (format nil "act-~a" actual))
         (obj "id" (format nil "event-~a" actual)
              "type" actual "payload" (obj "private" "do-not-retain"))))))

(runtime-observer-audit-test-check
 "real consumer observes every lifecycle class"
 (let ((report (runtime-observer-audit-report)))
   (and (= 10 (gethash "consumed" report))
        (every (lambda (row) (= 1 (gethash "consumed" row)))
               (coerce (gethash "coverage" report) 'list)))))

(runtime-observer-audit-test-check
 "audit report retains no payload content"
 (null (search "do-not-retain"
               (shasht:write-json (runtime-observer-audit-report) nil))))

(runtime-observer-unregister "tool-turn-committed"
                             "runtime-audit-tool-turn-committed")
(runtime-observer-audit-test-check
 "miswired required lifecycle consumer fails loudly"
 (handler-case (progn (runtime-observer-audit-assert) nil)
   (error () t)))
(runtime-observer-audit-register-all)
(runtime-observer-audit-test-check
 "registration repair restores the boot assertion"
 (runtime-observer-audit-assert))

(let* ((before (gethash "consumed" (runtime-observer-audit-report)))
       (result (runtime-observer-emit "memory-write" "not-an-object")))
  (runtime-observer-audit-test-check
   "malformed lifecycle input is isolated as an observer error"
   (and (string= "observer-error" (gethash "status" (first result)))
        (= before (gethash "consumed" (runtime-observer-audit-report))))))

(unless (fboundp 'auto-turn)
  (setf (fdefinition 'auto-turn) (lambda (&rest arguments)
                                   (declare (ignore arguments)) nil)))
(unless (fboundp 'execute)
  (setf (fdefinition 'execute) (lambda (&rest arguments)
                                 (declare (ignore arguments)) nil)))
(unless (fboundp 'propose-loop)
  (setf (fdefinition 'propose-loop) (lambda (&rest arguments)
                                      (declare (ignore arguments)) nil)))
(load (test-source "event-log.lisp"))
(setf *event-log-file* #P"/tmp/runtime-observer-event-log.jsonl"
      *event-next-id* 0
      *event-ring* nil)
(ignore-errors (delete-file *event-log-file*))
(runtime-observer-audit-reset)
(let ((id (log-event "user-message" (obj "text" "private-fixture"))))
  (runtime-observer-audit-test-check
   "durably appended events reach the registered consumer after commit"
   (let* ((report (runtime-observer-audit-report))
          (row (find "user-reply-observed" (gethash "coverage" report)
                     :test #'string=
                     :key (lambda (item) (gethash "logical_class" item)))))
     (and (= 1 id) row (= 1 (gethash "consumed" row))
          (string= "1" (gethash "last_source_id" row)))))
  (runtime-observer-audit-test-check
   "durable event content is absent from consumer report"
   (null (search "private-fixture"
                 (shasht:write-json (runtime-observer-audit-report) nil)))))
(ignore-errors (delete-file *event-log-file*))

(runtime-observer-audit-test-check
 "source declares content-free committed explore, artifact, and tool-turn signals"
 (let ((explore (uiop:read-file-string
                 (namestring (test-source "feedback-loop-containment.lisp"))))
       (artifacts (uiop:read-file-string
                   (namestring (test-source "first-person-evidence.lisp"))))
       (turn (uiop:read-file-string
              (namestring (test-source "conversation-turn-capture.lisp")))))
   (and (search "explore-stance-committed" explore :test #'char-equal)
        (search "root_evidence_count" explore :test #'char-equal)
        (search "\"artifact-validated\"" artifacts :test #'char-equal)
        (search "\"artifact-completed\"" artifacts :test #'char-equal)
        (search "tool-turn-committed" turn :test #'char-equal)
        (search "tool_entry_count" turn :test #'char-equal))))

(runtime-observer-audit-test-check
 "outbound completion signal remains observe-only and status-bound"
 (let ((source (uiop:read-file-string
                (namestring (test-source "public-outbound-gateway.lisp")))))
   (and (search "public-outbound-completed" source :test #'char-equal)
        (search "transport_status\" status" source :test #'char-equal)
        (search "*public-outbound-gateway-mode* :observe"
                source :test #'char-equal))))

(format t "~&runtime observer audit tests: ~d passed, ~d failed.~%"
        *runtime-observer-audit-test-pass* *runtime-observer-audit-test-fail*)
(when (plusp *runtime-observer-audit-test-fail*)
  (error "runtime observer audit tests failed"))
