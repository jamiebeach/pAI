;;;; conscious-runtime-composition-tests.lisp -- sealed plan compatibility.

(in-package :agent)

(ql:quickload '(:shasht :ironclad) :silent t)

(defvar *crpc-pass* 0)
(defvar *crpc-fail* 0)
(defvar *crpc-events* nil)
(defvar *crpc-receipt-mode* nil)
(defvar *crpc-before-append* nil)
(defvar *crpc-append-lock* (bt:make-lock "plan fixture append authority"))
(defparameter *agent-id* "runtime-plan-fixture")

(defun crpc-check (name condition)
  (if condition
      (progn (incf *crpc-pass*) (format t "PASS ~a~%" name))
      (progn (incf *crpc-fail*) (format t "FAIL ~a~%" name))))

(defun crpc-signals-p (thunk)
  (handler-case (progn (funcall thunk) nil) (error () t)))

(defun replay-events (&rest ignored)
  (declare (ignore ignored)) (copy-list *crpc-events*))

(defun log-event (type payload &key caused-by)
  (declare (ignore caused-by))
  (when *crpc-before-append*
    (let ((hook *crpc-before-append*))
      (setf *crpc-before-append* nil)
      (funcall hook)))
  (let* ((id (1+ (length *crpc-events*)))
         (event (obj "id" id "type" type "agent_id" *agent-id*
                     "payload" payload)))
    (setf *crpc-events* (append *crpc-events* (list event)))
    (let ((receipt (%conscious-runtime-plan-copy event)))
      (case *crpc-receipt-mode*
        (:wrong-id (setf (gethash "id" receipt) (+ id 100)))
        (:wrong-type (setf (gethash "type" receipt) "unrelated-event"))
        (:wrong-payload (setf (gethash "payload" receipt) (obj)))
        (:not-visible (setf *crpc-events* nil)))
      (values id t receipt))))

(defun log-event-if (predicate type payload &key caused-by)
  (bt:with-lock-held (*crpc-append-lock*)
    ;; Schedule a competing committed registration before predicate evaluation.
    (when *crpc-before-append*
      (let ((hook *crpc-before-append*))
        (setf *crpc-before-append* nil)
        (funcall hook)))
    (when (funcall predicate)
      (multiple-value-bind (id durable receipt)
          (log-event type payload :caused-by caused-by)
        (values id durable receipt t)))))

(defun crpc-json (relative)
  (shasht:read-json
   (uiop:read-file-string (merge-pathnames relative *pai-root*))))

(defun crpc-capabilities (work)
  (let ((tools (make-hash-table :test #'equal))
        (proposals (make-hash-table :test #'equal)))
    (dolist (tool (coerce (gethash "permitted_tools" work) 'list))
      (setf (gethash tool tools)
            (obj "consumer" "conscious-tool-operation-runtime"
                 "authority_class" "bounded-read-only"
                 "max_result_characters"
                 (gethash "max_tool_result_characters" work))))
    (dolist (kind (coerce (gethash "permitted_proposal_kinds" work) 'list))
      (setf (gethash kind proposals)
            (vector (if (string= kind "tool-call-proposal")
                        "cognitive-operation-executor"
                        "conversation-work-loop"))))
    (obj "tool_consumers" tools "proposal_consumers" proposals)))

(defun crpc-provider ()
  (let ((context (gethash "solicited-conversation-dev"
                         (gethash "profiles" (crpc-json "config/conscious-context-profiles.json"))))
        (work (gethash "interactive-dev"
                      (gethash "profiles" (crpc-json "config/conscious-work-profiles.json")))))
    (obj "profile_id" "contained-provider" "revision" 1
         "max_requests" (gethash "max_model_calls" work)
         "max_input_characters" (gethash "total_character_budget" context))))

(defun crpc-publication ()
  (obj "profile_id" "solicited-publication" "revision" 1
       "channels" (vector "terminal")))

(defun crpc-transport ()
  (obj "profile_id" "terminal-transport" "revision" 1
       "channel" "terminal"))

(format t "~%== conscious runtime composition ==~%")

(load (test-source "cognitive-work.lisp"))
(load (test-source "runtime-composition.lisp"))

(let* ((contexts (gethash "profiles" (crpc-json "config/conscious-context-profiles.json")))
       (works (gethash "profiles" (crpc-json "config/conscious-work-profiles.json")))
       (context (gethash "solicited-conversation-dev" contexts))
       (work (gethash "interactive-dev" works))
       (capabilities (crpc-capabilities work))
       (plan (conscious-runtime-plan-compile
              context work capabilities (crpc-provider)
              (crpc-publication) (crpc-transport)))
       (hash (conscious-runtime-plan-hash plan)))
  (crpc-check "production context/work composition compiles to a sealed hash"
              (and (= 64 (length hash))
                   (string= "dedicated-untrusted-tool-results-v1"
                            (gethash "compatibility_revision"
                                     (gethash "canonical_plan" plan)))
                   (= 13152
                      (gethash "required_tool_characters"
                               (gethash "canonical_plan" plan)))))
  (let ((narrow (%conscious-runtime-plan-copy context)))
    (setf (gethash "total_character_budget" narrow) 13000)
    (crpc-check "whole-budget incompatibility refuses before work admission"
                (crpc-signals-p
                 (lambda ()
                   (conscious-runtime-plan-compile
                    narrow work capabilities (crpc-provider)
                    (crpc-publication) (crpc-transport))))))
  (let ((more-operations (%conscious-runtime-plan-copy work)))
    (setf (gethash "max_tool_operations" more-operations) 7)
    (crpc-check "operation-budget growth revalidates accumulated wrapper cost"
                (crpc-signals-p
                 (lambda ()
                   (conscious-runtime-plan-compile
                    context more-operations (crpc-capabilities more-operations)
                    (crpc-provider) (crpc-publication) (crpc-transport))))))
  (let ((first (conscious-runtime-plan-retain plan))
        (second (conscious-runtime-plan-retain plan)))
    (crpc-check "plan retention is content-addressed and idempotent"
                (and (string= "registered" (gethash "status" first))
                     (string= "retained" (gethash "status" second))
                     (= 1 (length *crpc-events*)))))
  (let ((provider (crpc-provider)))
    (setf (gethash "max_input_characters" provider)
          (1- (gethash "total_character_budget" context)))
    (crpc-check "provider below the selected context bound is refused"
                (crpc-signals-p
                 (lambda () (conscious-runtime-plan-compile
                             context work capabilities provider
                             (crpc-publication) (crpc-transport))))))
  (let* ((*crpc-events* nil)
         (other-context (%conscious-runtime-plan-copy context)))
    (incf (gethash "revision" other-context))
    (let ((other-plan (conscious-runtime-plan-compile
                       other-context work capabilities (crpc-provider)
                       (crpc-publication) (crpc-transport))))
      (log-event *conscious-runtime-plan-event-type*
                 (obj "schema_version" 1 "plan_hash" hash "plan" other-plan))
      (crpc-check "retry refuses a different valid plan under the requested envelope hash"
                  (crpc-signals-p (lambda () (conscious-runtime-plan-retain plan))))))
  (dolist (mode '(:wrong-id :wrong-type :wrong-payload :not-visible))
    (let ((*crpc-events* nil) (*crpc-receipt-mode* mode))
      (crpc-check (format nil "retention refuses nonexact or unreadable receipt ~a" mode)
                  (crpc-signals-p (lambda () (conscious-runtime-plan-retain plan))))))
  (let* ((*crpc-events* nil)
         (*crpc-before-append*
           (lambda ()
             (log-event *conscious-runtime-plan-event-type*
                        (obj "schema_version" 1 "plan_hash" hash
                             "plan" (%conscious-runtime-plan-copy plan)))))
         (retained (conscious-runtime-plan-retain plan)))
    (crpc-check "competing registration joins one plan without a duplicate append"
                (and (= 1 (length *crpc-events*))
                     (string= "retained" (gethash "status" retained))
                     (not (crpc-signals-p
                           (lambda () (conscious-runtime-plan-resolve hash)))))))
  (let* ((revision-two (%conscious-runtime-plan-copy context)))
    (setf (gethash "revision" revision-two) 3)
    (conscious-runtime-plan-compile
     revision-two work capabilities (crpc-provider)
     (crpc-publication) (crpc-transport))
    (crpc-check "retained revision A resolves after current config moves on"
                (string= hash
                         (conscious-runtime-plan-hash
                          (conscious-runtime-plan-resolve hash)))))
  (let* ((event (first *crpc-events*))
         (retained (gethash "plan" (gethash "payload" event)))
         (body (gethash "canonical_plan" retained)))
    (setf (gethash "required_tool_characters" body) 1)
    (crpc-check "tampered retained plan fails integrity before reuse"
                (crpc-signals-p
                 (lambda () (conscious-runtime-plan-resolve hash))))))

(format t "~%~d passed, ~d failed~%" *crpc-pass* *crpc-fail*)
(when (plusp *crpc-fail*) (error "conscious runtime composition tests failed"))
