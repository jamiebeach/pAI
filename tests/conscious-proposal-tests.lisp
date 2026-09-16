;;;; conscious-proposal-tests.lisp -- Q4 captured proposal validation.
;;;; Written before proposal.lisp; retain the absent-subject red evidence.

(in-package :agent)

(defvar *cproposal-passed* 0)
(defvar *cproposal-failed* 0)

(defun cproposal-check (name condition)
  (if condition
      (progn (incf *cproposal-passed*) (format t "PASS ~a~%" name))
      (progn (incf *cproposal-failed*) (format t "FAIL ~a~%" name))))

(defun cproposal-signals-p (thunk)
  (handler-case (progn (funcall thunk) nil) (error () t)))

(defun cproposal-manifest ()
  (obj "schema_version" 1 "pulse_id" "pulse:40"
       "runtime_revision" "conscious-q4-test"
       "conscious_state_revision" 3
       "evidence_event_ids" (vector 10 11)
       "available_tools" (vector "search-memory")
       "permitted_proposal_kinds"
       (coerce *conscious-proposal-kinds* 'vector)
       "audience" "operator"
       "remaining_budget"
       (obj "tool_proposals" 1 "continuations" 1
            "publication_candidates" 1)))

(defun cproposal-row (id kind payload &optional (evidence (vector 10)))
  (obj "proposal_id" id "pulse_id" "pulse:40"
       "runtime_revision" "conscious-q4-test"
       "conscious_state_revision" 3
       "kind" kind "created_at_stage" "model-deliberation"
       "confidence" 0.8d0 "evidence_event_ids" evidence
       "payload" payload))

(defun cproposal-response (&rest proposals)
  (obj "schema_version" 1 "proposals" (coerce proposals 'vector)))

(format t "~%== Q4 proposal subject ==~%")

(let ((path (merge-pathnames "src/mind/conscious/proposal.lisp" *pai-root*)))
  (cproposal-check "captured proposal validator exists" (probe-file path))
  (when (probe-file path)
    (load path)
    (let* ((tool
             (cproposal-row
              "pulse:40:proposal:1" "tool-call-proposal"
              (obj "tool_name" "search-memory"
                   "arguments" (obj "query_ref" "event:10"))))
           (valid (conscious-proposals-validate
                   (cproposal-response tool) (cproposal-manifest))))
      (cproposal-check "valid permitted tool proposal remains inert data"
                       (and (= 1 (length (gethash "proposals" valid)))
                            (string= "tool-call-proposal"
                                     (gethash "kind"
                                              (aref (gethash "proposals" valid)
                                                    0))))))
    (cproposal-check
     "one invalid proposal rejects the complete captured response"
     (cproposal-signals-p
      (lambda ()
        (conscious-proposals-validate
         (cproposal-response
          (cproposal-row "pulse:40:proposal:1" "abstain" (obj))
          (cproposal-row "pulse:40:proposal:2" "unknown-effect" (obj)))
         (cproposal-manifest)))))
    (cproposal-check
     "nested proposal-shaped payload is rejected"
     (cproposal-signals-p
      (lambda ()
        (conscious-proposals-validate
         (cproposal-response
          (cproposal-row
           "pulse:40:proposal:1" "tool-call-proposal"
           (obj "tool_name" "search-memory" "arguments"
                (obj "proposals" (vector (obj "kind" "tool-call-proposal"))))))
         (cproposal-manifest)))))
    (cproposal-check
     "invented evidence is rejected"
     (cproposal-signals-p
      (lambda ()
        (conscious-proposals-validate
         (cproposal-response
          (cproposal-row "pulse:40:proposal:1" "abstain" (obj)
                         (vector 999)))
         (cproposal-manifest)))))
    (cproposal-check
     "free prose is not a captured proposal object"
     (cproposal-signals-p
      (lambda ()
        (conscious-proposals-validate "I think we should reply."
                                      (cproposal-manifest)))))
    (cproposal-check
     "proposal id must be derived from the manifest pulse"
     (cproposal-signals-p
      (lambda ()
        (conscious-proposals-validate
         (cproposal-response
          (cproposal-row "invented-id" "abstain" (obj)))
         (cproposal-manifest)))))
    (cproposal-check
     "silence must be explicit rather than an empty model response"
     (cproposal-signals-p
      (lambda ()
        (conscious-proposals-validate (cproposal-response)
                                      (cproposal-manifest)))))
    (let ((silence
            (conscious-proposals-validate
             (cproposal-response
              (cproposal-row "pulse:40:proposal:1" "abstain" (obj)))
             (cproposal-manifest))))
      (cproposal-check "explicit captured abstention is a valid outcome"
                       (string= "abstain"
                                (gethash "kind"
                                         (aref (gethash "proposals" silence)
                                               0)))))
    (let ((payloads
            `(("state-update"
               . ,(obj "operation" "replace-ref" "target_ref" "focus:1"
                       "value_ref" "event:10"))
              ("memory-admission-proposal"
               . ,(obj "content" "bounded memory candidate"
                       "evidence_event_ids" (vector 10)
                       "origin_class" "conversation"))
              ("tool-call-proposal"
               . ,(obj "tool_name" "search-memory" "arguments" (obj)))
              ("publication-candidate"
               . ,(obj "audience" "operator" "channel_class" "diagnostic"
                       "speech_act" "answer" "content" "bounded candidate"
                       "evidence_event_ids" (vector 10)
                       "reason_to_speak_now" "direct-response"))
              ("schedule-wake"
               . ,(obj "wake_at" "2030-01-01T00:00:00Z"
                       "reason_code" "fixture-deadline"))
              ("request-continuation" . ,(obj "purpose" "integrate-result"))
              ("self-mod-proposal"
               . ,(obj "change_ref" "change:1"
                       "qualification_profile" "deterministic"))
              ("yield" . ,(obj)) ("abstain" . ,(obj)))))
      (cproposal-check
       "all nine frozen proposal payload schemas validate"
       (every
        (lambda (entry)
          (let ((kind (car entry)))
            (handler-case
                (progn
                  (conscious-proposals-validate
                   (cproposal-response
                    (cproposal-row "pulse:40:proposal:1" kind (cdr entry)
                                   (if (member kind '("yield" "abstain")
                                               :test #'string=)
                                       (vector) (vector 10))))
                   (cproposal-manifest))
                  t)
              (error () nil))))
        payloads)))
    (let ((manifest (cproposal-manifest)))
      (setf (gethash "permitted_proposal_kinds" manifest) (vector "yield"))
      (cproposal-check
       "manifest proposal-kind scope cannot be bypassed"
       (cproposal-signals-p
        (lambda ()
          (conscious-proposals-validate
           (cproposal-response
            (cproposal-row "pulse:40:proposal:1" "abstain" (obj)))
           manifest)))))
    (cproposal-check
     "pulse runtime and state identity mismatch fails closed"
     (cproposal-signals-p
      (lambda ()
        (let ((row (cproposal-row "pulse:40:proposal:1" "yield" (obj)
                                  (vector))))
          (setf (gethash "conscious_state_revision" row) 4)
          (conscious-proposals-validate (cproposal-response row)
                                        (cproposal-manifest))))))
    (dolist (entry '(("tool-call-proposal" "tool_proposals")
                     ("request-continuation" "continuations")
                     ("publication-candidate" "publication_candidates")))
      (let* ((kind (first entry))
             (budget-key (second entry))
             (manifest (cproposal-manifest))
             (payload
               (cond
                 ((string= kind "tool-call-proposal")
                  (obj "tool_name" "search-memory" "arguments" (obj)))
                 ((string= kind "request-continuation")
                  (obj "purpose" "integrate-result"))
                 (t
                  (obj "audience" "operator" "channel_class" "diagnostic"
                       "speech_act" "answer" "content" "candidate"
                       "evidence_event_ids" (vector 10)
                       "reason_to_speak_now" "direct-response")))))
        (setf (gethash budget-key (gethash "remaining_budget" manifest)) 0)
        (cproposal-check
         (format nil "exhausted ~a budget rejects the complete response" budget-key)
         (cproposal-signals-p
          (lambda ()
            (conscious-proposals-validate
             (cproposal-response
              (cproposal-row "pulse:40:proposal:1" kind payload))
             manifest))))))
    (cproposal-check
     "publication payload evidence cannot diverge from its envelope"
     (cproposal-signals-p
      (lambda ()
        (conscious-proposals-validate
         (cproposal-response
          (cproposal-row
           "pulse:40:proposal:1" "publication-candidate"
           (obj "audience" "operator" "channel_class" "diagnostic"
                "speech_act" "answer" "content" "candidate"
                "evidence_event_ids" (vector 11)
                "reason_to_speak_now" "direct-response")))
         (cproposal-manifest)))))
    (let ((source (uiop:read-file-string path)))
      (cproposal-check
       "pure validator source has no provider effect or publication call"
       (notany (lambda (needle) (search needle source :test #'char-equal))
               '("(raw-call-model" "(call-model" "(execute"
                 "(telegram-send" "(log-event"))))))

(format t "~%~d passed, ~d failed~%" *cproposal-passed* *cproposal-failed*)
(when (plusp *cproposal-failed*) (uiop:quit 1))
