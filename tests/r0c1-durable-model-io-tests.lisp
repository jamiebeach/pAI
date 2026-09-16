(in-package :agent)

(ql:quickload :bordeaux-threads :silent t)

(defvar *r0c1-pass* 0)
(defvar *r0c1-fail* 0)
(defvar *r0c1-events* nil)
(defvar *r0c1-next-event-id* 0)

(defun r0c1-check (name condition)
  (if condition
      (progn (incf *r0c1-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *r0c1-fail*) (format t "  FAIL ~a~%" name))))

(defun r0c1-log-event (type payload &key caused-by)
  (let ((id (incf *r0c1-next-event-id*)))
    (push (list id type payload caused-by) *r0c1-events*)
    id))

(setf (fdefinition 'log-event) #'r0c1-log-event)

(unless (fboundp 'call-model)
  (setf (fdefinition 'call-model) (lambda (&rest arguments)
                                    (declare (ignore arguments)) nil)))

(load (test-source "cognitive-call.lisp"))

(setf *cognitive-call-recent-records-fn*
      (lambda (topic purpose) (declare (ignore topic purpose)) nil)
      *cognitive-call-similarity-fn*
      (lambda (left right) (if (string= left right) 1.0d0 0.0d0)))

(defun r0c1-root ()
  (obj "id" "root-user" "kind" "observation"
       "content" "the operator described a pancake breakfast."
       "origin_class" "lived-user" "epistemic_status" "user-report"
       "grounding_status" "grounded" "producer" "fixture"
       "root_observation_ids" (vector) "quarantined" nil))

(defun r0c1-record (purpose)
  (obj "speaker" "the agent" "human" "the operator" "purpose" purpose
       "record_type" "supported-inference"
       "content" "The breakfast detail may be worth following up on."
       "evidence_node_ids" (vector "root-user") "uncertainty" 0.2d0
       "novel_contribution" "Connects a recent detail to a follow-up."
       "proposed_next_operation" "none"))

(defun r0c1-response (record)
  (obj "choices"
       (vector (obj "message"
                    (obj "content" (shasht:write-json record nil))))
       "usage" (obj "prompt_tokens" 10 "completion_tokens" 5
                    "total_tokens" 15 "cost" 0.001d0)))

(defun r0c1-artifact-response ()
  (r0c1-response
   (obj "speaker" "the agent" "human" "the operator"
        "purpose" "creative-story-outline" "operation_type" "outline"
        "artifact_content" "A quiet breakfast opens the story."
        "change_summary" "Created the requested outline."
        "visibility" "private" "content_class" "synthetic")))

(defun r0c1-events-of-type (type)
  (remove-if-not (lambda (entry) (string= type (second entry)))
                 (nreverse (copy-list *r0c1-events*))))

(defun r0c1-reset-events ()
  (setf *r0c1-events* nil *r0c1-next-event-id* 0))

(format t "~%== exact primary pair ==~%")
(r0c1-reset-events)
(let* ((captured-messages nil)
       (returned (r0c1-response (r0c1-record "deep-reflection")))
       (*cognitive-call-model-fn*
         (lambda (messages model temperature)
           (declare (ignore model temperature))
           (setf captured-messages messages)
           returned))
       (result (cognitive-call "deep-reflection" (list (r0c1-root))
                               :generation-id "generation-primary"
                               :model "fixture-model" :temperature 0.25d0))
       (requests (r0c1-events-of-type "model-request"))
       (responses (r0c1-events-of-type "model-response"))
       (request (first requests))
       (response (first responses))
       (request-payload (third request))
       (response-payload (third response)))
  (r0c1-check "accepted result is unchanged"
               (string= "accepted" (gethash "status" result)))
  (r0c1-check "one request and one response are emitted"
               (and (= 1 (length requests)) (= 1 (length responses))))
  (r0c1-check "request records exact resolved adapter inputs"
               (and (string= "generation-primary"
                             (gethash "generation_id" request-payload))
                    (string= "primary" (gethash "request_kind" request-payload))
                    (string= "test-adapter" (gethash "adapter_kind" request-payload))
                    (string= "fixture-model" (gethash "model" request-payload))
                    (= 0.25d0 (gethash "temperature" request-payload))
                    (string= "json_object"
                             (gethash "type"
                                      (gethash "response_format" request-payload)))
                    (string=
                     (shasht:write-json (coerce captured-messages 'vector) nil)
                     (shasht:write-json (gethash "messages" request-payload) nil))))
  (r0c1-check "response is exact and caused by its request"
               (and (= (first request) (fourth response))
                    (string= (gethash "model_call_id" request-payload)
                             (gethash "model_call_id" response-payload))
                    (string=
                     (shasht:write-json returned nil)
                     (shasht:write-json (gethash "response" response-payload) nil))))
  (let ((serialized
          (shasht:write-json (vector request-payload response-payload) nil)))
    (r0c1-check "durable pair contains no authorization header or credential"
                 (and (not (search "Authorization" serialized :test #'char-equal))
                      (not (search "Bearer " serialized :test #'char-equal))))))

(format t "~%== repair attempts are separate pairs ==~%")
(r0c1-reset-events)
(let* ((calls 0)
       (*cognitive-call-model-fn*
        (lambda (messages model temperature)
          (declare (ignore messages model temperature))
          (incf calls)
          (if (= calls 1) "not-json"
              (r0c1-response (r0c1-record "deep-reflection"))))))
  (let* ((result (cognitive-call "deep-reflection" (list (r0c1-root))
                                 :generation-id "generation-repair"))
         (requests (r0c1-events-of-type "model-request"))
         (responses (r0c1-events-of-type "model-response")))
    (r0c1-check "repair behavior remains one bounded retry"
                 (and (string= "accepted" (gethash "status" result))
                      (= 1 (gethash "retries" result)) (= calls 2)))
    (r0c1-check "primary and repair each emit a complete pair"
                 (and (= 2 (length requests)) (= 2 (length responses))
                      (equal '("primary" "repair")
                             (mapcar (lambda (entry)
                                       (gethash "request_kind" (third entry)))
                                     requests))
                      (= 2 (length
                            (remove-duplicates
                             (mapcar (lambda (entry)
                                       (gethash "model_call_id" (third entry)))
                                     requests)
                             :test #'string=)))))))

(format t "~%== artifact pair ==~%")
(r0c1-reset-events)
(let* ((*cognitive-call-model-fn*
         (lambda (messages model temperature)
           (declare (ignore messages model temperature))
           (r0c1-artifact-response)))
       (result
         (cognitive-artifact-call
          "creative-story-outline" (list (r0c1-root))
          :generation-id "generation-artifact"
          :project-contract
          (obj "operation_type" "outline" "required_change" "create")))
       (request (first (r0c1-events-of-type "model-request")))
       (response (first (r0c1-events-of-type "model-response"))))
  (r0c1-check "artifact result remains accepted"
               (string= "accepted" (gethash "status" result)))
  (r0c1-check "artifact emits one correlated artifact pair"
               (and request response
                    (string= "artifact" (gethash "request_kind" (third request)))
                    (string= "generation-artifact"
                             (gethash "generation_id" (third request)))
                    (= (first request) (fourth response)))))

(format t "~%== errors and logger isolation ==~%")
(r0c1-reset-events)
(let* ((*cognitive-call-model-fn*
         (lambda (messages model temperature)
           (declare (ignore messages model temperature))
           (error "forced model failure")))
       (result (cognitive-call "deep-reflection" (list (r0c1-root))
                               :generation-id "generation-error"))
       (request (first (r0c1-events-of-type "model-request")))
       (response (first (r0c1-events-of-type "model-response"))))
  (r0c1-check "model error remains fail-closed"
               (string= "model-error" (gethash "status" result)))
  (r0c1-check "model error emits a caused error response"
               (and request response (= (first request) (fourth response))
                    (string= "error" (gethash "status" (third response)))
                    (eq :null (gethash "response" (third response))))))

(let ((saved (fdefinition 'log-event))
      (calls 0)
      (result nil)
      (error-result nil))
  (unwind-protect
      (progn
        (setf (fdefinition 'log-event)
              (lambda (&rest arguments)
                (declare (ignore arguments)) (error "event sink unavailable")))
        (let ((*cognitive-call-model-fn*
                (lambda (messages model temperature)
                  (declare (ignore messages model temperature))
                  (incf calls)
                  (r0c1-response (r0c1-record "deep-reflection")))))
          (setf result
                (cognitive-call "deep-reflection" (list (r0c1-root))
                                :generation-id "generation-log-failure")))
        (let ((*cognitive-call-model-fn*
                (lambda (messages model temperature)
                  (declare (ignore messages model temperature))
                  (error "model still fails"))))
          (setf error-result
                (cognitive-call "deep-reflection" (list (r0c1-root))
                                :generation-id
                                "generation-log-and-model-failure"))))
    (setf (fdefinition 'log-event) saved))
  (r0c1-check "event sink failure cannot alter model behavior"
               (and (= calls 1)
                    (string= "accepted" (gethash "status" result))))
  (r0c1-check "event sink failure cannot mask model failure"
               (string= "model-error" (gethash "status" error-result))))

(format t "~%R0C1 DURABLE MODEL I/O TESTS: ~d passed, ~d failed.~%"
        *r0c1-pass* *r0c1-fail*)
(when (plusp *r0c1-fail*) (uiop:quit 1))
