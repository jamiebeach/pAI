(defpackage :agent
  (:use :cl))

(in-package :agent)

(ql:quickload '(:bordeaux-threads :shasht :dexador) :silent t)

(defvar *r0c1b-pass* 0)
(defvar *r0c1b-fail* 0)
(defvar *r0c1b-events* nil)
(defvar *r0c1b-next-event-id* 0)
(defvar *r0c1b-provider-fn* nil)
(defvar *r0c1b-provider-messages* nil)

(defun r0c1b-check (name condition)
  (if condition
      (progn (incf *r0c1b-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *r0c1b-fail*) (format t "  FAIL ~a~%" name))))

(defun obj (&rest kvs)
  (loop with table = (make-hash-table :test #'equal)
        for (key value) on kvs by #'cddr
        do (setf (gethash key table) value)
        finally (return table)))

(defun r0c1b-json= (left right)
  (string= (shasht:write-json left nil) (shasht:write-json right nil)))

(defun log-event (type payload &key caused-by)
  (let ((id (incf *r0c1b-next-event-id*)))
    (push (list id type payload caused-by) *r0c1b-events*)
    id))

(defun call-model (messages) (declare (ignore messages)) nil)
(defun raw-call-model (messages) (declare (ignore messages)) nil)
(defun resolution-level () 0.5d0)

(defparameter *modulator-temp-min* 0.3d0)
(defparameter *modulator-temp-max* 1.0d0)
(defparameter *model* "fixture/model")
(defparameter *tools* (vector (obj "type" "function" "name" "fixture-tool")))
(defparameter *endpoint* "https://fixture.invalid/api/v1/chat/completions")
(defparameter *api-key* "SECRET-NEVER-LOG")
(defparameter *http-connect-timeout* 1)
(defparameter *http-read-timeout* 1)
(defvar *current-causing-event-id* nil)
(defvar *timing-model-purpose* nil)
(defvar *timing-trace-id* nil)
(defvar *timing-turn-id* nil)

(defun pai-base-raw-call-model-reasoning-fallback (messages)
  (push messages *r0c1b-provider-messages*)
  (funcall *r0c1b-provider-fn* messages))

;; Load the exact production forms under test, excluding unrelated top-level
;; persistence/thread behavior in modulator.lisp.
(let* ((source
         (with-open-file (stream (test-source "modulator.lisp"))
           (let ((text (make-string (file-length stream))))
             (read-sequence text stream)
             text)))
       (start (search "(defvar *call-model-temperature-override*" source))
       (end (search ";;; 2. Retrieval weight/width" source :start2 start)))
  (assert (and start end (< start end)))
  (with-input-from-string (stream (subseq source start end))
    (loop for form = (read stream nil :eof)
          until (eq form :eof)
          do (eval form))))

(defun r0c1b-reset ()
  (setf *r0c1b-events* nil
        *r0c1b-next-event-id* 0
        *r0c1b-provider-messages* nil))

(defun r0c1b-events (type)
  (remove-if-not (lambda (entry) (string= type (second entry)))
                 (nreverse (copy-list *r0c1b-events*))))

(format t "~%== ordinary captured HTTP branch ==~%")
(r0c1b-reset)
(let* ((messages (list (obj "role" "user" "content" "pancake follow-up")))
       (returned (obj "choices" (vector (obj "message" (obj "content" "How were they?")))))
       (*current-causing-event-id* 77)
       (*timing-model-purpose* "public-turn")
       (*timing-trace-id* "trace-fixture-1")
       (*timing-turn-id* "turn-fixture-1")
       (*r0c1b-provider-fn* (lambda (actual)
                              (declare (ignore actual))
                              returned))
       (result (pai-base-raw-call-model-reasoning-fallback messages))
       (request (first (r0c1b-events "model-request")))
       (response (first (r0c1b-events "model-response")))
       (request-payload (third request))
       (response-payload (third response))
       (expected-body
         (obj "model" *model* "messages" (coerce messages 'vector)
              "tools" *tools*)))
  (r0c1b-check "ordinary branch returns the identical provider object"
                (eq returned result))
  (r0c1b-check "ordinary branch invokes the captured provider exactly once"
                (and (= 1 (length *r0c1b-provider-messages*))
                     (eq messages (first *r0c1b-provider-messages*))))
  (r0c1b-check "ordinary request is exact and attached to current cause"
                (and (= 77 (fourth request))
                     (string= "public-turn" (gethash "purpose" request-payload))
                     (string= "legacy-raw" (gethash "request_kind" request-payload))
                     (string= "openrouter-http" (gethash "adapter_kind" request-payload))
                     (string= "trace-fixture-1" (gethash "trace_id" request-payload))
                     (string= "turn-fixture-1" (gethash "turn_id" request-payload))
                     (string= *endpoint* (gethash "endpoint" request-payload))
                     (r0c1b-json= expected-body
                                  (gethash "request" request-payload))))
  (r0c1b-check "ordinary response is exact and caused by request"
                (and (= (first request) (fourth response))
                     (string= (gethash "model_call_id" request-payload)
                              (gethash "model_call_id" response-payload))
                     (string= "trace-fixture-1" (gethash "trace_id" response-payload))
                     (string= "turn-fixture-1" (gethash "turn_id" response-payload))
                     (numberp (gethash "duration_ms" response-payload))
                     (>= (gethash "duration_ms" response-payload) 0)
                     (eq returned (gethash "response" response-payload)))))

(format t "~%== override attempts ==~%")
(r0c1b-reset)
(let* ((posted-bodies nil)
      (*modulator-http-post-fn*
        (lambda (body)
          (push body posted-bodies)
          (obj "attempt" (length posted-bodies)))))
  (let ((*call-model-temperature-override* 0.42d0))
    (pai-base-raw-call-model-reasoning-fallback
     (list (obj "role" "user" "content" "temperature"))))
  (let ((*call-model-reasoning-override* :disabled))
    (pai-base-raw-call-model-reasoning-fallback
     (list (obj "role" "user" "content" "retry"))))
  (let* ((requests (r0c1b-events "model-request"))
         (responses (r0c1b-events "model-response"))
         (first-body (gethash "request" (third (first requests))))
         (second-body (gethash "request" (third (second requests))))
         (reasoning (gethash "reasoning" second-body)))
    (r0c1b-check "each override is one separately correlated attempt"
                  (and (= 2 (length posted-bodies))
                       (= 2 (length requests))
                       (= 2 (length responses))
                       (= (first (first requests)) (fourth (first responses)))
                       (= (first (second requests)) (fourth (second responses)))
                       (r0c1b-json= (obj "attempt" 1)
                                    (gethash "response" (third (first responses))))
                       (r0c1b-json= (obj "attempt" 2)
                                    (gethash "response" (third (second responses))))
                       (not (string= (gethash "model_call_id" (third (first requests)))
                                     (gethash "model_call_id" (third (second requests)))))))
    (r0c1b-check "temperature body is the identical submitted body"
                  (and (= 0.42d0 (gethash "temperature" first-body))
                       (eq first-body (second posted-bodies))))
    (r0c1b-check "reasoning-disabled body is the identical submitted body"
                  (and (string= "none" (gethash "effort" reasoning))
                       (eq second-body (first posted-bodies))))))

(format t "~%== provider and event-sink failures ==~%")
(r0c1b-reset)
(let* ((expected (make-condition 'simple-error
                                 :format-control "forced provider failure"
                                 :format-arguments nil))
       (*r0c1b-provider-fn* (lambda (messages)
                              (declare (ignore messages))
                              (error expected)))
       (caught nil))
  (handler-case
      (pai-base-raw-call-model-reasoning-fallback nil)
    (error (condition) (setf caught condition)))
  (let ((request (first (r0c1b-events "model-request")))
        (response (first (r0c1b-events "model-response"))))
    (r0c1b-check "provider error re-signals the identical condition"
                  (eq expected caught))
    (r0c1b-check "provider error emits one caused error response"
                  (and request response
                       (= (first request) (fourth response))
                       (string= "error" (gethash "status" (third response)))
                       (string= "simple-error"
                                (gethash "error_type" (third response)))
                       (search "forced provider failure"
                               (gethash "error_message" (third response)))
                       (eq :null (gethash "response" (third response)))))))

(let ((saved-log (fdefinition 'log-event))
      (calls 0)
      (returned (obj "result" "unchanged"))
      (result nil)
      (expected (make-condition 'simple-error
                                :format-control "provider still fails"
                                :format-arguments nil))
      (caught nil))
  (unwind-protect
      (progn
        (setf (fdefinition 'log-event)
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (error "event sink unavailable")))
        (let ((*r0c1b-provider-fn*
                (lambda (messages)
                  (declare (ignore messages))
                  (incf calls)
                  returned)))
          (setf result (pai-base-raw-call-model-reasoning-fallback nil)))
        (let ((*r0c1b-provider-fn*
                (lambda (messages)
                  (declare (ignore messages))
                  (incf calls)
                  (error expected))))
          (handler-case
              (pai-base-raw-call-model-reasoning-fallback nil)
            (error (condition) (setf caught condition)))))
    (setf (fdefinition 'log-event) saved-log))
  (r0c1b-check "failed sink preserves success identity and one invocation"
                (and (eq returned result) (= 2 calls)))
  (r0c1b-check "failed sink cannot mask provider condition"
                (eq expected caught)))

(let ((serialized (shasht:write-json (coerce *r0c1b-events* 'vector) nil)))
  (r0c1b-check "events contain no authorization header or bearer credential"
                (and (not (search "Authorization" serialized :test #'char-equal))
                     (not (search "Bearer " serialized :test #'char-equal))
                     (not (search *api-key* serialized :test #'char-equal)))))

(format t "~%R0C1B LEGACY/RAW MODEL I/O TESTS: ~d passed, ~d failed.~%"
        *r0c1b-pass* *r0c1b-fail*)
(when (plusp *r0c1b-fail*) (uiop:quit 1))
