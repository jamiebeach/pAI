(in-package :agent)

(ql:quickload '(:bordeaux-threads :shasht) :silent t)

(defvar *a1-tool-pass* 0)
(defvar *a1-tool-fail* 0)
(defvar *a1-tool-presentations* 0)
(defvar *initiative-candidates* nil)
(defvar *initiative-policy-current-id* nil)

(defun a1-tool-check (name condition)
  (if condition
      (progn (incf *a1-tool-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *a1-tool-fail*) (format t "  FAIL ~a~%" name))))

;; Minimal legacy owners. EVENT-LOG and the gateway wrap these exactly as the
;; production load chain does; no transport, provider, thread, or database is
;; present in this fixture.
(defun auto-turn (prompt) prompt)
(defun propose-loop (source) (declare (ignore source)) "REJECTED fixture")
(defun %v2-broadcast (type data)
  (declare (ignore type data))
  (incf *a1-tool-presentations*)
  "presented")
(defun execute (tool-call)
  (%v2-broadcast "tool" "bounded tool status")
  (obj "role" "tool" "tool_call_id" (gethash "id" tool-call)
       "content" "fixture result"))

(load (test-source "runtime-observer-registry.lisp"))
(load (test-source "event-log.lisp"))
(setf *event-log-file* #P"/tmp/a1-tool-events.jsonl"
      *event-ring* nil
      *event-next-id* 0)
(ignore-errors (delete-file *event-log-file*))

(load (test-source "public-outbound-gateway.lisp"))
(setf *public-outbound-audit-file* #P"/tmp/a1-tool-outbound.json"
      *public-outbound-records* nil
      *public-outbound-audit-observer-count* 0
      *public-outbound-audit-observer-last-envelope-id* nil)
(ignore-errors (delete-file *public-outbound-audit-file*))

(let* ((*public-outbound-envelope*
         (make-public-outbound-envelope
          :kind :reply :channel "web"
          :source-event-ids (list "web-request-fixture")
          :causal-event-ids (list "web-request-fixture")
          :authorization-kind :inbound-request
          :authorization-id "web-request-fixture"
          :source "a1-tool-fixture" :dedupe-key "web-tool-fixture"))
       (tool-call
         (obj "id" "call-fixture-1"
              "function" (obj "name" "fixture"
                              "arguments" "{\"value\":1}")))
       (result (execute tool-call))
       (record (first *public-outbound-records*))
       (envelope (gethash "envelope" record))
       (call-event (find "tool-call" *event-ring*
                         :key (lambda (event) (gethash "type" event))
                         :test #'string=))
       (result-event (find "tool-result" *event-ring*
                           :key (lambda (event) (gethash "type" event))
                           :test #'string=)))
  (a1-tool-check "wrapped tool result is unchanged"
                 (string= "fixture result" (gethash "content" result)))
  (a1-tool-check "legacy presentation occurs exactly once"
                 (= 1 *a1-tool-presentations*))
  (a1-tool-check "public envelope carries authoritative correlation"
                 (and (string= "tool-result" (gethash "kind" envelope))
                      (string= "call-fixture-1"
                               (gethash "tool_call_id" envelope))
                      (string= "tool-result:call-fixture-1"
                               (gethash "tool_result_id" envelope))
                      (string= "would-permit"
                               (gethash "counterfactual_decision" record))))
  (a1-tool-check "tool events retain the same correlation"
                 (let ((call-payload (gethash "payload" call-event))
                       (result-payload (gethash "payload" result-event)))
                   (and (string= "call-fixture-1"
                                 (gethash "tool_call_id" call-payload))
                        (string= "tool-result:call-fixture-1"
                                 (gethash "tool_result_id" call-payload))
                        (string= "call-fixture-1"
                                 (gethash "tool_call_id" result-payload))
                        (string= "tool-result:call-fixture-1"
                                 (gethash "tool_result_id" result-payload)))))
  (a1-tool-check "result event is caused by call event"
                 (= (gethash "id" call-event)
                    (gethash "caused_by" result-event)))
  (a1-tool-check "required audit consumer receives the public act"
                 (= 1 (gethash "consumed"
                               (public-outbound-audit-observer-report)))))

(format t "~%A1 tool causality tests: ~d passed, ~d failed.~%"
        *a1-tool-pass* *a1-tool-fail*)
(when (plusp *a1-tool-fail*) (error "A1 tool causality tests failed"))
