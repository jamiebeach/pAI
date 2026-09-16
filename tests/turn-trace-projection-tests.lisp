(ql:quickload '(:shasht) :silent t)

(defpackage :agent (:use :cl))
(in-package :agent)

(defun obj (&rest key-values)
  (let ((table (make-hash-table :test #'equal)))
    (loop for (key value) on key-values by #'cddr
          do (setf (gethash key table) value))
    table))

(load (test-source "turn-trace-projection.lisp"))

(in-package :cl-user)
(defvar *turn-trace-pass* 0)
(defvar *turn-trace-fail* 0)
(defun turn-trace-check (name condition)
  (if condition
      (progn (incf *turn-trace-pass*) (format t "PASS ~a~%" name))
      (progn (incf *turn-trace-fail*) (format t "FAIL ~a~%" name))))
(defun turn-trace-error (thunk)
  (handler-case (progn (funcall thunk) nil) (error (condition) condition)))

(let* ((ordinary (agent:turn-trace-fixture "ordinary"))
       (attempts (gethash "provider_attempts" ordinary))
       (serialized (shasht:write-json ordinary nil)))
  (turn-trace-check "ordinary fixture is exactly linked"
                    (and (string= "exact" (gethash "physical_attempts_status" ordinary))
                         (= 1 (length attempts))))
  (turn-trace-check "ordinary fixture exposes provider model and usage"
                    (let ((attempt (aref attempts 0)))
                      (and (string= "Novita" (gethash "provider" attempt))
                           (string= "xiaomi/mimo-v2.5" (gethash "model" attempt))
                           (= 23729 (gethash "total_tokens" attempt))
                           (= 0.0038358977 (gethash "cost_usd" attempt)))))
  (turn-trace-check "ordinary first output is projected"
                    (= 4840 (gethash "first_public_output_ms" ordinary)))
  (turn-trace-check "private request response and error bodies are absent"
                    (and (null (search "PRIVATE-CANARY" serialized))
                         (null (search "PRIVATE-RESPONSE" serialized))
                         (null (search "PRIVATE-ERROR" serialized))
                         (null (search "messages" serialized))
                         (null (search "response" serialized))))
  (turn-trace-check "projection is explicitly content-free"
                    (null (gethash "private_content_included" ordinary))))

(let* ((trace (agent:turn-trace-fixture "pathological-memory-loop"))
       (growth (gethash "growth" trace))
       (totals (gethash "totals" trace))
       (repeated (gethash "repeated_tools" trace))
       (attempts (gethash "provider_attempts" trace)))
  (turn-trace-check "pathological duration and first output remain exact"
                    (and (= 259705 (gethash "duration_ms" trace))
                         (= 257801 (gethash "first_public_output_ms" trace))))
  (turn-trace-check "prompt and message growth are visible"
                    (and (= 25563 (gethash "first_prompt_tokens" growth))
                         (= 49364 (gethash "last_prompt_tokens" growth))
                         (= 70 (gethash "first_message_count" growth))
                         (= 92 (gethash "last_message_count" growth))))
  (turn-trace-check "repeated search-memory is diagnosed"
                    (and (= 1 (length repeated))
                         (string= "search-memory" (gethash "tool" (aref repeated 0)))
                         (= 2 (gethash "count" (aref repeated 0)))))
  (turn-trace-check "physical provider attempts remain separate"
                    (and (= 3 (length attempts))
                         (string= "DeepInfra" (gethash "provider" (aref attempts 0)))
                         (string= "Novita" (gethash "provider" (aref attempts 2)))))
  (turn-trace-check "physical cost and tokens sum without logical-span reuse"
                    (and (= (+ 25624 27775 49725)
                            (gethash "provider_attempt_total_tokens" totals))
                         (< (abs (- (+ 0.00218592 0.0023228 0.00468776)
                                    (gethash "provider_attempt_cost_usd" totals)))
                            1.0d-9)))
  (turn-trace-check "spans are ordered by start offset"
                    (loop with prior = -1
                          for span across (gethash "spans" trace)
                          for current = (gethash "start_offset_ms" span)
                          always (prog1 (>= current prior) (setf prior current)))))

(turn-trace-check "fixture index is bounded and enumerated"
                  (= 2 (length (agent:turn-trace-fixture-index))))
(turn-trace-check "unknown fixture fails closed"
                  (turn-trace-error
                   (lambda () (agent:turn-trace-fixture "../../production"))))
(turn-trace-check "unsafe trace identity fails closed"
                  (turn-trace-error
                   (lambda () (agent:turn-trace-project #() :turn-id "../bad"))))

(format t "RESULT turn-trace-projection: ~d passed, ~d failed~%"
        *turn-trace-pass* *turn-trace-fail*)
(when (plusp *turn-trace-fail*) (uiop:quit 1))
(uiop:quit 0)
