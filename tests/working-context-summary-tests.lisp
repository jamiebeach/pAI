;;;; harness: full-system
;;;; Holistic synthetic history; no external provider or durable writes.
(in-package :agent)
(load (merge-pathnames "sustained-activity-context-tests.lisp" *load-truename*))

(defun wcs-fixture ()
  (let ((rows (sac-fixture)))
    (dolist (id '(3 12))
      (let ((body (gethash "payload" (find id rows :key (lambda (e) (gethash "id" e))))))
        (setf (gethash "content" body)
              (concatenate 'string (make-string 20000 :initial-element #\x) (gethash "content" body)))))
    (project-sustained-activity-context
     (sac-reference #(1 10 20) 21)
     (append rows (list (sac-event 20 "user-message" (obj "text" "What remains?"))
                        (sac-event 21 "agent-message" (obj "text" "Integration is next.") 20)))
     :defer-budget-p t)))

(defun wcs-response (request)
  (obj "source_digest" (gethash "source_digest" request)
       "claims" (vector
                 (obj "kind" "objective" "basis" "reported" "text" "Preserve empty fields and trailing delimiters."
                      "source_event_ids" #(1 10))
                 (obj "kind" "correction" "basis" "observed" "text" "Initial parser test failed; the corrected parser passed its unit test."
                      "source_event_ids" #(3 12))
                 (obj "kind" "pending" "basis" "reported" "text" "Integration testing remains unperformed."
                      "source_event_ids" #(13))
                 (obj "kind" "next-step" "basis" "planned" "text" "Run integration tests."
                      "source_event_ids" #(13)))
       "excerpts" (vector (obj "event_id" 12 "text" "Parser tests passed; integration not run."))))

(defun wcs-fails (fn) (handler-case (progn (funcall fn) nil) (error () t)))

(let* ((packet (wcs-fixture)) (copy (%sac-copy packet))
       (request (prepare-working-context-summary packet 2))
       (response (wcs-response request))
       (applied (apply-working-context-summary packet request response))
       (text (gethash "content" (aref (gethash "messages" (aref applied 0)) 0))))
  (sac-check (equalp #(1 10) (gethash "root_event_ids" (gethash "source" request))))
  (sac-check (equalp #(1 2 3 5 10 11 12 13) (gethash "source_event_ids" (gethash "source" request))))
  (sac-check (= 8 (length (gethash "events" (gethash "source" request)))))
  (sac-check (not (search "PROVIDER-PRIVATE" (shasht:write-json request nil))))
  (sac-check (not (search "What remains?" (shasht:write-json request nil))))
  (sac-check (equalp request (prepare-working-context-summary (%sac-copy packet) 2)))
  (sac-check (equalp packet copy))
  (sac-check (equalp response (validate-working-context-summary request response)))
  (sac-check (= 3 (length applied)))
  (sac-check (= 1 (length (gethash "messages" (aref applied 0)))))
  (sac-check (= 0 (length (gethash "messages" (aref applied 1)))))
  (sac-check (equalp (aref applied 2) (aref (gethash "exchanges" packet) 2)))
  (sac-check (search "events 3, 12" text))
  (sac-check (search "Integration testing remains unperformed" text))
  (sac-check (search "Exact excerpt, event 12" text))
  (sac-check (search "Potentially lossy" text))
  (sac-check (< (%sac-exchanges-size applied) (/ (%sac-exchanges-size (gethash "exchanges" packet)) 4)))
  (dolist (n '(0 3 4))
    (sac-check (wcs-fails (lambda () (prepare-working-context-summary packet n)))))
  (sac-check (wcs-fails (lambda () (prepare-working-context-summary packet 2 :maximum-source-bytes 1))))
  (sac-check (wcs-fails (lambda () (validate-working-context-summary
                                    (prepare-working-context-summary packet 2 :maximum-output-characters 256) response))))
  (dolist (mutator
           (list (lambda (r) (setf (gethash "source_digest" r) "stale"))
                 (lambda (r) (setf (gethash "role" r) "system"))
                 (lambda (r) (setf (gethash "claims" r) #()))
                 (lambda (r) (setf (gethash "source_event_ids" (aref (gethash "claims" r) 0)) #()))
                 (lambda (r) (setf (gethash "source_event_ids" (aref (gethash "claims" r) 0)) #(6)))
                 (lambda (r) (setf (gethash "source_event_ids" (aref (gethash "claims" r) 0)) #(20)))
                 (lambda (r) (setf (gethash "source_event_ids" (aref (gethash "claims" r) 0)) #(1 1)))
                 (lambda (r) (setf (gethash "basis" (aref (gethash "claims" r) 0)) "certain"))
                 (lambda (r) (setf (gethash "text" (aref (gethash "excerpts" r) 0)) "All integration tests passed"))
                 (lambda (r) (setf (gethash "event_id" (aref (gethash "excerpts" r) 0)) 3))))
    (let ((bad (%sac-copy response)))
      (funcall mutator bad)
      (sac-check (wcs-fails (lambda () (validate-working-context-summary request bad))))))
  (dolist (key '("agent_id" "activity_id"))
    (let ((other (%sac-copy packet)))
      (setf (gethash key other) "unrelated")
      (sac-check (wcs-fails (lambda () (apply-working-context-summary other request response))))))
  (let ((changed (%sac-copy packet)))
    (setf (gethash "content" (aref (gethash "messages" (aref (gethash "exchanges" changed) 0)) 0)) "Changed original")
    (sac-check (wcs-fails (lambda () (apply-working-context-summary changed request response)))))
  (let ((derived (%sac-copy packet)))
    (setf (gethash "exchanges" derived) applied)
    (sac-check (wcs-fails (lambda () (prepare-working-context-summary derived 2)))))
  ;; Link validation cannot prove entailment. Expose that limitation explicitly.
  (let ((unsupported (%sac-copy response)))
    (setf (gethash "text" (aref (gethash "claims" unsupported) 0)) "A deliberately unsupported assertion")
    (sac-check (hash-table-p (validate-working-context-summary request unsupported))))
  (let ((calls 0))
    (multiple-value-bind (fitted report)
        (fit-holistic-working-context packet #'%sac-exchanges-size 16000
                                      (lambda (r) (incf calls) (wcs-response r)))
      (sac-check (= 1 calls))
      (sac-check (equal "accepted" (gethash "status" report)))
      (sac-check (eq :false (gethash "semantic_faithfulness_verified" report)))
      (sac-check (equalp applied fitted))))
  (let ((calls 0))
    (multiple-value-bind (fitted report)
        (fit-holistic-working-context packet #'%sac-exchanges-size 16000
                                      (lambda (r) (declare (ignore r)) (incf calls) (error "Private provider error")))
      (sac-check (= 1 calls))
      (sac-check (equalp fitted (gethash "exchanges" packet)))
      (sac-check (equal "summary-unavailable-or-invalid" (gethash "status" report)))
      (sac-check (not (search "Private provider error" (shasht:write-json report nil))))))
  (let ((calls 0))
    (fit-holistic-working-context packet #'%sac-exchanges-size 1000000
                                 (lambda (r) (incf calls) (wcs-response r)))
    (sac-check (zerop calls)))
  (multiple-value-bind (fitted report)
      (fit-holistic-working-context packet (lambda (e) (declare (ignore e)) 100000) 60000 #'wcs-response)
    (sac-check (equal "not-smaller" (gethash "status" report)))
    (sac-check (equalp fitted (gethash "exchanges" packet))))
  (sac-check (equalp packet copy)))

;; Integration into the actual complete-request fitter: callback is opt-in.
(let ((saved (symbol-function '%conversation-model-messages))
      (*conscious-conversation-max-output-tokens* nil)
      (calls 0))
  (unwind-protect
       (progn
         (setf (symbol-function '%conversation-model-messages)
               (lambda (opened ignored prompt) (declare (ignore opened ignored))
                 (list (obj "role" "system" "content" "Current instructions")
                       (obj "role" "user" "content" "Retrieved context")
                       (obj "role" "user" "content" prompt))))
         (let* ((packet (wcs-fixture)) (opened (obj "sustained_activity" packet))
                (profile (obj "context_capacity_tokens" 22000 "working_context_output_reserve_tokens" 2000)))
           (multiple-value-bind (messages budget fitted)
               (%recursive-fit-working-request opened "Continue" nil nil #()
                  :model "fixture" :endpoint "http://127.0.0.1:9999/v1/chat/completions" :profile profile
                  :summary-provider (lambda (r) (incf calls) (wcs-response r)))
             (sac-check (= 1 calls))
             (sac-check (equal "ready" (gethash "status" budget)))
             (sac-check (equal "accepted" (gethash "status" (gethash "holistic_summary" budget))))
             (sac-check (search "Integration testing remains unperformed" (shasht:write-json messages nil)))
             (sac-check (equalp (aref (gethash "exchanges" packet) 2) (aref (gethash "exchanges" fitted) 2)))
             (sac-check (equal "Continue" (gethash "content" (car (last messages))))))
           (let* ((large-tool (vector (obj "type" "function" "function"
                                           (obj "name" "synthetic-tool"
                                                "description" (make-string 30000 :initial-element #\z)))))
                  (budget (nth-value 1
                           (%recursive-fit-working-request
                            opened "Continue" nil nil large-tool
                            :model "fixture" :endpoint "http://127.0.0.1:9999/v1/chat/completions"
                            :profile profile))))
             (sac-check (equal "over-budget" (gethash "status" budget))))
           (%recursive-fit-working-request opened "Continue" nil nil #()
              :model "fixture" :endpoint "http://127.0.0.1:9999/v1/chat/completions" :profile profile)
           (sac-check (= 1 calls))))
    (setf (symbol-function '%conversation-model-messages) saved)))
(format t "Holistic working context: ~d passed, 0 failed~%" *sac-checks*)

;; Accepted projections survive a process/backend reopen and suppress regeneration.
(let* ((path (merge-pathnames "working-summary-cache.sqlite3" (test-state-dir)))
       (packet (wcs-fixture)) (request (prepare-working-context-summary packet 2))
       (calls 0) (backend nil))
  (when (probe-file path) (delete-file path))
  (setf backend (make-sqlite-derived-storage path))
  (unwind-protect
       (let ((provider (make-cached-working-context-summary-provider
                        backend "fixture-model-v1"
                        (lambda (r) (incf calls) (wcs-response r))
                        :provenance-fn (lambda () (obj "model" "fixture" "usage" :null)))))
         (sac-check (equalp (wcs-response request) (funcall provider request)))
         (sac-check (= 1 calls))
         (sac-check (equalp (wcs-response request) (funcall provider request)))
         (sac-check (= 1 calls)))
    (storage-close backend))
  (setf backend (make-sqlite-derived-storage path))
  (unwind-protect
       (progn
         (let ((provider (make-cached-working-context-summary-provider
                          backend "fixture-model-v1"
                          (lambda (r) (incf calls) (wcs-response r)))))
           (sac-check (equalp (wcs-response request) (funcall provider request)))
           (sac-check (= 1 calls)))
         (let ((provider (make-cached-working-context-summary-provider
                          backend "fixture-model-v2"
                          (lambda (r) (incf calls) (wcs-response r)))))
           (sac-check (hash-table-p (funcall provider request)))
           (sac-check (= 2 calls))))
    (storage-close backend)))
(format t "Holistic working context with persistence: ~d passed, 0 failed~%" *sac-checks*)
