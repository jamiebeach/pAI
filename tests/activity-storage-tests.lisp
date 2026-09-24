;;;; Original ledger activity storage and process restart qualification.
;;;; harness: full-system
(in-package :agent)
(defvar *ast-checks* 0)
(defmacro ast-check (condition)
  `(progn (unless ,condition (error "Activity check ~d failed: ~s" (1+ *ast-checks*) ',condition))
          (incf *ast-checks*)))
(defun ast-fails (thunk) (handler-case (progn (funcall thunk) nil) (error () t)))
(defun ast-append (backend type payload &optional root (agent "activity-fixture"))
  (storage-append-event backend type payload :agent-id agent :caused-by (or root :null)
                        :occurred-at "2026-01-01T12:00:00Z"))
(defun ast-reference ()
  (obj "schema_version" 1 "activity_id" "work:parser" "persona_id" "fixture-persona"
       "channel" "fixture-channel" "resource_id" "fixture-conversation"
       "retention_policy" "high-bandwidth" "root_event_ids" #(1 5)
       "evidence_event_ids" #(1 5) "previous_reference_event_id" :null
       "actor" "operator" "reason" "Continue the same parser exercise"
       "attention_state" "interrupted" "completion_state" "open"
       "completion_confidence" :null))
(defun ast-read (backend &rest args)
  (apply #'storage-read-activity-context backend 7 :agent-id "activity-fixture"
         :through-event-id 7 args))
(defun ast-packet (backend)
  (multiple-value-bind (ref rows report) (ast-read backend)
    (ast-check (equal "complete" (gethash "status" report)))
    (ast-check (equal '(1 2 3 4 5 6) (mapcar (lambda (e) (gethash "id" e)) rows)))
    (ast-check (stringp (gethash "reference_hash" report)))
    (let ((packet (project-sustained-activity-context ref rows)))
      (ast-check (equal "ready" (gethash "status" packet)))
      (ast-check (search "FINAL-RESULT-MARKER" (shasht:write-json packet nil)))
      (ast-check (search "run-parser-tests" (shasht:write-json packet nil)))
      (ast-check (equal "not-assessed" (gethash "activity_completion" packet)))
      packet)))

(if (boundp 'cl-user::*activity-reopen-fixture*)
    (let* ((path (symbol-value 'cl-user::*activity-reopen-fixture*))
           (backend (make-sqlite-storage-read-only path)))
      (unwind-protect
           (let* ((packet (ast-packet backend))
                  (expected (with-open-file (s (make-pathname :type "json" :defaults path))
                              (shasht:read-json s))))
             (ast-check (equalp expected packet))
             (format t "ACTIVITY-REOPEN-PASS~%"))
        (storage-close backend)))
    (let* ((path (merge-pathnames (format nil "activity-~a.sqlite3" (gensym)) (test-state-dir)))
           (packet-path (make-pathname :type "json" :defaults path))
           (backend nil))
      ;; A fresh Lisp process repeats GENSYM's initial sequence. The preceding
      ;; run intentionally corrupts this disposable database; never reopen it.
      (dolist (candidate (list path packet-path
                               (pathname (concatenate 'string (namestring path) "-wal"))
                               (pathname (concatenate 'string (namestring path) "-shm"))))
        (when (probe-file candidate) (delete-file candidate)))
      (setf backend (make-sqlite-storage path))
      (unwind-protect
           (progn
             (ast-append backend "user-message"
                         (obj "text" "Fix the parser" "channel" "fixture-channel"
                              "metadata" (obj "persona_id" "fixture-persona")))
             (ast-append backend "model-response"
                         (obj "status" "accepted" "model_call_id" "m:1"
                              "assistant_message"
                              (obj "content" :null "reasoning_content" "PRIVATE-REASONING"
                                   "tool_calls" (vector (obj "id" "c:1" "type" "function"
                                                            "function" (obj "name" "bash" "arguments"
                                                                            "{\"command\":\"run-parser-tests\"}"))))) 1)
             (ast-append backend "recursive-tool-result"
                         (obj "model_call_id" "m:1" "tool_call_id" "c:1" "tool_name" "bash"
                              "execution_status" "executed"
                              "content" (concatenate 'string (make-string 2400 :initial-element #\x)
                                                     "FINAL-RESULT-MARKER")) 1)
             (ast-append backend "agent-message" (obj "text" "A failing test identifies the fix") 1)
             (ast-append backend "user-message"
                         (obj "text" "Continue; also preserve whitespace" "channel" "fixture-channel"
                              "metadata" (obj "persona_id" "fixture-persona")))
             (ast-append backend "agent-message" (obj "text" "Correction retained; not done") 5)
             (ast-append backend "sustained-activity-revised" (ast-reference) 5)
             ;; Missing index must fail; read does not secretly build it.
             (ast-check (ast-fails (lambda () (ast-read backend))))
             (ast-check (ast-fails
                         (lambda () (storage-root-has-event-type-p
                                     backend "activity-fixture" 1 "absent"
                                     :through-position 7))))
             (ast-check (not (storage-activity-index-ready-p backend)))
             (storage-prepare-activity-index backend)
             (storage-prepare-activity-index backend)
             (ast-check (storage-activity-index-ready-p backend))
             (ast-check
              (equal '(3 4)
                     (mapcar (lambda (event) (gethash "id" event))
                             (%sqlite-authority-root-recent
                              backend "activity-fixture" 1
                              '("model-response" "recursive-tool-result"
                                "agent-message") 2 7))))
             (ast-check
              (equal '(2 3 4)
                     (mapcar (lambda (event) (gethash "id" event))
                             (%sqlite-authority-root-recent
                              backend "activity-fixture" 1
                              '("model-response" "recursive-tool-result"
                                "agent-message") 3 7))))
             (ast-check
              (ast-fails
               (lambda ()
                 (%sqlite-authority-root-recent
                  backend "activity-fixture" 7 '("agent-message") 2 1))))
             (let ((boundary (storage-authority-boundary
                              backend :agent-id "activity-fixture")))
               (multiple-value-bind (present witness)
                   (storage-root-has-event-type-p
                    backend "activity-fixture" 1
                    '("model-response" "recursive-tool-result")
                    :source-boundary boundary)
                 (ast-check (and present (= 2 (gethash "id" witness)))))
               (multiple-value-bind (present witness)
                   (storage-root-has-event-type-p
                    backend "activity-fixture" 1
                    '("model-response" "agent-message")
                    :source-boundary boundary :newest-p t)
                 (ast-check (and present (= 4 (gethash "id" witness)))))
               (ast-check (not (storage-root-has-event-type-p
                                backend "activity-fixture" 1 "absent"
                                :source-boundary boundary)))
               (ast-check (not (storage-root-has-event-type-p
                                backend "other-agent" 1 "model-response"
                                :through-position 7)))
               (ast-check (not (storage-root-has-event-type-p
                                backend "activity-fixture" 1 "model-response"
                                :through-position 1)))
               (ast-append backend "fixture-presence" (obj "text" "late") 1)
               (ast-check (not (storage-root-has-event-type-p
                                backend "activity-fixture" 1 "fixture-presence"
                                :source-boundary boundary)))
               (ast-check (storage-root-has-event-type-p
                           backend "activity-fixture" 1 "fixture-presence"
                           :through-position
                           (storage-head-position backend :agent-id "activity-fixture")))
               (ast-check (ast-fails
                           (lambda () (storage-root-has-event-type-p
                                       backend "other-agent" 1 "model-response"
                                       :source-boundary boundary)))))
             (%with-sqlite-statement
                 (s (%sqlite-handle backend :test)
                    "EXPLAIN QUERY PLAN SELECT event_id FROM pai_events INDEXED BY pai_activity_root_idx WHERE agent_id='activity-fixture' AND json_extract(event_json,'$.caused_by')=1 AND event_type IN ('model-response','recursive-tool-result','agent-message','recursive-root-failed') AND storage_sequence<=7 ORDER BY event_type,storage_sequence LIMIT 100" :test)
               (%sqlite-step (%sqlite-handle backend :test) s :test +sqlite-row+)
               (let ((plan (%sqlite-column-text s 3)))
                 (ast-check (and (search "SEARCH" plan) (search "pai_activity_root_idx" plan)
                                 (search "<expr>" plan) (search "event_type" plan)))))
             (let ((before (storage-head-position backend :agent-id "activity-fixture"))
                   (packet (ast-packet backend)))
               (ast-check (= before (storage-head-position backend :agent-id "activity-fixture")))
               (ast-check (not (search "PRIVATE-REASONING" (shasht:write-json packet nil))))
               (with-open-file (s (make-pathname :type "json" :defaults path)
                                  :direction :output :if-exists :error)
                 (shasht:write-json packet s))
               ;; Later arrivals, including a same-root row, cannot enter the frozen window.
               (ast-append backend "model-request" (obj "large" (make-string 10000 :initial-element #\z)) 1)
               (ast-append backend "agent-message" (obj "text" "later") 5)
               (ast-check (equalp packet (ast-packet backend)))
               (multiple-value-bind (ref rows report) (ast-read backend :maximum-rows 3)
                 (ast-check (and (null ref) (null rows)
                                 (equal "read-limit-exceeded" (gethash "status" report)))))
               (multiple-value-bind (ref rows report) (ast-read backend :maximum-bytes 100)
                 (ast-check (and (null ref) (null rows)
                                 (equal "read-limit-exceeded" (gethash "status" report)))))
               (ast-check (ast-fails (lambda () (storage-read-activity-context
                                                 backend 7 :agent-id "other-agent" :through-event-id 7))))
               (ast-check (ast-fails (lambda () (storage-read-activity-context
                                                 backend 7 :agent-id "activity-fixture" :through-event-id 6))))
               ;; Rebuildable SQL index, not another membership authority.
               (%sqlite-exec (%sqlite-handle backend :test) "DROP INDEX pai_activity_root_idx" :test)
               (ast-check (ast-fails
                           (lambda () (%sqlite-authority-root-recent
                                       backend "activity-fixture" 1
                                       '("agent-message") 1 7))))
               (ast-check (ast-fails
                           (lambda () (storage-root-has-event-type-p
                                       backend "activity-fixture" 1 "absent"
                                       :through-position 7))))
               (storage-prepare-activity-index backend)
               (ast-check (equalp packet (ast-packet backend))))
             ;; Revised attention/completion assessment is ledger data, not a
             ;; tool-success receipt. Identity cannot change through predecessor.
             (let ((revision (ast-reference)))
               (setf (gethash "previous_reference_event_id" revision) 7
                     (gethash "attention_state" revision) "parked"
                     (gethash "completion_state" revision) "provisionally-complete"
                     (gethash "completion_confidence" revision) 0.8)
               (let* ((event (ast-append backend "sustained-activity-revised" revision 5))
                      (id (gethash "id" event)))
                 (multiple-value-bind (ref rows report)
                     (storage-read-activity-context backend id :agent-id "activity-fixture" :through-event-id id)
                   (declare (ignore rows))
                   (ast-check (equal "complete" (gethash "status" report)))
                   (ast-check (equal "parked" (gethash "attention_state" ref)))
                   (ast-check (equal "provisionally-complete" (gethash "completion_state" ref))))))
             (dolist (bad '("persona_id" "channel" "actor" "completion_confidence" "resource_id"))
               (let ((revision (ast-reference)))
                 (setf (gethash "previous_reference_event_id" revision) 7
                       (gethash bad revision) "invalid")
                 (let ((id (gethash "id" (ast-append backend "sustained-activity-revised" revision 5))))
                   (ast-check (ast-fails (lambda ()
                                           (storage-read-activity-context backend id
                                            :agent-id "activity-fixture" :through-event-id id)))))))
             (storage-close backend)
             ;; Fresh SBCL process, read-only database, no inherited Lisp state.
             (multiple-value-bind (output errors code)
                 (uiop:run-program
                  (list "sbcl" "--dynamic-space-size" "2048" "--non-interactive"
                        "--load" "/opt/quicklisp/setup.lisp" "--eval" "(require :asdf)"
                        "--eval" (format nil "(defparameter cl-user::*activity-reopen-fixture* ~s)" (namestring path))
                        "--load" (namestring (merge-pathnames "tests/isolated-harness.lisp" cl-user::*pai-root*)))
                  :output :string :error-output :string :ignore-error-status t)
               (declare (ignore errors))
               (ast-check (and (zerop code) (search "ACTIVITY-REOPEN-PASS" output)
                               (not (search "HARNESS-ERR" output)))))
             (setf backend (make-sqlite-storage path))
             ;; Tampering must fail integrity verification, not yield partial context.
             (%sqlite-exec (%sqlite-handle backend :test)
                           "UPDATE pai_events SET event_json=replace(event_json,'FINAL-RESULT-MARKER','CORRUPT-RESULT') WHERE event_id=3" :test)
             (ast-check (ast-fails (lambda () (ast-read backend))))
             (ast-check (ast-fails
                         (lambda () (storage-root-has-event-type-p
                                     backend "activity-fixture" 1 "recursive-tool-result"
                                     :through-position 7)))))
        (storage-close backend))))
(let* ((path (merge-pathnames (format nil "activity-import-~a.sqlite3" (gensym))
                              (test-state-dir)))
       (source (make-pathname :type "jsonl" :defaults path))
       (backend nil))
  (dolist (candidate (list path source
                           (pathname (concatenate 'string (namestring path) "-wal"))
                           (pathname (concatenate 'string (namestring path) "-shm"))))
    (when (probe-file candidate) (delete-file candidate)))
  (with-open-file (stream source :direction :output :if-exists :error)
    (write-line (%storage-json
                 (obj "id" 10 "type" "fixture-root"
                      "timestamp" "2026-01-01T12:00:00Z"
                      "caused_by" :null "payload" (obj "text" "root")))
                stream)
    (write-line (%storage-json
                 (obj "id" 11 "type" "fixture-imported-type"
                      "timestamp" "2026-01-01T12:00:01Z"
                      "caused_by" 10 "payload" (obj "text" "child")))
                stream))
  (setf backend (make-sqlite-storage path))
  (unwind-protect
       (progn
         (sqlite-import-jsonl backend source :legacy-agent-id "import-fixture")
         (ast-check (ast-fails
                     (lambda () (storage-root-has-event-type-p
                                 backend "import-fixture" 10
                                 "fixture-imported-type" :through-position 2))))
         (storage-prepare-activity-index backend)
         (multiple-value-bind (present witness)
             (storage-root-has-event-type-p
              backend "import-fixture" 10 "fixture-imported-type"
              :source-boundary
              (storage-authority-boundary backend :agent-id "import-fixture"))
           (ast-check (and present (= 11 (gethash "id" witness)))))
         (ast-check (not (storage-root-has-event-type-p
                          backend "import-fixture" 10 "fixture-imported-type"
                          :through-position 1))))
    (storage-close backend)))
(format t "Activity storage: ~d passed, 0 failed~%" *ast-checks*)
