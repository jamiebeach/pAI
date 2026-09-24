;;;; Real authority and operator controls; synthetic execution, no model calls.
;;;; harness: full-system
(in-package :agent)
(defvar *sao-test-count* 0)
(defmacro sao-check (form)
  `(progn (unless ,form (error "Operator activity check failed: ~s" ',form)) (incf *sao-test-count*)))
(defun sao-fails (fn) (handler-case (progn (funcall fn) nil) (error () t)))
(let* ((path (merge-pathnames (format nil "operator-activity-~d-~d.sqlite3" (get-universal-time) (get-internal-real-time)) (test-state-dir)))
       (backend (make-sqlite-storage path))
       (*conscious-recursive-mind-agent-id* "fixture")
       (*conscious-conversation-persona-profile* (obj "persona_id" "persona"))
       (saved (mapcar (lambda (s) (cons s (symbol-function s)))
                     '(conscious-recursive-mind-submit %recursive-operator-admit %recursive-run-root-locked
                       %recursive-maybe-resolve-graph-confirmation)))
       (ordinary 0) (counts nil) (fail-next nil) (terminal-fail-next nil))
  (unwind-protect
       (progn
         (storage-prepare-activity-index backend)
         (%sqlite-authority-install backend path backend path "fixture")
         (log-event "user-message" (obj "text" "Implement parser" "channel" "web"
                                        "metadata" (obj "persona_id" "persona" "interaction_id" "one")))
         (log-event "agent-message" (obj "text" "Parser investigation") :caused-by 1)
         (setf (symbol-function 'conscious-recursive-mind-submit)
               (lambda (&rest args) (declare (ignore args)) (incf ordinary) (obj "status" "ordinary"))
               (symbol-function '%recursive-maybe-resolve-graph-confirmation)
               (lambda (&rest args) (declare (ignore args)) nil)
               (symbol-function '%recursive-operator-admit)
               (lambda (prompt channel &key activity-reference-event-id
                                             recovery-of-event-id)
                 (let ((id (log-event "user-message"
                                     (obj "text" prompt "channel" channel
                                          "metadata" (obj "persona_id" "persona" "interaction_id" "fixture"
                                                          "activity_reference_event_id" activity-reference-event-id
                                                          "recovery_of_event_id" (or recovery-of-event-id :null))))))
                   (values id "fixture")))
               (symbol-function '%recursive-run-root-locked)
               (lambda (root interaction &key channel content)
                 (declare (ignore interaction content))
                 (when fail-next (setf fail-next nil) (error "Synthetic interruption before execution"))
                 (if terminal-fail-next
                     (progn
                       (setf terminal-fail-next nil)
                       (log-event "recursive-root-failed"
                                  (obj "schema_version" 1
                                       "thread_id" (format nil "thread:user:~d" root)
                                       "root_kind" "user" "stage" "context-open"
                                       "error_code" "recursive-context-open-failed"
                                       "reason" "Synthetic pre-provider context failure"
                                       "condition_type" "SIMPLE-ERROR"
                                       "runtime_revision" "fixture" "failed_at" 1)
                                  :caused-by root)
                       (obj "status" "failed" "user_event_id" root
                            "error_code" "recursive-context-open-failed"))
                     (progn
                       (let ((packet (sustained-activity-for-admitted-root (event-read-event root) "fixture" "persona" channel)))
                         (push (length (gethash "exchanges" packet)) counts))
                       (log-event "agent-message" (obj "text" "Synthetic completed correction") :caused-by root)
                       (obj "status" "replied" "user_event_id" root)))))
         (flet ((command (action) (sustained-activity-operator-command backend (list action) "web" "operator:web"))
                (submit () (sustained-activity-operator-submit backend "Continue correction" "web" "operator:web")))
           (sao-check (search "No sustained" (command "status")))
           (submit)
           (sao-check (= ordinary 1))
           (sao-check (search "started" (command "start")))
           (sao-check (= 1 (length (gethash "root_event_ids" (gethash "payload" (%sao-latest backend "web" "operator:web"))))))
           (sao-check (sao-fails (lambda () (command "start"))))
           (sao-check (equal "replied" (gethash "status" (submit))))
           (submit)
           (sao-check (equal '(2 1) counts))
           (sao-check (search "3 roots" (command "status")))
           (sao-check (search "Synthetic completed correction" (command "window")))
           (sao-check (null (%sao-latest backend "web" "unrelated")))
           (command "pause")
           (submit)
           (sao-check (= ordinary 2))
           (event-authority-clear)
           (setf backend (make-sqlite-storage path))
           (%sqlite-authority-install backend path backend path "fixture")
           (sao-check (search "parked" (command "status")))
           (command "resume")
           (sao-check (%sao-active-p (%sao-latest backend "web" "operator:web")))
           (command "complete")
           (sao-check (not (%sao-active-p (%sao-latest backend "web" "operator:web"))))
           (command "resume")
           (setf fail-next t)
           (sao-check (sao-fails #'submit))
           (sao-check (search "4 roots" (command "status")))
           (let ((head (storage-head-position backend)))
             (sao-check (sao-fails #'submit))
             (sao-check (= head (storage-head-position backend))))
           (sao-check (search "replied" (command "recover")))
           (sao-check (search "working-history" (command "window")))
           (sao-check (= 3 (first counts)))
           ;; A durable pre-provider failure is terminal for its original root.
           ;; Explicit recovery admits a linked retry instead of replaying the
           ;; failed projection forever.
           (setf terminal-fail-next t)
           (let* ((failed (submit))
                  (failed-id (gethash "user_event_id" failed)))
             (sao-check (string= "failed" (gethash "status" failed "")))
             (sao-check (search "replied" (command "recover")))
             (let* ((latest (%sao-latest backend "web" "operator:web"))
                    (roots (gethash "root_event_ids" (gethash "payload" latest)))
                    (retry-id (aref roots (1- (length roots))))
                    (retry-meta (gethash "metadata"
                                         (gethash "payload" (event-read-event retry-id)))))
               (sao-check (not (= failed-id retry-id)))
               (sao-check (= failed-id (gethash "recovery_of_event_id" retry-meta)))
               (sao-check (= 6 (length roots)))))
           ;; Crash between user admission and membership append is recoverable.
           (multiple-value-bind (root ignored)
               (%recursive-operator-admit "Admitted but not enrolled" "web"
                                          :activity-reference-event-id (gethash "id" (%sao-latest backend "web" "operator:web")))
             (declare (ignore ignored))
             (sao-check (search "replied" (sustained-activity-operator-command
                                           backend (list "recover" (write-to-string root)) "web" "operator:web")))
             (sao-check (search "7 roots" (command "status")))
             (sao-check (= 6 (first counts))))
           ;; A failed root can also be missing its membership revision.
           (multiple-value-bind (root interaction)
               (%recursive-operator-admit "Synthetic unenrolled failed turn" "web"
                 :activity-reference-event-id (gethash "id" (%sao-latest backend "web" "operator:web")))
             (setf terminal-fail-next t)
             (%recursive-run-root-locked root interaction :channel "web" :content "Synthetic unenrolled failed turn")
             (sao-check (search "replied" (sustained-activity-operator-command
                                          backend (list "recover" (write-to-string root)) "web" "operator:web")))
             (let* ((latest (%sao-latest backend "web" "operator:web"))
                    (payload (gethash "payload" latest))
                    (roots (gethash "root_event_ids" payload))
                    (retry (event-read-event (aref roots (1- (length roots)))))
                    (meta (gethash "metadata" (gethash "payload" retry))))
               (sao-check (= 9 (length roots)))
               (sao-check (= root (gethash "recovery_of_event_id" meta)))
               (sao-check (= (gethash "previous_reference_event_id" payload)
                             (gethash "activity_reference_event_id" meta)))))))
    (dolist (pair saved) (setf (symbol-function (car pair)) (cdr pair)))
    (event-authority-clear)
    (storage-close backend)))
(format t "Sustained operator activity: ~d passed, 0 failed~%" *sao-test-count*)
