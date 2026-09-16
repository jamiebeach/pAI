;;;; Event-owned generation. Ports supply authenticated sources and durable events.
(in-package :pai.context-graph)

(defparameter +cg-runtime-generation+ "reviewed-private-episodes-v1")

(defstruct (context-graph-runtime (:constructor %make-cg-runtime))
  graph agent-id persona-id ontology revision
  (opens (make-hash-table :test #'eql))
  (terminals (make-hash-table :test #'eql))
  (phases (make-hash-table :test #'equal))
  (covered (make-hash-table :test #'equal))
  (last-event-id 0) (applications 0) (rejections 0))

(defun context-graph-runtime-create (ontology revision agent-id persona-id)
  (%make-cg-runtime :graph (make-context-graph ontology) :ontology (%cg-detach ontology)
                    :revision revision :agent-id agent-id :persona-id persona-id))

(defun context-graph-runtime-json (object)
  (%cg-authority-canonical-json object))

(defun context-graph-runtime-read-json (text)
  (let ((shasht:*read-default-true-value* :true)
        (shasht:*read-default-false-value* :false)
        (shasht:*read-default-null-value* :null))
    (shasht:read-json text)))

(defun %cgro-record (event)
  (context-graph-runtime-read-json (gethash "record_json" (gethash "payload" event))))

(defun %cgro-task (episode mode &optional (protocol "v1") (batch 0))
  (list episode mode protocol batch))
(defun %cgro-record-task (record)
  (%cgro-task (gethash "episode_event_id" record) (gethash "mode" record)
              (gethash "protocol" record "v1") (gethash "batch_index" record 0)))
(defun %cgro-covered-batch-p (runtime episode mode protocol batch)
  (or (gethash (%cgro-task episode mode protocol batch) (context-graph-runtime-covered runtime))
      (and (equal protocol "bounded-v4")
           (some (lambda (old)
                   (equal "context-graph-runtime-reviewed"
                          (gethash (%cgro-task episode mode old batch) (context-graph-runtime-covered runtime))))
                 '("bounded-v2" "bounded-v3")))))
(defun %cgro-source-for-record (runtime record source-fn)
  (let* ((graph (context-graph-runtime-graph runtime))
         (full (funcall source-fn graph (gethash "episode_event_id" record) (gethash "observed_at" record))))
    (if (member (gethash "protocol" record "v1") '("bounded-v2" "bounded-v3" "bounded-v4") :test #'equal)
        (%cgro-batch-context graph full (gethash "batch_index" record) (gethash "mode" record)
                             (gethash "protocol" record)) full)))
(defun %cgro-phase-key (opened phase) (list opened phase))

(defun %cgro-boundary (opened event)
  (%cg-object "episode_id" (gethash "episode_id" (gethash "context" (%cgro-record opened)))
              "opened_boundary_id" (gethash "id" opened)
              "application_event_id" (gethash "id" event)
              "observed_at" (gethash "observed_at" (%cgro-record opened))))

(defun %cgro-rebuild-envelope (runtime opened call-fn)
  (let* ((record (%cgro-record opened))
         (*cgt-protocol* (gethash "protocol" record "v1")))
    (context-graph-generate-reviewed (gethash "context" record)
       (context-graph-runtime-ontology runtime) (context-graph-runtime-revision runtime)
       call-fn :mode (if (equal "correction" (gethash "mode" record)) :correction :staged)
       :quality-review (equal *cgt-protocol* "bounded-v4"))))

(defun %cgro-saved-response (runtime opened phase digest)
  (let* ((row (gethash (%cgro-phase-key opened phase) (context-graph-runtime-phases runtime)))
         (record (and row (%cgro-record row))))
    (unless (and record (equal digest (gethash "request_digest" record))
                 (equal "response" (gethash "outcome" record)))
      (%cg-authority-fail "RUNTIME_PHASE_RECEIPT_MISSING"))
    (%cg-detach (gethash "response" record))))

(defun context-graph-runtime-consume (runtime event source-context-fn)
  "Fold a trusted, authority-ordered event once. Reauthenticate every opened
source against the ledger, not its saved digest. No provider or durable writes."
  (let* ((id (gethash "id" event)) (payload (gethash "payload" event))
         (type (gethash "type" event)))
    (unless (and (integerp id) (> id (context-graph-runtime-last-event-id runtime)))
      (%cg-authority-fail "RUNTIME_EVENT_ORDER_INVALID"))
    (when (and (equal (context-graph-runtime-agent-id runtime) (gethash "agent_id" event))
               (hash-table-p payload)
               (equal (context-graph-runtime-persona-id runtime) (gethash "persona_id" payload))
               (equal +cg-runtime-generation+ (gethash "generation" payload)))
      (let* ((record (%cgro-record event)) (parent (gethash "caused_by" event))
             (opened (gethash parent (context-graph-runtime-opens runtime))))
        (cond
          ((equal type "context-graph-runtime-opened")
           (unless (and (member (gethash "mode" record) '("staged" "correction") :test #'equal)
                        (member (gethash "protocol" record "v1") '("v1" "bounded-v2" "bounded-v3" "bounded-v4") :test #'equal)
                        (equal (gethash "ontology_digest" record)
                               (%cg-authority-digest "runtime-ontology" (context-graph-runtime-ontology runtime)))
                        (equal (gethash "ontology_revision" record) (context-graph-runtime-revision runtime))
                        (equal parent (gethash "episode_event_id" record))
                        (integerp parent) (< parent id)
                        (%cg-authority-equal-p
                         (gethash "context" record)
                         (%cgro-source-for-record runtime record source-context-fn)))
             (%cg-authority-fail "RUNTIME_OPEN_AUTHORITY_INVALID"))
           (when (loop for prior being the hash-values of (context-graph-runtime-opens runtime)
                       thereis (not (gethash (gethash "id" prior) (context-graph-runtime-terminals runtime))))
             (%cg-authority-fail "RUNTIME_CONCURRENT_OPEN_FORBIDDEN"))
           (when (gethash (%cgro-record-task record) (context-graph-runtime-covered runtime))
             (%cg-authority-fail "RUNTIME_TASK_ALREADY_TERMINAL"))
           (setf (gethash id (context-graph-runtime-opens runtime)) event))
          ((member type '("context-graph-runtime-phase" "context-graph-runtime-reviewed" "context-graph-runtime-failed") :test #'equal)
           (unless (and opened (not (gethash parent (context-graph-runtime-terminals runtime))))
             (%cg-authority-fail "RUNTIME_TERMINAL_ORDER_INVALID"))
           (if (equal type "context-graph-runtime-phase")
               (let* ((key (%cgro-phase-key parent (gethash "phase" record)))
                      (prior (gethash key (context-graph-runtime-phases runtime)))
                      (old (and prior (%cgro-record prior))) (outcome (gethash "outcome" record)))
                 (unless (and (member (gethash "phase" record) '("entities" "facts" "correction" "review") :test #'equal)
                              (%cg-authority-digest-p (gethash "request_digest" record))
                              (if (equal outcome "request")
                                  (or (null old) (equal "paused" (gethash "outcome" old)))
                                  (and old (equal "request" (gethash "outcome" old))
                                       (equal (gethash "request_digest" record) (gethash "request_digest" old))
                                       (member outcome '("response" "paused") :test #'equal))))
                   (%cg-authority-fail "RUNTIME_PHASE_ORDER_INVALID"))
                 (setf (gethash key (context-graph-runtime-phases runtime)) event))
               (progn
                 (when (equal type "context-graph-runtime-reviewed")
                   ;; Reconstruct all asks from durable phase results. A saved envelope
                   ;; alone does not establish request/response binding or admission.
                   (let* ((built (%cgro-rebuild-envelope runtime opened
                                   (lambda (phase spec digest) (declare (ignore spec))
                                     (%cgro-saved-response runtime parent phase digest))))
                          (envelope (gethash "value" built)))
                     (unless (and (equal "accepted" (gethash "status" built))
                                  (%cg-authority-equal-p envelope (gethash "envelope" record)))
                       (%cg-authority-fail "RUNTIME_ENVELOPE_BINDING_INVALID"))
                     (let ((result (handler-case
                                     (context-graph-apply-reviewed-generation
                                      (context-graph-runtime-graph runtime) (%cgro-boundary opened event) envelope)
                                     (context-graph-authority-input-error (c)
                                       (%cg-authority-result "rejected" nil (%cg-authority-error-code c))))))
                       (if (and (equal "accepted" (gethash "status" result))
                                (equal "applied" (gethash "status" (gethash "application" (gethash "value" result)))))
                           (incf (context-graph-runtime-applications runtime))
                           (incf (context-graph-runtime-rejections runtime)))
                       (setf (gethash id (context-graph-runtime-terminals runtime)) result))))
                 (setf (gethash parent (context-graph-runtime-terminals runtime)) event
                       (gethash (%cgro-record-task (%cgro-record opened))
                                (context-graph-runtime-covered runtime)) type)))))))
    (setf (context-graph-runtime-last-event-id runtime) id))
  runtime)

(defun context-graph-runtime-step (runtime episode-event-ids source-context-fn call-fn append-fn
                                  &key (now (get-universal-time)) (protocol "v1"))
  "One episode/mode per wake. APPEND-FN(type,payload,cause) must return its
durably reread event. Paused phases resume; ambiguous in-flight calls never retry.
Each episode gets ordinary formation then a separate correction-only task."
  (unless (member protocol '("v1" "bounded-v2" "bounded-v3" "bounded-v4") :test #'equal)
    (%cg-authority-fail "RUNTIME_PROTOCOL_INVALID"))
  (labels ((append-row (type record cause)
             (let ((event (funcall append-fn type
                            (%cg-object "persona_id" (context-graph-runtime-persona-id runtime)
                                        "generation" +cg-runtime-generation+
                                        "record_json" (context-graph-runtime-json record)) cause)))
               (context-graph-runtime-consume runtime event source-context-fn) event))
           (report (status &optional id)
             (%cg-object "schema_version" 1 "status" status "opened_event_id" (or id :null)
                         "generation" +cg-runtime-generation+
                         "applied_tasks" (context-graph-runtime-applications runtime)
                         "rejected_tasks" (context-graph-runtime-rejections runtime))))
    (let ((opened (loop for e being the hash-values of (context-graph-runtime-opens runtime)
                        unless (gethash (gethash "id" e) (context-graph-runtime-terminals runtime)) return e)))
      (unless opened
        (let ((task (loop for episode in (sort (copy-list episode-event-ids) #'<)
                          thereis
                          (loop for mode in '("staged" "correction")
                                ;; A new protocol can succeed a failed v1 task,
                                ;; never silently repeat an already reviewed one.
                                thereis (unless (and (not (equal protocol "v1"))
                                            (equal "context-graph-runtime-reviewed"
                                                   (gethash (%cgro-task episode mode) (context-graph-runtime-covered runtime))))
                                  (loop for batch below (if (equal protocol "v1") 1
                                                            (length (%cgro-source-batches
                                                              (funcall source-context-fn (context-graph-runtime-graph runtime) episode now))))
                                        unless (%cgro-covered-batch-p runtime episode mode protocol batch)
                                          return (list episode mode batch)))))))
          (unless task (return-from context-graph-runtime-step (report "idle")))
          (let* ((record (%cg-object "episode_event_id" (first task) "mode" (second task) "observed_at" now
                                     "ontology_revision" (context-graph-runtime-revision runtime)
                                     "ontology_digest" (%cg-authority-digest "runtime-ontology" (context-graph-runtime-ontology runtime)))))
            (unless (equal protocol "v1")
              (setf (gethash "protocol" record) protocol (gethash "batch_index" record) (third task)))
            (setf (gethash "context" record) (%cgro-source-for-record runtime record source-context-fn))
            (setf opened (append-row "context-graph-runtime-opened"
                          record
                          (first task))))))
      (let* ((id (gethash "id" opened))
             (result
               (handler-case
                   (%cgro-rebuild-envelope runtime opened
                     (lambda (phase spec digest)
                       (let* ((saved (gethash (%cgro-phase-key id phase) (context-graph-runtime-phases runtime)))
                              (record (and saved (%cgro-record saved))))
                         (when record
                           (unless (equal digest (gethash "request_digest" record))
                             (%cg-authority-fail "RUNTIME_PHASE_DIGEST_CHANGED")))
                         ;; Request without a durable response might already be billed.
                         (when (and record (equal "request" (gethash "outcome" record)))
                           (%cg-authority-fail "RUNTIME_CALL_OUTCOME_AMBIGUOUS"))
                         (if (and record (equal "response" (gethash "outcome" record)))
                             (%cg-detach (gethash "response" record))
                             (progn
                               (append-row "context-graph-runtime-phase"
                                 (%cg-object "phase" phase "request_digest" digest "outcome" "request") id)
                               (let ((response (funcall call-fn phase spec digest id)))
                                 (append-row "context-graph-runtime-phase"
                                   (%cg-object "phase" phase "request_digest" digest
                                               "outcome" (if (member response '(:preempted :paused-budget)) "paused" "response")
                                               "response" (if (hash-table-p response) response :null)) id)
                                 response))))))
                 (context-graph-authority-input-error (c) (%cg-authority-result "rejected" nil (%cg-authority-error-code c)))
                 (error (c) (%cg-authority-result "rejected" nil (string-downcase (symbol-name (type-of c))))))))
        (when (member result '(:preempted :paused-budget))
          (return-from context-graph-runtime-step (report (if (eq result :preempted) "preempted" "paused-budget") id)))
        (if (equal "accepted" (gethash "status" result))
            (let* ((event (append-row "context-graph-runtime-reviewed" (%cg-object "envelope" (gethash "value" result)) id))
                   (application (gethash (gethash "id" event) (context-graph-runtime-terminals runtime)))
                   (out (report (if (equal "accepted" (gethash "status" application)) "sealed" "failed") id)))
              (setf (gethash "admission" out) application) out)
            (progn (append-row "context-graph-runtime-failed" (%cg-object "result" result) id)
                   (report "failed" id)))))))
