;;;; conscious-lifecycle-tests.lisp -- Q5 pure lifecycle projection.
;;;;
;;;; Written before lifecycle.lisp. The first run fails on the absent subject;
;;;; the implementation must then satisfy the complete event/rebuild probes.

(in-package :agent)

(defvar *clt-passed* 0)
(defvar *clt-failed* 0)

(defun clt-check (name condition)
  (if condition
      (progn (incf *clt-passed*) (format t "PASS ~a~%" name))
      (progn (incf *clt-failed*) (format t "FAIL ~a~%" name))))

(defun clt-event (id transition &key (lifecycle-id "work:one")
                                         (request-id (format nil "request:~a" id))
                                         (kind "private-exploration")
                                         (origin "revision:a")
                                         (actor "revision:a")
                                         (source :null) (checkpoint :null)
                                         (reason "fixture")
                                         (agent-id "q5-dev"))
  (obj "schema_version" 1 "id" id "timestamp" (+ 1000 id)
       "type" "conscious-lifecycle-transition" "agent_id" agent-id
       "caused_by" (if (eq source :null) :null source)
       "payload"
       (obj "schema_version" 1 "request_id" request-id
            "lifecycle_id" lifecycle-id "lifecycle_kind" kind
            "transition" transition
            "origin_runtime_revision" origin
            "actor_runtime_revision" actor
            "source_event_id" source "checkpoint_ref" checkpoint
            "reason_code" reason "occurred_at" (+ 1000 id))))

(defun clt-rejected-event (id &key (lifecycle-id "work:one")
                                   (request-id (format nil "reject:~a" id))
                                   (source 90) (claimed "revision:old")
                                   (active "revision:a"))
  (obj "schema_version" 1 "id" id "timestamp" (+ 1000 id)
       "type" "conscious-lifecycle-result-rejected" "agent_id" "q5-dev"
       "caused_by" source
       "payload"
       (obj "schema_version" 1 "request_id" request-id
            "lifecycle_id" lifecycle-id "source_event_id" source
            "claimed_runtime_revision" claimed
            "active_runtime_revision" active
            "reason_code" "stale-runtime-revision"
            "occurred_at" (+ 1000 id))))

(defun clt-source-event (id &optional (type "fixture-source"))
  (obj "schema_version" 1 "id" id "timestamp" (+ 1000 id)
       "type" type "agent_id" "q5-dev" "caused_by" :null
       "payload" (obj "fixture" t)))

(format t "~%== Q5 pure lifecycle subject ==~%")

(let ((path (merge-pathnames "src/mind/conscious/lifecycle.lisp" *pai-root*)))
  (clt-check "pure lifecycle module exists" (probe-file path))
  (when (probe-file path)
    (load path)

    (let* ((open (clt-event 1 "open"))
           (checkpoint-source (clt-source-event 2))
           (checkpoint (clt-event 3 "checkpoint" :source 2
                                      :checkpoint "artifact:checkpoint-1"))
           (suspend-source (clt-source-event 4))
           (suspend (clt-event 5 "suspend" :source 4
                                   :reason "interactive-barrier"))
           (events (list open checkpoint-source checkpoint
                         suspend-source suspend))
           (projection (conscious-lifecycle-project events :agent-id "q5-dev"))
           (current (conscious-lifecycle-current projection "work:one")))
      (clt-check "open checkpoint suspend rebuilds one suspended lifecycle"
                 (and (string= "suspended" (gethash "status" current))
                      (string= "artifact:checkpoint-1"
                               (gethash "checkpoint_ref" current))
                      (= 1 (gethash "active_count" projection))))
      (clt-check "identical replay is byte-identical"
                 (string= (shasht:write-json projection nil)
                          (shasht:write-json
                           (conscious-lifecycle-project events :agent-id "q5-dev")
                           nil)))
      (clt-check "incremental fold resolves sources without a retained seen map"
                 (let ((fold (%make-lifecycle-fold "q5-dev"))
                       (prior '()))
                   (dolist (event events)
                     (%lifecycle-fold-apply
                      fold event :record-seen-p nil
                      :source-event-before-p
                      (lambda (source-id)
                        (find source-id prior :key (lambda (item)
                                                     (gethash "id" item)))))
                     (push event prior))
                   (and (zerop (hash-table-count
                                (%lifecycle-fold-seen-event-ids fold)))
                        (string= (shasht:write-json projection nil)
                                 (shasht:write-json
                                  (%lifecycle-fold-report fold) nil)))))
      (clt-check "awaiting references are bounded and content-free"
                 (let* ((awaiting (conscious-lifecycle-awaiting projection))
                        (row (and (= 1 (length awaiting)) (aref awaiting 0)))
                        (json (and row (shasht:write-json row nil))))
                   (and row
                        (string= "work:one" (gethash "lifecycle_id" row))
                        (search "checkpoint-1" json)
                        (eq :null (gethash "phase" row))
                        (not (search "payload" json :test #'char-equal)))))

      (let* ((resumed-events
               (append events
                       (list (clt-source-event 6)
                             (clt-event 7 "resume" :source 6
                                          :reason "barrier-cleared"))))
             (resumed (conscious-lifecycle-project resumed-events
                                                   :agent-id "q5-dev")))
        (clt-check "resume continues the suspended lifecycle and checkpoint"
                   (let ((row (conscious-lifecycle-current resumed "work:one")))
                     (and (string= "active" (gethash "status" row))
                          (string= "artifact:checkpoint-1"
                                   (gethash "checkpoint_ref" row)))))
          (let* ((completed-events
                 (append resumed-events
                         (list (clt-source-event 8)
                               (clt-event 9 "complete" :source 8
                                            :reason "result-succeeded"))))
               (completed (conscious-lifecycle-project completed-events
                                                       :agent-id "q5-dev"))
               (row (conscious-lifecycle-current completed "work:one")))
          (clt-check "completion requires and retains its receipt"
                     (and (string= "completed" (gethash "status" row))
                          (= 8 (gethash "terminal_source_event_id" row))
                          (zerop (gethash "active_count" completed))
                          (= 1 (gethash "terminal_count" completed))))
          (clt-check "terminal lifecycle is absent from awaited state"
                     (zerop (length (conscious-lifecycle-awaiting completed))))

          (let* ((illegal-events
                   (append completed-events
                           (list (clt-source-event 10)
                                 (clt-event 11 "resume" :source 10))))
                 (illegal (conscious-lifecycle-project illegal-events
                                                       :agent-id "q5-dev")))
            (clt-check "illegal post-terminal transition is diagnosed not applied"
                       (and (string= "completed"
                                     (gethash "status"
                                              (conscious-lifecycle-current
                                               illegal "work:one")))
                            (equalp #(11) (gethash "invalid_event_ids" illegal))))))))

    (let* ((events (list (clt-event 1 "open")
                         (clt-event 2 "complete" :source 9999)))
           (projection (conscious-lifecycle-project events :agent-id "q5-dev")))
      (clt-check "replay rejects a transition with a fabricated source receipt"
                 (and (string= "active"
                               (gethash "status"
                                        (conscious-lifecycle-current
                                         projection "work:one")))
                      (equalp #(2) (gethash "invalid_event_ids" projection)))))

    (let* ((events (list (clt-event 1 "open")
                         (clt-event 2 "open" :lifecycle-id "work:other"
                                            :agent-id "another-agent")
                         (obj "id" 3 "type" "conscious-lifecycle-transition"
                              "agent_id" "q5-dev" "payload" (obj))))
           (projection (conscious-lifecycle-project events :agent-id "q5-dev")))
      (clt-check "other partitions are ignored and malformed local events diagnosed"
                 (and (= 1 (gethash "active_count" projection))
                      (= 1 (length (gethash "invalid_event_ids" projection))))))

    (let* ((events (list (clt-event 1 "open")
                         (clt-source-event 2 "agent-operation-terminal")
                         (clt-rejected-event 3 :source 2)))
           (projection (conscious-lifecycle-project events :agent-id "q5-dev"))
           (report (conscious-lifecycle-report projection))
           (json (shasht:write-json report nil)))
      (clt-check "rejected asynchronous results remain measured without transition"
                 (and (= 1 (gethash "rejected_result_count" projection))
                      (string= "active"
                               (gethash "status"
                                        (conscious-lifecycle-current projection
                                                                     "work:one")))))
      (clt-check "safe report exposes counts without lifecycle identifiers or refs"
                 (and (= 1 (gethash "active_count" report))
                      (not (search "work:one" json))
                      (not (search "checkpoint" json :test #'char-equal)))))))

(format t "~%~d passed, ~d failed~%" *clt-passed* *clt-failed*)
(when (plusp *clt-failed*) (uiop:quit 1))
