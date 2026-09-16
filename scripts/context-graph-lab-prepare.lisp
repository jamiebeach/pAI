;;;; Bounded preparation atop the existing production owner; no provider path.
(in-package :agent)

(defun context-graph-lab-prepare (input cuts emit-checkpoint &key (deadline-seconds 180) resume)
  "One offline pass, optionally resumed from an explicitly supplied checkpoint.
EMIT-CHECKPOINT persists sealed state at target cuts or a clean event boundary."
  (let* ((contract (gethash "contract" input))
         (agent-id (gethash "agent_id" contract)) (persona-id (gethash "persona_id" contract))
         (revision (gethash "ontology_revision" contract))
         (generation (gethash "generation" contract))
         (events (map 'list (lambda (row) (gethash "event" row)) (gethash "events" input)))
         (index (make-hash-table :test #'eql))
         (runtime (pai.context-graph:context-graph-runtime-create (%cgel-ontology revision)
                                                               revision agent-id persona-id))
         (owner (%cgel-create-owner generation
                                   (pai.context-graph::context-graph-runtime-graph runtime)
                                   agent-id persona-id revision))
         (started (get-internal-real-time))
         (remaining (sort (copy-list cuts) #'<)) (processed 0) (last-id 0)
         (last-save 0))
    (unless (and (equal "private-preparation-input" (gethash "kind" input))
                 (or (and (equal "reviewed-inference-v8" (gethash "profile" contract))
                          (equal "identity-formation-owner-v8" generation)
                          (equal "identity-formation-v13" (gethash "protocol" contract))
                          (equal "personal-context-core-glm53-v1.2" revision))
                     (and (equal "reviewed-inference-v9" (gethash "profile" contract))
                          (equal "identity-formation-owner-v9" generation)
                          (equal "identity-formation-v14" (gethash "protocol" contract))
                          (equal "personal-context-core-glm53-v1.3" revision)))
                 remaining
                 (every (lambda (cut) (and (integerp cut) (<= 0 cut (gethash "cutoff" contract)))) remaining))
      (error "Invalid explicit preparation input/cuts"))
    (dolist (event events)
      (unless (uiop:string-prefix-p "context-graph-identity-" (gethash "type" event))
        (setf (gethash (gethash "id" event) index) event)))
    (labels ((elapsed () (/ (- (get-internal-real-time) started)
                           (float internal-time-units-per-second)))
             (seal (cut status)
               (let ((selection (pai.context-graph::%cg-detach contract)))
                 (setf (gethash "cutoff" selection) cut
                       (gethash "origin_digest" selection) (gethash "origin_digest" input)
                        (gethash "compatibility" selection)
                        (if (equal "reviewed-inference-v8" (gethash "profile" contract))
                            "v8-v13-v1.2-lab-replay-1"
                            "v9-v14-v1.3-lab-replay-1")
                       (pai.context-graph::context-graph-runtime-last-event-id runtime) last-id
                       (pai.context-graph::context-graph-runtime-opens runtime)
                       (pai.context-graph::cgi-owner-opens owner))
                 (funcall emit-checkpoint
                          (pai.context-graph::context-graph-lab-checkpoint-seal runtime owner index selection)
                          selection status)
                 (setf last-save (elapsed))))
             (source (graph episode now)
               (%ccg-source-context graph episode now index agent-id persona-id)))
      (when resume
        (multiple-value-setq (runtime owner index)
          (pai.context-graph::context-graph-lab-checkpoint-open
           (gethash "envelope" resume) (gethash "digest" resume) (gethash "contract" resume)))
        (unless (equal (gethash "origin_digest" input)
                       (gethash "origin_digest" (gethash "contract" resume)))
          (error "Preparation resume origin differs"))
        (setf last-id (gethash "cutoff" (gethash "contract" resume)))
        (when (some (lambda (cut) (< cut last-id)) remaining)
          (error "Requested cut precedes resume checkpoint")))
      (dolist (event events)
        (let ((id (gethash "id" event)))
          (when (> id last-id)
            (loop while (and remaining (< (first remaining) id)) do
              (seal (pop remaining) "target"))
            (unless remaining (return))
            (when (and (plusp processed) (>= (- (elapsed) last-save) 30))
              (seal last-id "resume"))
            (when (>= (elapsed) deadline-seconds)
              (seal last-id "resume")
              (return-from context-graph-lab-prepare
                (obj "status" "incomplete" "last_event_id" last-id
                     "elapsed_seconds" (elapsed) "events_processed" processed)))
            (handler-case
                (pai.context-graph::%cgi-owner-consume owner event #'source)
              (error (condition)
                (let* ((parent (gethash "caused_by" event))
                       (next (ignore-errors
                               (pai.context-graph::%cgi-owner-next owner parent)))
                       (record (ignore-errors
                                 (shasht:read-json
                                  (gethash "record_json"
                                           (gethash "payload" event)))))
                       (actual (and record (gethash "request_digest" record))))
                  (error "Lab replay failed at event ~d (~a, caused by ~a): ~a; stored phase/digest ~a/~a, expected ~a/~a"
                         id (gethash "type" event) parent condition
                         (and record (gethash "phase" record)) actual
                         (and next (gethash "phase" next))
                         (and next (gethash "request_digest" next))))))
            (when (fboundp '%ccg-apply-confirmation-resolution)
              (%ccg-apply-confirmation-resolution runtime event index agent-id persona-id))
            (setf last-id id)
            (incf processed)
            (when (zerop (mod processed 100))
              (format t "KG-EPISODE-LAB-PROGRESS event=~d processed=~d elapsed=~,2f~%"
                      id processed (elapsed))
              (force-output)))))
      (dolist (cut remaining) (seal cut "target"))
      (obj "status" "complete" "elapsed_seconds" (elapsed)
           "events_processed" processed "provider_calls" 0 "authority_writes" 0))))
