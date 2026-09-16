;;;; harness: bare
(load (merge-pathnames "context-graph-runtime-generation-tests.lisp" *load-truename*))
(in-package :pai.context-graph)

(dolist (protocol '("v1" "bounded-v2" "bounded-v3" "bounded-v4"))
(let* ((ontology (gethash "ontology" (as-fixture))) (revision "personal-context-core-glm53-v1.2")
       (runtime (context-graph-runtime-create ontology revision "lab-agent" "lab-persona"))
       (events nil) (sequence 10) (calls 0) (pause t)
       (episode (sm-episode "runtime-owner-source" "I own a cat named Mina." 100))
       (simple (sm-proposal)))
  (labels ((source (graph id now)
             (declare (ignore now)) (assert (= 1 id))
             (lab-authority-context graph episode 0))
           (append-event (type payload cause)
             ;; Actual JSON round trip preserves false/null in canonical records.
             (let ((event (context-graph-runtime-read-json
                            (context-graph-runtime-json
                              (%cg-object "id" (incf sequence) "agent_id" "lab-agent" "type" type
                                          "payload" payload "caused_by" cause)))))
               (push event events) event))
           (model (phase spec digest id)
             (declare (ignore spec digest id)) (incf calls)
             (when (and pause (equal phase "facts")) (setf pause nil) (return-from model :paused-budget))
             (let ((context (%cgro-source-for-record runtime
                              (%cgro-record (find "context-graph-runtime-opened" events
                                                  :test #'equal :key (lambda (e) (gethash "type" e)))) #'source)))
               (cond ((equal phase "entities")
                      (let ((selection (%cg-object "new_entities" (gethash "new_entities" simple))))
                        (when (equal protocol "bounded-v4") (setf (gethash "reuse_entities" selection) #())) selection))
                     ((equal phase "facts") (%cg-object "facts" (gethash "facts" simple) "name_corrections" #()))
                     ((equal phase "review")
                      (let ((review (sm-review context (%cgs-expand context ontology revision simple))))
                        (when (equal protocol "bounded-v4")
                          (loop for row across (gethash "claim_reviews" review)
                                for entity = (uiop:string-prefix-p "entity:" (gethash "claim_ref" row)) do
                            (setf (gethash "source_reading" row) (if entity "not-applicable" "assertion")
                                  (gethash "quality_checks" row)
                                  (apply #'%cg-object (loop for key in +cgq-checks+ append
                                      (list key (if (and entity (not (equal key "endpoint_identity"))) "not-applicable" "supported")))))))
                        review))))))
    (assert (equal "paused-budget" (gethash "status" (context-graph-runtime-step runtime '(1) #'source #'model #'append-event :now 100 :protocol protocol))))
    (assert (= calls 2))
    ;; Restart after a pause, then reuse entities without another provider call.
    (setf runtime (context-graph-runtime-create ontology revision "lab-agent" "lab-persona"))
    (dolist (e (reverse events)) (context-graph-runtime-consume runtime e #'source))
    (assert (equal "sealed" (gethash "status" (context-graph-runtime-step runtime '(1) #'source #'model #'append-event :now 100 :protocol protocol))))
    (assert (= calls 4))
    (assert (= 1 (context-graph-runtime-applications runtime)))
    (let ((before (%cg-authority-watermark (context-graph-runtime-graph runtime) "lab-agent" "lab-persona")))
      (setf runtime (context-graph-runtime-create ontology revision "lab-agent" "lab-persona"))
      (dolist (e (reverse events)) (context-graph-runtime-consume runtime e #'source))
      (assert (= calls 4))
      (assert (%cg-authority-equal-p before (%cg-authority-watermark (context-graph-runtime-graph runtime) "lab-agent" "lab-persona")))
      (assert (= 1 (length (gethash "facts" (gethash "context"
                      (%cg-authority-retrieve (context-graph-runtime-graph runtime) "lab-agent" "lab-persona" "MINA cat")))))))
    (let* ((ordered (reverse events))
           (crashed (context-graph-runtime-create ontology revision "lab-agent" "lab-persona"))
           (call-count calls))
      ;; Simulated crash after request append but before response append.
      (dolist (e (subseq ordered 0 2)) (context-graph-runtime-consume crashed e #'source))
      (assert (equal "failed" (gethash "status" (context-graph-runtime-step crashed '(1) #'source #'model #'append-event :now 100 :protocol protocol))))
      (assert (= calls call-count))
      (let* ((fresh (context-graph-runtime-create ontology revision "lab-agent" "lab-persona"))
             (bad (%cg-detach (first ordered))) (record (%cgro-record bad)))
        (setf (gethash "access_snapshot_digest" (gethash "context" record)) (%cg-sha256 "forged"))
        (setf (gethash "record_json" (gethash "payload" bad)) (context-graph-runtime-json record))
        (assert (sm-error (lambda () (context-graph-runtime-consume fresh bad #'source)) "RUNTIME_OPEN_AUTHORITY_INVALID")))))))
(format t "RUNTIME-OWNER durable pause/resume, JSON fidelity, replay and useful retrieval passed~%")

(let ((runtime (context-graph-runtime-create (gethash "ontology" (as-fixture))
                                           "personal-context-core-glm53-v1.2" "lab-agent" "lab-persona")))
  (setf (gethash (%cgro-task 1 "staged" "bounded-v3" 0) (context-graph-runtime-covered runtime)) "context-graph-runtime-reviewed"
        (gethash (%cgro-task 1 "staged" "bounded-v3" 1) (context-graph-runtime-covered runtime)) "context-graph-runtime-failed")
  (assert (%cgro-covered-batch-p runtime 1 "staged" "bounded-v4" 0))
  (assert (not (%cgro-covered-batch-p runtime 1 "staged" "bounded-v4" 1)))
  (assert (not (%cgro-covered-batch-p runtime 1 "correction" "bounded-v4" 0)))
  (assert (not (%cgro-covered-batch-p runtime 2 "staged" "bounded-v4" 0))))
(format t "RUNTIME-OWNER upgrade never silently regenerates reviewed older batches~%")
