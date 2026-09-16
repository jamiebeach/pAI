;;;; context-graph-inference-qualification.lisp -- private, read-only V14 worker.
;;;;
;;;; This worker reads an immutable episode-laboratory authority, rebuilds the
;;;; production graph at a caller-sealed authority cutoff in memory, and
;;;; applies only caller-selected episodes through identity-formation-v14.
;;;; Provider transport remains in
;;;; the host launcher; this process has no credentials and may run without a
;;;; network.  Nothing is appended to the source ledger or a derived store.

(require :asdf)
(load (uiop:getenv "PAI_QUICKLISP_SETUP"))
(push (pathname (uiop:getenv "PAI_REPOSITORY_ROOT"))
      asdf:*central-registry*)
(asdf:load-system :pai)
(load (merge-pathnames "scripts/context-graph-episode-lab-core.lisp"
                       (pathname (uiop:getenv "PAI_REPOSITORY_ROOT"))))
(load (merge-pathnames "scripts/context-graph-lab-checkpoint.lisp"
                       (pathname (uiop:getenv "PAI_REPOSITORY_ROOT"))))

(in-package :agent)

(defun %cgiq-read-json (text)
  (let ((shasht:*read-default-true-value* :true)
        (shasht:*read-default-false-value* :false)
        (shasht:*read-default-null-value* :null))
    (shasht:read-json text)))

(defun %cgiq-write-json (value)
  (let ((*print-pretty* nil))
    (shasht:write-json value nil)))

(defun %cgiq-graph-view (graph through-event-id)
  (let ((nodes
          (sort
           (loop for entity being the hash-values of
                 (pai.context-graph::context-graph-entities graph)
                 collect (%cgel-current-node graph entity))
           #'string< :key (lambda (row) (gethash "node_id" row))))
        (edges
          (sort
           (loop for fact being the hash-values of
                 (pai.context-graph::context-graph-facts graph)
                 collect (%cgel-edge graph fact))
           #'string< :key (lambda (row) (gethash "edge_id" row)))))
    (obj "schema_version" 1
         "through_event_id" through-event-id
         "node_count" (length nodes)
         "edge_count" (length edges)
         "query_eligible_edge_count"
         (count :true edges :key (lambda (row)
                                   (gethash "query_eligible" row)))
         "nodes" (coerce nodes 'vector)
         "edges" (coerce edges 'vector))))

(defun %cgiq-changed-rows (before after key)
  (remove "unchanged" (%cgel-mark-delta before after key)
          :test #'equal :key (lambda (row) (gethash "change" row))))

(defun %cgiq-baseline-runtime
    (checkpoint-path checkpoint-digest checkpoint-contract ontology-revision
     agent-id persona-id)
  (if (eq checkpoint-path :null)
      (pai.context-graph:context-graph-runtime-create
       (%cgel-ontology ontology-revision) ontology-revision agent-id persona-id)
      (let ((outer (%cgiq-read-json (uiop:read-file-string checkpoint-path))))
        (unless (and (hash-table-p outer)
                     (equal "target" (gethash "status" outer))
                     (equal checkpoint-digest (gethash "digest" outer)))
          (error "Qualification baseline checkpoint descriptor mismatch"))
        (multiple-value-bind (runtime owner source-index)
            (pai.context-graph::context-graph-lab-checkpoint-open
             (gethash "envelope" outer) checkpoint-digest checkpoint-contract)
          (declare (ignore owner source-index))
          runtime))))

(defun %cgiq-final-result
    (baseline graph head protocol ontology-revision audits)
  (let ((after (%cgiq-graph-view graph head)))
    (obj "schema_version" 1
         "status" "complete"
         "protocol" protocol
         "ontology_revision" ontology-revision
         "baseline"
         (obj "node_count" (gethash "node_count" baseline)
              "edge_count" (gethash "edge_count" baseline)
              "query_eligible_edge_count"
              (gethash "query_eligible_edge_count" baseline))
         "after"
         (obj "node_count" (gethash "node_count" after)
              "edge_count" (gethash "edge_count" after)
              "query_eligible_edge_count"
              (gethash "query_eligible_edge_count" after))
         "nodes" (coerce (%cgiq-changed-rows
                           (gethash "nodes" baseline)
                           (gethash "nodes" after) "node_id")
                          'vector)
         "edges" (coerce (%cgiq-changed-rows
                           (gethash "edges" baseline)
                           (gethash "edges" after) "edge_id")
                          'vector)
         "formation_attempts" (coerce (nreverse audits) 'vector)
         "database_write_count" 0)))

(let* ((config
         (%cgiq-read-json
          (or (uiop:getenv
               "PAI_CONTEXT_GRAPH_INFERENCE_QUALIFICATION_CONFIG_JSON")
              (uiop:read-file-string
               (uiop:getenv
                "PAI_CONTEXT_GRAPH_INFERENCE_QUALIFICATION_CONFIG")))))
       (events-path (gethash "events" config))
       (agent-id (gethash "agent_id" config))
       (persona-id (gethash "persona_id" config))
       (head (gethash "head_event_id" config))
       (baseline-event-id (gethash "baseline_event_id" config))
       (baseline-checkpoint-path
         (gethash "baseline_checkpoint_path" config :null))
       (baseline-checkpoint-digest
         (gethash "baseline_checkpoint_digest" config :null))
       (baseline-checkpoint-contract
         (gethash "baseline_checkpoint_contract" config :null))
       (maximum-calls-per-batch (gethash "maximum_calls_per_batch" config))
       (protocol (gethash "protocol" config))
       (ontology-revision (gethash "ontology_revision" config))
       (selected (gethash "episode_event_ids" config))
       (episode-batches (gethash "episode_batches" config :null))
       (episode-batch-call-limits
         (gethash "episode_batch_call_limits" config :null))
       (observed-at (gethash "observed_at" config))
       (recovery-position
         (let ((value (gethash "recovery_start_storage_position" config :null)))
           (unless (eq value :null) value)))
       (backend nil)
       (lab nil))
  (unless (and (hash-table-p config)
               (stringp events-path)
               (stringp agent-id) (plusp (length agent-id))
               (stringp persona-id) (plusp (length persona-id))
               (integerp head) (plusp head)
               (integerp baseline-event-id) (plusp baseline-event-id)
               (<= baseline-event-id head)
               (or (and (eq baseline-checkpoint-path :null)
                        (eq baseline-checkpoint-digest :null)
                        (eq baseline-checkpoint-contract :null))
                   (and (stringp baseline-checkpoint-path)
                        (stringp baseline-checkpoint-digest)
                        (hash-table-p baseline-checkpoint-contract)))
               (integerp maximum-calls-per-batch)
               (plusp maximum-calls-per-batch)
               (equal protocol "identity-formation-v14")
               (equal ontology-revision
                      *knowledge-graph-family-ontology-revision*)
               (vectorp selected) (<= 1 (length selected) 6)
               (= (length selected)
                  (length (remove-duplicates (coerce selected 'list))))
               (every (lambda (id) (and (integerp id) (plusp id) (< id head)))
                      selected)
               (or (eq episode-batches :null)
                   (and (vectorp episode-batches)
                        (plusp (length episode-batches))
                        (every
                         (lambda (row)
                           (and (hash-table-p row)
                                (pai.context-graph::%cg-closed-keys-p
                                 row '("episode_event_id" "batch_index"))
                                (find (gethash "episode_event_id" row) selected)
                                (integerp (gethash "batch_index" row))
                                (not (minusp (gethash "batch_index" row)))))
                         episode-batches)
                        (= (length episode-batches)
                           (length
                            (remove-duplicates
                             (coerce episode-batches 'list)
                             :test (lambda (a b)
                                     (and (= (gethash "episode_event_id" a)
                                             (gethash "episode_event_id" b))
                                          (= (gethash "batch_index" a)
                                             (gethash "batch_index" b)))))))))
               (or (eq episode-batch-call-limits :null)
                   (and (vectorp episode-batch-call-limits)
                        (not (eq episode-batches :null))
                        (= (length episode-batch-call-limits)
                           (length episode-batches))
                        (loop for limit across episode-batch-call-limits
                              for task across episode-batches
                              always
                              (and (hash-table-p limit)
                                   (pai.context-graph::%cg-closed-keys-p
                                    limit '("episode_event_id" "batch_index"
                                            "maximum_calls"))
                                   (= (gethash "episode_event_id" limit)
                                      (gethash "episode_event_id" task))
                                   (= (gethash "batch_index" limit)
                                      (gethash "batch_index" task))
                                   (integerp (gethash "maximum_calls" limit))
                                   (<= 1 (gethash "maximum_calls" limit)
                                       maximum-calls-per-batch)))))
               (integerp observed-at) (plusp observed-at))
    (error "Invalid inference qualification configuration"))
  (unwind-protect
       (progn
         (setf backend (make-sqlite-storage-read-only events-path)
               lab (make-context-graph-episode-lab
                    backend agent-id persona-id
                    :recovery-start-storage-position recovery-position))
         (let* ((runtime
                  (%cgiq-baseline-runtime
                   baseline-checkpoint-path baseline-checkpoint-digest
                   baseline-checkpoint-contract ontology-revision
                   agent-id persona-id))
                (graph
                  (pai.context-graph::context-graph-runtime-graph runtime))
                (baseline (%cgiq-graph-view graph baseline-event-id))
                (application-base
                  (max head
                       (pai.context-graph::context-graph-through-event-id graph)))
                (event-index (make-hash-table :test #'eql))
                (episode-index 0)
                (batch-index 0)
                (task-ordinal 0)
                (calls nil)
                (audits nil)
                (pending nil)
                (finished nil)
                (guide (%ccg-descriptor-guide ontology-revision)))
           (dolist (event (context-graph-episode-lab-events lab))
             (setf (gethash (gethash "id" event) event-index) event))
           (labels
               ((selected-batch-p (episode-id batch)
                  (or (eq episode-batches :null)
                      (find-if
                       (lambda (row)
                         (and (= episode-id (gethash "episode_event_id" row))
                              (= batch (gethash "batch_index" row))))
                       episode-batches)))
                (selected-batch-call-limit (episode-id batch)
                  (if (eq episode-batch-call-limits :null)
                      maximum-calls-per-batch
                      (gethash
                       "maximum_calls"
                       (find-if
                        (lambda (row)
                          (and (= episode-id
                                  (gethash "episode_event_id" row))
                               (= batch (gethash "batch_index" row))))
                        episode-batch-call-limits))))
                (source (episode-id)
                  (%ccg-source-context graph episode-id
                                       (+ observed-at task-ordinal)
                                       event-index agent-id persona-id))
                (advance ()
                  (loop
                    (when (= episode-index (length selected))
                      (setf finished
                            (%cgiq-final-result baseline graph head protocol
                                                 ontology-revision audits))
                      (return finished))
                    (let* ((episode-event-id (aref selected episode-index))
                           (full (source episode-event-id))
                           (batch-count
                             (length
                              (pai.context-graph::%cgro-source-batches full))))
                      (cond ((>= batch-index batch-count)
                          (progn
                            (incf episode-index)
                            (setf batch-index 0)))
                          ((not (selected-batch-p episode-event-id batch-index))
                           (incf batch-index))
                          (t (let ((call-index 0))
                            (let ((requested
                                (catch 'cgiq-request
                                  (let* ((pai.context-graph::*cgf-protocol*
                                          protocol)
                                         (envelope
                                           (pai.context-graph::%cgf-generate
                                            graph full batch-index
                                            (pai.context-graph::context-graph-ontology
                                             graph)
                                            ontology-revision
                                            (lambda (phase spec digest)
                                              (if (< call-index (length calls))
                                                  (let ((saved
                                                          (nth call-index calls)))
                                                    (incf call-index)
                                                    (unless
                                                        (and
                                                         (equal phase
                                                                (gethash "phase" saved))
                                                         (equal digest
                                                                (gethash "request_digest"
                                                                         saved)))
                                                      (error
                                                       "Qualification receipt mismatch"))
                                                    (pai.context-graph::%cg-detach
                                                     (gethash "response" saved)))
                                                  (let ((result
                                                          (obj
                                                           "schema_version" 1
                                                           "status" "request"
                                                           "protocol"
                                                           protocol
                                                           "task_ordinal" task-ordinal
                                                           "episode_ordinal"
                                                           episode-index
                                                           "batch_index" batch-index
                                                           "phase" phase
                                                           "request_digest" digest
                                                           "spec" spec)))
                                                    (setf pending result)
                                                    (throw 'cgiq-request
                                                           result))))
                                            :max-calls
                                            (selected-batch-call-limit
                                             episode-event-id batch-index)
                                            :descriptor-guide guide)))
                                    (unless (= call-index (length calls))
                                      (error "Qualification has extra receipts"))
                                    (let ((boundary
                                            (obj
                                             "episode_id"
                                             (gethash "episode_id"
                                                      (gethash "context" envelope))
                                             "opened_boundary_id"
                                             (+ application-base 1 (* task-ordinal 2))
                                             "application_event_id"
                                             (+ application-base 2 (* task-ordinal 2))
                                             "observed_at"
                                             (+ observed-at task-ordinal))))
                                      (let ((application
                                              (pai.context-graph::%cgf-apply
                                               graph boundary full envelope
                                               :descriptor-guide guide)))
                                        (push
                                         (obj
                                          "episode_event_id" episode-event-id
                                          "episode_ordinal" episode-index
                                          "batch_index" batch-index
                                          "protocol" protocol
                                          "proposal"
                                          (gethash "proposal" envelope :null)
                                          "review"
                                          (gethash "review" envelope :null)
                                          "identity_trace"
                                          (gethash "identity_trace" envelope :null)
                                          "application" application)
                                         audits)))
                                    nil))))
                              (when requested (return requested)))
                            (setf calls nil pending nil)
                            (incf batch-index)
                            (incf task-ordinal)))))))
                (handle (request)
                  (let ((operation (gethash "operation" request)))
                    (cond
                      ((equal operation "next")
                       (when pending
                         (error "A qualification response is pending"))
                       (or finished (advance)))
                      ((equal operation "snapshot")
                       (let ((snapshot
                               (%cgiq-final-result baseline graph head protocol
                                                    ontology-revision audits)))
                         (setf (gethash "status" snapshot) "snapshot"
                               (gethash "pending_phase" snapshot)
                               (if pending (gethash "phase" pending) :null))
                         snapshot))
                      ((equal operation "response")
                       (unless (and pending
                                    (equal (gethash "phase" pending)
                                           (gethash "phase" request))
                                    (equal (gethash "request_digest" pending)
                                           (gethash "request_digest" request))
                                    (hash-table-p (gethash "response" request)))
                         (error "Qualification response binding is invalid"))
                       (setf calls
                             (append
                              calls
                              (list
                               (obj "phase" (gethash "phase" request)
                                    "request_digest"
                                    (gethash "request_digest" request)
                                    "response"
                                    (pai.context-graph::%cg-detach
                                     (gethash "response" request)))))
                             pending nil)
                       (advance))
                      (t (error "Unknown inference qualification operation"))))))
             (let* ((available-batch-counts
                      (map 'vector
                           (lambda (episode-event-id)
                             (length
                              (pai.context-graph::%cgro-source-batches
                               (source episode-event-id))))
                           selected))
                    (unused
                      (unless (eq episode-batches :null)
                        (loop for row across episode-batches
                              for episode-event-id = (gethash "episode_event_id" row)
                              for position = (position episode-event-id selected)
                              unless (and position
                                          (< (gethash "batch_index" row)
                                             (aref available-batch-counts position)))
                                do (error "Selected episode batch is unavailable"))))
                    (batch-counts
                      (if (eq episode-batches :null)
                          available-batch-counts
                          (map 'vector
                               (lambda (episode-event-id)
                                 (count episode-event-id episode-batches
                                        :key (lambda (row)
                                               (gethash "episode_event_id" row))))
                               selected)))
                    (total-batches (reduce #'+ batch-counts :initial-value 0))
                    (batch-call-limits
                      (if (eq episode-batch-call-limits :null)
                          (make-array total-batches
                                      :initial-element
                                      maximum-calls-per-batch)
                          (map 'vector
                               (lambda (row)
                                 (gethash "maximum_calls" row))
                               episode-batch-call-limits))))
               (declare (ignore unused))
               (format t "KG-INFERENCE-QUALIFICATION-READY ~a~%"
                       (%cgiq-write-json
                        (obj "schema_version" 1
                             "protocol" protocol
                             "ontology_revision" ontology-revision
                             "baseline_event_id" baseline-event-id
                             "baseline_node_count"
                             (gethash "node_count" baseline)
                             "baseline_edge_count"
                             (gethash "edge_count" baseline)
                             "selected_episode_count" (length selected)
                             "source_batch_counts" batch-counts
                             "source_batch_count" total-batches
                             "source_batch_call_limits" batch-call-limits
                             "request_count_upper_bound"
                             (reduce #'+ batch-call-limits :initial-value 0)
                             "database_write_count" 0))))
             (force-output)
             (loop for line = (read-line *standard-input* nil nil)
                   while line
                   do (handler-case
                          (format t "KG-INFERENCE-QUALIFICATION-RESULT ~a~%"
                                  (%cgiq-write-json
                                   (handle (%cgiq-read-json line))))
                        (error (condition)
                          (format t "KG-INFERENCE-QUALIFICATION-ERROR ~a~%"
                                  (%cgiq-write-json
                                   (obj "error"
                                        (princ-to-string condition))))))
                      (force-output)))))
    (when backend (storage-close backend))))
