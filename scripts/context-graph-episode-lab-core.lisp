;;;; context-graph-episode-lab-core.lisp -- immutable-ledger KG replay helpers.
;;;;
;;;; This file defines no initializer and opens no storage at load time.  The
;;;; entrypoint supplies a read-only event backend and explicit partition.

(in-package :agent)

(defstruct (context-graph-episode-lab
            (:constructor %make-context-graph-episode-lab))
  backend
  agent-id
  persona-id
  recovery-start-storage-position
  events
  (sealed-only-p nil)
  (runtime-cache (make-hash-table :test #'eql))
  (owner-cache (make-hash-table :test #'eql)))

(defun %cgel-read-events (backend agent-id recovery-start-storage-position)
  "Read exactly the recursive graph replay lanes in physical authority order."
  (labels ((read-types (types &optional after-position)
             (let ((events nil))
               (multiple-value-bind (complete-p ignored-position ignored-count
                                      ignored-bytes)
                   (if after-position
                       (storage-map-events
                        backend
                        (lambda (event position)
                          (declare (ignore position))
                          (push event events))
                        :agent-id agent-id
                        :after-position after-position :event-types types)
                       (storage-map-events
                        backend
                        (lambda (event position)
                          (declare (ignore position))
                          (push event events))
                        :agent-id agent-id :event-types types))
                 (declare (ignore ignored-position ignored-count ignored-bytes))
                 (unless complete-p
                   (error "Episode laboratory replay stream was incomplete"))
                 (nreverse events)))))
    (if (integerp recovery-start-storage-position)
        (append
         (read-types *conscious-recursive-historical-dialogue-event-types*)
         (read-types
          (set-difference
           *conscious-recursive-thread-event-types*
           *conscious-recursive-historical-dialogue-event-types*
           :test #'string=)
          recovery-start-storage-position))
        (read-types *conscious-recursive-thread-event-types*))))

(defun make-context-graph-episode-lab
    (backend agent-id persona-id &key recovery-start-storage-position)
  (unless (and (typep backend 'sqlite-storage)
               (stringp agent-id) (plusp (length agent-id))
               (stringp persona-id) (plusp (length persona-id))
               (or (null recovery-start-storage-position)
                   (and (integerp recovery-start-storage-position)
                        (not (minusp recovery-start-storage-position)))))
    (error "Invalid episode laboratory authority"))
  (%make-context-graph-episode-lab
   :backend backend :agent-id agent-id :persona-id persona-id
   :recovery-start-storage-position recovery-start-storage-position
   :events (%cgel-read-events backend agent-id
                              recovery-start-storage-position)))

(defun %cgel-generation-contract (events through-event-id)
  "Select the newest formation generation opened at the requested ledger cut.

Each replacement generation rebuilds from event authority under its own
ontology. Folding only that generation is therefore intentional; older
generation receipts remain replayable at cuts before the replacement opens."
  (let ((generation nil) (revision nil))
    (dolist (event events)
      (let ((payload (gethash "payload" event)))
        (when (and (hash-table-p payload)
                   (<= (gethash "id" event -1) through-event-id)
                   (equal "context-graph-identity-opened"
                          (gethash "type" event ""))
                   (member (gethash "generation" payload)
                           '("identity-formation-owner-v6"
                             "identity-formation-owner-v7"
                             "identity-formation-owner-v8"
                             "identity-formation-owner-v9")
                           :test #'equal))
          (let ((record (pai.context-graph::%cgro-record event)))
            (setf generation (gethash "generation" payload)
                  revision (gethash "ontology_revision" record))))))
    (or (and generation revision (list generation revision))
        (list "identity-formation-owner-v6"
              *knowledge-graph-ontology-revision*))))

(defun %cgel-ontology (revision)
  ;; The V8/V1.2 qualified source exposed the descriptor without a revision
  ;; argument.  V9 added the optional revision selector for V1.3.  Keep the
  ;; lab able to replay either exact source boundary without changing runtime.
  (let ((descriptor
          (if (equal revision "personal-context-core-glm53-v1.2")
              (knowledge-graph-ontology-provider-descriptor)
              (knowledge-graph-ontology-provider-descriptor revision))))
    (obj "entity_types" (gethash "entity_types" descriptor)
         "edge_types"
         (map 'vector
              (lambda (predicate)
                (obj "name" (gethash "predicate" predicate)
                     "subject_types" (gethash "subject_types" predicate)
                     "object_types" (gethash "object_types" predicate)))
              (gethash "predicate_signatures" descriptor)))))

(defun %cgel-create-owner (generation graph agent-id persona-id revision)
  (cond
    ((equal generation "identity-formation-owner-v6")
     (pai.context-graph::%cgf-owner-create-v6
      graph agent-id persona-id revision))
    ((equal generation "identity-formation-owner-v7")
     (pai.context-graph::%cgf-owner-create-v7
      graph agent-id persona-id revision))
    ((equal generation "identity-formation-owner-v8")
     (pai.context-graph::%cgf-owner-create-v8
      graph agent-id persona-id revision))
    ((equal generation "identity-formation-owner-v9")
     (pai.context-graph::%cgf-owner-create-v9
      graph agent-id persona-id revision))
    (t (error "Episode laboratory formation generation is unsupported"))))

(defun %cgel-runtime-at (lab through-event-id)
  (or (gethash through-event-id
               (context-graph-episode-lab-runtime-cache lab))
      (when (context-graph-episode-lab-sealed-only-p lab)
        (error "Requested cut is not loaded; checkpoint cases never reconstruct history"))
      (let* ((agent-id (context-graph-episode-lab-agent-id lab))
             (persona-id (context-graph-episode-lab-persona-id lab))
             (events
               (remove-if
                (lambda (event)
                  (> (gethash "id" event -1) through-event-id))
                (context-graph-episode-lab-events lab)))
             (contract
               (%cgel-generation-contract
                (context-graph-episode-lab-events lab) through-event-id))
             (generation (first contract))
             (revision (second contract))
             (index (make-hash-table :test #'eql))
             (runtime
               (pai.context-graph:context-graph-runtime-create
                (%cgel-ontology revision) revision
                agent-id persona-id))
             (graph
               (pai.context-graph::context-graph-runtime-graph runtime))
             (owner
               (%cgel-create-owner generation graph agent-id persona-id
                                   revision)))
        (dolist (event events)
          (setf (gethash (gethash "id" event) index) event))
        (let ((source-fn
                (lambda (selected-graph episode now)
                  (%ccg-source-context selected-graph episode now index
                                       agent-id persona-id))))
          (dolist (event events)
            (when (> (gethash "id" event)
                     (pai.context-graph::cgi-owner-last-id owner))
              (handler-case
                  (progn
                    (pai.context-graph::%cgi-owner-consume
                     owner event source-fn)
                    (%ccg-apply-confirmation-resolution
                     runtime event index agent-id persona-id))
                (error (condition)
                  (let* ((record
                           (ignore-errors
                             (pai.context-graph::%cgro-record event)))
                         (parent (gethash "caused_by" event))
                         (expected
                           (and (integerp parent)
                                (ignore-errors
                                  (pai.context-graph::%cgi-owner-next
                                   owner parent)))))
                    (error
                     "Episode laboratory replay refused event ~d (~a), phase ~a; expected phase ~a (digest match ~a): ~a"
                     (gethash "id" event) (gethash "type" event)
                     (and record (gethash "phase" record))
                     (and expected (gethash "phase" expected))
                     (and expected record
                          (equal (gethash "request_digest" record)
                                 (gethash "request_digest" expected)))
                     condition)))))))
        (setf (pai.context-graph::context-graph-runtime-last-event-id runtime)
              (pai.context-graph::cgi-owner-last-id owner)
              (pai.context-graph::context-graph-runtime-opens runtime)
              (pai.context-graph::%cg-detach
               (pai.context-graph::cgi-owner-opens owner)))
        (setf (gethash through-event-id
                       (context-graph-episode-lab-runtime-cache lab))
              runtime
              (gethash through-event-id
                       (context-graph-episode-lab-owner-cache lab))
              owner)
        runtime)))

(defun %cgel-owner-at (lab through-event-id)
  (%cgel-runtime-at lab through-event-id)
  (or (gethash through-event-id
               (context-graph-episode-lab-owner-cache lab))
      (error "Episode laboratory owner cache is incomplete")))

(defun %cgel-proposal-claim-label (proposal claim-ref)
  (cond
    ((uiop:string-prefix-p "entity:" claim-ref)
     (let* ((local-ref (subseq claim-ref (length "entity:")))
            (entity (find local-ref (gethash "entities" proposal)
                          :test #'equal
                          :key (lambda (row) (gethash "local_ref" row)))))
       (and entity (gethash "label" entity))))
    ((uiop:string-prefix-p "relationship:" claim-ref)
     (let ((ordinal (ignore-errors
                      (parse-integer claim-ref
                                     :start (length "relationship:")))))
       (and ordinal
            (< ordinal (length (gethash "relationships" proposal)))
            (gethash "fact" (aref (gethash "relationships" proposal)
                                   ordinal)))))
    (t nil)))

(defun %cgel-diagnostic-codes (result)
  (if (hash-table-p result)
      (coerce
       (loop for row across (gethash "diagnostics" result #())
             for code = (and (hash-table-p row) (gethash "code" row))
             when code collect code)
       'vector)
      #()))

(defun %cgel-formation-attempt (opened terminal application)
  (let* ((opening (pai.context-graph::%cgro-record opened))
         (terminal-record (pai.context-graph::%cgro-record terminal))
         (terminal-type (gethash "type" terminal))
         (completed (equal terminal-type "context-graph-identity-completed"))
         (envelope (and completed (gethash "result" terminal-record)))
         (proposal (and (hash-table-p envelope) (gethash "proposal" envelope)))
         (application-value (and (hash-table-p application)
                                 (gethash "value" application)))
         (selection (and (hash-table-p application-value)
                         (gethash "selection_receipt" application-value)))
         (application-record (and (hash-table-p application-value)
                                  (gethash "application" application-value)))
         (omissions
           (if (and (hash-table-p selection) (hash-table-p proposal))
               (map 'vector
                    (lambda (row)
                      (let ((copy (pai.context-graph::%cg-detach row)))
                        (setf (gethash "label" copy)
                              (or (%cgel-proposal-claim-label
                                   proposal (gethash "claim_ref" row))
                                  :null))
                        copy))
                    (gethash "omissions" selection #()))
               #())))
    (obj "opened_event_id" (gethash "id" opened)
         "terminal_event_id" (gethash "id" terminal)
         "terminal_type" terminal-type
         "batch_index" (gethash "batch_index" opening 0)
         "attempt" (gethash "attempt" opening 1)
         "protocol" (gethash "formation_protocol" opening "identity-formation-v1")
         "status" (if completed "completed" "failed")
         "failure_reason" (if completed :null
                               (gethash "reason" terminal-record "unspecified"))
         "failure_class" (if completed :null
                              (gethash "failure_class" terminal-record :null))
         "proposal_entities"
         (if (hash-table-p proposal)
             (map 'vector
                  (lambda (entity)
                    (obj "claim_ref" (concatenate 'string "entity:"
                                                   (gethash "local_ref" entity))
                         "label" (gethash "label" entity)
                         "identity_action" (gethash "identity_action" entity)))
                  (gethash "entities" proposal))
             #())
         "proposal_relationships"
         (if (hash-table-p proposal)
             (map 'vector
                  (lambda (relationship ordinal)
                    (obj "claim_ref" (format nil "relationship:~d" ordinal)
                         "label" (gethash "fact" relationship)
                         "predicate" (gethash "predicate" relationship)))
                  (gethash "relationships" proposal)
                  (coerce (loop for ordinal below
                                (length (gethash "relationships" proposal))
                                collect ordinal)
                          'vector))
             #())
         "omissions" omissions
         "application_status" (if (hash-table-p application)
                                  (gethash "status" application :null) :null)
         "formation_outcome" (if (hash-table-p application-record)
                                 (gethash "formation_outcome"
                                          application-record :null)
                                 :null)
         "diagnostic_codes" (%cgel-diagnostic-codes application))))

(defun %cgel-formation-attempts (lab before-event-id after-event-id)
  "Explain proposal-to-admission attrition for the replay interval."
  (let ((owner (%cgel-owner-at lab after-event-id))
        (rows nil))
    (loop for opened-id being the hash-keys of
          (pai.context-graph::cgi-owner-opens owner)
          using (hash-value opened)
          for terminal = (gethash opened-id
                                  (pai.context-graph::cgi-owner-terminals owner))
          when (and terminal
                    (> (gethash "id" terminal) before-event-id)
                    (<= (gethash "id" terminal) after-event-id))
            do (push (%cgel-formation-attempt
                      opened terminal
                      (gethash opened-id
                               (pai.context-graph::cgi-owner-applications owner)))
                     rows))
    (coerce (sort rows #'< :key (lambda (row)
                                 (gethash "opened_event_id" row)))
            'vector)))

(defun %cgel-current-node (graph entity)
  (%ccg-node
   (pai.context-graph::%cg-authority-current-descriptor
    graph (gethash "entity_id" entity))))

(defun %cgel-edge (graph fact)
  (let* ((subject
           (pai.context-graph::%cg-authority-current-descriptor
            graph
            (gethash "entity_id"
                     (pai.context-graph::%cg-authority-entity
                      graph (gethash "subject_id" fact)))))
         (object
           (pai.context-graph::%cg-authority-current-descriptor
            graph
            (gethash "entity_id"
                     (pai.context-graph::%cg-authority-entity
                      graph (gethash "object_id" fact)))))
         (temporal (gethash "temporal" fact))
         (grounding (gethash "grounding" fact)))
    (obj "projection_name" "reviewed-context-graph-v1"
         "edge_id" (gethash "fact_id" fact)
         "from_node_id" (gethash "entity_id" subject)
         "to_node_id" (gethash "entity_id" object)
         "predicate" (gethash "predicate" fact)
         "statement" (or (gethash "statement" fact)
                           (gethash "fact" fact ""))
         "status" (gethash "status" fact "current")
         "scope" (gethash "scope" grounding :null)
         "polarity" (gethash "polarity" grounding :null)
         "source_basis" (gethash "source_basis" grounding :null)
         "query_eligible"
         (if (pai.context-graph::%cgr-factual-p fact) :true :false)
         "evidence_status" (gethash "evidence_status" fact)
         "evidence_note" (gethash "evidence_note" fact "")
         "source_ids" (pai.context-graph::%cg-detach
                       (gethash "accepted_source_ids" fact #()))
         "origin_agent_ids"
         (%kgs-origin-agent-ids
          (gethash "accepted_source_ids" fact #()))
         "valid_from" (gethash "valid_from" temporal :null)
         "valid_to" (gethash "valid_until" temporal :null))))

(defun %cgel-graph-at (lab through-event-id)
  (let* ((runtime (%cgel-runtime-at lab through-event-id))
         (graph (pai.context-graph::context-graph-runtime-graph runtime))
         (nodes
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

(defun %cgel-row-map (rows key)
  (let ((result (make-hash-table :test #'equal)))
    (loop for row across rows
          do (setf (gethash (gethash key row) result) row))
    result))

(defun %cgel-row-equal-p (left right)
  (string=
   (pai.context-graph::%cg-authority-canonical-json left)
   (pai.context-graph::%cg-authority-canonical-json right)))

(defun %cgel-mark-delta (before-rows after-rows key &optional (same-p #'%cgel-row-equal-p))
  (let ((before (%cgel-row-map before-rows key))
        (after (%cgel-row-map after-rows key))
        (rows nil))
    (loop for id being the hash-keys of after using (hash-value row)
          for prior = (gethash id before)
          do (let ((copy (pai.context-graph::%cg-detach row)))
               (setf (gethash "change" copy)
                     (cond ((null prior) "added")
                           ((funcall same-p prior row) "unchanged")
                           (t "changed")))
               (push copy rows)))
    (loop for id being the hash-keys of before using (hash-value row)
          unless (gethash id after)
            do (let ((copy (pai.context-graph::%cg-detach row)))
                 (setf (gethash "change" copy) "removed")
                 (push copy rows)))
    (coerce (sort rows #'string< :key (lambda (row) (gethash key row)))
            'vector)))

(defun context-graph-episode-lab-delta (lab before-event-id after-event-id)
  (unless (and (integerp before-event-id) (not (minusp before-event-id))
               (integerp after-event-id) (>= after-event-id before-event-id))
    (error "Invalid episode delta bounds"))
  (let ((before (%cgel-graph-at lab before-event-id))
        (after (%cgel-graph-at lab after-event-id)))
    (obj "schema_version" 1
         "before" (obj "through_event_id" before-event-id
                        "node_count" (gethash "node_count" before)
                        "edge_count" (gethash "edge_count" before)
                        "query_eligible_edge_count"
                        (gethash "query_eligible_edge_count" before))
         "after" (obj "through_event_id" after-event-id
                       "node_count" (gethash "node_count" after)
                       "edge_count" (gethash "edge_count" after)
                       "query_eligible_edge_count"
                       (gethash "query_eligible_edge_count" after))
         "nodes" (%cgel-mark-delta (gethash "nodes" before)
                                   (gethash "nodes" after) "node_id")
         "edges" (%cgel-mark-delta (gethash "edges" before)
                                   (gethash "edges" after) "edge_id")
         "formation_attempts"
         (%cgel-formation-attempts lab before-event-id after-event-id))))

(defun context-graph-episode-lab-query (lab through-event-id request)
  "Exercise pAI's normalized, reviewed and compacted search-graph path."
  (unless (and (integerp through-event-id) (not (minusp through-event-id))
               (hash-table-p request))
    (error "Invalid episode laboratory query"))
  (knowledge-graph-search-compact-result
   (%ccg-search (%cgel-runtime-at lab through-event-id)
                (knowledge-graph-search-tool-normalize request))))

(defun context-graph-episode-lab-graph (lab through-event-id)
  (%cgel-graph-at lab through-event-id))
