;;;; memory-storage.lisp -- capability boundary for memory persistence.
;;;;
;;;; Capability-shaped migration, retrieval and event-applied mutation port.
;;;; It intentionally has no generic SQL method and installs no runtime
;;;; authority.

(in-package :agent)

(export '(memory-storage-backend memory-storage-error
          memory-storage-unsupported-error memory-storage-capabilities
          memory-storage-characterize memory-storage-map-snapshot
          memory-storage-import-snapshot memory-storage-audit-snapshot
          make-memory-exact-query memory-exact-query-vector
          memory-exact-query-kinds memory-exact-query-origins
          memory-exact-query-statuses memory-exact-query-grounding-statuses
          memory-exact-query-excluded-ids
          memory-exact-query-excluded-turn-ids
          memory-exact-query-excluded-source-event-ids
          memory-exact-query-as-of memory-exact-query-hydrate-p
          memory-exact-query-include-vector-p
          memory-exact-query-lexemes memory-storage-exact-search
          memory-storage-lexical-search make-memory-storage-mutation
          make-memory-storage-mutation-from-receipt
          make-memory-operation-command make-memory-operation-payload
          memory-assemble-admission-operation
          memory-assemble-supersession-operation
          memory-assemble-tick-commit-operation
          memory-assemble-user-visible-rehearsal-operation
          memory-assemble-direct-edge-operation
          memory-assemble-quarantine-operation
          memory-cognitive-mutation-dispatch
          make-memory-node-upsert-payload
          make-memory-node-scalar-row memory-merge-node-upsert-rows
          memory-materialize-node-quarantine
          memory-materialize-node-supersession
          memory-materialize-node-rehearsal
          memory-materialize-node-decay
          memory-materialize-node-retrieval-backfill
          make-memory-edge-state-payload
          memory-storage-bind-projection memory-storage-apply-mutation
          memory-storage-operation-node memory-storage-operation-edges
          memory-storage-operation-next-edge-id
          memory-storage-operation-vector-dimension
          memory-storage-projection-report make-memory-mutation-coordinator
          memory-mutation-coordinator-reconcile
          memory-mutation-coordinator-commit
          memory-mutation-coordinator-commit-operation
          memory-mutation-coordinator-build-operation
          memory-mutation-coordinator-read
          make-storage-memory-mutation-coordinator
          make-coordinated-memory-storage))

(define-condition memory-storage-error (error)
  ((operation :initarg :operation :reader memory-storage-error-operation)
   (detail :initarg :detail :reader memory-storage-error-detail))
  (:report (lambda (condition stream)
             (format stream "Memory storage ~a failed: ~a"
                     (memory-storage-error-operation condition)
                     (memory-storage-error-detail condition)))))

(define-condition memory-storage-unsupported-error (memory-storage-error) ())

(defclass memory-storage-backend () ())

(defstruct (memory-exact-query
             (:constructor %make-memory-exact-query))
  vector profile limit turn-ids kinds origins statuses grounding-statuses
  excluded-ids excluded-turn-ids excluded-source-event-ids as-of hydrate-p
  include-vector-p lexemes)

(defparameter *memory-exact-query-profiles*
  '("all-vectors-v1" "safe-semantic-v1" "turn-neighborhood-v1"))

(defun %memory-storage-object (&rest fields)
  (loop with object = (make-hash-table :test #'equal)
        for (key value) on fields by #'cddr
        do (setf (gethash key object) value)
        finally (return object)))

(defun %memory-storage-required-string (value field &key (maximum 256))
  (unless (and (stringp value) (plusp (length value))
               (<= (length value) maximum))
    (error 'memory-storage-error :operation :validate-mutation
           :detail (format nil "~a must be a bounded non-empty string" field)))
  value)

(defun %memory-storage-exact-object-keys-p (object expected)
  (and (= (hash-table-count object) (length expected))
       (loop for key being the hash-keys of object
             always (and (stringp key)
                         (member key expected :test #'string=)))))

(defun %memory-storage-json-read (text operation)
  (handler-case (shasht:read-json text)
    (error (condition)
      (error 'memory-storage-error :operation operation :detail condition))))

(defun %memory-storage-sha256 (text)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :sha256 (sb-ext:string-to-octets text :external-format :utf-8))))

(defparameter *memory-atomic-operation-kinds*
  '(("admission"
     . (("memory-node-state" "admission")
        ("memory-edge-state" "admission-lineage")
        ("memory-edge-state" "admission-lineage-replacement")))
    ("supersession"
     . (("memory-node-state" "supersession")
        ("memory-edge-state" "supersession")))
    ("tick-commit"
     . (("memory-node-state" "node-write")
        ("memory-edge-state" "tick-commit-lineage")
        ("memory-edge-state" "tick-commit-supersession")
        ("memory-edge-state" "tick-commit-edge")))
    ("user-visible-rehearsal"
     . (("memory-node-state" "user-visible-rehearsal")))
    ("direct-edge" . (("memory-edge-state" "direct-edge")))
    ("quarantine" . (("memory-node-state" "quarantine")))))

(defun %memory-storage-state-event-type-p (event-type)
  (and (stringp event-type)
       (member event-type '("memory-node-state" "memory-edge-state")
               :test #'string=)))

(defun %memory-storage-validate-state-payload (event-type payload)
  (unless (and (%memory-storage-state-event-type-p event-type)
               (hash-table-p payload))
    (error 'memory-storage-error :operation :validate-mutation
           :detail "state command type or payload is invalid"))
  (let* ((node-p (string= event-type "memory-node-state"))
         (operation (gethash "operation" payload))
         (expected
           (if node-p
               '("operation" "mutation_kind" "scalar_json"
                 "embedding_binary_hex" "retrieval_embedding_binary_hex")
               '("operation" "mutation_kind" "row_json"))))
    (%memory-storage-required-string
     (gethash "mutation_kind" payload) "mutation-kind" :maximum 128)
    (unless (if node-p
                (member operation '("upsert" "update") :test #'string=)
                (member operation '("insert" "delete") :test #'string=))
      (error 'memory-storage-error :operation :validate-mutation
             :detail "operation is not valid for state command type"))
    (unless (%memory-storage-exact-object-keys-p payload expected)
      (error 'memory-storage-error :operation :validate-mutation
             :detail "state command payload does not match its closed schema"))
    (dolist (field (remove "operation" expected :test #'string=))
      (unless (and (stringp (gethash field payload))
                   (plusp (length (gethash field payload))))
        (error 'memory-storage-error :operation :validate-mutation
               :detail (format nil "~a must be a non-empty string" field)))))
  payload)

(defun make-memory-operation-command (event-type payload)
  (%memory-storage-validate-state-payload event-type payload)
  (%memory-storage-object "event_type" event-type "payload" payload))

(defun make-memory-operation-payload (operation-kind commands)
  (%memory-storage-required-string operation-kind "operation-kind" :maximum 64)
  (let* ((allowed (cdr (assoc operation-kind *memory-atomic-operation-kinds*
                              :test #'string=)))
         (command-vector
           (cond ((vectorp commands) commands)
                 ((listp commands) (coerce commands 'vector))
                 (t nil))))
    (unless (and allowed command-vector
                 (<= 1 (length command-vector) 4096))
      (error 'memory-storage-error :operation :validate-memory-operation
             :detail "operation family or bounded command vector is invalid"))
    (loop for command across command-vector
          for event-type = (and (hash-table-p command)
                                (gethash "event_type" command))
          for payload = (and (hash-table-p command)
                             (gethash "payload" command))
          do (unless (and
                      (hash-table-p command)
                      (%memory-storage-exact-object-keys-p
                       command '("event_type" "payload"))
                      (stringp event-type)
                      (hash-table-p payload)
                      (stringp (gethash "mutation_kind" payload))
                      (find-if
                       (lambda (pair)
                         (and (string= event-type (first pair))
                              (string= (gethash "mutation_kind" payload)
                                       (second pair))))
                       allowed))
               (error 'memory-storage-error
                      :operation :validate-memory-operation
                      :detail "operation contains an undeclared command kind"))
             (%memory-storage-validate-state-payload event-type payload))
    (%memory-storage-object "operation_kind" operation-kind
                            "commands" command-vector)))

(defun %memory-storage-validate-operation-payload (payload)
  (unless (and (hash-table-p payload)
               (%memory-storage-exact-object-keys-p
                payload '("operation_kind" "commands")))
    (error 'memory-storage-error :operation :validate-memory-operation
           :detail "operation payload does not match its closed schema"))
  (make-memory-operation-payload (gethash "operation_kind" payload)
                                 (gethash "commands" payload)))

(defparameter *memory-cognitive-mutation-families*
  '("admission" "supersession" "tick-commit" "user-visible-rehearsal"
    "direct-edge" "quarantine"))

(defvar *memory-cognitive-mutation-mode* :postgresql
  "Closed cognitive write selector. EVENT-FIRST is never selected at load.")
(defvar *memory-cognitive-mutation-router* nil
  "Optional complete replacement for one cognitive PostgreSQL transaction.")
(defvar *memory-search-storage-backend* nil
  "Selected memory read facade. NIL preserves incumbent PostgreSQL reads.")

(defun %memory-cognitive-list (value field &key (maximum 4096))
  (let ((items (cond ((null value) nil)
                     ((listp value) (copy-list value))
                     ((vectorp value) (coerce value 'list))
                     (t :invalid))))
    (when (or (eq items :invalid) (> (length items) maximum))
      (error 'memory-storage-error :operation :assemble-cognitive-operation
             :detail (format nil "~a is not a bounded sequence" field)))
    items))

(defun %memory-cognitive-node-envelope (envelope)
  (unless (and (hash-table-p envelope)
               (%memory-storage-exact-object-keys-p
                envelope '("scalar_json" "embedding_binary_hex"
                           "retrieval_embedding_binary_hex")))
    (error 'memory-storage-error :operation :assemble-cognitive-operation
           :detail "node envelope does not match its closed schema"))
  (let ((row (%memory-materializer-row (gethash "scalar_json" envelope))))
    (values row
            (gethash "embedding_binary_hex" envelope)
            (gethash "retrieval_embedding_binary_hex" envelope))))

(defparameter *memory-cognitive-edge-types*
  '("elaborates" "contradicts" "causes" "about" "follows"
    "evidence-for" "derived-from" "supersedes" "contains" "resolves"
    "proposal-outcome" "verdict-for"))

(defun %memory-cognitive-edge-row (row-json)
  (%memory-storage-required-string row-json "edge-row-json"
                                   :maximum (* 1024 1024))
  (let ((row (%memory-storage-json-read row-json
                                        :assemble-cognitive-operation)))
    (unless (hash-table-p row)
      (error 'memory-storage-error :operation :assemble-cognitive-operation
             :detail "cognitive edge row is not an object"))
    row))

(defun %memory-cognitive-edge-command
    (row-json operation mutation-kind &key from-id to-id edge-types)
  (let ((row (%memory-cognitive-edge-row row-json)))
    (unless (and (or (null from-id)
                     (string= from-id (gethash "from_id" row)))
                 (or (null to-id)
                     (string= to-id (gethash "to_id" row)))
                 (or (null edge-types)
                     (member (gethash "edge_type" row) edge-types
                             :test #'string=)))
      (error 'memory-storage-error :operation :assemble-cognitive-operation
             :detail "edge row conflicts with its cognitive operation")))
  (make-memory-operation-command
   "memory-edge-state"
   (make-memory-edge-state-payload row-json operation mutation-kind)))

(defun %memory-cognitive-operation-plan
    (operation-kind commands return-contract return-value)
  ;; Both validations happen here, before a router can append anything.
  (let* ((payload (make-memory-operation-payload operation-kind commands))
         (validated-return
           (%memory-operation-return-value return-contract return-value)))
    (%memory-storage-object
     "schema_version" 1
     "operation_kind" operation-kind
     "commands" (gethash "commands" payload)
     "return_contract" return-contract
     "return_value" validated-return)))

(defun memory-assemble-admission-operation
    (node-envelope deleted-lineage-row-jsons inserted-lineage-row-jsons)
  "Build one node upsert plus exact lineage replacement commands."
  (multiple-value-bind (row embedding retrieval)
      (%memory-cognitive-node-envelope node-envelope)
    (let ((commands nil))
      (push (make-memory-operation-command
             "memory-node-state"
             (make-memory-node-upsert-payload
              (%memory-materializer-json row) embedding retrieval
              "admission"))
            commands)
      (dolist (row-json (%memory-cognitive-list
                         deleted-lineage-row-jsons "deleted-lineage"))
        (push (%memory-cognitive-edge-command
               row-json "delete" "admission-lineage-replacement"
               :from-id (gethash "id" row)
               :edge-types '("derived-from"))
              commands))
      (dolist (row-json (%memory-cognitive-list
                         inserted-lineage-row-jsons "inserted-lineage"))
        (push (%memory-cognitive-edge-command
               row-json "insert" "admission-lineage"
               :from-id (gethash "id" row)
               :edge-types '("derived-from"))
              commands))
      (%memory-cognitive-operation-plan
       "admission" (nreverse commands) "node-id" (gethash "id" row)))))

(defun memory-assemble-supersession-operation
    (replacement-envelope old-id reason actor inserted-edge-row-json)
  "Build the replacement-node update and optional new supersession edge."
  (multiple-value-bind (row embedding retrieval)
      (%memory-cognitive-node-envelope replacement-envelope)
    (let ((commands
            (list
             (make-memory-operation-command
              "memory-node-state"
              (memory-materialize-node-supersession
               (%memory-materializer-json row) embedding retrieval
               old-id reason actor)))))
      (when inserted-edge-row-json
        (setf commands
              (append commands
                      (list (%memory-cognitive-edge-command
                             inserted-edge-row-json "insert"
                             "supersession"
                             :from-id (gethash "id" row) :to-id old-id
                             :edge-types '("supersedes"))))))
      (%memory-cognitive-operation-plan
       "supersession" commands "true" t))))

(defun memory-assemble-tick-commit-operation (node-envelopes edge-row-jsons)
  "Build one ordered autonomous node/edge commit. Caller supplies exact rows."
  (let ((commands nil) (ids nil))
    (dolist (envelope (%memory-cognitive-list node-envelopes "tick-nodes"))
      (multiple-value-bind (row embedding retrieval)
          (%memory-cognitive-node-envelope envelope)
        (push (make-memory-operation-command
               "memory-node-state"
               (make-memory-node-upsert-payload
                (%memory-materializer-json row) embedding retrieval
                "node-write"))
              commands)
        (push (gethash "id" row) ids)))
    (dolist (edge (%memory-cognitive-list edge-row-jsons "tick-edges"))
      (let* ((descriptor-p (hash-table-p edge))
             (row-json (if descriptor-p (gethash "row_json" edge) edge))
             (mutation-kind
               (if descriptor-p (gethash "mutation_kind" edge)
                   "tick-commit-edge"))
             (edge-types
               (cond ((and (stringp mutation-kind)
                           (string= mutation-kind "tick-commit-lineage"))
                      '("derived-from"))
                     ((and (stringp mutation-kind)
                           (string= mutation-kind "tick-commit-supersession"))
                      '("supersedes"))
                     ((and (stringp mutation-kind)
                           (string= mutation-kind "tick-commit-edge"))
                      *memory-cognitive-edge-types*)
                     (t nil))))
        (unless (and edge-types
                     (or (not descriptor-p)
                         (%memory-storage-exact-object-keys-p
                          edge '("row_json" "mutation_kind"))))
          (error 'memory-storage-error :operation :assemble-cognitive-operation
                 :detail "tick edge descriptor is outside the closed schema"))
        (push (%memory-cognitive-edge-command
               row-json "insert" mutation-kind :edge-types edge-types)
              commands)))
    (%memory-cognitive-operation-plan
     "tick-commit" (nreverse commands) "node-id-list" (nreverse ids))))

(defun memory-assemble-user-visible-rehearsal-operation
    (node-envelopes timestamp consumer generation-id requested-count)
  "Build exact successful rehearsal replacements and their incumbent report."
  (%memory-storage-required-string consumer "rehearsal-consumer" :maximum 256)
  (unless (and (integerp requested-count) (not (minusp requested-count)))
    (error 'memory-storage-error :operation :assemble-cognitive-operation
           :detail "requested rehearsal count must be non-negative"))
  (let ((commands nil))
    (dolist (envelope (%memory-cognitive-list
                       node-envelopes "rehearsal-nodes"))
      (multiple-value-bind (row embedding retrieval)
          (%memory-cognitive-node-envelope envelope)
        (push (make-memory-operation-command
               "memory-node-state"
               (memory-materialize-node-rehearsal
                (%memory-materializer-json row) embedding retrieval
                :user-visible timestamp))
              commands)))
    (unless commands
      (error 'memory-storage-error :operation :assemble-cognitive-operation
             :detail "an empty rehearsal is not a durable operation"))
    (setf commands (nreverse commands))
    (%memory-cognitive-operation-plan
     "user-visible-rehearsal" commands "use-report"
     (%memory-storage-object
      "consumer" consumer "generation_id" generation-id
      "user_visible" t "requested_count" requested-count
      "updated_count" (length commands)))))

(defun memory-assemble-direct-edge-operation (edge-row-json)
  (%memory-cognitive-operation-plan
   "direct-edge"
   (list (%memory-cognitive-edge-command
          edge-row-json "insert" "direct-edge"
          :edge-types *memory-cognitive-edge-types*))
   "true" t))

(defun memory-assemble-quarantine-operation
    (node-envelope reason actor)
  (multiple-value-bind (row embedding retrieval)
      (%memory-cognitive-node-envelope node-envelope)
    (%memory-cognitive-operation-plan
     "quarantine"
     (list
      (make-memory-operation-command
       "memory-node-state"
       (memory-materialize-node-quarantine
        (%memory-materializer-json row) embedding retrieval reason actor)))
     "true" t)))

(defun memory-cognitive-mutation-dispatch (family request postgres-thunk)
  "Choose exactly one complete transaction route. There is no shadow path."
  (unless (and (member family *memory-cognitive-mutation-families*
                       :test #'string=)
               (hash-table-p request) (functionp postgres-thunk))
    (error 'memory-storage-error :operation :route-cognitive-mutation
           :detail "cognitive mutation request is invalid"))
  (case *memory-cognitive-mutation-mode*
    (:postgresql (funcall postgres-thunk))
    (:event-first
     (unless (functionp *memory-cognitive-mutation-router*)
       (error 'memory-storage-error :operation :route-cognitive-mutation
              :detail "event-first mode has no complete router"))
     (funcall *memory-cognitive-mutation-router* family request))
    (otherwise
     (error 'memory-storage-error :operation :route-cognitive-mutation
            :detail "unknown cognitive mutation mode"))))

(defun make-memory-storage-mutation (&key event-json storage-position storage-id)
  (%memory-storage-required-string event-json "event-json" :maximum (* 16 1024 1024))
  (let* ((event (%memory-storage-json-read event-json :validate-memory-mutation))
         (event-id (and (hash-table-p event) (gethash "id" event)))
         (agent-id (and (hash-table-p event) (gethash "agent_id" event)))
         (event-type (and (hash-table-p event) (gethash "type" event)))
         (payload (and (hash-table-p event) (gethash "payload" event)))
         (event-hash (%memory-storage-sha256 event-json)))
  (unless (and (eql 1 (gethash "schema_version" event))
               (integerp event-id) (plusp event-id)
               (integerp storage-position) (plusp storage-position))
    (error 'memory-storage-error :operation :validate-mutation
           :detail "event identity must be positive"))
  (dolist (item (list (list storage-id "storage-id" 256)
                      (list agent-id "agent-id" 128)
                      (list event-hash "event-hash" 128)
                      (list event-type "event-type" 64)))
    (%memory-storage-required-string (first item) (second item)
                                     :maximum (third item)))
  (unless (member event-type '("memory-node-state" "memory-edge-state"
                               "memory-operation-state")
                  :test #'string=)
    (error 'memory-storage-error :operation :validate-mutation
           :detail "unknown memory mutation event type"))
  (if (string= event-type "memory-operation-state")
      (%memory-storage-validate-operation-payload payload)
      (%memory-storage-validate-state-payload event-type payload))
  (%memory-storage-object
   "schema_version" 1 "event_id" event-id
   "storage_position" storage-position "storage_id" storage-id
   "agent_id" agent-id "event_hash" event-hash
   "event_type" event-type "event_json" event-json)))

(defun %memory-storage-mutation-payload (mutation)
  (let* ((event-json (gethash "event_json" mutation))
         (event (%memory-storage-json-read event-json :apply-memory-mutation)))
    (unless (and (string= (gethash "event_hash" mutation)
                          (%memory-storage-sha256 event-json))
                 (= (gethash "event_id" mutation) (gethash "id" event))
                 (string= (gethash "agent_id" mutation)
                          (gethash "agent_id" event))
                 (string= (gethash "event_type" mutation)
                          (gethash "type" event)))
      (error 'memory-storage-error :operation :apply-memory-mutation
             :detail "mutation envelope diverges from durable event JSON"))
    (gethash "payload" event)))

(defun make-memory-storage-mutation-from-receipt (receipt)
  (let ((expected '("schema_version" "storage_id" "storage_position"
                    "event_id" "agent_id" "event_type" "event_json"
                    "integrity_hash")))
    (unless (and (hash-table-p receipt)
                 (%memory-storage-exact-object-keys-p receipt expected)
                 (eql 1 (gethash "schema_version" receipt))
                 (stringp (gethash "event_json" receipt))
                 (stringp (gethash "integrity_hash" receipt))
                 (stringp (gethash "storage_id" receipt))
                 (and (integerp (gethash "storage_position" receipt))
                      (plusp (gethash "storage_position" receipt)))
                 (and (integerp (gethash "event_id" receipt))
                      (plusp (gethash "event_id" receipt)))
                 (stringp (gethash "agent_id" receipt))
                 (stringp (gethash "event_type" receipt))
                 (string= (gethash "integrity_hash" receipt)
                          (%memory-storage-sha256
                           (gethash "event_json" receipt))))
      (error 'memory-storage-error :operation :validate-event-receipt
             :detail "durable event receipt is incomplete or hash-invalid"))
    (let ((mutation
            (make-memory-storage-mutation
             :event-json (gethash "event_json" receipt)
             :storage-position (gethash "storage_position" receipt)
             :storage-id (gethash "storage_id" receipt))))
      (unless (and (= (gethash "event_id" receipt)
                      (gethash "event_id" mutation))
                   (string= (gethash "agent_id" receipt)
                            (gethash "agent_id" mutation))
                   (string= (gethash "event_type" receipt)
                            (gethash "event_type" mutation)))
        (error 'memory-storage-error :operation :validate-event-receipt
               :detail "durable receipt index disagrees with event JSON"))
      mutation)))

(defparameter *memory-node-upsert-mutation-kinds*
  '("node-write" "admission"))
(defparameter *memory-edge-insert-mutation-kinds*
  '("direct-edge" "admission-lineage" "supersession"
    "tick-commit-lineage" "tick-commit-supersession" "tick-commit-edge"))
(defparameter *memory-edge-delete-mutation-kinds*
  '("admission-lineage-replacement"))

(defun %memory-materializer-required-kind (kind allowed)
  (%memory-storage-required-string kind "mutation-kind" :maximum 128)
  (unless (member kind allowed :test #'string=)
    (error 'memory-storage-error :operation :materialize-memory-mutation
           :detail "mutation kind is not valid for this cognitive operation"))
  kind)

(defun %memory-materializer-row (scalar-json)
  (%memory-storage-required-string scalar-json "scalar-json"
                                   :maximum (* 16 1024 1024))
  (let ((row (%memory-storage-json-read scalar-json
                                        :materialize-memory-mutation)))
    (unless (and (hash-table-p row)
                 (stringp (gethash "id" row))
                 (plusp (length (gethash "id" row))))
      (error 'memory-storage-error :operation :materialize-memory-mutation
             :detail "node row has no valid identity"))
    row))

(defun %memory-materializer-json (row)
  (handler-case
      (let ((*print-pretty* nil)) (shasht:write-json row nil))
    (error (condition)
      (error 'memory-storage-error :operation :materialize-memory-mutation
             :detail condition))))

(defun %memory-materializer-nullable (value)
  (if (null value) :null value))

(defun make-memory-node-scalar-row
    (&key id (kind "observation") content timestamp
          (importance 0.5) (valence 0.0) (arousal 0.3)
          source-event-id (origin-class "legacy-unclassified")
          (epistemic-status "legacy-unclassified") producer model-purpose
          confidence (grounding-status "unclassified")
          (root-observation-ids (vector)) generation-id supersedes-node-id
          quarantined (epistemic-metadata (%memory-storage-object)))
  "Construct the 23 scalar columns produced by the incumbent INSERT. The
caller supplies one explicit timestamp for PostgreSQL statement-time parity."
  (dolist (item (list (list id "node-id" 256)
                      (list kind "node-kind" 64)
                      (list timestamp "node-timestamp" 64)
                      (list origin-class "origin-class" 128)
                      (list epistemic-status "epistemic-status" 128)
                      (list grounding-status "grounding-status" 128)))
    (%memory-storage-required-string (first item) (second item)
                                     :maximum (third item)))
  (unless (and (every #'realp (list importance valence arousal))
               (or (null confidence) (realp confidence))
               (or (null quarantined) (eq quarantined t))
               (or (vectorp root-observation-ids)
                   (listp root-observation-ids))
               (hash-table-p epistemic-metadata))
    (error 'memory-storage-error :operation :materialize-memory-row
           :detail "node scalar inputs do not match the incumbent schema"))
  (%memory-materializer-json
   (%memory-storage-object
    "id" id "kind" kind "content" (%memory-materializer-nullable content)
    "created_at" timestamp "last_accessed" timestamp "access_count" 0
    ;; PostgreSQL columns are REAL (float4), so narrow these before JSON.
    "importance" (coerce importance 'single-float)
    "valence" (coerce valence 'single-float)
    "arousal_at_encoding" (coerce arousal 'single-float)
    "activation" 1.0f0
    "source_event_id" (%memory-materializer-nullable
                        (and source-event-id (format nil "~a" source-event-id)))
    "is_cold" nil "origin_class" origin-class
    "epistemic_status" epistemic-status
    "producer" (%memory-materializer-nullable producer)
    "model_purpose" (%memory-materializer-nullable model-purpose)
    "confidence" (%memory-materializer-nullable confidence)
    "grounding_status" grounding-status
    "root_observation_ids"
    (if (vectorp root-observation-ids) root-observation-ids
        (coerce root-observation-ids 'vector))
    "generation_id" (%memory-materializer-nullable generation-id)
    "supersedes_node_id" (%memory-materializer-nullable supersedes-node-id)
    "quarantined" (if quarantined t nil)
    "epistemic_metadata" epistemic-metadata)))

(defun %memory-materializer-json-null-p (value)
  (or (null value) (eq value :null)))

(defun %memory-materializer-nullable-string-p (value)
  (or (%memory-materializer-json-null-p value) (stringp value)))

(defparameter *memory-node-scalar-row-fields*
  '("id" "kind" "content" "created_at" "last_accessed" "access_count"
    "importance" "valence" "arousal_at_encoding" "activation"
    "source_event_id" "is_cold" "origin_class" "epistemic_status"
    "producer" "model_purpose" "confidence" "grounding_status"
    "root_observation_ids" "generation_id" "supersedes_node_id"
    "quarantined" "epistemic_metadata"))

(defun %memory-materializer-valid-scalar-row-p (row)
  (and (hash-table-p row)
       (%memory-storage-exact-object-keys-p
        row *memory-node-scalar-row-fields*)
       (stringp (gethash "id" row))
       (plusp (length (gethash "id" row)))
       (every (lambda (field) (stringp (gethash field row)))
              '("kind" "created_at" "last_accessed" "origin_class"
                "epistemic_status" "grounding_status"))
       (%memory-materializer-nullable-string-p (gethash "content" row))
       (and (integerp (gethash "access_count" row))
            (not (minusp (gethash "access_count" row))))
       (every (lambda (field) (realp (gethash field row)))
              '("importance" "valence" "arousal_at_encoding" "activation"))
       (every (lambda (field)
                (%memory-materializer-nullable-string-p (gethash field row)))
              '("source_event_id" "producer" "model_purpose"
                "generation_id" "supersedes_node_id"))
       (let ((confidence (gethash "confidence" row)))
         (or (%memory-materializer-json-null-p confidence)
             (realp confidence)))
       (member (gethash "is_cold" row) '(nil t))
       (member (gethash "quarantined" row) '(nil t))
       (or (vectorp (gethash "root_observation_ids" row))
           (listp (gethash "root_observation_ids" row)))
       (hash-table-p (gethash "epistemic_metadata" row))))

(defun memory-merge-node-upsert-rows (existing proposed)
  "Reproduce MEMORY-NODES.LISP's ON CONFLICT update over scalar row objects.
Fields absent from the SQL update list remain byte-semantically incumbent."
  (unless (and (%memory-materializer-valid-scalar-row-p existing)
               (%memory-materializer-valid-scalar-row-p proposed)
               (string= (gethash "id" existing) (gethash "id" proposed)))
    (error 'memory-storage-error :operation :merge-memory-row
           :detail "upsert rows must have the same valid identity"))
  (let ((merged
          (%memory-storage-json-read
           (%memory-materializer-json existing) :merge-memory-row)))
    (dolist (field '("kind" "content" "importance" "valence"
                     "arousal_at_encoding" "source_event_id"))
      (setf (gethash field merged) (gethash field proposed)))
    (dolist (entry '(("origin_class" "legacy-unclassified")
                     ("epistemic_status" "legacy-unclassified")
                     ("grounding_status" "unclassified")))
      (unless (string= (gethash (first entry) proposed) (second entry))
        (setf (gethash (first entry) merged)
              (gethash (first entry) proposed))))
    (dolist (field '("producer" "model_purpose" "confidence"
                     "generation_id" "supersedes_node_id"))
      (unless (%memory-materializer-json-null-p (gethash field proposed))
        (setf (gethash field merged) (gethash field proposed))))
    (let ((roots (gethash "root_observation_ids" proposed)))
      (unless (and (or (vectorp roots) (listp roots)) (zerop (length roots)))
        (setf (gethash "root_observation_ids" merged) roots)))
    (when (gethash "quarantined" proposed)
      (setf (gethash "quarantined" merged) t))
    (let ((metadata (gethash "epistemic_metadata" proposed)))
      (unless (and (hash-table-p metadata) (zerop (hash-table-count metadata)))
        (setf (gethash "epistemic_metadata" merged) metadata)))
    (%memory-materializer-json merged)))

(defun %memory-node-state-payload
    (row embedding-hex retrieval-hex operation mutation-kind)
  (dolist (item (list (list embedding-hex "embedding-binary-hex")
                      (list retrieval-hex "retrieval-embedding-binary-hex")))
    (%memory-storage-required-string (first item) (second item)
                                     :maximum (* 16 1024 1024)))
  (%memory-storage-object
   "operation" operation "mutation_kind" mutation-kind
   "scalar_json" (%memory-materializer-json row)
   "embedding_binary_hex" embedding-hex
   "retrieval_embedding_binary_hex" retrieval-hex))

(defun make-memory-node-upsert-payload
    (scalar-json embedding-hex retrieval-hex mutation-kind)
  (%memory-materializer-required-kind
   mutation-kind *memory-node-upsert-mutation-kinds*)
  (%memory-node-state-payload
   (%memory-materializer-row scalar-json) embedding-hex retrieval-hex
   "upsert" mutation-kind))

(defun %memory-materializer-metadata (row)
  (let ((metadata (gethash "epistemic_metadata" row)))
    (unless (hash-table-p metadata)
      (error 'memory-storage-error :operation :materialize-memory-mutation
             :detail "node epistemic metadata must be an object"))
    metadata))

(defun memory-materialize-node-quarantine
    (scalar-json embedding-hex retrieval-hex reason actor)
  (%memory-storage-required-string reason "quarantine-reason" :maximum 2048)
  (%memory-storage-required-string actor "quarantine-actor" :maximum 256)
  (let* ((row (%memory-materializer-row scalar-json))
         (metadata (%memory-materializer-metadata row)))
    (setf (gethash "quarantined" row) t
          (gethash "quarantine_reason" metadata) reason
          (gethash "quarantined_by" metadata) actor)
    (%memory-node-state-payload
     row embedding-hex retrieval-hex "update" "quarantine")))

(defun memory-materialize-node-supersession
    (scalar-json embedding-hex retrieval-hex old-id reason actor)
  (dolist (item (list (list old-id "superseded-node-id" 256)
                      (list reason "supersession-reason" 2048)
                      (list actor "supersession-actor" 256)))
    (%memory-storage-required-string (first item) (second item)
                                     :maximum (third item)))
  (let* ((row (%memory-materializer-row scalar-json))
         (metadata (%memory-materializer-metadata row)))
    (setf (gethash "supersedes_node_id" row) old-id
          (gethash "supersession_reason" metadata) reason
          (gethash "superseded_by_actor" metadata) actor)
    (%memory-node-state-payload
     row embedding-hex retrieval-hex "update" "supersession")))

(defun memory-materialize-node-rehearsal
    (scalar-json embedding-hex retrieval-hex mode timestamp)
  (%memory-storage-required-string timestamp "rehearsal-timestamp" :maximum 64)
  (unless (member mode '(:legacy-recall :user-visible))
    (error 'memory-storage-error :operation :materialize-memory-mutation
           :detail "unknown rehearsal policy"))
  (let* ((row (%memory-materializer-row scalar-json))
         (access-count (gethash "access_count" row))
         (activation (gethash "activation" row)))
    (unless (and (integerp access-count) (not (minusp access-count))
                 (realp activation))
      (error 'memory-storage-error :operation :materialize-memory-mutation
             :detail "node rehearsal fields are invalid"))
    (setf (gethash "access_count" row) (1+ access-count)
          (gethash "last_accessed" row) timestamp)
    (ecase mode
      (:legacy-recall
       (setf (gethash "activation" row)
             (min 1.0d0 (+ (coerce activation 'double-float) 0.2d0))))
      (:user-visible
       (when (< activation 0.85d0)
         (setf (gethash "activation" row)
               (min 0.85d0 (+ (coerce activation 'double-float) 0.05d0))))))
    (%memory-node-state-payload
     row embedding-hex retrieval-hex "update"
     (ecase mode
       (:legacy-recall "legacy-recall-rehearsal")
       (:user-visible "user-visible-rehearsal")))))

(defun memory-materialize-node-decay
    (scalar-json embedding-hex retrieval-hex activation cold-p)
  (unless (and (realp activation) (<= 0 activation 1)
               (member cold-p '(nil t)))
    (error 'memory-storage-error :operation :materialize-memory-mutation
           :detail "decay requires bounded activation and boolean cold state"))
  (let ((row (%memory-materializer-row scalar-json)))
    (setf (gethash "activation" row) activation
          (gethash "is_cold" row) (if cold-p t nil))
    (%memory-node-state-payload
     row embedding-hex retrieval-hex "update" "decay")))

(defun memory-materialize-node-retrieval-backfill
    (scalar-json embedding-hex retrieval-hex)
  (%memory-node-state-payload
   (%memory-materializer-row scalar-json) embedding-hex retrieval-hex
   "update" "retrieval-embedding-backfill"))

(defun make-memory-edge-state-payload (row-json operation mutation-kind)
  (%memory-storage-required-string row-json "edge-row-json"
                                   :maximum (* 1024 1024))
  (%memory-storage-required-string operation "edge-operation" :maximum 16)
  (let ((allowed
          (cond ((string= operation "insert")
                 *memory-edge-insert-mutation-kinds*)
                ((string= operation "delete")
                 *memory-edge-delete-mutation-kinds*)
                (t
                 (error 'memory-storage-error
                        :operation :materialize-memory-mutation
                        :detail "unknown edge operation")))))
    (%memory-materializer-required-kind mutation-kind allowed)
    (let ((row (%memory-storage-json-read row-json
                                         :materialize-memory-mutation)))
      (unless (and (hash-table-p row)
                   (integerp (gethash "id" row))
                   (plusp (gethash "id" row))
                   (every (lambda (field)
                            (let ((value (gethash field row)))
                              (and (stringp value) (plusp (length value)))))
                          '("from_id" "to_id" "edge_type")))
        (error 'memory-storage-error :operation :materialize-memory-mutation
               :detail "edge row is incomplete")))
    (%memory-storage-object
     "operation" operation "mutation_kind" mutation-kind
     "row_json" row-json)))

(defgeneric memory-storage-capabilities (backend)
  (:documentation "Return content-free implemented capability truth."))

(defgeneric memory-storage-characterize (backend)
  (:documentation
   "Return content-free source facts from one repeatable read-only snapshot."))

(defgeneric memory-storage-map-snapshot (backend node-visitor edge-visitor)
  (:documentation
   "Visit exact private node envelopes and edge row JSON in stable order from
one repeatable read-only snapshot. Return only a content-free receipt."))

(defgeneric memory-storage-import-snapshot (backend source provenance)
  (:documentation
   "Atomically import one exact source snapshot under explicit provenance."))

(defgeneric memory-storage-audit-snapshot (backend)
  (:documentation
   "Independently audit imported rows and return a content-free receipt."))

(defgeneric memory-storage-exact-search (backend query)
  (:documentation
   "Return deterministic cosine-distance candidates from a closed exact-query
profile. This qualification port does not confer runtime read authority."))

(defgeneric memory-storage-lexical-search (backend query)
  (:documentation
   "Return bounded literal token/phrase candidates through an exact query."))

(defgeneric memory-storage-bind-projection
    (backend &key baseline-seal storage-id agent-id through-event-id
                  through-position boundary-hash projector-revision))
(defgeneric memory-storage-apply-mutation (backend mutation))
(defgeneric memory-storage-projection-report (backend))
(defgeneric memory-storage-operation-node (backend node-id)
  (:documentation
   "Return an exact integrity-checked node envelope for operation planning."))
(defgeneric memory-storage-operation-edges
    (backend &key from-id to-id edge-type)
  (:documentation
   "Return exact integrity-checked edge row JSON ordered by durable identity."))
(defgeneric memory-storage-operation-next-edge-id (backend)
  (:documentation "Return the next available positive edge identity."))
(defgeneric memory-storage-operation-vector-dimension (backend)
  (:documentation "Return the sealed projection's vector dimension."))

(defmethod memory-storage-capabilities ((backend memory-storage-backend))
  (declare (ignore backend))
  (%memory-storage-object
   "schema_version" 1 "backend" "abstract" "authority_role" "none"
   "read_snapshot" nil "node_snapshot" nil "edge_snapshot" nil
   "exact_vector_export" nil "exact_retrieval" nil
   "mutation_projection" nil
   "runtime_reads" nil "runtime_writes" nil))

(defun %memory-exact-u32 (octets offset)
  (+ (ash (aref octets offset) 24)
     (ash (aref octets (+ offset 1)) 16)
     (ash (aref octets (+ offset 2)) 8)
     (aref octets (+ offset 3))))

(defun %memory-exact-float32 (bits)
  ;; Decode IEEE-754 explicitly. The stored interchange is pgvector's
  ;; network-order binary send format, not the host's float representation.
  (let* ((negative (logbitp 31 bits))
         (exponent (ldb (byte 8 23) bits))
         (fraction (ldb (byte 23 0) bits))
         (sign (if negative -1.0d0 1.0d0)))
    (when (= exponent 255)
      (error 'memory-storage-error :operation :decode-vector
             :detail "non-finite float in exact query vector"))
    (coerce
     (* sign
        (if (zerop exponent)
            (* fraction (expt 2.0d0 -149))
            (* (+ 1.0d0 (/ fraction (expt 2.0d0 23)))
               (expt 2.0d0 (- exponent 127)))))
     'single-float)))

(defun %memory-exact-decode-vector-octets (octets)
  (unless (and (typep octets '(vector (unsigned-byte 8)))
               (>= (length octets) 8))
    (error 'memory-storage-error :operation :decode-vector
           :detail "exact query vector must be a pgvector-send value"))
  (let ((dimension (+ (ash (aref octets 0) 8) (aref octets 1)))
        (flags (+ (ash (aref octets 2) 8) (aref octets 3))))
    (unless (and (plusp dimension) (zerop flags)
                 (= (length octets) (+ 4 (* dimension 4))))
      (error 'memory-storage-error :operation :decode-vector
             :detail "exact query vector header or length is invalid"))
    (let ((values (make-array dimension :element-type 'single-float)))
      (dotimes (index dimension values)
        (setf (aref values index)
              (%memory-exact-float32
               (%memory-exact-u32 octets (+ 4 (* index 4)))))))))

(defun %memory-exact-decode-vector-hex (text)
  (unless (and (stringp text) (evenp (length text)) (>= (length text) 8))
    (error 'memory-storage-error :operation :decode-vector
           :detail "exact query vector must be pgvector-send hexadecimal"))
  (let ((octets (make-array (/ (length text) 2)
                            :element-type '(unsigned-byte 8))))
    (handler-case
        (dotimes (index (length octets))
          (setf (aref octets index)
                (parse-integer text :start (* index 2) :end (+ (* index 2) 2)
                             :radix 16 :junk-allowed nil)))
      (error ()
        (error 'memory-storage-error :operation :decode-vector
               :detail "exact query vector contains invalid hexadecimal")))
    (%memory-exact-decode-vector-octets octets)))

(defun %memory-exact-string-list (value field)
  (let ((items (cond ((null value) nil)
                     ((vectorp value) (coerce value 'list))
                     ((listp value) value)
                     (t :invalid))))
    (unless (and (not (eq items :invalid))
                 (every (lambda (item)
                          (and (stringp item) (plusp (length item))
                               (<= (length item) 256)))
                        items))
      (error 'memory-storage-error :operation :exact-query
             :detail (format nil "~a must contain bounded strings" field)))
    (remove-duplicates items :test #'string=)))

(defun %memory-exact-values-vector (values)
  (let ((items (cond ((vectorp values) (coerce values 'list))
                     ((listp values) values)
                     (t nil))))
    (unless (and items (every #'realp items))
      (error 'memory-storage-error :operation :exact-query
             :detail "exact query vector values must be real numbers"))
    (let ((vector (make-array (length items) :element-type 'single-float)))
      (loop for value in items for index from 0
            do (setf (aref vector index) (coerce value 'single-float)))
      vector)))

(defun %memory-lexical-lexemes (value)
  (let ((items (cond ((listp value) value)
                     ((vectorp value) (coerce value 'list))
                     (t nil))))
    (unless (and items (<= (length items) 6)
                 (every
                  (lambda (item)
                    (and (hash-table-p item)
                         (let ((text (gethash "text" item))
                               (kind (gethash "kind" item)))
                           (and (stringp text) (<= 3 (length text) 80)
                                (member kind '("token" "phrase")
                                        :test #'string=)))))
                  items))
      (error 'memory-storage-error :operation :lexical-query
             :detail "lexemes must be a bounded closed token/phrase list"))
    items))

(defun make-memory-exact-query
    (&key vector-binary-hex vector-values profile (limit 20) turn-ids
          kinds origins statuses grounding-statuses excluded-ids
          excluded-turn-ids excluded-source-event-ids as-of hydrate-p
          include-vector-p lexemes)
  (unless (member profile *memory-exact-query-profiles* :test #'string=)
    (error 'memory-storage-error :operation :exact-query
           :detail "unknown exact retrieval profile"))
  (unless (and (integerp limit) (<= 1 limit 250))
    (error 'memory-storage-error :operation :exact-query
           :detail "exact retrieval limit must be from 1 through 250"))
  (unless (or (null as-of)
              (and (stringp as-of) (<= 19 (length as-of) 64)))
    (error 'memory-storage-error :operation :exact-query
           :detail "exact retrieval as-of must be a bounded timestamp"))
  (unless (not (eq (null vector-binary-hex) (null vector-values)))
    (error 'memory-storage-error :operation :exact-query
           :detail "provide exactly one exact query vector representation"))
  (let ((turn-list (%memory-exact-string-list turn-ids "turn-ids")))
    (unless (or (not (string= profile "turn-neighborhood-v1")) turn-list)
      (error 'memory-storage-error :operation :exact-query
             :detail "turn-neighborhood profile requires valid turn IDs"))
    (let ((vector (if vector-binary-hex
                      (%memory-exact-decode-vector-hex vector-binary-hex)
                      (%memory-exact-values-vector vector-values))))
      (unless (loop for value across vector
                    thereis (not (zerop value)))
        (error 'memory-storage-error :operation :exact-query
               :detail "exact retrieval query vector must be nonzero"))
      (%make-memory-exact-query
       :vector vector :profile profile :limit limit
       :turn-ids turn-list
       :kinds (%memory-exact-string-list kinds "kinds")
       :origins (%memory-exact-string-list origins "origins")
       :statuses (%memory-exact-string-list statuses "statuses")
       :grounding-statuses
       (%memory-exact-string-list grounding-statuses "grounding-statuses")
       :excluded-ids (%memory-exact-string-list excluded-ids "excluded-ids")
       :excluded-turn-ids
       (%memory-exact-string-list excluded-turn-ids "excluded-turn-ids")
       :excluded-source-event-ids
       (%memory-exact-string-list excluded-source-event-ids
                                  "excluded-source-event-ids")
       :as-of as-of :hydrate-p (if hydrate-p t nil)
       :include-vector-p (if include-vector-p t nil)
       :lexemes (and lexemes (%memory-lexical-lexemes lexemes))))))

(defun %memory-exact-timestamp-key (value)
  (when (and (stringp value) (>= (length value) 19))
    (let ((key (subseq value 0 19)))
      (setf (aref key 10) #\Space)
      key)))

(defun %memory-exact-row-eligible-p (row query)
  (let* ((profile (memory-exact-query-profile query))
         (metadata (gethash "epistemic_metadata" row))
         (turn-id (and (hash-table-p metadata) (gethash "turn_id" metadata)))
         (created-key (%memory-exact-timestamp-key (gethash "created_at" row)))
         (as-of-key (%memory-exact-timestamp-key
                     (memory-exact-query-as-of query))))
    (and
     (or
      (string= profile "all-vectors-v1")
      (and
       (null (gethash "is_cold" row))
       (null (gethash "quarantined" row))
       (not (string= "legacy-unclassified"
                     (or (gethash "origin_class" row) "")))
       (not (member (gethash "epistemic_status" row)
                    '("legacy-unclassified" "rejected") :test #'string=))
       (not (string= "unclassified"
                     (or (gethash "grounding_status" row) "")))
       (or
        (string= profile "safe-semantic-v1")
        (and
         (member (gethash "origin_class" row)
                 '("lived-user" "lived-agent-action" "tool-result"
                   "external-source") :test #'string=)
         (member (gethash "grounding_status" row)
                 '("grounded" "partially-grounded") :test #'string=)
         (member turn-id (memory-exact-query-turn-ids query)
                 :test #'string=)))))
     (or (null (memory-exact-query-kinds query))
         (member (gethash "kind" row) (memory-exact-query-kinds query)
                 :test #'string=))
     (or (null (memory-exact-query-origins query))
         (member (gethash "origin_class" row)
                 (memory-exact-query-origins query) :test #'string=))
     (or (null (memory-exact-query-statuses query))
         (member (gethash "epistemic_status" row)
                 (memory-exact-query-statuses query) :test #'string=))
     (or (null (memory-exact-query-grounding-statuses query))
         (member (gethash "grounding_status" row)
                 (memory-exact-query-grounding-statuses query) :test #'string=))
     (not (member (gethash "id" row)
                  (memory-exact-query-excluded-ids query) :test #'string=))
     (not (member turn-id (memory-exact-query-excluded-turn-ids query)
                  :test #'string=))
     (not (member (gethash "source_event_id" row)
                  (memory-exact-query-excluded-source-event-ids query)
                  :test #'string=))
     (or (null as-of-key)
         (and created-key (string<= created-key as-of-key))))))

(defun %memory-exact-cosine-distance (left right)
  (unless (= (length left) (length right))
    (error 'memory-storage-error :operation :exact-search
           :detail "query and candidate vector dimensions differ"))
  (let ((dot 0.0d0) (left-norm 0.0d0) (right-norm 0.0d0))
    (declare (type double-float dot left-norm right-norm))
    ;; The stored vectors are single-float arrays. Typed accumulation avoids
    ;; boxing a double per operation, which otherwise allocated on the order
    ;; of a gigabyte of garbage for one scan of a large memory.
    (if (and (typep left '(simple-array single-float (*)))
             (typep right '(simple-array single-float (*))))
        (let ((left left) (right right))
          (declare (type (simple-array single-float (*)) left right)
                   (optimize (speed 3) (safety 0)))
          (dotimes (index (length left))
            (let ((a (coerce (aref left index) 'double-float))
                  (b (coerce (aref right index) 'double-float)))
              (declare (type double-float a b))
              (incf dot (* a b))
              (incf left-norm (* a a))
              (incf right-norm (* b b)))))
        (dotimes (index (length left))
          (let ((a (coerce (aref left index) 'double-float))
                (b (coerce (aref right index) 'double-float)))
            (incf dot (* a b))
            (incf left-norm (* a a))
            (incf right-norm (* b b)))))
    (unless (and (plusp left-norm) (plusp right-norm))
      (error 'memory-storage-error :operation :exact-search
             :detail "cosine distance is undefined for a zero vector"))
    (- 1.0d0 (/ dot (sqrt (* left-norm right-norm))))))

(defmethod memory-storage-characterize ((backend memory-storage-backend))
  (declare (ignore backend))
  (error 'memory-storage-unsupported-error :operation :characterize
         :detail "backend does not implement source characterization"))

(defmethod memory-storage-map-snapshot
    ((backend memory-storage-backend) node-visitor edge-visitor)
  (declare (ignore backend node-visitor edge-visitor))
  (error 'memory-storage-unsupported-error :operation :map-snapshot
         :detail "backend does not implement source snapshot mapping"))

(defmethod memory-storage-import-snapshot
    ((backend memory-storage-backend) source provenance)
  (declare (ignore backend source provenance))
  (error 'memory-storage-unsupported-error :operation :import-snapshot
         :detail "backend does not implement atomic snapshot import"))

(defmethod memory-storage-audit-snapshot ((backend memory-storage-backend))
  (declare (ignore backend))
  (error 'memory-storage-unsupported-error :operation :audit-snapshot
         :detail "backend does not implement snapshot audit"))

(defmethod memory-storage-exact-search
    ((backend memory-storage-backend) (query memory-exact-query))
  (declare (ignore backend query))
  (error 'memory-storage-unsupported-error :operation :exact-search
         :detail "backend does not implement exact retrieval"))

(defmethod memory-storage-lexical-search
    ((backend memory-storage-backend) (query memory-exact-query))
  (declare (ignore backend query))
  (error 'memory-storage-unsupported-error :operation :lexical-search
         :detail "backend does not implement lexical retrieval"))

(defmethod memory-storage-bind-projection
    ((backend memory-storage-backend) &key &allow-other-keys)
  (declare (ignore backend))
  (error 'memory-storage-unsupported-error :operation :bind-projection
         :detail "backend does not implement projection binding"))

(defmethod memory-storage-apply-mutation
    ((backend memory-storage-backend) mutation)
  (declare (ignore backend mutation))
  (error 'memory-storage-unsupported-error :operation :apply-mutation
         :detail "backend does not implement memory mutation"))

(defmethod memory-storage-operation-node
    ((backend memory-storage-backend) node-id)
  (declare (ignore backend node-id))
  (error 'memory-storage-unsupported-error :operation :operation-node
         :detail "backend does not implement exact operation reads"))

(defmethod memory-storage-operation-edges
    ((backend memory-storage-backend) &key from-id to-id edge-type)
  (declare (ignore backend from-id to-id edge-type))
  (error 'memory-storage-unsupported-error :operation :operation-edges
         :detail "backend does not implement exact operation reads"))

(defmethod memory-storage-operation-next-edge-id
    ((backend memory-storage-backend))
  (declare (ignore backend))
  (error 'memory-storage-unsupported-error :operation :operation-next-edge-id
         :detail "backend does not implement edge identity allocation"))

(defmethod memory-storage-operation-vector-dimension
    ((backend memory-storage-backend))
  (declare (ignore backend))
  (error 'memory-storage-unsupported-error :operation :operation-vector-dimension
         :detail "backend does not expose a sealed vector dimension"))

(defmethod memory-storage-projection-report ((backend memory-storage-backend))
  (declare (ignore backend))
  (error 'memory-storage-unsupported-error :operation :projection-report
         :detail "backend does not implement projection reporting"))

(defvar *memory-mutation-coordinator-lock*
  (bt:make-lock "memory-mutation-coordinator-global"))

(defclass memory-mutation-coordinator ()
  ((projection :initarg :projection :reader %memory-coordinator-projection)
   (append-fn :initarg :append-fn :reader %memory-coordinator-append-fn)
   (map-tail-fn :initarg :map-tail-fn :reader %memory-coordinator-map-tail-fn)
   ;; All coordinator instances share the mutation lock. Runtime selection
   ;; must not be able to create two independently advancing writers.
   (lock :initform *memory-mutation-coordinator-lock*
         :reader %memory-coordinator-lock)))

(defclass coordinated-memory-storage (memory-storage-backend)
  ((coordinator :initarg :coordinator :reader %coordinated-memory-coordinator)))

(defun make-memory-mutation-coordinator
    (&key projection append-fn map-tail-fn)
  (unless (and (typep projection 'memory-storage-backend)
               (functionp append-fn) (functionp map-tail-fn))
    (error 'memory-storage-error :operation :make-mutation-coordinator
           :detail "projection and durable append/tail functions are required"))
  (make-instance 'memory-mutation-coordinator
                 :projection projection :append-fn append-fn
                 :map-tail-fn map-tail-fn))

(defun make-coordinated-memory-storage (coordinator)
  (unless (typep coordinator 'memory-mutation-coordinator)
    (error 'memory-storage-error :operation :make-coordinated-memory-storage
           :detail "memory mutation coordinator is required"))
  (make-instance 'coordinated-memory-storage :coordinator coordinator))

(defun %memory-mutation-reconcile-unlocked (coordinator)
  (let* ((projection (%memory-coordinator-projection coordinator))
         (report (memory-storage-projection-report projection))
         (after (gethash "through_storage_position" report))
         (last-position after)
         (applied 0))
    (unless (and (integerp after) (not (minusp after)))
      (error 'memory-storage-error :operation :reconcile-memory-mutations
             :detail "projection has no valid physical watermark"))
    (multiple-value-bind (complete-p authoritative-head)
        (funcall
         (%memory-coordinator-map-tail-fn coordinator) after
         (lambda (mutation)
           (let ((position (and (hash-table-p mutation)
                                (gethash "storage_position" mutation))))
             (unless (and (integerp position) (> position last-position))
               (error 'memory-storage-error
                      :operation :reconcile-memory-mutations
                      :detail "authoritative tail is not in strict physical order"))
             (memory-storage-apply-mutation projection mutation)
             (setf last-position position)
             (incf applied))))
      (unless (and complete-p (integerp authoritative-head)
                   (= authoritative-head last-position))
        (error 'memory-storage-error :operation :reconcile-memory-mutations
               :detail "authoritative tail did not close at its declared head")))
    (%memory-storage-object "schema_version" 1 "status" "reconciled"
                            "applied_count" applied
                            "through_storage_position" last-position)))

(defun memory-mutation-coordinator-reconcile (coordinator)
  (bt:with-lock-held ((%memory-coordinator-lock coordinator))
    (%memory-mutation-reconcile-unlocked coordinator)))

(defun memory-mutation-coordinator-commit (coordinator event-type payload)
  (bt:with-lock-held ((%memory-coordinator-lock coordinator))
    ;; Never append a second command while a durable first command is missing
    ;; from the projection.
    (%memory-mutation-reconcile-unlocked coordinator)
    (let ((mutation
            (funcall (%memory-coordinator-append-fn coordinator)
                     event-type payload)))
      (unless (hash-table-p mutation)
        (error 'memory-storage-error :operation :required-memory-append
               :detail "durable append returned no verified mutation receipt"))
      ;; A projection failure deliberately escapes. The exact durable receipt
      ;; remains in the tail and is reconciled before any later operation.
      (memory-storage-apply-mutation
       (%memory-coordinator-projection coordinator) mutation))))

(defun %memory-operation-return-value (contract value)
  (%memory-storage-required-string contract "return-contract" :maximum 32)
  (cond
    ((and (string= contract "node-id") (stringp value) (plusp (length value)))
     value)
    ((and (string= contract "true") (eq value t)) t)
    ((and (string= contract "count") (integerp value) (not (minusp value)))
     value)
    ((and (string= contract "node-id-list") (listp value)
          (every (lambda (id) (and (stringp id) (plusp (length id)))) value))
     (copy-list value))
    ((and (string= contract "use-report") (hash-table-p value)
          (%memory-storage-exact-object-keys-p
           value '("consumer" "generation_id" "user_visible"
                   "requested_count" "updated_count"))
          (stringp (gethash "consumer" value))
          (or (%memory-materializer-json-null-p
               (gethash "generation_id" value))
              (stringp (gethash "generation_id" value)))
          (member (gethash "user_visible" value) '(nil t))
          (every (lambda (field)
                   (let ((count (gethash field value)))
                     (and (integerp count) (not (minusp count)))))
                 '("requested_count" "updated_count")))
     (%memory-storage-json-read (%memory-materializer-json value)
                                :memory-operation-return))
    (t
     (error 'memory-storage-error :operation :memory-operation-return
            :detail "return value does not match a closed cognitive contract"))))

(defun memory-mutation-coordinator-commit-operation
    (coordinator operation-kind commands return-contract return-value)
  "Commit one closed multi-row operation, then reproduce its cognitive return.
The return value is validated before append but is observable only after the
durable event and its atomic projection application both succeed."
  (let ((validated-return
          (%memory-operation-return-value return-contract return-value))
        (payload (make-memory-operation-payload operation-kind commands)))
    (memory-mutation-coordinator-commit
     coordinator "memory-operation-state" payload)
    validated-return))

(defun memory-mutation-coordinator-build-operation
    (coordinator builder &key (empty-contract nil empty-contract-p)
                              (empty-value nil empty-value-p))
  "Reconcile, build from the current projection, append, and apply under one
global lock. BUILDER receives the projection and must return a closed plan
created by one of MEMORY-ASSEMBLE-*-OPERATION."
  (unless (functionp builder)
    (error 'memory-storage-error :operation :build-memory-operation
           :detail "operation builder is required"))
  (unless (eq empty-contract-p empty-value-p)
    (error 'memory-storage-error :operation :build-memory-operation
           :detail "empty operation contract and value must be supplied together"))
  (bt:with-lock-held ((%memory-coordinator-lock coordinator))
    (%memory-mutation-reconcile-unlocked coordinator)
    (let* ((projection (%memory-coordinator-projection coordinator))
           (plan (funcall builder projection)))
      (when (null plan)
        (unless empty-contract-p
          (error 'memory-storage-error :operation :build-memory-operation
                 :detail "builder returned no operation"))
        (return-from memory-mutation-coordinator-build-operation
          (%memory-operation-return-value empty-contract empty-value)))
      (unless (and (hash-table-p plan)
                   (%memory-storage-exact-object-keys-p
                    plan '("schema_version" "operation_kind" "commands"
                           "return_contract" "return_value"))
                   (eql 1 (gethash "schema_version" plan)))
        (error 'memory-storage-error :operation :build-memory-operation
               :detail "builder did not return a closed schema-v1 plan"))
      (let* ((kind (gethash "operation_kind" plan))
             (commands (gethash "commands" plan))
             (contract (gethash "return_contract" plan))
             (result (%memory-operation-return-value
                      contract (gethash "return_value" plan)))
             (payload (make-memory-operation-payload kind commands))
             (mutation
               (funcall (%memory-coordinator-append-fn coordinator)
                        "memory-operation-state" payload)))
        (unless (hash-table-p mutation)
          (error 'memory-storage-error :operation :required-memory-append
                 :detail "durable append returned no verified mutation receipt"))
        (memory-storage-apply-mutation projection mutation)
        result))))

(defun memory-mutation-coordinator-read (coordinator thunk)
  (unless (functionp thunk)
    (error 'memory-storage-error :operation :coordinated-memory-read
           :detail "read thunk is required"))
  (bt:with-lock-held ((%memory-coordinator-lock coordinator))
    (%memory-mutation-reconcile-unlocked coordinator)
    (funcall thunk)))

(defun %coordinated-memory-read (backend thunk)
  (let ((coordinator (%coordinated-memory-coordinator backend)))
    (memory-mutation-coordinator-read coordinator thunk)))

(defmethod memory-storage-capabilities ((backend coordinated-memory-storage))
  (let* ((source
           (memory-storage-capabilities
            (%memory-coordinator-projection
             (%coordinated-memory-coordinator backend))))
         (result (%memory-storage-json-read
                  (%memory-materializer-json source)
                  :coordinated-memory-capabilities)))
    (setf (gethash "backend" result) "coordinated-memory"
          (gethash "authority_role" result) "runtime-read-projection"
          (gethash "runtime_reads" result) t
          (gethash "runtime_writes" result) nil)
    result))

(defmethod memory-storage-exact-search
    ((backend coordinated-memory-storage) query)
  (%coordinated-memory-read
   backend
   (lambda ()
     (memory-storage-exact-search
      (%memory-coordinator-projection
       (%coordinated-memory-coordinator backend)) query))))

(defmethod memory-storage-lexical-search
    ((backend coordinated-memory-storage) query)
  (%coordinated-memory-read
   backend
   (lambda ()
     (memory-storage-lexical-search
      (%memory-coordinator-projection
       (%coordinated-memory-coordinator backend)) query))))

(defmethod memory-storage-operation-node
    ((backend coordinated-memory-storage) node-id)
  (%coordinated-memory-read
   backend
   (lambda ()
     (memory-storage-operation-node
      (%memory-coordinator-projection
       (%coordinated-memory-coordinator backend)) node-id))))

(defmethod memory-storage-operation-edges
    ((backend coordinated-memory-storage) &key from-id to-id edge-type)
  (%coordinated-memory-read
   backend
   (lambda ()
     (memory-storage-operation-edges
      (%memory-coordinator-projection
       (%coordinated-memory-coordinator backend))
      :from-id from-id :to-id to-id :edge-type edge-type))))

(defmethod memory-storage-operation-next-edge-id
    ((backend coordinated-memory-storage))
  (%coordinated-memory-read
   backend
   (lambda ()
     (memory-storage-operation-next-edge-id
      (%memory-coordinator-projection
       (%coordinated-memory-coordinator backend))))))

(defmethod memory-storage-operation-vector-dimension
    ((backend coordinated-memory-storage))
  (%coordinated-memory-read
   backend
   (lambda ()
     (memory-storage-operation-vector-dimension
      (%memory-coordinator-projection
       (%coordinated-memory-coordinator backend))))))

(defun make-storage-memory-mutation-coordinator
    (&key event-storage projection (agent-id "default"))
  (%memory-storage-required-string agent-id "agent-id" :maximum 128)
  (unless (typep event-storage 'storage-backend)
    (error 'memory-storage-error :operation :make-storage-coordinator
           :detail "event storage backend is required"))
  (make-memory-mutation-coordinator
   :projection projection
   :append-fn
   (lambda (event-type payload)
     (multiple-value-bind (event receipt)
         (storage-append-event event-storage event-type payload
                               :agent-id agent-id)
       (declare (ignore event))
       (make-memory-storage-mutation-from-receipt receipt)))
   :map-tail-fn
   (lambda (after visitor)
     (multiple-value-bind (complete-p head count)
         (storage-map-event-receipts
          event-storage
          (lambda (receipt)
            (funcall visitor
                     (make-memory-storage-mutation-from-receipt receipt)))
          :agent-id agent-id :after-position after
          :event-types '("memory-node-state" "memory-edge-state"
                         "memory-operation-state"))
       (declare (ignore count))
       (values complete-p head)))))
