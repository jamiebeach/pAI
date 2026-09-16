;;;; memory-ledger-baseline.lisp -- hash-closed, streaming memory baselines.
;;;;
;;;; The event ledger is sacred; derived SQLite is not.  These records make a
;;;; sealed pre-cutover memory import rebuildable without materializing the
;;;; corpus or the ledger in the Lisp heap.

(in-package :agent)

(export '(memory-ledger-write-baseline memory-ledger-find-baseline
          memory-ledger-rebuild-baseline memory-ledger-install-authority))

(defvar *memory-ledger-authority-installed-p* nil)

(define-condition memory-ledger-baseline-unavailable (storage-integrity-error) ())

(defparameter *memory-ledger-baseline-event-types*
  '("memory-baseline-started" "memory-baseline-node"
    "memory-baseline-edge" "memory-baseline-committed"))

(defparameter *memory-ledger-baseline-receipt-keys*
  '("node_count" "edge_count" "node_sha256" "vector_sha256"
    "edge_sha256" "vector_binary_encoding" "provenance_seal_hash"))

(defclass memory-ledger-empty-snapshot (memory-storage-backend) ())

(defmethod memory-storage-capabilities ((source memory-ledger-empty-snapshot))
  (declare (ignore source))
  (%memory-storage-object
   "schema_version" 1 "backend" "memory-ledger-empty-snapshot"
   "read_snapshot" t "node_snapshot" t "edge_snapshot" t
   "exact_vector_export" t))

(defmethod memory-storage-map-snapshot
    ((source memory-ledger-empty-snapshot) node-visitor edge-visitor)
  "Return the canonical empty snapshot used only for explicit genesis.
The SHA-256 of each empty stream is fixed; no model or external store is read."
  (declare (ignore source node-visitor edge-visitor))
  (%memory-storage-object
   "schema_version" 1 "node_count" 0 "edge_count" 0
   "node_sha256" "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
   "vector_sha256" "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
   "edge_sha256" "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
   "vector_binary_encoding" "pgvector-send-v1"))

(defun %memory-ledger-baseline-source-origin (origin)
  (unless (and (hash-table-p origin)
               (%memory-storage-exact-object-keys-p
                origin '("schema_version" "source_agent_id" "source_kind"
                         "migration_manifest_sha256"))
               (eql 1 (gethash "schema_version" origin))
               (stringp (gethash "source_agent_id" origin))
               (plusp (length (gethash "source_agent_id" origin)))
               (<= (length (gethash "source_agent_id" origin)) 128)
               (every (lambda (character)
                        (or (alphanumericp character)
                            (find character "_.-" :test #'char=)))
                      (gethash "source_agent_id" origin))
               (string= "migrated-semantic-memory"
                        (gethash "source_kind" origin ""))
               (let ((digest (gethash "migration_manifest_sha256" origin)))
                 (and (stringp digest) (= 64 (length digest))
                      (string= digest (string-downcase digest))
                      (every (lambda (character)
                               (not (null (digit-char-p character 16))))
                             digest))))
    (error 'memory-storage-error :operation :memory-baseline
           :detail "baseline source origin does not match its closed schema"))
  origin)

(defun %memory-ledger-baseline-time (universal-time)
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time universal-time 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            year month day hour minute second)))

(defun %memory-ledger-baseline-receipt (receipt)
  (unless (hash-table-p receipt)
    (error 'memory-storage-error :operation :memory-baseline
           :detail "baseline receipt is absent"))
  (let ((result (%memory-storage-object)))
    (dolist (key *memory-ledger-baseline-receipt-keys*)
      (multiple-value-bind (value present-p) (gethash key receipt)
        (unless present-p
          (error 'memory-storage-error :operation :memory-baseline
                 :detail (format nil "baseline receipt lacks ~a" key)))
        (setf (gethash key result) value)))
    (unless (and (every (lambda (key)
                          (let ((value (gethash key result)))
                            (and (integerp value) (not (minusp value)))))
                        '("node_count" "edge_count"))
                 (every (lambda (key)
                          (let ((value (gethash key result)))
                            (and (stringp value) (= 64 (length value)))))
                        '("node_sha256" "vector_sha256" "edge_sha256"
                          "provenance_seal_hash"))
                 (string= "pgvector-send-v1"
                          (gethash "vector_binary_encoding" result)))
      (error 'memory-storage-error :operation :memory-baseline
             :detail "baseline receipt values are invalid"))
    result))

(defun %memory-ledger-baseline-provenance (provenance)
  (unless (and (hash-table-p provenance)
               (%memory-storage-exact-object-keys-p
                provenance
                '("schema_version" "embedding_model" "embedding_revision"
                  "retrieval_embedding_model" "retrieval_embedding_revision"
                  "vector_dimension" "revision_evidence" "approval_scope")))
    (error 'memory-storage-error :operation :memory-baseline
           :detail "baseline provenance does not match its closed schema"))
  (make-memory-embedding-provenance
   :embedding-model (gethash "embedding_model" provenance)
   :embedding-revision (gethash "embedding_revision" provenance)
   :retrieval-embedding-model (gethash "retrieval_embedding_model" provenance)
   :retrieval-embedding-revision
   (gethash "retrieval_embedding_revision" provenance)
   :vector-dimension (gethash "vector_dimension" provenance)
   :revision-evidence (gethash "revision_evidence" provenance)
   :approval-scope (gethash "approval_scope" provenance)))

(defun %memory-ledger-baseline-equal-receipt-p (left right)
  (every (lambda (key) (equal (gethash key left) (gethash key right)))
         *memory-ledger-baseline-receipt-keys*))

(defun %memory-ledger-baseline-append
    (event-storage agent-id type payload clock-fn)
  (nth-value
   1 (storage-append-event
      event-storage type payload :agent-id agent-id
      :occurred-at (%memory-ledger-baseline-time (funcall clock-fn)))))

(defun memory-ledger-write-baseline
    (event-storage source provenance
     &key (agent-id "default") session-id
       source-origin
       (clock-fn #'get-universal-time))
  "Append START, exact row records, and COMMIT for one sealed snapshot.
An interrupted or changed source deliberately leaves no commit authority."
  (unless (and (typep event-storage 'storage-backend)
               (typep source 'memory-storage-backend)
               (functionp clock-fn))
    (error 'memory-storage-error :operation :write-memory-baseline
           :detail "event storage, snapshot source and clock are required"))
  (%memory-storage-required-string agent-id "agent-id" :maximum 128)
  (let* ((provenance (%memory-ledger-baseline-provenance provenance))
         (source-origin (and source-origin
                             (%memory-ledger-baseline-source-origin
                              source-origin)))
         (audit (%memory-ledger-baseline-receipt
                 (memory-storage-audit-snapshot source)))
         (session (or session-id
                      (format nil "memory-baseline:~d:~4,'0x"
                              (get-universal-time) (random #x10000))))
         (expected-seal (%derived-seal-hash provenance audit)))
    (%memory-storage-required-string session "baseline-session-id" :maximum 256)
    (unless (string= expected-seal (gethash "provenance_seal_hash" audit))
      (error 'storage-integrity-error :operation :write-memory-baseline
             :detail "declared provenance does not match the sealed source"))
    (let* ((start-payload
             (if source-origin
                 (%memory-storage-object
                  "schema_version" 2 "session_id" session
                  "provenance" provenance "expected_receipt" audit
                  "source_origin" source-origin)
                 (%memory-storage-object
                  "schema_version" 1 "session_id" session
                  "provenance" provenance "expected_receipt" audit)))
           (start-receipt
             (%memory-ledger-baseline-append
              event-storage agent-id "memory-baseline-started"
              start-payload clock-fn))
           (node-ordinal 0) (edge-ordinal 0)
           (measured
             (memory-storage-map-snapshot
              source
              (lambda (node)
                (incf node-ordinal)
                (%memory-ledger-baseline-append
                 event-storage agent-id "memory-baseline-node"
                 (if source-origin
                     (%memory-storage-object
                      "schema_version" 2 "session_id" session
                      "ordinal" node-ordinal "node" node
                      "source_origin" source-origin)
                     (%memory-storage-object
                      "schema_version" 1 "session_id" session
                      "ordinal" node-ordinal "node" node))
                 clock-fn))
              (lambda (edge-row-json)
                (incf edge-ordinal)
                (%memory-ledger-baseline-append
                 event-storage agent-id "memory-baseline-edge"
                 (if source-origin
                     (%memory-storage-object
                      "schema_version" 2 "session_id" session
                      "ordinal" edge-ordinal "row_json" edge-row-json
                      "source_origin" source-origin)
                     (%memory-storage-object
                      "schema_version" 1 "session_id" session
                      "ordinal" edge-ordinal "row_json" edge-row-json))
                 clock-fn)))))
      (setf measured (%memory-ledger-baseline-receipt measured)
            (gethash "provenance_seal_hash" measured)
            (gethash "provenance_seal_hash" audit))
      (unless (and (= node-ordinal (gethash "node_count" audit))
                   (= edge-ordinal (gethash "edge_count" audit))
                   (%memory-ledger-baseline-equal-receipt-p audit measured))
        (error 'storage-integrity-error :operation :write-memory-baseline
               :detail "streamed baseline changed after its start receipt"))
      (let ((commit
              (%memory-ledger-baseline-append
               event-storage agent-id "memory-baseline-committed"
               (%memory-storage-object
                "schema_version" 1 "session_id" session
                "start_event_id" (gethash "event_id" start-receipt)
                "receipt" audit)
               clock-fn)))
        (%memory-storage-object
         "schema_version" 1 "status" "committed"
         "session_id" session
         "start_event_id" (gethash "event_id" start-receipt)
         "commit_event_id" (gethash "event_id" commit)
         "commit_storage_position" (gethash "storage_position" commit)
         "node_count" node-ordinal "edge_count" edge-ordinal
         "provenance_seal_hash" (gethash "provenance_seal_hash" audit))))))

(defclass memory-ledger-baseline-source (memory-storage-backend)
  ((event-storage :initarg :event-storage :reader %memory-baseline-events)
   (agent-id :initarg :agent-id :reader %memory-baseline-agent)
   (session-id :initarg :session-id :reader %memory-baseline-session)
   (start-position :initarg :start-position :reader %memory-baseline-start-position)
   (commit-receipt :initarg :commit-receipt :reader %memory-baseline-commit)
   (expected-receipt :initarg :expected-receipt
                     :reader %memory-baseline-expected)
   (provenance :initarg :provenance :reader %memory-baseline-provenance)
   (source-origin :initarg :source-origin :initform nil
                  :reader %memory-baseline-source-origin)))

(defmethod memory-storage-capabilities ((source memory-ledger-baseline-source))
  (declare (ignore source))
  (%memory-storage-object
   "schema_version" 1 "backend" "memory-ledger-baseline"
   "authority_role" "event-ledger-source" "read_snapshot" t
   "node_snapshot" t "edge_snapshot" t "exact_vector_export" t
   "runtime_reads" nil "runtime_writes" nil))

(defun %memory-ledger-baseline-event (receipt operation)
  (%memory-storage-json-read (gethash "event_json" receipt) operation))

(defun %memory-ledger-baseline-payload (receipt operation)
  (gethash "payload" (%memory-ledger-baseline-event receipt operation)))

(defun memory-ledger-find-baseline (event-storage &key (agent-id "default"))
  "Return the latest syntactically complete baseline source. Row hashes are
independently re-established when the source is mapped/imported."
  (let ((starts (make-hash-table :test #'equal))
        (selected nil))
    (storage-map-event-receipts
     event-storage
     (lambda (receipt)
       (let* ((type (gethash "event_type" receipt))
              (payload (%memory-ledger-baseline-payload
                        receipt :find-memory-baseline)))
         (cond
           ((string= type "memory-baseline-started")
            (unless (and (hash-table-p payload)
                         (let ((version (gethash "schema_version" payload)))
                           (and (member version '(1 2))
                                (%memory-storage-exact-object-keys-p
                                 payload
                                 (if (= version 2)
                                     '("schema_version" "session_id" "provenance"
                                       "expected_receipt" "source_origin")
                                     '("schema_version" "session_id" "provenance"
                                       "expected_receipt")))))
                         (stringp (gethash "session_id" payload))
                         (plusp (length (gethash "session_id" payload))))
              (error 'storage-integrity-error
                     :operation :find-memory-baseline
                     :detail "baseline start schema is invalid"))
            (let ((provenance
                    (%memory-ledger-baseline-provenance
                     (gethash "provenance" payload)))
                  (expected
                    (%memory-ledger-baseline-receipt
                     (gethash "expected_receipt" payload)))
                  (source-origin
                    (and (= 2 (gethash "schema_version" payload))
                         (%memory-ledger-baseline-source-origin
                          (gethash "source_origin" payload)))))
              (declare (ignore source-origin))
              (unless (string=
                       (%derived-seal-hash provenance expected)
                       (gethash "provenance_seal_hash" expected))
                (error 'storage-integrity-error
                       :operation :find-memory-baseline
                       :detail "baseline start provenance seal is invalid")))
            (setf (gethash (gethash "session_id" payload) starts)
                  (cons payload receipt)))
           ((string= type "memory-baseline-committed")
            (unless (and (hash-table-p payload)
                         (%memory-storage-exact-object-keys-p
                          payload '("schema_version" "session_id"
                                    "start_event_id" "receipt"))
                         (eql 1 (gethash "schema_version" payload))
                         (stringp (gethash "session_id" payload))
                         (and (integerp (gethash "start_event_id" payload))
                              (plusp (gethash "start_event_id" payload))))
              (error 'storage-integrity-error
                     :operation :find-memory-baseline
                     :detail "baseline commit schema is invalid"))
            (let* ((start (gethash (gethash "session_id" payload) starts))
                   (start-payload (car start)) (start-receipt (cdr start)))
              (when (and start
                         (= (gethash "start_event_id" payload)
                            (gethash "event_id" start-receipt))
                         (%memory-ledger-baseline-equal-receipt-p
                          (%memory-ledger-baseline-receipt
                           (gethash "expected_receipt" start-payload))
                          (%memory-ledger-baseline-receipt
                           (gethash "receipt" payload))))
                (setf selected
                      (make-instance
                       'memory-ledger-baseline-source
                       :event-storage event-storage :agent-id agent-id
                       :session-id (gethash "session_id" payload)
                       :start-position (gethash "storage_position" start-receipt)
                       :commit-receipt receipt
                       :expected-receipt
                       (%memory-ledger-baseline-receipt
                        (gethash "receipt" payload))
                       :provenance
                       (%memory-ledger-baseline-provenance
                        (gethash "provenance" start-payload))
                       :source-origin
                       (and (= 2 (gethash "schema_version" start-payload))
                            (%memory-ledger-baseline-source-origin
                             (gethash "source_origin" start-payload)))))))))))
     :agent-id agent-id :event-types *memory-ledger-baseline-event-types*)
    (unless selected
      (error 'memory-ledger-baseline-unavailable
             :operation :find-memory-baseline
             :detail "event ledger contains no complete memory baseline"))
    selected))

(defmethod memory-storage-map-snapshot
    ((source memory-ledger-baseline-source) node-visitor edge-visitor)
  (let ((node-digest (ironclad:make-digest :sha256))
        (vector-digest (ironclad:make-digest :sha256))
        (edge-digest (ironclad:make-digest :sha256))
        (node-count 0) (edge-count 0)
        (commit-position
          (gethash "storage_position" (%memory-baseline-commit source))))
    (storage-map-event-receipts
     (%memory-baseline-events source)
     (lambda (receipt)
       (when (<= (gethash "storage_position" receipt) commit-position)
         (let* ((type (gethash "event_type" receipt))
                (payload (%memory-ledger-baseline-payload
                          receipt :map-memory-baseline)))
           (when (and (hash-table-p payload)
                      (string= (gethash "session_id" payload "")
                               (%memory-baseline-session source)))
             (cond
               ((string= type "memory-baseline-node")
                (unless (and (member (gethash "schema_version" payload) '(1 2))
                             (integerp (gethash "ordinal" payload))
                             (= (gethash "ordinal" payload) (1+ node-count))
                             (%memory-storage-exact-object-keys-p
                              payload
                              (if (= 2 (gethash "schema_version" payload))
                                  '("schema_version" "session_id" "ordinal"
                                    "node" "source_origin")
                                  '("schema_version" "session_id"
                                    "ordinal" "node")))
                             (or (= 1 (gethash "schema_version" payload))
                                 (equalp (%memory-baseline-source-origin source)
                                         (%memory-ledger-baseline-source-origin
                                          (gethash "source_origin" payload)))))
                  (error 'storage-integrity-error
                         :operation :map-memory-baseline
                         :detail "baseline node sequence is invalid"))
                (let ((node (gethash "node" payload)))
                  (unless (and (hash-table-p node)
                               (%memory-storage-exact-object-keys-p
                                node '("scalar_json" "embedding_binary_hex"
                                       "retrieval_embedding_binary_hex")))
                    (error 'storage-integrity-error
                           :operation :map-memory-baseline
                           :detail "baseline node envelope is invalid"))
                  (%derived-digest-update node-digest
                                          (gethash "scalar_json" node))
                  (%derived-digest-update
                   vector-digest (gethash "embedding_binary_hex" node))
                  (%derived-digest-update
                   vector-digest
                   (gethash "retrieval_embedding_binary_hex" node))
                  (incf node-count)
                  (funcall node-visitor node)))
               ((string= type "memory-baseline-edge")
                (unless (and (member (gethash "schema_version" payload) '(1 2))
                             (integerp (gethash "ordinal" payload))
                             (= (gethash "ordinal" payload) (1+ edge-count))
                             (%memory-storage-exact-object-keys-p
                              payload
                              (if (= 2 (gethash "schema_version" payload))
                                  '("schema_version" "session_id" "ordinal"
                                    "row_json" "source_origin")
                                  '("schema_version" "session_id"
                                    "ordinal" "row_json")))
                             (or (= 1 (gethash "schema_version" payload))
                                 (equalp (%memory-baseline-source-origin source)
                                         (%memory-ledger-baseline-source-origin
                                          (gethash "source_origin" payload)))))
                  (error 'storage-integrity-error
                         :operation :map-memory-baseline
                         :detail "baseline edge sequence is invalid"))
                (let ((row-json (gethash "row_json" payload)))
                  (unless (stringp row-json)
                    (error 'storage-integrity-error
                           :operation :map-memory-baseline
                           :detail "baseline edge row is invalid"))
                  (%derived-digest-update edge-digest row-json)
                  (incf edge-count)
                  (funcall edge-visitor row-json))))))))
     :agent-id (%memory-baseline-agent source)
     :after-position (1- (%memory-baseline-start-position source))
     :event-types '("memory-baseline-node" "memory-baseline-edge"))
    (let ((receipt
            (%memory-storage-object
             "schema_version" 1 "backend" "memory-ledger-baseline"
             "node_count" node-count "edge_count" edge-count
             "node_sha256" (%derived-digest-hex node-digest)
             "vector_sha256" (%derived-digest-hex vector-digest)
             "edge_sha256" (%derived-digest-hex edge-digest)
             "vector_binary_encoding" "pgvector-send-v1"
             "provenance_seal_hash"
             (gethash "provenance_seal_hash"
                      (%memory-baseline-expected source)))))
      (unless (%memory-ledger-baseline-equal-receipt-p
               receipt (%memory-baseline-expected source))
        (error 'storage-integrity-error :operation :map-memory-baseline
               :detail "baseline rows do not match the committed receipt"))
      receipt)))

(defun memory-ledger-rebuild-baseline
    (event-storage destination &key (agent-id "default")
                                     (projector-revision "memory-ledger-v1"))
  "Import the latest complete baseline into an empty derived store and bind
its immutable boundary to the exact commit receipt. Tail reconciliation is a
separate required startup step."
  (let* ((source (memory-ledger-find-baseline
                  event-storage :agent-id agent-id))
         (commit (%memory-baseline-commit source))
         (report
           (memory-storage-import-snapshot
            destination source (%memory-baseline-provenance source))))
    (unless (%memory-ledger-baseline-equal-receipt-p
             report (%memory-baseline-expected source))
      (error 'storage-integrity-error :operation :rebuild-memory-baseline
             :detail "rebuilt baseline receipt differs from its commit"))
    (memory-storage-bind-projection
     destination
     :baseline-seal (gethash "provenance_seal_hash" report)
     :storage-id (gethash "storage_id" commit)
     :agent-id agent-id
     :through-event-id (gethash "event_id" commit)
     :through-position (gethash "storage_position" commit)
     :boundary-hash (gethash "integrity_hash" commit)
     :projector-revision projector-revision)
    (%memory-storage-object
     "schema_version" 1 "status" "rebuilt"
     "session_id" (%memory-baseline-session source)
     "through_event_id" (gethash "event_id" commit)
     "through_storage_position" (gethash "storage_position" commit)
     "node_count" (gethash "node_count" report)
     "edge_count" (gethash "edge_count" report)
     "provenance_seal_hash" (gethash "provenance_seal_hash" report))))

(defun %memory-ledger-baseline-boundary (source destination agent-id)
  (let* ((commit (%memory-baseline-commit source))
         (expected (%memory-baseline-expected source))
         (projection (memory-storage-projection-report destination)))
    (if (string= "unbound" (gethash "status" projection ""))
        (let ((audit (memory-storage-audit-snapshot destination)))
          (if (and (zerop (gethash "node_count" audit))
                   (zerop (gethash "edge_count" audit))
                   (%memory-materializer-json-null-p
                    (gethash "provenance_seal_hash" audit)))
              (memory-ledger-rebuild-baseline
               (%memory-baseline-events source) destination
               :agent-id agent-id)
              (progn
                (unless (%memory-ledger-baseline-equal-receipt-p audit expected)
                  (error 'storage-integrity-error
                         :operation :install-memory-authority
                         :detail "unbound derived import differs from ledger baseline"))
                ;; Mapping independently recomputes every ledger row hash.
                (memory-storage-map-snapshot source (lambda (node) (declare (ignore node)))
                                                    (lambda (edge) (declare (ignore edge))))
                (memory-storage-bind-projection
                 destination
                 :baseline-seal (gethash "provenance_seal_hash" expected)
                 :storage-id (gethash "storage_id" commit)
                 :agent-id agent-id
                 :through-event-id (gethash "event_id" commit)
                 :through-position (gethash "storage_position" commit)
                 :boundary-hash (gethash "integrity_hash" commit)
                 :projector-revision "memory-ledger-v1"))))
        ;; Re-present immutable baseline identity. Advancing watermarks are
        ;; intentionally not supplied by the caller and cannot self-certify.
        (memory-storage-bind-projection
         destination
         :baseline-seal (gethash "provenance_seal_hash" expected)
         :storage-id (gethash "storage_id" commit)
         :agent-id agent-id
         :through-event-id (gethash "event_id" commit)
         :through-position (gethash "storage_position" commit)
         :boundary-hash (gethash "integrity_hash" commit)
         :projector-revision "memory-ledger-v1"))))

(defun memory-ledger-install-authority
    (event-storage destination
     &key (agent-id "default") migrate-p initialize-p initial-provenance
       source-origin
       (clock-fn #'get-universal-time) (postgres-writer-active-p t))
  "Install SQLite memory reads and writes once, only after baseline proof and
tail reconciliation. MIGRATE-P imports an existing derived snapshot.
INITIALIZE-P seals the canonical empty snapshot for an explicit new instance."
  (when *memory-ledger-authority-installed-p*
    (error 'storage-conflict-error :operation :install-memory-authority
           :detail "memory authority is already installed for this process"))
  (when postgres-writer-active-p
    (error 'storage-conflict-error :operation :install-memory-authority
           :detail "PostgreSQL writer absence was not explicitly established"))
  (when (and migrate-p initialize-p)
    (error 'storage-conflict-error :operation :install-memory-authority
           :detail "memory migration and empty initialization are mutually exclusive"))
  (let ((event-capabilities (storage-capabilities event-storage))
        (memory-capabilities (memory-storage-capabilities destination)))
    (unless (and (eq t (gethash "event_log" event-capabilities))
                 (eq t (gethash "single_authority" event-capabilities))
                 (eq t (gethash "exact_retrieval" memory-capabilities))
                 (eq t (gethash "mutation_projection" memory-capabilities)))
      (error 'memory-storage-error :operation :install-memory-authority
             :detail "required SQLite event/memory capabilities are absent")))
  (when migrate-p
    (handler-case
        (progn
          (memory-ledger-find-baseline event-storage :agent-id agent-id)
          (error 'storage-conflict-error :operation :install-memory-authority
                 :detail "memory ledger baseline already exists; omit migration"))
      (memory-ledger-baseline-unavailable ()
        (memory-ledger-write-baseline
         event-storage destination
         (sqlite-derived-memory-provenance destination)
         :agent-id agent-id :source-origin source-origin
         :clock-fn clock-fn))))
  (when initialize-p
    (handler-case
        (progn
          (memory-ledger-find-baseline event-storage :agent-id agent-id)
          (error 'storage-conflict-error :operation :install-memory-authority
                 :detail "memory ledger baseline already exists; omit initialization"))
      (memory-ledger-baseline-unavailable ()
        (unless (hash-table-p initial-provenance)
          (error 'memory-storage-error :operation :install-memory-authority
                 :detail "empty initialization requires explicit memory provenance"))
        (memory-storage-import-snapshot
         destination (make-instance 'memory-ledger-empty-snapshot)
         initial-provenance)
        (memory-ledger-write-baseline
         event-storage destination initial-provenance
         :agent-id agent-id :session-id "memory-baseline:empty-genesis:v1"
         :clock-fn clock-fn))))
  (let* ((source (memory-ledger-find-baseline
                  event-storage :agent-id agent-id))
         (boundary (%memory-ledger-baseline-boundary
                    source destination agent-id))
         (coordinator
           (make-storage-memory-mutation-coordinator
            :event-storage event-storage :projection destination
            :agent-id agent-id))
         (reconciliation
           (memory-mutation-coordinator-reconcile coordinator)))
    (declare (ignore boundary))
    ;; The globals change only after every proof above has succeeded. There is
    ;; no fallback path after this point and no mode for dual/shadow writes.
    (setf *memory-search-storage-backend*
          (make-coordinated-memory-storage coordinator)
          *memory-cognitive-mutation-router*
          (make-sqlite-memory-cognitive-router coordinator)
          *memory-cognitive-mutation-mode* :event-first
          *memory-ledger-authority-installed-p* t)
    (%memory-storage-object
     "schema_version" 1 "status" "installed"
     "authority" "sqlite-event-first"
     "session_id" (%memory-baseline-session source)
     "baseline_storage_position"
     (gethash "storage_position" (%memory-baseline-commit source))
     "through_storage_position"
     (gethash "through_storage_position" reconciliation)
     "reconciled_event_count" (gethash "applied_count" reconciliation))))
