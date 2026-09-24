;;;; sqlite-derived-storage.lisp -- rebuildable checkpoints and memory data.
;;;;
;;;; This database is deliberately separate from events.sqlite3. Construction
;;;; opens only the named derived database and installs no runtime authority.

(in-package :agent)

(export '(sqlite-derived-storage make-sqlite-derived-storage
          sqlite-derived-storage-path make-memory-embedding-provenance
          sqlite-derived-memory-provenance))

(defclass sqlite-derived-storage (storage-backend memory-storage-backend)
  ((path :initarg :path :reader sqlite-derived-storage-path)
   (handle :initarg :handle :accessor %sqlite-derived-handle-slot)
   (lock :initform (bt:make-lock "sqlite-derived-storage")
         :reader %sqlite-derived-lock)
   (verified-memory-seal :initform nil
                         :accessor %sqlite-derived-verified-memory-seal)
   (verified-memory-data-version
    :initform nil :accessor %sqlite-derived-verified-memory-data-version)
   (closed-p :initform nil :accessor %sqlite-derived-closed-p)))

(defun %sqlite-derived-handle (backend operation)
  (when (%sqlite-derived-closed-p backend)
    (error 'storage-error :operation operation :detail "backend is closed"))
  (%sqlite-derived-handle-slot backend))

(defun %sqlite-derived-in-transaction (backend operation thunk)
  (let ((handle (%sqlite-derived-handle backend operation)))
    (%sqlite-exec handle "BEGIN IMMEDIATE" operation)
    (handler-case
        (multiple-value-prog1 (funcall thunk handle)
          (%sqlite-exec handle "COMMIT" operation))
      (error (condition)
        (ignore-errors (%sqlite-exec handle "ROLLBACK" operation))
        (error condition)))))

(defun %sqlite-derived-data-version (handle operation)
  (%with-sqlite-statement (statement handle "PRAGMA data_version" operation)
    (%sqlite-step handle statement operation +sqlite-row+)
    (%sqlite-column-int64 statement 0)))

(defun %sqlite-derived-current-memory-seal (handle operation)
  (%with-sqlite-statement
      (statement handle
                 "SELECT seal_hash FROM pai_memory_imports WHERE import_name='canonical'"
                 operation)
    (let ((code (%sqlite-step-raw statement)))
      (cond ((= code +sqlite-row+) (%sqlite-column-text statement 0))
            ((= code +sqlite-done+) nil)
            (t (%sqlite-check code handle operation))))))

(defun %sqlite-memory-word-character-p (character)
  (or (alphanumericp character) (char= character #\_)))

(defun %sqlite-memory-token-match-p (content token)
  (loop with start = 0
        for position = (search token content :start2 start :test #'char-equal)
        while position
        for end = (+ position (length token))
        when (and (or (zerop position)
                      (not (%sqlite-memory-word-character-p
                            (aref content (1- position)))))
                  (or (= end (length content))
                      (not (%sqlite-memory-word-character-p
                            (aref content end)))))
          return t
        do (setf start (1+ position))
        finally (return nil)))

(defun %sqlite-memory-phrase-match-p (content phrase)
  ;; The producer normalizes phrase words to one space. Incumbent PostgreSQL
  ;; accepts one-or-more whitespace characters between them, with word bounds.
  (let ((words (uiop:split-string phrase :separator '(#\Space))))
    (loop with first = (first words) with start = 0
          for position = (search first content :start2 start :test #'char-equal)
          while position
          do (let ((cursor (+ position (length first))) (matched t))
               (unless (or (zerop position)
                           (not (%sqlite-memory-word-character-p
                                 (aref content (1- position)))))
                 (setf matched nil))
               (dolist (word (rest words))
                 (let ((space-start cursor))
                   (loop while (and (< cursor (length content))
                                    (find (aref content cursor)
                                          '(#\Space #\Tab #\Newline #\Return)))
                         do (incf cursor))
                   (unless (and matched (> cursor space-start)
                                (<= (+ cursor (length word)) (length content))
                                (string-equal word content :start2 cursor
                                              :end2 (+ cursor (length word))))
                     (setf matched nil))
                   (when matched (incf cursor (length word)))))
               (when (and matched
                          (or (= cursor (length content))
                              (not (%sqlite-memory-word-character-p
                                    (aref content cursor)))))
                 (return-from %sqlite-memory-phrase-match-p t)))
             (setf start (1+ position))
          finally (return nil))))

(defun %sqlite-memory-lexeme-match-p (content lexeme)
  (if (string= "phrase" (gethash "kind" lexeme))
      (%sqlite-memory-phrase-match-p content (gethash "text" lexeme))
      (%sqlite-memory-token-match-p content (gethash "text" lexeme))))

(declaim (ftype (function (t) t) %derived-audit-unlocked)
         (ftype (function (t t) t) %derived-verify-import-unlocked)
         (ftype (function (t t t) t) %derived-row-integrity)
         (ftype (function (t) t) %sqlite-memory-projection-unlocked))

(defun %sqlite-derived-ensure-memory-verified (backend handle operation)
  (let* ((previous-data-version
           (%sqlite-derived-verified-memory-data-version backend))
         (data-version (%sqlite-derived-data-version handle operation))
         (current-seal (%sqlite-derived-current-memory-seal handle operation))
         (verified-p
           (and (stringp current-seal)
                (string= current-seal
                         (or (%sqlite-derived-verified-memory-seal backend) ""))
                (eql data-version
                     (%sqlite-derived-verified-memory-data-version backend)))))
    (unless verified-p
      (let* ((projection (%sqlite-memory-projection-unlocked handle))
             (projection-seal (and projection
                                   (gethash "baseline_seal" projection))))
        (if projection
            (progn
              (unless (and (stringp current-seal) (stringp projection-seal)
                           (string= current-seal projection-seal))
                (error 'storage-integrity-error :operation operation
                       :detail "mutable projection baseline seal mismatch"))
              ;; A changed PRAGMA data_version means another connection wrote
              ;; outside this backend's controlled mutation path. Audit once
              ;; before trusting the new generation. Initial open and explicit
              ;; same-connection invalidation retain the projection receipt and
              ;; avoid turning every restart/read into a full-table audit.
              (when (and previous-data-version
                         (not (eql data-version previous-data-version)))
                (let ((audit (%derived-audit-unlocked handle)))
                  (%derived-verify-import-unlocked handle audit))))
            (let* ((audit (%derived-audit-unlocked handle))
                   (verified-seal (%derived-verify-import-unlocked handle audit)))
              (unless verified-seal
                (error 'storage-integrity-error :operation operation
                       :detail "memory retrieval requires a sealed import"))))
        ;; Mutable generations are authorized by the bound projection receipt;
        ;; each scanned row is verified below. The cached seal remains the
        ;; immutable baseline identity.
        (setf (%sqlite-derived-verified-memory-seal backend) current-seal
              (%sqlite-derived-verified-memory-data-version backend)
              data-version)))
    t))

(defun %sqlite-derived-copy-json-value (value)
  (cond
    ((hash-table-p value)
     (let ((copy (make-hash-table :test #'equal)))
       (maphash (lambda (key item)
                  (setf (gethash key copy)
                        (%sqlite-derived-copy-json-value item)))
                value)
       copy))
    ((and (vectorp value) (not (stringp value)))
     (map 'vector #'%sqlite-derived-copy-json-value value))
    ((listp value) (mapcar #'%sqlite-derived-copy-json-value value))
    (t value)))

(defparameter *sqlite-derived-base-schema-sql*
  "CREATE TABLE IF NOT EXISTS pai_projection_checkpoints (projection_name TEXT NOT NULL, agent_id TEXT NOT NULL, through_event_id INTEGER NOT NULL, through_storage_position INTEGER NOT NULL, projector_revision TEXT NOT NULL, policy_revision TEXT NOT NULL, state_json TEXT NOT NULL, integrity_hash TEXT NOT NULL, created_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP, PRIMARY KEY(projection_name,agent_id)); CREATE TABLE IF NOT EXISTS pai_memory_nodes (id TEXT PRIMARY KEY, source_ordinal INTEGER NOT NULL UNIQUE, scalar_json TEXT NOT NULL, embedding BLOB NOT NULL, retrieval_embedding BLOB NOT NULL, integrity_hash TEXT NOT NULL); CREATE TABLE IF NOT EXISTS pai_memory_edges (id INTEGER PRIMARY KEY, source_ordinal INTEGER NOT NULL UNIQUE, from_id TEXT NOT NULL, to_id TEXT NOT NULL, edge_type TEXT NOT NULL, row_json TEXT NOT NULL, integrity_hash TEXT NOT NULL, UNIQUE(from_id,to_id,edge_type), FOREIGN KEY(from_id) REFERENCES pai_memory_nodes(id), FOREIGN KEY(to_id) REFERENCES pai_memory_nodes(id)); CREATE INDEX IF NOT EXISTS pai_memory_edges_from_idx ON pai_memory_edges(from_id); CREATE INDEX IF NOT EXISTS pai_memory_edges_to_idx ON pai_memory_edges(to_id); CREATE TABLE IF NOT EXISTS pai_memory_imports (import_name TEXT PRIMARY KEY CHECK(import_name='canonical'), schema_version INTEGER NOT NULL, embedding_model TEXT NOT NULL, embedding_revision TEXT NOT NULL, retrieval_embedding_model TEXT NOT NULL, retrieval_embedding_revision TEXT NOT NULL, vector_dimension INTEGER NOT NULL, revision_evidence TEXT NOT NULL, approval_scope TEXT NOT NULL, node_count INTEGER NOT NULL, edge_count INTEGER NOT NULL, node_sha256 TEXT NOT NULL, vector_sha256 TEXT NOT NULL, edge_sha256 TEXT NOT NULL, vector_binary_encoding TEXT NOT NULL, seal_hash TEXT NOT NULL, created_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP); CREATE TABLE IF NOT EXISTS pai_memory_projection (projection_name TEXT PRIMARY KEY CHECK(projection_name='canonical'), baseline_seal TEXT NOT NULL, storage_id TEXT NOT NULL, agent_id TEXT NOT NULL, through_event_id INTEGER NOT NULL, through_storage_position INTEGER NOT NULL, boundary_hash TEXT NOT NULL, projector_revision TEXT NOT NULL); CREATE TABLE IF NOT EXISTS pai_memory_projection_binding (projection_name TEXT PRIMARY KEY CHECK(projection_name='canonical'), baseline_event_id INTEGER NOT NULL, baseline_storage_position INTEGER NOT NULL, baseline_boundary_hash TEXT NOT NULL, FOREIGN KEY(projection_name) REFERENCES pai_memory_projection(projection_name)); CREATE TABLE IF NOT EXISTS pai_memory_applied_events (event_id INTEGER PRIMARY KEY, storage_position INTEGER NOT NULL UNIQUE, mutation_hash TEXT NOT NULL, event_hash TEXT NOT NULL)")

(defparameter *sqlite-derived-graph-schema-sql*
  "CREATE TABLE IF NOT EXISTS pai_knowledge_graph_nodes (projection_name TEXT NOT NULL, agent_id TEXT NOT NULL, persona_id TEXT NOT NULL, node_id TEXT NOT NULL, node_kind TEXT NOT NULL, canonical_key TEXT NOT NULL, payload_json TEXT NOT NULL, integrity_hash TEXT NOT NULL, PRIMARY KEY(projection_name,agent_id,persona_id,node_id), UNIQUE(projection_name,agent_id,persona_id,node_kind,canonical_key)); CREATE TABLE IF NOT EXISTS pai_knowledge_graph_edges (projection_name TEXT NOT NULL, agent_id TEXT NOT NULL, persona_id TEXT NOT NULL, edge_id TEXT NOT NULL, from_node_id TEXT NOT NULL, predicate TEXT NOT NULL, to_node_id TEXT NOT NULL, payload_json TEXT NOT NULL, integrity_hash TEXT NOT NULL, PRIMARY KEY(projection_name,agent_id,persona_id,edge_id), UNIQUE(projection_name,agent_id,persona_id,from_node_id,predicate,to_node_id), FOREIGN KEY(projection_name,agent_id,persona_id,from_node_id) REFERENCES pai_knowledge_graph_nodes(projection_name,agent_id,persona_id,node_id), FOREIGN KEY(projection_name,agent_id,persona_id,to_node_id) REFERENCES pai_knowledge_graph_nodes(projection_name,agent_id,persona_id,node_id)); CREATE INDEX IF NOT EXISTS pai_knowledge_graph_edges_from_idx ON pai_knowledge_graph_edges(projection_name,agent_id,persona_id,from_node_id,predicate); CREATE INDEX IF NOT EXISTS pai_knowledge_graph_edges_to_idx ON pai_knowledge_graph_edges(projection_name,agent_id,persona_id,to_node_id,predicate); CREATE TABLE IF NOT EXISTS pai_knowledge_graph_evidence (projection_name TEXT NOT NULL, agent_id TEXT NOT NULL, persona_id TEXT NOT NULL, owner_kind TEXT NOT NULL CHECK(owner_kind IN ('node','edge')), owner_id TEXT NOT NULL, evidence_event_id INTEGER NOT NULL, evidence_role TEXT NOT NULL CHECK(evidence_role IN ('descriptor','source')), evidence_ordinal INTEGER NOT NULL CHECK(evidence_ordinal >= 0), PRIMARY KEY(projection_name,agent_id,persona_id,owner_kind,owner_id,evidence_event_id,evidence_role)); CREATE INDEX IF NOT EXISTS pai_knowledge_graph_evidence_owner_idx ON pai_knowledge_graph_evidence(projection_name,agent_id,persona_id,owner_kind,owner_id,evidence_ordinal)")

(defparameter *sqlite-derived-reviewed-graph-schema-sql*
  "CREATE TABLE IF NOT EXISTS pai_reviewed_graph_records (projection_name TEXT NOT NULL, agent_id TEXT NOT NULL, persona_id TEXT NOT NULL, record_kind TEXT NOT NULL, record_key TEXT NOT NULL, key_kind TEXT NOT NULL CHECK(key_kind IN ('scalar','list','singleton')), payload_json TEXT NOT NULL, integrity_hash TEXT NOT NULL, PRIMARY KEY(projection_name,agent_id,persona_id,record_kind,record_key)); CREATE INDEX IF NOT EXISTS pai_reviewed_graph_records_kind_idx ON pai_reviewed_graph_records(projection_name,agent_id,persona_id,record_kind,record_key); CREATE TABLE IF NOT EXISTS pai_reviewed_graph_aliases (projection_name TEXT NOT NULL, agent_id TEXT NOT NULL, persona_id TEXT NOT NULL, entity_id TEXT NOT NULL, alias_folded TEXT NOT NULL, PRIMARY KEY(projection_name,agent_id,persona_id,entity_id,alias_folded)); CREATE INDEX IF NOT EXISTS pai_reviewed_graph_alias_lookup_idx ON pai_reviewed_graph_aliases(projection_name,agent_id,persona_id,alias_folded,entity_id); CREATE TABLE IF NOT EXISTS pai_reviewed_graph_adjacency (projection_name TEXT NOT NULL, agent_id TEXT NOT NULL, persona_id TEXT NOT NULL, entity_id TEXT NOT NULL, fact_id TEXT NOT NULL, PRIMARY KEY(projection_name,agent_id,persona_id,entity_id,fact_id)); CREATE INDEX IF NOT EXISTS pai_reviewed_graph_adjacency_entity_idx ON pai_reviewed_graph_adjacency(projection_name,agent_id,persona_id,entity_id,fact_id)")

(defparameter *sqlite-derived-working-summary-schema-sql*
  "CREATE TABLE IF NOT EXISTS pai_working_context_summaries (agent_id TEXT NOT NULL, activity_id TEXT NOT NULL, policy_revision TEXT NOT NULL, model_revision TEXT NOT NULL, source_digest TEXT NOT NULL, source_event_ids_json TEXT NOT NULL, response_json TEXT NOT NULL, provenance_json TEXT NOT NULL, integrity_hash TEXT NOT NULL, created_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP, PRIMARY KEY(agent_id,activity_id,policy_revision,model_revision,source_digest))")

(defun %sqlite-derived-initialize-schema (handle)
  (%sqlite-exec handle "PRAGMA busy_timeout=5000" :derived-initialize)
  (%sqlite-exec handle "PRAGMA journal_mode=WAL" :derived-initialize)
  (%sqlite-exec handle "PRAGMA foreign_keys=ON" :derived-initialize)
  (%sqlite-exec handle "PRAGMA synchronous=FULL" :derived-initialize)
  (%sqlite-exec
   handle
   "CREATE TABLE IF NOT EXISTS pai_derived_meta (meta_key TEXT PRIMARY KEY, meta_value TEXT NOT NULL)"
   :derived-initialize)
  (%sqlite-exec handle "BEGIN IMMEDIATE" :derived-initialize)
  (handler-case
      (progn
        (let ((version nil))
          (%with-sqlite-statement
              (statement handle
                         "SELECT meta_value FROM pai_derived_meta WHERE meta_key='format_version'"
                         :derived-initialize)
            (let ((code (%sqlite-step-raw statement)))
              (cond ((= code +sqlite-row+)
                     (setf version (%sqlite-column-text statement 0)))
                    ((/= code +sqlite-done+)
                     (%sqlite-check code handle :derived-initialize)))))
          (unless (or (null version) (string= version "1")
                      (string= version "2") (string= version "3")
                      (string= version "4"))
            (error 'storage-conflict-error :operation :derived-initialize
                   :detail (format nil "unsupported derived SQLite format ~a"
                                   version)))
          ;; Version 2 is additive.  Reasserting the version-1 tables makes a
          ;; fresh database and an incumbent database take the same path; no
          ;; incumbent memory row is rewritten.
          (%sqlite-exec handle *sqlite-derived-base-schema-sql*
                        :derived-initialize)
          (%sqlite-exec handle *sqlite-derived-graph-schema-sql*
                        :derived-initialize)
          (%sqlite-exec handle *sqlite-derived-reviewed-graph-schema-sql*
                        :derived-initialize)
          (%sqlite-exec handle *sqlite-derived-working-summary-schema-sql*
                        :derived-initialize)
          (if version
              (%sqlite-exec
               handle
               "UPDATE pai_derived_meta SET meta_value='4' WHERE meta_key='format_version'"
               :derived-initialize)
              (%sqlite-exec
               handle
               "INSERT INTO pai_derived_meta(meta_key,meta_value) VALUES('format_version','4')"
               :derived-initialize)))
        (%sqlite-exec handle "COMMIT" :derived-initialize))
    (error (condition)
      (ignore-errors (%sqlite-exec handle "ROLLBACK" :derived-initialize))
      (error condition))))

(defun make-sqlite-derived-storage (path)
  (%sqlite-load-library)
  (let ((namestring (namestring (merge-pathnames path))))
    (ensure-directories-exist path)
    (cffi:with-foreign-object (handle-pointer :pointer)
      (setf (cffi:mem-ref handle-pointer :pointer) (cffi:null-pointer))
      (let* ((flags (logior +sqlite-open-readwrite+ +sqlite-open-create+
                            +sqlite-open-fullmutex+))
             (code (%sqlite-open-v2 namestring handle-pointer flags
                                    (cffi:null-pointer)))
             (handle (cffi:mem-ref handle-pointer :pointer)))
        (unless (= code +sqlite-ok+)
          (let ((message (if (cffi:null-pointer-p handle)
                             (format nil "SQLite open code ~d" code)
                             (%sqlite-error-text handle))))
            (unless (cffi:null-pointer-p handle) (%sqlite-close-v2 handle))
            (error 'storage-error :operation :derived-open :detail message)))
        (handler-case
            (progn
              (%sqlite-derived-initialize-schema handle)
              (make-instance 'sqlite-derived-storage
                             :path namestring :handle handle))
          (error (condition)
            (%sqlite-close-v2 handle)
            (error condition)))))))

(defmethod storage-capabilities ((backend sqlite-derived-storage))
  (declare (ignore backend))
  (%storage-object "schema_version" 1 "backend" "sqlite-derived"
                   "event_log" nil "projection_checkpoints" t
                   "memory_snapshot" t "working_context_summaries" t
                   "single_authority" nil))

(defun %sqlite-working-summary-integrity
    (agent-id activity-id policy-revision model-revision source-digest ids-json response-json provenance-json)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :sha256 (babel:string-to-octets
             (format nil "~a~%~a~%~a~%~a~%~a~%~a~%~a~%~a" agent-id activity-id
                     policy-revision model-revision source-digest ids-json response-json provenance-json)
             :encoding :utf-8))))

(defmethod storage-publish-working-context-summary
    ((backend sqlite-derived-storage) agent-id activity-id policy-revision model-revision
     source-digest source-event-ids response provenance)
  (let* ((ids-json (%storage-json source-event-ids)) (response-json (%storage-json response))
         (provenance-json (%storage-json provenance))
         (integrity (%sqlite-working-summary-integrity agent-id activity-id policy-revision
                                                       model-revision source-digest ids-json
                                                       response-json provenance-json)))
    (unless (and (<= (length ids-json) 65536) (<= (length response-json) 65536)
                 (<= (length provenance-json) 16384))
      (error 'storage-error :operation :working-summary-publish :detail "Working summary exceeds bounded columns"))
    (bt:with-lock-held ((%sqlite-derived-lock backend))
      (%sqlite-derived-in-transaction
       backend :working-summary-publish
       (lambda (handle)
         (%with-sqlite-statement
             (s handle "INSERT OR REPLACE INTO pai_working_context_summaries(agent_id,activity_id,policy_revision,model_revision,source_digest,source_event_ids_json,response_json,provenance_json,integrity_hash) VALUES(?1,?2,?3,?4,?5,?6,?7,?8,?9)" :working-summary-publish)
           (loop for value in (list agent-id activity-id policy-revision model-revision source-digest
                                    ids-json response-json provenance-json integrity)
                 for index from 1 do (%sqlite-bind-text handle s index value :working-summary-publish))
           (%sqlite-step handle s :working-summary-publish +sqlite-done+)))))
    t))

(defmethod storage-load-working-context-summary
    ((backend sqlite-derived-storage) agent-id activity-id policy-revision model-revision source-digest)
  (bt:with-lock-held ((%sqlite-derived-lock backend))
    (let ((handle (%sqlite-derived-handle backend :working-summary-load)))
      (%with-sqlite-statement
          (s handle "SELECT source_event_ids_json,response_json,provenance_json,integrity_hash FROM pai_working_context_summaries WHERE agent_id=?1 AND activity_id=?2 AND policy_revision=?3 AND model_revision=?4 AND source_digest=?5" :working-summary-load)
        (loop for value in (list agent-id activity-id policy-revision model-revision source-digest)
              for index from 1 do (%sqlite-bind-text handle s index value :working-summary-load))
        (let ((code (%sqlite-step-raw s)))
          (cond ((= code +sqlite-done+) nil)
                ((= code +sqlite-row+)
                 (when (or (> (%sqlite-column-bytes-raw s 0) 65536)
                           (> (%sqlite-column-bytes-raw s 1) 65536)
                           (> (%sqlite-column-bytes-raw s 2) 16384))
                   (error 'storage-integrity-error :operation :working-summary-load :detail "Oversized working summary"))
                 (let* ((ids-json (%sqlite-column-text s 0)) (response-json (%sqlite-column-text s 1))
                        (provenance-json (%sqlite-column-text s 2)) (stored (%sqlite-column-text s 3))
                        (expected (%sqlite-working-summary-integrity agent-id activity-id policy-revision
                                                                    model-revision source-digest ids-json
                                                                    response-json provenance-json)))
                   (unless (string-equal stored expected)
                     (error 'storage-integrity-error :operation :working-summary-load
                            :detail "Working summary integrity mismatch"))
                   (%storage-object "source_event_ids" (%storage-json-read ids-json :working-summary-load)
                                    "response" (%storage-json-read response-json :working-summary-load)
                                    "provenance" (%storage-json-read provenance-json :working-summary-load))))
                (t (%sqlite-check code handle :working-summary-load))))))))

(defmethod memory-storage-capabilities ((backend sqlite-derived-storage))
  (declare (ignore backend))
  (%memory-storage-object
   "schema_version" 1 "backend" "sqlite-derived"
   "authority_role" "migration-destination"
   "read_snapshot" t "node_snapshot" t "edge_snapshot" t
   "exact_vector_export" t "exact_retrieval" t "atomic_import" t
   "mutation_projection" t
   "runtime_reads" nil "runtime_writes" nil))

(defmethod storage-close ((backend sqlite-derived-storage))
  (bt:with-lock-held ((%sqlite-derived-lock backend))
    (unless (%sqlite-derived-closed-p backend)
      (let ((handle (%sqlite-derived-handle-slot backend)))
        (%sqlite-check (%sqlite-close-v2 handle) handle :derived-close)
        (setf (%sqlite-derived-closed-p backend) t
              (%sqlite-derived-handle-slot backend) (cffi:null-pointer))))
  t))

(defmethod storage-publish-checkpoint
    ((backend sqlite-derived-storage) projection-name state
     &key (agent-id "default") through-event-id through-position
       (projector-revision "1") (policy-revision "1"))
  (%storage-required-string projection-name "projection-name")
  (%storage-required-string agent-id "agent-id")
  (%storage-positive-integer through-event-id "through-event-id"
                             :zero-allowed t)
  (%storage-positive-integer through-position "through-position"
                             :zero-allowed t)
  (%storage-required-string projector-revision "projector-revision")
  (%storage-required-string policy-revision "policy-revision")
  (bt:with-lock-held ((%sqlite-derived-lock backend))
    (%sqlite-derived-in-transaction
     backend :derived-publish-checkpoint
     (lambda (handle)
       (let ((current-event -1) (current-position -1))
         (%with-sqlite-statement
             (statement handle
                        "SELECT through_event_id,through_storage_position FROM pai_projection_checkpoints WHERE projection_name=?1 AND agent_id=?2"
                        :derived-publish-checkpoint)
           (%sqlite-bind-text handle statement 1 projection-name
                              :derived-publish-checkpoint)
           (%sqlite-bind-text handle statement 2 agent-id
                              :derived-publish-checkpoint)
           (let ((code (%sqlite-step-raw statement)))
             (cond ((= code +sqlite-row+)
                    (setf current-event (%sqlite-column-int64 statement 0)
                          current-position (%sqlite-column-int64 statement 1)))
                   ((/= code +sqlite-done+)
                    (%sqlite-check code handle
                                   :derived-publish-checkpoint)))))
         (when (or (< through-event-id current-event)
                   (< through-position current-position))
           (error 'storage-conflict-error
                  :operation :derived-publish-checkpoint
                  :detail "checkpoint watermark would move backwards")))
       (let* ((state-json (%storage-json state))
              (hash (%storage-checkpoint-integrity-sha256
                      projection-name agent-id through-event-id through-position
                      projector-revision policy-revision state-json)))
         (%with-sqlite-statement
             (statement handle
                        "INSERT INTO pai_projection_checkpoints(projection_name,agent_id,through_event_id,through_storage_position,projector_revision,policy_revision,state_json,integrity_hash) VALUES(?1,?2,?3,?4,?5,?6,?7,?8) ON CONFLICT(projection_name,agent_id) DO UPDATE SET through_event_id=excluded.through_event_id,through_storage_position=excluded.through_storage_position,projector_revision=excluded.projector_revision,policy_revision=excluded.policy_revision,state_json=excluded.state_json,integrity_hash=excluded.integrity_hash,created_at=CURRENT_TIMESTAMP"
                        :derived-publish-checkpoint)
           (%sqlite-bind-text handle statement 1 projection-name
                              :derived-publish-checkpoint)
           (%sqlite-bind-text handle statement 2 agent-id
                              :derived-publish-checkpoint)
           (%sqlite-bind-int64 handle statement 3 through-event-id
                               :derived-publish-checkpoint)
           (%sqlite-bind-int64 handle statement 4 through-position
                               :derived-publish-checkpoint)
           (%sqlite-bind-text handle statement 5 projector-revision
                              :derived-publish-checkpoint)
           (%sqlite-bind-text handle statement 6 policy-revision
                              :derived-publish-checkpoint)
           (%sqlite-bind-text handle statement 7 state-json
                              :derived-publish-checkpoint)
           (%sqlite-bind-text handle statement 8 hash
                              :derived-publish-checkpoint)
           (%sqlite-step handle statement :derived-publish-checkpoint
                         +sqlite-done+))
         (%storage-object
          "schema_version" 1 "projection_name" projection-name
          "agent_id" agent-id "through_event_id" through-event-id
          "through_storage_position" through-position
          "projector_revision" projector-revision
          "policy_revision" policy-revision "state" state
          "integrity_hash" hash))))))

(defmethod storage-load-checkpoint
    ((backend sqlite-derived-storage) projection-name &key (agent-id "default"))
  (%storage-required-string projection-name "projection-name")
  (%storage-required-string agent-id "agent-id")
  (bt:with-lock-held ((%sqlite-derived-lock backend))
    (let ((handle (%sqlite-derived-handle backend :derived-load-checkpoint)))
      (%with-sqlite-statement
          (statement handle
                     "SELECT through_event_id,through_storage_position,projector_revision,policy_revision,state_json,integrity_hash FROM pai_projection_checkpoints WHERE projection_name=?1 AND agent_id=?2"
                     :derived-load-checkpoint)
        (%sqlite-bind-text handle statement 1 projection-name
                           :derived-load-checkpoint)
        (%sqlite-bind-text handle statement 2 agent-id
                           :derived-load-checkpoint)
        (let ((code (%sqlite-step-raw statement)))
          (cond
            ((= code +sqlite-done+) nil)
            ((= code +sqlite-row+)
             (let* ((through (%sqlite-column-int64 statement 0))
                    (position (%sqlite-column-int64 statement 1))
                    (projector (%sqlite-column-text statement 2))
                    (policy (%sqlite-column-text statement 3))
                    (state-json (%sqlite-column-text statement 4))
                    (stored-hash (%sqlite-column-text statement 5))
                    (actual-hash
                      (%storage-checkpoint-integrity-sha256
                        projection-name agent-id through position projector
                        policy state-json)))
               (unless (string= stored-hash actual-hash)
                 (error 'storage-integrity-error
                        :operation :derived-load-checkpoint
                        :detail "checkpoint integrity hash mismatch"))
               (%storage-object
                "schema_version" 1 "projection_name" projection-name
                "agent_id" agent-id "through_event_id" through
                "through_storage_position" position
                "projector_revision" projector "policy_revision" policy
                "state" (%storage-json-read state-json
                                             :derived-load-checkpoint)
                "integrity_hash" stored-hash)))
            (t (%sqlite-check code handle :derived-load-checkpoint))))))))

(defun %memory-provenance-string (value field)
  (unless (and (stringp value) (plusp (length value)) (<= (length value) 256))
    (error 'memory-storage-error :operation :provenance
           :detail (format nil "~a must be an explicit non-empty string" field)))
  value)

(defun make-memory-embedding-provenance
    (&key embedding-model embedding-revision retrieval-embedding-model
          retrieval-embedding-revision vector-dimension revision-evidence
          approval-scope)
  "Build an explicit declaration. Values are never inferred from live config."
  (dolist (entry `((,embedding-model "embedding-model")
                   (,embedding-revision "embedding-revision")
                   (,retrieval-embedding-model "retrieval-embedding-model")
                   (,retrieval-embedding-revision
                    "retrieval-embedding-revision")
                   (,revision-evidence "revision-evidence")
                   (,approval-scope "approval-scope")))
    (%memory-provenance-string (first entry) (second entry)))
  (unless (and (integerp vector-dimension) (plusp vector-dimension))
    (error 'memory-storage-error :operation :provenance
           :detail "vector-dimension must be a positive integer"))
  (%memory-storage-object
   "schema_version" 1 "embedding_model" embedding-model
   "embedding_revision" embedding-revision
   "retrieval_embedding_model" retrieval-embedding-model
   "retrieval_embedding_revision" retrieval-embedding-revision
   "vector_dimension" vector-dimension
   "revision_evidence" revision-evidence
   "approval_scope" approval-scope))

(defun %derived-digest-update (digest text)
  (let* ((octets (sb-ext:string-to-octets text :external-format :utf-8))
         (prefix (sb-ext:string-to-octets
                  (format nil "~d:" (length octets))
                  :external-format :utf-8)))
    (ironclad:update-digest digest prefix)
    (ironclad:update-digest digest octets)))

(defun %derived-digest-hex (digest)
  (string-downcase
   (ironclad:byte-array-to-hex-string (ironclad:produce-digest digest))))

(defun %derived-hex-octets (text)
  (unless (and (stringp text) (evenp (length text)))
    (error 'memory-storage-error :operation :decode-vector
           :detail "vector encoding must be even-length hexadecimal"))
  (let ((result (make-array (/ (length text) 2)
                            :element-type '(unsigned-byte 8))))
    (dotimes (index (length result) result)
      (let ((value (parse-integer text :start (* index 2)
                                  :end (+ (* index 2) 2)
                                  :radix 16 :junk-allowed nil)))
        (setf (aref result index) value)))))

(defun %derived-octets-hex (octets)
  (string-downcase (ironclad:byte-array-to-hex-string octets)))

(defun %derived-vector-dimension (octets)
  (unless (>= (length octets) 4)
    (error 'memory-storage-error :operation :validate-vector
           :detail "pgvector binary value is shorter than its header"))
  (+ (ash (aref octets 0) 8) (aref octets 1)))

(defun %derived-validate-vector (octets expected-dimension)
  (let ((dimension (%derived-vector-dimension octets)))
    (unless (and (= dimension expected-dimension)
                 (= (length octets) (+ 4 (* 4 dimension))))
      (error 'memory-storage-error :operation :validate-vector
             :detail "pgvector binary dimension or length mismatch"))))

(defun %derived-row-integrity (scalar-json embedding retrieval)
  (let ((digest (ironclad:make-digest :sha256)))
    (%derived-digest-update digest scalar-json)
    (%derived-digest-update digest (%derived-octets-hex embedding))
    (%derived-digest-update digest (%derived-octets-hex retrieval))
    (%derived-digest-hex digest)))

(defun %derived-edge-integrity (row-json)
  (let ((digest (ironclad:make-digest :sha256)))
    (%derived-digest-update digest row-json)
    (%derived-digest-hex digest)))

(defun %derived-reset-statement (statement)
  (%sqlite-reset-raw statement)
  (%sqlite-clear-bindings-raw statement))

(defun %derived-audit-unlocked (handle)
  (let ((node-digest (ironclad:make-digest :sha256))
        (vector-digest (ironclad:make-digest :sha256))
        (edge-digest (ironclad:make-digest :sha256))
        (node-count 0) (edge-count 0))
    (%with-sqlite-statement
        (statement handle
                   "SELECT scalar_json,embedding,retrieval_embedding,integrity_hash FROM pai_memory_nodes ORDER BY source_ordinal"
                   :audit-memory)
      (loop for code = (%sqlite-step-raw statement)
            while (= code +sqlite-row+)
            do (let* ((scalar (%sqlite-column-text statement 0))
                      (embedding (%sqlite-column-blob statement 1))
                      (retrieval (%sqlite-column-blob statement 2))
                      (stored (%sqlite-column-text statement 3)))
                 (unless (string= stored
                                  (%derived-row-integrity
                                   scalar embedding retrieval))
                   (error 'storage-integrity-error :operation :audit-memory
                          :detail "memory node integrity mismatch"))
                 (%derived-digest-update node-digest scalar)
                 (%derived-digest-update vector-digest
                                         (%derived-octets-hex embedding))
                 (%derived-digest-update vector-digest
                                         (%derived-octets-hex retrieval))
                 (incf node-count))
            finally (unless (= code +sqlite-done+)
                      (%sqlite-check code handle :audit-memory))))
    (%with-sqlite-statement
        (statement handle
                   "SELECT row_json,integrity_hash FROM pai_memory_edges ORDER BY source_ordinal"
                   :audit-memory)
      (loop for code = (%sqlite-step-raw statement)
            while (= code +sqlite-row+)
            do (let ((row (%sqlite-column-text statement 0))
                     (stored (%sqlite-column-text statement 1)))
                 (unless (string= stored (%derived-edge-integrity row))
                   (error 'storage-integrity-error :operation :audit-memory
                          :detail "memory edge integrity mismatch"))
                 (%derived-digest-update edge-digest row)
                 (incf edge-count))
            finally (unless (= code +sqlite-done+)
                      (%sqlite-check code handle :audit-memory))))
    (%memory-storage-object
     "schema_version" 1 "backend" "sqlite-derived"
     "node_count" node-count "edge_count" edge-count
     "node_sha256" (%derived-digest-hex node-digest)
     "vector_sha256" (%derived-digest-hex vector-digest)
     "vector_binary_encoding" "pgvector-send-v1"
     "edge_sha256" (%derived-digest-hex edge-digest))))

(defun %derived-provenance-value (provenance key)
  (unless (hash-table-p provenance)
    (error 'memory-storage-error :operation :provenance
           :detail "provenance must be built explicitly"))
  (gethash key provenance))

(defun %derived-seal-hash (provenance receipt)
  (let ((digest (ironclad:make-digest :sha256)))
    (dolist (value
             (list
              "1"
              (%derived-provenance-value provenance "embedding_model")
              (%derived-provenance-value provenance "embedding_revision")
              (%derived-provenance-value provenance
                                         "retrieval_embedding_model")
              (%derived-provenance-value provenance
                                         "retrieval_embedding_revision")
              (write-to-string
               (%derived-provenance-value provenance "vector_dimension"))
              (%derived-provenance-value provenance "revision_evidence")
              (%derived-provenance-value provenance "approval_scope")
              (write-to-string (gethash "node_count" receipt))
              (write-to-string (gethash "edge_count" receipt))
              (gethash "node_sha256" receipt)
              (gethash "vector_sha256" receipt)
              (gethash "edge_sha256" receipt)
              (gethash "vector_binary_encoding" receipt)))
      (%derived-digest-update digest value))
    (%derived-digest-hex digest)))

(defun %derived-verify-import-unlocked (handle audit)
  (let ((row nil))
    (%with-sqlite-statement
        (statement handle
                   "SELECT embedding_model,embedding_revision,retrieval_embedding_model,retrieval_embedding_revision,vector_dimension,revision_evidence,approval_scope,node_count,edge_count,node_sha256,vector_sha256,edge_sha256,vector_binary_encoding,seal_hash FROM pai_memory_imports WHERE import_name='canonical'"
                   :verify-memory-import)
      (let ((code (%sqlite-step-raw statement)))
        (cond
          ((= code +sqlite-done+)
           (when (or (plusp (gethash "node_count" audit))
                     (plusp (gethash "edge_count" audit)))
             (error 'storage-integrity-error :operation :verify-memory-import
                    :detail "memory rows exist without a provenance seal")))
          ((= code +sqlite-row+)
           (setf row
                 (list
                  (%sqlite-column-text statement 0)
                  (%sqlite-column-text statement 1)
                  (%sqlite-column-text statement 2)
                  (%sqlite-column-text statement 3)
                  (%sqlite-column-int64 statement 4)
                  (%sqlite-column-text statement 5)
                  (%sqlite-column-text statement 6)
                  (%sqlite-column-int64 statement 7)
                  (%sqlite-column-int64 statement 8)
                  (%sqlite-column-text statement 9)
                  (%sqlite-column-text statement 10)
                  (%sqlite-column-text statement 11)
                  (%sqlite-column-text statement 12)
                  (%sqlite-column-text statement 13))))
          (t (%sqlite-check code handle :verify-memory-import)))))
    (when row
      (let* ((provenance
               (make-memory-embedding-provenance
                :embedding-model (nth 0 row)
                :embedding-revision (nth 1 row)
                :retrieval-embedding-model (nth 2 row)
                :retrieval-embedding-revision (nth 3 row)
                :vector-dimension (nth 4 row)
                :revision-evidence (nth 5 row)
                :approval-scope (nth 6 row)))
             (stored-receipt
               (%memory-storage-object
                "node_count" (nth 7 row) "edge_count" (nth 8 row)
                "node_sha256" (nth 9 row) "vector_sha256" (nth 10 row)
                "edge_sha256" (nth 11 row)
                "vector_binary_encoding" (nth 12 row)))
             (stored-seal (nth 13 row)))
        (dolist (key '("node_count" "edge_count" "node_sha256"
                       "vector_sha256" "edge_sha256"
                       "vector_binary_encoding"))
          (unless (equal (gethash key stored-receipt) (gethash key audit))
            (error 'storage-integrity-error :operation :verify-memory-import
                   :detail "sealed receipt does not match stored memory")))
        (unless (string= stored-seal
                         (%derived-seal-hash provenance stored-receipt))
          (error 'storage-integrity-error :operation :verify-memory-import
                 :detail "memory provenance seal mismatch"))
        stored-seal))))

;;; Search keeps only K compact candidates. SQLite owns the collection; no
;;; generation-sized row/vector cache is constructed. The heap root is the
;;; worst retained candidate, giving O(N log K) selection and O(K) live state.
(defun %sqlite-memory-top-k-offer (heap candidate better-p)
  (labels ((better (a b) (funcall better-p a b)))
    (cond
      ((< (length heap) (array-total-size heap))
       (vector-push candidate heap)
       (loop for child = (1- (length heap)) then parent
             while (plusp child)
             for parent = (floor (1- child) 2)
             while (better (aref heap parent) (aref heap child))
             do (rotatef (aref heap parent) (aref heap child))))
      ((better candidate (aref heap 0))
       (setf (aref heap 0) candidate)
       (loop with count = (length heap)
             for parent = 0 then worst
             for left = (1+ (* 2 parent))
             while (< left count)
             for right = (1+ left)
             for worst = (if (and (< right count)
                                  (better (aref heap left) (aref heap right)))
                             right left)
             while (better (aref heap parent) (aref heap worst))
             do (rotatef (aref heap parent) (aref heap worst))))))
  heap)

(defun %sqlite-memory-exact-better-p (left right)
  (let ((ld (gethash "distance" left)) (rd (gethash "distance" right)))
    (if (= ld rd)
        (string< (gethash "id" left) (gethash "id" right))
        (< ld rd))))

(defun %sqlite-memory-lexical-better-p (left right)
  (let ((lt (gethash "lexical_tier" left))
        (rt (gethash "lexical_tier" right))
        (lc (gethash "lexical_match_count" left))
        (rc (gethash "lexical_match_count" right)))
    (cond ((/= lt rt) (> lt rt))
          ((/= lc rc) (> lc rc))
          (t (string< (gethash "id" left) (gethash "id" right))))))

(defun %sqlite-memory-verified-search-row (statement operation)
  (let* ((id (%sqlite-column-text statement 0))
         (scalar (%sqlite-column-text statement 1))
         (embedding (%sqlite-column-blob statement 2))
         (retrieval (%sqlite-column-blob statement 3))
         (integrity (%sqlite-column-text statement 4))
         (row (shasht:read-json scalar)))
    (unless (and (hash-table-p row)
                 (string= id (gethash "id" row ""))
                 (string= integrity
                          (%derived-row-integrity scalar embedding retrieval)))
      (error 'storage-integrity-error :operation operation
             :detail "memory node identity or integrity mismatch"))
    (values id row retrieval)))

(defun %sqlite-memory-ranking-row (statement operation)
  "Decode only fields required for ranking. The sealed projection authorizes
the scan; bounded winners are independently integrity-verified before return."
  (let* ((id (%sqlite-column-text statement 0))
         (scalar (%sqlite-column-text statement 1))
         (retrieval (%sqlite-column-blob statement 2))
         (row (shasht:read-json scalar)))
    (unless (and (hash-table-p row) (string= id (gethash "id" row "")))
      (error 'storage-integrity-error :operation operation
             :detail "memory ranking row identity mismatch"))
    (values id row retrieval)))

(defun %sqlite-memory-hydrate-search-results (handle results query operation)
  ;; Point reads remain inside the same read transaction as verification and
  ;; ranking. Only winners acquire full content / vectors; callers own them.
  ;; Always point-read and verify each bounded winner. Hydration controls only
  ;; which verified fields escape; it never disables output integrity checks.
  (%with-sqlite-statement
        (statement handle
                   "SELECT id,scalar_json,embedding,retrieval_embedding,integrity_hash FROM pai_memory_nodes WHERE id=?1"
                   operation)
      (loop for candidate across results
            do (%sqlite-bind-text handle statement 1 (gethash "id" candidate)
                                  operation)
               (%sqlite-step handle statement operation +sqlite-row+)
               (multiple-value-bind (id row retrieval)
                   (%sqlite-memory-verified-search-row statement operation)
                 (declare (ignore id))
                 (when (or (memory-exact-query-hydrate-p query)
                           (eq operation :lexical-search))
                   (setf (gethash "row" candidate) row))
                 (when (memory-exact-query-include-vector-p query)
                   (setf (gethash "vector" candidate)
                         (%memory-exact-decode-vector-octets retrieval))))
               (%sqlite-reset-raw statement)
               (%sqlite-clear-bindings-raw statement)))
  results)

(defmethod memory-storage-exact-search
    ((backend sqlite-derived-storage) (query memory-exact-query))
  (bt:with-lock-held ((%sqlite-derived-lock backend))
    (let ((handle (%sqlite-derived-handle backend :exact-search))
          (matches (make-array (memory-exact-query-limit query) :fill-pointer 0)))
      (%sqlite-exec handle "BEGIN" :exact-search)
      (handler-case
          (progn
            (%sqlite-derived-ensure-memory-verified backend handle :exact-search)
            ;; Ranking reads one vector, not both vectors plus a full integrity
            ;; encoding for every discarded row. The projection seal/data-version
            ;; authorizes the scan; bounded winners are verified by point read.
            (%with-sqlite-statement
                (statement handle
                           "SELECT id,scalar_json,retrieval_embedding FROM pai_memory_nodes ORDER BY source_ordinal"
                           :exact-search)
              (loop for code = (%sqlite-step-raw statement)
                    while (= code +sqlite-row+)
                    do (multiple-value-bind (id row retrieval)
                           (%sqlite-memory-ranking-row statement :exact-search)
                         (when (%memory-exact-row-eligible-p row query)
                           (%sqlite-memory-top-k-offer
                            matches
                            (%memory-storage-object
                             "id" id "distance"
                             (%memory-exact-cosine-distance
                              (memory-exact-query-vector query)
                              (%memory-exact-decode-vector-octets retrieval)))
                            #'%sqlite-memory-exact-better-p)))
                    finally (unless (= code +sqlite-done+)
                              (%sqlite-check code handle :exact-search))))
            (sort matches #'%sqlite-memory-exact-better-p)
            (%sqlite-memory-hydrate-search-results
             handle matches query :exact-search)
            (%sqlite-exec handle "COMMIT" :exact-search)
            (%memory-storage-object
             "schema_version" 1 "backend" "sqlite-derived"
             "profile" (memory-exact-query-profile query)
             "distance_metric" "cosine-distance"
             "ordering" "distance-then-id-codepoint"
             "exact_scan_forced" t "integrity_verified" t
             "integrity_scope" "sealed-projection-and-selected-results"
             "result_count" (length matches) "results" matches))
        (error (condition)
          (ignore-errors (%sqlite-exec handle "ROLLBACK" :exact-search))
          (error condition))))))

(defmethod memory-storage-lexical-search
    ((backend sqlite-derived-storage) (query memory-exact-query))
  (bt:with-lock-held ((%sqlite-derived-lock backend))
    (let ((handle (%sqlite-derived-handle backend :lexical-search))
          (lexemes (memory-exact-query-lexemes query))
          (matches (make-array (memory-exact-query-limit query) :fill-pointer 0)))
      (unless lexemes
        (error 'memory-storage-error :operation :lexical-search
               :detail "lexical search requires declared lexemes"))
      (%sqlite-exec handle "BEGIN" :lexical-search)
      (handler-case
          (progn
            (%sqlite-derived-ensure-memory-verified backend handle :lexical-search)
            (%with-sqlite-statement
                (statement handle
                           "SELECT id,scalar_json,retrieval_embedding FROM pai_memory_nodes ORDER BY source_ordinal"
                           :lexical-search)
              (loop for code = (%sqlite-step-raw statement)
                    while (= code +sqlite-row+)
                    do (multiple-value-bind (id row retrieval)
                           (%sqlite-memory-ranking-row statement :lexical-search)
                         (when (%memory-exact-row-eligible-p row query)
                           (let* ((content (gethash "content" row ""))
                                  (matched
                                    (remove-if-not
                                     (lambda (lexeme)
                                       (%sqlite-memory-lexeme-match-p content lexeme))
                                     lexemes))
                                  (phrase-p
                                    (some (lambda (lexeme)
                                            (string= "phrase" (gethash "kind" lexeme)))
                                          matched)))
                             (when matched
                               (%sqlite-memory-top-k-offer
                                matches
                                (%memory-storage-object
                                 "id" id "distance"
                                 (%memory-exact-cosine-distance
                                  (memory-exact-query-vector query)
                                  (%memory-exact-decode-vector-octets retrieval))
                                 "lexical_tier" (if phrase-p 2 1)
                                 "lexical_match_count" (length matched)
                                 "lexical_coverage"
                                 (/ (length matched) (float (length lexemes) 1.0d0))
                                 "lexical_terms"
                                 (coerce (mapcar (lambda (lexeme) (gethash "text" lexeme))
                                                 matched) 'vector))
                                #'%sqlite-memory-lexical-better-p)))))
                    finally (unless (= code +sqlite-done+)
                              (%sqlite-check code handle :lexical-search))))
            (sort matches #'%sqlite-memory-lexical-better-p)
            (%sqlite-memory-hydrate-search-results handle matches query :lexical-search)
            (%sqlite-exec handle "COMMIT" :lexical-search)
            (%memory-storage-object
             "schema_version" 1 "backend" "sqlite-derived"
             "profile" (memory-exact-query-profile query)
             "integrity_verified" t
             "integrity_scope" "sealed-projection-and-selected-results"
             "result_count" (length matches)
             "results" matches))
        (error (condition)
          (ignore-errors (%sqlite-exec handle "ROLLBACK" :lexical-search))
          (error condition))))))

(defun %sqlite-memory-projection-unlocked (handle)
  (%with-sqlite-statement
      (statement handle
                 "SELECT p.baseline_seal,p.storage_id,p.agent_id,p.through_event_id,p.through_storage_position,p.boundary_hash,p.projector_revision,b.baseline_event_id,b.baseline_storage_position,b.baseline_boundary_hash FROM pai_memory_projection p JOIN pai_memory_projection_binding b USING(projection_name) WHERE p.projection_name='canonical'"
                 :memory-projection)
    (let ((code (%sqlite-step-raw statement)))
      (cond
        ((= code +sqlite-done+) nil)
        ((= code +sqlite-row+)
         (%memory-storage-object
          "schema_version" 1
          "baseline_seal" (%sqlite-column-text statement 0)
          "storage_id" (%sqlite-column-text statement 1)
          "agent_id" (%sqlite-column-text statement 2)
          "through_event_id" (%sqlite-column-int64 statement 3)
          "through_storage_position" (%sqlite-column-int64 statement 4)
          "boundary_hash" (%sqlite-column-text statement 5)
          "projector_revision" (%sqlite-column-text statement 6)
          "baseline_event_id" (%sqlite-column-int64 statement 7)
          "baseline_storage_position" (%sqlite-column-int64 statement 8)
          "baseline_boundary_hash" (%sqlite-column-text statement 9)))
        (t (%sqlite-check code handle :memory-projection))))))

(defmethod memory-storage-bind-projection
    ((backend sqlite-derived-storage)
     &key baseline-seal storage-id agent-id through-event-id through-position
       boundary-hash (projector-revision "memory-projection-v1"))
  (dolist (item (list baseline-seal storage-id agent-id boundary-hash
                      projector-revision))
    (%memory-storage-required-string item "projection binding"))
  (unless (and (integerp through-event-id) (not (minusp through-event-id))
               (integerp through-position) (not (minusp through-position)))
    (error 'memory-storage-error :operation :bind-projection
           :detail "projection watermarks must be non-negative"))
  (bt:with-lock-held ((%sqlite-derived-lock backend))
    (%sqlite-derived-in-transaction
     backend :bind-memory-projection
     (lambda (handle)
       (let ((import-seal (%sqlite-derived-current-memory-seal
                           handle :bind-memory-projection))
             (current (%sqlite-memory-projection-unlocked handle)))
         (unless (and import-seal (string= import-seal baseline-seal))
           (error 'storage-integrity-error :operation :bind-memory-projection
                  :detail "baseline seal does not match the canonical import"))
         (when current
           (unless (and (string= baseline-seal (gethash "baseline_seal" current))
                        (string= storage-id (gethash "storage_id" current))
                        (string= agent-id (gethash "agent_id" current))
                        (= through-event-id (gethash "baseline_event_id" current))
                        (= through-position
                           (gethash "baseline_storage_position" current))
                        (string= boundary-hash
                                 (gethash "baseline_boundary_hash" current))
                        (string= projector-revision
                                 (gethash "projector_revision" current)))
             (error 'storage-conflict-error :operation :bind-memory-projection
                    :detail "memory projection is already bound differently")))
         (unless current
           (%with-sqlite-statement
               (statement handle
                          "INSERT INTO pai_memory_projection(projection_name,baseline_seal,storage_id,agent_id,through_event_id,through_storage_position,boundary_hash,projector_revision) VALUES('canonical',?1,?2,?3,?4,?5,?6,?7)"
                          :bind-memory-projection)
             (%sqlite-bind-text handle statement 1 baseline-seal :bind-memory-projection)
             (%sqlite-bind-text handle statement 2 storage-id :bind-memory-projection)
             (%sqlite-bind-text handle statement 3 agent-id :bind-memory-projection)
             (%sqlite-bind-int64 handle statement 4 through-event-id :bind-memory-projection)
             (%sqlite-bind-int64 handle statement 5 through-position :bind-memory-projection)
             (%sqlite-bind-text handle statement 6 boundary-hash :bind-memory-projection)
             (%sqlite-bind-text handle statement 7 projector-revision :bind-memory-projection)
             (%sqlite-step handle statement :bind-memory-projection +sqlite-done+)))
         (unless current
           (%with-sqlite-statement
               (statement handle
                          "INSERT INTO pai_memory_projection_binding(projection_name,baseline_event_id,baseline_storage_position,baseline_boundary_hash) VALUES('canonical',?1,?2,?3)"
                          :bind-memory-projection)
             (%sqlite-bind-int64 handle statement 1 through-event-id :bind-memory-projection)
             (%sqlite-bind-int64 handle statement 2 through-position :bind-memory-projection)
             (%sqlite-bind-text handle statement 3 boundary-hash :bind-memory-projection)
             (%sqlite-step handle statement :bind-memory-projection +sqlite-done+)))
         (%sqlite-memory-projection-unlocked handle))))))

(defmethod memory-storage-projection-report ((backend sqlite-derived-storage))
  (bt:with-lock-held ((%sqlite-derived-lock backend))
    (or (%sqlite-memory-projection-unlocked
         (%sqlite-derived-handle backend :memory-projection-report))
        (%memory-storage-object "schema_version" 1 "status" "unbound"))))

(defun %sqlite-memory-applied-event-hash-unlocked (handle event-id)
  (%with-sqlite-statement
      (statement handle
                 "SELECT event_hash FROM pai_memory_applied_events WHERE event_id=?1"
                 :apply-memory-mutation)
    (%sqlite-bind-int64 handle statement 1 event-id :apply-memory-mutation)
    (let ((code (%sqlite-step-raw statement)))
      (cond ((= code +sqlite-row+) (%sqlite-column-text statement 0))
            ((= code +sqlite-done+) nil)
            (t (%sqlite-check code handle :apply-memory-mutation))))))

(defun %sqlite-memory-apply-node-unlocked (handle payload operation)
  (let* ((scalar (gethash "scalar_json" payload))
         (embedding (%derived-hex-octets
                     (gethash "embedding_binary_hex" payload)))
         (retrieval (%derived-hex-octets
                     (gethash "retrieval_embedding_binary_hex" payload)))
         (row (and (stringp scalar) (shasht:read-json scalar)))
         (id (and (hash-table-p row) (gethash "id" row))))
    (%memory-storage-required-string id "node-id")
    (when (string= operation "update")
      (%with-sqlite-statement
          (statement handle "SELECT 1 FROM pai_memory_nodes WHERE id=?1"
                     :apply-memory-mutation)
        (%sqlite-bind-text handle statement 1 id :apply-memory-mutation)
        (unless (= (%sqlite-step-raw statement) +sqlite-row+)
          (error 'storage-conflict-error :operation :apply-memory-mutation
                 :detail "node update has no prior projection row"))))
    ;; Both vectors must remain in the same declared space as the import.
    (let ((dimension nil))
      (%with-sqlite-statement
          (statement handle
                     "SELECT vector_dimension FROM pai_memory_imports WHERE import_name='canonical'"
                     :apply-memory-mutation)
        (unless (= (%sqlite-step-raw statement) +sqlite-row+)
          (error 'storage-integrity-error :operation :apply-memory-mutation
                 :detail "canonical vector dimension is unavailable"))
        (setf dimension (%sqlite-column-int64 statement 0)))
      (%derived-validate-vector embedding dimension)
      (%derived-validate-vector retrieval dimension))
    (%with-sqlite-statement
        (statement handle
                   "INSERT INTO pai_memory_nodes(id,source_ordinal,scalar_json,embedding,retrieval_embedding,integrity_hash) VALUES(?1,(SELECT COALESCE(MAX(source_ordinal),0)+1 FROM pai_memory_nodes),?2,?3,?4,?5) ON CONFLICT(id) DO UPDATE SET scalar_json=excluded.scalar_json,embedding=excluded.embedding,retrieval_embedding=excluded.retrieval_embedding,integrity_hash=excluded.integrity_hash"
                   :apply-memory-mutation)
      (%sqlite-bind-text handle statement 1 id :apply-memory-mutation)
      (%sqlite-bind-text handle statement 2 scalar :apply-memory-mutation)
      (%sqlite-bind-blob handle statement 3 embedding :apply-memory-mutation)
      (%sqlite-bind-blob handle statement 4 retrieval :apply-memory-mutation)
      (%sqlite-bind-text handle statement 5
                         (%derived-row-integrity scalar embedding retrieval)
                         :apply-memory-mutation)
      (%sqlite-step handle statement :apply-memory-mutation +sqlite-done+))))

(defun %sqlite-memory-apply-edge-unlocked (handle payload operation)
  (let* ((row-json (gethash "row_json" payload))
         (row (and (stringp row-json) (shasht:read-json row-json)))
         (id (and (hash-table-p row) (gethash "id" row)))
         (from (and (hash-table-p row) (gethash "from_id" row)))
         (to (and (hash-table-p row) (gethash "to_id" row)))
         (kind (and (hash-table-p row) (gethash "edge_type" row))))
    (unless (and (integerp id) (plusp id))
      (error 'memory-storage-error :operation :apply-memory-mutation
             :detail "edge row has invalid identity"))
    (dolist (value (list from to kind))
      (%memory-storage-required-string value "edge field"))
    (if (string= operation "insert")
        (%with-sqlite-statement
            (statement handle
                       "INSERT INTO pai_memory_edges(id,source_ordinal,from_id,to_id,edge_type,row_json,integrity_hash) VALUES(?1,(SELECT COALESCE(MAX(source_ordinal),0)+1 FROM pai_memory_edges),?2,?3,?4,?5,?6)"
                       :apply-memory-mutation)
          (%sqlite-bind-int64 handle statement 1 id :apply-memory-mutation)
          (%sqlite-bind-text handle statement 2 from :apply-memory-mutation)
          (%sqlite-bind-text handle statement 3 to :apply-memory-mutation)
          (%sqlite-bind-text handle statement 4 kind :apply-memory-mutation)
          (%sqlite-bind-text handle statement 5 row-json :apply-memory-mutation)
          (%sqlite-bind-text handle statement 6 (%derived-edge-integrity row-json)
                             :apply-memory-mutation)
          (%sqlite-step handle statement :apply-memory-mutation +sqlite-done+))
        (let ((stored nil) (stored-integrity nil))
          (%with-sqlite-statement
              (statement handle "SELECT row_json,integrity_hash FROM pai_memory_edges WHERE id=?1"
                         :apply-memory-mutation)
            (%sqlite-bind-int64 handle statement 1 id :apply-memory-mutation)
            (when (= (%sqlite-step-raw statement) +sqlite-row+)
              (setf stored (%sqlite-column-text statement 0)
                    stored-integrity (%sqlite-column-text statement 1))))
          (unless (and stored
                       (string= stored-integrity (%derived-edge-integrity stored))
                       (equalp (shasht:read-json stored) row))
            (error 'storage-conflict-error :operation :apply-memory-mutation
                   :detail "edge deletion does not match current projection"))
          (%with-sqlite-statement
              (statement handle "DELETE FROM pai_memory_edges WHERE id=?1"
                         :apply-memory-mutation)
            (%sqlite-bind-int64 handle statement 1 id :apply-memory-mutation)
            (%sqlite-step handle statement :apply-memory-mutation +sqlite-done+))))))

(defmethod memory-storage-apply-mutation
    ((backend sqlite-derived-storage) mutation)
  (unless (and (hash-table-p mutation)
               (eql 1 (gethash "schema_version" mutation)))
    (error 'memory-storage-error :operation :apply-memory-mutation
           :detail "mutation is not a validated schema-v1 object"))
  (bt:with-lock-held ((%sqlite-derived-lock backend))
    (let ((receipt nil))
      (setf receipt
        (%sqlite-derived-in-transaction
         backend :apply-memory-mutation
         (lambda (handle)
           (block apply
             (let* ((projection (%sqlite-memory-projection-unlocked handle))
                    (event-id (gethash "event_id" mutation))
                    (mutation-position (gethash "storage_position" mutation))
                    (mutation-hash (%storage-sha256 (%storage-json mutation)))
                    (payload (%memory-storage-mutation-payload mutation))
                    (event-type (gethash "event_type" mutation))
                    (applied-event-hash
                      (%sqlite-memory-applied-event-hash-unlocked
                       handle event-id)))
               (unless projection
                 (error 'storage-conflict-error
                        :operation :apply-memory-mutation
                        :detail "memory projection is not bound"))
               (unless (and (string= (gethash "storage_id" mutation)
                                     (gethash "storage_id" projection))
                            (string= (gethash "agent_id" mutation)
                                     (gethash "agent_id" projection)))
                 (error 'storage-conflict-error
                        :operation :apply-memory-mutation
                        :detail "mutation belongs to another authority"))
               (when applied-event-hash
                 ;; The event authority's integrity hash is the stable replay
                 ;; identity. Hash-table serialization order is not.
                 (unless (string= applied-event-hash
                                  (gethash "event_hash" mutation))
                   (error 'storage-conflict-error
                          :operation :apply-memory-mutation
                          :detail "event identity conflicts with applied history"))
                 (return-from apply
                   (%memory-storage-object
                    "schema_version" 1 "status" "already-applied"
                    "event_id" event-id)))
               (unless (and (> event-id (gethash "through_event_id" projection))
                            (> mutation-position
                               (gethash "through_storage_position" projection)))
                 (error 'storage-conflict-error
                        :operation :apply-memory-mutation
                        :detail "mutation watermark is stale or non-increasing"))
               (labels ((apply-command (command-type command-payload)
                          (let ((operation (gethash "operation" command-payload)))
                            (if (string= command-type "memory-node-state")
                                (%sqlite-memory-apply-node-unlocked
                                 handle command-payload operation)
                                (%sqlite-memory-apply-edge-unlocked
                                 handle command-payload operation)))))
                 (if (string= event-type "memory-operation-state")
                     (let ((validated
                             (%memory-storage-validate-operation-payload
                              payload)))
                       (loop for command across (gethash "commands" validated)
                             do (apply-command
                                 (gethash "event_type" command)
                                 (gethash "payload" command))))
                     (progn
                       (%memory-storage-validate-state-payload
                        event-type payload)
                       (apply-command event-type payload))))
               (%with-sqlite-statement
                   (statement handle
                              "INSERT INTO pai_memory_applied_events(event_id,storage_position,mutation_hash,event_hash) VALUES(?1,?2,?3,?4)"
                              :apply-memory-mutation)
                 (%sqlite-bind-int64 handle statement 1 event-id
                                     :apply-memory-mutation)
                 (%sqlite-bind-int64 handle statement 2 mutation-position
                                     :apply-memory-mutation)
                 (%sqlite-bind-text handle statement 3 mutation-hash
                                    :apply-memory-mutation)
                 (%sqlite-bind-text handle statement 4
                                    (gethash "event_hash" mutation)
                                    :apply-memory-mutation)
                 (%sqlite-step handle statement :apply-memory-mutation
                               +sqlite-done+))
               (%with-sqlite-statement
                   (statement handle
                              "UPDATE pai_memory_projection SET through_event_id=?1,through_storage_position=?2,boundary_hash=?3 WHERE projection_name='canonical'"
                              :apply-memory-mutation)
                 (%sqlite-bind-int64 handle statement 1 event-id
                                     :apply-memory-mutation)
                 (%sqlite-bind-int64 handle statement 2 mutation-position
                                     :apply-memory-mutation)
                 (%sqlite-bind-text handle statement 3
                                    (gethash "event_hash" mutation)
                                    :apply-memory-mutation)
                 (%sqlite-step handle statement :apply-memory-mutation
                               +sqlite-done+))
               (%memory-storage-object
                "schema_version" 1 "status" "applied" "event_id" event-id))))))
      (when (string= "applied" (gethash "status" receipt ""))
        ;; Same-connection writes do not advance PRAGMA data_version.
        ;; Revalidate the small authority receipt on the next read, while
        ;; retaining the last observed data_version so a foreign connection's
        ;; later write still forces the exceptional full audit.
        (setf (%sqlite-derived-verified-memory-seal backend) nil))
      receipt)))

(defmethod memory-storage-operation-node
    ((backend sqlite-derived-storage) node-id)
  (%memory-storage-required-string node-id "node-id")
  (bt:with-lock-held ((%sqlite-derived-lock backend))
    (let ((handle (%sqlite-derived-handle backend :operation-node)))
      (%sqlite-derived-ensure-memory-verified backend handle :operation-node)
      (%with-sqlite-statement
          (statement handle
                     "SELECT scalar_json,embedding,retrieval_embedding,integrity_hash FROM pai_memory_nodes WHERE id=?1"
                     :operation-node)
        (%sqlite-bind-text handle statement 1 node-id :operation-node)
        (let ((code (%sqlite-step-raw statement)))
          (cond
            ((= code +sqlite-done+) nil)
            ((= code +sqlite-row+)
             (let* ((scalar (%sqlite-column-text statement 0))
                    (embedding (%sqlite-column-blob statement 1))
                    (retrieval (%sqlite-column-blob statement 2))
                    (integrity (%sqlite-column-text statement 3))
                    (row (%memory-storage-json-read scalar :operation-node)))
               (unless (string= integrity
                                (%derived-row-integrity
                                 scalar embedding retrieval))
                 (error 'storage-integrity-error :operation :operation-node
                        :detail "memory node integrity mismatch"))
               (unless (and (hash-table-p row)
                            (string= node-id (gethash "id" row "")))
                 (error 'storage-integrity-error :operation :operation-node
                        :detail "memory node key and row identity differ"))
               (%memory-storage-object
                "scalar_json" scalar
                "embedding_binary_hex" (%derived-octets-hex embedding)
                "retrieval_embedding_binary_hex"
                (%derived-octets-hex retrieval))))
            (t (%sqlite-check code handle :operation-node))))))))

(defmethod memory-storage-operation-edges
    ((backend sqlite-derived-storage) &key from-id to-id edge-type)
  (dolist (value (list from-id to-id edge-type))
    (when value (%memory-storage-required-string value "edge selector")))
  (bt:with-lock-held ((%sqlite-derived-lock backend))
    (let* ((handle (%sqlite-derived-handle backend :operation-edges))
           (clauses nil)
           (values nil))
      (%sqlite-derived-ensure-memory-verified backend handle :operation-edges)
      (dolist (entry (list (cons "from_id" from-id)
                           (cons "to_id" to-id)
                           (cons "edge_type" edge-type)))
        (when (cdr entry)
          (push (format nil "~a=?~d" (car entry) (1+ (length values))) clauses)
          (push (cdr entry) values)))
      (let ((sql (format nil
                         "SELECT row_json,integrity_hash FROM pai_memory_edges~@[ WHERE ~{~a~^ AND ~}~] ORDER BY id"
                         (and clauses (nreverse clauses))))
            (parameters (nreverse values))
            (rows nil))
        (%with-sqlite-statement (statement handle sql :operation-edges)
          (loop for value in parameters for index from 1
                do (%sqlite-bind-text handle statement index value
                                      :operation-edges))
          (loop for code = (%sqlite-step-raw statement)
                while (= code +sqlite-row+)
                do (let ((row-json (%sqlite-column-text statement 0))
                         (integrity (%sqlite-column-text statement 1)))
                     (unless (string= integrity
                                      (%derived-edge-integrity row-json))
                       (error 'storage-integrity-error
                              :operation :operation-edges
                              :detail "memory edge integrity mismatch"))
                     (let ((row (%memory-storage-json-read
                                 row-json :operation-edges)))
                       (unless (and (hash-table-p row)
                                    (or (null from-id)
                                        (string= from-id
                                                 (gethash "from_id" row "")))
                                    (or (null to-id)
                                        (string= to-id
                                                 (gethash "to_id" row "")))
                                    (or (null edge-type)
                                        (string= edge-type
                                                 (gethash "edge_type" row ""))))
                         (error 'storage-integrity-error
                                :operation :operation-edges
                                :detail "memory edge selector and row differ")))
                     (push row-json rows))
                finally (unless (= code +sqlite-done+)
                          (%sqlite-check code handle :operation-edges)))
        (nreverse rows))))))

(defmethod memory-storage-operation-next-edge-id
    ((backend sqlite-derived-storage))
  (bt:with-lock-held ((%sqlite-derived-lock backend))
    (let ((handle (%sqlite-derived-handle backend :operation-next-edge-id)))
      (%sqlite-derived-ensure-memory-verified
       backend handle :operation-next-edge-id)
      (%with-sqlite-statement
          (statement handle
                     "SELECT id,row_json,integrity_hash FROM pai_memory_edges ORDER BY id DESC LIMIT 1"
                     :operation-next-edge-id)
        (let ((code (%sqlite-step-raw statement)))
          (cond
            ((= code +sqlite-done+) 1)
            ((= code +sqlite-row+)
             (let ((id (%sqlite-column-int64 statement 0))
                   (row-json (%sqlite-column-text statement 1))
                   (integrity (%sqlite-column-text statement 2)))
               (unless (string= integrity (%derived-edge-integrity row-json))
                 (error 'storage-integrity-error
                        :operation :operation-next-edge-id
                        :detail "maximum memory edge integrity mismatch"))
               (let ((row (%memory-storage-json-read
                           row-json :operation-next-edge-id)))
                 (unless (and (hash-table-p row)
                              (= id (gethash "id" row -1)))
                   (error 'storage-integrity-error
                          :operation :operation-next-edge-id
                          :detail "maximum memory edge key and row differ")))
               (1+ id)))
            (t (%sqlite-check code handle :operation-next-edge-id))))))))

(defmethod memory-storage-operation-vector-dimension
    ((backend sqlite-derived-storage))
  (bt:with-lock-held ((%sqlite-derived-lock backend))
    (let ((handle (%sqlite-derived-handle backend :operation-vector-dimension)))
      (%sqlite-derived-ensure-memory-verified
       backend handle :operation-vector-dimension)
      (%with-sqlite-statement
          (statement handle
                     "SELECT embedding_model,embedding_revision,retrieval_embedding_model,retrieval_embedding_revision,vector_dimension,revision_evidence,approval_scope,node_count,edge_count,node_sha256,vector_sha256,edge_sha256,vector_binary_encoding,seal_hash FROM pai_memory_imports WHERE import_name='canonical'"
                     :operation-vector-dimension)
        (unless (= (%sqlite-step-raw statement) +sqlite-row+)
          (error 'storage-integrity-error
                 :operation :operation-vector-dimension
                 :detail "canonical vector dimension is unavailable"))
        (let* ((dimension (%sqlite-column-int64 statement 4))
               (provenance
                 (make-memory-embedding-provenance
                  :embedding-model (%sqlite-column-text statement 0)
                  :embedding-revision (%sqlite-column-text statement 1)
                  :retrieval-embedding-model (%sqlite-column-text statement 2)
                  :retrieval-embedding-revision (%sqlite-column-text statement 3)
                  :vector-dimension dimension
                  :revision-evidence (%sqlite-column-text statement 5)
                  :approval-scope (%sqlite-column-text statement 6)))
               (receipt
                 (%memory-storage-object
                  "node_count" (%sqlite-column-int64 statement 7)
                  "edge_count" (%sqlite-column-int64 statement 8)
                  "node_sha256" (%sqlite-column-text statement 9)
                  "vector_sha256" (%sqlite-column-text statement 10)
                  "edge_sha256" (%sqlite-column-text statement 11)
                  "vector_binary_encoding" (%sqlite-column-text statement 12)))
               (stored-seal (%sqlite-column-text statement 13)))
          (unless (string= stored-seal
                           (%derived-seal-hash provenance receipt))
            (error 'storage-integrity-error
                   :operation :operation-vector-dimension
                   :detail "memory provenance metadata seal mismatch"))
          dimension)))))

(defmethod memory-storage-audit-snapshot ((backend sqlite-derived-storage))
  (bt:with-lock-held ((%sqlite-derived-lock backend))
    (let* ((handle (%sqlite-derived-handle backend :audit-memory))
           (audit (%derived-audit-unlocked handle))
           (seal (%derived-verify-import-unlocked handle audit)))
      (setf (gethash "provenance_seal_hash" audit) (or seal :null))
      audit)))

(defun sqlite-derived-memory-provenance (backend)
  "Return the verified, content-free provenance of an immutable import."
  (unless (typep backend 'sqlite-derived-storage)
    (error 'memory-storage-error :operation :derived-memory-provenance
           :detail "SQLite derived backend is required"))
  (bt:with-lock-held ((%sqlite-derived-lock backend))
    (let* ((handle (%sqlite-derived-handle backend
                                           :derived-memory-provenance))
           (audit (%derived-audit-unlocked handle)))
      (%derived-verify-import-unlocked handle audit)
      (%with-sqlite-statement
          (statement handle
                     "SELECT embedding_model,embedding_revision,retrieval_embedding_model,retrieval_embedding_revision,vector_dimension,revision_evidence,approval_scope FROM pai_memory_imports WHERE import_name='canonical'"
                     :derived-memory-provenance)
        (unless (= (%sqlite-step-raw statement) +sqlite-row+)
          (error 'storage-integrity-error
                 :operation :derived-memory-provenance
                 :detail "canonical memory provenance is absent"))
        (make-memory-embedding-provenance
         :embedding-model (%sqlite-column-text statement 0)
         :embedding-revision (%sqlite-column-text statement 1)
         :retrieval-embedding-model (%sqlite-column-text statement 2)
         :retrieval-embedding-revision (%sqlite-column-text statement 3)
         :vector-dimension (%sqlite-column-int64 statement 4)
         :revision-evidence (%sqlite-column-text statement 5)
         :approval-scope (%sqlite-column-text statement 6))))))

(defmethod memory-storage-characterize ((backend sqlite-derived-storage))
  (bt:with-lock-held ((%sqlite-derived-lock backend))
    (let* ((handle (%sqlite-derived-handle backend :characterize-derived))
           (audit (%derived-audit-unlocked handle))
           (seal (%derived-verify-import-unlocked handle audit)))
      (%memory-storage-object
       "schema_version" 1 "backend" "sqlite-derived" "content_free" t
       "node_count" (gethash "node_count" audit)
       "edge_count" (gethash "edge_count" audit)
       "sealed_import_count" (if seal 1 0)
       "migration_ready" (if seal t nil)))))

(defmethod memory-storage-map-snapshot
    ((backend sqlite-derived-storage) node-visitor edge-visitor)
  (unless (and (functionp node-visitor) (functionp edge-visitor))
    (error 'memory-storage-error :operation :map-derived-snapshot
           :detail "node and edge visitors are required"))
  (bt:with-lock-held ((%sqlite-derived-lock backend))
    (let ((handle (%sqlite-derived-handle backend :map-derived-snapshot)))
      (%sqlite-exec handle "BEGIN" :map-derived-snapshot)
      (handler-case
          (let* ((audit (%derived-audit-unlocked handle))
                 (seal (%derived-verify-import-unlocked handle audit)))
            (unless seal
              (error 'storage-integrity-error
                     :operation :map-derived-snapshot
                     :detail "derived snapshot has no sealed canonical import"))
            (setf (gethash "provenance_seal_hash" audit) seal)
            (%with-sqlite-statement
                (statement handle
                           "SELECT scalar_json,embedding,retrieval_embedding,integrity_hash FROM pai_memory_nodes ORDER BY source_ordinal"
                           :map-derived-snapshot)
              (loop for code = (%sqlite-step-raw statement)
                    while (= code +sqlite-row+)
                    do (let ((scalar (%sqlite-column-text statement 0))
                             (embedding (%sqlite-column-blob statement 1))
                             (retrieval (%sqlite-column-blob statement 2))
                             (integrity (%sqlite-column-text statement 3)))
                         (unless (string= integrity
                                          (%derived-row-integrity
                                           scalar embedding retrieval))
                           (error 'storage-integrity-error
                                  :operation :map-derived-snapshot
                                  :detail "memory node integrity mismatch"))
                         (funcall node-visitor
                                  (%memory-storage-object
                                   "scalar_json" scalar
                                   "embedding_binary_hex"
                                   (%derived-octets-hex embedding)
                                   "retrieval_embedding_binary_hex"
                                   (%derived-octets-hex retrieval))))
                    finally (unless (= code +sqlite-done+)
                              (%sqlite-check code handle
                                             :map-derived-snapshot))))
            (%with-sqlite-statement
                (statement handle
                           "SELECT row_json,integrity_hash FROM pai_memory_edges ORDER BY source_ordinal"
                           :map-derived-snapshot)
              (loop for code = (%sqlite-step-raw statement)
                    while (= code +sqlite-row+)
                    do (let ((row-json (%sqlite-column-text statement 0))
                             (integrity (%sqlite-column-text statement 1)))
                         (unless (string= integrity
                                          (%derived-edge-integrity row-json))
                           (error 'storage-integrity-error
                                  :operation :map-derived-snapshot
                                  :detail "memory edge integrity mismatch"))
                         (funcall edge-visitor row-json))
                    finally (unless (= code +sqlite-done+)
                              (%sqlite-check code handle
                                             :map-derived-snapshot))))
            (%sqlite-exec handle "COMMIT" :map-derived-snapshot)
            audit)
        (error (condition)
          (ignore-errors (%sqlite-exec handle "ROLLBACK"
                                      :map-derived-snapshot))
          (error condition))))))

(defmethod memory-storage-import-snapshot
    ((backend sqlite-derived-storage) source provenance)
  (let* ((dimension
           (%derived-provenance-value provenance "vector_dimension"))
         (source-capabilities (memory-storage-capabilities source)))
    (unless (and (= 1 (%derived-provenance-value provenance "schema_version"))
                 (integerp dimension) (plusp dimension)
                 (eq t (gethash "read_snapshot" source-capabilities))
                 (eq t (gethash "exact_vector_export" source-capabilities)))
      (error 'memory-storage-error :operation :import-snapshot
             :detail "source capabilities or provenance declaration are invalid"))
    (dolist (key '("embedding_model" "embedding_revision"
                   "retrieval_embedding_model"
                   "retrieval_embedding_revision" "revision_evidence"
                   "approval_scope"))
      (%memory-provenance-string (%derived-provenance-value provenance key)
                                 key))
    (bt:with-lock-held ((%sqlite-derived-lock backend))
      (%sqlite-derived-in-transaction
       backend :import-snapshot
       (lambda (handle)
         (dolist (table '("pai_memory_nodes" "pai_memory_edges"
                          "pai_memory_imports"))
           (%with-sqlite-statement
               (statement handle (format nil "SELECT count(*) FROM ~a" table)
                          :import-snapshot)
             (%sqlite-step handle statement :import-snapshot +sqlite-row+)
             (unless (zerop (%sqlite-column-int64 statement 0))
               (error 'storage-conflict-error :operation :import-snapshot
                      :detail "derived memory destination is not empty"))))
         (%with-sqlite-statement
             (node-statement handle
                             "INSERT INTO pai_memory_nodes(id,source_ordinal,scalar_json,embedding,retrieval_embedding,integrity_hash) VALUES(?1,?2,?3,?4,?5,?6)"
                             :import-snapshot)
           (%with-sqlite-statement
               (edge-statement handle
                               "INSERT INTO pai_memory_edges(id,source_ordinal,from_id,to_id,edge_type,row_json,integrity_hash) VALUES(?1,?2,?3,?4,?5,?6,?7)"
                               :import-snapshot)
             (let* ((node-ordinal 0) (edge-ordinal 0)
                   (source-receipt
                     (memory-storage-map-snapshot
                      source
                      (lambda (node)
                        (let* ((scalar (gethash "scalar_json" node))
                               (row (and (stringp scalar)
                                         (shasht:read-json scalar)))
                               (id (and (hash-table-p row) (gethash "id" row)))
                               (embedding
                                 (%derived-hex-octets
                                  (gethash "embedding_binary_hex" node)))
                               (retrieval
                                 (%derived-hex-octets
                                  (gethash "retrieval_embedding_binary_hex"
                                           node))))
                          (%memory-provenance-string id "memory-node-id")
                          (%derived-validate-vector embedding dimension)
                          (%derived-validate-vector retrieval dimension)
                          (incf node-ordinal)
                          (%sqlite-bind-text handle node-statement 1 id
                                             :import-snapshot)
                          (%sqlite-bind-int64 handle node-statement 2
                                              node-ordinal :import-snapshot)
                          (%sqlite-bind-text handle node-statement 3 scalar
                                             :import-snapshot)
                          (%sqlite-bind-blob handle node-statement 4 embedding
                                             :import-snapshot)
                          (%sqlite-bind-blob handle node-statement 5 retrieval
                                             :import-snapshot)
                          (%sqlite-bind-text
                           handle node-statement 6
                           (%derived-row-integrity scalar embedding retrieval)
                           :import-snapshot)
                          (%sqlite-step handle node-statement :import-snapshot
                                        +sqlite-done+)
                          (%derived-reset-statement node-statement)))
                      (lambda (row-json)
                        (let* ((row (shasht:read-json row-json))
                               (id (gethash "id" row))
                               (from (gethash "from_id" row))
                               (to (gethash "to_id" row))
                               (kind (gethash "edge_type" row)))
                          (unless (integerp id)
                            (error 'memory-storage-error
                                   :operation :import-snapshot
                                   :detail "memory edge id is not an integer"))
                          (%memory-provenance-string from "edge-from-id")
                          (%memory-provenance-string to "edge-to-id")
                          (%memory-provenance-string kind "edge-type")
                          (incf edge-ordinal)
                          (%sqlite-bind-int64 handle edge-statement 1 id
                                              :import-snapshot)
                          (%sqlite-bind-int64 handle edge-statement 2
                                              edge-ordinal :import-snapshot)
                          (%sqlite-bind-text handle edge-statement 3 from
                                             :import-snapshot)
                          (%sqlite-bind-text handle edge-statement 4 to
                                             :import-snapshot)
                          (%sqlite-bind-text handle edge-statement 5 kind
                                             :import-snapshot)
                          (%sqlite-bind-text handle edge-statement 6 row-json
                                             :import-snapshot)
                          (%sqlite-bind-text
                           handle edge-statement 7
                           (%derived-edge-integrity row-json)
                           :import-snapshot)
                          (%sqlite-step handle edge-statement :import-snapshot
                                        +sqlite-done+)
                          (%derived-reset-statement edge-statement))))))
               (let ((audit (%derived-audit-unlocked handle)))
                 (dolist (key '("node_count" "edge_count" "node_sha256"
                                "vector_sha256" "edge_sha256"
                                "vector_binary_encoding"))
                   (unless (equal (gethash key source-receipt)
                                  (gethash key audit))
                     (error 'storage-integrity-error
                            :operation :import-snapshot
                            :detail (format nil "destination parity failed for ~a"
                                            key))))
                 (let ((seal (%derived-seal-hash provenance source-receipt)))
                   (%with-sqlite-statement
                       (statement handle
                                  "INSERT INTO pai_memory_imports(import_name,schema_version,embedding_model,embedding_revision,retrieval_embedding_model,retrieval_embedding_revision,vector_dimension,revision_evidence,approval_scope,node_count,edge_count,node_sha256,vector_sha256,edge_sha256,vector_binary_encoding,seal_hash) VALUES('canonical',1,?1,?2,?3,?4,?5,?6,?7,?8,?9,?10,?11,?12,?13,?14)"
                                  :import-snapshot)
                     (loop for value in
                           (list
                            (gethash "embedding_model" provenance)
                            (gethash "embedding_revision" provenance)
                            (gethash "retrieval_embedding_model" provenance)
                            (gethash "retrieval_embedding_revision" provenance))
                           for index from 1
                           do (%sqlite-bind-text handle statement index value
                                                 :import-snapshot))
                     (%sqlite-bind-int64 handle statement 5 dimension
                                         :import-snapshot)
                     (%sqlite-bind-text handle statement 6
                                        (gethash "revision_evidence" provenance)
                                        :import-snapshot)
                     (%sqlite-bind-text handle statement 7
                                        (gethash "approval_scope" provenance)
                                        :import-snapshot)
                     (%sqlite-bind-int64 handle statement 8
                                         (gethash "node_count" audit)
                                         :import-snapshot)
                     (%sqlite-bind-int64 handle statement 9
                                         (gethash "edge_count" audit)
                                         :import-snapshot)
                     (%sqlite-bind-text handle statement 10
                                        (gethash "node_sha256" audit)
                                        :import-snapshot)
                     (%sqlite-bind-text handle statement 11
                                        (gethash "vector_sha256" audit)
                                        :import-snapshot)
                     (%sqlite-bind-text handle statement 12
                                        (gethash "edge_sha256" audit)
                                        :import-snapshot)
                     (%sqlite-bind-text handle statement 13
                                        (gethash "vector_binary_encoding" audit)
                                        :import-snapshot)
                     (%sqlite-bind-text handle statement 14 seal
                                        :import-snapshot)
                     (%sqlite-step handle statement :import-snapshot
                                   +sqlite-done+))
                   (setf (%sqlite-derived-verified-memory-seal backend) nil
                         (%sqlite-derived-verified-memory-data-version backend) nil)
                   (%memory-storage-object
                    "schema_version" 1 "status" "imported"
                    "backend" "sqlite-derived"
                    "node_count" (gethash "node_count" audit)
                    "edge_count" (gethash "edge_count" audit)
                    "node_sha256" (gethash "node_sha256" audit)
                    "vector_sha256" (gethash "vector_sha256" audit)
                    "edge_sha256" (gethash "edge_sha256" audit)
                    "vector_binary_encoding"
                    (gethash "vector_binary_encoding" audit)
                    "provenance_seal_hash" seal)))))))))))
