;;;; postgres-memory-storage.lisp -- read-only memory migration source.
;;;;
;;;; Construction is inert. Each public operation owns one repeatable read-only
;;;; transaction and closes its connection before returning.

(in-package :agent)

(ql:quickload '(:postmodern :ironclad :babel) :silent t)

(export '(postgres-memory-storage make-postgres-memory-storage))

(defclass postgres-memory-storage (memory-storage-backend)
  ((database :initarg :database :reader %postgres-memory-database)
   (user :initarg :user :reader %postgres-memory-user)
   (password :initarg :password :reader %postgres-memory-password)
   (host :initarg :host :reader %postgres-memory-host)
   (port :initarg :port :reader %postgres-memory-port)))

(defparameter *postgres-memory-required-node-columns*
  '("id" "kind" "content" "embedding" "created_at" "last_accessed"
    "access_count" "importance" "valence" "arousal_at_encoding"
    "activation" "source_event_id" "is_cold" "origin_class"
    "epistemic_status" "producer" "model_purpose" "confidence"
    "grounding_status" "root_observation_ids" "generation_id"
    "supersedes_node_id" "quarantined" "epistemic_metadata"
    "retrieval_embedding"))

(defparameter *postgres-memory-required-edge-columns*
  '("id" "from_id" "to_id" "edge_type" "created_at"))

(defparameter *postgres-memory-provenance-columns*
  '("embedding_model_revision" "retrieval_embedding_model_revision"))

(defun make-postgres-memory-storage
    (&key (database (or (uiop:getenv "PAI_PG_DATABASE") "pai_memory"))
          (user (or (uiop:getenv "PAI_PG_USER") "pai"))
          (password (or (uiop:getenv "PAI_PG_PASSWORD")
                        "pai_local_dev_only"))
          (host (or (uiop:getenv "PAI_PG_HOST") "pai-postgres"))
          (port (parse-integer (or (uiop:getenv "PAI_PG_PORT") "5432"))))
  (unless (and (stringp database) (plusp (length database))
               (stringp user) (plusp (length user))
               (stringp password) (stringp host) (plusp (length host))
               (integerp port) (<= 1 port 65535))
    (error 'memory-storage-error :operation :construct
           :detail "invalid PostgreSQL connection configuration"))
  (make-instance 'postgres-memory-storage
                 :database database :user user :password password
                 :host host :port port))

(defmethod memory-storage-capabilities ((backend postgres-memory-storage))
  (declare (ignore backend))
  (%memory-storage-object
   "schema_version" 1 "backend" "postgresql"
   "authority_role" "qualification-source"
   "read_snapshot" t "node_snapshot" t "edge_snapshot" t
   "exact_vector_export" t "exact_retrieval" t
   ;; Runtime consumers have not crossed this port yet. Do not confuse the
   ;; database's abilities with this adapter's qualified surface.
   "runtime_reads" nil "runtime_writes" nil))

(defun %postgres-memory-vector-literal (vector)
  ;; CL's readable single-float representation round-trips every binary32
  ;; value. Values came from the closed binary decoder, so this is not a SQL
  ;; text injection surface.
  (format nil "[~{~a~^,~}]"
          (loop for value across vector collect (write-to-string value))))

(defun %postgres-memory-exact-where (query &key (require-vector-p t))
  (let ((profile (memory-exact-query-profile query))
        (clauses (if require-vector-p '("retrieval_embedding IS NOT NULL") nil))
        (parameters nil))
    (labels ((parameter (value)
               (setf parameters (append parameters (list value)))
               (format nil "$~d" (length parameters)))
             (add (text) (setf clauses (append clauses (list text))))
             (add-values (column values &key negate coalesce)
               (when values
                 (add
                  (format nil "~a ~aIN (~{~a~^,~})"
                          (if coalesce
                              (format nil "COALESCE(~a,'')" column)
                              column)
                          (if negate "NOT " "")
                          (mapcar #'parameter values))))))
      (cond
        ((string= profile "all-vectors-v1"))
        ((string= profile "safe-semantic-v1")
         (add "is_cold=false") (add "quarantined=false")
         (add "origin_class <> 'legacy-unclassified'")
         (add "epistemic_status NOT IN ('legacy-unclassified','rejected')")
         (add "grounding_status <> 'unclassified'"))
        ((string= profile "turn-neighborhood-v1")
         (add "is_cold=false") (add "quarantined=false")
         (add "origin_class IN ('lived-user','lived-agent-action','tool-result','external-source')")
         (add "epistemic_status NOT IN ('legacy-unclassified','rejected')")
         (add "grounding_status IN ('grounded','partially-grounded')")
         (add-values "epistemic_metadata->>'turn_id'"
                     (memory-exact-query-turn-ids query)))
        (t
         (error 'memory-storage-error :operation :exact-search
                :detail "unknown exact retrieval profile")))
      (add-values "kind" (memory-exact-query-kinds query))
      (add-values "origin_class" (memory-exact-query-origins query))
      (add-values "epistemic_status" (memory-exact-query-statuses query))
      (add-values "grounding_status"
                  (memory-exact-query-grounding-statuses query))
      (add-values "id" (memory-exact-query-excluded-ids query) :negate t)
      (add-values "epistemic_metadata->>'turn_id'"
                  (memory-exact-query-excluded-turn-ids query)
                  :negate t :coalesce t)
      (add-values "source_event_id"
                  (memory-exact-query-excluded-source-event-ids query)
                  :negate t :coalesce t)
      (when (memory-exact-query-as-of query)
        (add (format nil "created_at <= ~a::timestamptz"
                     (parameter (memory-exact-query-as-of query)))))
      (values (format nil "~{~a~^ AND ~}" clauses) parameters))))

(defun %postgres-memory-regex-escape (text)
  (with-output-to-string (stream)
    (loop for character across text
          do (when (find character "\\.^$|()[]{}*+?" :test #'char=)
               (write-char #\\ stream))
             (write-char character stream))))

(defun %postgres-memory-lexeme-pattern (lexeme)
  (let* ((text (gethash "text" lexeme))
         (kind (gethash "kind" lexeme))
         (body (if (string= kind "phrase")
                   (format nil "~{~a~^[[:space:]]+~}"
                           (mapcar #'%postgres-memory-regex-escape
                                   (uiop:split-string text
                                                      :separator '(#\Space))))
                   (%postgres-memory-regex-escape text))))
    (format nil "(^|[^[:alnum:]_])~a([^[:alnum:]_]|$)" body)))

(defun %postgres-memory-dynamic-query (sql parameters)
  "Execute parameterized dynamic SQL; POSTMODERN:QUERY is a macro and cannot
be APPLY'd. This is the same unnamed prepared-query path used by retrieval."
  (pomo::prepare-query pomo::*database* "" sql parameters)
  (pomo::exec-prepared pomo::*database* "" parameters
                       'cl-postgres:list-row-reader))

(declaim (ftype (function (t t t) t)
                %postgres-memory-call-read-snapshot))

(defmethod memory-storage-exact-search
    ((backend postgres-memory-storage) (query memory-exact-query))
  (%postgres-memory-call-read-snapshot
   backend :exact-search
   (lambda ()
     ;; The clone has HNSW. Disable every index path in this transaction so
     ;; the reference is the exact sequential behavior being ported.
     (pomo:execute "SET LOCAL enable_indexscan=off")
     (pomo:execute "SET LOCAL enable_indexonlyscan=off")
     (pomo:execute "SET LOCAL enable_bitmapscan=off")
     (multiple-value-bind (where parameters)
         (%postgres-memory-exact-where query)
       (let* ((literal
                (%postgres-memory-vector-literal
                 (memory-exact-query-vector query)))
              (limit-placeholder (format nil "$~d" (1+ (length parameters))))
              (sql
                (format nil
                        "SELECT id,(retrieval_embedding <=> '~a'::vector)::double precision AS distance~a~a FROM memory_nodes WHERE ~a ORDER BY distance,id COLLATE \"C\" LIMIT ~a"
                        literal
                        (if (memory-exact-query-hydrate-p query)
                            ",(to_jsonb(memory_nodes)-'embedding'-'retrieval_embedding')::text"
                            "")
                        (if (memory-exact-query-include-vector-p query)
                            ",encode(vector_send(retrieval_embedding),'hex')"
                            "")
                        where limit-placeholder))
              (all-parameters
                (append parameters (list (memory-exact-query-limit query))))
              (plan-lines
                (mapcar #'first
                        (%postgres-memory-dynamic-query
                         (format nil "EXPLAIN (COSTS OFF) ~a" sql)
                         all-parameters)))
              (plan (format nil "~{~a~%~}" plan-lines))
              (rows (%postgres-memory-dynamic-query sql all-parameters)))
         (unless (and (search "Seq Scan" plan :test #'char-equal)
                      (not (search "Index Scan" plan :test #'char-equal))
                      (not (search "Index Only Scan" plan
                                   :test #'char-equal))
                      (not (search "Bitmap" plan :test #'char-equal)))
           (error 'memory-storage-error :operation :exact-search
                  :detail "PostgreSQL did not produce a forced sequential plan"))
         (%memory-storage-object
          "schema_version" 1 "backend" "postgresql"
          "profile" (memory-exact-query-profile query)
          "distance_metric" "cosine-distance"
          "ordering" "distance-then-id-codepoint"
          "exact_scan_forced" t
          "transaction_read_only" t
          "result_count" (length rows)
          "results"
          (coerce
           (mapcar (lambda (row)
                     (let ((candidate
                             (%memory-storage-object
                              "id" (first row)
                              "distance" (coerce (second row) 'double-float))))
                       (when (memory-exact-query-hydrate-p query)
                         (setf (gethash "row" candidate)
                               (shasht:read-json (third row))))
                       (when (memory-exact-query-include-vector-p query)
                         (setf (gethash "vector" candidate)
                               (%memory-exact-decode-vector-hex
                                (nth (if (memory-exact-query-hydrate-p query)
                                         3 2)
                                     row))))
                       candidate))
                   rows)
           'vector)))))))

(defmethod memory-storage-lexical-search
    ((backend postgres-memory-storage) (query memory-exact-query))
  (%postgres-memory-call-read-snapshot
   backend :lexical-search
   (lambda ()
     (let ((lexemes (memory-exact-query-lexemes query)))
       (unless lexemes
         (error 'memory-storage-error :operation :lexical-search
                :detail "lexical search requires declared lexemes"))
       (multiple-value-bind (where parameters)
           (%postgres-memory-exact-where query :require-vector-p nil)
         (labels ((parameter (value)
                    (setf parameters (append parameters (list value)))
                    (format nil "$~d" (length parameters))))
           (let* ((literal (%postgres-memory-vector-literal
                            (memory-exact-query-vector query)))
                  (patterns (mapcar #'%postgres-memory-lexeme-pattern lexemes))
                  (placeholders (mapcar #'parameter patterns))
                  (flags (mapcar (lambda (placeholder)
                                   (format nil
                                           "CASE WHEN content ~~* ~a THEN 1 ELSE 0 END"
                                           placeholder))
                                 placeholders))
                  (phrase-flags
                    (loop for lexeme in lexemes for flag in flags
                          when (string= "phrase" (gethash "kind" lexeme))
                            collect flag))
                  (match-sum (format nil "(~{~a~^ + ~})" flags))
                  (phrase-sum (if phrase-flags
                                  (format nil "(~{~a~^ + ~})" phrase-flags)
                                  "0::integer"))
                  (lexical-condition
                    (format nil "(~{content ~~* ~a~^ OR ~})" placeholders))
                  (limit-placeholder
                    (parameter (memory-exact-query-limit query)))
                  (sql
                    (format nil
                            "SELECT id,CASE WHEN retrieval_embedding IS NULL THEN 1.0 ELSE (retrieval_embedding <=> '~a'::vector)::double precision END,(to_jsonb(memory_nodes)-'embedding'-'retrieval_embedding')::text,~{~a~^,~} FROM memory_nodes WHERE ~a AND ~a ORDER BY ~a DESC,~a DESC,id COLLATE \"C\" LIMIT ~a"
                            literal flags where lexical-condition phrase-sum
                            match-sum limit-placeholder))
                  (rows (%postgres-memory-dynamic-query sql parameters)))
             (%memory-storage-object
              "schema_version" 1 "backend" "postgresql"
              "profile" (memory-exact-query-profile query)
              "transaction_read_only" t "result_count" (length rows)
              "results"
              (coerce
               (mapcar
                (lambda (raw)
                  (let* ((matched
                           (loop for lexeme in lexemes for flag in (cdddr raw)
                                 when (and (numberp flag) (plusp flag))
                                   collect (gethash "text" lexeme)))
                         (phrase-p
                           (loop for lexeme in lexemes for flag in (cdddr raw)
                                  thereis (and (numberp flag) (plusp flag)
                                               (string= "phrase"
                                                       (gethash "kind" lexeme))))))
                    (%memory-storage-object
                     "id" (first raw) "distance"
                     (coerce (second raw) 'double-float)
                     "row" (shasht:read-json (third raw))
                     "lexical_tier" (if phrase-p 2 1)
                     "lexical_match_count" (length matched)
                     "lexical_coverage"
                     (/ (length matched) (float (length lexemes) 1.0d0))
                     "lexical_terms" (coerce matched 'vector))))
                rows)
               'vector)))))))))

(defun %postgres-memory-config (backend)
  (list (%postgres-memory-database backend)
        (%postgres-memory-user backend)
        (%postgres-memory-password backend)
        (%postgres-memory-host backend)
        :port (%postgres-memory-port backend)))

(defun %postgres-memory-call-read-snapshot (backend operation function)
  (handler-case
      (pomo:with-connection (%postgres-memory-config backend)
        (pomo:with-transaction ()
          ;; Must be the first statement in this transaction. PostgreSQL then
          ;; enforces the no-write claim rather than relying on caller intent.
          (pomo:execute
           "SET TRANSACTION ISOLATION LEVEL REPEATABLE READ READ ONLY")
          (funcall function)))
    (memory-storage-error (condition) (error condition))
    (error (condition)
      (error 'memory-storage-error :operation operation
             :detail (format nil "~a" condition)))))

(defun %postgres-memory-columns (table)
  (pomo:query
   "SELECT column_name FROM information_schema.columns WHERE table_schema='public' AND table_name=$1 ORDER BY ordinal_position"
   table :column))

(defun %postgres-memory-all-present-p (required present)
  (every (lambda (name) (member name present :test #'string=)) required))

(defun %postgres-memory-constraint-definitions (table)
  (pomo:query
   "SELECT pg_get_constraintdef(oid) FROM pg_constraint WHERE conrelid=$1::regclass ORDER BY conname"
   table :column))

(defun %postgres-memory-definition-present-p (needle definitions)
  (find-if (lambda (definition)
             (search needle definition :test #'char-equal))
           definitions))

(defun %postgres-memory-vector-dimensions (column)
  ;; COLUMN is selected only from this file's closed constants.
  (coerce
   (pomo:query
    (format nil
            "SELECT DISTINCT vector_dims(~a) FROM memory_nodes WHERE ~a IS NOT NULL ORDER BY 1"
            column column)
    :column)
   'vector))

(defun %postgres-memory-retrieval-index-kind ()
  (let ((definitions
          (pomo:query
           "SELECT indexdef FROM pg_indexes WHERE schemaname='public' AND tablename='memory_nodes' AND indexdef LIKE '%retrieval_embedding%' ORDER BY indexname"
           :column)))
    (cond ((find-if (lambda (text) (search " USING hnsw " text)) definitions)
           "hnsw")
          ((find-if (lambda (text) (search " USING ivfflat " text)) definitions)
           "ivfflat")
          (t "exact-scan"))))

(defmethod memory-storage-characterize ((backend postgres-memory-storage))
  (%postgres-memory-call-read-snapshot
   backend :characterize
   (lambda ()
     (let* ((read-only-p
              (string-equal "on"
                            (pomo:query "SHOW transaction_read_only" :single)))
            (node-columns (%postgres-memory-columns "memory_nodes"))
            (edge-columns (%postgres-memory-columns "memory_edges"))
            (node-constraints
              (and node-columns
                   (%postgres-memory-constraint-definitions "memory_nodes")))
            (edge-constraints
              (and edge-columns
                   (%postgres-memory-constraint-definitions "memory_edges")))
            (node-primary-key-p
              (%postgres-memory-definition-present-p
               "PRIMARY KEY (id)" node-constraints))
            (edge-primary-key-p
              (%postgres-memory-definition-present-p
               "PRIMARY KEY (id)" edge-constraints))
            (edge-unique-p
              (%postgres-memory-definition-present-p
               "UNIQUE (from_id, to_id, edge_type)" edge-constraints))
            (edge-from-fk-p
              (%postgres-memory-definition-present-p
               "FOREIGN KEY (from_id) REFERENCES memory_nodes(id)"
               edge-constraints))
            (edge-to-fk-p
              (%postgres-memory-definition-present-p
               "FOREIGN KEY (to_id) REFERENCES memory_nodes(id)"
               edge-constraints))
            (schema-p
              (not
               (null
                (and (%postgres-memory-all-present-p
                      *postgres-memory-required-node-columns* node-columns)
                     (%postgres-memory-all-present-p
                      *postgres-memory-required-edge-columns* edge-columns)
                     node-primary-key-p edge-primary-key-p edge-unique-p))))
            (provenance-p
              (%postgres-memory-all-present-p
               *postgres-memory-provenance-columns* node-columns))
            (node-count
              (if node-columns
                  (pomo:query "SELECT count(*) FROM memory_nodes" :single) 0))
            (edge-count
              (if edge-columns
                  (pomo:query "SELECT count(*) FROM memory_edges" :single) 0))
            (null-embedding
              (if schema-p
                  (pomo:query
                   "SELECT count(*) FROM memory_nodes WHERE embedding IS NULL"
                   :single)
                  0))
            (null-retrieval
              (if schema-p
                  (pomo:query
                   "SELECT count(*) FROM memory_nodes WHERE retrieval_embedding IS NULL"
                   :single)
                  0))
            (embedding-dimensions
              (if schema-p (%postgres-memory-vector-dimensions "embedding")
                  (vector)))
            (retrieval-dimensions
              (if schema-p
                  (%postgres-memory-vector-dimensions "retrieval_embedding")
                  (vector)))
            (duplicate-edges
              (if schema-p
                  (pomo:query
                   "SELECT coalesce(sum(n-1),0) FROM (SELECT count(*) n FROM memory_edges GROUP BY from_id,to_id,edge_type HAVING count(*)>1) duplicates"
                   :single)
                  0))
            (orphans
              (if schema-p
                  (pomo:query
                   "SELECT count(*) FILTER (WHERE source.id IS NULL),count(*) FILTER (WHERE target.id IS NULL) FROM memory_edges edge_row LEFT JOIN memory_nodes source ON source.id=edge_row.from_id LEFT JOIN memory_nodes target ON target.id=edge_row.to_id"
                   :row)
                  '(0 0)))
            (dimensions-p
              (and (= 1 (length embedding-dimensions))
                   (= 1 (length retrieval-dimensions))
                   (= (aref embedding-dimensions 0)
                      (aref retrieval-dimensions 0))))
            (structural-p
              (and read-only-p schema-p (zerop null-embedding)
                   (zerop null-retrieval) dimensions-p
                   (zerop duplicate-edges) (zerop (first orphans))
                   (zerop (second orphans)))))
       (%memory-storage-object
        "schema_version" 1 "backend" "postgresql"
        "content_free" t "transaction_read_only" read-only-p
        "required_schema_present" schema-p
        "node_primary_key_present" (if node-primary-key-p t nil)
        "edge_primary_key_present" (if edge-primary-key-p t nil)
        "edge_unique_triple_present" (if edge-unique-p t nil)
        "edge_from_foreign_key_present" (if edge-from-fk-p t nil)
        "edge_to_foreign_key_present" (if edge-to-fk-p t nil)
        "referential_constraints_complete"
        (if (and edge-from-fk-p edge-to-fk-p) t nil)
        "node_columns" (coerce node-columns 'vector)
        "edge_columns" (coerce edge-columns 'vector)
        "node_count" node-count "edge_count" edge-count
        "null_embedding_count" null-embedding
        "null_retrieval_embedding_count" null-retrieval
        "embedding_dimensions" embedding-dimensions
        "retrieval_embedding_dimensions" retrieval-dimensions
        "duplicate_edge_count" duplicate-edges
        "orphan_from_count" (first orphans)
        "orphan_to_count" (second orphans)
        "retrieval_index_kind" (%postgres-memory-retrieval-index-kind)
        "embedding_provenance_persisted" provenance-p
        "structurally_ready" structural-p
        "migration_ready" (and structural-p provenance-p))))))

(defun %postgres-memory-digest-update (digest text)
  (let* ((octets (babel:string-to-octets text :encoding :utf-8))
         (prefix (babel:string-to-octets
                  (format nil "~d:" (length octets)) :encoding :utf-8)))
    (ironclad:update-digest digest prefix)
    (ironclad:update-digest digest octets)))

(defun %postgres-memory-digest-hex (digest)
  (string-downcase
   (ironclad:byte-array-to-hex-string
    (ironclad:produce-digest digest))))

(defmethod memory-storage-map-snapshot
    ((backend postgres-memory-storage) node-visitor edge-visitor)
  (unless (and (functionp node-visitor) (functionp edge-visitor))
    (error 'memory-storage-error :operation :map-snapshot
           :detail "node and edge visitors must be functions"))
  (%postgres-memory-call-read-snapshot
   backend :map-snapshot
   (lambda ()
     (unless (string-equal "on"
                           (pomo:query "SHOW transaction_read_only" :single))
       (error 'memory-storage-error :operation :map-snapshot
              :detail "PostgreSQL did not enforce a read-only transaction"))
     (let ((node-digest (ironclad:make-digest :sha256))
           (vector-digest (ironclad:make-digest :sha256))
           (edge-digest (ironclad:make-digest :sha256))
           (node-count 0) (edge-count 0))
       (pomo:doquery
           ("SELECT (to_jsonb(memory_nodes)-'embedding'-'retrieval_embedding')::text,encode(vector_send(embedding),'hex'),encode(vector_send(retrieval_embedding),'hex') FROM memory_nodes ORDER BY id")
           (scalar-json embedding-hex retrieval-embedding-hex)
         (%postgres-memory-digest-update node-digest scalar-json)
         (%postgres-memory-digest-update vector-digest embedding-hex)
         (%postgres-memory-digest-update vector-digest retrieval-embedding-hex)
         (funcall node-visitor
                  (%memory-storage-object
                   "scalar_json" scalar-json
                   "embedding_binary_hex" embedding-hex
                   "retrieval_embedding_binary_hex" retrieval-embedding-hex))
         (incf node-count))
       (pomo:doquery
           ("SELECT row_to_json(memory_edges)::text FROM memory_edges ORDER BY id")
           (row-json)
         (%postgres-memory-digest-update edge-digest row-json)
         (funcall edge-visitor row-json)
         (incf edge-count))
       (%memory-storage-object
        "schema_version" 1 "backend" "postgresql"
        "transaction_read_only" t
        "node_count" node-count "edge_count" edge-count
        "node_sha256" (%postgres-memory-digest-hex node-digest)
        "vector_sha256" (%postgres-memory-digest-hex vector-digest)
        "vector_binary_encoding" "pgvector-send-v1"
        "edge_sha256" (%postgres-memory-digest-hex edge-digest))))))
