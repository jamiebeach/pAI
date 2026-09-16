;;;; typed-retrieval.lisp -- pure, explainable memory retrieval.
;;;;
;;;; MEMORY-SEARCH is deliberately read-only. It never rehearses a memory,
;;;; adjusts affect, writes an event, or changes access metadata. Explicit use
;;;; accounting is isolated in MEMORY-RECORD-USE so background cognition cannot
;;;; make its own synthetic evidence progressively easier to retrieve.

(in-package :agent)

(export '(memory-search memory-search-turn-neighborhood memory-record-use typed-retrieval-report
          typed-retrieval-embedding-readiness
          typed-retrieval-backfill-document-embeddings))

(defparameter *typed-retrieval-modes*
  '(:conversation :cognitive-evidence :initiative :audit))
(defparameter *typed-retrieval-safe-modes*
  '(:conversation :cognitive-evidence :initiative))
(defparameter *typed-retrieval-user-visible-activation-increment* 0.05d0)
(defparameter *typed-retrieval-user-visible-activation-cap* 0.85d0)
(defparameter *typed-retrieval-legacy-event-interval-seconds* 300)
(defvar *typed-retrieval-stats* (make-hash-table :test #'equal))
(defvar *typed-retrieval-stats-lock*
  (bt:make-lock "typed-retrieval-stats"))
(defvar *typed-retrieval-last-legacy-event-at* 0)
(defvar *typed-retrieval-pending-legacy-calls* 0)
(defparameter *typed-retrieval-backfill-batch-size* 32)
(defparameter *typed-retrieval-explicit-lexical-limit* 50)
(defparameter *typed-retrieval-explicit-max-terms* 6)
(defparameter *typed-retrieval-explicit-max-term-chars* 64)
(defparameter *typed-retrieval-explicit-max-phrase-chars* 80)
(defparameter *typed-retrieval-explicit-stop-words*
  '("a" "an" "and" "about" "do" "does" "for" "from" "how" "i" "is"
    "it" "me" "memories" "memory" "my" "of" "on" "our" "recall"
    "remember" "tell" "the" "to" "was" "were" "what" "when" "where"
    "which" "who" "why" "with" "you"))
(defvar *memory-search-query-vector-fn* nil
  "Optional (query typed-p) query-vector port for deterministic qualification.")
(defvar *memory-retrieval-timing-ms* nil)

(defun %memory-retrieval-time (phase thunk)
  (let ((started (get-internal-real-time)))
    (unwind-protect
         (funcall thunk)
      (when (hash-table-p *memory-retrieval-timing-ms*)
        (incf (gethash phase *memory-retrieval-timing-ms* 0)
              (round (* 1000d0
                        (/ (- (get-internal-real-time) started)
                           internal-time-units-per-second))))))))

(defun %typed-retrieval-normalize-mode (mode)
  (let ((normalized (etypecase mode
                      (keyword mode)
                      (symbol (intern (string-upcase (symbol-name mode)) :keyword))
                      (string (intern (string-upcase mode) :keyword)))))
    (unless (member normalized *typed-retrieval-modes*)
      (error "Unsupported typed retrieval mode ~s." mode))
    normalized))

(defun %typed-retrieval-normalize-strategy (strategy)
  (let ((normalized (etypecase strategy
                      (keyword strategy)
                      (symbol (intern (string-upcase (symbol-name strategy))
                                      :keyword))
                      (string (intern (string-upcase strategy) :keyword)))))
    (unless (member normalized '(:semantic :hybrid-explicit))
      (error "Unsupported typed retrieval candidate strategy ~s." strategy))
    normalized))

(defun %typed-retrieval-list (value)
  (cond ((null value) nil)
        ((listp value) (copy-list value))
        ((vectorp value) (coerce value 'list))
        (t (list value))))

(defun %typed-retrieval-string-values (values)
  (remove-duplicates
   (mapcar #'string (%typed-retrieval-list values))
   :test #'string=))

(defun %typed-retrieval-normalize-values (values &optional allowed)
  (let ((normalized
          (remove-duplicates
           (mapcar (lambda (value) (string-downcase (string value)))
                   (%typed-retrieval-list values))
           :test #'string=)))
    (when allowed
      (dolist (value normalized)
        (unless (member value allowed :test #'string=)
          (error "Unsupported typed retrieval filter value ~s." value))))
    normalized))

(defun %typed-retrieval-json (text fallback)
  (handler-case
      (if (and (stringp text) (plusp (length text)))
          (shasht:read-json text)
          fallback)
    (error () fallback)))

(defun %typed-retrieval-copy-object (table)
  (let ((copy (make-hash-table :test #'equal)))
    (loop for key being the hash-keys of table using (hash-value value)
          do (setf (gethash key copy) value))
    copy))

(defun %typed-retrieval-tokenize (text)
  (let ((tokens nil) (characters nil))
    (labels ((flush ()
               (when characters
                 (push (coerce (nreverse characters) 'string) tokens)
                 (setf characters nil))))
      (loop for character across (if (stringp text) text "")
            do (if (alphanumericp character)
                   (push character characters)
                   (flush)))
      (flush))
    (nreverse tokens)))

(defun %typed-retrieval-explicit-lexemes (query)
  "Return bounded topic-neutral quoted phrases/tokens for explicit recall."
  (let* ((text (if (stringp query) query ""))
         (unquoted (make-string (length text) :initial-element #\Space))
         (phrases nil) (phrase-characters nil) (inside nil))
    (loop for character across text for index from 0
          do (cond
               ((char= character #\")
                (if inside
                    (let* ((raw (coerce (nreverse phrase-characters) 'string))
                           (words (%typed-retrieval-tokenize raw))
                           (normalized
                             (string-downcase (format nil "~{~a~^ ~}" words))))
                      (when (and words (<= 3 (length normalized)
                                           *typed-retrieval-explicit-max-phrase-chars*))
                        (push normalized phrases))
                      (setf phrase-characters nil inside nil))
                    (setf inside t)))
               (inside (push character phrase-characters))
               (t (setf (aref unquoted index) character))))
    (let ((items nil) (seen (make-hash-table :test #'equal)))
      (labels ((add (value kind)
                 (let ((key (format nil "~a:~a" kind value)))
                   (when (and (< (length items)
                                 *typed-retrieval-explicit-max-terms*)
                              (not (gethash key seen)))
                     (setf (gethash key seen) t
                           items (append items
                                         (list (obj "text" value
                                                    "kind" kind))))))))
        (dolist (phrase (nreverse phrases)) (add phrase "phrase"))
        (dolist (raw (%typed-retrieval-tokenize unquoted))
          (let ((token (string-downcase raw)))
            (when (and (<= 3 (length token)
                              *typed-retrieval-explicit-max-term-chars*)
                       (not (member token *typed-retrieval-explicit-stop-words*
                                    :test #'string=)))
              (add token "token")))))
      items)))

(defun %typed-retrieval-regex-escape (text)
  (with-output-to-string (stream)
    (loop for character across text
          do (when (find character "\\.^$|()[]{}*+?" :test #'char=)
               (write-char #\\ stream))
             (write-char character stream))))

(defun %typed-retrieval-lexeme-pattern (lexeme)
  (let* ((text (gethash "text" lexeme))
         (kind (gethash "kind" lexeme))
         (body
           (if (string= kind "phrase")
               (format nil "~{~a~^[[:space:]]+~}"
                       (mapcar #'%typed-retrieval-regex-escape
                               (%typed-retrieval-tokenize text)))
               (%typed-retrieval-regex-escape text))))
    (format nil "(^|[^[:alnum:]_])~a([^[:alnum:]_]|$)" body)))

(defun %typed-retrieval-row-object (row weights)
  (destructuring-bind
      (id kind content created observed-at valid-from valid-to
          last-accessed access-count importance valence arousal activation
          source-event-id origin-class epistemic-status producer model-purpose
          confidence grounding-status roots-text generation-id
          supersedes-node-id quarantined metadata-text sim)
      row
    (let* ((similarity (if (numberp sim) sim 0.0d0))
           (recency (%recency-score created))
           (score (+ (* (gethash "sim" weights) similarity)
                     (* (gethash "imp" weights) importance)
                     (* (gethash "rec" weights) recency)
                     (* (gethash "act" weights) activation))))
      (obj "id" id "kind" kind "content" content
           "created_at" created "observed_at" (or observed-at created)
           "valid_from" (or valid-from :null) "valid_to" (or valid-to :null)
           "last_accessed" last-accessed "access_count" access-count
           "importance" importance "valence" valence
           "arousal_at_encoding" arousal "activation" activation
           "source_event_id" (or source-event-id :null)
           "origin_class" origin-class "epistemic_status" epistemic-status
           "producer" (or producer :null)
           "model_purpose" (or model-purpose :null)
           "confidence" (or confidence :null)
           "grounding_status" grounding-status
           "root_observation_ids" (%typed-retrieval-json roots-text (vector))
           "generation_id" (or generation-id :null)
           "supersedes_node_id" (or supersedes-node-id :null)
           "quarantined" (if quarantined t nil)
           "epistemic_metadata" (%typed-retrieval-json metadata-text (obj))
           "retrieval_score" score "similarity" similarity
           "recency_score" recency))))

(defun %typed-retrieval-storage-timestamp (value)
  "Render imported UTC JSON timestamps like the incumbent PostgreSQL ::text."
  (unless (or (null value) (eq value :null))
    (let ((text (copy-seq value)))
      (when (and (> (length text) 10) (char= (aref text 10) #\T))
        (setf (aref text 10) #\Space))
      (cond
        ((and (plusp (length text))
              (char-equal (aref text (1- (length text))) #\Z))
         (concatenate 'string (subseq text 0 (1- (length text))) "+00"))
        ((and (>= (length text) 6)
              (member (aref text (- (length text) 6)) '(#\+ #\-))
              (string= ":00" text :start2 (- (length text) 3)))
         (subseq text 0 (- (length text) 3)))
        (t text)))))

(defun %typed-retrieval-storage-row-object (candidate weights)
  (let* ((row (gethash "row" candidate))
         (created-at
           (and (hash-table-p row)
                (%typed-retrieval-storage-timestamp
                 (gethash "created_at" row))))
         (null-value (lambda (value) (if (eq value :null) nil value)))
         (json-text (lambda (value fallback)
                      (shasht:write-json
                       (if (or (null value) (eq value :null)) fallback value)
                       nil))))
    (unless (hash-table-p row)
      (error "Hydrated memory candidate is missing its private row."))
    (%typed-retrieval-row-object
     (list
      (gethash "id" row) (gethash "kind" row) (gethash "content" row)
      created-at
      (or (%typed-retrieval-storage-timestamp (gethash "observed_at" row))
          created-at)
      (%typed-retrieval-storage-timestamp (gethash "valid_from" row))
      (%typed-retrieval-storage-timestamp (gethash "valid_to" row))
      (%typed-retrieval-storage-timestamp (gethash "last_accessed" row))
      (gethash "access_count" row)
      (gethash "importance" row) (gethash "valence" row)
      (gethash "arousal_at_encoding" row) (gethash "activation" row)
      (funcall null-value (gethash "source_event_id" row))
      (gethash "origin_class" row) (gethash "epistemic_status" row)
      (funcall null-value (gethash "producer" row))
      (funcall null-value (gethash "model_purpose" row))
      (funcall null-value (gethash "confidence" row))
      (gethash "grounding_status" row)
      (funcall json-text (gethash "root_observation_ids" row) (vector))
      (funcall null-value (gethash "generation_id" row))
      (funcall null-value (gethash "supersedes_node_id" row))
      (gethash "quarantined" row)
      (funcall json-text (gethash "epistemic_metadata" row) (obj))
      (- 1.0d0 (gethash "distance" candidate)))
     weights)))

(defun %typed-retrieval-safe-mode-p (mode)
  (member mode *typed-retrieval-safe-modes*))

(defun %typed-retrieval-query (sql parameters)
  "Execute dynamically parameterized SQL using the same prepared-query path
expanded by POSTMODERN:QUERY. QUERY is a macro and cannot be APPLY'd."
  (pomo::prepare-query pomo::*database* "" sql parameters)
  (pomo::exec-prepared pomo::*database* "" parameters
                       'cl-postgres:list-row-reader))

(defun %typed-retrieval-record-stat (key &optional (amount 1))
  (bt:with-lock-held (*typed-retrieval-stats-lock*)
    (incf (gethash key *typed-retrieval-stats* 0) amount)))

(defun typed-retrieval-report ()
  (bt:with-lock-held (*typed-retrieval-stats-lock*)
    (let ((counts (obj)))
      (maphash (lambda (key value) (setf (gethash key counts) value))
               *typed-retrieval-stats*)
      (obj "schema_version" 1
           "embedding_contract"
           (if (and (boundp '*retrieval-embedding-mode*)
                    (eq *retrieval-embedding-mode* :enforced))
               "search_query/search_document" "legacy-untyped")
           "legacy_event_interval_seconds"
           *typed-retrieval-legacy-event-interval-seconds*
           "counts" counts))))

(defun typed-retrieval-embedding-readiness ()
  "Report typed-vector coverage without changing database state."
  (with-pg
    (let* ((column-p
             (pomo:query
              "SELECT EXISTS (SELECT 1 FROM information_schema.columns WHERE table_name='memory_nodes' AND column_name='retrieval_embedding')"
              :single))
           (counts
             (and column-p
                  (pomo:query
                   "SELECT count(*),count(retrieval_embedding) FROM memory_nodes"
                   :row)))
           (total (if counts (first counts) 0))
           (typed (if counts (second counts) 0)))
      (obj "schema_version" 1 "column_available" (if column-p t nil)
           "total_rows" total "typed_rows" typed
           "missing_rows" (- total typed)
           "ready" (if (and column-p (= total typed)) t nil)))))

(defun typed-retrieval-backfill-document-embeddings
    (&key (batch-size *typed-retrieval-backfill-batch-size*) max-rows
          time-limit-seconds)
  "Explicit operator migration. It is never called at load or during chat."
  (when (and (boundp '*memory-cognitive-mutation-mode*)
             (eq *memory-cognitive-mutation-mode* :event-first))
    (error 'memory-storage-error :operation :retrieval-embedding-backfill
           :detail "PostgreSQL backfill is unavailable after SQLite cutover"))
  (unless (and (integerp batch-size) (<= 1 batch-size 128))
    (error "Backfill batch size must be from 1 through 128."))
  (unless (or (null max-rows) (and (integerp max-rows) (plusp max-rows)))
    (error "Backfill max-rows must be a positive integer or NIL."))
  (unless (or (null time-limit-seconds)
              (and (integerp time-limit-seconds)
                   (plusp time-limit-seconds)))
    (error "Backfill time limit must be a positive integer or NIL."))
  (let ((updated 0)
        (deadline (and time-limit-seconds
                       (+ (get-internal-real-time)
                          (* time-limit-seconds
                             internal-time-units-per-second)))))
    (loop
      for remaining = (and max-rows (- max-rows updated))
      while (or (null remaining) (plusp remaining))
      for limit = (min batch-size (or remaining batch-size))
      for rows = (with-pg
                   (pomo:query
                    "SELECT id,content FROM memory_nodes WHERE retrieval_embedding IS NULL ORDER BY id LIMIT $1"
                    limit))
      while rows
      do (dolist (row rows)
           (when (and deadline (>= (get-internal-real-time) deadline))
             (error "Typed retrieval backfill exceeded its sealed time limit after ~a rows."
                    updated))
           (destructuring-bind (id content) row
             (let ((literal
                     (%vector-literal
                      (embed-retrieval-document (or content "")))))
               (with-pg
                 (let ((changed
                         (or (pomo:execute
                              (format nil "UPDATE memory_nodes SET retrieval_embedding='~a'::vector WHERE id=$1 AND retrieval_embedding IS NULL"
                                      literal)
                              id)
                             0)))
                   (when (plusp changed)
                     (%memory-queue-node-state-current-connection
                      id "update" "retrieval-embedding-backfill"))))
               (incf updated))))
      finally (return updated))))

(defun memory-search (query &key (k 8) (mode :conversation) kinds origins
                                  statuses (require-grounded nil require-grounded-p)
                                  exclude-ids exclude-turn-ids
                                  exclude-source-event-ids
                                  include-quarantined as-of
                                  (candidate-strategy :semantic))
  "Return ranked, typed memory objects without changing any durable or live
state. Safe modes exclude cold, quarantined, rejected, legacy-unclassified and
ungrounded rows. AUDIT may inspect those rows, and is the only mode where
INCLUDE-QUARANTINED takes effect. Every result includes the effective
eligibility policy and score components that caused it to be returned.
HYBRID-EXPLICIT adds a bounded lexical union for conscious recall only."
  (unless (and (integerp k) (plusp k) (<= k 50))
    (error "K must be an integer from 1 through 50."))
  (let* ((effective-mode (%typed-retrieval-normalize-mode mode))
         (effective-strategy
           (%typed-retrieval-normalize-strategy candidate-strategy))
         (safe-mode-p (%typed-retrieval-safe-mode-p effective-mode))
         (kind-values (%typed-retrieval-normalize-values kinds))
         (origin-values
           (%typed-retrieval-normalize-values
            origins (and (boundp '*epistemic-origin-classes*)
                         *epistemic-origin-classes*)))
         (status-values
           (%typed-retrieval-normalize-values
            statuses (and (boundp '*epistemic-statuses*)
                          *epistemic-statuses*)))
         (excluded (%typed-retrieval-string-values exclude-ids))
         (excluded-turns (%typed-retrieval-string-values exclude-turn-ids))
         (excluded-events
           (%typed-retrieval-string-values exclude-source-event-ids))
         (grounding-policy (cond (require-grounded-p
                                  (if require-grounded
                                      '("grounded")
                                      (and safe-mode-p
                                           '("grounded" "partially-grounded"))))
                                 (safe-mode-p '("grounded" "partially-grounded"))
                                 (t nil)))
         (effective-include-quarantined
           (and (eq effective-mode :audit) include-quarantined))
         (typed-embedding-p
           (and (boundp '*retrieval-embedding-mode*)
                (eq *retrieval-embedding-mode* :enforced)))
         (qvec
           (%memory-retrieval-time
            "memory_query_embedding"
            (lambda ()
              (if *memory-search-query-vector-fn*
                  (funcall *memory-search-query-vector-fn*
                           query typed-embedding-p)
                  (if typed-embedding-p
                      (embed-retrieval-query query)
                      (embed-text query))))))
         (qlit (%vector-literal qvec))
         (embedding-column
           (if typed-embedding-p "retrieval_embedding" "embedding"))
         (candidate-limit (min 250 (max 50 (* k 5))))
         (weights (if *memory-affect-modulator-fn*
                      (funcall *memory-affect-modulator-fn*
                               *memory-recall-weights*)
                      *memory-recall-weights*))
         (eligibility
           (obj "mode" (string-downcase (symbol-name effective-mode))
                "quarantined_included" (if effective-include-quarantined t nil)
                "grounding_policy"
                (if grounding-policy (coerce grounding-policy 'vector) :null)
                "origin_filter"
                (if origin-values (coerce origin-values 'vector) :null)
                "status_filter"
                (if status-values (coerce status-values 'vector) :null)
                "as_of" (or as-of :null))))
    (when (and *memory-search-storage-backend*
               (not (and safe-mode-p typed-embedding-p)))
      (error "Storage-port memory search requires safe mode and typed embeddings."))
    (labels
        ((conditions-for (parameter require-embedding-p)
           (let ((conditions nil))
             (labels ((add (condition) (push condition conditions))
                      (add-values (column values)
                        (when values
                          (add
                           (format nil "~a IN (~{~a~^, ~})" column
                                   (mapcar (lambda (value)
                                             (funcall parameter value))
                                           values))))))
               (when safe-mode-p
                 (add "is_cold = false")
                 (add "origin_class <> 'legacy-unclassified'")
                 (add "epistemic_status NOT IN ('legacy-unclassified','rejected')")
                 (add "grounding_status <> 'unclassified'"))
               (when (and require-embedding-p typed-embedding-p)
                 (add "retrieval_embedding IS NOT NULL"))
               (unless effective-include-quarantined
                 (add "quarantined = false"))
               (add-values "kind" kind-values)
               (add-values "origin_class" origin-values)
               (add-values "epistemic_status" status-values)
               (add-values "grounding_status" grounding-policy)
               (add-values "id" nil)
               (when excluded
                 (add (format nil "id NOT IN (~{~a~^, ~})"
                              (mapcar (lambda (value)
                                        (funcall parameter value))
                                      excluded))))
               (when excluded-turns
                 (add
                  (format nil
                          "COALESCE(epistemic_metadata->>'turn_id','') NOT IN (~{~a~^, ~})"
                          (mapcar (lambda (value)
                                    (funcall parameter value))
                                  excluded-turns))))
               (when excluded-events
                 (add
                  (format nil
                          "COALESCE(source_event_id,'') NOT IN (~{~a~^, ~})"
                          (mapcar (lambda (value)
                                    (funcall parameter value))
                                  excluded-events))))
               (when as-of
                 (add (format nil "created_at <= ~a::timestamptz"
                              (funcall parameter as-of)))))
             (nreverse conditions)))
         (decorate (row)
           (setf (gethash "eligibility" row) eligibility)
           row)
         (semantic-search ()
           (let ((scored
                   (if *memory-search-storage-backend*
                       (let* ((storage-query
                                (make-memory-exact-query
                                 :vector-values qvec
                                 :profile "safe-semantic-v1"
                                 :limit candidate-limit :hydrate-p t
                                 :kinds kind-values :origins origin-values
                                 :statuses status-values
                                 :grounding-statuses grounding-policy
                                 :excluded-ids excluded
                                 :excluded-turn-ids excluded-turns
                                 :excluded-source-event-ids excluded-events
                                 :as-of as-of))
                              (report
                                (%memory-retrieval-time
                                 "memory_semantic_scan"
                                 (lambda ()
                                   (memory-storage-exact-search
                                    *memory-search-storage-backend*
                                    storage-query)))))
                         (map 'list
                              (lambda (candidate)
                                (decorate
                                 (%typed-retrieval-storage-row-object
                                  candidate weights)))
                              (gethash "results" report)))
                       (let ((params nil))
                         (labels ((parameter (value)
                                    (setf params (append params (list value)))
                                    (format nil "$~d" (length params))))
                           (let* ((conditions (conditions-for #'parameter t))
                                  (limit-placeholder (parameter candidate-limit))
                                  (where (if conditions
                                             (format nil "WHERE ~{~a~^ AND ~}"
                                                     conditions)
                                             ""))
                                  (sql
                                    (format nil
                                            "SELECT id, kind, content, created_at::text, COALESCE(to_jsonb(memory_nodes)->>'observed_at',created_at::text), to_jsonb(memory_nodes)->>'valid_from', to_jsonb(memory_nodes)->>'valid_to', last_accessed::text, access_count, importance, valence, arousal_at_encoding, activation, source_event_id, origin_class, epistemic_status, producer, model_purpose, confidence, grounding_status, root_observation_ids::text, generation_id, supersedes_node_id, quarantined, epistemic_metadata::text, 1 - (~a <=> '~a'::vector) AS sim FROM memory_nodes ~a ORDER BY ~a <=> '~a'::vector LIMIT ~a"
                                            embedding-column qlit where
                                            embedding-column qlit
                                            limit-placeholder))
                                  (rows (with-pg
                                          (%typed-retrieval-query sql params))))
                             (mapcar
                              (lambda (row)
                                (decorate
                                 (%typed-retrieval-row-object row weights)))
                              rows)))))))
             (setf scored
                   (sort scored
                         (lambda (left right)
                           (let ((ls (gethash "retrieval_score" left))
                                 (rs (gethash "retrieval_score" right)))
                             (if (= ls rs)
                                 (string< (gethash "id" left)
                                          (gethash "id" right))
                                 (> ls rs))))))
             (subseq scored 0 (min k (length scored)))))
         (lexical-search (lexemes)
           (if (null lexemes)
               nil
               (if *memory-search-storage-backend*
                   (let* ((storage-query
                            (make-memory-exact-query
                             :vector-values qvec
                             :profile "safe-semantic-v1"
                             :limit *typed-retrieval-explicit-lexical-limit*
                             :hydrate-p t :lexemes lexemes
                             :kinds kind-values :origins origin-values
                             :statuses status-values
                             :grounding-statuses grounding-policy
                             :excluded-ids excluded
                             :excluded-turn-ids excluded-turns
                             :excluded-source-event-ids excluded-events
                             :as-of as-of))
                          (report
                            (memory-storage-lexical-search
                             *memory-search-storage-backend* storage-query)))
                     (map 'list
                          (lambda (candidate)
                            (let ((row
                                    (decorate
                                     (%typed-retrieval-storage-row-object
                                      candidate weights))))
                              (dolist (key '("lexical_tier"
                                             "lexical_match_count"
                                             "lexical_coverage"
                                             "lexical_terms"))
                                (setf (gethash key row)
                                      (gethash key candidate)))
                              row))
                          (gethash "results" report)))
                   (let ((params nil))
                 (labels ((parameter (value)
                            (setf params (append params (list value)))
                            (format nil "$~d" (length params))))
                   (let* ((conditions (conditions-for #'parameter nil))
                          (patterns (mapcar #'%typed-retrieval-lexeme-pattern
                                            lexemes))
                          (placeholders (mapcar #'parameter patterns))
                          (matches
                            (mapcar (lambda (placeholder)
                                      (format nil
                                              "CASE WHEN content ~~* ~a THEN 1 ELSE 0 END"
                                              placeholder))
                                    placeholders))
                          (phrase-matches
                            (loop for lexeme in lexemes
                                  for expression in matches
                                  when (string= "phrase"
                                                (gethash "kind" lexeme))
                                    collect expression))
                          (match-sum (format nil "(~{~a~^ + ~})" matches))
                          (phrase-sum
                            (if phrase-matches
                                (format nil "(~{~a~^ + ~})" phrase-matches)
                                 "0::integer"))
                          (lexical-condition
                             (format nil "(~{content ~~* ~a~^ OR ~})"
                                    placeholders))
                          (all-conditions
                            (append conditions (list lexical-condition)))
                          (limit-placeholder
                            (parameter *typed-retrieval-explicit-lexical-limit*))
                          (where
                            (format nil "WHERE ~{~a~^ AND ~}" all-conditions))
                          (similarity
                            (format nil
                                    "CASE WHEN ~a IS NULL THEN NULL ELSE 1 - (~a <=> '~a'::vector) END"
                                    embedding-column embedding-column qlit))
                          (sql
                            (format nil
                                    "SELECT id, kind, content, created_at::text, COALESCE(to_jsonb(memory_nodes)->>'observed_at',created_at::text), to_jsonb(memory_nodes)->>'valid_from', to_jsonb(memory_nodes)->>'valid_to', last_accessed::text, access_count, importance, valence, arousal_at_encoding, activation, source_event_id, origin_class, epistemic_status, producer, model_purpose, confidence, grounding_status, root_observation_ids::text, generation_id, supersedes_node_id, quarantined, epistemic_metadata::text, ~a AS sim, ~{~a~^, ~} FROM memory_nodes ~a ORDER BY ~a DESC, ~a DESC, id LIMIT ~a"
                                    similarity matches where phrase-sum match-sum
                                    limit-placeholder))
                          (rows (with-pg (%typed-retrieval-query sql params))))
                     (mapcar
                      (lambda (raw)
                        (let* ((row (decorate
                                     (%typed-retrieval-row-object
                                      (subseq raw 0 26) weights)))
                               (flags (subseq raw 26))
                               (matched
                                 (loop for lexeme in lexemes for flag in flags
                                       when (and (numberp flag) (plusp flag))
                                         collect (gethash "text" lexeme)))
                               (phrase-p
                                 (loop for lexeme in lexemes for flag in flags
                                       thereis
                                       (and (numberp flag) (plusp flag)
                                            (string= "phrase"
                                                     (gethash "kind" lexeme)))))
                               (coverage (/ (length matched)
                                            (float (length lexemes) 1.0d0))))
                          (setf (gethash "lexical_tier" row)
                                (if phrase-p 2 1)
                                (gethash "lexical_match_count" row)
                                (length matched)
                                (gethash "lexical_coverage" row) coverage
                                (gethash "lexical_terms" row)
                                (coerce matched 'vector))
                          row))
                       rows))))))))
      (let* ((semantic (semantic-search))
             (lexemes (and (eq effective-strategy :hybrid-explicit)
                           (%typed-retrieval-explicit-lexemes query))))
        (if (eq effective-strategy :semantic)
            (values semantic
                    (obj "strategy" "semantic"
                         "semantic_candidate_count" (length semantic)
                         "lexical_candidate_count" 0
                         "union_candidate_count" (length semantic)
                         "returned_count" (length semantic)
                         "database_write_count" 0))
            (let* ((lexical (lexical-search lexemes))
                   (by-id (make-hash-table :test #'equal)))
              (dolist (source semantic)
                (let ((row (%typed-retrieval-copy-object source)))
                  (setf (gethash "candidate_sources" row) (vector "semantic")
                        (gethash (gethash "id" row) by-id) row)))
              (dolist (source lexical)
                (let* ((id (gethash "id" source))
                       (existing (gethash id by-id))
                       (row (or existing (%typed-retrieval-copy-object source))))
                  (setf (gethash "candidate_sources" row)
                        (if existing (vector "semantic" "lexical")
                            (vector "lexical")))
                  (dolist (key '("lexical_tier" "lexical_match_count"
                                 "lexical_coverage" "lexical_terms"))
                    (setf (gethash key row) (gethash key source)))
                  (setf (gethash id by-id) row)))
              (let ((union nil))
                (maphash (lambda (id row) (declare (ignore id)) (push row union))
                         by-id)
                (setf union
                      (sort union
                            (lambda (left right)
                              (let ((lt (gethash "lexical_tier" left 0))
                                    (rt (gethash "lexical_tier" right 0))
                                    (lc (gethash "lexical_coverage" left 0.0d0))
                                    (rc (gethash "lexical_coverage" right 0.0d0))
                                    (ls (gethash "retrieval_score" left 0.0d0))
                                    (rs (gethash "retrieval_score" right 0.0d0)))
                                (cond ((/= lt rt) (> lt rt))
                                      ((/= lc rc) (> lc rc))
                                      ((/= ls rs) (> ls rs))
                                      (t (string< (gethash "id" left)
                                                  (gethash "id" right))))))))
                (let ((returned (subseq union 0 (min k (length union)))))
                  (values
                   returned
                   (obj "strategy" "hybrid-explicit"
                        "semantic_candidate_count" (length semantic)
                        "lexical_candidate_count" (length lexical)
                        "union_candidate_count" (length union)
                        "returned_count" (length returned)
                        "lexical_terms"
                        (coerce (mapcar (lambda (lexeme)
                                          (gethash "text" lexeme))
                                        lexemes)
                                'vector)
                         "database_write_count" 0))))))))))

(defun memory-search-turn-neighborhood
    (query anchors &key (max-turns 50) (max-rows 200))
  "Read safe same-turn siblings for semantic ANCHORS without durable effects.
The query is embedded once; stored typed document vectors provide similarity
and bundle-dedup vectors. TURN_ID is relational data only, never a topic rule."
  (unless (and (integerp max-turns) (<= 1 max-turns 50))
    (error "MAX-TURNS must be an integer from 1 through 50."))
  (unless (and (integerp max-rows) (<= 1 max-rows 250))
    (error "MAX-ROWS must be an integer from 1 through 250."))
  (let* ((all-turn-ids
           (remove-duplicates
            (loop for row in (%typed-retrieval-list anchors)
                  for metadata = (and (hash-table-p row)
                                      (gethash "epistemic_metadata" row))
                  for turn-id = (and (hash-table-p metadata)
                                     (gethash "turn_id" metadata))
                  when (and (stringp turn-id) (plusp (length turn-id)))
                    collect turn-id)
            :test #'string= :from-end t))
         (turn-ids (subseq all-turn-ids 0 (min max-turns
                                               (length all-turn-ids))))
         (anchor-by-id (make-hash-table :test #'equal)))
    (dolist (anchor (%typed-retrieval-list anchors))
      (when (hash-table-p anchor)
        (setf (gethash (gethash "id" anchor) anchor-by-id) anchor)))
    (when turn-ids
      (unless (and (boundp '*retrieval-embedding-mode*)
                   (eq *retrieval-embedding-mode* :enforced))
        (error "Turn-neighborhood retrieval requires enforced typed embeddings."))
      (when *memory-search-storage-backend*
        (let* ((qvec
                 (%memory-retrieval-time
                  "memory_query_embedding"
                  (lambda ()
                    (if *memory-search-query-vector-fn*
                        (funcall *memory-search-query-vector-fn* query t)
                        (embed-retrieval-query query)))))
               (weights (if *memory-affect-modulator-fn*
                            (funcall *memory-affect-modulator-fn*
                                     *memory-recall-weights*)
                            *memory-recall-weights*))
               (report
                 (%memory-retrieval-time
                  "memory_neighborhood_scan"
                  (lambda ()
                    (memory-storage-exact-search
                     *memory-search-storage-backend*
                     (make-memory-exact-query
                      :vector-values qvec :profile "turn-neighborhood-v1"
                      :turn-ids turn-ids :limit max-rows :hydrate-p t
                      :include-vector-p t))))))
          (return-from memory-search-turn-neighborhood
            (map 'list
                 (lambda (candidate)
                   (let* ((result
                            (%typed-retrieval-storage-row-object
                             candidate weights))
                          (id (gethash "id" result))
                          (anchor (gethash id anchor-by-id)))
                     (setf (gethash "retrieval_embedding" result)
                           (coerce (gethash "vector" candidate) 'list))
                     (when anchor
                       (dolist (key '("candidate_sources" "lexical_tier"
                                      "lexical_match_count"
                                      "lexical_coverage" "lexical_terms"))
                         (multiple-value-bind (value present-p)
                             (gethash key anchor)
                           (when present-p
                             (setf (gethash key result) value)))))
                     result))
                 (gethash "results" report)))))
      (let* ((qvec (if *memory-search-query-vector-fn*
                       (funcall *memory-search-query-vector-fn* query t)
                       (embed-retrieval-query query)))
             (qlit (%vector-literal qvec))
             (params (append turn-ids (list max-rows)))
             (turn-placeholders
               (loop for index from 1 to (length turn-ids)
                     collect (format nil "$~d" index)))
             (limit-placeholder (format nil "$~d" (1+ (length turn-ids))))
             (sql
               (format nil
                       "SELECT id,kind,content,created_at::text,last_accessed::text,access_count,importance,valence,arousal_at_encoding,activation,source_event_id,origin_class,epistemic_status,producer,model_purpose,confidence,grounding_status,root_observation_ids::text,generation_id,supersedes_node_id,quarantined,epistemic_metadata::text,retrieval_embedding::text,1-(retrieval_embedding <=> '~a'::vector) AS sim FROM memory_nodes WHERE is_cold=false AND quarantined=false AND retrieval_embedding IS NOT NULL AND origin_class IN ('lived-user','lived-agent-action','tool-result','external-source') AND epistemic_status NOT IN ('legacy-unclassified','rejected') AND grounding_status IN ('grounded','partially-grounded') AND epistemic_metadata->>'turn_id' IN (~{~a~^,~}) ORDER BY retrieval_embedding <=> '~a'::vector,id LIMIT ~a"
                       qlit turn-placeholders qlit limit-placeholder))
             (rows (with-pg (%typed-retrieval-query sql params)))
             (weights (if *memory-affect-modulator-fn*
                          (funcall *memory-affect-modulator-fn*
                                   *memory-recall-weights*)
                          *memory-recall-weights*))
             (w-sim (gethash "sim" weights))
             (w-imp (gethash "imp" weights))
             (w-rec (gethash "rec" weights))
             (w-act (gethash "act" weights)))
        (mapcar
         (lambda (row)
           (destructuring-bind
               (id kind content created last-accessed access-count importance
                   valence arousal activation source-event-id origin-class
                   epistemic-status producer model-purpose confidence
                   grounding-status roots-text generation-id supersedes-node-id
                   quarantined metadata-text embedding-text sim)
               row
             (let* ((similarity (if (numberp sim) sim 0.0d0))
                    (recency (%recency-score created))
                    (score (+ (* w-sim similarity) (* w-imp importance)
                              (* w-rec recency) (* w-act activation)))
                    (result
                      (obj "id" id "kind" kind "content" content
                           "created_at" created "last_accessed" last-accessed
                           "access_count" access-count "importance" importance
                           "valence" valence "arousal_at_encoding" arousal
                           "activation" activation
                           "source_event_id" (or source-event-id :null)
                           "origin_class" origin-class
                           "epistemic_status" epistemic-status
                           "producer" (or producer :null)
                           "model_purpose" (or model-purpose :null)
                           "confidence" (or confidence :null)
                           "grounding_status" grounding-status
                           "root_observation_ids"
                           (%typed-retrieval-json roots-text (vector))
                           "generation_id" (or generation-id :null)
                           "supersedes_node_id" (or supersedes-node-id :null)
                           "quarantined" (if quarantined t nil)
                           "epistemic_metadata"
                           (%typed-retrieval-json metadata-text (obj))
                           "retrieval_embedding" (%parse-pg-vector embedding-text)
                           "retrieval_score" score "similarity" similarity
                           "recency_score" recency))
                    (anchor (gethash id anchor-by-id)))
               (when anchor
                 (dolist (key '("candidate_sources" "lexical_tier"
                                "lexical_match_count" "lexical_coverage"
                                "lexical_terms"))
                   (multiple-value-bind (value present-p) (gethash key anchor)
                     (when present-p (setf (gethash key result) value)))))
               result)))
         rows)))))

(defun memory-record-use (node-ids &key consumer generation-id user-visible-p)
  "Explicitly account for use of NODE-IDS. Background/private use is audited
but never changes node rehearsal metadata. User-visible use increments access
metadata and activation by a small bounded amount, capped at 0.85."
  (let* ((ids (%typed-retrieval-string-values node-ids))
         (consumer-name (and consumer (string-downcase (string consumer))))
         (report nil))
    (unless (and consumer-name (plusp (length consumer-name)))
      (error "MEMORY-RECORD-USE requires a consumer."))
    (setf report
          (if (and user-visible-p ids)
              (memory-cognitive-mutation-dispatch
               "user-visible-rehearsal"
               (obj "node_ids" (coerce ids 'vector)
                    "consumer" consumer-name
                    "generation_id" (or generation-id :null))
               (lambda ()
                 (let ((updated 0))
                   (%call-with-memory-durable-event-buffer
                    (lambda ()
                      (with-pg
                        (pomo:with-transaction ()
                          (dolist (id ids)
                            (let ((changed
                                    (or (pomo:execute
                                         "UPDATE memory_nodes SET last_accessed=now(), access_count=access_count+1, activation=CASE WHEN activation >= $1 THEN activation ELSE LEAST($1, activation+$2) END WHERE id=$3"
                                         *typed-retrieval-user-visible-activation-cap*
                                         *typed-retrieval-user-visible-activation-increment*
                                         id)
                                        0)))
                              (incf updated changed)
                              (when (plusp changed)
                                (%memory-queue-node-state-current-connection
                                 id "update"
                                 "user-visible-rehearsal"))))))))
                   (obj "consumer" consumer-name
                        "generation_id" (or generation-id :null)
                        "user_visible" t
                        "requested_count" (length ids)
                        "updated_count" updated))))
              (obj "consumer" consumer-name
                   "generation_id" (or generation-id :null)
                   "user_visible" (if user-visible-p t nil)
                   "requested_count" (length ids)
                   "updated_count" 0)))
    (setf report (%memory-operation-return-value "use-report" report))
    (%typed-retrieval-record-stat
     (if user-visible-p "user-visible-use" "background-use") (length ids))
    (when (fboundp 'log-event)
      (ignore-errors
        (funcall 'log-event "memory-use-recorded" report)))
    report))

;;; MEMORY-RECALL remains behaviorally identical. This reload-safe facade only
;;; emits a content-free, rate-limited signal that legacy mutating retrieval is
;;; still in use, which provides the migration denominator for STAB-02.
(defvar *typed-retrieval-installed-memory-recall-wrapper* nil)
(let ((current (and (fboundp 'memory-recall) (fdefinition 'memory-recall))))
  (when (and current
             (not (eq current *typed-retrieval-installed-memory-recall-wrapper*)))
    (setf (fdefinition 'pai-base-memory-recall-typed-retrieval) current)))

(defun %typed-retrieval-legacy-memory-recall (&rest args)
  (let ((result (apply 'pai-base-memory-recall-typed-retrieval args))
        (emit-count nil))
    (bt:with-lock-held (*typed-retrieval-stats-lock*)
      (incf *typed-retrieval-pending-legacy-calls*)
      (let ((now (get-universal-time)))
        (when (>= (- now *typed-retrieval-last-legacy-event-at*)
                  *typed-retrieval-legacy-event-interval-seconds*)
          (setf emit-count *typed-retrieval-pending-legacy-calls*
                *typed-retrieval-pending-legacy-calls* 0
                *typed-retrieval-last-legacy-event-at* now))))
    (when (and emit-count (fboundp 'log-event))
      (ignore-errors
        (funcall 'log-event "legacy-memory-recall-used"
                 (obj "call_count" emit-count
                      "rate_window_seconds"
                      *typed-retrieval-legacy-event-interval-seconds*))))
    result))

(setf (fdefinition 'memory-recall) #'%typed-retrieval-legacy-memory-recall
      *typed-retrieval-installed-memory-recall-wrapper*
      (fdefinition 'memory-recall))
