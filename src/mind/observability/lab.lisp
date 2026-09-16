;;;; lab.lisp -- reproducible, capability-bounded the agent scenarios.

(in-package :agent)

(export '(pai-lab-run-suite pai-lab-run-scenario-file
          pai-lab-capability-report pai-lab-prove-database-read-only
          pai-lab-load-curator-response-file
          pai-lab-load-atom-response-file))

(defparameter *pai-lab-schema-version* 1)
(defparameter *pai-lab-max-scenarios* 50)
(defparameter *pai-lab-max-query-chars* 2000)
(defparameter *pai-lab-max-candidate-output* 15)
(defparameter *pai-lab-atom-max-atoms* 12)
(defparameter *pai-lab-atom-forms* '("episodic" "semantic" "procedural"))
(defvar *pai-lab-reveal-private-content* nil
  "Dynamically true only for an explicit local private-review invocation.")
(defvar *pai-lab-embed-text-fn*
  (and (fboundp 'embed-text) (fdefinition 'embed-text)))
(defvar *pai-lab-batch-embed-texts-fn* nil)
(defvar *pai-lab-corpus-read-fn* nil)
(defvar *pai-lab-experiment-embedding-cache* nil)
(defvar *pai-lab-experiment-local-embedding-calls* 0)
(defvar *pai-lab-experiment-local-embedding-requests* 0)
(defvar *pai-lab-curator-responses* nil)
(defvar *pai-lab-atom-responses* nil)
(defvar *pai-lab-deliverable-read-fn*
  (and (fboundp 'read-deliverable) (fdefinition 'read-deliverable)))

(defparameter *pai-lab-rerank-experiments*
  '("nomic-query-document" "nomic-prefixed-full-scan"
    "nomic-turn-bundle-neighborhood-v1"))
(defparameter *pai-lab-curator-experiments*
  '("captured-memory-evidence-v1" "captured-turn-bundle-evidence-v2"))
(defparameter *pai-lab-batch-embedding-size* 16)

(defun %pai-lab-atom-exact-keys (table allowed label)
  (unless (hash-table-p table) (error "~a is not an object." label))
  (loop for key being the hash-keys of table
        unless (member key allowed :test #'string=)
          do (error "Unknown ~a key ~a." label key))
  (dolist (key allowed)
    (unless (nth-value 1 (gethash key table))
      (error "Missing required ~a key ~a." label key)))
  table)

(defun %pai-lab-json-object (text)
  (if (and (stringp text) (plusp (length text)))
      (handler-case
          (let ((value (shasht:read-json text)))
            (if (hash-table-p value) value (obj)))
        (error () (obj)))
      (obj)))

(defun %pai-lab-default-read-only-transaction (thunk)
  (pomo:with-connection
      (list *pg-database* *pg-user* *pg-password* *pg-host* :port *pg-port*)
    (pomo:execute
     "BEGIN TRANSACTION ISOLATION LEVEL REPEATABLE READ READ ONLY")
    (unwind-protect
         (progn
           (unless (string-equal "on"
                                 (pomo:query "SHOW transaction_read_only"
                                             :single))
             (error "Database did not enter a read-only transaction."))
           (let ((*pai-pg-reuse-current-transaction-p* t))
             (funcall thunk)))
      ;; A successful return means rollback itself succeeded. Never emit a
      ;; false rollback claim after silently swallowing a connection error.
      (pomo:execute "ROLLBACK"))))

(defun pai-lab-prove-database-read-only ()
  "Return true only when PostgreSQL rejects a harmless UPDATE inside an
explicit READ ONLY transaction. WHERE FALSE plus unconditional rollback makes
the probe zero-row and non-mutating even if a server is misconfigured."
  (pomo:with-connection
      (list *pg-database* *pg-user* *pg-password* *pg-host* :port *pg-port*)
    (pomo:execute
     "BEGIN TRANSACTION ISOLATION LEVEL REPEATABLE READ READ ONLY")
    (let ((blocked nil))
      (unwind-protect
           (handler-case
               (pomo:execute
                "UPDATE memory_nodes SET content = content WHERE FALSE")
             (error () (setf blocked t)))
        (pomo:execute "ROLLBACK"))
      blocked)))

(defun %pai-lab-default-corpus-read (as-of)
  "Read the same direct, safe memory classes considered by public selection.
The caller owns the already-open read-only transaction."
  (let* ((base
           "SELECT id,kind,content,created_at::text,COALESCE(to_jsonb(memory_nodes)->>'observed_at',created_at::text),to_jsonb(memory_nodes)->>'valid_from',to_jsonb(memory_nodes)->>'valid_to',supersedes_node_id,origin_class,epistemic_status,grounding_status,epistemic_metadata::text FROM memory_nodes WHERE is_cold=false AND quarantined=false AND origin_class IN ('lived-user','lived-agent-action','tool-result','external-source') AND epistemic_status NOT IN ('legacy-unclassified','rejected') AND grounding_status IN ('grounded','partially-grounded')")
         (rows (if as-of
                   (pomo:query
                    (concatenate 'string base
                                 " AND created_at <= $1::timestamptz ORDER BY id")
                    as-of)
                   (pomo:query (concatenate 'string base " ORDER BY id")))))
    (mapcar
     (lambda (row)
       (destructuring-bind
           (id kind content created-at observed-at valid-from valid-to
               supersedes-node-id origin status grounding metadata-text) row
         (obj "id" id "kind" kind "content" content
               "created_at" created-at
               "observed_at" observed-at
               "valid_from" (or valid-from :null)
               "valid_to" (or valid-to :null)
               "supersedes_node_id" (or supersedes-node-id :null)
               "origin_class" origin "epistemic_status" status
               "grounding_status" grounding "quarantined" nil
               "epistemic_metadata"
               (%pai-lab-json-object metadata-text))))
     rows)))

(defun %pai-lab-batch-endpoint ()
  (let ((suffix "/api/embeddings"))
    (if (and (>= (length *ollama-endpoint*) (length suffix))
             (string= suffix *ollama-endpoint*
                      :start2 (- (length *ollama-endpoint*) (length suffix))))
        (concatenate 'string
                     (subseq *ollama-endpoint*
                             0 (- (length *ollama-endpoint*) (length suffix)))
                     "/api/embed")
        "http://pai-ollama:11434/api/embed")))

(defun %pai-lab-default-batch-embed-texts (texts)
  (let* ((payload (obj "model" *ollama-embed-model*
                       "input" (coerce texts 'vector)))
         (response
           (shasht:read-json
            (dex:post (%pai-lab-batch-endpoint)
                      :headers '(("Content-Type" . "application/json"))
                      :connect-timeout *ollama-timeout*
                      :read-timeout 120
                      :content (shasht:write-json payload nil))))
         (embeddings (gethash "embeddings" response)))
    (unless (and (vectorp embeddings)
                 (= (length embeddings) (length texts)))
      (error "Ollama batch embedding count mismatch."))
    (map 'list (lambda (embedding) (coerce embedding 'list)) embeddings)))

(defvar *pai-lab-read-only-transaction-fn*
  #'%pai-lab-default-read-only-transaction)
(defvar *pai-lab-write-probe-fn* #'pai-lab-prove-database-read-only)
(defvar *pai-lab-memory-search-fn*
  (and (fboundp 'memory-search) (fdefinition 'memory-search)))

(unless *pai-lab-corpus-read-fn*
  (setf *pai-lab-corpus-read-fn* #'%pai-lab-default-corpus-read))
(unless *pai-lab-batch-embed-texts-fn*
  (setf *pai-lab-batch-embed-texts-fn*
        #'%pai-lab-default-batch-embed-texts))

(defun %pai-lab-list (value)
  (cond ((null value) nil)
        ((listp value) value)
        ((vectorp value) (coerce value 'list))
        (t (list value))))

(defun %pai-lab-nonempty-string-p (value)
  (and (stringp value)
       (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                    value)))))

(defun %pai-lab-key-present-p (table key)
  (nth-value 1 (gethash key table)))

(defun %pai-lab-content-patterns (scenario)
  (%pai-lab-list
   (gethash "expected_selected_content_patterns" scenario)))

(defun %pai-lab-rerank-experiments (scenario)
  (%pai-lab-list (gethash "rerank_experiments" scenario)))

(defun %pai-lab-curator-experiments (scenario)
  (%pai-lab-list (gethash "curator_experiments" scenario)))

(defun pai-lab-load-curator-response-file (path)
  (let ((bundle (shasht:read-json (uiop:read-file-string path))))
    (unless (and (hash-table-p bundle)
                 (= (gethash "schema_version" bundle -1) 1)
                 (hash-table-p (gethash "responses" bundle)))
      (error "Curator response bundle must use schema version 1."))
    (setf *pai-lab-curator-responses* (gethash "responses" bundle))))

(defun pai-lab-load-atom-response-file (path)
  (let ((bundle (shasht:read-json (uiop:read-file-string path))))
    (unless (and (hash-table-p bundle)
                 (= (gethash "schema_version" bundle -1) 1)
                 (hash-table-p (gethash "responses" bundle)))
      (error "Atom response bundle must use schema version 1."))
    (setf *pai-lab-atom-responses* (gethash "responses" bundle))))

(defun %pai-lab-validate-content-patterns (scenario)
  (let ((patterns (%pai-lab-content-patterns scenario)))
    (when (> (length patterns) 20)
      (error "At most 20 private semantic-oracle patterns are allowed."))
    (dolist (pattern patterns)
      (let ((terms (%pai-lab-list pattern)))
        (unless (and terms (<= (length terms) 10)
                     (every (lambda (term)
                              (and (%pai-lab-nonempty-string-p term)
                                   (<= (length term) 200)))
                            terms))
          (error "Each semantic-oracle pattern needs 1-10 bounded terms."))))
    patterns))

(defun %pai-lab-validate-scenario (scenario)
  (unless (hash-table-p scenario) (error "Each scenario must be an object."))
  (let ((id (gethash "id" scenario))
        (mode (gethash "mode" scenario))
        (query (gethash "query" scenario))
        (limit (gethash "result_limit" scenario 3)))
    (unless (%pai-lab-nonempty-string-p id)
      (error "Scenario id is required."))
    (unless (string= mode "read-only-live")
      (error "First-slice lab scenarios must use read-only-live mode."))
    (unless (and (%pai-lab-nonempty-string-p query)
                 (<= (length query) *pai-lab-max-query-chars*))
      (error "Scenario query is empty or too large."))
    (unless (and (integerp limit) (<= 1 limit 5))
      (error "Scenario result_limit must be from 1 through 5."))
    (let ((as-of (gethash "as_of" scenario)))
      (when as-of
        (unless (and (%pai-lab-nonempty-string-p as-of)
                     (<= (length as-of) 100))
          (error "Scenario as_of must be a bounded PostgreSQL timestamp."))))
    (dolist (forbidden '("deliver" "send" "tick" "persist" "write"
                         "provider_api_key" "transport"))
      (when (%pai-lab-key-present-p scenario forbidden)
        (error "Authority-bearing field ~a is forbidden." forbidden)))
    (%pai-lab-validate-content-patterns scenario)
    (dolist (experiment (%pai-lab-rerank-experiments scenario))
      (unless (and (stringp experiment)
                   (member experiment *pai-lab-rerank-experiments*
                           :test #'string=))
        (error "Unknown or invalid rerank experiment ~s." experiment)))
    (dolist (experiment (%pai-lab-curator-experiments scenario))
      (unless (and (stringp experiment)
                   (member experiment *pai-lab-curator-experiments*
                           :test #'string=))
        (error "Unknown or invalid curator experiment ~s." experiment)))
    (when (and (member "captured-memory-evidence-v1"
                       (%pai-lab-curator-experiments scenario)
                       :test #'string=)
               (not (member "nomic-prefixed-full-scan"
                            (%pai-lab-rerank-experiments scenario)
                            :test #'string=)))
      (error "Atomic curator experiment requires nomic-prefixed-full-scan."))
    (when (and (member "captured-turn-bundle-evidence-v2"
                       (%pai-lab-curator-experiments scenario)
                       :test #'string=)
               (not (member "nomic-turn-bundle-neighborhood-v1"
                            (%pai-lab-rerank-experiments scenario)
                            :test #'string=)))
      (error "Turn-bundle curator experiment requires its bundle arm."))
    (let ((expected (gethash "expected_curator_decision" scenario)))
      (when (and expected
                 (not (member expected '("SELECT" "NO_EXTRA_CONTEXT")
                              :test #'string=)))
        (error "Invalid expected curator decision.")))
    scenario))

(defun %pai-lab-row-member-p (row rows)
  (member row rows :test #'eq))

(defun %pai-lab-selector-diagnostics
    (rows eligible content-safe non-echo selected limit)
  "Explain the production selector without changing its decision."
  (let* ((table (make-hash-table :test #'equal))
         (direct (remove-if-not #'%context-projection-direct-memory-p
                                non-echo))
         (ranked (stable-sort (copy-list direct) #'>
                              :key #'%context-projection-public-memory-score))
         (best (and ranked
                    (or (gethash "similarity" (first ranked)) 0.0d0))))
    (loop for row in rows
          for retrieval-rank from 1
          for id = (gethash "id" row)
          for selector-position = (position row ranked :test #'eq)
          for selector-rank = (and selector-position (1+ selector-position))
          for similarity = (or (gethash "similarity" row) 0.0d0)
          for floor-pass = (>= similarity
                               *context-projection-memory-similarity-floor*)
          for window-pass = (or (null best)
                                (>= similarity
                                    (- best
                                       *context-projection-memory-best-window*)))
          for reason =
            (cond
              ((not (%pai-lab-row-member-p row eligible))
               "eligibility-filtered")
              ((not (%pai-lab-row-member-p row content-safe))
               "sensitive-content-filtered")
              ((not (%pai-lab-row-member-p row non-echo))
               "current-query-echo-filtered")
              ((not (%context-projection-direct-memory-p row))
               "not-direct-evidence")
              ((not floor-pass) "below-similarity-floor")
              ((not window-pass) "outside-best-score-window")
              ((%pai-lab-row-member-p row selected) "selected")
              ((and selector-rank (> selector-rank limit))
               "beyond-result-limit")
              (t "not-selected"))
          do (setf (gethash id table)
                   (obj "retrieval_rank" retrieval-rank
                        "selector_rank" (or selector-rank :null)
                        "selection_reason" reason
                        "similarity_floor_pass" (if floor-pass t nil)
                        "best_window_pass" (if window-pass t nil))))
    table))

(defun %pai-lab-result-row (row rank selected-ids selector-diagnostics)
  (let ((result
          (obj "rank" rank
               "retrieval_rank" rank
               "id" (gethash "id" row)
               "kind" (gethash "kind" row)
               "origin_class" (gethash "origin_class" row)
               "epistemic_status" (gethash "epistemic_status" row)
               "grounding_status" (gethash "grounding_status" row)
               "observed_at" (or (gethash "observed_at" row)
                                 (gethash "created_at" row)
                                 :null)
               "valid_from" (gethash "valid_from" row :null)
               "valid_to" (gethash "valid_to" row :null)
               "supersedes_node_id"
               (gethash "supersedes_node_id" row :null)
               "similarity" (or (gethash "similarity" row) :null)
               "retrieval_score" (or (gethash "retrieval_score" row) :null)
               "importance" (or (gethash "importance" row) :null)
               "recency_score" (or (gethash "recency_score" row) :null)
               "activation" (or (gethash "activation" row) :null)
               "public_score" (%context-projection-public-memory-score row)
               "selected" (if (member (gethash "id" row) selected-ids
                                        :test #'string=) t nil))))
    (let ((diagnostic (gethash (gethash "id" row) selector-diagnostics)))
      (when diagnostic
        (setf (gethash "selector_rank" result)
              (gethash "selector_rank" diagnostic)
              (gethash "selection_reason" result)
              (gethash "selection_reason" diagnostic))))
    (when *pai-lab-reveal-private-content*
      (setf (gethash "content" result) (gethash "content" row "")))
    result))

(defun %pai-lab-pattern-matches-content-p (pattern content)
  (let ((haystack (string-downcase (or content ""))))
    (every (lambda (term)
             (search (string-downcase term) haystack :test #'char=))
           (%pai-lab-list pattern))))

(defun %pai-lab-content-matches (patterns rows)
  (if patterns
      (remove-if-not
       (lambda (row)
         (some (lambda (pattern)
                 (%pai-lab-pattern-matches-content-p
                  pattern (gethash "content" row "")))
               patterns))
       rows)
      nil))

(defun %pai-lab-match-diagnostic
    (row candidates selected best-direct &optional selector-diagnostics)
  (let* ((id (gethash "id" row))
         (similarity (or (gethash "similarity" row) 0.0d0))
         (direct (%context-projection-direct-memory-p row)))
    (let ((result
            (obj "id" id
                 "candidate_rank"
                 (1+ (or (position row candidates :test #'eq) -1))
                 "kind" (gethash "kind" row)
                 "origin_class" (gethash "origin_class" row)
                 "similarity" similarity
                 "public_score" (%context-projection-public-memory-score row)
                 "direct_memory" (if direct t nil)
                 "similarity_floor_pass"
                 (if (>= similarity
                          *context-projection-memory-similarity-floor*) t nil)
                 "best_window_pass"
                 (if (and direct
                          (or (null best-direct)
                              (>= similarity
                                  (- best-direct
                                     *context-projection-memory-best-window*))))
                     t nil)
                 "selected" (if (member id selected :test #'string=
                                        :key (lambda (item)
                                               (gethash "id" item)))
                                t nil))))
      (let ((selector (and selector-diagnostics
                           (gethash id selector-diagnostics))))
        (when selector
          (setf (gethash "selector_rank" result)
                (gethash "selector_rank" selector)
                (gethash "selection_reason" result)
                (gethash "selection_reason" selector))))
      result)))

(defun %pai-lab-content-oracle
    (scenario candidates selected &optional selector-diagnostics)
  "Apply an explicit test-only oracle without returning its terms or content.
This measures a fixture expectation; it never changes runtime retrieval."
  (let* ((patterns (%pai-lab-content-patterns scenario))
         (minimum (gethash "minimum_selected_content_matches" scenario
                           (if patterns 1 0)))
         (candidate-matches (%pai-lab-content-matches patterns candidates))
         (selected-matches (%pai-lab-content-matches patterns selected))
         (direct (remove-if-not #'%context-projection-direct-memory-p
                                candidates))
         (ranked-direct
           (stable-sort (copy-list direct) #'>
                        :key #'%context-projection-public-memory-score))
         (best-direct
           (and ranked-direct
                (or (gethash "similarity" (first ranked-direct)) 0.0d0))))
    (unless (and (integerp minimum) (<= 0 minimum 5))
      (error "minimum_selected_content_matches must be from 0 through 5."))
    (obj "enabled" (if patterns t nil)
         "passed" (if (>= (length selected-matches) minimum) t nil)
         "minimum_matches" minimum
         "candidate_match_count" (length candidate-matches)
         "candidate_matched_ids"
         (coerce (mapcar (lambda (row) (gethash "id" row)) candidate-matches)
                 'vector)
         "candidate_matches"
         (coerce (mapcar (lambda (row)
                           (%pai-lab-match-diagnostic
                            row candidates selected best-direct
                            selector-diagnostics))
                         candidate-matches)
                 'vector)
         "selected_match_count" (length selected-matches)
         "matched_ids"
         (coerce (mapcar (lambda (row) (gethash "id" row)) selected-matches)
                 'vector))))

(defun %pai-lab-expectations
    (scenario candidates selected &optional selector-diagnostics)
  (let* ((ids (mapcar (lambda (row) (gethash "id" row)) selected))
         (expected (%pai-lab-list
                    (gethash "expected_selected_ids" scenario)))
         (minimum (gethash "minimum_selected" scenario 0))
         (maximum (gethash "maximum_selected" scenario 5))
         (missing (remove-if (lambda (id) (member id ids :test #'string=))
                             expected))
         (content-oracle
           (%pai-lab-content-oracle
            scenario candidates selected selector-diagnostics)))
    (obj "passed" (if (and (null missing)
                            (<= minimum (length selected) maximum)
                            (gethash "passed" content-oracle)) t nil)
         "minimum_selected" minimum
         "maximum_selected" maximum
         "missing_expected_ids" (coerce missing 'vector)
         "content_oracle" content-oracle)))

(defun %pai-lab-experiment-embed (text)
  (unless *pai-lab-experiment-embedding-cache*
    (setf *pai-lab-experiment-embedding-cache*
          (make-hash-table :test #'equal)))
  (multiple-value-bind (value found)
      (gethash text *pai-lab-experiment-embedding-cache*)
    (if found
        value
        (let ((embedding (funcall *pai-lab-embed-text-fn* text)))
          (incf *pai-lab-experiment-local-embedding-calls*)
          (incf *pai-lab-experiment-local-embedding-requests*)
          (setf (gethash text *pai-lab-experiment-embedding-cache*) embedding)
          embedding))))

(defun %pai-lab-chunks (items size)
  (loop for tail on items by (lambda (rest) (nthcdr size rest))
        while tail
        collect (subseq tail 0 (min size (length tail)))))

(defun %pai-lab-experiment-embed-many (texts)
  (unless *pai-lab-experiment-embedding-cache*
    (setf *pai-lab-experiment-embedding-cache*
          (make-hash-table :test #'equal)))
  (let ((missing
          (remove-duplicates
           (remove-if (lambda (text)
                        (nth-value
                         1 (gethash text
                                    *pai-lab-experiment-embedding-cache*)))
                      texts)
           :test #'string=)))
    (dolist (chunk (%pai-lab-chunks missing *pai-lab-batch-embedding-size*))
      (let ((embeddings (funcall *pai-lab-batch-embed-texts-fn* chunk)))
        (unless (= (length embeddings) (length chunk))
          (error "Batch embedding adapter returned the wrong count."))
        (incf *pai-lab-experiment-local-embedding-requests*)
        (incf *pai-lab-experiment-local-embedding-calls* (length chunk))
        (loop for text in chunk
              for embedding in embeddings
              do (setf (gethash text *pai-lab-experiment-embedding-cache*)
                       embedding))))
    (mapcar (lambda (text)
              (or (gethash text *pai-lab-experiment-embedding-cache*)
                  (error "Missing cached experiment embedding.")))
            texts)))

(defun %pai-lab-experimental-public-score (row similarity)
  (+ similarity
     (cond ((string= (gethash "origin_class" row "") "lived-user") 0.03d0)
           ((member (gethash "origin_class" row "")
                    '("tool-result" "external-source") :test #'string=)
            0.02d0)
           (t 0.0d0))))

(defun %pai-lab-curator-experiments-pass-p (experiments)
  "Awaiting captured output is structurally neutral; a captured failure is red."
  (every (lambda (experiment)
           (member (gethash "status" experiment)
                   '("passed" "awaiting-captured-output") :test #'string=))
         (%pai-lab-list experiments)))

(defun %pai-lab-rerank-experiments-pass-p (experiments)
  (every (lambda (experiment)
           (and (string= "passed" (gethash "status" experiment))
                (%pai-lab-curator-experiments-pass-p
                 (gethash "curator_experiments" experiment))))
         (%pai-lab-list experiments)))

(defun %pai-lab-run-nomic-query-document (scenario query candidates limit)
  "Rerank the already-retrieved direct candidates with Nomic's documented
query/document task prefixes. This is an experiment only; it writes no vector."
  (let* ((calls-before *pai-lab-experiment-local-embedding-calls*)
         (requests-before *pai-lab-experiment-local-embedding-requests*)
         (query-vector
           (%pai-lab-experiment-embed (format nil "search_query: ~a" query)))
         (direct (remove-if-not #'%context-projection-direct-memory-p
                                candidates))
         (scored
           (mapcar
            (lambda (row)
              (let* ((document-vector
                       (%pai-lab-experiment-embed
                        (format nil "search_document: ~a"
                                (gethash "content" row ""))))
                     (similarity (cosine-similarity query-vector document-vector)))
                 (list row similarity
                       (%pai-lab-experimental-public-score row similarity)
                       document-vector)))
            direct))
         (ranked (stable-sort scored #'> :key #'third))
         (best (and ranked (second (first ranked))))
         (eligible
           (remove-if-not
            (lambda (entry)
              (let ((similarity (second entry)))
                (and (>= similarity
                         *context-projection-memory-similarity-floor*)
                     (or (null best)
                         (>= similarity
                             (- best
                                *context-projection-memory-best-window*))))))
            ranked))
         (selected-entries (subseq eligible 0 (min limit (length eligible))))
         (selected (mapcar #'first selected-entries))
         (selected-ids (mapcar (lambda (row) (gethash "id" row)) selected))
         (expectations (%pai-lab-expectations scenario candidates selected))
         (rows
           (loop for entry in ranked
                 for experimental-rank from 1
                 for row = (first entry)
                 for similarity = (second entry)
                 for public-score = (third entry)
                 for selected-p = (member row selected :test #'eq)
                 for floor-pass =
                   (>= similarity *context-projection-memory-similarity-floor*)
                 for window-pass =
                   (or (null best)
                       (>= similarity
                           (- best *context-projection-memory-best-window*)))
                 for result =
                   (obj "id" (gethash "id" row)
                        "retrieval_rank"
                        (or (1+ (or (position row candidates :test #'eq) -1))
                            :null)
                        "experimental_rank" experimental-rank
                        "experimental_similarity" similarity
                        "experimental_public_score" public-score
                        "similarity_floor_pass" (if floor-pass t nil)
                        "best_window_pass" (if window-pass t nil)
                        "selected" (if selected-p t nil)
                        "selection_reason"
                        (cond (selected-p "selected")
                              ((not floor-pass) "below-similarity-floor")
                              ((not window-pass) "outside-best-score-window")
                              ((> experimental-rank limit) "beyond-result-limit")
                              (t "not-selected")))
                 do (when *pai-lab-reveal-private-content*
                      (setf (gethash "content" result)
                            (gethash "content" row "")))
                 collect result)))
    (obj "experiment" "nomic-query-document"
         "embedding_contract" "search_query/search_document"
         "status" (if (gethash "passed" expectations) "passed" "failed")
         "candidate_count" (length candidates)
         "direct_candidate_count" (length direct)
         "selected_count" (length selected)
         "selected_ids" (coerce selected-ids 'vector)
         "rows" (coerce rows 'vector)
         "expectations" expectations
         "local_embedding_call_count"
         (- *pai-lab-experiment-local-embedding-calls* calls-before)
         "local_embedding_request_count"
         (- *pai-lab-experiment-local-embedding-requests* requests-before)
         "database_write_count" 0
         "provider_call_count" 0
         "delivery_authority" nil)))

(defun %pai-lab-curator-response (scenario-id)
  (and (hash-table-p *pai-lab-curator-responses*)
       (gethash scenario-id *pai-lab-curator-responses*)))

(defun %pai-lab-curator-selected-rows (validated candidate-rows)
  (let ((ids (%pai-lab-list (gethash "selected_context_ids" validated))))
    (remove-if-not
     (lambda (row) (member (gethash "id" row) ids :test #'string=))
     candidate-rows)))

(defun %pai-lab-run-captured-curator-rows
    (scenario query candidate-rows experiment response-key)
  "Exercise the production-candidate curator contract with captured output.
No model adapter exists in this path."
  (let* ((manifest (context-curator-build-manifest
                     query candidate-rows :tools nil
                     :as-of (or (gethash "as_of" scenario)
                                (get-universal-time))))
         (request (context-curator-build-request manifest))
         (captured (%pai-lab-curator-response response-key))
         (manifest-ids
           (coerce (mapcar (lambda (row) (gethash "id" row)) candidate-rows)
                    'vector)))
    (if (null captured)
        (obj "experiment" experiment
             "status" "awaiting-captured-output"
             "manifest_candidate_count" (length candidate-rows)
             "manifest_candidate_ids" manifest-ids
             "request_messages" (if *pai-lab-reveal-private-content*
                                      request :null)
             "captured_response_available" nil
             "selected_count" 0
             "selected_ids" (vector)
             "compiled_context_block" :null
             "database_write_count" 0 "provider_call_count" 0
             "delivery_authority" nil)
        (handler-case
            (let* ((validated
                     (context-curator-validate-response captured manifest))
                   (selected-rows
                     (%pai-lab-curator-selected-rows
                      validated candidate-rows))
                   (selected-ids
                     (%pai-lab-list
                      (gethash "selected_context_ids" validated)))
                   (decision (gethash "decision" validated))
                   (expected (gethash "expected_curator_decision" scenario))
                   (oracle (%pai-lab-content-oracle
                            scenario candidate-rows selected-rows))
                   (passed (and (or (null expected)
                                    (string= expected decision))
                                (gethash "passed" oracle)))
                   (compiled
                     (context-curator-compile-block validated manifest)))
              (obj "experiment" experiment
                   "status" (if passed "passed" "failed")
                   "manifest_candidate_count" (length candidate-rows)
                   "manifest_candidate_ids" manifest-ids
                   "request_messages" (if *pai-lab-reveal-private-content*
                                            request :null)
                   "captured_response_available" t
                   "validation_status" "valid"
                   "decision" decision
                   "expected_decision" (or expected :null)
                   "selected_count" (length selected-ids)
                   "selected_ids" (coerce selected-ids 'vector)
                   "content_oracle" oracle
                   "validated_response"
                   (if *pai-lab-reveal-private-content* validated :null)
                   "compiled_context_block"
                   (if *pai-lab-reveal-private-content* compiled :null)
                   "compiled_context_characters" (length compiled)
                   "database_write_count" 0 "provider_call_count" 0
                   "delivery_authority" nil))
          (error (condition)
            (obj "experiment" experiment
                 "status" "rejected"
                 "manifest_candidate_count" (length candidate-rows)
                 "manifest_candidate_ids" manifest-ids
                 "request_messages" (if *pai-lab-reveal-private-content*
                                          request :null)
                 "captured_response_available" t
                 "validation_status" "rejected"
                 "validation_error_type"
                 (string-downcase (symbol-name (type-of condition)))
                 "selected_count" 0 "selected_ids" (vector)
                 "compiled_context_block" :null
                 "database_write_count" 0 "provider_call_count" 0
                  "delivery_authority" nil))))))

(defun %pai-lab-run-captured-curator (scenario query ranked)
  (let ((candidate-rows
          (mapcar #'first
                  (subseq ranked 0 (min (length ranked)
                                        *context-curator-max-candidates*)))))
    (%pai-lab-run-captured-curator-rows
     scenario query candidate-rows "captured-memory-evidence-v1"
     (gethash "id" scenario))))

(defun %pai-lab-run-curator-experiments (scenario query ranked)
  (coerce
   (mapcar
    (lambda (experiment)
      (cond ((string= experiment "captured-memory-evidence-v1")
              (%pai-lab-run-captured-curator scenario query ranked))
            (t (error "Unknown atomic curator experiment ~a." experiment))))
    (remove-if-not
     (lambda (experiment)
       (string= experiment "captured-memory-evidence-v1"))
     (%pai-lab-curator-experiments scenario)))
   'vector))

(defun %pai-lab-run-nomic-prefixed-full-scan
    (scenario query original-candidates limit)
  "Build an ephemeral correctly-prefixed index of every eligible direct row.
No vector or score is written back to PostgreSQL or the state mount."
  (let* ((calls-before *pai-lab-experiment-local-embedding-calls*)
         (requests-before *pai-lab-experiment-local-embedding-requests*)
         (as-of (gethash "as_of" scenario))
         (raw-corpus (funcall *pai-lab-corpus-read-fn* as-of))
         (corpus
           (remove-if
            (lambda (row)
              (or (%context-projection-sensitive-memory-content-p row)
                  (let ((content (gethash "content" row "")))
                    (and (stringp content)
                         (search query content :test #'char-equal)))))
            raw-corpus))
         (query-vector
           (%pai-lab-experiment-embed (format nil "search_query: ~a" query)))
         (document-texts
           (mapcar (lambda (row)
                     (format nil "search_document: ~a"
                             (gethash "content" row "")))
                   corpus))
         (document-vectors (%pai-lab-experiment-embed-many document-texts))
         (scored
           (mapcar
            (lambda (row vector)
              (let ((similarity (cosine-similarity query-vector vector)))
                 (list row similarity
                       (%pai-lab-experimental-public-score row similarity)
                       vector)))
            corpus document-vectors))
         (ranked (stable-sort scored #'> :key #'third))
         (best (and ranked (second (first ranked))))
         (eligible
           (remove-if-not
            (lambda (entry)
              (let ((similarity (second entry)))
                (and (>= similarity
                         *context-projection-memory-similarity-floor*)
                     (or (null best)
                         (>= similarity
                             (- best
                                *context-projection-memory-best-window*))))))
            ranked))
         (selected-entries (subseq eligible 0 (min limit (length eligible))))
         (selected (mapcar #'first selected-entries))
         (selected-ids (mapcar (lambda (row) (gethash "id" row)) selected))
         (expectations (%pai-lab-expectations scenario corpus selected))
         (oracle-ids
           (%pai-lab-list
            (gethash "candidate_matched_ids"
                     (gethash "content_oracle" expectations))))
         (report-entries
           (remove-duplicates
            (append (subseq ranked 0 (min 50 (length ranked)))
                    (remove-if-not
                     (lambda (entry)
                       (member (gethash "id" (first entry)) oracle-ids
                               :test #'string=))
                     ranked))
            :test #'string=
            :key (lambda (entry) (gethash "id" (first entry)))))
         (rows
           (mapcar
            (lambda (entry)
              (let* ((row (first entry))
                     (id (gethash "id" row))
                     (similarity (second entry))
                     (public-score (third entry))
                     (rank (1+ (position entry ranked :test #'eq)))
                     (selected-p (member row selected :test #'eq))
                     (floor-pass
                       (>= similarity
                           *context-projection-memory-similarity-floor*))
                     (window-pass
                       (or (null best)
                           (>= similarity
                               (- best
                                  *context-projection-memory-best-window*))))
                     (result
                       (obj "id" id
                            "original_retrieval_rank"
                            (let ((position
                                    (position id original-candidates
                                              :test #'string=
                                              :key (lambda (candidate)
                                                     (gethash "id" candidate)))))
                              (if position (1+ position) :null))
                            "full_scan_rank" rank
                            "experimental_similarity" similarity
                            "experimental_public_score" public-score
                            "similarity_floor_pass" (if floor-pass t nil)
                            "best_window_pass" (if window-pass t nil)
                            "selected" (if selected-p t nil)
                            "selection_reason"
                            (cond (selected-p "selected")
                                  ((not floor-pass) "below-similarity-floor")
                                  ((not window-pass)
                                   "outside-best-score-window")
                                  ((> rank limit) "beyond-result-limit")
                                  (t "not-selected")))))
                (when *pai-lab-reveal-private-content*
                  (setf (gethash "content" result)
                        (gethash "content" row "")))
                result))
            report-entries))
         (curator-experiments
           (%pai-lab-run-curator-experiments scenario query ranked)))
    (obj "experiment" "nomic-prefixed-full-scan"
         "embedding_contract" "search_query/search_document"
         "ephemeral_index" t
         "status" (if (and (gethash "passed" expectations)
                            (%pai-lab-curator-experiments-pass-p
                             curator-experiments))
                       "passed" "failed")
         "raw_corpus_count" (length raw-corpus)
         "corpus_count" (length corpus)
         "pre_embedding_filtered_count" (- (length raw-corpus) (length corpus))
         "selected_count" (length selected)
         "selected_ids" (coerce selected-ids 'vector)
         "reported_row_count" (length rows)
         "rows" (coerce rows 'vector)
         "curator_experiments" curator-experiments
         "expectations" expectations
         "local_embedding_call_count"
         (- *pai-lab-experiment-local-embedding-calls* calls-before)
         "local_embedding_request_count"
         (- *pai-lab-experiment-local-embedding-requests* requests-before)
         "database_write_count" 0
          "provider_call_count" 0
          "delivery_authority" nil)))

(defun %pai-lab-run-nomic-turn-bundle-neighborhood
    (scenario query original-candidates limit)
  "Expand the bounded production semantic hits to same-turn neighborhoods.
Only anchors and their captured siblings receive local experimental vectors;
the full corpus is read solely to locate structural neighbors. No provider or
production adapter is reachable."
  (declare (ignore limit))
  (let* ((calls-before *pai-lab-experiment-local-embedding-calls*)
         (requests-before *pai-lab-experiment-local-embedding-requests*)
         (as-of (gethash "as_of" scenario))
         (raw-corpus (funcall *pai-lab-corpus-read-fn* as-of))
         (anchor-turn-ids
           (remove-duplicates
            (remove nil (mapcar #'turn-bundle-row-turn-id
                                original-candidates))
            :test #'string=))
         (neighborhood
           (remove-duplicates
            (append
             original-candidates
             (remove-if-not
              (lambda (row)
                (let ((turn-id (turn-bundle-row-turn-id row)))
                  (and turn-id
                       (member turn-id anchor-turn-ids :test #'string=))))
              raw-corpus))
            :test #'string=
            :key (lambda (row) (gethash "id" row))))
         (corpus
           (remove-if
            (lambda (row)
              (or (%context-projection-sensitive-memory-content-p row)
                  (let ((content (gethash "content" row "")))
                    (and (stringp content)
                         (search query content :test #'char-equal)))))
            neighborhood))
         (query-vector
           (%pai-lab-experiment-embed (format nil "search_query: ~a" query)))
         (document-texts
           (mapcar (lambda (row)
                     (format nil "search_document: ~a"
                             (gethash "content" row "")))
                   corpus))
         (document-vectors (%pai-lab-experiment-embed-many document-texts))
         (scored
           (mapcar
            (lambda (row vector)
              (let ((similarity (cosine-similarity query-vector vector)))
                (list row similarity
                      (%pai-lab-experimental-public-score row similarity)
                      vector)))
            corpus document-vectors))
         (ranked (stable-sort scored #'> :key #'third)))
    (multiple-value-bind (bundles suppressed considered)
        (turn-bundle-build-candidates ranked corpus)
      (let* ((bundle-ids
               (mapcar (lambda (row) (gethash "id" row)) bundles))
             (oracle (%pai-lab-content-oracle scenario bundles bundles))
             (bundle-curator-requested
               (member "captured-turn-bundle-evidence-v2"
                       (%pai-lab-curator-experiments scenario)
                       :test #'string=))
             (curator-experiments
               (if bundle-curator-requested
                   (vector
                    (%pai-lab-run-captured-curator-rows
                     scenario query bundles
                     "captured-turn-bundle-evidence-v2"
                     (format nil "~a::turn-bundle-v2"
                             (gethash "id" scenario))))
                   (vector)))
             (rows
               (mapcar
                (lambda (row)
                  (let ((result
                          (obj "id" (gethash "id" row)
                               "turn_id" (gethash "turn_id" row :null)
                               "anchor_id" (gethash "anchor_id" row)
                               "member_count" (gethash "member_count" row)
                               "member_roles" (gethash "member_roles" row)
                               "evidence_node_ids"
                               (gethash "evidence_node_ids" row)
                               "observed_at" (gethash "observed_at" row :null)
                               "valid_from" (gethash "valid_from" row :null)
                               "valid_to" (gethash "valid_to" row :null)
                               "supersedes_node_id"
                               (gethash "supersedes_node_id" row :null)
                               "experimental_similarity"
                               (gethash "similarity" row)
                               "original_anchor_retrieval_rank"
                               (let ((position
                                       (position
                                        (gethash "anchor_id" row)
                                        original-candidates :test #'string=
                                        :key (lambda (candidate)
                                               (gethash "id" candidate)))))
                                 (if position (1+ position) :null)))))
                    (when *pai-lab-reveal-private-content*
                      (setf (gethash "content" result)
                            (gethash "content" row "")))
                    result))
                bundles)))
        (obj "experiment" "nomic-turn-bundle-neighborhood-v1"
             "embedding_contract" "search_query/search_document"
             "ephemeral_index" t
             "status" (if (and (gethash "passed" oracle)
                                (%pai-lab-curator-experiments-pass-p
                                 curator-experiments))
                          "passed" "failed")
              "raw_corpus_count" (length raw-corpus)
              "semantic_anchor_count" (length original-candidates)
              "anchor_turn_count" (length anchor-turn-ids)
              "neighborhood_row_count" (length neighborhood)
              "corpus_count" (length corpus)
             "anchored_bundle_count" considered
             "manifest_candidate_count" (length bundles)
             "manifest_candidate_ids" (coerce bundle-ids 'vector)
             "near_duplicate_suppressed_count" suppressed
             "cluster_cap" *turn-bundle-cluster-cap*
             "bundle_member_cap" *turn-bundle-max-members*
             "rows" (coerce rows 'vector)
             "curator_experiments" curator-experiments
             "expectations" (obj "content_oracle" oracle)
             "local_embedding_call_count"
             (- *pai-lab-experiment-local-embedding-calls* calls-before)
             "local_embedding_request_count"
             (- *pai-lab-experiment-local-embedding-requests* requests-before)
             "database_write_count" 0
             "provider_call_count" 0
             "delivery_authority" nil)))))

(defun %pai-lab-run-rerank-experiments (scenario query candidates limit)
  (coerce
   (mapcar
    (lambda (experiment)
      (cond ((string= experiment "nomic-query-document")
             (%pai-lab-run-nomic-query-document
              scenario query candidates limit))
            ((string= experiment "nomic-prefixed-full-scan")
              (%pai-lab-run-nomic-prefixed-full-scan
               scenario query candidates limit))
            ((string= experiment "nomic-turn-bundle-neighborhood-v1")
             (%pai-lab-run-nomic-turn-bundle-neighborhood
              scenario query candidates limit))
            (t (error "Unknown rerank experiment ~a." experiment))))
    (%pai-lab-rerank-experiments scenario))
   'vector))

(defun %pai-lab-run-read-only-memory (scenario)
  (%pai-lab-validate-scenario scenario)
  (let* ((id (gethash "id" scenario))
         (query (gethash "query" scenario))
         (as-of (gethash "as_of" scenario))
         (limit (gethash "result_limit" scenario 3))
         (experiment-calls-before
           *pai-lab-experiment-local-embedding-calls*)
         (experiment-requests-before
           *pai-lab-experiment-local-embedding-requests*))
    (funcall
     *pai-lab-read-only-transaction-fn*
     (lambda ()
       (let* ((*embedding-fallback-policy* :error)
              (rows (if as-of
                        (funcall *pai-lab-memory-search-fn*
                                 query :k 50 :mode :conversation :as-of as-of)
                        (funcall *pai-lab-memory-search-fn*
                                 query :k 50 :mode :conversation)))
              (eligible
                (remove-if-not #'%context-projection-eligible-memory-p rows))
              (content-safe
                (remove-if #'%context-projection-sensitive-memory-content-p
                           eligible))
              (non-echo
                (remove-if
                 (lambda (row)
                   (let ((content (gethash "content" row "")))
                     (and (stringp content)
                          (search query content :test #'char-equal))))
                 content-safe))
              (selected
                (let ((*context-projection-max-shared-memory-results* limit))
                  (%context-projection-select-shared-memory non-echo)))
              (selected-ids
                (mapcar (lambda (row) (gethash "id" row)) selected))
              (selector-diagnostics
                (%pai-lab-selector-diagnostics
                 rows eligible content-safe non-echo selected limit))
              (expectations
                (%pai-lab-expectations
                 scenario non-echo selected selector-diagnostics))
              (rerank-experiments
                (%pai-lab-run-rerank-experiments
                 scenario query non-echo limit))
              (candidate-output
                (subseq rows 0 (min (length rows)
                                    (if *pai-lab-reveal-private-content*
                                        50
                                        *pai-lab-max-candidate-output*)))))
         (obj "schema_version" *pai-lab-schema-version*
              "scenario_id" id
              "status" (if (and (gethash "passed" expectations)
                                  (%pai-lab-rerank-experiments-pass-p
                                   rerank-experiments))
                            "passed" "failed")
              "mode" "read-only-live"
              "private_content_included"
              (if *pai-lab-reveal-private-content* t nil)
              "query" (if *pai-lab-reveal-private-content* query :null)
              "query_characters" (length query)
              "as_of" (or as-of :null)
              "embedding_model" *ollama-embed-model*
              "embedding_fallback" "forbidden"
              "transaction_read_only" t
              "transaction_rolled_back" t
              "candidate_count" (length rows)
              "eligible_count" (length eligible)
              "eligibility_filtered_count" (- (length rows)
                                                (length eligible))
              "sensitive_filtered_count" (- (length eligible)
                                              (length content-safe))
              "echo_filtered_count" (- (length content-safe)
                                         (length non-echo))
              "selected_count" (length selected)
              "selected_ids" (coerce selected-ids 'vector)
              "candidates"
              (coerce
               (loop for row in candidate-output
                     for rank from 1
                     collect (%pai-lab-result-row
                              row rank selected-ids selector-diagnostics))
               'vector)
              "selected_records"
              (if *pai-lab-reveal-private-content*
                  (coerce
                   (loop for row in selected
                         for rank from 1
                         collect (%pai-lab-result-row
                                  row rank selected-ids selector-diagnostics))
                   'vector)
                  :null)
              "expectations" expectations
              "rerank_experiments" rerank-experiments
              "experimental_local_embedding_call_count"
              (- *pai-lab-experiment-local-embedding-calls*
                 experiment-calls-before)
              "experimental_local_embedding_request_count"
              (- *pai-lab-experiment-local-embedding-requests*
                 experiment-requests-before)
              "database_write_count" 0
              "provider_call_count" 0
              "delivery_authority" nil))))))

(defun %pai-lab-validate-atom-scenario (scenario)
  (unless (fboundp 'memory-atom-build-manifest)
    (error "Atom candidate contract is unavailable."))
  (%pai-lab-atom-exact-keys
   scenario '("id" "mode" "turn_id" "captured_at" "evidence"
              "expected_decision" "expected_atom_count"
              "expected_memory_forms") "atom Lab scenario")
  (unless (%pai-lab-nonempty-string-p (gethash "id" scenario))
    (error "Atom Lab scenario id is required."))
  (unless (string= "atom-decomposition-shadow" (gethash "mode" scenario ""))
    (error "Atom Lab scenario mode is invalid."))
  (let ((expected-decision (gethash "expected_decision" scenario))
        (expected-count (gethash "expected_atom_count" scenario))
        (expected-forms (%pai-lab-list
                         (gethash "expected_memory_forms" scenario))))
    (unless (member expected-decision '("PROPOSE" "NO_ATOMS") :test #'string=)
      (error "Atom Lab expected_decision is invalid."))
    (unless (and (integerp expected-count)
                 (<= 0 expected-count *pai-lab-atom-max-atoms*))
      (error "Atom Lab expected_atom_count is invalid."))
    (unless (and (<= (length expected-forms) *pai-lab-atom-max-atoms*)
                 (every (lambda (form)
                          (member form *pai-lab-atom-forms* :test #'string=))
                        expected-forms))
      (error "Atom Lab expected_memory_forms is invalid.")))
  (memory-atom-build-manifest
   (gethash "turn_id" scenario) (gethash "captured_at" scenario)
   (gethash "evidence" scenario)))

(defun %pai-lab-atom-summary (atom)
  (obj "candidate_id" (gethash "candidate_id" atom)
       "claim_key" (gethash "claim_key" atom)
       "idempotency_key" (gethash "idempotency_key" atom)
       "memory_form" (gethash "memory_form" atom)
       "evidence_ids" (gethash "evidence_ids" atom)
       "durable_disclosure_class"
       (gethash "disclosure_class"
                (gethash "persistence_projection" atom))))

(defun %pai-lab-run-atom-decomposition (scenario)
  (%pai-lab-validate-atom-scenario scenario)
  (let* ((id (gethash "id" scenario))
         (manifest (memory-atom-build-manifest
                    (gethash "turn_id" scenario)
                    (gethash "captured_at" scenario)
                    (gethash "evidence" scenario)))
         (request (memory-atom-build-request manifest))
         (captured (and (hash-table-p *pai-lab-atom-responses*)
                        (gethash id *pai-lab-atom-responses*)))
         (validated (and captured
                         (memory-atom-validate-response captured manifest)))
         (atoms (if validated
                    (%pai-lab-list (gethash "atoms" validated)) nil))
         (actual-decision (if validated (gethash "decision" validated) :null))
         (actual-forms (mapcar (lambda (atom) (gethash "memory_form" atom)) atoms))
         (expected-forms (%pai-lab-list
                          (gethash "expected_memory_forms" scenario)))
         (expectations-pass
           (or (null validated)
               (and (string= actual-decision
                             (gethash "expected_decision" scenario))
                    (= (length atoms) (gethash "expected_atom_count" scenario))
                    (equal actual-forms expected-forms))))
         (evidence (%pai-lab-list (gethash "evidence" manifest))))
    (obj "schema_version" *pai-lab-schema-version*
         "scenario_id" id
         "status" (if expectations-pass "passed" "failed")
         "mode" "atom-decomposition-shadow"
         "validation_status" (if validated "valid" "awaiting-captured-output")
         "captured_response_available" (if captured t nil)
         "turn_id" (gethash "turn_id" manifest)
         "evidence_count" (length evidence)
         "evidence"
         (if *pai-lab-reveal-private-content*
             (coerce evidence 'vector)
             (coerce
              (mapcar (lambda (row)
                        (obj "id" (gethash "id" row)
                             "role" (gethash "role" row)
                             "sequence" (gethash "sequence" row)
                             "observed_at" (gethash "observed_at" row)
                             "content_characters"
                             (length (gethash "content" row))))
                      evidence)
              'vector))
         "request_messages"
         (if *pai-lab-reveal-private-content* request :null)
         "decision" actual-decision
         "atom_count" (length atoms)
         "atoms"
         (if *pai-lab-reveal-private-content*
             (if validated (gethash "atoms" validated) (vector))
             (coerce (mapcar #'%pai-lab-atom-summary atoms) 'vector))
         "exclusion_count"
         (if validated (length (%pai-lab-list
                                (gethash "exclusions" validated))) 0)
         "captured_response"
         (if (and validated *pai-lab-reveal-private-content*) captured :null)
         "expected_decision" (gethash "expected_decision" scenario)
         "expected_atom_count" (gethash "expected_atom_count" scenario)
         "expected_memory_forms" (coerce expected-forms 'vector)
         "expectations_passed" (if expectations-pass t nil)
         "raw_evidence_immutable" t
         "admission_authority" nil
         "database_write_count" 0
         "provider_call_count" 0
         "delivery_authority" nil)))

(defun %pai-lab-validate-deliverable-scenario (scenario)
  (unless (hash-table-p scenario)
    (error "Deliverable scenario must be an object."))
  (unless (%pai-lab-nonempty-string-p (gethash "id" scenario))
    (error "Deliverable scenario id is required."))
  (unless (string= "read-only-deliverable" (gethash "mode" scenario ""))
    (error "Deliverable scenario mode is invalid."))
  (let ((path (gethash "path" scenario)))
    (unless (and (%pai-lab-nonempty-string-p path)
                 (not (uiop:absolute-pathname-p (pathname path)))
                 (not (find ".." (uiop:split-string path
                                                      :separator '(#\/ #\\))
                            :test #'string=))
                 (member (string-downcase
                          (or (pathname-type (pathname path)) ""))
                         '("md" "txt") :test #'string=))
      (error "Deliverable path must be a bounded relative Markdown/text path.")))
  (dolist (forbidden '("content" "write" "persist" "provider" "deliver"))
    (when (%pai-lab-key-present-p scenario forbidden)
      (error "Authority-bearing deliverable field ~a is forbidden." forbidden)))
  t)

(defun %pai-lab-run-read-only-deliverable (scenario)
  (%pai-lab-validate-deliverable-scenario scenario)
  (unless *pai-lab-deliverable-read-fn*
    (error "Bounded deliverable reader is unavailable."))
  (let* ((path (gethash "path" scenario))
         (raw (funcall *pai-lab-deliverable-read-fn* path)))
    (when (and (stringp raw) (uiop:string-prefix-p "ERROR:" raw))
      (error "Bounded deliverable read failed."))
    (let ((payload (handler-case (shasht:read-json raw)
                     (error () (error "Bounded deliverable result is not JSON.")))))
      (unless (and (hash-table-p payload)
                   (%pai-lab-nonempty-string-p (gethash "path" payload))
                   (integerp (gethash "original_characters" payload))
                   (stringp (gethash "content" payload))
                   (not (gethash "written" payload)))
        (error "Bounded deliverable result failed validation."))
      (obj "schema_version" *pai-lab-schema-version*
           "scenario_id" (gethash "id" scenario)
           "status" "passed" "mode" "read-only-deliverable"
           "private_content_included"
           (if *pai-lab-reveal-private-content* t nil)
           "path" (if *pai-lab-reveal-private-content* path :null)
           "original_characters" (gethash "original_characters" payload)
           "truncated" (if (gethash "truncated" payload) t nil)
           "content" (if *pai-lab-reveal-private-content*
                         (gethash "content" payload) :null)
           "filesystem_read_count" 1 "filesystem_write_count" 0
           "database_write_count" 0 "provider_call_count" 0
           "delivery_authority" nil))))

(defun %pai-lab-run-scenario (scenario)
  (let ((mode (and (hash-table-p scenario) (gethash "mode" scenario))))
    (cond ((and (stringp mode) (string= mode "read-only-live"))
           (%pai-lab-run-read-only-memory scenario))
          ((and (stringp mode) (string= mode "read-only-deliverable"))
           (%pai-lab-run-read-only-deliverable scenario))
          ((and (stringp mode) (string= mode "atom-decomposition-shadow"))
           (%pai-lab-run-atom-decomposition scenario))
          (t (error "Unknown the Lab scenario mode.")))))

(defun pai-lab-run-suite (suite)
  (unless (hash-table-p suite) (error "Lab suite must be an object."))
  (unless (= (gethash "schema_version" suite -1)
             *pai-lab-schema-version*)
    (error "Unsupported lab schema version."))
  (unless (%pai-lab-nonempty-string-p (gethash "suite_id" suite))
    (error "Lab suite_id is required."))
  (dolist (forbidden '("deliver" "send" "tick" "persist" "write"
                       "provider_api_key" "transport"))
    (when (%pai-lab-key-present-p suite forbidden)
      (error "Authority-bearing suite field ~a is forbidden." forbidden)))
  (let ((scenarios (%pai-lab-list (gethash "scenarios" suite))))
    (unless (and scenarios (<= (length scenarios) *pai-lab-max-scenarios*))
      (error "Lab suite must contain from 1 through ~d scenarios."
             *pai-lab-max-scenarios*))
    (let* ((database-access-required
             (not (null
                   (find "read-only-live" scenarios :test #'string=
                         :key (lambda (scenario)
                                (and (hash-table-p scenario)
                                     (gethash "mode" scenario)))))))
           (filesystem-access-required
             (not (null
                   (find "read-only-deliverable" scenarios :test #'string=
                         :key (lambda (scenario)
                                (and (hash-table-p scenario)
                                     (gethash "mode" scenario)))))))
           (*pai-lab-experiment-embedding-cache*
             (make-hash-table :test #'equal))
           (*pai-lab-experiment-local-embedding-calls* 0)
           (*pai-lab-experiment-local-embedding-requests* 0)
           (write-probe-blocked
             (if database-access-required
                 (handler-case (if (funcall *pai-lab-write-probe-fn*) t nil)
                   (error () nil))
                 t))
           (results
             (if write-probe-blocked
                 (mapcar
                  (lambda (scenario)
                    (handler-case
                        (%pai-lab-run-scenario scenario)
                      (error (condition)
                        (obj "schema_version" *pai-lab-schema-version*
                             "scenario_id" (if (hash-table-p scenario)
                                               (gethash "id" scenario :null)
                                               :null)
                             "status" "error"
                             "condition_type"
                             (string-downcase
                              (symbol-name (type-of condition)))
                             "database_write_count" 0
                             "provider_call_count" 0
                             "delivery_authority" nil))))
                  scenarios)
                 (mapcar
                  (lambda (scenario)
                    (obj "schema_version" *pai-lab-schema-version*
                         "scenario_id" (if (hash-table-p scenario)
                                           (gethash "id" scenario :null)
                                           :null)
                         "status" "error"
                         "condition_type" "database-write-probe-not-blocked"
                         "database_write_count" 0
                         "provider_call_count" 0
                         "delivery_authority" nil))
                  scenarios))))
      (obj "schema_version" *pai-lab-schema-version*
           "suite_id" (gethash "suite_id" suite)
           "status" (if (and write-probe-blocked
                              (every (lambda (result)
                                       (string= "passed"
                                                (gethash "status" result)))
                                     results))
                        "passed" "failed")
           "scenario_count" (length results)
           "results" (coerce results 'vector)
           "private_content_included"
           (if *pai-lab-reveal-private-content* t nil)
           "database_access_required" (if database-access-required t nil)
           "filesystem_access_required"
           (if filesystem-access-required t nil)
           "state_mount_read_only" t
           "database_write_probe_blocked"
           (if database-access-required write-probe-blocked :null)
           "experimental_local_embedding_call_count"
           *pai-lab-experiment-local-embedding-calls*
           "experimental_local_embedding_request_count"
           *pai-lab-experiment-local-embedding-requests*
           "database_write_count" 0
           "provider_call_count" 0
           "delivery_authority" nil))))

(defun pai-lab-run-scenario-file (path)
  (pai-lab-run-suite
   (shasht:read-json (uiop:read-file-string (pathname path)))))

(defun pai-lab-capability-report ()
  (obj "schema_version" *pai-lab-schema-version*
       "modes" (vector "read-only-live" "read-only-deliverable"
                       "atom-decomposition-shadow")
       "real_pgvector" t
       "strict_local_embeddings" t
       "state_mount_required_read_only" t
       "database_transaction_read_only" t
       "database_write_probe" "required-and-blocked"
       "private_content_default" "redacted"
       "private_content_reveal_available" t
       "ticks_available" nil
       "persistence_available" nil
       "atom_admission_available" nil
       "provider_calls_available" nil
       "delivery_authority" nil))
