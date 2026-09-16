;;;; memory-nodes.lisp -- (node schema upgrade) + P1.3
;;;; (composite retrieval), now backed by Postgres + pgvector.
;;;;
;;;; 2026-07-27, second revision: originally a flat JSON file
;;;; (memory-nodes.json), rewritten wholesale on every write. That worked
;;;; fine at a handful of nodes but was heading toward two real problems
;;;; as the tick loop started writing every few minutes: (1)
;;;; full-file-rewrite-per-write cost growing with total data size, not
;;;; write size, and (2) the embedding vectors (768 floats each, ~10-15KB
;;;; of JSON text per node) dominating file size with no real similarity-
;;;; search capability beyond a linear scan in Lisp. Moved to a dedicated
;;;; Postgres+pgvector container (pai-postgres, on pai-net, kept
;;;; separate from Agent Foundry's own Postgres instance so the agent's memory
;;;; durability never depends on a different project's infrastructure
;;;; lifecycle) while this was still cheap to do (~20 nodes) rather than
;;;; deferring it into a much riskier migration later.
;;;;
;;;; ADDITIVE, not a replacement of enhancements.lisp's ENTITY/
;;;; RELATION graph -- unchanged from the original design, see
;;;; MEMORY-MIGRATE-FROM-ENTITY-GRAPH below.
;;;;
;;;; Every function keeps its EXACT prior signature and return shape (an
;;;; OBJ hash-table per node, same field names) -- tick-loop.lisp and
;;;; modulator.lisp's *MEMORY-AFFECT-MODULATOR-FN* hook needed zero
;;;; changes for this migration.
;;;;
;;;; pgvector values have no native postmodern marshalling -- vectors are
;;;; interpolated as literal '[0.1,0.2,...]'::vector text (system-generated
;;;; from EMBED-TEXT's own output, never user input, so this is safe
;;;; despite being string-built rather than a $N parameter) and read back
;;;; via an explicit ::text cast, parsed the same way either direction.
;;;;
;;;; No ANN index (ivfflat/hnsw) on the embedding column yet -- at current
;;;; scale (dozens of nodes) an exact sequential scan via pgvector's <=>
;;;; operator is fast and, unlike an approximate index, exact. Revisit
;;;; once node count reaches the thousands.
;;;;
;;;; Load live (no restart) via lisp-eval or repl-drop:
;;;;   (load "/agent/state/memory-nodes.lisp")
;;;; Requires QL:QUICKLOAD :POSTMODERN (baked into the image) and the
;;;; pai-postgres container reachable on pai-net.

(in-package :agent)

(ql:quickload :postmodern :silent t)

(export '(memory-write-node memory-get-node memory-add-edge memory-edges-from
           memory-edges-to memory-recall deep-recall memory-decay-tick
           memory-migrate-from-entity-graph save-memory-nodes load-memory-nodes))

;;; --- connection -------------------------------------------------------

(defparameter *pg-host* (or (uiop:getenv "PAI_PG_HOST") "pai-postgres"))
(defparameter *pg-port* (parse-integer (or (uiop:getenv "PAI_PG_PORT") "5432"))
  "Internal container port -- NOT the host-mapped 5433.")
(defparameter *pg-database* (or (uiop:getenv "PAI_PG_DATABASE") "pai_memory"))
(defparameter *pg-user* (or (uiop:getenv "PAI_PG_USER") "pai"))
(defparameter *pg-password* (or (uiop:getenv "PAI_PG_PASSWORD")
                                "pai_local_dev_only"))
(defvar *pai-pg-reuse-current-transaction-p* nil
  "Recovery-only dynamic switch. When true, WITH-PG reuses Postmodern's
already-bound connection so one complete scratch workflow can roll back under
an outer transaction. Ordinary runtime calls always leave this NIL.")

(defmacro with-pg (&body body)
  "Fresh connection per call rather than a shared global one -- simpler and
safer than connection pooling across the tick-loop thread and request-
handling threads concurrently, and cheap enough at this call frequency
(per-turn, per-tick, not a tight inner loop). Recovery may dynamically reuse a
single outer scratch transaction; ordinary calls do not."
  `(if *pai-pg-reuse-current-transaction-p*
       (progn ,@body)
       (pomo:with-connection
           (list *pg-database* *pg-user* *pg-password* *pg-host* :port *pg-port*)
         ,@body)))

;;; --- vector <-> Lisp marshalling ----------------------------------------

(defun %vector-literal (vec)
  ;; CL prints double-floats with a D exponent (for example 1.0d0), which
  ;; pgvector does not accept. pgvector stores float4 values, so nine fixed
  ;; decimal places preserve its meaningful precision and remain valid SQL.
  (format nil "[~{~,9f~^,~}]" (mapcar (lambda (value) (float value 1.0d0))
                                       (coerce vec 'list))))

(defun %parse-pg-vector (text)
  (mapcar (lambda (s) (let ((*read-default-float-format* 'double-float)) (read-from-string s)))
          (uiop:split-string (string-trim "[]" text) :separator '(#\,))))

;;; --- embeddings (unchanged from the JSON-file version) -----------------

(defparameter *ollama-endpoint*
  (or (uiop:getenv "PAI_OLLAMA_ENDPOINT")
      "http://pai-ollama:11434/api/embeddings")
  "A dedicated local Ollama container on pai-net, not exposed beyond it.")
(defparameter *ollama-embed-model* "nomic-embed-text")
(defparameter *ollama-timeout* 15)
(defparameter *ollama-embedding-batch-size* 64)
(defparameter *embedding-fallback-policy* :allow
  "Either :ALLOW for production-compatible degraded embeddings or :ERROR for
qualification runs that must prove they used the configured embedding model.")

(defun %embed-word-overlap-fallback (text)
  "Cheap pseudo-embedding when Ollama is unreachable: fixed-length word-hash
buckets, L2-normalized. Degraded, not broken."
  (let* ((dim 768)
         (v (make-array dim :element-type 'double-float :initial-element 0.0d0))
         (words (remove "" (uiop:split-string (string-downcase text) :separator '(#\Space #\Tab #\Newline #\. #\, #\! #\?))
                        :test #'string=)))
    (dolist (w words) (incf (aref v (mod (sxhash w) dim)) 1.0d0))
    (let ((norm (sqrt (loop for x across v sum (* x x)))))
      (when (plusp norm) (dotimes (i dim) (setf (aref v i) (/ (aref v i) norm)))))
    (coerce v 'list)))

(defun %embedding-fallback (text reason)
  (if (eq *embedding-fallback-policy* :error)
      (error "Configured embedding model unavailable in strict mode: ~a" reason)
      (progn
        (format t "~&[memory-nodes] ~a, using fallback~%" reason)
        (%embed-word-overlap-fallback text))))

(defun embed-text (text)
  (handler-case
      (let* ((resp (shasht:read-json
                    (dex:post *ollama-endpoint*
                              :headers '(("Content-Type" . "application/json"))
                              :connect-timeout *ollama-timeout* :read-timeout *ollama-timeout*
                              :content (shasht:write-json (obj "model" *ollama-embed-model* "prompt" text) nil))))
             (vec (gethash "embedding" resp)))
        (if (and (present-p vec) (plusp (length vec)))
            (coerce vec 'list)
            (%embedding-fallback text "Ollama returned no embedding")))
    (error (e)
      (%embedding-fallback text (format nil "embedding call failed (~a)" e)))))

(defun embed-retrieval-query (text)
  "Nomic retrieval query task typing; never use for stored documents."
  (embed-text (format nil "search_query: ~a" (or text ""))))

(defun embed-retrieval-document (text)
  "Nomic retrieval document task typing; stored separately from legacy vectors."
  (embed-text (format nil "search_document: ~a" (or text ""))))

(defun %ollama-batch-embedding-endpoint ()
  "Derive Ollama's batch endpoint from the sealed single-text endpoint."
  (let ((suffix "/api/embeddings"))
    (unless (and (stringp *ollama-endpoint*)
                 (>= (length *ollama-endpoint*) (length suffix))
                 (string= suffix *ollama-endpoint*
                          :start2 (- (length *ollama-endpoint*)
                                     (length suffix))))
      (error "Configured Ollama endpoint cannot authorize batch embeddings"))
    (concatenate 'string
                 (subseq *ollama-endpoint*
                         0 (- (length *ollama-endpoint*) (length suffix)))
                 "/api/embed")))

(defun embed-retrieval-documents (texts)
  "Embed ordered retrieval documents through bounded local Ollama batches.

This is a read-only acceleration seam. One failed strict batch fails the
semantic supplement; permissive callers degrade the complete batch locally
instead of multiplying a failed endpoint into one request per document."
  (let ((items (coerce texts 'list)))
    (when (null items) (return-from embed-retrieval-documents nil))
    (unless (and (integerp *ollama-embedding-batch-size*)
                 (plusp *ollama-embedding-batch-size*)
                 (<= (length items) 1024)
                 (every (lambda (text)
                          (and (stringp text) (<= (length text) 65536)))
                        items))
      (error "Retrieval embedding batch configuration or input is invalid"))
    (handler-case
        (loop for start from 0 below (length items)
                by *ollama-embedding-batch-size*
              for end = (min (length items)
                             (+ start *ollama-embedding-batch-size*))
              for batch = (subseq items start end)
              for payload = (obj "model" *ollama-embed-model*
                                 "input"
                                 (coerce
                                  (mapcar (lambda (text)
                                            (format nil "search_document: ~a"
                                                    (or text "")))
                                          batch)
                                  'vector))
              for response =
                (shasht:read-json
                 (dex:post (%ollama-batch-embedding-endpoint)
                           :headers '(("Content-Type" . "application/json"))
                           :connect-timeout *ollama-timeout*
                           :read-timeout (* 4 *ollama-timeout*)
                           :content (shasht:write-json payload nil)))
              for embeddings = (gethash "embeddings" response)
              unless (and (vectorp embeddings)
                          (= (length batch) (length embeddings))
                          (every (lambda (embedding)
                                   (and (vectorp embedding)
                                        (plusp (length embedding))))
                                 (coerce embeddings 'list)))
                do (error "Ollama returned an invalid embedding batch")
              append (map 'list (lambda (embedding)
                                  (coerce embedding 'list))
                          embeddings))
      (error (condition)
        (if (eq *embedding-fallback-policy* :error)
            (error "Configured embedding model unavailable in strict batch mode: ~a"
                   condition)
            (progn
              (format t "~&[memory-nodes] batch embedding failed (~a), using fallback~%"
                      condition)
              (mapcar #'%embed-word-overlap-fallback items)))))))

(defun cosine-similarity (v1 v2)
  "Kept for anything still calling it directly (e.g. ad-hoc lisp-eval
introspection) -- MEMORY-RECALL itself now uses pgvector's <=> operator
server-side instead."
  (let* ((n (min (length v1) (length v2))) (dot 0.0d0) (n1 0.0d0) (n2 0.0d0))
    (loop for i from 0 below n for a = (elt v1 i) for b = (elt v2 i)
          do (incf dot (* a b)) (incf n1 (* a a)) (incf n2 (* b b)))
    (if (or (zerop n1) (zerop n2)) 0.0d0 (/ dot (* (sqrt n1) (sqrt n2))))))

;;; --- node CRUD ----------------------------------------------------------

(defparameter *memory-node-kinds*
  '("observation" "thought" "reflection" "worldview" "episode"
    "skill" "self-fact" "prediction" "verdict"))
(defparameter *memory-edge-types*
  '("elaborates" "contradicts" "causes" "about" "follows" "evidence-for"
    "derived-from" "supersedes"))
(defvar *epistemic-memory-mode* :legacy
  "Compatibility default when memory-nodes is loaded before stabilization-config.")

(defun %row-to-node (row)
  "ROW column order must match every SELECT below:
id, kind, content, embedding::text, created_at::text, last_accessed::text,
access_count, importance, valence, arousal_at_encoding, activation,
  source_event_id, origin_class, epistemic_status, producer, model_purpose,
  confidence, grounding_status, root_observation_ids::text, generation_id,
  supersedes_node_id, quarantined, epistemic_metadata::text."
  (destructuring-bind (id kind content emb-text created last-accessed
                        access-count importance valence arousal activation source-event-id
                        origin-class epistemic-status producer model-purpose confidence
                        grounding-status roots-text generation-id supersedes-node-id
                        quarantined metadata-text)
      row
    (obj "id" id "kind" kind "content" content
         "embedding" (%parse-pg-vector emb-text)
         "created_at" created "last_accessed" last-accessed
         "access_count" access-count "importance" importance "valence" valence
          "arousal_at_encoding" arousal "activation" activation
          "source_event_id" (or source-event-id :null)
          "origin_class" origin-class "epistemic_status" epistemic-status
          "producer" (or producer :null) "model_purpose" (or model-purpose :null)
          "confidence" (or confidence :null) "grounding_status" grounding-status
          "root_observation_ids" (shasht:read-json roots-text)
          "generation_id" (or generation-id :null)
          "supersedes_node_id" (or supersedes-node-id :null)
          "quarantined" (if quarantined t nil)
          "epistemic_metadata" (shasht:read-json metadata-text)
          "edges" (coerce (memory-edges-from id) 'vector))))

(defparameter *node-select-columns*
  "id, kind, content, embedding::text, created_at::text, last_accessed::text, access_count, importance, valence, arousal_at_encoding, activation, source_event_id, origin_class, epistemic_status, producer, model_purpose, confidence, grounding_status, root_observation_ids::text, generation_id, supersedes_node_id, quarantined, epistemic_metadata::text")

;;; --- importance scoring --------------------------------------------
;;; 2026-07-27: . Every memory used to get importance 0.5 by
;;; default, or a hand-picked constant from whichever tick handler wrote it
;;; (0.3 for idle-drift, 0.6 for consolidate, etc. -- my own guesses, not a
;;; real signal). This replaces that with an actual cheap model call rating
;;; poignancy/significance 1-10 (Generative Agents style), normalized to
;;; 0.0-1.0. IMPORTANCE stays an explicit override when a caller passes
;;; one (e.g. MEMORY-MIGRATE-FROM-ENTITY-GRAPH's flat 0.5 for pre-existing
;;; facts, not new experiences worth judging) -- only NIL (unspecified)
;;; triggers real scoring, via (OR IMPORTANCE ...), which correctly leaves
;;; an explicit 0.0 alone too (0.0 is non-NIL in Lisp).

(defun %extract-first-integer (s)
  (let ((start (position-if #'digit-char-p s)))
    (and start (parse-integer s :start start :junk-allowed t))))

(defun %heuristic-importance (content)
  "Fallback when the model call fails or returns something unparseable --
a memory write must never block or error just because a rating call had a
hiccup. Blends content length (longer tends to carry more information,
though a crude proxy) with novelty vs. the single nearest EXISTING memory
(a near-duplicate of something already stored is less individually
important than something genuinely new) -- not the full length/novelty/
named-entity heuristic the backlog describes (named-entity detection has
no library here to do properly), but a real, working two-factor blend
rather than a flat constant."
  (let* ((len (length (or content "")))
         (len-component (min 1.0d0 (/ len 500.0d0)))
         (novelty-component
           (handler-case
               (let* ((vec (embed-text content)) (qlit (%vector-literal vec))
                      (best-sim (with-pg
                                  (pomo:query
                                   (format nil "SELECT 1 - (embedding <=> '~a'::vector) FROM memory_nodes ORDER BY embedding <=> '~a'::vector LIMIT 1"
                                           qlit qlit)
                                   :single))))
                 (if best-sim (- 1.0d0 best-sim) 0.7d0))
             (error () 0.5d0))))
    (max 0.1d0 (min 0.9d0 (+ (* 0.6d0 novelty-component) (* 0.4d0 len-component))))))

;;; Model-invocation port.
;;;
;;; The memory layer needs a model twice: to score how significant a memory is,
;;; and to ask whether two memories contradict. Calling RAW-CALL-MODEL directly
;;; made those the only hard upward calls from memory into cognition, and the
;;; only reason the two cannot be reasoned about separately. The memory
;;; module's own manifest has always declared :model-invocation among its
;;; adapter ports; this is that port finally existing.
;;;
;;; Late-bound on purpose. Setting the variable injects a model (a test double,
;;; a cheaper model for scoring than for conversation, a local one); leaving it
;;; unset falls back to whatever RAW-CALL-MODEL is bound to at call time, which
;;; preserves existing behaviour exactly; and with neither available both
;;; callers already degrade to their heuristics rather than failing.
;;;
;;; The fuller inversion is for the layer that owns model access to register
;;; itself here at :install, rather than for memory to reach for a name it
;;; knows. That is deliberately not done yet: RAW-CALL-MODEL's base currently
;;; lives in pending-split/enhancements.lisp, so registering from there would
;;; relocate the tangle rather than remove it. Revisit when that file is split.
(defvar *memory-model-invoke-fn* nil
  "Port: (messages) -> parsed model response, or NIL when no model is available.")

(defun %memory-invoke-model (messages)
  (cond (*memory-model-invoke-fn* (funcall *memory-model-invoke-fn* messages))
        ((fboundp 'raw-call-model) (funcall 'raw-call-model messages))
        (t nil)))

(defun %score-importance (content)
  "Rates CONTENT's poignancy 1-10 via RAW-CALL-MODEL (off the broadcast/
logging path, same primitive the verifier/summarizer/ticks all use),
normalized to 0.0-1.0. Cost is logged via LOG-EVENT regardless of caller
context (tick or otherwise) -- this task's own acceptance criterion asks
for cost per write to be bounded and logged, not just tracked when a tick
happens to be the one calling it."
  (handler-case
      (let* ((resp (%memory-invoke-model
                    (list (obj "role" "system" "content"
                               "On a scale of 1 to 10, rate the poignancy, significance, and emotional weight of this memory -- 1 is mundane and forgettable (small talk, routine status updates), 10 is extremely significant (major life facts, strong emotion, pivotal decisions). Respond with ONLY the number, nothing else.")
                          (obj "role" "user" "content" content))))
             (text (gethash "content" (ref resp "choices" 0 "message")))
             (usage (gethash "usage" resp))
             (n (and (stringp text) (%extract-first-integer text))))
        (when (fboundp 'log-event)
          (ignore-errors
            (funcall 'log-event "memory-importance-scored"
                     (obj "method" "model" "raw_score" (or n :null)
                          "cost" (and usage (or (gethash "cost" usage) 0))
                          "prompt_tokens" (and usage (or (gethash "prompt_tokens" usage) 0))
                          "completion_tokens" (and usage (or (gethash "completion_tokens" usage) 0))))))
        (if (and n (<= 1 n 10))
            (/ (float (1- n) 1.0d0) 9.0d0)
            (%heuristic-importance content)))
    (error (e)
      (format t "~&[memory-nodes] importance scoring failed (~a), using heuristic~%" e)
      (when (fboundp 'log-event)
        (ignore-errors (funcall 'log-event "memory-importance-scored" (obj "method" "heuristic" "reason" (format nil "~a" e)))))
      (%heuristic-importance content))))

;;; --- contradiction detection --------------------------------------
;;; 2026-07-27. On write of a self-fact or high-importance memory, checks
;;; the 3 nearest EXISTING (different-id) memories for a genuine, direct
;;; conflict -- not just unrelated or complementary content. Never auto-
;;; resolves: adds a CONTRADICTS edge and drops CERTAINTY (if modulator.lisp
;;; is loaded -- optional, FBOUNDP-guarded like every cross-file hook here)
;;; and leaves it there. "Queue for a reflection tick" (the backlog's
;;; phrasing) doesn't need a separate queue structure: any future
;;; consolidate-style tick can find unresolved contradictions just by
;;; querying memory_edges WHERE edge_type = 'contradicts' -- the edge graph
;;; already IS the queue.

(defparameter *contradiction-importance-threshold* 0.6
  "Observations at or below this skip the check entirely -- checking every
trivial memory would be wasteful; self-facts are always checked regardless
of their importance score.")
(defparameter *contradiction-candidate-count* 3)

(defun %check-contradiction (node-id content)
  (handler-case
      (let ((candidates
              (with-pg
                (pomo:query
                 (format nil "SELECT id, content FROM memory_nodes WHERE id != $1 ORDER BY embedding <=> (SELECT embedding FROM memory_nodes WHERE id = $1) LIMIT ~a"
                         *contradiction-candidate-count*)
                 node-id))))
        (dolist (cand candidates)
          (destructuring-bind (cand-id cand-content) cand
            (let* ((resp (%memory-invoke-model
                          (list (obj "role" "system" "content"
                                     "Does memory B directly contradict memory A -- state something factually incompatible, not just unrelated or complementary? Respond with ONLY YES or NO.")
                                (obj "role" "user" "content"
                                     (format nil "Memory A: ~a~%~%Memory B: ~a" cand-content content)))))
                   (verdict (string-trim '(#\Space #\Newline #\.)
                                          (or (gethash "content" (ref resp "choices" 0 "message")) ""))))
              (when (string-equal verdict "YES")
                (memory-add-edge node-id cand-id "contradicts")
                (when (fboundp 'modulator-adjust)
                  (ignore-errors (funcall 'modulator-adjust "certainty" -0.08)))
                (when (fboundp 'log-event)
                  (ignore-errors (funcall 'log-event "contradiction-detected" (obj "new_id" node-id "existing_id" cand-id))))
                (format t "~&[memory-nodes] contradiction detected: ~a vs ~a~%" node-id cand-id))))))
    (error (e) (format t "~&[memory-nodes] contradiction check failed: ~a~%" e))))

(defvar *importance-since-last-reflection* 0.0
  "Running total of IMPORTANCE across memory writes since the last full
reflection pass (P1.5's real pipeline, in tick-loop.lisp's CONSOLIDATE
handler) -- that handler resets this to 0 once a full pass runs.")

(defun %memory-json-string (value fallback)
  (let ((*print-pretty* nil))
    (shasht:write-json (or value fallback) nil)))

(defun %memory-root-vector (root-observation-ids)
  (cond ((null root-observation-ids) (vector))
        ((vectorp root-observation-ids) root-observation-ids)
        ((listp root-observation-ids) (coerce root-observation-ids 'vector))
        (t (vector root-observation-ids))))

(defun %memory-db-null (value)
  "Postmodern treats NIL as SQL FALSE; nullable SQL fields require :NULL."
  (if (null value) :null value))

;;; R0c2 dual-write seam. PostgreSQL remains authoritative until R0f, so a
;;; transaction accumulates exact row images and releases them only after a
;;; successful commit. This prevents rollback phantoms while preserving the
;;; current failure semantics: observability must never break a memory write.
(defvar *memory-durable-event-buffer-active-p* nil)
(defvar *memory-durable-event-buffer* nil)

(defun %memory-emit-durable-event (type payload)
  (when (fboundp 'log-event)
    (ignore-errors (funcall 'log-event type payload))))

(defun %memory-queue-durable-event (type payload)
  (if *memory-durable-event-buffer-active-p*
      (push (cons type payload) *memory-durable-event-buffer*)
      (%memory-emit-durable-event type payload)))

(defun %call-with-memory-durable-event-buffer (thunk)
  "Call THUNK and emit its queued row events iff it returns normally.
Nested users join the outer transaction buffer."
  (if *memory-durable-event-buffer-active-p*
      (funcall thunk)
      (let ((*memory-durable-event-buffer-active-p* t)
            (*memory-durable-event-buffer* nil))
        (multiple-value-prog1 (funcall thunk)
          (dolist (event (nreverse *memory-durable-event-buffer*))
            (%memory-emit-durable-event (car event) (cdr event)))))))

(defun %memory-json-row (text)
  (and text (shasht:read-json text)))

(defun %memory-node-row-json-current-connection (node-id)
  (pomo:query
   "SELECT row_to_json(memory_nodes)::text FROM memory_nodes WHERE id=$1"
   node-id :single))

(defun %memory-node-row-current-connection (node-id)
  (%memory-json-row (%memory-node-row-json-current-connection node-id)))

(defun %memory-edge-row-json-current-connection (from-id to-id edge-type)
  (pomo:query
   "SELECT row_to_json(memory_edges)::text FROM memory_edges WHERE from_id=$1 AND to_id=$2 AND edge_type=$3"
   from-id to-id edge-type :single))

(defun %memory-edge-row-current-connection (from-id to-id edge-type)
  (%memory-json-row
   (%memory-edge-row-json-current-connection from-id to-id edge-type)))

(defun %memory-queue-node-state (row operation mutation-kind &optional row-json)
  (when row
    (%memory-queue-durable-event
     "memory-node-state"
     (let ((payload
             (obj "operation" operation "mutation_kind" mutation-kind
                  "row" row)))
       (when row-json (setf (gethash "row_json" payload) row-json))
       payload)))
  row)

(defun %memory-queue-node-state-current-connection (node-id operation mutation-kind)
  (let ((row-json (%memory-node-row-json-current-connection node-id)))
    (%memory-queue-node-state
     (%memory-json-row row-json) operation mutation-kind row-json)))

(defun %memory-queue-edge-state (row operation mutation-kind &optional row-json)
  (when row
    (%memory-queue-durable-event
     "memory-edge-state"
     (let ((payload
             (obj "operation" operation "mutation_kind" mutation-kind
                  "row" row)))
       (when row-json (setf (gethash "row_json" payload) row-json))
       payload)))
  row)

(defun %memory-insert-edge-current-connection
    (from-id to-id edge-type &optional (mutation-kind "direct-edge"))
  "Insert one edge and queue its exact stored row only when it was new."
  (let* ((row-json
           (pomo:query
            "INSERT INTO memory_edges (from_id,to_id,edge_type) VALUES ($1,$2,$3) ON CONFLICT DO NOTHING RETURNING row_to_json(memory_edges)::text"
            from-id to-id edge-type :single))
         (row (%memory-json-row row-json)))
    (%memory-queue-edge-state row "insert" mutation-kind row-json)
    row))

(defun %memory-delete-edges-current-connection
    (from-id edge-type &optional (mutation-kind "lineage-replacement"))
  "Delete matching edges, retaining each full pre-delete row for replay."
  (let ((row-jsons
          (pomo:query
           "SELECT row_to_json(memory_edges)::text FROM memory_edges WHERE from_id=$1 AND edge_type=$2 ORDER BY to_id"
           from-id edge-type :column)))
    (pomo:execute
     "DELETE FROM memory_edges WHERE from_id=$1 AND edge_type=$2"
     from-id edge-type)
    (dolist (row-json row-jsons)
      (%memory-queue-edge-state
       (%memory-json-row row-json) "delete" mutation-kind row-json))
    (mapcar #'%memory-json-row row-jsons)))

(defun %memory-insert-node-current-connection
    (node-id kind content embedding retrieval-embedding importance valence arousal source-event-id
     origin-class epistemic-status producer model-purpose confidence
     grounding-status root-observation-ids generation-id supersedes-node-id
     quarantined epistemic-metadata)
  "Insert one fully materialized node using the caller's current connection.
admission uses this inside the same transaction as lineage edges."
  (let* ((emb-lit (%vector-literal embedding))
         (retrieval-emb-lit (%vector-literal retrieval-embedding))
         (roots-json (%memory-json-string (%memory-root-vector root-observation-ids)
                                          (vector)))
         (metadata-json (%memory-json-string epistemic-metadata (obj))))
    (pomo:execute
     (format nil "INSERT INTO memory_nodes
                  (id, kind, content, embedding, retrieval_embedding, importance, valence,
                   arousal_at_encoding, activation, source_event_id,
                   origin_class, epistemic_status, producer, model_purpose,
                   confidence, grounding_status, root_observation_ids,
                   generation_id, supersedes_node_id, quarantined,
                   epistemic_metadata)
                  VALUES ($1,$2,$3,'~a'::vector,'~a'::vector,$4,$5,$6,1.0,$7,$8,$9,$10,
                          $11,$12,$13,$14::jsonb,$15,$16,$17,$18::jsonb)
                  ON CONFLICT (id) DO UPDATE SET
                    kind=EXCLUDED.kind, content=EXCLUDED.content,
                    embedding='~a'::vector,
                    retrieval_embedding='~a'::vector,
                    importance=EXCLUDED.importance,
                    valence=EXCLUDED.valence,
                    arousal_at_encoding=EXCLUDED.arousal_at_encoding,
                    source_event_id=EXCLUDED.source_event_id,
                    origin_class=CASE WHEN EXCLUDED.origin_class='legacy-unclassified'
                                      THEN memory_nodes.origin_class ELSE EXCLUDED.origin_class END,
                    epistemic_status=CASE WHEN EXCLUDED.epistemic_status='legacy-unclassified'
                                          THEN memory_nodes.epistemic_status ELSE EXCLUDED.epistemic_status END,
                    producer=COALESCE(EXCLUDED.producer,memory_nodes.producer),
                    model_purpose=COALESCE(EXCLUDED.model_purpose,memory_nodes.model_purpose),
                    confidence=COALESCE(EXCLUDED.confidence,memory_nodes.confidence),
                    grounding_status=CASE WHEN EXCLUDED.grounding_status='unclassified'
                                          THEN memory_nodes.grounding_status ELSE EXCLUDED.grounding_status END,
                    root_observation_ids=CASE WHEN EXCLUDED.root_observation_ids='[]'::jsonb
                                              THEN memory_nodes.root_observation_ids ELSE EXCLUDED.root_observation_ids END,
                    generation_id=COALESCE(EXCLUDED.generation_id,memory_nodes.generation_id),
                    supersedes_node_id=COALESCE(EXCLUDED.supersedes_node_id,memory_nodes.supersedes_node_id),
                    quarantined=(memory_nodes.quarantined OR EXCLUDED.quarantined),
                    epistemic_metadata=CASE WHEN EXCLUDED.epistemic_metadata='{}'::jsonb
                                            THEN memory_nodes.epistemic_metadata ELSE EXCLUDED.epistemic_metadata END"
             emb-lit retrieval-emb-lit emb-lit retrieval-emb-lit)
     node-id (or kind "observation") content importance valence arousal
     (%memory-db-null (and source-event-id (format nil "~a" source-event-id)))
     (or origin-class "legacy-unclassified")
     (or epistemic-status "legacy-unclassified")
     (%memory-db-null producer) (%memory-db-null model-purpose)
     (%memory-db-null confidence)
     (or grounding-status "unclassified") roots-json (%memory-db-null generation-id)
     (%memory-db-null supersedes-node-id) (if quarantined t nil) metadata-json)
    (%memory-queue-node-state-current-connection
     node-id "upsert" "node-write")
    node-id))

(defun %memory-after-write (node-id kind content importance)
  "Non-transactional legacy side effects, called only after the node commit."
  (incf *importance-since-last-reflection* importance)
  (when (fboundp 'log-event)
    (ignore-errors
      (funcall 'log-event "memory-write"
               (obj "node_id" node-id "kind" kind "content" (or content :null)))))
  (when (or (string= (or kind "observation") "self-fact")
            (> importance *contradiction-importance-threshold*))
    (%check-contradiction node-id content))
  node-id)

(define-seam memory-write-node (&key kind content (importance nil) (valence 0.0) (arousal 0.3)
                                source-event-id (id nil) edges
                                origin-class epistemic-status producer model-purpose
                                confidence grounding-status root-observation-ids
                                generation-id supersedes-node-id quarantined
                                epistemic-metadata lineage-parent-ids novelty-passed
                                self-process-event-id)
  "Creates (or overwrites, if ID given and already present -- upsert) a
memory node. Persisted immediately (a single-row INSERT, not a whole-
table rewrite -- the actual point of this migration). IMPORTANCE, if not
given, is scored automatically (see %SCORE-IMPORTANCE above) rather than
defaulting to a flat constant. Self-facts and high-importance memories are
also checked for contradictions against nearby existing memories."
  (declare (ignore edges)) ; legacy inline edges remain unsupported
  ;; DEFUN gave this body an implicit block named MEMORY-WRITE-NODE, which the
  ;; RETURN-FROM below relies on. DEFINE-SEAM's base is a LAMBDA and has no
  ;; such block, so it is made explicit here (gotcha 14).
  (block memory-write-node
    ;; In enforced mode this compatibility seam cannot bypass admission. In
    ;; legacy/shadow mode its historical call/return contract is unchanged.
    (when (or (and (boundp '*memory-cognitive-mutation-mode*)
                   (eq *memory-cognitive-mutation-mode* :event-first))
              (and (boundp '*epistemic-memory-mode*)
                   (eq *epistemic-memory-mode* :enforced)))
      (unless (fboundp 'memory-admit-node)
        (error "Epistemic memory is enforced but MEMORY-ADMIT-NODE is unavailable"))
      (return-from memory-write-node
        (funcall 'memory-admit-node
                 :kind kind :content content :importance importance
                 :valence valence :arousal arousal :source-event-id source-event-id
                 :id id
                 :origin-class (or origin-class "legacy-unclassified")
                 :epistemic-status (or epistemic-status "legacy-unclassified")
                 :producer (or producer "legacy-memory-write-node")
                 :model-purpose model-purpose :confidence confidence
                 :grounding-status (or grounding-status "unclassified")
                 :generation-id generation-id
                 :supersedes-node-id supersedes-node-id :quarantined quarantined
                 :epistemic-metadata epistemic-metadata
                 :lineage-parent-ids lineage-parent-ids
                 :novelty-passed novelty-passed
                 :self-process-event-id self-process-event-id)))
    (let* ((importance (or importance (%score-importance (or content ""))))
           ;; Found live during P1.5 testing: GET-UNIVERSAL-TIME has only
           ;; 1-SECOND resolution -- two writes within the same second
           ;; generated the SAME auto-id, and the second silently overwrote
           ;; the first via the INSERT's ON CONFLICT upsert (no error, no
           ;; warning, just a vanished memory). A random suffix makes
           ;; same-second collisions negligible without needing a shared
           ;; counter or a full UUID library.
           (node-id (or id (format nil "node-~a-~4,'0x" (get-universal-time) (random 65536))))
           (embedding (embed-text (or content "")))
           (retrieval-embedding (embed-retrieval-document (or content ""))))
      (with-pg
        (%memory-insert-node-current-connection
         node-id kind content embedding retrieval-embedding importance valence arousal source-event-id
         origin-class epistemic-status producer model-purpose confidence
         grounding-status root-observation-ids generation-id supersedes-node-id
         quarantined epistemic-metadata))
      (%memory-after-write node-id kind content importance)
      (when (and (boundp '*epistemic-memory-mode*)
                 (eq *epistemic-memory-mode* :shadow)
                 (fboundp '%epistemic-shadow-assess-write))
        (ignore-errors
          (funcall '%epistemic-shadow-assess-write
                   node-id :kind kind :source-event-id source-event-id
                   :origin-class origin-class :epistemic-status epistemic-status
                   :producer producer :model-purpose model-purpose :confidence confidence
                   :grounding-status grounding-status
                   :lineage-parent-ids lineage-parent-ids
                   :novelty-passed novelty-passed
                   :self-process-event-id self-process-event-id)))
      node-id)))

(defun memory-get-node (id)
  (when (and (boundp '*memory-cognitive-mutation-mode*)
             (eq *memory-cognitive-mutation-mode* :event-first))
    (let ((envelope
            (memory-storage-operation-node *memory-search-storage-backend* id)))
      (return-from memory-get-node
        (and envelope
             (%memory-storage-json-read (gethash "scalar_json" envelope)
                                        :memory-get-node)))))
  (with-pg
    (let ((row (pomo:query (format nil "SELECT ~a FROM memory_nodes WHERE id = $1" *node-select-columns*) id :row)))
      (and row (%row-to-node row)))))

(defun memory-add-edge (from-id to-id type)
  "Adds a typed edge FROM-ID -> TO-ID, in its own table now (memory_edges)
rather than an inline vector on the node -- MEMORY-EDGES-FROM/TO both
query it directly, indexed both directions."
  (if (and (boundp '*memory-cognitive-mutation-mode*)
           (eq *memory-cognitive-mutation-mode* :event-first))
      (memory-cognitive-mutation-dispatch
       "direct-edge"
       (obj "from_id" from-id "to_id" to-id "edge_type" type)
       (lambda () (error "PostgreSQL fallback is unavailable")))
      (progn
        (with-pg
          (%memory-insert-edge-current-connection
           from-id to-id type "direct-edge"))
        t)))

(defun memory-edges-from (id &optional type)
  (when (and (boundp '*memory-cognitive-mutation-mode*)
             (eq *memory-cognitive-mutation-mode* :event-first))
    (return-from memory-edges-from
      (mapcar
       (lambda (row-json)
         (let ((row (%memory-storage-json-read row-json :memory-edges-from)))
           (obj "to" (gethash "to_id" row)
                "type" (gethash "edge_type" row))))
       (memory-storage-operation-edges
        *memory-search-storage-backend* :from-id id :edge-type type))))
  (with-pg
    (mapcar (lambda (row) (obj "to" (first row) "type" (second row)))
            (if type
                (pomo:query "SELECT to_id, edge_type FROM memory_edges WHERE from_id = $1 AND edge_type = $2" id type)
                (pomo:query "SELECT to_id, edge_type FROM memory_edges WHERE from_id = $1" id)))))

(defun memory-edges-to (id &optional type)
  (when (and (boundp '*memory-cognitive-mutation-mode*)
             (eq *memory-cognitive-mutation-mode* :event-first))
    (return-from memory-edges-to
      (mapcar
       (lambda (row-json)
         (let ((row (%memory-storage-json-read row-json :memory-edges-to)))
           (obj "from" (gethash "from_id" row)
                "type" (gethash "edge_type" row))))
       (memory-storage-operation-edges
        *memory-search-storage-backend* :to-id id :edge-type type))))
  (with-pg
    (mapcar (lambda (row) (obj "from" (first row) "type" (second row)))
            (if type
                (pomo:query "SELECT from_id, edge_type FROM memory_edges WHERE to_id = $1 AND edge_type = $2" id type)
                (pomo:query "SELECT from_id, edge_type FROM memory_edges WHERE to_id = $1" id)))))

;;; --- composite retrieval ----------------------------------------

(defparameter *memory-recall-half-life-seconds* (* 7 24 3600))
(defparameter *memory-recall-weights* (obj "sim" 0.4 "imp" 0.3 "rec" 0.2 "act" 0.1))
(defvar *memory-recall-clock-fn* #'get-universal-time
  "Injectable read-only clock for deterministic retrieval qualification.")
(defvar *memory-affect-modulator-fn* nil
  "Hook for (see modulator.lisp): if bound, called as (funcall
fn weights-obj), must return a weights OBJ of the same shape.")

(defun %parse-pg-timestamp (text)
  "Postgres TIMESTAMPTZ::text looks like '2026-07-27 20:58:13.951623+00' --
parses the leading date/time part only, timezone offset ignored
(everything here is UTC), into universal-time. Shared by %RECENCY-SCORE
and MEMORY-DECAY-TICK so there's exactly one parser to keep correct."
  (let* ((s text)
         (year (parse-integer s :start 0 :end 4)) (month (parse-integer s :start 5 :end 7))
         (day (parse-integer s :start 8 :end 10)) (hour (parse-integer s :start 11 :end 13))
         (min (parse-integer s :start 14 :end 16)) (sec (parse-integer s :start 17 :end 19)))
    (encode-universal-time sec min hour day month year 0)))

(defun %recency-score (created-at-text)
  (handler-case
      (let ((age (max 0 (- (funcall *memory-recall-clock-fn*)
                           (%parse-pg-timestamp created-at-text)))))
        (exp (- (/ age (float *memory-recall-half-life-seconds* 1.0d0)))))
    (error () 0.5d0)))

;;; 2026-07-29: added EXCLUDE-IDS, a narrow additive keyword (default NIL
;;; -- every existing caller is completely unaffected). Root-caused today:
;;; %TICK-HANDLE-LIGHT-CONSOLIDATE, %TICK-HANDLE-FULL-REFLECTION, and
;;; %EXPLORE-PICK-QUESTION all call this with a FIXED literal query string
;;; ("what matters most right now" et al) -- the query embedding never
;;; changes, so the same handful of nodes win the similarity ranking every
;;; time, and since retrieval itself boosts ACTIVATION below, those same
;;; nodes get more entrenched with every pass. Observed live as hours of
;;; near-verbatim-repeated reflections/self-model-revisions on one theme.
;;; EXCLUDE-IDS lets a caller (ambient-recall-diversity.lisp) filter
;;; recently-surfaced nodes out of the CANDIDATE POOL before scoring and
;;; before the activation-boost side effect below ever touches them --
;;; the boost and the candidate selection happen in the same function
;;; with no other seam to intercept between them, which is why this is a
;;; direct edit here rather than a wrap from another file (same narrow
;;; exception as AGENT_LOOP.LISP's MANAGE-CONTEXT).
(defun memory-recall (query &key (k (if (fboundp 'modulator-recall-k) (funcall 'modulator-recall-k) 8)) debug include-cold exclude-ids kinds)
  "pgvector does the expensive nearest-neighbour narrowing (ORDER BY <=>
LIMIT candidate-set); the full weighted composite score (similarity +
importance + recency + activation) is then computed in Lisp over just
that candidate set, matching the exact formula the JSON-file version
used, before taking the top K. Excludes cold-storage nodes unless
INCLUDE-COLD is true -- see DEEP-RECALL below, the explicit escape hatch:
forgetting is retrieval failure, never deletion, so a cold node is still
here, just not surfaced by ordinary recall. EXCLUDE-IDS (a list of node-id
strings), if given, removes those nodes from the candidate pool entirely,
before scoring. KINDS, when given, is a trusted internal list of memory
kinds that narrows the pool before semantic ranking; relational recall uses
it to prefer shared experience over the agent's internal abstractions."
  (when (and (boundp '*memory-cognitive-mutation-mode*)
             (eq *memory-cognitive-mutation-mode* :event-first))
    (when include-cold
      (error 'memory-storage-error :operation :legacy-deep-recall
             :detail "event-first deep recall requires an explicit audit profile"))
    (let ((records (memory-search query :k k :mode :audit
                                  :kinds kinds :exclude-ids exclude-ids)))
      (return-from memory-recall
        (if debug
            (mapcar (lambda (row)
                      (setf (gethash "score" row)
                            (gethash "retrieval_score" row))
                      row)
                    records)
            (mapcar (lambda (row) (gethash "content" row)) records)))))
  (let* ((qvec (embed-text query)) (qlit (%vector-literal qvec))
         (candidate-limit (max 50 (* k 5)))
         (weights (if *memory-affect-modulator-fn* (funcall *memory-affect-modulator-fn* *memory-recall-weights*)
                      *memory-recall-weights*))
         (w-sim (gethash "sim" weights)) (w-imp (gethash "imp" weights))
         (w-rec (gethash "rec" weights)) (w-act (gethash "act" weights))
         (kind-clause (if kinds (format nil " AND kind IN (~{'~a'~^,~})" kinds) ""))
         (rows (remove-if (lambda (row) (member (first row) exclude-ids :test #'equal))
                (with-pg
                 (pomo:query
                  (format nil "SELECT id, kind, content, importance, activation, created_at::text, 1 - (embedding <=> '~a'::vector) AS sim FROM memory_nodes ~a~a ORDER BY embedding <=> '~a'::vector LIMIT $1"
                          qlit (if include-cold "WHERE true" "WHERE is_cold = false") kind-clause qlit)
                  candidate-limit))))
         (scored nil))
    (dolist (row rows)
      (destructuring-bind (id kind content importance activation created sim) row
        (let* ((rec (%recency-score created))
               (score (+ (* w-sim sim) (* w-imp importance) (* w-rec rec) (* w-act activation))))
          (push (list :score score :sim sim :imp importance :rec rec :act activation
                      :id id :content content :kind kind)
                scored))))
    (setf scored (sort scored #'> :key (lambda (s) (getf s :score))))
    (let ((top (subseq scored 0 (min k (length scored)))))
      (when top
        (with-pg
          (dolist (s top)
            (pomo:execute "UPDATE memory_nodes SET last_accessed = now(), access_count = access_count + 1, activation = LEAST(1.0, activation + 0.2) WHERE id = $1"
                          (getf s :id))
            (%memory-queue-node-state-current-connection
             (getf s :id) "update" "legacy-recall-rehearsal")))
        ;; P2.2 appraisal: "low novelty of recent retrievals -> boredom;
        ;; novel input -> down boredom, up arousal". Uses the similarity
        ;; scores already computed above -- no extra model call. Optional,
        ;; FBOUNDP-guarded since modulator.lisp may load after this file.
        (when (fboundp 'modulator-adjust)
          (let ((avg-sim (/ (reduce #'+ top :key (lambda (s) (getf s :sim))) (length top))))
            (ignore-errors
              (if (> avg-sim 0.85)
                  (funcall 'modulator-adjust "boredom" 0.03)
                  (progn (funcall 'modulator-adjust "boredom" -0.02)
                         (funcall 'modulator-adjust "arousal" 0.02)))))))
      (if debug
          (mapcar (lambda (s) (obj "id" (getf s :id) "content" (getf s :content) "score" (getf s :score)
                                    "sim" (getf s :sim) "imp" (getf s :imp) "rec" (getf s :rec) "act" (getf s :act)))
                  top)
          (mapcar (lambda (s) (getf s :content)) top)))))

(defun deep-recall (query &key (k 8) debug)
  "MEMORY-RECALL, but including cold-storage nodes (P1.4's explicit escape
hatch) -- a memory that's been forgotten by ordinary recall is still
here, still real, just requires asking for it deliberately."
  (memory-recall query :k k :debug debug :include-cold t))

;;; --- decay and real forgetting --------------------------------------
;;; 2026-07-27. Per-node exponential decay of ACTIVATION, half-life varying
;;; by KIND (observations decay fast, self-facts barely at all) -- run
;;; periodically (wired into tick-loop.lisp's MAINTENANCE handler). ACTIVATION
;;; is recomputed as a pure function of elapsed time since LAST_ACCESSED,
;;; not an incremental decrement -- MEMORY-RECALL already bumps
;;; LAST_ACCESSED to now() on retrieval, so this gets "retrieval as
;;; rehearsal" for free, with no separate decay-tracking bookkeeping to
;;; keep in sync. Below *DECAY-COLD-THRESHOLD*, a node is marked IS_COLD:
;;; excluded from ordinary MEMORY-RECALL, still fully on disk, still
;;; reachable via DEEP-RECALL. Forgetting is retrieval failure, never
;;; deletion -- nothing here ever runs a DELETE.

(defparameter *decay-half-life-seconds*
  (obj "observation" (* 3 86400) "thought" (* 4 86400) "prediction" (* 4 86400)
       "verdict" (* 5 86400) "skill" (* 10 86400) "reflection" (* 14 86400)
       "self-fact" (* 90 86400))
  "Days converted to seconds. Observations and thoughts fade fastest;
reflections (already-distilled insight) slower; self-facts barely at all.")
(defparameter *decay-cold-threshold* 0.05)

(defun %decay-half-life-for-kind (kind)
  (or (gethash kind *decay-half-life-seconds*) (* 5 86400)))

(defun %memory-update-decay-row-json-current-connection (node-id activation cold)
  "Update one decay row and return PostgreSQL's exact row JSON string."
  (pomo:query
   "UPDATE memory_nodes SET activation = $1, is_cold = $2 WHERE id = $3 RETURNING row_to_json(memory_nodes)::text"
   activation cold node-id :single))

(defun %memory-update-decay-row-current-connection (node-id activation cold)
  "Update one decay row and return its exact committed-shape row image.
Caller owns the PostgreSQL connection; ordinary autocommit remains per row."
  (let ((row-json
          (%memory-update-decay-row-json-current-connection
           node-id activation cold)))
    (values (%memory-json-row row-json) row-json)))

(defun memory-decay-tick ()
  "Recomputes ACTIVATION and IS_COLD for every node from its KIND and
elapsed time since LAST_ACCESSED. Returns the count of nodes newly moved
to cold storage this pass (0 if none), logged via LOG-EVENT if loaded."
  (when (and (boundp '*memory-cognitive-mutation-mode*)
             (eq *memory-cognitive-mutation-mode* :event-first))
    (error 'memory-storage-error :operation :memory-decay
           :detail "event-first decay requires the bounded maintenance projector"))
  (let ((rows nil)
        (newly-cold 0))
    ;; Keep one connection for the scan and update loop. Do not wrap the loop
    ;; in an explicit transaction: each row retains the incumbent autocommit
    ;; and partial-progress behavior if a later row fails.
    (with-pg
      (setf rows
            (pomo:query
             "SELECT id, kind, is_cold, last_accessed::text FROM memory_nodes"))
      (dolist (row rows)
        (destructuring-bind (id kind was-cold last-accessed) row
          (let* ((half-life (%decay-half-life-for-kind kind))
                 (elapsed
                   (max 0 (- (get-universal-time)
                             (%parse-pg-timestamp last-accessed))))
                 (activation
                   (exp (- (/ elapsed (float half-life 1.0d0)))))
                 (cold (< activation *decay-cold-threshold*)))
            (multiple-value-bind (stored-row stored-row-json)
                (%memory-update-decay-row-current-connection
                 id activation cold)
              (when stored-row
                (%memory-queue-node-state
                 stored-row "update" "decay" stored-row-json)))
            (when (and cold (not was-cold))
              (incf newly-cold))))))
    (when (and (fboundp 'log-event) (plusp newly-cold))
      (ignore-errors (funcall 'log-event "memory-decay" (obj "nodes_scanned" (length rows) "newly_cold" newly-cold))))
    newly-cold))

;;; --- migration from the existing ENTITY/RELATION graph ------------------

(defun memory-migrate-from-entity-graph ()
  "One-time (idempotent -- fixed 'migrated:<id>' node id, re-running just
upserts current entity data) copy of every ENTITY in *MEMORY-GRAPH* into a
node here. Does not touch or remove anything in the source graph."
  (let ((migrated 0))
    (maphash
     (lambda (id entity)
       (when (typep entity 'entity)
         (let ((content
                 (with-output-to-string (s)
                   (format s "~a (~a): " id (entity-type entity))
                   (maphash (lambda (k v) (format s "~a=~a; " k v)) (entity-attributes entity)))))
           (memory-write-node :id (format nil "migrated:~a" id)
                               :kind (if (string= (entity-type entity) "user") "self-fact" "observation")
                               :content content :importance 0.5 :valence 0.0 :arousal 0.3)
           (incf migrated))))
     *memory-graph*)
    (format t "~&[memory-nodes] migrated ~a entities from *memory-graph* into Postgres nodes.~%" migrated)
    migrated))

;;; --- back-compat no-ops (JSON-file persistence no longer applies) -------

(defun save-memory-nodes ()
  "Every write is already persisted immediately (single-row upsert) --
this is now a deliberate no-op, kept only so nothing calling it (existing
scripts, muscle memory) errors out."
  t)

(defun load-memory-nodes ()
  "Nothing to load -- Postgres IS the store now, always current. Kept for
the same reason as SAVE-MEMORY-NODES above."
  t)

(defun %memory-node-count ()
  (with-pg (pomo:query "SELECT count(*) FROM memory_nodes" :single)))
