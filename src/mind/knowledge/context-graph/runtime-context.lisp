;;;; Composition on sources already authenticated by the runtime's ledger port.
(in-package :pai.context-graph)

(defun context-graph-runtime-context (graph agent-id persona-id episode-id sources participants access snapshot)
  "No access grant is made here. Caller must authenticate complete SOURCES,
participant bindings and the policy SNAPSHOT before constructing this context."
  (let* ((policy (%cg-object "policy_revision" "operator-conversational-label-correction-v1" "enabled" :true
                            "scope_definitions" #()))
         (context (%cg-object "schema_version" 1 "authority_revision" "context-graph-authority-v1"
                    "agent_id" agent-id "persona_id" persona-id "episode_id" episode-id
                    "source_packet" (%cg-object "schema_version" 2 "sources" sources)
                    "primary_source_ids" (map 'vector (lambda (s) (gethash "source_id" s)) sources)
                    "participants" participants "access_context" access "access_snapshot_digest" snapshot
                    "projection_watermark" (%cg-authority-watermark graph agent-id persona-id)
                    "correction_policy" policy "correction_scopes" #() "eligible_entities" #()
                    "operator_commands" #() "candidate_scan" (%cg-object "complete" :false "examined_count" 0)))
         (query (format nil "~{~a~^ ~}" (map 'list (lambda (s) (gethash "text" s)) sources))))
    (context-graph-select-runtime-candidates graph context (subseq query 0 (min 1000 (length query))))))

(defun %cgro-source-batches (context)
  "Partition complete original utterances; never truncate or invent source IDs.
A single larger utterance stands alone and still faces the request byte gate."
  (let ((batches nil) (batch nil) (bytes 0))
    (loop for source across (gethash "sources" (gethash "source_packet" context))
          for size = (length (sb-ext:string-to-octets (gethash "text" source) :external-format :utf-8)) do
      (when (and batch (or (>= (length batch) 8) (> (+ bytes size) 6000)))
        (push (coerce (nreverse batch) 'vector) batches) (setf batch nil bytes 0))
      (push source batch) (incf bytes size))
    (when batch (push (coerce (nreverse batch) 'vector) batches))
    (coerce (nreverse batches) 'vector)))

(defun %cgro-batch-context (graph full index mode &optional (protocol "bounded-v2"))
  (let* ((batches (%cgro-source-batches full))
         (context (%cg-detach full)))
    (unless (and (integerp index) (<= 0 index) (< index (length batches)))
      (%cg-authority-fail "RUNTIME_BATCH_INDEX_INVALID"))
    (let* ((sources (aref batches index))
           (query (format nil "~{~a~^ ~}" (map 'list (lambda (s) (gethash "text" s)) sources))))
      (setf (gethash "schema_version" context) 1
            (gethash "source_packet" context) (%cg-object "schema_version" 2 "sources" (%cg-detach sources))
            (gethash "primary_source_ids" context) (map 'vector (lambda (s) (gethash "source_id" s)) sources)
            (gethash "eligible_entities" context) #()
            (gethash "correction_scopes" context) #()
            (gethash "correction_policy" context)
            (%cg-object "policy_revision" "operator-conversational-label-correction-v1" "enabled" :true "scope_definitions" #())
            (gethash "candidate_scan" context) (%cg-object "complete" :false "examined_count" 0))
      (remhash "correction_scans" context)
      (when (member protocol '("bounded-v3" "bounded-v4") :test #'equal)
        ;; Projection NEW IDs derive from episode_id + local_ref. Different
        ;; batches reuse new_1, so each needs its own deterministic namespace.
        ;; Original source IDs and the sealed-root access binding stay intact.
        (setf (gethash "episode_id" context)
              (concatenate 'string "episode-batch:"
                (%cg-authority-digest (if (equal protocol "bounded-v4") "runtime-source-batch-v4" "runtime-source-batch-v3")
                  (vector (gethash "episode_id" full) mode index
                          (gethash "primary_source_ids" context))))))
      ;; The full authenticated access snapshot remains bound; selection is not
      ;; a new access grant. The owner recomputes this exact subset on replay.
      (context-graph-select-runtime-candidates graph context
        (if (and (equal protocol "bounded-v4") (equal mode "staged")) query (subseq query 0 (min 1000 (length query))))
        :full-source-p (and (equal protocol "bounded-v4") (equal mode "staged"))
        :target-kinds (if (equal mode "correction") #("organism") #())
        :ordinary-limit (if (equal mode "correction") 0 (if (equal protocol "bounded-v4") 48 12))))))

(defun %cgro-plan-identity-windows (graph full index)
  "Read-only capacity plan, NOT an executable generation or authority context.
Refine only overflowing original batches. Preserve complete utterances and one
adjacent utterance on either side inside the original batch. Focus coverage is
exactly once; context overlaps. A single focus whose context cannot fit remains
blocked, never silently loses alternatives. Callers must bind/freeze this plan
before scheduling anything; recomputation after graph writes may change it."
  (let* ((batches (%cgro-source-batches full)) (rows nil) (checks 0))
    (unless (and (integerp index) (<= 0 index) (< index (length batches)))
      (%cg-authority-fail "RUNTIME_BATCH_INDEX_INVALID"))
    (let* ((sources (aref batches index)) (n (length sources)))
      (labels ((visit (start end)
                 (let* ((left (max 0 (1- start))) (right (min n (1+ end)))
                        (window (subseq sources left right)) (copy (%cg-detach full))
                        (failure nil) (count 0))
                   (incf checks)
                   (setf (gethash "sources" (gethash "source_packet" copy)) (%cg-detach window))
                   (handler-case
                       (setf count (length (gethash "eligible_entities"
                         (%cgro-batch-context graph copy 0 "staged" "bounded-v4"))))
                     (context-graph-authority-input-error (e)
                       (unless (member (%cg-authority-error-code e)
                                       '("IDENTITY_CANDIDATE_LIMIT" "IDENTITY_SCAN_LIMIT") :test #'equal)
                         (error e))
                       (setf failure (%cg-authority-error-code e))))
                   (if (and (equal failure "IDENTITY_CANDIDATE_LIMIT") (> (- end start) 1))
                       (let ((mid (+ start (floor (- end start) 2))))
                         (visit start mid) (visit mid end))
                       (push (%cg-object "status" (if failure "blocked" "capacity-ready")
                               "reason" (or failure :null)
                               "focus_start" start "focus_end" end
                               "context_start" left "context_end" right
                               "focus_source_ids" (map 'vector (lambda (s) (gethash "source_id" s)) (subseq sources start end))
                               "context_source_ids" (map 'vector (lambda (s) (gethash "source_id" s)) window)
                               "candidate_count" (if failure :null count)) rows)))))
        (visit 0 n))
      (%cg-object "policy_revision" "identity-window-capacity-v1"
                  "projection_watermark" (%cg-detach (gethash "projection_watermark" full))
                  "source_digest" (%cg-authority-digest "identity-window-sources-v1" sources)
                  "original_batch_index" index "candidate_checks" checks
                  "windows" (coerce (nreverse rows) 'vector)))))
