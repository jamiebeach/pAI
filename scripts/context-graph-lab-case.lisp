;;;; Checkpoint-only recorded replay. No database or provider fallback.
(in-package :agent)

(defparameter *cgl-run-cache* (make-hash-table :test #'equal))
(defparameter *cgl-run-order* nil)

(defun %cgl-cache-run (id before after cut last-id)
  (when id
    ;; Queries need runtimes, not every historical phase response in the owner.
    (flet ((query-only (lab cutoff)
             (let ((copy (%make-context-graph-episode-lab :sealed-only-p t)))
               (setf (gethash cutoff (context-graph-episode-lab-runtime-cache copy))
                     (gethash cutoff (context-graph-episode-lab-runtime-cache lab)))
               copy)))
      (setf (gethash id *cgl-run-cache*) (list (query-only before cut) (query-only after last-id) cut last-id)))
    (setf
          *cgl-run-order* (cons id (remove id *cgl-run-order* :test #'equal)))
    (loop while (> (length *cgl-run-order*) 4) do
      (remhash (car (last *cgl-run-order*)) *cgl-run-cache*)
      (setf *cgl-run-order* (butlast *cgl-run-order*)))))

(defun context-graph-lab-cached-query (id side request)
  (let ((entry (gethash id *cgl-run-cache*)))
    (unless entry (error "Run graph is no longer loaded; no automatic replay was performed"))
    (unless (member side '("before" "after") :test #'equal) (error "Unknown graph side"))
    (context-graph-episode-lab-query (if (equal side "before") (first entry) (second entry))
                                    (if (equal side "before") (third entry) (fourth entry)) request)))

(defun %cgl-case-lab (runtime owner cutoff)
  (let ((lab (%make-context-graph-episode-lab
              :sealed-only-p t
              :agent-id (pai.context-graph::cgi-owner-agent-id owner)
              :persona-id (pai.context-graph::cgi-owner-persona-id owner))))
    (setf (gethash cutoff (context-graph-episode-lab-runtime-cache lab)) runtime
          (gethash cutoff (context-graph-episode-lab-owner-cache lab)) owner)
    lab))

(defun %cgl-identity-endpoint (local-ref trace)
  "Join one proposal endpoint to every exact mention/candidate decision available."
  (cond
    ((not (stringp local-ref))
     (obj "status" "unavailable" "reason" "endpoint-reference-absent" "local_ref" :null))
    ((member local-ref '("runtime:operator" "runtime:active-persona") :test #'equal)
     (obj "status" "reserved-participant" "local_ref" local-ref
          "participant_role" (if (equal local-ref "runtime:operator") "operator" "active-persona")))
    ((not (hash-table-p trace))
     (obj "status" "unavailable" "reason" "identity-trace-absent" "local_ref" local-ref))
    (t
     (let ((rows
             (loop for binding across (gethash "bindings" trace #())
                   when (equal local-ref (gethash "entity" binding)) collect
               (let* ((mention-id (gethash "mention" binding))
                      (mention (find mention-id (gethash "mentions" trace #()) :test #'equal
                                     :key (lambda (row) (gethash "mention" row))))
                      (resolution (find mention-id (gethash "resolutions" trace #()) :test #'equal
                                        :key (lambda (row) (gethash "mention" row))))
                      (candidate-id (and resolution (gethash "candidate" resolution)))
                      (candidate (and (stringp candidate-id)
                                      (find candidate-id (gethash "candidates" trace #()) :test #'equal
                                            :key (lambda (row) (gethash "candidate" row)))))
                      (pages
                        (loop for page across (gethash "page_responses" trace #())
                              for page-number from 1
                              for decision = (find mention-id (gethash "mentions" page #()) :test #'equal
                                                   :key (lambda (row) (gethash "mention" row)))
                              when decision collect
                                (obj "page" page-number "decision" decision))))
                 (obj "mention" (or mention :null) "binding" binding
                      "resolution" (or resolution :null)
                      "candidate" (or candidate :null)
                      "page_decisions" (coerce pages 'vector)
                      "status" (if candidate "resolved" "unresolved"))))))
       (if rows
           (obj "status" (if (every (lambda (row) (equal "resolved" (gethash "status" row))) rows)
                                "resolved" "partially-resolved")
                "local_ref" local-ref "mention_count" (length rows)
                "mentions" (coerce rows 'vector))
           (obj "status" "unavailable" "reason" "no-bound-mention-for-endpoint"
                "local_ref" local-ref))))))

(defun %cgl-linked-claims (result application graph terminal-id)
  "Join only exact application provenance; ambiguous/missing joins stay unknown."
  (let* ((proposal (and (hash-table-p result) (gethash "proposal" result)))
         (review (and (hash-table-p result) (gethash "review" result)))
         (value (and (hash-table-p application) (gethash "value" application)))
         (selection (and (hash-table-p value) (gethash "selection_receipt" value))))
    (if (not (hash-table-p proposal)) #()
        (map 'vector
          (lambda (relationship ordinal)
            (let* ((ref (format nil "relationship:~d" ordinal))
                   (identity-trace (gethash "identity_trace" result))
                   (grounding (gethash "grounding" relationship))
                   (omission (and selection
                                  (find ref (gethash "omissions" selection #()) :test #'equal
                                        :key (lambda (row) (gethash "claim_ref" row)))))
                   (candidates
                     (unless omission
                       (loop for fact being the hash-values of (pai.context-graph::context-graph-facts graph)
                             when (and (equal (gethash "predicate" relationship) (gethash "predicate" fact))
                                       (equal (gethash "fact" relationship) (gethash "fact" fact))
                                       (find terminal-id (gethash "evidence_records" fact #())
                                             :key (lambda (row) (gethash "application_event_id" row))))
                               collect fact)))
                   (fact (when (= 1 (length candidates)) (first candidates))))
              (obj "claim_ref" ref "proposed_relationship" relationship
                   "identity_endpoints"
                   (obj "subject" (%cgl-identity-endpoint (gethash "subject_ref" relationship) identity-trace)
                        "object" (%cgl-identity-endpoint (gethash "object_ref" relationship) identity-trace)
                        "attributed_to" (%cgl-identity-endpoint
                                         (and (hash-table-p grounding)
                                              (gethash "attributed_to_ref" grounding))
                                         identity-trace))
                   "review" (or (and review
                                  (find ref (gethash "claim_reviews" review #()) :test #'equal
                                        :key (lambda (row) (gethash "claim_ref" row)))) :null)
                   "selection" (cond (omission "omitted") (fact "retained") (t "unknown"))
                   "omission" (or omission :null)
                   "join_basis" "exact-terminal-id-predicate-statement"
                   "candidate_fact_ids" (coerce (mapcar (lambda (row) (gethash "fact_id" row)) candidates) 'vector)
                   "applied_fact_id" (if fact (gethash "fact_id" fact) :null)
                   "graph_edge" (if fact (%cgel-edge graph fact) :null)
                   "accepted_evidence_records" (if fact (gethash "evidence_records" fact #()) #())
                   "unknown_reason" (cond (omission :null) (fact :null)
                                            (candidates "ambiguous-provenance-join")
                                            (t "no-exact-provenance-join; not-proof-of-absence")))))
          (gethash "relationships" proposal #())
          (coerce (loop for i below (length (gethash "relationships" proposal #())) collect i) 'vector)))))

(defun %cgl-add-query-links (result)
  (loop for batch across (or (gethash "admission_trace" result) #()) do
    (loop for claim across (gethash "claim_trace" batch #())
          for id = (gethash "applied_fact_id" claim) do
      (setf (gethash "query_visibility" claim)
            (if (not (stringp id)) :null
                (map 'vector
                  (lambda (query)
                    (obj "request" (gethash "request" query)
                         "returned_before" (if (find id (gethash "edges" (gethash "before" query) #())
                                                      :test #'equal :key (lambda (edge) (gethash "edge_id" edge))) :true :false)
                         "returned_after" (if (find id (gethash "edges" (gethash "after" query) #())
                                                     :test #'equal :key (lambda (edge) (gethash "edge_id" edge))) :true :false)
                         "scope" "bounded-query-result; not-returned-is-not-absence"))
                  (gethash "queries" result #()))))))
  result)

(defun %cgl-evidence-inventory (runtime)
  "All accepted records, not the four-source visualizer anchor prefix."
  (coerce
   (sort
    (loop for fact being the hash-values of
          (pai.context-graph::context-graph-facts (pai.context-graph::context-graph-runtime-graph runtime))
          for records = (gethash "evidence_records" fact #()) collect
      (obj "fact_id" (gethash "fact_id" fact)
           "accepted_evidence_digest" (gethash "accepted_evidence_digest" fact :null)
           "records" (pai.context-graph::%cg-detach records)
           "source_ids" (coerce (sort (remove-duplicates
             (loop for record across records append
               (loop for span across (gethash "accepted_sources" record #())
                     collect (gethash "source_id" span))) :test #'equal) #'string<) 'vector)))
    #'string< :key (lambda (row) (gethash "fact_id" row))) 'vector))

(defun context-graph-lab-replay (frame events &optional (queries #()) counterfactual run-id)
  "Consume exact recorded events against isolated checkpoint copies. No generation."
  (let* ((envelope (gethash "envelope" frame)) (digest (gethash "digest" frame))
         (contract (gethash "contract" frame)) (cut (gethash "cutoff" contract))
         (started (get-internal-real-time))
         (before-runtime nil) (before-owner nil) (source-index nil)
         (runtime nil) (owner nil) (index nil) (last-id cut) (failure nil))
    (declare (ignorable source-index))
    (multiple-value-setq (before-runtime before-owner source-index)
      (pai.context-graph::context-graph-lab-checkpoint-open envelope digest contract))
    (multiple-value-setq (runtime owner index)
      (pai.context-graph::context-graph-lab-checkpoint-open envelope digest contract))
    (let* ((graph (pai.context-graph::context-graph-runtime-graph before-runtime))
           (saved (pai.context-graph::context-graph-projection-digest graph))
           (computed (pai.context-graph::%cg-authority-projection-digest
                      graph (gethash "agent_id" contract) (gethash "persona_id" contract))))
      (when (and saved (not (equal saved computed)))
        (error "Restored graph differs from the production fold's projection digest")))
    (when counterfactual
      (unless (and (integerp (gethash "event_id" counterfactual))
                   (hash-table-p (gethash "response" counterfactual))
                   (find (gethash "event_id" counterfactual) events
                         :key (lambda (event) (gethash "id" event))))
        (error "Counterfactual requires one selected phase response")))
    (loop for original across events
          for event = (pai.context-graph::%cg-detach original) do
      (let ((id (gethash "id" event)))
        (unless (and (integerp id) (> id last-id))
          (error "Recorded case events are out of order or precede baseline"))
        (handler-case
            (progn
              (unless (and (equal (gethash "agent_id" contract) (gethash "agent_id" event))
                           (equal (gethash "persona_id" contract) (gethash "persona_id" (gethash "payload" event)))
                           (member (gethash "type" event)
                                   '("context-graph-identity-opened" "context-graph-identity-phase"
                                     "context-graph-identity-completed" "context-graph-identity-failed") :test #'equal)
                           (equal (gethash "generation" contract) (gethash "generation" (gethash "payload" event))))
                (error "Recorded case event violates selected partition/generation"))
              (when (and counterfactual (= id (gethash "event_id" counterfactual)))
                (let ((record (pai.context-graph::%cgro-record event)))
                  (unless (and (equal "context-graph-identity-phase" (gethash "type" event))
                               (equal "response" (gethash "outcome" record)))
                    (error "Counterfactual target is not a phase response"))
                  (setf (gethash "response" record) (pai.context-graph::%cg-detach (gethash "response" counterfactual))
                        (gethash "record_json" (gethash "payload" event))
                        (pai.context-graph::context-graph-runtime-json record))))
              (when (and counterfactual (> id (gethash "event_id" counterfactual))
                         (equal "context-graph-identity-completed" (gethash "type" event)))
                ;; Rebuild deterministic completion only. Changed downstream model
                ;; requests still refuse against their recorded request digests.
                (let ((next (pai.context-graph::%cgi-owner-next owner (gethash "caused_by" event))))
                  (unless (equal "complete" (gethash "status" next))
                    (error "Counterfactual requires a changed downstream phase"))
                  (setf (gethash "record_json" (gethash "payload" event))
                        (pai.context-graph::context-graph-runtime-json
                         (obj "result" (gethash "result" next))))))
              (pai.context-graph::%cgi-owner-consume
               owner event
               (lambda (graph episode now)
                 (%ccg-source-context graph episode now index
                                      (gethash "agent_id" contract) (gethash "persona_id" contract))))
              (%ccg-apply-confirmation-resolution runtime event index
                                                  (gethash "agent_id" contract) (gethash "persona_id" contract))
              (setf last-id id))
          (error (condition)
            (let ((expected (ignore-errors (pai.context-graph::%cgi-owner-next owner (gethash "caused_by" event))))
                  (record (ignore-errors (pai.context-graph::%cgro-record event))))
              (setf failure (obj "event_id" id "event_type" (gethash "type" event)
                                 "error" (princ-to-string condition)
                                 "expected_phase" (if expected (gethash "phase" expected :null) :null)
                                 "recorded_phase" (if record (gethash "phase" record :null) :null)
                                 "request_digest_matches"
                                 (if (and expected record (gethash "request_digest" expected))
                                     (if (equal (gethash "request_digest" expected) (gethash "request_digest" record)) :true :false)
                                     :null))))
            (return)))))
    (setf (pai.context-graph::context-graph-runtime-opens runtime)
          (pai.context-graph::cgi-owner-opens owner)
          (pai.context-graph::context-graph-runtime-last-event-id runtime) last-id)
    (let* ((before (%cgl-case-lab before-runtime before-owner cut))
           (after (%cgl-case-lab runtime owner last-id))
           (pending (unless failure
                      (loop for opening-id being the hash-keys of (pai.context-graph::cgi-owner-opens owner)
                            unless (gethash opening-id (pai.context-graph::cgi-owner-terminals owner))
                              collect (obj "opening_id" opening-id
                                           "next" (pai.context-graph::%cgi-owner-next owner opening-id)))))
           (before-view (%cgel-graph-at before cut))
           (after-view (unless failure (%cgel-graph-at after last-id))))
      (setf (gethash "evidence" before-view) (%cgl-evidence-inventory before-runtime))
      (when after-view (setf (gethash "evidence" after-view) (%cgl-evidence-inventory runtime)))
      ;; The original frame must still open against its pinned manifest digest.
      (pai.context-graph::context-graph-lab-checkpoint-open envelope digest contract)
      (unless failure (%cgl-cache-run run-id before after cut last-id))
      (%cgl-add-query-links (obj "mode" (cond
                                           ((and counterfactual
                                                 (equal "fresh-selected-phase"
                                                        (gethash "provenance" counterfactual)))
                                            "fresh-selected-phase")
                                           (counterfactual "counterfactual-synthetic")
                                           (t "recorded"))
           "run_id" (or run-id :null)
           "counterfactual" (or counterfactual :null)
           "status" (cond (failure "refused") (pending "awaiting-phase") (t "complete"))
           "pending" (coerce pending 'vector)
           "contract" contract "baseline_digest" digest "failure" (or failure :null)
           "input_events_digest"
           (pai.context-graph::%cg-sha256 "case-events-v1"
             (pai.context-graph::%cgl-json (pai.context-graph::context-graph-lab-encode events)))
           "input_queries_digest"
           (pai.context-graph::%cg-sha256 "case-queries-v1"
             (pai.context-graph::%cgl-json (pai.context-graph::context-graph-lab-encode queries)))
           "before" before-view "after" (or after-view :null)
           "delta" (unless failure
                     (obj "nodes" (%cgel-mark-delta (gethash "nodes" before-view)
                                                     (gethash "nodes" after-view) "node_id")
                          "edges" (%cgel-mark-delta (gethash "edges" before-view)
                                                     (gethash "edges" after-view) "edge_id")))
           "formation_attempts" (unless failure (%cgel-formation-attempts after cut last-id))
           "queries"
           (unless failure
             (map 'vector (lambda (query)
                            (obj "request" query
                                 "before" (context-graph-episode-lab-query before cut query)
                                 "after" (context-graph-episode-lab-query after last-id query))) queries))
           "admission_trace"
           (unless failure
             (coerce
              (loop for opening-id being the hash-keys of (pai.context-graph::cgi-owner-opens owner)
                    for terminal = (gethash opening-id (pai.context-graph::cgi-owner-terminals owner))
                    when (and terminal (> (gethash "id" terminal) cut))
                      collect
                      (let ((result (gethash "result" (pai.context-graph::%cgro-record terminal))))
                        (obj "opening_id" opening-id
                             "claim_trace" (%cgl-linked-claims result
                                              (gethash opening-id (pai.context-graph::cgi-owner-applications owner))
                                              (pai.context-graph::context-graph-runtime-graph runtime)
                                              (gethash "id" terminal))
                             "identity_trace" (if result (gethash "identity_trace" result :null) :null)
                             "proposal" (if result (gethash "proposal" result :null) :null)
                             "review" (if result (gethash "review" result :null) :null)
                             "application" (gethash opening-id (pai.context-graph::cgi-owner-applications owner) :null))))
              'vector))
           "elapsed_seconds" (/ (- (get-internal-real-time) started)
                                (float internal-time-units-per-second))
           "provider_calls" 0 "historical_fold_count" 0 "authority_writes" 0)))))

(defun %cgl-result-equal-p (left right)
  "Compare bounded lab result data, not authority request envelopes."
  (let ((visited 0))
    (labels ((same (a b depth)
               (when (or (> (incf visited) 1000000) (> depth 96))
                 (error "Lab comparison exceeds structural bound"))
               (cond ((and (stringp a) (stringp b)) (string= a b))
                     ((and (hash-table-p a) (hash-table-p b))
                      (and (= (hash-table-count a) (hash-table-count b))
                           (loop for key being the hash-keys of a using (hash-value value)
                                 always (multiple-value-bind (other found) (gethash key b)
                                          (and found (same value other (1+ depth)))))))
                     ((and (vectorp a) (not (stringp a)) (vectorp b) (not (stringp b)))
                      (and (= (length a) (length b))
                           (loop for x across a for y across b always (same x y (1+ depth)))))
                     ((and (consp a) (consp b))
                      (and (same (car a) (car b) (1+ depth)) (same (cdr a) (cdr b) (1+ depth))))
                     (t (eql a b)))))
      (same left right 0))))

(defun context-graph-lab-compare (baseline candidate &optional expected-changes)
  "Compare identical recorded inputs. An explicit expected delta is the oracle."
  (unless (and (equal "complete" (gethash "status" baseline))
               (equal "complete" (gethash "status" candidate))
               (stringp (gethash "input_events_digest" baseline))
               (stringp (gethash "input_queries_digest" baseline))
               (equal (gethash "input_events_digest" baseline) (gethash "input_events_digest" candidate))
               (equal (gethash "input_queries_digest" baseline) (gethash "input_queries_digest" candidate))
               (hash-table-p (gethash "contract" baseline))
               (%cgl-result-equal-p (gethash "contract" baseline) (gethash "contract" candidate))
               (equal (gethash "baseline_digest" baseline) (gethash "baseline_digest" candidate)))
    (error "Comparison requires completed runs of identical baseline and recorded inputs"))
  (unless (and (vectorp (gethash "evidence" (gethash "after" baseline)))
               (vectorp (gethash "evidence" (gethash "after" candidate))))
    (error "Comparison requires full evidence inventories; older artifacts are inspection-only"))
  (let ((changes nil) (unchanged 0) (evidence-loss nil))
    (unless (%cgl-result-equal-p (gethash "admission_trace" baseline #())
                              (gethash "admission_trace" candidate #()))
      (push (obj "key" "admission:changed" "kind" "admission" "status" "changed"
                 "before" (gethash "admission_trace" baseline #())
                 "after" (gethash "admission_trace" candidate #())) changes))
    (let ((candidate-edges (make-hash-table :test #'equal)))
      (loop for edge across (gethash "evidence" (gethash "after" candidate)) do
        (setf (gethash (gethash "fact_id" edge) candidate-edges) edge))
      (loop for edge across (gethash "evidence" (gethash "after" baseline))
            for id = (gethash "fact_id" edge)
            for after = (gethash id candidate-edges)
            for lost = (set-difference (coerce (gethash "source_ids" edge #()) 'list)
                                      (coerce (if after (gethash "source_ids" after #()) #()) 'list)
                                      :test #'equal)
            when lost do
              (push (obj "edge_id" id "removed_source_ids" (coerce lost 'vector)
                         "edge_removed" (if after :false :true)) evidence-loss)))
    (let ((left (gethash "queries" baseline #())) (right (gethash "queries" candidate #())))
      (unless (= (length left) (length right)) (error "Comparison query sets differ"))
      (loop for a across left for b across right for i from 0 do
        (unless (%cgl-result-equal-p a b)
          (push (obj "key" (format nil "queries:~d:changed" i) "kind" "queries"
                     "status" "changed" "before" a "after" b) changes))))
    (loop for (kind key) in '(("nodes" "node_id") ("edges" "edge_id") ("evidence" "fact_id")) do
      (loop for row across (%cgel-mark-delta (gethash kind (gethash "after" baseline))
                                            (gethash kind (gethash "after" candidate)) key #'%cgl-result-equal-p)
            for status = (gethash "change" row) do
              (if (equal "unchanged" status) (incf unchanged)
                  (push (obj "key" (format nil "~a:~a:~a" kind (gethash key row) status)
                             "kind" kind "status" status "row" row) changes))))
    (let* ((keys (sort (mapcar (lambda (row) (gethash "key" row)) changes) #'string<))
           (expected (and expected-changes (sort (coerce expected-changes 'list) #'string<)))
           (unexpected (set-difference keys expected :test #'equal))
           (missing (set-difference expected keys :test #'equal)))
      (obj "status" (if expected-changes
                         (if (or unexpected missing) "failed" "passed") "comparison-only")
           "unchanged_count" unchanged "changed_count" (length changes)
           "changes" (coerce (nreverse changes) 'vector)
           "evidence_loss" (coerce (nreverse evidence-loss) 'vector)
           "graph_delta" (obj "nodes" (%cgel-mark-delta (gethash "nodes" (gethash "after" baseline))
                                                          (gethash "nodes" (gethash "after" candidate)) "node_id" #'%cgl-result-equal-p)
                              "edges" (%cgel-mark-delta (gethash "edges" (gethash "after" baseline))
                                                          (gethash "edges" (gethash "after" candidate)) "edge_id" #'%cgl-result-equal-p))
           "unexpected" (coerce unexpected 'vector) "missing_expected" (coerce missing 'vector)
           "baseline_mode" (gethash "mode" baseline) "candidate_mode" (gethash "mode" candidate)))))
