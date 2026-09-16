;;;; supplemental-memory-relevance-lab.lisp -- offline production-selector lab.

(require :asdf)
(load (uiop:getenv "PAI_QUICKLISP_SETUP"))
(push (pathname (uiop:getenv "PAI_REPOSITORY")) asdf:*central-registry*)
(asdf:load-system "pai")

(in-package :agent)

(defun smrl-read-json (path)
  (shasht:read-json (uiop:read-file-string path :external-format :utf-8)))

(defun smrl-write-json (path value)
  (with-open-file (out path :direction :output :if-exists :supersede
                            :if-does-not-exist :create :external-format :utf-8)
    (write-string (shasht:write-json value nil) out)))

(defun smrl-grounding-packet (packet)
  (obj
   "schema_version" 1
   "sources"
   (map 'vector
        (lambda (row)
          (obj "source_id" (gethash "source_id" row)
               "speaker_id" (gethash "speaker_id" row)
               "kind" (gethash "kind" row)
               "text" (gethash "text" row)
               "text_sha256" (gethash "text_sha256" row)))
        (gethash "evidence_records" packet))))

(defclass smrl-storage-backend (memory-storage-backend)
  ((events :initarg :events :reader smrl-backend-events)
   (semantic :initarg :semantic :reader smrl-backend-semantic)
   (lexical :initarg :lexical :reader smrl-backend-lexical)))

(defun smrl-memory-row (backend specification)
  (let* ((memory-id (gethash "memory_node_id" specification))
         (event
           (find memory-id (smrl-backend-events backend) :test #'string=
                 :key #'%kgfs-baseline-memory-id))
         (payload (and event (%kgfs-event-payload event)))
         (envelope (and (hash-table-p payload) (gethash "node" payload)))
         (memory
           (and (hash-table-p envelope)
                (shasht:read-json (gethash "scalar_json" envelope)))))
    (unless (hash-table-p memory)
      (error "Offline storage result has no matching baseline memory"))
    (obj
     "id" memory-id "distance" (gethash "distance" specification)
     "row"
     (obj "id" memory-id "kind" "observation"
          "content" (gethash "content" memory)
          "created_at" (gethash "created_at" memory)
          "observed_at" :null "valid_from" :null "valid_to" :null
          "last_accessed" (gethash "created_at" memory) "access_count" 0
          "importance" 0.5d0 "valence" 0.0d0
          "arousal_at_encoding" 0.0d0 "activation" 0.0d0
          "source_event_id" (format nil "event:~d" (gethash "id" event))
          "origin_class" (gethash "origin_class" memory)
          "epistemic_status" "user-report" "producer" "offline-fixture"
          "model_purpose" :null "confidence" 1.0d0
          "grounding_status" (gethash "grounding_status" memory)
          "root_observation_ids" #() "generation_id" :null
          "supersedes_node_id" :null "quarantined" nil
          "epistemic_metadata" (gethash "epistemic_metadata" memory)))))

(defmethod memory-storage-exact-search
    ((backend smrl-storage-backend) (query memory-exact-query))
  (declare (ignore query))
  (obj "schema_version" 1 "backend" "offline-fixture"
       "exact_scan_forced" t
       "results" (map 'vector (lambda (row) (smrl-memory-row backend row))
                      (smrl-backend-semantic backend))))

(defmethod memory-storage-lexical-search
    ((backend smrl-storage-backend) (query memory-exact-query))
  (declare (ignore query))
  (obj
   "schema_version" 1 "backend" "offline-fixture"
   "results"
   (map 'vector
        (lambda (specification)
          (let ((row (smrl-memory-row backend specification)))
            (setf (gethash "lexical_tier" row)
                  (gethash "lexical_tier" specification)
                  (gethash "lexical_match_count" row)
                  (gethash "lexical_match_count" specification)
                  (gethash "lexical_coverage" row)
                  (gethash "lexical_coverage" specification)
                  (gethash "lexical_terms" row)
                  (gethash "lexical_terms" specification))
            row))
        (smrl-backend-lexical backend))))

(defun smrl-process-case (case)
  (let* ((episode (gethash "episode_event" case))
         (events (coerce (gethash "events" case) 'list))
         (retrieval (gethash "retrieval" case))
         (*memory-search-storage-backend*
           (make-instance 'smrl-storage-backend :events events
             :semantic (gethash "semantic_results" retrieval)
             :lexical (gethash "lexical_results" retrieval)))
         (*memory-search-query-vector-fn*
           (lambda (query typed-p)
             (declare (ignore query typed-p)) #(1.0d0 0.0d0)))
         (*memory-recall-clock-fn*
           (lambda () (encode-universal-time 0 0 12 2 9 2026 0)))
         (*retrieval-embedding-mode* :enforced)
         (candidates nil)
         (discovery nil)
         (report nil))
    (multiple-value-setq (candidates discovery)
      (knowledge-graph-formation-discover-supplemental-memory-candidates
       episode events #'memory-search))
    (unless
        (equalp
         (gethash "expected_discovered_memory_node_ids" case)
         (map 'vector (lambda (candidate)
                        (gethash "memory_node_id" candidate))
              candidates))
      (error "Supplemental discovery did not match the fixed expectation"))
    (setf report
          (knowledge-graph-formation-select-supplemental-memories
           episode candidates
           :candidate-status (gethash "status" discovery)
           :maximum (gethash "maximum" case)))
    (let* ((selected (gethash "selected" report))
         (selected-events
           (map 'vector (lambda (row) (gethash "event" row)) selected))
         (packet
           (knowledge-graph-formation-select-episode-source
            events (gethash "agent_id" case) (gethash "persona_id" case)
            (lambda (selected-episode)
              (declare (ignore selected-episode)) #())
            :supplemental-memory-selector-fn
            (lambda (selected-episode all-events)
              (declare (ignore selected-episode all-events)) selected-events)))
         (selected-memory-ids (gethash "source_memory_node_ids" packet))
         (expected (gethash "expected_selected_memory_node_ids" case)))
    (unless (equalp expected selected-memory-ids)
      (error "Supplemental selection did not match the fixed expectation"))
    (let ((graph
            (pai.context-graph:make-context-graph (gethash "ontology" case))))
      (pai.context-graph:context-graph-apply-episode
       graph (gethash "graph_episode" case) (gethash "proposal" case)
       :source-packet (smrl-grounding-packet packet)
       :reuse-exact-identities-p nil)
      (let ((queries
              (map 'vector
                   (lambda (query-case)
                     (let* ((query (gethash "query" query-case))
                            (result
                              (pai.context-graph:context-graph-search
                               graph query :evidence-policy "verified"
                               :claim-policy "grounded-assertions"))
                            (count (gethash "result_count" result)))
                       (unless (= count (gethash "expected_result_count"
                                                query-case))
                         (error "Held-out graph retrieval expectation failed"))
                       (obj "query" query "result_count" count)))
                   (gethash "held_out_queries" case))))
        (obj "case_id" (gethash "case_id" case)
             "discovery" discovery
             "selection" report
             "source_memory_node_ids" selected-memory-ids
             "source_packet_valid"
             (if (knowledge-graph-formation-source-packet-valid-p packet) t nil)
             "held_out_queries" queries))))))

(let* ((bundle (smrl-read-json (uiop:getenv "PAI_SUPPLEMENTAL_MEMORY_BUNDLE")))
       (output (uiop:getenv "PAI_SUPPLEMENTAL_MEMORY_OUTPUT"))
       (results (map 'vector #'smrl-process-case (gethash "cases" bundle))))
  (smrl-write-json
   output
   (obj "schema_version" 1 "execution" "production-lisp-offline"
        "case_count" (length results) "cases" results))
  (format t "SUPPLEMENTAL-MEMORY-RELEVANCE-LAB-OK cases=~d~%"
          (length results)))
