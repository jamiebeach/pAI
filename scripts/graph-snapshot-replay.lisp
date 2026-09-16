;;;; Explicit snapshot qualification, not a runtime initializer.
;;;; harness: full-system
(in-package :agent)

(defun graph-replay-final-context (records budget)
  (multiple-value-bind (merged evidence)
      (%conversation-merge-graph-memory-records records #() budget)
    (let ((sections (make-hash-table :test #'equal))
          (budgets (make-hash-table :test #'equal)))
      (loop for name across *conscious-context-section-order*
            do (setf (gethash name sections) #() (gethash name budgets) budget))
      (setf (gethash "memory-bundles" sections) merged)
      (conscious-context-assemble
       (obj "state_revision" 1 "composition_hash" "snapshot-replay")
       (make-conscious-assembly-context
        :pulse-id "pulse:snapshot-replay" :purpose "respond" :audience "operator"
        :runtime-revision "snapshot-replay" :conscious-state-revision 1
        :clock-identity "replay-clock" :total-character-budget budget
        :section-character-budgets budgets :sections sections
        :eligible-evidence-ids (coerce evidence 'vector) :available-tools #()
        :permitted-proposal-kinds #("publication-candidate")
        :publication-constraints (obj "audiences" #("operator"))
        :remaining-budget (obj "tool_proposals" 0 "continuations" 0
                               "publication_candidates" 1))))))

(let* ((config (shasht:read-json
                (uiop:read-file-string (uiop:getenv "PAI_GRAPH_REPLAY_CONFIG"))))
       (backend nil)
       (results '()))
  (%sqlite-load-library)
  (cffi:with-foreign-object (pointer :pointer)
    (setf (cffi:mem-ref pointer :pointer) (cffi:null-pointer))
    (unless (= +sqlite-ok+
               (%sqlite-open-v2 (gethash "database" config) pointer
                                (logior 1 +sqlite-open-fullmutex+)
                                (cffi:null-pointer)))
      (unless (cffi:null-pointer-p (cffi:mem-ref pointer :pointer))
        (%sqlite-close-v2 (cffi:mem-ref pointer :pointer)))
      (error "Read-only replay database could not open"))
    (setf backend (make-instance 'sqlite-derived-storage
                   :path (gethash "database" config)
                   :handle (cffi:mem-ref pointer :pointer))))
  (unwind-protect
      (loop for query across (gethash "queries" config)
            for start = (get-internal-real-time)
            do (let ((search-result nil))
                 (multiple-value-bind (records report)
                     (knowledge-graph-attention-context-records
                      (knowledge-graph-attention-frame
                       :attention-kind "conversation" :stimulus query
                       :memory-records
                       (vector (obj "source_id" "distractor-control"
                                    "content" "Graph retrieval failure timeout technical system condition")))
                      #() #()
                      (lambda (request semantic episodes)
                        (declare (ignore semantic episodes))
                        (setf search-result
                              (knowledge-graph-search-storage
                               backend (gethash "agent_id" config)
                               (gethash "persona_id" config) request
                               :event-storage-id (gethash "event_storage_id" config))))
                      :character-budget (gethash "character_budget" config))
                   (let* ((assembled (graph-replay-final-context
                                      records (gethash "character_budget" config)))
                          (request (gethash "private_request" assembled)))
                     ;; Structural acceptance is independent of semantic usefulness.
                     (unless (and (member (gethash "status" report)
                                          '("selected" "empty") :test #'equal)
                                  (= (length records) (length request))
                                  (every (lambda (row) (equal "memory-data" (gethash "role" row))) request)
                                  (equalp (map 'vector (lambda (row) (gethash "content" row)) records)
                                          (map 'vector (lambda (row) (gethash "content" row)) request))
                                  (= 0 (gethash "database_write_count" report -1))
                                  (every
                                   (lambda (row)
                                     (every (lambda (id)
                                              (find id (gethash "evidence_event_ids"
                                                               (gethash "manifest" assembled))
                                                    :test #'equal))
                                            (gethash "evidence_event_ids"
                                                     (gethash "provenance" row))))
                                   records))
                       (error "Final context failed structural replay checks"))
                     (push (obj "query" query "search_result" search-result
                                "attention_report" report "selected_records" records
                                "assembled" assembled
                                "elapsed_ms" (round (* 1000 (/ (- (get-internal-real-time) start)
                                                                internal-time-units-per-second))))
                           results)))))
    (storage-close backend))
  (with-open-file (stream (gethash "result" config) :direction :output
                          :if-exists :error :external-format :utf-8)
    (write-string (shasht:write-json (coerce (nreverse results) 'vector) nil) stream)))
