;;;; conscious-latency-benchmark.lisp -- disposable-clone latency evidence.

(in-package :agent)

(defun %clb-ms (thunk)
  (let ((started (get-internal-real-time)))
    (values (funcall thunk)
            (* 1000.0d0
               (/ (- (get-internal-real-time) started)
                  internal-time-units-per-second)))))

(defun %clb-percentile (samples fraction)
  (let* ((ordered (sort (copy-list samples) #'<))
         (index (min (1- (length ordered))
                     (floor (* fraction (length ordered))))))
    (nth index ordered)))

(defun %clb-summary (samples)
  (obj "iterations" (length samples)
       "minimum_ms" (first (sort (copy-list samples) #'<))
       "p50_ms" (%clb-percentile samples 0.50d0)
       "p95_ms" (%clb-percentile samples 0.95d0)
       "maximum_ms" (car (last (sort (copy-list samples) #'<)))))

(defun %clb-result-fingerprint (report)
  (let ((digest (ironclad:make-digest :sha256)))
    (loop for candidate across (gethash "results" report)
          do (ironclad:update-digest
              digest
              (babel:string-to-octets
               (format nil "~a|~,17g~%"
                       (gethash "id" candidate)
                       (gethash "distance" candidate))
               :encoding :utf-8)))
    (ironclad:byte-array-to-hex-string
     (ironclad:produce-digest digest))))

(defun %clb-turn-ids (report)
  (remove-duplicates
   (loop for candidate across (gethash "results" report)
         for row = (gethash "row" candidate)
         for metadata = (and (hash-table-p row)
                             (gethash "epistemic_metadata" row))
         for turn-id = (and (hash-table-p metadata)
                            (gethash "turn_id" metadata))
         when (and (stringp turn-id) (plusp (length turn-id)))
           collect turn-id)
   :test #'string=))

(defun %clb-idle-scheduler-probe ()
  (let ((work-projections 0)
        (operation-event-reads 0)
        (agent-id "latency-benchmark-fixture"))
    (unwind-protect
         (progn
           (conscious-work-executor-reset)
           (conscious-operation-executor-reset)
           (conscious-work-executor-configure
            :agent-id agent-id
            :prepare-fn
            (lambda () (obj "schema_version" 1 "status" "idle"))
            :projection-fn
            (lambda ()
              (incf work-projections)
              (conscious-work-project nil agent-id))
            :quantum-fn
            (lambda (&rest ignored)
              (declare (ignore ignored)) (error "unreachable")))
           (conscious-operation-executor-configure
            :agent-id agent-id
            :events-fn (lambda () (incf operation-event-reads) nil)
            :projection-fn (lambda () (conscious-work-project nil agent-id))
            :operation-fn
            (lambda (&rest ignored)
              (declare (ignore ignored)) (error "unreachable"))
            :transition-fn
            (lambda (&rest ignored)
              (declare (ignore ignored)) (error "unreachable"))
            :cognition-wake-fn
            (lambda (&rest ignored) (declare (ignore ignored))))
           (conscious-operation-executor-start)
           (conscious-work-executor-start)
           (sleep 2.2d0)
           (obj "observation_seconds" 2.2d0
                "cognitive_projection_reads" work-projections
                "operation_event_reads" operation-event-reads))
      (ignore-errors (conscious-work-executor-reset))
      (ignore-errors (conscious-operation-executor-reset)))))

(let* ((database
         (pathname (or (uiop:getenv "PAI_LATENCY_BENCHMARK_DATABASE")
                       (error "PAI_LATENCY_BENCHMARK_DATABASE is required"))))
       (output
         (pathname (or (uiop:getenv "PAI_LATENCY_BENCHMARK_OUTPUT")
                       (error "PAI_LATENCY_BENCHMARK_OUTPUT is required"))))
       (query "What relevant context should be remembered for this conversation?")
       (iterations 21)
       (backend nil))
  (unwind-protect
       (progn
         (setf backend (make-sqlite-derived-storage database))
         (multiple-value-bind (query-vector embedding-ms)
             (%clb-ms (lambda () (embed-retrieval-query query)))
           (sb-ext:gc :full t)
           (let ((heap-before (sb-kernel:dynamic-usage))
                 (cold-report nil)
                 (cold-ms nil))
             (multiple-value-setq (cold-report cold-ms)
               (%clb-ms
                (lambda ()
                  (memory-storage-exact-search
                   backend
                   (make-memory-exact-query
                    :vector-values query-vector :profile "safe-semantic-v1"
                    :limit 50 :hydrate-p t)))))
             (sb-ext:gc :full t)
             (let* ((heap-after (sb-kernel:dynamic-usage))
                    (fingerprint (%clb-result-fingerprint cold-report))
                    (semantic-samples nil)
                    (semantic-identical t)
                    (turn-ids (%clb-turn-ids cold-report))
                    (neighborhood-samples nil)
                    (neighborhood-fingerprint nil)
                    (neighborhood-identical t))
               (dotimes (index iterations)
                 (declare (ignorable index))
                 (multiple-value-bind (report elapsed)
                     (%clb-ms
                      (lambda ()
                        (memory-storage-exact-search
                         backend
                         (make-memory-exact-query
                          :vector-values query-vector
                          :profile "safe-semantic-v1"
                          :limit 50 :hydrate-p t))))
                   (push elapsed semantic-samples)
                   (unless (string= fingerprint
                                    (%clb-result-fingerprint report))
                     (setf semantic-identical nil))))
               (when turn-ids
                 (dotimes (index iterations)
                   (declare (ignorable index))
                   (multiple-value-bind (report elapsed)
                       (%clb-ms
                        (lambda ()
                          (memory-storage-exact-search
                           backend
                           (make-memory-exact-query
                            :vector-values query-vector
                            :profile "turn-neighborhood-v1"
                            :turn-ids turn-ids :limit 200 :hydrate-p t
                            :include-vector-p t))))
                     (let ((current (%clb-result-fingerprint report)))
                       (if neighborhood-fingerprint
                           (unless (string= neighborhood-fingerprint current)
                             (setf neighborhood-identical nil))
                           (setf neighborhood-fingerprint current)))
                     (push elapsed neighborhood-samples))))
               (let* ((*memory-search-storage-backend* backend)
                      (*retrieval-embedding-mode* :enforced)
                      (*context-projection-mode* :enforced)
                      (*context-curator-mode* :off)
                      (*context-projection-events-fn*
                        (lambda (&rest ignored)
                          (declare (ignore ignored)) nil))
                      (*context-projection-soul-fn* (lambda () nil))
                      (*context-projection-active-questions-fn*
                        (lambda () nil))
                      (*context-projection-modulator-fn* (lambda () nil))
                      (*context-projection-drive-fn* (lambda () nil))
                      (*embedding-turn-cache* (make-hash-table :test #'equal))
                      (context-samples nil)
                      (hit-before *embedding-turn-cache-hits*)
                      (miss-before *embedding-turn-cache-misses*))
                 ;; First projection populates work-scoped embedding state.
                 (render-context-projection
                  (build-context-projection query :mode :enforced))
                 (dotimes (index iterations)
                   (declare (ignorable index))
                   (multiple-value-bind (ignored elapsed)
                       (%clb-ms
                        (lambda ()
                          (render-context-projection
                           (build-context-projection query :mode :enforced))))
                     (declare (ignore ignored))
                     (push elapsed context-samples)))
                 (let ((result
                         (obj
                          "schema_version" 1
                          "benchmark" "conscious-latency-disposable-clone-v1"
                          "source_revision"
                          (or (uiop:getenv "PAI_LATENCY_SOURCE_REVISION")
                              "working-tree")
                          "database_bytes"
                          (with-open-file (stream database
                                                  :direction :input
                                                  :element-type '(unsigned-byte 8))
                            (file-length stream))
                          "node_count"
                          (length
                           (%sqlite-exact-memory-cache-entries
                            (%sqlite-derived-exact-memory-cache backend)))
                          "embedding_ms" embedding-ms
                          "cold_semantic_ms" cold-ms
                          "cache_retained_heap_bytes"
                          (max 0 (- heap-after heap-before))
                          "warm_semantic" (%clb-summary semantic-samples)
                          "warm_semantic_identical" semantic-identical
                          "turn_id_count" (length turn-ids)
                          "warm_neighborhood"
                          (if neighborhood-samples
                              (%clb-summary neighborhood-samples) :null)
                          "warm_neighborhood_identical"
                          (if neighborhood-samples
                              neighborhood-identical :null)
                          "warm_context_projection"
                          (%clb-summary context-samples)
                          "work_embedding_cache_hits"
                          (- *embedding-turn-cache-hits* hit-before)
                          "work_embedding_cache_misses"
                          (- *embedding-turn-cache-misses* miss-before)
                          "exact_cache_builds"
                          (%sqlite-derived-exact-cache-builds backend)
                          "exact_cache_hits"
                          (%sqlite-derived-exact-cache-hits backend)
                          "idle_scheduler" (%clb-idle-scheduler-probe))))
                   (ensure-directories-exist output)
                   (with-open-file
                       (stream output :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :create
                                      :external-format :utf-8)
                     (write-string (shasht:write-json result nil) stream)
                     (terpri stream))
                   (format t "Latency benchmark written to ~a.~%" output)))))))
    (when backend (ignore-errors (storage-close backend)))))
