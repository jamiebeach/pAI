(defpackage :agent (:use :cl))
(in-package :agent)

(ql:quickload '(:bordeaux-threads :shasht :ironclad :uiop) :silent t)

(defun r0-bootstrap-object (&rest pairs)
  (loop with result = (make-hash-table :test #'equal)
        for (key value) on pairs by #'cddr
        do (setf (gethash key result) value)
        finally (return result)))

(setf (fdefinition 'obj) #'r0-bootstrap-object
      (fdefinition 'auto-turn) (lambda (prompt) prompt)
      (fdefinition 'execute) (lambda (&rest values)
                               (declare (ignore values)) nil)
      (fdefinition 'propose-loop) (lambda (&rest values)
                                    (declare (ignore values)) nil))
(defparameter *tools* (vector))

(load (test-source "event-log.lisp"))
(load (test-source "projection-rebuild.lisp"))
(load (merge-pathnames "r0-bootstrap-checkpoint-contract.lisp" *load-truename*))

(let* ((boundary
         (parse-integer
          (or (uiop:getenv "R0_BOOTSTRAP_BOUNDARY")
              (error "R0_BOOTSTRAP_BOUNDARY is required"))))
       (candidate-root #P"/candidate-state/")
       (scratch-root #P"/scratch/")
       (checkpoint-root (merge-pathnames "checkpoints/" scratch-root))
       (input-root (merge-pathnames "input/" scratch-root))
       (sql-root (merge-pathnames "sql/" scratch-root))
       (file-output-root (merge-pathnames "file-rebuild/" scratch-root))
       (summary-path (merge-pathnames "checkpoint-summary.json" scratch-root))
       (*event-checkpoint-directory* checkpoint-root)
       (mappings
         '(("memory-nodes" . "memory_nodes")
           ("memory-edges" . "memory_edges")
           ("memory_atom_rollouts" . "memory_atom_rollouts")
           ("memory_atom_jobs" . "memory_atom_jobs")
           ("memory_atom_candidates" . "memory_atom_candidates")
           ("memory_atom_candidate_roots" . "memory_atom_candidate_roots")
           ("grounded_project_proposals" . "grounded_project_proposals")
           ("agent_processes" . "agent_processes")
           ("creative_projects" . "creative_projects")
           ("agent_process_operations" . "agent_process_operations")
           ("agent_artifacts" . "agent_artifacts")
           ("agent_artifact_versions" . "agent_artifact_versions")
           ("agent_attestations" . "agent_attestations")
           ("publication_candidates" . "publication_candidates")))
       (table-names (mapcar #'cdr mappings))
       (expected-row-counts
         (r0-bootstrap-read-expected-row-counts
          (merge-pathnames "expected-row-counts.tsv" scratch-root)
          table-names))
       (expected-row-total
         (r0-bootstrap-expected-row-total expected-row-counts table-names))
       (started (get-internal-real-time))
       (consed-before (sb-ext:get-bytes-consed))
       (peak-usage (sb-kernel:dynamic-usage))
       (row-total 0)
       (row-results nil))
  (ensure-directories-exist (merge-pathnames "placeholder" checkpoint-root))
  (ensure-directories-exist (merge-pathnames "placeholder" sql-root))

  ;; Eight byte-exact file checkpoints at the same stopped boundary.
  (dolist (spec (projection-rebuild-file-specs))
    (let* ((name (getf spec :name))
           (file (getf spec :file))
           (path (merge-pathnames file candidate-root)))
      (unless (probe-file path)
        (error "Missing candidate projection file ~a" file))
      (write-event-checkpoint name boundary (uiop:read-file-string path))))
  (let* ((baselines (projection-rebuild-load-file-checkpoints))
         (result (projection-rebuild-fold-files
                  nil :baselines baselines :expected-tail-event-id boundary))
         (parity (projection-rebuild-file-parity result candidate-root)))
    (unless (and (gethash "complete" result)
                 (= 8 (length parity))
                 (every (lambda (row) (gethash "equal" row)) parity))
      (error "Candidate file checkpoint parity failed"))
    (projection-rebuild-write-files result file-output-root))

  ;; Fourteen exact-line relational checkpoints and constant-retention spools.
  (dolist (mapping mappings)
    (let* ((projection (car mapping))
           (table (cdr mapping))
           (input (merge-pathnames (format nil "~a.jsonl" table) input-root))
           (sql (merge-pathnames (format nil "~a.sql" table) sql-root))
           (rows-seen 0)
           (source (make-event-jsonl-line-source input))
           (monitored
             (lambda (visitor)
               (funcall source
                        (lambda (line)
                          (funcall visitor line)
                          (incf rows-seen)
                          (when (zerop (mod rows-seen 100))
                            (setf peak-usage
                                  (max peak-usage
                                       (sb-kernel:dynamic-usage)))))))))
      (unless (probe-file input)
        (error "Missing relational input ~a" table))
      (let ((manifest
              (write-event-row-checkpoint-lines
               projection boundary monitored)))
        (multiple-value-bind (path verified-manifest inserted)
            (projection-rebuild-write-row-checkpoint-inserts
             projection sql table)
          (declare (ignore path))
          (unless (and (= inserted rows-seen)
                       (= inserted (gethash "row_count" manifest))
                       (r0-bootstrap-row-count-matches-p
                        expected-row-counts table inserted)
                       (= boundary (gethash "event_id" verified-manifest)))
            (error "Checkpoint delivery mismatch for ~a" projection))
          (incf row-total inserted)
          (push (r0-bootstrap-object
                 "projection" projection "table" table "rows" inserted
                 "checkpoint_bytes" (gethash "byte_length" manifest)
                 "checkpoint_sha256" (gethash "sha256" manifest))
                row-results)))))

  (setf peak-usage (max peak-usage (sb-kernel:dynamic-usage)))
  (let* ((elapsed
           (/ (- (get-internal-real-time) started)
              (float internal-time-units-per-second 1.0d0)))
         (manifest-count
           (length (directory (merge-pathnames "checkpoint-*.json"
                                               checkpoint-root))))
         (summary
           (r0-bootstrap-object
            "schema_version" 1
            "status" "pass"
            "boundary_event_id" boundary
            "checkpoint_count" manifest-count
            "file_checkpoint_count" 8
            "relational_checkpoint_count" 14
            "relational_rows" row-total
            "elapsed_seconds" elapsed
            "peak_dynamic_usage_bytes" peak-usage
            "dynamic_space_bytes" (sb-ext:dynamic-space-size)
            "bytes_consed" (- (sb-ext:get-bytes-consed) consed-before)
            "tables" (coerce (nreverse row-results) 'vector))))
    (unless (and (= manifest-count 22) (= row-total expected-row-total)
                 (<= peak-usage (sb-ext:dynamic-space-size)))
      (error "Checkpoint count/resource contract failed"))
    (%event-atomic-write-json summary-path summary)
    (format t
            "R0_BOOTSTRAP_CHECKPOINT_PASS boundary=~d checkpoints=~d rows=~d elapsed=~,3f peak=~d consed=~d~%"
            boundary manifest-count row-total elapsed peak-usage
            (- (sb-ext:get-bytes-consed) consed-before))))
