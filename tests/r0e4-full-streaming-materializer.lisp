(defpackage :agent (:use :cl))
(in-package :agent)

(ql:quickload '(:bordeaux-threads :shasht :ironclad :uiop) :silent t)

(defun r0e4-full-object (&rest pairs)
  (loop with result = (make-hash-table :test #'equal)
        for (key value) on pairs by #'cddr
        do (setf (gethash key result) value)
        finally (return result)))

(setf (fdefinition 'obj) #'r0e4-full-object
      (fdefinition 'auto-turn) (lambda (prompt) prompt)
      (fdefinition 'execute) (lambda (&rest arguments)
                               (declare (ignore arguments)) nil)
      (fdefinition 'propose-loop) (lambda (&rest arguments)
                                    (declare (ignore arguments)) nil))
(defparameter *tools* (vector))

(load (test-source "event-log.lisp"))
(load (test-source "projection-rebuild.lisp"))

(let* ((root (merge-pathnames "r0e4-full/" (test-state-dir)))
       (input-root (merge-pathnames "input/" root))
       (checkpoint-root (merge-pathnames "checkpoints/" root))
       (sql-root (merge-pathnames "sql/" root))
       (summary-path (merge-pathnames "summary.json" root))
       (*event-checkpoint-directory* checkpoint-root)
       (boundary 106710)
       (tables
         '(("memory-nodes" . "memory_nodes")
           ("memory-edges" . "memory_edges")
           ("grounded_project_proposals" . "grounded_project_proposals")
           ("agent_processes" . "agent_processes")
           ("creative_projects" . "creative_projects")
           ("agent_process_operations" . "agent_process_operations")
           ("agent_artifacts" . "agent_artifacts")
           ("agent_artifact_versions" . "agent_artifact_versions")
           ("agent_attestations" . "agent_attestations")
           ("publication_candidates" . "publication_candidates")))
       (started (get-internal-real-time))
       (consed-before (sb-ext:get-bytes-consed))
       (peak-usage (sb-kernel:dynamic-usage))
       (results nil))
  (ensure-directories-exist (merge-pathnames "placeholder" sql-root))
  (dolist (mapping tables)
    (let* ((projection (car mapping))
           (table (cdr mapping))
           (input (merge-pathnames (format nil "~a.jsonl" table) input-root))
           (sql (merge-pathnames (format nil "~a.sql" table) sql-root))
           (rows-seen 0)
           (source (make-event-jsonl-line-source input))
           (monitored-source
             (lambda (visitor)
               (funcall source
                        (lambda (line)
                          (funcall visitor line)
                          (incf rows-seen)
                          (when (zerop (mod rows-seen 100))
                            (setf peak-usage
                                  (max peak-usage
                                       (sb-kernel:dynamic-usage)))))))))
      (let ((manifest
              (write-event-row-checkpoint-lines
               projection boundary monitored-source)))
        (setf peak-usage (max peak-usage (sb-kernel:dynamic-usage)))
        (multiple-value-bind (path verified-manifest inserted)
            (projection-rebuild-write-row-checkpoint-inserts
             projection sql table)
          (declare (ignore path))
          (unless (and (= inserted (gethash "row_count" manifest))
                       (= boundary (gethash "event_id" verified-manifest)))
            (error "Streaming count/boundary mismatch for ~a" projection))
          (push (r0e4-full-object
                 "projection" projection "table" table
                 "rows" inserted
                 "checkpoint_bytes" (gethash "byte_length" manifest)
                 "checkpoint_sha256" (gethash "sha256" manifest)
                 "sql_bytes" (with-open-file (in sql :direction :input)
                               (file-length in)))
                results)))))
  (setf peak-usage (max peak-usage (sb-kernel:dynamic-usage)))
  (let* ((elapsed (/ (- (get-internal-real-time) started)
                     internal-time-units-per-second))
         (summary
           (r0e4-full-object
            "schema_version" 1 "event_id" boundary
            "elapsed_seconds" elapsed
            "peak_dynamic_usage_bytes" peak-usage
            "dynamic_space_bytes" (sb-ext:dynamic-space-size)
            "bytes_consed" (- (sb-ext:get-bytes-consed) consed-before)
            "tables" (coerce (nreverse results) 'vector))))
    (%event-atomic-write-json summary-path summary)
    (format t "R0e4 full streaming spools complete: ~,3fs; peak sampled usage ~d bytes.~%"
            elapsed peak-usage)))
