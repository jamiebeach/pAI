(defpackage :agent (:use :cl))
(in-package :agent)

(ql:quickload '(:bordeaux-threads :shasht :ironclad :uiop) :silent t)

(defun r0-bootstrap-tail-materializer-object (&rest pairs)
  (loop with result = (make-hash-table :test #'equal)
        for (key value) on pairs by #'cddr
        do (setf (gethash key result) value)
        finally (return result)))

(setf (fdefinition 'obj) #'r0-bootstrap-tail-materializer-object
      (fdefinition 'auto-turn) (lambda (prompt) prompt)
      (fdefinition 'execute) (lambda (&rest values)
                               (declare (ignore values)) nil)
      (fdefinition 'propose-loop) (lambda (&rest values)
                                    (declare (ignore values)) nil))
(defparameter *tools* (vector))

(load (test-source "event-log.lisp"))
(load (test-source "projection-rebuild.lisp"))

(let* ((boundary
         (parse-integer
          (or (uiop:getenv "R0_BOOTSTRAP_BOUNDARY")
              (error "R0_BOOTSTRAP_BOUNDARY is required"))))
       (through
         (parse-integer
          (or (uiop:getenv "R0_BOOTSTRAP_THROUGH")
              (error "R0_BOOTSTRAP_THROUGH is required"))))
       (candidate-root #P"/agent/state/")
       (scratch-root #P"/scratch/")
       (*event-checkpoint-directory*
         (merge-pathnames "checkpoints/" scratch-root))
       (event-count 0)
       (source
         (make-projection-rebuild-event-source
          :after-id boundary :through-id through))
       (file-baselines (projection-rebuild-load-file-checkpoints))
       (file-result
         (projection-rebuild-fold-files
          source :baselines file-baselines :expected-tail-event-id through))
       (file-parity (projection-rebuild-file-parity file-result candidate-root))
       (table-baselines
         (let ((baselines (make-hash-table :test #'equal)))
           ;; The R0e4 exact-line checkpoints are streamed rather than loaded
           ;; into Lisp. All twelve registered tables were independently
           ;; verified at zero rows at this boundary, so their pure tail fold
           ;; starts from explicit empty states.
           (dolist (spec (projection-rebuild-postgres-table-specs) baselines)
             (let ((table (getf spec :table)))
               (setf (gethash table baselines)
                     (make-projection-rebuild-table-baseline
                      boundary table (vector)))))))
       (table-result
         (projection-rebuild-fold-postgres-tables
          source :baselines table-baselines :expected-tail-event-id through))
       (table-spool (merge-pathnames "sql/tail-postgres-tables.sql"
                                     scratch-root)))
  (multiple-value-bind (complete last-id visited)
      (map-events (lambda (event)
                    (declare (ignore event))
                    (incf event-count))
                  :after-id boundary :through-id through)
    (unless (and complete (= last-id through) (= visited 12)
                 (= event-count 12))
      (error "Bounded candidate tail scan failed")))
  (unless (and (gethash "complete" file-result)
               (= 8 (length file-parity))
               (every (lambda (row) (gethash "equal" row)) file-parity))
    (error "Synthetic file tail parity failed"))
  (unless (gethash "complete" table-result)
    (error "Synthetic atom table tail fold failed: ~{~a~^; ~}"
           (mapcar
            (lambda (gap)
              (format nil "~a|~a|~a"
                      (gethash "projection" gap)
                      (gethash "reason" gap)
                      (gethash "event_id" gap)))
            (gethash "gaps" table-result))))
  (projection-rebuild-write-files
   file-result (merge-pathnames "tail-file-rebuild/" scratch-root))
  (multiple-value-bind (spool exact-rows terminal-id)
      (projection-rebuild-write-exact-postgres-tail
       source table-spool through :destination-schema "r0e4_rebuild")
    (let ((tail-sql (uiop:read-file-string spool)))
      (unless (and (= exact-rows 4) (= terminal-id through)
                   (search "0.001" tail-sql)
                   (null (search "9.9999994e-4" tail-sql)))
        (error "Exact atom tail spool contract failed")))
    (format t
            "R0_BOOTSTRAP_TAIL_MATERIALIZER_PASS boundary=~d through=~d events=~d file_parity=8 exact_atom_rows=~d~%"
            boundary through event-count exact-rows)))
