(defpackage :agent (:use :cl))
(in-package :agent)

(ql:quickload '(:bordeaux-threads :shasht :ironclad) :silent t)

(defvar *r0e-checkpoint-pass* 0)
(defvar *r0e-checkpoint-fail* 0)

(defun r0e-checkpoint-check (name condition)
  (if condition
      (progn (incf *r0e-checkpoint-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *r0e-checkpoint-fail*) (format t "  FAIL ~a~%" name))))

(defun obj (&rest pairs)
  (loop with result = (make-hash-table :test #'equal)
        for (key value) on pairs by #'cddr
        do (setf (gethash key result) value)
        finally (return result)))

;; EVENT-LOG wrappers need the established call points, but this fixture only
;; exercises its checkpoint API.
(setf (fdefinition 'auto-turn) (lambda (prompt) prompt)
      (fdefinition 'execute) (lambda (&rest arguments)
                               (declare (ignore arguments)) nil)
      (fdefinition 'propose-loop) (lambda (&rest arguments)
                                    (declare (ignore arguments)) nil))
(defparameter *tools* (vector))

(load (test-source "event-log.lisp"))
(load (test-source "projection-rebuild.lisp"))

(let* ((root #P"/tmp/r0e-checkpoint-integration/")
       (checkpoint-directory (merge-pathnames "checkpoints/" root))
       (*event-checkpoint-directory* checkpoint-directory)
       (specs (projection-rebuild-file-specs)))
  (when (probe-file root)
    (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))
  (dolist (spec specs)
    (write-event-checkpoint
     (getf spec :name) 200
     (format nil "checkpoint/~a/unicode-π~a"
             (getf spec :name) (string #\Newline))))
  (let* ((baselines (projection-rebuild-load-file-checkpoints :specs specs))
         (result (projection-rebuild-fold-files
                  nil :baselines baselines :specs specs
                  :expected-tail-event-id 200)))
    (r0e-checkpoint-check "all eight verified checkpoints load"
                          (= 8 (hash-table-count baselines)))
    (r0e-checkpoint-check "checkpoint-only boundary is a complete rebuild"
                          (gethash "complete" result))
    (r0e-checkpoint-check
     "checkpoint bytes remain exact through the baseline adapter"
     (loop for spec in specs
           for expected = (format nil "checkpoint/~a/unicode-π~a"
                                  (getf spec :name) (string #\Newline))
           always (string=
                   expected
                    (gethash "content"
                             (gethash (getf spec :name)
                                      (gethash "states" result)))))))

  (let* ((node (obj "id" "checkpoint-node" "content" "exact"
                    "future_column" (obj "nested" t)))
         (edge (obj "id" 42 "from_id" "checkpoint-node"
                    "to_id" "other" "edge_type" "about")))
    (write-event-checkpoint
     "memory-nodes" 300
     (projection-rebuild-memory-checkpoint-content
      "memory-nodes" (list node)))
    (write-event-checkpoint
     "memory-edges" 300
     (projection-rebuild-memory-checkpoint-content
      "memory-edges" (list edge)))
    (let* ((baselines (projection-rebuild-load-memory-checkpoints))
           (result (projection-rebuild-fold-memory
                    nil :baselines baselines :expected-tail-event-id 300))
           (states (gethash "states" result)))
      (r0e-checkpoint-check "both verified relational checkpoints load"
                            (= 2 (hash-table-count baselines)))
      (r0e-checkpoint-check "relational checkpoint-only fold is complete"
                            (gethash "complete" result))
      (r0e-checkpoint-check
       "unknown nested row columns survive checkpoint verification"
       (gethash "nested"
                (gethash "future_column"
                         (gethash "checkpoint-node"
                                  (gethash "rows"
                                           (gethash "memory-nodes" states))))))))

  (let ((specs (projection-rebuild-postgres-table-specs)))
    (loop for spec in specs for index from 1
          for table = (getf spec :table)
          for row = (let ((value (obj "future_column" (obj "nested" table))))
                      (dolist (field (getf spec :keys) value)
                        (setf (gethash field value)
                              (if (string= field "version")
                                  index
                                  (format nil "~a-~a" table field)))))
          do (write-event-checkpoint
              table 500
              (projection-rebuild-table-checkpoint-content table (list row))))
    (let* ((baselines (projection-rebuild-load-table-checkpoints :specs specs))
           (result (projection-rebuild-fold-postgres-tables
                    nil :baselines baselines :specs specs
                    :expected-tail-event-id 500))
           (states (gethash "states" result)))
      (r0e-checkpoint-check "all twelve verified table checkpoints load"
                            (= 12 (hash-table-count baselines)))
      (r0e-checkpoint-check "twelve-table checkpoint-only fold is complete"
                            (gethash "complete" result))
      (r0e-checkpoint-check
       "generic table checkpoint retains unknown nested columns"
       (loop for spec in specs
             for state = (gethash (getf spec :table) states)
             for row = (loop for value being the hash-values of
                             (gethash "rows" state) return value)
             always (string= (getf spec :table)
                             (gethash "nested"
                                      (gethash "future_column" row)))))
      (let* ((ledger (merge-pathnames "postgres-row-events.jsonl" root))
             (*event-log-file* ledger)
             (*event-log-segmentation-enabled* nil)
             (*event-log-segmentation-ready-p* nil)
             (*event-next-id* 500)
             (*event-ring* nil)
             (table "memory_atom_rollouts")
             (row (obj "rollout_id" "memory_atom_rollouts-rollout_id"
                       "status" "actual-helper-envelope"
                       "future_column" (obj "nested" table))))
        (log-postgres-row-state table "upsert"
                                (obj "rollout_id" (gethash "rollout_id" row))
                                row)
        (let* ((events (replay-events))
               (folded (projection-rebuild-fold-postgres-tables
                        events :baselines baselines :specs specs
                        :expected-tail-event-id 501))
               (state (gethash table (gethash "states" folded)))
               (folded-row
                 (loop for value being the hash-values of
                       (gethash "rows" state)
                       when (string= "actual-helper-envelope"
                                     (gethash "status" value))
                         return value)))
          (r0e-checkpoint-check
           "real row helper JSONL replay folds through the generic contract"
           (and (= 1 (length events))
                (gethash "complete" folded)
                folded-row)))))))

(format t "~%checkpoint integration: ~a passed, ~a failed.~%"
        *r0e-checkpoint-pass* *r0e-checkpoint-fail*)
(when (plusp *r0e-checkpoint-fail*) (uiop:quit 1))
