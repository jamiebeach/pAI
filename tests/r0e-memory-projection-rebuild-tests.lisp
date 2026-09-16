(defpackage :agent (:use :cl))
(in-package :agent)

(ql:quickload '(:uiop :shasht :ironclad) :silent t)

(defvar *r0e2-pass* 0)
(defvar *r0e2-fail* 0)

(defun r0e2-check (name condition)
  (if condition
      (progn (incf *r0e2-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *r0e2-fail*) (format t "  FAIL ~a~%" name))))

(defun r0e2-obj (&rest pairs)
  (loop with result = (make-hash-table :test #'equal)
        for (key value) on pairs by #'cddr
        do (setf (gethash key result) value)
        finally (return result)))

(defun r0e2-event (id type operation mutation-kind row)
  (r0e2-obj "schema_version" 2 "id" id "type" type
             "payload" (r0e2-obj "operation" operation
                                  "mutation_kind" mutation-kind
                                  "row" row)))

(defun r0e2-node (id content &rest additions)
  (apply #'r0e2-obj
         "id" id "kind" "observation" "content" content
         "activation" 1.0d0 "quarantined" nil additions))

(defun r0e2-edge (id from to type &rest additions)
  (apply #'r0e2-obj
         "id" id "from_id" from "to_id" to "edge_type" type additions))

(load (test-source "projection-rebuild.lisp"))

(let* ((node-1 (r0e2-node "node-1" "before" "future_column" "retained"))
       (node-2 (r0e2-node "node-2" "stable"))
       (edge-1 (r0e2-edge 10 "node-1" "node-2" "about"
                          "created_at" "2026-08-09T00:00:00Z"))
       (baselines (make-hash-table :test #'equal)))
  (setf (gethash "memory-nodes" baselines)
        (make-projection-rebuild-row-baseline
         200 "memory-nodes" (list node-1 node-2))
        (gethash "memory-edges" baselines)
        (make-projection-rebuild-row-baseline
         200 "memory-edges" (list edge-1)))

  (let* ((node-1-after (r0e2-node "node-1" "after"
                                  "future_column" "still-retained"))
         (node-3 (r0e2-node "node-3" "new"))
         (edge-2 (r0e2-edge 11 "node-3" "node-2" "derived-from"
                            "created_at" "2026-08-09T00:01:00Z"))
         (events
           (list
            (r0e2-event 201 "memory-node-state" "update" "decay"
                        node-1-after)
            (r0e2-event 202 "memory-node-state" "upsert" "node-write" node-3)
            (r0e2-event 203 "memory-edge-state" "delete"
                        "lineage-replacement" edge-1)
            (r0e2-event 204 "memory-edge-state" "insert"
                        "admission-lineage" edge-2)
            (r0e2-obj "schema_version" 2 "id" 205 "type" "unrelated"
                      "payload" (r0e2-obj "value" t))))
         (result (projection-rebuild-fold-memory
                  events :baselines baselines :expected-tail-event-id 205))
         (states (gethash "states" result))
         (nodes (gethash "rows" (gethash "memory-nodes" states)))
         (edges (gethash "rows" (gethash "memory-edges" states))))
    (format t "~%== checkpoint plus relational tail ==~%")
    (r0e2-check "complete node/edge tail folds without gaps"
                (gethash "complete" result))
    (r0e2-check "node update and upsert produce exact final rows"
                (and (= 3 (hash-table-count nodes))
                     (string= "after" (gethash "content"
                                                (gethash "node-1" nodes)))
                     (string= "new" (gethash "content"
                                              (gethash "node-3" nodes)))))
    (r0e2-check "schema-unknown node columns are retained"
                (string= "still-retained"
                         (gethash "future_column" (gethash "node-1" nodes))))
    (r0e2-check "edge delete then insert yields exact final identity"
                (and (= 1 (hash-table-count edges))
                     (null (gethash 10 edges))
                     (string= "derived-from"
                              (gethash "edge_type" (gethash 11 edges)))))
    (r0e2-check "fold does not mutate checkpoint baseline rows"
                (= 1 (hash-table-count
                      (gethash "rows" (gethash "memory-edges" baselines))))))

  (format t "~%== named relational gaps ==~%")
  (let* ((empty (make-hash-table :test #'equal))
         (events
           (list
            (r0e2-event 1 "memory-node-state" "update" "decay"
                        (r0e2-node "missing" "post-update"))
            (r0e2-event 2 "memory-edge-state" "delete" "replacement"
                        (r0e2-edge 99 "missing" "other" "about"))))
         (result (projection-rebuild-fold-memory
                  events :baselines empty :expected-tail-event-id 2))
         (gaps (gethash "gaps" result)))
    (r0e2-check "node update without history is named"
                (find "update-without-prior-row" gaps
                      :key (lambda (gap) (gethash "reason" gap))
                      :test #'string=))
    (r0e2-check "edge delete without history is named"
                (find "delete-without-prior-row" gaps
                      :key (lambda (gap) (gethash "reason" gap))
                      :test #'string=))
    (r0e2-check "history gaps keep relational rebuild incomplete"
                (not (gethash "complete" result))))

  (let* ((wrong-delete (r0e2-edge 10 "node-1" "wrong" "about"
                                   "created_at" "2026-08-09T00:00:00Z"))
         (duplicate (r0e2-edge 10 "node-1" "node-2" "about"
                               "created_at" "2026-08-09T00:00:00Z"))
         (events
           (list
            (r0e2-event 201 "memory-edge-state" "insert" "duplicate" duplicate)
            (r0e2-event 202 "memory-edge-state" "delete" "replacement"
                        wrong-delete)))
         (result (projection-rebuild-fold-memory
                  events :baselines baselines :expected-tail-event-id 202))
         (gaps (gethash "gaps" result))
         (edges (gethash "rows"
                         (gethash "memory-edges" (gethash "states" result)))))
    (r0e2-check "duplicate edge insert is named"
                (find "duplicate-edge-insert" gaps
                      :key (lambda (gap) (gethash "reason" gap))
                      :test #'string=))
    (r0e2-check "pre-delete row mismatch is named"
                (find "delete-row-mismatch" gaps
                      :key (lambda (gap) (gethash "reason" gap))
                      :test #'string=))
    (r0e2-check "rejected edge events cannot alter trusted baseline state"
                (and (= 1 (hash-table-count edges)) (gethash 10 edges))))

  (let* ((edges-only (make-hash-table :test #'equal))
         (irrelevant (list (r0e2-obj "id" 201 "type" "unrelated"
                                     "payload" (r0e2-obj))))
         (edge-baseline (gethash "memory-edges" baselines)))
    (setf (gethash "memory-edges" edges-only) edge-baseline)
    (let ((result (projection-rebuild-fold-memory
                   irrelevant :baselines edges-only
                   :expected-tail-event-id 201)))
      (r0e2-check "truncated input names the missing node projection"
                  (find "memory-nodes" (gethash "gaps" result)
                        :key (lambda (gap) (gethash "projection" gap))
                        :test #'string=))))

  (let* ((malformed (r0e2-event 201 "memory-node-state" "update" "decay"
                                 (r0e2-node "node-1" "bad")))
         (payload (gethash "payload" malformed)))
    (remhash "mutation_kind" payload)
    (let ((result (projection-rebuild-fold-memory
                   (list malformed) :baselines baselines
                   :expected-tail-event-id 201)))
      (r0e2-check "malformed full-row event fails closed by projection"
                  (find "memory-nodes" (gethash "gaps" result)
                        :key (lambda (gap) (gethash "projection" gap))
                        :test #'string=))))

  (let* ((operation
           (r0e2-obj
            "schema_version" 2 "id" 201 "type" "memory-operation-state"
            "payload" (r0e2-obj "operation_kind" "supersession"
                                 "commands" (vector))))
         (result (projection-rebuild-fold-memory
                  (list operation) :baselines baselines
                  :expected-tail-event-id 201)))
    (r0e2-check "legacy memory fold refuses rather than skips atomic envelopes"
                (find "atomic-envelope-requires-event-first-projector"
                      (gethash "gaps" result)
                      :key (lambda (gap) (gethash "reason" gap))
                      :test #'string=))
    (r0e2-check "exact PostgreSQL tail also treats atomic envelopes as relevant"
                (%projection-rebuild-exact-tail-relevant-type-p
                 "memory-operation-state")))

  (let ((uneven (make-hash-table :test #'equal)))
    (setf (gethash "memory-nodes" uneven)
          (gethash "memory-nodes" baselines)
          (gethash "memory-edges" uneven)
          (make-projection-rebuild-row-baseline
           199 "memory-edges" (list edge-1)))
    (let ((result (projection-rebuild-fold-memory nil :baselines uneven
                                                  :expected-tail-event-id 200)))
      (r0e2-check "unequal relational checkpoint boundaries fail closed"
                  (find "checkpoint-boundary" (gethash "gaps" result)
                        :key (lambda (gap) (gethash "projection" gap))
                        :test #'string=)))))

(format t "~%R0e2 memory projection rebuild: ~a passed, ~a failed.~%"
        *r0e2-pass* *r0e2-fail*)
(when (plusp *r0e2-fail*) (uiop:quit 1))
