(in-package :agent)

(ql:quickload '(:shasht :ironclad :bordeaux-threads) :silent t)

(defvar *memory-materializer-pass* 0)
(defvar *memory-materializer-fail* 0)

(defun memory-materializer-check (name condition)
  (if condition
      (progn (incf *memory-materializer-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *memory-materializer-fail*) (format t "  FAIL ~a~%" name))))

(load (test-source "storage-substrate.lisp"))
(load (test-source "memory-storage.lisp"))

(defparameter *materializer-vector-a* "000200003f80000000000000")
(defparameter *materializer-vector-b* "00020000000000003f800000")

(defun materializer-row-json (&key (activation 0.8d0) (access-count 2))
  (%storage-json
   (%memory-storage-object
    "id" "n1" "kind" "observation" "content" "fixture"
    "created_at" "2026-08-19T00:00:00Z"
    "last_accessed" "2026-08-19T00:00:00Z"
    "access_count" access-count "importance" 0.5d0 "valence" 0.0d0
    "arousal_at_encoding" 0.3d0 "activation" activation
    "source_event_id" :null "origin_class" "lived-user"
    "epistemic_status" "asserted" "producer" :null
    "model_purpose" :null "confidence" :null
    "grounding_status" "grounded" "root_observation_ids" (vector)
    "generation_id" :null "supersedes_node_id" :null
    "quarantined" nil "is_cold" nil
    "epistemic_metadata" (%memory-storage-object "turn_id" "t1"))))

(defun materializer-payload-row (payload)
  (and payload (shasht:read-json (gethash "scalar_json" payload))))

(format t "~%== deterministic cognitive memory mutation materializers ==~%")

(memory-materializer-check
 "unknown cognitive mutation kind fails closed"
 (and (fboundp 'make-memory-node-upsert-payload)
      (handler-case
          (progn
            (make-memory-node-upsert-payload
             (materializer-row-json) *materializer-vector-a*
             *materializer-vector-b* "unknown")
            nil)
        (memory-storage-error () t))))

(memory-materializer-check
 "node admission preserves the complete row and both exact vectors"
 (and (fboundp 'make-memory-node-upsert-payload)
      (let ((payload
              (make-memory-node-upsert-payload
               (materializer-row-json) *materializer-vector-a*
               *materializer-vector-b* "admission")))
        (and (string= "upsert" (gethash "operation" payload))
             (string= *materializer-vector-a*
                      (gethash "embedding_binary_hex" payload))
             (string= *materializer-vector-b*
                      (gethash "retrieval_embedding_binary_hex" payload))))))

(memory-materializer-check
 "quarantine is a full replacement that preserves unrelated metadata"
 (and (fboundp 'memory-materialize-node-quarantine)
      (let* ((payload
               (memory-materialize-node-quarantine
                (materializer-row-json) *materializer-vector-a*
                *materializer-vector-b* "reason" "actor"))
             (row (materializer-payload-row payload))
             (metadata (gethash "epistemic_metadata" row)))
        (and (eq t (gethash "quarantined" row))
             (string= "t1" (gethash "turn_id" metadata))
             (string= "reason" (gethash "quarantine_reason" metadata))
             (string= "actor" (gethash "quarantined_by" metadata))))))

(memory-materializer-check
 "supersession replaces the target node with explicit lineage metadata"
 (and (fboundp 'memory-materialize-node-supersession)
      (let* ((payload
               (memory-materialize-node-supersession
                (materializer-row-json) *materializer-vector-a*
                *materializer-vector-b* "old" "reason" "actor"))
             (row (materializer-payload-row payload))
             (metadata (gethash "epistemic_metadata" row)))
        (and (string= "old" (gethash "supersedes_node_id" row))
             (string= "reason" (gethash "supersession_reason" metadata))
             (string= "actor" (gethash "superseded_by_actor" metadata))))))

(memory-materializer-check
 "legacy recall rehearsal increments access and caps activation at one"
 (and (fboundp 'memory-materialize-node-rehearsal)
      (let* ((payload
               (memory-materialize-node-rehearsal
                (materializer-row-json :activation 0.9d0)
                *materializer-vector-a* *materializer-vector-b*
                :legacy-recall "2026-08-19T01:00:00Z"))
             (row (materializer-payload-row payload)))
        (and (= 3 (gethash "access_count" row))
             (= 1.0d0 (gethash "activation" row))
             (string= "2026-08-19T01:00:00Z"
                      (gethash "last_accessed" row))))))

(memory-materializer-check
 "user-visible rehearsal preserves activation already above its cap"
 (and (fboundp 'memory-materialize-node-rehearsal)
      (let* ((payload
               (memory-materialize-node-rehearsal
                (materializer-row-json :activation 0.9d0)
                *materializer-vector-a* *materializer-vector-b*
                :user-visible "2026-08-19T01:00:00Z"))
             (row (materializer-payload-row payload)))
        (< (abs (- 0.9d0 (gethash "activation" row))) 1.0d-6))))

(memory-materializer-check
 "decay replacement changes only activation and cold state"
 (and (fboundp 'memory-materialize-node-decay)
      (let* ((payload
               (memory-materialize-node-decay
                (materializer-row-json) *materializer-vector-a*
                *materializer-vector-b* 0.04d0 t))
             (row (materializer-payload-row payload)))
        (and (< (abs (- 0.04d0 (gethash "activation" row))) 1.0d-6)
             (eq t (gethash "is_cold" row))
             (= 2 (gethash "access_count" row))))))

(memory-materializer-check
 "retrieval backfill replaces only the retrieval vector"
 (and (fboundp 'memory-materialize-node-retrieval-backfill)
      (let ((payload
              (memory-materialize-node-retrieval-backfill
               (materializer-row-json) *materializer-vector-a*
               *materializer-vector-b*)))
        (and (string= *materializer-vector-a*
                      (gethash "embedding_binary_hex" payload))
             (string= *materializer-vector-b*
                      (gethash "retrieval_embedding_binary_hex" payload))))))

(memory-materializer-check
 "edge materialization is closed to incumbent insert/delete families"
 (and (fboundp 'make-memory-edge-state-payload)
      (let ((row "{\"id\":1,\"from_id\":\"n1\",\"to_id\":\"n2\",\"edge_type\":\"derived-from\"}"))
        (and (string= "insert"
                      (gethash "operation"
                               (make-memory-edge-state-payload
                                row "insert" "admission-lineage")))
             (string= "delete"
                      (gethash "operation"
                               (make-memory-edge-state-payload
                                row "delete" "admission-lineage-replacement")))
             (handler-case
                 (progn
                   (make-memory-edge-state-payload row "delete" "direct-edge")
                   nil)
               (memory-storage-error () t))))))

(memory-materializer-check
 "tick commit edge families are present in the closed materializer vocabulary"
 (and (fboundp 'make-memory-edge-state-payload)
      (let ((row "{\"id\":1,\"from_id\":\"n1\",\"to_id\":\"n2\",\"edge_type\":\"derived-from\"}"))
        (every
         (lambda (kind)
           (string= "insert"
                    (gethash "operation"
                             (make-memory-edge-state-payload
                              row "insert" kind))))
         '("tick-commit-lineage" "tick-commit-supersession"
           "tick-commit-edge")))))

(memory-materializer-check
 "new node rows reproduce PostgreSQL scalar defaults with one explicit clock"
 (and (fboundp 'make-memory-node-scalar-row)
      (let ((row
              (shasht:read-json
               (make-memory-node-scalar-row
                :id "new" :content "content"
                :timestamp "2026-08-20T00:00:00Z"))))
        (and (= 23 (hash-table-count row))
             (string= "observation" (gethash "kind" row))
             (string= (gethash "created_at" row)
                      (gethash "last_accessed" row))
             (zerop (gethash "access_count" row))
             (= 1.0 (gethash "activation" row))
             (null (gethash "is_cold" row))))))

(memory-materializer-check
 "new node rows preserve nullable and epistemic inputs explicitly"
 (and (fboundp 'make-memory-node-scalar-row)
      (let* ((json
               (make-memory-node-scalar-row
                :id "new" :content nil :timestamp "2026-08-20T00:00:00Z"
                :origin-class "lived-user" :epistemic-status "asserted"
                :grounding-status "grounded" :confidence 0.75d0))
             (row (shasht:read-json json)))
        (and (search "\"content\":null" json)
             (%memory-materializer-json-null-p (gethash "content" row))
             (string= "lived-user" (gethash "origin_class" row))
             (string= "asserted" (gethash "epistemic_status" row))
             (string= "grounded" (gethash "grounding_status" row))
             (< (abs (- 0.75d0 (gethash "confidence" row))) 1.0d-6)))))

(memory-materializer-check
 "upsert merge preserves incumbent lifecycle fields while replacing content"
 (and (fboundp 'memory-merge-node-upsert-rows)
      (let* ((existing (shasht:read-json (materializer-row-json)))
             (proposed
               (shasht:read-json
                (make-memory-node-scalar-row
                 :id "n1" :kind "thought" :content "replacement"
                 :timestamp "2026-08-20T02:00:00Z" :importance 0.7d0)))
             (merged
               (shasht:read-json
                (memory-merge-node-upsert-rows existing proposed))))
        (and (string= "replacement" (gethash "content" merged))
             (string= "thought" (gethash "kind" merged))
             (string= "2026-08-19T00:00:00Z"
                      (gethash "created_at" merged))
             (= 2 (gethash "access_count" merged))
             (< (abs (- 0.8d0 (gethash "activation" merged))) 1.0d-6)))))

(memory-materializer-check
 "upsert sentinel values retain established epistemic provenance"
 (and (fboundp 'memory-merge-node-upsert-rows)
      (let* ((existing (shasht:read-json (materializer-row-json)))
             (proposed
               (shasht:read-json
                (make-memory-node-scalar-row
                 :id "n1" :content "replacement"
                 :timestamp "2026-08-20T02:00:00Z")))
             (merged
               (shasht:read-json
                (memory-merge-node-upsert-rows existing proposed))))
        (and (string= "lived-user" (gethash "origin_class" merged))
             (string= "asserted" (gethash "epistemic_status" merged))
             (string= "grounded" (gethash "grounding_status" merged))
             (string= "t1"
                      (gethash "turn_id"
                               (gethash "epistemic_metadata" merged)))))))

(memory-materializer-check
 "upsert quarantine is monotonic and nonempty metadata replaces atomically"
 (and (fboundp 'memory-merge-node-upsert-rows)
      (let* ((existing (shasht:read-json (materializer-row-json)))
             (proposed
               (shasht:read-json
                (make-memory-node-scalar-row
                 :id "n1" :content "replacement"
                 :timestamp "2026-08-20T02:00:00Z" :quarantined t
                 :epistemic-metadata
                 (%memory-storage-object "source" "replacement"))))
             (merged
               (shasht:read-json
                (memory-merge-node-upsert-rows existing proposed))))
        (and (eq t (gethash "quarantined" merged))
             (string= "replacement"
                      (gethash "source"
                               (gethash "epistemic_metadata" merged)))
             (null (gethash "turn_id"
                            (gethash "epistemic_metadata" merged)))))))

(memory-materializer-check
 "malformed merge rows fail through the typed storage boundary"
 (and (fboundp 'memory-merge-node-upsert-rows)
      (let ((existing (shasht:read-json (materializer-row-json)))
            (proposed (shasht:read-json (materializer-row-json))))
        (setf (gethash "origin_class" proposed) :not-a-string)
        (handler-case
            (progn (memory-merge-node-upsert-rows existing proposed) nil)
          (memory-storage-error () t)))))

(format t "~%~d passed, ~d failed~%"
        *memory-materializer-pass* *memory-materializer-fail*)
(when (plusp *memory-materializer-fail*)
  (error "Memory mutation materializer tests failed"))
