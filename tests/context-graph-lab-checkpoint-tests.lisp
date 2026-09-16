;;;; harness: full-system
(in-package :pai.context-graph)
(load (asdf:system-relative-pathname :pai "scripts/context-graph-lab-checkpoint.lisp"))

(let* ((graph (%make-context-graph))
       (owner (%cgi-owner-create graph "fixture-agent" "fixture-persona"))
       (runtime (%make-cg-runtime :graph graph))
       (phase (%cg-object "response" (vector :null :false :true nil t 1/3 0.75d0)))
       (key (list 7 "facts"))
       (root (vector owner runtime)))
  (setf (gethash key (cgi-owner-phases owner)) phase
        (gethash 7 (cgi-owner-opens owner)) phase
        (gethash "fixture" (context-graph-entities graph)) phase)
  (let* ((encoded (context-graph-lab-encode root))
         (copy (context-graph-lab-decode (context-graph-runtime-read-json (%cgl-json encoded))))
         (new-owner (aref copy 0)) (new-runtime (aref copy 1))
         (new-phase (gethash (list 7 "facts") (cgi-owner-phases new-owner))))
    (assert (equal (%cgl-json encoded)
                   (%cgl-json (%cg-object "nodes" (gethash "nodes" encoded)
                                          "root" (gethash "root" encoded)
                                          "codec" (gethash "codec" encoded)))))
    (assert (equal (%cgl-json encoded) (%cgl-json (context-graph-lab-encode copy))))
    (assert (eq (cgi-owner-graph new-owner) (context-graph-runtime-graph new-runtime)))
    (assert (eq new-phase (gethash 7 (cgi-owner-opens new-owner))))
    (assert (eq 'eql (hash-table-test (cgi-owner-opens new-owner))))
    (assert (eq 'equal (hash-table-test (cgi-owner-phases new-owner))))
    (assert (equalp (gethash "response" phase) (gethash "response" new-phase)))
    (setf (aref (gethash "response" new-phase) 0) "changed")
    (assert (eq :null (aref (gethash "response" phase) 0)))
    (assert (not (eq graph (cgi-owner-graph new-owner))))))

(let ((bad (%cg-object "codec" "unknown")))
  (assert (handler-case (progn (context-graph-lab-decode bad) nil) (error () t))))
(assert (handler-case (progn (context-graph-lab-encode #'identity) nil) (error () t)))
(let* ((revision "personal-context-core-glm53-v1.3")
       (graph (%make-context-graph))
       (runtime (%make-cg-runtime :graph graph :agent-id "fixture-agent"
                                  :persona-id "fixture-persona" :revision revision))
       (owner (%cgi-owner-create graph "fixture-agent" "fixture-persona"
                                 "identity-formation-owner-v9"))
       (contract (%cg-object "profile" "reviewed-inference-v9"
                             "generation" "identity-formation-owner-v9"
                             "protocol" "identity-formation-v14"
                             "ontology_revision" revision
                             "agent_id" "fixture-agent" "persona_id" "fixture-persona"
                             "cutoff" 10 "recovery_position" 0
                             "compatibility" "fixture-v1" "origin_digest" "fixture-origin")))
  (setf (cgi-owner-revision owner) revision)
  (let* ((sealed (context-graph-lab-checkpoint-seal runtime owner (make-hash-table :test #'eql) contract))
         (digest (gethash "digest" sealed)))
    (multiple-value-bind (restored restored-owner index)
        (context-graph-lab-checkpoint-open sealed digest contract)
      (assert (eq (context-graph-runtime-graph restored) (cgi-owner-graph restored-owner)))
      (assert (eq 'eql (hash-table-test index))))
    (assert (handler-case (progn (context-graph-lab-checkpoint-open sealed "bad" contract) nil)
              (error () t)))
    (setf (gethash "cutoff" contract) 11)
    (assert (handler-case (progn (context-graph-lab-checkpoint-open sealed digest contract) nil)
              (error () t)))))
(format t "LAB-CHECKPOINT typed roundtrip, shared references, isolation and refusals passed~%")
(format t "PASS context-graph-lab-checkpoint-tests~%")
