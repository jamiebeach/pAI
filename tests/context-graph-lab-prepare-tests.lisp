;;;; harness: full-system
(in-package :agent)
(load (asdf:system-relative-pathname :pai "scripts/context-graph-episode-lab-core.lisp"))
(load (asdf:system-relative-pathname :pai "scripts/context-graph-lab-checkpoint.lisp"))
(load (asdf:system-relative-pathname :pai "scripts/context-graph-lab-prepare.lisp"))

(let* ((contract (obj "profile" "reviewed-inference-v9"
                      "generation" "identity-formation-owner-v9"
                      "protocol" "identity-formation-v14"
                      "ontology_revision" "personal-context-core-glm53-v1.3"
                      "agent_id" "fixture-agent" "persona_id" "fixture-persona"
                      "cutoff" 5 "recovery_position" 0))
       (input (obj "kind" "private-preparation-input" "contract" contract
                   "origin_digest" "fixture" "events" #()))
       (saved nil))
  (flet ((save (envelope selection status)
           (push (obj "envelope" envelope "contract" selection "status" status
                      "digest" (gethash "digest" envelope)) saved)))
    (assert (equal "complete" (gethash "status" (context-graph-lab-prepare input '(0) #'save))))
    (assert (= 1 (length saved)))
    (assert (equal "target" (gethash "status" (first saved))))
    (let ((resume (first saved)))
      (setf (gethash "events" input)
            (vector (obj "event" (obj "id" 1 "type" "fixture" "payload" (obj)))))
      (assert (equal "incomplete"
                     (gethash "status" (context-graph-lab-prepare input '(5) #'save
                                                                 :deadline-seconds 0 :resume resume))))
      (assert (equal "resume" (gethash "status" (first saved))))
      (assert (= 0 (gethash "cutoff" (gethash "contract" (first saved))))))))
(format t "LAB-PREPARE explicit cuts and clean-boundary timeout/resume passed~%")
(format t "PASS context-graph-lab-prepare-tests~%")
