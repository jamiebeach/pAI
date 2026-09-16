;;;; harness: bare
;;;; Real lab subprocess, synthetic source and review bytes, no live runtime.
(require :asdf)
(unless (find-package :ql) (load (or (uiop:getenv "PAI_QUICKLISP_SETUP") "/opt/quicklisp/setup.lisp")))
(asdf:load-asd (merge-pathnames "../pai-context-graph.asd" *load-truename*))
(asdf:load-system :pai-context-graph)
(in-package :pai.context-graph)
(load (merge-pathnames "fixtures/context-graph-authority-fixtures.lisp" *load-truename*))
(defvar *authority-lab-checks* 0)
(load (merge-pathnames "fixtures/context-graph-authority-lab-driver.lisp" *load-truename*))
(defun al-check (name value)
  (unless value (error "FAIL ~a" name))
  (incf *authority-lab-checks*) (format t "PASS ~a~%" name))
(multiple-value-bind (context raw view) (at-fixture)
  (let* ((prepared (gethash "proposal" (gethash "value" (context-graph-prepare-authority context raw))))
         (review (at-raw-review context prepared)) (receipt (at-review-receipt context prepared review))
         (binding (%cg-object "opened_boundary_id" (gethash "opened_boundary_id" receipt)
                              "request_digest" (gethash "request_digest" receipt) "response_digest" (gethash "response_digest" receipt)))
         (bundle (%cg-object "schema_version" 2 "authority_operation" "formation-authority"
                             "authority_context" context "proposal" raw "raw_review" review "response_binding" binding "current_views" (vector view)))
         (wire (al-run bundle)))
    (al-check "production lab accepts reviewed conversational authority bytes"
              (equal "admitted" (gethash "formation_outcome" (gethash "batch" (gethash "value" wire)))))
    (al-check "lab batch exactly matches direct production Lisp"
              (%cg-authority-equal-p (%cg-decide-revision-batch context prepared receipt (vector view))
                                    (gethash "batch" (gethash "value" wire))))
    (setf (gethash "raw_review" bundle) :null (gethash "response_binding" bundle) :null)
    (let ((missing (al-run bundle)))
      (al-check "lab missing review is deferred, never accepted as an empty review"
                (equal "deferred" (gethash "formation_outcome" (gethash "batch" (gethash "value" missing))))))))
(multiple-value-bind (partition policy participants) (as-fixture)
  (setf (gethash "adjacency_complete" partition) :false)
  (let ((result (al-run (%cg-object "schema_version" 2 "authority_operation" "correction-scopes"
                                    "partition_view" partition "policy" policy "participants" participants))))
    (al-check "false completeness survives actual JSON wire boundary"
              (eq :false (gethash "complete" (aref (gethash "scopes" (gethash "value" result)) 0))))))
(format t "AUTHORITY-LAB ~d passed, 0 failed~%" *authority-lab-checks*)
