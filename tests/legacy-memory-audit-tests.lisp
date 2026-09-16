(in-package :agent)

(ql:quickload :postmodern :silent t)

(defvar *legacy-audit-test-pass* 0)
(defvar *legacy-audit-test-fail* 0)
(defvar *legacy-audit-test-quarantines* nil)
(defvar *legacy-audit-test-events* nil)

(defun legacy-audit-test-check (name condition)
  (if condition
      (progn (incf *legacy-audit-test-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *legacy-audit-test-fail*) (format t "  FAIL ~a~%" name))))

(setf (fdefinition 'log-event)
      (lambda (type payload &key caused-by)
        (declare (ignore caused-by)) (push (list type payload) *legacy-audit-test-events*)))
(load (test-source "legacy-memory-audit.lisp"))

(setf *legacy-audit-row-source-fn*
      (lambda ()
        (list
         (obj "id" "legacy-a" "kind" "thought" "origin_class" "legacy-unclassified"
              "epistemic_status" "legacy-unclassified" "grounding_status" "unclassified"
              "root_observation_ids" (vector) "producer" "old-tick"
              "access_count" 30 "activation" 0.91d0 "quarantined" nil
              "epistemic_metadata" (obj))
         (obj "id" "legacy-b" "kind" "reflection" "origin_class" "synthetic"
              "epistemic_status" "hypothesis" "grounding_status" "unclassified"
              "root_observation_ids" (vector) "producer" "broken-handler"
              "access_count" 1 "activation" 0.2d0 "quarantined" nil
              "epistemic_metadata" (obj "recursive_synthetic_lineage" t))
         (obj "id" "grounded-c" "kind" "observation" "origin_class" "lived-user"
              "epistemic_status" "user-report" "grounding_status" "grounded"
              "root_observation_ids" (vector "grounded-c") "producer" "turn"
              "access_count" 100 "activation" 0.99d0 "quarantined" nil
              "epistemic_metadata" (obj)))))
(setf *legacy-audit-similarity-source-fn*
      (lambda () (list (list "legacy-a" "legacy-b" 0.94d0))))
(setf *legacy-audit-quarantine-fn*
      (lambda (id reason actor) (push (list id reason actor) *legacy-audit-test-quarantines*) t))

(format t "~%== structural dry-run ==~%")
(let ((report (legacy-memory-audit-dry-run :failed-node-ids '("legacy-b"))))
  (legacy-audit-test-check "dry-run identifies structural candidates only"
                           (and (= 3 (gethash "scanned" report))
                                (= 2 (gethash "candidate_count" report))
                                (= 250 (gethash "similarity_scope_limit" report))))
  (legacy-audit-test-check "dry-run never mutates or deletes"
                           (and (null *legacy-audit-test-quarantines*)
                                (zerop (gethash "deletes" report))
                                (zerop (gethash "text_or_edge_changes" report))))
  (let ((a (find "legacy-a" (gethash "candidates" report)
                 :key (lambda (item) (gethash "id" item)) :test #'string=))
        (b (find "legacy-b" (gethash "candidates" report)
                 :key (lambda (item) (gethash "id" item)) :test #'string=)))
    (legacy-audit-test-check "fixed-query and similarity reasons are explicit"
                             (and (find "repeated-fixed-query-activation" (gethash "reasons" a) :test #'string=)
                                  (find "high-similarity-cluster" (gethash "reasons" a) :test #'string=)))
    (legacy-audit-test-check "recursive lineage and failed-window reasons are explicit"
                             (and (find "recursive-synthetic-lineage" (gethash "reasons" b) :test #'string=)
                                  (find "failed-handler-window" (gethash "reasons" b) :test #'string=))))
  (let ((failed nil))
    (handler-case (legacy-memory-audit-apply report "wrong-confirmation")
      (error () (setf failed t)))
    (legacy-audit-test-check "application requires exact out-of-band confirmation" failed))
  (let ((result (legacy-memory-audit-apply report (gethash "manifest_id" report)
                                           :operator "fixture")))
    (legacy-audit-test-check "confirmed apply quarantines without deletion"
                             (and (= 2 (length *legacy-audit-test-quarantines*))
                                  (= 2 (gethash "quarantined" result))
                                  (zerop (gethash "deleted" result))))
    (legacy-audit-test-check "confirmed batch is audited"
                             (find "memory-quarantine-changed" *legacy-audit-test-events*
                                   :key #'first :test #'string=))))

(format t "~%~a passed, ~a failed~%" *legacy-audit-test-pass* *legacy-audit-test-fail*)
(when (plusp *legacy-audit-test-fail*) (sb-ext:exit :code 1))
