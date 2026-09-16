(in-package :agent)

(ql:quickload :shasht :silent t)
(load (test-source "candidate-representation.lisp"))

(defvar *candidate-test-pass* 0)
(defvar *candidate-test-fail* 0)

(defun candidate-test-check (name condition)
  (if condition
      (progn (incf *candidate-test-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *candidate-test-fail*) (format t "  FAIL ~a~%" name))))

(let* ((legacy (obj "id" "legacy-1" "source" "explore-development"
                    "content" "The user proposed a nightly routine."))
       (before-content (gethash "content" legacy)))
  (multiple-value-bind (normalized changed)
      (candidate-representation-normalize-record legacy)
    (candidate-test-check "legacy records backfill conservatively as internal stance"
                          (and changed (= 2 (gethash "schema_version" normalized))
                               (string= "internal-stance"
                                        (gethash "artifact_class" normalized))
                               (string= "explore-stance-v1"
                                        (gethash "generation_contract" normalized))
                               (null (gethash "composition_eligible" normalized))))
    (candidate-test-check "backfill preserves identifier and content exactly"
                          (and (string= "legacy-1" (gethash "id" normalized))
                               (string= before-content (gethash "content" normalized))))))

(let ((lint (candidate-send-readiness-lint
             "The user proposed a nightly wind-down routine.")))
  (candidate-test-check "third-person memory prose deterministically fails readiness"
                        (and (null (gethash "passed" lint))
                             (find "third-person-reference-to-operator"
                                   (coerce (gethash "violations" lint) 'list)
                                   :test #'string=))))
(let ((lint (candidate-send-readiness-lint
             "I wanted to tell you that the continuity design now has a concrete boundary.")))
  (candidate-test-check "addressed first-person draft can pass diagnostic linter"
                        (gethash "passed" lint)))

(let* ((record (obj "id" "stance-1" "source" "explore-development"
                    "source_id" "source-1" "topic" "continuity"
                    "artifact_class" "internal-stance"
                    "generation_contract" "explore-stance-v1"
                    "evidence_node_ids" (vector "evidence-1")
                    "content" "private content"))
       (envelope (candidate-reduced-e0-envelope record :contact-state "quiet")))
  (candidate-test-check "reduced E0 envelope is inspectable and non-authorizing"
                        (and (string= "stance-1" (gethash "candidate_id" envelope))
                             (null (gethash "authorizes_publication" envelope))
                             (null (gethash "authorizes_contact" envelope))
                             (null (gethash "content" envelope)))))

(let ((report (candidate-representation-report
               (list (obj "artifact_class" "internal-stance"
                          "content" "The user proposed a nightly routine.")
                     (obj "artifact_class" "rendered-draft"
                          "content" "I wanted to tell you about this concrete result.")))))
  (candidate-test-check "representation report is aggregate and content-free"
                        (and (= 2 (gethash "records" report))
                             (null (gethash "content" report)))))

(dolist (file '("conversational-initiative.lisp" "explore-novelty.lisp"
                "feedback-loop-containment.lisp"))
  (let ((source (uiop:read-file-string (namestring (test-source file)))))
    (candidate-test-check
     (format nil "~a explicitly emits internal stance contract" file)
     (and (search "\"internal-stance\"" source)
          (search "\"explore-stance-v1\"" source)))))

(let ((source (uiop:read-file-string (namestring (test-source "grounded-agency.lisp")))))
  (candidate-test-check "grounded publication explicitly emits rendered draft contract"
                        (and (search ":artifact-class \"rendered-draft\"" source)
                             (search "renderer_version" source))))

;; Load order is declared in pai.asd now, not in a container ENTRYPOINT.
;; The system is :serial, so component order is load order -- the same
;; property this check always tested, read from the artifact that now owns it.
(let* ((source (uiop:read-file-string
                (namestring (merge-pathnames "pai.asd" *pai-root*))))
       (representation (search "candidate-representation" source))
       (canary (and representation
                    (search "reciprocity-canary" source
                            :start2 representation))))
  (candidate-test-check "runtime load order places representation before canary"
                        (and representation canary (< representation canary))))

(format t "~&candidate representation tests: ~d passed, ~d failed.~%"
        *candidate-test-pass* *candidate-test-fail*)
(when (plusp *candidate-test-fail*) (sb-ext:exit :code 1))
