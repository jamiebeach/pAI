;;;; harness: bare
(require :asdf)
(unless (find-package :ql)
  (load (or (uiop:getenv "PAI_QUICKLISP_SETUP") "/opt/quicklisp/setup.lisp")))
(asdf:load-asd (pathname
 (or (uiop:getenv "PAI_CONTEXT_GRAPH_ASD")
     (merge-pathnames "../pai-context-graph.asd" *load-truename*))))
(asdf:load-system :pai-context-graph :force t)

(in-package :pai.context-graph)

(defvar *cg-test-pass* 0)
(defvar *cg-test-fail* 0)

(defun cg-check (name condition)
  (if condition
      (progn (incf *cg-test-pass*) (format t "PASS ~a~%" name))
      (progn (incf *cg-test-fail*) (format t "FAIL ~a~%" name))))

(defun cg-entity (ref type name &optional (action "NEW") existing)
  (%cg-object "local_ref" ref "type" type "name" name "aliases" #()
              "action" action "existing_id" (or existing :null)))

(defun cg-fact (subject predicate object text
                &optional supersedes (evidence-status "unreviewed")
                  (evidence-note "not evidence-reviewed"))
  (%cg-object "subject_ref" subject "predicate" predicate
              "object_ref" object "fact" text
              "supersedes_fact_id" (or supersedes :null)
              "evidence_status" evidence-status
              "evidence_note" evidence-note))

(defun cg-episode (id time text)
  (%cg-object "episode_id" id "occurred_at" time "learned_at" time
              "content" text))

(let* ((ontology
         (%cg-object
          "entity_types" #( "person" "accessibility_need" "artifact")
          "edge_types"
          (vector
           (%cg-object "name" "requires"
                       "subject_types" #( "person")
                       "object_types" #( "accessibility_need"))
           (%cg-object "name" "uses"
                       "subject_types" #( "artifact")
                       "object_types" #( "accessibility_need")))))
       (graph (make-context-graph ontology))
       (first
         (context-graph-apply-episode
          graph (cg-episode "episode:1" "2026-01-01T12:00:00Z" "A requirement.")
          (%cg-object
           "entities"
           (vector (cg-entity "p" "person" "Casey")
                   (cg-entity "n" "accessibility_need" "non-color-only encoding"))
           "facts"
           (vector (cg-fact "p" "requires" "n"
                            "Casey requires non-color-only visual encoding."
                            nil "direct" "The episode states the requirement.")))))
       (second
         (context-graph-apply-episode
          graph (cg-episode "episode:2" "2026-01-02T12:00:00Z" "An artifact.")
          (%cg-object
           "entities"
           (vector (cg-entity "a" "artifact" "quarterly presentation")
                   (cg-entity "n" "accessibility_need" "non-color-only encoding"))
           "facts"
           (vector (cg-fact "a" "uses" "n"
                            "The presentation uses non-color-only encoding."
                            nil "inference" "The use is a reasonable inference.")))))
       (reinforcement
         (context-graph-apply-episode
          graph (cg-episode "episode:3" "2026-01-03T12:00:00Z" "Repeated requirement.")
          (%cg-object
           "entities"
           (vector (cg-entity "p" "person" "Casey")
                   (cg-entity "n" "accessibility_need" "non-color-only encoding"))
           "facts"
           (vector (cg-fact "p" "requires" "n"
                            "Casey requires non-color-only visual encoding."
                            nil "inference" "A later episode implies the same requirement.")))))
       (duplicate
         (context-graph-apply-episode
          graph (cg-episode "episode:2" "2026-01-02T12:00:00Z" "An artifact.")
          (%cg-object "entities" #() "facts" #())))
       (result (context-graph-search graph "Casey presentation color"))
       (direct-result
         (context-graph-search graph "Casey presentation color"
                               :evidence-policy "direct-only")))
  (cg-check "two real-shaped episodes apply"
            (and (string= "applied" (gethash "status" first))
                 (string= "applied" (gethash "status" second))))
  (cg-check "later reinforcement applies"
            (string= "applied" (gethash "status" reinforcement)))
  (cg-check "canonical entity identity is reused across episodes"
            (= 3 (context-graph-entity-count graph)))
  (cg-check "typed facts persist independently"
            (= 2 (context-graph-fact-count graph)))
  (cg-check "episode application is idempotent"
            (string= "already-applied" (gethash "status" duplicate)))
  (cg-check "fact-first query returns compact relevant relationships"
            (and (= 2 (gethash "result_count" result))
                 (every
                  (lambda (row)
                    (and (not (nth-value 1 (gethash "content" row)))
                         (vectorp (gethash "source_episode_ids" row))))
                  (gethash "facts" result))))
  (cg-check "query can exclude reasonable inference explicitly"
            (and (= 1 (gethash "result_count" direct-result))
                 (string= "direct"
                          (gethash "evidence_status"
                                   (aref (gethash "facts" direct-result) 0)))
                 (= 2 (length (gethash "evidence_records"
                                       (aref (gethash "facts" direct-result) 0)))))))

(format t "~%Context graph core: ~d passed, ~d failed.~%"
        *cg-test-pass* *cg-test-fail*)
(when (plusp *cg-test-fail*) (uiop:quit 1))
