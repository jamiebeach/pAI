;;;; harness: bare
(require :asdf)
(unless (find-package :ql) (load (or (uiop:getenv "PAI_QUICKLISP_SETUP") "/opt/quicklisp/setup.lisp")))
(asdf:load-asd (merge-pathnames "../pai-context-graph.asd" *load-truename*))
(asdf:load-system :pai-context-graph)
(in-package :pai.context-graph)
(load (merge-pathnames "fixtures/context-graph-authority-fixtures.lisp" *load-truename*))
(defvar *authority-identity-checks* 0)
(defun ai-check (name value)
  (unless value (error "FAIL ~a" name))
  (incf *authority-identity-checks*) (format t "PASS ~a~%" name))
(let* ((claim (%cg-object "predicate" "owns" "fact" "The operator owns a cat."
                          "grounding" (%cg-object "schema_version" 2 "scope" "assertion" "polarity" "positive"
                                                   "attributed_to_ref" "runtime:operator"
                                                   "evidence" (vector (%cg-object "source_id" "source:one" "quote" "a cat" "start_char" 0 "end_char" 5)))))
       (refs (%cg-object "subject_entity_id" "entity:operator" "object_entity_id" "entity:cat"
                         "attributed_entity_id" "entity:operator" "source_basis" "original"))
       (before (%cg-authority-canonical-json (vector claim refs)))
       (legacy-identity
         (vector "entity:operator" "owns" "entity:cat"
                 "the operator owns a cat." "assertion" "positive"
                 "entity:operator" "original"))
       (key (gethash "identity_sha256" (gethash "value" (context-graph-claim-identity claim refs)))))
  (ai-check "v1 claim identity retains its original durable digest domain"
            (equal key (%cg-authority-digest "claim-identity" legacy-identity)))
  (dolist (spec '(("claim" "fact" "The operator owns two cats.")
                  ("grounding" "scope" "reported-speech") ("grounding" "polarity" "negative")
                  ("grounding" "polarity" "unknown")
                  ("refs" "source_basis" "derived") ("refs" "attributed_entity_id" "entity:third-party")
                  ("refs" "object_entity_id" "entity:other-cat")))
    (let* ((changed (%cg-detach claim)) (resolved (%cg-detach refs))
           (target (cond ((equal "claim" (first spec)) changed) ((equal "grounding" (first spec)) (gethash "grounding" changed)) (t resolved))))
      (setf (gethash (second spec) target) (third spec))
      (ai-check "different claim authority cannot merge through triple equality"
                (not (equal key (gethash "identity_sha256" (gethash "value" (context-graph-claim-identity changed resolved))))))))
  (ai-check "identity construction is read-only" (equal before (%cg-authority-canonical-json (vector claim refs))))
  (setf (gethash "fact" claim) "  THE OPERATOR OWNS A CAT.  ")
  (ai-check "only statement presentation canonicalized" (equal key (gethash "identity_sha256" (gethash "value" (context-graph-claim-identity claim refs)))))
  (setf (gethash "evidence" (gethash "grounding" claim)) #())
  (ai-check "new factual identity rejects empty evidence"
            (handler-case (progn (context-graph-claim-identity claim refs) nil) (context-graph-authority-input-error () t))))
(let* ((claim (%cg-object "predicate" "owns" "fact" "The operator owns a cat."
                          "grounding" (%cg-object "schema_version" 2 "scope" "assertion" "polarity" "positive"
                                                   "attributed_to_ref" "runtime:operator"
                                                   "evidence" (vector (%cg-object "source_id" "source:one" "quote" "a cat" "start_char" 0 "end_char" 5)))
                          "temporal" (%cg-object "schema_version" 1 "character" "persistent"
                                                 "occurred_at" :null "valid_from" :null "valid_until" :null)))
       (original (%cg-object "subject_entity_id" "entity:operator" "object_entity_id" "entity:cat"
                             "attributed_entity_id" "entity:operator" "source_basis" "original"))
       (derived (%cg-detach original)))
  (setf (gethash "source_basis" derived) "derived")
  (let ((*cg-claim-identity-protocol* "claim-identity-v2"))
    (ai-check "v2 typed identity permits evidence promotion"
              (equal (gethash "identity_sha256" (gethash "value" (context-graph-claim-identity claim original)))
                     (gethash "identity_sha256" (gethash "value" (context-graph-claim-identity claim derived)))))
    (let ((restated (%cg-detach claim)))
      (setf (gethash "fact" restated) "A cat belongs to the operator.")
      (ai-check "v2 typed identity ignores wording-only restatement"
                (equal (gethash "identity_sha256" (gethash "value" (context-graph-claim-identity claim original)))
                       (gethash "identity_sha256" (gethash "value" (context-graph-claim-identity restated original)))))
      (setf (gethash "predicate" claim) "related_to"
            (gethash "predicate" restated) "related_to")
      (ai-check "v2 generic relation retains statement meaning"
                (not (equal (gethash "identity_sha256" (gethash "value" (context-graph-claim-identity claim original)))
                            (gethash "identity_sha256" (gethash "value" (context-graph-claim-identity restated original)))))))))
(let* ((claim (%cg-object "predicate" "parent_of" "fact" "Parent is parent of child."
                          "grounding" (%cg-object "schema_version" 2 "scope" "assertion" "polarity" "positive"
                                                   "attributed_to_ref" "runtime:operator"
                                                   "evidence" (vector (%cg-object "source_id" "source:one" "quote" "my child" "start_char" 0 "end_char" 8)))
                          "temporal" (%cg-object "schema_version" 1 "character" "ongoing-state"
                                                 "occurred_at" :null "valid_from" :null "valid_until" :null)))
       (restated (%cg-detach claim))
       (refs (%cg-object "subject_entity_id" "entity:parent" "object_entity_id" "entity:child"
                         "attributed_entity_id" "entity:parent" "source_basis" "original")))
  (setf (gethash "character" (gethash "temporal" restated)) "standing-disposition")
  (let ((*cg-claim-identity-protocol* "claim-identity-v3"))
    (ai-check "v3 coalesces unbounded enduring family temporal wording"
              (equal (gethash "identity_sha256" (gethash "value" (context-graph-claim-identity claim refs)))
                     (gethash "identity_sha256" (gethash "value" (context-graph-claim-identity restated refs)))))
    (setf (gethash "valid_from" (gethash "temporal" restated)) "2026-01-01")
    (ai-check "v3 preserves bounded family temporal identity"
              (not (equal (gethash "identity_sha256" (gethash "value" (context-graph-claim-identity claim refs)))
                          (gethash "identity_sha256" (gethash "value" (context-graph-claim-identity restated refs))))))
    (setf (gethash "valid_from" (gethash "temporal" restated)) :null
          (gethash "predicate" claim) "has_age"
          (gethash "predicate" restated) "has_age")
    (ai-check "v3 preserves non-family temporal character"
              (not (equal (gethash "identity_sha256" (gethash "value" (context-graph-claim-identity claim refs)))
                          (gethash "identity_sha256" (gethash "value" (context-graph-claim-identity restated refs))))))))
(multiple-value-bind (partition policy participants) (as-fixture 1)
  (declare (ignore policy participants))
  (let* ((entity (gethash "entity:000" (gethash "entities" partition)))
         (versions (make-hash-table :test #'equal)) (current (make-hash-table :test #'equal))
         (view (%cg-object "agent_id" "agent:one" "persona_id" "persona:one" "entity_versions" versions "current_entity_versions" current)))
    (setf (gethash "entity:000" versions) entity (gethash "entity:000" current) "entity:000")
    (let* ((before (%cg-authority-canonical-json view)) (result (context-graph-current-entity-view "entity:000" view)))
      (ai-check "current lookup uses enduring identity index" (equal "entity:000" (gethash "node_id" (gethash "value" result))))
      (setf (gethash "label" (gethash "value" result)) "Changed")
      (ai-check "current lookup is read-only and detached" (equal before (%cg-authority-canonical-json view))))
    (setf (gethash "entity:000" current) "missing-version")
    (ai-check "missing current version is corruption, never repaired during read"
              (handler-case (progn (context-graph-current-entity-view "entity:000" view) nil) (context-graph-authority-input-error () t)))))
(format t "AUTHORITY-IDENTITY ~d passed, 0 failed~%" *authority-identity-checks*)
