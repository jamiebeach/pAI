;;;; harness: bare
(require :asdf)
(unless (find-package :ql)
  (load (or (uiop:getenv "PAI_QUICKLISP_SETUP") "/opt/quicklisp/setup.lisp")))
(asdf:load-asd (merge-pathnames "../pai-context-graph.asd" *load-truename*))
(asdf:load-system :pai-context-graph)
(in-package :pai.context-graph)
(defvar *authority-participant-checks* 0)
(defun ap-check (name value)
  (unless value (error "FAIL ~a" name))
  (incf *authority-participant-checks*) (format t "PASS ~a~%" name))
(defun ap-participant (role kind label)
  (%cg-object "role" role "speaker_id" (format nil "principal:~a" role)
              "principal_id" (format nil "principal:~a" role)
              "identity_binding_id" (format nil "binding:~a" role)
              "local_ref" (format nil "runtime:~a" role)
              "entity_id" (format nil "identity:~a" role)
              "kind" kind "label" label "aliases" #()))
(defun ap-context ()
  (let ((text "I know someone with the same name."))
    (%cg-object "participants" (vector (ap-participant "operator" "person" "Same name")
                                         (ap-participant "active-persona" "agent" "Assistant"))
                "source_packet"
                (%cg-object "schema_version" 2 "sources"
                  (vector (%cg-object "source_id" "source:one" "speaker_id" "principal:operator"
                                      "kind" "original-utterance" "timestamp" 100
                                      "text" text "text_sha256" (%cg-sha256 text)
                                      "identity" (%cg-object "principal_id" "principal:operator"
                                                              "binding_id" "binding:operator"
                                                              "conversation_id" "conversation:one" "role" "operator")
                                      "resource_ref" (%cg-object "store" "event" "resource_id" "event:one"
                                                                  "version_id" "version:one" "component" "content")))))))
(defun ap-proposal ()
  (%cg-object "schema_version" 4 "ontology_revision" "fixture-v1" "entity_revisions" #()
              "entities" (vector (%cg-object "local_ref" "other" "kind" "person" "label" "Same name"
                                              "aliases" #() "classifications" #("operator" "colleague")
                                              "identity_action" "NEW" "existing_node_id" :null
                                              "evidence_status" "unreviewed" "evidence_note" "pending"))
              "relationships" (vector (%cg-object "subject_ref" "runtime:operator" "predicate" "knows"
                                                    "object_ref" "other" "grounding"
                                                    (%cg-object "attributed_to_ref" :null)))))
(let* ((context (ap-context)) (proposal (ap-proposal))
       (before (%cg-authority-canonical-json (vector proposal context)))
       (result (context-graph-normalize-participants proposal context))
       (value (gethash "value" result)) (normalized (gethash "proposal" value))
       (bindings (gethash "bindings" value)))
  (ap-check "reserved identities are injected without proposed roles" (= 3 (length (gethash "entities" normalized))))
  (ap-check "same-name third party is preserved" (equal "other" (gethash "local_ref" (aref (gethash "entities" normalized) 0))))
  (ap-check "malicious role metadata stripped" (equalp #("colleague") (gethash "classifications" (aref (gethash "entities" normalized) 0))))
  (ap-check "ignored role yields bounded diagnostic" (equal "ROLE_CLAIM_IGNORED" (gethash "code" (aref (gethash "diagnostics" result) 0))))
  (ap-check "runtime operator identity does not come from ordinary entity"
            (equal "identity:operator" (gethash "entity_id" (aref bindings 0))))
  (ap-check "persona exists without utterances" (zerop (length (gethash "source_ids" (aref bindings 1)))))
  (ap-check "null semantic attribution not inferred from authorship"
            (eq :null (gethash "attributed_to_ref" (gethash "grounding" (aref (gethash "relationships" normalized) 0)))))
  (ap-check "input is immutable" (equal before (%cg-authority-canonical-json (vector proposal context))))
  (setf (gethash "entity_id" (aref bindings 0)) "changed")
  (ap-check "output descriptors detached" (equal before (%cg-authority-canonical-json (vector proposal context)))))
(dolist (mode '("reserved-ref" "reserved-link"))
  (let* ((proposal (ap-proposal)) (entity (aref (gethash "entities" proposal) 0)))
    (if (equal mode "reserved-ref") (setf (gethash "local_ref" entity) "runtime:operator")
        (setf (gethash "identity_action" entity) "LINK_EXISTING" (gethash "existing_node_id" entity) "identity:operator"))
    (ap-check mode (equal "rejected" (gethash "status" (context-graph-normalize-participants proposal (ap-context)))))))
(let* ((context (ap-context)) (source (aref (gethash "sources" (gethash "source_packet" context)) 0)))
  (setf (gethash "binding_id" (gethash "identity" source)) "binding:foreign")
  (ap-check "foreign principal binding fails before normalization"
            (handler-case (progn (context-graph-normalize-participants (ap-proposal) context) nil)
              (context-graph-authority-input-error () t))))
(let ((context (ap-context)) (proposal (ap-proposal)))
  (setf (gethash "entities" proposal) #() (gethash "relationships" proposal) #())
  (ap-check "empty extraction still has two registry participants"
            (= 2 (length (gethash "bindings" (gethash "value" (context-graph-normalize-participants proposal context)))))))
(format t "AUTHORITY-PARTICIPANTS ~d passed, 0 failed~%" *authority-participant-checks*)
