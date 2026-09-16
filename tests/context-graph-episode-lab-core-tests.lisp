;;;; context-graph-episode-lab-core-tests.lisp -- versioned replay selection.
;;;; harness: full-system

(in-package :agent)

(load (asdf:system-relative-pathname
       :pai "scripts/context-graph-episode-lab-core.lisp"))

(defun cgel-test-open (id generation revision)
  (obj "id" id "type" "context-graph-identity-opened"
       "payload"
       (obj "generation" generation
            "record_json"
            (shasht:write-json
             (obj "ontology_revision" revision) nil))))

(let* ((v8 (cgel-test-open 10 "identity-formation-owner-v8"
                           *knowledge-graph-ontology-revision*))
       (v9 (cgel-test-open 20 "identity-formation-owner-v9"
                           *knowledge-graph-family-ontology-revision*))
       (events (list v8 v9))
       (before (%cgel-generation-contract events 19))
       (after (%cgel-generation-contract events 20))
       (ontology (%cgel-ontology (second after)))
       (graph (pai.context-graph:context-graph-runtime-create
               ontology (second after) "agent" "persona"))
       (owner (%cgel-create-owner
               (first after)
               (pai.context-graph::context-graph-runtime-graph graph)
               "agent" "persona" (second after))))
  (assert (equal before
                 (list "identity-formation-owner-v8"
                       *knowledge-graph-ontology-revision*)))
  (assert (equal after
                 (list "identity-formation-owner-v9"
                       *knowledge-graph-family-ontology-revision*)))
  (assert (find "attribute_value" (gethash "entity_types" ontology)
                :test #'equal))
  (assert (find "parent_of" (gethash "edge_types" ontology)
                :test #'equal :key (lambda (row) (gethash "name" row))))
  (assert (equal "identity-formation-owner-v9"
                 (pai.context-graph::cgi-owner-protocol owner))))

(format t "EPISODE-LAB versioned V8/V9 cut selection and V1.3 replay construction passed~%")
(format t "PASS context-graph-episode-lab-core-tests~%")
