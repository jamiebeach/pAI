;;;; knowledge-graph-ontology.lisp -- fixed KG2 upper ontology v1.2.
;;;;
;;;; This is pure source policy distilled from the lab-qualified ontology.  It
;;;; performs no IO and is checked at both provider admission and replay.

(in-package :agent)

(export '(knowledge-graph-ontology-kind-p
          knowledge-graph-ontology-predicate-p
          knowledge-graph-ontology-signature-valid-p
          knowledge-graph-ontology-provider-descriptor
          *knowledge-graph-family-ontology-revision*))

(defparameter *knowledge-graph-ontology-revision*
  "personal-context-core-glm53-v1.2")

(defparameter *knowledge-graph-family-ontology-revision*
  "personal-context-core-glm53-v1.3"
  "Versioned extension for recurring kinship and literal attribute values.")

(defparameter *knowledge-graph-ontology-kinds*
  '("person" "agent" "organization" "organism" "object" "place"
    "artifact" "system" "project" "concept" "condition" "event"
    "phenomenon" "gap" "other_thing"))

(defparameter *knowledge-graph-family-ontology-kinds*
  '("attribute_value"))

(defparameter *knowledge-graph-ontology-signatures*
  '(("part_of" ("artifact" "system" "project" "event" "organism" "object" "place")
               ("artifact" "system" "project" "organism" "object" "place"))
    ("has_part" ("artifact" "system" "project" "organism" "object" "place")
                ("artifact" "system" "project" "event" "organism" "object" "place"))
    ("works_at" ("person") ("organization"))
    ("formerly_at" ("person") ("organization"))
    ("operates" ("person" "agent") ("agent" "system"))
    ("owns" ("person" "agent" "organization") ("artifact" "organism" "system" "object"))
    ("authored" ("person" "agent" "organization") ("artifact" "system"))
    ("leads" ("person" "agent") ("project"))
    ("works_on" ("person" "agent") ("project" "artifact" "system"))
    ("addresses" ("artifact" "concept" "system" "project") ("gap" "concept"))
    ("affects" ("condition" "concept" "phenomenon")
               ("person" "system" "condition" "concept" "organism" "object" "place"))
    ("has_condition" ("person" "organism") ("condition"))
    ("uses" ("person" "agent" "system") ("artifact" "system" "object"))
    ("considered_using" ("person" "agent") ("artifact" "system" "object"))
    ("implements" ("artifact" "system") ("concept" "artifact"))
    ("occurs_after" ("event") ("event"))
    ("related_to" ("person" "agent" "organization" "organism" "object" "place"
                    "artifact" "system" "project" "concept" "condition" "event"
                    "phenomenon" "gap" "other_thing")
                  ("person" "agent" "organization" "organism" "object" "place"
                    "artifact" "system" "project" "concept" "condition" "event"
                    "phenomenon" "gap" "other_thing"))
    ("proposed" ("person" "agent") ("concept" "artifact" "project" "system"))
    ("recommends" ("person" "agent") ("artifact" "concept" "system"))
    ("warned_about" ("person" "agent") ("concept"))
    ("keeps_secret_from" ("person" "agent") ("person" "agent"))
    ("runs_on" ("agent" "system" "artifact") ("system" "artifact"))
    ("managed_with" ("condition") ("artifact" "system" "concept" "object"))
    ("has_gap" ("person" "agent" "system" "project") ("gap"))
    ("plans_migration_of" ("person" "agent" "organization") ("agent" "system"))
    ("migrates_to" ("agent" "system") ("system"))
    ("classified_as" ("person" "agent" "organization" "organism" "object" "place"
                       "artifact" "system" "project" "condition" "event" "phenomenon"
                       "gap" "other_thing") ("concept"))
    ("located_at" ("person" "agent" "organization" "organism" "object" "place"
                   "artifact" "system" "project" "condition" "event" "phenomenon"
                   "other_thing") ("place"))))

(defparameter *knowledge-graph-family-ontology-signatures*
  '(("parent_of" ("person") ("person"))
    ("daughter_of" ("person") ("person"))
    ("son_of" ("person") ("person"))
    ("spouse_of" ("person") ("person"))
    ("companion_of" ("person" "organism") ("person" "organism"))
    ("has_age" ("person" "organism") ("attribute_value"))
    ("has_gender" ("person" "organism") ("attribute_value"))))

(defun %knowledge-graph-ontology-extension-p (revision)
  (cond ((equal revision *knowledge-graph-ontology-revision*) nil)
        ((equal revision *knowledge-graph-family-ontology-revision*) t)
        (t (error "Unknown knowledge graph ontology revision"))))

(defun %knowledge-graph-ontology-kinds-for (revision)
  (if (%knowledge-graph-ontology-extension-p revision)
      (append *knowledge-graph-ontology-kinds*
              *knowledge-graph-family-ontology-kinds*)
      *knowledge-graph-ontology-kinds*))

(defun %knowledge-graph-ontology-signatures-for (revision)
  (if (%knowledge-graph-ontology-extension-p revision)
      (append *knowledge-graph-ontology-signatures*
              *knowledge-graph-family-ontology-signatures*)
      *knowledge-graph-ontology-signatures*))

(defun knowledge-graph-ontology-kind-p
    (kind &optional (revision *knowledge-graph-ontology-revision*))
  (and (stringp kind)
       (member kind (%knowledge-graph-ontology-kinds-for revision)
               :test #'string=)))

(defun knowledge-graph-ontology-predicate-p
    (predicate &optional (revision *knowledge-graph-ontology-revision*))
  (and (stringp predicate)
       (assoc predicate (%knowledge-graph-ontology-signatures-for revision)
              :test #'string=)))

(defun knowledge-graph-ontology-signature-valid-p
    (predicate subject-kind object-kind
     &optional (revision *knowledge-graph-ontology-revision*))
  (let ((signature (and (stringp predicate)
                        (assoc predicate
                               (%knowledge-graph-ontology-signatures-for revision)
                               :test #'string=))))
    (and signature
         (member subject-kind (second signature) :test #'string=)
         (member object-kind (third signature) :test #'string=))))

(defun knowledge-graph-ontology-provider-descriptor
    (&optional (revision *knowledge-graph-ontology-revision*))
  "Return a bounded provider-facing descriptor of the enforced ontology."
  (let ((kinds (%knowledge-graph-ontology-kinds-for revision))
        (signatures (%knowledge-graph-ontology-signatures-for revision)))
    (obj "revision" revision
         "entity_types" (coerce kinds 'vector)
         "predicate_signatures"
         (coerce
          (mapcar (lambda (row)
                    (obj "predicate" (first row)
                         "subject_types" (coerce (second row) 'vector)
                         "object_types" (coerce (third row) 'vector)))
                  signatures)
          'vector))))
