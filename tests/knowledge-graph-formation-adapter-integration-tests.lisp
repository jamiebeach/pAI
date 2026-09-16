;;;; knowledge-graph-formation-adapter-integration-tests.lisp -- disposable KG2 composition.
;;;; harness: full-system

(in-package :agent)

(defvar *kgfai-pass* 0)
(defvar *kgfai-fail* 0)

(defun kgfai-check (name condition)
  (if condition
      (progn (incf *kgfai-pass*) (format t "PASS ~a~%" name))
      (progn (incf *kgfai-fail*) (format t "FAIL ~a~%" name))))

(defun kgfai-delete-db (path)
  (dolist (candidate
           (list path
                 (pathname (concatenate 'string (namestring path) "-wal"))
                 (pathname (concatenate 'string (namestring path) "-shm"))))
    (when (probe-file candidate) (delete-file candidate))))

(defun kgfai-events (backend)
  (let ((events nil))
    (storage-map-events
     backend (lambda (event position)
               (declare (ignore position)) (push event events))
     :agent-id "kgfai-agent")
    (nreverse events)))

(defun kgfai-episode-payload ()
  (obj "schema_version" 1 "episode_id" "episode:kgfai:1"
       "persona_id" "kgfai-persona" "first_event_id" 1 "last_event_id" 2
       "first_timestamp" 4000000001 "last_timestamp" 4000000002
       "source_event_ids" #(1 2)
       "synopsis" "The operator requires accessible visual artifacts."
       "subjects" #( "artifact accessibility") "entities" #( "operator")
       "retrieval_cues" #( "accessible visuals")
       "broader_categories" #( "preferences") "unresolved_threads" #()
       "protocol_revision" "recursive-conversation-episode-v1"
       "projection_revision" "conversation-episodes-v2"
       "sealed_at" 4000000010))

(defun kgfai-proposal ()
  (obj "schema_version" 3
       "ontology_revision" *knowledge-graph-ontology-revision*
       "entities"
       (vector
        (obj "local_ref" "operator" "kind" "person" "label" "Operator"
             "aliases" #() "classifications" #() "identity_action" "NEW"
             "existing_node_id" :null "evidence_status" "direct"
             "evidence_note" "The episode names the operator.")
        (obj "local_ref" "requirement" "kind" "concept"
             "label" "Accessible visual artifacts" "aliases" #()
             "classifications" #("accessibility")
             "identity_action" "NEW" "existing_node_id" :null
             "evidence_status" "direct"
             "evidence_note" "The requirement is stated explicitly."))
       "relationships"
       (vector (obj "subject_ref" "operator" "predicate" "related_to"
                    "object_ref" "requirement"
                    "relationship_action" "ASSERT"
                    "fact" "The operator requires accessible visual artifacts."
                    "grounding"
                    (obj "schema_version" 1 "scope" "assertion"
                         "polarity" "positive" "attributed_to_ref" :null
                         "evidence"
                         (vector
                          (obj "source_id" "event:1"
                               "quote" "I require accessible visual artifacts.")))
                    "temporal"
                    (obj "schema_version" 1 "character" "standing-disposition"
                         "occurred_at" :null "valid_from" :null
                         "valid_until" :null)
                    "evidence_status" "direct"
                    "evidence_note" "The episode directly relates them."))))

(format t "~%== KG2 disposable concrete adapter ==~%")

(let ((event-db #p"/agent/state/kgfai-events.sqlite3")
      (derived-db #p"/agent/state/kgfai-derived.sqlite3")
      (events nil) (derived nil)
      (thread-function (symbol-function '%recursive-thread-events))
      (append-function (symbol-function '%conversation-append-readable))
      (provider-function (symbol-function '%recursive-kg-formation-provider)))
  (kgfai-delete-db event-db)
  (kgfai-delete-db derived-db)
  (unwind-protect
      (progn
        (setf events (make-sqlite-storage event-db)
              derived (make-sqlite-derived-storage derived-db))
        (storage-append-event
         events "user-message"
         (obj "text" "I require accessible visual artifacts.")
         :agent-id "kgfai-agent")
        (storage-append-event
         events "agent-message" (obj "text" "Understood.")
         :agent-id "kgfai-agent")
        (storage-append-event
         events "conversation-episode-sealed" (kgfai-episode-payload)
         :agent-id "kgfai-agent")
        (setf (symbol-function '%recursive-thread-events)
              (lambda () (kgfai-events events))
              (symbol-function '%conversation-append-readable)
              (lambda (type payload &key caused-by)
                (values nil
                        (storage-append-event
                         events type payload :agent-id "kgfai-agent"
                         :caused-by caused-by)))
              (symbol-function '%recursive-kg-formation-provider)
              (lambda (opened opened-id)
                (declare (ignore opened opened-id)) (kgfai-proposal)))
        (let ((first
                (conscious-recursive-knowledge-graph-formation-step
                 events derived "kgfai-agent" "kgfai-persona")))
          (kgfai-check "one concrete quantum opens seals and synchronizes"
                       (string= "sealed" (gethash "status" first ""))))
        (let ((types (mapcar (lambda (event) (gethash "type" event ""))
                             (kgfai-events events))))
          (kgfai-check "authority contains one open and one terminal seal"
                       (and (= 1 (count "knowledge-graph-formation-opened"
                                          types :test #'string=))
                            (= 1 (count "knowledge-graph-formation-sealed"
                                          types :test #'string=)))))
        (let* ((storage-id
                 (gethash "storage_id"
                          (storage-authority-boundary
                           events :agent-id "kgfai-agent")))
               (graph
                 (knowledge-graph-formation-restore
                  derived "kgfai-agent" "kgfai-persona"
                  :event-storage-id storage-id)))
          (kgfai-check "post-seal generation is restart-readable"
                       (and (= 2 (length (gethash "nodes" graph)))
                            (= 1 (length (gethash "edges" graph))))))
        (let ((again
                (conscious-recursive-knowledge-graph-formation-step
                 events derived "kgfai-agent" "kgfai-persona")))
          (kgfai-check "covered episode is idempotently provider-silent"
                       (string= "idle" (gethash "status" again "")))))
    (setf (symbol-function '%recursive-thread-events) thread-function
          (symbol-function '%conversation-append-readable) append-function
          (symbol-function '%recursive-kg-formation-provider) provider-function)
    (when events (ignore-errors (storage-close events)))
    (when derived (ignore-errors (storage-close derived)))
    (kgfai-delete-db event-db)
    (kgfai-delete-db derived-db)))

(format t "~%KG2 concrete adapter: ~d passed, ~d failed.~%"
        *kgfai-pass* *kgfai-fail*)
(when (plusp *kgfai-fail*) (uiop:quit 1))
