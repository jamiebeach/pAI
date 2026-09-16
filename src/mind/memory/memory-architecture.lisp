;;;; memory-architecture.lisp -- Workstream N0 additive memory foundation.
;;;;
;;;; Source-only in N0: this file deliberately performs no migration at load.
;;;; The first production consumer belongs to the combined N0/N1 slice.

(in-package :agent)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:ironclad :babel) :silent t)
  (unless (find-package :pai.mind.memory)
    (load (merge-pathnames "mind-memory-core.lisp"
                           (or *load-truename* *compile-file-truename*
                               *default-pathname-defaults*)))))

(export '(ensure-memory-architecture-schema
          memory-architecture-schema-report
          memory-architecture-characterization-report
          memory-architecture-validate-state
          memory-architecture-row-eligible-p))

(defparameter *memory-architecture-agent-id* (or (uiop:getenv "PAI_AGENT_ID") "default"))
(defparameter *memory-architecture-memory-forms*
  '("raw-evidence" "episodic" "semantic" "procedural" "reflection"
    "legacy-unclassified"))
(defparameter *memory-architecture-disclosure-classes*
  '("private" "personal-shareable" "public"))
(defparameter *memory-architecture-share-review-statuses*
  '("pending" "approved" "rejected"))

(defparameter *memory-architecture-columns*
  '("agent_id" "memory_form" "disclosure_class" "share_review_status"
    "share_review_event_id" "share_reviewed_at" "observed_at" "valid_from"
    "valid_to"))
(defparameter *memory-architecture-constraints*
  '("memory_nodes_agent_id_valid"
    "memory_nodes_memory_form_valid"
    "memory_nodes_disclosure_class_valid"
    "memory_nodes_share_review_status_valid"
    "memory_nodes_disclosure_approval_valid"
    "memory_nodes_valid_interval_valid"))
(defparameter *memory-architecture-indexes*
  '("memory_nodes_agent_disclosure_review_idx"
    "memory_nodes_agent_form_created_idx"
    "memory_nodes_agent_validity_idx"))

(defun %memory-architecture-string (value)
  (and value (string-downcase (string value))))

(defun %memory-architecture-nonempty-string-p (value)
  (and (stringp value)
       (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                   value)))))

(defun %memory-architecture-agent-id-p (value)
  (and (%memory-architecture-nonempty-string-p value)
       (<= (length value) 64)
       (let ((first (char value 0)))
         (or (and (char>= first #\a) (char<= first #\z))
             (digit-char-p first)))
       (every (lambda (character)
                (or (and (char>= character #\a) (char<= character #\z))
                    (digit-char-p character)
                    (member character '(#\. #\_ #\-) :test #'char=)))
              value)))

(defun memory-architecture-validate-state
    (&key (agent-id *memory-architecture-agent-id*)
          (memory-form "legacy-unclassified")
          (disclosure-class "private") share-review-status
          share-review-event-id share-reviewed-at valid-from valid-to)
  (pai.mind.memory:validate-state
   :agent-id agent-id :memory-form memory-form
   :disclosure-class disclosure-class
   :share-review-status share-review-status
   :share-review-event-id share-review-event-id
   :share-reviewed-at share-reviewed-at
   :valid-from valid-from :valid-to valid-to))

(defun memory-architecture-row-eligible-p
    (row &key (agent-id *memory-architecture-agent-id*) (audience :operator))
  (pai.mind.memory:row-eligible-p
   row :agent-id agent-id :audience audience))

(declaim (ftype function memory-architecture-schema-report))

(defun %memory-architecture-constraint-present-p (name)
  (not (null
        (pomo:query
         "SELECT 1 FROM pg_constraint WHERE conrelid='memory_nodes'::regclass AND conname=$1"
         name :single))))

(defun %memory-architecture-ensure-constraint (name definition)
  (unless (%memory-architecture-constraint-present-p name)
    ;; NAME and DEFINITION are closed constants owned by this module.
    (pomo:execute
     (format nil "ALTER TABLE memory_nodes ADD CONSTRAINT ~a ~a"
             name definition))))

(defun ensure-memory-architecture-schema ()
  "Explicit, idempotent N0 migration. This is never invoked at file load."
  (with-pg
    (pomo:with-transaction ()
      (dolist
          (ddl
            '("ALTER TABLE memory_nodes ADD COLUMN IF NOT EXISTS agent_id text NOT NULL DEFAULT 'default'"
              "ALTER TABLE memory_nodes ADD COLUMN IF NOT EXISTS memory_form text NOT NULL DEFAULT 'legacy-unclassified'"
              "ALTER TABLE memory_nodes ADD COLUMN IF NOT EXISTS disclosure_class text NOT NULL DEFAULT 'private'"
              "ALTER TABLE memory_nodes ADD COLUMN IF NOT EXISTS share_review_status text"
              "ALTER TABLE memory_nodes ADD COLUMN IF NOT EXISTS share_review_event_id text"
              "ALTER TABLE memory_nodes ADD COLUMN IF NOT EXISTS share_reviewed_at timestamptz"
              "ALTER TABLE memory_nodes ADD COLUMN IF NOT EXISTS observed_at timestamptz"
              "ALTER TABLE memory_nodes ADD COLUMN IF NOT EXISTS valid_from timestamptz"
              "ALTER TABLE memory_nodes ADD COLUMN IF NOT EXISTS valid_to timestamptz"))
        (pomo:execute ddl))
      (%memory-architecture-ensure-constraint
       "memory_nodes_agent_id_valid"
       "CHECK (agent_id ~ '^[a-z0-9][a-z0-9._-]{0,63}$')")
      (%memory-architecture-ensure-constraint
       "memory_nodes_memory_form_valid"
       "CHECK (memory_form IN ('raw-evidence','episodic','semantic','procedural','reflection','legacy-unclassified'))")
      (%memory-architecture-ensure-constraint
       "memory_nodes_disclosure_class_valid"
       "CHECK (disclosure_class IN ('private','personal-shareable','public'))")
      (%memory-architecture-ensure-constraint
       "memory_nodes_share_review_status_valid"
       "CHECK (share_review_status IS NULL OR share_review_status IN ('pending','approved','rejected'))")
      (%memory-architecture-ensure-constraint
       "memory_nodes_disclosure_approval_valid"
       "CHECK (disclosure_class='private' OR (share_review_status='approved' AND NULLIF(btrim(share_review_event_id),'') IS NOT NULL AND share_reviewed_at IS NOT NULL))")
      (%memory-architecture-ensure-constraint
       "memory_nodes_valid_interval_valid"
       "CHECK (valid_from IS NULL OR valid_to IS NULL OR valid_to > valid_from)")
      (pomo:execute
       "CREATE INDEX IF NOT EXISTS memory_nodes_agent_disclosure_review_idx ON memory_nodes (agent_id,disclosure_class,share_review_status)")
      (pomo:execute
       "CREATE INDEX IF NOT EXISTS memory_nodes_agent_form_created_idx ON memory_nodes (agent_id,memory_form,created_at DESC)")
      (pomo:execute
       "CREATE INDEX IF NOT EXISTS memory_nodes_agent_validity_idx ON memory_nodes (agent_id,valid_from,valid_to)")))
  (memory-architecture-schema-report))

(defun memory-architecture-schema-report ()
  "Return content-free N0 schema readiness and conservative-default counts."
  (with-pg
    (let* ((columns
             (pomo:query
              "SELECT column_name FROM information_schema.columns WHERE table_schema='public' AND table_name='memory_nodes' AND column_name IN ('agent_id','memory_form','disclosure_class','share_review_status','share_review_event_id','share_reviewed_at','observed_at','valid_from','valid_to') ORDER BY column_name"
              :column))
           (constraints
             (pomo:query
              "SELECT conname FROM pg_constraint WHERE conrelid='memory_nodes'::regclass AND conname IN ('memory_nodes_agent_id_valid','memory_nodes_memory_form_valid','memory_nodes_disclosure_class_valid','memory_nodes_share_review_status_valid','memory_nodes_disclosure_approval_valid','memory_nodes_valid_interval_valid') ORDER BY conname"
              :column))
           (indexes
             (pomo:query
              "SELECT indexname FROM pg_indexes WHERE schemaname='public' AND tablename='memory_nodes' AND indexname IN ('memory_nodes_agent_disclosure_review_idx','memory_nodes_agent_form_created_idx','memory_nodes_agent_validity_idx') ORDER BY indexname"
              :column))
           (ready (and (= (length columns)
                          (length *memory-architecture-columns*))
                       (= (length constraints)
                          (length *memory-architecture-constraints*))
                       (= (length indexes)
                          (length *memory-architecture-indexes*))))
           (defaults
             (if ready
                 (pomo:query
                  "SELECT count(*),count(*) FILTER (WHERE agent_id='default'),count(*) FILTER (WHERE memory_form='legacy-unclassified'),count(*) FILTER (WHERE disclosure_class='private'),count(*) FILTER (WHERE share_review_status IS NULL AND share_review_event_id IS NULL AND share_reviewed_at IS NULL),count(*) FILTER (WHERE observed_at IS NULL AND valid_from IS NULL AND valid_to IS NULL) FROM memory_nodes"
                  :row)
                 nil)))
      (obj "schema_version" 1
           "ready" (if ready t nil)
           "columns" (coerce columns 'vector)
           "constraints" (coerce constraints 'vector)
           "indexes" (coerce indexes 'vector)
           "row_count" (if defaults (first defaults) :null)
           "pai_default_count" (if defaults (second defaults) :null)
           "legacy_form_default_count" (if defaults (third defaults) :null)
           "private_default_count" (if defaults (fourth defaults) :null)
           "null_review_default_count" (if defaults (fifth defaults) :null)
           "null_temporal_default_count" (if defaults (sixth defaults) :null)))))

(defun memory-architecture-characterization-report ()
  "Content-free aggregate characterization. Never returns content or row IDs."
  (with-pg
    (let ((node-count (pomo:query "SELECT count(*) FROM memory_nodes" :single))
          (edge-count (pomo:query "SELECT count(*) FROM memory_edges" :single))
          (duplicates
            (pomo:query
             "SELECT count(*),coalesce(sum(n-1),0) FROM (SELECT count(*) n FROM memory_nodes GROUP BY content HAVING count(*)>1) duplicate_groups"
             :row))
          (confidence
            (pomo:query
             "SELECT count(*) FILTER (WHERE confidence IS NULL),count(*) FILTER (WHERE confidence IS NOT NULL),min(confidence),max(confidence) FROM memory_nodes"
             :row))
          (orphans
            (pomo:query
             "SELECT count(*) FILTER (WHERE from_node.id IS NULL),count(*) FILTER (WHERE to_node.id IS NULL) FROM memory_edges edge_row LEFT JOIN memory_nodes from_node ON from_node.id=edge_row.from_id LEFT JOIN memory_nodes to_node ON to_node.id=edge_row.to_id"
             :row)))
      (obj "schema_version" 1
           "content_free" t
           "node_count" node-count
           "edge_count" edge-count
           "exact_duplicate_group_count" (first duplicates)
           "exact_duplicate_excess_row_count" (second duplicates)
           "null_confidence_count" (first confidence)
           "non_null_confidence_count" (second confidence)
           "minimum_confidence" (or (third confidence) :null)
           "maximum_confidence" (or (fourth confidence) :null)
           "orphan_from_count" (first orphans)
           "orphan_to_count" (second orphans)))))
