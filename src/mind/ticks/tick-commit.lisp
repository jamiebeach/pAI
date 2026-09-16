;;;; tick-commit.lisp -- validated, atomic autonomous commit sets.
;;;;
;;;; Cognitive handlers propose data. This module validates the complete set
;;;; before any durable mutation, admits all memory nodes and edges in one
;;;; PostgreSQL transaction, and applies dependent state only after that commit.
;;;; In :SHADOW-ONLY mode the same validation runs but nothing is written.

(in-package :agent)

(export '(tick-commit-validate tick-commit-apply tick-commit-report
          make-tick-proposal tick-terminal-call))

(defparameter *tick-commit-types*
  '("idle-drift" "light-consolidate" "full-reflection" "anticipate"
    "ruminate" "curiosity" "explore" "episode-replay" "maintenance"))
(defparameter *tick-commit-record-types*
  '("supported-inference" "hypothesis" "prediction"))
(defparameter *tick-commit-direct-origins* '("external-signal" "tool-result"))
(defparameter *tick-commit-synthetic-hour-cap* 6)
(defvar *tick-commit-lock* (bt:make-lock "validated-tick-commit"))
(defvar *tick-commit-stats* (make-hash-table :test #'equal))
(defvar *tick-commit-stats-lock* (bt:make-lock "tick-commit-stats"))
(defvar *tick-commit-transaction-fn* nil
  "Test adapter: (proposal source-event-id) -> admitted ids.")
(defvar *tick-commit-dependent-fn* nil
  "Test adapter called only after every required memory write commits.")
(defvar *tick-commit-recent-count-fn* nil
  "Test adapter returning accepted synthetic records since the cap boundary.")
(defvar *tick-commit-recent-records-fn* nil
  "Test adapter: (topic tick-type) -> recent typed records.")
(defvar *tick-commit-similarity-fn* nil
  "Test adapter: (left right) -> cosine similarity.")
(defvar *tick-terminal-event-fn* nil
  "Test adapter: (type payload caused-by) -> event id.")
(defvar *tick-terminal-start-event-id* nil)

(define-condition tick-commit-validation-error (error)
  ((reasons :initarg :reasons :reader tick-commit-validation-reasons))
  (:report (lambda (condition stream)
             (format stream "Tick commit rejected: ~{~a~^; ~}"
                     (tick-commit-validation-reasons condition)))))

(defun %tick-commit-list (value)
  (cond ((null value) nil) ((listp value) value)
        ((vectorp value) (coerce value 'list)) (t (list value))))

(defun %tick-commit-stat (key)
  (bt:with-lock-held (*tick-commit-stats-lock*)
    (incf (gethash key *tick-commit-stats* 0))))

(defun tick-commit-report ()
  (bt:with-lock-held (*tick-commit-stats-lock*)
    (let ((counts (obj)))
      (maphash (lambda (key value) (setf (gethash key counts) value))
               *tick-commit-stats*)
      (obj "schema_version" 1
           "synthetic_hour_cap" *tick-commit-synthetic-hour-cap*
           "counts" counts))))

(defun make-tick-proposal (generation-id tick-type &key (status "proposed")
                                      memory-specs edge-specs continuity-facts
                                      initiative-specs modulator-deltas reason)
  (obj "generation_id" generation-id "tick_type" tick-type "status" status
       "reason" (or reason :null)
       "memory_specs" (coerce (%tick-commit-list memory-specs) 'vector)
       "edge_specs" (coerce (%tick-commit-list edge-specs) 'vector)
       "continuity_facts" (coerce (%tick-commit-list continuity-facts) 'vector)
       "initiative_specs" (coerce (%tick-commit-list initiative-specs) 'vector)
       "modulator_deltas" (or modulator-deltas (obj))))

(defun %tick-commit-memory-reasons (spec tick-type)
  (let* ((reasons nil)
         (content (and (hash-table-p spec) (gethash "content" spec)))
         (record-type (and (hash-table-p spec) (gethash "record_type" spec)))
         (origin (or (and (hash-table-p spec) (gethash "origin_class" spec))
                     "synthetic"))
         (evidence (%tick-commit-list
                    (and (hash-table-p spec) (gethash "evidence_node_ids" spec))))
         (role (and (hash-table-p spec) (gethash "role" spec))))
    (unless (hash-table-p spec) (push "memory spec is not an object" reasons))
    (unless (and (stringp content) (plusp (length content)))
      (push "memory content is empty" reasons))
    (if (member origin *tick-commit-direct-origins* :test #'string=)
        (progn
          (unless (string= (or record-type "") "direct-event")
            (push "direct memory requires direct-event record_type" reasons))
          (unless (gethash "source_event_id" spec)
            (push "direct memory requires source_event_id" reasons)))
        (progn
          (unless (string= origin "synthetic")
            (push "memory origin_class is invalid" reasons))
          (unless (member record-type *tick-commit-record-types* :test #'string=)
            (push "memory record_type is invalid" reasons))
          (unless (and evidence (every #'stringp evidence))
            (push "memory evidence_node_ids are required" reasons))))
    (when (and (fboundp '%cognitive-identity-confusion-p)
               (funcall '%cognitive-identity-confusion-p content))
      (push "memory content has identity confusion" reasons))
    (when (and (string= origin "synthetic") (string= tick-type "idle-drift")
               (not (string= (or record-type "") "hypothesis")))
      (push "idle-drift may only propose a hypothesis" reasons))
    (when (and (string= origin "synthetic")
               (member tick-type '("light-consolidate" "full-reflection")
                       :test #'string=)
               (< (length evidence) 2))
      (push "consolidation requires at least two evidence nodes" reasons))
    (when (and (string= origin "synthetic") (string= tick-type "anticipate")
               (or (not (stringp (gethash "target" spec)))
                   (not (stringp (gethash "time_horizon" spec)))))
      (push "anticipation requires target and time_horizon" reasons))
    (when (and (string= origin "synthetic") (string= tick-type "ruminate")
               (let ((kinds (%tick-commit-list (gethash "evidence_kinds" spec))))
                 (or (null kinds) (every (lambda (kind) (string= kind "rumination")) kinds))))
      (push "rumination cannot cite only prior ruminations" reasons))
    (when (and (string= origin "synthetic") (string= tick-type "episode-replay")
               (not (member "episode" (%tick-commit-list
                                        (gethash "evidence_kinds" spec))
                            :test #'string=)))
      (push "episode replay requires cited episode evidence" reasons))
    (when (and role
               (not (member role '("search-question" "external-result" "synthesis")
                            :test #'string=)))
      (push "memory role is invalid" reasons))
    (when (and (gethash "supersedes_node_id" spec)
               (not (stringp (gethash "supersedes_node_id" spec))))
      (push "supersedes_node_id must be a string" reasons))
    (nreverse reasons)))

(defun tick-commit-validate (proposal)
  "Return a list of complete-set rejection reasons. Pure and fail closed."
  (let ((reasons nil))
    (if (not (hash-table-p proposal))
        (list "proposal is not an object")
        (let* ((generation (gethash "generation_id" proposal))
               (tick-type (gethash "tick_type" proposal))
               (status (gethash "status" proposal))
               (memories (%tick-commit-list (gethash "memory_specs" proposal)))
               (edges (%tick-commit-list (gethash "edge_specs" proposal)))
               (facts (%tick-commit-list (gethash "continuity_facts" proposal)))
               (initiatives (%tick-commit-list (gethash "initiative_specs" proposal)))
               (deltas (gethash "modulator_deltas" proposal)))
          (unless (and (stringp generation) (plusp (length generation)))
            (push "generation_id is required" reasons))
          (unless (member tick-type *tick-commit-types* :test #'string=)
            (push "tick_type is invalid" reasons))
          (unless (member status '("proposed" "skipped" "rejected") :test #'string=)
            (push "status is invalid" reasons))
          (unless (vectorp (gethash "memory_specs" proposal))
            (push "memory_specs must be a vector" reasons))
          (unless (vectorp (gethash "edge_specs" proposal))
            (push "edge_specs must be a vector" reasons))
          (unless (vectorp (gethash "continuity_facts" proposal))
            (push "continuity_facts must be a vector" reasons))
          (unless (vectorp (gethash "initiative_specs" proposal))
            (push "initiative_specs must be a vector" reasons))
          (unless (hash-table-p deltas) (push "modulator_deltas must be an object" reasons))
          (when (and (string= (or status "") "proposed")
                     (not (string= (or tick-type "") "maintenance"))
                     (null memories))
            (push "generating proposal requires memory_specs" reasons))
          (when (and (string= (or tick-type "") "maintenance") memories)
            (push "maintenance cannot propose memory writes" reasons))
          (dolist (spec memories)
            (setf reasons (nconc reasons (%tick-commit-memory-reasons spec tick-type))))
          (dolist (edge edges)
            (unless (and (hash-table-p edge)
                         (stringp (gethash "from_id" edge))
                         (stringp (gethash "to_id" edge))
                         (stringp (gethash "edge_type" edge)))
              (push "edge spec is malformed" reasons)))
          (unless (every #'hash-table-p facts)
            (push "continuity facts must be factual objects" reasons))
          (unless (every #'hash-table-p initiatives)
            (push "initiative specs must be objects" reasons))
          (when (string= (or tick-type "") "curiosity")
            (let ((roles (mapcar (lambda (spec) (gethash "role" spec)) memories)))
              (dolist (required '("search-question" "external-result" "synthesis"))
                (unless (member required roles :test #'string=)
                  (push (format nil "curiosity requires ~a record" required) reasons)))))
          (remove-duplicates (nreverse reasons) :test #'string=)))))

(defun %tick-commit-recent-synthetic-count ()
  (with-pg
    (pomo:query
     "SELECT count(*) FROM memory_nodes WHERE origin_class='synthetic' AND created_at > greatest(now() - interval '1 hour', coalesce((SELECT max(created_at) FROM memory_nodes WHERE origin_class IN ('lived-user','lived-agent-action','tool-result','external-signal')), '-infinity'::timestamptz))"
     :single)))

(defun %tick-commit-similarity (left right)
  (if *tick-commit-similarity-fn*
      (funcall *tick-commit-similarity-fn* left right)
      (cosine-similarity (embed-text left) (embed-text right))))

(defun %tick-commit-novelty-reasons (proposal)
  (let ((reasons nil)
        (specs (%tick-commit-list (gethash "memory_specs" proposal)))
        (tick-type (gethash "tick_type" proposal)))
    (loop for tail on specs
          for left = (first tail)
          do (dolist (right (rest tail))
               (when (and (equal (gethash "topic" left) (gethash "topic" right))
                          (>= (%tick-commit-similarity (gethash "content" left)
                                                     (gethash "content" right))
                              0.90d0))
                 (push "same-topic proposal duplicate" reasons))))
    (dolist (spec specs)
      (let* ((topic (gethash "topic" spec))
             (recent (cond (*tick-commit-recent-records-fn*
                            (funcall *tick-commit-recent-records-fn* topic tick-type))
                           ((and topic (fboundp 'memory-search))
                            (memory-search topic :k 10 :mode :cognitive-evidence
                                           :kinds '("thought" "reflection"
                                                    "prediction" "worldview")))
                           (t nil))))
        (when (some (lambda (record)
                      (let ((prior (and (hash-table-p record)
                                        (gethash "content" record))))
                        (and (stringp prior)
                             (>= (%tick-commit-similarity
                                  (gethash "content" spec) prior) 0.90d0))))
                    recent)
          (push "recent same-topic duplicate" reasons))))
    (remove-duplicates reasons :test #'string=)))

(defun %tick-commit-prepare-memory (spec proposal source-event-id)
  (let* ((content (gethash "content" spec))
         (uncertainty (or (gethash "uncertainty" spec) 0.5d0))
         (id (or (gethash "id" spec)
                 (format nil "node-~a-~4,'0x" (get-universal-time) (random 65536))))
         (importance (or (gethash "importance" spec) (%score-importance content))))
    (list :id id :kind (or (gethash "kind" spec) "thought")
          :content content :embedding (embed-text content)
          :retrieval-embedding (embed-retrieval-document content)
          :importance importance
          :valence (or (gethash "valence" spec) 0.0d0)
          :arousal (or (gethash "arousal" spec) 0.3d0)
          :source-event-id (or (gethash "source_event_id" spec) source-event-id)
          :origin-class (or (gethash "origin_class" spec) "synthetic")
          :epistemic-status (gethash "record_type" spec)
          :producer "tick-commit-v1" :model-purpose (gethash "tick_type" proposal)
          :confidence (- 1.0d0 uncertainty)
          :grounding-status (or (gethash "grounding_status" spec) "grounded")
          :generation-id (gethash "generation_id" proposal)
          :supersedes-node-id (gethash "supersedes_node_id" spec)
          :lineage-parent-ids (%tick-commit-list (gethash "evidence_node_ids" spec))
          :novelty-passed t :self-process-event-id source-event-id
          :metadata (obj "tick_type" (gethash "tick_type" proposal)
                         "role" (or (gethash "role" spec) :null)))))

(defun %tick-commit-default-transaction (proposal source-event-id)
  (let ((prepared (mapcar (lambda (spec)
                            (%tick-commit-prepare-memory spec proposal source-event-id))
                          (%tick-commit-list (gethash "memory_specs" proposal))))
        (admitted nil))
    (setf admitted
          (memory-cognitive-mutation-dispatch
           "tick-commit"
           (obj "proposal" proposal "source_event_id" source-event-id
                "prepared" (coerce prepared 'vector))
           (lambda ()
             (%epistemic-run-serializable
              (lambda ()
                (%call-with-memory-durable-event-buffer
                 (lambda ()
                   (setf admitted nil)
                   (with-pg
                     (pomo:with-transaction (:serializable)
                       (dolist (item prepared)
                         (multiple-value-bind (reasons roots)
                             (%epistemic-validation-reasons-current-connection
                              :id (getf item :id) :kind (getf item :kind)
                              :source-event-id (getf item :source-event-id)
                              :origin-class (getf item :origin-class)
                              :epistemic-status (getf item :epistemic-status)
                              :producer (getf item :producer)
                              :model-purpose (getf item :model-purpose)
                              :confidence (getf item :confidence)
                              :grounding-status (getf item :grounding-status)
                              :lineage-parent-ids
                              (getf item :lineage-parent-ids)
                              :novelty-passed t
                              :self-process-event-id source-event-id
                              :epistemic-metadata (getf item :metadata))
                           (when reasons
                             (error 'tick-commit-validation-error
                                    :reasons reasons))
                           (%memory-insert-node-current-connection
                            (getf item :id) (getf item :kind)
                            (getf item :content) (getf item :embedding)
                            (getf item :retrieval-embedding)
                            (getf item :importance) (getf item :valence)
                            (getf item :arousal)
                            (getf item :source-event-id)
                            (getf item :origin-class)
                            (getf item :epistemic-status)
                            (getf item :producer) (getf item :model-purpose)
                            (getf item :confidence)
                            (getf item :grounding-status) roots
                            (getf item :generation-id)
                            (getf item :supersedes-node-id) nil
                            (getf item :metadata))
                           (dolist (parent
                                    (remove-duplicates
                                     (getf item :lineage-parent-ids)
                                     :test #'string=))
                             (%memory-insert-edge-current-connection
                              (getf item :id) parent "derived-from"
                              "tick-commit-lineage"))
                           (when (getf item :supersedes-node-id)
                             (unless
                                 (%epistemic-node-row-current-connection
                                  (getf item :supersedes-node-id))
                               (error 'tick-commit-validation-error
                                      :reasons
                                      (list "superseded node is missing")))
                             (%memory-insert-edge-current-connection
                              (getf item :id)
                              (getf item :supersedes-node-id)
                              "supersedes" "tick-commit-supersession"))
                           (push (getf item :id) admitted)))
                       (dolist (edge
                                (%tick-commit-list
                                 (gethash "edge_specs" proposal)))
                         (%memory-insert-edge-current-connection
                          (gethash "from_id" edge) (gethash "to_id" edge)
                          (gethash "edge_type" edge)
                          "tick-commit-edge"))))))
                (nreverse admitted))))))
    (unless (and (listp admitted)
                 (every (lambda (id) (and (stringp id) (plusp (length id))))
                        admitted)
                 (equal admitted (mapcar (lambda (item) (getf item :id))
                                         prepared)))
      (error 'memory-storage-error :operation :route-cognitive-mutation
             :detail "tick router did not preserve the prepared node IDs"))
    (dolist (item prepared)
      (%memory-after-write (getf item :id) (getf item :kind)
                           (getf item :content) (getf item :importance)))
    admitted))

(defun %tick-commit-dependent-default (proposal admitted-ids)
  (declare (ignore admitted-ids))
  (dolist (fact (%tick-commit-list (gethash "continuity_facts" proposal)))
    (when (fboundp 'log-event)
      (funcall 'log-event "tick-continuity-fact" fact)))
  (dolist (initiative (%tick-commit-list (gethash "initiative_specs" proposal)))
    (when (fboundp 'log-event)
      (funcall 'log-event "tick-initiative-proposed" initiative)))
  (maphash (lambda (name delta)
             (when (and (numberp delta) (fboundp 'modulator-adjust))
               (funcall 'modulator-adjust name delta)))
           (gethash "modulator_deltas" proposal))
  t)

(defun tick-commit-apply (proposal source-event-id &key mode)
  "Validate and atomically apply PROPOSAL, or return a fail-closed outcome."
  (bt:with-lock-held (*tick-commit-lock*)
    (let* ((effective-mode (or mode (and (boundp '*autonomous-write-mode*)
                                         (symbol-value '*autonomous-write-mode*))
                               :normal))
           (reasons (nconc (tick-commit-validate proposal)
                           (and (hash-table-p proposal)
                                (%tick-commit-novelty-reasons proposal))))
           (memories (%tick-commit-list (and (hash-table-p proposal)
                                             (gethash "memory_specs" proposal))))
           (synthetic-count
             (count "synthetic" memories :test #'string=
                    :key (lambda (spec)
                           (or (gethash "origin_class" spec) "synthetic"))))
           (recent-count
             (handler-case
                 (if *tick-commit-recent-count-fn*
                     (funcall *tick-commit-recent-count-fn*)
                     (%tick-commit-recent-synthetic-count))
               (error () nil))))
      (cond
        (reasons
         (%tick-commit-stat "rejected-validation")
         (obj "status" "rejected" "reason" "validation"
              "reasons" (coerce reasons 'vector) "write_count" 0))
        ((eq effective-mode :paused)
         (%tick-commit-stat "paused")
         (obj "status" "skipped" "reason" "autonomous-paused" "write_count" 0))
        ((string/= (gethash "status" proposal) "proposed")
         (%tick-commit-stat (gethash "status" proposal))
         (obj "status" (gethash "status" proposal)
              "reason" (or (gethash "reason" proposal) "handler-outcome")
              "write_count" 0))
        ((null recent-count)
         (%tick-commit-stat "audit-only-cap-unavailable")
         (obj "status" "audit-only" "reason" "synthetic-cap-unavailable"
              "write_count" 0))
        ((>= (+ recent-count synthetic-count)
             (1+ *tick-commit-synthetic-hour-cap*))
         (%tick-commit-stat "audit-only-flood-cap")
         (obj "status" "audit-only" "reason" "synthetic-hour-cap"
              "write_count" 0))
        ((eq effective-mode :shadow-only)
         (%tick-commit-stat "shadow-valid")
         (obj "status" "shadow-valid" "reason" "non-authoritative"
              "write_count" 0 "would_write_count" (length memories)))
        (t
         (handler-case
             (let ((ids (funcall (or *tick-commit-transaction-fn*
                                     #'%tick-commit-default-transaction)
                                 proposal source-event-id)))
               (funcall (or *tick-commit-dependent-fn*
                            #'%tick-commit-dependent-default)
                        proposal ids)
               (%tick-commit-stat "committed")
               (obj "status" "committed" "reason" "validated"
                    "write_count" (length ids)
                    "memory_node_ids" (coerce ids 'vector)))
           (error (condition)
             (%tick-commit-stat "commit-error")
             (obj "status" "error"
                  "reason" (string-downcase (symbol-name (type-of condition)))
                  "write_count" 0))))))))

(defun %tick-terminal-log (type payload &optional caused-by)
  (cond (*tick-terminal-event-fn*
         (funcall *tick-terminal-event-fn* type payload caused-by))
        ((fboundp 'log-event)
         (funcall 'log-event type payload :caused-by caused-by))
        (t nil)))

(defun %tick-event-correlation-id (generation-id)
  (or generation-id
      (and (fboundp 'make-event-tick-id)
           (funcall 'make-event-tick-id "tick-terminal"))
      (format nil "tick-terminal-~d-~d" (get-universal-time)
              (random 1000000))))

(defun %call-with-tick-event-context (tick-id thunk)
  (if (fboundp 'call-with-event-tick-context)
      (funcall 'call-with-event-tick-context tick-id thunk)
      (funcall thunk)))

(defun tick-terminal-call (tick-type thunk &key generation-id)
  "Run THUNK after a start and guarantee exactly one correlated terminal."
  (let ((tick-id (%tick-event-correlation-id generation-id)))
    (%call-with-tick-event-context
     tick-id
     (lambda ()
       (let* ((start (get-internal-real-time))
              (start-id (%tick-terminal-log
                         "tick-start" (obj "type" tick-type
                                           "generation_id" (or generation-id :null))))
              (status "success") (reason "completed") (write-count 0)
              (result nil) (condition-to-signal nil))
         (handler-case
             (progn
               (let ((*tick-terminal-start-event-id* start-id))
                 (setf result (funcall thunk)))
               (when (hash-table-p result)
                 (setf status (or (gethash "status" result) status)
                       reason (or (gethash "reason" result) reason)
                       write-count (or (gethash "write_count" result) 0))))
           (error (condition)
             (setf status "error" reason (string-downcase
                                           (symbol-name (type-of condition)))
                   condition-to-signal condition)))
         (%tick-terminal-log
          "tick-terminal"
          (let ((payload
                  (obj "type" tick-type "generation_id" (or generation-id :null)
                       "status" status "reason" reason "write_count" write-count
                       "duration_ms" (* 1000.0d0
                                        (/ (- (get-internal-real-time) start)
                                           (float internal-time-units-per-second 1.0d0))))))
            (when (hash-table-p result)
              (dolist (key '("cost" "prompt_tokens" "completion_tokens"
                             "legacy_executed" "proposal_status"))
                (when (nth-value 1 (gethash key result))
                  (setf (gethash key payload) (gethash key result)))))
            payload)
          start-id)
         (when condition-to-signal (error condition-to-signal))
         result)))))
