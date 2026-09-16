;;;; creative-projects.lisp -- Slice C story contracts and novelty authority.
;;;;
;;;; Model transport remains injected through COGNITIVE-ARTIFACT-CALL. This
;;;; module owns deterministic output validation and never starts a worker,
;;;; observes ticks, publishes content, or enables delivery.

(in-package :agent)

(export '(creative-project-create-from-approved-proposal creative-project-get
          creative-project-next-operation creative-project-run-operation
          creative-project-validate-artifact-output
          creative-project-install-cognitive-adapters creative-project-report))

(defparameter *creative-project-contract-version* "story-progress-v1")
(defparameter *creative-project-embedding-threshold* 0.92d0)
(defparameter *creative-project-lexical-threshold* 0.85d0)
(defvar *creative-project-expected-embedding-digest* nil
  "Pinned nomic-embed-text manifest digest required before embedding novelty is used.")
(defvar *creative-project-installed-embedding-digest-fn* nil
  "Injected zero-argument installed-manifest digest resolver.")
(defvar *creative-project-embedding-fn* nil
  "Injected text -> numeric-vector embedding function.")
(defvar *creative-project-stats* (make-hash-table :test #'equal))
(defvar *creative-project-stats-lock* (bt:make-lock "creative-project-stats"))

(defun %creative-project-stat (key)
  (bt:with-lock-held (*creative-project-stats-lock*)
    (incf (gethash key *creative-project-stats* 0))))

(defun %creative-project-row-object (row)
  (when row
    (destructuring-bind (id process title why intended next artifact sharing
                         uncertainty created updated metadata-json)
        row
      (obj "id" id "process_id" process "title" (or title :null)
           "why_cares" why "intended_form" intended
           "next_operation_type" (or next :null)
           "current_artifact_id" (or artifact :null)
           "sharing_condition" sharing "uncertainty" uncertainty
           "created_at" created "updated_at" updated
           "metadata" (%fpe-json-read metadata-json (obj))))))

(defun creative-project-get (project-id)
  (with-pg
    (%creative-project-row-object
     (pomo:query
      "SELECT id,process_id,title,why_cares,intended_form,next_operation_type,current_artifact_id,sharing_condition,uncertainty,created_at::text,updated_at::text,metadata::text FROM creative_projects WHERE id=$1"
      project-id :row))))

(defun creative-project-create-from-approved-proposal (proposal-id &key budget)
  (agent-process-start-from-approved-proposal proposal-id :budget budget))

(defun creative-project-next-operation (project-id &key now scheduler-cycle-id)
  (declare (ignore now))
  (let ((project (creative-project-get project-id)))
    (unless project (%fpe-reject "missing-project" "project ~a does not exist" project-id))
    (if scheduler-cycle-id
        (grounded-agency-claim-next-operation
         (gethash "process_id" project) scheduler-cycle-id)
        (gethash "next_operation_type" project))))

(defun creative-project-run-operation (project-id scheduler-cycle-id)
  (let ((operation (creative-project-next-operation
                    project-id :scheduler-cycle-id scheduler-cycle-id)))
    (grounded-agency-worker-signal)
    operation))

(defun %creative-project-normalized-tokens (content)
  (let ((spaced
          (map 'string
               (lambda (character)
                 (if (alphanumericp character) (char-downcase character) #\Space))
               (%fpe-canonical-content content))))
    (remove-if (lambda (part) (zerop (length part)))
               (uiop:split-string spaced :separator '(#\Space #\Tab #\Newline #\Return)))))

(defun %creative-project-trigrams (content)
  (let ((tokens (%creative-project-normalized-tokens content)))
    (loop for rest on tokens
          while (cddr rest)
          collect (format nil "~a~c~a~c~a"
                          (first rest) #\Null (second rest) #\Null (third rest)))))

(defun %creative-project-jaccard (left right)
  (let ((left-set (make-hash-table :test #'equal))
        (right-set (make-hash-table :test #'equal)))
    (dolist (item (%creative-project-trigrams left)) (setf (gethash item left-set) t))
    (dolist (item (%creative-project-trigrams right)) (setf (gethash item right-set) t))
    (when (or (zerop (hash-table-count left-set))
              (zerop (hash-table-count right-set)))
      (return-from %creative-project-jaccard nil))
    (let ((intersection 0) (union (hash-table-count left-set)))
      (maphash (lambda (key value)
                 (declare (ignore value))
                 (if (gethash key left-set) (incf intersection) (incf union)))
               right-set)
      (/ intersection (float union 1.0d0)))))

(defun %creative-project-cosine (left right)
  (unless (= (length left) (length right))
    (%fpe-reject "embedding-shape" "novelty vectors have different dimensions"))
  (let ((dot 0.0d0) (left-norm 0.0d0) (right-norm 0.0d0))
    (loop for a across left for b across right
          do (incf dot (* a b))
             (incf left-norm (* a a))
             (incf right-norm (* b b)))
    (when (or (zerop left-norm) (zerop right-norm))
      (return-from %creative-project-cosine nil))
    (/ dot (sqrt (* left-norm right-norm)))))

(defun %creative-project-embedding-ready-p ()
  (and (stringp *creative-project-expected-embedding-digest*)
       (functionp *creative-project-installed-embedding-digest-fn*)
       (functionp *creative-project-embedding-fn*)
       (string= *creative-project-expected-embedding-digest*
                (or (funcall *creative-project-installed-embedding-digest-fn*) ""))))

(defun %creative-project-similarity (left right)
  (if (%creative-project-embedding-ready-p)
      (let ((similarity
              (%creative-project-cosine
               (funcall *creative-project-embedding-fn* left)
               (funcall *creative-project-embedding-fn* right))))
        (unless (numberp similarity)
          (%fpe-reject "novelty-unavailable" "embedding similarity cannot be computed"))
        (values similarity "nomic-embed-text" *creative-project-embedding-threshold*))
      (let ((similarity (%creative-project-jaccard left right)))
        (unless (numberp similarity)
          (%fpe-reject "novelty-unavailable" "lexical trigram similarity cannot be computed"))
        (values similarity "token-trigram-jaccard"
                *creative-project-lexical-threshold*))))

(defun %creative-project-prior-contents (operation)
  (let ((artifact-id (gethash "input_artifact_id" operation))
        (version (gethash "input_artifact_version" operation)))
    (if (and (string= (gethash "operation_type" operation) "revise")
             (stringp artifact-id) (integerp version))
        (with-pg
          (pomo:query
           "SELECT content FROM agent_artifact_versions WHERE artifact_id=$1 AND version<=$2 ORDER BY version"
           artifact-id version :column))
        nil)))

(defun creative-project-validate-artifact-output (operation content metadata)
  "Validate canonical content, operation identity, private/synthetic marking,
and pinned novelty. Return the durable validation object or fail closed."
  (let* ((type (gethash "operation_type" operation))
         (words (%gaw-word-count content))
         (minimum (if (string= type "outline") 3 8))
         (maximum (if (string= type "outline") 800 2500))
         (maximum-similarity 0.0d0)
         (method "not-applicable")
         (threshold :null))
    (unless (member type '("outline" "draft" "revise") :test #'string=)
      (%fpe-reject "artifact-operation-type" "~a cannot produce story content" type))
    (unless (<= minimum words maximum)
      (%fpe-reject "artifact-word-bounds"
                   "~a content must contain ~a through ~a words" type minimum maximum))
    (unless (and (hash-table-p metadata)
                 (string= (or (gethash "visibility" metadata) "") "private")
                 (string= (or (gethash "content_class" metadata) "") "synthetic"))
      (%fpe-reject "artifact-classification"
                   "story artifacts must be explicitly private and synthetic"))
    (when (%cognitive-artifact-planning-leak-p content)
      (%fpe-reject "planning-label-leakage" "artifact body contains a private planning label"))
    (dolist (prior (%creative-project-prior-contents operation))
      (multiple-value-bind (similarity comparison-method comparison-threshold)
          (%creative-project-similarity content prior)
        (setf maximum-similarity (max maximum-similarity similarity)
              method comparison-method
              threshold comparison-threshold)
        (when (>= similarity comparison-threshold)
          (%creative-project-stat "rejected-near-duplicate")
          (%fpe-reject "near-duplicate-artifact"
                       "~a similarity ~,4f meets rejection threshold ~,4f"
                       comparison-method similarity comparison-threshold))))
    (%creative-project-stat "validated")
    (obj "passed" t "validation_version" *creative-project-contract-version*
         "operation_type" type "word_count" words
         "maximum_prior_version_similarity" maximum-similarity
         "novelty_method" method "novelty_threshold" threshold
         "embedding_manifest_digest"
         (if (%creative-project-embedding-ready-p)
             *creative-project-expected-embedding-digest* :null)
         "violation_codes" (vector))))

(defun %creative-project-purpose (operation-type)
  (cond ((string= operation-type "outline") "creative-story-outline")
        ((string= operation-type "draft") "creative-story-draft")
        ((string= operation-type "revise") "creative-story-revision")
        (t (%fpe-reject "artifact-operation-type" "unsupported type ~a" operation-type))))

(defun %creative-project-adapter (operation context cancelled-p heartbeat)
  (when (funcall cancelled-p)
    (%fpe-reject "public-turn-preemption" "artifact call cancelled before invocation"))
  (funcall heartbeat :force t)
  (let* ((type (gethash "operation_type" operation))
         (input (gethash "input_artifact" context))
         (prior (and (hash-table-p input) (gethash "content" input)))
         (contract
           (obj "contract_version" *creative-project-contract-version*
                "operation_type" type
                "required_change"
                (cond ((string= type "outline") "create a bounded story outline")
                      ((string= type "draft") "transform the exact outline into a draft")
                      (t "materially revise the exact prior draft"))))
         (call
           (cognitive-artifact-call
            (%creative-project-purpose type)
            (coerce (gethash "inspirations" context) 'list)
            :project-contract contract :prior-artifact prior
            :generation-id (gethash "id" operation)))
         (usage (gethash "usage" call))
         (record (gethash "record" call)))
    (obj "adapter_status" (gethash "status" call)
         "adapter_reason" (gethash "reason" call)
         "content" (if (hash-table-p record)
                       (gethash "artifact_content" record) "")
         "usage" (obj "model_name"
                      (or (and (boundp '*model*) (symbol-value '*model*)) "unknown")
                      "prompt_tokens"
                      (if (integerp (gethash "prompt_tokens" usage))
                          (gethash "prompt_tokens" usage) 0)
                      "completion_tokens"
                      (if (integerp (gethash "completion_tokens" usage))
                          (gethash "completion_tokens" usage) 0)
                      "cost" (if (numberp (gethash "cost_usd" usage))
                                 (gethash "cost_usd" usage) 0.0d0))
         "metadata" (obj "visibility" "private"
                         "content_class" "synthetic"
                         "contract_version" *creative-project-contract-version*))))

(defun creative-project-install-cognitive-adapters ()
  "Install the tool-free artifact-call adapter for all three model stages.
Installation performs no model or network call."
  (dolist (type '("outline" "draft" "revise"))
    (grounded-agency-register-operation-adapter type #'%creative-project-adapter))
  t)

(defun creative-project-report ()
  (let ((stats (obj)))
    (bt:with-lock-held (*creative-project-stats-lock*)
      (maphash (lambda (key value) (setf (gethash key stats) value))
               *creative-project-stats*))
    (obj "schema_version" 1
         "contract_version" *creative-project-contract-version*
         "embedding_threshold" *creative-project-embedding-threshold*
         "lexical_threshold" *creative-project-lexical-threshold*
         "embedding_digest_configured"
         (if (stringp *creative-project-expected-embedding-digest*) t nil)
         "embedding_ready" (if (%creative-project-embedding-ready-p) t nil)
         "stats" stats)))
