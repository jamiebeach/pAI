;;;; tick-proposals.lisp -- side-effect-free cognitive tick handlers.
;;;; Legacy handlers remain available behind :LEGACY. These builders retrieve
;;;; typed evidence and return commit-set data; only TICK-COMMIT-APPLY mutates.

(in-package :agent)

(export '(tick-build-proposal tick-execute-proposal tick-proposal-report))

(defvar *tick-proposal-evidence-fn* nil)
(defvar *tick-proposal-cognitive-fn* nil)
(defvar *tick-proposal-search-fn* nil)
(defvar *tick-proposal-event-fn* nil)
(defvar *tick-proposal-open-questions-fn* nil)
(defvar *tick-proposal-stats* (make-hash-table :test #'equal))
(defvar *tick-proposal-stats-lock* (bt:make-lock "tick-proposal-stats"))

(defun %tick-proposal-stat (key)
  (bt:with-lock-held (*tick-proposal-stats-lock*)
    (incf (gethash key *tick-proposal-stats* 0))))

(defun tick-proposal-report ()
  (bt:with-lock-held (*tick-proposal-stats-lock*)
    (let ((counts (obj)))
      (maphash (lambda (key value) (setf (gethash key counts) value))
               *tick-proposal-stats*)
      (obj "schema_version" 1 "counts" counts))))

(defun %tick-proposal-generation (type)
  (format nil "tick-~a-~a-~a" type (get-universal-time) (random 1000000)))

(defun %tick-proposal-evidence (query k &optional kinds)
  (if *tick-proposal-evidence-fn*
      (funcall *tick-proposal-evidence-fn* query k kinds)
      (memory-search query :k k :mode :cognitive-evidence :kinds kinds)))

(defun %tick-proposal-cognitive (purpose evidence &rest keys)
  (if *tick-proposal-cognitive-fn*
      (apply *tick-proposal-cognitive-fn* purpose evidence keys)
      (apply #'cognitive-call purpose evidence keys)))

(defun %tick-proposal-log (type payload)
  (cond (*tick-proposal-event-fn* (funcall *tick-proposal-event-fn* type payload))
        ((fboundp 'log-event) (funcall 'log-event type payload))
        (t nil)))

(defun %tick-proposal-skip (generation type reason)
  (%tick-proposal-stat (format nil "~a:skipped" type))
  (make-tick-proposal generation type :status "skipped" :reason reason))

(defun %tick-proposal-record (result)
  (and (hash-table-p result)
       (string= (gethash "status" result "") "accepted")
       (gethash "record" result)))

(defun %tick-proposal-search-result-valid-p (result)
  (and (stringp result)
       (>= (length (string-trim '(#\Space #\Tab #\Newline #\Return) result)) 12)
       (not (some (lambda (marker)
                    (search marker result :test #'char-equal))
                  '("ERROR:" "invalid api key" "unauthorized" "forbidden"
                    "\"error\"" "authentication failed")))))

(defun %tick-proposal-open-questions ()
  "Return rows and whether the authoritative question source is available."
  (cond (*tick-proposal-open-questions-fn*
         (values (funcall *tick-proposal-open-questions-fn*) t))
        ((fboundp 'self-model-active-open-questions)
         (values (funcall 'self-model-active-open-questions :limit 1) t))
        (t (values nil nil))))

(defun %tick-proposal-spec (record evidence kind topic &key role target
                                                         time-horizon evidence-kinds)
  (declare (ignore evidence))
  (let ((spec (obj "kind" kind "content" (gethash "content" record)
                   "record_type" (gethash "record_type" record)
                   "evidence_node_ids" (gethash "evidence_node_ids" record)
                   "uncertainty" (gethash "uncertainty" record)
                   "topic" topic)))
    (when role (setf (gethash "role" spec) role))
    (when target (setf (gethash "target" spec) target))
    (when time-horizon (setf (gethash "time_horizon" spec) time-horizon))
    (when evidence-kinds
      (setf (gethash "evidence_kinds" spec) (coerce evidence-kinds 'vector)))
    spec))

(defun %tick-proposal-one (type purpose query k kind &key required-count
                                                          record-type target
                                                          time-horizon kinds
                                                          evidence-kinds topic)
  (let* ((generation (%tick-proposal-generation type))
         (evidence (%tick-proposal-evidence query k kinds))
         (needed (or required-count 1)))
    (if (< (length evidence) needed)
        (%tick-proposal-skip generation type "insufficient-typed-evidence")
        (let* ((result (%tick-proposal-cognitive
                        purpose evidence :question query :topic (or topic query)
                        :generation-id generation))
               (record (%tick-proposal-record result)))
          (if (or (not record)
                  (and record-type
                       (not (string= record-type
                                     (gethash "record_type" record "")))))
              (%tick-proposal-skip generation type
                                   (if record "wrong-record-type"
                                       (or (and (hash-table-p result)
                                                (gethash "status" result))
                                           "cognitive-rejected")))
              (progn
                (%tick-proposal-stat (format nil "~a:proposed" type))
                (make-tick-proposal
                 generation type
                 :memory-specs
                 (list (%tick-proposal-spec
                        record evidence kind (or topic query)
                        :target target :time-horizon time-horizon
                        :evidence-kinds
                        (or evidence-kinds
                            (mapcar (lambda (node) (gethash "kind" node)) evidence))))
                 :continuity-facts
                 (list (obj "fact_type" "cognitive-proposal"
                            "generation_id" generation "tick_type" type
                            "evidence_count" (length evidence))))))))))

(defun %tick-propose-rumination ()
  (multiple-value-bind (questions source-available-p)
      (%tick-proposal-open-questions)
    (if (and source-available-p (null questions))
        (%tick-proposal-skip (%tick-proposal-generation "ruminate")
                             "ruminate" "no-active-open-question")
        (let* ((question (first questions))
               (statement (and (hash-table-p question)
                               (gethash "statement" question)))
               (query (if (and (stringp statement) (plusp (length statement)))
                          statement "unresolved grounded uncertainty")))
          (%tick-proposal-one "ruminate" "rumination" query 4
                              "thought" :record-type "hypothesis"
                              :topic query)))))

(defun %tick-propose-explore ()
  (let* ((type "explore") (generation (%tick-proposal-generation type))
         (topic "develop a materially new grounded point of view")
         (evidence (%tick-proposal-evidence topic 5 nil)))
    (if (null evidence)
        (%tick-proposal-skip generation type "insufficient-typed-evidence")
        (labels ((attempt (seed)
                   (%tick-proposal-cognitive
                    "worldview-exploration" evidence
                    :question (format nil "Develop a distinct claim using reorientation seed: ~a" seed)
                    :topic topic :generation-id generation :max-words 80)))
          (let* ((first (attempt "primary-evidence-connection"))
                 (retry-p (string= (gethash "status" first "") "rejected-duplicate"))
                 (result (if retry-p (attempt "different-root-and-implication") first))
                 (duplicate-id (and retry-p (gethash "duplicate_of_node_id" first)))
                 (record (%tick-proposal-record result)))
            (when retry-p
              (%tick-proposal-stat "explore:duplicate-retry")
              (%tick-proposal-log
               "tick-proposal-duplicate-merged"
               (obj "generation_id" generation "tick_type" type
                    "attempt" 1 "existing_node_id" (or duplicate-id :null))))
            (if (not record)
                (progn
                  (when retry-p
                    (%tick-proposal-log
                     "tick-proposal-duplicate-merged"
                     (obj "generation_id" generation "tick_type" type
                          "attempt" 2 "existing_node_id"
                          (or (gethash "duplicate_of_node_id" result) :null))))
                  (%tick-proposal-skip
                   generation type
                   (if retry-p "duplicate-after-different-seed-retry"
                       (or (gethash "status" result) "cognitive-rejected"))))
                (let ((spec (%tick-proposal-spec record evidence "worldview" topic)))
                  (when (and duplicate-id (stringp duplicate-id))
                    (setf (gethash "supersedes_node_id" spec) duplicate-id))
                  (make-tick-proposal
                   generation type :memory-specs (list spec)
                   :continuity-facts
                   (list (obj "fact_type" "cognitive-proposal"
                              "generation_id" generation "tick_type" type
                              "evidence_count" (length evidence)
                              "duplicate_retry" (if retry-p t nil)))))))))))

(defun %tick-propose-curiosity ()
  (let* ((type "curiosity") (generation (%tick-proposal-generation type))
         (evidence (%tick-proposal-evidence "specific unresolved curiosity" 3 nil)))
    (if (or (null evidence)
            (not (or *tick-proposal-search-fn* (fboundp 'brave-search))))
        (%tick-proposal-skip generation type "curiosity-input-unavailable")
        (let* ((query-result
                 (%tick-proposal-cognitive
                  "curiosity-synthesis" evidence
                  :question "Return a concise, evidence-grounded search question."
                  :topic "curiosity-search-question" :generation-id generation
                  :max-words 12))
               (query-record (%tick-proposal-record query-result)))
          (if (not query-record)
              (%tick-proposal-skip generation type
                                   (or (gethash "status" query-result)
                                       "query-rejected"))
              (let* ((query (gethash "content" query-record))
                     (external-id (format nil "external-~a" generation))
                     (raw-result (if *tick-proposal-search-fn*
                                     (funcall *tick-proposal-search-fn* query)
                                     (brave-search query :count 3)))
                     (valid-result
                       (%tick-proposal-search-result-valid-p raw-result))
                     (external-event
                       (and valid-result
                            (%tick-proposal-log
                             "external-signal"
                             (obj "generation_id" generation "provider" "search"
                                  "query_chars" (length query)))))
                     (external-node
                       (obj "id" external-id "kind" "observation"
                            "content" raw-result "origin_class" "external-signal"
                            "epistemic_status" "direct-event"
                            "grounding_status" "grounded" "producer" "search"
                            "root_observation_ids" (vector external-id)
                            "quarantined" nil))
                     (synthesis-evidence (append evidence (list external-node)))
                     (synthesis-result
                       (and valid-result
                            (%tick-proposal-cognitive
                             "curiosity-synthesis" synthesis-evidence
                             :question query :topic query :generation-id generation)))
                     (synthesis (%tick-proposal-record synthesis-result)))
                (if (or (not valid-result) (not synthesis) (not external-event))
                    (%tick-proposal-skip generation type
                                         (if valid-result
                                             "search-or-synthesis-rejected"
                                             "invalid-search-result"))
                    (make-tick-proposal
                     generation type
                     :memory-specs
                     (list (%tick-proposal-spec query-record evidence "thought" query
                                                :role "search-question")
                           (obj "id" external-id "kind" "observation"
                                "content" raw-result "record_type" "direct-event"
                                "origin_class" "external-signal"
                                "grounding_status" "grounded"
                                "source_event_id" external-event
                                "uncertainty" 0.0d0 "topic" query
                                "role" "external-result"
                                "evidence_node_ids" (vector))
                           (%tick-proposal-spec synthesis synthesis-evidence
                                                "reflection" query :role "synthesis"))
                     :continuity-facts
                     (list (obj "fact_type" "external-search-completed"
                                "generation_id" generation
                                "provider" "search"))))))))))

(defun tick-build-proposal (selected-type)
  "Build one mutation-free commit set using typed retrieval and COGNITIVE-CALL."
  (cond
    ((string= selected-type "idle-drift")
     (%tick-proposal-one "idle-drift" "idle-association" "one grounded memory" 1
                         "thought" :record-type "hypothesis"))
    ((string= selected-type "consolidate")
     (if (and (boundp '*importance-since-last-reflection*)
              (boundp '*reflection-importance-threshold*)
              (>= *importance-since-last-reflection* *reflection-importance-threshold*))
         (%tick-proposal-one "full-reflection" "deep-reflection"
                             "recent grounded patterns" 8 "reflection"
                             :required-count 2)
         (%tick-proposal-one "light-consolidate" "light-consolidation"
                             "recent grounded connections" 5 "reflection"
                             :required-count 2)))
    ((string= selected-type "anticipate")
     (%tick-proposal-one "anticipate" "anticipation" "the operator's open grounded plans" 5
                         "prediction" :record-type "prediction"
                         :target "the operator's next relevant need"
                         :time-horizon "next interaction"))
    ((string= selected-type "ruminate")
     (%tick-propose-rumination))
    ((string= selected-type "curiosity") (%tick-propose-curiosity))
    ((string= selected-type "explore") (%tick-propose-explore))
    ((string= selected-type "episode-replay")
     (%tick-proposal-one "episode-replay" "episode-replay" "grounded episodes" 5
                         "reflection" :required-count 1 :kinds '("episode")
                         :evidence-kinds '("episode")))
    ((string= selected-type "maintenance")
     (make-tick-proposal (%tick-proposal-generation "maintenance") "maintenance"
                         :continuity-facts
                         (list (obj "fact_type" "maintenance-requested"))))
    (t (error "Unsupported selected tick type ~s" selected-type))))

(defun tick-execute-proposal (selected-type source-event-id &key mode)
  (let ((proposal (tick-build-proposal selected-type)))
    (tick-commit-apply proposal source-event-id :mode mode)))
