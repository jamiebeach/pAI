;;;; conversation-episode-graph.lisp -- pure episodic projection and recall.
;;;;
;;;; Raw dialogue and sealed episode events remain authority.  This file owns
;;;; only rebuildable projections: deterministic episode ranges, semantic
;;;; concept links, and bounded selection.  It performs no IO and no writes.

(in-package :agent)

(export '(conversation-episode-candidates
          conversation-episode-next-sealable
          conversation-episode-coverage-report
          conversation-unsealed-dialogue-context-records
          conversation-episode-sealed-payload-valid-p
          conversation-episode-project
          conversation-episode-graph-project
          conversation-episode-graph-materialization
          conversation-episode-recall
          conversation-episode-context-records))

(defparameter *conversation-episode-projection-revision*
  "conversation-episode-graph-v1")
(defparameter *conversation-episode-quiet-gap-seconds* 7200)
(defparameter *conversation-episode-max-span-seconds* 21600)
(defparameter *conversation-episode-max-public-messages* 24)
(defparameter *conversation-episode-max-characters* 36000)
(defparameter *conversation-episode-semantic-fields*
  '("subjects" "entities" "retrieval_cues" "broader_categories"
    "unresolved_threads"))
(defvar *conversation-episode-semantic-score-fn* nil
  "Optional pure/read-only (cue episode) -> score in [0,1].")

(defun %conversation-episode-items (value)
  (cond ((null value) nil)
        ((listp value) value)
        ((vectorp value) (coerce value 'list))
        (t nil)))

(defun %conversation-episode-payload (event)
  (and (hash-table-p event) (gethash "payload" event)))

(defun %conversation-episode-metadata (event)
  (let ((payload (%conversation-episode-payload event)))
    (and (hash-table-p payload) (gethash "metadata" payload))))

(defun %conversation-episode-historical-role (event agent-id persona-id)
  "Admit a closed migration receipt without making it a live stimulus."
  (let* ((payload (%conversation-episode-payload event))
         (metadata (%conversation-episode-metadata event))
         (type (and (hash-table-p event) (gethash "type" event "")))
         (source-type (and (hash-table-p metadata)
                           (gethash "source_event_type" metadata "")))
         (digest (and (hash-table-p metadata)
                      (gethash "source_event_sha256" metadata))))
    (when (and (hash-table-p payload) (hash-table-p metadata)
               (equal agent-id (gethash "agent_id" event))
               (string= persona-id (gethash "persona_id" metadata ""))
               (string= "historical-agent-migration-v1"
                        (gethash "source" metadata ""))
               (stringp (gethash "source_agent_id" metadata))
               (plusp (length (gethash "source_agent_id" metadata)))
               (integerp (gethash "source_event_id" metadata))
               (plusp (gethash "source_event_id" metadata))
               (stringp digest) (= 64 (length digest))
               (stringp (gethash "text" payload))
               (plusp (length (gethash "text" payload))))
      (cond ((and (string= type "historical-user-message-imported")
                  (string= source-type "user-message"))
             "operator")
            ((and (string= type "historical-agent-message-imported")
                  (string= source-type "agent-message"))
             "historical-assistant")))))

(defun %conversation-episode-message-role (event agent-id persona-id)
  (or (%conversation-episode-historical-role event agent-id persona-id)
      (let* ((payload (%conversation-episode-payload event))
             (metadata (%conversation-episode-metadata event))
             (type (and (hash-table-p event) (gethash "type" event "")))
             (source (and (hash-table-p metadata)
                          (gethash "source" metadata ""))))
        (when (and (hash-table-p payload) (hash-table-p metadata)
                   (equal agent-id (gethash "agent_id" event))
                   (string= persona-id (gethash "persona_id" metadata ""))
                   (member type '("user-message" "agent-message")
                           :test #'string=)
                   (or (string= source "recursive-mind-v1")
                       (string= source "q4.5-conversation"))
                   (stringp (gethash "text" payload))
                   (plusp (length (gethash "text" payload))))
          (if (string= type "user-message") "operator" "assistant")))))

(defun %conversation-episode-time (event)
  (let* ((value (gethash "timestamp" event))
         (parsed
           (and (fboundp '%event-parse-ts)
                (handler-case (funcall '%event-parse-ts event)
                  (error () nil)))))
    (cond ((and (integerp parsed) (plusp parsed)) parsed)
          ((integerp value) value)
          ((and (stringp value) (>= (length value) 19))
           (handler-case
               (encode-universal-time
                (parse-integer value :start 17 :end 19)
                (parse-integer value :start 14 :end 16)
                (parse-integer value :start 11 :end 13)
                (parse-integer value :start 8 :end 10)
                (parse-integer value :start 5 :end 7)
                (parse-integer value :start 0 :end 4) 0)
             (error () 0)))
          (t 0))))

(defun %conversation-episode-public-message-p (event agent-id persona-id)
  (not (null (%conversation-episode-message-role event agent-id persona-id))))

(defun %conversation-episode-user-message-p (event agent-id persona-id)
  (string= "operator"
           (or (%conversation-episode-message-role event agent-id persona-id)
               "")))

(defun %conversation-episode-assistant-message-p (event agent-id persona-id)
  (member (%conversation-episode-message-role event agent-id persona-id)
          '("assistant" "historical-assistant") :test #'string=))

(defun %conversation-episode-completed-root-ids (events agent-id persona-id)
  (loop for event in events
        when (and (%conversation-episode-public-message-p
                   event agent-id persona-id)
                  (%conversation-episode-assistant-message-p
                   event agent-id persona-id)
                  (integerp (gethash "caused_by" event)))
          collect (gethash "caused_by" event)))

(defun %conversation-episode-row (event agent-id persona-id)
  (let ((payload (%conversation-episode-payload event)))
    (obj "event_id" (gethash "id" event)
         "role" (if (%conversation-episode-user-message-p
                     event agent-id persona-id)
                      "operator" "assistant")
         "text" (gethash "text" payload)
         "timestamp" (%conversation-episode-time event))))

(defun %conversation-episode-finalize (rows persona-id)
  (let* ((ordered (nreverse rows))
         (first (first ordered))
         (last (car (last ordered)))
         (ids (mapcar (lambda (row) (gethash "event_id" row)) ordered))
         (first-id (first ids))
         (last-id (car (last ids))))
    (obj "schema_version" 1
         "episode_id" (format nil "episode:~a:~d:~d"
                              persona-id first-id last-id)
         "persona_id" persona-id
         "first_event_id" first-id
         "last_event_id" last-id
         "first_timestamp" (gethash "timestamp" first)
         "last_timestamp" (gethash "timestamp" last)
         "source_event_ids" (coerce ids 'vector)
         "turns" (coerce ordered 'vector))))

(defun %conversation-episode-completed-pairs
    (events agent-id persona-id &optional excluded-source-event-ids)
  "Return authority-ordered completed public pairs outside settled coverage."
  (let ((agents (make-hash-table :test #'equal))
        (pairs nil))
    (dolist (event events)
      (when (and (%conversation-episode-public-message-p
                  event agent-id persona-id)
                 (%conversation-episode-assistant-message-p
                  event agent-id persona-id)
                 (integerp (gethash "caused_by" event)))
        (setf (gethash (gethash "caused_by" event) agents) event)))
    (dolist (event events)
      (when (and (%conversation-episode-public-message-p
                  event agent-id persona-id)
                 (%conversation-episode-user-message-p
                  event agent-id persona-id))
        (let* ((user-id (gethash "id" event))
               (agent (gethash user-id agents))
               (agent-id-value (and agent (gethash "id" agent))))
          (when (and agent
                     (not (or (member user-id excluded-source-event-ids
                                      :test #'equal)
                              (member agent-id-value excluded-source-event-ids
                                      :test #'equal))))
            (push (list (%conversation-episode-row event agent-id persona-id)
                        (%conversation-episode-row agent agent-id persona-id))
                  pairs)))))
    (nreverse pairs)))

(defun %conversation-episode-pair-characters (pair)
  (loop for row in pair sum (length (gethash "text" row ""))))

(defun conversation-episode-candidates
    (events agent-id persona-id
     &key (quiet-gap-seconds *conversation-episode-quiet-gap-seconds*)
          (max-span-seconds *conversation-episode-max-span-seconds*)
          (max-public-messages *conversation-episode-max-public-messages*)
          (max-characters *conversation-episode-max-characters*)
          excluded-source-event-ids)
  "Partition completed dialogue at quiet or bounded completed-pair edges."
  (unless (and (stringp agent-id) (plusp (length agent-id))
               (stringp persona-id) (plusp (length persona-id))
               (integerp quiet-gap-seconds) (plusp quiet-gap-seconds)
               (integerp max-span-seconds) (plusp max-span-seconds)
               (integerp max-public-messages) (>= max-public-messages 2)
               (evenp max-public-messages)
               (integerp max-characters) (plusp max-characters))
    (error "Conversation episode projection arguments are invalid"))
  (let* ((ordered (sort (copy-list (%conversation-episode-items events)) #'<
                        :key (lambda (event) (gethash "id" event 0))))
         (pairs (%conversation-episode-completed-pairs
                 ordered agent-id persona-id excluded-source-event-ids))
         (rows nil)
         (episodes nil)
         (first-time nil)
         (last-time nil)
         (characters 0)
         (message-count 0))
    (labels ((finish ()
               (when rows
                 (push (%conversation-episode-finalize rows persona-id) episodes))
               (setf rows nil first-time nil last-time nil
                     characters 0 message-count 0)))
      (dolist (pair pairs)
        (let* ((pair-first (gethash "timestamp" (first pair) 0))
               (pair-last (gethash "timestamp" (second pair) 0))
               (pair-characters (%conversation-episode-pair-characters pair))
               (boundary-p
                 (and rows
                      (or (> (- pair-first last-time) quiet-gap-seconds)
                          (> (- pair-last first-time) max-span-seconds)
                          (> (+ message-count 2) max-public-messages)
                          (> (+ characters pair-characters)
                             max-characters)))))
          (when boundary-p (finish))
          ;; Push in reverse because %CONVERSATION-EPISODE-FINALIZE restores
          ;; authority order once, after a boundary has kept the pair atomic.
          (push (first pair) rows)
          (push (second pair) rows)
          (unless first-time (setf first-time pair-first))
          (setf last-time pair-last)
          (incf characters pair-characters)
          (incf message-count 2)))
      (finish))
    (coerce (nreverse episodes) 'vector)))

(defun %conversation-episode-sealed-source-ids (events agent-id persona-id)
  (remove-duplicates
   (loop for event in (%conversation-episode-items events)
         for payload = (%conversation-episode-payload event)
         when (and (conversation-episode-sealed-payload-valid-p payload)
                   (equal agent-id (gethash "agent_id" event))
                   (string= persona-id (gethash "persona_id" payload ""))
                   (string= "conversation-episode-sealed"
                            (gethash "type" event "")))
           append (coerce (gethash "source_event_ids" payload) 'list))
   :test #'equal))

(defun conversation-episode-next-sealable
    (events agent-id persona-id now
     &key (quiet-gap-seconds *conversation-episode-quiet-gap-seconds*))
  "Return the oldest completed, quiet, unsealed episode candidate."
  (let* ((covered (%conversation-episode-sealed-source-ids
                   events agent-id persona-id))
         (episodes (conversation-episode-candidates
                    events agent-id persona-id
                    :quiet-gap-seconds quiet-gap-seconds
                    :excluded-source-event-ids covered)))
    (loop for index from 0 below (length episodes)
          for episode = (aref episodes index)
          for closed-by-successor-p = (< index (1- (length episodes)))
          for quiet-p = (>= (- now (gethash "last_timestamp" episode 0))
                            quiet-gap-seconds)
          when (or closed-by-successor-p quiet-p)
            return episode)))

(defun conversation-episode-coverage-report (events agent-id persona-id)
  "Describe sealed coverage and newer completed dialogue without mutation."
  (let* ((sealed
           (sort
            (loop for event in (%conversation-episode-items events)
                  for payload = (%conversation-episode-payload event)
                  when (and (conversation-episode-sealed-payload-valid-p payload)
                            (equal agent-id (gethash "agent_id" event))
                            (string= persona-id
                                     (gethash "persona_id" payload ""))
                            (string= "conversation-episode-sealed"
                                     (gethash "type" event "")))
                    collect payload)
            #'< :key (lambda (payload) (gethash "last_event_id" payload 0))))
         (latest (car (last sealed)))
         (frontier (if latest (gethash "last_event_id" latest) 0))
         (covered (%conversation-episode-sealed-source-ids
                   events agent-id persona-id))
         (pending (conversation-episode-candidates
                   events agent-id persona-id
                   :excluded-source-event-ids covered)))
    (obj "schema_version" 1
         "sealed_episode_count" (length sealed)
         "sealed_through_event_id" (if latest frontier :null)
         "sealed_through_timestamp"
         (if latest (gethash "last_timestamp" latest) :null)
         "pending_episode_count" (length pending)
         "newer_completed_dialogue" (if (plusp (length pending)) t nil)
         "next_pending_episode_id"
         (if (plusp (length pending))
             (gethash "episode_id" (aref pending 0)) :null))))

(defun %conversation-episode-term-match-p (left right)
  (or (string= left right)
      (and (>= (length left) 4) (>= (length right) 4)
           (string= (subseq left 0 4) (subseq right 0 4)))))

(defun %conversation-episode-raw-pair-score (cue pair)
  (let ((query-terms (%conversation-episode-terms cue))
        (text-terms
          (%conversation-episode-terms
           (format nil "~a ~a" (gethash "text" (first pair) "")
                   (gethash "text" (second pair) "")))))
    (count-if
     (lambda (query-term)
       (some (lambda (text-term)
               (%conversation-episode-term-match-p query-term text-term))
             text-terms))
     query-terms)))

(defun conversation-unsealed-dialogue-context-records
    (events agent-id persona-id cue
     &key (maximum 3) (character-budget 4000)
          (record-character-limit 1800))
  "Recall completed raw pairs not yet covered by a sealed episode."
  (unless (and (stringp cue) (plusp (length cue))
               (integerp maximum) (<= 1 maximum 8)
               (integerp character-budget) (plusp character-budget)
               (integerp record-character-limit) (plusp record-character-limit))
    (error "Unsealed dialogue recall arguments are invalid"))
  (let* ((ordered (sort (copy-list (%conversation-episode-items events)) #'<
                        :key (lambda (event) (gethash "id" event 0))))
         (coverage (conversation-episode-coverage-report
                    ordered agent-id persona-id))
         (covered (%conversation-episode-sealed-source-ids
                   ordered agent-id persona-id))
         (pairs (%conversation-episode-completed-pairs
                 ordered agent-id persona-id covered))
         (ranked
           (sort
            (loop for pair in pairs
                  for score = (%conversation-episode-raw-pair-score cue pair)
                  when (plusp score) collect (cons score pair))
            (lambda (left right)
              (if (= (car left) (car right))
                  (> (gethash "event_id" (second (cdr left)) 0)
                     (gethash "event_id" (second (cdr right)) 0))
                  (> (car left) (car right))))))
         (records nil) (ids nil) (selected-source-ids nil) (used 0)
         (budget-refusals 0))
    (dolist (scored ranked)
      (when (>= (length records) maximum)
        (return))
      (let* ((pair (cdr scored))
             (user (first pair)) (assistant (second pair))
             (user-id (gethash "event_id" user))
             (assistant-id (gethash "event_id" assistant))
             (prefix
               (format nil
                       "Recent unsealed raw dialogue for persona ~a, exact source events ~a and ~a at authority timestamps ~a and ~a. This authority-backed pair is newer than or absent from the sealed episodic frontier (~a); retrieval remains non-exhaustive. operator: "
                       persona-id user-id assistant-id
                       (gethash "timestamp" user)
                       (gethash "timestamp" assistant)
                       (gethash "sealed_through_event_id" coverage :null)))
             (body (format nil "~a assistant: ~a"
                           (gethash "text" user "")
                           (gethash "text" assistant "")))
             (content (concatenate 'string prefix body))
             (size (length content))
             (source-id (format nil "conversation-raw:~a:~a"
                                user-id assistant-id)))
        (if (and (<= size record-character-limit)
                 (<= (+ used size) character-budget))
            (progn
              (push (obj "source_id" source-id "content" content
                         "provenance"
                         (obj "descriptor_id" source-id
                              "descriptor_event_id" assistant-id
                              "evidence_event_ids"
                              (vector user-id assistant-id)))
                    records)
              ;; Keep this list one-for-one with RECORDS.  The shared context
              ;; merger admits provenance evidence only after admitting its
              ;; owning record under the final character budget.
              (push source-id ids)
              (push source-id selected-source-ids)
              (incf used size))
            (incf budget-refusals))))
    (setf (gethash "status" coverage)
          (if records "selected" "empty")
          (gethash "selected_count" coverage) (length records)
          (gethash "selected_ids" coverage)
          (coerce (nreverse selected-source-ids) 'vector)
          (gethash "rendered_characters" coverage) used
          (gethash "budget_refusal_count" coverage) budget-refusals
          (gethash "database_write_count" coverage) 0)
    (values (coerce (nreverse records) 'vector)
            (nreverse ids)
            coverage)))

(defun %conversation-episode-copy-array (payload key)
  (let ((value (gethash key payload)))
    (if (vectorp value) (copy-seq value) (vector))))

(defun conversation-episode-sealed-payload-valid-p (payload)
  "Validate the closed durable contract consumed by the pure projector."
  (and (hash-table-p payload)
       (= 1 (gethash "schema_version" payload -1))
       (stringp (gethash "episode_id" payload))
       (plusp (length (gethash "episode_id" payload)))
       (stringp (gethash "persona_id" payload))
       (plusp (length (gethash "persona_id" payload)))
       (integerp (gethash "first_event_id" payload))
       (integerp (gethash "last_event_id" payload))
       (<= (gethash "first_event_id" payload)
           (gethash "last_event_id" payload))
       (integerp (gethash "first_timestamp" payload))
       (integerp (gethash "last_timestamp" payload))
       (<= (gethash "first_timestamp" payload)
           (gethash "last_timestamp" payload))
       (let ((ids (gethash "source_event_ids" payload)))
         (and (vectorp ids) (plusp (length ids))
              (= (length ids)
                 (length (remove-duplicates (coerce ids 'list)
                                            :test #'equal)))
              (equal (aref ids 0) (gethash "first_event_id" payload))
              (equal (aref ids (1- (length ids)))
                     (gethash "last_event_id" payload))))
       (let ((synopsis (gethash "synopsis" payload)))
         (and (stringp synopsis) (plusp (length synopsis))
              (<= (length synopsis) 2400)))
       (every
        (lambda (key)
          (let ((values (gethash key payload)))
            (and (vectorp values) (<= (length values) 16)
                 (every (lambda (value)
                          (and (stringp value) (plusp (length value))
                               (<= (length value) 240)))
                        values))))
        *conversation-episode-semantic-fields*)))

(defun conversation-episode-project (events agent-id persona-id)
  "Project latest non-superseded sealed episodes from append-only events."
  (let ((by-id (make-hash-table :test #'equal)))
    (dolist (event (%conversation-episode-items events))
      (let ((payload (%conversation-episode-payload event)))
        (when (and (conversation-episode-sealed-payload-valid-p payload)
                   (equal agent-id (gethash "agent_id" event))
                   (string= "conversation-episode-sealed"
                            (gethash "type" event ""))
                   (string= persona-id (gethash "persona_id" payload "")))
          (let ((episode
                  (obj "episode_id" (gethash "episode_id" payload)
                       "event_id" (gethash "id" event)
                       "persona_id" persona-id
                       "first_event_id" (gethash "first_event_id" payload)
                       "last_event_id" (gethash "last_event_id" payload)
                       "first_timestamp" (gethash "first_timestamp" payload)
                       "last_timestamp" (gethash "last_timestamp" payload)
                       "source_event_ids"
                       (%conversation-episode-copy-array
                        payload "source_event_ids")
                       "synopsis" (gethash "synopsis" payload ""))))
            (dolist (key *conversation-episode-semantic-fields*)
              (setf (gethash key episode)
                    (%conversation-episode-copy-array payload key)))
            (setf (gethash (gethash "episode_id" payload) by-id) episode)))))
    (let ((episodes nil))
      (maphash (lambda (ignored episode)
                 (declare (ignore ignored))
                 (push episode episodes))
               by-id)
      (coerce (sort episodes #'< :key
                    (lambda (episode) (gethash "last_event_id" episode 0)))
              'vector))))

(defun %conversation-episode-terms (text)
  (let ((terms nil) (characters nil))
    (labels ((flush ()
               (when characters
                 (let ((term (string-downcase
                              (coerce (nreverse characters) 'string))))
                   (when (>= (length term) 3) (push term terms)))
                 (setf characters nil))))
      (loop for character across (if (stringp text) text "")
            do (if (alphanumericp character)
                   (push character characters)
                   (flush)))
      (flush))
    (remove-duplicates (nreverse terms) :test #'string=)))

(defun %conversation-episode-concepts (episode)
  (remove-duplicates
   (loop for key in *conversation-episode-semantic-fields*
         append
         (loop for value in (%conversation-episode-items
                             (gethash key episode))
               when (and (stringp value) (plusp (length value)))
                 collect (string-downcase value)))
   :test #'string=))

(defun conversation-episode-graph-project (episodes)
  "Build a derived bipartite episode/concept graph with exact provenance."
  (let ((concepts (make-hash-table :test #'equal))
        (episode-concepts (make-hash-table :test #'equal)))
    (dolist (episode (%conversation-episode-items episodes))
      (let ((id (gethash "episode_id" episode))
            (values (%conversation-episode-concepts episode)))
        (setf (gethash id episode-concepts) values)
        (dolist (concept values)
          (pushnew id (gethash concept concepts) :test #'string=))))
    (obj "schema_version" 1
         "projection_revision" *conversation-episode-projection-revision*
         "concepts" concepts "episode_concepts" episode-concepts)))

(defparameter *conversation-episode-graph-storage-projection-name*
  "conversation-episode-graph")

(defun %conversation-episode-graph-sha256 (text)
  (string-downcase
   (ironclad:byte-array-to-hex-string
    (ironclad:digest-sequence
     :sha256 (sb-ext:string-to-octets text :external-format :utf-8)))))

(defun %conversation-episode-graph-id (kind canonical-key)
  (format nil "kg:~a:~a" kind
          (%conversation-episode-graph-sha256
           (format nil "~a|~a|~a"
                   *conversation-episode-projection-revision*
                   kind canonical-key))))

(defun %conversation-episode-graph-integrity (&rest fields)
  (%conversation-episode-graph-sha256
   (with-output-to-string (stream)
     (dolist (field fields)
       (let ((text (princ-to-string field)))
         (format stream "~d:~a" (length text) text))))))

(defun %conversation-episode-concept-fields (episode)
  "Return normalized concept -> ordered semantic-field names for EPISODE."
  (let ((result (make-hash-table :test #'equal)))
    (dolist (key *conversation-episode-semantic-fields*)
      (dolist (value (%conversation-episode-items (gethash key episode)))
        (when (and (stringp value) (plusp (length value)))
          (let ((concept (string-downcase value)))
            (unless (member key (gethash concept result) :test #'string=)
              (setf (gethash concept result)
                    (append (gethash concept result) (list key))))))))
    result))

(defun %conversation-episode-graph-row
    (agent-id persona-id node-id node-kind canonical-key payload)
  (let* ((payload-json (shasht:write-json payload nil))
         (integrity
           (%conversation-episode-graph-integrity
            *conversation-episode-graph-storage-projection-name*
            agent-id persona-id node-id node-kind canonical-key payload-json)))
    (obj "projection_name"
         *conversation-episode-graph-storage-projection-name*
         "agent_id" agent-id "persona_id" persona-id
         "node_id" node-id "node_kind" node-kind
         "canonical_key" canonical-key "payload_json" payload-json
         "integrity_hash" integrity)))

(defun %conversation-episode-graph-edge-row
    (agent-id persona-id edge-id from-id to-id fields)
  (let* ((payload (obj "semantic_fields" (coerce fields 'vector)))
         (payload-json (shasht:write-json payload nil))
         (integrity
           (%conversation-episode-graph-integrity
            *conversation-episode-graph-storage-projection-name*
            agent-id persona-id edge-id from-id "has-concept" to-id
            payload-json)))
    (obj "projection_name"
         *conversation-episode-graph-storage-projection-name*
         "agent_id" agent-id "persona_id" persona-id
         "edge_id" edge-id "from_node_id" from-id
         "predicate" "has-concept" "to_node_id" to-id
         "payload_json" payload-json "integrity_hash" integrity)))

(defun %conversation-episode-graph-evidence-row
    (agent-id persona-id owner-kind owner-id event-id role ordinal)
  (obj "projection_name"
       *conversation-episode-graph-storage-projection-name*
       "agent_id" agent-id "persona_id" persona-id
       "owner_kind" owner-kind "owner_id" owner-id
       "evidence_event_id" event-id "evidence_role" role
       "evidence_ordinal" ordinal))

(defun conversation-episode-graph-materialization
    (episodes agent-id persona-id)
  "Canonical generic graph rows for the qualified episode/concept projection.

This pure function performs no database or event writes.  It is the shared
semantic boundary for cold rebuild, warm-restore equivalence and incremental
tail persistence."
  (unless (and (stringp agent-id) (plusp (length agent-id))
               (stringp persona-id) (plusp (length persona-id)))
    (error "Conversation graph materialization partition is invalid"))
  (let ((nodes nil) (edges nil) (evidence nil)
        (concepts (make-hash-table :test #'equal)))
    (dolist (episode (%conversation-episode-items episodes))
      (unless (and (hash-table-p episode)
                   (string= persona-id (gethash "persona_id" episode "")))
        (error "Conversation graph episode partition is invalid"))
      (let* ((episode-id (gethash "episode_id" episode))
             (episode-node-id
               (%conversation-episode-graph-id "episode" episode-id))
             (descriptor-id (gethash "event_id" episode))
             (source-ids (gethash "source_event_ids" episode))
             (fields (%conversation-episode-concept-fields episode)))
        (push (%conversation-episode-graph-row
               agent-id persona-id episode-node-id "episode" episode-id
               episode)
              nodes)
        (push (%conversation-episode-graph-evidence-row
               agent-id persona-id "node" episode-node-id descriptor-id
               "descriptor" 0)
              evidence)
        (loop for source-id across source-ids for ordinal from 0
              do (push (%conversation-episode-graph-evidence-row
                        agent-id persona-id "node" episode-node-id source-id
                        "source" ordinal)
                       evidence))
        (maphash
         (lambda (concept semantic-fields)
           (let* ((concept-id
                    (%conversation-episode-graph-id "concept" concept))
                  (edge-id
                    (%conversation-episode-graph-id
                     "edge"
                     (format nil "~a|has-concept|~a"
                             episode-node-id concept-id))))
             (setf (gethash concept concepts) concept-id)
             (push (%conversation-episode-graph-edge-row
                    agent-id persona-id edge-id episode-node-id concept-id
                    semantic-fields)
                   edges)
             (push (%conversation-episode-graph-evidence-row
                    agent-id persona-id "edge" edge-id descriptor-id
                    "descriptor" 0)
                   evidence)
             (loop for source-id across source-ids for ordinal from 0
                   do (push (%conversation-episode-graph-evidence-row
                             agent-id persona-id "edge" edge-id source-id
                             "source" ordinal)
                            evidence))))
         fields)))
    (maphash
     (lambda (concept concept-id)
         (push (%conversation-episode-graph-row
                agent-id persona-id concept-id "concept" concept
                (obj "label" concept))
               nodes))
     concepts)
    (labels ((row-key (row)
               (format nil "~a|~a|~20,'0d|~a|~20,'0d"
                       (gethash "owner_kind" row "")
                       (gethash "owner_id" row "")
                       (gethash "evidence_event_id" row 0)
                       (gethash "evidence_role" row "")
                       (gethash "evidence_ordinal" row 0))))
      (obj "schema_version" 1
           "projection_name"
           *conversation-episode-graph-storage-projection-name*
           "projection_revision" *conversation-episode-projection-revision*
           "agent_id" agent-id "persona_id" persona-id
           "nodes"
           (coerce (sort nodes #'string<
                         :key (lambda (row) (gethash "node_id" row)))
                   'vector)
           "edges"
           (coerce (sort edges #'string<
                         :key (lambda (row) (gethash "edge_id" row)))
                   'vector)
           "evidence"
           (coerce (sort evidence #'string<
                         :key #'row-key)
                   'vector)))))

(defun %conversation-episode-direct-score (cue episode)
  (let* ((lower (string-downcase (or cue "")))
         (cue-terms (%conversation-episode-terms lower))
         (concepts (%conversation-episode-concepts episode))
         (text (format nil "~a ~{~a~^ ~}" (gethash "synopsis" episode "")
                       concepts))
         (text-terms (%conversation-episode-terms text))
         (overlap (count-if (lambda (term)
                              (member term text-terms :test #'string=))
                            cue-terms))
         (phrase (count-if (lambda (concept) (search concept lower)) concepts)))
    (+ (* 4 phrase) overlap)))

(defun conversation-episode-recall
    (episodes cue &key (maximum 4) semantic-score-fn)
  "Select episodes by lexical/category evidence plus bounded graph expansion."
  (unless (and (stringp cue) (plusp (length cue))
               (integerp maximum) (<= 1 maximum 12))
    (error "Conversation episode recall arguments are invalid"))
  (let* ((items (%conversation-episode-items episodes))
         (graph (conversation-episode-graph-project items))
         (concept-map (gethash "concepts" graph))
         (direct (make-hash-table :test #'equal))
         (scores (make-hash-table :test #'equal))
         (semantic (or semantic-score-fn
                       *conversation-episode-semantic-score-fn*)))
    (dolist (episode items)
      (let* ((id (gethash "episode_id" episode))
             (lexical (%conversation-episode-direct-score cue episode))
             (semantic-score
               (if (functionp semantic)
                   (max 0d0 (min 1d0 (coerce (funcall semantic cue episode)
                                             'double-float)))
                   0d0)))
        (setf (gethash id direct) lexical
              (gethash id scores) (+ lexical (* 3d0 semantic-score)))))
    ;; One bounded hop: a directly matching episode lends a modest boost to
    ;; episodes sharing an explicit model-produced concept.  It can reorder
    ;; supported candidates but cannot make an unrelated zero-evidence row win.
    (maphash
     (lambda (concept ids)
       (declare (ignore concept))
       (let ((seed (reduce #'max ids :key (lambda (id) (gethash id direct 0))
                           :initial-value 0)))
         (when (plusp seed)
           (dolist (id ids)
             (incf (gethash id scores 0) (min 1d0 (* 0.25d0 seed)))))))
     concept-map)
    (let ((ranked
            (sort (remove-if
                   (lambda (episode)
                     (not (plusp (gethash (gethash "episode_id" episode)
                                          scores 0))))
                   (copy-list items))
                  (lambda (left right)
                    (let ((ls (gethash (gethash "episode_id" left) scores 0))
                          (rs (gethash (gethash "episode_id" right) scores 0)))
                      (if (= ls rs)
                          (> (gethash "last_timestamp" left 0)
                             (gethash "last_timestamp" right 0))
                          (> ls rs)))))))
      (coerce (subseq ranked 0 (min maximum (length ranked))) 'vector))))

(defun conversation-episode-context-records
    (episodes cue &key (maximum 4) (character-budget 4000)
                       (record-character-limit 1800) semantic-score-fn)
  "Render selected episodic recollections under a final-consumer budget."
  (unless (and (stringp cue) (plusp (length cue))
               (integerp maximum) (<= 1 maximum 12)
               (integerp character-budget) (plusp character-budget)
               (integerp record-character-limit) (plusp record-character-limit))
    (error "Conversation episode context arguments are invalid"))
  (let ((records nil) (ids nil) (selected-episodes nil) (used 0)
        (budget-refusals 0))
    (dolist (episode (%conversation-episode-items
                      (conversation-episode-recall
                       episodes cue :maximum 12
                       :semantic-score-fn semantic-score-fn)))
      (when (>= (length records) maximum)
        (return))
      (let* ((prefix
               (format nil
                       "Selected, non-exhaustive recollection of prior conversation episode ~a, grounded in exact source events ~{~a~^, ~}; absence here does not establish that an event did not occur. It is firsthand only for participating persona ~a: "
                       (gethash "episode_id" episode)
                       (%conversation-episode-items
                        (gethash "source_event_ids" episode))
                       (gethash "persona_id" episode)))
             (synopsis (gethash "synopsis" episode ""))
             (content (concatenate 'string prefix synopsis))
             (size (length content)))
        (if (and (<= size record-character-limit)
                 (<= (+ used size) character-budget))
            (let ((source-id
                    (format nil "conversation-episode:~a"
                            (gethash "event_id" episode))))
              (push (obj "source_id" source-id "content" content
                         "provenance"
                         (obj "descriptor_id" (gethash "episode_id" episode)
                              "descriptor_event_id"
                              (gethash "event_id" episode)
                              "evidence_event_ids"
                              (concatenate
                               'vector (vector (gethash "event_id" episode))
                               (copy-seq
                                (gethash "source_event_ids" episode)))))
                    records)
              (push source-id ids)
              (push episode selected-episodes)
              (incf used size))
            (incf budget-refusals))))
    (let ((ordered-records (nreverse records))
          (ordered-ids (nreverse ids)))
      (values (coerce ordered-records 'vector)
              ordered-ids
              (obj "schema_version" 1
                   "status" (if ordered-records "selected" "empty")
                   "selected_count" (length ordered-records)
                   "selected_ids" (coerce (copy-list ordered-ids) 'vector)
                   "rendered_characters" used
                   "budget_refusal_count" budget-refusals)
              (coerce (nreverse selected-episodes) 'vector)))))
