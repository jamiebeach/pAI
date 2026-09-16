;;;; proposal.lisp -- Q4 pure captured-proposal validation.

(in-package :agent)

(export '(conscious-proposals-validate conscious-proposals-report
          *conscious-proposal-kinds*))

(defparameter *conscious-proposal-schema-version* 1)
(defparameter *conscious-proposal-max-count* 8)
(defparameter *conscious-proposal-max-payload-chars* 8192)
(defparameter *conscious-proposal-kinds*
  '("state-update" "memory-admission-proposal" "tool-call-proposal"
    "publication-candidate" "schedule-wake" "request-continuation"
    "self-mod-proposal" "yield" "abstain"))

(defun %proposal-list (value label &optional (maximum 64))
  (let ((items (cond ((vectorp value) (coerce value 'list))
                     ((listp value) value)
                     (t (error "~a must be an array" label)))))
    (unless (<= (length items) maximum)
      (error "~a exceeds its item bound" label))
    items))

(defun %proposal-exact-keys (table allowed label)
  (unless (hash-table-p table) (error "~a must be an object" label))
  (loop for key being the hash-keys of table
        unless (member key allowed :test #'string=)
          do (error "Unknown ~a key ~s" label key))
  (dolist (key allowed)
    (unless (nth-value 1 (gethash key table))
      (error "Missing ~a key ~s" label key)))
  table)

(defun %proposal-data-only-p (value &optional (active (make-hash-table :test #'eq)))
  (cond
    ((or (null value) (eq value t) (stringp value) (keywordp value)
         (and (realp value) (or (not (floatp value)) (= value value)))) t)
    ((or (functionp value) (streamp value) (pathnamep value)) nil)
    ((hash-table-p value)
     (if (gethash value active) nil
         (progn
           (setf (gethash value active) t)
           (prog1
               (loop for key being the hash-keys of value using (hash-value item)
                     always (and (stringp key)
                                 (%proposal-data-only-p item active)))
             (remhash value active)))))
    ((vectorp value)
     (and (<= (length value) 1024)
          (not (gethash value active))
          (progn
            (setf (gethash value active) t)
            (prog1 (loop for item across value
                         always (%proposal-data-only-p item active))
              (remhash value active)))))
    ((listp value)
     (let ((length (ignore-errors (list-length value))))
       (and length (<= length 1024) (not (gethash value active))
            (progn
              (setf (gethash value active) t)
              (prog1 (every (lambda (item)
                              (%proposal-data-only-p item active)) value)
                (remhash value active))))))
    (t nil)))

(defun %proposal-nested-shape-p (value)
  (cond
    ((hash-table-p value)
     (or (loop for key being the hash-keys of value
               thereis (member (string-downcase key)
                               '("proposal" "proposals") :test #'string=))
         (loop for item being the hash-values of value
               thereis (%proposal-nested-shape-p item))))
    ((vectorp value) (loop for item across value
                           thereis (%proposal-nested-shape-p item)))
    ((listp value) (some #'%proposal-nested-shape-p value))
    (t nil)))

(defun %proposal-string (value label maximum)
  (unless (and (stringp value) (plusp (length value))
               (<= (length value) maximum))
    (error "~a must be non-empty bounded text" label))
  value)

(defun %proposal-manifest-list (manifest key)
  (%proposal-list (gethash key manifest) key 256))

(defun %proposal-evidence (value manifest)
  (let ((ids (%proposal-list value "proposal evidence" 32))
        (permitted (%proposal-manifest-list manifest "evidence_event_ids")))
    (unless (= (length ids) (length (remove-duplicates ids :test #'equal)))
      (error "Proposal evidence IDs must be unique"))
    (unless (every (lambda (id) (member id permitted :test #'equal)) ids)
      (error "Proposal cites evidence outside the context manifest"))
    ids))

(defun %proposal-empty-payload (payload kind)
  (%proposal-exact-keys payload '() (format nil "~a payload" kind)))

(defun %proposal-validate-payload (kind payload manifest evidence)
  (unless (and (%proposal-data-only-p payload)
               (<= (length (shasht:write-json payload nil))
                   *conscious-proposal-max-payload-chars*))
    (error "Proposal payload is not bounded data"))
  (when (%proposal-nested-shape-p payload)
    (error "Proposal payload contains a nested proposal shape"))
  (cond
    ((member kind '("yield" "abstain") :test #'string=)
     (%proposal-empty-payload payload kind))
    ((string= kind "tool-call-proposal")
     (%proposal-exact-keys payload '("tool_name" "arguments") "tool payload")
     (let ((name (%proposal-string (gethash "tool_name" payload)
                                   "Tool name" 128)))
       (unless (member name (%proposal-manifest-list manifest "available_tools")
                       :test #'string=)
         (error "Proposal requests an unadvertised tool")))
     (unless (hash-table-p (gethash "arguments" payload))
       (error "Tool arguments must remain an inert object")))
    ((string= kind "publication-candidate")
     (%proposal-exact-keys
      payload '("audience" "channel_class" "speech_act" "content"
                "evidence_event_ids" "reason_to_speak_now")
      "publication payload")
     (unless (equal (gethash "audience" payload) (gethash "audience" manifest))
       (error "Publication audience is not permitted by the manifest"))
     (%proposal-string (gethash "channel_class" payload) "Channel class" 80)
     (%proposal-string (gethash "speech_act" payload) "Speech act" 80)
     (%proposal-string (gethash "content" payload) "Publication content" 4000)
     (%proposal-string (gethash "reason_to_speak_now" payload)
                       "Reason to speak" 240)
     (unless (equalp (coerce evidence 'vector)
                     (coerce (%proposal-evidence
                              (gethash "evidence_event_ids" payload) manifest)
                             'vector))
       (error "Publication evidence must match its proposal envelope")))
    ((string= kind "request-continuation")
     (%proposal-exact-keys payload '("purpose") "continuation payload")
     (%proposal-string (gethash "purpose" payload) "Continuation purpose" 80))
    ((string= kind "state-update")
     (%proposal-exact-keys payload '("operation" "target_ref" "value_ref")
                           "state-update payload")
     (%proposal-string (gethash "operation" payload) "State operation" 80)
     (%proposal-string (gethash "target_ref" payload) "State target" 256)
     (%proposal-string (gethash "value_ref" payload) "State value reference" 256))
    ((string= kind "memory-admission-proposal")
     (%proposal-exact-keys payload '("content" "evidence_event_ids" "origin_class")
                           "memory payload")
     (%proposal-string (gethash "content" payload) "Memory candidate" 4000)
     (%proposal-string (gethash "origin_class" payload) "Memory origin class" 80)
     (unless (equalp (coerce evidence 'vector)
                     (coerce (%proposal-evidence
                              (gethash "evidence_event_ids" payload) manifest)
                             'vector))
       (error "Memory evidence must match its proposal envelope")))
    ((string= kind "schedule-wake")
     (%proposal-exact-keys payload '("wake_at" "reason_code") "schedule payload")
     (%proposal-string (gethash "wake_at" payload) "Wake time" 128)
     (%proposal-string (gethash "reason_code" payload) "Wake reason" 80))
    ((string= kind "self-mod-proposal")
     (%proposal-exact-keys payload '("change_ref" "qualification_profile")
                           "self-mod payload")
     (%proposal-string (gethash "change_ref" payload) "Change reference" 256)
     (%proposal-string (gethash "qualification_profile" payload)
                       "Qualification profile" 128)))
  payload)

(defun %proposal-budget-use (kind counts)
  (cond ((string= kind "tool-call-proposal") (incf (gethash "tool_proposals" counts 0)))
        ((string= kind "request-continuation") (incf (gethash "continuations" counts 0)))
        ((string= kind "publication-candidate")
         (incf (gethash "publication_candidates" counts 0)))))

(defun conscious-proposals-validate (captured manifest)
  "Validate one complete captured response; return a detached inert copy."
  (%proposal-exact-keys captured '("schema_version" "proposals")
                        "captured response")
  (unless (= (gethash "schema_version" captured -1)
             *conscious-proposal-schema-version*)
    (error "Unsupported captured proposal schema"))
  (unless (hash-table-p manifest) (error "Proposal manifest must be an object"))
  (let ((rows (%proposal-list (gethash "proposals" captured) "proposals"
                              *conscious-proposal-max-count*))
        (seen (make-hash-table :test #'equal))
        (counts (make-hash-table :test #'equal)))
    (unless rows
      (error "Captured silence must be an explicit yield or abstain proposal"))
    (dolist (row rows)
      (%proposal-exact-keys
       row '("proposal_id" "pulse_id" "runtime_revision"
             "conscious_state_revision" "kind" "created_at_stage"
             "confidence" "evidence_event_ids" "payload") "proposal")
      (let* ((id (%proposal-string (gethash "proposal_id" row)
                                   "Proposal id" 160))
             (pulse-id (gethash "pulse_id" manifest))
             (id-prefix (format nil "~a:proposal:" pulse-id))
             (kind (gethash "kind" row))
             (confidence (gethash "confidence" row))
             (evidence (%proposal-evidence
                        (gethash "evidence_event_ids" row) manifest)))
        (when (gethash id seen) (error "Duplicate proposal id"))
        (let ((suffix
                (and (zerop (or (search id-prefix id :test #'char=) -1))
                     (ignore-errors
                       (parse-integer id :start (length id-prefix)
                                        :junk-allowed nil)))))
          (unless (and (integerp suffix) (plusp suffix))
            (error "Proposal id is not derived from the manifest pulse")))
        (setf (gethash id seen) t)
        (unless (and (stringp kind)
                     (member kind *conscious-proposal-kinds* :test #'string=))
          (error "Unknown proposal kind ~s" kind))
        (let ((permitted (gethash "permitted_proposal_kinds" manifest)))
          (when permitted
            (unless (member kind (%proposal-list permitted
                                                "permitted proposal kinds" 32)
                            :test #'string=)
              (error "Proposal kind is not permitted by the context manifest"))))
        (unless (and (equal (gethash "pulse_id" row)
                            (gethash "pulse_id" manifest))
                     (equal (gethash "runtime_revision" row)
                            (gethash "runtime_revision" manifest))
                     (eql (gethash "conscious_state_revision" row)
                          (gethash "conscious_state_revision" manifest)))
          (error "Proposal pulse/runtime/state identity mismatch"))
        (unless (string= "model-deliberation"
                         (gethash "created_at_stage" row ""))
          (error "Captured proposal has an invalid creation stage"))
        (unless (and (realp confidence) (<= 0 confidence 1))
          (error "Proposal confidence is invalid"))
        (%proposal-validate-payload kind (gethash "payload" row)
                                    manifest evidence)
        (%proposal-budget-use kind counts)))
    (let ((remaining (gethash "remaining_budget" manifest)))
      (unless (hash-table-p remaining) (error "Manifest budget is absent"))
      (maphash (lambda (key used)
                 (let ((available (gethash key remaining)))
                   (unless (and (integerp available) (>= available used))
                     (error "Proposal set exceeds remaining ~a budget" key))))
               counts))
    (shasht:read-json (shasht:write-json captured nil))))

(defun conscious-proposals-report (validated)
  (let ((rows (%proposal-list (gethash "proposals" validated) "proposals"
                              *conscious-proposal-max-count*)))
    (obj "schema_version" *conscious-proposal-schema-version*
         "proposal_count" (length rows)
         "kinds" (coerce (mapcar (lambda (row) (gethash "kind" row)) rows)
                         'vector)
         "proposal_ids"
         (coerce (mapcar (lambda (row) (gethash "proposal_id" row)) rows)
                 'vector))))
