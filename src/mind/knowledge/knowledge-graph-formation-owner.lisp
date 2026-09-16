;;;; knowledge-graph-formation-owner.lisp -- bounded KG2 semantic boundary.
;;;;
;;;; This owner is port-composed so source qualification needs neither a live
;;;; provider nor a live ledger.  The model proposes semantics only; runtime
;;;; ports own eligibility, durable receipts and derived synchronization.

(in-package :agent)

(export '(knowledge-graph-formation-source-packet-valid-p
          knowledge-graph-formation-owner-step))

(defparameter *knowledge-graph-formation-owner-revision*
  "grounded-knowledge-graph-owner-v5")

(defun %kgfo-exact-keys-p (object keys)
  (and (hash-table-p object)
       (= (hash-table-count object) (length keys))
       (every (lambda (key) (nth-value 1 (gethash key object))) keys)))

(defun %kgfo-evidence-record-valid-p (record)
  (and (%kgfo-exact-keys-p
        record '("source_id" "speaker_id" "kind" "timestamp" "text"
                 "text_sha256"))
       (%kgf-required-string-p (gethash "source_id" record) 180)
       (%kgf-required-string-p (gethash "speaker_id" record) 180)
       (%kgf-token-p (gethash "kind" record))
       (or (integerp (gethash "timestamp" record))
           (%kgf-required-string-p (gethash "timestamp" record) 80))
       (%kgf-required-string-p (gethash "text" record) 30000)
       (let ((digest (gethash "text_sha256" record)))
         (and (stringp digest) (= 64 (length digest))
              (string= digest (%kgf-sha256 (gethash "text" record)))))))

(defun %kgfo-candidate-valid-p (candidate)
  (and (%kgfo-exact-keys-p
        candidate '("node_id" "kind" "label" "aliases"
                    "classifications" "participant_role"))
       (%kgf-required-string-p (gethash "node_id" candidate) 180)
       (%kgf-token-p (gethash "kind" candidate))
       (%kgf-required-string-p (gethash "label" candidate) 240)
       (%kgf-distinct-vector-p
        (gethash "aliases" candidate)
        (lambda (value) (%kgf-required-string-p value 240)) :maximum 8)
       (%kgf-distinct-vector-p
        (gethash "classifications" candidate)
        (lambda (value) (%kgf-required-string-p value 120)) :maximum 8)
       (let ((role (gethash "participant_role" candidate)))
         (or (%kgf-null-p role)
             (member role '("operator" "active-persona") :test #'string=)))))

(defun knowledge-graph-formation-source-packet-valid-p (packet)
  (and
   (%kgfo-exact-keys-p
    packet '("schema_version" "source_event_ids" "source_memory_node_ids"
             "source_episode_ids" "disclosure_class" "evidence_records"
             "eligible_existing_nodes"))
   (eql 1 (gethash "schema_version" packet))
   (%kgf-distinct-vector-p (gethash "source_event_ids" packet)
                           (lambda (value)
                             (and (integerp value) (plusp value)))
                           ;; One sealed episode root plus every retained
                           ;; utterance in the historical evidence envelope.
                           :maximum
                           (1+ *knowledge-graph-formation-maximum-source-evidence-records*))
   (plusp (length (gethash "source_event_ids" packet)))
   (%kgf-distinct-vector-p (gethash "source_memory_node_ids" packet)
                           (lambda (value)
                             (%kgf-required-string-p value 180)))
   (%kgf-distinct-vector-p (gethash "source_episode_ids" packet)
                           (lambda (value)
                             (%kgf-required-string-p value 180)))
   (member (gethash "disclosure_class" packet)
           '("private" "personal-shareable" "public") :test #'string=)
   ;; Preserve complete exact evidence for historical episodes formed under
   ;; earlier, wider sealing policies. The independent character ceiling keeps
   ;; this migration compatibility bounded.
   (%kgf-distinct-vector-p (gethash "evidence_records" packet)
                           #'%kgfo-evidence-record-valid-p
                           :maximum
                           *knowledge-graph-formation-maximum-source-evidence-records*)
   (plusp (length (gethash "evidence_records" packet)))
   (<= (loop for row across (gethash "evidence_records" packet)
             sum (length (gethash "text" row "")))
       *knowledge-graph-formation-maximum-source-evidence-characters*)
   (%kgf-distinct-vector-p (gethash "eligible_existing_nodes" packet)
                           #'%kgfo-candidate-valid-p :maximum 64)
   (= (length (gethash "eligible_existing_nodes" packet))
      (length
       (remove-duplicates
        (map 'list (lambda (row) (gethash "node_id" row))
             (gethash "eligible_existing_nodes" packet))
        :test #'string=)))))

(defun %kgfo-source-packet-diagnostic (packet)
  "Return content-free shape evidence for a rejected source packet."
  (let* ((evidence (and (hash-table-p packet)
                        (gethash "evidence_records" packet)))
         (candidates (and (hash-table-p packet)
                          (gethash "eligible_existing_nodes" packet))))
    (format nil
            "keys=~a source-events=~d evidence=~d valid-evidence=~a evidence-chars=~d candidates=~d valid-candidates=~a distinct-candidates=~a"
            (and (hash-table-p packet)
                 (%kgfo-exact-keys-p
                  packet '("schema_version" "source_event_ids"
                           "source_memory_node_ids" "source_episode_ids"
                           "disclosure_class" "evidence_records"
                           "eligible_existing_nodes")))
            (let ((source-events (and (hash-table-p packet)
                                      (gethash "source_event_ids" packet))))
              (if (vectorp source-events) (length source-events) -1))
            (if (vectorp evidence) (length evidence) -1)
            (and (vectorp evidence)
                 (every #'%kgfo-evidence-record-valid-p evidence))
            (if (vectorp evidence)
                (loop for row across evidence
                      sum (if (hash-table-p row)
                              (length (gethash "text" row "")) 0))
                -1)
            (if (vectorp candidates) (length candidates) -1)
            (and (vectorp candidates)
                 (every #'%kgfo-candidate-valid-p candidates))
            (and (vectorp candidates)
                 (= (length candidates)
                    (length
                     (remove-duplicates
                      (map 'list (lambda (row)
                                   (and (hash-table-p row)
                                        (gethash "node_id" row)))
                           candidates)
                      :test #'string=)))))))

(defun %kgfo-event-payload (event)
  (and (hash-table-p event) (gethash "payload" event)))

(defun %kgfo-terminal-p (event opened-id)
  (and (hash-table-p event)
       (equal opened-id (gethash "caused_by" event))
       (member (gethash "type" event "")
               '("knowledge-graph-formation-sealed"
                 "knowledge-graph-formation-failed")
               :test #'string=)))

(defun %kgfo-pending-open (events agent-id persona-id)
  (find-if
   (lambda (event)
     (let ((payload (%kgfo-event-payload event)))
       (and (hash-table-p payload)
            (string= "knowledge-graph-formation-opened"
                     (gethash "type" event ""))
            (string= agent-id (gethash "agent_id" event ""))
            (string= persona-id (gethash "persona_id" payload ""))
            (string= *knowledge-graph-formation-revision*
                     (gethash "formation_revision" payload ""))
            (string= *knowledge-graph-formation-owner-revision*
                     (gethash "owner_revision" payload ""))
            (notany (lambda (candidate)
                      (%kgfo-terminal-p candidate (gethash "id" event)))
                    events))))
   events :from-end t))

(defun %kgfo-open-payload (packet persona-id)
  (obj "schema_version" 1 "persona_id" persona-id
       "disclosure_class" (gethash "disclosure_class" packet)
       "formation_revision" *knowledge-graph-formation-revision*
       "owner_revision" *knowledge-graph-formation-owner-revision*
       "source_event_ids" (copy-seq (gethash "source_event_ids" packet))
       "source_memory_node_ids"
       (copy-seq (gethash "source_memory_node_ids" packet))
       "source_episode_ids"
       (copy-seq (gethash "source_episode_ids" packet))
       "evidence_records" (copy-seq (gethash "evidence_records" packet))
       "eligible_existing_nodes"
       (copy-seq (gethash "eligible_existing_nodes" packet))
       "opened_at" (get-universal-time)))

(defun %kgfo-sealed-payload (opened proposal)
  (let* ((source (%kgfo-event-payload opened))
         (candidates (gethash "eligible_existing_nodes" source))
         (payload
           (obj "schema_version" 1
                "persona_id" (gethash "persona_id" source)
                "disclosure_class" (gethash "disclosure_class" source)
                "formation_revision" *knowledge-graph-formation-revision*
                "source_event_ids"
                (copy-seq (gethash "source_event_ids" source))
                "source_memory_node_ids"
                (copy-seq (gethash "source_memory_node_ids" source))
                "source_episode_ids"
                (copy-seq (gethash "source_episode_ids" source))
                "source_evidence"
                (copy-seq (gethash "evidence_records" source))
                "eligible_existing_node_ids"
                (map 'vector (lambda (row) (gethash "node_id" row)) candidates)
                "proposal" proposal)))
    (unless (knowledge-graph-formation-sealed-payload-valid-p payload)
      (error "Provider proposal is outside the KG2 sealed contract"))
    payload))

(defun %kgfo-append (append-event-fn type payload caused-by)
  (let ((event (funcall append-event-fn type payload caused-by)))
    (unless (and (hash-table-p event)
                 (integerp (gethash "id" event))
                 (string= type (gethash "type" event "")))
      (error "KG2 append port returned an invalid event"))
    event))

(defun knowledge-graph-formation-owner-step
    (events agent-id persona-id source-selector-fn provider-fn append-event-fn
     synchronize-fn &key operator-pending-p budget-admissible-p)
  "Perform at most one provider-backed KG2 formation boundary.

PROVIDER-FN receives the durable opened payload and opened event ID, and returns
only the semantic proposal (or :PREEMPTED/:PAUSED-BUDGET). APPEND-EVENT-FN receives
(type payload caused-by). A sealed authority event remains successful even when
derived synchronization is temporarily unavailable."
  (unless (and (stringp agent-id) (plusp (length agent-id))
               (stringp persona-id) (plusp (length persona-id))
               (every #'functionp
                      (list source-selector-fn provider-fn append-event-fn
                            synchronize-fn)))
    (error "KG2 formation owner ports are invalid"))
  (when (and operator-pending-p (funcall operator-pending-p))
    (return-from knowledge-graph-formation-owner-step
      (obj "schema_version" 1 "status" "preempted")))
  (when (and budget-admissible-p (not (funcall budget-admissible-p)))
    (return-from knowledge-graph-formation-owner-step
      (obj "schema_version" 1 "status" "paused-budget")))
  (let ((opened (%kgfo-pending-open events agent-id persona-id)))
    (unless opened
      (let ((packet (funcall source-selector-fn events agent-id persona-id)))
        (when (null packet)
          (return-from knowledge-graph-formation-owner-step
            (obj "schema_version" 1 "status" "idle")))
        (unless (knowledge-graph-formation-source-packet-valid-p packet)
          (error "KG2 source selector returned an invalid packet"))
        (setf opened
              (%kgfo-append append-event-fn
                            "knowledge-graph-formation-opened"
                            (%kgfo-open-payload packet persona-id)
                            (aref (gethash "source_event_ids" packet) 0)))))
    (when (and operator-pending-p (funcall operator-pending-p))
      (return-from knowledge-graph-formation-owner-step
        (obj "schema_version" 1 "status" "preempted"
             "opened_event_id" (gethash "id" opened))))
    (let ((outcome
            (handler-case (funcall provider-fn (%kgfo-event-payload opened)
                                   (gethash "id" opened))
              (error (condition)
                (list :failed
                      (string-downcase (symbol-name (type-of condition))))))))
      (when (eq outcome :preempted)
        (return-from knowledge-graph-formation-owner-step
          (obj "schema_version" 1 "status" "preempted"
               "opened_event_id" (gethash "id" opened))))
      (when (eq outcome :paused-budget)
        (return-from knowledge-graph-formation-owner-step
          (obj "schema_version" 1 "status" "paused-budget"
               "opened_event_id" (gethash "id" opened))))
      (when (and (consp outcome) (eq :failed (first outcome)))
        (let ((failure
                (%kgfo-append
                 append-event-fn "knowledge-graph-formation-failed"
                 (obj "schema_version" 1
                      "persona_id" persona-id
                      "owner_revision"
                      *knowledge-graph-formation-owner-revision*
                      "reason" (second outcome)
                      "failed_at" (get-universal-time))
                 (gethash "id" opened))))
          (return-from knowledge-graph-formation-owner-step
            (obj "schema_version" 1 "status" "failed"
                 "opened_event_id" (gethash "id" opened)
                 "failure_event_id" (gethash "id" failure)))))
      (handler-case
          (let* ((sealed-payload (%kgfo-sealed-payload opened outcome))
                 (sealed (%kgfo-append
                          append-event-fn "knowledge-graph-formation-sealed"
                          sealed-payload (gethash "id" opened)))
                 (sync-report
                   (handler-case (funcall synchronize-fn)
                     (error (condition)
                       (obj "schema_version" 1 "status" "unavailable"
                            "reason"
                            (string-downcase
                             (symbol-name (type-of condition))))))))
            (obj "schema_version" 1 "status" "sealed"
                 "opened_event_id" (gethash "id" opened)
                 "sealed_event_id" (gethash "id" sealed)
                 "synchronization" sync-report))
        (error (condition)
          (let ((failure
                  (%kgfo-append
                   append-event-fn "knowledge-graph-formation-failed"
                   (obj "schema_version" 1 "persona_id" persona-id
                        "owner_revision"
                        *knowledge-graph-formation-owner-revision*
                        "reason"
                        (string-downcase (symbol-name (type-of condition)))
                        "failed_at" (get-universal-time))
                   (gethash "id" opened))))
            (obj "schema_version" 1 "status" "failed"
                 "opened_event_id" (gethash "id" opened)
                 "failure_event_id" (gethash "id" failure))))))))
