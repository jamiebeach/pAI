;;;; knowledge-graph-formation-owner-tests.lisp -- stubbed KG2 owner boundary.

(in-package :agent)

(ql:quickload '(:shasht :ironclad) :silent t)
(load (test-source "knowledge-graph-ontology.lisp"))
(load (test-source "knowledge-graph-formation.lisp"))
(load (test-source "knowledge-graph-formation-owner.lisp"))

(defvar *kgfo-pass* 0)
(defvar *kgfo-fail* 0)

(defun kgfo-check (name condition)
  (if condition
      (progn (incf *kgfo-pass*) (format t "PASS ~a~%" name))
      (progn (incf *kgfo-fail*) (format t "FAIL ~a~%" name))))

(defun kgfo-packet ()
  (obj "schema_version" 1 "source_event_ids" #(10 11)
       "source_memory_node_ids" #("memory:10")
       "source_episode_ids" #("episode:10")
       "disclosure_class" "private"
       "evidence_records"
       (vector (obj "source_id" "event:10" "speaker_id" "operator"
                    "kind" "original-utterance" "timestamp" 4000000000
                    "text" "The operator requires accessible visuals."
                    "text_sha256"
                    (%kgf-sha256 "The operator requires accessible visuals.")))
       "eligible_existing_nodes"
       (vector (obj "node_id" "kgf:entity:existing" "kind" "person"
                    "label" "Operator" "aliases" #("FixtureOperator")
                    "classifications" #("operator")
                    "participant_role" "operator"))))

(defun kgfo-proposal ()
  (obj "schema_version" 3
       "ontology_revision" *knowledge-graph-ontology-revision*
       "entities"
       (vector
        (obj "local_ref" "operator" "kind" "person" "label" "Operator"
             "aliases" #("FixtureOperator") "classifications" #("operator")
             "identity_action" "LINK_EXISTING"
             "existing_node_id" "kgf:entity:existing"
             "evidence_status" "direct" "evidence_note" "fixture evidence")
        (obj "local_ref" "need" "kind" "concept"
             "label" "Accessible visuals" "aliases" #() "classifications" #()
             "identity_action" "NEW" "existing_node_id" :null
             "evidence_status" "direct" "evidence_note" "fixture evidence"))
       "relationships"
       (vector
        (obj "subject_ref" "operator" "predicate" "related_to"
             "object_ref" "need" "relationship_action" "ASSERT"
             "fact" "The operator requires accessible visuals."
             "grounding"
             (obj "schema_version" 1 "scope" "assertion"
                  "polarity" "positive" "attributed_to_ref" :null
                  "evidence"
                  (vector (obj "source_id" "event:10"
                               "quote" "The operator requires accessible visuals.")))
             "temporal"
             (obj "schema_version" 1 "character" "standing-disposition"
                  "occurred_at" :null "valid_from" :null "valid_until" :null)
             "evidence_status" "direct" "evidence_note" "fixture evidence"))))

(defun kgfo-harness (&key provider sync operator budget selector)
  (let ((events nil) (next-id 100) (provider-calls 0) (sync-calls 0))
    (labels ((append-event (type payload caused-by)
               (let ((event
                       (obj "id" (incf next-id) "type" type
                            "agent_id" "owner-agent"
                            "timestamp" (get-universal-time)
                            "caused_by" caused-by "payload" payload)))
                 (setf events (append events (list event)))
                 event))
             (source (prior agent persona)
               (if selector
                   (funcall selector prior agent persona)
                   (kgfo-packet)))
             (call-provider (request opened-id)
               (declare (ignore opened-id))
               (incf provider-calls)
               (if provider (funcall provider request) (kgfo-proposal)))
             (synchronize ()
               (incf sync-calls)
               (if sync (funcall sync)
                   (obj "schema_version" 1 "status" "synchronized"))))
      (values
       (lambda ()
         (knowledge-graph-formation-owner-step
          events "owner-agent" "owner-persona" #'source #'call-provider
          #'append-event #'synchronize
          :operator-pending-p operator :budget-admissible-p budget))
       (lambda () events)
       (lambda () provider-calls)
       (lambda () sync-calls)))))

(format t "~%== KG2 formation owner ==~%")

(let* ((packet (kgfo-packet))
       (template (aref (gethash "evidence_records" packet) 0))
       (rows
         (loop for index below 100
               collect
               (let* ((text (format nil "Grounded episode utterance ~d." index))
                      (row (%kgf-copy-object template)))
                 (setf (gethash "source_id" row) (format nil "event:~d" index)
                       (gethash "text" row) text
                       (gethash "text_sha256" row) (%kgf-sha256 text))
                 row))))
  (setf (gethash "evidence_records" packet) (coerce rows 'vector))
  (kgfo-check "historical 100-message episode remains bounded and complete"
              (knowledge-graph-formation-source-packet-valid-p packet)))

(let* ((packet (kgfo-packet))
       (text (make-string 24000 :initial-element #\x))
       (rows
         (loop for index below 3
               collect
               (obj "source_id" (format nil "event:oversize-~d" index)
                    "speaker_id" "operator" "kind" "original-utterance"
                    "timestamp" 4000000000 "text" text
                    "text_sha256" (%kgf-sha256 text)))))
  (setf (gethash "evidence_records" packet) (coerce rows 'vector))
  (kgfo-check "aggregate historical evidence character ceiling fails closed"
              (not (knowledge-graph-formation-source-packet-valid-p packet))))

(multiple-value-bind (step events provider-calls sync-calls) (kgfo-harness)
  (let* ((report (funcall step))
         (rows (funcall events))
         (opened (first rows))
         (sealed (second rows))
         (payload (gethash "payload" sealed)))
    (kgfo-check "one source packet opens seals and synchronizes once"
                (and (string= "sealed" (gethash "status" report))
                     (= 1 (funcall provider-calls))
                     (= 1 (funcall sync-calls))
                     (= 2 (length rows))
                     (string= "knowledge-graph-formation-opened"
                              (gethash "type" opened))
                     (string= "knowledge-graph-formation-sealed"
                              (gethash "type" sealed))))
    (kgfo-check "runtime copies authority metadata around semantic proposal"
                (and (equalp #(10 11) (gethash "source_event_ids" payload))
                     (string= "owner-persona" (gethash "persona_id" payload))
                     (equalp #( "kgf:entity:existing")
                             (gethash "eligible_existing_node_ids" payload))
                     (knowledge-graph-formation-sealed-payload-valid-p
                      payload)))))

(let ((pending-once t))
  (multiple-value-bind (step events provider-calls ignored-sync)
      (kgfo-harness
       :provider (lambda (request)
                   (declare (ignore request))
                   (if pending-once
                       (progn (setf pending-once nil) :preempted)
                       (kgfo-proposal))))
    (declare (ignore ignored-sync))
    (let ((first (funcall step)) (second (funcall step)))
      (kgfo-check "preemption preserves one durable open and resumes it"
                  (and (string= "preempted" (gethash "status" first))
                       (string= "sealed" (gethash "status" second))
                       (= 2 (funcall provider-calls))
                       (= 2 (length (funcall events)))
                       (= (gethash "id" (first (funcall events)))
                          (gethash "caused_by" (second (funcall events)))))))))

(let ((paused-once t))
  (multiple-value-bind (step events provider-calls ignored-sync)
      (kgfo-harness
       :provider (lambda (request)
                   (declare (ignore request))
                   (if paused-once
                       (progn (setf paused-once nil) :paused-budget)
                       (kgfo-proposal))))
    (declare (ignore ignored-sync))
    (let ((first (funcall step)) (second (funcall step)))
      (kgfo-check "provider budget pause preserves one durable open and resumes it"
                  (and (string= "paused-budget" (gethash "status" first))
                       (string= "sealed" (gethash "status" second))
                       (= 2 (funcall provider-calls))
                       (= 2 (length (funcall events)))
                       (= (gethash "id" (first (funcall events)))
                          (gethash "caused_by" (second (funcall events)))))))))

(multiple-value-bind (step events provider-calls ignored-sync)
    (kgfo-harness
     :provider (lambda (request)
                 (declare (ignore request))
                 (error "bad provider")))
  (declare (ignore ignored-sync))
  (let ((report (funcall step)))
    (kgfo-check "provider failure settles one bounded terminal receipt"
                (and (string= "failed" (gethash "status" report))
                     (= 1 (funcall provider-calls))
                     (= 2 (length (funcall events)))
                     (string= "knowledge-graph-formation-failed"
                              (gethash "type" (second (funcall events))))))))

(multiple-value-bind (step events provider-calls ignored-sync)
    (kgfo-harness
     :provider (lambda (request)
                 (declare (ignore request))
                 (obj "schema_version" 1 "unexpected" "shape")))
  (declare (ignore ignored-sync))
  (let ((report (funcall step)))
    (kgfo-check "malformed provider proposal settles one bounded terminal receipt"
                (and (string= "failed" (gethash "status" report))
                     (= 1 (funcall provider-calls))
                     (= 2 (length (funcall events)))
                     (string= "knowledge-graph-formation-failed"
                              (gethash "type" (second (funcall events))))))))

(multiple-value-bind (step events ignored-provider sync-calls)
    (kgfo-harness :sync (lambda () (error "derived unavailable")))
  (declare (ignore ignored-provider))
  (let ((report (funcall step)))
    (kgfo-check "derived failure cannot revoke sealed event authority"
                (and (string= "sealed" (gethash "status" report))
                     (string= "unavailable"
                              (gethash "status"
                                       (gethash "synchronization" report)))
                     (= 1 (funcall sync-calls))
                     (string= "knowledge-graph-formation-sealed"
                              (gethash "type" (second (funcall events))))))))

(multiple-value-bind (step events provider-calls ignored-sync)
    (kgfo-harness :operator (lambda () t))
  (declare (ignore ignored-sync))
  (kgfo-check "operator preemption before opening is event-write free"
              (and (string= "preempted" (gethash "status" (funcall step)))
                   (zerop (length (funcall events)))
                   (zerop (funcall provider-calls)))))

(multiple-value-bind (step events provider-calls ignored-sync)
    (kgfo-harness :budget (lambda () nil))
  (declare (ignore ignored-sync))
  (kgfo-check "budget pause before opening is event-write free"
              (and (string= "paused-budget" (gethash "status" (funcall step)))
                   (zerop (length (funcall events)))
                   (zerop (funcall provider-calls)))))

(multiple-value-bind (step events provider-calls ignored-sync)
    (kgfo-harness :selector (lambda (events agent persona)
                              (declare (ignore events agent persona)) nil))
  (declare (ignore ignored-sync))
  (kgfo-check "empty selector is idle and event-write free"
              (and (string= "idle" (gethash "status" (funcall step)))
                   (zerop (length (funcall events)))
                   (zerop (funcall provider-calls)))))

(format t "~%KG2 owner: ~d passed, ~d failed.~%" *kgfo-pass* *kgfo-fail*)
(when (plusp *kgfo-fail*) (uiop:quit 1))
