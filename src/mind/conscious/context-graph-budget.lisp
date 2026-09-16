;;;; Shared exposure projection. Defines only; no scan, reservation or dispatch at load.
(in-package :agent)

(defun context-graph-budget-project (events policy)
  "Fold verified, complete selected receipts supplied by the admission owner.
POLICY is trusted authorization configuration, never taken from a model request.
This pure projection alone neither proves scan completeness nor grants spending."
  (let ((rows (make-hash-table :test #'equal))
        (legacy-latest (make-hash-table :test #'equal)) (last-id 0) (poisoned nil)
        (prior (gethash "prior_exposure_microusd" policy))
        (cap (gethash "ceiling_microusd" policy))
        (request-cap (gethash "per_request_ceiling_microusd" policy)))
    (unless (and (integerp prior) (<= 0 prior) (integerp cap) (< 0 cap)
                 (integerp request-cap) (<= 1 request-cap 60000)
                 (every (lambda (key) (let ((value (gethash key policy)))
                                       (and (stringp value) (plusp (length value)))))
                        '("authorization_id" "agent_id" "persona_id" "generation")))
      (error "Invalid graph budget authorization configuration"))
    (when (> (length events) 20000) (error "Graph budget receipt bound exceeded"))
    (labels ((apply-row (key digest reservation charge reservation-p)
               (let ((old (gethash key rows)))
                 (unless (and (stringp digest) (plusp (length digest)))
                   (error "Budget receipt lacks a request digest"))
                 (if reservation-p
                     (progn
                       (unless (and (null old) (integerp reservation) (<= 1 reservation request-cap))
                         (error "Duplicate or invalid graph budget reservation"))
                       (setf (gethash key rows) (obj "digest" digest "reserved" reservation "charge" :null)))
                     (progn
                       (unless (and old (equal digest (gethash "digest" old))
                                    (eq :null (gethash "charge" old)) (integerp charge) (<= 0 charge))
                         (error "Unbound, duplicate or invalid graph budget settlement"))
                       (when (> charge (gethash "reserved" old)) (setf poisoned t))
                       (setf (gethash "charge" old) charge))))))
      (loop for event across events
            for id = (gethash "id" event)
            for payload = (gethash "payload" event)
            for type = (gethash "type" event) do
        (unless (and (integerp id) (> id last-id)) (error "Budget receipts are not strictly ordered"))
        (setf last-id id)
        (unless (and (equal (gethash "agent_id" policy) (gethash "agent_id" event))
                     (equal (gethash "persona_id" policy) (gethash "persona_id" payload)))
          (error "Budget receipt partition mismatch"))
        (cond
          ((equal type "context-graph-identity-phase")
           (unless (equal (gethash "generation" policy) (gethash "generation" payload))
             (error "Budget generation mismatch; predecessor exposure must be explicitly imported"))
           (let* ((record (pai.context-graph:context-graph-runtime-read-json (gethash "record_json" payload)))
                  (outcome (gethash "outcome" record))
                  (key (list "generation" (gethash "caused_by" event) (gethash "phase" record)))
                  (latest (gethash key legacy-latest))
                  (request-p (equal outcome "request")))
             (unless (and (integerp (second key)) (stringp (third key))
                          (member outcome '("request" "response" "paused" "rejected" "overrun") :test #'equal))
               (error "Malformed generation budget receipt"))
             (when request-p
               (when (and latest
                          (not (and (member (gethash "outcome" latest) '("paused" "rejected") :test #'equal)
                                    (equal (gethash "digest" latest) (gethash "request_digest" record)))))
                 (error "Legacy phase cannot reserve again in its current state"))
               ;; Preserve every charged attempt, even if a later request uses
               ;; the same opening/phase. A pause releases only its own zero charge.
               (setf latest (obj "key" (list key id) "digest" (gethash "request_digest" record))
                     (gethash key legacy-latest) latest))
             (unless latest (error "Legacy settlement has no reservation"))
             (when (and (equal outcome "paused") (not (eql 0 (gethash "charged_microusd" record))))
               (error "Legacy pause must have a verified zero charge"))
             (apply-row (gethash "key" latest) (gethash "request_digest" record)
                        (when request-p (gethash "reserved_microusd" record))
                        (gethash "charged_microusd" record) request-p)
             (setf (gethash "outcome" latest) outcome)))
          ((member type '("context-graph-budget-reserved" "context-graph-budget-settled") :test #'equal)
           (unless (and (equal (gethash "authorization_id" policy) (gethash "authorization_id" payload))
                        (equal (gethash "generation" policy) (gethash "generation" payload))
                        (stringp (gethash "reservation_id" payload))
                        (plusp (length (gethash "reservation_id" payload))))
             (error "Shared budget authorization lineage mismatch"))
           (apply-row (list "shared" (gethash "reservation_id" payload))
                      (gethash "request_digest" payload)
                      (when (equal type "context-graph-budget-reserved") (gethash "reserved_microusd" payload))
                      (gethash "charged_microusd" payload) (equal type "context-graph-budget-reserved")))
          (t (error "Unexpected event in graph budget projection")))))
    (let ((exposure prior) (pending 0))
      (maphash (lambda (key row)
                 (declare (ignore key))
                 (let ((charge (gethash "charge" row)))
                   (when (eq charge :null) (incf pending))
                   (incf exposure (if (eq charge :null) (gethash "reserved" row) charge)))) rows)
      (obj "authorization_id" (gethash "authorization_id" policy)
           "exposure_microusd" exposure "prior_exposure_microusd" prior
           "remaining_microusd" (max 0 (- cap exposure))
           "reservation_count" (hash-table-count rows) "pending_count" pending
           "poisoned" (if (or poisoned (> exposure cap)) :true :false)
           "through_event_id" last-id))))

(defun context-graph-budget-snapshot (backend policy)
  "Read selected verified budget receipts through one pinned GLOBAL head.
The returned head must be used by conditional append; this snapshot is not a
reservation. Prior generations are covered only by the trusted policy's imported
exposure. Bound all inspected receipts, not merely those retained in the fold."
  ;; Validate before accessing storage, including when no matching rows exist.
  (context-graph-budget-project #() policy)
  (let ((head (storage-head-position backend))
        (events (make-array 0 :adjustable t :fill-pointer 0))
        (visited 0) (bytes 0) (last-position 0))
    (unless (and (integerp head) (<= 0 head))
      (error "Invalid global budget storage head"))
    (multiple-value-bind (complete returned-position count)
        (storage-map-event-receipts
         backend
         (lambda (receipt)
           (incf visited)
           (when (> visited 20000) (error "Budget scan receipt bound exceeded"))
           (let ((position (gethash "storage_position" receipt))
                 (json (gethash "event_json" receipt)))
             (unless (and (integerp position) (< last-position position) (<= position head)
                          (stringp json))
               (error "Budget scan receipt boundary mismatch"))
             (setf last-position position)
             ;; Conservative UTF-8 upper bound avoids allocating another copy.
             (incf bytes (* 4 (length json)))
             (when (> bytes (* 64 1024 1024)) (error "Budget scan byte bound exceeded"))
             (let* ((event (pai.context-graph:context-graph-runtime-read-json json))
                    (payload (gethash "payload" event)))
               (unless (and (equal (gethash "agent_id" policy) (gethash "agent_id" event))
                            (eql (gethash "event_id" receipt) (gethash "id" event))
                            (equal (gethash "event_type" receipt) (gethash "type" event)))
                 (error "Budget scan verified envelope mismatch"))
               (cond
                 ((equal "context-graph-identity-phase" (gethash "type" event))
                  ;; Predecessor phase exposure is represented only by the
                  ;; policy's authenticated imported amount.
                  (when (and (equal (gethash "persona_id" policy) (gethash "persona_id" payload))
                             (equal (gethash "generation" policy) (gethash "generation" payload)))
                    (vector-push-extend event events)))
                 ((equal (gethash "authorization_id" policy)
                         (gethash "authorization_id" payload))
                  ;; Reusing one grant in another partition/generation is an
                  ;; authority error; it must not vanish behind scan filtering.
                  (unless (and (equal (gethash "persona_id" policy) (gethash "persona_id" payload))
                               (equal (gethash "generation" policy) (gethash "generation" payload)))
                    (error "Shared budget authorization reused outside its lineage"))
                  (vector-push-extend event events))))))
         :agent-id (gethash "agent_id" policy) :after-position 0 :through-position head
         :event-types '("context-graph-identity-phase" "context-graph-budget-reserved"
                        "context-graph-budget-settled"))
      (unless (and (eq complete t) (eql returned-position last-position) (eql count visited))
        (error "Incomplete budget receipt scan")))
    (values (context-graph-budget-project events policy) head events)))

(defun %context-graph-budget-string (value)
  (and (stringp value) (<= 1 (length value) 256)))

(defun context-graph-budget-append-phase (backend policy payload cause &key append-fn)
  "Admit the existing native phase receipt itself, without a second reservation.
Production owner validation remains required. This adds cumulative admission
against shared lab exposure at the same durable append boundary."
  (multiple-value-bind (projection head events) (context-graph-budget-snapshot backend policy)
    (let* ((record (pai.context-graph:context-graph-runtime-read-json (gethash "record_json" payload)))
           (request-p (equal "request" (gethash "outcome" record)))
           (prospective (obj "id" (1+ (gethash "through_event_id" projection))
                             "agent_id" (gethash "agent_id" policy)
                             "type" "context-graph-identity-phase" "payload" payload "caused_by" cause))
           (next (context-graph-budget-project (concatenate 'vector events (vector prospective)) policy)))
      (when (and request-p (or (eq :true (gethash "poisoned" projection))
                              (eq :true (gethash "poisoned" next))))
        (error "Native graph phase exceeds shared cumulative budget"))
      (if append-fn
          (let ((event (funcall append-fn head "context-graph-identity-phase" payload cause)))
            (unless (and (hash-table-p event)
                         (equal "context-graph-identity-phase" (gethash "type" event))
                         (equal (gethash "agent_id" policy) (gethash "agent_id" event)))
              (error "Conditional publication did not return the durable phase event"))
            event)
          (storage-append-event-if-head backend head "context-graph-identity-phase" payload
                                       :agent-id (gethash "agent_id" policy) :caused-by cause)))))

(defun %context-graph-budget-reservation (events reservation-id)
  (find-if (lambda (event)
             (and (equal "context-graph-budget-reserved" (gethash "type" event))
                  (equal reservation-id (gethash "reservation_id" (gethash "payload" event)))))
           events))

(defun context-graph-budget-reserve (backend policy reservation-id request-digest phase bound)
  "Durably admit one attempt. A repeated ID NEVER grants permission to send.
There is no automatic retry on a head conflict or an unknown previous outcome.
All spending consumers must use this owner before dispatch is enabled."
  (unless (and (every #'%context-graph-budget-string (list reservation-id request-digest phase))
               (integerp bound) (<= 1 bound))
    (error "Invalid selected-phase budget request"))
  (multiple-value-bind (projection head events) (context-graph-budget-snapshot backend policy)
    (when (%context-graph-budget-reservation events reservation-id)
      (error "Budget attempt already exists; do not resend"))
    (unless (and (eq :false (gethash "poisoned" projection))
                 (<= bound (gethash "per_request_ceiling_microusd" policy))
                 (<= bound (gethash "remaining_microusd" projection)))
      (error "Selected-phase budget exhausted or poisoned"))
    (storage-append-event-if-head
     backend head "context-graph-budget-reserved"
     (obj "authorization_id" (gethash "authorization_id" policy)
          "persona_id" (gethash "persona_id" policy) "generation" (gethash "generation" policy)
          "reservation_id" reservation-id "request_digest" request-digest
          "phase" phase "reserved_microusd" bound)
     :agent-id (gethash "agent_id" policy))))

(defun context-graph-budget-settle (backend policy reservation-id request-digest charge receipt-id)
  "Persist a known charge from trusted transport verification, including overrun.
RECEIPT-ID identifies that verification evidence, not a model's charge assertion.
Unknown outcomes must not call this function with a guessed or zero charge."
  (unless (and (every #'%context-graph-budget-string (list reservation-id request-digest receipt-id))
               (integerp charge) (<= 0 charge))
    (error "Invalid selected-phase settlement"))
  (multiple-value-bind (projection head events) (context-graph-budget-snapshot backend policy)
    (declare (ignore projection))
    (let ((reservation (%context-graph-budget-reservation events reservation-id)))
      (unless (and reservation
                   (equal request-digest (gethash "request_digest" (gethash "payload" reservation)))
                   (not (find-if (lambda (event)
                                   (and (equal "context-graph-budget-settled" (gethash "type" event))
                                        (equal reservation-id
                                               (gethash "reservation_id" (gethash "payload" event)))))
                                 events)))
        (error "Missing, mismatched or already settled budget attempt"))
      ;; An overrun must remain in authority even though it poisons new admission.
      (storage-append-event-if-head
       backend head "context-graph-budget-settled"
       (obj "authorization_id" (gethash "authorization_id" policy)
            "persona_id" (gethash "persona_id" policy) "generation" (gethash "generation" policy)
            "reservation_id" reservation-id "request_digest" request-digest
            "charged_microusd" charge "receipt_id" receipt-id)
       :agent-id (gethash "agent_id" policy)))))
