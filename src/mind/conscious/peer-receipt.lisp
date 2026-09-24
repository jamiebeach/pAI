;;;; Self-contained inbound experience. No board or transport dependency.
(in-package :agent)

(define-condition peer-receipt-unavailable (error) ()
  (:report (lambda (condition stream) (declare (ignore condition))
             (write-string "Peer receipt authority is unavailable; retry the same operation." stream))))
(define-condition peer-receipt-conflict (error) ()
  (:report (lambda (condition stream) (declare (ignore condition))
             (write-string "Peer operation identity was reused with different content." stream))))

(defun peer-message-find-receipt (agent-id sender-id operation-id)
  "Read local authority only; never consult a sender or board."
  ;; Stream instead of retaining all historical message contents per request.
  ;; A future derived lookup index may accelerate this without owning truth.
  (handler-case
      (unless (map-events
               (lambda (event)
                 (let ((payload (gethash "payload" event)))
                   (when (and (hash-table-p payload)
                              (equal agent-id (gethash "agent_id" event))
                              (equal agent-id (gethash "agent_id" payload))
                              (equal sender-id (gethash "sender_id" payload))
                              (equal operation-id (gethash "operation_id" payload)))
                     (return-from peer-message-find-receipt event))))
               :types '("peer-message-received"))
        (error 'peer-receipt-unavailable))
    (error () (error 'peer-receipt-unavailable)))
  nil)

(defun peer-message-record-receipt (payload)
  "Append complete received content once. Return the exact durable event.
Input is a trusted adapter snapshot; sender identity must already be verified."
  (unless (and (hash-table-p payload)
               (equal *agent-id* (gethash "agent_id" payload))
               (equal "authenticated-peer-content" (gethash "trust" payload)))
    (error "Peer receipt must be authenticated for this local agent"))
  (dolist (key '("agent_id" "sender_id" "board_owner_id" "thread_id"
                 "message_id" "operation_id" "text" "request_key"))
    (let ((value (gethash key payload)))
      (unless (and (stringp value)
                   (<= 1 (length value)
                       (cond ((equal key "text") 4000)
                             ;; The serialized idempotency key may be larger
                             ;; than the accepted JSON body after escaping.
                             ((equal key "request_key") 196608)
                             (t 128))))
        (error "Missing peer receipt field ~a" key))))
  (let ((existing nil))
    (multiple-value-bind (id durable event accepted)
        (log-event-if
         (lambda ()
           (setf existing
                 (peer-message-find-receipt
                  (gethash "agent_id" payload) (gethash "sender_id" payload)
                  (gethash "operation_id" payload)))
           (when (and existing
                      (not (equal (gethash "request_key" payload)
                                  (gethash "request_key" (gethash "payload" existing)))))
             (error 'peer-receipt-conflict))
           (null existing))
         "peer-message-received" payload)
      (declare (ignore id))
      (cond (existing existing)
            ((and accepted durable event) event)
            (t (error 'peer-receipt-unavailable))))))

(defun peer-message-receipt-context (receipt agent-id)
  "Project a bounded authenticated delivery from this agent's own ledger.
No live board or sender lookup is needed to interpret the root."
  (let ((payload (and (hash-table-p receipt) (gethash "payload" receipt))))
    (unless (and (equal "peer-message-received" (gethash "type" receipt))
                 (integerp (gethash "id" receipt))
                 (equal agent-id (gethash "agent_id" receipt))
                 (hash-table-p payload)
                 (equal agent-id (gethash "agent_id" payload))
                 (every (lambda (key)
                          (%recursive-nonempty-string-p
                           (gethash key payload)
                           (if (equal key "text") 4000 128)))
                        '("sender_id" "board_owner_id" "thread_id"
                          "message_id" "text"))
                 (equal "authenticated-peer-content"
                        (gethash "trust" payload)))
      (error "Peer root requires this agent's complete authenticated receipt"))
    (obj "source" "fleet-board"
         "environment" (obj "kind" "fleet-board"
                            "owner_id" (gethash "board_owner_id" payload)
                            "resource_id" (gethash "thread_id" payload))
         "details" (obj "sender_id" (gethash "sender_id" payload)
                        "sender_name" (gethash "sender_name" payload :null)
                        "thread_id" (gethash "thread_id" payload)
                        "message_id" (gethash "message_id" payload)
                        "reply_to" (gethash "reply_to" payload :null)
                        "scope" (gethash "scope" payload :null)
                        "trust" "authenticated-peer-content"
                        "receipt_event_id" (gethash "id" receipt))
         "content" (gethash "text" payload))))
