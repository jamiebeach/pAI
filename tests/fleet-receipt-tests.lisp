;;;; harness: full-system
;;;; Synthetic receipt/retry tests using the real SQLite authority and board.
(in-package :agent)

(when (boundp 'cl-user::*fleet-direct-reopen-fixture*)
  (destructuring-bind (database root-id)
      (symbol-value 'cl-user::*fleet-direct-reopen-fixture*)
    (let* ((*agent-id* "recipient-fixture")
           (*event-authority-port* nil)
           (*event-ring* nil)
           (*event-next-id* 0)
           (*sqlite-event-authority-backend* nil)
           (*sqlite-event-authority-agent-id* nil)
           (*sqlite-event-authority-checkpoint-backend* nil)
           (*sqlite-event-authority-database* nil)
           (*sqlite-event-authority-derived-database* nil)
           (backend (make-sqlite-storage database)))
      (unwind-protect
           (progn
             (%sqlite-authority-install backend database backend database
                                        *agent-id*)
             (unless (and (equal "done"
                                 (gethash "state"
                                          (conscious-recursive-thread-project
                                           (replay-events) root-id *agent-id*)))
                          (not (find root-id
                                     (%recursive-pending-private-stimuli
                                      (replay-events) *agent-id*)
                                     :key (lambda (event)
                                            (gethash "id" event)))))
               (error "Fresh process did not restore terminal direct receipt"))
             (format t "DIRECT-RECEIPT-RESTART-PASS~%"))
        (event-authority-clear))))
  (uiop:quit 0))

(defun frt-check (name value)
  (unless value (error "FAIL ~a" name))
  (format t "PASS ~a~%" name))

(let* ((root (ensure-directories-exist
              (merge-pathnames (format nil "receipt-~a/" (pai.fleet:fleet-uuid4))
                               (test-state-dir))))
       (database (merge-pathnames "receipts.sqlite" root))
       (board-path (merge-pathnames "board.sexp" root))
       (*agent-id* "recipient-fixture")
       (*event-authority-port* nil) (*event-ring* nil) (*event-next-id* 0)
       (*runtime-observers* (make-hash-table :test #'equal))
       (*sqlite-event-authority-backend* nil) (*sqlite-event-authority-agent-id* nil)
       (*sqlite-event-authority-checkpoint-backend* nil)
       (*sqlite-event-authority-database* nil) (*sqlite-event-authority-derived-database* nil)
       (*fleet-store* (pai.fleet:fleet-store-load
                      (merge-pathnames "peers.sexp" root) (lambda () "board-owner-fixture")))
       (*board-store* (pai.fleet:board-store-load board-path))
       (peer (pai.fleet:make-fleet-peer
              :id "peer-fixture" :name "Peer One" :address "https://example.invalid"
              :shared-secret (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)
              :joined-at 0))
       (request (obj "operation_id" "send-1" "new_thread_title" "A question"
                     "text" "How did your synthetic restart go?"
                     "author_id" "forged-body-identity"))
       (first-response nil)
       (direct-root-id nil))
  (labels ((install-authority ()
             (let ((backend (make-sqlite-storage database)))
               (%sqlite-authority-install backend database backend database *agent-id*)))
           (receipt-count () (length (replay-events :types '("peer-message-received"))))
           (stimulus-count () (length (replay-events :types '("agent-stimulus-received"))))
           (local-reply (thread-id reply-to text &optional operation-id)
             (let ((original (symbol-function '%fleet-own-name)))
               (unwind-protect
                    (progn
                      (setf (symbol-function '%fleet-own-name)
                            (lambda () "Board Owner"))
                      (fleet-board-reply thread-id reply-to text operation-id))
                 (setf (symbol-function '%fleet-own-name) original)))))
    (unwind-protect
         (progn
           (install-authority)
           (let* ((*conscious-recursive-mind-fleet-board-reply-fn* #'local-reply)
                  (schemas (%recursive-tool-schemas nil nil nil))
                  (schema (find "reply-fleet-board-message" schemas
                                :key (lambda (item)
                                       (gethash "name" (gethash "function" item)))
                                :test #'equal)))
             (frt-check "recursive mind advertises local board reply" schema)
             (frt-check "reply schema requires thread, parent, and text"
                        (equalp #("thread_id" "reply_to" "text")
                                (gethash "required"
                                         (gethash "parameters"
                                                  (gethash "function" schema)))))
             (frt-check "reply arguments pass closed validation"
                        (hash-table-p
                         (%recursive-validate-tool-arguments
                          "reply-fleet-board-message"
                          (obj "thread_id" "thread" "reply_to" "message"
                               "text" "Synthetic reply"))))
             (frt-check "reply validation rejects missing parent"
                        (handler-case
                            (progn (%recursive-validate-tool-arguments
                                    "reply-fleet-board-message"
                                    (obj "thread_id" "thread" "text" "Synthetic reply"))
                                   nil)
                          (error () t))))
           (let* ((*conscious-recursive-mind-fleet-board-reply-fn* #'local-reply)
                  (response
                    (obj "choices"
                         (vector
                          (obj "message"
                               (obj "role" "assistant" "content" :null
                                    "tool_calls"
                                    (vector
                                     (obj "id" "provider-reply" "type" "function"
                                          "function"
                                          (obj "name" "reply-fleet-board-message"
                                               "arguments"
                                               "{\"thread_id\":\"thread\",\"reply_to\":\"message\",\"text\":\"Synthetic reply\"}")))))))))
             (frt-check "native wire admits advertised local board reply"
                        (handler-case
                            (multiple-value-bind (message arguments)
                                (%recursive-normalize-assistant-message
                                 response "thread:wire" "model:wire" t)
                              (and (= 1 (length (gethash "tool_calls" message)))
                                   (= 1 (length arguments))
                                   (equal "message"
                                          (gethash "reply_to" (aref arguments 0)))))
                          (error () nil))))
           (pai.fleet:fleet-store-add-peer *fleet-store* peer)
           (let ((saved (symbol-function '%fleet-http-post-signed))
                 (sent nil)
                 (attempts 0))
             (unwind-protect
                  (progn
                    (pai.fleet:fleet-store-set-peer-outbound-thread-id
                     *fleet-store* "peer-fixture" "thread:original")
                    (setf (symbol-function '%fleet-http-post-signed)
                          (lambda (target path body)
                            (declare (ignore target))
                            (unless (equal path "/board/post")
                              (error "Unexpected fleet transport"))
                            (unless (%fleet-notification-event
                                     "peer-board-publication-intent"
                                     "publish:fixture")
                              (error "Outbound request was not durable before transport"))
                            (push (shasht:write-json body nil) sent)
                            (incf attempts)
                            (when (= attempts 1)
                              (error "Synthetic timeout after peer acceptance"))
                            (obj "thread_id" "thread:original"
                                 "msg_id" "message:accepted")))
                    (ignore-errors
                      (fleet-board-post "peer-fixture" "A synthetic outbound thought."
                                        nil nil nil "publish:fixture"))
                    (pai.fleet:fleet-store-set-peer-outbound-thread-id
                     *fleet-store* "peer-fixture" "thread:changed")
                    (fleet-board-post "peer-fixture" "A synthetic outbound thought."
                                      nil nil nil "publish:fixture")
                    (frt-check "recovered implicit post uses frozen original request"
                               (and (= attempts 2)
                                    (equal (first sent) (second sent))
                                    (search "thread:original" (first sent))))
                    (frt-check "publication intent exists once before retry"
                               (= 1 (length (replay-events
                                             :types '("peer-board-publication-intent")))))
                    (frt-check "same operation ID rejects changed outbound text"
                               (handler-case
                                   (progn
                                     (fleet-board-post "peer-fixture" "Different text"
                                                       nil nil nil "publish:fixture")
                                     nil)
                                 (error () t))))
               (setf (symbol-function '%fleet-http-post-signed) saved)))
           (setf first-response (fleet-accept-board-post peer request))
           (frt-check "one durable receipt" (= 1 (receipt-count)))
           (frt-check "new delivery has no duplicate generic stimulus"
                      (zerop (stimulus-count)))
           (let ((*conscious-recursive-mind-agent-id* *agent-id*))
             (%conversation-append-readable
              "recursive-private-opportunity-selected"
              (obj "schema_version" 1 "opportunity" "stimulus"
                   "candidates" #("stimulus" "private-work")
                   "selected_at" 1))
             (frt-check "fairness choice reads from local authority"
                        (equal "stimulus"
                               (%recursive-last-private-opportunity))))
           (let* ((event (peer-message-find-receipt *agent-id* "peer-fixture" "send-1"))
                  (payload (gethash "payload" event))
                  (thread-id (gethash "thread_id" payload))
                  (message-id (gethash "message_id" payload)))
             (frt-check "full received text in ledger" (equal (gethash "text" request)
                                                               (gethash "text" payload)))
             (frt-check "identity is verified peer, not body" (equal "peer-fixture" (gethash "sender_id" payload)))
             (frt-check "scope defaults pairwise" (equal "pairwise" (gethash "scope" payload)))
             (frt-check "receipt is a private, non-operator stimulus"
                        (let ((stimulus (stimulus-from-event event)))
                          (and (equal "environment-change"
                                      (gethash "kind" stimulus))
                               (equal "system" (gethash "source" stimulus)))))
             (let ((context (recursive-stimulus-context event)))
               (frt-check "direct receipt retains exact authenticated content"
                          (and (not (find (gethash "id" event)
                                          (replay-events
                                           :types '("agent-stimulus-received"))
                                          :key (lambda (item)
                                                 (gethash "caused_by" item))))
                               (equal (gethash "text" request)
                                      (gethash "content" context))
                               (equal "peer-fixture"
                                      (gethash "sender_id"
                                               (gethash "details" context))))))
             (let ((rendered (fleet-board-read thread-id)))
               (frt-check "board read exposes replyable message ID"
                          (search message-id rendered)))
             (let ((result (local-reply thread-id message-id
                                        "My local synthetic reply."
                                        "fixture-reply-operation")))
               (frt-check "local reply reports same board thread" (search thread-id result))
               (frt-check "same local operation ID cannot duplicate reply"
                          (and (equal result
                                      (local-reply thread-id message-id
                                                   "My local synthetic reply."
                                                   "fixture-reply-operation"))
                               (= 2 (length (pai.fleet:board-thread-messages
                                             *board-store* thread-id)))))
               (frt-check "local reply retains one durable peer notification"
                          (let* ((queued (replay-events
                                          :types '("peer-board-notification-queued")))
                                 (payload (and queued (gethash "payload" (first queued)))))
                            (and (= 1 (length queued))
                                 (equal "peer-fixture" (gethash "peer_id" payload))
                                 (equal thread-id
                                        (gethash "thread_id"
                                                 (gethash "notification" payload))))))
               (let* ((messages (pai.fleet:board-thread-messages *board-store* thread-id))
                      (reply (find message-id messages :key #'pai.fleet:board-message-reply-to
                                                         :test #'equal)))
                 (frt-check "local reply remains in original thread" (= 2 (length messages)))
                 (frt-check "local reply has board owner identity"
                            (equal "board-owner-fixture"
                                   (pai.fleet:board-message-author-id reply)))))
             (let ((saved (symbol-function '%fleet-http-post-signed))
                   (received nil)
                   (delivery-count 0)
                   (receiver-database (merge-pathnames "reply-recipient.sqlite" root))
                   (sender-peer
                     (pai.fleet:make-fleet-peer
                      :id "board-owner-fixture" :name "Board Owner"
                      :address "https://example.invalid"
                      :shared-secret (make-array 32 :element-type '(unsigned-byte 8)
                                                 :initial-element 0)
                      :joined-at 0)))
               (unwind-protect
                    (progn
                      (setf (symbol-function '%fleet-http-post-signed)
                            (lambda (target path body)
                              (declare (ignore target))
                              (unless (equal path "/fleet/board-notification")
                                (error "Unexpected peer notification transport"))
                              (incf delivery-count)
                              (let* ((*agent-id* "peer-fixture")
                                     (*event-authority-port* nil)
                                     (*event-ring* nil)
                                     (*event-next-id* 0)
                                     (*sqlite-event-authority-backend* nil)
                                     (*sqlite-event-authority-agent-id* nil)
                                     (*sqlite-event-authority-checkpoint-backend* nil)
                                     (*sqlite-event-authority-database* nil)
                                     (*sqlite-event-authority-derived-database* nil)
                                     (backend (make-sqlite-storage receiver-database)))
                                (unwind-protect
                                     (let ((accepted
                                             (fleet-accept-board-notification
                                              sender-peer body)))
                                       (setf received
                                             (peer-message-find-receipt
                                              *agent-id* "board-owner-fixture"
                                              (gethash "operation_id" body)))
                                       accepted)
                                  (storage-close backend)))))
                      (fleet-board-notification-flush-one)
                      (frt-check "same-board reply reaches receiving authority"
                                 (and (= 1 delivery-count)
                                      (hash-table-p received)
                                      (equal "My local synthetic reply."
                                             (gethash "text"
                                                      (gethash "payload" received)))
                                      (equal thread-id
                                             (gethash "thread_id"
                                                      (gethash "payload" received)))
                                      (equal message-id
                                             (gethash "reply_to"
                                                      (gethash "payload" received)))))
                      (frt-check "delivered reply is not sent twice"
                                 (progn (fleet-board-notification-flush-one)
                                        (= 1 delivery-count))))
                 (setf (symbol-function '%fleet-http-post-signed) saved)))
             (let ((saved (symbol-function '%fleet-http-post-signed))
                   (attempts 0)
                   (manual-id "board-notification:manual-fixture"))
               (unwind-protect
                    (progn
                      (%fleet-notification-append-once
                       "peer-board-notification-queued"
                       (obj "schema_version" 1 "agent_id" *agent-id*
                            "peer_id" "peer-fixture" "operation_id" manual-id
                            "notification"
                            (obj "operation_id" manual-id "text" "Synthetic notice"
                                 "board_owner_id" "board-owner-fixture"
                                 "thread_id" thread-id "message_id" "manual-message"
                                 "reply_to" message-id)))
                      (setf (symbol-function '%fleet-http-post-signed)
                            (lambda (target path body)
                              (declare (ignore target body))
                              (unless (equal path "/fleet/board-notification")
                                (error "Unexpected notification transport"))
                              (incf attempts)
                              (obj "receipt_event_id" 777)))
                      (dotimes (ignored 2)
                        (declare (ignore ignored))
                        (fleet-board-notification-flush-one))
                      (frt-check "outbox retry marks original operation delivered"
                                 (%fleet-notification-event
                                  "peer-board-notification-delivered" manual-id))
                      (frt-check "delivered outbox item is not sent again"
                                 (let ((before attempts))
                                   (fleet-board-notification-flush-one)
                                   (= before attempts))))
                 (setf (symbol-function '%fleet-http-post-signed) saved)))
             (frt-check "cross-thread reply parent is rejected"
                        (multiple-value-bind (other-message other-thread)
                            (pai.fleet:board-post-message
                             *board-store* :new-thread-title "Other"
                             :author-id "peer-fixture" :author-name "Peer One"
                             :text "Other root")
                          (declare (ignore other-message))
                          (handler-case
                              (progn (local-reply other-thread message-id
                                                 "Invalid cross-thread reply") nil)
                            (pai.fleet:board-error () t)))))
           (let ((again (fleet-accept-board-post peer request)))
             (frt-check "retry preserves receipt ID"
                        (= (gethash "receipt_event_id" first-response) (gethash "receipt_event_id" again)))
             (frt-check "retry creates no event" (= 1 (receipt-count)))
             (frt-check "retry creates no generic bridge"
                        (zerop (stimulus-count)))
             (frt-check "retry creates no board message"
                        (= 2 (length (pai.fleet:board-thread-messages
                                      *board-store* (gethash "thread_id" again))))))
           (let ((different (obj "operation_id" "send-1" "new_thread_title" "A question" "text" "changed")))
             (frt-check "conflicting retry refused"
                        (handler-case (progn (fleet-accept-board-post peer different) nil)
                          (peer-receipt-conflict () t))))
           (dolist (mode '(incomplete signalled))
             (let ((port (copy-list *event-authority-port*)))
               (setf (getf port :map)
                     (lambda (&rest args)
                       (declare (ignore args))
                       (if (eq mode 'signalled) (error "synthetic read failure") nil)))
               (let ((*event-authority-port* port))
                 (frt-check (format nil "~a ledger scan is retryable, not absence" mode)
                            (handler-case (progn (fleet-accept-board-post peer request) nil)
                              (peer-receipt-unavailable () t))))))
           (frt-check "board persistence failure has retryable condition"
                      (handler-case
                          (progn (pai.fleet:board-store-save
                                  (pai.fleet::%make-board-store
                                   :path (pathname (concatenate 'string
                                                               (namestring database)
                                                               "/blocked.sexp")))) nil)
                        (pai.fleet:board-storage-unavailable () t)))
           ;; The board write succeeds, then the authority refuses its append.
           (let* ((retry (obj "operation_id" "send-2" "new_thread_title" "Second" "text" "Second question"))
                  (port (copy-list *event-authority-port*)))
             (setf (getf port :append) (lambda (&rest args) (declare (ignore args)) (values nil nil nil)))
             (let ((*event-authority-port* port))
               (frt-check "failed ledger append cannot acknowledge delivery"
                          (handler-case (progn (fleet-accept-board-post peer retry) nil)
                            (peer-receipt-unavailable () t))))
             (frt-check "failed receipt did not append" (= 1 (receipt-count)))
             ;; Reopen board after interrupted delivery, then retry.
             (setf *board-store* (pai.fleet:board-store-load board-path))
             (let ((response (fleet-accept-board-post peer retry)))
               (frt-check "retry finishes interrupted delivery" (= 2 (receipt-count)))
               (frt-check "interrupted retry retains single board message"
                          (= 1 (length (pai.fleet:board-thread-messages
                                        *board-store* (gethash "thread_id" response)))))))
           (let* ((retry (obj "operation_id" "direct-receipt"
                              "new_thread_title" "Direct attention"
                              "text" "A synthetic direct private stimulus."))
                  (response (fleet-accept-board-post peer retry)))
             (frt-check "successful receipt is sufficient for private admission"
                        (and (= 3 (receipt-count))
                             (zerop (stimulus-count))
                             (= 1 (length
                                   (pai.fleet:board-thread-messages
                                    *board-store* (gethash "thread_id" response)))))))
           ;; Same content, deliberately distinct send, is not content deduped.
           (let ((fresh (obj "operation_id" "send-3" "new_thread_title" "A question"
                             "text" (gethash "text" request))))
             (fleet-accept-board-post peer fresh)
             (frt-check "distinct send IDs preserve repeated content" (= 4 (receipt-count))))
           (let* ((manual
                    (peer-message-record-receipt
                     (obj "schema_version" 1 "agent_id" *agent-id*
                          "sender_id" "peer-fixture" "sender_name" "Peer One"
                          "operation_id" "interrupted-intake"
                          "request_key" "synthetic-intake"
                          "text" "A retained message after interrupted intake."
                          "board_owner_id" "board-owner-fixture"
                          "thread_id" "thread-fixture" "message_id" "message-fixture"
                          "reply_to" :null "scope" "pairwise"
                          "trust" "authenticated-peer-content")))
                  (before (stimulus-count))
                  (opened (%recursive-open-stimulus-activity
                           (replay-events) manual *agent-id*)))
             (frt-check "unbridged durable receipt is selected directly"
                        (and (find (gethash "id" manual)
                                   (%recursive-pending-private-stimuli
                                    (replay-events) *agent-id*)
                                   :key (lambda (event) (gethash "id" event)))
                             (= before (stimulus-count))))
             (frt-check "direct activity freezes local receipt and full content"
                        (let* ((data (gethash "payload" opened))
                               (retained (aref (gethash "contexts" data) 0)))
                          (and (equalp (vector (gethash "id" manual))
                                       (gethash "source_event_ids" data))
                               (equal (gethash "id" manual)
                                      (gethash "source_event_id" retained))
                               (equal "A retained message after interrupted intake."
                                      (gethash "content"
                                               (gethash "context" retained))))))
             (let* ((root-id (gethash "id" manual))
                    (thread (format nil "thread:stimulus:~a:~a"
                                    *agent-id* root-id))
                    (model-id "model:direct-restart")
                    (content "A synthetic private conclusion."))
               (setf direct-root-id root-id)
               (%conversation-append-readable
                "model-request"
                (obj "thread_id" thread "model_call_id" model-id)
                :caused-by root-id)
               (%conversation-append-readable
                "model-response"
                (obj "thread_id" thread "model_call_id" model-id
                     "status" "accepted"
                     "assistant_message"
                     (obj "role" "assistant" "content" content))
                :caused-by root-id)
               (%conversation-append-readable
                "recursive-stimulus-result"
                (obj "thread_id" thread "model_call_id" model-id
                     "status" "completed" "audience" "private"
                     "content" content)
                :caused-by root-id)
               (frt-check "completed direct root projects done before restart"
                          (equal "done"
                                 (gethash "state"
                                          (conscious-recursive-thread-project
                                           (replay-events) root-id *agent-id*))))))
           ;; Drop all cached state and reopen actual SQLite authority.
           (event-authority-clear)
           (setf *event-ring* nil *event-next-id* 0 *board-store* nil *fleet-store* nil)
           (delete-file board-path)
           (multiple-value-bind (output ignored status)
               (uiop:run-program
                (list "sbcl" "--dynamic-space-size" "2048"
                      "--non-interactive"
                      "--load" "/opt/quicklisp/setup.lisp"
                      "--eval" "(require :asdf)"
                      "--eval"
                      (format nil
                              "(defparameter cl-user::*fleet-direct-reopen-fixture* (list ~s ~d))"
                              (namestring database) direct-root-id)
                      "--load"
                      (namestring
                       (merge-pathnames "tests/isolated-harness.lisp"
                                        cl-user::*pai-root*)))
                :output :string :error-output :string
                :ignore-error-status t)
             (declare (ignore ignored))
             (frt-check "completed direct receipt survives fresh Lisp process"
                        (and (zerop status)
                             (search "DIRECT-RECEIPT-RESTART-PASS" output)
                             (not (search "HARNESS-ERR" output)))))
           (install-authority)
           (frt-check "frozen outbound request survives authority reopen"
                      (let* ((intent (%fleet-notification-event
                                      "peer-board-publication-intent"
                                      "publish:fixture"))
                             (payload (and intent (gethash "payload" intent)))
                             (body (and payload (gethash "request" payload))))
                        (and (hash-table-p body)
                             (equal "thread:original"
                                    (gethash "thread_id" body)))))
           (let ((*conscious-recursive-mind-agent-id* *agent-id*))
             (frt-check "fairness choice survives authority reopen"
                        (equal "stimulus"
                               (%recursive-last-private-opportunity))))
           (let* ((response (fleet-accept-board-post peer request))
                  (receipt (peer-message-find-receipt *agent-id* "peer-fixture" "send-1")))
             (frt-check "receipt survives reopen without board or peer store"
                        (= (gethash "receipt_event_id" response) (gethash "id" receipt)))
             (frt-check "experience reconstructs with sender unreachable"
                        (equal (gethash "text" request) (gethash "text" (gethash "payload" receipt))))
             (frt-check "recovery adds no receipts" (= 5 (receipt-count)))
             (frt-check "recovery creates no generic bridge"
                        (zerop (stimulus-count))))
           (let* ((receipt (peer-message-find-receipt
                            *agent-id* "peer-fixture" "interrupted-intake"))
                  (id (gethash "id" receipt))
                  (hot (%recursive-thread-events)))
             (frt-check "direct completion survives SQLite reopen without re-execution"
                        (and (equal "done"
                                    (gethash "state"
                                             (conscious-recursive-thread-project
                                              (replay-events) id *agent-id*)))
                             (find id hot :key (lambda (event)
                                                 (gethash "id" event)))
                             (find-if (lambda (event)
                                        (and (equal "recursive-stimulus-result"
                                                    (gethash "type" event))
                                             (equal id (gethash "caused_by" event))))
                                      hot)
                             (not (find id
                                        (%recursive-pending-private-stimuli
                                         hot *agent-id*)
                                        :key (lambda (event)
                                               (gethash "id" event))))
                             (not (find id
                                        (%recursive-pending-private-stimuli
                                         (replay-events) *agent-id*)
                                        :key (lambda (event)
                                               (gethash "id" event)))))))
           (frt-check "unknown peer rejected" (handler-case (progn (fleet-accept-board-post nil request) nil) (error () t)))
           (frt-check "oversized content rejected before acceptance"
                      (handler-case
                          (progn (fleet-accept-board-post peer (obj "new_thread_title" "large" "text" (make-string 4001 :initial-element #\x))) nil)
                        (error () t)))
           (let* ((notification
                    (obj "operation_id" "notice-1" "text" "A synthetic board reply"
                         "board_owner_id" "peer-fixture" "thread_id" "peer-thread"
                         "message_id" "peer-message" "reply_to" "peer-parent"))
                  (accepted (fleet-accept-board-notification peer notification))
                  (again (fleet-accept-board-notification peer notification)))
             (frt-check "signed notification retains complete local content"
                        (let ((receipt (peer-message-find-receipt
                                        *agent-id* "peer-fixture" "notice-1")))
                          (and receipt
                               (equal "A synthetic board reply"
                                      (gethash "text" (gethash "payload" receipt)))
                               (equal "peer-thread"
                                      (gethash "thread_id" (gethash "payload" receipt))))))
             (frt-check "notification retry retains one receipt"
                        (and (equal (gethash "receipt_event_id" accepted)
                                    (gethash "receipt_event_id" again))
                             (eq t (gethash "duplicate" again))))
             (frt-check "peer cannot claim another board in notification"
                        (handler-case
                            (progn
                              (fleet-accept-board-notification
                               peer (obj "operation_id" "notice-forged"
                                         "text" "forged" "board_owner_id" "another-peer"
                                         "thread_id" "thread" "message_id" "message"
                                         "reply_to" "parent"))
                              nil)
                          (error () t))))
           (let* ((*conscious-recursive-mind-agent-id* *agent-id*)
                  (receipt (peer-message-find-receipt
                            *agent-id* "peer-fixture" "send-1"))
                  (bridge
                    (nth-value 1
                     (%conversation-append-readable
                      "agent-stimulus-received"
                      (let ((context (peer-message-receipt-context
                                      receipt *agent-id*)))
                        (%recursive-stimulus-payload
                         (gethash "source" context)
                         (gethash "content" context)
                         :environment (gethash "environment" context)
                         :details (gethash "details" context)))
                      :caused-by (gethash "id" receipt))))
                  (bridge-id (gethash "id" bridge)))
             (%conversation-append-readable
              "recursive-stimulus-result"
              (obj "schema_version" 1 "status" "completed"
                   "content" "Synthetic private conclusion")
              :caused-by bridge-id)
             (frt-check "completed legacy peer bridge gets durable disposition"
                        (and (%recursive-reconcile-peer-bridge-one
                              (replay-events))
                             (some (lambda (event)
                                     (and (equal bridge-id
                                                 (gethash "caused_by" event))
                                          (equal "absorbed"
                                                 (gethash "disposition"
                                                          (gethash "payload" event)))))
                                   (replay-events
                                    :types '("recursive-stimulus-disposition")))))
             (frt-check "reconciliation repairs consumption after disposition"
                        (and (%recursive-reconcile-peer-bridge-one
                              (replay-events))
                             (some (lambda (event)
                                     (equal bridge-id
                                            (gethash "caused_by" event)))
                                   (replay-events :types '("stimulus-consumed")))))
             (frt-check "repeated bridge reconciliation is idempotent"
                        (null (%recursive-reconcile-peer-bridge-one
                               (replay-events)))))
           (let* ((*conscious-recursive-mind-agent-id* *agent-id*)
                  (environment (obj "kind" "fixture-resource"
                                    "owner_id" *agent-id*
                                    "resource_id" "activity-fixture"))
                  (leader (nth-value 1
                           (%conversation-append-readable
                            "agent-stimulus-received"
                            (%recursive-stimulus-payload
                             "fixture" "First retained synthetic item"
                             :environment environment))))
                  (follower (nth-value 1
                             (%conversation-append-readable
                              "agent-stimulus-received"
                              (%recursive-stimulus-payload
                               "fixture" "Second retained synthetic item"
                               :environment environment))))
                  (leader-id (gethash "id" leader))
                  (follower-id (gethash "id" follower))
                  (opened (%recursive-open-stimulus-activity
                           (replay-events) leader *agent-id*)))
             (frt-check "activity opening freezes two exact source IDs"
                        (equalp (vector leader-id follower-id)
                                (gethash "source_event_ids"
                                         (gethash "payload" opened))))
             (frt-check "live recursive projection retains frozen activity"
                        (%recursive-activity-for-root
                         (%recursive-thread-events) leader-id *agent-id*))
             (frt-check "frozen follower appears in leader's request context"
                        (search "Second retained synthetic item"
                                (gethash "prompt"
                                         (%recursive-root-descriptor
                                          (%recursive-thread-events)
                                          leader-id *agent-id*))))
             (frt-check "activity opening is idempotent on replay"
                        (= (gethash "id" opened)
                           (gethash "id"
                                    (%recursive-open-stimulus-activity
                                     (replay-events) leader *agent-id*))))
             (frt-check "frozen follower is not selected for its own turn"
                        (not (find follower-id
                                   (%recursive-pending-private-stimuli
                                    (replay-events) *agent-id*)
                                   :key (lambda (event) (gethash "id" event)))))
             (%conversation-append-readable
              "recursive-stimulus-result"
              (obj "schema_version" 1 "status" "completed"
                   "content" "Synthetic bounded result")
              :caused-by leader-id)
             (frt-check "completed leader settles one follower disposition"
                        (%recursive-reconcile-activity-followers-one
                         (replay-events) *agent-id*))
             (frt-check "interrupted follower consumption is repaired"
                        (%recursive-reconcile-activity-followers-one
                         (replay-events) *agent-id*))
             (frt-check "follower settlement is idempotent"
                        (null (%recursive-reconcile-activity-followers-one
                               (replay-events) *agent-id*)))
             (frt-check "covered follower has one durable consumption"
                        (some (lambda (event)
                                (and (equal follower-id
                                            (gethash "caused_by" event))
                                     (equal "covered"
                                            (gethash "disposition"
                                                     (gethash "payload" event)))))
                              (replay-events :types '("stimulus-consumed"))))
             (event-authority-clear)
             (install-authority)
             (frt-check "activity and covered follower survive authority reopen"
                        (and (%recursive-activity-for-root
                              (replay-events) leader-id *agent-id*)
                             (null (%recursive-reconcile-activity-followers-one
                                    (replay-events) *agent-id*))
                             (not (find follower-id
                                        (%recursive-pending-private-stimuli
                                         (replay-events) *agent-id*)
                                        :key (lambda (event)
                                               (gethash "id" event))))))))
      (event-authority-clear))))
(let* ((payload (obj "agent_id" "fixture-recipient"
                     "sender_id" "fixture-peer" "sender_name" "Peer"
                     "board_owner_id" "fixture-recipient"
                     "thread_id" "fixture-thread" "message_id" "fixture-message"
                     "text" "A synthetic question" "trust" "authenticated-peer-content"))
       (receipt (obj "id" 90011 "type" "peer-message-received"
                     "agent_id" "fixture-recipient" "payload" payload))
       (context (peer-message-receipt-context receipt "fixture-recipient"))
       (descriptor (%recursive-root-descriptor
                    (list receipt) 90011 "fixture-recipient")))
  (frt-check "direct peer context retains exact local receipt link and content"
             (and (= 90011 (gethash "receipt_event_id"
                                  (gethash "details" context)))
                  (equal "A synthetic question" (gethash "content" context))))
  (frt-check "shared stimulus context preserves receipt and separates guidance"
             (let* ((shared (recursive-stimulus-context receipt))
                    (manual (gethash "adapter_guidance" shared)))
               (and (equal (gethash "content" context)
                           (gethash "content" shared))
                    (equal (shasht:write-json (gethash "details" context) nil)
                           (shasht:write-json (gethash "details" shared) nil))
                    (equal "registered-adapter"
                           (gethash "source" manual))
                    (equal "reply-fleet-board-message"
                           (gethash "reply_operation" manual)))))
  (frt-check "direct activity context retains the exact local receipt"
             (let* ((batch (%recursive-build-stimulus-activity
                            receipt (list receipt)))
                    (retained (aref (gethash "contexts" batch) 0)))
               (and (= 90011 (gethash "source_event_id" retained))
                    (equal "A synthetic question"
                           (gethash "content" (gethash "context" retained))))))
  (frt-check "direct peer root interprets as private stimulus"
             (and (equal "stimulus" (gethash "kind" descriptor))
                  (equal "private" (gethash "channel" descriptor))
                  (search "A synthetic question" (gethash "prompt" descriptor))))
  (frt-check "direct receipt retains same-board reply target"
             (and (equal "fixture-peer" (gethash "peer_id" descriptor))
                  (equal "fixture-thread"
                         (gethash "board_thread_id" descriptor))
                  (equal "fixture-message"
                         (gethash "board_message_id" descriptor))
                  (equal "reply-fleet-board-message"
                         (gethash "peer_reply_tool" descriptor))))
  (frt-check "remote-board notice selects sender-board post"
             (let* ((remote (alexandria:copy-hash-table payload))
                    (notice (obj "id" 90013 "type" "peer-message-received"
                                 "agent_id" "fixture-recipient" "payload" remote)))
               (setf (gethash "board_owner_id" remote) "fixture-peer")
               (equal "post-fleet-message"
                      (gethash "peer_reply_tool"
                               (%recursive-root-descriptor
                                (list notice) 90013 "fixture-recipient")))))
  (frt-check "another agent cannot interpret a peer receipt"
             (handler-case
                 (progn (peer-message-receipt-context receipt "other-agent") nil)
               (error () t)))
  (frt-check "untrusted peer receipt is not an executable root"
             (handler-case
                 (progn
                   (peer-message-receipt-context
                    (obj "id" 90012 "type" "peer-message-received"
                         "agent_id" "fixture-recipient"
                         "payload" (let ((copy (alexandria:copy-hash-table payload)))
                                     (setf (gethash "trust" copy) "unverified")
                                     copy))
                    "fixture-recipient")
                   nil)
               (error () t))))
(let* ((opened (obj "id" 90100 "type" "recursive-activity-opened"
                    "agent_id" "fixture-recipient" "caused_by" 90001
                    "payload" (obj "source_event_ids" #(90001 90005))))
       (foreign (obj "id" 90101 "type" "recursive-activity-opened"
                     "agent_id" "other-recipient" "caused_by" 90006
                     "payload" (obj "source_event_ids" #(90006 90005))))
       (owners (%recursive-activity-membership
                (list opened foreign) "fixture-recipient")))
  (frt-check "activity ownership includes exact frozen follower"
             (and (= 90001 (gethash 90001 owners))
                  (= 90001 (gethash 90005 owners))))
  (frt-check "foreign agent activity cannot claim local follower"
             (not (gethash 90006 owners)))
  (frt-check "activity ownership rejects conflicting local claim"
             (handler-case
                 (progn (%recursive-activity-membership
                         (list opened
                               (obj "id" 90102 "type" "recursive-activity-opened"
                                    "agent_id" "fixture-recipient"
                                    "caused_by" 90007
                                    "payload" (obj "source_event_ids" #(90007 90005))))
                         "fixture-recipient")
                        nil)
               (error () t)))
  (frt-check "activity ownership rejects duplicate member IDs"
             (handler-case
                 (progn (%recursive-activity-membership
                         (list (obj "id" 90103 "type" "recursive-activity-opened"
                                    "agent_id" "fixture-recipient"
                                    "caused_by" 90001
                                    "payload" (obj "source_event_ids" #(90001 90001))))
                         "fixture-recipient")
                        nil)
               (error () t))))
(let* ((same (loop for id from 91001 to 91010
                   collect (obj "id" id "type" "agent-stimulus-received"
                                "agent_id" "fixture-recipient"
                                "payload" (obj "text" "Synthetic update"
                                               "environment"
                                               (obj "kind" "document" "owner_id" "fixture"
                                                    "resource_id" "one")))))
       (unrelated (obj "id" 91011 "type" "agent-stimulus-received"
                       "agent_id" "fixture-recipient"
                       "payload" (obj "text" "Different document"
                                      "environment"
                                      (obj "kind" "document" "owner_id" "fixture"
                                           "resource_id" "two"))))
       (foreign (obj "id" 91012 "type" "agent-stimulus-received"
                     "agent_id" "other-recipient"
                     "payload" (gethash "payload" (first same))))
       (batch (%recursive-build-stimulus-activity
               (first same) (append same (list unrelated foreign))))
       (ids (gethash "source_event_ids" batch)))
  (frt-check "frozen activity includes at most eight ordered related receipts"
             (equalp #(91001 91002 91003 91004 91005 91006 91007 91008) ids))
  (frt-check "frozen context preserves each original event link"
             (= 91001 (gethash "source_event_id"
                              (aref (gethash "contexts" batch) 0)))))
(let* ((direct (obj "id" 92001 "type" "peer-message-received"
                    "agent_id" "fixture-recipient" "payload" (obj "text" "Direct")))
       (old (obj "id" 92002 "type" "peer-message-received"
                 "agent_id" "fixture-recipient" "payload" (obj "text" "Old")))
       (bridge (obj "id" 92003 "type" "agent-stimulus-received"
                    "agent_id" "fixture-recipient" "caused_by" 92002
                    "payload" (obj "text" "Old bridge")))
       (generic (obj "id" 92004 "type" "agent-stimulus-received"
                     "agent_id" "fixture-recipient" "caused_by" :null
                     "payload" (obj "text" "Document update")))
       (events (list direct old bridge generic)))
  (frt-check "mixed ledger selects one root per receipt"
             (equal '(92001 92003 92004)
                    (mapcar (lambda (event) (gethash "id" event))
                            (%recursive-pending-stimuli
                             events "fixture-recipient"))))
  (frt-check "terminal consumption and result suppress only their roots"
             (equal '(92004)
                    (mapcar (lambda (event) (gethash "id" event))
                            (%recursive-pending-stimuli
                             (append events
                                     (list (obj "id" 92005 "type" "stimulus-consumed"
                                                "agent_id" "fixture-recipient"
                                                "payload" (obj "stimulus_ids" #("stimulus:92001")))
                                           (obj "id" 92006 "type" "recursive-stimulus-result"
                                                "agent_id" "fixture-recipient"
                                                "caused_by" 92003
                                                "payload" (obj "status" "completed"))))
                             "fixture-recipient")))))
(let* ((agent-id "fixture-recipient")
       (payload (obj "agent_id" agent-id "sender_id" "fixture-peer"
                     "board_owner_id" agent-id "thread_id" "fixture-thread"
                     "message_id" "fixture-message" "text" "A synthetic question"
                     "trust" "authenticated-peer-content"))
       (direct (obj "id" 94001 "type" "peer-message-received"
                    "agent_id" agent-id "payload" payload))
       (follower-payload (alexandria:copy-hash-table payload))
       (follower (progn
                   (setf (gethash "message_id" follower-payload) "fixture-followup"
                         (gethash "text" follower-payload)
                         "A second synthetic thought")
                   (obj "id" 94007 "type" "peer-message-received"
                        "agent_id" agent-id "payload" follower-payload)))
       (legacy-request
         (obj "id" 94002 "type" "model-request" "agent_id" agent-id
              "caused_by" 94001
              "payload" (obj "thread_id"
                             "thread:peer-message:fixture-recipient:94001")))
       (legacy-terminal
         (obj "id" 94003 "type" "recursive-peer-message-disposition"
              "agent_id" agent-id "caused_by" 94001
              "payload" (obj "disposition" "replied")))
       (bridge
         (obj "id" 94004 "type" "agent-stimulus-received"
              "agent_id" agent-id "caused_by" 94001
              "payload" (%recursive-stimulus-payload
                         "fleet-board" "A synthetic question"
                         :environment
                         (obj "kind" "fleet-board" "owner_id" agent-id
                              "resource_id" "fixture-thread")))))
  (frt-check "new direct receipt is selected as one private root"
             (equal '(94001)
                    (mapcar (lambda (event) (gethash "id" event))
                            (%recursive-pending-private-stimuli
                             (list direct) agent-id))))
  (frt-check "related direct receipts freeze together by board thread"
             (let* ((batch (%recursive-build-stimulus-activity
                            direct (list direct follower)))
                    (activity (obj "id" 94008 "type" "recursive-activity-opened"
                                   "agent_id" agent-id "caused_by" 94001
                                   "payload" batch))
                    (descriptor (%recursive-root-descriptor
                                 (list direct follower activity) 94001 agent-id)))
               (and (equalp #(94001 94007)
                            (gethash "source_event_ids" batch))
                    (search "A second synthetic thought"
                            (gethash "prompt" descriptor)))))
  (frt-check "unfinished legacy direct turn cannot be reinterpreted"
             (null (%recursive-pending-private-stimuli
                    (list direct legacy-request) agent-id)))
  (frt-check "legacy peer disposition is terminal on mixed-ledger replay"
             (null (%recursive-pending-private-stimuli
                    (list direct legacy-terminal) agent-id)))
  (frt-check "historical linked bridge remains the only owner"
             (equal '(94004)
                    (mapcar (lambda (event) (gethash "id" event))
                            (%recursive-pending-private-stimuli
                             (list direct bridge) agent-id))))
  (frt-check "duplicate historical bridges fail closed"
             (handler-case
                 (progn
                   (%recursive-pending-private-stimuli
                    (list direct bridge
                          (obj "id" 94005 "type" "agent-stimulus-received"
                               "agent_id" agent-id "caused_by" 94001
                               "payload" (gethash "payload" bridge)))
                    agent-id)
                   nil)
               (error () t)))
  (frt-check "legacy direct result remains terminal without a bridge"
             (null (%recursive-pending-private-stimuli
                    (list direct
                          (obj "id" 94006 "type" "recursive-peer-message-result"
                               "agent_id" agent-id "caused_by" 94001
                               "payload" (obj "status" "completed")))
                    agent-id))))
(let* ((agent-id "fixture-recipient")
       (environment (obj "kind" "fleet-board" "owner_id" agent-id
                         "resource_id" "fixture-thread"))
       (root (obj "id" 93001 "type" "agent-stimulus-received"
                  "agent_id" agent-id "caused_by" 93000
                  "payload" (%recursive-stimulus-payload
                             "fleet-board" "First synthetic message"
                             :environment environment)))
       (follower (obj "id" 93002 "type" "agent-stimulus-received"
                      "agent_id" agent-id "caused_by" :null
                      "payload" (%recursive-stimulus-payload
                                 "fleet-board" "Second synthetic message"
                                 :environment environment)))
       (other (obj "id" 93003 "type" "agent-stimulus-received"
                   "agent_id" agent-id "caused_by" :null
                   "payload" (%recursive-stimulus-payload
                              "document" "Unrelated synthetic update")))
       (activity (obj "id" 93004 "type" "recursive-activity-opened"
                      "agent_id" agent-id "caused_by" 93001
                      "payload" (obj "source_event_ids" #(93001 93002)))))
  (frt-check "live generic selector skips a frozen follower"
             (equal '(93001 93003)
                    (mapcar (lambda (event) (gethash "id" event))
                            (%recursive-pending-private-stimuli
                             (list root follower other activity) agent-id))))
  (frt-check "terminal root settlement removes leader without releasing follower"
             (equal '(93003)
                    (mapcar (lambda (event) (gethash "id" event))
                            (%recursive-pending-private-stimuli
                             (list root follower other activity
                                   (obj "id" 93005 "type" "recursive-stimulus-disposition"
                                        "agent_id" agent-id "caused_by" 93001
                                        "payload" (obj "disposition" "absorbed")))
                             agent-id)))))
(let* ((seen nil)
      (*conscious-recursive-mind-fleet-message-fn*
        (lambda (peer text new-thread thread-id reply-to operation-id)
          (push (list peer text new-thread thread-id reply-to operation-id) seen)
          "synthetic-post"))
      (*conscious-recursive-mind-fleet-board-reply-fn*
        (lambda (thread-id reply-to text operation-id)
          (push (list thread-id reply-to text operation-id) seen)
          "synthetic-reply")))
  (frt-check "fleet effect ID is stable for one durable tool call"
             (let ((id (%recursive-fleet-operation-id 5 "tool-1")))
               (and (equal id (%recursive-fleet-operation-id 5 "tool-1"))
                    (not (equal id (%recursive-fleet-operation-id 5 "tool-2")))
                    (<= (length id) 128))))
  (%recursive-execute-fleet-publication
   "post-fleet-message"
   (obj "peer_id" "peer" "text" "reply" "thread_id" "thread"
        "reply_to" "parent") 5 "tool-1")
  (frt-check "peer-board publication receives exact parent and stable key"
             (and (equal '("peer" "reply" nil "thread" "parent")
                         (subseq (first seen) 0 5))
                  (equal (%recursive-fleet-operation-id 5 "tool-1")
                         (sixth (first seen)))))
  (%recursive-execute-fleet-publication
   "reply-fleet-board-message"
   (obj "thread_id" "local-thread" "reply_to" "local-parent"
        "text" "local reply") 5 "tool-2")
  (frt-check "local-board publication receives stable key"
             (equal (%recursive-fleet-operation-id 5 "tool-2")
                    (fourth (first seen)))))
(let* ((root (ensure-directories-exist
              (merge-pathnames
               (format nil "board-observation-~a/" (pai.fleet:fleet-uuid4))
               (test-state-dir))))
       (*fleet-store*
         (pai.fleet:fleet-store-load
          (merge-pathnames "peers.sexp" root)
          (lambda () "observation-owner")))
       (*board-store*
         (pai.fleet:board-store-load (merge-pathnames "board.sexp" root))))
  (multiple-value-bind (parent-id thread-id)
      (pai.fleet:board-post-message
       *board-store* :new-thread-title "Synthetic observation"
       :author-id "observation-owner" :author-name "Fixture"
       :text "What changed?" :timestamp 100)
    (loop for index from 1 to 9
          do (pai.fleet:board-post-message
              *board-store* :thread-id thread-id
              :author-id "synthetic-peer" :author-name "Peer Fixture"
              :text (format nil "Answer ~d" index)
              :reply-to parent-id :timestamp 101))
    (let* ((request
             (obj "kind" "fleet-board-thread"
                  "owner_id" "observation-owner"
                  "resource_id" thread-id))
           (first-page (observe-agent-environment request))
           (revision (gethash "revision" first-page))
           (second-page
             (observe-agent-environment
              (obj "kind" "fleet-board-thread"
                   "owner_id" "observation-owner"
                   "resource_id" thread-id
                   "revision" revision
                   "cursor" (gethash "next_cursor" first-page)))))
      (frt-check "board observation exposes bounded ordered thread pages"
                 (and (equal "observed" (gethash "status" first-page))
                      (= 8 (length (gethash "messages" first-page)))
                      (eq t (gethash "has_more" first-page))
                      (not (eq t (gethash "complete" first-page)))
                      (equal revision (gethash "revision" second-page))
                      (= 2 (length (gethash "messages" second-page)))
                      (not (eq t (gethash "has_more" second-page)))))
      (frt-check "board observation retains missing parent reference"
                 (find parent-id
                       (gethash "missing_parent_ids" second-page)
                       :test #'equal))
      (frt-check "board observation endpoint requires fleet authentication"
                 (and (%web-fleet-path-p "/board/observe")
                      (not (%web-fleet-unauthenticated-path-p
                            "/board/observe"))))
      (frt-check "unknown observation owner does not become a model URL"
                 (handler-case
                     (progn
                       (observe-agent-environment
                        (obj "kind" "fleet-board-thread"
                             "owner_id" "https://untrusted.invalid"
                             "resource_id" thread-id))
                       nil)
                   (error () t)))
      (frt-check "peer receipt receives adapter guidance separately"
                 (let* ((event
                          (obj "id" 90123
                               "type" "peer-message-received"
                               "agent_id" "observation-owner"
                               "payload"
                               (obj "agent_id" "observation-owner"
                                    "sender_id" "synthetic-peer"
                                    "board_owner_id" "observation-owner"
                                    "thread_id" thread-id
                                    "message_id" parent-id
                                    "text" "Synthetic peer content"
                                    "trust" "authenticated-peer-content")))
                        (context (recursive-stimulus-context event))
                        (manual (gethash "adapter_guidance" context)))
                   (and (equal "Synthetic peer content"
                               (gethash "content" context))
                        (equal "registered-adapter"
                               (gethash "source" manual))
                        (equal "reply-fleet-board-message"
                               (gethash "reply_operation" manual))))))))
(format t "PASS fleet-receipt-tests~%")
