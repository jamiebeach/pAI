;;;; package.lisp -- fleet peer-to-peer agent communication (FLEET_DESIGN.md).
;;;;
;;;; Build-order step 1 only: agent identity and peer store persistence.
;;;; Transport, join handshake, boards and the stimulus loop are later files;
;;;; this package is inert on its own -- it opens no socket and starts no
;;;; thread, only reads and writes the peer store.
(defpackage :pai.fleet
  (:use :cl)
  (:export
   #:fleet-uuid4
   #:make-fleet-peer
   #:fleet-peer-id
   #:fleet-peer-name
   #:fleet-peer-address
   #:fleet-peer-shared-secret
   #:fleet-peer-joined-at
   #:fleet-peer-outbound-thread-id
   #:fleet-store-load
   #:fleet-store-save
   #:fleet-store-agent-id
   #:fleet-store-peers
   #:fleet-store-peer
   #:fleet-store-add-peer
   #:fleet-store-set-peer-outbound-thread-id
   #:fleet-store-path
   #:fleet-hmac-sign
   #:fleet-hmac-verify
   #:fleet-request-fresh-p
   #:fleet-unix-time
   #:fleet-join-code
   #:make-fleet-inbound-join-store
   #:fleet-inbound-join-register
   #:fleet-inbound-join-find
   #:fleet-inbound-join-remove
   #:fleet-inbound-join-prune
   #:make-fleet-outbound-join-store
   #:fleet-outbound-join-register
   #:fleet-outbound-join-find
   #:fleet-outbound-join-remove
   #:fleet-outbound-join-prune
   #:fleet-join-request-id
   #:fleet-join-request-requester-id
   #:fleet-join-request-requester-name
   #:fleet-join-request-requester-address
   #:fleet-join-request-code
   #:fleet-join-request-received-at
   #:fleet-join-approve
   #:fleet-join-accept-apply
   #:fleet-join-error
   #:fleet-join-error-reason
   #:+fleet-join-request-ttl-seconds+
   #:board-store-load
   #:board-store-save
   #:board-store-path
   #:board-post-message
   #:board-threads
   #:board-thread
   #:board-thread-messages
   #:board-messages-since
   #:board-thread-thread-id
   #:board-thread-title
   #:board-thread-status
   #:board-thread-created-by
   #:board-thread-created-at
   #:board-thread-last-activity-at
   #:board-message-msg-id
   #:board-message-thread-id
   #:board-message-author-id
   #:board-message-author-name
   #:board-message-text
   #:board-message-tags
   #:board-message-intent
   #:board-message-scope
   #:board-message-timestamp
   #:board-message-reply-to
   #:board-storage-unavailable
   #:board-error
   #:board-error-reason))

(in-package :pai.fleet)
