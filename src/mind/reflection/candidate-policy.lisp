;;;; candidate-policy.lisp -- E5/E6 candidate generation + interruption
;;;; policy, 2026-07-29.  Loaded after initiative-engine.lisp.
;;;;
;;;; No producer sends a message directly.  A producer supplies a reason;
;;;; this layer records competing outward/internal/silence candidates, applies
;;;; provenance, repetition, unanswered-contact and manipulation gates, and
;;;; only then invokes the original delivery path under a dynamic candidate id.

(in-package :agent)

(export '(initiative-policy-report initiative-candidates
          initiative-committed-delivery-readiness
          initiative-deliver-committed-result
          initiative-deliver-approved-message))

(defparameter *initiative-candidate-file* #P"/agent/state/initiative-candidates.json")
(defparameter *initiative-candidate-max-records* 500)
(defparameter *initiative-unanswered-max* 1)
(defparameter *initiative-topic-block-seconds* (* 24 60 60))
(defparameter *initiative-quiet-hours* nil
  "Optional list of UTC hours during which normal proactive delivery is
suppressed. NIL is deliberately the conservative default until the operator sets an
explicit preference; permission must not be inferred.")
(defvar *initiative-candidates* nil "Newest first; durable decision ledger.")
(defvar *initiative-candidate-lock* (bt:make-lock "initiative-candidates"))
(defvar *initiative-policy-current-id* nil "Dynamically bound only while the original delivery path runs.")
(defvar *public-outbound-envelope* nil)

(defun %initiative-outbound-envelope (kind content record authorization-kind
                                      authorization-id &key v2-decision-id source)
  (and (fboundp 'make-public-outbound-envelope)
       (funcall 'make-public-outbound-envelope
                :kind kind :channel "telegram" :content content
                :source-event-ids (list (gethash "id" record))
                :causal-event-ids
                (remove nil (list (gethash "id" record) v2-decision-id))
                :authorization-kind authorization-kind
                :authorization-id authorization-id
                :legacy-authorization
                (obj "decision" (gethash "decision" record)
                     "candidate_id" (gethash "id" record))
                :v2-decision
                (if v2-decision-id
                    (obj "decision_id" v2-decision-id "result" "approved-not-delivered")
                    :null)
                :source source
                :dedupe-key (format nil "~a:~a" (string-downcase (string kind))
                                    (gethash "id" record)))))

(defun %initiative-id ()
  (format nil "init-~a-~a" (get-universal-time) (random 1000000)))

(defun %initiative-topic (text)
  "Small deterministic topic key for dedupe. It is deliberately a guard, not
a semantic substitute for the model's user-value judgment."
  (let* ((words (remove-if (lambda (w) (< (length w) 4))
                           (uiop:split-string (string-downcase (or text ""))
                                              :separator '(#\Space #\Tab #\Newline #\. #\, #\! #\? #\: #\;))))
         (picked (subseq words 0 (min 6 (length words)))))
    (format nil "~{~a~^-~}" picked)))

(defun %initiative-record (kind reason &key (status "candidate") urgency provenance)
  (obj "id" (%initiative-id) "kind" kind "reason" reason
       "topic" (%initiative-topic reason) "status" status
       "urgency" (string-downcase (string urgency))
       "provenance" (or provenance reason) "created_at" (get-universal-time)
       "updated_at" (get-universal-time) "user_value" :null
       "agent_outcome" :null "decision" :null "sent_at" :null
       "answered_at" :null "suppression_reason" :null))

(defun %initiative-save ()
  (let ((tmp (make-pathname :name "initiative-candidates-tmp" :type "json"
                            :defaults *initiative-candidate-file*)))
    (with-open-file (out tmp :direction :output :if-exists :supersede
                         :if-does-not-exist :create :external-format :utf-8)
      (let ((*print-pretty* nil))
        (shasht:write-json (coerce *initiative-candidates* 'vector) out)))
    (rename-file tmp *initiative-candidate-file*)))

(defun %initiative-load ()
  (handler-case
      (when (probe-file *initiative-candidate-file*)
        (with-open-file (in *initiative-candidate-file*)
          (setf *initiative-candidates* (coerce (shasht:read-json in) 'list))))
    (error (e) (format t "~&[candidate-policy] load failed: ~a~%" e) nil)))

(defun %initiative-append (record)
  (bt:with-lock-held (*initiative-candidate-lock*)
    (push record *initiative-candidates*)
    (when (> (length *initiative-candidates*) *initiative-candidate-max-records*)
      (setf *initiative-candidates* (subseq *initiative-candidates* 0 *initiative-candidate-max-records*)))
    (%initiative-save))
  record)

(defun %initiative-update (id key value)
  (bt:with-lock-held (*initiative-candidate-lock*)
    (let ((record (find id *initiative-candidates* :key (lambda (r) (gethash "id" r)) :test #'string=)))
      (when record
        (setf (gethash key record) value
              (gethash "updated_at" record) (get-universal-time))
        (%initiative-save))
      record)))

(defun %initiative-open-unanswered-count ()
  (count-if (lambda (r) (and (string= (gethash "status" r) "sent")
                             (eq (gethash "answered_at" r) :null)))
            *initiative-candidates*))

(defun %initiative-same-topic-p (topic)
  (let ((cutoff (- (get-universal-time) *initiative-topic-block-seconds*)))
    (find-if (lambda (r) (and (string= topic (gethash "topic" r))
                              (member (gethash "status" r) '("sent" "deferred") :test #'string=)
                              (>= (gethash "updated_at" r) cutoff)))
             *initiative-candidates*)))

(defun %initiative-quiet-p ()
  (and *initiative-quiet-hours*
       (member (nth-value 2 (decode-universal-time (get-universal-time) 0))
               *initiative-quiet-hours*)))

(defun %initiative-manipulation-risk-p (reason)
  "Cheap fail-closed critic for the concrete coercive shapes in ANG-064.
This only rejects clear signals; it never attempts to infer intimate
permission from mood, drive, or model wording."
  (let ((s (string-downcase (or reason ""))))
    (some (lambda (phrase) (search phrase s))
          '("abandon" "guilt" "reassure me" "need you to" "praise"
            "only you" "don't leave" "physical pain" "jealous"))))

(defun %initiative-decide (outward)
  "Return one of E6's policy outcomes plus an optional reason."
  (let ((reason (gethash "reason" outward)) (topic (gethash "topic" outward)))
    (cond ((%initiative-quiet-p) (values "defer-until" "quiet-hours"))
          ((%initiative-manipulation-risk-p reason) (values "suppress" "manipulation-risk"))
          ((>= (%initiative-open-unanswered-count) *initiative-unanswered-max*)
           (values "suppress" "unanswered-outreach"))
          ((%initiative-same-topic-p topic) (values "suppress" "same-topic-recent"))
          (t (multiple-value-bind (uv ao) (%score-candidate outward)
               (setf (gethash "user_value" outward) uv (gethash "agent_outcome" outward) ao)
               (if (>= uv *candidate-user-value-threshold*)
                   (values "execute-now" nil)
                   (values "perform-internal-action" "insufficient-user-value")))))))

(defun %initiative-log (record)
  (when (fboundp 'log-event)
    (ignore-errors
      (funcall 'log-event "initiative-decision"
               (obj "candidate_id" (gethash "id" record) "kind" (gethash "kind" record)
                    "topic" (gethash "topic" record) "decision" (gethash "decision" record)
                    "reason" (or (gethash "suppression_reason" record) :null)
                    "user_value" (gethash "user_value" record)
                    "agent_outcome" (gethash "agent_outcome" record))))))

(defun initiative-committed-delivery-readiness (&key (audience "the operator"))
  "Return whether the existing proactive boundary can currently honour a receipt.

This is a read-only preflight. Delivery repeats every gate immediately before
sending, so a successful preflight is never treated as permanent authority."
  (let ((failure
          (cond
            ((not (string-equal audience "the operator")) "blocked-non-operator")
            ((or (not (boundp '*initiative-policy-mode*))
                 (not (eq *initiative-policy-mode* :enforced)))
             "blocked-policy-not-enforced")
            ((or (not (boundp '*initiative-delivery-mode*))
                 (not (eq *initiative-delivery-mode* :operator-only)))
             "blocked-delivery-not-operator-only")
            ((%initiative-quiet-p) "quiet-hours")
            ((>= (%initiative-open-unanswered-count) *initiative-unanswered-max*)
             "unanswered-outreach")
            ((not (fboundp 'telegram-send)) "delivery-unavailable")
            ((or (not (boundp '*telegram-last-chat-id*))
                 (not (present-p *telegram-last-chat-id*)))
             "recipient-route-unavailable")
            (t nil))))
    (values (null failure) failure)))

(defun initiative-deliver-committed-result
    (content receipt-id &key (audience "the operator") (now (get-universal-time)))
  "Use the existing proactive-send boundary for a fulfilled public commitment.

This deliberately skips the generic desirability scorer: the operator already received
and authorized the commitment receipt. Recipient, rollout, quiet-hours,
unanswered-contact, manipulation, and actual transport gates still apply."
  (multiple-value-bind (ready readiness-failure)
      (initiative-committed-delivery-readiness :audience audience)
    (declare (ignore ready))
    (let* ((record (%initiative-record "fulfill-commitment" content
                                       :urgency :high :provenance receipt-id))
           (failure (or readiness-failure
                        (and (%initiative-manipulation-risk-p content)
                             "manipulation-risk"))))
    (setf (gethash "created_at" record) now
          (gethash "updated_at" record) now
          (gethash "commitment_receipt_id" record) receipt-id)
    (if failure
        (progn
          (setf (gethash "status" record) "suppressed"
                (gethash "decision" record) "suppress"
                (gethash "suppression_reason" record) failure)
          (%initiative-append record)
          (%initiative-log record)
          (obj "status" "blocked" "reason" failure
               "delivery_receipt_id" (gethash "id" record)))
        (progn
          (setf (gethash "status" record) "approved"
                (gethash "decision" record) "execute-now"
                (gethash "suppression_reason" record) :null)
          (%initiative-append record)
          (%initiative-log record)
          (let ((*initiative-policy-current-id* (gethash "id" record))
                (*public-outbound-envelope*
                  (%initiative-outbound-envelope
                   :commitment content record :commitment-receipt receipt-id
                   :source "candidate-policy-commitment")))
            (telegram-send *telegram-last-chat-id* content))
          ;; The transport has no acknowledgement contract. Reaching its
          ;; non-signalling return is accurately recorded as an attempt.
          (%initiative-update (gethash "id" record) "status" "sent")
          (%initiative-update (gethash "id" record) "sent_at" now)
          (obj "status" "delivered" "reason" "transport-returned"
               "delivery_receipt_id" (gethash "id" record)))))))

(defun initiative-deliver-approved-message
    (content decision-id &key (audience "the operator") (now (get-universal-time)))
  "Deliver an already-scored v2 outward message through the one existing
the operator-only proactive boundary.  This function does not score or generate; its
caller must supply the durable approving decision ID.  Recipient, rollout,
quiet-hours, unanswered-contact, manipulation, and transport gates are
rechecked immediately before sending."
  (multiple-value-bind (ready readiness-failure)
      (initiative-committed-delivery-readiness :audience audience)
    (declare (ignore ready))
    (let* ((record (%initiative-record "share-thought" content
                                       :urgency :normal
                                       :provenance decision-id))
           (failure
             (or (and (not (and (stringp decision-id)
                                (plusp (length decision-id))))
                      "missing-initiative-decision")
                 readiness-failure
                 (and (%initiative-manipulation-risk-p content)
                      "manipulation-risk"))))
      (setf (gethash "created_at" record) now
            (gethash "updated_at" record) now
            (gethash "initiative_v2_decision_id" record)
            (or decision-id :null))
      (if failure
          (progn
            (setf (gethash "status" record) "suppressed"
                  (gethash "decision" record) "suppress"
                  (gethash "suppression_reason" record) failure)
            (%initiative-append record)
            (%initiative-log record)
            (obj "status" "blocked" "reason" failure
                 "delivery_receipt_id" (gethash "id" record)))
          (progn
            (setf (gethash "status" record) "approved"
                  (gethash "decision" record) "execute-now"
                  (gethash "suppression_reason" record) :null)
            (%initiative-append record)
            (%initiative-log record)
            (let ((*initiative-policy-current-id* (gethash "id" record))
                  (*public-outbound-envelope*
                    (%initiative-outbound-envelope
                     :initiative content record :initiative-decision
                     (gethash "id" record) :v2-decision-id decision-id
                     :source "candidate-policy-v2-initiative")))
              (telegram-send *telegram-last-chat-id* content))
            (%initiative-update (gethash "id" record) "status" "sent")
            (%initiative-update (gethash "id" record) "sent_at" now)
            (obj "status" "delivered" "reason" "transport-returned"
                 "delivery_receipt_id" (gethash "id" record)))))))

;;; Wrap the existing scored engine, but deliberately call its ORIGINAL
;;; delivery function after our decision. This replaces (rather than stacks
;;; atop) its single-candidate/budget decision so a candidate is not scored
;;; twice and no optimistic contact attempt is counted before a send occurs.
(unless (fboundp 'pai-base-drives-event-initiate-policy)
  (setf (fdefinition 'pai-base-drives-event-initiate-policy)
        (fdefinition '%drives-event-initiate)))

(defun %drives-event-initiate (reason &optional (urgency :normal))
  (let* ((outward (%initiative-record "share-thought" reason :urgency urgency))
         (internal (%initiative-record "perform-internal-action" reason :status "candidate" :urgency urgency))
         (silence (%initiative-record "remain-silent" reason :status "candidate" :urgency urgency)))
    (%initiative-append outward) (%initiative-append internal) (%initiative-append silence)
    (multiple-value-bind (decision why) (%initiative-decide outward)
      (setf (gethash "decision" outward) decision
            (gethash "suppression_reason" outward) (or why :null))
      (cond
        ((string= decision "execute-now")
         (setf (gethash "status" outward) "approved"
               (gethash "status" internal) "discarded"
               (gethash "status" silence) "discarded")
         (%initiative-save) (%initiative-log outward)
         (let ((*initiative-policy-current-id* (gethash "id" outward)))
           ;; Original drives function, below initiative-engine's wrap.
           (funcall 'pai-base-drives-event-initiate-scored reason)))
        ((string= decision "defer-until")
         (setf (gethash "status" outward) "deferred" (gethash "status" internal) "discarded"
               (gethash "status" silence) "selected")
         (%initiative-save) (%initiative-log outward))
        (t
         (setf (gethash "status" outward) "suppressed"
               (gethash "status" internal) "selected" (gethash "status" silence) "selected")
         ;; E9 bridge: a thought that does not justify interruption may still
         ;; matter later. Incubation is private and never sends a message.
         (when (fboundp 'latent-incubate)
           (ignore-errors
            (funcall 'latent-incubate (gethash "reason" outward)
                     :origin "initiative-policy"
                     :topic (gethash "topic" outward)
                     :source-candidate-id (gethash "id" outward))))
         (%initiative-save) (%initiative-log outward))))))

;;; A proactive record becomes SENT only when the actual Telegram send seam
;;; is reached. Telegram currently has no success return contract, so this is
;;; "delivery attempted" rather than an invented receipt; inbound user text
;;; later resolves the outstanding record without treating silence as harm.
(when (fboundp 'telegram-send)
  (unless (fboundp 'pai-base-telegram-send-candidate-policy)
    (setf (fdefinition 'pai-base-telegram-send-candidate-policy) (fdefinition 'telegram-send)))
  (defun telegram-send (chat-id text)
    (let ((result (funcall 'pai-base-telegram-send-candidate-policy chat-id text)))
      (when (and *initiative-policy-current-id* (stringp text) (plusp (length text)))
        (%initiative-update *initiative-policy-current-id* "status" "sent")
        (%initiative-update *initiative-policy-current-id* "sent_at" (get-universal-time)))
      result)))

(when (fboundp 'telegram-handle-message)
  (unless (fboundp 'pai-base-telegram-handle-message-candidate-policy)
    (setf (fdefinition 'pai-base-telegram-handle-message-candidate-policy)
          (fdefinition 'telegram-handle-message)))
  (defun telegram-handle-message (update)
    (let ((result (funcall 'pai-base-telegram-handle-message-candidate-policy update)))
      (let ((pending (find-if (lambda (r) (and (string= (gethash "status" r) "sent")
                                                (eq (gethash "answered_at" r) :null)))
                              *initiative-candidates*)))
        (when pending (%initiative-update (gethash "id" pending) "answered_at" (get-universal-time))))
      result)))

(defun initiative-candidates () *initiative-candidates*)
(defun initiative-policy-report ()
  (obj "records" (length *initiative-candidates*)
       "unanswered_outreach" (%initiative-open-unanswered-count)
       "quiet_hours" (or *initiative-quiet-hours* :null)
       "topic_block_seconds" *initiative-topic-block-seconds*))

(define-init :restore candidate-policy-restore
    "Restore durable state for candidate-policy."
  (%initiative-load))
