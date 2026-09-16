;;;; public-outbound-gateway.lisp -- observe-only audit seam for public sends.

(in-package :agent)

(export '(make-public-outbound-envelope with-public-outbound-envelope
          public-outbound-observe-presentation
          public-outbound-gateway-report public-outbound-gateway-records
          public-outbound-audit-observer-report))

(defvar *public-outbound-gateway-mode* :observe)
(defvar *public-outbound-envelope* nil)
(defvar *current-causing-event-id* nil)
(defvar *public-tool-call-id* nil)
(defvar *public-tool-result-id* nil)
(defvar *public-tool-call-event-id* nil)
(defvar *public-outbound-records* nil)
(defparameter *public-outbound-record-cap* 500)
(defparameter *public-outbound-audit-file* #P"/agent/state/public-outbound-audit.json")
(defvar *public-outbound-lock* (bt:make-lock "public-outbound-gateway"))
(defvar *public-outbound-private-debug-content-p* nil)
(defvar *public-outbound-audit-observer-count* 0)
(defvar *public-outbound-audit-observer-last-envelope-id* nil)
(defvar *public-outbound-installed-telegram-wrapper* nil)
(defvar *public-outbound-installed-web-wrapper* nil)

(defun %public-outbound-id ()
  (format nil "outbound-~d-~8,'0x" (get-universal-time) (random #x100000000)))

(defun %public-outbound-string-vector (value)
  (coerce (remove-if-not #'stringp
                         (cond ((null value) nil)
                               ((vectorp value) (coerce value 'list))
                               ((listp value) value)
                               (t (list value))))
          'vector))

(defun %public-outbound-merge-ids (&rest values)
  (%public-outbound-string-vector
   (remove-duplicates
    (mapcan (lambda (value)
              (cond ((null value) nil)
                    ((vectorp value) (coerce value 'list))
                    ((listp value) value)
                    (t (list value))))
            values)
    :test #'string=)))

(defun %public-outbound-content-sha256 (content)
  (when (stringp content)
    (handler-case
        (progn
          (unless (find-package :ironclad)
            (ql:quickload '(:ironclad :babel) :silent t))
          (let* ((octets (funcall (intern "STRING-TO-OCTETS" :babel)
                                  content :encoding :utf-8))
                 (digest (funcall (intern "DIGEST-SEQUENCE" :ironclad)
                                  :sha256 octets)))
            (string-downcase
             (funcall (intern "BYTE-ARRAY-TO-HEX-STRING" :ironclad) digest))))
      (error () nil))))

(defun make-public-outbound-envelope
    (&key kind (channel "telegram") (audience "the operator") content
          source-event-ids causal-event-ids authorization-kind authorization-id
          legacy-authorization v2-decision source expires-at dedupe-key
          public-act-id tool-call-id tool-result-id)
  (let ((id (%public-outbound-id)))
    (obj "schema_version" 2 "id" id
       "kind" (string-downcase (string (or kind :unclassified)))
       "channel" channel "audience" audience
       "content" (if *public-outbound-private-debug-content-p*
                     (or content :null) :null)
       "content_length" (if (stringp content) (length content) :null)
       "content_sha256" (or (%public-outbound-content-sha256 content) :null)
       "source_event_ids" (%public-outbound-string-vector source-event-ids)
       "causal_event_ids" (%public-outbound-string-vector causal-event-ids)
       "authorization_kind"
       (if authorization-kind (string-downcase (string authorization-kind)) :null)
       "authorization_id" (or authorization-id :null)
       "legacy_authorization" (or legacy-authorization :null)
       "v2_decision" (or v2-decision :null)
       "tool_call_id" (or tool-call-id :null)
       "tool_result_id" (or tool-result-id :null)
       "source" (or source "inferred-at-transport")
       "created_at" (get-universal-time)
       "expires_at" (or expires-at :null)
       "dedupe_key" (or dedupe-key :null)
       "canonical_public_act_id" (or public-act-id dedupe-key id))))

(defmacro with-public-outbound-envelope ((envelope) &body body)
  `(let ((*public-outbound-envelope* ,envelope)) ,@body))

(defun %public-outbound-find-candidate (id)
  (and id (boundp '*initiative-candidates*)
       (find id *initiative-candidates* :test #'string=
             :key (lambda (row) (gethash "id" row)))))

(defun %public-outbound-infer-envelope (text)
  (let* ((candidate-id (and (boundp '*initiative-policy-current-id*)
                            *initiative-policy-current-id*))
         (candidate (%public-outbound-find-candidate candidate-id))
         (candidate-kind (and candidate (gethash "kind" candidate)))
         (kind (cond ((string= (or candidate-kind "") "fulfill-commitment")
                      :commitment)
                     (candidate-id :initiative)
                     (t :unclassified))))
    (make-public-outbound-envelope
     :kind kind :content text
     :authorization-kind (and candidate-id
                              (if (eq kind :commitment)
                                  :commitment-receipt :initiative-decision))
     :authorization-id candidate-id)))

(defun %public-outbound-counterfactual (envelope)
  (let* ((kind (gethash "kind" envelope))
         (authorization-id (gethash "authorization_id" envelope))
         (known-kinds '("reply" "initiative" "commitment" "scheduled"
                        "system-alert" "tool-result"))
         (requires-proof (member kind known-kinds :test #'string=))
         (has-proof (and (stringp authorization-id)
                         (plusp (length authorization-id))))
         (tool-proof
           (or (not (string= kind "tool-result"))
               (let ((call-id (gethash "tool_call_id" envelope))
                     (result-id (gethash "tool_result_id" envelope)))
                 (and (stringp call-id) (plusp (length call-id))
                      (stringp result-id) (plusp (length result-id)))))))
    (cond ((string= kind "unclassified")
           (values "would-withhold" "unclassified-publication-kind"))
          ((not (member kind known-kinds :test #'string=))
           (values "would-withhold" "unsupported-publication-kind"))
          ((and requires-proof (not has-proof))
           (values "would-withhold" "missing-authorization-proof"))
          ((not tool-proof)
           (values "would-withhold" "missing-tool-correlation"))
          (t (values "would-permit" "typed-envelope-valid")))))

(defun %public-outbound-save ()
  (handler-case
      (progn
        (ensure-directories-exist *public-outbound-audit-file*)
        (let* ((tmp (make-pathname :name "public-outbound-audit-tmp" :type "json"
                                   :defaults *public-outbound-audit-file*))
               (content
                 (concatenate
                  'string
                  (shasht:write-json
                   (coerce *public-outbound-records* 'vector) nil)
                  (string #\Newline))))
          (with-open-file (out tmp :direction :output :if-exists :supersede
                                   :if-does-not-exist :create :external-format :utf-8)
            (write-string content out)
            (finish-output out))
          (multiple-value-prog1
              (uiop:rename-file-overwriting-target
               tmp *public-outbound-audit-file*)
            (when (fboundp 'log-projection-state)
              (ignore-errors
                (funcall 'log-projection-state
                         "public-outbound-audit"
                         *public-outbound-audit-file* content))))))
    (error () nil)))

(defun %public-outbound-load ()
  "Restore historical schema-v1/v2 audit rows without reclassification."
  (when (and (null *public-outbound-records*)
             (probe-file *public-outbound-audit-file*))
    (handler-case
        (setf *public-outbound-records*
              (let ((rows (coerce
                           (shasht:read-json
                            (uiop:read-file-string *public-outbound-audit-file*))
                           'list)))
                (subseq rows 0 (min (length rows)
                                    *public-outbound-record-cap*))))
      (error (condition)
        (format t "~&[public-outbound] historical audit unavailable: ~a~%"
                condition))))
  *public-outbound-records*)

(defun %public-outbound-record (envelope counterfactual reason effective status)
  (let* ((effective-permit (string= effective "legacy-permit"))
         (counterfactual-permit (string= counterfactual "would-permit"))
         (record (obj "schema_version" 2
                     "recorded_at" (get-universal-time)
                     "envelope" envelope
                     "mode" "observe"
                     "counterfactual_decision" counterfactual
                     "counterfactual_reason" reason
                     "effective_decision" effective
                     "decision_agreement"
                     (if (and (member counterfactual
                                      '("would-permit" "would-withhold")
                                      :test #'string=)
                              (eq effective-permit counterfactual-permit))
                         "agree" "disagree")
                     "transport_attempt_id" (%public-outbound-id)
                     "transport_status" status
                     "canonical_public_act_id"
                     (gethash "canonical_public_act_id" envelope))))
    (bt:with-lock-held (*public-outbound-lock*)
      (push record *public-outbound-records*)
      (when (> (length *public-outbound-records*) *public-outbound-record-cap*)
        (setf *public-outbound-records*
              (subseq *public-outbound-records* 0 *public-outbound-record-cap*)))
      (%public-outbound-save))
    (when (fboundp 'runtime-observer-emit)
      (runtime-observer-emit "public-outbound-evaluated" record)
      ;; Records are created only after transport returned or errored, so this
      ;; second lifecycle signal truthfully denotes completed observation. It
      ;; grants no authority and does not cause another send.
      (runtime-observer-emit "public-outbound-completed" record))
    record))

(defun %public-outbound-audit-observer (record)
  "Required content-free consumer proving emitted records are populated."
  (let* ((envelope (and (hash-table-p record) (gethash "envelope" record)))
         (id (and (hash-table-p envelope) (gethash "id" envelope)))
         (kind (and (hash-table-p envelope) (gethash "kind" envelope))))
    (unless (and (stringp id) (plusp (length id))
                 (stringp kind) (plusp (length kind))
                 (gethash "canonical_public_act_id" record))
      (error "Outbound observer received an incomplete record"))
    (incf *public-outbound-audit-observer-count*)
    (setf *public-outbound-audit-observer-last-envelope-id* id)
    t))

(defun public-outbound-audit-observer-report ()
  (obj "consumer" "public-outbound-audit-observer"
       "capability" "observe"
       "consumed" *public-outbound-audit-observer-count*
       "last_envelope_id"
       (or *public-outbound-audit-observer-last-envelope-id* :null)))

(define-init :restore public-outbound-gateway-restore
    "Restore durable state for public-outbound-gateway."
  (%public-outbound-load))

(when (fboundp 'runtime-observer-register)
  (runtime-observer-register "public-outbound-evaluated"
                             "public-outbound-audit-observer"
                             #'%public-outbound-audit-observer
                             :capability :observe :required t))

(when (and (fboundp 'runtime-observer-assert)
           (not (runtime-observer-assert)))
  (error "Required public outbound observer is miswired"))

(defun public-outbound-gateway-records () (copy-list *public-outbound-records*))

(defun public-outbound-gateway-report ()
  (let ((unclassified 0) (would-withhold 0))
    (dolist (record *public-outbound-records*)
      (when (string= "unclassified"
                     (gethash "kind" (gethash "envelope" record)))
        (incf unclassified))
      (when (string= "would-withhold"
                     (gethash "counterfactual_decision" record))
        (incf would-withhold)))
    (obj "schema_version" 2 "mode" "observe"
         "authority" "legacy-call-sites"
         "records" (length *public-outbound-records*)
         "unclassified" unclassified "would_withhold" would-withhold
         "record_cap" *public-outbound-record-cap*
         "consumer" (public-outbound-audit-observer-report))))

(defun public-outbound-observe-presentation (envelope status)
  "Observe a non-Telegram public presentation after its legacy path returned."
  (multiple-value-bind (counterfactual reason)
      (handler-case (%public-outbound-counterfactual envelope)
        (error () (values "evaluation-error" "gateway-evaluation-error")))
    (ignore-errors
      (%public-outbound-record envelope counterfactual reason
                               "legacy-permit" status))))

;; This is deliberately the final Telegram wrapper in the boot chain. In
;; observe mode the pre-existing call-site decision remains authoritative.
(when (fboundp 'telegram-send)
  (unless (fboundp 'pai-base-telegram-send-public-outbound)
    (setf (fdefinition 'pai-base-telegram-send-public-outbound)
          (fdefinition 'telegram-send)))
  (defun telegram-send (chat-id text)
    (let ((envelope (or *public-outbound-envelope*
                        (%public-outbound-infer-envelope text))))
      (multiple-value-bind (counterfactual reason)
          (handler-case (%public-outbound-counterfactual envelope)
            (error () (values "evaluation-error" "gateway-evaluation-error")))
        ;; Audit failure cannot create a new suppression or a second send.
        (handler-case
            (let ((result (funcall 'pai-base-telegram-send-public-outbound
                                   chat-id text)))
              (ignore-errors
                (%public-outbound-record envelope counterfactual reason
                                         "legacy-permit" "transport-returned"))
              result)
          (error (condition)
            (ignore-errors
              (%public-outbound-record envelope counterfactual reason
                                       "legacy-permit" "transport-error"))
            (error condition))))))
  (setf *public-outbound-installed-telegram-wrapper* (fdefinition 'telegram-send)))

;; Web is a public presentation seam, not a Telegram transport. Observe only
;; final/tool/error publications that already passed the legacy web path.
(when (fboundp '%v2-broadcast)
  (unless (fboundp 'pai-base-v2-broadcast-public-outbound)
    (setf (fdefinition 'pai-base-v2-broadcast-public-outbound)
          (fdefinition '%v2-broadcast)))
  (defun %v2-broadcast (type data &rest correlation-keys)
    (let ((result (apply 'pai-base-v2-broadcast-public-outbound
                         type data correlation-keys)))
      (when (member type '("final" "thinking" "tool" "image" "error")
                    :test #'string=)
        (let* ((kind (cond ((member type '("tool" "image") :test #'string=)
                            :tool-result)
                           ((string= type "error") :system-alert)
                           (t :reply)))
               (base *public-outbound-envelope*)
               (authorization-id
                 (or (and (eq kind :tool-result) *public-tool-result-id*)
                     (and base (gethash "authorization_id" base))
                     (and (boundp '*current-causing-event-id*)
                          (format nil "~a" *current-causing-event-id*))))
               (tool-causal-ids
                 (and (eq kind :tool-result)
                      (remove nil
                              (list
                               (and *public-tool-call-id*
                                    (format nil "tool-call:~a"
                                            *public-tool-call-id*))
                               *public-tool-result-id*
                               (and *public-tool-call-event-id*
                                    (format nil "event:~a"
                                            *public-tool-call-event-id*))))))
               (envelope
                 (make-public-outbound-envelope
                  :kind kind :channel "web" :content
                  (if (stringp data) data nil)
                  :source-event-ids (and base (gethash "source_event_ids" base))
                  :causal-event-ids
                  (%public-outbound-merge-ids
                   (and base (gethash "causal_event_ids" base))
                   tool-causal-ids)
                  :authorization-kind
                  (or (and (eq kind :tool-result) :tool-result)
                      (and base (gethash "authorization_kind" base))
                      :inbound-request)
                  :authorization-id authorization-id
                  :legacy-authorization
                  (and base (gethash "legacy_authorization" base))
                  :v2-decision (and base (gethash "v2_decision" base))
                  :tool-call-id (and (eq kind :tool-result)
                                     *public-tool-call-id*)
                  :tool-result-id (and (eq kind :tool-result)
                                       *public-tool-result-id*)
                  :source "web-terminal-publication"
                  :dedupe-key
                  (format nil "~a:~a"
                          (or (and base (gethash "dedupe_key" base))
                              authorization-id "web-publication") type))))
          (public-outbound-observe-presentation envelope "presentation-returned")))
      result))
  (setf *public-outbound-installed-web-wrapper* (fdefinition '%v2-broadcast)))

;;; --- the event log's presentation port -----------------------------------
;;; EVENT-LOG.LISP observes that a terminal reply was presented. It used to
;;; build the envelope for that itself, which meant the authority layer had to
;;; know this layer's envelope constructor and its observation entry point --
;;; two of the five hard back-edges out of the kernel. It now reports three
;;; facts through *PUBLIC-PRESENTATION-OBSERVER* and this file decides what an
;;; envelope for them looks like, which is where that decision belonged.

(defun %public-outbound-observe-channel-presentation (channel reply user-event-id)
  "Record a public presentation that a non-transport channel already returned."
  (let ((envelope
          (make-public-outbound-envelope
           :kind :reply :channel channel :content reply
           :source-event-ids (list (format nil "event:~a" user-event-id))
           :causal-event-ids (list (format nil "event:~a" user-event-id))
           :authorization-kind :inbound-user-event
           :authorization-id (format nil "event:~a" user-event-id)
           :source (format nil "~a-auto-turn" channel)
           :dedupe-key (format nil "~a-reply:~a" channel user-event-id))))
    (public-outbound-observe-presentation envelope "presentation-returned")))

(define-init :install public-outbound-presentation-port
    "Register the publication layer as the event log's presentation observer.
Registered at :INSTALL because turns can begin as soon as :START runs, and an
unset port means the presentation is simply not audited."
  (setf *public-presentation-observer*
        #'%public-outbound-observe-channel-presentation)
  t)
