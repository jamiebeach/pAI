;;;; reciprocity-canary.lisp -- bounded bridge from grounded initiative to
;;;; The operator-only unsolicited delivery.
;;;;
;;;; This module never generates or scores content.  It consumes the durable
;;;; decision returned by initiative-policy for an already-authored,
;;;; evidence-backed public draft.  Shadow mode records what would have been
;;;; sent.  The operator-only mode may use the existing candidate-policy transport
;;;; boundary, but only after every independent rollout and contact gate is
;;;; satisfied again.

(in-package :agent)

(export '(reciprocity-canary-consider-observation
          reciprocity-canary-observe-reply reciprocity-canary-records
          reciprocity-canary-snapshot
          reciprocity-canary-report reciprocity-canary-save
          reciprocity-canary-load))

(defparameter *reciprocity-canary-file*
  (pathname (or (uiop:getenv "PAI_RECIPROCITY_CANARY_FILE")
                "/agent/state/reciprocity-canary.json")))
(defparameter *reciprocity-canary-max-records* 200)
(defparameter *reciprocity-canary-max-per-24-hours* 2)
(defparameter *reciprocity-canary-min-spacing-seconds* (* 6 60 60))
(defparameter *reciprocity-canary-topic-block-seconds* (* 24 60 60))
(defparameter *reciprocity-canary-max-content-characters* 1000)
(defparameter *reciprocity-canary-sources*
  '("explore-development" "grounded-project"))
(defparameter *reciprocity-canary-generic-trigger-types*
  '("drive-threshold" "curiosity-finding" "contradiction"))
(defparameter *reciprocity-canary-generic-phrases*
  '("a pull toward connection" "a pull toward appreciation"
    "just became strong enough to notice"))
(defparameter *reciprocity-canary-deferred-intention-phrases*
  '("give me a minute" "give me a moment" "let me think"
    "i'll think about" "i will think about" "i'll come back with"
    "i will come back with" "i'll get back to you" "i will get back to you"))

(defvar *reciprocity-canary-records* nil "Newest-first durable lifecycle.")
(defvar *reciprocity-canary-lock* (bt:make-lock "reciprocity-canary"))
(defvar *reciprocity-canary-delivery-fn* nil
  "Optional test seam. NIL uses INITIATIVE-DELIVER-APPROVED-MESSAGE.")
(defvar *reciprocity-canary-load-status* "available")
(defvar *reciprocity-canary-pruned-count* 0)
(defvar *reciprocity-canary-public-authoring-stage* :missing
  "V6 promotion blocker. No production setter exists in this slice.")

(defun %reciprocity-list (value)
  (cond ((null value) nil)
        ((vectorp value) (coerce value 'list))
        ((listp value) value)
        (t (list value))))

(defun %reciprocity-trim (text)
  (string-trim '(#\Space #\Tab #\Newline #\Return) (or text "")))

(defun %reciprocity-id ()
  (format nil "recip-~a-~6,'0d" (get-universal-time) (random 1000000)))

(defun %reciprocity-topic (topic content)
  (let ((trimmed (%reciprocity-trim topic)))
    (if (plusp (length trimmed))
        (string-downcase trimmed)
        (let* ((words (remove-if
                       (lambda (word) (< (length word) 4))
                       (uiop:split-string (string-downcase content)
                                          :separator '(#\Space #\Tab #\Newline
                                                       #\. #\, #\! #\? #\: #\;))))
               (picked (subseq words 0 (min 8 (length words)))))
          (format nil "~{~a~^-~}" picked)))))

(defun reciprocity-canary-save ()
  (ensure-directories-exist *reciprocity-canary-file*)
  (let ((tmp (make-pathname :name "reciprocity-canary-tmp" :type "json"
                            :defaults *reciprocity-canary-file*)))
    (with-open-file (out tmp :direction :output :if-exists :supersede
                             :if-does-not-exist :create :external-format :utf-8)
      (write-string
       (shasht:write-json (coerce *reciprocity-canary-records* 'vector) nil)
       out)
      (terpri out)
      (finish-output out))
    (uiop:rename-file-overwriting-target tmp *reciprocity-canary-file*))
  t)

(defun reciprocity-canary-load ()
  (handler-case
      (if (probe-file *reciprocity-canary-file*)
          (let ((records
                  (with-open-file (in *reciprocity-canary-file*
                                      :external-format :utf-8)
                    (coerce (shasht:read-json in) 'list)))
                (changed nil))
            (dolist (record records)
              (multiple-value-bind (normalized row-changed)
                  (candidate-representation-normalize-record record)
                (declare (ignore normalized))
                (when row-changed (setf changed t))))
            (setf *reciprocity-canary-records* records
                  *reciprocity-canary-load-status* "available")
            (when changed (reciprocity-canary-save)))
          (setf *reciprocity-canary-records* nil
                *reciprocity-canary-load-status* "available-empty"))
    (error (condition)
      (format t "~&[reciprocity-canary] load failed; starting empty: ~a~%"
              condition)
      (setf *reciprocity-canary-records* nil
            *reciprocity-canary-load-status* "unavailable")
      nil)))

(defun reciprocity-canary-records (&key limit)
  (let ((copy (copy-list *reciprocity-canary-records*)))
    (if limit (subseq copy 0 (min limit (length copy))) copy)))

(defun reciprocity-canary-snapshot ()
  "Read-only V0/V1 source status. Old ledgers have no historical prune count."
  (obj "status" (if (string= *reciprocity-canary-load-status* "unavailable")
                    "unavailable" "available")
       "reason" (if (string= *reciprocity-canary-load-status* "unavailable")
                    "reciprocity-ledger-unreadable" :null)
       "records" (coerce (reciprocity-canary-records) 'vector)
       "record_cap" *reciprocity-canary-max-records*
       "at_cap" (>= (length *reciprocity-canary-records*)
                     *reciprocity-canary-max-records*)
       "pruned_count" (if (plusp *reciprocity-canary-pruned-count*)
                          *reciprocity-canary-pruned-count* :null)))

(defun %reciprocity-log (record transition)
  (when (fboundp 'log-event)
    (ignore-errors
      (funcall 'log-event "reciprocity-canary-transition"
               (obj "record_id" (gethash "id" record)
                    "decision_id" (gethash "initiative_decision_id" record)
                    "source" (gethash "source" record)
                    "topic" (gethash "topic" record)
                    "transition" transition
                    "status" (gethash "status" record)
                    "reason" (gethash "reason" record))))))

(defun %reciprocity-append-locked (record)
  (push record *reciprocity-canary-records*)
  (when (> (length *reciprocity-canary-records*)
           *reciprocity-canary-max-records*)
    (incf *reciprocity-canary-pruned-count*
          (- (length *reciprocity-canary-records*)
             *reciprocity-canary-max-records*))
    (setf *reciprocity-canary-records*
          (subseq *reciprocity-canary-records* 0
                  *reciprocity-canary-max-records*)))
  (reciprocity-canary-save)
  record)

(defun %reciprocity-update (id status reason now &key delivery-receipt-id
                                                        answered-at)
  (bt:with-lock-held (*reciprocity-canary-lock*)
    (let ((record (find id *reciprocity-canary-records*
                        :key (lambda (row) (gethash "id" row))
                        :test #'string=)))
      (when record
        (setf (gethash "status" record) status
              (gethash "reason" record) reason
              (gethash "updated_at" record) now)
        (when delivery-receipt-id
          (setf (gethash "delivery_receipt_id" record) delivery-receipt-id))
        (when answered-at
          (setf (gethash "answered_at" record) answered-at))
        (reciprocity-canary-save)
        (%reciprocity-log record status))
      record)))

(defun %reciprocity-decision-content (decision)
  (let* ((options (%reciprocity-list (gethash "options" decision)))
         (selected-id (gethash "selected_candidate_id" decision))
         (selected (find selected-id options
                         :key (lambda (row) (gethash "id" row))
                         :test #'string=)))
    (and selected (gethash "proposed_content" selected))))

(defun %reciprocity-generic-p (content trigger-type)
  (let ((lower (string-downcase content)))
    (or (member trigger-type *reciprocity-canary-generic-trigger-types*
                :test #'string=)
        (some (lambda (phrase) (search phrase lower))
              *reciprocity-canary-generic-phrases*))))

(defun %reciprocity-contract-violations (content)
  (if (and (fboundp 'build-publication-contract)
           (fboundp 'publication-contract-violations))
      (ignore-errors
        (funcall 'publication-contract-violations content
                 (funcall 'build-publication-contract "")))
      nil))

(defun %reciprocity-validation-failure
    (source content evidence decision artifact-class)
  (let* ((decision-content (and (hash-table-p decision)
                                (%reciprocity-decision-content decision)))
         (trigger-type (and (hash-table-p decision)
                            (gethash "trigger_type" decision "")))
         (gates (and (hash-table-p decision)
                     (%reciprocity-list (gethash "gates" decision))))
         (evidence-ids
           (remove nil
                   (append
                    (mapcar (lambda (node)
                              (and (hash-table-p node) (gethash "id" node)))
                            (%reciprocity-list evidence))
                    (and (hash-table-p decision)
                         (%reciprocity-list
                          (gethash "evidence_node_ids" decision)))))))
    (cond
      ((not (member source *reciprocity-canary-sources* :test #'string=))
       "source-not-allowed")
      ((not (hash-table-p decision)) "missing-v2-decision")
      ((not (and (stringp (gethash "id" decision))
                 (plusp (length (gethash "id" decision)))))
       "missing-v2-decision-id")
      ((not (string= artifact-class "rendered-draft"))
       "artifact-not-rendered-draft")
      ((not (string= (gethash "selected_action_type" decision "")
                     "outward-message"))
       "v2-withheld")
      ((not (string= (gethash "result" decision "")
                     "approved-not-delivered"))
       "v2-not-approved")
      (gates "v2-hard-gate")
      ((null evidence-ids) "missing-grounded-evidence")
      ((or (zerop (length content))
           (< (length content) 24))
       "content-not-substantive")
      ((> (length content) *reciprocity-canary-max-content-characters*)
       "content-too-long")
      ((or (null decision-content) (not (string= content decision-content)))
       "content-differs-from-scored-draft")
      ((%reciprocity-generic-p content trigger-type) "generic-drive-signal")
      ((some (lambda (phrase)
               (search phrase (string-downcase content)))
             *reciprocity-canary-deferred-intention-phrases*)
       "commitment-owned-by-near-term-intentions")
      ((%reciprocity-contract-violations content)
       "publication-contract-violation")
      (t nil))))

(defun %reciprocity-live-gate-failure ()
  (cond
    ((not (eq *reciprocity-canary-public-authoring-stage* :qualified))
     "public-authoring-stage-missing")
    ((or (not (boundp '*autonomous-write-mode*))
         (not (eq *autonomous-write-mode* :normal)))
     "autonomous-writing-not-normal")
    ((or (not (boundp '*initiative-policy-mode*))
         (not (eq *initiative-policy-mode* :enforced)))
     "initiative-policy-not-enforced")
    ((or (not (boundp '*initiative-delivery-mode*))
         (not (eq *initiative-delivery-mode* :operator-only)))
     "initiative-delivery-not-operator-only")
    (t nil)))

(defun %reciprocity-open-unanswered-p ()
  (find-if (lambda (record)
             (member (gethash "status" record)
                     '("delivery-pending" "sent") :test #'string=))
           *reciprocity-canary-records*))

(defun %reciprocity-budget-failure (topic now)
  (let* ((day-cutoff (- now (* 24 60 60)))
         (topic-cutoff (- now *reciprocity-canary-topic-block-seconds*))
         (sent (remove-if-not
                (lambda (record)
                  (and (member (gethash "status" record)
                               '("delivery-pending" "sent" "answered")
                               :test #'string=)
                       (>= (gethash "created_at" record 0) day-cutoff)))
                *reciprocity-canary-records*))
         (latest (first (sort (copy-list sent) #'>
                              :key (lambda (record)
                                     (gethash "created_at" record 0))))))
    (cond
      ((%reciprocity-open-unanswered-p) "unanswered-outreach")
      ((>= (length sent) *reciprocity-canary-max-per-24-hours*)
       "daily-contact-budget")
      ((and latest
            (< (- now (gethash "created_at" latest 0))
               *reciprocity-canary-min-spacing-seconds*))
       "minimum-spacing")
      ((find-if (lambda (record)
                  (and (string= topic (gethash "topic" record ""))
                       (member (gethash "status" record)
                               '("would-send" "delivery-pending" "sent" "answered")
                               :test #'string=)
                       (>= (gethash "created_at" record 0) topic-cutoff)))
                *reciprocity-canary-records*)
       "same-topic-recent")
      (t nil))))

(defun %reciprocity-deliver (content decision-id now)
  (cond
    (*reciprocity-canary-delivery-fn*
     (funcall *reciprocity-canary-delivery-fn* content decision-id now))
    ((fboundp 'initiative-deliver-approved-message)
     (funcall 'initiative-deliver-approved-message
              content decision-id :audience "the operator" :now now))
    (t (obj "status" "blocked" "reason" "delivery-function-unavailable"))))

(defun reciprocity-canary-consider-observation
    (source content evidence decision
     &key source-id artifact-class generation-contract
       (now (get-universal-time)))
  "Record one typed artifact. Only an explicitly rendered draft can ever
approach live gates. OFF is a true rollback with no persistence."
  (unless (and (boundp '*reciprocity-canary-mode*)
               (member *reciprocity-canary-mode* '(:shadow :operator-only)))
    (return-from reciprocity-canary-consider-observation nil))
  (let* ((draft (%reciprocity-trim content))
         (class (if (member artifact-class
                            '("internal-stance" "rendered-draft")
                            :test #'string=)
                    artifact-class
                    "internal-stance"))
         (contract (if (and (stringp generation-contract)
                            (plusp (length generation-contract)))
                       generation-contract
                       (if (string= source "explore-development")
                           "explore-stance-v1" "legacy-unversioned")))
         (decision-id (and (hash-table-p decision) (gethash "id" decision)))
         (topic (%reciprocity-topic
                 (and (hash-table-p decision) (gethash "topic" decision ""))
                 draft))
         (validation-failure
           (%reciprocity-validation-failure source draft evidence decision class))
         (record nil)
         (deliver-p nil))
    (bt:with-lock-held (*reciprocity-canary-lock*)
      (let ((existing (and decision-id
                           (find decision-id *reciprocity-canary-records*
                                 :key (lambda (row)
                                        (gethash "initiative_decision_id" row))
                                 :test #'string=))))
        (when existing
          (return-from reciprocity-canary-consider-observation existing)))
      (let* ((live-p (eq *reciprocity-canary-mode* :operator-only))
             (failure (or validation-failure
                          (and live-p (%reciprocity-live-gate-failure))
                          (and (null validation-failure)
                               (%reciprocity-budget-failure topic now))))
             (status (cond (failure (if (and live-p (null validation-failure))
                                       "delivery-blocked" "withheld"))
                           (live-p "delivery-pending")
                           (t "would-send"))))
        (setf record
              (obj "schema_version" 2 "id" (%reciprocity-id)
                   "initiative_decision_id" (or decision-id :null)
                   "source" source "source_id" (or source-id :null)
                   "topic" topic "content" draft
                   "content_preview" (subseq draft 0 (min 280 (length draft)))
                   "artifact_class" class
                   "generation_contract" contract
                   "composition_eligible"
                   (if (string= class "rendered-draft") t nil)
                   "evidence_node_ids"
                   (if (hash-table-p decision)
                       (coerce (%reciprocity-list
                                (gethash "evidence_node_ids" decision)) 'vector)
                       (vector))
                   "status" status "reason" (or failure "eligible")
                   "created_at" now "updated_at" now
                   "sent_at" :null "answered_at" :null
                   "delivery_receipt_id" :null))
        (%reciprocity-append-locked record)
        (%reciprocity-log record status)
        (setf deliver-p (string= status "delivery-pending"))))
    (when deliver-p
      (handler-case
          (let* ((receipt (%reciprocity-deliver draft decision-id now))
                 (delivered (and (hash-table-p receipt)
                                 (string= (gethash "status" receipt "")
                                          "delivered")))
                 (reason (if (hash-table-p receipt)
                             (gethash "reason" receipt "delivery-failed")
                             "malformed-delivery-receipt")))
            (setf record
                  (%reciprocity-update
                   (gethash "id" record)
                   (if delivered "sent" "delivery-blocked") reason now
                   :delivery-receipt-id
                   (and (hash-table-p receipt)
                        (gethash "delivery_receipt_id" receipt))))
            (when delivered
              (setf (gethash "sent_at" record) now)
              (bt:with-lock-held (*reciprocity-canary-lock*)
                (reciprocity-canary-save))))
        (error (condition)
          (setf record
                (%reciprocity-update
                 (gethash "id" record) "delivery-blocked"
                 (format nil "delivery-error/~a"
                         (string-downcase
                          (symbol-name (type-of condition))))
                 now)))))
    record))

(defun %reciprocity-public-reply-p (prompt origin)
  (let ((trimmed (%reciprocity-trim prompt))
        (origin-text (string-downcase (format nil "~a" origin))))
    (and (plusp (length trimmed))
         (not (member origin-text
                      '("initiative" "internal" "tick" "system" "scheduler")
                      :test #'string=))
         (not (and (>= (length trimmed) 7)
                   (string-equal "system:" (subseq trimmed 0 7))))
         (not (and (>= (length trimmed) 8)
                   (string-equal "(system:" (subseq trimmed 0 8)))))))

(defun reciprocity-canary-observe-reply
    (prompt &key (origin "unknown") (now (get-universal-time)))
  "Close the newest delivered canary after a successful public user turn.
The user's text is deliberately not persisted in the canary ledger."
  (when (%reciprocity-public-reply-p prompt origin)
    (let ((open (find-if (lambda (record)
                           (and (string= (gethash "status" record "") "sent")
                                (eq (gethash "answered_at" record :null) :null)))
                         *reciprocity-canary-records*)))
      (when open
        (%reciprocity-update (gethash "id" open) "answered"
                             "public-reply-observed" now
                             :answered-at now)))))

(defun reciprocity-canary-report ()
  (let ((counts (obj))
        (latest (first *reciprocity-canary-records*)))
    (dolist (record *reciprocity-canary-records*)
      (incf (gethash (gethash "status" record "unknown") counts 0)))
    (obj "schema_version" 1
         "mode" (if (boundp '*reciprocity-canary-mode*)
                    (string-downcase (symbol-name *reciprocity-canary-mode*))
                    "off")
         "record_count" (length *reciprocity-canary-records*)
         "status_counts" counts
         "max_per_24_hours" *reciprocity-canary-max-per-24-hours*
         "minimum_spacing_seconds" *reciprocity-canary-min-spacing-seconds*
         "max_unanswered" 1
         "topic_block_seconds" *reciprocity-canary-topic-block-seconds*
         "open_unanswered" (if (%reciprocity-open-unanswered-p) 1 0)
         "delivery_capable"
         (if (and (boundp '*reciprocity-canary-mode*)
                  (eq *reciprocity-canary-mode* :operator-only)
                  (null (%reciprocity-live-gate-failure))) t nil)
         "public_authoring_stage"
         (string-downcase
          (symbol-name *reciprocity-canary-public-authoring-stage*))
         "promotion_blocked"
         (if (eq *reciprocity-canary-public-authoring-stage* :qualified) nil t)
         "promotion_blocker"
         (if (eq *reciprocity-canary-public-authoring-stage* :qualified)
             :null "public-authoring-stage-missing")
         "representation"
         (candidate-representation-report *reciprocity-canary-records*)
         "latest" (or latest :null)
         "records" (coerce (reciprocity-canary-records :limit 40) 'vector))))

(define-init :restore reciprocity-canary-restore
    "Restore durable state for reciprocity-canary."
  (reciprocity-canary-load))
