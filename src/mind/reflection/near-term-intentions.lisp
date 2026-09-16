;;;; near-term-intentions.lisp -- durable conversational commitment receipts.
;;;;
;;;; This is the authoritative owner for short-horizon deferred intentions.
;;;; It stores bounded state and conclusions, never chain-of-thought.  It has no
;;;; timer, thread, public-message, initiative, tool, or direct delivery path.

(in-package :agent)

(declaim (ftype function near-term-intention-report))

(export '(near-term-intention-create
          near-term-intention-active
          near-term-intention-records
          near-term-intention-safe-view
          near-term-intention-events
          near-term-intention-due-p
          near-term-intention-process-due
          near-term-intention-mark-expressed
          near-term-intention-observe-public-reply
          near-term-intention-transition
          near-term-intention-report
          near-term-intention-save
          near-term-intention-load))

(defparameter *near-term-intention-file*
  (pathname (or (uiop:getenv "PAI_NEAR_TERM_INTENTIONS")
                "/agent/state/near-term-intentions.json")))
(defparameter *near-term-intention-min-window-seconds* 180)
(defparameter *near-term-intention-max-window-seconds* 300)
(defparameter *near-term-intention-max-passes* 2)
(defparameter *near-term-intention-max-records* 100)
(defparameter *near-term-intention-states*
  '("seeded" "evolving" "ready" "blocked" "expired" "superseded"
    "expressed" "discarded"))
(defparameter *near-term-intention-terminal-states*
  '("blocked" "expired" "superseded" "expressed" "discarded"))
(defvar *near-term-intention-records* nil)
(defvar *near-term-intention-lock* (bt:make-lock "near-term-intentions"))
(defvar *near-term-intention-evidence-fn* nil)
(defvar *near-term-intention-cognitive-fn* nil)
(defvar *near-term-intention-delivery-fn* nil)

(defun %near-term-intention-list (value)
  (cond ((null value) nil) ((listp value) value)
        ((vectorp value) (coerce value 'list)) (t (list value))))

(defun %near-term-intention-text (value)
  (and (stringp value)
       (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) value)))
         (and (plusp (length trimmed)) trimmed))))

(defun %near-term-intention-id (prefix now)
  (format nil "~a-~a-~6,'0x" prefix now (random #x1000000)))

(defun %near-term-intention-active-p (record)
  (and (hash-table-p record)
       (not (member (gethash "state" record "")
                    *near-term-intention-terminal-states* :test #'string=))))

(defun %near-term-intention-copy (record)
  (let ((copy (obj)))
    (when (hash-table-p record)
      (maphash (lambda (key value) (setf (gethash key copy) value)) record))
    copy))

(defun %near-term-intention-log (type record &optional detail)
  (when (fboundp 'log-event)
    (ignore-errors
      (funcall 'log-event type
               (obj "schema_version" 1
                    "intention_id" (gethash "id" record)
                    "receipt_id" (gethash "commitment_receipt_id" record)
                    "state" (gethash "state" record)
                    "pass_count" (gethash "pass_count" record 0)
                    "detail" (or detail :null))
               :caused-by (let ((ids (%near-term-intention-list
                                       (gethash "origin_event_ids" record))))
                            (or (first ids) nil))))))

(defun %near-term-intention-save-unlocked ()
  (ensure-directories-exist *near-term-intention-file*)
  (let ((tmp (make-pathname :name "near-term-intentions-tmp" :type "json"
                            :defaults *near-term-intention-file*)))
    (with-open-file (out tmp :direction :output :if-exists :supersede
                             :if-does-not-exist :create :external-format :utf-8)
      (shasht:write-json (coerce *near-term-intention-records* 'vector) out)
      (terpri out)
      (finish-output out))
    (uiop:rename-file-overwriting-target tmp *near-term-intention-file*))
  t)

(defun near-term-intention-save ()
  (bt:with-lock-held (*near-term-intention-lock*)
    (%near-term-intention-save-unlocked)))

(defun near-term-intention-load ()
  (handler-case
      (when (probe-file *near-term-intention-file*)
        (let ((loaded (shasht:read-json
                       (uiop:read-file-string *near-term-intention-file*))))
          (setf *near-term-intention-records*
                (%near-term-intention-list loaded))))
    (error (condition)
      (format t "~&[near-term-intentions] load failed; retaining memory state: ~a~%"
              condition)
      nil))
  (near-term-intention-report))

(defun %near-term-intention-expire-unlocked (now)
  (let ((changed nil))
    (dolist (record *near-term-intention-records*)
      (when (and (%near-term-intention-active-p record)
                 (numberp (gethash "response_deadline" record))
                 (<= (gethash "response_deadline" record) now))
        (setf (gethash "state" record) "expired"
              (gethash "updated_at" record) now
              (gethash "latest_transition" record) "deadline-expired"
              (gethash "failure_code" record) "deadline-expired")
        (incf (gethash "version" record 0))
        (%near-term-intention-log "near-term-intention-transition" record
                                  (obj "operation" "expire" "at" now))
        (setf changed t)))
    changed))

(defun near-term-intention-active (&key (now (get-universal-time)))
  (bt:with-lock-held (*near-term-intention-lock*)
    (when (%near-term-intention-expire-unlocked now)
      (%near-term-intention-save-unlocked))
    (let ((record (find-if #'%near-term-intention-active-p
                           *near-term-intention-records*)))
      (and record (%near-term-intention-copy record)))))

(defun near-term-intention-records (&key active-only (now (get-universal-time)))
  (bt:with-lock-held (*near-term-intention-lock*)
    (when (%near-term-intention-expire-unlocked now)
      (%near-term-intention-save-unlocked))
    (mapcar #'%near-term-intention-copy
            (if active-only
                (remove-if-not #'%near-term-intention-active-p
                               *near-term-intention-records*)
                *near-term-intention-records*))))

(defun near-term-intention-create
    (subject aim origin-turn-id origin-event-ids return-window-seconds
     &key (recipient "the operator") (completion-mode :proactive-delivery)
          (now (get-universal-time)))
  "Atomically create one short-horizon receipt. No cognition or delivery occurs."
  (let ((subject-text (%near-term-intention-text subject))
        (aim-text (%near-term-intention-text aim))
        (turn-id (%near-term-intention-text origin-turn-id))
        (event-ids (remove nil (%near-term-intention-list origin-event-ids)))
        (delivery-failure
          (when (and (eq completion-mode :proactive-delivery)
                     (fboundp 'initiative-committed-delivery-readiness))
            (multiple-value-bind (ready reason)
                (funcall 'initiative-committed-delivery-readiness
                         :audience recipient)
              (unless ready (or reason "delivery-not-ready"))))))
    (cond
      ((not (member completion-mode '(:proactive-delivery :manual-observation)))
       (values nil "unsupported-completion-mode"))
      ((or (null subject-text) (null aim-text)) (values nil "missing-subject-or-aim"))
      ((or (null turn-id) (null event-ids)) (values nil "missing-causal-origin"))
      ((or (not (integerp return-window-seconds))
           (< return-window-seconds *near-term-intention-min-window-seconds*)
           (> return-window-seconds *near-term-intention-max-window-seconds*))
       (values nil "unsupported-return-window"))
      ((and (boundp '*autonomous-write-mode*)
            (eq *autonomous-write-mode* :paused))
       (values nil "autonomous-writes-paused"))
      ((and (fboundp 'tick-budget-status)
            ;; The real tick governor's healthy value is :OK. Historical
            ;; isolated fixtures used :NORMAL, so accept both closed values;
            ;; :SOFT, :HARD, NIL and errors remain fail-closed here.
            (not (member (ignore-errors (funcall 'tick-budget-status))
                         '(:ok :normal) :test #'eq)))
       (values nil "autonomy-budget-degraded"))
      ((or (not (boundp '*near-term-intentions-mode*))
           (not (eq *near-term-intentions-mode* :enforced)))
       (values nil "near-term-intentions-not-enforced"))
      ((and (eq completion-mode :proactive-delivery)
            (not (fboundp 'initiative-committed-delivery-readiness)))
       (values nil "delivery-readiness-unavailable"))
      (delivery-failure (values nil delivery-failure))
      (t
       (bt:with-lock-held (*near-term-intention-lock*)
         (when (%near-term-intention-expire-unlocked now)
           (%near-term-intention-save-unlocked))
         (when (find-if #'%near-term-intention-active-p
                        *near-term-intention-records*)
           (return-from near-term-intention-create
             (values nil "active-intention-limit")))
         (let* ((id (%near-term-intention-id "near-term" now))
                (receipt (%near-term-intention-id "near-term-receipt" now))
                (deadline (+ now return-window-seconds))
                (record
                  (obj "schema_version" 1 "id" id
                       "commitment_receipt_id" receipt
                       "subject" subject-text "aim" aim-text
                       "recipient" recipient "origin_turn_id" turn-id
                       "completion_mode"
                       (string-downcase (symbol-name completion-mode))
                       "origin_event_ids" (coerce event-ids 'vector)
                       "evidence_ids" (vector) "state" "seeded"
                       "pass_count" 0 "max_passes" *near-term-intention-max-passes*
                       "next_reconsideration" now
                       "response_deadline" deadline "expires_at" deadline
                       "artifact_summary" :null "failure_code" :null
                       "latest_transition" "receipt-created"
                       "created_at" now "updated_at" now "version" 1)))
           (push record *near-term-intention-records*)
           (when (> (length *near-term-intention-records*)
                    *near-term-intention-max-records*)
             (setf *near-term-intention-records*
                   (subseq *near-term-intention-records* 0
                           *near-term-intention-max-records*)))
           (%near-term-intention-save-unlocked)
           (let ((source-event-id
                   (%near-term-intention-log "near-term-intention-created" record)))
             (values (%near-term-intention-copy record) "created"
                     source-event-id))))))))

(defun near-term-intention-transition (id to-state transition
                                        &key artifact-summary evidence-ids failure-code
                                             (now (get-universal-time)))
  (unless (member to-state *near-term-intention-states* :test #'string=)
    (return-from near-term-intention-transition (values nil "invalid-state")))
  (bt:with-lock-held (*near-term-intention-lock*)
    (let ((record (find id *near-term-intention-records*
                        :key (lambda (row) (gethash "id" row)) :test #'string=)))
      (cond
        ((null record) (values nil "missing-intention"))
        ((and (%near-term-intention-active-p record)
              (string= to-state "ready")
              (null (%near-term-intention-text artifact-summary)))
         (values nil "ready-without-artifact"))
        ((not (%near-term-intention-active-p record))
         (values nil "terminal-state"))
        (t
         (setf (gethash "state" record) to-state
               (gethash "updated_at" record) now
               (gethash "latest_transition" record) transition
               (gethash "failure_code" record) (or failure-code :null))
         (when (%near-term-intention-text artifact-summary)
           (setf (gethash "artifact_summary" record) artifact-summary))
         (let ((ids (remove nil (%near-term-intention-list evidence-ids))))
           (when ids
             (setf (gethash "evidence_ids" record)
                   (coerce (remove-duplicates
                            (append (%near-term-intention-list
                                     (gethash "evidence_ids" record)) ids)
                            :test #'string=) 'vector))))
         (incf (gethash "version" record 0))
         (%near-term-intention-save-unlocked)
         (let ((source-event-id
                 (%near-term-intention-log
                  "near-term-intention-transition" record
                  (obj "operation" transition "at" now))))
           (values (%near-term-intention-copy record) nil source-event-id)))))))

(defun %near-term-intention-due-unlocked (now)
  (find-if (lambda (record)
             (and (%near-term-intention-active-p record)
                   (string= "proactive-delivery"
                            (gethash "completion_mode" record
                                     "proactive-delivery"))
                  (or (string= (gethash "state" record "") "ready")
                      (and (member (gethash "state" record)
                                   '("seeded" "evolving") :test #'string=)
                           (< (gethash "pass_count" record 0)
                              (gethash "max_passes" record
                                       *near-term-intention-max-passes*))))
                  (numberp (gethash "next_reconsideration" record))
                  (<= (gethash "next_reconsideration" record) now)))
           *near-term-intention-records*))

(defun near-term-intention-due-p (&key (now (get-universal-time)))
  (and (boundp '*near-term-intentions-mode*)
       (eq *near-term-intentions-mode* :enforced)
       (bt:with-lock-held (*near-term-intention-lock*)
         (when (%near-term-intention-expire-unlocked now)
           (%near-term-intention-save-unlocked))
         (not (null (%near-term-intention-due-unlocked now))))))

(defun %near-term-intention-evidence (record)
  (if *near-term-intention-evidence-fn*
      (funcall *near-term-intention-evidence-fn* record)
      (if (fboundp 'memory-search)
          (funcall 'memory-search (gethash "subject" record) :k 6
                   :mode :cognitive-evidence)
          nil)))

(defun %near-term-intention-cognitive (record evidence)
  (let ((question (format nil "Aim: ~a. Develop one grounded, specific result for this short-horizon conversational commitment."
                          (gethash "aim" record))))
    (if *near-term-intention-cognitive-fn*
        (funcall *near-term-intention-cognitive-fn* record evidence question)
        (funcall 'cognitive-call "deferred-intention" evidence
                 :question question :topic (gethash "subject" record)
                 :max-words 100 :generation-id (gethash "id" record)))))

(defun %near-term-intention-deliver (record)
  (cond
    (*near-term-intention-delivery-fn*
     (funcall *near-term-intention-delivery-fn* record))
    ((fboundp 'initiative-deliver-committed-result)
     (funcall 'initiative-deliver-committed-result
              (gethash "artifact_summary" record)
              (gethash "commitment_receipt_id" record)
              :audience (gethash "recipient" record "the operator")))
    (t (obj "status" "blocked" "reason" "delivery-seam-unavailable"))))

(defun %near-term-intention-process-ready-delivery (candidate version now)
  (let* ((delivery (%near-term-intention-deliver candidate))
         (status (and (hash-table-p delivery) (gethash "status" delivery "blocked")))
         (receipt (and (hash-table-p delivery)
                       (gethash "delivery_receipt_id" delivery :null)))
         (reason (or (and (hash-table-p delivery) (gethash "reason" delivery))
                     "delivery-failed")))
    (bt:with-lock-held (*near-term-intention-lock*)
      (let ((current (find (gethash "id" candidate) *near-term-intention-records*
                           :key (lambda (row) (gethash "id" row))
                           :test #'string=)))
        (cond
          ((or (null current) (not (%near-term-intention-active-p current))
               (/= (gethash "version" current) version))
           (obj "status" "skipped" "reason" "stale-delivery-version"))
          ((string= status "delivered")
           (setf (gethash "state" current) "expressed"
                 (gethash "delivery_receipt_id" current) receipt
                 (gethash "expression_turn_id" current)
                 (format nil "proactive-delivery:~a" receipt)
                 (gethash "latest_transition" current) "proactive-delivery-attempted"
                 (gethash "failure_code" current) :null
                 (gethash "updated_at" current) now)
           (incf (gethash "version" current 0))
           (%near-term-intention-save-unlocked)
           (%near-term-intention-log "near-term-intention-transition" current
                                     (obj "operation" "proactive-delivery-attempted"
                                          "delivery_receipt_id" receipt "at" now))
           (obj "status" "delivered" "intention_id" (gethash "id" current)
                "delivery_receipt_id" receipt))
          (t
           (setf (gethash "state" current) "blocked"
                 (gethash "latest_transition" current) "delivery-blocked"
                 (gethash "failure_code" current) reason
                 (gethash "delivery_receipt_id" current) receipt
                 (gethash "updated_at" current) now)
           (incf (gethash "version" current 0))
           (%near-term-intention-save-unlocked)
           (%near-term-intention-log "near-term-intention-transition" current
                                     (obj "operation" "delivery-blocked"
                                          "reason" reason "at" now))
           (obj "status" "blocked" "reason" reason
                "intention_id" (gethash "id" current))))))))

(defun near-term-intention-process-due (&key (now (get-universal-time)))
  "Process at most one due item, only in :ENFORCED mode.

The model call occurs outside the state lock. Version checking prevents a
stale result from overwriting a concurrent cancellation or replacement."
  (unless (and (boundp '*near-term-intentions-mode*)
               (eq *near-term-intentions-mode* :enforced))
    (return-from near-term-intention-process-due
      (obj "status" "skipped" "reason" "near-term-intentions-not-enforced")))
  (let (candidate candidate-version)
    (bt:with-lock-held (*near-term-intention-lock*)
      (when (%near-term-intention-expire-unlocked now)
        (%near-term-intention-save-unlocked))
      (let ((due (%near-term-intention-due-unlocked now)))
        (when due
          (setf candidate (%near-term-intention-copy due)
                candidate-version (gethash "version" due)))))
    (unless candidate
      (return-from near-term-intention-process-due
        (obj "status" "skipped" "reason" "no-due-intention")))
    (when (string= (gethash "state" candidate "") "ready")
      (return-from near-term-intention-process-due
        (%near-term-intention-process-ready-delivery
         candidate candidate-version now)))
    (let ((evidence (%near-term-intention-evidence candidate)))
      (unless evidence
        (near-term-intention-transition
         (gethash "id" candidate) "blocked" "missing-grounded-evidence"
         :failure-code "missing-grounded-evidence" :now now)
        (return-from near-term-intention-process-due
          (obj "status" "blocked" "reason" "missing-grounded-evidence"
               "intention_id" (gethash "id" candidate))))
      (let* ((result (%near-term-intention-cognitive candidate evidence))
             (record (and (hash-table-p result)
                          (string= (gethash "status" result "") "accepted")
                          (gethash "record" result)))
             (artifact (and (hash-table-p record) (gethash "content" record)))
             (advance
               (bt:with-lock-held (*near-term-intention-lock*)
                 (let ((current (find (gethash "id" candidate)
                                      *near-term-intention-records*
                                      :key (lambda (row) (gethash "id" row))
                                      :test #'string=)))
                   (cond
                     ((or (null current)
                          (not (%near-term-intention-active-p current))
                          (/= (gethash "version" current) candidate-version))
                      (obj "status" "skipped" "reason" "stale-version"))
                     ((%near-term-intention-text artifact)
                      (incf (gethash "pass_count" current 0))
                      (setf (gethash "state" current) "ready"
                            (gethash "artifact_summary" current) artifact
                            (gethash "evidence_ids" current)
                            (gethash "evidence_node_ids" record (vector))
                            (gethash "latest_transition" current)
                            "cognitive-result-ready"
                            (gethash "failure_code" current) :null
                            (gethash "updated_at" current) now)
                      (incf (gethash "version" current 0))
                      (%near-term-intention-save-unlocked)
                      (%near-term-intention-log
                       "near-term-intention-transition" current
                       (obj "operation" "cognitive-result-ready" "at" now))
                      (obj "status" "ready"
                           "intention_id" (gethash "id" current)
                           "pass_count" (gethash "pass_count" current)))
                     (t
                      (incf (gethash "pass_count" current 0))
                      (let ((exhausted
                              (>= (gethash "pass_count" current)
                                  (gethash "max_passes" current))))
                        (setf (gethash "state" current)
                              (if exhausted "blocked" "evolving")
                              (gethash "latest_transition" current)
                              "cognitive-result-rejected"
                              (gethash "failure_code" current)
                              (or (and (hash-table-p result)
                                       (gethash "status" result))
                                  "cognitive-result-rejected")
                              (gethash "next_reconsideration" current) (+ now 60)
                              (gethash "updated_at" current) now)
                        (incf (gethash "version" current 0))
                        (%near-term-intention-save-unlocked)
                        (%near-term-intention-log
                         "near-term-intention-transition" current
                         (obj "operation" "cognitive-result-rejected" "at" now))
                        (obj "status" (if exhausted "blocked" "evolving")
                             "intention_id" (gethash "id" current)
                             "pass_count" (gethash "pass_count" current)))))))))
        ;; A ready result is fulfilled immediately through the one existing
        ;; delivery boundary. Recursion reselects the versioned READY record;
        ;; it performs no second cognitive call.
        (if (string= (gethash "status" advance "") "ready")
            (near-term-intention-process-due :now now)
            advance)))))

(defun near-term-intention-mark-expressed (id expression-turn-id
                                            &key (now (get-universal-time)))
  (let ((turn-id (%near-term-intention-text expression-turn-id)))
    (unless turn-id
      (return-from near-term-intention-mark-expressed
        (values nil "missing-expression-turn")))
    (multiple-value-bind (record reason)
        (near-term-intention-transition id "expressed" "publicly-expressed"
                                        :now now)
      (when record
        (bt:with-lock-held (*near-term-intention-lock*)
          (let ((stored (find id *near-term-intention-records*
                              :key (lambda (row) (gethash "id" row))
                              :test #'string=)))
            (setf (gethash "expression_turn_id" stored) turn-id)
            (%near-term-intention-save-unlocked))))
      (values record reason))))

(defun %near-term-intention-words (text)
  (remove-duplicates
   (remove-if (lambda (word) (< (length word) 4))
              (uiop:split-string (string-downcase (or text ""))
                                 :separator '(#\Space #\Tab #\Newline #\Return
                                              #\. #\, #\! #\? #\: #\; #\- #\' #\")))
   :test #'string=))

(defun %near-term-intention-reply-overlap (record reply)
  (let* ((basis (%near-term-intention-words
                 (format nil "~a ~a" (gethash "subject" record "")
                         (gethash "artifact_summary" record ""))))
         (reply-words (%near-term-intention-words reply))
         (shared (count-if (lambda (word)
                             (member word reply-words :test #'string=)) basis)))
    (if (null basis) 0.0d0 (/ shared (float (length basis) 1.0d0)))))

(defun near-term-intention-observe-public-reply
    (reply expression-turn-id &key (now (get-universal-time)))
  "Close a ready commitment only when the completed public reply carries its result."
  (let ((active (near-term-intention-active :now now)))
    (cond
      ((or (null active) (not (string= (gethash "state" active "") "ready")))
       (values nil "no-ready-intention"))
      ((or (not (stringp reply))
           (< (%near-term-intention-reply-overlap active reply) 0.30d0))
       (values nil "result-not-observed"))
      (t
       (near-term-intention-mark-expressed
        (gethash "id" active) expression-turn-id :now now)))))

(defun near-term-intention-events (&key (now (get-universal-time)))
  "Render authoritative records as immutable workspace observations."
  (mapcar
   (lambda (record)
     (obj "id" (format nil "near-term-intention-event:~a:v~a"
                       (gethash "id" record) (gethash "version" record))
          "type" "near-term-item-observed" "at" (gethash "updated_at" record)
          "payload"
          (obj "item_id" (gethash "id" record)
               "item_type" "deferred-intention" "source" "conversation"
               "state" (gethash "state" record) "summary" (gethash "subject" record)
               "artifact_summary" (gethash "artifact_summary" record :null)
               "origin_turn_id" (gethash "origin_turn_id" record)
               "origin_event_ids" (gethash "origin_event_ids" record (vector))
               "evidence_ids" (gethash "evidence_ids" record (vector))
               "commitment_receipt_id" (gethash "commitment_receipt_id" record)
               "recipient" (gethash "recipient" record)
               "attention_at" (gethash "next_reconsideration" record :null)
               "response_deadline" (gethash "response_deadline" record)
               "expires_at" (gethash "expires_at" record)
               "pass_count" (gethash "pass_count" record 0)
               "max_passes" (gethash "max_passes" record)
               "latest_transition" (gethash "latest_transition" record)
               "next_reconsideration" (gethash "next_reconsideration" record :null)
               "failure_code" (gethash "failure_code" record :null))))
   (near-term-intention-records :active-only t :now now)))

(defun near-term-intention-safe-view (record)
  (when (hash-table-p record)
    (obj "id" (gethash "id" record) "state" (gethash "state" record)
         "subject" (gethash "subject" record) "aim" (gethash "aim" record)
         "origin_turn_id" (gethash "origin_turn_id" record)
         "commitment_receipt_id" (gethash "commitment_receipt_id" record)
         "response_deadline" (gethash "response_deadline" record)
         "next_reconsideration" (gethash "next_reconsideration" record :null)
         "pass_count" (gethash "pass_count" record 0)
         "max_passes" (gethash "max_passes" record)
         "latest_transition" (gethash "latest_transition" record)
         "artifact_summary" (gethash "artifact_summary" record :null)
         "failure_code" (gethash "failure_code" record :null)
         "expression_turn_id" (gethash "expression_turn_id" record :null)
         "stores_chain_of_thought" nil)))

(defun near-term-intention-report (&key (now (get-universal-time)))
  (let* ((records (near-term-intention-records :now now))
         (active (remove-if-not #'%near-term-intention-active-p records)))
    (obj "schema_version" 1
         "mode" (if (boundp '*near-term-intentions-mode*)
                    (string-downcase (symbol-name *near-term-intentions-mode*)) "off")
         "active_count" (length active) "record_count" (length records)
         "active" (if active (near-term-intention-safe-view (first active)) :null)
         "latest" (if records (near-term-intention-safe-view (first records)) :null)
         "direct_delivery_capability" nil
         "stores_chain_of_thought" nil)))

(define-init :restore near-term-intentions-restore
    "Restore durable state for near-term-intentions."
  (near-term-intention-load))

(define-init :install near-term-intentions-public-reply-port
    "Register near-term-intentions as turn-capture's public-reply observer.
CONVERSATION-TURN-CAPTURE.LISP records the completed public reply and
previously reached up for NEAR-TERM-INTENTION-OBSERVE-PUBLIC-REPLY by bare
symbol. With no near-term-intention layer the port stays NIL and turn
capture is unaffected."
  (setf *near-term-intention-public-reply-observer*
        #'near-term-intention-observe-public-reply)
  t)
