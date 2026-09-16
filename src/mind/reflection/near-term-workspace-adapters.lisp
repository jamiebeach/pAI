;;;; near-term-workspace-adapters.lisp -- read-only source adapters for shadow inspection.
;;;;
;;;; Adapters translate existing public read APIs into near-term workspace
;;;; events. They never mutate a source record, persist an event, or invoke a
;;;; model, tool, tick, publication, initiative evaluation, or delivery seam.

(in-package :agent)

(export '(near-term-workspace-shadow-snapshot
          near-term-workspace-adapter-report))

(defparameter *near-term-workspace-adapter-source-limit* 20)
(defvar *near-term-workspace-adapter-last-report* nil)
(defvar *near-term-workspace-adapter-now* nil)

;; Injectable providers are intentionally the only dependencies on existing
;; subsystems. Tests bind these seams; production defaults call read APIs only.
(defparameter *near-term-workspace-latent-source-fn*
  (lambda ()
    (if (fboundp 'latent-v2-thoughts)
        (funcall 'latent-v2-thoughts) nil)))

(defparameter *near-term-workspace-question-source-fn*
  (lambda ()
    (if (fboundp 'self-model-active-open-questions)
        (funcall 'self-model-active-open-questions :limit 8) nil)))

(defparameter *near-term-workspace-scheduler-source-fn*
  (lambda ()
    (if (fboundp 'pai-scheduler-context-snapshot)
        (funcall 'pai-scheduler-context-snapshot 10) nil)))

(defparameter *near-term-workspace-initiative-source-fn*
  (lambda ()
    (if (fboundp 'initiative-candidates)
        (funcall 'initiative-candidates) nil)))

(defparameter *near-term-workspace-intention-source-fn*
  (lambda ()
    (if (fboundp 'near-term-intention-events)
        (funcall 'near-term-intention-events
                 :now (or *near-term-workspace-adapter-now*
                          (get-universal-time))) nil)))

(defun %near-term-adapter-list (value)
  (cond ((null value) nil)
        ((vectorp value) (coerce value 'list))
        ((listp value) value)
        (t nil)))

(defun %near-term-adapter-nonempty-string-p (value)
  (and (stringp value)
       (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                   value)))))

(defun %near-term-adapter-limit (rows)
  (let ((list (%near-term-adapter-list rows)))
    (subseq list 0 (min *near-term-workspace-adapter-source-limit*
                        (length list)))))

(defun %near-term-adapter-id (prefix value)
  (format nil "~a:~a" prefix value))

(defun %near-term-adapter-event (id at payload)
  (obj "id" id "type" "near-term-item-observed"
       "at" (if (numberp at) at 0) "payload" payload))

(defun %near-term-adapter-latent-state (state)
  (cond ((member state '("grounding-needed" "seeded") :test #'string=)
         "seeded")
        ((string= state "scheduled") "waiting")
        ((string= state "evolving") "evolving")
        ((string= state "ready") "ready")
        (t nil)))

(defun %near-term-adapt-latent (rows)
  (let ((events nil))
    (dolist (row (%near-term-adapter-limit rows) (nreverse events))
      (when (hash-table-p row)
        (let* ((id (gethash "id" row))
               (content (gethash "content" row))
               (state (%near-term-adapter-latent-state
                       (gethash "state" row ""))))
          (when (and (%near-term-adapter-nonempty-string-p id)
                     (%near-term-adapter-nonempty-string-p content)
                     state)
            (push
             (%near-term-adapter-event
              (%near-term-adapter-id "latent-event" id)
              (gethash "updated_at" row)
              (obj "item_id" (%near-term-adapter-id "latent-v2" id)
                   "item_type" "thought" "source" "latent-v2"
                   "state" state "summary" content
                   "origin_event_ids" (gethash "source_event_ids" row (vector))
                   "evidence_ids" (gethash "evidence_ids" row (vector))
                   "attention_at" (gethash "next_reconsideration" row :null)
                   "expires_at" (gethash "expires_at" row :null)
                   "artifact_summary"
                   (if (string= state "ready") content :null)))
              events)))))))

(defun %near-term-adapt-questions (rows)
  (let ((events nil))
    (dolist (row (%near-term-adapter-limit rows) (nreverse events))
      (when (hash-table-p row)
        (let ((id (gethash "id" row))
              (statement (gethash "statement" row)))
          (when (and id (%near-term-adapter-nonempty-string-p statement))
            (push
             (%near-term-adapter-event
              (%near-term-adapter-id "question-event" id)
              (gethash "created-at" row)
              (obj "item_id" (%near-term-adapter-id "question" id)
                   "item_type" "question" "source" "self-model"
                   "state" "waiting" "summary" statement
                   "evidence_ids"
                   (or (gethash "root-evidence-node-ids" row)
                       (gethash "evidence-node-ids" row) (vector))))
              events)))))))

(defun %near-term-adapt-scheduler (rows)
  (let ((events nil))
    (dolist (row (%near-term-adapter-limit rows) (nreverse events))
      (when (hash-table-p row)
        (let ((id (gethash "id" row))
              (text (gethash "text" row)))
          (when (and (%near-term-adapter-nonempty-string-p id)
                     (%near-term-adapter-nonempty-string-p text)
                     (not (numberp (gethash "consumed_at_utc" row))))
            (push
             (%near-term-adapter-event
              (%near-term-adapter-id "scheduler-event" id)
              (gethash "fired_at_utc" row)
              (obj "item_id" (%near-term-adapter-id "scheduler" id)
                   "item_type" "reminder" "source" "scheduler"
                   "state" "ready" "summary" text
                   "artifact_summary" text
                   "origin_event_ids" (vector id)
                   "attention_at" (gethash "fired_at_utc" row :null)))
              events)))))))

(defparameter *near-term-workspace-pending-initiative-statuses*
  '("candidate" "approved" "deferred"))

(defun %near-term-adapt-initiative (rows)
  (let ((events nil))
    (dolist (row (%near-term-adapter-limit rows) (nreverse events))
      (when (hash-table-p row)
        (let ((id (gethash "id" row))
              (reason (gethash "reason" row))
              (status (gethash "status" row "")))
          (when (and (%near-term-adapter-nonempty-string-p id)
                     (%near-term-adapter-nonempty-string-p reason)
                     (string= (gethash "kind" row "") "share-thought")
                     (member status
                             *near-term-workspace-pending-initiative-statuses*
                             :test #'string=))
            (push
             (%near-term-adapter-event
              (%near-term-adapter-id "initiative-event" id)
              (gethash "updated_at" row)
              (obj "item_id" (%near-term-adapter-id "initiative" id)
                   "item_type" "thought" "source" "initiative"
                   "state" "waiting" "summary" reason
                   "attention_at" (gethash "updated_at" row :null)))
              events)))))))

(defun %near-term-adapt-intentions (rows)
  ;; The intention owner already emits validated workspace event objects.
  (remove-if-not
   (lambda (row)
     (and (hash-table-p row)
          (string= (gethash "type" row "") "near-term-item-observed")))
   (%near-term-adapter-limit rows)))

(defun %near-term-adapter-source (name provider adapter)
  (handler-case
      (let* ((rows (funcall provider))
             (events (funcall adapter rows)))
        (values events
                (obj "source" name
                     "records_read" (length (%near-term-adapter-list rows))
                     "events_emitted" (length events)
                     "status" "ok")))
    (error (condition)
      (values nil
              (obj "source" name "records_read" 0 "events_emitted" 0
                   "status" "error"
                   "error_type"
                   (string-downcase (symbol-name (type-of condition))))))))

(defun near-term-workspace-shadow-snapshot
    (&key (now (get-universal-time)))
  "Build one read-only dashboard snapshot from existing source APIs."
  (let ((events nil) (sources nil)
        (*near-term-workspace-adapter-now* now))
    (dolist (spec
             (list (list "latent-v2" *near-term-workspace-latent-source-fn*
                         #'%near-term-adapt-latent)
                   (list "active-questions"
                         *near-term-workspace-question-source-fn*
                         #'%near-term-adapt-questions)
                   (list "scheduler" *near-term-workspace-scheduler-source-fn*
                         #'%near-term-adapt-scheduler)
                   (list "initiative"
                         *near-term-workspace-initiative-source-fn*
                         #'%near-term-adapt-initiative)
                   (list "conversational-intentions"
                         *near-term-workspace-intention-source-fn*
                         #'%near-term-adapt-intentions)))
      (multiple-value-bind (source-events source-report)
          (%near-term-adapter-source (first spec) (second spec) (third spec))
        (setf events (append events source-events))
        (push source-report sources)))
    (multiple-value-bind (items rejections)
        (near-term-workspace-materialize events :now now)
      (let ((snapshot
              (obj "schema_version" 1 "mode" "dashboard-shadow"
                   "built_at" now
                   "items" (coerce (mapcar #'near-term-workspace-safe-item-view
                                            items)
                                   'vector)
                   "rejections" (coerce rejections 'vector)
                   "sources" (coerce (nreverse sources) 'vector)
                   "events_considered" (length events)
                   "active_items" (length items)
                   "max_active_items" *near-term-workspace-max-active-items*
                   "conversational_intention"
                   (if (fboundp 'near-term-intention-report)
                       (gethash "latest"
                                (funcall 'near-term-intention-report :now now)
                                :null)
                       :null)
                   "prompt_integration" nil
                   "tick_integration" nil
                   "publication_integration" nil
                   "direct_delivery_capability" nil)))
        (setf *near-term-workspace-adapter-last-report* snapshot)
        snapshot))))

(defun near-term-workspace-adapter-report ()
  (or *near-term-workspace-adapter-last-report*
      (obj "schema_version" 1 "mode" "dashboard-shadow"
           "status" "not-yet-sampled")))
