;;;; Replayable ownership of related retained stimuli. No implicit admission.
(in-package :agent)

(define-seam recursive-stimulus-activity-key (event)
  "Pure adapter-owned resource identity, not a similarity judgment."
  (let* ((context (recursive-stimulus-context event))
         (environment (and (hash-table-p context)
                           (gethash "environment" context))))
    (if (and (hash-table-p environment)
             (every (lambda (key)
                      (%recursive-nonempty-string-p
                       (gethash key environment) 128))
                    '("kind" "owner_id" "resource_id")))
        (shasht:write-json
         (vector (gethash "kind" environment)
                 (gethash "owner_id" environment)
                 (gethash "resource_id" environment)) nil)
        (format nil "independent:~a" (gethash "id" event)))))

(defun %recursive-activity-for-root (events root-id agent-id)
  (find-if (lambda (event)
             (and (equal agent-id (gethash "agent_id" event))
                  (equal root-id (gethash "caused_by" event))
                  (equal "recursive-activity-opened" (gethash "type" event))))
           events))

(defun %recursive-activity-membership (events agent-id)
  "Project one ownership index; a follower cannot start a separate turn."
  (let ((owners (make-hash-table :test #'equal)))
    (dolist (event events owners)
      (when (and (equal agent-id (gethash "agent_id" event))
                 (equal "recursive-activity-opened" (gethash "type" event)))
        (let* ((payload (gethash "payload" event))
               (root (gethash "caused_by" event))
               (ids (and (hash-table-p payload)
                         (gethash "source_event_ids" payload))))
          (unless (and (vectorp ids) (not (stringp ids))
                       (<= 1 (length ids) 8)
                       (equal root (aref ids 0))
                       (= (length ids)
                          (length (remove-duplicates (coerce ids 'list)
                                                     :test #'equal))))
            (error "Malformed retained activity membership"))
          (loop for id across ids do
            (when (and (gethash id owners)
                       (not (equal root (gethash id owners))))
              (error "A stimulus belongs to two activities"))
            (setf (gethash id owners) root)))))))

(defun %recursive-build-stimulus-activity (receipt eligible)
  "Freeze a bounded same-resource batch before model execution.
ELIGIBLE must already exclude settled, started, and legacy-linked followers."
  (let ((key (recursive-stimulus-activity-key receipt))
        (members nil) (contexts nil) (size 0))
    (dolist (candidate eligible)
      (when (and (< (length members) 8)
                 (equal (gethash "agent_id" receipt)
                        (gethash "agent_id" candidate))
                 (equal (gethash "type" receipt) (gethash "type" candidate))
                 (equal key (recursive-stimulus-activity-key candidate)))
        (let* ((context (obj "source_event_id" (gethash "id" candidate)
                             "context" (recursive-stimulus-context candidate)))
               (width (length (shasht:write-json context nil))))
          (when (or (null members) (<= (+ size width) 48000))
            (push (gethash "id" candidate) members)
            (push context contexts)
            (incf size width)))))
    (unless (and members (equal (gethash "id" receipt) (car (last members))))
      (error "Activity leader must be the first eligible member"))
    (obj "schema_version" 1 "resource_key" key
         "source_event_ids" (coerce (nreverse members) 'vector)
         "contexts" (coerce (nreverse contexts) 'vector)
         "opened_at" (get-universal-time))))

(defun %recursive-pending-stimuli (events agent-id &optional (now (get-universal-time)))
  "Project unclaimed stimulus leaders without admitting or executing them.
Legacy linked generic roots own their peer receipts. Terminal settlement and
consumption suppress both forms; retry waits honor their durable deadline."
  (let ((receipts (make-hash-table :test #'equal))
        (bridges (make-hash-table :test #'equal))
        (consumed (make-hash-table :test #'equal))
        (dispositions (make-hash-table :test #'equal))
        (results (make-hash-table :test #'equal))
        (owners (%recursive-activity-membership events agent-id))
        (candidates nil))
    (dolist (event events)
      (when (equal agent-id (gethash "agent_id" event))
        (let ((type (gethash "type" event))
              (id (gethash "id" event))
              (parent (gethash "caused_by" event))
              (payload (gethash "payload" event)))
          (cond
            ((equal type "peer-message-received")
             (setf (gethash id receipts) event))
            ((equal type "agent-stimulus-received")
             (push event candidates)
             (when (and parent (not (eq parent :null)))
               (push event (gethash parent bridges))))
            ((equal type "stimulus-consumed")
             (when (hash-table-p payload)
               (loop for stimulus-id across (gethash "stimulus_ids" payload #())
                     do (setf (gethash stimulus-id consumed) t))))
            ((member type '("recursive-peer-message-disposition"
                            "recursive-stimulus-disposition") :test #'equal)
             (setf (gethash parent dispositions) payload))
            ((member type '("recursive-peer-message-result"
                            "recursive-stimulus-result") :test #'equal)
             (setf (gethash parent results) payload))))))
    (maphash (lambda (id receipt)
               (let ((linked (gethash id bridges)))
                 (when (> (length linked) 1)
                   (error "One receipt has multiple linked generic roots"))
                 (unless linked (push receipt candidates)))) receipts)
    (setf candidates (sort candidates #'< :key (lambda (event) (gethash "id" event))))
    (remove-if-not
     (lambda (candidate)
       (let* ((id (gethash "id" candidate))
              (owner (gethash id owners))
              (disposition (gethash id dispositions))
              (state (and (hash-table-p disposition)
                          (gethash "disposition" disposition)))
              (next (and (hash-table-p disposition)
                         (gethash "next_eligible_at" disposition))))
         (and (or (null owner) (equal id owner))
              (not (gethash (format nil "stimulus:~a" id) consumed))
              (not (equal "completed"
                          (let ((result (gethash id results)))
                            (and (hash-table-p result)
                                 (gethash "status" result)))))
              (not (member state '("covered" "completed" "replied" "absorbed"
                                   "delivery-failed" "failed" "outcome-unknown")
                           :test #'equal))
              (or (not (integerp next)) (<= next now)))))
     candidates)))

(defun %recursive-open-stimulus-activity (events root agent-id)
  "Freeze eligible same-resource retained inputs before their first model call.
Existing model requests keep their original context; replay never refreezes."
  (let* ((root-id (gethash "id" root))
         (existing (%recursive-activity-for-root events root-id agent-id)))
    (when existing
      (return-from %recursive-open-stimulus-activity existing))
    (when (some (lambda (event)
                  (and (equal agent-id (gethash "agent_id" event))
                       (equal root-id (gethash "caused_by" event))
                       (equal "model-request" (gethash "type" event))))
                events)
      (return-from %recursive-open-stimulus-activity nil))
    (unless (and (equal agent-id (gethash "agent_id" root))
                 (member (gethash "type" root)
                         '("agent-stimulus-received" "peer-message-received")
                         :test #'equal))
      (error "Activity opening requires an owned retained stimulus"))
    (let* ((started (make-hash-table :test #'equal))
           (eligible (%recursive-pending-stimuli events agent-id))
           (from-root (member root eligible
                              :test (lambda (left right)
                                      (equal (gethash "id" left)
                                             (gethash "id" right))))))
      (dolist (event events)
        (when (and (equal agent-id (gethash "agent_id" event))
                   (member (gethash "type" event)
                           '("model-request" "recursive-activity-opened")
                           :test #'equal))
          (setf (gethash (gethash "caused_by" event) started) t)))
      (unless (and from-root
                   (not (gethash root-id started)))
        (error "Activity leader is not an unstarted eligible stimulus"))
      (let ((batch (%recursive-build-stimulus-activity
                    root (remove-if (lambda (event)
                                      (gethash (gethash "id" event) started))
                                    from-root))))
        (nth-value 1
                   (%conversation-append-readable
                    "recursive-activity-opened" batch :caused-by root-id))))))

(defun %recursive-reconcile-activity-followers-one (events agent-id)
  "Durably cover one frozen follower after its leader completed.
Repair an interruption between disposition and consumption on the next wake."
  (%recursive-activity-membership events agent-id)
  (let ((completed (make-hash-table :test #'equal))
        (dispositions (make-hash-table :test #'equal))
        (consumed (make-hash-table :test #'equal))
        (activities nil))
    (dolist (event events)
      (when (equal agent-id (gethash "agent_id" event))
        (let* ((type (gethash "type" event))
               (root-id (gethash "caused_by" event))
               (payload (gethash "payload" event)))
          (cond
            ((equal type "recursive-activity-opened") (push event activities))
            ((and (equal type "recursive-stimulus-result")
                  (hash-table-p payload)
                  (equal "completed" (gethash "status" payload)))
             (setf (gethash root-id completed) t))
            ((equal type "recursive-stimulus-disposition")
             (setf (gethash root-id dispositions)
                   (and (hash-table-p payload)
                        (gethash "disposition" payload))))
            ((equal type "stimulus-consumed")
             (when (hash-table-p payload)
               (loop for id across (gethash "stimulus_ids" payload #())
                     do (setf (gethash id consumed) t))))))))
    (dolist (activity (nreverse activities))
      (let* ((root-id (gethash "caused_by" activity))
             (ids (gethash "source_event_ids" (gethash "payload" activity))))
        (when (gethash root-id completed)
          (loop for index from 1 below (length ids)
                for follower-id = (aref ids index)
                for prior = (gethash follower-id dispositions)
                do (cond
                     ((null prior)
                      (%conversation-append-readable
                       "recursive-stimulus-disposition"
                       (obj "schema_version" 1
                            "receipt_event_id" follower-id
                            "activity_root_event_id" root-id
                            "disposition" "covered"
                            "settled_at" (get-universal-time))
                       :caused-by follower-id)
                      (return-from %recursive-reconcile-activity-followers-one t))
                     ((and (equal prior "covered")
                           (not (gethash (format nil "stimulus:~a" follower-id)
                                         consumed)))
                      (%conversation-append-readable
                       "stimulus-consumed"
                       (obj "agent_id" agent-id
                            "stimulus_ids"
                            (vector (format nil "stimulus:~a" follower-id))
                            "consumer" "recursive-activity-v1"
                            "disposition" "covered")
                       :caused-by follower-id)
                      (return-from %recursive-reconcile-activity-followers-one t)))))))
    nil))
