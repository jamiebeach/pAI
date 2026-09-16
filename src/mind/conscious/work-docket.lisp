;;;; work-docket.lisp -- event-derived resumable useful work.
;;;;
;;;; The docket is not a second scheduler and it grants no new effect authority.
;;;; It is a rebuildable projection over immutable lifecycle events.  The
;;;; recursive mind may select one eligible entry and run one ordinary private
;;;; cognition root with the same model, tools, budgets, and recovery rules it
;;;; already uses.

(in-package :agent)

(export '(conscious-work-docket-project
          conscious-work-docket-inspect
          conscious-work-docket-select
          conscious-work-docket-open
          conscious-work-docket-transition
          conscious-work-docket-open-focus
          conscious-work-docket-install-continuity-contributor))

(defparameter *conscious-work-docket-revision* "work-docket-v1")
(defparameter *conscious-work-docket-default-revisit-seconds* 1800)
(defparameter *conscious-work-docket-event-types*
  '("conscious-work-docket-opened"
    "conscious-work-docket-transitioned"
    "recursive-work-docket-focus-opened"
    "recursive-work-docket-result"))

(defun %work-docket-items (value)
  (cond ((null value) nil)
        ((vectorp value) (coerce value 'list))
        ((listp value) value)
        (t (error "Work docket expected an array"))))

(defun %work-docket-text (value label maximum)
  (unless (and (stringp value) (plusp (length value))
               (<= (length value) maximum))
    (error "~a must be non-empty and at most ~d characters" label maximum))
  value)

(defun %work-docket-state-p (value)
  (member value '("active" "waiting" "completed" "cancelled")
          :test #'string=))

(defun %work-docket-copy (value)
  (shasht:read-json (shasht:write-json value nil)))

(defun %work-docket-event-time (event)
  (let ((payload (and (hash-table-p event) (gethash "payload" event))))
    (or (and (hash-table-p payload)
             (or (gethash "observed_at" payload)
                 (gethash "opened_at" payload)
                 (gethash "completed_at" payload)))
        0)))

(defun %work-docket-event-id (event)
  (and (hash-table-p event) (gethash "id" event)))

(defun %work-docket-fnv (text)
  (let ((hash 2166136261))
    (loop for character across text
          do (setf hash (logand #xffffffff
                                (* (logxor hash (char-code character))
                                   16777619))))
    (format nil "~8,'0x" hash)))

(defun %work-docket-id (agent-id source-event-id title purpose)
  (format nil "work:~a:~a:~a" agent-id source-event-id
          (%work-docket-fnv (format nil "~a~c~a" title (code-char 0) purpose))))

(defun %work-docket-open-payload-valid-p (payload)
  (and (hash-table-p payload)
       (eql 1 (gethash "schema_version" payload -1))
       (%work-docket-text (gethash "work_id" payload) "work_id" 256)
       (%work-docket-text (gethash "title" payload) "title" 240)
       (%work-docket-text (gethash "purpose" payload) "purpose" 1200)
       (%work-docket-text (gethash "operator_benefit" payload)
                          "operator_benefit" 1200)
       (%work-docket-text (gethash "next_step" payload) "next_step" 1200)
       (member (gethash "priority" payload)
               '("normal" "high") :test #'string=)
       (string= "private-cognition-existing-authority"
                (gethash "authority" payload ""))
       (let ((ids (gethash "source_event_ids" payload)))
         (and (vectorp ids) (<= 1 (length ids) 16)
              (every (lambda (id)
                       (or (and (integerp id) (plusp id))
                           (and (stringp id) (plusp (length id))
                                (<= (length id) 256))))
                     (coerce ids 'list))))
       (integerp (gethash "opened_at" payload))
       (integerp (gethash "next_eligible_at" payload))
       (not (minusp (gethash "opened_at" payload)))
       (not (minusp (gethash "next_eligible_at" payload)))))

(defun %work-docket-transition-payload-valid-p (payload)
  (and (hash-table-p payload)
       (eql 1 (gethash "schema_version" payload -1))
       (%work-docket-text (gethash "work_id" payload) "work_id" 256)
       (%work-docket-state-p (gethash "state" payload))
       (%work-docket-text (gethash "note" payload) "note" 2000)
       (%work-docket-text (gethash "next_step" payload) "next_step" 1200)
       (integerp (gethash "next_eligible_at" payload))
       (integerp (gethash "observed_at" payload))
       (not (minusp (gethash "next_eligible_at" payload)))
       (not (minusp (gethash "observed_at" payload)))))

(defun conscious-work-docket-project (events &key agent-id)
  "Purely rebuild the work docket from authority-ordered EVENTS."
  (let ((by-id (make-hash-table :test #'equal))
        (order nil))
    (dolist (event (%work-docket-items events))
      (when (and (hash-table-p event)
                 (or (null agent-id)
                     (equal agent-id (gethash "agent_id" event))))
        (let* ((type (gethash "type" event ""))
               (payload (gethash "payload" event)))
          (cond
            ((string= type "conscious-work-docket-opened")
             (unless (%work-docket-open-payload-valid-p payload)
               (error "Malformed work-docket open event ~s"
                      (%work-docket-event-id event)))
             (let ((work-id (gethash "work_id" payload)))
               (unless (gethash work-id by-id)
                 (let ((row (%work-docket-copy payload)))
                   (setf (gethash "state" row) "active"
                         (gethash "revision" row) 1
                         (gethash "source_event_id" row)
                         (%work-docket-event-id event)
                         (gethash "last_event_id" row)
                         (%work-docket-event-id event)
                         (gethash "last_progress_at" row)
                         (gethash "opened_at" payload)
                         (gethash "note" row) "Work entered the durable docket.")
                   (setf (gethash work-id by-id) row)
                   (push work-id order)))))
            ((string= type "conscious-work-docket-transitioned")
             (unless (%work-docket-transition-payload-valid-p payload)
               (error "Malformed work-docket transition event ~s"
                      (%work-docket-event-id event)))
             (let* ((work-id (gethash "work_id" payload))
                    (row (gethash work-id by-id)))
               ;; Orphan transitions are forensic evidence, never authority to
               ;; manufacture an entry that was not opened.
               (when row
                 (setf (gethash "state" row) (gethash "state" payload)
                       (gethash "next_step" row) (gethash "next_step" payload)
                       (gethash "next_eligible_at" row)
                       (gethash "next_eligible_at" payload)
                       (gethash "note" row) (gethash "note" payload)
                       (gethash "last_progress_at" row)
                       (gethash "observed_at" payload)
                       (gethash "last_event_id" row)
                       (%work-docket-event-id event)
                       (gethash "revision" row)
                       (1+ (gethash "revision" row))))))))))
    (let ((rows
            (loop for work-id in (nreverse order)
                  for row = (gethash work-id by-id)
                  when row collect row)))
      (obj "schema_version" 1 "projection_revision"
           *conscious-work-docket-revision*
           "item_count" (length rows)
           "items" (coerce rows 'vector)))))

(defun %work-docket-events ()
  ;; A newest-N replay can strand an old OPEN behind many transitions and
  ;; silently erase live work. Stream the small declared vocabulary instead;
  ;; authority replay is exhaustive, while callers bound their displayed view.
  (let ((events nil))
    (multiple-value-bind (complete-p ignored-last-id ignored-count)
        (map-events (lambda (event) (push event events))
                    :types *conscious-work-docket-event-types*)
      (declare (ignore ignored-last-id ignored-count))
      (unless complete-p (error "Work docket authority stream was incomplete"))
      (nreverse events))))

(defun conscious-work-docket-inspect (&optional (limit 64))
  "Return a detached, human-readable live projection."
  (unless (and (integerp limit) (<= 1 limit 256))
    (error "Work docket inspection limit must be from 1 to 256"))
  (let* ((report (conscious-work-docket-project
                  (%work-docket-events) :agent-id *agent-id*))
         (rows (%work-docket-items (gethash "items" report)))
         (selected (last rows (min limit (length rows)))))
    (setf (gethash "items" report)
          (coerce (mapcar #'%work-docket-copy selected) 'vector))
    report))

(defun conscious-work-docket-select (&key (as-of (get-universal-time)))
  "Select one eligible unfinished item deterministically."
  (unless (and (integerp as-of) (not (minusp as-of)))
    (error "Work docket selection time is invalid"))
  (let* ((report (conscious-work-docket-inspect 256))
         (eligible
           (remove-if-not
            (lambda (row)
              (and (member (gethash "state" row)
                           '("active" "waiting") :test #'string=)
                   (<= (gethash "next_eligible_at" row) as-of)))
            (%work-docket-items (gethash "items" report)))))
    (car
     (sort eligible
           (lambda (left right)
             (let ((left-high (string= "high" (gethash "priority" left)))
                   (right-high (string= "high" (gethash "priority" right)))
                   (left-time (gethash "next_eligible_at" left))
                   (right-time (gethash "next_eligible_at" right)))
               (cond ((and left-high (not right-high)) t)
                     ((and right-high (not left-high)) nil)
                     ((< left-time right-time) t)
                     ((> left-time right-time) nil)
                     (t (< (gethash "last_progress_at" left)
                           (gethash "last_progress_at" right))))))))))

(defun %work-docket-append (type payload caused-by)
  (multiple-value-bind (id durable-p event)
      (log-event type payload :caused-by caused-by)
    (unless (and id durable-p (hash-table-p event))
      (error "Work docket ~a was not durably appended" type))
    event))

(defun conscious-work-docket-open
    (&key title purpose operator-benefit next-step source-event-id
          source-event-ids (priority "normal")
          (as-of (get-universal-time)))
  "Open one idempotently identified entry grounded in an existing root."
  (%work-docket-text title "title" 240)
  (%work-docket-text purpose "purpose" 1200)
  (%work-docket-text operator-benefit "operator_benefit" 1200)
  (%work-docket-text next-step "next_step" 1200)
  (unless (member priority '("normal" "high") :test #'string=)
    (error "Work docket priority must be normal or high"))
  (unless source-event-id (error "Work docket requires source_event_id"))
  (let* ((agent-id (%work-docket-text *agent-id* "agent_id" 256))
         (work-id (%work-docket-id agent-id source-event-id title purpose))
         (existing
           (find work-id
                 (%work-docket-items
                  (gethash "items" (conscious-work-docket-inspect 256)))
                 :key (lambda (row) (gethash "work_id" row))
                 :test #'string=)))
    (if existing
        (%work-docket-copy existing)
        (let* ((ids (remove-duplicates
                     (cons source-event-id (%work-docket-items source-event-ids))
                     :test #'equal))
               (payload
                 (obj "schema_version" 1 "work_id" work-id "title" title
                      "purpose" purpose "operator_benefit" operator-benefit
                      "next_step" next-step "priority" priority
                      "authority" "private-cognition-existing-authority"
                      "source_event_ids" (coerce ids 'vector)
                      "opened_at" as-of "next_eligible_at" as-of)))
          (%work-docket-append "conscious-work-docket-opened"
                               payload source-event-id)
          (find work-id
                (%work-docket-items
                 (gethash "items" (conscious-work-docket-inspect 256)))
                :key (lambda (row) (gethash "work_id" row))
                :test #'string=)))))

(defun conscious-work-docket-transition
    (work-id state note next-step &key source-event-id next-eligible-at
                                      (as-of (get-universal-time)))
  "Append one lifecycle transition; return the updated detached item."
  (%work-docket-text work-id "work_id" 256)
  (unless (%work-docket-state-p state) (error "Invalid work docket state"))
  (%work-docket-text note "note" 2000)
  (%work-docket-text next-step "next_step" 1200)
  (unless source-event-id (error "Work transition requires source_event_id"))
  (let* ((report (conscious-work-docket-inspect 256))
         (existing
           (find work-id (%work-docket-items (gethash "items" report))
                 :key (lambda (row) (gethash "work_id" row))
                 :test #'string=)))
    (unless existing (error "Unknown work docket item ~a" work-id))
    (when (member (gethash "state" existing)
                  '("completed" "cancelled") :test #'string=)
      (error "Terminal work docket item ~a cannot transition" work-id))
    (let ((eligible
            (or next-eligible-at
                (if (member state '("completed" "cancelled") :test #'string=)
                    as-of
                    (+ as-of *conscious-work-docket-default-revisit-seconds*)))))
      (unless (and (integerp eligible) (not (minusp eligible)))
        (error "Work docket next_eligible_at is invalid"))
      (%work-docket-append
       "conscious-work-docket-transitioned"
       (obj "schema_version" 1 "work_id" work-id "state" state
            "note" note "next_step" next-step
            "next_eligible_at" eligible "observed_at" as-of)
       source-event-id))
    (find work-id
          (%work-docket-items
           (gethash "items" (conscious-work-docket-inspect 256)))
          :key (lambda (row) (gethash "work_id" row)) :test #'string=)))

(defun conscious-work-docket-open-focus (item &key (as-of (get-universal-time)))
  "Append one immutable private work quantum root from a projected item."
  (unless (and (hash-table-p item)
               (%work-docket-text (gethash "work_id" item) "work_id" 256)
               (integerp (gethash "revision" item)))
    (error "Work docket focus requires a projected item"))
  (%work-docket-append
   "recursive-work-docket-focus-opened"
   (obj "schema_version" 1
        "work_id" (gethash "work_id" item)
        "work_revision" (gethash "revision" item)
        "title" (gethash "title" item)
        "purpose" (gethash "purpose" item)
        "operator_benefit" (gethash "operator_benefit" item)
        "next_step" (gethash "next_step" item)
        "authority" (gethash "authority" item)
        "source_event_ids" (%work-docket-copy
                            (gethash "source_event_ids" item))
        "runtime_revision" *conscious-work-docket-revision*
        "opened_at" as-of)
   (gethash "last_event_id" item)))

(defun %work-docket-continuity-contributions (request ignored)
  (declare (ignore ignored))
  (let* ((as-of (gethash "as_of" request))
         (report (conscious-work-docket-inspect 64))
         (rows
           (remove-if-not
            (lambda (row)
              (member (gethash "state" row)
                      '("active" "waiting") :test #'string=))
            (%work-docket-items (gethash "items" report))))
         (chosen (subseq rows 0 (min 4 (length rows)))))
    (mapcar
     (lambda (row)
       (obj "kind" "maintained-work" "status" (gethash "state" row)
            "source_id" (gethash "last_event_id" row)
            "observed_at" (min as-of (gethash "last_progress_at" row))
            "content"
            (format nil "Maintained work: ~a. Why it matters: ~a. Next: ~a. Eligible again: ~a."
                    (gethash "title" row)
                    (gethash "operator_benefit" row)
                    (gethash "next_step" row)
                    (gethash "next_eligible_at" row))))
     chosen)))

(defun conscious-work-docket-install-continuity-contributor ()
  (continuity-capsule-register-contributor
   "maintained-work" '%work-docket-continuity-contributions
   :order 90 :revision *conscious-work-docket-revision*)
  t)
