;;;; pull-reciprocity.lisp -- V0/V1 solicited review and pairwise labels.
;;;;
;;;; This module has no delivery, model, tool, memory, policy, timer, worker, or
;;;; contact capability. It is called only from an already-active inbound turn.

(in-package :agent)

(export '(pull-reciprocity-handle-inbound pull-reciprocity-present-reply
          pull-reciprocity-report pull-reciprocity-set-mode))

(defparameter *pull-reciprocity-review-file*
  (pathname (or (uiop:getenv "PAI_PULL_RECIPROCITY_REVIEW_FILE")
                "/agent/state/pull-reciprocity-review.json")))
(defparameter *pull-reciprocity-label-file*
  (pathname (or (uiop:getenv "PAI_PULL_RECIPROCITY_LABEL_FILE")
                "/agent/state/pull-reciprocity-labels.json")))
(defparameter *pull-reciprocity-max-raw* 20)
(defparameter *pull-reciprocity-pairs-per-batch* 10)
(defparameter *pull-reciprocity-max-batches* 100)
(defparameter *pull-reciprocity-description-max-per-24-hours* 2)
(defvar *pull-reciprocity-mode*
  (if (string-equal (or (uiop:getenv "PAI_PULL_RECIPROCITY_MODE") "enabled")
                    "off") :off :enabled))
(defvar *pull-reciprocity-review* nil)
(defvar *pull-reciprocity-descriptions* nil)
(defvar *pull-reciprocity-batches* nil) ; immutable legacy audit, newest first
(defvar *pull-reciprocity-labels* nil) ; typed V6 labels, newest first
(defvar *pull-reciprocity-active-label* nil)
(defvar *pull-reciprocity-lock* (bt:make-lock "pull-reciprocity"))
(defvar *pull-reciprocity-random-fn* (lambda (limit) (random limit)))

(defun %pull-list (value)
  (cond ((null value) nil) ((vectorp value) (coerce value 'list))
        ((listp value) value) (t (list value))))

(defun %pull-trim-lower (text)
  (string-downcase
   (string-trim '(#\Space #\Tab #\Newline #\Return) (or text ""))))

(defun %pull-id (prefix)
  (format nil "~a-~d-~8,'0x" prefix (get-universal-time)
          (funcall *pull-reciprocity-random-fn* #x100000000)))

(defun %pull-atomic-write (path value)
  (ensure-directories-exist path)
  (let ((tmp (make-pathname :name (format nil "~a-tmp" (or (pathname-name path)
                                                             "pull-reciprocity"))
                            :type "json" :defaults path)))
    (with-open-file (out tmp :direction :output :if-exists :supersede
                             :if-does-not-exist :create :external-format :utf-8)
      (let ((*print-pretty* nil))
        (write-string (shasht:write-json value nil) out))
      (terpri out) (finish-output out))
    (uiop:rename-file-overwriting-target tmp path)))

(defun %pull-save ()
  (%pull-atomic-write
   *pull-reciprocity-review-file*
   (obj "schema_version" 2
        "reviewed" (coerce *pull-reciprocity-review* 'vector)
        "descriptions" (coerce *pull-reciprocity-descriptions* 'vector)))
  (%pull-atomic-write
   *pull-reciprocity-label-file*
   (obj "schema_version" 2
        "batches" (coerce *pull-reciprocity-batches* 'vector)
        "labels" (coerce *pull-reciprocity-labels* 'vector)
        "active_label" (or *pull-reciprocity-active-label* :null)))
  t)

(defun %pull-load-one (path key)
  (when (probe-file path)
    (%pull-list (gethash key (shasht:read-json (uiop:read-file-string path))))))

(defun %pull-load ()
  (let ((migration-changed nil))
    (handler-case
      (if (probe-file *pull-reciprocity-review-file*)
          (let ((document
                  (shasht:read-json
                   (uiop:read-file-string *pull-reciprocity-review-file*))))
            (setf *pull-reciprocity-review*
                  (%pull-list (gethash "reviewed" document))
                  *pull-reciprocity-descriptions*
                  (%pull-list (gethash "descriptions" document))))
          (setf *pull-reciprocity-review* nil
                *pull-reciprocity-descriptions* nil))
    (error (condition)
      (format t "~&[pull-reciprocity] review load unavailable: ~a~%" condition)
      (setf *pull-reciprocity-review* nil
            *pull-reciprocity-descriptions* nil)))
    (handler-case
      (if (probe-file *pull-reciprocity-label-file*)
          (let ((document
                  (shasht:read-json
                   (uiop:read-file-string *pull-reciprocity-label-file*))))
            (setf *pull-reciprocity-batches*
                  (%pull-list (gethash "batches" document))
                  *pull-reciprocity-labels*
                  (%pull-list (gethash "labels" document))
                  *pull-reciprocity-active-label*
                  (let ((active (gethash "active_label" document :null)))
                    (if (hash-table-p active) active nil)))
            (dolist (batch *pull-reciprocity-batches*)
              (unless (eq (gethash "quarantined" batch) t)
                (setf (gethash "quarantined" batch) t migration-changed t))
              (unless (string= (or (gethash "label_semantics" batch) "")
                               "superseded-send-worthiness-pair-v1")
                (setf (gethash "label_semantics" batch)
                      "superseded-send-worthiness-pair-v1"
                      migration-changed t))))
          (setf *pull-reciprocity-batches* nil
                *pull-reciprocity-labels* nil
                *pull-reciprocity-active-label* nil))
    (error (condition)
      (format t "~&[pull-reciprocity] label load unavailable: ~a~%" condition)
      (setf *pull-reciprocity-batches* nil
            *pull-reciprocity-labels* nil
            *pull-reciprocity-active-label* nil)))
    (when migration-changed (%pull-save))))

(defun pull-reciprocity-set-mode (mode)
  (unless (member mode '(:enabled :off))
    (error "Pull reciprocity mode must be :ENABLED or :OFF"))
  (setf *pull-reciprocity-mode* mode))

(defun %pull-snapshot ()
  (cond
    ((fboundp 'reciprocity-canary-snapshot)
     (funcall 'reciprocity-canary-snapshot))
    ((fboundp 'reciprocity-canary-records)
     (obj "status" "available"
          "records" (coerce (funcall 'reciprocity-canary-records) 'vector)
          "record_cap" :null "at_cap" nil "pruned_count" :null))
    (t (obj "status" "unavailable" "records" (vector)
            "reason" "reciprocity-source-not-loaded"))))

(defun %pull-sensitive-content-p (content)
  (let ((lower (string-downcase (or content ""))))
    (some (lambda (marker) (search marker lower))
          '("openrouter_api_key" "telegram_bot_token" "runware_api_key"
            "authorization: bearer" "sk-or-" "<thinking>"
            "chain of thought:"))))

(defun %pull-valid-record-p (row)
  (and (hash-table-p row)
       (let ((id (gethash "id" row)) (content (gethash "content" row))
             (status (gethash "status" row)))
         (and (stringp id) (plusp (length id))
              (stringp content) (plusp (length content))
              (<= (length content) 1000)
              (not (%pull-sensitive-content-p content))
              (member status '("withheld" "would-send" "delivery-blocked")
                      :test #'string=)))))

(defun %pull-candidates (snapshot)
  (remove-if-not #'%pull-valid-record-p
                 (%pull-list (gethash "records" snapshot))))

(defun %pull-rejected-count (snapshot)
  (count-if-not #'%pull-valid-record-p
                (%pull-list (gethash "records" snapshot))))

(defun %pull-join (items &optional (separator ", "))
  (if (null items) "none"
      (with-output-to-string (out)
        (loop for item in items for first = t then nil
              unless first do (write-string separator out)
              do (write-string item out)))))

(defun %pull-render-candidate (row ordinal)
  (let ((evidence (remove-if-not #'stringp
                                 (%pull-list (gethash "evidence_node_ids" row)))))
    (format nil
            "~d. [raw audit artifact ~a; class: ~a; generation: ~a]~%~a~%Status: ~a. Why withheld: ~a.~%Source/topic: ~a / ~a.~%Evidence IDs: ~a. Evidence detail: unavailable in this ledger. Why-now detail: unavailable in this ledger. Internal stances are not authored public prose.~%"
            ordinal (gethash "id" row)
            (candidate-artifact-class row)
            (candidate-generation-contract row)
            (gethash "content" row)
            (gethash "status" row) (gethash "reason" row "unavailable")
            (gethash "source" row "unavailable")
            (gethash "topic" row "unavailable") (%pull-join evidence))))

(defun %pull-description-count (now)
  (count-if (lambda (row)
              (>= (gethash "described_at" row 0) (- now (* 24 60 60))))
            *pull-reciprocity-descriptions*))

(defun %pull-record-description (row cause-id channel now)
  (push (obj "candidate_id" (gethash "id" row)
             "artifact_class" (candidate-artifact-class row)
             "generation_contract" (candidate-generation-contract row)
             "described_at" now "inbound_cause_id" cause-id
             "channel" channel)
        *pull-reciprocity-descriptions*)
  (%pull-save))

(defun %pull-render-internal-description (row)
  (format nil
          "I have internal material about ~a. It came from ~a, but it is not authored public prose, so I’m describing it rather than quoting it. If you want to judge whether the material itself is worth developing, use /label. For explicit audit only, use /mind raw N."
          (gethash "topic" row "an unavailable topic")
          (gethash "source" row "an unavailable source")))

(defun %pull-render-draft (row)
  (let ((lint (candidate-send-readiness-lint (gethash "content" row))))
    (format nil
            "[rendered draft; generation: ~a; readiness diagnostic: ~a]~%~a"
            (candidate-generation-contract row)
            (if (gethash "passed" lint) "pass" "fail")
            (gethash "content" row))))

(defun %pull-record-review (rows cause-id channel)
  (let ((now (get-universal-time)))
    (dolist (row rows)
      (let ((id (gethash "id" row)))
        (unless (find id *pull-reciprocity-review* :test #'string=
                      :key (lambda (item) (gethash "candidate_id" item)))
          (push (obj "candidate_id" id "reviewed_at" now
                     "inbound_cause_id" cause-id "channel" channel)
                *pull-reciprocity-review*)))))
  (%pull-save))

(defun %pull-state-prefix (snapshot candidates)
  (let ((status (gethash "status" snapshot "unavailable"))
        (rejected (%pull-rejected-count snapshot)))
    (cond
      ((not (string= status "available"))
       (format nil "Candidate source is unavailable (~a)."
               (gethash "reason" snapshot "unreadable")))
      ((null candidates)
       (format nil "There are no safely reviewable withheld or abstained candidates right now.~@[ ~d retained row(s) were rejected by the disclosure contract.~]"
               (and (plusp rejected) rejected)))
      (t nil))))

(defun %pull-command (text)
  (let ((s (%pull-trim-lower text)))
    (cond
      ((or (string= s "/mind") (string= s "/mind next")
           (member s '("anything on your mind?" "anything on your mind"
                       "what's on your mind?" "what's on your mind"
                       "what’s on your mind?" "what’s on your mind"
                       "what were you thinking of saying?"
                       "what were you thinking of saying") :test #'string=))
       (list :mind 1))
      ((and (>= (length s) 9) (string= (subseq s 0 9) "/mind raw"))
       (let* ((tail (string-trim '(#\Space #\Tab) (subseq s 9)))
              (n (or (and (plusp (length tail))
                          (ignore-errors (parse-integer tail)))
                     *pull-reciprocity-max-raw*)))
         (list :raw (max 1 (min n *pull-reciprocity-max-raw*)))))
      ((member s '("/label" "/triage") :test #'string=) (list :label-start))
      ((member s '("/label status" "/triage status") :test #'string=)
       (list :label-status))
      ((member s '("/label yes" "/label no" "/label unsure"
                   "/triage yes" "/triage no" "/triage unsure")
               :test #'string=)
       (list :label-choice
             (cond ((search " yes" s) "yes")
                   ((search " no" s) "no") (t "unsure"))))
      ((member s '("/label a" "/label b" "/label n" "/label neither"
                   "a" "b" "n" "neither") :test #'string=)
       (list :label-superseded))
      ((string= s "/label preference") (list :preference-unavailable))
      (t nil))))

(defun %pull-find-candidate (id candidates)
  (find id candidates :test #'string=
        :key (lambda (row) (gethash "id" row))))

(defun %pull-label-kind (row)
  (if (string= (candidate-artifact-class row) "rendered-draft")
      "send-ready-floor-v1" "material-triage-v1"))

(defun %pull-labeled-p (row)
  (let ((id (gethash "id" row)) (kind (%pull-label-kind row)))
    (find-if (lambda (label)
               (and (string= id (gethash "candidate_id" label ""))
                    (string= kind (gethash "label_kind" label ""))))
             *pull-reciprocity-labels*)))

(defun %pull-next-label-candidate (candidates)
  (find-if-not #'%pull-labeled-p candidates))

(defun %pull-start-label (row cause-id channel)
  (setf *pull-reciprocity-active-label*
        (obj "candidate_id" (gethash "id" row)
             "artifact_class" (candidate-artifact-class row)
             "generation_contract" (candidate-generation-contract row)
             "label_kind" (%pull-label-kind row)
             "started_at" (get-universal-time)
             "inbound_cause_id" (format nil "~a" cause-id)
             "channel" channel))
  (%pull-save)
  *pull-reciprocity-active-label*)

(defun %pull-render-label (active candidates snapshot)
  (let ((row (and active
                  (%pull-find-candidate (gethash "candidate_id" active)
                                        candidates))))
    (cond
      ((null active) "There is no active typed label. Use /label to start one.")
      ((null row)
       (if (gethash "at_cap" snapshot)
           (let ((pruned (gethash "pruned_count" snapshot :null)))
             (if (integerp pruned)
                 (format nil "This typed artifact is unavailable; the bounded ledger pruned ~d older record(s)." pruned)
                 "This typed artifact is unavailable; the bounded ledger may have pruned it."))
           "This typed artifact is no longer available in the source snapshot."))
      ((string= (gethash "label_kind" active) "material-triage-v1")
       (format nil
               "[internal stance; generation: ~a]~%Topic/source: ~a / ~a.~%This is material, not authored public prose. Is there something here worth saying? Reply /label yes, /label no, or /label unsure."
               (candidate-generation-contract row)
               (gethash "topic" row "unavailable")
               (gethash "source" row "unavailable")))
      (t
       (format nil
               "[rendered draft; generation: ~a]~%~a~%~%Is this send-ready? Reply /label yes, /label no, or /label unsure."
               (candidate-generation-contract row) (gethash "content" row))))))

(defun %pull-label-status ()
  (let ((triage 0) (floor 0) (yes 0) (no 0) (unsure 0))
    (dolist (label *pull-reciprocity-labels*)
      (if (string= (gethash "label_kind" label "") "material-triage-v1")
          (incf triage) (incf floor))
      (cond ((string= (gethash "choice" label "") "yes") (incf yes))
            ((string= (gethash "choice" label "") "no") (incf no))
            (t (incf unsure))))
    (format nil
            "Typed labels: ~d material-triage, ~d send-ready-floor; yes/no/unsure = ~d/~d/~d. Legacy pair batches quarantined: ~d.~@[ One label is active.~]"
            triage floor yes no unsure (length *pull-reciprocity-batches*)
            *pull-reciprocity-active-label*)))

(defun %pull-log (type payload)
  (when (fboundp 'log-event)
    (ignore-errors (funcall 'log-event type payload))))

(defun pull-reciprocity-handle-inbound (text &key cause-id (channel "terminal"))
  "Return a solicited reply object, or NIL when TEXT is not a V0/V1 surface."
  (when (or (eq *pull-reciprocity-mode* :off) (not (stringp text)))
    (return-from pull-reciprocity-handle-inbound nil))
  (let ((command (%pull-command text)))
    (when (and (null command) *pull-reciprocity-active-label*)
      (let ((short (%pull-trim-lower text)))
        (when (member short '("yes" "no" "unsure") :test #'string=)
          (setf command (list :label-choice short)))))
    (unless command (return-from pull-reciprocity-handle-inbound nil))
    (unless (and cause-id (plusp (length (format nil "~a" cause-id))))
      (error "Pull reciprocity requires an inbound cause ID"))
    (let* ((snapshot (%pull-snapshot))
           (candidates (%pull-candidates snapshot))
           (unavailable (%pull-state-prefix snapshot candidates))
           (reply
             (bt:with-lock-held (*pull-reciprocity-lock*)
               (case (first command)
                 ((:mind :raw)
                  (if unavailable unavailable
                      (if (eq (first command) :raw)
                          (let* ((count (second command))
                                 (rows (subseq candidates 0
                                               (min count (length candidates)))))
                            (%pull-record-review rows (format nil "~a" cause-id)
                                                 channel)
                            (with-output-to-string (out)
                              (format out "Explicit raw audit follows. Internal stances are not authored public prose and must not be treated as send-ready.~%~%")
                              (loop for row in rows for i from 1
                                    do (write-string (%pull-render-candidate row i) out)
                                       (terpri out))
                              (when (and (gethash "at_cap" snapshot)
                                         (eq (gethash "pruned_count" snapshot :null)
                                             :null))
                                (format out "The candidate ledger is at its cap; older records may have been pruned, and the old schema does not retain an exact count.~%"))))
                          (let* ((now (get-universal-time))
                                 (row (first candidates)))
                            (if (string= (candidate-artifact-class row)
                                         "internal-stance")
                                (if (>= (%pull-description-count now)
                                        *pull-reciprocity-description-max-per-24-hours*)
                                    "I’m not repeating the same internal-material description again inside the 24-hour cap. Use /label for material triage or /mind raw N for explicit audit."
                                    (progn
                                      (%pull-record-description
                                       row (format nil "~a" cause-id) channel now)
                                      (%pull-render-internal-description row)))
                                (%pull-render-draft row))))))
                 (:label-start
                  (cond ((and unavailable
                              (string/= (gethash "status" snapshot "unavailable")
                                        "available"))
                         unavailable)
                        (*pull-reciprocity-active-label*
                         (%pull-render-label *pull-reciprocity-active-label*
                                             candidates snapshot))
                        (unavailable unavailable)
                        (t
                         (let ((row (%pull-next-label-candidate candidates)))
                           (if row
                               (%pull-render-label
                                (%pull-start-label row cause-id channel)
                                candidates snapshot)
                               "All currently available artifacts already have typed labels.")))))
                 (:label-status (%pull-label-status))
                 (:label-choice
                  (if (null *pull-reciprocity-active-label*)
                      "There is no active typed label. Use /label to start one."
                      (let* ((active *pull-reciprocity-active-label*)
                             (choice (second command))
                             (label
                               (obj "id" (%pull-id "typed-label")
                                    "schema_version" 2
                                    "candidate_id" (gethash "candidate_id" active)
                                    "artifact_class" (gethash "artifact_class" active)
                                    "generation_contract"
                                    (gethash "generation_contract" active)
                                    "label_kind" (gethash "label_kind" active)
                                    "choice" choice
                                    "labeled_at" (get-universal-time)
                                    "inbound_cause_id" (format nil "~a" cause-id)
                                    "channel" channel)))
                        (push label *pull-reciprocity-labels*)
                        (setf *pull-reciprocity-active-label* nil)
                        (%pull-save)
                        (%pull-log "reciprocity-typed-label-recorded" label)
                        (let ((next (%pull-next-label-candidate candidates)))
                          (if next
                              (format nil "Label recorded.~%~%~a"
                                      (%pull-render-label
                                       (%pull-start-label next cause-id channel)
                                       candidates snapshot))
                              "Label recorded. All currently available artifacts now have typed labels.")))))
                 (:label-superseded
                  "A/B/neither pair labels are quarantined because they compared internal stances as public drafts. Use /label for yes/no/unsure material triage.")
                 (:preference-unavailable
                  "Preference mode is unavailable until rendered drafts clear the send-ready floor.")
                 (otherwise nil)))))
      (when reply
        (%pull-log "pull-reciprocity-reply"
                   (obj "surface" (string-downcase (symbol-name (first command)))
                        "channel" channel "inbound_cause_id" (format nil "~a" cause-id))))
      (and reply (obj "kind" "reply" "text" reply
                      "inbound_cause_id" (format nil "~a" cause-id)
                      "channel" channel)))))

(defun pull-reciprocity-present-reply (reply)
  "Present REPLY only on the already-active inbound channel; never initiates."
  (let ((channel (gethash "channel" reply)) (text (gethash "text" reply)))
    (cond ((string= channel "web")
           (when (fboundp '%v2-broadcast) (funcall '%v2-broadcast "final" text)))
          ((string= channel "terminal") (format t "~&~%~a~%" text)))
    text))

(defun pull-reciprocity-report ()
  (let* ((snapshot (%pull-snapshot))
         (candidates (%pull-candidates snapshot))
         (material 0) (send-ready 0)
         (yes 0) (no 0) (unsure 0))
    (dolist (label *pull-reciprocity-labels*)
      (if (string= (gethash "label_kind" label "") "material-triage-v1")
          (incf material) (incf send-ready))
      (cond ((string= (gethash "choice" label "") "yes") (incf yes))
            ((string= (gethash "choice" label "") "no") (incf no))
            ((string= (gethash "choice" label "") "unsure") (incf unsure))))
    (obj "schema_version" 2
         "mode" (string-downcase (symbol-name *pull-reciprocity-mode*))
         "reviewed_candidates" (length *pull-reciprocity-review*)
         "descriptions_24h" (%pull-description-count (get-universal-time))
         "description_max_per_24_hours"
         *pull-reciprocity-description-max-per-24-hours*
         "typed_labels"
         (obj "material_triage" material "send_ready_floor" send-ready
              "yes" yes "no" no "unsure" unsure)
         "active_label"
         (if *pull-reciprocity-active-label*
             (obj "candidate_id"
                  (gethash "candidate_id" *pull-reciprocity-active-label*)
                  "label_kind"
                  (gethash "label_kind" *pull-reciprocity-active-label*))
             :null)
         "quarantined_legacy_batches"
         (count-if (lambda (batch) (gethash "quarantined" batch))
                   *pull-reciprocity-batches*)
         "representation" (candidate-representation-report candidates)
         "preference_capable" nil
         "delivery_capable" nil "model_capable" nil
         "candidate_mutation_capable" nil)))

(define-init :restore pull-reciprocity-restore
    "Restore durable state for pull-reciprocity."
  (%pull-load))
