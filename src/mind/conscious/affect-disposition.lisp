;;;; Q5A1 source experiment: event-derived operational coping, no consumer.
(in-package :agent)
(export '(conscious-affect-disposition-project conscious-affect-inspect-window
          *conscious-affect-coping-policy*))

(defparameter *conscious-affect-coping-policy*
  '(:baseline 500 :setback 50 :recovery 50 :root-limit 100 :decay-per-hour 25)
  "Provisional instrument-scale integers, not calibrated emotions. No live consumer.")

(defun conscious-affect-disposition-project
    (events agent-id mind-id &key now (policy *conscious-affect-coping-policy*))
  "Rebuild from a complete caller-authorized event window; never read host time.
Process completion only repairs prior same-tool operational setbacks."
  (labels ((unavailable (reason)
             (obj "status" "unavailable" "reason" reason "disposition" :null)))
    (unless (and (integerp now) (not (minusp now))
                 (listp events) (<= (length events) 4096)
                 (equal (loop for (key value) on policy by #'cddr
                              collect key)
                        '(:baseline :setback :recovery :root-limit :decay-per-hour))
                 (every (lambda (key) (typep (getf policy key) '(integer 1 1000)))
                        '(:baseline :setback :recovery :root-limit :decay-per-hour)))
      (return-from conscious-affect-disposition-project (unavailable "invalid-input-or-policy")))
    (let ((seen (make-hash-table :test #'equal)))
      (dolist (event events)
        (when (and (hash-table-p event) (equal agent-id (gethash "agent_id" event)))
          (let* ((id (gethash "id" event)) (old (gethash id seen)))
            (when (and old (not (equalp old event)))
              (return-from conscious-affect-disposition-project
                (unavailable "conflicting-source-event-id")))
            (setf (gethash id seen) event)))))
    (let* ((report (conscious-affect-observation-report events agent-id mind-id))
           (rows (sort (coerce (gethash "observations" report) 'list)
                       #'< :key (lambda (row) (gethash "source_event_id" row))))
           (debts (make-hash-table :test #'equal))
           (root-use (make-hash-table :test #'equal))
           (sources nil) (anchor nil) (previous nil))
      (unless (equal "observed-window" (gethash "coverage" report))
        (return-from conscious-affect-disposition-project (unavailable "incomplete-or-empty-coverage")))
      (labels ((total () (loop for value being the hash-values of debts sum value))
               (decay-to (at)
                 (when anchor
                   (let* ((hours (floor (- at anchor) 3600))
                          (drop (* hours (getf policy :decay-per-hour))))
                     (when (plusp hours)
                       (maphash (lambda (tool value)
                                  (setf (gethash tool debts) (max 0 (- value drop)))) debts)
                       (incf anchor (* hours 3600)))))))
        (dolist (row rows)
          (let* ((raw-time (gethash "observed_at" row))
                 (at (if (integerp raw-time) raw-time
                         (and (stringp raw-time) (= 20 (length raw-time))
                              (char= #\Z (char raw-time 19))
                              (ignore-errors (%event-parse-ts-string raw-time)))))
                 (thread (gethash "thread_id" row))
                 (tool (gethash "tool_name" row))
                 (kind (gethash "coping_evidence" row)))
            (unless (and (integerp at) (plusp at) (<= at now)
                         (or (null previous) (<= previous at))
                         (stringp thread) (plusp (length thread))
                         (stringp tool) (plusp (length tool)))
              (return-from conscious-affect-disposition-project (unavailable "invalid-time-or-correlation")))
            (unless anchor (setf anchor at))
            (decay-to at)
            (setf previous at)
            (let* ((used (gethash thread root-use 0))
                   (remaining (max 0 (- (getf policy :root-limit) used)))
                   (delta
                     (cond
                       ((member kind '("execution-obstructed" "means-unavailable" "process-failed") :test #'equal)
                        (min remaining (getf policy :setback)
                             (max 0 (- (getf policy :baseline) (total)))))
                       ((equal kind "process-completed")
                        (- (min remaining (getf policy :recovery) (gethash tool debts 0))))
                       (t 0))))
              (incf (gethash tool debts 0) delta)
              (incf (gethash thread root-use 0) (abs delta))
              (unless (zerop delta) (push (gethash "source_event_id" row) sources)))))
        (decay-to now)
        (obj "status" "projected" "policy" (copy-list policy)
             "agent_id" agent-id "mind_identity_id" mind-id "as_of" now
             "operational_coping_milliunits" (- (getf policy :baseline) (total))
             "source_event_ids" (coerce (nreverse sources) 'vector)
             "certainty" :null "agency" :null "context_injection" :null)))))

(defun conscious-affect-inspect-window
    (baseline-event-id agent-id mind-id &key now through-event-id
                                             (policy *conscious-affect-coping-policy*))
  "Read and inspect one explicit post-baseline authority window without writes.
BASELINE-EVENT-ID is exclusive and must be retained by the launch configuration.
The authority head is captured when THROUGH-EVENT-ID is omitted. NOW is always
caller-supplied so the disposition fold never consults the host clock."
  (labels ((unavailable (reason &optional window)
             (obj "schema_version" 1 "status" "unavailable" "reason" reason
                  "mode" "read-only" "window" (or window :null)
                  "observation_time" (or now :null)
                  "policy" (and (listp policy) (copy-list policy))
                  "observation_report" :null "projection" :null
                  "context_injection" :null)))
    (unless (and (integerp baseline-event-id) (not (minusp baseline-event-id))
                 (integerp now) (not (minusp now))
                 (stringp agent-id) (plusp (length agent-id))
                 (stringp mind-id) (plusp (length mind-id)))
      (return-from conscious-affect-inspect-window
        (unavailable "invalid-window-request")))
    (let* ((authority (and (fboundp 'event-authority-report)
                           (event-authority-report)))
           (authority-agent (and (hash-table-p authority)
                                 (gethash "agent_id" authority)))
           (head (and (hash-table-p authority)
                      (gethash "max_event_id" authority)))
           (through (or through-event-id head))
           (window
             (obj "baseline_event_id_exclusive" baseline-event-id
                  "through_event_id_inclusive" (or through :null)
                  "boundary_source" "operator-launch-configuration-and-authority-head"
                  "event_type" "recursive-tool-result"
                  "maximum_tool_result_events" 4096)))
      (unless (and (equal authority-agent agent-id)
                   (integerp through) (plusp through)
                   (< baseline-event-id through))
        (return-from conscious-affect-inspect-window
          (unavailable "authority-partition-or-window-unavailable" window)))
      (let ((events nil) (overflow nil))
        (multiple-value-bind (complete-p last-event-id visited)
            (map-events
             (lambda (event)
               (if (< (length events) 4097)
                   (push event events)
                   (setf overflow t)))
             :after-id baseline-event-id :through-id through
             :types '("recursive-tool-result"))
          (setf (gethash "tool_result_event_count" window) visited
                (gethash "authority_last_event_id" window)
                (or last-event-id :null))
          (unless (and complete-p (eql last-event-id through))
            (return-from conscious-affect-inspect-window
              (unavailable "incomplete-authority-window" window)))
          (when (or overflow (> visited 4096))
            (return-from conscious-affect-inspect-window
              (unavailable "window-exceeds-event-bound" window)))
          (let* ((ordered (nreverse events))
                 (observations
                   (conscious-affect-observation-report ordered agent-id mind-id))
                 (projection
                   (conscious-affect-disposition-project
                    ordered agent-id mind-id :now now :policy policy)))
            (obj "schema_version" 1
                 "status" (if (equal "projected" (gethash "status" projection))
                              "inspected" "unavailable")
                 "reason" (if (equal "projected" (gethash "status" projection))
                              :null (gethash "reason" projection))
                 "mode" "read-only" "window" window
                 "observation_time" now
                 "policy" (and (listp policy) (copy-list policy))
                 "observation_report" observations "projection" projection
                 "context_injection" :null)))))))
