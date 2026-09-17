;;;; operational-anomaly-detector.lisp -- pattern detection over durable
;;;; provider-failure events.
;;;;
;;;; First slice of self-issue identification and escalation. See
;;;; docs/self-issue-identification-and-escalation-file-design-20260917.md
;;;; for the full design. This file is detection only: it reads the
;;;; already-replayed event log and returns candidate patterns. It raises
;;;; no curiosity candidate, writes no durable event, and is not yet wired
;;;; into the motivation codelet that would turn a detected pattern into an
;;;; operational-anomaly curiosity motive -- that wiring, and the
;;;; escalation artifact writer, are later slices.

(in-package :agent)

(export '(recursive-operational-anomaly-scan
          *recursive-operational-anomaly-lookback-events*
          *recursive-operational-anomaly-min-occurrences*))

(defparameter *recursive-operational-anomaly-lookback-events* 500
  "How many of the most recent durable events to consider. A bounded count,
not a wall-clock window: event id order is already the total, deterministic
order the rest of this substrate relies on, and this avoids every edge
case of parsing or trusting a timestamp field that not every event carries
the same way.")

(defparameter *recursive-operational-anomaly-min-occurrences* 3
  "How many failures sharing the same code, within the lookback window,
before a pattern is worth raising as a candidate at all. Two failures a
week apart are not a pattern; the model still decides whether a qualifying
pattern is actually worth escalating once raised -- this floor only bounds
what gets put in front of that judgment, it does not replace it.")

(defun %recursive-operational-failure-code (payload)
  "The failure-code field name a durable 'model-response' failure event
carries is not consistent across this substrate's own call sites: the
9 non-live-turn call sites in this file journal it as 'error_code', while
the live conversation-turn boundary (conversation-runtime.lisp) journals
the same fact as 'failure_code'. That split is pre-existing, not
introduced here -- this reads either key so detection is not blind to half
of the substrate's own provider-call sites. Worth reconciling into one
field name eventually; out of scope for detection alone."
  (and (hash-table-p payload)
       (let ((code (or (gethash "error_code" payload)
                        (gethash "failure_code" payload))))
         (and (stringp code) (plusp (length code)) code))))

(defun %recursive-operational-anomaly-event-payload (event)
  "Local, not %RECURSIVE-EVENT-PAYLOAD: this file loads before
recursive-mind-runtime.lisp in the system definition, and a load-order
dependency on it for one trivial accessor is not worth introducing."
  (and (hash-table-p event) (gethash "payload" event)))

(defun %recursive-operational-failure-event-p (event)
  (and (hash-table-p event)
       (equal "model-response" (gethash "type" event))
       (let ((payload (%recursive-operational-anomaly-event-payload event)))
         (and (hash-table-p payload)
              (equal "failed" (gethash "status" payload))
              (%recursive-operational-failure-code payload)))))

(defun recursive-operational-anomaly-scan
    (events &key (lookback *recursive-operational-anomaly-lookback-events*)
                 (min-occurrences
                  *recursive-operational-anomaly-min-occurrences*))
  "Scan durable EVENTS (oldest-first, the order every other replay
projection in this substrate assumes) for a provider failure code that
repeats at least MIN-OCCURRENCES times within the most recent LOOKBACK
events.

Returns a list of candidates, most-recently-failed code first, each an OBJ
with:
  failure_code      -- the repeated code
  occurrence_count  -- how many times it appeared in the window
  first_event_id    -- the earliest contributing event id
  last_event_id     -- the most recent contributing event id
  event_ids         -- every contributing event id, oldest first

Detection only: raises no candidate, writes nothing durable. What this can
see is bounded by what is already journaled -- a final per-call outcome at
every provider-call site, never per-attempt retry timing except at the one
call site that journals it today."
  (unless (and (integerp lookback) (plusp lookback))
    (error "Operational anomaly scan lookback must be a positive integer"))
  (unless (and (integerp min-occurrences) (plusp min-occurrences))
    (error "Operational anomaly scan minimum occurrence must be a positive integer"))
  (let* ((all (coerce events 'list))
         (windowed (last all (min lookback (length all))))
         (failures (remove-if-not #'%recursive-operational-failure-event-p
                                   windowed))
         (ids-by-code (make-hash-table :test 'equal)))
    (dolist (event failures)
      (let ((code (%recursive-operational-failure-code
                   (%recursive-operational-anomaly-event-payload event))))
        (push (gethash "id" event) (gethash code ids-by-code))))
    (let (candidates)
      (maphash
       (lambda (code ids)
         (let ((ordered (sort (copy-list ids) #'<)))
           (when (>= (length ordered) min-occurrences)
             (push (obj "failure_code" code
                        "occurrence_count" (length ordered)
                        "first_event_id" (first ordered)
                        "last_event_id" (car (last ordered))
                        "event_ids" (coerce ordered 'vector))
                   candidates))))
       ids-by-code)
      (stable-sort candidates #'>
                   :key (lambda (candidate)
                          (gethash "last_event_id" candidate))))))
