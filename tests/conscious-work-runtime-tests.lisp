;;;; conscious-work-runtime-tests.lisp -- exact durable work lifecycle adapter.

(in-package :agent)

(ql:quickload '(:shasht :ironclad) :silent t)

(defvar *cwrt-pass* 0)
(defvar *cwrt-fail* 0)
(defvar *cwrt-events* nil)
(defvar *cwrt-drop-type* nil)
(defvar *cwrt-replays* 0)
(defparameter *agent-id* "work-runtime-fixture")
(declaim
 (special *conscious-work-runtime-projection-cache-hits*
          *conscious-work-runtime-projection-cache-misses*
          *conscious-work-runtime-projection-cache-rebuilds*
          *conscious-work-runtime-projection-cache-advances*
          *conscious-work-runtime-projection-cache-fallbacks*
          *conscious-work-runtime-projection-cache-tail-events*))

(defun cwrt-check (name condition)
  (if condition
      (progn (incf *cwrt-pass*) (format t "PASS ~a~%" name))
      (progn (incf *cwrt-fail*) (format t "FAIL ~a~%" name))))

(defun cwrt-signals-p (thunk)
  (handler-case (progn (funcall thunk) nil) (error () t)))

(defun replay-events (&rest ignored)
  (declare (ignore ignored))
  (incf *cwrt-replays*)
  *cwrt-events*)

(defun log-event (type payload &key caused-by)
  (let* ((id (1+ (length *cwrt-events*)))
         (event (obj "id" id "type" type "agent_id" *agent-id*
                     "timestamp" id "caused_by" (or caused-by :null)
                     "payload" payload))
         (durable (not (and *cwrt-drop-type*
                            (string= type *cwrt-drop-type*)))))
    (when durable (setf *cwrt-events* (append *cwrt-events* (list event))))
    (values id durable (and durable event))))

(defun cwrt-profile ()
  (obj "profile_id" "interactive-dev" "revision" 1
       "max_model_calls" 8 "max_tool_operations" 6
       "max_reasoning_continuations" 3
       "max_tool_result_characters" 12000
       "permitted_proposal_kinds"
       (vector "tool-call-proposal" "publication-candidate"
               "request-continuation" "yield" "abstain")
       "permitted_tools" (vector "search-files")
       "budget_exhaustion" "suspend" "renewal_policy" "explicit-only"))

(defun cwrt-state ()
  (obj "focus"
       (obj "value"
            (obj "coalition_key" "agent|operator|grounded#solo:user"
                 "priority_class" "direct" "explanation_code" "direct-address"
                 "member_count" 1 "evidence_ids" (vector "stimulus:1")))))

(defun cwrt-transition (work-id transition reason-code)
  (let* ((projection (conscious-work-runtime-project))
         (work (gethash work-id (gethash "items" projection))))
    (conscious-work-runtime-transition
     work-id transition :reason-code reason-code
     :expected-state (gethash "state" work)
     :expected-revision (gethash "projection_revision" work))))

(format t "~%== durable cognitive work runtime ==~%")

(let ((subject (merge-pathnames "src/mind/conscious/cognitive-work-runtime.lisp"
                                *pai-root*)))
  (cwrt-check "cognitive work runtime source exists" (probe-file subject))
  (when (probe-file subject)
    (load (test-source "policy.lisp"))
    (load (test-source "stimulus.lisp"))
    (load (test-source "cognitive-work.lisp"))
    (load subject)
    (let ((*cwrt-events* nil)
          (*cwrt-replays* 0)
          (physical-head 10))
      (conscious-work-runtime-configure-head-position
       (lambda () physical-head))
      (let ((first (conscious-work-runtime-project)))
        (setf (gethash "item_count" first) 999)
        (let ((second (conscious-work-runtime-project)))
          (cwrt-check "unchanged physical head reuses one projected ledger"
                      (and (= 1 *cwrt-replays*)
                           (= 0 (gethash "item_count" second))))
          (cwrt-check "memoized projections are returned as detached values"
                      (= 0 (gethash "item_count" second)))))
      (let ((first (%conscious-work-runtime-project-shared))
            (second (%conscious-work-runtime-project-shared)))
        (cwrt-check "audited internal readers share the immutable generation"
                    (and (eq first second) (= 1 *cwrt-replays*))))
      ;; Physical position, not a logical MAX(id), is the cache identity.
      ;; Advancing it must force replay even when the fixture's logical event
      ;; set remains unchanged.
      (incf physical-head)
      (conscious-work-runtime-project)
      (cwrt-check "physical head advance invalidates the projected ledger"
                  (= 2 *cwrt-replays*)))
    (let ((*cwrt-events*
            (list (obj "id" 1 "type" "model-request"
                       "agent_id" *agent-id*
                       "payload" (obj "legacy_payload" "PRIVATE-CANARY"))))
          (*cwrt-replays* 0)
          (physical-head 1)
          (tail-after nil))
      (conscious-work-runtime-configure-head-position
       (lambda () physical-head)
       (lambda (after through types)
         (declare (ignore types))
         (setf tail-after after)
         (values (subseq *cwrt-events* after through) through))
       1)
      (let ((projection (%conscious-work-runtime-project-shared)))
        (cwrt-check "sealed import boundary excludes source-runtime cognition"
                    (and (= 1 tail-after)
                         (= 0 *cwrt-replays*)
                         (= 0 (gethash "item_count" projection)))))
      (log-event
       "conscious-work-opened"
       (obj "schema_version" 1 "work_id" "work:post-import"
            "concern_identity" "concern:post-import"
            "stimulus_ids" (vector "stimulus:1") "purpose" "respond"
            "priority_class" "direct" "urgency_class" "interactive"
            "deadline" :null "opened_at" 2 "profile" (cwrt-profile)))
      (incf physical-head)
      (let* ((projection (%conscious-work-runtime-project-shared))
             (work (gethash "work:post-import" (gethash "items" projection))))
        (cwrt-check "post-import cognitive work remains live recovery authority"
                    (and (hash-table-p work)
                         (string= "runnable" (gethash "state" work "")))))
      (cwrt-check "import-boundary projection never falls back to full replay"
                  (= 0 *cwrt-replays*))
      (conscious-work-runtime-configure-head-position nil))
    (let ((*cwrt-events* nil)
          (*cwrt-replays* 0)
          (*conscious-work-runtime-projection-cache-hits* 0)
          (*conscious-work-runtime-projection-cache-misses* 0)
          (*conscious-work-runtime-projection-cache-rebuilds* 0)
          (*conscious-work-runtime-projection-cache-advances* 0)
          (*conscious-work-runtime-projection-cache-fallbacks* 0)
          (*conscious-work-runtime-projection-cache-tail-events* 0))
      (log-event
       "conscious-work-opened"
       (obj "schema_version" 1 "work_id" "work:tail-runtime"
            "concern_identity" "concern:tail-runtime"
            "stimulus_ids" (vector "stimulus:1") "purpose" "respond"
            "priority_class" "direct" "urgency_class" "interactive"
            "deadline" :null "opened_at" 1 "profile" (cwrt-profile)))
      (conscious-work-runtime-configure-head-position
       (lambda () (length *cwrt-events*))
       (lambda (after through types)
         (values
          (loop for event in (subseq *cwrt-events* after through)
                when (member (gethash "type" event) types :test #'string=)
                  collect event)
          through)))
      (setf *cwrt-replays* 0)
      (let* ((held (%conscious-work-runtime-project-shared))
             (held-json (%conscious-work-canonical-json held)))
        (log-event "model-request"
                   (obj "work_id" "work:tail-runtime"
                        "pulse_id" "pulse:tail"))
        (%conscious-work-runtime-project-shared)
        (log-event
         "pulse-committed"
         (obj "work_id" "work:tail-runtime" "pulse_id" "pulse:tail"
              "pulse_sequence" 1 "proposals"
              (vector (obj "proposal_id" "proposal:tail"
                           "kind" "publication-candidate"))))
        (%conscious-work-runtime-project-shared)
        (log-event "conscious-work-completed"
                   (obj "work_id" "work:tail-runtime"
                        "reason_code" "reply-committed"))
        (%conscious-work-runtime-project-shared)
        (cwrt-check "changed physical heads advance without full replay"
                    (and (= 1 *cwrt-replays*)
                         (= 1 *conscious-work-runtime-projection-cache-rebuilds*)
                         (= 3 *conscious-work-runtime-projection-cache-advances*)
                         (= 0 *conscious-work-runtime-projection-cache-fallbacks*)
                         (= 3 *conscious-work-runtime-projection-cache-tail-events*)))
        (cwrt-check "held shared generation survives a multi-boundary turn"
                    (string= held-json
                             (%conscious-work-canonical-json held)))
        (let* ((public (conscious-work-runtime-project))
               (work (gethash "work:tail-runtime"
                              (gethash "items" public))))
          (cwrt-check "runtime public projection hides sufficient state"
                      (and (string= "completed" (gethash "state" work ""))
                           (every
                            (lambda (key)
                              (not (nth-value 1 (gethash key work))))
                            *conscious-work-private-item-keys*)))))
      (conscious-work-runtime-configure-head-position nil))
    (let ((*cwrt-events* nil)
          (*cwrt-replays* 0)
          (*conscious-work-runtime-projection-cache-rebuilds* 0)
          (*conscious-work-runtime-projection-cache-advances* 0)
          (*conscious-work-runtime-projection-cache-fallbacks* 0))
      (log-event
       "conscious-work-opened"
       (obj "schema_version" 1 "work_id" "work:tail-fallback"
            "concern_identity" "concern:tail-fallback"
            "stimulus_ids" (vector "stimulus:1") "purpose" "respond"
            "priority_class" "direct" "urgency_class" "interactive"
            "deadline" :null "opened_at" 1 "profile" (cwrt-profile)))
      (conscious-work-runtime-configure-head-position
       (lambda () (length *cwrt-events*))
       (lambda (after through types)
         (declare (ignore after types))
         (values nil (1- through))))
      (%conscious-work-runtime-project-shared)
      (log-event "conscious-work-suspended"
                 (obj "work_id" "work:tail-fallback"
                      "reason_code" "fallback-probe"))
      (let* ((projection (%conscious-work-runtime-project-shared))
             (work (gethash "work:tail-fallback"
                            (gethash "items" projection))))
        (cwrt-check "incomplete physical tail falls back to full replay"
                    (and (string= "suspended" (gethash "state" work ""))
                         (= 2 *cwrt-replays*)
                         (= 2 *conscious-work-runtime-projection-cache-rebuilds*)
                         (= 0 *conscious-work-runtime-projection-cache-advances*)
                         (= 1 *conscious-work-runtime-projection-cache-fallbacks*))))
      (conscious-work-runtime-configure-head-position nil))
    (let ((*cwrt-events* nil)
          (*cwrt-replays* 0)
          (*conscious-work-runtime-projection-cache-rebuilds* 0)
          (*conscious-work-runtime-projection-cache-advances* 0)
          (*conscious-work-runtime-projection-cache-fallbacks* 0))
      (log-event
       "conscious-work-opened"
       (obj "schema_version" 1 "work_id" "work:fold-fallback"
            "concern_identity" "concern:fold-fallback"
            "stimulus_ids" (vector "stimulus:1") "purpose" "respond"
            "priority_class" "direct" "urgency_class" "interactive"
            "deadline" :null "opened_at" 1 "profile" (cwrt-profile)))
      (conscious-work-runtime-configure-head-position
       (lambda () (length *cwrt-events*))
       (lambda (after through types)
         (declare (ignore after types))
         ;; Complete physical closure with semantically invalid fold content.
         (values
          (list (obj "id" 999 "type" "model-request"
                     "agent_id" *agent-id*
                     "payload" (obj "work_id" "work:absent"
                                    "pulse_id" "pulse:absent")))
          through)))
      (%conscious-work-runtime-project-shared)
      (log-event "conscious-work-suspended"
                 (obj "work_id" "work:fold-fallback"
                      "reason_code" "fold-fallback-probe"))
      (let* ((projection (%conscious-work-runtime-project-shared))
             (work (gethash "work:fold-fallback"
                            (gethash "items" projection))))
        (cwrt-check "invalid physical tail fold falls back to full replay"
                    (and (string= "suspended" (gethash "state" work ""))
                         (= 2 *cwrt-replays*)
                         (= 2 *conscious-work-runtime-projection-cache-rebuilds*)
                         (= 0 *conscious-work-runtime-projection-cache-advances*)
                         (= 1 *conscious-work-runtime-projection-cache-fallbacks*))))
      (conscious-work-runtime-configure-head-position nil))
    (conscious-work-runtime-configure-head-position
     (lambda () (length *cwrt-events*)))
    (setf (gethash "user-message" *stimulus-kind-map*)
          '("user-message" "channel" "interactive" t))
    (setf *cwrt-events*
          (list (obj "id" 1 "type" "user-message"
                     "agent_id" *agent-id* "timestamp" 1
                     "payload" (obj "text" "PRIVATE-CANARY"))))
    (let* ((first (conscious-work-runtime-open-selected
                   (cwrt-state) (cwrt-profile) :purpose "respond" :opened-at 10))
           (second (conscious-work-runtime-open-selected
                    (cwrt-state) (cwrt-profile) :purpose "respond" :opened-at 99))
           (work-id (gethash "work_id" first)))
      (cwrt-check "selected stimulus opens exactly one durable work item"
                  (= 1 (count "conscious-work-opened" *cwrt-events*
                              :key (lambda (event) (gethash "type" event))
                              :test #'string=)))
      (cwrt-check "restart-time open retry recovers the same work identity"
                  (and (string= "opened" (gethash "status" first ""))
                       (string= "recovered" (gethash "status" second ""))
                       (string= work-id (gethash "work_id" second ""))))
      (cwrt-check "durable work envelope contains no stimulus content"
                  (null (search "PRIVATE-CANARY"
                                (shasht:write-json
                                 (find "conscious-work-opened" *cwrt-events*
                                       :key (lambda (event) (gethash "type" event))
                                       :test #'string=)
                                 nil))))
      (let ((lineage (conscious-work-runtime-events-for-work work-id)))
        (cwrt-check "work lookup joins its post-stimulus open to the exact root"
                    (and (= 2 (length lineage))
                         (equal '("user-message" "conscious-work-opened")
                                (mapcar (lambda (event)
                                          (gethash "type" event))
                                        lineage)))))
      (let ((projection (conscious-work-runtime-project)))
        (cwrt-check "runtime projection exposes runnable selected work"
                    (string= work-id
                             (gethash "work_id"
                                      (conscious-work-select projection) ""))))
      (let* ((observed (gethash work-id
                                (gethash "items"
                                         (conscious-work-runtime-project))))
             (stale-state (gethash "state" observed))
             (stale-revision (gethash "projection_revision" observed)))
        (cwrt-transition work-id "suspended" "higher-priority-work")
        (cwrt-check "stale observed revision cannot append a competing transition"
                    (cwrt-signals-p
                     (lambda ()
                       (conscious-work-runtime-transition
                        work-id "failed" :reason-code "stale-writer"
                        :expected-state stale-state
                        :expected-revision stale-revision)))))
      (cwrt-transition work-id "resumed" "boundary-clear")
      (cwrt-check "explicit suspension and resumption are durable"
                  (string= "runnable"
                           (gethash "state"
                                    (gethash work-id
                                             (gethash "items"
                                                      (conscious-work-runtime-project)))
                                    "")))
      (cwrt-check "completed transition fails before completing disposition"
                  (cwrt-signals-p
                   (lambda ()
                     (cwrt-transition work-id "completed" "invalid-early")))))
    (setf *cwrt-events*
          (list (obj "id" 1 "type" "user-message"
                     "agent_id" *agent-id* "timestamp" 1
                     "payload" (obj "text" "lease fixture"))))
    (let* ((opened
             (conscious-work-runtime-open-direct-event
              1 (cwrt-profile) :opened-at 10))
           (work-id (gethash "work_id" opened)))
      (log-event "model-request"
                 (obj "work_id" work-id "pulse_id" "pulse:lease"
                      "requested_at" 20 "lease_expires_at" 30)
                 :caused-by 1)
      (cwrt-check "unexpired provider request remains durably deliberating"
                  (progn
                    (conscious-work-runtime-reap-expired-model-leases 29)
                    (string= "deliberating"
                             (gethash "state"
                                      (gethash work-id
                                               (gethash "items"
                                                        (conscious-work-runtime-project)))
                                      ""))))
      (cwrt-check "expired provider request is terminalized by runtime owner"
                  (progn
                    (conscious-work-runtime-reap-expired-model-leases 30)
                    (string= "outcome-unknown"
                             (gethash "state"
                                      (gethash work-id
                                               (gethash "items"
                                                        (conscious-work-runtime-project)))
                                      "")))))
    (setf *cwrt-drop-type* "conscious-work-opened"
          *cwrt-events*
          (list (obj "id" 1 "type" "user-message"
                     "agent_id" *agent-id* "timestamp" 1
                     "payload" (obj "text" "fixture"))))
    (cwrt-check "unreadable work open cannot claim success"
                (cwrt-signals-p
                 (lambda ()
                   (conscious-work-runtime-open-selected
                    (cwrt-state) (cwrt-profile)
                    :purpose "respond" :opened-at 10))))))

(format t "~%~d passed, ~d failed~%" *cwrt-pass* *cwrt-fail*)
(when (plusp *cwrt-fail*) (error "conscious work runtime tests failed"))
