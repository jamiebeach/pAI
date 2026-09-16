;;;; conscious-tool-operation-runtime-tests.lisp -- durable proposal execution.

(in-package :agent)

(ql:quickload '(:shasht :ironclad) :silent t)

(defvar *ctor-pass* 0)
(defvar *ctor-fail* 0)
(defvar *ctor-events* nil)
(defvar *ctor-calls* 0)
(defvar *ctor-drop-type* nil)
(defvar *conscious-tool-operation-search-fn* nil)
(defparameter *agent-id* "tool-operation-fixture")

(defun ctor-check (name condition)
  (if condition
      (progn (incf *ctor-pass*) (format t "PASS ~a~%" name))
      (progn (incf *ctor-fail*) (format t "FAIL ~a~%" name))))

(defun ctor-signals-p (thunk)
  (handler-case (progn (funcall thunk) nil) (error () t)))

(defun replay-events (&rest ignored)
  (declare (ignore ignored))
  *ctor-events*)

(defun log-event (type payload &key caused-by)
  (let* ((id (1+ (length *ctor-events*)))
         (event (obj "id" id "type" type "agent_id" *agent-id*
                     "caused_by" (or caused-by :null) "payload" payload))
         (durable (not (and *ctor-drop-type*
                            (string= type *ctor-drop-type*)))))
    (when durable (setf *ctor-events* (append *ctor-events* (list event))))
    (values id durable (and durable event))))

(defun ctor-manifest ()
  (obj "pulse_id" "pulse:fixture"
       "runtime_revision" "conscious-tool-test"
       "conscious_state_revision" 4
       "evidence_event_ids" (vector 10)
       "available_tools" (vector "search-files")
       "permitted_proposal_kinds" (vector "tool-call-proposal")
       "remaining_budget"
       (obj "tool_proposals" 1 "continuations" 1
            "publication_candidates" 1)))

(defun ctor-proposal (&optional (tool-name "search-files"))
  (obj "proposal_id" "pulse:fixture:proposal:1"
       "pulse_id" "pulse:fixture"
       "runtime_revision" "conscious-tool-test"
       "conscious_state_revision" 4
       "kind" "tool-call-proposal"
       "created_at_stage" "model-deliberation"
       "confidence" 0.8d0
       "evidence_event_ids" (vector 10)
       "payload"
       (obj "tool_name" tool-name
            "arguments"
            (obj "query" "needle" "path" "." "max_results" 3))))

(format t "~%== durable conscious tool operation adapter ==~%")

(let ((subject (merge-pathnames "src/mind/conscious/tool-operation-runtime.lisp"
                                *pai-root*)))
  (ctor-check "tool-operation runtime source exists" (probe-file subject))
  (when (probe-file subject)
    (load (test-source "proposal.lisp"))
    (load (test-source "conscious-file-search-tool.lisp"))
    (load subject)
    (let ((*conscious-tool-operation-search-fn*
            (lambda (arguments)
              (declare (ignore arguments))
              (incf *ctor-calls*)
              (obj "schema_version" 1 "status" "ok"
                   "matches" (vector) "database_write_count" 0))))
      (setf *ctor-events* nil *ctor-calls* 0 *ctor-drop-type* nil)
      (let ((first (conscious-tool-operation-run
                    (ctor-proposal) (ctor-manifest)
                    :interaction-id "interaction:fixture"
                    :user-event-id 10))
            (second nil))
        (setf second
              (conscious-tool-operation-run
               (ctor-proposal) (ctor-manifest)
               :interaction-id "interaction:fixture" :user-event-id 10))
        (ctor-check "validated proposal executes its handler once"
                    (= 1 *ctor-calls*))
        (ctor-check "durable result makes retry idempotent"
                    (equalp first second))
        (ctor-check "claim precedes one durable result"
                    (equal '("conscious-tool-operation-claimed"
                             "conscious-tool-operation-result")
                           (mapcar (lambda (event) (gethash "type" event))
                                    *ctor-events*))))
        (let* ((claim (first *ctor-events*))
               (payload (gethash "payload" claim)))
          (ctor-check "claim carries a bounded durable recovery lease"
                      (and (integerp (gethash "claimed_at" payload))
                           (= *conscious-tool-operation-lease-seconds*
                              (- (gethash "lease_expires_at" payload)
                                 (gethash "claimed_at" payload))))))
      (setf *ctor-events* nil *ctor-calls* 0)
      (ctor-check "unadvertised tool fails before claim"
                  (and (ctor-signals-p
                        (lambda ()
                          (conscious-tool-operation-run
                           (ctor-proposal "write-file") (ctor-manifest)
                           :interaction-id "interaction:fixture"
                           :user-event-id 10)))
                       (null *ctor-events*) (zerop *ctor-calls*)))
      (setf *ctor-events* nil *ctor-calls* 0
            *ctor-drop-type* "conscious-tool-operation-claimed")
      (ctor-check "unreadable claim prevents execution"
                  (and (ctor-signals-p
                        (lambda ()
                          (conscious-tool-operation-run
                           (ctor-proposal) (ctor-manifest)
                           :interaction-id "interaction:fixture"
                           :user-event-id 10)))
                       (zerop *ctor-calls*)))
      (setf *ctor-events*
            (list
             (obj "id" 1 "type" "conscious-tool-operation-claimed"
                  "agent_id" "another-mind" "caused_by" 10
                  "payload"
                  (obj "operation_id"
                       "tool-operation:pulse:fixture:proposal:1"))))
      (setf *ctor-calls* 0 *ctor-drop-type* nil)
      (conscious-tool-operation-run
       (ctor-proposal) (ctor-manifest)
       :interaction-id "interaction:fixture" :user-event-id 10)
      (ctor-check "another mind's matching operation cannot block execution"
                  (= 1 *ctor-calls*))
      (setf *ctor-events* nil *ctor-calls* 0 *ctor-drop-type* nil)
      (log-event
       "conscious-tool-operation-claimed"
       (obj "operation_id" "tool-operation:pulse:fixture:proposal:1"
            "proposal_id" "pulse:fixture:proposal:1"
            "interaction_id" "interaction:fixture"
            "user_event_id" 10 "tool_name" "search-files"
            "arguments_hash" "fixture" "attempt" 1)
       :caused-by 10)
      (ctor-check "claim without terminal is not blindly re-executed"
                  (and (ctor-signals-p
                        (lambda ()
                          (conscious-tool-operation-run
                           (ctor-proposal) (ctor-manifest)
                           :interaction-id "interaction:fixture"
                           :user-event-id 10)))
                       (zerop *ctor-calls*)))
      (setf *ctor-events* nil *ctor-calls* 0 *ctor-drop-type* nil)
      (let ((result
              (handler-case
                  (conscious-tool-operation-run
                   (ctor-proposal) (ctor-manifest)
                   :work-id "work:fixture" :user-event-id 10)
                (error () nil))))
        (ctor-check "generic work can own a tool without a conversation"
                    (hash-table-p result))
        (when (hash-table-p result)
          (let* ((event (find "conscious-tool-operation-result" *ctor-events*
                              :key (lambda (row) (gethash "type" row))
                              :test #'string=))
                 (payload (and event (gethash "payload" event))))
            (ctor-check "tool result carries work lineage and bounded size"
                        (and (string= "work:fixture"
                                      (gethash "work_id" payload ""))
                             (integerp (gethash "result_characters" payload))))))))
      (let* ((bounded-result
               (obj "schema_version" 1 "status" "ok"
                    "matches" (vector) "database_write_count" 0
                    "padding" (make-string 40 :initial-element #\b)))
             (canonical
               (%conscious-tool-operation-canonical-json bounded-result))
             (exact (length canonical))
             (*conscious-tool-operation-search-fn*
               (lambda (arguments)
                 (declare (ignore arguments)) bounded-result)))
        (setf *ctor-events* nil *ctor-calls* 0 *ctor-drop-type* nil)
        (conscious-tool-operation-run
         (ctor-proposal) (ctor-manifest)
         :interaction-id "interaction:bounded" :user-event-id 10
         :max-result-characters exact)
        (ctor-check "canonical result at its admitted maximum commits"
                    (= 1 (count "conscious-tool-operation-result"
                                *ctor-events*
                                :key (lambda (event) (gethash "type" event))
                                :test #'string=)))
        (setf *ctor-events* nil *ctor-calls* 0 *ctor-drop-type* nil)
        (ctor-check "one byte beyond the result bound fails before success commit"
                    (and
                     (ctor-signals-p
                      (lambda ()
                        (conscious-tool-operation-run
                         (ctor-proposal) (ctor-manifest)
                         :interaction-id "interaction:bounded"
                         :user-event-id 10
                         :max-result-characters (1- exact))))
                     (null (find "conscious-tool-operation-result"
                                 *ctor-events*
                                 :key (lambda (event) (gethash "type" event))
                                 :test #'string=))
                     (let* ((failed
                              (find "conscious-tool-operation-failed"
                                    *ctor-events*
                                    :key (lambda (event)
                                           (gethash "type" event))
                                    :test #'string=))
                            (payload (and failed (gethash "payload" failed))))
                       (and payload
                            (string= "tool-result-bound-exceeded"
                                     (gethash "error_code" payload "")))))))
    ))

(format t "~%~d passed, ~d failed~%" *ctor-pass* *ctor-fail*)
(when (plusp *ctor-fail*) (error "tool-operation runtime tests failed"))
