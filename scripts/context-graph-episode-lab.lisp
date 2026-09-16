;;;; context-graph-episode-lab.lisp -- line-oriented read-only KG worker.

(require :asdf)
(load (uiop:getenv "PAI_QUICKLISP_SETUP"))
(push #p"/workspace/" asdf:*central-registry*)
(asdf:load-system :pai)
(load #p"/workspace/scripts/context-graph-episode-lab-core.lisp")
(load #p"/workspace/scripts/context-graph-lab-checkpoint.lisp")
(load #p"/workspace/scripts/context-graph-lab-prepare.lisp")
(load #p"/workspace/scripts/context-graph-lab-case.lisp")

(in-package :agent)

(defun %cgel-read-json (text)
  (let ((shasht:*read-default-true-value* t)
        (shasht:*read-default-false-value* nil)
        (shasht:*read-default-null-value* :null))
    (shasht:read-json text)))

(defun %cgel-write-json (value)
  "Serialize one worker frame without imposing authority-canonical value limits."
  (let ((*print-pretty* nil))
    (shasht:write-json value nil)))

(defun %cgel-result (request lab budget-backend budget-policy)
  (let ((operation (gethash "operation" request))
        (through (gethash "through_event_id" request)))
    (cond
      ((string= operation "budget-snapshot")
       (unless budget-backend (error "Worker has no shared budget authority"))
       (context-graph-budget-snapshot budget-backend budget-policy))
      ((string= operation "budget-reserve")
       (unless budget-backend (error "Worker has no shared budget authority"))
       (context-graph-budget-reserve
        budget-backend budget-policy (gethash "reservation_id" request)
        (gethash "request_digest" request) (gethash "phase" request)
        (gethash "reserved_microusd" request)))
      ((string= operation "budget-settle")
       (unless budget-backend (error "Worker has no shared budget authority"))
       (context-graph-budget-settle
        budget-backend budget-policy (gethash "reservation_id" request)
        (gethash "request_digest" request) (gethash "charged_microusd" request)
        (gethash "receipt_id" request)))
      ((string= operation "compare-results")
       (context-graph-lab-compare (gethash "baseline" request) (gethash "candidate" request)
                                (gethash "expected_changes" request)))
      ((string= operation "case-query")
       (context-graph-lab-cached-query (gethash "run_id" request) (gethash "side" request)
                                       (gethash "request" request)))
      ((string= operation "compare-case")
       (let* ((events (gethash "events" request #()))
              (queries (gethash "queries" request #()))
              (frame (gethash "checkpoint" request lab))
              (id (gethash "run_id" request))
              (baseline (context-graph-lab-replay frame events queries nil
                                                 (when id (concatenate 'string id "-baseline"))))
              (candidate (context-graph-lab-replay frame events queries (gethash "counterfactual" request)
                                                  (when id (concatenate 'string id "-candidate")))))
         (obj "baseline" baseline "candidate" candidate
              "comparison" (if (and (equal "complete" (gethash "status" baseline))
                                     (equal "complete" (gethash "status" candidate)))
                               (context-graph-lab-compare baseline candidate (gethash "expected_changes" request))
                               (obj "status" "incomplete" "candidate_status" (gethash "status" candidate))))))
      ((string= operation "replay-case")
       (unless (hash-table-p lab) (error "Replay requires a checkpoint"))
       (context-graph-lab-replay (gethash "checkpoint" request lab) (gethash "events" request #())
                               (gethash "queries" request #())
                               (gethash "counterfactual" request) (gethash "run_id" request)))
      ((string= operation "prepare")
       (unless (hash-table-p lab) (error "Preparation requires compact input"))
       (context-graph-lab-prepare
        lab (coerce (gethash "cuts" request) 'list)
        (lambda (envelope contract status)
          (format t "KG-EPISODE-LAB-CHECKPOINT ~a~%"
                  (%cgel-write-json (obj "envelope" envelope "contract" contract
                                         "digest" (gethash "digest" envelope) "status" status)))
          (force-output))
        :deadline-seconds (gethash "deadline_seconds" request 180)
        :resume (gethash "resume" request)))
      ((string= operation "graph")
       (context-graph-episode-lab-graph lab through))
      ((string= operation "query")
       (context-graph-episode-lab-query
        lab through (gethash "request" request)))
      ((string= operation "delta")
       (context-graph-episode-lab-delta
        lab (gethash "before_event_id" request)
        (gethash "after_event_id" request)))
      (t (error "Unknown episode laboratory operation")))))

(let* ((config (%cgel-read-json
                (uiop:read-file-string
                 (uiop:getenv "PAI_CONTEXT_GRAPH_EPISODE_LAB_CONFIG"))))
       (compact-p (member (gethash "input_kind" config) '("preparation" "checkpoint") :test #'equal))
       (backend (unless compact-p (make-sqlite-storage-read-only (gethash "events" config))))
       (budget-path (gethash "budget_database" config))
       (budget-policy (gethash "budget_policy" config))
       (budget-backend (when budget-path
                         (unless (probe-file budget-path)
                           (error "Configured budget database does not exist"))
                         (make-sqlite-storage budget-path)))
       (lab nil))
  (unwind-protect
       (progn
         (setf lab
               (if compact-p
                   (pai.context-graph::context-graph-runtime-read-json
                    (uiop:read-file-string (gethash "events" config)))
                   (make-context-graph-episode-lab
                backend (gethash "agent_id" config)
                (gethash "persona_id" config)
                :recovery-start-storage-position
                (let ((value (gethash "recovery_start_storage_position"
                                      config :null)))
                  (unless (eq value :null) value)))))
         (format t "KG-EPISODE-LAB-READY~%")
         (force-output)
         (loop for line = (read-line *standard-input* nil nil)
               while line
               do (handler-case
                      (let ((value (%cgel-result (%cgel-read-json line) lab
                                                budget-backend budget-policy)))
                        (format t "KG-EPISODE-LAB-RESULT ~a~%"
                                (%cgel-write-json value)))
                    (error (condition)
                      (format t "KG-EPISODE-LAB-ERROR ~a~%"
                              (%cgel-write-json
                               (obj "error" (princ-to-string condition))))))
                  (force-output)))
    (when backend (storage-close backend))
    (when budget-backend (storage-close budget-backend))))
