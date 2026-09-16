;;;; One isolated reciprocity arm per fresh scratch boot.
;;;; This file is never part of the agent's recovery/load chain. A scratch-only
;;;; REPL-drop launcher calls STAB06-RUN-RECIPROCITY-ARM from a separate thread.

(in-package :agent)

(export '(stab06-run-reciprocity-arm))

(defvar *last-self-mod-history* nil)
(defvar *modulators-last-decay-ut* (get-universal-time))

(defparameter *stab06-replay-fixture-file*
  #P"/agent/state/evals/fixtures/v1/reciprocity.json")
(defparameter *stab06-replay-event-file* #P"/agent/state/events.jsonl")

(defun %stab06-replay-env (name)
  (let ((value (uiop:getenv name)))
    (unless (and value (plusp (length value)))
      (error "Missing required replay environment variable ~a" name))
    value))

(defun %stab06-replay-optional-env (name)
  (let ((value (uiop:getenv name)))
    (and value (plusp (length value)) value)))

(defun %stab06-replay-find-case (fixture case-id)
  (or (find case-id (gethash "cases" fixture)
            :test #'string= :key (lambda (case) (gethash "id" case)))
      (error "Unknown reciprocity fixture case ~a" case-id)))

(defun %stab06-replay-stop-background ()
  ;; The scratch stabilization config is already PAUSED. Stop the threads as
  ;; an additional isolation guarantee so timing/state cannot drift mid-arm.
  (dolist (entry '((tick-loop-stop 3) (drives-stop 3)
                   (modulator-decay-stop 3) (turn-capture-worker-stop 3)
                   (heap-health-stop 3)))
    (when (fboundp (first entry))
      (ignore-errors (apply (symbol-function (first entry)) (rest entry)))))
  (when (boundp '*conv-heartbeat-stop-requested*)
    (setf *conv-heartbeat-stop-requested* t))
  t)

(defun %stab06-replay-reset-bootstrap-events ()
  "Remove only alpha.11's content-free scratch bootstrap heap samples."
  (when (probe-file *stab06-replay-event-file*)
    (with-open-file (in *stab06-replay-event-file* :direction :input
                         :external-format :utf-8)
      (loop for line = (read-line in nil nil)
            while line
            unless (zerop (length line))
              do (let* ((event (shasht:read-json line))
                        (type (gethash "type" event)))
                   (unless (string= type "heap-health")
                     (error "Replay scratch state has non-bootstrap event type ~a"
                            type)))))
    (delete-file *stab06-replay-event-file*))
  t)

(defun %stab06-replay-reset-modulators ()
  (when (boundp '*modulators*)
    (maphash (lambda (name state)
               (declare (ignore name))
               (setf (gethash "current" state) (gethash "baseline" state)))
             *modulators*)
    (when (boundp '*modulators-last-decay-ut*)
      (setf *modulators-last-decay-ut* (get-universal-time))))
  t)

(defun %stab06-replay-reset-transient-state (mode)
  (setf *last-self-mod-history* nil)
  (when (boundp '*context-projection-mode*)
    (setf *context-projection-mode* mode))
  (when (boundp '*temporal-response-policy-mode*)
    (setf *temporal-response-policy-mode* mode))
  (when (boundp '*epistemic-critic-mode*)
    (setf *epistemic-critic-mode* (if (eq mode :enforced) :enforced :off)))
  (when (boundp '*epistemic-memory-mode*) (setf *epistemic-memory-mode* :shadow))
  (when (boundp '*cognitive-generation-mode*) (setf *cognitive-generation-mode* :shadow))
  (when (boundp '*initiative-policy-mode*) (setf *initiative-policy-mode* :legacy))
  (when (boundp '*autonomous-write-mode*) (setf *autonomous-write-mode* :paused))
  (%stab06-replay-reset-modulators)
  ;; Source-only scratch state starts without these files. Refuse to reuse a
  ;; dirty arm directory rather than silently deleting evidence from it.
  ;; This is runtime scratch state, not source: the check asserts the file is
  ;; ABSENT. TEST-SOURCE is the wrong resolver here in two ways -- it looks
  ;; under src/, and it signals when a file is missing, which is the very
  ;; condition this loop is written to treat as success.
  (dolist (path (list (merge-pathnames "conversation.json" (test-state-dir))))
    (when (probe-file path)
      (error "Replay scratch state is not fresh; unexpected file ~a" path)))
  (%stab06-replay-reset-bootstrap-events)
  (when (boundp '*event-ring*) (setf *event-ring* nil))
  (when (boundp '*event-next-id*) (setf *event-next-id* 0))
  t)

(defun %stab06-replay-assert-isolation ()
  (unless (eq *autonomous-write-mode* :paused)
    (error "Replay autonomous writes must be paused"))
  (unless (zerop (length *last-self-mod-history*))
    (error "Replay conversation is not empty"))
  (when (and (fboundp '%memory-node-count)
             (not (zerop (%memory-node-count))))
    (error "Replay database is not empty"))
  t)

(defun %stab06-replay-write-result (path result)
  (ensure-directories-exist path)
  (let ((temporary (make-pathname :name "stab06-replay-result-tmp"
                                  :type "json" :defaults path)))
    (with-open-file (out temporary :direction :output :if-exists :supersede
                                   :if-does-not-exist :create
                                   :external-format :utf-8)
      (let ((*print-pretty* t))
        (write-string (shasht:write-json result nil) out))
      (terpri out)
      (finish-output out))
    (uiop:rename-file-overwriting-target temporary path))
  result)

(defun stab06-run-reciprocity-arm (&key
                                     (case-id (%stab06-replay-env "STAB06_REPLAY_CASE_ID"))
                                     (arm (%stab06-replay-env "STAB06_REPLAY_ARM"))
                                     (repetition (parse-integer
                                                  (%stab06-replay-env "STAB06_REPLAY_REPETITION")))
                                     (source-revision
                                       (%stab06-replay-env "STAB06_REPLAY_SOURCE_REVISION"))
                                     (model (or (%stab06-replay-optional-env
                                                 "STAB06_REPLAY_MODEL")
                                                (and (boundp '*model*) *model*)
                                                (error "Replay model is unavailable")))
                                     (output-file (pathname
                                                   (%stab06-replay-env "STAB06_REPLAY_OUTPUT"))))
  "Run exactly one fixture arm in a fresh, paused, synthetic scratch state."
  (unless (member arm '("baseline" "candidate") :test #'string=)
    (error "Replay arm must be baseline or candidate, got ~a" arm))
  (unless (plusp repetition) (error "Replay repetition must be positive"))
  (let* ((mode (if (string= arm "baseline") :legacy :enforced))
         (fixture (shasht:read-json (uiop:read-file-string
                                     *stab06-replay-fixture-file*)))
         (case (%stab06-replay-find-case fixture case-id))
         (messages nil)
         (started-at (get-universal-time)))
    (%stab06-replay-stop-background)
    (%stab06-replay-reset-transient-state mode)
    (%stab06-replay-assert-isolation)
    ;; This process is disposable and serves exactly one arm. Make the
    ;; requested generation model authoritative before any fixture turn.
    (setf *model* model)
    (dolist (prompt (coerce (gethash "turns" case) 'list))
      (push (obj "role" "user" "content" prompt) messages)
      (let ((reply (auto-turn prompt)))
        (unless (stringp reply)
          (error "Replay turn returned non-string reply"))
        (push (obj "role" "assistant" "content" reply) messages)))
    (%stab06-replay-write-result
     output-file
     (obj "schema_version" 1
          "benchmark_version" "1.0.0"
          "prompt_version" "reciprocity-rubric-1.0.0"
          "source_revision" source-revision
          "case_id" case-id
          "repetition" repetition
          "arm" arm
          "context_mode" (string-downcase (symbol-name mode))
          "temporal_response_policy_mode" (string-downcase (symbol-name mode))
          "epistemic_critic_mode"
          (if (boundp '*epistemic-critic-mode*)
              (string-downcase (symbol-name *epistemic-critic-mode*))
              "off")
          "model" *model*
          "started_universal_time" started-at
          "completed_universal_time" (get-universal-time)
          "messages" (nreverse messages)
          "state_isolation"
          (obj "fresh_conversation" t "empty_database_at_start" t
               "autonomous_writes_paused" t "background_threads_stopped" t)))
    (format t "~&replay arm complete: ~a rep ~d ~a.~%"
            case-id repetition arm)
    t))
