;;;; conscious-lifecycle-scenario-core.lisp -- providerless Q5 operator actions.
;;;;
;;;; This is loaded by the native CLI and its isolated suite. It drives the
;;;; existing near-term producer and Q5 reconciliation boundary; it owns no
;;;; worker, provider, effect or publication route.

(in-package :agent)

(defvar *conscious-conversation-persona-profile* nil)
(declaim (ftype function %lifecycle-scenario-runtime-revision))

(export '(conscious-lifecycle-scenario-run))

(defun %lifecycle-scenario-text (value name &optional (maximum 1024))
  (unless (and (stringp value) (plusp (length value))
               (<= (length value) maximum))
    (error "Q5 scenario ~a must be bounded text" name))
  value)

(defun %lifecycle-scenario-records ()
  ;; NOW=0 prevents an inspection command from turning passage of wall time
  ;; into an implicit expiry mutation. This CLI changes state only by its
  ;; named operator action.
  (near-term-intention-records :now 0))

(defun %lifecycle-scenario-active-record ()
  (first (near-term-intention-records :active-only t :now 0)))

(defun %lifecycle-scenario-current-record ()
  (or (%lifecycle-scenario-active-record)
      (first (%lifecycle-scenario-records))))

(defun %lifecycle-scenario-mind-identity-id ()
  ;; Persona ID is continuity identity; the mutable profile fingerprint is not.
  (if (and (boundp '*conscious-conversation-persona-profile*)
           (hash-table-p *conscious-conversation-persona-profile*))
      (gethash "persona_id" *conscious-conversation-persona-profile* "dev")
      "dev"))

(defun %lifecycle-scenario-create (subject aim now)
  (let* ((subject (%lifecycle-scenario-text subject "subject" 512))
         (aim (%lifecycle-scenario-text aim "aim" 512))
         (origin-id
           (log-event
            "conscious-lifecycle-command-requested"
            (obj "schema_version" 1 "command" "create"
                 "channel" "q5-lifecycle-cli" "content_persisted" nil
                 "origin_runtime_revision" "conscious-q5-scenario-v1"))))
    (unless origin-id (error "Q5 scenario origin event did not append"))
    (multiple-value-bind (record reason source-event-id)
        (near-term-intention-create
         subject aim (format nil "q5-scenario-turn:~a" origin-id)
         (list origin-id) 300 :completion-mode :manual-observation :now now)
      (unless record
        (error "Q5 scenario producer refused creation: ~a" reason))
      (unless source-event-id
        (error "Q5 scenario creation event was not durably receipted"))
      (conscious-lifecycle-semantic-runtime-describe
       (format nil "near-term:~a" (gethash "id" record))
       "deferred-intention" (%lifecycle-scenario-mind-identity-id)
       "topic" subject aim source-event-id
       :source-revision "near-term-intentions-v1"
       :actor-runtime-revision (%lifecycle-scenario-runtime-revision)
       :disclosure-class "private-provider-eligible")
      record)))

(defun %lifecycle-scenario-transition (action result-summary observed-reply now)
  (let ((record (%lifecycle-scenario-active-record)))
    (unless record (error "Q5 scenario has no active intention"))
    (let ((id (gethash "id" record)))
      (multiple-value-bind (updated reason source-event-id)
          (cond
            ((string= action "ready")
             (near-term-intention-transition
              id "ready" "operator-observed-ready"
              :artifact-summary
              (%lifecycle-scenario-text result-summary "result summary" 2048)
              :now now))
            ((string= action "cancel")
             (near-term-intention-transition
              id "discarded" "operator-cancelled" :now now))
            ((string= action "complete")
             (near-term-intention-observe-public-reply
              (%lifecycle-scenario-text observed-reply "observed reply" 4096)
              (format nil "q5-scenario-observation:~a" now) :now now))
            (t (error "Unknown Q5 scenario transition ~s" action)))
        (unless updated
          (error "Q5 scenario ~a was refused: ~a" action reason))
        (when (string= action "ready")
          (unless source-event-id
            (error "Q5 scenario ready event was not durably receipted"))
          (conscious-lifecycle-semantic-runtime-add-result
           (format nil "near-term:~a" id)
           (%lifecycle-scenario-mind-identity-id) result-summary source-event-id
           :actor-runtime-revision (%lifecycle-scenario-runtime-revision)))
        updated))))

(defun %lifecycle-scenario-runtime-revision ()
  (if (and (boundp '*conscious-cognition-runtime-revision*)
           (stringp (symbol-value '*conscious-cognition-runtime-revision*)))
      (symbol-value '*conscious-cognition-runtime-revision*)
      "conscious-q5-v2"))

(defun %lifecycle-scenario-result (action record)
  (let* ((cached-inspection-p
           (and (string= action "inspect")
                (hash-table-p *conscious-lifecycle-runtime-projection*)
                (equal *agent-id* *conscious-lifecycle-runtime-agent-id*)))
         (reconciliation
           (unless cached-inspection-p
             (multiple-value-list
              (conscious-lifecycle-runtime-reconcile-producer-events
               *agent-id*
               :actor-runtime-revision
               (%lifecycle-scenario-runtime-revision)))))
         (report
           (if cached-inspection-p
               (conscious-lifecycle-source-report)
               (first reconciliation)))
         ;; Reconciliation installs the projection it just built. An inspect
         ;; in an already-restored process is a pure read of that projection.
         (projection *conscious-lifecycle-runtime-projection*)
         (ignored-semantic-refresh
           (when (and reconciliation (second reconciliation))
             (setf *conscious-lifecycle-semantic-runtime-projection*
                   (conscious-lifecycle-semantic-project
                    (second reconciliation) :agent-id *agent-id*))))
         (intention-id (and record (gethash "id" record)))
         (lifecycle-id (and intention-id (format nil "near-term:~a" intention-id)))
         (lifecycle
           (and lifecycle-id
                (conscious-lifecycle-current projection lifecycle-id)))
         (awaiting (conscious-lifecycle-awaiting projection))
         (awaiting-row
           (and lifecycle-id
                (find lifecycle-id awaiting
                      :key (lambda (row) (gethash "lifecycle_id" row))
                      :test #'string=)))
         (semantic-selection
           (multiple-value-list
            (if (and (boundp '*conscious-lifecycle-semantic-runtime-projection*)
                     (hash-table-p
                      *conscious-lifecycle-semantic-runtime-projection*))
                (conscious-lifecycle-semantic-context-records
                 awaiting *conscious-lifecycle-semantic-runtime-projection*
                 :mind-identity-id (%lifecycle-scenario-mind-identity-id)
                 :purpose "orient" :audience "operator" :channel "terminal"
                 :provider-class "local")
                (values (conscious-lifecycle-context-records awaiting)
                        (vector) (vector)))))
         (contexts (first semantic-selection))
         (semantic-refusals (second semantic-selection)))
    (declare (ignore ignored-semantic-refresh))
    (unless (hash-table-p projection)
      (error "Q5 scenario lifecycle projection is unavailable"))
    (obj "schema_version" 1 "status" "ok" "action" action
         "intention_id" (or intention-id :null)
         "producer_state" (if record (gethash "state" record) :null)
         "lifecycle_id" (or lifecycle-id :null)
         "lifecycle_status"
         (if lifecycle (gethash "status" lifecycle) :null)
         "phase" (if awaiting-row (gethash "phase" awaiting-row) :null)
         "awaiting_count" (length awaiting)
         "awaiting" awaiting
         "context_records" contexts
         "semantic_refusals" semantic-refusals
         "source_reconciliation" report)))

(defun conscious-lifecycle-scenario-run
    (action &key subject aim result-summary observed-reply
                 (now (get-universal-time)))
  "Run one named, providerless Q5 dev action and return bounded JSON data."
  (%lifecycle-scenario-text action "action" 32)
  (let ((record
          (cond
            ((string= action "create")
             (%lifecycle-scenario-create subject aim now))
            ((string= action "inspect")
             (%lifecycle-scenario-current-record))
            ((member action '("ready" "complete" "cancel") :test #'string=)
             (%lifecycle-scenario-transition
              action result-summary observed-reply now))
            (t (error "Unknown Q5 scenario action ~s" action)))))
    (%lifecycle-scenario-result action record)))
