;;;; Explicit offline rebuild for the recursive hot projection.
;;;; The live agent must be stopped.  This process may be given a larger heap;
;;;; normal startup never substitutes full replay for a missing checkpoint.

(in-package :cl-user)

(require :asdf)

(defparameter *rebuild-root*
  (uiop:ensure-directory-pathname
   (pathname (or (uiop:getenv "PAI_ROOT")
                 (error "PAI_ROOT is required")))))
(defparameter *rebuild-state*
  (uiop:ensure-directory-pathname
   (pathname (or (uiop:getenv "PAI_STATE_ROOT")
                 (error "PAI_STATE_ROOT is required")))))
(defparameter *rebuild-agent-id*
  (or (uiop:getenv "PAI_AGENT_ID")
      (error "PAI_AGENT_ID is required")))
(defparameter *rebuild-persona-id*
  (or (uiop:getenv "PAI_PERSONA_ID")
      (error "PAI_PERSONA_ID is required")))
(defparameter *rebuild-derived-database*
  (pathname
   (or (uiop:getenv "PAI_DERIVED_DATABASE")
       (namestring (merge-pathnames "derived.sqlite3" *rebuild-state*)))))

(setf (uiop:getenv "PAI_STATE_ROOT") (namestring *rebuild-state*))
(uiop:chdir *rebuild-root*)
(let ((*standard-output* (make-broadcast-stream)))
  (load (merge-pathnames "src/kernel/agent.lisp" *rebuild-root*)))
(setf (symbol-value (intern "*HEAP-HEALTH-AUTOSTART-P*" :agent)) nil)
(asdf:load-asd (merge-pathnames "pai.asd" *rebuild-root*))
(asdf:load-system :pai)

(in-package :agent)

(let* ((root (uiop:ensure-directory-pathname
              (pathname (uiop:getenv "PAI_ROOT"))))
       (state (uiop:ensure-directory-pathname
               (pathname (uiop:getenv "PAI_STATE_ROOT"))))
       (agent-id (uiop:getenv "PAI_AGENT_ID"))
       (persona-id (uiop:getenv "PAI_PERSONA_ID"))
       (database (merge-pathnames "events.sqlite3" state))
       (derived-database cl-user::*rebuild-derived-database*)
       (backend (make-sqlite-storage database))
       (checkpoint-backend (make-sqlite-derived-storage derived-database))
       (started (get-internal-real-time)))
  (declare (ignore root))
  (unwind-protect
       (progn
         (multiple-value-bind (report ignored-bundle)
             (conscious-storage-build-checkpoint
              backend :checkpoint-backend checkpoint-backend
              :agent-id agent-id)
           (declare (ignore ignored-bundle))
           (format t
                   "CONSCIOUS-CHECKPOINT-REBUILT events=~d retained=~d head=~d max_event_id=~d~%"
                   (gethash "event_count" report)
                   (gethash "retained_event_count" report)
                   (gethash "through_storage_position" report)
                   (gethash "through_event_id" report)))
         (%sqlite-authority-install
          backend database checkpoint-backend derived-database agent-id)
         (let ((*conscious-recursive-thread-events-maintenance-replay-p* t))
           (let ((events (%recursive-thread-events)))
             (let* ((checkpoint
                      (event-authority-checkpoint-load
                       *conscious-recursive-thread-events-checkpoint-name*))
                    (state (and (hash-table-p checkpoint)
                                (gethash "state" checkpoint))))
               (unless (and (hash-table-p checkpoint)
                            (hash-table-p state)
                            (vectorp (gethash "events" state))
                            (string= (gethash "projector_revision" checkpoint "")
                                     *conscious-recursive-thread-events-projector-revision*)
                            (eql (gethash "through_storage_position" checkpoint)
                                 *conscious-recursive-thread-events-cache-head*)
                            (eql (gethash "through_event_id" checkpoint)
                                 *conscious-recursive-thread-events-cache-max-id*)
                            (string= (gethash "source_binding" state "")
                                     (event-authority-checkpoint-source-binding
                                      *conscious-recursive-thread-events-cache-max-id*
                                      *conscious-recursive-thread-events-cache-head*)))
                 (error "Recursive checkpoint rebuild did not publish a current, source-bound projection")))
             (format t
                     "RECURSIVE-CHECKPOINT-REBUILT events=~d head=~d max_event_id=~d elapsed_ms=~,3f heap_bytes=~d~%"
                     (length events)
                     *conscious-recursive-thread-events-cache-head*
                     *conscious-recursive-thread-events-cache-max-id*
                     (* 1000d0
                        (/ (- (get-internal-real-time) started)
                           internal-time-units-per-second))
                     (sb-kernel:dynamic-usage))))
         (let ((*conscious-context-graph-maintenance-replay-p* t))
           (%ccg-sync backend agent-id persona-id checkpoint-backend)
           (let ((checkpoint
                   (storage-load-checkpoint
                    checkpoint-backend
                    *reviewed-context-graph-projection-name*
                    :agent-id agent-id)))
             (unless checkpoint
               (error "Reviewed context graph rebuild did not publish a projection"))
             (format t
                     "REVIEWED-GRAPH-PROJECTION-REBUILT rows=~d head=~d max_event_id=~d elapsed_ms=~,3f heap_bytes=~d~%"
                     (gethash "record_count" (gethash "state" checkpoint))
                     (gethash "through_storage_position" checkpoint)
                     (gethash "through_event_id" checkpoint)
                     (* 1000d0
                        (/ (- (get-internal-real-time) started)
                           internal-time-units-per-second))
                     (sb-kernel:dynamic-usage)))))
    (when *event-authority-port*
      (ignore-errors (event-authority-clear)))))
