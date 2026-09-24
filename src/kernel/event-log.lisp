;;;; event-log.lisp -- append-only event log, built
;;;; pragmatically against the CURRENT architecture rather than waiting on
;;;; P0.1's full package/CLOS refactor.
;;;;
;;;; 2026-07-27 decision: P0.1 as literally scoped is a big-bang rewrite --
;;;; every file built so far (agent_loop.lisp itself included) references
;;;; the current globals (*last-self-mod-history*, *tools*, *memory-graph*,
;;;; etc.) directly, and replacing them with slots on a CLOS instance means
;;;; touching all of them at once, then a hard restart to cut over -- no
;;;; incremental, zero-downtime path the way everything else here has been
;;;; built. The real risk isn't downtime, it's data: a subtle migration bug
;;;; could corrupt or lose the agent's actual persistent memory/conversation,
;;;; discovered at exactly the worst moment (the live cutover). This gets
;;;; the actual highest-value item -- one source of truth for
;;;; replay/reflection/drift-detection -- without that risk, using the same
;;;; rename-and-wrap idiom as everything else built live this session.
;;;;
;;;; REPLAY-EVENTS takes no AGENT argument (no CLOS instance exists yet,
;;;; consistent with the above) -- reads the immutable legacy file followed
;;;; by deterministic ID-range segments.
;;;;
;;;; Event types actually wired up this pass: user-message, agent-message,
;;;; tool-call, tool-result, self-mod-proposed, self-mod-accepted,
;;;; self-mod-rejected, self-mod-rolled-back, and rebuild-complete projection
;;;; replacements (including durable conversation-history transforms).
;;;; tick-start/tick-end/
;;;; appraisal/prediction/prediction-outcome/memory-archive/memory-write
;;;; are the backlog's full schema for subsystems (Phase 1/2/3/5) that
;;;; don't exist in this codebase yet -- LOG-EVENT is generic (any TYPE
;;;; string is valid, PAYLOAD is any shasht-serializable value), so those
;;;; phases can start calling it directly with their own event types with
;;;; no changes needed here.
;;;;
;;;; Load live (no restart) via lisp-eval or repl-drop:
;;;;   (load "/agent/state/event-log.lisp")

(in-package :agent)

(export '(log-event log-projection-state log-conversation-history-transform
          call-with-tool-event-observation
          log-postgres-row-state replay-events map-events call-with-event-tick-context
          make-event-tick-id event-log-initialize-segmentation
          event-log-ensure-segment-indexes
          write-event-checkpoint read-verified-event-checkpoint
          write-event-row-checkpoint map-verified-event-row-checkpoint
          make-event-jsonl-row-source make-event-jsonl-line-source
          write-event-row-checkpoint-lines
          map-verified-event-row-checkpoint-lines
          event-authority-install event-authority-clear event-authority-report
          event-authority-owns-storage-p
          event-authority-checkpoint-load
          event-authority-checkpoint-publish
          event-authority-checkpoint-source-binding
          event-projection-events event-recent-conversation-events
          event-root-recent-events
          event-episodic-context-events
          event-read-event))

(defparameter *event-log-file* (pai-state-path "events.jsonl"))
(defparameter *event-log-segment-directory*
  (pai-state-path "event-log/segments/"))
(defparameter *event-log-watermark-file*
  (pai-state-path "event-log/watermark.json"))
(defparameter *event-log-legacy-index-file*
  (pai-state-path "event-log/legacy-index.json"))
(defparameter *event-log-segment-index-directory*
  (pai-state-path "event-log/segment-indexes/"))
(defparameter *event-checkpoint-directory*
  (pai-state-path "event-log/checkpoints/"))
(defparameter *event-log-segment-span* 10000)
(defparameter *event-log-legacy-index-stride* 100)
(defparameter *event-log-segment-index-stride* 10)
(defparameter *event-log-segmentation-enabled*
  (not (null
        (member (string-downcase
                 (or (uiop:getenv "PAI_EVENT_LOG_SEGMENTED") "false"))
                '("1" "true" "yes" "on") :test #'string=))))
(defparameter *event-ring-cap* 2000)
(defparameter *event-schema-version* 2)

(defvar *event-ring* nil "List, newest-first, capped at *EVENT-RING-CAP*. A fast-path cache only -- durable event storage is the actual source of truth.")
(defvar *event-next-id* 0)
(defvar *event-authority-port* nil
  "Explicit single event authority. NIL retains the legacy JSONL adapter.")
(defvar *event-log-segmentation-ready-p* nil
  "True only after a valid cutover watermark has been read or initialized.")
(defvar *event-storage-open-observer* nil
  "Optional source-test observer called with OPERATION and PATHNAME.")
(defvar *event-log-lock* (bt:make-lock "event-log"))
(defvar *event-tick-id-lock* (bt:make-lock "event-tick-id"))
(defvar *event-tick-id-sequence* 0)
(defvar *current-event-tick-id* nil
  "Dynamically bound while one autonomous tick executes. Events outside a
tick retain JSON null; ordinary-turn causality remains represented by
CAUSED_BY rather than overloading TICK_ID with a non-tick identifier.")
(defvar *current-causing-event-id* nil
  "Dynamically bound around a turn so nested events (tool-call, tool-result,
self-mod-proposed/accepted/rejected) can record which event caused them,
per the backlog's CAUSED-BY field. One level deep (the triggering
user-message), not a full per-call causal chain -- sufficient for replay,
simple enough to reason about.")
(defvar *public-tool-call-id* nil)
(defvar *public-tool-result-id* nil)
(defvar *public-tool-call-event-id* nil)

(defvar *public-presentation-observer* nil
  "Port: (CHANNEL REPLY USER-EVENT-ID) -> ignored, or NIL when no publication
layer is present.

The event log observes that a public reply was presented; it does not know
what a public-outbound envelope is. Previously this file built one itself and
handed it to the gateway, which made the authority layer link against the
cognitive layer for a fact -- \"a reply went out on the terminal\" -- that it
already had in hand. The publication layer registers the observer during
:INSTALL; with no publication layer the port stays NIL and the log is
unaffected.")

(defun event-authority-install
    (kind &key append append-if-head owns-storage replay map restore read-event
               projection-events recent-conversation root-recent episodic-events
               experience-page activity-read checkpoint-load
               checkpoint-publish checkpoint-source-binding report close)
  "Install exactly one explicit event authority. No write fallback exists
once this port is installed."
  (unless (and (keywordp kind) (or (null append-if-head) (functionp append-if-head))
               (or (null activity-read) (functionp activity-read))
               (or (null root-recent) (functionp root-recent))
               (or (null episodic-events) (functionp episodic-events))
               (or (null owns-storage) (functionp owns-storage))
               (or (and (null checkpoint-load) (null checkpoint-publish)
                        (null checkpoint-source-binding))
                   (every #'functionp
                          (list checkpoint-load checkpoint-publish
                                checkpoint-source-binding)))
               (every #'functionp
                      (list append replay map restore projection-events
                            read-event recent-conversation report close)))
    (error "Event authority port is incomplete"))
  (bt:with-lock-held (*event-log-lock*)
    (when *event-authority-port*
      (error "Event authority is already installed"))
    (setf *event-authority-port*
          (list :kind kind :append append :append-if-head append-if-head
                :owns-storage owns-storage :replay replay :map map
                :read-event read-event
                :restore restore :projection-events projection-events
                :recent-conversation recent-conversation
                :root-recent root-recent
                :episodic-events episodic-events
                :experience-page experience-page
                :activity-read activity-read
                :checkpoint-load checkpoint-load
                :checkpoint-publish checkpoint-publish
                :checkpoint-source-binding checkpoint-source-binding
                :report report :close close)))
  kind)

(defun event-authority-clear ()
  "Close and clear an installed authority for shutdown or isolated tests."
  (let ((port *event-authority-port*))
    (when port (funcall (getf port :close)))
    (bt:with-lock-held (*event-log-lock*)
      (setf *event-authority-port* nil)))
  :jsonl)

(defun event-authority-report ()
  (if *event-authority-port*
      (funcall (getf *event-authority-port* :report))
      (obj "schema_version" 1 "authority" "jsonl"
           "path" (namestring *event-log-file*))))

(defun event-authority-owns-storage-p (backend)
  "True only when the installed authority explicitly claims this backend object."
  (let ((predicate (and *event-authority-port* (getf *event-authority-port* :owns-storage))))
    (and predicate (eq t (funcall predicate backend)))))

(defun event-authority-checkpoint-load (projection-name)
  "Load a derived checkpoint through the installed authority composition."
  (let ((fn (and *event-authority-port*
                 (getf *event-authority-port* :checkpoint-load))))
    (and fn (funcall fn projection-name))))

(defun event-authority-checkpoint-publish
    (projection-name state through-event-id through-position
     projector-revision policy-revision)
  "Publish rebuildable projection state without exposing adapter storage."
  (let ((fn (and *event-authority-port*
                 (getf *event-authority-port* :checkpoint-publish))))
    (and fn (funcall fn projection-name state through-event-id through-position
                     projector-revision policy-revision))))

(defun event-authority-checkpoint-source-binding
    (through-event-id through-position)
  "Bind a projection checkpoint to this authority and exact durable prefix."
  (let ((fn (and *event-authority-port*
                 (getf *event-authority-port* :checkpoint-source-binding))))
    (and fn (funcall fn through-event-id through-position))))

(defun event-projection-events ()
  (if *event-authority-port*
      (funcall (getf *event-authority-port* :projection-events))
      (replay-events)))

(defun event-recent-conversation-events (before-event-id limit)
  (if *event-authority-port*
      (funcall (getf *event-authority-port* :recent-conversation)
               before-event-id limit)
      (remove-if (lambda (event)
                   (and before-event-id
                        (>= (gethash "id" event 0) before-event-id)))
                 (replay-events :limit (if before-event-id (1+ limit) limit)
                                :types '("user-message" "agent-message"
                                         "model-response")))))

(defun event-root-recent-events (root-event-id event-types limit before-event-id)
  "Read bounded causal children before an exact event boundary.
An installed authority must supply an indexed reader, never a whole-ledger
fallback. JSONL-only fixtures retain their replay semantics."
  (unless (and (integerp root-event-id) (plusp root-event-id)
               (integerp before-event-id) (plusp before-event-id)
               (integerp limit) (<= 1 limit 32)
               (listp event-types) (<= 1 (length event-types) 16)
               (every (lambda (type)
                        (and (stringp type) (<= 1 (length type) 256)))
                      event-types))
    (error "Root-recent read requires exact IDs, bounded types and limit"))
  (if *event-authority-port*
      (let ((reader (getf *event-authority-port* :root-recent)))
        (unless (functionp reader)
          (error "Indexed root-recent read is unavailable for this authority"))
        (funcall reader root-event-id event-types limit before-event-id))
      (let ((matches nil))
        (dolist (event (replay-events :types event-types))
          (when (and (equal root-event-id (gethash "caused_by" event))
                     (< (gethash "id" event 0) before-event-id))
            (push event matches)))
        (last (nreverse matches) (min limit (length matches))))))

(defparameter *event-episodic-context-types*
  '("user-message" "agent-message"
    "historical-user-message-imported"
    "historical-agent-message-imported"
    "conversation-episode-sealed"))

(defun event-episodic-context-events (&optional through-event-id)
  "Read the exact episodic projector vocabulary from a bounded authority port.
An installed authority must not satisfy this by restoring a whole-generation
checkpoint. JSONL-only fixtures retain a bounded typed replay."
  (let ((reader (and *event-authority-port*
                     (getf *event-authority-port* :episodic-events))))
    (cond ((functionp reader) (funcall reader through-event-id))
          (*event-authority-port*
           (error "Typed episodic authority read is unavailable"))
          (t
           (let* ((all (replay-events))
                  (prefix (if through-event-id
                              (let ((position
                                      (position through-event-id all
                                                :key (lambda (event)
                                                       (gethash "id" event))
                                                :test #'equal)))
                                (unless position
                                  (error "Episodic boundary event is absent"))
                                (subseq all 0 (1+ position)))
                              all))
                  (events
                    (remove-if-not
                     (lambda (event)
                       (member (gethash "type" event)
                               *event-episodic-context-types*
                               :test #'string=))
                     prefix)))
             (when (> (length events) 16384)
               (error "JSONL episodic input exceeds bounded allowance"))
             events)))))

(defun event-experience-page (from to before-position limit)
  "Read one bounded page from the installed authority; never fall back to replay."
  (let ((reader (getf *event-authority-port* :experience-page)))
    (unless (functionp reader)
      (error "Time-based experience search is unavailable for this authority"))
    (funcall reader from to before-position limit)))

(defun event-read-activity-context (reference-id through-id)
  "Read a bounded original-ledger activity via the installed authority only."
  (let ((reader (getf *event-authority-port* :activity-read)))
    (unless (functionp reader)
      (error "Sustained activity storage is unavailable for this authority"))
    (funcall reader reference-id through-id)))

(defun event-read-event (event-id &key event-type)
  "Read one exact durable event without materializing the ledger."
  (unless (and (integerp event-id) (plusp event-id))
    (error "Event read requires a positive event ID"))
  (if *event-authority-port*
      (funcall (getf *event-authority-port* :read-event)
               event-id event-type)
      (let ((matches
              (remove-if-not
               (lambda (event) (equal event-id (gethash "id" event)))
               (replay-events :types (and event-type (list event-type))))))
        (when (> (length matches) 1)
          (error "Legacy event ID is not unique"))
        (first matches))))

(defvar *turn-capture-user-event-fn* nil
  "Port: (EVENT-ID PROMPT) -> ignored, or NIL when no turn-capture layer is
present. Registered by CONVERSATION-TURN-CAPTURE.LISP at :INSTALL.")
(defvar *turn-capture-completion-fn* nil
  "Port: (REPLY USER-EVENT-ID) -> generalized boolean, true when the
turn-capture layer handled this turn's completion. Registered by
CONVERSATION-TURN-CAPTURE.LISP at :INSTALL.")
(defvar *turn-capture-tool-call-fn* nil
  "Port: (CALL-EVENT-ID TOOL-CALL) -> ignored, or NIL when no turn-capture
layer is present. Registered by CONVERSATION-TURN-CAPTURE.LISP at :INSTALL.")
(defvar *turn-capture-tool-result-fn* nil
  "Port: (RESULT-EVENT-ID TOOL-CALL RESULT) -> ignored, or NIL when no
turn-capture layer is present. Registered by CONVERSATION-TURN-CAPTURE.LISP
at :INSTALL.")

(defun %public-tool-result-id-for-call (tool-call &optional fallback)
  "Derive the public tool-result id for TOOL-CALL.

Pure: reads the call's own id and formats it. Lives here, in the layer that
writes tool-result events, because that is the layer whose event shape defines
the id. AGENT_LOOP.LISP carried a byte-identical copy guarded by
\(UNLESS (FBOUNDP ...)) -- a guard that never fired, since the ASDF serial
order loads that file first."
  (let ((call-id (and (hash-table-p tool-call) (gethash "id" tool-call))))
    (cond ((and (stringp call-id) (plusp (length call-id)))
           (format nil "tool-result:~a" call-id))
          (fallback (format nil "tool-result:event:~a" fallback))
          (t nil))))

(defun %event-now-iso8601 ()
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~a-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ" year month day hour min sec)))

(defun make-event-tick-id (&optional (prefix "tick"))
  "Return a process-unique tick correlation id without writing state."
  (bt:with-lock-held (*event-tick-id-lock*)
    (format nil "~a-~d-~d" prefix (get-universal-time)
            (incf *event-tick-id-sequence*))))

(defun call-with-event-tick-context (tick-id thunk)
  "Call THUNK with TICK-ID attached to every nested LOG-EVENT call."
  (let ((*current-event-tick-id* tick-id))
    (funcall thunk)))

(defun %event-affect-snapshot ()
  "Copy current affect at the writer seam without making logging fallible."
  (or (and (fboundp 'modulator-state)
           (ignore-errors (funcall 'modulator-state)))
      (obj "status" "unavailable")))

;;; --- segmented storage, watermarks, and checkpoints -----------------

(defun %event-storage-observe (operation pathname)
  (when *event-storage-open-observer*
    (ignore-errors (funcall *event-storage-open-observer* operation pathname))))

(defun %event-utf8-octets (string)
  (sb-ext:string-to-octets string :external-format :utf-8))

(defun %event-octets-utf8 (octets)
  (sb-ext:octets-to-string octets :external-format :utf-8))

(defun %event-sha256-octets (octets)
  (unless (find-package :ironclad)
    (ql:quickload :ironclad :silent t))
  (let ((package (find-package :ironclad)))
    (unless package
      (error "IRONCLAD is unavailable; cannot verify event storage"))
    (string-downcase
     (funcall (intern "BYTE-ARRAY-TO-HEX-STRING" package)
              (funcall (intern "DIGEST-SEQUENCE" package) :sha256 octets)))))

(defun %event-make-sha256-digest ()
  (unless (find-package :ironclad)
    (ql:quickload :ironclad :silent t))
  (let ((package (find-package :ironclad)))
    (unless package
      (error "IRONCLAD is unavailable; cannot hash event storage"))
    (funcall (intern "MAKE-DIGEST" package) :sha256)))

(defun %event-update-digest (digest octets &key (start 0) end)
  (let ((package (find-package :ironclad)))
    (funcall (intern "UPDATE-DIGEST" package) digest octets
             :start start :end (or end (length octets)))))

(defun %event-finish-digest-hex (digest)
  (let ((package (find-package :ironclad)))
    (string-downcase
     (funcall (intern "BYTE-ARRAY-TO-HEX-STRING" package)
              (funcall (intern "PRODUCE-DIGEST" package) digest)))))

(defun %event-read-file-octets (pathname)
  (%event-storage-observe :read pathname)
  (with-open-file (in pathname :direction :input
                              :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length in)
                             :element-type '(unsigned-byte 8))))
      (read-sequence bytes in)
      bytes)))

(defun %event-file-byte-length (pathname)
  (if (probe-file pathname)
      (with-open-file (in pathname :direction :input
                                   :element-type '(unsigned-byte 8))
        (file-length in))
      0))

(defun %event-atomic-write-octets (pathname octets)
  (ensure-directories-exist pathname)
  (let ((temporary
          (make-pathname
           :name (format nil "~a-tmp-~d-~d" (pathname-name pathname)
                         (get-universal-time) (random 1000000))
           :type (pathname-type pathname) :defaults pathname)))
    (unwind-protect
        (progn
          (%event-storage-observe :write temporary)
          (with-open-file (out temporary :direction :output
                                          :if-exists :supersede
                                          :if-does-not-exist :create
                                          :element-type '(unsigned-byte 8))
            (write-sequence octets out)
            (finish-output out))
          (uiop:rename-file-overwriting-target temporary pathname)
          pathname)
      (when (probe-file temporary)
        (ignore-errors (delete-file temporary))))))

(defun %event-atomic-write-json (pathname value)
  (let ((*print-pretty* nil))
    (%event-atomic-write-octets
     pathname (%event-utf8-octets (shasht:write-json value nil)))))

(defun %event-read-json-file (pathname)
  (shasht:read-json (%event-octets-utf8 (%event-read-file-octets pathname))))

(defun %event-segment-range (id)
  (let* ((effective-id (max 1 id))
         (zero-based (1- effective-id))
         (first (+ 1 (* (floor zero-based *event-log-segment-span*)
                        *event-log-segment-span*)))
         (last (+ first *event-log-segment-span* -1)))
    (values first last)))

(defun %event-segment-file-name (id)
  (multiple-value-bind (first last) (%event-segment-range id)
    (format nil "events-~12,'0d-~12,'0d.jsonl" first last)))

(defun %event-segment-path (id)
  (merge-pathnames (%event-segment-file-name id)
                   *event-log-segment-directory*))

(defun %event-segment-paths ()
  (sort (copy-list
         (or (ignore-errors
               (directory (merge-pathnames "events-*.jsonl"
                                           *event-log-segment-directory*)))
             nil))
        #'string< :key #'file-namestring))

(defun %event-watermark-canonical (version reserved active first last)
  (format nil "~d|~d|~a|~d|~d" version reserved active first last))

(defun %event-watermark-for-id (id)
  (multiple-value-bind (first last) (%event-segment-range id)
    (let* ((version 1)
           (active (%event-segment-file-name id))
           (checksum
             (%event-sha256-octets
              (%event-utf8-octets
               (%event-watermark-canonical version id active first last)))))
      (obj "schema_version" version
           "last_reserved_id" id
           "active_segment" active
           "segment_first_id" first
           "segment_last_id" last
           "checksum_sha256" checksum))))

(defun %event-valid-watermark-p (watermark)
  (handler-case
      (let* ((version (gethash "schema_version" watermark))
             (reserved (gethash "last_reserved_id" watermark))
             (active (gethash "active_segment" watermark))
             (first (gethash "segment_first_id" watermark))
             (last (gethash "segment_last_id" watermark))
             (checksum (gethash "checksum_sha256" watermark)))
        (and (hash-table-p watermark)
             (eql version 1) (integerp reserved) (not (minusp reserved))
             (stringp active) (integerp first) (integerp last)
             (stringp checksum)
             (multiple-value-bind (expected-first expected-last)
                 (%event-segment-range reserved)
               (and (= first expected-first) (= last expected-last)
                    (string= active (%event-segment-file-name reserved))))
             (string=
              checksum
              (%event-sha256-octets
               (%event-utf8-octets
                (%event-watermark-canonical version reserved active
                                            first last))))))
    (error () nil)))

(defun %event-read-watermark ()
  (handler-case
      (when (probe-file *event-log-watermark-file*)
        (let ((watermark (%event-read-json-file *event-log-watermark-file*)))
          (if (%event-valid-watermark-p watermark)
              watermark
              (progn
                (format t "~&[event-log] invalid segmentation watermark; compatibility mode retained.~%")
                nil))))
    (error (condition)
      (format t "~&[event-log] watermark read failed; compatibility mode retained: ~a~%"
              condition)
      nil)))

(defun %event-write-watermark (id)
  (let ((watermark (%event-watermark-for-id id)))
    (%event-atomic-write-json *event-log-watermark-file* watermark)
    watermark))

(defun %event-append-json-line (pathname event)
  (ensure-directories-exist pathname)
  (%event-storage-observe :append pathname)
  (with-open-file (out pathname :direction :output :if-exists :append
                                :if-does-not-exist :create
                                :external-format :utf-8)
    (let ((start (file-position out))
          (*print-pretty* nil))
      (write-string (shasht:write-json event nil) out)
      (terpri out)
      (finish-output out)
      (values start (file-position out)))))

(defun %event-persist-segmented (event id)
  ;; Reserve before append. A crash may leave a hole, but can never reuse an
  ;; ID or hide a newly selected tail segment from watermark-only recovery.
  (%event-write-watermark id)
  (let ((pathname (%event-segment-path id)))
    (multiple-value-bind (start end) (%event-append-json-line pathname event)
      ;; Segment indexes are rebuildable acceleration data. The event append
      ;; remains successful if an index refresh is interrupted; boot repairs
      ;; the lag before trusting it.
      (handler-case
          (%event-update-segment-index-after-append
           pathname event start end)
        (error (condition)
          (format t "~&[event-log] segment index update deferred: ~a~%"
                  condition)))
      t)))

(defun %event-persist-legacy (event)
  (%event-append-json-line *event-log-file* event))

(defun %jsonl-log-event-locked (type payload caused-by affect-snapshot)
  "Append one JSONL event while the caller holds *EVENT-LOG-LOCK*."
  (let* ((id (incf *event-next-id*))
         (event (obj "schema_version" *event-schema-version*
                     "id" id "timestamp" (%event-now-iso8601) "type" type
                     "agent_id" (if (and (boundp '*agent-id*)
                                          (stringp (symbol-value '*agent-id*))
                                          (plusp (length (symbol-value '*agent-id*))))
                                     (symbol-value '*agent-id*)
                                     :null)
                     "payload" payload "caused_by" (or caused-by :null)
                     "tick_id" (or *current-event-tick-id* :null)
                     "affect_snapshot" affect-snapshot))
         (persisted-p nil))
    (handler-case
        (progn
          (cond
            ((not *event-log-segmentation-enabled*)
             (%event-persist-legacy event))
            (*event-log-segmentation-ready-p*
             (%event-persist-segmented event id))
            (t
             (error "segmented event storage requested without a valid watermark")))
          (setf persisted-p t))
      (error (e) (format t "~&[event-log] write failed: ~a~%" e)))
    (push event *event-ring*)
    (when (> (length *event-ring*) *event-ring-cap*)
      (setf *event-ring* (subseq *event-ring* 0 *event-ring-cap*)))
    (values id persisted-p (and persisted-p event))))

(defun %jsonl-log-event (type payload &key caused-by)
  "Append one event to the configured durable storage and in-memory ring. TYPE is any
string (schema is open -- see file header). PAYLOAD is anything
SHASHT:WRITE-JSON can serialize (usually an OBJ). CAUSED-BY is the parent
event's id, or NIL. Returns three values: the new event id, whether its durable
append completed, and the exact stamped event as a durable receipt (or NIL).
Existing one-value callers continue to receive the id. Never signals -- a logging
failure must never break the thing being logged -- and flushes after every
write, so a crash mid-session loses at most the event being written right
then, never anything already appended."
  (let ((event nil) (id nil) (persisted-p nil)
        ;; Capture before taking the event lock. Future modulator write events
        ;; may be emitted while their own subsystem lock is held; keeping the
        ;; snapshot outside this lock avoids creating a lock-order cycle.
        (affect-snapshot (%event-affect-snapshot)))
    (bt:with-lock-held (*event-log-lock*)
      (multiple-value-setq (id persisted-p event)
        (%jsonl-log-event-locked type payload caused-by affect-snapshot)))
    ;; Emit only after the append lock is released. Observer failures may log
    ;; their own content-free error event; emitting under *EVENT-LOG-LOCK*
    ;; would deadlock that fail-isolated path.
    (when (and persisted-p (fboundp 'runtime-observer-emit))
      (ignore-errors (runtime-observer-emit type event)))
    (values id persisted-p (and persisted-p event))))

(defun %authority-log-event-locked (type payload caused-by affect-snapshot &optional expected-head)
  "Append through the installed authority while *EVENT-LOG-LOCK* is held."
  (let ((event nil) (id nil) (persisted-p nil))
    (handler-case
        (multiple-value-setq (id persisted-p event)
          (if expected-head
              (funcall (or (getf *event-authority-port* :append-if-head)
                           (error "Authority does not support conditional durable append"))
                       expected-head type payload caused-by *current-event-tick-id* affect-snapshot)
              (funcall (getf *event-authority-port* :append)
                       type payload caused-by *current-event-tick-id* affect-snapshot)))
      (error (condition)
        (format t "~&[event-log] authority write failed: ~a~%" condition)))
    (when persisted-p
      (setf *event-next-id* (max *event-next-id* id))
      (push event *event-ring*)
      (when (> (length *event-ring*) *event-ring-cap*)
        (setf *event-ring* (subseq *event-ring* 0 *event-ring-cap*))))
    (values id persisted-p (and persisted-p event))))

(defun log-event (type payload &key caused-by expected-head)
  "Append through the single installed authority, or legacy JSONL when no
authority port has been selected. Returns ID, durable-p and exact receipt.
EXPECTED-HEAD requests atomic global-head admission, preserving normal publication.
Unsupported authority never falls back to an unconditional append."
  (when expected-head
    (unless (and (integerp expected-head) (<= 0 expected-head) *event-authority-port*
                 (getf *event-authority-port* :append-if-head))
      (error "Conditional event append requires a supported authority and head")))
  (if (null *event-authority-port*)
      (%jsonl-log-event type payload :caused-by caused-by)
      (let ((event nil) (id nil) (persisted-p nil)
            ;; Preserve the established lock order: affect providers may own
            ;; subsystem locks and must never be entered under the append lock.
            (affect-snapshot (%event-affect-snapshot)))
        (bt:with-lock-held (*event-log-lock*)
          (multiple-value-setq (id persisted-p event)
            (%authority-log-event-locked
             type payload caused-by affect-snapshot expected-head)))
        (when (and persisted-p (fboundp 'runtime-observer-emit))
          (ignore-errors (runtime-observer-emit type event)))
        (values id persisted-p (and persisted-p event)))))

(defun log-event-if (predicate type payload &key caused-by)
  "Conditionally append one event under the same lock as every event append.
PREDICATE is evaluated while append authority is quiescent. A false result
writes nothing and returns four NIL values; success returns LOG-EVENT's three
values plus T."
  (unless (functionp predicate)
    (error "Conditional event append requires a predicate"))
  (let ((event nil) (id nil) (persisted-p nil) (accepted-p nil)
        (affect-snapshot (%event-affect-snapshot)))
    (bt:with-lock-held (*event-log-lock*)
      (when (funcall predicate)
        (setf accepted-p t)
        (multiple-value-setq (id persisted-p event)
          (if (null *event-authority-port*)
              (%jsonl-log-event-locked type payload caused-by affect-snapshot)
              (%authority-log-event-locked
               type payload caused-by affect-snapshot)))))
    (when (and persisted-p (fboundp 'runtime-observer-emit))
      (ignore-errors (runtime-observer-emit type event)))
    (values id persisted-p (and persisted-p event) accepted-p)))

(defun log-projection-state (projection pathname content)
  "Log one rebuild-complete projection replacement after its atomic rename.
CONTENT is the exact UTF-8 file string, including any trailing newline."
  (log-event
   "projection-state"
   (obj "projection" projection
        "operation" "replace"
        "file" (file-namestring pathname)
        "encoding" "utf-8"
        "content" content)))

(defun log-conversation-history-transform (pathname content)
  "Log one rebuild-complete durable conversation replacement after rename.
CONTENT is the exact complete UTF-8 JSON string written to PATHNAME."
  (log-event
   "conversation-history-transform"
   (obj "projection" "conversation-history"
        "operation" "replace"
        "file" (file-namestring pathname)
        "encoding" "utf-8"
        "content" content)))

(defun log-postgres-row-state (table operation primary-key row
                               &optional row-json)
  "Log one rebuild-complete PostgreSQL row mutation after its transaction.
PRIMARY-KEY and ROW are JSON objects; ROW is the exact post-mutation image
returned by PostgreSQL rather than a Lisp-side re-derivation. When supplied,
ROW-JSON is the opaque row_to_json(... )::text source string retained for
spelling-exact tail replay."
  (when (and row-json (not (stringp row-json)))
    (error "PostgreSQL exact row JSON must be a string"))
  (log-event
   "postgres-row-state"
   (let ((payload
           (obj "projection" "postgres"
                "table" table
                "operation" operation
                "primary_key" primary-key
                "row" row)))
     (when row-json
       (setf (gethash "row_json" payload) row-json))
     payload)))

(defun %event-parse-ts-string (s)
  "Parse an ISO8601 UTC timestamp without first materializing its JSON row."
  (handler-case
      (if (and (stringp s) (>= (length s) 19))
          (encode-universal-time
           (parse-integer s :start 17 :end 19) (parse-integer s :start 14 :end 16)
           (parse-integer s :start 11 :end 13) (parse-integer s :start 8 :end 10)
           (parse-integer s :start 5 :end 7) (parse-integer s :start 0 :end 4) 0)
          0)
    (error () 0)))

(defun %event-parse-ts (event)
  "Parses an event's ISO8601 TIMESTAMP back into universal-time, for
FROM/TO filtering in REPLAY-EVENTS. Returns 0 (sorts first) if malformed."
  (%event-parse-ts-string (gethash "timestamp" event)))

(defun %event-line-ts (line)
  "Read only the timestamp field from a compact JSONL row.  This permits old
out-of-window rows to be rejected before allocating their full object graph."
  (let* ((key (search "\"timestamp\"" line))
         (colon (and key (position #\: line :start (+ key 11))))
         (start-quote (and colon (position #\" line :start (1+ colon))))
         (end-quote (and start-quote (position #\" line :start (1+ start-quote)))))
    (if end-quote
        (%event-parse-ts-string (subseq line (1+ start-quote) end-quote))
        0)))

(defun %event-line-type (line)
  "Extract the compact JSONL type field without materializing its payload."
  (let* ((key (search "\"type\"" line))
         (colon (and key (position #\: line :start (+ key 6))))
         (start-quote (and colon (position #\" line :start (1+ colon))))
         (end-quote (and start-quote
                         (position #\" line :start (1+ start-quote)))))
    (and end-quote (subseq line (1+ start-quote) end-quote))))

(defun %event-line-id (line)
  "Extract the integer ID from the compact envelope without parsing payload."
  (let* ((key (search "\"id\"" line))
         (colon (and key (position #\: line :start (+ key 4))))
         (start (and colon
                     (position-if-not (lambda (ch) (member ch '(#\Space #\Tab)))
                                      line :start (1+ colon))))
         (end (and start (position-if-not #'digit-char-p line :start start))))
    (and start (> (or end (length line)) start)
         (ignore-errors (parse-integer line :start start
                                            :end (or end (length line)))))))

;;; --- segment time bounds and sparse byte seeking --------------------

(defun %event-segment-index-path (segment-path)
  (merge-pathnames
   (format nil "~a.index.json" (file-namestring segment-path))
   *event-log-segment-index-directory*))

(defun %event-segment-index-canonical
    (version source-file source-byte-length source-row-count source-last-id
     minimum-time maximum-time stride entries)
  (with-output-to-string (out)
    (format out "~d|~a|~d|~d|~d|~d|~d|~d|"
            version source-file source-byte-length source-row-count
            source-last-id minimum-time maximum-time stride)
    (map nil
         (lambda (entry)
           (format out "~d,~d,~d;"
                   (gethash "offset" entry)
                   (gethash "max_timestamp_before_offset" entry)
                   (gethash "id" entry)))
         entries)))

(defun %event-make-segment-index
    (pathname source-byte-length source-row-count source-last-id minimum-time
     maximum-time entries)
  (let* ((version 1)
         (source-file (file-namestring pathname))
         (ordered (coerce entries 'vector))
         (checksum
           (%event-sha256-octets
            (%event-utf8-octets
             (%event-segment-index-canonical
              version source-file source-byte-length source-row-count
              source-last-id minimum-time maximum-time
              *event-log-segment-index-stride* ordered)))))
    (obj "schema_version" version
         "source_file" source-file
         "source_byte_length" source-byte-length
         "source_row_count" source-row-count
         "source_last_id" source-last-id
         "minimum_timestamp_universal" minimum-time
         "maximum_timestamp_universal" maximum-time
         "stride" *event-log-segment-index-stride*
         "entries" ordered
         "checksum_sha256" checksum)))

(defun %event-build-segment-index (pathname)
  "Scan one segment without materializing event payloads. Seek points carry
the maximum timestamp before their byte offset, so they remain safe even if
the wall clock moves backward."
  (let ((entries nil) (row 0) (maximum-before 0)
        (minimum-time nil) (maximum-time 0) (maximum-id 0))
    (when (probe-file pathname)
      (%event-storage-observe :index-scan pathname)
      (with-open-file (in pathname :direction :input :external-format :utf-8)
        (loop for offset = (file-position in)
              for line = (read-line in nil nil)
              while line
              do (let ((timestamp (%event-line-ts line))
                       (id (%event-line-id line)))
                   (unless (and (plusp timestamp) (integerp id) (plusp id)
                                (plusp (length line))
                                (char= #\} (char line (1- (length line)))))
                     (error "Malformed event row in segment ~a at byte ~d"
                            pathname offset))
                   (when (zerop (mod row *event-log-segment-index-stride*))
                     (push (obj "offset" offset
                                "max_timestamp_before_offset" maximum-before
                                "id" id)
                           entries))
                   (setf minimum-time (if minimum-time
                                          (min minimum-time timestamp)
                                          timestamp)
                         maximum-time (max maximum-time timestamp)
                         maximum-before (max maximum-before timestamp)
                         maximum-id (max maximum-id id))
                   (incf row)))))
    (%event-make-segment-index
     pathname (%event-file-byte-length pathname) row maximum-id
     (or minimum-time 0) maximum-time (nreverse entries))))

(defun %event-valid-segment-index-p (index pathname)
  (handler-case
      (let* ((version (gethash "schema_version" index))
             (source-file (gethash "source_file" index))
             (source-byte-length (gethash "source_byte_length" index))
             (source-row-count (gethash "source_row_count" index))
             (source-last-id (gethash "source_last_id" index))
             (minimum-time (gethash "minimum_timestamp_universal" index))
             (maximum-time (gethash "maximum_timestamp_universal" index))
             (stride (gethash "stride" index))
             (entries (gethash "entries" index))
             (checksum (gethash "checksum_sha256" index))
             (previous-offset -1) (previous-before -1) (previous-id -1))
        (and (eql version 1)
             (stringp source-file)
             (string= source-file (file-namestring pathname))
             (integerp source-byte-length)
             (<= 0 source-byte-length (%event-file-byte-length pathname))
             (integerp source-row-count) (not (minusp source-row-count))
             (integerp source-last-id) (not (minusp source-last-id))
             (integerp minimum-time) (integerp maximum-time)
             (<= 0 minimum-time maximum-time)
             (eql stride *event-log-segment-index-stride*)
             (vectorp entries)
             (every
              (lambda (entry)
                (let ((offset (gethash "offset" entry))
                      (before (gethash "max_timestamp_before_offset" entry))
                      (id (gethash "id" entry)))
                  (prog1
                      (and (integerp offset) (> offset previous-offset)
                           (< offset (max 1 source-byte-length))
                           (integerp before) (>= before previous-before)
                           (<= before maximum-time)
                           (integerp id) (> id previous-id)
                           (<= id source-last-id))
                    (when (integerp offset) (setf previous-offset offset))
                    (when (integerp before) (setf previous-before before))
                    (when (integerp id) (setf previous-id id)))))
              entries)
             (stringp checksum)
             (string=
              checksum
              (%event-sha256-octets
               (%event-utf8-octets
                (%event-segment-index-canonical
                 version source-file source-byte-length source-row-count
                 source-last-id minimum-time maximum-time stride entries))))))
    (error () nil)))

(defun %event-read-segment-index (pathname)
  (handler-case
      (let ((index-path (%event-segment-index-path pathname)))
        (when (probe-file index-path)
          (let ((index (%event-read-json-file index-path)))
            (and (%event-valid-segment-index-p index pathname) index))))
    (error () nil)))

(defun %event-write-built-segment-index (pathname)
  (let ((index (%event-build-segment-index pathname)))
    (%event-atomic-write-json (%event-segment-index-path pathname) index)
    index))

(defun %event-update-segment-index-after-append (pathname event start end)
  (let* ((index (%event-read-segment-index pathname))
         (timestamp (%event-parse-ts event))
         (id (gethash "id" event)))
    (cond
      ;; A new segment can be indexed without a rescan.
      ((and (zerop start) (null index))
       (%event-atomic-write-json
        (%event-segment-index-path pathname)
        (%event-make-segment-index
         pathname end 1 id timestamp timestamp
         (list (obj "offset" 0 "max_timestamp_before_offset" 0 "id" id)))))
      ;; Only extend an exact prefix. Any crash-left lag is repaired at boot.
      ((and index (= start (gethash "source_byte_length" index)))
       (let* ((row (gethash "source_row_count" index))
              (entries (coerce (gethash "entries" index) 'list))
              (maximum-before
                (gethash "maximum_timestamp_universal" index)))
         (when (zerop (mod row *event-log-segment-index-stride*))
           (setf entries
                 (append entries
                         (list (obj "offset" start
                                    "max_timestamp_before_offset"
                                    maximum-before
                                    "id" id)))))
         (%event-atomic-write-json
          (%event-segment-index-path pathname)
          (%event-make-segment-index
           pathname end (1+ row) (max id (gethash "source_last_id" index))
           (if (zerop row) timestamp
               (min timestamp
                    (gethash "minimum_timestamp_universal" index)))
           (max timestamp maximum-before) entries))))))
  t)

(defun %event-segment-scan-plan (pathname from to)
  "Return SCAN-P and a safe byte start. A stale or invalid derived index can
only increase work; it can never suppress rows."
  (let ((index (%event-read-segment-index pathname)))
    (if (null index)
        (values t 0)
        (let* ((indexed-length (gethash "source_byte_length" index))
               (complete-p (= indexed-length (%event-file-byte-length pathname)))
               (minimum-time (gethash "minimum_timestamp_universal" index))
               (maximum-time (gethash "maximum_timestamp_universal" index)))
          (if (and complete-p
                   (or (and to (< to minimum-time))
                       (and from (> from maximum-time))))
              (values nil 0)
              (values
               t
               (if (null from)
                   0
                   (loop with selected = 0
                         for entry across (gethash "entries" index)
                         while (< (gethash "max_timestamp_before_offset" entry)
                                  from)
                         do (setf selected (gethash "offset" entry))
                         finally (return selected)))))))))

(defun %event-index-id-start-offset (index after-id)
  (if (or (null index) (null after-id))
      0
      (loop with selected = 0
            for entry across (gethash "entries" index)
            while (<= (gethash "id" entry) after-id)
            do (setf selected (gethash "offset" entry))
            finally (return selected))))

(defun %event-segment-id-scan-plan (pathname after-id through-id)
  "Return SCAN-P and a safe ID-based byte start. Only an exact complete index
may suppress a segment; invalid/partial indexes fall back to more work."
  (let ((index (%event-read-segment-index pathname)))
    (if (null index)
        (values t 0)
        (let* ((complete-p
                 (= (gethash "source_byte_length" index)
                    (%event-file-byte-length pathname)))
               (row-count (gethash "source_row_count" index))
               (entries (gethash "entries" index))
               (first-id (and (plusp row-count) (plusp (length entries))
                              (gethash "id" (aref entries 0))))
               (last-id (gethash "source_last_id" index)))
          (if (and complete-p
                   (or (zerop row-count)
                       (and after-id (<= last-id after-id))
                       (and through-id first-id (> first-id through-id))))
              (values nil 0)
              (values t (%event-index-id-start-offset index after-id)))))))

(defun event-log-ensure-segment-indexes ()
  "Build or refresh exact sidecar indexes for every segment. This quiesces
event writes with the event lock and returns the number rebuilt."
  (bt:with-lock-held (*event-log-lock*)
    (loop for pathname in (%event-segment-paths)
          for index = (%event-read-segment-index pathname)
          unless (and index
                      (= (gethash "source_byte_length" index)
                         (%event-file-byte-length pathname)))
            do (%event-write-built-segment-index pathname)
            and count pathname)))

(defun %event-legacy-index-canonical
    (version source-file source-byte-length source-last-id entries)
  (with-output-to-string (out)
    (format out "~d|~a|~d|~d|" version source-file source-byte-length
            source-last-id)
    (map nil
         (lambda (entry)
           (format out "~d,~d,~d;"
                   (gethash "offset" entry)
                   (gethash "timestamp_universal" entry)
                   (gethash "id" entry)))
         entries)))

(defun %event-build-legacy-index (source-last-id)
  "Build a sparse byte-offset index without retaining legacy event objects."
  (let ((entries nil) (row 0) (last-index-ts 0))
    (when (probe-file *event-log-file*)
      (with-open-file (in *event-log-file* :direction :input
                                            :external-format :utf-8)
        (loop for offset = (file-position in)
              for line = (read-line in nil nil)
              while line
              do (when (zerop (mod row *event-log-legacy-index-stride*))
                   (let* ((timestamp (%event-line-ts line))
                          (event (shasht:read-json line))
                          (id (gethash "id" event)))
                     ;; A backward wall-clock adjustment must never produce an
                     ;; index entry that could seek past an in-window event.
                     (when (and (integerp id) (>= timestamp last-index-ts))
                       (push (obj "offset" offset
                                  "timestamp_universal" timestamp
                                  "id" id)
                             entries)
                       (setf last-index-ts timestamp))))
                 (incf row))))
    (let* ((ordered (coerce (nreverse entries) 'vector))
           (version 1)
           (source-file (file-namestring *event-log-file*))
           (source-byte-length (%event-file-byte-length *event-log-file*))
           (checksum
             (%event-sha256-octets
              (%event-utf8-octets
               (%event-legacy-index-canonical
                version source-file source-byte-length source-last-id
                ordered)))))
      (obj "schema_version" version
           "source_file" source-file
           "source_byte_length" source-byte-length
           "source_last_id" source-last-id
           "stride" *event-log-legacy-index-stride*
           "entries" ordered
           "checksum_sha256" checksum))))

(defun %event-valid-legacy-index-p (index)
  (handler-case
      (let* ((version (gethash "schema_version" index))
             (source-file (gethash "source_file" index))
             (source-byte-length (gethash "source_byte_length" index))
             (source-last-id (gethash "source_last_id" index))
             (entries (gethash "entries" index))
             (checksum (gethash "checksum_sha256" index))
             (previous-offset -1)
             (previous-time 0)
             (previous-id -1))
        (and (eql version 1)
             (stringp source-file)
             (string= source-file (file-namestring *event-log-file*))
             (integerp source-byte-length)
             (= source-byte-length
                (%event-file-byte-length *event-log-file*))
             (integerp source-last-id) (not (minusp source-last-id))
             (vectorp entries)
             (every
              (lambda (entry)
                (let ((offset (gethash "offset" entry))
                      (timestamp (gethash "timestamp_universal" entry))
                      (id (gethash "id" entry)))
                  (prog1
                      (and (integerp offset) (> offset previous-offset)
                           (< offset (max 1 source-byte-length))
                           (integerp timestamp) (>= timestamp previous-time)
                           (integerp id) (> id previous-id)
                           (<= id source-last-id))
                    (when (integerp offset) (setf previous-offset offset))
                    (when (integerp timestamp) (setf previous-time timestamp))
                    (when (integerp id) (setf previous-id id)))))
              entries)
             (stringp checksum)
             (string=
              checksum
              (%event-sha256-octets
               (%event-utf8-octets
                (%event-legacy-index-canonical
                 version source-file source-byte-length source-last-id
                 entries))))))
    (error () nil)))

(defun %event-read-legacy-index ()
  (handler-case
      (when (probe-file *event-log-legacy-index-file*)
        (let ((index (%event-read-json-file *event-log-legacy-index-file*)))
          (and (%event-valid-legacy-index-p index) index)))
    (error () nil)))

(defun %event-legacy-start-offset (from)
  (let ((index (and from (%event-read-legacy-index))))
    (if (null index)
        0
        (loop with selected = 0
              for entry across (gethash "entries" index)
              while (<= (gethash "timestamp_universal" entry) from)
              do (setf selected (gethash "offset" entry))
              finally (return selected)))))

(defun %event-legacy-id-scan-plan (after-id through-id)
  "Return SCAN-P and a safe ID start for the immutable legacy ledger."
  (let ((index (%event-read-legacy-index)))
    (if (null index)
        (values t 0)
        (let* ((entries (gethash "entries" index))
               (first-id (and (plusp (length entries))
                              (gethash "id" (aref entries 0))))
               (last-id (gethash "source_last_id" index)))
          (if (or (and after-id (<= last-id after-id))
                  (and through-id first-id (> first-id through-id)))
              (values nil 0)
              (values t (%event-index-id-start-offset index after-id)))))))

(defun %event-storage-paths ()
  (append (when (probe-file *event-log-file*) (list *event-log-file*))
          (%event-segment-paths)))

(defun %event-scan-file
    (pathname visitor &key from to after-id through-id types exclude-types
                            id-observer (start-position 0))
  (%event-storage-observe :scan pathname)
  (with-open-file (in pathname :external-format :utf-8)
    (when (plusp start-position)
      (%event-storage-observe :seek pathname)
      (unless (file-position in start-position)
        (error "Cannot seek event storage ~a to byte ~d"
               pathname start-position)))
    (loop for line = (read-line in nil nil)
          while line
          when (plusp (length (string-trim '(#\Space #\Return) line)))
            do (let ((ts (%event-line-ts line))
                     (id (and (or after-id through-id id-observer)
                              (%event-line-id line)))
                     (type (and (or types exclude-types)
                                (%event-line-type line))))
                 (when (and (or (null after-id)
                                (and (integerp id) (> id after-id)))
                            (or (null through-id)
                                (and (integerp id) (<= id through-id))))
                   (when id-observer (funcall id-observer id))
                   (when (and (or (null from) (>= ts from))
                              (or (null to) (<= ts to))
                              (or (null types)
                                  (member type types :test #'string=))
                              (or (null exclude-types)
                                  (not (member type exclude-types
                                               :test #'string=))))
                     (funcall visitor (shasht:read-json line)))))))
  t)

(defun %event-scan-disk
    (visitor &key from to after-id through-id types exclude-types id-observer)
  "Visit legacy and range segments in chronological storage order.
Returns true only on a complete scan; corrupt input never yields a partial
success result."
  (handler-case
      (progn
        (dolist (pathname (%event-storage-paths))
          (if (equal (namestring pathname) (namestring *event-log-file*))
              (multiple-value-bind (id-scan-p id-start)
                  (%event-legacy-id-scan-plan after-id through-id)
                (if id-scan-p
                    (%event-scan-file
                     pathname visitor :from from :to to
                     :after-id after-id :through-id through-id :types types
                     :exclude-types exclude-types :id-observer id-observer
                     :start-position (max id-start
                                          (%event-legacy-start-offset from)))
                    (%event-storage-observe :skip pathname)))
              (multiple-value-bind (time-scan-p time-start)
                  (%event-segment-scan-plan pathname from to)
                (multiple-value-bind (id-scan-p id-start)
                    (%event-segment-id-scan-plan
                     pathname after-id through-id)
                  (if (and time-scan-p id-scan-p)
                      (%event-scan-file
                       pathname visitor :from from :to to
                       :after-id after-id :through-id through-id :types types
                       :exclude-types exclude-types :id-observer id-observer
                       :start-position (max time-start id-start))
                      (%event-storage-observe :skip pathname))))))
        t)
    (error (e)
      (format t "~&[event-log] scan failed, treating as empty: ~a~%" e)
      nil)))

(defun %jsonl-map-events
    (visitor &key after-id through-id from to types exclude-types)
  "Incrementally visit durable events without materializing a result list.
Returns COMPLETE-P, LAST-PERSISTED-ID observed inside the ID window regardless
of type filtering, and VISITED-COUNT delivered to VISITOR."
  (unless (functionp visitor) (error "MAP-EVENTS visitor must be a function"))
  (dolist (bound (list after-id through-id))
    (unless (or (null bound) (and (integerp bound) (not (minusp bound))))
      (error "MAP-EVENTS ID bounds must be non-negative integers or NIL")))
  (when (and after-id through-id (> after-id through-id))
    (error "MAP-EVENTS AFTER-ID cannot exceed THROUGH-ID"))
  (let ((last-id nil) (visited 0))
    (values
     (%event-scan-disk
      (lambda (event) (incf visited) (funcall visitor event))
      :from from :to to :after-id after-id :through-id through-id
      :types types :exclude-types exclude-types
      :id-observer (lambda (id)
                     (when (and (integerp id)
                                (or (null last-id) (> id last-id)))
                       (setf last-id id))))
     last-id visited)))

(defun map-events (visitor &rest arguments &key &allow-other-keys)
  (if *event-authority-port*
      (apply (getf *event-authority-port* :map) visitor arguments)
      (apply #'%jsonl-map-events visitor arguments)))

(defun %event-read-all-from-disk ()
  "Read all durable events oldest first. Corrupt or missing storage is
treated as empty, matching conversation-persistence.lisp's fail-soft policy."
  (let ((events nil))
    (when (%event-scan-disk (lambda (event) (push event events)))
      (nreverse events))))

(defun %event-max-id-in-file (pathname)
  (let ((max-id 0))
    (handler-case
        (progn
          (when (probe-file pathname)
            (%event-scan-file
             pathname
             (lambda (event)
               (let ((id (and (hash-table-p event) (gethash "id" event))))
                 (when (and (integerp id) (> id max-id))
                   (setf max-id id))))))
          (values max-id t))
      (error (condition)
        (format t "~&[event-log] tail scan failed; reserved watermark retained: ~a~%"
                condition)
        (values 0 nil)))))

(defun %jsonl-event-restore-next-id ()
  "Restore from watermark + named tail when segmented storage is ready.
Compatibility mode retains the historical full storage scan."
  ;; The environment flag requests the first cutover, but the valid watermark
  ;; is the durable one-way activation record. Once published, every later
  ;; boot must continue segmented writes even if an older retained container
  ;; definition does not carry the flag; falling back to legacy append would
  ;; place newer IDs before older segment rows in replay order.
  (let ((watermark (%event-read-watermark)))
    (if watermark
        (let* ((reserved (gethash "last_reserved_id" watermark))
               (active (gethash "active_segment" watermark))
               (tail-path (merge-pathnames active
                                           *event-log-segment-directory*)))
          (setf *event-log-segmentation-enabled* t
                *event-log-segmentation-ready-p* t)
          (let ((index (%event-read-segment-index tail-path)))
            (handler-case
                (progn
                  ;; Exact indexes make normal boot independent of segment
                  ;; size. Missing or crash-left lag is repaired with one
                  ;; payload-free scan before the index is trusted.
                  (unless (and index
                               (= (gethash "source_byte_length" index)
                                  (%event-file-byte-length tail-path)))
                    (setf index (%event-write-built-segment-index tail-path)))
                  (setf *event-next-id*
                        (max reserved (gethash "source_last_id" index))))
              (error (condition)
                (format t "~&[event-log] tail index repair failed; reserved watermark retained: ~a~%"
                        condition)
                (setf *event-next-id* reserved)))))
        (let ((max-id 0))
          (setf *event-log-segmentation-ready-p* nil)
          (unless (%event-scan-disk
                   (lambda (event)
                     (let ((id (and (hash-table-p event)
                                    (gethash "id" event))))
                       (when (and (integerp id) (> id max-id))
                         (setf max-id id)))))
            (setf max-id 0))
          (setf *event-next-id* max-id)))
    *event-next-id*))

(defun %event-restore-next-id ()
  (if *event-authority-port*
      (setf *event-next-id*
            (funcall (getf *event-authority-port* :restore)))
      (%jsonl-event-restore-next-id)))

;; Restore the global append sequence before any install/restore phase can log.
;; This used to exist only as a callable helper, so every fresh host process
;; started again at one and appended duplicate IDs to otherwise durable data.
(define-init :configure event-log-restore-next-id
    "Restore the append-only event ID sequence before initialization writes."
  (%event-restore-next-id))

(defun event-log-initialize-segmentation ()
  "Create a cutover watermark without copying or rewriting any event."
  (bt:with-lock-held (*event-log-lock*)
    (let ((max-id 0))
      (unless (%event-scan-disk
               (lambda (event)
                 (let ((id (and (hash-table-p event) (gethash "id" event))))
                   (when (and (integerp id) (> id max-id))
                     (setf max-id id)))))
        (error "Cannot initialize segmentation from corrupt event storage"))
      (multiple-value-bind (legacy-max complete-p)
          (%event-max-id-in-file *event-log-file*)
        (unless complete-p
          (error "Cannot index corrupt legacy event storage"))
        (%event-atomic-write-json
         *event-log-legacy-index-file*
         (%event-build-legacy-index legacy-max)))
      (let ((watermark (%event-write-watermark max-id)))
        (setf *event-next-id* max-id
              *event-log-segmentation-ready-p* t)
        watermark))))

(defun %event-checkpoint-safe-name-p (name)
  (and (stringp name) (plusp (length name))
       (every (lambda (character)
                (or (alphanumericp character)
                    (char= character #\_)
                    (char= character #\-)))
              name)))

(defun %event-checkpoint-data-name (projection event-id)
  (format nil "checkpoint-~a-~12,'0d.data" projection event-id))

(defun %event-checkpoint-manifest-name (projection event-id)
  (format nil "checkpoint-~a-~12,'0d.json" projection event-id))

(defun %event-checkpoint-path (filename)
  (merge-pathnames filename *event-checkpoint-directory*))

(defun write-event-checkpoint (projection event-id content)
  "Write exact checkpoint bytes, then atomically publish their hash manifest."
  (unless (%event-checkpoint-safe-name-p projection)
    (error "Unsafe checkpoint projection name: ~s" projection))
  (unless (and (integerp event-id) (not (minusp event-id)))
    (error "Checkpoint event id must be a non-negative integer"))
  (unless (stringp content)
    (error "Checkpoint content must be a string"))
  (let* ((bytes (%event-utf8-octets content))
         (data-name (%event-checkpoint-data-name projection event-id))
         (data-path (%event-checkpoint-path data-name))
         (manifest-path
           (%event-checkpoint-path
            (%event-checkpoint-manifest-name projection event-id)))
         (manifest
           (obj "schema_version" 1
                "projection" projection
                "event_id" event-id
                "file" data-name
                "encoding" "utf-8"
                "byte_length" (length bytes)
                "sha256" (%event-sha256-octets bytes)
                "created_at" (%event-now-iso8601))))
    (%event-atomic-write-octets data-path bytes)
    (%event-atomic-write-json manifest-path manifest)
    manifest))

(defun %event-checkpoint-manifest-paths (projection)
  (sort
   (copy-list
    (or (ignore-errors
          (directory
           (merge-pathnames (format nil "checkpoint-~a-*.json" projection)
                            *event-checkpoint-directory*)))
        nil))
   #'string> :key #'file-namestring))

(defun %event-verify-checkpoint-manifest (projection manifest manifest-path)
  (handler-case
      (let* ((version (gethash "schema_version" manifest))
             (manifest-projection (gethash "projection" manifest))
             (event-id (gethash "event_id" manifest))
             (filename (gethash "file" manifest))
             (byte-length (gethash "byte_length" manifest))
             (sha256 (gethash "sha256" manifest))
             (expected-name
               (and (integerp event-id)
                    (%event-checkpoint-data-name projection event-id)))
             (expected-manifest-name
               (and (integerp event-id)
                    (%event-checkpoint-manifest-name projection event-id))))
        (when (and (eql version 1)
                   (stringp manifest-projection)
                   (string= manifest-projection projection)
                   (integerp event-id) (not (minusp event-id))
                   (string= (file-namestring manifest-path)
                            expected-manifest-name)
                   (stringp filename) (string= filename expected-name)
                   (integerp byte-length) (not (minusp byte-length))
                   (stringp sha256))
          (let* ((bytes
                   (%event-read-file-octets
                    (%event-checkpoint-path filename)))
                 (actual-sha (%event-sha256-octets bytes)))
            (when (and (= byte-length (length bytes))
                       (string= sha256 actual-sha))
              (%event-octets-utf8 bytes)))))
    (error () nil)))

(defun read-verified-event-checkpoint (projection)
  "Return the newest hash-valid checkpoint content and manifest.
Corrupt newer artifacts are skipped in favor of the latest verified one."
  (unless (%event-checkpoint-safe-name-p projection)
    (error "Unsafe checkpoint projection name: ~s" projection))
  (dolist (manifest-path (%event-checkpoint-manifest-paths projection)
                         (values nil nil))
    (let* ((manifest (ignore-errors (%event-read-json-file manifest-path)))
           (content (and manifest
                         (%event-verify-checkpoint-manifest projection
                                                            manifest
                                                            manifest-path))))
      (when content
        (return (values content manifest))))))

;;; R0e4 relational checkpoints use JSONL so complete baseline tables never
;;; need to exist as one Lisp string or vector. Version 1 exact-byte
;;; checkpoints remain unchanged for the small/file projections.

(defun %event-row-source-map (source visitor)
  (if (functionp source)
      (funcall source visitor)
      (progn
        (map nil visitor source)
        (values t (length source)))))

(defun make-event-jsonl-row-source (pathname)
  "Return a repeatable constant-space row source for a JSONL file."
  (lambda (visitor)
    (let ((visited 0))
      (handler-case
          (progn
            (%event-storage-observe :row-source pathname)
            (with-open-file (in pathname :direction :input
                                         :external-format :utf-8)
              (loop for line = (read-line in nil nil)
                    while line
                    do (unless (plusp (length line))
                         (error "Empty JSONL checkpoint source row"))
                       (let ((row (shasht:read-json line)))
                         (unless (hash-table-p row)
                           (error "JSONL checkpoint source row is not an object"))
                         (funcall visitor row)
                         (incf visited))))
            (values t visited))
        (error (condition) (values nil visited condition))))))

(defun make-event-jsonl-line-source (pathname)
  "Return a repeatable source of exact non-empty JSONL row strings."
  (lambda (visitor)
    (let ((visited 0))
      (handler-case
          (progn
            (%event-storage-observe :line-source pathname)
            (with-open-file (in pathname :direction :input
                                         :external-format :utf-8)
              (loop for line = (read-line in nil nil)
                    while line
                    do (unless (plusp (length line))
                         (error "Empty JSONL checkpoint source row"))
                       (funcall visitor line)
                       (incf visited)))
            (values t visited))
        (error (condition) (values nil visited condition))))))

(defun write-event-row-checkpoint (projection event-id row-source)
  "Atomically publish an incrementally hashed JSONL relational checkpoint.
ROW-SOURCE is a sequence or a function accepting a visitor and returning a
true completeness value. No complete row collection is retained here."
  (unless (%event-checkpoint-safe-name-p projection)
    (error "Unsafe checkpoint projection name: ~s" projection))
  (unless (and (integerp event-id) (not (minusp event-id)))
    (error "Checkpoint event id must be a non-negative integer"))
  (let* ((data-name (%event-checkpoint-data-name projection event-id))
         (data-path (%event-checkpoint-path data-name))
         (manifest-path
           (%event-checkpoint-path
            (%event-checkpoint-manifest-name projection event-id)))
         (temporary
           (make-pathname
            :name (format nil "~a-tmp-~d-~d" (pathname-name data-path)
                          (get-universal-time) (random 1000000))
            :type (pathname-type data-path) :defaults data-path))
         (digest (%event-make-sha256-digest))
         (row-count 0)
         (byte-length 0)
         (complete-p nil))
    (ensure-directories-exist data-path)
    (unwind-protect
        (progn
          (%event-storage-observe :write temporary)
          (with-open-file (out temporary :direction :output
                                          :if-exists :supersede
                                          :if-does-not-exist :create
                                          :element-type '(unsigned-byte 8))
            (multiple-value-bind (source-complete supplied-count source-error)
                (%event-row-source-map
                 row-source
                 (lambda (row)
                   (unless (hash-table-p row)
                     (error "Relational checkpoint row must be a JSON object"))
                   (let* ((*print-pretty* nil)
                          (line (shasht:write-json row nil))
                          (octets (%event-utf8-octets line)))
                     (write-sequence octets out)
                     (write-byte 10 out)
                     (%event-update-digest digest octets)
                     (%event-update-digest
                      digest (make-array 1 :element-type '(unsigned-byte 8)
                                           :initial-element 10))
                     (incf byte-length (1+ (length octets)))
                     (incf row-count))))
              (declare (ignore supplied-count))
              (unless source-complete
                (if source-error
                    (error "Refusing to publish an incomplete row checkpoint after ~d rows: ~a"
                           row-count source-error)
                    (error "Refusing to publish an incomplete row checkpoint")))
              (setf complete-p t))
            (finish-output out))
          (unless complete-p
            (error "Relational checkpoint source did not complete"))
          (let ((sha256 (%event-finish-digest-hex digest)))
            (uiop:rename-file-overwriting-target temporary data-path)
            (let ((manifest
                    (obj "schema_version" 2
                         "projection" projection
                         "event_id" event-id
                         "file" data-name
                         "encoding" "utf-8-jsonl"
                         "byte_length" byte-length
                         "row_count" row-count
                         "sha256" sha256
                         "created_at" (%event-now-iso8601))))
              (%event-atomic-write-json manifest-path manifest)
              manifest)))
      (when (probe-file temporary)
        (ignore-errors (delete-file temporary))))))

(defun write-event-row-checkpoint-lines (projection event-id line-source)
  "Publish exact JSONL lines without numeric/string reinterpretation.
Every line is still parsed as a JSON object before it is hashed and written."
  (unless (%event-checkpoint-safe-name-p projection)
    (error "Unsafe checkpoint projection name: ~s" projection))
  (unless (and (integerp event-id) (not (minusp event-id)))
    (error "Checkpoint event id must be a non-negative integer"))
  (let* ((data-name (%event-checkpoint-data-name projection event-id))
         (data-path (%event-checkpoint-path data-name))
         (manifest-path
           (%event-checkpoint-path
            (%event-checkpoint-manifest-name projection event-id)))
         (temporary
           (make-pathname
            :name (format nil "~a-tmp-~d-~d" (pathname-name data-path)
                          (get-universal-time) (random 1000000))
            :type (pathname-type data-path) :defaults data-path))
         (digest (%event-make-sha256-digest))
         (newline (make-array 1 :element-type '(unsigned-byte 8)
                                :initial-element 10))
         (row-count 0)
         (byte-length 0))
    (ensure-directories-exist data-path)
    (unwind-protect
        (progn
          (%event-storage-observe :write temporary)
          (with-open-file (out temporary :direction :output
                                          :if-exists :supersede
                                          :if-does-not-exist :create
                                          :element-type '(unsigned-byte 8))
            (multiple-value-bind (complete supplied-count source-error)
                (%event-row-source-map
                 line-source
                 (lambda (line)
                   (unless (and (stringp line) (plusp (length line))
                                (hash-table-p (shasht:read-json line)))
                     (error "Relational checkpoint line is not a JSON object"))
                   (let ((octets (%event-utf8-octets line)))
                     (write-sequence octets out)
                     (write-byte 10 out)
                     (%event-update-digest digest octets)
                     (%event-update-digest digest newline)
                     (incf byte-length (1+ (length octets)))
                     (incf row-count))))
              (declare (ignore supplied-count))
              (unless complete
                (if source-error
                    (error "Refusing incomplete exact-line checkpoint after ~d rows: ~a"
                           row-count source-error)
                    (error "Refusing incomplete exact-line checkpoint"))))
            (finish-output out))
          (let ((sha256 (%event-finish-digest-hex digest)))
            (uiop:rename-file-overwriting-target temporary data-path)
            (let ((manifest
                    (obj "schema_version" 2
                         "projection" projection
                         "event_id" event-id
                         "file" data-name
                         "encoding" "utf-8-jsonl"
                         "byte_length" byte-length
                         "row_count" row-count
                         "sha256" sha256
                         "created_at" (%event-now-iso8601))))
              (%event-atomic-write-json manifest-path manifest)
              manifest)))
      (when (probe-file temporary)
        (ignore-errors (delete-file temporary))))))

(defun %event-hash-file-incrementally (pathname)
  (let ((digest (%event-make-sha256-digest))
        (buffer (make-array 65536 :element-type '(unsigned-byte 8)))
        (byte-length 0))
    (%event-storage-observe :verify pathname)
    (with-open-file (in pathname :direction :input
                                :element-type '(unsigned-byte 8))
      (loop for count = (read-sequence buffer in)
            while (plusp count)
            do (%event-update-digest digest buffer :end count)
               (incf byte-length count)))
    (values (%event-finish-digest-hex digest) byte-length)))

(defun %event-valid-row-checkpoint (projection manifest manifest-path)
  "Return the verified data path and row count, or NIL. Validation parses every
row before any consumer sees it, while retaining no rows."
  (handler-case
      (let* ((event-id (gethash "event_id" manifest))
             (filename (gethash "file" manifest))
             (data-path (and (stringp filename)
                             (%event-checkpoint-path filename)))
             (expected-data
               (and (integerp event-id)
                    (%event-checkpoint-data-name projection event-id)))
             (expected-manifest
               (and (integerp event-id)
                    (%event-checkpoint-manifest-name projection event-id)))
             (expected-bytes (gethash "byte_length" manifest))
             (expected-rows (gethash "row_count" manifest))
             (expected-sha (gethash "sha256" manifest)))
        (when (and (eql 2 (gethash "schema_version" manifest))
                   (string= projection (gethash "projection" manifest))
                   (integerp event-id) (not (minusp event-id))
                   (string= filename expected-data)
                   (string= (file-namestring manifest-path) expected-manifest)
                   (string= "utf-8-jsonl" (gethash "encoding" manifest))
                   (integerp expected-bytes) (not (minusp expected-bytes))
                   (integerp expected-rows) (not (minusp expected-rows))
                   (stringp expected-sha) (probe-file data-path))
          (multiple-value-bind (actual-sha actual-bytes)
              (%event-hash-file-incrementally data-path)
            (when (and (= expected-bytes actual-bytes)
                       (string= expected-sha actual-sha))
              (let ((rows 0) (valid-p t))
                (%event-storage-observe :validate data-path)
                (with-open-file (in data-path :direction :input
                                              :external-format :utf-8)
                  (loop for line = (read-line in nil nil)
                        while line
                        do (unless (and (plusp (length line))
                                        (hash-table-p
                                         (shasht:read-json line)))
                             (setf valid-p nil)
                             (loop-finish))
                           (incf rows)))
                (when (and valid-p (= rows expected-rows))
                  (values data-path rows)))))))
    (error () nil)))

(defun map-verified-event-row-checkpoint (projection visitor)
  "Visit the newest fully verified JSONL row checkpoint.
Corrupt newer artifacts fall back before VISITOR receives any row. Returns
complete-p, manifest, and visited row count."
  (unless (%event-checkpoint-safe-name-p projection)
    (error "Unsafe checkpoint projection name: ~s" projection))
  (dolist (manifest-path (%event-checkpoint-manifest-paths projection)
                         (values nil nil 0))
    (let ((manifest (ignore-errors (%event-read-json-file manifest-path))))
      (when manifest
        (multiple-value-bind (data-path expected-rows)
            (%event-valid-row-checkpoint projection manifest manifest-path)
          (when data-path
            (let ((visited 0))
              (handler-case
                  (progn
                    (%event-storage-observe :deliver data-path)
                    (with-open-file (in data-path :direction :input
                                                  :external-format :utf-8)
                      (loop for line = (read-line in nil nil)
                            while line
                            do (funcall visitor (shasht:read-json line))
                               (incf visited)))
                    (return (values (= visited expected-rows) manifest visited)))
                (error () (return (values nil manifest visited)))))))))))

(defun map-verified-event-row-checkpoint-lines (projection visitor)
  "Visit exact lines from the newest fully verified JSONL checkpoint."
  (unless (%event-checkpoint-safe-name-p projection)
    (error "Unsafe checkpoint projection name: ~s" projection))
  (dolist (manifest-path (%event-checkpoint-manifest-paths projection)
                         (values nil nil 0))
    (let ((manifest (ignore-errors (%event-read-json-file manifest-path))))
      (when manifest
        (multiple-value-bind (data-path expected-rows)
            (%event-valid-row-checkpoint projection manifest manifest-path)
          (when data-path
            (let ((visited 0))
              (handler-case
                  (progn
                    (%event-storage-observe :deliver-lines data-path)
                    (with-open-file (in data-path :direction :input
                                                  :external-format :utf-8)
                      (loop for line = (read-line in nil nil)
                            while line
                            do (funcall visitor line)
                               (incf visited)))
                    (return (values (= visited expected-rows) manifest visited)))
                (error () (return (values nil manifest visited)))))))))))

(defun %jsonl-replay-events (&key from to limit types exclude-types)
  "Reconstructs a readable timeline of events, oldest first, optionally
bounded by FROM/TO (universal-time). Reads from disk -- the ring is only a
fast-path cache for recent events, not authoritative -- correctness over
speed, since this is for reflection/audit, not a hot path."
  (when (and limit (not (and (integerp limit) (plusp limit))))
    (error "Replay LIMIT must be a positive integer or NIL"))
  (let ((matches nil)
        (bounded (and limit (make-array limit :initial-element nil)))
        (matched-count 0))
    (if (%event-scan-disk
         (lambda (event)
           (if bounded
               (setf (aref bounded (mod matched-count limit)) event)
               (push event matches))
           (incf matched-count))
         :from from :to to :types types :exclude-types exclude-types)
        (if bounded
            (loop with count = (min matched-count limit)
                  with start = (if (> matched-count limit)
                                   (mod matched-count limit)
                                   0)
                  for index below count
                  collect (aref bounded (mod (+ start index) limit)))
            (nreverse matches))
        nil)))

(defun replay-events (&rest arguments &key &allow-other-keys)
  (if *event-authority-port*
      (apply (getf *event-authority-port* :replay) arguments)
      (apply #'%jsonl-replay-events arguments)))

;;; --- wire into the existing turn flow -----------------------------------
;;; AUTO-TURN is the one entry point all three channels (CLI, Telegram, the
;;; web terminal's /api/v2/send) already converge on -- wrapping it here,
;;; rather than re-deriving user/agent-message events separately per
;;; channel, guarantees exactly one user-message and one agent-message per
;;; real turn regardless of which channel it came in on. Already wrapped
;;; once today (enhancements.lisp, for the live tool-list refresh) --
;;; PAI-BASE-AUTO-TURN-EVENTLOG is a distinct name so this layers on top
;;; of that, not instead of it.

(unless (fboundp 'pai-base-auto-turn-eventlog)
  (setf (fdefinition 'pai-base-auto-turn-eventlog) (fdefinition 'auto-turn)))
(defvar *public-inbound-channel* "terminal")
(defun auto-turn (prompt)
  (let* ((trace-id (and (boundp '*timing-trace-id*)
                        (stringp (symbol-value '*timing-trace-id*))
                        (symbol-value '*timing-trace-id*)))
         (turn-id (and (boundp '*timing-turn-id*)
                       (stringp (symbol-value '*timing-turn-id*))
                       (symbol-value '*timing-turn-id*)))
         (user-event-id (log-event "user-message"
                                    (obj "text" (if (stringp prompt) prompt (format nil "~a" prompt))
                                         "trace_id" (or trace-id :null)
                                         "turn_id" (or turn-id :null))))
         (*current-causing-event-id* user-event-id))
    (cond (*turn-capture-user-event-fn*
           (ignore-errors (funcall *turn-capture-user-event-fn* user-event-id prompt)))
          ((fboundp '%turn-capture-register-user-event)
           (ignore-errors
             (funcall '%turn-capture-register-user-event user-event-id prompt))))
    (let* ((pull-reply
             (and (fboundp 'pull-reciprocity-handle-inbound)
                  (funcall 'pull-reciprocity-handle-inbound prompt
                           :cause-id (format nil "event:~a" user-event-id)
                           :channel *public-inbound-channel*)))
           (reply
             (if pull-reply
                 (funcall 'pull-reciprocity-present-reply pull-reply)
                 (funcall 'pai-base-auto-turn-eventlog prompt))))
      ;; records every public assistant segment. Its completion hook
      ;; returns true when it handled this turn; otherwise retain the exact
      ;; legacy single-final event as a fail-open fallback.
      (unless (if *turn-capture-completion-fn*
                  (ignore-errors (funcall *turn-capture-completion-fn* reply user-event-id))
                  (and (fboundp '%turn-capture-register-completion)
                       (ignore-errors
                         (funcall '%turn-capture-register-completion
                                  reply user-event-id))))
        (log-event "agent-message" (obj "text" (if (stringp reply) reply :null))
                   :caused-by user-event-id))
      ;; Terminal has no transport wrapper. Observe its already-completed
      ;; public return here; Telegram and web are observed at their actual
      ;; transport/presentation seams. What the envelope should contain is the
      ;; publication layer's business, so this reports the three facts it has
      ;; and lets that layer decide.
      (when (and (string= *public-inbound-channel* "terminal")
                 *public-presentation-observer*)
        ;; Deliberately not wrapped in IGNORE-ERRORS: the call it replaces was
        ;; not either, and turning an observer fault silent is a behaviour
        ;; change, not a structural one.
        (funcall *public-presentation-observer*
                 "terminal" reply user-event-id))
      reply)))

;;; --- tool-call / tool-result ---------------------------------------------
;;; EXECUTE is already a multi-layer wrap chain (enhancements.lisp ->
;;; web-terminal.lisp -> runware.lisp, each renaming the previous to its own
;;; PAI-BASE-EXECUTE-*). This adds one more layer, purely for logging --
;;; never changes the result, never intercepts a specific tool name the way
;;; the others do.

(defun call-with-tool-event-observation (tool-call thunk)
  "Observe one TOOL-CALL around exactly one invocation of THUNK."
  (unless (functionp thunk)
    (error "Tool event observer requires an explicit function."))
  (let* ((name (ignore-errors (ref tool-call "function" "name")))
         (args (ignore-errors (ref tool-call "function" "arguments")))
         (tool-call-id (and (hash-table-p tool-call) (gethash "id" tool-call)))
         (tool-result-id (%public-tool-result-id-for-call tool-call))
         (call-event-id (log-event "tool-call"
                                    (obj "name" (or name :null)
                                         "arguments" (or args :null)
                                         "tool_call_id" (or tool-call-id :null)
                                         "tool_result_id" (or tool-result-id :null))
                                    :caused-by *current-causing-event-id*)))
    (cond (*turn-capture-tool-call-fn*
           (ignore-errors (funcall *turn-capture-tool-call-fn* call-event-id tool-call)))
          ((fboundp '%turn-capture-register-tool-call-event)
           (ignore-errors
             (funcall '%turn-capture-register-tool-call-event call-event-id tool-call))))
    (let* ((*public-tool-call-id* tool-call-id)
           (*public-tool-result-id*
             (or tool-result-id
                 (%public-tool-result-id-for-call tool-call call-event-id)))
           (*public-tool-call-event-id* call-event-id)
           (result (funcall thunk tool-call)))
      (let ((result-event-id
              (log-event "tool-result"
                         (obj "name" (or name :null)
                              "tool_call_id" (or *public-tool-call-id* :null)
                              "tool_result_id" (or *public-tool-result-id* :null)
                              "content"
                              (let ((c (and (hash-table-p result)
                                            (gethash "content" result))))
                                (if (stringp c) c :null)))
                         :caused-by call-event-id)))
        (cond (*turn-capture-tool-result-fn*
               (ignore-errors
                 (funcall *turn-capture-tool-result-fn*
                          result-event-id tool-call result)))
              ((fboundp '%turn-capture-register-tool-result-event)
               (ignore-errors
                 (funcall '%turn-capture-register-tool-result-event
                          result-event-id tool-call result)))))
      result)))

(when (or (not (fboundp 'tool-dispatch-legacy-wrapper-enabled-p))
          (funcall 'tool-dispatch-legacy-wrapper-enabled-p))
  (unless (fboundp 'pai-base-execute-eventlog)
    (setf (fdefinition 'pai-base-execute-eventlog) (fdefinition 'execute)))
  (defun execute (tool-call)
    (call-with-tool-event-observation
     tool-call
     (lambda (call) (funcall 'pai-base-execute-eventlog call)))))

;;; --- self-mod-proposed / accepted / rejected / rolled-back --------------
;;; PROPOSE-LOOP already returns a human-readable result string (self-
;;; mod.lisp) that fully encodes the outcome: "REJECTED (static check): ..."
;;; / "REJECTED (verifier): ..." / "APPROVED by verifier ... installed." /
;;; "install failed, rolled back: ...". Parsing that string, rather than
;;; re-deriving the verdict independently, guarantees this can never
;;; disagree with what PROPOSE-LOOP itself actually decided. Already
;;; wrapped once today (eval-journal.lisp, for its own pre-call journal) --
;;; PAI-BASE-PROPOSE-LOOP-EVENTLOG is a distinct name so this layers on
;;; top of that.

(unless (fboundp 'pai-base-propose-loop-eventlog)
  (setf (fdefinition 'pai-base-propose-loop-eventlog) (fdefinition 'propose-loop)))
(defun propose-loop (proposed-src)
  (let ((propose-event-id (log-event "self-mod-proposed" (obj "source" proposed-src)
                                      :caused-by *current-causing-event-id*)))
    (let ((result (funcall 'pai-base-propose-loop-eventlog proposed-src)))
      (cond
        ((and (stringp result) (>= (length result) 8) (string= (subseq result 0 8) "APPROVED"))
         (log-event "self-mod-accepted" (obj "result" result) :caused-by propose-event-id))
        ((and (stringp result) (search "rolled back" result))
         (log-event "self-mod-rolled-back" (obj "result" result) :caused-by propose-event-id))
        ((and (stringp result) (>= (length result) 8) (string= (subseq result 0 8) "REJECTED"))
         (log-event "self-mod-rejected" (obj "result" result) :caused-by propose-event-id))
        (t (log-event "self-mod-proposed"
                       (obj "note" "unrecognized result shape from propose-loop" "result" result)
                       :caused-by propose-event-id)))
      result)))
