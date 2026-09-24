;;;; storage-substrate.lisp -- capability-shaped durable storage boundary.

(in-package :agent)

(export '(storage-backend storage-error storage-integrity-error
          storage-conflict-error storage-unavailable-error
          storage-capabilities storage-close storage-append-event storage-append-event-if-head
          storage-read-event storage-read-event-before-position
          storage-event-position
          storage-scan-events storage-map-events
          storage-map-event-receipts
          storage-query-events storage-range-max-event-id
          storage-recent-events storage-max-event-id
          storage-max-event-id-through-position storage-head-position
          storage-authority-boundary
          storage-checkpoint-source-binding
          storage-publish-checkpoint storage-load-checkpoint))

(define-condition storage-error (error)
  ((operation :initarg :operation :reader storage-error-operation)
   (detail :initarg :detail :reader storage-error-detail))
  (:report (lambda (condition stream)
             (format stream "Storage ~a failed: ~a"
                     (storage-error-operation condition)
                     (storage-error-detail condition)))))

(define-condition storage-integrity-error (storage-error) ())
(define-condition storage-conflict-error (storage-error) ())
(define-condition storage-unavailable-error (storage-error) ())

(defclass storage-backend () ())

(defgeneric storage-capabilities (backend))
(defgeneric storage-close (backend))
(defgeneric storage-append-event
    (backend type payload &key agent-id occurred-at caused-by tick-id
                          affect-snapshot))
(defgeneric storage-append-event-if-head
    (backend expected-head-position type payload
     &key agent-id occurred-at caused-by tick-id affect-snapshot)
  (:documentation
   "Atomically compare the global physical event head and append. A stale head
signals STORAGE-CONFLICT-ERROR without appending. This is an admission primitive,
not authorization: the caller must validate its policy against that exact head."))
(defmethod storage-append-event-if-head
    ((backend storage-backend) expected-head-position type payload
     &key agent-id occurred-at caused-by tick-id affect-snapshot)
  (declare (ignore backend expected-head-position type payload agent-id occurred-at
                   caused-by tick-id affect-snapshot))
  (error 'storage-unavailable-error :operation :conditional-append
         :detail "backend does not support atomic conditional append"))
(defgeneric storage-read-event (backend event-id &key agent-id event-type))
(defgeneric storage-read-event-before-position
    (backend agent-id event-id before-position)
  (:documentation
   "Return the newest verified same-agent occurrence of EVENT-ID strictly before
physical BEFORE-POSITION, or NIL. Imported duplicate logical IDs remain distinct
by physical position; this is a bounded authority read, not a replay."))
(defgeneric storage-event-position (backend agent-id event-id)
  (:documentation
   "Return the unique physical position of one same-agent logical event ID,
or NIL. Refuse imported duplicate IDs rather than guessing an as-of boundary."))
(defgeneric storage-scan-events
    (backend &key agent-id after-id event-type limit))
(defgeneric storage-map-events
    (backend visitor &key agent-id after-position through-position
                          event-type event-types limit)
  (:documentation
   "Visit verified events in physical order inside the optional
(AFTER-POSITION, THROUGH-POSITION] interval. Return completion, last matching
physical position, row count, and exact stored JSON byte count. EVENT-TYPE
selects one type; EVENT-TYPES selects a bounded set and is mutually exclusive
with it."))
(defgeneric storage-map-event-receipts
    (backend visitor &key agent-id after-position through-position event-types)
  (:documentation
   "Visit exact verified durable receipts in one bounded physical interval.
Return completion, the last matching physical position (or AFTER-POSITION),
and row count."))
(defgeneric storage-query-events
    (backend &key agent-id after-id through-id from to limit event-types
                  exclude-event-types)
  (:documentation
   "Return verified matching events in physical order. LIMIT retains the
newest matching rows while preserving chronological output. The second value
is the greatest logical ID in the ID window before content/time filtering,
observed from the same storage snapshot."))
(defgeneric storage-range-max-event-id
    (backend &key agent-id after-id through-id)
  (:documentation
   "Return the greatest logical ID in an inclusive/exclusive ID window,
independent of content filters."))
(defgeneric storage-recent-events
    (backend event-types limit &key agent-id before-event-id))
(defgeneric storage-max-event-id (backend &key agent-id))
(defgeneric storage-max-event-id-through-position
    (backend agent-id through-position)
  (:documentation
   "Return the greatest logical ID in a same-agent physical prefix. Unlike
the last row's ID this remains correct for imported logical-ID rewinds."))
(defgeneric storage-head-position (backend &key agent-id))
(defgeneric storage-authority-boundary (backend &key agent-id)
  (:documentation
   "Capture one immutable per-agent authority watermark. The returned object
contains the event storage identity, logical event ID, physical position and
source binding observed under one backend lock."))
(defgeneric storage-checkpoint-source-binding
    (backend &key agent-id through-event-id through-position)
  (:documentation
   "Return an opaque identity and boundary hash binding a checkpoint to its
exact event database and durable watermark."))
(defgeneric storage-publish-checkpoint
    (backend projection-name state &key agent-id through-event-id
                                    through-position projector-revision
                                    policy-revision))
(defgeneric storage-load-checkpoint (backend projection-name &key agent-id))

(defun %storage-object (&rest fields)
  (loop with object = (make-hash-table :test #'equal)
        for (key value) on fields by #'cddr
        do (setf (gethash key object) value)
        finally (return object)))

(defun %storage-required-string (value field &key (maximum 256))
  (unless (and (stringp value) (plusp (length value))
               (<= (length value) maximum))
    (error 'storage-error :operation :validate
                          :detail (format nil "~a must be a non-empty string of at most ~d characters"
                                          field maximum)))
  value)

(defun %storage-positive-integer (value field &key zero-allowed)
  (unless (and (integerp value) (if zero-allowed (not (minusp value)) (plusp value)))
    (error 'storage-error :operation :validate
                          :detail (format nil "~a must be ~:[positive~;non-negative~]"
                                          field zero-allowed)))
  value)

(defun %storage-json (value)
  (handler-case
      (let ((*print-pretty* nil)) (shasht:write-json value nil))
    (error (condition)
      (error 'storage-error :operation :serialize :detail condition))))

(defun %storage-json-read (text operation)
  (handler-case (shasht:read-json text)
    (error (condition)
      (error 'storage-integrity-error :operation operation :detail condition))))

(defun %storage-sha256 (text)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :sha256 (sb-ext:string-to-octets text :external-format :utf-8))))

(defun %storage-sha256-string-parts (parts &key (chunk-size 65536))
  "Hash strings as one UTF-8 sequence without materializing their concatenation."
  (unless (and (integerp chunk-size) (plusp chunk-size))
    (error 'storage-error :operation :integrity
                          :detail "integrity hash chunk size must be positive"))
  (let ((digest (ironclad:make-digest :sha256)))
    (dolist (part parts)
      (unless (stringp part)
        (error 'storage-error :operation :integrity
                              :detail "integrity hash parts must be strings"))
      (loop for start from 0 below (length part) by chunk-size
            for end = (min (length part) (+ start chunk-size))
            do (ironclad:update-digest
                digest
                (sb-ext:string-to-octets
                 part :external-format :utf-8 :start start :end end))))
    (ironclad:byte-array-to-hex-string
     (ironclad:produce-digest digest))))

(defun %storage-checkpoint-integrity-input
    (projection-name agent-id through-event-id through-position projector-revision
     policy-revision state-json)
  ;; Length-prefix every string so concatenation is unambiguous without
  ;; depending on hash-table iteration order or a backend's JSON functions.
  (with-output-to-string (stream)
    (dolist (value (list projection-name agent-id
                         (write-to-string through-event-id)
                         (write-to-string through-position)
                         projector-revision policy-revision state-json))
      (format stream "~d:~a" (length value) value))))

(defun %storage-checkpoint-integrity-sha256
    (projection-name agent-id through-event-id through-position projector-revision
     policy-revision state-json)
  "Return the legacy checkpoint digest without constructing its full input string."
  (let ((values (list projection-name agent-id
                      (write-to-string through-event-id)
                      (write-to-string through-position)
                      projector-revision policy-revision state-json)))
    (%storage-sha256-string-parts
     (loop for value in values
           append (list (write-to-string (length value)) ":" value)))))
