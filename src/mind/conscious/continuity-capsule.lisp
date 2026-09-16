;;;; continuity-capsule.lisp -- bounded pull-based continuity composition.
;;;;
;;;; Contributors do not push mutable state into a new "self" object.  At a
;;;; reasoning boundary the capsule asks registered owners for event-grounded
;;;; observations, validates them, and returns detached data.  A contributor
;;;; receives a small detached boundary envelope and consults its own
;;;; rebuildable projection.  The composer keeps the evidence window only to
;;;; validate citations; it grants no effect, attention, provider, or
;;;; publication authority.

(in-package :agent)

(export '(continuity-capsule-register-contributor
          continuity-capsule-unregister-contributor
          continuity-capsule-contributor-report
          continuity-capsule-build
          continuity-capsule-context-records
          continuity-capsule-format-elapsed))

(defparameter *continuity-capsule-schema-version* 1)
(defparameter *continuity-capsule-max-contributors* 32)
(defparameter *continuity-capsule-max-contributions* 32)
(defparameter *continuity-capsule-max-content-characters* 1600)
(defparameter *continuity-capsule-max-total-characters* 6000)

(defvar *continuity-capsule-contributors* (make-hash-table :test #'equal))
(defvar *continuity-capsule-contributor-lock*
  (bt:make-lock "continuity-capsule-contributors"))

(defun %continuity-capsule-name (value label)
  (let ((text (string-downcase (string value))))
    (unless (and (plusp (length text)) (<= (length text) 128))
      (error "~a must be a bounded non-empty name" label))
    text))

(defun continuity-capsule-register-contributor
    (name function &key (order 500) (revision "v1") required)
  "Register or replace one read-only continuity contributor.

FUNCTION receives a small detached boundary request and an optional owner-only
projection snapshot, then returns an array/list of contribution objects. It
must consult state through its owning projection; the general event evidence
window is deliberately not copied into the request.
Registration grants no authority and performs no durable write. Re-registering
NAME replaces that provider, which makes symbol registrations reload safe."
  (let ((id (%continuity-capsule-name name "Contributor name"))
        (revision-text (%continuity-capsule-name revision "Contributor revision")))
    (unless (or (functionp function)
                (and (symbolp function) (fboundp function)))
      (error "Continuity contributor ~a is not callable" id))
    (unless (and (integerp order) (<= 0 order 10000))
      (error "Continuity contributor order is invalid"))
    (bt:with-lock-held (*continuity-capsule-contributor-lock*)
      (unless (or (gethash id *continuity-capsule-contributors*)
                  (< (hash-table-count *continuity-capsule-contributors*)
                     *continuity-capsule-max-contributors*))
        (error "Continuity contributor bound is exhausted"))
      (setf (gethash id *continuity-capsule-contributors*)
            (obj "name" id "function" function "order" order
                 "revision" revision-text "required" (if required t nil))))
    id))

(defun continuity-capsule-unregister-contributor (name)
  "Remove one contributor. This affects only future derived capsules."
  (bt:with-lock-held (*continuity-capsule-contributor-lock*)
    (remhash (%continuity-capsule-name name "Contributor name")
             *continuity-capsule-contributors*))
  t)

(defun %continuity-capsule-contributor-snapshot ()
  (bt:with-lock-held (*continuity-capsule-contributor-lock*)
    (let ((rows nil))
      (maphash (lambda (ignored row)
                 (declare (ignore ignored))
                 (push row rows))
               *continuity-capsule-contributors*)
      (sort rows
            (lambda (left right)
              (or (< (gethash "order" left) (gethash "order" right))
                  (and (= (gethash "order" left) (gethash "order" right))
                       (string< (gethash "name" left)
                                (gethash "name" right)))))))))

(defun continuity-capsule-contributor-report ()
  "Return content-free registration data for inspection."
  (let ((rows (%continuity-capsule-contributor-snapshot)))
    (obj "schema_version" *continuity-capsule-schema-version*
         "contributor_count" (length rows)
         "contributors"
         (coerce
          (mapcar (lambda (row)
                    (obj "name" (gethash "name" row)
                         "order" (gethash "order" row)
                         "revision" (gethash "revision" row)
                         "required" (if (gethash "required" row) t nil)))
                  rows)
          'vector))))

(defun continuity-capsule-format-elapsed (seconds)
  "Render a deliberately coarse non-negative elapsed interval."
  (unless (and (integerp seconds) (not (minusp seconds)))
    (error "Elapsed seconds must be a non-negative integer"))
  (cond
    ((< seconds 60) "less than a minute")
    ((< seconds 3600)
     (let ((minutes (floor seconds 60)))
       (format nil "~d minute~:p" minutes)))
    ((< seconds 86400)
     (let ((hours (floor seconds 3600)))
       (format nil "~d hour~:p" hours)))
    (t
     (let ((days (floor seconds 86400)))
       (format nil "~d day~:p" days)))))

(defun %continuity-capsule-items (value label maximum)
  (let ((items (cond ((vectorp value) (coerce value 'list))
                     ((listp value) (copy-list value))
                     (t (error "~a must be an array" label)))))
    (unless (<= (length items) maximum)
      (error "~a exceeds its item bound" label))
    items))

(defun %continuity-capsule-event-id-p (value)
  (or (and (integerp value) (plusp value))
      (and (stringp value) (plusp (length value))
           (<= (length value) 256))))

(defun %continuity-capsule-event-index (events)
  (let ((index (make-hash-table :test #'equal)))
    (dolist (event events index)
      (when (and (hash-table-p event)
                 (%continuity-capsule-event-id-p (gethash "id" event)))
        (setf (gethash (gethash "id" event) index) event)))))

(defun %continuity-capsule-validate-contribution
    (value contributor as-of event-index)
  (unless (hash-table-p value)
    (error "Continuity contributor ~a returned a non-object" contributor))
  (let ((keys nil))
    (maphash (lambda (key ignored) (declare (ignore ignored)) (push key keys)) value)
    (unless (equal (sort keys #'string<)
                   '("content" "kind" "observed_at" "source_id" "status"))
      (error "Continuity contributor ~a returned an invalid shape" contributor)))
  (let ((source-id (gethash "source_id" value))
        (observed-at (gethash "observed_at" value))
        (kind (gethash "kind" value))
        (status (gethash "status" value))
        (content (gethash "content" value)))
    (unless (and (%continuity-capsule-event-id-p source-id)
                 (gethash source-id event-index))
      (error "Continuity contributor ~a cited unavailable source evidence"
             contributor))
    (unless (and (integerp observed-at) (not (minusp observed-at))
                 (<= observed-at as-of))
      (error "Continuity contributor ~a returned invalid observation time"
             contributor))
    (dolist (pair (list (cons kind 64) (cons status 64)
                        (cons content *continuity-capsule-max-content-characters*)))
      (unless (and (stringp (car pair)) (plusp (length (car pair)))
                   (<= (length (car pair)) (cdr pair)))
        (error "Continuity contributor ~a returned invalid bounded text"
               contributor)))
    (obj "contributor" contributor "kind" kind "status" status
         "source_id" source-id "observed_at" observed-at "content" content)))

(defun %continuity-capsule-request
    (mind-identity-id as-of clock-identity boundary-kind boundary-source-id
     time-context)
  ;; Round-trip the small envelope so a contributor cannot mutate caller data.
  ;; Evidence stays with the composer; owners query their own projections.
  (shasht:read-json
   (shasht:write-json
    (obj "schema_version" *continuity-capsule-schema-version*
         "mind_identity_id" mind-identity-id "as_of" as-of
         "clock_identity" clock-identity "boundary_kind" boundary-kind
         "boundary_source_id" boundary-source-id "time_context" time-context)
    nil)))

(defun continuity-capsule-build
    (&key mind-identity-id as-of clock-identity boundary-kind
          boundary-source-id time-context events contributor-contexts)
  "Derive one bounded temporal-continuity capsule without writes or model calls."
  (unless (and (stringp mind-identity-id) (plusp (length mind-identity-id))
               (<= (length mind-identity-id) 256))
    (error "Continuity capsule mind identity is invalid"))
  (unless (and (integerp as-of) (not (minusp as-of)))
    (error "Continuity capsule as-of time is invalid"))
  (dolist (pair (list (cons clock-identity 128) (cons boundary-kind 128)
                      (cons time-context 512)))
    (unless (and (stringp (car pair)) (plusp (length (car pair)))
                 (<= (length (car pair)) (cdr pair)))
      (error "Continuity capsule boundary text is invalid")))
  (unless (%continuity-capsule-event-id-p boundary-source-id)
    (error "Continuity capsule boundary source is invalid"))
  (unless (or (null contributor-contexts) (hash-table-p contributor-contexts))
    (error "Continuity capsule contributor contexts must be an object"))
  (let* ((supplied-events (%continuity-capsule-items
                           events "Capsule events" most-positive-fixnum))
         (boundary-position
           (position boundary-source-id supplied-events :test #'equal
                     :key (lambda (event)
                            (and (hash-table-p event) (gethash "id" event)))))
         ;; Later concurrent events cannot ground this boundary's capsule.
         (event-list
           (and boundary-position
                (subseq supplied-events 0 (1+ boundary-position))))
         (event-index (%continuity-capsule-event-index event-list))
         (providers (%continuity-capsule-contributor-snapshot))
         (request (%continuity-capsule-request
                   mind-identity-id as-of clock-identity boundary-kind
                   boundary-source-id time-context))
         (contributions nil)
         (errors nil)
         (required-failure-p nil)
         (used 0))
    (unless boundary-position
      (error "Continuity capsule boundary source is outside supplied evidence"))
    (dolist (provider providers)
      (let ((name (gethash "name" provider)))
        (handler-case
            (dolist (candidate
                     (%continuity-capsule-items
                      (funcall (gethash "function" provider) request
                               (and contributor-contexts
                                    (gethash name contributor-contexts)))
                      "Contributor output" *continuity-capsule-max-contributions*))
              (let* ((row (%continuity-capsule-validate-contribution
                           candidate name as-of event-index))
                     (size (length (gethash "content" row))))
                (when (>= (length contributions)
                          *continuity-capsule-max-contributions*)
                  (error "Continuity contribution bound is exhausted"))
                (when (> (+ used size) *continuity-capsule-max-total-characters*)
                  (error "Continuity contribution character bound is exhausted"))
                (unless (find row contributions
                              :test (lambda (left right)
                                      (and (equal (gethash "source_id" left)
                                                  (gethash "source_id" right))
                                           (string= (gethash "kind" left)
                                                    (gethash "kind" right)))))
                  (incf used size)
                  (push row contributions))))
          (error (condition)
            (when (gethash "required" provider)
              (setf required-failure-p t))
            (push (obj "contributor" name
                       "required" (if (gethash "required" provider) t nil)
                       "error_class"
                       (string-downcase (symbol-name (type-of condition))))
                  errors)))))
    (setf contributions (nreverse contributions)
          errors (nreverse errors))
    (let* ((status (cond (required-failure-p "unavailable")
                         (errors "partial")
                         (t "complete")))
           (orientation
             (obj "contributor" "runtime-boundary"
                  "kind" "temporal-orientation" "status" status
                  "source_id" boundary-source-id "observed_at" as-of
                  "content"
                  (format nil
                          "Temporal orientation (runtime-derived): ~a Boundary kind: ~a. Continuity source coverage: ~a (~d registered source~:p, ~d available contribution~:p). Elapsed time is not by itself evidence of continuous experience, importance, urgency, or affect."
                          time-context boundary-kind status (length providers)
                          (length contributions))))
           (revision
             (format nil "temporal-continuity-v1~{|~a/~a~}"
                     (loop for provider in providers
                           append (list (gethash "name" provider)
                                        (gethash "revision" provider))))))
      (obj "schema_version" *continuity-capsule-schema-version*
           "status" status "mind_identity_id" mind-identity-id
           "as_of" as-of "clock_identity" clock-identity
           "boundary_kind" boundary-kind
           "boundary_source_id" boundary-source-id
           "composition_revision" revision
           "contributions" (coerce (cons orientation contributions) 'vector)
           "errors" (coerce errors 'vector)))))

(defun continuity-capsule-context-records (capsule)
  "Render validated capsule contributions into ordinary context rows."
  (unless (and (hash-table-p capsule)
               (= *continuity-capsule-schema-version*
                  (gethash "schema_version" capsule -1)))
    (error "Continuity capsule is invalid"))
  (map 'vector
       (lambda (row)
         (obj "source_id" (gethash "source_id" row)
              "content" (gethash "content" row)))
       (gethash "contributions" capsule)))
