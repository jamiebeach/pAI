;;;; runtime-observer-registry.lisp -- explicit, non-actuating observation bus.

(in-package :agent)

(export '(runtime-observer-register runtime-observer-unregister
          runtime-observer-emit runtime-observer-report
          runtime-observer-assert runtime-observer-reset
          runtime-authority-declare runtime-authority-report
          runtime-authority-assert))

(defvar *runtime-observers* (make-hash-table :test #'equal))
(defvar *runtime-observer-required* (make-hash-table :test #'equal))
(defvar *runtime-authorities* (make-hash-table :test #'equal))
(defvar *runtime-authority-required* (make-hash-table :test #'equal))
(defvar *runtime-observer-lock* (bt:make-lock "runtime-observers"))

(defun runtime-observer-register (event name function
                                  &key (capability :observe) required)
  "Register NAME once for EVENT. Observers never receive send authority."
  (unless (eq capability :observe)
    (error "Observer registry only accepts :OBSERVE capability, got ~s" capability))
  (unless (or (functionp function) (and (symbolp function) (fboundp function)))
    (error "Observer ~a is not callable" name))
  (let ((key (string-downcase (string event)))
        (id (string-downcase (string name))))
    (bt:with-lock-held (*runtime-observer-lock*)
      (when required
        (setf (gethash (format nil "~a/~a" key id)
                       *runtime-observer-required*) t))
      (let* ((rows (gethash key *runtime-observers*))
             (existing (find id rows :test #'string=
                             :key (lambda (row) (gethash "name" row)))))
        (if existing
            (setf (gethash "function" existing) function
                  (gethash "required" existing) (not (null required)))
            (push (obj "name" id "function" function
                       "capability" "observe"
                       "required" (not (null required)))
                  (gethash key *runtime-observers*)))))
    id))

(defun runtime-observer-unregister (event name)
  (let ((key (string-downcase (string event)))
        (id (string-downcase (string name))))
    (bt:with-lock-held (*runtime-observer-lock*)
      (setf (gethash key *runtime-observers*)
            (remove id (gethash key *runtime-observers*) :test #'string=
                    :key (lambda (row) (gethash "name" row)))))
    t))

(defun runtime-observer-emit (event payload)
  "Notify a stable snapshot of observers. An observer failure is isolated."
  (let* ((key (string-downcase (string event)))
         (rows (bt:with-lock-held (*runtime-observer-lock*)
                 (copy-list (gethash key *runtime-observers*))))
         (results nil))
    (dolist (row rows (nreverse results))
      (let ((name (gethash "name" row)))
        (handler-case
            (progn (funcall (gethash "function" row) payload)
                   (push (obj "name" name "status" "observed") results))
          (error (condition)
            (push (obj "name" name "status" "observer-error"
                       "error_class"
                       (string-downcase (symbol-name (type-of condition))))
                  results)
            (when (fboundp 'log-event)
              (ignore-errors
                (log-event "runtime-observer-error"
                           (obj "event" key "observer" name
                                "error_class"
                                (string-downcase
                                 (symbol-name (type-of condition)))))))))))))

(defun runtime-observer-report ()
  (let ((events (obj)) (count 0))
    (bt:with-lock-held (*runtime-observer-lock*)
      (maphash
       (lambda (event rows)
         (incf count (length rows))
         (setf (gethash event events)
               (coerce (mapcar (lambda (row)
                                 (obj "name" (gethash "name" row)
                                      "capability" "observe"
                                      "required" (gethash "required" row)))
                               rows)
                       'vector)))
       *runtime-observers*))
    (obj "schema_version" 1 "observer_count" count "events" events)))

(defun runtime-authority-declare (decision-class owner &key required)
  "Declare metadata for an existing effective authority; grant no capability."
  (let ((class (string-downcase (string decision-class)))
        (name (string-downcase (string owner))))
    (bt:with-lock-held (*runtime-observer-lock*)
      (when required (setf (gethash class *runtime-authority-required*) t))
      (pushnew name (gethash class *runtime-authorities*) :test #'string=))
    name))

(defun runtime-authority-report ()
  (let ((classes nil) (authority-count 0))
    (bt:with-lock-held (*runtime-observer-lock*)
      (maphash
       (lambda (class owners)
         (incf authority-count (length owners))
         (push (obj "decision_class" class
                    "required" (if (gethash class *runtime-authority-required*) t nil)
                    "authority_count" (length owners)
                    "effective_authorities" (coerce (reverse owners) 'vector))
               classes))
       *runtime-authorities*)
      ;; Required classes remain visible even when deliberately miswired.
      (maphash
       (lambda (class ignored)
         (declare (ignore ignored))
         (unless (gethash class *runtime-authorities*)
           (push (obj "decision_class" class "required" t
                      "authority_count" 0 "effective_authorities" (vector))
                 classes)))
       *runtime-authority-required*))
    (obj "schema_version" 1 "declarative_only" t
         "grants_capability" nil "authority_count" authority-count
         "decision_classes" (coerce (sort classes #'string<
                                            :key (lambda (row)
                                                   (gethash "decision_class" row)))
                                    'vector))))

(defun runtime-authority-assert ()
  "Fail unless every required decision class has exactly one declared owner."
  (bt:with-lock-held (*runtime-observer-lock*)
    (maphash
     (lambda (class ignored)
       (declare (ignore ignored))
       (let ((count (length (gethash class *runtime-authorities*))))
         (unless (= count 1)
           (error "Decision class ~a has ~d effective authorities" class count))))
     *runtime-authority-required*))
  t)

(defun runtime-observer-assert ()
  "Reject duplicate names and missing required callables; return T otherwise."
  (let ((ok t))
    (bt:with-lock-held (*runtime-observer-lock*)
      (maphash
       (lambda (event rows)
         (declare (ignore event))
         (let ((seen (make-hash-table :test #'equal)))
           (dolist (row rows)
             (let ((name (gethash "name" row))
                   (fn (gethash "function" row)))
               (when (gethash name seen) (setf ok nil))
               (setf (gethash name seen) t)
               (when (and (gethash "required" row)
                          (not (or (functionp fn)
                                   (and (symbolp fn) (fboundp fn)))))
                 (setf ok nil))))))
       *runtime-observers*)
      (maphash
       (lambda (required-key ignored)
         (declare (ignore ignored))
         (let* ((slash (position #\/ required-key))
                (event (subseq required-key 0 slash))
                (name (subseq required-key (1+ slash))))
           (unless (find name (gethash event *runtime-observers*)
                         :test #'string=
                         :key (lambda (row) (gethash "name" row)))
             (setf ok nil))))
       *runtime-observer-required*))
    ok))

(defun runtime-observer-reset ()
  "Test/operator helper. Removes declarations but never changes runtime behavior."
  (bt:with-lock-held (*runtime-observer-lock*)
    (clrhash *runtime-observers*)
    (clrhash *runtime-observer-required*)
    (clrhash *runtime-authorities*)
    (clrhash *runtime-authority-required*))
  t)
