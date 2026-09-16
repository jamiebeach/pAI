;;;; runtime-observer-audit.lisp -- required content-free A2 lifecycle consumer.

(in-package :agent)

(export '(runtime-observer-audit-report runtime-observer-audit-assert
          runtime-observer-audit-register-all runtime-observer-audit-reset))

(defparameter *runtime-observer-audit-specs*
  '(("memory-write" "memory-committed" t "observe")
    ("explore-stance-committed" "explore-stance-committed" t "observe")
    ("artifact-validated" "project-artifact-validated" t "observe")
    ("artifact-completed" "project-artifact-completed" t "observe")
    ("turn-capture-complete" "public-turn-completed" t "observe")
    ("user-message" "user-reply-observed" t "observe")
    ("tool-turn-committed" "tool-turn-committed" t "observe")
    ("initiative-decision" "initiative-decision-made" t "observe")
    ("public-outbound-evaluated" "outbound-evaluated" t "observe")
    ("public-outbound-completed" "outbound-completed" t "observe"))
  "Actual event, logical lifecycle class, required flag, rollout capability.")

(defvar *runtime-observer-audit-state* (make-hash-table :test #'equal))
(defvar *runtime-observer-audit-lock*
  (bt:make-lock "runtime-observer-audit"))

(defun %runtime-observer-audit-name (logical)
  (format nil "runtime-audit-~a" logical))

(defun %runtime-observer-audit-source-id (payload)
  (or (and (hash-table-p payload) (gethash "id" payload))
      (let ((envelope (and (hash-table-p payload)
                           (gethash "envelope" payload))))
        (and (hash-table-p envelope) (gethash "id" envelope)))
      (and (hash-table-p payload) (gethash "canonical_public_act_id" payload))))

(defun %runtime-observer-audit-consume (actual logical payload)
  "Consume only bounded identifiers and counts; never retain PAYLOAD content."
  (unless (hash-table-p payload)
    (error "A2 observer ~a received a non-object payload" logical))
  (let ((source-id (%runtime-observer-audit-source-id payload)))
    (unless source-id
      (error "A2 observer ~a received no durable source identifier" logical))
    (bt:with-lock-held (*runtime-observer-audit-lock*)
      (let ((row (or (gethash logical *runtime-observer-audit-state*)
                     (setf (gethash logical *runtime-observer-audit-state*)
                           (obj "actual_event" actual "logical_class" logical
                                "consumed" 0 "last_source_id" :null)))))
        (incf (gethash "consumed" row))
        (setf (gethash "last_source_id" row) (format nil "~a" source-id))))
    t))

(defun runtime-observer-audit-register-all ()
  (dolist (spec *runtime-observer-audit-specs*)
    (destructuring-bind (actual logical required rollout) spec
      (declare (ignore rollout))
      (let ((actual-copy actual) (logical-copy logical))
        (runtime-observer-register
         actual-copy (%runtime-observer-audit-name logical-copy)
         (lambda (payload)
           (%runtime-observer-audit-consume actual-copy logical-copy payload))
         :capability :observe :required required))))
  t)

(defun runtime-observer-audit-assert ()
  "Fail unless every declared required lifecycle consumer is registered once."
  (unless (runtime-observer-assert)
    (error "Base runtime observer registry assertion failed"))
  (dolist (spec *runtime-observer-audit-specs* t)
    (destructuring-bind (actual logical required rollout) spec
      (declare (ignore rollout))
      (when required
        (let* ((name (%runtime-observer-audit-name logical))
               (rows (gethash actual *runtime-observers*))
               (matches (count name rows :test #'string=
                               :key (lambda (row) (gethash "name" row)))))
          (unless (= matches 1)
            (error "Required A2 observer ~a/~a count is ~d" actual name matches)))))))

(defun runtime-observer-audit-report ()
  (let ((coverage nil) (total 0))
    (bt:with-lock-held (*runtime-observer-audit-lock*)
      (dolist (spec *runtime-observer-audit-specs*)
        (destructuring-bind (actual logical required rollout) spec
          (let* ((state (gethash logical *runtime-observer-audit-state*))
                 (consumed (if state (gethash "consumed" state 0) 0)))
            (incf total consumed)
            (push (obj "actual_event" actual "logical_class" logical
                       "observer" (%runtime-observer-audit-name logical)
                       "required" (if required t nil)
                       "capability" rollout
                       "consumed" consumed
                       "last_source_id"
                       (if state (gethash "last_source_id" state) :null))
                  coverage)))))
    (obj "schema_version" 1 "consumer" "runtime-observer-audit"
         "capability" "observe" "retains_payload_content" nil
         "required_classes" (length *runtime-observer-audit-specs*)
         "consumed" total "coverage" (coerce (nreverse coverage) 'vector))))

(defun runtime-observer-audit-reset ()
  "Test helper: clear bounded counters without changing registrations."
  (bt:with-lock-held (*runtime-observer-audit-lock*)
    (clrhash *runtime-observer-audit-state*))
  t)

(define-init :install observer-registry
    "Register all required lifecycle observers."
  (runtime-observer-audit-register-all))
(define-init :verify observer-registry-assert
    "Fail closed if a required observer is missing or miswired."
  (runtime-observer-audit-assert))
