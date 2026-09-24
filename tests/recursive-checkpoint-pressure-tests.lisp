;;;; harness: full-system
(in-package :agent)
(let ((checks 0))
  (flet ((check (value) (unless value (error "Checkpoint pressure regression")) (incf checks)))
    (check (%recursive-checkpoint-within-budget-p (obj "a" #(1 "ok" :null)) 256))
    (check (not (%recursive-checkpoint-within-budget-p (make-string 1000) 256)))
    (check (not (%recursive-checkpoint-within-budget-p (obj "a" (vector (make-string 1000))) 256)))
    (let ((*recursive-checkpoint-maximum-json-characters* 100)
          (*recursive-checkpoint-deferred-head* nil)
          (*conscious-recursive-thread-events-maintenance-replay-p* nil)
          (*conscious-recursive-thread-events-checkpoint-head* 10)
          (*conscious-recursive-thread-events-checkpoint-due-p* t)
          (old (symbol-function 'event-authority-checkpoint-publish)))
      (unwind-protect
           (progn
             (setf (symbol-function 'event-authority-checkpoint-publish)
                   (lambda (&rest args) (declare (ignore args)) (error "Oversized state reached serialization")))
             (handler-bind ((warning #'muffle-warning))
               (check (null (%recursive-thread-events-checkpoint-publish (list (make-string 1000)) 50 50))))
             (check (= 10 *conscious-recursive-thread-events-checkpoint-head*))
             (check (= 50 *recursive-checkpoint-deferred-head*))
             (check (null *conscious-recursive-thread-events-checkpoint-due-p*))
             (check (null (%recursive-thread-events-checkpoint-publish nil 51 51))))
        (setf (symbol-function 'event-authority-checkpoint-publish) old))))
  (format t "Recursive checkpoint pressure: ~d passed, 0 failed~%" checks))

;; A real SQLite authority must refuse ordinary replay of a ledger above the
;; configured threshold, accept explicit maintenance, then restore the sealed
;; projection after the in-memory generation has been discarded.
(uiop:call-with-temporary-file
 (lambda (path)
   (let* ((backend (make-sqlite-storage path))
          (*event-authority-port* nil) (*event-ring* nil) (*event-next-id* 0)
          (*runtime-observers* (make-hash-table :test #'equal))
          (*sqlite-event-authority-backend* nil)
          (*sqlite-event-authority-agent-id* nil)
          (*sqlite-event-authority-checkpoint-backend* nil)
          (*sqlite-event-authority-database* nil)
          (*sqlite-event-authority-derived-database* nil)
          (*conscious-recursive-thread-events-cache* nil)
          (*conscious-recursive-thread-events-cache-key* nil)
          (*conscious-recursive-thread-events-cache-head* nil)
          (*conscious-recursive-thread-events-cache-max-id* nil)
          (*conscious-recursive-thread-events-checkpoint-head* nil)
          (*conscious-recursive-thread-events-full-replay-max-head* 2)
          (*conscious-recursive-thread-events-maintenance-replay-p* nil))
     (unwind-protect
          (progn
            (%sqlite-authority-install backend path backend path "checkpoint-fixture")
            (dotimes (n 3)
              (log-event "user-message" (obj "text" (format nil "synthetic-~d" n))))
            (assert (handler-case (progn (%recursive-thread-events) nil)
                      (error () t)))
            (let ((*conscious-recursive-thread-events-maintenance-replay-p* t))
              (assert (= 3 (length (%recursive-thread-events)))))
            (let ((checkpoint
                    (event-authority-checkpoint-load
                     *conscious-recursive-thread-events-checkpoint-name*)))
              (assert (hash-table-p checkpoint))
              (assert (equal *conscious-recursive-thread-events-projector-revision*
                             (gethash "projector_revision" checkpoint)))
              (assert (= 3 (gethash "through_storage_position" checkpoint))))
            (setf *conscious-recursive-thread-events-cache* nil
                  *conscious-recursive-thread-events-cache-key* nil
                  *conscious-recursive-thread-events-cache-head* nil
                  *conscious-recursive-thread-events-cache-max-id* nil)
            (assert (= 3 (length (%recursive-thread-events))))
            (log-event "user-message" (obj "text" "synthetic-tail"))
            (assert (= 4 (length (%recursive-thread-events))))
            (setf *conscious-recursive-thread-events-cache* nil
                  *conscious-recursive-thread-events-cache-key* nil
                  *conscious-recursive-thread-events-cache-head* nil
                  *conscious-recursive-thread-events-cache-max-id* nil)
            (assert (= 4 (length (%recursive-thread-events))))
            (format t "PASS SQLite replay gate, explicit rebuild, sealed restore, and bounded tail~%"))
       (event-authority-clear))))
 :want-stream-p nil :type "sqlite")
