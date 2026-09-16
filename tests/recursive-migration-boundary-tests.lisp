;;;; recursive-migration-boundary-tests.lisp -- migrated dialogue recovery split.
;;;; harness: full-system

(in-package :agent)

(defvar *rmb-pass* 0)
(defvar *rmb-fail* 0)

(defun rmb-check (name condition)
  (if condition
      (progn (incf *rmb-pass*) (format t "PASS ~a~%" name))
      (progn (incf *rmb-fail*) (format t "FAIL ~a~%" name))))

(defun rmb-event (id type payload &optional caused-by)
  (obj "id" id "type" type "agent_id" "migration-boundary-fixture"
       "caused_by" (or caused-by :null) "payload" payload))

(let* ((map-symbol 'map-events)
       (map-original (symbol-function map-symbol))
       (calls nil)
       (historical-user
         (rmb-event 50 "historical-user-message-imported"
                    (obj "text" "historical operator")))
       (historical-agent
         (rmb-event 51 "historical-agent-message-imported"
                    (obj "text" "historical source assistant") 50))
       (live-request (rmb-event 101 "model-request" (obj "kind" "live"))))
  (unwind-protect
       (progn
         (setf (symbol-function map-symbol)
               (lambda (visitor &rest arguments)
                 (let ((types (getf arguments :types))
                       (after (getf arguments :after-position)))
                   (push (cons (copy-list types) after) calls)
                   (dolist (event
                            (if after
                                (list live-request)
                                (list historical-user historical-agent)))
                     (funcall visitor event))
                   (values t (or after 51) (if after 1 2)))))
         (let ((*conscious-recursive-recovery-start-storage-position* 100))
           (let ((events (%recursive-thread-events-full-replay)))
             (rmb-check
              "pre-boundary replay contains only provenance-closed dialogue"
              (equal '(50 51 101)
                     (mapcar (lambda (event) (gethash "id" event)) events)))
             (rmb-check
              "historical and destination recovery use separate authority reads"
              (and (= 2 (length calls))
                   (find-if
                    (lambda (call)
                      (and (null (cdr call))
                           (equal
                            *conscious-recursive-historical-dialogue-event-types*
                            (car call))))
                    calls)
                   (find-if
                    (lambda (call)
                      (and (eql 100 (cdr call))
                           (notany
                            (lambda (type)
                              (member type (car call) :test #'string=))
                            *conscious-recursive-historical-dialogue-event-types*)))
                    calls))))))
    (setf (symbol-function map-symbol) map-original)))

(let* ((storage-map-symbol 'storage-map-events)
       (storage-map-original (symbol-function storage-map-symbol))
       (types (loop for index below 40
                    collect (format nil "migration-type-~d" index)))
       (call nil)
       (visited nil))
  (unwind-protect
       (progn
         (setf (symbol-function storage-map-symbol)
               (lambda (backend visitor &key agent-id after-position
                                           through-position event-types
                                           &allow-other-keys)
                 (declare (ignore backend))
                 (setf call (list agent-id after-position through-position
                                  event-types))
                 (let ((event (rmb-event 105 "migration-type-1" (obj))))
                   (funcall visitor event 105))
                 (values t 105 1 64)))
         (multiple-value-bind (complete last-id count)
             (%sqlite-authority-map
              nil "migration-boundary-fixture"
              (lambda (event) (push (gethash "id" event) visited))
              :after-position 100 :types types)
           (rmb-check
            "SQLite authority streams wide vocabulary after physical boundary"
            (and complete (= 105 last-id) (= 1 count)
                 (equal '(105) visited)
                 (string= "migration-boundary-fixture" (first call))
                 (= 100 (second call))
                 (null (third call))
                 (equal types (fourth call))))))
    (setf (symbol-function storage-map-symbol) storage-map-original)))

(format t "~%~d passed, ~d failed~%" *rmb-pass* *rmb-fail*)
(when (plusp *rmb-fail*)
  (error "recursive migration boundary tests failed"))
