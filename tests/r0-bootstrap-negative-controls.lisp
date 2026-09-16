(defpackage :agent (:use :cl))
(in-package :agent)

(ql:quickload '(:bordeaux-threads :shasht :ironclad :uiop) :silent t)

(defun r0-negative-object (&rest pairs)
  (loop with result = (make-hash-table :test #'equal)
        for (key value) on pairs by #'cddr
        do (setf (gethash key result) value)
        finally (return result)))

(setf (fdefinition 'obj) #'r0-negative-object
      (fdefinition 'auto-turn) (lambda (prompt) prompt)
      (fdefinition 'execute) (lambda (&rest values) (declare (ignore values)) nil)
      (fdefinition 'propose-loop) (lambda (&rest values) (declare (ignore values)) nil))
(defparameter *tools* (vector))

(load (test-source "event-log.lisp"))
(load (test-source "projection-rebuild.lisp"))
(load (merge-pathnames "r0-bootstrap-checkpoint-contract.lisp" *load-truename*))

(defun r0-negative-clone (value)
  (cond
    ((hash-table-p value)
     (let ((copy (make-hash-table :test #'equal)))
       (maphash (lambda (key item) (setf (gethash key copy) (r0-negative-clone item))) value)
       copy))
    ((stringp value) value)
    ((vectorp value) (map 'vector #'r0-negative-clone value))
    ((consp value) (mapcar #'r0-negative-clone value))
    (t value)))

(let* ((root #P"/scratch/negative-controls/")
       (expected-failures 0)
       (unexpected-passes 0)
       (unexpected-names '())
       (spec (first (projection-rebuild-postgres-table-specs)))
       (raw "{\"rollout_id\":\"r1\",\"max_cost_credits\":0.001}")
       (row (shasht:read-json raw))
       (good (obj "id" 101 "type" "postgres-row-state"
                  "payload" (obj "projection" "postgres"
                                 "table" (getf spec :table)
                                 "operation" "upsert"
                                 "primary_key" (obj "rollout_id" "r1")
                                 "row" row "row_json" raw))))
  (when (probe-file root)
    (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))
  (ensure-directories-exist (merge-pathnames "placeholder" root))
  (labels ((rejects (name thunk &optional path)
             (handler-case
                 (progn (funcall thunk) (incf unexpected-passes)
                        (push name unexpected-names))
               (error ()
                 (if (and path (probe-file path))
                     (progn (incf unexpected-passes)
                            (push name unexpected-names))
                     (incf expected-failures)))))
           (path (name) (merge-pathnames (format nil "~a.sql" name) root))
           (write-one (event output &optional (tail 101) (schema "r0e5_negative"))
             (projection-rebuild-write-exact-postgres-tail
              (list event) output tail :destination-schema schema)))
    (let ((event (r0-negative-clone good)) (output (path "missing")))
      (remhash "row_json" (gethash "payload" event))
      (rejects "missing" (lambda () (write-one event output)) output))
    (let ((event (r0-negative-clone good)) (output (path "malformed")))
      (setf (gethash "row_json" (gethash "payload" event)) "{broken")
      (rejects "malformed" (lambda () (write-one event output)) output))
    (let ((event (r0-negative-clone good)) (output (path "tampered")))
      (setf (gethash "max_cost_credits" (gethash "row" (gethash "payload" event))) 2)
      (rejects "tampered" (lambda () (write-one event output)) output))
    (let ((event (r0-negative-clone good)) (output (path "key")))
      (setf (gethash "rollout_id" (gethash "primary_key" (gethash "payload" event))) "other")
      (rejects "key" (lambda () (write-one event output)) output))
    (let ((event (r0-negative-clone good)) (output (path "unregistered")))
      (setf (gethash "table" (gethash "payload" event)) "unknown_table")
      (rejects "unregistered" (lambda () (write-one event output)) output))
    (let ((output (path "order")))
      (rejects "order"
               (lambda ()
                 (projection-rebuild-write-exact-postgres-tail
                  (list good good) output 101 :destination-schema "r0e5_negative"))
               output))
    (let ((output (path "incomplete"))
          (source (lambda (visitor) (funcall visitor good) (values nil 101 1))))
      (rejects "incomplete"
               (lambda ()
                 (projection-rebuild-write-exact-postgres-tail
                  source output 101 :destination-schema "r0e5_negative"))
               output))
    (let ((output (path "boundary")))
      (rejects "boundary" (lambda () (write-one good output 102)) output))
    (let ((output (path "public")))
      (rejects "public" (lambda () (write-one good output 101 "public")) output))
    (rejects "truncated-manifest"
             (lambda ()
               (r0-bootstrap-parse-expected-row-count-lines
                (list (format nil "memory_nodes~c1" #\Tab))
                '("memory_nodes" "memory_edges"))))
    (let ((final-spools (length (directory (merge-pathnames "*.sql" root)))))
      (unless (and (= expected-failures 10) (zerop unexpected-passes)
                   (zerop final-spools))
        (error "R0e5 negative-control contract failed: expected=~d unexpected=~d names=~s spools=~d"
               expected-failures unexpected-passes
               (nreverse unexpected-names) final-spools))
      (format t
              "R0_BOOTSTRAP_NEGATIVE_CONTROLS_PASS expected_failures=~d unexpected_passes=~d final_spools=~d~%"
              expected-failures unexpected-passes final-spools))))
