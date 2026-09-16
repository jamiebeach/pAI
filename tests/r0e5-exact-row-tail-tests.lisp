(defpackage :agent (:use :cl))
(in-package :agent)

(ql:quickload '(:uiop :shasht :ironclad :bordeaux-threads) :silent t)

(defvar *r0e5-pass* 0)
(defvar *r0e5-fail* 0)

(defun r0e5-check (name condition)
  (if condition
      (progn (incf *r0e5-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *r0e5-fail*) (format t "  FAIL ~a~%" name))))

(defun r0e5-obj (&rest pairs)
  (loop with result = (make-hash-table :test #'equal)
        for (key value) on pairs by #'cddr
        do (setf (gethash key result) value)
        finally (return result)))

(defun r0e5-read (json) (shasht:read-json json))

(defun r0e5-clone (value)
  (cond
    ((hash-table-p value)
     (let ((copy (make-hash-table :test #'equal)))
       (maphash (lambda (key item)
                  (setf (gethash key copy) (r0e5-clone item)))
                value)
       copy))
    ((stringp value) value)
    ((vectorp value) (map 'vector #'r0e5-clone value))
    ((consp value) (mapcar #'r0e5-clone value))
    (t value)))

(defun r0e5-key (spec row)
  (let ((key (make-hash-table :test #'equal)))
    (dolist (field (getf spec :keys) key)
      (setf (gethash field key) (gethash field row)))))

(defun r0e5-postgres-event (id spec row-json)
  (let ((row (r0e5-read row-json)))
    (r0e5-obj
     "id" id "type" "postgres-row-state"
     "payload" (r0e5-obj
                 "projection" "postgres" "table" (getf spec :table)
                 "operation" "upsert" "primary_key" (r0e5-key spec row)
                 "row" row "row_json" row-json))))

(defun r0e5-memory-event (id type operation row-json)
  (r0e5-obj "id" id "type" type
             "payload" (r0e5-obj
                         "operation" operation "mutation_kind" "fixture"
                         "row" (r0e5-read row-json) "row_json" row-json)))

(defun r0e5-error-text (thunk)
  (handler-case (progn (funcall thunk) nil)
    (error (condition) (princ-to-string condition))))

(defun r0e5-file-text (pathname)
  (with-open-file (in pathname :direction :input :external-format :utf-8)
    (let ((text (make-string (file-length in))))
      (read-sequence text in)
      text)))

(setf (fdefinition 'auto-turn) (lambda (prompt) prompt)
      (fdefinition 'execute) (lambda (&rest arguments)
                               (declare (ignore arguments)) nil)
      (fdefinition 'propose-loop) (lambda (&rest arguments)
                                    (declare (ignore arguments)) nil)
      (fdefinition 'obj) #'r0e5-obj)
(defparameter *tools* (vector))

(load (test-source "event-log.lisp"))
(load (test-source "projection-rebuild.lisp"))

(format t "~%== emitter opaque-string contract ==~%")
(let* ((raw "{\"rollout_id\":\"r1\",\"max_cost_credits\":0.001,\"label\":\"the agent’s pool book\"}")
       (row (r0e5-read raw))
       (key (r0e5-obj "rollout_id" "r1"))
       (captured nil)
       (saved (fdefinition 'log-event)))
  (unwind-protect
       (progn
         (setf (fdefinition 'log-event)
               (lambda (type payload &key caused-by)
                 (declare (ignore caused-by))
                 (setf captured (list type payload))
                 1))
         (log-postgres-row-state
          "memory_atom_rollouts" "upsert" key row raw))
    (setf (fdefinition 'log-event) saved))
  (let* ((payload (second captured))
         (encoded (shasht:write-json payload nil))
         (decoded (shasht:read-json encoded)))
    (r0e5-check "generic PostgreSQL logger retains raw string"
                (and (string= "postgres-row-state" (first captured))
                     (string= raw (gethash "row_json" payload))))
    (r0e5-check "enclosing event JSON roundtrip preserves raw characters"
                (string= raw (gethash "row_json" decoded)))))

(let* ((root #P"/tmp/r0e5-exact-row-tail/")
       (valid-path (merge-pathnames "valid.sql" root))
       (specs (projection-rebuild-postgres-table-specs))
       (events nil)
       (id 0))
  (when (probe-file root)
    (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))
  (ensure-directories-exist valid-path)

  ;; Exercise every R0c5 registry shape with spelling-sensitive row text.
  (dolist (spec specs)
    (let ((fields nil))
      (dolist (field (getf spec :keys))
        (push (format nil "\"~a\":~s" field
                      (format nil "~a-key" field)) fields))
      (push (r0e5-postgres-event
             (incf id) spec
             (format nil "{~{~a~^,~},\"max_cost_credits\":0.001,\"label\":\"the agent’s pool book\"}"
                     (nreverse fields)))
            events)))
  (push (r0e5-memory-event
         (incf id) "memory-node-state" "update"
         "{\"id\":\"node-1\",\"activation\":0.001,\"content\":\"quote: ' and slash: \\\\\"}")
        events)
  (push (r0e5-memory-event
         (incf id) "memory-edge-state" "insert"
         "{\"id\":91,\"from_id\":\"node-1\",\"to_id\":\"node-2\",\"edge_type\":\"supports\"}")
        events)
  (push (r0e5-memory-event
         (incf id) "memory-edge-state" "delete"
         "{\"id\":91,\"from_id\":\"node-1\",\"to_id\":\"node-2\",\"edge_type\":\"supports\"}")
        events)
  (setf events (nreverse events))

  (format t "~%== exact constant-retention tail ==~%")
  (multiple-value-bind (path count terminal)
      (projection-rebuild-write-exact-postgres-tail
       events valid-path id :destination-schema "r0e5_fixture")
    (let ((sql (r0e5-file-text path)))
      (r0e5-check "all registered table and memory mutations stream"
                  (and (= count (+ (length specs) 3)) (= terminal id)))
      (r0e5-check "decimal spelling survives without float reserialization"
                  (and (search "0.001" sql)
                       (null (search "9.9999994e-4" sql))))
      (r0e5-check "opaque Unicode row text survives in SQL spool"
                  (search "the agent’s pool book" sql))
      (r0e5-check "SQL literal escaping does not alter row JSON semantics"
                  (search "quote: '' and slash:" sql))
      (r0e5-check "edge delete emits deletion without a following insert"
                  (= (+ (* 2 (+ (length specs) 2)) 1)
                     (count #\Newline sql)))))

  (format t "~%== fail-closed exactness gates ==~%")
  (let* ((raw (r0e5-read "{\"max_cost_credits\":0.001}"))
         (narrowed (r0e5-read "{\"max_cost_credits\":9.9999994e-4}"))
         (changed (r0e5-read "{\"max_cost_credits\":0.0011}")))
    (r0e5-check "envelope float narrowing remains compatible with opaque row"
                (%projection-rebuild-exact-row-compatible-p raw narrowed))
    (r0e5-check "meaningful numeric row change remains incompatible"
                (not (%projection-rebuild-exact-row-compatible-p raw changed))))
  (labels ((fresh-path (name) (merge-pathnames name root))
           (refusal (name event expected &optional (tail (gethash "id" event)))
             (let* ((path (fresh-path (format nil "~a.sql" name)))
                    (message
                      (r0e5-error-text
                       (lambda ()
                         (projection-rebuild-write-exact-postgres-tail
                          (list event) path tail
                          :destination-schema "r0e5_fixture")))))
               (r0e5-check name
                           (and message (search expected message)
                                (not (probe-file path)))))))
    (let* ((spec (first specs))
           (good (r0e5-postgres-event
                  101 spec "{\"rollout_id\":\"r1\",\"max_cost_credits\":0.001}")))
      (let ((event (r0e5-clone good)))
        (remhash "row_json" (gethash "payload" event))
        (refusal "missing raw row is named" event "missing-exact-row-json"))
      (let ((event (r0e5-clone good)))
        (setf (gethash "row_json" (gethash "payload" event)) "{broken")
        (refusal "malformed raw row is named" event "malformed-exact-row-json"))
      (let ((event (r0e5-clone good)))
        (setf (gethash "max_cost_credits"
                       (gethash "row" (gethash "payload" event))) 2)
        (refusal "parsed and opaque row mismatch is named"
                 event "exact-row-mismatch"))
      (let ((event (r0e5-clone good)))
        (setf (gethash "rollout_id"
                       (gethash "primary_key" (gethash "payload" event))) "other")
        (refusal "declared primary key mismatch is named"
                 event "primary-key-mismatch"))
      (let ((event (r0e5-clone good)))
        (setf (gethash "table" (gethash "payload" event)) "unknown_table")
        (refusal "unregistered table is named" event "unregistered-table"))
      (let* ((path (fresh-path "order.sql"))
             (message
               (r0e5-error-text
                (lambda ()
                  (projection-rebuild-write-exact-postgres-tail
                   (list good good) path 101
                   :destination-schema "r0e5_fixture")))))
        (r0e5-check "non-increasing event IDs refuse atomic publication"
                    (and (search "non-increasing-event-id" message)
                         (not (probe-file path)))))
      (let* ((path (fresh-path "incomplete.sql"))
             (source (lambda (visitor)
                       (funcall visitor good)
                       (values nil 101 1)))
             (message
               (r0e5-error-text
                (lambda ()
                  (projection-rebuild-write-exact-postgres-tail
                   source path 101 :destination-schema "r0e5_fixture")))))
        (r0e5-check "incomplete scanner refuses atomic publication"
                    (and (search "incomplete-scan" message)
                         (not (probe-file path)))))
      (refusal "wrong terminal boundary is named" good
               "unexpected-last-event-id" 102)
      (let ((message
              (r0e5-error-text
               (lambda ()
                 (projection-rebuild-write-exact-postgres-tail
                  (list good) (fresh-path "public.sql") 101
                  :destination-schema "public")))))
        (r0e5-check "public destination is refused"
                    (and message (search "public schema" message))))))

  (format t "~%R0e5 exact-row tail: ~d passed, ~d failed~%"
          *r0e5-pass* *r0e5-fail*)
  (when (plusp *r0e5-fail*) (uiop:quit 1)))
