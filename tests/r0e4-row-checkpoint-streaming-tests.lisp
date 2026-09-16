(defpackage :agent (:use :cl))
(in-package :agent)

(ql:quickload '(:bordeaux-threads :shasht :ironclad :uiop) :silent t)

(defvar *r0e4-row-pass* 0)
(defvar *r0e4-row-fail* 0)

(defun r0e4-row-check (name condition)
  (if condition
      (progn (incf *r0e4-row-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *r0e4-row-fail*) (format t "  FAIL ~a~%" name))))

(defun r0e4-row-object (&rest pairs)
  (loop with result = (make-hash-table :test #'equal)
        for (key value) on pairs by #'cddr
        do (setf (gethash key result) value)
        finally (return result)))

(defun r0e4-row-json (value)
  (let ((*print-pretty* nil)) (shasht:write-json value nil)))

(setf (fdefinition 'obj) #'r0e4-row-object
      (fdefinition 'auto-turn) (lambda (prompt) prompt)
      (fdefinition 'execute) (lambda (&rest arguments)
                               (declare (ignore arguments)) nil)
      (fdefinition 'propose-loop) (lambda (&rest arguments)
                                    (declare (ignore arguments)) nil))
(defparameter *tools* (vector))

(load (test-source "event-log.lisp"))
(load (test-source "projection-rebuild.lisp"))

(let* ((root #P"/tmp/r0e4-row-checkpoints/")
       (*event-checkpoint-directory* root)
       (rows (vector
              (r0e4-row-object "id" "a" "text" "pancakes")
              (r0e4-row-object "id" "b" "text" "the agent's café")
              (r0e4-row-object "id" "c" "nested"
                               (r0e4-row-object "future" 3))))
       (operations nil)
       (*event-storage-open-observer*
         (lambda (operation pathname)
           (declare (ignore pathname)) (push operation operations))))
  (when (probe-file root)
    (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))

  (format t "~%== incrementally hashed JSONL checkpoint ==~%")
  (let ((manifest (write-event-row-checkpoint "memory-nodes" 10 rows)))
    (r0e4-row-check "manifest freezes streaming format, bytes, and row count"
                    (and (= 2 (gethash "schema_version" manifest))
                         (= 3 (gethash "row_count" manifest))
                         (plusp (gethash "byte_length" manifest))
                         (= 64 (length (gethash "sha256" manifest))))))
  (let ((visited nil))
    (setf operations nil)
    (multiple-value-bind (complete manifest count)
        (map-verified-event-row-checkpoint
         "memory-nodes" (lambda (row) (push (r0e4-row-json row) visited)))
      (setf visited (nreverse visited) operations (nreverse operations))
      (r0e4-row-check "verified rows stream in deterministic source order"
                      (and complete (= 10 (gethash "event_id" manifest))
                           (= 3 count)
                           (equal (map 'list #'r0e4-row-json rows) visited)))
      (r0e4-row-check "hash and syntax validation finish before delivery"
                      (< (position :validate operations)
                         (position :deliver operations)))))

  (format t "~%== fail closed and verified fallback ==~%")
  (write-event-row-checkpoint
   "memory-nodes" 5 (vector (r0e4-row-object "id" "older")))
  ;; Corrupt the newer data without changing its published manifest.
  (with-open-file (out (%event-checkpoint-path
                        (%event-checkpoint-data-name "memory-nodes" 10))
                       :direction :output :if-exists :supersede
                       :external-format :utf-8)
    (write-line "{corrupt" out))
  (let ((ids nil))
    (multiple-value-bind (complete manifest count)
        (map-verified-event-row-checkpoint
         "memory-nodes" (lambda (row) (push (gethash "id" row) ids)))
      (r0e4-row-check "corrupt newest checkpoint falls back before delivery"
                      (and complete (= 5 (gethash "event_id" manifest))
                           (= 1 count) (equal '("older") ids)))))

  (let ((published nil))
    (r0e4-row-check
     "an incomplete source can never publish a manifest"
     (handler-case
         (progn
           (write-event-row-checkpoint
            "memory-edges" 20
            (lambda (visitor)
              (funcall visitor (r0e4-row-object "id" 1))
              (values nil 1)))
           nil)
       (error ()
         (setf published
               (probe-file (%event-checkpoint-path
                            (%event-checkpoint-manifest-name
                             "memory-edges" 20))))
         (null published)))))

  (write-event-row-checkpoint "memory-edges" 15 rows)
  (multiple-value-bind (complete manifest count)
      (map-verified-event-row-checkpoint
       "memory-edges"
       (lambda (row)
         (declare (ignore row))
         (error "forced consumer rollback")))
    (r0e4-row-check "consumer failure reports an incomplete delivery"
                    (and (not complete) (= 15 (gethash "event_id" manifest))
                         (zerop count))))

  (let ((sql-path (merge-pathnames "memory-edges.sql" root)))
    (multiple-value-bind (path manifest count)
        (projection-rebuild-write-row-checkpoint-inserts
         "memory-edges" sql-path "memory_edges")
      (r0e4-row-check "verified checkpoint streams into an atomic SQL spool"
                      (and (equal path sql-path)
                           (= 15 (gethash "event_id" manifest))
                           (= 3 count)
                           (= 3 (count #\Newline
                                       (uiop:read-file-string sql-path)))))))
  (r0e4-row-check "streaming loader also rejects the public schema"
                  (handler-case
                      (progn
                        (projection-rebuild-write-row-checkpoint-inserts
                         "memory-edges" (merge-pathnames "public.sql" root)
                         "memory_edges" :destination-schema "public")
                        nil)
                    (error () t)))
  (r0e4-row-check "omitted relational checkpoint refusal names its projection"
                  (handler-case
                      (progn
                        (projection-rebuild-write-row-checkpoint-inserts
                         "omitted-table" (merge-pathnames "omitted.sql" root)
                         "memory_edges")
                        nil)
                    (error (condition)
                      (search "omitted-table" (princ-to-string condition)))))

  (let* ((input (merge-pathnames "precision-input.jsonl" root))
         (line "{\"id\":1,\"value\":0.12345678901234567}")
         (delivered nil))
    (with-open-file (out input :direction :output :if-exists :supersede
                               :if-does-not-exist :create)
      (write-line line out))
    (write-event-row-checkpoint-lines
     "precision" 25 (make-event-jsonl-line-source input))
    (multiple-value-bind (complete manifest count)
        (map-verified-event-row-checkpoint-lines
         "precision" (lambda (raw) (push raw delivered)))
      (r0e4-row-check "exact-line checkpoints preserve numeric spelling"
                      (and complete (= 25 (gethash "event_id" manifest))
                           (= 1 count) (equal (list line) delivered)))))

  (r0e4-row-check "unsafe projection names remain rejected"
                  (handler-case
                      (progn
                        (write-event-row-checkpoint
                         "../escape" 1 rows)
                        nil)
                    (error () t)))

  (when (probe-file root)
    (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))

(format t "~%R0e4 row checkpoint streaming: ~a passed, ~a failed.~%"
        *r0e4-row-pass* *r0e4-row-fail*)
(when (plusp *r0e4-row-fail*) (uiop:quit 1))
