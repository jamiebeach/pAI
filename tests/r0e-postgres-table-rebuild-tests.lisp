(defpackage :agent (:use :cl))
(in-package :agent)

(ql:quickload '(:uiop :shasht :ironclad) :silent t)

(defvar *r0e3-pass* 0)
(defvar *r0e3-fail* 0)

(defun r0e3-check (name condition)
  (if condition
      (progn (incf *r0e3-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *r0e3-fail*) (format t "  FAIL ~a~%" name))))

(defun r0e3-obj (&rest pairs)
  (loop with result = (make-hash-table :test #'equal)
        for (key value) on pairs by #'cddr
        do (setf (gethash key result) value)
        finally (return result)))

(defun r0e3-key-value (table field index)
  (cond
    ((string= field "version") index)
    ((and (string= table "memory_atom_jobs") (string= field "id"))
     (+ 1000 index))
    (t (format nil "~a-~a-~d" table field index))))

(defun r0e3-row (spec index status)
  (let ((row (r0e3-obj
              "status" status
              "future_column" (r0e3-obj "nested" status))))
    (dolist (field (getf spec :keys) row)
      (setf (gethash field row)
            (r0e3-key-value (getf spec :table) field index)))))

(defun r0e3-primary-key (spec row)
  (let ((key (make-hash-table :test #'equal)))
    (dolist (field (getf spec :keys) key)
      (setf (gethash field key) (gethash field row)))))

(defun r0e3-event (id spec row &optional (operation "upsert"))
  (r0e3-obj
   "schema_version" 2 "id" id "type" "postgres-row-state"
   "payload" (r0e3-obj
              "projection" "postgres" "table" (getf spec :table)
              "operation" operation
              "primary_key" (r0e3-primary-key spec row)
              "row" row)))

(defun r0e3-only-row (state)
  (loop for row being the hash-values of (gethash "rows" state)
        return row))

(load (test-source "projection-rebuild.lisp"))

(let* ((specs (projection-rebuild-postgres-table-specs))
       (baselines (make-hash-table :test #'equal))
       (events nil))
  (loop for spec in specs for index from 1
        for baseline-row = (r0e3-row spec index "baseline")
        for latest-row = (r0e3-row spec index "latest")
        for id from 401
        do (setf (gethash (getf spec :table) baselines)
                 (make-projection-rebuild-table-baseline
                  400 (getf spec :table) (list baseline-row)))
           (push (r0e3-event id spec latest-row) events))
  (setf events
        (append (nreverse events)
                (list (r0e3-obj "id" 413 "type" "unrelated"
                                "payload" (r0e3-obj)))))

  (format t "~%== twelve-table checkpoint plus tail ==~%")
  (let* ((result (projection-rebuild-fold-postgres-tables
                  events :baselines baselines :expected-tail-event-id 413))
         (states (gethash "states" result)))
    (r0e3-check "all twelve tables fold without gaps"
                (and (gethash "complete" result)
                     (= 12 (hash-table-count states))))
    (r0e3-check "every table contains its exact latest complete row"
                (loop for spec in specs
                      for row = (r0e3-only-row
                                 (gethash (getf spec :table) states))
                      always (and row (string= "latest"
                                              (gethash "status" row)))))
    (r0e3-check "schema-unknown nested columns survive generically"
                (loop for spec in specs
                      for row = (r0e3-only-row
                                 (gethash (getf spec :table) states))
                      always (string=
                              "latest"
                              (gethash "nested"
                                       (gethash "future_column" row)))))
    (r0e3-check "composite table identities remain typed ordered lists"
                (let* ((root-state
                         (gethash "memory_atom_candidate_roots" states))
                       (version-state
                         (gethash "agent_artifact_versions" states))
                       (root-key
                         (loop for key being the hash-keys of
                               (gethash "rows" root-state) return key))
                       (version-key
                         (loop for key being the hash-keys of
                               (gethash "rows" version-state) return key)))
                  (and (= 2 (length root-key))
                       (= 2 (length version-key))
                       (integerp (second version-key)))))
    (r0e3-check "fold does not mutate table checkpoint baselines"
                (loop for spec in specs
                      for row = (r0e3-only-row
                                 (gethash (getf spec :table) baselines))
                      always (string= "baseline" (gethash "status" row))))

    (let* ((path #P"/tmp/r0e3-materialize.sql")
           (map (projection-rebuild-postgres-table-map specs)))
      (projection-rebuild-write-postgres-inserts result path map)
      (let ((sql (uiop:read-file-string path)))
        (r0e3-check "complete folds emit deterministic typed-row inserts"
                    (and (search "INSERT INTO \"r0e4_rebuild\"." sql)
                         (search "json_populate_record(NULL::\"public\"." sql)
                         (= 12 (count #\Newline sql))))))
    (r0e3-check "SQL string literals double embedded apostrophes"
                (string= "'the agent''s row'"
                         (%projection-rebuild-sql-literal "the agent's row")))
    (r0e3-check "materializer rejects the public schema"
                (handler-case
                    (progn
                      (projection-rebuild-write-postgres-inserts
                       result #P"/tmp/r0e3-public.sql"
                       (projection-rebuild-postgres-table-map specs)
                       :destination-schema "public")
                      nil)
                  (error () t))))

  (format t "~%== generic table gap refusal ==~%")
  (let* ((spec (first specs))
         (trusted (r0e3-row spec 1 "trusted"))
         (bad-row (r0e3-row spec 99 "bad"))
         (bad-key-event (r0e3-event 401 spec bad-row))
         (payload (gethash "payload" bad-key-event)))
    ;; Retain the bad row while claiming the trusted key.
    (setf (gethash "primary_key" payload) (r0e3-primary-key spec trusted))
    (let* ((delete-event (r0e3-event 402 spec trusted "delete"))
           (result (projection-rebuild-fold-postgres-tables
                    (list bad-key-event delete-event) :baselines baselines
                    :expected-tail-event-id 402))
           (gaps (gethash "gaps" result))
           (row (r0e3-only-row
                 (gethash (getf spec :table) (gethash "states" result)))))
      (r0e3-check "primary-key/row mismatch is named by table"
                  (find (getf spec :table) gaps
                        :key (lambda (gap) (gethash "projection" gap))
                        :test #'string=))
      (r0e3-check "unsupported delete is rejected"
                  (= 2 (count "invalid-table-row-event" gaps
                              :key (lambda (gap) (gethash "reason" gap))
                              :test #'string=)))
      (r0e3-check "rejected table events cannot alter trusted state"
                  (string= "baseline" (gethash "status" row)))))

  (let* ((missing-table "publication_candidates")
         (partial (make-hash-table :test #'equal)))
    (maphash (lambda (table baseline)
               (unless (string= table missing-table)
                 (setf (gethash table partial) baseline)))
             baselines)
    (let ((result
            (projection-rebuild-fold-postgres-tables
             (list (r0e3-obj "id" 401 "type" "unrelated"
                             "payload" (r0e3-obj)))
             :baselines partial :expected-tail-event-id 401)))
      (r0e3-check "truncated family names the missing table"
                  (find missing-table (gethash "gaps" result)
                        :key (lambda (gap) (gethash "projection" gap))
                        :test #'string=))))

  (let* ((unknown-event
           (r0e3-obj
            "id" 401 "type" "postgres-row-state"
            "payload" (r0e3-obj
                       "projection" "postgres" "table" "future_table"
                       "operation" "upsert" "primary_key" (r0e3-obj "id" 1)
                       "row" (r0e3-obj "id" 1))))
         (result (projection-rebuild-fold-postgres-tables
                  (list unknown-event) :baselines baselines
                  :expected-tail-event-id 401)))
    (r0e3-check "future table events require an explicit registry extension"
                (find "postgres-table-registry" (gethash "gaps" result)
                      :key (lambda (gap) (gethash "projection" gap))
                      :test #'string=)))

  (let ((uneven (make-hash-table :test #'equal)))
    (maphash (lambda (table baseline) (setf (gethash table uneven) baseline))
             baselines)
    (let* ((spec (first specs))
           (table (getf spec :table)))
      (setf (gethash table uneven)
            (make-projection-rebuild-table-baseline
             399 table (list (r0e3-row spec 1 "older"))))
      (let ((result (projection-rebuild-fold-postgres-tables
                     nil :baselines uneven :expected-tail-event-id 400)))
        (r0e3-check "unequal table checkpoint boundaries fail closed"
                    (find "checkpoint-boundary" (gethash "gaps" result)
                          :key (lambda (gap) (gethash "projection" gap))
                          :test #'string=))))))

(format t "~%R0e3 PostgreSQL table rebuild: ~a passed, ~a failed.~%"
        *r0e3-pass* *r0e3-fail*)
(when (plusp *r0e3-fail*) (uiop:quit 1))
