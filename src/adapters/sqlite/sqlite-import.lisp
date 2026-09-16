;;;; sqlite-import.lisp -- explicit offline JSONL import and parity audit.

(in-package :agent)

(export '(sqlite-import-jsonl sqlite-audit-jsonl-import))

(defun %sqlite-import-sources (sources)
  (let ((items (cond ((or (stringp sources) (pathnamep sources)) (list sources))
                     ((vectorp sources) (coerce sources 'list))
                     ((listp sources) sources)
                     (t nil))))
    (unless items
      (error 'storage-error :operation :import
             :detail "sources must name one or more ordered JSONL files"))
    (mapcar (lambda (source)
              (let ((path (pathname source)))
                (unless (probe-file path)
                  (error 'storage-error :operation :import
                         :detail (format nil "source does not exist: ~a" path)))
                path))
            items)))

(defun %sqlite-import-digest ()
  (ironclad:make-digest :sha256))

(defun %sqlite-import-digest-line-hash (digest line)
  (ironclad:update-digest
   digest
   (sb-ext:string-to-octets (%storage-sha256 line) :external-format :utf-8)))

(defun %sqlite-import-finish-digest (digest)
  (string-downcase
   (ironclad:byte-array-to-hex-string (ironclad:produce-digest digest))))

(defun %sqlite-import-parse-event (line legacy-agent-id operation)
  (let ((event
          (handler-case (shasht:read-json line)
            (error (condition)
              (error 'storage-integrity-error :operation operation
                     :detail (format nil "malformed source JSON: ~a" condition))))))
    (unless (hash-table-p event)
      (error 'storage-integrity-error :operation operation
             :detail "source event must be a JSON object"))
    (let ((id (gethash "id" event))
          (type (gethash "type" event))
          (timestamp (gethash "timestamp" event))
          (source-agent-id (gethash "agent_id" event)))
      (unless (and (integerp id) (plusp id))
        (error 'storage-integrity-error :operation operation
               :detail "source event ID must be a positive integer"))
      (%storage-required-string type "source event type")
      (%storage-required-string timestamp "source event timestamp")
      (cond
        ((stringp source-agent-id)
         (%storage-required-string source-agent-id "source agent-id")
         (values event id source-agent-id "verified" type timestamp))
        ((and (stringp legacy-agent-id) (plusp (length legacy-agent-id)))
         (%storage-required-string legacy-agent-id "legacy-agent-id")
         (values event id legacy-agent-id "legacy-partition-assumed"
                 type timestamp))
        (t
         (error 'storage-conflict-error :operation operation
                :detail "source event has no trusted partition and no legacy-agent-id was supplied"))))))

(defun %sqlite-import-map-lines (sources function)
  (dolist (path sources)
    (with-open-file (input path :direction :input :external-format :utf-8)
      (loop for line = (read-line input nil nil)
            while line
            unless (zerop (length line))
              do (funcall function line path))))
  t)

(defun %sqlite-table-count (handle table operation)
  (%with-sqlite-statement
      (statement handle (format nil "SELECT COUNT(*) FROM ~a" table) operation)
    (%sqlite-step handle statement operation +sqlite-row+)
    (%sqlite-column-int64 statement 0)))

(defun %sqlite-import-insert-row
    (handle statement id agent-id partition-status type timestamp line operation)
  (%sqlite-bind-int64 handle statement 1 id operation)
  (%sqlite-bind-text handle statement 2 agent-id operation)
  (%sqlite-bind-text handle statement 3 partition-status operation)
  (%sqlite-bind-text handle statement 4 type operation)
  (%sqlite-bind-text handle statement 5 timestamp operation)
  (%sqlite-bind-text handle statement 6 line operation)
  (%sqlite-bind-text handle statement 7 (%storage-sha256 line) operation)
  (%sqlite-step handle statement operation +sqlite-done+)
  (%sqlite-check (%sqlite-reset-raw statement) handle operation)
  (%sqlite-check (%sqlite-clear-bindings-raw statement) handle operation))

(defun %sqlite-import-report
    (source-count event-count first-id last-id maximum-id forward-hole-count
     duplicate-count rewind-count digest verified-count assumed-count
     &optional (status "imported"))
  (%storage-object
   "schema_version" 1 "status" status "source_file_count" source-count
   "event_count" event-count "first_event_id" (or first-id :null)
   "last_event_id" (or last-id :null) "maximum_event_id" (or maximum-id :null)
   "forward_hole_count" forward-hole-count
   "duplicate_id_count" duplicate-count "rewind_count" rewind-count
   "row_hash_chain_sha256" digest
   "verified_partition_count" verified-count
   "assumed_partition_count" assumed-count))

(defun sqlite-import-jsonl
    (backend sources &key legacy-agent-id progress-fn)
  "Atomically import ordered JSONL files into an empty SQLite backend.
PROGRESS-FN is a test/operator observation seam invoked after each staged row;
an error from it aborts and rolls back the complete import."
  (unless (typep backend 'sqlite-storage)
    (error 'storage-error :operation :import :detail "backend must be SQLite"))
  (when progress-fn
    (unless (functionp progress-fn)
      (error 'storage-error :operation :import
             :detail "progress-fn must be a function or NIL")))
  (let ((paths (%sqlite-import-sources sources))
        (count 0) (first-id nil) (last-id nil) (maximum-id nil)
        (forward-holes 0) (duplicates 0) (rewinds 0)
        (seen (make-hash-table :test #'eql))
        (verified 0) (assumed 0) (digest (%sqlite-import-digest)))
    (bt:with-lock-held ((%sqlite-storage-lock backend))
      (%sqlite-in-transaction
       backend :import
       (lambda (handle)
         (unless (and (zerop (%sqlite-table-count handle "pai_events" :import))
                      (zerop (%sqlite-table-count
                              handle "pai_projection_checkpoints" :import)))
           (error 'storage-conflict-error :operation :import
                  :detail "destination must contain no events or checkpoints"))
         (%with-sqlite-statement
             (statement handle
                        "INSERT INTO pai_events(event_id,storage_origin,agent_id,partition_status,event_type,occurred_at,event_json,integrity_hash) VALUES(?1,'legacy-import',?2,?3,?4,?5,?6,?7)"
                        :import)
           (%sqlite-import-map-lines
            paths
            (lambda (line source)
              (declare (ignore source))
              (multiple-value-bind
                    (event id agent-id partition-status type timestamp)
                  (%sqlite-import-parse-event line legacy-agent-id :import)
                (declare (ignore event))
                (when last-id
                  (cond ((> id last-id)
                         (incf forward-holes (max 0 (1- (- id last-id)))))
                        ((< id last-id) (incf rewinds))))
                (when (gethash id seen) (incf duplicates))
                (setf (gethash id seen) t
                      maximum-id (if maximum-id (max maximum-id id) id))
                (%sqlite-import-insert-row
                 handle statement id agent-id partition-status type timestamp
                 line :import)
                (%sqlite-import-digest-line-hash digest line)
                (unless first-id (setf first-id id))
                (setf last-id id)
                (incf count)
                (if (string= partition-status "verified")
                    (incf verified) (incf assumed))
                (when progress-fn (funcall progress-fn count id)))))))))
    (%sqlite-import-report
     (length paths) count first-id last-id maximum-id forward-holes
     duplicates rewinds (%sqlite-import-finish-digest digest)
     verified assumed)))

(defun sqlite-audit-jsonl-import (backend sources &key legacy-agent-id)
  "Read-only exact ordered parity audit between JSONL and SQLite."
  (unless (typep backend 'sqlite-storage)
    (error 'storage-error :operation :audit :detail "backend must be SQLite"))
  (let ((paths (%sqlite-import-sources sources))
        (count 0) (first-id nil) (last-id nil) (maximum-id nil)
        (forward-holes 0) (duplicates 0) (rewinds 0)
        (seen (make-hash-table :test #'eql))
        (verified 0) (assumed 0) (digest (%sqlite-import-digest)))
    (bt:with-lock-held ((%sqlite-storage-lock backend))
      (let ((handle (%sqlite-handle backend :audit)))
        (%with-sqlite-statement
            (statement handle
                       "SELECT event_id,agent_id,partition_status,event_type,occurred_at,event_json,integrity_hash FROM pai_events ORDER BY storage_sequence ASC"
                       :audit)
          (%sqlite-import-map-lines
           paths
           (lambda (line source)
             (declare (ignore source))
             (multiple-value-bind
                   (event id agent-id partition-status type timestamp)
                 (%sqlite-import-parse-event line legacy-agent-id :audit)
               (declare (ignore event))
               (let ((code (%sqlite-step-raw statement)))
                 (unless (= code +sqlite-row+)
                   (if (= code +sqlite-done+)
                       (error 'storage-integrity-error :operation :audit
                              :detail "destination ended before source")
                       (%sqlite-check code handle :audit)))
                 (unless (and (= id (%sqlite-column-int64 statement 0))
                              (string= agent-id (%sqlite-column-text statement 1))
                              (string= partition-status
                                       (%sqlite-column-text statement 2))
                              (string= type (%sqlite-column-text statement 3))
                              (string= timestamp (%sqlite-column-text statement 4))
                              (string= line (%sqlite-column-text statement 5))
                              (string= (%storage-sha256 line)
                                       (%sqlite-column-text statement 6)))
                   (error 'storage-integrity-error :operation :audit
                          :detail (format nil "source/destination mismatch at event ~d" id))))
               (when last-id
                 (cond ((> id last-id)
                        (incf forward-holes (max 0 (1- (- id last-id)))))
                       ((< id last-id) (incf rewinds))))
               (when (gethash id seen) (incf duplicates))
               (setf (gethash id seen) t
                     maximum-id (if maximum-id (max maximum-id id) id))
               (%sqlite-import-digest-line-hash digest line)
               (unless first-id (setf first-id id))
               (setf last-id id)
               (incf count)
               (if (string= partition-status "verified")
                   (incf verified) (incf assumed)))))
          (let ((tail-code (%sqlite-step-raw statement)))
            (unless (= tail-code +sqlite-done+)
              (if (= tail-code +sqlite-row+)
                  (error 'storage-integrity-error :operation :audit
                         :detail "destination contains an extra event tail")
                  (%sqlite-check tail-code handle :audit)))))))
    (%sqlite-import-report
     (length paths) count first-id last-id maximum-id forward-holes
     duplicates rewinds (%sqlite-import-finish-digest digest)
     verified assumed "verified")))
