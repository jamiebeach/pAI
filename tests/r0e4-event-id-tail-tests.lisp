(defpackage :agent (:use :cl))
(in-package :agent)

(ql:quickload '(:bordeaux-threads :shasht :ironclad) :silent t)

(defvar *r0e4a-pass* 0)
(defvar *r0e4a-fail* 0)

(defun r0e4a-check (name condition)
  (if condition
      (progn (incf *r0e4a-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *r0e4a-fail*) (format t "  FAIL ~a~%" name))))

(defun obj (&rest pairs)
  (loop with result = (make-hash-table :test #'equal)
        for (key value) on pairs by #'cddr
        do (setf (gethash key result) value)
        finally (return result)))

(defun r0e4a-event (id)
  (obj "schema_version" 2 "id" id
       "timestamp" (format nil "2026-08-09T00:00:~2,'0dZ" id)
       "type" (if (oddp id) "projection-state" "noise")
       "payload" (if (oddp id)
                     (obj "projection" "modulators" "operation" "replace"
                          "file" "modulators.json" "encoding" "utf-8"
                          "content" (format nil "~a" id))
                     (obj "value" id))))

(defun r0e4a-write-events (pathname ids)
  (ensure-directories-exist pathname)
  (with-open-file (out pathname :direction :output :if-exists :supersede
                                :if-does-not-exist :create
                                :external-format :utf-8)
    (let ((*print-pretty* nil))
      (dolist (id ids)
        (write-line (shasht:write-json (r0e4a-event id) nil) out)))))

(setf (fdefinition 'auto-turn) (lambda (prompt) prompt)
      (fdefinition 'execute) (lambda (&rest arguments)
                               (declare (ignore arguments)) nil)
      (fdefinition 'propose-loop) (lambda (&rest arguments)
                                    (declare (ignore arguments)) nil))
(defparameter *tools* (vector))

(load (test-source "event-log.lisp"))
(load (test-source "projection-rebuild.lisp"))

(let* ((root #P"/tmp/r0e4-event-id-tail/")
       (legacy (merge-pathnames "events.jsonl" root))
       (segments (merge-pathnames "segments/" root))
       (legacy-index (merge-pathnames "legacy-index.json" root))
       (segment-indexes (merge-pathnames "segment-indexes/" root))
       (*event-log-file* legacy)
       (*event-log-segment-directory* segments)
       (*event-log-legacy-index-file* legacy-index)
       (*event-log-segment-index-directory* segment-indexes)
       (*event-log-segment-span* 5)
       (*event-log-legacy-index-stride* 2)
       (*event-log-segment-index-stride* 2))
  (when (probe-file root)
    (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))
  (r0e4a-write-events legacy '(1 2 3 4 5))
  (r0e4a-write-events (%event-segment-path 6) '(6 7 8 9 10))
  (r0e4a-write-events (%event-segment-path 11) '(11 12 13 15))
  (r0e4a-write-events (%event-segment-path 16) '(16 17 18 19 20))
  (%event-atomic-write-json legacy-index (%event-build-legacy-index 5))
  (dolist (path (%event-segment-paths)) (%event-write-built-segment-index path))

  (format t "~%== bounded incremental ID tail ==~%")
  (let* ((operations nil) (visited-ids nil)
         (*event-storage-open-observer*
           (lambda (operation pathname)
             (push (list operation (file-namestring pathname)) operations))))
    (multiple-value-bind (complete last-id visited)
        (map-events
         (lambda (event) (push (gethash "id" event) visited-ids))
         :after-id 13 :through-id 18 :types '("projection-state"))
      (setf visited-ids (nreverse visited-ids))
      (r0e4a-check "visitor receives only selected tail events in order"
                   (equal '(15 17) visited-ids))
      (r0e4a-check "scan reports complete boundary independent of type filter"
                   (and complete (= 18 last-id) (= 2 visited)))
      (r0e4a-check "legacy and completed pre-checkpoint segment are skipped"
                   (and (find (list :skip "events.jsonl") operations
                              :test #'equal)
                        (find (list :skip
                                   "events-000000000006-000000000010.jsonl")
                              operations :test #'equal)))
      (r0e4a-check "only overlapping segments are scanned"
                   (= 2 (count :scan operations :key #'first)))
      (r0e4a-check "sparse ID index seeks within first overlapping segment"
                   (find :seek operations :key #'first))))

  (format t "~%== streaming source feeds pure fold ==~%")
  (let ((baselines (make-hash-table :test #'equal)))
    (dolist (spec (projection-rebuild-file-specs))
      (setf (gethash (getf spec :name) baselines)
            (make-projection-rebuild-baseline
             10 (getf spec :file) "checkpoint")))
    (let* ((source (make-projection-rebuild-event-source
                    :after-id 10 :through-id 18
                    :types '("projection-state"
                             "conversation-history-transform")))
           (result (projection-rebuild-fold-files
                    source :baselines baselines :expected-tail-event-id 18))
           (state (gethash "modulators" (gethash "states" result))))
      (r0e4a-check "fold consumes callback source without an event list"
                   (and (gethash "complete" result)
                        (= 18 (gethash "last_event_id" result))
                        (string= "17" (gethash "content" state))))
      (r0e4a-check "type-filtered source still proves the full tail boundary"
                   (null (gethash "gaps" result)))))

  (let ((baselines (make-hash-table :test #'equal)))
    (dolist (spec (projection-rebuild-file-specs))
      (setf (gethash (getf spec :name) baselines)
            (make-projection-rebuild-baseline
             10 (getf spec :file) "checkpoint")))
    (let* ((source (lambda (visitor)
                     (funcall visitor (r0e4a-event 11))
                     (values nil 11 1)))
           (result (projection-rebuild-fold-files source :baselines baselines)))
      (r0e4a-check "an incomplete scanner can never yield a complete rebuild"
                   (and (not (gethash "complete" result))
                        (find "event-source" (gethash "gaps" result)
                              :key (lambda (gap) (gethash "projection" gap))
                              :test #'string=)))))

  (format t "~%== holes and invalid-index fallback ==~%")
  (multiple-value-bind (complete last-id visited)
      (map-events (lambda (event) (declare (ignore event)))
                  :after-id 12 :through-id 15)
    (r0e4a-check "reserved-style ID hole does not imply corruption"
                 (and complete (= 15 last-id) (= 2 visited))))

  (let* ((path (%event-segment-path 11))
         (index-path (%event-segment-index-path path))
         (tampered (%event-read-json-file index-path))
         (ids nil))
    (setf (gethash "checksum_sha256" tampered) "invalid")
    (%event-atomic-write-json index-path tampered)
    (multiple-value-bind (complete last-id visited)
        (map-events (lambda (event) (push (gethash "id" event) ids))
                    :after-id 12 :through-id 15
                    :types '("projection-state"))
      (r0e4a-check "invalid index falls back without suppressing rows"
                   (and complete (= 15 last-id) (= 2 visited)
                        (equal '(13 15) (nreverse ids))))))

  (r0e4a-check "invalid reversed ID bounds are rejected"
               (handler-case
                   (progn (map-events (lambda (event) (declare (ignore event)))
                                      :after-id 20 :through-id 10)
                          nil)
                 (error () t))))

(format t "~%R0e4 incremental event-ID tail: ~a passed, ~a failed.~%"
        *r0e4a-pass* *r0e4a-fail*)
(when (plusp *r0e4a-fail*) (uiop:quit 1))
