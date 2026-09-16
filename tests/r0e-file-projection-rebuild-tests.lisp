(defpackage :agent (:use :cl))
(in-package :agent)

(ql:quickload '(:uiop :ironclad) :silent t)

(defvar *r0e-pass* 0)
(defvar *r0e-fail* 0)

(defun r0e-check (name condition)
  (if condition
      (progn (incf *r0e-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *r0e-fail*) (format t "  FAIL ~a~%" name))))

(defun r0e-obj (&rest pairs)
  (loop with result = (make-hash-table :test #'equal)
        for (key value) on pairs by #'cddr
        do (setf (gethash key result) value)
        finally (return result)))

(defun r0e-event (id spec content)
  (r0e-obj
   "schema_version" 2 "id" id "type" (getf spec :type)
   "payload" (r0e-obj
              "projection" (getf spec :name) "operation" "replace"
              "file" (getf spec :file) "encoding" "utf-8"
              "content" content)))

(defun r0e-remove-tree (path)
  (when (probe-file path)
    (uiop:delete-directory-tree path :validate t :if-does-not-exist :ignore)))

(load (test-source "projection-rebuild.lisp"))

(let* ((specs (projection-rebuild-file-specs))
       (root #P"/tmp/r0e-file-projections/")
       (live (merge-pathnames "live/" root))
       (rebuilt (merge-pathnames "rebuilt/" root))
       (baselines (make-hash-table :test #'equal))
       (events nil))
  (r0e-remove-tree root)
  (ensure-directories-exist (merge-pathnames "sentinel" live))
  (loop for spec in specs for id from 101
        for baseline = (format nil "baseline/~a" (getf spec :name))
        for latest = (format nil "latest/~a/~a~a"
                             (getf spec :name) "unicode-π" (string #\Newline))
        do (setf (gethash (getf spec :name) baselines)
                 (make-projection-rebuild-baseline 100 (getf spec :file)
                                                   baseline))
           (push (r0e-event id spec latest) events)
           (with-open-file (out (merge-pathnames (getf spec :file) live)
                                :direction :output :if-exists :supersede
                                :if-does-not-exist :create
                                :external-format :utf-8)
             (write-string latest out)))
  (setf events (nreverse events))

  (format t "~%== checkpoint plus tail ==~%")
  (let* ((result (projection-rebuild-fold-files
                  events :baselines baselines :expected-tail-event-id 108))
         (states (gethash "states" result)))
    (r0e-check "all eight projections fold without gaps"
               (and (gethash "complete" result) (= 8 (hash-table-count states))))
    (r0e-check "later events replace every checkpoint"
               (loop for spec in specs
                     always (string= "event"
                                     (gethash "source"
                                              (gethash (getf spec :name)
                                                       states)))))
    (projection-rebuild-write-files result rebuilt)
    (r0e-check "explicit destination materializes exact bytes"
               (loop for spec in specs
                     always (string=
                             (uiop:read-file-string
                              (merge-pathnames (getf spec :file) live))
                             (uiop:read-file-string
                              (merge-pathnames (getf spec :file) rebuilt)))))
    (let ((parity (projection-rebuild-file-parity result live)))
      (r0e-check "all eight per-projection SHA-256 hashes match"
                 (and (= 8 (length parity))
                      (every (lambda (row) (gethash "equal" row)) parity))))
    (let* ((event-only (projection-rebuild-fold-files
                        events :expected-tail-event-id 108))
           (event-states (gethash "states" event-only)))
      (r0e-check "checkpoint plus tail equals event-only reconstruction"
                 (and (gethash "complete" event-only)
                      (loop for spec in specs
                            for name = (getf spec :name)
                            always (string=
                                    (gethash "content" (gethash name states))
                                    (gethash "content"
                                             (gethash name event-states))))))))

  (format t "~%== deterministic gap refusal ==~%")
  (let* ((missing-name "drives")
         (truncated-events
           (remove missing-name events
                   :key (lambda (event)
                          (gethash "projection" (gethash "payload" event)))
                   :test #'string=))
         (partial-baselines (alexandria:copy-hash-table baselines)))
    (remhash missing-name partial-baselines)
    (let* ((result (projection-rebuild-fold-files
                    truncated-events :baselines partial-baselines
                    :expected-tail-event-id 108))
           (gaps (gethash "gaps" result)))
      (r0e-check "truncated input names the missing projection"
                 (find missing-name gaps :key (lambda (gap)
                                                (gethash "projection" gap))
                                    :test #'string=))
      (r0e-check "truncated input is incomplete" (not (gethash "complete" result)))
      (r0e-check "incomplete rebuild refuses materialization"
                 (handler-case
                     (progn (projection-rebuild-write-files result rebuilt) nil)
                   (error () t)))))

  (let ((result (projection-rebuild-fold-files
                 (butlast events) :baselines baselines
                 :expected-tail-event-id 108)))
    (r0e-check "short tail is detected by its ledger-tail name"
               (find "ledger-tail" (gethash "gaps" result)
                     :key (lambda (gap) (gethash "projection" gap))
                     :test #'string=)))

  (let* ((bad (r0e-event 109 (first specs) "bad"))
         (payload (gethash "payload" bad)))
    (remhash "encoding" payload)
    (let* ((result (projection-rebuild-fold-files
                    (append events (list bad)) :baselines baselines
                    :expected-tail-event-id 109))
           (gaps (gethash "gaps" result))
           (state (gethash "modulators" (gethash "states" result))))
      (r0e-check "malformed event creates a named projection gap"
                 (find "modulators" gaps
                       :key (lambda (gap) (gethash "projection" gap))
                       :test #'string=))
      (r0e-check "malformed event cannot replace last good state"
                 (= 101 (gethash "event_id" state)))))

  (let ((uneven (alexandria:copy-hash-table baselines)))
    (setf (gethash "drives" uneven)
          (make-projection-rebuild-baseline 99 "drives.json" "older"))
    (let ((result (projection-rebuild-fold-files events :baselines uneven
                                                 :expected-tail-event-id 108)))
      (r0e-check "unequal checkpoint boundaries fail closed"
                 (find "checkpoint-boundary" (gethash "gaps" result)
                       :key (lambda (gap) (gethash "projection" gap))
                       :test #'string=)))))

(format t "~%file projection rebuild: ~a passed, ~a failed.~%"
        *r0e-pass* *r0e-fail*)
(when (plusp *r0e-fail*) (uiop:quit 1))
