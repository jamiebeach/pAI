(defpackage :agent (:use :cl))
(in-package :agent)

(ql:quickload '(:uiop :ironclad :shasht) :silent t)

(defvar *r0e4-file-pass* 0)
(defvar *r0e4-file-fail* 0)

(defun r0e4-file-check (name condition)
  (if condition
      (progn (incf *r0e4-file-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *r0e4-file-fail*) (format t "  FAIL ~a~%" name))))

(load (test-source "projection-rebuild.lisp"))

(let* ((root (merge-pathnames "r0e4-full/file-snapshot/" (test-state-dir)))
       (output (merge-pathnames "r0e4-full/file-rebuild/" (test-state-dir)))
       (specs (projection-rebuild-file-specs))
       (baselines (make-hash-table :test #'equal)))
  ;; The public suite owns a deterministic synthetic checkpoint family. The
  ;; original test accidentally depended on files copied from a private run.
  (dolist (spec specs)
    (let ((path (merge-pathnames (getf spec :file) root)))
      (ensure-directories-exist path)
      (with-open-file (stream path :direction :output :if-exists :supersede
                              :if-does-not-exist :create :external-format :utf-8)
        (format stream "{\"fixture\":\"~a\",\"event_id\":106710}~%"
                (getf spec :name)))))
  (dolist (spec specs)
    (let ((file (getf spec :file)))
      (setf (gethash (getf spec :name) baselines)
            (make-projection-rebuild-baseline
             106710 file (uiop:read-file-string (merge-pathnames file root))))))
  (let* ((result (projection-rebuild-fold-files
                  nil :baselines baselines :expected-tail-event-id 106710))
         (written (projection-rebuild-write-files result output))
         (parity (projection-rebuild-file-parity result root)))
    (r0e4-file-check "all eight captured file projections are complete"
                     (gethash "complete" result))
    (r0e4-file-check "scratch materialization writes only its explicit root"
                     (equal written output))
    (r0e4-file-check "all eight rebuilt files have byte-derived SHA parity"
                     (and (= 8 (length parity))
                          (every (lambda (row) (gethash "equal" row)) parity))))
  (let ((partial (make-hash-table :test #'equal)))
    (maphash (lambda (name baseline)
               (unless (string= name "conversation-history")
                 (setf (gethash name partial) baseline)))
             baselines)
    (let ((result (projection-rebuild-fold-files nil :baselines partial)))
      (r0e4-file-check "omitted projection produces its exact named gap"
                       (find "conversation-history" (gethash "gaps" result)
                             :key (lambda (gap) (gethash "projection" gap))
                             :test #'string=)))))

(format t "~%R0e4 current file snapshot: ~a passed, ~a failed.~%"
        *r0e4-file-pass* *r0e4-file-fail*)
(when (plusp *r0e4-file-fail*) (uiop:quit 1))
