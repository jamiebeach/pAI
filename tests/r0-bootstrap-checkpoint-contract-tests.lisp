(defpackage :agent (:use :cl))
(in-package :agent)

(load (merge-pathnames "r0-bootstrap-checkpoint-contract.lisp" *load-truename*))

(defvar *r0-bootstrap-contract-pass* 0)
(defvar *r0-bootstrap-contract-fail* 0)

(defmacro r0-bootstrap-contract-check (name form)
  `(handler-case
       (if ,form
           (progn (incf *r0-bootstrap-contract-pass*)
                  (format t "PASS ~a~%" ,name))
           (progn (incf *r0-bootstrap-contract-fail*)
                  (format t "FAIL ~a~%" ,name)))
     (error (condition)
       (incf *r0-bootstrap-contract-fail*)
       (format t "FAIL ~a: ~a~%" ,name condition))))

(defun r0-bootstrap-contract-rejected-p (lines tables)
  (handler-case
      (progn (r0-bootstrap-parse-expected-row-count-lines lines tables) nil)
    (error () t)))

(let ((tables '("memory_nodes" "memory_edges" "memory_atom_jobs")))
  (let ((counts
          (r0-bootstrap-parse-expected-row-count-lines
           '("memory_nodes	8204" "memory_edges	14449"
             "memory_atom_jobs	0")
           tables)))
    (r0-bootstrap-contract-check
     "current mutable clone total is derived"
     (= 22653 (r0-bootstrap-expected-row-total counts tables)))
    (r0-bootstrap-contract-check
     "per-table observed count matches manifest"
     (r0-bootstrap-row-count-matches-p counts "memory_nodes" 8204))
    (r0-bootstrap-contract-check
     "per-table truncated export is rejected"
     (not (r0-bootstrap-row-count-matches-p counts "memory_nodes" 8203))))
  (let ((counts
          (r0-bootstrap-parse-expected-row-count-lines
           '("memory_nodes	8162" "memory_edges	14371"
             "memory_atom_jobs	0")
           tables)))
    (r0-bootstrap-contract-check
     "prior clone total remains data rather than source policy"
     (= 22533 (r0-bootstrap-expected-row-total counts tables))))
  (r0-bootstrap-contract-check
   "missing table rejected"
   (r0-bootstrap-contract-rejected-p
    '("memory_nodes	8204" "memory_edges	14449") tables))
  (r0-bootstrap-contract-check
   "duplicate table rejected"
   (r0-bootstrap-contract-rejected-p
    '("memory_nodes	8204" "memory_nodes	8204" "memory_edges	14449"
      "memory_atom_jobs	0") tables))
  (r0-bootstrap-contract-check
   "unknown table rejected"
   (r0-bootstrap-contract-rejected-p
    '("memory_nodes	8204" "memory_edges	14449" "unknown	0") tables))
  (r0-bootstrap-contract-check
   "negative count rejected"
   (r0-bootstrap-contract-rejected-p
    '("memory_nodes	-1" "memory_edges	14449" "memory_atom_jobs	0")
    tables))
  (r0-bootstrap-contract-check
   "noncanonical count rejected"
   (r0-bootstrap-contract-rejected-p
    '("memory_nodes	8 204" "memory_edges	14449"
      "memory_atom_jobs	0") tables)))

(format t "R0_BOOTSTRAP_CHECKPOINT_CONTRACT ~d passed, ~d failed~%"
        *r0-bootstrap-contract-pass* *r0-bootstrap-contract-fail*)
(when (plusp *r0-bootstrap-contract-fail*)
  (error "bootstrap checkpoint contract qualification failed"))
