(in-package :agent)

(ql:quickload '(:shasht :ironclad) :silent t)

(defvar *memory-storage-contract-pass* 0)
(defvar *memory-storage-contract-fail* 0)

(defun memory-storage-contract-check (name condition)
  (if condition
      (progn (incf *memory-storage-contract-pass*)
             (format t "  ok   ~a~%" name))
      (progn (incf *memory-storage-contract-fail*)
             (format t "  FAIL ~a~%" name))))

(defun memory-storage-contract-signals-p (condition-type thunk)
  (handler-case (progn (funcall thunk) nil)
    (condition (actual) (typep actual condition-type))))

(load (test-source "storage-substrate.lisp"))
(load (test-source "memory-storage.lisp"))

(defclass unsupported-memory-storage (memory-storage-backend) ())

(format t "~%== memory storage capability contract ==~%")

(let* ((backend (make-instance 'unsupported-memory-storage))
       (capabilities (memory-storage-capabilities backend)))
  (memory-storage-contract-check
   "abstract backend advertises no source snapshot capabilities"
   (and (= 1 (gethash "schema_version" capabilities))
        (null (gethash "read_snapshot" capabilities))
        (null (gethash "node_snapshot" capabilities))
        (null (gethash "edge_snapshot" capabilities))
        (null (gethash "exact_retrieval" capabilities))
        (null (gethash "mutation_projection" capabilities))
        (null (gethash "runtime_reads" capabilities))
        (null (gethash "runtime_writes" capabilities))))
  (memory-storage-contract-check
   "unsupported characterization fails with the typed condition"
   (memory-storage-contract-signals-p
    'memory-storage-unsupported-error
    (lambda () (memory-storage-characterize backend))))
  (memory-storage-contract-check
   "unsupported snapshot mapping fails before invoking a visitor"
   (let ((calls 0))
     (and (memory-storage-contract-signals-p
           'memory-storage-unsupported-error
           (lambda ()
             (memory-storage-map-snapshot
              backend (lambda (row) (declare (ignore row)) (incf calls))
              (lambda (row) (declare (ignore row)) (incf calls)))))
          (zerop calls))))
  (memory-storage-contract-check
   "unsupported import and audit fail with the typed condition"
   (and (memory-storage-contract-signals-p
         'memory-storage-unsupported-error
         (lambda () (memory-storage-import-snapshot backend backend nil)))
        (memory-storage-contract-signals-p
         'memory-storage-unsupported-error
         (lambda () (memory-storage-audit-snapshot backend)))))
  (memory-storage-contract-check
   "unsupported retrieval and mutation operations fail through closed ports"
   (let ((query
           (make-memory-exact-query
            :vector-binary-hex "000100003f800000"
            :profile "all-vectors-v1" :limit 1
            :lexemes (list (%memory-storage-object
                            "kind" "token" "text" "fixture")))))
     (and (memory-storage-contract-signals-p
           'memory-storage-unsupported-error
           (lambda () (memory-storage-exact-search backend query)))
          (memory-storage-contract-signals-p
           'memory-storage-unsupported-error
           (lambda () (memory-storage-lexical-search backend query)))
          (memory-storage-contract-signals-p
           'memory-storage-unsupported-error
           (lambda () (memory-storage-apply-mutation backend (make-hash-table))))
          (memory-storage-contract-signals-p
           'memory-storage-unsupported-error
           (lambda () (memory-storage-projection-report backend)))))))

(format t "~%~d passed, ~d failed~%"
        *memory-storage-contract-pass* *memory-storage-contract-fail*)
(when (plusp *memory-storage-contract-fail*)
  (error "Memory storage contract tests failed"))
