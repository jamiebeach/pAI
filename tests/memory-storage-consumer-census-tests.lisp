(in-package :agent)

(defvar *memory-storage-census-pass* 0)
(defvar *memory-storage-census-fail* 0)

(defun memory-storage-census-check (name condition)
  (if condition
      (progn (incf *memory-storage-census-pass*)
             (format t "  ok   ~a~%" name))
      (progn (incf *memory-storage-census-fail*)
             (format t "  FAIL ~a~%" name))))

(defparameter *memory-storage-census-expected*
  '("src/adapters/postgres/postgres-memory-storage.lisp"
    "src/adapters/sqlite/sqlite-derived-storage.lisp"
    "src/kernel/projection-rebuild.lisp"
    "src/mind/drives/drives.lisp"
    "src/mind/drives/prediction-journal.lisp"
    "src/mind/memory/epistemic-memory.lisp"
    "src/mind/memory/legacy-memory-audit.lisp"
    "src/mind/memory/memory-architecture.lisp"
    "src/mind/memory/memory-atom-shadow.lisp"
    "src/mind/memory/memory-nodes.lisp"
    "src/mind/memory/typed-retrieval.lisp"
    "src/mind/observability/dashboard.lisp"
    "src/mind/observability/lab.lisp"
    "src/mind/reflection/episode-boundary.lisp"
    "src/mind/reflection/reflection-novelty.lisp"
    "src/mind/reflection/spreading-activation.lisp"
    "src/mind/stabilization/stabilization-config.lisp"
    "src/mind/stabilization/stabilization-smoke-tests.lisp"
    "src/mind/ticks/tick-commit.lisp"
    "src/mind/ticks/tick-loop.lisp"))

(defun memory-storage-census-relative-name (path)
  (substitute #\/ #\\
              (namestring (uiop:enough-pathname path *pai-root*))))

(defun memory-storage-census-direct-reference-p (path)
  (let ((source (uiop:read-file-string path)))
    (or (search "memory_nodes" source)
        (search "memory_edges" source)
        (search "memory_atom_" source))))

(format t "~%== direct PostgreSQL memory consumer census ==~%")

(let* ((files (directory (merge-pathnames "src/**/*.lisp" *pai-root*)))
       (actual
         (sort
          (mapcar #'memory-storage-census-relative-name
                  (remove-if-not #'memory-storage-census-direct-reference-p
                                 files))
          #'string<))
       (expected (sort (copy-list *memory-storage-census-expected*) #'string<)))
  (memory-storage-census-check
   "census scans a non-empty recursive source set" (plusp (length files)))
  (memory-storage-census-check
   "every direct memory-table owner is explicitly enumerated"
   (equal expected actual))
  (memory-storage-census-check
   "only the qualified source and destination adapters own memory tables"
   (equal '("src/adapters/postgres/postgres-memory-storage.lisp"
            "src/adapters/sqlite/sqlite-derived-storage.lisp")
          (remove-if-not
           (lambda (name) (uiop:string-prefix-p "src/adapters/" name))
           actual))))

(format t "~%~d passed, ~d failed~%"
        *memory-storage-census-pass* *memory-storage-census-fail*)
(when (plusp *memory-storage-census-fail*)
  (error "Memory storage consumer census failed"))
