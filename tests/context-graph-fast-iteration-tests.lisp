;;;; harness: full-system
;;;; Explicit KG-only gate. Never delegates to tests/run-all.lisp.
(in-package :cl-user)

(let ((started (get-internal-real-time)))
  (dolist (file '("context-graph-formation-owner-tests.lisp"
                  "context-graph-runtime-adapter-tests.lisp"
                  "context-graph-confirmation-projection-tests.lisp"
                  "context-graph-lab-checkpoint-tests.lisp"
                  "context-graph-lab-prepare-tests.lisp"
                  "context-graph-lab-case-tests.lisp"))
    (let ((case-start (get-internal-real-time)))
      (handler-case (load (merge-pathnames file *load-truename*))
        (error (condition)
          (format t "KG-FAST-GATE ~a FAILED: ~a~%" file condition)
          (force-output)
          (uiop:quit 1)))
      (format t "KG-FAST-GATE ~a passed ~,3f seconds~%" file
              (/ (- (get-internal-real-time) case-start)
                 (float internal-time-units-per-second)))
      (force-output)))
  (format t "KG-FAST-GATE complete ~,3f seconds; provider fixtures only~%"
          (/ (- (get-internal-real-time) started)
             (float internal-time-units-per-second))))
