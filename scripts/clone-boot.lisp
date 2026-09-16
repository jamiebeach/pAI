;;;; clone-boot.lisp -- boot pAI against a restored clone, without starting it.
;;;;
;;;; This is the first thing that exercises the :restore phase. Until it ran,
;;;; 39 of 57 init actions had never executed once (docs/architecture-
;;;; assessment.md).
;;;;
;;;; Deliberately withholds :start. That phase launches nine threads -- tick
;;;; loop, drives, watchdogs, persistence heartbeat -- which would begin
;;;; writing to the clone and calling a provider. Restoration is the thing
;;;; under test here; running is a separate decision.
;;;;
;;;; STOP-ON-ERROR is NIL on purpose. The default fails closed at the first
;;;; bad action, which is right for a real boot and wrong for this: one run
;;;; that reports every failure is worth a dozen that each report one.
;;;;
;;;; Usage (from scripts/clone-boot.sh):
;;;;   sbcl --load scripts/clone-boot.lisp
;;;;
;;;; Environment:
;;;;   PAI_PG_*        clone connection (never production)
;;;;   PAI_CLONE_PHASES  space-separated phase names, default "configure install restore"

(in-package :cl-user)

(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(push #P"/pai/" asdf:*central-registry*)

(defun %phases ()
  (let ((raw (or (uiop:getenv "PAI_CLONE_PHASES") "configure install restore")))
    (mapcar (lambda (name) (intern (string-upcase name) :keyword))
            (uiop:split-string raw :separator " "))))

(defun %guard-not-production ()
  "Refuse to run against anything that is not an explicitly disposable clone.

   The whole point of this harness is to touch a copy. A typo in PAI_PG_HOST
   that reached the live database would be discovered by its consequences."
  (let ((host (or (uiop:getenv "PAI_PG_HOST") "")))
    (unless (search "clone" host)
      (error "Refusing to boot: PAI_PG_HOST ~s is not a clone host." host))
    host))

(handler-case
    (progn
      (format t "~&== loading system ==~%")
      (let ((*standard-output* (make-broadcast-stream)))
        (asdf:load-system :pai))
      (format t "~&system loaded.~%")
      (format t "~&clone host: ~a~%" (%guard-not-production))

      (let* ((phases (%phases))
             (results (progn
                        (format t "~&~%== initialize ~{~a~^ ~} ==~%" phases)
                        (funcall (intern "INITIALIZE" :agent)
                                 :phases phases :stop-on-error nil :verbose t))))
        (format t "~&~%== results ==~%")
        (let ((ok 0) (skipped 0) (failed '()))
          (dolist (row results)
            (destructuring-bind (name . outcome) row
              (cond ((eq outcome :ok) (incf ok))
                    ((eq outcome :skipped) (incf skipped))
                    (t (push (cons name outcome) failed)))))
          (format t "~&ok       ~3d~%skipped  ~3d~%failed   ~3d~%"
                  ok skipped (length failed))
          (when failed
            (format t "~&~%== failures ==~%")
            (dolist (row (nreverse failed))
              (format t "~&FAILED ~a~%       ~a~%" (car row) (cdr row))))
          (format t "~&~%CLONE-BOOT-DONE ok=~d skipped=~d failed=~d~%"
                  ok skipped (length failed)))))
  (error (e)
    (format t "~&~%CLONE-BOOT-FATAL: ~a~%" e)))

(finish-output)
