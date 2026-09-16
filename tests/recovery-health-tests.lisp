(in-package :agent)

(ql:quickload '(:bordeaux-threads :ironclad :babel) :silent t)

(defvar *recovery-test-pass* 0)
(defvar *recovery-test-fail* 0)
(defvar *autonomous-write-mode* :normal)

(defun recovery-test-check (name condition)
  (if condition
      (progn (incf *recovery-test-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *recovery-test-fail*) (format t "  FAIL ~a~%" name))))

(load (test-source "mind-memory-core.lisp"))
(load (test-source "recovery-health.lisp"))

(recovery-test-check
 "pure memory core has a cold-recovery file and six-function inventory"
 (and (member "mind-memory-core.lisp" *pai-recovery-required-files*
              :test #'string=)
      (= 6 (length *pai-recovery-required-mind-memory-functions*))
      (let* ((report (pai-recovery-report))
             (functions (gethash "mind_memory_functions" report)))
        (every (lambda (name)
                 (eq t (gethash (string-downcase name) functions)))
               *pai-recovery-required-mind-memory-functions*))))

(recovery-test-check "memory search has cold-recovery file and function consumers"
                     (and (member "memory-search-tool.lisp"
                                  *pai-recovery-required-files*
                                  :test #'string=)
                          (member 'search-memory
                                  *pai-recovery-required-functions*)))

(setf *pai-recovery-required-files* nil
      *pai-recovery-required-functions* '(identity)
      *pai-recovery-grounded-required-functions* '(grounded-fixture-missing)
      (fdefinition '%memory-node-count) (lambda () 0))

(let* ((report (pai-recovery-report))
       (functions (gethash "functions" report))
       (encoded (shasht:write-json report nil)))
  (recovery-test-check "function availability is a strict boolean"
                       (eq t (gethash "identity" functions)))
  (recovery-test-check "complete recovery report is JSON serializable"
                       (and (stringp encoded) (search "\"identity\": true" encoded))))

(let ((report (pai-recovery-report)))
  (recovery-test-check "legacy rollback skips the unloaded grounded contract"
                       (and (not (gethash "grounded_contract_required" report))
                            (null (gethash "grounded-fixture-missing"
                                           (gethash "functions" report))))))
(setf (fdefinition 'grounded-agency-report) (lambda () (obj)))
(unwind-protect
     (let ((report (pai-recovery-report)))
       (recovery-test-check "grounded image requires every grounded function"
                            (and (gethash "grounded_contract_required" report)
                                 (eq nil (gethash "grounded-fixture-missing"
                                                  (gethash "functions" report))))))
  (fmakunbound 'grounded-agency-report))

(setf (fdefinition 'stabilization-smoke-run)
      (lambda () (obj "ok" t "probes" (obj))))
(let ((report (pai-recovery-report :level :executable)))
  (recovery-test-check "executable level includes probe result"
                       (gethash "ok" (gethash "executable" report))))
(let ((*autonomous-write-mode* :normal))
  (setf (fdefinition 'stabilization-smoke-run)
        (lambda () (obj "ok" nil "probes" (obj))))
  (recovery-test-check "executable failure is fatal"
                       (not (pai-recovery-assert :level :executable)))
  (recovery-test-check "failure pauses autonomy but leaves chat process available"
                       (eq *autonomous-write-mode* :paused)))

(format t "~%~a passed, ~a failed~%" *recovery-test-pass* *recovery-test-fail*)
(when (plusp *recovery-test-fail*) (sb-ext:exit :code 1))
