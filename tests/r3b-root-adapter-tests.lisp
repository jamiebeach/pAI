(in-package :agent)

(defvar *r3b-root-adapter-pass* 0)
(defvar *r3b-root-adapter-fail* 0)

(defun r3b-root-check (name condition)
  (if condition
      (progn (incf *r3b-root-adapter-pass*) (format t "PASS ~a~%" name))
      (progn (incf *r3b-root-adapter-fail*) (format t "FAIL ~a~%" name))))

(defun r3b-source (name)
  (uiop:read-file-string (test-source name)))

(let ((application-files '("kernel-tool-dispatch-bootstrap.lisp"
                           "recovery-health.lisp" "runware.lisp"))
      (artifact-files '("bounded-work-tools.lisp" "runware.lisp"
                        "postgres-backup.lisp" "core-snapshot.lisp"
                        "llm-debug-capture.lisp" "audit-ephemeral.lisp"
                        "agent_print.lisp"))
      (proposal-files '("agent_helpers.lisp" "drift-monitor.lisp"))
      (secret-files '("admin-console.lisp" "brave-credential.lisp"
                      "runware.lisp" "telegram.lisp")))
  (r3b-root-check
   "stable application root is present in every source authority adapter"
   (every (lambda (name) (search "PAI_APPLICATION_ROOT" (r3b-source name)))
          application-files))
  (r3b-root-check
   "stable artifact root is present in every artifact adapter"
   (every (lambda (name) (search "PAI_ARTIFACT_ROOT" (r3b-source name)))
          artifact-files))
  (r3b-root-check
   "stable staged proposal root is present in proposal adapters"
   (every (lambda (name) (search "PAI_STAGED_PROPOSAL_ROOT" (r3b-source name)))
          proposal-files))
  (r3b-root-check
   "stable secret root is present in every credential adapter"
   (every (lambda (name) (search "PAI_SECRET_ROOT" (r3b-source name)))
          secret-files))
  (r3b-root-check
   "legacy defaults remain available before activation"
   (and (search "/agent/state/admin-token.txt" (r3b-source "admin-console.lisp"))
        (search "/agent/state/bravekey.txt" (r3b-source "brave-credential.lisp"))
        (search "/agent/state/runwarekey.txt" (r3b-source "runware.lisp"))
        (search "read-token-from-file \"bot.txt\"" (r3b-source "telegram.lisp"))))
  (r3b-root-check
   "aliases remain available to frozen isolated fixtures"
   (every (lambda (name) (search "PAI_R3A_" (r3b-source name)))
          (append application-files artifact-files proposal-files))))

(format t "R3B_ROOT_ADAPTER_TESTS: ~d passed, ~d failed~%"
        *r3b-root-adapter-pass* *r3b-root-adapter-fail*)
(when (plusp *r3b-root-adapter-fail*) (sb-ext:exit :code 1))
