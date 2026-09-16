(in-package :agent)

(ql:quickload '(:postmodern :hunchentoot) :silent t)

;; Reader-only qualification catches malformed source without starting any
;; The agent subsystem, thread, database connection, provider call, or transport.
(dolist (file '("agent_loop.lisp"
                "candidate-policy.lisp" "conversation-turn-capture.lisp"
                "user-time.lisp" "publication-contract.lisp"
                "temporal-response-policy.lisp"
                "spreading-activation.lisp" "latent-thoughts.lisp"
                "conversational-initiative.lisp" "explore-novelty.lisp"
                "feedback-loop-containment.lisp"
                "telegram.lisp" "web-terminal.lisp" "event-log.lisp"
                "drives.lisp" "scheduler.lisp" "workout_nudge.lisp"
                "turn-watchdog.lisp"
                "runtime-observer-registry.lisp" "runtime-observer-audit.lisp"
                "public-outbound-gateway.lisp" "runtime-truth.lisp"
                "replay-capsules.lisp"
                "first-person-evidence.lisp" "grounded-agency.lisp"
                "candidate-representation.lisp"
                "reciprocity-canary.lisp"
                "pull-reciprocity.lisp"
                "stabilization-config.lisp" "dashboard.lisp"
                "recovery-health.lisp"))
  (let ((path (namestring (test-source file))))
    (with-open-file (stream path :external-format :utf-8)
      (loop for form = (read stream nil :eof)
            until (eq form :eof)))
    (format t "  ok   reader accepted ~a~%" file)))

(format t "~%30 source files read successfully.~%")

(let ((loop-source
        (uiop:read-file-string (namestring (test-source "agent_loop.lisp"))))
      (persona-source
        (uiop:read-file-string (namestring (test-source "PAI-SYSTEM-PROMPT.default.md"))))
      (intrusion-source
        (uiop:read-file-string (namestring (test-source "spreading-activation.lisp"))))
      (latent-source
        (uiop:read-file-string (namestring (test-source "latent-thoughts.lisp")))))
  (when (search "(utc-minute" loop-source :test #'char-equal)
    (error "Model-visible UTC fallback remains in agent_loop.lisp"))
  (when (search "Area/Location" loop-source :test #'char-equal)
    (error "Model-visible timestamp contract still exposes an IANA location"))
  (when (search "## Unbidden thoughts" persona-source :test #'char-equal)
    (error "Stable persona still frames internal cognition as public prose"))
  (when (search "A private thought became relevant now"
                latent-source :test #'char-equal)
    (error "Legacy latent injector still supplies a public cognition label"))
  (when (search "## Unbidden thoughts" intrusion-source :test #'char-equal)
    (error "Legacy intrusion injector still supplies a public cognition label"))
  (format t "  ok   model-visible clock has no UTC or location fallback~%")
  (format t "  ok   stable and legacy prompts expose no cognition label~%"))
