(ql:quickload '(:hunchentoot :shasht :ironclad :babel) :silent t)

(defpackage :agent (:use :cl))
(in-package :agent)

(defun obj (&rest pairs)
  (loop with table = (make-hash-table :test #'equal)
        for (key value) on pairs by #'cddr
        do (setf (gethash key table) value)
        finally (return table)))

(defparameter *autonomous-write-mode* :paused)
(defparameter *memory-atom-decomposition-mode* :off)
(defparameter *context-curator-mode* :off)
(defparameter *context-projection-event-fn* nil)
(defvar *tools*
  (vector (obj "type" "function" "function" (obj "name" "search-memory"))
          (obj "type" "function" "function" (obj "name" "lisp-eval"))))
(defvar *turn-capture-context* nil)
(defvar *publication-contract-current* nil)
(defun execute (tool-call) tool-call)

(defun search-memory (query &key limit)
  (declare (ignore limit))
  (shasht:write-json
   (obj "schema_version" 1 "status" "empty" "query_characters" (length query)
        "database_write_count" 0 "results" #()) nil))
(defun build-context-projection (message &key mode)
  (obj "message_chars" (length message) "mode" (string-downcase (symbol-name mode))))
(defun render-context-projection (projection)
  (format nil "projection:~a" (gethash "message_chars" projection)))
(defun modulator-state () (obj "arousal" 0.3d0 "valence" 0.5d0))
(defun public-system-prompt-render-stable () "stable prompt")
(defun call-with-timing-span (name thunk &key attributes)
  (declare (ignore name attributes)) (funcall thunk))
(defun call-with-tool-event-observation (call thunk) (funcall thunk call))
(defun memory-search-tool-handle (call) call)
(defun observe-tool-result-appraisal (result) result)
(defun call-with-proposal-provenance (call thunk) (funcall thunk call))
(defun self-mod-tool-handle (call) call)
(defun pai-enhancements-tool-handle (call) call)
(defun web-terminal-tool-handle (call) call)
(defun runware-tool-handle (call) call)
(defun bounded-work-tool-handle (call) call)
(defun near-term-intention-tool-handle (call) call)
(defun kernel-tool-dispatch-bootstrap-report ()
  (obj "schema_version" 1 "status" "kernel-installed" "installed" t))
(defun tool-dispatch-boot-mode () :kernel)

(in-package :cl-user)
(defvar *dev-workbench-pass* 0)
(defvar *dev-workbench-fail* 0)
(defun dev-workbench-check (name condition)
  (if condition
      (progn (incf *dev-workbench-pass*) (format t "PASS ~a~%" name))
      (progn (incf *dev-workbench-fail*) (format t "FAIL ~a~%" name))))
(defun dev-workbench-error (thunk)
  (handler-case (progn (funcall thunk) nil) (error (condition) condition)))

(let ((manifest-path "/tmp/dev-workbench-test-manifest.json"))
  (ensure-directories-exist manifest-path)
  (with-open-file (stream manifest-path :direction :output :if-exists :supersede
                         :if-does-not-exist :create)
    (write-string "{\"schema_version\":1,\"run_id\":\"rdev0-test\",\"disposable\":true,\"database_label_verified\":true,\"network_internal\":true,\"ingress_scope\":\"loopback-only\",\"provider_egress\":\"disabled\",\"delivery_authority\":\"absent\"}" stream))
  (setf (uiop:getenv "PAI_DEV_WORKBENCH") "enabled"
        (uiop:getenv "PAI_DEV_RUN_ID") "rdev0-test"
        (uiop:getenv "PAI_DEV_MANIFEST_FILE") manifest-path)
  (unwind-protect
      (progn
        ;; AGENT.LISP supplies the package's production catalogue before this
        ;; isolated suite loads. Own the exact two-tool fixture explicitly;
        ;; DEFVAR above intentionally cannot replace an existing binding.
        (setf agent::*tools*
              (vector (agent::obj "type" "function" "function"
                                  (agent::obj "name" "search-memory"))
                      (agent::obj "type" "function" "function"
                                  (agent::obj "name" "lisp-eval"))))
        (load (test-source "kernel-tool-dispatch-core.lisp"))
        (load (test-source "kernel-tool-dispatch-shadow.lisp"))
        (load (test-source "kernel-tool-handler-bindings.lisp"))
        (load (test-source "kernel-tool-dispatch-runtime.lisp"))
        (load (test-source "kernel-tool-dispatch-shadow-runtime.lisp"))
        ;; TOOL-DISPATCH-RUNTIME-INSTALL refuses to install unless all three of
        ;; its required ports are available:
        ;;
        ;;   CALL-WITH-TIMING-SPAN             observability-tracing.lisp
        ;;   CALL-WITH-TOOL-EVENT-OBSERVATION  event-log.lisp
        ;;   CALL-WITH-PROPOSAL-PROVENANCE     self-mod-provenance.lisp
        ;;
        ;; The original entrypoint had already loaded every file, so this suite
        ;; never had to name the dependency. Sibling dispatch suites stub the
        ;; ports; this one drives real dispatch end to end, so it takes the
        ;; real modules.
        ;;
        ;; The install refuses one port at a time, and the failure surfaces
        ;; several frames away as "DEV-WORKBENCH-TOOL-EXECUTE is undefined" --
        ;; a message that names neither the port nor the install.
        ;;
        ;; These are transparent stubs rather than the real modules, matching
        ;; the sibling dispatch suites. Loading event-log.lisp for real is not
        ;; an option here: it redefines EXECUTE, AUTO-TURN and PROPOSE-LOOP as
        ;; part of a wrap chain, and pulling it in mid-sequence leaves the
        ;; dispatch facade uninitialised. The ports are cross-cutting
        ;; observers; what this suite examines is workbench behaviour, which
        ;; passes through them unchanged.
        (unless (fboundp 'agent::call-with-timing-span)
          (setf (fdefinition 'agent::call-with-timing-span)
                (lambda (name thunk &key attributes)
                  (declare (ignore name attributes))
                  (funcall thunk))))
        (unless (fboundp 'agent::call-with-tool-event-observation)
          (setf (fdefinition 'agent::call-with-tool-event-observation)
                (lambda (call thunk) (funcall thunk call))))
        (unless (fboundp 'agent::call-with-proposal-provenance)
          (setf (fdefinition 'agent::call-with-proposal-provenance)
                (lambda (call thunk) (funcall thunk call))))
        ;; TOOL-DISPATCH-SHADOW-RUNTIME-INSTALL wraps EXECUTE and refuses to
        ;; install if it is unbound. This suite supplies its own EXECUTE later,
        ;; for the ownership-conflict cases; it needs an incumbent to exist
        ;; before the first install too. Under the original entrypoint the real
        ;; one was always already there.
        (unless (fboundp 'agent::execute)
          (setf (fdefinition 'agent::execute) (lambda (call) call)))
        (agent::tool-dispatch-shadow-initialize
         (namestring (test-source "kernel-tool-dispatch-module.sexp")))
        (agent::tool-dispatch-runtime-install '("search-memory" "lisp-eval"))
        (agent::tool-dispatch-shadow-runtime-install)
        (load (test-source "turn-trace-projection.lisp"))
        (load (test-source "dev-workbench.lisp"))
        (let ((report (agent::dev-workbench-capability-report)))
          (dev-workbench-check "capability report is ready"
                               (string= "ready" (gethash "status" report)))
          (dev-workbench-check "provider is structurally unavailable"
                               (null (gethash "provider_calls_available" report)))
          (dev-workbench-check "production state is structurally unavailable"
                               (null (gethash "production_state_available" report)))
          (dev-workbench-check "delivery authority is absent"
                               (null (gethash "delivery_authority" report)))
          (dev-workbench-check "access is explicitly loopback-only"
                               (string= "loopback-only"
                                        (gethash "access_scope" report)))
          (dev-workbench-check "bearer authentication is not required"
                               (null (gethash "authentication_required" report)))
          (dev-workbench-check "tool dispatch shadow is initialized"
                               (eq t (gethash "tool_dispatch_shadow" report)))
          (dev-workbench-check "dispatch shadow is retired from clean boot"
                               (null (gethash "tool_dispatch_runtime" report)))
          (dev-workbench-check "kernel dispatch cutover is active"
                               (eq t (gethash "kernel_dispatch_runtime" report)))
          (dev-workbench-check "real tool execution is available in contained dev"
                               (and (eq t (gethash "tool_execution_available" report))
                                    (eq t (gethash "database_mutation_available" report))))
          (dev-workbench-check "workbench exposes the Rdev1 build"
                               (string= "rdev1-trace-1"
                                        (gethash "build_id" report)))
          (dev-workbench-check "turn trace fixtures are available"
                               (eq t (gethash "turn_trace_fixtures" report))))
        (let* ((index (agent::dev-workbench-turn-trace-index))
               (trace (agent::dev-workbench-turn-trace
                       (agent::obj "fixture_id" "pathological-memory-loop"))))
          (dev-workbench-check "trace index is bounded and content-free"
                               (and (= 2 (length (gethash "fixtures" index)))
                                    (null (gethash "private_content_included" index))))
          (dev-workbench-check "workbench uses the shared sanitized projection"
                               (and (string= "projected" (gethash "status" trace))
                                    (= 2 (gethash "search-memory"
                                                   (gethash "tool_counts" trace)))
                                    (null (gethash "private_content_included" trace)))))
        (let* ((request (agent::obj "message" "hello" "records" #()
                                    "overlay" "temporary"))
               (result (agent::dev-workbench-assemble request)))
          (dev-workbench-check "assemble returns bounded typed result"
                               (and (string= "assembled" (gethash "status" result))
                                    (= 5 (gethash "message_characters" result))
                                    (= 0 (gethash "provider_calls" result))
                                    (= 0 (gethash "database_writes" result)))))
        (dev-workbench-check
         "assemble rejects unknown input keys"
         (not (null (dev-workbench-error
                     (lambda ()
                       (agent::dev-workbench-assemble
                        (agent::obj "message" "x" "records" #() "overlay" ""
                                    "surprise" t)))))))
        (dev-workbench-check
         "assemble enforces message bound"
         (not (null (dev-workbench-error
                     (lambda ()
                       (agent::dev-workbench-assemble
                        (agent::obj "message" (make-string 8001 :initial-element #\x)
                                    "records" #() "overlay" "")))))))
        (let ((memory (agent::dev-workbench-memory-search
                       (agent::obj "query" "earlier book" "limit" 3))))
          (dev-workbench-check "memory stage uses declared read-only operator"
                               (and (string= "empty" (gethash "status" memory))
                                    (= 0 (gethash "database_write_count" memory)))))
        (let ((projection (agent::dev-workbench-context-projection
                           (agent::obj "message" "hello"))))
          (dev-workbench-check "context stage renders real declared seam"
                               (and (string= "projected" (gethash "status" projection))
                                    (string= "projection:5" (gethash "rendered" projection))
                                    (= 0 (gethash "provider_calls" projection)))))
        (let ((affect (agent::dev-workbench-affect-snapshot)))
          (dev-workbench-check "affect stage reads real state without simulation"
                               (and (string= "available" (gethash "status" affect))
                                    (null (gethash "simulated" affect)))))
        (let ((appraisal (agent::dev-workbench-appraisal (agent::obj))))
          (dev-workbench-check "missing public appraisal port fails transparently"
                               (string= "unavailable" (gethash "status" appraisal))))
        (let ((prompt (agent::dev-workbench-prompt-preview
                       (agent::obj "overlay" "candidate note"))))
          (dev-workbench-check "prompt overlay is ephemeral and visibly delimited"
                               (and (null (gethash "persisted" prompt))
                                    (search "DEV-OVERLAY:BEGIN"
                                            (gethash "rendered_prompt" prompt)))))
        (let ((agent::*tools*
                (vector (agent::obj "type" "function" "function"
                                    (agent::obj "name" "lisp-eval")))))
          (let ((dispatch (agent::dev-workbench-tool-dispatch
                           (agent::obj "name" "lisp-eval"))))
            (dev-workbench-check
             "tool dispatch inspector resolves without execution"
             (and (string= "match" (gethash "classification" dispatch))
                  (null (gethash "execution_attempted" dispatch))
                  (= 0 (gethash "provider_calls" dispatch))
                  (= 0 (gethash "database_writes" dispatch))
                  (= 0 (gethash "event_appends" dispatch))
                  (= 0 (gethash "delivery_attempts" dispatch)))))
          (let ((unknown (agent::dev-workbench-tool-dispatch
                          (agent::obj "name" "not-a-tool")))
                (malformed (agent::dev-workbench-tool-dispatch
                            (agent::obj "name" ""))))
            (dev-workbench-check
             "unknown and malformed dispatch controls are results"
             (and (string= "unknown" (gethash "classification" unknown))
                  (string= "malformed"
                           (gethash "classification" malformed)))))
          (dev-workbench-check
           "tool dispatch rejects an arguments escape hatch"
           (not (null
                 (dev-workbench-error
                  (lambda ()
                    (agent::dev-workbench-tool-dispatch
                     (agent::obj "name" "lisp-eval" "arguments" "{}"))))))))
        (let* ((runtime (agent::dev-workbench-tool-dispatch-runtime))
               (installed (gethash "runtime" runtime))
               (kernel (gethash "kernel_dispatch" runtime)))
          (dev-workbench-check
           "retired-shadow status is read-only and non-executing"
           (and (null (gethash "installed" installed))
                (string= "retired" (gethash "status" installed))
                (null (gethash "execution_attempted" runtime))
                (= 0 (gethash "provider_calls" runtime))
                (= 0 (gethash "database_writes" runtime))
                (= 0 (gethash "event_appends" runtime))
                (= 0 (gethash "delivery_attempts" runtime))
                (gethash "installed" kernel)
                (null (gethash "ownership_conflict" kernel)))))
        (let* ((agent::*tools*
                 (vector (agent::obj "type" "function" "function"
                                     (agent::obj "name" "lisp-eval"
                                                 "parameters"
                                                 (agent::obj "type" "object")))))
               (before (gethash "sequence"
                                (agent::tool-dispatch-shadow-runtime-report)))
               (catalog (agent::dev-workbench-tool-catalog)))
          (dev-workbench-check
           "catalogue copies exact advertisement without dispatch"
           (and (= 1 (gethash "tool_count" catalog))
                (string= "kernel explicit composition"
                         (gethash "rdev0_dispatch_route"
                                  (aref (gethash "tools" catalog) 0)))
                (null (gethash "execution_attempted" catalog))
                (= before (gethash "sequence"
                                   (agent::tool-dispatch-shadow-runtime-report)))))
          (setf (gethash "name" (gethash "function"
                                         (aref (gethash "tools" catalog) 0)))
                "tampered")
          (dev-workbench-check
           "catalogue result cannot mutate incumbent advertisement"
           (string= "lisp-eval"
                    (gethash "name" (gethash "function"
                                             (aref agent::*tools* 0)))))
          (let* ((arguments (agent::obj "form" "(+ 20 22)"))
                 (result
                   (agent::dev-workbench-tool-execute
                    (agent::obj "name" "lisp-eval" "arguments" arguments)))
                 (tool-call (gethash "result" result))
                 (function (gethash "function" tool-call)))
            (dev-workbench-check
             "execution constructs actual call and crosses installed shadow once"
             (and (string= "executed" (gethash "status" result))
                  (string= "lisp-eval" (gethash "name" function))
                  (search "rdev0-rdev0-test-" (gethash "id" tool-call))
                  (string= "(+ 20 22)"
                           (gethash "form"
                                    (shasht:read-json
                                     (gethash "arguments" function))))
                  (= 1 (- (gethash "shadow_sequence_after" result)
                          (gethash "shadow_sequence_before" result)))
                  (gethash "shadow_sequence_advanced" result)
                  (gethash "execution_attempted" result)
                  (gethash "dev_state_mutation_possible" result)
                  (null (gethash "rollback_performed" result))
                  (null (gethash "production_state_available" result))
                  (null (gethash "external_egress_available" result)))))
          (let ((sequence (gethash "sequence"
                                   (agent::tool-dispatch-shadow-runtime-report))))
            (dolist (bad
                     (list
                      (agent::obj "name" "unknown" "arguments" (agent::obj))
                      (agent::obj "name" "" "arguments" (agent::obj))
                      (agent::obj "name" "lisp-eval" "arguments" "{}")
                      (agent::obj "name" "lisp-eval" "arguments" (agent::obj)
                                  "id" "caller-id")))
              (dev-workbench-check
               "invalid execution request fails before dispatch"
               (dev-workbench-error
                (lambda () (agent::dev-workbench-tool-execute bad)))))
            (dev-workbench-check
             "rejected execution requests do not advance shadow"
             (= sequence (gethash "sequence"
                                  (agent::tool-dispatch-shadow-runtime-report))))))
          (let ((agent::*tools*
                  (vector
                   (agent::obj "type" "function" "function"
                               (agent::obj "name" "lisp-eval"))
                   (agent::obj "type" "function" "function"
                               (agent::obj "name" "lisp-eval")))))
            (dev-workbench-check
             "duplicate advertisement fails before dispatch"
             (dev-workbench-error
              (lambda ()
                (agent::dev-workbench-tool-execute
                 (agent::obj "name" "lisp-eval"
                             "arguments" (agent::obj))))))))
        (agent::tool-dispatch-shadow-runtime-uninstall)
        (agent::tool-dispatch-runtime-uninstall)
        (setf (fdefinition 'agent::execute)
              (lambda (call) (declare (ignore call))
                (error "fixture incumbent condition")))
        (agent::tool-dispatch-runtime-install nil)
        (agent::tool-dispatch-shadow-runtime-install)
        (let ((agent::*tools*
                (vector (agent::obj "type" "function" "function"
                                    (agent::obj "name" "lisp-eval")))))
          (let ((result
                  (agent::dev-workbench-tool-execute
                   (agent::obj "name" "lisp-eval"
                               "arguments" (agent::obj "form" "nil")))))
            (dev-workbench-check
             "incumbent condition returns once and leaves workbench usable"
             (and (string= "condition" (gethash "status" result))
                  (search "fixture incumbent condition"
                          (gethash "message" (gethash "condition" result)))
                  (gethash "shadow_sequence_advanced" result)))))
        (agent::tool-dispatch-shadow-runtime-uninstall)
        (agent::tool-dispatch-runtime-uninstall)
        (setf (fdefinition 'agent::execute) (lambda (tool-call) tool-call))
        (agent::tool-dispatch-runtime-install nil)
        (agent::tool-dispatch-shadow-runtime-install)
        (let ((saved-tools agent::*tools*))
          (unwind-protect
              (let ((active 0) (maximum-active 0)
                    (counter-lock (bt:make-lock "r2c1-concurrency-fixture")))
                (setf agent::*tools*
                      (vector (agent::obj "type" "function" "function"
                                          (agent::obj "name" "search-memory"))
                              (agent::obj "type" "function" "function"
                                          (agent::obj "name" "lisp-eval"))))
                (agent::tool-dispatch-shadow-runtime-uninstall)
                (agent::tool-dispatch-runtime-uninstall)
                (setf (fdefinition 'agent::execute)
                      (lambda (call)
                        (bt:with-lock-held (counter-lock)
                          (incf active)
                          (setf maximum-active (max maximum-active active)))
                        (sleep 0.05)
                        (bt:with-lock-held (counter-lock) (decf active))
                        call))
                (agent::tool-dispatch-runtime-install nil)
                (agent::tool-dispatch-shadow-runtime-install)
                (let ((threads
                        (loop repeat 2 collect
                          (bt:make-thread
                           (lambda ()
                             (agent::dev-workbench-tool-execute
                              (agent::obj "name" "lisp-eval"
                                          "arguments" (agent::obj))))))))
                  (dolist (thread threads) (bt:join-thread thread)))
                (dev-workbench-check
                 "concurrent HTTP adapters serialize actual execute calls"
                 (= maximum-active 1)))
            (agent::tool-dispatch-shadow-runtime-uninstall)
            (agent::tool-dispatch-runtime-uninstall)
            (setf agent::*tools* saved-tools
                  (fdefinition 'agent::execute) (lambda (tool-call) tool-call))
            (agent::tool-dispatch-runtime-install '("search-memory" "lisp-eval"))
            (agent::tool-dispatch-shadow-runtime-install)))
        (let ((source (uiop:read-file-string (namestring (test-source "dev-workbench.lisp")))))
          (dev-workbench-check "offline source has no model invocation"
                               (and (null (search "raw-call-model" source :test #'char-equal))
                                    (null (search "(call-model" source :test #'char-equal))))
          (dev-workbench-check "offline source has no Telegram or delivery call"
                               (and (null (search "telegram-send" source :test #'char-equal))
                                    (null (search "public-outbound" source :test #'char-equal))))
          (dev-workbench-check
           "tool-dispatch endpoint exposes no executable argument field"
           (and (search "/api/test/tool-dispatch" source :test #'char-equal)
                (null (search "tool-dispatch-arguments" source
                              :test #'char-equal))))
          (dev-workbench-check
           "installed-shadow endpoint is GET-only and contains no execution input"
           (and (search "/api/test/tool-dispatch-runtime" source
                        :test #'char-equal)
                (search "request-method*) :get" source :test #'char-equal)
                (null (search "dev-workbench-tool-dispatch-runtime (request"
                              source :test #'char-equal))))
          (dev-workbench-check
           "real execution endpoint has no caller ID callback or raw argument escape"
           (and (search "/api/test/tool-execute" source :test #'char-equal)
                (search "/api/test/tool-catalog" source :test #'char-equal)
                (search "dev_workbench_simulation" source :test #'char-equal)
                (search "delivery_authority\" nil" source :test #'char-equal)
                (null (search "caller-supplied-tool-id" source
                              :test #'char-equal)))))
    (ignore-errors (delete-file manifest-path))))

(format t "RESULT dev-workbench: ~d passed, ~d failed~%"
        *dev-workbench-pass* *dev-workbench-fail*)
(when (plusp *dev-workbench-fail*) (uiop:quit 1))
