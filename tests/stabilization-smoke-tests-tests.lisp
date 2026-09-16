(in-package :agent)

(ql:quickload '(:postmodern :bordeaux-threads) :silent t)

(defmacro with-pg (&body body) `(progn ,@body))
(defvar *smoke-test-pass* 0)
(defvar *smoke-test-fail* 0)
(defun smoke-test-check (name condition)
  (if condition
      (progn (incf *smoke-test-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *smoke-test-fail*) (format t "  FAIL ~a~%" name))))

(load (test-source "stabilization-smoke-tests.lisp"))

(let* ((entry (assoc "grounded-agency-transaction"
                     *stabilization-smoke-probes* :test #'string=))
       (result (funcall (cdr entry))))
  (smoke-test-check "legacy rollback reports grounded probe as not applicable"
                    (and (gethash "ok" result)
                         (gethash "skipped" result)
                         (string= "module-not-loaded" (gethash "reason" result)))))

(format t "~%== executable invocation and broken-handler regression ==~%")
(dolist (entry *stabilization-smoke-probes*)
  (setf (gethash (car entry) *stabilization-smoke-probe-overrides*)
        (lambda () t)))
(let ((report (stabilization-smoke-run)))
  (smoke-test-check "all deterministic probe adapters execute"
                    (and (gethash "ok" report)
                         (= 9 (hash-table-count (gethash "probes" report)))
                         (zerop (gethash "network_calls" report)))))

;; Shallow recovery would see this handler as FBOUNDP. Executable recovery
;; must invoke it and expose the AGENT::E-style bound-but-broken class.
(setf (fdefinition 'bound-but-broken-handler)
      (lambda () (error "AGENT::E fixture")))
(setf (gethash "handlers" *stabilization-smoke-probe-overrides*)
      (symbol-function 'bound-but-broken-handler))
(let* ((report (stabilization-smoke-run))
       (handler (gethash "handlers" (gethash "probes" report))))
  (smoke-test-check "bound handler passes shallow presence check"
                    (fboundp 'bound-but-broken-handler))
  (smoke-test-check "bound-but-broken handler fails executable recovery"
                    (and (not (gethash "ok" report))
                         (not (gethash "ok" handler))
                         (string= "simple-error" (gethash "error_class" handler)))))

(let ((fingerprint (stabilization-state-fingerprint)))
  (smoke-test-check "durable fingerprint excludes process-local threads"
                    (and (gethash "fingerprint" fingerprint)
                         (not (gethash "threads" fingerprint))
                         (gethash "conversation_count" fingerprint))))

(let ((before (obj "conversation_count" 2 "conversation_last_hash" "a"
                   "event_next_id" 6 "memory_counts" (obj "lived" 2)
                   "latest_candidate_id" :null "latest_latent_id" :null
                   "self_model_hash" "b" "soul_hash" "c" "config_hash" "d"))
      (after (obj "conversation_count" 2 "conversation_last_hash" "a"
                  "event_next_id" 9 "memory_counts" (obj "lived" 2)
                  "latest_candidate_id" :null "latest_latent_id" :null
                  "self_model_hash" "b" "soul_hash" "c" "config_hash" "d")))
  (smoke-test-check "fingerprint permits monotonic recovery audit events"
                    (stabilization-fingerprint-equivalent-p before after))
  (setf (gethash "config_hash" after) "changed")
  (smoke-test-check "fingerprint rejects substantive durable drift"
                    (not (stabilization-fingerprint-equivalent-p before after))))

(format t "~%~a passed, ~a failed~%" *smoke-test-pass* *smoke-test-fail*)
(when (plusp *smoke-test-fail*) (sb-ext:exit :code 1))
