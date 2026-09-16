(in-package :cl-user)

(defvar *kernel-tool-dispatch-parity-pass* 0)
(defvar *kernel-tool-dispatch-parity-fail* 0)

(defun kernel-tool-dispatch-parity-check (name condition)
  (if condition
      (progn (incf *kernel-tool-dispatch-parity-pass*)
             (format t "PASS ~a~%" name))
      (progn (incf *kernel-tool-dispatch-parity-fail*)
             (format t "FAIL ~a~%" name))))

(defun kernel-tool-dispatch-parity-read-one (path)
  (with-open-file (stream path :direction :input)
    (let ((value (read stream nil :eof))
          (trailing (read stream nil :eof)))
      (unless (eq trailing :eof) (error "Manifest has trailing forms."))
      value)))

(defun kernel-tool-dispatch-parity-slurp (path)
  (with-open-file (stream path :direction :input)
    (let ((text (make-string (file-length stream))))
      (read-sequence text stream)
      text)))

(defun kernel-tool-dispatch-parity-call (name)
  (let ((call (make-hash-table :test #'equal))
        (function (make-hash-table :test #'equal)))
    (setf (gethash "name" function) name
          (gethash "function" call) function)
    call))

(defun kernel-tool-dispatch-parity-vector-list (vector)
  (loop for value across vector collect value))

(defun kernel-tool-dispatch-parity-owner-file-p (path)
  (let ((source (string-downcase (kernel-tool-dispatch-parity-slurp path))))
    (or (search "(defun execute" source)
        (search "(fdefinition 'execute)" source)
        (search "(%timing-install-wrapper 'execute" source))))

(load (test-source "kernel-tool-dispatch-core.lisp"))

(let* ((manifest-path (namestring (test-source "kernel-tool-dispatch-module.sexp")))
       (manifest (kernel-tool-dispatch-parity-read-one manifest-path))
       (expected
         '(("lisp-eval" :lisp-eval "self-mod.lisp")
           ("propose-loop" :propose-loop "self-mod.lisp")
           ("web-search" :web-search "enhancements.lisp")
           ("broadcast-image" :broadcast-image "web.lisp")
           ("upload-reference-image" :upload-reference-image "runware.lisp")
           ("generate-image" :generate-image "runware.lisp")
           ("view-image" :view-image "runware.lisp")
           ("find-state-files" :find-state-files "runware.lisp")
           ("web-fetch" :web-fetch "bounded-work-tools.lisp")
           ("write-deliverable" :write-deliverable "bounded-work-tools.lisp")
           ("read-deliverable" :read-deliverable "bounded-work-tools.lisp")
           ("search-memory" :search-memory "memory-search-tool.lisp")
           ("hold-near-term-thought" :hold-near-term-thought
            "near-term-intention-tool.lisp"))))
  (kernel-tool-dispatch-parity-check "manifest has exactly thirteen tools"
                                     (= (length (getf manifest :tools)) 13))
  (dolist (row expected)
    (destructuring-bind (name handler owner) row
      (multiple-value-bind (tool status resolved-name)
          (pai.kernel.tool-dispatch:resolve-tool
           manifest (kernel-tool-dispatch-parity-call name))
        (kernel-tool-dispatch-parity-check
         (format nil "exact classification parity for ~a" name)
         (and (eq status :resolved)
              (string= resolved-name name)
              (eq (getf tool :handler-id) handler)
              (string= (getf tool :handler-owner) owner))))))
  (let ((unknown (pai.kernel.tool-dispatch:compose-dispatch-plan
                  manifest (kernel-tool-dispatch-parity-call "not-a-tool")))
        (wrong-case (pai.kernel.tool-dispatch:compose-dispatch-plan
                     manifest (kernel-tool-dispatch-parity-call "LISP-EVAL")))
        (malformed (pai.kernel.tool-dispatch:compose-dispatch-plan
                    manifest (make-hash-table :test #'equal))))
    (kernel-tool-dispatch-parity-check
     "unknown name produces explicit failure plan"
     (and (string= (gethash "status" unknown) "unknown-tool")
          (eq (gethash "handler_id" unknown) :null)))
    (kernel-tool-dispatch-parity-check
     "matching is exact and case-sensitive"
     (string= (gethash "status" wrong-case) "unknown-tool"))
    (kernel-tool-dispatch-parity-check
     "malformed call produces explicit failure plan"
     (and (string= (gethash "status" malformed) "malformed-tool-call")
          (eq (gethash "external_name" malformed) :null))))
  (let* ((base (pai.kernel.tool-dispatch:compose-dispatch-plan
                manifest (kernel-tool-dispatch-parity-call "lisp-eval")))
         (proposal (pai.kernel.tool-dispatch:compose-dispatch-plan
                    manifest (kernel-tool-dispatch-parity-call "propose-loop"))))
    (kernel-tool-dispatch-parity-check
     "timing surrounds dispatch"
     (equal (kernel-tool-dispatch-parity-vector-list
             (gethash "around_stages" base)) '("timing")))
    (kernel-tool-dispatch-parity-check
     "call recording precedes handler"
     (equal (kernel-tool-dispatch-parity-vector-list
             (gethash "before_stages" base)) '("tool-call-event")))
    (kernel-tool-dispatch-parity-check
     "appraisal then result recording follow handler"
     (equal (kernel-tool-dispatch-parity-vector-list
             (gethash "after_stages" base))
            '("modulator-appraisal" "tool-result-event")))
    (kernel-tool-dispatch-parity-check
     "proposal provenance applies only to propose-loop"
     (and (equal (kernel-tool-dispatch-parity-vector-list
                  (gethash "around_stages" proposal))
                 '("timing" "proposal-provenance"))
          (not (find "proposal-provenance"
                     (kernel-tool-dispatch-parity-vector-list
                      (gethash "around_stages" base))
                     :test #'string=)))))
  (let ((observed nil))
    ;; ASDF replaced the flat /workspace/state load tree. Resolve the declared
    ;; audit inventory through the same source index used by every suite.
    (dolist (name (getf manifest :execute-audit-files))
      (let ((path (test-source name)))
        (when (probe-file path)
          (push (file-namestring path) observed))))
    (setf observed (sort (remove-duplicates observed :test #'string=)
                         #'string<))
    (kernel-tool-dispatch-parity-check
     "static inventory covers every production execute owner and installer"
     (equal observed
            (sort (copy-list (getf manifest :execute-audit-files)) #'string<))))
  (let* ((registry-source
           (kernel-tool-dispatch-parity-slurp
            (namestring (test-source "wrap-chain-registry.lisp"))))
         (system-source
           (kernel-tool-dispatch-parity-slurp
            (merge-pathnames "pai.asd" *pai-root*)))
         (boot-event (search "event-log" system-source :from-end t))
         (boot-provenance
           (search "self-mod-provenance" system-source :from-end t))
         (boot-memory
           (search "memory-search-tool" system-source :from-end t)))
    (kernel-tool-dispatch-parity-check
     "retired manual registry has no execute wrap chain"
     (null (search "(obj \"execute\"" registry-source)))
    (kernel-tool-dispatch-parity-check
     "ASDF composition places event and provenance before memory"
     (and boot-event boot-provenance boot-memory
          (< boot-event boot-provenance boot-memory))))
  (let ((approved-consumers '("kernel-tool-dispatch-shadow.lisp"
                              "kernel-tool-dispatch-bootstrap.lisp"))
        (system-source
          (string-downcase
           (kernel-tool-dispatch-parity-slurp
            (merge-pathnames "pai.asd" *pai-root*)))))
    (kernel-tool-dispatch-parity-check
     "the facade and bootstrap retain their explicit kernel references"
     (every (lambda (consumer)
              (let ((source
                      (string-downcase
                       (kernel-tool-dispatch-parity-slurp
                        (namestring (test-source consumer))))))
                (if (string= consumer "kernel-tool-dispatch-shadow.lisp")
                    (search "pai.kernel.tool-dispatch" source)
                    (search "kernel-tool-dispatch-module.sexp" source))))
            approved-consumers))
    (kernel-tool-dispatch-parity-check
     "ASDF loads production dispatch through the conditional bootstrap"
     (and (search "kernel-tool-dispatch-bootstrap" system-source)
          (null (search "kernel-tool-dispatch-shadow-runtime" system-source))
          (null (search "kernel-tool-dispatch-runtime" system-source))))
    (kernel-tool-dispatch-parity-check
     "only the exact dev runtime adds the approved shadow installer"
     (let ((runtime
             (string-downcase
              (kernel-tool-dispatch-parity-slurp
               (namestring (test-source "kernel-tool-dispatch-shadow-runtime.lisp")))))
           (recovery
             (string-downcase
              (kernel-tool-dispatch-parity-slurp
               (namestring (test-source "recovery-health.lisp"))))))
       (and (search "(fdefinition 'execute)" runtime)
            (search "tool-dispatch-shadow-runtime-uninstall" runtime)
            (null (search "kernel-tool-dispatch-shadow-runtime" recovery)))))
    (kernel-tool-dispatch-parity-check
     "is a generic bounded dev runtime with no production or recovery load"
     (let ((runtime
             (string-downcase
              (kernel-tool-dispatch-parity-slurp
               (namestring (test-source "kernel-tool-dispatch-runtime.lisp")))))
           (recovery
             (string-downcase
              (kernel-tool-dispatch-parity-slurp
               (namestring (test-source "recovery-health.lisp"))))))
       (and (search "tool-dispatch-shadow-inspect" runtime)
            (search "tool-handler-binding-lookup" runtime)
            (search "call-with-tool-event-observation" runtime)
            (search "observe-tool-result-appraisal" runtime)
            (search "call-with-proposal-provenance" runtime)
            (search "call-with-timing-span" runtime)
            (null (search "postgres" runtime))
            (null (search "openrouter" runtime))
            (null (search "kernel-tool-dispatch-runtime" recovery)))))
    (kernel-tool-dispatch-parity-check
     "incumbent wrappers expose all observer and handler ports"
     (let ((event (string-downcase
                   (kernel-tool-dispatch-parity-slurp
                    (namestring (test-source "event-log.lisp")))))
           (modulator (string-downcase
                       (kernel-tool-dispatch-parity-slurp
                        (namestring (test-source "modulator.lisp")))))
           (memory (string-downcase
                    (kernel-tool-dispatch-parity-slurp
                     (namestring (test-source "memory-search-tool.lisp")))))
           (enhancements (string-downcase
                          (kernel-tool-dispatch-parity-slurp
                           (namestring (test-source "enhancements.lisp")))))
           (web (string-downcase
                 (kernel-tool-dispatch-parity-slurp
                  (namestring (test-source "web-terminal.lisp")))))
           (runware (string-downcase
                     (kernel-tool-dispatch-parity-slurp
                      (namestring (test-source "runware.lisp")))))
           (bounded (string-downcase
                     (kernel-tool-dispatch-parity-slurp
                      (namestring (test-source "bounded-work-tools.lisp")))))
           (near-term (string-downcase
                       (kernel-tool-dispatch-parity-slurp
                        (namestring (test-source "near-term-intention-tool.lisp")))))
           (provenance (string-downcase
                        (kernel-tool-dispatch-parity-slurp
                         (namestring (test-source "self-mod-provenance.lisp")))))
           (self-mod (string-downcase
                      (kernel-tool-dispatch-parity-slurp
                       (namestring (test-source "self-mod.lisp"))))))
       (and (search "(defun call-with-tool-event-observation" event)
            (search "(call-with-tool-event-observation" event :from-end t)
            (search "(defun observe-tool-result-appraisal" modulator)
            (search "(observe-tool-result-appraisal result)" modulator)
            (search "(defun memory-search-tool-handle" memory)
            (search "(memory-search-tool-handle tool-call)" memory)
            (search "(defun self-mod-tool-handle" self-mod)
            (search "(defun pai-enhancements-tool-handle" enhancements)
            (search "(defun web-terminal-tool-handle" web)
            (search "(defun runware-tool-handle" runware)
            (search "(defun bounded-work-tool-handle" bounded)
            (search "(defun near-term-intention-tool-handle" near-term)
            (search "(defun call-with-proposal-provenance" provenance))))))

(format t "RESULT kernel-tool-dispatch-classification-parity: ~d passed, ~d failed~%"
        *kernel-tool-dispatch-parity-pass* *kernel-tool-dispatch-parity-fail*)
(when (plusp *kernel-tool-dispatch-parity-fail*) (sb-ext:exit :code 1))
