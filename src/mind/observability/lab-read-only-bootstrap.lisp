;;;; Minimal disposable bootstrap for PAI-LAB read-only-live scenarios.

(in-package :agent)

(defun %pai-lab-ascii-json (text)
  "Encode non-ASCII JSON characters as escapes before crossing native stdout."
  (with-output-to-string (stream)
    (loop for character across text
          for code = (char-code character)
          do (cond
               ((<= code 127) (write-char character stream))
               ((<= code #xffff)
                (format stream "\\u~4,'0X" code))
               (t
                (let* ((value (- code #x10000))
                       (high (+ #xd800 (ash value -10)))
                       (low (+ #xdc00 (logand value #x3ff))))
                  (format stream "\\u~4,'0X\\u~4,'0X" high low)))))))

(ql:quickload '(:bordeaux-threads :dexador :postmodern :shasht :ironclad :babel)
              :silent t)

(uiop:chdir #P"/agent/state/")

(let* ((scenario-path (uiop:getenv "PAI_LAB_SCENARIO"))
       (suite (and (stringp scenario-path) (plusp (length scenario-path))
                   (shasht:read-json (uiop:read-file-string scenario-path))))
       (scenarios (and (hash-table-p suite) (gethash "scenarios" suite)))
       (modes (and (vectorp scenarios)
                   (map 'list (lambda (scenario)
                                (and (hash-table-p scenario)
                                     (gethash "mode" scenario)))
                        scenarios)))
       (memory-required (member "read-only-live" modes :test #'string=))
       (deliverable-required
         (member "read-only-deliverable" modes :test #'string=)))
  (when memory-required
    (load "/agent/state/memory-nodes.lisp")
    (load "/agent/state/typed-retrieval.lisp")

    ;; CONTEXT-PROJECTION owns the production selector but also installs an
    ;; AUTO-TURN wrapper when loaded. The lab supplies a fail-closed base so
    ;; loading that exact selector cannot make a public/model turn reachable.
    (unless (fboundp 'auto-turn)
      (defun auto-turn (&rest arguments)
        (declare (ignore arguments))
        (error "AUTO-TURN is structurally unavailable in read-only the Lab.")))
    (load "/agent/state/context-projection.lisp")
    (load "/agent/state/context-curator-candidate.lisp")
    (load "/agent/state/turn-bundle-retrieval.lisp"))
  (when deliverable-required
    (load "/agent/state/bounded-work-tools.lisp"))
  (load "/agent/state/memory-atom-candidate.lisp")
  (load "/agent/state/lab.lisp"))

(let ((responses (uiop:getenv "PAI_LAB_CURATOR_RESPONSES")))
  (when (and (stringp responses) (plusp (length responses)))
    (pai-lab-load-curator-response-file responses)))

(let ((responses (uiop:getenv "PAI_LAB_ATOM_RESPONSES")))
  (when (and (stringp responses) (plusp (length responses)))
    (pai-lab-load-atom-response-file responses)))

(let ((scenario (uiop:getenv "PAI_LAB_SCENARIO"))
      (reveal (string= "1" (or (uiop:getenv "PAI_LAB_REVEAL_PRIVATE") ""))))
  (unless (and (stringp scenario) (plusp (length scenario)))
    (error "PAI_LAB_SCENARIO is required."))
  (let* ((*pai-lab-reveal-private-content* reveal)
         (result (pai-lab-run-scenario-file scenario))
         (json (remove-if (lambda (character)
                            (member character '(#\Newline #\Return)))
                          (shasht:write-json result nil)))
         (ascii-json (%pai-lab-ascii-json json)))
    (format t "~&PAI_LAB_RESULT=~a~%" ascii-json)
    (sb-ext:exit :code (if (string= "passed" (gethash "status" result))
                           0 1))))
