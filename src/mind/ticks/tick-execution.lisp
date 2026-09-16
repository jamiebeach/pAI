;;;; tick-execution.lisp -- reversible tick orchestration.

(in-package :agent)

(defvar *tick-execution-installed-wrapper* nil)
(let* ((current (fdefinition 'tick-once))
       ;; A live install happens after observability. Capture its underlying
       ;; base, then let observability recapture this new wrapper; otherwise
       ;; legacy mode would become timing -> execution -> old timing -> base.
       (effective
         (if (and (fboundp '%timing-tick-once)
                  (eq current (fdefinition '%timing-tick-once))
                  (fboundp 'pai-base-tick-once-timing))
             (fdefinition 'pai-base-tick-once-timing)
             current)))
  (unless (and *tick-execution-installed-wrapper*
               (eq current *tick-execution-installed-wrapper*))
    (setf (fdefinition 'pai-base-tick-once-stab04) effective)))

(defun %tick-execution-preempted-p ()
  (and (boundp '*v2-turn-in-flight*)
       (symbol-value '*v2-turn-in-flight*)))

(defun %tick-execution-paused-p ()
  (and (boundp '*autonomous-write-mode*)
       (eq (symbol-value '*autonomous-write-mode*) :paused)))

(defun %tick-execution-result-cost (result accumulator legacy-executed)
  (setf (gethash "cost" result) (first accumulator)
        (gethash "prompt_tokens" result) (second accumulator)
        (gethash "completion_tokens" result) (third accumulator)
        (gethash "legacy_executed" result) (if legacy-executed t nil)
        (gethash "proposal_status" result) (gethash "status" result))
  result)

(defun tick-once ()
  "Legacy is byte-for-byte rollback. Shadow runs the final legacy handler plus
the typed proposal path without new writes. Enforced runs only validated data."
  (cond
    ((%tick-execution-paused-p)
     (obj "status" "skipped" "reason" "autonomous-writes-paused"
          "write_count" 0))
    ((or (not (boundp '*cognitive-generation-mode*))
         (eq *cognitive-generation-mode* :legacy))
     (funcall 'pai-base-tick-once-stab04))
    (t
     (bt:with-lock-held (*tick-lock*)
       (let* ((selected
                (or *tick-forced-type*
                    (and (fboundp 'near-term-intention-due-p)
                         (funcall 'near-term-intention-due-p)
                         "near-term-intention")
                    (%tick-select-type)))
              (generation (format nil "tick-exec-~a-~a" (get-universal-time)
                                  (random 1000000)))
              (*tick-cost-accumulator* (list 0.0d0 0 0)))
         (%tick-record-run)
         (tick-terminal-call
          selected
          (lambda ()
            (cond
              ((%tick-execution-preempted-p)
               (obj "status" "skipped" "reason" "user-turn-preemption"
                    "write_count" 0))
              ((string= selected "near-term-intention")
               (%tick-execution-result-cost
                (funcall 'near-term-intention-process-due)
                *tick-cost-accumulator* nil))
              ((eq *cognitive-generation-mode* :shadow)
               (let ((handler (gethash selected *tick-handlers*)))
                 (unless handler (error "Missing legacy tick handler ~a" selected))
                 (funcall handler)
                 (%tick-execution-result-cost
                  (tick-execute-proposal selected *tick-terminal-start-event-id*
                                         :mode :shadow-only)
                  *tick-cost-accumulator* t)))
              (t
               (%tick-execution-result-cost
                (tick-execute-proposal selected *tick-terminal-start-event-id*
                                       :mode *autonomous-write-mode*)
                *tick-cost-accumulator* nil))))
          :generation-id generation))))))

(setf *tick-execution-installed-wrapper* (fdefinition 'tick-once))
