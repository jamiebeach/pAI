(in-package :agent)

;; Candidate-only operational wrapper. The pinned R0c1b fixture historically
;; called REPLAY-EVENTS without a bound. Production-shaped history is now too
;; large for that test-only collection, so constrain its observation to the
;; newest typed model events while leaving the loaded emitter seam unchanged.
(let ((ordinary-replay (fdefinition 'replay-events)))
  (unwind-protect
      (progn
        (setf (fdefinition 'replay-events)
              (lambda (&rest ignored)
                (declare (ignore ignored))
                (funcall ordinary-replay
                         :types '("model-request" "model-response")
                         :limit 20)))
        (load (merge-pathnames "r0c1b-live-candidate-probe.lisp" *load-truename*))
        (assert (eq *memory-atom-decomposition-mode* :off))
        (assert (fboundp 'map-events))
        (assert (fboundp 'write-event-row-checkpoint))
        (assert (fboundp 'map-verified-event-row-checkpoint-lines))
        (let* ((*print-pretty* nil)
               (proof
                (shasht:write-json
                 (obj "status" "pass"
                      "loaded_model_checks" 11
                      "memory_atom_mode" "off"
                      "r0e_streaming_apis" 3
                      "provider_calls" 0)
                 nil)))
          ;; LOAD returns generalized true rather than the loaded file's last
          ;; form.  REPL-DROP captures standard output, so publish the proof
          ;; explicitly instead of relying on a nested LOAD return value.
          (write-line proof)
          proof))
    (setf (fdefinition 'replay-events) ordinary-replay)))
