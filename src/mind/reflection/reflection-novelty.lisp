;;;; reflection-novelty.lisp -- suppress semantically duplicate reflection writes.
(in-package :agent)
(defparameter *reflection-novelty-similarity-threshold* 0.82d0)
(defparameter *reflection-novelty-cooldown-seconds* (* 60 60))
(defvar *reflection-novelty-cooldown-until* 0)
(defun %reflection-cooldown-p () (< (get-universal-time) *reflection-novelty-cooldown-until*))
(defun %reflection-near-duplicate (content)
  (handler-case
      (let* ((vec (%vector-literal (embed-text content)))
             (row (first (with-pg (pomo:query
                 (format nil "SELECT id, 1 - (embedding <=> '~a'::vector) FROM memory_nodes WHERE kind = 'reflection' AND is_cold = false ORDER BY embedding <=> '~a'::vector LIMIT 1" vec vec)))))
             (id (first row)) (similarity (second row)))
        (and id similarity (>= similarity *reflection-novelty-similarity-threshold*) (values id similarity)))
    (error () nil)))
(register-layer memory-write-node reflection-novelty-suppression :order 100
  ;; Outermost of the two MEMORY-WRITE-NODE layers: a suppressed duplicate
  ;; must never reach candidate-pool nomination (:order 200), matching the
  ;; original rename-and-fall-through order.
  :function (lambda (next &rest args &key kind content &allow-other-keys)
    (if (and (stringp kind) (string= kind "reflection") (stringp content))
        (multiple-value-bind (existing similarity) (%reflection-near-duplicate content)
          (if existing
              (progn
                (when (fboundp 'log-event) (ignore-errors (log-event "reflection-no-novelty" (obj "existing_node_id" existing "similarity" similarity))))
                (setf *reflection-novelty-cooldown-until* (+ (get-universal-time) *reflection-novelty-cooldown-seconds*))
                existing)
              (apply next args)))
        (apply next args))))
(unless (fboundp 'pai-base-tick-handle-consolidate-reflection-novelty)
  (setf (fdefinition 'pai-base-tick-handle-consolidate-reflection-novelty) (fdefinition '%tick-handle-consolidate)))
(defun %tick-handle-consolidate ()
  (if (%reflection-cooldown-p)
      (continuity-buffer-append "Skipped consolidation because the last pass produced no materially new reflection; attention is being left free for a different source of evidence.")
      (funcall 'pai-base-tick-handle-consolidate-reflection-novelty)))
(unless (fboundp 'pai-base-tick-type-weights-reflection-novelty)
  (setf (fdefinition 'pai-base-tick-type-weights-reflection-novelty) (fdefinition '%tick-type-weights)))
(defun %tick-type-weights ()
  (let ((weights (funcall 'pai-base-tick-type-weights-reflection-novelty)))
    (when (%reflection-cooldown-p) (setf (gethash "consolidate" weights) 0.0))
    weights))
