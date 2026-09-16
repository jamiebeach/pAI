(in-package :agent)

(ql:quickload :postmodern :silent t)

(defvar *wrapper-test-pass* 0)
(defvar *wrapper-test-fail* 0)
(defun wrapper-test-check (name condition)
  (if condition
      (progn (incf *wrapper-test-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *wrapper-test-fail*) (format t "  FAIL ~a~%" name))))

;; MEMORY-WRITE-NODE is now a seam (P0c item 3, 2026-08-16). Install a
;; counting stub as its BASE via DEFINE-SEAM, not a plain DEFUN -- a DEFUN
;; would overwrite the seam's dispatcher instead of supplying its base, so
;; calls would bypass the chain entirely and neither layer below would ever
;; run. See lisp-eval-safety-tests.lisp for the same fix on the first
;; converted chain.
(define-seam memory-write-node (&rest args) (declare (ignore args)) :old-base)

;; Everything else here still uses the original rename-and-fall-through
;; idiom -- SOUL-CANDIDATE-POOL.LISP also wraps SELF-MODEL-PROPOSE-REVISION
;; and %TICK-HANDLE-MAINTENANCE that way, and REFLECTION-NOVELTY.LISP wraps
;; %TICK-HANDLE-CONSOLIDATE and %TICK-TYPE-WEIGHTS that way; only their
;; MEMORY-WRITE-NODE layer converted. Stubs for those chains are unchanged.
(defun self-model-propose-revision (&rest args) (declare (ignore args)) nil)
(defun %tick-handle-maintenance () nil)
(defun %tick-handle-consolidate () nil)
(defun %tick-type-weights () (obj))
(defun continuity-buffer-append (text) text)
(defun embed-text (text) (declare (ignore text)) '(1.0d0))
(defun with-pg-test-placeholder () nil)

(load (test-source "soul-candidate-pool.lisp"))
(load (test-source "reflection-novelty.lisp"))
(wrapper-test-check "initial complete chain reaches the seam base"
                    (eq :old-base (memory-write-node :kind "thought" :content "x")))
(wrapper-test-check "both layers registered, outer first"
                    (equal '(reflection-novelty-suppression candidate-pool-nomination)
                           (seam-layers-in-order 'memory-write-node)))

;; Simulate memory-nodes.lisp replacing the base (DEFINE-SEAM against an
;; existing seam replaces only the base and leaves registered layers
;; intact -- the property the old test called "reload recaptures the new
;; base," now provided structurally rather than by a saved-original
;; convention each wrapper had to reimplement).
(define-seam memory-write-node (&rest args) (declare (ignore args)) :new-base)
(wrapper-test-check "base swap alone reaches the new base through both layers"
                    (eq :new-base (memory-write-node :kind "thought" :content "x")))
(wrapper-test-check "base swap alone does not touch registered layers"
                    (equal '(reflection-novelty-suppression candidate-pool-nomination)
                           (seam-layers-in-order 'memory-write-node)))

;; Reloading the outer wrapper alone must replace only its own layer entry.
(load (test-source "reflection-novelty.lisp"))
(wrapper-test-check "single-layer reload does not duplicate or drop layers"
                    (equal '(reflection-novelty-suppression candidate-pool-nomination)
                           (seam-layers-in-order 'memory-write-node)))
(wrapper-test-check "single-layer reload leaves the chain callable end to end"
                    (eq :new-base (memory-write-node :kind "thought" :content "x")))

(format t "~%~a passed, ~a failed~%" *wrapper-test-pass* *wrapper-test-fail*)
(when (plusp *wrapper-test-fail*) (sb-ext:exit :code 1))
