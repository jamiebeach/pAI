;;;; seams-tests.lisp -- explicit composition at extension points.
;;;;
;;;; Fixture 1 is the one that matters: under the rename-and-fall-through
;;;; idiom it fails by construction, because rebinding a wrapped function
;;;; discards every wrap above it. That is what makes it evidence rather
;;;; than decoration.
;;;;
;;;; Fixture 4 proves the constraint the design was required to honour --
;;;; that runtime self-modification survives. A mechanism that fixed
;;;; behaviour at load time would pass every other test here and still be
;;;; the wrong answer.

(in-package :agent)

(defvar *seam-passed* 0)
(defvar *seam-failed* 0)
(defvar *seam-trace* '())

(defun seam-check (name condition)
  (if condition
      (progn (incf *seam-passed*) (format t "PASS ~a~%" name))
      (progn (incf *seam-failed*) (format t "FAIL ~a~%" name))))

(defun %trace! (tag) (push tag *seam-trace*))
(defun %trace-order () (reverse *seam-trace*))
(defun %reset-trace () (setf *seam-trace* '()))

;;; ---------------------------------------------------------------- 1
;;; Reload safety: replacing one layer must not disturb the others.

(define-seam demo-seam (x) (%trace! :base) (* x 2))

(register-layer demo-seam outer :order 100
  :function (lambda (next x) (%trace! :outer) (funcall next x)))
(register-layer demo-seam middle :order 200
  :function (lambda (next x) (%trace! :middle) (funcall next x)))
(register-layer demo-seam inner :order 300
  :function (lambda (next x) (%trace! :inner) (funcall next x)))

(%reset-trace)
(let ((result (demo-seam 5)))
  (seam-check "chain runs outermost-first and reaches base"
              (and (equal (%trace-order) '(:outer :middle :inner :base))
                   (= result 10))))

;; Re-register the middle layer only.
(register-layer demo-seam middle :order 200
  :function (lambda (next x) (%trace! :middle-v2) (funcall next x)))

(%reset-trace)
(demo-seam 5)
(seam-check "re-registering one layer replaces only that layer"
            (equal (%trace-order) '(:outer :middle-v2 :inner :base)))

(seam-check "layer count unchanged after replacement"
            (= 3 (length (seam-layers-in-order 'demo-seam))))

;;; ---------------------------------------------------------------- 2
;;; Order is declared, not an accident of registration sequence.

(define-seam order-seam () (%trace! :base) t)
(register-layer order-seam third  :order 300
  :function (lambda (next) (%trace! :third) (funcall next)))
(register-layer order-seam first  :order 100
  :function (lambda (next) (%trace! :first) (funcall next)))
(register-layer order-seam second :order 200
  :function (lambda (next) (%trace! :second) (funcall next)))

(%reset-trace)
(order-seam)
(seam-check "execution follows :order, not registration sequence"
            (equal (%trace-order) '(:first :second :third :base)))

(seam-check "seam-layers-in-order reports execution order"
            (equal (seam-layers-in-order 'order-seam) '(first second third)))

;;; ---------------------------------------------------------------- 3
;;; Introspection: the chain is answerable without running it.

(seam-check "introspection lists exactly the registered layers"
            (null (set-difference (seam-layers-in-order 'demo-seam)
                                  '(outer middle inner))))

;;; ---------------------------------------------------------------- 4
;;; Runtime mutation -- the self-modification guarantee.

(define-seam runtime-seam () (%trace! :base) :base-result)

(%reset-trace)
(runtime-seam)
(seam-check "seam works with no layers" (equal (%trace-order) '(:base)))

;; Register after the fact, exactly as the agent would at runtime.
(register-layer runtime-seam added-later :order 100
  :function (lambda (next) (%trace! :added) (funcall next)))

(%reset-trace)
(runtime-seam)
(seam-check "a layer registered at runtime takes effect on the next call"
            (equal (%trace-order) '(:added :base)))

(unregister-layer 'runtime-seam 'added-later)
(%reset-trace)
(runtime-seam)
(seam-check "unregistering restores prior behaviour"
            (equal (%trace-order) '(:base)))

;;; ---------------------------------------------------------------- 5
;;; A layer may transform, and may decline to call NEXT at all.

(define-seam transform-seam (x) (+ x 1))
(register-layer transform-seam double :order 100
  :function (lambda (next x) (* 2 (funcall next x))))
(seam-check "a layer may transform the result" (= 12 (transform-seam 5)))

(define-seam shortcircuit-seam () (%trace! :base) :reached-base)
(register-layer shortcircuit-seam stop :order 100
  :function (lambda (next) (declare (ignore next)) :short-circuited))
(%reset-trace)
(seam-check "a layer may decline to call NEXT"
            (and (eq :short-circuited (shortcircuit-seam))
                 (null (%trace-order))))

;;; ---------------------------------------------------------------- 6
;;; Redefining the base preserves registered layers. Under the old idiom,
;;; reloading the defining file is precisely what destroyed them.

(define-seam demo-seam (x) (%trace! :base-v2) (* x 3))
(%reset-trace)
(let ((result (demo-seam 5)))
  (seam-check "redefining the base keeps every layer intact"
              (and (equal (%trace-order) '(:outer :middle-v2 :inner :base-v2))
                   (= result 15))))

(format t "~%SEAM TESTS: ~d passed, ~d failed.~%" *seam-passed* *seam-failed*)
(when (plusp *seam-failed*) (sb-ext:quit :unix-status 1))
