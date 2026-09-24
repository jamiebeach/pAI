;;;; seams.lisp -- explicit composition at extension points.
;;;;
;;;; Replaces the rename-and-fall-through wrap idiom. A wrap already IS
;;;; middleware; it was merely implemented by mutating a global function
;;;; binding instead of composing a list. This makes the list real.
;;;;
;;;;   (define-seam auto-turn (messages)
;;;;     "Base behaviour."
;;;;     ...body...)
;;;;
;;;;   (register-layer auto-turn timing :order 100
;;;;     (lambda (next messages)
;;;;       (with-timing (funcall next messages))))
;;;;
;;;; Each layer receives NEXT and may call it, transform its arguments or
;;;; result, wrap it, or decline to call it -- the full expressive power of
;;;; the idiom it replaces, including short-circuiting.
;;;;
;;;; WHAT THIS FIXES, none of which a registry could:
;;;;
;;;;   Reload dropping layers. Re-registering by name replaces ONE entry.
;;;;   The old idiom rebound the global function, silently discarding every
;;;;   wrap layered above it -- a failure that cost user-facing behaviour
;;;;   twice in a single day.
;;;;
;;;;   Order as load-sequence accident. :ORDER is declared data.
;;;;
;;;;   Liveness undecidable by reading. The chain resolves by NAME at call
;;;;   time. Nothing captures a layer as a function object, which is how
;;;;   %TICK-HANDLE-EXPLORE ended up with a handler table holding a
;;;;   definition taken before its own later redefinition -- leaving nobody
;;;;   able to say which one actually ran.
;;;;
;;;; WHAT THIS DELIBERATELY PRESERVES: registration works at runtime. The
;;;; agent can add, replace, inspect and remove its own layers. Self-
;;;; modification is the product; this makes it legible rather than
;;;; removing it.

(in-package :agent)

(export '(define-seam register-layer unregister-layer
          seam-layers seam-report seam-defined-p seam-has-layers-p))

(defstruct (seam (:constructor %make-seam))
  name
  base                ; innermost function: the original body
  (layers '())        ; alist of name -> (order . function)
  (cache nil)         ; composed chain, invalidated on any change
  (cache-valid nil))

(defvar *seams* (make-hash-table :test #'eq)
  "Seam name -> SEAM. The chain is looked up here at call time; no caller
   ever holds a layer function object, which is what made the previous
   idiom's liveness unanswerable.")

(defun seam-defined-p (name) (nth-value 1 (gethash name *seams*)))

(defun seam-has-layers-p (name)
  "True when NAME has at least one registered extension layer.
The base implementation alone is deliberately not treated as an available
adapter capability: bases commonly fail closed while a deployment decides
which concrete authority ports to register."
  (let ((seam (gethash name *seams*)))
    (and seam (not (null (seam-layers seam))))))

(defun %seam (name)
  (or (gethash name *seams*)
      (error "No seam named ~s. Declare it with DEFINE-SEAM." name)))

(defun %compose-seam (seam)
  "Build the call chain: lowest :ORDER outermost, base innermost."
  (let ((sorted (sort (copy-list (seam-layers seam)) #'< :key #'cadr)))
    ;; :FROM-END T calls the reducer as (element accumulator), so the entry
    ;; comes first and the already-composed inner chain second.
    (reduce (lambda (entry next)
              (let ((fn (cddr entry)))
                (lambda (&rest args) (apply fn next args))))
            sorted
            :initial-value (seam-base seam)
            :from-end t)))

(defun %seam-chain (seam)
  (unless (seam-cache-valid seam)
    (setf (seam-cache seam) (%compose-seam seam)
          (seam-cache-valid seam) t))
  (seam-cache seam))

(defun %invalidate (seam)
  (setf (seam-cache-valid seam) nil))

(defun seam-invoke (name &rest args)
  (apply (%seam-chain (%seam name)) args))

(defmacro define-seam (name lambda-list &body body)
  "Declare NAME an extension point whose base behaviour is BODY.

   Defines NAME as an ordinary function, so existing call sites are
   unchanged. Re-evaluating DEFINE-SEAM replaces only the base; registered
   layers survive -- unlike redefining a wrapped function, which discarded
   everything above it."
  (let ((docstring (when (and (stringp (first body)) (rest body))
                     (first body)))
        (real-body (if (and (stringp (first body)) (rest body))
                       (rest body) body)))
    `(progn
       (let ((existing (gethash ',name *seams*))
             (base (lambda ,lambda-list ,@real-body)))
         (if existing
             (setf (seam-base existing) base)
             (setf (gethash ',name *seams*)
                   (%make-seam :name ',name :base base)))
         (%invalidate (gethash ',name *seams*)))
       (defun ,name (&rest args) ,@(when docstring (list docstring))
         (apply #'seam-invoke ',name args))
       ',name)))

(defmacro register-layer (seam-name layer-name &key (order 500) function)
  "Register FUNCTION as a named layer on SEAM-NAME.

   FUNCTION takes (NEXT &rest args). Lower ORDER runs further out. A layer
   registered under an existing name REPLACES that entry and nothing else,
   which is the property the previous idiom lacked.

   Valid at runtime: the agent may register its own layers."
  `(%register-layer ',seam-name ',layer-name ,order ,function))

(defun %register-layer (seam-name layer-name order function)
  (let ((seam (%seam seam-name)))
    (setf (seam-layers seam)
          (cons (list* layer-name order function)
                (remove layer-name (seam-layers seam) :key #'car)))
    (%invalidate seam)
    layer-name))

(defun unregister-layer (seam-name layer-name)
  "Remove one layer. Others are untouched."
  (let ((seam (%seam seam-name)))
    (setf (seam-layers seam)
          (remove layer-name (seam-layers seam) :key #'car))
    (%invalidate seam)
    layer-name))

(defun seam-layers-in-order (seam-name)
  "Layer names outermost-first -- the exact execution order."
  (mapcar #'car (sort (copy-list (seam-layers (%seam seam-name)))
                      #'< :key #'cadr)))

(defun seam-report (&optional name)
  "What is registered where. Answers by inspection the question the wrap
   idiom could only answer by experiment."
  (flet ((one (n)
           (let ((s (gethash n *seams*)))
             (format t "~&~a (~d layer~:p)~%" n (length (seam-layers s)))
             (dolist (entry (sort (copy-list (seam-layers s)) #'< :key #'cadr))
               (format t "    ~4d  ~a~%" (cadr entry) (car entry)))
             (format t "    base~%"))))
    (if name
        (one name)
        (let ((names '()))
          (maphash (lambda (k v) (declare (ignore v)) (push k names)) *seams*)
          (dolist (n (sort names #'string< :key #'symbol-name)) (one n))))))
