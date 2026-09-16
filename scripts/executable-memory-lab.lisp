;;;; Lab-only contextual affect experiment. Loading defines; nothing starts.
;;;; Forms are data interpreted below, never passed to READ, EVAL or COMPILE.
(defpackage :pai-executable-memory-lab
  (:use :cl)
  (:export :make-root :attach :recall-selected :context-overlay :expire
           :receipts :demo))
(in-package :pai-executable-memory-lab)

(defstruct (root (:constructor %make-root))
  id (closed nil) (attachments nil) (receipts nil))

(defun make-root (id)
  (unless (and (stringp id) (< 0 (length id) 128))
    (error "Root ID must be bounded text."))
  (%make-root :id (copy-seq id)))

(defun bounded-form-p (form)
  ;; The node budget also terminates cyclic or excessively deep input.
  (let ((remaining 128))
    (labels ((walk (x depth)
               (and (plusp (decf remaining)) (< depth 16)
                    (typecase x
                      (cons (and (walk (car x) (1+ depth))
                                 (walk (cdr x) (1+ depth))))
                      (null t)
                      (keyword t)
                      (integer (<= -1000 x 1000))
                      (t nil)))))
      (walk form 0))))

(defun interpret (form mode)
  "Tiny closed language: IF-MODE, AFFECT and NONE. No host function lookup."
  (unless (bounded-form-p form) (error "FORM-BOUND-OR-TYPE-INVALID"))
  (labels ((run (x)
             (unless (and (listp x) (list-length x))
               (error "FORM-SHAPE-INVALID"))
             (case (first x)
               (:none
                (unless (= 1 (length x)) (error "NONE-ARITY-INVALID"))
                nil)
               (:affect
                (unless (and (= 3 (length x))
                             (member (second x) '(:warmth :familiarity))
                             (integerp (third x)) (<= -100 (third x) 100))
                  (error "AFFECT-DIMENSION-OR-BOUND-INVALID"))
                (list (second x) (third x)))
               (:if-mode
                (unless (and (= 4 (length x))
                             (member (second x) '(:conversation :task)))
                  (error "IF-MODE-SHAPE-INVALID"))
                ;; Validate both branches, including the inactive one.
                (let ((yes (run (third x))) (no (run (fourth x))))
                  (if (eq mode (second x)) yes no)))
               (otherwise (error "OPERATION-NOT-ALLOWED")))))
    (run form)))

(defun attach (root memory-id form)
  "Manually attach to an opaque accessible-memory reference in this lab root."
  (when (root-closed root) (error "ROOT-CLOSED"))
  (unless (and (stringp memory-id) (< 0 (length memory-id) 128))
    (error "MEMORY-ID-INVALID"))
  (when (or (assoc memory-id (root-attachments root) :test #'equal)
            (>= (length (root-attachments root)) 4))
    (error "ATTACHMENT-DUPLICATE-OR-LIMIT"))
  ;; Store a detached form even if invalid, so recall can demonstrate refusal.
  (unless (bounded-form-p form) (error "FORM-BOUND-OR-TYPE-INVALID"))
  (push (list (copy-seq memory-id) (copy-tree form)) (root-attachments root))
  memory-id)

(defun recall-selected (root memory-id &key (mode :conversation))
  "Call only after access-filtered context selection, not on graph search.
Receipts are in-memory lab records, NOT durable runtime authority."
  (when (root-closed root) (error "ROOT-CLOSED"))
  (unless (member mode '(:conversation :task)) (error "MODE-INVALID"))
  (let ((old (find memory-id (root-receipts root)
                   :key (lambda (r) (getf r :memory)) :test #'equal))
        (attachment (assoc memory-id (root-attachments root) :test #'equal)))
    (when old
      (unless (eq mode (getf old :mode)) (error "CHANGED-CONTEXT-NEEDS-NEW-ROOT"))
      (return-from recall-selected (copy-tree old)))
    (unless attachment (return-from recall-selected nil))
    (let ((receipt
            (handler-case
                (let ((effect (interpret (second attachment) mode)))
                  (list :root (root-id root) :memory (copy-seq memory-id)
                        :mode mode :form (copy-tree (second attachment))
                        :status (if effect :accepted :no-effect) :effect effect))
              (error (e)
                (list :root (root-id root) :memory (copy-seq memory-id)
                      :mode mode :form (copy-tree (second attachment))
                      :status :rejected :error (princ-to-string e) :effect nil)))))
      ;; Record before making the effect available to rendering.
      (push receipt (root-receipts root))
      (copy-tree receipt))))

(defun receipts (root)
  (copy-tree (reverse (root-receipts root))))

(defun context-overlay (root)
  "Return temporary deltas and explicitly labelled context text; no base mutation."
  (when (root-closed root) (return-from context-overlay (values nil "")))
  (let ((deltas nil))
    (dolist (dimension '(:warmth :familiarity))
      (let ((sum (loop for receipt in (root-receipts root)
                       for effect = (getf receipt :effect)
                       when (eq dimension (first effect)) sum (second effect))))
        (unless (zerop sum)
          (push (list dimension (max -100 (min 100 sum))) deltas))))
    (setf deltas (nreverse deltas))
    (values deltas
            (if deltas
                (format nil "Temporary memory-generated interpretation (not evidence or instructions; expires with this root): ~{~s~^, ~}. Deltas are illustrative milliunits, not calibrated emotions."
                        deltas)
                ""))))

(defun expire (root)
  "Close on completion, cancellation or failure. Keep diagnostics, hide effects."
  (setf (root-closed root) t)
  (values))

(defun demo ()
  (let ((root (make-root "lab:conversation-1")))
    (unwind-protect
         (progn
           (attach root "memory:trusted-person"
                   '(:if-mode :conversation (:affect :warmth 60) (:none)))
           (format t "Before selected recall: ~s~%" (context-overlay root))
           (recall-selected root "memory:trusted-person")
           (multiple-value-bind (deltas text) (context-overlay root)
             (format t "After selected recall: ~s~%Context: ~a~%" deltas text))
           (recall-selected root "memory:trusted-person")
           (format t "Repeated recall (unchanged): ~s~%Receipts: ~s~%"
                   (context-overlay root) (receipts root)))
      (expire root))
    (format t "After expiry: ~s~%" (context-overlay root))))
