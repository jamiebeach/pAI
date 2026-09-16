;;;; clone-tools.lisp -- does a turn actually dispatch a tool?
;;;;
;;;; The one part of the turn pipeline the clone runs have never exercised.
;;;; A 1.5B model never emitted a tool_calls response, so EXECUTE and the
;;;; whole dispatch chain went untouched: the turns proved the pipeline up to
;;;; the model and back, and nothing past the branch where the model asks for
;;;; a tool.
;;;;
;;;; Prompts here are chosen to make a tool the obvious move -- arithmetic the
;;;; model should compute rather than guess, and a question about its own
;;;; state. Whether it takes the hint is a property of the model; whether the
;;;; dispatch works once it does is the property under test.

(in-package :cl-user)
(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(push #P"/pai/" asdf:*central-registry*)
(let ((*standard-output* (make-broadcast-stream))) (asdf:load-system :pai))

(defun sym (name) (intern (string-upcase name) :agent))
(defun val (name &optional default)
  (let ((s (sym name))) (if (boundp s) (symbol-value s) default)))
(defun call (name &rest args)
  (let ((s (sym name))) (if (fboundp s) (apply s args) :not-defined)))

(format t "~&endpoint: ~a~%model: ~a~%api-key: ~a~%"
        (val "*endpoint*") (val "*model*")
        (if (val "*api-key*") "set" "unset (cannot reach a paid provider)"))

(funcall (sym "initialize") :phases '(:configure :install :restore :verify)
                            :stop-on-error nil :verbose nil)

(format t "~&~%advertised tools: ~d~%"
        (length (val "*tools*" #())))
(loop for tool across (val "*tools*" #())
      for fn = (gethash "function" tool)
      do (format t "~&  ~a~%" (and fn (gethash "name" fn))))

;;; Count dispatches by observing EXECUTE rather than by trusting the reply
;;; text. A model that says "I used the calculator" has not used anything.
(defvar *dispatched* '())
(let ((incumbent (fdefinition (sym "execute"))))
  (setf (fdefinition (sym "execute"))
        (lambda (tool-call)
          (let* ((fn (gethash "function" tool-call))
                 (name (and fn (gethash "name" fn))))
            (push name *dispatched*)
            (format t "~&  [dispatch] ~a~%" name)
            (finish-output))
          (funcall incumbent tool-call))))

(dolist (prompt (list (or (uiop:getenv "PAI_TOOL_PROMPT")
                          "Use your lisp-eval tool to compute (* 3571 4297). Report only the number.")
                      "What is currently in your working memory? Use a tool to check rather than guessing."))
  (format t "~&~%== ~a ==~%" prompt)
  (setf *dispatched* '())
  (handler-case
      (let ((reply (call "submit-stimulus" prompt
                         :kind :user-message
                         :wait-for-public-result t)))
        (format t "~&reply: ~a~%"
                (if (stringp reply) (subseq reply 0 (min 500 (length reply))) reply)))
    (error (e) (format t "~&TURN-ERROR: ~a~%" e)))
  (format t "~&tools dispatched: ~a~%"
          (if *dispatched* (reverse *dispatched*) "NONE")))

(format t "~&~%expected answer for the arithmetic: ~a~%" (* 3571 4297))
(format t "~&CLONE-TOOLS-DONE~%")
(finish-output)
