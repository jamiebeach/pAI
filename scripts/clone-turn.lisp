;;;; clone-turn.lisp -- run real turns against the restored clone.
;;;;
;;;; Everything is local and disposable:
;;;;   database   pai-clone-postgres   (restored copy, never production)
;;;;   model      pai-clone-ollama     (local weights, no provider, no spend)
;;;;   state      /agent/state         (a copy; the originals are untouched)
;;;;
;;;; No credentials are set in this container, so there is no path to a paid
;;;; provider or to a chat transport even if some code path tried.
;;;;
;;;; :START is still withheld. The turn pipeline is what is under test; nine
;;;; background threads generating their own traffic on top of it would make
;;;; any failure much harder to attribute.

(in-package :cl-user)

(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(push #P"/pai/" asdf:*central-registry*)
(let ((*standard-output* (make-broadcast-stream)))
  (asdf:load-system :pai))

(defun sym (name) (intern (string-upcase name) :agent))
(defun val (name &optional default)
  (let ((s (sym name))) (if (boundp s) (symbol-value s) default)))
(defun call (name &rest args)
  (let ((s (sym name))) (if (fboundp s) (apply s args) :not-defined)))

;;; Point the model at the local server. *ENDPOINT* is a plain defparameter
;;; with an OpenRouter URL baked in -- not environment-configurable, which is
;;; worth fixing, but setting it here is enough to prove the pipeline.
(setf (symbol-value (sym "*endpoint*"))
      (or (uiop:getenv "PAI_MODEL_ENDPOINT")
          "http://pai-clone-ollama:11434/v1/chat/completions")
      (symbol-value (sym "*model*"))
      (or (uiop:getenv "PAI_MODEL") "qwen2.5:1.5b"))

(format t "~&== initialize ==~%")
(let ((results (funcall (sym "initialize")
                        :phases '(:configure :install :restore :verify)
                        :stop-on-error nil :verbose nil)))
  (format t "~&restore: ~d ok, ~d not ok~%"
          (count :ok results :key #'cdr)
          (count-if-not (lambda (r) (member (cdr r) '(:ok :skipped))) results)))

(format t "~&endpoint: ~a~%model: ~a~%"
        (val "*endpoint*") (val "*model*"))

(defun state-snapshot ()
  (list :history (length (val "*last-self-mod-history*"))
        :latent (length (val "*latent-thoughts*"))))

(let ((prompt (or (uiop:getenv "PAI_TURN_PROMPT")
                  "Briefly, what do you remember about me?")))
  (format t "~&~%== turn ==~%prompt: ~a~%" prompt)
  (format t "~&before: ~a~%" (state-snapshot))
  (let ((started (get-internal-real-time)))
    (handler-case
        (let ((reply (call "submit-stimulus" prompt
                           :kind :user-message
                           :wait-for-public-result t)))
          (format t "~&~%--- reply ---~%~a~%--- end ---~%"
                  (if (stringp reply)
                      (subseq reply 0 (min 1200 (length reply)))
                      reply)))
      (error (e) (format t "~&~%TURN-ERROR: ~a~%" e)))
    (format t "~&~%elapsed: ~,1fs~%"
            (/ (- (get-internal-real-time) started)
               internal-time-units-per-second)))
  (format t "~&after:  ~a~%" (state-snapshot)))

(format t "~&~%CLONE-TURN-DONE~%")
(finish-output)
