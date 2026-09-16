;;;; clone-start.lisp -- full lifecycle: all five phases, then multiple turns.
;;;;
;;;; This is the first thing to run :START, which launches nine background
;;;; threads: tick loop, drives, modulator, watchdogs, persistence heartbeat,
;;;; turn capture, backup, drift monitor, repl drop.
;;;;
;;;; Contained by construction rather than by care:
;;;;   database   the restored clone, on its own network
;;;;   model      local weights in pai-clone-ollama
;;;;   state      a copy; the originals are untouched
;;;;   credentials none are set, so no transport and no paid provider
;;;;
;;;; The tick loop generates its own model traffic on a timer. That is the
;;;; point -- it is what has never been exercised -- but it means output from
;;;; a turn may interleave with autonomous activity. Counts are sampled before
;;;; and after rather than attributed to a specific turn.
;;;;
;;;; Usage:
;;;;   PAI_TURNS=3 PAI_SETTLE=45 sbcl --load scripts/clone-start.lisp

(in-package :cl-user)
(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(push #P"/pai/" asdf:*central-registry*)
(let ((*standard-output* (make-broadcast-stream))) (asdf:load-system :pai))

(defun sym (name) (intern (string-upcase name) :agent))
(defun val (name &optional default)
  (let ((s (sym name))) (if (boundp s) (symbol-value s) default)))
(defun call (name &rest args)
  (let ((s (sym name))) (if (fboundp s) (apply s args) :not-defined)))
(defun env-int (name default)
  (let ((v (uiop:getenv name)))
    (or (and v (ignore-errors (parse-integer v))) default)))

(setf (symbol-value (sym "*endpoint*"))
      (or (uiop:getenv "PAI_MODEL_ENDPOINT")
          "http://pai-clone-ollama:11434/v1/chat/completions")
      (symbol-value (sym "*model*"))
      (or (uiop:getenv "PAI_MODEL") "qwen2.5:1.5b"))


;;; WITH-PG is a macro, so it cannot be called through FUNCALL. Count through
;;; a fresh connection using the same parameters the system uses.
(defun node-count-direct ()
  (handler-case
      (pomo:with-connection (list (val "*pg-database*") (val "*pg-user*")
                                  (val "*pg-password*") (val "*pg-host*")
                                  :port (val "*pg-port*"))
        (first (pomo:query "SELECT count(*) FROM memory_nodes" :list)))
    (error (e) (format nil "unavailable: ~a" e))))

(format t "~&== initialize: all five phases ==~%")
(let* ((results (funcall (sym "initialize") :stop-on-error nil :verbose nil))
       (bad (remove-if (lambda (r) (member (cdr r) '(:ok :skipped))) results)))
  (format t "~&~d actions, ~d ok, ~d failed~%"
          (length results) (count :ok results :key #'cdr) (length bad))
  (dolist (row bad) (format t "~&  FAILED ~a: ~a~%" (car row) (cdr row))))

(format t "~&~%live threads: ~d~%" (length (bt:all-threads)))
(dolist (th (bt:all-threads)) (format t "~&  ~a~%" (bt:thread-name th)))

(let ((turns (env-int "PAI_TURNS" 3))
      (settle (env-int "PAI_SETTLE" 45))
      (prompts '("What do you remember about me?"
                 "What have you been thinking about lately?"
                 "Tell me one thing you learned recently.")))
  (format t "~&~%memory nodes before: ~a~%" (node-count-direct))
  (dotimes (i turns)
    (let ((prompt (nth (mod i (length prompts)) prompts)))
      (format t "~&~%== turn ~d: ~a ==~%" (1+ i) prompt)
      (handler-case
          (let ((reply (call "submit-stimulus" prompt
                             :kind :user-message
                             :wait-for-public-result t)))
            (format t "~&reply: ~a~%"
                    (if (stringp reply) (subseq reply 0 (min 200 (length reply))) reply)))
        (error (e) (format t "~&TURN-ERROR: ~a~%" e)))
      (format t "~&history=~a nodes=~a~%"
              (length (val "*last-self-mod-history*")) (node-count-direct))))

  (format t "~&~%== settling ~ds for background workers ==~%" settle)
  (finish-output)
  (sleep settle)
  (format t "~&memory nodes after settle: ~a~%" (node-count-direct))
  (format t "~&history: ~a~%" (length (val "*last-self-mod-history*"))))

(format t "~&~%== stopping workers ==~%")
(dolist (stopper '("turn-capture-worker-stop" "tick-loop-stop" "drives-stop"))
  (handler-case (format t "~&  ~a -> ~a~%" stopper (call stopper))
    (error (e) (format t "~&  ~a -> ~a~%" stopper e))))

(format t "~&~%CLONE-START-DONE~%")
(finish-output)
