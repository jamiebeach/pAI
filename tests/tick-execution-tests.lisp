(in-package :agent)

(ql:quickload '(:bordeaux-threads :postmodern) :silent t)
(defvar *tick-exec-pass* 0)
(defvar *tick-exec-fail* 0)
(defun tick-exec-check (name condition)
  (if condition
      (progn (incf *tick-exec-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *tick-exec-fail*) (format t "  FAIL ~a~%" name))))

(defvar *tick-lock* (bt:make-lock "test-tick"))
(defvar *tick-forced-type* nil)
(defvar *tick-handlers* (obj))
(defvar *tick-cost-accumulator* nil)
(defvar *cognitive-generation-mode* :legacy)
(defvar *autonomous-write-mode* :normal)
(defvar *v2-turn-in-flight* nil)
(defvar *tick-exec-legacy-calls* 0)
(defvar *tick-exec-record-calls* 0)
(defvar *tick-exec-proposal-calls* 0)
(defvar *tick-exec-intention-due* nil)
(defvar *tick-exec-intention-calls* 0)

(defun %tick-select-type () "explore")
(defun %tick-record-run () (incf *tick-exec-record-calls*))
(defun tick-once () (incf *tick-exec-legacy-calls*) "legacy")
(defun tick-execute-proposal (type event &key mode)
  (declare (ignore type event mode))
  (incf *tick-exec-proposal-calls*)
  (obj "status" "shadow-valid" "reason" "fixture" "write_count" 0))
(defun near-term-intention-due-p () *tick-exec-intention-due*)
(defun near-term-intention-process-due ()
  (incf *tick-exec-intention-calls*)
  (obj "status" "ready" "write_count" 0))

(load (test-source "tick-commit.lisp"))
(load (test-source "tick-execution.lisp"))

(setf (gethash "explore" *tick-handlers*)
      (lambda () (incf *tick-exec-legacy-calls*) :legacy-handler))

(let ((*cognitive-generation-mode* :legacy))
  (tick-exec-check "legacy mode preserves original tick-once"
                   (string= "legacy" (tick-once)))
  (tick-exec-check "legacy base called once" (= *tick-exec-legacy-calls* 1)))

(let* ((*cognitive-generation-mode* :legacy)
       (*autonomous-write-mode* :paused)
       (legacy-before *tick-exec-legacy-calls*)
       (record-before *tick-exec-record-calls*)
       (proposal-before *tick-exec-proposal-calls*)
       (result (tick-once)))
  (tick-exec-check "paused mode skips legacy autonomous handler"
                   (= legacy-before *tick-exec-legacy-calls*))
  (tick-exec-check "paused mode records no tick run"
                   (= record-before *tick-exec-record-calls*))
  (tick-exec-check "paused mode evaluates no proposal"
                   (= proposal-before *tick-exec-proposal-calls*))
  (tick-exec-check "paused mode reports an explicit zero-write skip"
                   (and (string= "skipped" (gethash "status" result))
                        (string= "autonomous-writes-paused"
                                 (gethash "reason" result))
                        (zerop (gethash "write_count" result)))))

(let* ((events nil)
       (*cognitive-generation-mode* :shadow)
       (*v2-turn-in-flight* nil)
       (*tick-terminal-event-fn*
         (lambda (type payload caused-by)
           (push (list type payload caused-by) events) (length events))))
  (let ((result (tick-once)))
    (tick-exec-check "shadow executes final legacy handler"
                     (= *tick-exec-legacy-calls* 2))
    (tick-exec-check "shadow evaluates one proposal" (= *tick-exec-proposal-calls* 1))
    (tick-exec-check "shadow returns proposal outcome"
                     (string= "shadow-valid" (gethash "status" result)))
    (tick-exec-check "shadow emits one start"
                     (= 1 (count "tick-start" events :key #'first :test #'string=)))
    (tick-exec-check "shadow emits one terminal"
                     (= 1 (count "tick-terminal" events :key #'first :test #'string=)))
    (tick-exec-check "terminal is correlated"
                     (third (find "tick-terminal" events :key #'first :test #'string=)))))

(let* ((events nil)
       (*cognitive-generation-mode* :enforced)
       (*autonomous-write-mode* :normal)
       (*v2-turn-in-flight* nil)
       (legacy-before *tick-exec-legacy-calls*)
       (proposal-before *tick-exec-proposal-calls*)
       (*tick-terminal-event-fn*
         (lambda (type payload caused-by)
           (push (list type payload caused-by) events) (length events))))
  (tick-once)
  (tick-exec-check "enforced bypasses legacy mutation handler"
                   (= legacy-before *tick-exec-legacy-calls*))
  (tick-exec-check "enforced evaluates one proposal"
                   (= (1+ proposal-before) *tick-exec-proposal-calls*)))

(let* ((*cognitive-generation-mode* :enforced)
       (*autonomous-write-mode* :normal)
       (*v2-turn-in-flight* nil)
       (*tick-exec-intention-due* t)
       (proposal-before *tick-exec-proposal-calls*)
       (intention-before *tick-exec-intention-calls*)
       (result (tick-once)))
  (tick-exec-check "due conversational commitment gets tick priority"
                   (= (1+ intention-before) *tick-exec-intention-calls*))
  (tick-exec-check "priority commitment suppresses ambient proposal"
                   (= proposal-before *tick-exec-proposal-calls*))
  (tick-exec-check "priority result is returned through terminal path"
                   (string= "ready" (gethash "status" result))))

(let* ((events nil)
       (*cognitive-generation-mode* :shadow)
       (*v2-turn-in-flight* t)
       (legacy-before *tick-exec-legacy-calls*)
       (proposal-before *tick-exec-proposal-calls*)
       (*tick-terminal-event-fn*
         (lambda (type payload caused-by)
           (push (list type payload caused-by) events) (length events))))
  (let ((result (tick-once)))
    (tick-exec-check "preemption is explicit skip"
                     (string= "skipped" (gethash "status" result)))
    (tick-exec-check "preemption runs no legacy handler"
                     (= legacy-before *tick-exec-legacy-calls*))
    (tick-exec-check "preemption spends no proposal/model work"
                     (= proposal-before *tick-exec-proposal-calls*))
    (tick-exec-check "preemption still terminates"
                     (= 1 (count "tick-terminal" events :key #'first :test #'string=)))))

(let* ((events nil)
       (*cognitive-generation-mode* :shadow)
       (*v2-turn-in-flight* nil)
       (*tick-terminal-event-fn*
         (lambda (type payload caused-by)
           (push (list type payload caused-by) events) (length events))))
  (setf (gethash "explore" *tick-handlers*) (lambda () (error "forced handler")))
  (tick-exec-check "handler exception propagates"
                   (handler-case (progn (tick-once) nil) (error () t)))
  (tick-exec-check "handler exception still emits one terminal"
                   (= 1 (count "tick-terminal" events :key #'first :test #'string=)))
  (tick-exec-check
   "handler exception terminal is error"
   (string= "error"
            (gethash "status" (second (find "tick-terminal" events
                                             :key #'first :test #'string=)))))
  (setf (gethash "explore" *tick-handlers*)
        (lambda () (incf *tick-exec-legacy-calls*) :legacy-handler)))

(let ((wrapper (fdefinition 'tick-once)))
  (load (test-source "tick-execution.lisp"))
  (tick-exec-check "harmless reload retains wrapper identity"
                   (not (eq wrapper (fdefinition 'pai-base-tick-once-stab04))))
  (let ((*cognitive-generation-mode* :legacy))
    (tick-exec-check "reload does not recurse" (string= "legacy" (tick-once)))))

(let ((true-base (fdefinition 'pai-base-tick-once-stab04)))
  (setf (fdefinition 'pai-base-tick-once-timing) true-base
        (fdefinition '%timing-tick-once)
        (lambda () (funcall 'pai-base-tick-once-timing))
        (fdefinition 'tick-once) (fdefinition '%timing-tick-once))
  (load (test-source "tick-execution.lisp"))
  (tick-exec-check "live install unwraps prior timing layer"
                   (eq true-base (fdefinition 'pai-base-tick-once-stab04)))
  (let ((*cognitive-generation-mode* :legacy))
    (tick-exec-check "live timing-order install preserves legacy result"
                     (string= "legacy" (tick-once)))))

(format t "~%~a passed, ~a failed~%" *tick-exec-pass* *tick-exec-fail*)
(when (plusp *tick-exec-fail*) (sb-ext:exit :code 1))
