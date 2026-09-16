;;;; pai-cli-tests.lisp -- canonical interactive command grammar.

(in-package :agent)

(defvar *pai-cli-passed* 0)
(defvar *pai-cli-failed* 0)

(defun pai-cli-check (name condition)
  (if condition
      (progn (incf *pai-cli-passed*) (format t "PASS ~a~%" name))
      (progn (incf *pai-cli-failed*) (format t "FAIL ~a~%" name))))

(format t "~%== canonical pAI CLI command grammar ==~%")

(let ((core (merge-pathnames "scripts/pai-cli-core.lisp" *pai-root*)))
  (pai-cli-check "unified CLI core exists" (probe-file core))
  (when (probe-file core)
    (load core)
    (multiple-value-bind (kind payload) (pai-cli-parse-input "hello there")
      (pai-cli-check "ordinary input remains chat"
                     (and (eq kind :chat) (string= payload "hello there"))))
    (multiple-value-bind (kind payload)
        (pai-cli-parse-input "/lifecycle-create doorbell interruption | resume the conversation")
      (pai-cli-check "lifecycle create has bounded subject and aim"
                     (and (eq kind :lifecycle-create)
                          (string= "doorbell interruption"
                                   (gethash "subject" payload))
                          (string= "resume the conversation"
                                   (gethash "aim" payload)))))
    (multiple-value-bind (kind payload)
        (pai-cli-parse-input "/lifecycle-ready bounded result")
      (pai-cli-check "ready carries its bounded result summary"
                     (and (eq kind :lifecycle-ready)
                          (string= "bounded result"
                                   (gethash "result_summary" payload)))))
    (multiple-value-bind (kind payload)
        (pai-cli-parse-input "/memory-inspect cobalt test word")
      (pai-cli-check "memory inspect carries one bounded read-only query"
                     (and (eq kind :memory-inspect)
                          (string= "cobalt test word"
                                   (gethash "query" payload)))))
    (multiple-value-bind (kind payload)
        (pai-cli-parse-input "/lifecycle-complete bounded public result")
      (pai-cli-check "complete carries observed public text"
                     (and (eq kind :lifecycle-complete)
                          (string= "bounded public result"
                                   (gethash "observed_reply" payload)))))
    (pai-cli-check "inspect is argument-free"
                   (eq :lifecycle-inspect
                       (nth-value 0 (pai-cli-parse-input "/lifecycle-inspect"))))
    (pai-cli-check "private curiosity inspection is argument-free"
                   (eq :curiosity-inspect
                       (nth-value 0 (pai-cli-parse-input "/curiosity-inspect"))))
    (pai-cli-check "private affect inspection is argument-free"
                   (eq :affect-inspect
                       (nth-value 0 (pai-cli-parse-input "/affect-inspect"))))
    (pai-cli-check "content-free graph inspection is argument-free"
                   (eq :graph-inspect
                       (nth-value 0 (pai-cli-parse-input "/graph-inspect"))))
    (pai-cli-check "runtime settings inspection is argument-free"
                   (eq :config-inspect
                       (nth-value 0 (pai-cli-parse-input "/config"))))
    (multiple-value-bind (kind payload)
        (pai-cli-parse-input "/config-set private_budget_percent 50")
      (pai-cli-check "runtime setting update preserves key and typed source text"
                     (and (eq kind :config-set)
                          (string= "private_budget_percent" (gethash "key" payload))
                          (string= "50" (gethash "value_text" payload)))))
    (multiple-value-bind (kind payload)
        (pai-cli-parse-input "/graph-search operator accessibility")
      (pai-cli-check "graph search carries one bounded read-only query"
                     (and (eq kind :graph-search)
                          (string= "operator accessibility"
                                   (gethash "query" payload)))))
    (let ((observed nil))
      (multiple-value-bind (kind payload)
          (pai-cli-parse-input "/lifecycle-create subject | aim")
        (pai-cli-run-lifecycle
         kind payload (lambda (&rest arguments) (setf observed arguments))))
      (pai-cli-check "parsed create reaches the lifecycle runner exactly"
                     (equal observed '("create" :subject "subject" :aim "aim"))))
    (let ((observed nil))
      (pai-cli-run-lifecycle
       :lifecycle-inspect nil
       (lambda (&rest arguments) (setf observed arguments)))
      (pai-cli-check "inspect reaches the lifecycle runner without arguments"
                     (equal observed '("inspect"))))
    (pai-cli-check "unknown slash command fails closed"
                   (handler-case
                       (progn (pai-cli-parse-input "/invent-authority now") nil)
                     (error () t)))))

(format t "~%~d passed, ~d failed~%" *pai-cli-passed* *pai-cli-failed*)
(when (plusp *pai-cli-failed*) (uiop:quit 1))
