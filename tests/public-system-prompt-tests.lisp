(defpackage :agent (:use :cl))
(in-package :agent)
(ql:quickload '(:shasht :ironclad :babel) :silent t)

(defvar *psp-test-pass* 0)
(defvar *psp-test-fail* 0)
(defun psp-check (name condition)
  (if condition
      (progn (incf *psp-test-pass*) (format t "PASS ~a~%" name))
      (progn (incf *psp-test-fail*) (format t "FAIL ~a~%" name))))
(defun obj (&rest kvs)
  (loop with table = (make-hash-table :test #'equal)
        for (key value) on kvs by #'cddr
        do (setf (gethash key table) value)
        finally (return table)))

(uiop:chdir (test-state-dir))
(load (test-source "public-system-prompt.lisp"))

(let* ((config #P"/tmp/pai-public-system-prompt-test.json")
       (*public-system-prompt-config-file* config)
       (*public-system-prompt-current* nil)
       (*public-system-prompt-history* nil)
       (*tools* (vector
                 (obj "type" "function"
                      "function" (obj "name" "lisp-eval"
                                      "description" "Evaluate bounded Lisp.")))))
  (when (probe-file config) (delete-file config))
  (load-public-system-prompt-config)
  (let ((first (public-system-prompt-render-stable))
        (second (public-system-prompt-render-stable)))
    (psp-check "same fragments and tools render byte-identically"
               (string= first second))
    (psp-check "Markdown defaults seed identity and voice"
               (and (search "I am **ACME Agent**" first)
                    (search "Voice and demeanor" first)))
    (psp-check "live tool schema is rendered"
               (search "lisp-eval: Evaluate bounded Lisp." first))
    (psp-check "renderer owns exactly one tools block"
               (= 1 (loop with start = 0 for pos = (search "<!-- TOOLS:BEGIN -->" first :start2 start)
                          while pos count t do (setf start (1+ pos))))))
  (let ((*tools* (vector
                  (obj "type" "function"
                       "function" (obj "name" "brave-search"
                                       "description" "Search the web.")))))
    (let ((changed (public-system-prompt-render-stable)))
      (psp-check "tool registry changes the next render"
                 (and (search "brave-search: Search the web." changed)
                      (null (search "lisp-eval: Evaluate bounded Lisp." changed))))))
  (let* ((before (public-system-prompt-report))
         (before-revision (gethash "revision" before))
         (updated (public-system-prompt-update "# Who I am\n\nI am test the agent."
                                               "# Voice\n\nPrecise and warm."
                                               :actor "test")))
    (psp-check "admin update increments revision"
               (= (1+ before-revision) (gethash "revision" updated)))
    (psp-check "admin update persists before publication"
               (probe-file config))
    (psp-check "admin values affect next render without a new conversation"
               (let ((rendered (public-system-prompt-render-stable)))
                 (and (search "I am test the agent." rendered)
                      (search "Precise and warm." rendered))))
    (setf *public-system-prompt-current* nil
          *public-system-prompt-history* nil)
    (load-public-system-prompt-config)
    (psp-check "persisted override reloads after simulated restart"
               (search "I am test the agent."
                       (public-system-prompt-render-stable)))
    (let ((rolled (public-system-prompt-rollback :actor "test")))
      (psp-check "rollback creates a new monotonic revision"
                 (> (gethash "revision" rolled) (gethash "revision" updated)))
      (psp-check "rollback restores prior content"
                 (search "I am **ACME Agent**"
                         (public-system-prompt-render-stable))))
    (public-system-prompt-update "# Changed identity" "# Changed voice"
                                 :actor "test")
    (let ((reset (public-system-prompt-reset-defaults :actor "test")))
      (psp-check "reset rereads reviewed Markdown defaults"
                 (and (string= "markdown-reset" (gethash "source" reset))
                      (search "I am **ACME Agent**"
                              (public-system-prompt-render-stable))))))
  (psp-check "renderer-owned markers are rejected"
             (handler-case
                 (progn (public-system-prompt-update
                         "<!-- PAI-STATE:BEGIN -->" "voice") nil)
               (error () t)))
  (psp-check "unknown update fields fail closed"
             (handler-case
                 (progn (public-system-prompt-update-from-object
                         (obj "identity" "identity" "voice" "voice"
                              "autonomous_write" "normal"))
                        nil)
               (error () t)))
  (let* ((before-revision (gethash "revision" *public-system-prompt-current*))
         (*public-system-prompt-config-file*
           #P"/proc/pai-public-system-prompt-test.json"))
    (psp-check "atomic persistence failure leaves live revision untouched"
               (and (handler-case
                        (progn (public-system-prompt-update "identity" "voice") nil)
                      (error () t))
                    (= before-revision
                       (gethash "revision" *public-system-prompt-current*)))))
  (when (probe-file config) (delete-file config)))

(format t "~%PUBLIC SYSTEM PROMPT TESTS: ~d passed, ~d failed.~%"
        *psp-test-pass* *psp-test-fail*)
(when (plusp *psp-test-fail*) (uiop:quit 1))
