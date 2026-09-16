(in-package :agent)

(ql:quickload '(:bordeaux-threads :shasht :dexador) :silent t)

(defvar *telegram-envelope-pass* 0)
(defvar *telegram-envelope-fail* 0)
(defvar *telegram-envelope-captured* nil)
(defvar *telegram-envelope-turn-count* 0)
(defvar *initiative-candidates* nil)
(defvar *initiative-policy-current-id* nil)
(defvar *public-inbound-ordinary-reply-p* nil)
(defvar *telegram-envelope-inbound-authorized* nil)

(defun telegram-envelope-check (name condition)
  (if condition
      (progn (incf *telegram-envelope-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *telegram-envelope-fail*) (format t "  FAIL ~a~%" name))))

(defun auto-turn (prompt)
  (declare (ignore prompt))
  (error "Telegram bypassed the cognition runtime registry"))

(defun submit-stimulus (prompt &key kind metadata wait-for-public-result)
  (declare (ignore prompt metadata))
  (unless (and (eq kind :user-message) wait-for-public-result)
    (error "Telegram submitted the wrong cognition contract"))
  (incf *telegram-envelope-turn-count*)
  (setf *telegram-envelope-inbound-authorized*
        *public-inbound-ordinary-reply-p*)
  "fixture reply")

(load (test-source "runtime-observer-registry.lisp"))
(load (test-source "public-outbound-gateway.lisp"))
(load (test-source "telegram.lisp"))

;; Replace only the network transport after loading the real handler. The
;; handler's dynamic envelope is captured without any HTTP or provider call;
;; AUTO-TURN above is a tripwire proving the handler used the registry seam.
(setf (fdefinition 'telegram-send)
      (lambda (chat-id text)
        (setf *telegram-envelope-captured*
              (obj "chat_id" chat-id "text" text
                   "envelope" *public-outbound-envelope*))
        "sent"))

(let ((update
        (obj "update_id" 42
             "message" (obj "chat" (obj "id" 1001)
                            "text" "/mind"))))
  (telegram-handle-message update))

(let ((envelope (gethash "envelope" *telegram-envelope-captured*)))
  (telegram-envelope-check "reactive handler executes exactly one turn"
                           (= 1 *telegram-envelope-turn-count*))
  (telegram-envelope-check "reactive turn carries explicit inbound reply authority"
                           (eq *telegram-envelope-inbound-authorized* t))
  (telegram-envelope-check "update offset advances exactly once"
                           (= 43 *telegram-offset*))
  (telegram-envelope-check "reply envelope retains lexical update ID"
                           (and (string= "reply" (gethash "kind" envelope))
                                (string= "telegram-update:42"
                                         (gethash "authorization_id" envelope))
                                (find "telegram-update:42"
                                      (gethash "causal_event_ids" envelope)
                                      :test #'string=)))
  (telegram-envelope-check "legacy send receives unchanged reply"
                           (and (= 1001 (gethash "chat_id"
                                                *telegram-envelope-captured*))
                                (string= "fixture reply"
                                         (gethash "text"
                                                  *telegram-envelope-captured*)))))

(format t "~%Telegram envelope tests: ~d passed, ~d failed.~%"
        *telegram-envelope-pass* *telegram-envelope-fail*)
(when (plusp *telegram-envelope-fail*)
  (error "Telegram envelope tests failed"))
