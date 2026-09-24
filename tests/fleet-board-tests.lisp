;;;; harness: bare
(require :asdf)
(unless (find-package :ql) (load (or (uiop:getenv "PAI_QUICKLISP_SETUP") "/opt/quicklisp/setup.lisp")))
(asdf:load-asd (merge-pathnames "../pai-fleet.asd" *load-truename*))
(asdf:load-system :pai-fleet)
(in-package :pai.fleet)

(defvar *fbt-checks* 0)
(defun fbt-check (name value)
  (unless value (error "FAIL ~a" name))
  (incf *fbt-checks*) (format t "PASS ~a~%" name))

(defun fbt-signals-board-error-p (thunk &optional reason-substring)
  (handler-case (progn (funcall thunk) nil)
    (board-error (condition)
      (if (or (null reason-substring)
              (search reason-substring (board-error-reason condition)))
          t nil))
    (error () :wrong-condition-type)))

(defun fbt-signals-p (thunk)
  (handler-case (progn (funcall thunk) nil) (error () t)))

(defun fbt-scratch-path (name)
  (let ((path (merge-pathnames name (uiop:temporary-directory))))
    (ignore-errors (delete-file path))
    path))

;;; --- zero-item round trip (the exact class of bug that broke the peer
;;; store live: NIL threads/messages must survive a reload) -------------

(let* ((path (fbt-scratch-path
              (format nil "fleet-board-test-empty-~a.sexp" (random 1000000)))))
  (board-store-load path)
  (let ((reloaded (board-store-load path)))
    (fbt-check "a freshly created empty board reloads without error"
               (and (zerop (length (board-threads reloaded)))
                    (zerop (length (board-messages-since reloaded 0)))))))

;;; --- board-post-message: new thread ---------------------------------

(let* ((path (fbt-scratch-path
              (format nil "fleet-board-test-~a.sexp" (random 1000000))))
       (store (board-store-load path)))
  (multiple-value-bind (msg-id thread-id)
      (board-post-message store :new-thread-title "Hello fleet"
                                 :author-id "agent-a" :author-name "AgentA"
                                 :text "first post" :timestamp 1000)
    (fbt-check "posting a new thread returns a msg-id and thread-id"
               (and (stringp msg-id) (stringp thread-id)))
    (let ((thread (board-thread store thread-id)))
      (fbt-check "the new thread has the given title"
                 (string= "Hello fleet" (board-thread-title thread)))
      (fbt-check "the new thread is open"
                 (string= "open" (board-thread-status thread)))
      (fbt-check "the new thread's created-by defaults to author-id"
                 (string= "agent-a" (board-thread-created-by thread)))
      (fbt-check "the new thread's last-activity-at matches the post"
                 (= 1000 (board-thread-last-activity-at thread))))
    (let ((messages (board-thread-messages store thread-id)))
      (fbt-check "the thread has exactly one message"
                 (= 1 (length messages)))
      (fbt-check "the message text round-trips"
                 (string= "first post" (board-message-text (first messages)))))

    ;; --- posting again onto the SAME existing thread -------------------
    (multiple-value-bind (msg-id-2 thread-id-2)
        (board-post-message store :thread-id thread-id
                                   :author-id "agent-b" :author-name "AgentB"
                                   :text "reply" :reply-to msg-id
                                   :timestamp 2000)
      (fbt-check "posting to an existing thread returns the same thread-id"
                 (string= thread-id thread-id-2))
      (fbt-check "a second post is a distinct message"
                 (not (string= msg-id msg-id-2)))
      (fbt-check "posting bumps the thread's last-activity-at"
                 (= 2000 (board-thread-last-activity-at (board-thread store thread-id))))
      (fbt-check "the thread now has two messages, oldest first"
                 (let ((messages (board-thread-messages store thread-id)))
                   (and (= 2 (length messages))
                        (string= "first post" (board-message-text (first messages)))
                        (string= "reply" (board-message-text (second messages))))))
      (fbt-check "a reply's reply-to round-trips"
                 (string= msg-id
                          (board-message-reply-to
                           (second (board-thread-messages store thread-id))))))))

;;; --- validation -------------------------------------------------------

(let* ((path (fbt-scratch-path
              (format nil "fleet-board-test-validation-~a.sexp" (random 1000000))))
       (store (board-store-load path)))
  (fbt-check "an empty author-id is refused"
             (fbt-signals-p
              (lambda () (board-post-message store :new-thread-title "T"
                                              :author-id "" :author-name "A"
                                              :text "x"))))
  (fbt-check "empty text is refused"
             (fbt-signals-p
              (lambda () (board-post-message store :new-thread-title "T"
                                              :author-id "a" :author-name "A"
                                              :text ""))))
  (fbt-check "neither thread-id nor new-thread-title is refused"
             (fbt-signals-p
              (lambda () (board-post-message store :author-id "a" :author-name "A"
                                              :text "x"))))
  (fbt-check "both thread-id and new-thread-title is refused"
             (fbt-signals-p
              (lambda () (board-post-message store :thread-id "x" :new-thread-title "T"
                                              :author-id "a" :author-name "A"
                                              :text "x"))))
  (fbt-check "posting to a nonexistent thread-id is a board-error"
             (eq t (fbt-signals-board-error-p
                    (lambda () (board-post-message store :thread-id "no-such-thread"
                                                    :author-id "a" :author-name "A"
                                                    :text "x"))
                    "no such thread")))
  (fbt-check "an invalid intent is refused"
             (fbt-signals-p
              (lambda () (board-post-message store :new-thread-title "T"
                                              :author-id "a" :author-name "A"
                                              :text "x" :intent "not-a-real-intent"))))
  (fbt-check "an invalid scope is refused"
             (fbt-signals-p
              (lambda () (board-post-message store :new-thread-title "T"
                                              :author-id "a" :author-name "A"
                                              :text "x" :scope "not-a-real-scope"))))
  (fbt-check "non-string tags are refused"
             (fbt-signals-p
              (lambda () (board-post-message store :new-thread-title "T"
                                              :author-id "a" :author-name "A"
                                              :text "x" :tags '(1 2 3)))))
  (multiple-value-bind (msg-id thread-id)
      (board-post-message store :new-thread-title "T2" :author-id "a"
                                 :author-name "A" :text "x" :timestamp 1)
    (declare (ignore msg-id))
    (fbt-check "reply-to referencing a message in a DIFFERENT thread is refused"
               (eq t (fbt-signals-board-error-p
                      (lambda ()
                        (board-post-message store :new-thread-title "T3"
                                             :author-id "a" :author-name "A"
                                             :text "y")
                        (board-post-message store :thread-id thread-id
                                             :author-id "a" :author-name "A"
                                             :text "z" :reply-to "no-such-msg-id"))
                      "reply-to")))))

;;; --- resolved threads reject new posts ---------------------------------

(let* ((path (fbt-scratch-path
              (format nil "fleet-board-test-resolved-~a.sexp" (random 1000000))))
       (store (board-store-load path)))
  (multiple-value-bind (msg-id thread-id)
      (board-post-message store :new-thread-title "T" :author-id "a"
                                 :author-name "A" :text "x" :timestamp 1)
    (declare (ignore msg-id))
    (setf (board-thread-status (board-thread store thread-id)) "resolved")
    (board-store-save store)
    (fbt-check "posting to a resolved thread is refused"
               (eq t (fbt-signals-board-error-p
                      (lambda () (board-post-message store :thread-id thread-id
                                                      :author-id "b" :author-name "B"
                                                      :text "y"))
                      "resolved")))))

;;; --- board-threads / board-messages-since ordering ----------------------

(let* ((path (fbt-scratch-path
              (format nil "fleet-board-test-order-~a.sexp" (random 1000000))))
       (store (board-store-load path)))
  (board-post-message store :new-thread-title "Oldest" :author-id "a"
                             :author-name "A" :text "1" :timestamp 100)
  (board-post-message store :new-thread-title "Newest" :author-id "a"
                             :author-name "A" :text "2" :timestamp 300)
  (board-post-message store :new-thread-title "Middle" :author-id "a"
                             :author-name "A" :text "3" :timestamp 200)
  (let ((threads (board-threads store)))
    (fbt-check "board-threads sorts most-recently-active first"
               (equal '("Newest" "Middle" "Oldest")
                      (mapcar #'board-thread-title threads))))
  (let ((recent (board-messages-since store 150)))
    (fbt-check "board-messages-since excludes messages at or before the cutoff"
               (= 2 (length recent)))
    (fbt-check "board-messages-since returns oldest-first"
               (equal '("3" "2") (mapcar #'board-message-text recent)))))

;;; --- full round-trip with every field populated -------------------------

(let* ((path (fbt-scratch-path
              (format nil "fleet-board-test-roundtrip-~a.sexp" (random 1000000))))
       (store (board-store-load path)))
  (multiple-value-bind (msg-id thread-id)
      (board-post-message store :new-thread-title "Full fields" :author-id "a"
                                 :author-name "AgentA" :text "hi"
                                 :tags '("agent-b" "agent-c") :intent "question"
                                 :scope "fleet" :timestamp 500)
    (let ((reloaded (board-store-load path)))
      (let ((thread (board-thread reloaded thread-id))
            (message (first (board-thread-messages reloaded thread-id))))
        (fbt-check "reloaded thread title round-trips" (string= "Full fields" (board-thread-title thread)))
        (fbt-check "reloaded message tags round-trip"
                   (equal '("agent-b" "agent-c") (board-message-tags message)))
        (fbt-check "reloaded message intent round-trips"
                   (string= "question" (board-message-intent message)))
        (fbt-check "reloaded message scope round-trips"
                   (string= "fleet" (board-message-scope message)))
        (fbt-check "reloaded message id round-trips"
                   (string= msg-id (board-message-msg-id message)))))))

(let* ((path (fbt-scratch-path
              (format nil "fleet-board-test-ties-~a.sexp" (random 1000000))))
       (store (board-store-load path)))
  (multiple-value-bind (first-id thread-id)
      (board-post-message store :new-thread-title "Synthetic tied timestamps"
                          :author-id "fixture" :author-name "Fixture"
                          :text "First" :timestamp 1000)
    (multiple-value-bind (second-id ignored)
        (board-post-message store :thread-id thread-id
                            :author-id "fixture" :author-name "Fixture"
                            :text "Second" :timestamp 1000)
      (declare (ignore ignored))
      (let ((expected (sort (list first-id second-id) #'string<)))
        (fbt-check "equal timestamp messages have deterministic ID ordering"
                   (equal expected (mapcar #'board-message-msg-id
                                           (board-thread-messages store thread-id))))
        (fbt-check "tied ordering survives board reload"
                   (equal expected
                          (mapcar #'board-message-msg-id
                                  (board-thread-messages (board-store-load path) thread-id))))))))

(format t "~%FLEET BOARD TESTS: ~a checks passed.~%" *fbt-checks*)
