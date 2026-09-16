;;;; harness: full-system
(in-package :agent)

(uiop:call-with-temporary-file
 (lambda (path)
   (let ((first (make-sqlite-storage path)) (second nil))
     (unwind-protect
          (progn
            (setf second (make-sqlite-storage path))
            (let ((head (storage-head-position first)))
              (storage-append-event-if-head first head "fixture-reserved" (obj "amount" 3)
                                           :agent-id "fixture-agent")
              (assert (handler-case
                          (progn (storage-append-event-if-head second head "fixture-reserved" (obj "amount" 3)
                                                              :agent-id "fixture-agent") nil)
                        (storage-conflict-error () t)))
              (assert (= (1+ head) (storage-head-position second)))
              ;; A current head succeeds, and ordinary appends remain compatible.
              (storage-append-event-if-head second (storage-head-position second)
                                           "fixture-settled" (obj "amount" 2) :agent-id "fixture-agent")
              (storage-append-event first "fixture-other" (obj) :agent-id "other-agent")
              (assert (= (+ head 3) (storage-head-position first)))
              (let* ((shared-head (storage-head-position first))
                     (start (sb-thread:make-semaphore :count 0))
                     (results (make-array 2 :initial-element :pending))
                     (threads
                       (loop for backend in (list first second) for index from 0 collect
                         (let ((target backend) (slot index))
                           (sb-thread:make-thread
                            (lambda ()
                              (sb-thread:wait-on-semaphore start)
                              (setf (aref results slot)
                                    (handler-case
                                        (progn (storage-append-event-if-head target shared-head "fixture-race" (obj)
                                                                            :agent-id "fixture-agent") :accepted)
                                      (storage-conflict-error () :conflict)))))))))
                (sb-thread:signal-semaphore start 2)
                (dolist (thread threads) (sb-thread:join-thread thread :timeout 10 :default :timeout))
                (assert (= 1 (count :accepted results)))
                (assert (= 1 (count :conflict results)))
                (assert (= (1+ shared-head) (storage-head-position first))))
              (assert (handler-case
                          (progn (storage-append-event-if-head second nil "fixture-invalid" (obj)) nil)
                        (storage-error () t)))))
       (when second (storage-close second))
       (storage-close first))))
 :want-stream-p nil :type "sqlite")
(format t "CONDITIONAL-APPEND concurrent connections admit exactly one stale-head contender; current and ordinary appends pass~%")
(format t "PASS storage-conditional-append-tests~%")
