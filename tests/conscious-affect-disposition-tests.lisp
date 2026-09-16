;;;; Load after full system; synthetic evidence only, no provider or initialization.
;;;; harness: full-system
(in-package :agent)
(let ((checks 0))
  (labels ((check (x) (incf checks) (assert x))
           (event (id at root tool outcome &optional exit)
             (obj "id" id "timestamp" at "agent_id" "test-mind"
                  "type" "recursive-tool-result" "payload"
                  (obj "thread_id" root "tool_name" tool
                       "execution_status" "executed"
                       "process_outcome" (if exit (obj "kind" "process-exit" "exit_code" exit) nil)
                       "affect_observation" (conscious-affect-tool-observation outcome "test-mind"))))
           (project (events &optional (now 100))
             (conscious-affect-disposition-project events "test-mind" "test-mind" :now now))
           (value (events &optional (now 100))
             (gethash "operational_coping_milliunits" (project events now))))
    (let* ((bad (event 1 100 "root:1" "bash" "returned" 1))
           (good (event 2 100 "root:2" "bash" "returned" 0))
           (plain (event 3 100 "root:3" "bash" "returned")))
      (check (= 450 (value (list bad))))
      (check (= 500 (value (list good))))
      (check (= 500 (value (list bad good))))
      (check (= 450 (value (list bad plain))))
      (check (= 500 (value (list bad) 7300)))
      (check (equalp (project (list bad good)) (project (list bad good))))
      (check (= 450 (value (list bad bad))))
      (check (= 400 (value (loop for i from 1 to 8 collect
                                (event i 100 "root:one" "bash" "returned" 1)))))
      (check (= 0 (value (loop for i from 1 to 20 collect
                              (event i 100 (format nil "root:~d" i) "bash" "returned" 1)))))
      (check (equal "unavailable" (gethash "status" (project nil))))
      (check (equal "unavailable" (gethash "status" (project (list bad) 99))))
      (check (equal "unavailable" (gethash "status"
                       (project (list bad (event 1 100 "different" "bash" "returned" 0))))))
      (check (equal "unavailable" (gethash "status"
                       (project (list bad (event 2 99 "root:2" "bash" "returned" 0))))))
      (check (= 475 (value (list bad) 3700)))
      (check (= 450 (value (list (event 1 100 "r1" "other" "raised-error") good))))
      (check (eq :null (gethash "context_injection" (project (list bad))))))
    ;; The read-only authority inspector binds an explicit baseline and head.
    (let* ((history (list (event 11 100 "root:11" "bash" "returned" 1)
                          (event 12 100 "root:12" "bash" "returned" 0)))
           (saved-report (symbol-function 'event-authority-report))
           (saved-map (symbol-function 'map-events)))
      (unwind-protect
           (progn
             (setf (symbol-function 'event-authority-report)
                   (lambda () (obj "authority" "fixture" "agent_id" "test-mind"
                                   "max_event_id" 12))
                   (symbol-function 'map-events)
                   (lambda (visitor &key after-id through-id types &allow-other-keys)
                     (check (= 10 after-id))
                     (check (= 12 through-id))
                     (check (equal '("recursive-tool-result") types))
                     (dolist (item history) (funcall visitor item))
                     (values t 12 2)))
             (let* ((inspection
                      (conscious-affect-inspect-window
                       10 "test-mind" "test-mind" :now 100))
                    (report (gethash "observation_report" inspection))
                    (rows (gethash "observations" report)))
               (check (equal "inspected" (gethash "status" inspection)))
               (check (= 2 (length rows)))
               (check (= 500 (gethash "operational_coping_milliunits"
                                      (gethash "projection" inspection))))
               (check (equalp #(11 12)
                              (map 'vector (lambda (row)
                                             (gethash "source_event_id" row))
                                   rows)))
               (check (eq :null (gethash "context_injection" inspection))))
             (setf (symbol-function 'event-authority-report)
                   (lambda () (obj "agent_id" "other-mind" "max_event_id" 12)))
             (check (equal "authority-partition-or-window-unavailable"
                           (gethash "reason"
                                    (conscious-affect-inspect-window
                                     10 "test-mind" "test-mind" :now 100))))
             (setf (symbol-function 'event-authority-report)
                   (lambda () (obj "agent_id" "test-mind" "max_event_id" 12))
                   (symbol-function 'map-events)
                   (lambda (visitor &rest arguments)
                     (declare (ignore visitor arguments))
                     (values nil 11 1)))
             (check (equal "incomplete-authority-window"
                           (gethash "reason"
                                    (conscious-affect-inspect-window
                                     10 "test-mind" "test-mind" :now 100)))))
        (setf (symbol-function 'event-authority-report) saved-report
              (symbol-function 'map-events) saved-map)))
    ;; Real primitive adapter: process status is a second value, not parsed text.
    (let ((*recursive-primitive-workspace-root* #p"/tmp/")
          (*recursive-primitive-bash-executable* #p"/bin/bash"))
      (multiple-value-bind (text receipt) (%recursive-primitive-bash "exit 0")
        (check (search "exit_code: 0" text))
        (check (= 0 (gethash "exit_code" receipt)))))
  (format t "~&PASS: ~d focused disposition checks.~%" checks)))
