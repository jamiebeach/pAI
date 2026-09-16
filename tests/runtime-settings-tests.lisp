;;;; runtime-settings-tests.lisp -- event-sourced operator configuration.

(in-package :agent)
(ql:quickload '(:bordeaux-threads :shasht :hunchentoot) :silent t)

(defvar *runtime-settings-test-events* nil)
(defvar *runtime-settings-test-next-id* 0)
(defvar *runtime-settings-test-applied* nil)

(defun obj (&rest pairs)
  (loop with result = (make-hash-table :test #'equal)
        for (key value) on pairs by #'cddr
        do (setf (gethash key result) value)
        finally (return result)))

(defun replay-events (&key types &allow-other-keys)
  (remove-if-not (lambda (event)
                   (or (null types)
                       (member (gethash "type" event) types :test #'string=)))
                 *runtime-settings-test-events*))

(defun log-event (type payload &key &allow-other-keys)
  (let* ((id (incf *runtime-settings-test-next-id*))
         (event (obj "id" id "type" type "payload" payload)))
    (setf *runtime-settings-test-events*
          (append *runtime-settings-test-events* (list event)))
    (values id t event)))

(load (test-source "runtime-settings.lisp"))

(defvar *runtime-settings-test-passed* 0)
(defvar *runtime-settings-test-failed* 0)
(defun runtime-settings-check (name condition)
  (if condition
      (progn (incf *runtime-settings-test-passed*) (format t "PASS ~a~%" name))
      (progn (incf *runtime-settings-test-failed*) (format t "FAIL ~a~%" name))))

(defun runtime-settings-test-seed ()
  (let ((seed (make-hash-table :test #'equal)))
    (dolist (spec *runtime-settings-catalog*)
      (setf (gethash (first spec) seed)
            (case (second spec)
              (:boolean nil)
              (:string "test")
              (:enum (first (fifth spec)))
              (:integer (fifth spec))
              (:nullable-integer nil)
              (:number (fifth spec)))))
    ;; Representative real values make report assertions easier to read.
    (setf (gethash "private_budget_percent" seed) 30
          (gethash "web_port" seed) 8080)
    seed))

(let ((seed (runtime-settings-test-seed)))
  (runtime-settings-initialize seed :actor "test-bootstrap")
  (runtime-settings-check "first boot appends exactly one durable seed event"
                          (and (= 1 (length *runtime-settings-test-events*))
                               (string= "runtime-settings-initialized"
                                        (gethash "type" (first *runtime-settings-test-events*)))))
  (runtime-settings-configure-applier
   (lambda (key value) (push (list key value) *runtime-settings-test-applied*)))
  (runtime-settings-update "private_budget_percent" 50 :actor "test-operator")
  (runtime-settings-check "live setting update is applied after durable append"
                          (and (= 50 (runtime-settings-value "private_budget_percent"))
                               (equal '("private_budget_percent" 50)
                                      (first *runtime-settings-test-applied*))))
  (let ((before (length *runtime-settings-test-applied*)))
    (runtime-settings-update "web_port" 9090 :actor "test-operator")
    (runtime-settings-check "restart setting is durable but is not falsely hot-applied"
                            (= before (length *runtime-settings-test-applied*))))
  (runtime-settings-check "restart setting report distinguishes desired from effective"
                          (let ((row (find "web_port"
                                           (coerce (gethash "settings" (runtime-settings-report)) 'list)
                                           :key (lambda (item) (gethash "key" item))
                                           :test #'string=)))
                            (and (= 9090 (gethash "desired" row))
                                 (= 8080 (gethash "effective" row))
                                 (eq t (gethash "pending_restart" row)))))
  (runtime-settings-check "invalid values fail before another event is appended"
                          (let ((before (length *runtime-settings-test-events*)))
                            (and (handler-case
                                     (progn (runtime-settings-update
                                             "private_budget_percent" 101)
                                            nil)
                                   (error () t))
                                 (= before (length *runtime-settings-test-events*)))))
  ;; A new process must reconstruct from the ledger, not from changed launch defaults.
  (setf *runtime-settings-initialized-p* nil
        *runtime-settings-values* (make-hash-table :test #'equal)
        *runtime-settings-effective* (make-hash-table :test #'equal))
  (let ((different-seed (runtime-settings-test-seed)))
    (setf (gethash "private_budget_percent" different-seed) 7)
    (runtime-settings-initialize different-seed :actor "second-launch")
    (runtime-settings-check "restart reconstructs durable settings instead of launcher defaults"
                            (= 50 (runtime-settings-value "private_budget_percent")))
    (runtime-settings-check "restart does not append a duplicate seed"
                            (= 3 (length *runtime-settings-test-events*)))))

(format t "~%~d passed, ~d failed~%"
        *runtime-settings-test-passed* *runtime-settings-test-failed*)
(when (plusp *runtime-settings-test-failed*) (uiop:quit 1))
