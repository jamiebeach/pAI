(in-package :agent)

(load (test-source "operational-anomaly-detector.lisp"))

(defvar *oad-pass* 0)
(defvar *oad-fail* 0)

(defun oad-check (name condition)
  (if condition
      (progn (incf *oad-pass*) (format t "PASS ~a~%" name))
      (progn (incf *oad-fail*) (format t "FAIL ~a~%" name))))

(defun oad-failed-event (id failure-code &key (field "error_code"))
  (obj "id" id "type" "model-response"
       "payload" (obj "status" "failed" field failure-code)))

(defun oad-accepted-event (id)
  (obj "id" id "type" "model-response"
       "payload" (obj "status" "accepted")))

(defun oad-unrelated-event (id)
  (obj "id" id "type" "user-message" "payload" (obj "content" "hello")))

;;; --- basic detection ------------------------------------------------

(let ((events (list (oad-failed-event 1 "provider-http-429")
                     (oad-failed-event 2 "provider-http-429")
                     (oad-failed-event 3 "provider-http-429"))))
  (let ((candidates (recursive-operational-anomaly-scan events)))
    (oad-check "three repeats at the floor produce exactly one candidate"
               (= 1 (length candidates)))
    (oad-check "the candidate names the repeated code"
               (equal "provider-http-429"
                      (gethash "failure_code" (first candidates))))
    (oad-check "the candidate counts every occurrence"
               (= 3 (gethash "occurrence_count" (first candidates))))
    (oad-check "the candidate bounds are the first and last contributing ids"
               (and (= 1 (gethash "first_event_id" (first candidates)))
                    (= 3 (gethash "last_event_id" (first candidates)))))
    (oad-check "the candidate lists every contributing id, oldest first"
               (equalp #(1 2 3) (gethash "event_ids" (first candidates))))))

(let ((events (list (oad-failed-event 1 "provider-http-429")
                     (oad-failed-event 2 "provider-http-429"))))
  (oad-check "below the floor, nothing is raised"
             (null (recursive-operational-anomaly-scan events))))

(let ((events (list (oad-failed-event 1 "provider-http-429")
                     (oad-accepted-event 2)
                     (oad-failed-event 3 "provider-http-429")
                     (oad-unrelated-event 4)
                     (oad-failed-event 5 "provider-http-429"))))
  (oad-check "accepted and unrelated events never count toward a pattern"
             (let ((candidates (recursive-operational-anomaly-scan events)))
               (and (= 1 (length candidates))
                    (= 3 (gethash "occurrence_count" (first candidates)))))))

;;; --- multiple distinct patterns --------------------------------------

(let ((events (list (oad-failed-event 1 "provider-http-429")
                     (oad-failed-event 2 "provider-http-500")
                     (oad-failed-event 3 "provider-http-429")
                     (oad-failed-event 4 "provider-http-500")
                     (oad-failed-event 5 "provider-http-429")
                     (oad-failed-event 6 "provider-http-500"))))
  (let ((candidates (recursive-operational-anomaly-scan events)))
    (oad-check "two independently qualifying codes both raise"
               (= 2 (length candidates)))
    (oad-check "candidates are ordered by most-recently-failed code first"
               (equal "provider-http-500"
                      (gethash "failure_code" (first candidates))))))

;;; --- the two inconsistent field names across the substrate's own
;;; call sites: recursive-mind-runtime.lisp uses error_code, the live
;;; conversation-turn boundary uses failure_code. Both must be readable.

(let ((events (list (oad-failed-event 1 "provider-call-timeout"
                                       :field "failure_code")
                     (oad-failed-event 2 "provider-call-timeout"
                                       :field "failure_code")
                     (oad-failed-event 3 "provider-call-timeout"
                                       :field "failure_code"))))
  (oad-check "the live-turn boundary's failure_code key is read too"
             (let ((candidates (recursive-operational-anomaly-scan events)))
               (and (= 1 (length candidates))
                    (equal "provider-call-timeout"
                           (gethash "failure_code" (first candidates)))))))

(let ((events (list (oad-failed-event 1 "x" :field "error_code")
                     (oad-failed-event 2 "x" :field "failure_code")
                     (oad-failed-event 3 "x" :field "error_code"))))
  (oad-check "the same code is one pattern regardless of which key carried it"
             (let ((candidates (recursive-operational-anomaly-scan events)))
               (and (= 1 (length candidates))
                    (= 3 (gethash "occurrence_count" (first candidates)))))))

;;; --- the lookback window is bounded, not the whole ledger -----------

(let* ((old-failures (loop for id from 1 to 10
                            collect (oad-failed-event id "provider-http-429")))
       (filler (loop for id from 11 to 20 collect (oad-accepted-event id)))
       (events (append old-failures filler)))
  (oad-check "a lookback narrower than the window excludes older failures"
             (null (recursive-operational-anomaly-scan events :lookback 5))))

;;; --- configuration validation -----------------------------------------

(oad-check "a non-positive lookback is refused"
           (handler-case
               (progn (recursive-operational-anomaly-scan nil :lookback 0) nil)
             (error () t)))
(oad-check "a non-positive minimum occurrence is refused"
           (handler-case
               (progn (recursive-operational-anomaly-scan nil :min-occurrences 0)
                      nil)
             (error () t)))
(oad-check "an empty event list raises nothing and does not error"
           (null (recursive-operational-anomaly-scan nil)))

(format t "~%OPERATIONAL ANOMALY DETECTOR TESTS: ~a passed, ~a failed.~%"
        *oad-pass* *oad-fail*)
(when (plusp *oad-fail*) (sb-ext:exit :code 1))
