(in-package :agent)

(ql:quickload '(:bordeaux-threads :postmodern :shasht) :silent t)

(defvar *r0b-pass* 0)
(defvar *r0b-fail* 0)

(defun r0b-check (name condition)
  (if condition
      (progn (incf *r0b-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *r0b-fail*) (format t "  FAIL ~a~%" name))))

(defun r0b-reader-clean-p (path)
  (with-open-file (in path)
    (loop for form = (read in nil :eof)
          until (eq form :eof)
          finally (return t))))

(r0b-check
 "reader accepts every changed Lisp module"
 (every #'r0b-reader-clean-p
        (list (test-source "event-log.lisp")
      (test-source "tick-commit.lisp")
      (test-source "tick-loop.lisp"))))

;; EVENT-LOG's wrappers need legacy call points. This fixture exercises only
;; the writer envelope and the shared tick-terminal boundary.
(unless (fboundp 'auto-turn)
  (setf (fdefinition 'auto-turn) (lambda (prompt) prompt)))
(unless (fboundp 'execute)
  (setf (fdefinition 'execute) (lambda (&rest arguments)
                                 (declare (ignore arguments)) nil)))
(unless (fboundp 'propose-loop)
  (setf (fdefinition 'propose-loop) (lambda (&rest arguments)
                                      (declare (ignore arguments)) nil)))
(unless (boundp '*tools*) (defparameter *tools* (vector)))

(defun modulator-state ()
  (obj "arousal" 0.4d0 "valence" 0.6d0 "certainty" 0.7d0
       "competence" 0.8d0 "boredom" 0.1d0 "social_need" 0.2d0))

(load (test-source "event-log.lisp"))
(load (test-source "tick-commit.lisp"))

(let* ((path #P"/tmp/pai-r0b-event-envelope-tests.jsonl")
       (*event-log-file* path)
       (*event-next-id* 0)
       (*event-ring* nil)
       (legacy
         (obj "id" 1 "timestamp" "2026-08-06T00:00:00Z" "type" "legacy"
              "payload" (obj "value" 1) "caused_by" :null
              "tick_id" :null "affect_snapshot" :null)))
  (ignore-errors (delete-file path))
  (unwind-protect
      (progn
        (with-open-file (out path :direction :output :if-exists :supersede
                                  :if-does-not-exist :create
                                  :external-format :utf-8)
          (let ((*print-pretty* nil))
            (write-line (shasht:write-json legacy nil) out)))
        (setf *event-next-id* 1)
        (log-event "turn-fixture" (obj "value" 2))
        (tick-terminal-call
         "fixture-tick"
         (lambda ()
           (log-event "tick-inner" (obj "value" 3))
           (obj "status" "success" "reason" "fixture" "write_count" 0))
         :generation-id "fixture-generation")
        (let* ((events (replay-events))
               (old (first events))
               (turn (second events))
               (tick-events (cddr events)))
          (r0b-check "schema-1 row remains present" (= 5 (length events)))
          (r0b-check "schema-1 row is not rewritten"
                     (and (not (nth-value 1 (gethash "schema_version" old)))
                          (= 1 (gethash "value" (gethash "payload" old)))
                          (eq :null (gethash "tick_id" old))
                          (eq :null (gethash "affect_snapshot" old))))
          (r0b-check "new envelope is schema 2"
                     (every (lambda (event)
                              (= 2 (gethash "schema_version" event -1)))
                            (rest events)))
          (r0b-check "ordinary turn does not masquerade as a tick"
                     (eq :null (gethash "tick_id" turn)))
          (r0b-check "ordinary turn carries affect snapshot"
                     (< (abs (- 0.4d0
                                (gethash "arousal"
                                         (gethash "affect_snapshot" turn))))
                        1.0d-6))
          (r0b-check "tick emits start, inner and terminal events"
                     (equal '("tick-start" "tick-inner" "tick-terminal")
                            (mapcar (lambda (event) (gethash "type" event))
                                    tick-events)))
          (r0b-check "every tick event carries the existing generation id"
                     (every (lambda (event)
                              (string= "fixture-generation"
                                       (gethash "tick_id" event)))
                            tick-events))
          (r0b-check "every tick event carries affect"
                     (every (lambda (event)
                              (hash-table-p (gethash "affect_snapshot" event)))
                            tick-events))
          (r0b-check "terminal remains caused by start"
                     (= (gethash "id" (first tick-events))
                        (gethash "caused_by" (third tick-events))))
          (let ((first-id (make-event-tick-id "fixture"))
                (second-id (make-event-tick-id "fixture")))
            (r0b-check "generated tick ids are distinct"
                       (not (string= first-id second-id))))))
    (ignore-errors (delete-file path))))

(format t "~%R0B EVENT ENVELOPE TESTS: ~d passed, ~d failed.~%"
        *r0b-pass* *r0b-fail*)
(when (plusp *r0b-fail*) (uiop:quit 1))
