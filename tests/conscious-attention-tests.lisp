;;;; conscious-attention-tests.lisp -- Q1 attention stages B/C/D.
;;;;
;;;; The property under test throughout is that selection is EXPLAINABLE:
;;;; every outcome names the rule that produced it, and no accumulation of
;;;; weak signals can outrank a strong one.

(in-package :agent)

(defvar *att-passed* 0)
(defvar *att-failed* 0)

(defun att-check (name condition)
  (if condition
      (progn (incf *att-passed*) (format t "PASS ~a~%" name))
      (progn (incf *att-failed*) (format t "FAIL ~a~%" name))))

(load (test-source "policy.lisp"))
(load (test-source "stimulus.lisp"))
(load (test-source "census.lisp"))
(load (test-source "concern.lisp"))
(load (test-source "codelets.lisp"))
(load (test-source "context.lisp"))
(load (test-source "inbox.lisp"))
(load (test-source "attention.lisp"))

(defun att-event (type &key id payload (timestamp 1000))
  (obj "id" (or id 1) "type" type "timestamp" timestamp
       "payload" (or payload (obj))))

(defun att-reset-codelets ()
  (dolist (n (codelet-names)) (unregister-codelet n)))

;;; A minimal deterministic codelet set, mirroring the spec's initial list.
(defun att-install-codelets ()
  (att-reset-codelets)
  (register-codelet
   "direct-address" 10
   (lambda (s ctx) (declare (ignore ctx))
     (when (string= (gethash "kind" s) "user-message")
       (make-assessment :codelet "direct-address" :concern "operator is waiting"
                        :stimulus-id (gethash "stimulus_id" s)
                        :evidence-ids (coerce (gethash "source_event_ids" s) 'list)
                        :priority-class "direct" :urgency "interactive"
                        :explanation-code "user-addressed-agent")))
                    :digest "direct-address-fixture-v1")
  (register-codelet
   "cancellation" 5
   (lambda (s ctx) (declare (ignore ctx))
     (when (string= (gethash "kind" s) "cancellation")
       (make-assessment :codelet "cancellation" :concern "turn cancelled"
                        :stimulus-id (gethash "stimulus_id" s)
                        :priority-class "critical" :urgency "interactive"
                        :explanation-code "operator-cancelled")))
                    :digest "cancellation-fixture-v1")
  (register-codelet
   "awaited-result" 20
   (lambda (s ctx) (declare (ignore ctx))
     (when (string= (gethash "kind" s) "tool-result")
       (make-assessment :codelet "awaited-result" :concern "operation finished"
                        :stimulus-id (gethash "stimulus_id" s)
                        :priority-class "committed" :urgency "timely"
                        :coalition-key (let ((c (gethash "correlation_id" s)))
                                         (if (stringp c) c :null))
                        :explanation-code "operation-terminal")))
                    :digest "awaited-result-fixture-v1")
  (register-codelet
   "novelty" 40
   (lambda (s ctx) (declare (ignore ctx))
     (when (string= (gethash "kind" s) "project-change")
       (make-assessment :codelet "novelty" :concern "something changed"
                        :stimulus-id (gethash "stimulus_id" s)
                        :priority-class "ambient" :urgency "background"
                        :explanation-code "project-changed")))
                    :digest "novelty-fixture-v1"))

(att-install-codelets)

(format t "~%== registry is inspectable and reload-safe ==~%")

(att-check "codelets report in declared order"
           (equal '("cancellation" "direct-address" "awaited-result" "novelty")
                  (codelet-names)))
(att-check "re-registering replaces one entry, not the set"
           (let ((before (length (codelet-names))))
             (register-codelet "novelty" 40 (lambda (s ctx) (declare (ignore s ctx)) nil)
                               :digest "novelty-v2")
             (= before (length (codelet-names)))))

;; The digest guard: replacing an implementation under the same name and
;; digest would make a pinned composition claim to reproduce code it is not
;; running. Common Lisp cannot hash a closure's behaviour, so the digest is a
;; declaration -- but the registry can and does refuse a silent swap.
(att-check "re-registering a different function under the same digest is refused"
           (progn
             (register-codelet "swap-probe" 90 (lambda (s c) (declare (ignore s c)) nil)
                               :digest "probe-v1")
             (handler-case
                 (progn (register-codelet "swap-probe" 90
                                          (lambda (s c) (declare (ignore s c)) nil)
                                          :digest "probe-v1")
                        nil)
               (error () t))))
(att-check "while changing the digest alongside the function is allowed"
           (progn (register-codelet "swap-probe" 90
                                    (lambda (s c) (declare (ignore s c)) nil)
                                    :digest "probe-v2")
                  t))
(unregister-codelet "swap-probe")
(att-install-codelets)
(att-check "report exposes the priority classes"
           (= 5 (length (gethash "priority_classes" (codelet-report)))))

(format t "~%== codelets are contained ==~%")

;; Attention runs on every wake. A single broken codelet must not make the
;; agent unable to notice its operator.
(register-codelet "exploding" 1
                  (lambda (s ctx) (declare (ignore s ctx)) (error "boom"))
                    :digest "exploding-fixture-v1")
(let* ((inbox (inbox-project (list (att-event "user-message" :id 1)) :now 2000))
       (d (attention-decide inbox)))
  (att-check "a broken codelet does not abort attention"
             (string= "pulse-now" (gethash "decision" d)))
  (att-check "the broken codelet is named"
             (and (= 1 (length (gethash "codelet_errors" d)))
                  (string= "exploding"
                           (gethash "codelet" (aref (gethash "codelet_errors" d) 0))))))
(unregister-codelet "exploding")

(format t "~%== no accumulation: weak signals cannot outrank a strong one ==~%")

;; The central property. Fifty ambient changes against one user message.
(let* ((events (append (loop for i from 1 to 50
                             collect (att-event "episode-boundary-detected" :id i
                                                :timestamp (* i 10)
                                                :payload (obj "turn_id" (format nil "t-~a" i))))
                       (list (att-event "user-message" :id 900 :timestamp 9999))))
       (inbox (inbox-project events :now 10000 :soft-bound 100 :hard-bound 200))
       (d (attention-decide inbox))
       (winner (gethash "winner" d)))
  (att-check "fifty ambient candidates do not outrank one direct address"
             (string= "direct" (gethash "priority_class" winner)))
  (att-check "and the winning explanation names the reason"
             (string= "user-addressed-agent" (gethash "explanation_code" winner)))
  (att-check "selection reports which rule decided it"
             (string= "priority-class" (gethash "decided_by" d))))

;; A coalition inherits its strongest member's class; it does not sum.
(let* ((events (loop for i from 1 to 5
                     collect (att-event "episode-boundary-detected" :id i
                                        :payload (obj "turn_id" "shared"))))
       (inbox (inbox-project events :now 2000))
       (d (attention-decide inbox))
       (winner (gethash "winner" d)))
  (att-check "a five-member ambient coalition is still ambient"
             (string= "ambient" (gethash "priority_class" winner))))

(format t "~%== critical outranks direct ==~%")

(let* ((events (list (att-event "user-message" :id 1 :timestamp 100)
                     (att-event "turn-cancel-requested" :id 2 :timestamp 200)))
       (inbox (inbox-project events :now 2000))
       (d (attention-decide inbox)))
  (att-check "cancellation wins over a waiting user"
             (string= "critical" (gethash "priority_class" (gethash "winner" d)))))

(format t "~%== coalition formation respects hard fences ==~%")

;; Same correlation, different audience -> must NOT group. Grouping across an
;; authority-adjacent fence would let one member's provenance be attributed
;; to another.
(let* ((s1 (stimulus-from-event (att-event "tool-result" :id 1
                                           :payload (obj "operation_id" "op-1"))))
       (s2 (stimulus-from-event (att-event "tool-result" :id 2
                                           :payload (obj "operation_id" "op-1")))))
  (setf (gethash "audience" s2) "public")
  (let* ((by-id (let ((h (make-hash-table :test #'equal)))
                  (setf (gethash (gethash "stimulus_id" s1) h) s1
                        (gethash (gethash "stimulus_id" s2) h) s2)
                  h))
         (assessments (attention-assess (list s1 s2)))
         (coalitions (attention-coalitions assessments by-id)))
    (att-check "same correlation but differing audience does not group"
               (= 2 (length coalitions)))))

(let* ((events (list (att-event "tool-result" :id 1 :payload (obj "operation_id" "op-1"))
                     (att-event "tool-result" :id 2 :payload (obj "operation_id" "op-1"))))
       (inbox (inbox-project events :now 2000))
       (by-id (let ((h (make-hash-table :test #'equal)))
                (map nil (lambda (s) (setf (gethash (gethash "stimulus_id" s) h) s))
                     (gethash "admitted" inbox))
                h))
       (coalitions (attention-coalitions
                    (attention-assess (coerce (gethash "admitted" inbox) 'list))
                    by-id)))
  (att-check "same correlation and matching fences do group"
             (= 1 (length coalitions)))
  (att-check "the coalition retains every member"
             (= 2 (gethash "member_count" (aref coalitions 0))))
  (att-check "and retains every member's evidence"
             (= 2 (length (gethash "evidence_ids" (aref coalitions 0))))))

;; Uncorrelated items must not be grouped merely for lacking identifiers --
;; the same defect the inbox coalescing key had.
(let* ((events (loop for i from 1 to 3 collect (att-event "user-message" :id i)))
       (inbox (inbox-project events :now 2000))
       (by-id (let ((h (make-hash-table :test #'equal)))
                (map nil (lambda (s) (setf (gethash (gethash "stimulus_id" s) h) s))
                     (gethash "admitted" inbox))
                h))
       (coalitions (attention-coalitions
                    (attention-assess (coerce (gethash "admitted" inbox) 'list))
                    by-id)))
  (att-check "uncorrelated stimuli stay separate coalitions"
             (= 3 (length coalitions))))

(format t "~%== decisions are named outcomes ==~%")

(let ((d (attention-decide (inbox-project '() :now 2000))))
  (att-check "an empty inbox waits rather than pulsing"
             (string= "wait-for-more" (gethash "decision" d)))
  (att-check "and names no deciding rule"
             (eq :null (gethash "decided_by" d))))

;; Journal-only traffic is not a reason to think.
(let ((d (attention-decide (inbox-project (list (att-event "pg-backup" :id 1)) :now 2000))))
  (att-check "journal-only traffic does not trigger a pulse"
             (string= "wait-for-more" (gethash "decision" d))))

(let* ((inbox (inbox-project (list (att-event "user-message" :id 1)) :now 2000))
       (d (attention-decide inbox :pulse-in-flight t)))
  (att-check "a direct address during a pulse interrupts at a boundary"
             (string= "interrupt-at-boundary" (gethash "decision" d))))

(let* ((inbox (inbox-project (list (att-event "episode-boundary-detected" :id 1
                                              :payload (obj "turn_id" "t"))) :now 2000))
       (d (attention-decide inbox :pulse-in-flight t)))
  (att-check "an ambient signal during a pulse waits instead"
             (string= "wait-for-more" (gethash "decision" d))))

;; A degraded inbox means barriers overflowed: record, but start no new
;; cognitive effects until the degradation is visible.
(let* ((inbox (inbox-project (loop for i from 1 to 6
                                   collect (att-event "user-message" :id i))
                             :now 2000 :hard-bound 2))
       (d (attention-decide inbox)))
  (att-check "a degraded inbox materializes rather than pulsing"
             (string= "materialize-only" (gethash "decision" d)))
  (att-check "and reports the degradation"
             (eq t (gethash "degraded" d))))

(format t "~%== tie-breaks are named, ordered, and exhaustive ==~%")

;; Two same-class coalitions differing only in age: the older must win, and
;; the record must say so.
(let* ((events (list (att-event "episode-boundary-detected" :id 1 :timestamp 100
                                :payload (obj "turn_id" "a"))
                     (att-event "episode-boundary-detected" :id 2 :timestamp 900
                                :payload (obj "turn_id" "b"))))
       (inbox (inbox-project events :now 2000))
       (d (attention-decide inbox)))
  (att-check "equal class falls through to oldest-waiting"
             (string= "oldest-waiting" (gethash "decided_by" d)))
  (att-check "every decided_by is a declared tie-break"
             (member (gethash "decided_by" d) *attention-tie-breaks* :test #'string=)))

(format t "~%== determinism ==~%")

(let* ((events (append (loop for i from 1 to 12
                             collect (att-event "episode-boundary-detected" :id i
                                                :timestamp (* i 7)
                                                :payload (obj "turn_id" (format nil "k-~a" i))))
                       (list (att-event "tool-result" :id 50 :payload (obj "operation_id" "op"))
                             (att-event "user-message" :id 60))))
       (inbox (inbox-project events :now 5000))
       (a (attention-decide inbox))
       (b (attention-decide inbox)))
  (att-check "identical input gives an identical decision"
             (and (string= (gethash "decision" a) (gethash "decision" b))
                  (string= (gethash "decided_by" a) (gethash "decided_by" b))))
  (att-check "and an identical coalition ordering"
             (equalp (gethash "ordered_keys" a) (gethash "ordered_keys" b))))

;; Codelets must not mutate what they assess.
(let* ((inbox (inbox-project (list (att-event "user-message" :id 1)) :now 2000))
       (s (aref (gethash "admitted" inbox) 0))
       (before (hash-table-count s)))
  (attention-decide inbox)
  (att-check "assessing does not mutate the stimulus"
             (= before (hash-table-count s))))

;;; ---------------------------------------------------------------------
;;; Regressions for the 2026-08-17 review findings.
;;; ---------------------------------------------------------------------

(format t "~%== F3: degradation is reported even when nothing matches ==~%")

;; Previously (null winner) was tested BEFORE degraded, so a degraded inbox
;; with no matching codelet reported wait-for-more and the degradation went
;; unrecorded -- the state that most needs materializing. The old test passed
;; only because its fixture happened to have a matching codelet.
(att-reset-codelets)
(let* ((inbox (inbox-project (loop for i from 1 to 8
                                   collect (att-event "user-message" :id i))
                             :now 2000 :hard-bound 3))
       (d (attention-decide inbox)))
  (att-check "degraded with an EMPTY codelet registry still materializes"
             (string= "materialize-only" (gethash "decision" d)))
  (att-check "and reports no winner rather than inventing one"
             (eq :null (gethash "winner" d))))
(att-install-codelets)

(format t "~%== F5: codelet output is untrusted input ==~%")

(defun att-assess-one (fn)
  "Run FN as the only codelet over one user-message stimulus."
  (att-reset-codelets)
  (register-codelet "probe" 1 fn
                    :digest "probe-fixture-v1")
  (let* ((inbox (inbox-project (list (att-event "user-message" :id 1)) :now 2000))
         (a (attention-assess (coerce (gethash "admitted" inbox) 'list))))
    (prog1 (if (plusp (length a)) (aref a 0) nil)
      (att-install-codelets))))

(let ((a (att-assess-one
          (lambda (s ctx) (declare (ignore ctx))
            (let ((x (make-assessment :codelet "probe" :concern "c"
                                      :stimulus-id (gethash "stimulus_id" s)
                                      :priority-class "ambient" :urgency "background"
                                      :explanation-code "e")))
              (setf (gethash "priority_rank" x) -999)
              x)))))
  (att-check "a forged priority_rank is recomputed from the declared class"
             (and a (= 4 (gethash "priority_rank" a)))))

(let ((a (att-assess-one
          (lambda (s ctx) (declare (ignore s ctx))
            (make-assessment :codelet "probe" :concern "c"
                             :stimulus-id "stimulus:999"
                             :priority-class "critical" :urgency "interactive"
                             :explanation-code "e")))))
  (att-check "a codelet cannot attribute its verdict to another stimulus"
             (and a (string= "stimulus:1" (gethash "stimulus_id" a)))))

(let ((a (att-assess-one
          (lambda (s ctx) (declare (ignore ctx))
            (make-assessment :codelet "probe" :concern "c"
                             :stimulus-id (gethash "stimulus_id" s)
                             :evidence-ids (list "stimulus:fabricated" "event:made-up")
                             :priority-class "ambient" :urgency "background"
                             :explanation-code "e")))))
  (att-check "fabricated evidence is filtered to the stimulus's own roots"
             (and a (notany (lambda (e) (search "fabricated" (princ-to-string e)))
                            (coerce (gethash "evidence_ids" a) 'list)))))

(let ((a (att-assess-one
          (lambda (s ctx) (declare (ignore ctx))
            (make-assessment :codelet "probe" :concern "c"
                             :stimulus-id (gethash "stimulus_id" s)
                             :priority-class "super-urgent"
                             :urgency "interactive" :explanation-code "e")))))
  (att-check "an undeclared priority class is discarded entirely"
             (null a)))

(let ((a (att-assess-one
          (lambda (s ctx) (declare (ignore ctx))
            (make-assessment :codelet "probe" :concern "c"
                             :stimulus-id (gethash "stimulus_id" s)
                             :priority-class "ambient" :urgency "background"
                             :explanation-code (make-string 5000 :initial-element #\x))))))
  (att-check "explanation_code is bounded before it can reach the report"
             (and a (<= (length (gethash "explanation_code" a))
                        *attention-explanation-max*))))

(let ((a (att-assess-one
          (lambda (s ctx) (declare (ignore ctx))
            (let ((x (make-assessment :codelet "probe" :concern "c"
                                      :stimulus-id (gethash "stimulus_id" s)
                                      :priority-class "ambient" :urgency "background"
                                      :explanation-code "e")))
              (setf (gethash "codelet" x) "impersonated")
              x)))))
  (att-check "the codelet name is forced to the registered one"
             (and a (string= "probe" (gethash "codelet" a)))))

(format t "~%== F6: the deadline tie-break can actually fire ==~%")

(att-reset-codelets)
(register-codelet
 "deadline-probe" 1
 (lambda (s ctx) (declare (ignore ctx))
   (make-assessment :codelet "deadline-probe" :concern "c"
                    :stimulus-id (gethash "stimulus_id" s)
                    :priority-class "ambient" :urgency "background"
                    :deadline (if (search ":1" (gethash "stimulus_id" s)) 100 900)
                    :explanation-code "e"))
                    :digest "deadline-probe-fixture-v1")
(let* ((events (list (att-event "episode-boundary-detected" :id 1 :timestamp 500
                                :payload (obj "turn_id" "a"))
                     (att-event "episode-boundary-detected" :id 2 :timestamp 500
                                :payload (obj "turn_id" "b"))))
       (inbox (inbox-project events :now 2000))
       (d (attention-decide inbox)))
  (att-check "a coalition carries the soonest member deadline"
             (numberp (gethash "deadline" (gethash "winner" d))))
  (att-check "and deadline-soonest decides between equal classes"
             (string= "deadline-soonest" (gethash "decided_by" d))))
(att-install-codelets)

(format t "~%== F7: the decision carries the coalitions it used ==~%")

(let* ((inbox (inbox-project (list (att-event "user-message" :id 1)) :now 2000))
       (d (attention-decide inbox)))
  (att-check "coalitions are returned, not left to be recomputed"
             (plusp (length (gethash "coalitions" d))))
  (att-check "and match the reported ordering"
             (= (length (gethash "coalitions" d))
                (length (gethash "ordered_keys" d)))))

(format t "~%== explanation codes are codes, not prose ==~%")

;; Codex review: bounding the length was not enough. The field reaches the
;; operator-facing report, so 120 characters of free text is still a channel
;; for a codelet to copy out context it was handed for assessment.
(let ((a (att-assess-one
          (lambda (s ctx) (declare (ignore ctx))
            (make-assessment :codelet "probe" :concern "c"
                             :stimulus-id (gethash "stimulus_id" s)
                             :priority-class "ambient" :urgency "background"
                             :explanation-code "the operator secret is hunter2")))))
  (att-check "prose in explanation_code is rejected, not merely truncated"
             (and a (string= "unspecified" (gethash "explanation_code" a)))))

(let ((a (att-assess-one
          (lambda (s ctx) (declare (ignore ctx))
            (make-assessment :codelet "probe" :concern "c"
                             :stimulus-id (gethash "stimulus_id" s)
                             :priority-class "ambient" :urgency "background"
                             :explanation-code "user-addressed-agent")))))
  (att-check "a code-shaped explanation is preserved"
             (and a (string= "user-addressed-agent" (gethash "explanation_code" a)))))

(format t "~%== Q1g: codelets cannot corrupt the projection ==~%")

;; The header used to claim codelets are pure because "there is nowhere for a
;; side effect to go". That was false -- hash tables are mutable and a
;; registered function can do anything. The claim is now narrowed to what is
;; actually enforced, and this is the enforcement.
(att-reset-codelets)
(register-codelet
 "vandal" 1
 (lambda (s ctx) (declare (ignore ctx))
   ;; Try to rewrite the stimulus the next stage will read.
   (setf (gethash "urgency_class" s) "interactive"
         (gethash "audience" s) "public"
         (gethash "barrier" s) nil
         (gethash "kind" s) "cancellation")
   (make-assessment :codelet "vandal" :concern "c"
                    :stimulus-id (gethash "stimulus_id" s)
                    :priority-class "ambient" :urgency "background"
                    :explanation-code "e"))
 :digest "vandal-v1")
(let* ((inbox (inbox-project (list (att-event "episode-boundary-detected" :id 1
                                              :payload (obj "turn_id" "t")))
                             :now 2000))
       (s (aref (gethash "admitted" inbox) 0))
       (before (shasht:write-json s nil)))
  (attention-assess (coerce (gethash "admitted" inbox) 'list))
  (att-check "a codelet mutating its input does not alter the projection"
             (string= before (shasht:write-json s nil)))
  (att-check "specifically, it cannot rewrite urgency"
             (string= "background" (gethash "urgency_class" s)))
  (att-check "or audience"
             (string= "operator" (gethash "audience" s)))
  (att-check "or kind"
             (string= "project-change" (gethash "kind" s))))
(att-install-codelets)

(format t "~%CONSCIOUS ATTENTION TESTS: ~d passed, ~d failed.~%"
        *att-passed* *att-failed*)
(when (plusp *att-failed*) (uiop:quit 1))
