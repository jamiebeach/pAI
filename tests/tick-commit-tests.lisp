(in-package :agent)

(ql:quickload '(:bordeaux-threads :postmodern) :silent t)

(defvar *tick-commit-test-pass* 0)
(defvar *tick-commit-test-fail* 0)

(defun tick-commit-test-check (name condition)
  (if condition
      (progn (incf *tick-commit-test-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *tick-commit-test-fail*) (format t "  FAIL ~a~%" name))))

(defun tick-commit-test-memory (&key (content "A grounded novel result.")
                                     (record-type "supported-inference")
                                     (evidence '("lived-root-1" "lived-root-2"))
                                     (topic "fixture") role target time-horizon
                                     evidence-kinds)
  (let ((spec (obj "kind" "thought" "content" content
                   "record_type" record-type
                   "evidence_node_ids" (coerce evidence 'vector)
                   "uncertainty" 0.2d0 "topic" topic)))
    (when role (setf (gethash "role" spec) role))
    (when target (setf (gethash "target" spec) target))
    (when time-horizon (setf (gethash "time_horizon" spec) time-horizon))
    (when evidence-kinds
      (setf (gethash "evidence_kinds" spec) (coerce evidence-kinds 'vector)))
    spec))

(defun tick-commit-test-proposal (type &optional memories)
  (make-tick-proposal (format nil "g-~a" type) type
                      :memory-specs memories
                      :continuity-facts (list (obj "fact_type" "test" "value" 1))))

(load (test-source "cognitive-call.lisp"))
(load (test-source "tick-commit.lisp"))

(let ((*tick-commit-similarity-fn*
        (lambda (left right) (if (string= left right) 1.0d0 0.0d0)))
      (*tick-commit-recent-records-fn*
        (lambda (topic type) (declare (ignore topic type)) nil)))
  (format t "~%== handler contracts ==~%")
  (let ((cases
          (list
           (tick-commit-test-proposal
            "idle-drift" (list (tick-commit-test-memory
                                 :record-type "hypothesis" :evidence '("lived-root-1"))))
           (tick-commit-test-proposal "light-consolidate"
                                      (list (tick-commit-test-memory)))
           (tick-commit-test-proposal "full-reflection"
                                      (list (tick-commit-test-memory)))
           (tick-commit-test-proposal
            "anticipate" (list (tick-commit-test-memory
                                :record-type "prediction" :target "meeting"
                                :time-horizon "next week")))
           (tick-commit-test-proposal
            "ruminate" (list (tick-commit-test-memory
                              :record-type "hypothesis"
                              :evidence-kinds '("observation"))))
           (tick-commit-test-proposal
            "curiosity"
            (list (tick-commit-test-memory :content "Question" :role "search-question")
                  (tick-commit-test-memory :content "External" :role "external-result")
                  (tick-commit-test-memory :content "Synthesis" :role "synthesis")))
           (tick-commit-test-proposal "explore" (list (tick-commit-test-memory)))
           (tick-commit-test-proposal
            "episode-replay" (list (tick-commit-test-memory
                                    :evidence-kinds '("episode"))))
           (tick-commit-test-proposal "maintenance" nil))))
    (dolist (proposal cases)
      (tick-commit-test-check
       (format nil "~a validates" (gethash "tick_type" proposal))
       (null (tick-commit-validate proposal)))))

  (format t "~%== fail-closed rules ==~%")
  (tick-commit-test-check
   "idle requires hypothesis"
   (plusp (length (tick-commit-validate
                    (tick-commit-test-proposal
                     "idle-drift" (list (tick-commit-test-memory
                                         :evidence '("lived-root-1"))))))))
  (tick-commit-test-check
   "consolidation requires two roots"
   (plusp (length (tick-commit-validate
                    (tick-commit-test-proposal
                     "light-consolidate"
                     (list (tick-commit-test-memory :evidence '("one"))))))))
  (tick-commit-test-check
   "anticipation requires resolvable target and horizon"
   (plusp (length (tick-commit-validate
                    (tick-commit-test-proposal
                     "anticipate" (list (tick-commit-test-memory
                                         :record-type "prediction")))))))
  (tick-commit-test-check
   "rumination cannot cite only rumination"
   (plusp (length (tick-commit-validate
                    (tick-commit-test-proposal
                     "ruminate" (list (tick-commit-test-memory
                                       :record-type "hypothesis"
                                       :evidence-kinds '("rumination"))))))))
  (tick-commit-test-check
   "curiosity keeps question result synthesis distinct"
   (plusp (length (tick-commit-validate
                    (tick-commit-test-proposal
                     "curiosity" (list (tick-commit-test-memory
                                        :role "synthesis")))))))
  (tick-commit-test-check
   "maintenance cannot generate"
   (plusp (length (tick-commit-validate
                    (tick-commit-test-proposal
                     "maintenance" (list (tick-commit-test-memory)))))))
  (tick-commit-test-check
   "identity confusion fails closed"
   (plusp (length (tick-commit-validate
                    (tick-commit-test-proposal
                     "explore" (list (tick-commit-test-memory
                                      :content "I am the operator.")))))))
  (let ((direct (tick-commit-test-memory :content "Untrusted external result."
                                         :record-type "direct-event"
                                         :evidence nil :role "external-result")))
    (setf (gethash "origin_class" direct) "external-signal"
          (gethash "source_event_id" direct) 42)
    (tick-commit-test-check
     "typed external signal validates without synthetic lineage"
     (null (tick-commit-validate
            (tick-commit-test-proposal "explore" (list direct))))))

  (format t "~%== novelty, modes, cap, and atomic dependency boundary ==~%")
  (let* ((duplicate (tick-commit-test-proposal
                     "explore"
                     (list (tick-commit-test-memory :content "same")
                           (tick-commit-test-memory :content "same"))))
         (*tick-commit-recent-count-fn* (lambda () 0)))
    (tick-commit-test-check
     "same-topic pair at threshold is rejected"
     (string= "rejected" (gethash "status" (tick-commit-apply duplicate 1)))))
  (let* ((proposal (tick-commit-test-proposal "explore"
                                              (list (tick-commit-test-memory))))
         (transactions 0)
         (*tick-commit-recent-count-fn* (lambda () 0))
         (*tick-commit-transaction-fn*
           (lambda (p event) (declare (ignore p event))
             (incf transactions) '("new-node"))))
    (tick-commit-test-check
     "shadow-only validates without transaction"
     (let ((result (tick-commit-apply proposal 1 :mode :shadow-only)))
       (and (string= "shadow-valid" (gethash "status" result))
            (zerop transactions))))
    (tick-commit-test-check
     "paused mode writes nothing"
     (let ((result (tick-commit-apply proposal 1 :mode :paused)))
       (and (string= "skipped" (gethash "status" result))
            (zerop transactions)))))
  (let* ((proposal (tick-commit-test-proposal "explore"
                                              (list (tick-commit-test-memory))))
         (*tick-commit-recent-count-fn* (lambda () 6))
         (*tick-commit-transaction-fn*
           (lambda (p event) (declare (ignore p event)) (error "cap failed"))))
    (tick-commit-test-check
     "six-per-hour cap leaves extra output audit-only"
     (string= "audit-only"
              (gethash "status" (tick-commit-apply proposal 1 :mode :normal)))))
  (let* ((direct (tick-commit-test-memory :content "Direct external."
                                          :record-type "direct-event"
                                          :evidence nil :role "external-result"))
         (transactions 0)
         (*tick-commit-recent-count-fn* (lambda () 6))
         (*tick-commit-transaction-fn*
           (lambda (proposal event) (declare (ignore proposal event))
             (incf transactions) '("direct"))))
    (setf (gethash "origin_class" direct) "external-signal"
          (gethash "source_event_id" direct) 42)
    (tick-commit-apply (tick-commit-test-proposal "explore" (list direct))
                       1 :mode :normal)
    (tick-commit-test-check "direct signals do not consume synthetic cap"
                            (= transactions 1)))
  (let* ((proposal (tick-commit-test-proposal "explore"
                                              (list (tick-commit-test-memory))))
         (*tick-commit-recent-count-fn* (lambda () (error "count unavailable"))))
    (tick-commit-test-check
     "unavailable cap fails closed to audit-only"
     (string= "synthetic-cap-unavailable"
              (gethash "reason" (tick-commit-apply proposal 1 :mode :normal)))))
  (let* ((proposal (tick-commit-test-proposal "explore"
                                              (list (tick-commit-test-memory))))
         (dependent-calls 0)
         (*tick-commit-recent-count-fn* (lambda () 0))
         (*tick-commit-transaction-fn*
           (lambda (p event) (declare (ignore p event)) (error "forced admission")))
         (*tick-commit-dependent-fn*
           (lambda (p ids) (declare (ignore p ids)) (incf dependent-calls))))
    (let ((result (tick-commit-apply proposal 1 :mode :normal)))
      (tick-commit-test-check "admission failure is explicit"
                              (string= "error" (gethash "status" result)))
      (tick-commit-test-check "dependent state waits for memory commit"
                              (zerop dependent-calls))))

  (format t "~%== 100 simulated commits and terminal pairing ==~%")
  (let* ((events nil) (transactions 0) (dependencies 0)
        (*tick-commit-recent-count-fn* (lambda () 0))
        (*tick-commit-transaction-fn*
          (lambda (proposal event)
            (declare (ignore event)) (incf transactions)
            (list (format nil "node-~a" (gethash "generation_id" proposal)))))
        (*tick-commit-dependent-fn*
          (lambda (proposal ids) (declare (ignore proposal ids))
            (incf dependencies)))
        (*tick-terminal-event-fn*
          (lambda (type payload caused-by)
            (push (list type payload caused-by) events) (length events))))
    (dotimes (index 100)
      (let* ((proposal
               (make-tick-proposal
                (format nil "sim-~a" index) "explore"
                :memory-specs
                (list (tick-commit-test-memory
                       :content (format nil "novel result ~a" index)))))
             (result
               (tick-terminal-call
                "explore"
                (lambda () (tick-commit-apply proposal 1 :mode :normal))
                :generation-id (format nil "sim-~a" index))))
        (tick-commit-test-check (format nil "simulated commit ~a" index)
                                (string= "committed" (gethash "status" result)))))
    (tick-commit-test-check "100 transactions committed" (= 100 transactions))
    (tick-commit-test-check "100 dependent applications followed" (= 100 dependencies))
    (tick-commit-test-check "100 starts emitted"
                            (= 100 (count "tick-start" events :key #'first :test #'string=)))
    (tick-commit-test-check "100 terminals emitted"
                            (= 100 (count "tick-terminal" events :key #'first :test #'string=)))
    (tick-commit-test-check
     "every terminal is correlated"
     (every #'third (remove-if-not (lambda (event)
                                    (string= "tick-terminal" (first event))) events))))
  (let* ((events nil)
        (*tick-terminal-event-fn*
          (lambda (type payload caused-by)
            (push (list type payload caused-by) events) (length events))))
    (handler-case (tick-terminal-call "explore" (lambda () (error "forced")))
      (error () nil))
    (tick-commit-test-check "exception still has one terminal"
                            (= 1 (count "tick-terminal" events
                                        :key #'first :test #'string=)))
    (tick-commit-test-check
     "exception terminal is error"
     (string= "error"
              (gethash "status" (second (find "tick-terminal" events
                                               :key #'first :test #'string=)))))))

(format t "~%~a passed, ~a failed~%" *tick-commit-test-pass* *tick-commit-test-fail*)
(when (plusp *tick-commit-test-fail*) (sb-ext:exit :code 1))
