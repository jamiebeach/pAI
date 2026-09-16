;;;; conscious-motivation-runtime-tests.lisp -- Q5M2 durable attention seam.
;;;;
;;;; Written before motivation-runtime.lisp. The first run must fail because
;;;; the durable candidate reconciler is absent.

(in-package :agent)

(dolist (file '("policy.lisp" "stimulus.lisp" "census.lisp"
                "concern.lisp" "codelets.lisp" "context.lisp" "inbox.lisp"
                "attention.lisp" "mind/conscious/lifecycle.lisp"
                "lifecycle-semantics.lisp" "motivation.lisp"))
  (load (test-source file)))

(defvar *q5mr-pass* 0)
(defvar *q5mr-fail* 0)
(defvar *q5mr-events* nil)
(defvar *q5mr-next-id* 0)
(defvar *q5mr-unreadable-append-p* nil)
(defparameter *agent-id* "q5m-runtime-dev")

(defun q5mr-check (name condition)
  (if condition
      (progn (incf *q5mr-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *q5mr-fail*) (format t "  FAIL ~a~%" name))))

(defun replay-events (&rest ignored)
  (declare (ignore ignored))
  (copy-list *q5mr-events*))

(defun log-event (type payload &key caused-by)
  (let* ((id (incf *q5mr-next-id*))
         (event
           (obj "schema_version" 1 "id" id "timestamp" (+ 1000 id)
                "type" type "agent_id" *agent-id*
                "caused_by" (or caused-by :null) "payload" payload)))
    (unless *q5mr-unreadable-append-p*
      (setf *q5mr-events* (append *q5mr-events* (list event))))
    id))

(defun q5mr-evidence ()
  (log-event "fixture-evidence" (obj "fixture" t)))

(defun q5mr-observe (request kind root at)
  (log-event
   "conscious-curiosity-observed"
   (conscious-curiosity-observation-payload
    :request-id request :mind-identity-id "dev-persona"
    :subject-type "topic" :subject-label "Private subject words"
    :subject-refs '("topic:stable") :reinforcement-kind kind
    :supporting-event-ids (list root) :source-revision "q5m-fixture-v1"
    :actor-runtime-revision "q5m-runtime-v1" :observed-at at)))

(defun q5mr-build-salient-history ()
  (setf *q5mr-events* nil *q5mr-next-id* 0 *q5mr-unreadable-append-p* nil)
  (let ((a (q5mr-evidence)))
    (q5mr-observe "obs-a" "novel-observation" a 100))
  (let ((b (q5mr-evidence)))
    (q5mr-observe "obs-b" "operator-interest" b 200))
  (let ((c (q5mr-evidence)))
    (q5mr-observe "obs-c" "unresolved-recurrence" c 300)))

(format t "~%== Q5M durable curiosity candidate seam ==~%")

(let ((runtime
        (merge-pathnames "src/mind/conscious/motivation-runtime.lisp"
                         *pai-root*)))
  (q5mr-check "candidate runtime adapter exists" (probe-file runtime))
  (when (probe-file runtime)
    (load runtime)
    (conscious-motivation-codelet-install)
    ;; Incumbent direct-address interpretation, supplied explicitly just as a
    ;; pinned runtime composition does. Q5M owns the curiosity codelet only.
    (register-codelet
     "q5mr-direct-address" 10
     (lambda (stimulus context)
       (declare (ignore context))
       (when (string= "user-message" (gethash "kind" stimulus ""))
         (make-assessment
          :codelet "q5mr-direct-address" :concern "operator is waiting"
          :stimulus-id (gethash "stimulus_id" stimulus)
          :evidence-ids (coerce (gethash "source_event_ids" stimulus) 'list)
          :priority-class "direct" :urgency "interactive"
          :explanation-code "user-addressed-agent")))
     :digest "q5mr-direct-address-v1")
    (q5mr-check "candidate reconciliation surface exists"
                (and (fboundp 'conscious-motivation-runtime-reconcile)
                     (fboundp 'conscious-motivation-runtime-report)))

    (q5mr-build-salient-history)
    (let* ((report
             (conscious-motivation-runtime-reconcile
              *agent-id* :actor-runtime-revision "q5m-runtime-v1"
              :origin-runtime-revision "q5m-runtime-v1" :now 400))
           (candidate-event
             (find "conscious-curiosity-candidate-raised" *q5mr-events*
                   :key (lambda (event) (gethash "type" event))
                   :test #'string=))
           (payload (and candidate-event (gethash "payload" candidate-event))))
      (q5mr-check "one salient motive appends one durable candidate"
                  (and (= 1 (gethash "examined_count" report))
                       (= 1 (gethash "appended_count" report))
                       candidate-event))
      (q5mr-check "durable candidate contains no subject prose or authority"
                  (and payload
                       (not (nth-value 1 (gethash "subject_label" payload)))
                       (not (nth-value 1 (gethash "subject_refs" payload)))
                       (not (nth-value 1 (gethash "tool" payload)))
                       (not (nth-value 1 (gethash "audience" payload)))
                       (not (search "Private subject words"
                                    (%stimulus-canonical-json payload)))))
      (let ((before (length *q5mr-events*))
            (recovered
              (conscious-motivation-runtime-reconcile
               *agent-id* :actor-runtime-revision "q5m-runtime-v2"
               :origin-runtime-revision "q5m-runtime-v1" :now 500)))
        (q5mr-check "same origin revision recovers without another append"
                    (and (zerop (gethash "appended_count" recovered))
                         (= 1 (gethash "recovered_count" recovered))
                         (= before (length *q5mr-events*)))))

      (let* ((context
               (make-projection-context
                :now 500 :agent-id *agent-id*
                :runtime-revision "q5m-runtime-v1" :consumer "q5m-runtime"))
             (stimulus (stimulus-from-event candidate-event
                                            :agent-id *agent-id*))
             (inbox (inbox-project *q5mr-events* :context context)))
        (q5mr-check "actual stimulus and inbox consumers admit background cue"
                    (and stimulus
                         (string= "intention-cue" (gethash "kind" stimulus))
                         (string= "background"
                                  (gethash "urgency_class" stimulus))
                         (= 1 (gethash "admitted_count" inbox))))
        (log-event
         "user-message"
         (obj "text" "operator interruption"
              "origin_runtime_revision" "q5m-runtime-v1"))
        (let* ((interrupted (inbox-project *q5mr-events* :context context))
               (decision (attention-decide interrupted :context context
                                           :pulse-in-flight t)))
          (q5mr-check "interactive message interrupts while curiosity remains"
                      (and (= 2 (gethash "admitted_count" interrupted))
                           (string= "interrupt-at-boundary"
                                    (gethash "decision" decision)))))
        (log-event
         *inbox-consumption-event-type*
         (obj "stimulus_ids"
              (vector (gethash "stimulus_id" stimulus))
              "agent_id" *agent-id* "consumer" "q5m-runtime"
              "disposition" "handled"))
        (let ((consumed (inbox-project *q5mr-events* :context context))
              (before (length *q5mr-events*)))
          (conscious-motivation-runtime-reconcile
           *agent-id* :actor-runtime-revision "q5m-runtime-v2"
           :origin-runtime-revision "q5m-runtime-v1" :now 600)
          (q5mr-check "ordinary receipt consumes and restart does not recreate"
                      (and (= 1 (gethash "consumed_count" consumed))
                           (= before (length *q5mr-events*)))))))

    (q5mr-build-salient-history)
    (setf *q5mr-unreadable-append-p* t)
    (q5mr-check "append without durable reread fails closed"
                (handler-case
                    (progn
                      (conscious-motivation-runtime-reconcile
                       *agent-id* :actor-runtime-revision "q5m-runtime-v1"
                       :origin-runtime-revision "q5m-runtime-v1" :now 400)
                      nil)
                  (error () t)))
    (setf *q5mr-unreadable-append-p* nil)))

    (q5mr-build-salient-history)
    (conscious-motivation-runtime-reconcile
     *agent-id* :actor-runtime-revision "q5m-runtime-v1"
     :origin-runtime-revision "q5m-runtime-v1" :now 400)
    (let* ((stored
             (find "conscious-curiosity-candidate-raised" *q5mr-events*
                   :key (lambda (event) (gethash "type" event))
                   :test #'string=))
           (payload (gethash "payload" stored)))
      (setf (gethash "candidate_id" payload) "candidate:tampered")
      (q5mr-check "conflicting durable request content fails closed"
                  (handler-case
                      (progn
                        (conscious-motivation-runtime-reconcile
                         *agent-id* :actor-runtime-revision "q5m-runtime-v2"
                         :origin-runtime-revision "q5m-runtime-v1" :now 500)
                        nil)
                    (error () t))))

(format t "~%~d passed, ~d failed~%" *q5mr-pass* *q5mr-fail*)
(when (plusp *q5mr-fail*)
  (error "Q5M motivation runtime tests failed"))
