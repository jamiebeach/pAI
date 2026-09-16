;; Quicklisp is a host-runner responsibility. The suite must not depend on one
;; user's installation path; tests/run-isolated.sh loads PAI_QUICKLISP_SETUP.
(load (test-source "agent.lisp"))
(load (test-source "self-mod.lisp"))
(load (test-source "self-model.lisp"))

(in-package :agent)

(defvar *feedback-test-pass* 0)
(defvar *feedback-test-fail* 0)
(defvar *feedback-test-next-id* 0)
(defvar *feedback-test-memory-arguments* nil)
(defvar *feedback-test-embedding-mode* :orthogonal)
(defvar *feedback-test-model-content*
  "What should we do differently when plans go sideways?")
(defvar *feedback-test-memory-writes* 0)
(defvar *feedback-test-v2-observations* nil)
(defvar *feedback-test-canary-observations* nil)

(defun feedback-test-check (name condition)
  (if condition
      (progn (incf *feedback-test-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *feedback-test-fail*) (format t "  FAIL ~a~%" name))))

(defparameter *explore-state-file* #P"/tmp/feedback-loop-no-state.json")
(defparameter *explore-current-topic* nil)
(defparameter *explore-topic-started-at* 0)
(defparameter *explore-continuation-count* 0)
(defparameter *explore-requery-seconds* 21600)
(defparameter *explore-last-stance* nil)
(defparameter *tick-handlers* (make-hash-table :test #'equal))

(defun %explore-truncate (text max-words)
  (declare (ignore max-words)) text)
(defun %explore-cosine-similarity (a b)
  (let ((dot (+ (* (first a) (first b)) (* (second a) (second b))))
        (na (sqrt (+ (expt (first a) 2) (expt (second a) 2))))
        (nb (sqrt (+ (expt (first b) 2) (expt (second b) 2)))))
    (/ dot (* na nb))))
(defun %explore-materially-novel-p (&rest arguments)
  (declare (ignore arguments)) (values t :fixture 0.0d0))
(defun %explore-defer-near-duplicate (&rest arguments)
  (declare (ignore arguments)) nil)
(defun %explore-saturate-current-topic ()
  (setf *explore-current-topic* nil
        *explore-topic-started-at* 0
        *explore-continuation-count* 0
        *explore-last-stance* nil)
  nil)
(defun %bringup-dedupe-p (&rest arguments)
  (declare (ignore arguments)) nil)
(defun unresolved-predictions () nil)
(defun continuity-buffer-append (&rest arguments)
  (declare (ignore arguments)) nil)
(defun memory-write-node (&rest arguments)
  (declare (ignore arguments))
  (incf *feedback-test-memory-writes*)
  "written-node")
(defun memory-add-edge (&rest arguments)
  (declare (ignore arguments)) nil)
(defun memory-get-node (id)
  (obj "id" id
       "kind" (if (string= id "written-node") "worldview" "observation")
       "content" (format nil "record ~a" id)))
(defun memory-search (query &rest arguments &key k kinds origins &allow-other-keys)
  (declare (ignore query))
  (setf *feedback-test-memory-arguments* arguments)
  (let ((rows
          (cond
            ((equal kinds '("observation" "episode" "self-fact"))
             (list (obj "id" "lived-1" "kind" "observation"
                        "origin_class" "lived-agent-action"
                        "content" "Went to the cinema.")
                   (obj "id" "lived-2" "kind" "episode"
                        "origin_class" "synthetic"
                        "content" "Tickets had the wrong date.")))
            ((equal kinds '("episode"))
             (list (obj "id" "episode-1" "kind" "episode"
                        "origin_class" "synthetic"
                        "content" "The cinema visit ended at the ticket desk.")))
            ((equal kinds '("self-fact"))
             (list (obj "id" "self-1" "kind" "self-fact"
                        "origin_class" "lived-agent-action"
                        "content" "I prefer checking concrete details.")))
            ((and (equal kinds '("observation"))
                  (equal origins '("lived-agent-action" "tool-result")))
             (list (obj "id" "agent-1" "kind" "observation"
                        "origin_class" "lived-agent-action"
                        "content" "I checked the ticket date.")
                   (obj "id" "agent-2" "kind" "observation"
                        "origin_class" "tool-result"
                        "content" "The listed date was yesterday.")))
            ((and (equal kinds '("observation"))
                  (equal origins '("lived-user")))
             (list (obj "id" "user-1" "kind" "observation"
                        "origin_class" "lived-user"
                        "content" "the operator said the tickets had the wrong date.")))
            (t (error "unexpected retrieval policy: ~s / ~s" kinds origins)))))
    (subseq rows 0 (min (or k (length rows)) (length rows)))))
(defun memory-recall (query &rest arguments)
  (declare (ignore query arguments)) nil)
(defun %recall-ambient (query &rest arguments &key kinds &allow-other-keys)
  (declare (ignore query))
  (setf *feedback-test-memory-arguments* arguments)
  (unless (equal kinds '("observation" "episode" "self-fact"))
    (error "unexpected ambient kinds"))
  (list (obj "id" "lived-1" "kind" "observation" "content" "Went to the cinema.")
        (obj "id" "lived-2" "kind" "episode" "content" "Tickets had the wrong date.")))
(defun raw-call-model (&rest arguments)
  (declare (ignore arguments))
  (obj "choices" (vector (obj "message" (obj "content"
                                                   *feedback-test-model-content*)))))
(defun embed-text (text)
  (cond
    ((eq *feedback-test-embedding-mode* :same) '(1.0d0 0.0d0))
    ((search "candidate" text :test #'char-equal) '(0.7d0 0.714142842854285d0))
    ((search "existing" text :test #'char-equal) '(1.0d0 0.0d0))
    (t '(0.0d0 1.0d0))))

;; Replace the persistence-validating writer with a deterministic in-memory
;; seam. The containment layer still exercises versioning, lifecycle, and cap.
(defun self-model-propose-revision (section statement evidence-node-ids)
  (incf *feedback-test-next-id*)
  (let ((entry (obj "id" *feedback-test-next-id* "statement" statement
                    "evidence-node-ids" (coerce evidence-node-ids 'vector)
                    "created-at" (get-universal-time))))
    (push entry (gethash section *self-model*))
    (values entry nil)))
(defun save-self-model () nil)
(defun save-explore-state () nil)
(defun initiative-v2-observe-trigger (content evidence &rest arguments)
  (let ((decision
          (obj "id" "decision-1"
               "selected_action_type" "outward-message"
               "result" "approved-not-delivered")))
    (push (list content evidence arguments decision)
          *feedback-test-v2-observations*)
    decision))
(defun reciprocity-canary-consider-observation
    (source content evidence decision &rest arguments)
  (push (list source content evidence decision arguments)
        *feedback-test-canary-observations*)
  t)

(load (test-source "feedback-loop-containment.lisp"))

(defun feedback-test-reset-model ()
  (setf *self-model* (let ((model (obj)))
                       (dolist (section *self-model-sections*)
                         (setf (gethash section model) nil))
                       model)
        *feedback-test-next-id* 0))

(format t "~%== bounded reports and lifecycle ==~%")
(feedback-test-reset-model)
(dotimes (index 15)
  (push (obj "id" index "statement" (format nil "question ~a" index)
             "evidence-node-ids" (vector "e"))
        (gethash "open-questions" *self-model*)))
(setf (gethash "status" (first (gethash "open-questions" *self-model*))) "parked")
(let ((reported (gethash "open-questions" (self-model-report))))
  (feedback-test-check "conversational report caps active questions at eight"
                       (= 8 (length reported)))
  (feedback-test-check "parked question is absent from conversational report"
                       (not (find "question 14" reported :test #'string=))))
(feedback-test-check "full audit preserves every historical question"
                     (= 15 (length (gethash "open-questions"
                                           (gethash "sections" (self-model-audit-report))))))
(let* ((before (mapcar (lambda (entry) (gethash "status" entry))
                       (gethash "open-questions" *self-model*)))
       (plan (self-model-question-migration-plan :keep-limit 3))
       (after (mapcar (lambda (entry) (gethash "status" entry))
                      (gethash "open-questions" *self-model*))))
  (feedback-test-check "legacy migration planning is read-only"
                       (equal before after))
  (feedback-test-check "migration plan retains every legacy operation for audit"
                       (= 14 (gethash "operation_count" plan)))
  (multiple-value-bind (result reason)
      (self-model-apply-question-migration plan "not-approved")
    (feedback-test-check "migration apply rejects an incorrect confirmation"
                         (and (null result) (search "confirmation" reason)))))

(format t "~%== same-root versioning ==~%")
(feedback-test-reset-model)
(multiple-value-bind (first reason)
    (self-model-upsert-open-question "Root -- first" '("worldview-1")
                                     "Root" '("lived-1" "lived-2"))
  (declare (ignore reason))
  (multiple-value-bind (second second-reason)
      (self-model-upsert-open-question "Root -- second" '("worldview-2")
                                       "Root" '("lived-1" "lived-3"))
    (declare (ignore second-reason))
    (feedback-test-check "prior root version is superseded"
                         (string= "superseded" (gethash "status" first)))
    (feedback-test-check "new root version is the only active version"
                         (and (string= "open" (gethash "status" second))
                              (= 1 (length (self-model-active-open-questions)))))))

(format t "~%== root semantic and lineage novelty ==~%")
(feedback-test-reset-model)
(push (obj "id" 1 "statement" "existing root -- stance" "status" "open"
           "root-topic-id" "existing root"
           "root-evidence-node-ids" (vector "lived-1" "lived-2"))
      (gethash "open-questions" *self-model*))
(setf *feedback-test-embedding-mode* :same)
(multiple-value-bind (novel reason)
    (%feedback-question-novel-p "a reworded root" '("other-1" "other-2"))
  (feedback-test-check "semantic sibling root is rejected"
                       (and (not novel) (eq reason :root-near-duplicate))))
(setf *feedback-test-embedding-mode* :orthogonal)
(multiple-value-bind (novel reason)
    (%feedback-question-novel-p "candidate root" '("lived-1" "lived-2"))
  (feedback-test-check "same-lineage moderately similar root is rejected"
                       (and (not novel) (eq reason :root-near-duplicate))))
(multiple-value-bind (novel reason)
    (%feedback-question-novel-p "unrelated root" '("other-1" "other-2"))
  (feedback-test-check "distinct root with distinct evidence is accepted"
                       (and novel (eq reason :new-root))))
(feedback-test-check "observed recursive paradox root is rejected deterministically"
                     (%feedback-recursive-root-p
                      "Why should I engage in this exercise if it captures my thoughts?"))
(multiple-value-bind (question reason)
    (%feedback-root-question-valid-p
     "What should we do differently when plans go sideways?")
  (feedback-test-check "one concrete short question passes the root contract"
                       (and question (eq reason :valid-root))))
(multiple-value-bind (question reason)
    (%feedback-root-question-valid-p
     (format nil "I don't actually have memory of our previous conversations.~%Each time we talk, I start fresh - no continuity."))
  (feedback-test-check "observed continuity disclaimer fails the root contract"
                       (and (null question)
                            (member reason '(:not-one-question
                                             :recursive-or-identity-root)))))
(multiple-value-bind (stance reason)
    (%feedback-stance-valid-p
     "I will check the ticket date before treating the next cinema plan as settled.")
  (feedback-test-check "concrete declarative stance passes"
                       (and stance (eq reason :valid-stance))))
(multiple-value-bind (stance reason)
    (%feedback-stance-valid-p
     "I am uncertain whether my uncertainty is real?")
  (feedback-test-check "identity-loop question fails the stance contract"
                       (and (null stance)
                            (eq reason :recursive-or-identity-stance))))

(format t "~%== grounded seeding and continuation bound ==~%")
(setf *feedback-test-memory-arguments* nil)
(feedback-test-check "grounded recall returns direct lived kinds only"
                     (= 2 (length (%feedback-grounded-recall "test" :k 4))))
(feedback-test-check "grounded recall passes the fixed kind allowlist"
                     (equal (getf *feedback-test-memory-arguments* :kinds)
                            '("observation" "episode" "self-fact")))
(let* ((evidence (%feedback-root-evidence "test" :k 6))
       (user-count
         (count "lived-user" evidence :test #'string=
                :key (lambda (row) (gethash "origin_class" row))))
       (non-user-count (- (length evidence) user-count)))
  (feedback-test-check "root seed includes at most one raw user turn"
                       (<= user-count 1))
  (feedback-test-check "root seed requires multiple non-user records"
                       (>= non-user-count 2)))
(feedback-test-reset-model)
(setf *explore-current-topic* nil *explore-current-root-evidence-ids* nil
      *explore-continuation-count* 0 *feedback-test-embedding-mode* :orthogonal)
(multiple-value-bind (question continuing) (%explore-pick-question)
  (feedback-test-check "fresh root is generated from balanced typed records"
                       (and question (not continuing)
                            (>= (length *explore-current-root-evidence-ids*) 4))))
(multiple-value-bind (question continuing) (%explore-pick-question)
  (feedback-test-check "one continuation reuses the managed root"
                       (and question continuing
                            (= 1 *explore-continuation-count*))))
(let ((report (feedback-loop-containment-report)))
  (feedback-test-check "report exposes the typed balanced root policy"
                       (and (string= "typed-balanced-no-legacy-fallback"
                                     (gethash "root_evidence_policy" report))
                            (= 1 (gethash "root_user_record_limit" report)))))
(feedback-test-reset-model)
(setf *explore-current-topic* "What concrete change should follow?"
      *explore-current-root-evidence-ids* '("lived-1" "lived-2")
      *explore-topic-started-at* (get-universal-time)
      *explore-continuation-count* 0
      *feedback-test-memory-writes* 0
      *feedback-test-model-content*
      "I don't actually have memory, so am I merely pattern matching?")
(%feedback-run-explore *explore-current-topic* nil)
(feedback-test-check "invalid stance reaches no memory or self-model write"
                     (and (zerop *feedback-test-memory-writes*)
                          (null (gethash "open-questions" *self-model*))))
(feedback-test-check "invalid stance clears the transient root pointer"
                     (null *explore-current-topic*))
(setf *feedback-test-model-content*
      "What should we do differently when plans go sideways?")

(format t "~%== final explore handler initiative integration ==~%")
(feedback-test-reset-model)
(setf *feedback-test-v2-observations* nil
      *feedback-test-canary-observations* nil
      *feedback-test-memory-writes* 0
      *explore-current-topic* "What should change after the cinema mix-up?"
      *explore-current-root-evidence-ids* '("lived-1" "lived-2"))
(let* ((question *explore-current-topic*)
       (stance "I will verify the ticket date before treating a cinema plan as settled.")
       (evidence (list (memory-get-node "lived-1")
                       (memory-get-node "lived-2"))))
  (%feedback-commit-explore question nil evidence stance)
  (let* ((v2-call (first *feedback-test-v2-observations*))
         (v2-evidence (second v2-call))
         (v2-arguments (third v2-call))
         (decision (fourth v2-call))
         (canary-call (first *feedback-test-canary-observations*)))
    (feedback-test-check "final handler emits exactly one v2 observation"
                         (= 1 (length *feedback-test-v2-observations*)))
    (feedback-test-check "v2 observation identifies the explore source and topic"
                         (and (string= "explore-development"
                                       (getf v2-arguments :trigger-type))
                              (equal '("written-node")
                                     (getf v2-arguments :trigger-event-ids))
                              (string= question (getf v2-arguments :topic))))
    (feedback-test-check "v2 evidence includes the committed worldview node"
                         (find "written-node" v2-evidence :test #'string=
                               :key (lambda (row) (gethash "id" row))))
    (feedback-test-check "final handler forwards the same decision to the canary"
                         (and (= 1 (length *feedback-test-canary-observations*))
                              (string= "explore-development" (first canary-call))
                              (eq decision (fourth canary-call))
                              (string= "written-node"
                                       (getf (fifth canary-call) :source-id))))))

(format t "~%== resolvable rumination contract ==~%")
(multiple-value-bind (reflection disposition)
    (%feedback-parse-rumination
     (format nil "Reflection: The ticket date is the concrete issue.~%Disposition: answered"))
  (feedback-test-check "rumination parser accepts an answered disposition"
                       (and (string= reflection "The ticket date is the concrete issue.")
                            (string= disposition "answered"))))
(feedback-test-check "rumination parser rejects recursive free-form output"
                     (null (%feedback-parse-rumination
                            "I remain uncertain about uncertainty.")))

(format t "~%~a passed, ~a failed~%" *feedback-test-pass* *feedback-test-fail*)
(when (plusp *feedback-test-fail*) (sb-ext:exit :code 1))
