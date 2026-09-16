(in-package :agent)

(ql:quickload '(:bordeaux-threads :postmodern) :silent t)

(defvar *tick-proposal-test-pass* 0)
(defvar *tick-proposal-test-fail* 0)
(defun tick-proposal-check (name condition)
  (if condition
      (progn (incf *tick-proposal-test-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *tick-proposal-test-fail*) (format t "  FAIL ~a~%" name))))

(unless (fboundp 'raw-call-model)
  (defun raw-call-model (messages) (declare (ignore messages)) (error "legacy raw path called")))
(unless (boundp '*importance-since-last-reflection*)
  (defvar *importance-since-last-reflection* 0.0d0))
(unless (boundp '*reflection-importance-threshold*)
  (defvar *reflection-importance-threshold* 4.0d0))

(load (test-source "cognitive-call.lisp"))
(load (test-source "tick-commit.lisp"))
(load (test-source "tick-proposals.lisp"))

(tick-proposal-check
 "proposal builders contain no direct raw-call-model"
 (not (search "raw-call-model"
              (string-downcase
               (uiop:read-file-string (namestring (test-source "tick-proposals.lisp")))))))

(defun tick-proposal-fixture-evidence (query k kinds)
  (declare (ignore query))
  (loop for index below k
        collect (obj "id" (format nil "root-~a" index)
                     "kind" (if kinds (first kinds) "observation")
                     "content" (format nil "Grounded evidence ~a" index)
                     "origin_class" "lived-user"
                     "epistemic_status" "user-report"
                     "grounding_status" "grounded"
                     "producer" "fixture" "root_observation_ids" (vector)
                     "quarantined" nil)))

(let ((cognitive-calls 0) (search-calls 0) (events 0) (questions nil)
      (raw-before (fdefinition 'raw-call-model)))
  (labels ((accepted (purpose evidence &rest keys)
             (incf cognitive-calls)
             (push (getf keys :question) questions)
             (let ((record-type
                     (cond ((string= purpose "idle-association") "hypothesis")
                           ((string= purpose "anticipation") "prediction")
                           ((string= purpose "rumination") "hypothesis")
                           (t "supported-inference"))))
               (obj "status" "accepted"
                    "record"
                    (obj "speaker" "the agent" "human" "the operator"
                         "purpose" purpose "record_type" record-type
                         "content" (if (and (string= purpose "curiosity-synthesis")
                                            (= (length evidence) 3))
                                       "grounded search query"
                                       (format nil "Novel grounded ~a result ~a"
                                               purpose cognitive-calls))
                         "evidence_node_ids"
                         (coerce (mapcar (lambda (node) (gethash "id" node)) evidence)
                                 'vector)
                         "uncertainty" 0.2d0
                         "novel_contribution" "fixture novelty"
                         "proposed_next_operation" "none")))))
    (let ((*tick-proposal-evidence-fn* #'tick-proposal-fixture-evidence)
          (*tick-proposal-cognitive-fn* #'accepted)
          (*tick-proposal-search-fn*
            (lambda (query) (declare (ignore query)) (incf search-calls)
              "Untrusted external fixture result."))
          (*tick-proposal-event-fn*
            (lambda (type payload) (declare (ignore type payload)) (incf events)))
          (*tick-commit-similarity-fn*
            (lambda (left right) (declare (ignore left right)) 0.0d0)))
      (dolist (selected '("idle-drift" "consolidate" "anticipate" "ruminate"
                          "curiosity" "explore" "episode-replay" "maintenance"))
        (let ((proposal (tick-build-proposal selected)))
          (tick-proposal-check (format nil "~a returns object" selected)
                               (hash-table-p proposal))
          (tick-proposal-check (format nil "~a proposal validates" selected)
                               (null (tick-commit-validate proposal)))))
      (let ((*importance-since-last-reflection* 9.0d0))
        (tick-proposal-check
         "full reflection branch validates"
         (null (tick-commit-validate (tick-build-proposal "consolidate")))))
      (tick-proposal-check "curiosity performs one external search" (= search-calls 1))
      (tick-proposal-check "curiosity records one external signal event" (= events 1))
      (let ((before cognitive-calls))
        (tick-build-proposal "maintenance")
        (tick-proposal-check "maintenance makes no cognitive call"
                             (= cognitive-calls before)))
      (tick-proposal-check "legacy raw-call-model definition unchanged"
                           (eq raw-before (fdefinition 'raw-call-model)))))

(format t "~%== rumination no-work suppression ==~%")
(let ((calls 0)
      (*tick-proposal-open-questions-fn* (lambda () nil))
      (*tick-proposal-evidence-fn* #'tick-proposal-fixture-evidence)
      (*tick-proposal-cognitive-fn*
        (lambda (&rest args) (declare (ignore args)) (incf calls))))
  (let ((proposal (tick-build-proposal "ruminate")))
    (tick-proposal-check "no active question skips rumination"
                         (string= "skipped" (gethash "status" proposal)))
    (tick-proposal-check "no active question spends no model call" (zerop calls))
    (tick-proposal-check "skip reason is inspectable"
                         (string= "no-active-open-question"
                                  (gethash "reason" proposal)))))

(format t "~%== curiosity provider-error containment ==~%")
(let* ((calls 0) (events 0)
      (*tick-proposal-evidence-fn* #'tick-proposal-fixture-evidence)
      (*tick-proposal-cognitive-fn*
        (lambda (purpose evidence &rest keys)
          (declare (ignore purpose keys))
          (incf calls)
          (obj "status" "accepted" "record"
               (obj "content" "grounded search query"
                    "evidence_node_ids"
                    (coerce (mapcar (lambda (node) (gethash "id" node)) evidence)
                            'vector)))))
      (*tick-proposal-search-fn*
        (lambda (query) (declare (ignore query))
          "ERROR: Web search failed: invalid API key"))
      (*tick-proposal-event-fn*
        (lambda (&rest args) (declare (ignore args)) (incf events))))
  (let ((proposal (tick-build-proposal "curiosity")))
    (tick-proposal-check "provider error is rejected before synthesis"
                         (string= "invalid-search-result"
                                  (gethash "reason" proposal)))
    (tick-proposal-check "provider error performs only query-generation call"
                         (= calls 1))
    (tick-proposal-check "provider error is not logged as external evidence"
                         (zerop events))))

  (format t "~%== explore bounded different-seed retry ==~%")
  (let* ((calls 0) (retry-questions nil)
        (*tick-proposal-evidence-fn* #'tick-proposal-fixture-evidence)
        (*tick-proposal-cognitive-fn*
          (lambda (purpose evidence &rest keys)
            (declare (ignore purpose))
            (incf calls) (push (getf keys :question) retry-questions)
            (if (= calls 1)
                (obj "status" "rejected-duplicate"
                     "duplicate_of_node_id" "prior-worldview")
                (obj "status" "accepted"
                     "record"
                     (obj "speaker" "the agent" "human" "the operator"
                          "purpose" "worldview-exploration"
                          "record_type" "supported-inference"
                          "content" "A materially different grounded implication."
                          "evidence_node_ids"
                          (coerce (mapcar (lambda (node) (gethash "id" node)) evidence)
                                  'vector)
                          "uncertainty" 0.2d0 "novel_contribution" "different"
                          "proposed_next_operation" "none"))))))
    (let ((proposal (tick-build-proposal "explore")))
      (tick-proposal-check "near repeat retries exactly once" (= calls 2))
      (tick-proposal-check "retry uses a different seed"
                           (not (string= (first retry-questions)
                                         (second retry-questions))))
      (tick-proposal-check "different retry becomes valid proposal"
                           (null (tick-commit-validate proposal)))
      (tick-proposal-check
       "accepted retry supersedes the merged near-repeat"
       (string= "prior-worldview"
                (gethash "supersedes_node_id"
                         (aref (gethash "memory_specs" proposal) 0))))))
  (let* ((calls 0)
        (*tick-proposal-evidence-fn* #'tick-proposal-fixture-evidence)
        (*tick-proposal-cognitive-fn*
          (lambda (&rest args) (declare (ignore args)) (incf calls)
            (obj "status" "rejected-duplicate"))))
    (let ((proposal (tick-build-proposal "explore")))
      (tick-proposal-check "second duplicate performs only two calls" (= calls 2))
      (tick-proposal-check "second duplicate writes nothing"
                           (and (string= "skipped" (gethash "status" proposal))
                                (zerop (length (gethash "memory_specs" proposal)))))))
  (let* ((calls 0)
        (*tick-proposal-evidence-fn*
          (lambda (query k kinds) (declare (ignore query k kinds)) nil))
        (*tick-proposal-cognitive-fn*
          (lambda (&rest args) (declare (ignore args)) (incf calls))))
    (tick-proposal-check "missing typed roots skips fail closed"
                         (string= "skipped"
                                  (gethash "status" (tick-build-proposal "ruminate"))))
    (tick-proposal-check "missing roots spend no model call" (zerop calls))))

(format t "~%~a passed, ~a failed~%"
        *tick-proposal-test-pass* *tick-proposal-test-fail*)
(when (plusp *tick-proposal-test-fail*) (sb-ext:exit :code 1))
