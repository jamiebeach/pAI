;;;; explore-novelty.lisp -- novelty gate for repeated explore passes, 2026-07-29.
;;;; Loaded after conversational-initiative.lisp.  A continuation may consume
;;;; attention without earning another worldview/self-model write.

(in-package :agent)

(defparameter *explore-novelty-similarity-threshold* 0.90d0)
(defparameter *explore-reorientation-max-per-pass* 1)
(defvar *explore-last-stance* nil
  "Last accepted stance for the active explore topic; persisted with the topic state.")
(defvar *explore-reorientations-used* 0
  "Dynamically scoped by one explore pass; prevents novelty rejection from
turning a single tick into an unbounded novelty chase.")

(defun %explore-cosine-similarity (a b)
  (let ((dot 0.0d0) (na 0.0d0) (nb 0.0d0))
    (loop for x in a for y in b do (incf dot (* x y)) (incf na (* x x)) (incf nb (* y y)))
    (if (or (zerop na) (zerop nb)) 0.0d0 (/ dot (* (sqrt na) (sqrt nb))))))

(defun %explore-materially-novel-p (stance continuing-p)
  "Fail closed for a continuation if semantic comparison cannot be made.
A fresh topic has no prior stance and is always eligible for its first write."
  (if (or (not continuing-p) (null *explore-last-stance*))
      (values t :first-take :null)
      (handler-case
          (let ((similarity (%explore-cosine-similarity (embed-text stance)
                                                        (embed-text *explore-last-stance*))))
            (values (< similarity *explore-novelty-similarity-threshold*)
                    (if (< similarity *explore-novelty-similarity-threshold*) :material-advance :near-duplicate)
                    similarity))
        (error (e)
          (format t "~&[explore] novelty comparison failed; deferring continuation: ~a~%" e)
          (values nil :comparison-unavailable :null)))))

(defun %explore-defer-near-duplicate (question stance reason similarity)
  (when (fboundp 'latent-incubate)
    (ignore-errors
      (funcall 'latent-incubate (format nil "On ~a: ~a" question stance)
               :origin "explore-continuation" :topic (%latent-topic question))))
  (when (fboundp 'log-event)
    (ignore-errors
      (funcall 'log-event "explore-novelty-deferred"
               (obj "topic" question "reason" (string-downcase (string reason))
                    "similarity" (or similarity :null)))))
  (continuity-buffer-append
   "Revisited an open question, but the new take was too close to what I had already said; kept it incubating rather than mistaking repetition for development."))

(defun %explore-saturate-current-topic ()
  "End the current continuation chain after a near-repeat.  The next pick is
forced through the ambient-recall diversity path rather than the same topic."
  (setf *explore-current-topic* nil *explore-topic-started-at* 0
        *explore-continuation-count* 0 *explore-last-stance* nil)
  (ignore-errors (save-explore-state)))

;; Replacement body rather than a wrapper: the write must be gated before
;; MEMORY-WRITE-NODE/SELF-MODEL-PROPOSE-REVISION run.
(defun %tick-handle-explore ()
  (handler-case
      (multiple-value-bind (question continuing-p) (%explore-pick-question)
        (cond
          ((not question)
           (continuity-buffer-append "Tried to develop a real point of view on something, but there wasn't enough to go on yet."))
          (t
           (let* ((evidence (memory-recall question :k 5 :debug t))
                  (evidence-text (if evidence
                                     (format nil "~{- ~a~%~}" (mapcar (lambda (e) (gethash "content" e)) evidence))
                                     "(no specific memories surfaced)"))
                  (resp (raw-call-model
                         (list (obj "role" "system" "content"
                                    "Given this question and whatever evidence is below, write a genuine, considered first-person take. A continuation must add a distinct claim, evidence, or implication; do not merely restate the prior position. Up to about 80 words.")
                               (obj "role" "user" "content"
                                     (format nil "Question: ~a~%~%Evidence:~%~a" question evidence-text)))))
                  (stance (gethash "content" (ref resp "choices" 0 "message"))))
             (when (and (stringp stance) (plusp (length stance)))
               (multiple-value-bind (novel-p reason similarity)
                   (%explore-materially-novel-p stance continuing-p)
                 (if novel-p
                     (let ((node-id (memory-write-node :kind "worldview" :content (format nil "Q: ~a~%~a" question stance))))
                       (dolist (e evidence)
                         (ignore-errors (memory-add-edge node-id (gethash "id" e) "evidence-for")))
                       (setf *explore-last-stance* stance)
                       (ignore-errors (save-explore-state))
                       (multiple-value-bind (entry rejection)
                           (self-model-propose-revision "open-questions"
                                                         (format nil "~a -- ~a" question (%explore-truncate stance 60))
                                                         (list node-id))
                         (declare (ignore entry))
                         (when rejection (format t "~&[explore] self-model revision rejected: ~a~%" rejection)))
                       (continuity-buffer-append (format nil "Spent some real time thinking about: ~a" question))
                       (when (fboundp 'initiative-v2-observe-trigger)
                         (ignore-errors
                           (let* ((node (and (fboundp 'memory-get-node)
                                             (ignore-errors (memory-get-node node-id))))
                                  (grounded-evidence
                                    (append evidence (and node (list node))))
                                  (decision
                                    (funcall 'initiative-v2-observe-trigger
                                             stance grounded-evidence
                                             :trigger-type "explore-development"
                                             :trigger-event-ids (list node-id)
                                             :topic question)))
                             (when (and decision
                                        (fboundp 'reciprocity-canary-consider-observation))
                               (funcall 'reciprocity-canary-consider-observation
                                        "explore-development" stance
                                        grounded-evidence decision
                                        :source-id node-id
                                        :artifact-class "internal-stance"
                                        :generation-contract
                                        "explore-stance-v1")))))
                       (when (fboundp '%drives-event-initiate)
                         (ignore-errors
                           (funcall '%drives-event-initiate
                                    (format nil "Explore development [worldview ~a]. Question: ~a~%Stance: ~a"
                                            node-id question (%explore-truncate stance 80))))) )
                     (progn
                       (%explore-defer-near-duplicate question stance reason similarity)
                       (%explore-saturate-current-topic)
                       (when (< *explore-reorientations-used* *explore-reorientation-max-per-pass*)
                         (continuity-buffer-append "That thread had stopped moving, so I set it aside and looked for a different question to develop.")
                         (let ((*explore-reorientations-used* (1+ *explore-reorientations-used*)))
                           (%tick-handle-explore)))))))))))
    (error (e)
      (format t "~&[tick-loop] explore failed: ~a~%" e)
      (continuity-buffer-append "Tried to develop a real point of view on something, but it didn't come together."))))

;; Extend the existing explore-state persistence without changing its file
;; identity or breaking state written before this field existed.
;;; (removed: dead definition -- superseded downstream)
;;; (removed: dead definition -- superseded downstream)
(setf (gethash "explore" *tick-handlers*) #'%tick-handle-explore)
(define-init :restore explore-novelty-restore
    "Restore durable state for explore-novelty."
  (load-explore-state))
