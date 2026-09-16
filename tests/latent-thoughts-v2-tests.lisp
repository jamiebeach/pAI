(in-package :agent)

(ql:quickload :bordeaux-threads :silent t)

(defvar *latent-v2-test-pass* 0)
(defvar *latent-v2-test-fail* 0)
(defvar *latent-v2-test-events* nil)
(defvar *latent-v2-test-send-calls* 0)

(defun latent-v2-test-check (name condition)
  (if condition
      (progn (incf *latent-v2-test-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *latent-v2-test-fail*) (format t "  FAIL ~a~%" name))))

(setf (fdefinition 'log-event)
      (lambda (type payload &key caused-by)
        (declare (ignore caused-by))
        (push (list type payload) *latent-v2-test-events*)))
(setf (fdefinition 'telegram-send)
      (lambda (&rest arguments) (declare (ignore arguments))
        (incf *latent-v2-test-send-calls*)))

(load (test-source "latent-thoughts-v2.lisp"))
(setf *latent-v2-file* #P"/tmp/latent-v2-test.json"
      *latent-v2-thoughts* nil)

(defun latent-v2-test-seed (topic &optional (now 1000))
  (latent-v2-seed (format nil "A materially distinct private thought about ~a." topic)
                  :topic topic :evidence-ids (list (format nil "evidence-~a" topic))
                  :now now :expires-at (+ now 10000)))

(format t "~%== all operations and transition audit ==~%")
(dolist (case '(("connect-evidence" :evidence-ids ("new-evidence"))
                ("differentiate" :content "A materially differentiated private thought.")
                ("research" :evidence-ids ("research-result"))
                ("form-question" :content "What concrete question follows from this thought?")
                ("wait-for-cue" :next-reconsideration 2000)
                ("draft" :content "A complete prior-thought draft for later projection.")
                ("merge" :content "Merged terminal material.")
                ("discard" :content "Discarded terminal material.")))
  (let* ((operation (first case))
         (thought (latent-v2-test-seed operation))
         (arguments (rest case)))
    (multiple-value-bind (changed reason)
        (apply #'latent-v2-transition (gethash "id" thought) operation arguments)
      (latent-v2-test-check (format nil "~a transition succeeds" operation)
                            (and changed (null reason)
                                 (= 1 (length (gethash "transitions" changed)))))
      (let ((audit (aref (gethash "transitions" changed) 0)))
        (latent-v2-test-check (format nil "~a stores hashes/actor/schedule" operation)
                              (and (gethash "before_hash" audit)
                                   (gethash "after_hash" audit)
                                   (gethash "actor" audit)
                                   (gethash "next_reconsideration" audit)))))))

(format t "~%== no-op, merge selection, depth and expiry ==~%")
(let ((thought (latent-v2-test-seed "no-op")))
  (multiple-value-bind (changed reason)
      (latent-v2-transition (gethash "id" thought) "differentiate")
    (latent-v2-test-check "no-op rejected explicitly"
                          (and (null changed) (string= reason "no-state-change")))))

(let* ((first (latent-v2-test-seed "duplicate-topic" 3000))
       (other (latent-v2-test-seed "eligible-other" 3000)))
  (declare (ignore other))
  (multiple-value-bind (merged result)
      (latent-v2-seed "A repeat phrasing for the same topic that must merge."
                      :topic "duplicate-topic" :evidence-ids '("evidence-repeat")
                      :now 3001 :expires-at 9000)
    (latent-v2-test-check "near repeat merges into existing thought"
                          (and (eq merged first) (string= result "merged"))))
  (let ((selected (latent-v2-process-pass :now 3002)))
    (latent-v2-test-check "same pass can select a different eligible seed"
                          (and selected
                               (not (string= (gethash "topic" selected)
                                             "duplicate-topic"))))))

(let ((thought (latent-v2-test-seed "depth-cap" 4000)))
  (dotimes (index *latent-v2-max-depth-without-evidence*)
    (latent-v2-transition (gethash "id" thought) "differentiate"
                          :content (format nil "Depth evolution number ~a with changed wording." index)))
  (multiple-value-bind (blocked reason)
      (latent-v2-transition (gethash "id" thought) "differentiate"
                            :content "One unsupported evolution too many.")
    (latent-v2-test-check "depth cap blocks evolution without new evidence"
                          (and blocked (string= reason "depth-cap")
                               (string= (gethash "state" blocked) "blocked")))))

(let ((thought (latent-v2-seed "This private thought expires promptly."
                               :topic "expiry" :evidence-ids '("expiry-evidence")
                               :now 5000 :expires-at 5001)))
  (latent-v2-process-pass :now 5002)
  (latent-v2-test-check "expired thought reaches explicit expired state"
                        (string= (gethash "state" thought) "expired")))

(format t "~%== cue projection, no-send invariant, restart ==~%")
(let ((thought (latent-v2-seed "the operator asked about the release readiness evidence."
                               :topic "release-readiness" :evidence-ids '("lived-1")
                               :now 6000 :expires-at 16000)))
  (latent-v2-transition (gethash "id" thought) "draft"
                        :content "the operator asked about release readiness evidence and I have a prior private thought."
                        :now 6001)
  (let ((relevant (latent-v2-for-prompt
                   (list (obj "content" "the operator asked for release readiness evidence."
                              "origin_class" "lived-user"))))
        (unrelated (latent-v2-for-prompt
                    (list (obj "content" "The weather changed today."
                               "origin_class" "lived-user"))))
        (synthetic (latent-v2-for-prompt
                    (list (obj "content" "release readiness evidence"
                               "origin_class" "synthetic")))))
    (latent-v2-test-check "ready thought projects only for relevant lived cue"
                          (and (= 1 (length relevant))
                               (string= "prior private thought"
                                        (gethash "label" (first relevant)))
                               (null unrelated) (null synthetic)))
    (latent-v2-test-check "latent v2 has no direct-send side effect"
                          (zerop *latent-v2-test-send-calls*))))

(let ((thought (latent-v2-seed "A confirmed expression lifecycle fixture."
                               :topic "expression-lifecycle" :evidence-ids '("lived-2")
                               :now 7000 :expires-at 17000)))
  (latent-v2-transition (gethash "id" thought) "draft"
                        :content "A confirmed expression lifecycle draft." :now 7001)
  (multiple-value-bind (expressed reason)
      (latent-v2-mark-expressed (gethash "id" thought) "turn-1" :now 7002)
    (latent-v2-test-check "confirmed downstream turn reaches expressed state"
                          (and expressed (null reason)
                               (string= (gethash "state" expressed) "expressed")))))

(let ((before (length *latent-v2-thoughts*)))
  (latent-v2-save)
  (setf *latent-v2-thoughts* nil)
  (latent-v2-load)
  (latent-v2-test-check "restart restores durable state machine"
                        (= before (length *latent-v2-thoughts*))))

(ignore-errors (delete-file *latent-v2-file*))
(format t "~%~a passed, ~a failed~%" *latent-v2-test-pass* *latent-v2-test-fail*)
(when (plusp *latent-v2-test-fail*) (sb-ext:exit :code 1))
