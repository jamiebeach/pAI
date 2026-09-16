(in-package :agent)

(ql:quickload :bordeaux-threads :silent t)

(defvar *cognitive-test-pass* 0)
(defvar *cognitive-test-fail* 0)
(defvar *cognitive-test-events* nil)
(defvar *cognitive-test-model-calls* nil)
(defvar *cognitive-test-memory-writes* 0)

(defun cognitive-test-check (name condition)
  (if condition
      (progn (incf *cognitive-test-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *cognitive-test-fail*) (format t "  FAIL ~a~%" name))))

(defun cognitive-test-root (&optional (id "root-user"))
  (obj "id" id "kind" "observation" "content" "typed lived fixture"
       "origin_class" "lived-user" "epistemic_status" "user-report"
       "grounding_status" "grounded" "producer" "fixture"
       "root_observation_ids" (vector) "quarantined" nil))

(defun cognitive-test-record (purpose &key (speaker "the agent") (human "the operator")
                                      (ids '("root-user"))
                                      (content "A grounded new connection appears.")
                                      (record-type "supported-inference")
                                      (novel "Connects two details without extending the evidence."))
  (obj "speaker" speaker "human" human "purpose" purpose
       "record_type" record-type "content" content
       "evidence_node_ids" (coerce ids 'vector) "uncertainty" 0.2d0
       "novel_contribution" novel "proposed_next_operation" "none"))

(defun cognitive-test-response (record)
  (obj "choices" (vector (obj "message" (obj "content"
                                                (shasht:write-json record nil))))
       "usage" (obj "prompt_tokens" 10 "completion_tokens" 5
                    "total_tokens" 15 "cost" 0.001d0)))

(setf (fdefinition 'log-event)
      (lambda (type payload &key caused-by)
        (push (list type payload caused-by) *cognitive-test-events*)
        (length *cognitive-test-events*)))
(setf (fdefinition 'memory-write-node)
      (lambda (&rest args) (declare (ignore args))
        (incf *cognitive-test-memory-writes*) "forbidden-write"))

;; CALL-MODEL is a seam (P0c item 3): the :INSTALL phase run below (for
;; "nested timing") registers observability-tracing's timing layer on it,
;; which requires the seam to already exist -- the harness preload's
;; self-mod.lisp still installs it as a plain DEFUN (untouched; see
;; wrap-chain-registry.lisp's call-model note), so re-declare it here as a
;; seam before anything captures or depends on its identity.
(define-seam call-model (messages) (declare (ignore messages)) :unused-stub)

(defparameter *cognitive-test-call-model-before*
  (and (fboundp 'call-model) (fdefinition 'call-model)))
(load (test-source "cognitive-call.lisp"))
(cognitive-test-check "conversational call-model is not replaced"
                      (eq *cognitive-test-call-model-before*
                          (fdefinition 'call-model)))

(setf *cognitive-call-recent-records-fn* (lambda (topic purpose)
                                            (declare (ignore topic purpose)) nil)
      *cognitive-call-similarity-fn* (lambda (left right)
                                       (if (string= left right) 1.0d0 0.0d0)))

(format t "~%== every declared purpose ==~%")
(dolist (purpose *cognitive-call-purposes*)
  (let ((*cognitive-call-model-fn*
          (lambda (messages model temperature)
            (declare (ignore messages model temperature))
            (cognitive-test-response (cognitive-test-record purpose)))))
    (let ((result (cognitive-call purpose (list (cognitive-test-root))
                                  :topic "fixture" :generation-id
                                  (format nil "g-~a" purpose))))
      (cognitive-test-check (format nil "~a accepts valid structure" purpose)
                            (string= "accepted" (gethash "status" result)))
      (cognitive-test-check (format nil "~a is never directly deliverable" purpose)
                            (null (gethash "delivery_eligible" result))))))
(cognitive-test-check "accepted calls never write memory"
                      (zerop *cognitive-test-memory-writes*))

(format t "~%== prompt isolation ==~%")
(let* ((captured nil)
      (*cognitive-call-model-fn*
        (lambda (messages model temperature)
          (declare (ignore model temperature))
          (setf captured messages)
          (cognitive-test-response (cognitive-test-record "deep-reflection")))))
  (cognitive-call "deep-reflection" (list (cognitive-test-root))
                  :question "What follows?" :topic "fixture")
  (let ((serialized (shasht:write-json (coerce captured 'vector) nil)))
    (cognitive-test-check "cognitive prompt has only system and user roles"
                          (equal '("system" "user")
                                 (mapcar (lambda (message) (gethash "role" message))
                                         captured)))
    (cognitive-test-check "production dynamic sections are absent"
                          (not (some (lambda (marker) (search marker serialized))
                                     '("CONTINUITY:BEGIN" "WANTS:BEGIN"
                                       "SHARED-MEMORY:BEGIN" "LATENT:BEGIN"))))
    (cognitive-test-check "tool catalogue is absent"
                          (and (not (search "lisp-eval" serialized))
                               (not (search "tool_calls" serialized))))))

(format t "~%== strict repair boundary ==~%")
(let* ((purpose "deep-reflection")
       (calls 0)
       (captured nil)
       (*cognitive-call-model-fn*
         (lambda (messages model temperature)
           (declare (ignore model temperature))
           (incf calls) (push messages captured)
           (if (= calls 1)
               "not json"
               (cognitive-test-response (cognitive-test-record purpose))))))
  (let ((result (cognitive-call purpose (list (cognitive-test-root))
                                :topic "fixture")))
    (cognitive-test-check "malformed output receives exactly one repair"
                          (and (string= "accepted" (gethash "status" result))
                               (= calls 2) (= 1 (gethash "retries" result))))
    (cognitive-test-check "repair receives malformed output but no evidence content"
                          (let ((serialized (shasht:write-json (first captured) nil)))
                            (and (search "not json" serialized)
                                 (not (search "typed lived fixture" serialized)))))))
(let* ((calls 0)
      (*cognitive-call-model-fn*
        (lambda (messages model temperature)
          (declare (ignore messages model temperature))
          (incf calls) "still-not-json")))
  (let ((result (cognitive-call "rumination" (list (cognitive-test-root)))))
    (cognitive-test-check "malformed repair fails closed"
                          (string= "rejected-schema" (gethash "status" result)))
    (cognitive-test-check "malformed repair is bounded to two calls" (= calls 2))))

(format t "~%== identity, evidence, grounding, and novelty rejection ==~%")
(flet ((run-record (record &key evidence recent errorp)
         (let ((*cognitive-call-model-fn*
                 (lambda (messages model temperature)
                   (declare (ignore messages model temperature))
                   (if errorp (error "forced timeout")
                       (cognitive-test-response record))))
               (*cognitive-call-recent-records-fn*
                 (lambda (topic purpose) (declare (ignore topic purpose)) recent)))
           (cognitive-call "deep-reflection"
                           (or evidence (list (cognitive-test-root)))
                           :topic "fixture"))))
  (cognitive-test-check
   "swapped speaker fails closed"
   (string= "rejected-identity"
            (gethash "status"
                     (run-record (cognitive-test-record "deep-reflection"
                                                        :speaker "the operator" :human "the agent")))))
  (cognitive-test-check
   "generic assistant identity phrase fails closed"
   (string= "rejected-identity"
            (gethash "status"
                     (run-record
                      (cognitive-test-record
                       "deep-reflection"
                       :content "As an AI language model, I cannot know.")))))
  (cognitive-test-check
   "known overnight identity-confusion phrase fails closed"
   (string= "rejected-identity"
            (gethash "status"
                     (run-record
                      (cognitive-test-record
                       "deep-reflection" :content "Is the operator a character here?")))))
  (cognitive-test-check
   "unsupported elapsed subjective experience fails closed"
   (string= "rejected-grounding"
            (gethash "status"
                     (run-record
                      (cognitive-test-record
                       "deep-reflection"
                       :content "I had continuous subjective experience while you were away.")))))
  (cognitive-test-check
   "invented evidence id fails closed"
   (string= "rejected-grounding"
            (gethash "status"
                     (run-record (cognitive-test-record "deep-reflection"
                                                        :ids '("invented"))))))
  (let ((legacy (cognitive-test-root)))
    (setf (gethash "origin_class" legacy) "legacy-unclassified")
    (cognitive-test-check
     "legacy evidence fails closed"
     (string= "rejected-grounding"
              (gethash "status"
                       (run-record (cognitive-test-record "deep-reflection")
                                   :evidence (list legacy))))))
  (let ((synthetic (cognitive-test-root)))
    (setf (gethash "origin_class" synthetic) "synthetic"
          (gethash "epistemic_status" synthetic) "supported-inference"
          (gethash "root_observation_ids" synthetic) (vector))
    (cognitive-test-check
     "generated-only evidence without lived roots fails closed"
     (string= "rejected-grounding"
              (gethash "status"
                       (run-record (cognitive-test-record "deep-reflection")
                                   :evidence (list synthetic))))))
  (let* ((prior (obj "id" "prior-node"
                     "content" "A grounded new connection appears."))
         (result (run-record (cognitive-test-record "deep-reflection")
                             :recent (list prior))))
    (cognitive-test-check "near duplicate fails closed"
                          (string= "rejected-duplicate"
                                   (gethash "status" result)))
    (cognitive-test-check "duplicate returns content-free merge lineage id"
                          (string= "prior-node"
                                   (gethash "duplicate_of_node_id" result))))
  (cognitive-test-check
   "model error fails closed"
   (string= "model-error"
            (gethash "status"
                     (run-record (cognitive-test-record "deep-reflection")
                                 :errorp t))))
  (let ((*cognitive-call-model-fn*
          (lambda (messages model temperature)
            (declare (ignore messages model temperature))
            (obj "choices" (vector (obj "message" (obj "content" :null)))))))
    (cognitive-test-check
     "empty response is explicit"
     (string= "empty"
              (gethash "status"
                       (cognitive-call "deep-reflection"
                                       (list (cognitive-test-root)))))))
  (cognitive-test-check
   "wrong declared purpose fails identity contract"
   (string= "rejected-identity"
            (gethash "status"
                     (run-record (cognitive-test-record "rumination")))))
  (cognitive-test-check
   "word bound fails schema"
   (string= "rejected-schema"
            (gethash "status"
                     (let ((*cognitive-call-model-fn*
                             (lambda (messages model temperature)
                               (declare (ignore messages model temperature))
                               (cognitive-test-response
                                (cognitive-test-record
                                 "deep-reflection"
                                 :content "one two three four five six")))))
                       (cognitive-call "deep-reflection"
                                       (list (cognitive-test-root))
                                       :max-words 5))))))
  (let ((extra (cognitive-test-record "deep-reflection")))
    (setf (gethash "extra" extra) "not allowed")
    (let ((*cognitive-call-model-fn*
            (lambda (messages model temperature)
              (declare (ignore messages model temperature))
              (cognitive-test-response extra))))
      (cognitive-test-check
       "extra schema fields fail closed"
       (string= "rejected-schema"
                (gethash "status"
                         (cognitive-call "deep-reflection"
                                         (list (cognitive-test-root))))))))

(format t "~%== audit privacy and accounting ==~%")
(let* ((end-events (remove-if-not (lambda (entry)
                                    (string= "cognitive-call-end" (first entry)))
                                  *cognitive-test-events*))
       (metadata-events
         (remove-if-not
          (lambda (entry)
            (member (first entry) '("cognitive-call-start" "cognitive-call-end")
                    :test #'string=))
          *cognitive-test-events*))
       (serialized (shasht:write-json (coerce metadata-events 'vector) nil)))
  (cognitive-test-check "start and terminal events are paired"
                        (= (length end-events)
                           (count "cognitive-call-start" *cognitive-test-events*
                                  :key #'first :test #'string=)))
  (cognitive-test-check "metadata audit events contain no evidence content"
                        (not (search "typed lived fixture" serialized)))
  (cognitive-test-check "metadata audit events contain no generated content"
                        (not (search "grounded new connection" serialized)))
  (cognitive-test-check "usage is retained on accepted terminal event"
                        (some (lambda (entry)
                                (= 15 (gethash "total_tokens" (second entry) -1)))
                              end-events)))

(format t "~%== nested timing ==~%")
(load (test-source "observability-tracing.lisp"))
;; Loading defines; INITIALIZE installs. observability-tracing used to install
;; its thirteen timing wrappers as a top-level side effect of being loaded,
;; so this suite got them for free. They are now an :install action, which
;; means the spans below do not exist until the phase runs.
;;
;; :install is pure in-memory -- no filesystem, no database, no network -- so
;; running it here keeps the suite offline.
(initialize :phases '(:install) :stop-on-error nil :verbose nil)
(let* ((traces nil)
      (*timing-event-sink* (lambda (payload) (push payload traces)))
      (*cognitive-call-model-fn*
        (lambda (messages model temperature)
          (declare (ignore messages model temperature))
          (cognitive-test-response (cognitive-test-record "deep-reflection")))))
  (call-with-timing-trace
   (lambda () (cognitive-call "deep-reflection" (list (cognitive-test-root))))
   :trace-id "cognitive-timing" :origin "test"
   :root-span "test.total" :sampled-p t)
  (let ((spans (coerce (gethash "spans" (first traces)) 'list)))
    (cognitive-test-check "cognitive total timing span exists"
                          (find "cognitive.total" spans :test #'string=
                                :key (lambda (span) (gethash "name" span))))
    (cognitive-test-check "cognitive model timing span exists"
                          (find "model.cognitive_request" spans :test #'string=
                                :key (lambda (span) (gethash "name" span))))))

(format t "~%~a passed, ~a failed~%" *cognitive-test-pass* *cognitive-test-fail*)
(when (plusp *cognitive-test-fail*) (sb-ext:exit :code 1))
