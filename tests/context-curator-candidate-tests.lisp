(in-package :agent)

(ql:quickload '(:shasht) :silent t)

(defvar *curator-test-pass* 0)
(defvar *curator-test-fail* 0)

(defun curator-test-check (name condition)
  (if condition
      (progn (incf *curator-test-pass*) (format t "PASS ~a~%" name))
      (progn (incf *curator-test-fail*) (format t "FAIL ~a~%" name))))

(load (test-source "context-curator-candidate.lisp"))

(defparameter *curator-test-rows*
  (list (obj "id" "memory-1" "kind" "observation"
             "origin_class" "lived-user" "epistemic_status" "user-report"
             "grounding_status" "grounded" "content" "the operator likes tea.")
        (obj "id" "memory-2" "kind" "observation"
             "origin_class" "lived-agent-action" "epistemic_status" "observed"
             "grounding_status" "grounded" "content" "the agent discussed tea.")))

(defparameter *curator-test-manifest*
  (context-curator-build-manifest
   "What drink do I like?" *curator-test-rows* :tools '("search-memory")
   :as-of "2026-08-07T16:00:00Z"))

(curator-test-check "manifest uses temporal schema version 2"
                    (= 2 (gethash "schema_version" *curator-test-manifest*)))
(curator-test-check "manifest carries code-owned as-of time"
                    (string= "2026-08-07T16:00:00Z"
                             (gethash "as_of" *curator-test-manifest*)))
(curator-test-check
 "universal as-of is rendered as canonical UTC"
 (string= "2026-08-07T16:00:00Z"
          (gethash
           "as_of"
           (context-curator-build-manifest
            "What drink do I like?" *curator-test-rows*
            :as-of (encode-universal-time 0 0 16 7 8 2026 0)))))

(defun curator-test-response (&key (decision "SELECT")
                                   (ids (vector "memory-1"))
                                   (possible
                                     (vector
                                      (obj "content" "the operator has said he likes tea."
                                           "evidence_ids" (vector "memory-1"))))
                                   (tools (vector)))
  (obj "schema_version" 1 "decision" decision
       "active_task" "Recall the operator's stated preference."
       "response_obligations" (vector "Use cited evidence only.")
       "selected_context_ids" ids
       "possible_context" possible
       "recommended_tools" tools
       "continuity_risks" (vector "Avoid guessing.")
       "uncertainty" (obj "level" "low" "note" "Direct report.")))

(let* ((validated (context-curator-validate-response
                   (curator-test-response) *curator-test-manifest*))
       (compiled (context-curator-compile-block validated
                                                *curator-test-manifest*)))
  (curator-test-check "valid selection preserves stable evidence ID"
                      (string= "memory-1"
                               (aref (gethash "selected_context_ids" validated)
                                     0)))
  (curator-test-check "compiler includes exact evidence and derived boundary"
                      (and (search "the operator likes tea." compiled)
                           (search "[id memory-1]" compiled)
                           (search "Derived orientation (not external fact)"
                                   compiled))))

(let ((validated
        (context-curator-validate-response
         (curator-test-response :decision "NO_EXTRA_CONTEXT" :ids (vector)
                                :possible (vector))
         *curator-test-manifest*)))
  (curator-test-check "explicit abstention accepts zero selected evidence"
                      (string= "NO_EXTRA_CONTEXT"
                               (gethash "decision" validated))))

(curator-test-check
 "unknown context ID rejects the whole response"
 (handler-case
     (progn (context-curator-validate-response
             (curator-test-response :ids (vector "invented"))
             *curator-test-manifest*)
            nil)
   (error () t)))

(curator-test-check
 "unknown tool rejects the whole response"
 (handler-case
     (progn (context-curator-validate-response
             (curator-test-response
              :tools (vector (obj "name" "delete-file" "reason" "No.")))
             *curator-test-manifest*)
            nil)
   (error () t)))

(curator-test-check
 "drafted public reply key fails closed"
 (let ((response (curator-test-response)))
   (setf (gethash "public_reply" response) "Here is the answer")
   (handler-case
       (progn (context-curator-validate-response response
                                                *curator-test-manifest*)
              nil)
     (error () t))))

(curator-test-check
 "missing required key fails closed"
 (let ((response (curator-test-response)))
   (remhash "continuity_risks" response)
   (handler-case
       (progn (context-curator-validate-response response
                                                *curator-test-manifest*)
              nil)
     (error () t))))

(curator-test-check
 "decision and selection mismatch fails closed"
 (handler-case
     (progn (context-curator-validate-response
             (curator-test-response :decision "NO_EXTRA_CONTEXT")
             *curator-test-manifest*)
            nil)
   (error () t)))

(curator-test-check
 "over-budget obligations fail closed"
 (let ((response (curator-test-response)))
   (setf (gethash "response_obligations" response)
         (vector "1" "2" "3" "4" "5" "6"))
   (handler-case
       (progn (context-curator-validate-response response
                                                *curator-test-manifest*)
              nil)
     (error () t))))

(curator-test-check
 "possible context may cite only selected manifest IDs"
 (handler-case
     (progn
       (context-curator-validate-response
        (curator-test-response
         :possible (vector (obj "content" "Unsupported."
                                "evidence_ids" (vector "memory-2"))))
        *curator-test-manifest*)
       nil)
   (error () t)))

(curator-test-check
 "request is exactly one private system and one manifest message"
 (let ((request (context-curator-build-request *curator-test-manifest*)))
   (and (= 2 (length request))
        (string= "system" (gethash "role" (aref request 0)))
        (string= "user" (gethash "role" (aref request 1)))
        (search "not the public respondent"
                (gethash "content" (aref request 0)))
        (search "copied exactly from candidate_context[].id"
                (gethash "content" (aref request 0)))
        (search "topical similarity alone is insufficient"
                (gethash "content" (aref request 0)))
        (search "do not promote an earlier one-off statement to current"
                (gethash "content" (aref request 0)))
        (search "uncertainty must not be low"
                (gethash "content" (aref request 0)))
        (search "ordinary greeting, acknowledgement, emoji, or small talk"
                (gethash "content" (aref request 0))))))

(let* ((bundle
         (obj "id" "turn-bundle:turn-9" "kind" "turn-bundle"
              "origin_class" "derived-lived"
              "epistemic_status" "grounded-turn-bundle"
              "grounding_status" "grounded"
              "label" "Grounded conversation exchange"
              "content" "the operator: Which drink?~%the agent: Tea."
              "observed_at" "2026-08-05T09:00:00Z"
              "valid_from" "2026-08-05T09:00:00Z"
              "valid_to" :null "supersedes_node_id" :null
              "turn_id" "turn-9" "member_count" 2
              "member_roles" (vector "user" "assistant")
              "evidence_node_ids" (vector "turn-9-user" "turn-9-assistant")
              "anchor_id" "turn-9-user"))
       (manifest (context-curator-build-manifest
                  "Which drink?" (list bundle)
                  :as-of "2026-08-07T16:00:00Z"))
       (candidate (aref (gethash "candidate_context" manifest) 0)))
  (curator-test-check
   "bundle manifest preserves structural provenance and exact member evidence"
   (and (string= "turn-9" (gethash "turn_id" candidate))
        (= 2 (gethash "member_count" candidate))
        (equal '("user" "assistant")
               (coerce (gethash "member_roles" candidate) 'list))
        (equal '("turn-9-user" "turn-9-assistant")
               (coerce (gethash "evidence_node_ids" candidate) 'list))
        (string= "Grounded conversation exchange"
                 (gethash "label" candidate))
        (string= "2026-08-05T09:00:00Z"
                 (gethash "observed_at" candidate))
        (string= "2026-08-05T09:00:00Z"
                 (gethash "valid_from" candidate))
        (eq :null (gethash "valid_to" candidate)))))

;; Sanitized current-agenda temporal contract. This is captured-output
;; qualification: the candidate never invents a model or semantic classifier.
(let* ((stale
         (obj "id" "agenda-old" "kind" "turn-bundle"
              "origin_class" "derived-lived"
              "epistemic_status" "grounded-turn-bundle"
              "grounding_status" "grounded"
              "content" "the operator: Today's one-off agenda is task Alpha."
              "observed_at" "2026-08-04T09:00:00Z"
              "valid_from" "2026-08-04T00:00:00Z"
              "valid_to" "2026-08-05T00:00:00Z"
              "supersedes_node_id" :null))
       (current
         (obj "id" "agenda-current" "kind" "turn-bundle"
              "origin_class" "derived-lived"
              "epistemic_status" "grounded-turn-bundle"
              "grounding_status" "grounded"
              "content" "the operator: Today's agenda includes task Beta."
              "observed_at" "2026-08-07T08:00:00Z"
              "valid_from" "2026-08-07T00:00:00Z"
              "valid_to" "2026-08-08T00:00:00Z"
              "supersedes_node_id" "agenda-old"))
       (recurring
         (obj "id" "agenda-recurring" "kind" "turn-bundle"
              "origin_class" "derived-lived"
              "epistemic_status" "grounded-turn-bundle"
              "grounding_status" "grounded"
              "content" "the operator: Every Friday I review task Gamma."
              "observed_at" "2026-07-31T08:00:00Z"
              "valid_from" "2026-07-31T00:00:00Z"
              "valid_to" :null "supersedes_node_id" :null))
       (manifest
         (context-curator-build-manifest
          "What is on my agenda?" (list stale current recurring)
          :as-of "2026-08-07T16:00:00Z"))
       (response
         (obj "schema_version" 1 "decision" "SELECT"
              "active_task" "Reconstruct the operator's likely current agenda."
              "response_obligations"
              (vector "Use current evidence" "Mark recurrence explicitly")
              "selected_context_ids"
              (vector "agenda-current" "agenda-recurring")
              "possible_context"
              (vector
               (obj "content"
                    "the operator said task Beta is on today's agenda; Friday review of task Gamma is recurring."
                    "evidence_ids"
                    (vector "agenda-current" "agenda-recurring")))
              "recommended_tools" (vector)
              "continuity_risks" (vector "The agenda may have changed.")
              "uncertainty"
              (obj "level" "medium"
                   "note" "Current and recurring evidence is grounded, but no completion state is available.")))
       (validated (context-curator-validate-response response manifest))
       (candidates (coerce (gethash "candidate_context" manifest) 'list)))
  (curator-test-check
   "agenda manifest carries temporal provenance for every candidate"
   (every (lambda (row)
            (and (nth-value 1 (gethash "observed_at" row))
                 (nth-value 1 (gethash "valid_from" row))
                 (nth-value 1 (gethash "valid_to" row))
                 (nth-value 1 (gethash "supersedes_node_id" row))))
          candidates))
  (curator-test-check
   "captured agenda synthesis excludes expired one-off evidence"
   (not (find "agenda-old"
              (coerce (gethash "selected_context_ids" validated) 'list)
              :test #'string=)))
  (curator-test-check
   "captured agenda synthesis preserves current and recurring evidence"
   (equal '("agenda-current" "agenda-recurring")
          (coerce (gethash "selected_context_ids" validated) 'list)))
  (curator-test-check
   "captured agenda synthesis does not overstate temporal certainty"
   (string= "medium"
            (gethash "level" (gethash "uncertainty" validated)))))

(format t "~%CONTEXT-CURATOR TESTS: ~a passed, ~a failed.~%"
        *curator-test-pass* *curator-test-fail*)
(when (plusp *curator-test-fail*) (sb-ext:exit :code 1))
