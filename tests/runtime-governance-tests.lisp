(in-package :agent)

(defvar *runtime-governance-pass* 0)
(defvar *runtime-governance-fail* 0)
(defun governance-check (name condition)
  (if condition
      (progn (incf *runtime-governance-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *runtime-governance-fail*) (format t "  FAIL ~a~%" name))))

(load (test-source "runtime-observer-registry.lisp"))
(runtime-observer-reset)
(let ((seen 0))
  (runtime-observer-register "fixture" "working" (lambda (x) (declare (ignore x)) (incf seen)))
  (runtime-observer-register "fixture" "broken" (lambda (x) (declare (ignore x)) (error "fixture")))
  (let ((results (runtime-observer-emit "fixture" (obj "value" 1))))
    (governance-check "observer errors are isolated" (= 2 (length results)))
    (governance-check "healthy observer still runs" (= seen 1))
    (governance-check "registry assertion passes" (runtime-observer-assert))))

(runtime-observer-reset)
(load (test-source "runtime-observer-audit.lisp"))
;; Observer registration is a DEFINE-INIT :install action now, not a
;; load-time side effect. Under the original entrypoint the observers were
;; registered by the time any suite ran, so this was never stated; the
;; assertion below checks they are registered and fails closed without it.
(initialize :phases (list :install) :stop-on-error nil :verbose nil)

(governance-check "A2 lifecycle consumers pass required boot assertion"
                  (runtime-observer-audit-assert))

(defvar *initiative-policy-current-id* nil)
(defvar *initiative-candidates* nil)
(defvar *governance-transport-count* 0)
(defvar *governance-web-count* 0)
(defvar *governance-web-correlation-keys* nil)
(setf (fdefinition 'telegram-send)
      (lambda (chat text) (declare (ignore chat text))
        (incf *governance-transport-count*) "ok"))
(setf (fdefinition '%v2-broadcast)
      (lambda (type data &rest correlation-keys)
        (declare (ignore type data))
        (setf *governance-web-correlation-keys* correlation-keys)
        (incf *governance-web-count*) "web-ok"))
(load (test-source "public-outbound-gateway.lisp"))
(setf *public-outbound-audit-file* #P"/tmp/public-outbound-audit-test.json"
      *public-outbound-records* nil)
(with-open-file (out *public-outbound-audit-file* :direction :output
                     :if-exists :supersede :if-does-not-exist :create)
  (write-string
   (shasht:write-json
    (vector (obj "schema_version" 1
                 "envelope" (obj "kind" "unclassified" "id" "historic-1")))
    nil)
   out))
(%public-outbound-load)
(governance-check "historical audit rows reload without reclassification"
                  (and (= 1 (length *public-outbound-records*))
                       (= 1 (gethash "schema_version"
                                     (first *public-outbound-records*)))
                       (string= "unclassified"
                                (gethash "kind" (gethash "envelope"
                                                        (first *public-outbound-records*))))))
(setf *public-outbound-records* nil)
(ignore-errors (delete-file *public-outbound-audit-file*))
(governance-check "observe gateway preserves exactly one legacy send"
                  (and (string= "ok" (telegram-send "operator" "hello"))
                       (= 1 *governance-transport-count*)))
(let ((record (first *public-outbound-records*)))
  (governance-check "unclassified path is visible"
                    (string= "unclassified"
                             (gethash "kind" (gethash "envelope" record))))
  (governance-check "counterfactual withholding does not suppress observe send"
                    (and (string= "would-withhold"
                                  (gethash "counterfactual_decision" record))
                         (string= "legacy-permit"
                                  (gethash "effective_decision" record)))))

(let ((*public-outbound-envelope*
        (make-public-outbound-envelope
         :kind :reply :channel "web" :content "web reply"
         :source-event-ids (list "web-request-1")
         :causal-event-ids (list "web-request-1")
         :authorization-kind :inbound-request :authorization-id "web-request-1"
         :source "web-test" :dedupe-key "web-reply:1")))
  (governance-check "web presentation is called exactly once"
                    (and (string= "web-ok" (%v2-broadcast "final" "web reply"))
                         (= 1 *governance-web-count*)))
  (governance-check "web final is observed as a reply"
                    (let ((record (first *public-outbound-records*)))
                      (and (string= "reply"
                                    (gethash "kind" (gethash "envelope" record)))
                           (string= "presentation-returned"
                                    (gethash "transport_status" record)))))

  (governance-check "web thinking presentation is also observed as a reply"
                    (and (%v2-broadcast "thinking" "bounded progress")
                         (= 2 *governance-web-count*)
                         (string= "reply"
                                  (gethash "kind"
                                           (gethash "envelope"
                                                     (first *public-outbound-records*)))))))

(let ((before *governance-web-count*))
  (governance-check "late public-outbound wrapper preserves Rdev1 correlation keywords"
                    (and (string= "web-ok"
                                  (%v2-broadcast "user" "private input"
                                                 :trace-id "trace-fixture"
                                                 :turn-id "turn-fixture"))
                         (= (1+ before) *governance-web-count*)
                         (equal '(:trace-id "trace-fixture" :turn-id "turn-fixture")
                                *governance-web-correlation-keys*))))

(let ((*public-outbound-envelope*
        (make-public-outbound-envelope
         :kind :reply :channel "web"
         :source-event-ids (list "web-request-1")
         :causal-event-ids (list "web-request-1")
         :authorization-kind :inbound-request
         :authorization-id "web-request-1"
         :source "web-tool-test" :dedupe-key "web-tool:1"))
      (*public-tool-call-id* "call-authoritative-1")
      (*public-tool-result-id* "tool-result:call-authoritative-1")
      (*public-tool-call-event-id* 77))
  (%v2-broadcast "tool" "bounded tool status")
  (let* ((record (first *public-outbound-records*))
         (envelope (gethash "envelope" record))
         (causes (coerce (gethash "causal_event_ids" envelope) 'list)))
    (governance-check "web tool result carries authoritative call and result IDs"
                      (and (string= "tool-result" (gethash "kind" envelope))
                           (string= "call-authoritative-1"
                                    (gethash "tool_call_id" envelope))
                           (string= "tool-result:call-authoritative-1"
                                    (gethash "tool_result_id" envelope))
                           (string= "tool-result"
                                    (gethash "authorization_kind" envelope))
                           (string= "would-permit"
                                    (gethash "counterfactual_decision" record))))
    (governance-check "web tool result causal chain retains request and event IDs"
                      (and (member "web-request-1" causes :test #'string=)
                           (member "tool-call:call-authoritative-1"
                                   causes :test #'string=)
                           (member "tool-result:call-authoritative-1"
                                   causes :test #'string=)
                           (member "event:77" causes :test #'string=)))))

(let ((before *governance-web-count*))
  (%v2-broadcast "tool" "uncorrelated legacy status")
  (let ((record (first *public-outbound-records*)))
    (governance-check "missing tool correlation is visible but cannot block presentation"
                      (and (= (1+ before) *governance-web-count*)
                           (string= "would-withhold"
                                    (gethash "counterfactual_decision" record))
                           (string= "missing-tool-correlation"
                                    (gethash "counterfactual_reason" record))
                           (string= "legacy-permit"
                                    (gethash "effective_decision" record))))))

(setf *public-outbound-records* nil
      *public-outbound-audit-observer-count* 0
      *public-outbound-audit-observer-last-envelope-id* nil)
(runtime-observer-audit-reset)
(runtime-observer-unregister "public-outbound-evaluated"
                             "public-outbound-audit-observer")
(governance-check "miswired required observer fails boot assertion"
                  (not (runtime-observer-assert)))
(runtime-observer-register "public-outbound-evaluated"
                           "public-outbound-audit-observer"
                           #'%public-outbound-audit-observer
                           :capability :observe :required t)
(governance-check "restored required observer passes boot assertion"
                  (runtime-observer-assert))
(let ((kinds '(:reply :initiative :commitment :scheduled :system-alert :tool-result)))
  (dolist (kind kinds)
    (let* ((proof-required (member kind '(:initiative :commitment :scheduled)))
           (envelope
             (make-public-outbound-envelope
              :kind kind :content (format nil "fixture-~a" kind)
              :source-event-ids (list "source-1")
              :causal-event-ids (list "cause-1")
              :authorization-kind (if proof-required :fixture-proof :inbound-event)
              :authorization-id (if proof-required "proof-1" "inbound-1")
               :legacy-authorization (obj "decision" "permit")
               :v2-decision (obj "decision_id" "v2-1" "result" "approved")
               :tool-call-id (and (eq kind :tool-result) "fixture-call-1")
               :tool-result-id
               (and (eq kind :tool-result) "tool-result:fixture-call-1")
               :source "runtime-governance-fixture"
              :dedupe-key (format nil "fixture:~a" kind))))
      (with-public-outbound-envelope (envelope)
        (telegram-send "operator" (format nil "fixture-~a" kind)))))
  (governance-check "all six envelope kinds are typed"
                    (and (= 6 (length *public-outbound-records*))
                         (notany (lambda (record)
                                   (string= "unclassified"
                                            (gethash "kind" (gethash "envelope" record))))
                                 *public-outbound-records*)))
  (governance-check "typed sends carry causal and authorization evidence"
                    (every (lambda (record)
                             (let ((envelope (gethash "envelope" record)))
                               (and (plusp (length (gethash "source_event_ids" envelope)))
                                    (plusp (length (gethash "causal_event_ids" envelope)))
                                    (stringp (gethash "authorization_id" envelope))
                                    (gethash "canonical_public_act_id" record))))
                           *public-outbound-records*))
  (governance-check "content is redacted but fingerprinted"
                    (every (lambda (record)
                             (let ((envelope (gethash "envelope" record)))
                               (and (eq :null (gethash "content" envelope))
                                    (= 64 (length (gethash "content_sha256" envelope))))) )
                           *public-outbound-records*))
  (governance-check "required real observer consumed every typed record"
                    (= 6 (gethash "consumed"
                                  (public-outbound-audit-observer-report)))))
  (governance-check
   "every typed transport completion reaches the required A2 consumer"
   (let* ((coverage (gethash "coverage" (runtime-observer-audit-report)))
          (row (find "outbound-completed" coverage :test #'string=
                     :key (lambda (item) (gethash "logical_class" item)))))
     (and row (= 6 (gethash "consumed" row)))))

(let ((original (fdefinition '%public-outbound-counterfactual))
      (before *governance-transport-count*))
  (unwind-protect
       (progn
         (setf (fdefinition '%public-outbound-counterfactual)
               (lambda (envelope) (declare (ignore envelope))
                 (error "fixture evaluator failure")))
         (governance-check "evaluation failure preserves exactly one send"
                           (and (string= "ok" (telegram-send "operator" "again"))
                                 (= (1+ before) *governance-transport-count*)
                                (string= "evaluation-error"
                                         (gethash "counterfactual_decision"
                                                  (first *public-outbound-records*))))))
    (setf (fdefinition '%public-outbound-counterfactual) original)))

(load (test-source "runtime-truth.lisp"))
;; Seam authorities are declared by an init action now, not by loading this
;; file. The declaration depends on what :install actually installed, so it
;; cannot be a load-time side effect -- see RUNTIME-TRUTH-DECLARE-AUTHORITIES.
(runtime-truth-declare-authorities)
(setf (fdefinition 'brave-credential-status) (lambda () "available"))
(let ((manifest (runtime-truth-manifest)))
  (governance-check "runtime manifest identifies live source"
                    (string= "live-bound-values" (gethash "source_of_truth" manifest)))
  (governance-check "transport inventory is explicit"
                    (let* ((inventory (gethash "transport_inventory" manifest))
                           (tool-row
                             (find "public-tool-presentation" inventory
                                   :key (lambda (row) (gethash "owner" row))
                                   :test #'string=)))
                      (and (= 12 (length inventory))
                           tool-row
                           (string=
                            "tool-call-id+tool-result-id+active-inbound-request"
                            (gethash "proof" tool-row))))))
  (governance-check
   "runtime truth exposes content-free A2 lifecycle coverage"
   (let* ((manifest (runtime-truth-manifest))
          (audit (gethash "observer_audit" manifest)))
     (and (= 10 (gethash "required_classes" audit))
          (not (gethash "retains_payload_content" audit)))))
  (governance-check
   "private admin status never declares delivery authority"
   (let ((manifest (runtime-truth-manifest)))
     (not (gethash "delivery_authority" (gethash "private_admin" manifest)))))
  (governance-check
   "every required decision class has exactly one declarative authority"
   (let* ((manifest (runtime-truth-manifest))
          (authorities (gethash "authorities" manifest))
          (classes (gethash "decision_classes" authorities)))
     (and (= 6 (length classes))
          (not (gethash "grants_capability" authorities))
          (every (lambda (row)
                   (and (gethash "required" row)
                        (= 1 (gethash "authority_count" row))))
                 (coerce classes 'list))
          (runtime-authority-assert))))

(let* ((path #P"/tmp/runtime-truth-context-config.json")
       (*conversation-context-config-file* path))
  (with-open-file (out path :direction :output :if-exists :supersede
                            :if-does-not-exist :create)
    (write-string
     (shasht:write-json
      (obj "schema_version" 1 "target_records" 60
           "minimum_recent_records" 24 "hard_records" 100
           "target_estimated_tokens" 100000
           "hard_estimated_tokens" 160000 "target_chars" 400000
           "hard_chars" 640000 "brief_chars" 4000
           "tool_result_chars" 6000)
      nil)
     out))
  (let* ((manifest
           (progv '(*conversation-context-target-tokens*) '(100000)
             (runtime-truth-manifest)))
         (rows (gethash "parameter_rows" manifest))
         (row (find "context.target_estimated_tokens" rows
                    :key (lambda (item) (gethash "name" item))
                    :test #'string=)))
    (governance-check "runtime truth compares persisted and live context parameters"
                      (and (= 100000 (gethash "persisted" row))
                           (= 100000 (gethash "live" row))
                           (string= "match" (gethash "drift" row)))))
  (ignore-errors (delete-file path)))
(unwind-protect
     (progn
       (runtime-authority-declare "transport" "fixture-competing-owner")
       (governance-check
        "a competing effective authority fails deterministically"
        (handler-case (progn (runtime-authority-assert) nil)
          (error () t))))
  (setf (gethash "transport" *runtime-authorities*)
        (remove "fixture-competing-owner"
                (gethash "transport" *runtime-authorities*) :test #'string=)))
(governance-check "authority repair restores the boot assertion"
                  (runtime-authority-assert))
(let ((original (copy-list (gethash "transport" *runtime-authorities*))))
  (unwind-protect
       (progn
         (setf (gethash "transport" *runtime-authorities*)
               (list "wrong-single-owner"))
         (governance-check
          "a singular declaration that disagrees with the live owner fails"
          (handler-case (progn (runtime-truth-assert) nil)
            (error () t))))
    (setf (gethash "transport" *runtime-authorities*) original)))
(governance-check
 "production source has no non-reply transport escape"
 (runtime-nonreply-transport-source-assert))
(let ((root #P"/tmp/transport-source-miswire/")
      (path #P"/tmp/transport-source-miswire/escape.lisp"))
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                            :if-does-not-exist :create)
    (write-line "(telegram-send chat-id content)" out))
  (governance-check
   "a source-level transport escape fails deterministically"
   (not (gethash "passed" (runtime-nonreply-transport-source-audit root))))
  (ignore-errors (delete-file path)))
;; The cold-boot contract is declared in the init registry, not in a
;; container definition.
;;
;; This check used to grep the ENTRYPOINT for assertion names. That artifact
;; is gone: pai.asd carries the load chain and DEFINE-INIT :verify carries the
;; boot assertions. Asserting against the registry is the same property
;; expressed against the thing that now owns it -- and it is a stronger check,
;; because a name in a shell string proved only that somebody typed it.
;;
;; Two of these were in fact missing. The load/init separation converted
;; load-time side effects found in source, and these three assertions existed
;; only in the ENTRYPOINT, so nothing carried them across; pAI booted without
;; them until this suite was made runnable again.
(let ((verify-actions (mapcar #'init-action-name (init-actions :verify))))
  (governance-check
   "cold boot asserts observers, authorities, owners, and transport source"
   (every (lambda (name) (member name verify-actions))
          '(observer-registry-assert
            runtime-truth-assert-boot
            runtime-nonreply-transport-source-assert-boot))))
(let ((manifest (runtime-truth-manifest)))
  (governance-check
   "runtime truth exposes Brave presence without a credential value"
   (let* ((providers (gethash "providers" manifest))
          (status (gethash "credential_status" providers)))
     (and (string= "redacted" (gethash "credentials" providers))
          (string= "available" (gethash "brave_search" status))))))

(let ((manifest (runtime-truth-manifest)) (failed nil))
  (setf (gethash "drift" (aref (gethash "mode_rows" manifest) 0)) "mismatch")
  (handler-case (runtime-truth-assert manifest) (error () (setf failed t)))
  (governance-check "persisted/live mismatch fails deterministically" failed))

(let ((manifest (runtime-truth-manifest)) (failed nil))
  (setf (gethash "status" (aref (gethash "final_owners" manifest) 0))
        "unexpected-final-owner")
  (handler-case (runtime-truth-assert manifest) (error () (setf failed t)))
  (governance-check "unexpected final owner fails deterministically" failed))

(let ((origins
        '(("telegram.lisp" "telegram-reactive" "telegram-reactive-error")
          ("candidate-policy.lisp" "candidate-policy-commitment"
           "candidate-policy-v2-initiative")
          ("drives.lisp" "legacy-drives")
          ("scheduler.lisp" ":kind :scheduled")
          ("workout_nudge.lisp" "workout-nudge")
          ("turn-watchdog.lisp" "turn-watchdog"))))
  (governance-check
   "every declared source origin contains an explicit envelope binding"
   (every
    (lambda (spec)
      (let ((source (uiop:read-file-string
                     (namestring (test-source (first spec))))))
        (and (search "public-outbound-envelope" source :test #'char-equal)
             (every (lambda (marker) (search marker source :test #'char-equal))
                    (rest spec)))))
    origins)))

(let ((event-source (uiop:read-file-string
                     (namestring (test-source "event-log.lisp"))))
      (gateway-source (uiop:read-file-string
                       (namestring (test-source "public-outbound-gateway.lisp")))))
  (governance-check
   "returned web and terminal presentations use the registered outbound port"
   (and (search "*public-presentation-observer*" event-source
                :test #'char-equal)
        (search "terminal" event-source :test #'char-equal)
        (search "public-outbound-presentation-port" gateway-source
                :test #'char-equal)
        (search "make-public-outbound-envelope" gateway-source
                :test #'char-equal))))

(let ((loop-source (uiop:read-file-string
                    (namestring (test-source "agent_loop.lisp"))))
      (event-source (uiop:read-file-string
                     (namestring (test-source "event-log.lisp"))))
      (capture-source (uiop:read-file-string
                       (namestring (test-source "conversation-turn-capture.lisp"))))
      (gateway-source (uiop:read-file-string
                       (namestring (test-source "public-outbound-gateway.lisp")))))
  (governance-check
   "direct and wrapped tool paths share authoritative outbound correlation"
   (and (search "*public-tool-call-id*" loop-source :test #'char-equal)
        (search "*public-tool-result-id*" loop-source :test #'char-equal)
        (every (lambda (source)
                 (and (search "tool_call_id" source :test #'char-equal)
                      (search "tool_result_id" source :test #'char-equal)))
               (list event-source capture-source gateway-source)))))

(let* ((fixture (shasht:read-json
                 (uiop:read-file-string
                  (namestring (merge-pathnames "evals/fixtures/agency-v1-anchors.json" *pai-root*)))))
       (anchors (gethash "anchors" fixture)))
  (governance-check "agency corpus has ten stable anchors" (= 10 (length anchors)))
  (governance-check "agency corpus labels fixture provenance"
                    (every (lambda (row)
                             (member (gethash "source" row)
                                     '("captured-production-event"
                                       "captured-conversation"
                                       "captured-pattern" "deterministic")
                                     :test #'string=))
                           (coerce anchors 'list))))

(load (test-source "replay-capsules.lisp"))
(setf *replay-capsule-file* #P"/tmp/replay-capsules-test.json"
      *replay-capsules* nil)
(ignore-errors (delete-file *replay-capsule-file*))
(let ((capsule (replay-capsule-capture "fixture" :trigger-type "test" :trigger-id "1")))
  (governance-check "capsule records explicit fidelity"
                    (and (= 2 (gethash "schema_version" capsule))
                         (string= "exact"
                                  (gethash "fidelity"
                                           (gethash "references" capsule)))))
  (let* ((conversation (gethash "conversation" capsule))
         (value (gethash "value" conversation)))
    (governance-check "capsule does not duplicate conversation content"
                      (or (eq value :null)
                          (and (hash-table-p value)
                               (not (gethash "content_in_capsule" value)))))))

(format t "~&runtime governance tests: ~d passed, ~d failed.~%"
        *runtime-governance-pass* *runtime-governance-fail*)
(when (plusp *runtime-governance-fail*)
  (error "runtime governance tests failed"))
