(ql:quickload '(:postmodern :shasht :ironclad :babel) :silent t)

(defpackage :agent (:use :cl))
(in-package :agent)

(defun obj (&rest pairs)
  (loop with table = (make-hash-table :test #'equal)
        for (key value) on pairs by #'cddr
        do (setf (gethash key table) value)
        finally (return table)))

(defmacro with-pg (&body body) `(progn ,@body))

(load (test-source "memory-architecture.lisp"))
(load (test-source "memory-atom-candidate.lisp"))
(load (test-source "mind-memory-core.lisp"))

;; Pin the module's partition keys to this fixture's ids.
;;
;; Attribution validity is decided by comparing an atom's subject against
;; these (%ATTRIBUTION-VALID-P): subject = operator id takes the "user"
;; evidence branch, subject = agent id takes the "assistant" branch, and
;; anything else falls through to a weaker rule. So the fixture ids are not
;; decoration -- they select which contract is under test. Leaving them to
;; the environment-derived defaults makes this suite pass or fail depending
;; on PAI_OPERATOR_ID, which is exactly the coupling the golden contract
;; exists to rule out.
;;
;; The facade in memory-architecture.lisp carries its own partition-key
;; default, so pinning only the module's would leave the facade comparing
;; fixture rows against a different id and reporting everything ineligible.
(setf pai.mind.memory::*agent-id* "test-agent"
      pai.mind.memory::*operator-id* "test-operator"
      agent::*memory-architecture-agent-id* "test-agent")

(in-package :cl-user)

(defvar *mind-memory-golden-pass* 0)
(defvar *mind-memory-golden-fail* 0)

(defun mind-memory-golden-check (name condition)
  (if condition
      (progn (incf *mind-memory-golden-pass*) (format t "PASS ~a~%" name))
      (progn (incf *mind-memory-golden-fail*) (format t "FAIL ~a~%" name))))

(defun mind-memory-golden-tree-equal (left right)
  (cond
    ((and (hash-table-p left) (hash-table-p right))
     (and (= (hash-table-count left) (hash-table-count right))
          (loop for key being the hash-keys of left
                always (multiple-value-bind (other present-p) (gethash key right)
                         (and present-p
                              (mind-memory-golden-tree-equal
                               (gethash key left) other))))))
    ((and (vectorp left) (vectorp right))
     (and (= (length left) (length right))
          (loop for a across left for b across right
                always (mind-memory-golden-tree-equal a b))))
    ((and (consp left) (consp right))
     (and (mind-memory-golden-tree-equal (car left) (car right))
          (mind-memory-golden-tree-equal (cdr left) (cdr right))))
    (t (equal left right))))

(defun mind-memory-golden-error (thunk)
  (handler-case (progn (funcall thunk) nil)
    (error (condition) (princ-to-string condition))))

(defun mind-memory-golden-object (&rest pairs)
  (apply #'agent::obj pairs))

(defun mind-memory-golden-sha256 (text)
  (string-downcase
   (ironclad:byte-array-to-hex-string
    (ironclad:digest-sequence
     :sha256 (babel:string-to-octets text :encoding :utf-8)))))

(let ((cases
        (list
         (cons nil nil)
         (cons '(:agent-id "test-agent" :memory-form "Semantic") nil)
         (cons '(:agent-id "bad space" :memory-form "other"
                 :disclosure-class "secret" :share-review-status "maybe")
               '("invalid agent_id" "invalid memory_form"
                 "invalid disclosure_class" "invalid share_review_status"
                 "non-private disclosure requires an approved durable review"))
         (cons '(:disclosure-class "public" :share-review-status "approved"
                 :share-review-event-id "review:1" :share-reviewed-at 10)
               nil)
         (cons '(:disclosure-class "personal-shareable"
                 :share-review-status "pending")
               '("non-private disclosure requires an approved durable review"))
         (cons '(:valid-from 20 :valid-to 10)
               '("valid_to must be later than valid_from")))))
  (loop for case in cases for index from 1 do
    (mind-memory-golden-check
     (format nil "state validation golden ~d" index)
     (equal (cdr case)
            (apply #'agent::memory-architecture-validate-state (car case))))))

(let ((rows
        (list
         (mind-memory-golden-object
          "agent_id" "test-agent" "disclosure_class" "private"
          "share_review_status" :null "share_review_event_id" :null
          "share_reviewed_at" :null)
         (mind-memory-golden-object
          "agent_id" "test-agent" "disclosure_class" "public"
          "share_review_status" "approved" "share_review_event_id" "review:1"
          "share_reviewed_at" "2026-08-11T00:00:00Z")
         (mind-memory-golden-object
          "agent_id" "watcher" "disclosure_class" "public"
          "share_review_status" "approved" "share_review_event_id" "review:2"
          "share_reviewed_at" "2026-08-11T00:00:00Z")))
      (expected '((t nil nil) (t t nil) (nil nil nil))))
  (loop for row in rows for row-expected in expected for index from 1 do
    (loop for audience in '(:operator :external-party :unknown)
          for wanted in row-expected do
      (mind-memory-golden-check
       (format nil "eligibility golden ~d ~a" index audience)
       (eql wanted
            (not (null (agent::memory-architecture-row-eligible-p
                        row :audience audience))))))))

(let* ((evidence
         (vector
          (mind-memory-golden-object
           "id" "turn:1:user" "role" "user" "sequence" 0
           "observed_at" "2026-08-11T00:00:00Z"
           "content" (format nil "the operator prefers ~s on pizza.~%Café note."
                             "mushrooms"))
          (mind-memory-golden-object
           "id" "turn:1:assistant" "role" "assistant" "sequence" 1
           "observed_at" "2026-08-11T00:00:01Z"
           "content" "the agent planned to make a shopping list.")))
       (manifest
         (agent::memory-atom-build-manifest
          "turn:1" "2026-08-11T00:00:02Z" evidence))
       (expected-manifest
         (mind-memory-golden-object
          "schema_version" 1
          "contract_version" "n1-n2-atom-identity-v1"
          "prompt_version" "n1-decomposer-v1"
          "agent_id" "test-agent" "turn_id" "turn:1"
          "captured_at" "2026-08-11T00:00:02Z" "evidence" evidence))
       (request (agent::memory-atom-build-request manifest))
       (response
         (mind-memory-golden-object
          "schema_version" 1 "decision" "PROPOSE"
          "atoms"
          (vector
           (mind-memory-golden-object
            "memory_form" "semantic" "subject" "test-operator"
            "predicate" "food.prefers"
            "value" "the operator prefers mushrooms on pizza."
            "polarity" "affirmed" "qualifiers" #()
            "observed_at" "2026-08-11T00:00:00Z"
            "valid_from" :null "valid_to" :null
            "disclosure_candidate" "personal-shareable"
            "evidence_ids" #("turn:1:user"))
           (mind-memory-golden-object
            "memory_form" "episodic" "subject" "test-agent"
            "predicate" "planned"
            "value" "the agent planned to make a shopping list."
            "polarity" "affirmed"
            "qualifiers" (vector (mind-memory-golden-object
                                   "name" "scope" "value" "shopping"))
            "observed_at" "2026-08-11T00:00:01Z"
            "valid_from" :null "valid_to" :null
            "disclosure_candidate" "private"
            "evidence_ids" #("turn:1:assistant")))
          "exclusions" #()
          "uncertainty" (mind-memory-golden-object
                          "level" "low" "note" "Direct statements.")))
       (normalized (agent::memory-atom-validate-response response manifest))
       (expected-normalized
         (mind-memory-golden-object
          "schema_version" 1 "decision" "PROPOSE"
          "atoms"
          (vector
           (mind-memory-golden-object
            "candidate_id" "atom-candidate:cc3509f10bd725cd6661f64bf943c3c2"
            "claim_key" "1bdd4a60d991eeeb0fc9562ee09a93f503bbbf6148e217722709cdcc8d36a21d"
            "idempotency_key" "cc3509f10bd725cd6661f64bf943c3c250dee9994bccd76bdcff69b3ef63c142"
            "agent_id" "test-agent" "memory_form" "semantic" "subject" "test-operator"
            "predicate" "food.prefers"
            "value" "the operator prefers mushrooms on pizza."
            "polarity" "affirmed" "qualifiers" #()
            "observed_at" "2026-08-11T00:00:00Z"
            "valid_from" :null "valid_to" :null
            "evidence_ids" #("turn:1:user")
            "disclosure_candidate" "personal-shareable"
            "persistence_projection"
            (mind-memory-golden-object
             "memory_form" "semantic" "disclosure_class" "private"
             "share_review_status" "pending"))
           (mind-memory-golden-object
            "candidate_id" "atom-candidate:8efa3e96e15064c980d9714640d5b4d6"
            "claim_key" "0fe3a6328abbc916328dea6cbece2971aba3c3de16caa977c5af6eea94d2239e"
            "idempotency_key" "8efa3e96e15064c980d9714640d5b4d6e95d389b27d047c200caaf18106f2f0e"
            "agent_id" "test-agent" "memory_form" "episodic" "subject" "test-agent"
            "predicate" "planned"
            "value" "the agent planned to make a shopping list."
            "polarity" "affirmed"
            "qualifiers" (vector (mind-memory-golden-object
                                   "name" "scope" "value" "shopping"))
            "observed_at" "2026-08-11T00:00:01Z"
            "valid_from" :null "valid_to" :null
            "evidence_ids" #("turn:1:assistant")
            "disclosure_candidate" "private"
            "persistence_projection"
            (mind-memory-golden-object
             "memory_form" "episodic" "disclosure_class" "private"
             "share_review_status" :null)))
          "exclusions" #()
          "uncertainty" (mind-memory-golden-object
                          "level" "low" "note" "Direct statements.")))
       (expected-capability
         (mind-memory-golden-object
          "schema_version" 1
          "contract_version" "n1-n2-atom-identity-v1"
          "prompt_version" "n1-decomposer-v1"
          "memory_forms" #("episodic" "semantic" "procedural")
          "raw_evidence_immutable" t "admission_available" nil
          "provider_calls_available" nil "database_writes_available" nil
          "ticks_available" nil "delivery_authority" nil)))
  (mind-memory-golden-check
   "atom manifest exact golden tree"
   (mind-memory-golden-tree-equal expected-manifest manifest))
  (let* ((system-message (aref request 0))
         (user-message (aref request 1))
         (json (gethash "content" user-message))
         (octets (babel:string-to-octets json :encoding :utf-8)))
    (mind-memory-golden-check
     "atom request exact structure and UTF-8 golden"
     (and (= 2 (length request))
          (= 2 (hash-table-count system-message))
          (= 2 (hash-table-count user-message))
          (string= "system" (gethash "role" system-message))
          (plusp (length (gethash "content" system-message "")))
          (string= "user" (gethash "role" user-message))
          ;; Re-baselined for pAI. The inherited golden hashed a request whose
          ;; fixture text carried the originating instance's real names; those
          ;; strings are shorter than the neutral ids used here, so the length
          ;; and digest necessarily move. The serialized structure was compared
          ;; field by field against the original before rebaselining and is
          ;; unchanged -- same keys, same order, same nesting.
          ;;
          ;; The octet count staying exactly one above the character count is
          ;; load-bearing: it proves the one non-ASCII character in the fixture
          ;; ("Café") survives as UTF-8 rather than being mangled or stripped.
          ;; Keep a non-ASCII character in the evidence text or this assertion
          ;; silently stops testing encoding.
          (= 629 (length json))
          (= 630 (length octets))
          (string= "37b129738d7845a658ceddf8c84c817d64b0475cb9adbd8e48e4cacaaebcf4ce"
                   (mind-memory-golden-sha256 json)))))
  (mind-memory-golden-check
   "atom normalized response exact golden tree"
   (mind-memory-golden-tree-equal expected-normalized normalized))
  (mind-memory-golden-check
   "pure request construction remains deterministic"
   (mind-memory-golden-tree-equal
    request (agent::memory-atom-build-request manifest)))
  (mind-memory-golden-check
   "pure response validation remains deterministic"
   (mind-memory-golden-tree-equal
    normalized (agent::memory-atom-validate-response response manifest)))
  (mind-memory-golden-check
   "atom capability report exact golden tree"
   (mind-memory-golden-tree-equal
    expected-capability (agent::memory-atom-candidate-report)))
  (mind-memory-golden-check
   "invalid turn exact failure"
   (string= "Turn id is invalid."
            (mind-memory-golden-error
             (lambda () (agent::memory-atom-build-manifest
                         "bad id" "2026-08-11T00:00:02Z" evidence)))))
  (let ((bad-response
          (mind-memory-golden-object
           "schema_version" 1 "decision" "PROPOSE"
           "atoms"
           (vector
            (mind-memory-golden-object
             "memory_form" "semantic" "subject" "test-operator"
             "predicate" "claimed"
             "value" "Unsupported assistant attribution."
             "polarity" "affirmed" "qualifiers" #()
             "observed_at" "2026-08-11T00:00:01Z"
             "valid_from" :null "valid_to" :null
             "disclosure_candidate" "private"
             "evidence_ids" #("turn:1:assistant")))
           "exclusions" #()
           "uncertainty" (mind-memory-golden-object
                           "level" "high" "note" "Invalid attribution."))))
    (mind-memory-golden-check
     "attribution exact failure"
     (string= "Atom attribution is not supported by an eligible evidence role."
              (mind-memory-golden-error
               (lambda () (agent::memory-atom-validate-response
                           bad-response manifest)))))))

(format t "RESULT mind-memory-golden-contract: ~d passed, ~d failed~%"
        *mind-memory-golden-pass* *mind-memory-golden-fail*)
(when (plusp *mind-memory-golden-fail*) (uiop:quit 1))
