(in-package :agent)

(ql:quickload '(:shasht :ironclad :babel) :silent t)

(defvar *memory-atom-test-pass* 0)
(defvar *memory-atom-test-fail* 0)

(defun memory-atom-test-check (name condition)
  (if condition
      (progn (incf *memory-atom-test-pass*) (format t "PASS ~a~%" name))
      (progn (incf *memory-atom-test-fail*) (format t "FAIL ~a~%" name))))

(defun memory-atom-test-signals-p (thunk)
  (handler-case (progn (funcall thunk) nil)
    (error () t)))

(load (test-source "memory-atom-candidate.lisp"))

(defparameter *memory-atom-test-evidence*
  (vector
   (obj "id" "turn-fixture-user-0000" "role" "user" "sequence" 0
        "observed_at" "2026-08-06T22:00:00Z"
        "content" "I prefer tea, I visited the library today, and remind me to bring my notebook next time.")
   (obj "id" "turn-fixture-assistant-0001" "role" "assistant" "sequence" 1
        "observed_at" "2026-08-06T22:00:02Z"
        "content" "the agent acknowledged the three details.")))

(defparameter *memory-atom-test-manifest*
  (memory-atom-build-manifest
   "turn-fixture" "2026-08-06T22:00:03Z" *memory-atom-test-evidence*))

(defun memory-atom-test-atom
    (form predicate value &key (roots (vector "turn-fixture-user-0000"))
          (subject "operator") (polarity "affirmed")
          (observed-at "2026-08-06T22:00:00Z")
          (valid-from :null) (valid-to :null) (disclosure "private")
          (qualifiers (vector)))
  (obj "memory_form" form "subject" subject "predicate" predicate
       "value" value "polarity" polarity "qualifiers" qualifiers
       "observed_at" observed-at "valid_from" valid-from "valid_to" valid-to
       "disclosure_candidate" disclosure "evidence_ids" roots))

(defparameter *memory-atom-test-response*
  (obj
   "schema_version" 1 "decision" "PROPOSE"
   "atoms"
   (vector
    (memory-atom-test-atom
     "semantic" "preference.drink" "the operator prefers tea."
     :disclosure "personal-shareable")
    (memory-atom-test-atom
     "episodic" "visited.place" "the operator visited the library."
     :valid-from "2026-08-06T00:00:00Z"
     :valid-to "2026-08-07T00:00:00Z"
     :qualifiers (vector (obj "name" "place" "value" "library")))
    (memory-atom-test-atom
     "procedural" "reminder.bring" "Bring the operator's notebook next time."
     :qualifiers (vector (obj "name" "item" "value" "notebook"))))
   "exclusions"
   (vector (obj "evidence_ids" (vector "turn-fixture-assistant-0001")
                "reason" "Acknowledgement adds no independent claim."))
   "uncertainty" (obj "level" "low" "note" "Direct user evidence.")))

(format t "~%== manifest and request ==~%")
(memory-atom-test-check
 "manifest preserves ordered immutable evidence"
 (equal '("turn-fixture-user-0000" "turn-fixture-assistant-0001")
        (mapcar (lambda (row) (gethash "id" row))
                (coerce (gethash "evidence" *memory-atom-test-manifest*) 'list))))
(let ((request (memory-atom-build-request *memory-atom-test-manifest*)))
  (memory-atom-test-check "request is a conventional two-message context"
                          (and (= 2 (length request))
                               (string= "system" (gethash "role" (aref request 0)))
                               (string= "user" (gethash "role" (aref request 1)))))
  (memory-atom-test-check "request contains the exact manifest"
                          (search "turn-fixture-user-0000"
                                  (gethash "content" (aref request 1)))))

(format t "~%== validated three-form decomposition ==~%")
(let* ((first (memory-atom-validate-response
               *memory-atom-test-response* *memory-atom-test-manifest*))
       (second (memory-atom-validate-response
                *memory-atom-test-response* *memory-atom-test-manifest*))
       (atoms (coerce (gethash "atoms" first) 'list)))
  (memory-atom-test-check "preference event and instruction produce three atoms"
                          (= 3 (length atoms)))
  (memory-atom-test-check "three memory forms remain distinct"
                          (equal '("semantic" "episodic" "procedural")
                                 (mapcar (lambda (row)
                                           (gethash "memory_form" row))
                                         atoms)))
  (memory-atom-test-check "every atom retains an exact immutable root"
                          (every (lambda (row)
                                   (equal '("turn-fixture-user-0000")
                                          (coerce (gethash "evidence_ids" row)
                                                  'list)))
                                 atoms))
  (memory-atom-test-check "absolute validity interval is preserved"
                          (and (string= "2026-08-06T00:00:00Z"
                                        (gethash "valid_from" (second atoms)))
                               (string= "2026-08-07T00:00:00Z"
                                        (gethash "valid_to" (second atoms)))))
  (memory-atom-test-check "repeat validation is byte-stable"
                          (string= (shasht:write-json first nil)
                                   (shasht:write-json second nil)))
  (memory-atom-test-check "claim and idempotency keys are SHA-256"
                          (every (lambda (row)
                                   (and (= 64 (length (gethash "claim_key" row)))
                                        (= 64 (length
                                               (gethash "idempotency_key" row)))))
                                 atoms))
  (let ((projection (gethash "persistence_projection" (first atoms))))
    (memory-atom-test-check
     "non-private proposal remains private and only pending review"
     (and (string= "private" (gethash "disclosure_class" projection))
          (string= "pending" (gethash "share_review_status" projection))))))

(format t "~%== claim identity versus evidence occurrence ==~%")
(let* ((other-evidence
         (vector
          (obj "id" "turn-repeat-user-0000" "role" "user" "sequence" 0
               "observed_at" "2026-08-07T22:00:00Z"
               "content" "I prefer tea.")))
       (other-manifest
         (memory-atom-build-manifest
          "turn-repeat" "2026-08-07T22:00:01Z" other-evidence))
       (other-response
         (obj "schema_version" 1 "decision" "PROPOSE"
              "atoms"
              (vector
               (memory-atom-test-atom
                "semantic" "preference.drink" "the operator prefers tea."
                :roots (vector "turn-repeat-user-0000")
                :observed-at "2026-08-07T22:00:00Z"
                :disclosure "personal-shareable"))
              "exclusions" (vector)
              "uncertainty" (obj "level" "low" "note" "Direct report.")))
       (first (aref (gethash "atoms"
                             (memory-atom-validate-response
                              *memory-atom-test-response*
                              *memory-atom-test-manifest*)) 0))
       (other (aref (gethash "atoms"
                             (memory-atom-validate-response
                              other-response other-manifest)) 0)))
  (memory-atom-test-check "same structural claim has the same claim key"
                          (string= (gethash "claim_key" first)
                                   (gethash "claim_key" other)))
  (memory-atom-test-check "different evidence has a distinct idempotency key"
                          (not (string= (gethash "idempotency_key" first)
                                        (gethash "idempotency_key" other)))))

(format t "~%== abstention and fail-closed validation ==~%")
(let ((no-atoms
        (obj "schema_version" 1 "decision" "NO_ATOMS" "atoms" (vector)
             "exclusions"
             (vector (obj "evidence_ids" (vector "turn-fixture-user-0000")
                          "reason" "The statement is hypothetical or quoted."))
             "uncertainty" (obj "level" "low" "note" "No grounded claim."))))
  (memory-atom-test-check "quotation or hypothetical may explicitly abstain"
                          (string= "NO_ATOMS"
                                   (gethash "decision"
                                            (memory-atom-validate-response
                                             no-atoms
                                             *memory-atom-test-manifest*)))))
(memory-atom-test-check
 "unknown evidence root is rejected"
 (memory-atom-test-signals-p
  (lambda ()
    (let ((response
            (obj "schema_version" 1 "decision" "PROPOSE"
                 "atoms" (vector (memory-atom-test-atom
                                  "semantic" "preference.drink" "Tea."
                                  :roots (vector "invented-root")))
                 "exclusions" (vector)
                 "uncertainty" (obj "level" "high" "note" "Bad root."))))
      (memory-atom-validate-response response *memory-atom-test-manifest*)))))
(memory-atom-test-check
 "assistant-only evidence cannot establish a the operator claim"
 (memory-atom-test-signals-p
  (lambda ()
    (memory-atom-validate-response
     (obj "schema_version" 1 "decision" "PROPOSE"
          "atoms" (vector (memory-atom-test-atom
                           "semantic" "appearance.hair" "the operator has blonde hair."
                           :roots (vector "turn-fixture-assistant-0001")
                           :observed-at "2026-08-06T22:00:02Z"))
          "exclusions" (vector)
          "uncertainty" (obj "level" "high" "note" "Assistant only."))
     *memory-atom-test-manifest*))))
(memory-atom-test-check
 "relative or malformed atom time is rejected"
 (memory-atom-test-signals-p
  (lambda ()
    (memory-atom-validate-response
     (obj "schema_version" 1 "decision" "PROPOSE"
          "atoms" (vector (memory-atom-test-atom
                           "episodic" "visited.place" "Visited yesterday."
                           :valid-from "yesterday"))
          "exclusions" (vector)
          "uncertainty" (obj "level" "high" "note" "Bad time."))
     *memory-atom-test-manifest*))))
(memory-atom-test-check
 "duplicate structural claims are rejected"
 (memory-atom-test-signals-p
  (lambda ()
    (let ((atom (memory-atom-test-atom
                 "semantic" "preference.drink" "the operator prefers tea.")))
      (memory-atom-validate-response
       (obj "schema_version" 1 "decision" "PROPOSE"
            "atoms" (vector atom atom) "exclusions" (vector)
            "uncertainty" (obj "level" "low" "note" "Duplicate."))
       *memory-atom-test-manifest*)))))
(memory-atom-test-check
 "unknown response key is rejected"
 (memory-atom-test-signals-p
  (lambda ()
    (memory-atom-validate-response
     (obj "schema_version" 1 "decision" "NO_ATOMS" "atoms" (vector)
          "exclusions" (vector)
          "uncertainty" (obj "level" "low" "note" "None.")
          "approval" t)
     *memory-atom-test-manifest*))))
(memory-atom-test-check
 "out-of-order evidence is rejected"
 (memory-atom-test-signals-p
  (lambda ()
    (memory-atom-build-manifest
     "turn-order" "2026-08-06T22:00:03Z"
     (vector (aref *memory-atom-test-evidence* 1)
             (aref *memory-atom-test-evidence* 0))))))
(memory-atom-test-check
 "impossible calendar timestamps are rejected"
 (memory-atom-test-signals-p
  (lambda ()
    (memory-atom-build-manifest
     "turn-calendar" "2026-02-30T22:00:00Z"
     (vector (obj "id" "calendar-user-0000" "role" "user" "sequence" 0
                  "observed_at" "2026-02-28T22:00:00Z"
                  "content" "A bounded fixture."))))))
(memory-atom-test-check
 "evidence observed after capture is rejected"
 (memory-atom-test-signals-p
  (lambda ()
    (memory-atom-build-manifest
     "turn-future" "2026-08-06T22:00:00Z"
     (vector (obj "id" "future-user-0000" "role" "user" "sequence" 0
                  "observed_at" "2026-08-06T22:00:01Z"
                  "content" "A bounded fixture."))))))

(format t "~%== structural capability boundary ==~%")
(let* ((report (memory-atom-candidate-report))
       (source (uiop:read-file-string
                (namestring (test-source "memory-atom-candidate.lisp")))))
  (memory-atom-test-check
   "candidate reports no admission provider writes ticks or delivery"
   (and (null (gethash "admission_available" report))
        (null (gethash "provider_calls_available" report))
        (null (gethash "database_writes_available" report))
        (null (gethash "ticks_available" report))
        (null (gethash "delivery_authority" report))))
  (memory-atom-test-check
   "candidate source has no live authority primitive"
   (notany (lambda (needle) (search needle source :test #'char-equal))
           '("raw-call-model" "memory-admit-node" "memory-write-node"
             "send-telegram" "public-outbound" "auto-turn"))))

(format t "~%MEMORY-ATOM-CANDIDATE TESTS: ~a passed, ~a failed.~%"
        *memory-atom-test-pass* *memory-atom-test-fail*)
(when (plusp *memory-atom-test-fail*) (sb-ext:exit :code 1))
