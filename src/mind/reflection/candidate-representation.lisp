;;;; candidate-representation.lisp -- V6 typed artifact and diagnostic contract.
;;;;
;;;; Capability-free: no provider, transport, memory, policy, timer or worker.

(in-package :agent)

(export '(candidate-artifact-class candidate-generation-contract
          candidate-representation-normalize-record
          candidate-send-readiness-lint candidate-reduced-e0-envelope
          candidate-representation-report))

(defparameter *candidate-representation-schema-version* 2)
(defparameter *candidate-representation-artifact-classes*
  '("internal-stance" "rendered-draft"))
(defparameter *candidate-representation-max-content-characters* 1000)

(defun %candidate-representation-string (value)
  (and (stringp value) (plusp (length value)) value))

(defun candidate-artifact-class (record)
  (let ((value (and (hash-table-p record) (gethash "artifact_class" record))))
    (if (member value *candidate-representation-artifact-classes*
                :test #'string=)
        value
        "internal-stance")))

(defun %candidate-default-generation-contract (record artifact-class)
  (let ((source (and (hash-table-p record) (gethash "source" record))))
    (cond ((string= artifact-class "rendered-draft")
           "semantic-publication-legacy")
          ((string= (or source "") "explore-development")
           "explore-stance-v1")
          (t "legacy-unversioned"))))

(defun candidate-generation-contract (record)
  (or (%candidate-representation-string
       (and (hash-table-p record) (gethash "generation_contract" record)))
      (%candidate-default-generation-contract
       record (candidate-artifact-class record))))

(defun candidate-representation-normalize-record (record)
  "Normalize RECORD in place. Return RECORD and whether any field changed."
  (unless (hash-table-p record)
    (return-from candidate-representation-normalize-record
      (values record nil)))
  (let* ((changed nil)
         (artifact-class (candidate-artifact-class record))
         (contract (candidate-generation-contract record)))
    (unless (eql (gethash "schema_version" record)
                 *candidate-representation-schema-version*)
      (setf (gethash "schema_version" record)
            *candidate-representation-schema-version*
            changed t))
    (unless (string= (or (gethash "artifact_class" record) "") artifact-class)
      (setf (gethash "artifact_class" record) artifact-class changed t))
    (unless (string= (or (gethash "generation_contract" record) "") contract)
      (setf (gethash "generation_contract" record) contract changed t))
    (unless (eq (gethash "composition_eligible" record :missing)
                (if (string= artifact-class "rendered-draft") t nil))
      (setf (gethash "composition_eligible" record)
            (if (string= artifact-class "rendered-draft") t nil)
            changed t))
    (values record changed)))

(defun %candidate-lower-padded (content)
  (format nil " ~a "
          (string-downcase
           (string-trim '(#\Space #\Tab #\Newline #\Return) (or content "")))))

(defun %candidate-has-any (text needles)
  (some (lambda (needle) (search needle text :test #'char-equal)) needles))

(defun %candidate-addressed-opening-p (text)
  (some (lambda (prefix)
          (and (>= (length text) (length prefix))
               (string= prefix text :end2 (length prefix))))
        '("i " "i'm " "i’ve " "i've " "you " "your " "we "
          "hey " "remember " "i wanted " "i was wondering ")))

(defun candidate-send-readiness-lint (content)
  "Return deterministic diagnostics. This is never an authorization verdict."
  (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return)
                               (or content "")))
         (lower (string-downcase trimmed))
         (padded (%candidate-lower-padded trimmed))
         (first-or-second
           (%candidate-has-any
            padded '(" i " " i'm " " i've " " i’ve " " my " " me "
                     " you " " your " " we " " our ")))
         (third-person-operator
           (%candidate-has-any
            padded '(" the user " " operator is " " operator has " " operator said "
                     " operator proposed " " operator told ")))
         (question (find #\? trimmed))
         (violations nil))
    (when (< (length trimmed) 24) (push "content-too-short" violations))
    (when (> (length trimmed) *candidate-representation-max-content-characters*)
      (push "content-too-long" violations))
    (unless first-or-second (push "no-first-or-second-person" violations))
    (when third-person-operator (push "third-person-reference-to-operator" violations))
    (unless (or first-or-second question)
      (push "no-addressed-speech-act" violations)
      (push "bare-fact-or-narration" violations))
    (unless (%candidate-addressed-opening-p lower)
      (push "narrated-or-unaddressed-opening" violations))
    (setf violations (nreverse (remove-duplicates violations :test #'string=)))
    (obj "schema_version" 1
         "diagnostic_only" t
         "passed" (if (null violations) t nil)
         "violation_count" (length violations)
         "violations" (coerce violations 'vector))))

(defun candidate-reduced-e0-envelope (record &key contact-state)
  "Assemble an inspectable, non-authorizing reduced E0 rendering envelope."
  (let* ((artifact-class (candidate-artifact-class record))
         (evidence (and (hash-table-p record)
                        (gethash "evidence_node_ids" record))))
    (obj "schema_version" 1
         "candidate_id" (or (and (hash-table-p record)
                                   (gethash "id" record)) :null)
         "artifact_class" artifact-class
         "generation_contract" (candidate-generation-contract record)
         "source_type" (or (and (hash-table-p record)
                                  (gethash "source" record)) :null)
         "source_id" (or (and (hash-table-p record)
                                (gethash "source_id" record)) :null)
         "topic" (or (and (hash-table-p record)
                            (gethash "topic" record)) :null)
         "evidence_node_ids"
         (cond ((vectorp evidence) evidence)
               ((listp evidence) (coerce evidence 'vector))
               (t (vector)))
         "temporal_bounds"
         (obj "created_at" (or (and (hash-table-p record)
                                     (gethash "created_at" record)) :null)
              "updated_at" (or (and (hash-table-p record)
                                     (gethash "updated_at" record)) :null)
              "expires_at" (or (and (hash-table-p record)
                                     (gethash "expires_at" record)) :null))
         "contact_state" (or contact-state :unavailable)
         "permitted_claims" (vector "supported-by-listed-evidence")
         "prohibited_claims"
         (vector "unsupported-private-state" "invented-why-now"
                 "invented-activity" "unverified-temporal-claim")
         "rendering_substrate"
         (obj "required" "ordinary-public-reply-context"
              "identity_voice_context" "required"
              "relationship_context" "required"
              "current_conversation_context" "required"
              "abstain_available" t)
         "authorizes_publication" nil
         "authorizes_contact" nil)))

(defun candidate-representation-report (records)
  (let ((internal 0) (rendered 0) (unknown 0) (lint-pass 0) (total 0))
    (dolist (record (cond ((vectorp records) (coerce records 'list))
                          ((listp records) records) (t nil)))
      (incf total)
      (let ((class (candidate-artifact-class record)))
        (cond ((string= class "internal-stance") (incf internal))
              ((string= class "rendered-draft") (incf rendered))
              (t (incf unknown))))
      (let ((content (and (hash-table-p record) (gethash "content" record))))
        (when (and (stringp content)
                   (gethash "passed" (candidate-send-readiness-lint content)))
          (incf lint-pass))))
    (obj "schema_version" 1 "records" total
         "artifact_counts"
         (obj "internal_stance" internal "rendered_draft" rendered
              "unknown" unknown)
         "linter" (obj "diagnostic_only" t "passed" lint-pass
                       "failed" (- total lint-pass)))))
