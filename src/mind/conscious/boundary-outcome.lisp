;;;; boundary-outcome.lisp -- closed knowledge-bearing operation/pulse outcomes.

(in-package :agent)

(export '(conscious-boundary-outcome-make
          conscious-boundary-outcome-validate
          conscious-boundary-outcome-kind
          conscious-boundary-outcome-work-transition))

(defparameter *conscious-boundary-outcome-schema-version* 1)
(defparameter *conscious-boundary-outcome-kinds*
  '("succeeded" "failed-before-claim" "failed-after-claim"
    "outcome-unknown" "cancelled"))

(defun conscious-boundary-outcome-make
    (kind &key claim-id terminal-event-id reason-code)
  (conscious-boundary-outcome-validate
   (obj "schema_version" *conscious-boundary-outcome-schema-version*
        "kind" kind
        "claim_id" (or claim-id :null)
        "terminal_event_id" (or terminal-event-id :null)
        "reason_code" (or reason-code :null))))

(defun conscious-boundary-outcome-validate (outcome)
  (unless (hash-table-p outcome)
    (error "Boundary outcome must be an object"))
  (let ((keys '("schema_version" "kind" "claim_id"
                "terminal_event_id" "reason_code")))
    (loop for key being the hash-keys of outcome
          unless (member key keys :test #'string=)
            do (error "Unknown boundary outcome key ~s" key))
    (dolist (key keys)
      (unless (nth-value 1 (gethash key outcome))
        (error "Boundary outcome is missing ~s" key))))
  (unless (and (= *conscious-boundary-outcome-schema-version*
                  (gethash "schema_version" outcome -1))
               (member (gethash "kind" outcome)
                       *conscious-boundary-outcome-kinds* :test #'string=))
    (error "Boundary outcome kind is invalid"))
  (dolist (key '("claim_id" "reason_code"))
    (let ((value (gethash key outcome)))
      (unless (or (eq value :null)
                  (and (stringp value) (plusp (length value))
                       (<= (length value) 256)))
        (error "Boundary outcome ~a is invalid" key))))
  (let ((event-id (gethash "terminal_event_id" outcome)))
    (unless (or (eq event-id :null)
                (and (integerp event-id) (plusp event-id)))
      (error "Boundary outcome terminal event ID is invalid")))
  (let ((kind (gethash "kind" outcome))
        (claim (gethash "claim_id" outcome))
        (terminal (gethash "terminal_event_id" outcome)))
    (cond
      ((string= kind "failed-before-claim")
       (unless (eq claim :null)
         (error "Pre-claim failure cannot cite a claim")))
      ((member kind '("succeeded" "failed-after-claim") :test #'string=)
       (unless (and (stringp claim) (integerp terminal))
         (error "Known post-claim outcome requires claim and terminal receipt")))
      ((string= kind "outcome-unknown")
       (unless (and (stringp claim) (eq terminal :null))
         (error "Unknown outcome requires claim without terminal")))))
  outcome)

(defun conscious-boundary-outcome-kind (outcome)
  (gethash "kind" (conscious-boundary-outcome-validate outcome)))

(defun conscious-boundary-outcome-work-transition (outcome)
  "Map a boundary outcome to an existing explicit work transition or NIL."
  (let ((kind (conscious-boundary-outcome-kind outcome)))
    (cond ((string= kind "succeeded") nil)
          ((member kind '("failed-before-claim" "failed-after-claim")
                   :test #'string=)
           "failed")
          ((string= kind "outcome-unknown") "outcome-unknown")
          ((string= kind "cancelled") "suspended"))))
