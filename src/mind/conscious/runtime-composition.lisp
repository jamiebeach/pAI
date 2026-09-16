;;;; runtime-composition.lisp -- pure compilation and durable retention of
;;;; one non-secret conscious work runtime contract.

(in-package :agent)

(export '(conscious-runtime-plan-compile conscious-runtime-plan-retain
          conscious-runtime-plan-resolve conscious-runtime-plan-hash
          conscious-runtime-plan-required-tool-characters))

(defparameter *conscious-runtime-plan-schema-version* 1)
(defparameter *conscious-runtime-plan-event-type*
  "conscious-runtime-plan-registered")
(defparameter *conscious-runtime-plan-compatibility-revision*
  "dedicated-untrusted-tool-results-v1")

(defun %conscious-runtime-plan-hash-text (text)
  (string-downcase
   (ironclad:byte-array-to-hex-string
    (ironclad:digest-sequence
     :sha256 (sb-ext:string-to-octets text :external-format :utf-8)))))

(defun %conscious-runtime-plan-copy (value)
  (%conscious-work-copy value))

(defun %conscious-runtime-plan-bounded-text-p (value &optional (maximum 256))
  (and (stringp value) (plusp (length value)) (<= (length value) maximum)))

(defun %conscious-runtime-plan-component (component label)
  (unless (hash-table-p component)
    (error "Runtime plan ~a component must be an object" label))
  (unless (and (%conscious-runtime-plan-bounded-text-p
                (gethash "profile_id" component) 128)
               (integerp (gethash "revision" component))
               (not (minusp (gethash "revision" component))))
    (error "Runtime plan ~a component has no public revision" label))
  component)

(defun conscious-runtime-plan-required-tool-characters
    (context-profile work-profile)
  (let ((result-characters (gethash "max_tool_result_characters" work-profile))
        (operations (gethash "max_tool_operations" work-profile))
        (wrapper
          (gethash "tool_result_wrapper_characters_per_record"
                   context-profile)))
    (unless (and (integerp result-characters) (not (minusp result-characters))
                 (integerp operations) (not (minusp operations))
                 (integerp wrapper) (not (minusp wrapper)))
      (error "Runtime plan tool-result bounds are invalid"))
    (+ result-characters (* operations wrapper))))

(defun %conscious-runtime-plan-tool-consumers (capabilities work-profile)
  (let ((tools (coerce (gethash "permitted_tools" work-profile) 'list))
        (consumers (and (hash-table-p capabilities)
                        (gethash "tool_consumers" capabilities))))
    (unless (hash-table-p consumers)
      (error "Runtime plan has no tool consumer manifest"))
    (dolist (tool tools)
      (let ((entry (gethash tool consumers)))
        (unless (and (hash-table-p entry)
                     (%conscious-runtime-plan-bounded-text-p
                      (gethash "consumer" entry) 128)
                     (%conscious-runtime-plan-bounded-text-p
                      (gethash "authority_class" entry) 128)
                     (integerp (gethash "max_result_characters" entry))
                     (>= (gethash "max_result_characters" entry)
                         (gethash "max_tool_result_characters" work-profile)))
          (error "Advertised tool ~a has no compatible unique consumer" tool))))
    (loop for tool being the hash-keys of consumers
          unless (member tool tools :test #'string=)
            do (error "Tool consumer ~a is not permitted by the work profile" tool))))

(defun %conscious-runtime-plan-proposal-consumers (capabilities work-profile)
  (let ((kinds (coerce (gethash "permitted_proposal_kinds" work-profile) 'list))
        (consumers (and (hash-table-p capabilities)
                        (gethash "proposal_consumers" capabilities))))
    (unless (hash-table-p consumers)
      (error "Runtime plan has no proposal consumer manifest"))
    (dolist (kind kinds)
      (let ((entries (gethash kind consumers)))
        (unless (and (vectorp entries) (= 1 (length entries))
                     (%conscious-runtime-plan-bounded-text-p (aref entries 0) 128))
          (error "Proposal kind ~a must have exactly one consumer" kind))))))

(defun conscious-runtime-plan-compile
    (context-profile work-profile capabilities provider publication transport)
  "Compile one immutable, non-secret runtime plan or refuse incompatibility."
  (%conscious-runtime-plan-component context-profile "context")
  (conscious-work-profile-validate work-profile)
  (dolist (entry (list (cons provider "provider")
                       (cons publication "publication")
                       (cons transport "transport")))
    (%conscious-runtime-plan-component (car entry) (cdr entry)))
  (%conscious-runtime-plan-tool-consumers capabilities work-profile)
  (%conscious-runtime-plan-proposal-consumers capabilities work-profile)
  (let* ((sections (gethash "section_character_budgets" context-profile))
         (required
           (conscious-runtime-plan-required-tool-characters
            context-profile work-profile))
         (tool-section
           (and (hash-table-p sections)
                (gethash "untrusted-tool-results" sections)))
         (total (gethash "total_character_budget" context-profile))
         (provider-requests (gethash "max_requests" provider))
         (provider-input (gethash "max_input_characters" provider))
         (channel (gethash "channel" transport))
         (publication-channels (gethash "channels" publication)))
    (unless (and (integerp tool-section) (>= tool-section required)
                 (integerp total) (>= total required))
      (error "Runtime composition cannot reserve bounded required tool evidence"))
    (unless (and (integerp provider-requests)
                 (>= provider-requests (gethash "max_model_calls" work-profile))
                 (integerp provider-input) (>= provider-input total))
      (error "Provider bounds are incompatible with the work/context lease"))
    (unless (and (%conscious-runtime-plan-bounded-text-p channel 128)
                 (vectorp publication-channels)
                 (find channel publication-channels :test #'string=))
      (error "Publication and transport channels are incompatible"))
    (let* ((body
             (obj "schema_version" *conscious-runtime-plan-schema-version*
                  "compatibility_revision"
                  *conscious-runtime-plan-compatibility-revision*
                  "context_profile" (%conscious-runtime-plan-copy context-profile)
                  "work_profile" (%conscious-runtime-plan-copy work-profile)
                  "capabilities" (%conscious-runtime-plan-copy capabilities)
                  "provider" (%conscious-runtime-plan-copy provider)
                  "publication" (%conscious-runtime-plan-copy publication)
                  "transport" (%conscious-runtime-plan-copy transport)
                  "required_tool_characters" required))
           (canonical (%conscious-work-canonical-json body))
           (hash (%conscious-runtime-plan-hash-text canonical)))
      (obj "schema_version" *conscious-runtime-plan-schema-version*
           "plan_hash" hash "canonical_plan" body))))

(defun conscious-runtime-plan-hash (plan)
  (unless (hash-table-p plan) (error "Runtime plan must be an object"))
  (let* ((body (gethash "canonical_plan" plan))
         (stored (gethash "plan_hash" plan))
         (actual
           (and (hash-table-p body)
                (%conscious-runtime-plan-hash-text
                 (%conscious-work-canonical-json body)))))
    (unless (and (%conscious-runtime-plan-bounded-text-p stored 128)
                 (string= stored actual))
      (error "Runtime plan hash does not match its canonical bytes"))
    stored))

(defun %conscious-runtime-plan-events ()
  (unless (fboundp 'replay-events)
    (error "Runtime plan retention requires durable event replay"))
  (funcall 'replay-events :types (list *conscious-runtime-plan-event-type*)))

(defun %conscious-runtime-plan-registration (hash)
  "Read zero or one exact registration; conflicting durable identity fails closed."
  (let ((matches
          (remove-if-not
           (lambda (event)
             (let ((payload (and (hash-table-p event)
                                 (gethash "payload" event))))
               (and (hash-table-p payload)
                    (equal *conscious-runtime-plan-event-type* (gethash "type" event))
                    (string= hash (gethash "plan_hash" payload "")))))
           (%conscious-runtime-plan-events))))
    (when (> (length matches) 1)
      (error "Runtime plan identity has duplicate registrations"))
    (when matches
      (let* ((event (first matches))
             (id (gethash "id" event))
             (payload (gethash "payload" event)))
        (unless (and (integerp id) (plusp id)
                     (eql 1 (gethash "schema_version" payload))
                     (string= hash (conscious-runtime-plan-hash (gethash "plan" payload))))
          (error "Retained runtime plan failed identity verification"))
        event))))

(defun conscious-runtime-plan-retain (plan)
  "Retain or join one exact plan under the shared conditional append authority."
  (unless (fboundp 'log-event-if)
    (error "Runtime plan retention requires conditional durable event append"))
  (let* ((snapshot (%conscious-runtime-plan-copy plan))
         (hash (conscious-runtime-plan-hash snapshot))
         (payload (obj "schema_version" 1 "plan_hash" hash "plan" snapshot))
         (existing nil))
    (multiple-value-bind (id durable receipt accepted)
        (funcall 'log-event-if
                 (lambda ()
                   (setf existing (%conscious-runtime-plan-registration hash))
                   (null existing))
                 *conscious-runtime-plan-event-type* payload)
      (when accepted
        (unless (and (integerp id) (plusp id) durable (hash-table-p receipt)
                     (eql id (gethash "id" receipt))
                     (equal *conscious-runtime-plan-event-type* (gethash "type" receipt))
                     (equal (%conscious-work-canonical-json payload)
                            (%conscious-work-canonical-json (gethash "payload" receipt))))
          (error "Runtime plan has no exact durable retention receipt")))
      (let* ((registered (%conscious-runtime-plan-registration hash))
             (expected-id (if accepted id (and existing (gethash "id" existing)))))
        (unless (and registered expected-id (eql expected-id (gethash "id" registered)))
          (error "Runtime plan retention is not durably readable"))
        (obj "schema_version" 1 "status" (if accepted "registered" "retained")
             "plan_hash" hash "event_id" expected-id)))))

(defun conscious-runtime-plan-resolve (hash)
  "Resolve and verify one exact retained plan by content hash."
  (let ((event (%conscious-runtime-plan-registration hash)))
    (unless event (error "Runtime plan identity does not resolve exactly once"))
    (%conscious-runtime-plan-copy (gethash "plan" (gethash "payload" event)))))
