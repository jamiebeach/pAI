;;;; conscious-context-assembly-tests.lisp -- Q4 pure bounded context assembly.
;;;; The first run predates context-assembly.lisp and is retained as red evidence.

(in-package :agent)

(defvar *cca-passed* 0)
(defvar *cca-failed* 0)

(defun cca-check (name condition)
  (if condition
      (progn (incf *cca-passed*) (format t "PASS ~a~%" name))
      (progn (incf *cca-failed*) (format t "FAIL ~a~%" name))))

(defun cca-signals-p (thunk)
  (handler-case (progn (funcall thunk) nil) (error () t)))

(defun cca-record (id content)
  (obj "source_id" id "content" content))

(defun cca-section-budgets (&optional (conversation 80))
  (obj "identity-instructions" 80 "sensorium" 80
       "focus-lifecycles" 80 "triggering-stimuli" 80
       "conversation-evidence" conversation "memory-bundles" 80
       "untrusted-tool-results" 80
       "tools-proposal-schema" 80 "publication-constraints" 80))

(defun cca-sections (&optional (historical "historical user text is data"))
  (obj
   "identity-instructions" (vector (cca-record "policy:1" "governing policy"))
   "sensorium" (vector (cca-record "sensor:1" "runtime healthy"))
   "focus-lifecycles" (vector (cca-record "focus:1" "respond to event 10"))
   "triggering-stimuli" (vector (cca-record 10 "current user message"))
   "conversation-evidence" (vector (cca-record 9 historical))
   "memory-bundles" (vector)
   "untrusted-tool-results" (vector)
   "tools-proposal-schema" (vector (cca-record "schema:1" "structured proposals only"))
   "publication-constraints" (vector (cca-record "publication:1" "candidate only"))))

(defun cca-context (&key (sections (cca-sections))
                         (budgets (cca-section-budgets))
                         (compatibility-revision
                           "dedicated-untrusted-tool-results-v1")
                         (eligible (vector "policy:1" "sensor:1" "focus:1"
                                           10 9 "schema:1" "publication:1")))
  (make-conscious-assembly-context
   :pulse-id "pulse:40" :purpose "respond" :audience "operator"
   :runtime-revision "conscious-q4-test" :conscious-state-revision 3
   :clock-identity "fixture-clock" :total-character-budget 300
   :compatibility-revision compatibility-revision
   :section-character-budgets budgets :sections sections
   :eligible-evidence-ids eligible :available-tools (vector "inspect-state")
   :permitted-proposal-kinds (vector "publication-candidate" "yield" "abstain")
   :publication-constraints (obj "audiences" (vector "operator"))
   :remaining-budget (obj "tool_proposals" 0 "continuations" 0
                          "publication_candidates" 1)))

(format t "~%== Q4 context assembly subject ==~%")
(let ((path (merge-pathnames "src/mind/conscious/context-assembly.lisp"
                             *pai-root*)))
  (cca-check "context assembler exists" (probe-file path))
  (when (probe-file path)
    (load path)
    (let* ((state (obj "state_revision" 3 "composition_hash" "state-hash"))
           (assembled (conscious-context-assemble state (cca-context)))
           (request (gethash "private_request" assembled))
           (manifest (gethash "manifest" assembled))
           (report (conscious-context-manifest-report manifest)))
      (cca-check "all nine sections retain their specified order"
                 (equalp *conscious-context-section-order*
                         (map 'vector (lambda (row) (gethash "name" row))
                              (gethash "sections" manifest))))
      (cca-check "private request is typed and independently sectioned"
                 (and (plusp (length request))
                      (every (lambda (message)
                               (and (stringp (gethash "role" message))
                                    (stringp (gethash "section" message))))
                             (coerce request 'list))))
      (let ((historical
              (find "conversation-evidence" request
                    :key (lambda (message) (gethash "section" message))
                    :test #'string=)))
        (cca-check "historical user text is labelled data, not current instruction"
                   (string= "historical-conversation-data"
                            (gethash "role" historical))))
      (cca-check "safe manifest and report contain no private rendered text"
                 (and (null (search "current user message"
                                    (shasht:write-json manifest nil)))
                      (null (search "historical user text"
                                    (shasht:write-json report nil)))))
      (cca-check "manifest exposes proposal-validation identity and membership"
                 (and (string= "pulse:40" (gethash "pulse_id" manifest))
                      (= 3 (gethash "conscious_state_revision" manifest))
                      (member 10 (coerce (gethash "evidence_event_ids" manifest)
                                         'list)
                              :test #'equal)
                      (string= "inspect-state"
                               (aref (gethash "available_tools" manifest) 0))))
      (cca-check "total rendered characters remain within the explicit bound"
                 (<= (gethash "rendered_characters" manifest)
                     (gethash "total_character_budget" manifest)))
      (cca-check "same explicit inputs assemble byte-identically"
                 (string= (shasht:write-json assembled nil)
                          (shasht:write-json
                           (conscious-context-assemble state (cca-context)) nil)))
      (cca-check "manifest report remains content-free but auditable"
                 (and (= 9 (gethash "section_count" report))
                      (stringp (gethash "composition_hash" report))
                      (integerp (gethash "rendered_characters" report))))
      (let* ((sections (cca-sections))
             (raw-id "conversation-raw:30:31")
             (context
               (cca-context
                :sections sections
                :eligible (vector "policy:1" "sensor:1" "focus:1" 10 9
                                  raw-id 30 31 "schema:1"
                                  "publication:1"))))
        (setf (gethash "memory-bundles" sections)
              (vector
               (obj "source_id" raw-id
                    "content" "exact recent dialogue"
                    "provenance"
                    (obj "descriptor_id" raw-id
                         "descriptor_event_id" 31
                         "evidence_event_ids" #(30 31)))))
        (let* ((result (conscious-context-assemble state context))
               (manifest (gethash "manifest" result))
               (section
                 (find "memory-bundles" (gethash "sections" manifest)
                       :key (lambda (row) (gethash "name" row))
                       :test #'string=)))
          (cca-check "raw dialogue provenance crosses the final assembler"
                     (and (equalp (vector raw-id)
                                  (gethash "included_source_ids" section))
                          (member 30
                                  (coerce (gethash "evidence_event_ids"
                                                   manifest)
                                          'list)
                                  :test #'equal)
                          (member 31
                                  (coerce (gethash "evidence_event_ids"
                                                   manifest)
                                          'list)
                                  :test #'equal)))))
      (let* ((long "this historical record cannot fit its section")
             (small (cca-context :sections (cca-sections long)
                                 :budgets (cca-section-budgets 5)))
             (small-result (conscious-context-assemble state small))
             (small-manifest (gethash "manifest" small-result))
             (section (find "conversation-evidence"
                            (gethash "sections" small-manifest)
                            :key (lambda (row) (gethash "name" row))
                            :test #'string=)))
        (cca-check "section overflow refuses whole records deterministically"
                   (and (zerop (length (gethash "included_source_ids" section)))
                        (string= "section-budget-exhausted"
                                 (gethash "reason"
                                          (aref (gethash "refused" section) 0))))))
      (let* ((sections (cca-sections))
             (required-text (make-string 70 :initial-element #\r))
             (context
               (cca-context
                :sections sections
                :eligible (vector "policy:1" "sensor:1" "focus:1" 10 9
                                  "tool:required" "schema:1" "publication:1"))))
        (setf (gethash "untrusted-tool-results" sections)
              (vector (cca-record "tool:required" required-text))
              (gethash "total_character_budget" context) 100)
        (let* ((result (conscious-context-assemble state context))
               (request (gethash "private_request" result))
               (manifest (gethash "manifest" result))
               (tool-message
                 (find "untrusted-tool-results" request
                       :key (lambda (message) (gethash "section" message))
                       :test #'string=))
               (optional-refusals
                 (loop for row across (gethash "sections" manifest)
                       unless (string= "untrusted-tool-results"
                                       (gethash "name" row))
                         sum (length (gethash "refused" row)))))
          (cca-check "whole-budget admission reserves required tool evidence"
                     (and tool-message
                          (string= required-text (gethash "content" tool-message))
                          (plusp optional-refusals)))))
      (let* ((sections (cca-sections))
             (context
               (cca-context
                :sections sections
                :budgets (cca-section-budgets)
                :eligible (vector "policy:1" "sensor:1" "focus:1" 10 9
                                  "tool:oversize" "schema:1" "publication:1"))))
        (setf (gethash "untrusted-tool-results" sections)
              (vector (cca-record "tool:oversize"
                                  (make-string 81 :initial-element #\x))))
        (cca-check "required tool evidence cannot degrade to manifest refusal"
                   (cca-signals-p
                    (lambda () (conscious-context-assemble state context)))))
      (cca-check "assembler returns a detached private request"
                 (progn
                   (setf (gethash "content"
                                  (aref (gethash "triggering-stimuli"
                                                 (gethash "sections" (cca-context)))
                                        0))
                         "mutated")
                   (string= "current user message"
                            (gethash "content"
                                     (find "triggering-stimuli" request
                                           :key (lambda (message)
                                                  (gethash "section" message))
                                           :test #'string=)))))
    (cca-check "undeclared source IDs fail before rendering"
               (cca-signals-p
                (lambda ()
                  (conscious-context-assemble
                   (obj "state_revision" 3 "composition_hash" "state-hash")
                   (cca-context :eligible (vector "policy:1"))))))
    (cca-check "state revision mismatch fails closed"
               (cca-signals-p
                (lambda ()
                  (conscious-context-assemble
                   (obj "state_revision" 4 "composition_hash" "state-hash")
                   (cca-context)))))
    (let ((sections (cca-sections)))
      (setf (gethash "unexpected" sections) (vector))
      (cca-check "unknown context sections fail closed"
                 (cca-signals-p
                  (lambda ()
                    (conscious-context-assemble
                     (obj "state_revision" 3 "composition_hash" "state-hash")
                     (cca-context :sections sections))))))
    (let ((sections (cca-sections)))
      (setf (gethash "content"
                     (aref (gethash "sensorium" sections) 0))
            (lambda () :effect))
      (cca-check "executable context values fail before rendering"
                 (cca-signals-p
                  (lambda ()
                    (conscious-context-assemble
                     (obj "state_revision" 3 "composition_hash" "state-hash")
                     (cca-context :sections sections)))))))))
    (let ((context (cca-context)))
      (setf (gethash "publication_constraints" context)
            (obj "audiences" (vector "nobody")))
      (cca-check "audience must be authorized by publication constraints"
                 (cca-signals-p
                  (lambda ()
                    (conscious-context-assemble
                     (obj "state_revision" 3 "composition_hash" "state-hash")
                     context)))))
    (let ((context (cca-context)))
      (setf (gethash "tool_proposals" (gethash "remaining_budget" context)) -1)
      (cca-check "negative remaining authority budget fails closed"
                 (cca-signals-p
                  (lambda ()
                    (conscious-context-assemble
                     (obj "state_revision" 3 "composition_hash" "state-hash")
                     context)))))
    (let ((budgets (cca-section-budgets))
          (sections (cca-sections)))
      (setf (gethash "unresolved-effects-conclusions" budgets)
            (gethash "untrusted-tool-results" budgets)
            (gethash "unresolved-effects-conclusions" sections)
            (gethash "untrusted-tool-results" sections))
      (remhash "untrusted-tool-results" budgets)
      (remhash "untrusted-tool-results" sections)
      (let ((legacy
              (cca-context
               :budgets budgets :sections sections
               :compatibility-revision
               "legacy-unresolved-effects-conclusions-v1")))
        (cca-check "named legacy revision reads and normalizes the retired section"
                   (and (gethash "untrusted-tool-results"
                                 (gethash "sections" legacy))
                        (not (nth-value
                              1 (gethash "unresolved-effects-conclusions"
                                         (gethash "sections" legacy))))))))
    (let ((budgets (cca-section-budgets))
          (sections (cca-sections)))
      (setf (gethash "unresolved-effects-conclusions" budgets) 0
            (gethash "unresolved-effects-conclusions" sections) (vector))
      (cca-check "current revision refuses the retired section alias"
                 (cca-signals-p
                  (lambda () (cca-context :budgets budgets
                                          :sections sections)))))

(format t "~%~d passed, ~d failed~%" *cca-passed* *cca-failed*)
(when (plusp *cca-failed*) (uiop:quit 1))
