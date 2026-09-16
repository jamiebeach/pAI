;;;; memory-atom-candidate.lisp -- N1/N2 non-admitting atom contract.
;;;;
;;;; Source/Lab only. This file has no provider, queue, admission, persistence,
;;;; public-rendering, transport, tick, or delivery adapter.

(in-package :agent)

(ql:quickload '(:ironclad :babel) :silent t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :pai.mind.memory)
    (load (merge-pathnames "mind-memory-core.lisp"
                           (or *load-truename* *compile-file-truename*
                               *default-pathname-defaults*)))))

(export '(memory-atom-build-manifest memory-atom-build-request
          memory-atom-validate-response memory-atom-candidate-report))

(defparameter *memory-atom-schema-version* 1)
(defparameter *memory-atom-contract-version* "n1-n2-atom-identity-v1")
(defparameter *memory-atom-prompt-version* "n1-decomposer-v1")
(defparameter *memory-atom-max-evidence* 12)
(defparameter *memory-atom-max-atoms* 12)
(defparameter *memory-atom-max-evidence-chars* 16000)
(defparameter *memory-atom-max-value-chars* 600)
(defparameter *memory-atom-max-annotation-chars* 300)
(defparameter *memory-atom-max-qualifiers* 5)
(defparameter *memory-atom-max-roots* 4)
(defparameter *memory-atom-forms* '("episodic" "semantic" "procedural"))
(defparameter *memory-atom-roles* '("user" "assistant" "tool"))
(defparameter *memory-atom-disclosure-candidates*
  '("private" "personal-shareable" "public"))

(defun %memory-atom-list (value)
  (cond ((null value) nil)
        ((vectorp value) (coerce value 'list))
        ((listp value) value)
        (t (error "Memory atom array field is not an array."))))

(defun %memory-atom-nonempty-string-p (value &optional maximum)
  (and (stringp value)
       (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                   value)))
       (or (null maximum) (<= (length value) maximum))))

(defun %memory-atom-exact-keys (table allowed label)
  (unless (hash-table-p table) (error "~a is not an object." label))
  (loop for key being the hash-keys of table
        unless (member key allowed :test #'string=)
          do (error "Unknown ~a key ~a." label key))
  (dolist (key allowed)
    (unless (nth-value 1 (gethash key table))
      (error "Missing required ~a key ~a." label key)))
  table)

(defun %memory-atom-safe-id-p (value &key (colon t))
  (and (%memory-atom-nonempty-string-p value 160)
       (every (lambda (character)
                (or (alphanumericp character)
                    (member character
                            (if colon '(#\. #\_ #\- #\:)
                                '(#\. #\_ #\-))
                            :test #'char=)))
              value)))

(defun %memory-atom-leap-year-p (year)
  (and (zerop (mod year 4))
       (or (not (zerop (mod year 100))) (zerop (mod year 400)))))

(defun %memory-atom-days-in-month (year month)
  (case month
    ((1 3 5 7 8 10 12) 31)
    ((4 6 9 11) 30)
    (2 (if (%memory-atom-leap-year-p year) 29 28))
    (otherwise 0)))

(defun %memory-atom-utc-timestamp-p (value)
  "Accept real, canonical, second-resolution UTC timestamps only."
  (and (stringp value)
       (= (length value) 20)
       (char= (char value 4) #\-)
       (char= (char value 7) #\-)
       (char= (char value 10) #\T)
       (char= (char value 13) #\:)
       (char= (char value 16) #\:)
       (char= (char value 19) #\Z)
       (every #'digit-char-p
              (loop for index in '(0 1 2 3 5 6 8 9 11 12 14 15 17 18)
                    collect (char value index)))
       (let ((year (parse-integer value :start 0 :end 4))
             (month (parse-integer value :start 5 :end 7))
             (day (parse-integer value :start 8 :end 10))
             (hour (parse-integer value :start 11 :end 13))
             (minute (parse-integer value :start 14 :end 16))
             (second (parse-integer value :start 17 :end 19)))
         (and (<= 1970 year 9999)
              (<= 1 month 12)
              (<= 1 day (%memory-atom-days-in-month year month))
              (<= 0 hour 23) (<= 0 minute 59) (<= 0 second 59)))))

(defun %memory-atom-nullable-timestamp (value label)
  (cond ((or (null value) (eq value :null)) :null)
        ((%memory-atom-utc-timestamp-p value) value)
        (t (error "~a must be null or a canonical UTC timestamp." label))))

(defun %memory-atom-normalize-text (value)
  (string-downcase
   (with-output-to-string (stream)
     (let ((space-p nil))
       (loop for character across
             (string-trim '(#\Space #\Tab #\Newline #\Return) value)
             do (if (member character '(#\Space #\Tab #\Newline #\Return))
                    (setf space-p t)
                    (progn
                      (when space-p (write-char #\Space stream))
                      (setf space-p nil)
                      (write-char character stream))))))))

(defun %memory-atom-canonical-field (value)
  (let ((text (if (eq value :null) "<null>" (format nil "~a" value))))
    (format nil "~d:~a" (length text) text)))

(defun %memory-atom-sha256 (&rest fields)
  (let* ((canonical
           (format nil "~{~a~^|~}" (mapcar #'%memory-atom-canonical-field fields)))
         (octets (babel:string-to-octets canonical :encoding :utf-8)))
    (string-downcase
     (ironclad:byte-array-to-hex-string
      (ironclad:digest-sequence :sha256 octets)))))

(defun %memory-atom-validate-evidence (evidence)
  (%memory-atom-exact-keys
   evidence '("id" "role" "sequence" "observed_at" "content") "evidence")
  (let ((id (gethash "id" evidence))
        (role (gethash "role" evidence))
        (sequence (gethash "sequence" evidence))
        (observed-at (gethash "observed_at" evidence))
        (content (gethash "content" evidence)))
    (unless (%memory-atom-safe-id-p id)
      (error "Evidence id is invalid."))
    (unless (member role *memory-atom-roles* :test #'string=)
      (error "Evidence role is invalid."))
    (unless (and (integerp sequence) (<= 0 sequence 9999))
      (error "Evidence sequence is invalid."))
    (unless (%memory-atom-utc-timestamp-p observed-at)
      (error "Evidence observed_at is not canonical UTC."))
    (unless (%memory-atom-nonempty-string-p
             content *memory-atom-max-evidence-chars*)
      (error "Evidence content is empty or too large."))
    evidence))

(defun %memory-atom-manifest-map (manifest)
  (let ((table (make-hash-table :test #'equal)))
    (dolist (row (%memory-atom-list (gethash "evidence" manifest)))
      (setf (gethash (gethash "id" row) table) row))
    table))

(defun %memory-atom-string-array (value maximum label &key safe-ids)
  (let ((items (%memory-atom-list value)))
    (unless (<= (length items) maximum)
      (error "~a exceeds its bound." label))
    (dolist (item items)
      (unless (if safe-ids
                  (%memory-atom-safe-id-p item)
                  (%memory-atom-nonempty-string-p
                   item *memory-atom-max-annotation-chars*))
        (error "~a contains an invalid string." label)))
    (unless (= (length items) (length (remove-duplicates items :test #'string=)))
      (error "~a contains duplicates." label))
    items))

(defun %memory-atom-normalize-qualifiers (value)
  (let ((items (%memory-atom-list value)))
    (when (> (length items) *memory-atom-max-qualifiers*)
      (error "Atom qualifier count exceeds its bound."))
    (let ((normalized
            (mapcar
             (lambda (item)
               (%memory-atom-exact-keys item '("name" "value") "qualifier")
               (let ((name (gethash "name" item))
                     (qualifier-value (gethash "value" item)))
                 (unless (and (%memory-atom-safe-id-p name :colon nil)
                              (%memory-atom-nonempty-string-p
                               qualifier-value *memory-atom-max-value-chars*))
                   (error "Atom qualifier is invalid."))
                 (obj "name" (string-downcase name)
                      "value" qualifier-value)))
             items)))
      (setf normalized
            (sort normalized #'string<
                  :key (lambda (item)
                         (format nil "~a=~a" (gethash "name" item)
                                 (%memory-atom-normalize-text
                                  (gethash "value" item))))))
      (let ((names (mapcar (lambda (item) (gethash "name" item)) normalized)))
        (unless (= (length names)
                   (length (remove-duplicates names :test #'string=)))
          (error "Atom qualifier names must be unique.")))
      normalized)))

(defun %memory-atom-attribution-valid-p (subject evidence-rows)
  (cond ((string= subject *operator-id*)
         (find "user" evidence-rows :test #'string=
               :key (lambda (row) (gethash "role" row))))
        ((string= subject *agent-id*)
         (find "assistant" evidence-rows :test #'string=
               :key (lambda (row) (gethash "role" row))))
        (t (find-if (lambda (row)
                      (member (gethash "role" row) '("user" "tool")
                              :test #'string=))
                    evidence-rows))))

(defun %memory-atom-qualifier-canonical-text (qualifiers)
  (format nil "~{~a~^;~}"
          (mapcar (lambda (item)
                    (format nil "~a=~a" (gethash "name" item)
                            (%memory-atom-normalize-text
                             (gethash "value" item))))
                  qualifiers)))

(defun %memory-atom-normalize-one (atom manifest evidence-map)
  (%memory-atom-exact-keys
   atom '("memory_form" "subject" "predicate" "value" "polarity"
          "qualifiers" "observed_at" "valid_from" "valid_to"
          "disclosure_candidate" "evidence_ids") "atom")
  (let* ((form (gethash "memory_form" atom))
         (subject (gethash "subject" atom))
         (predicate (gethash "predicate" atom))
         (value (gethash "value" atom))
         (polarity (gethash "polarity" atom))
         (disclosure (gethash "disclosure_candidate" atom))
         (roots (%memory-atom-string-array
                 (gethash "evidence_ids" atom) *memory-atom-max-roots*
                 "atom evidence_ids" :safe-ids t))
         (evidence-rows (mapcar (lambda (id) (gethash id evidence-map)) roots))
         (qualifiers (%memory-atom-normalize-qualifiers
                      (gethash "qualifiers" atom)))
         (observed-at (gethash "observed_at" atom))
         (valid-from (%memory-atom-nullable-timestamp
                      (gethash "valid_from" atom) "valid_from"))
         (valid-to (%memory-atom-nullable-timestamp
                    (gethash "valid_to" atom) "valid_to")))
    (unless (member form *memory-atom-forms* :test #'string=)
      (error "Atom memory_form is invalid."))
    (unless (and (%memory-atom-safe-id-p subject)
                 (string= subject (string-downcase subject)))
      (error "Atom subject is invalid or not normalized."))
    (unless (and (%memory-atom-safe-id-p predicate :colon nil)
                 (string= predicate (string-downcase predicate)))
      (error "Atom predicate is invalid or not normalized."))
    (unless (%memory-atom-nonempty-string-p value *memory-atom-max-value-chars*)
      (error "Atom value is empty or too large."))
    (unless (member polarity '("affirmed" "negated") :test #'string=)
      (error "Atom polarity is invalid."))
    (unless (member disclosure *memory-atom-disclosure-candidates* :test #'string=)
      (error "Atom disclosure candidate is invalid."))
    (unless (and roots (every #'identity evidence-rows))
      (error "Atom cites an unknown evidence id."))
    (unless (%memory-atom-attribution-valid-p subject evidence-rows)
      (error "Atom attribution is not supported by an eligible evidence role."))
    (unless (and (%memory-atom-utc-timestamp-p observed-at)
                 (find observed-at evidence-rows :test #'string=
                       :key (lambda (row) (gethash "observed_at" row))))
      (error "Atom observed_at must copy a cited evidence timestamp."))
    (when (and (not (eq valid-from :null)) (not (eq valid-to :null))
               (not (string< valid-from valid-to)))
      (error "Atom valid_to must be later than valid_from."))
    (let* ((claim-key
             (%memory-atom-sha256
              (gethash "agent_id" manifest) form subject predicate
              (%memory-atom-normalize-text value) polarity
              (%memory-atom-qualifier-canonical-text qualifiers)
              valid-from valid-to))
           (sorted-roots (sort (copy-list roots) #'string<))
           (idempotency-key
             (%memory-atom-sha256
              *memory-atom-contract-version* *memory-atom-prompt-version*
              (gethash "turn_id" manifest) claim-key
              (format nil "~{~a~^,~}" sorted-roots))))
      (obj "candidate_id" (format nil "atom-candidate:~a"
                                   (subseq idempotency-key 0 32))
           "claim_key" claim-key "idempotency_key" idempotency-key
           "agent_id" *agent-id* "memory_form" form "subject" subject
           "predicate" predicate "value" value "polarity" polarity
           "qualifiers" (coerce qualifiers 'vector)
           "observed_at" observed-at "valid_from" valid-from
           "valid_to" valid-to "evidence_ids" (coerce roots 'vector)
           "disclosure_candidate" disclosure
           ;; A model candidate never promotes disclosure. N5 may later queue
           ;; an explicit review while the durable class remains private.
           "persistence_projection"
           (obj "memory_form" form "disclosure_class" "private"
                "share_review_status"
                (if (string= disclosure "private") :null "pending"))))))

(defun %memory-atom-normalize-exclusion (item evidence-map)
  (%memory-atom-exact-keys item '("evidence_ids" "reason") "exclusion")
  (let ((roots (%memory-atom-string-array
                (gethash "evidence_ids" item) *memory-atom-max-roots*
                "exclusion evidence_ids" :safe-ids t))
        (reason (gethash "reason" item)))
    (unless (and roots (every (lambda (id) (gethash id evidence-map)) roots))
      (error "Exclusion cites an unknown evidence id."))
    (unless (%memory-atom-nonempty-string-p
             reason *memory-atom-max-annotation-chars*)
      (error "Exclusion reason is invalid."))
    (obj "evidence_ids" (coerce roots 'vector) "reason" reason)))

(defun memory-atom-build-manifest (turn-id captured-at evidence)
  (pai.mind.memory:build-atom-manifest turn-id captured-at evidence))

(defun memory-atom-build-request (manifest)
  (pai.mind.memory:build-atom-request manifest))

(defun memory-atom-validate-response (response manifest)
  (pai.mind.memory:validate-atom-response response manifest))

(defun memory-atom-candidate-report ()
  (pai.mind.memory:capability-report))
