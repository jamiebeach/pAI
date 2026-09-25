;;;; recall-selection.lisp -- pure bounded evidence fusion for public recall.

(in-package :agent)

(export '(build-recall-query-plan
          recall-selection-candidate
          recall-selection-select
          recall-selection-report))

(defparameter *personal-recall-selection-revision*
  "personal-recall-selection-v2")

(defparameter *personal-recall-query-character-budget* 8000)

(defun %recall-selection-bounded-stimulus (stimulus)
  "Keep the current/root stimulus at the tail of an assembled recursive prompt."
  (let ((length (length stimulus)))
    (if (<= length *personal-recall-query-character-budget*)
        stimulus
        (subseq stimulus
                (- length *personal-recall-query-character-budget*)))))

(defparameter *personal-recall-stop-terms*
  '("a" "an" "and" "are" "did" "do" "earlier" "for" "have" "i"
    "in" "is" "it" "me" "my" "of" "our" "please" "the" "this"
    "to" "was" "were" "what" "when" "who" "with" "year"))

(defparameter *personal-recall-category-policy*
  '(("spouse" "partner" "wife" "husband" "married")
    ("child" "children" "kid" "kids" "son" "sons" "daughter" "daughters")
    ("pet" "pets" "animal" "animals" "dog" "dogs" "cat" "cats")
    ("health" "condition" "conditions" "symptom" "symptoms" "treatment"
     "treatments" "recovery" "medical" "medicine" "medication")
    ("name" "names" "named" "called")))

(defun %recall-selection-items (value)
  (cond ((null value) nil)
        ((vectorp value) (coerce value 'list))
        ((listp value) value)
        (t (list value))))

(defun %recall-selection-terms (text)
  (let ((terms nil) (characters nil))
    (labels ((flush ()
               (when characters
                 (let* ((raw (string-downcase
                              (coerce (nreverse characters) 'string)))
                        (term (if (and (> (length raw) 2)
                                       (char= #\s (char raw (1- (length raw))))
                                       (not (member raw '("this" "was" "is")
                                                    :test #'string=)))
                                  (subseq raw 0 (1- (length raw))) raw)))
                   (when (and (>= (length term) 2)
                              (not (member term *personal-recall-stop-terms*
                                           :test #'string=)))
                     (pushnew term terms :test #'string=)))
                 (setf characters nil))))
      (loop for character across (if (stringp text) text "")
            do (if (alphanumericp character)
                   (push character characters)
                   (flush)))
      (flush))
    (nreverse terms)))

(defun %recall-selection-category (term)
  (find-if (lambda (group) (member term group :test #'string=))
           *personal-recall-category-policy*))

(defun %recall-selection-categories (terms)
  (remove-duplicates
   (loop for term in terms
         for group = (%recall-selection-category term)
         when group collect (first group))
   :test #'string=))

(defun build-recall-query-plan
    (stimulus &key agent-id persona-id operator-binding root-id
                    trigger-event-id as-of (policy *personal-recall-selection-revision*))
  "Build a closed, detached query description.  It performs no IO or mutation."
  (unless (and (stringp stimulus)
               (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                            stimulus)))
               (stringp policy) (plusp (length policy)))
    (error "Recall query plan inputs are invalid"))
  (let* ((bounded-stimulus (%recall-selection-bounded-stimulus stimulus))
         (terms (%recall-selection-terms bounded-stimulus))
         (lower (string-downcase bounded-stimulus))
         (categories
           (remove-if
            (lambda (category)
              (and (string= category "spouse")
                   (or (search "business partner" lower)
                       (search "work partner" lower)
                       (search "project partner" lower))))
            (%recall-selection-categories terms)))
         (time-scope
           (cond ((or (search "previous" lower) (search "earlier" lower)
                      (search "past" lower) (search "history" lower)
                      (search "used to" lower) (search "ago" lower))
                  "historical")
                 ((or (search "current" lower) (search "today" lower)
                      (search "right now" lower) (search "presently" lower))
                  "current")
                 (t "unspecified")))
         (operator-referent-p
           (or (search " my " (format nil " ~a " lower))
               (search " our " (format nil " ~a " lower))
               (search " me " (format nil " ~a " lower))
               (search " i " (format nil " ~a " lower))))
         (personal-p (and operator-referent-p categories)))
    (obj "schema_version" 1
         "policy_revision" policy
         "original_query" (copy-seq bounded-stimulus)
         "original_query_truncated" (if (> (length stimulus)
                                                *personal-recall-query-character-budget*)
                                          t nil)
         "normalized_terms" (coerce terms 'vector)
         "requested_categories" (coerce categories 'vector)
         "requested_time_scope" time-scope
         "operator_fact_query" (if personal-p t nil)
         "operator_binding" (if personal-p (or operator-binding :null) :null)
         "agent_id" (or agent-id :null)
         "persona_id" (or persona-id :null)
         "root_id" (or root-id :null)
         "trigger_event_id" (or trigger-event-id :null)
         "as_of" (or as-of :null))))

(defun %recall-selection-overlap (left right)
  (count-if (lambda (term) (member term right :test #'string=)) left))

(defun %recall-selection-category-overlap (plan text-terms)
  (%recall-selection-overlap
   (%recall-selection-items (gethash "requested_categories" plan))
   (%recall-selection-categories text-terms)))

(defun recall-selection-candidate
    (plan source-kind record &key candidate-id support-key local-rank
                                  semantic-rank lexical-rank operator-support-p
                                  speaker-basis observed-at supported-match-p
                                  maximum-relevance-class)
  "Normalize one already-authorized detached evidence row for pure selection."
  (unless (and (hash-table-p plan) (stringp source-kind)
               (hash-table-p record)
               (integerp local-rank) (plusp local-rank))
    (error "Recall candidate inputs are invalid"))
  (let* ((content (gethash "content" record ""))
         (query-terms (%recall-selection-items
                       (gethash "normalized_terms" plan)))
         (text-terms (%recall-selection-terms content))
         (term-overlap (%recall-selection-overlap query-terms text-terms))
         (requested-categories
           (%recall-selection-items (gethash "requested_categories" plan)))
         (text-categories (%recall-selection-categories text-terms))
         (category-overlap
           (%recall-selection-overlap requested-categories text-categories))
         ;; Name is an attribute, not the relationship/health qualifier that
         ;; makes a personal result answer-bearing.  A row mentioning a name
         ;; cannot satisfy a children, spouse, pet or health request by itself.
         (requested-qualifiers
           (remove "name" requested-categories :test #'string=))
         (qualifier-overlap
           (%recall-selection-overlap requested-qualifiers text-categories))
         (personal-p (gethash "operator_fact_query" plan))
         (operator-ok (if operator-support-p t nil))
         (raw-class
           (cond
             ((and personal-p (not operator-ok)) 0)
             ((and requested-qualifiers (zerop qualifier-overlap)) 0)
             ;; A recognized family/health category without a resolved
             ;; operator referent is ambiguous.  Do not silently disclose an
             ;; operator fact as though "their" meant "my".
             ((and requested-qualifiers (not personal-p)) 0)
             ((plusp term-overlap) 3)
             ((or (plusp category-overlap) supported-match-p) 2)
             (t 0)))
         (class (if maximum-relevance-class
                    (min raw-class maximum-relevance-class)
                    raw-class))
         (ranks (remove-if-not #'identity
                               (list local-rank semantic-rank lexical-rank)))
         (fused (reduce #'+ ranks
                        :key (lambda (rank)
                               (floor 1000000 (+ 60 rank)))
                        :initial-value 0))
         (id (or candidate-id (gethash "source_id" record)
                 (gethash "id" record))))
    (unless (and id (stringp content) (plusp (length content)))
      (error "Recall candidate lacks identity or content"))
    (obj "candidate_id" id "source_kind" source-kind
         "support_key" (or support-key id)
         "record" record
         "relevance_class" class
         "matched_facet_count" (+ term-overlap category-overlap)
         "fused_rank_score" fused
         "operator_support" operator-ok
         "speaker_basis" (or speaker-basis "unknown")
         "observed_at" (or observed-at :null)
         "rendered_characters" (length content))))

(defun %recall-selection-duplicate-key (candidate)
  (let* ((record (gethash "record" candidate))
         (content (string-downcase (gethash "content" record ""))))
    (format nil "~a|~a" (gethash "support_key" candidate) content)))

(defun %recall-selection-better-p (left right)
  ;; Equivalent projections represent the same disclosure, not independent
  ;; corroboration.  Put the compact validated graph fact first so the later
  ;; exact-dedup pass retains it while preserving all support identity.
  (when (equal (%recall-selection-duplicate-key left)
               (%recall-selection-duplicate-key right))
    (let ((left-graph-p (string= "graph-fact"
                                 (gethash "source_kind" left "")))
          (right-graph-p (string= "graph-fact"
                                  (gethash "source_kind" right ""))))
      (unless (eql left-graph-p right-graph-p)
        (return-from %recall-selection-better-p left-graph-p))))
  (dolist (key '("relevance_class" "matched_facet_count" "fused_rank_score"))
    (let ((left-value (gethash key left 0))
          (right-value (gethash key right 0)))
      (when (/= left-value right-value)
        (return-from %recall-selection-better-p (> left-value right-value)))))
  (unless (eql (gethash "operator_support" left)
               (gethash "operator_support" right))
    (return-from %recall-selection-better-p
      (if (gethash "operator_support" left) t nil)))
  (string< (princ-to-string (gethash "support_key" left))
           (princ-to-string (gethash "support_key" right))))

(defun recall-selection-select (candidates maximum character-budget)
  "Rank, exact-deduplicate and pack one bounded evidence set.
Returns selected candidates and a content-free report."
  (unless (and (integerp maximum) (<= 0 maximum 12)
               (integerp character-budget) (<= 0 character-budget 16384))
    (error "Recall selection bounds are invalid"))
  (let* ((eligible (remove-if (lambda (row)
                                (zerop (gethash "relevance_class" row 0)))
                              (%recall-selection-items candidates)))
         (ranked (stable-sort (copy-list eligible)
                              #'%recall-selection-better-p))
         (seen (make-hash-table :test #'equal))
         (deduplicated nil) (duplicates 0))
    (dolist (candidate ranked)
      (let ((key (%recall-selection-duplicate-key candidate)))
        (if (gethash key seen)
            (incf duplicates)
            (progn (setf (gethash key seen) t)
                   (push candidate deduplicated)))))
    (setf deduplicated (nreverse deduplicated))
    (let ((selected nil) (used 0) (budget-refusals 0))
      (dolist (candidate deduplicated)
        (let ((size (gethash "rendered_characters" candidate 0)))
          (cond ((>= (length selected) maximum) (incf budget-refusals))
                ((> (+ used size) character-budget) (incf budget-refusals))
                (t (push candidate selected) (incf used size)))))
      (let ((ordered (nreverse selected)))
        (values
         ordered
         (obj "schema_version" 1
              "policy_revision" *personal-recall-selection-revision*
              "examined_count" (length (%recall-selection-items candidates))
              "relevant_count" (length eligible)
              "selected_count" (length ordered)
              "duplicate_refusal_count" duplicates
              "budget_refusal_count" budget-refusals
              "rendered_characters" used
              "non_exhaustive" (if (or (plusp duplicates)
                                         (plusp budget-refusals)) t nil)
              "database_write_count" 0))))))

(defun recall-selection-report (selection-report)
  (unless (hash-table-p selection-report)
    (error "Recall selection report is invalid"))
  selection-report)
