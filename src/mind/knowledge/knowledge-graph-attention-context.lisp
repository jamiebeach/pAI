;;;; knowledge-graph-attention-context.lisp -- pure KG5 frame selection.

(in-package :agent)

(export '(knowledge-graph-attention-frame
          knowledge-graph-attention-context-records))

(defparameter *knowledge-graph-attention-context-revision*
  "knowledge-graph-attention-context-v4")
(defparameter *knowledge-graph-attention-query-characters* 1000)
(defparameter *knowledge-graph-attention-evidence-items* 64)

(defparameter *knowledge-graph-attention-stop-terms*
  '("about" "after" "again" "also" "and" "are" "but" "can" "could"
    "current" "does" "for" "from" "have" "into" "just" "like" "not"
    "operator" "private" "that" "the" "their" "there" "these" "they"
    "this" "through" "was" "what" "when" "where" "which" "with" "would"))

(defun %kgac-id-p (value)
  (or (%kgs-present-string-p value 512)
      (and (integerp value) (plusp value))))

(defun %kgac-records-p (value)
  (and (vectorp value) (every #'hash-table-p value)
       (every (lambda (row)
                (and (%kgac-id-p (gethash "source_id" row))
                     (stringp (gethash "content" row))))
              value)))

(defun knowledge-graph-attention-frame
    (&key attention-kind stimulus (private-focus-records #())
          (lifecycle-records #()) (work-records #()) (memory-records #()))
  (let ((frame
          (obj "attention_kind" attention-kind "stimulus" stimulus
               "private_focus_records" private-focus-records
               "lifecycle_records" lifecycle-records
               "work_records" work-records "memory_records" memory-records)))
    (unless (and (%kgs-present-string-p attention-kind 120)
                 (%kgs-present-string-p stimulus 8000)
                 (%kgac-records-p private-focus-records)
                 (%kgac-records-p lifecycle-records)
                 (%kgac-records-p work-records)
                 (%kgac-records-p memory-records))
      (error "Knowledge graph attention frame is invalid"))
    frame))

(defun %kgac-terms (text)
  (let ((terms nil) (characters nil))
    (labels ((flush ()
               (when characters
                 (let ((term (string-downcase
                              (coerce (nreverse characters) 'string))))
                   (when (and (>= (length term) 3)
                              (not (member term
                                           *knowledge-graph-attention-stop-terms*
                                           :test #'string=)))
                     (pushnew term terms :test #'string=)))
                 (setf characters nil))))
      (loop for character across (if (stringp text) text "")
            do (if (alphanumericp character)
                   (push character characters)
                   (flush)))
      (flush))
    (nreverse terms)))

(defun %kgac-public-conversation-kind-p (kind)
  "Closed compatibility set for public conversation attention roots."
  (member kind '("operator-conversation" "conversation") :test #'string=))

(defun %kgac-frame-text (frame)
  (with-output-to-string (stream)
    (format stream "~a " (gethash "stimulus" frame))
    ;; Retrieved memory already supplies hybrid seeds; feeding its prose back
    ;; into the query amplifies stale retrieval failures. For conversation,
    ;; the current stimulus leads. Private roots retain their active focus.
    (dolist (key (unless (%kgac-public-conversation-kind-p
                         (gethash "attention_kind" frame))
                   '("private_focus_records" "work_records")))
      (loop for row across (gethash key frame)
            do (write-string (gethash "content" row "") stream)
               (write-char #\Space stream)))))

(defun %kgac-query (frame)
  (let* ((terms (%kgac-terms (%kgac-frame-text frame)))
         (text (format nil "~{~a~^ ~}" terms)))
    (if (plusp (length text))
        (subseq text 0
                (min (length text)
                     *knowledge-graph-attention-query-characters*))
        ;; The exact stimulus is already non-empty; this only covers a frame
        ;; made entirely of stop words and remains a valid bounded cue.
        (subseq (gethash "stimulus" frame) 0
                (min (length (gethash "stimulus" frame))
                     *knowledge-graph-attention-query-characters*)))))

(defun %kgac-path-text (path)
  (with-output-to-string (stream)
    (let ((nodes (gethash "nodes" path))
          (edges (gethash "edges" path)))
      (loop for index below (length nodes)
            for node = (aref nodes index)
            do (when (plusp index)
                 (let ((edge (aref edges (1- index))))
                   (format stream
                           (if (equal "incoming" (gethash "traversal_direction" edge))
                               " <--~a-- " " --~a--> ")
                           (gethash "predicate" edge))))
               (format stream "~a [~a]"
                       (gethash "label" node "unnamed")
                       (gethash "node_kind" node "unknown"))
               (when (and (string= "episode" (gethash "node_kind" node ""))
                          (plusp (length (gethash "summary" node ""))))
                 (format stream " — summary: ~a" (gethash "summary" node)))))))

(defun %kgac-path-score (path terms)
  (let* ((text (string-downcase (%kgac-path-text path)))
         (overlap (count-if (lambda (term) (search term text)) terms))
         (edges (%kgs-items (gethash "edges" path)))
         (applicability
           (count-if
            (lambda (edge)
              (member (gethash "predicate" edge "")
                      '("requires" "prefers" "avoids" "constraint-for"
                        "applies-to" "supports")
                      :test #'string=))
            edges)))
    (if (and (zerop overlap) (not (gethash "hybrid_seed" path)))
        0
        (+ (* 10 overlap)
           (* 3 applicability)
           (if (gethash "hybrid_seed" path) 4 0)
           (if edges 2 0)
           (- (gethash "depth" path 0))))))

(defun %kgac-evidence (path)
  "Return a balanced bounded witness set and whether exhaustive evidence clipped.

Graph entities can accumulate evidence across the entire ledger.  A context
descriptor needs enough evidence to ground this path, not an exhaustive copy of
each entity's history.  Round-robin admission prevents one high-degree node
from crowding the relationship or its other endpoint out of the witness set."
  (let* ((descriptors
           (append (%kgs-items (gethash "nodes" path))
                   (%kgs-items (gethash "edges" path))))
         (buckets
           (mapcar
            (lambda (descriptor)
              (sort
               (remove-duplicates
                (remove-if-not
                 #'integerp
                 (%kgs-items (gethash "evidence_event_ids" descriptor)))
                :test #'=)
               #'>))
            descriptors))
         (all (remove-duplicates (apply #'append (copy-list buckets)) :test #'=))
         (selected nil) (seen (make-hash-table)) (index 0) (progress t))
    (loop while (and progress
                     (< (length selected)
                        *knowledge-graph-attention-evidence-items*))
          do (setf progress nil)
             (dolist (bucket buckets)
               (when (and (< index (length bucket))
                          (< (length selected)
                             *knowledge-graph-attention-evidence-items*))
                 (setf progress t)
                 (let ((id (nth index bucket)))
                   (unless (gethash id seen)
                     (setf (gethash id seen) t)
                     (push id selected)))))
             (incf index))
    (values (coerce (sort selected #'<) 'vector)
            (> (length all) (length selected)))))

(defun %kgac-source-path-p (path)
  (equal "conversation-episode-graph" (gethash "projection_name" path)))

(defun %kgac-ranked-layer (entries)
  "Prefer relationships within a layer, never across source/knowledge layers."
  (let ((relationships (remove-if-not
                        (lambda (entry) (plusp (gethash "depth" (cdr entry) 0)))
                        entries)))
    (stable-sort (copy-list (or relationships entries))
                 (lambda (left right)
                   (if (= (car left) (car right))
                       (string< (%kgs-path-key (cdr left)) (%kgs-path-key (cdr right)))
                       (> (car left) (car right)))))))

(defun %kgac-record (path)
  (let* ((nodes (gethash "nodes" path))
         (first (aref nodes 0))
         (last (aref nodes (1- (length nodes))))
         (evidence (%kgac-evidence path))
         (key (%kgs-path-key path))
         (hash
           (loop with value = 14695981039346656037
                 for character across key
                 do (setf value
                          (logand #xffffffffffffffff
                                  (* (logxor value (char-code character))
                                     1099511628211)))
                 finally (return value)))
         (source-id
           (format nil "graph-context:~16,'0x" hash)))
    (declare (ignore first last))
    (unless (plusp (length evidence))
      (error "Selected graph path has no verified evidence"))
    (obj "source_id" source-id
         "content"
         (format nil
                 "~a (selected, non-exhaustive; ~a): ~a"
                 (cond ((%kgac-source-path-p path)
                        "Episode/source navigation, not a typed relationship assertion")
                       ((plusp (gethash "depth" path 0)) "Typed relationship evidence")
                       (t "Typed entity reference, not a relationship assertion"))
                 (gethash "projection_name" path) (%kgac-path-text path))
         "provenance"
         (obj "descriptor_id" source-id
              "descriptor_event_id" (aref evidence 0)
              "evidence_event_ids" evidence))))

(defun knowledge-graph-attention-context-records
    (frame semantic-candidates episode-candidates hybrid-search-fn
     &key (character-budget 1600))
  "Return bounded selected graph records and one content-free report."
  (unless (and (hash-table-p frame)
               (equal '("attention_kind" "lifecycle_records" "memory_records"
                        "private_focus_records" "stimulus" "work_records")
                      (sort (loop for key being the hash-keys of frame collect key)
                            #'string<))
               (integerp character-budget) (<= 0 character-budget 8192))
    (error "Knowledge graph attention context inputs are invalid"))
  (when (zerop character-budget)
    (return-from knowledge-graph-attention-context-records
      (values #() (obj "schema_version" 1 "status" "disabled"
                       "selected_count" 0 "rendered_characters" 0
                       "database_write_count" 0))))
  (let* ((query (%kgac-query frame))
         (request (knowledge-graph-search-request
                   :query query :direction "both" :maximum-depth 2
                   :maximum-paths 20 :evidence-policy "verified"))
         (result (funcall hybrid-search-fn request semantic-candidates
                          episode-candidates))
         (terms (%kgac-terms query))
         (evidence-clipped-path-count 0)
         (scored
           (loop for path across (gethash "paths" result #())
                 for score = (%kgac-path-score path terms)
                 for evidence-result = (multiple-value-list (%kgac-evidence path))
                 for evidence = (first evidence-result)
                 when (second evidence-result)
                   do (incf evidence-clipped-path-count)
                 ;; Verified graph rows can be useful deliberate-search seeds
                 ;; without carrying event evidence (notably legacy KG1
                 ;; concepts). They cannot enter a strict model context record.
                 ;; Skip them locally; one ineligible path must not make the
                 ;; entire graph context operationally unavailable.
                 when (and (plusp score) (plusp (length evidence)))
                   collect (cons score path)))
         (relationship-scored
           (remove-if-not (lambda (entry)
                            (plusp (gethash "depth" (cdr entry) 0)))
                          scored))
         ;; A verbose source synopsis must not set the cutoff for typed facts.
         ;; Each layer has its own relevance cutoff; typed knowledge gets
         ;; first admission to the shared budget, sources use the remainder.
         (layers
           (list (%kgac-ranked-layer
                   (remove-if (lambda (entry) (%kgac-source-path-p (cdr entry))) scored))
                 (%kgac-ranked-layer
                   (remove-if-not (lambda (entry) (%kgac-source-path-p (cdr entry))) scored))))
         (records nil) (used 0) (clipped nil) (source-count 0) (typed-count 0))
    (dolist (ranked layers)
      (let ((threshold (if ranked (* 0.6d0 (caar ranked)) most-positive-fixnum)))
        (dolist (entry ranked)
          (when (< (car entry) threshold)
            (setf clipped t)
            (return))
          (let* ((record (%kgac-record (cdr entry)))
                 (size (length (gethash "content" record))))
            (if (<= (+ used size) character-budget)
                (progn (push record records) (incf used size)
                       (if (%kgac-source-path-p (cdr entry))
                           (incf source-count) (incf typed-count)))
                (setf clipped t))))))
    (let ((ordered (coerce (nreverse records) 'vector)))
      (values
       ordered
       (obj "schema_version" 1
            "selection_revision" *knowledge-graph-attention-context-revision*
            "status" (if (plusp (length ordered)) "selected"
                         (if (member (gethash "status" result "")
                                     '("available" "empty") :test #'string=)
                             "empty" "unavailable"))
            "attention_kind" (gethash "attention_kind" frame)
            "candidate_path_count" (length scored)
            "relationship_candidate_count" (length relationship-scored)
            "selected_count" (length ordered)
            "typed_record_count" typed-count
            "source_record_count" source-count
            "rendered_characters" used
            "evidence_clipped_path_count" evidence-clipped-path-count
            "non_exhaustive" (if (or clipped
                                      (plusp evidence-clipped-path-count)
                                      (gethash "non_exhaustive" result))
                                  t nil)
            "database_write_count" 0)))))
