;;;; turn-bundle-retrieval.lisp -- pure bounded relational evidence bundles.

(in-package :agent)

(export '(turn-bundle-build-candidates turn-bundle-row-turn-id
          turn-bundle-row-role turn-bundle-row-sequence))

(defparameter *turn-bundle-max-members* 4)
(defparameter *turn-bundle-max-candidates* 12)
(defparameter *turn-bundle-cluster-cap* 3)
(defparameter *turn-bundle-near-duplicate-threshold* 0.88d0)
(defparameter *turn-bundle-member-content-chars* 420)
(defparameter *turn-bundle-render-content-budget* 920)

(defun %turn-bundle-metadata (row)
  (let ((metadata (and (hash-table-p row)
                       (gethash "epistemic_metadata" row))))
    (if (hash-table-p metadata) metadata (obj))))

(defun turn-bundle-row-turn-id (row)
  (let ((value (gethash "turn_id" (%turn-bundle-metadata row))))
    (and (stringp value) (plusp (length value)) value)))

(defun turn-bundle-row-role (row)
  (let ((value (gethash "role" (%turn-bundle-metadata row))))
    (if (and (stringp value)
             (member value '("user" "assistant" "tool") :test #'string=))
        value
        (cond ((string= (gethash "origin_class" row "") "lived-user")
               "user")
              ((string= (gethash "origin_class" row "")
                        "lived-agent-action") "assistant")
              ((string= (gethash "origin_class" row "") "tool-result")
               "tool")
              (t "evidence")))))

(defun turn-bundle-row-sequence (row)
  (let ((value (gethash "sequence" (%turn-bundle-metadata row))))
    (and (integerp value) (not (minusp value)) value)))

(defun %turn-bundle-key (row)
  (let ((turn-id (turn-bundle-row-turn-id row)))
    (if turn-id
        (format nil "turn:~a" turn-id)
        (format nil "node:~a" (gethash "id" row "unknown")))))

(defun %turn-bundle-row< (left right)
  (let ((left-sequence (turn-bundle-row-sequence left))
        (right-sequence (turn-bundle-row-sequence right)))
    (cond ((and left-sequence right-sequence)
           (if (= left-sequence right-sequence)
               (string< (gethash "id" left "") (gethash "id" right ""))
               (< left-sequence right-sequence)))
          (left-sequence t)
          (right-sequence nil)
          (t (string< (gethash "id" left "")
                      (gethash "id" right ""))))))

(defun %turn-bundle-member-subset (rows anchor maximum)
  (let* ((ordered (stable-sort (copy-list rows) #'%turn-bundle-row<))
         (first-user (find "user" ordered :test #'string=
                           :key #'turn-bundle-row-role))
         (last-assistant
           (find "assistant" ordered :test #'string=
                 :key #'turn-bundle-row-role :from-end t))
         (chosen (remove-duplicates
                  (remove nil (list first-user anchor last-assistant))
                  :test #'eq)))
    (dolist (row ordered)
      (when (< (length chosen) maximum)
        (pushnew row chosen :test #'eq)))
    (stable-sort (subseq chosen 0 (min maximum (length chosen)))
                 #'%turn-bundle-row<)))

(defun %turn-bundle-cosine (left right)
  (if (or (null left) (null right) (/= (length left) (length right)))
      0.0d0
      (let ((dot 0.0d0) (left-norm 0.0d0) (right-norm 0.0d0))
        (loop for a in left for b in right
              do (incf dot (* a b))
                 (incf left-norm (* a a))
                 (incf right-norm (* b b)))
        (if (or (zerop left-norm) (zerop right-norm))
            0.0d0
            (/ dot (sqrt (* left-norm right-norm)))))))

(defun %turn-bundle-role-label (role)
  (cond ((string= role "user") "the operator")
        ((string= role "assistant") "the agent")
        ((string= role "tool") "Tool")
        (t "Evidence")))

(defun %turn-bundle-render (members)
  (let ((per-member
          (min *turn-bundle-member-content-chars*
               (floor *turn-bundle-render-content-budget*
                      (max 1 (length members))))))
    (with-output-to-string (stream)
      (loop for row in members
            for index from 0
            for content = (let ((value (gethash "content" row "")))
                            (if (stringp value) value (format nil "~a" value)))
            for bounded = (subseq content 0 (min (length content) per-member))
            when (plusp index) do (terpri stream)
            do (format stream "~a: ~a"
                       (%turn-bundle-role-label (turn-bundle-row-role row))
                       bounded)))))

(defun %turn-bundle-build-row (key anchor members relevance public-score)
  (let* ((turn-id (turn-bundle-row-turn-id anchor))
         (member-ids (mapcar (lambda (row) (gethash "id" row)) members))
         (roles (mapcar #'turn-bundle-row-role members)))
    (let ((result
            (obj "id" (if turn-id
                          (format nil "turn-bundle:~a" turn-id)
                          (format nil "turn-bundle-node:~a"
                                  (gethash "id" anchor)))
                 "kind" "turn-bundle"
                 "content" (%turn-bundle-render members)
                 "origin_class" "derived-lived"
                 "epistemic_status" "grounded-turn-bundle"
                 "grounding_status"
                 (if (every (lambda (row)
                              (string= "grounded"
                                       (gethash "grounding_status" row "")))
                            members)
                     "grounded" "partially-grounded")
                 "label" "Grounded conversation exchange"
                 "turn_id" (or turn-id :null)
                 "bundle_key" key
                 "anchor_id" (gethash "id" anchor)
                 "observed_at" (or (gethash "observed_at" anchor)
                                    (gethash "created_at" anchor)
                                    :null)
                 "valid_from" (gethash "valid_from" anchor :null)
                 "valid_to" (gethash "valid_to" anchor :null)
                 "supersedes_node_id"
                 (gethash "supersedes_node_id" anchor :null)
                 "member_count" (length members)
                 "member_roles" (coerce roles 'vector)
                 "evidence_node_ids" (coerce member-ids 'vector)
                 "similarity" relevance
                 "retrieval_score" public-score)))
      (dolist (field '("candidate_sources" "lexical_tier"
                       "lexical_match_count" "lexical_coverage"
                       "lexical_terms"))
        (multiple-value-bind (value present-p) (gethash field anchor)
          (when present-p (setf (gethash field result) value))))
      result)))

(defun turn-bundle-build-candidates
    (ranked corpus &key
                     (max-bundles *turn-bundle-max-candidates*)
                     (max-members *turn-bundle-max-members*)
                     (cluster-cap *turn-bundle-cluster-cap*)
                     (near-duplicate-threshold
                       *turn-bundle-near-duplicate-threshold*))
  "Build bounded speaker-ordered bundles from ranked atomic anchors.
RANKED entries are (ROW SIMILARITY PUBLIC-SCORE VECTOR). Returns three values:
selected bundle rows, count suppressed by the semantic cluster cap, and total
anchored bundles considered. No adapter or side effect is reachable here."
  (unless (and (integerp max-bundles) (<= 1 max-bundles 50)
               (integerp max-members) (<= 1 max-members 12)
               (integerp cluster-cap) (<= 1 cluster-cap 12)
               (numberp near-duplicate-threshold)
               (<= 0.0d0 near-duplicate-threshold 1.0d0))
    (error "Turn-bundle bounds are invalid."))
  (let ((groups (make-hash-table :test #'equal))
        (vectors (make-hash-table :test #'equal))
        (seen (make-hash-table :test #'equal))
        (candidates nil))
    (dolist (row corpus)
      (push row (gethash (%turn-bundle-key row) groups)))
    (dolist (entry ranked)
      (let ((row (first entry)) (vector (fourth entry)))
        (when (and (hash-table-p row)
                   (or vector (plusp (gethash "lexical_tier" row 0))))
          (setf (gethash (gethash "id" row) vectors) vector))))
    (dolist (entry ranked)
      (let* ((anchor (first entry))
             (key (%turn-bundle-key anchor)))
        (unless (gethash key seen)
          (setf (gethash key seen) t)
          (let* ((members (%turn-bundle-member-subset
                           (or (gethash key groups) (list anchor))
                           anchor max-members))
                 ;; Diversity is measured on the semantic anchor, not the
                 ;; rendered bundle. Otherwise varied answers can hide a
                 ;; cluster of repeated historical question paraphrases.
                 (vector (gethash (gethash "id" anchor) vectors))
                 (row (%turn-bundle-build-row
                       key anchor members (second entry) (third entry))))
            (push (list row vector) candidates)))))
    (setf candidates (nreverse candidates))
    (let ((clusters nil) (selected nil) (suppressed 0))
      (dolist (candidate candidates)
        (when (< (length selected) max-bundles)
          (let* ((vector (second candidate))
                 (cluster
                   (find-if
                    (lambda (item)
                      (>= (%turn-bundle-cosine vector (first item))
                          near-duplicate-threshold))
                    clusters)))
            (cond ((null cluster)
                   (push (list vector 1) clusters)
                   (push (first candidate) selected))
                  ((< (second cluster) cluster-cap)
                   (incf (second cluster))
                   (push (first candidate) selected))
                  (t (incf suppressed))))))
      (values (nreverse selected) suppressed (length candidates)))))
