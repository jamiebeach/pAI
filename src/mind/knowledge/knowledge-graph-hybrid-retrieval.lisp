;;;; knowledge-graph-hybrid-retrieval.lisp -- pure KG4 link extraction.

(in-package :agent)

(export '(knowledge-graph-hybrid-retrieve))

(defparameter *knowledge-graph-hybrid-revision*
  "knowledge-graph-hybrid-retrieval-v1")
(defparameter *knowledge-graph-hybrid-maximum-semantic-candidates* 8)
(defparameter *knowledge-graph-hybrid-maximum-episode-candidates* 8)
(defparameter *knowledge-graph-hybrid-maximum-source-ids* 16)
(defparameter *knowledge-graph-hybrid-maximum-evidence-event-ids* 32)

(defun %kgh-items (value)
  (cond ((null value) nil)
        ((vectorp value) (coerce value 'list))
        ((listp value) value)
        (t nil)))

(defun %kgh-memory-id (id)
  (when (%kgs-present-string-p id 240)
    (if (uiop:string-prefix-p "memory:" id)
        id
        (format nil "memory:~a" id))))

(defun %kgh-source-id (id)
  (and (%kgs-present-string-p id 240) id))

(defun %kgh-event-id (id)
  (and (integerp id) (plusp id) id))

(defun knowledge-graph-hybrid-retrieve
    (request semantic-candidates episode-candidates search-fn
     &key (semantic-status "available") (episodic-status "available"))
  "Bridge already-selected typed candidates to one verified graph search.

SEARCH-FN receives REQUEST, a vector of exact opaque source IDs and a vector
of exact evidence event IDs. It owns storage verification and traversal."
  (unless (and (knowledge-graph-search-request-valid-p request)
               (vectorp semantic-candidates)
               (vectorp episode-candidates)
               (every #'hash-table-p semantic-candidates)
               (every #'hash-table-p episode-candidates)
               (functionp search-fn)
               (member semantic-status '("available" "empty" "unavailable")
                       :test #'string=)
               (member episodic-status '("available" "empty" "unavailable")
                       :test #'string=))
    (error "Hybrid graph retrieval inputs are invalid"))
  (let ((source-ids nil) (event-ids nil)
        (semantic-clipped
          (> (length semantic-candidates)
             *knowledge-graph-hybrid-maximum-semantic-candidates*))
        (episode-clipped
          (> (length episode-candidates)
             *knowledge-graph-hybrid-maximum-episode-candidates*)))
    (loop for candidate across semantic-candidates
          repeat *knowledge-graph-hybrid-maximum-semantic-candidates*
          do (let ((id (%kgh-memory-id (gethash "id" candidate))))
               (when id (pushnew id source-ids :test #'string=)))
             (dolist (evidence (%kgh-items
                                (gethash "evidence_node_ids" candidate)))
               (let ((id (%kgh-memory-id evidence)))
                 (when id (pushnew id source-ids :test #'string=))))
             (let ((event-id (%kgh-event-id
                              (gethash "source_event_id" candidate))))
               (when event-id (pushnew event-id event-ids :test #'=))))
    (loop for episode across episode-candidates
          repeat *knowledge-graph-hybrid-maximum-episode-candidates*
          do (let ((id (%kgh-source-id (gethash "episode_id" episode))))
               (when id (pushnew id source-ids :test #'string=)))
             (dolist (source (%kgh-items
                              (gethash "source_event_ids" episode)))
               (let ((event-id (%kgh-event-id source)))
                 (when event-id (pushnew event-id event-ids :test #'=))))
             (let ((descriptor (%kgh-event-id
                                (gethash "event_id" episode))))
               (when descriptor (pushnew descriptor event-ids :test #'=))))
    (setf source-ids (nreverse source-ids)
          event-ids (nreverse event-ids))
    (let* ((source-clipped
             (> (length source-ids)
                *knowledge-graph-hybrid-maximum-source-ids*))
           (event-clipped
             (> (length event-ids)
                *knowledge-graph-hybrid-maximum-evidence-event-ids*))
           (bounded-source
             (subseq source-ids 0
                     (min (length source-ids)
                          *knowledge-graph-hybrid-maximum-source-ids*)))
           (bounded-events
             (subseq event-ids 0
                     (min (length event-ids)
                          *knowledge-graph-hybrid-maximum-evidence-event-ids*)))
           (result
             (funcall search-fn request
                      (coerce bounded-source 'vector)
                      (coerce bounded-events 'vector))))
      (unless (hash-table-p result)
        (error "Hybrid graph read port returned no structural result"))
      (setf (gethash "hybrid_revision" result)
            *knowledge-graph-hybrid-revision*
            (gethash "semantic_candidate_status" result) semantic-status
            (gethash "semantic_candidate_count" result)
            (min (length semantic-candidates)
                 *knowledge-graph-hybrid-maximum-semantic-candidates*)
            (gethash "episodic_candidate_status" result) episodic-status
            (gethash "episodic_candidate_count" result)
            (min (length episode-candidates)
                 *knowledge-graph-hybrid-maximum-episode-candidates*)
            (gethash "hybrid_source_ids" result)
            (coerce bounded-source 'vector)
            (gethash "hybrid_evidence_event_ids" result)
            (coerce bounded-events 'vector)
            (gethash "hybrid_non_exhaustive" result)
            (if (or semantic-clipped episode-clipped source-clipped
                    event-clipped
                    (string= semantic-status "unavailable")
                    (string= episodic-status "unavailable"))
                t nil))
      (when (gethash "hybrid_non_exhaustive" result)
        (setf (gethash "non_exhaustive" result) t))
      result)))
