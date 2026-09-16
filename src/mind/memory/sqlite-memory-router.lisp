;;;; sqlite-memory-router.lisp -- concrete event-first cognitive transactions.
;;;;
;;;; Loading this file installs no authority.  The operator/runtime must bind
;;;; the returned router and select :EVENT-FIRST once baseline qualification
;;;; and projection reconciliation have succeeded.

(in-package :agent)

(export '(make-sqlite-memory-cognitive-router))

(defun %sqlite-memory-router-null (value)
  (if (or (null value) (eq value :null)) nil value))

(defun %sqlite-memory-router-time (&optional (universal-time (get-universal-time)))
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time universal-time 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            year month day hour minute second)))

(defun %sqlite-memory-router-vector-hex (values expected-dimension)
  (let* ((items (cond ((vectorp values) values)
                      ((listp values) (coerce values 'vector))
                      (t nil)))
         (dimension (and items (length items))))
    (unless (and dimension (= dimension expected-dimension)
                 (<= dimension #xffff))
      (error 'memory-storage-error :operation :encode-operation-vector
             :detail "operation vector dimension differs from the sealed import"))
    (let ((octets (make-array (+ 4 (* dimension 4))
                              :element-type '(unsigned-byte 8)
                              :initial-element 0)))
      (setf (aref octets 0) (ldb (byte 8 8) dimension)
            (aref octets 1) (ldb (byte 8 0) dimension))
      (dotimes (index dimension)
        (unless (realp (aref items index))
          (error 'memory-storage-error :operation :encode-operation-vector
                 :detail "operation vector contains a non-number"))
        (let* ((single (coerce (aref items index) 'single-float))
               (bits (sb-kernel:single-float-bits single))
               (offset (+ 4 (* index 4))))
          (when (= #xff (ldb (byte 8 23) bits))
            (error 'memory-storage-error :operation :encode-operation-vector
                   :detail "operation vector contains a non-finite value"))
          (dotimes (byte 4)
            (setf (aref octets (+ offset byte))
                  (ldb (byte 8 (* 8 (- 3 byte))) bits)))))
      (%derived-octets-hex octets))))

(defun %sqlite-memory-router-row (envelope)
  (and envelope
       (%memory-storage-json-read (gethash "scalar_json" envelope)
                                  :sqlite-memory-router)))

(defun %sqlite-memory-router-epistemic-row (projection node-id)
  (let ((row (%sqlite-memory-router-row
              (memory-storage-operation-node projection node-id))))
    (when row
      (list (gethash "id" row) (gethash "origin_class" row)
            (gethash "epistemic_status" row)
            (gethash "grounding_status" row)
            (gethash "quarantined" row)))))

(defun %sqlite-memory-router-parent-ids (projection node-id)
  (mapcar (lambda (row-json)
            (gethash "to_id"
                     (%memory-storage-json-read row-json
                                                :sqlite-memory-router)))
          (memory-storage-operation-edges
           projection :from-id node-id :edge-type "derived-from")))

(defun %sqlite-memory-router-edge-json (id from-id to-id edge-type timestamp)
  (%memory-materializer-json
   (%memory-storage-object "id" id "from_id" from-id "to_id" to-id
                           "edge_type" edge-type "created_at" timestamp)))

(defun %sqlite-memory-router-node-envelope
    (projection request timestamp roots)
  (let* ((dimension (memory-storage-operation-vector-dimension projection))
         (proposed
           (make-memory-node-scalar-row
            :id (gethash "node_id" request)
            :kind (or (%sqlite-memory-router-null (gethash "kind" request))
                      "observation")
            :content (%sqlite-memory-router-null (gethash "content" request))
            :timestamp timestamp
            :importance (gethash "importance" request)
            :valence (gethash "valence" request)
            :arousal (gethash "arousal" request)
            :source-event-id
            (%sqlite-memory-router-null (gethash "source_event_id" request))
            :origin-class (gethash "origin_class" request)
            :epistemic-status (gethash "epistemic_status" request)
            :producer (%sqlite-memory-router-null (gethash "producer" request))
            :model-purpose
            (%sqlite-memory-router-null (gethash "model_purpose" request))
            :confidence
            (%sqlite-memory-router-null (gethash "confidence" request))
            :grounding-status (gethash "grounding_status" request)
            :root-observation-ids (coerce roots 'vector)
            :generation-id
            (%sqlite-memory-router-null (gethash "generation_id" request))
            :supersedes-node-id
            (%sqlite-memory-router-null (gethash "supersedes_node_id" request))
            :quarantined (gethash "quarantined" request)
            :epistemic-metadata (gethash "epistemic_metadata" request)))
         (existing
           (memory-storage-operation-node projection
                                          (gethash "node_id" request))))
    (%memory-storage-object
     "scalar_json"
     (if existing
         (memory-merge-node-upsert-rows
          (%sqlite-memory-router-row existing)
          (%memory-storage-json-read proposed :sqlite-memory-router))
         proposed)
     "embedding_binary_hex"
     (%sqlite-memory-router-vector-hex
      (gethash "embedding" request) dimension)
     "retrieval_embedding_binary_hex"
     (%sqlite-memory-router-vector-hex
      (gethash "retrieval_embedding" request) dimension))))

(defun %sqlite-memory-router-admission-plan (projection request clock-fn roots-cell)
  (let* ((node-id (gethash "node_id" request))
         (parents (remove-duplicates
                   (coerce (gethash "lineage_parent_ids" request) 'list)
                   :test #'string=))
         (timestamp (%sqlite-memory-router-time (funcall clock-fn))))
    (multiple-value-bind (reasons roots)
        (%epistemic-validation-reasons-with-accessors
         (lambda (id) (%sqlite-memory-router-epistemic-row projection id))
         (lambda (id) (%sqlite-memory-router-parent-ids projection id))
         :id node-id :kind (%sqlite-memory-router-null (gethash "kind" request))
         :source-event-id
         (%sqlite-memory-router-null (gethash "source_event_id" request))
         :origin-class (gethash "origin_class" request)
         :epistemic-status (gethash "epistemic_status" request)
         :producer (%sqlite-memory-router-null (gethash "producer" request))
         :model-purpose
         (%sqlite-memory-router-null (gethash "model_purpose" request))
         :confidence (%sqlite-memory-router-null (gethash "confidence" request))
         :grounding-status (gethash "grounding_status" request)
         :lineage-parent-ids parents
         :novelty-passed (gethash "novelty_passed" request)
         :self-process-event-id
         (%sqlite-memory-router-null (gethash "self_process_event_id" request))
         :epistemic-metadata (gethash "epistemic_metadata" request))
      (when reasons (error 'epistemic-admission-error :reasons reasons))
      (setf (car roots-cell) roots)
      (let ((next-id (memory-storage-operation-next-edge-id projection)))
        (memory-assemble-admission-operation
         (%sqlite-memory-router-node-envelope projection request timestamp roots)
         (memory-storage-operation-edges
          projection :from-id node-id :edge-type "derived-from")
         (loop for parent in parents for id from next-id
               collect (%sqlite-memory-router-edge-json
                        id node-id parent "derived-from" timestamp)))))))

(defun %sqlite-memory-router-supersession-plan
    (projection request clock-fn)
  (let* ((old-id (gethash "old_node_id" request))
         (replacement-id (gethash "replacement_node_id" request))
         (replacement (memory-storage-operation-node projection replacement-id)))
    (unless (memory-storage-operation-node projection old-id)
      (error "Missing superseded node ~a" old-id))
    (unless replacement (error "Missing replacement node ~a" replacement-id))
    (let* ((existing-edge
             (memory-storage-operation-edges
              projection :from-id replacement-id :to-id old-id
              :edge-type "supersedes"))
           (edge-json
             (unless existing-edge
               (%sqlite-memory-router-edge-json
                (memory-storage-operation-next-edge-id projection)
                replacement-id old-id "supersedes"
                (%sqlite-memory-router-time (funcall clock-fn))))))
      (memory-assemble-supersession-operation
       replacement old-id (gethash "reason" request)
       (gethash "actor" request) edge-json))))

(defun %sqlite-memory-router-rehearsal-plan
    (projection request clock-fn)
  (let ((envelopes nil))
    (loop for id across (gethash "node_ids" request)
          for envelope = (memory-storage-operation-node projection id)
          when envelope do (push envelope envelopes))
    (when envelopes
      (memory-assemble-user-visible-rehearsal-operation
       (nreverse envelopes)
       (%sqlite-memory-router-time (funcall clock-fn))
       (gethash "consumer" request) (gethash "generation_id" request)
       (length (gethash "node_ids" request))))))

(defun %sqlite-memory-router-direct-edge-plan
    (projection request clock-fn)
  (let ((from (gethash "from_id" request))
        (to (gethash "to_id" request))
        (kind (gethash "edge_type" request)))
    (unless (and (stringp kind)
                 (member kind *memory-cognitive-edge-types* :test #'string=))
      (error 'memory-storage-error :operation :route-cognitive-mutation
             :detail "direct edge type is outside the closed vocabulary"))
    (unless (and (memory-storage-operation-node projection from)
                 (memory-storage-operation-node projection to))
      (error 'memory-storage-error :operation :route-cognitive-mutation
             :detail "direct edge endpoint is missing"))
    (unless (memory-storage-operation-edges
             projection :from-id from :to-id to :edge-type kind)
      (memory-assemble-direct-edge-operation
       (%sqlite-memory-router-edge-json
        (memory-storage-operation-next-edge-id projection)
        from to kind (%sqlite-memory-router-time (funcall clock-fn)))))))

(defun %sqlite-memory-router-quarantine-plan (projection request)
  (let ((envelope
          (memory-storage-operation-node projection
                                         (gethash "node_id" request))))
    (unless envelope
      (error 'memory-storage-error :operation :route-cognitive-mutation
             :detail "quarantine target is missing"))
    (memory-assemble-quarantine-operation
     envelope (gethash "reason" request) (gethash "actor" request))))

(defun %sqlite-memory-router-sequence (value)
  (cond ((null value) nil) ((listp value) (copy-list value))
        ((vectorp value) (coerce value 'list))
        (t (error 'memory-storage-error :operation :route-cognitive-mutation
                  :detail "cognitive request contains a non-sequence"))))

(defun %sqlite-memory-router-tick-plan (projection request clock-fn)
  (let* ((prepared (%sqlite-memory-router-sequence
                    (gethash "prepared" request)))
         (proposal (gethash "proposal" request))
         (edge-specs (%sqlite-memory-router-sequence
                      (and (hash-table-p proposal)
                           (gethash "edge_specs" proposal))))
         (timestamp (%sqlite-memory-router-time (funcall clock-fn)))
         (dimension (memory-storage-operation-vector-dimension projection))
         (next-edge-id (memory-storage-operation-next-edge-id projection))
         (virtual-rows (make-hash-table :test #'equal))
         (virtual-parents (make-hash-table :test #'equal))
         (edge-keys (make-hash-table :test #'equal))
         (node-envelopes nil) (edge-descriptors nil))
    (labels
        ((stored-row (id)
           (or (gethash id virtual-rows)
               (let ((envelope (memory-storage-operation-node projection id)))
                 (and envelope (%sqlite-memory-router-row envelope)))))
         (epistemic-row (id)
           (let ((row (stored-row id)))
             (and row
                  (list (gethash "id" row) (gethash "origin_class" row)
                        (gethash "epistemic_status" row)
                        (gethash "grounding_status" row)
                        (gethash "quarantined" row)))))
         (parents (id)
           (multiple-value-bind (virtual present-p) (gethash id virtual-parents)
             (if present-p virtual
                 (%sqlite-memory-router-parent-ids projection id))))
         (edge-key (from to kind) (list from to kind))
         (edge-exists-p (from to kind)
           (or (gethash (edge-key from to kind) edge-keys)
               (memory-storage-operation-edges
                projection :from-id from :to-id to :edge-type kind)))
         (add-edge (from to kind mutation-kind)
           (unless (member kind *memory-cognitive-edge-types* :test #'string=)
             (error 'tick-commit-validation-error
                    :reasons (list "tick edge type is outside the closed vocabulary")))
           (unless (and (epistemic-row from) (epistemic-row to))
             (error 'tick-commit-validation-error
                    :reasons (list "tick edge endpoint is missing")))
           (unless (edge-exists-p from to kind)
             (setf (gethash (edge-key from to kind) edge-keys) t)
             (push (%memory-storage-object
                    "row_json" (%sqlite-memory-router-edge-json
                                next-edge-id from to kind timestamp)
                    "mutation_kind" mutation-kind)
                   edge-descriptors)
             (incf next-edge-id))))
      (dolist (item prepared)
        (unless (listp item)
          (error 'memory-storage-error :operation :route-cognitive-mutation
                 :detail "prepared tick memory is not a property list"))
        (let* ((id (getf item :id))
               (parent-ids (remove-duplicates
                            (%sqlite-memory-router-sequence
                             (getf item :lineage-parent-ids))
                            :test #'string=)))
          (multiple-value-bind (reasons roots)
              (%epistemic-validation-reasons-with-accessors
               #'epistemic-row #'parents
               :id id :kind (getf item :kind)
               :source-event-id (getf item :source-event-id)
               :origin-class (getf item :origin-class)
               :epistemic-status (getf item :epistemic-status)
               :producer (getf item :producer)
               :model-purpose (getf item :model-purpose)
               :confidence (getf item :confidence)
               :grounding-status (getf item :grounding-status)
               :lineage-parent-ids parent-ids :novelty-passed t
               :self-process-event-id (gethash "source_event_id" request)
               :epistemic-metadata (getf item :metadata))
            (when reasons
              (error 'tick-commit-validation-error :reasons reasons))
            (let* ((proposed
                     (make-memory-node-scalar-row
                      :id id :kind (getf item :kind)
                      :content (getf item :content) :timestamp timestamp
                      :importance (getf item :importance)
                      :valence (getf item :valence)
                      :arousal (getf item :arousal)
                      :source-event-id (getf item :source-event-id)
                      :origin-class (getf item :origin-class)
                      :epistemic-status (getf item :epistemic-status)
                      :producer (getf item :producer)
                      :model-purpose (getf item :model-purpose)
                      :confidence (getf item :confidence)
                      :grounding-status (getf item :grounding-status)
                      :root-observation-ids (coerce roots 'vector)
                      :generation-id (getf item :generation-id)
                      :supersedes-node-id (getf item :supersedes-node-id)
                      :epistemic-metadata (getf item :metadata)))
                   (incumbent (stored-row id))
                   (scalar (if incumbent
                               (memory-merge-node-upsert-rows
                                incumbent
                                (%memory-storage-json-read
                                 proposed :sqlite-memory-router))
                               proposed))
                   (row (%memory-storage-json-read
                         scalar :sqlite-memory-router)))
              (setf (gethash id virtual-rows) row
                    (gethash id virtual-parents) parent-ids)
              (push (%memory-storage-object
                     "scalar_json" scalar
                     "embedding_binary_hex"
                     (%sqlite-memory-router-vector-hex
                      (getf item :embedding) dimension)
                     "retrieval_embedding_binary_hex"
                     (%sqlite-memory-router-vector-hex
                      (getf item :retrieval-embedding) dimension))
                    node-envelopes))
            (dolist (parent parent-ids)
              (add-edge id parent "derived-from" "tick-commit-lineage"))
            (when (getf item :supersedes-node-id)
              (add-edge id (getf item :supersedes-node-id) "supersedes"
                        "tick-commit-supersession")))))
      (dolist (edge edge-specs)
        (unless (hash-table-p edge)
          (error 'memory-storage-error :operation :route-cognitive-mutation
                 :detail "tick edge specification is not an object"))
        (add-edge (gethash "from_id" edge) (gethash "to_id" edge)
                  (gethash "edge_type" edge) "tick-commit-edge"))
      (when (or node-envelopes edge-descriptors)
        (memory-assemble-tick-commit-operation
         (nreverse node-envelopes) (nreverse edge-descriptors))))))

(defun make-sqlite-memory-cognitive-router (coordinator &key (clock-fn #'get-universal-time))
  "Return a complete event-first router. No global selector is changed."
  (unless (and (typep coordinator 'memory-mutation-coordinator)
               (functionp clock-fn))
    (error 'memory-storage-error :operation :make-sqlite-memory-router
           :detail "coordinator and clock function are required"))
  (lambda (family request)
    (cond
      ((string= family "admission")
       (let ((roots-cell (list nil)))
         (values
          (memory-mutation-coordinator-build-operation
           coordinator
           (lambda (projection)
             (%sqlite-memory-router-admission-plan
              projection request clock-fn roots-cell)))
          (car roots-cell))))
      ((string= family "supersession")
       (memory-mutation-coordinator-build-operation
        coordinator
        (lambda (projection)
          (%sqlite-memory-router-supersession-plan
           projection request clock-fn))))
      ((string= family "user-visible-rehearsal")
       (memory-mutation-coordinator-build-operation
        coordinator
        (lambda (projection)
          (%sqlite-memory-router-rehearsal-plan projection request clock-fn))
        :empty-contract "use-report"
        :empty-value
        (%memory-storage-object
         "consumer" (gethash "consumer" request)
         "generation_id" (gethash "generation_id" request)
         "user_visible" t
         "requested_count" (length (gethash "node_ids" request))
         "updated_count" 0)))
      ((string= family "tick-commit")
       (memory-mutation-coordinator-build-operation
        coordinator
        (lambda (projection)
          (%sqlite-memory-router-tick-plan projection request clock-fn))
        :empty-contract "node-id-list" :empty-value nil))
      ((string= family "direct-edge")
       (memory-mutation-coordinator-build-operation
        coordinator
        (lambda (projection)
          (%sqlite-memory-router-direct-edge-plan
           projection request clock-fn))
        :empty-contract "true" :empty-value t))
      ((string= family "quarantine")
       (memory-mutation-coordinator-build-operation
        coordinator
        (lambda (projection)
          (%sqlite-memory-router-quarantine-plan projection request))))
      (t
       (error 'memory-storage-error :operation :route-cognitive-mutation
              :detail "unknown cognitive mutation family")))))
