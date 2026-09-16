;;;; legacy-memory-audit.lisp -- reversible legacy quarantine audit.

(in-package :agent)

(export '(legacy-memory-audit-dry-run legacy-memory-audit-apply
          legacy-memory-audit-report))

(defparameter *legacy-audit-high-similarity* 0.90d0)
(defparameter *legacy-audit-fixed-query-access-floor* 20)
(defparameter *legacy-audit-fixed-query-activation-floor* 0.85d0)
(defparameter *legacy-audit-similarity-max-nodes* 250
  "Bound the quadratic embedding comparison while all structural rows remain audited.")
(defvar *legacy-audit-last-report* nil)
(defvar *legacy-audit-row-source-fn* nil
  "Optional () -> node hashes source used by deterministic tests.")
(defvar *legacy-audit-similarity-source-fn* nil
  "Optional () -> (left-id right-id similarity) rows source used by tests.")
(defvar *legacy-audit-quarantine-fn* nil
  "Optional (id reason actor) adapter used by tests.")
(defvar *legacy-audit-hash-fn* nil
  "Port: (TEXT) -> content-hash string, or NIL when no v2 latent-thoughts
layer is present. Registered by LATENT-THOUGHTS-V2.LISP at :INSTALL; falls
back to a bare SXHASH so the fingerprint is stable either way.")

(defun %legacy-audit-list (value)
  (cond ((null value) nil) ((listp value) value)
        ((vectorp value) (coerce value 'list)) (t (list value))))

(defun %legacy-audit-json (text fallback)
  (if (and (stringp text) (plusp (length text)))
      (handler-case (shasht:read-json text) (error () fallback)) fallback))

(defun %legacy-audit-db-rows ()
  (with-pg
    (mapcar
     (lambda (row)
       (destructuring-bind (id kind content created origin status grounding roots
                            producer access activation quarantined metadata) row
         (obj "id" id "kind" kind "content" content "created_at" created
              "origin_class" origin "epistemic_status" status
              "grounding_status" grounding
              "root_observation_ids" (%legacy-audit-json roots (vector))
              "producer" (or producer :null) "access_count" access
              "activation" activation "quarantined" (if quarantined t nil)
              "epistemic_metadata" (%legacy-audit-json metadata (obj)))))
     (pomo:query
      "SELECT id,kind,content,created_at::text,origin_class,epistemic_status,grounding_status,root_observation_ids::text,producer,access_count,activation,quarantined,epistemic_metadata::text FROM memory_nodes WHERE origin_class='legacy-unclassified' OR epistemic_status='legacy-unclassified' OR grounding_status='unclassified'"))))

(defun %legacy-audit-db-similarities ()
  (with-pg
    (pomo:query
     (format nil
             "WITH legacy AS (SELECT id,embedding FROM memory_nodes WHERE origin_class='legacy-unclassified' OR epistemic_status='legacy-unclassified' OR grounding_status='unclassified' ORDER BY activation DESC,access_count DESC,created_at DESC LIMIT ~a) SELECT a.id,b.id,1-(a.embedding <=> b.embedding) AS similarity FROM legacy a JOIN legacy b ON a.id < b.id WHERE 1-(a.embedding <=> b.embedding) >= 0.90 ORDER BY similarity DESC LIMIT 1000"
             *legacy-audit-similarity-max-nodes*))))

(defun %legacy-audit-reasons (node similar-ids failed-window-p)
  (let ((reasons nil)
        (roots (%legacy-audit-list (gethash "root_observation_ids" node)))
        (metadata (gethash "epistemic_metadata" node)))
    (when (and (null roots)
               (member (gethash "origin_class" node)
                       '("legacy-unclassified" "synthetic") :test #'string=))
      (push "no-lived-source" reasons))
    (when (and (null roots)
               (or (string= (gethash "origin_class" node "") "synthetic")
                   (and (hash-table-p metadata)
                        (gethash "recursive_synthetic_lineage" metadata))))
      (push "recursive-synthetic-lineage" reasons))
    (when similar-ids (push "high-similarity-cluster" reasons))
    (when (and (>= (gethash "access_count" node 0)
                   *legacy-audit-fixed-query-access-floor*)
               (>= (gethash "activation" node 0.0d0)
                   *legacy-audit-fixed-query-activation-floor*))
      (push "repeated-fixed-query-activation" reasons))
    (when failed-window-p (push "failed-handler-window" reasons))
    (nreverse reasons)))

(defun %legacy-audit-candidate-node-p (node)
  (or (string= (gethash "origin_class" node "") "legacy-unclassified")
      (string= (gethash "epistemic_status" node "") "legacy-unclassified")
      (string= (gethash "grounding_status" node "") "unclassified")
      (and (hash-table-p (gethash "epistemic_metadata" node))
           (gethash "recursive_synthetic_lineage"
                    (gethash "epistemic_metadata" node)))))

(defun %legacy-audit-fingerprint (items)
  (let ((text (with-output-to-string (out)
                (dolist (item (sort (copy-list items) #'string<
                                    :key (lambda (x) (gethash "id" x))))
                  (format out "~a:~{~a~^,~};"
                          (gethash "id" item)
                          (%legacy-audit-list (gethash "reasons" item)))))))
    (cond (*legacy-audit-hash-fn* (funcall *legacy-audit-hash-fn* text))
          ((fboundp '%latent-v2-hash) (funcall '%latent-v2-hash text))
          (t (format nil "~16,'0x" (ldb (byte 64 0) (sxhash text)))))))

(defun legacy-memory-audit-dry-run (&key failed-node-ids)
  "Return a manifest only. No node, edge, or text is changed."
  (let* ((nodes (if *legacy-audit-row-source-fn*
                    (funcall *legacy-audit-row-source-fn*)
                    (%legacy-audit-db-rows)))
         (pairs (if *legacy-audit-similarity-source-fn*
                    (funcall *legacy-audit-similarity-source-fn*)
                    (%legacy-audit-db-similarities)))
         (similar (make-hash-table :test #'equal))
         (manifest nil))
    (dolist (pair pairs)
      (destructuring-bind (left right similarity) pair
        (when (>= similarity *legacy-audit-high-similarity*)
          (push right (gethash left similar))
          (push left (gethash right similar)))))
    (dolist (node nodes)
      (let* ((id (gethash "id" node))
             (peers (remove-duplicates (gethash id similar) :test #'string=))
             (reasons (%legacy-audit-reasons
                       node peers (member id failed-node-ids :test #'string=))))
        (when (and (%legacy-audit-candidate-node-p node)
                   reasons (not (gethash "quarantined" node)))
          (push (obj "id" id "kind" (gethash "kind" node)
                     "producer" (gethash "producer" node)
                     "reasons" (coerce reasons 'vector)
                     "similar_node_ids" (coerce peers 'vector)) manifest))))
    (setf manifest (nreverse manifest))
    (setf *legacy-audit-last-report*
          (obj "mode" "dry-run" "scanned" (length nodes)
               "candidate_count" (length manifest)
               "similarity_scope_limit" *legacy-audit-similarity-max-nodes*
               "similarity_pairs_found" (length pairs)
               "manifest_id" (%legacy-audit-fingerprint manifest)
               "candidates" (coerce manifest 'vector)
               "deletes" 0 "text_or_edge_changes" 0))
    *legacy-audit-last-report*))

(defun legacy-memory-audit-apply (report confirmation &key (operator "out-of-band"))
  "Apply exactly a reviewed manifest. Confirmation must equal MANIFEST_ID.
The operation only sets quarantine metadata through MEMORY-QUARANTINE."
  (unless (and (hash-table-p report)
               (stringp confirmation)
               (string= confirmation (gethash "manifest_id" report)))
    (error "Explicit manifest confirmation mismatch"))
  (let ((batch-id (format nil "legacy-quarantine-~a" (get-universal-time)))
        (changed nil))
    (dolist (item (%legacy-audit-list (gethash "candidates" report)))
      (let ((id (gethash "id" item))
            (reason (format nil "~{~a~^, ~}"
                            (%legacy-audit-list (gethash "reasons" item)))))
        (if *legacy-audit-quarantine-fn*
            (funcall *legacy-audit-quarantine-fn* id reason operator)
            (memory-quarantine id :reason reason :actor operator))
        (push id changed)))
    (setf changed (nreverse changed))
    (when (fboundp 'log-event)
      (funcall 'log-event "memory-quarantine-changed"
               (obj "event_version" 1 "batch_id" batch-id
                    "node_ids" (coerce changed 'vector)
                    "count" (length changed) "old_value" nil "new_value" t
                    "reasons" "reviewed manifest"
                    "operator" operator "source" "legacy-memory-audit")))
    (obj "batch_id" batch-id "quarantined" (length changed)
         "deleted" 0 "manifest_id" confirmation)))

(defun legacy-memory-audit-report ()
  (or *legacy-audit-last-report*
      (obj "mode" "not-run" "candidate_count" 0 "deletes" 0)))
