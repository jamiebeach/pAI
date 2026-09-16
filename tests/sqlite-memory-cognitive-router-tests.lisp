(in-package :agent)

(ql:quickload '(:cffi :shasht :ironclad :bordeaux-threads) :silent t)

(defvar *sqlite-router-pass* 0)
(defvar *sqlite-router-fail* 0)

(defun sqlite-router-check (name condition)
  (if condition
      (progn (incf *sqlite-router-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *sqlite-router-fail*) (format t "  FAIL ~a~%" name))))

(dolist (file '("storage-substrate.lisp" "memory-storage.lisp"
                "stabilization-config.lisp" "memory-nodes.lisp"
                "epistemic-memory.lisp" "typed-retrieval.lisp"
                "cognitive-call.lisp" "tick-commit.lisp"
                "sqlite-storage.lisp" "sqlite-derived-storage.lisp"
                "sqlite-memory-router.lisp" "memory-ledger-baseline.lisp"))
  (load (test-source file)))

(defclass sqlite-router-source (memory-storage-backend) ())

(defmethod memory-storage-capabilities ((source sqlite-router-source))
  (declare (ignore source))
  (%memory-storage-object
   "schema_version" 1 "backend" "sqlite-router-fixture"
   "read_snapshot" t "node_snapshot" t "edge_snapshot" t
   "exact_vector_export" t))

(defun sqlite-router-vector () "000200003f80000000000000")

(defun sqlite-router-root (id content)
  (list (make-memory-node-scalar-row
         :id id :kind "observation" :content content
         :timestamp "2026-08-20T12:00:00Z" :importance 0.1d0
         :origin-class "lived-user" :epistemic-status "user-report"
         :producer "operator" :grounding-status "grounded"
         :root-observation-ids (vector id)
         :epistemic-metadata (%memory-storage-object))
        (sqlite-router-vector)))

(defun sqlite-router-digest-update (digest text)
  (let* ((octets (sb-ext:string-to-octets text :external-format :utf-8))
         (prefix (sb-ext:string-to-octets
                  (format nil "~d:" (length octets))
                  :external-format :utf-8)))
    (ironclad:update-digest digest prefix)
    (ironclad:update-digest digest octets)))

(defun sqlite-router-digest-hex (digest)
  (string-downcase
   (ironclad:byte-array-to-hex-string (ironclad:produce-digest digest))))

(defmethod memory-storage-map-snapshot
    ((source sqlite-router-source) node-visitor edge-visitor)
  (declare (ignore source edge-visitor))
  (let ((nodes (list (sqlite-router-root "root-a" "alpha lived evidence")
                     (sqlite-router-root "root-b" "beta lived evidence")))
        (node-digest (ironclad:make-digest :sha256))
        (vector-digest (ironclad:make-digest :sha256))
        (edge-digest (ironclad:make-digest :sha256)))
    (dolist (node nodes)
      (sqlite-router-digest-update node-digest (first node))
      (dotimes (ignored 2)
        (declare (ignore ignored))
        (sqlite-router-digest-update vector-digest (second node)))
      (funcall node-visitor
               (%memory-storage-object
                "scalar_json" (first node)
                "embedding_binary_hex" (second node)
                "retrieval_embedding_binary_hex" (second node))))
    (%memory-storage-object
     "schema_version" 1 "node_count" 2 "edge_count" 0
     "node_sha256" (sqlite-router-digest-hex node-digest)
     "vector_sha256" (sqlite-router-digest-hex vector-digest)
     "edge_sha256" (sqlite-router-digest-hex edge-digest)
     "vector_binary_encoding" "pgvector-send-v1")))

(defun sqlite-router-provenance ()
  (make-memory-embedding-provenance
   :embedding-model "fixture" :embedding-revision "fixture-v1"
   :retrieval-embedding-model "fixture"
   :retrieval-embedding-revision "fixture-v1"
   :vector-dimension 2 :revision-evidence "deterministic-fixture"
   :approval-scope "qualification-only"))

(defun sqlite-router-delete-db (path)
  (dolist (candidate (list path
                           (pathname (concatenate 'string (namestring path) "-wal"))
                           (pathname (concatenate 'string (namestring path) "-shm"))))
    (when (probe-file candidate) (delete-file candidate))))

(format t "~%== concrete SQLite cognitive memory router ==~%")

(let* ((root (test-state-dir))
       (event-path (merge-pathnames "sqlite-router-genesis-events.sqlite3" root))
       (derived-path (merge-pathnames "sqlite-router-genesis-derived.sqlite3" root))
       (rebuilt-path (merge-pathnames "sqlite-router-genesis-rebuilt.sqlite3" root))
       (events nil) (derived nil) (rebuilt nil)
       (old-search *memory-search-storage-backend*)
       (old-router *memory-cognitive-mutation-router*)
       (old-mode *memory-cognitive-mutation-mode*))
  (dolist (path (list event-path derived-path rebuilt-path))
    (sqlite-router-delete-db path))
  (unwind-protect
      (progn
        (setf events (make-sqlite-storage event-path)
              derived (make-sqlite-derived-storage derived-path))
        (let* ((provenance (sqlite-router-provenance))
               (installed
                 (memory-ledger-install-authority
                  events derived :agent-id "genesis-agent"
                  :initialize-p t :initial-provenance provenance
                  :clock-fn (lambda () 3996288000)
                  :postgres-writer-active-p nil))
               (source (memory-ledger-find-baseline
                        events :agent-id "genesis-agent")))
          (sqlite-router-check
           "explicit genesis seals an empty memory baseline"
           (and (string= "installed" (gethash "status" installed))
                (string= "memory-baseline:empty-genesis:v1"
                         (%memory-baseline-session source))
                (zerop (gethash "node_count" (%memory-baseline-expected source)))
                (zerop (gethash "edge_count" (%memory-baseline-expected source)))))
          (setf *memory-ledger-authority-installed-p* nil
                *memory-search-storage-backend* old-search
                *memory-cognitive-mutation-router* old-router
                *memory-cognitive-mutation-mode* old-mode
                rebuilt (make-sqlite-derived-storage rebuilt-path))
          (let ((reopened
                  (memory-ledger-install-authority
                   events rebuilt :agent-id "genesis-agent"
                   :postgres-writer-active-p nil)))
            (sqlite-router-check
             "empty genesis restarts and rebuilds from the ledger"
             (and (string= "installed" (gethash "status" reopened))
                  (= 1 (memory-storage-operation-next-edge-id rebuilt))
                  (= 2 (memory-storage-operation-vector-dimension rebuilt)))))))
    (setf *memory-ledger-authority-installed-p* nil
          *memory-search-storage-backend* old-search
          *memory-cognitive-mutation-router* old-router
          *memory-cognitive-mutation-mode* old-mode)
    (dolist (backend (list rebuilt derived events))
      (when backend (ignore-errors (storage-close backend))))
    (dolist (path (list event-path derived-path rebuilt-path))
      (sqlite-router-delete-db path))))

(let* ((root (test-state-dir))
       (event-path (merge-pathnames "sqlite-router-events.sqlite3" root))
       (derived-path (merge-pathnames "sqlite-router-derived.sqlite3" root))
       (rebuilt-path (merge-pathnames "sqlite-router-rebuilt.sqlite3" root))
       (events nil) (derived nil) (rebuilt nil))
  (sqlite-router-delete-db event-path)
  (sqlite-router-delete-db derived-path)
  (sqlite-router-delete-db rebuilt-path)
  (unwind-protect
      (progn
        (setf events (make-sqlite-storage event-path)
              derived (make-sqlite-derived-storage derived-path))
        (let* ((import (memory-storage-import-snapshot
                        derived (make-instance 'sqlite-router-source)
                        (sqlite-router-provenance)))
               (bootstrap
                 (nth-value 1
                            (storage-append-event
                             events "fixture-bootstrap"
                             (%memory-storage-object "status" "ready")
                             :agent-id "default" :occurred-at "fixture-time")))
               (storage-id (gethash "storage_id" bootstrap)))
          (memory-storage-bind-projection
           derived :baseline-seal (gethash "provenance_seal_hash" import)
           :storage-id storage-id :agent-id "default"
           :through-event-id 0 :through-position 0 :boundary-hash "empty")
          (let* ((audit (%memory-ledger-baseline-receipt
                         (memory-storage-audit-snapshot derived)))
                 (provenance (sqlite-router-provenance)))
            (storage-append-event
             events "memory-baseline-started"
             (%memory-storage-object
              "schema_version" 1 "session_id" "interrupted-baseline"
              "provenance" provenance "expected_receipt" audit)
             :agent-id "default" :occurred-at "fixture-time")
            (sqlite-router-check
             "interrupted baseline confers no authority"
             (handler-case
                 (progn (memory-ledger-find-baseline
                         events :agent-id "default") nil)
               (memory-ledger-baseline-unavailable () t))))
          (let ((baseline
                  (memory-ledger-write-baseline
                   events derived (sqlite-router-provenance)
                   :agent-id "default" :session-id "fixture-baseline"
                   :source-origin
                   (%memory-storage-object
                    "schema_version" 1 "source_agent_id" "source-agent"
                    "source_kind" "migrated-semantic-memory"
                    "migration_manifest_sha256"
                    "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
                   :clock-fn (lambda () 3996288000))))
            (setf rebuilt (make-sqlite-derived-storage rebuilt-path))
            (let ((rebuild (memory-ledger-rebuild-baseline
                            events rebuilt :agent-id "default")))
              (sqlite-router-check
               "complete hash-closed ledger baseline rebuilds exact private rows"
               (and (string= "committed" (gethash "status" baseline))
                    (string= "rebuilt" (gethash "status" rebuild))
                    (= 2 (gethash "node_count" rebuild))
                    (string=
                     (gethash "scalar_json"
                              (memory-storage-operation-node derived "root-a"))
                     (gethash "scalar_json"
                              (memory-storage-operation-node rebuilt "root-a")))))))
            (sqlite-router-check
             "baseline retains its source-agent migration origin"
             (let* ((source (memory-ledger-find-baseline
                             events :agent-id "default"))
                    (origin (%memory-baseline-source-origin source)))
               (and (string= "source-agent"
                             (gethash "source_agent_id" origin))
                    (string= "migrated-semantic-memory"
                             (gethash "source_kind" origin)))))
          (let* ((audit (%memory-ledger-baseline-receipt
                         (memory-storage-audit-snapshot derived)))
                 (bad (%memory-storage-json-read
                       (%memory-materializer-json audit) :fixture-bad-baseline)))
            (setf (gethash "node_count" bad) (1+ (gethash "node_count" bad)))
            (multiple-value-bind (start start-receipt)
                (storage-append-event
                 events "memory-baseline-started"
                 (%memory-storage-object
                  "schema_version" 1 "session_id" "mismatched-baseline"
                  "provenance" (sqlite-router-provenance)
                  "expected_receipt" audit)
                 :agent-id "default" :occurred-at "fixture-time")
              (declare (ignore start))
              (storage-append-event
               events "memory-baseline-committed"
               (%memory-storage-object
                "schema_version" 1 "session_id" "mismatched-baseline"
                "start_event_id" (gethash "event_id" start-receipt)
                "receipt" bad)
               :agent-id "default" :occurred-at "fixture-time"))
            (sqlite-router-check
             "hash-mismatched later commit cannot replace a valid baseline"
             (string= "fixture-baseline"
                      (%memory-baseline-session
                       (memory-ledger-find-baseline
                        events :agent-id "default")))))
          (sqlite-router-check
           "startup gate installs SQLite only after baseline and reconciliation"
           (let ((installed
                   (memory-ledger-install-authority
                    events rebuilt :agent-id "default"
                    :postgres-writer-active-p nil)))
             (and (string= "installed" (gethash "status" installed))
                  (integerp (gethash "baseline_storage_position" installed))
                  (eq *memory-cognitive-mutation-mode* :event-first)
                  (typep *memory-search-storage-backend*
                         'coordinated-memory-storage)
                  (functionp *memory-cognitive-mutation-router*))))
          (sqlite-router-check
           "operation reads expose verified exact rows and sealed dimension"
           (and (memory-storage-operation-node derived "root-a")
                (= 2 (memory-storage-operation-vector-dimension derived))
                (= 1 (memory-storage-operation-next-edge-id derived))
                (null (memory-storage-operation-edges derived))))
          (let* ((coordinator
                   (make-storage-memory-mutation-coordinator
                    :event-storage events :projection derived
                    :agent-id "default"))
                 (*memory-cognitive-mutation-mode* :event-first)
                 (*memory-search-storage-backend*
                   (make-coordinated-memory-storage coordinator))
                 (*memory-cognitive-mutation-router*
                   (make-sqlite-memory-cognitive-router
                    coordinator :clock-fn (lambda () 3996288000)))
                 (*memory-model-invoke-fn* nil))
            (setf (fdefinition 'embed-text)
                  (lambda (text) (declare (ignore text)) (list 1.0f0 0.0f0))
                  (fdefinition 'embed-retrieval-document)
                  (lambda (text) (declare (ignore text)) (list 1.0f0 0.0f0))
                  (fdefinition 'log-event)
                  (lambda (&rest arguments) (declare (ignore arguments)) 1))
            (sqlite-router-check
             "public admission commits without PostgreSQL authority"
             (string= "thought-1"
                      (memory-admit-node
                       :id "thought-1" :kind "thought" :content "derived alpha"
                       :importance 0.1d0 :origin-class "synthetic"
                       :epistemic-status "hypothesis" :producer "fixture"
                       :confidence 0.8d0 :grounding-status "grounded"
                       :lineage-parent-ids '("root-a"))))
            (sqlite-router-check
             "legacy node seam is contained by event-first admission"
             (string= "legacy-1"
                      (memory-write-node
                       :id "legacy-1" :kind "observation"
                       :content "legacy compatibility memory"
                       :importance 0.1d0)))
            (sqlite-router-check
             "admission materializes its resolved root and lineage edge"
             (let* ((envelope (memory-storage-operation-node derived "thought-1"))
                    (row (%sqlite-memory-router-row envelope))
                    (edges (memory-storage-operation-edges
                            derived :from-id "thought-1"
                            :edge-type "derived-from")))
               (and (equalp #("root-a")
                            (gethash "root_observation_ids" row))
                    (= 1 (length edges))
                    (string= "root-a"
                             (gethash "to_id"
                                      (shasht:read-json (first edges)))))))
            (memory-admit-node
             :id "thought-1" :kind "thought" :content "derived beta"
             :importance 0.1d0 :origin-class "synthetic"
             :epistemic-status "hypothesis" :producer "fixture"
             :confidence 0.8d0 :grounding-status "grounded"
             :lineage-parent-ids '("root-b"))
            (sqlite-router-check
             "re-admission replaces lineage atomically rather than accumulating"
             (let ((edges (memory-storage-operation-edges
                           derived :from-id "thought-1"
                           :edge-type "derived-from")))
               (and (= 1 (length edges))
                    (string= "root-b"
                             (gethash "to_id"
                                      (shasht:read-json (first edges)))))))
            (sqlite-router-check
             "public supersession updates the node and inserts one typed edge"
             (and (memory-supersede "root-a" "thought-1"
                                    :reason "new evidence" :actor "fixture")
                  (= 1 (length
                        (memory-storage-operation-edges
                         derived :from-id "thought-1" :to-id "root-a"
                         :edge-type "supersedes")))))
            (sqlite-router-check
             "legacy direct edge and graph reads stay on SQLite"
             (and (memory-add-edge "thought-1" "root-b" "contains")
                  ;; A duplicate is a reconciled no-op, not a second event.
                  (memory-add-edge "thought-1" "root-b" "contains")
                  (memory-get-node "thought-1")
                  (= 1 (length (memory-edges-from "thought-1" "contains")))
                  (= 1 (length (memory-edges-to "root-b" "contains")))))
            (sqlite-router-check
             "quarantine is an exact event-first node replacement"
             (and (memory-quarantine "root-b" :reason "fixture" :actor "test")
                  (gethash
                   "quarantined"
                   (%sqlite-memory-router-row
                    (memory-storage-operation-node derived "root-b")))))
            (sqlite-router-check
             "public rehearsal updates only existing requested nodes"
             (= 1 (gethash
                   "updated_count"
                   (memory-record-use
                    '("root-a" "missing") :consumer "public-response"
                    :generation-id "generation-1" :user-visible-p t))))
            (sqlite-router-check
             "empty rehearsal is reconciled but does not append a no-op event"
             (zerop (gethash
                     "updated_count"
                     (memory-record-use
                      '("still-missing") :consumer "public-response"
                      :generation-id "generation-1" :user-visible-p t))))
            (let* ((memory (%memory-storage-object
                            "id" "tick-1" "kind" "thought"
                            "content" "tick-derived alpha" "importance" 0.1d0
                            "origin_class" "synthetic"
                            "record_type" "hypothesis"
                            "grounding_status" "grounded"
                            "evidence_node_ids" (vector "root-a")))
                   (proposal (%memory-storage-object
                              "tick_type" "idle-drift"
                              "memory_specs" (vector memory)
                              "edge_specs" (vector)
                              "generation_id" "generation-2")))
              (sqlite-router-check
               "default tick transaction commits through the same authority"
               (equal '("tick-1")
                      (%tick-commit-default-transaction proposal 77))))
            (let ((envelope (memory-storage-operation-node derived "legacy-1")))
              (storage-append-event
               events "memory-node-state"
               (memory-materialize-node-quarantine
                (gethash "scalar_json" envelope)
                (gethash "embedding_binary_hex" envelope)
                (gethash "retrieval_embedding_binary_hex" envelope)
                "tail fixture" "test")
               :agent-id "default" :occurred-at "fixture-time")
              (sqlite-router-check
               "installed read facade reconciles an unapplied durable tail"
               (gethash "quarantined" (memory-get-node "legacy-1"))))
            (sqlite-router-check
             "all cognitive writes are durable operation events and reconcile cleanly"
             (let ((count 0))
               (multiple-value-bind (complete head visited)
                   (storage-map-event-receipts
                    events (lambda (receipt)
                             (declare (ignore receipt)) (incf count))
                    :agent-id "default"
                    :event-types '("memory-operation-state"))
                 (declare (ignore head visited))
                 (and complete (= 8 count)
                      (zerop (gethash
                              "applied_count"
                              (memory-mutation-coordinator-reconcile
                               coordinator)))))))
            (let ((rebuilt-coordinator
                    (make-storage-memory-mutation-coordinator
                     :event-storage events :projection rebuilt
                     :agent-id "default")))
              (sqlite-router-check
               "baseline plus authoritative tail reaches the same final nodes"
               (and (= 9 (gethash
                          "applied_count"
                          (memory-mutation-coordinator-reconcile
                           rebuilt-coordinator)))
                    (memory-storage-operation-node rebuilt "thought-1")
                    (memory-storage-operation-node rebuilt "tick-1")))))))
    (when events (ignore-errors (storage-close events)))
    (when derived (ignore-errors (storage-close derived)))
    (when rebuilt (ignore-errors (storage-close rebuilt)))
    (sqlite-router-delete-db event-path)
    (sqlite-router-delete-db derived-path)
    (sqlite-router-delete-db rebuilt-path)))

(format t "~%~d passed, ~d failed~%" *sqlite-router-pass* *sqlite-router-fail*)
(when (plusp *sqlite-router-fail*)
  (error "SQLite cognitive memory router tests failed"))
