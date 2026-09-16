;;;; conversation-episode-graph-storage-tests.lisp -- KG1 persisted owner.

(in-package :agent)

(ql:quickload '(:cffi :shasht :ironclad :bordeaux-threads) :silent t)

(dolist (file '("storage-substrate.lisp" "memory-storage.lisp"
                "sqlite-storage.lisp" "sqlite-derived-storage.lisp"
                "conversation-episode-graph.lisp"
                "conversation-episode-graph-storage.lisp"))
  (load (test-source file)))

(defvar *cegs-pass* 0)
(defvar *cegs-fail* 0)

(defun cegs-check (name condition)
  (if condition
      (progn (incf *cegs-pass*) (format t "PASS ~a~%" name))
      (progn (incf *cegs-fail*) (format t "FAIL ~a~%" name))))

(defun cegs-signals-p (type thunk)
  (handler-case (progn (funcall thunk) nil)
    (condition (actual) (typep actual type))))

(defun cegs-delete-db (path)
  (dolist (candidate
           (list path
                 (pathname (concatenate 'string (namestring path) "-wal"))
                 (pathname (concatenate 'string (namestring path) "-shm"))))
    (when (probe-file candidate) (delete-file candidate))))

(defun cegs-obj (&rest pairs)
  (let ((object (make-hash-table :test #'equal)))
    (loop for (key value) on pairs by #'cddr
          do (setf (gethash key object) value))
    object))

(defun cegs-seal (event-id episode-id source-start synopsis concepts)
  (cegs-obj
   "id" event-id "type" "conversation-episode-sealed"
   "agent_id" "graph-agent" "timestamp" "2026-08-30T12:00:00Z"
   "payload"
   (cegs-obj
    "schema_version" 1 "episode_id" episode-id "persona_id" "graph-persona"
    "first_event_id" source-start "last_event_id" (1+ source-start)
    "first_timestamp" source-start "last_timestamp" (1+ source-start)
    "source_event_ids" (vector source-start (1+ source-start))
    "synopsis" synopsis "subjects" (vector)
    "entities" (vector) "retrieval_cues" (coerce concepts 'vector)
    "broader_categories" (vector "shared interest")
    "unresolved_threads" (vector))))

(format t "~%== conversation episode graph storage ==~%")

(let* ((database (merge-pathnames "conversation-graph.sqlite3"
                                  (test-state-dir)))
       (backend nil)
       (events
         (list (cegs-seal 20 "episode:one" 10
                          "The first persisted episode." '("first topic"))
               (cegs-seal 30 "episode:two" 12
                          "The second persisted episode." '("second topic"))))
       (episodes (conversation-episode-project
                  events "graph-agent" "graph-persona"))
       (materialization
         (conversation-episode-graph-materialization
          episodes "graph-agent" "graph-persona")))
  (cegs-delete-db database)
  (unwind-protect
      (progn
        (setf backend (make-sqlite-derived-storage database))
        (let ((report
                (conversation-episode-graph-persist
                 backend materialization
                 :through-event-id 30 :through-position 40
                 :event-storage-id "fixture-ledger"
                 :boundary-hash "fixture-boundary")))
          (cegs-check "atomic persistence reports bounded content-free counts"
                      (and (string= "persisted" (gethash "status" report))
                           (= (length (gethash "nodes" materialization))
                              (gethash "node_count" report))
                           (= 0 (gethash "event_write_count" report -1))
                           (= 0 (gethash "memory_write_count" report -1)))))
        (multiple-value-bind (restored report)
            (conversation-episode-graph-restore
             backend "graph-agent" "graph-persona"
             :event-storage-id "fixture-ledger")
          (cegs-check "warm restore reconstructs exact canonical episodes"
                      (string= (shasht:write-json episodes nil)
                               (shasht:write-json restored nil)))
          (cegs-check "warm restore verifies checkpoint and graph digest"
                      (and (string= "restored" (gethash "status" report))
                           (= 40 (gethash "through_storage_position" report))))
          (cegs-check "restored graph preserves recall identity"
                      (string=
                       (shasht:write-json
                        (conversation-episode-recall episodes "first topic") nil)
                       (shasht:write-json
                        (conversation-episode-recall restored "first topic") nil))))
        (let ((before
                (conversation-episode-graph-inspect
                 backend "graph-agent" "graph-persona"
                 :event-storage-id "fixture-ledger")))
          (cegs-check
           "backward checkpoint is rejected without replacing graph rows"
           (and
            (cegs-signals-p
             'storage-conflict-error
             (lambda ()
               (conversation-episode-graph-persist
                backend materialization
                :through-event-id 29 :through-position 39
                :event-storage-id "fixture-ledger"
                :boundary-hash "older-boundary")))
            (equal (gethash "graph_digest" before)
                   (gethash "graph_digest"
                            (conversation-episode-graph-inspect
                             backend "graph-agent" "graph-persona"
                             :event-storage-id "fixture-ledger"))))))
        (%sqlite-exec
         (%sqlite-derived-handle backend :fixture-corrupt-graph)
         "UPDATE pai_knowledge_graph_nodes SET payload_json='{}' WHERE node_kind='episode' AND canonical_key='episode:one'"
         :fixture-corrupt-graph)
        (cegs-check "row corruption fails restore closed"
                    (cegs-signals-p
                     'storage-integrity-error
                     (lambda ()
                       (conversation-episode-graph-restore
                        backend "graph-agent" "graph-persona"
                        :event-storage-id "fixture-ledger")))))
    (when backend (ignore-errors (storage-close backend)))
    (cegs-delete-db database)))

(format t "~%~d passed, ~d failed~%" *cegs-pass* *cegs-fail*)
(when (plusp *cegs-fail*)
  (error "Conversation episode graph storage tests failed"))
