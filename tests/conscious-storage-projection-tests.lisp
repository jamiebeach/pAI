(in-package :agent)

(ql:quickload '(:cffi :shasht :ironclad :bordeaux-threads) :silent t)

(defvar *csp-pass* 0)
(defvar *csp-fail* 0)

(defun csp-check (name condition)
  (if condition
      (progn (incf *csp-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *csp-fail*) (format t "  FAIL ~a~%" name))))

(defun csp-signals-p (condition-type thunk)
  (handler-case (progn (funcall thunk) nil)
    (condition (actual) (typep actual condition-type))))

(defun csp-delete-db (path)
  (dolist (candidate (list path
                           (pathname (concatenate 'string (namestring path) "-wal"))
                           (pathname (concatenate 'string (namestring path) "-shm"))))
    (when (probe-file candidate) (delete-file candidate))))

(defun csp-write-jsonl (path events)
  (with-open-file (output path :direction :output :if-exists :supersede
                               :if-does-not-exist :create
                               :external-format :utf-8)
    (dolist (event events) (write-line (%storage-json event) output))))

(defun csp-read-jsonl (path)
  (with-open-file (input path :direction :input :external-format :utf-8)
    (loop for line = (read-line input nil nil) while line
          collect (shasht:read-json line))))

(defun csp-bundle= (left right)
  (string= (%conscious-storage-canonical-hash left)
           (%conscious-storage-canonical-hash right)))

(dolist (file '("policy.lisp" "stimulus.lisp" "census.lisp" "concern.lisp"
                "codelets.lisp" "context.lisp" "inbox.lisp" "attention.lisp"
                "mind/conscious/lifecycle.lisp" "lifecycle-semantics.lisp" "state.lisp"
                "storage-substrate.lisp" "sqlite-storage.lisp"
                "sqlite-import.lisp" "storage-projection.lisp"))
  (load (test-source file)))

(format t "~%== conscious storage projection checkpoint ==~%")
(csp-check "full projection entry point is present"
           (fboundp 'conscious-storage-full-project))
(csp-check "checkpoint entry point is present"
           (fboundp 'conscious-storage-build-checkpoint))
(csp-check "tail restore entry point is present"
           (fboundp 'conscious-storage-restore-checkpoint-tail))

(dolist (name (codelet-names)) (unregister-codelet name))
(register-codelet
 "checkpoint-direct" 10
 (lambda (stimulus context)
   (declare (ignore context))
   (when (string= "user-message" (gethash "kind" stimulus ""))
     (make-assessment
      :codelet "checkpoint-direct" :concern "operator"
      :stimulus-id (gethash "stimulus_id" stimulus)
      :evidence-ids (coerce (gethash "source_event_ids" stimulus) 'list)
      :priority-class "direct" :urgency "interactive"
      :explanation-code "checkpoint-user-message")))
 :digest "checkpoint-direct-v1")

(let* ((root (test-state-dir))
       (database (merge-pathnames "conscious-storage.sqlite3" root))
       (empty-database (merge-pathnames "conscious-storage-empty.sqlite3" root))
       (jsonl (merge-pathnames "conscious-storage.jsonl" root))
       (backend nil) (empty-backend nil) (events nil)
       (agent-id "projection-dev") (now 5000))
  (csp-delete-db database)
  (csp-delete-db empty-database)
  (when (probe-file jsonl) (delete-file jsonl))
  (unwind-protect
      (progn
        (setf backend (make-sqlite-storage database))
        (flet ((append-event (type payload &key caused-by)
                 (let ((event
                         (storage-append-event
                          backend type payload :agent-id agent-id
                          :occurred-at
                          (format nil "2026-08-19T14:00:~2,'0dZ"
                                  (length events))
                          :caused-by (or caused-by :null))))
                   (setf events (append events (list event)))
                   event)))
          (append-event
           "user-message"
           (obj "text" "first" "channel" "cli"
                "origin_runtime_revision" "conscious-q5-v2"))
          (append-event "heap-health"
                        (obj "status" "ok"
                             "irrelevant" (make-string 20000 :initial-element #\x)))
          (append-event
           "agent-message"
           (obj "authorization_kind" "solicited-publication-candidate"
                "metadata"
                (obj "source" "q4.5-conversation"
                     "publication_validation" "accepted")
                "content" (make-string 8000 :initial-element #\y))
           :caused-by 1)
          (let* ((source
                   (append-event
                    "near-term-intention-created"
                    (obj "schema_version" 1 "intention_id" "i1"
                         "receipt_id" "r1" "state" "seeded"
                         "pass_count" 0 "detail" :null)))
                 (source-id (gethash "id" source)))
            (append-event
             "conscious-lifecycle-transition"
             (conscious-lifecycle-transition-payload
              "near-term:i1" "open" :request-id "open:i1"
              :lifecycle-kind "deferred-intention"
              :origin-runtime-revision "near-term-intentions-v1"
              :actor-runtime-revision "conscious-q5-v2"
              :source-event-id source-id :reason-code "near-term-seeded"
              :occurred-at 1005)))
          (append-event "conscious-lifecycle-semantic-described" (obj))
          (csp-write-jsonl jsonl events)
          (let* ((jsonl-before
                   (%conscious-storage-project-events
                    (csp-read-jsonl jsonl) agent-id now))
                 (sqlite-before (conscious-storage-full-project
                                 backend :agent-id agent-id :now now)))
            (csp-check "full JSONL and full SQLite projections match"
                       (csp-bundle= jsonl-before sqlite-before))
            (multiple-value-bind (checkpoint-report checkpoint-bundle)
                (conscious-storage-build-checkpoint
                 backend :agent-id agent-id :now now)
              (csp-check "capsule projection equals full projection"
                         (csp-bundle= sqlite-before checkpoint-bundle))
              (csp-check "large journal and reply bodies are compacted"
                         (and (> (gethash "stubbed_event_count"
                                            checkpoint-report) 0)
                              (< (gethash "capsule_event_json_bytes"
                                          checkpoint-report)
                                 (/ (gethash "source_json_bytes"
                                             checkpoint-report) 2))))
              (csp-check "checkpoint binds logical and physical watermarks"
                         (and (= 6 (gethash "through_event_id"
                                            checkpoint-report))
                              (= 6 (gethash "through_storage_position"
                                            checkpoint-report))))))
          (append-event
           "stimulus-consumed"
           (obj "agent_id" agent-id "consumer" "conscious-state"
                "stimulus_ids" (vector "stimulus:1")
                "disposition" "handled"))
          (append-event
           "user-message"
           (obj "text" "second" "channel" "cli"
                "origin_runtime_revision" "conscious-q5-v2"))
          (append-event "heap-health"
                        (obj "status" "ok" "irrelevant" "tail"))
          (csp-write-jsonl jsonl events)
          (let* ((jsonl-after
                   (%conscious-storage-project-events
                    (csp-read-jsonl jsonl) agent-id (+ now 10)))
                 (sqlite-after (conscious-storage-full-project
                                backend :agent-id agent-id :now (+ now 10))))
            (multiple-value-bind (restored restore-report)
                (conscious-storage-restore-checkpoint-tail
                 backend :agent-id agent-id :now (+ now 10))
              (csp-check "later full JSONL and SQLite projections match"
                         (csp-bundle= jsonl-after sqlite-after))
              (csp-check "checkpoint plus physical tail equals full replay"
                         (and (csp-bundle= sqlite-after restored)
                              (= 3 (gethash "tail_event_count" restore-report))))
              (csp-check "tail starts strictly after checkpoint position"
                         (and (= 6 (gethash "checkpoint_storage_position"
                                            restore-report))
                              (= 9 (gethash "through_storage_position"
                                            restore-report))))))
          (register-codelet
           "composition-change" 20 (lambda (s c) (declare (ignore s c)) nil)
           :digest "composition-change-v1")
          (csp-check "composition change invalidates checkpoint reuse"
                     (csp-signals-p
                      'storage-conflict-error
                      (lambda ()
                        (conscious-storage-restore-checkpoint-tail
                         backend :agent-id agent-id :now (+ now 20)))))
          (unregister-codelet "composition-change"))
        (setf empty-backend (make-sqlite-storage empty-database))
        (csp-check "missing checkpoint fails closed"
                   (csp-signals-p
                    'storage-conflict-error
                    (lambda ()
                      (conscious-storage-restore-checkpoint-tail
                       empty-backend :agent-id agent-id :now now)))))
    (when backend (ignore-errors (storage-close backend)))
    (when empty-backend (ignore-errors (storage-close empty-backend)))
    (csp-delete-db database)
    (csp-delete-db empty-database)
    (when (probe-file jsonl) (delete-file jsonl))))

;; A checkpoint must scale with facts that can still affect a future tail,
;; not with the number of already-handled conversation turns.
(let* ((root (test-state-dir))
       (database (merge-pathnames "conscious-storage-bounded.sqlite3" root))
       (backend nil)
       (agent-id "bounded-dev")
       (now 9000)
       (historical-source-id nil)
       (outstanding-id nil))
  (csp-delete-db database)
  (unwind-protect
      (progn
        (setf backend (make-sqlite-storage database))
        (dotimes (index 200)
          (let* ((message
                   (storage-append-event
                    backend "user-message"
                    (obj "text" (format nil "handled-~d" index)
                         "channel" "cli"
                         "origin_runtime_revision" "conscious-q5-v2")
                    :agent-id agent-id))
                 (message-id (gethash "id" message)))
            (storage-append-event
             backend "agent-message"
             (obj "authorization_kind" "solicited-publication-candidate"
                  "metadata"
                  (obj "source" "q4.5-conversation"
                       "publication_validation" "accepted")
                  "content" (make-string 1024 :initial-element #\z))
             :agent-id agent-id :caused-by message-id)))
        (setf historical-source-id
              (gethash
               "id"
               (storage-append-event
                backend "heap-health" (obj "status" "historical-source")
                :agent-id agent-id)))
        (storage-append-event
         backend "heap-health" (obj "status" "gap-sentinel")
         :agent-id agent-id)
        (setf outstanding-id
              (gethash
               "id"
               (storage-append-event
                backend "user-message"
                (obj "text" "still outstanding" "channel" "cli"
                     "origin_runtime_revision" "conscious-q5-v2")
                :agent-id agent-id)))
        (multiple-value-bind (report ignored)
            (conscious-storage-build-checkpoint
             backend :agent-id agent-id :now now)
          (declare (ignore ignored))
          (let* ((checkpoint
                   (storage-load-checkpoint
                    backend *conscious-storage-checkpoint-name*
                    :agent-id agent-id))
                 (capsule (gethash "state" checkpoint))
                 (capsule-count (length (gethash "events" capsule))))
            (csp-check "handled conversation history has bounded capsule state"
                       (and (= 403 (gethash "event_count" report))
                            (< capsule-count 20)))))
        (storage-append-event
         backend "conscious-lifecycle-transition"
         (conscious-lifecycle-transition-payload
          "tail:l1" "open" :request-id "tail:open:l1"
          :lifecycle-kind "deferred-intention"
          :origin-runtime-revision "near-term-intentions-v1"
          :actor-runtime-revision "conscious-q5-v2"
          :source-event-id historical-source-id :reason-code "near-term-seeded"
          :occurred-at (+ now 1))
         :agent-id agent-id)
        (storage-append-event
         backend "agent-message"
         (obj "authorization_kind" "solicited-publication-candidate"
              "metadata"
              (obj "source" "q4.5-conversation"
                   "publication_validation" "accepted")
              "content" "handled after checkpoint")
         :agent-id agent-id :caused-by outstanding-id)
        (storage-append-event
         backend "user-message"
         (obj "text" "new tail barrier" "channel" "cli"
              "origin_runtime_revision" "conscious-q5-v2")
         :agent-id agent-id)
        (let ((full (conscious-storage-full-project
                     backend :agent-id agent-id :now (+ now 10))))
          (multiple-value-bind (restored report)
              (conscious-storage-restore-checkpoint-tail
               backend :agent-id agent-id :now (+ now 10))
            (csp-check "bounded prefix plus tail equals later full replay"
                       (csp-bundle= full restored))
            (csp-check "tail point-hydrates an omitted historical source"
                       (= 1 (gethash "hydrated_prefix_reference_count"
                                     report))))))
    (when backend (ignore-errors (storage-close backend)))
    (csp-delete-db database)))

(let* ((root (test-state-dir))
       (database (merge-pathnames "conscious-storage-mismatch.sqlite3" root))
       (backend nil)
       (original (symbol-function '%conscious-storage-summarize-prefix)))
  (csp-delete-db database)
  (unwind-protect
      (progn
        (setf backend (make-sqlite-storage database))
        (storage-append-event
         backend "user-message"
         (obj "text" "must remain" "channel" "cli"
              "origin_runtime_revision" "conscious-q5-v2")
         :agent-id "mismatch-dev")
        (setf (symbol-function '%conscious-storage-summarize-prefix)
              (lambda (events agent-id now)
                (declare (ignore events agent-id now))
                (values nil 0 0)))
        (csp-check "summary mismatch cannot publish a checkpoint"
                   (and (csp-signals-p
                         'storage-integrity-error
                         (lambda ()
                           (conscious-storage-build-checkpoint
                            backend :agent-id "mismatch-dev" :now 10000)))
                        (null (storage-load-checkpoint
                               backend *conscious-storage-checkpoint-name*
                               :agent-id "mismatch-dev")))))
    (setf (symbol-function '%conscious-storage-summarize-prefix) original)
    (when backend (ignore-errors (storage-close backend)))
    (csp-delete-db database)))

(format t "~%~d passed, ~d failed~%" *csp-pass* *csp-fail*)
(when (plusp *csp-fail*) (error "conscious storage projection tests failed"))
