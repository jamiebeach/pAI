;;;; harness: full-system
;;;; Sparse, source-bound attention input parity and recovery.
(in-package :agent)

(defun sas-full-events (source)
  (let ((events nil))
    (storage-map-events
     source (lambda (event position)
              (declare (ignore position))
              (push event events))
     :agent-id "attention-fixture")
    (nreverse events)))

(defun sas-build (derived source)
  (let ((revision (conscious-attention-shadow-policy-revision)))
    (loop for row = (storage-shadow-attention-apply-page
                     derived source #'conscious-attention-shadow-select
                     :agent-id "attention-fixture" :policy-revision revision
                     :limit 2)
          until (= (gethash "through_position" row)
                   (storage-head-position source :agent-id "attention-fixture"))
          finally (return row))))

(defun sas-build-conscious-inputs (derived source)
  (sas-build derived source)
  (loop for row =
          (storage-shadow-lifecycle-apply-page
           derived source #'conscious-lifecycle-shadow-step
           :agent-id "attention-fixture" :limit 2)
        until (= (gethash "through_position" row)
                 (storage-head-position
                  source :agent-id "attention-fixture")))
  (loop for row =
          (storage-shadow-conscious-pulse-apply-page
           derived source :agent-id "attention-fixture" :limit 2)
        until (= (gethash "through_position" row)
                 (storage-head-position
                  source :agent-id "attention-fixture"))))

(defun sas-parity (derived source)
  (let ((context (make-projection-context
                  :now 4000000000 :agent-id "attention-fixture"
                  :runtime-revision "fixture-v1" :consumer "fixture")))
    (equalp (inbox-project (sas-full-events source) :context context)
            (inbox-project
             (conscious-attention-shadow-events
              derived source "attention-fixture" :limit 2)
             :context context))))

(let ((checks 0))
  (flet ((check (label value)
           (unless value (error "Attention shadow failed: ~a" label))
           (incf checks)))
    (let* ((user (obj "schema_version" 2 "id" 1
                      "timestamp" "2026-01-01T00:00:00Z"
                      "type" "user-message" "agent_id" "attention-fixture"
                      "payload" (obj "text" "synthetic-root")))
           (draft (obj "schema_version" 2 "id" 2
                       "timestamp" "2026-01-01T00:00:01Z"
                       "type" "agent-message" "agent_id" "attention-fixture"
                       "caused_by" 1
                       "payload" (obj "text" "draft" "final" nil)))
           (private (obj "schema_version" 2 "id" 3
                         "timestamp" "2026-01-01T00:00:02Z"
                         "type" "agent-message"
                         "agent_id" "attention-fixture" "caused_by" 1
                         "payload"
                         (obj "text" "private-finding"
                              "authorization_kind" "private-finding-evidence")))
           (published (obj "schema_version" 2 "id" 4
                           "timestamp" "2026-01-01T00:00:03Z"
                           "type" "agent-message"
                           "agent_id" "attention-fixture" "caused_by" 1
                           "payload" (obj "text" "public" "final" t)))
           (legacy (obj "schema_version" 2 "id" 5
                        "timestamp" "2026-01-01T00:00:04Z"
                        "type" "agent-message" "caused_by" 1
                        "payload" (obj "text" "legacy-public")))
           (foreign (obj "schema_version" 2 "id" 6
                         "timestamp" "2026-01-01T00:00:05Z"
                         "type" "agent-message" "agent_id" "other"
                         "caused_by" 1
                         "payload" (obj "text" "not-this-agent")))
           (context (make-projection-context
                     :now 4000000000 :agent-id "attention-fixture")))
      (check "draft and private finding leave barrier pending"
             (= 1 (gethash "admitted_count"
                           (inbox-project (list user draft private)
                                          :context context))))
      (check "final public reply retires barrier"
             (= 1 (gethash "consumed_count"
                           (inbox-project
                            (list user draft private published)
                            :context context))))
      (check "legacy partition-assumed public reply retires barrier"
             (= 1 (gethash "consumed_count"
                           (inbox-project (list user legacy)
                                          :context context))))
      (check "compacted legacy null partition retires barrier"
             (= 1 (gethash "consumed_count"
                           (inbox-project
                            (list user
                                  (conscious-attention-shadow-select legacy))
                            :context context))))
      (check "explicit foreign reply cannot retire barrier"
             (= 1 (gethash "admitted_count"
                           (inbox-project (list user foreign)
                                          :context context)))))
    (let* ((user (obj "schema_version" 2 "id" 21
                      "timestamp" "2026-01-01T00:00:00Z"
                      "type" "user-message" "agent_id" "attention-fixture"
                      "payload" (obj "text" "tool-root")))
           (call (obj "schema_version" 2 "id" 22
                      "timestamp" "2026-01-01T00:00:01Z"
                      "type" "tool-call" "agent_id" "attention-fixture"
                      "caused_by" 21 "payload" (obj "name" "fixture")))
           (result (obj "schema_version" 2 "id" 23
                        "timestamp" "2026-01-01T00:00:02Z"
                        "type" "tool-result"
                        "agent_id" "attention-fixture" "caused_by" 22
                        "payload" (obj "content" "fixture-result")))
           (reply (obj "schema_version" 2 "id" 24
                       "timestamp" "2026-01-01T00:00:03Z"
                       "type" "agent-message"
                       "agent_id" "attention-fixture" "caused_by" 21
                       "payload" (obj "text" "published" "final" t)))
           (late (obj "schema_version" 2 "id" 25
                      "timestamp" "2026-01-01T00:00:04Z"
                      "type" "tool-result"
                      "agent_id" "attention-fixture" "caused_by" 22
                      "payload" (obj "content" "late-result")))
           (context (make-projection-context
                     :now 4000000000 :agent-id "attention-fixture")))
      (check "tool result is pending before public reply"
             (= 2 (gethash "admitted_count"
                           (inbox-project (list user call result)
                                          :context context))))
      (check "published reply settles prior causal tool result"
             (= 2 (gethash "consumed_count"
                           (inbox-project (list user call result reply)
                                          :context context))))
      (check "late tool result stays pending"
             (= 1 (gethash "admitted_count"
                           (inbox-project (list user call result reply late)
                                          :context context)))))
    (uiop:call-with-temporary-file
     (lambda (source-path)
       (uiop:call-with-temporary-file
        (lambda (derived-path)
          (let ((source (make-sqlite-storage source-path))
                (derived (make-sqlite-derived-storage derived-path)))
            (unwind-protect
                 (progn
                   (storage-shadow-attention-prepare derived)
                   (check "empty source parity" (progn (sas-build derived source)
                                                       (sas-parity derived source)))
                   (storage-append-event source "journal-only" (obj "n" 1)
                                         :agent-id "attention-fixture")
                   (let ((first (storage-append-event
                                 source "user-message" (obj "text" "fixture-one")
                                 :agent-id "attention-fixture")))
                     (storage-append-event source "journal-only" (obj "n" 2)
                                           :agent-id "attention-fixture")
                     (storage-append-event
                      source "stimulus-consumed"
                      (obj "agent_id" "attention-fixture"
                           "consumer" "fixture" "disposition" "handled"
                           "stimulus_ids"
                           (vector (format nil "stimulus:~a"
                                           (gethash "id" first))))
                      :agent-id "attention-fixture"))
                   (storage-append-event source "journal-only" (obj "n" 3)
                                         :agent-id "attention-fixture")
                   (storage-append-event source "user-message"
                                         (obj "text" "fixture-two")
                                         :agent-id "attention-fixture")
                   (storage-append-event source "journal-only" (obj "n" 4)
                                         :agent-id "attention-fixture")
                   (sas-build derived source)
                   (check "unique native event has physical position"
                          (= 6 (storage-event-position
                                source "attention-fixture" 6)))
                   (check "selected count"
                          (= 3 (gethash "selected_count"
                                        (storage-shadow-attention-report
                                         derived source
                                         :agent-id "attention-fixture"
                                         :policy-revision
                                         (conscious-attention-shadow-policy-revision)))))
                   (check "sparse projection parity" (sas-parity derived source))
                   (storage-close derived)
                   (storage-close source)
                   (setf source (make-sqlite-storage source-path)
                         derived (make-sqlite-derived-storage derived-path))
                   (check "fresh-process-equivalent reopen parity"
                          (sas-parity derived source))
                   (storage-append-event source "journal-only" (obj "n" 5)
                                         :agent-id "attention-fixture")
                   (check "stale cursor refuses read"
                          (handler-case
                              (progn
                                (conscious-attention-shadow-events
                                 derived source "attention-fixture")
                                nil)
                            (storage-conflict-error () t)))
                   (sas-build derived source)
                   (check "journal-only tail parity" (sas-parity derived source))
                   ;; A pre-emptive acknowledgement cannot retire a later
                   ;; barrier. An authorized legacy reply can retire it only
                   ;; after its causal source has been observed.
                   (storage-append-event
                    source "stimulus-consumed"
                    (obj "agent_id" "attention-fixture"
                         "consumer" "fixture" "disposition" "handled"
                         "stimulus_ids" (vector "stimulus:10"))
                    :agent-id "attention-fixture")
                   (let ((third (storage-append-event
                                 source "user-message"
                                 (obj "text" "fixture-three")
                                 :agent-id "attention-fixture")))
                     (check "preempted target is expected source"
                            (= 10 (gethash "id" third)))
                     (storage-append-event
                      source "agent-message"
                      (obj "text" "synthetic-reply"
                           "authorization_kind"
                           "solicited-publication-candidate"
                           "metadata"
                           (obj "source" "q4.5-conversation"
                                "publication_validation" "accepted"))
                      :agent-id "attention-fixture"
                      :caused-by (gethash "id" third)))
                   (storage-append-event source "journal-only" (obj "n" 6)
                                         :agent-id "attention-fixture")
                   (sas-build derived source)
                   (check "causal and legacy reply parity"
                          (sas-parity derived source))
                   (let ((before
                           (gethash "selected_count"
                                    (storage-shadow-attention-report
                                     derived source
                                     :agent-id "attention-fixture"
                                     :policy-revision
                                     (conscious-attention-shadow-policy-revision)))))
                     (storage-append-event
                      source "agent-message"
                      (obj "text" "legacy-final" "final" t)
                      :agent-id "attention-fixture" :caused-by 10)
                     (storage-append-event
                      source "agent-message"
                      (obj "text" "draft-only" "final" nil)
                      :agent-id "attention-fixture" :caused-by 10)
                     (storage-append-event
                      source "agent-message" (obj "text" "legacy-fallback")
                      :agent-id "attention-fixture" :caused-by 10)
                     (storage-append-event
                      source "agent-message"
                      (obj "text" "recursive-public"
                           "authorization_kind" "recursive-solicited-reply"
                           "authorization_id" "fixture-model"
                           "metadata" (obj "source" "recursive-mind-v1"))
                      :agent-id "attention-fixture" :caused-by 10)
                     (storage-append-event
                      source "agent-message"
                      (obj "text" "private-evidence"
                           "authorization_kind" "private-finding-evidence"
                           "metadata"
                           (obj "source"
                                "recursive-curiosity-incorporation-v1"))
                      :agent-id "attention-fixture" :caused-by 10)
                     (sas-build derived source)
                     (check "only published reply shapes selected"
                            (= (+ before 3)
                               (gethash "selected_count"
                                        (storage-shadow-attention-report
                                         derived source
                                         :agent-id "attention-fixture"
                                         :policy-revision
                                         (conscious-attention-shadow-policy-revision)))))
                     (check "legacy and recursive reply parity"
                            (sas-parity derived source)))
                   (let* ((root (storage-append-event
                                 source "user-message"
                                 (obj "text" "fixture-tool-root")
                                 :agent-id "attention-fixture"))
                          (call (storage-append-event
                                 source "tool-call"
                                 (obj "name" "fixture" "arguments" "{}")
                                 :agent-id "attention-fixture"
                                 :caused-by (gethash "id" root))))
                     (storage-append-event
                      source "tool-result" (obj "content" "before-reply")
                      :agent-id "attention-fixture"
                      :caused-by (gethash "id" call))
                     (storage-append-event
                      source "agent-message"
                      (obj "text" "final-reply" "final" t)
                      :agent-id "attention-fixture"
                      :caused-by (gethash "id" root))
                     (storage-append-event
                      source "tool-result" (obj "content" "after-reply")
                      :agent-id "attention-fixture"
                      :caused-by (gethash "id" call)))
                   (sas-build derived source)
                   (check "causal tool result shadow parity"
                          (sas-parity derived source))
                   (check "wrong selector revision fails closed"
                          (handler-case
                              (progn
                                (storage-shadow-attention-report
                                 derived source :agent-id "attention-fixture"
                                 :policy-revision "not-the-selector")
                                nil)
                            (storage-conflict-error () t)))
                   (let ((handle (%sqlite-derived-handle derived :fixture)))
                     (%sqlite-exec
                      handle
                      "DELETE FROM pai_attention_v3_events WHERE agent_id='attention-fixture' AND selected_ordinal=2"
                      :fixture))
                   (check "deleted middle row fails closed"
                          (handler-case
                              (progn
                                (conscious-attention-shadow-events
                                 derived source "attention-fixture")
                                nil)
                            (storage-integrity-error () t))))
              (storage-close derived)
              (storage-close source))))
        :want-stream-p nil :type "sqlite"))
     :want-stream-p nil :type "sqlite")
    (uiop:call-with-temporary-file
     (lambda (jsonl-path)
       (with-open-file (stream jsonl-path :direction :output
                                      :if-exists :supersede)
         (dolist (event
                  (list (obj "schema_version" 2 "id" 20
                             "timestamp" "2026-01-01T00:00:00Z"
                             "type" "journal-only"
                             "agent_id" "attention-fixture"
                             "payload" (obj))
                        (obj "schema_version" 2 "id" 11
                             "timestamp" "2026-01-01T00:00:01Z"
                             "type" "user-message"
                             "agent_id" "attention-fixture"
                             "payload" (obj "text" "imported-one"))
                        (obj "schema_version" 2 "id" 11
                             "timestamp" "2026-01-01T00:00:02Z"
                             "type" "journal-only"
                             "agent_id" "attention-fixture"
                             "payload" (obj))
                        (obj "schema_version" 2 "id" 12
                             "timestamp" "2026-01-01T00:00:03Z"
                             "type" "user-message"
                             "agent_id" "attention-fixture"
                             "payload" (obj "text" "imported-two"))))
           (write-line (%storage-json event) stream)))
       (uiop:call-with-temporary-file
        (lambda (source-path)
          (uiop:call-with-temporary-file
           (lambda (derived-path)
             (let ((source (make-sqlite-storage source-path))
                   (derived (make-sqlite-derived-storage derived-path)))
               (unwind-protect
                    (progn
                      (check "imported duplicate accepted"
                             (= 1 (gethash "duplicate_id_count"
                                           (sqlite-import-jsonl source jsonl-path))))
                      (check "imported duplicate as-of boundary refuses"
                             (handler-case
                                 (progn
                                   (storage-event-position
                                    source "attention-fixture" 11)
                                   nil)
                               (storage-conflict-error () t)))
                      (storage-shadow-attention-prepare derived)
                      (sas-build derived source)
                      (check "imported duplicate physical-order parity"
                             (sas-parity derived source))
                      (storage-append-event
                       source "journal-only" (obj "n" "later")
                       :agent-id "attention-fixture")
                      (sas-build derived source)
                      (multiple-value-bind (sparse highest)
                          (conscious-attention-shadow-events
                           derived source "attention-fixture"
                           :through-event-id 12 :limit 2)
                        (let* ((context (make-projection-context
                                         :now 4000000000
                                         :agent-id "attention-fixture"
                                         :runtime-revision "fixture-v1"))
                               (expected
                                 (inbox-project
                                  (subseq (sas-full-events source) 0 4)
                                  :context context))
                               (actual
                                 (inbox-project
                                  sparse :context context
                                  :observed-highest-event-id highest)))
                          (check "as-of imported rewind preserves maximum ID"
                                 (= 20 highest))
                          (check "as-of attention equals physical prefix"
                                 (equalp expected actual)))))
                 (storage-close derived)
                 (storage-close source))))
           :want-stream-p nil :type "sqlite"))
        :want-stream-p nil :type "sqlite"))
     :want-stream-p nil :type "jsonl")
    (uiop:call-with-temporary-file
     (lambda (source-path)
       (uiop:call-with-temporary-file
        (lambda (derived-path)
          (let ((source (make-sqlite-storage source-path))
                (derived (make-sqlite-derived-storage derived-path)))
            (unwind-protect
                 (progn
                   (storage-shadow-attention-prepare derived)
                   (storage-shadow-lifecycle-prepare derived)
                   (storage-shadow-conscious-pulse-prepare derived)
                   (check "runtime refresh refuses unprepared rows"
                          (handler-case
                              (progn
                                (conscious-storage-refresh-indexed
                                 source derived "attention-fixture")
                                nil)
                            (storage-conflict-error () t)))
                   (storage-append-event source "journal-only" (obj "n" 1)
                                         :agent-id "attention-fixture")
                   (storage-append-event source "user-message"
                                         (obj "text" "synthetic-pending")
                                         :agent-id "attention-fixture")
                   (storage-append-event
                    source "pulse-committed"
                    (obj "pulse_sequence" 7 "agent_id" "attention-fixture"
                         "consumer" "fixture" "disposition" "handled"
                         "stimulus_ids" (vector "stimulus:2"))
                    :agent-id "attention-fixture")
                   (sas-build-conscious-inputs derived source)
                   (multiple-value-bind (state context lifecycle inbox)
                       (conscious-storage-indexed-state
                        source derived "attention-fixture" :now 4000000000
                        :runtime-revision "fixture-v1")
                     (check "indexed lifecycle matches ledger fold"
                            (equalp lifecycle
                                    (conscious-lifecycle-project
                                     (sas-full-events source)
                                     :agent-id "attention-fixture")))
                     (check "indexed inbox matches ledger fold"
                            (equalp inbox
                                    (inbox-project (sas-full-events source)
                                                   :context context)))
                     (check "indexed conscious state matches ledger fold"
                            (equalp state
                                    (conscious-state-project
                                     (sas-full-events source)
                                     :context context)))
                     (check "indexed pulse scalar retained"
                            (= 7 (gethash "state_revision" state))))
                   (check "indexed event bound fails closed"
                          (handler-case
                              (progn
                                (conscious-storage-indexed-state
                                 source derived "attention-fixture"
                                 :maximum-selected-events 1)
                                nil)
                            (storage-conflict-error () t)))
                   (storage-append-event source "journal-only" (obj "n" 2)
                                         :agent-id "attention-fixture")
                   (storage-append-event source "journal-only" (obj "n" 3)
                                         :agent-id "attention-fixture")
                   (check "cross-family stale frontier fails closed"
                          (handler-case
                              (progn
                                (conscious-storage-indexed-state
                                 source derived "attention-fixture")
                                nil)
                            (storage-conflict-error () t)))
                   (check "bounded refresh refuses an over-limit tail"
                          (handler-case
                              (progn
                                (conscious-storage-refresh-indexed
                                 source derived "attention-fixture"
                                 :page-limit 1 :maximum-pages 1)
                                nil)
                            (storage-conflict-error () t)))
                   (check "partial refresh remains unreadable"
                          (handler-case
                              (progn
                                (conscious-storage-indexed-state
                                 source derived "attention-fixture")
                                nil)
                            (storage-conflict-error () t)))
                   (multiple-value-bind (refreshed)
                       (conscious-storage-refresh-indexed
                        source derived "attention-fixture"
                        :page-limit 1 :maximum-pages 4)
                     (check "bounded refresh reaches shared head"
                            (equalp refreshed
                                    (nth-value
                                     0
                                     (conscious-storage-indexed-state
                                      source derived "attention-fixture")))))
                   (multiple-value-bind (as-of context lifecycle inbox)
                       (conscious-storage-indexed-state
                        source derived "attention-fixture"
                        :through-event-id 3 :now 4000000000
                        :runtime-revision "fixture-v1")
                     (let ((prefix (subseq (sas-full-events source) 0 3)))
                       (check "as-of lifecycle matches earlier prefix"
                              (equalp lifecycle
                                      (conscious-lifecycle-project
                                       prefix :agent-id "attention-fixture")))
                       (check "as-of inbox matches earlier prefix"
                              (equalp inbox (inbox-project
                                             prefix :context context)))
                       (check "as-of conscious state matches earlier prefix"
                              (equalp as-of
                                      (conscious-state-project
                                       prefix :context context)))))
                   (storage-append-event
                    source "pulse-committed"
                    (obj "pulse_sequence" 8 "agent_id" "attention-fixture"
                         "consumer" "fixture" "disposition" "handled"
                         "stimulus_ids" (vector))
                    :agent-id "attention-fixture")
                   (sas-build-conscious-inputs derived source)
                   (check "as-of pulse mutation fails closed"
                          (handler-case
                              (progn
                                (conscious-storage-indexed-state
                                 source derived "attention-fixture"
                                 :through-event-id 3)
                                nil)
                            (storage-conflict-error () t))))
              (storage-close derived)
              (storage-close source))))
        :want-stream-p nil :type "sqlite"))
     :want-stream-p nil :type "sqlite")
    (format t "Attention shadow: ~d passed, 0 failed~%" checks)))
