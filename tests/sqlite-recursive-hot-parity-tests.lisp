;;;; harness: full-system
;;;; Shadow selection parity, with a recorded settlement cutover gap.
(in-package :agent)

(let ((checks 0))
  (flet ((check (name value)
           (unless value (error "Recursive hot parity: ~a" name))
           (incf checks)))
    (uiop:call-with-temporary-file
     (lambda (source-path)
       (uiop:call-with-temporary-file
        (lambda (derived-path)
          (let* ((source (make-sqlite-storage source-path))
                 (derived (make-sqlite-derived-storage derived-path))
                 (*event-authority-port* nil)
                 (*sqlite-event-authority-backend* nil)
                 (*sqlite-event-authority-agent-id* nil)
                 (*sqlite-event-authority-checkpoint-backend* nil)
                 (*sqlite-event-authority-database* nil)
                 (*sqlite-event-authority-derived-database* nil)
                 (agent-id "parity-fixture")
                 (primary-root-id nil)
                 (selector
                   (lambda (event position)
                     (declare (ignore position))
                     (when (equal "unrelated-fixture" (gethash "type" event))
                       (error "Filtered projection decoded an unrelated event"))
                     (when (%recursive-thread-event-p event)
                       (values event
                               (let ((cause (gethash "caused_by" event)))
                                 (if (and (integerp cause) (plusp cause))
                                     cause (gethash "id" event))))))))
            (unwind-protect
                 (progn
                   (%sqlite-authority-install
                    source source-path derived derived-path agent-id)
                   (storage-prepare-activity-index source)
                   (storage-shadow-recursive-hot-prepare derived)
                   (let* ((root
                            (storage-append-event source "user-message"
                                                  (obj "text" "synthetic root")
                                                  :agent-id agent-id))
                          (root-id (gethash "id" root)))
                     (setf primary-root-id root-id)
                     (storage-append-event source "unrelated-fixture"
                                           (obj "text" "ignore")
                                           :agent-id agent-id)
                     (storage-append-event source "model-request"
                                           (obj "prompt" "synthetic request")
                                           :agent-id agent-id :caused-by root-id)
                     (storage-append-event source "model-response"
                                           (obj "assistant_message"
                                                (obj "content" "synthetic answer"))
                                           :agent-id agent-id :caused-by root-id)
                     (storage-append-event source "model-response"
                                           (obj "knowledge_graph_formation" t)
                                           :agent-id agent-id :caused-by root-id)
                     (storage-append-event source "recursive-activity-opened"
                                           (obj "activity_id" "synthetic-activity")
                                           :agent-id agent-id :caused-by root-id)
                     (loop for report =
                             (storage-shadow-recursive-hot-apply-page
                              derived source selector :agent-id agent-id
                              :projector-revision "parity-v1"
                              :policy-revision "synthetic" :limit 2
                              :event-types *conscious-recursive-thread-event-types*)
                           until (= (gethash "through_position" report)
                                    (storage-head-position source :agent-id agent-id)))
                     (multiple-value-bind (page)
                         (storage-shadow-recursive-hot-read-page
                          derived source :agent-id agent-id
                          :projector-revision "parity-v1"
                          :policy-revision "synthetic")
                       (let ((expected (%recursive-thread-events-full-replay)))
                         (check "selected event ids match the old projection"
                                (equal (mapcar (lambda (row)
                                                 (gethash "id" (cdr row))) page)
                                       (mapcar (lambda (event)
                                                 (gethash "id" event)) expected)))
                         (check "graph-owned provider row is omitted"
                                (= 4 (length page)))
                         (check "activity row remains visible"
                                (find "recursive-activity-opened" page
                                      :key (lambda (row)
                                             (gethash "type" (cdr row)))
                                      :test #'string=))))
                     (multiple-value-bind (root-page)
                         (storage-shadow-recursive-hot-read-root
                          derived source root-id :agent-id agent-id
                          :projector-revision "parity-v1"
                          :policy-revision "synthetic")
                       (check "root-scoped page matches selected generation"
                              (= 4 (length root-page)))
                       (check "active root has exact event payload parity"
                              (equalp (mapcar #'cdr root-page)
                                      (remove-if-not
                                       (lambda (event)
                                         (or (eql root-id (gethash "id" event))
                                             (eql root-id
                                                  (gethash "caused_by" event))))
                                       (%recursive-thread-events-full-replay)))))
                     (multiple-value-bind (candidate)
                         (conscious-recursive-hot-root-page
                          source derived agent-id root-id
                          :projector-revision "parity-v1"
                          :policy-revision "synthetic")
                       (check "bounded candidate reader matches active root"
                              (equalp candidate
                                      (remove-if-not
                                       (lambda (event)
                                         (or (eql root-id (gethash "id" event))
                                             (eql root-id
                                                  (gethash "caused_by" event))))
                                       (%recursive-thread-events-full-replay)))))
                     (let ((before-terminal
                             (storage-authority-boundary source :agent-id agent-id)))
                       (check "authority says unsettled at frozen frontier"
                              (not (storage-root-has-event-type-p
                                    source agent-id root-id "agent-message"
                                    :source-boundary before-terminal)))
                     ;; Settlement changes earlier provider payloads in the
                     ;; legacy projection. The append-only shadow does not yet
                     ;; rewrite them, so no live reader may switch to it.
                     (storage-append-event source "agent-message"
                                           (obj "text" "done")
                                           :agent-id agent-id :caused-by root-id)
                     (check "late settlement does not alter frozen authority answer"
                            (not (storage-root-has-event-type-p
                                  source agent-id root-id "agent-message"
                                  :source-boundary before-terminal))))
                     (check "authority reports terminal at current frontier"
                            (storage-root-has-event-type-p
                             source agent-id root-id "agent-message"
                             :source-boundary
                             (storage-authority-boundary source :agent-id agent-id)))
                     (check "candidate reader refuses a stale shadow head"
                            (handler-case
                                (progn
                                  (conscious-recursive-hot-root-page
                                   source derived agent-id root-id
                                   :projector-revision "parity-v1"
                                   :policy-revision "synthetic")
                                  nil)
                              (storage-conflict-error () t)))
                     (storage-shadow-recursive-hot-apply-page
                      derived source selector :agent-id agent-id
                      :projector-revision "parity-v1"
                      :policy-revision "synthetic"
                      :event-types *conscious-recursive-thread-event-types*)
                     (multiple-value-bind (page)
                         (storage-shadow-recursive-hot-read-root
                          derived source root-id :agent-id agent-id
                          :projector-revision "parity-v1"
                          :policy-revision "synthetic")
                       (let* ((legacy (%recursive-thread-events-full-replay))
                              (old-response
                                (find "model-response" legacy
                                      :key (lambda (event)
                                             (gethash "type" event))
                                      :test #'string=))
                              (shadow-response
                                (find "model-response" page
                                      :key (lambda (row)
                                             (gethash "type" (cdr row)))
                                      :test #'string=)))
                         (check "indexed root fact finds terminal message"
                                (storage-shadow-recursive-hot-root-has-type-p
                                 derived source root-id "agent-message"
                                 :agent-id agent-id
                                 :projector-revision "parity-v1"
                                 :policy-revision "synthetic"))
                         (check "indexed root fact rejects absent graph protection"
                                (not (storage-shadow-recursive-hot-root-has-type-p
                                      derived source root-id
                                      "context-graph-update-proposed"
                                      :agent-id agent-id
                                      :projector-revision "parity-v1"
                                      :policy-revision "synthetic")))
                         (check "settlement compaction remains an explicit cutover gate"
                                (and (gethash "settled_assistant_compacted"
                                              (gethash "payload" old-response))
                                     (gethash "assistant_message"
                                              (gethash "payload"
                                                       (cdr shadow-response)))))
                         (let ((candidate nil) (cursor 0))
                           (loop
                             (multiple-value-bind (page next)
                                 (conscious-recursive-hot-root-page
                                  source derived agent-id root-id :limit 2
                                  :after-position cursor
                                  :projector-revision "parity-v1"
                                  :policy-revision "synthetic")
                               (unless page (return))
                               (setf candidate (nconc candidate page)
                                     cursor next)))
                           (check "paged candidate reader matches settled root"
                                  (equalp
                                   candidate
                                   (remove-if-not
                                    (lambda (event)
                                      (or (eql root-id (gethash "id" event))
                                          (eql root-id
                                               (gethash "caused_by" event))))
                                    legacy))))
                         (let ((settled (make-hash-table :test #'eql))
                               (protected (make-hash-table :test #'eql)))
                           (setf (gethash root-id settled)
                                 (storage-root-has-event-type-p
                                  source agent-id root-id
                                  *conscious-recursive-terminal-event-types*
                                  :source-boundary
                                  (storage-authority-boundary
                                   source :agent-id agent-id))
                                 (gethash root-id protected)
                                 (storage-root-has-event-type-p
                                  source agent-id root-id
                                  "context-graph-update-proposed"
                                  :source-boundary
                                  (storage-authority-boundary
                                   source :agent-id agent-id)))
                           (check "settled root has exact read-time payload parity"
                                  (equalp
                                   (mapcar (lambda (row)
                                             (%recursive-compact-settled-provider-event
                                              (cdr row) settled protected))
                                           page)
                                   (remove-if-not
                                    (lambda (event)
                                      (or (eql root-id (gethash "id" event))
                                          (eql root-id (gethash "caused_by" event))))
                                    legacy)))
                           (let ((compacted
                                   (%recursive-compact-settled-provider-event
                                    (cdr shadow-response) settled protected)))
                             (check "root-scoped read-time compaction reproduces settled response"
                                    (and (gethash "settled_assistant_compacted"
                                                  (gethash "payload" compacted))
                                         (null (gethash "assistant_message"
                                                        (gethash "payload" compacted)))))))))
                     ;; A graph proposal preserves exact provider evidence
                     ;; even after the same root receives a terminal event.
                     (let* ((protected-root
                              (storage-append-event source "user-message"
                                                    (obj "text" "protected synthetic root")
                                                    :agent-id agent-id))
                            (protected-id (gethash "id" protected-root)))
                       (storage-append-event source "model-response"
                                             (obj "assistant_message"
                                                  (obj "content" "protected answer"))
                                             :agent-id agent-id
                                             :caused-by protected-id)
                       (storage-append-event source "context-graph-update-proposed"
                                             (obj "proposal" "synthetic")
                                             :agent-id agent-id
                                             :caused-by protected-id)
                       (storage-append-event source "agent-message"
                                             (obj "text" "protected done")
                                             :agent-id agent-id
                                             :caused-by protected-id)
                       (check "authority reports graph protection"
                              (storage-root-has-event-type-p
                               source agent-id protected-id
                               "context-graph-update-proposed"
                               :source-boundary
                               (storage-authority-boundary source :agent-id agent-id)))
                       (storage-shadow-recursive-hot-apply-page
                        derived source selector :agent-id agent-id
                        :projector-revision "parity-v1"
                        :policy-revision "synthetic"
                        :event-types *conscious-recursive-thread-event-types*)
                       (multiple-value-bind (protected-page)
                           (storage-shadow-recursive-hot-read-root
                            derived source protected-id :agent-id agent-id
                            :projector-revision "parity-v1"
                            :policy-revision "synthetic")
                         (let ((response
                                 (find "model-response" protected-page
                                       :key (lambda (row)
                                              (gethash "type" (cdr row)))
                                       :test #'string=)))
                           (check "protected response is selected"
                                  response)
                           (check "old projection preserves protected provider evidence"
                                  (let ((old
                                          (find-if
                                           (lambda (event)
                                             (and (string= "model-response"
                                                           (gethash "type" event ""))
                                                  (eql protected-id
                                                       (gethash "caused_by" event))))
                                           (%recursive-thread-events-full-replay))))
                                    (gethash "assistant_message"
                                             (gethash "payload" old))))
                           (let ((settled (make-hash-table :test #'eql))
                                 (protected (make-hash-table :test #'eql)))
                             (setf (gethash protected-id settled)
                                   (storage-root-has-event-type-p
                                    source agent-id protected-id
                                    *conscious-recursive-terminal-event-types*
                                    :source-boundary
                                    (storage-authority-boundary
                                     source :agent-id agent-id))
                                   (gethash protected-id protected)
                                   (storage-root-has-event-type-p
                                    source agent-id protected-id
                                    "context-graph-update-proposed"
                                    :source-boundary
                                    (storage-authority-boundary
                                     source :agent-id agent-id)))
                             (check "protected root has exact read-time payload parity"
                                    (equalp
                                     (mapcar (lambda (row)
                                               (%recursive-compact-settled-provider-event
                                                (cdr row) settled protected))
                                             protected-page)
                                     (remove-if-not
                                      (lambda (event)
                                        (or (eql protected-id (gethash "id" event))
                                            (eql protected-id
                                                 (gethash "caused_by" event))))
                                      (%recursive-thread-events-full-replay))))
                             (check "protected read-time transform preserves provider evidence"
                                    (gethash "assistant_message"
                                             (gethash "payload"
                                                      (%recursive-compact-settled-provider-event
                                                       (cdr response) settled protected))))))))))
              (let ((candidate nil) (cursor 0))
                (loop
                  (multiple-value-bind (page next)
                      (conscious-recursive-hot-page
                       source derived agent-id :limit 2
                       :after-position cursor
                       :projector-revision "parity-v1"
                       :policy-revision "synthetic")
                    (unless page (return))
                    (setf candidate (nconc candidate page)
                          cursor next)))
                (check "global bounded pages match active, settled and protected replay"
                       (equalp candidate (%recursive-thread-events-full-replay))))
              (let ((first (storage-append-event
                            source "recursive-root-failed"
                            (obj "reason" "synthetic first")
                            :agent-id agent-id :caused-by primary-root-id))
                    (saved (symbol-function '%recursive-thread-events)))
                (declare (ignore first))
                (let ((latest (storage-append-event
                               source "recursive-root-failed"
                               (obj "reason" "synthetic latest")
                               :agent-id agent-id :caused-by primary-root-id)))
                  (unwind-protect
                       (progn
                         (setf (symbol-function '%recursive-thread-events)
                               (lambda () (error "whole generation was loaded")))
                         (check "indexed failure receipt avoids the whole generation"
                                (eql (gethash "id" latest)
                                     (gethash "id"
                                              (recursive-root-failure-receipt
                                               primary-root-id)))))
                    (setf (symbol-function '%recursive-thread-events) saved))))
              (storage-append-event source "unrelated-fixture"
                                    (obj "text" "filtered tail")
                                    :agent-id agent-id)
              (check "global reader refuses an unadvanced physical cursor"
                     (handler-case
                         (progn
                           (conscious-recursive-hot-page
                            source derived agent-id
                            :projector-revision "parity-v1"
                            :policy-revision "synthetic")
                           nil)
                       (storage-conflict-error () t)))
              (storage-shadow-conscious-pulse-prepare derived)
              (let ((baseline
                      (storage-shadow-conscious-pulse-apply-page
                       derived source :agent-id agent-id :limit 2)))
                (check "empty pulse scalar seals the current physical frontier"
                       (and (zerop (gethash "max_sequence" baseline))
                            (= (gethash "through_position" baseline)
                               (storage-head-position source
                                                      :agent-id agent-id)))))
              (storage-append-event source "pulse-committed"
                                    (obj "pulse_sequence" 3)
                                    :agent-id agent-id)
              (storage-append-event source "pulse-committed"
                                    (obj "pulse_sequence" 2)
                                    :agent-id agent-id)
              (storage-append-event source "pulse-committed"
                                    (obj "pulse_sequence" 5)
                                    :agent-id agent-id)
              (storage-append-event source "unrelated-fixture"
                                    (obj "text" "pulse filtered tail")
                                    :agent-id agent-id)
              (let ((first-page
                      (storage-shadow-conscious-pulse-apply-page
                       derived source :agent-id agent-id :limit 2)))
                (check "bounded pulse page retains the maximum valid sequence"
                       (= 3 (gethash "max_sequence" first-page)))
                (check "bounded pulse page stops at the selected-row frontier"
                       (< (gethash "through_position" first-page)
                          (storage-head-position source
                                                 :agent-id agent-id))))
              (let ((final
                      (storage-shadow-conscious-pulse-apply-page
                       derived source :agent-id agent-id :limit 2)))
                (check "pulse scalar catches up across filtered physical tail"
                       (and (= 5 (gethash "max_sequence" final))
                            (= (gethash "through_position" final)
                               (storage-head-position source
                                                      :agent-id agent-id))))
                (check "pulse scalar is idempotent at a sealed head"
                       (equalp final
                               (storage-shadow-conscious-pulse-apply-page
                                derived source :agent-id agent-id :limit 2))))
              (bt:with-lock-held ((%sqlite-derived-lock derived))
                (%sqlite-exec
                 (%sqlite-derived-handle derived :pulse-tamper-fixture)
                 "UPDATE pai_conscious_pulse_v1 SET max_sequence=max_sequence+1 WHERE agent_id='parity-fixture'"
                 :pulse-tamper-fixture))
              (check "pulse scalar rejects a mutated derived value"
                     (handler-case
                         (progn
                           (storage-shadow-conscious-pulse-report
                            derived source :agent-id agent-id)
                           nil)
                       (storage-conflict-error () t)))
              (event-authority-clear))))
        :want-stream-p nil :type "sqlite"))
     :want-stream-p nil :type "sqlite"))
  (format t "Recursive hot parity: ~d passed, 0 failed~%" checks))
