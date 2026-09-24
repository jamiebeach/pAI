;;;; harness: full-system
;;;; Durable shadow cursor across a fresh Lisp process and ledger-only rebuild.
(in-package :agent)

(defun srhr-selector (event position)
  (declare (ignore position))
  (when (equal "unrelated-fixture" (gethash "type" event))
    (error "Filtered restart projection decoded an unrelated event"))
  (when (%recursive-thread-event-p event)
    (values event (let ((cause (gethash "caused_by" event)))
                    (if (and (integerp cause) (plusp cause))
                        cause (gethash "id" event))))))

(defun srhr-apply-all (derived source)
  (loop for report = (storage-shadow-recursive-hot-apply-page
                       derived source #'srhr-selector :agent-id "restart-fixture"
                       :projector-revision "restart-v1"
                       :policy-revision "synthetic" :limit 2
                       :event-types *conscious-recursive-thread-event-types*)
        until (= (gethash "through_position" report)
                 (storage-head-position source :agent-id "restart-fixture"))
        finally (return report)))

(defun srhr-root-ids (derived source &optional (root-id 1))
  (multiple-value-bind (rows)
      (storage-shadow-recursive-hot-read-root
       derived source root-id :agent-id "restart-fixture"
       :projector-revision "restart-v1" :policy-revision "synthetic")
    (mapcar (lambda (row) (gethash "id" (cdr row))) rows)))

(if (boundp 'cl-user::*recursive-shadow-reopen-fixture*)
    (destructuring-bind (source-name derived-name)
        (symbol-value 'cl-user::*recursive-shadow-reopen-fixture*)
      (let ((source (make-sqlite-storage source-name))
            (derived (make-sqlite-derived-storage derived-name)))
        (unwind-protect
             (progn
               (unless (and (= 4 (gethash "through_position"
                                          (storage-shadow-recursive-hot-report
                                           derived source :agent-id "restart-fixture"
                                           :projector-revision "restart-v1"
                                           :policy-revision "synthetic")))
                            (equal '(1 2 3) (srhr-root-ids derived source)))
                 (error "Shadow restart did not restore sealed rows"))
               (srhr-apply-all derived source)
               (storage-append-event source "recursive-activity-opened"
                                     (obj "activity_id" "synthetic-tail")
                                     :agent-id "restart-fixture" :caused-by 1)
               (srhr-apply-all derived source)
               (unless (equal '(1 2 3 5) (srhr-root-ids derived source))
                 (error "Shadow restart did not append only the new tail"))
               (format t "SHADOW-RESTART-PASS~%"))
          (storage-close derived)
          (storage-close source))))
    (let ((checks 0))
      (flet ((check (name value)
               (unless value (error "Shadow restart check failed: ~a" name))
               (incf checks)))
        (uiop:call-with-temporary-file
         (lambda (source-path)
           (uiop:call-with-temporary-file
            (lambda (derived-path)
              (uiop:call-with-temporary-file
               (lambda (rebuilt-path)
                 (let ((source (make-sqlite-storage source-path))
                       (derived (make-sqlite-derived-storage derived-path)))
                   (unwind-protect
                        (progn
                          (storage-shadow-recursive-hot-prepare derived)
                          (storage-append-event source "user-message"
                                                (obj "text" "synthetic root")
                                                :agent-id "restart-fixture")
                          (storage-append-event source "model-response"
                                                (obj "assistant_message"
                                                     (obj "content" "synthetic reply"))
                                                :agent-id "restart-fixture"
                                                :caused-by 1)
                          (storage-append-event source "agent-message"
                                                (obj "text" "synthetic completion")
                                                :agent-id "restart-fixture"
                                                :caused-by 1)
                          (storage-append-event source "unrelated-fixture"
                                                (obj "text" "not selected")
                                                :agent-id "restart-fixture")
                          (srhr-apply-all derived source)
                          (check "initial selection" (equal '(1 2 3)
                                                            (srhr-root-ids derived source)))
                          (check "source cursor includes filtered event"
                                 (= 4 (gethash "through_position"
                                               (storage-shadow-recursive-hot-report
                                                derived source :agent-id "restart-fixture"
                                                :projector-revision "restart-v1"
                                                :policy-revision "synthetic"))))
                          (storage-close derived)
                          (storage-close source)
                          (multiple-value-bind (output ignored status)
                              (uiop:run-program
                               (list "sbcl" "--dynamic-space-size" "2048"
                                     "--non-interactive"
                                     "--load" "/opt/quicklisp/setup.lisp"
                                     "--eval" "(require :asdf)"
                                     "--eval"
                                     (format nil
                                             "(defparameter cl-user::*recursive-shadow-reopen-fixture* (list ~s ~s))"
                                             (namestring source-path)
                                             (namestring derived-path))
                                     "--load"
                                     (namestring
                                      (merge-pathnames "tests/isolated-harness.lisp"
                                                       cl-user::*pai-root*)))
                               :output :string :error-output :string
                               :ignore-error-status t)
                            (declare (ignore ignored))
                            (check "fresh-process reopen and tail"
                                   (and (zerop status)
                                        (search "SHADOW-RESTART-PASS" output)
                                        (not (search "HARNESS-ERR" output)))))
                          (setf source (make-sqlite-storage source-path)
                                derived (make-sqlite-derived-storage derived-path))
                          (check "second reopen sees durable tail"
                                 (equal '(1 2 3 5)
                                        (srhr-root-ids derived source)))
                          (check "second reopen sees durable cursor"
                                 (= 5 (gethash "through_position"
                                               (storage-shadow-recursive-hot-report
                                                derived source :agent-id "restart-fixture"
                                                :projector-revision "restart-v1"
                                                :policy-revision "synthetic"))))
                          (storage-append-event source "unrelated-fixture"
                                                (obj "text" "filtered-only tail")
                                                :agent-id "restart-fixture")
                          (srhr-apply-all derived source)
                          (check "filtered-only page advances physical cursor"
                                 (= 6 (gethash "through_position"
                                               (storage-shadow-recursive-hot-report
                                                derived source :agent-id "restart-fixture"
                                                :projector-revision "restart-v1"
                                                :policy-revision "synthetic"))))
                          (check "filtered-only page adds no selected row"
                                 (equal '(1 2 3 5)
                                        (srhr-root-ids derived source)))
                          ;; A fresh derived database must rebuild from the
                          ;; ledger without a serialized hot-state checkpoint.
                          (let ((rebuilt (make-sqlite-derived-storage rebuilt-path)))
                            (unwind-protect
                                 (progn
                                   (storage-shadow-recursive-hot-prepare rebuilt)
                                   (srhr-apply-all rebuilt source)
                                   (check "ledger-only rebuild rows"
                                          (equalp
                                           (srhr-root-ids derived source)
                                           (srhr-root-ids rebuilt source)))
                                   (check "ledger-only rebuild cursor"
                                          (= 6 (gethash "through_position"
                                                        (storage-shadow-recursive-hot-report
                                                         rebuilt source
                                                         :agent-id "restart-fixture"
                                                         :projector-revision "restart-v1"
                                                         :policy-revision "synthetic")))))
                              (storage-close rebuilt))))
                     (storage-close derived)
                     (storage-close source))))
               :want-stream-p nil :type "sqlite"))
            :want-stream-p nil :type "sqlite"))
         :want-stream-p nil :type "sqlite")
      ;; Legacy imports may rewind or reuse event IDs. Physical positions,
      ;; not event IDs, must still define the applied prefix and row order.
      (uiop:call-with-temporary-file
       (lambda (jsonl-path)
         (uiop:call-with-temporary-file
          (lambda (source-path)
            (uiop:call-with-temporary-file
             (lambda (derived-path)
               (with-open-file (stream jsonl-path :direction :output
                                            :if-exists :supersede)
                 (dolist (event
                          (list
                           (obj "schema_version" 2 "id" 10
                                "timestamp" "2026-01-01T12:00:00Z"
                                "type" "user-message" "caused_by" :null
                                "payload" (obj "text" "imported root"))
                           (obj "schema_version" 2 "id" 11
                                "timestamp" "2026-01-01T12:00:01Z"
                                "type" "model-response" "caused_by" 10
                                "payload" (obj "assistant_message"
                                               (obj "content" "imported response")))
                           (obj "schema_version" 2 "id" 3
                                "timestamp" "2026-01-01T12:00:02Z"
                                "type" "agent-message" "caused_by" 10
                                "payload" (obj "text" "imported completion"))
                           (obj "schema_version" 2 "id" 11
                                "timestamp" "2026-01-01T12:00:03Z"
                                "type" "context-graph-update-proposed"
                                "caused_by" 10
                                "payload" (obj "proposal" "imported"))))
                   (write-line (%storage-json event) stream)))
               (let ((source (make-sqlite-storage source-path))
                     (derived (make-sqlite-derived-storage derived-path)))
                 (unwind-protect
                      (progn
                        (let ((report (sqlite-import-jsonl
                                       source jsonl-path
                                       :legacy-agent-id "restart-fixture")))
                          (check "legacy rows keep assumed partition"
                                 (= 4 (gethash "assumed_partition_count" report)))
                          (check "legacy rewind and duplicate retained"
                                 (and (= 1 (gethash "rewind_count" report))
                                      (= 1 (gethash "duplicate_id_count" report)))))
                        (storage-prepare-activity-index source)
                        (storage-shadow-recursive-hot-prepare derived)
                        (srhr-apply-all derived source)
                        (check "imported physical prefix sealed"
                               (= 4 (gethash "through_position"
                                             (storage-shadow-recursive-hot-report
                                              derived source :agent-id "restart-fixture"
                                              :projector-revision "restart-v1"
                                              :policy-revision "synthetic"))))
                        (check "imported duplicate IDs remain distinct rows"
                               (equal '(10 11 3 11)
                                      (srhr-root-ids derived source 10)))
                        (check "historical lookup excludes later duplicate ID"
                               (string=
                                "model-response"
                                (gethash "type"
                                         (storage-read-event-before-position
                                          source "restart-fixture" 11 4))))
                        (check "historical lookup sees newest duplicate at head"
                               (string=
                                "context-graph-update-proposed"
                                (gethash "type"
                                         (storage-read-event-before-position
                                          source "restart-fixture" 11 5))))
                        (check "historical lookup excludes frontier row"
                               (null (storage-read-event-before-position
                                      source "restart-fixture" 11 2)))
                        (check "historical lookup is agent scoped"
                               (null (storage-read-event-before-position
                                      source "other-agent" 11 5)))
                        (check "imported terminal is visible at position three"
                               (storage-root-has-event-type-p
                                source "restart-fixture" 10 "agent-message"
                                :through-position 3))
                        (check "later graph proposal excluded at earlier frontier"
                               (not (storage-root-has-event-type-p
                                     source "restart-fixture" 10
                                     "context-graph-update-proposed"
                                     :through-position 3)))
                        (check "later graph proposal visible at sealed head"
                               (storage-root-has-event-type-p
                                source "restart-fixture" 10
                                "context-graph-update-proposed"
                                :source-boundary
                                (storage-authority-boundary
                                 source :agent-id "restart-fixture"))))
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
                     (storage-append-event source "pulse-committed"
                                           (obj "pulse_sequence" 4)
                                           :agent-id "pulse-restart")
                     (storage-append-event source "pulse-committed"
                                           (obj "pulse_sequence" 7)
                                           :agent-id "pulse-restart")
                     (storage-shadow-conscious-pulse-prepare derived)
                     (loop for row =
                             (storage-shadow-conscious-pulse-apply-page
                              derived source :agent-id "pulse-restart"
                              :limit 1)
                           until (= (gethash "through_position" row)
                                    (storage-head-position
                                     source :agent-id "pulse-restart")))
                     (check "pulse scalar builds in bounded pages"
                            (= 7 (gethash "max_sequence"
                                          (storage-shadow-conscious-pulse-report
                                           derived source
                                           :agent-id "pulse-restart"))))
                     (storage-close derived)
                     (storage-close source)
                     (setf source (make-sqlite-storage source-path)
                           derived (make-sqlite-derived-storage derived-path))
                     (check "pulse scalar survives a fresh storage reopen"
                            (= 7 (gethash "max_sequence"
                                          (storage-shadow-conscious-pulse-report
                                           derived source
                                           :agent-id "pulse-restart"))))
                     (storage-append-event source "pulse-committed"
                                           (obj "pulse_sequence" 9)
                                           :agent-id "pulse-restart")
                     (check "pulse scalar advances a new durable tail"
                            (= 9 (gethash "max_sequence"
                                          (storage-shadow-conscious-pulse-apply-page
                                           derived source
                                           :agent-id "pulse-restart")))))
                (storage-close derived)
                (storage-close source))))
          :want-stream-p nil :type "sqlite"))
       :want-stream-p nil :type "sqlite")
      (format t "Recursive hot restart: ~d passed, 0 failed~%" checks))))
