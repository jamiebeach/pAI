;;;; harness: full-system
(in-package :agent)

(let ((checks 0))
  (flet ((check (value) (unless value (error "Recursive hot shadow regression"))
           (incf checks)))
    (uiop:call-with-temporary-file
     (lambda (source-path)
       (uiop:call-with-temporary-file
        (lambda (derived-path)
          (let ((source (make-sqlite-storage source-path))
                (derived (make-sqlite-derived-storage derived-path)))
            (unwind-protect
                 (let ((selector
                         (lambda (event position)
                           (declare (ignore position))
                           (when (equal "user-message" (gethash "type" event))
                             (values event (gethash "id" event))))))
                   (storage-shadow-recursive-hot-prepare derived)
                   ;; A pre-existing v1 shadow artifact is not a v2 seal.
                   (%sqlite-exec
                    (%sqlite-derived-handle derived :fixture)
                    "CREATE TABLE pai_recursive_hot_shadow_watermark (agent_id TEXT PRIMARY KEY, source_binding TEXT NOT NULL)"
                    :fixture)
                   (%sqlite-exec
                    (%sqlite-derived-handle derived :fixture)
                    "INSERT INTO pai_recursive_hot_shadow_watermark VALUES('fixture','legacy-seal')"
                    :fixture)
                   (check (null (storage-shadow-recursive-hot-report
                                 derived source :agent-id "fixture"
                                 :projector-revision "fixture-v1"
                                 :policy-revision "fixture-p1")))
                   (check (zerop (gethash "through_position"
                                         (storage-shadow-recursive-hot-apply-page
                                          derived source selector :agent-id "empty"
                                          :projector-revision "fixture-v1"
                                          :policy-revision "fixture-p1"))))
                   (dotimes (i 5)
                     (storage-append-event
                      source (if (evenp i) "user-message" "other")
                      (obj "text" (format nil "synthetic-~d" i))
                      :agent-id "fixture"))
                   (check (zerop (gethash "through_position"
                                         (storage-shadow-recursive-hot-report
                                          derived source :agent-id "empty"
                                          :projector-revision "fixture-v1"
                                          :policy-revision "fixture-p1"))))
                   (let ((first (storage-shadow-recursive-hot-apply-page
                                 derived source selector :agent-id "fixture"
                                 :projector-revision "fixture-v1"
                                 :policy-revision "fixture-p1" :limit 2)))
                     (check (= 2 (gethash "through_position" first))))
                   (let ((second (storage-shadow-recursive-hot-apply-page
                                  derived source selector :agent-id "fixture"
                                  :projector-revision "fixture-v1"
                                  :policy-revision "fixture-p1" :limit 2)))
                     (check (= 4 (gethash "through_position" second))))
                   (let ((third (storage-shadow-recursive-hot-apply-page
                                 derived source selector :agent-id "fixture"
                                 :projector-revision "fixture-v1"
                                 :policy-revision "fixture-p1" :limit 2)))
                     (check (= 5 (gethash "through_position" third)))
                     (check (= 5 (gethash "through_position"
                                  (storage-shadow-recursive-hot-apply-page
                                   derived source selector :agent-id "fixture"
                                   :projector-revision "fixture-v1"
                                   :policy-revision "fixture-p1" :limit 2)))))
                   (multiple-value-bind (page watermark)
                       (storage-shadow-recursive-hot-read-page
                        derived source :agent-id "fixture"
                        :projector-revision "fixture-v1"
                        :policy-revision "fixture-p1" :limit 2)
                     (check (= 2 (length page)))
                     (check (= 5 (gethash "through_position" watermark)))
                     (check (= 1 (caar page))))
                   (multiple-value-bind (page)
                       (storage-shadow-recursive-hot-read-page
                        derived source :agent-id "fixture"
                        :projector-revision "fixture-v1"
                        :policy-revision "fixture-p1" :after-position 1 :limit 2)
                     (check (equal '(3 5) (mapcar #'car page))))
                   (multiple-value-bind (root)
                       (storage-shadow-recursive-hot-read-root
                        derived source 3 :agent-id "fixture"
                        :projector-revision "fixture-v1"
                        :policy-revision "fixture-p1")
                     (check (equal '(3) (mapcar #'car root))))
                   (multiple-value-bind (present watermark)
                       (storage-shadow-recursive-hot-root-has-type-p
                        derived source 3 '("other" "user-message")
                        :agent-id "fixture" :projector-revision "fixture-v1"
                        :policy-revision "fixture-p1")
                     (check present)
                     (check (= 5 (gethash "through_position" watermark))))
                   (check (storage-shadow-recursive-hot-root-has-type-p
                           derived source 1 "user-message"
                           :agent-id "fixture" :projector-revision "fixture-v1"
                           :policy-revision "fixture-p1"))
                   (check (null (storage-shadow-recursive-hot-root-has-type-p
                                 derived source 3 '("other" "model-response")
                                 :agent-id "fixture" :projector-revision "fixture-v1"
                                 :policy-revision "fixture-p1")))
                   (check (null (storage-shadow-recursive-hot-root-has-type-p
                                 derived source 999 "user-message"
                                 :agent-id "fixture" :projector-revision "fixture-v1"
                                 :policy-revision "fixture-p1")))
                   (check (null (storage-shadow-recursive-hot-root-has-type-p
                                 derived source 3 "user-message"
                                 :agent-id "empty" :projector-revision "fixture-v1"
                                 :policy-revision "fixture-p1")))
                   (check (handler-case
                              (progn
                                (storage-shadow-recursive-hot-root-has-type-p
                                 derived source 3 "user-message"
                                 :agent-id "fixture" :projector-revision "wrong"
                                 :policy-revision "fixture-p1")
                                nil)
                            (storage-conflict-error () t)))
                   (check (handler-case
                              (progn
                                (storage-shadow-recursive-hot-root-has-type-p
                                 derived source 3 (make-list 17 :initial-element "user-message")
                                 :agent-id "fixture" :projector-revision "fixture-v1"
                                 :policy-revision "fixture-p1")
                                nil)
                            (storage-error () t)))
                   (check (handler-case
                              (progn
                                (storage-shadow-recursive-hot-read-page
                                 derived source :agent-id "fixture"
                                 :projector-revision "fixture-v2"
                                 :policy-revision "fixture-p1")
                                nil)
                            (storage-conflict-error () t)))
                   (let ((bad-selector (lambda (event position)
                                         (declare (ignore position))
                                         (error "synthetic interrupted page"))))
                     (storage-append-event source "user-message" (obj "text" "tail")
                                           :agent-id "fixture")
                     (check (handler-case
                                (progn
                                  (storage-shadow-recursive-hot-apply-page
                                   derived source bad-selector :agent-id "fixture"
                                   :projector-revision "fixture-v1"
                                   :policy-revision "fixture-p1")
                                  nil)
                              (error () t)))
                     (check (= 5 (gethash "through_position"
                                      (storage-shadow-recursive-hot-report
                                       derived source :agent-id "fixture"
                                       :projector-revision "fixture-v1"
                                       :policy-revision "fixture-p1")))))
                   (storage-shadow-recursive-hot-apply-page
                    derived source selector :agent-id "fixture"
                    :projector-revision "fixture-v1" :policy-revision "fixture-p1")
                   (multiple-value-bind (root)
                       (storage-shadow-recursive-hot-read-root
                        derived source 6 :agent-id "fixture"
                        :projector-revision "fixture-v1"
                        :policy-revision "fixture-p1")
                     (check (= 1 (length root))))
                   ;; A failure after the first row insert must roll back the
                   ;; row and watermark together, not leave a false seal.
                   (storage-append-event source "user-message" (obj "text" "seven")
                                         :agent-id "fixture")
                   (storage-append-event source "user-message" (obj "text" "eight")
                                         :agent-id "fixture")
                   (%sqlite-exec
                    (%sqlite-derived-handle derived :fixture)
                    "CREATE TRIGGER fixture_abort_shadow BEFORE INSERT ON pai_recursive_hot_shadow_v2_events WHEN NEW.storage_position=8 BEGIN SELECT RAISE(ABORT,'synthetic interrupted transaction'); END"
                    :fixture)
                   (check (handler-case
                              (progn
                                (storage-shadow-recursive-hot-apply-page
                                 derived source selector :agent-id "fixture"
                                 :projector-revision "fixture-v1"
                                 :policy-revision "fixture-p1" :limit 2)
                                nil)
                            (storage-error () t)))
                   (check (= 6 (gethash "through_position"
                                    (storage-shadow-recursive-hot-report
                                     derived source :agent-id "fixture"
                                     :projector-revision "fixture-v1"
                                     :policy-revision "fixture-p1"))))
                   (multiple-value-bind (page)
                       (storage-shadow-recursive-hot-read-page
                        derived source :agent-id "fixture"
                        :projector-revision "fixture-v1"
                        :policy-revision "fixture-p1" :after-position 6)
                     (check (null page)))
                   (%sqlite-exec
                    (%sqlite-derived-handle derived :fixture)
                    "DROP TRIGGER fixture_abort_shadow" :fixture)
                   (storage-shadow-recursive-hot-apply-page
                    derived source selector :agent-id "fixture"
                    :projector-revision "fixture-v1"
                    :policy-revision "fixture-p1" :limit 2)
                   (multiple-value-bind (page)
                       (storage-shadow-recursive-hot-read-page
                        derived source :agent-id "fixture"
                        :projector-revision "fixture-v1"
                        :policy-revision "fixture-p1" :after-position 6)
                     (check (equal '(7 8) (mapcar #'car page))))
                   (storage-append-event source "user-message"
                                         (obj "text" "nullable-root")
                                         :agent-id "nullable")
                   (storage-shadow-recursive-hot-apply-page
                    derived source
                    (lambda (event position)
                      (declare (ignore position))
                      (values event nil))
                    :agent-id "nullable" :projector-revision "fixture-v1"
                    :policy-revision "fixture-p1")
                   (multiple-value-bind (page)
                       (storage-shadow-recursive-hot-read-page
                        derived source :agent-id "nullable"
                        :projector-revision "fixture-v1"
                        :policy-revision "fixture-p1")
                     (check (= 1 (length page)))
                     (check (equal "nullable-root"
                                   (gethash "text" (gethash "payload" (cdar page))))))
                   (%sqlite-exec
                    (%sqlite-derived-handle derived :fixture)
                    "DELETE FROM pai_recursive_hot_shadow_v2_events WHERE agent_id='nullable'"
                    :fixture)
                   (check (handler-case
                              (progn
                                (storage-shadow-recursive-hot-read-page
                                 derived source :agent-id "nullable"
                                 :projector-revision "fixture-v1"
                                 :policy-revision "fixture-p1")
                                nil)
                            (storage-integrity-error () t)))
                   ;; A deleted selected row must not look like an ordinary
                   ;; filtered source-position gap, including the final row.
                   (%sqlite-exec
                    (%sqlite-derived-handle derived :fixture)
                    "DELETE FROM pai_recursive_hot_shadow_v2_events WHERE agent_id='fixture' AND storage_position=1"
                    :fixture)
                   (check (handler-case
                              (progn
                                (storage-shadow-recursive-hot-read-page
                                 derived source :agent-id "fixture"
                                 :projector-revision "fixture-v1"
                                 :policy-revision "fixture-p1")
                                nil)
                            (storage-integrity-error () t)))
                   (check (handler-case
                              (progn
                                (storage-shadow-recursive-hot-read-root
                                 derived source 1 :agent-id "fixture"
                                 :projector-revision "fixture-v1"
                                 :policy-revision "fixture-p1")
                                nil)
                            (storage-integrity-error () t)))
                   (%sqlite-exec
                    (%sqlite-derived-handle derived :fixture)
                    "DELETE FROM pai_recursive_hot_shadow_v2_events WHERE agent_id='fixture' AND storage_position=3"
                    :fixture)
                   (check (handler-case
                              (progn
                                (storage-shadow-recursive-hot-read-page
                                 derived source :agent-id "fixture"
                                 :projector-revision "fixture-v1"
                                 :policy-revision "fixture-p1")
                                nil)
                            (storage-integrity-error () t)))
                   (check (handler-case
                              (progn
                                (storage-shadow-recursive-hot-read-root
                                 derived source 3 :agent-id "fixture"
                                 :projector-revision "fixture-v1"
                                 :policy-revision "fixture-p1")
                                nil)
                            (storage-integrity-error () t)))
                   ;; Other roots remain readable; this is a scoped check.
                   (multiple-value-bind (root)
                       (storage-shadow-recursive-hot-read-root
                        derived source 7 :agent-id "fixture"
                        :projector-revision "fixture-v1"
                        :policy-revision "fixture-p1")
                     (check (= 1 (length root))))
                   (%sqlite-exec
                    (%sqlite-derived-handle derived :fixture)
                    "DELETE FROM pai_recursive_hot_shadow_v2_events WHERE agent_id='fixture' AND storage_position=8"
                    :fixture)
                   (check (handler-case
                              (progn
                                (storage-shadow-recursive-hot-read-page
                                 derived source :agent-id "fixture"
                                 :projector-revision "fixture-v1"
                                 :policy-revision "fixture-p1" :after-position 6)
                                nil)
                            (storage-integrity-error () t)))
                   (check (handler-case
                              (progn
                                (storage-shadow-recursive-hot-read-root
                                 derived source 8 :agent-id "fixture"
                                 :projector-revision "fixture-v1"
                                 :policy-revision "fixture-p1")
                                nil)
                            (storage-integrity-error () t)))
                   ;; The indexed presence query is intentionally shadow-only:
                   ;; a deleted sole matching row can yield a false negative.
                   ;; Root paging above must still detect the missing row.
                   (check (null (storage-shadow-recursive-hot-root-has-type-p
                                 derived source 8 "user-message"
                                 :agent-id "fixture" :projector-revision "fixture-v1"
                                 :policy-revision "fixture-p1")))
                   (%sqlite-exec
                    (%sqlite-derived-handle derived :fixture)
                    "UPDATE pai_recursive_hot_shadow_v2_events SET event_json='{}' WHERE agent_id='fixture' AND storage_position=7"
                    :fixture)
                   (check (handler-case
                              (progn
                                (storage-shadow-recursive-hot-root-has-type-p
                                 derived source 7 "user-message"
                                 :agent-id "fixture" :projector-revision "fixture-v1"
                                 :policy-revision "fixture-p1")
                                nil)
                            (storage-integrity-error () t)))
                   (%sqlite-exec
                    (%sqlite-derived-handle derived :fixture)
                    "UPDATE pai_recursive_hot_shadow_v2_events SET root_event_id=999 WHERE agent_id='fixture' AND storage_position=7"
                    :fixture)
                   (check (handler-case
                              (progn
                                (storage-shadow-recursive-hot-read-page
                                 derived source :agent-id "fixture"
                                 :projector-revision "fixture-v1"
                                 :policy-revision "fixture-p1" :after-position 6)
                                nil)
                            (storage-integrity-error () t)))
                   (%sqlite-exec
                    (%sqlite-derived-handle derived :fixture)
                    "UPDATE pai_recursive_hot_shadow_v2_watermark SET source_binding='wrong' WHERE agent_id='fixture'"
                    :fixture)
                   (check (handler-case
                              (progn
                                (storage-shadow-recursive-hot-report
                                 derived source :agent-id "fixture"
                                 :projector-revision "fixture-v1"
                                 :policy-revision "fixture-p1")
                                nil)
                            (storage-conflict-error () t)))
                   (check (handler-case
                              (progn
                                (storage-shadow-recursive-hot-root-has-type-p
                                 derived source 7 "user-message"
                                 :agent-id "fixture" :projector-revision "fixture-v1"
                                 :policy-revision "fixture-p1")
                                nil)
                            (storage-conflict-error () t)))
                   (format t "Recursive hot shadow: ~d passed, 0 failed~%" checks))
              (storage-close derived)
              (storage-close source))))
        :want-stream-p nil :type "sqlite"))
     :want-stream-p nil :type "sqlite")))
