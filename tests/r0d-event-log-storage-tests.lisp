(defpackage :agent
  (:use :cl))

(in-package :agent)

(ql:quickload '(:bordeaux-threads :shasht) :silent t)

(defvar *r0d-pass* 0)
(defvar *r0d-fail* 0)

(defun r0d-check (name condition)
  (if condition
      (progn (incf *r0d-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *r0d-fail*) (format t "  FAIL ~a~%" name))))

(defun obj (&rest kvs)
  (loop with table = (make-hash-table :test #'equal)
        for (key value) on kvs by #'cddr
        do (setf (gethash key table) value)
        finally (return table)))

(defun r0d-event (id type timestamp)
  (obj "schema_version" 2 "id" id "timestamp" timestamp "type" type
       "payload" (obj "value" id) "caused_by" :null "tick_id" :null
       "affect_snapshot" (obj "status" "fixture")))

(defun r0d-write-events (pathname events)
  (ensure-directories-exist pathname)
  (with-open-file (out pathname :direction :output :if-exists :supersede
                                :if-does-not-exist :create
                                :external-format :utf-8)
    (let ((*print-pretty* nil))
      (dolist (event events)
        (write-line (shasht:write-json event nil) out)))))

(defun r0d-json= (left right)
  (let ((*print-pretty* nil))
    (string= (shasht:write-json left nil)
             (shasht:write-json right nil))))

(defun r0d-remove-tree (pathname)
  (when (probe-file pathname)
    (uiop:delete-directory-tree pathname :validate t
                                         :if-does-not-exist :ignore)))

;; EVENT-LOG's wrappers need legacy call points, but this test exercises only
;; storage and replay.
(setf (fdefinition 'auto-turn) (lambda (prompt) prompt)
      (fdefinition 'execute) (lambda (&rest arguments)
                               (declare (ignore arguments)) nil)
      (fdefinition 'propose-loop) (lambda (&rest arguments)
                                    (declare (ignore arguments)) nil))
(defparameter *tools* (vector))

(load (test-source "event-log.lisp"))

(let* ((root #P"/tmp/pai-r0d-storage-tests/")
       (legacy (merge-pathnames "events.jsonl" root))
       (segments (merge-pathnames "segments/" root))
       (watermark (merge-pathnames "watermark.json" root))
       (legacy-index (merge-pathnames "legacy-index.json" root))
       (segment-indexes (merge-pathnames "segment-indexes/" root))
       (checkpoints (merge-pathnames "checkpoints/" root))
       (reference-root #P"/tmp/pai-r0d-reference-tests/")
       (reference (merge-pathnames "events.jsonl" reference-root)))
  (r0d-remove-tree root)
  (r0d-remove-tree reference-root)
  (unwind-protect
      (let ((*event-log-file* legacy)
            (*event-log-segment-directory* segments)
            (*event-log-watermark-file* watermark)
            (*event-log-legacy-index-file* legacy-index)
            (*event-log-segment-index-directory* segment-indexes)
            (*event-checkpoint-directory* checkpoints)
            (*event-log-segment-span* 2)
            (*event-log-legacy-index-stride* 1)
            (*event-log-segment-index-stride* 1)
            (*event-log-segmentation-enabled* t)
            (*event-log-segmentation-ready-p* nil)
            (*event-next-id* 0)
            (*event-ring* nil))
        (format t "~%== deterministic cutover and replay ==~%")
        (r0d-write-events
         legacy
         (list (r0d-event 1 "legacy-a" "2026-08-08T00:00:00Z")
               (r0d-event 2 "legacy-b" "2026-08-08T00:00:01Z")))
        (let ((initial (event-log-initialize-segmentation)))
          (r0d-check "cutover derives legacy max without rewriting legacy"
                     (and (= 2 (gethash "last_reserved_id" initial))
                          (= 2 *event-next-id*)
                          *event-log-segmentation-ready-p*
                          (= 2 (length (replay-events))))))
        (let ((offset
                (%event-legacy-start-offset
                 (%event-parse-ts-string "2026-08-08T00:00:01Z"))))
          (r0d-check "bounded legacy replay seeks through verified sparse index"
                     (and (probe-file legacy-index) (plusp offset))))
        (r0d-check "boundary IDs map to deterministic fixed ranges"
                   (equal '("events-000000000001-000000000002.jsonl"
                            "events-000000000003-000000000004.jsonl"
                            "events-000000000005-000000000006.jsonl")
                          (mapcar #'%event-segment-file-name '(1 3 6))))
        (log-event "segment-a" (obj "value" 3))
        (sleep 1)
        (let ((boundary (get-universal-time)))
          (log-event "segment-b" (obj "value" 4))
          (log-event "segment-c" (obj "value" 5))
          (log-event "segment-d" (obj "value" 6))
          (r0d-check "bounded replay crosses segments in order"
                     (equal '("segment-b" "segment-c" "segment-d")
                            (mapcar (lambda (event) (gethash "type" event))
                                    (replay-events :from boundary)))))
        (multiple-value-bind (scan-p start)
            (%event-segment-scan-plan
             (%event-segment-path 3) nil
             (%event-parse-ts-string "2026-08-08T00:00:00Z"))
          (declare (ignore start))
          (r0d-check "complete non-overlapping segment is skipped"
                     (not scan-p)))
        (let* ((first-segment (%event-segment-path 3))
               (first-index (%event-read-segment-index first-segment))
               (second-entry (aref (gethash "entries" first-index) 1)))
          (multiple-value-bind (scan-p start)
              (%event-segment-scan-plan
               first-segment
               (1+ (gethash "max_timestamp_before_offset" second-entry)) nil)
            (r0d-check "overlapping segment seeks to a sparse byte offset"
                       (and scan-p (plusp start)))))
        (let ((multi (replay-events)))
          (r0d-write-events reference multi)
          (let ((*event-log-file* reference)
                (*event-log-segment-directory*
                  (merge-pathnames "empty-segments/" reference-root)))
            (r0d-check "multi-segment replay equals single-file replay"
                       (r0d-json= (coerce multi 'vector)
                                  (coerce (replay-events) 'vector)))))
        (r0d-check "rollover creates two exact range segments"
                   (equal '("events-000000000003-000000000004.jsonl"
                            "events-000000000005-000000000006.jsonl")
                          (mapcar #'file-namestring (%event-segment-paths))))
        (r0d-check "bounded replay retains only newest matching event types"
                   (equal '("segment-c" "segment-d")
                          (mapcar (lambda (event) (gethash "type" event))
                                  (replay-events
                                   :limit 2
                                   :types '("segment-b" "segment-c"
                                            "segment-d")))))

        (format t "~%== watermark-only boot and crash holes ==~%")
        (let* ((opened nil)
              (*event-storage-open-observer*
                (lambda (operation pathname)
                  (push (list operation (file-namestring pathname)) opened))))
          (setf *event-next-id* 0 *event-log-segmentation-ready-p* nil)
          (r0d-check "watermark restore returns persisted maximum"
                     (= 6 (%event-restore-next-id)))
          (setf opened (nreverse opened))
          (r0d-check "boot opens only watermark and tail sidecar"
                     (and (= 2 (length opened))
                          (equal '(:read :read) (mapcar #'first opened))
                          (equal '("watermark.json"
                                   "events-000000000005-000000000006.jsonl.index.json")
                                 (mapcar #'second opened)))))
        (%event-write-watermark 7)
        (setf *event-next-id* 0 *event-log-segmentation-ready-p* nil)
        (r0d-check "reserved but unappended ID is never reused"
                   (and (= 7 (%event-restore-next-id))
                        (= 8 (log-event "after-hole" (obj "value" 8)))))
        (setf *event-log-segmentation-enabled* nil
              *event-next-id* 0
              *event-log-segmentation-ready-p* nil)
        (r0d-check "valid watermark durably latches segmented mode on restart"
                   (and (= 8 (%event-restore-next-id))
                        *event-log-segmentation-enabled*
                        *event-log-segmentation-ready-p*))
        (with-open-file (out (%event-segment-path 8)
                             :direction :output :if-exists :append)
          (write-line "{malformed" out))
        (r0d-check "malformed segment never returns partial replay"
                   (null (replay-events)))
        (setf *event-next-id* 0 *event-log-segmentation-ready-p* nil)
        (r0d-check "corrupt tail cannot lower reserved watermark"
                   (= 8 (%event-restore-next-id)))

        (format t "~%== verified checkpoints ==~%")
        (write-event-checkpoint "conversation" 4 "first checkpoint")
        (write-event-checkpoint "conversation" 8
                                "latest checkpoint — pancakes")
        (multiple-value-bind (content manifest)
            (read-verified-event-checkpoint "conversation")
          (r0d-check "latest exact UTF-8 checkpoint verifies"
                     (and (string= "latest checkpoint — pancakes" content)
                          (= 8 (gethash "event_id" manifest))
                          (= (length (%event-utf8-octets content))
                             (gethash "byte_length" manifest)))))
        (with-open-file
            (out (%event-checkpoint-path
                  (%event-checkpoint-data-name "conversation" 8))
                 :direction :output :if-exists :supersede)
          (write-string "truncated" out))
        (multiple-value-bind (content manifest)
            (read-verified-event-checkpoint "conversation")
          (r0d-check "corrupt latest checkpoint falls back to latest verified"
                     (and (string= "first checkpoint" content)
                          (= 4 (gethash "event_id" manifest)))))
        (let* ((orphan (write-event-checkpoint "conversation" 12 "orphan"))
               (published
                 (%event-checkpoint-path
                  (%event-checkpoint-manifest-name "conversation" 12)))
               (temporary
                 (%event-checkpoint-path
                  "checkpoint-conversation-000000000012-tmp-1.json")))
          (declare (ignore orphan))
          (rename-file published temporary)
          (multiple-value-bind (content manifest)
              (read-verified-event-checkpoint "conversation")
            (r0d-check "crash-left temporary manifest is never published"
                       (and (string= "first checkpoint" content)
                            (= 4 (gethash "event_id" manifest))))))
        (r0d-check "unsafe checkpoint names are rejected"
                   (handler-case
                       (progn (write-event-checkpoint "../escape" 1 "bad") nil)
                     (error () t))))
    (r0d-remove-tree root)
    (r0d-remove-tree reference-root)))

(let* ((root #P"/tmp/pai-r0d-invalid-watermark-tests/")
       (legacy (merge-pathnames "events.jsonl" root))
       (segments (merge-pathnames "segments/" root))
       (watermark (merge-pathnames "watermark.json" root)))
  (r0d-remove-tree root)
  (unwind-protect
      (let ((*event-log-file* legacy)
            (*event-log-segment-directory* segments)
            (*event-log-watermark-file* watermark)
            (*event-log-legacy-index-file*
              (merge-pathnames "legacy-index.json" root))
            (*event-log-segment-index-directory*
              (merge-pathnames "segment-indexes/" root))
            (*event-log-segment-span* 2)
            (*event-log-segmentation-enabled* t)
            (*event-log-segmentation-ready-p* nil)
            (*event-next-id* 0)
            (*event-ring* nil))
        (format t "~%== invalid watermark and sink isolation ==~%")
        (r0d-write-events
         legacy (list (r0d-event 11 "legacy-only" "2026-08-08T00:00:00Z")))
        (let ((tampered (%event-watermark-for-id 11)))
          (setf (gethash "checksum_sha256" tampered) "tampered")
          (%event-atomic-write-json watermark tampered))
        (r0d-check "checksum-invalid watermark uses compatibility restore but is not ready"
                   (and (= 11 (%event-restore-next-id))
                        (not *event-log-segmentation-ready-p*)))
        (let* ((legacy-size (with-open-file (in legacy) (file-length in)))
               (returned
                 (multiple-value-list
                  (log-event "must-not-persist" (obj "value" 12)))))
          (r0d-check "requested segmentation refuses append without valid watermark"
                     (and (= 12 (first returned))
                          (null (second returned))
                          (null (third returned))
                          (= legacy-size
                             (with-open-file (in legacy) (file-length in)))
                          (null (%event-segment-paths)))))
        (let ((saved (fdefinition '%event-write-watermark))
              (returned nil))
          (unwind-protect
              (progn
                (setf (fdefinition '%event-write-watermark)
                      (lambda (id) (declare (ignore id))
                        (error "forced watermark sink failure"))
                      *event-log-segmentation-ready-p* t)
                (setf returned (log-event "sink-failure" (obj "value" 13))))
            (setf (fdefinition '%event-write-watermark) saved))
          (r0d-check "watermark sink failure cannot escape or append"
                     (and (= 13 returned) (null (%event-segment-paths))))))
    (r0d-remove-tree root)))

(let* ((root #P"/tmp/pai-r0d-stale-segment-index-tests/")
       (segments (merge-pathnames "segments/" root))
       (segment-indexes (merge-pathnames "segment-indexes/" root)))
  (r0d-remove-tree root)
  (unwind-protect
      (let ((*event-log-file* (merge-pathnames "events.jsonl" root))
            (*event-log-segment-directory* segments)
            (*event-log-segment-index-directory* segment-indexes)
            (*event-log-segment-span* 10)
            (*event-log-segment-index-stride* 1))
        (format t "~%== stale and invalid segment indexes ==~%")
        (let ((path (%event-segment-path 1)))
          (r0d-write-events
           path (list (r0d-event 1 "first" "2026-08-08T00:00:00Z")
                      (r0d-event 2 "second" "2026-08-08T00:00:01Z")))
          (%event-write-built-segment-index path)
          ;; Bypass the normal writer to model a crash after append and before
          ;; its derived sidecar refresh.
          (%event-append-json-line
           path (r0d-event 3 "crash-tail" "2026-08-08T00:00:02Z"))
          (r0d-check "stale sidecar cannot skip an appended crash tail"
                     (equal '("crash-tail")
                            (mapcar (lambda (event) (gethash "type" event))
                                    (replay-events
                                     :from (%event-parse-ts-string
                                            "2026-08-08T00:00:02Z")))))
          (let ((tampered (%event-read-json-file
                           (%event-segment-index-path path))))
            (setf (gethash "checksum_sha256" tampered) "tampered")
            (%event-atomic-write-json (%event-segment-index-path path) tampered))
          (multiple-value-bind (scan-p start)
              (%event-segment-scan-plan
               path (%event-parse-ts-string "2026-08-08T00:00:02Z") nil)
            (r0d-check "checksum-invalid sidecar safely falls back to byte zero"
                       (and scan-p (zerop start))))))
    (r0d-remove-tree root)))

(let* ((root #P"/tmp/pai-r0d-type-prefilter-tests/")
       (path (merge-pathnames "events.jsonl" root))
       (seen nil))
  (r0d-remove-tree root)
  (unwind-protect
      (progn
        (ensure-directories-exist path)
        (with-open-file (out path :direction :output :if-exists :supersede
                                  :if-does-not-exist :create
                                  :external-format :utf-8)
          ;; Deliberately invalid JSON after the fields used by the raw-line
          ;; prefilter.  A post-decode type filter would fail this scan.
          (write-line
           "{\"timestamp\":\"2026-08-08T00:00:00Z\",\"type\":\"model-response\",\"payload\":NOT-JSON}"
           out)
          (let ((*print-pretty* nil))
            (write-line
             (shasht:write-json
              (r0d-event 2 "tick-end" "2026-08-08T00:00:01Z") nil)
             out)))
        (r0d-check
         "typed replay rejects large nonmatching rows before JSON decode"
         (and (%event-scan-file
               path
               (lambda (event) (push (gethash "type" event) seen))
               :types '("tick-end" "tick-terminal"))
              (equal '("tick-end") (nreverse seen)))))
    (r0d-remove-tree root)))

(format t "~%R0D EVENT STORAGE TESTS: ~d passed, ~d failed.~%"
        *r0d-pass* *r0d-fail*)
(when (plusp *r0d-fail*) (uiop:quit 1))
