;;;; conversation-episode-graph-capacity.lisp -- KG1 bounded scale probe.

(in-package :agent)

(ql:quickload '(:cffi :shasht :ironclad :bordeaux-threads) :silent t)

(dolist (file '("storage-substrate.lisp" "memory-storage.lisp"
                "sqlite-storage.lisp"
                "sqlite-derived-storage.lisp"
                "conversation-episode-graph.lisp"
                "conversation-episode-graph-storage.lisp"))
  (load (test-source file)))

(defun ceg-capacity-episode (index &optional revised-p)
  (let ((episode (make-hash-table :test #'equal))
        (start (+ 1000 (* index 3))))
    (setf (gethash "episode_id" episode) (format nil "episode:capacity:~8,'0d" index)
          (gethash "event_id" episode) (+ 100000 index (if revised-p 100000 0))
          (gethash "persona_id" episode) "capacity-persona"
          (gethash "first_event_id" episode) start
          (gethash "last_event_id" episode) (1+ start)
          (gethash "first_timestamp" episode) start
          (gethash "last_timestamp" episode) (1+ start)
          (gethash "source_event_ids" episode) (vector start (1+ start))
          (gethash "synopsis" episode)
          (format nil "Synthetic episode ~d~:[.~; revised.~]" index revised-p)
          (gethash "subjects" episode) (vector "shared subject")
          (gethash "entities" episode) (vector (format nil "entity ~d" (mod index 97)))
          (gethash "retrieval_cues" episode)
          (vector (format nil "topic ~d" (mod index 503)))
          (gethash "broader_categories" episode) (vector "capacity category")
          (gethash "unresolved_threads" episode) (vector))
    episode))

(defun ceg-capacity-delete (path)
  (dolist (candidate
           (list path
                 (pathname (concatenate 'string (namestring path) "-wal"))
                 (pathname (concatenate 'string (namestring path) "-shm"))))
    (when (probe-file candidate) (delete-file candidate))))

(defun ceg-capacity-ms (thunk)
  (let ((start (get-internal-real-time)))
    (multiple-value-prog1 (funcall thunk)
      (format t "~d"
              (round (* 1000
                        (/ (- (get-internal-real-time) start)
                           internal-time-units-per-second)))))))

(format t "depth,cold_ms,warm_ms,tail_ms,db_bytes,equivalent~%")

(dolist (depth '(100 1000 10000))
  (let* ((path (merge-pathnames
                (format nil "ceg-capacity-~d.sqlite3" depth)
                (test-state-dir)))
         (backend nil)
         (episodes (coerce (loop for index below depth
                                 collect (ceg-capacity-episode index))
                           'vector))
         (cold-ms 0) (warm-ms 0) (tail-ms 0) (equivalent nil))
    (ceg-capacity-delete path)
    (unwind-protect
        (progn
          (setf backend (make-sqlite-derived-storage path))
          (let ((start (get-internal-real-time)))
            (conversation-episode-graph-persist
             backend
             (conversation-episode-graph-materialization
              episodes "capacity-agent" "capacity-persona")
             :through-event-id depth :through-position depth
             :event-storage-id "capacity-ledger" :boundary-hash "cold")
            (setf cold-ms
                  (round (* 1000
                            (/ (- (get-internal-real-time) start)
                               internal-time-units-per-second)))))
          (let ((start (get-internal-real-time)))
            (conversation-episode-graph-restore
             backend "capacity-agent" "capacity-persona"
             :event-storage-id "capacity-ledger")
            (setf warm-ms
                  (round (* 1000
                            (/ (- (get-internal-real-time) start)
                               internal-time-units-per-second)))))
          (setf (aref episodes (1- depth))
                (ceg-capacity-episode (1- depth) t))
          ;; The warm-restore probe intentionally allocates the full verified
          ;; generation. Reclaim that discarded probe result before measuring
          ;; the ordinary tail path, whose coordinator never performs it.
          (sb-ext:gc :full t)
          (let ((start (get-internal-real-time)))
            (conversation-episode-graph-persist-tail
             backend
             (conversation-episode-graph-materialization
              (vector (aref episodes (1- depth)))
              "capacity-agent" "capacity-persona")
             (vector (gethash "episode_id" (aref episodes (1- depth))))
             :through-event-id (1+ depth) :through-position (1+ depth)
             :event-storage-id "capacity-ledger" :boundary-hash "tail")
            (setf tail-ms
                  (round (* 1000
                            (/ (- (get-internal-real-time) start)
                               internal-time-units-per-second)))))
          (multiple-value-bind (restored ignored)
              (conversation-episode-graph-restore
               backend "capacity-agent" "capacity-persona"
               :event-storage-id "capacity-ledger")
            (declare (ignore ignored))
            (setf equivalent
                  (string= (shasht:write-json episodes nil)
                           (shasht:write-json restored nil))))
          (storage-close backend)
          (setf backend nil)
          (with-open-file (stream path :direction :input
                                       :element-type '(unsigned-byte 8))
            (format t "~d,~d,~d,~d,~d,~:[false~;true~]~%"
                    depth cold-ms warm-ms tail-ms (file-length stream)
                    equivalent)))
      (when backend (ignore-errors (storage-close backend)))
      (ceg-capacity-delete path))))
