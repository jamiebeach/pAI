(in-package :agent)

(let* ((legacy-size (%event-file-byte-length *event-log-file*))
       (before *event-next-id*)
       (id (log-event "r0d-candidate-live-probe"
                      (obj "source" "network-disabled-restored-clone")))
       (watermark (%event-read-watermark))
       (segment (%event-segment-path id))
       (matches (replay-events :types '("r0d-candidate-live-probe")
                               :limit 1))
       (passed
         (and (= id (1+ before))
              watermark
              (= id (gethash "last_reserved_id" watermark))
              (probe-file segment)
              (= legacy-size (%event-file-byte-length *event-log-file*))
              (= 1 (length matches))
              (= id (gethash "id" (first matches))))))
  (format t "~&R0D_CANDIDATE_LIVE_PROBE_~a id=~d legacy_bytes=~d segment=~a~%"
          (if passed "PASS" "FAIL") id legacy-size
          (file-namestring segment))
  (if passed :r0d-candidate-live-probe-pass
      :r0d-candidate-live-probe-fail))
