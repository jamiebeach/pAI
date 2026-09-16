(in-package :agent)

(let* ((watermark (%event-read-watermark))
       (segments (%event-segment-paths))
       (usage (sb-kernel:dynamic-usage))
       (limit (sb-ext:dynamic-space-size))
       (passed (and *event-log-segmentation-enabled*
                    *event-log-segmentation-ready-p*
                    watermark
                    (= *event-next-id* (gethash "last_reserved_id" watermark))
                    (plusp (length segments))
                    (< usage (* 0.50d0 limit)))))
  (format t
          "~&R0D_PRODUCTION_POST_CUTOVER_~a next_id=~d watermark=~d segments=~d legacy_bytes=~d heap_bytes=~d heap_ratio=~,6f~%"
          (if passed "PASS" "FAIL")
          *event-next-id*
          (if watermark (gethash "last_reserved_id" watermark) -1)
          (length segments)
          (%event-file-byte-length *event-log-file*)
          usage
          (/ (float usage 1.0d0) (float limit 1.0d0)))
  (if passed :r0d-production-post-cutover-pass
      :r0d-production-post-cutover-fail))
