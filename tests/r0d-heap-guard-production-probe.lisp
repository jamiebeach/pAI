(in-package :agent)

(let* ((report (heap-health-report))
       (latest (gethash "latest" report))
       (latest-gc
         (bt:with-lock-held (*heap-health-lock*)
           (find-if (lambda (sample) (gethash "full_gc_ran" sample))
                    *heap-health-samples*)))
       (usage (gethash "usage_bytes" report))
       (limit (gethash "limit_bytes" report)))
  (format t
          "~&R0D_HEAP_GUARD_PROBE usage=~d ratio=~,6f thread_alive=~a samples=~d latest_status=~a latest_before=~a latest_after=~a latest_gc=~a~%"
          usage
          (/ (float usage 1.0d0) (float limit 1.0d0))
          (gethash "thread_alive" report)
          (gethash "sample_count" report)
          (if (hash-table-p latest) (gethash "status" latest) :null)
          (if (hash-table-p latest) (gethash "usage_before_bytes" latest) :null)
          (if (hash-table-p latest) (gethash "usage_bytes" latest) :null)
          (if (hash-table-p latest) (gethash "full_gc_ran" latest) :null))
  (format t
          "R0D_HEAP_GUARD_LAST_GC sampled_at=~a before=~a after=~a status=~a~%"
          (if latest-gc (gethash "sampled_at" latest-gc) :null)
          (if latest-gc (gethash "usage_before_bytes" latest-gc) :null)
          (if latest-gc (gethash "usage_bytes" latest-gc) :null)
          (if latest-gc (gethash "status" latest-gc) :null))
  :r0d-heap-guard-probe-complete)
