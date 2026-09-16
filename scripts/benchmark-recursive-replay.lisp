;;;; benchmark-recursive-replay.lisp -- read-only recursive authority timing.
;;;;
;;;; PAI_BENCHMARK_STATE names a disposable verified state copy.  This script
;;;; never targets the live state directory and never appends an event.

(in-package :cl-user)

(require :asdf)

(defparameter *benchmark-root*
  (uiop:ensure-directory-pathname
   (pathname (or (uiop:getenv "PAI_ROOT")
                 (error "PAI_ROOT is required")))))
(defparameter *benchmark-state*
  (uiop:ensure-directory-pathname
   (pathname (or (uiop:getenv "PAI_BENCHMARK_STATE")
                 (error "PAI_BENCHMARK_STATE is required")))))

(setf (uiop:getenv "PAI_STATE_ROOT") (namestring *benchmark-state*))
(uiop:chdir *benchmark-state*)
(asdf:load-asd (merge-pathnames "pai.asd" *benchmark-root*))
(asdf:load-system :pai)

(in-package :agent)

(defparameter *benchmark-state*
  (uiop:ensure-directory-pathname
   (pathname (or (uiop:getenv "PAI_BENCHMARK_STATE")
                 (error "PAI_BENCHMARK_STATE is required")))))

(defun %recursive-replay-benchmark-ms (thunk)
  (let ((started (get-internal-real-time)))
    (prog1 (funcall thunk)
      (setf started
            (* 1000d0
               (/ (- (get-internal-real-time) started)
                  internal-time-units-per-second))))))

(defun %recursive-replay-percentile (values percentile)
  (let* ((ordered (sort (copy-list values) #'<))
         (index (min (1- (length ordered))
                     (1- (ceiling (* percentile (length ordered)))))))
    (nth index ordered)))

(defun %recursive-replay-fingerprint (events)
  (let ((ids (mapcar (lambda (event) (gethash "id" event 0)) events)))
    (format nil "count=~d;first=~a;last=~a;id-sum=~d"
            (length ids) (or (first ids) 0) (or (car (last ids)) 0)
            (reduce #'+ ids :initial-value 0))))

(let* ((database (merge-pathnames "events.sqlite3" *benchmark-state*))
       (samples nil)
       (fingerprints nil))
  (unwind-protect
       (progn
         ;; This benchmark needs only the replay port.  Installing it directly
         ;; avoids rebuilding or accepting a stale derived conscious
         ;; checkpoint merely because the source tree advanced after the
         ;; snapshot was sealed.
         (let ((backend (make-sqlite-storage database)))
           (%sqlite-authority-install
            backend database backend nil "q45-conversation-dev"))
         ;; One unreported warm-up separates module/database initialization
         ;; from steady recursive-boundary cost.
         (%recursive-thread-events)
         (dotimes (index 20)
           (declare (ignore index))
           (let ((events nil)
                 (started (get-internal-real-time)))
             (setf events (%recursive-thread-events))
             (push (* 1000d0
                      (/ (- (get-internal-real-time) started)
                         internal-time-units-per-second))
                   samples)
             (push (%recursive-replay-fingerprint events) fingerprints)))
         (unless (= 1 (length (remove-duplicates fingerprints
                                                  :test #'string=)))
           (error "Recursive replay result identity changed during benchmark"))
         (format t
                 "RECURSIVE-REPLAY-BENCHMARK n=20 p50_ms=~,3f p95_ms=~,3f min_ms=~,3f max_ms=~,3f cache_hits=~d cache_advances=~d cache_rebuilds=~d cache_fallbacks=~d ~a~%"
                 (%recursive-replay-percentile samples 0.50d0)
                 (%recursive-replay-percentile samples 0.95d0)
                 (reduce #'min samples) (reduce #'max samples)
                 *conscious-recursive-thread-events-cache-hits*
                 *conscious-recursive-thread-events-cache-advances*
                 *conscious-recursive-thread-events-cache-rebuilds*
                 *conscious-recursive-thread-events-cache-fallbacks*
                 (first fingerprints)))
    (when *event-authority-port*
      (ignore-errors (event-authority-clear)))))
