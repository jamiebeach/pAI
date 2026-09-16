(in-package :agent)

(ql:quickload :bordeaux-threads :silent t)

(defvar *cache-integration-embed-calls* 0)
(defvar *context-projection-mode* :legacy)
(defvar *timing-installed-wrappers* nil)
(defvar *last-self-mod-history*
  (list (obj "role" "system" "content" "Stable system prompt")))
(defvar *cache-integration-events* nil)

(defun embed-text (text)
  (incf *cache-integration-embed-calls*)
  (list (length text) 1.0d0))
(defun memory-search (query &rest arguments)
  (declare (ignore arguments))
  (embed-text query)
  nil)
(defun replay-events (&rest arguments) (declare (ignore arguments)) nil)
(defun modulator-state () (obj))
(defun drive-state () (obj))
(defun log-event (type payload)
  (push (list type payload) *cache-integration-events*)
  nil)
(defun auto-turn (prompt)
  ;; Models the inner legacy memory injector's same-query embedding call.
  (embed-text prompt)
  (format nil "reply:~a" prompt))

(load (test-source "embedding-turn-cache.lisp"))
(load (test-source "context-projection.lisp"))

(let ((passed 0) (failed 0))
  (flet ((check (name condition)
           (if condition
               (progn (incf passed) (format t "PASS ~a~%" name))
               (progn (incf failed) (format t "FAIL ~a~%" name)))))
    (setf *cache-integration-embed-calls* 0
          *embedding-turn-cache-hits* 0
          *embedding-turn-cache-misses* 0
          *cache-integration-events* nil)
    (let ((*context-projection-mode* :shadow))
      (check "shadow preserves exact base reply"
             (string= "reply:shared query" (auto-turn "shared query"))))
    (check "projection and inner legacy retrieval share one embedding call"
           (= 1 *cache-integration-embed-calls*))
    (check "shared query records one miss and one hit"
           (and (= 1 *embedding-turn-cache-misses*)
                (= 1 *embedding-turn-cache-hits*)))
    (check "turn cache is not retained after dynamic scope"
           (null *embedding-turn-cache*))
    (let* ((event (find "embedding-turn-cache-turn"
                        *cache-integration-events*
                        :key #'first :test #'string=))
           (payload (second event)))
      (check "per-turn cache audit is emitted"
             (not (null event)))
      (check "per-turn cache audit reports exact hit and miss"
             (and (= 1 (gethash "hits" payload))
                  (= 1 (gethash "misses" payload))
                  (= 1 (gethash "entry_count" payload)))))

    (setf *cache-integration-embed-calls* 0)
    (let ((*context-projection-mode* :legacy))
      (auto-turn "legacy query"))
    (check "legacy path remains a single uncached base call"
           (= 1 *cache-integration-embed-calls*))

    (format t "~%CONTEXT CACHE INTEGRATION TESTS: ~a passed, ~a failed.~%"
            passed failed)
    (when (plusp failed) (uiop:quit 1))))
