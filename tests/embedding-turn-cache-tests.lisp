(in-package :agent)

(ql:quickload :bordeaux-threads :silent t)

(defvar *embedding-cache-test-calls* 0)
(defvar *timing-installed-wrappers* nil)
(defvar *ollama-endpoint* nil)
(defvar *ollama-embed-model* nil)

(defun embed-text (text)
  (incf *embedding-cache-test-calls*)
  (list (length text) 1.0d0))

(load (test-source "embedding-turn-cache.lisp"))

(let ((passed 0) (failed 0))
  (flet ((check (name condition)
           (if condition
               (progn (incf passed) (format t "PASS ~a~%" name))
               (progn (incf failed) (format t "FAIL ~a~%" name)))))
    (setf *embedding-cache-test-calls* 0)
    (embed-text "same")
    (embed-text "same")
    (check "disabled cache preserves two base calls"
           (= 2 *embedding-cache-test-calls*))

    (setf *embedding-cache-test-calls* 0)
    (let ((*embedding-turn-cache* (make-hash-table :test #'equal)))
      (let ((first (embed-text "same"))
            (second (embed-text "same")))
        (check "enabled cache returns equal vector" (equal first second))
        (check "enabled cache calls base once" (= 1 *embedding-cache-test-calls*)))
      (embed-text "different")
      (check "different text has distinct cache entry"
             (= 2 *embedding-cache-test-calls*)))

    (setf *embedding-cache-test-calls* 0)
    (let ((*embedding-turn-cache* (make-hash-table :test #'equal))
          (*ollama-endpoint* "fixture-a")
          (*ollama-embed-model* "fixture-model"))
      (embed-text "same")
      (embed-text "same")
      (setf *ollama-endpoint* "fixture-b")
      (embed-text "same")
      (check "embedding configuration change has a distinct cache identity"
             (= 2 *embedding-cache-test-calls*)))

    (let* ((wrapper (fdefinition 'embed-text))
           (true-base (fdefinition 'pai-base-embed-text-turn-cache))
           (timing-wrapper (lambda (text)
                             (funcall 'pai-base-embed-text-timing text)))
           (*timing-installed-wrappers* (make-hash-table :test #'eq)))
      (setf (fdefinition 'pai-base-embed-text-timing) wrapper
            (gethash 'embed-text *timing-installed-wrappers*) timing-wrapper
            (fdefinition 'embed-text) timing-wrapper)
      (load (test-source "embedding-turn-cache.lisp"))
      (check "reload under timing retains true cache base"
             (eq true-base (fdefinition 'pai-base-embed-text-turn-cache)))
      (setf *embedding-cache-test-calls* 0)
      (let ((*embedding-turn-cache* (make-hash-table :test #'equal)))
        (embed-text "reload")
        (embed-text "reload")
        (check "timing-order reload remains callable and cached"
               (= 1 *embedding-cache-test-calls*))))

    (format t "~%EMBEDDING TURN CACHE TESTS: ~a passed, ~a failed.~%"
            passed failed)
    (when (plusp failed) (uiop:quit 1))))
