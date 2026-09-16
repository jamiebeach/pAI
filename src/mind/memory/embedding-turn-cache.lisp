;;;; embedding-turn-cache.lisp -- per-turn duplicate query elimination.
;;;;
;;;; Projection and the legacy shared-memory injector both retrieve against
;;;; the same prompt in shadow. Cache only in the dynamic public turn so the
;;;; second caller reuses the exact local embedding. Nothing persists.

(in-package :agent)

(export '(embedding-turn-cache-report))

(defvar *embedding-turn-cache* nil)
(defvar *embedding-turn-cache-hits* 0)
(defvar *embedding-turn-cache-misses* 0)
(defvar *embedding-turn-cache-turn-hits* 0)
(defvar *embedding-turn-cache-turn-misses* 0)
(defvar *embedding-turn-cache-lock* (bt:make-lock "embedding-turn-cache"))
(defvar *ollama-endpoint*)
(defvar *ollama-embed-model*)
(defvar *retrieval-embedding-mode*)
(defvar *embedding-fallback-policy*)

(defun %embedding-turn-cache-key (text)
  ;; Configuration is part of identity: a process-local work cache must never
  ;; reuse a vector after an operator changes endpoint, model or fallback
  ;; contract between quanta.
  (list (and (boundp '*ollama-endpoint*) *ollama-endpoint*)
        (and (boundp '*ollama-embed-model*) *ollama-embed-model*)
        (and (boundp '*retrieval-embedding-mode*)
             *retrieval-embedding-mode*)
        (and (boundp '*embedding-fallback-policy*)
             *embedding-fallback-policy*)
        text))

(defun embedding-turn-cache-report ()
  (bt:with-lock-held (*embedding-turn-cache-lock*)
    (obj "schema_version" 1
         "scope" "dynamic-public-turn-only"
         "enabled" (not (null (hash-table-p *embedding-turn-cache*)))
         "hits" *embedding-turn-cache-hits*
         "misses" *embedding-turn-cache-misses*)))

(defun %embedding-turn-cache-stat (slot)
  (bt:with-lock-held (*embedding-turn-cache-lock*)
    (ecase slot
      (:hit
       (incf *embedding-turn-cache-hits*)
       (incf *embedding-turn-cache-turn-hits*))
      (:miss
       (incf *embedding-turn-cache-misses*)
       (incf *embedding-turn-cache-turn-misses*)))))

(defvar *embedding-turn-cache-installed-wrapper* nil)
(let* ((current (fdefinition 'embed-text))
       (effective
         (if (and (boundp '*timing-installed-wrappers*)
                  (hash-table-p (symbol-value '*timing-installed-wrappers*))
                  (gethash 'embed-text
                           (symbol-value '*timing-installed-wrappers*))
                  (eq current
                      (gethash 'embed-text
                               (symbol-value '*timing-installed-wrappers*)))
                  (fboundp 'pai-base-embed-text-timing))
             (fdefinition 'pai-base-embed-text-timing)
             current)))
  (unless (and *embedding-turn-cache-installed-wrapper*
               (or (eq current *embedding-turn-cache-installed-wrapper*)
                   (eq effective *embedding-turn-cache-installed-wrapper*)))
    (setf (fdefinition 'pai-base-embed-text-turn-cache) effective)))

(defun embed-text (text)
  (if (not (hash-table-p *embedding-turn-cache*))
      (funcall 'pai-base-embed-text-turn-cache text)
      (let ((key (%embedding-turn-cache-key text)))
        (multiple-value-bind (cached present-p)
          (gethash key *embedding-turn-cache*)
        (if present-p
            (progn (%embedding-turn-cache-stat :hit) cached)
            (let ((embedding
                    (funcall 'pai-base-embed-text-turn-cache text)))
              (setf (gethash key *embedding-turn-cache*) embedding)
              (%embedding-turn-cache-stat :miss)
              embedding))))))

(setf *embedding-turn-cache-installed-wrapper* (fdefinition 'embed-text))
