;;;; brave-credential.lisp -- private Brave credential resolution.

(in-package :agent)

(defparameter *brave-api-key-file*
  (or (uiop:getenv "BRAVE_API_KEY_FILE")
      (let ((root (uiop:getenv "PAI_SECRET_ROOT")))
        (and root (plusp (length root))
             (namestring (merge-pathnames "bravekey.txt" (pathname root)))))
      "/agent/state/bravekey.txt"))

(defun %brave-nonempty-secret (value)
  (when (stringp value)
    (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) value)))
      (when (plusp (length trimmed)) trimmed))))

(defun load-brave-api-key
    (&key (environment-value (uiop:getenv "BRAVE_API_KEY"))
          (file *brave-api-key-file*))
  "Resolve the Brave key without logging it. Environment wins; the private
state-file fallback supports sealed container deployments whose incumbent
environment predates Brave configuration. Empty and unreadable sources are
treated as unavailable."
  (or (%brave-nonempty-secret environment-value)
      (ignore-errors
        (when (and (or (stringp file) (pathnamep file)) (probe-file file))
          (%brave-nonempty-secret
           (uiop:read-file-string file))))))

(defparameter *brave-api-key* (load-brave-api-key))

(defun brave-api-key ()
  "Return the private Brave key, rechecking configured sources if boot found
none. Callers must never print or persist the returned value."
  (or *brave-api-key*
      (setf *brave-api-key* (load-brave-api-key))))

(defun brave-credential-status ()
  "Return content-free runtime truth suitable for health evidence."
  (if (brave-api-key) "available" "missing"))
