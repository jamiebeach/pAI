(defpackage :agent (:use :cl))
(in-package :agent)

(ql:quickload '(:dexador :shasht :cl-ppcre) :silent t)

(defvar *bounded-work-pass* 0)
(defvar *bounded-work-fail* 0)
(defun bounded-work-check (name condition)
  (if condition
      (progn (incf *bounded-work-pass*) (format t "PASS ~a~%" name))
      (progn (incf *bounded-work-fail*) (format t "FAIL ~a~%" name))))
(defun obj (&rest kvs)
  (loop with table = (make-hash-table :test #'equal)
        for (key value) on kvs by #'cddr
        do (setf (gethash key table) value)
        finally (return table)))
(defun ref (table &rest keys)
  (reduce (lambda (value key)
            (etypecase key
              (string (gethash key value))
              (integer (aref value key))))
          keys :initial-value table))
(defvar *tools* (vector))
(defvar *http-connect-timeout* 1)
(defun execute (tool-call) (declare (ignore tool-call)) :fallback)

(load (test-source "bounded-work-tools.lisp"))

(let* ((root #P"/tmp/pai-bounded-work-tools/")
       (*deliverable-root* root))
  (let* ((result (write-deliverable "pitch/draft.md" "grounded draft"))
         (decoded (shasht:read-json result)))
    (bounded-work-check "bounded writer succeeds atomically"
                        (and (gethash "written" decoded)
                             (= 14 (gethash "characters" decoded))))
    (let* ((read-result (read-deliverable "pitch/draft.md"))
           (read-decoded (shasht:read-json read-result)))
      (bounded-work-check "bounded reader returns exact small document"
                          (string= "grounded draft"
                                   (gethash "content" read-decoded)))))
  (let* ((unicode-text (format nil "follow-up ~c caf~c ~c"
                               (code-char #x2014) (code-char #xe9)
                               (code-char #x1f43e)))
         (write-result (shasht:read-json
                        (write-deliverable "pitch/unicode.md" unicode-text)))
         (read-result (shasht:read-json
                       (read-deliverable "pitch/unicode.md"))))
    (bounded-work-check "UTF-8 deliverable round trip preserves Unicode"
                        (and (gethash "written" write-result)
                             (string= unicode-text
                                      (gethash "content" read-result)))))
  (bounded-work-check "writer rejects parent traversal"
                      (search "ERROR:" (write-deliverable "../escape.md" "no")))
  (bounded-work-check "writer rejects executable extension"
                      (search "ERROR:" (write-deliverable "escape.lisp" "no")))
  (let ((*deliverable-read-max-chars* 120))
    (write-deliverable "large.md" (make-string 500 :initial-element #\x))
    (let ((decoded (shasht:read-json (read-deliverable "large.md"))))
      (bounded-work-check "large document preview is bounded"
                          (and (gethash "truncated" decoded)
                               (<= (length (gethash "content" decoded)) 120))))))

(let ((*bounded-web-fetch-fn*
        (lambda (url)
          (declare (ignore url))
          "<html><style>hidden</style><body>Agent Foundry facts</body></html>")))
  (let ((decoded (shasht:read-json (web-fetch "https://example.com/page"))))
    (bounded-work-check "exact public URL is fetched through dedicated seam"
                        (search "Agent Foundry facts"
                                (gethash "content" decoded)))
    (bounded-work-check "HTML scaffolding is removed"
                        (not (search "<body>" (gethash "content" decoded))))))

(bounded-work-check "localhost fetch is rejected"
                    (search "ERROR:" (web-fetch "http://127.0.0.1/private")))
(bounded-work-check "private-network fetch is rejected"
                    (search "ERROR:" (web-fetch "http://192.168.1.2/private")))

(dolist (name '("web-fetch" "write-deliverable" "read-deliverable"))
  (bounded-work-check
   (format nil "~a tool is advertised" name)
   (find name *tools* :key (lambda (tool) (ref tool "function" "name"))
         :test #'string=)))

(let ((source (string-downcase
               (uiop:read-file-string (namestring (test-source "bounded-work-tools.lisp"))))))
  (bounded-work-check "work tools cannot send public messages"
                      (and (not (search "telegram-send" source))
                           (not (search "broadcast-image" source))
                           (not (search "public-outbound-envelope" source)))))

(format t "~%BOUNDED WORK TOOLS: ~d passed, ~d failed.~%"
        *bounded-work-pass* *bounded-work-fail*)
(when (plusp *bounded-work-fail*) (uiop:quit 1))
