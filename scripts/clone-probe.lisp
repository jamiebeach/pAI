;;;; clone-probe.lisp -- after a clone boot, report what actually came back.
;;;;
;;;; A restore phase that reports 39/39 has proved it did not signal. It has
;;;; not proved it restored anything: an action that reads a missing file and
;;;; defaults to empty succeeds just as loudly. This asks the restored system
;;;; what it now holds.
;;;;
;;;; Read-only. Nothing here writes state or calls a provider.

(in-package :cl-user)

(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(push #P"/pai/" asdf:*central-registry*)

(let ((*standard-output* (make-broadcast-stream)))
  (asdf:load-system :pai))

(defmacro probe (label &body body)
  `(handler-case (format t "~&  ~28a ~a~%" ,label (progn ,@body))
     (error (e) (format t "~&  ~28a ERROR: ~a~%" ,label e))))

(defun sym (name) (intern (string-upcase name) :agent))
(defun val (name &optional default)
  (let ((s (sym name))) (if (boundp s) (symbol-value s) default)))
(defun call (name &rest args)
  (let ((s (sym name)))
    (if (fboundp s) (apply s args) :not-defined)))

(format t "~&== restoring ==~%")
(funcall (sym "initialize")
         :phases '(:configure :install :restore) :stop-on-error nil :verbose nil)

(format t "~&~%== conversation ==~%")
(probe "messages restored" (length (val "*conversation*")))
(probe "last-self-mod-history" (length (val "*last-self-mod-history*")))

(format t "~&~%== drives and affect ==~%")
(probe "drives" (let ((d (val "*drives*")))
                  (if (hash-table-p d) (hash-table-count d) d)))
(probe "modulators" (let ((m (val "*modulators*")))
                      (if (hash-table-p m) (hash-table-count m) m)))

(format t "~&~%== memory (database) ==~%")
;; The full retrieval path: embed the query via Ollama, let pgvector narrow by
;; nearest neighbour, then score in Lisp. If this returns rows, the restored
;; memory is genuinely readable rather than merely present.
(probe "embedding dimensions"
       (let ((e (call "embed-text" "what do you remember about pizza")))
         ;; Report the LENGTH. Printing the vector itself buries the report
         ;; under 768 floats.
         (cond ((vectorp e) (length e)) ((listp e) (length e)) (t e))))
(probe "recall rows"
       (let ((r (call "memory-recall" "what the operator prefers" :k 3)))
         (if (listp r) (length r) r)))
(probe "recall top hit"
       (let ((r (call "memory-recall" "what the operator prefers" :k 3)))
         (if (and (listp r) r)
             (let ((c (let ((row (first r))) (if (hash-table-p row) (gethash "content" row) row))))
               (if (stringp c) (subseq c 0 (min 90 (length c))) c))
             r)))

(format t "~&~%== scheduler / initiative ==~%")
(probe "schedules" (let ((s (val "*pai-schedules*")))
                     (if (hash-table-p s) (hash-table-count s) s)))
(probe "latent thoughts" (length (val "*latent-thoughts*")))
(probe "initiative candidates" (length (val "*initiative-candidates*")))

(format t "~&~%== identity ==~%")
(probe "agent id" (val "*agent-id*"))
(probe "operator id" (val "*operator-id*"))

(format t "~&~%CLONE-PROBE-DONE~%")
(finish-output)
