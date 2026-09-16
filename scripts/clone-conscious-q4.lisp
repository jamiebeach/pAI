;;;; clone-conscious-q4.lisp -- contained Q4 captured-deliberation demo.

(in-package :cl-user)

(require :asdf)

(defparameter *q4-repo-root*
  (uiop:pathname-parent-directory-pathname
   (uiop:pathname-directory-pathname *load-truename*)))

(defun %q4-quicklisp-setup ()
  (let ((configured (uiop:getenv "PAI_QUICKLISP_SETUP")))
    (or (and configured (probe-file configured))
        (probe-file (merge-pathnames #P".tools/quicklisp/setup.lisp"
                                     *q4-repo-root*))
        (probe-file (merge-pathnames #P"quicklisp/setup.lisp"
                                     (user-homedir-pathname)))
        (error "Quicklisp setup not found; run scripts/bootstrap-local-lisp.lisp"))))

(load (%q4-quicklisp-setup))
(push *q4-repo-root* asdf:*central-registry*)

(defun %q4-guard-clone ()
  (let ((host (or (uiop:getenv "PAI_PG_HOST") ""))
        (label (or (uiop:getenv "PAI_DEV_DATABASE_LABEL") "")))
    (unless (or (search "clone" host :test #'char-equal)
                (and (member host '("127.0.0.1" "localhost" "::1")
                             :test #'string-equal)
                     (search "clone" label :test #'char-equal)))
      (error "Refusing Q4 demo: database host ~s / label ~s is not a labelled clone"
             host label))))

(defun %q4-symbol (name) (intern (string-upcase name) :agent))
(defun %q4-call (name &rest arguments)
  (apply (symbol-function (%q4-symbol name)) arguments))

(%q4-guard-clone)

(let ((manifest
        (namestring
         (merge-pathnames
          #P".pai-q4-demo-manifest.json"
          (uiop:ensure-directory-pathname
           (pathname (or (uiop:getenv "PAI_STATE_ROOT")
                         (uiop:temporary-directory))))))))
  (with-open-file (stream manifest :direction :output :if-exists :supersede
                                   :if-does-not-exist :create)
    (write-string
     "{\"schema_version\":1,\"run_id\":\"q4-captured-demo\",\"disposable\":true,\"database_label_verified\":true,\"network_internal\":true,\"ingress_scope\":\"loopback-only\",\"provider_egress\":\"disabled\",\"delivery_authority\":\"absent\"}"
     stream))
  (setf (uiop:getenv "PAI_DEV_WORKBENCH") "enabled"
        (uiop:getenv "PAI_DEV_RUN_ID") "q4-captured-demo"
        (uiop:getenv "PAI_DEV_MANIFEST_FILE") manifest))

(format t "~&== loading pAI ==~%")
(let ((*standard-output* (make-broadcast-stream)))
  (asdf:load-system :pai))
;; The dev workbench is intentionally outside the production ASDF load chain.
;; This explicit demo-only load is the capability boundary.
(let ((*standard-output* (make-broadcast-stream)))
  (load (merge-pathnames #P"src/kernel/dev-workbench.lisp" *q4-repo-root*)))

(format t "~&== initialize :conscious-state (no start phase) ==~%")
(let ((results (%q4-call "initialize"
                         :phases '(:configure :install :restore)
                         :stop-on-error nil :verbose nil)))
  (let ((ok (count :ok results :key #'cdr))
        (not-ok
          (count-if-not (lambda (row) (member (cdr row) '(:ok :skipped)))
                        results)))
    (format t "~&init: ~d ok, ~d not-ok~%" ok not-ok)
    (when (plusp not-ok)
      (error "Q4 demo initialization failed in ~d action(s)" not-ok))))

(setf (symbol-value (%q4-symbol "*autonomous-write-mode*")) :paused)

(format t "~&== submit closed user-message fixture ==~%")
(format t "~a~%"
        (shasht:write-json
         (%q4-call "dev-workbench-conscious-submit-fixture"
                   (%q4-call "obj" "fixture_id" "q4-user-message"))
         nil))

(format t "~&== open durable captured deliberation ==~%")
(let* ((opened
         (%q4-call "dev-workbench-conscious-open-captured"
                   (%q4-call "obj" "now" (get-universal-time))))
       (manifest (gethash "manifest" opened))
       (fixture (or (uiop:getenv "PAI_Q4_FIXTURE")
                    "q4-publication-candidate")))
  (format t "~&private request (dev-only):~%~a~%"
          (shasht:write-json (gethash "private_request" opened) nil))
  (format t "~&safe manifest:~%~a~%"
          (shasht:write-json
           (%q4-call "conscious-context-manifest-report"
                     manifest)
           nil))
  ;; This is an explicit private CLI preview of one closed fixture, not a
  ;; diagnostic/report field. Keep proposal content out of safe manifests and
  ;; general runtime status surfaces.
  (format t "~&captured response (private, untrusted, dev-only):~%~a~%"
          (shasht:write-json
           (%q4-call "%dev-workbench-q4-captured-fixture" fixture manifest)
           nil))
  (format t "~&== submit named captured fixture: ~a ==~%" fixture)
  (handler-case
      (let ((result
              (%q4-call "dev-workbench-conscious-submit-captured-fixture"
                        (%q4-call "obj" "fixture_id" fixture))))
        (format t "~a~%" (shasht:write-json result nil))
        (format t "~&No proposal was executed or delivered.~%"))
    (error (condition)
      (format t "~&CAPTURED-FIXTURE-REJECTED: ~a~%" condition))))

(format t "~&~%CONSCIOUS-Q4-DEMO-DONE~%")
(finish-output)
