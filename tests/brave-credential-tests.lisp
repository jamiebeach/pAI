(require :asdf)

(defpackage :agent
  (:use :cl))

(in-package :agent)

(defvar *passed* 0)
(defvar *failed* 0)

(defun check (name condition)
  (if condition
      (progn (incf *passed*) (format t "PASS ~a~%" name))
      (progn (incf *failed*) (format t "FAIL ~a~%" name))))

(load (test-source "brave-credential.lisp"))

(defpackage :dexador
  (:use :cl)
  (:shadow #:get)
  (:export #:get))

(defpackage :shasht
  (:use :cl)
  (:export #:read-json))

(defvar *captured-brave-headers* nil)

(defun dexador:get (url &key headers)
  (declare (ignore url))
  (setf agent::*captured-brave-headers* headers)
  (values "fixture-body" 200))

(defun shasht:read-json (body)
  (declare (ignore body))
  (let ((root (make-hash-table :test #'equal))
        (web (make-hash-table :test #'equal))
        (result (make-hash-table :test #'equal)))
    (setf (gethash "title" result) "Fixture title"
          (gethash "url" result) "https://example.test/"
          (gethash "description" result) "Fixture description"
          (gethash "results" web) (vector result)
          (gethash "web" root) web)
    root))

(let ((fixture #P"/tmp/pai-brave-credential-fixture.txt"))
  (unwind-protect
       (progn
         (with-open-file (out fixture :direction :output :if-exists :supersede)
           (write-string "  fixture-secret  " out))
         (check "environment credential wins"
                (string= "environment-secret"
                         (load-brave-api-key
                          :environment-value "environment-secret"
                          :file fixture)))
         (check "private file is used when environment is empty"
                (string= "fixture-secret"
                         (load-brave-api-key :environment-value " " :file fixture)))
         (check "missing sources return nil"
                (null (load-brave-api-key
                       :environment-value nil
                       :file #P"/tmp/pai-no-such-brave-key")))
         (let ((*brave-api-key* nil)
               (*brave-api-key-file* fixture))
           (check "status exposes presence but not credential"
                  (string= "available" (brave-credential-status)))))
    (when (probe-file fixture) (delete-file fixture))))

;; Reproduce the ASDF component boundary: every source file must select its
;; own package rather than inheriting the package of a test or prior LOAD.
(let ((*package* (find-package :cl-user)))
  (load (namestring (test-source "definitions.lisp"))))
(check "Brave adapter is defined in AGENT across an independent load boundary"
       (and (fboundp 'agent::brave-search)
            (not (fboundp 'common-lisp-user::brave-search))))
(let* ((*brave-api-key* "dispatch-fixture-secret")
       (result (brave-search "fixture query" :count 1))
       (token (cdr (assoc "X-Subscription-Token"
                          *captured-brave-headers* :test #'string=))))
  (check "stubbed Brave dispatch reads the shared credential"
         (string= "dispatch-fixture-secret" token))
  (check "stubbed Brave dispatch renders a usable result"
         (and (search "Fixture title" result)
              (search "https://example.test/" result))))

(format t "~%Brave credential tests: ~d passed, ~d failed.~%" *passed* *failed*)
(sb-ext:exit :code (if (plusp *failed*) 1 0))
