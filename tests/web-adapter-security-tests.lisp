(in-package :agent)

(defvar *web-security-pass* 0)
(defvar *web-security-fail* 0)

(defun web-security-check (name condition)
  (if condition
      (progn (incf *web-security-pass*) (format t "PASS ~a~%" name))
      (progn (incf *web-security-fail*) (format t "FAIL ~a~%" name))))

(defun web-security-signals-p (thunk)
  (handler-case (progn (funcall thunk) nil)
    (error () t)))

(ql:quickload '(:hunchentoot :cl-base64 :dexador :ironclad) :silent t)
(load (test-source "web-security.lisp"))
(load (test-source "web.lisp"))

(hunchentoot:define-easy-handler
    (web-security-probe :uri "/security-probe") ()
  (setf (hunchentoot:content-type*) "text/plain; charset=utf-8")
  "ok")

(let* ((root (merge-pathnames "web-security/root/" (test-state-dir)))
       (outside (merge-pathnames "web-security/outside/" (test-state-dir))))
  (ensure-directories-exist (merge-pathnames "nested/seed.txt" root))
  (ensure-directories-exist (merge-pathnames "seed.txt" outside))
  (with-open-file (stream (merge-pathnames "nested/seed.txt" root)
                          :direction :output :if-exists :supersede)
    (write-string "inside" stream))

  (web-security-check
   "existing path below root resolves"
   (probe-file (web-resolve-contained-path root "nested/seed.txt")))
  (web-security-check
   "nonexistent write target below an existing parent resolves"
   (string-equal
    (namestring (merge-pathnames "nested/new.txt" (truename root)))
    (namestring (web-resolve-contained-path root "nested/new.txt"))))
  (web-security-check
   "nonexistent traversal target is rejected before merge"
   (web-security-signals-p
    (lambda ()
      (web-resolve-contained-path root "../../outside/new.txt"))))
  (web-security-check
   "absolute path outside root is rejected"
   (web-security-signals-p
    (lambda ()
      (web-resolve-contained-path root (namestring outside)))))
  (web-security-check
   "absolute path inside root remains supported"
   (not
    (web-security-signals-p
     (lambda ()
       (probe-file
        (web-resolve-contained-path
         root
         (namestring (merge-pathnames "nested/seed.txt" (truename root))))))))))

(web-security-check "file mutation is disabled by default"
                    (not (web-file-mutation-authorized-p)))
(let ((*web-file-mutation-authority* :operator-local-development))
  (web-security-check "local operator can explicitly enable mutation"
                      (web-file-mutation-authorized-p)))
(web-security-check "default web listen address is loopback"
                    (web-loopback-address-p *web-listen-address*))
(web-security-check "wildcard listen address is not loopback"
                    (not (web-loopback-address-p "0.0.0.0")))
(web-security-check
 "remote listen is refused without authentication"
 (not (web-listen-address-authorized-p "0.0.0.0")))

(let ((*web-auth-username* "operator")
      (*web-auth-password* "a-long-test-password-with-32-chars"))
  (web-security-check "complete web credentials configure authentication"
                      (web-authentication-configured-p))
  (let ((token (%web-session-token)))
    (web-security-check "session token is stable and does not expose the password"
                        (and (stringp token)
                             (= 64 (length token))
                             (string= token (%web-session-token))
                             (not (search *web-auth-password* token))))
    (web-security-check "exact session cookie is accepted"
                        (web-request-session-authorized-p token))
    (web-security-check "incorrect session cookie is rejected"
                        (not (web-request-session-authorized-p
                              (make-string 64 :initial-element #\0)))))
  (let ((authorization (web-basic-authorization-value
                        *web-auth-username* *web-auth-password*)))
    (web-security-check "exact Basic credential is accepted"
                        (web-request-authorized-p authorization))
    (web-security-check "incorrect Basic credential is rejected"
                        (not (web-request-authorized-p
                              (web-basic-authorization-value
                               *web-auth-username* "incorrect-password")))))
  (web-security-check "authentication permits an explicit remote listen"
                      (web-listen-address-authorized-p "0.0.0.0"))
  (let ((*web-file-mutation-authority* :authenticated-web)
        (*web-request-authenticated-p* t))
    (web-security-check "authenticated request can receive explicit mutation authority"
                        (web-file-mutation-authorized-p)))
  (let ((*web-file-mutation-authority* :authenticated-web)
        (*web-request-authenticated-p* nil))
    (web-security-check "mutation authority does not bypass request authentication"
                        (not (web-file-mutation-authorized-p)))))

(web-security-check "same-origin marker admits a mutating request"
                    (web-request-csrf-authorized-p :post "same-origin"))
(web-security-check "missing same-origin marker rejects a mutating request"
                    (not (web-request-csrf-authorized-p :post nil)))
(web-security-check "read request needs no CSRF marker"
                    (web-request-csrf-authorized-p :get nil))
(web-security-check "login shell is public but contains no private data"
                    (%web-pwa-public-path-p "/login"))
(web-security-check "login submission is a public authentication boundary"
                    (%web-pwa-public-path-p "/api/v2/login"))
(web-security-check "agent APIs remain outside the public shell"
                    (not (%web-pwa-public-path-p "/api/v2/history")))
(web-security-check "terminal navigation is redirected when unauthenticated"
                    (%web-terminal-navigation-path-p "/terminal"))

(let ((previous-username *web-auth-username*)
      (previous-password *web-auth-password*))
  ;; Hunchentoot request workers are new threads and correctly observe the
  ;; process credential, not this test thread's dynamic bindings.
  (setf *web-auth-username* "operator"
        *web-auth-password* "a-long-test-password-with-32-chars")
  (unwind-protect
      (let ((acceptor (start-web 0 "127.0.0.1")))
        (unwind-protect
            (let* ((url (format nil "http://127.0.0.1:~d/security-probe"
                                (hunchentoot:acceptor-port acceptor)))
                   (authorization
                     (web-basic-authorization-value
                      *web-auth-username* *web-auth-password*)))
              (multiple-value-bind (body status)
                  (handler-bind
                      ((dex:http-request-failed #'dex:ignore-and-continue))
                    (dex:get url))
                (declare (ignore body))
                (web-security-check
                 "live acceptor challenges an anonymous request"
                 (= status 401)))
              (multiple-value-bind (body status headers)
                  (handler-bind
                      ((dex:http-request-failed #'dex:ignore-and-continue))
                    (dex:get
                     (format nil "http://127.0.0.1:~d/terminal"
                             (hunchentoot:acceptor-port acceptor))
                     :max-redirects 0))
                (declare (ignore body))
                (web-security-check
                 "live terminal navigation redirects to the PWA login"
                 (and (= status 303)
                      (string= "/login" (gethash "location" headers)))))
              (multiple-value-bind (body status)
                  (dex:get url :headers `(("Authorization" . ,authorization)))
                (web-security-check
                 "live acceptor serves the exact Basic credential"
                 (and (= status 200) (stringp body))))
              (multiple-value-bind (body status)
                  (handler-bind
                      ((dex:http-request-failed #'dex:ignore-and-continue))
                    (dex:post url
                              :headers
                              `(("Authorization" . ,authorization))))
                (declare (ignore body))
                (web-security-check
                 "live acceptor rejects mutation without CSRF marker"
                 (= status 403))))
          (stop-web)))
    (setf *web-auth-username* previous-username
          *web-auth-password* previous-password)))

(format t "~%WEB ADAPTER SECURITY: ~d passed, ~d failed.~%"
        *web-security-pass* *web-security-fail*)
(when (plusp *web-security-fail*) (uiop:quit 1))
