;;;; web.lisp -- authenticated HTTP acceptor for the pAI web terminal.
;;;;
;;;; This file owns only transport security and acceptor lifecycle. Terminal
;;;; routes, presentation and selected-mind submission live in
;;;; web-terminal.lisp. The pre-pAI chat page and interaction loop are gone.

(ql:quickload '(:hunchentoot) :silent t)

(in-package :agent)

(export '(start-web stop-web))

;;; --- HTTP layer ----------------------------------------------------------

(defvar *acceptor* nil)

(defclass authenticated-web-acceptor (hunchentoot:easy-acceptor)
  ((authentication-required-p
    :initarg :authentication-required-p
    :reader web-acceptor-authentication-required-p)))

(defun %web-separately-authenticated-path-p (path)
  ;; The admin API already owns a distinct Bearer credential. Requiring Basic
  ;; first would make it impossible to send that one Authorization header.
  (and (stringp path)
       (or (string= path "/api/admin")
           (and (>= (length path) 11)
                (string= path "/api/admin/" :end1 11 :end2 11)))))

(defun %web-pwa-public-path-p (path)
  "Public shell surfaces contain no agent or operator data."
  (member path '("/login" "/api/v2/login" "/manifest.webmanifest"
                 "/service-worker.js" "/pwa-icon.svg" "/viewport.js")
          :test #'string=))

(defun %web-terminal-navigation-path-p (path)
  (member path '("/" "/terminal" "/graph" "/dashboard" "/settings") :test #'string=))

(defun %web-authentication-failure ()
  (setf (hunchentoot:return-code*) 401)
  (setf (hunchentoot:content-type*) "text/plain; charset=utf-8")
  (setf (hunchentoot:header-out "Cache-Control") "no-store")
  (setf (hunchentoot:header-out "WWW-Authenticate")
        "Basic realm=\"pAI web terminal\", charset=\"UTF-8\"")
  "Authentication required.")

(defun %web-login-redirect ()
  (setf (hunchentoot:return-code*) 303)
  (setf (hunchentoot:header-out "Location") "/login")
  (setf (hunchentoot:header-out "Cache-Control") "no-store")
  "")

(defun %web-csrf-failure ()
  (setf (hunchentoot:return-code*) 403)
  (setf (hunchentoot:content-type*) "text/plain; charset=utf-8")
  (setf (hunchentoot:header-out "Cache-Control") "no-store")
  "Same-origin request marker required.")

(defmethod hunchentoot:acceptor-dispatch-request
    ((acceptor authenticated-web-acceptor) request)
  (let* ((path (hunchentoot:script-name request))
         (separate-auth (%web-separately-authenticated-path-p path))
         (pwa-public (%web-pwa-public-path-p path))
         (authentication-required
           (web-acceptor-authentication-required-p acceptor))
         (authenticated
           (and authentication-required
                (or (multiple-value-bind (username password)
                        (hunchentoot:authorization request)
                      (web-request-credentials-authorized-p username password))
                    (web-request-session-authorized-p
                     (hunchentoot:cookie-in *web-session-cookie-name*
                                            request))))))
    (cond
      (separate-auth
       (call-next-method))
      ((and authentication-required (not authenticated) (not pwa-public)
            (%web-terminal-navigation-path-p path))
       (%web-login-redirect))
      ((and authentication-required (not authenticated) (not pwa-public))
       (%web-authentication-failure))
      ((not (web-request-csrf-authorized-p
             (hunchentoot:request-method request)
             (hunchentoot:header-in* "X-PAI-Request" request)))
       (%web-csrf-failure))
      (t
       (let ((*web-request-authenticated-p* authenticated))
         (setf (hunchentoot:header-out "Cache-Control") "no-store")
         (setf (hunchentoot:header-out "X-Content-Type-Options") "nosniff")
         (setf (hunchentoot:header-out "Referrer-Policy") "no-referrer")
         (setf (hunchentoot:header-out "X-Frame-Options") "DENY")
         (call-next-method))))))

(defun start-web (&optional
                    (port (or (ignore-errors (parse-integer (uiop:getenv "PORT")))
                              8080))
                    (address *web-listen-address*))
  (unless (web-listen-address-authorized-p address)
    (error "non-loopback web listening requires PAI_WEB_PASSWORD or PAI_WEB_PASSWORD_FILE (minimum ~d characters)"
           *web-minimum-password-characters*))
  (when *acceptor* (stop-web))
  (setf *acceptor*
        (make-instance 'authenticated-web-acceptor
                       :port port :address address
                       :authentication-required-p
                       (web-authentication-configured-p)))
  (hunchentoot:start *acceptor*)
  (format t "~&pAI web terminal running at http://~a:~a/ (~a).~%"
          address (hunchentoot:acceptor-port *acceptor*)
          (if (web-authentication-configured-p)
              "HTTP Basic authentication required"
              "loopback-only without authentication"))
  *acceptor*)

(defun stop-web ()
  (when *acceptor*
    (hunchentoot:stop *acceptor*)
    (setf *acceptor* nil)
    (format t "~&Stopped.~%")))
