;;;; web-security.lisp -- closed defaults for the inherited HTTP adapter.

(in-package :agent)

(export '(web-resolve-contained-path
          web-authentication-configured-p
          web-request-session-authorized-p
          web-file-mutation-authorized-p
          web-loopback-address-p))

(defparameter *web-listen-address* "127.0.0.1"
  "The web adapter remains loopback by default; remote binding is explicit.")

(defun %web-secret-file-value (environment-name)
  (let ((path (uiop:getenv environment-name)))
    (when (and (stringp path) (plusp (length path)) (probe-file path))
      (string-trim '(#\Space #\Tab #\Newline #\Return)
                   (uiop:read-file-string path)))))

(defparameter *web-auth-username*
  (or (uiop:getenv "PAI_WEB_USERNAME") "pai"))

(defparameter *web-auth-password*
  (or (uiop:getenv "PAI_WEB_PASSWORD")
      (%web-secret-file-value "PAI_WEB_PASSWORD_FILE")))

(defparameter *web-minimum-password-characters* 24)
(defparameter *web-request-authenticated-p* nil)
(defparameter *web-session-cookie-name*
  ;; __Host- forbids a Domain attribute (by design: no subdomain scope
  ;; widening), but browsers never scope cookies by port at all -- two
  ;; instances served from the same hostname on different ports (e.g. two
  ;; agents behind Tailscale Serve on one tailnet node) share ONE cookie
  ;; jar entry for this name regardless. Confirmed live: logging into one
  ;; agent logged the operator out of the other, because each login
  ;; overwrote the single shared cookie value. Suffixing by PAI_WEB_PORT --
  ;; already required, already unique per instance -- gives each instance
  ;; its own cookie name instead, with no change to __Host-'s other
  ;; guarantees (still HTTPS-only, still path "/", still no Domain).
  (format nil "__Host-pai_web_session_~a"
          (or (uiop:getenv "PAI_WEB_PORT") "default")))
(defparameter *web-session-cookie-max-age-seconds* (* 30 24 60 60))

(defparameter *web-file-mutation-authority*
  (if (string-equal (or (uiop:getenv "PAI_WEB_FILE_MUTATION") "")
                    "authenticated")
      :authenticated-web
      :disabled)
  "File mutation stays unavailable unless a local operator deliberately binds
this to :OPERATOR-LOCAL-DEVELOPMENT for one controlled dynamic extent, or the
authenticated web capability is explicitly enabled at process start.")

(defun %web-printable-ascii-p (value)
  (and (stringp value)
       (every (lambda (character)
                (<= 32 (char-code character) 126))
              value)))

(defun web-authentication-configured-p ()
  (and (%web-printable-ascii-p *web-auth-username*)
       (plusp (length *web-auth-username*))
       (not (find #\: *web-auth-username*))
       (%web-printable-ascii-p *web-auth-password*)
       (>= (length *web-auth-password*)
           *web-minimum-password-characters*)))

(defun %web-constant-time-string-equal-p (left right)
  (when (and (stringp left) (stringp right))
    (let ((difference (logxor (length left) (length right))))
      (loop for index below (min (length left) (length right))
            do (setf difference
                     (logior difference
                             (logxor (char-code (char left index))
                                     (char-code (char right index))))))
      (zerop difference))))

(defun web-basic-authorization-value (username password)
  (format nil "Basic ~a"
          (cl-base64:string-to-base64-string
           (format nil "~a:~a" username password))))

(defun web-request-authorized-p (authorization)
  (and (web-authentication-configured-p)
       (%web-constant-time-string-equal-p
        authorization
        (web-basic-authorization-value *web-auth-username*
                                       *web-auth-password*))))

(defun web-request-credentials-authorized-p (username password)
  (and (web-authentication-configured-p)
       (%web-constant-time-string-equal-p username *web-auth-username*)
       (%web-constant-time-string-equal-p password *web-auth-password*)))

(defun %web-session-token ()
  "Derive a domain-separated bearer token from the configured credential.

The token survives process restarts, rotates with the password and never
requires the browser to retain the Basic credential. Credentials are already
restricted to printable ASCII by WEB-AUTHENTICATION-CONFIGURED-P."
  (when (web-authentication-configured-p)
    (let* ((text (format nil "pai-web-session-v1~c~a~c~a"
                         #\Null *web-auth-username* #\Null *web-auth-password*))
           (octets (make-array (length text)
                               :element-type '(unsigned-byte 8))))
      (loop for character across text
            for index from 0
            do (setf (aref octets index) (char-code character)))
      (string-downcase
       (ironclad:byte-array-to-hex-string
        (ironclad:digest-sequence :sha256 octets))))))

(defun web-request-session-authorized-p (cookie-value)
  (and (stringp cookie-value)
       (%web-constant-time-string-equal-p cookie-value
                                          (%web-session-token))))

(defun web-request-csrf-authorized-p (method marker)
  (or (member method '(:get :head :options))
      (and (stringp marker) (string= marker "same-origin"))))

(defun web-file-mutation-authorized-p ()
  (or (eq *web-file-mutation-authority* :operator-local-development)
      (and (eq *web-file-mutation-authority* :authenticated-web)
           *web-request-authenticated-p*)))

(defun web-loopback-address-p (address)
  (member (string-downcase (or address ""))
          '("127.0.0.1" "localhost" "::1")
          :test #'string=))

(defun web-listen-address-authorized-p (address)
  (or (web-loopback-address-p address)
      (web-authentication-configured-p)))

(defun %web-path-string (value)
  (etypecase value
    (string value)
    (pathname (namestring value))))

(defun %web-normalize-separators (value)
  (substitute #\/ #\\ (%web-path-string value)))

(defun %web-directory-prefix-p (prefix directory)
  (and (<= (length prefix) (length directory))
       (loop for expected in prefix
             for actual in directory
             always (equalp expected actual))))

(defun %web-contained-canonical-path-p (root path)
  (and (equalp (pathname-host root) (pathname-host path))
       (equalp (pathname-device root) (pathname-device path))
       (%web-directory-prefix-p (pathname-directory root)
                                (pathname-directory path))))

(defun %web-relative-request-path (root raw)
  (let* ((root-string (%web-normalize-separators root))
         (root-prefix (if (and (plusp (length root-string))
                               (char= (char root-string
                                            (1- (length root-string)))
                                      #\/))
                          root-string
                          (concatenate 'string root-string "/")))
         (request (%web-normalize-separators (or raw "")))
         (relative
           (cond
             ((string-equal request root-string) "")
             ((and (>= (length request) (length root-prefix))
                   (string-equal root-prefix request
                                 :end2 (length root-prefix)))
              (subseq request (length root-prefix)))
             ;; Anything that still has an absolute spelling is outside the
             ;; declared root. Do not reinterpret it as a relative path.
             ((or (and (plusp (length request))
                       (char= (char request 0) #\/))
                  (position #\: request))
              (error "absolute web path is outside the configured root"))
             (t request))))
    (dolist (component (uiop:split-string relative :separator '(#\/)))
      (when (or (string= component "..")
                (string= component "."))
        (error "relative web path contains a traversal component")))
    relative))

(defun web-resolve-contained-path (root requested-path)
  "Resolve REQUESTED-PATH beneath ROOT for reads or a new file write.

Unlike a TRUENAME-or-candidate fallback, this verifies the canonical target
when it exists and the canonical parent when it does not. A symlinked parent
therefore cannot redirect a new file outside ROOT, and lexical parent
components are rejected before MERGE-PATHNAMES can normalize them away."
  (let* ((canonical-root
           (uiop:ensure-directory-pathname (truename root)))
         (relative (%web-relative-request-path canonical-root requested-path))
         (candidate (merge-pathnames relative canonical-root))
         (existing (probe-file candidate)))
    (if existing
        (let ((canonical-target (truename existing)))
          (unless (%web-contained-canonical-path-p canonical-root
                                                    canonical-target)
            (error "web path resolves outside the configured root"))
          canonical-target)
        (let* ((parent (uiop:pathname-directory-pathname candidate))
               (canonical-parent (truename parent)))
          (unless (%web-contained-canonical-path-p canonical-root
                                                    canonical-parent)
            (error "web path parent resolves outside the configured root"))
          candidate))))
