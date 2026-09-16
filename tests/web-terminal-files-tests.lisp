;;;; File routes against the real authenticated HTTP acceptor and a scratch root.
;;;; harness: full-system
(in-package :agent)

(let* ((root (merge-pathnames "web-file-fixture/" (cl-user::test-state-dir)))
       (old-root *recursive-primitive-workspace-root*)
       (old-user *web-auth-username*) (old-password *web-auth-password*)
       (old-authority *web-file-mutation-authority*)
       (passed 0))
  (ensure-directories-exist root)
  (with-open-file (s (merge-pathnames "note.txt" root) :direction :output :if-exists :supersede :external-format :utf-8)
    (write-string "Hello λ" s))
  (with-open-file (s (merge-pathnames "binary.bin" root) :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
    (write-sequence #(0 1 255) s))
  (with-open-file (s (merge-pathnames "large.txt" root) :direction :output :if-exists :supersede)
    (write-string (make-string 2000001 :initial-element #\a) s))
  (unwind-protect
       (progn
         ;; HTTP request threads need global bindings, not this thread's LET.
         (setf *recursive-primitive-workspace-root* root
               *web-auth-username* "fixture-user"
               *web-auth-password* "fixture-password-for-local-tests-only"
               *web-file-mutation-authority* :disabled)
         (start-web 0 "127.0.0.1")
         (let ((base (format nil "http://127.0.0.1:~d" (hunchentoot:acceptor-port *acceptor*))))
           (labels ((request (path &key (method :get) content (auth t) (csrf t) (content-type "application/json"))
                      (handler-bind ((dexador:http-request-failed #'dexador:ignore-and-continue))
                        (dexador:request (concatenate 'string base path) :method method :content content
                          :basic-auth (when auth (cons *web-auth-username* *web-auth-password*))
                          :headers (append (when csrf '(("X-PAI-Request" . "same-origin")))
                                           (when (and content content-type) (list (cons "Content-Type" content-type)))))))
                    (check (name value)
                      (assert value () "~a" name) (incf passed) (format t "PASS ~a~%" name))
                    (status (path expected &rest options)
                      (check path (= expected (nth-value 1 (apply #'request path options))))))
             (status "/api/v2/files" 401 :auth nil)
             (status "/api/v2/file?path=note.txt" 200)
             (let ((data (shasht:read-json (request "/api/v2/file?path=note.txt"))))
               (check "UTF-8 content" (string= "Hello λ" (gethash "content" data))))
             (status "/api/v2/file?path=../outside.txt" 400)
             (status "/api/v2/download?path=../outside.txt" 404)
             (status "/api/v2/download?path=binary.bin" 200)
             (status "/api/v2/download?path=binary.bin" 401 :auth nil)
             (status "/api/v2/upload" 405)
             (status "/api/v2/file" 403 :method :post :content "{}")
             (status "/api/v2/upload" 403 :method :post)
             (setf *web-file-mutation-authority* :authenticated-web)
             (status "/api/v2/file" 403 :method :post :csrf nil :content "{}")
             (status "/api/v2/upload" 403 :method :post :csrf nil)
             (status "/api/v2/file" 200 :method :post
                     :content "{\"path\":\"note.txt\",\"content\":\"Saved\"}")
             (check "write persisted" (string= "Saved" (uiop:read-file-string (merge-pathnames "note.txt" root))))
             (dolist (name '("large.txt" "binary.bin"))
               (let ((data (shasht:read-json (request (concatenate 'string "/api/v2/file?path=" name)))))
                 (check "download-only preview" (zerop (length (gethash "content" data)))))
               (status "/api/v2/file" 400 :method :post
                       :content (%v2-json (obj "path" name "content" "partial"))))
             (let ((source (merge-pathnames "upload-source.txt" (cl-user::test-state-dir))))
               (with-open-file (s source :direction :output :if-exists :supersede) (write-string "Uploaded" s))
               (when (probe-file (merge-pathnames "upload-source.txt" root))
                 (delete-file (merge-pathnames "upload-source.txt" root)))
               (status "/api/v2/upload" 200 :method :post :content (list (cons "file" source)) :content-type nil)
               (check "upload persisted" (string= "Uploaded" (uiop:read-file-string (merge-pathnames "upload-source.txt" root))))
               (status "/api/v2/upload" 400 :method :post :content (list (cons "file" source)) :content-type nil)))))
    (stop-web)
    (setf *recursive-primitive-workspace-root* old-root *web-auth-username* old-user
          *web-auth-password* old-password *web-file-mutation-authority* old-authority))
  (format t "~d passed, 0 failed~%" passed))
