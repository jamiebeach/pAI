(in-package :agent)

;; The original entrypoint had already loaded every source file, so this suite
;; never loaded its own subject. Nothing preloads it here.
(load (test-source "public-read-only-replay.lisp"))

(defvar *public-read-only-replay-test-pass* 0)
(defvar *public-read-only-replay-test-fail* 0)

(defun public-read-only-replay-test-check (name condition)
  (if condition
      (progn (incf *public-read-only-replay-test-pass*)
             (format t "~&PASS ~a~%" name))
      (progn (incf *public-read-only-replay-test-fail*)
             (format t "~&FAIL ~a~%" name))))

(defun replay-test-tool (name description)
  (obj "type" "function" "function"
       (obj "name" name "description" description
            "parameters" (obj "type" "object"))))

(defun replay-test-call (id name arguments)
  (obj "id" id "type" "function"
       "function" (obj "name" name
                       "arguments" (shasht:write-json arguments nil))))

(defun replay-test-response (&key content tool-call finish-reason)
  (obj "choices"
       (vector
        (obj "message"
             (obj "role" "assistant" "content" (or content :null)
                  "tool_calls" (if tool-call (vector tool-call) :null))
             "finish_reason" (or finish-reason
                                  (if tool-call "tool_calls" "stop"))))))

(defun replay-test-result (id content)
  (obj "role" "tool" "tool_call_id" id "content" content))

(let* ((*tools*
         (vector
          (replay-test-tool "write-deliverable" "write")
          (replay-test-tool "read-deliverable" "read")
          (replay-test-tool "search-memory" "search")))
       (system
         (obj "role" "system"
              "content" "Available: search-memory and read-deliverable."))
       (request
         (obj "model" "captured" "messages"
              (vector system (obj "role" "user" "content" "agenda?"))
              "tools" (vector (replay-test-tool "search-memory" "stale"))))
       (search-call
         (replay-test-call "search-1" "search-memory"
                           (obj "query" "current agenda" "limit" 3)))
       (search-content
         (shasht:write-json
          (obj "status" "available" "database_write_count" 0
               "results" (vector (obj "id" "bundle-1"))) nil))
       (search-step
         (public-read-only-replay-step
          request (replay-test-response :tool-call search-call)
          :tool-result (replay-test-result "search-1" search-content)
          :tools *tools*)))
  (public-read-only-replay-test-check
   "aligned continuation succeeds"
   (string= "continuation" (gethash "status" search-step)))
  (let* ((next (gethash "next_request" search-step))
         (names (map 'list #'%public-read-only-replay-tool-name
                     (gethash "tools" next)))
         (messages (%public-read-only-replay-list (gethash "messages" next)))
         (assistant (nth (- (length messages) 2) messages))
         (result (car (last messages))))
    (public-read-only-replay-test-check
     "request exposes exactly the two source-derived read-only tools"
     (equal names '("search-memory" "read-deliverable")))
    (public-read-only-replay-test-check
     "assistant and result identities remain exact"
     (and (string= "search-1"
                   (gethash "id" (aref (gethash "tool_calls" assistant) 0)))
          (string= "search-1" (gethash "tool_call_id" result))))
    (public-read-only-replay-test-check
     "continuation appends one matched assistant/result pair"
     (= 4 (length messages))))

  (let* ((read-call
           (replay-test-call "read-1" "read-deliverable"
                             (obj "path" "contacts/tracker.md")))
         (read-content
           (shasht:write-json
            (obj "path" "/agent/state/deliverables/contacts/tracker.md"
                 "original_characters" 12 "truncated" nil
                 "content" "bounded text") nil))
         (step
           (public-read-only-replay-step
            request (replay-test-response :tool-call read-call)
            :tool-result (replay-test-result "read-1" read-content)
            :tools *tools*)))
    (public-read-only-replay-test-check
     "bounded deliverable read succeeds"
     (and (string= "continuation" (gethash "status" step))
          (string= "read-deliverable" (gethash "tool_name" step)))))

  (let ((step (public-read-only-replay-step
               request (replay-test-response :content "A grounded answer.")
               :tools *tools*)))
    (public-read-only-replay-test-check
     "plain final response succeeds"
     (and (string= "final" (gethash "status" step))
          (string= "A grounded answer." (gethash "content" step)))))

  (public-read-only-replay-test-check
   "length-truncated final response is rejected"
   (string=
    "rejected"
    (gethash "status"
             (public-read-only-replay-step
              request
              (replay-test-response :content "A cut-off answer"
                                    :finish-reason "length")
              :tools *tools*))))

  (public-read-only-replay-test-check
   "raw pseudo-tool final text is rejected"
   (string=
    "rejected"
    (gethash "status"
             (public-read-only-replay-step
              request
              (replay-test-response :content
                                    "<tool_call>read-deliverable</tool_call>")
              :tools *tools*))))

  (let ((write-call
          (replay-test-call "write-1" "write-deliverable"
                            (obj "path" "x.md" "content" "no"))))
    (public-read-only-replay-test-check
     "write-capable tool is rejected"
     (string=
      "rejected"
      (gethash "status"
               (public-read-only-replay-step
                request (replay-test-response :tool-call write-call)
                :tool-result (replay-test-result "write-1" "{}")
                :tools *tools*)))))

  (public-read-only-replay-test-check
   "mismatched tool result identity is rejected"
   (string=
    "rejected"
    (gethash "status"
             (public-read-only-replay-step
              request (replay-test-response :tool-call search-call)
              :tool-result (replay-test-result "other" search-content)
              :tools *tools*))))

  (let* ((read-call
           (replay-test-call "read-path" "read-deliverable"
                             (obj "path" "contacts/tracker.md")))
         (wrong-path
           (shasht:write-json
            (obj "path" "/agent/state/deliverables/other.md"
                 "original_characters" 4 "truncated" nil "content" "nope")
            nil)))
    (public-read-only-replay-test-check
     "mismatched deliverable result path is rejected"
     (string=
      "rejected"
      (gethash "status"
               (public-read-only-replay-step
                request (replay-test-response :tool-call read-call)
                :tool-result (replay-test-result "read-path" wrong-path)
                :tools *tools*)))))

  (let ((duplicate-system-request
          (obj "model" "captured" "messages"
               (vector system (%public-read-only-replay-json-copy system)
                       (obj "role" "user" "content" "agenda?")))))
    (public-read-only-replay-test-check
     "multiple system messages fail closed"
     (string=
      "rejected"
      (gethash "status"
               (public-read-only-replay-step
                duplicate-system-request
                (replay-test-response :tool-call search-call)
                :tool-result (replay-test-result "search-1" search-content)
                :tools *tools*)))))

  (public-read-only-replay-test-check
   "malformed search result fails closed"
   (string=
    "rejected"
    (gethash "status"
             (public-read-only-replay-step
              request (replay-test-response :tool-call search-call)
              :tool-result
              (replay-test-result
               "search-1"
               (shasht:write-json
                (obj "status" "available" "database_write_count" 1
                     "results" (vector)) nil))
              :tools *tools*))))

  (let ((report (public-read-only-replay-report)))
    (public-read-only-replay-test-check
     "report exposes no provider, write or delivery authority"
     (and (zerop (gethash "database_write_count" report))
          (not (gethash "provider_calls_available" report))
          (not (gethash "delivery_authority" report))))))

(format t "~&public read-only replay tests: ~d passed, ~d failed~%"
        *public-read-only-replay-test-pass*
        *public-read-only-replay-test-fail*)
(when (plusp *public-read-only-replay-test-fail*) (sb-ext:exit :code 1))
