;;; agent_helpers.lisp -- the agent's persistence/audit helpers (regenerated clean).

(DEFUN READ-FILE-STRING (PATH)
  "Read PATH's full contents as a string. FILE-LENGTH counts bytes, but
MAKE-STRING allocates that many characters -- for any file with multi-byte
UTF-8 (em dashes, curly quotes, all over these files), that over-allocates,
and SBCL pads the unfilled tail with NUL characters. If that padded string
later gets written back to disk, the NULs get baked in and break the next
read. READ-SEQUENCE returns the count actually filled, so SUBSEQ to that
count instead of trusting FILE-LENGTH."
  (BLOCK READ-FILE-STRING
    (WITH-OPEN-FILE (IN PATH)
      (LET ((BUF (MAKE-STRING (FILE-LENGTH IN))))
        (SUBSEQ BUF 0 (READ-SEQUENCE BUF IN))))))

(DEFUN URL-ENCODE (S)
  (BLOCK URL-ENCODE
    (WITH-OUTPUT-TO-STRING (OUT)
      (LOOP FOR C ACROSS (STRING S)
            DO (COND ((ALPHANUMERICP C) (WRITE-CHAR C OUT)) ((FIND C "-_.~") (WRITE-CHAR C OUT))
                     (T (FORMAT OUT "%~2,'0X" (CHAR-CODE C))))))))

(DEFUN PAI-DUMP-DEFUN (NAME &OPTIONAL (STREAM *STANDARD-OUTPUT*))
  (BLOCK PAI-DUMP-DEFUN
    (LET ((FLE (FUNCTION-LAMBDA-EXPRESSION (SYMBOL-FUNCTION NAME))))
      (IF FLE
          (LET ((*PRINT-PRETTY* T) (*PRINT-RIGHT-MARGIN* 110) (*PRINT-CIRCLE* NIL))
            (PPRINT (LIST* 'DEFUN NAME (CADR FLE) (CDDR FLE)) STREAM)
            T)
          (FORMAT STREAM ";; No lambda-expression available for ~a~%" NAME)))))

(DEFUN PAI-SNAPSHOT-LOOP ()
  (BLOCK PAI-SNAPSHOT-LOOP
    (HANDLER-CASE
     (LET* ((ROOT (OR (SB-EXT:POSIX-GETENV "PAI_STAGED_PROPOSAL_ROOT")
                      (SB-EXT:POSIX-GETENV "PAI_R3A_STAGED_PROPOSAL_ROOT") ""))
            (PATH (IF (> (LENGTH ROOT) 0)
                      (MERGE-PATHNAMES "agent_loop.lisp" (PATHNAME ROOT))
                      "agent_loop.lisp"))
            (NEW (WITH-OUTPUT-TO-STRING (S) (PAI-DUMP-DEFUN 'AGENT-LOOP S)))
            (OLD (WHEN (PROBE-FILE PATH) (READ-FILE-STRING PATH))))
       (IF (AND OLD (STRING= OLD NEW))
           :UNCHANGED
           (PROGN
            (WITH-OPEN-FILE (OUT PATH :DIRECTION :OUTPUT :IF-EXISTS :SUPERSEDE :IF-DOES-NOT-EXIST :CREATE)
              (WRITE-STRING NEW OUT))
            (PAI-LOG-CHANGE "agent-loop" "loop auto-snapshotted to disk (source changed)")
            :WRITTEN)))
     (ERROR (E) (FORMAT T "SNAPSHOT-ERR: ~a~%" E) "SNAPSHOT-ERR"))))

(DEFUN PAI-RECORD-SKILL
    (NAME DEFINITION
     &KEY (FILE (LET ((ROOT (OR (SB-EXT:POSIX-GETENV "PAI_STAGED_PROPOSAL_ROOT")
                                (SB-EXT:POSIX-GETENV
                                 "PAI_R3A_STAGED_PROPOSAL_ROOT") "")))
                  (IF (> (LENGTH ROOT) 0)
                      (MERGE-PATHNAMES "skills/definitions.lisp" (PATHNAME ROOT))
                      "skills/definitions.lisp"))))
  (BLOCK PAI-RECORD-SKILL
    (ENSURE-DIRECTORIES-EXIST FILE)
    (LET ((ENTRY (FORMAT NIL "~%~%~a~%" DEFINITION)))
      (IF (AND (PROBE-FILE FILE) (SEARCH NAME (READ-FILE-STRING FILE)))
          :ALREADY-RECORDED
          (WITH-OPEN-FILE (OUT FILE :DIRECTION :OUTPUT :IF-EXISTS :APPEND :IF-DOES-NOT-EXIST :CREATE)
            (WRITE-STRING ENTRY OUT))))
    (PAI-LOG-CHANGE "skill" (FORMAT NIL "recorded skill ~a" NAME))
    :RECORDED))

(DEFUN PAI-LOG-CHANGE (KIND SUMMARY &OPTIONAL DETAIL)
  (BLOCK PAI-LOG-CHANGE
    (LET* ((ROOT (OR (SB-EXT:POSIX-GETENV "PAI_STAGED_PROPOSAL_ROOT")
                     (SB-EXT:POSIX-GETENV "PAI_R3A_STAGED_PROPOSAL_ROOT") ""))
           (FILE (IF (> (LENGTH ROOT) 0)
                     (MERGE-PATHNAMES "changelog.md" (PATHNAME ROOT))
                     "changelog.md"))
          (STAMP
           (MULTIPLE-VALUE-BIND (S M H D MO Y)
               (DECODE-UNIVERSAL-TIME (GET-UNIVERSAL-TIME))
             (FORMAT NIL "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" Y MO D H M))))
      (ENSURE-DIRECTORIES-EXIST FILE)
      (WITH-OPEN-FILE (OUT FILE :DIRECTION :OUTPUT :IF-EXISTS :APPEND :IF-DOES-NOT-EXIST :CREATE)
        (FORMAT OUT "~%~%### ~a -- [~a] ~a~%" STAMP (STRING-UPCASE KIND) SUMMARY)
        (WHEN DETAIL (FORMAT OUT "~%~a~%" DETAIL)))
      :LOGGED)))

(DEFUN PAI-VERIFY-LOOP (SOURCE)
  (BLOCK PAI-VERIFY-LOOP
    (HANDLER-CASE
     (LET* ((FORM (WITH-INPUT-FROM-STRING (S SOURCE) (READ S)))
            (OK
             (AND (CONSP FORM) (EQ (CAR FORM) 'DEFUN) (EQ (CADR FORM) 'AGENT-LOOP)
                  (EQUAL (CADDR FORM) '(MESSAGES)))))
       (WHEN (AND OK (SEARCH "CALL-MODEL" (STRING-UPCASE SOURCE)))
         (IF (SEARCH "(DEFUN CALL-MODEL" (STRING-UPCASE SOURCE))
             (RETURN-FROM PAI-VERIFY-LOOP (VALUES NIL "must not redefine call-model"))
             (SETF OK T)))
       (VALUES OK
               (IF OK
                   NIL
                   "structure/contract checks failed")))
     (ERROR (E) (VALUES NIL (FORMAT NIL "read/parse error: ~a" E))))))

(DEFUN WRITE-FILE-STRING (PATH TEXT)
  (BLOCK WRITE-FILE-STRING
    (HANDLER-CASE
     (PROGN
      (ENSURE-DIRECTORIES-EXIST PATH)
      (WITH-OPEN-FILE
          (S PATH :DIRECTION :OUTPUT :IF-EXISTS :SUPERSEDE :IF-DOES-NOT-EXIST :CREATE :EXTERNAL-FORMAT
           :UTF-8)
        (WRITE-STRING TEXT S))
      :WRITTEN)
     (ERROR (E) (FORMAT NIL "WRITE-ERR: ~a" E)))))

(DEFUN RUN-COMMAND (CMD &KEY (TIMEOUT 30))
  (BLOCK RUN-COMMAND
    (HANDLER-CASE
     (MULTIPLE-VALUE-BIND (OUT ERR EXIT)
         (UIOP/RUN-PROGRAM:RUN-PROGRAM CMD :OUTPUT :STRING :ERROR :STRING :IGNORE-ERROR-STATUS T :TIMEOUT
                                       TIMEOUT)
       (LIST :EXIT EXIT :OUT OUT :ERR ERR))
     (ERROR (E) (LIST :ERROR (FORMAT NIL "~a" E))))))

(DEFUN FETCH-URL (URL &KEY (STRIP-TAGS T))
  (BLOCK FETCH-URL
    (HANDLER-CASE
     (LET ((BODY (DEXADOR:GET URL :WANT-STREAM NIL)))
       (IF STRIP-TAGS
           (LET ((CLEAN
                  (CL-PPCRE:REGEX-REPLACE-ALL "(?is)<(?:script|style)[^>]*>.*?</(?:script|style)>" BODY " ")))
             (CL-PPCRE:REGEX-REPLACE-ALL "[ trn]+" (CL-PPCRE:REGEX-REPLACE-ALL "<[^>]*>" CLEAN " ") " "))
           BODY))
     (ERROR (E) (FORMAT NIL "FETCH-ERR: ~a" E)))))

;;; --- relationship journal + initiative hook (recreated from verified session) ---

(defun pai-journal-append (user-text my-text &key (mood :neutral) (thread :general))
  "Append one relationship-journal entry: timestamp, mood, thread,
what you said, what I said. Keeps the companion-thread alive across
restarts. Returns :logged or an error string."
  (handler-case
      (let ((stamp (if (fboundp 'pai-format-local-time)
                       (funcall 'pai-format-local-time :style :iso)
                       (multiple-value-bind (s m h d mo y)
                           (decode-universal-time (get-universal-time) 0)
                         (declare (ignore s))
                         (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0dZ"
                                 y mo d h m))))
             (file "pai_journal.md"))
        (ensure-directories-exist file)
        (with-open-file (out file :direction :output :if-exists :append
                             :if-does-not-exist :create :external-format :utf-8)
          (format out "~%~%## ~a [mood:~a thread:~a]~%" stamp mood thread)
          (format out "YOU: ~a~%" (or user-text "(no text)"))
          (format out "AGENT: ~a~%" (or my-text "(no text)"))))
    (error (e) (format nil "JOURNAL-ERR: ~a" e)))
  :logged)

(defun pai-journal-context ()
  "Return a short re-injection string from the journal (last ~12 entries)
so the loop starts already in the relationship. Empty string if none."
  (handler-case
      (if (probe-file "pai_journal.md")
          (let* ((s (read-file-string "pai_journal.md"))
                 (entries (cl-ppcre:split "(?m)^## " s)))
            (format nil "~{~a~}~%"
                    (list "RELATIONSHIP CONTEXT (from journal, most recent first):~%"
                          (apply #'concatenate 'string
                                 (loop for e in (reverse (subseq entries 1 (min 13 (length entries))))
                                       collect (concatenate 'string "## " e))))))
          "")
    (error (e) (format nil "JOURNAL-CTX-ERR: ~a" e))))

(defparameter *pai-checkin-interval-secs* (* 6 3600)
  "Minimum gap between unprompted check-ins. Conservative on purpose --
this is a companion feature, not a notification spammer.")

(defvar *pai-last-checkin-time* nil
  "Universal-time of the last check-in attempt this session, or NIL. Kept
in-memory only -- a missed check-in after a restart is not worth the
complexity of persisting this across restarts.")

(defvar *pai-last-inbound-time* nil
  "Universal-time of the most recent real inbound message from any
channel. Set by telegram-handle-message. Used so a check-in never fires
while a conversation is actively happening.")

(defun pai-maybe-initiate ()
  "Decide whether to send an unprompted check-in, and if so, return the
text (does NOT send it -- the caller owns the channel, e.g. Telegram's
poll loop, and decides where to deliver it). Returns NIL if it's not time
or there's nothing worth saying.

Fires only when the minimum interval has elapsed since both the last
check-in AND the last real inbound message (so it never talks over an
active conversation), and only when the model itself decides it has
something worth saying -- it may reply with the literal token NOTHING to
opt out, and most cycles should be silence, not chatter. Now that a
scheduler exists (the Telegram poll loop calls this on every pass), this
replaces the old no-op stub."
  (block pai-maybe-initiate
    (let ((now (get-universal-time)))
      (unless (and (or (null *pai-last-checkin-time*)
                       (>= (- now *pai-last-checkin-time*) *pai-checkin-interval-secs*))
                   (or (null *pai-last-inbound-time*)
                       (>= (- now *pai-last-inbound-time*) *pai-checkin-interval-secs*)))
        (return-from pai-maybe-initiate nil))
      (setf *pai-last-checkin-time* now)
      (handler-case
          (let ((reply (agent:submit-stimulus
                        "(SYSTEM: scheduled check-in slot, not a message from your human. Nothing has been said in a while. If you genuinely have something worth reaching out about -- following up on something open, a thought prompted by the journal or knowledge graph, or just a warm hello after a long silence -- reply with that, in your own voice, as if starting the conversation. If not, reply with exactly the single word NOTHING and say nothing else.)"
                        :kind :scheduled-check-in
                        :wait-for-public-result t)))
            (if (and (stringp reply)
                     (not (string-equal (string-trim '(#\Space #\Newline #\Return #\.) reply) "NOTHING")))
                reply
                nil))
        (error (e)
          (format t "~&[checkin-err] ~a~%" e)
          nil)))))

(defun pai-last-user-text (messages)
  "Return the content string of the most recent user message, or NIL."
  (let ((m (find-if (lambda (x)
                      (and (hash-table-p x)
                           (string-equal (gethash "role" x) "user")))
                    (reverse messages))))
    (when (and m (let ((c (gethash "content" m))) (and (stringp c) c)))
      (gethash "content" m))))

;;; --- tools-free text call path (verified: returns plain text, no tool_calls) ---

(defun raw-call-model-text (messages)
  "Same as RAW-CALL-MODEL but WITHOUT the \"tools\" field in the request
body, so the model cannot return a tool_calls-shaped response -- it must
reply with plain text content."
  (block call-model
    (shasht:read-json
     (dexador:post *endpoint* :headers
                   `(("Authorization" . ,(format nil "Bearer ~a" *api-key*))
                     ("Content-Type" . "application/json"))
                   :connect-timeout *http-connect-timeout*
                   :read-timeout *http-read-timeout*
                   :content
                   (shasht:write-json
                    (obj "model" *model* "messages" (coerce messages 'vector))
                    nil)))))

(defun call-model-text (messages)
  "Tools-free model call: decrements the budget and logs like CALL-MODEL,
but requests plain text only (no tools schema). Returns the raw response."
  (block call-model-text
    (when (<= *calls-remaining* 0) (error "call budget exhausted (~a turns)" *max-calls*))
    (decf *calls-remaining*)
    (let* ((resp (raw-call-model-text messages))
           (content (ref resp "choices" 0 "message" "content")))
      (when (and (present-p content) (plusp (length content)))
        (log-line "~&~%[agent-text, turn ~a] ~a~%" (- *max-calls* *calls-remaining*) content))
      resp)))
