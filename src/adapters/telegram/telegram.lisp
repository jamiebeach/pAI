;;;; telegram.lisp -- Telegram bridge for the agent (long-polling, no public URL).
;;;;
;;;; Loads ON TOP of agent.lisp + self-mod.lisp (+ enhancements.lisp).
;;;; Reuses auto-turn so the agent's voice, memory graph, web search, and
;;;; context compression all apply to Telegram messages automatically.
;;;;
;;;; Transport: uses `curl` via uiop:run-program (proven to reach
;;;; api.telegram.org from this container, where dexador stalled).
;;;;
;;;; Token: read from bot.txt (./bot.txt or /agent/pai/bot.txt) or the
;;;; TELEGRAM_BOT_TOKEN env var. NEVER hardcode the token here.
;;;;
;;;; CRITICAL: Telegram permits ONLY ONE getUpdates connection per bot.
;;;; A second connection (even a stale one still open server-side) yields
;;;; 409 Conflict. Therefore:
;;;;   - start-telegram is strictly idempotent: if a poll thread is already
;;;;     alive it returns that thread WITHOUT destroying anything.
;;;;   - stop never force-destroys a thread mid-long-poll (that leaves the
;;;;     in-flight curl connection open server-side for ~30-60s, which keeps
;;;;     colliding with the new thread). It sets a stop flag and joins; the
;;;;     thread exits on its own at the next loop check.
;;;;   - The long-poll timeout is kept short so a stopped/restarted
;;;;     connection frees quickly.

(ql:quickload '(:dexador) :silent t)

(in-package :agent)

(export '(start-telegram stop-telegram telegram-send telegram-poll-once
          telegram-check))

(defparameter *telegram-token* nil
  "Bot token, loaded lazily from bot.txt or env. Not set at compile time.")

(defparameter *telegram-offset* nil
  "Next update_id to fetch (long-poll cursor). Nil = fetch from start.")

(defparameter *telegram-stop* nil
  "When non-nil, the poll loop exits.")

(defparameter *telegram-thread* nil
  "Handle for the background poll thread, if running.")

(defvar *telegram-last-chat-id* nil
  "Chat-id of the most recent inbound message -- the only channel
pai-maybe-initiate currently knows how to reach unprompted, so
check-ins are addressed here.")
(defvar *public-outbound-envelope* nil)
(defvar *public-inbound-channel* "terminal")
(defvar *public-inbound-ordinary-reply-p* nil)

(defun %telegram-origin-envelope (kind content source authorization-kind
                                  authorization-id causal-id dedupe-key)
  (and (fboundp 'make-public-outbound-envelope)
       (funcall 'make-public-outbound-envelope
                :kind kind :channel "telegram" :content content
                :source-event-ids (and causal-id (list causal-id))
                :causal-event-ids (and causal-id (list causal-id))
                :authorization-kind authorization-kind
                :authorization-id authorization-id :source source
                :dedupe-key dedupe-key)))

(defun read-token-from-file (path)
  "FILE-LENGTH counts bytes, not characters, so (make-string (file-length s))
over-allocates for any multi-byte UTF-8 content and the unfilled tail comes
back as NUL characters -- trim to what READ-SEQUENCE actually filled."
  (let ((f (probe-file path)))
    (when f
      (let* ((txt (with-open-file (s f)
                    (let ((b (make-string (file-length s))))
                      (subseq b 0 (read-sequence b s))))))
        (string-trim '(#\Space #\Newline #\Return #\Tab) txt)))))

(defun telegram-token ()
  "Resolve the bot token from bot.txt or the TELEGRAM_BOT_TOKEN env var."
  (or *telegram-token*
      (setf *telegram-token*
            (or (uiop:getenv "TELEGRAM_BOT_TOKEN")
                (let ((root (uiop:getenv "PAI_SECRET_ROOT")))
                  (and root (plusp (length root))
                       (read-token-from-file
                        (merge-pathnames "bot.txt" (pathname root)))))
                (read-token-from-file "bot.txt")
                (read-token-from-file "/agent/pai/bot.txt")
                (read-token-from-file "/agent/bot.txt")))))

(defun telegram-api (method &key (timeout 20) post-json)
  "Call a Telegram Bot API method via curl. Returns parsed JSON or NIL."
  (let* ((url (format nil "https://api.telegram.org/bot~a/~a" (telegram-token) method))
         (cmd
          (if post-json
              (format nil "curl -s --max-time ~a -H 'Content-Type: application/json' -d ~s ~s"
                      timeout post-json url)
              (format nil "curl -s --max-time ~a '~a'" timeout url))))
    (handler-case
        (multiple-value-bind (out err-out exit-code)
            (uiop:run-program cmd :output :string :error-output :string :ignore-error-status t)
          (declare (ignore err-out))
          (unless (zerop exit-code)
            (format t "~&[telegram] curl exited ~a for '~a'~%" exit-code method))
          (when (and out (plusp (length out)))
            (handler-case (shasht:read-json out)
              (error () nil))))
      (error (e)
        (format t "~&[telegram] API '~a' failed: ~a~%" method e)
        nil))))

(defun telegram-get-updates (&optional (timeout 20))
  "Fetch pending updates via long poll. Returns the 'result' array or NIL.
curl's --max-time must exceed Telegram's long-poll TIMEOUT, or curl kills
the connection at the exact moment Telegram would otherwise respond at the
timeout boundary -- a race curl always loses (exit 28, CURLE_OPERATION_TIMEDOUT)."
  (let ((resp (telegram-api "getUpdates"
                             :timeout (+ timeout 10)
                             :post-json
                             (shasht:write-json
                              (obj "offset" (or *telegram-offset* 0)
                                   "timeout" timeout)
                              nil))))
    (if (and resp (eq (gethash "ok" resp) t))
        (gethash "result" resp)
        (progn
          (when resp
            (format t "~&[telegram] getUpdates error: ~a~%" (gethash "description" resp)))
          nil))))

(defun telegram-send (chat-id text)
  "Send a text message to CHAT-ID. Splits long messages to respect limits.

2026-07-27: found live -- this used to fire-and-forget with parse_mode
Markdown, never checking the response. Telegram's legacy Markdown parser
rejects the ENTIRE message on any unmatched _/*/`/[ (confirmed live:
'Social_need' alone -- one literal underscore in a modulator name the agent
now references constantly via its own introspection -- triggered 'Bad
Request: can't parse entities: Can't find end of the entity'), and
because the response was never inspected, every one of those failures was
completely silent: the agent generated a real reply, the turn completed
normally, and it just never arrived. Now checks the response and retries
the same chunk as plain text (no parse_mode) if Telegram rejects the
Markdown -- keeps nice formatting when it parses cleanly, guarantees
delivery either way."
  (let ((chunks
         (if (> (length text) 4000)
             (loop for i = 0 then (+ j 4000)
                   for j = (min (length text) (+ i 4000))
                   while (< i (length text))
                   collect (subseq text i j))
             (list text))))
    (dolist (chunk chunks)
      (let ((resp (telegram-api "sendMessage"
                                 :post-json
                                 (shasht:write-json
                                  (obj "chat_id" chat-id "text" chunk "parse_mode" "Markdown")
                                  nil))))
        (unless (and resp (eq (gethash "ok" resp) t))
          (format t "~&[telegram] sendMessage with Markdown failed (~a), retrying as plain text~%"
                  (and resp (gethash "description" resp)))
          (let ((retry-resp (telegram-api "sendMessage"
                                           :post-json
                                           (shasht:write-json (obj "chat_id" chat-id "text" chunk) nil))))
            (unless (and retry-resp (eq (gethash "ok" retry-resp) t))
              (format t "~&[telegram] sendMessage plain-text retry ALSO failed (~a) -- message genuinely undelivered~%"
                      (and retry-resp (gethash "description" retry-resp))))))))))

(defun telegram-handle-message (update)
  "Process one Telegram update: run auto-turn, reply. Updates the offset."
  (let* ((update-id (gethash "update_id" update))
         (message (gethash "message" update))
         (chat (and message (gethash "chat" message)))
         (chat-id (and chat (gethash "id" chat)))
         (text (and message (gethash "text" message))))
    (setf *telegram-offset* (1+ update-id))
    (when (and chat-id (stringp text) (plusp (length text)))
      (setf *telegram-last-chat-id* chat-id)
      (when (boundp '*pai-last-inbound-time*)
        (setf *pai-last-inbound-time* (get-universal-time)))
      (format t "~&[telegram] <= ~a~%" text)
      (handler-case
          (let* ((*public-inbound-channel* "telegram")
                 (*public-inbound-ordinary-reply-p* t)
                 (reply (submit-stimulus text :kind :user-message
                                              :wait-for-public-result t))
                 ;; AUTO historically publishes a placeholder for a NIL
                 ;; legacy return. Conscious Q2 has no publication outcome;
                 ;; manufacturing that placeholder would turn acceptance
                 ;; into speech, so it sends nothing.
                 (content (and (or reply
                                   (cognition-runtime-selected-p :auto))
                               (or reply "(no response)")))
                 (*public-outbound-envelope*
                   (and content
                        (%telegram-origin-envelope
                         :reply content "telegram-reactive"
                         :inbound-update (format nil "telegram-update:~a" update-id)
                         (format nil "telegram-update:~a" update-id)
                         (format nil "telegram-reply:~a" update-id)))))
            (when content (telegram-send chat-id content)))
        (error (e)
          (format t "~&[telegram] auto-turn failed: ~a~%" e)
          (let* ((content (format nil "I hit an error: ~a" e))
                 (*public-outbound-envelope*
                   (%telegram-origin-envelope
                    :system-alert content "telegram-reactive-error"
                    :inbound-update (format nil "telegram-update:~a" update-id)
                    (format nil "telegram-update:~a" update-id)
                    (format nil "telegram-error:~a" update-id))))
            (telegram-send chat-id content)))))))

(defun telegram-poll-once ()
  "One polling pass: fetch updates and handle each."
  (let ((updates (telegram-get-updates 20)))
    (when (and updates (typep updates 'sequence) (plusp (length updates)))
      (loop for u across updates
            do (telegram-handle-message u)))))

(defun telegram-check ()
  "Quick connectivity check: return bot info via getMe, or NIL on failure."
  (telegram-api "getMe" :timeout 20))

(defun telegram-poll-thread-alive-p ()
  "True if our single poll thread exists and is alive."
  (and *telegram-thread*
       (bt:thread-alive-p *telegram-thread*)))

(defun %stop-poll-gently ()
  "Ask the poll thread to stop and wait for it to exit on its own.
We do NOT destroy the thread: destroying it mid-long-poll leaves the
in-flight curl connection open server-side (Telegram holds it ~30-60s),
which would keep colliding with any replacement thread (409 Conflict).
Setting the flag lets the thread finish its current pass and exit cleanly."
  (setf *telegram-stop* t)
  (when (telegram-poll-thread-alive-p)
    (ignore-errors (bt:join-thread *telegram-thread* :timeout 70)))
  (setf *telegram-thread* nil))

(defun stop-telegram ()
  "Stop the Telegram poll thread and wait for it to exit cleanly."
  (%stop-poll-gently)
  (format t "~&[telegram] Stop requested.~%"))

#|
;;; --- chat-id persistence (added so outbound Telegram survives restarts)
;;; The bot learns your chat id from the first inbound message; we persist
;;; it to disk and re-seed it on start-telegram, so a container restart
;;; never drops the agent's ability to message you first.
|#

(defvar *telegram-chatid-file* "/agent/state/telegram_chatid.txt"
  "Path where the most-recent inbound chat id is persisted.")

(defun telegram-chatid-save (chat-id)
  "Persist CHAT-ID so a restart can re-seed *telegram-last-chat-id*."
  (handler-case
      (with-open-file (out *telegram-chatid-file*
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
        (format out "~a" chat-id))
    (error (e) (format t "~&[telegram] chatid save failed: ~a~%" e))))

(defun telegram-chatid-load ()
  "Re-seed *telegram-last-chat-id* from the persisted file (written by
telegram-chatid-save on inbound). Returns the chat id or NIL. Lets outbound
Telegram messages survive a container restart without waiting for a new
inbound message. Runs whether the var is unbound (fresh image) or empty."
  (when (probe-file *telegram-chatid-file*)
    (let ((txt (string-trim '(#\Space #\Newline #\Return #\Tab)
                            (read-file-string *telegram-chatid-file*))))
      (when (plusp (length txt))
        (setf *telegram-last-chat-id* (read-from-string txt))
        *telegram-last-chat-id*))))

(defun start-telegram ()
  "Begin long-polling for Telegram messages in a background thread.
Strictly idempotent: if a poll thread is already alive, return it without
touching anything (no destroy, no second connection). This is what keeps
us from ever running two getUpdates instances against the bot. A fresh boot
also refuses to poll until conversation persistence has backed up and restored
the durable history; otherwise a queued Telegram update can race restoration
and replace the recovered conversation with a three-message fresh turn."
  (block start-telegram
    (unless (and (fboundp 'conversation-persistence-ready-p)
                 (funcall 'conversation-persistence-ready-p))
      (format t "~&[telegram] Deferred: conversation persistence is not input-ready.~%")
      (return-from start-telegram :conversation-not-ready))
    (unless (telegram-token)
      (format t "~&[telegram] No bot token found (bot.txt / TELEGRAM_BOT_TOKEN). Aborting.~%")
      (return-from start-telegram nil))
    (when (telegram-poll-thread-alive-p)
      (format t "~&[telegram] Poll thread already running; returning existing instance.~%")
      (return-from start-telegram *telegram-thread*))
    ;; Recover the persisted chat id so outbound messages survive a restart.
    (telegram-chatid-load)
    (setf *telegram-stop* nil)
    (setf *telegram-thread*
          (bt:make-thread
           (lambda ()
             (format t "~&[telegram] Poll thread started.~%")
             (loop until *telegram-stop*
                   do (block poll-iteration
                        (handler-bind
                            ((error (lambda (e)
                                      (format t "~&[telegram] poll error: ~a~%" e)
                                      (sb-debug:print-backtrace :count 30 :stream *standard-output*)
                                      (return-from poll-iteration))))
                          (telegram-poll-once)
                          (when (and *telegram-last-chat-id* (fboundp 'pai-maybe-initiate))
                            (let ((msg (funcall 'pai-maybe-initiate)))
                              (when msg
                                (let ((*public-outbound-envelope*
                                        (%telegram-origin-envelope
                                         :initiative msg "telegram-poll-legacy-initiative"
                                         :legacy-initiative "pai-maybe-initiate"
                                         nil (format nil "poll-initiative:~a"
                                                     (get-universal-time)))))
                                  (telegram-send *telegram-last-chat-id* msg)))))))
                      (sleep 1))
             (format t "~&[telegram] Poll thread stopped.~%"))
           :name "pai-telegram-poll"))
    (format t "~&[telegram] Started in background thread.~%")
    *telegram-thread*))
