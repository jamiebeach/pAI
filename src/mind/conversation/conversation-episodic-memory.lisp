;;;; conversation-episodic-memory.lisp -- durable shared conversational traces.
;;;; A companion cannot retrieve a shared life if its graph contains almost
;;;; exclusively internal reflections.  This records substantive turns
;;;; literally and retrieves only relevant shared material on later turns.

(in-package :agent)

(defparameter *conversation-episode-minimum-characters* 24)
(defparameter *conversation-episode-kinds* '("observation" "episode" "self-fact"))

(defvar *conversation-prompt-mutation-allowed-fn* nil
  "Port: () -> generalized boolean, or NIL when nothing has registered.

Whether legacy injectors may still splice into the system prompt is a context
layer policy, and this file used to call the context layer's predicate
directly -- the last compile-time reference from memory up into cognition.
The context layer registers itself here during :INSTALL.

Unset permits mutation, which is what the FBOUNDP guard this replaces did:
absent predicate, no early return.

NOTE: the deeper problem is that a memory file mutates the system prompt at
all. %CONVERSATION-MEMORY-REFRESH-SECTION rewrites a SHARED-MEMORY block
inside the system message, which is a publication concern; the port makes the
dependency explicit rather than fixing the layering. See docs/progress.md.")

(defun %conversation-prompt-mutation-allowed-p ()
  (cond (*conversation-prompt-mutation-allowed-fn*
         (funcall *conversation-prompt-mutation-allowed-fn*))
        ((fboundp 'context-projection-legacy-mutation-enabled-p)
         (funcall 'context-projection-legacy-mutation-enabled-p))
        (t t)))

(defun %conversation-episode-prompt-text (prompt)
  (cond ((stringp prompt) prompt)
        ((vectorp prompt)
         (loop for part across prompt
               when (and (hash-table-p part) (string= (gethash "type" part "") "text"))
                 return (gethash "text" part)))
        (t nil)))

(defun %conversation-memory-record-turn (prompt reply)
  (let ((user-text (%conversation-episode-prompt-text prompt)))
    (when (and (stringp user-text)
               (>= (length (string-trim '(#\Space #\Newline #\Tab) user-text))
                   *conversation-episode-minimum-characters*))
      (let ((id (memory-write-node
                 :kind "observation"
                 :content (format nil "Shared conversation — the operator: ~a~%the agent: ~a"
                                  user-text
                                  (if (and (stringp reply) (plusp (length reply))) reply "(no reply)"))
                 :importance 0.55 :arousal 0.35)))
        (when (fboundp 'log-event)
          (ignore-errors (log-event "conversation-episode-written" (obj "node_id" id))))
        id))))

(defun %conversation-memory-refresh-section (prompt)
  (unless (%conversation-prompt-mutation-allowed-p)
    (return-from %conversation-memory-refresh-section nil))
  (let ((sysmsg (find "system" *last-self-mod-history* :key (lambda (m) (gethash "role" m)) :test #'string=))
        (query (%conversation-episode-prompt-text prompt)))
    (when (and sysmsg (stringp query) (plusp (length query)))
      (let* ((matches (handler-case (memory-recall query :k 3 :debug t :kinds *conversation-episode-kinds*)
                        (error (e) (format t "~&[conversation-memory] recall failed: ~a~%" e) nil)))
             (items (remove-if (lambda (m) (search query (gethash "content" m "") :test #'char-equal)) matches))
             (content (gethash "content" sysmsg))
             (begin "<!-- SHARED-MEMORY:BEGIN -->") (end "<!-- SHARED-MEMORY:END -->")
             (bp (and (stringp content) (search begin content))) (ep (and (stringp content) (search end content)))
             (text (if items
                       (format nil "Use only if it naturally helps this turn; do not recite it or pretend it is current:~%~{- ~a~%~}"
                               (mapcar (lambda (m) (gethash "content" m)) items))
                       "(no directly relevant shared episode surfaced)")))
        (if (and bp ep (< bp ep))
            (setf (gethash "content" sysmsg)
                  (concatenate 'string (subseq content 0 (+ bp (length begin))) (format nil "~%~a~%" text) (subseq content ep)))
            (setf (gethash "content" sysmsg)
                  (format nil "~a~%~%## Relevant shared memories (retrieved, not invented)~%~a~%~a~%~a"
                          content begin text end)))))))

(unless (fboundp 'pai-base-auto-turn-conversation-episodic-memory)
  (setf (fdefinition 'pai-base-auto-turn-conversation-episodic-memory) (fdefinition 'auto-turn)))
(defun auto-turn (prompt)
  (%conversation-memory-refresh-section prompt)
  (let ((reply (funcall 'pai-base-auto-turn-conversation-episodic-memory prompt)))
    ;; replaces the lossy combined-summary recorder with ordered typed
    ;; public segments. Keep this exact legacy fallback when the new outer turn
    ;; context is absent (rollback/bare test loads).
    (unless (and (fboundp 'turn-capture-handles-current-turn-p)
                 (ignore-errors (funcall 'turn-capture-handles-current-turn-p)))
      (ignore-errors (%conversation-memory-record-turn prompt reply)))
    reply))
