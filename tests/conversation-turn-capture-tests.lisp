(unless (find-package :agent) (defpackage :agent (:use :cl)))
(in-package :agent)

(ql:quickload :bordeaux-threads :silent t)

(unless (fboundp 'obj)
  (defun obj (&rest pairs)
    (loop with table = (make-hash-table :test #'equal)
          for (key value) on pairs by #'cddr
          do (setf (gethash key table) value)
          finally (return table))))
(unless (fboundp 'ref)
  (defun ref (object &rest keys)
    (reduce (lambda (value key)
              (cond ((hash-table-p value) (gethash key value))
                    ((and (vectorp value) (integerp key)) (aref value key))
                    ((and (listp value) (integerp key)) (nth key value))))
            keys :initial-value object)))

(defvar *turn-capture-test-pass* 0)
(defvar *turn-capture-test-fail* 0)
(defun turn-capture-check (name condition)
  (if condition
      (progn (incf *turn-capture-test-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *turn-capture-test-fail*) (format t "  FAIL ~a~%" name))))

(defvar *turn-capture-test-events* nil)
(defvar *turn-capture-test-event-id* 0)
(defvar *turn-capture-test-responses* nil)
(defvar *turn-capture-test-scenario* :no-tool)
(defvar *turn-capture-test-persisted* nil)
(defvar *turn-capture-test-timing* nil)
(defvar *current-causing-event-id* nil)
(defvar *timing-origin* nil)
(defvar *timing-turn-id* nil)
(defvar *public-inbound-ordinary-reply-p* nil)

(defun log-event (type payload &key caused-by)
  (let ((event (obj "id" (incf *turn-capture-test-event-id*)
                    "type" type "payload" payload "caused_by" caused-by)))
    (push event *turn-capture-test-events*)
    (gethash "id" event)))

(defun replay-events (&key from to types limit exclude-types)
  (declare (ignore from to types limit exclude-types))
  (reverse *turn-capture-test-events*))

(defun call-with-timing-trace (thunk &rest keys)
  (push keys *turn-capture-test-timing*)
  (funcall thunk))

(defun turn-capture-test-response (content &optional tool-calls)
  (let ((message (obj "role" "assistant" "content" content)))
    (when tool-calls (setf (gethash "tool_calls" message) (coerce tool-calls 'vector)))
    (obj "choices" (vector (obj "message" message)))))

(defun turn-capture-test-tool-call (id name)
  (obj "id" id "function" (obj "name" name "arguments" "{\"q\":\"x\"}")))

;; CALL-MODEL is a seam (P0c item 3): conversation-turn-capture.lisp below
;; registers a layer on it, which requires the seam to already exist. A
;; plain DEFUN would not create one, and REGISTER-LAYER would error "No seam
;; named CALL-MODEL" (gotcha 12).
(define-seam call-model (messages)
  (or (pop *turn-capture-test-responses*)
      (error "No fixture response")))

(defun auto-turn (prompt)
  ;; Mirrors the cooperation points in EVENT-LOG while keeping this
  ;; suite independent of the full production load chain.
  (let* ((user-event-id (log-event "user-message" (obj "text" prompt)))
         (*current-causing-event-id* user-event-id))
    (%turn-capture-register-user-event user-event-id prompt)
    (let ((reply
            (ecase *turn-capture-test-scenario*
              (:no-tool
               (gethash "content" (ref (call-model nil) "choices" 0 "message")))
              (:direct-tool
               (let* ((first (call-model nil))
                      (tool-call (aref (ref first "choices" 0 "message" "tool_calls") 0))
                      (tool-message
                        (obj "role" "tool" "tool_call_id" (gethash "id" tool-call)
                             "content" "direct search result")))
                 (declare (ignore first))
                 (gethash "content"
                          (ref (call-model (list tool-message))
                               "choices" 0 "message"))))
              (:generic-tool
               (let* ((first (call-model nil))
                      (tool-call (aref (ref first "choices" 0 "message" "tool_calls") 0))
                      (call-id (log-event "tool-call" (obj "name" "fixture")))
                      (result (obj "role" "tool" "tool_call_id" (gethash "id" tool-call)
                                   "content" "generic tool result"))
                      (result-id nil))
                 (%turn-capture-register-tool-call-event call-id tool-call)
                 (setf result-id (log-event "tool-result"
                                            (obj "name" "fixture"
                                                 "content" "generic tool result")
                                            :caused-by call-id))
                 (%turn-capture-register-tool-result-event result-id tool-call result)
                 (gethash "content"
                          (ref (call-model (list result)) "choices" 0 "message"))))
              (:empty-interim
               (let* ((first (call-model nil))
                      (tool-call (aref (ref first "choices" 0 "message" "tool_calls") 0))
                      (tool-message
                        (obj "role" "tool" "tool_call_id" (gethash "id" tool-call)
                             "content" "empty-interim result")))
                 (gethash "content"
                          (ref (call-model (list (obj "role" "system"
                                                     "content" "compacted history")
                                                 tool-message))
                               "choices" 0 "message")))))))
      (%turn-capture-register-completion reply user-event-id)
      reply)))

(load (test-source "conversation-turn-capture.lisp"))

(let ((*timing-origin* "direct")
      (*turn-capture-origin* nil))
  (turn-capture-check
   "direct timing origin normalizes to inbound conversation"
   (string= "conversation" (%turn-capture-origin))))
(let ((*timing-origin* "web")
      (*turn-capture-origin* nil)
      (*public-inbound-ordinary-reply-p* t))
  (turn-capture-check
   "trusted web inbound binding normalizes to conversation"
   (string= "conversation" (%turn-capture-origin))))
(let ((*timing-origin* "web")
      (*turn-capture-origin* nil)
      (*public-inbound-ordinary-reply-p* nil))
  (turn-capture-check
   "unbound web tracing label is not reply authority"
   (string= "web" (%turn-capture-origin))))
(let ((*timing-origin* "web")
      (*turn-capture-origin* "initiative")
      (*public-inbound-ordinary-reply-p* t))
  (turn-capture-check
   "explicit background origin overrides inbound ambient binding"
   (string= "initiative" (%turn-capture-origin))))
(let ((*timing-origin* "initiative")
      (*turn-capture-origin* nil))
  (turn-capture-check
   "explicit background timing origin remains non-conversation"
   (string= "initiative" (%turn-capture-origin))))
(let ((web-source (uiop:read-file-string (namestring (test-source "web-terminal.lisp"))))
      (telegram-source
        (uiop:read-file-string (namestring (test-source "telegram.lisp"))))
      (driver-source
        (uiop:read-file-string
         (namestring
          (merge-pathnames "scripts/conscious-conversation.lisp" *pai-root*)))))
  (turn-capture-check
   "interactive adapters carry explicit inbound reply authority at their current seams"
   (and
    ;; Telegram still enters the legacy AUTO-TURN capture boundary directly.
    (search "(*public-inbound-ordinary-reply-p* t)" telegram-source)
    ;; Web now delegates to the selected mind and carries its public channel as
    ;; an argument; the driver installs the recursive or coordinated ingress.
    (search "(funcall *v2-submit-fn* text \"web\")" web-source)
    (search "web-terminal-configure-submit" driver-source)
    (search "prompt :channel channel" driver-source))))
(let ((context (%turn-capture-new-context "turn clock fixture")))
  (turn-capture-check
   "new inbound capture owns one universal turn clock"
   (and (integerp (gethash "as_of" context))
        (plusp (gethash "as_of" context)))))
(let* ((*timing-turn-id* "turn-canonical-fixture")
       (context (%turn-capture-new-context "canonical identity fixture")))
  (turn-capture-check
   "capture reuses the active turn identity"
   (string= "turn-canonical-fixture" (gethash "turn_id" context))))
(turn-capture-worker-stop)

(defun turn-capture-test-reset ()
  (setf *turn-capture-test-events* nil
        *turn-capture-test-event-id* 0
        *turn-capture-test-responses* nil
        *turn-capture-test-persisted* nil
        *turn-capture-test-timing* nil)
  (bt:with-lock-held (*turn-capture-queue-lock*)
    (setf *turn-capture-queue* nil)
    (clrhash *turn-capture-queued-ids*)
    (clrhash *turn-capture-completed-ids*))
  (setf *turn-capture-persist-fn*
        (lambda (context) (push context *turn-capture-test-persisted*) t)))

(defun turn-capture-test-flush ()
  (loop while (%turn-capture-worker-step))
  (first *turn-capture-test-persisted*))

(defun turn-capture-test-roles (context)
  (mapcar (lambda (entry) (gethash "role" entry))
          (%turn-capture-ordered-entries context)))

(defun turn-capture-test-contents (context)
  (mapcar (lambda (entry) (gethash "content" entry))
          (%turn-capture-ordered-entries context)))

(format t "~%== persistence content sanitization ==~%")
(multiple-value-bind (content replacements)
    (%turn-capture-persistence-content
     (format nil "before~cafter~c" (code-char 0) (code-char 0)))
  (turn-capture-check "NUL bytes become explicit markers"
                      (string= content "before<NUL>after<NUL>"))
  (turn-capture-check "NUL replacement count is exact" (= replacements 2)))
(multiple-value-bind (content replacements)
    (%turn-capture-persistence-content "ordinary text")
  (turn-capture-check "ordinary content is byte-for-byte unchanged"
                      (string= content "ordinary text"))
  (turn-capture-check "ordinary content records zero replacements"
                      (zerop replacements)))
(multiple-value-bind (content replacements truncated-p original-chars)
    (%turn-capture-memory-content
     "tool" (make-string 12000 :initial-element #\z) 42)
  (turn-capture-check "large tool result is compacted only for semantic memory"
                      (and truncated-p
                           (= original-chars 12000)
                           (zerop replacements)
                           (<= (length content)
                               *turn-capture-tool-memory-max-chars*)
                           (search "full evidence event: 42" content))))
(multiple-value-bind (content replacements truncated-p original-chars)
    (%turn-capture-memory-content
     "assistant" (make-string 12000 :initial-element #\a) 43)
  (declare (ignore replacements))
  (turn-capture-check "lived assistant speech remains exact"
                      (and (not truncated-p) (= original-chars 12000)
                           (= (length content) 12000))))

(format t "~%== no-tool complete turn ==~%")
(turn-capture-test-reset)
(setf *turn-capture-test-scenario* :no-tool
      *turn-capture-test-responses* (list (turn-capture-test-response "complete answer")))
(let ((reply (auto-turn "user report"))
      (context (turn-capture-test-flush)))
  (turn-capture-check "reply is unchanged" (string= reply "complete answer"))
  (turn-capture-check "user and final assistant captured"
                      (equal (turn-capture-test-roles context)
                             '("user" "assistant")))
  (turn-capture-check "content reconstructs exactly"
                      (equal (turn-capture-test-contents context)
                             '("user report" "complete answer")))
  (turn-capture-check "ready and complete journal events emitted"
                      (and (find "turn-capture-ready" *turn-capture-test-events*
                                 :key (lambda (event) (gethash "type" event))
                                 :test #'string=)
                           (find "turn-capture-complete" *turn-capture-test-events*
                                 :key (lambda (event) (gethash "type" event))
                                 :test #'string=)))
  (turn-capture-check "background persistence has a dedicated timing trace"
                      (string= "turn_capture.persist"
                               (getf (first *turn-capture-test-timing*)
                               :root-span))))

(format t "~%== publication boundary owns the single final segment ==~%")
(turn-capture-test-reset)
(let* ((context (%turn-capture-new-context "publication test"))
       (*turn-capture-context* context)
       (user-event-id (log-event "user-message" (obj "text" "publication test"))))
  (%turn-capture-register-user-event user-event-id "publication test")
  (%turn-capture-register-assistant-response
   (turn-capture-test-response "rejected private draft"))
  (%turn-capture-register-assistant-response
   (turn-capture-test-response "accepted public reply"))
  (%turn-capture-register-completion "accepted public reply" user-event-id)
  (turn-capture-check
   "tool-free drafts are not journaled before publication completes"
   (equal (turn-capture-test-roles context) '("user" "assistant")))
  (turn-capture-check
   "only the accepted public reply is captured"
   (equal (turn-capture-test-contents context)
          '("publication test" "accepted public reply")))
  (turn-capture-check
   "exactly one final agent event is emitted"
   (= 1 (count "agent-message" *turn-capture-test-events*
               :key (lambda (event) (gethash "type" event))
               :test #'string=))))

(format t "~%== text before directly-dispatched tool and final text ==~%")
(turn-capture-test-reset)
(setf *turn-capture-test-scenario* :direct-tool
      *turn-capture-test-responses*
      (list (turn-capture-test-response
             "I will check first."
             (list (turn-capture-test-tool-call "direct-1" "brave-search")))
            (turn-capture-test-response "Here is the grounded answer.")))
(let ((context (progn (auto-turn "find this") (turn-capture-test-flush))))
  (turn-capture-check "all direct-tool public parts are ordered"
                      (equal (turn-capture-test-roles context)
                             '("user" "assistant" "tool" "assistant")))
  (turn-capture-check "pre-tool and final text both retained"
                      (equal (turn-capture-test-contents context)
                             '("find this" "I will check first."
                               "direct search result" "Here is the grounded answer.")))
  (turn-capture-check "direct dispatcher fallback creates one tool result event"
                      (= 1 (count "tool-result" *turn-capture-test-events*
                                  :key (lambda (event) (gethash "type" event))
                                  :test #'string=)))
  (let* ((event (find "tool-result" *turn-capture-test-events*
                      :key (lambda (row) (gethash "type" row))
                      :test #'string=))
         (payload (and event (gethash "payload" event))))
    (turn-capture-check "direct tool event retains call/result correlation"
                        (and (string= "direct-1"
                                      (gethash "tool_call_id" payload))
                              (string= "tool-result:direct-1"
                                       (gethash "tool_result_id" payload))))))
  (let* ((event (find "tool-turn-committed" *turn-capture-test-events*
                      :key (lambda (row) (gethash "type" row))
                      :test #'string=))
         (payload (and event (gethash "payload" event))))
    (turn-capture-check "persisted tool turn emits one content-free committed signal"
                        (and (= 1 (count "tool-turn-committed"
                                         *turn-capture-test-events*
                                         :key (lambda (row) (gethash "type" row))
                                         :test #'string=))
                             (= 1 (gethash "tool_entry_count" payload))
                             (= 4 (gethash "entry_count" payload))
                             (gethash "caused_by" event))))

(format t "~%== generic tool uses existing event-log correlation ==~%")
(turn-capture-test-reset)
(setf *turn-capture-test-scenario* :generic-tool
      *turn-capture-test-responses*
      (list (turn-capture-test-response
             "Using a tool."
             (list (turn-capture-test-tool-call "generic-1" "fixture")))
            (turn-capture-test-response "Tool work complete.")))
(let ((context (progn (auto-turn "do work") (turn-capture-test-flush))))
  (turn-capture-check "generic tool reconstructs every segment"
                      (equal (turn-capture-test-roles context)
                             '("user" "assistant" "tool" "assistant")))
  (turn-capture-check "generic tool events are not duplicated"
                      (and (= 1 (count "tool-call" *turn-capture-test-events*
                                       :key (lambda (event) (gethash "type" event))
                                       :test #'string=))
                           (= 1 (count "tool-result" *turn-capture-test-events*
                                       :key (lambda (event) (gethash "type" event))
                                       :test #'string=)))))

(format t "~%== empty interim and compaction ==~%")
(turn-capture-test-reset)
(setf *turn-capture-test-scenario* :empty-interim
      *turn-capture-test-responses*
      (list (turn-capture-test-response
             "" (list (turn-capture-test-tool-call "empty-1" "brave-search")))
            (turn-capture-test-response "final after compaction")))
(let ((context (progn (auto-turn "compact me") (turn-capture-test-flush))))
  (turn-capture-check "empty assistant segment excluded"
                      (equal (turn-capture-test-roles context)
                             '("user" "tool" "assistant")))
  (turn-capture-check "tool result survives compacted input"
                      (member "empty-interim result"
                              (turn-capture-test-contents context)
                              :test #'string=)))

(format t "~%== proactive label and persistence isolation ==~%")
(turn-capture-test-reset)
(setf *turn-capture-test-scenario* :no-tool
      *turn-capture-test-responses* (list (turn-capture-test-response "proactive text")))
(let ((*turn-capture-origin* "initiative"))
  (auto-turn "initiative seed"))
(let ((context (turn-capture-test-flush)))
  (turn-capture-check "proactive origin retained"
                      (string= "initiative" (gethash "origin" context))))

(turn-capture-test-reset)
(setf *turn-capture-test-scenario* :no-tool
      *turn-capture-test-responses* (list (turn-capture-test-response "reply survives"))
      *turn-capture-persist-fn* (lambda (context) (declare (ignore context))
                                  (error "forced persistence failure")))
(let ((reply (auto-turn "failure case")))
  (%turn-capture-worker-step)
  (turn-capture-check "persistence failure does not alter reply"
                      (string= reply "reply survives"))
  (turn-capture-check "persistence failure is explicit"
                      (= 1 (count "turn-capture-persistence-error"
                                  *turn-capture-test-events*
                                  :key (lambda (event) (gethash "type" event))
                                  :test #'string=))))

(format t "~%== private cognitive path exclusion and source contract ==~%")
(let ((source (string-downcase
               (uiop:read-file-string
                (namestring (test-source "conversation-turn-capture.lisp"))))))
  (turn-capture-check "raw-call-model is not wrapped"
                      (not (search "(defun raw-call-model" source)))
  (turn-capture-check "cognitive-call is not wrapped"
                      (not (search "(defun cognitive-call" source))))

(format t "~%== post-persistence observer isolation ==~%")
(let ((observed 0))
  (setf *turn-capture-complete-hooks* nil)
  (turn-capture-register-complete-hook
   'failing-fixture (lambda (&rest arguments) (declare (ignore arguments))
                      (error "observer failure")))
  (turn-capture-register-complete-hook
   'successful-fixture
   (lambda (context episode-id node-ids)
     (declare (ignore context episode-id node-ids)) (incf observed)))
  ;; Re-registering by name replaces rather than duplicates the observer.
  (turn-capture-register-complete-hook
   'successful-fixture
   (lambda (context episode-id node-ids)
     (declare (ignore context episode-id node-ids)) (incf observed)))
  (turn-capture-test-reset)
  (setf *turn-capture-test-responses*
        (list (turn-capture-test-response "observer-safe reply")))
  (auto-turn "observer-safe prompt")
  (%turn-capture-worker-step)
  (turn-capture-check "successful observer runs once" (= observed 1))
  (turn-capture-check
   "observer failure is content-free and explicit"
   (= 1 (count "turn-capture-completion-hook-error"
               *turn-capture-test-events*
               :key (lambda (event) (gethash "type" event)) :test #'string=)))
  (turn-capture-check
   "observer failure cannot trigger raw-persistence retry"
   (zerop (count "turn-capture-persistence-error" *turn-capture-test-events*
                 :key (lambda (event) (gethash "type" event)) :test #'string=))))

(format t "~%== reciprocity reply lifecycle hook ==~%")
(let ((observations nil))
  (setf (fdefinition 'reciprocity-canary-observe-reply)
        (lambda (prompt &key origin)
          (push (list prompt origin) observations)))
  (turn-capture-test-reset)
  (setf *turn-capture-test-scenario* :no-tool
        *turn-capture-test-responses*
        (list (turn-capture-test-response "completed reply")))
  (let ((*turn-capture-origin* "web"))
    (auto-turn "successful inbound turn"))
  (turn-capture-check "successful public turn reaches reply observer once"
                      (and (= 1 (length observations))
                           (equal '("successful inbound turn" "web")
                                  (first observations))))
  (let ((before (length observations)))
    (setf *turn-capture-test-responses* nil)
    (handler-case (auto-turn "failed inbound turn")
      (error () nil))
    (turn-capture-check "failed turn never closes an unsolicited outreach"
                        (= before (length observations)))))

(format t "~%== reload safety ==~%")
;; AUTO-TURN remains the old rename-and-fall-through idiom (not yet
;; converted); this simulates observability-tracing.lisp's timing wrapper
;; landing on top of it and checks a reload still recaptures the true base.
;; CALL-MODEL is now a seam (P0c item 3) -- register-layer is reload-safe by
;; construction, so the equivalent property is `seam-layers-in-order`
;; unchanged across a reload, not a saved-original fdefinition survives.
(let ((true-auto-base (fdefinition 'pai-base-auto-turn-turn-capture))
      (capture-auto (fdefinition 'auto-turn))
      (call-layers-before (seam-layers-in-order 'call-model)))
  (setf (fdefinition 'pai-base-auto-turn-timing) capture-auto
        (fdefinition '%timing-auto-turn)
        (lambda (prompt) (funcall 'pai-base-auto-turn-timing prompt))
        (fdefinition 'auto-turn) (fdefinition '%timing-auto-turn))
  (load (test-source "conversation-turn-capture.lisp"))
  (turn-capture-worker-stop)
  (turn-capture-check "timing-order reload retains true auto-turn base"
                      (eq true-auto-base
                          (fdefinition 'pai-base-auto-turn-turn-capture)))
  (turn-capture-check "call-model reload does not duplicate or drop layers"
                      (equal call-layers-before (seam-layers-in-order 'call-model)))
  (setf *turn-capture-test-responses*
        (list (turn-capture-test-response "reload smoke check")))
  (turn-capture-check "call-model chain remains callable end to end"
                      (equal "reload smoke check"
                             (gethash "content"
                                      (ref (call-model nil) "choices" 0 "message")))))

(format t "~%~a passed, ~a failed~%"
        *turn-capture-test-pass* *turn-capture-test-fail*)
(when (plusp *turn-capture-test-fail*) (sb-ext:exit :code 1))
