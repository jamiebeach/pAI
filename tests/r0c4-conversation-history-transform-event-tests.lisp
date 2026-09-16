(defpackage :agent (:use :cl))
(in-package :agent)

(ql:quickload '(:bordeaux-threads :shasht) :silent t)

(defvar *r0c4-pass* 0)
(defvar *r0c4-fail* 0)
(defvar *r0c4-events* nil)
(defvar *r0c4-real-log-event* nil)
(defvar *last-self-mod-history* nil)

(defun r0c4-check (name condition)
  (if condition
      (progn (incf *r0c4-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *r0c4-fail*) (format t "  FAIL ~a~%" name))))

(defun r0c4-signals-p (thunk)
  (handler-case (progn (funcall thunk) nil)
    (error () t)))

(defun obj (&rest kvs)
  (loop with table = (make-hash-table :test #'equal)
        for (key value) on kvs by #'cddr
        do (setf (gethash key table) value)
        finally (return table)))

(defun r0c4-log-event (type payload &key caused-by)
  (declare (ignore caused-by))
  (push (list type payload) *r0c4-events*)
  (length *r0c4-events*))

(defun r0c4-transform-events ()
  (remove-if-not
   (lambda (event) (string= "conversation-history-transform" (first event)))
   (reverse *r0c4-events*)))

(defun r0c4-write-exact (path content)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                            :if-does-not-exist :create :external-format :utf-8)
    (write-string content out)))

;; EVENT-LOG's established wrappers need only names at load time. They are not
;; exercised by this network-disabled persistence fixture.
(setf (fdefinition 'auto-turn) (lambda (prompt) prompt)
      (fdefinition 'execute) (lambda (tool-call) tool-call)
      (fdefinition 'propose-loop) (lambda (source) source)
      (fdefinition 'raw-call-model)
      (lambda (&rest arguments)
        (declare (ignore arguments))
        (obj "choices" (vector (obj "message" (obj "content" ""))))))

(load (test-source "event-log.lisp"))
(setf *r0c4-real-log-event* (fdefinition 'log-event)
      (fdefinition 'log-event) #'r0c4-log-event)

;; The persistence wrapper captures this base definition when loaded.
(setf (fdefinition '%run-self-mod-messages)
      (lambda (messages)
        (setf *last-self-mod-history* messages)
        "synthetic-turn-result"))
(load (test-source "conversation-persistence.lisp"))
(setf *r0c4-events* nil)

(r0c4-check "conversation history resolves below the selected state root"
             (equal (namestring (pai-state-path "conversation.json"))
                    (namestring *conversation-file*)))
(r0c4-check "conversation backups resolve below the selected state root"
             (equal (namestring (pai-state-path "conversation-backups/"))
                    (namestring *conversation-backup-dir*)))

(let* ((root #P"/tmp/r0c4-conversation-history/")
       (conversation (merge-pathnames "conversation.json" root))
       (rebuilt (merge-pathnames "rebuilt/conversation.json" root))
       (history
         (list
          (obj "role" "system" "content" "synthetic identity")
          (obj "role" "user" "content" "Unicode: π — synthetic only")
          (obj "role" "assistant" "content" :null
               "tool_calls"
               (vector
                (obj "id" "call-1" "type" "function"
                     "function"
                     (obj "name" "synthetic-tool"
                          "arguments" "{\"value\":1}"))))
          (obj "role" "tool" "tool_call_id" "call-1"
               "content" "synthetic result"))))
  (ensure-directories-exist conversation)
  (let ((*conversation-file* conversation))
    (%conv-persist-write history))

  (format t "~%== exact durable transform event ==~%")
  (let* ((events (r0c4-transform-events))
         (event (first events))
         (payload (second event))
         (content (and payload (gethash "content" payload))))
    (r0c4-check "successful replacement emits exactly one transform event"
                 (= 1 (length events)))
    (r0c4-check "event type and projection contract are stable"
                 (and (string= "conversation-history-transform" (first event))
                      (string= "conversation-history"
                               (gethash "projection" payload))
                      (string= "replace" (gethash "operation" payload))
                      (string= "conversation.json" (gethash "file" payload))
                      (string= "utf-8" (gethash "encoding" payload))))
    (r0c4-check "event content is byte-exact persisted JSON"
                 (string= (uiop:read-file-string conversation) content))
    (let* ((decoded (shasht:read-json content))
           (assistant (aref decoded 2))
           (tool-calls (gethash "tool_calls" assistant)))
      (r0c4-check "full nested tool fields and JSON null survive"
                   (and (= 4 (length decoded))
                        (eq :null (gethash "content" assistant))
                        (= 1 (length tool-calls))
                        (string= "synthetic-tool"
                                 (gethash "name"
                                          (gethash "function"
                                                   (aref tool-calls 0)))))))
    (r0c4-write-exact rebuilt content)
    (r0c4-check "event-only reconstruction is byte-identical"
                 (string= (uiop:read-file-string conversation)
                          (uiop:read-file-string rebuilt))))

  (format t "~%== replacement and wrapper convergence ==~%")
  (let* ((next-history
           (append history
                   (list (obj "role" "assistant"
                              "content" "synthetic final answer"))))
         (*conversation-file* conversation))
    (%conv-persist-write next-history)
    (let* ((events (r0c4-transform-events))
           (latest (second (car (last events)))))
      (r0c4-check "latest replacement event reconstructs latest file"
                   (and (= 2 (length events))
                        (string= (uiop:read-file-string conversation)
                                 (gethash "content" latest)))))
    (let ((*last-self-mod-history* nil))
      (r0c4-check "completed-turn wrapper preserves base result"
                   (string= "synthetic-turn-result"
                            (%run-self-mod-messages next-history)))
      (r0c4-check "completed-turn wrapper persists through the same seam"
                   (and (equal *last-self-mod-history* next-history)
                        (= 3 (length (r0c4-transform-events)))))))

  (format t "~%== failure and read-only boundaries ==~%")
  (let ((before (length (r0c4-transform-events)))
        (*conversation-file*
          #P"/tmp/r0c4-parent-does-not-exist/child/conversation.json"))
    (r0c4-check "failed durable write signals"
                 (r0c4-signals-p (lambda () (%conv-persist-write history))))
    (r0c4-check "failed durable write emits no transform event"
                 (= before (length (r0c4-transform-events)))))

  (let ((saved (fdefinition 'log-event))
        (*conversation-file* (merge-pathnames "sink-failure.json" root)))
    (unwind-protect
         (progn
           (setf (fdefinition 'log-event)
                 (lambda (&rest arguments)
                   (declare (ignore arguments))
                   (error "synthetic event sink failure")))
           (%conv-persist-write history))
      (setf (fdefinition 'log-event) saved))
    (r0c4-check "event-sink failure cannot alter successful file save"
                 (and (probe-file *conversation-file*)
                      (plusp (length (uiop:read-file-string
                                      *conversation-file*))))))

  (setf *r0c4-events* nil)
  (let ((*conversation-file* conversation))
    (r0c4-check "load returns the durable synthetic history"
                 (= 5 (length (%conv-persist-load)))))
  (r0c4-check "read-only load emits no transform event"
               (null (r0c4-transform-events)))

  (format t "~%== schema-invalid restore boundary ==~%")
  (let* ((invalid (merge-pathnames "schema-invalid.json" root))
         (backup-dir (merge-pathnames "schema-invalid-backups/" root))
         (bytes "[\"A\",\"B\"]")
         (*conversation-file* invalid)
         (*conversation-backup-dir* backup-dir))
    (r0c4-write-exact invalid bytes)
    (r0c4-check "syntax-valid non-object rows are rejected as absent history"
                 (null (%conv-persist-load)))
    (r0c4-check "schema rejection preserves the durable source byte-for-byte"
                 (string= bytes (uiop:read-file-string invalid)))
    (r0c4-check "schema-invalid history is not promoted into a boot backup"
                 (and (null (%conv-backup-durable-at-boot))
                      (null (directory (merge-pathnames "conversation-*.json"
                                                        backup-dir))))))

  (let* ((invalid (merge-pathnames "invalid-message-field.json" root))
         (*conversation-file* invalid))
    (r0c4-write-exact invalid "[{\"role\":7,\"content\":\"fixture\"}]")
    (r0c4-check "malformed message fields fail the complete restore closed"
                 (null (%conv-persist-load))))

  (format t "~%== real JSONL roundtrip ==~%")
  (let* ((ledger (merge-pathnames "roundtrip-events.jsonl" root))
         (*event-log-file* ledger)
         (*event-next-id* 0)
         (*event-ring* nil)
         (saved (fdefinition 'log-event))
         (content (uiop:read-file-string conversation)))
    (unwind-protect
         (progn
           (setf (fdefinition 'log-event) *r0c4-real-log-event*)
           (log-conversation-history-transform conversation content)
           (let* ((events (replay-events))
                  (event (first events))
                  (payload (and event (gethash "payload" event))))
             (r0c4-check "real JSONL append/replay preserves exact content"
                          (and (= 1 (length events))
                               (string= "conversation-history-transform"
                                        (gethash "type" event))
                               (string= content
                                        (gethash "content" payload))))))
      (setf (fdefinition 'log-event) saved))))

(format t "~%R0c4 conversation history-transform events: ~a passed, ~a failed.~%"
        *r0c4-pass* *r0c4-fail*)
(when (plusp *r0c4-fail*)
  (error "R0c4 conversation history-transform event tests failed"))
