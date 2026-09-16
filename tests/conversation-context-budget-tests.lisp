(defpackage :agent (:use :cl))
(in-package :agent)
(ql:quickload '(:bordeaux-threads :shasht) :silent t)

(defvar *ccb-pass* 0)
(defvar *ccb-fail* 0)
(defun ccb-check (name condition)
  (if condition
      (progn (incf *ccb-pass*) (format t "PASS ~a~%" name))
      (progn (incf *ccb-fail*) (format t "FAIL ~a~%" name))))
(defun obj (&rest kvs)
  (loop with table = (make-hash-table :test #'equal)
        for (key value) on kvs by #'cddr
        do (setf (gethash key table) value)
        finally (return table)))

(defvar *conversation-context-budget-mode* :shadow)
(load (test-source "conversation-context-budget.lisp"))

(defun ccb-message (role content &optional tool-calls)
  (let ((message (obj "role" role "content" content)))
    (when tool-calls (setf (gethash "tool_calls" message) tool-calls))
    message))

(let* ((canonical (ccb-message "system" "identity"))
       (briefs (loop for i from 1 to 17
                     collect (ccb-message
                              "system"
                              (format nil "[Picking up after a closed episode: handoff ~d]" i))))
       (body (loop for i from 1 to 18
                   collect (ccb-message (if (oddp i) "user" "assistant")
                                        (format nil "turn ~d" i))))
       (input (append (list canonical) briefs body))
       (legacy-calls 0)
       (summary-calls 0)
       (result
         (conversation-context-budget-manage
          input
          (lambda (messages) (incf legacy-calls) messages)
          (lambda (messages)
            (declare (ignore messages))
            (incf summary-calls)
            (ccb-message "system" "[Summary of earlier context: compact]")))))
  (ccb-check "shadow returns byte-for-byte legacy list" (eq result input))
  (ccb-check "shadow invokes legacy manager once" (= legacy-calls 1))
  (ccb-check "shadow never invokes model summarizer" (zerop summary-calls))
  (let ((report (conversation-context-budget-report)))
    (ccb-check "shadow detects all accumulated handoffs"
               (= 17 (gethash "leading_briefs" report)))
    (ccb-check "shadow candidate folds continuity into one system record"
               (= 19 (gethash "candidate_records" report)))
    (ccb-check "shadow remains non-enforcing" (null (gethash "enforced" report)))))

(setf *conversation-context-budget-mode* :enforced)

(let* ((canonical (ccb-message "system" "identity"))
       (older (loop for i from 1 to 30
                    collect (ccb-message
                             (if (oddp i) "user" "assistant")
                             (make-string 900 :initial-element #\o))))
       (game (list
              (ccb-message "user"
                           "Want to play a game? I say a word and you say the first word that comes to mind.")
              (ccb-message "assistant" "Absolutely. Go for it.")
              (ccb-message "user" "blue")))
       (result
         (conversation-context-budget-manage
          (append (list canonical) older game)
          #'identity
          (lambda (messages)
            (declare (ignore messages))
            (ccb-message "system" "[Summary of earlier context: compact]")))))
  (ccb-check "word-game contract survives tighter context"
             (find "Want to play a game? I say a word and you say the first word that comes to mind."
                   result :key #'%ccb-content :test #'string=))
  (ccb-check "current one-word game move survives tighter context"
             (string= "blue" (%ccb-content (car (last result)))))
  (ccb-check "successful prompt remains within 100K estimated-token target"
             (<= (%ccb-message-estimated-tokens result)
                 *conversation-context-target-tokens*)))

(let* ((canonical (ccb-message "system"
                               (make-string 12379 :initial-element #\s)))
       (brief (ccb-message
               "system"
               (format nil "[Continuity brief:~%~a]"
                       (make-string 3950 :initial-element #\c))))
       (body (list
              (ccb-message "user" "Earlier ordinary turn")
              (ccb-message "assistant" "Earlier answer")
              (ccb-message "user" "Want to keep the word game going?")
              (ccb-message "assistant" "Absolutely. Your turn.")
              (ccb-message "user"
                           "I say a word and you say the first word that comes to mind.")
              (ccb-message "assistant" "Got it. Hit me.")
              (ccb-message "user" "blonde")))
       (result
         (conversation-context-budget-manage
          (append (list canonical brief) body) #'identity
          (lambda (messages)
            (declare (ignore messages))
            (error "existing continuity must avoid summarizer")))))
  (ccb-check "production-sized fixed overhead retains game instruction"
             (find "I say a word and you say the first word that comes to mind."
                   result :key #'%ccb-content :test #'string=))
  (ccb-check "production-sized fixed overhead retains current move"
             (string= "blonde" (%ccb-content (car (last result)))))
  (ccb-check "production-sized report exposes actual retained body"
             (= (length body)
                (gethash "retained_body_records"
                         (conversation-context-budget-report))))
  (ccb-check "production-sized continuity is re-bounded"
             (multiple-value-bind (ignored briefs ignored-body valid-p)
                 (%ccb-split result)
               (declare (ignore ignored ignored-body))
               (and valid-p (= 1 (length briefs))
                    (<= (length (%ccb-content (first briefs)))
                        (+ *conversation-context-brief-chars* 32))))))

(let* ((canonical (ccb-message "system"
                               (make-string 405000 :initial-element #\s)))
       (body (loop for i from 1 to 40
                   collect (ccb-message (if (oddp i) "user" "assistant")
                                        (format nil "recent ~d" i))))
       (result (conversation-context-budget-manage
                (append (list canonical) body) #'identity
                (lambda (messages) (declare (ignore messages)) nil)))
       (report (conversation-context-budget-report)))
  (multiple-value-bind (ignored ignored-briefs retained valid-p)
      (%ccb-split result)
    (declare (ignore ignored ignored-briefs))
    (ccb-check "oversized fixed overhead preserves recent-dialogue floor"
               (and valid-p
                    (>= (length retained)
                        *conversation-context-min-recent-records*)))
    (ccb-check "soft target miss is explicit when recency wins"
               (null (gethash "target_met" report)))))

(let* ((canonical (ccb-message "system" "identity"))
       (assistant-call (ccb-message "assistant" "" (vector (obj "id" "call-1"))))
       (tool-result (ccb-message "tool" "result"))
       (body (append (loop for i from 1 to 24
                           collect (ccb-message (if (oddp i) "user" "assistant")
                                                (make-string 1500 :initial-element #\x)))
                     (list assistant-call tool-result (ccb-message "assistant" "done"))))
       (summary-calls 0)
       (result
         (conversation-context-budget-manage
          (append (list canonical
                        (ccb-message "system" "[Summary of earlier context: old]"))
                  body)
          #'identity
          (lambda (messages)
            (declare (ignore messages))
            (incf summary-calls)
            (ccb-message "system" "[Summary of earlier context: compact]")))))
  (ccb-check "existing continuity avoids another summarizer call"
             (zerop summary-calls))
  (ccb-check "enforced result satisfies hard invariants"
             (%ccb-valid-candidate-p result))
  (ccb-check "tool result is not orphaned at body start"
             (multiple-value-bind (canonical briefs retained valid-p)
                 (%ccb-split result)
               (declare (ignore canonical briefs))
               (and valid-p (not (%ccb-tool-result-p (first retained)))))))

(let* ((canonical (ccb-message "system" (make-string 1000 :initial-element #\s)))
       (briefs (loop for i from 1 to 25
                     collect (ccb-message
                              "system"
                              (format nil "[Summary of earlier context: ~a]"
                                      (make-string 1000 :initial-element
                                                   (code-char (+ 65 (mod i 20))))))))
       (large-tool (ccb-message "tool" (make-string 20000 :initial-element #\t)))
       (body (append (loop for i from 1 to 30
                           collect (ccb-message (if (oddp i) "user" "assistant")
                                                (format nil "old ~d" i)))
                     (list (ccb-message "user" "current request")
                           (ccb-message "assistant" "working"
                                        (vector (obj "id" "call-current")))
                           large-tool
                           (ccb-message "assistant" "current answer"))))
       (result (conversation-context-budget-manage
                (append (list canonical) briefs body) #'identity
                (lambda (messages) (declare (ignore messages))
                  (error "existing brief must avoid summarizer")))))
  (ccb-check "enforced prompt reaches target estimated-token budget"
             (<= (%ccb-message-estimated-tokens result)
                 *conversation-context-target-tokens*))
  (ccb-check "current user exchange is retained intact"
             (find "current request" result :key #'%ccb-content :test #'string=))
  (ccb-check "immediately preceding assistant record is retained"
             (find "old 30" result :key #'%ccb-content :test #'string=))
  (ccb-check "oversized current tool result is compacted"
             (let ((tool (find "tool" result
                               :key (lambda (message) (gethash "role" message))
                               :test #'string=)))
               (and tool
                    (<= (length (%ccb-content tool))
                        *conversation-context-tool-result-chars*)
                    (search "compacted" (%ccb-content tool)))))
  (multiple-value-bind (ignored briefs-in-result ignored-body valid-p)
      (%ccb-split result)
    (declare (ignore ignored ignored-body))
    (ccb-check "legacy continuity stack collapses to one bounded brief"
               (and valid-p (= 1 (length briefs-in-result))
                    (<= (length (%ccb-content (first briefs-in-result)))
                        (+ *conversation-context-brief-chars* 32))))))

(let* ((bad (list (ccb-message "system" "identity-a")
                  (ccb-message "system" "identity-b")
                  (ccb-message "user" "hello")))
       (refused
         (handler-case
             (progn
               (conversation-context-budget-manage
                bad #'identity
                (lambda (messages)
                  (declare (ignore messages))
                  (error "must not run")))
               nil)
           (error () t))))
  (ccb-check "ambiguous identity shape is refused in enforced mode" refused)
  (ccb-check "fallback reason is observable"
             (string= "invalid-leading-system-shape"
                      (gethash "fallback_reason"
                               (conversation-context-budget-report)))))

(let* ((canonical (ccb-message "system" "identity"))
       (damaged
         (ccb-message
          "system"
          (format nil
                  "[Continuity brief:~%[Older continuity omitted; original characters: 4004]~%[Continuity brief:~%unrelated research fragment]]]]")))
       (body (list (ccb-message "user" "Want to play a word game?")
                   (ccb-message "assistant" "Yes.")
                   (ccb-message "user" "blonde")))
       (result
         (conversation-context-budget-manage
          (append (list canonical damaged) body) #'identity
          (lambda (messages)
            (declare (ignore messages))
            (error "damaged legacy brief must not trigger summarization"))))
       (system (first result)))
  (ccb-check "damaged recursive continuity is removed from projection"
             (and (= 1 (count "system" result
                              :key (lambda (message)
                                     (gethash "role" message ""))
                              :test #'string=))
                  (null (search "unrelated research fragment"
                                (%ccb-content system)))
                  (null (search "Older continuity omitted"
                                (%ccb-content system)))
                  (null (search "[Continuity brief:"
                                (%ccb-content system)))))
  (ccb-check "recent coherent dialogue survives damaged-brief removal"
             (string= "blonde" (%ccb-content (car (last result))))))

(let* ((canonical (ccb-message "system" "identity"))
       (brief (ccb-message "system"
                           "[Summary of earlier context: A clean decision remains open.]"))
       (body (list (ccb-message "user" "continue")
                   (ccb-message "assistant" "okay")))
       (first
         (conversation-context-budget-manage
          (append (list canonical brief) body) #'identity
          (lambda (messages) (declare (ignore messages))
            (error "no compaction required"))))
       (second
         (conversation-context-budget-manage
          first #'identity
          (lambda (messages) (declare (ignore messages))
            (error "idempotent context must not summarize"))))
       (first-json (shasht:write-json (coerce first 'vector) nil))
       (second-json (shasht:write-json (coerce second 'vector) nil)))
  (ccb-check "clean continuity is embedded in the sole system message"
             (and (= 1 (count "system" first
                              :key (lambda (message)
                                     (gethash "role" message ""))
                              :test #'string=))
                  (= 1 (%ccb-count-substring
                        *conversation-context-continuity-begin*
                        (%ccb-content (first first))))
                  (search "A clean decision remains open."
                          (%ccb-content (first first)))))
  (ccb-check "context management is byte-idempotent"
             (string= first-json second-json)))

(let* ((canonical (ccb-message "system" "identity"))
       (older (loop for i from 1 to 59
                    collect (ccb-message (if (oddp i) "user" "assistant")
                                         (format nil "small turn ~d" i))))
       (current (list (ccb-message "user" "update the document")
                      (ccb-message "assistant" "" (vector (obj "id" "edit-1")))
                      (ccb-message "tool" "file updated")
                      (ccb-message "assistant" "continuing the same task")))
       (summary-calls 0)
       (first
         (conversation-context-budget-manage
          (append (list canonical) older current) #'identity
          (lambda (messages)
            (declare (ignore messages))
            (incf summary-calls)
            (error "small record growth must not summarize"))))
       (second
         (conversation-context-budget-manage
          (append first
                  (list (ccb-message "assistant" "" (vector (obj "id" "edit-2")))
                        (ccb-message "tool" "second update complete")))
          #'identity
          (lambda (messages)
            (declare (ignore messages))
            (incf summary-calls)
            (error "tool recursion below hard pressure must not summarize")))))
  (ccb-check "63 small body records do not trigger semantic compaction"
             (zerop summary-calls))
  (ccb-check "soft-record hysteresis retains every small record"
             (multiple-value-bind (ignored ignored-briefs retained valid-p)
                 (%ccb-split first)
               (declare (ignore ignored ignored-briefs))
               (and valid-p (= 63 (length retained)))))
  (ccb-check "post-tool recursion retains both completed tool results"
             (and (find "file updated" second :key #'%ccb-content :test #'string=)
                  (find "second update complete" second
                        :key #'%ccb-content :test #'string=)))
  (ccb-check "small-record report has no compaction pressure"
             (eq :null (gethash "compaction_pressure"
                                (conversation-context-budget-report)))))

(let* ((canonical (ccb-message "system" "identity"))
       (body (loop for i from 1 to 101
                   collect (ccb-message (if (oddp i) "user" "assistant")
                                        (format nil "substantive turn ~d" i))))
       (summary-calls 0)
       (purpose nil)
       (result
         (conversation-context-budget-manage
          (append (list canonical) body) #'identity
          (lambda (messages)
            (incf summary-calls)
            (setf purpose *timing-model-purpose*)
            (ccb-check "summarizer receives displaced complete records"
                       (= 41 (length messages)))
             (ccb-message
             "system"
             "[Summary of earlier context: the operator and the agent established one coherent earlier topic.]"))))
       (pressure (gethash "compaction_pressure"
                          (conversation-context-budget-report)))
       (after-tool
         (conversation-context-budget-manage
          (append result
                  (list (ccb-message "assistant" "" (vector (obj "id" "call-after")))
                        (ccb-message "tool" "post-compaction result")))
          #'identity
          (lambda (messages)
            (declare (ignore messages))
            (incf summary-calls)
            (error "hysteresis must prevent immediate resummarization")))))
  (ccb-check "hard-record pressure summarizes exactly once"
             (= 1 summary-calls))
  (ccb-check "semantic summary is typed as internal housekeeping"
             (string= "context-summary" purpose))
  (ccb-check "tool recursion after compaction does not immediately summarize"
             (find "post-compaction result" after-tool
                   :key #'%ccb-content :test #'string=))
  (ccb-check "compacted request still has exactly one system message"
             (= 1 (count "system" result
                         :key (lambda (message) (gethash "role" message ""))
                         :test #'string=)))
  (ccb-check "coherent summary is embedded rather than added as system two"
             (and (search "the operator and the agent established one coherent earlier topic."
                          (%ccb-content (first result)))
                  (= 1 (%ccb-count-substring
                        *conversation-context-continuity-begin*
                        (%ccb-content (first result))))))
  (ccb-check "hard-record pressure is explicit in report"
             (string= "hard-records" pressure)))

(let* ((config-path
         (pathname (format nil "/tmp/pai-context-config-~d.json"
                           (get-universal-time))))
       (*conversation-context-config-file* config-path)
       (valid (obj "target_records" 72 "minimum_recent_records" 30
                   "hard_records" 120 "target_estimated_tokens" 120000
                   "hard_estimated_tokens" 180000 "target_chars" 480000
                   "hard_chars" 720000 "brief_chars" 5000
                   "tool_result_chars" 7000)))
  (conversation-context-budget-update valid :actor "deterministic-test")
  (ccb-check "validated runtime config is applied live"
             (let ((report (conversation-context-budget-config-report)))
               (and (= 72 (gethash "target_records" report))
                    (= 120000 (gethash "target_estimated_tokens" report))
                    (= 30 (gethash "minimum_recent_records" report)))))
  (ccb-check "validated runtime config is persisted"
             (and (probe-file config-path)
                  (= 180000
                     (gethash "hard_estimated_tokens"
                              (shasht:read-json
                               (uiop:read-file-string config-path))))))
  (let ((before (conversation-context-budget-config-report)))
    (ccb-check
     "invalid cross-field update fails without changing live config"
     (and (handler-case
              (progn
                (conversation-context-budget-update
                 (obj "target_records" 72 "minimum_recent_records" 30
                      "hard_records" 20 "target_estimated_tokens" 120000
                      "hard_estimated_tokens" 180000 "target_chars" 480000
                      "hard_chars" 720000 "brief_chars" 5000
                      "tool_result_chars" 7000))
                nil)
            (error () t))
          (= (gethash "hard_records" before)
             (gethash "hard_records"
                      (conversation-context-budget-config-report))))))
  ;; Prove the persisted object is boot-loadable after deliberately changing
  ;; the live bindings away from it.
  (setf *conversation-context-target-records* 60
        *conversation-context-target-tokens* 100000)
  (load-conversation-context-budget-config)
  (ccb-check "persisted runtime config reloads across process starts"
             (and (= 72 *conversation-context-target-records*)
                  (= 120000 *conversation-context-target-tokens*))))

(format t "~%CONVERSATION CONTEXT BUDGET: ~d passed, ~d failed.~%"
        *ccb-pass* *ccb-fail*)
(when (plusp *ccb-fail*) (uiop:quit 1))
