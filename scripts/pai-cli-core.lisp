;;;; pai-cli-core.lisp -- pure command grammar for the canonical operator CLI.

(in-package :agent)

(export '(pai-cli-parse-input pai-cli-run-lifecycle pai-cli-help-text))

(defparameter *pai-cli-command-text-limit* 4096)

(defun %pai-cli-bounded-text (value field &key (allow-empty nil))
  (let ((text (and (stringp value)
                   (string-trim '(#\Space #\Tab #\Newline #\Return) value))))
    (unless (and text
                 (or allow-empty (plusp (length text)))
                 (<= (length text) *pai-cli-command-text-limit*))
      (error "pAI CLI ~a must be bounded text" field))
    text))

(defun %pai-cli-command-and-rest (line)
  (let* ((text (%pai-cli-bounded-text line "input"))
         (space (position-if (lambda (character)
                               (member character '(#\Space #\Tab)))
                             text)))
    (values (string-downcase (if space (subseq text 0 space) text))
            (if space
                (string-trim '(#\Space #\Tab) (subseq text (1+ space)))
                ""))))

(defun %pai-cli-no-arguments (command rest kind)
  (unless (zerop (length rest))
    (error "~a takes no arguments" command))
  (values kind nil))

(defun pai-cli-parse-input (line)
  "Classify one operator line without performing any action."
  (let ((text (%pai-cli-bounded-text line "input")))
    (unless (char= (char text 0) #\/)
      (return-from pai-cli-parse-input (values :chat text)))
    (multiple-value-bind (command rest) (%pai-cli-command-and-rest text)
      (cond
        ((member command '("/quit" "/exit") :test #'string=)
         (%pai-cli-no-arguments command rest :quit))
        ((string= command "/help")
         (%pai-cli-no-arguments command rest :help))
        ((string= command "/lifecycle-inspect")
         (%pai-cli-no-arguments command rest :lifecycle-inspect))
        ((string= command "/lifecycle-cancel")
         (%pai-cli-no-arguments command rest :lifecycle-cancel))
        ((string= command "/curiosity-inspect")
         (%pai-cli-no-arguments command rest :curiosity-inspect))
        ((string= command "/affect-inspect")
         (%pai-cli-no-arguments command rest :affect-inspect))
        ((string= command "/graph-inspect")
         (%pai-cli-no-arguments command rest :graph-inspect))
        ((string= command "/config")
         (%pai-cli-no-arguments command rest :config-inspect))
        ((string= command "/config-set")
         (let ((space (position-if (lambda (character)
                                     (member character '(#\Space #\Tab)))
                                   rest)))
           (unless space (error "/config-set requires KEY VALUE"))
           (values :config-set
                   (obj "key" (%pai-cli-bounded-text (subseq rest 0 space)
                                                     "setting key")
                        "value_text" (%pai-cli-bounded-text
                                      (subseq rest (1+ space))
                                      "setting value")))))
        ((string= command "/graph-search")
         (values :graph-search
                 (obj "query" (%pai-cli-bounded-text rest "graph query"))))
        ((string= command "/memory-inspect")
         (values :memory-inspect
                 (obj "query" (%pai-cli-bounded-text rest "memory query"))))
        ((string= command "/lifecycle-create")
         (let ((separator (position #\| rest)))
           (unless separator
             (error "/lifecycle-create requires SUBJECT | AIM"))
           (values
            :lifecycle-create
            (obj "subject" (%pai-cli-bounded-text
                            (subseq rest 0 separator) "lifecycle subject")
                 "aim" (%pai-cli-bounded-text
                        (subseq rest (1+ separator)) "lifecycle aim")))))
        ((string= command "/lifecycle-ready")
         (values :lifecycle-ready
                 (obj "result_summary" (%pai-cli-bounded-text
                                        rest "lifecycle result summary"))))
        ((string= command "/lifecycle-complete")
         (values :lifecycle-complete
                 (obj "observed_reply" (%pai-cli-bounded-text
                                        rest "observed public reply"))))
        (t (error "Unknown pAI command ~a; use /help" command))))))

(defun pai-cli-help-text ()
  (format nil
          "Commands:~%  /help~%  /config~%  /config-set KEY VALUE~%  /memory-inspect QUERY~%  /curiosity-inspect~%  /affect-inspect~%  /graph-inspect~%  /graph-search QUERY~%  /lifecycle-create SUBJECT | AIM~%  /lifecycle-inspect~%  /lifecycle-ready RESULT-SUMMARY~%  /lifecycle-complete OBSERVED REPLY~%  /lifecycle-cancel~%  /quit"))

(defun pai-cli-run-lifecycle (kind payload runner)
  "Map one parsed command to the providerless lifecycle runner seam."
  (unless (functionp runner) (error "pAI CLI lifecycle runner is absent"))
  (case kind
    (:lifecycle-create
     (funcall runner "create" :subject (gethash "subject" payload)
                            :aim (gethash "aim" payload)))
    (:lifecycle-inspect (funcall runner "inspect"))
    (:lifecycle-ready
     (funcall runner "ready"
              :result-summary (gethash "result_summary" payload)))
    (:lifecycle-complete
     (funcall runner "complete"
              :observed-reply (gethash "observed_reply" payload)))
    (:lifecycle-cancel (funcall runner "cancel"))
    (otherwise (error "Unsupported lifecycle command ~s" kind))))
