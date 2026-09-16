;;;; harness: bare
(require :asdf)
(unless (find-package :ql)
  (load (or (uiop:getenv "PAI_QUICKLISP_SETUP") "/opt/quicklisp/setup.lisp")))
(asdf:load-asd (merge-pathnames "../pai-context-graph.asd" *load-truename*))
(asdf:load-system :pai-context-graph)
(in-package :pai.context-graph)

(defvar *span-checks* 0)
(defun span-check (name value)
  (unless value (error "FAIL ~a" name))
  (incf *span-checks*) (format t "PASS ~a~%" name))
(defun span-source (text)
  (%cg-object "source_id" "source:fixture" "speaker_id" "principal:owner"
              "kind" "original-utterance" "timestamp" 100
              "text" text "text_sha256" (%cg-sha256 text)
              "identity" (%cg-object "principal_id" "principal:owner"
                                      "binding_id" "binding:owner"
                                      "conversation_id" "conversation:fixture"
                                      "role" "operator")
              "resource_ref" (%cg-object "store" "event" "resource_id" "event:fixture"
                                          "version_id" "version:one" "component" "content")))
(defun span-test (source quote expected &optional method)
  (let* ((row (span-source source))
         (before (shasht:write-json (%cg-canonical-tree row) nil))
         (result (context-graph-resolve-source-span row quote))
         (value (gethash "value" result)))
    (span-check (format nil "~s -> ~a" quote expected)
                (equal expected (gethash "status" result)))
    (span-check "resolver leaves input unchanged"
                (equal before (shasht:write-json (%cg-canonical-tree row) nil)))
    (when (equal expected "accepted")
      (span-check "exact original character interval"
                  (equal (gethash "quote" value)
                         (subseq source (gethash "start_char" value) (gethash "end_char" value))))
      (when method (span-check "expected resolver method" (equal method (gethash "method" value))))
      (setf (gethash "role" (gethash "identity" value)) "changed")
      (span-check "returned provenance is detached"
                  (equal "operator" (gethash "role" (gethash "identity" row)))))
    result))

(span-test "Literal ... punctuation." "Literal ... punctuation." "accepted" "exact")
(let ((r (span-test "same same" "same" "accepted" "exact")))
  (span-check "repeated exact quote records leftmost occurrence"
              (= 0 (gethash "start_char" (gethash "value" r)))))
(span-test "I use **a notebook** every day." "i use a notebook" "accepted" "presentation")
(span-test (format nil "é I~%  use `a` notebook.") "I use a notebook" "accepted" "presentation")
(span-test "I use tea. I use tea." "i use tea" "rejected")
(span-test "I use a notebook on Monday." "I ... notebook ... Monday." "accepted" "ellipsis")
(span-test "I use a notebook on Monday." "I … notebook … Monday." "accepted" "ellipsis")
(span-test "I first stopped. I then used the notebook." "I then ... notebook." "accepted" "ellipsis")
(span-test "A x A y Z" "A ... Z" "rejected")
(span-test "A x B x B x Z" "A ... B ... Z" "accepted" "ellipsis")
(span-test "A x B x Z x Z" "A ... B ... Z" "rejected")
(span-test "B then A" "A ... B" "rejected")
(span-test "A uses B" "A ... owns B" "rejected")
(dolist (quote '("... A" "A ..." "A .... B" "A ...... B" "A …... B"
                 "A ... … B" "A . . . B" "A [...] B" "A ... !!! ... B"
                 "A ... B ... C ... D ... E ... F ... G" "" "   "))
  (span-test "A B C D E F G" quote "rejected"))
(let* ((r (span-test "I do not take supplement X." "I ... take supplement X." "accepted" "ellipsis"))
       (v (gethash "value" r)))
  (span-check "omitted negation reaches review unchanged"
              (equal "I do not take supplement X." (gethash "quote" v))))
(span-test (concatenate 'string "A" (make-string 512 :initial-element #\x) "B")
           "A ... B" "accepted" "ellipsis")
(span-test (concatenate 'string "A" (make-string 513 :initial-element #\x) "B")
           "A ... B" "rejected")
(span-test (make-string 30001 :initial-element #\a) "a" "incomplete")
(span-test (concatenate 'string (make-string 29999 :initial-element #\a) "Z") "Z" "accepted" "exact")
(span-test (make-string 1000 :initial-element #\a) (make-string 1000 :initial-element #\a) "accepted" "exact")
(span-test (concatenate 'string (make-string 128 :initial-element #\a) (make-string 128 :initial-element #\b) "C")
           "a ... b ... a ... C" "incomplete")
(span-test (make-string 1001 :initial-element #\a)
           (make-string 1001 :initial-element #\a) "incomplete")
(span-test (format nil "~{~a~} Z" (make-list 129 :initial-element "a "))
           "a ... Z" "incomplete")
(let ((source (span-source "Original")))
  (setf (gethash "text" source) "Changed")
  (span-check "forged source digest is a typed input failure"
              (handler-case (progn (context-graph-resolve-source-span source "Changed") nil)
                (context-graph-authority-input-error () t))))
(let ((source (span-source "Original")))
  (setf (gethash "unexpected" source) t)
  (span-check "source unknown keys fail closed"
              (handler-case (progn (context-graph-resolve-source-span source "Original") nil)
                (context-graph-authority-input-error () t))))
(span-check "legacy compatibility cannot activate elision under old generation"
            (null (context-graph-resolve-legacy-source-quote "A x B" "A ... B")))
(span-check "legacy adapter retains exact presentation recovery"
            (equal "I use **tea" (context-graph-resolve-legacy-source-quote "I use **tea**." "i use tea")))
(format t "AUTHORITY-SPAN ~d passed, 0 failed~%" *span-checks*)
