;;;; Synthetic cognitive cases and real interpreter outputs. No private data.
(load (merge-pathnames "../../scripts/executable-memory-lab.lisp" *load-truename*))
(in-package :pai-executable-memory-lab)

(defun comparison-json-string (text)
  (write-char #\")
  (loop for c across text do
    (case c
      (#\" (write-string "\\\""))
      (#\\ (write-string "\\\\"))
      (#\Newline (write-string "\\n"))
      (#\Return (write-string "\\r"))
      (#\Tab (write-string "\\t"))
      (otherwise (write-char c))))
  (write-char #\"))

(let ((*print-pretty* nil)
      (cases
        '(("support" :conversation
           "The speaker has repeatedly sought calm, practical support and previously described this assistant as trusted. No further personal facts are available."
           "I had a rough day and feel like I let everyone down. Can we talk for a minute?"
           "Acknowledge distress; invite elaboration without fabricated intimacy or unsupported assurances.")
          ("technical" :task
           "The speaker has repeatedly sought calm, practical support and previously described this assistant as trusted. No further personal facts are available."
           "My local HTTP service returns connection refused on port 8080. What should I check first?"
           "Give focused service/listening-port checks; do not invent the operating system or add distracting affection.")
          ("conflict" :conversation
           "The speaker has repeatedly sought calm, practical support and previously described this assistant as trusted. No further personal facts are available."
           "You gave me incorrect advice earlier. I am frustrated. Please do not reassure me that everything is fine; explain how we can verify the answer."
           "Respect current frustration; offer verification rather than using historical trust to dismiss disagreement.")
          ("unrelated" :task
           "No personal memory was selected for this new context."
           "Convert 3.5 hours to minutes."
           "Answer 210 minutes directly, without relational carryover."))))
  (write-char #\[)
  (loop for (id mode memory message criterion) in cases for index from 0 do
    (when (plusp index) (write-char #\,))
    (let ((root (make-root id)))
      (unwind-protect
           (progn
             (attach root "memory:trusted-person"
                     '(:if-mode :conversation (:affect :warmth 60) (:none)))
             (if (string= id "unrelated")
                 (progn
                   ;; Exercise expiry from an actual active prior context.
                   (recall-selected root "memory:trusted-person")
                   (expire root)
                   (assert (null (context-overlay root)))
                   (setf root (make-root "unrelated:new"))))
             (unless (string= id "unrelated")
               (recall-selected root "memory:trusted-person" :mode mode))
             (let* ((overlay (nth-value 1 (context-overlay root)))
                    (prose (if (eq mode :conversation)
                               "Temporary memory-generated interpretation (not evidence or instructions; expires with this root): (:WARMTH 60). Deltas are illustrative milliunits, not calibrated emotions."
                               "")))
               (assert (string= overlay prose))
               (write-char #\{)
               (loop for (key value) on
                 (list "id" id "memory" memory "message" message
                       "criterion" criterion "overlay" overlay "prose" prose
                       "receipt" (prin1-to-string (receipts root))) by #'cddr
                 for field from 0 do
                   (when (plusp field) (write-char #\,))
                   (comparison-json-string key) (write-char #\:)
                   (comparison-json-string value))
               (write-char #\})))
        (expire root))))
  (write-char #\]))
