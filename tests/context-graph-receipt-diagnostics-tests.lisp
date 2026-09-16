;;;; harness: full-system
(in-package :agent)
(load (asdf:system-relative-pathname :pai "scripts/context-graph-receipt-diagnostics.lisp"))

(defun cgrd-test-row (id record)
  (obj "event" (obj "id" id "payload"
                    (obj "record_json" (shasht:write-json record nil)))))

(let* ((quote "The remembered child is fourteen.")
       (source (obj "source_id" "fixture:prior" "kind" "prior-agent-utterance"
                    "identity" (obj "role" "other") "text" quote
                    "text_sha256" "fixture-digest"))
       (opening (cgrd-test-row
                 10 (obj "episode_event_id" 1 "batch_index" 0
                         "source_context" (obj "source_packet"
                                               (obj "sources" (vector source))))))
       (fact (obj "predicate" "has_age"
                  "evidence" (vector (obj "quote" quote))))
       (review (obj "verdict" "DIRECTLY_EVIDENCED" "source_reading" "assertion"))
       (case (obj "contract" (obj "generation" "fixture")
                  "attempts"
                  (vector (obj "opening" opening "receipts"
                               (vector
                                (cgrd-test-row 11 (obj "outcome" "response" "phase" "facts"
                                                     "response" (obj "facts" (vector fact))))
                                (cgrd-test-row 12 (obj "outcome" "response" "phase" "review"
                                                     "response" (obj "claim_reviews"
                                                                      (obj "relationship:0" review)))))))))
       (result (context-graph-receipt-diagnostics case))
       (claim (aref (gethash "claims" result) 0))
       (match (aref (gethash "source_matches"
                             (aref (gethash "evidence_quote_matches" claim) 0)) 0)))
  (assert (= 1 (length (gethash "claims" result))))
  (assert (equal "prior-agent-utterance" (gethash "kind" match)))
  (assert (equal "assertion" (gethash "source_reading" (gethash "review" claim))))
  (assert (equal "unknown-requires-production-replay" (gethash "admission" claim)))
  (assert (= 0 (gethash "provider_calls" result)))
  (assert (= 0 (gethash "database_write_count" result))))
(format t "RECEIPT-DIAGNOSTICS provenance and unknown-stage assertions passed~%")
(format t "PASS context-graph-receipt-diagnostics-tests~%")
