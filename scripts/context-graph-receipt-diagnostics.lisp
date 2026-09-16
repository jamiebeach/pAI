;;;; Receipt-only diagnostics for private laboratory cases. No graph mutation.
(in-package :agent)

(defun context-graph-receipt-diagnostics (case)
  "Expose recorded decisions and exact quote provenance; never infer admission."
  (let ((rows nil))
    (loop for attempt across (gethash "attempts" case) do
      (let* ((opening-event (gethash "event" (gethash "opening" attempt)))
             (opening (pai.context-graph::%cgro-record opening-event))
             (sources (gethash "sources" (gethash "source_packet"
                                                  (gethash "source_context" opening))))
             (facts #()) (reviews nil))
        (loop for receipt across (gethash "receipts" attempt)
              for event = (gethash "event" receipt)
              for record = (pai.context-graph::%cgro-record event)
              when (equal "response" (gethash "outcome" record)) do
                (cond ((equal "facts" (gethash "phase" record))
                       (setf facts (gethash "facts" (gethash "response" record) #())))
                      ((equal "review" (gethash "phase" record))
                       (setf reviews (gethash "claim_reviews" (gethash "response" record))))))
        (loop for fact across facts for ordinal from 0
              for ref = (format nil "relationship:~d" ordinal)
              for review = (and reviews (gethash ref reviews)) do
          (push
           (obj "opening_id" (gethash "id" opening-event)
                "episode_event_id" (gethash "episode_event_id" opening)
                "batch_index" (gethash "batch_index" opening)
                "claim_ref" ref "proposal" fact "review" (or review :null)
                "evidence_quote_matches"
                (map 'vector
                     (lambda (citation)
                       (let ((quote (gethash "quote" citation)))
                         (obj "citation" citation
                              "source_matches"
                              (coerce
                               (loop for source across sources
                                     when (and (stringp quote) (plusp (length quote))
                                               (search quote (gethash "text" source "")))
                                     collect
                                     (obj "source_id" (gethash "source_id" source)
                                          "kind" (gethash "kind" source)
                                          "identity" (gethash "identity" source)
                                          "text_sha256" (gethash "text_sha256" source)))
                               'vector))))
                     (gethash "evidence" fact #()))
                "admission" "unknown-requires-production-replay"
                "query_eligibility" "unknown-requires-production-replay")
           rows))))
    (obj "schema_version" 1 "mode" "recorded-receipt-inspection"
         "contract" (gethash "contract" case) "claims" (coerce (nreverse rows) 'vector)
         "provider_calls" 0 "database_write_count" 0)))
