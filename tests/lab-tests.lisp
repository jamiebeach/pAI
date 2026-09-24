(in-package :agent)

(ql:quickload '(:bordeaux-threads :dexador :postmodern :shasht) :silent t)

(defvar *pai-lab-test-pass* 0)
(defvar *pai-lab-test-fail* 0)
(defvar *pai-lab-test-transactions* 0)
(defvar *pai-lab-test-searches* 0)
(defvar *pai-lab-test-write-probes* 0)
(defvar *pai-lab-test-as-of* nil)
(defvar *pai-lab-test-atom-write-probes* 0)

(defun pai-lab-test-check (name condition)
  (if condition
      (progn (incf *pai-lab-test-pass*) (format t "PASS ~a~%" name))
      (progn (incf *pai-lab-test-fail*) (format t "FAIL ~a~%" name))))

(load (test-source "memory-nodes.lisp"))

(defun memory-search (&rest args)
  (declare (ignore args)) nil)
(defun %context-projection-eligible-memory-p (row)
  (and (string= "grounded" (gethash "grounding_status" row ""))
       (not (gethash "quarantined" row))))
(defun %context-projection-sensitive-memory-content-p (row)
  (search "# operational constitution"
          (string-downcase (gethash "content" row ""))))
(defun %context-projection-public-memory-score (row)
  (gethash "similarity" row 0.0d0))
(defparameter *context-projection-max-shared-memory-results* 3)
(defparameter *context-projection-memory-similarity-floor* 0.52d0)
(defparameter *context-projection-memory-best-window* 0.22d0)
(defun %context-projection-direct-memory-p (row)
  (and (string= "grounded" (gethash "grounding_status" row ""))
       (member (gethash "origin_class" row "")
               '("lived-user" "lived-agent-action" "tool-result"
                 "external-source")
               :test #'string=)))
(defun %context-projection-select-shared-memory (rows)
  (let ((selected
          (remove-if (lambda (row)
                       (< (gethash "similarity" row 0.0d0) 0.52d0))
                     (stable-sort (copy-list rows) #'>
                                  :key #'%context-projection-public-memory-score))))
    (subseq selected 0 (min (length selected)
                            *context-projection-max-shared-memory-results*))))

(load (test-source "context-curator-candidate.lisp"))
(load (test-source "turn-bundle-retrieval.lisp"))
(load (test-source "memory-atom-candidate.lisp"))
(load (test-source "lab.lisp"))

(pai-lab-test-check
 "strict embedding policy refuses silent fallback"
 (let ((*embedding-fallback-policy* :error))
   (handler-case (progn (%embedding-fallback "query" "fixture outage") nil)
     (error () t))))
(pai-lab-test-check
 "production-compatible embedding policy retains bounded fallback"
 (let ((*embedding-fallback-policy* :allow))
   (= 768 (length (%embedding-fallback "query" "fixture outage")))))
(pai-lab-test-check
 "embedding input is bounded by encoded bytes"
 (let* ((*ollama-embedding-input-byte-limit* 8)
        (bounded (%embedding-bounded-text "abcdefghijkl")))
   (and (string= "abcdefgh" bounded)
        (= 8 (length (babel:string-to-octets bounded :encoding :utf-8))))))
(pai-lab-test-check
 "embedding byte bound never splits a multibyte character"
 (let* ((*ollama-embedding-input-byte-limit* 5)
        (bounded (%embedding-bounded-text "éééé")))
   (and (string= "éé" bounded)
        (= 4 (length (babel:string-to-octets bounded :encoding :utf-8))))))
(pai-lab-test-check
 "embedding boundary preserves short text and rejects invalid limits"
 (and (let* ((*ollama-embedding-input-byte-limit* 8)
             (text "éé"))
        (eq text (%embedding-bounded-text text)))
      (let ((*ollama-embedding-input-byte-limit* 0))
        (handler-case (progn (%embedding-bounded-text "abc") nil)
          (error () t)))))
(pai-lab-test-check
 "batch task prefix is counted inside the embedding byte envelope"
 (let* ((*ollama-embedding-input-byte-limit* 19)
        (bounded (%embedding-bounded-text "search_document: éé")))
   (and (string= "search_document: é" bounded)
        (= 19 (length (babel:string-to-octets bounded :encoding :utf-8))))))

;;; --- embedding retry, backoff, and circuit breaker ----------------------

(defmacro with-fixture-embed-once ((responses-var) &body body)
  "Replace %EMBED-TEXT-ONCE with a fixture returning the successive results
in RESPONSES-VAR (each either a vector, for success, or a reason string, for
failure), then restore the real definition."
  (let ((orig (gensym)) (queue (gensym)))
    `(let* ((,orig (fdefinition '%embed-text-once))
            (,queue ,responses-var))
       (unwind-protect
            (progn
              (setf (fdefinition '%embed-text-once)
                    (lambda (text)
                      (declare (ignore text))
                      (let ((next (pop ,queue)))
                        (if (and (vectorp next) (not (stringp next)))
                            (values (coerce next 'list) nil)
                            (values nil (or next "fixture failure"))))))
              ,@body)
         (setf (fdefinition '%embed-text-once) ,orig)))))

(defmacro with-reset-embedding-circuit (&body body)
  `(let ((*embedding-consecutive-failures* 0)
         (*embedding-circuit-open-until* 0)
         (*embedding-degraded-announced-p* nil)
         (*embedding-fallback-policy* :allow)
         (*ollama-embed-attempts* 3)
         (*ollama-embed-retry-backoff-seconds* 0.0d0))
     ,@body))

(with-reset-embedding-circuit
  (with-fixture-embed-once ((list "first attempt down" #(0.5d0 0.25d0)))
    (pai-lab-test-check
     "embed-text retries once and returns the live vector on the second try"
     (equal '(0.5d0 0.25d0) (embed-text "hello")))
    (pai-lab-test-check
     "a recovered call clears the consecutive-failure count"
     (zerop *embedding-consecutive-failures*))))

(with-reset-embedding-circuit
  (with-fixture-embed-once ((list "down" "down" "down"))
    (let ((result (embed-text "hello")))
      (pai-lab-test-check
       "exhausting every attempt still returns a usable fallback vector"
       (and (listp result) (= 768 (length result))))))
  (pai-lab-test-check
   "exhausting every attempt records one failure toward the circuit"
   (= 1 *embedding-consecutive-failures*)))

(with-reset-embedding-circuit
  (let ((announcements nil))
    (let ((orig (fdefinition '%embedding-announce)))
      (unwind-protect
           (progn
             (setf (fdefinition '%embedding-announce)
                   (lambda (state reason) (push (cons state reason) announcements)))
             (with-fixture-embed-once ((list "down" "down" "down"))
               (embed-text "one"))
             (with-fixture-embed-once ((list "down" "down" "down"))
               (embed-text "two"))
             (with-fixture-embed-once ((list "down" "down" "down"))
               (embed-text "three")))
        (setf (fdefinition '%embedding-announce) orig)))
    (pai-lab-test-check
     "three consecutive fully-failed calls announce degraded exactly once"
     (= 1 (count "degraded" announcements :key #'car :test #'string=)))
    (pai-lab-test-check
     "the third failure opens the circuit for the cooldown window"
     (> *embedding-circuit-open-until* (get-universal-time)))))

(with-reset-embedding-circuit
  (setf *embedding-circuit-open-until* (+ (get-universal-time) 60))
  (with-fixture-embed-once ((list #(1.0d0)))
    (pai-lab-test-check
     "an open circuit short-circuits to the fallback without a live attempt"
     (let ((result (embed-text "hello")))
       (and (listp result) (= 768 (length result))
            (not (equal result '(1.0d0))))))))

(with-reset-embedding-circuit
  (let ((announcements nil))
    (setf *embedding-consecutive-failures* *embedding-degraded-threshold*
          *embedding-degraded-announced-p* t)
    (let ((orig (fdefinition '%embedding-announce)))
      (unwind-protect
           (progn
             (setf (fdefinition '%embedding-announce)
                   (lambda (state reason) (push (cons state reason) announcements)))
             (with-fixture-embed-once ((list #(1.0d0)))
               (embed-text "recovered")))
        (setf (fdefinition '%embedding-announce) orig)))
    (pai-lab-test-check
     "a live success after a degraded run announces recovery"
     (find "recovered" announcements :key #'car :test #'string=))
    (pai-lab-test-check
     "recovery clears the degraded-announced latch"
     (not *embedding-degraded-announced-p*))))

(with-reset-embedding-circuit
  (let ((batches nil))
    (let ((orig (fdefinition '%embed-batch-once)))
      (unwind-protect
           (progn
             (setf (fdefinition '%embed-batch-once)
                   (lambda (batch)
                     (push batch batches)
                     (if (<= (length batches) 1)
                         (error "batch endpoint unavailable")
                         (mapcar (lambda (text)
                                   (declare (ignore text))
                                   (list 0.1d0 0.2d0))
                                 batch))))
             (pai-lab-test-check
              "batch retrieval retries a failed batch before falling back"
              (equal (list (list 0.1d0 0.2d0) (list 0.1d0 0.2d0))
                     (embed-retrieval-documents (list "a" "b")))))
        (setf (fdefinition '%embed-batch-once) orig)))))

(let* ((query "What shared routine was discussed?")
       (rows
         (list
          (obj "id" "query-echo" "content" query "kind" "observation"
               "origin_class" "lived-user" "epistemic_status" "user-report"
               "grounding_status" "grounded" "similarity" 0.99d0)
          (obj "id" "system" "content" "# Operational Constitution private"
               "kind" "observation" "origin_class" "lived-user"
               "epistemic_status" "user-report" "grounding_status" "grounded"
               "similarity" 0.98d0)
          (obj "id" "unsafe" "content" "Generated guess" "kind" "thought"
               "origin_class" "generated-cognition" "epistemic_status" "hypothesis"
               "grounding_status" "unclassified" "similarity" 0.97d0)
          (obj "id" "older-grounded" "content" "A relevant older report."
               "kind" "observation" "origin_class" "lived-user"
               "epistemic_status" "user-report" "grounding_status" "grounded"
               "similarity" 0.82d0 "retrieval_score" 0.71d0)))
       (*pai-lab-read-only-transaction-fn*
         (lambda (thunk)
           (incf *pai-lab-test-transactions*)
           (funcall thunk)))
       (*pai-lab-write-probe-fn*
         (lambda () (incf *pai-lab-test-write-probes*) t))
       (*pai-lab-memory-search-fn*
         (lambda (actual-query &key k mode)
           (incf *pai-lab-test-searches*)
           (pai-lab-test-check "runner binds strict embedding policy"
                                 (eq *embedding-fallback-policy* :error))
           (pai-lab-test-check "runner requests real bounded retrieval"
                                 (and (string= query actual-query)
                                      (= k 50) (eq mode :conversation)))
           rows))
       (scenario
         (obj "id" "fixture" "mode" "read-only-live" "query" query
              "result_limit" 3 "minimum_selected" 1 "maximum_selected" 1
              "expected_selected_ids" (vector "older-grounded")
              "expected_selected_content_patterns"
              (vector (vector "relevant" "older"))
              "minimum_selected_content_matches" 1))
       (suite (obj "schema_version" 1 "suite_id" "fixture-suite"
                   "scenarios" (vector scenario)))
       (result (pai-lab-run-suite suite))
       (run (aref (gethash "results" result) 0))
       (json (shasht:write-json result nil)))
  (pai-lab-test-check "suite runs through one read-only transaction"
                        (and (= 1 *pai-lab-test-transactions*)
                             (= 1 *pai-lab-test-searches*)
                             (= 1 *pai-lab-test-write-probes*)))
  (pai-lab-test-check "eligibility sensitive and echo filters refill"
                        (and (= 1 (gethash "selected_count" run))
                             (string= "older-grounded"
                                      (aref (gethash "selected_ids" run) 0))))
  (pai-lab-test-check "expectations pass on stable selected id"
                        (string= "passed" (gethash "status" run)))
  (pai-lab-test-check
   "test-only semantic oracle emits IDs and counts but no private terms"
   (let ((oracle (gethash "content_oracle" (gethash "expectations" run))))
     (and (gethash "passed" oracle)
          (= 1 (gethash "candidate_match_count" oracle))
          (= 1 (gethash "selected_match_count" oracle))
          (string= "older-grounded" (aref (gethash "matched_ids" oracle) 0))
          (null (search "relevant" (shasht:write-json oracle nil))))))
  (pai-lab-test-check "structured output is content-free"
                        (and (null (search query json))
                             (null (search "A relevant older report" json))
                             (null (search "Operational Constitution" json))))
  (pai-lab-test-check "private content remains redacted by default"
                        (null (gethash "private_content_included" result)))
  (pai-lab-test-check "run structurally reports no effects or authority"
                        (and (gethash "transaction_read_only" run)
                             (gethash "transaction_rolled_back" run)
                             (zerop (gethash "database_write_count" run))
                             (zerop (gethash "provider_call_count" run))
                             (null (gethash "delivery_authority" run)))))

(let* ((*pai-lab-write-probe-fn* (lambda () nil))
       (*pai-lab-read-only-transaction-fn* (lambda (thunk) (funcall thunk)))
       (*pai-lab-memory-search-fn*
         (lambda (&rest arguments) (declare (ignore arguments)) nil))
       (scenario (obj "id" "probe" "mode" "read-only-live"
                      "query" "hello"))
       (suite (obj "schema_version" 1 "suite_id" "probe-suite"
                   "scenarios" (vector scenario)))
       (result (pai-lab-run-suite suite)))
  (pai-lab-test-check
   "suite fails closed when database does not reject write probe"
   (and (string= "failed" (gethash "status" result))
        (null (gethash "database_write_probe_blocked" result))
        (string= "database-write-probe-not-blocked"
                 (gethash "condition_type"
                          (aref (gethash "results" result) 0))))))

(let* ((*pai-lab-reveal-private-content* t)
       (*pai-lab-write-probe-fn* (lambda () t))
       (*pai-lab-read-only-transaction-fn* (lambda (thunk) (funcall thunk)))
       (*pai-lab-memory-search-fn*
         (lambda (&rest arguments)
           (declare (ignore arguments))
           (list (obj "id" "private-row"
                      "content" "Private returned memory text"
                      "kind" "observation" "origin_class" "lived-user"
                      "epistemic_status" "user-report"
                      "grounding_status" "grounded"
                      "created_at" "2026-08-05T10:00:00Z"
                      "similarity" 0.88d0 "retrieval_score" 0.80d0))))
       (scenario (obj "id" "private-review" "mode" "read-only-live"
                      "query" "Private review query"))
       (suite (obj "schema_version" 1 "suite_id" "private-review-suite"
                   "scenarios" (vector scenario)))
       (result (pai-lab-run-suite suite))
       (run (aref (gethash "results" result) 0))
       (json (shasht:write-json result nil)))
  (pai-lab-test-check
   "explicit private review includes query and returned candidate text"
   (and (gethash "private_content_included" result)
        (gethash "private_content_included" run)
        (search "Private review query" json)
        (search "Private returned memory text" json)))
  (pai-lab-test-check
   "explicit private review includes selected records"
   (let ((selected (gethash "selected_records" run)))
     (and (= 1 (length selected))
          (string= "Private returned memory text"
                   (gethash "content" (aref selected 0)))
          (string= "2026-08-05T10:00:00Z"
                   (gethash "observed_at" (aref selected 0)))))))

(let* ((*pai-lab-write-probe-fn* (lambda () t))
       (*pai-lab-read-only-transaction-fn* (lambda (thunk) (funcall thunk)))
       (*pai-lab-memory-search-fn*
         (lambda (query &key k mode as-of)
           (declare (ignore query k mode))
           (setf *pai-lab-test-as-of* as-of)
           (list
            (obj "id" "topical-greeting" "content" "Hello there tonight"
                 "kind" "observation" "origin_class" "lived-user"
                 "epistemic_status" "user-report"
                 "grounding_status" "grounded" "similarity" 0.90d0
                 "retrieval_score" 0.91d0 "importance" 0.4d0
                 "recency_score" 0.9d0 "activation" 0.3d0)
            (obj "id" "answer-bearing" "content" "I chat before sleep"
                 "kind" "observation" "origin_class" "lived-user"
                 "epistemic_status" "user-report"
                 "grounding_status" "grounded" "similarity" 0.60d0
                 "retrieval_score" 0.70d0 "importance" 0.8d0
                 "recency_score" 0.4d0 "activation" 0.2d0))))
       (*pai-lab-embed-text-fn*
         (lambda (text)
           (cond ((search "search_query:" text) '(1.0d0 0.0d0))
                 ((search "I chat before sleep" text) '(1.0d0 0.0d0))
                 (t '(0.0d0 1.0d0)))))
       (*pai-lab-corpus-read-fn*
         (lambda (as-of)
           (declare (ignore as-of))
           (list
            (obj "id" "topical-greeting" "content" "Hello there tonight"
                 "kind" "observation" "origin_class" "lived-user"
                 "epistemic_status" "user-report"
                 "grounding_status" "grounded")
            (obj "id" "answer-bearing" "content" "I chat before sleep"
                 "kind" "observation" "origin_class" "lived-user"
                 "epistemic_status" "user-report"
                 "grounding_status" "grounded")
            (obj "id" "corpus-only" "content" "Unrelated corpus record"
                 "kind" "observation" "origin_class" "lived-user"
                 "epistemic_status" "user-report"
                 "grounding_status" "grounded")
            (obj "id" "sensitive-corpus"
                 "content" "# Operational Constitution private"
                 "kind" "observation" "origin_class" "tool-result"
                 "epistemic_status" "observed"
                 "grounding_status" "grounded")
            (obj "id" "query-echo" "content" "What do I enjoy at night?"
                 "kind" "observation" "origin_class" "lived-user"
                 "epistemic_status" "user-report"
                 "grounding_status" "grounded"))))
       (*pai-lab-batch-embed-texts-fn*
         (lambda (texts)
           (mapcar (lambda (text)
                     (if (search "I chat before sleep" text)
                         '(1.0d0 0.0d0)
                         '(0.0d0 1.0d0)))
                   texts)))
       (*pai-lab-curator-responses*
         (obj
          "rerank-inversion"
          (obj "schema_version" 1 "decision" "SELECT"
               "active_task" "Recall the established bedtime routine."
               "response_obligations" (vector "Answer from cited evidence.")
               "selected_context_ids" (vector "answer-bearing")
               "possible_context"
               (vector (obj "content"
                            "the operator likes chatting with the agent before sleep."
                            "evidence_ids" (vector "answer-bearing")))
               "recommended_tools" (vector)
               "continuity_risks" (vector "Do not substitute recent chatter.")
               "uncertainty" (obj "level" "low" "note" "Direct report."))))
       (scenario
         (obj "id" "rerank-inversion" "mode" "read-only-live"
              "query" "What do I enjoy at night?"
              "as_of" "2026-08-05 02:27:00+00" "result_limit" 1
              "minimum_selected" 1 "maximum_selected" 1
              "expected_selected_ids" (vector "answer-bearing")
              "expected_selected_content_patterns"
              (vector (vector "chat" "sleep"))
              "rerank_experiments"
              (vector "nomic-query-document" "nomic-prefixed-full-scan")
              "curator_experiments" (vector "captured-memory-evidence-v1")
              "expected_curator_decision" "SELECT"))
       (suite (obj "schema_version" 1 "suite_id" "rerank-suite"
                   "scenarios" (vector scenario)))
       (result (pai-lab-run-suite suite))
       (run (aref (gethash "results" result) 0))
       (candidates (gethash "candidates" run))
       (arm (aref (gethash "rerank_experiments" run) 0))
       (full-arm (aref (gethash "rerank_experiments" run) 1))
       (curator-arm (aref (gethash "curator_experiments" full-arm) 0))
       (json (shasht:write-json result nil)))
  (pai-lab-test-check
   "lab distinguishes retrieval rank from production selector rank"
   (let ((greeting (aref candidates 0))
         (answer (aref candidates 1)))
     (and (= 1 (gethash "retrieval_rank" greeting))
          (= 1 (gethash "selector_rank" greeting))
          (string= "selected" (gethash "selection_reason" greeting))
          (= 2 (gethash "retrieval_rank" answer))
          (= 2 (gethash "selector_rank" answer))
          (string= "outside-best-score-window"
                   (gethash "selection_reason" answer)))))
  (pai-lab-test-check
   "lab forwards bounded historical as-of to typed retrieval"
   (string= "2026-08-05 02:27:00+00" *pai-lab-test-as-of*))
  (pai-lab-test-check
   "prefixed reranker promotes answer-bearing evidence without changing oracle"
   (and (string= "failed" (gethash "status" run))
        (string= "passed" (gethash "status" arm))
        (string= "answer-bearing" (aref (gethash "selected_ids" arm) 0))))
  (pai-lab-test-check
   "prefixed reranker accounts only local embedding calls"
   (and (= 3 (gethash "local_embedding_call_count" arm))
        (= 4 (gethash "experimental_local_embedding_call_count" result))
        (zerop (gethash "provider_call_count" arm))
        (zerop (gethash "database_write_count" arm))
        (null (gethash "delivery_authority" arm))))
  (pai-lab-test-check
   "ephemeral full scan reuses cache and batches only missing corpus rows"
   (and (string= "passed" (gethash "status" full-arm))
        (gethash "ephemeral_index" full-arm)
        (= 5 (gethash "raw_corpus_count" full-arm))
        (= 3 (gethash "corpus_count" full-arm))
        (= 2 (gethash "pre_embedding_filtered_count" full-arm))
        (= 1 (gethash "local_embedding_call_count" full-arm))
        (= 1 (gethash "local_embedding_request_count" full-arm))
        (string= "answer-bearing"
                 (aref (gethash "selected_ids" full-arm) 0))))
  (pai-lab-test-check
   "prefixed reranker remains content-free by default"
   (and (null (search "Hello there tonight" json))
        (null (search "I chat before sleep" json))
        (null (search "Recall the established bedtime routine" json))))
  (pai-lab-test-check
   "captured curator validates and selects only manifest evidence"
   (and (string= "passed" (gethash "status" curator-arm))
        (string= "valid" (gethash "validation_status" curator-arm))
        (= 1 (gethash "selected_count" curator-arm))
        (string= "answer-bearing"
                 (aref (gethash "selected_ids" curator-arm) 0))))
  (pai-lab-test-check
   "captured curator has no provider write or delivery authority"
   (and (zerop (gethash "provider_call_count" curator-arm))
        (zerop (gethash "database_write_count" curator-arm))
        (null (gethash "delivery_authority" curator-arm))
        (eq :null (gethash "request_messages" curator-arm))
        (eq :null (gethash "compiled_context_block" curator-arm)))))

(let* ((*pai-lab-write-probe-fn* (lambda () t))
       (*pai-lab-read-only-transaction-fn* (lambda (thunk) (funcall thunk)))
       (*pai-lab-memory-search-fn*
         (lambda (&rest ignored)
           (declare (ignore ignored))
           (list (obj "id" "question-anchor" "content" "Which drink?"
                      "origin_class" "lived-user"
                      "epistemic_status" "user-report"
                      "grounding_status" "grounded" "similarity" 0.95d0
                      "epistemic_metadata"
                      (obj "turn_id" "turn-drink" "role" "user"
                           "sequence" 0)))))
       (*pai-lab-corpus-read-fn*
         (lambda (as-of)
           (declare (ignore as-of))
           (list
            (obj "id" "question-anchor" "content" "Which drink do I prefer?"
                 "created_at" "2026-08-05T09:00:00Z"
                 "origin_class" "lived-user" "epistemic_status" "user-report"
                 "grounding_status" "grounded" "epistemic_metadata"
                 (obj "turn_id" "turn-drink" "role" "user" "sequence" 0))
            (obj "id" "answer-neighbor" "content" "the operator prefers tea."
                 "origin_class" "lived-agent-action"
                 "epistemic_status" "agent-action"
                 "grounding_status" "grounded" "epistemic_metadata"
                 (obj "turn_id" "turn-drink" "role" "assistant" "sequence" 1))
            (obj "id" "other" "content" "A distinct fact."
                 "origin_class" "lived-user" "epistemic_status" "user-report"
                 "grounding_status" "grounded" "epistemic_metadata" (obj)))))
       (*pai-lab-embed-text-fn* (lambda (text) (declare (ignore text))
                                    '(1.0d0 0.0d0)))
       (*pai-lab-batch-embed-texts-fn*
         (lambda (texts)
           (mapcar (lambda (text)
                     (cond ((search "Which drink" text) '(1.0d0 0.0d0))
                           ((search "prefers tea" text) '(0.7d0 0.3d0))
                           (t '(0.0d0 1.0d0))))
                   texts)))
       (*pai-lab-curator-responses* nil)
       (scenario
         (obj "id" "turn-bundle" "mode" "read-only-live"
              "query" "What drink is my favorite?" "result_limit" 1
              "expected_selected_content_patterns" (vector (vector "tea"))
              "rerank_experiments"
              (vector "nomic-turn-bundle-neighborhood-v1")
              "curator_experiments"
              (vector "captured-turn-bundle-evidence-v2")))
       (suite (obj "schema_version" 1 "suite_id" "turn-bundle-suite"
                   "scenarios" (vector scenario)))
       (result (pai-lab-run-suite suite))
       (run (aref (gethash "results" result) 0))
       (arm (aref (gethash "rerank_experiments" run) 0))
       (row (aref (gethash "rows" arm) 0))
       (curator (aref (gethash "curator_experiments" arm) 0))
       (json (shasht:write-json result nil)))
  (pai-lab-test-check
   "turn-bundle arm recovers answer-bearing sibling into bounded manifest"
   (and (string= "passed" (gethash "status" arm))
        (= 2 (gethash "member_count" row))
        (string= "2026-08-05T09:00:00Z"
                 (gethash "observed_at" row))
        (equal '("user" "assistant")
               (coerce (gethash "member_roles" row) 'list))
        (= 1 (gethash "selected_match_count"
                      (gethash "content_oracle"
                               (gethash "expectations" arm))))))
  (pai-lab-test-check
   "turn-bundle curator request is captured-output only and redacted by default"
   (and (string= "awaiting-captured-output" (gethash "status" curator))
        (eq :null (gethash "request_messages" curator))
        (zerop (gethash "provider_call_count" arm))
        (zerop (gethash "database_write_count" arm))
        (null (gethash "delivery_authority" arm))
        (null (search "the operator prefers tea" json)))))

(let* ((*pai-lab-write-probe-fn* (lambda () t))
       (scenario
         (obj "id" "forbidden" "mode" "read-only-live" "query" "hello"
              "tick" nil))
       (suite (obj "schema_version" 1 "suite_id" "forbidden-suite"
                   "scenarios" (vector scenario)))
       (result (pai-lab-run-suite suite))
       (run (aref (gethash "results" result) 0)))
  (pai-lab-test-check "authority-bearing scenario field fails closed"
                        (and (string= "error" (gethash "status" run))
                             (null (gethash "delivery_authority" run)))))

(let* ((database-probes 0)
       (*pai-lab-write-probe-fn* (lambda () (incf database-probes) t))
       (*pai-lab-deliverable-read-fn*
         (lambda (path)
           (shasht:write-json
            (obj "path" (format nil "/agent/state/deliverables/~a" path)
                 "original_characters" 24 "truncated" nil
                 "content" "private tracker contents") nil)))
       (scenario
         (obj "id" "deliverable" "mode" "read-only-deliverable"
              "path" "contacts/tracker.md"))
       (suite (obj "schema_version" 1 "suite_id" "deliverable-suite"
                   "scenarios" (vector scenario)))
       (result (pai-lab-run-suite suite))
       (run (aref (gethash "results" result) 0))
       (json (shasht:write-json result nil)))
  (pai-lab-test-check
   "deliverable mode is redacted and filesystem-read-only by default"
   (and (string= "passed" (gethash "status" result))
        (string= "read-only-deliverable" (gethash "mode" run))
        (eq :null (gethash "content" run))
        (= 1 (gethash "filesystem_read_count" run))
        (zerop (gethash "filesystem_write_count" run))
        (gethash "filesystem_access_required" result)
        (gethash "state_mount_read_only" result)
        (null (search "private tracker contents" json))))
  (pai-lab-test-check
   "deliverable-only mode never invokes the database write probe"
   (zerop database-probes))
  (let* ((*pai-lab-reveal-private-content* t)
         (private-result (pai-lab-run-suite suite))
         (private-run (aref (gethash "results" private-result) 0)))
    (pai-lab-test-check
     "explicit deliverable review reveals only the bounded reader result"
     (and (string= "private tracker contents" (gethash "content" private-run))
          (string= "contacts/tracker.md" (gethash "path" private-run)))))
  (let* ((unsafe
           (obj "schema_version" 1 "suite_id" "unsafe-deliverable"
                "scenarios"
                (vector
                 (obj "id" "unsafe" "mode" "read-only-deliverable"
                      "path" "../secret.md"))))
         (unsafe-result (pai-lab-run-suite unsafe))
         (unsafe-run (aref (gethash "results" unsafe-result) 0)))
    (pai-lab-test-check
     "deliverable path traversal fails closed"
     (string= "error" (gethash "status" unsafe-run)))))

(let ((report (pai-lab-capability-report)))
  (pai-lab-test-check "first lab slice exposes no tick provider or delivery"
                        (and (find "read-only-deliverable"
                                   (gethash "modes" report) :test #'string=)
                             (null (gethash "ticks_available" report))
                             (null (gethash "provider_calls_available" report))
                             (null (gethash "delivery_authority" report)))))

(let* ((question-1
         (obj "id" "q-1" "content" "Which drink do I prefer?"
              "created_at" "2026-08-05 09:00:00+00"
              "origin_class" "lived-user" "grounding_status" "grounded"
              "epistemic_metadata"
              (obj "turn_id" "turn-1" "role" "user" "sequence" 0)))
       (answer-1
         (obj "id" "a-1" "content" "You told me that tea is your preference."
              "origin_class" "lived-agent-action"
              "grounding_status" "grounded" "epistemic_metadata"
              (obj "turn_id" "turn-1" "role" "assistant" "sequence" 1)))
       (question-2
         (obj "id" "q-2" "content" "What drink is my favorite?"
              "origin_class" "lived-user" "grounding_status" "grounded"
              "epistemic_metadata"
              (obj "turn_id" "turn-2" "role" "user" "sequence" 0)))
       (answer-2
         (obj "id" "a-2" "content" "The earlier exchange did not establish it."
              "origin_class" "lived-agent-action"
              "grounding_status" "grounded" "epistemic_metadata"
              (obj "turn_id" "turn-2" "role" "assistant" "sequence" 1)))
       (distinct
         (obj "id" "distinct" "content" "A separate grounded fact."
              "origin_class" "lived-user" "grounding_status" "grounded"
              "epistemic_metadata" (obj)))
       (corpus (list question-1 answer-1 question-2 answer-2 distinct))
       (ranked (list (list question-1 0.90d0 0.93d0 '(1.0d0 0.0d0))
                     (list question-2 0.89d0 0.92d0 '(0.99d0 0.01d0))
                     (list answer-1 0.60d0 0.60d0 '(0.8d0 0.2d0))
                     (list answer-2 0.59d0 0.59d0 '(0.79d0 0.21d0))
                     (list distinct 0.55d0 0.58d0 '(0.0d0 1.0d0)))))
  (multiple-value-bind (bundles suppressed considered)
      (turn-bundle-build-candidates
       ranked corpus :cluster-cap 1 :near-duplicate-threshold 0.98d0)
    (let* ((first (first bundles))
           (singleton (find "turn-bundle-node:distinct" bundles
                            :test #'string= :key (lambda (row)
                                                   (gethash "id" row)))))
      (pai-lab-test-check
       "turn anchor expands to ordered user and assistant evidence"
       (and (search "the operator: Which drink" (gethash "content" first))
            (search "the agent: You told me" (gethash "content" first))
            (equal '("q-1" "a-1")
                   (%pai-lab-list (gethash "evidence_node_ids" first)))))
      (pai-lab-test-check
       "bundle preserves structural roles without question regex"
       (equal '("user" "assistant")
              (%pai-lab-list (gethash "member_roles" first))))
      (pai-lab-test-check
       "bundle preserves typed observation time from its anchor"
       (string= "2026-08-05 09:00:00+00"
                (gethash "observed_at" first)))
      (pai-lab-test-check
       "unrelated turn never becomes a neighborhood member"
       (not (member "distinct"
                    (%pai-lab-list (gethash "evidence_node_ids" first))
                    :test #'string=)))
      (pai-lab-test-check
       "missing turn metadata produces a bounded singleton"
       (and singleton (= 1 (gethash "member_count" singleton))))
      (pai-lab-test-check
       "near-duplicate bundle cluster is capped but distinct evidence remains"
       (and (= 3 considered) (= 1 suppressed) (= 2 (length bundles)))))))

(let* ((lexical
         (obj "id" "petula-user" "content"
              "the operator said that Petula is their dog."
              "origin_class" "lived-user" "grounding_status" "grounded"
              "epistemic_metadata"
              (obj "turn_id" "turn-petula" "role" "user" "sequence" 0)
              "candidate_sources" (vector "lexical")
              "lexical_tier" 1 "lexical_match_count" 1
              "lexical_coverage" 1.0d0 "lexical_terms" (vector "petula")))
       (reply
         (obj "id" "petula-assistant" "content"
              "the agent acknowledged the relationship."
              "origin_class" "lived-agent-action"
              "grounding_status" "grounded" "epistemic_metadata"
              (obj "turn_id" "turn-petula" "role" "assistant" "sequence" 1)))
       (ranked (list (list lexical 0.0d0 3.0d0 nil))))
  (multiple-value-bind (bundles suppressed considered)
      (turn-bundle-build-candidates ranked (list lexical reply))
    (declare (ignore suppressed considered))
    (let ((bundle (first bundles)))
      (pai-lab-test-check
       "vectorless explicit lexical anchor expands through the generic turn bundler"
       (and bundle
            (equal '("petula-user" "petula-assistant")
                   (%pai-lab-list (gethash "evidence_node_ids" bundle)))))
      (pai-lab-test-check
       "bundle preserves explicit candidate diagnostics"
       (and (= 1 (gethash "lexical_tier" bundle 0))
            (equal '("lexical")
                   (%pai-lab-list (gethash "candidate_sources" bundle))))))))

(pai-lab-test-check
 "invalid turn-bundle bounds fail closed"
 (handler-case
     (progn (turn-bundle-build-candidates nil nil :max-bundles 0) nil)
   (error () t)))

(let* ((long (make-string 1000 :initial-element #\x))
       (members
         (list
          (obj "content" long "origin_class" "lived-user"
               "epistemic_metadata" (obj "role" "user" "sequence" 0))
          (obj "content" long "origin_class" "lived-agent-action"
               "epistemic_metadata" (obj "role" "assistant" "sequence" 1))
          (obj "content" long "origin_class" "tool-result"
               "epistemic_metadata" (obj "role" "tool" "sequence" 2))
          (obj "content" (concatenate 'string "FINAL-ANSWER " long)
               "origin_class" "lived-agent-action"
               "epistemic_metadata" (obj "role" "assistant" "sequence" 3))))
       (rendered (%turn-bundle-render members)))
  (pai-lab-test-check
   "fixed bundle budget preserves final assistant evidence"
   (and (<= (length rendered) *context-curator-max-evidence-chars*)
        (search "FINAL-ANSWER" rendered))))

(pai-lab-test-check
 "captured curator failures propagate while pending capture remains neutral"
 (and (%pai-lab-curator-experiments-pass-p
       (vector (obj "status" "awaiting-captured-output")))
      (%pai-lab-curator-experiments-pass-p
       (vector (obj "status" "passed")))
      (not (%pai-lab-curator-experiments-pass-p
            (vector (obj "status" "failed"))))
      (not (%pai-lab-curator-experiments-pass-p
            (vector (obj "status" "rejected"))))))

(pai-lab-test-check
 "rerank qualification fails closed on a failed captured curator child"
 (not (%pai-lab-rerank-experiments-pass-p
       (vector (obj "status" "passed"
                    "curator_experiments"
                    (vector (obj "status" "failed")))))))

(format t "~%== N1 atom decomposition Lab mode ==~%")

(pai-lab-test-check
 "atom Lab scenario key validation matches incumbent contract mechanics"
 (let ((valid (obj "id" "fixture" "mode" "atom-decomposition-shadow")))
   (and (eq valid
            (%pai-lab-atom-exact-keys
             valid '("id" "mode") "atom Lab fixture"))
        (eq valid
            (%memory-atom-exact-keys
             valid '("id" "mode") "atom Lab fixture")))))

(pai-lab-test-check
 "atom Lab scenario bounds match the incumbent atom vocabulary"
 (and (= *pai-lab-atom-max-atoms* *memory-atom-max-atoms*)
      (equal *pai-lab-atom-forms* *memory-atom-forms*)))
(setf *pai-lab-test-atom-write-probes* 0)
(let* ((evidence
         (vector
          (obj "id" "lab-atom-user-0000" "role" "user" "sequence" 0
               "observed_at" "2026-08-06T23:00:00Z"
               "content" "I prefer mint tea.")))
       (scenario
         (obj "id" "atom-preference" "mode" "atom-decomposition-shadow"
              "turn_id" "lab-atom-turn" "captured_at" "2026-08-06T23:00:01Z"
              "evidence" evidence "expected_decision" "PROPOSE"
              "expected_atom_count" 1
              "expected_memory_forms" (vector "semantic")))
       (captured
         (obj "schema_version" 1 "decision" "PROPOSE"
              "atoms"
              (vector
               (obj "memory_form" "semantic" "subject" *operator-id*
                    "predicate" "preference.drink" "value" "the operator prefers mint tea."
                    "polarity" "affirmed" "qualifiers" (vector)
                    "observed_at" "2026-08-06T23:00:00Z"
                    "valid_from" :null "valid_to" :null
                    "disclosure_candidate" "personal-shareable"
                    "evidence_ids" (vector "lab-atom-user-0000")))
              "exclusions" (vector)
              "uncertainty" (obj "level" "low" "note" "Direct report.")))
       (suite (obj "schema_version" 1 "suite_id" "atom-lab-suite"
                   "scenarios" (vector scenario))))
  (let* ((*pai-lab-write-probe-fn*
           (lambda () (incf *pai-lab-test-atom-write-probes*) t))
         (*pai-lab-atom-responses* nil)
         (result (pai-lab-run-suite suite))
         (run (aref (gethash "results" result) 0)))
    (pai-lab-test-check
     "atom Lab exports a neutral captured-output request"
     (and (string= "passed" (gethash "status" result))
          (string= "awaiting-captured-output" (gethash "validation_status" run))
          (eq :null (gethash "request_messages" run))
          (zerop (gethash "provider_call_count" run))
          (zerop (gethash "database_write_count" run))
          (null (gethash "admission_authority" run))
          (null (gethash "delivery_authority" run)))))
  (let* ((*pai-lab-write-probe-fn*
           (lambda () (incf *pai-lab-test-atom-write-probes*) t))
         (*pai-lab-atom-responses* (obj "atom-preference" captured))
         (result (pai-lab-run-suite suite))
         (run (aref (gethash "results" result) 0))
         (atom (aref (gethash "atoms" run) 0))
         (json (shasht:write-json result nil)))
    (pai-lab-test-check
     "captured atom response validates with deterministic identity"
     (and (string= "passed" (gethash "status" result))
          (string= "valid" (gethash "validation_status" run))
          (= 1 (gethash "atom_count" run))
          (= 64 (length (gethash "claim_key" atom)))
          (= 64 (length (gethash "idempotency_key" atom)))
          (string= "private" (gethash "durable_disclosure_class" atom))))
    (pai-lab-test-check
     "default atom Lab output redacts evidence and candidate value"
     (and (null (search "I prefer mint tea" json))
          (null (search "the operator prefers mint tea" json)))))
  (let* ((*pai-lab-reveal-private-content* t)
         (*pai-lab-write-probe-fn*
           (lambda () (incf *pai-lab-test-atom-write-probes*) t))
         (*pai-lab-atom-responses* (obj "atom-preference" captured))
         (result (pai-lab-run-suite suite))
         (run (aref (gethash "results" result) 0))
         (json (shasht:write-json result nil)))
    (pai-lab-test-check
     "explicit private atom review shows request evidence and normalized value"
     (and (vectorp (gethash "request_messages" run))
          (search "I prefer mint tea" json)
          (search "the operator prefers mint tea" json))))
  (pai-lab-test-check
   "atom-only Lab scenarios never invoke the database write probe"
   (zerop *pai-lab-test-atom-write-probes*)))

(format t "~%PAI-LAB TESTS: ~a passed, ~a failed.~%"
        *pai-lab-test-pass* *pai-lab-test-fail*)
(when (plusp *pai-lab-test-fail*) (sb-ext:exit :code 1))
