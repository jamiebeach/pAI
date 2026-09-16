;;;; recall-selection-tests.lisp -- pure personal-recall planning and fusion.
;;;; harness: full-system

(in-package :agent)

(defvar *rs-pass* 0)
(defvar *rs-fail* 0)

(defun rs-check (name condition)
  (if condition
      (progn (incf *rs-pass*) (format t "PASS ~a~%" name))
      (progn (incf *rs-fail*) (format t "FAIL ~a~%" name))))

(defun rs-row (id content)
  (obj "source_id" id "content" content))

(format t "~%== personal recall selector ==~%")

(let ((plan (build-recall-query-plan
             "What were my children's names earlier?"
             :agent-id "selector-agent" :persona-id "selector-persona"
             :operator-binding "operator:fixture" :root-id 40
             :trigger-event-id 41 :as-of 1000)))
  (rs-check "query plan is closed and versioned"
            (and (= 1 (gethash "schema_version" plan))
                 (string= "personal-recall-selection-v1"
                          (gethash "policy_revision" plan))
                 (eq t (gethash "operator_fact_query" plan))
                 (string= "historical" (gethash "requested_time_scope" plan))
                 (equalp #("child" "name")
                         (gethash "requested_categories" plan))))
  (let* ((child-a (recall-selection-candidate
                   plan "graph-fact" (rs-row "fact:a" "The operator's child name is Child-A.")
                   :candidate-id "fact:a" :support-key "support:a"
                   :local-rank 4 :operator-support-p t
                   :speaker-basis "validated-graph-fact"))
         (child-a-duplicate (recall-selection-candidate
                             plan "semantic-bundle"
                             (rs-row "memory:a" "The operator's child name is Child-A.")
                             :candidate-id "memory:a" :support-key "support:a"
                             :local-rank 1 :operator-support-p t
                             :speaker-basis "operator-utterance"))
         (child-b (recall-selection-candidate
                   plan "semantic-bundle" (rs-row "memory:b" "My child is Child-B.")
                   :candidate-id "memory:b" :support-key "support:b"
                   :local-rank 20 :semantic-rank 2 :operator-support-p t
                   :speaker-basis "operator-utterance"))
         (recent-echo (recall-selection-candidate
                       plan "raw-dialogue"
                       (rs-row "event:recent" "I could not find your children's names.")
                       :candidate-id "event:recent" :support-key "event:recent"
                       :local-rank 1 :operator-support-p nil
                       :speaker-basis "assistant-utterance"))
         (unrelated (recall-selection-candidate
                     plan "sealed-episode"
                     (rs-row "episode:pet" "Synopsis: the operator discussed Pet-A.")
                     :candidate-id "episode:pet" :support-key "episode:pet"
                     :local-rank 1 :operator-support-p t
                     :speaker-basis "generated-synopsis")))
    (multiple-value-bind (selected report)
        (recall-selection-select
         (list recent-echo unrelated child-b child-a-duplicate child-a) 5 1000)
      (rs-check "historical positives beat recent echo and unrelated synopsis"
                (and (= 2 (length selected))
                     (every (lambda (row)
                              (member (gethash "candidate_id" row)
                                      '("fact:a" "memory:b") :test #'string=))
                            selected)))
      (rs-check "exact support duplicate is collapsed"
                (= 1 (gethash "duplicate_refusal_count" report)))
      (rs-check "selection report is content-free and zero-write"
                (and (= 0 (gethash "database_write_count" report))
                     (null (search "Child-A" (shasht:write-json report nil))))))
    (multiple-value-bind (selected report)
        (recall-selection-select (list child-b child-a) 1 1000)
      (declare (ignore report))
      (rs-check "limit one takes deterministic best evidence"
                (and (= 1 (length selected))
                     (string= "fact:a" (gethash "candidate_id" (first selected))))))
    (multiple-value-bind (selected report)
        (recall-selection-select (list child-a child-b) 5 10)
      (rs-check "whole evidence units are refused under a tight budget"
                (and (null selected)
                     (= 2 (gethash "budget_refusal_count" report)))))))

(let ((business (build-recall-query-plan "What is my business partner's name?"
                                          :operator-binding "operator:fixture"))
      (unknown (build-recall-query-plan "What is my blood type?"
                                        :operator-binding "operator:fixture"))
      (third-person (build-recall-query-plan "What is their spouse's name?"
                                             :operator-binding "operator:fixture")))
  (rs-check "business partner does not become spouse"
            (not (find "spouse" (gethash "requested_categories" business)
                       :test #'string=)))
  (rs-check "unrecognized personal qualifier fails closed"
            (not (gethash "operator_fact_query" unknown)))
  (rs-check "ambiguous third person does not acquire operator authority"
            (and (not (gethash "operator_fact_query" third-person))
                 (eq :null (gethash "operator_binding" third-person)))))

(let* ((plan (build-recall-query-plan "What are my children's names?"
                                      :operator-binding "operator:fixture"))
       (right (recall-selection-candidate
               plan "graph-fact"
               (rs-row "fact:child" "The operator's child is named Child-A.")
               :local-rank 2 :operator-support-p t))
       (wrong (recall-selection-candidate
               plan "graph-fact"
               (rs-row "fact:spouse" "The operator's spouse is named Partner-A.")
               :local-rank 1 :operator-support-p t)))
  (multiple-value-bind (selected report)
      (recall-selection-select (list wrong right) 1 3200)
    (declare (ignore report))
    (rs-check "attribute-only name overlap cannot replace the requested relationship"
              (and (= 1 (length selected))
                   (search "Child-A"
                           (gethash "content"
                                    (gethash "record" (first selected))))))))

(let* ((plan (build-recall-query-plan "What is their spouse's name?"
                                      :operator-binding "operator:fixture"))
       (candidate
         (recall-selection-candidate
          plan "graph-fact"
          (rs-row "fact:ambiguous" "The operator's spouse is named Partner-A.")
          :local-rank 1 :operator-support-p t)))
  (multiple-value-bind (selected report)
      (recall-selection-select (list candidate) 4 3200)
    (declare (ignore report))
    (rs-check "ambiguous third-person family request does not become operator recall"
              (null selected))))

(rs-check "first-person subject pronoun resolves the operator recall plan"
          (eq t (gethash "operator_fact_query"
                         (build-recall-query-plan
                          "What condition did I have earlier?"
                          :operator-binding "operator:fixture"))))

;; Fixed mature selector corpus: the useful row follows distractors beyond the
;; source discovery ceilings.  This measures deterministic fusion only; it is
;; deliberately not reported as provider, embedding, or end-to-end latency.
(let* ((plan (build-recall-query-plan "What are my pets' names?"
                                      :operator-binding "operator:fixture"))
       (candidates
         (append
          (loop for index from 1 to 80
                collect
                (recall-selection-candidate
                 plan "sealed-episode"
                 (rs-row (format nil "distractor:~d" index)
                         (format nil "Unrelated presentation color ~d." index))
                 :local-rank index :operator-support-p t))
          (list
           (recall-selection-candidate
            plan "graph-fact"
            (rs-row "fact:pet" "The operator's pet name is Pet-A.")
            :local-rank 81 :operator-support-p t))))
       (timings nil) (all-selected-p t)
       (cold-start (get-internal-real-time))
       (cold-report nil))
  (multiple-value-bind (selected report)
      (recall-selection-select candidates 4 3200)
    (setf cold-report report)
    (unless (and (= 1 (length selected))
                 (string= "fact:pet"
                          (gethash "candidate_id" (first selected))))
      (setf all-selected-p nil)))
  (let ((cold-ms (* 1000d0
                    (/ (- (get-internal-real-time) cold-start)
                       internal-time-units-per-second))))
  (loop repeat 20 do
    (let ((start (get-internal-real-time)))
      (multiple-value-bind (selected report)
          (recall-selection-select candidates 4 3200)
        (declare (ignore report))
        (unless (and (= 1 (length selected))
                     (string= "fact:pet"
                              (gethash "candidate_id" (first selected))))
          (setf all-selected-p nil)))
      (push (* 1000d0
               (/ (- (get-internal-real-time) start)
                  internal-time-units-per-second))
            timings)))
  (setf timings (sort timings #'<))
  (rs-check "twenty mature-corpus selector runs retain the late positive"
            all-selected-p)
    (format t (concatenate 'string
                           "SELECTOR TIMING cold_runs=1 repeat_runs=20 corpus=81 "
                           "cold_ms=~,3f median_ms=~,3f p95_ms=~,3f "
                           "examined=~d selected=~d budget_refused=~d~%")
            cold-ms (nth 9 timings) (nth 18 timings)
            (gethash "examined_count" cold-report)
            (gethash "selected_count" cold-report)
            (gethash "budget_refusal_count" cold-report))))

(format t "~%Personal recall selector: ~d passed, ~d failed~%"
        *rs-pass* *rs-fail*)
(when (plusp *rs-fail*) (error "personal recall selector failures"))
