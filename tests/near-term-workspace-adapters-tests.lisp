(in-package :agent)

(defvar *near-term-adapter-test-pass* 0)
(defvar *near-term-adapter-test-fail* 0)
(defvar *near-term-adapter-test-action-calls* 0)

(defun near-term-adapter-test-check (name condition)
  (if condition
      (progn (incf *near-term-adapter-test-pass*)
             (format t "  ok   ~a~%" name))
      (progn (incf *near-term-adapter-test-fail*)
             (format t "  FAIL ~a~%" name))))

(defun near-term-adapter-test-json (value)
  (with-output-to-string (stream) (shasht:write-json value stream)))

(defun near-term-adapter-test-source (snapshot name)
  (find name (coerce (gethash "sources" snapshot) 'list)
        :key (lambda (row) (gethash "source" row)) :test #'string=))

(load (test-source "near-term-workspace.lisp"))
(load (test-source "near-term-workspace-adapters.lisp"))

(setf (fdefinition 'telegram-send)
      (lambda (&rest arguments)
        (declare (ignore arguments))
        (incf *near-term-adapter-test-action-calls*)))
(setf (fdefinition 'cognitive-call)
      (lambda (&rest arguments)
        (declare (ignore arguments))
        (incf *near-term-adapter-test-action-calls*)))

(let* ((latent-ready
         (obj "id" "latent-1" "state" "ready"
              "content" "A grounded private idea about the quarterly talk."
              "evidence_ids" (vector "memory-1")
              "source_event_ids" (vector "event-1")
              "next_reconsideration" 900 "expires_at" 2000
              "created_at" 800 "updated_at" 900))
       (latent-terminal
         (obj "id" "latent-old" "state" "expressed"
              "content" "Already expressed material."
              "evidence_ids" (vector "memory-old")
              "created_at" 700 "updated_at" 750))
       (question
         (obj "id" 42 "status" "open"
              "statement" "What concrete detail would strengthen the demo?"
              "evidence-node-ids" (vector "memory-2")
              "root-evidence-node-ids" (vector "root-2")
              "created-at" 850))
       (scheduled
         (obj "id" "scheduled-context-1" "schedule_id" "schedule-1"
              "text" "Review the quarterly presentation outline."
              "fired_at_utc" 950 "consumed_at_utc" :null))
       (initiative
         (obj "id" "initiative-1" "kind" "share-thought"
              "reason" "A useful grounded observation about the demo flow."
              "status" "deferred" "created_at" 880 "updated_at" 960))
       (initiative-terminal
         (obj "id" "initiative-old" "kind" "share-thought"
              "reason" "An already suppressed thought."
              "status" "suppressed" "created_at" 700 "updated_at" 710))
       (all-source-records
         (list latent-ready latent-terminal question scheduled initiative
               initiative-terminal))
       (before (near-term-adapter-test-json all-source-records)))
  (let ((*near-term-workspace-latent-source-fn*
          (lambda () (list latent-ready latent-terminal)))
        (*near-term-workspace-question-source-fn*
          (lambda () (list question)))
        (*near-term-workspace-scheduler-source-fn*
          (lambda () (vector scheduled)))
        (*near-term-workspace-initiative-source-fn*
          (lambda () (list initiative initiative-terminal)))
        (*near-term-workspace-intention-source-fn* (lambda () nil)))
    (format t "~%== read-only source translation ==~%")
    (let* ((snapshot (near-term-workspace-shadow-snapshot :now 1000))
           (items (coerce (gethash "items" snapshot) 'list))
           (sources (coerce (gethash "sources" snapshot) 'list)))
      (near-term-adapter-test-check "all five source APIs are sampled"
                                    (= 5 (length sources)))
      (near-term-adapter-test-check "only active relevant records emit items"
                                    (and (= 4 (length items))
                                         (= 4 (gethash "events_considered"
                                                       snapshot))))
      (near-term-adapter-test-check "source types remain explicit"
                                    (every
                                     (lambda (source)
                                       (find source items
                                             :key (lambda (item)
                                                    (gethash "source" item))
                                             :test #'string=))
                                     '("latent-v2" "self-model" "scheduler"
                                       "initiative")))
      (near-term-adapter-test-check "ready latent record retains its artifact"
                                    (let ((item
                                            (find "latent-v2" items
                                                  :key (lambda (row)
                                                         (gethash "source" row))
                                                  :test #'string=)))
                                      (and (string= "ready"
                                                    (gethash "state" item))
                                           (search "quarterly talk"
                                                   (gethash "artifact_summary"
                                                            item)))))
      (near-term-adapter-test-check "snapshot is dashboard-only and non-actuating"
                                    (and (null (gethash "prompt_integration"
                                                       snapshot))
                                         (null (gethash "tick_integration"
                                                       snapshot))
                                         (null (gethash "publication_integration"
                                                       snapshot))
                                         (null (gethash
                                                "direct_delivery_capability"
                                                snapshot))
                                         (zerop
                                          *near-term-adapter-test-action-calls*)))
      (near-term-adapter-test-check "adapters do not mutate source records"
                                    (string= before
                                             (near-term-adapter-test-json
                                              all-source-records)))))

  (format t "~%== isolated source failure ==~%")
  (let ((*near-term-workspace-latent-source-fn*
          (lambda () (error "fixture source failure")))
        (*near-term-workspace-question-source-fn*
          (lambda () (list question)))
        (*near-term-workspace-scheduler-source-fn* (lambda () nil))
        (*near-term-workspace-initiative-source-fn* (lambda () nil))
        (*near-term-workspace-intention-source-fn* (lambda () nil)))
    (let ((snapshot (near-term-workspace-shadow-snapshot :now 1000)))
      (near-term-adapter-test-check "one failing source does not lose other items"
                                    (= 1 (gethash "active_items" snapshot)))
      (near-term-adapter-test-check "source failure is explicit and content-free"
                                    (let ((source
                                            (near-term-adapter-test-source
                                             snapshot "latent-v2")))
                                      (and (string= "error"
                                                    (gethash "status" source))
                                           (gethash "error_type" source)
                                           (null (gethash "error" source))))))))

(format t "~%~a passed, ~a failed~%"
        *near-term-adapter-test-pass* *near-term-adapter-test-fail*)
(when (plusp *near-term-adapter-test-fail*) (sb-ext:exit :code 1))
