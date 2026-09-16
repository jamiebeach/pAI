(unless (find-package :agent) (defpackage :agent (:use :cl)))
(in-package :agent)

(ql:quickload '(:bordeaux-threads :shasht) :silent t)

(unless (fboundp 'obj)
  (defun obj (&rest pairs)
    (loop with table = (make-hash-table :test #'equal)
          for (key value) on pairs by #'cddr
          do (setf (gethash key table) value)
          finally (return table))))

(defvar *trace-test-pass* 0)
(defvar *trace-test-fail* 0)

(defun trace-check (name condition)
  (if condition
      (progn (incf *trace-test-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *trace-test-fail*) (format t "  FAIL ~a~%" name))))

(defun trace-ref (object key)
  (and (hash-table-p object) (gethash key object)))

(defun trace-span-by-name (trace name)
  (find name (coerce (trace-ref trace "spans") 'list)
        :key (lambda (span) (trace-ref span "name")) :test #'string=))

;;; Define optional production seams so isolated tests exercise every wrapper.
(unless (fboundp 'memory-recall)
  (defun memory-recall (&rest args) (declare (ignore args)) (list "memory")))
(unless (fboundp 'memory-search)
  (defun memory-search (&rest args) (declare (ignore args)) (list "typed-memory")))
(unless (fboundp 'memory-record-use)
  (defun memory-record-use (&rest args) (declare (ignore args)) (obj "updated_count" 0)))
(unless (fboundp 'cognitive-call)
  (defun cognitive-call (&rest args) (declare (ignore args)) (obj "status" "accepted")))
(unless (fboundp 'raw-call-model)
  (defun raw-call-model (messages)
    (declare (ignore messages))
    (obj "choices" (vector (obj "message" (obj "content" "stub"))))))
;; CALL-MODEL is a seam (P0c item 3): the :INSTALL phase below registers a
;; timing layer on it, which requires the seam to already exist. The harness
;; preload's self-mod.lisp installs it as a plain DEFUN (untouched; see
;; wrap-chain-registry.lisp's call-model note), which is NOT a seam, so this
;; must unconditionally re-declare it rather than guard on FBOUNDP the way
;; the stubs above do (gotcha 12).
(define-seam call-model (messages) (raw-call-model messages))
(unless (fboundp 'tick-commit-apply)
  (defun tick-commit-apply (&rest args)
    (declare (ignore args)) (obj "status" "shadow-valid" "write_count" 0)))
(unless (fboundp 'embed-text)
  (defun embed-text (text) (declare (ignore text)) '(0.1d0 0.2d0)))
(unless (fboundp '%conv-persist-write)
  (defun %conv-persist-write (history) (length history)))
(unless (fboundp '%v2-broadcast)
  (defun %v2-broadcast (type content) (declare (ignore content)) type))
(unless (fboundp 'tick-once)
  (defun tick-once () "maintenance"))
(unless (fboundp '%drives-event-initiate)
  (defun %drives-event-initiate (reason &optional (urgency :normal))
    (list reason urgency)))

(load (test-source "observability-tracing.lisp"))

;; The timing wrappers are installed by a DEFINE-INIT :install action, not by
;; loading the file. Under the original entrypoint installation was a load-time
;; side effect, so this suite never had to ask for it; after the load/init
;; separation the wrapper base (PAI-BASE-RAW-CALL-MODEL-TIMING) simply does not
;; exist until :install runs.
(initialize :phases (list :install) :stop-on-error nil :verbose nil)

(format t "~%== nested spans and value transparency ==~%")
(let* ((emitted nil)
       (*timing-event-sink* (lambda (payload) (push payload emitted))))
  (let ((values
          (multiple-value-list
           (call-with-timing-trace
            (lambda ()
              (with-timing-span ("outer" :attributes (obj "safe" 7))
                (with-timing-span ("inner")
                  (values :one :two))))
            :trace-id "trace-values" :turn-id "turn-values"
            :origin "test" :root-span "test.total" :sampled-p t))))
    (trace-check "multiple return values survive" (equal values '(:one :two)))
    (trace-check "one batched event emitted" (= (length emitted) 1))
    (let* ((trace (first emitted))
           (outer (trace-span-by-name trace "outer"))
           (inner (trace-span-by-name trace "inner")))
      (trace-check "root and nested spans present"
                   (and (trace-span-by-name trace "test.total") outer inner))
      (trace-check "parent relationship retained"
                   (string= (trace-ref inner "parent_span_id")
                            (trace-ref outer "span_id")))
      (trace-check "span start offsets support waterfall reconstruction"
                   (and (numberp (trace-ref outer "start_offset_ms"))
                        (numberp (trace-ref inner "start_offset_ms"))))
      (trace-check "trace is successful" (string= (trace-ref trace "status") "ok")))))

(format t "~%== errors close and remain observable ==~%")
(let* ((emitted nil)
       (*timing-event-sink* (lambda (payload) (push payload emitted))))
  (trace-check "original error propagates"
               (handler-case
                   (progn
                     (call-with-timing-trace
                      (lambda () (with-timing-span ("failing") (error "expected")))
                      :trace-id "trace-error" :origin "tick"
                      :root-span "tick.total" :sampled-p nil)
                     nil)
                 (error () t)))
  (trace-check "unsampled error is force-emitted" (= (length emitted) 1))
  (trace-check "error trace has terminal status"
               (string= (trace-ref (first emitted) "status") "error")))

(format t "~%== queue and wrapper coverage ==~%")
(let* ((emitted nil)
       (*timing-event-sink* (lambda (payload) (push payload emitted))))
  (let ((context (timing-enqueue-context "web")))
    (sleep 0.01)
    (call-with-timing-enqueue-context
     context
     (lambda ()
       (memory-recall "query" :k 3)
       (memory-search "query" :k 3 :mode :conversation)
       (memory-record-use '("node-1") :consumer "test" :user-visible-p nil)
       (cognitive-call :deep-reflection (list (obj "id" "node-1")))
       (tick-commit-apply
        (obj "tick_type" "explore" "memory_specs" (vector (obj "content" "x")))
        1 :mode :shadow-only)
       (embed-text "query")
       (%conv-persist-write '(a b))
       (%v2-broadcast "final" "private content")
       (values))))
  (let ((trace (first emitted)))
    (trace-check "queue wait captured" (trace-span-by-name trace "turn.queue_wait"))
    (trace-check "memory recall captured" (trace-span-by-name trace "memory.recall"))
    (trace-check "typed memory search captured" (trace-span-by-name trace "memory.search"))
    (trace-check "explicit memory use captured" (trace-span-by-name trace "memory.record_use"))
    (trace-check "cognitive call captured" (trace-span-by-name trace "cognitive.total"))
    (trace-check "tick commit captured" (trace-span-by-name trace "tick.commit"))
    (trace-check "embedding captured" (trace-span-by-name trace "embeddings.query"))
    (trace-check "persistence captured" (trace-span-by-name trace "conversation.persist"))
    (trace-check "broadcast captured" (trace-span-by-name trace "web.broadcast"))
    (trace-check "first public output captured"
                 (trace-span-by-name trace "turn.first_public_output"))
    (trace-check "content absent from serialized trace"
                 (not (search "private content" (shasht:write-json trace nil))))
    (trace-check "unknown attribute keys are dropped"
                 (let ((safe (%timing-safe-attributes
                              (obj "content" "must-not-appear"
                                   "message_count" 2))))
                   (and (null (gethash "content" safe))
                        (= 2 (gethash "message_count" safe)))))))

(format t "~%== reload-safe wrapper installation ==~%")
(let* ((emitted nil)
       (*timing-event-sink* (lambda (payload) (push payload emitted))))
  ;; Reload while our wrapper is still installed: must not wrap itself.
  (load (test-source "observability-tracing.lisp"))
  (call-with-timing-trace (lambda () (memory-recall "after-reload"))
                          :trace-id "trace-reload" :origin "test"
                          :root-span "test.total" :sampled-p t)
  (trace-check "plain tracing reload does not recurse" (= (length emitted) 1))
  ;; Simulate an underlying chain reload, then reload tracing. The new base
  ;; must be captured instead of continuing to call the stale definition.
  (setf (fdefinition 'memory-recall)
        (lambda (&rest args) (declare (ignore args)) (list "new-base")))
  (load (test-source "observability-tracing.lisp"))
  (trace-check "underlying reload is recaptured"
               (equal (memory-recall "x") '("new-base"))))

(format t "~%== model usage is retained without content ==~%")
(let* ((emitted nil)
       (*timing-event-sink* (lambda (payload) (push payload emitted)))
       (original-base (fdefinition 'pai-base-raw-call-model-timing))
       (existing-capture (and (fboundp 'llm-debug-capture-call)
                              (fdefinition 'llm-debug-capture-call)))
       (capture-calls 0)
       (capture-purpose nil))
  (unwind-protect
       (progn
         (setf (fdefinition 'llm-debug-capture-call)
               (lambda (messages purpose thunk)
                 (declare (ignore messages))
                 (incf capture-calls)
                 (setf capture-purpose purpose)
                 (funcall thunk)))
         (setf (fdefinition 'pai-base-raw-call-model-timing)
               (lambda (messages)
                 (declare (ignore messages))
                 (obj "provider" "FixtureProvider" "model" "fixture/model"
                      "service_tier" "standard"
                      "choices" (vector (obj "finish_reason" "stop"
                                             "message" (obj "content" "private reply")))
                      "usage" (obj "prompt_tokens" 120 "completion_tokens" 30
                                   "completion_tokens_details"
                                   (obj "reasoning_tokens" 18)
                                   "total_tokens" 150 "cost" 0.0042d0))))
         (let ((*timing-model-purpose* "public"))
           (call-with-timing-trace
            (lambda () (raw-call-model (list (obj "role" "user" "content" "private prompt"))))
            :trace-id "trace-model-usage" :turn-id "turn-model-usage"
            :origin "test" :root-span "turn.total" :sampled-p t))
         (let* ((trace (first emitted))
                (span (trace-span-by-name trace "model.public_request"))
                (attributes (trace-ref span "attributes"))
                (serialized (shasht:write-json trace nil)))
           (trace-check "public prompt tokens retained" (= 120 (trace-ref attributes "prompt_tokens")))
           (trace-check "public completion tokens retained" (= 30 (trace-ref attributes "completion_tokens")))
           (trace-check "public reasoning tokens retained" (= 18 (trace-ref attributes "reasoning_tokens")))
           (trace-check "public total tokens retained" (= 150 (trace-ref attributes "total_tokens")))
           (trace-check "public model cost retained" (= 0.0042d0 (trace-ref attributes "cost_usd")))
           (trace-check "provider model and finish metadata retained"
                        (and (string= "FixtureProvider" (trace-ref attributes "provider"))
                             (string= "fixture/model" (trace-ref attributes "model"))
                             (string= "standard" (trace-ref attributes "service_tier"))
                             (string= "stop" (trace-ref attributes "finish_reason"))))
           (trace-check "raw provider call passes through debug capture seam"
                        (and (= 1 capture-calls)
                             (string= "public" capture-purpose)))
           (trace-check "model content remains absent"
                        (and (not (search "private prompt" serialized))
                             (not (search "private reply" serialized))))))
    (setf (fdefinition 'pai-base-raw-call-model-timing) original-base)
    (if existing-capture
        (setf (fdefinition 'llm-debug-capture-call) existing-capture)
        (fmakunbound 'llm-debug-capture-call))))

(format t "~%== bounded overhead ==~%")
(defun trace-test-workload ()
  ;; Long enough that timer quantization is small, with no model/network dependency.
  (let ((sum 0))
    (dotimes (i 150000000 sum) (setf sum (logxor sum i)))))

(defun trace-test-elapsed (thunk)
  ;; CPU time excludes host scheduling pauses, which otherwise dominate a
  ;; strict 2% in-memory wrapper gate in shared CI/desktop Docker runtimes.
  (let ((start (get-internal-run-time)))
    (funcall thunk)
    (- (get-internal-run-time) start)))

(let ((pairs nil) (paired-ratios nil)
      (*timing-event-sink* (lambda (payload) (declare (ignore payload)) nil)))
  ;; Warm both paths, then use balanced four-sample blocks. BTTB/TBBT pairing
  ;; cancels gradual host/CPU drift that made a single alternating A/B pair
  ;; fail or pass nondeterministically despite identical code.
  (trace-test-workload)
  (call-with-timing-trace #'trace-test-workload :origin "test"
                          :root-span "overhead.warmup" :sampled-p t)
  (dotimes (iteration 11)
    (let (base-first base-second traced-first traced-second)
      (flet ((measure-base-first ()
               (setf base-first (trace-test-elapsed #'trace-test-workload)))
             (measure-base-second ()
               (setf base-second (trace-test-elapsed #'trace-test-workload)))
             (measure-traced-first ()
               (setf traced-first
                     (trace-test-elapsed
                      (lambda ()
                        (call-with-timing-trace #'trace-test-workload
                                                :origin "test"
                                                :root-span "overhead.total"
                                                :sampled-p t)))))
             (measure-traced-second ()
               (setf traced-second
                     (trace-test-elapsed
                      (lambda ()
                        (call-with-timing-trace #'trace-test-workload
                                                :origin "test"
                                                :root-span "overhead.total"
                                                :sampled-p t))))))
        (if (evenp iteration)
            (progn (measure-base-first) (measure-traced-first)
                   (measure-traced-second) (measure-base-second))
            (progn (measure-traced-first) (measure-base-first)
                   (measure-base-second) (measure-traced-second))))
      (let* ((base (+ base-first base-second))
             (observed (+ traced-first traced-second))
             (ratio (if (zerop base) 999.0d0
                        (/ observed (float base 1.0d0)))))
        (push ratio paired-ratios)
        (push (list ratio base observed) pairs))))
  (setf paired-ratios (sort paired-ratios #'<)
        pairs (sort pairs #'< :key #'first))
  (let* ((median-pair (sixth pairs))
         (ratio (first median-pair))
         (base (second median-pair))
         (observed (third median-pair)))
    (format t "  note median overhead ratio: ~,4f (base ~a ticks, traced ~a ticks)~%"
            ratio base observed)
    (trace-check "median in-memory trace overhead <= 2%" (<= ratio 1.02d0))))

(format t "~%~a passed, ~a failed~%" *trace-test-pass* *trace-test-fail*)
(when (plusp *trace-test-fail*) (sb-ext:exit :code 1))
