(in-package :agent)

(ql:quickload '(:bordeaux-threads :postmodern :shasht) :silent t)

(defvar *r0c3-pass* 0)
(defvar *r0c3-fail* 0)
(defvar *r0c3-events* nil)
(defvar *r0c3-real-log-event* nil)

(defun r0c3-check (name condition)
  (if condition
      (progn (incf *r0c3-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *r0c3-fail*) (format t "  FAIL ~a~%" name))))

(defun r0c3-signals-p (thunk)
  (handler-case (progn (funcall thunk) nil)
    (error () t)))

(defun r0c3-log-event (type payload &key caused-by)
  (declare (ignore caused-by))
  (push (list type payload) *r0c3-events*)
  (length *r0c3-events*))

(defun r0c3-projection-events ()
  (remove-if-not (lambda (event) (string= "projection-state" (first event)))
                 (reverse *r0c3-events*)))

(defun r0c3-projection-payload (projection)
  (second
   (find projection (r0c3-projection-events)
         :key (lambda (event) (gethash "projection" (second event)))
         :test #'string= :from-end t)))

(defun r0c3-write-exact (path content)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                            :if-does-not-exist :create :external-format :utf-8)
    (write-string content out)))

;; Load the real common event helper, then capture its LOG-EVENT calls in
;; memory so the fixture never writes an event ledger.
(unless (fboundp 'auto-turn)
  (setf (fdefinition 'auto-turn) (lambda (prompt) prompt)))
(unless (fboundp 'execute)
  (setf (fdefinition 'execute) (lambda (tool-call) tool-call)))
(unless (fboundp 'propose-loop)
  (setf (fdefinition 'propose-loop) (lambda (source) source)))
(unless (fboundp 'raw-call-model)
  (setf (fdefinition 'raw-call-model)
        (lambda (&rest arguments)
          (declare (ignore arguments))
          (obj "choices" (vector (obj "message" (obj "content" "")))))))
(load (test-source "event-log.lisp"))
(setf *r0c3-real-log-event* (fdefinition 'log-event))
(setf (fdefinition 'log-event) #'r0c3-log-event)

;; Minimal load-time dependencies for the stateful modules. These stubs never
;; execute behavior under test; they only satisfy established wrapper seams.
(unless (fboundp '%tick-handle-light-consolidate)
  (setf (fdefinition '%tick-handle-light-consolidate) (lambda () nil)))
(unless (fboundp '%tick-handle-full-reflection)
  (setf (fdefinition '%tick-handle-full-reflection) (lambda () nil)))
(unless (fboundp '%drives-event-initiate)
  (setf (fdefinition '%drives-event-initiate)
        (lambda (&rest arguments) (declare (ignore arguments)) nil)))
(unless (fboundp 'pai-timezone-name)
  (setf (fdefinition 'pai-timezone-name) (lambda () "UTC")))
(unless (fboundp 'pai-cron-next-fire)
  (setf (fdefinition 'pai-cron-next-fire)
        (lambda (&rest arguments) (declare (ignore arguments)) 4102444800)))

(load (test-source "modulator.lisp"))
(modulator-decay-stop 0)
(load (test-source "drives.lisp"))
(drives-stop 0)
(load (test-source "initiative-engine.lisp"))
(load (test-source "ambient-recall-diversity.lisp"))
(load (test-source "scheduler.lisp"))
(load (test-source "public-outbound-gateway.lisp"))

(setf *r0c3-events* nil)

(let* ((root (merge-pathnames "r0c3-stateful-projections/" (test-state-dir)))
       (modulators (merge-pathnames "modulators.json" root))
       (drives (merge-pathnames "drives.json" root))
       (contact (merge-pathnames "contact-log.json" root))
       (ambient (merge-pathnames "ambient-recall-history.json" root))
       (schedules (merge-pathnames "schedules.json" root))
       (scheduled-context (merge-pathnames "scheduled-context.json" root))
       (outbound (merge-pathnames "public-outbound-audit.json" root))
       (paths
         `(("modulators" . ,modulators)
           ("drives" . ,drives)
           ("contact-log" . ,contact)
           ("ambient-recall-history" . ,ambient)
           ("schedules" . ,schedules)
           ("scheduled-context" . ,scheduled-context)
           ("public-outbound-audit" . ,outbound))))
  (let ((ledger (merge-pathnames "roundtrip-events.jsonl" root)))
    (when (probe-file ledger) (delete-file ledger)))
  (ensure-directories-exist modulators)
  (let ((*modulators-file* modulators)
        (*modulators*
          (obj "arousal"
               (obj "current" 0.25 "baseline" 0.3 "decay_rate" 0.05
                    "min" 0.0 "max" 1.0))))
    (save-modulators))
  (let ((*drives-file* drives)
        (*drives*
          (obj "curiosity"
               (obj "current" 0.75 "baseline" 0.4 "mode" "event"
                    "target" "synthetic target"))))
    (save-drives))
  (let ((*contact-log-file* contact)
        (*contact-log* '(4102444000 4102440000)))
    (save-contact-log))
  (let ((*ambient-history-file* ambient)
        (*ambient-history* '(("synthetic-node-a" . 4102444000)
                             ("synthetic-node-b" . 4102440000))))
    (save-ambient-history))
  (let ((*pai-schedules-file* schedules)
        (*pai-scheduled-context-file* scheduled-context)
        (*pai-schedules* (make-hash-table :test #'equal))
        (*pai-scheduled-context*
          (list (obj "id" "context-1" "text" "synthetic context"
                     "consumed_at_utc" :null))))
    (setf (gethash "schedule-1" *pai-schedules*)
          (obj "id" "schedule-1" "kind" "once" "status" "active"
               "text" "synthetic reminder" "next_fire_utc" 4102444800))
    (%scheduler-save-jobs)
    (%scheduler-save-context))
  (let ((*public-outbound-audit-file* outbound)
        (*public-outbound-records*
          (list (obj "schema_version" 2
                     "canonical_public_act_id" "synthetic-act-1"
                     "transport_status" "synthetic-returned"))))
    (%public-outbound-save))

  (format t "~%== exact persisted state events ==~%")
  (r0c3-check "all seven projections emit exactly once"
               (= 7 (length (r0c3-projection-events))))
  (dolist (entry paths)
    (let* ((projection (car entry))
           (path (cdr entry))
           (payload (r0c3-projection-payload projection))
           (content (and payload (gethash "content" payload))))
      (r0c3-check
       (format nil "~a event is rebuild-complete and byte-exact" projection)
       (and payload
            (string= "replace" (gethash "operation" payload))
            (string= "utf-8" (gethash "encoding" payload))
            (string= (file-namestring path) (gethash "file" payload))
            (string= (uiop:read-file-string path) content)))))
  (r0c3-check "legacy newline conventions are preserved"
               (and
                (char= #\Newline
                       (char (gethash "content"
                                      (r0c3-projection-payload "schedules"))
                             (1- (length
                                  (gethash
                                   "content"
                                   (r0c3-projection-payload "schedules"))))))
                (char= #\Newline
                       (char (gethash
                              "content"
                              (r0c3-projection-payload
                               "public-outbound-audit"))
                             (1- (length
                                  (gethash
                                   "content"
                                   (r0c3-projection-payload
                                    "public-outbound-audit"))))))
                (not (char= #\Newline
                            (char (gethash
                                   "content"
                                   (r0c3-projection-payload "drives"))
                                  (1- (length
                                       (gethash
                                        "content"
                                        (r0c3-projection-payload
                                         "drives")))))))))

  (format t "~%== event-only reconstruction ==~%")
  (r0c3-check
   "latest projection events reconstruct all seven files byte-identically"
   (every
    (lambda (entry)
      (let* ((projection (car entry))
             (source (cdr entry))
             (rebuilt
               (merge-pathnames
                (format nil "rebuilt/~a.json" projection) root))
             (content
               (gethash "content" (r0c3-projection-payload projection))))
        (r0c3-write-exact rebuilt content)
        (string= (uiop:read-file-string source)
                 (uiop:read-file-string rebuilt))))
    paths))

  (let* ((ledger (merge-pathnames "roundtrip-events.jsonl" root))
         (*event-log-file* ledger)
         (*event-next-id* 0)
         (*event-ring* nil)
         (roundtrip-content
           (concatenate 'string "synthetic unicode: π" (string #\Newline)))
         (saved (fdefinition 'log-event)))
    (unwind-protect
         (progn
           (setf (fdefinition 'log-event) *r0c3-real-log-event*)
           (log-projection-state "roundtrip" #P"roundtrip.json"
                                 roundtrip-content)
           (let* ((events (replay-events))
                  (event (first events))
                  (payload (and event (gethash "payload" event))))
             (r0c3-check
              "real JSONL append/replay preserves exact projection content"
              (and (= 1 (length events))
                   (string= "projection-state" (gethash "type" event))
                   (string= roundtrip-content
                            (gethash "content" payload))))))
      (setf (fdefinition 'log-event) saved)))

  (format t "~%== failure and read-only boundaries ==~%")
  (let ((before (length (r0c3-projection-events)))
        (*drives-file*
          #P"/tmp/r0c3-parent-does-not-exist/child/drives.json")
        (*drives* (obj "synthetic" (obj "current" 0.1))))
    (r0c3-check "failed file write signals"
                 (r0c3-signals-p #'save-drives))
    (r0c3-check "failed file write emits no projection event"
                 (= before (length (r0c3-projection-events)))))

  (let ((saved (fdefinition 'log-event))
        (result :not-called)
        (*drives-file* (merge-pathnames "sink-failure-drives.json" root))
        (*drives* (obj "synthetic" (obj "current" 0.2))))
    (unwind-protect
         (progn
           (setf (fdefinition 'log-event)
                 (lambda (&rest arguments)
                   (declare (ignore arguments))
                   (error "synthetic event sink failure")))
           (setf result (save-drives)))
      (setf (fdefinition 'log-event) saved))
    (r0c3-check "event-sink failure cannot alter successful file save"
                 (and (not (eq result :not-called))
                      (probe-file *drives-file*)
                      (plusp (length (uiop:read-file-string
                                      *drives-file*))))))

  (setf *r0c3-events* nil)
  (let ((*modulators-file* modulators)
        (*drives-file* drives)
        (*contact-log-file* contact)
        (*ambient-history-file* ambient)
        (*pai-schedules-file* schedules)
        (*pai-scheduled-context-file* scheduled-context)
        (*public-outbound-audit-file* outbound)
        (*public-outbound-records* nil))
    (load-modulators)
    (load-drives)
    (load-contact-log)
    (load-ambient-history)
    (%scheduler-load)
    (%public-outbound-load))
  (r0c3-check "load paths emit no projection events"
               (null (r0c3-projection-events))))

(format t "~%R0c3 stateful projection events: ~a passed, ~a failed.~%"
        *r0c3-pass* *r0c3-fail*)
(when (plusp *r0c3-fail*)
  (error "R0c3 stateful projection event tests failed"))
