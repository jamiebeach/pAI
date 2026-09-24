;;;; harness: bare
(require :asdf)
(unless (find-package :ql) (load (or (uiop:getenv "PAI_QUICKLISP_SETUP") "/opt/quicklisp/setup.lisp")))
(asdf:load-asd (merge-pathnames "../pai-fleet.asd" *load-truename*))
(asdf:load-system :pai-fleet)
(in-package :pai.fleet)

(defvar *fit-checks* 0)
(defun fit-check (name value)
  (unless value (error "FAIL ~a" name))
  (incf *fit-checks*) (format t "PASS ~a~%" name))

(defun fit-signals-p (thunk)
  (handler-case (progn (funcall thunk) nil) (error () t)))

;;; --- fleet-uuid4 ------------------------------------------------------

(let ((a (fleet-uuid4)) (b (fleet-uuid4)))
  (fit-check "a uuid is 36 characters" (= 36 (length a)))
  (fit-check "hyphens land at the standard positions"
             (and (char= #\- (char a 8)) (char= #\- (char a 13))
                  (char= #\- (char a 18)) (char= #\- (char a 23))))
  (fit-check "the version nibble is 4" (char= #\4 (char a 14)))
  (fit-check "the variant nibble is one of 8/9/a/b"
             (find (char a 19) "89ab"))
  (fit-check "two calls produce different ids" (not (string= a b))))

;; The real regression this guards against cannot be exercised by two
;; sequential calls in one process -- *RANDOM-STATE* mutates after any
;; call, so two calls in the same image always differ regardless of which
;; source is used. The actual bug (found live, two agents launched
;; together generated the identical id) was two FRESH processes each
;; drawing their first-ever random bytes from SBCL's default
;; *RANDOM-STATE*, which is baked into the core image and not reseeded
;; from OS entropy at startup -- so two images performing the same
;; operations in the same order before their first RANDOM call draw the
;; exact same bytes. IRONCLAD:RANDOM-DATA (FLEET-UUID4's actual source)
;; does not have this failure mode; there is no single-process unit test
;; for "two separate processes," only the fix and this note.

;;; --- make-fleet-peer validation ----------------------------------------

(defun fit-secret () (ironclad:random-data 32))

(fit-check "a well-formed peer constructs"
           (fleet-peer-p
            (make-fleet-peer :id "peer-1" :name "AgentOne"
                              :address "100.0.0.1:8081"
                              :shared-secret (fit-secret) :joined-at 1)))
(fit-check "an empty id is refused"
           (fit-signals-p
            (lambda () (make-fleet-peer :id "" :name "AgentOne"
                                        :address "a:1" :shared-secret (fit-secret)
                                        :joined-at 1))))
(fit-check "an empty name is refused"
           (fit-signals-p
            (lambda () (make-fleet-peer :id "peer-1" :name ""
                                        :address "a:1" :shared-secret (fit-secret)
                                        :joined-at 1))))
(fit-check "an empty address is refused"
           (fit-signals-p
            (lambda () (make-fleet-peer :id "peer-1" :name "AgentOne"
                                        :address "" :shared-secret (fit-secret)
                                        :joined-at 1))))
(fit-check "a non-byte-vector shared-secret is refused"
           (fit-signals-p
            (lambda () (make-fleet-peer :id "peer-1" :name "AgentOne"
                                        :address "a:1" :shared-secret "not-bytes"
                                        :joined-at 1))))
(fit-check "a non-integer joined-at is refused"
           (fit-signals-p
            (lambda () (make-fleet-peer :id "peer-1" :name "AgentOne"
                                        :address "a:1" :shared-secret (fit-secret)
                                        :joined-at "now"))))

;;; --- fleet-store-load / fleet-store-save round-trip --------------------

(defun fit-scratch-path (name)
  "A scratch path under the OS temp directory, guaranteed absent before the
caller uses it. SBCL's default *RANDOM-STATE* is deterministic per fresh
process, so a name built from (RANDOM N) alone is identical on every fresh
invocation and collides with a leftover file from an earlier test run --
deleting first, not just naming uniquely, is what actually makes each run
independent."
  (let ((path (merge-pathnames name (uiop:temporary-directory))))
    (ignore-errors (delete-file path))
    path))

;; Reload a store while it still has zero peers -- the exact case a real
;; live launch found broken: a fresh store's :peers is an empty list, and
;; NIL read back through a reader package that does not inherit
;; COMMON-LISP is a distinct, useless symbol rather than the real empty
;; list, crashing the DOLIST in FLEET-STORE-LOAD. Every other round-trip
;; test in this file happens to add a peer before ever reloading, so this
;; case must be exercised on its own or it silently regresses again.
(let* ((path (fit-scratch-path
              (format nil "fleet-identity-test-zeropeer-~a.sexp" (random 1000000))))
       (generated-id nil))
  (fleet-store-load path (lambda () (setf generated-id (fleet-uuid4))))
  (let ((reloaded (fleet-store-load path (lambda () (error "must not regenerate an id")))))
    (fit-check "a zero-peer store reloads without error"
               (equal generated-id (fleet-store-agent-id reloaded)))
    (fit-check "a zero-peer store reloads with an empty (not broken) peer table"
               (zerop (hash-table-count (fleet-store-peers reloaded))))))

(let* ((path (fit-scratch-path
              (format nil "fleet-identity-test-~a.sexp" (random 1000000))))
       (generated-id nil)
       (store (fleet-store-load path (lambda ()
                                        (setf generated-id (fleet-uuid4))))))
  (fit-check "a fresh store gets an id from agent-id-fn"
             (equal generated-id (fleet-store-agent-id store)))
  (fit-check "a fresh store is persisted immediately" (probe-file path))
  (fit-check "a fresh store starts with no peers"
             (zerop (hash-table-count (fleet-store-peers store))))

  (let* ((secret (fit-secret))
         (peer (make-fleet-peer :id "peer-1" :name "AgentOne"
                                 :address "100.64.0.1:8081"
                                 :shared-secret secret :joined-at 3998000000)))
    (fleet-store-add-peer store peer)
    (fit-check "the peer is retrievable by id"
               (eq peer (fleet-store-peer store "peer-1")))
    (fit-check "an unknown peer id returns nil"
               (null (fleet-store-peer store "no-such-peer")))

    (let ((reloaded (fleet-store-load
                      path (lambda () (error "must not regenerate an id")))))
      (fit-check "reloading preserves the agent id"
                  (equal generated-id (fleet-store-agent-id reloaded)))
      (fit-check "reloading preserves the peer count"
                  (= 1 (hash-table-count (fleet-store-peers reloaded))))
      (let ((reloaded-peer (fleet-store-peer reloaded "peer-1")))
        (fit-check "reloaded peer name round-trips"
                    (equal "AgentOne" (fleet-peer-name reloaded-peer)))
        (fit-check "reloaded peer address round-trips"
                    (equal "100.64.0.1:8081" (fleet-peer-address reloaded-peer)))
        (fit-check "reloaded peer joined-at round-trips"
                    (= 3998000000 (fleet-peer-joined-at reloaded-peer)))
        (fit-check "reloaded peer shared-secret round-trips byte-for-byte"
                    (equalp secret (fleet-peer-shared-secret reloaded-peer))))))

  (fit-check "no leftover temp file survives a successful save"
             (not (probe-file
                   (make-pathname
                    :name (concatenate 'string (pathname-name path) "-tmp")
                    :type (pathname-type path) :defaults path)))))

;;; --- multiple peers ------------------------------------------------------

(let* ((path (fit-scratch-path
              (format nil "fleet-identity-test-multi-~a.sexp" (random 1000000))))
       (store (fleet-store-load path #'fleet-uuid4)))
  (fleet-store-add-peer store (make-fleet-peer :id "peer-a" :name "A"
                                                :address "a:1"
                                                :shared-secret (fit-secret)
                                                :joined-at 1))
  (fleet-store-add-peer store (make-fleet-peer :id "peer-b" :name "B"
                                                :address "b:1"
                                                :shared-secret (fit-secret)
                                                :joined-at 2))
  (let ((reloaded (fleet-store-load path (lambda () (error "unreachable")))))
    (fit-check "both peers survive a reload"
               (= 2 (hash-table-count (fleet-store-peers reloaded))))
    (fit-check "each peer keeps its own identity after reload"
               (and (equal "A" (fleet-peer-name (fleet-store-peer reloaded "peer-a")))
                    (equal "B" (fleet-peer-name (fleet-store-peer reloaded "peer-b")))))))

;;; --- outbound-thread-id (FLEET_DESIGN.md S9 back-and-forth) --------------

(let* ((secret (fit-secret))
       (peer (make-fleet-peer :id "peer-1" :name "AgentOne"
                               :address "100.64.0.1:8081"
                               :shared-secret secret :joined-at 3998000000)))
  (fit-check "a freshly constructed peer has no outbound thread yet"
             (null (fleet-peer-outbound-thread-id peer))))

(fit-check "make-fleet-peer rejects an empty-string outbound-thread-id"
           (fit-signals-p
            (lambda ()
              (make-fleet-peer :id "peer-1" :name "AgentOne"
                                :address "100.64.0.1:8081"
                                :shared-secret (fit-secret) :joined-at 1
                                :outbound-thread-id ""))))

(let* ((path (fit-scratch-path
              (format nil "fleet-identity-test-thread-~a.sexp" (random 1000000))))
       (store (fleet-store-load path #'fleet-uuid4))
       (peer (make-fleet-peer :id "peer-1" :name "AgentOne"
                               :address "100.64.0.1:8081"
                               :shared-secret (fit-secret) :joined-at 1)))
  (fleet-store-add-peer store peer)
  (fleet-store-set-peer-outbound-thread-id store "peer-1" "thread-abc")
  (fit-check "setting the outbound thread id updates the live peer"
             (equal "thread-abc" (fleet-peer-outbound-thread-id
                                   (fleet-store-peer store "peer-1"))))
  (fit-check "setting an unknown peer's outbound thread id signals, not silently no-ops"
             (fit-signals-p
              (lambda ()
                (fleet-store-set-peer-outbound-thread-id
                 store "no-such-peer" "thread-xyz"))))
  (let ((reloaded (fleet-store-load path (lambda () (error "unreachable")))))
    (fit-check "the outbound thread id round-trips through a reload"
               (equal "thread-abc" (fleet-peer-outbound-thread-id
                                     (fleet-store-peer reloaded "peer-1"))))))

;;; --- schema version guard ------------------------------------------------

(let ((path (fit-scratch-path
             (format nil "fleet-identity-test-badversion-~a.sexp" (random 1000000)))))
  (with-open-file (stream path :direction :output :if-exists :supersede
                                :if-does-not-exist :create :external-format :utf-8)
    (prin1 (list :schema-version 999 :agent-id "x" :peers nil) stream))
  (fit-check "an unsupported schema version is refused, not silently misread"
             (fit-signals-p
              (lambda () (fleet-store-load path (lambda () (error "unreachable")))))))

(format t "~%FLEET IDENTITY TESTS: ~a checks passed.~%" *fit-checks*)
