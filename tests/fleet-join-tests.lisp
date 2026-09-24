;;;; harness: bare
(require :asdf)
(unless (find-package :ql) (load (or (uiop:getenv "PAI_QUICKLISP_SETUP") "/opt/quicklisp/setup.lisp")))
(asdf:load-asd (merge-pathnames "../pai-fleet.asd" *load-truename*))
(asdf:load-system :pai-fleet)
(in-package :pai.fleet)

(defvar *fjt-checks* 0)
(defun fjt-check (name value)
  (unless value (error "FAIL ~a" name))
  (incf *fjt-checks*) (format t "PASS ~a~%" name))

(defun fjt-signals-join-error-p (thunk &optional reason-substring)
  "T if THUNK signals a FLEET-JOIN-ERROR whose reason contains
REASON-SUBSTRING (or any FLEET-JOIN-ERROR at all, if REASON-SUBSTRING is
omitted); :WRONG-CONDITION-TYPE if it signals some other error; NIL if it
does not signal at all. Coerces SEARCH's index result to T -- a match at
position 0 must not read as false."
  (handler-case (progn (funcall thunk) nil)
    (fleet-join-error (condition)
      (if (or (null reason-substring)
              (search reason-substring (fleet-join-error-reason condition)))
          t
          nil))
    (error () :wrong-condition-type)))

(defun fjt-scratch-store-path (name)
  (let ((path (merge-pathnames name (uiop:temporary-directory))))
    (ignore-errors (delete-file path))
    path))

;;; --- fleet-join-code -------------------------------------------------------

(let ((codes (loop repeat 20 collect (fleet-join-code))))
  (fjt-check "every code is exactly 6 characters"
             (every (lambda (c) (= 6 (length c))) codes))
  (fjt-check "every code is all digits"
             (every (lambda (c) (every #'digit-char-p c)) codes))
  (fjt-check "20 codes are not all identical (sanity on the RNG)"
             (plusp (length (remove-duplicates codes :test #'string=)))))

;;; --- inbound join store ------------------------------------------------

(let ((store (make-fleet-inbound-join-store)))
  (fjt-check "an unregistered requester has no pending request"
             (null (fleet-inbound-join-find store "agent-two")))
  (fleet-inbound-join-register
   store :request-id "req-1" :requester-id "agent-two"
         :requester-name "AgentTwo" :requester-address "10.0.0.2:8081"
         :code "048213" :received-at 1000)
  (let ((found (fleet-inbound-join-find store "agent-two" :now 1000)))
    (fjt-check "a registered request is found"
               (and found (string= "048213" (fleet-join-request-code found))))
    (fjt-check "the requester name round-trips"
               (string= "AgentTwo" (fleet-join-request-requester-name found))))
  (fjt-check "the request is gone once past the TTL"
             (null (fleet-inbound-join-find
                    store "agent-two"
                    :now (+ 1000 +fleet-join-request-ttl-seconds+ 1))))
  (fjt-check "the request is still present exactly at the TTL boundary"
             (fleet-inbound-join-find
              store "agent-two" :now (+ 1000 +fleet-join-request-ttl-seconds+)))
  ;; A second request from the same requester refreshes, not duplicates.
  (fleet-inbound-join-register
   store :request-id "req-2" :requester-id "agent-two"
         :requester-name "AgentTwo" :requester-address "10.0.0.2:8081"
         :code "999999" :received-at 2000)
  (fjt-check "re-registering the same requester replaces the pending code"
             (string= "999999"
                      (fleet-join-request-code
                       (fleet-inbound-join-find store "agent-two" :now 2000))))
  (fleet-inbound-join-remove store "agent-two")
  (fjt-check "remove clears the pending request"
             (null (fleet-inbound-join-find store "agent-two" :now 2000))))

(let ((store (make-fleet-inbound-join-store)))
  (fleet-inbound-join-register store :request-id "r1" :requester-id "a"
                                      :requester-name "A" :requester-address "a:1"
                                      :code "111111" :received-at 1900)
  (fleet-inbound-join-register store :request-id "r2" :requester-id "b"
                                      :requester-name "B" :requester-address "b:1"
                                      :code "222222" :received-at 2000)
  (fleet-inbound-join-register store :request-id "r3" :requester-id "c"
                                      :requester-name "C" :requester-address "c:1"
                                      :code "333333"
                                      :received-at (- 2000 +fleet-join-request-ttl-seconds+ 100))
  (let ((pending (fleet-inbound-join-prune store :now 2000)))
    (fjt-check "prune drops the expired entry and keeps the live ones"
               (= 2 (length pending)))
    (fjt-check "prune returns the survivors oldest first"
               (and (string= "a" (fleet-join-request-requester-id (first pending)))
                    (string= "b" (fleet-join-request-requester-id (second pending)))))
    (fjt-check "prune actually removed the expired entry from the store"
               (null (fleet-inbound-join-find store "c" :now 2000)))))

;;; --- outbound join store -----------------------------------------------

(let ((store (make-fleet-outbound-join-store)))
  (fjt-check "an unknown request-id has no pending outbound request"
             (null (fleet-outbound-join-find store "req-x")))
  (fleet-outbound-join-register store :request-id "req-x" :target-address "a:1"
                                       :code "123456" :sent-at 1000)
  (fjt-check "a registered outbound request is found by its request-id"
             (fleet-outbound-join-find store "req-x" :now 1000))
  (fjt-check "an outbound request also expires"
             (null (fleet-outbound-join-find
                    store "req-x" :now (+ 1000 +fleet-join-request-ttl-seconds+ 1))))
  (fleet-outbound-join-remove store "req-x")
  (fjt-check "remove clears the outbound request"
             (null (fleet-outbound-join-find store "req-x" :now 1000))))

;;; --- fleet-join-approve --------------------------------------------------

(defun fjt-fresh-fleet-store (path-name own-id)
  (fleet-store-load (fjt-scratch-store-path path-name) (lambda () own-id)))

(let* ((fleet-store (fjt-fresh-fleet-store "fleet-join-test-a.sexp" "agent-one"))
       (inbound (make-fleet-inbound-join-store)))
  (fleet-inbound-join-register
   inbound :request-id "req-1" :requester-id "agent-two"
           :requester-name "AgentTwo" :requester-address "10.0.0.2:8081"
           :code "048213" :received-at 1000)
  (multiple-value-bind (peer accept-plist)
      (fleet-join-approve inbound fleet-store "agent-two" "048213"
                           :own-id "agent-one" :own-name "AgentOne"
                           :own-address "10.0.0.1:8081" :now 1000)
    (fjt-check "approval returns the newly created peer"
               (string= "agent-two" (fleet-peer-id peer)))
    (fjt-check "the peer is persisted in the fleet store"
               (fleet-store-peer fleet-store "agent-two"))
    (fjt-check "the accept plist names the original request-id"
               (string= "req-1" (getf accept-plist :request-id)))
    (fjt-check "the accept plist carries this agent's own identity"
               (and (string= "agent-one" (getf accept-plist :acceptor-id))
                    (string= "AgentOne" (getf accept-plist :acceptor-name))
                    (string= "10.0.0.1:8081" (getf accept-plist :acceptor-address))))
    (fjt-check "the accept plist's shared secret matches what was persisted"
               (string= (getf accept-plist :shared-secret)
                        (ironclad:byte-array-to-hex-string
                         (fleet-peer-shared-secret
                          (fleet-store-peer fleet-store "agent-two"))))))
  (fjt-check "the inbound request is consumed after approval"
             (null (fleet-inbound-join-find inbound "agent-two" :now 1000))))

(let* ((fleet-store (fjt-fresh-fleet-store "fleet-join-test-b.sexp" "agent-one"))
       (inbound (make-fleet-inbound-join-store)))
  (fleet-inbound-join-register
   inbound :request-id "req-1" :requester-id "agent-two"
           :requester-name "AgentTwo" :requester-address "10.0.0.2:8081"
           :code "048213" :received-at 1000)
  (fjt-check "the wrong code is refused with a join-error"
             (eq t (fjt-signals-join-error-p
                    (lambda ()
                      (fleet-join-approve inbound fleet-store "agent-two" "000000"
                                           :own-id "agent-one" :own-name "AgentOne"
                                           :own-address "a:1" :now 1000))
                    "code does not match")))
  (fjt-check "a wrong-code attempt does not persist a peer"
             (null (fleet-store-peer fleet-store "agent-two")))
  (fjt-check "a wrong-code attempt leaves the request pending for a retry"
             (fleet-inbound-join-find inbound "agent-two" :now 1000)))

(let* ((fleet-store (fjt-fresh-fleet-store "fleet-join-test-c.sexp" "agent-one"))
       (inbound (make-fleet-inbound-join-store)))
  (fjt-check "approving a nonexistent request is refused with a join-error"
             (eq t (fjt-signals-join-error-p
                    (lambda ()
                      (fleet-join-approve inbound fleet-store "nobody" "048213"
                                           :own-id "agent-one" :own-name "AgentOne"
                                           :own-address "a:1" :now 1000))
                    "no pending request")))
  (fleet-inbound-join-register
   inbound :request-id "req-1" :requester-id "agent-two"
           :requester-name "AgentTwo" :requester-address "10.0.0.2:8081"
           :code "048213" :received-at 1000)
  (fjt-check "approving an expired request is refused even with the right code"
             (eq t (fjt-signals-join-error-p
                    (lambda ()
                      (fleet-join-approve
                       inbound fleet-store "agent-two" "048213"
                       :own-id "agent-one" :own-name "AgentOne" :own-address "a:1"
                       :now (+ 1000 +fleet-join-request-ttl-seconds+ 1)))
                    "no pending request")))
  (fjt-check "approving self (requester-id = own-id) is always refused"
             (eq t (fjt-signals-join-error-p
                    (lambda ()
                      (fleet-join-approve inbound fleet-store "agent-one" "048213"
                                           :own-id "agent-one" :own-name "AgentOne"
                                           :own-address "a:1" :now 1000))
                    "cannot join self")))
  (fjt-check "missing own identity fields is a plain error, not a join-error"
             (eq :wrong-condition-type
                 (fjt-signals-join-error-p
                  (lambda ()
                    (fleet-join-approve inbound fleet-store "agent-two" "048213"
                                         :own-id "" :own-name "AgentOne"
                                         :own-address "a:1" :now 1000))))))

;;; --- fleet-join-accept-apply ---------------------------------------------

(let* ((fleet-store (fjt-fresh-fleet-store "fleet-join-test-d.sexp" "agent-two"))
       (outbound (make-fleet-outbound-join-store))
       (real-secret (fleet-shared-secret)))
  (fleet-outbound-join-register outbound :request-id "req-1" :target-address "a:1"
                                          :code "048213" :sent-at 1000)
  (let ((peer (fleet-join-accept-apply
               outbound fleet-store "req-1"
               :acceptor-id "agent-one" :acceptor-name "AgentOne"
               :acceptor-address "10.0.0.1:8081"
               :shared-secret-hex (ironclad:byte-array-to-hex-string real-secret)
               :now 1000)))
    (fjt-check "applying a genuine accept returns the new peer"
               (string= "agent-one" (fleet-peer-id peer)))
    (fjt-check "the peer's shared secret matches exactly what was sent"
               (equalp real-secret (fleet-peer-shared-secret peer)))
    (fjt-check "the peer is persisted in the fleet store"
               (fleet-store-peer fleet-store "agent-one")))
  (fjt-check "the outbound request is consumed after a successful accept"
             (null (fleet-outbound-join-find outbound "req-1" :now 1000))))

(let* ((fleet-store (fjt-fresh-fleet-store "fleet-join-test-e.sexp" "agent-two"))
       (outbound (make-fleet-outbound-join-store)))
  (fjt-check "an unsolicited join-accept (no matching outbound request) is refused"
             (eq t (fjt-signals-join-error-p
                    (lambda ()
                      (fleet-join-accept-apply
                       outbound fleet-store "no-such-request"
                       :acceptor-id "agent-one" :acceptor-name "AgentOne"
                       :acceptor-address "a:1"
                       :shared-secret-hex (ironclad:byte-array-to-hex-string
                                           (fleet-shared-secret))
                       :now 1000))
                    "does not correlate")))
  (fjt-check "no peer is persisted from an unsolicited accept"
             (null (fleet-store-peer fleet-store "agent-one"))))

(let* ((fleet-store (fjt-fresh-fleet-store "fleet-join-test-f.sexp" "agent-two"))
       (outbound (make-fleet-outbound-join-store)))
  (fleet-outbound-join-register outbound :request-id "req-1" :target-address "a:1"
                                          :code "048213" :sent-at 1000)
  (fjt-check "an expired outbound request refuses the accept"
             (eq t (fjt-signals-join-error-p
                    (lambda ()
                      (fleet-join-accept-apply
                       outbound fleet-store "req-1"
                       :acceptor-id "agent-one" :acceptor-name "AgentOne"
                       :acceptor-address "a:1"
                       :shared-secret-hex (ironclad:byte-array-to-hex-string
                                           (fleet-shared-secret))
                       :now (+ 1000 +fleet-join-request-ttl-seconds+ 1)))
                    "does not correlate"))))

(format t "~%FLEET JOIN TESTS: ~a checks passed.~%" *fjt-checks*)
