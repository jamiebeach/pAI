;;;; harness: bare
(require :asdf)
(unless (find-package :ql) (load (or (uiop:getenv "PAI_QUICKLISP_SETUP") "/opt/quicklisp/setup.lisp")))
(asdf:load-asd (merge-pathnames "../pai-fleet.asd" *load-truename*))
(asdf:load-system :pai-fleet)
(in-package :pai.fleet)

(defvar *fat-checks* 0)
(defun fat-check (name value)
  (unless value (error "FAIL ~a" name))
  (incf *fat-checks*) (format t "PASS ~a~%" name))

(defun fat-signals-p (thunk)
  (handler-case (progn (funcall thunk) nil) (error () t)))

(defparameter *fat-secret-a* (ironclad:random-data 32))
(defparameter *fat-secret-b* (ironclad:random-data 32))

;;; --- fleet-hmac-sign -----------------------------------------------------

(fat-check "signing is deterministic for the same inputs"
           (string= (fleet-hmac-sign *fat-secret-a* 1000 "hello")
                    (fleet-hmac-sign *fat-secret-a* 1000 "hello")))
(fat-check "a different secret produces a different signature"
           (not (string= (fleet-hmac-sign *fat-secret-a* 1000 "hello")
                         (fleet-hmac-sign *fat-secret-b* 1000 "hello"))))
(fat-check "a different timestamp produces a different signature"
           (not (string= (fleet-hmac-sign *fat-secret-a* 1000 "hello")
                         (fleet-hmac-sign *fat-secret-a* 1001 "hello"))))
(fat-check "a different body produces a different signature"
           (not (string= (fleet-hmac-sign *fat-secret-a* 1000 "hello")
                         (fleet-hmac-sign *fat-secret-a* 1000 "goodbye"))))
(fat-check "the canonical delimiter does not let a body forge a timestamp shift"
           ;; "1." + "23hello" must not equal "12." + "3hello": the decimal
           ;; timestamp followed by a literal "." is unambiguous.
           (not (string= (fleet-hmac-sign *fat-secret-a* 1 "23hello")
                         (fleet-hmac-sign *fat-secret-a* 12 "3hello"))))
(fat-check "signing a non-integer timestamp is refused"
           (fat-signals-p (lambda () (fleet-hmac-sign *fat-secret-a* "1000" "hello"))))
(fat-check "unicode body content signs and is internally consistent"
           (string= (fleet-hmac-sign *fat-secret-a* 1000 "héllo 🖤")
                    (fleet-hmac-sign *fat-secret-a* 1000 "héllo 🖤")))

;;; --- fleet-hmac-verify -----------------------------------------------------

;; A bodyless request (a bare GET, or a POST with nothing to send) signs
;; and verifies over an empty string. This is the exact case a real
;; smoke test against a running acceptor found broken on the Hunchentoot
;; adapter side (HUNCHENTOOT:RAW-POST-DATA returns NIL, not "", for no
;; body) -- worth a standing regression test at this pure layer too, since
;; the fix belongs to the caller's NIL-to-"" coercion, not to this function.
(let ((sig (fleet-hmac-sign *fat-secret-a* 1000 "")))
  (fat-check "an empty body signs and verifies"
             (fleet-hmac-verify *fat-secret-a* 1000 "" sig))
  (fat-check "an empty-body signature does not verify against a non-empty body"
             (not (fleet-hmac-verify *fat-secret-a* 1000 "x" sig))))

(let ((sig (fleet-hmac-sign *fat-secret-a* 1000 "hello")))
  (fat-check "a genuine signature verifies"
             (fleet-hmac-verify *fat-secret-a* 1000 "hello" sig))
  (fat-check "a tampered body fails verification"
             (not (fleet-hmac-verify *fat-secret-a* 1000 "hellx" sig)))
  (fat-check "a tampered timestamp fails verification"
             (not (fleet-hmac-verify *fat-secret-a* 1001 "hello" sig)))
  (fat-check "the wrong secret fails verification"
             (not (fleet-hmac-verify *fat-secret-b* 1000 "hello" sig)))
  (fat-check "a malformed (non-hex) signature fails cleanly, not with an error"
             (eq nil (fleet-hmac-verify *fat-secret-a* 1000 "hello" "not-hex!!")))
  (fat-check "an empty signature fails cleanly"
             (eq nil (fleet-hmac-verify *fat-secret-a* 1000 "hello" ""))))

;;; --- fleet-unix-time -------------------------------------------------------

;; The exact bug a real smoke test against a running acceptor found: a wire
;; timestamp from `date +%s` (Unix epoch) compared against
;; GET-UNIVERSAL-TIME's CL epoch (1900) is off by 2208988800 seconds --
;; every real request looked 70 years stale. FLEET-REQUEST-FRESH-P's
;; default NOW must be Unix epoch, matching what any peer actually sends.
(fat-check "fleet-unix-time is within a day of the OS's own epoch notion"
           ;; A loose bound on purpose: this checks the epoch is right
           ;; (1970, not 1900), not clock precision.
           (< (abs (- (fleet-unix-time)
                      (- (get-universal-time) +fleet-unix-epoch-offset-seconds+)))
              86400))
(fat-check "a real Unix timestamp from just now is fresh by default"
           (fleet-request-fresh-p (fleet-unix-time)))
(fat-check "a CL-universal-time-epoch value is NOT fresh by default"
           ;; This is the regression case itself: passing a universal-time
           ;; value where a Unix-epoch value was expected must read as
           ;; stale, not accidentally as fresh.
           (not (fleet-request-fresh-p (get-universal-time))))

;;; --- fleet-request-fresh-p -----------------------------------------------

(fat-check "exactly now is fresh" (fleet-request-fresh-p 1000 :now 1000))
(fat-check "at the trailing edge of the window is fresh"
           (fleet-request-fresh-p 940 :now 1000))
(fat-check "one second past the trailing edge is stale"
           (not (fleet-request-fresh-p 939 :now 1000)))
(fat-check "at the leading edge of the window is fresh"
           (fleet-request-fresh-p 1060 :now 1000))
(fat-check "one second past the leading edge is stale"
           (not (fleet-request-fresh-p 1061 :now 1000)))
(fat-check "a non-integer timestamp is never fresh"
           (not (fleet-request-fresh-p "1000" :now 1000)))

(format t "~%FLEET AUTH TESTS: ~a checks passed.~%" *fat-checks*)
