;;;; auth.lisp -- HMAC request signing/verification for fleet peers.
;;;;
;;;; FLEET_DESIGN.md S2.1/S7: every peer-to-peer request carries an HMAC
;;;; signature over (timestamp + body), keyed by the per-peer shared secret
;;;; exchanged at join. This file is pure logic with no Hunchentoot
;;;; dependency -- the request/header plumbing lives in the web adapter
;;;; that calls into it, so this can be tested standalone (harness: bare).
(in-package :pai.fleet)

(defun %fleet-utf8-bytes (string)
  (babel:string-to-octets string :encoding :utf-8))

(defun fleet-hmac-sign (secret timestamp body)
  "Hex HMAC-SHA256 signature over TIMESTAMP (an integer -- Unix epoch
seconds on the wire, see FLEET-REQUEST-FRESH-P, though this function itself
only needs signer and verifier to agree on the same integer) and BODY (a
string, the raw request body). The canonical signed string is the
decimal timestamp, a literal '.', then BODY's UTF-8 bytes -- unambiguous
since decimal digits cannot themselves contain '.'."
  (unless (integerp timestamp)
    (error "fleet-hmac-sign requires an integer timestamp"))
  (let ((hmac (ironclad:make-hmac secret :sha256)))
    (ironclad:update-hmac hmac (%fleet-utf8-bytes (format nil "~d." timestamp)))
    (ironclad:update-hmac hmac (%fleet-utf8-bytes body))
    (ironclad:byte-array-to-hex-string (ironclad:hmac-digest hmac))))

(defun fleet-hmac-verify (secret timestamp body signature-hex)
  "T if SIGNATURE-HEX is a valid HMAC-SHA256 over TIMESTAMP and BODY under
SECRET, compared in constant time (IRONCLAD:CONSTANT-TIME-EQUAL, not
STRING= or EQUALP) so a network observer learns nothing from response
timing about how much of a forged signature was correct. A malformed
SIGNATURE-HEX (not valid hex) is a verification failure, not an error."
  (handler-case
      (ironclad:constant-time-equal
       (ironclad:hex-string-to-byte-array (fleet-hmac-sign secret timestamp body))
       (ironclad:hex-string-to-byte-array signature-hex))
    (error () nil)))

(defparameter +fleet-request-freshness-window-seconds+ 60
  "Reject a fleet request whose declared timestamp is further than this from
now, in either direction. Bounds naive replay of a captured, still-validly-
signed request (FLEET_DESIGN.md S2.1/S7). This is a freshness window, not a
nonce cache: it does not prevent replay within the window, which is an
accepted residual risk at this design's LAN/tailnet, single-operator threat
model (FLEET_DESIGN.md S6.6).")

(defconstant +fleet-unix-epoch-offset-seconds+ 2208988800
  "Common Lisp universal-time (epoch 1900-01-01) minus Unix time (epoch
1970-01-01), i.e. GET-UNIVERSAL-TIME minus this equals Unix epoch seconds.
Fleet wire timestamps use Unix epoch seconds, not universal-time, matching
what any peer implementation (Lisp or otherwise) naturally produces --
`date +%s`, not a CL-specific epoch. Everything else this codebase persists
(FLEET-PEER's JOINED-AT included) stays on universal-time; only the wire
timestamp differs, and only this function needs to know that.")

(defun fleet-unix-time ()
  (- (get-universal-time) +fleet-unix-epoch-offset-seconds+))

(defun fleet-request-fresh-p (timestamp &key (now (fleet-unix-time)))
  (and (integerp timestamp)
       (<= (abs (- now timestamp)) +fleet-request-freshness-window-seconds+)))
