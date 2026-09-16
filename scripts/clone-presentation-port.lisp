;;;; clone-presentation-port.lisp -- prove the event log's presentation port fires.
;;;;
;;;; The terminal reply path used to build a public-outbound envelope inside
;;;; EVENT-LOG.LISP and hand it to the publication layer directly. That was two
;;;; of the five hard back-edges out of the kernel. It now goes through
;;;; *PUBLIC-PRESENTATION-OBSERVER*, registered at :INSTALL.
;;;;
;;;; A passing boot proves the port is registered. It does not prove the port
;;;; is reached: AUTO-TURN is a wrap chain, and only one of its layers contains
;;;; the call. This runs a real turn and checks that the gateway recorded a
;;;; terminal presentation with the same shape it recorded before.

(load "/pai/scripts/clone-common.lisp" :if-does-not-exist nil)

(require :asdf)
(push #p"/pai/" asdf:*central-registry*)
(handler-bind ((warning #'muffle-warning))
  (asdf:load-system "pai"))

(in-package :agent)

;; *PUBLIC-OUTBOUND-RECORDS* is PUSHed, so it is newest-first, and it is capped
;; at *PUBLIC-OUTBOUND-RECORD-CAP*. Counting records therefore proves nothing
;; on a restored clone that is already at the cap -- the first version of this
;; probe read 500 before and 500 after and called a working port a failure.
;; The newest terminal record's attempt id is the identity that must change.
(defun %pp-latest-terminal ()
  (find-if (lambda (r)
             (let ((env (gethash "envelope" r)))
               (and env (equal "terminal" (gethash "channel" env)))))
           *public-outbound-records*))

(defun %pp-mark (record)
  (and record (list (gethash "transport_attempt_id" record)
                    (gethash "dedupe_key" (gethash "envelope" record)))))

(handler-case
    (progn
      (initialize)
      (format t "~&~%== presentation port ==~%")
      (format t "observer registered: ~a~%"
              (if *public-presentation-observer* "yes" "NO"))
      (let ((before (%pp-mark (%pp-latest-terminal))))
        (format t "newest terminal record before: ~a~%" before)
        (let ((reply (submit-stimulus
                      "say the single word: ping"
                      :kind :user-message
                      :wait-for-public-result t)))
          (declare (ignore reply))
          (let* ((record (%pp-latest-terminal))
                 (after (%pp-mark record))
                 (fired (and after (not (equal before after)))))
            (format t "newest terminal record after:  ~a~%" after)
            (format t "port fired: ~a~%" (if fired "yes" "NO"))
            (when record
              (let ((env (gethash "envelope" record)))
                (format t "  channel        ~a~%" (gethash "channel" env))
                (format t "  source         ~a~%" (gethash "source" env))
                (format t "  dedupe_key     ~a~%" (gethash "dedupe_key" env))
                (format t "  authorization  ~a~%"
                        (gethash "authorization_kind" env))
                (format t "  transport      ~a~%"
                        (gethash "transport_status" record))))
            (format t "~%PRESENTATION-PORT-~a~%" (if fired "OK" "FAIL")))))
      (finish-output))
  (error (e)
    (format t "~&PRESENTATION-PORT-ERR: ~a~%" e)
    (finish-output)))

(sb-ext:quit)
