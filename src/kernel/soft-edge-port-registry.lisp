;;;; soft-edge-port-registry.lisp -- 2026-08-16.
;;;;
;;;; scripts/coupling-report.js measures two kinds of back-edge: a lower
;;;; subsystem calling a higher one in head position (HARD -- must be
;;;; inverted) and the same reference reached through FBOUNDP/#' (soft --
;;;; already late-bound, but undeclared and unenforced). Hard back-edges are
;;;; zero as of the previous pass (see docs/progress.md, "P0c" and the git
;;;; history around "Eliminate the hard back-edges out of the kernel"). This
;;;; file is the declaration pass for the ten that remain soft: name each as
;;;; a port or an accepted shared primitive, so a NEW undeclared soft edge is
;;;; a decision someone has to write down, not a fact that only a script
;;;; happens to notice.
;;;;
;;;; Two ways a soft edge gets declared here:
;;;;
;;;;   PORT       A DEFVAR lives in the lower/calling file; the higher/
;;;;              providing file sets it to its own function via
;;;;              (DEFINE-INIT :INSTALL ...). The bare FBOUNDP/FUNCALL-by-
;;;;              symbol call site is kept as a fallback so behaviour is
;;;;              identical whether or not :INSTALL has run yet -- this is
;;;;              the same shape as *PUBLIC-PRESENTATION-OBSERVER* in
;;;;              event-log.lisp and *CONVERSATION-PROMPT-MUTATION-ALLOWED-FN*
;;;;              in conversation-episodic-memory.lisp, both landed in the
;;;;              hard-back-edge pass.
;;;;   ACCEPTED   The edge is a deliberate use of an existing shared
;;;;              primitive or generalized read-helper, not a bespoke
;;;;              reach-up, and a bespoke port would duplicate a mechanism
;;;;              that already exists. A reason is required, same as
;;;;              *ACCEPTED-REPLACEMENTS* in wrap-chain-completeness-tests.lisp.
;;;;
;;;; This is declaration, not restructuring: no call site's *behaviour*
;;;; changes here, only whether the reach-up is a named, registered fact.
;;;;
;;;; Read by scripts/coupling-report.js as source text (see
;;;; %WCC-REGISTRY-NAMES in tests/wrap-chain-completeness-tests.lisp for the
;;;; precedent of parsing a Lisp registry from a non-Lisp checker). Keep each
;;;; entry a single quoted symbol-name string so that parse stays trivial.

(in-package :agent)

(export '(soft-edge-ports))

(defparameter *soft-edge-ports*
  (obj
   ;; authority -> memory. Four separate ports, one per capture call --
   ;; EVENT-LOG.LISP records the raw stream and does not know how a typed
   ;; public turn is assembled; CONVERSATION-TURN-CAPTURE.LISP registers all
   ;; four at :INSTALL.
   "%turn-capture-register-user-event"
   (obj "kind" "port" "var" "*turn-capture-user-event-fn*"
        "caller" "kernel/event-log.lisp"
        "provider" "mind/conversation/conversation-turn-capture.lisp")
   "%turn-capture-register-completion"
   (obj "kind" "port" "var" "*turn-capture-completion-fn*"
        "caller" "kernel/event-log.lisp"
        "provider" "mind/conversation/conversation-turn-capture.lisp")
   "%turn-capture-register-tool-call-event"
   (obj "kind" "port" "var" "*turn-capture-tool-call-fn*"
        "caller" "kernel/event-log.lisp"
        "provider" "mind/conversation/conversation-turn-capture.lisp")
   "%turn-capture-register-tool-result-event"
   (obj "kind" "port" "var" "*turn-capture-tool-result-fn*"
        "caller" "kernel/event-log.lisp"
        "provider" "mind/conversation/conversation-turn-capture.lisp")

   ;; authority -> cognitive. Both already go through
   ;; %RUNTIME-TRUTH-REPORT, a single generalized "read an optional report
   ;; from a higher layer, or DEFAULT when absent" helper covering nine such
   ;; reads (see runtime-truth.lisp). A bespoke port per symbol would
   ;; duplicate that helper rather than declare anything new.
   "public-outbound-gateway-report"
   (obj "kind" "accepted"
        "reason" "Read via %RUNTIME-TRUTH-REPORT in kernel/runtime-truth.lisp, the generalized optional-report helper. See its docstring.")
   "public-system-prompt-report"
   (obj "kind" "accepted"
        "reason" "Read via %RUNTIME-TRUTH-REPORT in kernel/runtime-truth.lisp, the generalized optional-report helper. See its docstring.")

   ;; memory -> cognitive.
   "context-projection-legacy-mutation-enabled-p"
   (obj "kind" "port" "var" "*conversation-prompt-mutation-allowed-fn*"
        "caller" "mind/conversation/conversation-episodic-memory.lisp"
        "provider" "mind/context/context-projection.lisp"
        "note" "Landed in the hard-back-edge pass; the coupling report still lists the symbol because five other call sites in mind/drives and mind/reflection call it directly rather than through the port -- none of those are back-edges themselves (mind/drives is unclassified; the mind/reflection ones read the same-subsystem symbol), so no port conversion is owed there.")
   "%latent-v2-hash"
   (obj "kind" "port" "var" "*legacy-audit-hash-fn*"
        "caller" "mind/memory/legacy-memory-audit.lisp"
        "provider" "mind/reflection/latent-thoughts-v2.lisp")
   "near-term-intention-observe-public-reply"
   (obj "kind" "port" "var" "*near-term-intention-public-reply-observer*"
        "caller" "mind/conversation/conversation-turn-capture.lisp"
        "provider" "mind/reflection/near-term-intentions.lisp")
   "raw-call-model"
   (obj "kind" "accepted"
        "reason" "A wrap-chain base function (see \"raw-call-model\" in wrap-chain-registry.lisp), not an observer callback -- it is the model-invocation primitive itself, deliberately callable from any layer that needs to talk to the model, the same way CALL-MODEL is. memory-nodes.lisp's FBOUNDP guard exists for load-order safety (memory can load before the layer that defines raw-call-model), not to hide a layering violation a port would fix."))
  "Declares every soft back-edge scripts/coupling-report.js currently finds
(authority -> memory, authority -> cognitive, memory -> cognitive), either as
a registered PORT or as an ACCEPTED use of an existing shared mechanism.
Hand-maintained, same as *WRAP-CHAINS* in wrap-chain-registry.lisp -- a new
undeclared soft edge should fail the completeness check in
scripts/coupling-report.js and get added here with a reason, not silenced.")

(defun soft-edge-ports () *soft-edge-ports*)
