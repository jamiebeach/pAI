;;;; wrap-chain-registry.lisp -- 2026-07-29.
;;;;
;;;; Structural fix for a bug class that has now cost real, user-facing
;;;; functionality TWICE in one day: reloading one file in the middle of
;;;; a rename-and-fall-through wrap chain silently drops every wrap
;;;; layered on top of it (from OTHER files, loaded later at original
;;;; boot), with zero error anywhere. First hit: reloading
;;;; PAI-ENHANCEMENTS.LISP for the reasoning-fallback fix dropped
;;;; CONVERSATION-PERSISTENCE.LISP's wrap on %RUN-SELF-MOD-MESSAGES for
;;;; ~9 hours (fixed with a redundant heartbeat, but the underlying
;;;; fragility was explicitly left for "later"). Second hit, same day:
;;;; reloading PAI-ENHANCEMENTS.LISP again (for the E1 structural
;;;; reasoning-isolation fix) orphaned RUNWARE.LISP's wrap on EXECUTE --
;;;; *TOOLS* still correctly listed generate-image/upload-reference-image
;;;; (that list is additive and wasn't reset), so the model tried to use
;;;; them, but EXECUTE no longer knew how to dispatch either one. Found
;;;; live because the operator asked for a picture and the agent couldn't produce
;;;; one, even though it believed the tool existed.
;;;;
;;;; This doesn't prevent the underlying fragility -- the wrap idiom
;;;; itself still has this property, and fixing THAT would mean a real
;;;; architectural change (P0.1's package/CLOS refactor, still deferred
;;;; for the same reasons as always). What this DOES do: turn "remember
;;;; the exact right file list and order" into "look it up" and, via
;;;; RELOAD-WRAP-CHAIN, "just call one function" -- so restoring one
;;;; wrap after an unrelated live edit reliably restores the whole
;;;; chain, rather than depending on whoever's doing the reload to recall
;;;; or re-derive it correctly under time pressure.
;;;;
;;;; Load live (no restart) via lisp-eval or repl-drop:
;;;;   (load "/agent/state/wrap-chain-registry.lisp")

(in-package :agent)

(export '(reload-wrap-chain wrap-chains))

(defparameter *wrap-chains*
  (obj
       ;; 2026-08-15: four chains added after a completeness test found this
       ;; registry incomplete -- eleven multi-file redefinitions existed that
       ;; it did not list. Each of these was verified to call through to a
       ;; saved original before being recorded here.
       ;; lisp-eval was listed here; converted to a seam 2026-08-15, with
       ;; safety-timeout outermost (:order 100) and eval-journal inside it
       ;; (:order 200) -- the ordering that keeps the whole chain under the
       ;; wall-time bound, now declared rather than implied by load sequence.
       ;; memory-write-node was listed here; converted to a seam 2026-08-16
       ;; (P0c item 3, one of the three cross-subsystem chains), with
       ;; reflection-novelty-suppression outermost (:order 100) and
       ;; candidate-pool-nomination inside it (:order 200) -- a suppressed
       ;; duplicate must never reach nomination, matching the original
       ;; rename-and-fall-through order.
       ;; call-model was listed here (incompletely: the file list never
       ;; included self-mod.lisp, enhancements.lisp or agent.lisp, which also
       ;; each (defun call-model ...) -- the completeness test only checks
       ;; that the NAME is declared somewhere, not that a chain's file list is
       ;; exhaustive). Converted to a seam 2026-08-16 (P0c item 3). Tracing it
       ;; found those three earlier layers, plus modulator.lisp's, are already
       ;; dead in production: agent_print.lisp's call-model calls RAW-CALL-MODEL
       ;; directly and reimplements budget-check/masking/logging itself,
       ;; rather than falling through to a saved original, so it fully
       ;; replaces every earlier definition instead of wrapping it -- the
       ;; four earlier files' call-model bodies never run. (One concrete
       ;; consequence: modulator.lisp's resolution-level-driven temperature
       ;; override, *CALL-MODEL-TEMPERATURE-OVERRIDE*, is bound nowhere else
       ;; and is therefore permanently NIL in production. Not fixed here --
       ;; recorded, not resurrected, since registering it as a layer would be
       ;; a behaviour change, not a declaration.) The seam's base is
       ;; therefore AGENT_PRINT.LISP, the true first live layer, not
       ;; AGENT.LISP; the four earlier same-name defuns are accepted
       ;; replacements (see *ACCEPTED-REPLACEMENTS* in
       ;; wrap-chain-completeness-tests.lisp), not part of the live chain.
       ;; Order of the three still-live layers, outermost first:
       ;; temporal-response-policy (:order 100), timing (:order 200,
       ;; observability-tracing.lisp), turn-capture (:order 300,
       ;; conversation-turn-capture.lisp), base (agent_print.lisp).
       ;; run-self-mod was listed here; converted to a seam 2026-08-15.
       ;; pai-turn-log and log-line were listed here. Both were converted to
       ;; seams on 2026-08-15 -- define-seam in agent_print, register-layer
       ;; in web-terminal -- and are the first two chains retired under P0c.
       ;;
       ;; This list only shrinks. An entry returning here means a wrap was
       ;; reintroduced, which the completeness test is meant to prevent.
       "execute"
       ;; 2026-07-28: retroactively added event-log.lisp and
       ;; self-mod-provenance.lisp -- event-log.lisp already wrapped
       ;; EXECUTE and was never listed here, a pre-existing registry gap
       ;; found while building self-mod-provenance.lisp.
       (vector "enhancements.lisp" "web-terminal.lisp" "runware.lisp"
               "bounded-work-tools.lisp" "memory-search-tool.lisp"
               "event-log.lisp"
               "self-mod-provenance.lisp" "observability-tracing.lisp"
               "near-term-intention-tool.lisp")
       "%run-self-mod-messages"
       (vector "enhancements.lisp" "web-terminal.lisp" "conversation-persistence.lisp")
       "auto-turn"
       ;; 2026-07-29: retroactively added event-log.lisp (same
       ;; pre-existing-gap class as EXECUTE/PROPOSE-LOOP above -- it
       ;; already wrapped AUTO-TURN and was never listed) and
       ;; conversational-initiative.lisp, in real boot order (the latter
       ;; loads in the same early block as SOUL.LISP, well before
       ;; EVENT-LOG.LISP's later block).
       (vector "modulator.lisp" "tick-loop.lisp" "spreading-activation.lisp" "drives.lisp" "soul.lisp"
               "conversational-initiative.lisp" "event-log.lisp" "latent-thoughts.lisp" "conversation-episodic-memory.lisp"
               "conversation-turn-capture.lisp" "context-projection.lisp"
               "observability-tracing.lisp")
       "raw-call-model"
       (vector "enhancements.lisp" "modulator.lisp" "tick-loop.lisp"
               "observability-tracing.lisp" "temporal-response-policy.lisp")
       "memory-recall"
       (vector "memory-nodes.lisp" "typed-retrieval.lisp" "observability-tracing.lisp")
       "cognitive-call"
       (vector "cognitive-call.lisp" "observability-tracing.lisp")
       "tick-commit-apply"
       (vector "tick-commit.lisp" "observability-tracing.lisp")
       "embed-text"
       (vector "memory-nodes.lisp" "embedding-turn-cache.lisp"
               "observability-tracing.lisp")
       "%conv-persist-write"
       (vector "conversation-persistence.lisp" "observability-tracing.lisp")
       "%v2-broadcast"
       (vector "web-terminal.lisp" "observability-tracing.lisp"
               "public-outbound-gateway.lisp")
       "tick-once"
       ;; TICK-EXECUTION-V2 can install safely over an existing timing wrapper,
       ;; but a full cold-order rebuild remains tick-loop -> execution -> timing
       ;; -> grounded observer. GROUNDED-AGENCY is enqueue-only and must remain
       ;; outermost so its post-tick observation cannot affect tick execution.
       (vector "tick-loop.lisp" "tick-execution.lisp"
               "observability-tracing.lisp" "grounded-agency.lisp")
       "propose-loop"
       ;; 2026-07-28: retroactively added event-log.lisp (same
       ;; pre-existing gap as EXECUTE above) and self-mod-provenance.lisp.
       ;; 2026-08-17: retroactively added modulator.lisp and eval-journal.lisp,
       ;; found by the Q0 producer inventory. This entry declared four of six
       ;; wrappers, and the two it omitted were the two that only act when
       ;; something is wrong -- modulator.lisp is the competence gate that
       ;; rejects proposals unless competence is above a floor and certainty
       ;; below a ceiling, and eval-journal.lisp is the fail-closed audit that
       ;; rejects anything it cannot journal first. RELOAD-WRAP-CHAIN therefore
       ;; reported success while leaving self-modification ungated and
       ;; unjournaled. See docs/q0-producer-inventory.md Finding F, gotcha 35.
       (vector "self-mod-phase4.lisp" "self-mod-sandbox.lisp" "modulator.lisp"
               "eval-journal.lisp" "event-log.lisp" "self-mod-provenance.lisp")
       "static-check"
       (vector "self-mod-phase4.lisp")
       "telegram-send"
       ;; 2026-08-17: added by the Q0 producer inventory. This chain was
       ;; undeclared entirely, though its outermost layer IS the publication
       ;; authority boundary -- public-outbound-gateway.lisp resolves the
       ;; envelope, runs the counterfactual evaluation and audits the send.
       ;; Reloading telegram.lisp alone would have dropped both wrappers and
       ;; sent outbound messages without passing that gateway, with no
       ;; documented way to restore them.
       ;;
       ;; It was invisible to the completeness test because both wrapping
       ;; DEFUNs are indented inside (when (fboundp 'telegram-send) ...) and
       ;; that test matches DEFUN only at column 0 (gotcha 34). The detector
       ;; gap and this declaration gap compound: nothing could have prompted
       ;; anyone to write this entry.
       (vector "candidate-policy.lisp" "public-outbound-gateway.lisp")
       "%tick-handle-anticipate"
       (vector "prediction-journal.lisp")
       "%tick-select-type"
       (vector "attention-schema.lisp")
       "%tick-handle-maintenance"
       (vector "self-mod-provenance.lisp" "soul-candidate-pool.lisp")
       "modulator-decay-tick"
       (vector "modulator-watchdog.lisp")
       "self-model-propose-revision"
       (vector "soul-candidate-pool.lisp")
       "%tick-type-weights"
       (vector "conversational-initiative.lisp" "reflection-novelty.lisp")
       "%drives-event-initiate"
       (vector "initiative-engine.lisp" "candidate-policy.lisp" "observability-tracing.lisp")
       "%tick-handle-consolidate"
       (vector "episode-boundary.lisp" "reflection-novelty.lisp")
       ;; 2026-07-29: ambient-recall-diversity.lisp fully replaces both
       ;; handlers' bodies (not additive on top) to swap their one fixed-
       ;; query MEMORY-RECALL call for %RECALL-AMBIENT -- everything else
       ;; in each body is unchanged from tick-loop.lisp's original.
       "%tick-handle-light-consolidate"
       (vector "ambient-recall-diversity.lisp")
       "%tick-handle-full-reflection"
       (vector "ambient-recall-diversity.lisp"))
  "Documents, in verified boot-load order, every file known to wrap each
base function via the rename-and-fall-through idiom. NOT auto-discovered
-- hand-maintained, since the wrap relationships themselves are hand-
written. Update this whenever a new file starts wrapping one of these
functions, or a new base function grows its own wrap chain -- the
registry is only as good as its own upkeep, which is a real, honest
limitation, not a solved problem.")

(defun wrap-chains () *wrap-chains*)

(defun reload-wrap-chain (fn-name)
  "FN-NAME is a string (e.g. \"execute\"). Reloads every file known to
wrap it, in the documented order, from /agent/state/. Prints what it did
either way -- an unknown FN-NAME is reported, not silently ignored,
since that usually means the registry itself needs updating before this
can help."
  (when (and (string= fn-name "execute")
             (fboundp 'tool-dispatch-kernel-boot-p)
             (funcall 'tool-dispatch-kernel-boot-p))
    (error "kernel mode forbids reloading the legacy EXECUTE chain; cold-restart in legacy mode."))
  (let ((files (gethash fn-name *wrap-chains*)))
    (if (not files)
        (progn
          (format t "~&[wrap-chain] no known chain for ~s -- either nothing wraps it, or *WRAP-CHAINS* needs updating.~%" fn-name)
          nil)
        (progn
          (format t "~&[wrap-chain] reloading ~s's full chain, in order: ~{~a~^ -> ~}~%" fn-name (coerce files 'list))
          (map 'list
               (lambda (f)
                 (if (probe-file f)
                     (progn (load f) (cons f :ok))
                     (progn (format t "~&[wrap-chain]   ~a not found, skipped~%" f) (cons f :not-found))))
               files)))))
