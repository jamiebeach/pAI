;;;; agent-config.lisp -- P0.4, 2026-07-28.
;;;;
;;;; Two things, both deliberately scoped against the same "don't force a
;;;; P0.1-shaped rewrite" reasoning used everywhere else this session:
;;;;
;;;;   1. CONFIG-REPORT -- one place to SEE every currently-tunable
;;;;      parameter, live, across ~10 files (tick loop, memory decay,
;;;;      modulators, drives, spreading activation, soul, self-mod).
;;;;      This is NOT an authoritative config source that overrides each
;;;;      subsystem's own DEFPARAMETER -- making it authoritative would
;;;;      require either loading before every subsystem (and then each
;;;;      subsystem's own DEFPARAMETER would clobber it right back,
;;;;      DEFPARAMETER always reassigns) or after every subsystem (and
;;;;      then reloading any ONE subsystem live, an extremely common
;;;;      operation this whole session, would silently revert its values
;;;;      to defaults until this file is also reloaded -- yet another
;;;;      fragile reload-order dependency, exactly the class of bug that
;;;;      caused the conversation-persistence incident). A read-only,
;;;;      always-live INTROSPECTION view sidesteps that fragility
;;;;      entirely while still solving the actual problem named in the
;;;;      backlog ("every knob in one place").
;;;;
;;;;   2. SNAPSHOT-ALL / RESTORE-ALL -- pure orchestration over each
;;;;      subsystem's OWN already-existing, already-tested SAVE-*/LOAD-*
;;;;      function. Never reimplements persistence logic. Each subsystem
;;;;      already saves and restores itself independently on its own
;;;;      schedule (most on every write; conversation.json additionally
;;;;      has its own independent heartbeat) -- what was missing was a
;;;;      single atomic "save everything now" / "restore everything now"
;;;;      call, not a new persistence mechanism.
;;;;
;;;; Load live (no restart) via lisp-eval or repl-drop:
;;;;   (load "/agent/state/agent-config.lisp")

(in-package :agent)

(export '(config-report snapshot-all restore-all))

(defun %config-safe (thunk)
  (handler-case (funcall thunk) (error () :null)))

(defun config-report ()
  "Live snapshot of every currently-tunable parameter this session has
touched, pulled straight from each subsystem's own special variables --
read-only, for visibility and tuning-by-hand, not enforced anywhere."
  (obj "tick_loop"
       (obj "base_interval_seconds" (%config-safe (lambda () *tick-base-interval-seconds*))
            "max_per_hour" (%config-safe (lambda () *tick-max-per-hour*))
            "budget_soft_daily" (%config-safe (lambda () *tick-budget-soft-daily*))
            "budget_hard_daily" (%config-safe (lambda () *tick-budget-hard-daily*)))
       "memory_decay"
       (obj "half_lives_days"
            (%config-safe (lambda ()
                             (let ((out (obj)))
                               (maphash (lambda (k v) (setf (gethash k out) (/ v 86400.0))) *decay-half-life-seconds*)
                               out)))
            "cold_threshold" (%config-safe (lambda () *decay-cold-threshold*)))
       "modulators" (%config-safe (lambda () (modulator-state)))
       "drives" (%config-safe (lambda () (drive-state)))
       "spreading_activation"
       (obj "decay_per_hop" (%config-safe (lambda () *spread-decay-per-hop*))
            "max_hops" (%config-safe (lambda () *spread-max-hops*))
            "intrusion_threshold" (%config-safe (lambda () *intrusion-base-threshold*))
            "intrusion_refractory_seconds" (%config-safe (lambda () *intrusion-refractory-seconds*)))
       "soul"
       (obj "cap" (%config-safe (lambda () *soul-max-entries*))
            "count" (%config-safe (lambda () (length (soul-entries)))))
       "self_mod"
       (obj "max_proposals" (%config-safe (lambda () *max-proposals*))
            "immutable_core_count" (%config-safe (lambda () (length *immutable-core-symbols*))))
       "turn_watchdog"
       (obj "first_checkin_seconds" (%config-safe (lambda () *turn-watchdog-first-checkin-seconds*))
            "checkin_interval_seconds" (%config-safe (lambda () *turn-watchdog-checkin-interval-seconds*)))))

(defparameter *snapshot-subsystems*
  '(("stabilization-config" save-stabilization-config load-stabilization-config)
    ("drives" save-drives load-drives)
    ("modulators" save-modulators load-modulators)
    ("continuity-buffer" save-continuity-buffer load-continuity-buffer)
    ("soul" save-soul load-soul)
    ("memory-nodes" save-memory-nodes load-memory-nodes)
    ("episode-state" save-episode-state load-episode-state)
    ("candidate-pool" save-candidate-pool load-candidate-pool)
    ("contact-log" save-contact-log load-contact-log)
    ("initiative-candidates" %initiative-save %initiative-load)
    ("initiative-v2" initiative-v2-save initiative-v2-load)
    ("latent-thoughts" %latent-save %latent-load)
    ("latent-thoughts-v2" latent-v2-save latent-v2-load)
    ("ambient-recall-history" save-ambient-history load-ambient-history))
  "Each entry: (name save-fn-symbol load-fn-symbol). Both must already
exist as self-contained subsystem functions -- this list is purely a
dispatch table, never persistence logic of its own. Conversation history
is deliberately excluded: it already has its own dedicated wrap PLUS an
independent heartbeat (conversation-persistence-heartbeat.lisp), a more
robust story on its own than adding it to a manual snapshot call would
provide. Loop-version history (self-mod-phase4.lisp) is append-only by
design (every accepted install is already durably journaled the moment
it happens) -- there's nothing for a bulk snapshot to add there either.")

(defun snapshot-all ()
  "Best-effort: calls every subsystem's own SAVE-* function and returns
(name . :ok / (:error . reason) / :not-loaded) per subsystem. One
subsystem failing to save must never prevent the others from being
tried -- this is why HANDLER-CASE wraps each call individually rather
than the whole loop."
  (mapcar (lambda (entry)
            (destructuring-bind (name save-fn load-fn) entry
              (declare (ignore load-fn))
              (cons name
                    (if (fboundp save-fn)
                        (handler-case (progn (funcall save-fn) :ok)
                          (error (e) (cons :error (format nil "~a" e))))
                        :not-loaded))))
          *snapshot-subsystems*))

(defun restore-all ()
  "Symmetric to SNAPSHOT-ALL -- calls every subsystem's own LOAD-*
function. Mainly useful for an explicit, deliberate re-sync (e.g. after
manually editing a subsystem's JSON file on disk); every subsystem
already restores itself automatically at boot regardless."
  (mapcar (lambda (entry)
            (destructuring-bind (name save-fn load-fn) entry
              (declare (ignore save-fn))
              (cons name
                    (if (fboundp load-fn)
                        (handler-case (progn (funcall load-fn) :ok)
                          (error (e) (cons :error (format nil "~a" e))))
                        :not-loaded))))
          *snapshot-subsystems*))
