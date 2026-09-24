;; Standalone system so isolated tests (harness: bare) can load just the
;; fleet layer without booting the entire "pai" serial chain. Mirrors
;; pai-context-graph.asd. Also listed inside pai.asd's own component list
;; for production loading -- this file exists purely for test isolation.
(asdf:defsystem "pai-fleet"
  :description "Standalone peer-to-peer fleet layer for pAI (docs/FLEET_DESIGN.md)."
  :license "MIT"
  :version "0.1.0"
  :depends-on ("ironclad" "babel")
  :serial t
  :components
  ((:file "src/mind/fleet/package")
   (:file "src/mind/fleet/identity")
   (:file "src/mind/fleet/auth")
   (:file "src/mind/fleet/join")
   (:file "src/mind/fleet/board")))
