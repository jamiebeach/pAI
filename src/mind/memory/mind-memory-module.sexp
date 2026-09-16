(:schema-version 1
 :module-id "pai.mind.memory"
 :layer :mind
 :purpose "Pure memory policy, atom identity and validation; later pure N2-N5 mechanics."
 :owned-projections
 ("memory_nodes" "memory_edges" "memory_atom_rollouts" "memory_atom_jobs"
  "memory_atom_candidates" "memory_atom_candidate_roots")
 :owned-event-contracts
 ("memory-node-state" "memory-edge-state"
  (:event-type "postgres-row-state"
   :tables ("memory_atom_rollouts" "memory_atom_jobs"
            "memory_atom_candidates" "memory_atom_candidate_roots")))
 :permitted-package-dependencies (:common-lisp :ironclad :babel)
 :forbidden-dependencies
 (:agent :postmodern :sql :http :provider :threads :filesystem :event-append
  :conversation-rendering :context-rendering :ticks :transport :delivery
  :authority-mutation)
 :public-interface
 ("VALIDATE-STATE" "ROW-ELIGIBLE-P" "BUILD-ATOM-MANIFEST"
  "BUILD-ATOM-REQUEST" "VALIDATE-ATOM-RESPONSE" "CAPABILITY-REPORT")
 :adapter-ports
 (:projection-lookup :projection-mutation :embedding :model-invocation :clock
  :event-append)
 :effective-behavior :none
 :production-loaded nil)
