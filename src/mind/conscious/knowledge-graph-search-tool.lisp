;;;; knowledge-graph-search-tool.lisp -- one shared native/operator KG3 port.

(in-package :agent)

(export '(knowledge-graph-search-tool-schema
          knowledge-graph-search-tool-normalize
          knowledge-graph-search-tool-render))

(defun knowledge-graph-search-tool-schema ()
  (obj "type" "function" "function"
       (obj "name" "search-graph"
            "description"
            "Search and traverse the configured personal knowledge graph read-only. verified is the default for grounded factual answers; use inferred for relationship discovery or inspection of what the graph contains. In an inferred search, evidence_status inference and confirmation_recommended true identify reviewed but unconfirmed conclusions: use them cautiously and, when consequential and contextually natural, confirm at most one with the operator rather than asking a batch of questions. A verified result may report omitted_reviewed_inference_count and a retrieval_hint when reviewed relationships were hidden. node_count and edge_count describe only the bounded result subset, never the whole graph; use graph_entity_count and graph_fact_count for global totals. Results distinguish query facts, identity-only discovery, and related suggestions; suggestions do not establish the requested fact. To audit several names, include exact_queries in this same call. Claim a name is absent only when its exact_query_results item says absence_confirmed true. Do not use generic queries such as project task work to discover current runtime activity—the runtime sensorium owns that state."
            "parameters"
            (obj "type" "object" "additionalProperties" nil
                 "properties"
                 (obj
                  "starting_node_id"
                  (obj "description"
                       "Optional exact opaque node_id copied from an earlier search-graph result. Use null for natural-language cold start; query will resolve candidate nodes."
                       "anyOf" (vector (obj "type" "string" "maxLength" 240)
                                        (obj "type" "null")))
                  "query"
                  (obj "anyOf" (vector (obj "type" "string" "maxLength" 1000)
                                        (obj "type" "null")))
                  "exact_queries"
                  (obj "type" "array" "maxItems" 16 "uniqueItems" t
                       "description"
                       "Optional complete label or alias audits, answered together in this call. A zero match proves absence only when that result says absence_confirmed true."
                       "items" (obj "type" "string" "minLength" 1
                                    "maxLength" 240))
                  "predicates"
                  (obj "type" "array" "maxItems" 16 "uniqueItems" t
                       "description"
                       "Optional exact predicate filters. Use an empty array when no listed predicate precisely expresses the question."
                       "items"
                       (obj "type" "string"
                            "enum"
                            (knowledge-graph-search-supported-predicates)))
                  "direction" (obj "type" "string"
                                   "enum" #( "outgoing" "incoming" "both"))
                  "evidence_policy"
                  (obj "description"
                       "verified is the normal grounded view; inferred adds reviewed, explicitly labeled inferences; all additionally includes legacy unreviewed hypotheses."
                       "type" "string" "enum" #( "verified" "inferred" "all"))
                  "maximum_depth" (obj "type" "integer" "minimum" 0
                                       "maximum" 3)
                  "maximum_paths" (obj "type" "integer" "minimum" 1
                                       "maximum" 20))
                 "required"
                 #()))))

(defun knowledge-graph-search-tool-normalize (arguments)
  "Close a possibly sparse native call into the exact KG3 request shape.

The runtime, not the model, owns harmless traversal defaults. Unknown fields
and unsupported predicate vocabulary remain invalid rather than being guessed
or silently discarded."
  (unless (and
           (hash-table-p arguments)
           (every
            (lambda (key)
              (member key
                      '("starting_node_id" "query" "exact_queries"
                        "predicates" "direction"
                        "evidence_policy" "maximum_depth" "maximum_paths")
                      :test #'string=))
            (loop for key being the hash-keys of arguments collect key)))
    (error "search-graph arguments contain unknown fields"))
  (knowledge-graph-search-request
   :starting-node-id (gethash "starting_node_id" arguments :null)
   :query (gethash "query" arguments :null)
   :exact-queries (gethash "exact_queries" arguments #())
   :predicates (gethash "predicates" arguments #())
   :direction (gethash "direction" arguments "both")
   :evidence-policy (gethash "evidence_policy" arguments "verified")
   :maximum-depth (gethash "maximum_depth" arguments 2)
   :maximum-paths (gethash "maximum_paths" arguments 6)))

(defun knowledge-graph-search-tool-render (result)
  (unless (and (hash-table-p result)
               (= 1 (gethash "schema_version" result -1))
               (member (gethash "status" result)
                       '("available" "empty") :test #'string=))
    (error "search-graph result violates the KG3 tool contract"))
  (shasht:write-json (knowledge-graph-search-compact-result result) nil))
