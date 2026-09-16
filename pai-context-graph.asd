;; Direct lab callers load this ASD without installing a source registry.
;; Register the sibling definition only; ASDF owns loading the inert library.
(asdf:load-asd (merge-pathnames "pai-memory-access.asd" *load-pathname*))

(asdf:defsystem "pai-context-graph"
  :description "Standalone typed temporal context-graph primitives for pAI labs."
  :license "MIT"
  :version "0.1.0"
  :depends-on ("ironclad" "shasht" "pai-memory-access")
  :serial t
  :components
  ((:file "src/mind/knowledge/context-graph/package")
   (:file "src/mind/knowledge/context-graph/core")
   (:file "src/mind/knowledge/context-graph/grounding")
   (:file "src/mind/knowledge/context-graph/authority")
   (:file "src/mind/knowledge/context-graph/search")
   (:file "src/mind/knowledge/context-graph/resolution")
   (:file "src/mind/knowledge/context-graph/lifecycle")
   (:file "src/mind/knowledge/context-graph/authority-projection")
   (:file "src/mind/knowledge/context-graph/authority-retrieval")
   (:file "src/mind/knowledge/context-graph/runtime-candidates")
   (:file "src/mind/knowledge/context-graph/model-adapter")
   (:file "src/mind/knowledge/context-graph/simple-model-adapter")
     (:file "src/mind/knowledge/context-graph/staged-model-adapter")
     (:file "src/mind/knowledge/context-graph/identity-page-adapter")
   (:file "src/mind/knowledge/context-graph/runtime-generation")
   (:file "src/mind/knowledge/context-graph/runtime-owner")
   (:file "src/mind/knowledge/context-graph/runtime-context")
   (:file "src/mind/knowledge/context-graph/identity-page-owner")
   (:file "src/mind/knowledge/context-graph/identity-formation")))
