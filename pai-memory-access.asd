(asdf:defsystem "pai-memory-access"
  :description "Pure memory protection and access policy primitives."
  :license "MIT"
  :version "0.1.0"
  :depends-on ("ironclad" "shasht")
  :serial t
  :components
  ((:file "src/mind/knowledge/memory-access/package")
   (:file "src/mind/knowledge/memory-access/core")))
