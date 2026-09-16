;;;; Offline, pure-library qualification. Does not load or initialize the agent.
(require :asdf)
(let ((*compile-verbose* nil) (*compile-print* nil))
  (handler-bind ((sb-ext:compiler-note #'muffle-warning))
    (dolist (name '("context-graph-authority-span-tests.lisp"
                    "context-graph-authority-participant-tests.lisp"
                    "context-graph-authority-revision-tests.lisp"
                    "context-graph-authority-scope-tests.lisp"
                    "context-graph-authority-preparation-tests.lisp"
                    "context-graph-authority-admission-tests.lisp"
                    "context-graph-authority-identity-tests.lisp"
                    "context-graph-authority-installation-tests.lisp"
                    "memory-access-policy-tests.lisp"))
      (load (merge-pathnames name *load-truename*)))))
