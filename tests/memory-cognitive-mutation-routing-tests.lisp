(in-package :agent)

(ql:quickload '(:shasht :ironclad :bordeaux-threads) :silent t)

(defvar *memory-cognitive-routing-pass* 0)
(defvar *memory-cognitive-routing-fail* 0)

(defun memory-cognitive-routing-check (name condition)
  (if condition
      (progn (incf *memory-cognitive-routing-pass*)
             (format t "  ok   ~a~%" name))
      (progn (incf *memory-cognitive-routing-fail*)
             (format t "  FAIL ~a~%" name))))

(load (test-source "storage-substrate.lisp"))
(load (test-source "memory-storage.lisp"))
(load (test-source "stabilization-config.lisp"))
(load (test-source "memory-nodes.lisp"))
(load (test-source "epistemic-memory.lisp"))
(load (test-source "typed-retrieval.lisp"))
(load (test-source "cognitive-call.lisp"))
(load (test-source "tick-commit.lisp"))

(defun memory-cognitive-routing-signals-p (thunk)
  (handler-case (progn (funcall thunk) nil)
    (memory-storage-error () t)
    (error () nil)))

(defun memory-cognitive-routing-node (id &key (access-count 0) (activation 0.4))
  (let ((scalar
          (make-memory-node-scalar-row
           :id id :kind "thought" :content (format nil "content ~a" id)
           :timestamp "2026-08-20T12:00:00Z" :importance 0.6d0
           :origin-class "synthetic" :epistemic-status "hypothesis"
           :grounding-status "grounded"
           :epistemic-metadata (%memory-storage-object))))
    (let ((row (shasht:read-json scalar)))
      (setf (gethash "access_count" row) access-count
            (gethash "activation" row) activation)
      (%memory-storage-object
       "scalar_json" (shasht:write-json row nil)
       "embedding_binary_hex" "000100003f800000"
       "retrieval_embedding_binary_hex" "000100003f800000"))))

(defun memory-cognitive-routing-edge (id from to kind)
  (shasht:write-json
   (%memory-storage-object
    "id" id "from_id" from "to_id" to "edge_type" kind
    "created_at" "2026-08-20T12:00:00Z") nil))

(format t "~%== cognitive memory command assembly and routing ==~%")

(let* ((required '(memory-assemble-admission-operation
                   memory-assemble-supersession-operation
                   memory-assemble-tick-commit-operation
                   memory-assemble-user-visible-rehearsal-operation
                   memory-cognitive-mutation-dispatch))
       (available (every #'fboundp required)))
  (memory-cognitive-routing-check
   "closed cognitive operation assembly surface exists" available)
  (when available
    (let* ((node (memory-cognitive-routing-node "n1"))
           (delete-edge (memory-cognitive-routing-edge
                         1 "n1" "root-old" "derived-from"))
           (insert-edge (memory-cognitive-routing-edge
                         2 "n1" "root-new" "derived-from"))
           (plan (memory-assemble-admission-operation
                  node (vector delete-edge) (vector insert-edge))))
      (memory-cognitive-routing-check
       "admission plan preserves one node ID after closed node/lineage commands"
       (and (string= "admission" (gethash "operation_kind" plan))
            (string= "node-id" (gethash "return_contract" plan))
            (string= "n1" (gethash "return_value" plan))
            (= 3 (length (gethash "commands" plan))))))
    (let* ((node (memory-cognitive-routing-node "replacement"))
           (edge (memory-cognitive-routing-edge
                  3 "replacement" "old" "supersedes"))
           (plan (memory-assemble-supersession-operation
                  node "old" "new evidence" "operator" edge)))
      (memory-cognitive-routing-check
       "supersession plan atomically carries replacement row and edge"
       (and (string= "supersession" (gethash "operation_kind" plan))
            (string= "true" (gethash "return_contract" plan))
            (eq t (gethash "return_value" plan))
            (= 2 (length (gethash "commands" plan))))))
    (let* ((nodes (vector (memory-cognitive-routing-node "t1")
                          (memory-cognitive-routing-node "t2")))
           (edges (vector (memory-cognitive-routing-edge
                           4 "t2" "t1" "derived-from")))
           (plan (memory-assemble-tick-commit-operation nodes edges)))
      (memory-cognitive-routing-check
       "tick plan preserves ordered admitted IDs"
       (equal '("t1" "t2") (gethash "return_value" plan))))
    (let* ((nodes (vector (memory-cognitive-routing-node
                           "r1" :access-count 4 :activation 0.83d0)
                          (memory-cognitive-routing-node
                           "r2" :access-count 2 :activation 0.90d0)))
           (plan (memory-assemble-user-visible-rehearsal-operation
                  nodes "2026-08-20T13:00:00Z" "public-response" :null 3)))
      (memory-cognitive-routing-check
       "rehearsal plan preserves exact use-report contract"
       (let ((report (gethash "return_value" plan)))
         (and (= 2 (length (gethash "commands" plan)))
              (= 3 (gethash "requested_count" report))
              (= 2 (gethash "updated_count" report))))))
    (memory-cognitive-routing-check
     "assembler rejects an edge kind outside its cognitive family"
     (memory-cognitive-routing-signals-p
      (lambda ()
        (memory-assemble-tick-commit-operation
         (vector (memory-cognitive-routing-node "bad"))
         (vector (memory-cognitive-routing-edge
                  5 "bad" "other" "invented"))))))
    (let ((*memory-cognitive-mutation-mode* :postgresql)
          (*memory-cognitive-mutation-router* (lambda (&rest args)
                                                (declare (ignore args))
                                                (error "router called")))
          (fallback-called nil))
      (memory-cognitive-routing-check
       "default selector invokes only the incumbent transaction"
       (and (string= "pg"
                     (memory-cognitive-mutation-dispatch
                      "admission" (%memory-storage-object)
                      (lambda () (setf fallback-called t) "pg")))
            fallback-called)))
    (let ((*memory-cognitive-mutation-mode* :event-first)
          (*memory-cognitive-mutation-router*
            (lambda (family request)
              (declare (ignore request))
              (format nil "event:~a" family)))
          (fallback-called nil))
      (memory-cognitive-routing-check
       "event-first selector replaces rather than shadows PostgreSQL"
       (and (string= "event:admission"
                     (memory-cognitive-mutation-dispatch
                      "admission" (%memory-storage-object)
                      (lambda () (setf fallback-called t) "pg")))
            (null fallback-called))))
    (let ((*memory-cognitive-mutation-mode* :event-first)
          (*memory-cognitive-mutation-router* nil))
      (memory-cognitive-routing-check
       "event-first selection without a complete router fails closed"
       (memory-cognitive-routing-signals-p
        (lambda ()
          (memory-cognitive-mutation-dispatch
           "admission" (%memory-storage-object) (lambda () "pg")))))))
  (dolist (entry '(("epistemic-memory.lisp" "admission")
                   ("epistemic-memory.lisp" "supersession")
                   ("typed-retrieval.lisp" "user-visible-rehearsal")
                   ("tick-commit.lisp" "tick-commit")))
    (let ((text (uiop:read-file-string (test-source (first entry)))))
      (memory-cognitive-routing-check
       (format nil "~a consumer crosses the explicit cognitive mutation port"
               (second entry))
       (and (search "memory-cognitive-mutation-dispatch" text)
            (search (format nil "\"~a\"" (second entry)) text))))))

(let ((route-calls nil)
      (*memory-cognitive-mutation-mode* :event-first)
      (*memory-cognitive-mutation-router* nil)
      (*memory-model-invoke-fn* nil))
  (setf *memory-cognitive-mutation-router*
        (lambda (family request)
          (push family route-calls)
          (cond
            ((string= family "admission")
             (values (gethash "node_id" request)
                     (coerce (gethash "lineage_parent_ids" request) 'list)))
            ((string= family "supersession") t)
            ((string= family "user-visible-rehearsal")
             (%memory-storage-object
              "consumer" (gethash "consumer" request)
              "generation_id" (gethash "generation_id" request)
              "user_visible" t
              "requested_count" (length (gethash "node_ids" request))
              "updated_count" (length (gethash "node_ids" request))))
            ((string= family "tick-commit")
             (loop for item across (gethash "prepared" request)
                   collect (getf item :id)))
            (t (error "unexpected family")))))
  (setf (fdefinition 'embed-text)
        (lambda (text) (declare (ignore text)) (list 1.0d0))
        (fdefinition 'embed-retrieval-document)
        (lambda (text) (declare (ignore text)) (list 1.0d0))
        (fdefinition 'log-event)
        (lambda (&rest args) (declare (ignore args)) 1))
  (memory-cognitive-routing-check
   "public admission replaces PostgreSQL when event-first is selected"
   (string= "route-admission"
            (memory-admit-node
             :id "route-admission" :kind "observation" :content "fixture"
             :importance 0.1d0 :origin-class "lived-user"
             :epistemic-status "user-report" :grounding-status "grounded")))
  (memory-cognitive-routing-check
   "public supersession replaces PostgreSQL when event-first is selected"
   (memory-supersede "route-old" "route-replacement"))
  (memory-cognitive-routing-check
   "public use accounting replaces PostgreSQL and preserves its report"
   (= 2 (gethash "updated_count"
                 (memory-record-use
                  '("route-a" "route-b") :consumer "public-response"
                  :generation-id "g1" :user-visible-p t))))
  (let* ((memory (%memory-storage-object
                  "id" "route-tick" "kind" "thought" "content" "fixture"
                  "importance" 0.1d0 "origin_class" "synthetic"
                  "record_type" "hypothesis" "grounding_status" "grounded"
                  "evidence_node_ids" (vector "route-admission")))
         (proposal (%memory-storage-object
                    "tick_type" "idle-drift" "memory_specs" (vector memory)
                    "edge_specs" (vector) "generation_id" "g2")))
    (memory-cognitive-routing-check
     "default tick transaction replaces PostgreSQL when event-first is selected"
     (equal '("route-tick")
            (%tick-commit-default-transaction proposal 42))))
  (memory-cognitive-routing-check
   "all four public operation families crossed exactly once"
   (equal '("admission" "supersession" "user-visible-rehearsal" "tick-commit")
          (nreverse route-calls))))

(format t "~%~d passed, ~d failed~%"
        *memory-cognitive-routing-pass* *memory-cognitive-routing-fail*)
(when (plusp *memory-cognitive-routing-fail*)
  (error "Cognitive memory mutation routing tests failed"))
