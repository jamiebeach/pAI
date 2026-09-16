;;;; knowledge-graph-attention-context-tests.lisp -- pure KG5 frame fixtures.
;;;; harness: full-system

(in-package :agent)

(defvar *kgac-pass* 0)
(defvar *kgac-fail* 0)

(defun kgac-check (name condition)
  (if condition
      (progn (incf *kgac-pass*) (format t "PASS ~a~%" name))
      (progn (incf *kgac-fail*) (format t "FAIL ~a~%" name))))

(defun kgac-node (id kind label evidence)
  (obj "projection_name" "generic-knowledge-graph" "node_id" id
       "node_kind" kind "label" label "aliases" #() "status" "current"
       "disclosure_class" "private" "evidence_event_ids" evidence))

(defun kgac-edge (id from predicate to evidence)
  (obj "projection_name" "generic-knowledge-graph" "edge_id" id
       "from_node_id" from "predicate" predicate "to_node_id" to
       "traversal_direction" "outgoing" "status" "current"
       "evidence_event_ids" evidence))

(defun kgac-path (from-kind from-label predicate to-kind to-label base-id)
  (let ((from (format nil "~a-from" base-id))
        (to (format nil "~a-to" base-id)))
    (obj "projection_name" "generic-knowledge-graph" "depth" 1
         "hybrid_seed" nil
         "nodes" (vector (kgac-node from from-kind from-label #(10 11))
                         (kgac-node to to-kind to-label #(12)))
         "edges" (vector (kgac-edge base-id from predicate to #(13))))))

(defparameter *kgac-fixture-paths*
  (vector
   (kgac-path "person" "FixtureOperator" "requires" "requirement"
              "large print accessible visual encoding" "accessibility")
   (kgac-path "person" "FixtureAgent" "interested-in" "topic"
              "volcanic island geology" "astronomy")))

(defun kgac-search (request semantic episodes)
  (declare (ignore request semantic episodes))
  (obj "schema_version" 1 "status" "available"
       "paths" *kgac-fixture-paths* "path_count" 2
       "non_exhaustive" nil "database_write_count" 0))

(format t "~%== KG5 attention-owned graph context ==~%")

(let ((query (%kgac-query (knowledge-graph-attention-frame
               :attention-kind "conversation" :stimulus "health history"
               :memory-records (vector (obj "source_id" "old-failure"
                   "content" "technical timeout graph not wired"))))))
  (kgac-check "retrieved failure narrative does not become the current query"
    (and (search "health" query) (not (search "timeout" query)))))

(let ((query (%kgac-query
              (knowledge-graph-attention-frame
               :attention-kind "operator-conversation"
               :stimulus "What is my spouse's name?"
               :private-focus-records
               (vector (obj "source_id" "private:distraction"
                            "content" "volcanic island geology"))))))
  (kgac-check "operator conversation excludes distracting private focus"
              (and (search "spouse" query)
                   (not (search "volcanic" query)))))

(let ((captured-policy nil))
  (knowledge-graph-attention-context-records
   (knowledge-graph-attention-frame
    :attention-kind "conversation" :stimulus "FixtureOperator accessibility")
   #() #()
   (lambda (request semantic episodes)
     (declare (ignore semantic episodes))
     (setf captured-policy (gethash "evidence_policy" request))
     (obj "schema_version" 1 "status" "empty" "paths" #()))
   :character-budget 1200)
  (kgac-check "automatic context always requests verified graph evidence"
              (string= "verified" captured-policy)))

(multiple-value-bind (records report)
    (knowledge-graph-attention-context-records
     (knowledge-graph-attention-frame
      :attention-kind "conversation" :stimulus
      "Create a presentation with clear visual encoding")
     #() #() #'kgac-search :character-budget 1200)
  (kgac-check "artifact frame selects the applicable operator requirement"
              (and (= 1 (length records))
                   (search "large print accessible visual encoding"
                           (gethash "content" (aref records 0)))))
  (kgac-check "selected row carries exact graph evidence provenance"
              (equal '(10 11 12 13)
                     (coerce
                      (gethash
                       "evidence_event_ids"
                       (gethash "provenance" (aref records 0)))
                      'list)))
  (kgac-check "selection report is bounded and zero-write"
              (and (string= "selected" (gethash "status" report))
                   (= 0 (gethash "database_write_count" report -1)))))

(multiple-value-bind (records report)
    (knowledge-graph-attention-context-records
     (knowledge-graph-attention-frame
      :attention-kind "curiosity" :stimulus "continue quiet thinking"
      :private-focus-records
      (vector (obj "source_id" 88
                   "content" "Investigate volcanic island geology")))
     #() #() #'kgac-search :character-budget 1200)
  (declare (ignore report))
  (kgac-check "non-conversation private focus can dominate graph relevance"
              (and (= 1 (length records))
                   (search "volcanic island geology"
                           (gethash "content" (aref records 0))))))

(multiple-value-bind (records report)
    (knowledge-graph-attention-context-records
     (knowledge-graph-attention-frame
      :attention-kind "conversation" :stimulus "How are you today?")
     #() #() #'kgac-search :character-budget 1200)
  (kgac-check "unrelated casual frame receives no graph disclosure"
              (and (zerop (length records))
                   (string= "empty" (gethash "status" report)))))

(multiple-value-bind (records report)
    (knowledge-graph-attention-context-records
     (knowledge-graph-attention-frame
      :attention-kind "conversation"
      :stimulus "Create a color-accessible visual artifact")
     #() #()
     (lambda (request semantic episodes)
       (declare (ignore request semantic episodes))
       (obj "schema_version" 1 "status" "available"
            "paths"
            (vector
             (obj "projection_name" "conversation-episode-graph"
                  "depth" 0
                  "nodes"
                  (vector (kgac-node "concept:no-evidence" "concept"
                                     "color accessibility" #()))
                  "edges" #()
                  "hybrid_seed" nil))))
     :character-budget 1200)
  (kgac-check "relevant path without evidence is skipped, not unavailable"
              (and (zerop (length records))
                   (string= "empty" (gethash "status" report)))))

(multiple-value-bind (records report)
    (knowledge-graph-attention-context-records
     (knowledge-graph-attention-frame
      :attention-kind "conversation"
      :stimulus "FixtureOperator accessibility requirement")
     #() #()
     (lambda (request semantic episodes)
       (declare (ignore request semantic episodes))
       (obj
        "schema_version" 1 "status" "available"
        "paths"
        (vector
         (obj "projection_name" "generic-knowledge-graph" "depth" 0
              "nodes" (vector (kgac-node "fixtureoperator" "person" "FixtureOperator" #(91)))
              "edges" #() "hybrid_seed" nil)
         (obj "projection_name" "generic-knowledge-graph" "depth" 1
              "nodes"
              (vector (kgac-node "fixtureoperator" "person" "FixtureOperator" #(91))
                      (kgac-node "accessible" "requirement"
                                 "accessible visual encoding" #(92)))
              "edges"
              (vector (kgac-edge "requires-accessible" "fixtureoperator" "requires"
                                 "accessible" #(93)))
              "hybrid_seed" nil))))
     :character-budget 1200)
  (kgac-check "relationship context displaces an isolated identity seed"
              (and (= 1 (length records))
                   (= 1 (gethash "relationship_candidate_count" report))
                   (search "--requires-->"
                           (gethash "content" (aref records 0))))))

(multiple-value-bind (records report)
    (knowledge-graph-attention-context-records
     (knowledge-graph-attention-frame
      :attention-kind "conversation" :stimulus
      "Create a presentation with clear visual encoding")
     #() #() #'kgac-search :character-budget 40)
  (kgac-check "character ceiling clips rather than leaking a partial row"
              (and (zerop (length records))
                   (gethash "non_exhaustive" report))))

(let* ((graph
         (vector (obj "source_id" "graph:one"
                      "content" (make-string 30 :initial-element #\g)
                      "provenance" (obj "evidence_event_ids" #(7)))))
       (memory
         (vector (obj "source_id" "memory:one"
                      "content" (make-string 30 :initial-element #\m))))
       (merged (multiple-value-list
                (%conversation-merge-graph-memory-records graph memory 40))))
  (kgac-check "graph context displaces only trailing memory under one budget"
              (and (= 1 (length (first merged)))
                   (string= "graph:one"
                            (gethash "source_id" (aref (first merged) 0)))
                   (member 7 (second merged)))))

(let* ((memory
         (vector (obj "source_id" "memory:one"
                      "content" (make-string 30 :initial-element #\m))))
       (merged (multiple-value-list
                (%conversation-merge-graph-memory-records #() memory 40))))
  (kgac-check "empty graph selection preserves the ordinary memory budget"
              (and (= 1 (length (first merged)))
                   (string= "memory:one"
                            (gethash "source_id" (aref (first merged) 0))))))

(defun kgac-final-request (records)
  "Exercise the actual conversation merge and final context assembler."
  (multiple-value-bind (merged evidence)
      (%conversation-merge-graph-memory-records records #() 1600)
    (let ((sections (make-hash-table :test #'equal))
          (budgets (make-hash-table :test #'equal)))
      (loop for name across *conscious-context-section-order*
            do (setf (gethash name sections) #() (gethash name budgets) 1600))
      (setf (gethash "memory-bundles" sections) merged)
      (conscious-context-assemble
        (obj "state_revision" 1 "composition_hash" "graph-context-fixture")
        (make-conscious-assembly-context
          :pulse-id "pulse:graph-context" :purpose "respond" :audience "operator"
          :runtime-revision "graph-context-fixture" :conscious-state-revision 1
          :clock-identity "fixture-clock" :total-character-budget 1600
          :section-character-budgets budgets :sections sections
          :eligible-evidence-ids (coerce evidence 'vector) :available-tools #()
          :permitted-proposal-kinds #( "publication-candidate")
          :publication-constraints (obj "audiences" #( "operator"))
          :remaining-budget (obj "tool_proposals" 0 "continuations" 0 "publication_candidates" 1))))))

(let* ((left-evidence (coerce (loop for id from 1 to 100 collect id) 'vector))
       (right-evidence (coerce (loop for id from 101 to 200 collect id) 'vector))
       (edge-evidence (coerce (loop for id from 201 to 300 collect id) 'vector))
       (path
         (obj "projection_name" "generic-knowledge-graph" "depth" 1
              "nodes"
              (vector (kgac-node "operator" "person" "Operator" left-evidence)
                      (kgac-node "condition" "condition"
                                 "medical condition" right-evidence))
              "edges"
              (vector (kgac-edge "operator-condition" "operator" "has-condition"
                                 "condition" edge-evidence))
              "hybrid_seed" nil))
       (frame (knowledge-graph-attention-frame
               :attention-kind "conversation"
               :stimulus "operator medical condition")))
  (multiple-value-bind (records report)
      (knowledge-graph-attention-context-records
       frame #() #()
       (lambda (&rest ignored)
         (declare (ignore ignored))
         (obj "status" "available" "paths" (vector path)))
       :character-budget 1600)
    (let* ((provenance (gethash "provenance" (aref records 0)))
           (evidence (gethash "evidence_event_ids" provenance)))
      (kgac-check "large graph provenance becomes one balanced bounded witness set"
                  (and (= 64 (length evidence))
                       (find 100 evidence) (find 200 evidence) (find 300 evidence)
                       (= 1 (gethash "evidence_clipped_path_count" report))
                       (gethash "non_exhaustive" report)))
      (kgac-check "bounded graph provenance survives final context assembly"
                  (hash-table-p (kgac-final-request records))))))

(let* ((typed (obj "projection_name" "generic-knowledge-graph" "depth" 1
                   "nodes" (vector (kgac-node "person" "person" "Operator" #(101))
                                   (kgac-node "topic" "concept" "astronomy" #(102)))
                   "edges" (vector (kgac-edge "interest" "person" "interested_in" "topic" #(103)))))
       (source (obj "projection_name" "conversation-episode-graph" "depth" 1
                    "nodes" (vector (kgac-node "episode" "episode" "Navigation astronomy archive" #(104))
                                    (kgac-node "cue" "concept" "navigation astronomy archive" #(104)))
                    "edges" (vector (kgac-edge "cue-edge" "episode" "mentions" "cue" #(104)))))
       (frame (knowledge-graph-attention-frame :attention-kind "conversation"
                                               :stimulus "navigation astronomy archive")))
  (multiple-value-bind (records report)
      (knowledge-graph-attention-context-records frame #() #()
        (lambda (&rest ignored)
          (declare (ignore ignored))
          (obj "status" "available" "paths" (vector source typed)))
        :character-budget 1600)
    (kgac-check "episode overlap cannot threshold out a relevant typed relationship"
      (and (= 2 (length records))
           (search "Typed relationship evidence" (gethash "content" (aref records 0)))
           (search "Episode/source navigation" (gethash "content" (aref records 1)))
           (= 1 (gethash "typed_record_count" report -1))
           (= 1 (gethash "source_record_count" report -1))))
    (let* ((assembled (kgac-final-request records))
           (request (gethash "private_request" assembled)))
      (kgac-check "source distinction and exact provenance survive final request assembly"
        (and (= 2 (length request))
             (every (lambda (row) (equal "memory-data" (gethash "role" row))) request)
             (equalp (map 'vector (lambda (row) (gethash "content" row)) records)
                     (map 'vector (lambda (row) (gethash "content" row)) request))
             (every (lambda (id) (find id (gethash "evidence_event_ids" (gethash "manifest" assembled))))
                    '(101 102 103 104))))))
  (multiple-value-bind (records report)
      (knowledge-graph-attention-context-records frame #() #()
        (lambda (&rest ignored)
          (declare (ignore ignored))
          (obj "status" "available" "paths" (vector source typed)))
        :character-budget 300)
    (declare (ignore report))
    (kgac-check "tight shared budget admits typed knowledge before episode navigation"
      (and (= 1 (length records))
           (search "Typed relationship evidence" (gethash "content" (aref records 0))))))
  (let ((incoming (%kgs-copy-object typed)))
    (setf (gethash "nodes" incoming) (reverse (gethash "nodes" incoming))
          (gethash "traversal_direction" (aref (gethash "edges" incoming) 0)) "incoming")
    (kgac-check "incoming traversal does not reverse the meaning of the typed predicate"
      (search "<--interested_in--" (%kgac-path-text incoming)))))

(format t "~%KG5 attention context: ~d passed, ~d failed.~%"
        *kgac-pass* *kgac-fail*)
(when (plusp *kgac-fail*) (uiop:quit 1))
