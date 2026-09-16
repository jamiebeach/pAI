;;;; spreading-activation.lisp -- (spreading activation) +
;;;; P8.2 (intrusion injection).
;;;;
;;;; 2026-07-27. Intent, from the backlog itself: "the largest single lever
;;;; for human-likeness in the entire document." Right now MEMORY-RECALL
;;;; only ever runs when something deliberately decides to search -- the
;;;; least human property of the memory system. This makes memories surface
;;;; UNBIDDEN: seeded from whatever's currently being talked about, spread
;;;; across the edge graph already built (elaborates/contradicts/causes/
;;;; about/follows/evidence-for/derived-from), and anything crossing a
;;;; threshold gets injected into its context explicitly marked as an
;;;; intrusion, not a retrieval result -- the framing matters, per the
;;;; backlog: it arrives unbidden.
;;;;
;;;; Built pragmatically against the current architecture, same reasoning
;;;; as everything else added this pass -- no P0.1 refactor, no virtual
;;;; clock. Spread activation is computed FRESH each turn (a transient,
;;;; cue-driven boost) rather than written back into memory_nodes.activation
;;;; -- P1.4's long-term decay-based activation and this turn's associative
;;;; spread are deliberately kept separate, so a moment of loose association
;;;; doesn't permanently alter a memory's baseline salience.
;;;;
;;;; Injected via the same CONTINUITY:BEGIN/END-style marker-refresh trick
;;;; already used for continuity/affect (tick-loop.lisp/modulator.lisp) --
;;;; a new INTRUSIONS:BEGIN/END pair, refreshed on every AUTO-TURN call
;;;; using the just-arrived prompt as the spreading-activation seed context.
;;;;
;;;; Load live (no restart) via lisp-eval or repl-drop:
;;;;   (load "/agent/state/spreading-activation.lisp")
;;;; Requires memory-nodes.lisp already loaded; modulator.lisp optional
;;;; (arousal-biased threshold, FBOUNDP-guarded).

(in-package :agent)

(export '(compute-intrusions spread-activation))

;;; --- spreading activation -----------------------------------------

(defparameter *spread-edge-conductance*
  (obj "elaborates" 0.9 "evidence-for" 0.7 "about" 0.6 "follows" 0.5
       "derived-from" 0.6 "causes" 0.7 "contradicts" 0.3)
  "How strongly activation propagates across each edge type. elaborates
conducts strongly (a thought elaborating on something IS closely
associated with it); contradicts conducts weakly but non-zero -- a
contradiction is still a real, notable association, per the backlog's
own framing, just a fainter one.")
(defparameter *spread-decay-per-hop* 0.6)
(defparameter *spread-max-hops* 3)
(defparameter *spread-fanout-cap* 5
  "At most this many neighbours explored per node per hop -- bounds the
traversal without needing a global cap to kick in on a densely-linked node.")
(defparameter *spread-node-cap* 30
  "Total distinct nodes touched across the whole traversal, hard stop.")

(defun %spread-neighbors (node-id edges)
  "EDGES is the full edge list (FROM-ID TO-ID EDGE-TYPE). Returns
neighbours of NODE-ID in EITHER direction -- activation should spread
along a relationship regardless of which node the edge was recorded FROM,
same as a real association works both ways."
  (remove-duplicates
   (append
    (loop for e in edges when (string= (first e) node-id) collect (list (second e) (third e)))
    (loop for e in edges when (string= (second e) node-id) collect (list (first e) (third e))))
   :key #'first :test #'string=))

(defun spread-activation (seed-ids)
  "Returns an alist (node-id . boost), sorted strongest-first, for nodes
reached via edge traversal from SEED-IDS -- excludes the seeds themselves.
Pure, transient: reads memory_edges, writes nothing back to the database."
  (let* ((edges (with-pg (pomo:query "SELECT from_id, to_id, edge_type FROM memory_edges")))
         (boosts (make-hash-table :test #'equal))
         (visited (make-hash-table :test #'equal))
         (frontier (mapcar (lambda (id) (cons id 1.0d0)) seed-ids)))
    (dolist (s seed-ids) (setf (gethash s visited) t))
    (dotimes (hop *spread-max-hops*)
      (when (>= (hash-table-count boosts) *spread-node-cap*) (return))
      (let ((next-frontier nil))
        (dolist (fnode frontier)
          (let* ((node-id (car fnode)) (strength (cdr fnode))
                 (neighbors (%spread-neighbors node-id edges))
                 (capped (subseq neighbors 0 (min *spread-fanout-cap* (length neighbors)))))
            (dolist (n capped)
              (destructuring-bind (nid etype) n
                (unless (gethash nid visited)
                  (let* ((conductance (or (gethash etype *spread-edge-conductance*) 0.4))
                         (new-strength (* strength *spread-decay-per-hop* conductance)))
                    (when (> new-strength 0.01)
                      (incf (gethash nid boosts 0.0d0) new-strength)
                      (push (cons nid new-strength) next-frontier))))))))
        (dolist (nf next-frontier) (setf (gethash (car nf) visited) t))
        (setf frontier next-frontier)))
    (let (result)
      (maphash (lambda (k v) (push (cons k v) result)) boosts)
      (sort result #'> :key #'cdr))))

;;; --- intrusion injection -------------------------------------------

(defparameter *intrusion-base-threshold* 0.15
  "Combined spread-activation boost above which a node is eligible to
intrude. Deliberately conservative to start -- the backlog's own note:
too low and it's incoherent, too high and it never fires; this is the
parameter most worth tuning by feel.")
(defparameter *intrusion-max-per-turn* 2)
(defparameter *intrusion-refractory-seconds* (* 30 60)
  "A node that just intruded won't intrude again for this long, even if
still above threshold -- otherwise the same association would surface
every single turn.")
(defvar *intrusion-recent* (make-hash-table :test #'equal)
  "node-id -> universal-time last intruded.")

(defun %spread-seeds-from-text (text &key (k 3))
  (handler-case (mapcar (lambda (e) (gethash "id" e)) (memory-recall text :k k :debug t))
    (error () nil)))

(defun compute-intrusions (context-text)
  "Runs spreading activation from CONTEXT-TEXT's nearest-neighbour nodes as
seeds; returns up to *INTRUSION-MAX-PER-TURN* node contents whose spread
boost crosses threshold, excluding anything still in its refractory
period. Threshold is biased by AROUSAL (modulator.lisp, if loaded, else
unbiased): high arousal narrows intrusion to only strongly-boosted
(affectively congruent) nodes; low arousal permits looser, more
associative drift through -- per the backlog's own framing."
  (handler-case
      (let* ((seeds (%spread-seeds-from-text context-text)))
        (if (null seeds)
            nil
            (let* ((boosts (spread-activation seeds))
                   (arousal (if (fboundp 'modulator-value) (funcall 'modulator-value "arousal") 0.3))
                   (threshold (+ *intrusion-base-threshold* (* arousal 0.15)))
                   (now (get-universal-time))
                   (eligible
                     (remove-if
                      (lambda (b)
                        (let ((last (gethash (car b) *intrusion-recent*)))
                          (and last (< (- now last) *intrusion-refractory-seconds*))))
                      (remove-if (lambda (b) (< (cdr b) threshold)) boosts)))
                   (chosen (subseq eligible 0 (min *intrusion-max-per-turn* (length eligible)))))
              (dolist (c chosen) (setf (gethash (car c) *intrusion-recent*) now))
              (remove nil (mapcar (lambda (c) (let ((node (memory-get-node (car c)))) (and node (gethash "content" node)))) chosen)))))
    (error (e) (format t "~&[spreading-activation] compute-intrusions failed: ~a~%" e) nil)))

;;; --- wire into every real turn --------------------------------------------

(defun %intrusion-context-text (prompt)
  "PROMPT (AUTO-TURN's argument) is usually a string, but can be a content-
parts vector for an image message (web-terminal.lisp) -- extract just the text
part in that case, or skip (return NIL) if there's nothing textual to
seed from."
  (cond ((and (stringp prompt) (plusp (length prompt))) prompt)
        ((vectorp prompt)
         (loop for part across prompt
               when (and (hash-table-p part) (string= (gethash "type" part "") "text"))
                 return (gethash "text" part)))
        (t nil)))

(defun %intrusion-refresh-section (prompt)
  (when (and (fboundp 'context-projection-legacy-mutation-enabled-p)
             (not (context-projection-legacy-mutation-enabled-p)))
    (return-from %intrusion-refresh-section nil))
  (let ((sysmsg (find "system" *last-self-mod-history* :key (lambda (m) (gethash "role" m)) :test #'string=))
        (context-text (%intrusion-context-text prompt)))
    (when (and sysmsg context-text)
      (let* ((intrusions (compute-intrusions context-text))
             (content (gethash "content" sysmsg))
             (begin "<!-- INTRUSIONS:BEGIN -->") (end "<!-- INTRUSIONS:END -->")
             (bp (and (stringp content) (search begin content)))
             (ep (and (stringp content) (search end content)))
             (text (if intrusions (format nil "~{- ~a~%~}" intrusions) "(nothing surfacing unbidden right now)")))
        (if (and bp ep (< bp ep))
            (setf (gethash "content" sysmsg)
                  (concatenate 'string (subseq content 0 (+ bp (length begin)))
                               (format nil "~%~a~%" text) (subseq content ep)))
            (when (stringp content)
              (setf (gethash "content" sysmsg)
                    (format nil "~a~%~%## Private contextual associations~%The following rows are private evidence, not public wording. Integrate only genuinely relevant meaning into natural speech; never reproduce a row or its label verbatim.~%~a~%~a~%~a"
                            content begin text end))))))))

(unless (fboundp 'pai-base-auto-turn-intrusion)
  (setf (fdefinition 'pai-base-auto-turn-intrusion) (fdefinition 'auto-turn)))
(defun auto-turn (prompt)
  (ignore-errors (%intrusion-refresh-section prompt))
  (funcall 'pai-base-auto-turn-intrusion prompt))
