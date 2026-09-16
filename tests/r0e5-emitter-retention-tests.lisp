(defpackage :agent (:use :cl))
(in-package :agent)

(ql:quickload '(:postmodern :bordeaux-threads :shasht :ironclad :babel
                :dexador)
              :silent t)

(defvar *r0e5-emitter-pass* 0)
(defvar *r0e5-emitter-fail* 0)

(defun r0e5-emitter-check (name condition)
  (if condition
      (progn (incf *r0e5-emitter-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *r0e5-emitter-fail*) (format t "  FAIL ~a~%" name))))

(defun obj (&rest pairs)
  (loop with result = (make-hash-table :test #'equal)
        for (key value) on pairs by #'cddr
        do (setf (gethash key result) value)
        finally (return result)))

(defvar *grounded-agency-mode* :shadow)
(defvar *autonomous-write-mode* :normal)

(load (test-source "stabilization-config.lisp"))
(load (test-source "memory-nodes.lisp"))
(load (test-source "first-person-evidence.lisp"))
(load (test-source "memory-atom-shadow.lisp"))

(format t "~%== PostgreSQL-owned emitter strings ==~%")
(let ((captured nil)
      (saved (and (fboundp 'log-postgres-row-state)
                  (fdefinition 'log-postgres-row-state))))
  (unwind-protect
       (progn
         (setf (fdefinition 'log-postgres-row-state)
               (lambda (&rest arguments) (setf captured arguments)))
         (let ((raw
                 "{\"id\":\"proposal-1\",\"summary\":\"the agent’s 0.001 pool note\"}"))
           (%fpe-emit-grounded-row-json "grounded_project_proposals" raw)
           (r0e5-emitter-check
            "grounded-agency emitter passes its original row string"
            (and (= 5 (length captured))
                 (string= raw (fifth captured))
                 (string= "proposal-1" (gethash "id" (fourth captured))))))
         (let ((raw
                 "{\"rollout_id\":\"rollout-1\",\"max_cost_credits\":0.001}"))
           (setf captured nil)
           (%memory-atom-shadow-emit-row "memory_atom_rollouts" raw)
           (r0e5-emitter-check
            "memory-atom emitter passes its original decimal spelling"
            (and (= 5 (length captured))
                 (string= raw (fifth captured))
                 (string= "rollout-1"
                          (gethash "rollout_id" (third captured)))))))
    (if saved
        (setf (fdefinition 'log-postgres-row-state) saved)
        (fmakunbound 'log-postgres-row-state))))

(format t "~%== memory row payload retention ==~%")
(let ((events nil)
      (saved (fdefinition '%memory-queue-durable-event)))
  (unwind-protect
       (progn
         (setf (fdefinition '%memory-queue-durable-event)
               (lambda (type payload) (push (list type payload) events)))
         (let* ((node-raw "{\"id\":\"node-1\",\"activation\":0.001}")
                (node (shasht:read-json node-raw))
                (edge-raw
                  "{\"id\":7,\"from_id\":\"node-1\",\"to_id\":\"node-2\",\"edge_type\":\"supports\"}")
                (edge (shasht:read-json edge-raw)))
           (%memory-queue-node-state node "update" "fixture" node-raw)
           (%memory-queue-edge-state edge "delete" "fixture" edge-raw)
           (let ((node-payload (second (find "memory-node-state" events
                                             :key #'first :test #'string=)))
                 (edge-payload (second (find "memory-edge-state" events
                                             :key #'first :test #'string=))))
             (r0e5-emitter-check "memory-node payload retains exact row string"
                                  (string= node-raw
                                           (gethash "row_json" node-payload)))
             (r0e5-emitter-check "memory-edge delete retains pre-delete string"
                                  (string= edge-raw
                                           (gethash "row_json" edge-payload))))))
    (setf (fdefinition '%memory-queue-durable-event) saved)))

(format t "~%R0e5 emitter retention: ~d passed, ~d failed~%"
        *r0e5-emitter-pass* *r0e5-emitter-fail*)
(when (plusp *r0e5-emitter-fail*) (uiop:quit 1))
