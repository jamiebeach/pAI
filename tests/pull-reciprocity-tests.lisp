(in-package :agent)

(ql:quickload '(:bordeaux-threads :shasht) :silent t)

(defvar *pull-test-pass* 0)
(defvar *pull-test-fail* 0)
(defvar *pull-test-snapshot* nil)
(defvar *pull-test-forbidden-calls* 0)

(defun pull-test-check (name condition)
  (if condition
      (progn (incf *pull-test-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *pull-test-fail*) (format t "  FAIL ~a~%" name))))

(defun reciprocity-canary-snapshot () *pull-test-snapshot*)
(defun telegram-send (&rest args) (declare (ignore args))
  (incf *pull-test-forbidden-calls*) (error "transport forbidden"))
(defun initiative-deliver-approved-message (&rest args) (declare (ignore args))
  (incf *pull-test-forbidden-calls*) (error "initiative forbidden"))
(defun call-model (&rest args) (declare (ignore args))
  (incf *pull-test-forbidden-calls*) (error "model forbidden"))
(defun memory-write-node (&rest args) (declare (ignore args))
  (incf *pull-test-forbidden-calls*) (error "memory forbidden"))

(load (test-source "candidate-representation.lisp"))
(load (test-source "pull-reciprocity.lisp"))

(setf *pull-reciprocity-review-file* #P"/tmp/pull-reciprocity-review-test.json"
      *pull-reciprocity-label-file* #P"/tmp/pull-reciprocity-label-test.json")

(defun pull-test-clean ()
  (ignore-errors (delete-file *pull-reciprocity-review-file*))
  (ignore-errors (delete-file *pull-reciprocity-label-file*)))

(defun pull-test-reset ()
  (pull-test-clean)
  (setf *pull-reciprocity-review* nil
        *pull-reciprocity-descriptions* nil
        *pull-reciprocity-batches* nil
        *pull-reciprocity-labels* nil
        *pull-reciprocity-active-label* nil
        *pull-reciprocity-mode* :enabled
        *pull-test-forbidden-calls* 0))

(defun pull-test-record (n &key (artifact-class "internal-stance")
                                (contract "explore-stance-v1")
                                content)
  (obj "id" (format nil "recip-~2,'0d" n)
       "schema_version" 2
       "initiative_decision_id" (format nil "v2-~2,'0d" n)
       "source" "explore-development" "source_id" (format nil "source-~d" n)
       "topic" (format nil "topic-~d" n)
       "content" (or content (format nil "PRIVATE-INTERNAL-CONTENT-~d" n))
       "artifact_class" artifact-class "generation_contract" contract
       "composition_eligible" (string= artifact-class "rendered-draft")
       "evidence_node_ids" (vector (format nil "evidence-~d" n))
       "status" "withheld" "reason" "v2-hard-gate" "created_at" (+ 1000 n)))

(defun pull-test-snapshot (records &key at-cap pruned-count (status "available"))
  (obj "status" status "reason" (if (string= status "available") :null "fixture-unavailable")
       "records" (coerce records 'vector) "record_cap" 200
       "at_cap" (if at-cap t nil) "pruned_count" (or pruned-count :null)))

(pull-test-reset)
(setf *pull-test-snapshot* (pull-test-snapshot (list (pull-test-record 1))))

(let* ((reply (pull-reciprocity-handle-inbound "anything on your mind?"
                                                :cause-id "inbound-1"
                                                :channel "telegram"))
       (text (gethash "text" reply)))
  (pull-test-check "natural surface is an inbound-caused ordinary reply"
                   (and (string= "reply" (gethash "kind" reply))
                        (string= "inbound-1" (gethash "inbound_cause_id" reply))))
  (pull-test-check "normal mind surface describes but never quotes internal stance"
                   (and (search "internal material" text :test #'char-equal)
                        (not (search "PRIVATE-INTERNAL-CONTENT" text)))))

(let ((reply (pull-reciprocity-handle-inbound "/mind raw 1"
                                               :cause-id "raw-1"
                                               :channel "telegram")))
  (pull-test-check "explicit raw audit exposes typed source material with warning"
                   (let ((text (gethash "text" reply)))
                     (and (search "Explicit raw audit" text)
                          (search "internal-stance" text)
                          (search "PRIVATE-INTERNAL-CONTENT-1" text)))))

(pull-reciprocity-handle-inbound "/mind" :cause-id "desc-2" :channel "telegram")
(let ((reply (pull-reciprocity-handle-inbound "/mind" :cause-id "desc-3"
                                               :channel "telegram")))
  (pull-test-check "describe-not-quote has a persistent global 24-hour cap"
                   (and (= 2 (length *pull-reciprocity-descriptions*))
                        (search "24-hour cap" (gethash "text" reply))
                        (probe-file *pull-reciprocity-review-file*))))

(setf *pull-reciprocity-descriptions* nil)
(let* ((before (shasht:write-json (gethash "records" *pull-test-snapshot*) nil))
       (reply (pull-reciprocity-handle-inbound "/label"
                                               :cause-id "label-start"
                                               :channel "telegram"))
       (text (gethash "text" reply)))
  (pull-test-check "internal artifact receives material-triage question without quote"
                   (and (search "worth saying" text)
                        (not (search "PRIVATE-INTERNAL-CONTENT" text))))
  (pull-reciprocity-handle-inbound "/label yes"
                                   :cause-id "label-choice" :channel "telegram")
  (let ((label (first *pull-reciprocity-labels*)))
    (pull-test-check "typed triage label persists immutable semantics and cause"
                     (and (string= "material-triage-v1" (gethash "label_kind" label))
                          (string= "yes" (gethash "choice" label))
                          (string= "label-choice" (gethash "inbound_cause_id" label))
                          (probe-file *pull-reciprocity-label-file*))))
  (pull-test-check "triage leaves candidate snapshot byte-equivalent"
                   (string= before
                            (shasht:write-json (gethash "records" *pull-test-snapshot*) nil))))

(pull-test-reset)
(setf *pull-test-snapshot*
      (pull-test-snapshot
       (list (pull-test-record
              2 :artifact-class "rendered-draft"
              :contract "semantic-publication-v2"
              :content "I have a concrete update for you about the continuity design."))))
(let ((reply (pull-reciprocity-handle-inbound "/label"
                                               :cause-id "draft-label"
                                               :channel "web")))
  (pull-test-check "rendered draft receives the distinct send-ready-floor label"
                   (and (search "send-ready" (gethash "text" reply))
                        (string= "send-ready-floor-v1"
                                 (gethash "label_kind" *pull-reciprocity-active-label*)))))

(pull-test-check "A/B/neither semantics are truthfully quarantined"
                 (search "quarantined"
                         (gethash "text" (pull-reciprocity-handle-inbound
                                          "/label A" :cause-id "old-label"
                                          :channel "telegram"))))
(pull-test-check "preference mode stays unavailable before authored drafts qualify"
                 (search "unavailable"
                         (gethash "text" (pull-reciprocity-handle-inbound
                                          "/label preference" :cause-id "pref"
                                          :channel "telegram"))))

(pull-test-reset)
(%pull-atomic-write
 *pull-reciprocity-label-file*
 (obj "schema_version" 1
      "batches" (vector (obj "id" "legacy-batch" "pairs" (vector)))))
(%pull-load)
(let ((batch (first *pull-reciprocity-batches*)))
  (pull-test-check "legacy pair batches reload only as quarantined evidence"
                   (and (gethash "quarantined" batch)
                        (string= "superseded-send-worthiness-pair-v1"
                                 (gethash "label_semantics" batch)))))

(setf *pull-test-snapshot* (pull-test-snapshot nil))
(pull-test-check "empty state is truthful"
                 (search "no safely reviewable"
                         (gethash "text" (pull-reciprocity-handle-inbound
                                          "/mind" :cause-id "empty" :channel "terminal"))))
(setf *pull-test-snapshot* (pull-test-snapshot nil :status "unavailable"))
(pull-test-check "unavailable state does not invent a candidate"
                 (search "source is unavailable"
                         (gethash "text" (pull-reciprocity-handle-inbound
                                          "/mind" :cause-id "unavailable"
                                          :channel "terminal"))))

(let* ((source (string-downcase
                (uiop:read-file-string (test-source "pull-reciprocity.lisp"))))
       (forbidden '("telegram-send" "initiative-deliver-approved-message"
                    "pai-maybe-initiate" "call-model" "memory-write-node"
                    "contact-policy")))
  (pull-test-check "module has no delivery/model/memory/contact symbol reference"
                   (notany (lambda (name) (search name source)) forbidden)))
(pull-test-check "all transport/model/memory traps remain unused"
                 (zerop *pull-test-forbidden-calls*))

(let ((report (pull-reciprocity-report)))
  (pull-test-check "content-free schema-2 report exposes representation state"
                   (and (= 2 (gethash "schema_version" report))
                        (hash-table-p (gethash "representation" report))
                        (null (gethash "delivery_capable" report)))))

(setf *pull-reciprocity-mode* :off)
(pull-test-check "off mode is a true command fall-through"
                 (null (pull-reciprocity-handle-inbound "/mind"
                                                        :cause-id "off"
                                                        :channel "telegram")))

(pull-test-clean)
(format t "~&pull reciprocity tests: ~d passed, ~d failed.~%"
        *pull-test-pass* *pull-test-fail*)
(when (plusp *pull-test-fail*) (sb-ext:exit :code 1))
