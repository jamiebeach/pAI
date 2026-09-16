;;;; Lab-only data codec. No readers, evaluation, storage, or startup side effects.
(in-package :pai.context-graph)

(defparameter *cgl-structures* (make-hash-table :test #'equal))

(defmacro %cgl-register (tag type constructor &rest slots)
  (let ((prefix (symbol-name type)))
    `(setf (gethash ,tag *cgl-structures*)
           (list #',(intern (concatenate 'string prefix "-P")) #',constructor
                 (list ,@(loop for slot in slots
                               for accessor = (intern (format nil "~a-~a" prefix slot))
                               collect `(list ,(string-downcase slot)
                                              #',accessor
                                              (lambda (object value)
                                                (setf (,accessor object) value)))))))))

(defun %cgl-empty-owner () (%cgi-owner-create nil nil nil))

(%cgl-register "graph" context-graph %make-context-graph
  ontology entities entity-index facts current-triples episodes corrections
  authority-profile authority-partition entity-versions current-entity-versions
  revision-lineage application-receipts entity-scan-index fact-scan-index
  entity-adjacency through-event-id projection-digest)
(%cgl-register "owner" cgi-owner
  ;; Constructor adapter below supplies the required arguments.
  %cgl-empty-owner
  graph agent-id persona-id protocol revision last-id opens terminals phases tasks applications)
(%cgl-register "runtime" context-graph-runtime %make-cg-runtime
  graph agent-id persona-id ontology revision opens terminals phases covered
  last-event-id applications rejections)

(defun %cgl-json (value)
  ;; JSON object ordering is not preserved by every host serializer. Encoded
  ;; documents have one object wrapper and exclusively tagged arrays beneath it.
  ;; Keep the v1 wire order stable, including for already-sealed checkpoints.
  (let ((*print-pretty* nil))
    (shasht:write-json
     (if (and (hash-table-p value)
              (equal "lab-object-graph-v1" (gethash "codec" value)))
         (%cg-object "codec" (gethash "codec" value)
                     "root" (gethash "root" value) "nodes" (gethash "nodes" value))
         value) nil)))

(defun %cgl-key-order (key)
  "Only data keys used by production indexes; independent of hash iteration order."
  (cond ((null key) "N")
        ((stringp key) (concatenate 'string "S" (%cgl-json key)))
        ((integerp key) (format nil "I~d" key))
        ((consp key) (format nil "C~a/~a" (%cgl-key-order (car key))
                            (%cgl-key-order (cdr key))))
        (t (error "Unsupported checkpoint index key"))))

(defun context-graph-lab-encode (value)
  "Typed JSON object graph. Preserve table tests, cons keys and shared references."
  (let ((seen (make-hash-table :test #'eq))
        (nodes (make-array 0 :adjustable t :fill-pointer 0)))
    (labels ((encode (item)
               (cond
                 ((null item) (vector "nil"))
                 ((eq t item) (vector "true"))
                 ((member item '(:null :true :false)) (vector "keyword" (symbol-name item)))
                 ((stringp item) (vector "string" (copy-seq item)))
                 ((integerp item) (vector "integer" (format nil "~d" item)))
                 ((rationalp item) (vector "ratio" (encode (numerator item)) (encode (denominator item))))
                 ((floatp item)
                  (multiple-value-bind (significand exponent sign) (integer-decode-float item)
                    (vector "float" (if (typep item 'double-float) "double" "single")
                            (encode significand) exponent sign)))
                 (t
                  (multiple-value-bind (old found) (gethash item seen)
                    (when found (return-from encode (vector "ref" old))))
                  (let ((id (length nodes)))
                    (setf (gethash item seen) id)
                    (vector-push-extend :null nodes)
                    (setf (aref nodes id)
                          (cond
                            ((hash-table-p item)
                             (let ((test (hash-table-test item)))
                               (unless (member test '(equal eql))
                                 (error "Unsupported checkpoint table test"))
                               (when (eq test 'eql)
                                 (unless (loop for key being the hash-keys of item
                                               always (integerp key))
                                   (error "Checkpoint EQL indexes require integer keys")))
                               (vector "table" (symbol-name test)
                                       (coerce (loop for key in
                                                     (sort (loop for key being the hash-keys of item collect key)
                                                           #'string< :key #'%cgl-key-order)
                                                     collect (vector (encode key) (encode (gethash key item))))
                                               'vector))))
                            ((consp item) (vector "cons" (encode (car item)) (encode (cdr item))))
                            ((vectorp item) (vector "vector" (map 'vector #'encode item)))
                            (t
                             (let ((tag (loop for tag being the hash-keys of *cgl-structures*
                                             using (hash-value spec)
                                             when (funcall (first spec) item) return tag)))
                               (unless tag (error "Unsupported checkpoint value type ~a" (type-of item)))
                               (vector tag
                                       (coerce (loop for (name getter) in (third (gethash tag *cgl-structures*))
                                                     collect (vector name (encode (funcall getter item))))
                                               'vector))))))
                    (vector "ref" id))))))
      (let ((root (encode value)))
        (%cg-object "codec" "lab-object-graph-v1" "root" root "nodes" nodes)))))

(defun context-graph-lab-decode (document)
  "Decode only the closed data vocabulary; never invoke the Lisp reader."
  (unless (equal "lab-object-graph-v1" (gethash "codec" document))
    (error "Checkpoint codec mismatch"))
  (let* ((nodes (gethash "nodes" document))
         (objects (make-array (length nodes))))
    (loop for node across nodes for id from 0 for tag = (aref node 0) do
      (setf (aref objects id)
            (cond ((equal tag "table")
                   (make-hash-table :test (cond ((equal "EQUAL" (aref node 1)) 'equal)
                                               ((equal "EQL" (aref node 1)) 'eql)
                                               (t (error "Checkpoint table test invalid")))))
                  ((equal tag "cons") (cons nil nil))
                  ((equal tag "vector") (make-array (length (aref node 1))))
                  ((gethash tag *cgl-structures*)
                   (funcall (second (gethash tag *cgl-structures*))))
                  (t (error "Checkpoint node tag invalid")))))
    (labels ((decode (item)
               (let ((tag (aref item 0)))
                 (cond ((equal tag "nil") nil) ((equal tag "true") t)
                       ((equal tag "keyword")
                        (or (find (aref item 1) '(:null :true :false) :key #'symbol-name :test #'equal)
                            (error "Checkpoint keyword invalid")))
                       ((equal tag "string") (copy-seq (aref item 1)))
                       ((equal tag "integer") (parse-integer (aref item 1)))
                       ((equal tag "ratio") (/ (decode (aref item 1)) (decode (aref item 2))))
                       ((equal tag "float")
                        (* (aref item 4) (scale-float
                                          (coerce (decode (aref item 2))
                                                  (cond ((equal "double" (aref item 1)) 'double-float)
                                                        ((equal "single" (aref item 1)) 'single-float)
                                                        (t (error "Checkpoint float type invalid"))))
                                          (aref item 3))))
                       ((equal tag "ref") (aref objects (aref item 1)))
                       (t (error "Checkpoint value tag invalid"))))))
      ;; Populate cons/vector/structure objects before hashing compound keys.
      (loop for node across nodes for object across objects for tag = (aref node 0)
            unless (equal tag "table") do
              (cond ((equal tag "cons") (setf (car object) (decode (aref node 1))
                                              (cdr object) (decode (aref node 2))))
                    ((equal tag "vector")
                     (loop for value across (aref node 1) for i from 0 do
                       (setf (aref object i) (decode value))))
                    (t (let ((slots (third (gethash tag *cgl-structures*))))
                         (unless (equal (mapcar #'first slots)
                                        (map 'list (lambda (row) (aref row 0)) (aref node 1)))
                           (error "Checkpoint structure slots mismatch"))
                         (loop for slot in slots for row across (aref node 1) do
                           (funcall (third slot) object (decode (aref row 1))))))))
      (loop for node across nodes for object across objects
            when (equal "table" (aref node 0)) do
              (loop for pair across (aref node 2) do
                (setf (gethash (decode (aref pair 0)) object) (decode (aref pair 1)))))
      (decode (gethash "root" document)))))

(defun %cgl-check-contract (contract runtime owner)
  (unless (and (hash-table-p contract)
               (or (and (equal "reviewed-inference-v8" (gethash "profile" contract))
                        (equal "identity-formation-owner-v8" (gethash "generation" contract))
                        (equal "identity-formation-v13" (gethash "protocol" contract))
                        (equal "personal-context-core-glm53-v1.2"
                               (gethash "ontology_revision" contract)))
                   (and (equal "reviewed-inference-v9" (gethash "profile" contract))
                        (equal "identity-formation-owner-v9" (gethash "generation" contract))
                        (equal "identity-formation-v14" (gethash "protocol" contract))
                        (equal "personal-context-core-glm53-v1.3"
                               (gethash "ontology_revision" contract))))
               (every (lambda (key) (let ((value (gethash key contract)))
                                     (and (stringp value) (plusp (length value)))))
                      '("profile" "generation" "protocol" "ontology_revision"
                        "agent_id" "persona_id" "compatibility" "origin_digest"))
               (integerp (gethash "cutoff" contract))
               (not (minusp (gethash "cutoff" contract)))
               (integerp (gethash "recovery_position" contract))
               (not (minusp (gethash "recovery_position" contract)))
               (eq (cgi-owner-graph owner) (context-graph-runtime-graph runtime))
               (equal (gethash "generation" contract) (cgi-owner-protocol owner))
               (equal (gethash "ontology_revision" contract) (cgi-owner-revision owner))
               (equal (gethash "ontology_revision" contract) (context-graph-runtime-revision runtime))
               (every (lambda (pair)
                        (and (equal (gethash (first pair) contract) (second pair))
                             (equal (second pair) (third pair))))
                      (list (list "agent_id" (cgi-owner-agent-id owner)
                                  (context-graph-runtime-agent-id runtime))
                            (list "persona_id" (cgi-owner-persona-id owner)
                                  (context-graph-runtime-persona-id runtime))))
               (<= (cgi-owner-last-id owner) (gethash "cutoff" contract))
               (<= (context-graph-runtime-last-event-id runtime) (gethash "cutoff" contract)))
    (error "Checkpoint explicit contract or state mismatch")))

(defun context-graph-lab-checkpoint-seal (runtime owner source-index contract)
  "Return a data envelope. Caller pins the returned digest in the private manifest."
  (%cgl-check-contract contract runtime owner)
  (let* ((document (context-graph-lab-encode
                    (vector contract runtime owner source-index)))
         (digest (%cg-sha256 "lab-checkpoint-v1" (%cgl-json document))))
    (%cg-object "schema" "lab-checkpoint-v1" "digest" digest "document" document)))

(defun context-graph-lab-checkpoint-open (envelope expected-digest expected-contract)
  "No fallback reconstruction. Require manifest-pinned integrity and exact identity."
  (let ((document (gethash "document" envelope)))
    (unless (and (equal "lab-checkpoint-v1" (gethash "schema" envelope))
                 (stringp expected-digest)
                 (equal expected-digest (gethash "digest" envelope))
                 (equal expected-digest (%cg-sha256 "lab-checkpoint-v1" (%cgl-json document))))
      (error "Checkpoint integrity mismatch"))
    (let* ((root (context-graph-lab-decode document))
           (contract (aref root 0)) (runtime (aref root 1)) (owner (aref root 2)))
      (unless (equal (%cgl-json (context-graph-lab-encode contract))
                     (%cgl-json (context-graph-lab-encode expected-contract)))
        (error "Checkpoint compatibility or authority contract mismatch"))
      (%cgl-check-contract contract runtime owner)
      (values runtime owner (aref root 3)))))
