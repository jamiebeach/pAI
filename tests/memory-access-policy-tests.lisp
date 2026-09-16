;;;; harness: bare
(require :asdf)
(unless (find-package :ql)
  (load (or (uiop:getenv "PAI_QUICKLISP_SETUP") "/opt/quicklisp/setup.lisp")))
(asdf:load-asd (merge-pathnames "../pai-memory-access.asd" *load-truename*))
(asdf:load-system :pai-memory-access)
(in-package :pai.memory-access)

(defvar *ma-checks* 0)
(defun ma-check (name value)
  (unless value (error "FAIL ~a" name))
  (incf *ma-checks*) (format t "PASS ~a~%" name))
(defun ma-fixture ()
  (let* ((partition (%ma-object "agent_id" "agent:fixture" "persona_id" "persona:fixture"))
         (ref (%ma-object "store" "semantic" "resource_id" "memory:fixture"
                          "version_id" "version:one" "component" "content"))
         (policy (%ma-object "policy_id" "policy:owner" "policy_version" "1"))
         (protection (%ma-object
                      "schema_version" 1 "protection_revision" "memory-protection-v1"
                      "resource_ref" ref "partition" partition
                      "controller_principal_ids" #("owner") "subject_principal_ids" #("owner")
                      "sensitivity" "sensitive" "categories" #("health" "food-preference")
                      "policy_refs" (vector policy) "dependency_refs" #() "lineage_complete" :true))
         (context (%ma-object
                   "schema_version" 1 "executor_principal_id" "agent" "authority_principal_id" "owner"
                   "recipient_principal_id" :null "recipient_binding_id" :null "recipient_set_digest" :null
                   "task_id" :null "purpose" "private-planning" "channel_id" "private"
                   "partition" partition "action" "read" "grant_ids" #() "now_utc" 100 "policy_epoch" 1))
         (rule (%ma-object
                "rule_id" "allow:owner" "policy_id" "policy:owner" "policy_version" "1"
                "effect" "allow" "executor_principal_ids" #("agent") "recipient_principal_ids" #()
                "actions" #("read" "derive") "purposes" #("private-planning") "channels" #("private")
                "allowed_components" #("content" "claim") "require_task_grant" :false
                "resource_selectors"
                (vector (%ma-object "partition" partition "resource_refs" #()
                                    "categories" #("health" "food-preference") "maximum_sensitivity" "restricted"))))
         (snapshot (%ma-object
                    "schema_version" 1 "policy_revision" "memory-access-policy-v1" "epoch" 1
                    "principals" (vector
                                   (%ma-object "principal_id" "owner" "kind" "human" "identity_binding_id" "owner:binding" "status" "verified")
                                   (%ma-object "principal_id" "agent" "kind" "agent" "identity_binding_id" "agent:binding" "status" "verified"))
                    "protections" (vector protection) "rules" (vector rule)
                    "tasks" #() "grants" #() "recipes" #() "recipe_authorizations" #() "revoked_grant_ids" #())))
    (values ref context snapshot)))
(defun ma-case (name mutation expected)
  (multiple-value-bind (ref context snapshot) (ma-fixture)
    (funcall mutation ref context snapshot)
    (let* ((before (memory-access-canonical-json (vector ref context snapshot)))
           (result (memory-access-decide ref context snapshot)))
      (ma-check name (equal expected (gethash "reason_code" result)))
      (ma-check "read evaluator leaves inputs unchanged"
                (equal before (memory-access-canonical-json (vector ref context snapshot)))))))

(ma-case "verified private planning reads mixed protected memory"
         (lambda (&rest ignored) (declare (ignore ignored))) "POLICY_SATISFIED")
(ma-case "third party cannot assert operator identity through chat text"
         (lambda (r c s) (declare (ignore r s)) (setf (gethash "executor_principal_id" c) "stranger"))
         "IDENTITY_UNVERIFIED")
(ma-case "revoked executor denied"
         (lambda (r c s) (declare (ignore r c))
           (setf (gethash "status" (aref (gethash "principals" s) 1)) "revoked")) "IDENTITY_UNVERIFIED")
(ma-case "different partition denied"
         (lambda (r c s) (declare (ignore r))
           (setf (gethash "partition" (aref (gethash "protections" s) 0))
                 (%ma-object "agent_id" "other" "persona_id" (gethash "persona_id" (gethash "partition" c)))))
         "PARTITION_DENIED")
(ma-case "missing protection is never public"
         (lambda (r c s) (declare (ignore r c)) (setf (gethash "protections" s) #())) "PROTECTION_MISSING")
(ma-case "stale policy epoch denied"
         (lambda (r c s) (declare (ignore r s)) (setf (gethash "policy_epoch" c) 2)) "POLICY_UNAVAILABLE")
(ma-case "incomplete lineage denied"
         (lambda (r c s) (declare (ignore r c))
           (setf (gethash "lineage_complete" (aref (gethash "protections" s) 0)) :false)) "ACCESS_INCOMPLETE")
(ma-case "cyclic lineage denied"
         (lambda (r c s) (declare (ignore c))
           (setf (gethash "dependency_refs" (aref (gethash "protections" s) 0)) (vector r))) "ACCESS_INCOMPLETE")
(ma-case "health denial survives additional food category"
         (lambda (r c s) (declare (ignore r c))
           (let ((deny (%ma-detach (aref (gethash "rules" s) 0))))
             (setf (gethash "rule_id" deny) "deny:health" (gethash "effect" deny) "deny"
                   (gethash "categories" (aref (gethash "resource_selectors" deny) 0)) #("health")
                   (gethash "rules" s) (vector (aref (gethash "rules" s) 0) deny)))) "EXPLICIT_DENY")
(ma-case "allow must cover every effective category"
         (lambda (r c s) (declare (ignore r c))
           (setf (gethash "categories" (aref (gethash "resource_selectors" (aref (gethash "rules" s) 0)) 0))
                 #("food-preference"))) "ALLOW_MISSING")
(ma-case "read permission does not permit evidence hydration"
         (lambda (r c s) (declare (ignore c s)) (setf (gethash "component" r) "evidence")) "ALLOW_MISSING")
(ma-case "all governing policies require an allow"
         (lambda (r c s) (declare (ignore r c))
           (setf (gethash "policy_refs" (aref (gethash "protections" s) 0))
                 (vector (%ma-object "policy_id" "policy:owner" "policy_version" "1")
                         (%ma-object "policy_id" "policy:second-controller" "policy_version" "1")))) "ALLOW_MISSING")
(ma-case "ancestor policy restrictions survive semantic derivation"
         (lambda (r c s) (declare (ignore r c))
           (let* ((parent (%ma-detach (aref (gethash "protections" s) 0)))
                  (parent-ref (gethash "resource_ref" parent)))
             (setf (gethash "store" parent-ref) "event"
                   (gethash "policy_refs" parent) (vector (%ma-object "policy_id" "policy:ancestor" "policy_version" "1"))
                   (gethash "dependency_refs" (aref (gethash "protections" s) 0)) (vector parent-ref)
                   (gethash "protections" s) (vector (aref (gethash "protections" s) 0) parent)))) "ALLOW_MISSING")
(ma-case "external recipient stays disabled before grant implementation"
         (lambda (r c s) (declare (ignore r s))
           (setf (gethash "recipient_principal_id" c) "owner" (gethash "action" c) "disclose")) "GRANT_INVALID")
(ma-case "grant-dependent read has no permissive fallback"
         (lambda (r c s) (declare (ignore r c))
           (setf (gethash "require_task_grant" (aref (gethash "rules" s) 0)) :true)) "ALLOW_MISSING")
(ma-case "task-scoped calls require task validation"
         (lambda (r c s) (declare (ignore r s)) (setf (gethash "task_id" c) "task:unverified")) "GRANT_INVALID")
(ma-case "all stores use the same evaluator"
         (lambda (r c s) (declare (ignore c s)) (setf (gethash "store" r) "graph" (gethash "component" r) "claim"))
         "POLICY_SATISFIED")
(multiple-value-bind (r c s) (ma-fixture)
  (setf (gethash "lineage_complete" (aref (gethash "protections" s) 0)) nil)
  (ma-check "false and null are never Lisp NIL in the new protocol"
            (handler-case (progn (memory-access-decide r c s) nil) (memory-access-input-error () t))))
(ma-check "canonical JSON ignores object insertion order"
          (equal (memory-access-canonical-json (%ma-object "b" :false "a" :null))
                 (memory-access-canonical-json (%ma-object "a" :null "b" :false))))
(ma-check "booleans and null serialize distinctly"
          (equal "[true,false,null]" (memory-access-canonical-json #(:true :false :null))))
(multiple-value-bind (ref context snapshot) (ma-fixture)
  (let ((output (%ma-object "store" "graph" "resource_id" "claim:derived" "version_id" "version:one" "component" "claim")))
    (ma-check "read permission cannot stand in for derive permission"
              (equal "deny" (gethash "outcome" (memory-access-derive-protection output (vector ref) context snapshot))))
    (setf (gethash "action" context) "derive")
    (let* ((before (memory-access-canonical-json (vector ref context snapshot)))
           (result (memory-access-derive-protection output (vector ref) context snapshot)) (protection (gethash "value" result)))
      (ma-check "ordinary derivation retains mixed-content categories" (equalp #("food-preference" "health") (gethash "categories" protection)))
      (ma-check "ordinary derivation retains highest sensitivity" (equal "sensitive" (gethash "sensitivity" protection)))
      (ma-check "ordinary derivation retains governing policies" (= 1 (length (gethash "policy_refs" protection))))
      (ma-check "ordinary derivation keeps traceable dependencies" (%ma-equal (vector ref) (gethash "dependency_refs" protection)))
      (setf (gethash "sensitivity" protection) "public")
      (ma-check "derived protections are detached and source unchanged" (equal before (memory-access-canonical-json (vector ref context snapshot))))
      (ma-check "derivation cannot overwrite its own source component"
                (equal "deny" (gethash "outcome" (memory-access-derive-protection ref (vector ref) context snapshot)))))
    (setf (gethash "actions" (aref (gethash "rules" snapshot) 0)) #("read"))
    (ma-check "policy can allow reads while denying ordinary derivation"
              (equal "deny" (gethash "outcome" (memory-access-derive-protection output (vector ref) context snapshot))))))
(format t "MEMORY-ACCESS-POLICY ~d passed, 0 failed~%" *ma-checks*)
