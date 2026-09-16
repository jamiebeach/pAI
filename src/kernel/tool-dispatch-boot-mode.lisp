;;;; tool-dispatch-boot-mode.lisp -- immutable process boot choice.

(in-package :agent)

(export '(tool-dispatch-boot-mode tool-dispatch-kernel-boot-p
          tool-dispatch-legacy-wrapper-enabled-p
          tool-dispatch-terminal-execute-identity
          tool-dispatch-boot-mode-report))

(defun %tool-dispatch-parse-boot-mode (raw)
  (cond ((or (null raw) (string= raw "") (string-equal raw "legacy"))
         :legacy)
        ((string-equal raw "kernel") :kernel)
        (t (error "Unknown PAI_TOOL_DISPATCH_BOOT_MODE ~s; expected legacy or kernel."
                  raw))))

(defparameter *tool-dispatch-boot-mode*
  (%tool-dispatch-parse-boot-mode (uiop:getenv "PAI_TOOL_DISPATCH_BOOT_MODE")))
(defparameter *tool-dispatch-terminal-execute-identity*
  (and (fboundp 'execute) (fdefinition 'execute)))

(unless *tool-dispatch-terminal-execute-identity*
  (error "Tool-dispatch boot mode loaded before terminal EXECUTE exists."))

(defun tool-dispatch-boot-mode () *tool-dispatch-boot-mode*)
(defun tool-dispatch-kernel-boot-p () (eq *tool-dispatch-boot-mode* :kernel))
(defun tool-dispatch-legacy-wrapper-enabled-p ()
  (eq *tool-dispatch-boot-mode* :legacy))
(defun tool-dispatch-terminal-execute-identity ()
  *tool-dispatch-terminal-execute-identity*)

(defun tool-dispatch-boot-mode-report ()
  (obj "schema_version" 1
       "mode" (string-downcase (symbol-name *tool-dispatch-boot-mode*))
       "kernel" (if (tool-dispatch-kernel-boot-p) t nil)
       "terminal_execute_intact"
       (if (and (fboundp 'execute)
                (eq (fdefinition 'execute)
                    *tool-dispatch-terminal-execute-identity*)) t nil)
       "mutable" nil))
