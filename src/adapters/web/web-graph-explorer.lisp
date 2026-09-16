;;;; web-graph-explorer.lisp -- independent read-only operator graph surface.

(in-package :agent)

(export '(web-graph-explorer-configure-search))

(defvar *web-graph-explorer-search-fn* nil
  "Injected selected-mind graph port. Receives one closed KG3 request.")

(defun web-graph-explorer-configure-search (function)
  (unless (functionp function)
    (error "Web graph explorer requires a search function"))
  (setf *web-graph-explorer-search-fn* function)
  t)

(defparameter *web-graph-explorer-html* (asset "assets/graph-explorer.html"))
(defparameter *web-graph-explorer-js* (asset "assets/graph-explorer.js"))
(defparameter *web-app-shell-js* (asset "assets/app-shell.js"))

(hunchentoot:define-easy-handler (web-graph-explorer-page :uri "/graph") ()
  (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
  *web-graph-explorer-html*)

(hunchentoot:define-easy-handler (web-graph-explorer-script
                                  :uri "/graph-explorer.js") ()
  (setf (hunchentoot:content-type*) "text/javascript; charset=utf-8")
  *web-graph-explorer-js*)

(hunchentoot:define-easy-handler (web-app-shell-script :uri "/app-shell.js") ()
  (setf (hunchentoot:content-type*) "text/javascript; charset=utf-8")
  *web-app-shell-js*)

(hunchentoot:define-easy-handler (web-graph-explorer-search
                                  :uri "/api/v2/graph/search") ()
  (setf (hunchentoot:content-type*) "application/json; charset=utf-8")
  (handler-case
      (progn
        (unless (eq (hunchentoot:request-method*) :post)
          (error "Graph explorer search requires POST"))
        (unless (functionp *web-graph-explorer-search-fn*)
          (error "Graph explorer search service is unavailable"))
        (let* ((body (hunchentoot:raw-post-data :force-text t))
               (arguments (shasht:read-json body))
               (request (knowledge-graph-search-tool-normalize arguments))
               (rich-result (funcall *web-graph-explorer-search-fn* request)))
          (%v2-json (knowledge-graph-search-compact-result rich-result))))
    (error (condition)
      (setf (hunchentoot:return-code*) 400)
      (%v2-json
       (obj "error" (%v2-condition-summary condition)
            "status" "failed")))))
