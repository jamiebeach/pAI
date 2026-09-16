;;;; web-terminal-ambient-activity-tests.lisp -- presentation separation.
;;;; harness: full-system

(in-package :agent)

(defvar *web-ambient-passed* 0)
(defvar *web-ambient-failed* 0)

(defun web-ambient-check (name condition)
  (if condition
      (progn (incf *web-ambient-passed*) (format t "PASS ~a~%" name))
      (progn (incf *web-ambient-failed*) (format t "FAIL ~a~%" name))))

(defun web-ambient-count-substring (needle text)
  (loop with start = 0
        for position = (search needle text :start2 start)
        while position
        count t
        do (setf start (+ position (length needle)))))

(format t "~%== web terminal ambient activity ==~%")

(let ((*v2-ring* nil)
      (*v2-next-id* 0)
      (*v2-clients* nil))
  (let ((mailbox (make-array 0 :adjustable t :fill-pointer 0)))
    (push mailbox *v2-clients*)
    (let ((ambient (web-terminal-present-activity
                    "private model step" :private-p t
                    :turn-id "private:1")))
      (web-ambient-check "private activity has an explicit ambient type"
                         (string= "ambient" (gethash "type" ambient)))
      (web-ambient-check "private activity reaches connected clients"
                         (and (= 1 (length mailbox))
                              (eq ambient (aref mailbox 0))))
      (web-ambient-check "private activity is absent from replay history"
                         (null *v2-ring*)))
    (let ((operator (web-terminal-present-activity
                     "operator tool step" :private-p nil
                     :turn-id "operator:1")))
      (web-ambient-check "operator activity retains the tool presentation"
                         (string= "tool" (gethash "type" operator)))
      (web-ambient-check "operator activity remains replayable"
                         (and (= 1 (length *v2-ring*))
                              (eq operator (first *v2-ring*)))))
    (let ((progress
            (web-terminal-present-stream-progress
             (obj "generation_id" "generation:1"
                  "display" "pAI is reasoning · ~ 42 tokens received"
                  "private" nil "estimated_output_tokens" 42)
             :turn-id "operator:1")))
      (web-ambient-check "stream progress reaches connected clients"
                         (and (= 3 (length mailbox))
                              (eq progress (aref mailbox 2))
                              (string= "stream-progress"
                                       (gethash "type" progress))))
      (web-ambient-check "stream progress remains transient"
                         (not (member progress *v2-ring* :test #'eq))))
    (let ((notice (web-terminal-present-operational-notice
                   "bounded accounting fallback; cognition continued"
                   :turn-id "private:1")))
      (web-ambient-check "operational anomalies use a distinct retained type"
                         (string= "operational" (gethash "type" notice)))
      (web-ambient-check "operational anomalies remain replayable"
                         (eq notice (first *v2-ring*)))
      (let ((repeated
              (web-terminal-present-operational-notice
               "bounded accounting fallback; cognition continued"
               :turn-id "private:1")))
        (web-ambient-check "consecutive identical anomalies reuse one event"
                           (and (eq notice repeated)
                                (= 2 (length *v2-ring*))))))))

(let ((*v2-ring*
        (append
         (loop for id downfrom 500 to 301
               collect (obj "id" id "type" "operational"
                            "data" "repeated failure"))
         (loop for id downfrom 300 to 1
               collect (obj "id" id "type" "final"
                            "data" (format nil "reply ~d" id))))))
  (multiple-value-bind (page more-p) (%v2-ring-page nil 200)
    (web-ambient-check "operational flood cannot displace newest chat page"
                       (and more-p
                            (= 201 (length page))
                            (= 200 (count "final" page
                                          :key (lambda (event)
                                                 (gethash "type" event ""))
                                          :test #'string=))
                            (= 1 (count "operational" page
                                        :key (lambda (event)
                                               (gethash "type" event ""))
                                        :test #'string=))))
    (let ((oldest-chat-id
            (gethash "id"
                     (find-if (lambda (event)
                                (string= "final"
                                         (gethash "type" event "")))
                              page))))
      (multiple-value-bind (older older-more-p)
          (%v2-ring-page oldest-chat-id 200)
        (web-ambient-check "older-page cursor remains anchored in chat"
                           (and (not older-more-p)
                                (= 100 (length older))
                                (every (lambda (event)
                                         (string= "final"
                                                  (gethash "type" event "")))
                                       older)))))))

(let ((html (uiop:read-file-string
             (merge-pathnames "src/adapters/web/assets/terminal.html"
                              *pai-root*))))
  (web-ambient-check "frontend declares a separate activity shelf"
                     (and (search "id=\"activity-shelf\"" html)
                          (search "function renderAmbient" html)))
  (web-ambient-check "ambient events bypass transcript rendering"
                     (search "ev.type === 'ambient'" html))
  (web-ambient-check "ambient entries have a bounded visible lifetime"
                     (and (search "AMBIENT_VISIBLE_MS" html)
                          (search "MAX_AMBIENT" html)))
  (web-ambient-check "operational anomalies occupy a persistent shelf"
                     (and (search "id=\"operational-shelf\"" html)
                          (search "function renderOperational" html)
                          (search "ev.type === 'operational'" html)))
  (web-ambient-check "frontend replaces streamed token progress by generation"
                     (and (search "function renderStreamProgress" html)
                          (search "data-stream-key" html)
                          (search "ev.type === 'stream-progress'" html))))

(format t "~%== web terminal workspace, mobile and PWA shell ==~%")

(let ((*recursive-primitive-workspace-root* (truename *pai-root*)))
  (web-ambient-check "Files root follows the configured recursive workspace"
                     (equal (truename *pai-root*) (%v2-workspace-root)))
  (web-ambient-check "Files paths are contained by the recursive workspace"
                     (equal (truename (merge-pathnames "README.md" *pai-root*))
                            (v2-file-safe-path "README.md")))
  (web-ambient-check "Files traversal remains rejected by the shared guard"
                     (handler-case
                         (progn (v2-file-safe-path "../README.md") nil)
                       (error () t))))

(let* ((assets (merge-pathnames "src/adapters/web/assets/" *pai-root*))
       (html (uiop:read-file-string (merge-pathnames "terminal.html" assets)))
       (manifest
         (shasht:read-json
          (uiop:read-file-string
           (merge-pathnames "manifest.webmanifest" assets))))
       (worker
         (uiop:read-file-string (merge-pathnames "service-worker.js" assets)))
       (login
         (uiop:read-file-string (merge-pathnames "login.html" assets)))
       (shell
         (uiop:read-file-string (merge-pathnames "app-shell.js" assets)))
       (graph
         (uiop:read-file-string (merge-pathnames "graph-explorer.html" assets)))
       (graph-js
         (uiop:read-file-string (merge-pathnames "graph-explorer.js" assets))))
  (web-ambient-check "mobile shell uses the visual viewport and safe areas"
                     (and (search "viewport-fit=cover" html)
                          (search "src=\"/viewport.js\"" html)
                          (search "safe-area-inset-bottom" html)
                          (search "min-height: 0" html)))
  (web-ambient-check "standalone PWA shell has no inferred device-screen reserve"
                     (and (search "#app { position: fixed; inset: 0" html)
                          (null (search "screen-reserve" html))
                          (null (search "screen.height" html))))
  (web-ambient-check "file browser labels the server-selected workspace root"
                     (and (search "data.root || 'workspace'" html)
                          (null (search "/agent/state/" html))))
  (web-ambient-check "terminal advertises an installable PWA shell"
                     (and (search "rel=\"manifest\"" html)
                          (search "navigator.serviceWorker.register" html)
                          (string= "/terminal" (gethash "start_url" manifest))
                          (string= "standalone" (gethash "display" manifest))))
  (web-ambient-check "terminal and graph use one decoupled navigation shell"
                     (and (search "src=\"/app-shell.js\"" html)
                          (search "src=\"/app-shell.js\"" graph)
                          (search "Graph Explorer" shell)
                          (search "pai-shell-ready" shell)))
  (web-ambient-check "graph explorer uses bounded same-origin graph search"
                     (and (search "/api/v2/graph/search" graph-js)
                          (search "maximum_paths:12" graph-js)
                          (search "X-PAI-Request':'same-origin'" graph-js)
                          (search "node.node_id" graph-js)
                          (search "edge.from_node_id" graph-js)))
  (web-ambient-check "graph explorer distinguishes exact identities from suggestions"
                     (and (search "query_match_kind === 'exact-identity'" graph-js)
                          (search "No exact identity" graph-js)
                          (search "related suggestions" graph-js)))
  (web-ambient-check "service worker never caches authenticated content"
                     (and (search "intentionally caches no" worker)
                          (null (search "caches.open" worker))
                          (null (search "addEventListener('fetch'" worker))))
  (web-ambient-check "PWA login uses a protected cookie exchange without browser storage"
                     (and (search "X-PAI-Request':'same-origin'" login)
                          (search "credentials: 'same-origin'" login)
                          (search "form.password.value = ''" login)
                          (null (search "localStorage" login))
                          (null (search "sessionStorage" login))))
  (web-ambient-check "PWA login fits dynamic mobile viewport and safe areas"
                     (and (search "viewport-fit=cover" login)
                          (search "body { position:fixed; inset:0" login)
                          (search "safe-area-inset-top" login)
                          (search "safe-area-inset-bottom" login)))
  (web-ambient-check "PWA login shares viewport policy and standalone metadata"
                     (and (search "src=\"/viewport.js\"" login)
                          (search "black-translucent" login)
                          (null (search "screen-reserve" login))
                          (%web-pwa-public-path-p "/viewport.js"))))

(web-ambient-check "graph navigation redirects unauthenticated PWA requests to login"
                   (%web-terminal-navigation-path-p "/graph"))

(let ((adapter
        (uiop:read-file-string
         (merge-pathnames "src/adapters/web/web-graph-explorer.lisp"
                          *pai-root*)))
      (launcher
        (uiop:read-file-string
         (merge-pathnames "scripts/conscious-conversation.lisp"
                          *pai-root*))))
  (web-ambient-check "graph endpoint delegates one closed read-only search composition"
                     (and (search "knowledge-graph-search-tool-normalize" adapter)
                          (search "knowledge-graph-search-compact-result" adapter)
                          (search "*web-graph-explorer-search-fn*" adapter)
                          (search "#'%conversation-knowledge-graph-search" launcher)
                          (null (search "log-event" adapter))
                          (null (search "sqlite" adapter)))))

(let ((source
        (uiop:read-file-string
         (merge-pathnames "src/adapters/web/web-terminal.lisp" *pai-root*))))
  (web-ambient-check "file and generated-image containment remain separate"
                     (and (search "(v2-file-safe-path" source)
                          (search "(v2-state-safe-path" source)
                          (search "(v2-state-safe-path (or path \"\"))" source)))
  (web-ambient-check "terminal documents are never served with stale renderers"
                     (and (search "(v2-terminal :uri \"/terminal\")" source)
                          (search "(v2-root :uri \"/\")" source)
                          (>= (web-ambient-count-substring
                               "(hunchentoot:header-out \"Cache-Control\") \"no-store\""
                               source)
                              2))))

(let ((launcher (uiop:read-file-string
                 (merge-pathnames "scripts/conscious-conversation.lisp"
                                  *pai-root*))))
  (web-ambient-check "runtime routing uses the explicit private channel"
                     (and (search "web-terminal-present-activity" launcher)
                          (search "(string= \"private\" (gethash \"channel\" item \"\"))"
                                  launcher)))
  (web-ambient-check "runtime routes accounting anomalies to retained notices"
                     (and (search "operational-anomaly" launcher)
                          (search "web-terminal-present-operational-notice"
                                  launcher)))
  (web-ambient-check "stream progress builds its web payload across the package boundary"
                     (and (search "(%conversation-object"
                                  launcher
                                  :start2
                                  (or (search
                                       "web-terminal-present-stream-progress"
                                       launcher)
                                      0))
                          (null (search "(obj \"generation_id\" generation"
                                        launcher)))))

(let* ((condition
         (make-condition 'simple-error
                         :format-control "first line~%second line ~a"
                         :format-arguments
                         (list (make-string 1200 :initial-element #\x))))
       (summary (%v2-condition-summary condition))
       (source
         (uiop:read-file-string
          (merge-pathnames "src/adapters/web/web-terminal.lisp"
                           *pai-root*))))
  (web-ambient-check "web failure diagnostic is single-line and bounded"
                     (and (<= (length summary) 1024)
                          (not (find #\Newline summary))
                          (not (find #\Return summary))))
  (web-ambient-check "web submit failure is broadcast instead of discarded"
                     (and (search
                           "(%v2-broadcast \"error\" failure)" source)
                          (search "(%v2-condition-summary e)" source)
                          (search "web-interaction-submit-failed" source))))

(format t "~%web terminal ambient activity: ~d passed, ~d failed~%"
        *web-ambient-passed* *web-ambient-failed*)
(when (plusp *web-ambient-failed*) (uiop:quit 1))
