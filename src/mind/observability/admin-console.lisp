;;;; admin-console.lisp -- authenticated context/prompt controls and private view.
;;;;
;;;; This surface is deliberately separate from the unauthenticated operator
;;;; dashboard. It controls conversation-context budget numbers plus versioned
;;;; identity/voice fragments, and reads one credential-redacted in-memory
;;;; public-model request. It has no route to conversation records, autonomy
;;;; modes, tools, gateway, initiative, transport, or delivery.

(in-package :agent)

(export '(admin-console-configured-p))

(defvar *admin-console-token-file*
  (pathname (or (uiop:getenv "PAI_ADMIN_TOKEN_FILE")
                (let ((root (uiop:getenv "PAI_SECRET_ROOT")))
                  (and root (plusp (length root))
                       (namestring (merge-pathnames "admin-token.txt"
                                                    (pathname root)))))
                "/agent/state/admin-token.txt")))
(defun %admin-load-token ()
  (let ((environment (uiop:getenv "PAI_ADMIN_TOKEN")))
    (cond ((and (stringp environment) (plusp (length environment))) environment)
          ((probe-file *admin-console-token-file*)
           (string-trim '(#\Space #\Tab #\Newline #\Return)
                        (uiop:read-file-string *admin-console-token-file*)))
          (t nil))))
(defvar *admin-console-token* (%admin-load-token))
(defparameter *admin-console-min-token-chars* 32)
(defparameter *admin-console-max-request-chars* 65536)

(defun admin-console-configured-p ()
  (and (stringp *admin-console-token*)
       (>= (length *admin-console-token*) *admin-console-min-token-chars*)))

(defun %admin-bearer-value (header)
  (let ((prefix "Bearer "))
    (and (stringp header)
         (> (length header) (length prefix))
         (string= prefix header :end2 (length prefix))
         (subseq header (length prefix)))))

(defun %admin-token-equal-p (left right)
  (when (and (stringp left) (stringp right))
    (let ((difference (logxor (length left) (length right))))
      (loop for index below (min (length left) (length right))
            do (setf difference
                     (logior difference
                             (logxor (char-code (char left index))
                                     (char-code (char right index))))))
      (zerop difference))))

(defun %admin-authorized-p (&optional authorization)
  (and (admin-console-configured-p)
       (%admin-token-equal-p
        (%admin-bearer-value
         (or authorization (hunchentoot:header-in* "Authorization")))
        *admin-console-token*)))

(defun %admin-json (value)
  (setf (hunchentoot:content-type*) "application/json; charset=utf-8")
  (setf (hunchentoot:header-out "Cache-Control") "no-store")
  (shasht:write-json value nil))

(defun %admin-error (status message)
  (setf (hunchentoot:return-code*) status)
  (%admin-json (obj "error" message)))

(defun %admin-require-authorization ()
  (cond ((not (admin-console-configured-p))
         (%admin-error 503 "Admin API is disabled until an admin token is configured."))
        ((not (%admin-authorized-p))
         (setf (hunchentoot:header-out "WWW-Authenticate") "Bearer realm=pai-admin")
         (%admin-error 401 "Unauthorized."))
        (t nil)))

(defun %admin-context-config-response ()
  (if (fboundp 'conversation-context-budget-config-report)
      (conversation-context-budget-config-report)
      (error "Conversation context configuration is unavailable.")))

(defun %admin-update-context-config (body)
  (unless (and (stringp body)
               (plusp (length body))
               (<= (length body) *admin-console-max-request-chars*))
    (error "Request body is empty or too large."))
  (let ((data (shasht:read-json body)))
    (unless (hash-table-p data) (error "Expected one JSON object."))
    (conversation-context-budget-update data :actor "authenticated-admin-api")))

(defun %admin-prompt-config-response ()
  (if (fboundp 'public-system-prompt-report)
      (public-system-prompt-report)
      (error "Public system-prompt renderer is unavailable.")))

(defun %admin-update-prompt-config (body)
  (unless (and (stringp body)
               (plusp (length body))
               (<= (length body) *admin-console-max-request-chars*))
    (error "Request body is empty or too large."))
  (let ((data (shasht:read-json body)))
    (public-system-prompt-update-from-object
     data :actor "authenticated-admin-api")))

(defparameter *admin-console-html*
"<!doctype html><html><head><meta charset='utf-8'><meta name='viewport' content='width=device-width,initial-scale=1'><title>the agent Admin</title><style>
body{margin:0;background:#10121a;color:#e8eaf0;font:14px system-ui;padding:24px}main{max-width:1100px;margin:auto}h1,h2{margin:.4em 0}.panel{background:#181c28;border:1px solid #30384d;border-radius:9px;padding:16px;margin:14px 0}.row,.grid{display:flex;gap:10px;flex-wrap:wrap;align-items:end}.grid label{display:grid;gap:5px;min-width:190px;flex:1}input,button,textarea{background:#202535;color:#e8eaf0;border:1px solid #3a425b;border-radius:6px;padding:9px}textarea{width:100%;box-sizing:border-box;min-height:180px;resize:vertical;font:13px ui-monospace,monospace}button{cursor:pointer}.muted{color:#9ba5bf}.error{color:#ff9f9f}.ok{color:#9fe3b1}pre{max-height:620px;overflow:auto;white-space:pre-wrap;word-break:break-word;background:#0c0e14;padding:12px;border-radius:6px}</style></head><body><main><h1>the agent <span class='muted'>private admin</span></h1><div class='panel'><div class='row'><label>Admin token <input id='token' type='password' autocomplete='off' size='52'></label><button id='connect'>Connect</button></div><p class='muted'>The token stays only in this page's memory. APIs fail closed when no server token is configured.</p><div id='status' class='muted'>Not connected.</div></div><div class='panel'><h2>Conversation context parameters</h2><div id='fields' class='grid'></div><div class='row' style='margin-top:12px'><button id='reload'>Reload context controls</button><button id='save'>Validate and apply live</button></div><p class='muted'>Changes are atomic and persist across restarts. They affect future prompt assembly only; conversation records are not rewritten.</p></div><div class='panel'><h2>Public identity and voice</h2><p id='promptMeta' class='muted'>Not loaded.</p><label>Who I am<textarea id='promptIdentity'></textarea></label><label>Voice and demeanor<textarea id='promptVoice'></textarea></label><div class='row' style='margin-top:12px'><button id='promptReload'>Reload</button><button id='promptSave'>Validate and apply next turn</button><button id='promptRollback'>Rollback one revision</button><button id='promptReset'>Reset to Markdown defaults</button></div><p class='muted'>Operational rules and tools are not editable here. Updates are atomic, versioned, and affect the next inference without rewriting conversation records.</p><details><summary>Next-turn stable prompt preview</summary><pre id='promptPreview'>No preview loaded.</pre></details></div><div class='panel'><h2>Current public inference context</h2><div class='row'><button id='context'>Refresh raw JSON</button><button id='download'>Download JSON</button></div><p class='muted'>Latest public model request since process start. Loaded only on demand. Credential-shaped values are redacted.</p><pre id='raw'>No context loaded.</pre></div></main><script>
let adminToken='',lastRaw=null;const $=id=>document.getElementById(id);const names=['target_records','minimum_recent_records','hard_records','target_estimated_tokens','hard_estimated_tokens','target_chars','hard_chars','brief_chars','tool_result_chars'];
$('fields').innerHTML=names.map(n=>`<label>${n.replaceAll('_',' ')}<input id='f_${n}' type='number' step='1'></label>`).join('');
function status(text,error=false){$('status').className=error?'error':'ok';$('status').textContent=text}
async function api(path,options={}){let headers={...(options.headers||{}),Authorization:`Bearer ${adminToken}`};let response=await fetch(path,{...options,headers,cache:'no-store'});let text=await response.text(),data;try{data=JSON.parse(text)}catch(_){throw new Error(`HTTP ${response.status}: invalid JSON`)}if(!response.ok)throw new Error(data.error||`HTTP ${response.status}`);return data}
async function loadConfig(){try{let data=await api('/api/admin/context/config');names.forEach(n=>$(`f_${n}`).value=data[n]);status('Authenticated. Configuration loaded.')}catch(error){status(error.message,true)}}
function showPrompt(data){$('promptIdentity').value=data.identity;$('promptVoice').value=data.voice;$('promptMeta').textContent=`Revision ${data.revision} · ${data.source} · ${data.rendered_stable_chars} chars · ${data.rendered_stable_sha256}`;$('promptPreview').textContent=data.rendered_stable_prompt}
async function loadPrompt(){try{showPrompt(await api('/api/admin/prompt/config'));status('Authenticated. Prompt fragments loaded.')}catch(error){status(error.message,true)}}
function configBody(){return Object.fromEntries(names.map(n=>[n,Number($(`f_${n}`).value)]))}
$('connect').onclick=()=>{adminToken=$('token').value;$('token').value='';loadConfig();loadPrompt()};$('reload').onclick=loadConfig;
$('save').onclick=async()=>{try{let data=await api('/api/admin/context/config',{method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify(configBody())});names.forEach(n=>$(`f_${n}`).value=data[n]);status('Configuration validated, persisted, and applied live.')}catch(error){status(error.message,true)}};
$('promptReload').onclick=loadPrompt;
$('promptSave').onclick=async()=>{try{showPrompt(await api('/api/admin/prompt/config',{method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify({identity:$('promptIdentity').value,voice:$('promptVoice').value})}));status('Identity and voice validated, versioned, and active for the next inference.')}catch(error){status(error.message,true)}};
$('promptRollback').onclick=async()=>{try{showPrompt(await api('/api/admin/prompt/rollback',{method:'POST'}));status('Previous identity and voice restored as a new revision.')}catch(error){status(error.message,true)}};
$('promptReset').onclick=async()=>{try{showPrompt(await api('/api/admin/prompt/reset',{method:'POST'}));status('Markdown defaults restored as a new revision.')}catch(error){status(error.message,true)}};
$('context').onclick=async()=>{try{lastRaw=await api('/api/admin/context/current');$('raw').textContent=JSON.stringify(lastRaw,null,2);status('Current context loaded.')}catch(error){status(error.message,true)}};
$('download').onclick=()=>{if(!lastRaw)return;let blob=new Blob([JSON.stringify(lastRaw,null,2)],{type:'application/json'}),url=URL.createObjectURL(blob),a=document.createElement('a');a.href=url;a.download='pai-current-context.json';a.click();URL.revokeObjectURL(url)};
</script></body></html>")

(hunchentoot:define-easy-handler (admin-console-page :uri "/admin") ()
  (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
  (setf (hunchentoot:header-out "Cache-Control") "no-store")
  (setf (hunchentoot:header-out "X-Content-Type-Options") "nosniff")
  (setf (hunchentoot:header-out "Referrer-Policy") "no-referrer")
  (setf (hunchentoot:header-out "Content-Security-Policy")
        "default-src 'self'; script-src 'unsafe-inline'; style-src 'unsafe-inline'; connect-src 'self'; object-src 'none'; base-uri 'none'; frame-ancestors 'none'")
  *admin-console-html*)

(hunchentoot:define-easy-handler
    (admin-context-config :uri "/api/admin/context/config") ()
  (or (%admin-require-authorization)
      (handler-case
          (case (hunchentoot:request-method*)
            (:get (%admin-json (%admin-context-config-response)))
            (:post (%admin-json
                    (%admin-update-context-config
                     (hunchentoot:raw-post-data :force-text t))))
            (otherwise (%admin-error 405 "Method not allowed.")))
        (error (condition)
          (%admin-error 400 (princ-to-string condition))))))

(hunchentoot:define-easy-handler
    (admin-current-context :uri "/api/admin/context/current") ()
  (or (%admin-require-authorization)
      (if (not (eq (hunchentoot:request-method*) :get))
          (%admin-error 405 "Method not allowed.")
          (%admin-json
           (if (fboundp 'llm-debug-current-public-context)
               (llm-debug-current-public-context)
               (obj "schema_version" 1 "status" "unavailable"
                    "reason" "context-capture-not-loaded"))))))

(hunchentoot:define-easy-handler
    (admin-current-curator :uri "/api/admin/curator/current") ()
  (or (%admin-require-authorization)
      (if (not (eq (hunchentoot:request-method*) :get))
          (%admin-error 405 "Method not allowed.")
          (%admin-json
           (obj "schema_version" 1
                "report" (if (fboundp 'context-curator-report)
                               (context-curator-report)
                               (obj "status" "unavailable"))
                "current" (if (fboundp 'context-curator-current-private-result)
                                (context-curator-current-private-result)
                                (obj "schema_version" 1
                                     "status" "unavailable"))
                "last_selected"
                (if (fboundp 'context-curator-last-selected-private-result)
                    (context-curator-last-selected-private-result)
                    (obj "schema_version" 1 "status" "unavailable")))))))

(hunchentoot:define-easy-handler
    (admin-memory-atoms-report :uri "/api/admin/memory-atoms/report") ()
  (or (%admin-require-authorization)
      (if (not (eq (hunchentoot:request-method*) :get))
          (%admin-error 405 "Method not allowed.")
          (%admin-json
           (if (fboundp 'memory-atom-shadow-report)
               (memory-atom-shadow-report)
               (obj "schema_version" 1 "status" "unavailable"))))))

(hunchentoot:define-easy-handler
    (admin-memory-atoms-current :uri "/api/admin/memory-atoms/current") ()
  (or (%admin-require-authorization)
      (if (not (eq (hunchentoot:request-method*) :get))
          (%admin-error 405 "Method not allowed.")
          (%admin-json
           (if (fboundp 'memory-atom-shadow-current-private-review)
               (memory-atom-shadow-current-private-review)
               (obj "schema_version" 1 "status" "unavailable"))))))

(hunchentoot:define-easy-handler
    (admin-prompt-config :uri "/api/admin/prompt/config") ()
  (or (%admin-require-authorization)
      (handler-case
          (case (hunchentoot:request-method*)
            (:get (%admin-json (%admin-prompt-config-response)))
            (:post (%admin-json
                    (%admin-update-prompt-config
                     (hunchentoot:raw-post-data :force-text t))))
            (otherwise (%admin-error 405 "Method not allowed.")))
        (error (condition)
          (%admin-error 400 (princ-to-string condition))))))

(hunchentoot:define-easy-handler
    (admin-prompt-rollback :uri "/api/admin/prompt/rollback") ()
  (or (%admin-require-authorization)
      (if (not (eq (hunchentoot:request-method*) :post))
          (%admin-error 405 "Method not allowed.")
          (handler-case
              (%admin-json
               (public-system-prompt-rollback
                :actor "authenticated-admin-api"))
            (error (condition)
              (%admin-error 400 (princ-to-string condition)))))))

(hunchentoot:define-easy-handler
    (admin-prompt-reset :uri "/api/admin/prompt/reset") ()
  (or (%admin-require-authorization)
      (if (not (eq (hunchentoot:request-method*) :post))
          (%admin-error 405 "Method not allowed.")
          (handler-case
              (%admin-json
               (public-system-prompt-reset-defaults
                :actor "authenticated-admin-api"))
            (error (condition)
              (%admin-error 400 (princ-to-string condition)))))))
