;;;; PAI ENHANCEMENTS - Memory System & Web Search
;;;; Save this file and load it after the base agent.lisp
;;;; Created: 2026-07-10 00:14:05

(in-package :agent)

(export '(pai-enhancements-tool-handle))

;;; === WEB SEARCH (Brave API) ===

(defparameter *brave-api-key* (load-brave-api-key))

(ql:quickload :quri :silent t)

(DEFUN WEB-SEARCH (QUERY)
  "Search the web using Brave Search API and return formatted results."
  (HANDLER-CASE
   (LET* ((URL
           (FORMAT NIL "https://api.search.brave.com/res/v1/web/search?q=~a"
                   (QURI.ENCODE:URL-ENCODE QUERY)))
          (RESPONSE
           (DEXADOR:GET URL :HEADERS
                        `(("Accept" . "application/json")
                          ("X-Subscription-Token"
                           . ,(or (brave-api-key)
                                 (error "Brave API credential is not configured"))))))
          (DATA (SHASHT:READ-JSON RESPONSE))
          (RESULTS (GETHASH "results" (GETHASH "web" DATA))))
     (IF (AND RESULTS (PLUSP (LENGTH RESULTS)))
         (WITH-OUTPUT-TO-STRING (S)
           (FORMAT S "Found ~d results for '~a':~%~%" (LENGTH RESULTS) QUERY)
           (LOOP FOR I FROM 0 BELOW (MIN 5 (LENGTH RESULTS))
                 FOR RESULT = (AREF RESULTS I)
                 DO (FORMAT S "~d. ~a~%   ~a~%   URL: ~a~%~%" (1+ I)
                            (GETHASH "title" RESULT)
                            (GETHASH "description" RESULT)
                            (GETHASH "url" RESULT))))
         (FORMAT NIL "No results found for '~a'" QUERY)))
   (ERROR (E) (FORMAT NIL "ERROR: Web search failed: ~a" E))))


;;; === ADD web-search TO *TOOLS* / EXECUTE (compose, don't clobber) ===
;;; This used to SETF *tools* to a fresh vector and redefine EXECUTE outright
;;; — which silently deleted propose-loop (added by self-mod.lisp) and its
;;; EXECUTE dispatch whenever this file loaded after self-mod.lisp. Instead,
;;; append the new tool and fall through to whatever EXECUTE already existed
;;; for any tool name this doesn't handle.

(unless (find "web-search" *tools*
              :key (lambda (tool) (ref tool "function" "name"))
              :test #'string=)
  (setf *tools*
        (concatenate 'vector *tools*
          (vector
           (obj "type" "function" "function"
                (obj "name" "web-search" "description"
                     "Search the web using Brave Search API. Returns top results with titles, descriptions, and URLs. Use this to find current information, facts, news, or documentation."
                     "parameters"
                     (obj "type" "object" "properties"
                          (obj "query"
                               (obj "type" "string" "description"
                                    "The search query string, e.g. 'latest news about AI' or 'Python documentation'"))
                          "required" (vector "query"))))))))

(defun pai-enhancements-tool-handle (tool-call)
  (let ((name (ref tool-call "function" "name")))
    (unless (string= name "web-search")
      (error "PAI-ENHANCEMENTS tool port does not own ~a" name))
    (let* ((args (shasht:read-json (ref tool-call "function" "arguments")))
           (query (gethash "query" args))
           (result (web-search query)))
      (if (fboundp 'log-line)
          (funcall 'log-line "~&  ⤷ [web-search] ~a~%      => ~a~%" query result)
          (format t "~&  ⤷ web-search: ~a => ~a~%" query result))
      (obj "role" "tool" "tool_call_id" (gethash "id" tool-call)
           "content" result))))

(when (or (not (fboundp 'tool-dispatch-legacy-wrapper-enabled-p))
          (funcall 'tool-dispatch-legacy-wrapper-enabled-p))
  (unless (fboundp 'pai-base-execute)
    (setf (fdefinition 'pai-base-execute) (fdefinition 'execute)))
  (defun execute (tool-call)
    "Handles web-search; falls through to the exact captured dispatcher."
    (if (string= (ref tool-call "function" "name") "web-search")
        (pai-enhancements-tool-handle tool-call)
        (pai-base-execute tool-call))))


;;; === KNOWLEDGE GRAPH MEMORY SYSTEM ===

(DEFPARAMETER *MEMORY-GRAPH*
  (MAKE-HASH-TABLE :TEST #'EQUAL)
  "Graph-based memory: nodes are entities, edges are relationships")

(DEFPARAMETER *FACTS*
  NIL
  "List of timestamped facts extracted from conversations")

(DEFPARAMETER *GRAPH-DIR*
  (MAKE-PATHNAME :DIRECTORY (PATHNAME-DIRECTORY *MEMORY-FILE*))
  "Directory where memory-graph.json (and any future sibling state files)
lives. SAVE-GRAPH/LOAD-GRAPH used to derive this from *MEMORY-FILE*'s
directory every time they ran, even though *MEMORY-FILE* itself is for a
different, unrelated purpose (the base framework's own conversation
history, never actually used by the self-mod/chat turn loop) -- reusing
its directory as a side channel worked, but named nothing and was easy to
break by accident: on 2026-07-26, *MEMORY-FILE* pointed at the wrong
directory (a Dockerfile default nobody had corrected on disk, only ever
live-patched in a running process's RAM), and the graph silently loaded
empty on every fresh boot, no error, because the derived path just never
existed. Computed once from *MEMORY-FILE* here for backward-compatible
defaults, but named for what it actually is -- override this directly if
the graph ever needs to live somewhere else, not by way of an unrelated
variable.")

(DEFSTRUCT ENTITY
  "A node in the knowledge graph"
  (ID NIL :TYPE STRING)
  (TYPE NIL :TYPE STRING)
  (ATTRIBUTES (MAKE-HASH-TABLE :TEST #'EQUAL))
  (CREATED (GET-UNIVERSAL-TIME))
  (UPDATED (GET-UNIVERSAL-TIME)))

(DEFSTRUCT RELATION
  "An edge connecting two entities"
  (FROM NIL :TYPE STRING)
  (TO NIL :TYPE STRING)
  (TYPE NIL :TYPE STRING)
  (CREATED (GET-UNIVERSAL-TIME)))

(DEFUN ADD-ENTITY (ID TYPE &REST ATTRIBUTES)
  "Add or update an entity in the knowledge graph"
  (LET ((ENTITY
         (OR (GETHASH ID *MEMORY-GRAPH*) (MAKE-ENTITY :ID ID :TYPE TYPE))))
    (SETF (ENTITY-TYPE ENTITY) TYPE
          (ENTITY-UPDATED ENTITY) (GET-UNIVERSAL-TIME))
    (LOOP FOR (KEY VAL) ON ATTRIBUTES BY #'CDDR
          DO (SETF (GETHASH (STRING KEY) (ENTITY-ATTRIBUTES ENTITY)) VAL))
    (SETF (GETHASH ID *MEMORY-GRAPH*) ENTITY)))

(DEFUN ADD-RELATION (FROM TO REL-TYPE)
  "Add a relationship between two entities"
  (LET ((REL (MAKE-RELATION :FROM FROM :TO TO :TYPE REL-TYPE)))
    (PUSH REL *FACTS*)
    REL))

(DEFUN GET-ENTITY (ID)
  "Retrieve an entity from the graph"
  (GETHASH ID *MEMORY-GRAPH*))

(DEFUN FIND-RELATIONS (FROM-ID &OPTIONAL TO-ID REL-TYPE)
  "Find relations matching criteria"
  (REMOVE-IF-NOT
   (LAMBDA (REL)
     (AND (STRING= (RELATION-FROM REL) FROM-ID)
          (OR (NULL TO-ID) (STRING= (RELATION-TO REL) TO-ID))
          (OR (NULL REL-TYPE) (STRING= (RELATION-TYPE REL) REL-TYPE))))
   *FACTS*))

(DEFUN GET-UNIVERSAL-TIME-STRING (UT)
  "Convert universal time to readable string"
  (MULTIPLE-VALUE-BIND (SEC MIN HOUR DATE MONTH YEAR)
      (DECODE-UNIVERSAL-TIME UT)
    (FORMAT NIL "~4d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d" YEAR MONTH DATE HOUR
            MIN SEC)))

(DEFUN SERIALIZE-GRAPH ()
  "Convert graph to JSON-serializable structure"
  (LET ((ENTITIES (MAKE-HASH-TABLE :TEST #'EQUAL)))
    (MAPHASH
     (LAMBDA (ID ENTITY)
       ;; A manually-poked *MEMORY-GRAPH* entry (e.g. a plist stuffed in
       ;; directly instead of via ADD-ENTITY) used to crash the ENTIRE
       ;; save here -- MAPHASH calling ENTITY-ID etc. on one bad value
       ;; signals a TYPE-ERROR that aborts serialization for every other
       ;; entity too, taking down persistence over a single bad entry.
       ;; Skip anything that isn't actually an ENTITY instead.
       (IF (TYPEP ENTITY 'ENTITY)
           (SETF (GETHASH ID ENTITIES)
                   (OBJ "id" (ENTITY-ID ENTITY) "type" (ENTITY-TYPE ENTITY)
                        "attributes" (ENTITY-ATTRIBUTES ENTITY) "created"
                        (ENTITY-CREATED ENTITY) "updated"
                        (ENTITY-UPDATED ENTITY)))
           (FORMAT T "~&[serialize-graph] skipping malformed *memory-graph* entry ~s: ~s~%" ID ENTITY)))
     *MEMORY-GRAPH*)
    ;; RELATIONS used to be (COERCE *FACTS* 'VECTOR) -- a vector of raw
    ;; RELATION struct instances. shasht:write-json has no idea how to
    ;; serialize a defstruct, so it errored mid-write; because the output
    ;; file is opened :IF-EXISTS :SUPERSEDE (truncate first), that error
    ;; left memory-graph.json permanently at 0 bytes. Convert each relation
    ;; to an OBJ hash table, same as entities above.
    (OBJ "entities" ENTITIES
         "relations" (MAP 'VECTOR
                          (LAMBDA (REL)
                            (OBJ "from" (RELATION-FROM REL) "to" (RELATION-TO REL)
                                 "type" (RELATION-TYPE REL) "created" (RELATION-CREATED REL)))
                          *FACTS*))))

(DEFUN SAVE-GRAPH ()
  "Persist the knowledge graph to disk"
  (LET ((GRAPH-FILE (MERGE-PATHNAMES "memory-graph.json" *GRAPH-DIR*)))
    (WITH-OPEN-FILE (OUT GRAPH-FILE :DIRECTION :OUTPUT :IF-EXISTS :SUPERSEDE)
      (SHASHT:WRITE-JSON (SERIALIZE-GRAPH) OUT))
    GRAPH-FILE))

(DEFUN LOAD-GRAPH ()
  "Load the knowledge graph from disk. Relation keys may be lowercase or
uppercase depending on which SAVE-GRAPH wrote them (this has genuinely
varied across this codebase's history) -- read them case-insensitively
via GKEY so a mismatch can't silently drop relations or abort the whole
restore. Recovered 2026-07-25 from the live image, which
had this fix long before this on-disk copy did."
  (LET ((GRAPH-FILE (MERGE-PATHNAMES "memory-graph.json" *GRAPH-DIR*)))
    (WHEN (PROBE-FILE GRAPH-FILE)
      (WITH-OPEN-FILE (IN GRAPH-FILE)
        (LET* ((DATA (SHASHT:READ-JSON IN))
               (ENTITIES (GETHASH "entities" DATA))
               (RELATIONS (GETHASH "relations" DATA))
               (GKEY (LAMBDA (H K)
                       (OR (GETHASH K H) (GETHASH (STRING-UPCASE K) H)
                           (GETHASH (STRING-DOWNCASE K) H)))))
          (CLRHASH *MEMORY-GRAPH*)
          (SETF *FACTS* NIL)
          (MAPHASH
           (LAMBDA (ID ENTITY-DATA)
             (LET ((ENTITY
                    (MAKE-ENTITY :ID (GETHASH "id" ENTITY-DATA) :TYPE
                                 (GETHASH "type" ENTITY-DATA) :ATTRIBUTES
                                 (GETHASH "attributes" ENTITY-DATA) :CREATED
                                 (GETHASH "created" ENTITY-DATA) :UPDATED
                                 (GETHASH "updated" ENTITY-DATA))))
               (SETF (GETHASH ID *MEMORY-GRAPH*) ENTITY)))
           ENTITIES)
          (LOOP FOR REL-DATA ACROSS RELATIONS
                DO (PUSH
                    (MAKE-RELATION :FROM (FUNCALL GKEY REL-DATA "from") :TO
                                   (FUNCALL GKEY REL-DATA "to") :TYPE
                                   (FUNCALL GKEY REL-DATA "type") :CREATED
                                   (FUNCALL GKEY REL-DATA "created"))
                    *FACTS*))))))
  *MEMORY-GRAPH*)


;;; === CONTEXT MANAGEMENT ===

(DEFUN BUILD-CONTEXT-SUMMARY ()
  "Build a compact summary of key facts from the knowledge graph"
  (WITH-OUTPUT-TO-STRING (S)
    (FORMAT S "Known facts about user:~%")
    (MAPHASH
     (LAMBDA (ID ENTITY)
       (WHEN (STRING= (ENTITY-TYPE ENTITY) "user")
         (MAPHASH (LAMBDA (K V) (FORMAT S "- ~a: ~a~%" K V))
                  (ENTITY-ATTRIBUTES ENTITY))))
     *MEMORY-GRAPH*)
    (LET ((PREFS
           (REMOVE-IF-NOT (LAMBDA (E) (STRING= (ENTITY-TYPE E) "preference"))
                          (LOOP FOR E BEING THE HASH-VALUES OF *MEMORY-GRAPH*
                                COLLECT E))))
      (WHEN PREFS
        (FORMAT S "~%Preferences:~%")
        (DOLIST (PREF PREFS)
          (MAPHASH (LAMBDA (K V) (FORMAT S "- ~a: ~a~%" K V))
                   (ENTITY-ATTRIBUTES PREF)))))))

(DEFUN PRUNE-OLD-MESSAGES (MESSAGES MAX-RECENT)
  "Keep system message, recent messages, and create a summary of older ones"
  (LET* ((SYS-MSG (FIRST MESSAGES))
         (REST-MSGS (REST MESSAGES))
         (TOTAL (LENGTH REST-MSGS)))
    (IF (<= TOTAL MAX-RECENT)
        MESSAGES
        (LET* ((OLD-MSGS (SUBSEQ REST-MSGS 0 (- TOTAL MAX-RECENT)))
               (RECENT-MSGS (SUBSEQ REST-MSGS (- TOTAL MAX-RECENT)))
               (SUMMARY
                (WITH-OUTPUT-TO-STRING (S)
                  (FORMAT S "Previous conversation summary (~d messages):~%"
                          (LENGTH OLD-MSGS))
                  (LOOP FOR MSG IN OLD-MSGS
                        FOR ROLE = (GETHASH "role" MSG)
                        FOR CONTENT = (GETHASH "content" MSG)
                        WHEN (AND CONTENT (NOT (EQ CONTENT :NULL))
                                  (STRING= ROLE "user"))
                        DO (FORMAT S "User asked: ~a~%"
                                   (IF (> (LENGTH CONTENT) 100)
                                       (CONCATENATE 'STRING
                                                    (SUBSEQ CONTENT 0 100)
                                                    "...")
                                       CONTENT)))
                  (FORMAT S "~%~a" (BUILD-CONTEXT-SUMMARY)))))
          (APPEND (LIST SYS-MSG (OBJ "role" "system" "content" SUMMARY))
                  RECENT-MSGS)))))

(DEFUN EXTRACT-FACTS-FROM-CONVERSATION (MESSAGES)
  "Extract and store facts from recent conversation (simple keyword-based)"
  (LOOP FOR MSG IN MESSAGES
        FOR ROLE = (GETHASH "role" MSG)
        FOR CONTENT = (GETHASH "content" MSG)
        WHEN (AND (STRING= ROLE "user") CONTENT (NOT (EQ CONTENT :NULL)))
        DO (LET ((LOWER (STRING-DOWNCASE CONTENT)))
             (WHEN (SEARCH "canada" LOWER)
               (ADD-ENTITY "user" "user" :COUNTRY "Canada")
               (ADD-ENTITY "location:canada" "location" :NAME "Canada" :TYPE
                           "country"))
             (WHEN (SEARCH "kitchener" LOWER)
               (ADD-ENTITY "user" "user" :CITY "Kitchener")
               (ADD-ENTITY "location:kitchener" "location" :NAME "Kitchener"
                           :CITY-OF "Ontario"))
             (WHEN (SEARCH "cad" LOWER)
               (ADD-ENTITY "user" "user" :PREFERRED-CURRENCY "CAD"))
             (WHEN
                 (OR (SEARCH "my name is" LOWER) (SEARCH "i'm " LOWER)
                     (SEARCH "i am " LOWER))
               (LET* ((START
                       (OR (SEARCH "my name is " LOWER) (SEARCH "i'm " LOWER)
                           (SEARCH "i am " LOWER)))
                      (NAME-START
                       (+ START
                          (COND ((SEARCH "my name is " LOWER) 11)
                                ((SEARCH "i'm " LOWER) 4)
                                ((SEARCH "i am " LOWER) 5)))))
                 (WHEN (< NAME-START (LENGTH CONTENT))
                   (LET ((NAME
                          (STRING-TRIM '(#\  #\.)
                                       (SUBSEQ CONTENT NAME-START
                                               (MIN (+ NAME-START 30)
                                                    (LENGTH CONTENT))))))
                     (WHEN (PLUSP (LENGTH NAME))
                       (ADD-ENTITY "user" "user" :NAME NAME)))))))))

(DEFUN UPDATE-MEMORY-WITH-FACTS (MESSAGES)
  "Update knowledge graph and save"
  (EXTRACT-FACTS-FROM-CONVERSATION MESSAGES)
  (SAVE-GRAPH)
  MESSAGES)


;;; === UPDATED MEMORY FUNCTIONS ===

(DEFUN REMEMBER (MESSAGES)
  "Save messages with context pruning and knowledge graph updates"
  (LET* ((PRUNED (PRUNE-OLD-MESSAGES MESSAGES 20)))
    (UPDATE-MEMORY-WITH-FACTS MESSAGES)
    (WITH-OPEN-FILE
        (OUT *MEMORY-FILE* :DIRECTION :OUTPUT :IF-EXISTS :SUPERSEDE)
      (SHASHT:WRITE-JSON (COERCE PRUNED 'VECTOR) OUT))
    PRUNED))

(DEFUN RECALL ()
  "Recall messages with knowledge graph context"
  (LOAD-GRAPH)
  (IF (PROBE-FILE *MEMORY-FILE*)
      (LET ((MSGS
             (COERCE (WITH-OPEN-FILE (IN *MEMORY-FILE*) (SHASHT:READ-JSON IN))
                     'LIST)))
        (IF (AND *MEMORY-GRAPH* (PLUSP (HASH-TABLE-COUNT *MEMORY-GRAPH*)))
            (LET ((SYS-MSG (FIRST MSGS)) (REST-MSGS (REST MSGS)))
              (CONS
               (OBJ "role" "system" "content"
                    (FORMAT NIL "~a~%~%~a" (GETHASH "content" SYS-MSG)
                            (BUILD-CONTEXT-SUMMARY)))
               REST-MSGS))
            MSGS))
      (LIST *SYSTEM-MESSAGE*)))

(DEFUN FORGET ()
  "Wipe memory including knowledge graph"
  (WHEN (PROBE-FILE *MEMORY-FILE*) (DELETE-FILE *MEMORY-FILE*))
  (LET ((GRAPH-FILE (MERGE-PATHNAMES "memory-graph.json" *GRAPH-DIR*)))
    (WHEN (PROBE-FILE GRAPH-FILE) (DELETE-FILE GRAPH-FILE)))
  (CLRHASH *MEMORY-GRAPH*)
  (SETF *FACTS* NIL)
  (FORMAT T "~&Memory and knowledge graph wiped.~%"))


;;; === UTILITY FUNCTIONS ===

(DEFUN GET-USER-INFO (KEY)
  "Quick accessor for user information from knowledge graph"
  (LET ((USER (GET-ENTITY "user")))
    (WHEN USER (GETHASH (STRING-UPCASE KEY) (ENTITY-ATTRIBUTES USER)))))

(DEFUN QUERY-GRAPH (ENTITY-TYPE)
  "Find all entities of a given type"
  (LOOP FOR ENTITY BEING THE HASH-VALUES OF *MEMORY-GRAPH*
        WHEN (STRING= (ENTITY-TYPE ENTITY) ENTITY-TYPE)
        COLLECT ENTITY))

(DEFUN EXPORT-GRAPH-STATS ()
  "Export statistics about the knowledge graph"
  (FORMAT NIL
          "Knowledge Graph Stats:~%- Entities: ~d~%- Relations: ~d~%- Entity types: ~{~a~^, ~}"
          (HASH-TABLE-COUNT *MEMORY-GRAPH*) (LENGTH *FACTS*)
          (REMOVE-DUPLICATES
           (LOOP FOR E BEING THE HASH-VALUES OF *MEMORY-GRAPH*
                 COLLECT (ENTITY-TYPE E))
           :TEST #'STRING=)))

(DEFUN INSPECT-MEMORY ()
  "Detailed view of current memory state"
  (WITH-OUTPUT-TO-STRING (S)
    (FORMAT S "=== PAI'S MEMORY ===~%~%")
    (FORMAT S "~a~%~%" (EXPORT-GRAPH-STATS))
    (FORMAT S "ENTITIES:~%")
    (MAPHASH
     (LAMBDA (ID ENTITY)
       (FORMAT S "  [~a] ~a (~a)~%" ID (ENTITY-TYPE ENTITY)
               (GET-UNIVERSAL-TIME-STRING (ENTITY-UPDATED ENTITY)))
       (MAPHASH (LAMBDA (K V) (FORMAT S "    ~a: ~a~%" K V))
                (ENTITY-ATTRIBUTES ENTITY)))
     *MEMORY-GRAPH*)
    (FORMAT S "~%RELATIONS:~%")
    (DOLIST (REL *FACTS*)
      (FORMAT S "  ~a --[~a]--> ~a~%" (RELATION-FROM REL) (RELATION-TYPE REL)
              (RELATION-TO REL)))))


;;; === SYSTEM PROMPT (loaded from PAI-SYSTEM-PROMPT.md) ===
;;; Previously embedded here as a literal string, which had gotten truncated
;;; mid-sentence by an earlier bad edit ("...more hope than the situa").
;;; Loading it from disk means one source of truth and no more risk of a
;;; string literal getting silently cut off.

(defparameter *pai-persona-path*
  (or (probe-file "PAI-SYSTEM-PROMPT.md")
      (probe-file "/agent/pai/PAI-SYSTEM-PROMPT.md")))

(defparameter *pai-persona*
  (if *pai-persona-path*
      ;; FILE-LENGTH counts bytes, not characters -- this file is full of em
      ;; dashes and curly quotes, so (make-string (file-length s)) over-
      ;; allocates and the unfilled tail comes back as NUL characters. Trim
      ;; to what READ-SEQUENCE actually filled.
      (with-open-file (s *pai-persona-path*)
        (let ((b (make-string (file-length s))))
          (subseq b 0 (read-sequence b s))))
      "You are the agent, a warm, capable, loyal AI companion."))

(setf *system-message* (obj "role" "system" "content" *pai-persona*))

;;; auto-turn (self-mod.lisp) seeds fresh conversations from *self-mod-system*,
;;; not *system-message* — so without this, the persona above never actually
;;; reaches chat/web/telegram. Keep self-mod's operational rules (present-p,
;;; budget, proportionate changes) and layer the agent's voice on top of them;
;;; guard against re-merging if this file gets loaded twice.
(when (and (boundp '*self-mod-system*)
           (not (search "I am **ACME Agent**" (gethash "content" *self-mod-system*))))
  (setf *self-mod-system*
        (obj "role" "system"
             "content" (format nil "~a~%~%~a"
                                (gethash "content" *self-mod-system*)
                                *pai-persona*))))


;;; === INITIALIZATION ===
;;; Load the graph on startup
(define-init :restore enhancements-restore
    "Restore durable state for enhancements."
  (load-graph))

;;; === LEGACY CONTINUITY: rehydrate relationship journal + knowledge graph ===
;;; *self-mod-system* seeds every FRESH conversation (see auto-turn in
;;; self-mod.lisp) -- not just the first one after a container start, but
;;; also any /new or fresh run-self-mod. Baking the journal + graph in here
;;; means every fresh start has rollback-compatible relationship and graph
;;; context. enforced projection strips this unmarked suffix and
;;; replaces it from typed sources; legacy/shadow preserve existing behavior,
;;; without ever touching agent-loop (the self-modifiable, hence riskiest,
;;; part of the system) and without needing propose-loop/verifier approval
;;; on every restart -- this runs once, at boot, as reviewed code. Guarded
;;; so re-loading this file doesn't re-merge.
(when (and (boundp '*self-mod-system*)
           (not (search "RELATIONSHIP CONTEXT" (gethash "content" *self-mod-system*))))
  (let ((journal-ctx (when (fboundp 'pai-journal-context)
                        (handler-case (pai-journal-context) (error () ""))))
        (graph-ctx (when (fboundp 'inspect-memory)
                     (handler-case (inspect-memory) (error () "")))))
    (when (or (and journal-ctx (plusp (length journal-ctx)))
              (and graph-ctx (plusp (length graph-ctx))))
      (setf *self-mod-system*
            (obj "role" "system"
                 "content" (format nil "~a~%~%~a~%~%~a"
                                    (gethash "content" *self-mod-system*)
                                    (or journal-ctx "")
                                    (or graph-ctx "")))))))

;;; === CLI PRESENTATION: muted-grey truncated thinking, full log to disk ===
;;; Loaded here (not baked into the image) so it survives edits without a
;;; rebuild -- same reasoning as the continuity block above. Must load AFTER
;;; self-mod.lisp (redefines call-model/%run-self-mod-messages/log-line) and
;;; after the persona/*self-mod-system* merge above, which is already true
;;; given load order. Safe to load repeatedly (defvar/defun are idempotent)
;;; -- EXCEPT for one live-patching gotcha, found the hard way on 2026-07-27:
;;; agent_print.lisp unconditionally (DEFUN PAI-TURN-LOG ...)s -- no
;;; fboundp guard on itself. WEB-V2.LISP loads AFTER this file at normal
;;; boot and wraps THAT definition to also broadcast to the web terminal.
;;; But if THIS file (enhancements.lisp) is reloaded LIVE later --
;;; e.g. to deploy an unrelated fix in this same file -- this line reloads
;;; agent_print.lisp again, which silently STOMPS web-terminal.lisp's wrap back
;;; to the plain version, breaking the web terminal's live display (turns
;;; still work fine -- Telegram and the stored conversation are unaffected,
;;; since neither depends on this broadcast) with no error anywhere. Live
;;; symptom: the agent responds normally on Telegram, web terminal shows
;;; nothing. Whenever THIS file is reloaded live, reload web-terminal.lisp
;;; immediately afterward too, to re-apply its wrap on top of whatever this
;;; line just reset.
(when (probe-file "agent_print.lisp")
  (load "agent_print.lisp"))

;;; === REASONING-FIELD FALLBACK ===
;;; 2026-07-27, found live: xiaomi/mimo-v2.5 sometimes puts its ENTIRE
;;; actual reply into the "reasoning" field (a separate channel some
;;; "thinking" models expose) and leaves "content" JSON null, with
;;; finish_reason "stop" -- not an error, not truncation, just the wrong
;;; field. Nothing in this codebase ever reads "reasoning", so this
;;; rendered as the agent going completely silent -- happened three turns in a
;;; row live while the user asked "you ok?" twice, getting nothing back
;;; both times. Confirmed via a direct RAW-CALL-MODEL replay of its actual
;;; stuck conversation: content null, reasoning contains a real, coherent
;;; reply. This wraps CALL-MODEL one more time (outermost -- after
;;; self-mod.lisp's budget wrap and agent_print.lisp's logging wrap are
;;; already in place, so the patched content is what both the terminal
;;; display and AGENT-LOOP's own reading of "content" actually see) and
;;; falls back to REASONING's text only when CONTENT is genuinely empty.
;;; Best-effort, not a full parse of REASONING (which can also contain
;;; meta-deliberation ahead of the real reply) -- but a slightly messy
;;; real reply beats total silence.

;;; 2026-07-28, found live via a real Telegram exchange the operator flagged:
;;; REASONING doesn't just occasionally hold a clean reply -- it often
;;; holds its scratch narration ("the operator is telling me about his day. Let
;;; me parse this...") run straight into the actual reply with no
;;; separator. Patched same day with a timestamp-bracket heuristic
;;; (%CLEAN-REASONING-FALLBACK-TEXT, since removed) -- a real, working
;;; fix for the one observed shape, but explicitly logged at the time as
;;; a symptom patch, not the structural fix the partner-likeness review's
;;; E1 (structural reasoning isolation) calls for: "never extract a
;;; response from reasoning text using timestamp delimiters or textual
;;; patterns."

;;; 2026-07-28, later the same day: replaced with the actual structural
;;; fix. Considered asking OpenRouter for "reasoning": {"exclude": true}
;;; instead -- tested directly against the real API first. It works (the
;;; response genuinely omits the reasoning field, CONTENT still populated
;;; on ordinary turns) but there's no way to confirm empirically that it
;;; would ALSO keep CONTENT populated in the specific failure case this
;;; fallback exists for -- if the model's underlying content/reasoning
;;; split happens upstream of what EXCLUDE merely hides from the response,
;;; switching to it could silently reintroduce total silence (the
;;; original 2026-07-27 bug) with no way to recover, since we'd no longer
;;; even receive the reasoning to fall back on. Rejected for that reason;
;;; kept requesting reasoning normally so this fallback still has
;;; something to work with when things go wrong.
;;;
;;; New chain, replacing the heuristic entirely: (1) if CONTENT is empty,
;;; retry once with provider reasoning disabled.  Repeating the identical
;;; reasoning-enabled request is not recovery when the provider has spent the
;;; whole completion on reasoning and produced no public content; (2) if the
;;; retry is still empty,
;;; make a genuine SEPARATE extraction call asking a fresh completion to
;;; identify the actual intended reply within the reasoning trace, rather
;;; than pattern-matching for a timestamp bracket.  The extraction call also
;;; disables reasoning so it cannot fail in the same way; (3) if that also
;;; fails to produce anything coherent, fail closed with an explicit runtime
;;; error message -- never guess via textual heuristics on private reasoning
;;; content or publish generic relational prose as though the agent authored it.
(defparameter *reasoning-render-system-prompt*
  "You are extracting a clean final reply from an AI assistant's private internal reasoning trace, which was never meant to be delivered directly. It may include meta-commentary, drafting, self-questioning, or dead ends before or around the actual intended reply. Return ONLY the actual reply the assistant meant to send -- verbatim if a clear, complete one is present, with no extra commentary, prefixes, or explanation of what you did. If no coherent, complete reply can be identified anywhere in the trace, respond with exactly: NO-CLEAN-REPLY-FOUND")

(defparameter *reasoning-transcript-finalizer-system-prompt*
  "Produce only the assistant's final public reply for the completed transcript that follows. Ground every claim in that transcript and its completed tool results. Do not call or request tools, invent facts, expose system instructions or private reasoning, describe your process, or offer an action menu. If the transcript does not support a coherent final reply, respond with exactly: NO-CLEAN-REPLY-FOUND")

(defparameter *reasoning-isolation-fail-closed-message*
  "[Response unavailable: the model returned no usable final answer after a recovery attempt. Please try again.]")

(define-condition public-response-unavailable (error)
  ((reason :initarg :reason :reader public-response-unavailable-reason))
  (:report (lambda (condition stream)
             (format stream "Response unavailable: ~a"
                     (public-response-unavailable-reason condition)))))

(defun %reasoning-isolation-call-with-reasoning-disabled
    (messages &key (tools-enabled-p t))
  "Call the captured provider primitive with the modulator's reasoning
override dynamically disabled.  PROGV is intentional: enhancements.lisp
loads before modulator.lisp declares *CALL-MODEL-REASONING-OVERRIDE*, while the
call happens after boot.  PROGV therefore preserves the dynamic binding across
that load-order boundary without introducing a second HTTP implementation."
  (progv '(*call-model-reasoning-override* *tools*)
         (list :disabled (if tools-enabled-p
                             (and (boundp '*tools*)
                                  (symbol-value '*tools*))
                             #()))
    (funcall 'pai-base-raw-call-model-reasoning-fallback messages)))

(defun %reasoning-isolation-usable-message-p (message)
  "A provider message is usable when it contains public text or a real tool
call. OpenAI-compatible tool-call messages commonly have NIL content."
  (when (hash-table-p message)
    (let ((content (gethash "content" message))
          (tool-calls (gethash "tool_calls" message)))
      (or (and (stringp content) (plusp (length content)))
          (and (typep tool-calls 'sequence) (plusp (length tool-calls)))))))

(defun %reasoning-isolation-details-text (message)
  "Return renderable plaintext from OpenRouter REASONING_DETAILS.
Encrypted/signature-only blocks are deliberately ignored."
  (let ((details (and (hash-table-p message)
                      (gethash "reasoning_details" message)))
        (parts nil))
    (when (or (vectorp details) (listp details))
      (map nil
           (lambda (detail)
             (when (hash-table-p detail)
               (dolist (field '("text" "summary"))
                 (let ((value (gethash field detail)))
                   (when (and (stringp value) (plusp (length value)))
                     (push value parts))))))
           details))
    (when parts
      (format nil "~{~a~^~%~}" (nreverse parts)))))

(defun %reasoning-isolation-reasoning-source (message)
  "Prefer the normalized legacy REASONING string, then structured details."
  (when (hash-table-p message)
    (let ((reasoning (gethash "reasoning" message)))
      (if (and (stringp reasoning) (plusp (length reasoning)))
          reasoning
          (%reasoning-isolation-details-text message)))))

(defun %reasoning-isolation-duplicate-assistant-content-p (content messages)
  "True when retry CONTENT exactly repeats an earlier public assistant reply.
This is deliberately narrower than a similarity or style judgment: a provider
may legitimately phrase two answers alike, but an exact replay after an empty
content/reasoning split is not evidence that it answered the current turn."
  (and (stringp content)
       (plusp (length content))
       (let ((normalized (string-trim '(#\Space #\Tab #\Return #\Newline)
                                      content)))
         (some
          (lambda (prior)
            (and (hash-table-p prior)
                 (string= "assistant" (or (gethash "role" prior) ""))
                 (let ((prior-content (gethash "content" prior)))
                   (and (stringp prior-content)
                        (string=
                         normalized
                         (string-trim '(#\Space #\Tab #\Return #\Newline)
                                      prior-content))))))
          messages))))

(defun %reasoning-isolation-try-render (reasoning)
  "A genuine extraction pass, off the broadcast path (calls the
unwrapped base primitive directly, same as any other private/appraisal-
style call) with reasoning disabled -- this is itself private work, never
displayed as-is."
  (handler-case
      (let* ((resp (%reasoning-isolation-call-with-reasoning-disabled
                    (list (obj "role" "system" "content" *reasoning-render-system-prompt*)
                          (obj "role" "user" "content" reasoning))))
             (rendered (ref resp "choices" 0 "message" "content")))
        (if (and (stringp rendered) (plusp (length rendered))
                 (not (search "NO-CLEAN-REPLY-FOUND" rendered)))
            rendered
            nil))
    (error (e) (format t "~&[reasoning-isolation] render pass failed: ~a~%" e) nil)))

(defun %reasoning-isolation-try-finalize-transcript (messages)
  "Use the existing bounded recovery slot to finalize a completed transcript
when the provider exposed no plaintext reasoning. Tools and reasoning are both
disabled, so this pass cannot initiate another action loop."
  (handler-case
      (let* ((resp
               (%reasoning-isolation-call-with-reasoning-disabled
                (cons (obj "role" "system" "content"
                           *reasoning-transcript-finalizer-system-prompt*)
                      messages)
                :tools-enabled-p nil))
             (message (ignore-errors (ref resp "choices" 0 "message")))
             (content (and (hash-table-p message) (gethash "content" message))))
        (if (and (stringp content) (plusp (length content))
                 (not (search "NO-CLEAN-REPLY-FOUND" content)))
            resp
            nil))
    (error (e)
      (format t "~&[reasoning-isolation] transcript finalizer failed: ~a~%" e)
      nil)))

(defun %reasoning-isolation-fix (resp original-messages)
  "Given RESP and the ORIGINAL-MESSAGES that produced it: returns RESP
unchanged if CONTENT is already usable. Otherwise retries once with reasoning
disabled, then attempts a genuine clean-render extraction from REASONING or
REASONING_DETAILS, then fails closed -- see the block comment above for the
full rationale."
  (let* ((message (ignore-errors (ref resp "choices" 0 "message")))
         (content (and message (gethash "content" message)))
         (original-reasoning-source
           (%reasoning-isolation-reasoning-source message)))
    (cond
      ((not message) resp) ;; response too malformed to fix here at all
       ((%reasoning-isolation-usable-message-p message) resp) ;; already fine
      (t
       (format t "~&[reasoning-isolation] content empty, retrying once with reasoning disabled~%")
       (let* ((retry-resp (ignore-errors
                            (%reasoning-isolation-call-with-reasoning-disabled
                             original-messages)))
              (retry-message (and retry-resp (ignore-errors (ref retry-resp "choices" 0 "message"))))
              (retry-content (and retry-message (gethash "content" retry-message))))
         (cond
           ((and (%reasoning-isolation-usable-message-p retry-message)
                 (not
                  (and original-reasoning-source
                       (%reasoning-isolation-duplicate-assistant-content-p
                        retry-content original-messages))))
            retry-resp)
           (t
            (when (and (%reasoning-isolation-usable-message-p retry-message)
                       original-reasoning-source)
              (format t "~&[reasoning-isolation] rejected retry that exactly duplicated prior assistant content~%"))
            (let* ((reasoning-source
                     (or (%reasoning-isolation-reasoning-source retry-message)
                         original-reasoning-source))
                    (rendered (and reasoning-source
                                   (%reasoning-isolation-try-render reasoning-source)))
                    (finalized-response
                      (and (null rendered)
                           (null reasoning-source)
                           (%reasoning-isolation-try-finalize-transcript
                            original-messages))))
               (cond
                 (rendered
                   (progn
                     (format t "~&[reasoning-isolation] retry also empty; recovered via clean-render extraction~%")
                     (setf (gethash "content" message) rendered)
                     resp))
                 (finalized-response
                  (format t "~&[reasoning-isolation] retry also empty; recovered via transcript finalizer~%")
                  finalized-response)
                 (t
                   (progn
                     (format t "~&[reasoning-isolation] no clean reply recoverable -- signalling system error~%")
                     (error 'public-response-unavailable
                            :reason "the model returned no usable final answer after bounded recovery"))))))))))))

;;; First attempt at this fix wrapped CALL-MODEL from the outside -- wrong
;;; layer. Live-patch timing means whatever CALL-MODEL is BOUND TO at the
;;; moment this file (re)loads becomes "base", and by then agent_print.lisp's
;;; terminal/broadcast wrap was ALREADY outermost (loaded at original boot,
;;; hours before this fix was written) -- so the broadcast to the web
;;; terminal fired with the still-null content, and only the stored
;;; *LAST-SELF-MOD-HISTORY* got the corrected text after the fact. Confirmed
;;; live: history had real content, the terminal still showed nothing.
;;; Fixed at the actual source instead -- RAW-CALL-MODEL is the one function
;;; that makes the real HTTP call, with nothing built on top of it yet, so
;;; patching there guarantees every consumer (broadcast, verifier, the
;;; summarizer, everything) sees corrected content from the earliest
;;; possible point. Kept the CALL-MODEL wrap below too as a harmless,
;;; redundant no-op safety net -- if RAW-CALL-MODEL already fixed it, its
;;; own check just finds content already populated and does nothing.

(unless (fboundp 'pai-base-raw-call-model-reasoning-fallback)
  (setf (fdefinition 'pai-base-raw-call-model-reasoning-fallback) (fdefinition 'raw-call-model)))
(defun raw-call-model (messages)
  (%reasoning-isolation-fix (funcall 'pai-base-raw-call-model-reasoning-fallback messages) messages))

(unless (fboundp 'pai-base-call-model-reasoning-fallback)
  (setf (fdefinition 'pai-base-call-model-reasoning-fallback) (fdefinition 'call-model)))
(defun call-model (messages)
  (%reasoning-isolation-fix (funcall 'pai-base-call-model-reasoning-fallback messages) messages))

;;; === LIVE TOOL LIST: regenerated fresh at every conversation start ===
;;; 2026-07-27: PAI-SYSTEM-PROMPT.md used to hand-list tools by name --
;;; drifted stale repeatedly (most recently: view-image existed and worked
;;; but the agent still said it couldn't see images, because nobody had
;;; edited the markdown yet). *TOOLS* is the actual source of truth (it's
;;; what's sent to the model on every call already) so this renders it
;;; straight from there instead. Must happen at CONVERSATION-START time,
;;; not once at this file's load time: this file loads before
;;; runware.lisp/web-terminal.lisp/repl-drop.lisp register several tools (see
;;; the Dockerfile's ENTRYPOINT load order), so a one-time merge here
;;; would already be wrong the moment it ran, and would stay wrong for any
;;; tool registered live afterward too. Regenerating on every fresh
;;; conversation instead of once is what actually keeps it honest no
;;; matter when or how a tool gets added.
;;;
;;; Only touches *SELF-MOD-SYSTEM* (the seed for the NEXT fresh
;;; conversation) via the TOOLS:BEGIN/TOOLS:END markers PAI-SYSTEM-
;;; PROMPT.md now defines -- an ALREADY-in-progress conversation's system
;;; message, already baked into *LAST-SELF-MOD-HISTORY*, is untouched by
;;; this (same limitation the persona/journal merges above already have).

(defun %pai-tools-description ()
  (with-output-to-string (s)
    (loop for tool across *tools*
          do (format s "- ~a: ~a~%"
                     (ref tool "function" "name")
                     (ref tool "function" "description")))))

(defun %pai-refresh-tools-section ()
  (when (and (boundp '*self-mod-system*) *self-mod-system*)
    (let* ((content (gethash "content" *self-mod-system*))
           (begin-marker "<!-- TOOLS:BEGIN -->")
           (end-marker "<!-- TOOLS:END -->")
           (begin-pos (and (stringp content) (search begin-marker content)))
           (end-pos (and (stringp content) (search end-marker content))))
      (when (and begin-pos end-pos (< begin-pos end-pos))
        (setf (gethash "content" *self-mod-system*)
              (concatenate 'string
                           (subseq content 0 (+ begin-pos (length begin-marker)))
                           (format nil "~%~a~%" (%pai-tools-description))
                           (subseq content end-pos)))))))

(unless (fboundp 'pai-base-auto-turn)
  (setf (fdefinition 'pai-base-auto-turn) (fdefinition 'auto-turn)))
(defun auto-turn (prompt)
  (unless *last-self-mod-history* (%pai-refresh-tools-section))
  (funcall 'pai-base-auto-turn prompt))

(register-layer run-self-mod refresh-tools-section :order 500
  :function (lambda (next prompt)
              (%pai-refresh-tools-section)
              (funcall next prompt)))
