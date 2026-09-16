;;;; conscious-file-search-tool.lisp -- one bounded read-only local capability.

(in-package :agent)

(export '(conscious-file-search-configure conscious-file-search
          conscious-file-search-tool-schema
          conscious-file-search-openai-tool-schema
          conscious-file-search-report))

(defparameter *conscious-file-search-max-query-characters* 256)
(defparameter *conscious-file-search-max-files* 1000)
(defparameter *conscious-file-search-max-file-bytes* 1048576)
(defparameter *conscious-file-search-max-results* 20)
(defparameter *conscious-file-search-max-line-characters* 500)
(defparameter *conscious-file-search-max-result-characters* 12000)
(defparameter *conscious-file-search-extensions*
  '("asd" "c" "cl" "css" "csv" "h" "html" "ini" "js" "json" "lisp"
    "md" "py" "rst" "sexp" "sql" "svg" "toml" "ts" "tsx" "txt" "yaml"
    "yml"))
(defvar *conscious-file-search-root* nil)
(defvar *conscious-file-search-call-count* 0)
(declaim (ftype (function () t) conscious-file-search-report))

(defun %conscious-file-search-directory-prefix-p (prefix directory)
  (and (<= (length prefix) (length directory))
       (loop for expected in prefix
             for actual in directory
             always (equalp expected actual))))

(defun %conscious-file-search-contained-p (root path)
  (and (equalp (pathname-host root) (pathname-host path))
       (equalp (pathname-device root) (pathname-device path))
       (%conscious-file-search-directory-prefix-p
        (pathname-directory root) (pathname-directory path))))

(defun conscious-file-search-configure (root)
  "Bind the capability to one existing canonical read-only directory."
  (let ((canonical (truename (uiop:ensure-directory-pathname (pathname root)))))
    (unless (uiop:directory-exists-p canonical)
      (error "File-search root must be an existing directory"))
    (setf *conscious-file-search-root*
          (uiop:ensure-directory-pathname canonical)))
  (conscious-file-search-report))

(defun %conscious-file-search-exact-arguments (arguments)
  (unless (hash-table-p arguments)
    (error "File-search arguments must be an object"))
  (loop for key being the hash-keys of arguments
        unless (member key '("query" "path" "max_results") :test #'string=)
          do (error "Unknown file-search argument ~s" key))
  (let ((query (gethash "query" arguments))
        (relative (gethash "path" arguments "."))
        (maximum (gethash "max_results" arguments 10)))
    (unless (and (stringp query)
                 (plusp (length (string-trim '(#\Space #\Tab #\Newline
                                                #\Return) query)))
                 (<= (length query)
                     *conscious-file-search-max-query-characters*))
      (error "File-search query must be bounded non-empty text"))
    (unless (and (stringp relative) (plusp (length relative))
                 (not (uiop:absolute-pathname-p (pathname relative)))
                 (not (find ".." (uiop:split-string
                                   relative :separator '(#\/ #\\))
                            :test #'string=)))
      (error "File-search path must remain relative below its configured root"))
    (unless (and (integerp maximum)
                 (<= 1 maximum *conscious-file-search-max-results*))
      (error "File-search max_results is outside its bound"))
    (values query relative maximum)))

(defun %conscious-file-search-target (relative)
  (unless *conscious-file-search-root*
    (error "File-search capability is not configured"))
  (let ((target
          (truename
           (uiop:ensure-directory-pathname
            (merge-pathnames relative *conscious-file-search-root*)))))
    (unless (and (uiop:directory-exists-p target)
                 (%conscious-file-search-contained-p
                  *conscious-file-search-root* target))
      (error "File-search target escapes its configured root"))
    target))

(defun %conscious-file-search-text-file-p (path)
  (member (string-downcase (or (pathname-type path) ""))
          *conscious-file-search-extensions* :test #'string=))

(defun %conscious-file-search-line (line)
  (if (<= (length line) *conscious-file-search-max-line-characters*)
      line
      (subseq line 0 *conscious-file-search-max-line-characters*)))

(defun %conscious-file-search-relative-name (path)
  (substitute #\/ #\\
              (enough-namestring path *conscious-file-search-root*)))

(defun conscious-file-search (arguments)
  "Search bounded text files for a literal string and return inert JSON data."
  (multiple-value-bind (query relative maximum)
      (%conscious-file-search-exact-arguments arguments)
    (let ((target (%conscious-file-search-target relative))
          (matches nil)
          (files-scanned 0)
          (files-skipped 0)
          (truncated nil))
      (labels
          ((record-match (match)
             (push match matches)
             (when (> (length matches) maximum)
               (setf truncated t)))
           (record-file (path)
             (when (and (< files-scanned *conscious-file-search-max-files*)
                        (%conscious-file-search-text-file-p path))
               (let ((canonical (ignore-errors (truename path))))
                 (unless (and canonical
                              (%conscious-file-search-contained-p
                               *conscious-file-search-root* canonical))
                   (error "File-search traversal escaped its configured root"))
                 (incf files-scanned)
                 (when (search query (file-namestring canonical)
                               :test #'char-equal)
                   (record-match
                    (obj "path" (%conscious-file-search-relative-name canonical)
                         "line" :null "text" (file-namestring canonical)
                         "match_kind" "filename")))
                 (handler-case
                     (unless truncated
                       (with-open-file (stream canonical :direction :input
                                                         :external-format :utf-8)
                         (if (> (file-length stream)
                                *conscious-file-search-max-file-bytes*)
                             (incf files-skipped)
                             (loop for line = (read-line stream nil nil)
                                   for line-number from 1
                                   while line
                                   when (search query line :test #'char-equal)
                                     do (record-match
                                         (obj "path"
                                              (%conscious-file-search-relative-name
                                               canonical)
                                              "line" line-number
                                              "text"
                                              (%conscious-file-search-line line)
                                              "match_kind" "content"))
                                        (when truncated (return))))))
                   (error () (incf files-skipped))))))
           (walk (directory)
             (when (and (< files-scanned *conscious-file-search-max-files*)
                        (not truncated))
               (dolist (file (sort (copy-list (uiop:directory-files directory))
                                   #'string< :key #'namestring))
                 (record-file file)
                 (when truncated (return)))
               (dolist (subdirectory
                         (sort (copy-list (uiop:subdirectories directory))
                               #'string< :key #'namestring))
                 (let ((canonical (ignore-errors (truename subdirectory))))
                   (unless (and canonical
                                (%conscious-file-search-contained-p
                                 *conscious-file-search-root* canonical))
                     (error "File-search directory escaped its configured root"))
                   (walk canonical))
                 (when truncated (return))))))
        (walk target))
      (when (>= files-scanned *conscious-file-search-max-files*)
        (setf truncated t))
      (setf matches (nreverse (if (> (length matches) maximum)
                                  (subseq matches 1)
                                  matches)))
      (let ((result
              (obj "schema_version" 1 "status" "ok"
                   "query" query "path" relative
                   "matches" (coerce matches 'vector)
                   "match_count" (length matches)
                   "files_scanned" files-scanned
                   "files_skipped" files-skipped
                   "truncated" (if truncated t nil)
                   "database_write_count" 0)))
        (when (> (length (shasht:write-json result nil))
                 *conscious-file-search-max-result-characters*)
          (error "File-search result exceeded its serialized bound"))
        (incf *conscious-file-search-call-count*)
        result))))

(defun conscious-file-search-tool-schema ()
  (obj "schema_version" 1 "tool_name" "search-files"
       "authority_class" "read-only-local-files"
       "description"
       "Search for a literal string below the configured read-only root."
       "argument_keys" (vector "query" "path" "max_results")
       "max_results" *conscious-file-search-max-results*
       "max_files" *conscious-file-search-max-files*
       "max_file_bytes" *conscious-file-search-max-file-bytes*
       "database_write_count" 0))

(defun conscious-file-search-openai-tool-schema ()
  "Return the closed provider wire schema; it grants no execution authority."
  (obj
   "type" "function"
   "function"
   (obj
    "name" "search-files"
    "description"
    (concatenate
     'string
     "Search for a literal string in bounded text files below the already "
     "authorized workspace root. Use path \".\" for the workspace root; "
     "never send an absolute path.")
    "strict" t
    "parameters"
    (obj
     "type" "object"
     "properties"
     (obj
      "query"
      (obj "type" "string"
           "description" "Bounded non-empty literal text to find.")
      "path"
      (obj "type" "string"
           "description"
           "Relative directory below the authorized root, or . for the root.")
      "max_results"
      (obj "type" "integer" "minimum" 1
           "maximum" *conscious-file-search-max-results*
           "description" "Maximum number of matches to return."))
     "required" (vector "query" "path" "max_results")
     "additionalProperties" nil))))

(defun conscious-file-search-report ()
  (obj "schema_version" 1
       "configured" (if *conscious-file-search-root* t nil)
       "root" (if *conscious-file-search-root*
                  (namestring *conscious-file-search-root*) :null)
       "calls" *conscious-file-search-call-count*
       "database_write_count" 0))
