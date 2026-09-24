;;;; board.lisp -- per-agent bulletin board (FLEET_DESIGN.md S3).
;;;;
;;;; "Only the board owner writes to its own board" (S3.2): every function
;;;; here operates on exactly one agent's own board. There is no notion of
;;;; a remote board anywhere in this file -- to post on a peer's board,
;;;; that peer's own /board/post endpoint is called instead (web-fleet.lisp).
;;;; Persisted the same way as the peer store (identity.lisp): one
;;;; s-expression file, atomic write-then-rename, read back with
;;;; %FLEET-SAFE-READ-FORM.
(in-package :pai.fleet)

(define-condition board-error (error)
  ((reason :initarg :reason :reader board-error-reason))
  (:report (lambda (condition stream)
             (format stream "Board error: ~a" (board-error-reason condition)))))

(define-condition board-storage-unavailable (error) ()
  (:report (lambda (condition stream) (declare (ignore condition))
             (write-string "Board persistence is unavailable; retry the same operation." stream))))

(defparameter +board-valid-statuses+ '("open" "resolved"))
(defparameter +board-valid-intents+ '("question" "answer" "fyi" "request" "proposal"))
(defparameter +board-valid-scopes+ '("fleet" "pairwise"))

(defstruct (board-thread (:constructor %make-board-thread))
  thread-id title status created-by created-at last-activity-at)

(defstruct (board-message (:constructor %make-board-message))
  msg-id thread-id author-id author-name text tags intent scope timestamp
  reply-to operation-id request-key)

(defstruct (board-store (:constructor %make-board-store))
  (threads (make-hash-table :test #'equal))
  (messages (make-hash-table :test #'equal))
  path)

(defun board-thread (store thread-id)
  (gethash thread-id (board-store-threads store)))

(defun board-threads (store)
  "Every thread, most recently active first."
  (sort (loop for th being the hash-values of (board-store-threads store)
              collect th)
        #'> :key #'board-thread-last-activity-at))

(defun board-thread-messages (store thread-id)
  "Every message in THREAD-ID, oldest first."
  (sort (loop for msg being the hash-values of (board-store-messages store)
              when (equal thread-id (board-message-thread-id msg))
                collect msg)
        (lambda (left right)
          (let ((lt (board-message-timestamp left))
                (rt (board-message-timestamp right)))
            (or (< lt rt)
                (and (= lt rt)
                     (string< (board-message-msg-id left)
                              (board-message-msg-id right))))))))

(defun board-messages-since (store timestamp)
  "Every message with a timestamp strictly greater than TIMESTAMP, oldest
first -- the poll-on-wake sync primitive (FLEET_DESIGN.md S2.6/S3.3)."
  (sort (loop for msg being the hash-values of (board-store-messages store)
              when (> (board-message-timestamp msg) timestamp)
                collect msg)
        #'< :key #'board-message-timestamp))

(defun board-post-message
    (store &key thread-id new-thread-title author-id author-name text
                tags intent scope reply-to operation-id request-key (created-by nil)
                (timestamp (fleet-unix-time)))
  "Post one message to STORE, this agent's own board. Exactly one of
THREAD-ID (an existing open thread) or NEW-THREAD-TITLE (create one,
owned by CREATED-BY or AUTHOR-ID if unspecified) is required. Returns
(VALUES MSG-ID THREAD-ID). Mutates and persists STORE."
  (unless (and (stringp author-id) (plusp (length author-id)))
    (error "board-post-message requires a non-empty author-id"))
  (unless (and (stringp author-name) (plusp (length author-name)))
    (error "board-post-message requires a non-empty author-name"))
  (unless (and (stringp text) (plusp (length text)))
    (error "board-post-message requires non-empty text"))
  (unless (if thread-id (not new-thread-title) new-thread-title)
    (error "board-post-message requires exactly one of thread-id or new-thread-title"))
  (when (and tags (not (and (listp tags) (every #'stringp tags))))
    (error "tags must be a list of strings"))
  (when (and intent (not (member intent +board-valid-intents+ :test #'string=)))
    (error "intent must be one of ~a" +board-valid-intents+))
  (when (and scope (not (member scope +board-valid-scopes+ :test #'string=)))
    (error "scope must be one of ~a" +board-valid-scopes+))
  (when operation-id
    (unless (and (stringp operation-id) (<= 1 (length operation-id) 128)
                 (stringp request-key) (plusp (length request-key)))
      (error "operation-id requires a bounded ID and canonical request-key"))
    (let ((existing
            (loop for message being the hash-values of (board-store-messages store)
                  when (and (equal author-id (board-message-author-id message))
                            (equal operation-id (board-message-operation-id message)))
                    return message)))
      (when existing
        (unless (equal request-key (board-message-request-key existing))
          (error 'board-error :reason "operation-id conflicts with prior message"))
        ;; A previous save may have failed after in-memory insertion.
        (board-store-save store)
        (return-from board-post-message
          (values (board-message-msg-id existing) (board-message-thread-id existing))))))
  (let ((actual-thread-id
          (if thread-id
              (let ((thread (board-thread store thread-id)))
                (unless thread
                  (error 'board-error :reason "no such thread"))
                (unless (string= "open" (board-thread-status thread))
                  (error 'board-error :reason "thread is resolved"))
                thread-id)
              (let ((new-id (fleet-uuid4)))
                (setf (gethash new-id (board-store-threads store))
                      (%make-board-thread
                       :thread-id new-id :title new-thread-title :status "open"
                       :created-by (or created-by author-id)
                       :created-at timestamp :last-activity-at timestamp))
                new-id))))
    (when reply-to
      (let ((parent (gethash reply-to (board-store-messages store))))
        (unless (and parent (equal actual-thread-id (board-message-thread-id parent)))
          (error 'board-error :reason "reply-to does not reference a message in this thread"))))
    (let ((msg-id (fleet-uuid4)))
      (setf (gethash msg-id (board-store-messages store))
            (%make-board-message
             :msg-id msg-id :thread-id actual-thread-id
             :author-id author-id :author-name author-name :text text
             :tags (or tags nil) :intent intent :scope scope
             :timestamp timestamp :reply-to reply-to
             :operation-id operation-id :request-key request-key))
      (setf (board-thread-last-activity-at (board-thread store actual-thread-id))
            timestamp)
      (board-store-save store)
      (values msg-id actual-thread-id))))

;;; --- persistence ----------------------------------------------------------

(defparameter +board-store-schema-version+ 1)

(defun %board-thread-to-plist (thread)
  (list :thread-id (board-thread-thread-id thread)
        :title (board-thread-title thread)
        :status (board-thread-status thread)
        :created-by (board-thread-created-by thread)
        :created-at (board-thread-created-at thread)
        :last-activity-at (board-thread-last-activity-at thread)))

(defun %board-thread-from-plist (plist)
  (%make-board-thread :thread-id (getf plist :thread-id)
                       :title (getf plist :title)
                       :status (getf plist :status)
                       :created-by (getf plist :created-by)
                       :created-at (getf plist :created-at)
                       :last-activity-at (getf plist :last-activity-at)))

(defun %board-message-to-plist (msg)
  (list :msg-id (board-message-msg-id msg)
        :thread-id (board-message-thread-id msg)
        :author-id (board-message-author-id msg)
        :author-name (board-message-author-name msg)
        :text (board-message-text msg)
        :tags (board-message-tags msg)
        :intent (board-message-intent msg)
        :scope (board-message-scope msg)
        :timestamp (board-message-timestamp msg)
        :reply-to (board-message-reply-to msg)
        :operation-id (board-message-operation-id msg)
        :request-key (board-message-request-key msg)))

(defun %board-message-from-plist (plist)
  (%make-board-message :msg-id (getf plist :msg-id)
                        :thread-id (getf plist :thread-id)
                        :author-id (getf plist :author-id)
                        :author-name (getf plist :author-name)
                        :text (getf plist :text)
                        :tags (getf plist :tags)
                        :intent (getf plist :intent)
                        :scope (getf plist :scope)
                        :timestamp (getf plist :timestamp)
                        :reply-to (getf plist :reply-to)
                        :operation-id (getf plist :operation-id)
                        :request-key (getf plist :request-key)))

(defun board-store-load (path)
  "Load the board at PATH, or create a fresh, empty, persisted one if it
does not yet exist."
  (if (probe-file path)
      (let ((form (%fleet-safe-read-form path)))
        (unless (eql +board-store-schema-version+ (getf form :schema-version))
          (error "board store at ~a has an unsupported schema version ~a"
                 path (getf form :schema-version)))
        (let ((store (%make-board-store :path path)))
          (dolist (thread-plist (getf form :threads))
            (let ((thread (%board-thread-from-plist thread-plist)))
              (setf (gethash (board-thread-thread-id thread)
                             (board-store-threads store))
                    thread)))
          (dolist (message-plist (getf form :messages))
            (let ((message (%board-message-from-plist message-plist)))
              (setf (gethash (board-message-msg-id message)
                             (board-store-messages store))
                    message)))
          store))
      (let ((store (%make-board-store :path path)))
        (board-store-save store)
        store)))

(defun %board-store-save (store)
  "Write STORE to its path atomically: write to a sibling temp file, then
rename over the target, same discipline as FLEET-STORE-SAVE."
  (let* ((path (board-store-path store))
         (tmp (make-pathname
               :name (concatenate 'string (pathname-name path) "-tmp")
               :type (pathname-type path) :defaults path)))
    (ensure-directories-exist path)
    (with-open-file (stream tmp :direction :output :if-exists :supersede
                                 :if-does-not-exist :create
                                 :external-format :utf-8)
      (let ((*print-pretty* t) (*print-readably* nil))
        (prin1 (list :schema-version +board-store-schema-version+
                     :threads (loop for thread being the hash-values
                                      of (board-store-threads store)
                                    collect (%board-thread-to-plist thread))
                     :messages (loop for message being the hash-values
                                       of (board-store-messages store)
                                     collect (%board-message-to-plist message)))
               stream)))
    (rename-file tmp path)
    store))

(defun board-store-save (store)
  "Distinguish persistence failure from invalid input at the transport boundary."
  (handler-case (%board-store-save store)
    (error () (error 'board-storage-unavailable))))
