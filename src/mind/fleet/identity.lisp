;;;; identity.lisp -- fleet agent identity and peer store persistence.
;;;;
;;;; FLEET_DESIGN.md S2.3: each agent generates a UUIDv4 at first boot,
;;;; persisted in its state directory, as its unique identity across the
;;;; fleet. Display names are labels only -- never identity -- so this file
;;;; deals exclusively in ids; naming is a later, separate concern.
;;;;
;;;; FLEET_DESIGN.md S2.4/S8: peers are persisted alongside the agent's own
;;;; id in one s-expression file (not JSON -- s-expressions are read back
;;;; with a disabled reader, see %FLEET-STORE-READ-FORM, so there is no
;;;; parser to keep in sync with a schema and no JSON gotchas to inherit).
(in-package :pai.fleet)

(defun fleet-uuid4 ()
  "A random UUID v4 string. Unlike the codebase's other ad hoc UUID
generator (%RUNWARE-UUID4, which uses plain COMMON-LISP:RANDOM for a
cosmetic per-request tag), this one must actually be unique across
*processes*, including two agents booting at close to the same moment --
and SBCL's default *RANDOM-STATE* is not reliably distinct per process. It
is whatever the implementation baked into the core image, not reseeded
from OS entropy at startup, so two fresh images that perform the same
operations in the same order before their first RANDOM call draw the exact
same bytes. Confirmed live: two agents launched together generated the
identical id under COMMON-LISP:RANDOM. Drawing from IRONCLAD:RANDOM-DATA
instead -- the same properly-seeded source FLEET-SHARED-SECRET already
uses below -- fixes this; the cost is one extra dependency already paid
for."
  (let ((bytes (ironclad:random-data 16)))
    (setf (aref bytes 6) (logior (logand (aref bytes 6) #x0f) #x40)) ; version 4
    (setf (aref bytes 8) (logior (logand (aref bytes 8) #x3f) #x80)) ; variant
    (format nil "~(~2,'0x~2,'0x~2,'0x~2,'0x-~2,'0x~2,'0x-~2,'0x~2,'0x-~2,'0x~2,'0x-~2,'0x~2,'0x~2,'0x~2,'0x~2,'0x~2,'0x~)"
            (aref bytes 0) (aref bytes 1) (aref bytes 2) (aref bytes 3)
            (aref bytes 4) (aref bytes 5) (aref bytes 6) (aref bytes 7)
            (aref bytes 8) (aref bytes 9) (aref bytes 10) (aref bytes 11)
            (aref bytes 12) (aref bytes 13) (aref bytes 14) (aref bytes 15))))

(defun fleet-shared-secret ()
  "32 random bytes for a per-peer HMAC key (FLEET_DESIGN.md S2.1/S7).
Unlike FLEET-UUID4 this is security-sensitive, so it is drawn from
Ironclad's generator rather than COMMON-LISP:RANDOM."
  (ironclad:random-data 32))

(defstruct (fleet-peer (:constructor %make-fleet-peer))
  id name address shared-secret joined-at
  ;; This agent's own outstanding thread on PEER's board (FLEET_DESIGN.md
  ;; S9 back-and-forth) -- NIL until the first post. Remembering it here
  ;; means every later post to this peer continues the same thread instead
  ;; of starting a fresh, disconnected one each time; see
  ;; FLEET-STORE-SET-PEER-OUTBOUND-THREAD-ID.
  (outbound-thread-id nil))

(defun make-fleet-peer (&key id name address shared-secret joined-at
                              outbound-thread-id)
  "Construct and validate a FLEET-PEER. Every field is required and
type-checked here, once, so a malformed peer can never enter the store
through any caller -- the join handshake, a test fixture, and the s-exp
loader below all funnel through this single constructor."
  (unless (and (stringp id) (plusp (length id)))
    (error "fleet peer requires a non-empty id"))
  (unless (and (stringp name) (plusp (length name)))
    (error "fleet peer requires a non-empty name"))
  (unless (and (stringp address) (plusp (length address)))
    (error "fleet peer requires a non-empty address"))
  (unless (typep shared-secret '(vector (unsigned-byte 8)))
    (error "fleet peer shared-secret must be a byte vector"))
  (unless (integerp joined-at)
    (error "fleet peer requires an integer joined-at (universal-time)"))
  (unless (or (null outbound-thread-id)
              (and (stringp outbound-thread-id) (plusp (length outbound-thread-id))))
    (error "fleet peer outbound-thread-id must be a non-empty string or NIL"))
  (%make-fleet-peer :id id :name name :address address
                     :shared-secret shared-secret :joined-at joined-at
                     :outbound-thread-id outbound-thread-id))

(defstruct (fleet-store (:constructor %make-fleet-store))
  agent-id
  (peers (make-hash-table :test #'equal))
  path)

(defun fleet-store-peer (store peer-id)
  (gethash peer-id (fleet-store-peers store)))

(defun fleet-store-add-peer (store peer)
  "Add or replace a peer, then persist immediately -- membership must never
be lost to a crash between accepting a peer and the next unrelated save."
  (setf (gethash (fleet-peer-id peer) (fleet-store-peers store)) peer)
  (fleet-store-save store)
  peer)

(defun fleet-store-set-peer-outbound-thread-id (store peer-id thread-id)
  "Remember THREAD-ID as this agent's outstanding thread on PEER-ID's board,
then persist immediately, mirroring FLEET-STORE-ADD-PEER's discipline --
losing this silently to a crash just means the next post starts a new
thread instead of continuing the existing one, which is recoverable, not
corrupting, but still worth not losing needlessly."
  (let ((peer (fleet-store-peer store peer-id)))
    (unless peer (error "no such peer: ~a" peer-id))
    (setf (fleet-peer-outbound-thread-id peer) thread-id)
    (fleet-store-save store)
    peer))

(defun %fleet-peer-to-plist (peer)
  (list :id (fleet-peer-id peer)
        :name (fleet-peer-name peer)
        :address (fleet-peer-address peer)
        :shared-secret (ironclad:byte-array-to-hex-string
                         (fleet-peer-shared-secret peer))
        :joined-at (fleet-peer-joined-at peer)
        :outbound-thread-id (fleet-peer-outbound-thread-id peer)))

(defun %fleet-peer-from-plist (plist)
  (make-fleet-peer :id (getf plist :id)
                    :name (getf plist :name)
                    :address (getf plist :address)
                    :shared-secret (ironclad:hex-string-to-byte-array
                                     (getf plist :shared-secret))
                    :joined-at (getf plist :joined-at)
                    :outbound-thread-id (getf plist :outbound-thread-id)))

(defparameter +fleet-store-schema-version+ 1)

(defun %fleet-safe-read-form (path)
  "Read exactly one s-expression from PATH with *READ-EVAL* disabled and
bare symbols interned into a throwaway, :CL-USER-free package -- shared by
every s-expression-persisted fleet store (peer store, board store: the
format only ever contains keywords and self-evaluating literals), so this
costs nothing and means a hand-edited or corrupted store file can never
execute code or collide with a real package on load. USE :COMMON-LISP,
not NIL: NIL itself (an empty list -- the common case for a fresh store's
peers, or a thread with no messages yet) is a bare symbol token like any
other, and without inheriting COMMON-LISP it reads as a distinct, useless
symbol rather than the actual empty list -- confirmed live, this broke
loading any peer store with zero peers. NIL and T are universal, harmless
constants; *READ-EVAL* NIL is what actually keeps this reader from
executing anything."
  (let ((*read-eval* nil)
        (*package* (or (find-package :pai.fleet.reader)
                       (make-package :pai.fleet.reader :use '(:common-lisp)))))
    (with-open-file (stream path :direction :input :external-format :utf-8)
      (read stream))))

(defun fleet-store-load (path agent-id-fn)
  "Load the peer store at PATH, creating a fresh one (agent id from
AGENT-ID-FN, called with no arguments) if the file does not yet exist.
AGENT-ID-FN is a parameter rather than a direct call to FLEET-UUID4 so
tests can supply a deterministic id instead of a random one."
  (if (probe-file path)
      (let ((form (%fleet-safe-read-form path)))
        (unless (eql +fleet-store-schema-version+ (getf form :schema-version))
          (error "fleet store at ~a has an unsupported schema version ~a"
                 path (getf form :schema-version)))
        (let ((store (%make-fleet-store :agent-id (getf form :agent-id)
                                         :path path)))
          (dolist (peer-plist (getf form :peers))
            (setf (gethash (getf peer-plist :id) (fleet-store-peers store))
                  (%fleet-peer-from-plist peer-plist)))
          store))
      (let ((store (%make-fleet-store :agent-id (funcall agent-id-fn)
                                       :path path)))
        (fleet-store-save store)
        store)))

(defun fleet-store-save (store)
  "Write STORE to its path atomically: write to a sibling temp file, then
rename over the target, mirroring SAVE-SELF-MOD-PROPOSALS. A crash mid-write
must never leave a truncated peer store -- shared secrets here have no other
copy, and losing one silently drops fleet membership."
  (let* ((path (fleet-store-path store))
         (tmp (make-pathname
               :name (concatenate 'string (pathname-name path) "-tmp")
               :type (pathname-type path) :defaults path)))
    (ensure-directories-exist path)
    (with-open-file (stream tmp :direction :output :if-exists :supersede
                                 :if-does-not-exist :create
                                 :external-format :utf-8)
      (let ((*print-pretty* t) (*print-readably* nil))
        (prin1 (list :schema-version +fleet-store-schema-version+
                     :agent-id (fleet-store-agent-id store)
                     :peers (loop for peer being the hash-values
                                    of (fleet-store-peers store)
                                  collect (%fleet-peer-to-plist peer)))
               stream)))
    (rename-file tmp path)
    store))
