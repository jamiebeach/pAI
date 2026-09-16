;;;; census.lisp -- build the admission policy from the census manifest.
;;;;
;;;; Workstream Q, slice Q1d. Replaces a hand-maintained Lisp table that was
;;;; supposed to agree with a hand-maintained markdown document, checked by a
;;;; fixture that compared a Lisp constant to a Lisp hash table while being
;;;; described as comparing document to code. It was not checking what it
;;;; claimed, and the two had already drifted before anyone looked.
;;;;
;;;; Now there is one source: docs/event-type-census.sexp. This file builds
;;;; the admission table and the discriminators from it; the markdown is
;;;; generated from it. Agreement is structural rather than asserted, because
;;;; there is nothing left to disagree.
;;;;
;;;; LOADING DEFINES, INITIALIZE STARTS -- so the manifest is read at LOAD
;;;; time here rather than by an init action, because the admission table is a
;;;; definition, not a runtime resource. It reads one file from the source
;;;; tree with no database, network or state directory, which keeps the
;;;; offline-load guarantee intact.

(in-package :agent)

(export '(census-manifest census-entries census-report
          *census-manifest-path* *census-version*))

(defvar *census-version* nil
  "Version declared by the loaded manifest.")

(defvar *census-entries* nil
  "Parsed manifest entries, in file order.")

(defvar *census-spec-kinds* nil
  "The spec 9.1 kind vocabulary the manifest declares itself against.")

(defvar *census-implementation-digest* nil
  "Digest of THIS FILE's content, where discriminator closures live.

The manifest names discriminators; their behaviour is in %CENSUS-DISCRIMINATOR
below. An earlier comment claimed discriminator behaviour lives in the
manifest and was wrong -- two same-key discriminator tables holding different
closures received the same manifest-derived identity, so changing what a
discriminator DOES left the composition hash unmoved.

Hashing this file's source closes that mechanically rather than by asking an
author to bump a version.")

(defvar *census-content-digest* nil
  "Digest of the manifest FILE CONTENT as loaded.

Folded into the composition hash so that any manifest edit -- including one
that changes a discriminator's behaviour without changing the declared
version -- makes two projections incomparable. Hashing the declared version
alone would have trusted an author to bump it, which is the same weakness as
trusting a codelet digest, and here it is avoidable because the manifest is
a file we can read.

HONEST LIMIT: directly mutating *STIMULUS-DISCRIMINATORS* at runtime bypasses
this, because a closure's behaviour cannot be digested. CENSUS-LOAD is the
only supported writer of those tables; anything else is out of contract and
undetectable by design, not by oversight.")

(defparameter *census-manifest-path* "event-type-census.sexp"
  "Resolved by PAI-SOURCE-FILE, which indexes src/ by basename.

The manifest lives beside this file rather than in docs/ because it IS
source: the admission policy is built from it at load time. docs/ holds the
GENERATED markdown, which is the human-readable view. Keeping the source of
truth in the source tree also means PAI-SOURCE-FILE can find it -- docs/ is
not indexed, and a manifest the loader cannot locate is a runtime that admits
nothing.")

;;; --- discriminators ------------------------------------------------------
;;;
;;; Named rather than inline so the manifest stays declarative data. A
;;; discriminator refines kind, sub-kind, urgency and barrier from payload
;;; fields the PRODUCER set -- never from anything a model wrote.

(defun %census-discriminator (name)
  (ecase name
    (:schedule-mode
     ;; A delivered notification is a published EFFECT, not a reason to think
     ;; again. Classifying it as `schedule-due` kept it eligible to trigger a
     ;; pulse and blocked the consumption watermark until acknowledged, which
     ;; is why it is now journal. Awareness of what was published belongs in a
     ;; publication projection.
     (lambda (payload)
       (if (equal (gethash "mode" payload) "notify")
           :journal
           (list "schedule-due" "pending" "timely" t))))
    (:operation-status
     ;; Failure is not a quieter success: a codelet asking "did the work I was
     ;; waiting on land" has to branch on it, and burying the distinction in
     ;; the payload would make that branch invisible to the admission table.
     ;; No status reads as failure rather than assumed success.
     (lambda (payload)
       (let ((status (gethash "status" payload)))
         (if (equal status "succeeded")
             (list "tool-result"  "succeeded" "timely" t)
             (list "tool-failure" (if (stringp status) status "unknown")
                   "timely" t)))))
    (:motivation-candidate
     ;; The base kind remains the established intention-cue vocabulary. The
     ;; sub-kind lets a codelet distinguish this inert private candidate from
     ;; a commitment lifecycle without inspecting producer payload prose.
     (lambda (payload)
       (if (and (string= "curiosity" (gethash "motive_kind" payload ""))
                (string= "private-consideration-only"
                         (gethash "expression_policy" payload "")))
           (list "intention-cue" "curiosity" "background" nil)
           :journal)))))

;;; --- manifest loading ----------------------------------------------------

(defparameter *census-load-directory*
  (and *load-truename* (make-pathname :name nil :type nil :defaults *load-truename*))
  "Directory this file was loaded from, captured at load time.

Under ASDF *LOAD-TRUENAME* points into the fasl cache and this is useless --
which is exactly why PAI-SOURCE-FILE exists and is tried first. But under the
test harness, which LOADs source directly, ASDF has not registered the system,
so PAI-SOURCE-ROOT cannot resolve and PAI-SOURCE-FILE returns NIL. The two
load paths fail in opposite conditions, so the resolver tries both.

This is the same trap recorded in PAI-SOURCE-ROOT's own docstring, met from
the other side: there the danger was computing a root from *LOAD-TRUENAME*
under ASDF; here it is relying solely on ASDF under a harness that does not
use it.")

(defun %census-manifest-file ()
  "Locate the manifest, trying every resolution that works in some context.

Fails loudly rather than defaulting. An admission table that silently stayed
empty would produce a runtime that never wakes for anything -- gotcha 25 (a
checker that finds nothing reports success) applied to the wake path, which is
the worst possible place for it."
  (let* ((candidates
           (remove nil
                   (list (ignore-errors (pai-source-file *census-manifest-path*))
                         (and (boundp '*pai-root*)
                              (merge-pathnames
                               (format nil "src/mind/conscious/~a" *census-manifest-path*)
                               (symbol-value '*pai-root*)))
                         (and *census-load-directory*
                              (merge-pathnames *census-manifest-path*
                                               *census-load-directory*)))))
         (found (find-if #'probe-file candidates)))
    (or found
        (error "Census manifest ~s not found; tried ~{~a~^, ~}. The stimulus ~
                admission table is built from it and cannot be defaulted: an ~
                empty table would admit nothing and the runtime would never ~
                wake."
               *census-manifest-path* (or candidates '("no candidate paths"))))))

(defun %census-digest (string)
  "FNV-1a 64-bit, matching %PC-DIGEST. Pure Common Lisp: the digest detects
change, it does not resist forgery, and the manifest is local source."
  (let ((hash 14695981039346656037))
    (declare (type (unsigned-byte 64) hash))
    (loop for ch across string
          do (setf hash (ldb (byte 64 0)
                             (* (logxor hash (char-code ch)) 1099511628211))))
    (format nil "~(~16,'0x~)" hash)))

(defun %census-entry-get (entry key)
  (getf entry key))

(defun census-load (&optional (path (%census-manifest-file)))
  "Read the manifest and populate the admission policy.

Fails loudly on a malformed or missing manifest. An empty admission table
would make the runtime silently admit nothing -- a checker that finds nothing
reporting success (gotcha 25), applied to the wake path, which is the worst
place for it."
  (let* ((raw (with-open-file (s path :direction :input :external-format :utf-8)
                (let ((buf (make-string (file-length s))))
                  (subseq buf 0 (read-sequence buf s)))))
         (form (with-input-from-string (s raw)
                 (let ((*read-eval* nil))
                   (read s))))
         (entries (getf form :entries))
         (kinds (getf form :spec-kinds)))
    (unless entries (error "Census manifest ~a declares no entries." path))
    (setf *census-content-digest* (%census-digest raw)
          *census-implementation-digest*
          (let ((impl (ignore-errors (pai-source-file "census.lisp"))))
            (if (and impl (probe-file impl))
                (%census-digest
                 (with-open-file (in impl :direction :input :external-format :utf-8)
                   (let ((buf (make-string (file-length in))))
                     (subseq buf 0 (read-sequence buf in)))))
                "unresolved"))
          *census-version* (getf form :census-version)
          *census-entries* entries
          *census-spec-kinds* kinds)
    (clrhash *stimulus-kind-map*)
    (clrhash *stimulus-discriminators*)
    (dolist (entry entries)
      (let ((type (%census-entry-get entry :type))
            (class (%census-entry-get entry :class)))
        (unless (stringp type)
          (error "Census entry with no :type: ~s" entry))
        (unless (stringp (%census-entry-get entry :reason))
          (error "Census entry ~a has no :reason. Every classification, ~
                  including every exclusion, must state why." type))
        (when (eq class :stimulus)
          (let ((kind (%census-entry-get entry :kind)))
            (unless (member kind kinds :test #'string=)
              (error "Census entry ~a declares kind ~s, which is not in the ~
                      spec 9.1 vocabulary." type kind))
            (setf (gethash type *stimulus-kind-map*)
                  (list kind
                        (%census-entry-get entry :source)
                        (%census-entry-get entry :urgency)
                        (%census-entry-get entry :barrier)))
            (let ((d (%census-entry-get entry :discriminator)))
              (when d
                (setf (gethash type *stimulus-discriminators*)
                      (%census-discriminator d))))))))
    (when (zerop (hash-table-count *stimulus-kind-map*))
      (error "Census manifest ~a admitted no stimulus types." path))
    (hash-table-count *stimulus-kind-map*)))

(defun census-entries () *census-entries*)

(defun census-manifest ()
  (obj "census_version" *census-version*
       "content_digest" *census-content-digest*
       "implementation_digest" *census-implementation-digest*
       "entry_count" (length *census-entries*)
       "admitted_count" (hash-table-count *stimulus-kind-map*)
       "discriminated_count" (hash-table-count *stimulus-discriminators*)
       "spec_kinds" (coerce *census-spec-kinds* 'vector)))

(defun census-report ()
  "Every classification, admitted and excluded, with its reason. This is the
inspectable answer to 'what can wake this agent, and what deliberately
cannot' -- the second half being the part that is otherwise invisible."
  (obj "census_version" *census-version*
       "rows"
       (coerce
        (mapcar (lambda (e)
                  (obj "event_type" (getf e :type)
                       "class" (string-downcase (symbol-name (getf e :class)))
                       "kind" (or (getf e :kind) :null)
                       "urgency" (or (getf e :urgency) :null)
                       "barrier" (if (getf e :barrier) t nil)
                       "discriminator" (let ((d (getf e :discriminator)))
                                         (if d (string-downcase (symbol-name d)) :null))
                       "group" (or (getf e :group) :null)
                       "reason" (getf e :reason)))
                *census-entries*)
        'vector)))

;;; Loading defines: the admission table IS a definition, so it is built at
;;; load time. No database, network or state directory is touched.
(census-load)
