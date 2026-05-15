;;; firefox-to-emacs-native-messenger.el --- Emacs-Lisp bridge for Firefox/Tridactyl native messaging  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 User

;; Keywords: tools, processes, comm

;;; Commentary:

;; A pure Emacs-Lisp bridge implementing the Firefox WebExtensions native
;; messaging protocol as consumed by the Tridactyl browser extension.
;; The bridge accepts length-prefixed JSON frames over a Unix-domain socket
;; and dispatches the five in-scope handlers `version', `getconfigpath',
;; `temp', `read', and `run' with per-handler default-deny access control.
;;
;; See PROTOCOL.md for the wire-contract specification and per-handler
;; request/response shapes; see README.md for installation, operations,
;; rollback, and whitelist configuration.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'map)
(require 'json)
(require 'bindat)
(require 'xdg)
(require 'warnings)

(defgroup firefox-to-emacs-native-messenger nil
  "Pure Emacs-Lisp bridge for Firefox/Tridactyl native messaging.

See PROTOCOL.md for the wire-contract specification and per-handler
request/response shapes; see README.md for installation, operations,
rollback, and whitelist configuration."
  :group 'applications
  :prefix "firefox-to-emacs-native-messenger-")

(defcustom firefox-to-emacs-native-messenger-log-buffer-name
  "*firefox-to-emacs-native-messenger-log*"
  "Name of the buffer used by the bridge logger."
  :type 'string
  :group 'firefox-to-emacs-native-messenger)

(defcustom firefox-to-emacs-native-messenger-log-level
  'info
  "Minimum severity level of log records the bridge logger emits.
Records below this severity are silently dropped."
  :type '(choice (const :tag "Debug" debug)
                 (const :tag "Info" info)
                 (const :tag "Warn" warn)
                 (const :tag "Error" error))
  :group 'firefox-to-emacs-native-messenger)

(defcustom firefox-to-emacs-native-messenger-log-redact
  t
  "When non-nil, the logger redacts payload-content fields from log records."
  :type 'boolean
  :group 'firefox-to-emacs-native-messenger)

(defcustom firefox-to-emacs-native-messenger-inbound-frame-cap
  (* 10 1024 1024)
  "Maximum size, in bytes, of an inbound length-prefixed frame.
Frames declaring a length above this cap cause the connection to be
closed silently with a log entry; no error frame is sent."
  :type '(integer :min 1)
  :group 'firefox-to-emacs-native-messenger)

(defcustom firefox-to-emacs-native-messenger-outbound-response-cap
  (* 768 1024)
  "Maximum size, in bytes, of a serialized outbound JSON response payload.
A response that exceeds this cap is replaced exactly once with the
generic oversized-response error response."
  :type '(integer :min 1)
  :group 'firefox-to-emacs-native-messenger)

(defcustom firefox-to-emacs-native-messenger-run-output-cap
  (* 512 1024)
  "Maximum size, in bytes, of captured stdout+stderr of a `run' subprocess.
Exceeding this cap triggers process-group cancellation and produces the
overflow terminal cause."
  :type '(integer :min 1)
  :group 'firefox-to-emacs-native-messenger)

(defcustom firefox-to-emacs-native-messenger-run-timeout
  nil
  "Maximum wall-clock duration, in seconds, of a `run' subprocess, or nil.
When nil, the run handler imposes no timeout; otherwise expiration
triggers process-group cancellation and produces the timeout terminal cause."
  :type '(choice (const :tag "Unset" nil) (integer :min 1))
  :group 'firefox-to-emacs-native-messenger)

(defcustom firefox-to-emacs-native-messenger-read-timer
  30
  "Per-connection read timeout, in seconds.
A connection that has not received a complete length-prefixed frame
within this window is closed silently with a log entry."
  :type '(integer :min 1)
  :group 'firefox-to-emacs-native-messenger)

(defcustom firefox-to-emacs-native-messenger-post-response-cleanup-timer
  5
  "Post-response cleanup timeout, in seconds.
After a response has been written and EOF sent, the connection is
forcibly deleted after this many seconds if the peer has not closed."
  :type '(integer :min 1)
  :group 'firefox-to-emacs-native-messenger)

(defconst firefox-to-emacs-native-messenger-shell-binary
  "/bin/sh"
  "POSIX shell binary used to evaluate `run' command strings.")

(defconst firefox-to-emacs-native-messenger-shell-command-switch
  "-c"
  "Flag passed to the shell binary to introduce the command string.")

(defconst firefox-to-emacs-native-messenger-socat-half-close-timeout-seconds
  86400
  "Half-close timeout, in seconds, applied to socat by the wrapper.
Comfortably above the longest expected `run' command duration so that the
wrapper does not close the connection while a long-running run is still
producing output.")

(defconst firefox-to-emacs-native-messenger-tempfile-directory-base-prefix
  "/tmp/firefox-to-emacs-native-messenger-tempfiles-"
  "Base prefix of FILE-1600, the dedicated tempfile directory.
The daemon's numeric UID is appended at runtime to form the per-UID
tempfile directory path.")

(defconst firefox-to-emacs-native-messenger-tempfile-directory
  (format "%s%d/"
          firefox-to-emacs-native-messenger-tempfile-directory-base-prefix
          (user-uid))
  "Per-UID dedicated tempfile directory at FILE-1600.
Derived from `firefox-to-emacs-native-messenger-tempfile-directory-base-prefix'
and the daemon's numeric UID; verified to exist at mode 0700 on every
listener start.  Holds tempfiles created by the `temp' handler.")

(defconst firefox-to-emacs-native-messenger-cache-directory
  (expand-file-name "firefox-to-emacs-native-messenger/" (xdg-cache-home))
  "Runtime cache directory at FILE-0800.
Verified to exist at mode 0700 on every listener start.")

(defconst firefox-to-emacs-native-messenger-socket-path
  (expand-file-name "messenger.sock"
                    firefox-to-emacs-native-messenger-cache-directory)
  "Unix-domain socket path at FILE-0600, the listener's bound socket.")

(defconst firefox-to-emacs-native-messenger-pid-file-path
  (expand-file-name "firefox.pid"
                    firefox-to-emacs-native-messenger-cache-directory)
  "Side-channel PID file at FILE-0700, written by the wrapper.")

(defconst firefox-to-emacs-native-messenger-version
  "0.3.7"
  "Version string claimed by the bridge in `version' handler responses.

Pinned to the lowest upstream `native_main.nim' VERSION whose request
and response contract is fully implemented for the five v2 handlers in
scope, constrained to be at least the upstream
`requiredNativeMessengerVersion' that gates `Native.getrcpath' per
Tridactyl `src/lib/native.ts'.  See PROTOCOL.md Section 8 for the
audit reasoning behind the literal value.")

(defconst firefox-to-emacs-native-messenger-rcpath-candidates
  (list
   (expand-file-name "tridactyl/tridactylrc" (xdg-config-home))
   (expand-file-name "~/.config/tridactyl/tridactylrc")
   (expand-file-name "~/_config/tridactyl/tridactylrc")
   (expand-file-name "~/.tridactylrc")
   (expand-file-name "~/_tridactylrc"))
  "Hardcoded candidate list for the `getconfigpath' handler.
Mirrors upstream `native_main.nim's `findUserConfigFile' candidate set in
upstream-defined order; see PROTOCOL.md Section 17 for the audit citation.
Each candidate is passed through the path-expansion helper's TRAMP guard
before any I/O is attempted (per SEC-1300).")

(defcustom firefox-to-emacs-native-messenger-run-whitelist
  nil
  "Default-deny whitelist for the `run' handler.
A list of template strings; each entry contains zero or more
`<TEMP-PATH>' markers anywhere in the string.  The literal value
nil and the empty list both mean DENY ALL; the one-element list
\\='(\"*\") means ALLOW ALL.  See README.md \"Whitelist Configuration
Walkthrough\" for syntax and examples; see PROTOCOL.md Section 16
for the matcher's semantics."
  :type '(repeat string)
  :initialize #'custom-initialize-default
  :set #'firefox-to-emacs-native-messenger--whitelist-set
  :group 'firefox-to-emacs-native-messenger)

(defcustom firefox-to-emacs-native-messenger-read-whitelist
  nil
  "Default-deny whitelist for the `read' handler.
A list of literal absolute paths, glob paths, or the literal token
`<TEMP-PATH>'.  The literal value nil and the empty list both mean
DENY ALL; the one-element list \\='(\"*\") means ALLOW ALL.  See
README.md \"Whitelist Configuration Walkthrough\" for syntax and
examples; see PROTOCOL.md Section 16 for the matcher's semantics."
  :type '(repeat string)
  :initialize #'custom-initialize-default
  :set #'firefox-to-emacs-native-messenger--whitelist-set
  :group 'firefox-to-emacs-native-messenger)

(defcustom firefox-to-emacs-native-messenger-temp-registry-cap
  1024
  "Maximum number of entries the capability registry may hold.
The `temp' handler refuses to create a new tempfile when registration
would exceed this cap (after a prune sweep) and returns the generic
error response instead.  Bounds active-session DoS via repeated
`temp' invocations."
  :type '(integer :min 1)
  :initialize #'custom-initialize-default
  :set #'firefox-to-emacs-native-messenger--registry-cap-set
  :group 'firefox-to-emacs-native-messenger)

(defvar firefox-to-emacs-native-messenger--capability-registry
  (make-hash-table :test 'equal)
  "In-memory capability registry populated by the `temp' handler.
Maps absolute tempfile paths to a plist recording the file's identity
\(:dev DEV :inode INODE :uid UID) at registration time.  The registry
is cleared on listener start and stop; it is pruned on access when a
path no longer matches its stored identity.  Bounded by
`firefox-to-emacs-native-messenger-temp-registry-cap'.")

(defvar firefox-to-emacs-native-messenger--listener-process nil
  "The bridge's currently-active listener process, or nil when none is running.
Set by `firefox-to-emacs-native-messenger-start' after a successful bind;
cleared by `firefox-to-emacs-native-messenger-stop'.  A non-nil value
makes a subsequent `start' refuse with `bad-state' to enforce listener
idempotency (one listener per UID per daemon).")

(defvar firefox-to-emacs-native-messenger--connection-registry
  (make-hash-table :test 'eq)
  "Module-level registry of currently-active client connection processes.

A hash table keyed by the per-connection process object (test `eq').
Populated by the accept handler when a client connects (Phase 0500);
entries are removed by the per-connection sentinel or by listener stop.
The list of LIVE connections is available via
`firefox-to-emacs-native-messenger--connection-registry-list'; dead
processes that remain in the table are silently skipped by the lister.")

(defconst firefox-to-emacs-native-messenger--connection-key-read-buffer
  'read-buffer
  "Per-connection plist key for the unibyte read buffer.

The value at this key is a unibyte string holding bytes that have
arrived from the client but have not yet been parsed into a complete
length-prefixed JSON frame.  The per-connection filter appends incoming
chunks to this buffer, decodes the length prefix once four bytes have
arrived, and consumes the full frame once the declared length is
satisfied.  Per-connection plist keys are defined as defconsts to keep
plist accesses symbolic (not string-keyed) per Section 8.11.")

(defconst firefox-to-emacs-native-messenger--connection-key-declared-length
  'declared-length
  "Per-connection plist key for the declared frame length.

The value at this key is a non-negative integer that the filter has
decoded from the first four bytes of the read buffer, or nil when the
buffer has not yet accumulated four bytes.  Once decoded, the value is
used to determine whether the read buffer holds a complete frame.")

(defconst firefox-to-emacs-native-messenger--connection-key-read-timer
  'read-timer
  "Per-connection plist key for the read-timeout timer object.

The value at this key is a timer object scheduled to fire if the
filter has not produced a complete frame within the configured
`firefox-to-emacs-native-messenger-read-timer' window, or nil when no
timer is pending.  Canceled on complete-frame receipt and on connection
close.")

(defconst firefox-to-emacs-native-messenger--connection-key-state
  'state
  "Per-connection plist key for the connection state-machine field.

The value at this key is a symbol drawn from
`reading' (filter is accumulating the request frame), `dispatched' (a
deferred-response handler is in flight), `responded' (the response
writer has marked the connection responded), or `closing' (teardown in
progress).  The filter, dispatcher, response writer, and sentinel all
consult this field to make routing decisions per Section 8.22.")

(defconst firefox-to-emacs-native-messenger--connection-key-cleanup-timer
  'cleanup-timer
  "Per-connection plist key for the post-response cleanup timer.

The timer is scheduled by the response writer (PAT-0600 / Section 8.18
step 6) and cancelled by the per-connection sentinel on peer-close.
Bounded liveness: timer expiration triggers connection teardown per
Section 8.22.")

(defconst firefox-to-emacs-native-messenger--subprocess-key-run-state
  'run-state
  "Subprocess plist key for the Phase 0800 run-state hash table.

The value at this key is a hash table (test `eq') populated by the
`run' handler at subprocess launch and consulted by the run
accumulator filter, the run subprocess sentinel, the run-timeout
timer, the signal-escalation helper, and the connection sentinel's
run-cancellation extension.  Per-process plist keys are defined as
defconsts to keep plist accesses symbolic per Section 8.11 / 8.3.

The hash table's keys are documented in Section 8.3 of the plan:
`terminal-cause', `output-buffer', `output-bytes', `output-cap',
`subprocess', `connection', `command-string', `pgrp',
`pgrp-fallback', `timeout-timer'.")

(defconst firefox-to-emacs-native-messenger--connection-key-run-subprocess
  'run-subprocess
  "Per-connection plist key for the back-reference to the run subprocess.

The value at this key is the `process' object of the run subprocess
linked to this connection, or nil when no run is in flight.  Set by
the `run' handler when transitioning the connection state to
`dispatched'; consulted by the connection sentinel's run-cancellation
extension to find the subprocess that needs signal-escalation on
peer-close per PAT-0800.")

(define-error 'firefox-to-emacs-native-messenger-error
  "firefox-to-emacs-native-messenger bridge error")

(define-error 'firefox-to-emacs-native-messenger-bad-request
  "Malformed request"
  'firefox-to-emacs-native-messenger-error)

(define-error 'firefox-to-emacs-native-messenger-frame-too-large
  "Inbound frame exceeded the configured size cap"
  'firefox-to-emacs-native-messenger-error)

(define-error 'firefox-to-emacs-native-messenger-frame-parse-error
  "Inbound frame is not well-formed UTF-8 JSON"
  'firefox-to-emacs-native-messenger-error)

(define-error 'firefox-to-emacs-native-messenger-unsupported-command
  "Unhandled message"
  'firefox-to-emacs-native-messenger-error)

(define-error 'firefox-to-emacs-native-messenger-handler-error
  "Handler raised an error"
  'firefox-to-emacs-native-messenger-error)

(define-error 'firefox-to-emacs-native-messenger-bad-state
  "Bridge is in a state that prevents the requested operation"
  'firefox-to-emacs-native-messenger-error)

(define-error 'firefox-to-emacs-native-messenger-whitelist-rejection
  "Argument is not allowed by the per-handler whitelist"
  'firefox-to-emacs-native-messenger-error)

(define-error 'firefox-to-emacs-native-messenger-whitelist-malformed
  "Whitelist value violates its per-handler validation rules"
  'firefox-to-emacs-native-messenger-error)

(defconst firefox-to-emacs-native-messenger--length-prefix-spec
  '((len u32r))
  "Bindat spec for the 4-byte little-endian length prefix per CON-0800.")

(defun firefox-to-emacs-native-messenger--pack-length (n)
  "Pack non-negative integer N as a 4-byte little-endian unibyte string."
  (bindat-pack firefox-to-emacs-native-messenger--length-prefix-spec
               `((len . ,n))))

(defun firefox-to-emacs-native-messenger--unpack-length (bytes)
  "Unpack a 4-byte little-endian unibyte string BYTES to an integer."
  (bindat-get-field
   (bindat-unpack firefox-to-emacs-native-messenger--length-prefix-spec
                  bytes)
   'len))

(defconst firefox-to-emacs-native-messenger--log-level-ranks
  '((debug . 0) (info . 1) (warn . 2) (error . 3))
  "Ordering of severity levels used to filter log records.")

(defun firefox-to-emacs-native-messenger--log-level-rank (level)
  "Return the numeric rank of LEVEL, or nil if LEVEL is not a known severity."
  (cdr (assq level firefox-to-emacs-native-messenger--log-level-ranks)))

(defun firefox-to-emacs-native-messenger--log (level format-string &rest args)
  "Write a log record at LEVEL to the bridge's log buffer.
LEVEL is one of `debug', `info', `warn', `error'.  FORMAT-STRING and ARGS
are forwarded to `format'.  Records whose LEVEL is below the configured
`firefox-to-emacs-native-messenger-log-level' threshold are silently
dropped.  Any error raised during formatting or buffer manipulation is
swallowed; the logger never propagates errors per GUD-400."
  (condition-case _err
      (let ((record-rank
             (firefox-to-emacs-native-messenger--log-level-rank level))
            (threshold-rank
             (firefox-to-emacs-native-messenger--log-level-rank
              firefox-to-emacs-native-messenger-log-level)))
        (when (and record-rank threshold-rank
                   (>= record-rank threshold-rank))
          (let ((message-text (apply #'format format-string args))
                (buffer (get-buffer-create
                         firefox-to-emacs-native-messenger-log-buffer-name)))
            (with-current-buffer buffer
              (goto-char (point-max))
              (insert (format-time-string "%FT%T.%3N%z")
                      " ["
                      (symbol-name level)
                      "] "
                      message-text
                      "\n")))))
    (error nil)))

(defun firefox-to-emacs-native-messenger--glob-to-regexp (pattern)
  "Translate fnmatch-style PATTERN to an anchored Emacs regexp.
`**' matches any sequence (including `/').  `*' matches a sequence of
non-slash characters.  `?' matches one non-slash character.  All other
characters match literally.  The returned regexp is full-string anchored
with \\` and \\'."
  (let ((i 0)
        (n (length pattern))
        (parts '()))
    (while (< i n)
      (let ((c (aref pattern i)))
        (cond
         ((and (eq c ?*)
               (< (1+ i) n)
               (eq (aref pattern (1+ i)) ?*))
          (push ".*" parts)
          (setq i (+ i 2)))
         ((eq c ?*)
          (push "[^/]*" parts)
          (setq i (1+ i)))
         ((eq c ?\?)
          (push "[^/]" parts)
          (setq i (1+ i)))
         (t
          (push (regexp-quote (char-to-string c)) parts)
          (setq i (1+ i))))))
    (concat "\\`" (apply #'concat (nreverse parts)) "\\'")))

(defun firefox-to-emacs-native-messenger--glob-match-p (pattern candidate)
  "Return non-nil if CANDIDATE matches the fnmatch-style PATTERN per REQ-2900.
Matches are full-string anchored and case-sensitive.  See
`firefox-to-emacs-native-messenger--glob-to-regexp' for the supported
glob alphabet."
  (let ((case-fold-search nil))
    (and (string-match-p
          (firefox-to-emacs-native-messenger--glob-to-regexp pattern)
          candidate)
         t)))

(defun firefox-to-emacs-native-messenger--validate-read-entry (entry)
  "Signal `whitelist-malformed' unless ENTRY is a valid `read' whitelist entry.
A valid entry is the literal token \"<TEMP-PATH>\" or an absolute path
\(starts with `/'); glob characters within an absolute path are permitted."
  (unless (or (equal entry "<TEMP-PATH>")
              (and (> (length entry) 0) (eq (aref entry 0) ?/)))
    (signal 'firefox-to-emacs-native-messenger-whitelist-malformed
            (list (concat "read whitelist entry must be an absolute path "
                          "or the literal \"<TEMP-PATH>\" token")
                  entry))))

(defun firefox-to-emacs-native-messenger--validate-run-entry (entry)
  "Signal `whitelist-malformed' unless ENTRY is a valid `run' whitelist entry.
Run entries are template strings; this validator enforces two rules from
Section 8.10:

  1. Typo guard: every \"<...>\" token in ENTRY must be exactly
     \"<TEMP-PATH>\" (case-sensitive).
  2. Adjacent-marker rejection: no two \"<TEMP-PATH>\" markers may appear
     with no literal text between them (empty interior literal segment)."
  (save-match-data
    (let ((start 0))
      (while (string-match "<\\([^<>]*\\)>" entry start)
        (let ((inside (match-string 1 entry)))
          (unless (equal inside "TEMP-PATH")
            (signal 'firefox-to-emacs-native-messenger-whitelist-malformed
                    (list (concat "run whitelist entry contains a "
                                  "non-<TEMP-PATH> placeholder")
                          (concat "<" inside ">") entry))))
        (setq start (match-end 0)))))
  (when (string-match-p "<TEMP-PATH><TEMP-PATH>" entry)
    (signal 'firefox-to-emacs-native-messenger-whitelist-malformed
            (list (concat "run whitelist entry has two adjacent "
                          "<TEMP-PATH> markers (empty interior literal segment)")
                  entry))))

(defun firefox-to-emacs-native-messenger--validate-whitelist-entry (entry kind)
  "Validate one whitelist ENTRY for handler KIND (`run' or `read').
Signals `firefox-to-emacs-native-messenger-whitelist-malformed' on failure."
  (unless (stringp entry)
    (signal 'firefox-to-emacs-native-messenger-whitelist-malformed
            (list "whitelist entry must be a string" entry)))
  (when (string-empty-p entry)
    (signal 'firefox-to-emacs-native-messenger-whitelist-malformed
            (list "whitelist entry must be non-empty")))
  (when (string-match-p "[\n\0]" entry)
    (signal 'firefox-to-emacs-native-messenger-whitelist-malformed
            (list "whitelist entry must not contain newline or null bytes"
                  entry)))
  (pcase kind
    ('read (firefox-to-emacs-native-messenger--validate-read-entry entry))
    ('run  (firefox-to-emacs-native-messenger--validate-run-entry entry))
    (_     (signal 'firefox-to-emacs-native-messenger-whitelist-malformed
                   (list "unknown handler kind" kind)))))

(defun firefox-to-emacs-native-messenger--validate-whitelist (value kind)
  "Validate VALUE as a whitelist for handler KIND (`run' or `read').

Returns t when VALUE is acceptable, otherwise signals
`firefox-to-emacs-native-messenger-whitelist-malformed' with a message
that names the violation.  The accepted shapes are:

  - nil or the empty list: deny-all (REQ-3700)
  - (\"*\"): the allow-all sentinel; mixing \"*\" with any other element
    is rejected (REQ-3900)
  - a proper list of valid entries: each entry is validated by
    `firefox-to-emacs-native-messenger--validate-whitelist-entry'

This is the single shared validator invoked at all three validation sites
of PAT-1100: the defcustom `:set' slot, the listener-start sweep, and
per-gate-check."
  (cond
   ((null value) t)
   ((not (and (listp value) (null (cdr (last value)))))
    (signal 'firefox-to-emacs-native-messenger-whitelist-malformed
            (list "whitelist value must be a proper list" value)))
   ((member "*" value)
    (if (equal value '("*"))
        t
      (signal 'firefox-to-emacs-native-messenger-whitelist-malformed
              (list (concat "the allow-all entry \"*\" must appear alone "
                            "in the whitelist")
                    value))))
   (t
    (dolist (entry value)
      (firefox-to-emacs-native-messenger--validate-whitelist-entry entry kind))
    t)))
(defvar firefox-to-emacs-native-messenger--whitelist-kinds)

(defun firefox-to-emacs-native-messenger--whitelist-set (symbol newval)
  "Defcustom :set slot for the bridge's whitelist defcustoms.
Validate NEWVAL via the shared validator before storing it under SYMBOL.
On validation failure the validator signals
`firefox-to-emacs-native-messenger-whitelist-malformed' and `set-default'
is NOT called, so the variable retains its previous value."
  (let ((kind (cdr (assq symbol
                         firefox-to-emacs-native-messenger--whitelist-kinds))))
    (unless kind
      (signal 'firefox-to-emacs-native-messenger-whitelist-malformed
              (list "no handler kind for whitelist symbol" symbol)))
    (firefox-to-emacs-native-messenger--validate-whitelist newval kind))
  (set-default symbol newval))

(defun firefox-to-emacs-native-messenger--registry-cap-set (symbol newval)
  "Defcustom :set slot for `temp-registry-cap'.
Reject non-positive or non-integer NEWVAL with an explicit error so the
variable's previous value is retained."
  (unless (and (integerp newval) (> newval 0))
    (signal 'firefox-to-emacs-native-messenger-bad-request
            (list "temp-registry-cap must be a positive integer" newval)))
  (set-default symbol newval))


(defconst firefox-to-emacs-native-messenger--whitelist-kinds
  '((firefox-to-emacs-native-messenger-run-whitelist  . run)
    (firefox-to-emacs-native-messenger-read-whitelist . read))
  "Mapping from each whitelist defcustom symbol to its handler kind.")

(defun firefox-to-emacs-native-messenger--whitelist-watcher
    (symbol newval operation _where)
  "Validate NEWVAL on assignment to a whitelist defcustom SYMBOL.
This watcher runs on every binding-form OPERATION that supplies a value
\(`set', `set-default'); `let' bindings are ignored.  On validation
failure, signal `firefox-to-emacs-native-messenger-whitelist-malformed'
to abort the assignment so the variable retains its pre-set value."
  (when (memq operation '(set set-default))
    (let ((kind (cdr (assq symbol
                           firefox-to-emacs-native-messenger--whitelist-kinds))))
      (when kind
        (firefox-to-emacs-native-messenger--validate-whitelist newval kind)))))

(dolist (entry firefox-to-emacs-native-messenger--whitelist-kinds)
  (add-variable-watcher
   (car entry)
   #'firefox-to-emacs-native-messenger--whitelist-watcher))

(defun firefox-to-emacs-native-messenger--registry-identity-plist (attrs)
  "Build the identity plist `(:dev :inode :uid)' from `file-attributes' ATTRS."
  (list :dev (file-attribute-device-number attrs)
        :inode (file-attribute-inode-number attrs)
        :uid (file-attribute-user-id attrs)))

(defun firefox-to-emacs-native-messenger--registry-register (path)
  "Record PATH in the capability registry with its current identity.
Identity is `(:dev :inode :uid)' read from `file-attributes' at registration
time.  PATH MUST refer to an existing regular file; otherwise the function
signals `firefox-to-emacs-native-messenger-bad-state'."
  (let ((attrs (file-attributes path)))
    (unless attrs
      (signal 'firefox-to-emacs-native-messenger-bad-state
              (list "cannot register: file not found" path)))
    (when (file-attribute-type attrs)
      (signal 'firefox-to-emacs-native-messenger-bad-state
              (list "cannot register: not a regular file" path)))
    (puthash path
             (firefox-to-emacs-native-messenger--registry-identity-plist attrs)
             firefox-to-emacs-native-messenger--capability-registry)))

(defun firefox-to-emacs-native-messenger--registry-contains-p (path)
  "Return non-nil if PATH is registered and its current identity matches.
On mismatch (file missing, dev/inode/uid changed, no longer a regular
file) the entry is pruned and the function returns nil."
  (let ((stored (gethash path
                         firefox-to-emacs-native-messenger--capability-registry)))
    (and stored
         (let ((attrs (file-attributes path)))
           (cond
            ((null attrs)
             (remhash path
                      firefox-to-emacs-native-messenger--capability-registry)
             nil)
            ((file-attribute-type attrs)
             (remhash path
                      firefox-to-emacs-native-messenger--capability-registry)
             nil)
            ((not (equal stored
                         (firefox-to-emacs-native-messenger--registry-identity-plist
                          attrs)))
             (remhash path
                      firefox-to-emacs-native-messenger--capability-registry)
             nil)
            (t t))))))

(defun firefox-to-emacs-native-messenger--registry-clear-all ()
  "Empty the capability registry."
  (clrhash firefox-to-emacs-native-messenger--capability-registry))

(defun firefox-to-emacs-native-messenger--registry-prune-all ()
  "Run `--registry-contains-p' on every entry, pruning all stale ones."
  (let (keys)
    (maphash (lambda (k _v) (push k keys))
             firefox-to-emacs-native-messenger--capability-registry)
    (dolist (k keys)
      (firefox-to-emacs-native-messenger--registry-contains-p k))))

(defun firefox-to-emacs-native-messenger--registry-clear-on-start ()
  "Empty the capability registry as part of the listener-start sequence.

Per Section 8.8 / REQ-3000, the capability registry survives only within
a single listener lifetime; on listener start the registry is cleared
AFTER the FILE-1600 tempfile sweep (the sweep removes the on-disk files
that the prior lifetime's registry referred to, so clearing the in-memory
state afterward leaves the bridge in a consistent empty-but-bounded
state).  The ordering against the FILE-1600 sweep is enforced by the
listener-start composition; this function is the unconditional clear
hook.  Idempotent: calling repeatedly leaves the registry empty."
  (clrhash firefox-to-emacs-native-messenger--capability-registry))

(defun firefox-to-emacs-native-messenger--registry-clear-on-stop ()
  "Empty the capability registry as part of the listener-stop sequence.

Per Section 8.8 / REQ-3000 / SEC-1200, listener stop invalidates every
prior registration; subsequent listener starts re-establish an empty
registry.  Idempotent: calling repeatedly leaves the registry empty."
  (clrhash firefox-to-emacs-native-messenger--capability-registry))

(defun firefox-to-emacs-native-messenger--connection-registry-add (proc)
  "Add connection process PROC to the connection registry.
PROC's value cell is t in the registry; subsequent reads use the key."
  (puthash proc t firefox-to-emacs-native-messenger--connection-registry))

(defun firefox-to-emacs-native-messenger--connection-registry-remove (proc)
  "Remove connection process PROC from the connection registry.
Silent no-op when PROC is not registered."
  (remhash proc firefox-to-emacs-native-messenger--connection-registry))

(defun firefox-to-emacs-native-messenger--connection-registry-clear ()
  "Empty the connection registry."
  (clrhash firefox-to-emacs-native-messenger--connection-registry))

(defun firefox-to-emacs-native-messenger--connection-registry-list ()
  "Return the list of live connection processes registered in the registry.
Dead processes that linger in the table are silently filtered out."
  (let (live)
    (maphash
     (lambda (proc _v)
       (when (process-live-p proc) (push proc live)))
     firefox-to-emacs-native-messenger--connection-registry)
    live))

(defun firefox-to-emacs-native-messenger--listener-start-whitelist-sweep ()
  "Validate every whitelist defcustom before allowing listener start.

Implements site 2 of PAT-1100's three-site validation tower (REQ-4100):
iterates over each entry in
`firefox-to-emacs-native-messenger--whitelist-kinds' and invokes the
shared validator
\(`firefox-to-emacs-native-messenger--validate-whitelist') on the
current value of that defcustom.  On any malformed value the validator
signals `firefox-to-emacs-native-messenger-whitelist-malformed' from
this function, refusing listener start before any socket is bound.

Returns t on success."
  (dolist (entry firefox-to-emacs-native-messenger--whitelist-kinds)
    (firefox-to-emacs-native-messenger--validate-whitelist
     (symbol-value (car entry))
     (cdr entry)))
  t)

(defun firefox-to-emacs-native-messenger--cleanup-on-shutdown ()
  "Run on `kill-emacs-hook' to sweep stale tempfiles before Emacs exits.

Invokes `firefox-to-emacs-native-messenger--sweep-tempfiles' against
FILE-1600 so that `tmp_*.txt' entries created by the `temp' handler do
not outlive the daemon.  Wrapped in `condition-case' so errors here
never block Emacs shutdown (per GUD-400 logger discipline applied to
the shutdown path)."
  (condition-case err
      (firefox-to-emacs-native-messenger--sweep-tempfiles)
    (error
     (firefox-to-emacs-native-messenger--log
      'warn "cleanup-on-shutdown raised: %S" err))))

(defun firefox-to-emacs-native-messenger--verify-cache-directory ()
  "Ensure the bridge cache directory exists at mode 0700 and is a real directory.

Behavior per SEC-0500 and the Phase 0400 design:

  - If `firefox-to-emacs-native-messenger-cache-directory' does not exist,
    it is created (with all needed parents) under umask 077, then its mode
    is explicitly set to 0700 to neutralize any inherited umask.
  - If the path exists and `file-symlink-p' returns non-nil for its
    canonical name, the function signals
    `firefox-to-emacs-native-messenger-bad-state' (a symlink at this
    location could redirect socket and PID-file writes to an
    attacker-controlled target, even within the same UID).
  - If the path exists as a real directory whose mode is exactly 0700,
    the function returns normally.
  - In every other case (regular file, wrong mode, ...) the function
    signals `firefox-to-emacs-native-messenger-bad-state'."
  (let* ((dir firefox-to-emacs-native-messenger-cache-directory)
         (without-slash (directory-file-name dir)))
    (cond
     ((not (file-exists-p without-slash))
      (with-file-modes #o700
        (make-directory dir t))
      (set-file-modes without-slash #o700))
     ((file-symlink-p without-slash)
      (signal 'firefox-to-emacs-native-messenger-bad-state
              (list "cache directory path is a symlink" dir)))
     ((not (file-directory-p without-slash))
      (signal 'firefox-to-emacs-native-messenger-bad-state
              (list "cache directory path is not a directory" dir)))
     ((/= (logand (file-modes without-slash) #o7777) #o700)
      (signal 'firefox-to-emacs-native-messenger-bad-state
              (list "cache directory mode is not 0700" dir
                    (format "%o" (logand (file-modes without-slash) #o7777))))))))
(defun firefox-to-emacs-native-messenger--verify-tempfile-directory ()
  "Ensure the bridge tempfile directory exists at mode 0700 as a real directory.

The tempfile directory at FILE-1600
\(`firefox-to-emacs-native-messenger-tempfile-directory') hosts every file
created by the `temp' handler.  Behavior per SEC-1100:

  - If the directory does not exist, it is created (with all needed
    parents) under umask 077, then its mode is explicitly set to 0700
    to neutralize any inherited umask.
  - If the path exists and `file-symlink-p' returns non-nil for its
    canonical name, the function signals
    `firefox-to-emacs-native-messenger-bad-state' (a symlink at this
    location could redirect tempfile writes to an attacker-controlled
    target, even within the same UID).
  - If the path exists as a regular file rather than a directory, the
    function signals `firefox-to-emacs-native-messenger-bad-state'.
  - If the path exists as a real directory whose mode differs from
    0700, the function signals
    `firefox-to-emacs-native-messenger-bad-state'.
  - If the path exists as a real 0700 directory NOT owned by the
    daemon's effective UID, the function signals
    `firefox-to-emacs-native-messenger-bad-state'."
  (let* ((dir firefox-to-emacs-native-messenger-tempfile-directory)
         (without-slash (directory-file-name dir)))
    (cond
     ((not (file-exists-p without-slash))
      (with-file-modes #o700
        (make-directory dir t))
      (set-file-modes without-slash #o700))
     ((file-symlink-p without-slash)
      (signal 'firefox-to-emacs-native-messenger-bad-state
              (list "tempfile directory path is a symlink" dir)))
     ((not (file-directory-p without-slash))
      (signal 'firefox-to-emacs-native-messenger-bad-state
              (list "tempfile directory path is not a directory" dir)))
     ((/= (logand (file-modes without-slash) #o7777) #o700)
      (signal 'firefox-to-emacs-native-messenger-bad-state
              (list "tempfile directory mode is not 0700" dir
                    (format "%o" (logand (file-modes without-slash) #o7777)))))
     ((/= (file-attribute-user-id (file-attributes without-slash))
          (user-uid))
      (signal 'firefox-to-emacs-native-messenger-bad-state
              (list "tempfile directory is not owned by daemon UID" dir
                    (file-attribute-user-id (file-attributes without-slash))
                    (user-uid)))))))

(defun firefox-to-emacs-native-messenger--path-is-real-socket-p (path)
  "Return non-nil if PATH names a real (non-symlink) UNIX socket file.

Uses `file-attributes' to read both the type bit of the mode-string and
the symlink target; rejects symlinks even when their target is a socket."
  (let ((attrs (file-attributes path)))
    (and attrs
         (null (file-attribute-type attrs))
         (let ((modes (file-attribute-modes attrs)))
           (and (stringp modes)
                (> (length modes) 0)
                (eq (aref modes 0) ?s))))))

(defun firefox-to-emacs-native-messenger--socket-connectable-p (path)
  "Return non-nil if a client connection to UNIX socket PATH succeeds.

Opens a transient AF_UNIX client to PATH and immediately tears it down;
returns t iff the connect completed.  Used to distinguish a live listener
from a stale socket file before unlinking."
  (let ((proc (condition-case _err
                  (make-network-process
                   :name "fenm-probe-client"
                   :family 'local
                   :service path
                   :coding '(binary . binary)
                   :filter-multibyte nil
                   :noquery t)
                (error nil))))
    (when proc
      (unwind-protect
          t
        (when (process-live-p proc) (delete-process proc))))))

(defun firefox-to-emacs-native-messenger--probe-and-delete-stale-socket
    (socket-path)
  "Per PAT-0100 / Section 8.21: clear any stale socket file at SOCKET-PATH.

Behavior:
  - If SOCKET-PATH does not exist, returns nil (caller proceeds to bind).
  - If SOCKET-PATH is a symlink, signals
    `firefox-to-emacs-native-messenger-bad-state' (a symlink could
    redirect the subsequent bind to an attacker-controlled location).
  - If a client connection to SOCKET-PATH succeeds, a live listener is
    still bound; signals `firefox-to-emacs-native-messenger-bad-state'.
  - If the connect fails and SOCKET-PATH is NOT a real socket file
    (e.g., regular file, directory, FIFO), signals
    `firefox-to-emacs-native-messenger-bad-state' (the bridge refuses
    to clobber an unexpected file at the configured socket path).
  - Otherwise (connect failed AND SOCKET-PATH is a real socket file
    with no live listener), unlinks SOCKET-PATH via `delete-file' so
    the caller's subsequent bind can succeed."
  (when (file-exists-p socket-path)
    (cond
     ((file-symlink-p socket-path)
      (signal 'firefox-to-emacs-native-messenger-bad-state
              (list "socket path is a symlink" socket-path)))
     ((firefox-to-emacs-native-messenger--socket-connectable-p socket-path)
      (signal 'firefox-to-emacs-native-messenger-bad-state
              (list "live listener already bound to socket path"
                    socket-path)))
     ((not (firefox-to-emacs-native-messenger--path-is-real-socket-p
            socket-path))
      (signal 'firefox-to-emacs-native-messenger-bad-state
              (list "non-socket file at configured socket path"
                    socket-path)))
     (t
      (delete-file socket-path)))))

(defconst firefox-to-emacs-native-messenger--tempfile-glob "tmp_*.txt"
  "Filename glob matched by `--sweep-tempfiles' inside FILE-1600.
Mirrors the `make-temp-file' shape produced by the `temp' handler:
prefix `tmp_<sanitized>_<emacs-uniqueness>' plus the literal `.txt'
suffix.  Entries in FILE-1600 not matching this glob are preserved
by the sweep.")

(defun firefox-to-emacs-native-messenger--sweep-tempfiles ()
  "Delete every `tmp_*.txt' entry in the bridge's tempfile directory.

Operates on `firefox-to-emacs-native-messenger-tempfile-directory'
\(FILE-1600).  Only files matching the
`firefox-to-emacs-native-messenger--tempfile-glob' filename pattern at
the top level are removed; non-matching entries (subdirectories,
unrelated files, dotfiles) are preserved.  A count summary is written
to the bridge log at info level naming the number of files deleted and
the number preserved.  Returns the count of files deleted."
  (let ((dir firefox-to-emacs-native-messenger-tempfile-directory)
        (deleted 0)
        (preserved 0))
    (when (file-directory-p dir)
      (dolist (entry (directory-files dir t directory-files-no-dot-files-regexp t))
        (let ((basename (file-name-nondirectory entry)))
          (cond
           ((and (file-regular-p entry)
                 (not (file-symlink-p entry))
                 (let ((case-fold-search nil))
                   (string-match-p
                    (concat
                     "\\`"
                     (wildcard-to-regexp
                      firefox-to-emacs-native-messenger--tempfile-glob)
                     "\\'")
                    basename)))
            (condition-case err
                (progn (delete-file entry) (cl-incf deleted))
              (error
               (cl-incf preserved)
               (firefox-to-emacs-native-messenger--log
                'warn "sweep: failed to delete %s: %S" entry err))))
           (t (cl-incf preserved))))))
    (firefox-to-emacs-native-messenger--log
     'info "sweep: deleted %d files, preserved %d in %s"
     deleted preserved dir)
    deleted))

(defun firefox-to-emacs-native-messenger--command-gate-match-p (entry candidate)
  "Return non-nil if CANDIDATE matches the run-whitelist template ENTRY.

Implements the deterministic match algorithm in PROTOCOL.md Section 16 /
plan Section 8.10:

  1. Split ENTRY on the literal token \"<TEMP-PATH>\" into N+1 literal
     segments L_0..L_N and N marker positions.
  2. If N == 0, accept iff CANDIDATE equals ENTRY byte-for-byte.
  3. Otherwise: require CANDIDATE to start with L_0; for each interior
     marker, find the FIRST occurrence of the next literal at or after
     the cursor and treat the intervening bytes as the marker's extracted
     value; require the trailing segment L_N to be the candidate's exact
     suffix; reject if any extracted marker substring is empty.
  4. Consult the capability registry's `--registry-contains-p' predicate
     on every extracted marker substring; reject on any miss.
  5. Accept only if CANDIDATE is fully consumed by the match.

ENTRY is expected to have already passed the whitelist validator, which
guarantees no two adjacent markers (no empty interior literal segment)."
  (let* ((parts (split-string entry "<TEMP-PATH>"))
         (n-markers (1- (length parts)))
         (l0 (car parts))
         (interior-and-tail (cdr parts)))
    (cond
     ((zerop n-markers)
      (equal entry candidate))
     ((not (string-prefix-p l0 candidate))
      nil)
     (t
      (let ((cursor (length l0))
            (markers '())
            (ok t))
        (let* ((interiors (butlast interior-and-tail))
               (tail (car (last interior-and-tail))))
          (dolist (interior interiors)
            (when ok
              (let ((pos (string-search interior candidate cursor)))
                (cond
                 ((null pos) (setq ok nil))
                 (t
                  (push (substring candidate cursor pos) markers)
                  (setq cursor (+ pos (length interior))))))))
          (when ok
            (let* ((cand-len (length candidate))
                   (tail-len (length tail))
                   (tail-pos (- cand-len tail-len)))
              (cond
               ((< tail-pos cursor) (setq ok nil))
               ((not (equal tail (substring candidate tail-pos)))
                (setq ok nil))
               (t
                (push (substring candidate cursor tail-pos) markers))))))
        (when ok
          (setq markers (nreverse markers))
          (cl-block result
            (dolist (m markers)
              (when (string-empty-p m) (cl-return-from result nil))
              (unless (firefox-to-emacs-native-messenger--registry-contains-p m)
                (cl-return-from result nil)))
            t)))))))

;;;; Per-connection lifecycle: accept handler, filter and sentinel
;;;; stubs, and read-timeout timer scaffolding.

(defun firefox-to-emacs-native-messenger--read-timer-expire (client)
  "Read-timer expiration handler for CLIENT.

Invoked when the per-connection read-timeout timer fires before a
complete request frame has arrived.  The handler logs the event at
warn level, removes CLIENT from the connection registry, and deletes
the underlying process.  Removing from the registry is idempotent
with the sentinel's removal path: the per-connection sentinel also
removes on close, so cleanup-path ordering between the two is
tolerated.

Returns nil unconditionally."
  (firefox-to-emacs-native-messenger--log
   'warn "read timer expired; closing connection %S" client)
  (firefox-to-emacs-native-messenger--connection-registry-remove client)
  (when (process-live-p client)
    (delete-process client))
  nil)

(defun firefox-to-emacs-native-messenger--cancel-read-timer (client)
  "Cancel the per-connection read-timeout timer for CLIENT.

Reads the timer object stored on the client's plist under
firefox-to-emacs-native-messenger--connection-key-read-timer; if
the value is non-nil, calls cancel-timer to remove it from the
active timer list.  Clears the plist key to nil unconditionally
so that a subsequent cancel call observes the same nil state.

Safe to call when no timer is scheduled: a nil timer is treated
as a silent no-op, supporting the double-cancel contract.

Returns nil unconditionally."
  (let ((timer
         (process-get
          client
          firefox-to-emacs-native-messenger--connection-key-read-timer)))
    (when timer
      (cancel-timer timer))
    (process-put
     client
     firefox-to-emacs-native-messenger--connection-key-read-timer
     nil))
  nil)

(defun firefox-to-emacs-native-messenger--start-read-timer (client)
  "Schedule the per-connection read-timeout timer for CLIENT.

The timer is configured to fire after
`firefox-to-emacs-native-messenger-read-timer' seconds, at which
point `firefox-to-emacs-native-messenger--read-timer-expire' is
invoked on CLIENT.  The freshly-created timer is stored on the
client's plist under
`firefox-to-emacs-native-messenger--connection-key-read-timer'.
Returns the timer object."
  (let ((timer
         (run-at-time
          firefox-to-emacs-native-messenger-read-timer
          nil
          #'firefox-to-emacs-native-messenger--read-timer-expire
          client)))
    (process-put
     client
     firefox-to-emacs-native-messenger--connection-key-read-timer
     timer)
    timer))

(defun firefox-to-emacs-native-messenger--close-connection (client)
  "Silently close CLIENT after a protocol violation or timeout.

Cancels the per-connection read-timeout timer, removes the
connection from the connection registry, and deletes the
underlying process.  The bridge MUST NOT emit any response on
these paths per SEC-0900; the connection is torn down without
acknowledgement.  Returns nil unconditionally."
  (firefox-to-emacs-native-messenger--cancel-read-timer client)
  (firefox-to-emacs-native-messenger--connection-registry-remove client)
  (when (process-live-p client)
    (delete-process client))
  nil)

(defun firefox-to-emacs-native-messenger--filter-handle-parse-error
    (client error-data)
  "Handle a parse failure on CLIENT with diagnostic ERROR-DATA.

Build a generic error response per PAT-0300 (cmd field set to
the string \"error\"; error field carrying the parse-error
message; no code field) and route it through the response writer
helper.  Reset the per-connection plist state so that the
buffer and declared-length match the post-frame quiescent
shape (read-buffer empty unibyte string, declared-length nil).
The dispatcher is deliberately not invoked on this path.

Returns nil unconditionally."
  (let ((response
        (list (cons 'cmd "error")
          (cons 'error (error-message-string error-data)))))
    (process-put
      client
      firefox-to-emacs-native-messenger--connection-key-read-buffer
      (unibyte-string))
    (process-put
      client
      firefox-to-emacs-native-messenger--connection-key-declared-length
      nil)
    (firefox-to-emacs-native-messenger--write-response client response)
    nil))

(defvar firefox-to-emacs-native-messenger--handlers
  (make-hash-table :test 'equal)
  "Handler registry mapping cmd strings to handler functions.

The dispatcher consults this registry on each request; on a miss
the unhandled-message generic error response is returned.
Handlers are registered by the per-cmd implementation phases
(Phase 0700 for the synchronous cmds, Phase 0800 for run).")

(defun firefox-to-emacs-native-messenger--build-error-response (message)
  "Build the generic error response per PAT-0300 with MESSAGE.

Returns an alist with the cmd field set to the literal string
\"error\" and the error field set to MESSAGE.  No other fields
are present; the response is shape-compatible with the response
builder that lands in a subsequent phase."
  (list (cons 'cmd "error") (cons 'error message)))

(defun firefox-to-emacs-native-messenger--build-response (response-data)
  "Apply structural null-stripping to RESPONSE-DATA.

RESPONSE-DATA is an alist of (FIELD-SYMBOL . VALUE) cons cells
representing a handler's response.  Per PROTOCOL.md S6
(\"Null-Stripping Rules (Structural, Not Semantic)\"), every
field whose value is the keyword `:absent' MUST be omitted from
the output; every other value, including the empty string \"\",
the integer 0, and the keyword `:false', MUST be preserved
verbatim.

The keyword `:absent' is the in-process marker the handler uses
to communicate \"this field is structurally absent\" without
conflating absence with semantic emptiness.  This distinction is
mandatory for back-compat with Tridactyl's editor flow, which
relies on `read' open-failure carrying an empty-string `content'
rather than an absent `content'.

Callers MUST use `:absent' (not nil) for structural absence;
nil at the value position is not stripped, and is reserved for
representing other values per `json-serialize' semantics.

Returns a fresh alist; the input is not mutated.  Insertion order
is preserved across the surviving fields."
  (cl-remove-if (lambda (pair) (eq (cdr pair) :absent))
                response-data))

(defun firefox-to-emacs-native-messenger--serialize-response (stripped-response)
  "Serialize STRIPPED-RESPONSE to a length-prefixed UTF-8 JSON frame.

STRIPPED-RESPONSE is the post-null-stripping output of
`firefox-to-emacs-native-messenger--build-response' (an alist whose
`:absent' fields have already been removed).  The function:

  1. Calls `json-serialize' with `:null-object :null' and
     `:false-object :false' so the `:false' sentinel emitted by
     handlers serializes as JSON `false', matching the parser's
     configuration in the per-connection filter.
  2. UTF-8 encodes the resulting (potentially multibyte) JSON string
     to a unibyte string so byte arithmetic is accurate (using
     `length' on the original multibyte string would understate the
     byte count for any non-ASCII content).
  3. Packs the 4-byte little-endian length prefix via the existing
     codec at REQ-0100 / CON-0800.
  4. Returns the concatenation of prefix and UTF-8 payload as a
     unibyte string suitable for `process-send-string'.

The output length is exactly 4 + N where N is the UTF-8 byte length
of the serialized JSON.

This function does NOT enforce the outbound-response cap; that check
is the response writer's responsibility per PAT-0600."
  (let* ((json (json-serialize stripped-response
                               :null-object :null
                               :false-object :false))
         (unibyte (if (multibyte-string-p json)
                      (encode-coding-string json 'utf-8)
                    json))
         (prefix (firefox-to-emacs-native-messenger--pack-length
                  (length unibyte))))
    (concat prefix unibyte)))

(defun firefox-to-emacs-native-messenger--cleanup-timer-expire (client)
  "Post-response cleanup-timer expiration handler for CLIENT.

Performs idempotent teardown: clears the cleanup-timer plist key,
transitions the connection state to `closing' (per Section 8.22),
removes the connection from the connection registry, and deletes the
process if still live.  Safe to invoke after the connection has
already been deleted."
  (process-put
   client
   firefox-to-emacs-native-messenger--connection-key-cleanup-timer
   nil)
  (when (process-live-p client)
    (process-put
     client
     firefox-to-emacs-native-messenger--connection-key-state
     'closing)
    (firefox-to-emacs-native-messenger--connection-registry-remove client)
    (delete-process client)))

(defun firefox-to-emacs-native-messenger--cancel-cleanup-timer (client)
  "Cancel any post-response cleanup timer on CLIENT.

Idempotent: silent no-op if no timer is set.  Clears the cleanup-timer
plist key after cancellation so subsequent observers see a clean
slate."
  (let ((timer (process-get
                client
                firefox-to-emacs-native-messenger--connection-key-cleanup-timer)))
    (when (timerp timer)
      (cancel-timer timer))
    (process-put
     client
     firefox-to-emacs-native-messenger--connection-key-cleanup-timer
     nil)))

(defun firefox-to-emacs-native-messenger--start-cleanup-timer (client)
  "Schedule the post-response cleanup timer for CLIENT.

Cancels any prior cleanup timer first so re-entry is idempotent.
The timer fires after the post-response cleanup interval defcustom
seconds and invokes the cleanup-timer expiration handler per
Section 8.22 transitions.

Returns the new timer."
  (firefox-to-emacs-native-messenger--cancel-cleanup-timer client)
  (let ((timer (run-at-time
                firefox-to-emacs-native-messenger-post-response-cleanup-timer
                nil
                #'firefox-to-emacs-native-messenger--cleanup-timer-expire
                client)))
    (process-put
     client
     firefox-to-emacs-native-messenger--connection-key-cleanup-timer
     timer)
    timer))

(defun firefox-to-emacs-native-messenger--write-response (client response-data)
  "Build, serialize, and send RESPONSE-DATA on CLIENT.

The complete response writer composite per GUD-300 / Section 8.18 and
PAT-0600:

  1. Pre-send guard: refuse to send unless CLIENT is live AND its
     state is one of `reading' or `dispatched'.  Other states are
     post-response or in-teardown; writing again would double-send.

  2. Build the response (structural null-stripping per PROTOCOL.md
     S6) and serialize it (length prefix plus UTF-8 JSON).

  3. If the serialized payload exceeds the outbound cap, replace
     the response with the generic `response too large' error
     (PROTOCOL.md S11 / fixture `oversized-response-error') and
     re-serialize.  If the replacement also exceeds the cap (a
     degenerate configuration), log a critical message, mark state
     `responded', start the cleanup timer, and send NO frame.

  4. Mark state `responded' BEFORE sending so a concurrent
     observation cannot see an inconsistent state.

  5. Send the framed payload then `process-send-eof'.  Both calls
     are wrapped in `condition-case-unless-debug' so a failing send
     (peer disconnected, EPIPE) is logged at warn level and does not
     propagate to the caller.

  6. Schedule the post-response cleanup timer from an `unwind-protect'
     cleanup form so it starts even when the send raises.

Returns nil unconditionally.  Section 8.22 transitions: `reading' or
`dispatched' -> `responded' here, then `responded' -> `closing' on
cleanup-timer expiration or sentinel peer-close."
  (let ((state (process-get
                client
                firefox-to-emacs-native-messenger--connection-key-state)))
    (cond
     ((or (not (process-live-p client))
          (not (memq state '(reading dispatched))))
      (firefox-to-emacs-native-messenger--log
       'warn
       "write-response refused: client live=%S state=%s; no frame sent"
       (process-live-p client) state)
      nil)
     (t
      (let* ((cap firefox-to-emacs-native-messenger-outbound-response-cap)
             (built (firefox-to-emacs-native-messenger--build-response
                     response-data))
             (framed (firefox-to-emacs-native-messenger--serialize-response
                      built))
             (payload-len (- (length framed) 4)))
        (when (> payload-len cap)
          (firefox-to-emacs-native-messenger--log
           'warn "response payload %d exceeds cap %d; replacing"
           payload-len cap)
          (let* ((replacement
                  (firefox-to-emacs-native-messenger--build-response
                   '((cmd . "error") (error . "response too large"))))
                 (replacement-framed
                  (firefox-to-emacs-native-messenger--serialize-response
                   replacement))
                 (replacement-len (- (length replacement-framed) 4)))
            (cond
             ((> replacement-len cap)
              (firefox-to-emacs-native-messenger--log
               'error
               "degenerate oversize: replacement %d still exceeds cap %d on %S; sending no frame"
               replacement-len cap client)
              (setq framed nil))
             (t
              (setq framed replacement-framed)))))
        (process-put
         client
         firefox-to-emacs-native-messenger--connection-key-state
         'responded)
        (unwind-protect
            (when framed
              (condition-case-unless-debug err
                  (progn
                    (process-send-string client framed)
                    (process-send-eof client))
                (error
                 (firefox-to-emacs-native-messenger--log
                  'warn "send failed on %S: %S" client err))))
          (firefox-to-emacs-native-messenger--start-cleanup-timer client)))))
    nil))

(defun firefox-to-emacs-native-messenger--dispatch-request
    (client request)
  "Dispatch REQUEST on CLIENT to its registered handler.

Returns the response object.  Sending is performed by the
response writer in a subsequent phase, not by this function.

Behavior per the Phase 0500 dispatcher contract:

  - If REQUEST has no cmd field, returns the generic error
    response naming the missing field.
  - If the cmd field is non-string, returns the
    unhandled-message error response.
  - If the cmd string is not registered in
    firefox-to-emacs-native-messenger--handlers, returns the
    unhandled-message error response.
  - Otherwise invokes the handler with CLIENT and REQUEST.
    Errors raised inside the handler are caught with
    condition-case-unless-debug and converted into the generic
    error response carrying the signal's message."
  (let ((cmd-pair (and (listp request) (assq 'cmd request))))
    (cond
      ((null cmd-pair)
        (firefox-to-emacs-native-messenger--build-error-response
          "missing cmd field"))
      ((not (stringp (cdr cmd-pair)))
        (firefox-to-emacs-native-messenger--build-error-response
          "Unhandled message"))
      (t
        (let* ((cmd (cdr cmd-pair))
              (handler
                (gethash
                  cmd firefox-to-emacs-native-messenger--handlers)))
          (cond
            ((null handler)
              (firefox-to-emacs-native-messenger--build-error-response
                "Unhandled message"))
            (t
              (condition-case-unless-debug err
                    (funcall handler client request)
                (error
                  (firefox-to-emacs-native-messenger--build-error-response
                    (error-message-string err)))))))))))

(defun firefox-to-emacs-native-messenger--filter-on-complete-frame
    (client payload)
  "Handle a complete request frame's PAYLOAD bytes on CLIENT.

PAYLOAD is the unibyte JSON-encoded request body, exclusive of
the 4-byte length prefix.  The helper cancels the read-timer,
decodes the JSON as an alist keyed by symbols, dispatches the
request, and routes the dispatcher's response through the
response writer for synchronous handlers (state still `reading'
after dispatch).  Asynchronous handlers (state transitioned to
`dispatched' during the handler's execution) are responsible for
calling the writer themselves later from their own callbacks; the
dispatcher MUST NOT call the writer in that case.

JSON-parse failures and any unexpected handler-layer signals are
routed to `firefox-to-emacs-native-messenger--filter-handle-parse-error',
which writes a generic error response and resets the read-buffer
state.  This routing preserves the one-request-one-response wire
contract per GUD-200."
  (firefox-to-emacs-native-messenger--cancel-read-timer client)
  (condition-case err
      (let* ((request
              (json-parse-string
               payload
               :object-type 'alist
               :null-object nil
               :false-object :false))
             (response
              (firefox-to-emacs-native-messenger--dispatch-request
               client request)))
        (when (eq 'reading
                  (process-get
                   client
                   firefox-to-emacs-native-messenger--connection-key-state))
          (firefox-to-emacs-native-messenger--write-response
           client response)))
    (error
     (firefox-to-emacs-native-messenger--filter-handle-parse-error
      client err))))

(defun firefox-to-emacs-native-messenger--filter-reading (client string)
  "Reading-state branch of the per-connection filter.

Appends STRING to CLIENT's read buffer, decodes the 4-byte
length prefix once enough bytes have accumulated, enforces the
inbound frame-size cap per SEC-0200/SEC-0900, rejects
zero-length frames, rejects buffers carrying surplus bytes
after the declared payload, and dispatches once the buffer
contains exactly the declared payload.  Underfilled buffers
wait silently for additional bytes; the read-timeout timer
will eventually fire if the peer never sends them."
  (let* ((buf-key
          firefox-to-emacs-native-messenger--connection-key-read-buffer)
        (len-key
          firefox-to-emacs-native-messenger--connection-key-declared-length)
        (buf (concat (process-get client buf-key) string))
        (cap firefox-to-emacs-native-messenger-inbound-frame-cap))
    (process-put client buf-key buf)
    (when (and (null (process-get client len-key))
            (>= (length buf) 4))
      (process-put
        client len-key
        (firefox-to-emacs-native-messenger--unpack-length
          (substring buf 0 4))))
    (let ((declared (process-get client len-key)))
      (cond
        ((null declared) nil)
        ((> declared cap)
          (firefox-to-emacs-native-messenger--log
            'warn "frame size %d exceeds cap %d; closing %S"
            declared cap client)
          (firefox-to-emacs-native-messenger--close-connection client))
        ((zerop declared)
          (firefox-to-emacs-native-messenger--log
            'warn "zero-length frame; closing %S" client)
          (firefox-to-emacs-native-messenger--close-connection client))
        ((< (length buf) (+ 4 declared)) nil)
        ((> (length buf) (+ 4 declared))
          (firefox-to-emacs-native-messenger--log
            'warn "surplus bytes after complete frame; closing %S" client)
          (firefox-to-emacs-native-messenger--close-connection client))
        (t
          (firefox-to-emacs-native-messenger--filter-on-complete-frame
            client (substring buf 4)))))))

(defun firefox-to-emacs-native-messenger--connection-filter (client string)
  "Per-connection filter for the bridge listener's accepted clients.

STRING is the chunk of bytes most recently received from the
peer.  The filter dispatches on the connection's state plist
field per Section 8.22:

  - reading: accumulate bytes, decode the length prefix, parse
    a complete frame and hand it to the dispatcher; silently
    close the connection on protocol violations (zero-length
    frame, oversize frame, surplus bytes after the payload)
    per SEC-0900.
  - dispatched: a deferred-response handler is in flight; any
    further bytes from the peer are a protocol violation, so
    delete the process and let the sentinel finalize cleanup.
  - responded or closing: teardown is in progress; log and
    ignore the incoming bytes."
  (let ((state
        (process-get
          client
          firefox-to-emacs-native-messenger--connection-key-state)))
    (cond
      ((eq state 'reading)
        (firefox-to-emacs-native-messenger--filter-reading client string))
      ((eq state 'dispatched)
        (firefox-to-emacs-native-messenger--log
          'warn "bytes received in dispatched state; closing %S" client)
        (when (process-live-p client)
          (delete-process client)))
      (t
        (firefox-to-emacs-native-messenger--log
          'debug "bytes received in state %s on %S; ignoring" state client)
        nil))))

(defconst firefox-to-emacs-native-messenger--close-event-prefixes
  '("deleted" "finished" "exited" "killed" "connection broken")
  "Process-event prefixes recognized as connection-close indications.
The per-connection sentinel reacts on any event whose textual
representation starts with one of these prefixes; non-close events
(typically \"open from ...\") are silently ignored.")

(defun firefox-to-emacs-native-messenger--close-event-p (event)
  "Return non-nil if EVENT describes a connection-close transition.

EVENT is the textual event string passed to the process sentinel.
Recognition is by prefix-match against
firefox-to-emacs-native-messenger--close-event-prefixes."
  (and (stringp event)
    (seq-some
      (lambda (prefix) (string-prefix-p prefix event))
      firefox-to-emacs-native-messenger--close-event-prefixes)))

(defun firefox-to-emacs-native-messenger--connection-sentinel (client event)
  "Per-connection sentinel for the bridge listener's accepted clients.

Invoked by Emacs on every state transition of CLIENT.  The
sentinel reacts only to close-like events per
firefox-to-emacs-native-messenger--close-event-prefixes.  On a
close event:

  - cancels the per-connection read-timeout timer;
  - cancels the post-response cleanup timer (if scheduled);
  - logs the event, distinguishing peer-close-after-response
    (prior state was responded) from premature peer-close (prior
    state was reading, dispatched, or already closing);
  - if prior state was `dispatched' (a Phase 0800 deferred-response
    run is in flight), triggers the run-cancellation extension:
    locates the linked subprocess via the connection's
    `run-subprocess' plist key, CAS-sets the run-state's
    `terminal-cause' to `connection-loss', cancels any pending
    timeout-timer, and invokes signal-escalation on the subprocess
    process group per PAT-0800;
  - sets the connection state to closing;
  - removes the connection from the connection registry.

Non-close events (most commonly Emacs's accept-side \"open\"
signal) are silently ignored.  Returns nil unconditionally."
  (when (firefox-to-emacs-native-messenger--close-event-p event)
    (let ((prior-state
          (process-get
            client
            firefox-to-emacs-native-messenger--connection-key-state))
        (trimmed (string-trim event)))
      (firefox-to-emacs-native-messenger--cancel-cleanup-timer client)
      (firefox-to-emacs-native-messenger--cancel-read-timer client)
      (if (eq prior-state 'responded)
        (firefox-to-emacs-native-messenger--log
          'info "peer close after response on %S (event: %s)"
          client trimmed)
        (firefox-to-emacs-native-messenger--log
          'warn
          "premature peer close on %S in state %s (event: %s)"
          client prior-state trimmed))
      (when (eq prior-state 'dispatched)
        (firefox-to-emacs-native-messenger--connection-sentinel--cancel-run
          client))
      (process-put
        client
        firefox-to-emacs-native-messenger--connection-key-state
        'closing)
      (firefox-to-emacs-native-messenger--connection-registry-remove
        client)))
  nil)

(defun firefox-to-emacs-native-messenger--connection-sentinel--cancel-run
    (client)
  "Trigger the run-cancellation chain on CLIENT's linked run subprocess.

Called by the per-connection sentinel when prior state was
`dispatched' (a Phase 0800 deferred-response run was in flight).
The helper:

  1. Resolves the linked subprocess via the connection's
     `run-subprocess' plist key.  No-op if unset or not a process.
  2. Resolves the run-state hash table via the subprocess's
     `run-state' plist key.  No-op if unset or not a hash table.
  3. Attempts the CAS transition `terminal-cause' nil ->
     `\\='connection-loss'.  On successful CAS (the first observer
     wins), cancels any pending `timeout-timer' and invokes
     `firefox-to-emacs-native-messenger--run-signal-escalate'.
     On unsuccessful CAS, the cause is already set by another
     terminal path (overflow filter, timeout timer, or a
     concurrent connection sentinel) and the helper is a no-op
     beyond the log line that the connection-loss observer was
     not first.

Per PAT-0800 the subprocess sentinel will subsequently fire when
the subprocess exits and route a `connection-loss' response (which
is a no-response: the writer's pre-send guard refuses to send when
the connection state is `closing')."
  (let ((sub (process-get
              client
              firefox-to-emacs-native-messenger--connection-key-run-subprocess)))
    (when (processp sub)
      (let ((state (process-get
                    sub
                    firefox-to-emacs-native-messenger--subprocess-key-run-state)))
        (when (hash-table-p state)
          (cond
           ((firefox-to-emacs-native-messenger--run-state-cas-terminal-cause
             state 'connection-loss)
            (firefox-to-emacs-native-messenger--log
             'info "connection lost on %S; cancelling run subprocess %S"
             client sub)
            (firefox-to-emacs-native-messenger--cancel-timer
             (gethash 'timeout-timer state))
            (puthash 'timeout-timer nil state)
            (firefox-to-emacs-native-messenger--run-signal-escalate state))
           (t
            (firefox-to-emacs-native-messenger--log
             'info
             "connection lost on %S after terminal-cause already set; no-op"
             client))))))))

(defun firefox-to-emacs-native-messenger--accept-handler
    (_server client message)
  "Install per-connection state on newly-accepted CLIENT.

Installed as the listener's `:log' callback by
`firefox-to-emacs-native-messenger-start'.  Emacs invokes this
function with the server process, the freshly-created client
process, and a textual MESSAGE describing the event (for accepted
connections the message begins with the literal \"accept\").

On an accept event the handler:

  - sets `process-query-on-exit-flag' to nil on CLIENT;
  - attaches the per-connection filter and sentinel functions;
  - initializes the per-connection plist (empty unibyte read
    buffer, declared length unset, state `reading');
  - schedules the read-timeout timer;
  - registers CLIENT in the connection registry;
  - emits an info-level log entry naming the accept event.

Non-accept events (server-process failures, ...) are forwarded to
the logger at info level and otherwise ignored."
  (cond
   ((and (stringp message)
         (string-prefix-p "accept" message))
    (set-process-query-on-exit-flag client nil)
    (set-process-filter
     client
     #'firefox-to-emacs-native-messenger--connection-filter)
    (set-process-sentinel
     client
     #'firefox-to-emacs-native-messenger--connection-sentinel)
    (process-put
     client
     firefox-to-emacs-native-messenger--connection-key-read-buffer
     (unibyte-string))
    (process-put
     client
     firefox-to-emacs-native-messenger--connection-key-declared-length
     nil)
    (process-put
     client
     firefox-to-emacs-native-messenger--connection-key-state
     'reading)
    (firefox-to-emacs-native-messenger--start-read-timer client)
    (firefox-to-emacs-native-messenger--connection-registry-add client)
    (firefox-to-emacs-native-messenger--log
     'info "accept connection %S" client))
   (t
    (firefox-to-emacs-native-messenger--log
     'info "listener event: %s"
     (if (stringp message) message (format "%S" message))))))


;;;; ============================================================
;;;; Phase 0700: handlers, gates, and supporting helpers.
;;;; ============================================================

(defun firefox-to-emacs-native-messenger--tramp-guard (path)
  "Reject PATH when it is a TRAMP/remote path per SEC-0100.

`file-remote-p' is the authoritative locality check; any non-nil return
indicates a remote/TRAMP path.  The guard signals
`firefox-to-emacs-native-messenger-bad-request' on rejection so that
callers (the path-expansion helper, the `read' handler, the
`getconfigpath' candidate walker) can translate the signal into the
fixture-defined generic error wording (\"remote paths not permitted\").

Returns PATH unchanged on success; the return value allows callers to
chain the guard inline with the path argument (e.g.,
`(setq path (--tramp-guard path))')."
  (when (file-remote-p path)
    (signal 'firefox-to-emacs-native-messenger-bad-request
            (list "remote paths not permitted" path)))
  path)

(defun firefox-to-emacs-native-messenger--expand-path (path)
  "Expand PATH per the PAT-0400 path-expansion helper sequence.

The sequence enforces TRAMP/remote-path rejection at two stages, with
env-var substitution and tilde/relative expansion in between, exactly
as documented in plan Section 8.16:

  1. First TRAMP guard on PATH as supplied by the caller.
  2. Let-bind `default-directory' to the user's home directory so
     relative paths and ~ references resolve against a known-local
     base.
  3. `substitute-env-vars' with WHEN-UNDEFINED set to t (Emacs 30+):
     set variables are expanded to their values; unset references
     pass through literally (the t value is the documented
     keep-original passthrough behavior).
  4. `expand-file-name' to canonicalize the path.
  5. Second TRAMP guard on the expanded path (catches the case where
     a local raw input expands into a TRAMP path via env-var
     substitution).

Returns the expanded canonical local path on success.  Signals
`firefox-to-emacs-native-messenger-bad-request' at either guard
boundary, propagating to the caller (typically the `read' handler or
the `getconfigpath' candidate walker) for translation into the
generic error response."
  (firefox-to-emacs-native-messenger--tramp-guard path)
  (let* ((default-directory (expand-file-name "~/"))
         (substituted (substitute-env-vars path t))
         (expanded (expand-file-name substituted)))
    (firefox-to-emacs-native-messenger--tramp-guard expanded)
    expanded))

(defun firefox-to-emacs-native-messenger--read-gate-match-p (expanded-path)
  "Return non-nil iff EXPANDED-PATH is permitted by the read-whitelist.

Re-validates `firefox-to-emacs-native-messenger-read-whitelist' per
PAT-1100 site 3 (the per-gate-check site) before consulting entries.
On malformed defcustom values the shared validator signals
`firefox-to-emacs-native-messenger-whitelist-malformed'; the `read'
handler catches the signal and produces the generic error response.

Whitelist semantics per REQ-2700, REQ-3700, REQ-3800:

  - nil or the empty list: deny-all (return nil).
  - `(\"*\")': allow-all sentinel (return t).
  - otherwise: each entry is one of:
      * the literal token `<TEMP-PATH>': match iff EXPANDED-PATH is in
        the capability registry per
        `firefox-to-emacs-native-messenger--registry-contains-p'.
      * any other absolute-path string: match per
        `firefox-to-emacs-native-messenger--glob-match-p' (literal
        paths are globs without metacharacters, so the same matcher
        covers both cases).

The function reads the whitelist fresh on every call per REQ-4200;
no caching is performed."
  (let ((whitelist firefox-to-emacs-native-messenger-read-whitelist))
    (firefox-to-emacs-native-messenger--validate-whitelist whitelist 'read)
    (cond
      ((null whitelist) nil)
      ((equal whitelist '("*")) t)
      (t
        (cl-block found
          (dolist (entry whitelist)
            (when (firefox-to-emacs-native-messenger--read-entry-match-p
                    entry expanded-path)
              (cl-return-from found t)))
          nil)))))

(defun firefox-to-emacs-native-messenger--read-entry-match-p (entry path)
  "Return non-nil iff ENTRY matches PATH under read-whitelist semantics.

ENTRY is one of the validator-accepted forms: the literal token
`<TEMP-PATH>' (consults the capability registry), or an absolute path
with or without glob metacharacters (matched via the bridge's glob
matcher per REQ-2900).  The validator at PAT-1100 sites 1-3 guarantees
ENTRY has one of those shapes; this helper is for the MATCH step only,
not for re-validation."
  (cond
    ((equal entry "<TEMP-PATH>")
      (firefox-to-emacs-native-messenger--registry-contains-p path))
    (t
      (firefox-to-emacs-native-messenger--glob-match-p entry path))))

(defun firefox-to-emacs-native-messenger--version-handler (_client _request)
  "Return the wire response for the `version' cmd per PROTOCOL.md Section 8.

CLIENT and REQUEST are accepted for dispatch-table uniformity but
neither is consulted: the `version' handler reads no request fields
and produces a pure function of the bridge's version defconst.  The
handler is ungated (no whitelist applies) per REQ-1600/1900."
  (list (cons 'cmd "version")
        (cons 'version firefox-to-emacs-native-messenger-version)
        (cons 'code 0)))

(defun firefox-to-emacs-native-messenger--sanitize-temp-prefix (prefix)
  "Sanitize PREFIX for use in a `make-temp-file' template per upstream rules.

The sanitization mirrors upstream `native_main.nim's filename
sanitization for the `temp' handler (REQ-1100 / SEC-1100):

  1. Coerce non-string PREFIX (nil, integer, etc.) to the empty
     string before any substring operations.
  2. Lowercase ASCII letters via `downcase'.
  3. Retain only ASCII letters, digits, and the dot character;
     replace every other character with the empty string.
  4. Collapse any run of two or more consecutive dots into a single
     dot.

Returns the sanitized prefix string; an empty input yields the empty
string.  Callers integrating with `make-temp-file' append a fixed
literal suffix so a degenerate empty prefix still yields a valid
template."
  (if (not (stringp prefix))
      ""
    (let* ((lower (downcase prefix))
           (stripped (replace-regexp-in-string "[^a-z0-9.]" "" lower))
           (collapsed (replace-regexp-in-string "\\.\\.+" "." stripped)))
      collapsed)))

(defun firefox-to-emacs-native-messenger--temp-handler (_client request)
  "Implement the `temp' cmd: write a tempfile and register it for later access.

Ordering per TASK-14800 is non-negotiable:

  1. Field validation.  Request MUST carry a string `content' field;
     missing, null, or non-string `content' returns the generic error
     response WITHOUT creating any tempfile.  `prefix' is optional and
     defaults to the empty string; non-string prefix is coerced to
     empty by the sanitizer.
  2. Registry-cap preflight per REQ-3000 / Section 8.8: if the registry
     count is at or above
     `firefox-to-emacs-native-messenger-temp-registry-cap', invoke
     `--registry-prune-all' to drop stale entries.  If still at or
     above the cap, return the generic error without creating a
     tempfile.
  3. Build the `make-temp-file' template: prefix = FILE-1600 + `tmp_'
     + sanitized + `_'; suffix = `.txt'; no DIR-FLAG (regular file).
  4. Atomically chmod the new file to 0600.
  5. Write `content' to the file.  Any signal from steps 4-7 (chmod,
     write, registration) triggers cleanup: the just-created tempfile
     is unlinked and the generic error response is returned without
     registering.
  6. Register the path in the capability registry with its current
     `file-attributes' identity.
  7. Return the success response per the `temp-success' fixture:
     `((cmd . \"temp\") (content . PATH) (code . 0))'.

CLIENT is accepted for dispatch-table uniformity and not consulted.
The handler is ungated per REQ-2600: any well-formed `temp' request
succeeds regardless of the per-handler whitelists.  The security
boundary is FILE-1600's mode 0700 daemon-UID-owned directory and the
capability-registry cap."
  (let ((content-cell (assq 'content request)))
    (cond
      ((or (null content-cell) (not (stringp (cdr content-cell))))
        (firefox-to-emacs-native-messenger--build-error-response
          "missing required field: content"))
      (t
        (firefox-to-emacs-native-messenger--temp-handler--proceed
          (cdr content-cell)
          (cdr-safe (assq 'prefix request)))))))

(defun firefox-to-emacs-native-messenger--temp-handler--proceed
    (content prefix)
  "Internal continuation of `--temp-handler' after field validation succeeded.

Implements the registry-cap preflight (REQ-3000), tempfile creation,
chmod-then-write sequence with cleanup-on-failure, and capability
registration.  Returns the wire response alist."
  (let* ((registry firefox-to-emacs-native-messenger--capability-registry)
         (cap firefox-to-emacs-native-messenger-temp-registry-cap))
    (when (>= (hash-table-count registry) cap)
      (firefox-to-emacs-native-messenger--registry-prune-all))
    (cond
      ((>= (hash-table-count registry) cap)
        (firefox-to-emacs-native-messenger--build-error-response
          "temp registry cap exceeded"))
      (t
        (firefox-to-emacs-native-messenger--temp-handler--create
          content prefix)))))

(defun firefox-to-emacs-native-messenger--temp-handler--create
    (content prefix)
  "Create the tempfile, chmod/write/register, and build the success response.

On any error during chmod, write, or registration, unlinks the
just-created tempfile and returns the generic error response.  This
helper assumes the field validation (string content) and the
registry-cap preflight have already passed."
  (let* ((sanitized
           (firefox-to-emacs-native-messenger--sanitize-temp-prefix prefix))
         (template
           (concat firefox-to-emacs-native-messenger-tempfile-directory
                   "tmp_" sanitized "_"))
         (path (make-temp-file template nil ".txt"))
         (succeeded nil))
    (unwind-protect
        (condition-case err
            (progn
              (set-file-modes path #o600)
              (let ((coding-system-for-write 'binary))
                (write-region content nil path nil 'no-message))
              (firefox-to-emacs-native-messenger--registry-register path)
              (setq succeeded t)
              (list (cons 'cmd "temp")
                    (cons 'content path)
                    (cons 'code 0)))
          (error
            (firefox-to-emacs-native-messenger--build-error-response
              (format "temp handler error: %s"
                      (error-message-string err)))))
      (unless succeeded
        (when (file-exists-p path)
          (ignore-errors (delete-file path)))))))

(defun firefox-to-emacs-native-messenger--find-rcpath ()
  "Return the first existing candidate in `--rcpath-candidates' or nil.

The walker iterates
`firefox-to-emacs-native-messenger-rcpath-candidates' in upstream-defined
order (per PROTOCOL.md Section 17).  For each candidate:

  1. Runs the TRAMP guard (PAT-0400 step 1) defensively per SEC-1300.
     A TRAMP-shaped candidate signals `bad-request' immediately; this
     can occur only if a future code change introduces a TRAMP-shaped
     entry into the bridge-hardcoded list.
  2. Checks `file-attributes' on the candidate; returns the candidate
     iff the file exists AND is a regular file (not a directory,
     symlink, or other non-regular file).

Returns nil if no candidate qualifies.  The `getconfigpath' handler
(and any future `getconfig' implementation) consumes this walker to
implement the upstream-compatible first-existing-candidate semantics."
  (cl-block found
    (dolist (candidate firefox-to-emacs-native-messenger-rcpath-candidates)
      (firefox-to-emacs-native-messenger--tramp-guard candidate)
      (let ((attrs (file-attributes candidate)))
        (when (and attrs (null (file-attribute-type attrs)))
          (cl-return-from found candidate))))
    nil))

(defun firefox-to-emacs-native-messenger--getconfigpath-handler
    (_client _request)
  "Return the upstream-compatible `getconfigpath' response.

Invokes `--find-rcpath' to locate the first existing rcpath candidate.
On a hit, returns `((cmd . \"getconfigpath\") (content . PATH) (code
. 0))' matching the PROTOCOL.md `getconfigpath-success' fixture.  On a
miss, returns `((cmd . \"getconfigpath\") (code . 1))' matching the
`getconfigpath-empty' fixture (no `content' field).

The handler is UNGATED per REQ-4600: no whitelist applies, the handler
takes no request fields, and the returned path is NOT registered in
the capability registry (REQ-4700).  The candidate list is
bridge-hardcoded; the rationale parallels the `temp' handler's
no-gate posture (security boundary lives elsewhere).

CLIENT and REQUEST are accepted for dispatch-table uniformity and not
consulted."
  (let ((path (firefox-to-emacs-native-messenger--find-rcpath)))
    (if path
        (list (cons 'cmd "getconfigpath")
              (cons 'content path)
              (cons 'code 0))
      (list (cons 'cmd "getconfigpath")
            (cons 'code 1)))))

(defun firefox-to-emacs-native-messenger--read-handler (_client request)
  "Implement the `read' cmd per PROTOCOL.md Sections 5, 12, 15, 16.

Composition order per REQ-3300:

  1. Field validation: `file' MUST be a string.  Missing / null /
     non-string `file' returns the generic error response
     \"missing required field: file\" without any I/O.
  2. Per-gate VALIDATOR (PAT-1100 site 3) on the current
     `firefox-to-emacs-native-messenger-read-whitelist'.  On malformed
     value the validator signals `whitelist-malformed'; the handler
     catches and emits the generic error WITHOUT invoking PAT-0400.
  3. PAT-0400 path expansion (TRAMP guards before and after).
     On TRAMP rejection the handler emits the generic error wording
     \"remote paths not permitted\" matching the
     `read-tramp-rejection' fixture.
  4. Whitelist MATCH against the expanded path via
     `--read-gate-match-p'.  On miss, returns
     \"path not in whitelist\".
  5. Bounded read: reads up to (outbound-cap + 1) bytes from the file
     via `insert-file-contents-literally' into a unibyte buffer.  If
     the raw read returned (outbound-cap + 1) bytes, returns the
     generic error \"file too large to return\" without building a
     content field.  Otherwise returns the upstream success shape
     `((cmd . \"read\") (content . CONTENT) (code . 0))'.
  6. Open-failure: a `file-error' (typically missing file or
     permission denied) returns the upstream open-failure shape
     `((cmd . \"read\") (content . \"\") (code . 2))' per PROTOCOL.md
     Section 5.  This shape is distinct from the generic error shape.

CLIENT is accepted for dispatch-table uniformity and not consulted."
  (let ((file-cell (assq 'file request)))
    (cond
      ((or (null file-cell) (not (stringp (cdr file-cell))))
        (firefox-to-emacs-native-messenger--build-error-response
          "missing required field: file"))
      (t
        (firefox-to-emacs-native-messenger--read-handler--gated
          (cdr file-cell))))))

(defun firefox-to-emacs-native-messenger--read-handler--gated (raw-path)
  "Run the validate-expand-match-read sequence on RAW-PATH from a `read' request.

Returns the wire response alist.  Translates the various internal
signals into the fixture-defined error wordings."
  (condition-case err
      (progn
        (firefox-to-emacs-native-messenger--validate-whitelist
          firefox-to-emacs-native-messenger-read-whitelist 'read)
        (let ((expanded
                (firefox-to-emacs-native-messenger--expand-path raw-path)))
          (cond
            ((firefox-to-emacs-native-messenger--read-gate-match-p expanded)
              (firefox-to-emacs-native-messenger--read-handler--read-file
                expanded))
            (t
              (firefox-to-emacs-native-messenger--build-error-response
                "path not in whitelist")))))
    (firefox-to-emacs-native-messenger-bad-request
      (firefox-to-emacs-native-messenger--build-error-response
        "remote paths not permitted"))
    (firefox-to-emacs-native-messenger-whitelist-malformed
      (firefox-to-emacs-native-messenger--build-error-response
        (format "whitelist malformed: %s"
                (error-message-string err))))))

(defun firefox-to-emacs-native-messenger--read-handler--read-file (path)
  "Bounded-read PATH and return the wire response.

Reads up to (outbound-cap + 1) bytes via
`insert-file-contents-literally' into a unibyte buffer; a fill-to-cap+1
read produces the `file too large to return' generic error; open
failure produces the upstream `read' open-failure shape (`content =
\"\"', `code = 2')."
  (let* ((outbound-cap
           firefox-to-emacs-native-messenger-outbound-response-cap)
         (read-cap (1+ outbound-cap)))
    (condition-case _err
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert-file-contents-literally path nil 0 read-cap)
          (let ((bytes-read (buffer-size)))
            (cond
              ((>= bytes-read read-cap)
                (firefox-to-emacs-native-messenger--build-error-response
                  "file too large to return"))
              (t
                (list (cons 'cmd "read")
                      (cons 'content
                            (decode-coding-string (buffer-string) 'utf-8))
                      (cons 'code 0))))))
      (file-error
        (list (cons 'cmd "read")
              (cons 'content "")
              (cons 'code 2))))))


;;;; ============================================================
;;;; Phase 0800: `run' handler, command-gate, and supporting helpers.
;;;; ============================================================

(defun firefox-to-emacs-native-messenger--run-gate-match-p (command-string)
  "Return non-nil iff COMMAND-STRING is permitted by the run-whitelist.

Re-validates `firefox-to-emacs-native-messenger-run-whitelist' per
PAT-1100 site 3 (the per-gate-check site) before consulting entries.
On malformed defcustom values the shared validator signals
`firefox-to-emacs-native-messenger-whitelist-malformed'; the `run'
handler catches the signal and produces the generic error response.

Whitelist semantics per REQ-2800, REQ-3700, REQ-3800:

  - nil or the empty list: deny-all (return nil).
  - `(\"*\")': allow-all sentinel (return t); the registry is not
    consulted in this branch.
  - otherwise: each entry is a template string with zero or more
    `<TEMP-PATH>' markers; match per
    `firefox-to-emacs-native-messenger--command-gate-match-p',
    which anchors L_0 as a prefix, L_N as a suffix, uses
    first-occurrence search for interior literal segments, and
    consults `--registry-contains-p' on every extracted marker
    substring (Section 8.10).

The whitelist is read fresh on every call per REQ-4200; no caching
is performed so live edits via `customize-set-variable' or `setq'
take immediate effect on the next gate check."
  (let ((whitelist firefox-to-emacs-native-messenger-run-whitelist))
    (firefox-to-emacs-native-messenger--validate-whitelist whitelist 'run)
    (cond
     ((null whitelist) nil)
     ((equal whitelist '("*")) t)
     (t
      (cl-block found
        (dolist (entry whitelist)
          (when (firefox-to-emacs-native-messenger--command-gate-match-p
                 entry command-string)
            (cl-return-from found t)))
        nil)))))

(defun firefox-to-emacs-native-messenger--run-state-cas-terminal-cause
    (state new-cause)
  "Attempt to set STATE's `terminal-cause' to NEW-CAUSE if currently unset.

Returns t on successful transition (the field was nil before this
call and is now NEW-CAUSE); returns nil without mutating STATE if
`terminal-cause' is already set.

This is the single-threaded ELisp equivalent of an atomic
compare-and-set: the first caller wins; subsequent callers return
nil without modifying STATE.  Per PAT-0700 every terminal path
(subprocess sentinel observing exit, accumulator filter detecting
overflow, run-timeout-timer firing, connection sentinel observing
peer close) calls this helper so that the response shape is
determined by the FIRST terminal cause that fires, not the latest.

STATE is the run-state hash table created by the `run' handler;
NEW-CAUSE is one of the five symbols `normal-zero',
`normal-nonzero', `overflow', `timeout', `connection-loss'."
  (cond
   ((gethash 'terminal-cause state) nil)
   (t
    (puthash 'terminal-cause new-cause state)
    t)))

(defun firefox-to-emacs-native-messenger--cancel-timer (timer)
  "Cancel TIMER if it is a live `timerp' object; no-op otherwise.

Idempotent and nil-tolerant.  `timerp' returns non-nil for any timer
object including ones that have already been cancelled or fired;
`cancel-timer' on such objects is a no-op per Emacs convention so
repeated calls are safe.  Callers pass possibly-nil values from
plist/hash-table slots (e.g., the run-state's `timeout-timer' field
when the run-timeout defcustom is unset) without conditional guards
at every callsite.

Returns nil unconditionally."
  (when (timerp timer)
    (cancel-timer timer))
  nil)

(defcustom firefox-to-emacs-native-messenger-run-sigint-grace 2.0
  "Seconds between SIGINT and SIGTERM in the run-subprocess signal escalation.

PAT-0800's cancellation policy escalates SIGINT -> SIGTERM ->
SIGKILL to the run subprocess's process group.  This defcustom
controls the grace period the bridge waits after SIGINT before
sending SIGTERM.  The grace period gives well-behaved subprocesses
time to perform cleanup-on-interrupt (e.g., flushing buffers,
removing tempfiles) before the more aggressive SIGTERM arrives.

Defaults to 2 seconds.  Tests typically let-bind this to a much
smaller value (e.g., 0.3) to keep the test suite fast."
  :type 'number
  :group 'firefox-to-emacs-native-messenger)

(defcustom firefox-to-emacs-native-messenger-run-sigterm-grace 2.0
  "Seconds between SIGTERM and SIGKILL in the run-subprocess signal escalation.

PAT-0800's cancellation policy escalates SIGINT -> SIGTERM ->
SIGKILL to the run subprocess's process group.  This defcustom
controls the grace period the bridge waits after SIGTERM before
sending the un-trappable SIGKILL.  Most processes that ignore
SIGINT respect SIGTERM, so SIGKILL is reached only by processes
that have explicitly trapped or ignored both prior signals.

Defaults to 2 seconds.  Tests typically let-bind this to a much
smaller value (e.g., 0.3) to keep the test suite fast."
  :type 'number
  :group 'firefox-to-emacs-native-messenger)

(defun firefox-to-emacs-native-messenger--pgrp-has-procs-p (pgrp)
  "Return non-nil iff process group PGRP currently contains at least one process.

Uses the POSIX null-signal trick: `signal-process (- pgrp) 0' sends
no actual signal but returns t iff the kernel finds at least one
process in the group.  Wrapped in `ignore-errors' so a kernel
error (EPERM, ESRCH) degrades to nil rather than propagating.

This predicate is used by the signal-escalation chain to decide
whether to send the next signal level.  It is stricter than
`process-live-p' on the immediate child Emacs subprocess because
job-control orphans (e.g., a backgrounded sleep whose parent shell
has already exited) remain in the same pgrp but are no longer
visible as Emacs processes."
  (and (integerp pgrp)
       (ignore-errors (signal-process (- pgrp) 0))))

(defun firefox-to-emacs-native-messenger--run-signal-escalate (state)
  "Escalate signals to the run-state STATE's subprocess process group.

Implements PAT-0800's SIGINT -> SIGTERM -> SIGKILL escalation:

  1. Send SIGINT to the negative pgrp immediately.
  2. After `firefox-to-emacs-native-messenger-run-sigint-grace'
     seconds, if any process in the pgrp is still alive, send
     SIGTERM.
  3. After an additional
     `firefox-to-emacs-native-messenger-run-sigterm-grace' seconds,
     if any process in the pgrp is still alive, send SIGKILL.

Signaling the NEGATIVE pgrp targets the entire process group per
`kill(2)' POSIX semantics.  Emacs creates each subprocess as its
own session leader (via `child_setup'), so the run subprocess and
every descendant in its session share the pgrp; the escalation
chain therefore reaps grandchildren too (TASK-18400 verifies this).

Liveness checks at each step use `--pgrp-has-procs-p' rather than
`process-live-p' on the immediate child Emacs subprocess.  A
backgrounded grandchild whose parent shell exited on SIGINT remains
in the pgrp but is no longer an Emacs-tracked process; the pgrp
check still sees it and triggers the next signal level.  Errors
from `signal-process' (e.g., the kernel already reaped the pgrp)
are caught with `ignore-errors' so a race between exit and signal
does not propagate.

STATE is the run-state hash table populated by the `run' handler.
Returns nil unconditionally.  Idempotent: calling twice schedules
two escalation chains, but both converge on the same outcome once
the pgrp is empty.  The terminal-cause CAS gates the cancellation
paths so in practice this function is called at most once per run."
  (let ((pgrp (gethash 'pgrp state))
        (proc (gethash 'subprocess state)))
    (when (firefox-to-emacs-native-messenger--pgrp-has-procs-p pgrp)
      (firefox-to-emacs-native-messenger--log
       'info "signal-escalate: SIGINT to pgrp %d on %S" pgrp proc)
      (ignore-errors (signal-process (- pgrp) 'int))
      (run-at-time
       firefox-to-emacs-native-messenger-run-sigint-grace nil
       (lambda ()
         (when (firefox-to-emacs-native-messenger--pgrp-has-procs-p pgrp)
           (firefox-to-emacs-native-messenger--log
            'info "signal-escalate: SIGTERM to pgrp %d on %S" pgrp proc)
           (ignore-errors (signal-process (- pgrp) 'term))
           (run-at-time
            firefox-to-emacs-native-messenger-run-sigterm-grace nil
            (lambda ()
              (when (firefox-to-emacs-native-messenger--pgrp-has-procs-p pgrp)
                (firefox-to-emacs-native-messenger--log
                 'warn "signal-escalate: SIGKILL to pgrp %d on %S" pgrp proc)
                (ignore-errors (signal-process (- pgrp) 'kill))))))))))
  nil)

(defun firefox-to-emacs-native-messenger--run-accumulator-filter (proc string)
  "Process filter accumulating STRING into the run-state hash table on PROC.

PROC is the run subprocess; STRING is the raw bytes most recently
emitted via the combined stdout+stderr stream (`:stderr nil' in
`make-process' merges stderr into stdout at the OS level).  The
filter:

  1. Resolves the run-state hash table via `process-get' on
     `firefox-to-emacs-native-messenger--subprocess-key-run-state'.
  2. Appends STRING to the `output-buffer' field.
  3. Increments `output-bytes' by `(length string)' (byte count of
     the unibyte chunk).
  4. If `output-bytes' now exceeds `output-cap', attempts the CAS
     transition `terminal-cause' nil -> `\\='overflow'.  On
     successful CAS (the first observer wins), invokes
     `firefox-to-emacs-native-messenger--run-signal-escalate' to
     terminate the subprocess.  On unsuccessful CAS (a prior path
     already claimed the terminal cause), skips the escalation step
     to preserve the first-writer-wins invariant per PAT-0700.

The filter NEVER emits a wire response itself; the subprocess
sentinel observes the eventual exit and routes the response per
PAT-0700's terminal-cause branching.

Defensively: if PROC's run-state property is unset (the filter was
attached but the handler aborted before populating state, or the
plist was cleared by a prior teardown), the filter is a silent
no-op.  A zero-byte STRING is accumulated normally (no advance of
counters) so the cap check remains correct.

Returns nil unconditionally."
  (let ((state (process-get
                proc
                firefox-to-emacs-native-messenger--subprocess-key-run-state)))
    (when (hash-table-p state)
      (let* ((buf (or (gethash 'output-buffer state) (unibyte-string)))
             (chunk-len (length string))
             (new-buf (concat buf string))
             (prev-bytes (or (gethash 'output-bytes state) 0))
             (new-bytes (+ prev-bytes chunk-len))
             (cap (gethash 'output-cap state)))
        (puthash 'output-buffer new-buf state)
        (puthash 'output-bytes new-bytes state)
        (when (and (integerp cap) (> new-bytes cap))
          (when (firefox-to-emacs-native-messenger--run-state-cas-terminal-cause
                 state 'overflow)
            (firefox-to-emacs-native-messenger--log
             'warn "run output-cap %d exceeded on %S; escalating" cap proc)
            (firefox-to-emacs-native-messenger--run-signal-escalate state))))))
  nil)

(defun firefox-to-emacs-native-messenger--run-build-response (state cause)
  "Build the wire response for a terminated run based on CAUSE.

STATE is the run-state hash table; CAUSE is the symbol from
`terminal-cause' after the sentinel observes exit.  Per PAT-0700
and PROTOCOL.md Sections 3.2.6, 10, 11, and 15:

  - `normal-zero' / `normal-nonzero': returns
    ((cmd . \"run\") (command . CMD) (content . OUTPUT) (code . EXIT))
    where CMD is the request's command string (echoed verbatim),
    OUTPUT is the captured output-buffer decoded as UTF-8 (lenient
    decoding preserves non-UTF-8 bytes as raw), and EXIT is the
    subprocess's exit status from `process-exit-status'.
  - `overflow': returns the generic error shape with
    `\"response too large\"' wording (PROTOCOL.md Section 11).
  - `timeout': returns the generic error shape with
    `\"run timeout exceeded\"' wording (PROTOCOL.md Section 15).
  - any other CAUSE (defensive): returns a generic error naming
    the unexpected value.

The function does NOT consider `connection-loss' because the
sentinel's branching short-circuits that case before calling here.

Decoding the captured unibyte buffer to multibyte UTF-8 happens
here (not in the serializer) so the response object is fully
specified at build time; downstream serialization can rely on the
content being a proper multibyte string."
  (cl-case cause
    ((normal-zero normal-nonzero)
     (let* ((command-string (gethash 'command-string state))
            (raw-bytes (or (gethash 'output-buffer state) (unibyte-string)))
            (sub (gethash 'subprocess state))
            (exit (if (processp sub) (process-exit-status sub) 0))
            (content (decode-coding-string raw-bytes 'utf-8)))
       (list (cons 'cmd "run")
             (cons 'command command-string)
             (cons 'content content)
             (cons 'code (or exit 0)))))
    ((overflow)
     (firefox-to-emacs-native-messenger--build-error-response
      "response too large"))
    ((timeout)
     (firefox-to-emacs-native-messenger--build-error-response
      "run timeout exceeded"))
    (t
     (firefox-to-emacs-native-messenger--build-error-response
      (format "internal error: unknown terminal-cause %S" cause)))))

(defun firefox-to-emacs-native-messenger--run-subprocess-sentinel (proc event)
  "Sentinel for the run subprocess; dispatches the wire response on exit.

Invoked by Emacs on every state transition of PROC; reacts only to
terminal events (`exit' or `signal' status).  Non-terminal events
(e.g., \"open\") are silently ignored.

On a terminal event:

  1. Resolves the run-state hash table via `process-get' on the
     subprocess-key defconst; if absent (defensive), bails out.
  2. Cancels any pending `timeout-timer' field via the nil-tolerant
     `--cancel-timer' helper and clears the field.
  3. If `terminal-cause' is still unset, CAS-sets it to
     `normal-zero' or `normal-nonzero' based on
     `process-exit-status'.
  4. Branches on the final `terminal-cause':
     - `connection-loss': logs and returns without writing.
       The connection has already been torn down by the connection
       sentinel; emitting a frame would either fail (peer gone) or
       be dropped by the writer's pre-send guard, so we skip the
       call entirely.
     - any other cause: builds the response via
       `--run-build-response' and routes it through
       `--write-response' on the connection.  The writer's
       pre-send guard handles double-send protection if the
       sentinel were to fire more than once.

EVENT is accepted for sentinel-API uniformity; the function
inspects `process-status' rather than event text to determine
termination, which is more robust against textual event-string
variations across Emacs versions.

Returns nil unconditionally."
  (when (memq (process-status proc) '(exit signal))
    (let ((state (process-get
                  proc
                  firefox-to-emacs-native-messenger--subprocess-key-run-state)))
      (when (hash-table-p state)
        (firefox-to-emacs-native-messenger--cancel-timer
         (gethash 'timeout-timer state))
        (puthash 'timeout-timer nil state)
        (unless (gethash 'terminal-cause state)
          (let ((exit (process-exit-status proc)))
            (firefox-to-emacs-native-messenger--run-state-cas-terminal-cause
             state
             (if (and (integerp exit) (zerop exit))
                 'normal-zero
               'normal-nonzero))))
        (let ((cause (gethash 'terminal-cause state))
              (conn (gethash 'connection state))
              (trimmed (if (stringp event) (string-trim event) "")))
          (cond
           ((eq cause 'connection-loss)
            (firefox-to-emacs-native-messenger--log
             'info
             "run subprocess exited under connection-loss; no response (event: %s)"
             trimmed))
           (t
            (let ((response
                   (firefox-to-emacs-native-messenger--run-build-response
                    state cause)))
              (firefox-to-emacs-native-messenger--log
               'info
               "run subprocess terminal cause=%S; dispatching response on %S"
               cause conn)
              (when (and (processp conn) (process-live-p conn))
                (firefox-to-emacs-native-messenger--write-response
                 conn response)))))))))
  nil)

(defun firefox-to-emacs-native-messenger--run-timeout-expire (state)
  "Run-timeout timer expiration handler.

Invoked when the per-run timeout timer fires (only relevant when
the `firefox-to-emacs-native-messenger-run-timeout' defcustom was
set to a positive number at run start).  The handler:

  1. Attempts the CAS transition `terminal-cause' nil -> `\\='timeout'.
  2. On successful CAS (the timeout step claims the terminal cause
     first), invokes
     `firefox-to-emacs-native-messenger--run-signal-escalate' to
     terminate the subprocess.
  3. On unsuccessful CAS (the cause is already set by the sentinel,
     the accumulator filter, or the connection sentinel), the
     handler is a silent no-op so escalation is not re-triggered.

The signal-escalation chain ends with SIGKILL; the resulting
subprocess exit fires the run subprocess sentinel which builds the
`run timeout exceeded' error response per PROTOCOL.md Section 15.

STATE is the run-state hash table.  Returns nil unconditionally."
  (when (firefox-to-emacs-native-messenger--run-state-cas-terminal-cause
         state 'timeout)
    (firefox-to-emacs-native-messenger--log
     'warn "run timeout exceeded on %S; escalating"
     (gethash 'subprocess state))
    (firefox-to-emacs-native-messenger--run-signal-escalate state))
  nil)

(defun firefox-to-emacs-native-messenger--run-timeout-schedule (state)
  "Schedule a run-timeout timer on STATE if the timeout defcustom is non-nil.

Reads `firefox-to-emacs-native-messenger-run-timeout' fresh; if it
is a positive number, schedules a one-shot timer firing after that
many seconds whose callback invokes `--run-timeout-expire' on
STATE.  The timer object is stored in STATE's `timeout-timer'
field for later cancellation by the subprocess sentinel or
connection sentinel.

If the defcustom is nil, zero, or negative the function is a
silent no-op and STATE's `timeout-timer' field remains nil.  This
makes the run handler's schedule call unconditional: the absence
of a timeout simply means no timer fires.

Returns the scheduled timer object or nil when no timer was
created."
  (let ((seconds firefox-to-emacs-native-messenger-run-timeout))
    (when (and (numberp seconds) (> seconds 0))
      (puthash 'timeout-timer
               (run-at-time
                seconds nil
                (lambda ()
                  (firefox-to-emacs-native-messenger--run-timeout-expire
                   state)))
               state))))

(defun firefox-to-emacs-native-messenger--run-handler (client request)
  "Implement the `run' cmd per PROTOCOL.md Sections 5, 10, 11, 15, 16.

Composition order per REQ-3300:

  1. Field validation: `command' MUST be a non-nil string.  Missing,
     null, or non-string `command' returns the generic error
     response `\"missing required field: command\"' without any
     side effects.  `content' is optional; if absent or non-string,
     defaults to the empty string.
  2. Per-gate VALIDATOR (PAT-1100 site 3) on the current
     `firefox-to-emacs-native-messenger-run-whitelist'.  Malformed
     defcustom values signal `whitelist-malformed' from inside
     `--run-gate-match-p'; the handler catches and emits the generic
     error with the validator's wording.
  3. Command-gate MATCH per Section 8.10.  On a miss, returns the
     generic error `\"command not in whitelist\"' BEFORE any
     subprocess is spawned.
  4. Subprocess launch via
     `firefox-to-emacs-native-messenger--run-handler--launch':
     creates the subprocess via `make-process' (no external `setsid'
     wrapper; Emacs's `child_setup' already creates each subprocess
     as its own session and process-group leader);
     captures pgrp via `process-attributes' with PID fallback;
     populates the run-state hash table; cross-links the connection
     and subprocess via plist keys; transitions the connection
     state to `dispatched' (so the dispatcher does NOT call
     `--write-response' for this request); schedules the
     run-timeout timer if the defcustom is non-nil; sends
     `content' to stdin followed by EOF (EPIPE tolerated and
     logged); returns a placeholder response that the dispatcher
     discards.

The eventual wire response is produced by the run subprocess
sentinel observing exit and routed via `--write-response' on the
connection.  The connection sentinel's run-cancellation extension
(Phase 0800) handles peer close in the `dispatched' state.

CLIENT is the listener-side accepted client process (a network
process); REQUEST is the parsed JSON alist with at least `cmd' and
`command' keys.  Returns a response alist suitable for the
dispatcher to inspect, even though the dispatcher will not write
it because the connection state is now `dispatched'."
  (let ((cmd-pair (assq 'command request))
        (content-pair (assq 'content request)))
    (cond
     ((or (null cmd-pair) (not (stringp (cdr cmd-pair))))
      (firefox-to-emacs-native-messenger--build-error-response
       "missing required field: command"))
     (t
      (firefox-to-emacs-native-messenger--run-handler--gated
       client
       (cdr cmd-pair)
       (or (and content-pair (stringp (cdr content-pair))
                (cdr content-pair))
           ""))))))

(defun firefox-to-emacs-native-messenger--run-handler--gated
    (client command-string content)
  "Apply gate composition then launch the subprocess.

Per REQ-3300 ordering: whitelist VALIDATOR (raised from inside the
gate-match call) -> command-gate MATCH -> launch.  Each rejection
path returns the generic error response WITHOUT echoing original
request fields per PAT-0300.

Errors from any sub-step are caught and translated to wire-shape
generic errors so the caller (`--run-handler') can return them
directly to the dispatcher."
  (condition-case err
      (cond
       ((not (firefox-to-emacs-native-messenger--run-gate-match-p
              command-string))
        (firefox-to-emacs-native-messenger--build-error-response
         "command not in whitelist"))
       (t
        (firefox-to-emacs-native-messenger--run-handler--launch
         client command-string content)))
    (firefox-to-emacs-native-messenger-whitelist-malformed
     (firefox-to-emacs-native-messenger--build-error-response
      (format "whitelist malformed: %s" (error-message-string err))))
    (firefox-to-emacs-native-messenger-bad-state
     (firefox-to-emacs-native-messenger--build-error-response
      (error-message-string err)))))

(defun firefox-to-emacs-native-messenger--run-handler--launch
    (client command-string content)
  "Launch the run subprocess and link CLIENT to its run-state.

Implements the launch mechanics per PAT-0800 and Section 8.19:

  - `default-directory' is let-bound to `~/' so the subprocess
    inherits the user's home as its cwd.
  - `make-process' invocation: `:command' is `(SHELL SHELL-SWITCH
    COMMAND-STRING)' (e.g., `(\"/bin/sh\" \"-c\" COMMAND-STRING)').  No
    external `setsid' wrapper is used: Emacs's `child_setup' calls
    `setsid(2)' on every async subprocess before exec, so the
    immediate child is already its own session and process-group
    leader (`pid == pgrp == sess').  `:coding' is binary; `:stderr'
    is nil (which merges stderr into stdout at the OS level);
    `:noquery' is t so the subprocess does not block Emacs exit;
    `:connection-type' is pipe.  The `--run-accumulator-filter'
    captures output; the `--run-subprocess-sentinel' dispatches the
    wire response on exit.
  - pgrp capture: attempts `(alist-get \\='pgrp (process-attributes
    PID))'; if that returns nil (subprocess already exited before
    the call, kernel returned no attributes, etc.), falls back to
    PID itself.  Because Emacs's `child_setup' creates each
    subprocess as a session leader, PID == PGID at exec time.  The
    `pgrp-fallback' boolean records whether the fallback was used
    for logging.
  - Run-state hash table: populated with every documented field
    from Section 8.3, plus the `pgrp-fallback' boolean.
  - Cross-link: connection's `run-subprocess' plist key points at
    the subprocess; subprocess's `run-state' plist key points at
    the hash table.
  - State transition: connection state set to `dispatched' so the
    dispatcher does NOT call `--write-response' for this request.
  - run-timeout-timer: scheduled iff the defcustom is non-nil.
  - stdin: `process-send-string' followed by
    `process-send-eof'.  Errors are caught by
    `condition-case-unless-debug' and logged at info level; they
    do NOT propagate so the subprocess sentinel still produces the
    terminal response from the exit observation.

Returns a placeholder response alist that the dispatcher discards
because the connection state has already moved to `dispatched'."
  (let* ((default-directory (expand-file-name "~/"))
         (proc (make-process
                :name "firefox-to-emacs-native-messenger-run"
                :command (list firefox-to-emacs-native-messenger-shell-binary
                               firefox-to-emacs-native-messenger-shell-command-switch
                               command-string)
                :coding 'binary
                :stderr nil
                :noquery t
                :connection-type 'pipe
                :filter
                #'firefox-to-emacs-native-messenger--run-accumulator-filter
                :sentinel
                #'firefox-to-emacs-native-messenger--run-subprocess-sentinel))
         (pid (process-id proc))
         (attrs (ignore-errors (process-attributes pid)))
         (pgrp-from-attrs (and attrs (alist-get 'pgrp attrs)))
         (pgrp (or pgrp-from-attrs pid))
         (pgrp-fallback (not (integerp pgrp-from-attrs)))
         (state (make-hash-table :test 'eq)))
    (puthash 'terminal-cause nil state)
    (puthash 'output-buffer (unibyte-string) state)
    (puthash 'output-bytes 0 state)
    (puthash 'output-cap firefox-to-emacs-native-messenger-run-output-cap state)
    (puthash 'subprocess proc state)
    (puthash 'connection client state)
    (puthash 'command-string command-string state)
    (puthash 'pgrp pgrp state)
    (puthash 'pgrp-fallback pgrp-fallback state)
    (puthash 'timeout-timer nil state)
    (process-put proc
                 firefox-to-emacs-native-messenger--subprocess-key-run-state
                 state)
    (process-put client
                 firefox-to-emacs-native-messenger--connection-key-run-subprocess
                 proc)
    (process-put client
                 firefox-to-emacs-native-messenger--connection-key-state
                 'dispatched)
    (firefox-to-emacs-native-messenger--log
     'info
     "run launched on %S as %S; cmd=%S pgrp=%d pgrp-fallback=%S"
     client proc command-string pgrp pgrp-fallback)
    (firefox-to-emacs-native-messenger--run-timeout-schedule state)
    (condition-case err
        (progn
          (when (> (length content) 0)
            (process-send-string proc content))
          (process-send-eof proc))
      (error
       (firefox-to-emacs-native-messenger--log
        'info "run stdin send error on %S: %S" proc err)))
    (list (cons 'cmd "run-deferred"))))

;;;; Register the five v2 handlers in the dispatcher table.
;;;; Per REQ-1600, only these five cmds are implemented; every other
;;;; cmd falls through to "Unhandled message".  Phase 0800 adds
;;;; `run' (the async / deferred-response handler).

(puthash "version"
         #'firefox-to-emacs-native-messenger--version-handler
         firefox-to-emacs-native-messenger--handlers)
(puthash "getconfigpath"
         #'firefox-to-emacs-native-messenger--getconfigpath-handler
         firefox-to-emacs-native-messenger--handlers)
(puthash "temp"
         #'firefox-to-emacs-native-messenger--temp-handler
         firefox-to-emacs-native-messenger--handlers)
(puthash "read"
         #'firefox-to-emacs-native-messenger--read-handler
         firefox-to-emacs-native-messenger--handlers)
(puthash "run"
         #'firefox-to-emacs-native-messenger--run-handler
         firefox-to-emacs-native-messenger--handlers)

;;;###autoload
(defun firefox-to-emacs-native-messenger-start ()
  "Start the bridge listener, binding the Unix-domain socket at FILE-0600.

Pre-bind checks run in the order documented by the plan and Section 8.21:

  1. Idempotency: if a listener process is already recorded, signal
     `firefox-to-emacs-native-messenger-bad-state' and do not double-bind.
  2. Verify the cache directory (SEC-0500).
  3. Verify the tempfile directory (SEC-1100).
  4. Sweep `tmp_*.txt' from the tempfile directory (PAT-0900).
  5. Clear the capability registry (REQ-3000 / Section 8.8).
  6. Run the whitelist-validation sweep (REQ-4100 site 2).
  7. Probe and delete any stale socket file (PAT-0100).

Only after every pre-bind check succeeds is the listener bound via
`make-network-process' with the arguments specified in Section 8.17.
The new listener process is recorded in
`firefox-to-emacs-native-messenger--listener-process'.

Returns the listener process.  Signals `bad-state' or
`whitelist-malformed' on pre-bind failure; no socket is created in that
case."
  (interactive)
  (when (and firefox-to-emacs-native-messenger--listener-process
             (process-live-p
              firefox-to-emacs-native-messenger--listener-process))
    (signal 'firefox-to-emacs-native-messenger-bad-state
            (list "listener already running"
                  firefox-to-emacs-native-messenger--listener-process)))
  (firefox-to-emacs-native-messenger--verify-cache-directory)
  (firefox-to-emacs-native-messenger--verify-tempfile-directory)
  (firefox-to-emacs-native-messenger--sweep-tempfiles)
  (firefox-to-emacs-native-messenger--registry-clear-on-start)
  (firefox-to-emacs-native-messenger--listener-start-whitelist-sweep)
  (firefox-to-emacs-native-messenger--probe-and-delete-stale-socket
   firefox-to-emacs-native-messenger-socket-path)
  (let ((proc (make-network-process
               :name "firefox-to-emacs-native-messenger-listener"
               :family 'local
               :server t
               :service firefox-to-emacs-native-messenger-socket-path
               :coding '(binary . binary)
               :filter-multibyte nil
               :noquery t
               :log #'firefox-to-emacs-native-messenger--accept-handler)))
    (setq firefox-to-emacs-native-messenger--listener-process proc)
    (add-hook 'kill-emacs-hook
              'firefox-to-emacs-native-messenger--cleanup-on-shutdown)
    (firefox-to-emacs-native-messenger--log
     'info "listener started: socket=%s"
     firefox-to-emacs-native-messenger-socket-path)
    proc))

;;;###autoload
(defun firefox-to-emacs-native-messenger-stop ()
  "Stop the bridge listener and clear its in-memory state.

Tear-down sequence:

  1. If `firefox-to-emacs-native-messenger--listener-process' is nil,
     return nil immediately as a silent no-op (idempotency).
  2. If the file at the configured socket path exists and is NOT a real
     socket (regular file, symlink, fifo, ...), rename it aside before
     calling `delete-process' so the listener's auto-unlink does not
     clobber a non-socket replacement; restore the file on cleanup.
  3. Call `delete-process' on the listener.  For UNIX-domain server
     processes Emacs unlinks the bound socket file as part of cleanup;
     if for any reason a real socket lingers, we unlink it explicitly.
  4. Set the listener process variable to nil.
  5. Clear the capability registry (REQ-3000 / SEC-1200).

Returns nil."
  (interactive)
  (let ((proc firefox-to-emacs-native-messenger--listener-process))
    (when proc
      (let* ((path firefox-to-emacs-native-messenger-socket-path)
             (backup-path nil))
        (unwind-protect
            (progn
              (when (and (file-exists-p path)
                         (not (firefox-to-emacs-native-messenger--path-is-real-socket-p
                               path)))
                (setq backup-path (concat path ".fenm-stop-backup"))
                (when (file-exists-p backup-path)
                  (delete-file backup-path))
                (rename-file path backup-path))
              (when (process-live-p proc)
                (delete-process proc))
              (when (and (file-exists-p path)
                         (firefox-to-emacs-native-messenger--path-is-real-socket-p
                          path))
                (delete-file path)))
          (when backup-path
            (when (file-exists-p path)
              (delete-file path))
            (rename-file backup-path path))
          (setq firefox-to-emacs-native-messenger--listener-process nil)
          (firefox-to-emacs-native-messenger--registry-clear-on-stop)))
      (firefox-to-emacs-native-messenger--log 'info "listener stopped")))
  nil)

(provide 'firefox-to-emacs-native-messenger)
;;; firefox-to-emacs-native-messenger.el ends here
