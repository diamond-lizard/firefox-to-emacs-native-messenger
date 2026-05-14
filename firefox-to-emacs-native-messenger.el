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

(defconst firefox-to-emacs-native-messenger-setsid-binary
  "setsid"
  "Name of the setsid binary used to launch `run' subprocesses in a new session.
The binary is resolved against the daemon's PATH at launch time.")

(defconst firefox-to-emacs-native-messenger-run-command-prefix
  (list firefox-to-emacs-native-messenger-setsid-binary
        "--"
        firefox-to-emacs-native-messenger-shell-binary
        firefox-to-emacs-native-messenger-shell-command-switch)
  "Fixed command-list prefix passed to `make-process' for `run' subprocesses.
At dispatch time the per-request COMMAND-STRING is appended to this prefix to
form the full :command list ((setsid -- /bin/sh -c COMMAND-STRING)).")

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

(defun firefox-to-emacs-native-messenger--send-response (client response)
  "Stub response sink for CLIENT and RESPONSE.

The real response writer arrives in the response-writer task
pair, which serializes RESPONSE to a length-prefixed UTF-8 JSON
frame and sends it on CLIENT.  This stub exists so the filter's
parse-error path can route generic-error responses to a stable
function symbol before the response-writer wiring lands.
Calling the stub is a no-op."
  (ignore client response)
  nil)

(defun firefox-to-emacs-native-messenger--filter-handle-parse-error
    (client error-data)
  "Handle a parse failure on CLIENT with diagnostic ERROR-DATA.

Build a generic error response per PAT-0300 (cmd field set to
the string \"error\"; error field carrying the parse-error
message; no code field) and route it through the response-sink
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
    (firefox-to-emacs-native-messenger--send-response client response)
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
decodes the JSON as an alist keyed by symbols, and routes
either to the dispatcher (on success) or to the parse-error
handler (on decoding failure)."
  (firefox-to-emacs-native-messenger--cancel-read-timer client)
  (condition-case err
        (let ((request
            (json-parse-string
              payload
              :object-type 'alist
              :null-object nil
              :false-object :false)))
      (firefox-to-emacs-native-messenger--dispatch-request
        client request))
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
  - logs the event, distinguishing peer-close-after-response
    (prior state was responded) from premature peer-close
    (prior state was reading, dispatched, or already closing);
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
      (firefox-to-emacs-native-messenger--cancel-read-timer client)
      (if (eq prior-state 'responded)
        (firefox-to-emacs-native-messenger--log
          'info "peer close after response on %S (event: %s)"
          client trimmed)
        (firefox-to-emacs-native-messenger--log
          'warn
          "premature peer close on %S in state %s (event: %s)"
          client prior-state trimmed))
      (process-put
        client
        firefox-to-emacs-native-messenger--connection-key-state
        'closing)
      (firefox-to-emacs-native-messenger--connection-registry-remove
        client)))
  nil)

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
