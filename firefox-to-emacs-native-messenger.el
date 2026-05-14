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

(provide 'firefox-to-emacs-native-messenger)
;;; firefox-to-emacs-native-messenger.el ends here
