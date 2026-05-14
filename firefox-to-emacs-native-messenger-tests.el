;;; firefox-to-emacs-native-messenger-tests.el --- ERT tests for firefox-to-emacs-native-messenger  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 User

;; Keywords: tools, processes

;;; Commentary:

;; ERT test harness for firefox-to-emacs-native-messenger.el.
;; See PROTOCOL.md for the wire-contract and per-handler
;; request/response fixtures consumed by these tests.
;; See README.md for install/usage.
;;
;; ERT tag taxonomy:
;;
;;   :unit            Pure-function tests that touch no external state
;;                    (filesystem, network, subprocesses) and run in
;;                    microseconds.
;;   :integration     Tests that exercise real-environment side effects
;;                    (filesystem, network, subprocesses).
;;   :slow            Tests that may take several seconds; allowed to be
;;                    skipped via test-selector exclusion in fast loops.
;;   :sandbox         Uses the
;;                    `firefox-to-emacs-native-messenger-test-with-sandbox-tempdir'
;;                    macro for an isolated, mode-0700 working directory.
;;   :fixture-driven  Consumes one or more named fixtures from PROTOCOL.md
;;                    via `firefox-to-emacs-native-messenger-test-load-fixture'.
;;   :run-subprocess  Uses
;;                    `firefox-to-emacs-native-messenger-test-run-shell-command'
;;                    to launch a real subprocess.
;;
;; Tags compose freely on a single test (e.g. `:unit :fixture-driven',
;; `:integration :sandbox :run-subprocess :slow').
;;
;; Harness conventions:
;;
;; Fixture loader:
;;   (firefox-to-emacs-native-messenger-test-load-fixture NAME) -> alist
;;   NAME is the kebab-case identifier of a `<!-- fixture: NAME -->' anchor
;;   in PROTOCOL.md.  Returns the parsed JSON body as an alist (object-type
;;   `alist', array-type `list', null-object nil, false-object `:false').
;;   Signals `firefox-to-emacs-native-messenger-test-fixture-not-found' when
;;   NAME is missing or the fixture is malformed.
;;
;; Sandbox-tempdir macro:
;;   (firefox-to-emacs-native-messenger-test-with-sandbox-tempdir VAR BODY...)
;;   Binds VAR to a fresh, mode-0700 directory for the duration of BODY.
;;   The directory is removed (recursively, via `unwind-protect') on body
;;   exit, including non-local exits.  Use this for any test that needs an
;;   isolated working directory (e.g., to host a Unix-domain socket).
;;
;; Framed-request sender:
;;   (firefox-to-emacs-native-messenger-test-send-frame
;;     SOCKET-PATH REQUEST-OBJECT &optional TIMEOUT) -> alist or nil
;;   Opens a Unix-domain client connection to SOCKET-PATH; sends
;;   REQUEST-OBJECT JSON-encoded with a 4-byte little-endian length prefix;
;;   waits up to TIMEOUT (default 5 s) for a framed reply; returns the
;;   parsed reply alist (or nil if the connection failed).
;;
;;   Companion stub-echo listener:
;;   (firefox-to-emacs-native-messenger-test-stub-echo-listener
;;     SOCKET-PATH REPLY-OBJECT) -> server process
;;   Binds SOCKET-PATH and replies with the framed REPLY-OBJECT to each
;;   accepted connection.  Callers stop it via `delete-process'.
;;
;; Real-subprocess helper:
;;   (firefox-to-emacs-native-messenger-test-run-shell-command COMMAND-STRING)
;;     -> (:exit-status N :stdout S)
;;   Synchronously runs COMMAND-STRING under `setsid -- /bin/sh -c'.  Stderr
;;   is merged into stdout.  Output is captured and returned as a plist.
;;
;; Per-test cleanup convention:
;;   Tests that allocate external resources (network processes, subprocesses,
;;   tempdirs, tempfiles) MUST release them via `unwind-protect' so they are
;;   freed even when an assertion fails.  The sandbox-tempdir macro already
;;   uses `unwind-protect' internally; tests that allocate other resources
;;   on top of it should wrap their own teardown in `unwind-protect' as
;;   well.

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'json)
(require 'cl-lib)

(define-error 'firefox-to-emacs-native-messenger-test-fixture-not-found
  "Fixture not found or malformed in PROTOCOL.md")

(defconst firefox-to-emacs-native-messenger-test--project-root
  (file-name-directory
   (or load-file-name
       (bound-and-true-p byte-compile-current-file)
       (buffer-file-name)
       default-directory))
  "Project root directory, used to locate PROTOCOL.md fixtures.")

(defconst firefox-to-emacs-native-messenger-test--protocol-path
  (expand-file-name "PROTOCOL.md"
                    firefox-to-emacs-native-messenger-test--project-root)
  "Absolute path to PROTOCOL.md, the source of fixtures consumed by the harness.")

(defun firefox-to-emacs-native-messenger-test-load-fixture (name)
  "Return the fixture named NAME from PROTOCOL.md as a Lisp alist.

NAME is a kebab-case string matching the identifier in a
`<!-- fixture: NAME -->' anchor (PROTOCOL.md Appendix A).  The
fenced JSON block immediately after the anchor, separated by at
most one blank line, is parsed as JSON with `:object-type \\='alist'
and returned.  Signal
`firefox-to-emacs-native-messenger-test-fixture-not-found' when
NAME is absent or the fixture is malformed."
  (with-temp-buffer
    (insert-file-contents
     firefox-to-emacs-native-messenger-test--protocol-path)
    (goto-char (point-min))
    (let ((anchor-re (concat "^<!-- fixture: "
                             (regexp-quote name)
                             " -->$")))
      (unless (re-search-forward anchor-re nil t)
        (signal 'firefox-to-emacs-native-messenger-test-fixture-not-found
                (list name "anchor not found"))))
    (forward-line 1)
    (when (looking-at-p "^$")
      (forward-line 1))
    (unless (looking-at-p "^```json$")
      (signal 'firefox-to-emacs-native-messenger-test-fixture-not-found
              (list name "opening json fence not found at expected position")))
    (forward-line 1)
    (let ((body-start (point)))
      (unless (re-search-forward "^```$" nil t)
        (signal 'firefox-to-emacs-native-messenger-test-fixture-not-found
                (list name "closing fence not found")))
      (let ((body-end (match-beginning 0)))
        (json-parse-string
         (buffer-substring-no-properties body-start body-end)
         :object-type 'alist
         :array-type 'list
         :null-object nil
         :false-object :false)))))

(defmacro firefox-to-emacs-native-messenger-test-with-sandbox-tempdir
    (var &rest body)
  "Bind VAR to a fresh 0700-mode tempdir for BODY's evaluation.

The directory is created with `make-temp-file' (DIR-FLAG t), its mode is
explicitly set to 0700 to neutralize the inherited umask, then BODY is
evaluated.  The directory is recursively removed when BODY exits, even on
non-local exit (`unwind-protect')."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,var (make-temp-file
                "firefox-to-emacs-native-messenger-test-sandbox-" t)))
     (set-file-modes ,var #o700)
     (unwind-protect
         (progn ,@body)
       (when (file-directory-p ,var)
         (delete-directory ,var t)))))

(defun firefox-to-emacs-native-messenger-test--pack-length (n)
  "Return a 4-byte little-endian unibyte string encoding integer N."
  (unibyte-string (logand n #xff)
                  (logand (ash n -8) #xff)
                  (logand (ash n -16) #xff)
                  (logand (ash n -24) #xff)))

(defun firefox-to-emacs-native-messenger-test--unpack-length (bytes)
  "Decode a 4-byte little-endian unibyte string BYTES into an integer."
  (logior (aref bytes 0)
          (ash (aref bytes 1) 8)
          (ash (aref bytes 2) 16)
          (ash (aref bytes 3) 24)))

(defun firefox-to-emacs-native-messenger-test-stub-echo-listener
    (socket-path reply-object)
  "Bind a Unix-domain server socket at SOCKET-PATH replying with REPLY-OBJECT.

REPLY-OBJECT is JSON-serialized and emitted as a 4-byte little-endian
length-prefix plus UTF-8 payload.  On each accepted connection the listener
reads the inbound length-prefix and full payload (the content is discarded),
sends the framed reply, then closes the connection.  Returns the listener
process so callers may stop it via `delete-process'."
  (let* ((reply-bytes (encode-coding-string
                       (json-serialize reply-object) 'utf-8 t))
         (reply-frame (concat
                       (firefox-to-emacs-native-messenger-test--pack-length
                        (length reply-bytes))
                       reply-bytes)))
    (make-network-process
     :name "firefox-to-emacs-native-messenger-test-stub-echo"
     :family 'local
     :server t
     :service socket-path
     :coding '(binary . binary)
     :filter-multibyte nil
     :noquery t
     :log
     (lambda (_server client _message)
       (set-process-coding-system client 'binary 'binary)
       (process-put client :buf "")
       (process-put client :declared nil)
       (set-process-filter
        client
        (lambda (cproc data)
          (process-put cproc :buf (concat (process-get cproc :buf) data))
          (let ((buf (process-get cproc :buf))
                (declared (process-get cproc :declared)))
            (when (and (null declared) (>= (length buf) 4))
              (setq declared
                    (firefox-to-emacs-native-messenger-test--unpack-length
                     (substring buf 0 4)))
              (process-put cproc :declared declared)
              (process-put cproc :buf (substring buf 4))
              (setq buf (process-get cproc :buf)))
            (when (and declared (>= (length buf) declared))
              (process-send-string cproc reply-frame)
              (process-send-eof cproc)))))))))

(defun firefox-to-emacs-native-messenger-test-send-frame
    (socket-path request-object &optional timeout)
  "Send framed REQUEST-OBJECT to SOCKET-PATH and return the parsed reply.

REQUEST-OBJECT is JSON-encoded and prefixed with its 4-byte little-endian
UTF-8-byte length.  TIMEOUT (default 5 seconds) bounds the wait for the
framed reply.  Returns the parsed JSON reply as an alist (object-type
`alist', array-type `list', null-object nil, false-object `:false') or
nil if the connection cannot be opened."
  (let* ((timeout (or timeout 5))
         (request-bytes (encode-coding-string
                         (json-serialize request-object) 'utf-8 t))
         (frame (concat
                 (firefox-to-emacs-native-messenger-test--pack-length
                  (length request-bytes))
                 request-bytes))
         (reply-buf "")
         (proc (condition-case _err
                   (make-network-process
                    :name "firefox-to-emacs-native-messenger-test-send-frame"
                    :family 'local
                    :service socket-path
                    :coding '(binary . binary)
                    :filter-multibyte nil
                    :noquery t
                    :filter (lambda (_p data)
                              (setq reply-buf (concat reply-buf data))))
                 (error nil))))
    (when proc
      (unwind-protect
          (progn
            (process-send-string proc frame)
            (let ((deadline (+ (float-time) timeout)))
              (while (and (< (length reply-buf) 4)
                          (process-live-p proc)
                          (< (float-time) deadline))
                (accept-process-output proc 0.05))
              (when (>= (length reply-buf) 4)
                (let ((declared
                       (firefox-to-emacs-native-messenger-test--unpack-length
                        (substring reply-buf 0 4))))
                  (while (and (< (length reply-buf) (+ 4 declared))
                              (process-live-p proc)
                              (< (float-time) deadline))
                    (accept-process-output proc 0.05))
                  (when (>= (length reply-buf) (+ 4 declared))
                    (json-parse-string
                     (decode-coding-string
                      (substring reply-buf 4 (+ 4 declared))
                      'utf-8 t)
                     :object-type 'alist
                     :array-type 'list
                     :null-object nil
                     :false-object :false))))))
        (when (process-live-p proc)
          (delete-process proc))))))

(defun firefox-to-emacs-native-messenger-test--match-path (v)
  "Match `path' placeholder: any non-empty string."
  (and (stringp v) (> (length v) 0)))

(defun firefox-to-emacs-native-messenger-test--match-absolute-path (v)
  "Match `absolute-path' placeholder: non-empty string starting with /."
  (and (stringp v) (> (length v) 0) (eq (aref v 0) ?/)))

(defun firefox-to-emacs-native-messenger-test--match-temp-path (v)
  "Match `temp-path': dedicated tempfile-directory pattern."
  (and (stringp v)
       (string-match-p
        "\\`/tmp/firefox-to-emacs-native-messenger-tempfiles-[0-9]+/tmp_[A-Za-z0-9._]*_[A-Za-z0-9]+\\.txt\\'"
        v)))

(defun firefox-to-emacs-native-messenger-test--match-version-string (v)
  "Match `version-string': non-empty semver-shaped string."
  (and (stringp v) (string-match-p "\\`[0-9]+\\.[0-9]+\\.[0-9]+\\'" v)))

(defun firefox-to-emacs-native-messenger-test--match-nonempty-string (v)
  "Match `nonempty-string': any non-empty string."
  (and (stringp v) (> (length v) 0)))

(defun firefox-to-emacs-native-messenger-test--match-error-message (v)
  "Match `error-message': any non-empty string."
  (and (stringp v) (> (length v) 0)))

(defun firefox-to-emacs-native-messenger-test--match-pid (v)
  "Match `pid': any positive integer."
  (and (integerp v) (> v 0)))

(defun firefox-to-emacs-native-messenger-test--match-timestamp (v)
  "Match `timestamp': ISO 8601-shaped string."
  (and (stringp v)
       (string-match-p
        "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\(\\.[0-9]+\\)?\\(Z\\|[+-][0-9]\\{2\\}:[0-9]\\{2\\}\\)\\'"
        v)))

(defun firefox-to-emacs-native-messenger-test--match-exit-code (v)
  "Match `exit-code': integer in [-1, 255]."
  (and (integerp v) (>= v -1) (<= v 255)))

(defun firefox-to-emacs-native-messenger-test--match-nonzero-integer (v)
  "Match `nonzero-integer': any non-zero integer."
  (and (integerp v) (/= v 0)))

(defconst firefox-to-emacs-native-messenger-test-placeholder-types
  '(("path" . firefox-to-emacs-native-messenger-test--match-path)
    ("absolute-path" . firefox-to-emacs-native-messenger-test--match-absolute-path)
    ("temp-path" . firefox-to-emacs-native-messenger-test--match-temp-path)
    ("version-string" . firefox-to-emacs-native-messenger-test--match-version-string)
    ("nonempty-string" . firefox-to-emacs-native-messenger-test--match-nonempty-string)
    ("error-message" . firefox-to-emacs-native-messenger-test--match-error-message)
    ("pid" . firefox-to-emacs-native-messenger-test--match-pid)
    ("timestamp" . firefox-to-emacs-native-messenger-test--match-timestamp)
    ("exit-code" . firefox-to-emacs-native-messenger-test--match-exit-code)
    ("nonzero-integer" . firefox-to-emacs-native-messenger-test--match-nonzero-integer))
  "Mapping from placeholder type names (strings) to predicate functions.

Each predicate is called with one argument: the candidate value to be
matched against the placeholder.  See PROTOCOL.md Appendix A.4 for the
authoritative type vocabulary.")

(defun firefox-to-emacs-native-messenger-test--placeholder-p (value)
  "Return non-nil if VALUE is a `$schema'-form placeholder.

A placeholder is an alist with exactly one entry whose key is the symbol
`$schema' and whose value is a string naming a registered placeholder type."
  (and (consp value)
       (consp (car value))
       (eq (caar value) '$schema)
       (stringp (cdar value))
       (= (length value) 1)))

(defun firefox-to-emacs-native-messenger-test--alist-of-symbols-p (value)
  "Return non-nil if VALUE is a non-empty list whose entries are (SYMBOL . _)."
  (and (consp value)
       (cl-every (lambda (cell) (and (consp cell) (symbolp (car cell)))) value)))

(defun firefox-to-emacs-native-messenger-test--placeholder-match (placeholder actual)
  "Match ACTUAL against PLACEHOLDER's schema type.

PLACEHOLDER must satisfy
`firefox-to-emacs-native-messenger-test--placeholder-p'.  The schema name
must be present in
`firefox-to-emacs-native-messenger-test-placeholder-types'; otherwise an
error is signaled to flag a mistyped fixture."
  (let* ((type-name (cdar placeholder))
         (entry (assoc type-name
                       firefox-to-emacs-native-messenger-test-placeholder-types)))
    (unless entry
      (error "Unknown fixture placeholder type: %s" type-name))
    (funcall (cdr entry) actual)))

(defun firefox-to-emacs-native-messenger-test-fixture-equal-p (fixture actual)
  "Return non-nil if ACTUAL satisfies FIXTURE.

FIXTURE may contain `$schema'-form placeholders (see
`firefox-to-emacs-native-messenger-test--placeholder-p').  Each placeholder
matches any ACTUAL value satisfying the corresponding predicate in
`firefox-to-emacs-native-messenger-test-placeholder-types'.

For alists (lists whose entries are (SYMBOL . VALUE) cons cells), keys are
compared as a set: missing or extra keys cause failure.  For other lists,
elements are compared positionally with the same recursion.  Atoms are
compared with `equal'."
  (cond
   ((firefox-to-emacs-native-messenger-test--placeholder-p fixture)
    (firefox-to-emacs-native-messenger-test--placeholder-match fixture actual))
   ((and (firefox-to-emacs-native-messenger-test--alist-of-symbols-p fixture)
         (firefox-to-emacs-native-messenger-test--alist-of-symbols-p actual))
    (and (= (length fixture) (length actual))
         (cl-every
          (lambda (kv)
            (let ((cell (assq (car kv) actual)))
              (and cell
                   (firefox-to-emacs-native-messenger-test-fixture-equal-p
                    (cdr kv) (cdr cell)))))
          fixture)))
   ((and (consp fixture) (consp actual))
    (and (= (length fixture) (length actual))
         (cl-every #'firefox-to-emacs-native-messenger-test-fixture-equal-p
                   fixture actual)))
   (t (equal fixture actual))))

(defun firefox-to-emacs-native-messenger-test-run-shell-command (command-string)
  "Run COMMAND-STRING in a subprocess and return (:exit-status N :stdout S).

The subprocess is launched synchronously via `call-process' as
`setsid -- /bin/sh -c COMMAND-STRING'.  Stderr is merged into stdout
because `call-process' with a buffer DESTINATION captures both into the
same buffer when DESTINATION is a single buffer.  Returns a plist."
  (let ((output-buffer (generate-new-buffer
                        " *firefox-to-emacs-native-messenger-test-run*")))
    (unwind-protect
        (let ((exit (call-process "setsid" nil
                                  (list output-buffer t)
                                  nil
                                  "--" "/bin/sh" "-c" command-string)))
          (list :exit-status (if (integerp exit) exit -1)
                :stdout (with-current-buffer output-buffer
                          (buffer-string))))
      (when (buffer-live-p output-buffer)
        (kill-buffer output-buffer)))))

(ert-deftest firefox-to-emacs-native-messenger-test-load-fixture-version-success ()
  "load-fixture returns the `version-success' fixture as an alist.
Drilling into the response sub-alist, `cmd' MUST be \"version\" and `version'
MUST be a non-empty string."
  :tags '(:unit :fixture-driven)
  (let* ((fixture (firefox-to-emacs-native-messenger-test-load-fixture
                   "version-success"))
         (response (alist-get 'response fixture))
         (version-string (alist-get 'version response)))
    (should (equal (alist-get 'cmd response) "version"))
    (should (stringp version-string))
    (should (> (length version-string) 0))))

(ert-deftest firefox-to-emacs-native-messenger-test-with-sandbox-tempdir-basic ()
  "`with-sandbox-tempdir' supplies a 0700 dir cleaned up on exit.
Inside the body, the bound symbol names an existing directory whose mode is
exactly 0700.  After body exit, that directory no longer exists."
  :tags '(:unit :sandbox)
  (let (captured-path)
    (firefox-to-emacs-native-messenger-test-with-sandbox-tempdir
     dir
     (setq captured-path dir)
     (should (file-directory-p dir))
     (should (= (logand (file-modes dir) #o7777) #o700)))
    (should captured-path)
    (should-not (file-exists-p captured-path))))

(ert-deftest firefox-to-emacs-native-messenger-test-send-frame-roundtrip ()
  "`send-frame' round-trips a framed request to a stub echo listener.
Within a sandbox tempdir, a Unix-domain socket is bound by
`firefox-to-emacs-native-messenger-test-stub-echo-listener', preloaded with a
fixed reply.  `firefox-to-emacs-native-messenger-test-send-frame' sends a
framed JSON request and reads back the framed reply, which must parse equal to
the configured reply."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test-with-sandbox-tempdir dir
    (let* ((socket-path (expand-file-name "stub.sock" dir))
           (reply '((cmd . "version") (version . "0.0.0") (code . 0)))
           (request '((cmd . "version")))
           (listener (firefox-to-emacs-native-messenger-test-stub-echo-listener
                      socket-path reply)))
      (unwind-protect
          (let ((received (firefox-to-emacs-native-messenger-test-send-frame
                           socket-path request)))
            (should (equal received reply)))
        (when (process-live-p listener)
          (delete-process listener))))))

(ert-deftest firefox-to-emacs-native-messenger-test-fixture-equal-p-literals ()
  "literal alists must match byte-for-byte under fixture-equal-p."
  :tags '(:unit :fixture-driven)
  (should (firefox-to-emacs-native-messenger-test-fixture-equal-p
           '((cmd . "version") (code . 0))
           '((cmd . "version") (code . 0))))
  (should-not (firefox-to-emacs-native-messenger-test-fixture-equal-p
               '((cmd . "version") (code . 0))
               '((cmd . "version") (code . 1)))))

(ert-deftest firefox-to-emacs-native-messenger-test-fixture-equal-p-missing-extra-fields ()
  "missing or extra keys cause comparison failure."
  :tags '(:unit :fixture-driven)
  (should-not (firefox-to-emacs-native-messenger-test-fixture-equal-p
               '((cmd . "version") (code . 0))
               '((cmd . "version"))))
  (should-not (firefox-to-emacs-native-messenger-test-fixture-equal-p
               '((cmd . "version"))
               '((cmd . "version") (extra . "x")))))

(ert-deftest firefox-to-emacs-native-messenger-test-fixture-equal-p-placeholder-path ()
  "a `path' placeholder matches any non-empty string."
  :tags '(:unit :fixture-driven)
  (should (firefox-to-emacs-native-messenger-test-fixture-equal-p
           '((content . (($schema . "path"))))
           '((content . "/tmp/anything"))))
  (should (firefox-to-emacs-native-messenger-test-fixture-equal-p
           '((content . (($schema . "path"))))
           '((content . "relative/path/ok"))))
  (should-not (firefox-to-emacs-native-messenger-test-fixture-equal-p
               '((content . (($schema . "path"))))
               '((content . "")))))

(ert-deftest firefox-to-emacs-native-messenger-test-fixture-equal-p-placeholder-absolute-path ()
  "an `absolute-path' placeholder requires a leading slash."
  :tags '(:unit :fixture-driven)
  (should (firefox-to-emacs-native-messenger-test-fixture-equal-p
           '((content . (($schema . "absolute-path"))))
           '((content . "/etc/hosts"))))
  (should-not (firefox-to-emacs-native-messenger-test-fixture-equal-p
               '((content . (($schema . "absolute-path"))))
               '((content . "relative/path")))))

(ert-deftest firefox-to-emacs-native-messenger-test-fixture-equal-p-placeholder-pid ()
  "a `pid' placeholder matches positive integers only."
  :tags '(:unit :fixture-driven)
  (should (firefox-to-emacs-native-messenger-test-fixture-equal-p
           '((pid . (($schema . "pid"))))
           '((pid . 12345))))
  (should-not (firefox-to-emacs-native-messenger-test-fixture-equal-p
               '((pid . (($schema . "pid"))))
               '((pid . 0))))
  (should-not (firefox-to-emacs-native-messenger-test-fixture-equal-p
               '((pid . (($schema . "pid"))))
               '((pid . -1))))
  (should-not (firefox-to-emacs-native-messenger-test-fixture-equal-p
               '((pid . (($schema . "pid"))))
               '((pid . "12345")))))

(ert-deftest firefox-to-emacs-native-messenger-test-fixture-equal-p-placeholder-timestamp ()
  "a `timestamp' placeholder requires an ISO-8601-shaped string."
  :tags '(:unit :fixture-driven)
  (should (firefox-to-emacs-native-messenger-test-fixture-equal-p
           '((ts . (($schema . "timestamp"))))
           '((ts . "2026-05-13T20:00:00Z"))))
  (should (firefox-to-emacs-native-messenger-test-fixture-equal-p
           '((ts . (($schema . "timestamp"))))
           '((ts . "2026-05-13T20:00:00.123-04:00"))))
  (should-not (firefox-to-emacs-native-messenger-test-fixture-equal-p
               '((ts . (($schema . "timestamp"))))
               '((ts . "yesterday")))))

(ert-deftest firefox-to-emacs-native-messenger-test-fixture-equal-p-placeholder-exit-code ()
  "an `exit-code' placeholder matches integers in [-1, 255]."
  :tags '(:unit :fixture-driven)
  (should (firefox-to-emacs-native-messenger-test-fixture-equal-p
           '((code . (($schema . "exit-code"))))
           '((code . 0))))
  (should (firefox-to-emacs-native-messenger-test-fixture-equal-p
           '((code . (($schema . "exit-code"))))
           '((code . 255))))
  (should (firefox-to-emacs-native-messenger-test-fixture-equal-p
           '((code . (($schema . "exit-code"))))
           '((code . -1))))
  (should-not (firefox-to-emacs-native-messenger-test-fixture-equal-p
               '((code . (($schema . "exit-code"))))
               '((code . 256))))
  (should-not (firefox-to-emacs-native-messenger-test-fixture-equal-p
               '((code . (($schema . "exit-code"))))
               '((code . -2)))))

(ert-deftest firefox-to-emacs-native-messenger-test-fixture-equal-p-nested ()
  "placeholders work inside nested alists."
  :tags '(:unit :fixture-driven)
  (should (firefox-to-emacs-native-messenger-test-fixture-equal-p
           '((response . ((cmd . "temp") (content . (($schema . "absolute-path"))) (code . 0))))
           '((response . ((cmd . "temp") (content . "/tmp/foo") (code . 0))))))
  (should-not (firefox-to-emacs-native-messenger-test-fixture-equal-p
               '((response . ((cmd . "temp") (content . (($schema . "absolute-path"))) (code . 0))))
               '((response . ((cmd . "temp") (content . "/tmp/foo") (code . 1)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-shell-command-basic ()
  "`run-shell-command' returns exit 0 and stdout for `echo hello'."
  :tags '(:integration :run-subprocess)
  (let ((result (firefox-to-emacs-native-messenger-test-run-shell-command
                 "echo hello")))
    (should (= (plist-get result :exit-status) 0))
    (should (equal (plist-get result :stdout) "hello\n"))))

(provide 'firefox-to-emacs-native-messenger-tests)
;;; firefox-to-emacs-native-messenger-tests.el ends here
