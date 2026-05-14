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
(require 'firefox-to-emacs-native-messenger)

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

(ert-deftest firefox-to-emacs-native-messenger-test-run-whitelist-defcustom-exists ()
  "`run-whitelist' is registered as a defcustom with default nil in our group."
  :tags '(:unit)
  (should (custom-variable-p 'firefox-to-emacs-native-messenger-run-whitelist))
  (should (eq (default-value 'firefox-to-emacs-native-messenger-run-whitelist) nil))
  (should (member '(firefox-to-emacs-native-messenger-run-whitelist custom-variable)
                  (get 'firefox-to-emacs-native-messenger 'custom-group))))

(ert-deftest firefox-to-emacs-native-messenger-test-read-whitelist-defcustom-exists ()
  "`read-whitelist' is registered as a defcustom with default nil in our group."
  :tags '(:unit)
  (should (custom-variable-p 'firefox-to-emacs-native-messenger-read-whitelist))
  (should (eq (default-value 'firefox-to-emacs-native-messenger-read-whitelist) nil))
  (should (member '(firefox-to-emacs-native-messenger-read-whitelist custom-variable)
                  (get 'firefox-to-emacs-native-messenger 'custom-group))))

(ert-deftest firefox-to-emacs-native-messenger-test-temp-registry-cap-defcustom-exists ()
  "`temp-registry-cap' is registered as a defcustom with default 1024 in our group."
  :tags '(:unit)
  (should (custom-variable-p 'firefox-to-emacs-native-messenger-temp-registry-cap))
  (should (= (default-value 'firefox-to-emacs-native-messenger-temp-registry-cap) 1024))
  (should (member '(firefox-to-emacs-native-messenger-temp-registry-cap custom-variable)
                  (get 'firefox-to-emacs-native-messenger 'custom-group))))

(ert-deftest firefox-to-emacs-native-messenger-test-capability-registry-init ()
  "The capability registry exists as an empty `equal'-tested hash table.
This assertion captures the registry's state at module load.  Other tests
that mutate the registry MUST restore the empty state via `unwind-protect'."
  :tags '(:unit)
  (should (boundp 'firefox-to-emacs-native-messenger--capability-registry))
  (should (hash-table-p firefox-to-emacs-native-messenger--capability-registry))
  (should (eq (hash-table-test
               firefox-to-emacs-native-messenger--capability-registry)
              'equal))
  (should (= (hash-table-count
              firefox-to-emacs-native-messenger--capability-registry)
             0)))

(ert-deftest firefox-to-emacs-native-messenger-test-error-hierarchy ()
  "Every child error condition inherits from the bridge's parent error.
Each child's `error-conditions' list MUST contain the child itself, the
parent `firefox-to-emacs-native-messenger-error', and the root `error'."
  :tags '(:unit)
  (let ((children '(firefox-to-emacs-native-messenger-bad-request
                    firefox-to-emacs-native-messenger-frame-too-large
                    firefox-to-emacs-native-messenger-frame-parse-error
                    firefox-to-emacs-native-messenger-unsupported-command
                    firefox-to-emacs-native-messenger-handler-error
                    firefox-to-emacs-native-messenger-bad-state
                    firefox-to-emacs-native-messenger-whitelist-rejection
                    firefox-to-emacs-native-messenger-whitelist-malformed)))
    (should (consp (get 'firefox-to-emacs-native-messenger-error 'error-conditions)))
    (should (stringp (get 'firefox-to-emacs-native-messenger-error 'error-message)))
    (dolist (child children)
      (let ((conds (get child 'error-conditions)))
        (should conds)
        (should (memq child conds))
        (should (memq 'firefox-to-emacs-native-messenger-error conds))
        (should (memq 'error conds))
        (should (stringp (get child 'error-message)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-pack-length-produces-4-bytes ()
  "`pack-length' always returns exactly four bytes (a unibyte string)."
  :tags '(:unit)
  (dolist (n '(0 1 65536 16777216 2147483647 4294967295))
    (let ((bytes (firefox-to-emacs-native-messenger--pack-length n)))
      (should (stringp bytes))
      (should-not (multibyte-string-p bytes))
      (should (= (length bytes) 4)))))

(ert-deftest firefox-to-emacs-native-messenger-test-pack-length-little-endian ()
  "`pack-length' uses little-endian byte order per CON-0800."
  :tags '(:unit)
  (should (equal (firefox-to-emacs-native-messenger--pack-length 1)
                 (unibyte-string 1 0 0 0)))
  (should (equal (firefox-to-emacs-native-messenger--pack-length 256)
                 (unibyte-string 0 1 0 0)))
  (should (equal (firefox-to-emacs-native-messenger--pack-length 16777216)
                 (unibyte-string 0 0 0 1))))

(ert-deftest firefox-to-emacs-native-messenger-test-codec-roundtrip ()
  "Every boundary value round-trips identically through pack+unpack."
  :tags '(:unit)
  (dolist (n (list 0 1 65536 16777216 (1- (expt 2 31))
                   firefox-to-emacs-native-messenger-inbound-frame-cap
                   (1- (expt 2 32))))
    (let ((roundtripped (firefox-to-emacs-native-messenger--unpack-length
                         (firefox-to-emacs-native-messenger--pack-length n))))
      (should (= roundtripped n)))))

(defmacro firefox-to-emacs-native-messenger-test--with-fresh-log-buffer (&rest body)
  "Rebind the log buffer name to a unique temp name for BODY; clean up after."
  (declare (indent 0) (debug (body)))
  `(let* ((unique-name (generate-new-buffer-name
                        "*firefox-to-emacs-native-messenger-test-log*"))
          (firefox-to-emacs-native-messenger-log-buffer-name unique-name))
     (unwind-protect
         (progn ,@body)
       (when (get-buffer unique-name)
         (kill-buffer unique-name)))))

(ert-deftest firefox-to-emacs-native-messenger-test-log-creates-buffer-and-writes ()
  "Logger creates the log buffer when absent and writes the formatted message."
  :tags '(:unit)
  (firefox-to-emacs-native-messenger-test--with-fresh-log-buffer
   (should-not (get-buffer firefox-to-emacs-native-messenger-log-buffer-name))
   (firefox-to-emacs-native-messenger--log 'info "hello %s" "world")
   (let ((buf (get-buffer firefox-to-emacs-native-messenger-log-buffer-name)))
     (should buf)
     (with-current-buffer buf
       (should (string-match-p "hello world" (buffer-string)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-log-respects-level ()
  "Records below the configured level are dropped; equal-or-above are kept."
  :tags '(:unit)
  (firefox-to-emacs-native-messenger-test--with-fresh-log-buffer
   (let ((firefox-to-emacs-native-messenger-log-level 'warn))
     (firefox-to-emacs-native-messenger--log 'debug "debug-line-marker")
     (firefox-to-emacs-native-messenger--log 'info "info-line-marker")
     (firefox-to-emacs-native-messenger--log 'warn "warn-line-marker")
     (firefox-to-emacs-native-messenger--log 'error "error-line-marker")
     (let* ((buf (get-buffer firefox-to-emacs-native-messenger-log-buffer-name))
            (contents (and buf (with-current-buffer buf (buffer-string)))))
       (should buf)
       (should-not (string-match-p "debug-line-marker" contents))
       (should-not (string-match-p "info-line-marker" contents))
       (should (string-match-p "warn-line-marker" contents))
       (should (string-match-p "error-line-marker" contents))))))

(ert-deftest firefox-to-emacs-native-messenger-test-log-never-raises ()
  "Logger swallows internal errors per GUD-400.
A bad format directive, a malformed argument list, and an unrecognized
level symbol all MUST be silently no-oped rather than propagated."
  :tags '(:unit)
  (firefox-to-emacs-native-messenger-test--with-fresh-log-buffer
   (should-not (condition-case _err
                   (progn
                     (firefox-to-emacs-native-messenger--log
                      'info "%d" "not-int")
                     nil)
                 (error t)))
   (should-not (condition-case _err
                   (progn
                     (firefox-to-emacs-native-messenger--log
                      'nonsense-level "x")
                     nil)
                 (error t)))
   (should-not (condition-case _err
                   (progn
                     (firefox-to-emacs-native-messenger--log
                      'info "%s %s" "only-one-arg")
                     nil)
                 (error t)))))

(ert-deftest firefox-to-emacs-native-messenger-test-glob-literal-no-globs ()
  "A pattern with no glob characters matches only the identical string."
  :tags '(:unit)
  (should (firefox-to-emacs-native-messenger--glob-match-p "foo" "foo"))
  (should-not (firefox-to-emacs-native-messenger--glob-match-p "foo" "foob"))
  (should-not (firefox-to-emacs-native-messenger--glob-match-p "foo" "afoo"))
  (should-not (firefox-to-emacs-native-messenger--glob-match-p "foo" "bar"))
  (should (firefox-to-emacs-native-messenger--glob-match-p
           "/tmp/file.txt" "/tmp/file.txt"))
  (should-not (firefox-to-emacs-native-messenger--glob-match-p
               "/tmp/file.txt" "/tmp/file.txt.bak")))

(ert-deftest firefox-to-emacs-native-messenger-test-glob-star-non-slash ()
  "`*' matches a (possibly empty) sequence of non-slash characters."
  :tags '(:unit)
  (should (firefox-to-emacs-native-messenger--glob-match-p "*" "foo"))
  (should (firefox-to-emacs-native-messenger--glob-match-p "*.txt" "foo.txt"))
  (should (firefox-to-emacs-native-messenger--glob-match-p "*.txt" ".txt"))
  (should-not (firefox-to-emacs-native-messenger--glob-match-p "*" "a/b"))
  (should-not (firefox-to-emacs-native-messenger--glob-match-p
               "*.txt" "a/b.txt"))
  (should (firefox-to-emacs-native-messenger--glob-match-p
           "/tmp/*.txt" "/tmp/foo.txt"))
  (should-not (firefox-to-emacs-native-messenger--glob-match-p
               "/tmp/*.txt" "/tmp/sub/foo.txt")))

(ert-deftest firefox-to-emacs-native-messenger-test-glob-double-star-cross-slash ()
  "`**' matches any sequence including slashes."
  :tags '(:unit)
  (should (firefox-to-emacs-native-messenger--glob-match-p "**" "foo"))
  (should (firefox-to-emacs-native-messenger--glob-match-p "**" "a/b/c"))
  (should (firefox-to-emacs-native-messenger--glob-match-p "/tmp/**" "/tmp/foo"))
  (should (firefox-to-emacs-native-messenger--glob-match-p
           "/tmp/**" "/tmp/sub/foo"))
  (should (firefox-to-emacs-native-messenger--glob-match-p
           "/tmp/**.txt" "/tmp/sub/foo.txt")))

(ert-deftest firefox-to-emacs-native-messenger-test-glob-question-mark ()
  "`?' matches exactly one non-slash character."
  :tags '(:unit)
  (should (firefox-to-emacs-native-messenger--glob-match-p "a?b" "axb"))
  (should-not (firefox-to-emacs-native-messenger--glob-match-p "a?b" "ab"))
  (should-not (firefox-to-emacs-native-messenger--glob-match-p "a?b" "axxb"))
  (should-not (firefox-to-emacs-native-messenger--glob-match-p "a?b" "a/b")))

(ert-deftest firefox-to-emacs-native-messenger-test-glob-full-string-anchored ()
  "Matches are full-string anchored: no prefix or suffix slop is allowed."
  :tags '(:unit)
  (should-not (firefox-to-emacs-native-messenger--glob-match-p "foo" "foobar"))
  (should-not (firefox-to-emacs-native-messenger--glob-match-p "foo" "barfoo"))
  (should-not (firefox-to-emacs-native-messenger--glob-match-p
               "bar" "foobarbaz"))
  (should (firefox-to-emacs-native-messenger--glob-match-p
           "/a/b/c" "/a/b/c")))

(ert-deftest firefox-to-emacs-native-messenger-test-glob-empty-edge-cases ()
  "Empty pattern matches only empty candidate; `*' and `**' match empty."
  :tags '(:unit)
  (should (firefox-to-emacs-native-messenger--glob-match-p "" ""))
  (should-not (firefox-to-emacs-native-messenger--glob-match-p "" "x"))
  (should-not (firefox-to-emacs-native-messenger--glob-match-p "x" ""))
  (should (firefox-to-emacs-native-messenger--glob-match-p "*" ""))
  (should (firefox-to-emacs-native-messenger--glob-match-p "**" "")))

(ert-deftest firefox-to-emacs-native-messenger-test-validate-whitelist-nil-and-empty ()
  "nil and the empty list are accepted as deny-all for both handler kinds."
  :tags '(:unit)
  (dolist (kind '(run read))
    (should (eq (firefox-to-emacs-native-messenger--validate-whitelist
                 nil kind)
                t))
    (should (eq (firefox-to-emacs-native-messenger--validate-whitelist
                 '() kind)
                t))))

(ert-deftest firefox-to-emacs-native-messenger-test-validate-whitelist-allow-all ()
  "(\"*\") is the allow-all sentinel; mixing it with anything else is rejected."
  :tags '(:unit)
  (dolist (kind '(run read))
    (should (eq (firefox-to-emacs-native-messenger--validate-whitelist
                 '("*") kind)
                t))
    (should-error (firefox-to-emacs-native-messenger--validate-whitelist
                   '("*" "/foo") kind)
                  :type 'firefox-to-emacs-native-messenger-whitelist-malformed)
    (should-error (firefox-to-emacs-native-messenger--validate-whitelist
                   '("/foo" "*") kind)
                  :type 'firefox-to-emacs-native-messenger-whitelist-malformed)))

(ert-deftest firefox-to-emacs-native-messenger-test-validate-whitelist-non-list-top-level ()
  "Non-list top-level values (string, vector, integer, symbol) are rejected."
  :tags '(:unit)
  (dolist (kind '(run read))
    (dolist (val '("a-string" [1 2 3] 42 some-symbol))
      (should-error (firefox-to-emacs-native-messenger--validate-whitelist
                     val kind)
                    :type 'firefox-to-emacs-native-messenger-whitelist-malformed))))

(ert-deftest firefox-to-emacs-native-messenger-test-validate-whitelist-non-string-entries ()
  "Lists containing non-strings (numbers, lists, vectors, nil) are rejected."
  :tags '(:unit)
  (dolist (kind '(run read))
    (dolist (val (list '("/ok" 42)
                       '("/ok" (nested list))
                       (list "/ok" [1 2])
                       '("/ok" nil)))
      (should-error (firefox-to-emacs-native-messenger--validate-whitelist
                     val kind)
                    :type 'firefox-to-emacs-native-messenger-whitelist-malformed))))

(ert-deftest firefox-to-emacs-native-messenger-test-validate-whitelist-empty-string ()
  "Empty-string entries are rejected for both handler kinds."
  :tags '(:unit)
  (dolist (kind '(run read))
    (should-error (firefox-to-emacs-native-messenger--validate-whitelist
                   '("") kind)
                  :type 'firefox-to-emacs-native-messenger-whitelist-malformed)
    (should-error (firefox-to-emacs-native-messenger--validate-whitelist
                   '("/ok" "") kind)
                  :type 'firefox-to-emacs-native-messenger-whitelist-malformed)))

(ert-deftest firefox-to-emacs-native-messenger-test-validate-whitelist-control-chars ()
  "Entries containing newline or null bytes are rejected."
  :tags '(:unit)
  (dolist (kind '(run read))
    (should-error (firefox-to-emacs-native-messenger--validate-whitelist
                   '("/contains\nnewline") kind)
                  :type 'firefox-to-emacs-native-messenger-whitelist-malformed)
    (should-error (firefox-to-emacs-native-messenger--validate-whitelist
                   (list (concat "/contains" (string 0) "null")) kind)
                  :type 'firefox-to-emacs-native-messenger-whitelist-malformed)))

(ert-deftest firefox-to-emacs-native-messenger-test-validate-whitelist-read-valid ()
  "Read entries: absolute paths, glob paths, and the literal <TEMP-PATH> token."
  :tags '(:unit)
  (should (eq (firefox-to-emacs-native-messenger--validate-whitelist
               '("/etc/hosts"
                 "/home/u/*.txt"
                 "<TEMP-PATH>"
                 "/srv/**.txt"
                 "/single?char")
               'read)
              t)))

(ert-deftest firefox-to-emacs-native-messenger-test-validate-whitelist-read-rejects-relative ()
  "Read entries that are neither absolute paths nor the <TEMP-PATH> token are rejected."
  :tags '(:unit)
  (should-error (firefox-to-emacs-native-messenger--validate-whitelist
                 '("foo/bar") 'read)
                :type 'firefox-to-emacs-native-messenger-whitelist-malformed)
  (should-error (firefox-to-emacs-native-messenger--validate-whitelist
                 '("./relative.txt") 'read)
                :type 'firefox-to-emacs-native-messenger-whitelist-malformed)
  (should-error (firefox-to-emacs-native-messenger--validate-whitelist
                 '("noslashes") 'read)
                :type 'firefox-to-emacs-native-messenger-whitelist-malformed)
  (should-error (firefox-to-emacs-native-messenger--validate-whitelist
                 '("<temp-path>") 'read)
                :type 'firefox-to-emacs-native-messenger-whitelist-malformed))

(ert-deftest firefox-to-emacs-native-messenger-test-validate-whitelist-run-valid ()
  "Run entries: literal commands and <TEMP-PATH>-templated commands accepted."
  :tags '(:unit)
  (should (eq (firefox-to-emacs-native-messenger--validate-whitelist
               '("emacsclient <TEMP-PATH>"
                 "rm -f '<TEMP-PATH>'"
                 "ls /home"
                 "cp <TEMP-PATH> <TEMP-PATH>"
                 "a<TEMP-PATH>b<TEMP-PATH>c")
               'run)
              t)))

(ert-deftest firefox-to-emacs-native-messenger-test-validate-whitelist-run-typo-guard ()
  "Run entries with any non-<TEMP-PATH> <...> token are rejected (typo guard)."
  :tags '(:unit)
  (should-error (firefox-to-emacs-native-messenger--validate-whitelist
                 '("rm <TMP-PATH>") 'run)
                :type 'firefox-to-emacs-native-messenger-whitelist-malformed)
  (should-error (firefox-to-emacs-native-messenger--validate-whitelist
                 '("rm <temp-path>") 'run)
                :type 'firefox-to-emacs-native-messenger-whitelist-malformed)
  (should-error (firefox-to-emacs-native-messenger--validate-whitelist
                 '("rm <FOO>") 'run)
                :type 'firefox-to-emacs-native-messenger-whitelist-malformed))

(ert-deftest firefox-to-emacs-native-messenger-test-validate-whitelist-run-adjacent-markers ()
  "Run entries with two adjacent <TEMP-PATH> markers are rejected."
  :tags '(:unit)
  (should-error (firefox-to-emacs-native-messenger--validate-whitelist
                 '("<TEMP-PATH><TEMP-PATH>") 'run)
                :type 'firefox-to-emacs-native-messenger-whitelist-malformed)
  (should-error (firefox-to-emacs-native-messenger--validate-whitelist
                 '("prefix <TEMP-PATH><TEMP-PATH> suffix") 'run)
                :type 'firefox-to-emacs-native-messenger-whitelist-malformed)
  (should (eq (firefox-to-emacs-native-messenger--validate-whitelist
               '("a<TEMP-PATH>b<TEMP-PATH>c") 'run)
              t)))

(defmacro firefox-to-emacs-native-messenger-test--with-saved-whitelists (&rest body)
  "Save the bridge's whitelist defcustom values, run BODY, restore on exit.
Restoration uses `setq', which re-invokes the watcher; the originally
captured values are therefore expected to be well-formed."
  (declare (indent 0) (debug (body)))
  `(let ((orig-run firefox-to-emacs-native-messenger-run-whitelist)
         (orig-read firefox-to-emacs-native-messenger-read-whitelist))
     (unwind-protect
         (progn ,@body)
       (setq firefox-to-emacs-native-messenger-run-whitelist orig-run)
       (setq firefox-to-emacs-native-messenger-read-whitelist orig-read))))

(ert-deftest firefox-to-emacs-native-messenger-test-watcher-accepts-wellformed-setq ()
  "Setq of a well-formed whitelist value succeeds without raising."
  :tags '(:unit)
  (firefox-to-emacs-native-messenger-test--with-saved-whitelists
   (setq firefox-to-emacs-native-messenger-run-whitelist nil)
   (should (eq firefox-to-emacs-native-messenger-run-whitelist nil))
   (setq firefox-to-emacs-native-messenger-run-whitelist
         '("emacsclient <TEMP-PATH>"))
   (should (equal firefox-to-emacs-native-messenger-run-whitelist
                  '("emacsclient <TEMP-PATH>")))
   (setq firefox-to-emacs-native-messenger-run-whitelist '("*"))
   (should (equal firefox-to-emacs-native-messenger-run-whitelist '("*")))
   (setq firefox-to-emacs-native-messenger-read-whitelist
         '("<TEMP-PATH>" "/etc/hosts"))
   (should (equal firefox-to-emacs-native-messenger-read-whitelist
                  '("<TEMP-PATH>" "/etc/hosts")))))

(ert-deftest firefox-to-emacs-native-messenger-test-watcher-rejects-malformed-setq ()
  "Setq of a malformed whitelist value signals `whitelist-malformed' and
leaves the variable's prior value unchanged."
  :tags '(:unit)
  (firefox-to-emacs-native-messenger-test--with-saved-whitelists
   (setq firefox-to-emacs-native-messenger-run-whitelist
         '("emacsclient <TEMP-PATH>"))
   (setq firefox-to-emacs-native-messenger-read-whitelist '("<TEMP-PATH>"))
   (should-error
    (setq firefox-to-emacs-native-messenger-run-whitelist
          '("rm <TMP-PATH>"))
    :type 'firefox-to-emacs-native-messenger-whitelist-malformed)
   (should (equal firefox-to-emacs-native-messenger-run-whitelist
                  '("emacsclient <TEMP-PATH>")))
   (should-error
    (setq firefox-to-emacs-native-messenger-read-whitelist
          '("relative/path"))
    :type 'firefox-to-emacs-native-messenger-whitelist-malformed)
   (should (equal firefox-to-emacs-native-messenger-read-whitelist
                  '("<TEMP-PATH>")))
   (should-error
    (setq firefox-to-emacs-native-messenger-run-whitelist '("*" "/foo"))
    :type 'firefox-to-emacs-native-messenger-whitelist-malformed)
   (should (equal firefox-to-emacs-native-messenger-run-whitelist
                  '("emacsclient <TEMP-PATH>")))))

(ert-deftest firefox-to-emacs-native-messenger-test-watcher-idempotent-registration ()
  "Re-loading the production module does not stack additional watchers.
Watchers MUST be installed via a named function and registered idempotently."
  :tags '(:unit)
  (let* ((before-run (get-variable-watchers
                      'firefox-to-emacs-native-messenger-run-whitelist))
         (before-read (get-variable-watchers
                       'firefox-to-emacs-native-messenger-read-whitelist)))
    (load (locate-library "firefox-to-emacs-native-messenger")
          nil 'nomessage)
    (let ((after-run (get-variable-watchers
                      'firefox-to-emacs-native-messenger-run-whitelist))
          (after-read (get-variable-watchers
                       'firefox-to-emacs-native-messenger-read-whitelist)))
      (should (equal before-run after-run))
      (should (equal before-read after-read))
      (should (= (length after-run) 1))
      (should (= (length after-read) 1)))))

(defmacro firefox-to-emacs-native-messenger-test--with-saved-registry-cap (&rest body)
  "Save `temp-registry-cap', run BODY, restore on exit via setq."
  (declare (indent 0) (debug (body)))
  `(let ((orig firefox-to-emacs-native-messenger-temp-registry-cap))
     (unwind-protect
         (progn ,@body)
       (setq firefox-to-emacs-native-messenger-temp-registry-cap orig))))

(ert-deftest firefox-to-emacs-native-messenger-test-set-slot-wired ()
  "The :set slot is wired on each whitelist and the registry-cap defcustom."
  :tags '(:unit)
  (should (get 'firefox-to-emacs-native-messenger-run-whitelist 'custom-set))
  (should (get 'firefox-to-emacs-native-messenger-read-whitelist 'custom-set))
  (should (get 'firefox-to-emacs-native-messenger-temp-registry-cap 'custom-set)))

(ert-deftest firefox-to-emacs-native-messenger-test-customize-whitelist-wellformed ()
  "`customize-set-variable' of a well-formed whitelist value succeeds."
  :tags '(:unit)
  (firefox-to-emacs-native-messenger-test--with-saved-whitelists
   (customize-set-variable 'firefox-to-emacs-native-messenger-run-whitelist
                            '("emacsclient <TEMP-PATH>"))
   (should (equal firefox-to-emacs-native-messenger-run-whitelist
                  '("emacsclient <TEMP-PATH>")))
   (customize-set-variable 'firefox-to-emacs-native-messenger-read-whitelist
                            '("<TEMP-PATH>"))
   (should (equal firefox-to-emacs-native-messenger-read-whitelist
                  '("<TEMP-PATH>")))))

(ert-deftest firefox-to-emacs-native-messenger-test-customize-whitelist-malformed ()
  "`customize-set-variable' of malformed value raises and leaves value unchanged."
  :tags '(:unit)
  (firefox-to-emacs-native-messenger-test--with-saved-whitelists
   (setq firefox-to-emacs-native-messenger-run-whitelist
         '("emacsclient <TEMP-PATH>"))
   (setq firefox-to-emacs-native-messenger-read-whitelist '("<TEMP-PATH>"))
   (should-error
    (customize-set-variable
     'firefox-to-emacs-native-messenger-run-whitelist '("rm <TMP-PATH>"))
    :type 'firefox-to-emacs-native-messenger-whitelist-malformed)
   (should (equal firefox-to-emacs-native-messenger-run-whitelist
                  '("emacsclient <TEMP-PATH>")))
   (should-error
    (customize-set-variable
     'firefox-to-emacs-native-messenger-read-whitelist '("relative"))
    :type 'firefox-to-emacs-native-messenger-whitelist-malformed)
   (should (equal firefox-to-emacs-native-messenger-read-whitelist
                  '("<TEMP-PATH>")))))

(ert-deftest firefox-to-emacs-native-messenger-test-customize-registry-cap-wellformed ()
  "`customize-set-variable' of a positive integer for the cap succeeds."
  :tags '(:unit)
  (firefox-to-emacs-native-messenger-test--with-saved-registry-cap
   (customize-set-variable 'firefox-to-emacs-native-messenger-temp-registry-cap 2048)
   (should (= firefox-to-emacs-native-messenger-temp-registry-cap 2048))
   (customize-set-variable 'firefox-to-emacs-native-messenger-temp-registry-cap 1)
   (should (= firefox-to-emacs-native-messenger-temp-registry-cap 1))))

(ert-deftest firefox-to-emacs-native-messenger-test-customize-registry-cap-malformed ()
  "Non-positive integers and non-integers are rejected by the cap's :set slot."
  :tags '(:unit)
  (firefox-to-emacs-native-messenger-test--with-saved-registry-cap
   (setq firefox-to-emacs-native-messenger-temp-registry-cap 1024)
   (should-error
    (customize-set-variable
     'firefox-to-emacs-native-messenger-temp-registry-cap -1))
   (should (= firefox-to-emacs-native-messenger-temp-registry-cap 1024))
   (should-error
    (customize-set-variable
     'firefox-to-emacs-native-messenger-temp-registry-cap 0))
   (should (= firefox-to-emacs-native-messenger-temp-registry-cap 1024))
   (should-error
    (customize-set-variable
     'firefox-to-emacs-native-messenger-temp-registry-cap "string"))
   (should (= firefox-to-emacs-native-messenger-temp-registry-cap 1024))))

(defmacro firefox-to-emacs-native-messenger-test--with-stub-registry (registered-paths &rest body)
  "Stub `--registry-contains-p' to accept only REGISTERED-PATHS during BODY.
REGISTERED-PATHS is a list of absolute path strings."
  (declare (indent 1) (debug (form body)))
  `(cl-letf (((symbol-function
               'firefox-to-emacs-native-messenger--registry-contains-p)
              (let ((paths ,registered-paths))
                (lambda (p) (and (member p paths) t)))))
     ,@body))

(ert-deftest firefox-to-emacs-native-messenger-test-command-gate-literal-no-markers ()
  "Entry with zero markers accepts iff candidate equals entry byte-for-byte."
  :tags '(:unit)
  (firefox-to-emacs-native-messenger-test--with-stub-registry '()
    (should (firefox-to-emacs-native-messenger--command-gate-match-p
             "ls /home" "ls /home"))
    (should-not (firefox-to-emacs-native-messenger--command-gate-match-p
                 "ls /home" "ls /home "))
    (should-not (firefox-to-emacs-native-messenger--command-gate-match-p
                 "ls /home" "ls /etc"))
    (should-not (firefox-to-emacs-native-messenger--command-gate-match-p
                 "ls /home" "ls /home extra"))))

(ert-deftest firefox-to-emacs-native-messenger-test-command-gate-single-marker ()
  "Single-marker entry matches iff prefix/suffix and registry hit are all true."
  :tags '(:unit)
  (firefox-to-emacs-native-messenger-test--with-stub-registry
      '("/tmp/firefox-to-emacs-native-messenger-tempfiles-1000/tmp_x.txt")
    (should (firefox-to-emacs-native-messenger--command-gate-match-p
             "emacsclient <TEMP-PATH>"
             "emacsclient /tmp/firefox-to-emacs-native-messenger-tempfiles-1000/tmp_x.txt"))
    (should-not (firefox-to-emacs-native-messenger--command-gate-match-p
                 "emacsclient <TEMP-PATH>"
                 "emacsclient /other/path"))
    (should-not (firefox-to-emacs-native-messenger--command-gate-match-p
                 "emacsclient <TEMP-PATH>"
                 "vim /tmp/firefox-to-emacs-native-messenger-tempfiles-1000/tmp_x.txt"))))

(ert-deftest firefox-to-emacs-native-messenger-test-command-gate-quoted-suffix ()
  "Entry with a non-empty trailing literal matches when suffix is present."
  :tags '(:unit)
  (firefox-to-emacs-native-messenger-test--with-stub-registry
      '("/tmp/foo")
    (should (firefox-to-emacs-native-messenger--command-gate-match-p
             "rm -f '<TEMP-PATH>'" "rm -f '/tmp/foo'"))
    (should-not (firefox-to-emacs-native-messenger--command-gate-match-p
                 "rm -f '<TEMP-PATH>'" "rm -f '/tmp/foo' extra"))
    (should-not (firefox-to-emacs-native-messenger--command-gate-match-p
                 "rm -f '<TEMP-PATH>'" "rm -f /tmp/foo"))))

(ert-deftest firefox-to-emacs-native-messenger-test-command-gate-trailing-bytes-reject ()
  "Trailing bytes after the matched template reject the candidate."
  :tags '(:unit)
  (firefox-to-emacs-native-messenger-test--with-stub-registry
      '("/tmp/foo")
    (should-not (firefox-to-emacs-native-messenger--command-gate-match-p
                 "emacsclient <TEMP-PATH>"
                 "emacsclient /tmp/foo ;rm -rf ~"))))

(ert-deftest firefox-to-emacs-native-messenger-test-command-gate-multi-marker-first-occurrence ()
  "Multi-marker entry uses first-occurrence search for interior literals."
  :tags '(:unit)
  (firefox-to-emacs-native-messenger-test--with-stub-registry
      '("/a" "/b")
    (should (firefox-to-emacs-native-messenger--command-gate-match-p
             "cp <TEMP-PATH> <TEMP-PATH>" "cp /a /b"))
    (should-not (firefox-to-emacs-native-messenger--command-gate-match-p
                 "cp <TEMP-PATH> <TEMP-PATH>" "cp /a /b /c")))
  (firefox-to-emacs-native-messenger-test--with-stub-registry
      '("/b /c")
    (should-not (firefox-to-emacs-native-messenger--command-gate-match-p
                 "cp <TEMP-PATH> <TEMP-PATH>" "cp /a /b /c"))))

(ert-deftest firefox-to-emacs-native-messenger-test-command-gate-marker-missing-from-registry ()
  "An extracted marker substring not in the registry rejects."
  :tags '(:unit)
  (firefox-to-emacs-native-messenger-test--with-stub-registry '()
    (should-not (firefox-to-emacs-native-messenger--command-gate-match-p
                 "emacsclient <TEMP-PATH>" "emacsclient /tmp/foo"))))

(ert-deftest firefox-to-emacs-native-messenger-test-command-gate-empty-marker-rejects ()
  "A marker that matches zero bytes rejects."
  :tags '(:unit)
  (firefox-to-emacs-native-messenger-test--with-stub-registry '("")
    (should-not (firefox-to-emacs-native-messenger--command-gate-match-p
                 "emacsclient <TEMP-PATH>" "emacsclient "))
    (should-not (firefox-to-emacs-native-messenger--command-gate-match-p
                 "rm -f '<TEMP-PATH>'" "rm -f ''"))))

(ert-deftest firefox-to-emacs-native-messenger-test-command-gate-multi-marker-missing-interior ()
  "Multi-marker entry rejects when the interior literal is missing in candidate."
  :tags '(:unit)
  (firefox-to-emacs-native-messenger-test--with-stub-registry '("/a" "/b")
    (should-not (firefox-to-emacs-native-messenger--command-gate-match-p
                 "cp <TEMP-PATH> <TEMP-PATH>" "cp/a/b"))))

(defmacro firefox-to-emacs-native-messenger-test--with-fresh-registry (&rest body)
  "Save the capability registry, run BODY, clear and restore on exit."
  (declare (indent 0) (debug (body)))
  `(let ((saved-entries
          (let (acc)
            (maphash
             (lambda (k v) (push (cons k v) acc))
             firefox-to-emacs-native-messenger--capability-registry)
            acc)))
     (clrhash firefox-to-emacs-native-messenger--capability-registry)
     (unwind-protect
         (progn ,@body)
       (clrhash firefox-to-emacs-native-messenger--capability-registry)
       (dolist (e saved-entries)
         (puthash (car e) (cdr e)
                  firefox-to-emacs-native-messenger--capability-registry)))))

(ert-deftest firefox-to-emacs-native-messenger-test-registry-register-stores-identity ()
  "`register' adds an entry whose value plist records dev/inode/uid."
  :tags '(:unit :sandbox)
  (firefox-to-emacs-native-messenger-test--with-fresh-registry
   (firefox-to-emacs-native-messenger-test-with-sandbox-tempdir dir
     (let ((path (expand-file-name "f.txt" dir)))
       (with-temp-file path (insert "x"))
       (firefox-to-emacs-native-messenger--registry-register path)
       (let ((value (gethash path
                             firefox-to-emacs-native-messenger--capability-registry))
             (attrs (file-attributes path)))
         (should (plistp value))
         (should (equal (plist-get value :dev)
                        (file-attribute-device-number attrs)))
         (should (equal (plist-get value :inode)
                        (file-attribute-inode-number attrs)))
         (should (equal (plist-get value :uid)
                        (file-attribute-user-id attrs))))))))

(ert-deftest firefox-to-emacs-native-messenger-test-registry-contains-p-hit ()
  "`contains-p' returns t for a registered path that still matches identity."
  :tags '(:unit :sandbox)
  (firefox-to-emacs-native-messenger-test--with-fresh-registry
   (firefox-to-emacs-native-messenger-test-with-sandbox-tempdir dir
     (let ((path (expand-file-name "f.txt" dir)))
       (with-temp-file path (insert "x"))
       (firefox-to-emacs-native-messenger--registry-register path)
       (should (firefox-to-emacs-native-messenger--registry-contains-p path))))))

(ert-deftest firefox-to-emacs-native-messenger-test-registry-contains-p-not-registered ()
  "`contains-p' returns nil for unregistered paths."
  :tags '(:unit :sandbox)
  (firefox-to-emacs-native-messenger-test--with-fresh-registry
   (firefox-to-emacs-native-messenger-test-with-sandbox-tempdir dir
     (let ((path (expand-file-name "f.txt" dir)))
       (with-temp-file path (insert "x"))
       (should-not (firefox-to-emacs-native-messenger--registry-contains-p path))))))

(ert-deftest firefox-to-emacs-native-messenger-test-registry-contains-p-prunes-missing ()
  "`contains-p' prunes and returns nil when the file is gone."
  :tags '(:unit :sandbox)
  (firefox-to-emacs-native-messenger-test--with-fresh-registry
   (firefox-to-emacs-native-messenger-test-with-sandbox-tempdir dir
     (let ((path (expand-file-name "f.txt" dir)))
       (with-temp-file path (insert "x"))
       (firefox-to-emacs-native-messenger--registry-register path)
       (delete-file path)
       (should-not (firefox-to-emacs-native-messenger--registry-contains-p path))
       (should-not (gethash path
                            firefox-to-emacs-native-messenger--capability-registry))))))

(ert-deftest firefox-to-emacs-native-messenger-test-registry-contains-p-prunes-dev-inode-mismatch ()
  "`contains-p' prunes when dev/inode changes (file replaced via rename)."
  :tags '(:unit :sandbox)
  (firefox-to-emacs-native-messenger-test--with-fresh-registry
   (firefox-to-emacs-native-messenger-test-with-sandbox-tempdir dir
     (let ((path (expand-file-name "f.txt" dir))
           (replacement (expand-file-name "g.txt" dir)))
       (with-temp-file path (insert "x"))
       (firefox-to-emacs-native-messenger--registry-register path)
       (with-temp-file replacement (insert "y"))
       (rename-file replacement path t)
       (should-not (firefox-to-emacs-native-messenger--registry-contains-p path))
       (should-not (gethash path
                            firefox-to-emacs-native-messenger--capability-registry))))))

(ert-deftest firefox-to-emacs-native-messenger-test-registry-contains-p-prunes-symlink ()
  "`contains-p' prunes when the registered path becomes a symlink."
  :tags '(:unit :sandbox)
  (firefox-to-emacs-native-messenger-test--with-fresh-registry
   (firefox-to-emacs-native-messenger-test-with-sandbox-tempdir dir
     (let ((path (expand-file-name "f.txt" dir))
           (target (expand-file-name "tgt.txt" dir)))
       (with-temp-file path (insert "x"))
       (with-temp-file target (insert "t"))
       (firefox-to-emacs-native-messenger--registry-register path)
       (delete-file path)
       (make-symbolic-link target path)
       (should-not (firefox-to-emacs-native-messenger--registry-contains-p path))
       (should-not (gethash path
                            firefox-to-emacs-native-messenger--capability-registry))))))

(ert-deftest firefox-to-emacs-native-messenger-test-registry-contains-p-prunes-directory ()
  "`contains-p' prunes when the registered path becomes a directory."
  :tags '(:unit :sandbox)
  (firefox-to-emacs-native-messenger-test--with-fresh-registry
   (firefox-to-emacs-native-messenger-test-with-sandbox-tempdir dir
     (let ((path (expand-file-name "f.txt" dir)))
       (with-temp-file path (insert "x"))
       (firefox-to-emacs-native-messenger--registry-register path)
       (delete-file path)
       (make-directory path)
       (should-not (firefox-to-emacs-native-messenger--registry-contains-p path))
       (should-not (gethash path
                            firefox-to-emacs-native-messenger--capability-registry))))))

(ert-deftest firefox-to-emacs-native-messenger-test-registry-clear-all ()
  "`clear-all' empties the registry."
  :tags '(:unit :sandbox)
  (firefox-to-emacs-native-messenger-test--with-fresh-registry
   (firefox-to-emacs-native-messenger-test-with-sandbox-tempdir dir
     (let ((p1 (expand-file-name "f1.txt" dir))
           (p2 (expand-file-name "f2.txt" dir)))
       (with-temp-file p1 (insert "x"))
       (with-temp-file p2 (insert "y"))
       (firefox-to-emacs-native-messenger--registry-register p1)
       (firefox-to-emacs-native-messenger--registry-register p2)
       (should (= (hash-table-count
                   firefox-to-emacs-native-messenger--capability-registry)
                  2))
       (firefox-to-emacs-native-messenger--registry-clear-all)
       (should (= (hash-table-count
                   firefox-to-emacs-native-messenger--capability-registry)
                  0))))))

(ert-deftest firefox-to-emacs-native-messenger-test-registry-prune-all ()
  "`prune-all' removes only missing/mismatched entries; preserves valid ones."
  :tags '(:unit :sandbox)
  (firefox-to-emacs-native-messenger-test--with-fresh-registry
   (firefox-to-emacs-native-messenger-test-with-sandbox-tempdir dir
     (let ((p1 (expand-file-name "f1.txt" dir))
           (p2 (expand-file-name "f2.txt" dir)))
       (with-temp-file p1 (insert "x"))
       (with-temp-file p2 (insert "y"))
       (firefox-to-emacs-native-messenger--registry-register p1)
       (firefox-to-emacs-native-messenger--registry-register p2)
       (delete-file p1)
       (firefox-to-emacs-native-messenger--registry-prune-all)
       (should-not (gethash p1
                            firefox-to-emacs-native-messenger--capability-registry))
       (should (gethash p2
                        firefox-to-emacs-native-messenger--capability-registry))))))

(ert-deftest firefox-to-emacs-native-messenger-test-verify-cache-directory-creates-if-absent ()
  "Verification creates the cache directory at mode 0700 when the path is absent.
After the call the path MUST be a real directory (not a symlink) at exactly mode 0700."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test-with-sandbox-tempdir sandbox
    (let* ((cache (file-name-as-directory
                   (expand-file-name "fenm-cache" sandbox)))
           (firefox-to-emacs-native-messenger-cache-directory cache))
      (should-not (file-exists-p cache))
      (firefox-to-emacs-native-messenger--verify-cache-directory)
      (should (file-directory-p cache))
      (should-not (file-symlink-p (directory-file-name cache)))
      (should (= (logand (file-modes cache) #o7777) #o700)))))

(ert-deftest firefox-to-emacs-native-messenger-test-verify-cache-directory-ok-at-0700 ()
  "Verification succeeds when the cache directory exists as a real 0700 directory."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test-with-sandbox-tempdir sandbox
    (let* ((cache (file-name-as-directory
                   (expand-file-name "fenm-cache" sandbox)))
           (firefox-to-emacs-native-messenger-cache-directory cache))
      (make-directory cache)
      (set-file-modes cache #o700)
      (firefox-to-emacs-native-messenger--verify-cache-directory)
      (should (file-directory-p cache))
      (should-not (file-symlink-p (directory-file-name cache)))
      (should (= (logand (file-modes cache) #o7777) #o700)))))

(ert-deftest firefox-to-emacs-native-messenger-test-verify-cache-directory-rejects-more-permissive ()
  "Verification signals bad-state when the cache directory has a more permissive mode (e.g., 0755)."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test-with-sandbox-tempdir sandbox
    (let* ((cache (file-name-as-directory
                   (expand-file-name "fenm-cache" sandbox)))
           (firefox-to-emacs-native-messenger-cache-directory cache))
      (make-directory cache)
      (set-file-modes cache #o755)
      (should-error
       (firefox-to-emacs-native-messenger--verify-cache-directory)
       :type 'firefox-to-emacs-native-messenger-bad-state))))

(ert-deftest firefox-to-emacs-native-messenger-test-verify-cache-directory-rejects-less-permissive ()
  "Verification signals bad-state when the cache directory has a less permissive mode (e.g., 0500)."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test-with-sandbox-tempdir sandbox
    (let* ((cache (file-name-as-directory
                   (expand-file-name "fenm-cache" sandbox)))
           (firefox-to-emacs-native-messenger-cache-directory cache))
      (make-directory cache)
      (set-file-modes cache #o500)
      (should-error
       (firefox-to-emacs-native-messenger--verify-cache-directory)
       :type 'firefox-to-emacs-native-messenger-bad-state))))

(ert-deftest firefox-to-emacs-native-messenger-test-verify-cache-directory-rejects-symlink ()
  "Verification signals bad-state when the cache-directory path is a symlink, even when its target is a real 0700 directory."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test-with-sandbox-tempdir sandbox
    (let* ((real (expand-file-name "real-cache" sandbox))
           (link (expand-file-name "fenm-cache" sandbox))
           (cache (file-name-as-directory link))
           (firefox-to-emacs-native-messenger-cache-directory cache))
      (make-directory real)
      (set-file-modes real #o700)
      (make-symbolic-link real link)
      (should (file-symlink-p link))
      (should-error
       (firefox-to-emacs-native-messenger--verify-cache-directory)
       :type 'firefox-to-emacs-native-messenger-bad-state))))

(ert-deftest firefox-to-emacs-native-messenger-test-verify-tempfile-directory-creates-if-absent ()
  "Verification creates the tempfile directory at mode 0700 when the path is absent.
After the call the path MUST be a real directory (not a symlink) at exactly mode 0700,
owned by the daemon's effective UID."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test-with-sandbox-tempdir sandbox
    (let* ((tmp (file-name-as-directory
                 (expand-file-name "fenm-tempfiles" sandbox)))
           (firefox-to-emacs-native-messenger-tempfile-directory tmp))
      (should-not (file-exists-p tmp))
      (firefox-to-emacs-native-messenger--verify-tempfile-directory)
      (should (file-directory-p tmp))
      (should-not (file-symlink-p (directory-file-name tmp)))
      (should (= (logand (file-modes tmp) #o7777) #o700))
      (should (= (file-attribute-user-id
                  (file-attributes (directory-file-name tmp)))
                 (user-uid))))))

(ert-deftest firefox-to-emacs-native-messenger-test-verify-tempfile-directory-ok-at-0700 ()
  "Verification succeeds when the tempfile directory exists as a real 0700 directory."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test-with-sandbox-tempdir sandbox
    (let* ((tmp (file-name-as-directory
                 (expand-file-name "fenm-tempfiles" sandbox)))
           (firefox-to-emacs-native-messenger-tempfile-directory tmp))
      (make-directory tmp)
      (set-file-modes tmp #o700)
      (firefox-to-emacs-native-messenger--verify-tempfile-directory)
      (should (file-directory-p tmp))
      (should-not (file-symlink-p (directory-file-name tmp)))
      (should (= (logand (file-modes tmp) #o7777) #o700)))))

(ert-deftest firefox-to-emacs-native-messenger-test-verify-tempfile-directory-rejects-wrong-mode ()
  "Verification signals bad-state when the tempfile directory has a non-0700 mode."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test-with-sandbox-tempdir sandbox
    (let* ((tmp (file-name-as-directory
                 (expand-file-name "fenm-tempfiles" sandbox)))
           (firefox-to-emacs-native-messenger-tempfile-directory tmp))
      (make-directory tmp)
      (set-file-modes tmp #o755)
      (should-error
       (firefox-to-emacs-native-messenger--verify-tempfile-directory)
       :type 'firefox-to-emacs-native-messenger-bad-state))))

(ert-deftest firefox-to-emacs-native-messenger-test-verify-tempfile-directory-rejects-symlink ()
  "Verification signals bad-state when the tempfile-directory path is a symlink,
even when its target is a real 0700 directory owned by the daemon."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test-with-sandbox-tempdir sandbox
    (let* ((real (expand-file-name "real-tempfiles" sandbox))
           (link (expand-file-name "fenm-tempfiles" sandbox))
           (tmp (file-name-as-directory link))
           (firefox-to-emacs-native-messenger-tempfile-directory tmp))
      (make-directory real)
      (set-file-modes real #o700)
      (make-symbolic-link real link)
      (should (file-symlink-p link))
      (should-error
       (firefox-to-emacs-native-messenger--verify-tempfile-directory)
       :type 'firefox-to-emacs-native-messenger-bad-state))))

(defun firefox-to-emacs-native-messenger-test--create-stale-unix-socket (path)
  "Create a UNIX socket file at PATH that has no live listener.

Spawns a short-lived `python3' subprocess that binds an AF_UNIX SOCK_STREAM
socket at PATH, then closes it without unlinking.  After the call the file
at PATH is a real socket (mode-string begins with `s') but no process is
accepting on it.  Tests using this helper require python3 on PATH."
  (let ((exit (call-process
               "python3" nil nil nil "-c"
               "import socket, sys
s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
s.bind(sys.argv[1])
s.close()"
               path)))
    (unless (eq exit 0)
      (error "python3 helper exited %s for path %s" exit path))
    (unless (eq (aref (file-attribute-modes (file-attributes path)) 0) ?s)
      (error "expected a socket at %s but got mode-string %s"
             path (file-attribute-modes (file-attributes path))))))

(ert-deftest firefox-to-emacs-native-messenger-test-probe-stale-socket-absent-noop ()
  "Probe is a no-op when the socket path is absent; a subsequent bind succeeds."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test-with-sandbox-tempdir sandbox
    (let ((sock (expand-file-name "messenger.sock" sandbox)))
      (should-not (file-exists-p sock))
      (firefox-to-emacs-native-messenger--probe-and-delete-stale-socket sock)
      (should-not (file-exists-p sock))
      (let ((server (make-network-process
                     :name "fenm-probe-bind-test"
                     :family 'local
                     :server t
                     :service sock
                     :coding '(binary . binary)
                     :filter-multibyte nil
                     :noquery t)))
        (unwind-protect
            (should (process-live-p server))
          (when (process-live-p server) (delete-process server)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-probe-stale-socket-live-listener-refuses ()
  "Probe signals bad-state when the path is bound by a live listener."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test-with-sandbox-tempdir sandbox
    (let* ((sock (expand-file-name "messenger.sock" sandbox))
           (server (make-network-process
                    :name "fenm-probe-live-listener"
                    :family 'local
                    :server t
                    :service sock
                    :coding '(binary . binary)
                    :filter-multibyte nil
                    :noquery t)))
      (unwind-protect
          (progn
            (should (file-exists-p sock))
            (should-error
             (firefox-to-emacs-native-messenger--probe-and-delete-stale-socket
              sock)
             :type 'firefox-to-emacs-native-messenger-bad-state)
            (should (file-exists-p sock)))
        (when (process-live-p server) (delete-process server))))))

(ert-deftest firefox-to-emacs-native-messenger-test-probe-stale-socket-dead-listener-unlinks ()
  "Probe unlinks when the path is a socket file with no live listener.
After the probe the path is gone; a fresh bind at the same path succeeds."
  :tags '(:integration :sandbox)
  (skip-unless (executable-find "python3"))
  (firefox-to-emacs-native-messenger-test-with-sandbox-tempdir sandbox
    (let ((sock (expand-file-name "messenger.sock" sandbox)))
      (firefox-to-emacs-native-messenger-test--create-stale-unix-socket sock)
      (should (file-exists-p sock))
      (firefox-to-emacs-native-messenger--probe-and-delete-stale-socket sock)
      (should-not (file-exists-p sock))
      (let ((server (make-network-process
                     :name "fenm-probe-rebind-after-stale"
                     :family 'local
                     :server t
                     :service sock
                     :coding '(binary . binary)
                     :filter-multibyte nil
                     :noquery t)))
        (unwind-protect
            (should (process-live-p server))
          (when (process-live-p server) (delete-process server)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-probe-stale-socket-non-socket-refuses ()
  "Probe signals bad-state when the path is a regular file (not a socket).
The file is NOT deleted in this case; the bridge refuses to clobber it."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test-with-sandbox-tempdir sandbox
    (let ((sock (expand-file-name "messenger.sock" sandbox)))
      (with-temp-file sock (insert "i am not a socket"))
      (should (file-regular-p sock))
      (should-error
       (firefox-to-emacs-native-messenger--probe-and-delete-stale-socket sock)
       :type 'firefox-to-emacs-native-messenger-bad-state)
      (should (file-exists-p sock)))))

(ert-deftest firefox-to-emacs-native-messenger-test-probe-stale-socket-symlink-refuses ()
  "Probe signals bad-state when the path is a symlink (not a real socket).
A symlink at the socket path could redirect a subsequent bind, so the
probe MUST refuse rather than unlink it."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test-with-sandbox-tempdir sandbox
    (let ((sock (expand-file-name "messenger.sock" sandbox))
          (target (expand-file-name "target.txt" sandbox)))
      (with-temp-file target (insert "i am the target"))
      (make-symbolic-link target sock)
      (should (file-symlink-p sock))
      (should-error
       (firefox-to-emacs-native-messenger--probe-and-delete-stale-socket sock)
       :type 'firefox-to-emacs-native-messenger-bad-state)
      (should (file-exists-p sock)))))

(defmacro firefox-to-emacs-native-messenger-test--with-sandbox-tempfile-dir
    (var &rest body)
  "Bind VAR to a sandbox tempfile directory and let-bind the bridge's
tempfile-directory defconst to that path for the duration of BODY.

Creates a fresh sandbox tempdir, places a sub-directory `fenm-tempfiles'
inside it at mode 0700, binds VAR to that sub-directory, and let-binds
`firefox-to-emacs-native-messenger-tempfile-directory' to the same path so
production helpers consulting that variable see the sandbox.  All artifacts
are cleaned up on body exit via the underlying sandbox-tempdir macro."
  (declare (indent 1) (debug (symbolp body)))
  (let ((sandbox-sym (gensym "sandbox-")))
    `(firefox-to-emacs-native-messenger-test-with-sandbox-tempdir
      ,sandbox-sym
      (let* ((,var (file-name-as-directory
                    (expand-file-name "fenm-tempfiles" ,sandbox-sym)))
             (firefox-to-emacs-native-messenger-tempfile-directory ,var))
        (make-directory ,var)
        (set-file-modes ,var #o700)
        ,@body))))

(ert-deftest firefox-to-emacs-native-messenger-test-sweep-tempfiles-deletes-matching ()
  "Sweep deletes every file in FILE-1600 matching the `tmp_*.txt' glob."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-sandbox-tempfile-dir tmp
    (dolist (n '("tmp_a.txt" "tmp_b_c.txt" "tmp_.txt"))
      (with-temp-file (expand-file-name n tmp) (insert "x")))
    (should (file-exists-p (expand-file-name "tmp_a.txt" tmp)))
    (firefox-to-emacs-native-messenger--sweep-tempfiles)
    (should-not (file-exists-p (expand-file-name "tmp_a.txt" tmp)))
    (should-not (file-exists-p (expand-file-name "tmp_b_c.txt" tmp)))
    (should-not (file-exists-p (expand-file-name "tmp_.txt" tmp)))))

(ert-deftest firefox-to-emacs-native-messenger-test-sweep-tempfiles-preserves-non-matching ()
  "Sweep preserves files in FILE-1600 that do not match `tmp_*.txt'."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-sandbox-tempfile-dir tmp
    (let ((keepers '("other.txt" "tmp_x.dat" "tmp.txt"
                     "TMP_x.txt" ".dotfile")))
      (dolist (n keepers)
        (with-temp-file (expand-file-name n tmp) (insert "x")))
      (with-temp-file (expand-file-name "tmp_doomed.txt" tmp) (insert "x"))
      (firefox-to-emacs-native-messenger--sweep-tempfiles)
      (dolist (n keepers)
        (should (file-exists-p (expand-file-name n tmp))))
      (should-not (file-exists-p (expand-file-name "tmp_doomed.txt" tmp))))))

(ert-deftest firefox-to-emacs-native-messenger-test-sweep-tempfiles-empty-directory ()
  "Sweep is a no-op when FILE-1600 contains no matching entries."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-sandbox-tempfile-dir tmp
    (with-temp-file (expand-file-name "keep.txt" tmp) (insert "x"))
    (firefox-to-emacs-native-messenger--sweep-tempfiles)
    (should (file-exists-p (expand-file-name "keep.txt" tmp)))))

(ert-deftest firefox-to-emacs-native-messenger-test-sweep-tempfiles-logs-counts ()
  "Sweep logs a count summary at info level.
The log buffer contains a record naming the bridge's sweep, the count of
files deleted (3), and the count of files preserved (2)."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-sandbox-tempfile-dir tmp
    (let ((firefox-to-emacs-native-messenger-log-buffer-name
           (generate-new-buffer-name "*fenm-sweep-test-log*"))
          (firefox-to-emacs-native-messenger-log-level 'info))
      (unwind-protect
          (progn
            (dolist (n '("tmp_a.txt" "tmp_b.txt" "tmp_c.txt"
                         "keep1.txt" "keep2.dat"))
              (with-temp-file (expand-file-name n tmp) (insert "x")))
            (firefox-to-emacs-native-messenger--sweep-tempfiles)
            (with-current-buffer
                (get-buffer firefox-to-emacs-native-messenger-log-buffer-name)
              (let ((contents (buffer-string)))
                (should (string-match-p "sweep" contents))
                (should (string-match-p "\\b3\\b" contents))
                (should (string-match-p "\\b2\\b" contents)))))
        (when (get-buffer firefox-to-emacs-native-messenger-log-buffer-name)
          (kill-buffer firefox-to-emacs-native-messenger-log-buffer-name))))))

(ert-deftest firefox-to-emacs-native-messenger-test-registry-clear-on-start-empties ()
  "`registry-clear-on-start' empties a populated capability registry.
Files registered before the call remain on disk; only the registry
in-memory state is cleared."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-fresh-registry
   (firefox-to-emacs-native-messenger-test-with-sandbox-tempdir dir
     (let ((p1 (expand-file-name "f1.txt" dir))
           (p2 (expand-file-name "f2.txt" dir)))
       (with-temp-file p1 (insert "x"))
       (with-temp-file p2 (insert "y"))
       (firefox-to-emacs-native-messenger--registry-register p1)
       (firefox-to-emacs-native-messenger--registry-register p2)
       (should (= (hash-table-count
                   firefox-to-emacs-native-messenger--capability-registry)
                  2))
       (firefox-to-emacs-native-messenger--registry-clear-on-start)
       (should (= (hash-table-count
                   firefox-to-emacs-native-messenger--capability-registry)
                  0))
       (should (file-exists-p p1))
       (should (file-exists-p p2))))))

(ert-deftest firefox-to-emacs-native-messenger-test-registry-clear-on-stop-empties ()
  "`registry-clear-on-stop' empties a populated capability registry.
Files registered before the call remain on disk; only the registry
in-memory state is cleared."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-fresh-registry
   (firefox-to-emacs-native-messenger-test-with-sandbox-tempdir dir
     (let ((p1 (expand-file-name "f1.txt" dir)))
       (with-temp-file p1 (insert "x"))
       (firefox-to-emacs-native-messenger--registry-register p1)
       (should (= (hash-table-count
                   firefox-to-emacs-native-messenger--capability-registry)
                  1))
       (firefox-to-emacs-native-messenger--registry-clear-on-stop)
       (should (= (hash-table-count
                   firefox-to-emacs-native-messenger--capability-registry)
                  0))
       (should (file-exists-p p1))))))

(ert-deftest firefox-to-emacs-native-messenger-test-registry-clear-on-start-idempotent ()
  "Calling `registry-clear-on-start' twice is a silent no-op the second time."
  :tags '(:unit)
  (firefox-to-emacs-native-messenger-test--with-fresh-registry
   (firefox-to-emacs-native-messenger--registry-clear-on-start)
   (firefox-to-emacs-native-messenger--registry-clear-on-start)
   (should (= (hash-table-count
               firefox-to-emacs-native-messenger--capability-registry)
              0))))

(ert-deftest firefox-to-emacs-native-messenger-test-registry-clear-on-stop-idempotent ()
  "Calling `registry-clear-on-stop' twice is a silent no-op the second time."
  :tags '(:unit)
  (firefox-to-emacs-native-messenger-test--with-fresh-registry
   (firefox-to-emacs-native-messenger--registry-clear-on-stop)
   (firefox-to-emacs-native-messenger--registry-clear-on-stop)
   (should (= (hash-table-count
               firefox-to-emacs-native-messenger--capability-registry)
              0))))

(ert-deftest firefox-to-emacs-native-messenger-test-listener-start-whitelist-sweep-defaults-ok ()
  "Default-deny whitelists (nil/nil) are valid; the start sweep returns without error."
  :tags '(:integration)
  (let ((firefox-to-emacs-native-messenger-run-whitelist nil)
        (firefox-to-emacs-native-messenger-read-whitelist nil))
    (should
     (firefox-to-emacs-native-messenger--listener-start-whitelist-sweep))))

(ert-deftest firefox-to-emacs-native-messenger-test-listener-start-whitelist-sweep-wellformed-ok ()
  "Well-formed run and read whitelists pass the start sweep."
  :tags '(:integration)
  (let ((firefox-to-emacs-native-messenger-run-whitelist
         '("emacsclient <TEMP-PATH>" "rm -f '<TEMP-PATH>'"))
        (firefox-to-emacs-native-messenger-read-whitelist
         '("<TEMP-PATH>" "/etc/hosts" "/etc/*")))
    (should
     (firefox-to-emacs-native-messenger--listener-start-whitelist-sweep))))

(ert-deftest firefox-to-emacs-native-messenger-test-listener-start-whitelist-sweep-rejects-bad-run ()
  "A malformed run-whitelist (mixed allow-all + literal) refuses start sweep."
  :tags '(:integration)
  (let ((firefox-to-emacs-native-messenger-run-whitelist '("*" "extra"))
        (firefox-to-emacs-native-messenger-read-whitelist nil))
    (should-error
     (firefox-to-emacs-native-messenger--listener-start-whitelist-sweep)
     :type 'firefox-to-emacs-native-messenger-whitelist-malformed)))

(ert-deftest firefox-to-emacs-native-messenger-test-listener-start-whitelist-sweep-rejects-bad-read ()
  "A malformed read-whitelist (relative path) refuses start sweep."
  :tags '(:integration)
  (let ((firefox-to-emacs-native-messenger-run-whitelist nil)
        (firefox-to-emacs-native-messenger-read-whitelist
         '("relative/path/no-leading-slash")))
    (should-error
     (firefox-to-emacs-native-messenger--listener-start-whitelist-sweep)
     :type 'firefox-to-emacs-native-messenger-whitelist-malformed)))

(defmacro firefox-to-emacs-native-messenger-test--with-listener-sandbox
    (var-cache var-tmp var-sock &rest body)
  "Bind sandbox cache-dir/tempfile-dir/socket-path for listener tests.

Creates a fresh sandbox tempdir, computes a cache-directory subpath
inside it, a tempfile-directory subpath inside it, and a socket path
inside the cache.  Let-binds each of the bridge's three location
defconsts to the corresponding sandbox path for the duration of BODY,
and binds VAR-CACHE, VAR-TMP, VAR-SOCK to those values.  Also forcibly
clears `firefox-to-emacs-native-messenger--listener-process' on body
exit so a stop call is not strictly required by the test author.

The directories are NOT pre-created; the listener-start sequence is
expected to create them itself.  The macro restores all bindings via
`unwind-protect' regardless of test outcome."
  (declare (indent 3) (debug (symbolp symbolp symbolp body)))
  (let ((sandbox-sym (gensym "sandbox-")))
    `(firefox-to-emacs-native-messenger-test-with-sandbox-tempdir
      ,sandbox-sym
      (let* ((,var-cache (file-name-as-directory
                          (expand-file-name "cache" ,sandbox-sym)))
             (,var-tmp   (file-name-as-directory
                          (expand-file-name "tempfiles" ,sandbox-sym)))
             (,var-sock  (expand-file-name "messenger.sock" ,var-cache))
             (firefox-to-emacs-native-messenger-cache-directory ,var-cache)
             (firefox-to-emacs-native-messenger-tempfile-directory ,var-tmp)
             (firefox-to-emacs-native-messenger-socket-path ,var-sock))
        (unwind-protect
            (progn ,@body)
          (when (and (boundp
                      'firefox-to-emacs-native-messenger--listener-process)
                     firefox-to-emacs-native-messenger--listener-process
                     (process-live-p
                      firefox-to-emacs-native-messenger--listener-process))
            (delete-process
             firefox-to-emacs-native-messenger--listener-process))
          (when (boundp 'firefox-to-emacs-native-messenger--listener-process)
            (setq firefox-to-emacs-native-messenger--listener-process nil)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-start-binds-socket ()
  "`start' binds the listener socket and records the listener process.

After a successful start: the listener process variable is non-nil and
live; the socket file exists at the configured path; the cache and
tempfile directories exist at mode 0700."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-listener-sandbox
      cache tmp sock
    (firefox-to-emacs-native-messenger-start)
    (should (process-live-p
             firefox-to-emacs-native-messenger--listener-process))
    (should (file-exists-p sock))
    (should (file-directory-p cache))
    (should (= (logand (file-modes cache) #o7777) #o700))
    (should (file-directory-p tmp))
    (should (= (logand (file-modes tmp) #o7777) #o700))))

(ert-deftest firefox-to-emacs-native-messenger-test-start-runs-prebind-checks-in-order ()
  "`start' invokes all pre-bind checks in the order documented by the plan.

Order asserted: verify-cache-directory, verify-tempfile-directory,
sweep-tempfiles, registry-clear-on-start, listener-start-whitelist-sweep,
probe-and-delete-stale-socket.  The bind itself happens last."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-listener-sandbox
      cache tmp sock
    (let* ((order nil)
           (targets '(firefox-to-emacs-native-messenger--verify-cache-directory
                      firefox-to-emacs-native-messenger--verify-tempfile-directory
                      firefox-to-emacs-native-messenger--sweep-tempfiles
                      firefox-to-emacs-native-messenger--registry-clear-on-start
                      firefox-to-emacs-native-messenger--listener-start-whitelist-sweep
                      firefox-to-emacs-native-messenger--probe-and-delete-stale-socket))
           (installed
            (mapcar (lambda (sym)
                      (let ((captured sym))
                        (cons captured
                              (lambda (&rest _) (push captured order)))))
                    targets)))
      (unwind-protect
          (progn
            (dolist (pair installed)
              (advice-add (car pair) :before (cdr pair)))
            (firefox-to-emacs-native-messenger-start)
            (should (equal (nreverse order) targets)))
        (dolist (pair installed)
          (advice-remove (car pair) (cdr pair)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-start-idempotent-second-call-signals ()
  "A second `start' call while a listener is already recorded signals an error
and does NOT replace or duplicate the bound socket.  The original listener
process is left intact."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-listener-sandbox
      cache tmp sock
    (firefox-to-emacs-native-messenger-start)
    (let ((first firefox-to-emacs-native-messenger--listener-process))
      (should (process-live-p first))
      (should-error
       (firefox-to-emacs-native-messenger-start)
       :type 'firefox-to-emacs-native-messenger-bad-state)
      (should (eq firefox-to-emacs-native-messenger--listener-process first))
      (should (process-live-p first))
      (should (file-exists-p sock)))))

(ert-deftest firefox-to-emacs-native-messenger-test-start-refuses-on-malformed-whitelist ()
  "A malformed `run-whitelist' refuses start loudly BEFORE binding.

The whitelist sweep runs before the stale-socket probe and the bind, so a
malformed whitelist causes `start' to signal `whitelist-malformed' (the
type bubbled from the shared validator) and the socket file is never
created.  The listener-process variable remains nil."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-listener-sandbox
      cache tmp sock
    (let ((firefox-to-emacs-native-messenger-run-whitelist '("*" "extra"))
          (firefox-to-emacs-native-messenger-read-whitelist nil))
      (should-error
       (firefox-to-emacs-native-messenger-start)
       :type 'firefox-to-emacs-native-messenger-whitelist-malformed)
      (should-not firefox-to-emacs-native-messenger--listener-process)
      (should-not (file-exists-p sock)))))

(ert-deftest firefox-to-emacs-native-messenger-test-stop-after-start-tears-down ()
  "Stop after a successful start: process deleted, socket removed,
listener variable cleared, capability registry cleared."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-listener-sandbox
      cache tmp sock
    (firefox-to-emacs-native-messenger-start)
    (let ((proc firefox-to-emacs-native-messenger--listener-process))
      (should (process-live-p proc))
      (should (file-exists-p sock))
      (puthash "x" '(:dev 1 :inode 2 :uid 3)
               firefox-to-emacs-native-messenger--capability-registry)
      (firefox-to-emacs-native-messenger-stop)
      (should-not (process-live-p proc))
      (should-not (file-exists-p sock))
      (should-not firefox-to-emacs-native-messenger--listener-process)
      (should (= (hash-table-count
                  firefox-to-emacs-native-messenger--capability-registry)
                 0)))))

(ert-deftest firefox-to-emacs-native-messenger-test-stop-noop-when-no-listener ()
  "Stop is a silent no-op when no listener is recorded."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-listener-sandbox
      cache tmp sock
    (should-not firefox-to-emacs-native-messenger--listener-process)
    (should-not (firefox-to-emacs-native-messenger-stop))
    (should-not firefox-to-emacs-native-messenger--listener-process)))

(ert-deftest firefox-to-emacs-native-messenger-test-stop-idempotent ()
  "Stop after a prior stop is a silent no-op."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-listener-sandbox
      cache tmp sock
    (firefox-to-emacs-native-messenger-start)
    (firefox-to-emacs-native-messenger-stop)
    (should-not firefox-to-emacs-native-messenger--listener-process)
    (firefox-to-emacs-native-messenger-stop)
    (should-not firefox-to-emacs-native-messenger--listener-process)))

(ert-deftest firefox-to-emacs-native-messenger-test-stop-preserves-non-socket-at-path ()
  "Stop does not unlink a file at the socket path that is no longer a socket.

If the listener's socket file at the configured path has been replaced
externally by a regular file or a symlink (e.g., by an attacker who
unlinked the listener's socket and dropped a file in its place), stop
MUST not clobber the replacement.  Stop still tears down the listener
process and clears the bridge's in-memory state."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-listener-sandbox
      cache tmp sock
    (firefox-to-emacs-native-messenger-start)
    (let ((proc firefox-to-emacs-native-messenger--listener-process))
      (delete-file sock)
      (with-temp-file sock (insert "not a socket"))
      (should (file-regular-p sock))
      (firefox-to-emacs-native-messenger-stop)
      (should-not (process-live-p proc))
      (should-not firefox-to-emacs-native-messenger--listener-process)
      (should (file-exists-p sock))
      (should (file-regular-p sock)))))

(defmacro firefox-to-emacs-native-messenger-test--with-fresh-connection-registry
    (&rest body)
  "Save the connection registry, run BODY, clear and restore on exit."
  (declare (indent 0) (debug (body)))
  `(let ((saved-entries
          (let (acc)
            (maphash
             (lambda (k v) (push (cons k v) acc))
             firefox-to-emacs-native-messenger--connection-registry)
            acc)))
     (clrhash firefox-to-emacs-native-messenger--connection-registry)
     (unwind-protect
         (progn ,@body)
       (clrhash firefox-to-emacs-native-messenger--connection-registry)
       (dolist (e saved-entries)
         (puthash (car e) (cdr e)
                  firefox-to-emacs-native-messenger--connection-registry)))))

(ert-deftest firefox-to-emacs-native-messenger-test-connection-registry-init ()
  "Connection registry is a hash table (test `eq') held in a module-level
variable, initially empty."
  :tags '(:unit)
  (should (hash-table-p
           firefox-to-emacs-native-messenger--connection-registry))
  (should (eq 'eq
              (hash-table-test
               firefox-to-emacs-native-messenger--connection-registry))))

(ert-deftest firefox-to-emacs-native-messenger-test-connection-registry-add-and-list ()
  "`registry-add' inserts a connection; `registry-list' returns live
connections; dead processes are filtered out of the list."
  :tags '(:integration)
  (firefox-to-emacs-native-messenger-test--with-fresh-connection-registry
   (let ((live (make-pipe-process :name "fenm-conn-live" :noquery t))
         (dead (make-pipe-process :name "fenm-conn-dead" :noquery t)))
     (unwind-protect
         (progn
           (delete-process dead)
           (firefox-to-emacs-native-messenger--connection-registry-add live)
           (firefox-to-emacs-native-messenger--connection-registry-add dead)
           (should (= (hash-table-count
                       firefox-to-emacs-native-messenger--connection-registry)
                      2))
           (let ((listed
                  (firefox-to-emacs-native-messenger--connection-registry-list)))
             (should (memq live listed))
             (should-not (memq dead listed))))
       (when (process-live-p live) (delete-process live))))))

(ert-deftest firefox-to-emacs-native-messenger-test-connection-registry-remove ()
  "`registry-remove' removes the specified connection; double-remove is harmless."
  :tags '(:integration)
  (firefox-to-emacs-native-messenger-test--with-fresh-connection-registry
   (let ((proc (make-pipe-process :name "fenm-conn-rm" :noquery t)))
     (unwind-protect
         (progn
           (firefox-to-emacs-native-messenger--connection-registry-add proc)
           (should (= (hash-table-count
                       firefox-to-emacs-native-messenger--connection-registry)
                      1))
           (firefox-to-emacs-native-messenger--connection-registry-remove proc)
           (should (= (hash-table-count
                       firefox-to-emacs-native-messenger--connection-registry)
                      0))
           (firefox-to-emacs-native-messenger--connection-registry-remove proc))
       (when (process-live-p proc) (delete-process proc))))))

(ert-deftest firefox-to-emacs-native-messenger-test-connection-registry-clear ()
  "`registry-clear' empties the connection registry."
  :tags '(:integration)
  (firefox-to-emacs-native-messenger-test--with-fresh-connection-registry
   (let ((p1 (make-pipe-process :name "fenm-conn-clear-1" :noquery t))
         (p2 (make-pipe-process :name "fenm-conn-clear-2" :noquery t)))
     (unwind-protect
         (progn
           (firefox-to-emacs-native-messenger--connection-registry-add p1)
           (firefox-to-emacs-native-messenger--connection-registry-add p2)
           (should (= (hash-table-count
                       firefox-to-emacs-native-messenger--connection-registry)
                      2))
           (firefox-to-emacs-native-messenger--connection-registry-clear)
           (should (= (hash-table-count
                       firefox-to-emacs-native-messenger--connection-registry)
                      0)))
       (when (process-live-p p1) (delete-process p1))
       (when (process-live-p p2) (delete-process p2))))))

(ert-deftest firefox-to-emacs-native-messenger-test-start-registers-kill-emacs-hook ()
  "`start' adds the cleanup function to `kill-emacs-hook'."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-listener-sandbox
      cache tmp sock
    (let ((kill-emacs-hook nil))
      (firefox-to-emacs-native-messenger-start)
      (should (memq 'firefox-to-emacs-native-messenger--cleanup-on-shutdown
                    kill-emacs-hook)))))

(ert-deftest firefox-to-emacs-native-messenger-test-start-kill-emacs-hook-idempotent ()
  "Repeated `start' (with intervening `stop') does not stack the hook."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-listener-sandbox
      cache tmp sock
    (let ((kill-emacs-hook nil))
      (firefox-to-emacs-native-messenger-start)
      (firefox-to-emacs-native-messenger-stop)
      (firefox-to-emacs-native-messenger-start)
      (firefox-to-emacs-native-messenger-stop)
      (firefox-to-emacs-native-messenger-start)
      (let ((occurrences
             (length
              (cl-remove-if-not
               (lambda (h)
                 (eq h 'firefox-to-emacs-native-messenger--cleanup-on-shutdown))
               kill-emacs-hook))))
        (should (= occurrences 1))))))

(ert-deftest firefox-to-emacs-native-messenger-test-cleanup-on-shutdown-sweeps-tempfiles ()
  "`cleanup-on-shutdown' invokes the tempfile sweep against FILE-1600."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-sandbox-tempfile-dir tmp
    (with-temp-file (expand-file-name "tmp_doomed_a.txt" tmp) (insert "x"))
    (with-temp-file (expand-file-name "tmp_doomed_b.txt" tmp) (insert "x"))
    (with-temp-file (expand-file-name "keep.txt" tmp) (insert "x"))
    (firefox-to-emacs-native-messenger--cleanup-on-shutdown)
    (should-not (file-exists-p (expand-file-name "tmp_doomed_a.txt" tmp)))
    (should-not (file-exists-p (expand-file-name "tmp_doomed_b.txt" tmp)))
    (should (file-exists-p (expand-file-name "keep.txt" tmp)))))

(provide 'firefox-to-emacs-native-messenger-tests)
;;; firefox-to-emacs-native-messenger-tests.el ends here
