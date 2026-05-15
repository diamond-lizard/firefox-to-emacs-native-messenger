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
;;   Synchronously runs COMMAND-STRING via `/bin/sh -c`.  Stderr
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
`/bin/sh -c COMMAND-STRING'.  Stderr is merged into stdout
because `call-process' with a buffer DESTINATION captures both into the
same buffer when DESTINATION is a single buffer.  Returns a plist."
  (let ((output-buffer (generate-new-buffer
                        " *firefox-to-emacs-native-messenger-test-run*")))
    (unwind-protect
        (let ((exit (call-process "/bin/sh" nil
                                  (list output-buffer t)
                                  nil
                                  "-c" command-string)))
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


;;;; Phase 0500 -- accept handler, read timer, filter, dispatcher, sentinel

(defmacro firefox-to-emacs-native-messenger-test--with-accepted-client
    (server-side-var &rest body)
  "Start listener; open a client; bind SERVER-SIDE-VAR to the accepted process.

The bound process is the listener-side client endpoint registered in
the connection registry after the accept handler fires.  The macro
composes `with-listener-sandbox' and `with-fresh-connection-registry',
starts the listener, opens a local test-side client connection to the
bound socket, drains pending I/O via `accept-process-output' with a
short timeout to let the accept handler fire, binds SERVER-SIDE-VAR to
the first entry of the connection registry's live list, runs BODY, and
deletes the test-side client process on exit.

BODY is expected to assert with `should' that SERVER-SIDE-VAR is
non-nil before consulting it; the binding may be nil if the accept
handler did not fire (which itself indicates a Phase 0500 regression)."
  (declare (indent 1) (debug (symbolp body)))
  (let ((cache-sym (gensym "fenm-test-cache-"))
        (tmp-sym (gensym "fenm-test-tmp-"))
        (sock-sym (gensym "fenm-test-sock-"))
        (test-client-sym (gensym "fenm-test-client-")))
    `(firefox-to-emacs-native-messenger-test--with-listener-sandbox
         ,cache-sym ,tmp-sym ,sock-sym
       (firefox-to-emacs-native-messenger-test--with-fresh-connection-registry
        (firefox-to-emacs-native-messenger-start)
        (let ((,test-client-sym
               (make-network-process
                :name "fenm-test-accept-client"
                :family 'local
                :service ,sock-sym
                :coding '(binary . binary)
                :noquery t)))
          (unwind-protect
              (progn
                (accept-process-output nil 0.2)
                (let ((,server-side-var
                       (car
                        (firefox-to-emacs-native-messenger--connection-registry-list))))
                  ,@body))
            (when (process-live-p ,test-client-sym)
              (delete-process ,test-client-sym))))))))

(ert-deftest firefox-to-emacs-native-messenger-test-accept-handler-registers-connection ()
  "The accept handler records the new client in the connection registry.

After a client connects to the listener, the connection registry holds
exactly one live entry (the listener-side endpoint of the connection)."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-accepted-client client
    (should client)
    (should (memq client
                  (firefox-to-emacs-native-messenger--connection-registry-list)))
    (should (= 1
               (length
                (firefox-to-emacs-native-messenger--connection-registry-list))))))

(ert-deftest firefox-to-emacs-native-messenger-test-accept-handler-sets-noquery ()
  "The accept handler sets `process-query-on-exit-flag' to nil on the new client."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-accepted-client client
    (should client)
    (should-not (process-query-on-exit-flag client))))

(ert-deftest firefox-to-emacs-native-messenger-test-accept-handler-attaches-filter ()
  "The accept handler attaches the per-connection filter to the new client."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-accepted-client client
    (should client)
    (should (eq (process-filter client)
                'firefox-to-emacs-native-messenger--connection-filter))))

(ert-deftest firefox-to-emacs-native-messenger-test-accept-handler-attaches-sentinel ()
  "The accept handler attaches the per-connection sentinel to the new client."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-accepted-client client
    (should client)
    (should (eq (process-sentinel client)
                'firefox-to-emacs-native-messenger--connection-sentinel))))

(ert-deftest firefox-to-emacs-native-messenger-test-accept-handler-initializes-plist ()
  "The accept handler initializes the per-connection plist.

Required initial values: the read buffer is the empty unibyte string;
the declared length is nil (unset); the read timer is a live timer
object; the state field is the symbol `reading'."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-accepted-client client
    (should client)
    (let ((read-buffer
           (process-get
            client
            firefox-to-emacs-native-messenger--connection-key-read-buffer))
          (declared-length
           (process-get
            client
            firefox-to-emacs-native-messenger--connection-key-declared-length))
          (read-timer
           (process-get
            client
            firefox-to-emacs-native-messenger--connection-key-read-timer))
          (state
           (process-get
            client
            firefox-to-emacs-native-messenger--connection-key-state)))
      (should (stringp read-buffer))
      (should (string-empty-p read-buffer))
      (should-not (multibyte-string-p read-buffer))
      (should-not declared-length)
      (should (timerp read-timer))
      (should (eq state 'reading)))))

(ert-deftest firefox-to-emacs-native-messenger-test-accept-handler-logs-event ()
  "The accept handler emits a log entry naming the accept event."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-fresh-log-buffer
   (firefox-to-emacs-native-messenger-test--with-accepted-client client
     (should client)
     (let ((buf (get-buffer
                 firefox-to-emacs-native-messenger-log-buffer-name)))
       (should buf)
       (with-current-buffer buf
         (should (save-excursion
                   (goto-char (point-min))
                   (search-forward "accept" nil t))))))))

(ert-deftest firefox-to-emacs-native-messenger-test-read-timer-expire-deletes-connection ()
  "Read-timer expiration deletes the connection process.

After invoking the expiration handler on a live accepted client, the
client process is no longer live."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-accepted-client client
    (should client)
    (should (process-live-p client))
    (firefox-to-emacs-native-messenger--read-timer-expire client)
    (accept-process-output nil 0.1)
    (should-not (process-live-p client))))

(ert-deftest firefox-to-emacs-native-messenger-test-read-timer-expire-removes-from-registry ()
  "Read-timer expiration removes the connection from the registry.

The connection-registry hash table must not contain the expired
client after the expiration handler runs."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-accepted-client client
    (should client)
    (should (gethash client
                     firefox-to-emacs-native-messenger--connection-registry))
    (firefox-to-emacs-native-messenger--read-timer-expire client)
    (accept-process-output nil 0.1)
    (should-not
     (gethash client
              firefox-to-emacs-native-messenger--connection-registry))))

(ert-deftest firefox-to-emacs-native-messenger-test-read-timer-expire-logs ()
  "Read-timer expiration emits a log entry naming the event."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-fresh-log-buffer
   (firefox-to-emacs-native-messenger-test--with-accepted-client client
     (should client)
     (firefox-to-emacs-native-messenger--read-timer-expire client)
     (let ((buf (get-buffer
                 firefox-to-emacs-native-messenger-log-buffer-name)))
       (should buf)
       (with-current-buffer buf
         (should (save-excursion
                   (goto-char (point-min))
                   (search-forward "read timer" nil t))))))))

(ert-deftest firefox-to-emacs-native-messenger-test-cancel-read-timer-cancels-timer ()
  "Cancel removes the per-connection read timer from the active timer list.

Before cancel the timer object is present in timer-list; after cancel
the timer is no longer scheduled."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-accepted-client client
    (should client)
    (let ((timer
           (process-get
            client
            firefox-to-emacs-native-messenger--connection-key-read-timer)))
      (should (timerp timer))
      (should (member timer timer-list))
      (firefox-to-emacs-native-messenger--cancel-read-timer client)
      (should-not (member timer timer-list)))))

(ert-deftest firefox-to-emacs-native-messenger-test-cancel-read-timer-clears-plist ()
  "Cancel clears the read-timer plist key on the connection process.

After cancel the connection's read-timer plist key is nil."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-accepted-client client
    (should client)
    (firefox-to-emacs-native-messenger--cancel-read-timer client)
    (should-not
     (process-get
      client
      firefox-to-emacs-native-messenger--connection-key-read-timer))))

(ert-deftest firefox-to-emacs-native-messenger-test-cancel-read-timer-double-cancel-noop ()
  "Calling cancel a second time on a connection without a timer is harmless.

The second cancel raises no error and leaves the plist key nil."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-accepted-client client
    (should client)
    (firefox-to-emacs-native-messenger--cancel-read-timer client)
    (should-not
     (condition-case _err
         (progn
           (firefox-to-emacs-native-messenger--cancel-read-timer client)
           nil)
       (error t)))
    (should-not
     (process-get
      client
      firefox-to-emacs-native-messenger--connection-key-read-timer))))

(defun firefox-to-emacs-native-messenger-test--frame-bytes (json-string)
  "Build a length-prefixed frame for the test harness.

Returns a unibyte string consisting of the 4-byte little-endian
length prefix followed by the UTF-8 encoded bytes of JSON-STRING."
  (let* ((utf8 (encode-coding-string json-string 'utf-8))
         (prefix (firefox-to-emacs-native-messenger--pack-length
                  (length utf8))))
    (concat prefix utf8)))

(ert-deftest firefox-to-emacs-native-messenger-test-filter-reading-appends-chunks-and-waits ()
  "Reading-state filter appends chunks to the read buffer and waits for more.

Two short partial sends each fewer than four bytes leave the buffer
holding the concatenated bytes, the declared length unset, and the
connection alive."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-accepted-client client
    (should client)
    (firefox-to-emacs-native-messenger--connection-filter client "ab")
    (firefox-to-emacs-native-messenger--connection-filter client "c")
    (should (equal
             "abc"
             (process-get
              client
              firefox-to-emacs-native-messenger--connection-key-read-buffer)))
    (should-not
     (process-get
      client
      firefox-to-emacs-native-messenger--connection-key-declared-length))
    (should (process-live-p client))))

(ert-deftest firefox-to-emacs-native-messenger-test-filter-reading-decodes-prefix ()
  "Reading-state filter decodes the 4-byte length prefix once 4+ bytes arrive.

After receiving exactly 4 bytes representing length N, the
declared-length plist key is N, the connection remains alive, and
the dispatcher has not been invoked."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-accepted-client client
    (should client)
    (let ((prefix (firefox-to-emacs-native-messenger--pack-length 100))
          (calls 0))
      (cl-letf (((symbol-function
                  'firefox-to-emacs-native-messenger--dispatch-request)
                 (lambda (&rest _) (cl-incf calls))))
        (firefox-to-emacs-native-messenger--connection-filter client prefix))
      (should (= 100
                 (process-get
                  client
                  firefox-to-emacs-native-messenger--connection-key-declared-length)))
      (should (= 0 calls))
      (should (process-live-p client)))))

(ert-deftest firefox-to-emacs-native-messenger-test-filter-reading-zero-length-closes ()
  "A zero-length declared frame causes silent close with no dispatch."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-accepted-client client
    (should client)
    (let ((prefix (firefox-to-emacs-native-messenger--pack-length 0))
          (calls 0))
      (cl-letf (((symbol-function
                  'firefox-to-emacs-native-messenger--dispatch-request)
                 (lambda (&rest _) (cl-incf calls))))
        (firefox-to-emacs-native-messenger--connection-filter client prefix))
      (should (= 0 calls))
      (accept-process-output nil 0.05)
      (should-not (process-live-p client)))))

(ert-deftest firefox-to-emacs-native-messenger-test-filter-reading-frame-cap-exceeded-closes ()
  "Frames exceeding the inbound frame cap cause silent close per SEC-0900.

A declared length larger than the configured inbound frame cap causes
the filter to silently close the connection without invoking the
dispatcher and without sending any response."
  :tags '(:integration :sandbox)
  (let ((firefox-to-emacs-native-messenger-inbound-frame-cap 100))
    (firefox-to-emacs-native-messenger-test--with-accepted-client client
      (should client)
      (let ((prefix (firefox-to-emacs-native-messenger--pack-length 200))
            (calls 0))
        (cl-letf (((symbol-function
                    'firefox-to-emacs-native-messenger--dispatch-request)
                   (lambda (&rest _) (cl-incf calls))))
          (firefox-to-emacs-native-messenger--connection-filter client prefix))
        (should (= 0 calls))
        (accept-process-output nil 0.05)
        (should-not (process-live-p client))))))

(ert-deftest firefox-to-emacs-native-messenger-test-filter-reading-surplus-bytes-closes ()
  "Surplus bytes after the declared payload cause silent close.

Sending one byte more than the declared length is a violation of the
one-frame-per-connection contract and the filter closes the
connection without invoking the dispatcher."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-accepted-client client
    (should client)
    (let* ((json "{\"cmd\":\"version\"}")
           (frame (firefox-to-emacs-native-messenger-test--frame-bytes json))
           (with-trailing (concat frame "X"))
           (calls 0))
      (cl-letf (((symbol-function
                  'firefox-to-emacs-native-messenger--dispatch-request)
                 (lambda (&rest _) (cl-incf calls))))
        (firefox-to-emacs-native-messenger--connection-filter
         client with-trailing))
      (should (= 0 calls))
      (accept-process-output nil 0.05)
      (should-not (process-live-p client)))))

(ert-deftest firefox-to-emacs-native-messenger-test-filter-reading-underfilled-waits ()
  "Underfilled receipt leaves the connection alive without dispatching.

After receiving the 4-byte prefix declaring N bytes plus fewer than N
payload bytes, the filter buffers what it has, retains the declared
length, and waits for additional bytes without invoking the
dispatcher."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-accepted-client client
    (should client)
    (let* ((prefix (firefox-to-emacs-native-messenger--pack-length 100))
           (partial (concat prefix "hello"))
           (calls 0))
      (cl-letf (((symbol-function
                  'firefox-to-emacs-native-messenger--dispatch-request)
                 (lambda (&rest _) (cl-incf calls))))
        (firefox-to-emacs-native-messenger--connection-filter client partial))
      (should (= 0 calls))
      (should (= 100
                 (process-get
                  client
                  firefox-to-emacs-native-messenger--connection-key-declared-length)))
      (should (process-live-p client)))))

(ert-deftest firefox-to-emacs-native-messenger-test-filter-reading-exact-length-dispatches ()
  "A complete length-prefixed frame triggers the dispatcher with the parsed alist.

The dispatcher receives the original connection process and the
parsed request as an alist keyed by symbols."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-accepted-client client
    (should client)
    (let* ((json "{\"cmd\":\"version\"}")
           (frame (firefox-to-emacs-native-messenger-test--frame-bytes json))
           (captured-conn nil)
           (captured-request nil)
           (call-count 0))
      (cl-letf (((symbol-function
                  'firefox-to-emacs-native-messenger--dispatch-request)
                 (lambda (conn req)
                   (cl-incf call-count)
                   (setq captured-conn conn captured-request req))))
        (firefox-to-emacs-native-messenger--connection-filter client frame))
      (should (= 1 call-count))
      (should (eq captured-conn client))
      (should (equal "version" (cdr (assq 'cmd captured-request)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-filter-dispatched-state-deletes-process ()
  "Filter receiving bytes in dispatched state calls delete-process on CLIENT.

The dispatched state means a deferred-response handler is in flight;
any further bytes from the peer indicate a protocol violation, and
the filter terminates the connection."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-accepted-client client
    (should client)
    (process-put
     client
     firefox-to-emacs-native-messenger--connection-key-state
     'dispatched)
    (firefox-to-emacs-native-messenger--connection-filter client "anything")
    (accept-process-output nil 0.05)
    (should-not (process-live-p client))))

(ert-deftest firefox-to-emacs-native-messenger-test-filter-responded-state-ignores ()
  "Filter receiving bytes in responded state logs and ignores without buffer change."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-accepted-client client
    (should client)
    (process-put
     client
     firefox-to-emacs-native-messenger--connection-key-state
     'responded)
    (let ((saved-buf
           (process-get
            client
            firefox-to-emacs-native-messenger--connection-key-read-buffer)))
      (firefox-to-emacs-native-messenger--connection-filter client "ignored")
      (should (process-live-p client))
      (should (equal
               saved-buf
               (process-get
                client
                firefox-to-emacs-native-messenger--connection-key-read-buffer))))))

(ert-deftest firefox-to-emacs-native-messenger-test-filter-closing-state-ignores ()
  "Filter receiving bytes in closing state logs and ignores without buffer change."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-accepted-client client
    (should client)
    (process-put
     client
     firefox-to-emacs-native-messenger--connection-key-state
     'closing)
    (let ((saved-buf
           (process-get
            client
            firefox-to-emacs-native-messenger--connection-key-read-buffer)))
      (firefox-to-emacs-native-messenger--connection-filter client "ignored")
      (should (equal
               saved-buf
               (process-get
                client
                firefox-to-emacs-native-messenger--connection-key-read-buffer))))))

(ert-deftest firefox-to-emacs-native-messenger-test-filter-concurrent-connection-independence ()
  "Two concurrent connections maintain independent read buffers and declared lengths.

Streaming different partial frames to each connection does not
cross-contaminate the other's per-connection state."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-listener-sandbox
      cache tmp sock
    (firefox-to-emacs-native-messenger-test--with-fresh-connection-registry
     (firefox-to-emacs-native-messenger-start)
     (let ((c1 (make-network-process
                :name "fenm-test-c1"
                :family 'local
                :service sock
                :coding '(binary . binary)
                :noquery t))
           (c2 (make-network-process
                :name "fenm-test-c2"
                :family 'local
                :service sock
                :coding '(binary . binary)
                :noquery t)))
       (unwind-protect
           (progn
             (accept-process-output nil 0.2)
             (let* ((registered
                     (firefox-to-emacs-native-messenger--connection-registry-list))
                    (server-c1 (car registered))
                    (server-c2 (cadr registered)))
               (should server-c1)
               (should server-c2)
               (should (not (eq server-c1 server-c2)))
               (firefox-to-emacs-native-messenger--connection-filter
                server-c1 "abc")
               (firefox-to-emacs-native-messenger--connection-filter
                server-c2 "wxyz")
               (let ((b1 (process-get
                          server-c1
                          firefox-to-emacs-native-messenger--connection-key-read-buffer))
                     (b2 (process-get
                          server-c2
                          firefox-to-emacs-native-messenger--connection-key-read-buffer)))
                 (should (equal "abc" b1))
                 (should (equal "wxyz" b2)))))
         (when (process-live-p c1) (delete-process c1))
         (when (process-live-p c2) (delete-process c2)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-filter-parse-error-invalid-json-sends-error ()
  "Invalid JSON in a complete frame routes the generic error response.

The response object has cmd equal to error and a stringy error field;
no other fields are present per PAT-0300."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-accepted-client client
    (should client)
    (let* ((invalid "{not valid json")
           (frame
            (firefox-to-emacs-native-messenger-test--frame-bytes invalid))
           (sent '()))
      (cl-letf (((symbol-function
                  'firefox-to-emacs-native-messenger--write-response)
                 (lambda (conn resp) (push (cons conn resp) sent))))
        (firefox-to-emacs-native-messenger--connection-filter client frame))
      (should (= 1 (length sent)))
      (let ((resp (cdar sent)))
        (should (eq client (caar sent)))
        (should (equal "error" (alist-get 'cmd resp)))
        (should (stringp (alist-get 'error resp)))
        (should-not (alist-get 'code resp))))))

(ert-deftest firefox-to-emacs-native-messenger-test-filter-parse-error-malformed-utf8-sends-error ()
  "Malformed UTF-8 in a complete frame routes the generic error response."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-accepted-client client
    (should client)
    (let* ((bad-bytes (unibyte-string #xfe #xff))
           (prefix
            (firefox-to-emacs-native-messenger--pack-length
             (length bad-bytes)))
           (frame (concat prefix bad-bytes))
           (sent '()))
      (cl-letf (((symbol-function
                  'firefox-to-emacs-native-messenger--write-response)
                 (lambda (conn resp) (push (cons conn resp) sent))))
        (firefox-to-emacs-native-messenger--connection-filter client frame))
      (should (= 1 (length sent)))
      (let ((resp (cdar sent)))
        (should (equal "error" (alist-get 'cmd resp)))
        (should (stringp (alist-get 'error resp)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-filter-parse-error-resets-buffer ()
  "After a parse error the read-buffer plist key is reset to empty."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-accepted-client client
    (should client)
    (let* ((invalid "}}}not json")
           (frame
            (firefox-to-emacs-native-messenger-test--frame-bytes invalid)))
      (cl-letf (((symbol-function
                  'firefox-to-emacs-native-messenger--write-response)
                 (lambda (&rest _) nil)))
        (firefox-to-emacs-native-messenger--connection-filter client frame))
      (should (equal
               ""
               (process-get
                client
                firefox-to-emacs-native-messenger--connection-key-read-buffer))))))

(ert-deftest firefox-to-emacs-native-messenger-test-filter-parse-error-resets-declared-length ()
  "After a parse error the declared-length plist key is reset to nil."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-accepted-client client
    (should client)
    (let* ((invalid "}}}not json")
           (frame
            (firefox-to-emacs-native-messenger-test--frame-bytes invalid)))
      (cl-letf (((symbol-function
                  'firefox-to-emacs-native-messenger--write-response)
                 (lambda (&rest _) nil)))
        (firefox-to-emacs-native-messenger--connection-filter client frame))
      (should-not
       (process-get
        client
        firefox-to-emacs-native-messenger--connection-key-declared-length)))))

(ert-deftest firefox-to-emacs-native-messenger-test-filter-parse-error-does-not-call-dispatcher ()
  "A parse failure must not invoke the dispatcher."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-accepted-client client
    (should client)
    (let* ((invalid "(this is not json)")
           (frame
            (firefox-to-emacs-native-messenger-test--frame-bytes invalid))
           (dispatch-calls 0))
      (cl-letf (((symbol-function
                  'firefox-to-emacs-native-messenger--dispatch-request)
                 (lambda (&rest _) (cl-incf dispatch-calls)))
                ((symbol-function
                  'firefox-to-emacs-native-messenger--write-response)
                 (lambda (&rest _) nil)))
        (firefox-to-emacs-native-messenger--connection-filter client frame))
      (should (= 0 dispatch-calls)))))

(ert-deftest firefox-to-emacs-native-messenger-test-dispatcher-known-cmd-routes-to-handler ()
  "A cmd registered in the handler table is invoked with the connection and request."
  :tags '(:unit)
  (let ((calls 0)
        (captured nil))
    (let ((firefox-to-emacs-native-messenger--handlers
           (let ((h (make-hash-table :test 'equal)))
             (puthash "test-known"
                      (lambda (_c req)
                        (cl-incf calls)
                        (setq captured req)
                        (list (cons 'cmd "test-known-response")))
                      h)
             h)))
      (let ((resp
             (firefox-to-emacs-native-messenger--dispatch-request
              nil
              (list (cons 'cmd "test-known") (cons 'foo 1)))))
        (should (= 1 calls))
        (should (equal "test-known" (alist-get 'cmd captured)))
        (should (equal 1 (alist-get 'foo captured)))
        (should (equal "test-known-response" (alist-get 'cmd resp)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-dispatcher-unknown-cmd-error ()
  "An unknown cmd produces the unhandled-message generic error response.

The response carries cmd equal to error and error equal to the
unhandled-message literal; no other fields are present per PAT-0300."
  :tags '(:unit)
  (let ((firefox-to-emacs-native-messenger--handlers
         (make-hash-table :test 'equal)))
    (let ((resp
           (firefox-to-emacs-native-messenger--dispatch-request
            nil
            (list (cons 'cmd "nonexistent-cmd")))))
      (should (equal "error" (alist-get 'cmd resp)))
      (should (equal "Unhandled message" (alist-get 'error resp)))
      (should-not (alist-get 'code resp)))))

(ert-deftest firefox-to-emacs-native-messenger-test-dispatcher-missing-cmd-error ()
  "A request without a cmd field produces a generic error identifying the missing field."
  :tags '(:unit)
  (let ((resp
         (firefox-to-emacs-native-messenger--dispatch-request
          nil
          (list (cons 'foo 1)))))
    (should (equal "error" (alist-get 'cmd resp)))
    (should (string-match-p "cmd" (alist-get 'error resp)))))

(ert-deftest firefox-to-emacs-native-messenger-test-dispatcher-ill-typed-cmd-error ()
  "A request with a non-string cmd produces the unhandled-message error.

Non-string values for the cmd field (integer, list, nil, vector, etc.)
are treated identically to unknown cmd values."
  :tags '(:unit)
  (dolist (bad-cmd (list 42 'a-symbol '(1 2) nil [1 2]))
    (let ((resp
           (firefox-to-emacs-native-messenger--dispatch-request
            nil
            (list (cons 'cmd bad-cmd)))))
      (should (equal "error" (alist-get 'cmd resp)))
      (should (equal "Unhandled message" (alist-get 'error resp))))))

(ert-deftest firefox-to-emacs-native-messenger-test-dispatcher-handler-signal-becomes-error ()
  "A signal raised inside a handler becomes a generic error with the signal's message.

The condition-case-unless-debug wrapper catches the error and
converts it into the generic error response shape."
  :tags '(:unit)
  (let ((debug-on-error nil)
        (firefox-to-emacs-native-messenger--handlers
         (let ((h (make-hash-table :test 'equal)))
           (puthash "boom"
                    (lambda (_c _r) (error "kaboom-marker"))
                    h)
           h)))
    (let ((resp
           (firefox-to-emacs-native-messenger--dispatch-request
            nil
            (list (cons 'cmd "boom")))))
      (should (equal "error" (alist-get 'cmd resp)))
      (should (string-match-p "kaboom-marker" (alist-get 'error resp))))))

(ert-deftest firefox-to-emacs-native-messenger-test-sentinel-cancels-read-timer ()
  "On a close event the sentinel cancels the per-connection read timer.

After invocation the read-timer plist key is nil and the timer is
no longer scheduled in timer-list."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-accepted-client client
    (should client)
    (let ((timer
           (process-get
            client
            firefox-to-emacs-native-messenger--connection-key-read-timer)))
      (should (timerp timer))
      (firefox-to-emacs-native-messenger--connection-sentinel
       client "connection broken by remote peer\n")
      (should-not
       (process-get
        client
        firefox-to-emacs-native-messenger--connection-key-read-timer))
      (should-not (member timer timer-list)))))

(ert-deftest firefox-to-emacs-native-messenger-test-sentinel-sets-state-closing ()
  "On a close event the sentinel sets the connection state to closing."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-accepted-client client
    (should client)
    (firefox-to-emacs-native-messenger--connection-sentinel
     client "deleted\n")
    (should
     (eq 'closing
         (process-get
          client
          firefox-to-emacs-native-messenger--connection-key-state)))))

(ert-deftest firefox-to-emacs-native-messenger-test-sentinel-removes-from-registry ()
  "On a close event the sentinel removes the connection from the registry."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-accepted-client client
    (should client)
    (should
     (gethash client
              firefox-to-emacs-native-messenger--connection-registry))
    (firefox-to-emacs-native-messenger--connection-sentinel
     client "connection broken by remote peer\n")
    (should-not
     (gethash client
              firefox-to-emacs-native-messenger--connection-registry))))

(ert-deftest firefox-to-emacs-native-messenger-test-sentinel-logs-premature-close ()
  "Peer close before a response causes a premature-close log entry.

The connection's prior state is reading (the default after accept);
the sentinel's log message must identify the close as premature."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-fresh-log-buffer
   (firefox-to-emacs-native-messenger-test--with-accepted-client client
     (should client)
     (firefox-to-emacs-native-messenger--connection-sentinel
      client "connection broken by remote peer\n")
     (let ((buf (get-buffer
                 firefox-to-emacs-native-messenger-log-buffer-name)))
       (should buf)
       (with-current-buffer buf
         (should
          (save-excursion
            (goto-char (point-min))
            (re-search-forward "premature" nil t))))))))

(ert-deftest firefox-to-emacs-native-messenger-test-sentinel-logs-peer-close-after-response ()
  "Peer close after the writer marked responded causes a post-response log entry.

The connection's prior state is responded; the sentinel's log message
must identify the close as occurring after the response was sent."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-fresh-log-buffer
   (firefox-to-emacs-native-messenger-test--with-accepted-client client
     (should client)
     (process-put
      client
      firefox-to-emacs-native-messenger--connection-key-state
      'responded)
     (firefox-to-emacs-native-messenger--connection-sentinel
      client "connection broken by remote peer\n")
     (let ((buf (get-buffer
                 firefox-to-emacs-native-messenger-log-buffer-name)))
       (should buf)
       (with-current-buffer buf
         (should
          (save-excursion
            (goto-char (point-min))
            (re-search-forward "after response" nil t))))))))


;;;; Phase 0600 -- response builder, serializer, writer, cleanup timer

(ert-deftest firefox-to-emacs-native-messenger-test-build-response-no-absent-passes-through ()
  "A response with no `:absent' fields is returned with all fields preserved.

The structural null-stripping rule (PROTOCOL.md S6) strips only fields
whose value is the sentinel `:absent'; every other value, including
strings, numbers, and the empty string, MUST be preserved verbatim."
  :tags '(:unit :fixture-driven)
  (let* ((input '((cmd . "version") (version . "0.3.7") (code . 0)))
         (built (firefox-to-emacs-native-messenger--build-response input)))
    (should (equal built input))))

(ert-deftest firefox-to-emacs-native-messenger-test-build-response-version-success-shape ()
  "Builder output for a `version' handler-style input matches the fixture.

The `version-success' fixture's response shape contains no absent
fields; the builder passes the handler-style input through unchanged."
  :tags '(:unit :fixture-driven)
  (let* ((handler-output '((cmd . "version") (version . "0.3.7") (code . 0)))
         (built (firefox-to-emacs-native-messenger--build-response handler-output))
         (fixture (firefox-to-emacs-native-messenger-test-load-fixture
                   "version-success"))
         (expected-response (alist-get 'response fixture)))
    (should (equal built expected-response))))

(ert-deftest firefox-to-emacs-native-messenger-test-build-response-strips-absent-fields ()
  "Fields whose value is the keyword `:absent' MUST be stripped.

Per PROTOCOL.md S6, structural absence is preserved on the wire; the
sentinel `:absent' is the in-process marker telling the builder to omit
the field."
  :tags '(:unit)
  (let* ((input '((cmd . "getconfigpath") (content . :absent) (code . 1)))
         (built (firefox-to-emacs-native-messenger--build-response input)))
    (should (equal built '((cmd . "getconfigpath") (code . 1))))))

(ert-deftest firefox-to-emacs-native-messenger-test-build-response-getconfigpath-empty-shape ()
  "Builder strips the absent content field to match `getconfigpath-empty'.

The handler-style input for the empty-result branch uses `:absent' for
the content field; the builder's output MUST equal the fixture's
response shape (which has no content key)."
  :tags '(:unit :fixture-driven)
  (let* ((handler-output '((cmd . "getconfigpath") (content . :absent) (code . 1)))
         (built (firefox-to-emacs-native-messenger--build-response handler-output))
         (fixture (firefox-to-emacs-native-messenger-test-load-fixture
                   "getconfigpath-empty"))
         (expected-response (alist-get 'response fixture)))
    (should (equal built expected-response))))

(ert-deftest firefox-to-emacs-native-messenger-test-build-response-preserves-empty-string ()
  "An explicit empty-string value MUST be preserved (not stripped).

The `read' open-failure response has `content' set to the empty string
(distinct from absence): PROTOCOL.md S5 and S6.  Stripping the empty
string would conflate two distinct wire shapes."
  :tags '(:unit :fixture-driven)
  (let* ((input '((cmd . "read") (content . "") (code . 2)))
         (built (firefox-to-emacs-native-messenger--build-response input))
         (fixture (firefox-to-emacs-native-messenger-test-load-fixture
                   "read-open-failure"))
         (expected-response (alist-get 'response fixture)))
    (should (equal built input))
    (should (equal built expected-response))))

(ert-deftest firefox-to-emacs-native-messenger-test-build-response-preserves-zero ()
  "An explicit zero value MUST be preserved (not stripped).

Per PROTOCOL.md S6, integers including zero are preserved verbatim;
stripping zero would lose semantic information (e.g., success code)."
  :tags '(:unit)
  (let* ((input '((cmd . "version") (version . "0.0.0") (code . 0)))
         (built (firefox-to-emacs-native-messenger--build-response input)))
    (should (equal built input))))

(ert-deftest firefox-to-emacs-native-messenger-test-build-response-preserves-false ()
  "An explicit `:false' value MUST be preserved (not stripped).

Per PROTOCOL.md S6, boolean false (represented as `:false' to match the
parser's :false-object) is preserved.  No in-scope handler currently
uses boolean fields, but the structural rule applies uniformly."
  :tags '(:unit)
  (let* ((input '((cmd . "x") (flag . :false)))
         (built (firefox-to-emacs-native-messenger--build-response input)))
    (should (equal built input))))

(ert-deftest firefox-to-emacs-native-messenger-test-build-response-multiple-absent ()
  "Multiple `:absent' fields are all stripped in one pass."
  :tags '(:unit)
  (let* ((input '((cmd . "x") (a . :absent) (b . 1) (c . :absent) (d . 2)))
         (built (firefox-to-emacs-native-messenger--build-response input)))
    (should (equal built '((cmd . "x") (b . 1) (d . 2))))))

(ert-deftest firefox-to-emacs-native-messenger-test-build-response-preserves-insertion-order ()
  "The builder preserves cmd-first insertion order across stripping."
  :tags '(:unit)
  (let* ((input '((cmd . "x") (a . :absent) (b . 1) (c . :absent) (d . 2)))
         (built (firefox-to-emacs-native-messenger--build-response input)))
    (should (eq 'cmd (caar built)))
    (should (equal '(cmd b d) (mapcar #'car built)))))

(ert-deftest firefox-to-emacs-native-messenger-test-build-response-output-is-json-serializable ()
  "Build output passes through `json-serialize' to produce valid JSON.

The builder's contract is that its output is JSON-serializable by the
response serializer; this test validates the contract empirically by
serializing then re-parsing and asserting structural identity."
  :tags '(:unit)
  (let* ((input '((cmd . "version") (version . "0.3.7") (code . 0)))
         (built (firefox-to-emacs-native-messenger--build-response input))
         (json (json-serialize built :null-object :null :false-object :false))
         (round-tripped (json-parse-string
                         json :object-type 'alist :null-object nil
                         :false-object :false)))
    (should (equal round-tripped input))))

(ert-deftest firefox-to-emacs-native-messenger-test-build-response-empty-alist ()
  "An empty input alist is returned unchanged.

The empty alist is a degenerate case (no handler should return it);
the builder accepts it as-is rather than signaling, leaving safety to
the writer's downstream checks."
  :tags '(:unit)
  (should (equal nil (firefox-to-emacs-native-messenger--build-response nil)))
  (should (equal '() (firefox-to-emacs-native-messenger--build-response '()))))

(ert-deftest firefox-to-emacs-native-messenger-test-build-response-no-side-effects ()
  "The builder MUST NOT mutate its input alist.

Re-running the builder on the same input MUST yield equal results;
the input alist MUST be unchanged after the call (no destructive
modification of cons cells)."
  :tags '(:unit)
  (let* ((input (copy-tree '((cmd . "x") (a . :absent) (b . 1))))
         (snapshot (copy-tree input))
         (built1 (firefox-to-emacs-native-messenger--build-response input))
         (built2 (firefox-to-emacs-native-messenger--build-response input)))
    (should (equal input snapshot))
    (should (equal built1 built2))))

(ert-deftest firefox-to-emacs-native-messenger-test-serialize-response-ascii ()
  "Serializer produces a 4-byte little-endian prefix plus UTF-8 JSON payload.

For an ASCII response, the byte length and character length coincide;
the prefix is the JSON byte length packed little-endian, and the
output unibyte string is exactly prefix+payload."
  :tags '(:unit)
  (let* ((input '((cmd . "version") (version . "0.3.7") (code . 0)))
         (framed (firefox-to-emacs-native-messenger--serialize-response input))
         (declared (firefox-to-emacs-native-messenger--unpack-length
                    (substring framed 0 4)))
         (payload (substring framed 4))
         (json (json-serialize input
                               :null-object :null
                               :false-object :false)))
    (should-not (multibyte-string-p framed))
    (should (= declared (length payload)))
    (should (= declared (string-bytes json)))
    (should (equal (decode-coding-string payload 'utf-8) json))))

(ert-deftest firefox-to-emacs-native-messenger-test-serialize-response-multibyte ()
  "Serializer's length prefix reflects UTF-8 byte length, not character count.

A response carrying non-ASCII characters serializes to a JSON string
whose UTF-8 byte representation has more bytes than characters; the
prefix MUST be the byte length, not the character count.  Using `length'
on the original multibyte JSON would understate the byte count and
produce a too-small length prefix."
  :tags '(:unit)
  (let* ((input '((cmd . "read") (content . "héllo") (code . 0)))
         (expected-json "{\"cmd\":\"read\",\"content\":\"héllo\",\"code\":0}")
         (framed (firefox-to-emacs-native-messenger--serialize-response input))
         (declared (firefox-to-emacs-native-messenger--unpack-length
                    (substring framed 0 4)))
         (payload (substring framed 4))
         (json (json-serialize input
                               :null-object :null
                               :false-object :false)))
    (should-not (multibyte-string-p framed))
    (should (= declared (length payload)))
    (should (= declared (string-bytes json)))
    (should (= declared (string-bytes expected-json)))
    (should (> (string-bytes expected-json) (length expected-json)))))

(ert-deftest firefox-to-emacs-native-messenger-test-serialize-response-little-endian-prefix ()
  "The 4-byte length prefix is little-endian (low byte first).

Each of the four prefix bytes is asserted to equal the corresponding
8-bit slice of the payload length, lowest-order byte first."
  :tags '(:unit)
  (let* ((padding (make-string 233 ?x))
         (input (list (cons 'cmd "x") (cons 'data padding)))
         (framed (firefox-to-emacs-native-messenger--serialize-response input))
         (payload-len (length (substring framed 4))))
    (should (= (aref framed 0) (logand payload-len #xff)))
    (should (= (aref framed 1) (logand (ash payload-len -8) #xff)))
    (should (= (aref framed 2) (logand (ash payload-len -16) #xff)))
    (should (= (aref framed 3) (logand (ash payload-len -24) #xff)))))

(ert-deftest firefox-to-emacs-native-messenger-test-serialize-response-roundtrip ()
  "Framed payload, when unpacked and JSON-parsed, yields the original alist.

The full pipeline (serialize -> unpack length -> decode UTF-8 -> parse
JSON) is exercised end-to-end on a representative response."
  :tags '(:unit :fixture-driven)
  (let* ((input '((cmd . "version") (version . "0.3.7") (code . 0)))
         (framed (firefox-to-emacs-native-messenger--serialize-response input))
         (declared (firefox-to-emacs-native-messenger--unpack-length
                    (substring framed 0 4)))
         (payload (substring framed 4 (+ 4 declared)))
         (parsed (json-parse-string
                  (decode-coding-string payload 'utf-8)
                  :object-type 'alist
                  :null-object nil
                  :false-object :false)))
    (should (equal parsed input))))

(ert-deftest firefox-to-emacs-native-messenger-test-serialize-response-preserves-false ()
  "An explicit `:false' value serializes as JSON `false' and round-trips back.

The :false-object marker matches the parser's configuration, so the
roundtrip preserves `:false' verbatim."
  :tags '(:unit)
  (let* ((input '((cmd . "x") (flag . :false)))
         (framed (firefox-to-emacs-native-messenger--serialize-response input))
         (declared (firefox-to-emacs-native-messenger--unpack-length
                    (substring framed 0 4)))
         (payload (substring framed 4 (+ 4 declared)))
         (json (decode-coding-string payload 'utf-8))
         (parsed (json-parse-string json
                                    :object-type 'alist
                                    :null-object nil
                                    :false-object :false)))
    (should (string-match-p "\"flag\":false" json))
    (should (equal parsed input))))

(ert-deftest firefox-to-emacs-native-messenger-test-serialize-response-version-success-fixture ()
  "Framed payload's decoded JSON parses to the `version-success' fixture response.

End-to-end: the handler-shaped input, serialized and round-tripped,
MUST equal the `version-success' fixture's `response' sub-alist."
  :tags '(:unit :fixture-driven)
  (let* ((input '((cmd . "version") (version . "0.3.7") (code . 0)))
         (framed (firefox-to-emacs-native-messenger--serialize-response input))
         (declared (firefox-to-emacs-native-messenger--unpack-length
                    (substring framed 0 4)))
         (payload (substring framed 4 (+ 4 declared)))
         (parsed (json-parse-string
                  (decode-coding-string payload 'utf-8)
                  :object-type 'alist
                  :null-object nil
                  :false-object :false))
         (fixture (firefox-to-emacs-native-messenger-test-load-fixture
                   "version-success"))
         (expected (alist-get 'response fixture)))
    (should (equal parsed expected))))

(ert-deftest firefox-to-emacs-native-messenger-test-serialize-response-prefix-length-accurate ()
  "The 4-byte prefix is exactly the byte length of the JSON payload.

Verifies that the prefix can be used to slice the framed payload
into prefix and JSON body with no leftover bytes."
  :tags '(:unit)
  (let* ((input '((cmd . "x") (data . "abc")))
         (framed (firefox-to-emacs-native-messenger--serialize-response input))
         (declared (firefox-to-emacs-native-messenger--unpack-length
                    (substring framed 0 4))))
    (should (= (length framed) (+ 4 declared)))))

(ert-deftest firefox-to-emacs-native-messenger-test-serialize-response-unibyte-output ()
  "The serializer's output is a unibyte string, ready for `process-send-string'.

A multibyte string would be problematic for accurate byte-length
arithmetic and for transmission over a binary channel."
  :tags '(:unit)
  (let* ((input '((cmd . "read") (content . "héllo")))
         (framed (firefox-to-emacs-native-messenger--serialize-response input)))
    (should (stringp framed))
    (should-not (multibyte-string-p framed))))

(defmacro firefox-to-emacs-native-messenger-test--with-paired-clients
    (server-var test-var &rest body)
  "Listener + test-side client setup exposing both sides for writer tests.

SERVER-VAR is bound to the listener-side accepted client process
extracted from the connection registry.  TEST-VAR is bound to the
test-side client process; its filter accumulates received bytes into
the test-side process's `received' plist key (read it via
(process-get TEST-VAR 'received)).

BODY can call the response writer on SERVER-VAR and drain I/O via
`accept-process-output' to observe the bytes that propagate to the
test side."
  (declare (indent 2) (debug (symbolp symbolp body)))
  (let ((cache-sym (gensym "fenm-test-cache-"))
        (tmp-sym (gensym "fenm-test-tmp-"))
        (sock-sym (gensym "fenm-test-sock-")))
    `(firefox-to-emacs-native-messenger-test--with-listener-sandbox
         ,cache-sym ,tmp-sym ,sock-sym
       (firefox-to-emacs-native-messenger-test--with-fresh-connection-registry
        (firefox-to-emacs-native-messenger-start)
        (let ((,test-var
               (make-network-process
                :name "fenm-test-paired-client"
                :family 'local
                :service ,sock-sym
                :coding '(binary . binary)
                :noquery t
                :filter (lambda (proc str)
                          (process-put
                           proc 'received
                           (concat (or (process-get proc 'received) "")
                                   str))))))
          (unwind-protect
              (progn
                (accept-process-output nil 0.2)
                (let ((,server-var
                       (car
                        (firefox-to-emacs-native-messenger--connection-registry-list))))
                  ,@body))
            (when (process-live-p ,test-var)
              (delete-process ,test-var))))))))

(ert-deftest firefox-to-emacs-native-messenger-test-write-response-state-responded-noop ()
  "Writer is a no-op when state is `responded' (pre-send guard).

Per PAT-0600 / Section 8.18, the writer's pre-send guard refuses to
write when the connection has already transitioned past the
`reading'/`dispatched' allowlist.  No bytes go on the wire."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-paired-clients server tc
    (should server)
    (process-put
     server
     firefox-to-emacs-native-messenger--connection-key-state
     'responded)
    (firefox-to-emacs-native-messenger--write-response
     server '((cmd . "version") (version . "0.3.7") (code . 0)))
    (accept-process-output nil 0.1)
    (should-not (process-get tc 'received))))

(ert-deftest firefox-to-emacs-native-messenger-test-write-response-state-closing-noop ()
  "Writer is a no-op when state is `closing' (pre-send guard)."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-paired-clients server tc
    (should server)
    (process-put
     server
     firefox-to-emacs-native-messenger--connection-key-state
     'closing)
    (firefox-to-emacs-native-messenger--write-response
     server '((cmd . "version") (version . "0.3.7") (code . 0)))
    (accept-process-output nil 0.1)
    (should-not (process-get tc 'received))))

(ert-deftest firefox-to-emacs-native-messenger-test-write-response-dead-process-noop ()
  "Writer is a no-op (no error) when CLIENT is no longer live."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-paired-clients server _tc
    (should server)
    (delete-process server)
    (firefox-to-emacs-native-messenger--write-response
     server '((cmd . "version") (version . "0.3.7") (code . 0)))))

(ert-deftest firefox-to-emacs-native-messenger-test-write-response-reading-state-sends-frame ()
  "Writer sends a framed response when state is `reading'.

The test-side client's filter accumulates the bytes; the bytes are
length-prefixed UTF-8 JSON that round-trips to the input response."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-paired-clients server tc
    (should server)
    (firefox-to-emacs-native-messenger--write-response
     server '((cmd . "version") (version . "0.3.7") (code . 0)))
    (accept-process-output nil 0.2)
    (let* ((received (process-get tc 'received))
           (declared (firefox-to-emacs-native-messenger--unpack-length
                      (substring received 0 4)))
           (payload (substring received 4 (+ 4 declared)))
           (parsed (json-parse-string
                    (decode-coding-string payload 'utf-8)
                    :object-type 'alist
                    :null-object nil
                    :false-object :false)))
      (should (= 4 (- (length received) declared)))
      (should (equal parsed
                     '((cmd . "version") (version . "0.3.7") (code . 0)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-write-response-dispatched-state-sends-frame ()
  "Writer sends a framed response when state is `dispatched'.

The `dispatched' state is reserved for async handlers that defer the
response (Phase 0800); the writer MUST accept this state."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-paired-clients server tc
    (should server)
    (process-put
     server
     firefox-to-emacs-native-messenger--connection-key-state
     'dispatched)
    (firefox-to-emacs-native-messenger--write-response
     server '((cmd . "version") (version . "0.3.7") (code . 0)))
    (accept-process-output nil 0.2)
    (let* ((received (process-get tc 'received))
           (declared (firefox-to-emacs-native-messenger--unpack-length
                      (substring received 0 4)))
           (payload (substring received 4 (+ 4 declared)))
           (parsed (json-parse-string
                    (decode-coding-string payload 'utf-8)
                    :object-type 'alist
                    :null-object nil
                    :false-object :false)))
      (should (equal parsed
                     '((cmd . "version") (version . "0.3.7") (code . 0)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-write-response-transitions-state-to-responded ()
  "After a successful send the connection state is `responded'."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-paired-clients server _tc
    (should server)
    (firefox-to-emacs-native-messenger--write-response
     server '((cmd . "version") (version . "0.3.7") (code . 0)))
    (should (eq 'responded
                (process-get
                 server
                 firefox-to-emacs-native-messenger--connection-key-state)))))

(ert-deftest firefox-to-emacs-native-messenger-test-write-response-marks-responded-before-send ()
  "State transitions to `responded' BEFORE `process-send-string' runs.

The pre-send mark is observable from inside an instrumented
`process-send-string' replacement: at the moment the send is invoked,
the connection state MUST already be `responded'."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-paired-clients server _tc
    (should server)
    (let ((observed nil)
          (orig (symbol-function 'process-send-string)))
      (cl-letf (((symbol-function 'process-send-string)
                 (lambda (proc str)
                   (when (eq proc server)
                     (setq observed
                           (process-get
                            proc
                            firefox-to-emacs-native-messenger--connection-key-state)))
                   (funcall orig proc str))))
        (firefox-to-emacs-native-messenger--write-response
         server '((cmd . "x"))))
      (should (eq observed 'responded)))))

(ert-deftest firefox-to-emacs-native-messenger-test-write-response-starts-cleanup-timer ()
  "After a successful send the cleanup-timer plist key holds a live timer."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-paired-clients server _tc
    (should server)
    (firefox-to-emacs-native-messenger--write-response
     server '((cmd . "version") (version . "0.3.7") (code . 0)))
    (let ((timer (process-get
                  server
                  firefox-to-emacs-native-messenger--connection-key-cleanup-timer)))
      (should (timerp timer))
      (should (member timer timer-list)))))

(ert-deftest firefox-to-emacs-native-messenger-test-write-response-oversized-replaced-with-error ()
  "An oversized response is replaced exactly once with the `response too large' error.

With the outbound cap reduced to a small value, the response's
serialized payload exceeds the cap; the writer MUST send the generic
oversized-response error fixture instead."
  :tags '(:integration :sandbox :fixture-driven)
  (firefox-to-emacs-native-messenger-test--with-paired-clients server tc
    (should server)
    (let ((firefox-to-emacs-native-messenger-outbound-response-cap 50))
      (firefox-to-emacs-native-messenger--write-response
       server
       (list (cons 'cmd "version")
             (cons 'version "0.3.7")
             (cons 'code 0)
             (cons 'padding (make-string 500 ?x))))
      (accept-process-output nil 0.2)
      (let* ((received (process-get tc 'received))
             (declared (firefox-to-emacs-native-messenger--unpack-length
                        (substring received 0 4)))
             (payload (substring received 4 (+ 4 declared)))
             (parsed (json-parse-string
                      (decode-coding-string payload 'utf-8)
                      :object-type 'alist
                      :null-object nil
                      :false-object :false)))
        (should (equal "error" (alist-get 'cmd parsed)))
        (should (equal "response too large" (alist-get 'error parsed)))
        (should-not (alist-get 'code parsed))))))

(ert-deftest firefox-to-emacs-native-messenger-test-write-response-degenerate-sends-no-frame ()
  "When the replacement error response also exceeds the cap, no frame is sent.

Configure an absurdly small cap (e.g., 5 bytes) so even
`{\"cmd\":\"error\",\"error\":\"response too large\"}' overflows.  The
writer MUST log a critical message, mark state `responded', start the
cleanup timer, and send NO bytes."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-paired-clients server tc
    (should server)
    (let ((firefox-to-emacs-native-messenger-outbound-response-cap 5))
      (firefox-to-emacs-native-messenger--write-response
       server
       '((cmd . "version") (version . "0.3.7") (code . 0)))
      (accept-process-output nil 0.2)
      (should-not (process-get tc 'received))
      (should (eq 'responded
                  (process-get
                   server
                   firefox-to-emacs-native-messenger--connection-key-state)))
      (should (timerp
               (process-get
                server
                firefox-to-emacs-native-messenger--connection-key-cleanup-timer))))))

(ert-deftest firefox-to-emacs-native-messenger-test-write-response-degenerate-logs-critical ()
  "The degenerate-fallback path emits a critical (error-level) log entry."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-fresh-log-buffer
   (firefox-to-emacs-native-messenger-test--with-paired-clients server _tc
     (should server)
     (let ((firefox-to-emacs-native-messenger-outbound-response-cap 5))
       (firefox-to-emacs-native-messenger--write-response
        server
        '((cmd . "version") (version . "0.3.7") (code . 0)))
       (accept-process-output nil 0.1)
       (let ((buf (get-buffer
                   firefox-to-emacs-native-messenger-log-buffer-name)))
         (should buf)
         (with-current-buffer buf
           (should
            (save-excursion
              (goto-char (point-min))
              (re-search-forward "ERROR" nil t)))))))))

(ert-deftest firefox-to-emacs-native-messenger-test-write-response-send-error-still-marks-responded ()
  "Even when `process-send-string' raises, state is `responded' and timer is scheduled.

The mark-responded-before-send invariant plus the unwind-protect
cleanup-timer scheduling are both observable in the error path."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-paired-clients server _tc
    (should server)
    (let ((debug-on-error nil))
      (cl-letf (((symbol-function 'process-send-string)
                 (lambda (&rest _) (error "synthetic-send-failure"))))
        (firefox-to-emacs-native-messenger--write-response
         server '((cmd . "x")))))
    (should (eq 'responded
                (process-get
                 server
                 firefox-to-emacs-native-messenger--connection-key-state)))
    (should (timerp
             (process-get
              server
              firefox-to-emacs-native-messenger--connection-key-cleanup-timer)))))

(ert-deftest firefox-to-emacs-native-messenger-test-cleanup-timer-expire-deletes-live-connection ()
  "Expiration on a live connection deletes the process.

Per Section 8.22, cleanup-timer expiration transitions state from
`responded' to `closing' and tears down the connection (process
deletion plus registry removal)."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-paired-clients server _tc
    (should server)
    (should (process-live-p server))
    (firefox-to-emacs-native-messenger--cleanup-timer-expire server)
    (should-not (process-live-p server))))

(ert-deftest firefox-to-emacs-native-messenger-test-cleanup-timer-expire-sets-state-closing ()
  "Expiration on a live connection transitions state to `closing'."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-paired-clients server _tc
    (should server)
    (firefox-to-emacs-native-messenger--cleanup-timer-expire server)
    (should (eq 'closing
                (process-get
                 server
                 firefox-to-emacs-native-messenger--connection-key-state)))))

(ert-deftest firefox-to-emacs-native-messenger-test-cleanup-timer-expire-removes-from-registry ()
  "Expiration removes the connection from the connection registry."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-paired-clients server _tc
    (should server)
    (should (gethash server
                     firefox-to-emacs-native-messenger--connection-registry))
    (firefox-to-emacs-native-messenger--cleanup-timer-expire server)
    (should-not (gethash server
                         firefox-to-emacs-native-messenger--connection-registry))))

(ert-deftest firefox-to-emacs-native-messenger-test-cleanup-timer-expire-clears-plist-key ()
  "Expiration clears the cleanup-timer plist key.

Even if the timer was the one that fired, the plist key is cleared to
signal that no cleanup timer is currently scheduled."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-paired-clients server _tc
    (should server)
    (firefox-to-emacs-native-messenger--start-cleanup-timer server)
    (firefox-to-emacs-native-messenger--cleanup-timer-expire server)
    (should-not
     (process-get
      server
      firefox-to-emacs-native-messenger--connection-key-cleanup-timer))))

(ert-deftest firefox-to-emacs-native-messenger-test-cleanup-timer-expire-on-deleted-noop ()
  "Expiration on an already-deleted connection is a silent no-op.

The handler MUST NOT raise when called on a process that was already
deleted (the sentinel won the race and torn down the connection
first)."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-paired-clients server _tc
    (should server)
    (delete-process server)
    (should-not (process-live-p server))
    (firefox-to-emacs-native-messenger--cleanup-timer-expire server)))

(ert-deftest firefox-to-emacs-native-messenger-test-cancel-cleanup-timer-removes-from-timer-list ()
  "Cancel removes the scheduled timer from `timer-list' and clears the plist key."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-paired-clients server _tc
    (should server)
    (let ((timer
           (firefox-to-emacs-native-messenger--start-cleanup-timer server)))
      (should (timerp timer))
      (should (member timer timer-list))
      (firefox-to-emacs-native-messenger--cancel-cleanup-timer server)
      (should-not (member timer timer-list))
      (should-not
       (process-get
        server
        firefox-to-emacs-native-messenger--connection-key-cleanup-timer)))))

(ert-deftest firefox-to-emacs-native-messenger-test-cancel-cleanup-timer-noop-when-absent ()
  "Cancel is a silent no-op when no cleanup timer is set."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-paired-clients server _tc
    (should server)
    (should-not
     (process-get
      server
      firefox-to-emacs-native-messenger--connection-key-cleanup-timer))
    (firefox-to-emacs-native-messenger--cancel-cleanup-timer server)
    (should-not
     (process-get
      server
      firefox-to-emacs-native-messenger--connection-key-cleanup-timer))))

(ert-deftest firefox-to-emacs-native-messenger-test-cancel-cleanup-timer-double-cancel-silent ()
  "Two consecutive cancels are silent and idempotent."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-paired-clients server _tc
    (should server)
    (firefox-to-emacs-native-messenger--start-cleanup-timer server)
    (firefox-to-emacs-native-messenger--cancel-cleanup-timer server)
    (firefox-to-emacs-native-messenger--cancel-cleanup-timer server)
    (should-not
     (process-get
      server
      firefox-to-emacs-native-messenger--connection-key-cleanup-timer))))

(ert-deftest firefox-to-emacs-native-messenger-test-start-cleanup-timer-cancels-prior ()
  "Re-starting cancels any prior cleanup timer (idempotent registration)."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-paired-clients server _tc
    (should server)
    (let ((t1 (firefox-to-emacs-native-messenger--start-cleanup-timer server)))
      (let ((t2 (firefox-to-emacs-native-messenger--start-cleanup-timer server)))
        (should (timerp t1))
        (should (timerp t2))
        (should-not (eq t1 t2))
        (should-not (member t1 timer-list))
        (should (member t2 timer-list))
        (should (eq t2
                    (process-get
                     server
                     firefox-to-emacs-native-messenger--connection-key-cleanup-timer)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-sentinel-cancels-cleanup-timer-on-peer-close ()
  "Per-connection sentinel cancels the cleanup timer on peer-close.

Required by PAT-0600 / Section 8.18 step 5: peer-close arrives via the
sentinel, which MUST cancel the cleanup timer to avoid an orphan timer
trying to tear down an already-deleted connection."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-paired-clients server _tc
    (should server)
    (firefox-to-emacs-native-messenger--start-cleanup-timer server)
    (let ((timer
           (process-get
            server
            firefox-to-emacs-native-messenger--connection-key-cleanup-timer)))
      (should (timerp timer))
      (firefox-to-emacs-native-messenger--connection-sentinel
       server "connection broken by remote peer\n")
      (should-not (member timer timer-list))
      (should-not
       (process-get
        server
        firefox-to-emacs-native-messenger--connection-key-cleanup-timer)))))

(defun firefox-to-emacs-native-messenger-test--build-request-frame (request-alist)
  "Build a length-prefixed request frame from REQUEST-ALIST.

Returns a unibyte string suitable for feeding to the per-connection
filter (`firefox-to-emacs-native-messenger--connection-filter')."
  (let* ((json (json-serialize request-alist
                               :null-object :null
                               :false-object :false))
         (unibyte (if (multibyte-string-p json)
                      (encode-coding-string json 'utf-8)
                    json))
         (prefix (firefox-to-emacs-native-messenger--pack-length
                  (length unibyte))))
    (concat prefix unibyte)))

(ert-deftest firefox-to-emacs-native-messenger-test-dispatcher-wiring-calls-writer-once ()
  "Synchronous dispatch produces exactly one writer call per request.

Instruments `--write-response' with a counter; sends a framed request
that hits a stub handler; asserts the counter is exactly 1."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-paired-clients server _tc
    (should server)
    (let ((calls 0)
          (orig (symbol-function
                 'firefox-to-emacs-native-messenger--write-response))
          (firefox-to-emacs-native-messenger--handlers
           (let ((h (make-hash-table :test 'equal)))
             (puthash "wiring-test"
                      (lambda (_c _r)
                        '((cmd . "wiring-test-response") (code . 0)))
                      h)
             h)))
      (cl-letf (((symbol-function
                  'firefox-to-emacs-native-messenger--write-response)
                 (lambda (c r) (cl-incf calls) (funcall orig c r))))
        (let ((frame (firefox-to-emacs-native-messenger-test--build-request-frame
                      '((cmd . "wiring-test")))))
          (firefox-to-emacs-native-messenger--connection-filter server frame)
          (accept-process-output nil 0.2)
          (should (= 1 calls)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-dispatcher-wiring-parse-error-routes-through-writer ()
  "The filter's parse-error path emits a generic error frame on the wire.

A malformed JSON payload in a well-framed envelope must produce a
`{cmd:\"error\", error:...}' response on the wire (not the void
stub-send-response from earlier phases)."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-paired-clients server tc
    (should server)
    (let* ((bad-payload "not valid json")
           (bad-frame
            (concat (firefox-to-emacs-native-messenger--pack-length
                     (length bad-payload))
                    bad-payload)))
      (firefox-to-emacs-native-messenger--connection-filter server bad-frame)
      (accept-process-output nil 0.3)
      (let* ((received (process-get tc 'received))
             (declared (firefox-to-emacs-native-messenger--unpack-length
                        (substring received 0 4)))
             (payload (substring received 4 (+ 4 declared)))
             (parsed (json-parse-string
                      (decode-coding-string payload 'utf-8)
                      :object-type 'alist
                      :null-object nil
                      :false-object :false)))
        (should (equal "error" (alist-get 'cmd parsed)))
        (should (stringp (alist-get 'error parsed)))
        (should (> (length (alist-get 'error parsed)) 0))))))

(ert-deftest firefox-to-emacs-native-messenger-test-dispatcher-wiring-unknown-cmd-routes-through-writer ()
  "An unknown cmd produces the `Unhandled message' frame on the wire."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-paired-clients server tc
    (should server)
    (let ((firefox-to-emacs-native-messenger--handlers
           (make-hash-table :test 'equal))
          (frame (firefox-to-emacs-native-messenger-test--build-request-frame
                  '((cmd . "no-such-cmd")))))
      (firefox-to-emacs-native-messenger--connection-filter server frame)
      (accept-process-output nil 0.3)
      (let* ((received (process-get tc 'received))
             (declared (firefox-to-emacs-native-messenger--unpack-length
                        (substring received 0 4)))
             (payload (substring received 4 (+ 4 declared)))
             (parsed (json-parse-string
                      (decode-coding-string payload 'utf-8)
                      :object-type 'alist
                      :null-object nil
                      :false-object :false)))
        (should (equal "error" (alist-get 'cmd parsed)))
        (should (equal "Unhandled message" (alist-get 'error parsed)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-dispatcher-wiring-handler-response-on-wire ()
  "A stub handler's response round-trips on the wire end-to-end.

The handler returns a known response; the writer serializes and sends
it; the test-side client receives and parses it.  The parsed JSON MUST
equal the handler's response object structurally."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-paired-clients server tc
    (should server)
    (let ((firefox-to-emacs-native-messenger--handlers
           (let ((h (make-hash-table :test 'equal)))
             (puthash "ping"
                      (lambda (_c _r) '((cmd . "pong") (code . 0)))
                      h)
             h)))
      (let ((frame (firefox-to-emacs-native-messenger-test--build-request-frame
                    '((cmd . "ping")))))
        (firefox-to-emacs-native-messenger--connection-filter server frame)
        (accept-process-output nil 0.3)
        (let* ((received (process-get tc 'received))
               (declared (firefox-to-emacs-native-messenger--unpack-length
                          (substring received 0 4)))
               (payload (substring received 4 (+ 4 declared)))
               (parsed (json-parse-string
                        (decode-coding-string payload 'utf-8)
                        :object-type 'alist
                        :null-object nil
                        :false-object :false)))
          (should (equal parsed '((cmd . "pong") (code . 0)))))))))

(ert-deftest firefox-to-emacs-native-messenger-test-dispatcher-wiring-wire-roundtrip-via-send-frame ()
  "Full wire round-trip via the framed-request sender returns the expected response.

This exercises the listener accept handler, the filter, the dispatcher,
and the writer end-to-end through a real client connecting from
outside the test process."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-listener-sandbox
      _cache _tmp sock
    (firefox-to-emacs-native-messenger-test--with-fresh-connection-registry
     (let ((firefox-to-emacs-native-messenger--handlers
            (let ((h (make-hash-table :test 'equal)))
              (puthash "ping"
                       (lambda (_c _r) '((cmd . "pong") (code . 0)))
                       h)
              h)))
       (firefox-to-emacs-native-messenger-start)
       (let ((response (firefox-to-emacs-native-messenger-test-send-frame
                        sock '((cmd . "ping")))))
         (should (equal response '((cmd . "pong") (code . 0)))))))))

(ert-deftest firefox-to-emacs-native-messenger-test-dispatcher-wiring-deferred-handler-skips-writer ()
  "When a handler sets state to `dispatched', the dispatcher does NOT call the writer.

This documents the deferred-response semantics that Phase 0800's run
handler relies on: an async handler signals deferral by transitioning
the connection state to `dispatched' before returning; the dispatcher
MUST then leave the wire write to the handler's subsequent callback."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-paired-clients server tc
    (should server)
    (let ((calls 0)
          (orig (symbol-function
                 'firefox-to-emacs-native-messenger--write-response))
          (firefox-to-emacs-native-messenger--handlers
           (let ((h (make-hash-table :test 'equal)))
             (puthash "deferred-test"
                      (lambda (c _r)
                        (process-put
                         c
                         firefox-to-emacs-native-messenger--connection-key-state
                         'dispatched)
                        '((cmd . "this-should-not-be-sent")))
                      h)
             h)))
      (cl-letf (((symbol-function
                  'firefox-to-emacs-native-messenger--write-response)
                 (lambda (c r) (cl-incf calls) (funcall orig c r))))
        (let ((frame (firefox-to-emacs-native-messenger-test--build-request-frame
                      '((cmd . "deferred-test")))))
          (firefox-to-emacs-native-messenger--connection-filter server frame)
          (accept-process-output nil 0.2)
          (should (= 0 calls))
          (should-not (process-get tc 'received))
          (should (eq 'dispatched
                      (process-get
                       server
                       firefox-to-emacs-native-messenger--connection-key-state))))))))

(defun firefox-to-emacs-native-messenger-test--make-response-of-byte-length (target-bytes)
  "Build a synthetic response whose serialized JSON byte length is TARGET-BYTES.

Returns an alist of the form ((cmd . \"x\") (data . PADDING)) where
PADDING is sized so that the JSON-serialized form is exactly
TARGET-BYTES bytes long.

Signals an error if TARGET-BYTES is smaller than the minimum overhead
of the response envelope."
  (let* ((overhead (length (json-serialize '((cmd . "x") (data . ""))
                                           :null-object :null
                                           :false-object :false)))
         (data-len (- target-bytes overhead)))
    (when (< data-len 0)
      (error "target-bytes %d less than overhead %d" target-bytes overhead))
    (list (cons 'cmd "x") (cons 'data (make-string data-len ?a)))))

(ert-deftest firefox-to-emacs-native-messenger-test-outbound-cap-boundary-cap-minus-1-sent-verbatim ()
  "A response whose serialized payload is exactly cap-1 bytes is sent verbatim.

Per PAT-0600 the cap is a strict `> cap' threshold: cap-1 passes the
check unchanged and reaches the wire as the handler intended."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-paired-clients server tc
    (should server)
    (let ((cap 200))
      (let ((firefox-to-emacs-native-messenger-outbound-response-cap cap))
        (let* ((resp
                (firefox-to-emacs-native-messenger-test--make-response-of-byte-length
                 (1- cap))))
          (firefox-to-emacs-native-messenger--write-response server resp)
          (accept-process-output nil 0.2)
          (let* ((received (process-get tc 'received))
                 (declared (firefox-to-emacs-native-messenger--unpack-length
                            (substring received 0 4)))
                 (payload (substring received 4 (+ 4 declared)))
                 (parsed (json-parse-string
                          (decode-coding-string payload 'utf-8)
                          :object-type 'alist
                          :null-object nil
                          :false-object :false)))
            (should (= declared (1- cap)))
            (should (equal parsed resp))))))))

(ert-deftest firefox-to-emacs-native-messenger-test-outbound-cap-boundary-cap-exact-sent-verbatim ()
  "A response whose serialized payload is exactly cap bytes is sent verbatim.

Per PAT-0600 the cap is `> cap', not `>= cap'.  A payload of exactly
the cap value passes the check and reaches the wire intact."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-paired-clients server tc
    (should server)
    (let ((cap 200))
      (let ((firefox-to-emacs-native-messenger-outbound-response-cap cap))
        (let* ((resp
                (firefox-to-emacs-native-messenger-test--make-response-of-byte-length
                 cap)))
          (firefox-to-emacs-native-messenger--write-response server resp)
          (accept-process-output nil 0.2)
          (let* ((received (process-get tc 'received))
                 (declared (firefox-to-emacs-native-messenger--unpack-length
                            (substring received 0 4)))
                 (payload (substring received 4 (+ 4 declared)))
                 (parsed (json-parse-string
                          (decode-coding-string payload 'utf-8)
                          :object-type 'alist
                          :null-object nil
                          :false-object :false)))
            (should (= declared cap))
            (should (equal parsed resp))))))))

(ert-deftest firefox-to-emacs-native-messenger-test-outbound-cap-boundary-cap-plus-1-replaced ()
  "A response whose serialized payload is cap+1 bytes is replaced with the oversized error.

Per PAT-0600 a strict over-cap value triggers exactly one replacement
with the generic `response too large' error."
  :tags '(:integration :sandbox :fixture-driven)
  (firefox-to-emacs-native-messenger-test--with-paired-clients server tc
    (should server)
    (let ((cap 200))
      (let ((firefox-to-emacs-native-messenger-outbound-response-cap cap))
        (let* ((resp
                (firefox-to-emacs-native-messenger-test--make-response-of-byte-length
                 (1+ cap))))
          (firefox-to-emacs-native-messenger--write-response server resp)
          (accept-process-output nil 0.2)
          (let* ((received (process-get tc 'received))
                 (declared (firefox-to-emacs-native-messenger--unpack-length
                            (substring received 0 4)))
                 (payload (substring received 4 (+ 4 declared)))
                 (parsed (json-parse-string
                          (decode-coding-string payload 'utf-8)
                          :object-type 'alist
                          :null-object nil
                          :false-object :false)))
            (should (<= declared cap))
            (should (equal "error" (alist-get 'cmd parsed)))
            (should (equal "response too large" (alist-get 'error parsed)))))))))

(ert-deftest firefox-to-emacs-native-messenger-test-outbound-cap-boundary-degenerate-no-frame ()
  "When the cap is so small that even the error response overflows, no frame is sent.

The wording of the error response is approximately 41 bytes
serialized; a cap below that triggers the degenerate fallback."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-paired-clients server tc
    (should server)
    (let ((firefox-to-emacs-native-messenger-outbound-response-cap 10))
      (let* ((resp
              (firefox-to-emacs-native-messenger-test--make-response-of-byte-length
               50)))
        (firefox-to-emacs-native-messenger--write-response server resp)
        (accept-process-output nil 0.2)
        (should-not (process-get tc 'received))
        (should (eq 'responded
                    (process-get
                     server
                     firefox-to-emacs-native-messenger--connection-key-state)))))))


;;;; ============================================================
;;;; Phase 0700: handlers, gates, and supporting helpers.
;;;; ============================================================

;;;; TASK-13700: TRAMP guard tests.

(ert-deftest firefox-to-emacs-native-messenger-test-tramp-guard-rejects-ssh ()
  "TRAMP guard signals `bad-request' for an /ssh: TRAMP path.

Per SEC-0100, paths that `file-remote-p' recognizes as remote MUST be
rejected by the guard with the bridge's `bad-request' error condition
before any I/O is attempted."
  :tags '(:unit)
  (should-error
   (firefox-to-emacs-native-messenger--tramp-guard "/ssh:host:/tmp/file")
   :type 'firefox-to-emacs-native-messenger-bad-request))

(ert-deftest firefox-to-emacs-native-messenger-test-tramp-guard-rejects-sudo ()
  "TRAMP guard signals `bad-request' for a /sudo: TRAMP path."
  :tags '(:unit)
  (should-error
   (firefox-to-emacs-native-messenger--tramp-guard "/sudo:root@:/etc/passwd")
   :type 'firefox-to-emacs-native-messenger-bad-request))

(ert-deftest firefox-to-emacs-native-messenger-test-tramp-guard-rejects-docker ()
  "TRAMP guard signals `bad-request' for a /docker: TRAMP path."
  :tags '(:unit)
  (should-error
   (firefox-to-emacs-native-messenger--tramp-guard "/docker:c:/x")
   :type 'firefox-to-emacs-native-messenger-bad-request))

(ert-deftest firefox-to-emacs-native-messenger-test-tramp-guard-passes-local ()
  "TRAMP guard returns local paths unchanged.

`file-remote-p' returns nil for non-remote paths; the guard MUST return
its argument unchanged so callers can chain it inside expressions like
`(setq path (--tramp-guard path))'."
  :tags '(:unit)
  (should (equal "/tmp/local-file"
                 (firefox-to-emacs-native-messenger--tramp-guard
                  "/tmp/local-file")))
  (should (equal "/home/user/file.txt"
                 (firefox-to-emacs-native-messenger--tramp-guard
                  "/home/user/file.txt")))
  (should (equal "relative/path"
                 (firefox-to-emacs-native-messenger--tramp-guard
                  "relative/path"))))

;;;; TASK-13900: path-expansion helper (PAT-0400) tests.

(ert-deftest firefox-to-emacs-native-messenger-test-expand-path-rejects-raw-tramp ()
  "Raw TRAMP input is rejected by the first TRAMP guard before any expansion.

PAT-0400 step 1 runs the TRAMP guard on the raw path argument before
let-binding `default-directory' or invoking `substitute-env-vars';
the resulting signal MUST be `bad-request'."
  :tags '(:unit)
  (should-error
   (firefox-to-emacs-native-messenger--expand-path "/ssh:host:/etc/hosts")
   :type 'firefox-to-emacs-native-messenger-bad-request)
  (should-error
   (firefox-to-emacs-native-messenger--expand-path "/sudo:root@:/etc/passwd")
   :type 'firefox-to-emacs-native-messenger-bad-request))

(ert-deftest firefox-to-emacs-native-messenger-test-expand-path-rejects-expanded-tramp ()
  "Local raw input that EXPANDS to a TRAMP path is rejected by the second guard.

PAT-0400's second TRAMP guard (step 5) runs after `substitute-env-vars'
and `expand-file-name'.  An env-var bound to a TRAMP-shaped prefix
makes the expanded path remote even though the raw input wasn't."
  :tags '(:unit)
  (let ((process-environment
         (cons "FENM_TEST_REMOTE=/ssh:host:" process-environment)))
    (should-error
     (firefox-to-emacs-native-messenger--expand-path "$FENM_TEST_REMOTE/etc/hosts")
     :type 'firefox-to-emacs-native-messenger-bad-request)))

(ert-deftest firefox-to-emacs-native-messenger-test-expand-path-substitutes-set-env-var ()
  "A set env-var reference is replaced with its value per PAT-0400 step 3."
  :tags '(:unit)
  (let ((process-environment
         (cons "FENM_TEST_DIR=/tmp/fenm-test-dir" process-environment)))
    (should (equal "/tmp/fenm-test-dir/file.txt"
                   (firefox-to-emacs-native-messenger--expand-path
                    "$FENM_TEST_DIR/file.txt")))))

(ert-deftest firefox-to-emacs-native-messenger-test-expand-path-unset-env-var-literal-passthrough ()
  "An unset env-var reference passes through literally per PAT-0400 step 3.

`substitute-env-vars' with WHEN-UNDEFINED set to nil leaves unset
references in place; the bridge invokes it with that behavior so a
path containing an unset variable is not silently rewritten to an
empty segment."
  :tags '(:unit)
  (let ((process-environment
         (seq-filter
          (lambda (e) (not (string-prefix-p "FENM_TEST_UNSET_" e)))
          process-environment)))
    (let ((result
           (firefox-to-emacs-native-messenger--expand-path
            "/tmp/$FENM_TEST_UNSET_VAR/file")))
      (should (equal "/tmp/$FENM_TEST_UNSET_VAR/file" result)))))

(ert-deftest firefox-to-emacs-native-messenger-test-expand-path-absolute-pass-through ()
  "Absolute paths pass through `expand-file-name' idempotently."
  :tags '(:unit)
  (should (equal "/etc/hosts"
                 (firefox-to-emacs-native-messenger--expand-path "/etc/hosts")))
  (should (equal "/tmp/foo/bar"
                 (firefox-to-emacs-native-messenger--expand-path
                  "/tmp/foo/bar"))))

(ert-deftest firefox-to-emacs-native-messenger-test-expand-path-tilde-resolves-to-home ()
  "~ in the raw input is expanded against the user's home directory."
  :tags '(:unit)
  (should (equal (expand-file-name "~/some-file.txt")
                 (firefox-to-emacs-native-messenger--expand-path
                  "~/some-file.txt"))))

(ert-deftest firefox-to-emacs-native-messenger-test-expand-path-relative-resolves-under-home ()
  "Relative paths resolve against the let-bound `default-directory' of ~/.

PAT-0400 step 2 binds `default-directory' to ~/ before
`expand-file-name' runs; a relative input therefore resolves under
the user's home directory regardless of the caller's
`default-directory'."
  :tags '(:unit)
  (let ((default-directory "/etc/"))
    (should (equal (expand-file-name "subdir/file.txt" "~/")
                   (firefox-to-emacs-native-messenger--expand-path
                    "subdir/file.txt")))))


;;;; TASK-14100: read path-gate tests.

(ert-deftest firefox-to-emacs-native-messenger-test-read-gate-empty-rejects ()
  "Empty (nil) or empty-list read-whitelist rejects every path per REQ-3700."
  :tags '(:unit)
  (let ((firefox-to-emacs-native-messenger-read-whitelist nil))
    (should-not (firefox-to-emacs-native-messenger--read-gate-match-p
                 "/tmp/anything")))
  (let ((firefox-to-emacs-native-messenger-read-whitelist '()))
    (should-not (firefox-to-emacs-native-messenger--read-gate-match-p
                 "/tmp/anything"))))

(ert-deftest firefox-to-emacs-native-messenger-test-read-gate-allow-all-accepts ()
  "The (\"*\") sentinel allows any path through the gate per REQ-3800."
  :tags '(:unit)
  (let ((firefox-to-emacs-native-messenger-read-whitelist '("*")))
    (should (firefox-to-emacs-native-messenger--read-gate-match-p "/tmp/any-file"))
    (should (firefox-to-emacs-native-messenger--read-gate-match-p
             "/home/user/somewhere/else.txt"))))

(ert-deftest firefox-to-emacs-native-messenger-test-read-gate-literal-match ()
  "A literal absolute-path entry matches the expanded path verbatim."
  :tags '(:unit)
  (let ((firefox-to-emacs-native-messenger-read-whitelist
         '("/tmp/specific-file.txt")))
    (should (firefox-to-emacs-native-messenger--read-gate-match-p
             "/tmp/specific-file.txt"))
    (should-not (firefox-to-emacs-native-messenger--read-gate-match-p
                 "/tmp/specific-file.txt.bak"))
    (should-not (firefox-to-emacs-native-messenger--read-gate-match-p
                 "/tmp/other-file.txt"))))

(ert-deftest firefox-to-emacs-native-messenger-test-read-gate-glob-match ()
  "A glob entry matches per `--glob-match-p' semantics (REQ-2900)."
  :tags '(:unit)
  (let ((firefox-to-emacs-native-messenger-read-whitelist
         '("/home/user/projects/*.txt")))
    (should (firefox-to-emacs-native-messenger--read-gate-match-p
             "/home/user/projects/notes.txt"))
    (should-not (firefox-to-emacs-native-messenger--read-gate-match-p
                 "/home/user/projects/sub/notes.txt"))
    (should-not (firefox-to-emacs-native-messenger--read-gate-match-p
                 "/home/user/projects/notes.md")))
  (let ((firefox-to-emacs-native-messenger-read-whitelist
         '("/home/user/**/*.txt")))
    (should (firefox-to-emacs-native-messenger--read-gate-match-p
             "/home/user/deep/sub/notes.txt"))))

(ert-deftest firefox-to-emacs-native-messenger-test-read-gate-temp-path-marker-hit ()
  "The <TEMP-PATH> marker matches iff the path is in the capability registry."
  :tags '(:unit)
  (firefox-to-emacs-native-messenger-test--with-stub-registry
      '("/tmp/registered-tempfile.txt")
    (let ((firefox-to-emacs-native-messenger-read-whitelist '("<TEMP-PATH>")))
      (should (firefox-to-emacs-native-messenger--read-gate-match-p
               "/tmp/registered-tempfile.txt"))
      (should-not (firefox-to-emacs-native-messenger--read-gate-match-p
                   "/tmp/unregistered.txt")))))

(ert-deftest firefox-to-emacs-native-messenger-test-read-gate-temp-path-marker-no-registry ()
  "The <TEMP-PATH> marker rejects when the path is not in the registry."
  :tags '(:unit)
  (firefox-to-emacs-native-messenger-test--with-stub-registry '()
    (let ((firefox-to-emacs-native-messenger-read-whitelist '("<TEMP-PATH>")))
      (should-not (firefox-to-emacs-native-messenger--read-gate-match-p
                   "/tmp/anything-at-all")))))

(ert-deftest firefox-to-emacs-native-messenger-test-read-gate-multi-entry-any-match ()
  "Multiple entries: gate accepts on the first matching entry."
  :tags '(:unit)
  (firefox-to-emacs-native-messenger-test--with-stub-registry
      '("/tmp/registered.txt")
    (let ((firefox-to-emacs-native-messenger-read-whitelist
           '("<TEMP-PATH>" "/etc/some-config" "/home/user/*.rc")))
      (should (firefox-to-emacs-native-messenger--read-gate-match-p
               "/tmp/registered.txt"))
      (should (firefox-to-emacs-native-messenger--read-gate-match-p
               "/etc/some-config"))
      (should (firefox-to-emacs-native-messenger--read-gate-match-p
               "/home/user/foo.rc"))
      (should-not (firefox-to-emacs-native-messenger--read-gate-match-p
                   "/etc/something-else")))))

(ert-deftest firefox-to-emacs-native-messenger-test-read-gate-malformed-signals ()
  "Per-gate validator at gate-check time signals `whitelist-malformed' on bad input.

PAT-1100 site 3 re-validates the whitelist defcustom at every gate check.
The let-binding here intentionally bypasses the variable watcher's
:set validator (PAT-1100 site 1), simulating a value that arrived via
list mutation or other non-assignment path."
  :tags '(:unit)
  (let ((firefox-to-emacs-native-messenger-read-whitelist
         '("*" "/etc/oops")))
    (should-error
     (firefox-to-emacs-native-messenger--read-gate-match-p "/tmp/whatever")
     :type 'firefox-to-emacs-native-messenger-whitelist-malformed))
  (let ((firefox-to-emacs-native-messenger-read-whitelist
         '("relative/path/not/allowed")))
    (should-error
     (firefox-to-emacs-native-messenger--read-gate-match-p "/tmp/whatever")
     :type 'firefox-to-emacs-native-messenger-whitelist-malformed)))

;;;; TASK-14300: `version' handler tests.

(ert-deftest firefox-to-emacs-native-messenger-test-version-handler-fixture ()
  "The `version' handler returns the PROTOCOL.md `version-success' fixture.

The fixture pins the wire response to `((cmd . \"version\") (version
. \"0.3.7\") (code . 0))'.  The handler takes no request fields, has
no gate, and is a pure function of the bridge's version defconst."
  :tags '(:unit :fixture-driven)
  (let* ((fixture (firefox-to-emacs-native-messenger-test-load-fixture
                   "version-success"))
         (expected-response (alist-get 'response fixture))
         (actual-response (firefox-to-emacs-native-messenger--version-handler
                           nil '((cmd . "version")))))
    (should (firefox-to-emacs-native-messenger-test-fixture-equal-p
             expected-response actual-response))))

(ert-deftest firefox-to-emacs-native-messenger-test-version-handler-ignores-extra-fields ()
  "The `version' handler reads no request fields; extra fields are ignored."
  :tags '(:unit)
  (let ((response (firefox-to-emacs-native-messenger--version-handler
                   nil
                   '((cmd . "version")
                     (file . "/some/path")
                     (extra . "ignored")))))
    (should (equal "version" (alist-get 'cmd response)))
    (should (stringp (alist-get 'version response)))
    (should (= 0 (alist-get 'code response)))))

;;;; TASK-14500: `temp' handler prefix sanitization tests.

(ert-deftest firefox-to-emacs-native-messenger-test-sanitize-temp-prefix-lowercases ()
  "Sanitization lowercases ASCII letters per upstream `sanitiseFilename'."
  :tags '(:unit)
  (should (equal "example.com"
                 (firefox-to-emacs-native-messenger--sanitize-temp-prefix
                  "Example.com")))
  (should (equal "abc"
                 (firefox-to-emacs-native-messenger--sanitize-temp-prefix
                  "ABC")))
  (should (equal "mixedcase"
                 (firefox-to-emacs-native-messenger--sanitize-temp-prefix
                  "MixedCase"))))

(ert-deftest firefox-to-emacs-native-messenger-test-sanitize-temp-prefix-retains-alnum-dot ()
  "Sanitization retains ASCII letters, digits, and dots; drops everything else."
  :tags '(:unit)
  (should (equal "abc123"
                 (firefox-to-emacs-native-messenger--sanitize-temp-prefix
                  "abc123")))
  (should (equal "a.b.c"
                 (firefox-to-emacs-native-messenger--sanitize-temp-prefix
                  "a.b.c")))
  (should (equal "abc"
                 (firefox-to-emacs-native-messenger--sanitize-temp-prefix
                  "a-b-c")))
  (should (equal "abc"
                 (firefox-to-emacs-native-messenger--sanitize-temp-prefix
                  "a b c")))
  (should (equal "abc"
                 (firefox-to-emacs-native-messenger--sanitize-temp-prefix
                  "a/b/c"))))

(ert-deftest firefox-to-emacs-native-messenger-test-sanitize-temp-prefix-collapses-repeated-dots ()
  "Sanitization collapses runs of two or more consecutive dots to a single dot."
  :tags '(:unit)
  (should (equal "."
                 (firefox-to-emacs-native-messenger--sanitize-temp-prefix
                  "..")))
  (should (equal "."
                 (firefox-to-emacs-native-messenger--sanitize-temp-prefix
                  "...")))
  (should (equal "a.b"
                 (firefox-to-emacs-native-messenger--sanitize-temp-prefix
                  "a..b")))
  (should (equal "a.b.c"
                 (firefox-to-emacs-native-messenger--sanitize-temp-prefix
                  "a..b...c"))))

(ert-deftest firefox-to-emacs-native-messenger-test-sanitize-temp-prefix-empty-cases ()
  "Empty and pathological inputs produce the empty string."
  :tags '(:unit)
  (should (equal ""
                 (firefox-to-emacs-native-messenger--sanitize-temp-prefix
                  "")))
  (should (equal ""
                 (firefox-to-emacs-native-messenger--sanitize-temp-prefix
                  nil)))
  (should (equal ""
                 (firefox-to-emacs-native-messenger--sanitize-temp-prefix
                  "---")))
  (should (equal ""
                 (firefox-to-emacs-native-messenger--sanitize-temp-prefix
                  "!@#$%"))))

(ert-deftest firefox-to-emacs-native-messenger-test-sanitize-temp-prefix-combined ()
  "All sanitization stages compose in the order: lowercase, strip, collapse."
  :tags '(:unit)
  (should (equal "example.com"
                 (firefox-to-emacs-native-messenger--sanitize-temp-prefix
                  "Example.Com")))
  (should (equal "host.with.dots"
                 (firefox-to-emacs-native-messenger--sanitize-temp-prefix
                  "Host..With..Dots")))
  (should (equal "abc"
                 (firefox-to-emacs-native-messenger--sanitize-temp-prefix
                  "A-B-C"))))

;;;; TASK-14700: `temp' handler end-to-end tests.

(defmacro firefox-to-emacs-native-messenger-test--with-temp-handler-env
    (tmp &rest body)
  "Bind TMP to a sandbox tempfile dir with a fresh registry around BODY."
  (declare (indent 1) (debug (symbolp body)))
  `(firefox-to-emacs-native-messenger-test--with-sandbox-tempfile-dir ,tmp
     (firefox-to-emacs-native-messenger-test--with-fresh-registry
      ,@body)))

(ert-deftest firefox-to-emacs-native-messenger-test-temp-handler-success-response ()
  "Successful `temp' returns the success fixture shape with the new path."
  :tags '(:integration :sandbox :fixture-driven)
  (firefox-to-emacs-native-messenger-test--with-temp-handler-env tmp
    (let* ((request '((cmd . "temp")
                      (content . "hello world")
                      (prefix . "example.com")))
           (response (firefox-to-emacs-native-messenger--temp-handler
                      nil request))
           (path (alist-get 'content response)))
      (should (equal "temp" (alist-get 'cmd response)))
      (should (= 0 (alist-get 'code response)))
      (should (stringp path))
      (should (string-prefix-p tmp path))
      (should (file-exists-p path)))))

(ert-deftest firefox-to-emacs-native-messenger-test-temp-handler-writes-content ()
  "Tempfile contents equal the request's content byte-for-byte."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-temp-handler-env _tmp
    (let* ((content "line 1\nline 2\n\nblank above\n")
           (request `((cmd . "temp")
                      (content . ,content)
                      (prefix . "test")))
           (response (firefox-to-emacs-native-messenger--temp-handler
                      nil request))
           (path (alist-get 'content response)))
      (should (equal content
                     (with-temp-buffer
                       (insert-file-contents-literally path)
                       (buffer-string)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-temp-handler-file-mode-0600 ()
  "Tempfile is created with mode 0600 (no group or world bits)."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-temp-handler-env _tmp
    (let* ((request '((cmd . "temp") (content . "x") (prefix . "t")))
           (response (firefox-to-emacs-native-messenger--temp-handler
                      nil request))
           (path (alist-get 'content response)))
      (should (= #o600 (logand #o777 (file-modes path)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-temp-handler-registers-path ()
  "Tempfile path is entered into the capability registry on success per PAT-1000."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-temp-handler-env _tmp
    (let* ((request '((cmd . "temp") (content . "x") (prefix . "t")))
           (response (firefox-to-emacs-native-messenger--temp-handler
                      nil request))
           (path (alist-get 'content response)))
      (should (firefox-to-emacs-native-messenger--registry-contains-p path)))))

(ert-deftest firefox-to-emacs-native-messenger-test-temp-handler-sanitized-prefix-in-path ()
  "The sanitized prefix appears between `tmp_' and the random component."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-temp-handler-env _tmp
    (let* ((request '((cmd . "temp")
                      (content . "x")
                      (prefix . "Example.Com")))
           (response (firefox-to-emacs-native-messenger--temp-handler
                      nil request))
           (path (alist-get 'content response))
           (name (file-name-nondirectory path)))
      (should (string-match-p "\\`tmp_example.com_" name))
      (should (string-suffix-p ".txt" name)))))

(ert-deftest firefox-to-emacs-native-messenger-test-temp-handler-empty-prefix-ok ()
  "Empty or absent `prefix' produces `tmp__'-style filenames; still .txt suffix."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-temp-handler-env _tmp
    (let* ((request '((cmd . "temp") (content . "x")))
           (response (firefox-to-emacs-native-messenger--temp-handler
                      nil request))
           (path (alist-get 'content response))
           (name (file-name-nondirectory path)))
      (should (= 0 (alist-get 'code response)))
      (should (string-match-p "\\`tmp__" name))
      (should (string-suffix-p ".txt" name)))))

(ert-deftest firefox-to-emacs-native-messenger-test-temp-handler-missing-content ()
  "Missing `content' field returns generic error and creates no tempfile."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-temp-handler-env tmp
    (let* ((request '((cmd . "temp") (prefix . "x")))
           (response (firefox-to-emacs-native-messenger--temp-handler
                      nil request)))
      (should (equal "error" (alist-get 'cmd response)))
      (should (string-match-p "content"
                              (alist-get 'error response)))
      (should-not (alist-get 'code response))
      (should (= 0 (length (directory-files tmp nil "\\`tmp_")))))))

(ert-deftest firefox-to-emacs-native-messenger-test-temp-handler-null-content ()
  "Null `content' (JSON null arrives as nil) returns generic error, no file."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-temp-handler-env tmp
    (let* ((request '((cmd . "temp") (content . nil) (prefix . "x")))
           (response (firefox-to-emacs-native-messenger--temp-handler
                      nil request)))
      (should (equal "error" (alist-get 'cmd response)))
      (should (= 0 (length (directory-files tmp nil "\\`tmp_")))))))

(ert-deftest firefox-to-emacs-native-messenger-test-temp-handler-non-string-content ()
  "Non-string `content' (e.g., a number) returns generic error, no file."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-temp-handler-env tmp
    (let* ((request '((cmd . "temp") (content . 42) (prefix . "x")))
           (response (firefox-to-emacs-native-messenger--temp-handler
                      nil request)))
      (should (equal "error" (alist-get 'cmd response)))
      (should (= 0 (length (directory-files tmp nil "\\`tmp_")))))))

(ert-deftest firefox-to-emacs-native-messenger-test-temp-handler-registry-cap-exceeded ()
  "When the registry cap is exceeded after prune-all, returns error and no file.

Pre-fills the registry with N valid entries pointing at existing files in
the sandbox so prune-all is a no-op; sets the cap to N; calls temp; expects
the generic error and no new tempfile."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-temp-handler-env tmp
    (firefox-to-emacs-native-messenger-test--with-saved-registry-cap
     (let ((firefox-to-emacs-native-messenger-temp-registry-cap 2))
       (let ((p1 (expand-file-name "tmp_preexist_a.txt" tmp))
             (p2 (expand-file-name "tmp_preexist_b.txt" tmp)))
         (with-temp-file p1 (insert "a"))
         (with-temp-file p2 (insert "b"))
         (firefox-to-emacs-native-messenger--registry-register p1)
         (firefox-to-emacs-native-messenger--registry-register p2)
         (let* ((before-count
                 (length (directory-files tmp nil "\\`tmp_")))
                (request '((cmd . "temp") (content . "x") (prefix . "t")))
                (response (firefox-to-emacs-native-messenger--temp-handler
                           nil request))
                (after-count
                 (length (directory-files tmp nil "\\`tmp_"))))
           (should (equal "error" (alist-get 'cmd response)))
           (should (string-match-p "temp registry cap exceeded"
                                   (alist-get 'error response)))
           (should (= before-count after-count))))))))

(ert-deftest firefox-to-emacs-native-messenger-test-temp-handler-cap-prune-then-succeed ()
  "Prune-all clears stale entries to make room when the cap is reached.

Pre-fills the registry with N entries pointing at files that DO NOT exist;
sets the cap to N; calls temp.  Prune-all removes all stale entries
because their files are missing; count drops to 0; temp succeeds."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-temp-handler-env tmp
    (firefox-to-emacs-native-messenger-test--with-saved-registry-cap
     (let ((firefox-to-emacs-native-messenger-temp-registry-cap 2))
       (let ((stale-a (expand-file-name "missing-a" tmp))
             (stale-b (expand-file-name "missing-b" tmp)))
         (puthash stale-a (list :dev 0 :inode 0 :uid 0)
                  firefox-to-emacs-native-messenger--capability-registry)
         (puthash stale-b (list :dev 0 :inode 0 :uid 0)
                  firefox-to-emacs-native-messenger--capability-registry)
         (let* ((request '((cmd . "temp") (content . "x") (prefix . "t")))
                (response (firefox-to-emacs-native-messenger--temp-handler
                           nil request)))
           (should (equal "temp" (alist-get 'cmd response)))
           (should (= 0 (alist-get 'code response)))))))))

;;;; TASK-14900: `temp' handler no-gate (ungated per REQ-2600).

(ert-deftest firefox-to-emacs-native-messenger-test-temp-handler-ungated-deny-all-whitelists ()
  "`temp' succeeds with both whitelists at nil (deny-all) per REQ-2600.

The handler is structurally ungated: no per-handler whitelist applies.
Setting both `run-whitelist' and `read-whitelist' to nil therefore
must NOT prevent a `temp' request from creating its file."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-temp-handler-env _tmp
    (firefox-to-emacs-native-messenger-test--with-saved-whitelists
     (setq firefox-to-emacs-native-messenger-run-whitelist nil)
     (setq firefox-to-emacs-native-messenger-read-whitelist nil)
     (let* ((request '((cmd . "temp") (content . "x") (prefix . "t")))
            (response (firefox-to-emacs-native-messenger--temp-handler
                       nil request)))
       (should (equal "temp" (alist-get 'cmd response)))
       (should (= 0 (alist-get 'code response)))
       (should (stringp (alist-get 'content response)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-temp-handler-ungated-allow-all-equivalent ()
  "`temp' produces the same response shape under any whitelist value.

REQ-2600 requires `temp' to be insensitive to per-handler whitelists;
two requests sent in the same listener lifetime but under different
whitelist configurations therefore produce structurally identical
success responses."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-temp-handler-env _tmp
    (firefox-to-emacs-native-messenger-test--with-saved-whitelists
     (setq firefox-to-emacs-native-messenger-run-whitelist nil)
     (setq firefox-to-emacs-native-messenger-read-whitelist nil)
     (let* ((r1 (firefox-to-emacs-native-messenger--temp-handler
                 nil '((cmd . "temp") (content . "a") (prefix . "t")))))
       (setq firefox-to-emacs-native-messenger-run-whitelist '("*"))
       (setq firefox-to-emacs-native-messenger-read-whitelist '("*"))
       (let ((r2 (firefox-to-emacs-native-messenger--temp-handler
                  nil '((cmd . "temp") (content . "b") (prefix . "t")))))
         (should (equal (alist-get 'cmd r1) (alist-get 'cmd r2)))
         (should (equal (alist-get 'code r1) (alist-get 'code r2)))
         (should (stringp (alist-get 'content r1)))
         (should (stringp (alist-get 'content r2))))))))

;;;; TASK-15100: rcpath candidate-list walker tests.

(ert-deftest firefox-to-emacs-native-messenger-test-find-rcpath-none-exist ()
  "Returns nil when no candidate exists on disk."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test-with-sandbox-tempdir dir
    (let ((firefox-to-emacs-native-messenger-rcpath-candidates
           (list
            (expand-file-name "a.rc" dir)
            (expand-file-name "b.rc" dir))))
      (should-not (firefox-to-emacs-native-messenger--find-rcpath)))))

(ert-deftest firefox-to-emacs-native-messenger-test-find-rcpath-only-second-exists ()
  "Returns the second candidate when only it exists on disk."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test-with-sandbox-tempdir dir
    (let ((p1 (expand-file-name "first.rc" dir))
          (p2 (expand-file-name "second.rc" dir)))
      (with-temp-file p2 (insert "rc content"))
      (let ((firefox-to-emacs-native-messenger-rcpath-candidates
             (list p1 p2)))
        (should (equal p2 (firefox-to-emacs-native-messenger--find-rcpath)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-find-rcpath-first-and-third-exist ()
  "Returns the FIRST existing candidate when multiple candidates exist."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test-with-sandbox-tempdir dir
    (let ((p1 (expand-file-name "first.rc" dir))
          (p2 (expand-file-name "second.rc" dir))
          (p3 (expand-file-name "third.rc" dir)))
      (with-temp-file p1 (insert "first"))
      (with-temp-file p3 (insert "third"))
      (let ((firefox-to-emacs-native-messenger-rcpath-candidates
             (list p1 p2 p3)))
        (should (equal p1 (firefox-to-emacs-native-messenger--find-rcpath)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-find-rcpath-tramp-candidate-rejects ()
  "A TRAMP-shaped candidate in the list signals `bad-request' defensively per SEC-1300."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test-with-sandbox-tempdir dir
    (let ((firefox-to-emacs-native-messenger-rcpath-candidates
           (list "/ssh:host:/etc/some.rc"
                 (expand-file-name "local.rc" dir))))
      (should-error
       (firefox-to-emacs-native-messenger--find-rcpath)
       :type 'firefox-to-emacs-native-messenger-bad-request))))

(ert-deftest firefox-to-emacs-native-messenger-test-find-rcpath-skips-directory ()
  "A candidate that resolves to a directory is skipped (only regular files match)."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test-with-sandbox-tempdir dir
    (let ((subdir (expand-file-name "candidate-dir" dir))
          (regular (expand-file-name "regular.rc" dir)))
      (make-directory subdir)
      (with-temp-file regular (insert "ok"))
      (let ((firefox-to-emacs-native-messenger-rcpath-candidates
             (list subdir regular)))
        (should (equal regular
                       (firefox-to-emacs-native-messenger--find-rcpath)))))))

;;;; TASK-15300: `getconfigpath' handler tests.

(ert-deftest firefox-to-emacs-native-messenger-test-getconfigpath-success-fixture ()
  "With one candidate present in the sandbox, returns the success-fixture shape."
  :tags '(:unit :sandbox :fixture-driven)
  (firefox-to-emacs-native-messenger-test-with-sandbox-tempdir dir
    (let* ((rc-path (expand-file-name "tridactylrc" dir))
           (firefox-to-emacs-native-messenger-rcpath-candidates
            (list rc-path)))
      (with-temp-file rc-path (insert "config content"))
      (let* ((fixture
              (firefox-to-emacs-native-messenger-test-load-fixture
               "getconfigpath-success"))
             (expected-response (alist-get 'response fixture))
             (actual-response
              (firefox-to-emacs-native-messenger--getconfigpath-handler
               nil '((cmd . "getconfigpath")))))
        (should (firefox-to-emacs-native-messenger-test-fixture-equal-p
                 expected-response actual-response))
        (should (equal rc-path (alist-get 'content actual-response)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-getconfigpath-empty-fixture ()
  "With no candidate present, returns the empty-fixture shape (no content field, code 1)."
  :tags '(:unit :sandbox :fixture-driven)
  (firefox-to-emacs-native-messenger-test-with-sandbox-tempdir dir
    (let ((firefox-to-emacs-native-messenger-rcpath-candidates
           (list (expand-file-name "missing-a.rc" dir)
                 (expand-file-name "missing-b.rc" dir))))
      (let* ((fixture
              (firefox-to-emacs-native-messenger-test-load-fixture
               "getconfigpath-empty"))
             (expected-response (alist-get 'response fixture))
             (actual-response
              (firefox-to-emacs-native-messenger--getconfigpath-handler
               nil '((cmd . "getconfigpath")))))
        (should (firefox-to-emacs-native-messenger-test-fixture-equal-p
                 expected-response actual-response))
        (should (equal "getconfigpath" (alist-get 'cmd actual-response)))
        (should (= 1 (alist-get 'code actual-response)))
        (should-not (alist-get 'content actual-response))))))

(ert-deftest firefox-to-emacs-native-messenger-test-getconfigpath-ignores-extra-fields ()
  "The handler reads no request fields; extra fields in the request are ignored."
  :tags '(:unit :sandbox)
  (firefox-to-emacs-native-messenger-test-with-sandbox-tempdir dir
    (let* ((rc-path (expand-file-name "tridactylrc" dir))
           (firefox-to-emacs-native-messenger-rcpath-candidates
            (list rc-path)))
      (with-temp-file rc-path (insert "content"))
      (let ((response
             (firefox-to-emacs-native-messenger--getconfigpath-handler
              nil '((cmd . "getconfigpath")
                    (file . "/should/be/ignored")
                    (junk . 42)))))
        (should (equal rc-path (alist-get 'content response)))
        (should (= 0 (alist-get 'code response)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-getconfigpath-ungated ()
  "`getconfigpath' succeeds regardless of whitelist defcustom values per REQ-4600."
  :tags '(:unit :sandbox)
  (firefox-to-emacs-native-messenger-test-with-sandbox-tempdir dir
    (firefox-to-emacs-native-messenger-test--with-saved-whitelists
     (let* ((rc-path (expand-file-name "tridactylrc" dir))
            (firefox-to-emacs-native-messenger-rcpath-candidates
             (list rc-path)))
       (with-temp-file rc-path (insert "x"))
       (setq firefox-to-emacs-native-messenger-run-whitelist nil)
       (setq firefox-to-emacs-native-messenger-read-whitelist nil)
       (let ((response
              (firefox-to-emacs-native-messenger--getconfigpath-handler
               nil '((cmd . "getconfigpath")))))
         (should (equal rc-path (alist-get 'content response)))
         (should (= 0 (alist-get 'code response))))))))

(ert-deftest firefox-to-emacs-native-messenger-test-getconfigpath-does-not-register ()
  "The returned path is NOT entered into the capability registry per REQ-4700."
  :tags '(:unit :sandbox)
  (firefox-to-emacs-native-messenger-test--with-fresh-registry
   (firefox-to-emacs-native-messenger-test-with-sandbox-tempdir dir
     (let* ((rc-path (expand-file-name "tridactylrc" dir))
            (firefox-to-emacs-native-messenger-rcpath-candidates
             (list rc-path)))
       (with-temp-file rc-path (insert "x"))
       (firefox-to-emacs-native-messenger--getconfigpath-handler
        nil '((cmd . "getconfigpath")))
       (should-not (firefox-to-emacs-native-messenger--registry-contains-p
                    rc-path))))))

;;;; TASK-15500: `read' handler gate-composition tests.

(defmacro firefox-to-emacs-native-messenger-test--with-read-env
    (dir &rest body)
  "Bind DIR to a sandbox tempdir, give a fresh registry, and save whitelists."
  (declare (indent 1) (debug (symbolp body)))
  `(firefox-to-emacs-native-messenger-test-with-sandbox-tempdir ,dir
     (firefox-to-emacs-native-messenger-test--with-fresh-registry
      (firefox-to-emacs-native-messenger-test--with-saved-whitelists
       ,@body))))

(ert-deftest firefox-to-emacs-native-messenger-test-read-handler-raw-tramp-rejected ()
  "Raw TRAMP `file' produces the generic error wording \"remote paths not permitted\"."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-read-env _dir
    (setq firefox-to-emacs-native-messenger-read-whitelist '("*"))
    (let ((response (firefox-to-emacs-native-messenger--read-handler
                     nil '((cmd . "read")
                           (file . "/ssh:host:/etc/hosts")))))
      (should (equal "error" (alist-get 'cmd response)))
      (should (equal "remote paths not permitted"
                     (alist-get 'error response))))))

(ert-deftest firefox-to-emacs-native-messenger-test-read-handler-expansion-tramp-rejected ()
  "Local raw `file' that EXPANDS to a TRAMP path is rejected by the second guard."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-read-env _dir
    (setq firefox-to-emacs-native-messenger-read-whitelist '("*"))
    (let ((process-environment
           (cons "FENM_TEST_REMOTE=/ssh:host:" process-environment)))
      (let ((response (firefox-to-emacs-native-messenger--read-handler
                       nil '((cmd . "read")
                             (file . "$FENM_TEST_REMOTE/etc/hosts")))))
        (should (equal "error" (alist-get 'cmd response)))
        (should (equal "remote paths not permitted"
                       (alist-get 'error response)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-read-handler-empty-whitelist-rejects ()
  "Local path with deny-all whitelist returns \"path not in whitelist\"."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-read-env dir
    (let ((path (expand-file-name "readable.txt" dir)))
      (with-temp-file path (insert "data"))
      (setq firefox-to-emacs-native-messenger-read-whitelist nil)
      (let ((response (firefox-to-emacs-native-messenger--read-handler
                       nil `((cmd . "read") (file . ,path)))))
        (should (equal "error" (alist-get 'cmd response)))
        (should (equal "path not in whitelist"
                       (alist-get 'error response)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-read-handler-allow-all-accepts ()
  "(\"*\") whitelist plus existing local path → success with file content."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-read-env dir
    (let ((path (expand-file-name "ok.txt" dir)))
      (with-temp-file path (insert "hello"))
      (setq firefox-to-emacs-native-messenger-read-whitelist '("*"))
      (let ((response (firefox-to-emacs-native-messenger--read-handler
                       nil `((cmd . "read") (file . ,path)))))
        (should (equal "read" (alist-get 'cmd response)))
        (should (= 0 (alist-get 'code response)))
        (should (equal "hello" (alist-get 'content response)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-read-handler-literal-entry-accepts ()
  "A literal absolute-path whitelist entry matching the expanded path → success."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-read-env dir
    (let ((path (expand-file-name "literal.txt" dir)))
      (with-temp-file path (insert "lit"))
      (setq firefox-to-emacs-native-messenger-read-whitelist
            (list path))
      (let ((response (firefox-to-emacs-native-messenger--read-handler
                       nil `((cmd . "read") (file . ,path)))))
        (should (equal "read" (alist-get 'cmd response)))
        (should (equal "lit" (alist-get 'content response)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-read-handler-glob-entry-accepts ()
  "A glob whitelist entry matching the expanded path → success."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-read-env dir
    (let* ((path (expand-file-name "notes.txt" dir))
           (glob (concat (file-name-as-directory dir) "*.txt")))
      (with-temp-file path (insert "g"))
      (setq firefox-to-emacs-native-messenger-read-whitelist (list glob))
      (let ((response (firefox-to-emacs-native-messenger--read-handler
                       nil `((cmd . "read") (file . ,path)))))
        (should (equal "read" (alist-get 'cmd response)))
        (should (equal "g" (alist-get 'content response)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-read-handler-temp-marker-registered ()
  "<TEMP-PATH> marker matches an expanded path that is in the registry."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-read-env dir
    (let ((path (expand-file-name "tmp.txt" dir)))
      (with-temp-file path (insert "t"))
      (firefox-to-emacs-native-messenger--registry-register path)
      (setq firefox-to-emacs-native-messenger-read-whitelist '("<TEMP-PATH>"))
      (let ((response (firefox-to-emacs-native-messenger--read-handler
                       nil `((cmd . "read") (file . ,path)))))
        (should (equal "read" (alist-get 'cmd response)))
        (should (equal "t" (alist-get 'content response)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-read-handler-temp-marker-unregistered ()
  "<TEMP-PATH> marker rejects when the expanded path is not registered."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-read-env dir
    (let ((path (expand-file-name "tmp.txt" dir)))
      (with-temp-file path (insert "t"))
      (setq firefox-to-emacs-native-messenger-read-whitelist '("<TEMP-PATH>"))
      (let ((response (firefox-to-emacs-native-messenger--read-handler
                       nil `((cmd . "read") (file . ,path)))))
        (should (equal "error" (alist-get 'cmd response)))
        (should (equal "path not in whitelist"
                       (alist-get 'error response)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-read-handler-malformed-before-pat0400 ()
  "Malformed whitelist at gate-check time errors BEFORE PAT-0400 expansion runs.

The test instruments `--expand-path' with a counter; with a malformed
whitelist value and a TRAMP-shaped `file' field, expand-path's counter
MUST remain at zero (validator short-circuits the gate)."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-read-env _dir
    (setq firefox-to-emacs-native-messenger-read-whitelist '("/tmp/legit"))
    (let* ((orig-expand
            (symbol-function 'firefox-to-emacs-native-messenger--expand-path))
           (expand-calls 0))
      (cl-letf (((symbol-function
                  'firefox-to-emacs-native-messenger--expand-path)
                 (lambda (p)
                   (cl-incf expand-calls)
                   (funcall orig-expand p))))
        (let ((firefox-to-emacs-native-messenger-read-whitelist
               '("*" "/etc/oops")))
          (let ((response (firefox-to-emacs-native-messenger--read-handler
                           nil '((cmd . "read")
                                 (file . "/ssh:host:/anything")))))
            (should (equal "error" (alist-get 'cmd response)))
            (should-not (equal "remote paths not permitted"
                               (alist-get 'error response)))
            (should (= 0 expand-calls))))))))

(ert-deftest firefox-to-emacs-native-messenger-test-read-handler-missing-file-field ()
  "Missing or non-string `file' field returns the missing-field generic error."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-read-env _dir
    (setq firefox-to-emacs-native-messenger-read-whitelist '("*"))
    (let ((response (firefox-to-emacs-native-messenger--read-handler
                     nil '((cmd . "read")))))
      (should (equal "error" (alist-get 'cmd response)))
      (should (string-match-p "file" (alist-get 'error response))))
    (let ((response (firefox-to-emacs-native-messenger--read-handler
                     nil '((cmd . "read") (file . nil)))))
      (should (equal "error" (alist-get 'cmd response))))
    (let ((response (firefox-to-emacs-native-messenger--read-handler
                     nil '((cmd . "read") (file . 42)))))
      (should (equal "error" (alist-get 'cmd response))))))

;;;; TASK-15600: `read' handler bounded-read tests.

(ert-deftest firefox-to-emacs-native-messenger-test-read-handler-within-cap-returns-content ()
  "A small file within the cap returns its content verbatim."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-read-env dir
    (let ((path (expand-file-name "ok.txt" dir)))
      (with-temp-file path (insert "abcdef"))
      (setq firefox-to-emacs-native-messenger-read-whitelist '("*"))
      (let ((response (firefox-to-emacs-native-messenger--read-handler
                       nil `((cmd . "read") (file . ,path)))))
        (should (equal "read" (alist-get 'cmd response)))
        (should (equal "abcdef" (alist-get 'content response)))
        (should (= 0 (alist-get 'code response)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-read-handler-exact-cap-returns-content ()
  "A file exactly at outbound-response-cap bytes is returned without truncation.

Per PROTOCOL.md Section 12, the bounded-read cap is (outbound-cap + 1)
so the handler can detect over-cap by observing a fill-to-cap+1 read.
A file of exactly outbound-cap bytes reads outbound-cap bytes (less
than the bounded-read cap)."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-read-env dir
    (let* ((cap 32)
           (firefox-to-emacs-native-messenger-outbound-response-cap cap)
           (path (expand-file-name "atcap.txt" dir))
           (content (make-string cap ?A)))
      (with-temp-file path (insert content))
      (setq firefox-to-emacs-native-messenger-read-whitelist '("*"))
      (let ((response (firefox-to-emacs-native-messenger--read-handler
                       nil `((cmd . "read") (file . ,path)))))
        (should (equal "read" (alist-get 'cmd response)))
        (should (equal content (alist-get 'content response)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-read-handler-over-cap-returns-error ()
  "A file with size > outbound-response-cap returns the file-too-large error."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-read-env dir
    (let* ((cap 32)
           (firefox-to-emacs-native-messenger-outbound-response-cap cap)
           (path (expand-file-name "overcap.txt" dir))
           (content (make-string (+ cap 5) ?B)))
      (with-temp-file path (insert content))
      (setq firefox-to-emacs-native-messenger-read-whitelist '("*"))
      (let ((response (firefox-to-emacs-native-messenger--read-handler
                       nil `((cmd . "read") (file . ,path)))))
        (should (equal "error" (alist-get 'cmd response)))
        (should (equal "file too large to return"
                       (alist-get 'error response)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-read-handler-cap-plus-one-detected ()
  "A file of exactly outbound-cap + 1 bytes triggers the file-too-large error.

PROTOCOL.md Section 12 mandates the bounded-read sees cap+1 bytes
exactly when the file is at least cap+1 bytes; the handler returns
the error without building a content field."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-read-env dir
    (let* ((cap 32)
           (firefox-to-emacs-native-messenger-outbound-response-cap cap)
           (path (expand-file-name "exactly-plus-one.txt" dir))
           (content (make-string (1+ cap) ?C)))
      (with-temp-file path (insert content))
      (setq firefox-to-emacs-native-messenger-read-whitelist '("*"))
      (let ((response (firefox-to-emacs-native-messenger--read-handler
                       nil `((cmd . "read") (file . ,path)))))
        (should (equal "error" (alist-get 'cmd response)))
        (should (equal "file too large to return"
                       (alist-get 'error response)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-read-handler-open-failure-shape ()
  "Opening a non-existent path returns the upstream open-failure shape.

Per PROTOCOL.md Section 5, `read' open-failure is `cmd = \"read\"',
`content = \"\"' (empty string, explicitly present), `code = 2'.  The
shape is distinct from the generic error response."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-read-env dir
    (let ((missing (expand-file-name "definitely-not-here.txt" dir)))
      (setq firefox-to-emacs-native-messenger-read-whitelist '("*"))
      (let ((response (firefox-to-emacs-native-messenger--read-handler
                       nil `((cmd . "read") (file . ,missing)))))
        (should (equal "read" (alist-get 'cmd response)))
        (should (equal "" (alist-get 'content response)))
        (should (= 2 (alist-get 'code response)))))))

;;;; TASK-15800: dispatcher-to-handler wiring tests for the four sync cmds.

(ert-deftest firefox-to-emacs-native-messenger-test-dispatcher-registered-cmds ()
  "All four synchronous v2 cmds are registered in the dispatcher table."
  :tags '(:unit)
  (dolist (cmd '("version" "getconfigpath" "temp" "read"))
    (should (functionp
             (gethash cmd firefox-to-emacs-native-messenger--handlers)))))

(ert-deftest firefox-to-emacs-native-messenger-test-dispatcher-unknown-cmd-still-unhandled ()
  "Registration of the four sync cmds does not break the unknown-cmd path."
  :tags '(:unit)
  (let ((response (firefox-to-emacs-native-messenger--dispatch-request
                   nil '((cmd . "not-a-real-cmd")))))
    (should (equal "error" (alist-get 'cmd response)))
    (should (equal "Unhandled message" (alist-get 'error response)))))

(ert-deftest firefox-to-emacs-native-messenger-test-dispatcher-routes-version ()
  "The dispatcher invokes the `version' handler for a `version' request."
  :tags '(:unit)
  (let ((response (firefox-to-emacs-native-messenger--dispatch-request
                   nil '((cmd . "version")))))
    (should (equal "version" (alist-get 'cmd response)))
    (should (= 0 (alist-get 'code response)))))

(ert-deftest firefox-to-emacs-native-messenger-test-dispatcher-routes-getconfigpath ()
  "The dispatcher invokes the `getconfigpath' handler for that cmd."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test-with-sandbox-tempdir dir
    (let ((firefox-to-emacs-native-messenger-rcpath-candidates
           (list (expand-file-name "absent.rc" dir))))
      (let ((response (firefox-to-emacs-native-messenger--dispatch-request
                       nil '((cmd . "getconfigpath")))))
        (should (equal "getconfigpath" (alist-get 'cmd response)))
        (should (= 1 (alist-get 'code response)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-dispatcher-routes-temp ()
  "The dispatcher invokes the `temp' handler for that cmd."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-temp-handler-env _tmp
    (let ((response (firefox-to-emacs-native-messenger--dispatch-request
                     nil '((cmd . "temp")
                           (content . "x")
                           (prefix . "t")))))
      (should (equal "temp" (alist-get 'cmd response)))
      (should (= 0 (alist-get 'code response))))))

(ert-deftest firefox-to-emacs-native-messenger-test-dispatcher-routes-read ()
  "The dispatcher invokes the `read' handler for that cmd."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-read-env dir
    (let ((path (expand-file-name "x.txt" dir)))
      (with-temp-file path (insert "hello"))
      (setq firefox-to-emacs-native-messenger-read-whitelist '("*"))
      (let ((response (firefox-to-emacs-native-messenger--dispatch-request
                       nil `((cmd . "read") (file . ,path)))))
        (should (equal "read" (alist-get 'cmd response)))
        (should (equal "hello" (alist-get 'content response)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-dispatcher-wire-roundtrip-version ()
  "Full wire round-trip: send a framed `version' request, receive the response."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-listener-sandbox
      _cache _tmp sock
    (firefox-to-emacs-native-messenger-test--with-fresh-connection-registry
     (firefox-to-emacs-native-messenger-start)
     (let ((response (firefox-to-emacs-native-messenger-test-send-frame
                      sock '((cmd . "version")))))
       (should (equal "version" (alist-get 'cmd response)))
       (should (= 0 (alist-get 'code response)))
       (should (stringp (alist-get 'version response)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-dispatcher-wire-roundtrip-temp-then-read ()
  "Full wire round-trip: temp creates a path, read returns its content."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-listener-sandbox
      _cache _tmp sock
    (firefox-to-emacs-native-messenger-test--with-fresh-connection-registry
     (firefox-to-emacs-native-messenger-test--with-saved-whitelists
      (setq firefox-to-emacs-native-messenger-read-whitelist '("<TEMP-PATH>"))
      (firefox-to-emacs-native-messenger-start)
      (let* ((temp-response
              (firefox-to-emacs-native-messenger-test-send-frame
               sock '((cmd . "temp") (content . "round-trip-data") (prefix . "t"))))
             (path (alist-get 'content temp-response))
             (read-response
              (firefox-to-emacs-native-messenger-test-send-frame
               sock `((cmd . "read") (file . ,path)))))
        (should (equal "temp" (alist-get 'cmd temp-response)))
        (should (stringp path))
        (should (equal "read" (alist-get 'cmd read-response)))
        (should (equal "round-trip-data" (alist-get 'content read-response))))))))

;;;; TASK-16000: capability-registry round-trip behavior tests.

(ert-deftest firefox-to-emacs-native-messenger-test-roundtrip-temp-read-ok ()
  "Temp returns a path; subsequent read with `<TEMP-PATH>' in whitelist succeeds."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-listener-sandbox
      _cache _tmp sock
    (firefox-to-emacs-native-messenger-test--with-fresh-connection-registry
     (firefox-to-emacs-native-messenger-test--with-saved-whitelists
      (setq firefox-to-emacs-native-messenger-read-whitelist '("<TEMP-PATH>"))
      (firefox-to-emacs-native-messenger-start)
      (let* ((temp-resp
              (firefox-to-emacs-native-messenger-test-send-frame
               sock '((cmd . "temp")
                      (content . "registered content")
                      (prefix . "host"))))
             (path (alist-get 'content temp-resp))
             (read-resp
              (firefox-to-emacs-native-messenger-test-send-frame
               sock `((cmd . "read") (file . ,path)))))
        (should (equal "temp" (alist-get 'cmd temp-resp)))
        (should (equal "read" (alist-get 'cmd read-resp)))
        (should (equal "registered content"
                       (alist-get 'content read-resp))))))))

(ert-deftest firefox-to-emacs-native-messenger-test-roundtrip-temp-unlink-then-read-rejects ()
  "After the temp file is deleted, read produces \"path not in whitelist\".

Prune-on-access kicks in inside the gate's `--registry-contains-p'
check: the file's `file-attributes' fails, the registry entry is
removed, and the gate match against `<TEMP-PATH>' fails."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-listener-sandbox
      _cache _tmp sock
    (firefox-to-emacs-native-messenger-test--with-fresh-connection-registry
     (firefox-to-emacs-native-messenger-test--with-saved-whitelists
      (setq firefox-to-emacs-native-messenger-read-whitelist '("<TEMP-PATH>"))
      (firefox-to-emacs-native-messenger-start)
      (let* ((temp-resp
              (firefox-to-emacs-native-messenger-test-send-frame
               sock '((cmd . "temp") (content . "x") (prefix . "h"))))
             (path (alist-get 'content temp-resp)))
        (should (file-exists-p path))
        (delete-file path)
        (should-not (file-exists-p path))
        (let ((read-resp
               (firefox-to-emacs-native-messenger-test-send-frame
                sock `((cmd . "read") (file . ,path)))))
          (should (equal "error" (alist-get 'cmd read-resp)))
          (should (equal "path not in whitelist"
                         (alist-get 'error read-resp)))))))))

(ert-deftest firefox-to-emacs-native-messenger-test-roundtrip-restart-clears-registry ()
  "After listener stop+start the previously-registered path is no longer admitted.

Registry lifetime is bounded by a single listener lifetime per
SEC-1200; a stop+start cycle clears the registry, so a previously
registered path fails the `<TEMP-PATH>' gate even though the file may
still exist on disk."
  :tags '(:integration :sandbox)
  (firefox-to-emacs-native-messenger-test--with-listener-sandbox
      _cache _tmp sock
    (firefox-to-emacs-native-messenger-test--with-fresh-connection-registry
     (firefox-to-emacs-native-messenger-test--with-saved-whitelists
      (setq firefox-to-emacs-native-messenger-read-whitelist '("<TEMP-PATH>"))
      (firefox-to-emacs-native-messenger-start)
      (let* ((temp-resp
              (firefox-to-emacs-native-messenger-test-send-frame
               sock '((cmd . "temp") (content . "x") (prefix . "h"))))
             (path (alist-get 'content temp-resp)))
        (should (file-exists-p path))
        (firefox-to-emacs-native-messenger-stop)
        (firefox-to-emacs-native-messenger-start)
        ;; After the start-sweep the on-disk tempfile may be gone (FILE-1600
        ;; is swept on listener start per PAT-0900).  The test does not
        ;; depend on file existence; the registry-cleared-on-stop semantics
        ;; means the gate rejects regardless of whether the file survived.
        (let ((read-resp
               (firefox-to-emacs-native-messenger-test-send-frame
                sock `((cmd . "read") (file . ,path)))))
          (should (equal "error" (alist-get 'cmd read-resp)))
          (should (equal "path not in whitelist"
                         (alist-get 'error read-resp)))))))))


;;;; ============================================================
;;;; Phase 0800: `run' handler, command-gate, and supporting helpers.
;;;; ============================================================

;;;; TASK-16500: command-gate FUNCTION tests.
;;;; The gate function `--run-gate-match-p' consumes the
;;;; `firefox-to-emacs-native-messenger-run-whitelist' defcustom and
;;;; dispatches to `--command-gate-match-p' for individual entries.

(ert-deftest firefox-to-emacs-native-messenger-test-run-gate-empty-rejects ()
  "Nil run-whitelist denies every candidate per REQ-3700.

`nil' is the documented deny-all default; the gate must not even
inspect the candidate string."
  :tags '(:unit :sandbox)
  (firefox-to-emacs-native-messenger-test--with-saved-whitelists
   (setq firefox-to-emacs-native-messenger-run-whitelist nil)
   (firefox-to-emacs-native-messenger-test--with-stub-registry '()
     (should-not (firefox-to-emacs-native-messenger--run-gate-match-p "ls /"))
     (should-not (firefox-to-emacs-native-messenger--run-gate-match-p ""))
     (should-not (firefox-to-emacs-native-messenger--run-gate-match-p
                  "emacsclient /tmp/foo")))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-gate-empty-list-rejects ()
  "Empty list `()' is semantically equivalent to nil and denies every candidate."
  :tags '(:unit :sandbox)
  (firefox-to-emacs-native-messenger-test--with-saved-whitelists
   (setq firefox-to-emacs-native-messenger-run-whitelist '())
   (firefox-to-emacs-native-messenger-test--with-stub-registry '()
     (should-not (firefox-to-emacs-native-messenger--run-gate-match-p
                  "ls /home")))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-gate-allow-all-accepts ()
  "Allow-all sentinel `(\"*\")' accepts every candidate without registry checks.

REQ-3800 pins `(\"*\")' (a one-element list with the string \"*\") as
the allow-all marker.  The gate must not consult the capability
registry in this branch; the test stubs the registry to empty and
still expects acceptance."
  :tags '(:unit :sandbox)
  (firefox-to-emacs-native-messenger-test--with-saved-whitelists
   (setq firefox-to-emacs-native-messenger-run-whitelist '("*"))
   (firefox-to-emacs-native-messenger-test--with-stub-registry '()
     (should (firefox-to-emacs-native-messenger--run-gate-match-p "ls /"))
     (should (firefox-to-emacs-native-messenger--run-gate-match-p "rm -rf /"))
     (should (firefox-to-emacs-native-messenger--run-gate-match-p "")))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-gate-literal-entry-exact-match ()
  "Entry with zero markers matches iff candidate equals entry byte-for-byte."
  :tags '(:unit :sandbox)
  (firefox-to-emacs-native-messenger-test--with-saved-whitelists
   (setq firefox-to-emacs-native-messenger-run-whitelist '("ls /home"))
   (firefox-to-emacs-native-messenger-test--with-stub-registry '()
     (should (firefox-to-emacs-native-messenger--run-gate-match-p "ls /home"))
     (should-not (firefox-to-emacs-native-messenger--run-gate-match-p
                  "ls /home "))
     (should-not (firefox-to-emacs-native-messenger--run-gate-match-p
                  "ls /etc"))
     (should-not (firefox-to-emacs-native-messenger--run-gate-match-p
                  "ls /home extra")))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-gate-single-marker-registered ()
  "Single-marker entry matches iff prefix/suffix and registry hit are all true."
  :tags '(:unit :sandbox)
  (let ((path "/tmp/firefox-to-emacs-native-messenger-tempfiles-1000/tmp_x.txt"))
    (firefox-to-emacs-native-messenger-test--with-saved-whitelists
     (setq firefox-to-emacs-native-messenger-run-whitelist
           '("emacsclient <TEMP-PATH>"))
     (firefox-to-emacs-native-messenger-test--with-stub-registry (list path)
       (should (firefox-to-emacs-native-messenger--run-gate-match-p
                (concat "emacsclient " path)))
       (should-not (firefox-to-emacs-native-messenger--run-gate-match-p
                    (concat "vim " path)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-gate-single-marker-not-registered ()
  "Single-marker entry rejects when the extracted substring is not in the registry."
  :tags '(:unit :sandbox)
  (firefox-to-emacs-native-messenger-test--with-saved-whitelists
   (setq firefox-to-emacs-native-messenger-run-whitelist
         '("emacsclient <TEMP-PATH>"))
   (firefox-to-emacs-native-messenger-test--with-stub-registry '()
     (should-not (firefox-to-emacs-native-messenger--run-gate-match-p
                  "emacsclient /some/path.txt")))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-gate-quoted-marker-suffix ()
  "Single-marker entry with a non-empty suffix literal also matches correctly.

Tests the `rm -f \\='<TEMP-PATH>\\='' shape used by Tridactyl's
`editor_rm' alias: prefix is `rm -f \\='', suffix is `\\='', and the
extracted marker substring is whatever the candidate has between
those literals."
  :tags '(:unit :sandbox)
  (let ((path "/tmp/foo.txt"))
    (firefox-to-emacs-native-messenger-test--with-saved-whitelists
     (setq firefox-to-emacs-native-messenger-run-whitelist
           '("rm -f '<TEMP-PATH>'"))
     (firefox-to-emacs-native-messenger-test--with-stub-registry (list path)
       (should (firefox-to-emacs-native-messenger--run-gate-match-p
                (format "rm -f '%s'" path)))
       (should-not (firefox-to-emacs-native-messenger--run-gate-match-p
                    (format "rm -f \"%s\"" path)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-gate-multi-marker-all-registered ()
  "Multi-marker entry accepts iff every extracted marker is in the registry."
  :tags '(:unit :sandbox)
  (let ((a "/tmp/a") (b "/tmp/b"))
    (firefox-to-emacs-native-messenger-test--with-saved-whitelists
     (setq firefox-to-emacs-native-messenger-run-whitelist
           '("cp <TEMP-PATH> <TEMP-PATH>"))
     (firefox-to-emacs-native-messenger-test--with-stub-registry (list a b)
       (should (firefox-to-emacs-native-messenger--run-gate-match-p
                (concat "cp " a " " b))))
     (firefox-to-emacs-native-messenger-test--with-stub-registry (list a)
       (should-not (firefox-to-emacs-native-messenger--run-gate-match-p
                    (concat "cp " a " " b)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-gate-trailing-bytes-reject ()
  "Single-marker entry rejects when candidate has trailing bytes after the suffix.

Per Section 8.10 the matcher requires the candidate to be fully
consumed; a trailing `; rm -rf ~' after a registered path therefore
rejects rather than being silently dropped."
  :tags '(:unit :sandbox)
  (let ((path "/tmp/foo.txt"))
    (firefox-to-emacs-native-messenger-test--with-saved-whitelists
     (setq firefox-to-emacs-native-messenger-run-whitelist
           '("emacsclient <TEMP-PATH>"))
     (firefox-to-emacs-native-messenger-test--with-stub-registry (list path)
       (should-not (firefox-to-emacs-native-messenger--run-gate-match-p
                    (concat "emacsclient " path " ; rm -rf ~")))))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-gate-multi-entry-any-match ()
  "Multiple entries: gate accepts iff ANY entry matches the candidate."
  :tags '(:unit :sandbox)
  (let ((path "/tmp/x"))
    (firefox-to-emacs-native-messenger-test--with-saved-whitelists
     (setq firefox-to-emacs-native-messenger-run-whitelist
           '("emacsclient <TEMP-PATH>" "rm -f '<TEMP-PATH>'"))
     (firefox-to-emacs-native-messenger-test--with-stub-registry (list path)
       (should (firefox-to-emacs-native-messenger--run-gate-match-p
                (concat "emacsclient " path)))
       (should (firefox-to-emacs-native-messenger--run-gate-match-p
                (format "rm -f '%s'" path)))
       (should-not (firefox-to-emacs-native-messenger--run-gate-match-p
                    (format "cat %s" path)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-gate-malformed-signals ()
  "Per-gate validator at gate-check time signals `whitelist-malformed' on bad input.

PAT-1100 site 3 re-validates the whitelist defcustom at every gate
check.  The let-binding here intentionally bypasses the variable
watcher's `:set' validator (PAT-1100 site 1), simulating a value
that arrived via list mutation or other non-assignment path."
  :tags '(:unit :sandbox)
  (let ((firefox-to-emacs-native-messenger-run-whitelist
         '("*" "/etc/oops")))
    (should-error
     (firefox-to-emacs-native-messenger--run-gate-match-p "ls /")
     :type 'firefox-to-emacs-native-messenger-whitelist-malformed))
  (let ((firefox-to-emacs-native-messenger-run-whitelist
         '("rm <TMP-PATH>")))
    (should-error
     (firefox-to-emacs-native-messenger--run-gate-match-p "rm /tmp/x")
     :type 'firefox-to-emacs-native-messenger-whitelist-malformed)))

;;;; TASK-16700: terminal-cause CAS helper tests (PAT-0700).
;;;; The CAS helper guarantees first-writer-wins semantics on a
;;;; run-state's `terminal-cause' field; subsequent writes are no-ops.

(ert-deftest firefox-to-emacs-native-messenger-test-cas-terminal-cause-unset-to-set ()
  "CAS on an unset state transitions the field and returns t.

The first terminal observer to call CAS on the run-state wins and
gets the t return so it knows it is responsible for the subsequent
response-write step."
  :tags '(:unit)
  (let ((state (make-hash-table :test 'eq)))
    (puthash 'terminal-cause nil state)
    (should (firefox-to-emacs-native-messenger--run-state-cas-terminal-cause
             state 'normal-zero))
    (should (eq 'normal-zero (gethash 'terminal-cause state)))))

(ert-deftest firefox-to-emacs-native-messenger-test-cas-terminal-cause-already-set-noop ()
  "CAS on an already-set state returns nil and does not mutate the field.

The second-arriving terminal observer must NOT overwrite the
first-writer's decision; the response shape is determined by the
first cause that fires per PAT-0700."
  :tags '(:unit)
  (let ((state (make-hash-table :test 'eq)))
    (puthash 'terminal-cause 'overflow state)
    (should-not (firefox-to-emacs-native-messenger--run-state-cas-terminal-cause
                 state 'normal-nonzero))
    (should (eq 'overflow (gethash 'terminal-cause state)))))

(ert-deftest firefox-to-emacs-native-messenger-test-cas-terminal-cause-first-wins ()
  "Two sequential CAS calls: the first wins; the second returns nil and no-ops.

This is the documented `concurrent (simulated by sequencing two CAS
calls) -> first wins' case in PAT-0700: under single-threaded ELisp
semantics the sequencing is deterministic, and the helper guarantees
that the second call sees the first's effect."
  :tags '(:unit)
  (let ((state (make-hash-table :test 'eq)))
    (puthash 'terminal-cause nil state)
    (should (firefox-to-emacs-native-messenger--run-state-cas-terminal-cause
             state 'timeout))
    (should-not (firefox-to-emacs-native-messenger--run-state-cas-terminal-cause
                 state 'overflow))
    (should (eq 'timeout (gethash 'terminal-cause state)))))

;;;; TASK-16900: timer-cancel helper tests.
;;;; The helper wraps `cancel-timer' with nil-tolerance so callers can
;;;; pass possibly-nil timer slots from the run-state plist or per-
;;;; connection plist without conditional logic at every callsite.

(ert-deftest firefox-to-emacs-native-messenger-test-cancel-timer-nil-noop ()
  "Calling the cancel-timer helper with nil is a silent no-op.

The run-state's `timeout-timer' field is nil when the run-timeout
defcustom is unset; the helper must accept nil without error."
  :tags '(:unit)
  (should-not (firefox-to-emacs-native-messenger--cancel-timer nil)))

(ert-deftest firefox-to-emacs-native-messenger-test-cancel-timer-active-timer ()
  "Calling the helper on an active timer cancels it.

After cancellation the timer must NOT fire even after waiting past
its scheduled time."
  :tags '(:unit)
  (let* ((fired nil)
         (timer (run-at-time 0.05 nil (lambda () (setq fired t)))))
    (firefox-to-emacs-native-messenger--cancel-timer timer)
    (sleep-for 0.15)
    (should-not fired)))

(ert-deftest firefox-to-emacs-native-messenger-test-cancel-timer-already-cancelled-noop ()
  "Calling the helper twice on the same timer is a no-op the second time.

Idempotency is required because multiple cleanup paths (subprocess
sentinel, connection sentinel, signal-escalation) may all attempt
to cancel the same timer; the helper must accept repeated calls."
  :tags '(:unit)
  (let ((timer (run-at-time 60 nil (lambda ()))))
    (firefox-to-emacs-native-messenger--cancel-timer timer)
    (firefox-to-emacs-native-messenger--cancel-timer timer)))

(ert-deftest firefox-to-emacs-native-messenger-test-cancel-timer-already-fired-noop ()
  "Calling the helper on an already-fired timer is a silent no-op.

After a timer fires it is removed from the active list; calling
`cancel-timer' on it is implicitly a no-op, and our wrapper
preserves that behavior."
  :tags '(:unit)
  (let* ((fired nil)
         (timer (run-at-time 0.01 nil (lambda () (setq fired t)))))
    (sleep-for 0.1)
    (should fired)
    (firefox-to-emacs-native-messenger--cancel-timer timer)))

(ert-deftest firefox-to-emacs-native-messenger-test-cancel-timer-non-timer-noop ()
  "Calling the helper on a non-timer value is a silent no-op.

Defensive guard against accidental nil-vs-timer confusion in upper
layers: passing an integer or symbol should not crash the cleanup
path."
  :tags '(:unit)
  (should-not (firefox-to-emacs-native-messenger--cancel-timer 42))
  (should-not (firefox-to-emacs-native-messenger--cancel-timer 'not-a-timer))
  (should-not (firefox-to-emacs-native-messenger--cancel-timer "string")))

;;;; TASK-17100: signal-escalation helper tests (PAT-0800).
;;;; The helper sends SIGINT to the negative pgrp; schedules SIGTERM
;;;; after a configurable grace; schedules SIGKILL after a further
;;;; grace; each escalation step checks `process-live-p' so a
;;;; subprocess that exits early skips later signals.

(ert-deftest firefox-to-emacs-native-messenger-test-signal-escalate-nil-pgrp-noop ()
  "With pgrp nil the escalation helper is a silent no-op.

Defensive guard against partial state initialization (e.g., the
subprocess died before `process-attributes' captured a pgrp and the
fallback didn't populate either)."
  :tags '(:unit)
  (let ((state (make-hash-table :test 'eq)))
    (puthash 'pgrp nil state)
    (puthash 'subprocess nil state)
    (should-not (firefox-to-emacs-native-messenger--run-signal-escalate state))))

(ert-deftest firefox-to-emacs-native-messenger-test-signal-escalate-dead-subprocess-noop ()
  "With a subprocess no longer live the escalation helper is a silent no-op.

The connection-loss path may invoke escalation after the subprocess
has already exited; the helper must accept that gracefully."
  :tags '(:unit)
  (let ((state (make-hash-table :test 'eq))
        (dead-proc (make-process
                    :name "fenm-test-escalate-dead"
                    :command (list "/bin/sh" "-c" "true")
                    :noquery t)))
    (while (process-live-p dead-proc)
      (accept-process-output dead-proc 0.05))
    (puthash 'pgrp (process-id dead-proc) state)
    (puthash 'subprocess dead-proc state)
    (should-not
     (firefox-to-emacs-native-messenger--run-signal-escalate state))))

(ert-deftest firefox-to-emacs-native-messenger-test-signal-escalate-sigint-reaps-vanilla ()
  "A vanilla sleeping subprocess is reaped by the SIGINT step.

`/bin/sh -c \"sleep 30\"' exits on SIGINT directed at the
pgrp; the SIGTERM and SIGKILL escalation steps `process-live-p'
guards make them no-ops in this case."
  :tags '(:integration :run-subprocess :slow)
  (let* ((proc (make-process
                :name "fenm-test-escalate-sigint-vanilla"
                :command (list "/bin/sh" "-c" "sleep 30")
                :coding 'binary
                :stderr nil
                :noquery t))
         (state (make-hash-table :test 'eq))
         (pid (process-id proc))
         (pgrp (or (alist-get 'pgrp (process-attributes pid))
                   pid)))
    (unwind-protect
        (let ((firefox-to-emacs-native-messenger-run-sigint-grace 0.5)
              (firefox-to-emacs-native-messenger-run-sigterm-grace 0.5))
          (puthash 'pgrp pgrp state)
          (puthash 'subprocess proc state)
          (firefox-to-emacs-native-messenger--run-signal-escalate state)
          (let ((deadline (+ (float-time) 1.5)))
            (while (and (process-live-p proc) (< (float-time) deadline))
              (accept-process-output proc 0.05)))
          (should-not (process-live-p proc)))
      (when (process-live-p proc)
        (ignore-errors (signal-process (- pgrp) 'kill))))))

(ert-deftest firefox-to-emacs-native-messenger-test-signal-escalate-sigterm-reaps-sigint-trapped ()
  "A subprocess that traps SIGINT is reaped by the SIGTERM escalation step.

The shell ignores SIGINT via `trap \\='\\=' INT'; the inherited
disposition propagates to the sleep child.  When the SIGINT step
fires, the pgrp-wide signal has no effect; the SIGTERM step then
reaps both processes."
  :tags '(:integration :run-subprocess :slow)
  (let* ((proc (make-process
                :name "fenm-test-escalate-sigint-trapped"
                :command (list "/bin/sh" "-c"
                               "trap '' INT; sleep 30")
                :coding 'binary
                :stderr nil
                :noquery t))
         (state (make-hash-table :test 'eq))
         (pid (process-id proc))
         (pgrp (or (alist-get 'pgrp (process-attributes pid))
                   pid)))
    (unwind-protect
        (let ((firefox-to-emacs-native-messenger-run-sigint-grace 0.3)
              (firefox-to-emacs-native-messenger-run-sigterm-grace 0.5))
          (puthash 'pgrp pgrp state)
          (puthash 'subprocess proc state)
          (firefox-to-emacs-native-messenger--run-signal-escalate state)
          (let ((deadline (+ (float-time) 2.0)))
            (while (and (process-live-p proc) (< (float-time) deadline))
              (accept-process-output proc 0.05)))
          (should-not (process-live-p proc)))
      (when (process-live-p proc)
        (ignore-errors (signal-process (- pgrp) 'kill))))))

(ert-deftest firefox-to-emacs-native-messenger-test-signal-escalate-sigkill-reaps-sigterm-trapped ()
  "A subprocess that traps SIGINT and SIGTERM is reaped by the SIGKILL step.

The shell ignores both SIGINT and SIGTERM via `trap \\='\\=' INT TERM';
SIGKILL is the only way out.  The escalation chain must reach the
SIGKILL step within sigint-grace + sigterm-grace seconds."
  :tags '(:integration :run-subprocess :slow)
  (let* ((proc (make-process
                :name "fenm-test-escalate-sigterm-trapped"
                :command (list "/bin/sh" "-c"
                               "trap '' INT TERM; sleep 30")
                :coding 'binary
                :stderr nil
                :noquery t))
         (state (make-hash-table :test 'eq))
         (pid (process-id proc))
         (pgrp (or (alist-get 'pgrp (process-attributes pid))
                   pid)))
    (unwind-protect
        (let ((firefox-to-emacs-native-messenger-run-sigint-grace 0.3)
              (firefox-to-emacs-native-messenger-run-sigterm-grace 0.3))
          (puthash 'pgrp pgrp state)
          (puthash 'subprocess proc state)
          (firefox-to-emacs-native-messenger--run-signal-escalate state)
          (let ((deadline (+ (float-time) 2.0)))
            (while (and (process-live-p proc) (< (float-time) deadline))
              (accept-process-output proc 0.05)))
          (should-not (process-live-p proc)))
      (when (process-live-p proc)
        (ignore-errors (signal-process (- pgrp) 'kill))))))

;;;; TASK-17300: run accumulator filter tests (REQ-2800 / PAT-0700).
;;;; The accumulator filter is the subprocess-side process filter for
;;;; the run handler.  It accumulates stdout+stderr bytes into the
;;;; run-state's output-buffer, tracks output-bytes, and triggers the
;;;; overflow CAS + signal-escalation path when output-bytes exceeds
;;;; the captured output-cap.  The filter never emits a wire response.

(defmacro firefox-to-emacs-native-messenger-test--with-run-state-on-subprocess
    (state-var proc-var output-cap &rest body)
  "Bind STATE-VAR to a fresh run-state, PROC-VAR to a sleep subprocess.

Emacs creates each subprocess as its own session leader so its pgrp == pid; the
state is populated with sensible defaults (terminal-cause unset,
empty output-buffer, output-bytes 0, the given OUTPUT-CAP, and the
subprocess + pgrp linkage).  PROC-VAR carries the run-state via
its process plist key so the filter can resolve state from PROC.
On body exit the subprocess is unconditionally killed via SIGKILL
on its pgrp."
  (declare (indent 3) (debug (symbolp symbolp form body)))
  `(let* ((,proc-var (make-process
                      :name "fenm-test-run-accum"
                      :command (list "/bin/sh" "-c" "sleep 30")
                      :coding 'binary
                      :stderr nil
                      :noquery t))
          (,state-var (make-hash-table :test 'eq))
          (pid (process-id ,proc-var))
          (pgrp (or (alist-get 'pgrp (process-attributes pid)) pid)))
     (unwind-protect
         (progn
           (puthash 'terminal-cause nil ,state-var)
           (puthash 'output-buffer (unibyte-string) ,state-var)
           (puthash 'output-bytes 0 ,state-var)
           (puthash 'output-cap ,output-cap ,state-var)
           (puthash 'subprocess ,proc-var ,state-var)
           (puthash 'pgrp pgrp ,state-var)
           (process-put ,proc-var
                        firefox-to-emacs-native-messenger--subprocess-key-run-state
                        ,state-var)
           ,@body)
       (when (process-live-p ,proc-var)
         (ignore-errors (signal-process (- pgrp) 'kill))))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-accumulator-filter-appends-bytes ()
  "Filter appends incoming chunks to the output-buffer field and updates output-bytes."
  :tags '(:integration :run-subprocess)
  (firefox-to-emacs-native-messenger-test--with-run-state-on-subprocess
      state proc 1024
    (firefox-to-emacs-native-messenger--run-accumulator-filter proc "hello")
    (should (equal "hello" (gethash 'output-buffer state)))
    (should (= 5 (gethash 'output-bytes state)))
    (firefox-to-emacs-native-messenger--run-accumulator-filter proc " world")
    (should (equal "hello world" (gethash 'output-buffer state)))
    (should (= 11 (gethash 'output-bytes state)))
    ;; Terminal-cause must still be unset because we did not exceed cap.
    (should-not (gethash 'terminal-cause state))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-accumulator-filter-overflow-triggers-cas ()
  "Exceeding output-cap CAS-sets terminal-cause to overflow.

The first observer to see output-bytes > output-cap claims the
terminal cause; subsequent observers find it set."
  :tags '(:integration :run-subprocess :slow)
  (firefox-to-emacs-native-messenger-test--with-run-state-on-subprocess
      state proc 10
    (let ((firefox-to-emacs-native-messenger-run-sigint-grace 0.3)
          (firefox-to-emacs-native-messenger-run-sigterm-grace 0.3))
      (firefox-to-emacs-native-messenger--run-accumulator-filter
       proc "this-chunk-exceeds-cap")
      (should (eq 'overflow (gethash 'terminal-cause state)))
      ;; Subprocess must terminate within the escalation budget.
      (let ((deadline (+ (float-time) 2.0)))
        (while (and (process-live-p proc) (< (float-time) deadline))
          (accept-process-output proc 0.05)))
      (should-not (process-live-p proc)))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-accumulator-filter-overflow-once ()
  "When terminal-cause is already set, filter does NOT re-invoke escalation.

CAS guarantees first-writer-wins; subsequent filter invocations
that would have triggered overflow find the cause already set and
skip the escalation step.  Verified by stubbing escalate to count
calls."
  :tags '(:integration :run-subprocess)
  (firefox-to-emacs-native-messenger-test--with-run-state-on-subprocess
      state proc 10
    (puthash 'terminal-cause 'overflow state)
    (let ((escalate-calls 0))
      (cl-letf (((symbol-function
                  'firefox-to-emacs-native-messenger--run-signal-escalate)
                 (lambda (_state) (cl-incf escalate-calls))))
        (firefox-to-emacs-native-messenger--run-accumulator-filter
         proc "this-chunk-exceeds-cap-twice")
        (should (= 0 escalate-calls))
        (should (eq 'overflow (gethash 'terminal-cause state)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-accumulator-filter-no-run-state-noop ()
  "If the subprocess has no run-state on its plist, the filter is a silent no-op.

Defensive guard against partial initialization (e.g., the handler
attached the filter but errored before populating the run-state)."
  :tags '(:integration :run-subprocess)
  (let ((proc (make-process
               :name "fenm-test-accum-no-state"
               :command (list "/bin/sh" "-c" "exec sleep 30")
               :coding 'binary
               :stderr nil
               :noquery t)))
    (unwind-protect
        (progn
          (should-not
           (process-get proc
                        firefox-to-emacs-native-messenger--subprocess-key-run-state))
          (should-not
           (firefox-to-emacs-native-messenger--run-accumulator-filter
            proc "anything")))
      (when (process-live-p proc)
        (ignore-errors (signal-process (- (process-id proc)) 'kill))))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-accumulator-filter-zero-byte-chunks-ok ()
  "A zero-byte chunk is appended without errors and does not advance counters.

Defensive guard for synthetic test inputs and rare empty-write
scenarios from the subprocess side."
  :tags '(:integration :run-subprocess)
  (firefox-to-emacs-native-messenger-test--with-run-state-on-subprocess
      state proc 100
    (firefox-to-emacs-native-messenger--run-accumulator-filter proc "")
    (should (equal "" (gethash 'output-buffer state)))
    (should (= 0 (gethash 'output-bytes state)))
    (should-not (gethash 'terminal-cause state))))

;;;; TASK-17500: run subprocess sentinel tests (PAT-0700).
;;;; The sentinel observes subprocess exit, CAS-sets terminal-cause to
;;;; `normal-zero' or `normal-nonzero' based on exit status if not set,
;;;; cancels any pending timeout-timer, and dispatches the wire
;;;; response according to the final `terminal-cause' value.  No
;;;; response is sent when `terminal-cause' is `connection-loss'.

(defmacro firefox-to-emacs-native-messenger-test--with-completed-subprocess
    (proc-var command-list &rest body)
  "Bind PROC-VAR to a subprocess running COMMAND-LIST; wait for exit; run BODY.

The subprocess's coding is `binary' and `:stderr nil' so its output
capture conventions match the production run handler's invocation."
  (declare (indent 2) (debug (symbolp form body)))
  `(let ((,proc-var (make-process
                     :name "fenm-test-completed-sub"
                     :command ,command-list
                     :coding 'binary
                     :stderr nil
                     :noquery t)))
     (unwind-protect
         (progn
           (let ((deadline (+ (float-time) 2.0)))
             (while (and (process-live-p ,proc-var) (< (float-time) deadline))
               (accept-process-output ,proc-var 0.05)))
           ,@body)
       (when (process-live-p ,proc-var)
         (ignore-errors (signal-process (process-id ,proc-var) 'kill))))))

(defmacro firefox-to-emacs-native-messenger-test--with-run-sentinel-fixture
    (state-var sub-var conn-var captured-var command-list &rest body)
  "Configure a run-state and stub write-response for a sentinel test.

CONN-VAR is a live `sleep 30' subprocess used as a stand-in for the
listener-side connection so the sentinel sees a live process and
forwards the response object to the stubbed
`firefox-to-emacs-native-messenger--write-response'.  Calls to the
writer are collected into CAPTURED-VAR as (CONN . RESPONSE) pairs.

CONN-VAR is launched OUTSIDE the run-state's pgrp so the subprocess
escalation chain (if it fires) cannot affect it."
  (declare (indent 5) (debug (symbolp symbolp symbolp symbolp form body)))
  `(let ((,conn-var (make-process
                     :name "fenm-test-conn-proxy"
                     :command (list "/bin/sh" "-c" "exec sleep 30")
                     :coding 'binary
                     :stderr nil
                     :noquery t))
         (,captured-var '()))
     (unwind-protect
         (firefox-to-emacs-native-messenger-test--with-completed-subprocess
             ,sub-var ,command-list
           (let ((,state-var (make-hash-table :test 'eq)))
             (puthash 'terminal-cause nil ,state-var)
             (puthash 'output-buffer (unibyte-string) ,state-var)
             (puthash 'output-bytes 0 ,state-var)
             (puthash 'output-cap 1024 ,state-var)
             (puthash 'subprocess ,sub-var ,state-var)
             (puthash 'connection ,conn-var ,state-var)
             (puthash 'command-string "test-cmd" ,state-var)
             (puthash 'pgrp (process-id ,sub-var) ,state-var)
             (puthash 'timeout-timer nil ,state-var)
             (process-put ,sub-var
                          firefox-to-emacs-native-messenger--subprocess-key-run-state
                          ,state-var)
             (cl-letf (((symbol-function
                         'firefox-to-emacs-native-messenger--write-response)
                        (lambda (c r) (push (cons c r) ,captured-var))))
               ,@body)))
       (when (process-live-p ,conn-var)
         (ignore-errors (signal-process (process-id ,conn-var) 'kill))))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-sentinel-normal-zero-exit ()
  "A subprocess exit with code 0 and no prior terminal-cause CAS-sets normal-zero.

The sentinel must build a response with `cmd' = \"run\", echoed
`command', present `content' (possibly empty), and `code' = 0."
  :tags '(:integration :run-subprocess)
  (firefox-to-emacs-native-messenger-test--with-run-sentinel-fixture
      state sub conn captured (list "/bin/sh" "-c" "true")
    (firefox-to-emacs-native-messenger--run-subprocess-sentinel
     sub "finished\n")
    (should (eq 'normal-zero (gethash 'terminal-cause state)))
    (should (= 1 (length captured)))
    (let* ((entry (car captured))
           (response (cdr entry)))
      (should (eq conn (car entry)))
      (should (equal "run" (alist-get 'cmd response)))
      (should (equal "test-cmd" (alist-get 'command response)))
      (should (equal "" (alist-get 'content response)))
      (should (= 0 (alist-get 'code response))))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-sentinel-normal-nonzero-exit ()
  "A subprocess exit with non-zero code CAS-sets `normal-nonzero'.

The response's `code' field carries the actual exit status."
  :tags '(:integration :run-subprocess)
  (firefox-to-emacs-native-messenger-test--with-run-sentinel-fixture
      state sub conn captured (list "/bin/sh" "-c" "exit 7")
    (firefox-to-emacs-native-messenger--run-subprocess-sentinel
     sub "exited\n")
    (should (eq 'normal-nonzero (gethash 'terminal-cause state)))
    (should (= 1 (length captured)))
    (let ((response (cdr (car captured))))
      (should (equal "run" (alist-get 'cmd response)))
      (should (= 7 (alist-get 'code response))))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-sentinel-connection-loss-no-response ()
  "When terminal-cause is `connection-loss', the sentinel sends NO response.

The connection has been torn down by the connection sentinel; the
run subprocess sentinel must log and bail out without invoking the
response writer."
  :tags '(:integration :run-subprocess)
  (firefox-to-emacs-native-messenger-test--with-run-sentinel-fixture
      state sub conn captured (list "/bin/sh" "-c" "true")
    (puthash 'terminal-cause 'connection-loss state)
    (firefox-to-emacs-native-messenger--run-subprocess-sentinel
     sub "finished\n")
    (should (eq 'connection-loss (gethash 'terminal-cause state)))
    (should (null captured))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-sentinel-overflow-error-response ()
  "When terminal-cause is `overflow', the response is the generic too-large error.

The wording is the shared `response too large' string used for
both run-side overflow and the writer's post-serialize over-cap
replacement (PROTOCOL.md Section 11)."
  :tags '(:integration :run-subprocess)
  (firefox-to-emacs-native-messenger-test--with-run-sentinel-fixture
      state sub conn captured (list "/bin/sh" "-c" "true")
    (puthash 'terminal-cause 'overflow state)
    (firefox-to-emacs-native-messenger--run-subprocess-sentinel
     sub "finished\n")
    (should (= 1 (length captured)))
    (let ((response (cdr (car captured))))
      (should (equal "error" (alist-get 'cmd response)))
      (should (equal "response too large" (alist-get 'error response))))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-sentinel-timeout-error-response ()
  "When terminal-cause is `timeout', the response carries the timeout wording.

The wording is `run timeout exceeded' per PROTOCOL.md Section 15."
  :tags '(:integration :run-subprocess)
  (firefox-to-emacs-native-messenger-test--with-run-sentinel-fixture
      state sub conn captured (list "/bin/sh" "-c" "true")
    (puthash 'terminal-cause 'timeout state)
    (firefox-to-emacs-native-messenger--run-subprocess-sentinel
     sub "finished\n")
    (should (= 1 (length captured)))
    (let ((response (cdr (car captured))))
      (should (equal "error" (alist-get 'cmd response)))
      (should (equal "run timeout exceeded" (alist-get 'error response))))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-sentinel-cancels-timeout-timer ()
  "The sentinel cancels the run-state's `timeout-timer' on subprocess exit.

After the sentinel runs, the timer must NOT fire even after its
scheduled time elapses; the state's `timeout-timer' field is
cleared to nil."
  :tags '(:integration :run-subprocess)
  (firefox-to-emacs-native-messenger-test--with-run-sentinel-fixture
      state sub conn captured (list "/bin/sh" "-c" "true")
    (let ((fired nil))
      (puthash 'timeout-timer
               (run-at-time 0.1 nil (lambda () (setq fired t)))
               state)
      (firefox-to-emacs-native-messenger--run-subprocess-sentinel
       sub "finished\n")
      (should-not (gethash 'timeout-timer state))
      (sleep-for 0.2)
      (should-not fired))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-sentinel-output-buffer-in-response ()
  "The captured output-buffer becomes the response's `content' field.

Verifies the byte-faithful capture per Section 10.2: bytes the
subprocess wrote (and the accumulator filter captured) appear in
the response's content."
  :tags '(:integration :run-subprocess)
  (firefox-to-emacs-native-messenger-test--with-run-sentinel-fixture
      state sub conn captured (list "/bin/sh" "-c" "true")
    (puthash 'output-buffer "hello\n" state)
    (puthash 'output-bytes 6 state)
    (firefox-to-emacs-native-messenger--run-subprocess-sentinel
     sub "finished\n")
    (let ((response (cdr (car captured))))
      (should (equal "hello\n" (alist-get 'content response))))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-sentinel-skips-intermediate-events ()
  "The sentinel ignores non-terminal events; only acts on exit/signal status."
  :tags '(:integration :run-subprocess)
  (let ((proc (make-process
               :name "fenm-test-sentinel-skips"
               :command (list "/bin/sh" "-c" "exec sleep 30")
               :coding 'binary
               :stderr nil
               :noquery t))
        (captured '()))
    (unwind-protect
        (let ((state (make-hash-table :test 'eq)))
          (puthash 'terminal-cause nil state)
          (puthash 'output-buffer (unibyte-string) state)
          (puthash 'output-bytes 0 state)
          (puthash 'output-cap 1024 state)
          (puthash 'subprocess proc state)
          (puthash 'connection proc state)
          (puthash 'command-string "sleep 30" state)
          (puthash 'pgrp (process-id proc) state)
          (puthash 'timeout-timer nil state)
          (process-put proc
                       firefox-to-emacs-native-messenger--subprocess-key-run-state
                       state)
          (cl-letf (((symbol-function
                      'firefox-to-emacs-native-messenger--write-response)
                     (lambda (c r) (push (cons c r) captured))))
            (firefox-to-emacs-native-messenger--run-subprocess-sentinel
             proc "open from foo\n")
            (should-not (gethash 'terminal-cause state))
            (should (null captured))))
      (when (process-live-p proc)
        (ignore-errors (signal-process (process-id proc) 'kill))))))

;;;; TASK-17700: run-timeout timer tests.
;;;; The timer is scheduled iff the `run-timeout' defcustom is non-nil.
;;;; On expiration it CAS-sets `terminal-cause' to `timeout' and
;;;; invokes signal-escalation; the subprocess sentinel observes the
;;;; subsequent exit and builds the `run timeout exceeded' error.

(ert-deftest firefox-to-emacs-native-messenger-test-run-timeout-schedule-nil-noop ()
  "When the run-timeout defcustom is nil, the schedule helper is a no-op.

The state's `timeout-timer' field remains nil and no callback is
ever queued."
  :tags '(:unit)
  (let ((state (make-hash-table :test 'eq))
        (firefox-to-emacs-native-messenger-run-timeout nil))
    (puthash 'timeout-timer nil state)
    (firefox-to-emacs-native-messenger--run-timeout-schedule state)
    (should-not (gethash 'timeout-timer state))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-timeout-schedule-zero-noop ()
  "A zero or negative timeout is treated as `disabled' (defensive)."
  :tags '(:unit)
  (let ((state (make-hash-table :test 'eq)))
    (puthash 'timeout-timer nil state)
    (let ((firefox-to-emacs-native-messenger-run-timeout 0))
      (firefox-to-emacs-native-messenger--run-timeout-schedule state)
      (should-not (gethash 'timeout-timer state)))
    (let ((firefox-to-emacs-native-messenger-run-timeout -1))
      (firefox-to-emacs-native-messenger--run-timeout-schedule state)
      (should-not (gethash 'timeout-timer state)))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-timeout-schedule-creates-timer ()
  "Setting the defcustom to a positive number installs an active timer."
  :tags '(:unit)
  (let ((state (make-hash-table :test 'eq))
        (firefox-to-emacs-native-messenger-run-timeout 60))
    (puthash 'timeout-timer nil state)
    (unwind-protect
        (progn
          (firefox-to-emacs-native-messenger--run-timeout-schedule state)
          (should (timerp (gethash 'timeout-timer state))))
      (firefox-to-emacs-native-messenger--cancel-timer
       (gethash 'timeout-timer state)))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-timeout-expire-cas-and-escalate ()
  "The expire handler CAS-sets `terminal-cause' to timeout AND invokes escalate."
  :tags '(:unit)
  (let ((state (make-hash-table :test 'eq))
        (escalate-calls 0))
    (puthash 'terminal-cause nil state)
    (puthash 'subprocess nil state)
    (cl-letf (((symbol-function
                'firefox-to-emacs-native-messenger--run-signal-escalate)
               (lambda (_state) (cl-incf escalate-calls))))
      (firefox-to-emacs-native-messenger--run-timeout-expire state)
      (should (eq 'timeout (gethash 'terminal-cause state)))
      (should (= 1 escalate-calls)))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-timeout-expire-already-set-no-escalate ()
  "When `terminal-cause' is already set, expire does NOT re-escalate.

CAS returns nil; the helper short-circuits before calling escalate."
  :tags '(:unit)
  (let ((state (make-hash-table :test 'eq))
        (escalate-calls 0))
    (puthash 'terminal-cause 'normal-zero state)
    (cl-letf (((symbol-function
                'firefox-to-emacs-native-messenger--run-signal-escalate)
               (lambda (_state) (cl-incf escalate-calls))))
      (firefox-to-emacs-native-messenger--run-timeout-expire state)
      (should (eq 'normal-zero (gethash 'terminal-cause state)))
      (should (= 0 escalate-calls)))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-timeout-end-to-end ()
  "End-to-end: scheduled timer fires, signals subprocess, sets terminal-cause.

The subprocess is `sleep 30' (Emacs makes each subprocess its own session leader); the timeout is 0.2 seconds.
Within ~2 seconds (timeout + sigint-grace + sigterm-grace + buffer)
the subprocess must be terminated and `terminal-cause' must be
`timeout'."
  :tags '(:integration :run-subprocess :slow)
  (let* ((proc (make-process
                :name "fenm-test-timeout-e2e"
                :command (list "/bin/sh" "-c" "sleep 30")
                :coding 'binary
                :stderr nil
                :noquery t))
         (state (make-hash-table :test 'eq))
         (pid (process-id proc))
         (pgrp (or (alist-get 'pgrp (process-attributes pid)) pid)))
    (unwind-protect
        (let ((firefox-to-emacs-native-messenger-run-timeout 0.2)
              (firefox-to-emacs-native-messenger-run-sigint-grace 0.3)
              (firefox-to-emacs-native-messenger-run-sigterm-grace 0.3))
          (puthash 'terminal-cause nil state)
          (puthash 'subprocess proc state)
          (puthash 'pgrp pgrp state)
          (puthash 'timeout-timer nil state)
          (firefox-to-emacs-native-messenger--run-timeout-schedule state)
          (should (timerp (gethash 'timeout-timer state)))
          (let ((deadline (+ (float-time) 2.0)))
            (while (and (process-live-p proc) (< (float-time) deadline))
              (accept-process-output proc 0.05)))
          (should-not (process-live-p proc))
          (should (eq 'timeout (gethash 'terminal-cause state))))
      (when (process-live-p proc)
        (ignore-errors (signal-process (- pgrp) 'kill)))
      (firefox-to-emacs-native-messenger--cancel-timer
       (gethash 'timeout-timer state)))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-timeout-cancelable ()
  "A scheduled timer can be cancelled via the cancel-timer helper before firing.

The cancelled timer does NOT fire even after its scheduled time elapses."
  :tags '(:integration :run-subprocess :slow)
  (let ((state (make-hash-table :test 'eq))
        (escalate-calls 0))
    (puthash 'terminal-cause nil state)
    (puthash 'subprocess nil state)
    (cl-letf (((symbol-function
                'firefox-to-emacs-native-messenger--run-signal-escalate)
               (lambda (_s) (cl-incf escalate-calls))))
      (let ((firefox-to-emacs-native-messenger-run-timeout 0.2))
        (firefox-to-emacs-native-messenger--run-timeout-schedule state)
        (should (timerp (gethash 'timeout-timer state)))
        (firefox-to-emacs-native-messenger--cancel-timer
         (gethash 'timeout-timer state))
        (sleep-for 0.4)
        (should (= 0 escalate-calls))
        (should-not (gethash 'terminal-cause state))))))

;;;; TASK-17900: connection-sentinel run-cancellation extension tests.
;;;; When peer close arrives while the connection state is `dispatched',
;;;; the sentinel must locate the linked run subprocess, CAS-set its
;;;; terminal-cause to `connection-loss', cancel any pending
;;;; timeout-timer, and invoke signal-escalation per PAT-0800.

(ert-deftest firefox-to-emacs-native-messenger-test-connection-sentinel-cancels-run-on-peer-close ()
  "Peer close in dispatched state CAS-sets connection-loss and escalates the run.

Sets up a connection (live process) linked via the
`run-subprocess' plist key to a long-running shell subprocess; populates a
run-state with `terminal-cause' nil and a pgrp; sets the
connection's state to `dispatched'; calls the connection sentinel
with a close-event string; asserts the run-state's terminal-cause
becomes `connection-loss', the stubbed signal-escalate was called
exactly once, and the connection state transitioned to `closing'."
  :tags '(:integration :run-subprocess)
  (firefox-to-emacs-native-messenger-test--with-fresh-connection-registry
   (let* ((conn (make-process
                 :name "fenm-test-conn-sentinel-cancel"
                 :command (list "/bin/sh" "-c" "exec sleep 30")
                 :coding 'binary :stderr nil :noquery t))
          (sub (make-process
                :name "fenm-test-sub-sentinel-cancel"
                :command (list "/bin/sh" "-c" "sleep 30")
                :coding 'binary :stderr nil :noquery t))
          (state (make-hash-table :test 'eq))
          (escalate-calls 0))
     (unwind-protect
         (progn
           (puthash 'terminal-cause nil state)
           (puthash 'subprocess sub state)
           (puthash 'pgrp (process-id sub) state)
           (puthash 'timeout-timer nil state)
           (process-put sub
                        firefox-to-emacs-native-messenger--subprocess-key-run-state
                        state)
           (process-put conn
                        firefox-to-emacs-native-messenger--connection-key-run-subprocess
                        sub)
           (process-put conn
                        firefox-to-emacs-native-messenger--connection-key-state
                        'dispatched)
           (puthash conn t
                    firefox-to-emacs-native-messenger--connection-registry)
           (cl-letf (((symbol-function
                       'firefox-to-emacs-native-messenger--run-signal-escalate)
                      (lambda (_s) (cl-incf escalate-calls))))
             (firefox-to-emacs-native-messenger--connection-sentinel
              conn "finished\n"))
           (should (eq 'connection-loss (gethash 'terminal-cause state)))
           (should (= 1 escalate-calls))
           (should (eq 'closing
                       (process-get
                        conn
                        firefox-to-emacs-native-messenger--connection-key-state)))
           (should-not (gethash
                        conn
                        firefox-to-emacs-native-messenger--connection-registry)))
       (when (process-live-p conn)
         (ignore-errors (signal-process (process-id conn) 'kill)))
       (when (process-live-p sub)
         (ignore-errors (signal-process (process-id sub) 'kill)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-connection-sentinel-cancels-run-cancels-timeout-timer ()
  "Run-cancellation extension cancels any pending run-timeout timer.

After the sentinel runs, the run-state's `timeout-timer' field is
cleared and the timer is no longer in the active timer list."
  :tags '(:integration :run-subprocess :slow)
  (firefox-to-emacs-native-messenger-test--with-fresh-connection-registry
   (let* ((conn (make-process
                 :name "fenm-test-conn-sentinel-cancels-timer"
                 :command (list "/bin/sh" "-c" "exec sleep 30")
                 :coding 'binary :stderr nil :noquery t))
          (sub (make-process
                :name "fenm-test-sub-sentinel-cancels-timer"
                :command (list "/bin/sh" "-c" "sleep 30")
                :coding 'binary :stderr nil :noquery t))
          (state (make-hash-table :test 'eq))
          (timer-fired nil))
     (unwind-protect
         (progn
           (puthash 'terminal-cause nil state)
           (puthash 'subprocess sub state)
           (puthash 'pgrp (process-id sub) state)
           (puthash 'timeout-timer
                    (run-at-time 0.2 nil (lambda () (setq timer-fired t)))
                    state)
           (process-put sub
                        firefox-to-emacs-native-messenger--subprocess-key-run-state
                        state)
           (process-put conn
                        firefox-to-emacs-native-messenger--connection-key-run-subprocess
                        sub)
           (process-put conn
                        firefox-to-emacs-native-messenger--connection-key-state
                        'dispatched)
           (puthash conn t
                    firefox-to-emacs-native-messenger--connection-registry)
           (cl-letf (((symbol-function
                       'firefox-to-emacs-native-messenger--run-signal-escalate)
                      (lambda (_s) nil)))
             (firefox-to-emacs-native-messenger--connection-sentinel
              conn "finished\n"))
           (should-not (gethash 'timeout-timer state))
           (sleep-for 0.3)
           (should-not timer-fired))
       (when (process-live-p conn)
         (ignore-errors (signal-process (process-id conn) 'kill)))
       (when (process-live-p sub)
         (ignore-errors (signal-process (process-id sub) 'kill)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-connection-sentinel-no-link-noop ()
  "Peer close in dispatched state with NO linked subprocess is a silent no-op.

Defensive guard: if the `run-subprocess' plist key is unset on the
connection (e.g., the run handler errored before linkage), the
sentinel must not crash and must not invoke escalate."
  :tags '(:integration :run-subprocess)
  (firefox-to-emacs-native-messenger-test--with-fresh-connection-registry
   (let ((conn (make-process
                :name "fenm-test-conn-sentinel-no-link"
                :command (list "/bin/sh" "-c" "exec sleep 30")
                :coding 'binary :stderr nil :noquery t))
         (escalate-calls 0))
     (unwind-protect
         (progn
           (process-put conn
                        firefox-to-emacs-native-messenger--connection-key-state
                        'dispatched)
           (puthash conn t
                    firefox-to-emacs-native-messenger--connection-registry)
           (cl-letf (((symbol-function
                       'firefox-to-emacs-native-messenger--run-signal-escalate)
                      (lambda (_s) (cl-incf escalate-calls))))
             (firefox-to-emacs-native-messenger--connection-sentinel
              conn "finished\n"))
           (should (= 0 escalate-calls))
           (should (eq 'closing
                       (process-get
                        conn
                        firefox-to-emacs-native-messenger--connection-key-state))))
       (when (process-live-p conn)
         (ignore-errors (signal-process (process-id conn) 'kill)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-connection-sentinel-non-dispatched-no-cancel ()
  "Peer close when prior state is NOT `dispatched' does NOT trigger run cancellation.

The run-cancellation extension only fires when a deferred-response
run is in flight."
  :tags '(:integration :run-subprocess)
  (firefox-to-emacs-native-messenger-test--with-fresh-connection-registry
   (let ((conn (make-process
                :name "fenm-test-conn-sentinel-non-dispatched"
                :command (list "/bin/sh" "-c" "exec sleep 30")
                :coding 'binary :stderr nil :noquery t))
         (escalate-calls 0))
     (unwind-protect
         (progn
           (process-put conn
                        firefox-to-emacs-native-messenger--connection-key-state
                        'reading)
           (puthash conn t
                    firefox-to-emacs-native-messenger--connection-registry)
           (cl-letf (((symbol-function
                       'firefox-to-emacs-native-messenger--run-signal-escalate)
                      (lambda (_s) (cl-incf escalate-calls))))
             (firefox-to-emacs-native-messenger--connection-sentinel
              conn "finished\n"))
           (should (= 0 escalate-calls)))
       (when (process-live-p conn)
         (ignore-errors (signal-process (process-id conn) 'kill)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-connection-sentinel-cancel-after-cause-set ()
  "If terminal-cause is already set when peer-close arrives, escalate is NOT re-invoked.

Defensive: the first terminal observer wins via CAS; the
connection sentinel's helper acknowledges the loss via log but
skips escalate."
  :tags '(:integration :run-subprocess)
  (firefox-to-emacs-native-messenger-test--with-fresh-connection-registry
   (let* ((conn (make-process
                 :name "fenm-test-conn-sentinel-cause-set"
                 :command (list "/bin/sh" "-c" "exec sleep 30")
                 :coding 'binary :stderr nil :noquery t))
          (sub (make-process
                :name "fenm-test-sub-sentinel-cause-set"
                :command (list "/bin/sh" "-c" "sleep 30")
                :coding 'binary :stderr nil :noquery t))
          (state (make-hash-table :test 'eq))
          (escalate-calls 0))
     (unwind-protect
         (progn
           (puthash 'terminal-cause 'overflow state)
           (puthash 'subprocess sub state)
           (puthash 'pgrp (process-id sub) state)
           (puthash 'timeout-timer nil state)
           (process-put sub
                        firefox-to-emacs-native-messenger--subprocess-key-run-state
                        state)
           (process-put conn
                        firefox-to-emacs-native-messenger--connection-key-run-subprocess
                        sub)
           (process-put conn
                        firefox-to-emacs-native-messenger--connection-key-state
                        'dispatched)
           (puthash conn t
                    firefox-to-emacs-native-messenger--connection-registry)
           (cl-letf (((symbol-function
                       'firefox-to-emacs-native-messenger--run-signal-escalate)
                      (lambda (_s) (cl-incf escalate-calls))))
             (firefox-to-emacs-native-messenger--connection-sentinel
              conn "finished\n"))
           (should (eq 'overflow (gethash 'terminal-cause state)))
           (should (= 0 escalate-calls)))
       (when (process-live-p conn)
         (ignore-errors (signal-process (process-id conn) 'kill)))
       (when (process-live-p sub)
         (ignore-errors (signal-process (process-id sub) 'kill)))))))

;;;; TASK-18100: run handler end-to-end tests.
;;;; The handler must launch via /bin/sh -c (Emacs makes each subprocess its own session leader), capture pgrp
;;;; (with fallback to PID), populate the run-state hash table, set up
;;;; the cross-link between connection and subprocess, transition the
;;;; connection state to `dispatched', schedule the run-timeout timer
;;;; if configured, and send any content followed by EOF to stdin
;;;; (EPIPE tolerated and logged).

(defmacro firefox-to-emacs-native-messenger-test--with-stub-connection
    (conn-var &rest body)
  "Bind CONN-VAR to a live shell-stub process suitable as a fake connection.

The process is `exec sleep 60' under /bin/sh so it sits idle until
the test completes; on body exit it is unconditionally killed."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,conn-var (make-process
                     :name "fenm-test-stub-conn"
                     :command (list "/bin/sh" "-c" "exec sleep 60")
                     :coding 'binary :stderr nil :noquery t)))
     (unwind-protect
         (progn ,@body)
       (when (process-live-p ,conn-var)
         (ignore-errors (signal-process (process-id ,conn-var) 'kill))))))

(defmacro firefox-to-emacs-native-messenger-test--with-run-handler-fixture
    (client-var run-whitelist &rest body)
  "Set up an allow-all (or supplied) run-whitelist plus a stub connection.

CLIENT-VAR is bound to a stub connection process (see
`--with-stub-connection').  The run-whitelist defcustom is
saved/restored around BODY; RUN-WHITELIST is the value used inside
the test.  Calls to `--write-response' are stubbed to a no-op so a
spuriously-firing sentinel does not write to the stub connection."
  (declare (indent 2) (debug (symbolp form body)))
  `(firefox-to-emacs-native-messenger-test--with-saved-whitelists
    (setq firefox-to-emacs-native-messenger-run-whitelist ,run-whitelist)
    (firefox-to-emacs-native-messenger-test--with-stub-connection ,client-var
      (cl-letf (((symbol-function
                  'firefox-to-emacs-native-messenger--write-response)
                 (lambda (_c _r) nil)))
        ,@body))))

(defun firefox-to-emacs-native-messenger-test--cleanup-run-state-and-subprocess
    (client)
  "Tear down a run-state and its subprocess linked from CLIENT.
Used in `unwind-protect' cleanups to ensure tests do not leak."
  (let ((sub (process-get
              client
              firefox-to-emacs-native-messenger--connection-key-run-subprocess)))
    (when (processp sub)
      (let ((state (process-get
                    sub
                    firefox-to-emacs-native-messenger--subprocess-key-run-state)))
        (when (hash-table-p state)
          (firefox-to-emacs-native-messenger--cancel-timer
           (gethash 'timeout-timer state))))
      (when (process-live-p sub)
        (ignore-errors (signal-process (process-id sub) 'kill))))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-handler-missing-command-field ()
  "Request without `command' field returns the generic missing-field error."
  :tags '(:unit)
  (firefox-to-emacs-native-messenger-test--with-saved-whitelists
   (setq firefox-to-emacs-native-messenger-run-whitelist '("*"))
   (let ((resp (firefox-to-emacs-native-messenger--run-handler
                nil '((cmd . "run")))))
     (should (equal "error" (alist-get 'cmd resp)))
     (should (equal "missing required field: command"
                    (alist-get 'error resp))))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-handler-non-string-command ()
  "Request with `command' that is null or non-string returns the generic error."
  :tags '(:unit)
  (firefox-to-emacs-native-messenger-test--with-saved-whitelists
   (setq firefox-to-emacs-native-messenger-run-whitelist '("*"))
   (let ((resp1 (firefox-to-emacs-native-messenger--run-handler
                 nil '((cmd . "run") (command))))
         (resp2 (firefox-to-emacs-native-messenger--run-handler
                 nil '((cmd . "run") (command . 42)))))
     (should (equal "error" (alist-get 'cmd resp1)))
     (should (equal "missing required field: command"
                    (alist-get 'error resp1)))
     (should (equal "error" (alist-get 'cmd resp2)))
     (should (equal "missing required field: command"
                    (alist-get 'error resp2))))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-handler-launches-subprocess ()
  "Handler launches a subprocess via /bin/sh -c COMMAND-STRING.

Asserts the subprocess exists, the run-state hash table is
populated, and the connection state transitions to `dispatched'."
  :tags '(:integration :run-subprocess)
  (firefox-to-emacs-native-messenger-test--with-run-handler-fixture
      client '("*")
    (unwind-protect
        (progn
          (firefox-to-emacs-native-messenger--run-handler
           client `((cmd . "run") (command . "true") (content . "")))
          (let ((sub (process-get
                      client
                      firefox-to-emacs-native-messenger--connection-key-run-subprocess)))
            (should (processp sub))
            (let ((state (process-get
                          sub
                          firefox-to-emacs-native-messenger--subprocess-key-run-state)))
              (should (hash-table-p state))
              (should (eq client (gethash 'connection state)))
              (should (eq sub (gethash 'subprocess state)))
              (should (equal "true" (gethash 'command-string state))))
            (should (eq 'dispatched
                        (process-get
                         client
                         firefox-to-emacs-native-messenger--connection-key-state)))))
      (firefox-to-emacs-native-messenger-test--cleanup-run-state-and-subprocess
       client))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-handler-cross-link ()
  "Handler creates bidirectional cross-link between connection and subprocess."
  :tags '(:integration :run-subprocess)
  (firefox-to-emacs-native-messenger-test--with-run-handler-fixture
      client '("*")
    (unwind-protect
        (progn
          (firefox-to-emacs-native-messenger--run-handler
           client `((cmd . "run") (command . "true") (content . "")))
          (let* ((sub (process-get
                       client
                       firefox-to-emacs-native-messenger--connection-key-run-subprocess))
                 (state (process-get
                         sub
                         firefox-to-emacs-native-messenger--subprocess-key-run-state)))
            (should (eq sub (gethash 'subprocess state)))
            (should (eq client (gethash 'connection state)))))
      (firefox-to-emacs-native-messenger-test--cleanup-run-state-and-subprocess
       client))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-handler-pgrp-captured ()
  "Handler captures pgrp via `process-attributes', falling back to PID.

Emacs creates each subprocess as its own session and process-group
leader; its PID equals its PGID.  If `process-attributes' returns
an alist with `pgrp', the handler uses that; otherwise (or if the
subprocess exited too quickly to enumerate), the handler falls
back to `process-id'."
  :tags '(:integration :run-subprocess)
  (firefox-to-emacs-native-messenger-test--with-run-handler-fixture
      client '("*")
    (unwind-protect
        (progn
          (firefox-to-emacs-native-messenger--run-handler
           client `((cmd . "run") (command . "sleep 2") (content . "")))
          (let* ((sub (process-get
                       client
                       firefox-to-emacs-native-messenger--connection-key-run-subprocess))
                 (state (process-get
                         sub
                         firefox-to-emacs-native-messenger--subprocess-key-run-state))
                 (pgrp (gethash 'pgrp state))
                 (pid (process-id sub)))
            (should (integerp pgrp))
            ;; Emacs creates each subprocess as its own session leader, so pgrp == pid.
            (should (or (= pgrp pid)
                        (= pgrp (or (alist-get 'pgrp
                                               (ignore-errors
                                                 (process-attributes pid)))
                                    pid))))))
      (firefox-to-emacs-native-messenger-test--cleanup-run-state-and-subprocess
       client))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-handler-state-fields-populated ()
  "After launch, the run-state hash table has every documented field initialized."
  :tags '(:integration :run-subprocess)
  (firefox-to-emacs-native-messenger-test--with-run-handler-fixture
      client '("*")
    (unwind-protect
        (progn
          (firefox-to-emacs-native-messenger--run-handler
           client `((cmd . "run") (command . "echo hello") (content . "in")))
          (let* ((sub (process-get
                       client
                       firefox-to-emacs-native-messenger--connection-key-run-subprocess))
                 (state (process-get
                         sub
                         firefox-to-emacs-native-messenger--subprocess-key-run-state)))
            (should (hash-table-p state))
            (should-not (gethash 'terminal-cause state))
            (should (stringp (gethash 'output-buffer state)))
            (should (integerp (gethash 'output-bytes state)))
            (should (integerp (gethash 'output-cap state)))
            (should (eq sub (gethash 'subprocess state)))
            (should (eq client (gethash 'connection state)))
            (should (equal "echo hello" (gethash 'command-string state)))
            (should (integerp (gethash 'pgrp state)))
            (should (memq (gethash 'pgrp-fallback state) '(t nil)))))
      (firefox-to-emacs-native-messenger-test--cleanup-run-state-and-subprocess
       client))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-handler-stdin-content-sent ()
  "The request's `content' is sent to subprocess stdin, then EOF closes it.

A `cat' subprocess echoes stdin to stdout; the accumulator filter
captures the result in the run-state's output-buffer."
  :tags '(:integration :run-subprocess :slow)
  (firefox-to-emacs-native-messenger-test--with-run-handler-fixture
      client '("*")
    (unwind-protect
        (progn
          (firefox-to-emacs-native-messenger--run-handler
           client `((cmd . "run") (command . "cat") (content . "input data")))
          (let* ((sub (process-get
                       client
                       firefox-to-emacs-native-messenger--connection-key-run-subprocess))
                 (state (process-get
                         sub
                         firefox-to-emacs-native-messenger--subprocess-key-run-state)))
            ;; Wait for cat to read stdin, echo it, and exit on EOF.
            (let ((deadline (+ (float-time) 2.0)))
              (while (and (process-live-p sub) (< (float-time) deadline))
                (accept-process-output sub 0.05)))
            (should-not (process-live-p sub))
            (should (equal "input data" (gethash 'output-buffer state)))))
      (firefox-to-emacs-native-messenger-test--cleanup-run-state-and-subprocess
       client))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-handler-empty-content-ok ()
  "Empty content (or absent content) does not break the launch.

The handler defaults content to the empty string when the field is
absent or null; an empty string write is a zero-byte no-op
followed by EOF."
  :tags '(:integration :run-subprocess :slow)
  (firefox-to-emacs-native-messenger-test--with-run-handler-fixture
      client '("*")
    (unwind-protect
        (progn
          (firefox-to-emacs-native-messenger--run-handler
           client `((cmd . "run") (command . "echo done")))
          (let* ((sub (process-get
                       client
                       firefox-to-emacs-native-messenger--connection-key-run-subprocess)))
            (let ((deadline (+ (float-time) 2.0)))
              (while (and (process-live-p sub) (< (float-time) deadline))
                (accept-process-output sub 0.05)))
            (should-not (process-live-p sub))))
      (firefox-to-emacs-native-messenger-test--cleanup-run-state-and-subprocess
       client))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-handler-epipe-tolerated ()
  "Subprocess that closes stdin before we write does NOT propagate EPIPE.

The handler catches the send error and logs at info level; state
is still set up correctly and the subprocess sentinel will fire
the response from the subsequent exit."
  :tags '(:integration :run-subprocess :slow)
  (firefox-to-emacs-native-messenger-test--with-run-handler-fixture
      client '("*")
    (unwind-protect
        (progn
          ;; Subprocess closes stdin then exits immediately.
          (firefox-to-emacs-native-messenger--run-handler
           client `((cmd . "run")
                    (command . "exec 0<&-; true")
                    (content . "this-data-might-EPIPE")))
          (let* ((sub (process-get
                       client
                       firefox-to-emacs-native-messenger--connection-key-run-subprocess)))
            (should (processp sub))
            ;; Wait for exit
            (let ((deadline (+ (float-time) 2.0)))
              (while (and (process-live-p sub) (< (float-time) deadline))
                (accept-process-output sub 0.05)))
            (should-not (process-live-p sub))))
      (firefox-to-emacs-native-messenger-test--cleanup-run-state-and-subprocess
       client))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-handler-default-directory-home ()
  "Subprocess inherits cwd from `default-directory' bound to ~/.

Running `pwd' should print the user's home directory (resolved
absolute path)."
  :tags '(:integration :run-subprocess :slow)
  (firefox-to-emacs-native-messenger-test--with-run-handler-fixture
      client '("*")
    (unwind-protect
        (progn
          (firefox-to-emacs-native-messenger--run-handler
           client `((cmd . "run") (command . "pwd") (content . "")))
          (let* ((sub (process-get
                       client
                       firefox-to-emacs-native-messenger--connection-key-run-subprocess))
                 (state (process-get
                         sub
                         firefox-to-emacs-native-messenger--subprocess-key-run-state)))
            (let ((deadline (+ (float-time) 2.0)))
              (while (and (process-live-p sub) (< (float-time) deadline))
                (accept-process-output sub 0.05)))
            (let* ((home (expand-file-name "~/"))
                   (captured (gethash 'output-buffer state))
                   ;; Trim trailing newline from pwd output for comparison
                   (trimmed (string-trim captured))
                   (home-trimmed (directory-file-name home)))
              ;; pwd typically prints the resolved path; allow either ~ or its
              ;; canonical form.
              (should (or (equal trimmed home-trimmed)
                          (equal trimmed home)
                          (equal (file-name-as-directory trimmed) home))))))
      (firefox-to-emacs-native-messenger-test--cleanup-run-state-and-subprocess
       client))))

;;;; TASK-18300: end-to-end run scenarios via the wire.
;;;; These tests exercise the full pipeline: listener -> filter ->
;;;; dispatcher -> run handler -> subprocess -> accumulator filter ->
;;;; subprocess sentinel -> response writer -> framed response on
;;;; the wire.  They use `send-frame' against a sandbox listener.

(defmacro firefox-to-emacs-native-messenger-test--with-run-wire-listener
    (sock-var &rest body)
  "Start a listener in a sandbox with run-whitelist `(\"*\")' for BODY.

SOCK-VAR is bound to the bound socket path.  Both the run-whitelist
and read-whitelist defcustoms are saved/restored; for tests that
need different values BODY can re-set them inside its body."
  (declare (indent 1) (debug (symbolp body)))
  (let ((cache-sym (gensym "fenm-wire-cache-"))
        (tmp-sym (gensym "fenm-wire-tmp-")))
    `(firefox-to-emacs-native-messenger-test--with-listener-sandbox
         ,cache-sym ,tmp-sym ,sock-var
       (firefox-to-emacs-native-messenger-test--with-fresh-connection-registry
        (firefox-to-emacs-native-messenger-test--with-saved-whitelists
         (setq firefox-to-emacs-native-messenger-run-whitelist '("*"))
         (setq firefox-to-emacs-native-messenger-read-whitelist '("*"))
         (firefox-to-emacs-native-messenger-start)
         ,@body)))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-wire-zero-exit-stdout ()
  "Full wire round-trip: `run' with `echo hello' returns content+code 0."
  :tags '(:integration :run-subprocess :slow)
  (firefox-to-emacs-native-messenger-test--with-run-wire-listener sock
    (let ((resp (firefox-to-emacs-native-messenger-test-send-frame
                 sock '((cmd . "run") (command . "echo hello") (content . ""))
                 10.0)))
      (should resp)
      (should (equal "run" (alist-get 'cmd resp)))
      (should (equal "echo hello" (alist-get 'command resp)))
      (should (equal "hello\n" (alist-get 'content resp)))
      (should (= 0 (alist-get 'code resp))))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-wire-nonzero-exit ()
  "Full wire round-trip: nonzero exit status surfaces in `code'."
  :tags '(:integration :run-subprocess :slow)
  (firefox-to-emacs-native-messenger-test--with-run-wire-listener sock
    (let ((resp (firefox-to-emacs-native-messenger-test-send-frame
                 sock '((cmd . "run")
                        (command . "echo err >&2; exit 7")
                        (content . ""))
                 10.0)))
      (should resp)
      (should (equal "run" (alist-get 'cmd resp)))
      (should (= 7 (alist-get 'code resp)))
      (should (string-match-p "err" (alist-get 'content resp))))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-wire-stdin-piped ()
  "Full wire round-trip: request `content' is piped to subprocess stdin."
  :tags '(:integration :run-subprocess :slow)
  (firefox-to-emacs-native-messenger-test--with-run-wire-listener sock
    (let ((resp (firefox-to-emacs-native-messenger-test-send-frame
                 sock '((cmd . "run")
                        (command . "cat")
                        (content . "pipeable bytes"))
                 10.0)))
      (should resp)
      (should (equal "run" (alist-get 'cmd resp)))
      (should (equal "pipeable bytes" (alist-get 'content resp)))
      (should (= 0 (alist-get 'code resp))))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-wire-overflow ()
  "Full wire round-trip: exceeding run-output-cap returns the overflow error."
  :tags '(:integration :run-subprocess :slow)
  (firefox-to-emacs-native-messenger-test--with-run-wire-listener sock
    (let ((firefox-to-emacs-native-messenger-run-output-cap 32)
          (firefox-to-emacs-native-messenger-run-sigint-grace 0.3)
          (firefox-to-emacs-native-messenger-run-sigterm-grace 0.3))
      (let ((resp (firefox-to-emacs-native-messenger-test-send-frame
                   sock '((cmd . "run")
                          (command . "yes overflow-bytes")
                          (content . ""))
                   10.0)))
        (should resp)
        (should (equal "error" (alist-get 'cmd resp)))
        (should (equal "response too large" (alist-get 'error resp)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-wire-timeout ()
  "Full wire round-trip: the run-timeout defcustom triggers the timeout error."
  :tags '(:integration :run-subprocess :slow)
  (firefox-to-emacs-native-messenger-test--with-run-wire-listener sock
    (let ((firefox-to-emacs-native-messenger-run-timeout 0.3)
          (firefox-to-emacs-native-messenger-run-sigint-grace 0.3)
          (firefox-to-emacs-native-messenger-run-sigterm-grace 0.3))
      (let ((resp (firefox-to-emacs-native-messenger-test-send-frame
                   sock '((cmd . "run")
                          (command . "sleep 30")
                          (content . ""))
                   10.0)))
        (should resp)
        (should (equal "error" (alist-get 'cmd resp)))
        (should (equal "run timeout exceeded" (alist-get 'error resp)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-wire-peer-close-cancellation ()
  "Peer close while subprocess running cancels via connection-loss; no response.

The test opens a client, sends a long-running command, closes the
client without reading the response, and verifies the subprocess
has been terminated within the escalation grace + small buffer."
  :tags '(:integration :run-subprocess :slow)
  (firefox-to-emacs-native-messenger-test--with-run-wire-listener sock
    (let ((firefox-to-emacs-native-messenger-run-sigint-grace 0.3)
          (firefox-to-emacs-native-messenger-run-sigterm-grace 0.3))
      (let* ((request '((cmd . "run") (command . "sleep 30") (content . "")))
             (request-bytes (encode-coding-string
                             (json-serialize request) 'utf-8 t))
             (frame (concat
                     (firefox-to-emacs-native-messenger-test--pack-length
                      (length request-bytes))
                     request-bytes))
             (client (make-network-process
                      :name "fenm-test-peer-close-client"
                      :family 'local :service sock
                      :coding '(binary . binary)
                      :noquery t)))
        (unwind-protect
            (progn
              (process-send-string client frame)
              (accept-process-output nil 0.3)
              (let* ((server-side
                      (car (firefox-to-emacs-native-messenger--connection-registry-list)))
                     (sub (and server-side
                               (process-get
                                server-side
                                firefox-to-emacs-native-messenger--connection-key-run-subprocess))))
                (should server-side)
                (should (processp sub))
                (should (process-live-p sub))
                (delete-process client)
                (let ((deadline (+ (float-time) 2.0)))
                  (while (and (process-live-p sub) (< (float-time) deadline))
                    (accept-process-output sub 0.05)))
                (should-not (process-live-p sub))))
          (when (process-live-p client)
            (delete-process client)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-wire-raw-vs-serialized-cap-gap ()
  "Output that fits the run-output-cap can still exceed the outbound cap once JSON-escaped.

A subprocess emitting N newline bytes (each escaped to two-byte
`\\n' in JSON) doubles in size on serialization.  With a
run-output-cap above N and an outbound-response-cap below 2*N, the
response writer's post-serialize replacement surfaces the
`response too large' error."
  :tags '(:integration :run-subprocess :slow)
  (firefox-to-emacs-native-messenger-test--with-run-wire-listener sock
    (let ((firefox-to-emacs-native-messenger-run-output-cap 8192)
          (firefox-to-emacs-native-messenger-outbound-response-cap 4096)
          (firefox-to-emacs-native-messenger-run-sigint-grace 0.3)
          (firefox-to-emacs-native-messenger-run-sigterm-grace 0.3))
      (let ((resp (firefox-to-emacs-native-messenger-test-send-frame
                   sock '((cmd . "run")
                          (command . "yes '' | head -c 4000")
                          (content . ""))
                   10.0)))
        (should resp)
        (should (equal "error" (alist-get 'cmd resp)))
        (should (equal "response too large" (alist-get 'error resp)))))))

;;;; TASK-18400: process-group cancellation reaches grandchildren.

(defun firefox-to-emacs-native-messenger-test--count-procs-in-pgrp (pgrp)
  "Return the count of processes whose `pgrp' matches PGRP.

Enumerates the system process table via `list-system-processes' and
filters by attributes.  Used to verify that signal-escalation has
terminated every descendant in the run subprocess's session."
  (length
   (cl-remove-if-not
    (lambda (pid)
      (let ((attrs (ignore-errors (process-attributes pid))))
        (and attrs (eq pgrp (alist-get 'pgrp attrs)))))
    (list-system-processes))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-wire-pgrp-reaches-grandchildren ()
  "Process-group cancellation reaps backgrounded grandchildren.

The command `sleep 100 & wait' forks a shell that backgrounds a
sleep then waits.  Both processes live in the same session via
the same shell session (Emacs creates each subprocess as its own session leader); SIGKILL on (- pgrp) must reach both.  After cancellation
no process in the original pgrp must survive."
  :tags '(:integration :run-subprocess :slow)
  (firefox-to-emacs-native-messenger-test--with-run-wire-listener sock
    (let ((firefox-to-emacs-native-messenger-run-timeout 0.3)
          (firefox-to-emacs-native-messenger-run-sigint-grace 0.2)
          (firefox-to-emacs-native-messenger-run-sigterm-grace 0.2))
      (let* ((request '((cmd . "run")
                        (command . "sleep 100 & wait")
                        (content . "")))
             (request-bytes (encode-coding-string
                             (json-serialize request) 'utf-8 t))
             (frame (concat
                     (firefox-to-emacs-native-messenger-test--pack-length
                      (length request-bytes))
                     request-bytes))
             (client (make-network-process
                      :name "fenm-test-grandchild-client"
                      :family 'local :service sock
                      :coding '(binary . binary)
                      :noquery t))
             (recv-buf ""))
        (set-process-filter
         client
         (lambda (_p data) (setq recv-buf (concat recv-buf data))))
        (unwind-protect
            (progn
              (process-send-string client frame)
              (accept-process-output nil 0.3)
              (let* ((server-side
                      (car (firefox-to-emacs-native-messenger--connection-registry-list)))
                     (sub (and server-side
                               (process-get
                                server-side
                                firefox-to-emacs-native-messenger--connection-key-run-subprocess)))
                     (state (and sub (process-get
                                      sub
                                      firefox-to-emacs-native-messenger--subprocess-key-run-state)))
                     (pgrp (and state (gethash 'pgrp state))))
                (should (integerp pgrp))
                (let ((deadline (+ (float-time) 4.0)))
                  (while (and (process-live-p sub) (< (float-time) deadline))
                    (accept-process-output sub 0.05)))
                (sleep-for 0.2)
                (should-not (process-live-p sub))
                (should
                 (= 0
                    (firefox-to-emacs-native-messenger-test--count-procs-in-pgrp
                     pgrp)))))
          (when (process-live-p client)
            (delete-process client)))))))

;;;; TASK-18600: `run' command-gate composition tests.
;;;; Verifies the gate fires BEFORE subprocess setup per REQ-3300 and
;;;; integrates with the capability registry: only commands matching a
;;;; whitelist entry (literal or template with `<TEMP-PATH>') are run.

(ert-deftest firefox-to-emacs-native-messenger-test-run-gate-composition-empty-rejects ()
  "Default deny: empty whitelist rejects all run requests at the wire."
  :tags '(:integration :run-subprocess)
  (firefox-to-emacs-native-messenger-test--with-listener-sandbox
      _cache _tmp sock
    (firefox-to-emacs-native-messenger-test--with-fresh-connection-registry
     (firefox-to-emacs-native-messenger-test--with-saved-whitelists
      (setq firefox-to-emacs-native-messenger-run-whitelist nil)
      (firefox-to-emacs-native-messenger-start)
      (let ((resp (firefox-to-emacs-native-messenger-test-send-frame
                   sock '((cmd . "run") (command . "echo hello") (content . ""))
                   5.0)))
        (should resp)
        (should (equal "error" (alist-get 'cmd resp)))
        (should (equal "command not in whitelist"
                       (alist-get 'error resp))))))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-gate-composition-emacsclient-temp-path ()
  "Whitelist `(\"true <TEMP-PATH>\")' allows running with a registered path.

`true' is used in place of `emacsclient' because the test
environment may not have `emacsclient' on PATH; the gate logic is
identical."
  :tags '(:integration :run-subprocess :slow)
  (firefox-to-emacs-native-messenger-test--with-listener-sandbox
      _cache _tmp sock
    (firefox-to-emacs-native-messenger-test--with-fresh-connection-registry
     (firefox-to-emacs-native-messenger-test--with-saved-whitelists
      (setq firefox-to-emacs-native-messenger-run-whitelist '("true <TEMP-PATH>"))
      (setq firefox-to-emacs-native-messenger-read-whitelist '("*"))
      (firefox-to-emacs-native-messenger-start)
      (let* ((temp-resp
              (firefox-to-emacs-native-messenger-test-send-frame
               sock '((cmd . "temp") (content . "x") (prefix . "host"))
               5.0))
             (path (alist-get 'content temp-resp))
             (run-resp
              (firefox-to-emacs-native-messenger-test-send-frame
               sock `((cmd . "run")
                      (command . ,(concat "true " path))
                      (content . ""))
               5.0)))
        (should (equal "temp" (alist-get 'cmd temp-resp)))
        (should (equal "run" (alist-get 'cmd run-resp)))
        (should (= 0 (alist-get 'code run-resp))))))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-gate-composition-rejects-non-registered-path ()
  "Whitelist `(\"true <TEMP-PATH>\")' rejects commands with non-registered paths."
  :tags '(:integration :run-subprocess)
  (firefox-to-emacs-native-messenger-test--with-listener-sandbox
      _cache _tmp sock
    (firefox-to-emacs-native-messenger-test--with-fresh-connection-registry
     (firefox-to-emacs-native-messenger-test--with-saved-whitelists
      (setq firefox-to-emacs-native-messenger-run-whitelist '("true <TEMP-PATH>"))
      (firefox-to-emacs-native-messenger-start)
      (let ((resp (firefox-to-emacs-native-messenger-test-send-frame
                   sock '((cmd . "run")
                          (command . "true /not/a/registered/path")
                          (content . ""))
                   5.0)))
        (should (equal "error" (alist-get 'cmd resp)))
        (should (equal "command not in whitelist"
                       (alist-get 'error resp))))))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-gate-composition-rm-removes-file ()
  "Whitelist `(\"rm -f '<TEMP-PATH>'\")' allows rm on a registered path and the file is gone."
  :tags '(:integration :run-subprocess :slow)
  (firefox-to-emacs-native-messenger-test--with-listener-sandbox
      _cache _tmp sock
    (firefox-to-emacs-native-messenger-test--with-fresh-connection-registry
     (firefox-to-emacs-native-messenger-test--with-saved-whitelists
      (setq firefox-to-emacs-native-messenger-run-whitelist
            '("rm -f '<TEMP-PATH>'"))
      (firefox-to-emacs-native-messenger-start)
      (let* ((temp-resp
              (firefox-to-emacs-native-messenger-test-send-frame
               sock '((cmd . "temp") (content . "y") (prefix . "host"))
               5.0))
             (path (alist-get 'content temp-resp))
             (cmd-str (format "rm -f '%s'" path))
             (run-resp
              (firefox-to-emacs-native-messenger-test-send-frame
               sock `((cmd . "run") (command . ,cmd-str) (content . ""))
               5.0)))
        (should (equal "run" (alist-get 'cmd run-resp)))
        (should (= 0 (alist-get 'code run-resp)))
        (sleep-for 0.1)
        (should-not (file-exists-p path)))))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-gate-composition-no-subprocess-on-reject ()
  "Gate rejection happens BEFORE subprocess setup; no run-subprocess link is established.

After a rejected request the connection's `run-subprocess' plist
key remains unset and no run-state hash table is attached anywhere
that the test can discover via the registry-side accepted client."
  :tags '(:integration :run-subprocess)
  (firefox-to-emacs-native-messenger-test--with-listener-sandbox
      _cache _tmp sock
    (firefox-to-emacs-native-messenger-test--with-fresh-connection-registry
     (firefox-to-emacs-native-messenger-test--with-saved-whitelists
      (setq firefox-to-emacs-native-messenger-run-whitelist nil)
      (firefox-to-emacs-native-messenger-start)
      (let* ((make-process-calls 0)
             (orig-make-process (symbol-function 'make-process)))
        (cl-letf (((symbol-function 'make-process)
                   (lambda (&rest args)
                     (cl-incf make-process-calls)
                     (apply orig-make-process args))))
          (let ((resp (firefox-to-emacs-native-messenger-test-send-frame
                       sock '((cmd . "run")
                              (command . "should-not-run")
                              (content . ""))
                       5.0)))
            (should resp)
            (should (equal "error" (alist-get 'cmd resp)))
            (should (= 0 make-process-calls)))))))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-gate-composition-prune-after-file-removed ()
  "Registry prune-on-access invalidates a previously-allowed `<TEMP-PATH>'.

After the file is deleted out-of-band, the same command-string
fails the gate because `--registry-contains-p' prunes the missing
entry, so the template's marker substring no longer matches."
  :tags '(:integration :run-subprocess :slow)
  (firefox-to-emacs-native-messenger-test--with-listener-sandbox
      _cache _tmp sock
    (firefox-to-emacs-native-messenger-test--with-fresh-connection-registry
     (firefox-to-emacs-native-messenger-test--with-saved-whitelists
      (setq firefox-to-emacs-native-messenger-run-whitelist
            '("cat <TEMP-PATH>"))
      (firefox-to-emacs-native-messenger-start)
      (let* ((temp-resp
              (firefox-to-emacs-native-messenger-test-send-frame
               sock '((cmd . "temp") (content . "marker") (prefix . "h"))
               5.0))
             (path (alist-get 'content temp-resp))
             (cmd-str (concat "cat " path))
             (run-resp1
              (firefox-to-emacs-native-messenger-test-send-frame
               sock `((cmd . "run") (command . ,cmd-str) (content . ""))
               5.0)))
        (should (file-exists-p path))
        (should (equal "run" (alist-get 'cmd run-resp1)))
        (should (equal "marker" (alist-get 'content run-resp1)))
        (delete-file path)
        (should-not (file-exists-p path))
        (let ((run-resp2
               (firefox-to-emacs-native-messenger-test-send-frame
                sock `((cmd . "run") (command . ,cmd-str) (content . ""))
                5.0)))
          (should (equal "error" (alist-get 'cmd run-resp2)))
          (should (equal "command not in whitelist"
                         (alist-get 'error run-resp2)))))))))

;;;; TASK-19000: fast-exit pgrp capture and stdin-EPIPE wire-level tests.
;;;; Verifies the `process-attributes' fallback to `process-id' for
;;;; subprocesses that exit before attributes can be enumerated, and
;;;; that EPIPE on the stdin-send step does NOT propagate (the
;;;; sentinel still delivers the response from the exit observation).

(ert-deftest firefox-to-emacs-native-messenger-test-run-wire-fast-exit-pgrp-fallback ()
  "A subprocess running `true' may exit before `process-attributes' enumerates pgrp.

The handler must use the PID fallback in that case; the response
is delivered with code 0 and empty content."
  :tags '(:integration :run-subprocess :slow)
  (firefox-to-emacs-native-messenger-test--with-run-wire-listener sock
    (let ((resp (firefox-to-emacs-native-messenger-test-send-frame
                 sock '((cmd . "run")
                        (command . "true")
                        (content . ""))
                 5.0)))
      (should resp)
      (should (equal "run" (alist-get 'cmd resp)))
      (should (equal "" (alist-get 'content resp)))
      (should (= 0 (alist-get 'code resp))))))

(ert-deftest firefox-to-emacs-native-messenger-test-run-wire-stdin-send-after-exit ()
  "Subprocess that closes stdin and exits before we send must not propagate EPIPE.

The handler catches the stdin-send error and logs at info level;
the subprocess sentinel still produces the terminal response from
the exit observation."
  :tags '(:integration :run-subprocess :slow)
  (firefox-to-emacs-native-messenger-test--with-run-wire-listener sock
    (let ((resp (firefox-to-emacs-native-messenger-test-send-frame
                 sock '((cmd . "run")
                        (command . "exec 0<&-; true")
                        (content . "this-data-might-EPIPE"))
                 5.0)))
      (should resp)
      (should (equal "run" (alist-get 'cmd resp)))
      (should (= 0 (alist-get 'code resp))))))

;;;; TASK-19100: full FIVE-cmd wire round-trip.

(ert-deftest firefox-to-emacs-native-messenger-test-five-cmd-wire-roundtrip ()
  "Sequence: version -> getconfigpath -> temp -> read -> run.

Each cmd's wire response matches its PROTOCOL.md fixture shape.
The whitelists are set to specific entries (NOT `(\"*\")') so the
gate matcher is actually exercised on the registered path."
  :tags '(:integration :sandbox :run-subprocess :slow)
  (firefox-to-emacs-native-messenger-test--with-listener-sandbox
      _cache _tmp sock
    (firefox-to-emacs-native-messenger-test--with-fresh-connection-registry
     (firefox-to-emacs-native-messenger-test--with-saved-whitelists
      (firefox-to-emacs-native-messenger-test-with-sandbox-tempdir
       rc-sandbox
       (let* ((rc-path (expand-file-name "tridactylrc" rc-sandbox))
              (firefox-to-emacs-native-messenger-rcpath-candidates
               (list rc-path)))
         (with-temp-file rc-path
           (insert "set editorcmd emacsclient %f\n"))
         (setq firefox-to-emacs-native-messenger-run-whitelist
               '("cat <TEMP-PATH>"))
         (setq firefox-to-emacs-native-messenger-read-whitelist
               '("<TEMP-PATH>"))
         (firefox-to-emacs-native-messenger-start)
         ;; 1. version
         (let ((ver (firefox-to-emacs-native-messenger-test-send-frame
                     sock '((cmd . "version")) 5.0)))
           (should (equal "version" (alist-get 'cmd ver)))
           (should (stringp (alist-get 'version ver)))
           (should (> (length (alist-get 'version ver)) 0))
           (should (= 0 (alist-get 'code ver))))
         ;; 2. getconfigpath with a sandboxed candidate that exists
         (let ((gcp (firefox-to-emacs-native-messenger-test-send-frame
                     sock '((cmd . "getconfigpath")) 5.0)))
           (should (equal "getconfigpath" (alist-get 'cmd gcp)))
           (should (equal rc-path (alist-get 'content gcp)))
           (should (= 0 (alist-get 'code gcp))))
         ;; 3. temp
         (let* ((temp-resp
                 (firefox-to-emacs-native-messenger-test-send-frame
                  sock '((cmd . "temp")
                         (content . "five-cmd-roundtrip")
                         (prefix . "host"))
                  5.0))
                (path (alist-get 'content temp-resp)))
           (should (equal "temp" (alist-get 'cmd temp-resp)))
           (should (stringp path))
           (should (file-exists-p path))
           ;; 4. read
           (let ((read-resp
                  (firefox-to-emacs-native-messenger-test-send-frame
                   sock `((cmd . "read") (file . ,path)) 5.0)))
             (should (equal "read" (alist-get 'cmd read-resp)))
             (should (equal "five-cmd-roundtrip"
                            (alist-get 'content read-resp)))
             (should (= 0 (alist-get 'code read-resp))))
           ;; 5. run
           (let ((run-resp
                  (firefox-to-emacs-native-messenger-test-send-frame
                   sock `((cmd . "run")
                          (command . ,(concat "cat " path))
                          (content . ""))
                   10.0)))
             (should (equal "run" (alist-get 'cmd run-resp)))
             (should (equal "five-cmd-roundtrip"
                            (alist-get 'content run-resp)))
             (should (= 0 (alist-get 'code run-resp)))))))))))

(provide 'firefox-to-emacs-native-messenger-tests)
;;; firefox-to-emacs-native-messenger-tests.el ends here
