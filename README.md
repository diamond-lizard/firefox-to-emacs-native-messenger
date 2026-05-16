# firefox-to-emacs-native-messenger

A pure-Emacs-Lisp WebExtensions native-messaging bridge that lets the Tridactyl Firefox extension talk to a long-running Emacs daemon.  The bridge replaces the upstream Nim-compiled `native_messenger` with a small Emacs Lisp module hosted inside the user's daemon, so Tridactyl's editor flow (and a few related commands) can be served without a separate compiled binary.

This implementation is **opinionated and narrow**.  It implements exactly six handlers from the upstream protocol, with per-handler default-deny access control on the two handlers that expose arbitrary file or command capabilities.  See [PROTOCOL.md](PROTOCOL.md) for the per-command request and response contract.

## 1. Overview

Data flow on every request:

```
Firefox (Tridactyl extension)
   v   (4-byte little-endian length prefix + UTF-8 JSON over stdio)
firefox-to-emacs-native-messenger-wrapper           (POSIX shell)
   v   (writes ~/.cache/firefox-to-emacs-native-messenger/firefox.pid then execs socat)
socat                                                (byte pipe)
   v   (over a Unix-domain socket)
~/.cache/firefox-to-emacs-native-messenger/messenger.sock
   v
Emacs daemon listener  (firefox-to-emacs-native-messenger.el)
   v   (dispatch on cmd field)
per-handler implementation: version, getconfigpath, getconfig, temp, read, run
```

Each connection carries exactly one request and produces exactly one response (or zero on connection-loss during `run` cancellation).  The bridge owns the wire protocol; the wrapper and socat are dumb plumbing.

### What it does

- `version`          — return the bridge's claimed messenger-protocol version.
- `getconfigpath`    — return the path of the first existing tridactylrc candidate.
- `getconfig`        — return the contents of the first existing tridactylrc candidate (consumed by Tridactyl's `:source` and the `TriStart -> source_quiet` startup auto-source path).
- `temp`             — write a string to a fresh tempfile under a dedicated tempfile directory and return the path.
- `read`             — read the bytes of a file the user has whitelisted.
- `run`              — synchronously execute a shell command the user has whitelisted, return its merged stdout+stderr and exit code.

### What it does not do

Every other upstream cmd (`write`, `writerc`, `mkdir`, `move`, `list_dir`, `env`, `ppid`, `run_async`, `eval`, `win_firefox_restart`) falls through to the generic "Unhandled message" error.  Tridactyl features that depend on those — notably `:installnative`, `:restart`, `:pyeval` — will not work.  See [Section 7](#7-known-limitations) for the full list.

## 2. Prerequisites

- Linux (validated on Void Linux; other Linux distributions are very likely to work but are not validated; see [Section 8](#8-porting-notes) for porting notes including macOS).
- Emacs 30.1 or newer, running as a long-lived user daemon (`emacs --daemon`).
- `socat` on `PATH`.
- GNU coreutils (`date` with `%N` nanoseconds, `mv -n`, `sha256sum`, `mkdir -p`, `ln`) for the install rules.
- A writable home directory.  The runtime cache directory is `~/.cache/firefox-to-emacs-native-messenger/` at mode 0700; the bridge creates it if absent and refuses to start if it exists at a different mode.
- Firefox with Tridactyl installed.

This project targets little-endian architectures; the length-prefix codec is wired explicitly little-endian, so x86_64 and Apple Silicon both work in principle.  Big-endian platforms are out of scope.  Windows is out of scope; macOS is not validated but likely portable (see [Section 8](#8-porting-notes)).  Multi-profile Firefox is out of scope.

## 3. Install Order

The first-time install is a strict two-step sequence: install the support files and start the listener, then activate the manifest only after a smoke test confirms the listener works.  The convenience `make install` target is for repeat / known-good installs only; do not use it on a fresh install.

### 3.1 Clone the repository

```sh
git clone <repository-url> ~/compilation/own/emacs/firefox-to-emacs-native-messenger
cd ~/compilation/own/emacs/firefox-to-emacs-native-messenger
```

The project must live at a stable absolute path because the elisp and wrapper symlinks created in 3.2 point at the project's source files.

### 3.2 Install support files

```sh
make install-support
```

This creates:

- `~/.cache/firefox-to-emacs-native-messenger/` at mode 0700 (runtime cache directory).
- `~/.emacs.d/soma/packages/firefox-to-emacs-native-messenger.el` — a symlink to the project's `firefox-to-emacs-native-messenger.el` so the daemon can load the module.
- `~/bin/firefox-to-emacs-native-messenger-wrapper` — a symlink to the project's wrapper script so Firefox can launch it from `PATH`.

If a symlink already points at the project's source file the rule is a no-op.  A foreign symlink is replaced.  A regular file at the symlink path aborts the rule rather than silently clobbering local data.

### 3.3 Load (or reload) the Emacs daemon

If your daemon is already running, restart it so it picks up the new symlink, or evaluate `(load-file "~/.emacs.d/soma/packages/firefox-to-emacs-native-messenger.el")` in the running daemon.  Confirm the feature loaded:

```elisp
(featurep 'firefox-to-emacs-native-messenger)  ; -> t
```

Make sure your init file loads or requires `firefox-to-emacs-native-messenger` if you want the feature to be available without manual `load-file` calls.

### 3.4 Configure whitelists

The two gated handlers (`run` and `read`) default to `nil`, which means deny-all.  The bridge will accept connections and dispatch requests but every gated request will return the rejection error until you configure whitelists.  See [Section 6](#6-whitelist-configuration-walkthrough) for the canonical first-time-setup walkthrough.

### 3.5 Start the listener

```
M-x firefox-to-emacs-native-messenger-start
```

This binds the Unix-domain socket at `~/.cache/firefox-to-emacs-native-messenger/messenger.sock` and runs the listener-start whitelist-validation sweep.  Any malformed whitelist defcustom aborts the start with a clear error; correct it via `M-x customize-group RET firefox-to-emacs-native-messenger RET` or `setq` and try again.

### 3.6 Smoke-test before activation

Before activating the Firefox manifest, confirm the bridge works end-to-end by sending a framed request through the wrapper script:

```sh
printf '\x10\x00\x00\x00{"cmd":"version"}' | ~/bin/firefox-to-emacs-native-messenger-wrapper
```

You should see a framed JSON response on stdout containing the version string and `"code":0`.  If you do not, see [Section 10](#10-troubleshooting).

### 3.7 Activate the Firefox manifest

```sh
make activate
```

This creates a symlink at `~/.mozilla/native-messaging-hosts/tridactyl.json` pointing at the project's manifest.  If an existing manifest is present:

- A symlink already pointing at the project's manifest: no-op.
- A regular file with content matching the project's manifest: replaced by a symlink (no backup; bytes are identical).
- A regular file with different content: moved to a timestamped backup, then replaced by a symlink.
- A foreign symlink: moved to a timestamped backup, then replaced by a symlink.

The backup naming convention is `tridactyl.json.<UTC-timestamp>`, with a `.<PID>` retry suffix on the rare collision.

### 3.8 Reload Tridactyl

Restart Firefox, or run `:native` in the Tridactyl command bar.  `:native` should report the version string (matching what your smoke test in 3.6 returned).

### 3.9 Confirm the editor flow

Open a textarea in any web page and trigger Tridactyl's `:editor` excmd.  Your configured editor should open with the textarea's content; saving the file should update the textarea.  See [Section 6.4](#64-worked-example-the-editor-flow) for the matching whitelist configuration.

## 4. Operations

### 4.1 Start and stop

```
M-x firefox-to-emacs-native-messenger-start
M-x firefox-to-emacs-native-messenger-stop
```

Start fails loudly if a listener is already recorded, if the cache directory has the wrong mode, or if any whitelist defcustom is malformed.  Stop is a silent no-op when no listener is recorded.  Stop unlinks the socket file only if it still exists as a socket and matches the configured path, never as a regular file.

### 4.2 Inspecting the log

The bridge logs connection-level events to a dedicated buffer.  Open it with:

```
C-x b *firefox-to-emacs-native-messenger-log* RET
```

The log buffer name is configurable via the `firefox-to-emacs-native-messenger-log-buffer-name` defcustom; the log level via `firefox-to-emacs-native-messenger-log-level` (one of `debug`, `info`, `warn`, `error`; default `info`).  The logger never raises; failures inside the logger become silent no-ops.

### 4.3 Inspecting the capability registry

The capability registry is an in-memory hash table that records paths returned by the `temp` handler so subsequent `read` / `run` requests can refer to them via the `<TEMP-PATH>` marker (see [Section 6](#6-whitelist-configuration-walkthrough)).  Inspect its current contents from the daemon:

```elisp
(hash-table-count firefox-to-emacs-native-messenger--capability-registry)
```

Or list the registered paths:

```elisp
(let (paths)
  (maphash (lambda (k _v) (push k paths))
           firefox-to-emacs-native-messenger--capability-registry)
  paths)
```

The registry is cleared on listener start (after the tempfile-directory sweep) and on listener stop.  Paths are pruned on access when the file is missing, when the file's identity (device, inode, owner UID) no longer matches the recorded values, or when the file is no longer a regular file.

### 4.4 Customization group

Every behaviorally-tunable knob lives under one customization group:

```
M-x customize-group RET firefox-to-emacs-native-messenger RET
```

The frequently-edited entries are the two whitelists (`run-whitelist`, `read-whitelist`); the rest are caps and timeouts that rarely need adjustment.  Changes via Customize take immediate effect; whitelists are re-read on every gate evaluation, so there is no need to restart the listener after edits.

## 5. Rollback

### 5.1 Disable the bridge temporarily

Stop the listener:

```
M-x firefox-to-emacs-native-messenger-stop
```

Tridactyl requests will fail with Firefox's "no native messenger" error until you start it again.  The Firefox manifest remains in place; activation status is unchanged.

### 5.2 Restore the previous messenger

If `make activate` moved your previous manifest aside, the backup lives at `~/.mozilla/native-messaging-hosts/tridactyl.json.<UTC-timestamp>` (with an optional `.<PID>` retry suffix).  To restore it:

```sh
ls ~/.mozilla/native-messaging-hosts/tridactyl.json.* | tail -1   # find the most recent backup
rm  ~/.mozilla/native-messaging-hosts/tridactyl.json              # remove the project symlink
mv  ~/.mozilla/native-messaging-hosts/tridactyl.json.<UTC-timestamp> \
    ~/.mozilla/native-messaging-hosts/tridactyl.json
```

If no manifest existed before activation, simply remove the symlink:

```sh
rm ~/.mozilla/native-messaging-hosts/tridactyl.json
```

Restart Firefox so Tridactyl picks up the new manifest.

### 5.3 Re-enable the bridge

Re-run the activate target:

```sh
make activate
```

This handles the four branches (symlink already correct, regular file with matching content, regular file with different content, foreign symlink) so it is safe to run repeatedly.

## 6. Whitelist Configuration Walkthrough

This is the canonical first-time-setup section.  Read it in full before configuring whitelists.

### 6.1 Defcustom names and defaults

Two whitelist defcustoms gate the two non-trivial handlers:

| Defcustom                                              | Gated handler | Default value |
|--------------------------------------------------------|---------------|---------------|
| `firefox-to-emacs-native-messenger-run-whitelist`      | `run`         | `nil`         |
| `firefox-to-emacs-native-messenger-read-whitelist`     | `read`        | `nil`         |

Both default to `nil`, which means **deny everything**.  Until you configure them, every `run` request returns "command not in whitelist" and every `read` request returns "path not in whitelist".

The remaining four in-scope handlers (`version`, `getconfigpath`, `getconfig`, `temp`) are ungated and need no configuration; their security boundaries are documented in [Section 9](#9-security).

### 6.2 Whitelist value shapes

Each whitelist accepts one of three shapes:

1. **`nil` (deny-all).**  The default.  All requests to the gated handler are rejected.
2. **`'("*")` (allow-all sentinel).**  A one-element list containing exactly the string `"*"`.  All requests are accepted.  Use sparingly; this disables the gate entirely.
3. **A list of entry strings.**  Each entry is matched against the candidate request per the rules below.

A list that contains `"*"` mixed with any other element is rejected at validation: that combination is almost always a mistake (most users mean either deny-all or allow-all, not a logically incoherent mixture).

### 6.3 The `<TEMP-PATH>` marker

The `<TEMP-PATH>` marker authorizes paths previously returned by the `temp` handler in the current listener lifetime.  When the bridge's matcher encounters `<TEMP-PATH>` in an entry, it extracts the corresponding substring from the candidate command-string or path and consults the in-memory capability registry: the marker only matches a substring that the bridge itself created via `temp`.

This is the only marker defined in the current bridge version.  Other `<...>`-shaped tokens are rejected at validation as typo guards.

#### Path-whitelist entry forms (`read-whitelist`)

Each entry is one of:

- The literal token `<TEMP-PATH>` — matches any path in the capability registry.
- A literal absolute path — matches the candidate request's expanded path byte-for-byte.
- A glob path — matches per fnmatch semantics (full-string anchored): `*` matches any sequence of non-`/`; `**` matches anything including `/`; `?` matches one character.

Relative paths are rejected at validation.

#### Command-whitelist entry forms (`run-whitelist`)

Each entry is a template string containing zero or more `<TEMP-PATH>` markers.  Literal segments between markers are matched verbatim; marker positions must equal substrings in the capability registry.  The matcher anchors the first literal segment as a prefix, the last as a suffix, and uses first-occurrence search for interior literals — see [Section 6.6](#66-advanced-multi-marker-templates) for the deterministic algorithm and a worked example.

Adjacent `<TEMP-PATH>` markers (with no literal between them) are rejected at validation; that pattern can never produce a meaningful match.

### 6.4 Worked example: the editor flow

Tridactyl's `:editor` excmd needs:

1. `temp` to write the textarea content to a fresh tempfile.
2. `run` to launch the user's configured editor on that tempfile.
3. `read` to read the saved file back into the textarea.

The minimal whitelist configuration is:

```elisp
(setq firefox-to-emacs-native-messenger-run-whitelist
      '("emacs-in-new-xterm <TEMP-PATH>"))
(setq firefox-to-emacs-native-messenger-read-whitelist
      '("<TEMP-PATH>"))
```

If your editor command is different, substitute the actual command-line.  Set Tridactyl's `editorcmd` to the same command:

```
:set editorcmd emacs-in-new-xterm %f
```

The `<TEMP-PATH>` markers ensure that:

- `run` only allows `emacs-in-new-xterm <some path>` where `<some path>` was previously returned by `temp`.
- `read` only allows reading paths previously returned by `temp`.

This means an attacker who compromises the Tridactyl extension cannot use the bridge to launch arbitrary commands or read arbitrary files; they can only invoke the editor on tempfiles the bridge itself created.

Note on `editorcmd = "auto"`: Tridactyl's auto-detection probes editors with `which X` invocations via `run`.  Those `which` probes do not match the whitelist entry above, so auto mode does not work under the canonical configuration.  Set `editorcmd` explicitly.

#### Optional: the `editor_rm` alias

If you want Tridactyl's `editor_rm` alias (delete the tempfile after closing the editor), add the rm command-template to the run-whitelist:

```elisp
(setq firefox-to-emacs-native-messenger-run-whitelist
      '("emacs-in-new-xterm <TEMP-PATH>"
        "rm -f '<TEMP-PATH>'"))
```

### 6.5 No-restart reload semantics

The whitelist defcustoms are read fresh on every gate evaluation.  Edits via `setq`, `customize-set-variable`, or the Customize UI take immediate effect on the next request; the listener does not need to be restarted.

Defcustom edits are validated at three sites:

1. The defcustom's `:set` slot — invoked by Customize and by `customize-set-variable`.
2. A variable watcher — invoked on raw `setq` assignments that bypass the `:set` slot.
3. Per-gate-check at request time.

A malformed value is rejected at the first site that fires.  The variable's old value is preserved; you do not need to restart the listener to recover from a typo.

### 6.6 Advanced: multi-marker templates

The command-whitelist matcher splits each entry on `<TEMP-PATH>` into literal segments and marker positions, then matches deterministically (no backtracking):

1. The first literal segment is anchored as a prefix.
2. Interior literal segments are matched at the FIRST occurrence at or after the current cursor.
3. The last literal segment is anchored as a suffix.
4. Every extracted marker substring is checked against the capability registry.

Worked example.  Entry `"cp <TEMP-PATH> <TEMP-PATH>"` (two markers, interior literal `" "`):

- Candidate `cp /a /b` matches: extracted markers are `/a` and `/b`; both must be registered.
- Candidate `cp /a /b /c` is matched as: first marker = `/a`, interior literal `" "` matches at first occurrence, second marker = `/b /c`.  This rejects unless `/b /c` is a registered path (which it never is, since registered paths come from `make-temp-file` and contain no embedded spaces).

The first-occurrence rule is deliberate: users who need a different match for the same candidate add a separate, less ambiguous whitelist entry that explicitly covers the case they want, rather than encoding ambiguity in a single entry and relying on backtracking.  For example, to allow `cp <TEMP-PATH> <TEMP-PATH> <TEMP-PATH>` (three markers, two interior literals), write that entry explicitly; do not expect a two-marker entry to match it.

### 6.7 Customize-UI alternative

If you prefer a graphical interface to `setq`:

```
M-x customize-group RET firefox-to-emacs-native-messenger RET
```

Find `firefox-to-emacs-native-messenger-run-whitelist` and `firefox-to-emacs-native-messenger-read-whitelist`; each presents a "list of strings" widget you can edit, save, and apply.

### 6.8 `:source` semantics under v2.2

- Bare `:source` uses the ungated `getconfig` handler and requires no whitelist entry.
- `:source /explicit/path` uses the gated `read` handler and requires the path (or a glob covering it) in `firefox-to-emacs-native-messenger-read-whitelist`.

If you frequently source specific files, add their paths (or a covering glob) to the read-whitelist.

## 7. Known Limitations

- **Validated on Linux only.**  Only Void Linux is validated.  Other Linux distributions are likely to work as-is.  macOS is likely to work with a small set of patches documented in [Section 8](#8-porting-notes).  Windows is out of scope.
- **x86_64 and Apple Silicon supported in principle.**  The little-endian length-prefix codec is wired explicitly; both x86_64 and Apple Silicon are little-endian and would interoperate.  Big-endian platforms are out of scope.
- **Six handlers only.**  Implemented: `version`, `getconfigpath`, `getconfig`, `temp`, `read`, `run`.  Other Tridactyl features break with "Unhandled message" — notably `:restart`, `:installnative`, `:pyeval`, and any flow that depends on `write`, `writerc`, `mkdir`, `move`, `list_dir`, `env`, `ppid`, `run_async`, or `eval`.
- **`:viewconfig --user`** is independent of this bridge: it dumps in-memory `config.USERCONFIG` and does not consume `getconfigpath`.  It works regardless of bridge state.
- **Default-deny whitelists.**  `run` and `read` require user configuration before they work.  `version`, `getconfigpath`, `getconfig`, and `temp` are ungated and work out of the box.
- **`:source` semantics.**  Bare `:source` works via the ungated `getconfig` handler and requires no whitelist entry.  `:source /explicit/path` uses the gated `read` handler and requires the path (or a glob covering it) in `firefox-to-emacs-native-messenger-read-whitelist`.
- **`editorcmd = "auto"` is not supported** under the canonical whitelist configuration.  Set `editorcmd` explicitly per [Section 6.4](#64-worked-example-the-editor-flow).
- **Capability registry does not survive listener restart.**  Paths returned by `temp` are tracked in memory only; restarting the listener clears the registry, after which `<TEMP-PATH>`-marker matches against previously-issued paths will fail.
- **Concurrent multi-profile Firefox is out of scope.**  The bridge assumes a single user-managed daemon and a single Firefox profile.

## 8. Porting Notes

Only Linux (specifically Void Linux) is validated.  The codebase is mostly portable, but several integration points are Linux-specific.  This section documents what would have to change for each non-Linux target.  Patches welcome.

### 8.1 macOS

Likely to work with the changes below.  Not validated.

**Portable as-is:**

- The Emacs Lisp bridge module.  It uses only Emacs built-ins, Unix-domain sockets, and `make-process` / `make-network-process`, all cross-platform on Emacs 30.1.
- The POSIX-shell wrapper.  Mac's `/bin/sh` is bash and the script is POSIX-clean.
- `socat`.  Available via `brew install socat`.
- The tempfile directory under `/tmp`.  Mac's `/tmp` (symlink to `/private/tmp`) behaves the same way.
- Process-group cancellation in `run`.  Emacs's `child_setup` calls `setsid` on macOS subprocesses, matching the Linux behavior the cancellation logic depends on.

**Would need changes:**

1. **Firefox manifest path.**  macOS Firefox looks for native-messaging manifests under `~/Library/Application Support/Mozilla/NativeMessagingHosts/`, not under `~/.mozilla/native-messaging-hosts/`.  The Makefile's `MANIFEST_TARGET` hardcodes the Linux path; an OS-detection branch is needed for `make activate` to install the manifest where macOS Firefox will find it.
2. **GNU coreutils dependencies in the Makefile.**  The activate rule's timestamped-backup naming uses `date %N` (nanoseconds) and its content-comparison uses `sha256sum`.  BSD `date` on macOS does not support `%N`; macOS ships `shasum -a 256` instead of `sha256sum`.  Either install GNU coreutils via `brew install coreutils` and call `gdate` / `gsha256sum`, or shim the two calls with a platform-detection wrapper.
3. **Cache directory convention.**  The runtime cache lives at `~/.cache/firefox-to-emacs-native-messenger/`.  This path works on macOS but is not the Mac convention (`~/Library/Caches/...`).  No code change is required; the README's cache-path references would be more idiomatic if they branched on platform.

**Apple Silicon (M1/M2/...):**  The little-endian length-prefix codec works unchanged.  Apple Silicon is little-endian, matching x86_64 byte order on this point.

### 8.2 Other Linux distributions

Likely to work as-is on any modern glibc-based or musl-based distribution.  The bridge relies on standard POSIX semantics for Unix-domain sockets, `setsid`, `signal-process` to a process group, and basic shell utilities.  The Makefile assumes GNU coreutils; distributions whose default coreutils are BusyBox or another non-GNU implementation may need shims for `date %N` and `sha256sum`, the same patches as for macOS.

### 8.3 Windows

Out of scope and unlikely to work without substantial rework.  Firefox on Windows uses a registry-based manifest discovery mechanism, not a filesystem path; `socat` is not idiomatic; the POSIX-shell wrapper would need a different launcher.

## 9. Security

### 9.1 Stance

This bridge tightens the threat model relative to the upstream Nim messenger: upstream's `run` handler is unconditional shell execution; this bridge gates `run` behind a user-authored whitelist.  A compromised Tridactyl extension can no longer run arbitrary shell commands through the bridge.

**The whitelist is not a sandbox.**  It reduces blast radius by restricting WHICH commands can be invoked.  Once a command is invoked, it runs with the full privileges of the Emacs daemon's user.  A whitelisted program that itself invokes other programs (an editor that can shell out, a viewer that can open URLs, etc.) extends the trust boundary to whatever those downstream programs can do.

When you author a whitelist entry, ask: "If an attacker controls the request fields and the corresponding `<TEMP-PATH>` contents, what's the worst they can do?"  If the answer is "open my editor with arbitrary text," that's probably acceptable.  If it's "execute arbitrary code," reconsider.

### 9.2 Forward-compatible sandboxing

The bridge does not sandbox subprocesses itself; sandboxing belongs in a layer the user can compose.  Concretely: instead of whitelisting `emacsclient <TEMP-PATH>`, you can whitelist `firejail --net=none emacsclient <TEMP-PATH>` (or `bwrap`, `nsjail`, etc.) so the editor runs under the sandbox's restrictions.  The whitelist matcher treats the sandbox prefix as opaque literal text and forwards everything past it to the editor.

### 9.3 Other security properties

- TRAMP and remote paths are rejected in any path-accepting handler before any I/O is attempted.
- Inbound frames are capped (default 10 mebibytes); oversized frames cause a silent connection close with a log entry.
- Outbound response payloads are capped (default 768 kibibytes); oversized responses are replaced with a generic error frame.
- Run subprocess captured output is capped (default 512 kibibytes).
- The runtime cache directory and the tempfile directory must exist at exactly mode 0700; the listener refuses to start otherwise.
- The `getconfig` handler enforces a file-UID equality check: candidate rc files whose UID differs from the daemon's UID are treated as IOError without disclosing content.

## 10. Troubleshooting

### 10.1 Smoke test fails with no response

If the smoke test in [Section 3.6](#36-smoke-test-before-activation) produces no output:

- Confirm the listener is running: `M-x list-processes RET` should list `firefox-to-emacs-native-messenger-listener`.
- Confirm the socket exists: `ls -la ~/.cache/firefox-to-emacs-native-messenger/messenger.sock` should show a socket file (`s` in the first column) at mode 0700.
- Confirm `socat` is on `PATH` (`command -v socat`).
- Check the bridge log buffer for connection accept events.

### 10.2 Listener refuses to start

Common causes:

- The cache directory exists but at the wrong mode.  Fix: `chmod 0700 ~/.cache/firefox-to-emacs-native-messenger/`.
- A whitelist defcustom contains a malformed value.  The error message identifies the violation; correct the value via `customize-set-variable` or `setq` and try again.
- A stale socket file exists from a crashed previous listener.  The listener probes for a live peer and unlinks stale sockets automatically; if it refuses anyway, the path may have become a regular file or symlink (which the listener refuses to unlink for safety).  Remove the file manually only after confirming it is yours and not a still-running listener.

### 10.3 Tridactyl reports "no native messenger"

- Confirm `~/.mozilla/native-messaging-hosts/tridactyl.json` exists and points at this project's manifest: `readlink ~/.mozilla/native-messaging-hosts/tridactyl.json`.
- Confirm the wrapper at `~/bin/firefox-to-emacs-native-messenger-wrapper` is on Firefox's effective `PATH` (Firefox restricts the PATH it consults for native-messaging hosts; `~/bin` is typically included via the user's login shell environment, but headless or sandboxed Firefox processes may not see it).
- Restart Firefox after activation; Firefox caches its manifest list.

### 10.4 Run requests are rejected even though the whitelist looks correct

- Confirm the request matches the whitelist entry byte-for-byte, including spaces and quotes.  Tridactyl's editor flow includes single quotes around the path in some commands.
- Confirm the `<TEMP-PATH>` substring is in the capability registry: a previous `temp` call must have produced that exact path within the current listener lifetime.  If the listener restarted between the `temp` call and the `run` call, the path is no longer registered.
- Increase the log level to `debug` temporarily to see gate-check details: `(setq firefox-to-emacs-native-messenger-log-level 'debug)`.

## 11. References

- [PROTOCOL.md](PROTOCOL.md) — the per-command request and response contract.
- Mozilla WebExtensions Native Messaging specification: https://developer.mozilla.org/en-US/docs/Mozilla/Add-ons/WebExtensions/Native_messaging
- Upstream Tridactyl native messenger: https://github.com/tridactyl/native_messenger
- Upstream Tridactyl extension: https://github.com/tridactyl/tridactyl
