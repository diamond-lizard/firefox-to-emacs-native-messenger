# PROTOCOL.md — Firefox-to-Emacs Native Messenger Wire Contract

This document is the single authoritative specification of the per-command request and response contract implemented by the v2 firefox-to-emacs-native-messenger bridge; no other document overrides it on per-command shapes.

The v2 bridge implements EXACTLY SIX handlers from upstream Tridactyl's native-messaging protocol (v2.2): `version`, `getconfigpath`, `getconfig`, `temp`, `read`, `run`. All other commands fall through to the dispatcher's generic error response (`cmd = "error"`, `error = "Unhandled message"`). The wire protocol itself (4-byte little-endian length prefix + UTF-8 JSON) is unchanged from Mozilla's WebExtensions native-messaging specification.

Sources audited at the time of writing:

- Upstream `tridactyl/native_messenger` `src/native_main.nim`.
- Upstream `tridactyl/tridactyl` `src/lib/native.ts`.
- Upstream `tridactyl/native_messenger` `gen_native_message.py`.
- Upstream `tridactyl/native_messenger` `tridactyl.json` manifest.

## 1. Command-Set Enumeration

Every cmd that Tridactyl's `src/lib/native.ts` is capable of issuing, cross-referenced with `native_main.nim`'s `handleMessage` dispatcher branches. The "Implemented in v2" column records whether the v2 bridge handles the request natively. Commands marked "no" fall through to the dispatcher's "Unhandled message" generic error response without any further side effects.

| cmd | Implemented in v2 | Notes |
|---|---|---|
| `version` | yes | Returns VERSION string + `code=0`. JS wrapper: `getNativeMessengerVersion()`. |
| `getconfigpath` | yes | Returns first existing candidate config path in the `content` field. JS wrapper: `getrcpath()`. |
| `getconfig` | yes | Returns file contents of the first existing rc candidate (same walker as `getconfigpath`) with `content` + `code=0`; empty-result is `code=1` (no `content` field); IOError-or-foreign-UID is `code=2` per Section 8.24.4 of the implementation plan; oversize is the generic-error shape. JS wrapper: `getrc()`. v2.2 added; supports Tridactyl `:source` and `TriStart -> source_quiet` startup auto-source. |
| `temp` | yes | Writes content to a tempfile under the dedicated tempfile directory; returns path in `content`. |
| `read` | yes | Reads file content with bounded size cap. |
| `run` | yes | Synchronous shell-out; returns merged stdout+stderr + exit code. |
| `write` | no | Falls through. |
| `writerc` | no | Falls through. |
| `mkdir` | no | Falls through. |
| `move` | no | Falls through. |
| `list_dir` | no | Falls through. |
| `env` | no | Falls through. |
| `ppid` | no | Falls through. v2 explicitly omits the `ppid` handler. The wrapper still writes the side-channel PID file at `~/.cache/firefox-to-emacs-native-messenger/firefox.pid` for forward compatibility, but no v2 handler reads it. |
| `run_async` | no | Falls through. Breaks Tridactyl's `:installnative` flow. |
| `eval` | no | Falls through. v2 strengthens upstream's `TODO: NOT IMPLEMENTED` stderr-log to a hard refusal via the dispatcher's "Unhandled message" path. |
| `win_firefox_restart` | no | Windows-only upstream; out of v2 scope (v2 supports Linux only). |

## 2. Request Shapes (Implemented Commands)

All inbound JSON objects MUST include a `cmd` field whose value selects the handler. Additional fields are handler-specific. The wire-level minimum is therefore a single-field object `{"cmd": "<name>"}`. Field-presence and null-vs-absence semantics below describe how the v2 handler reacts when fields are absent, explicitly null, or empty.

### 2.1 `version` Request

```json
{ "cmd": "version" }
```

- Required: `cmd` (string, value `"version"`).
- Optional: none.
- Null vs absence: irrelevant; no other fields are inspected.
- JS wrapper: `Native.getNativeMessengerVersion()` calls `sendNativeMsg("version", {}, quiet)` (`src/lib/native.ts:96`).

### 2.2 `getconfigpath` Request

```json
{ "cmd": "getconfigpath" }
```

- Required: `cmd` (string, value `"getconfigpath"`).
- Optional: none.
- Null vs absence: irrelevant.
- JS wrapper: `Native.getrcpath(separator = "auto")` calls `sendNativeMsg("getconfigpath", {})` (`src/lib/native.ts:65`). The wrapper's `separator` argument is post-processing applied to the response's `content` field and does NOT appear in the request.

### 2.3 `temp` Request

```json
{ "cmd": "temp", "content": "<string>", "prefix": "<string>" }
```

- Required: `cmd` (string, value `"temp"`), `content` (string; written verbatim to the new tempfile).
- Optional: `prefix` (string; default empty string `""` if absent or null per upstream `msg.prefix.get("")`).
- Null vs absence (`prefix`): both treated identically as empty-string default.
- Null vs absence (`content`): in upstream, absent or null causes `Option.get()` to raise, which crashes the process. In v2 the handler returns the generic error response `{"cmd": "error", "error": "missing required field: content"}` without crashing the listener.
- JS wrapper: `Native.temp(content, prefix)` calls `sendNativeMsg("temp", {content, prefix})` (`src/lib/native.ts:516`). The wrapper is called by the editor flow with `prefix = document.location.hostname`.
- Prefix sanitization happens inside the handler per upstream `sanitiseFilename` (lowercase, retain ASCII alphanumerics and `.`, collapse `..` to `.`); the request is NOT validated against the sanitization rules.

### 2.4 `read` Request

```json
{ "cmd": "read", "file": "<string>" }
```

- Required: `cmd` (string, value `"read"`), `file` (string).
- Optional: none.
- `file` syntax: absolute paths, `~`-prefixed paths, and paths containing `$VAR` env-var references are all accepted by upstream's `expandTilde(expandVars(file))`. v2 reproduces this via its path-expansion helper (described in Section 14).
- TRAMP / remote paths: v2 REJECTS BEFORE any I/O is attempted. Upstream applies tilde and env-var expansion then calls `open` directly with no remote-path check.
- Null vs absence (`file`): both treated as missing-required-field; v2 returns the generic error response.
- JS wrapper: `Native.read(file)` calls `sendNativeMsg("read", {file})` (`src/lib/native.ts:489`).

### 2.5 `run` Request

```json
{ "cmd": "run", "command": "<string>", "content": "<string>" }
```

- Required: `cmd` (string, value `"run"`), `command` (string; the shell command-string passed to `/bin/sh -c`).
- Optional: `content` (string). v2 ALWAYS writes the content (or, if `content` is absent or null, the empty string) to the subprocess's stdin and then closes stdin. The subprocess therefore observes its stdin as a (possibly empty) input stream followed by EOF, regardless of whether `content` was absent, null, or `""`. Stdin-write errors (e.g., EPIPE when the subprocess closed stdin or already exited) are logged at info level and do NOT propagate to the response.
- Null vs absence (`content`): semantically equivalent in v2; both are normalized to the empty string before stdin-write. This is a deliberate divergence from upstream `native_main.nim:170-175`: upstream's `if msg.content.isSome` branch means absent content leaves stdin UNTOUCHED — the subprocess's stdin remains as an open empty pipe, and a subprocess that reads stdin (e.g., `cat`) would BLOCK indefinitely waiting for input. v2 guarantees prompt EOF instead. In practice Tridactyl's JS wrapper always sends `content` (defaulting to `""`), so this divergence is observable only via hand-crafted requests; documented here for completeness.
- Null vs absence (`command`): both treated as missing-required-field; v2 returns the generic error response.
- JS wrapper: `Native.run(command, content = "")` calls `sendNativeMsg("run", {command, content})` (`src/lib/native.ts:558`). The wrapper ALWAYS sends a `content` field, defaulting to empty string `""`. In practice v2 will therefore typically observe `content = ""` from Tridactyl (not absent), which is treated identically to "no stdin sent" since empty-string stdin writes zero bytes before EOF.

### 2.6 `getconfig` Request

```json
{ "cmd": "getconfig" }
```

- Required: `cmd` (string, value `"getconfig"`).
- Optional: none.
- Null vs absence: irrelevant; no other fields are inspected.
- Extra fields (e.g., `file`, `content`, anything else) are SILENTLY IGNORED. The handler MUST NOT echo them in the response and MUST NOT raise an error.
- JS wrapper: `Native.getrc()` calls `sendNativeMsg("getconfig", {})` (`src/lib/native.ts`; corresponds to upstream's only consumer site for this cmd). v2.2 added; Tridactyl's `:source` excmd and the `TriStart -> source_quiet` startup auto-source path consume the response.
- Audit cite: upstream `native_main.nim:140` case statement; `getconfig` branch at lines 145-154 reads only `cmd`.

## 3. Response Field Membership and Presence Rules

This section enumerates which fields can appear in each response and when each field is present, conditionally present, or always omitted.

### 3.1 Response Field Universe

Upstream `native_main.nim` defines a single `MessageResp` record with the following fields (`src/native_main.nim:28-35`):

| Field | Type | Used by impl-in-v2 cmds? |
|---|---|---|
| `cmd` | string | yes (all) |
| `version` | string | yes (`version` only) |
| `error` | string | only in error responses |
| `sep` | string | NO (`list_dir` only; not in v2 scope) |
| `content` | optional string | yes (`getconfigpath` success, `temp`, `read`, `run`) |
| `command` | optional string | yes (`run` only) |
| `files` | string sequence | NO (`list_dir` only) |
| `isDir` | boolean | NO (`list_dir` only) |
| `code` | optional integer | yes (all six handlers in success cases) |

For the five in-scope handlers, the only response fields that ever appear are: `cmd`, `version`, `content`, `command`, `code`, and (only in error responses) `error`.

### 3.2 Per-cmd Success Response Shapes

For each in-scope handler, the canonical success response shape. Concrete fixtures (with placeholders for non-deterministic values) appear in Appendix B.

#### 3.2.1 `version` success

```json
{ "cmd": "version", "version": "<VERSION-STRING>", "code": 0 }
```

- `cmd`: always present.
- `version`: always present in success case (the whole point of the handler).
- `code`: always present, always `0`.
- All other fields: always omitted.

#### 3.2.2 `getconfigpath` success (candidate found)

```json
{ "cmd": "getconfigpath", "content": "<absolute path>", "code": 0 }
```

- `cmd`: always present.
- `content`: present in this case, carrying the first existing candidate path.
- `code`: always present, always `0`.
- All other fields: always omitted.

#### 3.2.3 `getconfigpath` empty-result (no candidate exists)

```json
{ "cmd": "getconfigpath", "code": 1 }
```

- `cmd`: always present.
- `content`: ABSENT (upstream's `result.content` stays as `none` because the early-return on empty path skips setting it; null-stripping then omits it).
- `code`: always present, value `1` (per upstream's "recoverable failure" convention).

Audit cite: `native_main.nim:155-160`. The empty-result branch sets only `code = some(1)` and never assigns `content`.

#### 3.2.4 `temp` success

```json
{ "cmd": "temp", "content": "<absolute path to new tempfile>", "code": 0 }
```

- `cmd`: always present.
- `content`: present in success case; carries the absolute path to the newly created tempfile.
- `code`: always present, always `0` in success case.
- On I/O failure in upstream: the response shape depends on WHERE in the handler the failure occurred. If `mkstemp` or the initial `write` raises, the response is `{ "cmd": "temp", "code": 2 }` with NO `content` (because `result.content` was never assigned). If `write` SUCCEEDED but `close` raises, `result.content = some(filepath)` was already set on the success path BEFORE the `close` call, so the response is `{ "cmd": "temp", "content": "<filepath>", "code": 2 }` — content IS present even though code indicates failure. v2 sidesteps BOTH upstream shapes by returning the generic error shape AND unlinking the partially-written tempfile; v2 does not need to choose between the two upstream paths because it does not surface either.

#### 3.2.5 `read` success

```json
{ "cmd": "read", "content": "<file content as string>", "code": 0 }
```

- `cmd`: always present.
- `content`: present in success case; carries the file's bytes decoded as a string. Empty file is `"content": ""` (empty string preserved per null-stripping rules below).
- `code`: always present, always `0` in success case.
- Open-failure shape is documented separately in Section 5.

#### 3.2.6 `run` normal-exit (any exit code)

```json
{ "cmd": "run", "command": "<command-string echoed verbatim>", "content": "<merged stdout+stderr>", "code": <process exit status> }
```

- `cmd`: always present.
- `command`: ALWAYS present; echoes the request's `command` field verbatim. Audit cite: `native_main.nim:172`.
- `content`: ALWAYS present (even when the subprocess produced zero bytes of output; in that case `content` is the empty string).
- `code`: ALWAYS present; integer exit status from `waitForExit`. May be 0 (normal-zero), positive (normal-nonzero), or signal-derived (typically 128 + signum on POSIX shells via `/bin/sh -c`).
- v2 additional terminal causes (overflow / timeout / connection-loss) are NOT modeled as upstream response shapes; see Section 7 for the v2-specific error responses.

#### 3.2.7 `getconfig` success (rc candidate found, non-empty content)

```json
{ "cmd": "getconfig", "content": "<verbatim rc file contents>", "code": 0 }
```

- `cmd`: always present.
- `content`: present in this case; the verbatim bytes of the first existing rc candidate, decoded as UTF-8 (matching v2's existing `read`-handler decoding semantics).
- `code`: always present, always `0`.
- All other fields: always omitted.
- Audit cite: upstream `native_main.nim:151-152`.

#### 3.2.8 `getconfig` empty-rc-file (rc candidate found, file is 0 bytes)

```json
{ "cmd": "getconfig", "content": "", "code": 0 }
```

- `cmd`: always present.
- `content`: PRESENT-AND-EMPTY (explicitly set to the empty string; not absent). Upstream's `readFile` returns an empty string for a 0-byte file; `toJson` at lines 43-45 emits empty strings via `%value.get`.
- `code`: always present, always `0`.
- This shape is STRUCTURALLY DISTINCT from 3.2.9 (no-candidate), which omits `content` entirely. Consumers MUST distinguish absent-vs-empty per Section 6.

#### 3.2.9 `getconfig` no-candidate (no existing rc on disk)

```json
{ "cmd": "getconfig", "code": 1 }
```

- `cmd`: always present.
- `content`: ABSENT (upstream's `result.content` stays as `none(string)` because the no-candidate branch never assigns it; `toJson` then omits the field).
- `code`: always present, value `1` (per upstream's "recoverable failure" convention).
- Audit cite: `native_main.nim:148-149`.

#### 3.2.10 `getconfig` IOError-on-read or foreign-UID

```json
{ "cmd": "getconfig", "code": 2 }
```

- `cmd`: always present.
- `content`: ABSENT.
- `code`: always present, value `2` (per upstream's "I/O failure" convention).
- Upstream fires this when `fileExists` returned true but `readFile` then failed (permission denied, race, etc.). Audit cite: `native_main.nim:153-154`.
- v2.2 ALSO fires this when the candidate exists but its file-UID differs from the daemon's UID per REQ-4800 / Section 8.24.9 of the implementation plan. The UID check makes the "user-owned configuration" claim true before disclosing the file's contents to the requesting extension.

#### 3.2.11 `getconfig` oversize (rc file exceeds outbound response cap)

The oversize case has NO upstream equivalent (upstream has no cap). v2.2 returns the GENERIC ERROR shape (Section 7):

```json
{ "cmd": "error", "error": "file too large to return" }
```

This matches the existing `read`-handler oversize response shape; the bounded-read primitive (shared between `read` and `getconfig`) returns the `:oversize` status which the handler maps to this generic error.

### 3.3 Field Presence Summary Table

For each `(cmd, field)` pair in the success case:

| field \ cmd | `version` | `getconfigpath` (found) | `getconfigpath` (empty) | `getconfig` (found) | `getconfig` (not found) | `temp` | `read` | `run` |
|---|---|---|---|---|---|---|---|---|
| `cmd` | always | always | always | always | always | always | always | always |
| `version` | always | omitted | omitted | omitted | omitted | omitted | omitted | omitted |
| `content` | omitted | present (path) | omitted | present (file bytes, possibly empty) | omitted | present (path) | present (file bytes) | present (output) |
| `command` | omitted | omitted | omitted | omitted | omitted | omitted | omitted | present (echo) |
| `code` | always `0` | always `0` | always `1` | always `0` | always `1` (no candidate) or `2` (IOError or foreign-UID per REQ-4800) | always `0` (in success; `2` on I/O failure upstream / generic error in v2) | always `0` (in success; open-failure shape in Section 5) | always (exit status) |
| `error` | omitted | omitted | omitted | omitted | omitted | omitted | omitted | omitted |

## 4. `code` Value Semantics

Upstream convention from `native_main.nim`:

- `0` — success. The handler completed and any data it produced is in `content` (or, for `version`, in `version`).
- `1` — recoverable failure. Used by upstream for "the operation cannot be performed but the system is fine." Concrete uses in scope: `getconfigpath` empty-result (no candidate exists), `getconfig` no-candidate (no existing rc on disk; v2.2). Outside v2 scope: `writerc` when file exists and `force` is false, `move` when destination exists and `overwrite` is false.
- `2` — I/O failure. Used by upstream for "an underlying OS or file operation raised." Concrete uses in scope: `read` open-failure (file does not exist or cannot be opened), `getconfig` IOError-on-read OR foreign-UID candidate (v2.2; the foreign-UID branch is v2.2-specific per REQ-4800 / Section 8.24.9 of the implementation plan). In upstream `temp` and `write` also use `code=2` on I/O failure; v2 returns the generic error response shape for `temp` I/O failures instead.

V2 reproduces this code-numbering for the five in-scope handlers in their success and (for `read`) defined I/O-failure shapes. V2 does NOT introduce additional `code` values. Other failure modes (parse errors, whitelist rejections, oversized responses, missing required fields, TRAMP-path rejection) use the generic error response shape from Section 7, which has NO `code` field.

## 5. `read` Open-Failure Response Shape

When the upstream `read` handler's `open()` call fails (file does not exist, permission denied, OS-level open error), it returns:

```json
{ "cmd": "read", "content": "", "code": 2 }
```

Audit cite: `native_main.nim:204-212`.

This shape is DISTINCT from the generic error response. The distinguishing characteristics:

- `cmd` is `"read"` (not `"error"`).
- `content` is the empty string (explicitly set, included verbatim per null-stripping rules).
- `code` is `2`.
- `error` is OMITTED.

Tridactyl callers depend on the distinction. The editor flow in `excmds.ts:372` checks `exec.code != 0` to decide whether to write the file's content back to the textarea; `loadtheme` in `excmds.ts:491` checks `file.code !== 0` and falls back to the in-config theme. Returning the generic error shape (`cmd = "error"`) would break this code path because the consumer would see `code = undefined`.

V2 preserves this shape exactly for `read` open-failures. In v2 the open-failure path runs AFTER the path-expansion guards (which reject TRAMP and remote paths with a generic error) and AFTER the v2 whitelist gate (which rejects non-whitelisted paths with a generic error). So the open-failure shape is only emitted when the path passes the gate AND TRAMP rejection AND then the OS-level open fails.

## 6. Null-Stripping Rules (Structural, Not Semantic)

Upstream `MessageResp.toJson` (`native_main.nim:37-58`) applies structural null-stripping:

- String fields (`cmd`, `version`, `error`, `sep`): included in JSON only if `value.len > 0`.
- Optional fields (`content`, `command`, `code`): included only if `value.isSome` (i.e., the handler explicitly assigned it).
- `isDir`: included only if `sep.len > 0` (paired with `sep`).
- `files`: included only if non-empty.

Key consequence: **structural absence is what is stripped, not semantic emptiness.**

- A handler that explicitly sets `result.content = some ""` (empty string Option) produces JSON containing `"content": ""` because the Option is `some` even though the string is empty. This is exactly what `read` open-failure does (`native_main.nim:212`).
- A handler that never assigns `result.content` produces JSON with `content` ABSENT because the Option is `none`. This is what `getconfigpath` empty-result does (`native_main.nim:158-160`).
- An explicit `some 0` for `code` is included (`"code": 0`); an absent `code` is omitted entirely. Likewise `false` would be preserved if any handler used a `some false` for an Option[bool], though no in-scope handler does.

V2 reproduces this with a structural null-stripping helper:

- The handler returns an alist/plist whose keys correspond to response fields and whose values are either the explicit field value (any type) or a sentinel `:absent` (a keyword) signaling "do not include this field."
- The response builder iterates the alist and omits any field whose value is `:absent`.
- All other values — including the empty string `""`, the integer `0`, and the boolean `nil`/`false` — are preserved in the JSON output.

The strict distinction between absent and present-with-empty value is mandatory for back-compat with Tridactyl's editor flow (which relies on `read` open-failure delivering an empty-string `content`, not an absent `content`).

## 7. Error Response Shapes

This section specifies the generic error response shape and enumerates the specific cases that produce it.

### 7.1 Generic Error Response

```json
{ "cmd": "error", "error": "<descriptive message string>" }
```

- `cmd` is the literal string `"error"`. This is HOW callers recognize a generic-error response (as opposed to a per-cmd error like `read`'s open-failure).
- `error` is a human-readable diagnostic string. Callers MAY display it to the user but MUST NOT parse it as a stable API; the exact wording is not part of the contract.
- ALL other fields are omitted:
  - No `code` field (this is what distinguishes the generic error from `read`'s I/O-failure shape, which DOES have `code = 2`).
  - No echo of request fields. Specifically, the request's `cmd`, `file`, `command`, `content`, or any other field is NOT included.

### 7.2 Specific Cases Producing the Generic Error

| Case | Response | Notes |
|---|---|---|
| Unknown cmd | `{ "cmd": "error", "error": "Unhandled message" }` | The exact wording `"Unhandled message"` is preserved from upstream `native_main.nim:382` for back-compat with any Tridactyl code that pattern-matches on it (none currently does, but the audit-driven minimum-divergence principle preserves it). |
| Handler signals (raises) during dispatch | `{ "cmd": "error", "error": "<signal message>" }` | The signal's `error-message-string` is used as the `error` value. The listener's `condition-case-unless-debug` catches the signal and routes it through this shape. |
| Parse failure (malformed JSON or invalid UTF-8 in the body) | `{ "cmd": "error", "error": "parse error: <reason>" }` | The frame is well-formed (length prefix + correct number of bytes) but the body does not decode to a JSON object. |
| Inbound frame larger than inbound-frame cap (default 10 MiB) | NO RESPONSE; connection closed silently | The listener logs the event and drops the connection. No error frame is sent because the listener cannot trust the frame's content to even derive a `cmd` echo. |
| Missing required field (`file` absent for `read`, `command` absent for `run`, `content` absent for `temp`) | `{ "cmd": "error", "error": "missing required field: <field name>" }` | v2-specific. Upstream would crash on `Option.get()`; v2 handles gracefully. |
| TRAMP / remote path rejection | `{ "cmd": "error", "error": "remote paths not permitted" }` | v2-specific. The path-expansion helper's TRAMP guard rejects before any I/O. |
| `read` whitelist rejection | `{ "cmd": "error", "error": "path not in whitelist" }` | v2-specific. Detailed in Section 15. |
| `run` whitelist rejection | `{ "cmd": "error", "error": "command not in whitelist" }` | v2-specific. Detailed in Section 15. |
| Listener-start whitelist-validation failed | NO RESPONSE; listener refuses to bind | The listener never starts; no socket exists; no client request ever reaches a handler. The failure surfaces only through the `firefox-to-emacs-native-messenger-start` command's error in the daemon. |
| Oversized response (over outbound cap) | `{ "cmd": "error", "error": "response too large" }` | v2-specific. Detailed in Section 11. |
| `read` file larger than configured cap | `{ "cmd": "error", "error": "file too large to return" }` | v2-specific. Detailed in Section 12. |
| Response content cannot be encoded as UTF-8 JSON (e.g., `run` captured invalid-UTF-8 bytes) | `{ "cmd": "error", "error": "response not encodable" }` | v2-specific. The response writer's `json-serialize` step raises `wrong-type-argument json-value-p` on unibyte strings containing bytes outside the UTF-8 range; the writer catches the signal and replaces with this generic error, structurally analogous to the oversized-response replacement (Section 11). The serialization-failure path is a small extension to the response writer's oversize-replacement logic. Detailed in Section 10 (run output capture). |

The wording shown here may be refined as the dedicated sections (Sections 11, 12, 15, 16) provide more detail.

## 8. Version String Choice and Reasoning

The v2 bridge claims the upstream-compatible VERSION string `"0.3.7"` in its `version` handler response.

### 8.1 Decision Rule

v2 claims the LOWEST upstream `native_main.nim` `VERSION` whose request and response contract is fully implemented for the six handlers in scope, with the additional floor of `"0.1.9"` to satisfy any Tridactyl callsite that gates on `nativegate("0.1.9")`.

### 8.2 Audit of Per-cmd Contract Floors

| Contract requirement | First upstream version | Audit citation |
|---|---|---|
| `version` returns the constant `VERSION` string with `code = 0` | trivially old; predates the cloned history | `native_main.nim:135-137` |
| `getconfigpath` returns the path in the `content` field with `code = 0` (or `code = 1` for empty-result with absent `content`) | `0.2.3` (oldest visible in the depth-50 clone; possibly earlier) | commit `b6bf23a` of `native_main.nim` already implements the handler |
| `temp` writes content to `mkstemp`-created tempfile, returns path in `content`, with `tmp_<prefix>_` filename prefix | trivially old; stable for many releases | `native_main.nim:285-294` |
| `read` open-failure shape `{cmd: "read", content: "", code: 2}` (rather than `{cmd: "read", code: 2}`) | **`0.3.4`** | commit `e9195ed` "Fix #45: make failed read return empty string" |
| `read` env-var expansion (`$VAR` and `${VAR}` substituted in `file` paths) | **`0.3.7`** | commit `717500e` "Expand env variables" |
| `run` writes the request's `content` field to the subprocess's stdin then closes stdin | **`0.3.7`** | commits `dc2e7d6` "Write content to input stream" and `a796621` "Fix error if msg content is empty" |
| Tridactyl-side `getrcpath` caller (`loadtheme`) gates with `nativegate("0.1.9")` | **`0.1.9`** | `tridactyl/src/excmds.ts:487` |

### 8.3 Computation

`max(0.3.4, 0.3.7, 0.3.7, 0.2.3, 0.1.9) = 0.3.7`

### 8.4 Consequences of Claiming `"0.3.7"`

Tridactyl's `nativegate(...)` will report v2 as compatible with all handlers up to and including the `0.3.7` floor. The following Tridactyl callsites will therefore ATTEMPT to invoke the v2 bridge:

| Tridactyl callsite | Wire cmd | Tridactyl version floor | v2 response |
|---|---|---|---|
| `Native.run_async` | `run_async` | `0.3.1` | Falls through to `{"cmd": "error", "error": "Unhandled message"}` (`run_async` is out of v2 scope). Tridactyl flows that rely on this (notably `:installnative`) will fail. |
| `Native.move` | `move` | `0.3.0` | Falls through. |
| `Native.writerc` | `writerc` | `0.1.11` | Falls through. |
| `Native.getenv` | `env` | `0.1.2` | Falls through. |
| `:winFirefoxRestart` | `win_firefox_restart` | `0.1.6` | Falls through (Linux build of v2; Windows is out of scope). |
| `ff_cmdline` (Linux non-Windows path) | `ppid` | `0.2.0` (implicit, since `ppid` was added before our visible history) | Falls through. Tridactyl falls back to its `0.2.0`-or-older code path that uses pyeval, which also falls through. The user-facing consequence is that Tridactyl cannot recover Firefox's command line, which is used only for profile discovery via the `:fixamo`-style code paths. Not on the editor flow path. |
| `Native.getrc` (loadtheme, `:source`, `TriStart -> source_quiet`) | `getconfig` | `0.1.0` | Implemented in v2.2 (Sections 3.2.7-3.2.11, B.8); ungated per REQ-4800 with file-UID equality check per Section 8.24.9 of the implementation plan. Adding the handler does NOT raise the claimed VERSION because `getconfig` predates `0.3.7` in upstream `native_messenger`. |

These fall-throughs are by design (the corresponding handlers are out of v2 scope). The README documents the affected user-facing features in the Known Limitations section.

### 8.5 Why Not a Different Version?

- `"0.5.0"` (current upstream latest): would over-claim. The v2 bridge does not implement upstream's `0.4.0` change to `getconfigpath` candidate list (XDG_CONFIG_HOME on Windows) — although v2 is Linux-only, so this is moot in practice. There is no Tridactyl callsite that gates above `0.3.3` for any in-scope cmd, so claiming `0.5.0` enables no additional features for the bridge's intended use. Strictly less correct per the "lowest" rule.
- `"0.3.4"`: under-claims. The v2 `read` handler implements env-var expansion via its path-expansion helper, and the v2 `run` handler writes request `content` to subprocess stdin. Both behaviors were absent from upstream until `0.3.7`. Claiming `0.3.4` would represent the v2 messenger as more limited than it actually is. Functionally harmless (the additional features are strict adds that don't break old-shape requests) but not consistent with v2's "report version honestly" rule.
- `"0.1.9"`: under-claims by a wide margin. The `read` empty-string open-failure contract did not exist until `0.3.4`, so a `0.1.9` claim is inconsistent with v2's `read` shape.

### 8.6 Forward-Compatibility Notes

Future v2 releases that add new handlers MUST re-run this audit against the new combined contract. Any added handler whose upstream contract first appeared in a version higher than `0.3.7` would force the claimed VERSION upward to match. v2.2 adds `getconfig` to the in-scope handler set; per Section 8.24.8 of the implementation plan, `getconfig` is present in upstream `native_main.nim` since native_messenger tag `0.0.1` (verified via `git show 0.0.1:src/native_main.nim`), so adding it does NOT raise the claimed VERSION above `0.3.7`. Subsequent handler additions remain deliberate scope decisions.

## 9. Tridactyl Wire-Usage Audit

Tridactyl uses ONLY `browser.runtime.sendNativeMessage` for all five in-scope cmds. The relevant audit point in `tridactyl/src/lib/native.ts`:

- Line 51 (`sendNativeMsg` function body): `resp = await browserBg.runtime.sendNativeMessage(NATIVE_NAME, send)`. This is the SOLE invocation surface.
- There is NO call to `browser.runtime.connectNative` (port-style multi-frame native-messaging) anywhere in `src/lib/native.ts` or `src/excmds.ts`.

Consequence: the v2 bridge handles exactly ONE length-prefixed JSON frame per accepted connection and produces at most ONE length-prefixed JSON response per connection. Port-style sessions are out of scope. The filter's `'dispatched`/`'responded` state-field semantics reflect this one-frame-per-connection contract: after a response is sent, the connection moves to `'closing` rather than `'reading` again.

## 10. `run` Output Capture Semantics

This section describes upstream's stdout/stderr capture vs. v2's diverging design.

### 10.1 Upstream Behavior

Upstream `native_main.nim` (lines 167-186) captures `run` output as follows:

```
let process = startProcess(command, options = {poEvalCommand, poStdErrToStdOut})
...
var content = ""
for line in process.outputStream.lines:
    content.add(line)
    content.add('\n')
result.content = some content
result.code = some waitForExit(process)
```

Key observations:

- **stderr merging**: `poStdErrToStdOut` causes stderr to merge into stdout at the OS level before the messenger reads it.
- **Line-based reading**: `process.outputStream.lines` is Nim's text-mode line iterator, which reads up to each newline and strips the newline.
- **Newline normalization**: each iterated line gets a `\n` appended to `content`. Consequence: any subprocess that emits output WITHOUT a trailing newline gets one added by the messenger; any subprocess that emits a trailing newline has it round-trip preserved; any subprocess that emits CR or CRLF line endings has them normalized to LF.
- **Binary corruption risk**: Nim's text-mode line iterator may not handle NUL bytes or non-UTF-8 sequences correctly. Binary output from the subprocess is at risk of corruption.

### 10.2 v2 Divergence

v2's `make-process` invocation specifies `:coding 'binary` and `:stderr nil` for the `make-process` call. This produces:

- **stderr merging**: same end result (stderr merges into stdout) via Emacs's `:stderr nil` semantics, which is equivalent at the OS level.
- **Raw-byte in-memory capture**: the filter accumulates raw bytes from the subprocess's combined stdout+stderr stream into a unibyte string. NO line-based reading and NO newline normalization. NUL bytes and arbitrary high-bit byte sequences are preserved IN MEMORY exactly as the subprocess emitted them.
- **Wire-encoding gate**: response `content` must be representable as UTF-8 JSON to transit the wire. The response writer's serialization step first DECODES the captured unibyte string as UTF-8 (e.g., via `decode-coding-string ... 'utf-8`), then passes the resulting multibyte string to `json-serialize`. JSON's `\uXXXX` escapes support every Unicode scalar (including control characters like NUL), so any captured byte sequence that decodes as valid UTF-8 transits the wire faithfully with `content` carrying the (possibly escape-encoded) decoded string. If the UTF-8 decode step fails or `json-serialize` raises `wrong-type-argument json-value-p` (Emacs rejects unibyte strings containing bytes outside the UTF-8 range; this would happen if the writer skipped the decode step), the writer catches the signal and replaces the response with the generic error shape `{ "cmd": "error", "error": "response not encodable" }`, structurally analogous to the oversized-response replacement (Section 11). The writer's serialization-failure path is a small extension to its oversize-replacement logic, also noted in Section 7.2 above. v2 thus preserves binary fidelity in MEMORY but the WIRE remains UTF-8-bound; the relevant fixture is `run-success-binary-output` in Appendix B.5.

### 10.3 User-visible Differences

For typical shell commands that emit text with trailing newlines (e.g., `echo hello`, `ls -la`, `git status`), the upstream and v2 outputs are byte-identical.

For commands that emit text WITHOUT a trailing newline (e.g., `printf abc`, `echo -n hello`), v2 captures the exact bytes the subprocess wrote, while upstream would append a `\n`. This is a deliberate divergence: v2 reports what the subprocess actually wrote.

For commands that emit binary output (e.g., `head -c 1024 /dev/urandom`), v2 preserves the bytes; upstream would corrupt them. v2's outbound JSON serialization may still fail on non-UTF-8 bytes — that failure surfaces as the oversized-response generic error or a serialization signal, depending on which check fires first. The behavior is bounded by the run-output cap so the worst case is a generic error response, not a listener crash.

### 10.4 Fixtures

The fixtures `run-success-zero-exit`, `run-success-no-trailing-newline`, `run-success-nonzero-exit`, and `run-stderr-merged` in Appendix B together exercise this divergence:

- `run-success-zero-exit` uses `echo hello` and expects `"hello\n"` — the trailing newline comes from `echo`, not from any v2 normalization.
- `run-success-no-trailing-newline` uses `printf abc` and expects exactly `"abc"` (three bytes) — confirming v2 does NOT add a newline.
- `run-success-nonzero-exit` confirms zero-byte output is reported as the present-empty-string `""` (not absent and not a synthesized `"\n"`).
- `run-stderr-merged` uses placeholder content because process-scheduling determines the interleaving order; tests assert byte-set membership.

## 11. Oversized-Response Error

This section specifies the v2-specific oversized-response error shape introduced by the outbound-response-payload cap.

### 11.1 The Cap

Each outbound response payload is capped at 768 kibibytes (786432 bytes) by default, where "payload" means the SERIALIZED UTF-8 JSON byte length AFTER null-stripping, exclusive of the 4-byte length prefix. The cap is configurable via a defcustom; this section describes the contract regardless of the chosen value.

### 11.2 The Response Shape

When the response builder's structural null-stripping pass produces a payload exceeding the cap, the response is replaced exactly once with:

```json
{ "cmd": "error", "error": "response too large" }
```

This is the generic error shape (Section 7.1). The replacement preserves NO information from the original response: not the original `cmd`, not echoes of request fields, not the original `code`. Per the wording rules for whitelist rejections (which apply broadly to v2-specific generic errors), the original request fields are NOT echoed.

### 11.3 Degenerate Fallback

If the replacement error response itself somehow exceeds the cap (e.g., the cap is configured pathologically low), the response builder logs a critical message and sends NO frame. The listener still transitions the connection to `'responded` so the connection-cleanup path runs normally; the peer simply observes EOF without an error response.

The degenerate case is extremely unlikely in practice: the replacement payload `{"cmd":"error","error":"response too large"}` is approximately 50 bytes, well below any reasonable cap.

### 11.4 Fixture

See `oversized-response-error` in Appendix B. The fixture's request side is parametric because the failure can arise from any handler that produces large output; specific-source variants exist for `read` (`read-file-too-large`) and `run` (`run-overflow-error`).

### 11.5 Relationship to the Inbound Frame Cap

The outbound cap is SEPARATE from the inbound frame-size cap (default 10 MiB). The inbound cap covers REQUEST frames received from Firefox; oversized inbound frames cause silent connection close (no frame is sent in response). The outbound cap covers RESPONSE frames the v2 listener emits; oversized outbound responses are REPLACED with the generic error shape per this section.

## 12. `read` Bounded-Read Cap

This section specifies the v2-specific bounded-read cap for the `read` handler.

### 12.1 The Cap

The `read` handler enforces a production cap on the raw file bytes it will read into memory before building a response. The cap value is `(outbound-response-cap + 1)` bytes — exactly one byte more than the outbound cap (Section 11). The "plus one" exists so the response builder can detect over-cap raw reads by observing that the read returned exactly `outbound-response-cap + 1` bytes (i.e., the read filled the buffer and the file has at least one more byte beyond it).

In default configuration (outbound cap 768 KiB), the `read` cap is 786433 bytes (768 KiB + 1 byte).

### 12.2 Over-cap Behavior

When the `read` handler observes that the raw read filled the (outbound-cap + 1) buffer:

- It does NOT build a `content` field from the buffer (that would waste cycles on a request guaranteed to fail the outbound check).
- It returns the generic error shape:

```json
{ "cmd": "error", "error": "file too large to return" }
```

The wording is suggested and may be refined.

### 12.3 Why This Cap Differs from the Inbound Frame Cap

The inbound frame cap caps INBOUND frame size (the request). The `read` bounded-read cap covers RESPONSE size by capping the file the handler is asked to read. Without this cap, `read` of a 1 GB file would consume 1 GB of Emacs heap before the response builder detected the oversize and replaced it with the generic error. The bounded-read cap avoids that memory pressure entirely.

### 12.4 Within-cap Edge Cases

For files near the cap boundary:

- File size strictly less than `outbound-response-cap`: raw read succeeds; response builder includes `content`; outbound-size check passes; full content goes on the wire.
- File size equal to or greater than `outbound-response-cap`: raw read may succeed; the SERIALIZED response (which adds JSON quoting overhead — every non-ASCII or escape-needing byte expands) may exceed the outbound cap; the response writer replaces with `oversized-response-error`.
- File size at or above `outbound-response-cap + 1`: `read` returns the bounded-read error WITHOUT building a content field.

The JSON-escape inflation means a file at exactly the outbound cap can still trigger the response writer's replacement on the response builder side; the v2 test suite exercises this gap.

### 12.5 Fixture

See `read-file-too-large` in Appendix B.

## 13. Per-connection Plist Keys

This section enumerates the v2 implementation-level plist keys (an internal-implementation note rather than a wire-contract requirement).

The keys are NOT part of the wire contract (the peer never observes them); they are documented here to keep the v2 implementation's listener state machine traceable from the contract document.

| Key | Type | Lifetime | Meaning |
|---|---|---|---|
| `read-buffer` | unibyte string | from accept to dispatch | Accumulates bytes received from the peer. Used to decode the 4-byte length prefix once 4+ bytes are buffered and then the JSON body once `declared-length` bytes are buffered. |
| `declared-length` | integer or `:unset` | from accept to dispatch | The frame's declared body length, decoded from the 4-byte little-endian prefix. Unset until 4 bytes are buffered; set once and not modified thereafter. |
| `read-timer` | timer or nil | from accept to dispatch or peer-close | Per-connection read deadline. Started on accept; canceled on complete-frame receipt or peer close. Default duration is configurable via the v2 listener's defcustoms. |
| `state` | symbol | from accept to teardown | One of `'reading` (filter accumulating), `'dispatched` (handler deferred response, used only for `run`), `'responded` (writer mark-responded), `'closing` (teardown). Transitions per the v2 listener implementation. |

The per-connection plist lives on the connection process object via process properties (`process-put`/`process-get`). It is freed automatically when the process is deleted.

## 14. Editor-Flow Call Graph

This section presents the editor-flow audit finding and its consequences for v2's default-deny gates.

### 14.1 Tridactyl-side Sequence

The `:editor` excmd on Linux performs the following sequence of native-messenger calls (audit cite: `tridactyl/src/excmds.ts:349-394`):

1. **`nativegate()` with default args**: invokes the `version` cmd internally to check messenger presence. The v2 `version` handler is ungated (no whitelist for `version`).
2. **`Native.temp(text, document.location.hostname)`**: invokes the `temp` cmd to write the textarea's content to a tempfile. The v2 `temp` handler is ungated.
3. **`Native.editor(file, ...pos)`** which is `tridactyl/src/lib/native.ts:464-485`:
   a. If `editorcmd === "auto"`, calls `getBestEditor()`. This in turn calls `firstinpath(...)` which iterates candidate editor commands, calling `inpath(cmd)` for each. Each `inpath` call invokes the `run` cmd with `which <command>` (Linux) or `where <command>` (Windows). Zero or more of these probe `run` invocations occur before a candidate is found.
   b. Resolved editor command (or the user's explicit `editorcmd`) is invoked via `Native.run(editorcmd_with_substituted_args)`.
4. **`Native.read(file)`**: invokes the `read` cmd to retrieve the (possibly-edited) tempfile content.

The `editor_rm` alias adds one more `Native.run` call with `rm -f '<temp-path>'` to delete the tempfile after reading.

### 14.2 v2 Compatibility Under Default Deny-All Whitelists

The v2 default whitelists are `nil` (deny-all). For the editor flow to work, the user MUST configure:

- `firefox-to-emacs-native-messenger-run-whitelist` to admit the editor invocation, e.g., `("emacsclient <TEMP-PATH>")`.
- `firefox-to-emacs-native-messenger-read-whitelist` to admit reads of registered tempfiles, e.g., `("<TEMP-PATH>")`.

The README documents this configuration walkthrough.

### 14.3 `editorcmd === "auto"` Is NOT Supported Under Default Whitelists

Tridactyl's `getBestEditor` issues `run` requests with `which <name>` commands (e.g., `run "which vim"`, `run "which emacs"`). None of these match the canonical editor-flow whitelist entry `'("emacsclient <TEMP-PATH>")` because:

- `which vim` has no `<TEMP-PATH>` substring and is not the literal `emacsclient <TEMP-PATH>`.
- The `run` command-gate matcher requires every entry to either be a literal exact-match or contain `<TEMP-PATH>` markers that resolve to capability-registered paths. `which vim` matches neither shape.

Consequently, `editorcmd === "auto"` produces a cascade of "command not in whitelist" rejections before any editor is found, and the editor flow aborts. v2 requires the user to set `editorcmd` to an explicit value such as `emacsclient %f` (substituted to `emacsclient <temp-path>` at request time).

### 14.4 `getconfigpath` Is Independent of the Editor Flow

The fifth in-scope handler `getconfigpath` and the sixth `getconfig` (added in v2.2) are NOT used by the editor flow. `getconfigpath` returns the path of the first existing rc candidate and is consumed by user-authored `:js`-based read-of-tridactylrc bindings and theme loading via `:colors` / `:js --rc`. `getconfig` returns the file contents of the same candidate and is consumed by Tridactyl's `:source` excmd and the `TriStart -> source_quiet` startup auto-source path. The `:viewconfig --user` excmd dumps in-memory `config.USERCONFIG` and does NOT consume `getconfigpath` (it is independent of the bridge).

## 15. Whitelist-Rejection Response Wording

This section pins the concrete wording for whitelist-rejection generic errors.

### 15.1 Suggested Wording

| Handler | Rejection condition | Suggested `error` wording |
|---|---|---|
| `run` | Whitelist match fails | `"command not in whitelist"` |
| `read` | Whitelist match fails (after path expansion succeeded) | `"path not in whitelist"` |
| `read` | TRAMP path detected | `"remote paths not permitted"` |
| `read` | Bounded-read cap exceeded | `"file too large to return"` |
| `run` | Subprocess output cap exceeded | `"response too large"` |
| `run` | Subprocess timeout exceeded | `"run timeout exceeded"` |
| Any handler with malformed whitelist at gate-time | Per-gate validator fired | Whatever the shared validator emits (typically `"whitelist malformed: <constraint>"`) |

### 15.2 Wording Stability Contract

The exact wording is NOT part of the wire contract: callers MUST NOT pattern-match on the `error` string. Callers MUST distinguish error categories by the `cmd = "error"` shape itself, optionally cross-referenced with `read`'s I/O-failure shape (which has `cmd = "read"` and `code = 2`, NOT the generic error shape).

Tridactyl currently does not pattern-match on any `error` wording, so v2 has wording freedom. The wording chosen above is intended to be informative for users inspecting the bridge log buffer , not for programmatic consumers.

### 15.3 Fixtures

See `run-whitelist-rejection`, `read-whitelist-rejection`, `whitelist-rejection-run`, `whitelist-rejection-read`, and `whitelist-malformed-gate` in Appendix B.

## 16. Capability-Registry Behavior

This section specifies the capability-registry contract that the `temp` handler establishes and that the `read` and `run` whitelist matchers consume.

### 16.1 Registration

When a `temp` request succeeds, the bridge AFTER writing and flushing the new tempfile, BEFORE returning the response, enters the absolute path into the in-memory capability registry. The registry entry stores the file's identity at registration time: device number, inode number, and user ID (extracted via `file-attributes`). Sections 16.2 through 16.5 below specify the full semantics.

### 16.2 Subsequent Whitelist Match

A subsequent `read` or `run` request whose argument (the request's `file` for `read`, or a `<TEMP-PATH>`-marker-extracted substring for `run`) is in the registry AND whose current file identity (dev/inode/uid) matches the registered identity AND whose current file type is a regular file passes the `<TEMP-PATH>` whitelist match. Otherwise the match fails.

### 16.3 Prune-on-access

The capability-registry `contains-p` predicate prunes entries on access when:

- The file at the registered path no longer exists.
- The file's current dev/inode/uid does not match the registered identity (file replaced, renamed, or chown'd).
- The file is no longer a regular file (e.g., became a symlink or directory).

In any of these cases, `contains-p` returns nil AND removes the entry from the registry. Subsequent `read`/`run` requests with that path fail the `<TEMP-PATH>` match.

### 16.4 Registry Lifetime

The registry survives only for a single listener lifetime. The registry is cleared:

- On listener start (AFTER the dedicated tempfile-directory sweep).
- On listener stop.

A `temp`-registered path therefore becomes unrecognized after `M-x firefox-to-emacs-native-messenger-stop` followed by `-start`, even though the file may still exist on disk.

### 16.5 Bounded Size

The registry size is bounded by the `firefox-to-emacs-native-messenger-temp-registry-cap` defcustom (default 1024 entries). When a new `temp` registration would exceed the cap, the bridge runs a `contains-p` sweep over all entries (which prunes missing/identity-mismatched entries); if still at or above the cap, the `temp` handler returns the generic error response `{"cmd": "error", "error": "temp registry cap exceeded"}` and creates NO tempfile.

### 16.6 Three-fixture Behavior Captured in Appendix B

- `temp-creates-and-registers`: the `temp` call that adds to the registry.
- `read-of-registered-temp-path-ok`: a subsequent `read` of the registered path succeeds.
- `read-after-file-deleted-prune-and-reject`: deleting the file invalidates the registry entry.
- `read-after-listener-restart-reject`: restarting the listener invalidates ALL registry entries.

## 17. `getconfigpath` Candidate List

This section pins the upstream-defined hardcoded candidate-list order that the v2 `getconfigpath` handler walks.

### 17.1 Upstream Candidate List

Upstream `native_main.nim`'s `findUserConfigFile` (`src/native_main.nim:87-103`) defines the candidate list as:

```
let standardConfigDir =
    when not defined(windows):
        getConfigDir()
    else:
        getEnv("XDG_CONFIG_HOME", getConfigDir())

let candidateFiles =
    [
        standardConfigDir / "tridactyl" / "tridactylrc",
        getHomeDir() / ".config" / "tridactyl" / "tridactylrc",
        getHomeDir() / "_config" / "tridactyl" / "tridactylrc",
        getHomeDir() / ".tridactylrc",
        getHomeDir() / "_tridactylrc",
    ]
```

On Linux (v2's target), Nim's `getConfigDir()` returns `$XDG_CONFIG_HOME` if set, otherwise `~/.config/`. The candidate list on Linux is therefore (in upstream-defined order):

1. `$XDG_CONFIG_HOME/tridactyl/tridactylrc` (or `~/.config/tridactyl/tridactylrc` if `XDG_CONFIG_HOME` is unset)
2. `~/.config/tridactyl/tridactylrc` (literal `.config` path; this is candidate 1 expanded when `XDG_CONFIG_HOME` is unset, so on systems without `XDG_CONFIG_HOME` set, candidates 1 and 2 collide to the same path)
3. `~/_config/tridactyl/tridactylrc` (the underscore variant; legacy / Windows-style fallback)
4. `~/.tridactylrc`
5. `~/_tridactylrc` (legacy / Windows-style fallback)

### 17.2 v2 Candidate List

The v2 bridge MUST walk the same five candidates in the same order to match upstream's resolution semantics. The candidate list is recorded as a defconst in the v2 module and consumed by a shared walker function that the `getconfigpath` handler calls.

When `XDG_CONFIG_HOME` is set in the daemon's environment, the first candidate resolves to that path's tridactyl/tridactylrc. When unset, the first candidate resolves identically to the second; v2 retains both for ordering fidelity but the de-facto resolution is unaffected.

### 17.3 Path-Expansion Helper Application

The walker runs the path-expansion helper's TRAMP guard on EACH candidate before any I/O is attempted. The candidates are bridge-hardcoded so TRAMP injection is not a realistic vector, but the guard is applied defensively for consistency with the `read` handler's path-handling discipline. A candidate that fails the TRAMP guard is rejected (signals `firefox-to-emacs-native-messenger-bad-request`) rather than skipped; in practice this can occur only if a future code change introduces a TRAMP-shaped candidate.

### 17.4 Response Shapes

- **Success**: the first existing candidate (verified as a regular file via `file-attributes`) is returned in the `content` field with `code = 0`. See fixture `getconfigpath-success`.
- **Empty-result**: no candidate exists as a regular file. Returns `{"cmd": "getconfigpath", "code": 1}` with `content` ABSENT. See fixture `getconfigpath-empty`.

## 18. Audit Completeness Verification

This section records the verification gates and completeness confirmations applied during PROTOCOL.md authoring.

### 18.1 Per-cmd Audit Coverage

For each of the five in-scope cmds, the following sources have been audited and reflected in this document:

| cmd | `handleMessage` branch | `MessageResp.toJson` rules | `sendNativeMsg` callsite(s) | requiredNativeMessengerVersion |
|---|---|---|---|---|
| `version` | `native_main.nim:135-137` (Sections 2.1, 3.2.1) | strings + code option (Section 6) | `getNativeMessengerVersion` (Section 2.1) | floor `0.1.9` via `getrcpath` caller (Section 8) |
| `getconfigpath` | `native_main.nim:155-161` (Sections 2.2, 3.2.2, 3.2.3, 17) | strings + content option + code option (Section 6) | `getrcpath` (Section 2.2) | gated at caller via `nativegate("0.1.9")` (Section 8) |
| `temp` | `native_main.nim:285-294` (Sections 2.3, 3.2.4) | content option + code option (Section 6) | `Native.temp` (Section 2.3) | trivially old; included in min |
| `read` | `native_main.nim:204-212` (Sections 2.4, 3.2.5, 5) | content option + code option (Section 6) | `Native.read` (Section 2.4) | `0.3.4` for empty-string open-failure; `0.3.7` for env-var expansion (Section 8) |
| `run` | `native_main.nim:163-185` (Sections 2.5, 3.2.6, 10) | command option + content option + code option (Section 6) | `Native.run` (Section 2.5) | `0.3.7` for stdin support (Section 8) |

### 18.2 Unresolved Fields

No fields are referenced in upstream source for the five in-scope cmds that are not documented in this PROTOCOL.md. Specifically:

- `MessageResp` fields `sep`, `files`, `isDir` are inspected by the audit (Section 3.1) and confirmed NOT used by any in-scope handler.
- `MessageRecv` fields `version`, `error`, `dir`, `to`, `from`, `path`, `profiledir`, `browsercmd`, `force`, `overwrite`, `cleanup`, `code` are inspected and confirmed NOT used by any in-scope handler (`var` is read by the non-scope `env` handler; `path` is read by `list_dir`).

### 18.3 Fixture Coverage

Every in-scope cmd has at least one request-response fixture pair with non-deterministic fields schema-described:

| cmd | Primary fixture | Non-deterministic placeholder uses |
|---|---|---|
| `version` | `version-success` | none (VERSION is a literal) |
| `getconfigpath` | `getconfigpath-success`, `getconfigpath-empty` | `absolute-path` placeholder for the returned path |
| `temp` | `temp-success`, `temp-creates-and-registers` | `temp-path` placeholder for the returned tempfile path |
| `read` | `read-success`, `read-open-failure`, plus 6 variants | `nonempty-string` and `temp-path` placeholders |
| `run` | `run-success-zero-exit`, plus 6 variants | `nonempty-string` for interleaved stderr cases |

### 18.4 Required Content Present

The following content elements are present in this document:

| Element | Section |
|---|---|
| Generic error response shape | Section 7.1 |
| `read` I/O-failure shape distinct from generic error | Section 5 |
| Null-stripping rules per field | Section 6 |
| Version string with reasoning | Section 8 |
| Whitelist-rejection fixtures | Appendix B (`whitelist-rejection-run`, `whitelist-rejection-read`, `whitelist-malformed-gate`) |
| Capability-registry behavior fixtures | Appendix B (`temp-creates-and-registers`, `read-of-registered-temp-path-ok`, `read-after-file-deleted-prune-and-reject`, `read-after-listener-restart-reject`) |
| Fixture grammar specification | Appendix A |
| Per-handler request shapes with null-vs-absence semantics | Section 2 |
| Per-handler response field membership and presence rules | Section 3 |
| `code` value semantics | Section 4 |
| Tridactyl wire-usage audit | Section 9 |
| `run` output capture semantics with v2 divergence | Section 10 |
| Oversized-response error shape | Section 11 |
| `read` bounded-read cap | Section 12 |
| Per-connection plist keys (v2 implementation note) | Section 13 |
| Editor-flow call graph | Section 14 |
| Whitelist-rejection wording | Section 15 |
| Capability-registry behavior | Section 16 |
| `getconfigpath` candidate list | Section 17 |

## Appendix A: Fixture Grammar

Fixtures are concrete request/response pairs (or single responses) embedded in this document for use by the v2 ERT test harness's fixture loader (`firefox-to-emacs-native-messenger-test-load-fixture`). The grammar is mechanical so the loader can find every fixture without ambiguity.

### A.1 Anchor and Body Form

Each fixture consists of two elements that MUST appear in this order. AT MOST ONE blank line is allowed between them (the convention used throughout Appendix B is exactly one blank line for readability; the loader tolerates zero or one):

1. A standalone HTML comment line of the form `<!-- fixture: NAME -->` where NAME is a kebab-case identifier unique across the entire document (e.g., `version-success`, `read-of-registered-temp-path-ok`).
2. A fenced JSON code block (using the markdown triple-backtick fence with the `json` language tag) whose body is parsed as JSON.

The loader scans the document for the comment anchor with the matching NAME, then reads the next fenced JSON code block, ignoring at most one intervening blank line. Any other intervening content (a non-blank line, an HTML comment, a heading, a second blank line) causes the loader to signal `firefox-to-emacs-native-messenger-test-fixture-not-found` to flag a malformed fixture.

### A.2 Body Forms

The fenced JSON body may take one of two shapes:

- **Pair fixture**: a JSON object with exactly two keys, `request` and `response`, each holding a JSON object. Used when a complete request-response interaction is being captured. This is the primary form.
- **Response fixture**: a single JSON object representing only the wire response. Used when the request is described in prose nearby (e.g., when the same request shape is reused across many fixtures).

The harness loader detects the form by inspecting the top-level keys: an object whose top-level keys are exactly `{"request", "response"}` is a pair; any other shape is treated as a response fixture.

### A.3 Schema Placeholders for Non-deterministic Values

Concrete wire responses contain values that vary at runtime: tempfile paths, exit codes, timestamps. Fixtures use placeholders instead of literal values so they remain valid across test runs. The placeholder syntax is a JSON object with a single key `$schema` whose value is the placeholder type name. Example: a placeholder for any path-shaped string appears as:

```json
{ "content": { "$schema": "temp-path" } }
```

The harness's schema-aware equality helper (`firefox-to-emacs-native-messenger-test-fixture-equal-p`) matches a placeholder against ANY runtime value that satisfies the declared type. Literal values (e.g., `"cmd": "version"`, `"code": 0`) MUST match byte-for-byte. Fields ABSENT from the fixture's response object MUST be ABSENT in the runtime response; fields present in the runtime response but not in the fixture cause comparison failure.

**Request-side placeholders.** Placeholders may also appear in REQUEST fields. A request-side placeholder represents a value that the TEST SETUP supplies at runtime (e.g., a `temp-path` placeholder for a `read` request's `file` field is a path the test setup obtains by first calling `temp`, then substitutes into the request before sending). Request-side placeholders are DOCUMENTATION ONLY: a fixture with request-side placeholders is NOT directly sendable to the bridge. Tests that consume such fixtures must perform substitution before invoking the framed-request sender. The harness convention is for tests to substitute placeholders with the values bound by their preceding test setup; the exact substitution mechanism is implementation-defined per test (e.g., let-binding a captured value and `format`ing it into the request).

### A.4 Defined Placeholder Types

| Placeholder name | Matches |
|---|---|
| `path` | Any non-empty string. No further structure asserted. |
| `absolute-path` | Any non-empty string that begins with `/`. |
| `temp-path` | Any non-empty string matching the dedicated tempfile-directory pattern: `/tmp/firefox-to-emacs-native-messenger-tempfiles-<uid>/tmp_<sanitized-prefix>_<random>.txt`. |
| `version-string` | Any non-empty semver-shaped string (e.g., `"0.3.7"`, `"1.0.0"`). |
| `nonempty-string` | Any non-empty string. |
| `error-message` | Any non-empty string. The fixture does not pin error wording; tests that require specific wording use literal values instead. |
| `pid` | Any positive integer. |
| `timestamp` | Any ISO 8601-shaped string. |
| `exit-code` | Any integer in the range `[-1, 255]` (POSIX exit-status convention plus signal-derived values). |
| `nonzero-integer` | Any non-zero integer (positive or negative). |

The placeholder set is extensible: harness updates that introduce a new type MUST also define its match semantics in this table and bump the harness's placeholder-type defconst .

### A.5 Naming Convention

Fixture names are kebab-case. The recommended naming structure is `<command-or-context>-<scenario>`:

- `version-success`, `version-quiet-mode` (if multiple scenarios)
- `getconfigpath-success`, `getconfigpath-empty`
- `temp-success`, `temp-creates-and-registers`
- `read-success`, `read-open-failure`, `read-of-registered-temp-path-ok`, etc.
- `run-success-zero-exit`, `run-success-nonzero-exit`, `run-overflow-error`
- `whitelist-rejection-<handler>`, `whitelist-malformed-gate`, `unhandled-message`, `oversized-response-error`

### A.6 Authoring Constraints

- The `<!-- fixture: NAME -->` anchor MUST appear at column 0 (no leading whitespace).
- The fenced block MUST use the language tag `json` to ensure unambiguous parsing by the loader.
- Fixtures MUST appear inside an `## Appendix B: Fixtures` (or equivalently named) section so the loader's whole-document scan does not pick up unrelated JSON snippets elsewhere. In practice the loader anchors on the fixture comment, not the section boundary, so this is documentation discipline rather than loader machinery.
- A fixture name MUST be unique across the whole document. Duplicate names cause undefined loader behavior.

## Appendix B: Fixtures

This appendix collects every named fixture referenced by the v2 test harness, organized by handler. Per Appendix A's grammar, each fixture is preceded by a `<!-- fixture: NAME -->` comment line and consists of a fenced JSON block immediately after.

### B.1 `version` Fixtures

<!-- fixture: version-success -->

```json
{
  "request": { "cmd": "version" },
  "response": { "cmd": "version", "version": "0.3.7", "code": 0 }
}
```

The `version` field's value is the literal string chosen per Section 8; it is NOT a placeholder.

### B.2 `getconfigpath` Fixtures

<!-- fixture: getconfigpath-success -->

```json
{
  "request": { "cmd": "getconfigpath" },
  "response": { "cmd": "getconfigpath", "content": { "$schema": "absolute-path" }, "code": 0 }
}
```

The returned `content` is the first existing candidate path from the bridge's hardcoded candidate list (recorded in Section 17).

<!-- fixture: getconfigpath-empty -->

```json
{
  "request": { "cmd": "getconfigpath" },
  "response": { "cmd": "getconfigpath", "code": 1 }
}
```

Note the ABSENCE of the `content` field. Per Section 6, structural absence is preserved on the wire; an absent field is structurally distinct from a present-empty-string field.

### B.3 `temp` Fixtures

<!-- fixture: temp-success -->

```json
{
  "request": { "cmd": "temp", "content": "arbitrary content", "prefix": "example.com" },
  "response": { "cmd": "temp", "content": { "$schema": "temp-path" }, "code": 0 }
}
```

The response's `content` is the path returned by `make-temp-file` under the dedicated tempfile directory, matching the `tmp_<sanitized-prefix>_<random>.txt` pattern.

<!-- fixture: temp-creates-and-registers -->

```json
{
  "request": { "cmd": "temp", "content": "registered content", "prefix": "example.com" },
  "response": { "cmd": "temp", "content": { "$schema": "temp-path" }, "code": 0 }
}
```

This fixture is wire-shape identical to `temp-success`. Its dedicated name documents the side-effect requirement (the returned path MUST be entered into the capability registry , as specified in Section 16) which the corresponding ERT test  asserts.

### B.4 `read` Fixtures

<!-- fixture: read-success -->

```json
{
  "request": { "cmd": "read", "file": "/etc/hosts" },
  "response": { "cmd": "read", "content": { "$schema": "nonempty-string" }, "code": 0 }
}
```

For test purposes, the request's `file` MUST be a path that the test setup configured as readable AND covered by the test's whitelist value. The placeholder for `content` is `nonempty-string`; tests asserting exact file content use a literal `content` value instead.

<!-- fixture: read-open-failure -->

```json
{
  "request": { "cmd": "read", "file": "/nonexistent/path/for/testing" },
  "response": { "cmd": "read", "content": "", "code": 2 }
}
```

The literal empty string `""` in the response's `content` field is significant: it must be present-and-empty, not absent. See Section 5.

<!-- fixture: read-of-registered-temp-path-ok -->

```json
{
  "request": { "cmd": "read", "file": { "$schema": "temp-path" } },
  "response": { "cmd": "read", "content": { "$schema": "nonempty-string" }, "code": 0 }
}
```

The request's `file` is a path previously returned by a `temp` handler call in the same listener lifetime. The path MUST be in the capability registry for the `<TEMP-PATH>` whitelist entry to admit it. Test sequence: call `temp` (registers a path); use the returned path as `read`'s `file` argument; observe success. See the read-handler whitelist-match cases in Section 15.

<!-- fixture: read-after-file-deleted-prune-and-reject -->

```json
{
  "request": { "cmd": "read", "file": { "$schema": "temp-path" } },
  "response": { "cmd": "error", "error": "path not in whitelist" }
}
```

Same setup as `read-of-registered-temp-path-ok`, but the file at `file` was unlinked on disk BEFORE this `read` request. The capability registry's `contains-p` check detects the missing file, prunes the entry, and the gate's whitelist MATCH then fails because the `<TEMP-PATH>` entry no longer resolves to a registered path

<!-- fixture: read-after-listener-restart-reject -->

```json
{
  "request": { "cmd": "read", "file": { "$schema": "temp-path" } },
  "response": { "cmd": "error", "error": "path not in whitelist" }
}
```

Same setup as `read-of-registered-temp-path-ok`, but the listener was stopped and restarted BETWEEN the `temp` registration and this `read` request. The capability registry is cleared on listener stop (Section 16), so the formerly-registered path is no longer in the registry. The whitelist MATCH for `<TEMP-PATH>` therefore fails

<!-- fixture: read-whitelist-rejection -->

```json
{
  "request": { "cmd": "read", "file": "/etc/passwd" },
  "response": { "cmd": "error", "error": "path not in whitelist" }
}
```

A path not covered by any whitelist entry (and not in the capability registry) is rejected with the generic error shape. The wording is the suggested phrasing for whitelist rejections (Section 15).

<!-- fixture: read-tramp-rejection -->

```json
{
  "request": { "cmd": "read", "file": "/ssh:remote-host:/etc/hosts" },
  "response": { "cmd": "error", "error": "remote paths not permitted" }
}
```

The TRAMP-shape path is rejected by the path-expansion helper's TRAMP guard BEFORE the whitelist MATCH runs. No I/O is attempted on a remote path.

<!-- fixture: read-file-too-large -->

```json
{
  "request": { "cmd": "read", "file": { "$schema": "absolute-path" } },
  "response": { "cmd": "error", "error": "file too large to return" }
}
```

The path is whitelisted and exists, but the file's size exceeds the v2 bounded-read cap (the production read cap is set just above the outbound-response cap ; see Section 12 of this document). The handler detects the over-cap raw read and returns the generic error before building a `content` field.

### B.5 `run` Fixtures

<!-- fixture: run-success-zero-exit -->

```json
{
  "request": { "cmd": "run", "command": "echo hello", "content": "" },
  "response": { "cmd": "run", "command": "echo hello", "content": "hello\n", "code": 0 }
}
```

The subprocess's stdout content is captured verbatim per v2's design (`:coding 'binary` and `:stderr nil`). The trailing newline comes from `echo`'s own output, not from any v2-side normalization.

<!-- fixture: run-success-no-trailing-newline -->

```json
{
  "request": { "cmd": "run", "command": "printf abc", "content": "" },
  "response": { "cmd": "run", "command": "printf abc", "content": "abc", "code": 0 }
}
```

`printf abc` writes three bytes with NO trailing newline. v2's raw-byte capture preserves this exactly. Upstream `native_main.nim` would have added a `\n` per its line-by-line output assembly (`native_main.nim:178-182`); v2 intentionally diverges to preserve binary fidelity. This is the canonical fixture for the run-output divergence noted in Section 10.

<!-- fixture: run-success-nonzero-exit -->

```json
{
  "request": { "cmd": "run", "command": "false", "content": "" },
  "response": { "cmd": "run", "command": "false", "content": "", "code": 1 }
}
```

The `false` builtin exits with status 1 and produces no output. `content` is `""` (present-and-empty); `code` is `1`.

<!-- fixture: run-stderr-merged -->

```json
{
  "request": { "cmd": "run", "command": "echo OUT; echo ERR >&2", "content": "" },
  "response": { "cmd": "run", "command": "echo OUT; echo ERR >&2", "content": { "$schema": "nonempty-string" }, "code": 0 }
}
```

stderr is merged into stdout per v2's `:stderr nil`. The interleaving order is process-scheduling-dependent and not part of the contract; `content` is a placeholder rather than a literal so tests assert byte-set membership (both `OUT\n` and `ERR\n` must appear) rather than exact byte order.

<!-- fixture: run-stdin-content -->

```json
{
  "request": { "cmd": "run", "command": "cat", "content": "input data" },
  "response": { "cmd": "run", "command": "cat", "content": "input data", "code": 0 }
}
```

The request's `content` field is written to the subprocess's stdin, then stdin is closed. `cat` echoes the input to stdout.
<!-- fixture: run-success-stderr-only -->

```json
{
  "request": { "cmd": "run", "command": "echo ERR >&2; true", "content": "" },
  "response": { "cmd": "run", "command": "echo ERR >&2; true", "content": "ERR\n", "code": 0 }
}
```

A subprocess that writes only to stderr (`>&2`). Because stderr is merged into stdout per `:stderr nil`, the response's `content` carries the stderr output. The trailing `; true` makes the zero exit code explicit (the shell's exit status is the status of the last command in the pipeline).

<!-- fixture: run-success-binary-output -->

```json
{
  "request": { "cmd": "run", "command": "printf '\\x00\\x01ABC\\xff'", "content": "" },
  "response": { "cmd": "error", "error": "response not encodable" }
}
```

A subprocess that writes a binary byte sequence including NUL bytes and a lone `\xff` (which is not a valid UTF-8 sequence). The bytes are captured raw in memory per Section 10.2's in-memory-capture step, but the response writer's `json-serialize` step signals because Emacs rejects unibyte strings containing bytes outside the UTF-8 range (e.g., `(json-serialize (list :content (unibyte-string 255)))` raises `wrong-type-argument json-value-p`). The response writer catches the signal and replaces the response with the generic error shape, structurally analogous to the oversized-response replacement (Section 11). Tests that need ONLY-valid-UTF-8 binary output (NUL bytes, low control chars) should use a different command-string and assert successful return with `content` matching the byte sequence in its JSON-escaped form (e.g., `\u0000` for NUL); this fixture pins the invalid-UTF-8 case which is the failure surface relevant to the upstream-v2 divergence noted in Section 10. Note: the response writer's serialization-failure path is a small extension to its oversize-replacement logic.


<!-- fixture: run-whitelist-rejection -->

```json
{
  "request": { "cmd": "run", "command": "rm -rf /", "content": "" },
  "response": { "cmd": "error", "error": "command not in whitelist" }
}
```

A request whose `command` does not match any whitelist entry's template is rejected with the generic error BEFORE any subprocess is spawned. The wording `"command not in whitelist"` is the suggested phrasing for whitelist rejections (Section 15).

<!-- fixture: run-overflow-error -->

```json
{
  "request": { "cmd": "run", "command": "head -c $((1024*1024)) /dev/urandom", "content": "" },
  "response": { "cmd": "error", "error": "response too large" }
}
```

The subprocess produced output exceeding the v2 `run`-output cap (default 512 KiB). The signal-escalation helper terminates the subprocess and the run-state machine's `terminal-cause = 'overflow` produces this generic error response. The wording `"response too large"` is shared with the post-serialize oversized-response replacement (Section 11); the distinction between cap-hit-during-capture and cap-hit-after-serialize is internal to v2 and not part of the wire contract.

<!-- fixture: run-timeout-error -->

```json
{
  "request": { "cmd": "run", "command": "sleep 600", "content": "" },
  "response": { "cmd": "error", "error": "run timeout exceeded" }
}
```

The optional run-timeout defcustom is set (in test setup); the subprocess exceeded it; signal-escalation terminated the subprocess; the run-state machine's `terminal-cause = 'timeout` produces this generic error response.

### B.6 Whitelist-Rejection and Validation Fixtures

Three fixtures are named explicitly. The first two are aliases for `run-whitelist-rejection` and `read-whitelist-rejection` above; the third covers the gate-time whitelist-validation failure path.

<!-- fixture: whitelist-rejection-run -->

```json
{
  "request": { "cmd": "run", "command": "/bin/sh -c 'echo not-allowed'", "content": "" },
  "response": { "cmd": "error", "error": "command not in whitelist" }
}
```

Equivalent in semantics to `run-whitelist-rejection`. Named per the explicit fixture-name requirement.

<!-- fixture: whitelist-rejection-read -->

```json
{
  "request": { "cmd": "read", "file": "/var/log/auth.log" },
  "response": { "cmd": "error", "error": "path not in whitelist" }
}
```

Equivalent in semantics to `read-whitelist-rejection`. Named per the explicit fixture-name requirement.

<!-- fixture: whitelist-malformed-gate -->

```json
{
  "request": { "cmd": "read", "file": "/etc/hosts" },
  "response": { "cmd": "error", "error": { "$schema": "error-message" } }
}
```

The configured `firefox-to-emacs-native-messenger-read-whitelist` is malformed (e.g., a list containing `"*"` mixed with another element, violating the strict-sentinel-mixing rule) AND the per-gate-check validator detects this at the gate site. The handler returns the generic error response; the exact wording is whatever the shared validator emits, hence the `error-message` placeholder. The `cmd = "error"` shape is fixed.

### B.7 Cross-handler Error-Shape Fixtures

<!-- fixture: unhandled-message -->

```json
{
  "request": { "cmd": "run_async", "command": "anything" },
  "response": { "cmd": "error", "error": "Unhandled message" }
}
```

The exact wording `"Unhandled message"` is preserved from upstream `native_main.nim:382` per Section 7.2.

<!-- fixture: oversized-response-error -->

```json
{
  "request": { "cmd": { "$schema": "nonempty-string" } },
  "response": { "cmd": "error", "error": "response too large" }
}
```

The handler produced a response whose serialized JSON byte length (after structural null-stripping) exceeds the outbound cap (default 768 KiB). The response builder replaces the over-cap response with this generic error. The fixture's request side is parametric because the failure can arise from any handler that produces large output; test cases that need a specific source use the per-handler fixture instead (notably `read-file-too-large` is the read-side variant).

<!-- fixture: parse-error -->

```json
{ "cmd": "error", "error": { "$schema": "error-message" } }
```

This is a response-only fixture (it uses the response-fixture form per Appendix A.2 rather than the pair form). The triggering request is a frame whose declared length is within the inbound frame cap, but whose body is not parseable as a JSON object: malformed UTF-8 sequences, malformed JSON, and JSON values that are not objects (a JSON array, a JSON scalar) all reach this path. The harness's framed-request sender for this fixture transmits raw bytes prepared per the test, since the request side cannot be expressed within the fixture grammar's JSON-object request form. NOTE: a frame whose LENGTH PREFIX claims MORE bytes than the inbound frame cap allows produces NO response at all (silent close per Section 7.2); that scenario has no fixture because there is nothing to compare against.

### B.8 `getconfig` Fixtures (v2.2)

The fixtures below cover the five response shapes documented in Sections 3.2.7-3.2.11 and the implementation plan's Section 8.24.2-8.24.6.

<!-- fixture: getconfig-success -->

```json
{
  "request": { "cmd": "getconfig" },
  "response": { "cmd": "getconfig", "content": { "$schema": "nonempty-string" }, "code": 0 }
}
```

The returned `content` is the verbatim bytes of the first existing rc candidate (same walker as `getconfigpath` per Section 17), decoded as UTF-8.

<!-- fixture: getconfig-empty -->

```json
{
  "request": { "cmd": "getconfig" },
  "response": { "cmd": "getconfig", "code": 1 }
}
```

Note the ABSENCE of the `content` field. The candidate walker found no existing rc; the handler returns the upstream no-candidate shape (`code = 1`, no `content`). Per Section 6, structural absence is preserved on the wire and is structurally distinct from a present-empty-string `content` (see `getconfig-empty-rc-file` below).

<!-- fixture: getconfig-empty-rc-file -->

```json
{
  "request": { "cmd": "getconfig" },
  "response": { "cmd": "getconfig", "content": "", "code": 0 }
}
```

The candidate walker found an rc file that is exactly 0 bytes. The response carries an explicit empty-string `content` (PRESENT-AND-EMPTY); this shape is STRUCTURALLY DISTINCT from `getconfig-empty` above. Both `code` and `content` come straight from upstream's `readFile` semantics on a zero-byte file.

<!-- fixture: getconfig-open-failure -->

```json
{
  "request": { "cmd": "getconfig" },
  "response": { "cmd": "getconfig", "code": 2 }
}
```

The IOError shape. Upstream fires this when `fileExists` returned true but `readFile` then failed. v2.2 ALSO fires this when the candidate exists but its file-UID differs from the daemon's UID per REQ-4800 / Section 8.24.9 of the implementation plan. Distinct from `read`'s open-failure shape (which carries a present-empty `content` field; see Section 5) because `getconfig` follows upstream's `readFile`-raises path that never assigns `content`.

<!-- fixture: getconfig-oversize -->

```json
{
  "request": { "cmd": "getconfig" },
  "response": { "cmd": "error", "error": "file too large to return" }
}
```

The candidate rc file's raw byte size exceeds the outbound response cap. The bounded-read primitive returns the `:oversize` status; the handler maps it to the generic error shape with the same wording as the `read-file-too-large` fixture (Section 11 / B.7's `oversized-response-error` variant). v2-specific shape with no upstream equivalent.
