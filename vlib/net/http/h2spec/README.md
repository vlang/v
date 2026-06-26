# h2spec conformance harness

Independent RFC 9113 (HTTP/2) and RFC 7541 (HPACK) conformance testing for the
`net.http` HTTP/2 server, using [h2spec](https://github.com/summerwind/h2spec) —
the reference conformance suite (~146 cases). This complements the in-repo unit
tests, which exercise the implementation against its *own* encoder/decoder; h2spec
is an *external* client, so it catches bugs shared between V's encode and decode
sides that internal round-trip tests cannot.

## Files

| File | Purpose |
|------|---------|
| `h2spec_server.v` | Minimal HTTP/2 target server (TLS + ALPN `h2`, answers 200). The thing h2spec connects to. |
| `run_h2spec.sh` | Builds the target, starts it, runs a **pinned** h2spec, diffs failures against the baseline. |
| `h2spec_expected_failures.txt` | Known-failure baseline (37 cases). CI fails only on a *regression* (a new failure not listed). |
| `.github/workflows/h2spec.yml` | CI job: fetches a pinned h2spec release (sha256-verified) and runs the gate. |

## Running locally

```sh
# 1. obtain a PINNED h2spec (no `go install` / @latest — pin a release):
#    https://github.com/summerwind/h2spec/releases  ->  h2spec_<os>_amd64.{tar.gz,zip}
# 2. point the harness at it and at the V compiler:
H2SPEC_BIN=/path/to/h2spec VEXE=./vnew VFLAGS_CC='-cc gcc' \
  bash vlib/net/http/h2spec/run_h2spec.sh
```

It builds `h2spec_server.v`, starts it on `127.0.0.1:$H2SPEC_PORT` (default 18443),
runs `h2spec -t -k` against it, and writes `h2spec_report.xml` +
`h2spec_failed_now.txt`. Exit 0 = no regressions vs the baseline.

## Why pinned, not `go install`

`go install …@latest` is unpinned (results can change with an upstream release),
needs a Go toolchain in the job, and adds supply-chain surface. The CI workflow
downloads a **specific** h2spec release tarball and verifies its `sha256`. The
runner never installs anything — it expects `h2spec` to be provided.

## Status / what is validated

- ✅ `h2spec_server.v` builds (gcc), listens, and negotiates ALPN `h2` (the same
  server driver `test_server_tls_h2_negotiation` exercises end-to-end).
- ✅ **The harness was run end-to-end with a pinned h2spec v2.6.0**: 146 tests,
  **109 pass / 37 fail** against the server at this branch's base. The 37-failure
  set was **identical across repeated runs** (deterministic), and is recorded in
  `h2spec_expected_failures.txt` as the gate baseline. The JUnit parser is
  validated against that real report.
- The baseline reflects genuine server-side conformance gaps (see the file's
  header). Each is a tracked item; remove a line as the server is hardened.
- The baseline was generated locally; the first CI run on Linux validates it. If
  a case drifts by platform, the harness's "now passing" / regression messages
  make the one-line fix obvious.

## Expected value

h2spec independently covers the areas the internal tests cover only piecemeal —
the **stream state machine** (§5.1 transitions, illegal-frame `STREAM_CLOSED`),
frame sequencing, and error semantics — turning "internally tested" into
"externally RFC-validated." It is the missing Stage-3 of the HTTP/2 test plan.
