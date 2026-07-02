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
| `h2spec_expected_failures.txt` | Known-failure baseline — currently empty (146/146 pass). |
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
- ✅ **The harness was run end-to-end with a pinned h2spec v2.6.0**: originally
  146 tests, **109 pass / 37 fail**, identical across four runs (`--timeout 2`
  and `--timeout 5`, two each); the JUnit parser was validated against those real
  reports. Three follow-up PRs (#27569, #27589, #27627) closed every baselined
  case; as of #27627 the server passes **all 146 h2spec cases** and
  `h2spec_expected_failures.txt` is empty.
- ⚠️ **Timing flakiness — handled.** A *separate* handful of h2spec cases
  (CONTINUATION sequencing, unknown error codes) wait for the server's GOAWAY/close
  and flake at h2spec's 2s default under load. The runner therefore uses
  `--timeout 5` (`H2SPEC_TIMEOUT`). The lesson: always confirm the failure set is
  stable across repeated runs before trusting a per-case h2spec gate — a single
  run can mis-attribute a flaky timeout as a real failure (or regression).
- The baseline is currently empty, but the mechanism stays in place: a future
  conformance gap is recorded as a new line and tracked until the server is
  hardened to pass it, exactly as the original 37-case baseline was closed out.
- If a case ever drifts by platform, the harness's "now passing" / regression
  messages make the one-line fix obvious.

## Expected value

h2spec independently covers the areas the internal tests cover only piecemeal —
the **stream state machine** (§5.1 transitions, illegal-frame `STREAM_CLOSED`),
frame sequencing, and error semantics — turning "internally tested" into
"externally RFC-validated." It is the missing Stage-3 of the HTTP/2 test plan.
