# VPM registry protocol

This document covers the registry protocol on the `fix/vpm-registry-protocol`
branch. It is implemented in three files:

- `cmd/tools/vpm/registry.v` — routing, the metadata format, ed25519 signing,
  entity-tag caching, the change feed, and artefact storage.
- `cmd/tools/vpm/sbom.v` — the SPDX bill of materials the registry serves.
- `cmd/tools/vpm/registry_server.v` — the HTTP server, and the `registry`
  subcommand of the vpm tool.

A VPM registry, as built here, is an HTTP service that reads one directory tree
and answers questions about the module versions in it: which versions exist,
what each one declares, where its archive is, what changed since a given moment,
what the whole set hashes to, and which signing key vouches for it. There is no
database, no user account and no replication. Everything the server serves comes
from `index.json` and the archives beside it, both read at startup.

## Status: reference implementation, not a client path

**`v install` does not use this protocol, and nothing in the toolchain does.**

`v install` still resolves a module through `get_mod_vpm_info` in
`cmd/tools/vpm/common.v`, which issues `GET <server>/api/packages/<name>` and
then clones the VCS URL named in the reply. The routes listed below are not that
route. Counting the call sites: `serve`, `route` and `handle_request` have no
caller in `cmd/tools/vpm` outside `registry_server.v` and the tests, and
`/api/packages/<name>` is not served by the router at all.

`v registry` is not reachable from the `v` frontend either. `registry` is listed
in `valid_vpm_commands` in `cmd/tools/vpm/vpm.v` and in the command list at
`cmd/v/v.v:172`, but it is absent from `external_commands` there and, above all,
from the list `find_command` scans at `cmd/v/v.v:349`. `main` finds the
subcommand through `find_command`, so for `v registry serve` it returns no
command at all, the dispatch at `cmd/v/v.v:171` is never reached, and the two
words fall through to the compiler as input paths. `run_external_tool` has no
`registry` arm either, so even with `find_command` fixed it would look for a
`cmd/tools/vregistry` tool that does not exist. Measured on this checkout with
a `v.exe` built from it:

```
$ v registry serve --port 9099
multiple input paths are not supported: `registry` and `serve`
```

## Endpoints

Every route is matched by `handle_request` in `registry.v`, except the archive
route, which `artifact_request` answers before it. All of them are `GET`: each
arm compares `method == 'GET'`, so any other method falls through to the
not-found body. The server sets `Content-Type: application/json` on any
non-empty body and `application/zip` on an archive.

| Method | Path | Returns |
| --- | --- | --- |
| `GET` | `/config.json` | `RegistryConfig` JSON |
| `GET` | `/<module>/@v/list` | JSON array of version strings, highest first |
| `GET` | `/<module>/@v/<version>.info` | `ModuleInfo` JSON for one version |
| `GET` | `/<module>/@latest` | `ModuleInfo` JSON, highest non-yanked version |
| `GET` | `/<module>/@v/<version>.zip` | the published archive, `application/zip` |
| `GET` | `/api/search?q=<query>` | JSON array of `RegistryEntry` |
| `GET` | `/api/modules/<module>` | `RegistryEntry` JSON, every version |
| `GET` | `/api/changes?since=<unix>` | JSON array of `Change` |
| `GET` | `/sbom.spdx.json` | SPDX 2.3 document, one package per version |
| `GET` | `/signature.sig` | JSON string, hex ed25519 signature over the index |

The fields:

- `/config.json` serves `dl`, `api`, `auth_required` and `public_key`. `dl` and
  `api` are the fixed strings `https://example.com/downloads` and
  `https://example.com/api`, `auth_required` is always `false`, and `public_key`
  is `Registry.public_key_hex()`: the registry's own key, or `""` when it holds
  no signing key.
- `/<module>/@v/list` returns versions sorted by semver, descending. An unknown
  module answers `200` with `[]`, not `404`.
- `/<module>/@v/<version>.info` returns one `ModuleInfo`: name, version,
  description, license, dependencies, checksum, published_at, features, and
  `yanked`.
- `/<module>/@latest` returns the highest version that is not yanked.
- `/api/search` matches the query against the module **name** only.
- `/api/modules/<module>` returns the whole `RegistryEntry` with every version.
- `/api/changes` takes `since` as a unix timestamp and returns the changes
  recorded at or after it; the mark itself is included. A missing `since` is
  read as `0`, which means everything.
- `/sbom.spdx.json` accepts `namespace` and `dl` as query parameters, defaulting
  to `https://vpm.local/spdx/vpm-registry` and `https://vpm.local/downloads`.
- `/signature.sig` returns the signature inside a JSON string.

Any path the router does not match, and any module it cannot find, answers the
body `{"error": "not found"}` with status `404`. The router has no other error
body: a 404 is the only failure status it can produce, and it is also the status
for an archive whose file is missing or whose bytes no longer match the index.

## Running a registry

From the checkout root:

```
v run cmd/tools/vpm registry serve
```

It prints the number of modules it loaded and the address it bound:

```
Serving 1 module(s) on port 9090
Listening on http://127.0.0.1:9090
```

Data is stored in `os.getwd()/.vpm-registry`, from `registry_dir()` in
`registry.v`. That directory holds:

- `index.json` — read once, at startup, by `load_registry()`.
- `artifacts/<module>/<version>.zip` — the published archives. `/`, `\` and `:`
  in a version are percent-encoded so it can be a file name.
- `signing.key` — the hex ed25519 seed, read but never served.

The default port is 9090 (`default_registry_port`). The server binds `:9090`,
which is every interface, although the message it prints names loopback.

### The port flag reaches the parser

`vpm_registry` looks for `--port` or `-p` in `query[1..]`, and `parse_query_args`
in `cmd/tools/vpm/vpm.v` now hands it the arguments of the `registry` subcommand
untouched. The strip it applies to every other subcommand — drop any argument
beginning with `-`, and drop the value that follows an option that takes one —
is right for module names and wrong here, where the only arguments are options.
`-p` is on the shared value-option list because `v update -p <pkg>` means a
package, so it was dropped along with its value and the port stayed at its
default. `-m <url>` before `registry` is still skipped, so a mirror cannot be
read as a registry argument.

```
$ v run cmd/tools/vpm registry serve --port 9099
Serving 0 module(s) on port 9099
Listening on http://127.0.0.1:9099
```

A value that is not a port number is refused rather than read as 0, which would
ask the operating system for a port of its own choosing and serve on one nobody
named.

### Environment variables

- `VPM_REGISTRY_KEY` — a hex-encoded ed25519 seed. It takes precedence over
  `signing.key`; the seed must be exactly 32 bytes or the registry is treated as
  unsigned. With neither, `signature()` returns `""`.
- `VPM_TOKEN_<HOST>` and `VPM_TOKEN` — bearer tokens for **client** requests, not
  for the server. `registry_token` in `common.v` builds the scoped name from the
  host, replacing `.` and `-` with `_` and uppercasing it, so
  `https://vpm.example.com/a` reads `VPM_TOKEN_VPM_EXAMPLE_COM`, and falls back
  to `VPM_TOKEN` when that is unset. The registry server reads neither: it
  authenticates nothing.

## Publishing

`Registry.publish(info ModuleInfo, archive_path string) !` in `registry.v`
records a version and stores its archive. There is no HTTP route that reaches
it, and no non-test code calls it: a registry is populated either by a test or
by writing `index.json` and the archive files directly.

What `publish` does:

- reads the archive and computes its sha256;
- when `info.checksum` is not empty, it must equal that digest once a `sha256:`
  or `SHA256:` prefix is stripped, or the call fails;
- when `info.checksum` is empty, the digest of the bytes is recorded as
  `sha256:<digest>`, so the entry always carries a hash that vouches for
  something;
- refuses to replace a version that is already published, unless the checksum is
  unchanged, in which case the call is a no-op and records no change;
- writes the archive to `artifacts/<module>/<version>.zip`;
- appends a `publish` entry to the change log.

`publish` does not persist the index, and neither does anything else: `save()`
has no caller in this branch, and the server reads `index.json` once at startup.
A registry's contents are therefore fixed for the lifetime of the process, and
replacing `index.json` underneath a running server has no effect until restart.

## Signing

- Algorithm: ed25519, from `crypto.ed25519`. The key is a 32-byte seed, hex
  encoded, loaded from `VPM_REGISTRY_KEY` when set and from `signing.key`
  otherwise.
- The signed bytes are `canonical_json()`, which is `json2.encode` of
  `SignedIndex{ modules }`.
- `SignedIndex` holds the module map and nothing else. **The change log is
  deliberately excluded**, because `Change.occurred_at` is wall-clock time:
  including it would make an unchanged registry produce a different signature on
  every run, even though the metadata being signed had not moved. A test in
  `registry_feed_test.v` asserts that a longer log leaves the signature
  unchanged.
- The signature is hex encoded, and is served at `GET /signature.sig` inside a
  JSON string. Measured with no key configured, that endpoint's body is `""`,
  two characters.
- An unsigned registry also reports `public_key_hex()` as `""`.
- `verify_signature(public_key_hex, sig_hex)` is the check a client runs; it has
  no caller in the tool.
- `/config.json` serves the key as `public_key`, so a client learns which key
  vouches for the registry from the same document that tells it where the API
  is, rather than having to be given the key out of band.

## Caching

`serve` computes the entity tag of the body it is about to return — the sha256 of
the body, in the double quotes the HTTP grammar requires — and replies `304`
with no body when `If-None-Match` names the same entity. `etag_matches` accepts
a comma-separated list of tags, a `W/` weak prefix and `*`.

Measured against `/demo/@latest`:

```
GET /demo/@latest                                 -> 200, ETag "e2629a20...", 249 bytes
GET /demo/@latest  If-None-Match: "e2629a20..."   -> 304, ETag "e2629a20...", 0 bytes
GET /demo/@latest  If-None-Match: "deadbeef"      -> 200, ETag "e2629a20...", 249 bytes
```

Two details:

- The archive route never carries an entity tag and never answers a conditional
  request, because an archive is streamed from disk and there is no body to hash.
  Measured: `GET /demo/@v/1.0.0.zip` returned `application/zip`, 28 bytes, and
  no `ETag` header.
- A `404` carries an entity tag as well, since the not-found body is hashed like
  any other body.

## Not implemented yet

- No HTTP route publishes, yanks or unyanks. Those are methods on `Registry`,
  and `yank` and `unyank` have no caller outside the tests.
- No code calls `publish`, and no code calls `save()`, so the tool never writes
  an index.
- `v registry` is still not routed by the `v` frontend; the server has to be
  started through `v run cmd/tools/vpm` or from a built binary. `registry` is
  named in the dispatch list at `cmd/v/v.v:172`, but `find_command` at
  `cmd/v/v.v:349` does not return it, so that dispatch is never reached.
- `config.json` still returns fixed `https://example.com/...` values for `dl`
  and `api` and a fixed `auth_required: false`; only `public_key` comes from the
  registry's own state. No client reads the document.
- `RequestOptions.body` is dead: `serve` calls `handle_request` without options,
  so no request body is routed anywhere.
- `compute_checksum` in `registry.v` is unused; the compiler reports it as a
  notice when the tool is built.
- `search` compares the query against the module name only, although its doc
  comment says name or description.
- The SPDX `download_location` is `<dl>/<name>/<version>.zip`, which is not the
  shape of the archive route `/<module>/@v/<version>.zip`.
- The registry server enforces no authentication whatsoever.
- Nothing consumes the change feed. `changes_since` exists for a mirror to call,
  and no mirror is implemented.
- `/api/packages/<name>` and `/api/packages/<name>/incr_downloads`, the two
  routes `v install` does use, are not served.
