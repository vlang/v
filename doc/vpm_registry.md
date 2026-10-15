# VPM registry protocol

This document covers the registry protocol on the `fix/vpm-registry-protocol`
branch. It is implemented in four files:

- `cmd/tools/vpm/registry.v` — routing, the metadata format, ed25519 signing,
  entity-tag caching, the change feed, and artefact storage.
- `cmd/tools/vpm/sbom.v` — the SPDX bill of materials the registry serves.
- `cmd/tools/vpm/registry_server.v` — the HTTP server, and the `registry`
  subcommand of the vpm tool.
- `cmd/tools/vpm/registry_client.v` — metadata and version-list requests using the
  registry's existing token configuration.

A VPM registry, as built here, is an HTTP service that reads one directory tree
and answers questions about the module versions in it: which versions exist,
what each one declares, where its archive is, what changed since a given moment,
what the whole set hashes to, and which signing key vouches for it. There is no
database, no user account and no replication. Everything the server serves comes
from `index.json` and the archives beside it, with metadata read at startup and archive bytes
verified on each download.

## Status: an additional metadata source, not an install path

**`v install` keeps its existing VCS install path.**

`v install` still resolves a registered module through `get_mod_vpm_info` in
`cmd/tools/vpm/common.v`. That loop now asks the registry protocol first, from
the one base `VPM_REGISTRY` names, and then the vpm servers exactly as it did
before: `GET <server>/api/packages/<name>`, then a clone of the VCS url named
in the reply. With `VPM_REGISTRY` unset, no registry route is requested at all
and resolution behaves as it did before a registry could be configured.

A registry is a metadata source and nothing more. `RegistryModule` carries no
repository url, and nothing downloads and unpacks an archive, so a module found
in a registry is reported rather than installed: the resolution records that the
registry holds it, why its archive could not be located, and goes on to the
servers. Counting the call sites: `serve`, `route` and `handle_request` still
have no caller in `cmd/tools/vpm` outside `registry_server.v` and the tests,
and `/api/packages/<name>` is still not served by the router at all.

The compiler frontend dispatches `./v registry serve` to the VPM tool. The server is also
available through `./v run cmd/tools/vpm registry serve` or a compiled VPM binary.

The client helpers `fetch_registry_versions`, `fetch_registry_latest`, and
`fetch_registry_info` read these routes and return version strings or `RegistryModule` metadata.
An unknown module has an empty version list; absent metadata and unsuccessful HTTP responses
return errors. Returned module names must match the request exactly; version-specific metadata
must also match the requested version exactly.
Every module metadata response must include a version.

### How a registry is consulted

`get_mod_vpm_info_with_selector` asks `GET /<name>/@v/list` first and reads an
empty list as "this registry does not hold the module", the way it reads a 404
from a vpm server. That is the rule the protocol forces: an unknown module
answers `200` with `[]` rather than a 404, so treating an empty list as a hit
would pin the first registry that answers at all as the source of a module it
never heard of. A non-empty list is followed by
`GET /<name>/@v/<version>.info` for the highest listed version whose metadata
answers and is not yanked, and then by `GET /config.json` for the archive base.

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
  no signing key. `registry_archive_base` in `registry_client.v` reads `dl` for
  the install path and refuses a base on an RFC 2606 documentation domain, which
  is where that fixed value points.
- `/<module>/@v/list` first orders versions accepted by the existing `semver` parser by
  descending precedence, followed by opaque versions in descending lexical order. Equal-precedence
  spellings also use descending lexical order, including build metadata. Thus `release_2026`
  precedes `release_2025`; this is lexical ordering, so `release_9` precedes `release_10`.
  An unknown module answers `200` with `[]`, not `404`.
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
  to a fresh `urn:uuid:<UUID>` for each document and `https://vpm.local/downloads`.
  An explicit namespace is preserved; its caller is responsible for keeping it unique.
  Package URLs percent-encode reserved bytes in both name and version: `acme@internal` at
  `1.0.0+build` becomes `pkg:vpm/acme%40internal@1.0.0%2Bbuild`.
- `/signature.sig` returns the signature inside a JSON string.

Any path the router does not match, and any module it cannot find, answers the
body `{"error": "not found"}` with status `404`. The router has no other error
body: a 404 is the only failure status it can produce, and it is also the status
for an archive whose file is missing or whose bytes no longer match the index.

## Running a registry

From the checkout root:

```
./v registry serve
```

It prints the number of modules it loaded and the address it bound:

```
Serving 1 module(s) on port 9090
Listening on http://0.0.0.0:9090 (all IPv4 interfaces; no authentication)
```

Data is stored in `os.getwd()/.vpm-registry`, from `registry_dir()` in
`registry.v`. That directory holds:

- `index.json` — read once, at startup, by `load_registry()`.
- `artifacts/<module>/<version>.zip` — the published archives. Names and versions must be
  nonempty single path components without slashes, backslashes, colons, control separators,
  trailing spaces or trailing dots.
  Allowed bytes are ASCII letters, digits and `-._~!$&'()*+,;=@[]`, subject to filesystem limits.
  URL delimiters, percent escapes, whitespace and other control characters are rejected before
  publication because the registry routes preserve raw path spelling.
- `signing.key` — the hex ed25519 seed, read but never served.

Only a missing index initializes an empty registry. Read and JSON decoding failures for an
existing index abort startup and report the error. Imported module keys, entry names, version names,
and versions must satisfy the same component rules as publication. The key and every name within
its entry must agree; inconsistent or unroutable metadata is rejected. Each version string must
occur only once within a module, even when duplicate records have identical metadata.
Module names that differ only by ASCII letter case are rejected, as are versions differing only
by case within one module. Publication enforces the same rules before writing an archive, so
`alpha@1.0.0-ALPHA` and `alpha@1.0.0-alpha` cannot overwrite each other on a case-insensitive
filesystem. The same version remains valid in different, non-colliding modules.

Registry serving does not initialize the VPM client's module directory, cache or home log.
Its data stays in the working directory; serving does not require writable `HOME` or `VMODULES`.

The default port is 9090 (`default_registry_port`). The server binds every IPv4 interface,
reported as `0.0.0.0:9090`. It performs no authentication: other hosts that can reach this port
can read the registry metadata and archives. The startup message states this exposure explicitly.

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
$ ./v registry serve --port 9099
Serving 0 module(s) on port 9099
Listening on http://0.0.0.0:9099 (all IPv4 interfaces; no authentication)
```

Ports must contain decimal digits and be from 1 to 65535. Invalid text, numeric suffixes and
out-of-range values are rejected before the server binds a socket.
Bind errors also exit nonzero and report their cause. The success messages are printed only after
the listener has bound successfully. On Windows the registry disables address reuse and enables
`SO_EXCLUSIVEADDRUSE` before binding, so an occupied port fails instead of starting a duplicate
listener.

### Environment variables

- `VPM_REGISTRY_KEY` — a hex-encoded ed25519 seed. It takes precedence over
  `signing.key`; the seed must be exactly 32 bytes. An empty, malformed, wrong-length, or
  unreadable configured key aborts startup before binding. An explicitly set environment variable
  never falls back to the key file, including when empty or invalid. Only an unset variable and
  genuinely absent key file select unsigned operation, where `signature()` returns `""`.
- `VPM_REGISTRY` — the base url of the registry `v install` asks before the vpm
  servers, read per resolution and normalised like a server url. Unset means
  there is no registry, and then no registry route is requested at all. Unlike a
  selected server, a registry is never recorded: a module found in one is
  reported rather than installed, so there is nothing to prefer next time.
- `VPM_REGISTRY_TOKEN_HOST_<HEX>` and `VPM_TOKEN` — bearer tokens for **client** requests, not
  for the server. The scoped suffix is the uppercase hexadecimal encoding of the lowercased
  hostname's UTF-8 bytes. For example, `vpm.example.com` selects
  `VPM_REGISTRY_TOKEN_HOST_76706D2E6578616D706C652E636F6D`. Distinct hostnames have distinct keys,
  including `foo-bar.example.com` and `foo.bar-example.com`. The old punctuation-to-underscore
  keys are ignored because their mapping could expose a token to a different host.
  An explicitly set empty scoped token disables fallback. When the scoped key is unset,
  `VPM_TOKEN` applies only to the configured `VPM_REGISTRY` origin (scheme,
  hostname and effective port). An unset registry disables the fallback. Public VPM requests,
  including public-server fallback after consulting a registry, receive no registry credentials.
  The registry server reads neither token variable: it authenticates nothing.
  Requests carrying tokens require HTTPS and reject plaintext HTTP before connecting.
- `VPM_REGISTRY_ALLOW_INSECURE_LOOPBACK` — set to exactly `1` to allow authenticated HTTP
  for local development, only at the literal address `127.0.0.1`. The exception
  does not accept DNS names, other addresses or HTTPS certificate failures.
- `VPM_REGISTRY_CA_FILE` — a trusted PEM CA file for all registry HTTPS requests.
  Certificate chains and hostnames are checked even without a bearer token. HTTPS registry
  redirects preserve their scheme; configure HTTPS directly instead of relying on an HTTP upgrade.
  An empty setting, missing file or invalid custom CA
  file fails closed. Without a custom file, mbedTLS searches common
  Unix PEM bundles; OpenSSL and native Windows SChannel use their default roots. A custom
  file requires `-d no_vschannel` on Windows because the native HTTP backend cannot honor it.

## Publishing

`Registry.publish(info ModuleInfo, archive_path string) !` in `registry.v`
records a version and stores its archive. There is no HTTP route that reaches
it, and no non-test code calls it: a registry is populated either by a test or
by writing `index.json` and the archive files directly.

What `publish` does:

- reads the archive and computes its sha256;
- when `info.checksum` is not empty, it must equal that digest once a `sha256:`
  or `SHA256:` prefix is stripped, or the call fails;
- records every accepted digest spelling as `sha256:<digest>`, including an empty checksum,
  while retaining the publication's other metadata;
- refuses to replace a version that is already published, unless the checksum is
  unchanged after normalization. A matching repeat replaces and syncs the verified archive bytes,
  including when already present, so a retry after a directory-sync error re-establishes durability.
  The original metadata, yank state and change log are preserved;
- writes the archive to `artifacts/<module>/<version>.zip`;
- appends a `publish` entry to the change log.

`publish` does not persist the index, and neither does anything else: `save()`
has no caller in this branch, and the server reads `index.json` once at startup.
`save()` first validates module identities, duplicate versions and case collisions using the same
rules as loading. Invalid in-process state is rejected before any directory creation or file
replacement, preserving a previous valid index. This also covers direct changes to `modules`
and duplicate `add_module` calls.
`save()` writes a unique temporary file beside `index.json`, flushes and syncs its contents,
checks its close, and atomically replaces the index. Archive writes use the same replacement path.
On Unix it syncs the containing directory after replacement, and newly created storage directories
and their parents before writing; macOS also requests `F_FULLFSYNC`. Windows syncs the file with
`_commit` and requests `MOVEFILE_WRITE_THROUGH` for replacement; it has no explicit directory
sync in this implementation. Windows directory-creation persistence depends on the filesystem.
Failures before replacement preserve the previous file. A directory-sync failure after replacement
reports that replacement occurred and its durability is uncertain; it cannot restore the old file.
Temporary files start private; existing regular-file permissions are retained on Unix.
Sync errors are reported rather than silently claiming a durable save.
A registry's contents are therefore fixed for the lifetime of the process, and
replacing `index.json` underneath a running server has no effect until restart.

## Signing

- Algorithm: ed25519, from `crypto.ed25519`. The key is a 32-byte seed, hex
  encoded, loaded from `VPM_REGISTRY_KEY` when set and from `signing.key`
  otherwise. Configured keys are validated before the server binds; invalid keys stop startup
  instead of silently selecting unsigned operation.
- The signed bytes are `canonical_json()`, which is `json2.encode` of
  `SignedIndex{ modules }`, with module, dependency and feature map keys sorted. Version and
  feature arrays retain their order, and serialization leaves the registry unchanged.
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
- `verify_signature(trusted_public_key_hex, sig_hex)` checks a registry snapshot against an
  independently trusted key. Obtain the origin key over a trusted channel and pin it in locally
  managed configuration before accepting a mirror's metadata. Update the pin only through an
  independently authenticated process.
- `/config.json` serves `public_key` for discovery only. A malicious mirror can replace its key,
  signature and metadata together; a key taken from that response cannot authenticate the mirror.
  Always pass the locally pinned origin key to `verify_signature`.
- The helper has no caller in the tool. Automatic snapshot verification and a client key-pinning
  CLI are not implemented; fetching the configuration and signature alone provides no mirror
  authentication.

## Caching

`serve` computes the entity tag of the body it is about to return — the sha256 of
the body, in the double quotes the HTTP grammar requires — and replies `304`
with no body for a successful GET when `If-None-Match` names the same entity.
Header names
are case-insensitive. Conditional requests for missing metadata retain HTTP 404.
`etag_matches` accepts
a comma-separated list of tags, a `W/` weak prefix and `*`.

Measured against `/demo/@latest`:

```
GET /demo/@latest                                 -> 200, ETag "e2629a20...", 249 bytes
GET /demo/@latest  If-None-Match: "e2629a20..."   -> 304, ETag "e2629a20...", 0 bytes
GET /demo/@latest  If-None-Match: "deadbeef"      -> 200, ETag "e2629a20...", 249 bytes
```

Two details:

- The archive route never carries an entity tag and never answers a conditional request.
  Routing reads and hashes the archive once, then passes that verified binary buffer to the HTTP
  response without reopening the file or hashing it again for an ETag. Downloads are buffered in
  memory; this implementation does not stream archives or promise constant memory per request.
- A `404` carries an entity tag as well, since the not-found body is hashed like
  any other body, but a matching conditional request still returns 404.
- Archive transports verify the exact bytes sent against indexed metadata, even if the file
  changes after routing. Unknown metadata and missing or mismatched checksums refuse the archive.
- Request paths retain their percent-encoded spelling; routing removes trailing slashes.
- Query keys and values follow `urllib.parse_query`: percent escapes are decoded and `+` means
  space. Query parsing errors return HTTP 400 before routing.

## SPDX output

Request `GET /sbom.spdx.json` from the running registry to obtain the document. The `namespace`
and `dl` query parameters customize its namespace and download base. There is no `v sbom` command.

The SBOM uses the [SPDX 2.3 JSON field names][spdx-schema], including `SPDXID`, `spdxVersion`,
`downloadLocation` and `checksumValue`. Package IDs encode both name and version without collisions.
Metadata-only packages have `filesAnalyzed: false`; an unspecified license is `NOASSERTION`.
Valid SHA-256 checksums appear for published archives and imported index entries alike: raw
64-character lowercase digests and the `sha256:` and `SHA256:` prefixes are accepted.
Invalid or missing checksum values are omitted.
CPE product and version components quote literal wildcard and punctuation characters according to
CPE 2.3 formatted-string rules. If a component cannot safely be represented, including empty,
non-ASCII, whitespace, or a whole `-`, the CPE reference is omitted; package URLs remain present.
Versions containing build metadata (`+`) also omit the CPE reference.

[spdx-schema]: https://raw.githubusercontent.com/spdx/spdx-spec/v2.3.1/schemas/spdx-schema.json

## Not implemented yet

- No HTTP route publishes, yanks or unyanks. Those are methods on `Registry`,
  and `yank` and `unyank` have no caller outside the tests.
- No code calls `publish`, and no code calls `save()`, so the tool never writes
  an index.
- `config.json` still returns fixed `https://example.com/...` values for `dl`
  and `api` and a fixed `auth_required: false`; only `public_key` comes from the
  registry's own state. The install path reads `dl` and refuses the placeholder,
  but nothing configures it.
- No archive download. A module found in a registry is reported, not installed:
  `RegistryModule` carries no repository url, and nothing downloads and unpacks
  the archive. The resolution names the registry, the version and the archive
  base it would have needed, which is the difference an operator can act on.
- `list_versions` includes yanked versions, so the client walks the list from
  the top and takes the first version whose metadata answers and is not yanked.
  A registry whose every version is yanked is reported as holding no installable
  one.
- `RequestOptions.body` is dead: `serve` calls `handle_request` without options,
  so no request body is routed anywhere.
- `compute_checksum` in `registry.v` is unused; the compiler reports it as a
  notice when the tool is built.
- `search` compares the query against the module name only, although its doc
  comment says name or description.
- The SPDX `downloadLocation` is `<dl>/<name>/<version>.zip`, which is not the
  shape of the archive route `/<module>/@v/<version>.zip`.
- The registry server enforces no authentication whatsoever.
- Nothing consumes the change feed. `changes_since` exists for a mirror to call,
  and no mirror is implemented.
- No checksum transparency log or Merkle tree is implemented. The ordinary change feed contains
  module, version, kind and time; it has no checksum, tree root, inclusion proof or tamper-evident
  history. It cannot audit published hashes.
- `GET /api/modules/<module>/versions` is planned and currently returns 404. Use the implemented
  `GET /<module>/@v/list` route instead.
- `GET /<module>/@v/<version>.mod` is planned and currently returns 404; the server neither
  stores nor generates a separate `v.mod` response.
- `/api/packages/<name>` and `/api/packages/<name>/incr_downloads`, the two
  routes `v install` does use, are not served.
