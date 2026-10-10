# VPM registry protocol

This document defines the wire protocol a V package registry is expected to
serve. It is implemented in two files:

- `cmd/tools/vpm/registry.v` — routing, the metadata format, ed25519 signing,
  entity-tag caching, the change feed, and artefact storage.
- `cmd/tools/vpm/sbom.v` — the SPDX bill of materials a version can declare.

An HTTP server that serves it, a client that speaks it, and the `v install`
change that asks one are separate and later. Until they land, nothing in the
tree listens for or issues these requests.

A VPM registry, as defined here, is an HTTP service that answers questions
about the module versions in one directory tree: which versions exist, what
each one declares, where its archive is, what changed since a given moment,
what the whole set hashes to, and which signing key vouches for it. There is no
database, no user account and no replication. Everything a registry serves
comes from `index.json` and the archives beside it, with metadata read at
startup and archive bytes verified on each download.

This document defines the wire protocol. It is not a description of a working
server: the change that lands it adds the data types, the on-disk
`.vpm-registry` store and the metadata format, and nothing that listens on a
socket. Serving it, signing it and resolving against it are separate changes,
planned in that order.

## Status: a data definition, not yet a metadata source

**Nothing consumes this protocol yet, and `v install` is unchanged.**

`v install` still resolves a registered module through `get_mod_vpm_info` in
`cmd/tools/vpm/common.v`, issuing `GET <server>/api/packages/<name>` and
cloning the VCS url named in the reply. No code in the tree requests a registry
route. Nothing in this change touches that path.

What is here is the vocabulary. `registry.v` defines the index structure and
the metadata a version carries; `sbom.v` defines the bill of materials one can
declare. A directory rather than a database is the deliberate choice: it means
a registry can be a static tree behind anything, which is why these files
compile against upstream master with no companion change and no server to
drag in.

The compiler frontend dispatches `./v registry serve` to the VPM tool. The server is also
available through `./v run cmd/tools/vpm registry serve` or a compiled VPM binary.

The client helpers `fetch_registry_versions`, `fetch_registry_latest`, and
`fetch_registry_info` read these routes and return version strings or `RegistryModule` metadata.
An unknown module has an empty version list; absent metadata and unsuccessful HTTP responses
return errors.

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
  no signing key. `dl` points at a documentation domain, so an archive base
  must not be assumed to exist until a real one is configured.
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
./v registry serve
```

It prints the number of modules it loaded and the address it bound:

```
Serving 1 module(s) on port 9090
Listening on http://127.0.0.1:9090
```

Data is stored in `os.getwd()/.vpm-registry`, from `registry_dir()` in
`registry.v`. That directory holds:

- `index.json` — read once, at startup, by `load_registry()`.
- `artifacts/<module>/<version>.zip` — the published archives. Names and versions must be
  nonempty single path components without slashes, backslashes, colons, control separators,
  trailing spaces or trailing dots.
- `signing.key` — the hex ed25519 seed, read but never served.

The default port is 9090 (`default_registry_port`). A server binds `:9090`,
which is every interface, although a startup message that names loopback is
misleading. No server lands in this change.

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
records a version and stores its archive. No HTTP route reaches it and no code
outside the tests calls it: a registry is populated either by a test or by
writing `index.json` and the archive files directly.

What `publish` does:

- reads the archive and computes its sha256;
- when `info.checksum` is not empty, it must equal that digest once a `sha256:`
  or `SHA256:` prefix is stripped, or the call fails;
- records every accepted digest spelling as `sha256:<digest>`, including an empty checksum,
  while retaining the publication's other metadata;
- refuses to replace a version that is already published, unless the checksum is
  unchanged after normalization, in which case the call is a no-op and records no change;
- writes the archive to `artifacts/<module>/<version>.zip`;
- appends a `publish` entry to the change log.

`publish` does not persist the index, and neither does anything else: `save()`
has no caller. A registry's contents are therefore fixed for the lifetime of
the process, and replacing `index.json` underneath a running server has no
effect until restart.

## Signing

- Algorithm: ed25519, from `crypto.ed25519`. The key is a 32-byte seed, hex
  encoded, loaded from `VPM_REGISTRY_KEY` when set and from `signing.key`
  otherwise.
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
- `verify_signature(public_key_hex, sig_hex)` is the check a client runs; it has
  no caller in the tool.
- `/config.json` serves the key as `public_key`, so a client learns which key
  vouches for the registry from the same document that tells it where the API
  is, rather than having to be given the key out of band.

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

- The archive route never carries an entity tag and never answers a conditional
  request, because an archive is streamed from disk and there is no body to hash.
  Measured: `GET /demo/@v/1.0.0.zip` returned `application/zip`, 28 bytes, and
  no `ETag` header.
- A `404` carries an entity tag as well, since the not-found body is hashed like
  any other body, but a matching conditional request still returns 404.
- Archive transports verify the exact bytes sent against indexed metadata, even if the file
  changes after routing. Unknown metadata and missing or mismatched checksums refuse the archive.
- Request paths retain their percent-encoded spelling; routing removes trailing slashes.

## SPDX output

The SBOM uses the [SPDX 2.3 JSON field names][spdx-schema], including `SPDXID`, `spdxVersion`,
`downloadLocation` and `checksumValue`. Package IDs encode both name and version without collisions.
Metadata-only packages have `filesAnalyzed: false`; an unspecified license is `NOASSERTION`.
Checksums of successfully published archives appear in the document regardless of the submitted
checksum spelling.

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
- `/api/packages/<name>` and `/api/packages/<name>/incr_downloads`, the two
  routes `v install` does use, are not served.
