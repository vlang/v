# VPM Registry Protocol Design

## Overview

The VPM registry protocol defines how registries communicate with VPM. It enables third-party
registries and private registries. The reference implementation is not connected to `v install`;
the client integration remains separate work.

## Running the Reference Server

Run the server from the repository root:

```sh
./v registry serve --port 9090
```

The standalone tool also accepts `vpm registry serve [--port <port>]`, with `-p` as a port alias.
Ports use decimal digits and must be from 1 to 65535; the default is 9090. The server loads
`.vpm-registry/index.json` and artifacts from the working directory. It binds `0.0.0.0`, all IPv4
interfaces, and its startup message states that it performs no authentication. Other hosts able
to reach the port can read its metadata and archives.
Publishing and yanking remain in-process `Registry` methods.
Only a missing index starts an empty registry. An unreadable or invalid existing index aborts
startup with an error instead of serving an empty module list. Repeated version strings within
a module are rejected, including identical duplicates; the same version may occur in other modules.
Invalid configured signing keys also abort startup before binding. An empty or malformed
`VPM_REGISTRY_KEY` does not fall back to `signing.key`. Unsigned operation requires an unset
environment variable and a genuinely missing key file.
The server binds its listener before printing the startup messages. A bind failure reports the
underlying error and exits with a nonzero status.

Unknown routes return HTTP 404, including conditional requests for missing metadata.
Successful GET metadata supports ETag revalidation with case-insensitive HTTP header names.
Archive transports verify the exact bytes sent against recorded metadata.
Paths retain percent-encoded spelling; the server removes trailing slashes before routing.
Query keys and values follow `urllib.parse_query`: percent escapes are decoded and `+` means space.
Query parsing errors return HTTP 400.

## Design Goals

- **CDN-compatible**: No query parameters, just GET on predictable paths (like Go's proxy protocol)
- **Content-addressed artifacts**: Immutable once published, like Zig's hash-as-version or Go's
  immutable zips
- **Scalable**: Per-file fetch, not full clone (like Cargo's sparse protocol)
- **Signed registry**: Verification against an independently pinned origin key
- **Checksum transparency log**: Append-only Merkle tree for all published hashes, like Go's
  sum.golang.org
- **Private registries**: Token-based authentication (like Cargo or Packagist)
- **SBOM/SPDX output**: Supply chain security

## API Endpoints

### Core Endpoints (Go-style, CDN-compatible)

| Endpoint | Purpose |
|----------|---------|
| `GET /<module>/@v/list` | List all known versions of a module |
| `GET /<module>/@v/<version>.info` | JSON metadata for a specific version |
| `GET /<module>/@v/<version>.mod` | The `v.mod` file for that version |
| `GET /<module>/@v/<version>.zip` | The full module source as a zip file |
| `GET /<module>/@latest` | Latest version info |

### Registry Configuration (Cargo sparse-style)

| Endpoint | Purpose |
|----------|---------|
| `GET /config.json` | Registry URLs, the `auth_required` flag, and signing public key |

### Authentication Endpoints

There are none. No route in this repository accepts or issues credentials, so there is nothing to
log in to and no token to obtain from one. What the client sends, and what the server does with it,
is described under Authentication below.

### Planned write routes

| Endpoint | Purpose | Status |
|----------|---------|--------|
| `PUT /api/publish` | Publish a new module version | Planned |
| `PUT /api/yank/<module>/<version>` | Yank a version | Planned |
| `PUT /api/unyank/<module>/<version>` | Unyank a version | Planned |

The reference server does not implement these HTTP write routes; requests to them return 404.
Populate `.vpm-registry/index.json` and `artifacts/<module>/<version>.zip` before starting the
server, following the [index population documentation](../doc/vpm_registry.md#publishing).
Publishing, yanking, and unyanking are currently in-process `Registry` methods; they are not
HTTP operations. Save index changes and restart the server to expose updated metadata.

### Search and Discovery

| Endpoint | Purpose |
|----------|---------|
| `GET /api/search?q=<query>` | Search modules |
| `GET /api/modules/<module>` | Module metadata |
| `GET /api/modules/<module>/versions` | List versions |

## Metadata Format

Per-version entry (JSON):

```json
{
  "name": "mymodule",
  "version": "1.2.3",
  "description": "A sample module",
  "license": "MIT",
  "dependencies": {
    "othermodule": "^1.0.0"
  },
  "checksum": "sha256:abc123...",
  "yanked": false,
  "published_at": "2024-01-01T00:00:00Z",
  "features": {
    "extra": ["othermodule/extra"]
  }
}
```

## Content Addressing

- Each version's source is stored as a zip file
- Published module names and versions must each be one nonempty path component, without separators,
  drive prefixes, `.` or `..` components, trailing dots/spaces, or NUL/line-break characters
- Components use ASCII letters, digits, or `-._~!$&'()*+,;=@[]`, subject to filesystem restrictions.
  URL delimiters, percent escapes, whitespace and other control characters are rejected before
  publishing or importing an index, so the raw `.info` and `.zip` paths remain reachable.
  Imported map keys, entry names, and per-version names must agree; all names and versions
  must satisfy these component rules.
- The zip file's SHA-256 checksum is recorded in the metadata
- Accepted raw, `sha256:`, `SHA256:`, and omitted checksums are stored as `sha256:<verified digest>`
- The checksum is used for integrity verification on download
- Republishing identical bytes verifies the stored archive and repairs missing or corrupt bytes
  without changing existing version metadata, yank state, or change history
- Saving the index uses a unique temporary file in the same directory and an atomic replacement
  after a successful complete write; write or replacement failures preserve the previous index
- Artifact downloads require a recorded version and a matching, nonempty checksum; orphan files
  are never served
- A transparency log records all published checksums

## Registry Signing

- The registry signs its metadata with a private key
- `VPM_REGISTRY_KEY` takes precedence over `signing.key`; configured values must decode to an
  exactly 32-byte ed25519 seed. Invalid or unreadable configured keys stop startup, and only an
  unset variable plus a missing key file select an unsigned registry.
- `config.json` includes the server's public key for discovery, not as a trust root. A mirror can
  replace this key along with the metadata and signature.
- Before verifying a mirror's metadata, obtain the origin's public key through a separate trusted
  channel and pin it in locally managed configuration. Pass that pinned value as
  `trusted_public_key_hex` to `Registry.verify_signature`; never use the mirrored `public_key` as
  the expected key. An origin key change requires an independently authenticated pin update.
- The verification helper checks the supplied registry snapshot against that key and signature.
  The VPM client does not automatically fetch or verify signed snapshots, and has no key-pinning
  CLI. Merely fetching `config.json` and `signature.sig` does not authenticate a mirror.
- Signing sorts module, dependency, and feature map keys; map insertion order does not affect the
  signed bytes. Array order remains significant, and the change log is excluded.

## Yank/Retire Semantics

- Yanked versions are not selected by new resolutions
- Existing lockfiles that reference a yanked version continue to work
- Yanking is reversible (unyank)
- Retired versions are marked as deprecated with a reason

## Caching Strategy

- ETag / Last-Modified headers for conditional requests
- CDN-friendly: all endpoints are GET with predictable paths
- Immutable artifacts cached indefinitely
- Metadata cached with ETag validation

## SBOM/SPDX Output

- `GET /sbom.spdx.json` returns SPDX JSON for the versions in the registry index.
- Query parameters `namespace` and `dl` customize the document namespace and download base URL.
- Without `namespace`, each response gets a fresh `urn:uuid:<UUID>` document namespace. An explicit
  namespace is preserved; the caller must keep it unique for the document and version it identifies.
  This follows the [SPDX namespace requirements][spdx-namespace]. A newly generated default document
  has a new ETag, so it does not revalidate a previous document with HTTP 304.
- Includes license information
- Includes checksums for all packages
- JSON uses the SPDX 2.3 field names, with `filesAnalyzed: false` for registry metadata. Package IDs
  encode both module name and version, so each package version has a distinct identifier.
- Field names and package identifiers follow the [official SPDX JSON schema][spdx-schema].
- Package URLs encode reserved bytes in both the module name and version. For example,
  `acme@internal` at `1.0.0+build` is `pkg:vpm/acme%40internal@1.0.0%2Bbuild`.

[spdx-schema]: https://github.com/spdx/spdx-spec/blob/v2.3.1/schemas/spdx-schema.json
[spdx-namespace]: https://spdx.github.io/spdx-spec/v2.3.1/document-creation-information/

## Change Feed

- `GET /api/changes?since=<timestamp>` returns changes since timestamp
- Enables incremental syncing for mirrors
- Each change is a JSON object with module, version, and change type

## Authentication

Authentication here is entirely a **client-side** concern. The VPM client attaches a bearer token to
its own registry requests. The registry server in this repository reads no credential and
authenticates nothing.

- `Authorization: Bearer <token>` is set by the VPM client only.
- Credentials are attached only to registry protocol requests. Public metadata, search, health,
  download-count requests, and public-server fallback requests receive no registry token.
- Credential-bearing requests require HTTPS; plaintext HTTP is rejected before connecting.
  For local development, `VPM_REGISTRY_ALLOW_INSECURE_LOOPBACK=1` permits authenticated HTTP
  only to the literal address `127.0.0.1`. DNS names and other addresses are excluded.
  This opt-in does not disable certificate validation for HTTPS.
- Credential-bearing HTTPS requests validate the peer's certificate chain and hostname. Set
  `VPM_REGISTRY_CA_FILE` to a trusted PEM CA file for a private CA. An empty setting, missing file,
  or invalid certificate file fails the request; it never disables certificate validation.
- Without that setting, mbedTLS uses an installed system PEM bundle from common Unix locations,
  and reports an error requesting `VPM_REGISTRY_CA_FILE` if none is available. OpenSSL and native
  Windows SChannel use their default trust stores. A per-request CA file requires a Windows build
  with `-d no_vschannel`; VPM rejects this setting on the native HTTP backend, which ignores it.
- The token comes from `VPM_TOKEN_<HOST>`, where `<HOST>` is the registry's hostname with `.` and
  `-` folded to `_` and uppercased, so `https://vpm.example.com/` reads
  `VPM_TOKEN_VPM_EXAMPLE_COM`. `VPM_TOKEN` is the fallback only for the configured `VPM_REGISTRY`
  origin, matching its scheme, hostname and effective port. With `VPM_REGISTRY` unset, this fallback
  is unused. `registry_token` builds the name; `registry_http_get` selects authenticated transport.
- There is no login endpoint, and nothing obtains or issues a token. No route in this repository
  accepts credentials. A token is provisioned in the environment by whoever configures it, which is
  why it is a VPM client variable and not part of the protocol.
- The registry server authenticates nothing. `auth_required` is a field of `RegistryConfig`
  that is hardcoded to `false` and that no code reads, so no
  endpoint can require a credential. It is present in `/config.json` so that a client can parse the
  document, not because anything checks it.
- A registry answering `401` is reported by `require_registry_token` as an error
  naming the variable to set, rather than as an opaque transport failure. This repository's registry
  never answers `401`, so that path is reached against a third-party registry.
- Authenticated HTTP redirects keep the registry's scheme, host and effective port
  (`vpm_registry_redirect`), so `http://` and `https://` are different origins, as
  are different effective ports. Omitted ports match their explicit defaults. A cross-origin request
  fails rather than
  leaking the token to the new target. Unauthenticated requests are unaffected: the check is skipped
  when the request carries no `Authorization` header.

## Migration Path

- Existing VPM registry continues to work
- New protocol is opt-in via registry configuration
- VPM falls back to direct VCS for modules not in any registry
- Gradual migration: registry serves metadata, VCS serves source

## Research Summary

| Pattern | Go | Cargo | Zig | npm | Hex | Packagist |
|---------|-----|-------|-----|-----|-----|-----------|
| SemVer | Yes | Yes | No (hash) | Yes | Yes | Yes |
| Content-addressed | zip | .crate | hash | tarball | tarball | No (VCS) |
| Lockfile | go.sum | Cargo.lock | build.zig.zon | package-lock.json | mix.lock | composer.lock |
| Checksum DB | sum.golang.org | Index cksum | N/A | dist.integrity | Signed registry | N/A |
| Yank/Retire | No | Yes | N/A | Yes (revoke) | Yes (retire) | No |
| CDN-compatible | Yes | Yes (sparse) | N/A | Yes | Yes | Yes |
| Signed registry | No | No | No | No | Yes | No |
| Auth for reads | No | Optional | No | No | Optional | No |
| Auth for writes | N/A | Token | N/A | Token | OAuth2/Key | Token |
| Search | No | Yes | N/A | Yes | Yes | Yes |
| Change feed | No | No | No | No | No | Yes |
