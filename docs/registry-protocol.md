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
`.vpm-registry/index.json` and artifacts from the working directory and listens on all interfaces.
Publishing and yanking remain in-process `Registry` methods.

Unknown routes return HTTP 404, including conditional requests for missing metadata.
Successful GET metadata supports ETag revalidation with case-insensitive HTTP header names.
Archive transports verify the exact bytes sent against recorded metadata.
Paths retain percent-encoded spelling; the server removes trailing slashes before routing.

## Design Goals

- **CDN-compatible**: No query parameters, just GET on predictable paths (like Go's proxy protocol)
- **Content-addressed artifacts**: Immutable once published, like Zig's hash-as-version or Go's
  immutable zips
- **Scalable**: Per-file fetch, not full clone (like Cargo's sparse protocol)
- **Signed registry**: Mirrors don't need to be trusted (like Hex's signed protobuf registry)
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

### Publishing

| Endpoint | Purpose |
|----------|---------|
| `PUT /api/publish` | Publish a new module version |
| `PUT /api/yank/<module>/<version>` | Yank a version |
| `PUT /api/unyank/<module>/<version>` | Unyank a version |

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
- The zip file's SHA-256 checksum is recorded in the metadata
- Accepted raw, `sha256:`, `SHA256:`, and omitted checksums are stored as `sha256:<verified digest>`
- The checksum is used for integrity verification on download
- Artifact downloads require a recorded version and a matching, nonempty checksum; orphan files
  are never served
- A transparency log records all published checksums

## Registry Signing

- The registry signs its metadata with a private key
- `config.json` includes the registry's signing public key
- Mirrors serve the signed metadata without needing to be trusted
- Clients verify the signature before accepting metadata
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

- `v sbom` command outputs SPDX JSON format
- Includes all transitive dependencies
- Includes license information
- Includes checksums for all packages
- JSON uses the SPDX 2.3 field names, with `filesAnalyzed: false` for registry metadata. Package IDs
  encode both module name and version, so each package version has a distinct identifier.
- Field names and package identifiers follow the [official SPDX JSON schema][spdx-schema].

[spdx-schema]: https://github.com/spdx/spdx-spec/blob/v2.3.1/schemas/spdx-schema.json

## Change Feed

- `GET /api/changes?since=<timestamp>` returns changes since timestamp
- Enables incremental syncing for mirrors
- Each change is a JSON object with module, version, and change type

## Authentication

Authentication here is entirely a **client-side** concern. The VPM client attaches a bearer token to
its own registry requests. The registry server in this repository reads no credential and
authenticates nothing.

- `Authorization: Bearer <token>` is set by the VPM client only.
- The token comes from `VPM_TOKEN_<HOST>`, where `<HOST>` is the registry's hostname with `.` and
  `-` folded to `_` and uppercased, so `https://vpm.example.com/` reads
  `VPM_TOKEN_VPM_EXAMPLE_COM`. `VPM_TOKEN` is the fallback when no scoped variable is set, and
  covers a machine with a single private registry. `registry_token` in
  `cmd/tools/vpm/common.v:149` builds the name; `vpm_http_request` at `common.v:98` attaches the
  header when it is non-empty, so a registry that needs none receives no such header at all.
- There is no login endpoint, and nothing obtains or issues a token. No route in this repository
  accepts credentials. A token is provisioned in the environment by whoever configures it, which is
  why it is a VPM client variable and not part of the protocol.
- The registry server authenticates nothing. `auth_required` is a field of `RegistryConfig`
  (`registry.v:26`) that is hardcoded to `false` (`registry.v:478`) and that no code reads, so no
  endpoint can require a credential. It is present in `/config.json` so that a client can parse the
  document, not because anything checks it.
- A registry answering `401` is reported by `require_registry_token` (`common.v:166`) as an error
  naming the variable to set, rather than as an opaque transport failure. This repository's registry
  answers only `200`, `304` and `404`, so that path is reached against a third-party registry.
- Authenticated HTTP redirects keep the registry's scheme, host and effective port
  (`vpm_registry_redirect`, `common.v:133`), so `http://` and `https://` are different origins, as
  are a default port and an explicit one. A request that would cross an origin fails rather than
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
