# VPM Registry Protocol Design

## Overview

The VPM registry protocol defines how registries communicate with VPM. It enables third-party
registries and private registries. The reference implementation is not connected to `v install`;
the client and transport integration remain separate work.

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
| `GET /config.json` | Registry URLs, auth requirement, and signing public key |

### Authentication Endpoints

| Endpoint | Purpose |
|----------|---------|
| `POST /api/login` | Obtain access token |
| `POST /api/refresh` | Refresh access token |

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

- Token-based authentication for private registries
- `Authorization: Bearer <token>` header
- Tokens obtained via `POST /api/login`
- Read endpoints can be public or require auth (configurable per registry)
- Authenticated HTTP redirects must keep the registry's scheme, host, and effective port;
  unauthenticated requests retain normal redirect handling

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
