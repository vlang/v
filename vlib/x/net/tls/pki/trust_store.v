// Copyright (c) 2026 blackshirt.
//
// trust_store.v loads and indexes trusted root CA certificates, seeding the
// verifier's chain-of-trust check from the host's system trust bundle. The
// @[has_globals] attribute opts in the module-level system-roots cache below.
@[has_globals]
module pki

import crypto.pem
import os
import sync

// TrustStore stores trusted root CA certificates. Instances come from
// new_trust_store(); @[noinit] stops a bare TrustStore{} from silently existing
// with a garbage internal slice.
@[noinit]
pub struct TrustStore {
mut:
	certs []Certificate // trusted roots, checked against the chain in order
}

// new_trust_store creates an empty TrustStore.
pub fn new_trust_store() TrustStore {
	return TrustStore{
		certs: []Certificate{}
	}
}

// pem_begin_marker matches crypto.pem's internal delimiter; used to split an
// incoming bundle into per-block chunks so one malformed envelope isolates to
// its own block instead of aborting the scan.
const pem_begin_marker = '-----BEGIN '

// max_pem_blocks_per_bundle bounds work per add_pem call. Real system trust
// bundles carry ~150 roots; anything larger is either hostile or pathological.
const max_pem_blocks_per_bundle = 1024

// max_pem_bundle_bytes caps the input a single add_pem call may inspect.
// Debian's ca-certificates.crt is ~220 KB; 2 MB leaves room for enterprise
// bundles while bounding the split-time allocation to a known ceiling. The
// check runs BEFORE split so a huge file never reaches the heap.
const max_pem_bundle_bytes = 2 * 1024 * 1024

// The per-block DER ceiling is owned by x509.v as `max_certificate_der_bytes`;
// add_pem below references it directly so the loader cap and the parser cap
// stay in lock-step (a block that would pass add_pem's size filter and then
// trip parse_certificate's own cap would produce a confusing double guard).

// cached_system_roots memoizes the parsed system trust bundle so dial() does
// not re-read a ~150-root file on every connection. The mutex guards the slice
// header against concurrent first dials; system_root_cas() hands each caller a
// private clone, so mutating the returned store cannot corrupt the cache.
__global cached_system_roots []Certificate
__global cached_system_roots_mu sync.Mutex

// add_der parses and appends a single DER-encoded certificate into the store.
pub fn (mut ts TrustStore) add_der(der []u8) ! {
	cert := parse_certificate(der)!
	ts.certs << cert
}

// add_pem parses and appends every X.509 certificate from a PEM string. PEM
// grammar is delegated to crypto.pem; non-CERTIFICATE blocks (CRLs, private
// keys, PKCS #7) are filtered before DER parsing. Blocks are isolated so one
// malformed envelope costs only itself — every later anchor in a 150-root
// system bundle still loads. The bounds fail as follows:
//   - total input bytes  > max_pem_bundle_bytes        → error
//   - block count        > max_pem_blocks_per_bundle   → error
//   - single DER size    > max_certificate_der_bytes   → block skipped
// The first two fail CLOSED (never a silent partial load, so an operator can't
// end up with a short trust store); the DER cap is per-block because an
// over-long envelope is a hostile input, not a missing anchor.
pub fn (mut ts TrustStore) add_pem(pem_content string) !int {
	// Bound the input BEFORE split so a hostile file cannot drive an oversized
	// allocation via the chunk vector.
	if pem_content.len > max_pem_bundle_bytes {
		return error('trust_store: PEM bundle of ${pem_content.len} bytes exceeds ${max_pem_bundle_bytes} cap')
	}
	mut count := 0
	// Split on the BEGIN marker: chunks[0] is prose before the first marker
	// (usually empty); every subsequent element gets the marker restored
	// before decode so crypto.pem sees a well-formed block each iteration.
	// Safe because `-` is not in the base64 alphabet, so the marker cannot
	// legitimately appear inside a well-formed PEM body.
	mut chunks := pem_content.split(pem_begin_marker)
	if chunks.len <= 1 {
		return 0
	}
	chunks = chunks[1..]
	if chunks.len > max_pem_blocks_per_bundle {
		// Fail closed: silently truncating the tail here would leave the trust
		// store looking healthy while every anchor past the cap is missing.
		return error('trust_store: PEM bundle declares ${chunks.len} blocks (> cap ${max_pem_blocks_per_bundle})')
	}
	for chunk in chunks {
		block_text := pem_begin_marker + chunk
		blk := pem.decode_only(block_text) or { continue }
		if blk.block_type != 'CERTIFICATE' {
			continue
		}
		if blk.data.len == 0 || blk.data.len > max_certificate_der_bytes {
			// Empty or absurdly large DER: never hand this to parse_certificate.
			continue
		}
		cert := parse_certificate(blk.data) or { continue }
		ts.certs << cert
		count++
	}
	return count
}

// load_system_ca_bundle attempts to load trusted root CAs from standard system
// paths. There is no portable trust-store API across Linux distributions, so each
// known bundle location is probed and the first that yields certificates wins.
pub fn (mut ts TrustStore) load_system_ca_bundle() !int {
	paths := [
		'/etc/ssl/certs/ca-certificates.crt', // Debian/Ubuntu/Gentoo
		'/etc/pki/tls/certs/ca-bundle.crt', // Fedora/RHEL/CentOS
		'/etc/ca-certificates/extracted/tls-ca-bundle.pem', // Arch Linux
		'/etc/ssl/ca-bundle.pem', // OpenSUSE
		'/etc/pki/tls/cacert.pem', // OpenELEC
		'/usr/local/share/certs/ca-root-nss.crt', // FreeBSD
		'/etc/ssl/cert.pem', // Alpine / macOS
	]

	for path in paths {
		if os.exists(path) {
			// Stat-check BEFORE read_file so an oversized / mislabelled bundle
			// does not allocate a huge in-memory buffer for a file we intend to
			// reject anyway. os.file_size returns a negative value on stat failure;
			// skipping the path (rather than erroring) lets the loop fall through
			// to the next known bundle location.
			size := os.file_size(path)
			if size < 0 || size > max_pem_bundle_bytes {
				continue
			}
			content := os.read_file(path) or { continue }
			count := ts.add_pem(content) or { continue }
			if count > 0 {
				return count
			}
		}
	}
	return error('trust_store: no system root CA bundle found')
}

// system_root_cas returns a TrustStore backed by the process-wide parsed
// system bundle, parsing it on first use. dial() uses this instead of
// load_system_ca_bundle() so an application that opens many connections pays
// the bundle parse once per process rather than once per connection. The
// returned store is a private clone, safe for the caller to mutate.
pub fn system_root_cas() !TrustStore {
	cached_system_roots_mu.lock()
	// defer (not unlock-after-success): a bundle load error must still release
	// the mutex, or the failed first dial would deadlock every later one.
	defer {
		cached_system_roots_mu.unlock()
	}
	if cached_system_roots.len == 0 {
		mut loader := new_trust_store()
		loader.load_system_ca_bundle()!
		cached_system_roots = loader.certs
	}
	mut ts := new_trust_store()
	ts.certs = cached_system_roots.clone()
	return ts
}
