@[has_globals]
module tls

import os
import crypto.pem
import crypto.x509

pub const error_chain_untrusted = 'tls: error_chain_untrusted'
pub const error_chain_expired = 'tls: error_chain_expired'
pub const error_hostname_mismatch = 'tls: error_hostname_mismatch'
pub const error_bad_signature = 'tls: error_bad_signature'

// CertStore holds trusted root certificates for chain verification (direct-root only in v1).
pub struct CertStore {
pub:
	roots []x509.Certificate
}

// embedded_cacert_pem is the Mozilla CA bundle vendored at
// vlib/net/tls/cacert.pem (provenance and refresh notes in README.md).
// It gives every OS the same trust roots with zero C interop, so chain
// verification works on Windows and macOS as well as Unix.
const embedded_cacert_pem = $embed_file('cacert.pem')

// embedded_roots is the parsed Mozilla bundle, built once at program startup
// (before any thread spawns) so handshakes reuse it without reparsing ~120
// certificates — or racing a lazy initializer — on every connection.
__global embedded_roots = parse_bundle_pem(embedded_cacert_pem.to_string())

// parse_bundle_pem extracts all CERTIFICATE blocks from PEM text, skipping
// blocks that fail to parse (a system bundle must never fail hard on one
// unexpected entry).
fn parse_bundle_pem(text string) []x509.Certificate {
	mut roots := []x509.Certificate{}
	mut rest := text
	for {
		block, next := pem.decode(rest) or { break }
		if block.block_type == 'CERTIFICATE' {
			cert := x509.parse_der(block.data) or {
				rest = next
				continue
			}
			roots << cert
		}
		rest = next
		if rest.len == 0 {
			break
		}
	}
	return roots
}

// load_bundle_file reads one PEM bundle file, returning its certificates
// (empty when the file is missing or holds no parsable certificates).
fn load_bundle_file(path string) []x509.Certificate {
	txt := os.read_file(path) or { return []x509.Certificate{} }
	return parse_bundle_pem(txt)
}

// explicit_roots resolves Config trust settings to roots: ca_store wins,
// then the ca_cert bundle file, then ca_cert_pem bytes. Empty when nothing
// is configured (verify_peer_chain then falls back to system roots).
fn explicit_roots(cfg Config) []x509.Certificate {
	if cfg.ca_store.roots.len > 0 {
		return cfg.ca_store.roots
	}
	if cfg.ca_cert.len > 0 {
		roots := load_bundle_file(cfg.ca_cert)
		if roots.len > 0 {
			return roots
		}
	}
	if cfg.ca_cert_pem.len > 0 {
		return parse_bundle_pem(cfg.ca_cert_pem.bytestr())
	}
	return []x509.Certificate{}
}

// load_bundle_dir scans every file in dir for CERTIFICATE blocks, returning
// the union (unreadable files and non-PEM content are skipped).
fn load_bundle_dir(dir string) []x509.Certificate {
	mut roots := []x509.Certificate{}
	entries := os.ls(dir) or { return roots }
	for e in entries {
		txt := os.read_file(os.join_path(dir, e)) or { continue }
		roots << parse_bundle_pem(txt)
	}
	return roots
}

// load_system_roots loads trusted root certificates for chain verification
// (direct-root only in v1). Resolution order:
//  1. SSL_CERT_FILE: explicit bundle file (also honored as a directory scan
//     target would be surprising, so a file is required; missing/unparsable
//     falls through).
//  2. SSL_CERT_DIR: every file in the directory is scanned for certificates.
//  3. Windows Trusted Root stores (Windows only, via load_windows_roots):
//     PowerShell PEM export of LocalMachine/CurrentUser Root, certutil
//     scrape as fallback. Empty on failure so Unix paths and the embedded
//     bundle below still apply.
//  4. Unix bundle paths (freshest on systems that maintain them).
//  5. The embedded Mozilla bundle: identical roots on every OS, which is
//     what makes verification work on Windows and macOS.
// An explicitly configured Config.ca_store/ca_cert still wins over all of
// these (see conn.v); this is only the default when nothing is configured.
pub fn load_system_roots() !CertStore {
	path := os.getenv('SSL_CERT_FILE')
	if path.len > 0 {
		roots := load_bundle_file(path)
		if roots.len > 0 {
			return CertStore{ roots: roots }
		}
	}
	dir := os.getenv('SSL_CERT_DIR')
	if dir.len > 0 {
		roots := load_bundle_dir(dir)
		if roots.len > 0 {
			return CertStore{ roots: roots }
		}
	}
	$if windows {
		windows_roots := load_windows_roots()
		if windows_roots.len > 0 {
			return CertStore{ roots: windows_roots }
		}
	}
	paths := ['/etc/ssl/certs/ca-certificates.crt', '/etc/ssl/cert.pem', '/etc/ssl/certs/ca-bundle.crt']
	mut roots := []x509.Certificate{}
	for p in paths {
		roots << load_bundle_file(p)
	}
	if roots.len == 0 {
		roots = embedded_roots.clone()
	}
	if roots.len == 0 {
		return error('tls: no system roots found')
	}
	return CertStore{ roots: roots }
}

// contains reports whether cert is byte-equal to a trusted root.
// contains checks direct-root trust only; intermediates are not built in v1.
pub fn (cs CertStore) contains(cert x509.Certificate) bool {
	for r in cs.roots {
		if r.raw == cert.raw { // byte equality of DER
			return true
		}
	}
	return false
}
