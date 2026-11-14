module tls

import os
import crypto.pem
import crypto.x509

pub const error_chain_untrusted = 'tls: error_chain_untrusted'
pub const error_chain_expired = 'tls: error_chain_expired'
pub const error_hostname_mismatch = 'tls: error_hostname_mismatch'
pub const error_bad_signature = 'tls: error_bad_signature'

pub struct CertStore {
pub:
	roots []x509.Certificate
}

pub fn load_system_roots() !CertStore {
	paths := ['/etc/ssl/certs/ca-certificates.crt', '/etc/ssl/cert.pem', '/etc/ssl/certs/ca-bundle.crt']
	mut roots := []x509.Certificate{}
	for p in paths {
		txt := os.read_file(p) or { continue }
		mut rest := txt
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
			if rest.len == 0 { break }
		}
	}
	if roots.len == 0 {
		return error('tls: no system roots found')
	}
	return CertStore{ roots: roots }
}

// verify_leaf checks expiry and hostname against the cert. Trust membership is
// checked by the caller against CertStore.roots via der_equal.
pub fn (cs CertStore) contains(cert x509.Certificate) bool {
	for r in cs.roots {
		if r.raw == cert.raw { // byte equality of DER
			return true
		}
	}
	return false
}
