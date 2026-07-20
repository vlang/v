module quic

import net.mbedtls

// VerifiedCertificateChain wraps an mbedTLS certificate chain built from a
// parsed TLS 1.3 Certificate message (tls13_certificate.v's
// ParsedCertificate) once it has passed chain-trust validation. The caller
// MUST call `free()` when done — the underlying mbedtls_x509_crt chain
// holds C-heap-allocated buffers with no GC visibility (see
// net.mbedtls.build_certificate_chain's own doc comment).
pub struct VerifiedCertificateChain {
mut:
	chain &C.mbedtls_x509_crt = unsafe { nil }
}

// free releases the underlying mbedTLS chain. Nulls out `chain` after
// freeing so a second free() call (e.g. a defer racing an explicit early
// free) is a harmless no-op rather than a double-free — same discipline
// as net.mbedtls.SSLConn.shutdown()'s own documented guard for the
// identical class of repeated-cleanup-call bug.
pub fn (mut c VerifiedCertificateChain) free() {
	if c.chain != unsafe { nil } {
		mbedtls.free_certificate_chain(c.chain)
		c.chain = unsafe { nil }
	}
}

// verify_server_certificate_chain builds an mbedTLS certificate chain from
// a parsed Certificate message's certificate_list (leaf-first, per RFC
// 8446 §4.4.2) and validates it against `ca_bundle_pem` (one or more
// trusted CA certificates in PEM format — the caller's trust anchor,
// mirroring this codebase's existing
// net.mbedtls.SSLConnectConfig.verify contract, since there is no OS
// trust-store lookup anywhere in this codebase for any TLS client).
//
// Hostname verification (matching the leaf certificate against the SNI
// name the client actually sent) is deliberately NOT done here — see
// net.mbedtls.verify_certificate_chain's own doc comment for why it can't
// do this itself; the caller must do it using the leaf certificate this
// function's result makes available (once Phase 2c's signature
// verification, still pending, extracts and exposes it).
pub fn verify_server_certificate_chain(parsed ParsedCertificate, ca_bundle_pem string) !&VerifiedCertificateChain {
	mut der_certs := [][]u8{cap: parsed.certificate_list.len}
	for entry in parsed.certificate_list {
		der_certs << entry.cert_data
	}
	chain := mbedtls.build_certificate_chain(der_certs)!
	mbedtls.verify_certificate_chain(chain, ca_bundle_pem) or {
		mbedtls.free_certificate_chain(chain)
		return err
	}
	return &VerifiedCertificateChain{
		chain: chain
	}
}
