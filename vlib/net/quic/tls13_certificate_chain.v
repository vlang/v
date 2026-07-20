module quic

import net.mbedtls
import crypto.sha256
import crypto.sha512

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

// verify_certificate_verify_signature checks a parsed CertificateVerify
// message's signature against this chain's leaf certificate's public key.
// `role`/`transcript_hash` feed certificate_verify_signed_content's exact
// RFC 8446 §4.4.3 signed-content construction -- what was actually signed,
// not `transcript_hash` directly. Dispatches on `cv.algorithm` to the
// matching digest + mbedTLS verification call; parse_certificate_verify has
// already restricted `cv.algorithm` to v1's fixed offered set
// (sig_scheme_ecdsa_secp256r1_sha256/rsa_pss_rsae_sha256/384/512), so the
// `else` arm below is unreachable in practice, not a real fallback path.
//
// Guards against being called after free(): mbedtls.get_leaf_public_key's
// own doc comment already states the precondition ("do not call this after
// free_certificate_chain") but doesn't enforce it -- free() nulls c.chain,
// and the C shim behind get_leaf_public_key computes `&crt->pk` (pointer
// arithmetic on a NULL crt), which is undefined behavior, not a clean nil
// dereference an `or {}` could catch. No caller does this today, but the
// upcoming client state machine will hold a VerifiedCertificateChain across
// multiple calls (trust check, then this), making the free-then-use
// ordering an easy mistake to introduce later -- same defensive rationale
// as free()'s own idempotency guard, just checked from the other side.
pub fn (c &VerifiedCertificateChain) verify_certificate_verify_signature(cv ParsedCertificateVerify, role CertificateVerifyRole, transcript_hash []u8) ! {
	if c.chain == unsafe { nil } {
		return error('quic: verify_certificate_verify_signature called on a freed VerifiedCertificateChain')
	}
	signed_content := certificate_verify_signed_content(role, transcript_hash)
	pk := mbedtls.get_leaf_public_key(c.chain)
	match cv.algorithm {
		sig_scheme_ecdsa_secp256r1_sha256 {
			hash := sha256.sum256(signed_content)
			mbedtls.verify_ecdsa_signature(pk, .sha256, hash, cv.signature)!
		}
		sig_scheme_rsa_pss_rsae_sha256 {
			hash := sha256.sum256(signed_content)
			mbedtls.verify_rsa_pss_signature(pk, .sha256, hash, cv.signature)!
		}
		sig_scheme_rsa_pss_rsae_sha384 {
			hash := sha512.sum384(signed_content)
			mbedtls.verify_rsa_pss_signature(pk, .sha384, hash, cv.signature)!
		}
		sig_scheme_rsa_pss_rsae_sha512 {
			hash := sha512.sum512(signed_content)
			mbedtls.verify_rsa_pss_signature(pk, .sha512, hash, cv.signature)!
		}
		else {
			return error('quic: CertificateVerify algorithm 0x${cv.algorithm:04x} has no signature-verification dispatch (unreachable: parse_certificate_verify already restricts algorithm to the offered set)')
		}
	}
}
