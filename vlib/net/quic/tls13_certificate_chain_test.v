module quic

import encoding.base64
import net.mbedtls

// Duplicated locally (rather than referenced from any other _test.v file)
// deliberately, per this module's own established convention — see
// tls13_keyschedule_test.v's rfc9001_client_dcid and others.
const chain_test_cert_pem = '-----BEGIN CERTIFICATE-----\nMIIEOTCCAyECFG64Q2g46jZb3kRbDOJWX/BwjSp6MA0GCSqGSIb3DQEBCwUAMEUx\nCzAJBgNVBAYTAkFVMRMwEQYDVQQIDApTb21lLVN0YXRlMSEwHwYDVQQKDBhJbnRl\ncm5ldCBXaWRnaXRzIFB0eSBMdGQwIBcNMjMwODAyMTcyOTQyWhgPMjA1MDEyMTcx\nNzI5NDJaMGsxCzAJBgNVBAYTAlVTMRMwEQYDVQQIDApDYWxpZm9ybmlhMRQwEgYD\nVQQHDAtMb3MgQW5nZWxlczEdMBsGA1UECgwUQ2F0YWx5c3QgRGV2ZWxvcG1lbnQx\nEjAQBgNVBAMMCWxvY2FsaG9zdDCCAiIwDQYJKoZIhvcNAQEBBQADggIPADCCAgoC\nggIBALqAI4fqUi+QBVWcsXglouLdOML5+w0+1hSR1KdO0Q5XPdQAs/yYWJ+KUkDw\nG++rfy9DUPq7FNRBVurXQkcAtn6gXdllGUSjwUiDo/N4mMOyS/2sufBuaeww7jVi\nrppH+zwP1tUnjRd6khl6bi1Ian9VSzr3Iy9CkXIg1GU4CPXkOydLeoQfepXxWoK1\nOUNwT3VKC/stAfY3j/NIIeiJYkyuRGFCkxn/BUjN+AsXiTugRcYKEFHdIPkOuCXp\nYbhf+lLsczpxCs3rdZG9b/N6mEDCzXTmeHkmsjdPTf+1k5DZZvKzVBBrgdxCgBb7\n5RwjF5v9WmnIc33wWgfJC6FaUzj9NYxYUbPHD+jTz0rJB/jj4u/xJlM/e5NRmXdW\n70pOMKXtWjRSolLOFIPKLY1qs3KMTAZxKKWPDDF7WlMJxMRt7nnnks5yw43Nog4C\njDLk1ZgETnPpLgo3jbmJdIv+OHKTJrBlVvDq7VTyixCoS5G8KoOmyQJhaXG6NwE2\niVhH5JIKgzgCfetfDsnjxqJ/qtrFXPa8FF2TsomD0NK/GZmIcs+9OeVB75Jn5uhF\nfLHScpiTbuu5w3P/LI/MqihLRB6RRNnRzPH8fIg5bYC9b770ta/8GcFRuYE8t+UR\nGtqXJoIKixbDlqV54kal8FQzYzhETf9+NM6Kb/lKEfG/pslvAgMBAAEwDQYJKoZI\nhvcNAQELBQADggEBALI3uNiNO0QE1brA3QYFK+d9ZroB72NrJ0UNkzYHDg2Fc6xg\n4aVVfaxY08+TmKc0JlMOW+pUxeCW/+UBSngdQiR9EE9xm0k0XIrAsy9RXxRvEtPu\nM1VI2h7ayp1Y2BrnQinevTSgtqLRyS1VbOFRl1FiyVvinw2I0KsDdAMNevAPXcOa\nQ8pUgUq6f56DkhocQaj+hxD/uV8HryNxuoSXnPhvfTN3z4YRGzsaWevJ9EYJliOM\n+XugcqfFJ+W7/QCEcAHCL+Bw6OydG5NFORr3p57PXjjcL/uKmxPBrWg2Bz6uT4uR\nMhj0zttiFHLAt9jGfyk6W57UNUja1e1ggftJJhs=\n-----END CERTIFICATE-----\n'

fn chain_test_pem_to_der(pem string) []u8 {
	body :=
		pem.replace('-----BEGIN CERTIFICATE-----', '').replace('-----END CERTIFICATE-----', '').replace('\n', '').trim_space()
	return base64.decode(body)
}

// test_verify_server_certificate_chain_end_to_end exercises the full
// pipeline from raw Certificate message bytes (as parse_certificate would
// hand them off) through mbedTLS chain building and validation. Uses the
// same real self-signed test cert as net.mbedtls's own tests; expects the
// identical "not a CA" failure for the identical reason (this cert has no
// CA:TRUE basic constraint) — proving this integration layer doesn't
// alter that outcome by mishandling the DER bytes along the way.
fn test_verify_server_certificate_chain_end_to_end() {
	der := chain_test_pem_to_der(chain_test_cert_pem)
	parsed := ParsedCertificate{
		certificate_request_context: []u8{}
		certificate_list:            [
			CertificateEntry{
				cert_data:  der
				extensions: []TlsExtension{}
			},
		]
	}

	verify_server_certificate_chain(parsed, chain_test_cert_pem) or {
		assert err.msg().contains('verification failed')
		return
	}
	assert false, 'expected verification to fail: chain_test_cert_pem has no CA:TRUE basic constraint'
}

fn test_verify_server_certificate_chain_rejects_malformed_der() {
	parsed := ParsedCertificate{
		certificate_request_context: []u8{}
		certificate_list:            [
			CertificateEntry{
				cert_data:  [u8(0xff), 0xff, 0xff, 0xff]
				extensions: []TlsExtension{}
			},
		]
	}
	verify_server_certificate_chain(parsed, chain_test_cert_pem) or { return }
	assert false, 'expected an error for malformed DER data'
}

// test_verified_certificate_chain_free_is_idempotent exercises the exact
// bug /vreview caught and fixed in VerifiedCertificateChain.free(): a
// second free() call must be a harmless no-op, not a double-free. Neither
// of the two tests above ever reaches a live VerifiedCertificateChain
// (both exercise verification-failure paths, which clean up internally
// and never construct one) -- this test builds a real chain directly via
// net.mbedtls to exercise the struct's own cleanup discipline in
// isolation, since this codebase has no CA-flagged test certificate to
// drive a genuine successful chain verification end-to-end yet.
//
// Like the DER-length exact-size requirement in
// net.mbedtls/x509_standalone.v, this is verified by REASONING, not by
// this test's pass/fail status: empirically, removing the `c.chain =
// unsafe { nil }` line and calling free() twice on this platform's
// allocator still passes cleanly (no crash) -- a double-free of a
// moderate-sized heap allocation isn't reliably detected without ASan/
// valgrind. Trust the fix (nulling the pointer makes a second free() see
// `!= nil` fail and skip the call entirely, so there is provably no
// second mbedtls_x509_crt_free call, not just "nothing crashed this
// time"), not this test's outcome, for the actual guarantee.
fn test_verified_certificate_chain_free_is_idempotent() {
	der := chain_test_pem_to_der(chain_test_cert_pem)
	real_chain := mbedtls.build_certificate_chain([der])!
	mut vcc := &VerifiedCertificateChain{
		chain: real_chain
	}
	vcc.free()
	vcc.free() // must not double-free
}

// test_verify_certificate_verify_signature_rejects_use_after_free is a
// regression test for a /vreview finding: calling
// verify_certificate_verify_signature on an already-freed
// VerifiedCertificateChain used to be undefined behavior, not a clean
// error. free() nulls c.chain; mbedtls.get_leaf_public_key's C shim then
// computes `&crt->pk` (pointer arithmetic on a NULL crt), handing back a
// small near-zero, non-null "pointer" that only actually crashes once
// something dereferences it (confirmed by a standalone Phase-R probe:
// mbedtls_pk_verify_ext reading `ctx->pk_info` through that pointer
// segfaults reliably, address 0xa8 -- the pk field's offset in
// mbedtls_x509_crt). The guard added alongside this test checks
// `c.chain == unsafe { nil }` before ever reaching get_leaf_public_key.
fn test_verify_certificate_verify_signature_rejects_use_after_free() {
	der := chain_test_pem_to_der(chain_test_cert_pem)
	real_chain := mbedtls.build_certificate_chain([der])!
	mut vcc := &VerifiedCertificateChain{
		chain: real_chain
	}
	vcc.free()

	cv := ParsedCertificateVerify{
		algorithm: sig_scheme_rsa_pss_rsae_sha256
		signature: []u8{len: 8, init: 0x11}
	}
	vcc.verify_certificate_verify_signature(cv, .server, []u8{len: 32, init: 0xcd}) or {
		assert err.msg().contains('freed VerifiedCertificateChain')
		return
	}
	assert false, 'expected use-after-free to be rejected cleanly, not crash or silently proceed'
}

// test_verify_certificate_verify_signature_rejects_garbage exercises
// verify_certificate_verify_signature's dispatch (algorithm ->
// digest -> net.mbedtls call) and signed-content construction end to end,
// for all four algorithms parse_certificate_verify can ever hand it. A
// genuine POSITIVE case (a real signature that verifies) is deliberately
// not attempted here: net.mbedtls's own
// x509_standalone_signature_test.v already proves
// verify_rsa_pss_signature/verify_ecdsa_signature work correctly against
// real cryptography (a genuine RSA-PSS sign+verify round trip using this
// same test certificate's matching private key), and reproducing that
// round trip at this layer would need to reach into net.mbedtls's
// RNG/key-parsing internals that are deliberately private to that module
// (this codebase's own convention: net.quic only ever calls net.mbedtls's
// pub API, never raw C.mbedtls_* symbols directly -- see
// tls13_certificate_chain.v's free()/verify_server_certificate_chain,
// neither of which touches a C.mbedtls_* symbol itself). What IS new and
// worth testing at this layer is the DISPATCH and the
// certificate_verify_signed_content construction that feeds it, which this
// test exercises via the reject path for all four schemes.
fn test_verify_certificate_verify_signature_rejects_garbage() {
	der := chain_test_pem_to_der(chain_test_cert_pem)
	// Built directly (not via verify_server_certificate_chain), same as
	// test_verified_certificate_chain_free_is_idempotent above: this cert
	// isn't a CA, so the trust-validating constructor always fails for it,
	// and trust validation is orthogonal to what this test exercises
	// (signature verification against the leaf key).
	real_chain := mbedtls.build_certificate_chain([der])!
	mut vcc := &VerifiedCertificateChain{
		chain: real_chain
	}
	defer {
		vcc.free()
	}

	transcript_hash := []u8{len: 32, init: 0xab}
	garbage_signature := []u8{len: 64, init: 0x42}

	for algorithm in [sig_scheme_ecdsa_secp256r1_sha256, sig_scheme_rsa_pss_rsae_sha256,
		sig_scheme_rsa_pss_rsae_sha384, sig_scheme_rsa_pss_rsae_sha512] {
		cv := ParsedCertificateVerify{
			algorithm: algorithm
			signature: garbage_signature
		}
		vcc.verify_certificate_verify_signature(cv, .server, transcript_hash) or { continue }
		assert false, 'expected a garbage signature to be rejected for algorithm 0x${algorithm:04x}'
	}
}

// test_verify_certificate_verify_signature_rejects_unknown_algorithm covers
// the match's `else` arm -- unreachable via parse_certificate_verify's own
// validation in real use, but ParsedCertificateVerify is a public struct
// nothing stops a test (or a future caller who skips parse_certificate_verify)
// from constructing directly with an out-of-range algorithm.
fn test_verify_certificate_verify_signature_rejects_unknown_algorithm() {
	der := chain_test_pem_to_der(chain_test_cert_pem)
	real_chain := mbedtls.build_certificate_chain([der])!
	mut vcc := &VerifiedCertificateChain{
		chain: real_chain
	}
	defer {
		vcc.free()
	}

	cv := ParsedCertificateVerify{
		algorithm: 0x9999
		signature: []u8{len: 8, init: 0x11}
	}
	vcc.verify_certificate_verify_signature(cv, .server, []u8{len: 32, init: 0xcd}) or {
		assert err.msg().contains('no signature-verification dispatch')
		return
	}
	assert false, 'expected an unrecognized algorithm to be rejected'
}
