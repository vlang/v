module mbedtls

import encoding.base64
import crypto.sha256
import crypto.sha512

// Duplicated locally (rather than referenced from another _test.v file)
// deliberately, per this module's own established convention -- see
// x509_standalone_chain_test.v's own note. This is the same real
// self-signed test cert used throughout net.mbedtls's test suite, paired
// here with its matching RSA private key (also already used by
// mbedtls_sslconn_shutdown_does_not_panic_test.v) so this file can produce
// a GENUINE RSA-PSS signature to verify against -- proving
// verify_rsa_pss_signature's C-binding parameter marshaling (enum casts,
// struct field order, pointer plumbing) actually works with real
// cryptography, not just source-level reasoning about it.
const signature_test_cert = '-----BEGIN CERTIFICATE-----\nMIIEOTCCAyECFG64Q2g46jZb3kRbDOJWX/BwjSp6MA0GCSqGSIb3DQEBCwUAMEUx\nCzAJBgNVBAYTAkFVMRMwEQYDVQQIDApTb21lLVN0YXRlMSEwHwYDVQQKDBhJbnRl\ncm5ldCBXaWRnaXRzIFB0eSBMdGQwIBcNMjMwODAyMTcyOTQyWhgPMjA1MDEyMTcx\nNzI5NDJaMGsxCzAJBgNVBAYTAlVTMRMwEQYDVQQIDApDYWxpZm9ybmlhMRQwEgYD\nVQQHDAtMb3MgQW5nZWxlczEdMBsGA1UECgwUQ2F0YWx5c3QgRGV2ZWxvcG1lbnQx\nEjAQBgNVBAMMCWxvY2FsaG9zdDCCAiIwDQYJKoZIhvcNAQEBBQADggIPADCCAgoC\nggIBALqAI4fqUi+QBVWcsXglouLdOML5+w0+1hSR1KdO0Q5XPdQAs/yYWJ+KUkDw\nG++rfy9DUPq7FNRBVurXQkcAtn6gXdllGUSjwUiDo/N4mMOyS/2sufBuaeww7jVi\nrppH+zwP1tUnjRd6khl6bi1Ian9VSzr3Iy9CkXIg1GU4CPXkOydLeoQfepXxWoK1\nOUNwT3VKC/stAfY3j/NIIeiJYkyuRGFCkxn/BUjN+AsXiTugRcYKEFHdIPkOuCXp\nYbhf+lLsczpxCs3rdZG9b/N6mEDCzXTmeHkmsjdPTf+1k5DZZvKzVBBrgdxCgBb7\n5RwjF5v9WmnIc33wWgfJC6FaUzj9NYxYUbPHD+jTz0rJB/jj4u/xJlM/e5NRmXdW\n70pOMKXtWjRSolLOFIPKLY1qs3KMTAZxKKWPDDF7WlMJxMRt7nnnks5yw43Nog4C\njDLk1ZgETnPpLgo3jbmJdIv+OHKTJrBlVvDq7VTyixCoS5G8KoOmyQJhaXG6NwE2\niVhH5JIKgzgCfetfDsnjxqJ/qtrFXPa8FF2TsomD0NK/GZmIcs+9OeVB75Jn5uhF\nfLHScpiTbuu5w3P/LI/MqihLRB6RRNnRzPH8fIg5bYC9b770ta/8GcFRuYE8t+UR\nGtqXJoIKixbDlqV54kal8FQzYzhETf9+NM6Kb/lKEfG/pslvAgMBAAEwDQYJKoZI\nhvcNAQELBQADggEBALI3uNiNO0QE1brA3QYFK+d9ZroB72NrJ0UNkzYHDg2Fc6xg\n4aVVfaxY08+TmKc0JlMOW+pUxeCW/+UBSngdQiR9EE9xm0k0XIrAsy9RXxRvEtPu\nM1VI2h7ayp1Y2BrnQinevTSgtqLRyS1VbOFRl1FiyVvinw2I0KsDdAMNevAPXcOa\nQ8pUgUq6f56DkhocQaj+hxD/uV8HryNxuoSXnPhvfTN3z4YRGzsaWevJ9EYJliOM\n+XugcqfFJ+W7/QCEcAHCL+Bw6OydG5NFORr3p57PXjjcL/uKmxPBrWg2Bz6uT4uR\nMhj0zttiFHLAt9jGfyk6W57UNUja1e1ggftJJhs=\n-----END CERTIFICATE-----\n'

const signature_test_key = '-----BEGIN RSA PRIVATE KEY-----\n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\n-----END RSA PRIVATE KEY-----\n'

fn signature_test_pem_to_der(pem string) []u8 {
	body :=
		pem.replace('-----BEGIN CERTIFICATE-----', '').replace('-----END CERTIFICATE-----', '').replace('\n', '').trim_space()
	return base64.decode(body)
}

// sign_rsa_pss_for_test signs `hash` with the test private key using real
// RSA-PSS (mbedtls_pk_sign_ext), with the salt length pinned to hash.len --
// matching exactly what verify_rsa_pss_signature expects to check, so a
// successful verify here proves the two functions actually agree, not just
// that each independently "looks right." Test-only: production code never
// signs anything (v1 is client-only and never sends a client
// CertificateVerify), see mbedtls_pk_sign_ext's own doc comment in
// mbedtls.c.v for why the binding still lives there rather than here.
fn sign_rsa_pss_for_test(md_alg MbedtlsMdType, hash []u8) ![]u8 {
	mut ctr_drbg := C.mbedtls_ctr_drbg_context{}
	mut entropy := C.mbedtls_entropy_context{}
	init_rng(mut ctr_drbg, mut entropy)!
	defer {
		free_rng(mut ctr_drbg, mut entropy)
	}

	mut pk := C.mbedtls_pk_context{}
	C.mbedtls_pk_init(&pk)
	defer {
		C.mbedtls_pk_free(&pk)
	}
	unsafe {
		parse_ret := C.mbedtls_pk_parse_key(&pk, signature_test_key.str,
			signature_test_key.len + 1, 0, 0, C.mbedtls_ctr_drbg_random, &ctr_drbg)
		if parse_ret != 0 {
			return error_with_code('test: failed to parse RSA private key, mbedtls ret: ${parse_ret}',
				parse_ret)
		}
	}
	// 600 bytes is comfortable headroom above this test key's 4096-bit
	// (512-byte) modulus -- the largest signature mbedtls_pk_sign_ext could
	// possibly produce for it.
	mut sig := []u8{len: 600}
	mut sig_len := usize(0)
	ret := C.mbedtls_pk_sign_ext(pk_type_rsassa_pss, &pk, i32(md_alg), hash.data, usize(hash.len),
		sig.data, usize(sig.len), &sig_len, C.mbedtls_ctr_drbg_random, &ctr_drbg)
	if ret != 0 {
		return error_with_code('test: mbedtls_pk_sign_ext failed, mbedtls ret: ${ret}', ret)
	}
	return sig[..int(sig_len)].clone()
}

fn test_verify_rsa_pss_signature_accepts_genuine_signature_all_hash_sizes() {
	der := signature_test_pem_to_der(signature_test_cert)
	chain := build_certificate_chain([der])!
	defer {
		free_certificate_chain(chain)
	}
	pk := get_leaf_public_key(chain)
	message := 'net.quic RSA-PSS round-trip test message'.bytes()

	hash256 := sha256.sum256(message)
	sig256 := sign_rsa_pss_for_test(.sha256, hash256)!
	verify_rsa_pss_signature(pk, .sha256, hash256, sig256)!

	hash384 := sha512.sum384(message)
	sig384 := sign_rsa_pss_for_test(.sha384, hash384)!
	verify_rsa_pss_signature(pk, .sha384, hash384, sig384)!

	hash512 := sha512.sum512(message)
	sig512 := sign_rsa_pss_for_test(.sha512, hash512)!
	verify_rsa_pss_signature(pk, .sha512, hash512, sig512)!
}

fn test_verify_rsa_pss_signature_rejects_corrupted_signature() {
	der := signature_test_pem_to_der(signature_test_cert)
	chain := build_certificate_chain([der])!
	defer {
		free_certificate_chain(chain)
	}
	pk := get_leaf_public_key(chain)

	hash := sha256.sum256('net.quic RSA-PSS corruption test message'.bytes())
	mut sig := sign_rsa_pss_for_test(.sha256, hash)!
	sig[0] ^= 0xff

	verify_rsa_pss_signature(pk, .sha256, hash, sig) or { return }
	assert false, 'expected a corrupted signature to be rejected'
}

fn test_verify_rsa_pss_signature_rejects_signature_over_a_different_message() {
	der := signature_test_pem_to_der(signature_test_cert)
	chain := build_certificate_chain([der])!
	defer {
		free_certificate_chain(chain)
	}
	pk := get_leaf_public_key(chain)

	hash := sha256.sum256('original message'.bytes())
	sig := sign_rsa_pss_for_test(.sha256, hash)!
	wrong_hash := sha256.sum256('a completely different message'.bytes())

	verify_rsa_pss_signature(pk, .sha256, wrong_hash, sig) or { return }
	assert false, 'expected a signature over a different message to be rejected'
}

// Not covered by any test in this file: a signature with a WRONG (but
// internally consistent) salt length being rejected -- proving
// expected_salt_len is pinned to hash.len rather than
// MBEDTLS_RSA_SALT_LEN_ANY, specifically. mbedtls_pk_sign_ext (used by
// sign_rsa_pss_for_test above) offers no way to force a non-default salt
// length -- that needs the lower-level mbedtls_rsa_rsassa_pss_sign_ext (or
// an OpenSSL EVP_PKEY_CTX_set_rsa_pss_saltlen-based signer), either of
// which is meaningfully more C-binding surface than this one edge case
// justifies. Trust the source-level confirmation in
// verify_rsa_pss_signature's own doc comment (x509_standalone.v: rsa.c's
// real equality check, cross-checked against mbedTLS's own TLS 1.3 code
// using the identical pattern) for this one, not a test result.

// test_verify_ecdsa_signature_rejects_incompatible_key_type documents an
// honest test-coverage gap: this repo has no EC private key fixture
// anywhere (grepped the whole tree for "BEGIN EC PRIVATE KEY" -- none
// exist), so a genuine ECDSA sign+verify round trip like the RSA-PSS ones
// above isn't possible here. This test instead proves the ECDSA code path
// doesn't crash and correctly REJECTS when asked to verify against an
// RSA-typed key: mbedtls_pk_can_do's eckey_can_do/rsa_can_do split
// (pk_wrap.c, confirmed by source read -- see verify_ecdsa_signature's own
// doc comment in x509_standalone.v) means an RSA-typed key can never
// satisfy an ECDSA verify request, so mbedtls_pk_verify_ext must return a
// type-mismatch error here -- exercising the real call path (C-binding
// types, enum casts, pointer plumbing) even though it can't exercise a
// genuine ECDSA signature.
fn test_verify_ecdsa_signature_rejects_incompatible_key_type() {
	der := signature_test_pem_to_der(signature_test_cert)
	chain := build_certificate_chain([der])!
	defer {
		free_certificate_chain(chain)
	}
	pk := get_leaf_public_key(chain)

	hash := sha256.sum256('irrelevant content'.bytes())
	fake_signature := []u8{len: 64, init: 0x42}

	verify_ecdsa_signature(pk, .sha256, hash, fake_signature) or { return }
	assert false, 'expected ECDSA verification against an RSA-typed key to fail'
}
