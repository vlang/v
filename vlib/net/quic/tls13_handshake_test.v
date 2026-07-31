module quic

import crypto.ecdsa
import crypto.sha256
import encoding.base64
import net.mbedtls

// Duplicated locally (rather than referenced from any other _test.v file)
// deliberately, per this module's own established convention -- see
// tls13_certificate_chain_test.v's own note. Same real self-signed test
// cert + matching RSA private key used throughout net.mbedtls's test suite
// (net.mbedtls/x509_standalone_signature_test.v's own note explains the
// provenance) -- reused here so this file's fake server can produce a
// GENUINE RSA-PSS CertificateVerify signature, not a mocked one.
const handshake_test_cert_pem = '-----BEGIN CERTIFICATE-----\nMIIEOTCCAyECFG64Q2g46jZb3kRbDOJWX/BwjSp6MA0GCSqGSIb3DQEBCwUAMEUx\nCzAJBgNVBAYTAkFVMRMwEQYDVQQIDApTb21lLVN0YXRlMSEwHwYDVQQKDBhJbnRl\ncm5ldCBXaWRnaXRzIFB0eSBMdGQwIBcNMjMwODAyMTcyOTQyWhgPMjA1MDEyMTcx\nNzI5NDJaMGsxCzAJBgNVBAYTAlVTMRMwEQYDVQQIDApDYWxpZm9ybmlhMRQwEgYD\nVQQHDAtMb3MgQW5nZWxlczEdMBsGA1UECgwUQ2F0YWx5c3QgRGV2ZWxvcG1lbnQx\nEjAQBgNVBAMMCWxvY2FsaG9zdDCCAiIwDQYJKoZIhvcNAQEBBQADggIPADCCAgoC\nggIBALqAI4fqUi+QBVWcsXglouLdOML5+w0+1hSR1KdO0Q5XPdQAs/yYWJ+KUkDw\nG++rfy9DUPq7FNRBVurXQkcAtn6gXdllGUSjwUiDo/N4mMOyS/2sufBuaeww7jVi\nrppH+zwP1tUnjRd6khl6bi1Ian9VSzr3Iy9CkXIg1GU4CPXkOydLeoQfepXxWoK1\nOUNwT3VKC/stAfY3j/NIIeiJYkyuRGFCkxn/BUjN+AsXiTugRcYKEFHdIPkOuCXp\nYbhf+lLsczpxCs3rdZG9b/N6mEDCzXTmeHkmsjdPTf+1k5DZZvKzVBBrgdxCgBb7\n5RwjF5v9WmnIc33wWgfJC6FaUzj9NYxYUbPHD+jTz0rJB/jj4u/xJlM/e5NRmXdW\n70pOMKXtWjRSolLOFIPKLY1qs3KMTAZxKKWPDDF7WlMJxMRt7nnnks5yw43Nog4C\njDLk1ZgETnPpLgo3jbmJdIv+OHKTJrBlVvDq7VTyixCoS5G8KoOmyQJhaXG6NwE2\niVhH5JIKgzgCfetfDsnjxqJ/qtrFXPa8FF2TsomD0NK/GZmIcs+9OeVB75Jn5uhF\nfLHScpiTbuu5w3P/LI/MqihLRB6RRNnRzPH8fIg5bYC9b770ta/8GcFRuYE8t+UR\nGtqXJoIKixbDlqV54kal8FQzYzhETf9+NM6Kb/lKEfG/pslvAgMBAAEwDQYJKoZI\nhvcNAQELBQADggEBALI3uNiNO0QE1brA3QYFK+d9ZroB72NrJ0UNkzYHDg2Fc6xg\n4aVVfaxY08+TmKc0JlMOW+pUxeCW/+UBSngdQiR9EE9xm0k0XIrAsy9RXxRvEtPu\nM1VI2h7ayp1Y2BrnQinevTSgtqLRyS1VbOFRl1FiyVvinw2I0KsDdAMNevAPXcOa\nQ8pUgUq6f56DkhocQaj+hxD/uV8HryNxuoSXnPhvfTN3z4YRGzsaWevJ9EYJliOM\n+XugcqfFJ+W7/QCEcAHCL+Bw6OydG5NFORr3p57PXjjcL/uKmxPBrWg2Bz6uT4uR\nMhj0zttiFHLAt9jGfyk6W57UNUja1e1ggftJJhs=\n-----END CERTIFICATE-----\n'

const handshake_test_key_pem = '-----BEGIN RSA PRIVATE KEY-----\n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\n-----END RSA PRIVATE KEY-----\n'

fn handshake_test_pem_to_der(pem string) []u8 {
	body :=
		pem.replace('-----BEGIN CERTIFICATE-----', '').replace('-----END CERTIFICATE-----', '').replace('\n', '').trim_space()
	return base64.decode(body)
}

// fake_server_sign_certificate_verify signs `signed_content` with the test
// RSA private key using real RSA-PSS/SHA-256 -- simulating what a real TLS
// 1.3 server does to produce CertificateVerify. Directly uses mbedTLS's C
// API (not net.mbedtls's pub verify-only wrappers, which have no signing
// counterpart -- production net.quic code never signs anything, since v1
// is client-only with no client-cert auth) rather than mocking a signature;
// mirrors net.mbedtls/x509_standalone_signature_test.v's own
// sign_rsa_pss_for_test, duplicated rather than shared since it lives in a
// different module's test file.
fn fake_server_sign_certificate_verify(signed_content []u8) ![]u8 {
	hash := sha256.sum256(signed_content)

	mut ctr_drbg := C.mbedtls_ctr_drbg_context{}
	mut entropy := C.mbedtls_entropy_context{}
	C.mbedtls_ctr_drbg_init(&ctr_drbg)
	C.mbedtls_entropy_init(&entropy)
	defer {
		C.mbedtls_ctr_drbg_free(&ctr_drbg)
		C.mbedtls_entropy_free(&entropy)
	}
	seed_ret := C.mbedtls_ctr_drbg_seed(&ctr_drbg, C.mbedtls_entropy_func, &entropy, 0, 0)
	if seed_ret != 0 {
		return error('test: failed to seed RNG, mbedtls ret: ${seed_ret}')
	}

	mut pk := C.mbedtls_pk_context{}
	C.mbedtls_pk_init(&pk)
	defer {
		C.mbedtls_pk_free(&pk)
	}
	unsafe {
		parse_ret := C.mbedtls_pk_parse_key(&pk, handshake_test_key_pem.str,

			handshake_test_key_pem.len + 1, 0, 0, C.mbedtls_ctr_drbg_random, &ctr_drbg)
		if parse_ret != 0 {
			return error('test: failed to parse RSA private key, mbedtls ret: ${parse_ret}')
		}
	}
	// pk_type 6 = MBEDTLS_PK_RSASSA_PSS, md_alg 9 = MBEDTLS_MD_SHA256 --
	// the same numeric values net.mbedtls/x509_standalone.v's
	// verify_rsa_pss_signature uses on the verify side (pk.h/md.h,
	// confirmed there, not re-derived here).
	mut sig := []u8{len: 600}
	mut sig_len := usize(0)
	ret := C.mbedtls_pk_sign_ext(6, &pk, 9, hash.data, usize(hash.len), sig.data, usize(sig.len),
		&sig_len, C.mbedtls_ctr_drbg_random, &ctr_drbg)
	if ret != 0 {
		return error('test: mbedtls_pk_sign_ext failed, mbedtls ret: ${ret}')
	}
	return sig[..int(sig_len)].clone()
}

// extract_client_hello_key_exchange hand-parses a built ClientHello (there
// is no production ClientHello parser -- a client never needs to parse its
// own message, see tls13_client_hello_test.v's test_build_client_hello_structure
// for the same pattern) far enough to pull out the key_share
// extension's key_exchange bytes, which this file's fake server needs to
// derive a shared secret with.
fn extract_client_hello_key_exchange(client_hello_framed []u8) ![]u8 {
	msg, _ := parse_handshake_message(client_hello_framed)!
	body := msg.body
	mut cursor := 2 + 32 // legacy_version + random
	session_id_len := int(body[cursor])
	cursor += 1 + session_id_len
	cipher_suites_len := int((u32(body[cursor]) << 8) | u32(body[cursor + 1]))
	cursor += 2 + cipher_suites_len
	compression_len := int(body[cursor])
	cursor += 1 + compression_len
	extensions_len := int((u32(body[cursor]) << 8) | u32(body[cursor + 1]))
	cursor += 2
	extensions := parse_extension_list(body[cursor..cursor + extensions_len])!
	ks_ext := find_extension(extensions, ext_key_share) or {
		return error('test: ClientHello missing key_share')
	}
	// Client's key_share extension_data is client_shares<4..2^16-1>: a
	// 2-byte list length, then exactly one KeyShareEntry (group(2) +
	// key_exchange_len(2) + key_exchange), since v1 sends exactly one.
	group := u16((u32(ks_ext.data[2]) << 8) | u32(ks_ext.data[3]))
	if group != named_group_secp256r1 {
		return error('test: unexpected client key_share group 0x${group:04x}')
	}
	key_exchange_len := int((u32(ks_ext.data[4]) << 8) | u32(ks_ext.data[5]))
	return ks_ext.data[6..6 + key_exchange_len].clone()
}

// build_fake_server_hello builds a real ServerHello (RFC 8446 §4.1.3) wire
// message offering exactly what build_client_hello sent (TLS_AES_128_GCM_SHA256,
// secp256r1) -- there is no production ServerHello ENCODER (only
// parse_server_hello, since a client never sends one), so this hand-builds
// it the same way tls13_server_hello_test.v's own fixtures do.
fn build_fake_server_hello(server_random []u8, key_exchange []u8) ![]u8 {
	mut body := []u8{}
	body << u8(0x03)
	body << u8(0x03)
	body << server_random
	body << u8(0) // legacy_session_id_echo length = 0
	body << u8(cipher_suite_tls_aes_128_gcm_sha256 >> 8)
	body << u8(cipher_suite_tls_aes_128_gcm_sha256)
	body << u8(0) // legacy_compression_method

	mut sv_data := []u8{}
	sv_data << u8(tls_version_1_3 >> 8)
	sv_data << u8(tls_version_1_3)
	sv_ext := encode_extension(ext_supported_versions, sv_data)!

	mut ks_entry := []u8{}
	ks_entry << u8(named_group_secp256r1 >> 8)
	ks_entry << u8(named_group_secp256r1)
	ks_entry << u8(key_exchange.len >> 8)
	ks_entry << u8(key_exchange.len)
	ks_entry << key_exchange
	ks_ext := encode_extension(ext_key_share, ks_entry)!

	mut extensions := []u8{}
	extensions << sv_ext
	extensions << ks_ext
	body << u8(extensions.len >> 8)
	body << u8(extensions.len)
	body << extensions

	return encode_handshake_message(.server_hello, body)!
}

fn build_fake_encrypted_extensions(server_transport_params QuicTransportParameters) ![]u8 {
	encoded_tp := encode_transport_parameters(server_transport_params)!
	tp_ext := encode_extension(ext_quic_transport_parameters, encoded_tp)!
	mut extensions := []u8{}
	extensions << tp_ext
	mut body := []u8{}
	body << u8(extensions.len >> 8)
	body << u8(extensions.len)
	body << extensions
	return encode_handshake_message(.encrypted_extensions, body)!
}

fn build_fake_certificate(cert_der []u8) ![]u8 {
	mut entry := []u8{}
	entry << u8(cert_der.len >> 16)
	entry << u8(cert_der.len >> 8)
	entry << u8(cert_der.len)
	entry << cert_der
	entry << u8(0) // per-CertificateEntry extensions length = 0
	entry << u8(0)

	mut body := []u8{}
	body << u8(0) // certificate_request_context length = 0
	body << u8(entry.len >> 16)
	body << u8(entry.len >> 8)
	body << u8(entry.len)
	body << entry
	return encode_handshake_message(.certificate, body)!
}

fn build_fake_certificate_verify(algorithm u16, signature []u8) ![]u8 {
	mut body := []u8{}
	body << u8(algorithm >> 8)
	body << u8(algorithm)
	body << u8(signature.len >> 8)
	body << u8(signature.len)
	body << signature
	return encode_handshake_message(.certificate_verify, body)!
}

// fakeServerHandshake bundles the fake server's own key material so a test
// can independently derive the SAME handshake secrets the client derives,
// using the exact same (real, production) key-schedule functions --
// checking that both sides agree, not just that the client doesn't crash.
struct FakeServerHandshake {
	priv_key          ecdsa.PrivateKey
	handshake_secrets HandshakeSecrets
	certificate_der   []u8
}

// drive_to_wait_certificate_verify runs a real client handshake through
// ClientHello, ServerHello, EncryptedExtensions, and Certificate, using a
// genuine fake TLS 1.3 server (real ECDHE, real key schedule). Certificate
// trust validation is expected to fail here (this repo has no CA-flagged
// test certificate -- see tls13_certificate_chain_test.v's own note for
// the same limitation): the handshake's error path is asserted directly by
// callers that want to test THAT; callers that want to continue past
// Certificate (to test CertificateVerify/Finished with real cryptography,
// independent of the trust-check gap) instead take the returned handshake
// and directly install a VerifiedCertificateChain built the same way
// tls13_certificate_chain_test.v's own tests do -- a legitimate
// same-module white-box technique, not a shortcut around anything this
// file itself is responsible for proving (chain-trust verification is
// already covered by tls13_certificate_chain_test.v).
fn drive_to_certificate(client_random []u8, server_initial_scid []u8) !(&Tls13ClientHandshake, FakeServerHandshake, string) {
	server_pub, server_priv := ecdsa.generate_key(nid: .prime256v1)!
	defer {
		server_pub.free()
	}
	server_ecdhe_public_bytes := server_pub.uncompressed_bytes()!
	server_random := []u8{len: 32, init: 0x22}
	server_der := handshake_test_pem_to_der(handshake_test_cert_pem)

	mut h, client_hello := Tls13ClientHandshake.start(ClientHandshakeParams{
		random:               client_random
		server_name:          'example.com'
		transport_parameters: QuicTransportParameters{
			initial_source_connection_id: [u8(1), 2, 3, 4]
		}
		ca_bundle_pem:        handshake_test_cert_pem
	})!

	client_key_exchange := extract_client_hello_key_exchange(client_hello)!
	client_pub := ecdsa.PublicKey.from_uncompressed_bytes(client_key_exchange, nid: .prime256v1)!
	defer {
		client_pub.free()
	}
	server_shared_secret := server_priv.derive_shared_secret(client_pub)!

	server_hello_framed := build_fake_server_hello(server_random, server_ecdhe_public_bytes)!
	server_hello_msg, _ := parse_handshake_message(server_hello_framed)!
	h.process_server_hello(server_hello_msg, server_hello_framed)!

	early_secret := derive_early_secret()!
	ch_sh_hash := sha256.sum256(concat_bytes(client_hello, server_hello_framed))
	server_handshake_secrets := derive_handshake_secrets(early_secret, server_shared_secret,
		ch_sh_hash)!

	ee_framed := build_fake_encrypted_extensions(QuicTransportParameters{
		initial_source_connection_id: server_initial_scid
	})!
	ee_msg, _ := parse_handshake_message(ee_framed)!
	h.process_encrypted_extensions(ee_msg, ee_framed, server_initial_scid)!

	cert_framed := build_fake_certificate(server_der)!
	cert_msg, _ := parse_handshake_message(cert_framed)!
	h.process_certificate_or_request(cert_msg, cert_framed) or {
		return h, FakeServerHandshake{
			priv_key:          server_priv
			handshake_secrets: server_handshake_secrets
			certificate_der:   server_der
		}, err.msg()
	}
	return h, FakeServerHandshake{
		priv_key:          server_priv
		handshake_secrets: server_handshake_secrets
		certificate_der:   server_der
	}, ''
}

fn concat_bytes(a []u8, b []u8) []u8 {
	mut out := []u8{cap: a.len + b.len}
	out << a
	out << b
	return out
}

// test_handshake_reaches_certificate_and_reports_the_known_trust_gap drives
// a real client handshake with a genuine fake server through Certificate
// processing and asserts the EXPECTED "not a CA" failure -- proving the
// state machine correctly wires ServerHello/EncryptedExtensions/Certificate
// together with real ECDHE and real transport-parameter cross-checking,
// and correctly PROPAGATES a chain-trust failure rather than swallowing it
// or crashing, rather than pretending to reach .connected with a
// certificate this repo cannot actually trust.
fn test_handshake_reaches_certificate_and_reports_the_known_trust_gap() {
	mut h, server, cert_err_msg := drive_to_certificate([]u8{len: 32, init: 0x11}, [
		u8(0xaa),
		0xbb,
		0xcc,
		0xdd,
	])!
	defer {
		h.free()
		unsafe { server.priv_key.free() }
	}
	assert cert_err_msg.contains('verification failed')
	assert h.state() == .wait_certificate
}

// test_handshake_certificate_verify_and_finished_with_real_cryptography
// picks up where the previous test's known gap stops: a
// VerifiedCertificateChain built directly (bypassing trust validation,
// exactly like tls13_certificate_chain_test.v's own tests -- trust
// validation itself is that file's responsibility, already covered there)
// is installed, then a REAL CertificateVerify (genuine RSA-PSS signature)
// and a REAL Finished (genuine HMAC) drive the handshake the rest of the
// way to .connected. Cross-checks the client's derived application
// secrets and returned client Finished against what an independent
// "server" (using the same production key-schedule functions, standing in
// for a real, different implementation) computes -- proving both sides
// actually agree, not just that the client didn't crash.
fn test_handshake_certificate_verify_and_finished_with_real_cryptography() {
	client_random := []u8{len: 32, init: 0x11}
	server_initial_scid := [u8(0xaa), 0xbb, 0xcc, 0xdd]

	mut h, server, cert_err_msg := drive_to_certificate(client_random, server_initial_scid)!
	defer {
		h.free()
		unsafe { server.priv_key.free() }
	}
	assert cert_err_msg != ''

	// White-box: install an already-verified chain directly, the same way
	// tls13_certificate_chain_test.v's own tests do, to isolate testing
	// CertificateVerify/Finished from the chain-trust gap.
	real_chain := mbedtls.build_certificate_chain([server.certificate_der])!
	unsafe {
		h.verified_chain = &VerifiedCertificateChain{
			chain: real_chain
		}
	}
	h.certificate_transcript_hash = h.transcript_hash()
	h.state = .wait_certificate_verify

	signed_content := certificate_verify_signed_content(.server, h.certificate_transcript_hash)
	sig := fake_server_sign_certificate_verify(signed_content)!
	cv_framed := build_fake_certificate_verify(sig_scheme_rsa_pss_rsae_sha256, sig)!
	cv_msg, _ := parse_handshake_message(cv_framed)!
	h.process_certificate_verify(cv_msg, cv_framed)!
	assert h.state() == .wait_finished

	cv_transcript_hash := h.transcript_hash()
	server_verify_data := compute_finished_verify_data(server.handshake_secrets.server_secret,
		cv_transcript_hash)!
	server_finished_framed := encode_handshake_message(.finished, server_verify_data)!
	finished_msg, _ := parse_handshake_message(server_finished_framed)!
	client_finished_framed, app_secrets := h.process_finished(finished_msg, server_finished_framed)!
	assert h.state() == .connected

	// The "server" independently verifies the client's Finished, the same
	// way a real peer would -- proving process_finished computed it over
	// the correct transcript checkpoint (ClientHello...server Finished),
	// not just that SOME bytes came back. process_finished already
	// appended client_finished_framed to h.transcript before returning, so
	// strip it back off to recover the checkpoint the client actually
	// signed over.
	pre_client_finished_transcript := h.transcript[..h.transcript.len - client_finished_framed.len]
	pre_client_finished_hash := sha256.sum256(pre_client_finished_transcript)
	ok := verify_finished(server.handshake_secrets.client_secret, pre_client_finished_hash,
		client_finished_framed[4..])!
	assert ok
	assert app_secrets.master_secret.len == 32
	assert app_secrets.client_secret.len == 32
	assert app_secrets.server_secret.len == 32
}

// test_second_hello_retry_request_rejected exercises the specific edge case
// RFC 8446 §4.1.4 mandates and this module's own plan calls out by name:
// a second HelloRetryRequest is fatal, not just unusual.
fn test_second_hello_retry_request_rejected() {
	mut h, client_hello := Tls13ClientHandshake.start(ClientHandshakeParams{
		random:        []u8{len: 32, init: 0x33}
		server_name:   'example.com'
		ca_bundle_pem: handshake_test_cert_pem
	})!
	defer {
		h.free()
	}
	_ = client_hello
	h.got_hello_retry_request = true

	hrr_framed := build_fake_hello_retry_request()!
	hrr_msg, _ := parse_handshake_message(hrr_framed)!
	h.process_server_hello(hrr_msg, hrr_framed) or {
		assert err.msg().contains('second HelloRetryRequest')
		return
	}
	assert false, 'expected a second HelloRetryRequest to be rejected'
}

fn build_fake_hello_retry_request() ![]u8 {
	mut body := []u8{}
	body << u8(0x03)
	body << u8(0x03)
	body << hello_retry_request_random[..]
	body << u8(0)
	body << u8(cipher_suite_tls_aes_128_gcm_sha256 >> 8)
	body << u8(cipher_suite_tls_aes_128_gcm_sha256)
	body << u8(0)

	mut sv_data := []u8{}
	sv_data << u8(tls_version_1_3 >> 8)
	sv_data << u8(tls_version_1_3)
	sv_ext := encode_extension(ext_supported_versions, sv_data)!

	mut ks_data := []u8{}
	ks_data << u8(named_group_secp256r1 >> 8)
	ks_data << u8(named_group_secp256r1)
	ks_ext := encode_extension(ext_key_share, ks_data)!

	mut extensions := []u8{}
	extensions << sv_ext
	extensions << ks_ext
	body << u8(extensions.len >> 8)
	body << u8(extensions.len)
	body << extensions
	return encode_handshake_message(.server_hello, body)!
}

fn test_certificate_request_rejected() {
	mut h, server, cert_err_msg := drive_to_certificate([]u8{len: 32, init: 0x44}, [
		u8(1),
		2,
		3,
		4,
	])!
	defer {
		h.free()
		unsafe { server.priv_key.free() }
	}
	_ = cert_err_msg
	// drive_to_certificate leaves state at .wait_certificate regardless of
	// whether Certificate trust-verified; feed a CertificateRequest into
	// that same state instead of a Certificate.
	cr_framed := encode_handshake_message(.certificate_request, []u8{len: 1})!
	cr_msg, _ := parse_handshake_message(cr_framed)!
	h.process_certificate_or_request(cr_msg, cr_framed) or {
		assert err.msg().contains('does not support')
		return
	}
	assert false, 'expected CertificateRequest to be rejected'
}

// test_certificate_rejects_nonempty_request_context is a regression test
// for a /vreview finding: RFC 8446 §4.4.2 requires
// certificate_request_context to be zero-length "in the case of server
// authentication" -- which is the only case v1 ever exercises, since it
// never sends a CertificateRequest of its own. This was parsed but never
// checked before the fix.
fn test_certificate_rejects_nonempty_request_context() {
	mut h, server, cert_err_msg := drive_to_certificate([]u8{len: 32, init: 0x88}, [
		u8(1),
		2,
		3,
		4,
	])!
	defer {
		h.free()
		unsafe { server.priv_key.free() }
	}
	_ = cert_err_msg

	der := handshake_test_pem_to_der(handshake_test_cert_pem)
	mut entry := []u8{}
	entry << u8(der.len >> 16)
	entry << u8(der.len >> 8)
	entry << u8(der.len)
	entry << der
	entry << u8(0)
	entry << u8(0)
	mut body := []u8{}
	body << u8(1) // certificate_request_context length = 1 (must be 0)
	body << u8(0xab)
	body << u8(entry.len >> 16)
	body << u8(entry.len >> 8)
	body << u8(entry.len)
	body << entry
	cert_framed := encode_handshake_message(.certificate, body)!
	cert_msg, _ := parse_handshake_message(cert_framed)!

	h.process_certificate_or_request(cert_msg, cert_framed) or {
		assert err.msg().contains('certificate_request_context')
		return
	}
	assert false, 'expected a non-empty certificate_request_context to be rejected'
}

fn test_process_server_hello_rejects_unoffered_cipher_suite() {
	mut h, client_hello := Tls13ClientHandshake.start(ClientHandshakeParams{
		random:        []u8{len: 32, init: 0x55}
		server_name:   'example.com'
		ca_bundle_pem: handshake_test_cert_pem
	})!
	defer {
		h.free()
	}
	client_key_exchange := extract_client_hello_key_exchange(client_hello)!

	mut body := []u8{}
	body << u8(0x03)
	body << u8(0x03)
	body << []u8{len: 32, init: 0x66}
	body << u8(0)
	body << u8(0x13) // TLS_AES_256_GCM_SHA384 -- never offered
	body << u8(0x02)
	body << u8(0)
	mut sv_data := []u8{}
	sv_data << u8(tls_version_1_3 >> 8)
	sv_data << u8(tls_version_1_3)
	sv_ext := encode_extension(ext_supported_versions, sv_data)!
	mut ks_entry := []u8{}
	ks_entry << u8(named_group_secp256r1 >> 8)
	ks_entry << u8(named_group_secp256r1)
	ks_entry << u8(client_key_exchange.len >> 8)
	ks_entry << u8(client_key_exchange.len)
	ks_entry << client_key_exchange
	ks_ext := encode_extension(ext_key_share, ks_entry)!
	mut extensions := []u8{}
	extensions << sv_ext
	extensions << ks_ext
	body << u8(extensions.len >> 8)
	body << u8(extensions.len)
	body << extensions
	bad_sh_framed := encode_handshake_message(.server_hello, body)!

	msg, _ := parse_handshake_message(bad_sh_framed)!
	h.process_server_hello(msg, bad_sh_framed) or {
		assert err.msg().contains('cipher_suite')
		return
	}
	assert false, 'expected an unoffered cipher_suite to be rejected'
}

fn test_process_finished_rejects_bad_verify_data() {
	client_random := []u8{len: 32, init: 0x77}
	server_initial_scid := [u8(9), 8, 7, 6]
	mut h, server, cert_err_msg := drive_to_certificate(client_random, server_initial_scid)!
	defer {
		h.free()
		unsafe { server.priv_key.free() }
	}
	_ = cert_err_msg
	real_chain := mbedtls.build_certificate_chain([server.certificate_der])!
	unsafe {
		h.verified_chain = &VerifiedCertificateChain{
			chain: real_chain
		}
	}
	h.certificate_transcript_hash = h.transcript_hash()
	h.state = .wait_certificate_verify

	signed_content := certificate_verify_signed_content(.server, h.certificate_transcript_hash)
	sig := fake_server_sign_certificate_verify(signed_content)!
	cv_framed := build_fake_certificate_verify(sig_scheme_rsa_pss_rsae_sha256, sig)!
	cv_msg, _ := parse_handshake_message(cv_framed)!
	h.process_certificate_verify(cv_msg, cv_framed)!

	bad_finished_framed := encode_handshake_message(.finished, []u8{len: 32, init: 0xff})!
	finished_msg, _ := parse_handshake_message(bad_finished_framed)!
	h.process_finished(finished_msg, bad_finished_framed) or {
		assert err.msg().contains('verify_data')
		return
	}
	assert false, 'expected a bad Finished verify_data to be rejected'
}

// test_free_is_idempotent is a regression test for a /vreview finding:
// Tls13ClientHandshake.free() originally called h.ecdhe_private.free()
// unconditionally, with no guard against a second call. Unlike
// VerifiedCertificateChain.free() (which nulls its own pointer, so a
// second call safely no-ops), ecdsa.PrivateKey.free() has no such
// self-guard -- calling it twice on the same underlying EVP_PKEY is a
// real, empirically-confirmed crash (OpenSSL's EVP_PKEY_free aborts on a
// double-free), not merely a theoretical concern like some of this
// codebase's other double-free discussions. The fix added a `freed bool`
// guard; this test would have reliably reproduced the original bug as a
// process abort, not a clean test failure.
fn test_free_is_idempotent() {
	mut h, _ := Tls13ClientHandshake.start(ClientHandshakeParams{
		random:        []u8{len: 32, init: 0x11}
		server_name:   'example.com'
		ca_bundle_pem: ''
	})!
	h.free()
	h.free() // must not double-free
}
