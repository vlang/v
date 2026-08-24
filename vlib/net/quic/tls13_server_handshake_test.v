// vtest build: present_openssl?
module quic

import crypto.ecdsa
import crypto.sha256

// server_handshake_test_client_params builds a valid, deterministic
// ClientHandshakeParams -- the exact same shape a real client would send,
// reused across every test in this file so each one only varies what it's
// actually testing.
fn server_handshake_test_client_params() ClientHandshakeParams {
	return ClientHandshakeParams{
		random:               []u8{len: 32, init: 0x11}
		server_name:          'example.com'
		transport_parameters: QuicTransportParameters{
			initial_source_connection_id: []u8{len: 8, init: 0xaa}
		}
		ca_bundle_pem:        ''
		alpn_protocols:       ['h3']
	}
}

// server_handshake_test_server_params builds a valid, deterministic
// ServerHandshakeParams. `signing_key` is generated fresh per call (cheap,
// P-256) rather than shared, so tests never accidentally depend on
// call-ordering.
fn server_handshake_test_server_params() !ServerHandshakeParams {
	_, signing_key := ecdsa.generate_key()!
	return ServerHandshakeParams{
		transport_parameters:     QuicTransportParameters{
			initial_source_connection_id:       []u8{len: 8, init: 0xbb}
			original_destination_connection_id: []u8{len: 8, init: 0xaa}
		}
		supported_alpn_protocols: ['h3']
		certificate_chain:        [
			CertificateEntry{
				cert_data: []u8{len: 200, init: 0x30}
			},
		]
		signing_key:              signing_key
		server_hello_random:      []u8{len: 32, init: 0x22}
	}
}

// build_test_client_hello_body constructs a minimal but structurally valid
// ClientHello message BODY directly from this module's own low-level
// extension encoders (encode_supported_versions_extension,
// encode_key_share_extension, ...), with a caller-controlled
// transport_parameters -- unlike build_client_hello, this deliberately
// bypasses that function's own role-restriction checks (it refuses to
// encode a client-illegal transport parameter at all), so a test can
// construct the wire bytes a hand-crafted malicious or buggy client would
// send. encode_transport_parameters itself enforces no role restriction
// (that's the RECEIVING side's job, per its own doc comment) -- which is
// exactly what makes this possible.
fn build_test_client_hello_body(transport_parameters QuicTransportParameters) ![]u8 {
	mut body := []u8{}
	body << u8(0x03)
	body << u8(0x03)
	body << []u8{len: 32}
	body << u8(0) // empty legacy_session_id
	body << u8(0)
	body << u8(2)
	body << u8(cipher_suite_tls_aes_128_gcm_sha256 >> 8)
	body << u8(cipher_suite_tls_aes_128_gcm_sha256)
	body << u8(1)
	body << u8(0)

	mut extensions := []u8{}
	extensions << encode_supported_versions_extension()!
	extensions << encode_signature_algorithms_extension()!
	extensions << encode_alpn_extension(['h3'])!
	extensions << encode_key_share_extension(named_group_secp256r1, []u8{len: 65, init: 0x04})!
	extensions << encode_quic_transport_parameters_extension(transport_parameters)!

	body << u8(extensions.len >> 8)
	body << u8(extensions.len)
	body << extensions
	return body
}

// test_server_handshake_full_flow_agrees_with_real_client is the primary
// integration test: a REAL Tls13ClientHandshake and a REAL
// Tls13ServerHandshake run against each other, each independently written
// against the RFC text (never against each other), with fresh ECDHE
// keypairs on both sides -- not fixed RFC 8448 vectors. This is a strictly
// stronger cross-check than either side's own vector-based tests: it
// proves the two independently-implemented roles actually agree on wire
// bytes and derived secrets when talking to EACH OTHER, not just that each
// one matches a canned expected value in isolation.
//
// Certificate/CertificateVerify chain verification is NOT exercised here:
// this repo has no EC self-signed certificate fixture (only an RSA one,
// used by tls13_handshake_test.v's OWN fake-server tests, which
// encode_certificate_verify can't sign with -- RSA-PSS signing isn't wired
// up yet, see that function's own doc comment). This is the SAME documented
// gap already stated on encode_certificate_verify's own tests
// (tls13_certificate_test.v) and Phase 2c's x509_standalone_signature_test.v
// before it -- not silently skipped, stated here for the same reason.
// What IS proven below: real ECDH agreement, Handshake secret agreement,
// EncryptedExtensions/ALPN/transport-parameter cross-validation via the
// client's own real process_encrypted_extensions, and both directions'
// Finished messages verifying via each side's own independently-tested
// verify_finished/build_finished.
fn test_server_handshake_full_flow_agrees_with_real_client() {
	mut client_h, client_hello := Tls13ClientHandshake.start(server_handshake_test_client_params())!
	defer {
		client_h.free()
	}
	ch_msg, ch_consumed := parse_handshake_message(client_hello)!
	assert ch_consumed == client_hello.len

	server_params := server_handshake_test_server_params()!
	mut server_h, flight := Tls13ServerHandshake.respond_to_client_hello(ch_msg, client_hello,
		server_params)!
	defer {
		server_h.free()
	}
	assert server_h.state() == .wait_finished
	assert flight.negotiated_alpn == 'h3'

	// Cross-check #1: the client's own real process_server_hello, given
	// the server's real ServerHello, derives the EXACT SAME Handshake
	// secrets the server itself computed -- proving the ECDH + key
	// schedule genuinely agree between two independent implementations of
	// each role, not just that each one is internally consistent.
	sh_msg, sh_consumed := parse_handshake_message(flight.server_hello)!
	assert sh_consumed == flight.server_hello.len
	client_handshake_secrets := client_h.process_server_hello(sh_msg, flight.server_hello)!
	assert client_handshake_secrets.handshake_secret == flight.handshake_secrets.handshake_secret
	assert client_handshake_secrets.client_secret == flight.handshake_secrets.client_secret
	assert client_handshake_secrets.server_secret == flight.handshake_secrets.server_secret
	assert client_h.state() == .wait_encrypted_extensions

	// Split the flight's Handshake-level messages back into their 4
	// individually-framed pieces (EncryptedExtensions, Certificate,
	// CertificateVerify, Finished) -- a real caller would already have
	// these as separate CRYPTO-stream reads; this test reconstructs that
	// split from the concatenated flight the same way it was built.
	ee_msg, ee_consumed := parse_handshake_message(flight.handshake_messages)!
	ee_framed := flight.handshake_messages[..ee_consumed]
	rest_after_ee := flight.handshake_messages[ee_consumed..]

	cert_msg, cert_consumed := parse_handshake_message(rest_after_ee)!
	cert_framed := rest_after_ee[..cert_consumed]
	rest_after_cert := rest_after_ee[cert_consumed..]

	cv_msg, cv_consumed := parse_handshake_message(rest_after_cert)!
	cv_framed := rest_after_cert[..cv_consumed]
	rest_after_cv := rest_after_cert[cv_consumed..]

	fin_msg, fin_consumed := parse_handshake_message(rest_after_cv)!
	assert fin_consumed == rest_after_cv.len // Finished is the last message in the flight
	assert ee_msg.typ == .encrypted_extensions
	assert cert_msg.typ == .certificate
	assert cv_msg.typ == .certificate_verify
	assert fin_msg.typ == .finished

	// Cross-check #2: the client's own real process_encrypted_extensions
	// accepts the server's real EncryptedExtensions -- ALPN selection and
	// every RFC 9000 §7.3 connection-ID/transport-parameter cross-check
	// included. peer_initial_scid/original_dcid are this test's server/
	// client transport parameters' own values, matching what a real
	// caller would have observed on the wire (the packet header SCID/DCID)
	// rather than trusted blindly from the transport parameters alone.
	client_h.process_encrypted_extensions(ee_msg, ee_framed, []u8{len: 8, init: 0xbb},
		[]u8{len: 8, init: 0xaa}, none)!
	assert client_h.negotiated_alpn()? == 'h3'
	assert client_h.state() == .wait_certificate

	// Cross-check #3: the client's own real, RFC-8448-vector-tested
	// verify_finished accepts the server's real build_finished output --
	// proving the two independently-written Finished implementations
	// genuinely agree, not just that each one accepts its own output.
	mut transcript_before_finished := []u8{}
	transcript_before_finished << client_hello
	transcript_before_finished << flight.server_hello
	transcript_before_finished << ee_framed
	transcript_before_finished << cert_framed
	transcript_before_finished << cv_framed
	ok := verify_finished(flight.handshake_secrets.server_secret,
		sha256.sum256(transcript_before_finished), fin_msg.body)!
	assert ok

	// Cross-check #4: a real client Finished, built via build_finished
	// (the same function under test on the server's own side, applied to
	// the CLIENT's secret this time), is accepted by the server's own
	// process_finished -- closing the loop on both directions.
	client_finished := build_finished(flight.handshake_secrets.client_secret,
		sha256.sum256(server_h.transcript))!
	cf_msg, cf_consumed := parse_handshake_message(client_finished)!
	assert cf_consumed == client_finished.len
	server_h.process_finished(cf_msg, client_finished)!
	assert server_h.state() == .connected
}

fn test_server_handshake_process_finished_rejects_tampered_verify_data() {
	mut client_h, client_hello := Tls13ClientHandshake.start(server_handshake_test_client_params())!
	defer {
		client_h.free()
	}
	ch_msg, _ := parse_handshake_message(client_hello)!
	server_params := server_handshake_test_server_params()!
	mut server_h, flight := Tls13ServerHandshake.respond_to_client_hello(ch_msg, client_hello,
		server_params)!
	defer {
		server_h.free()
	}

	mut tampered := build_finished(flight.handshake_secrets.client_secret,
		sha256.sum256(server_h.transcript))!
	tampered[tampered.len - 1] ^= 0xff
	cf_msg, _ := parse_handshake_message(tampered)!
	server_h.process_finished(cf_msg, tampered) or {
		assert err.msg().contains('does not match')
		assert server_h.state() == .wait_finished // must NOT have advanced
		return
	}
	assert false, 'expected an error for a tampered client Finished'
}

fn test_server_handshake_rejects_unoffered_cipher_suite() {
	body := build_test_client_hello_body(QuicTransportParameters{
		initial_source_connection_id: []u8{len: 8}
	})!
	// build_test_client_hello_body always offers TLS_AES_128_GCM_SHA256 at
	// this fixed offset (legacy_version(2) + random(32) +
	// legacy_session_id_len(1) + cipher_suites_len(2) = 37) -- splice in a
	// different suite value to exercise the rejection.
	mut bad_body := body.clone()
	bad_body[37] = 0x13
	bad_body[38] = 0x02 // TLS_AES_256_GCM_SHA384, never offered/supported here
	bad_msg := HandshakeMessage{
		typ:  .client_hello
		body: bad_body
	}
	bad_framed := encode_handshake_message(.client_hello, bad_body)!
	server_params := server_handshake_test_server_params()!
	Tls13ServerHandshake.respond_to_client_hello(bad_msg, bad_framed, server_params) or {
		assert err.msg().contains('TLS_AES_128_GCM_SHA256')
		return
	}
	assert false, 'expected an error for a ClientHello offering no supported cipher suite'
}

fn test_server_handshake_rejects_alpn_mismatch() {
	client_hello := build_client_hello(
		random:               []u8{len: 32}
		server_name:          'example.com'
		ecdhe_public_key:     []u8{len: 65, init: 0x04}
		transport_parameters: QuicTransportParameters{
			initial_source_connection_id: []u8{len: 8}
		}
		alpn_protocols:       ['h2'] // this server (below) only supports 'h3'
	)!
	msg, _ := parse_handshake_message(client_hello)!
	server_params := server_handshake_test_server_params()!
	Tls13ServerHandshake.respond_to_client_hello(msg, client_hello, server_params) or {
		assert err.msg().contains('no ALPN protocol in common')
		return
	}
	assert false, 'expected an error for no ALPN protocol in common'
}

fn test_server_handshake_rejects_server_only_transport_parameter_from_client() {
	body := build_test_client_hello_body(QuicTransportParameters{
		initial_source_connection_id: []u8{len: 8}
		stateless_reset_token:        []u8{len: 16, init: 0x01} // client MUST NOT send this
	})!
	msg := HandshakeMessage{
		typ:  .client_hello
		body: body
	}
	framed := encode_handshake_message(.client_hello, body)!
	server_params := server_handshake_test_server_params()!
	Tls13ServerHandshake.respond_to_client_hello(msg, framed, server_params) or {
		assert err.msg().contains('stateless_reset_token')
		assert err.code() == int(quic_error_transport_parameter)
		return
	}
	assert false, 'expected an error for a client-sent, server-only stateless_reset_token'
}

fn test_server_handshake_propagates_nonempty_session_id_error_code() {
	// A regression test that respond_to_client_hello's error wrapper
	// (`if err.code() != 0 { return err }`) actually preserves
	// parse_client_hello's own PROTOCOL_VIOLATION code for this case,
	// rather than remapping it to the generic decode_error alert -- the
	// exact class of wrapper bug process_server_hello/process_encrypted_
	// extensions's own doc comments warn against getting wrong.
	mut body := []u8{}
	body << u8(0x03)
	body << u8(0x03)
	body << []u8{len: 32}
	body << u8(1) // non-empty session id -- always wrong, RFC 9001 §8.4
	body << u8(0xaa)
	body << u8(0)
	body << u8(2)
	body << u8(cipher_suite_tls_aes_128_gcm_sha256 >> 8)
	body << u8(cipher_suite_tls_aes_128_gcm_sha256)
	body << u8(1)
	body << u8(0)
	body << u8(0)
	body << u8(0)
	msg := HandshakeMessage{
		typ:  .client_hello
		body: body
	}
	framed := encode_handshake_message(.client_hello, body)!
	server_params := server_handshake_test_server_params()!
	Tls13ServerHandshake.respond_to_client_hello(msg, framed, server_params) or {
		assert err.code() == int(quic_error_protocol_violation)
		return
	}
	assert false, 'expected an error for a non-empty legacy_session_id'
}
