// Copyright (c) 2026 blackshirt.
//
// full_flight_test.v drives the real handshake core against the adaptive
// fakeserver through complete TLS 1.3 handshakes — both HelloRetryRequest
// shapes and the negative flights — without sockets or OpenSSL.
module handshake

import tls.fakeserver
import tls.pki

// fixture_trusted_validator builds a Validator over the fakeserver's embedded
// test CA so the Certificate message authenticates exactly the fake chain.
fn fixture_trusted_validator() pki.Validator {
	ts := fakeserver.test_trust_store() or { panic('fakeserver trust store: ${err.msg()}') }
	return pki.new_validator(ts)
}

// expect_no_traffic_keys reports whether traffic_keys() refuses — the expected
// state once zeroize_traffic_secrets() dropped all traffic material.
// Only the error is inspected: a refused generation must not leak a handle.
fn expect_no_traffic_keys(h &HandshakeCore) bool {
	mut refused := false
	h.traffic_keys() or {
		refused = true
	}
	return refused
}

// expect_no_exported_handshake_secrets reports whether the handshake traffic
// secrets are gone — the expected state once they were wiped.
fn expect_no_exported_handshake_secrets(h &HandshakeCore) bool {
	return h.c_hsk_tsec.len == 0 && h.s_hsk_tsec.len == 0
}

// assert_handshake_material_destroyed checks the failure-path contract: after a
// failed feed the machine is .failed and every derived secret is gone (RFC 8446 §7.5).
fn assert_handshake_material_destroyed(h &HandshakeCore) {
	assert h.state() == .failed
	assert expect_no_traffic_keys(h), 'traffic keys survived a failed feed'
	assert expect_no_exported_handshake_secrets(h), 'handshake traffic secrets survived a failed feed'
}

// feed_encrypted_flight feeds the whole server flight one byte at a time and
// returns every outbound message, exercising reassembly at every split point.
fn feed_encrypted_flight(mut h HandshakeCore, flight []u8) ![]HandshakeMessage {
	mut produced := []HandshakeMessage{}
	for i in 0 .. flight.len {
		msgs := h.receive_handshake_bytes([flight[i]])!
		produced << msgs
	}
	return produced
}

// test_client_auth_request_answered_with_empty_certificate checks that a
// CertificateRequest is answered with an empty Certificate (§4.4.2).
fn test_client_auth_request_answered_with_empty_certificate() ! {
	mut h := new_tls_handshake(fakeserver.server_name, HandshakeOptions{ validator: fixture_trusted_validator() })!
	mut srv := fakeserver.new_server(client_auth: true)!

	ch := h.create_client_hello()!
	fl := srv.handle_client_hello(ch)!
	assert !fl.is_hrr
	h.receive_initial_bytes(fl.server_hello)!

	// EE + CertificateRequest: the client must emit exactly its empty Certificate.
	assert fl.rest.len == 2
	produced := feed_encrypted_flight(mut h, fakeserver.flatten(fl.rest))!
	assert produced.len == 1
	assert h.state() == .wait_cert

	// The server validates the echo and returns the deferred Cert/CV/Fin tail.
	tail := srv.handle_client_certificate(produced[0].msg)!
	assert tail.len == 3

	// The tail completes the handshake and yields the client Finished.
	out := feed_encrypted_flight(mut h, fakeserver.flatten(tail))!
	assert out.len == 1
	assert h.state() == .connected
	srv.handle_client_finished(out[0].msg)!

	assert h.server_certs.len == 1
}

// test_full_handshake_reaches_connected_byte_by_byte checks that a full
// handshake reaches .connected with byte-correct secrets, fed one byte at a time.
fn test_full_handshake_reaches_connected_byte_by_byte() ! {
	mut h := new_tls_handshake(fakeserver.server_name, HandshakeOptions{ alpn: ['h2'], validator: fixture_trusted_validator() })!
	mut srv := fakeserver.new_server(alpn: ['h2', 'http/1.1'])!

	ch := h.create_client_hello()!
	fl := srv.handle_client_hello(ch)!
	assert !fl.is_hrr

	// Plaintext ServerHello on the Initial level; nothing outbound yet.
	out := h.receive_initial_bytes(fl.server_hello)!
	assert out.len == 0
	assert h.state() == .wait_ee

	// The encrypted flight, one byte at a time, across every reassembly boundary.
	produced := feed_encrypted_flight(mut h, fakeserver.flatten(fl.rest))!
	assert produced.len == 1
	assert h.state() == .connected

	// The server must accept the exact client Finished the core produced.
	srv.handle_client_finished(produced[0].msg)!

	assert h.negotiated_alpn() == 'h2'
	assert h.server_certs.len == 1

	// Application traffic keys are the reusable post-handshake state (§7.1). The
	// generation arrives as one opaque handle per direction, so every length is
	// asserted by name instead of by tuple position.
	mut keys := h.traffic_keys()!
	assert keys.is_ready()
	mut client := keys.take_client_material()
	mut server := keys.take_server_material()
	assert client.take_key().len == 16
	assert client.take_iv().len == 12
	assert server.take_key().len == 16
	assert server.take_iv().len == 12
	keys.wipe()
	client.wipe()
	server.wipe()

	// All traffic secrets are dropped at teardown (RFC 8446 §7.5).
	h.zeroize_traffic_secrets()!
	assert expect_no_traffic_keys(&h)
}

// test_hello_retry_request_change_group_reaches_connected checks that the
// change_group retry round trip completes (RFC 8446 §4.1.4).
fn test_hello_retry_request_change_group_reaches_connected() ! {
	mut h := new_tls_handshake(fakeserver.server_name, HandshakeOptions{ validator: fixture_trusted_validator() })!
	mut srv := fakeserver.new_server(hrr: .change_group)!

	ch := h.create_client_hello()!
	fl1 := srv.handle_client_hello(ch)!
	assert fl1.is_hrr

	// The HRR arrives at Initial level; the core answers with the retry ClientHello.
	out := h.receive_initial_bytes(fl1.server_hello)!
	assert out.len == 1
	assert h.state() == .wait_sh

	fl2 := srv.handle_client_hello(out[0].msg)!
	assert !fl2.is_hrr
	out2 := h.receive_initial_bytes(fl2.server_hello)!
	assert out2.len == 0
	assert h.state() == .wait_ee

	produced := h.receive_handshake_bytes(fakeserver.flatten(fl2.rest))!
	assert produced.len == 1
	srv.handle_client_finished(produced[0].msg)!
	assert h.state() == .connected
	assert h.negotiated_alpn() == ''
}

// test_hello_retry_request_cookie_only_reaches_connected checks that the
// cookie retry round trip completes with the echoed cookie (RFC 8446 §4.1.4).
fn test_hello_retry_request_cookie_only_reaches_connected() ! {
	mut h := new_tls_handshake(fakeserver.server_name, HandshakeOptions{ validator: fixture_trusted_validator() })!
	mut srv := fakeserver.new_server(hrr: .cookie_only)!

	ch := h.create_client_hello()!
	fl1 := srv.handle_client_hello(ch)!
	assert fl1.is_hrr

	out := h.receive_initial_bytes(fl1.server_hello)!
	assert out.len == 1
	// The retry ClientHello must carry the echoed cookie; the fakeserver verifies it.
	fl2 := srv.handle_client_hello(out[0].msg)!
	assert !fl2.is_hrr

	out2 := h.receive_initial_bytes(fl2.server_hello)!
	assert out2.len == 0
	produced := h.receive_handshake_bytes(fakeserver.flatten(fl2.rest))!
	assert produced.len == 1
	srv.handle_client_finished(produced[0].msg)!
	assert h.state() == .connected
}

// test_rejects_bad_server_finished checks that a corrupted server Finished
// aborts the handshake with a decrypt_error alert and no surviving secrets.
fn test_rejects_bad_server_finished() ! {
	mut h := new_tls_handshake(fakeserver.server_name, HandshakeOptions{ validator: fixture_trusted_validator() })!
	mut srv := fakeserver.new_server(bad_finished: true)!

	ch := h.create_client_hello()!
	fl := srv.handle_client_hello(ch)!
	h.receive_initial_bytes(fl.server_hello)!

	h.receive_handshake_bytes(fakeserver.flatten(fl.rest)) or {
		assert err.msg().contains('Finished verify_data mismatch')
		assert h.failure_alert() == .decrypt_error
		assert_handshake_material_destroyed(h)
		return
	}
	assert false, 'a corrupted server Finished must abort the handshake'
}

// test_rejects_bad_certificate_verify checks that a corrupted CertificateVerify
// aborts the handshake with a decrypt_error alert and no surviving secrets.
fn test_rejects_bad_certificate_verify() ! {
	mut h := new_tls_handshake(fakeserver.server_name, HandshakeOptions{ validator: fixture_trusted_validator() })!
	mut srv := fakeserver.new_server(bad_certificate_verify: true)!

	ch := h.create_client_hello()!
	fl := srv.handle_client_hello(ch)!
	h.receive_initial_bytes(fl.server_hello)!

	h.receive_handshake_bytes(fakeserver.flatten(fl.rest)) or {
		assert err.msg().contains('Ed25519 CertificateVerify failed')
		assert h.failure_alert() == .decrypt_error
		assert_handshake_material_destroyed(h)
		return
	}
	assert false, 'a corrupted CertificateVerify must abort the handshake'
}

// test_rejects_out_of_order_certificate checks that a Certificate where
// EncryptedExtensions is expected is rejected (§4.4.1).
fn test_rejects_out_of_order_certificate() ! {
	mut h := new_tls_handshake(fakeserver.server_name, HandshakeOptions{ validator: fixture_trusted_validator() })!
	mut srv := fakeserver.new_server()!

	ch := h.create_client_hello()!
	fl := srv.handle_client_hello(ch)!
	h.receive_initial_bytes(fl.server_hello)!

	// fl.rest is [EE, Cert, CV, Fin]; feed Certificate where EncryptedExtensions is expected.
	assert fl.rest.len == 4
	h.receive_handshake_bytes(fl.rest[1]) or {
		assert err.msg().contains('unexpected Certificate')
		assert h.failure_alert() == .unexpected_message
		assert_handshake_material_destroyed(h)
		return
	}
	assert false, 'an out-of-order Certificate must abort the handshake'
}

// test_rejects_unoffered_alpn checks that an unoffered ALPN pick aborts rather
// than negotiating a protocol the client never offered (RFC 7301 §3.1).
fn test_rejects_unoffered_alpn() ! {
	mut h := new_tls_handshake(fakeserver.server_name, HandshakeOptions{ validator: fixture_trusted_validator() })!
	mut srv := fakeserver.new_server(force_alpn: 'h2')!

	ch := h.create_client_hello()!
	fl := srv.handle_client_hello(ch)!
	h.receive_initial_bytes(fl.server_hello)!

	h.receive_handshake_bytes(fakeserver.flatten(fl.rest)) or {
		assert err.msg().contains('not offered by client')
		assert h.failure_alert() == .illegal_parameter
		assert_handshake_material_destroyed(h)
		return
	}
	assert false, 'an unoffered ALPN selection must abort the handshake'
}

// test_rejects_substring_alpn checks that an ALPN pick which is only a substring
// of an offered protocol is refused — element membership, not substring matching
// (RFC 7301 §3.1).
fn test_rejects_substring_alpn() ! {
	mut h := new_tls_handshake(fakeserver.server_name, HandshakeOptions{ alpn: ['http/1.1'], validator: fixture_trusted_validator() })!
	// '1.1' is contained in the offered 'http/1.1' but was never offered itself.
	mut srv := fakeserver.new_server(force_alpn: '1.1')!

	ch := h.create_client_hello()!
	fl := srv.handle_client_hello(ch)!
	h.receive_initial_bytes(fl.server_hello)!

	h.receive_handshake_bytes(fakeserver.flatten(fl.rest)) or {
		assert err.msg().contains('not offered by client')
		assert h.failure_alert() == .illegal_parameter
		assert_handshake_material_destroyed(h)
		return
	}
	assert false, 'a substring ALPN pick must abort the handshake'
}

// test_fail_before_secrets_is_safe checks failure cleanup before anything is derived.
fn test_fail_before_secrets_is_safe() ! {
	mut h := new_tls_handshake(fakeserver.server_name, HandshakeOptions{ validator: fixture_trusted_validator() })!
	h.fail_and_zeroize_secrets()
	assert h.state() == .failed
	assert expect_no_traffic_keys(h)
	assert expect_no_exported_handshake_secrets(h)
}

// test_malformed_server_hello_aborts_and_destroys checks that a ServerHello
// which cannot parse still parks the machine in .failed with no keys left.
fn test_malformed_server_hello_aborts_and_destroys() ! {
	mut h := new_tls_handshake(fakeserver.server_name, HandshakeOptions{ validator: fixture_trusted_validator() })!
	_ := h.create_client_hello()!
	// A complete ServerHello header with an empty body: parse fails before the key schedule ran.
	h.receive_initial_bytes([u8(0x02), 0x00, 0x00, 0x00]) or {
		assert h.state() == .failed
		assert expect_no_traffic_keys(h)
		return
	}
	assert false, 'a malformed ServerHello must abort the handshake'
}

// test_default_failure_alert_is_internal_error checks that an unclassified
// failure reports internal_error, the §6 catch-all.
fn test_default_failure_alert_is_internal_error() ! {
	mut h := new_tls_handshake(fakeserver.server_name, HandshakeOptions{ validator: fixture_trusted_validator() })!
	_ := h.create_client_hello()!
	assert h.failure_alert() == .internal_error
}

// test_connected_destroys_handshake_secret_keeps_app_secrets locks the §7.5 retention
// ordering the split depends on end-to-end: the Master Secret is derived from the
// retained Handshake Secret (so the per-direction app derivers still get it), the
// caller-side master_secret wipe runs, and the Handshake Secret intermediate is
// destroyed at .connected while the application traffic secrets it fed stay live.
fn test_connected_destroys_handshake_secret_keeps_app_secrets() ! {
	mut h := new_tls_handshake(fakeserver.server_name, HandshakeOptions{ validator: fixture_trusted_validator() })!
	mut srv := fakeserver.new_server()!

	ch := h.create_client_hello()!
	fl := srv.handle_client_hello(ch)!
	h.receive_initial_bytes(fl.server_hello)!
	produced := feed_encrypted_flight(mut h, fakeserver.flatten(fl.rest))!
	assert produced.len == 1
	srv.handle_client_finished(produced[0].msg)!
	assert h.state() == .connected

	// The key-schedule intermediate is gone (wipe_key_schedule_secrets), proving the
	// Master Secret was derived from the retained Handshake Secret before destruction.
	assert h.handshake_secret.len == 0
	// The application traffic secrets the split derivers produced from the caller-owned
	// Master Secret survive.
	assert h.c_app_tsec.len > 0 && h.s_app_tsec.len > 0
	assert h.state() == .connected
	// The record accessor derives the key/iv pair the transport consumes on demand.
	mut app_keys := h.traffic_keys()!
	mut app_client := app_keys.take_client_material()
	mut app_server := app_keys.take_server_material()
	assert app_client.take_key().len == 16 && app_client.take_iv().len == 12
	assert app_server.take_key().len == 16 && app_server.take_iv().len == 12
	app_keys.wipe()
	app_client.wipe()
	app_server.wipe()
}
