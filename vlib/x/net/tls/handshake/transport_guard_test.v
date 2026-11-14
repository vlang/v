// Copyright (c) 2026 blackshirt.
//
// transport_guard_test.v locks the is_quic guards on the unified HandshakeCore:
// a TLS-record-only call on a QUIC-mode core, and a QUIC-only call on a TLS-mode
// core, must refuse with a plain error — no state change, no teardown, no key
// material touched — mirroring the out-of-phase guard convention. The per-phase
// key accessors are no longer guard cases: both modes answer the same call, in
// their own leg shape, which the handle-shape test below locks down.
module handshake

import tls.pki
import tls.wire

// fixture_validator builds a Validator over an empty trust store; none of these
// tests reach chain verification. Defined locally because V compiles each
// _test.v file as its own program, so helpers are not shared across test files.
fn fixture_validator() pki.Validator {
	return pki.new_validator(pki.new_trust_store())
}

// wrap_hsk frames a handshake body with its 4-byte header, mirroring the core's
// own framing so tests can plant complete messages on a level stream.
fn wrap_hsk(typ HandshakeType, body []u8) []u8 {
	mut w := wire.new_writer(4 + body.len)
	w.write_u8(u8(typ))
	w.write_u24(u32(body.len))
	w.write_bytes(body)
	return w.bytes()
}

fn tls_mode_core() !HandshakeCore {
	return new_tls_handshake('example.com', HandshakeOptions{ validator: fixture_validator() })
}

fn quic_mode_core() !HandshakeCore {
	return new_quic_handshake('example.com', HandshakeOptions{ validator: fixture_validator() })
}

// test_tls_mode_refuses_quic_only_calls covers the QUIC-only seams on a TLS-mode
// core: transport parameters and the HelloRetryRequest flag. Each must refuse,
// and the guards must not tear the machine down.
fn test_tls_mode_refuses_quic_only_calls() ! {
	mut h := tls_mode_core()!
	_ = h.create_client_hello()!
	mut rejected := false

	h.peer_transport_parameters() or {
		rejected = err.msg().contains('QUIC-only')
	}
	assert rejected
	rejected = false
	h.is_hello_retry_request_received() or {
		rejected = err.msg().contains('QUIC-only')
	}
	assert rejected

	// Plain guards: no classify_failure, no teardown — state and alert untouched.
	assert h.state() == .wait_sh
	assert h.failure_alert() == .internal_error
}

// test_packet_key_legs_fill_hp_where_record_legs_do_not locks the unified handle's
// transport shape on the derivation side, through the ONE accessor name both
// transports call: a QUIC-mode core answers with all three KeyMaterial slots filled
// (RFC 9001 §5.1), while a TLS-mode core hands over an empty hp for the same phase's
// secrets (record headers are unprotected, §5.2).
fn test_packet_key_legs_fill_hp_where_record_legs_do_not() ! {
	// QUIC side: planted handshake secrets derive full packet legs.
	mut q := quic_mode_core()!
	_ = q.create_client_hello()!
	q.c_hsk_tsec = []u8{len: 32, init: 1}
	q.s_hsk_tsec = []u8{len: 32, init: 2}
	q.state = HandshakeState.wait_ee
	mut gen := q.traffic_keys()!
	assert gen.is_ready()
	mut client := gen.take_client_material()
	assert client.key.len == q.key_len && client.iv.len == aead_nonce_len && client.hp.len == q.key_len
	mut server := gen.take_server_material()
	assert server.key.len == q.key_len && server.iv.len == aead_nonce_len && server.hp.len == q.key_len
	client.wipe()
	server.wipe()
	gen.wipe()

	// TLS side: the same accessor name on a TLS-mode core leaves hp genuinely empty.
	mut t := tls_mode_core()!
	_ = t.create_client_hello()!
	t.c_hsk_tsec = []u8{len: 32, init: 1}
	t.s_hsk_tsec = []u8{len: 32, init: 2}
	t.state = HandshakeState.wait_ee
	mut rec_gen := t.traffic_keys()!
	mut rec_client := rec_gen.take_client_material()
	assert rec_client.key.len == t.key_len && rec_client.iv.len == aead_nonce_len
	assert rec_client.take_hp().len == 0
	rec_client.wipe()
	rec_gen.wipe()
}

// test_quic_mode_refuses_tls_only_calls covers the TLS-record surface on a
// QUIC-mode core: KeyUpdate, reassembly_len accessor, and the
// coalesced-bytes handoff. The key accessors are absent: they answer in QUIC mode
// instead of refusing.
fn test_quic_mode_refuses_tls_only_calls() ! {
	mut h := quic_mode_core()!
	_ = h.create_client_hello()!
	mut rejected := false

	h.next_application_key_material(true) or {
		rejected = err.msg().contains('TLS-record-only')
	}
	assert rejected
	rejected = false
	h.next_application_key_material(false) or {
		rejected = err.msg().contains('TLS-record-only')
	}
	assert rejected
	rejected = false
	h.take_buffered_handshake_bytes() or {
		rejected = err.msg().contains('TLS-record-only')
	}
	assert rejected

	assert h.state() == .wait_sh
	assert h.failure_alert() == .internal_error
}

// test_quic_reassembly_len_is_mode_agnostic locks that reassembly_len() now works
// on QUIC-mode cores: it reports reassembly byte count without an is_quic refusal,
// while take_buffered_handshake_bytes() remains TLS-record-only.
fn test_quic_reassembly_len_is_mode_agnostic() ! {
	mut h := quic_mode_core()!
	_ = h.create_client_hello()!

	// reassembly_len() answers on QUIC mode — a plain byte count, no is_quic refusal.
	assert h.reassembly_len()! == 0

	// take_buffered_handshake_bytes() still refuses QUIC mode.
	h.take_buffered_handshake_bytes() or {
		assert err.msg().contains('TLS-record-only')
	}
	assert h.state() == .wait_sh
	assert h.failure_alert() == .internal_error
}

// test_quic_mode_tls_only_calls_refuse_even_with_secrets locks the is_quic guard
// precedence on a QUIC-mode core: even with traffic secrets planted, the TLS-only
// ratchet refuses with the transport guard — never a ratchet, never a silent empty
// result — while the shared accessor derives in the same situation (the shape test
// above covers what it returns).
fn test_quic_mode_tls_only_calls_refuse_even_with_secrets() ! {
	mut h := quic_mode_core()!
	_ = h.create_client_hello()!
	h.c_hsk_tsec = [u8(1), 2, 3]
	h.s_hsk_tsec = [u8(4), 5, 6]
	h.c_app_tsec = [u8(7), 8]
	h.s_app_tsec = [u8(9), 10]
	mut rejected := false
	h.next_application_key_material(true) or {
		rejected = err.msg().contains('TLS-record-only')
	}
	assert rejected
	// A garbage ServerHello still tears the machine down through the normal parse path.
	h.receive_initial_bytes(wrap_hsk(.server_hello, []u8{len: 4, init: 0xAA})) or {}
	assert h.state() == .failed
}

// test_tls_mode_zeroize_and_failure_retire_record_keys guards the behavior-preserving
// half of the change: on a TLS-mode core the unified teardown still makes the record
// accessor refuse, because it now derives from secrets the wipe just cleared.
fn test_tls_mode_zeroize_and_failure_retire_record_keys() ! {
	mut h := tls_mode_core()!
	h.state = HandshakeState.connected
	h.c_app_tsec = [u8(11), 12]
	h.s_app_tsec = [u8(13), 14]
	// While both secrets live the accessor derives; the guard is presence, not a cache.
	h.traffic_keys() or {
		assert false, 'record keys should derive while the application secrets live'
	}
	h.zeroize_traffic_secrets()!
	assert h.c_app_tsec.len == 0
	assert h.s_app_tsec.len == 0
	h.traffic_keys() or {
		assert err.msg().contains('application traffic keys unavailable')
	}

	// The failure path wipes both levels too (§7.5), so the secrets go with them.
	h.c_hsk_tsec = [u8(1), 2]
	h.s_hsk_tsec = [u8(3), 4]
	h.fail_and_zeroize_secrets()
	assert h.c_hsk_tsec.len == 0
	assert h.s_hsk_tsec.len == 0
	assert h.state() == .failed
}
