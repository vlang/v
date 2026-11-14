// Copyright (c) 2026 blackshirt.
//
// Core-contract suite for the transport-neutral handshake: phase-named
// receive_initial_bytes()/receive_handshake_bytes() reassembly across adversarial
// splits, reassembly_len accessor, the confirm/zeroize secret lifecycle, and the
// record-key guards.
module handshake

import tls.pki
import tls.wire

// fixture_validator builds a Validator over an empty trust store; none of these
// tests reach chain verification.
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

// test_handshake_message_carries_bytes checks that a message carries the full
// framed bytes.
fn test_handshake_message_carries_bytes() ! {
	m := new_handshake_message([u8(0x14), 0x00, 0x00, 0x01, 0xff])!
	assert m.msg.len == 5
}

// test_new_handshake_message_rejects_length_mismatch checks that a header
// disagreeing with the body it carries is refused.
fn test_new_handshake_message_rejects_length_mismatch() ! {
	// Finished header declaring a 5-byte body, message carrying only 1.
	bad := [u8(0x14), 0x00, 0x00, 0x05, 0xff]
	new_handshake_message(bad) or {
		assert err.msg().contains('declares 5 body bytes, message carries 1')
		return
	}
	assert false
}

// test_new_handshake_message_rejects_truncated_header checks that a message
// shorter than its own header surfaces the header error.
fn test_new_handshake_message_rejects_truncated_header() ! {
	new_handshake_message([u8(0x01), 0x00]) or {
		assert err.msg().contains('header truncated')
		return
	}
	assert false
}

// ---- receive_initial_bytes()/receive_handshake_bytes() reassembly: split and coalesced delivery ----

// test_feed_initial_buffers_incomplete_message checks that an incomplete
// ServerHello parks in the initial-phase buffer without parsing.
fn test_feed_initial_buffers_incomplete_message() ! {
	mut h := new_tls_handshake('example.com', HandshakeOptions{ validator: fixture_validator() })!
	_ = h.create_client_hello()!

	full := wrap_hsk(.server_hello, []u8{len: 10, init: 0xAA})
	split := full.len / 2

	first := h.receive_initial_bytes(full[..split])!
	assert first.len == 0
	assert h.reassembly_len()! == split

	// Completing the frame consumes it; the garbage body then fails to parse.
	h.receive_initial_bytes(full[split..]) or {
		assert h.reassembly_len()! == 0
		return
	}
	assert false // a garbage ServerHello body must never parse cleanly
}

// test_backlog_tracks_parked_fragment checks that the reassembly buffer starts
// empty and the accessor tracks a parked fragment.
fn test_backlog_tracks_parked_fragment() ! {
	mut h := new_tls_handshake('example.com', HandshakeOptions{ validator: fixture_validator() })!
	assert h.reassembly_len()! == 0
	assert h.reassembly_len()! == 0
	_ = h.create_client_hello()!
	h.state = .wait_ee
	// A 2-byte fragment parked at the Handshake level is buffered, not dropped.
	_ = h.receive_handshake_bytes([u8(2), 0x00])!
	assert h.reassembly_len()! == 2
}

// ---- state machine accessor ----

fn test_tls_and_quic_modes_share_handshake_state() ! {
	mut record := new_tls_handshake('example.com', HandshakeOptions{ validator: fixture_validator() })!
	mut quic := new_quic_handshake('example.com', HandshakeOptions{ validator: fixture_validator() })!

	assert record.state() == .start
	assert quic.state() == .start
	assert record.failure_alert() == .internal_error
	assert quic.failure_alert() == .internal_error

	_ = record.create_client_hello()!
	_ = quic.create_client_hello()!
	assert record.state() == .wait_sh
	assert quic.state() == .wait_sh
}

fn test_handshake_options_select_initial_key_share() ! {
	options := HandshakeOptions{
		alpn:             ['h2']
		validator:        fixture_validator()
		key_share_group:  .secp256r1
		supported_groups: [.secp256r1]
	}
	mut h := new_tls_handshake('example.com', options)!
	assert h.config.alpn == ['h2']
	assert h.ecdhe_group == .secp256r1
	assert h.config.client_share.group == .secp256r1
	assert h.config.client_share.key_exchange.len == 65
	_ = h.create_client_hello()!
}

fn test_handshake_options_reject_unoffered_initial_group() ! {
	new_tls_handshake('example.com', HandshakeOptions{
		validator:        fixture_validator()
		key_share_group:  .x25519
		supported_groups: [.secp256r1]
	}) or {
		assert err.msg().contains('must be included in supported_groups')
		return
	}
	assert false
}

fn test_handshake_options_reject_empty_alpn_name() {
	new_tls_handshake('example.com', HandshakeOptions{
		alpn:      ['']
		validator: fixture_validator()
	}) or {
		assert err.msg().contains('ALPN protocol names must not be empty')
		return
	}
	assert false
}

fn test_handshake_options_reject_overlong_alpn_name() {
	new_tls_handshake('example.com', HandshakeOptions{
		alpn:      ['a'.repeat(256)]
		validator: fixture_validator()
	}) or {
		assert err.msg().contains('ALPN protocol name exceeds 255 bytes')
		return
	}
	assert false
}

// test_handshake_options_reject_overlong_legacy_session_id pins RFC 8446 §4.1.2:
// legacy_session_id MUST NOT exceed 32 bytes — the wire vector would frame up to
// 255, so the config gate is the normative bound.
fn test_handshake_options_reject_overlong_legacy_session_id() {
	new_tls_handshake('example.com', HandshakeOptions{
		legacy_session_id: []u8{len: 33, init: 0x55}
		validator:         fixture_validator()
	}) or {
		assert err.msg().contains('legacy_session_id exceeds 32 bytes')
		return
	}
	assert false
}

// test_handshake_options_keeps_legacy_session_id_verbatim guards both edges of
// the legal range: the App. D.4 default length (32) and a short (still legal)
// value must reach config unchanged, because the ServerHello echo check compares
// by equality — any rewrite would break it silently.
fn test_handshake_options_keeps_legacy_session_id_verbatim() ! {
	sid := []u8{len: 32, init: 0x55}
	h := new_tls_handshake('example.com', HandshakeOptions{
		legacy_session_id: sid
		validator:         fixture_validator()
	})!
	assert h.config.session_id == sid
	short := [u8(0xAA), 0xBB]
	h2 := new_tls_handshake('example.com', HandshakeOptions{
		legacy_session_id: short
		validator:         fixture_validator()
	})!
	assert h2.config.session_id == short
}

// test_quic_handshake_uses_empty_legacy_session_id pins RFC 9001 §8.4 at the
// constructor too: a QUIC core must never reuse the record path's generated session
// id, and an explicitly configured one is a caller error.
fn test_quic_handshake_uses_empty_legacy_session_id() ! {
	h := new_quic_handshake('example.com', HandshakeOptions{
		validator:             fixture_validator()
		quic_transport_params: [u8(0)]
	})!
	assert h.config.session_id.len == 0, 'QUIC must not generate a legacy_session_id'
	assert h.config.is_quic

	new_quic_handshake('example.com', HandshakeOptions{
		legacy_session_id:     [u8(0x55)]
		validator:             fixture_validator()
		quic_transport_params: [u8(0)]
	}) or {
		assert err.msg().contains('QUIC forbids a non-empty legacy_session_id')
		return
	}
	assert false, 'a QUIC handshake with a configured legacy_session_id must be refused'
}

// test_handshake_options_reject_unsupported_cipher_suite closes the raw-cast hole:
// an unsafe CipherSuite(0x1399) cast is constructible in V, would be advertised on
// the wire, and an echoed server choice would match it against the offered list.
// Config must fail fast at a membership gate instead.
fn test_handshake_options_reject_unsupported_cipher_suite() {
	// Held in a variable: a literal unsafe cast constant-folds into an enum-value
	// warning, while the runtime value is exactly the un-checkable cast a
	// hostile config caller could produce.
	raw_suite := u16(0x1399)
	new_tls_handshake('example.com', HandshakeOptions{
		cipher_suites: [unsafe { CipherSuite(raw_suite) }]
		validator:     fixture_validator()
	}) or {
		assert err.msg().contains('unsupported configured cipher suite')
		return
	}
	assert false
}

// test_handshake_options_reject_unsupported_signature_scheme: offering a scheme
// the verifier cannot check in CertificateVerify is a self-inflicted failure;
// only the signature_scheme_from_u16 set is acceptable.
fn test_handshake_options_reject_unsupported_signature_scheme() {
	raw_scheme := u16(0x0999) // runtime value, same warning-avoiding indirection
	new_tls_handshake('example.com', HandshakeOptions{
		signature_algorithms: [unsafe { SignatureScheme(raw_scheme) }]
		validator:            fixture_validator()
	}) or {
		assert err.msg().contains('unsupported configured signature scheme')
		return
	}
	assert false
}

// test_handshake_options_accepts_full_capability_lists guards over-rejection:
// every implementable suite, scheme, and group configured at once must pass.
fn test_handshake_options_accepts_full_capability_lists() ! {
	h := new_tls_handshake('example.com', HandshakeOptions{
		validator:            fixture_validator()
		key_share_group:      .x25519
		supported_groups:     default_supported_groups()
		cipher_suites:        [CipherSuite.tls_aes_128_gcm_sha256, .tls_aes_256_gcm_sha384,
			.tls_chacha20_poly1305_sha256]
		signature_algorithms: default_signature_algorithms()
	})!
	assert h.config.cipher_suites.len == 3
	assert h.config.signature_algorithms.len == 7
	assert h.config.supported_groups.len == 2
}

// ---- capability single-source lock (dedup regression) ----

// test_capability_decoders_roundtrip pins that every member of a supported_*
// list decodes from its own wire code — the *_from_u16 scanners and the lists
// in messages.v cannot drift, because the scanner walks the same list.
fn test_capability_decoders_roundtrip() ! {
	for cs in supported_cipher_suites {
		assert cipher_suite_from_u16(u16(cs))! == cs
	}
	for s in supported_signature_schemes {
		assert signature_scheme_from_u16(u16(s))! == s
	}
	for g in supported_named_groups {
		assert named_group_from_u16(u16(g))! == g
	}
}

// test_capability_unknown_codes_rejected_by_decoder_and_gate locks that an
// absent wire code fails BOTH paths with the pinned decoder message: the
// decoder rejects it on the wire, the membership gate rejects it at config, so
// acceptance and advertisement stay one source.
fn test_capability_unknown_codes_rejected_by_decoder_and_gate() ! {
	unknown_suites := [u16(0x1399), 0x1300, 0x0000]
	unknown_schemes := [u16(0x0201), 0x0808, 0x0402]
	unknown_groups := [u16(0x001f), 0x001e, 0x0000]
	for raw in unknown_suites {
		cipher_suite_from_u16(raw) or {
			assert err.msg() == 'handshake: unsupported cipher suite 0x${raw:04x}'
		}
		assert !(is_supported_cipher_suite(unsafe { CipherSuite(raw) }))
	}
	for raw in unknown_schemes {
		signature_scheme_from_u16(raw) or {
			assert err.msg() == 'handshake: unsupported signature scheme 0x${raw:04x}'
		}
		assert !(is_supported_signature_scheme(unsafe { SignatureScheme(raw) }))
	}
	for raw in unknown_groups {
		named_group_from_u16(raw) or {
			assert err.msg() == 'handshake: unsupported group 0x${raw:04x}'
		}
		assert !(is_supported_group(unsafe { NamedGroup(raw) }))
	}
}

// test_capability_default_offer_order pins the wire-byte identity of the default
// offer lists: preference order carries meaning (ChaCha20 first for the
// software-only AEAD), so any reorder must be a deliberate, fixture-reviewed
// change to the supported_* lists in messages.v.
fn test_capability_default_offer_order() ! {
	assert default_cipher_suites() == [CipherSuite.tls_chacha20_poly1305_sha256,
		.tls_aes_128_gcm_sha256, .tls_aes_256_gcm_sha384]
	assert default_signature_algorithms() == [SignatureScheme.rsa_pss_rsae_sha256,
		.rsa_pss_rsae_sha384, .rsa_pss_rsae_sha512, .ecdsa_secp256r1_sha256, .ecdsa_secp384r1_sha384,
		.ed25519, .rsa_pkcs1_sha256]
	assert default_supported_groups() == [NamedGroup.x25519, .secp256r1]
	// The offered-set fallback reads the same single source.
	assert offered_supported_groups([]) == supported_named_groups
}

fn test_handshake_options_default_validator() ! {
	h := new_tls_handshake('example.com')!
	assert h.state() == .start
}

fn test_quic_handshake_options_include_alpn_and_validator() ! {
	mut h := new_quic_handshake('example.com', alpn: ['h3'], validator: fixture_validator())!
	assert h.config.alpn == ['h3']
	assert h.state() == .start
}

fn test_record_handshake_accepts_named_options() ! {
	h := new_tls_handshake('example.com', alpn: ['h2'], validator: fixture_validator())!
	assert h.config.alpn == ['h2']
}

fn test_reassembly_is_owned_by_each_handshake_core() ! {
	mut record := new_tls_handshake('example.com', HandshakeOptions{ validator: fixture_validator() })!
	mut quic := new_quic_handshake('example.com', HandshakeOptions{ validator: fixture_validator() })!
	_ = record.create_client_hello()!
	_ = quic.create_client_hello()!

	_ = record.receive_initial_bytes([u8(2), 0])!
	_ = quic.receive_initial_bytes([u8(2)])!

	assert record.reassembly.len == 2
	assert quic.reassembly.len == 1
	assert record.reassembly_len()! == 2
	record.fail_and_zeroize_secrets()
	assert record.reassembly_len()! == 0
	assert quic.reassembly.len == 1
}

// test_state_progression_to_wait_sh checks that the machine starts at
// .start and moves to .wait_sh once the ClientHello is built.
fn test_state_progression_to_wait_sh() ! {
	mut h := new_tls_handshake('example.com', HandshakeOptions{ validator: fixture_validator() })!
	assert h.state() == .start
	_ = h.create_client_hello()!
	assert h.state() == .wait_sh
}

// test_feed_rejects_out_of_order_message checks that a Certificate arriving
// while the ServerHello is awaited is rejected before parsing.
fn test_feed_rejects_out_of_order_message() ! {
	mut h := new_tls_handshake('example.com', HandshakeOptions{ validator: fixture_validator() })!
	_ = h.create_client_hello()!
	cert := wrap_hsk(.certificate, []u8{len: 4, init: 0x01})
	h.receive_handshake_bytes(cert) or {
		assert err.msg().contains('only expected after ServerHello')
		assert h.reassembly_len()! == 0
		assert h.state() == .wait_sh
		return
	}
	assert false
}

// ---- receive_initial_bytes()/receive_handshake_bytes() phase guards & failure semantics ----

// test_create_client_hello_only_runs_once checks that the single-shot entry
// point refuses a second call that would poison the transcript.
fn test_create_client_hello_only_runs_once() ! {
	mut h := new_tls_handshake('example.com', HandshakeOptions{ validator: fixture_validator() })!
	_ = h.create_client_hello()!
	h.create_client_hello() or {
		assert err.msg().contains('only runs once')
		assert h.state() == .wait_sh
		return
	}
	assert false
}

// test_feed_before_client_hello_refused checks that peer bytes before the
// ClientHello are refused as a contract error, leaving the state .start.
fn test_feed_before_client_hello_refused() ! {
	mut h := new_tls_handshake('example.com', HandshakeOptions{ validator: fixture_validator() })!
	h.receive_initial_bytes(wrap_hsk(.server_hello, []u8{len: 4})) or {
		assert err.msg().contains('before create_client_hello()')
		assert h.state() == .start
		return
	}
	assert false
}

// test_feed_initial_after_server_hello_refused checks that initial-phase bytes
// after the ServerHello wait are refused, leaving the machine usable.
fn test_feed_initial_after_server_hello_refused() ! {
	mut h := new_tls_handshake('example.com', HandshakeOptions{ validator: fixture_validator() })!
	_ = h.create_client_hello()!
	h.state = .wait_ee
	h.receive_initial_bytes(wrap_hsk(.server_hello, []u8{len: 4})) or {
		assert err.msg().contains('while awaiting ServerHello')
		assert h.state() == .wait_ee
		return
	}
	assert false
}

// test_feed_marks_failed_and_refuses_after_protocol_error checks that a parse
// failure is terminal: the state becomes .failed and later feeds are refused.
fn test_feed_marks_failed_and_refuses_after_protocol_error() ! {
	mut h := new_tls_handshake('example.com', HandshakeOptions{ validator: fixture_validator() })!
	_ = h.create_client_hello()!
	// A garbage ServerHello body fails to parse; the failure is terminal.
	bad := wrap_hsk(.server_hello, []u8{len: 8, init: 0x01})
	h.receive_initial_bytes(bad) or {
		assert h.state() == .failed
		// No later phase-named receive may make progress against a dead machine.
		h.receive_initial_bytes(bad) or {
			assert err.msg().contains('state machine has failed')
			return
		}
		assert false
		return
	}
	assert false
}

// test_class_a_failure_zeroizes_and_parks_failed locks the failure-path choke-point:
// a Class A dispatch failure (an explicit classify) at the handshake level must both
// park the machine in .failed and zeroize every derived secret — not just record the
// alert. Planting key-schedule material first makes the zeroization observable.
fn test_class_a_failure_zeroizes_and_parks_failed() ! {
	mut h := new_tls_handshake('example.com', HandshakeOptions{ validator: fixture_validator() })!
	_ = h.create_client_hello()!
	h.handshake_secret = [u8(9), 8, 7]
	h.c_hsk_tsec = [u8(1), 2, 3]
	h.s_hsk_tsec = [u8(4), 5, 6]
	h.state = .wait_ee
	// Class A: a Certificate is illegal while EncryptedExtensions is awaited (unexpected_message).
	h.receive_handshake_bytes(wrap_hsk(.certificate, []u8{len: 4, init: 0x01})) or {
		assert h.state() == .failed
		assert h.handshake_secret.len == 0
		assert h.c_hsk_tsec.len == 0
		assert h.s_hsk_tsec.len == 0
		return
	}
	assert false, 'an out-of-order Certificate must abort the handshake'
}

// test_class_a_failure_scrubs_transcript_and_certs checks that classify_failure's
// zeroization also scrubs plaintext handshake history — transcript messages — so
// SNI/ALPN material does not survive on the heap after a terminal failure.
fn test_class_a_failure_scrubs_transcript_and_certs() ! {
	mut h := new_tls_handshake('example.com', HandshakeOptions{ validator: fixture_validator() })!
	_ = h.create_client_hello()!
	// Plant plaintext transcript history.
	h.transcript.messages = [u8(0xAA), 0xBB, 0xCC, 0xDD]
	assert h.server_certs.len == 0
	h.state = .wait_ee
	// Class A: a Certificate is illegal while EncryptedExtensions is awaited.
	h.receive_handshake_bytes(wrap_hsk(.certificate, []u8{len: 4, init: 0x01})) or {
		assert h.state() == .failed
		assert h.transcript.messages.len == 0
		assert h.server_certs.len == 0
		assert h.reassembly.len == 0
		return
	}
	assert false, 'an out-of-order Certificate must abort the handshake'
}

// test_reset_key_share_clears_old_ephemeral_on_group_change checks that the HRR
// group-change path releases the old private key before deriving the new one (§7.5).
// vlib key types expose no in-place wipe, so releasing the handle is the available
// destruction — both slots are nulled before the new key is created.
fn test_reset_key_share_clears_old_ephemeral_on_group_change() ! {
	mut h := new_tls_handshake('example.com', HandshakeOptions{
		validator:        fixture_validator()
		key_share_group:  .x25519
		supported_groups: [.x25519]
	})!
	_ = h.create_client_hello()!
	assert h.priv_x25519 != unsafe { nil }
	assert h.priv_p256 == none
	// Retry with a different group: the X25519 handle must be released first.
	h.reset_key_share(.secp256r1)!
	assert h.priv_x25519 == unsafe { nil }
	assert h.priv_p256 != none
}

// test_class_b_parse_failure_zeroizes_and_reports_decode_error locks the Phase-2
// classification fix: a Class B internal error (a peer-triggered parse that uses `!`)
// now routes through classify_failure, so it BOTH tears down and surfaces the
// RFC-correct decode_error instead of the stale .internal_error default.
fn test_class_b_parse_failure_zeroizes_and_reports_decode_error() ! {
	mut h := new_tls_handshake('example.com', HandshakeOptions{ validator: fixture_validator() })!
	_ = h.create_client_hello()!
	h.handshake_secret = [u8(9), 8, 7]
	h.c_hsk_tsec = [u8(1), 2, 3]
	h.state = .wait_cert
	// Class B: a malformed Certificate body fails to decode while in the right state.
	h.receive_handshake_bytes(wrap_hsk(.certificate, []u8{len: 8, init: 0x01})) or {
		assert h.state() == .failed
		assert h.failure_alert() == .decode_error
		assert h.handshake_secret.len == 0
		assert h.c_hsk_tsec.len == 0
		return
	}
	assert false, 'a malformed Certificate must abort the handshake'
}

// test_malformed_finished_reports_decode_error checks that a Finished shorter than
// Hash.length is classified decode_error (not the stale internal_error) and is terminal.
fn test_malformed_finished_reports_decode_error() ! {
	mut h := new_tls_handshake('example.com', HandshakeOptions{ validator: fixture_validator() })!
	_ = h.create_client_hello()!
	h.s_hsk_tsec = [u8(1), 2, 3]
	h.state = .wait_finished
	h.receive_handshake_bytes(wrap_hsk(.finished, []u8{len: 4, init: 0xab})) or {
		assert h.state() == .failed
		assert h.failure_alert() == .decode_error
		return
	}
	assert false, 'a malformed Finished must abort the handshake'
}

// test_feed_handshake_level_after_connected_refused checks that the handshake
// API refuses handshake-flight input after Finished without buffering it.
fn test_feed_handshake_level_after_connected_refused() ! {
	mut h := new_tls_handshake('example.com', HandshakeOptions{ validator: fixture_validator() })!
	_ = h.create_client_hello()!
	h.state = HandshakeState.connected
	h.receive_handshake_bytes(wrap_hsk(.new_session_ticket, [u8(0)])) or {
		assert err.msg().contains('only expected after ServerHello')
		assert h.state() == .connected
		assert h.reassembly_len()! == 0
		return
	}
	assert false
}

// test_traffic_keys_unavailable_before_derivation checks that the accessors
// refuse until the handshake derives keys.
fn test_traffic_keys_unavailable_before_derivation() ! {
	mut h := new_tls_handshake('example.com', HandshakeOptions{ validator: fixture_validator() })!
	_ = h.create_client_hello()!
	h.traffic_keys() or {
		return
	}
	assert false
}

// A rejected input at the wrong handshake phase must not discard usable traffic
// secrets — and therefore must not make the on-demand record-key accessor refuse.
fn test_nonfatal_receive_error_preserves_record_keys() ! {
	mut h := new_tls_handshake('example.com', HandshakeOptions{ validator: fixture_validator() })!
	h.c_app_tsec = [u8(1), 2, 3]
	h.s_app_tsec = [u8(4), 5, 6]
	h.state = HandshakeState.connected

	h.receive_initial_bytes([u8(0)]) or {
		assert err.msg().contains('only expected while awaiting ServerHello')
		assert h.state() == .connected
		assert h.c_app_tsec.len == 3
		assert h.s_app_tsec.len == 3
		h.traffic_keys() or {
			assert false, 'usable secrets must survive a non-fatal receive error'
		}
		return
	}
	assert false
}

// test_zeroize_traffic_secrets_drops_keys drops all traffic secrets and, with
// them, the record keys derived from them on demand.
fn test_zeroize_traffic_secrets_drops_keys() ! {
	mut h := new_tls_handshake('example.com', HandshakeOptions{ validator: fixture_validator() })!
	h.c_hsk_tsec = [u8(1), 2, 3]
	h.s_hsk_tsec = [u8(4), 5, 6]
	h.state = HandshakeState.connected

	h.zeroize_traffic_secrets()!

	assert h.c_hsk_tsec.len == 0
	assert h.s_hsk_tsec.len == 0
	// Traffic secrets are gone: the record accessor, which derives from them, refuses.
	h.traffic_keys() or {
		return
	}
	assert false
}

// test_zeroize_traffic_secrets_requires_connected_state checks that
// early cleanup is refused with nothing wiped.
fn test_zeroize_traffic_secrets_requires_connected_state() ! {
	mut h := new_tls_handshake('example.com', HandshakeOptions{ validator: fixture_validator() })!
	h.c_hsk_tsec = [u8(1), 2, 3]
	h.s_hsk_tsec = [u8(4), 5, 6]
	h.zeroize_traffic_secrets() or {
		assert err.msg().contains('reaches .connected')
		assert h.c_hsk_tsec.len == 3
		assert h.s_hsk_tsec.len == 3
		return
	}
	assert false
}

// test_zeroize_traffic_secrets_clears_and_refuses checks that zeroization forgets
// the application secrets (§7.5) — and with them the record keys derived on demand.
fn test_zeroize_traffic_secrets_clears_and_refuses() ! {
	mut h := new_tls_handshake('example.com', HandshakeOptions{ validator: fixture_validator() })!
	h.state = HandshakeState.connected
	h.c_app_tsec = [u8(11), 12]
	h.s_app_tsec = [u8(13), 14]

	h.zeroize_traffic_secrets()!

	assert h.c_app_tsec.len == 0
	assert h.s_app_tsec.len == 0
	// §7.5 forgets the secrets; the accessor derives from them, so it now refuses.
	h.traffic_keys() or {
		return
	}
	assert false
}

// test_zeroize_traffic_secrets_refuses_before_connected keeps an out-of-phase
// cleanup call from silently destroying secrets the handshake still needs.
fn test_zeroize_traffic_secrets_refuses_before_connected() ! {
	mut h := new_tls_handshake('example.com', HandshakeOptions{ validator: fixture_validator() })!
	h.c_app_tsec = [u8(11), 12]
	h.s_app_tsec = [u8(13), 14]

	h.zeroize_traffic_secrets() or {
		assert err.msg().contains('only valid once the handshake reaches .connected')
		assert h.state() == .start
		assert h.c_app_tsec == [u8(11), 12]
		assert h.s_app_tsec == [u8(13), 14]
		return
	}
	assert false
}

// test_next_application_key_material_refuse_missing_secrets checks that a key
// update refuses while the application traffic secrets are still empty.
fn test_next_application_key_material_refuse_missing_secrets() ! {
	mut h := new_tls_handshake('example.com', HandshakeOptions{ validator: fixture_validator() })!
	h.state = HandshakeState.connected
	h.next_application_key_material(true) or {
		assert err.msg().contains('server application traffic secret')
		h.next_application_key_material(false) or {
			assert err.msg().contains('client application traffic secret')
			return
		}
		assert false
		return
	}
	assert false
}

// test_next_application_key_material_refuse_before_connected ensures a caller
// cannot update secrets merely because they are present in the core.
fn test_next_application_key_material_refuse_before_connected() ! {
	mut h := new_tls_handshake('example.com', HandshakeOptions{ validator: fixture_validator() })!
	h.s_app_tsec = []u8{len: 32, init: 1}
	h.c_app_tsec = []u8{len: 32, init: 2}
	old_server := h.s_app_tsec.clone()
	old_client := h.c_app_tsec.clone()

	h.next_application_key_material(true) or {
		assert err.msg().contains('before the handshake is connected')
		assert h.state() == .start
		assert h.s_app_tsec == old_server
		h.next_application_key_material(false) or {
			assert err.msg().contains('before the handshake is connected')
			assert h.state() == .start
			assert h.c_app_tsec == old_client
			return
		}
		assert false
		return
	}
	assert false
}

// test_next_application_key_material_derives_next_generation checks that one call both
// ratchets the application secret (§7.2) and hands back the keys derived from the
// ratcheted secret — the invariant the split accessor/ratchet pair could not enforce.
fn test_next_application_key_material_derives_next_generation() ! {
	mut h := new_tls_handshake('example.com', HandshakeOptions{ validator: fixture_validator() })!
	h.state = HandshakeState.connected
	h.s_app_tsec = []u8{len: 32, init: 1}
	h.c_app_tsec = []u8{len: 32, init: 2}
	old_server := h.s_app_tsec.clone()
	old_client := h.c_app_tsec.clone()

	mut read_leg := h.next_application_key_material(true)!
	mut write_leg := h.next_application_key_material(false)!
	assert h.s_app_tsec != old_server
	assert h.c_app_tsec != old_client
	assert h.s_app_tsec.len == 32
	assert h.c_app_tsec.len == 32
	assert read_leg.key.len == h.key_len && read_leg.iv.len == aead_nonce_len
	assert write_leg.key.len == h.key_len && write_leg.iv.len == aead_nonce_len
	// TLS record legs hand over an empty header-protection slot.
	assert read_leg.hp.len == 0 && write_leg.hp.len == 0
	// The pair a ratchet returned is exactly the generation the record accessor now
	// derives: the secret advanced and the keys came with it, so no generation is skipped.
	mut gen := h.traffic_keys()!
	assert gen.is_ready()
	mut server := gen.take_server_material()
	mut client := gen.take_client_material()
	assert server.key == read_leg.key
	assert server.iv == read_leg.iv
	assert client.key == write_leg.key
	assert client.iv == write_leg.iv
	// Ratcheting again moves to a different generation rather than reusing this one.
	mut next := h.next_application_key_material(true)!
	assert next.key != read_leg.key
	read_leg.wipe()
	write_leg.wipe()
	next.wipe()
	server.wipe()
	client.wipe()
	gen.wipe()
}

// test_next_application_key_material_refuses_after_zeroize checks that a key update
// after teardown is refused instead of resurrecting the secret.
fn test_next_application_key_material_refuses_after_zeroize() ! {
	mut h := new_tls_handshake('example.com', HandshakeOptions{ validator: fixture_validator() })!
	h.state = HandshakeState.connected
	h.c_app_tsec = [u8(1), 2, 3]
	h.zeroize_traffic_secrets()!
	h.next_application_key_material(false) or {
		assert err.msg().contains('destroyed by zeroize_traffic_secrets()')
		return
	}
	assert false
}

// ---- HRR second-hello identity (§4.1.4) via the codec ----

// test_second_hello_reproduces_pinned_random checks that the retry ClientHello
// carries the same pinned random as the first (§4.1.4).
fn test_second_hello_reproduces_pinned_random() ! {
	mut cfg := ClientHelloConfig{
		server_name:  'example.com'
		session_id:   []u8{len: 32, init: 0xAA} // App. D.4's default length; §4.1.2 caps at 32
		random:       []u8{len: 32, init: 0x42}
		client_share: KeyShareEntry{
			group:        .x25519
			key_exchange: []u8{len: 32, init: 0x01}
		}
	}
	_, full1 := build_client_hello(cfg)!
	cfg.cookie = [u8(0xC0), 0x69, 0x65] // a retry hello carries a cookie
	_, full2 := build_client_hello(cfg)!
	assert full1 != full2 // the cookie changes the bytes
	// Both hellos embed the identical 32-byte random (§4.1.4); layout puts it at offset 6.
	random1 := full1[6..6 + 32]
	random2 := full2[6..6 + 32]
	assert random1 == cfg.random
	assert random2 == cfg.random
}

// ---- recordless framing ----

// test_next_recordless_splits_one_byte_cadence checks that a message
// materializes exactly once, at the final byte (RFC 9001 §4).
fn test_next_recordless_splits_one_byte_cadence() ! {
	mut buf := []u8{}
	full := wrap_hsk(.finished, [u8(0xDE), 0xAD, 0xBE, 0xEF])
	mut got := 0
	for b in full {
		buf << b
		msg := next_unframed_message(mut buf)!
		if msg.len > 0 {
			got += 1
			assert msg == full
			assert buf.len == 0
		}
	}
	assert got == 1
}

// test_next_recordless_two_messages_sequential checks that two coalesced
// messages are extracted one at a time.
fn test_next_recordless_two_messages_sequential() ! {
	// `+` cannot join []u8 in V, so the second frame is appended with `<<`.
	mut buf := wrap_hsk(.encrypted_extensions, [u8(0x00)])
	buf << wrap_hsk(.certificate, [u8(0x01), 0x02])
	m1 := next_unframed_message(mut buf)!
	assert m1.len == 5 // 4-byte header + 1-byte body
	m2 := next_unframed_message(mut buf)!
	assert m2.len == 6
	assert buf.len == 0
}

// test_next_recordless_rejects_oversize_length checks that a length above
// max_handshake_message_len aborts at the header.
fn test_next_recordless_rejects_oversize_length() ! {
	mut w := wire.new_writer(4)
	w.write_u8(u8(HandshakeType.certificate))
	w.write_u24(u32(max_handshake_message_len + 1))
	mut buf := w.bytes()
	next_unframed_message(mut buf) or {
		return
	}
	assert false
}

// test_next_unframed_message_reassembles_fragments checks that a fragmented
// message is withheld until its header and body are complete.
fn test_next_unframed_message_reassembles_fragments() ! {
	mut buf := [u8(0x08), 0x00, 0x00] // EncryptedExtensions, declared len 5; header short
	assert next_unframed_message(mut buf)!.len == 0
	assert buf.len == 3

	buf << u8(0x05) // header complete, body missing
	assert next_unframed_message(mut buf)!.len == 0
	assert buf.len == 4

	buf << [u8(0x01), 0x02, 0x03, 0x04, 0x05]
	msg := next_unframed_message(mut buf)!
	assert msg.len == 9
	assert msg[0] == u8(0x08)
	assert msg[8] == u8(0x05)
	assert buf.len == 0
}

// test_next_unframed_message_splits_coalesced checks that coalesced
// messages are extracted one at a time, the remainder staying buffered.
fn test_next_unframed_message_splits_coalesced() ! {
	mut buf := [u8(0x08), 0x00, 0x00, 0x01, 0xaa, 0x14, 0x00, 0x00, 0x02, 0xbb, 0xcc]

	first := next_unframed_message(mut buf)!
	assert first.len == 5
	assert first[0] == u8(0x08)
	assert buf.len == 6

	second := next_unframed_message(mut buf)!
	assert second.len == 6
	assert second[0] == u8(0x14)
	assert buf.len == 0
}

// test_next_unframed_message_rejects_oversized_length checks that the
// declared length fails at the header, before a gigantic message is buffered.
fn test_next_unframed_message_rejects_oversized_length() ! {
	mut buf := [u8(0x08), 0xff, 0xff, 0xff, 0x00] // declares ~16 MiB
	next_unframed_message(mut buf) or {
		assert err.msg().contains('exceeds maximum')
		return
	}
	assert false
}

// test_traffic_keys_are_caller_owned checks the accessor hands out a freshly derived,
// caller-owned generation: draining a leg moves it out and empties that handle, while
// the secrets stay intact so a later call re-derives independently.
fn test_traffic_keys_are_caller_owned() ! {
	mut h := new_tls_handshake('example.com', HandshakeOptions{ validator: fixture_validator() })!
	_ = h.create_client_hello()!
	h.state = HandshakeState.wait_ee
	h.c_hsk_tsec = []u8{len: 32, init: 1}
	h.s_hsk_tsec = []u8{len: 32, init: 2}
	mut keys := h.traffic_keys()!
	assert keys.is_ready()
	assert keys.client.key.len == h.key_len
	assert keys.client.iv.len == aead_nonce_len
	// Draining a leg moves it out of the generation and empties that handle.
	mut client := keys.take_client_material()
	assert client.key.len == h.key_len
	assert keys.client.key.len == 0
	client.wipe()
	keys.wipe()
	// Re-deriving is independent of the wiped handles: the secrets were never touched.
	mut again := h.traffic_keys()!
	assert again.is_ready()
	again.wipe()
}

// test_next_application_key_material_wipes_superseded_secret checks that the superseded
// application traffic secret is zeroed in place after the key update.
fn test_next_application_key_material_wipes_superseded_secret() ! {
	mut h := new_tls_handshake('example.com', HandshakeOptions{ validator: fixture_validator() })!
	h.state = HandshakeState.connected
	h.s_app_tsec = []u8{len: 32, init: 7}
	h.c_app_tsec = []u8{len: 32, init: 8}
	// Alias into the old secret, captured before the key update runs.
	old_secret := h.s_app_tsec

	mut keys := h.next_application_key_material(true)!

	// The update derives the successor first, then zeroes the superseded secret in place.
	assert old_secret.len == 32
	for b in old_secret {
		assert b == 0
	}
	assert h.s_app_tsec != old_secret
	assert h.s_app_tsec.len == 32
	// The handed-back pair came from the surviving secret, not the zeroed alias.
	assert keys.key.len == h.key_len
	keys.wipe()
}
