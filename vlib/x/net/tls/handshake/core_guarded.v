// Copyright (c) 2026 blackshirt.
//
// core_guarded.v holds the one-sided surface of the unified HandshakeCore: the
// methods that belong to exactly one transport, each guarded by is_quic so the
// wrong-mode core refuses it with a plain error — no state change, no key
// material touched — mirroring the out-of-phase guards. The TLS-record side
// holds the KeyUpdate ratchet (which returns the generation it just derived)
// and the reassembly_len hook; the QUIC side holds the opaque server transport
// parameters and the HelloRetryRequest flag the QUIC adapter needs for Initial
// re-keying. The per-phase key generations are not in this file at all — the
// shared traffic_keys() accessor in key_schedule.v answers in the core's mode,
// and raw traffic secrets never leave the core in either mode.
module handshake

// reassembly_len reports bytes the reassembly buffer retained across both phases —
// pre-ServerHello plaintext fragments and coalesced handshake-flight bytes alike.
// TLS-record and QUIC-mode cores share one reassembly buffer (state_machine_extra.v),
// so there is no per-phase count to split. TLS-record scope: only the record
// adapter enforces the §5.1 ServerHello record-boundary rule and the coalesced-flight
// drain check; QUIC frames its own CRYPTO stream but may still query this byte count
// for observability or its own drain checks before HANDSHAKE_DONE.
pub fn (h &HandshakeCore) reassembly_len() !int {
	return h.reassembly.len
}

// take_buffered_handshake_bytes moves whatever the handshake receive path left
// buffered at .connected (legal only per RFC 8446 §4.6.1 coalescing) to the record
// adapter, zeroizing the core's copy (§7.5).
pub fn (mut h HandshakeCore) take_buffered_handshake_bytes() ![]u8 {
	if h.is_quic {
		return error('handshake: take_buffered_handshake_bytes() is TLS-record-only (QUIC reassembles its own CRYPTO stream)')
	}
	taken := h.reassembly.clone()
	wipe_secret(mut h.reassembly)
	return taken
}

// next_application_key_material ratchets one direction's application traffic secret
// around a TLS KeyUpdate (RFC 8446 §7.2) and returns that leg's next record key
// material, derived from the ratcheted secret in the same call. TLS-record-only, and
// valid only once connected. Advance-and-derive is one step on purpose: a caller cannot
// move a secret without receiving the keys that go with it, so the core's secret and the
// record layer's installed protection always name the same generation. is_server_leg
// selects the server→client read leg (true, after receiving) vs the client→server write
// leg (false, before sending) — the same direction vocabulary the record layer swaps by.
// The leg is caller-owned: new_tls_protection_from_keys() drains it through take_key()/take_iv()
// (take_hp() hands over the empty TLS slot), and wipe() scrubs whatever a failed install
// left behind (§7.5). The legs built here leave KeyMaterial.hp empty: TLS protects
// record bodies, not record headers.
pub fn (mut h HandshakeCore) next_application_key_material(is_server_leg bool) !KeyMaterial {
	if h.is_quic {
		return error('handshake: next_application_key_material() is TLS-record-only (QUIC key updates live in the packet layer)')
	}
	side := if is_server_leg { 'server' } else { 'client' }
	if h.state() != .connected {
		return error('handshake: cannot update ${side} application traffic keys before the handshake is connected (state: ${h.state()})')
	}
	secret := if is_server_leg { h.s_app_tsec } else { h.c_app_tsec }
	if secret.len == 0 {
		return error('handshake: cannot update the ${side} application traffic secret (unavailable before the server Finished, destroyed by zeroize_traffic_secrets())')
	}
	next_secret := derive_updated_traffic_secret(h.hash_algo, secret) or {
		h.fail_and_zeroize_secrets()
		return err
	}
	// The superseded secret is zeroed in place before its field is rebound, which is
	// what lets an adapter aliasing the old bytes observe the zeros.
	if is_server_leg {
		wipe_secret(mut h.s_app_tsec)
		h.s_app_tsec = next_secret
	} else {
		wipe_secret(mut h.c_app_tsec)
		h.c_app_tsec = next_secret
	}
	leg := derive_key_material(h.hash_algo, next_secret, h.key_len, h.is_quic) or {
		h.fail_and_zeroize_secrets()
		return err
	}
	return leg
}

// derive_updated_traffic_secret performs the TLS KeyUpdate "traffic upd" derivation:
// the next application traffic secret from the current one. next_application_key_material()
// derives that secret's record key and IV in the same call; the secret alone never
// reaches the record layer.
fn derive_updated_traffic_secret(hash_algo HashAlgorithm, secret []u8) ![]u8 {
	return hkdf_expand_label(hash_algo, secret, 'traffic upd', []u8{}, hash_algo.hash_len())!
}

// peer_transport_parameters returns the raw bytes of the server's
// quic_transport_parameters extension, captured from EncryptedExtensions once the
// server sends it — an empty slice until it arrives. TLS never interprets them: the
// QUIC adapter decodes the id/length/value triplets (RFC 9000 §7.3).
pub fn (h &HandshakeCore) peer_transport_parameters() ![]u8 {
	if !h.is_quic {
		return error('handshake: peer_transport_parameters() is QUIC-only (TLS never interprets them)')
	}
	params := h.quic_params or { return []u8{} }
	return params.clone()
}

// is_hello_retry_request_received reports whether the core has already accepted a
// HelloRetryRequest. TLS itself has no further use for the flag once it has
// built the retry ClientHello, but a QUIC adapter must: the retry is sent under a
// different destination connection ID, and Initial protection is a function of
// that ID, so the adapter's Initial keys go stale here (RFC 9001 §5.5).
pub fn (h &HandshakeCore) is_hello_retry_request_received() !bool {
	if !h.is_quic {
		return error('handshake: is_hello_retry_request_received() is QUIC-only (the record path re-keys inside the core)')
	}
	return h.hrr_cipher_suite != none
}
