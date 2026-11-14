// Copyright (c) 2026 blackshirt.
//
// record_layer.v is the TLS record facade: the four per-direction protection states, the
// post-handshake reassembly buffer, and the KeyUpdate mechanism built on them
// (RFC 8446 §4.6.3/§5.5; §4.5 is EndOfEarlyData, not a KeyUpdate prohibition). Conn keeps
// the transport, alert and lifecycle concerns;
// the record policy that used to be spread across conn.v lives here, beside the state it
// acts on. The core never sees any of this state — it hands over key material only.
module tls

import x.net.tls.handshake

// RecordLayer holds the record layer's own state for one connection.
struct RecordLayer {
mut:
	// The four TLSProtections are the record layer's own state; the core never sees them.
	s_hsk_protection TLSProtection // protection state for records the server sends under handshake keys
	c_hsk_protection TLSProtection // protection state for records we send under handshake keys
	s_app_protection TLSProtection // protection state for records the server sends under app keys
	c_app_protection TLSProtection // protection state for records we send under app keys
	// Decrypted handshake bytes staged when a post-handshake message spans records;
	// the incomplete tail waits for the next record (RFC 8446 §5.2).
	post_hsk_buf []u8
	// Set when the peer sends KeyUpdate(update_requested) and we have answered with our
	// own KeyUpdate(update_not_requested) but have not yet seen the peer acknowledge our
	// response (a KeyUpdate(update_not_requested) from them). A second update_requested
	// before that acknowledgment is a sender-side MUST-NOT violation (RFC 8446 §4.6.3)
	// and a receiver-side DoS vector, so it is rejected with unexpected_message.
	wait_ku_response bool
}

// new_record_layer builds the record state of a fresh connection: four uninitialized
// protection states (each probed by IV length before use), an empty reassembly buffer and
// a cleared latch. Conn is @[noinit], so dial() and the test fixtures must assign this
// constructor's value rather than leave the nested fields to whatever the heap page holds.
fn new_record_layer() RecordLayer {
	return RecordLayer{
		s_hsk_protection: TLSProtection{ aead: unsafe { nil } }
		c_hsk_protection: TLSProtection{ aead: unsafe { nil } }
		s_app_protection: TLSProtection{ aead: unsafe { nil } }
		c_app_protection: TLSProtection{ aead: unsafe { nil } }
		post_hsk_buf:     []u8{}
		wait_ku_response: false
	}
}

// install_traffic_protection builds both protection states for the current
// traffic-key generation, called once after ServerHello (handshake keys) or once
// the server Finished verifies (application keys). The core's traffic_keys()
// derives the right secret set from HandshakeState, and this builder installs it
// into the matching protection slots.
fn (mut r RecordLayer) install_traffic_protection(mut h handshake.HandshakeCore) ! {
	mut keys := h.traffic_keys()!
	suite := h.negotiated_cipher_suite()
	mut client := keys.take_client_material()
	mut server := keys.take_server_material()
	// The builders drain the legs they install, so these wipes scrub every remaining
	// copy on all exit paths (§7.5); a handle may keep zero-filled buffers of the
	// original length, since the builders take their by-value copies.
	defer {
		keys.wipe()
		client.wipe()
		server.wipe()
	}
	if h.state() == .connected {
		r.c_app_protection = new_tls_protection_from_keys(suite, mut client)!
		r.s_app_protection = new_tls_protection_from_keys(suite, mut server)!
	} else {
		r.c_hsk_protection = new_tls_protection_from_keys(suite, mut client)!
		r.s_hsk_protection = new_tls_protection_from_keys(suite, mut server)!
	}
}

// needs_rekey reports whether the current write generation has reached the §5.5 rekey
// point: one slot before the hard sequence limit, so the KeyUpdate notice still fits
// under the old key.
fn (r &RecordLayer) needs_rekey() bool {
	return r.c_app_protection.seq_num >= record_seq_limit - 1
}

// build_key_update_notice frames a KeyUpdate(update_not_requested) as one record under
// the CURRENT write keys (§4.6.3: the notice is always the last record of the generation
// it retires). The caller writes it and then calls complete_key_update(); pairing them
// is what keeps the key advance after the socket hand-off, so a failed write never
// leaves the peer holding keys we have already retired.
fn (mut r RecordLayer) build_key_update_notice() !Record {
	msg := handshake.build_key_update(false)
	return r.c_app_protection.encrypt_record(.handshake, msg)!
}

// complete_key_update ratchets the write leg in the core and installs the next key
// material over it (RFC 8446 §4.6.3/§7.2).
fn (mut r RecordLayer) complete_key_update(mut h handshake.HandshakeCore) ! {
	r.swap_application_leg(mut h, false)!
}

// receive_key_update applies a peer KeyUpdate body (RFC 8446 §4.6.3): it advances the
// read leg and reports whether the peer also asked for our own update. RFC 8446 has no
// one-in-a-row prohibition — crossing update requests and responses advance both sides
// by two generations (§4.6.3) — so there is no one-in-a-row latch; the only bounds are
// the per-read post-handshake message cap in conn.v and the update_requested flood
// guard below. Answering a request_update needs the socket, so it stays with the caller.
fn (mut r RecordLayer) receive_key_update(mut h handshake.HandshakeCore, body []u8) !bool {
	request_update := handshake.parse_key_update(body)!
	// §4.6.3: the sender MUST NOT send another KeyUpdate until it receives one from its
	// peer. A second update_requested before we have seen the peer acknowledge our
	// response (a KeyUpdate(update_not_requested) from them) is a sender-side protocol
	// violation and a receiver-side CPU-exhaustion DoS vector, so it is rejected with
	// unexpected_message.
	if request_update && r.wait_ku_response {
		return error('tls: consecutive update_requested KeyUpdate before peer response (unexpected_message)')
	}
	// RFC 8446 §5.1: handshake messages MUST NOT span key changes. If a partial
	// handshake message is staged in post_hsk_buf and a KeyUpdate now retires its
	// generation, the remaining tail bytes would be decrypted under a different key
	// than the head — reject before swapping.
	if r.post_hsk_buf.len > 0 {
		return error('tls: handshake message spans key change (unexpected_message)')
	}
	// §4.6.3: a recipient always advances its read keys on receiving a KeyUpdate. The
	// read leg is the server direction: swap_application_leg(true).
	r.swap_application_leg(mut h, true)!
	r.wait_ku_response = request_update
	return request_update
}

// swap_application_leg ratchets one direction's application secret in the core and installs
// the key material that ratchet returns over that leg (RFC 8446 §4.6.3). is_server_leg=true
// swaps the server→client read leg (the one this endpoint reads), false the client→server
// write leg (the one it writes) — the same direction vocabulary the core's
// next_application_key_material uses. Because the core ratchets and derives in one call,
// the secret and the installed protection can never name different generations. Sequence
// numbers restart at 0 per generation (§5.3).
fn (mut r RecordLayer) swap_application_leg(mut h handshake.HandshakeCore, is_server_leg bool) ! {
	mut keys := h.next_application_key_material(is_server_leg)!
	suite := h.negotiated_cipher_suite()
	// new_tls_protection_from_keys() drains the material it installs; the wipe is the safety net
	// for a failure part-way through, so no key material escapes unscrubbed (§7.5).
	defer {
		keys.wipe()
	}
	// Copy the superseded leg before rebinding it, then wipe its nonce base: §7.5 wants
	// old traffic keys gone once replaced. wipe() is IV-only (the AEAD backend key is a
	// documented platform limit) and idempotent, so it is safe even if never installed.
	if is_server_leg {
		mut old := r.s_app_protection
		r.s_app_protection = new_tls_protection_from_keys(suite, mut keys)!
		old.wipe()
	} else {
		mut old := r.c_app_protection
		r.c_app_protection = new_tls_protection_from_keys(suite, mut keys)!
		old.wipe()
	}
}

// wipe_protection_states zeroes this layer's four record protection states; it is the
// single place the four *installed* states are destroyed — swap_application_leg()
// additionally wipes the one superseded leg the fields no longer reference
// (RFC 8446 §7.5). wipe() is idempotent, so it is safe on a state never installed.
fn (mut r RecordLayer) wipe_protection_states() {
	r.s_hsk_protection.wipe()
	r.c_hsk_protection.wipe()
	r.s_app_protection.wipe()
	r.c_app_protection.wipe()
}

// wipe_post_hsk_buf zeroes then drops staged post-handshake bytes so freed memory
// retains no partial handshake message (RFC 8446 §5.2; §7.5 spirit).
fn (mut r RecordLayer) wipe_post_hsk_buf() {
	zero_bytes(mut r.post_hsk_buf)
	r.post_hsk_buf = []u8{}
}
